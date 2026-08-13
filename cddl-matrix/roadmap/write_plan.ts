import type { RoadmapIssue } from "./errors.ts";
import type { RoadmapWritePorts } from "./io.ts";
import type { RoadmapName } from "./model/core.ts";
import type { RoadmapDocument } from "./model/documents.ts";
import {
  isProductionOutputAuthority,
  resolvedWholeFileClaim,
  type ValidatedOutputAuthority,
} from "./output_registry.ts";

const validatedPlans = new WeakSet<object>();
const privatePlanBytes = new WeakMap<object, Uint8Array>();

export interface ProjectionWriteRequest {
  readonly write_coordinate: "projection";
  readonly roadmap: RoadmapName;
  readonly document: RoadmapDocument;
  readonly projection_bytes?: Uint8Array;
  readonly output_authority?: ValidatedOutputAuthority;
  readonly validation_issues: readonly RoadmapIssue[];
}

export interface ValidatedProjectionWritePlan {
  readonly roadmap: RoadmapName;
  readonly target: import("./model/core.ts").RepoPath;
  readonly bytes: Uint8Array;
}

export type ProjectionWritePlanResult =
  | { readonly ok: true; readonly plan: ValidatedProjectionWritePlan; readonly issues: readonly [] }
  | { readonly ok: false; readonly issues: readonly RoadmapIssue[] };

function issue(
  request: ProjectionWriteRequest,
  code: "E-OUTPUT-AUTHORITY" | "E-OUTPUT-TOML" | "E-OUTPUT-PATH" | "E-OUTPUT-WRITER",
  logical_path: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source: request.document.document.source_path,
    logical_path,
    message,
    exit: 1,
  };
}

function forbiddenAuthorityPath(path: string): boolean {
  return path.startsWith("cddl-matrix/roadmap/fixtures/") || path.startsWith("draft/");
}

/** Mint one opaque whole-file write plan only after all supplied validation sets are green. */
export function createProjectionWritePlan(request: ProjectionWriteRequest): ProjectionWritePlanResult {
  const issues: RoadmapIssue[] = [...request.validation_issues];
  const meta = request.document.document;
  const authorityValid = request.output_authority !== undefined &&
    isProductionOutputAuthority(request.output_authority);
  if (!authorityValid) {
    issues.push(issue(request, "E-OUTPUT-AUTHORITY", "output_claims.scope", "projection write requires authority from the closed production output inventory"));
  }
  if (request.write_coordinate !== "projection") {
    issues.push(issue(request, "E-OUTPUT-AUTHORITY", "write_coordinate", "projection writer cannot authorize another write coordinate"));
  }
  if (meta.roadmap !== request.roadmap) {
    issues.push(issue(request, "E-OUTPUT-AUTHORITY", "document.roadmap", "selected roadmap does not match document"));
  }
  if (meta.schema_version === 0 || meta.authority !== "authoritative") {
    issues.push(issue(request, "E-OUTPUT-AUTHORITY", "document.authority", "projection write requires an authoritative roadmap"));
  }
  if (meta.projection_path.endsWith(".toml") || meta.projection_path === meta.source_path) {
    issues.push(issue(request, "E-OUTPUT-TOML", "document.projection_path", "projection write cannot target TOML source bytes"));
  }
  if (forbiddenAuthorityPath(meta.projection_path)) {
    issues.push(issue(request, "E-OUTPUT-PATH", "document.projection_path", "projection target is not an authorized whole-file Markdown path"));
  }
  if (authorityValid && (request.projection_bytes === undefined || request.projection_bytes.byteLength === 0)) {
    issues.push(issue(request, "E-OUTPUT-WRITER", "projection", "projection write payload is empty"));
  }
  const wholeClaim = request.output_authority === undefined
    ? undefined
    : resolvedWholeFileClaim(request.output_authority, meta.projection_path);
  if (wholeClaim === undefined) {
    issues.push(issue(
      request,
      "E-OUTPUT-AUTHORITY",
      "output_claims",
      "projection path lacks an opaque validated whole-file authority",
    ));
  } else if (wholeClaim.claim.producer !== "roadmap-projector") {
    issues.push(issue(
      request,
      "E-OUTPUT-AUTHORITY",
      "output_claims",
      "whole-file authority is not owned by the roadmap-projector producer",
    ));
  } else if (
    wholeClaim.interval.start_byte !== 0 || wholeClaim.interval.end_byte <= 0 ||
    wholeClaim.payload_interval.start_byte !== wholeClaim.interval.start_byte ||
    wholeClaim.payload_interval.end_byte !== wholeClaim.interval.end_byte
  ) {
    issues.push(issue(
      request,
      "E-OUTPUT-AUTHORITY",
      "output_claims",
      "whole-file claim does not resolve to one nonempty complete target interval",
    ));
  }
  if (issues.length > 0) return Object.freeze({ ok: false, issues: Object.freeze(issues) });
  const projectionBytes = request.projection_bytes;
  if (projectionBytes === undefined) throw new Error("internal: validated projection write lost its payload");
  const plan: ValidatedProjectionWritePlan = Object.freeze({
    roadmap: request.roadmap,
    target: meta.projection_path,
    bytes: new Uint8Array(projectionBytes),
  });
  validatedPlans.add(plan);
  privatePlanBytes.set(plan, new Uint8Array(projectionBytes));
  return Object.freeze({ ok: true, plan, issues: Object.freeze([]) as readonly [] });
}

/** Apply only a capability minted by createProjectionWritePlan; atomicity belongs to the port. */
export function applyProjectionWritePlan(
  plan: ValidatedProjectionWritePlan,
  ports: RoadmapWritePorts,
): void {
  if (!validatedPlans.has(plan)) throw new Error("unvalidated projection write plan");
  const bytes = privatePlanBytes.get(plan);
  if (bytes === undefined) throw new Error("projection write plan lost its private byte snapshot");
  ports.atomicReplace(plan.target, new Uint8Array(bytes));
}
