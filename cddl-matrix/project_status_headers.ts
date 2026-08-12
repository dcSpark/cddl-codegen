#!/usr/bin/env bun
/**
 * Status-header count projection — pure committed-file reads, no cargo and no external oracles.
 *
 * The campaign-stage-owned marker payloads are derived and planned by the pure roadmap
 * compatibility seam. This file classifies argv before it can read a Markdown target, snapshots
 * each still-owned target once for check/write, and applies a successful write plan in historical
 * order. Once matrix authority cuts over, ROADMAP.md is no longer in this writer's inventory.
 *
 * Run from cddl-matrix/:
 *   bun run project_status_headers.ts           -> readable count report
 *   bun run project_status_headers.ts --check   -> derivation/marker/drift gate
 *   bun run project_status_headers.ts --write   -> preflight all targets, then rewrite all spans
 */
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { REGISTRY } from "../check.ts";
import { decodeCampaignSource } from "./roadmap/decode/campaign.ts";
import {
  classifyLegacyStatusHeaderInvocation,
  planLegacyStatusHeaderRun,
} from "./roadmap/matrix_status_facts.ts";
import type { RepoPath } from "./roadmap/model/core.ts";
import type {
  ClassifiedLegacyStatusInvocation,
  MatrixStatusInputs,
} from "./roadmap/model/matrix.ts";
import {
  productionOutputInventory,
  productionOutputStage,
} from "./roadmap/output_registry.ts";

const HERE = import.meta.dir;
const ROADMAP_PATH = "cddl-matrix/ROADMAP.md" as RepoPath;
const MATRIX_README_PATH = "cddl-matrix/README.md" as RepoPath;
const TESTS_README_PATH = "tests/README.md" as RepoPath;
const TARGETS: readonly { readonly path: RepoPath; readonly relative: string }[] = Object.freeze([
  { path: ROADMAP_PATH, relative: "ROADMAP.md" },
  { path: MATRIX_README_PATH, relative: "README.md" },
  { path: TESTS_README_PATH, relative: "../tests/README.md" },
]);

const pathOf = (relative: string): string => `${HERE}/${relative}`;

interface MatrixJson {
  readonly annotations: {
    readonly cddl_codegen: readonly {
      readonly id: string;
      readonly status: string;
      readonly emission?: Readonly<Record<string, { readonly status?: string }>>;
    }[];
  };
  readonly features: readonly { readonly id: string; readonly profile?: string }[];
  readonly containment: readonly { readonly id: string }[];
  readonly control_operators: readonly { readonly id: string }[];
}

interface CatalogToml {
  readonly row?: readonly {
    readonly id?: unknown;
    readonly vector?: readonly { readonly expect?: unknown; readonly class?: unknown }[];
  }[];
}

interface TimingsJson {
  readonly tiers?: readonly { readonly tier?: unknown; readonly wall_ms?: unknown }[];
}

function deriveInputs(): MatrixStatusInputs {
  const matrix = JSON.parse(readFileSync(pathOf("matrix.json"), "utf8")) as MatrixJson;
  const catalog = Bun.TOML.parse(
    readFileSync(pathOf("../tests/decode_conformance/catalog.toml"), "utf8"),
  ) as CatalogToml;
  const timings = JSON.parse(
    readFileSync(pathOf("../tests/timings.json"), "utf8"),
  ) as TimingsJson;

  return {
    matrix: {
      annotations: matrix.annotations.cddl_codegen.map((annotation) => ({
        id: annotation.id,
        status: annotation.status,
        ...(annotation.emission === undefined ? {} : { emission: annotation.emission }),
      })),
      features: matrix.features.map((feature) => ({
        id: feature.id,
        ...(feature.profile === undefined ? {} : { profile: feature.profile }),
      })),
      containment_ids: matrix.containment.map((row) => row.id),
      control_operator_ids: matrix.control_operators.map((row) => row.id),
    },
    catalog: {
      rows: (catalog.row ?? []).map((row) => ({
        id: typeof row.id === "string" ? row.id : "",
        vectors: (row.vector ?? []).map((vector) => ({
          ...(typeof vector.expect === "string" ? { expect: vector.expect } : {}),
          ...(typeof vector.class === "string" ? { class: vector.class } : {}),
        })),
      })),
    },
    registry: {
      gates: REGISTRY.map((gate) => ({
        id: gate.id,
        kind: gate.kind === "stub" ? "stub" : "cmd",
        ...(gate.ignoredTest === undefined ? {} : { ignored_test: gate.ignoredTest }),
      })),
    },
    timings: {
      tiers: (timings.tiers ?? []).flatMap((row) =>
        (row.tier === "fast" || row.tier === "local" || row.tier === "full")
          ? [{
            tier: row.tier,
            ...(typeof row.wall_ms === "number" ? { wall_ms: row.wall_ms } : {}),
          }]
          : []
      ),
    },
  };
}

const argv = process.argv.slice(2);
const mode = classifyLegacyStatusHeaderInvocation(argv);
const inputs = deriveInputs();
const campaignRelative = "../roadmap-campaign.toml";
const campaignAbsolute = pathOf(campaignRelative);
const campaign = existsSync(campaignAbsolute)
  ? decodeCampaignSource(
    new Uint8Array(readFileSync(campaignAbsolute)),
    "roadmap-campaign.toml",
    true,
  )
  : undefined;
const outputStage = productionOutputStage(campaign);
const ownedPaths = new Set(
  productionOutputInventory(outputStage).status_claims.map((claim) => claim.path),
);
const activeTargets = TARGETS.filter((target) => ownedPaths.has(target.path));
let invocation: ClassifiedLegacyStatusInvocation;

if (mode === "report") {
  invocation = { mode, argv };
} else {
  const targets = new Map<RepoPath, Uint8Array>();
  for (const target of activeTargets) {
    targets.set(target.path, new Uint8Array(readFileSync(pathOf(target.relative))));
  }
  invocation = { mode, argv, targets };
}

const plan = planLegacyStatusHeaderRun(inputs, invocation, undefined, outputStage);
if (plan.exit_code === 0 && mode === "write") {
  const relativeByPath = new Map(activeTargets.map((target) => [target.path, target.relative] as const));
  for (const write of plan.writes) {
    const relative = relativeByPath.get(write.path);
    if (relative === undefined) throw new Error(`status write plan contains unknown path ${write.path}`);
    writeFileSync(pathOf(relative), write.bytes);
  }
}
if (plan.stdout.byteLength > 0) process.stdout.write(plan.stdout);
if (plan.stderr.byteLength > 0) process.stderr.write(plan.stderr);
process.exitCode = plan.exit_code;
