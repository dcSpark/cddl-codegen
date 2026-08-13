import type { RoadmapIssue } from "./errors.ts";
import type {
  ReferenceId,
  RoadmapId,
  RoadmapName,
} from "./model/core.ts";

const ROADMAP_ID_GRAMMAR =
  "^(matrix|testing)\\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*(?:\\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*)*$";
const SUBORDINATE_ID_GRAMMAR = "^[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*$";
const LEGACY_NUMERIC_TOKEN_GRAMMAR = "^(?:b|t|f|q|wp|item)[0-9]+$";

/** Versioned permanent-ID policy. The strings are data so a future version cannot mutate v1. */
export const ROADMAP_ID_POLICY_V1 = Object.freeze({
  version: 1 as const,
  grammar: ROADMAP_ID_GRAMMAR,
  subordinate_grammar: SUBORDINATE_ID_GRAMMAR,
  reserved_tokens: Object.freeze([
    "item",
    "entry",
    "record",
    "section",
    "next",
    "standing",
    "residual",
    "work",
    "decision",
    "watch",
    "closeout",
    "family",
    "cell",
    "axis",
  ] as const),
  legacy_numeric_token_grammar: LEGACY_NUMERIC_TOKEN_GRAMMAR,
});

const roadmapIdPattern = new RegExp(ROADMAP_ID_POLICY_V1.grammar);
const subordinateIdPattern = new RegExp(ROADMAP_ID_POLICY_V1.subordinate_grammar);
const legacyNumericTokenPattern = new RegExp(
  ROADMAP_ID_POLICY_V1.legacy_numeric_token_grammar,
);
const reservedTokens: ReadonlySet<string> = new Set(ROADMAP_ID_POLICY_V1.reserved_tokens);

export type IdValidationCode = Extract<
  RoadmapIssue["code"],
  "E-ID-GRAMMAR" | "E-ID-RESERVED" | "E-ID-NAMESPACE"
>;

export type IdValidationResult<T> =
  | { readonly ok: true; readonly id: T }
  | { readonly ok: false; readonly code: IdValidationCode; readonly message: string };

function accepted<T>(id: T): IdValidationResult<T> {
  return { ok: true, id };
}

function rejected<T>(code: IdValidationCode, message: string): IdValidationResult<T> {
  return { ok: false, code, message };
}

/**
 * Validate and brand one permanent ID. Tokenization is deliberately private: accepted IDs leave
 * this boundary as opaque strings and no consumer receives ancestry or kind information.
 */
export function validateRoadmapId(
  value: string,
  expectedNamespace?: RoadmapName,
): IdValidationResult<RoadmapId> {
  const match = roadmapIdPattern.exec(value);
  if (match === null) {
    return rejected(
      "E-ID-GRAMMAR",
      "roadmap ID does not match the permanent namespaced grammar",
    );
  }
  if (expectedNamespace !== undefined && match[1] !== expectedNamespace) {
    return rejected(
      "E-ID-NAMESPACE",
      `roadmap ID must use the ${expectedNamespace} namespace`,
    );
  }

  // This split exists only inside validation. It is never returned or used to infer semantics.
  const tokens = value.slice(value.indexOf(".") + 1).split(/[.-]/u);
  const reserved = tokens.find((token) =>
    reservedTokens.has(token) || legacyNumericTokenPattern.test(token)
  );
  if (reserved !== undefined) {
    return rejected(
      "E-ID-RESERVED",
      `roadmap ID token ${JSON.stringify(reserved)} is reserved by permanent-ID policy v1`,
    );
  }
  return accepted(value as RoadmapId);
}

/**
 * Namespace of an ID by its declared prefix — undefined for any string carrying neither prefix.
 * This is the tree's one namespace classifier: it never invents a namespace for an unknown ID
 * (a fallback arm that answered "testing" would silently misclassify garbage). Callers needing
 * full grammar/reserved-token validation use validateRoadmapId first.
 */
export function namespaceOf(id: RoadmapId): RoadmapName | undefined {
  return id.startsWith("matrix.") ? "matrix" : id.startsWith("testing.") ? "testing" : undefined;
}

export function validateSubordinateId(value: string): IdValidationResult<string> {
  return subordinateIdPattern.test(value)
    ? accepted(value)
    : rejected(
      "E-ID-GRAMMAR",
      "subordinate ID does not match the closed lowercase grammar",
    );
}

export function validateReferenceId(value: string): IdValidationResult<ReferenceId> {
  const result = validateSubordinateId(value);
  return result.ok
    ? accepted(result.id as ReferenceId)
    : rejected(result.code, "reference ID does not match the closed lowercase grammar");
}
