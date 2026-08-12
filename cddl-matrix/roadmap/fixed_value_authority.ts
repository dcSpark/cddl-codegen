import type { RegistryView } from "./adapters/types.ts";
import type {
  DenominatorAuthorityAdapter,
  DenominatorAuthorityRegistry,
  DerivedDenominatorCandidate,
} from "./denominator.ts";
import type { ReferenceId, RoadmapId } from "./model/core.ts";
import type { EvidenceStage, EvidenceStageOutcome } from "./model/systematic.ts";

const rid = (value: string): RoadmapId => value as RoadmapId;
const ref = (value: string): ReferenceId => value as ReferenceId;
const FAMILY = rid("matrix.systematic.fixed-value-choice-member");
const AXIS = rid("matrix.fixed-value-representative-kind");
const GENERATION = rid("matrix.requirement.fixed-value-choice-member-generation");
const RUNTIME = rid("matrix.requirement.fixed-value-choice-member-runtime");

const REPRESENTATIVES = Object.freeze({
  Bool: { value: "bool", feature: "prelude.true", cell: "contain.choice-member.prelude.true.fixed-kind", example: "t = true / tstr" },
  Bytes: { value: "bytes", feature: "type2.value", cell: "contain.choice-member.type2.value.bytes.fixed-kind", example: "t = h'CAFE' / tstr" },
  Float: { value: "float", feature: "type2.value", cell: "contain.choice-member.type2.value.float.fixed-kind", example: "t = 1.5 / tstr" },
  Nint: { value: "nint", feature: "type2.value", cell: "contain.choice-member.type2.value.nint.fixed-kind", example: "t = -1 / null / tstr" },
  Null: { value: "null", feature: "prelude.null", cell: "contain.choice-member.prelude.null.fixed-kind", example: "t = null / tstr / uint" },
  Text: { value: "text", feature: "type2.value", cell: "contain.choice-member.type2.value.text.fixed-kind", example: 't = "x" / uint' },
  Uint: { value: "uint", feature: "type2.value", cell: "contain.choice-member.type2.value.uint.fixed-kind", example: "t = 5 / tstr" },
  Undefined: { value: "undefined", feature: "prelude.undefined", cell: "contain.choice-member.prelude.undefined.fixed-kind", example: "t = undefined / tstr" },
} as const);

const expectedPayload = Object.freeze({
  Bool: "bool", Bytes: "Vec<u8>", Float: "f64", Nint: "i128", Null: null,
  Text: "String", Uint: "u64", Undefined: null,
} as const);

function outcome(requirement_id: RoadmapId, face: string, stage: EvidenceStage, value: EvidenceStageOutcome) {
  return { requirement_id, profile: "default", face, stage, outcome: value } as const;
}

function derive(view: RegistryView) {
  const source = view.fixed_value_source;
  const containment = view.matrix_containment;
  const support = view.matrix_support;
  if (source === undefined || containment === undefined || support === undefined) {
    throw new Error("revision registry omitted FixedValue/containment/support authority facts");
  }
  const variants = new Map(source.variants.map((value) => [value.name, value.payload]));
  const expectedNames = Object.keys(REPRESENTATIVES).sort();
  if (JSON.stringify([...variants.keys()].sort()) !== JSON.stringify(expectedNames) ||
    expectedNames.some((name) => variants.get(name) !== expectedPayload[name as keyof typeof expectedPayload])) {
    throw new Error("FixedValue variants/payloads no longer equal the reviewed representative vocabulary");
  }
  if (JSON.stringify([...source.lowered_variants].sort()) !== JSON.stringify(expectedNames)) {
    throw new Error("type2_to_fixed_value no longer lowers the reviewed FixedValue vocabulary exactly");
  }
  const containmentById = new Map(containment.map((value) => [value.id, value]));
  const supportById = new Map(support.map((value) => [value.id, value]));
  const expectedRowIds = Object.values(REPRESENTATIVES).map((value) => value.cell).sort();
  const fixedKindRowIds = containment.filter((value) => value.id.endsWith(".fixed-kind")).map((value) => value.id).sort();
  const fixedKindSupportIds = support.filter((value) => value.id.endsWith(".fixed-kind")).map((value) => value.id).sort();
  if (JSON.stringify(fixedKindRowIds) !== JSON.stringify(expectedRowIds) ||
    JSON.stringify(fixedKindSupportIds) !== JSON.stringify(expectedRowIds)) {
    throw new Error("fixed-value choice-member representative rows/support no longer equal the exact reviewed eight-row set");
  }
  const candidates: DerivedDenominatorCandidate[] = [];
  for (const name of expectedNames) {
    const representative = REPRESENTATIVES[name as keyof typeof REPRESENTATIVES];
    const row = containmentById.get(representative.cell);
    const verdict = supportById.get(representative.cell);
    if (row === undefined || row.role !== "role.choice-member" || row.feature !== representative.feature ||
      row.spec !== "allowed" || row.example !== representative.example) {
      throw new Error(`missing or drifted canonical containment representative ${representative.cell}`);
    }
    if (verdict === undefined || !["supported", "unsupported"].includes(verdict.status)) {
      throw new Error(`missing or non-final support verdict for ${representative.cell}`);
    }
    const supported = verdict.status === "supported";
    const evidence = verdict.evidence;
    if (supported) {
      if (!evidence.includes("cddl-codegen exit 0") || !evidence.includes("compiles=ok") ||
        !evidence.includes("round-trips=ok") || !evidence.includes("wasm round-trips")) {
        throw new Error(`supported representative ${representative.cell} lacks exact rust+wasm round-trip evidence`);
      }
    } else if (!evidence.includes("cddl-codegen exit 1")) {
      throw new Error(`refused representative ${representative.cell} lacks graceful rust generation-refusal evidence`);
    }
    candidates.push(Object.freeze({
      coordinates: Object.freeze([{ axis_id: AXIS, value_id: rid(`matrix.fixed-value-kind.${representative.value}`) }]),
      spec_legality: "legal" as const,
      affected_profiles: Object.freeze(["default"]),
      affected_faces: Object.freeze(supported ? ["rust", "wasm"] : ["rust"]),
      expected_disposition: supported ? "supported" as const : "safely_refused" as const,
      expected_outcomes: Object.freeze(supported
        ? [
          outcome(GENERATION, "rust", "generated", "succeeded"),
          outcome(RUNTIME, "rust", "compiled", "succeeded"),
          outcome(RUNTIME, "rust", "round_tripped", "succeeded"),
          outcome(RUNTIME, "wasm", "compiled", "succeeded"),
          outcome(RUNTIME, "wasm", "round_tripped", "succeeded"),
        ]
        : [
          outcome(GENERATION, "rust", "generated", "safely_refused"),
          outcome(RUNTIME, "rust", "compiled", "inapplicable"),
          outcome(RUNTIME, "rust", "round_tripped", "inapplicable"),
        ]),
    }));
  }
  return Object.freeze({
    axes: Object.freeze([Object.freeze({ id: AXIS, value_ids: Object.freeze(expectedNames.map((name) => rid(`matrix.fixed-value-kind.${REPRESENTATIVES[name as keyof typeof REPRESENTATIVES].value}`)).sort()) })]),
    candidates: Object.freeze(candidates),
    evidence_requirements: Object.freeze([
      Object.freeze({ id: GENERATION, profiles: ["default"], faces: ["rust"], stages: ["generated"] as EvidenceStage[] }),
      Object.freeze({ id: RUNTIME, profiles: ["default"], faces: ["rust", "wasm"], stages: ["compiled", "round_tripped"] as EvidenceStage[] }),
    ]),
    legal_cell_floor: 8,
    evidence_binding_floor: 36,
  });
}

export const FIXED_VALUE_CHOICE_MEMBER_AUTHORITY: DenominatorAuthorityAdapter = Object.freeze({
  family_id: FAMILY,
  authority_kind: "registry",
  authority_reference_id: ref("ref-matrix-systematic-fixed-value-choice-member-authority"),
  derive,
});

export const MATRIX_DENOMINATOR_AUTHORITIES: DenominatorAuthorityRegistry = Object.freeze(new Map([
  [FAMILY, FIXED_VALUE_CHOICE_MEMBER_AUTHORITY],
]));
