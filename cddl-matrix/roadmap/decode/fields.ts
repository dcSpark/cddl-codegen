/**
 * Descriptor-driven payload field decoding.  The per-kind drivers in `semantic.ts` / `matrix.ts` /
 * `testing.ts` own DISCRIMINATION (pre-tables, state enums, nested-predicate sequencing) and the
 * cross-field state rules; every per-field acceptance decision — key sets, value kinds, nonempty
 * floors, nested table shapes — comes from the arm's field descriptors, so a payload field exists
 * end-to-end once its descriptor entry does.
 */
import {
  DISCRIMINATOR_ROWS,
  NESTED_TRANSITION_PRESENCE_ROW,
  PREDICATE_KINDS,
  TRANSITION_PREDICATE_GROUP,
  armOfGroupValue,
  fieldProperty,
  type NestedGroup,
  type PayloadArm,
  type PayloadField,
} from "../payload_descriptors.ts";
import {
  childLogicalPath as p,
  expectArrayOf,
  expectCivilDate,
  expectEnum,
  expectExactTable,
  expectFiniteNumber,
  expectFullCommitId,
  expectMarkdown,
  expectNonemptyArray,
  expectReferenceId,
  expectReferenceIdSet,
  expectRoadmapId,
  expectRoadmapIdSet,
  expectString,
  expectStringSet,
  expectSubordinateSlug,
  hasOwn,
  optionalValue,
  requiredValue,
  schemaFail,
  type DecodeContext,
} from "./primitives.ts";

function decodeScalar(
  ctx: DecodeContext,
  field: PayloadField,
  value: unknown,
  fieldPath: string,
): unknown {
  const spec = field.value;
  switch (spec.t) {
    case "kind":
      return expectString(ctx, value, fieldPath);
    case "enum":
      return expectEnum(ctx, value, spec.values, fieldPath);
    case "string":
      return expectString(ctx, value, fieldPath);
    case "slug":
      return expectSubordinateSlug(ctx, value, fieldPath);
    case "markdown": {
      const decoded = expectMarkdown(ctx, value, fieldPath);
      if (spec.nonempty === true && decoded.length === 0) {
        schemaFail(ctx, "E-SCHEMA-FLOOR", fieldPath, `${field.name} must be nonempty`);
      }
      return decoded;
    }
    case "number":
      return expectFiniteNumber(ctx, value, fieldPath);
    case "civil_date":
      return expectCivilDate(ctx, value, fieldPath);
    case "commit":
      return expectFullCommitId(ctx, value, fieldPath);
    case "string_set":
      return expectStringSet(ctx, value, fieldPath, spec.nonempty === true);
    case "roadmap_id":
      return expectRoadmapId(ctx, value, fieldPath);
    case "roadmap_id_set":
      return expectRoadmapIdSet(ctx, value, fieldPath, spec.nonempty === true);
    case "reference_id":
      return expectReferenceId(ctx, value, fieldPath);
    case "reference_id_set":
      return expectReferenceIdSet(ctx, value, fieldPath, spec.nonempty === true);
    case "table": {
      if (spec.group.arms.length > 1) {
        // The only multi-arm nested group is the trigger contract, discriminated by its own
        // nested predicate (mirroring the standalone transition's predicate-first flow).
        return decodeNestedTransition(ctx, value, fieldPath, spec.group);
      }
      return decodeArmFields(ctx, value, fieldPath, singleArm(spec.group));
    }
    case "array_table": {
      const groupArm = singleArm(spec.group);
      const elements = expectArrayOf(ctx, value, fieldPath, (entry, entryPath) => {
        const decoded = decodeArmFields(ctx, entry, entryPath, groupArm);
        return spec.flatten === undefined ? decoded : decoded[spec.flatten];
      });
      return spec.nonempty === true ? expectNonemptyArray(ctx, elements, fieldPath) : elements;
    }
  }
}

/** Decode a transition predicate (shared by standalone transition drivers and nested trigger tables). */
export function decodePredicate(ctx: DecodeContext, raw: unknown, path: string): Record<string, unknown> {
  const pre = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.transition_predicate);
  const kind = expectEnum(ctx, requiredValue(pre, "predicate_kind"), PREDICATE_KINDS, p(path, "predicate_kind"));
  const arm = armOfGroupValue(TRANSITION_PREDICATE_GROUP, { predicate_kind: kind });
  return decodeArmFields(ctx, raw, path, arm, { predicate_kind: kind });
}

function decodeNestedTransition(
  ctx: DecodeContext,
  raw: unknown,
  path: string,
  group: NestedGroup,
): Record<string, unknown> {
  const pre = expectExactTable(ctx, raw, path, NESTED_TRANSITION_PRESENCE_ROW);
  const predicate = decodePredicate(ctx, requiredValue(pre, "predicate"), p(path, "predicate"));
  const arm = armOfGroupValue(group, { predicate });
  return decodeArmFields(ctx, raw, path, arm, { predicate });
}

function singleArm(group: NestedGroup): PayloadArm {
  if (group.arms.length !== 1) {
    // Multi-arm groups (the transition predicate) are discriminated by their driver, which supplies
    // the decoded value through `presupplied`; reaching here is a descriptor-table defect.
    throw new Error("nested descriptor group requires driver-side discrimination");
  }
  return group.arms[0]!;
}

/**
 * Decode one arm: exact-table acceptance from the arm row, then every field by its descriptor.
 * `presupplied` carries the driver's already-decoded discriminants (and nested discriminated
 * values); those fields are assigned verbatim and never re-decoded, keeping the decode trace
 * (one enum/table call per authored value) identical to the hand-written decoders'.
 */
export function decodeArmFields(
  ctx: DecodeContext,
  raw: unknown,
  path: string,
  arm: PayloadArm,
  presupplied: Readonly<Record<string, unknown>> = {},
): Record<string, unknown> {
  const table = expectExactTable(ctx, raw, path, arm.row);
  const out: Record<string, unknown> = {};
  for (const field of arm.fields) {
    const prop = fieldProperty(field);
    if (hasOwn(presupplied, prop)) {
      out[prop] = presupplied[prop];
      continue;
    }
    const fieldPath = p(path, field.name);
    if (field.presence === "required") {
      out[prop] = decodeScalar(ctx, field, requiredValue(table, field.name), fieldPath);
      continue;
    }
    if (hasOwn(table, field.name)) {
      out[prop] = decodeScalar(ctx, field, optionalValue(table, field.name), fieldPath);
      continue;
    }
    if (field.value.t === "array_table" && field.value.default_empty === true) {
      out[prop] = [];
    }
  }
  return out;
}

export { armOfGroupValue };
