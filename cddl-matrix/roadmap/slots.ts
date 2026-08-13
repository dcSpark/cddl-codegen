/**
 * Inline generated slots: `{{slot:<id>}}` placeholders inside a section's `body_md`, declared by
 * `[section.slots.<id>]`.
 *
 * The resolution direction is fixed by the determinism invariant: a slot is found because it was
 * DECLARED, never because a global scan of prose noticed something placeholder-shaped. The reverse
 * direction is a validation-only scan for the `{{slot:` marker, which turns an undeclared
 * placeholder into a loud error instead of literal prose that silently survives into the
 * projection. Declarations and placements must therefore be an exact bijection.
 */
import type { GeneratedSlot, Section } from "./model/documents.ts";
import type { SlotId } from "./model/core.ts";

const UTF8 = new TextEncoder();

export const SLOT_PLACEHOLDER_MARKER = "{{slot:";

export interface SlotPlacement {
  readonly slot: GeneratedSlot;
  /** Placeholder coordinates inside the owning section's `body_md`. */
  readonly start_in_body: number;
  readonly end_in_body: number;
}

export interface SectionBodyRun {
  readonly start_in_body: number;
  readonly end_in_body: number;
}

export interface SectionSlotPlan {
  readonly section: Section;
  /** Literal prose runs between placements, in body order; zero-length runs are omitted. */
  readonly runs: readonly SectionBodyRun[];
  readonly placements: readonly SlotPlacement[];
  readonly issues: readonly SlotPlanIssue[];
}

export interface SlotPlanIssue {
  readonly logical_path: string;
  readonly message: string;
}

export function placeholderFor(slotId: SlotId | string): string {
  return `${SLOT_PLACEHOLDER_MARKER}${slotId}}}`;
}

export function documentSlots(sections: readonly Section[]): readonly GeneratedSlot[] {
  return Object.freeze(sections.flatMap((section) => [...section.slots ?? []]));
}

export function sectionOfSlot(
  sections: readonly Section[],
  slotId: SlotId | string,
): Section | undefined {
  return sections.find((section) => (section.slots ?? []).some((slot) => slot.slot_id === slotId));
}

function occurrences(haystack: Uint8Array, needle: Uint8Array): number[] {
  const found: number[] = [];
  if (needle.byteLength === 0) return found;
  outer: for (let index = 0; index + needle.byteLength <= haystack.byteLength; index++) {
    for (let offset = 0; offset < needle.byteLength; offset++) {
      if (haystack[index + offset] !== needle[offset]) continue outer;
    }
    found.push(index);
  }
  return found;
}

/**
 * Plan one section's body from its declarations. Every declared slot must occur exactly once, and
 * the count of `{{slot:` markers in the body must equal the declaration count — the two halves of
 * the bijection.
 */
export function planSectionBody(section: Section): SectionSlotPlan {
  const path = `section[${JSON.stringify(section.section_id)}]`;
  const issues: SlotPlanIssue[] = [];
  const declarations = section.slots ?? [];
  const placements: SlotPlacement[] = [];
  for (const slot of declarations) {
    const found = occurrences(section.body_md, UTF8.encode(placeholderFor(slot.slot_id)));
    if (found.length !== 1) {
      issues.push({
        logical_path: `${path}.slots.${slot.slot_id}`,
        message: `declared slot ${JSON.stringify(slot.slot_id)} occurs ${found.length} times in body_md, expected exactly one placeholder`,
      });
      continue;
    }
    const start = found[0]!;
    placements.push({
      slot,
      start_in_body: start,
      end_in_body: start + placeholderFor(slot.slot_id).length,
    });
  }
  const markers = occurrences(section.body_md, UTF8.encode(SLOT_PLACEHOLDER_MARKER));
  if (markers.length !== declarations.length) {
    issues.push({
      logical_path: `${path}.body_md`,
      message: `body_md contains ${markers.length} ${JSON.stringify(SLOT_PLACEHOLDER_MARKER)} markers for ${declarations.length} declared slot(s); every placeholder must have a declaration`,
    });
  }
  const ordered = [...placements].sort((left, right) => left.start_in_body - right.start_in_body);
  const runs: SectionBodyRun[] = [];
  let offset = 0;
  for (const placement of ordered) {
    if (placement.start_in_body < offset) {
      issues.push({
        logical_path: `${path}.slots.${placement.slot.slot_id}`,
        message: "slot placeholders overlap",
      });
      continue;
    }
    if (placement.start_in_body > offset) {
      runs.push({ start_in_body: offset, end_in_body: placement.start_in_body });
    }
    offset = placement.end_in_body;
  }
  if (offset < section.body_md.byteLength) {
    runs.push({ start_in_body: offset, end_in_body: section.body_md.byteLength });
  }
  return Object.freeze({
    section,
    runs: Object.freeze(runs),
    placements: Object.freeze(ordered),
    issues: Object.freeze(issues),
  });
}
