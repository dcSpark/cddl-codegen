import type { SlotId, SpanId } from "../model/core.ts";

export type MatrixSlotFloor = readonly [SlotId, string, SpanId, number, number, string];

/**
 * The reviewed live matrix generated-slot contract: slot identity, binding, owning span and the
 * exact projection interval each inline status marker occupies. `usesLiveMatrixInlineSlots` reads
 * these rows to decide whether the live inline-marker rendering applies to a document.
 */
export const MATRIX_SLOT_FLOORS: readonly MatrixSlotFloor[] = Object.freeze([
  ["constraint" as SlotId, "status_header_markers:roadmap-constraint", "slot-constraint" as SpanId, 2685, 2763, "479bfe1507b84a4b19508b78ad50233c11964370206cb5b334a26fdfc535802e"],
  ["counts" as SlotId, "status_header_markers:roadmap-counts", "slot-counts" as SpanId, 1761, 1890, "c4ba9082a3c86cee77945905c9e5e08430c6f7cfa6dbd39e4cf6d5066d10e6a9"],
  ["emission" as SlotId, "status_header_markers:roadmap-emission", "slot-emission" as SpanId, 2494, 2528, "31665fb7ce5fee14a7ecff06812f2d0435f628eaf7ea6f70a8362699d4289c28"],
  ["ops" as SlotId, "status_header_markers:roadmap-ops", "slot-ops" as SpanId, 2079, 2101, "a2b6db2ce3af9f7f44c2119f4bf134b6fe7c7d4fae9613ed669f79d26d24d363"],
]);
