/**
 * The per-roadmap document validation entry points.  They live beside the adapters rather than
 * inside one of them because the testing roadmap's reference universe includes the matrix
 * providers: composing two adapters is a caller's job, and doing it here is what keeps either
 * adapter from importing its sibling.
 */
import type { RoadmapDocument } from "../model/documents.ts";
import type { RegistryView } from "./types.ts";
import {
  validateDecodedRoadmapDocument,
  type DecodedRoadmapValidationOptions,
  type DecodedRoadmapValidationResult,
} from "./engine.ts";
import { MATRIX_ADAPTER, validateMatrixPayloadFact } from "./matrix.ts";
import { TESTING_ADAPTER, validateTestingPayloadFact } from "./testing.ts";

export function validateMatrixRoadmapDocument(
  document: RoadmapDocument,
  view: RegistryView,
  options: DecodedRoadmapValidationOptions = {},
): DecodedRoadmapValidationResult {
  return validateDecodedRoadmapDocument(
    document,
    view,
    MATRIX_ADAPTER,
    MATRIX_ADAPTER.referenceProviders(view),
    validateMatrixPayloadFact,
    options,
  );
}

export function validateTestingRoadmapDocument(
  document: RoadmapDocument,
  view: RegistryView,
  options: DecodedRoadmapValidationOptions = {},
): DecodedRoadmapValidationResult {
  return validateDecodedRoadmapDocument(
    document,
    view,
    TESTING_ADAPTER,
    [
      ...MATRIX_ADAPTER.referenceProviders(view),
      ...TESTING_ADAPTER.referenceProviders(view),
    ],
    validateTestingPayloadFact,
    options,
  );
}
