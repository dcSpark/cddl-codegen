export const REQUIRED_ADAPTER_SELFTEST_CASE_IDS = [
  "decoder_domain_dispatch_once",
  "adapter_surface_has_no_decode_hook",
  "pipeline_indexes_before_adapter_validation",
  "indexes_created_from_decoded_document",
] as const;

export type RequiredAdapterSelfTestCaseId =
  (typeof REQUIRED_ADAPTER_SELFTEST_CASE_IDS)[number];
