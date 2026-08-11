import type { SelfTestCase, SelfTestCategory, SelfTestContext, SelfTestReceipt } from "../selftest.ts";

export interface SelfTestRegistry {
  readonly cases: readonly SelfTestCase[];
  readonly category_floors: ReadonlyMap<SelfTestCategory, number>;
  run(context: SelfTestContext): SelfTestReceipt;
}

export type CreateSelfTestRegistry = (
  cases: readonly SelfTestCase[],
  categoryFloors: ReadonlyMap<SelfTestCategory, number>,
) => SelfTestRegistry;
