# rust `cddl` gap: tag-typed map keys are over-rejected (validated against the whole map)

Status: OPEN upstream (observed on the local-fixes sibling checkout, HEAD `54f14b7`
"fix(validation) non-uint range support"; pre-existing — the same disagreement kept
`contain.map-key.type2.tag` pinned with the same `ruby=0 rust=1` signature across the `773b723`
and `2c7548e` oracle bumps). No upstream issue filed yet.

## Symptom

A type-domain member key that is a TAGGED type fails rust-`cddl` validation on spec-valid data;
the ruby `cddl` reference (gem 0.12.14) accepts it. This is the disagreement that keeps the matrix
row `contain.map-key.type2.tag` permanently `pinned_reason`-vectorless in
`tests/decode_conformance/catalog.toml` (every ruby-generated candidate dies on the two-oracle
gate: `ruby=0 rust=1`). Confirmed spelling-independent during the no-occurrence-arrow close-out
(2026-07-10): the row's re-mint under the `*` spelling drops candidates identically, so the
occurrence marker is not a factor.

## Repro (minimal differential)

spec (`tagkey.cddl`):

```cddl
m = { * #6.24(uint) => tstr }
```

instance (`tagkey.cbor`, hex `a1 d8 18 05 61 78` = `{24(5): "x"}` — spec-valid):

- ruby gem 0.12.14: `cddl tagkey.cddl validate tagkey.cbor` → exit 0 (accept)
- rust fork: `cddl --ci validate --cddl tagkey.cddl --cbor tagkey.cbor` → exit 1 with

```
error validating at cbor location : expected tagged data #6.24(uint), got Map([(Tag(24, Integer(Integer(5))), Text("x"))])
```

Control (`m = { * uint => tstr }` against `{5: "x"}` = `a1 05 61 78`): both oracles accept, so the
map-entry machinery itself is fine for untagged key types — the tag head is the trigger. The
no-occurrence spelling `{ #6.24(uint) => tstr }` reproduces identically (irrelevant to the bug;
that spelling is separately rejected by cddl-codegen itself since the exactly-once-widening
close-out).

## Suspected mechanism (from reading `src/validator/cbor.rs` at `54f14b7`)

The error text is minted by the `Type2::TaggedData` visitor arm (~line 2781). Its `match &self.cbor`
expects the CURRENT cbor value to be the `Value::Tag` being validated — but in the failing trace the
current value is still the whole `Map(...)`: the member-key walk (`visit_memberkey` →
`walk_memberkey`, ~line 4203) never repositions `self.cbor` onto the map ENTRY KEYS for a
tag-typed Type1 key the way it does for the key classes it special-cases (ints, text), so the
tagged key type is evaluated against the map itself and falls into the "got {:?}" error arm.
A fix should make type-domain tagged keys participate in the same entry-key iteration as other
non-literal key types (match each entry key against the `TaggedData`, not the container).

## Impact here

- Matrix: `contain.map-key.type2.tag` stays a supported row with NO foreign decode vectors
  (`pinned_reason` in the catalog names the oracle disagreement) — decode-foreign corroboration for
  tagged map keys is structurally unavailable until the oracle is fixed.
- No cddl-codegen behavior is affected (our generation/decoding of tagged map keys is
  execution-gated green; this is oracle-side only).

## Close-out steps (when a fixed rust `cddl` ships)

1. Re-mint the row: `cd cddl-matrix && bun run verify.ts --mint-decode-foreign --only=contain.map-key.type2.tag`
   (the pinned row should mint real accept vectors; the `pinned_reason` disappears from the catalog).
2. Re-run the full `verify.ts` so the row's evidence picks up the corroboration clause; fold both
   into one commit.
3. Prune this report and the `contain.map-key.type2.tag` mention in the roadmap.toml upstream close-outs.

Before filing upstream: re-verify against a stock upstream `cddl` build (the observed binary carries
local-fixes commits; the failing path is untouched by them, but the issue should cite vanilla) and
minimize to the standalone two-file repro above.
