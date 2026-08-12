# rust `cddl` gap: string-typed rest-row keys in a MIXED map demand an entry (`*` read as `+`)

Status: OPEN upstream (observed on the pinned local-fixes sibling checkout, the same binary
`verify.ts` resolves via `RUST_CDDL`; found by the cycle-13 containment-cell decode-foreign mint,
2026-08-05). No upstream issue filed yet. Same over-rejection direction and probably the same
`src/validator/cbor.rs` key-matching neighborhood as README gaps #8 (tag-typed map keys) and #11
(named-rule/choice map keys), but a DISJOINT differential grid — no named rule, no choice, no tag
anywhere in the trigger.

## Symptom

In a map that mixes DECLARED entries with a `*`-occurrence rest row, a rest-row key domain of a
STRING class (`bytes`, `tstr`) makes rust-`cddl` REQUIRE at least one rest entry — a spec-valid
instance carrying only the declared entries fails `map requires entry key of type bytes`/`tstr`.
Integer key domains (`uint`, `nint`) do not trigger it, and the pure table (no declared entries)
keeps its documented empty-map pass. The ruby reference (gem 0.12.14) accepts; RFC 8610 §3.2 makes
`*` zero-or-more, so ruby is right.

## Repro (differential grid, run 2026-08-05 against the local-fixes binary)

Instances: `a1011902b4` = `{1: 692}`; `a2011902b441aa05` = `{1: 692, h'aa': 5}`; `a0` = `{}`.
Rust verdict read from the validate output line (the binary exits 0 either way when invoked as
`cddl validate -d <spec> -c <inst>`).

| spec | instance | ruby | rust |
|---|---|---|---|
| `m = { 1: uint, * bytes => any }` | `{1: 692}` | ok (mint tally) | **FAIL** `map requires entry key of type bytes` |
| `m = { 1: uint, * tstr => any }` | `{1: 692}` | not re-probed (spec-valid by §3.2) | **FAIL** `map requires entry key of type tstr` |
| `m = { 1: uint, * uint => any }` | `{1: 692}` | ok | ok |
| `m = { 1: uint, * nint => any }` | `{1: 692}` | ok | ok |
| `m = { 1: uint, * bytes => any }` | `{1: 692, h'aa': 5}` | ok | ok |
| control `m = { * bytes => any }` | `{}` | ok | ok (the gap-#11 empty-map control still holds) |
| control (spec-invalid) `m = { * bytes => any }` | `{1: 692}` | — | FAIL (correctly — discriminating) |

So the failing cell is exactly (mixed map) × (string-typed rest key) × (zero rest entries): the
occurrence's zero lower bound is honored for integer key domains and for the pure table, and
dropped for string key domains once a declared entry coexists.

## Impact here (decode-conformance catalog)

`contain.occurrence-target.memberkey.type1.open_struct_bytes_key` (`m = { 1: uint, * bytes => any }`)
lost its two zero-rest-entry accept candidates to the two-oracle gate on its first mint
(`a1011902b4`, `a10119108c`, both recorded as `ruby=0 rust=1`) and minted
only rest-entry-bearing vectors. The row stays active, so the cost today is corroboration breadth,
not a verdict — but it inherits gap #11's lossy-re-mint hazard: a re-mint whose random draws are all
zero-rest-entry candidates would pin the row `pinned_reason`-vectorless. Distinct from the sibling
`…open_struct_named_key`, whose every candidate dies (gap #11 proper, named-rule key domain).

## Upstream report sketch

One paragraph + the grid above: `*`-occurrence lower bound of zero is not honored for
`bytes`/`tstr`-typed member keys when the map also has declared entries; integer-typed keys and the
declared-entries-free table honor it. Same key-matching site as the named-rule/choice
over-rejection (gap #11 note) is a plausible root, so the two could be one upstream issue with two
grids — file whichever way the maintainer prefers.
