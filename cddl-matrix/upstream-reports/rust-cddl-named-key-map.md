# rust `cddl` gap: named-rule / parenthesized-choice map keys are over-rejected

Status: OPEN upstream (observed on the pinned local-fixes sibling checkout @ `ac1b98e`; found by the
first corpus decode-conformance mint, 2026-07-12). No upstream issue filed yet. Same neighborhood as
the tag-typed map-key gap (README gap #8, `cddl-matrix/upstream-reports/rust-cddl-tag-map-key.md`) — the member-key walk
special-cases a fixed set of key classes and mis-handles the rest.

## Symptom

A `*`-occurrence table whose KEY domain is a NAMED-RULE reference or a PARENTHESIZED CHOICE fails
rust-`cddl` validation on every spec-valid non-empty instance; the ruby `cddl` reference (gem
0.12.14) accepts. Inline prelude keys (`uint`, `tstr`) and inline literal keys (`0`) validate fine —
the indirection/choice spelling of the key type is the trigger, not the key values. The EMPTY map
validates fine everywhere (no key to mis-match), which is why the affected corpus rows keep exactly
their empty-instance vectors.

## Repro (differential grid)

CBOR instances: `a10005` = `{0: 5}`; `a1616105` = `{"a": 5}` (for the tstr rows).

| spec | instance | ruby | rust `--ci validate` |
|---|---|---|---|
| `m = { * fe => uint }` + `fe = 0 / 1 / 2` | `{0: 5}` | 0 | 1 |
| `m = { * (0 / 1 / 2) => uint }` | `{0: 5}` | 0 | 1 |
| `m = { * fe => uint }` + `fe = 0` | `{0: 5}` | 0 | 1 |
| `m = { * fe => uint }` + `fe = uint` | `{0: 5}` | 0 | 1 |
| `m = { * fe => uint }` + `fe = uint / nint` | `{0: 5}` | 0 | 1 |
| `m = { * fe => uint }` + `fe = tstr` | `{"a": 5}` | 0 | 1 |
| control `m = { * uint => uint }` | `{0: 5}` | 0 | 0 |
| control `m = { * 0 => uint }` | `{0: 5}` | 0 | 0 |
| control `m = { * tstr => uint }` | `{0: 5}` | 1 | 1 (discriminating) |
| control `m = { * tstr => uint }` | `{"a": 5}` | 0 | 0 |

Rust error shape: `unexpected key Integer(Integer(0))` from the map-key arm — same neighborhood as
README gap #8 (TAG-typed map keys, `cddl-matrix/upstream-reports/rust-cddl-tag-map-key.md`), likely the same
key-matching site in `src/validator/cbor.rs` not resolving typename references / choices when
matching entry keys.

## Impact here (corpus decode-conformance catalog)

Verified against the first full `--mint-decode-corpus` log — these drops carry the
`(accept-intended; ruby 0 rust 1)` signature on a named-rule/choice KEY table:

- `c_style_enum_map_key.enum_keyed_map` (`{ * fixed_enum => uint }`, `fixed_enum = 0 / 1 / 2`):
  every non-empty candidate contested. Whether the committed row is ACTIVE (empty-map instance
  only) or `pinned_reason`-vectorless flip-flops across re-mints on whether ruby's 10 random draws
  happen to include the empty map (the first full mint pinned it; the follow-up `--only=` re-mint
  drew `{}` and committed the single `8200a0` accept vector). A pin carries the per-oracle tallies
  in its wording (`ruby accepted N/N, rust --ci accepted 0/N`).
- `table_enum_key.enum_keyed` (`{ * ikey => text }`, `ikey = uint / nint`): 8 candidates dropped;
  only the empty-map instance survived (1 committed vector).
- `table_enum_key.enum_key_holder` (holder over `enum_keyed` + `int_keyed`): 9 candidates dropped;
  only the both-empty instance survived (1 committed vector). (`int_keyed = { * int => text }` is
  NOT affected — `int` is an inline prelude name — its row minted 10 vectors.)

No cddl-codegen behavior is affected (our generated decoder accepts these instances; the drops are
oracle-side only — nothing validated ever reached the decoder).

## Adjacent observations from the same mint (distinct signatures, recorded for the record)

Tested while classifying the mint's dropped candidates; NOT the named-key gap above:

1. **Nested map VALUES in a `*` table** — `pm = { * uint => { * uint => text } }` (inline, no named
   rule) rejects `{1: {}}` and `{1: {2: "a"}}` (ruby 0, rust 1); the same inner map standalone, and
   array values (`[uint]` inline or named `[* uint]`), validate fine. This is what dropped 8
   candidates on `wasm_nested_alias.passthru_tags_map` (its KEYS are plain `uint`) — only the
   empty-outer instance survived.
2. **Multi-entry tables with composite-ARRAY keys** — `m = { * [+ uint] => uint }` accepts the
   single-entry `{[5]: 5}` but rejects the two-entry `{[5]: 5, [6, 7]: 8}` (ruby 0, rust 1). This is
   what dropped 13 candidates across `composite_map_key.t` / `composite_map_key.holder` — their
   surviving vectors are the empty/single-entry instances.
3. **ruby tdate laxity** (opposite direction — ruby over-accepts) — ruby's generator minted
   `0("5906-11-31t22:32:49-05:59")` (November 31 does not exist) and its validator accepts it; rust
   rejects (arguably correctly, RFC 3339 date validity). One `prelude.prelude` candidate dropped.

1 and 2 are plausibly the same underlying entry-matching machinery as the named-key gap (and gap
#8); they should be re-probed whenever that site is fixed. 3 is a ruby-side generator/validator
laxity, unrelated to map matching.

## Close-out steps (when a fixed rust `cddl` ships)

1. Bump the oracle pin (fingerprint + `Cargo.toml` rev) per the usual flow.
2. Re-mint the blocked corpus rows:
   `bun run verify.ts --mint-decode-corpus --only=c_style_enum_map_key.enum_keyed_map,table_enum_key.enum_keyed,table_enum_key.enum_key_holder`
   — the rows pick up non-empty instances (and stop flip-flopping between active-empty and
   pinned) once candidates survive the two-oracle gate. If the fix also covers the adjacent
   observations, re-mint `--only=wasm_nested_alias.passthru_tags_map,composite_map_key` in the
   same change.
3. Prune README gap #11, the roadmap.toml § findings close-out entry, and this report.
