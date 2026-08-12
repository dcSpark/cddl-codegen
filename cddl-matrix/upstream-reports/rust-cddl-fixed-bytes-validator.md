# Pinned rust-cddl fixed-byte validator and AST-display defects

Date: 2026-08-10

Both findings below are upstream `dcSpark/cddl` defects, not cddl-codegen codec failures. They were
observed at the exact Cargo pin used by this repository:

- crate version: `cddl 0.10.6`
- branch/revision: `local-fixes` at
  `ac1b98ec07184236517da4511b1bbea239e35190` (`ac1b98e`)
- the parser, generated-crate Rust conformance oracle, and matrix Rust oracle all use this pin

## 1. Valid fixed-byte CBOR panics in the validator

### Minimal library repro and exact failure

```rust
fn main() {
    cddl::validate_cbor_from_slice("x = h'CAFE'", &[0x42, 0xca, 0xfe], None).unwrap();
}
```

The spec parses and compiles, and `42cafe` is the preferred CBOR encoding of its one valid value.
Instead of returning `Ok(())`, the call panics:

```text
thread 'main' panicked at .../cddl-ac1b98e/src/validator/cbor.rs:4840:29:
called `Option::unwrap()` on a `None` value
```

The failing source is `self.state.ctrl.unwrap()` in a byte-value mismatch arm even though this is
an ordinary fixed byte value with no active control operator. A containing-member repro has the
same signature:

```cddl
a = [v: h'0102', x: uint]
```

against CBOR hex `824201021910fe`. Ruby `cddl` 0.12.14 accepts both valid instances. The rust-cddl
`compile-cddl` path also succeeds, so this is validation, not parsing.

### Impact and current narrow accommodations

The panic blocks independent Rust-oracle certification in two places; it does not control the
support verdict:

- the matrix rows `value.bytes`, `contain.array-element.value.bytes`, and
  `contain.map-value.value.bytes` remain pinned/vectorless for foreign-decode corroboration and
  explicitly read `enforce=unverified` in Q4;
- the corpus `fixed_singletons` Rust conformance calls for exactly `bytes_hex`, `bytes_raw`,
  `bytes_empty`, `bytes_members`, `bytes_map_members`, `tagged_bytes`, and `cbor_bytes` are skipped.

The accommodations are deliberately narrower than either fixture:

- `rust_oracle_fingerprint_preflight` calls the minimal `h'CAFE'` / `42cafe` repro inside
  `catch_unwind` and requires the exact string panic payload above. An `Ok`, a returned `Err`, a
  non-string payload, or changed message fails stale.
- `RUST_ORACLE_RULE_SKIP` names seven `(fixture, rule)` pairs. The gate separately requires each
  rule and its exact emitted validator call to exist, so a renamed/deleted call or an accidental
  eighth skip fails. All ordinary generated round trips and dumps still run, as do all unaffected
  Rust calls, the complete Ruby sweep, and the structural reference-codec differential.
- `CORPUS_DECODE_FLOOR_ARM_EXEMPT` contains only
  `fixed_singletons.mixed_text_bytes_choice/2`. The projected floor independently pins that row's
  full `2`/`3` major-type set, so the exemption cannot erase the byte arm from the declared floor.
- `HEADER_MUTANT_ACCEPT_SKIP` contains only
  `(fixed_singletons.mixed_text_bytes_choice, wrong_major)`: changing the sampled text head in
  `82006161` to a byte head produces `82004161`, the choice's valid `h'61'` arm. `trunc_head` is
  forbidden from this ledger, and the row/mutant pair is stale-checked.
- Q4 pins the exact three-row unverified set and the empty over-acceptance set. No fixture-wide or
  all-fixed-byte oracle skip exists.

Generator execution remains independently green: the fixed-singleton integration fixture checks
exact wrong-byte rejection plus default/preserve/canonical behavior, and the cycle's standalone
full matrix verify passed 109 supported / 13 unsupported / 1 out-of-profile, 177 decode-foreign
rows / 1,513 vectors / 0 failures, and 13/13 component probes.

### Removal and recheck criteria

When a new pinned rust-cddl revision makes the minimal call return `Ok(())`:

1. Remove the seven fixed-byte `RUST_ORACLE_RULE_SKIP` entries and replace or retire the panic
   preflight; do not bless a changed panic or returned error as a fix.
2. Remove the three pinned decode-row reasons and re-mint with
   `bun run verify.ts --mint-decode-foreign --only=value.bytes,contain.array-element.value.bytes,contain.map-value.value.bytes`.
3. Mint reason-bearing invalid fixed-byte vectors so those same three rows move from Q4's exact
   unverified set to its enforce-green set.
4. Remove `fixed_singletons.mixed_text_bytes_choice/2` from the corpus arm exemption and re-mint
   that corpus row. Re-evaluate the exact `wrong_major` header-mutant skip: the mutation still lands
   on another legal choice arm, so retain it if observed, but remove its obsolete "unsampled because
   of the validator" wording; remove the entry only if the replay no longer produces it.
5. Run `ir_conformance_corpus`, both decode replay gates, the full matrix verify, and then the next
   appropriate `check.ts full` checkpoint. Remove README gap #17 and the corresponding roadmap
   close-out only after that evidence is green.

## 2. `B16ByteString` AST `Display` treats binary bytes as UTF-8

### Minimal library repro and exact failure

```rust
fn main() {
    let ast = cddl::parser::cddl_from_str("x = h'CAFE'", false).unwrap();
    let _rendered = ast.to_string();
}
```

Parsing succeeds and stores decoded bytes `[0xca, 0xfe]`. `Display` for `Type2::B16ByteString` at
`src/ast/mod.rs:1314-1319` nevertheless calls `core::str::from_utf8(value)` on those decoded binary
bytes. It returns `fmt::Error`; `ToString::to_string()` then panics with:

```text
thread 'main' panicked at .../library/alloc/src/string.rs:2916:14:
a Display implementation returned an error unexpectedly: Error
```

The defect is independent of validation. A UTF-8-valued hex literal can hide it; `h'CAFE'` is the
minimal non-UTF-8 discriminator. The correct renderer must hex-encode the decoded bytes (and restore
the `h'…'` syntax) rather than interpret them as text.

### Local accommodation and anti-broadening guard

cddl-codegen no longer eagerly formats a group entry merely because it is a one-entry array.
`group_entry_source_desc_for_diagnostic` is called only after the unsupported occurrence case is
known, and `type_source_desc_for_diagnostic` renders a fixed value from the owned byte-safe IR
spelling. Non-literal types still use the upstream AST display; this is not a blanket replacement
or a parser fork.

`bare_fixed_byte_member_generates_and_occurrence_rejects_gracefully` uses non-UTF-8 `h'CAFE'` under
both default and preserve profiles: `[h'CAFE']` must generate, while `[* h'CAFE']` must reject
gracefully with the byte literal and cardinality reason. The recombination ingredient also uses
uppercase `h'CAFE'`, so the systematic recombination sweep covers other placements and would
re-expose an eager AST-display path as a panic.

### Removal and recheck criteria

After updating the rust-cddl pin, run the minimal `ast.to_string()` repro and require a byte-safe
rendering. Then re-run the focused fixed-member regression and the complete recombination sweep.
The local lazy renderer is defensive and preserves better diagnostic control, so an upstream fix
does not require its removal; simplify it only if the same tests prove every affected supported and
rejected path retains its behavior and message. Update README gap #17 and the roadmap close-out to
distinguish an upstream fix from a deliberate decision to keep the local renderer.
