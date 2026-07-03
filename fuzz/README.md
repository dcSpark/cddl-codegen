# Adversarial CBOR fuzzing

`cargo-fuzz` targets over generated crates' `from_cbor_bytes` — the untrusted-input surface this
library's consumers (chain data) actually parse. No other test layer covers hostile bytes: the
`--emit-tests` harness generates *valid* values by construction, and `catch_unwind`-based harnesses
can't observe an OOM or a stack overflow, so the fuzz process boundary is the only oracle for those.

The crates under test are **generated** (`generated/`, gitignored). Regenerate them + the seed
corpora + the derived probe lists first:

```sh
./generate.sh
cargo +nightly fuzz run from_cbor_bytes             # needs: cargo install cargo-fuzz, a nightly toolchain
cargo +nightly fuzz run from_cbor_bytes_recursive
```

Two oracles per input (both targets): (1) no panic/abort/OOM/stack-overflow; (2) preserve-encodings
round-trip fidelity — anything `from_cbor_bytes` accepts must re-encode byte-identically.

Not a CI gate (needs nightly + unbounded time) — run it periodically and when touching
deserialization. What *is* gated (`check.ts` `full` tier, `fuzz_compile_rot`) is compile-rot: it runs
`generate.sh` (iff `generated/` is absent or `--refresh-fuzz`) then `cargo check`s the fuzz crate, so
a probe rename or a broken scrape regex fails a local `full` run.

## Two targets

- **`from_cbor_bytes`** → `generated/rust`, the preserve-encodings fixture
  (`tests/preserve-encodings/input.cddl`) — the richest deserialization surface (encoding-preserving
  paths, nested tables, type/group choices, bounds, defaults, custom serialization).
- **`from_cbor_bytes_recursive`** → `generated/recursive/rust`, generated from
  `tests/corpus/recursive.cddl` **with `--deserialize-depth-limit=64`** (its package is renamed
  `cddl-lib-recursive` → `recursive_lib` so two `cddl-lib` packages don't collide in one workspace).
  Its recursive-descent deserializer would overflow the stack on hostile-deep input; the depth limit
  turns that into a graceful `DeserializeFailure::DepthLimitExceeded`. This is the repo's only
  stack-overflow oracle, and it needs a recursive spec to be structurally capable of firing.

## Derived probe lists (no hand-sync)

The set of types each target fuzzes is **derived**, not hand-listed: `generate.sh` scrapes
`impl Deserialize for <T>` out of the generated crate's `serialization.rs` and writes a
`fn probe_all(data: &[u8])` into `generated/probe_list.in` / `generated/recursive_probe_list.in`
(gitignored), which the target `include!`s in item position and calls from its fuzz body. A new rule
in the fuzzed spec lands in the probe set with zero manual edits — drift is impossible, not merely
detected. A vacuity floor (20 types for the preserve crate, 1 for recursive) fails `generate.sh` if
a rotted regex silently shrinks the set.

## Seed corpora

- `from_cbor_bytes`: every hand-derived RFC 8949 hex vector in all three golden-hex suites (default +
  preserve + canonical). The crate is built with `--preserve-encodings`, so the preserve/canonical
  suites' irregular encodings (indefinite chunks, non-minimal heads) are exactly the inputs its
  decode paths exist for.
- `from_cbor_bytes_recursive`: the hand-built hostile-deep vector from
  `integration_tests::deserialize_depth_limit_guards_recursion` (100 000 nested
  `array(2), uint 0, array(1)` frames + an empty-children leaf — what SIGABRTs with the flag off),
  plus a few shallow valid `tree` values.

## Findings disposition

Findings graduate to committed regressions: codegen-owned crashes become a `tests/core`
`structural_rejects` case; dependency-level ones are ledgered in `cddl-matrix/ROADMAP.md`.

- The **`cbor_event` untrusted length-prefix over-allocation** (a `0x7b`/`0x5b`… 8-byte length
  header drives a multi-GB `Vec::with_capacity` before any payload byte is read → `capacity overflow`
  / OOM) is surfaced by `from_cbor_bytes` and already ledgered there (dependency-level, fix deferred
  upstream — see the "Untrusted length-prefix over-allocation" entry). It is not a codegen bug; the
  derived probe list simply exercises it across every deserializable type.
- The recursive target's depth guard was **validated against the failure class it defends**: built
  once *without* `--deserialize-depth-limit`, the hostile-deep seed reproduces
  `AddressSanitizer: stack-overflow … ABORTING` (confirming the fuzz process boundary can see the
  abort); built *with* the flag (the committed configuration), the same input returns
  `DepthLimitExceeded` and a 10 000-run smoke completes with no abort. The unguarded crate is not
  kept — an expected-to-abort target is not a committed regression.
