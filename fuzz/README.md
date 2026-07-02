# Adversarial CBOR fuzzing

`cargo-fuzz` target over a generated crate's `from_cbor_bytes` — the untrusted-input surface this
library's consumers (chain data) actually parse. No other test layer covers hostile bytes: the
`--emit-tests` harness generates *valid* values by construction, and `catch_unwind`-based harnesses
can't observe an OOM or a stack overflow, so the fuzz process boundary is the only oracle for those.

The crate under test is **generated** (`generated/`, gitignored). Regenerate it + the seed corpus
(the hand-derived RFC 8949 vectors from `tests/golden_hex`) first:

```sh
./generate.sh
cargo +nightly fuzz run from_cbor_bytes            # needs: cargo install cargo-fuzz, a nightly toolchain
```

Two oracles per input: (1) no panic/abort/OOM/stack-overflow; (2) preserve-encodings round-trip
fidelity — anything `from_cbor_bytes` accepts must re-encode byte-identically.

Not a CI gate (needs nightly + unbounded time) — run it periodically and when touching
deserialization. Findings graduate to committed regressions: codegen-owned crashes become a
`tests/core` `structural_rejects` case; dependency-level ones (e.g. `cbor_event` length-prefix
over-allocation) are ledgered in `cddl-matrix/ROADMAP.md`.
