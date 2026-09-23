# Probe discipline

Read this before designing scratch reproductions, adding fixtures, writing implementation premises, or reviewing behavioral and absence claims.

## Explicit inputs

Spell every coordinate a conclusion depends on, including profile/registry rows.
For generation outside the repository root, pass `--static-dir <checkout>/static`: its default is relative to the process working directory.
Wasm defaults to true; a Rust-only or component-only probe needs `--wasm=false`.

For claims about the repository's compiler pin, use `rustup run <pin> cargo ...` or build from inside the repository.
Scratch commands without `RUSTUP_TOOLCHAIN` can select the rustup default instead.
Cargo spawned by the repository's own `cargo test` inherits the pin through `RUSTUP_TOOLCHAIN`, including in scratch directories; do not assume the same of shell- or Bun-launched cargo.
A scratch `E0463: can't find crate for core` is an environment-red baseline: rerun with the exact pin and provision its target before attributing a generator defect.

Prefer separate `CARGO_TARGET_DIR`s or unique package names for scratch crates.
Never share a target directory between same-name crates unless every tree is made newer immediately before its build; otherwise Cargo can reuse another tree's artifact and produce a false pass.
The shared-target verifier deliberately touches each cell before every missed build.

## Scope and review

Treat a plan's code-behavior premises as claims to probe empirically before implementation, including claims supported by code comments or prior review.
Reviewers must independently verify premises on which their approval depends.
State evidence as “probed against X (tier T); not probed against Y,” including relevant shapes, profiles, and parse paths.
A broader conclusion than the probe supports is a review finding.
CI runs only `fast`, so it cannot discharge an unprobed `full` requirement.

Establish a negative premise by enumerating and checking the mechanism's members: gate registry entries, module test functions, or registry constants.
A keyword search can establish a positive finding; no hits cannot establish absence when another implementation uses different vocabulary.

## Fixture registration and coverage

When adding fixtures, check the owning registry and the gate and tier that enforce it.
Adding `tests/<dir>/input.cddl` requires a `CORPUS_PARITY_INPUTS` or `CORPUS_PARITY_EXCLUDED` row in `src/tests/wasm_parity_tests.rs`; other trees have their own registries.
`fast` executes the Rust substring filter `snapshot_tests`; other Rust tests run at `local` or later.
`clippy --all-targets` checks test compilation, not whether those tests pass.

Use [verification operations](verification.md) for logging, execution, and interpreting partial runs.
