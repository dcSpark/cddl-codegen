# Generation contract

Read before changing generation, export, preservation, or config orchestration.
The pipeline and universal editing rules are in [AGENTS.md](../../AGENTS.md); architectural constraints are in [Decisions](decisions.md).

## Reproducibility and layout

The same explicit inputs must produce byte-identical output.
Use `BTreeMap`/`BTreeSet`, never `HashMap`; hash iteration order breaks reproducibility.
Canonical layout additionally requires stable item ordering through `codegen` sorting and the `rustfmt` post-pass.
Generated Rust is `no_std`-capable: use `core`/`alloc` paths and usage-derived per-file `extern crate alloc;` and imports.
The generator always emits the [no-std-check shim](../docs/output_format.mdx#the-no-std-check-shim-crate).

Generation must not inspect prior output to decide what code to generate.
The following bounded reads preserve user ownership, apply explicit preservation edits, or report diagnostics.
They must not become additional inputs to IR construction or emission.
The generation write boundary lives in [write_tail.rs](../../src/generation/write_tail.rs); config orchestration has the separate reads described below.

## Manifest merges and crate roots

Generated `Cargo.toml` files merge a declarative changeset through [cargo_manifest.rs](../../src/cargo_manifest.rs).
Keys no operation mentions pass through; `SeedOnce` checks existence only.
The `--export-static-crate` target uses the same machinery outside the output directory: package identity is seed-only and dependencies are asserted, never removed.
That dependency rule also applies to `--json-gen-dep` in `wasm/json-gen/Cargo.toml`, `--wasm-dep` in `wasm/Cargo.toml`, and `--rust-dep` in `rust/Cargo.toml`.
Dropping such a flag leaves a stale entry because its package name existed only in the flag value; claims that those surfaces do not add external dependencies or touch manifests are conditional on these flags.

Each generated rust/wasm/json-gen `src/lib.rs` is a thin root, written only when absent; this is an existence check, not content-dependent generation.
Generated code lives in the always-clobbered `src/generated/**` tree, subject to the preservation overlay below.
The committed, tool-owned sibling `extern-interface/<dep>/**` is deleted and recreated on every export, freshly projected from finalized IR without reading its prior contents.
See [generated roots](../docs/output_format.mdx#generated-crate-roots-thin-root-seed-once) and [manifest ownership](../docs/output_format.mdx#generated-cargotoml-merge-not-clobber).

## Preservation and final-content recomputation

[comment_preserve.rs](../../src/comment_preserve.rs) may read a prior generated `.rs` under `src/generated/**` only to contribute:

- Comment bytes and tagged regions: `cddl-codegen:unpreserved-comment` compile-error blocks and `cddl-codegen:replace`/`insert`/`keep` user blocks.
- Removal of exactly the token span identified by a replace block's recorded original.

The overlay must insert or remove no other code tokens.
Apply it to the in-memory file map before the write loop.
Then rerun usage-derived [import pruning](../../src/import_prune.rs) once over the post-overlay map, so an import loses its place when a replacement removes its last user.
Next, [alloc import injection](../../src/alloc_import_inject.rs) strips and recomputes its own `use alloc::…`/`extern crate alloc;` block, adding or removing imports as needed.
Rustfmt every written surface after injection: formatting stability is necessary for a second regeneration to reproduce the first.

These recomputations are pure functions of final content, not extra prior-output reads; they do not widen what prior output itself contributes.
Preservation defaults on; `--no-preserve-comments` disables it.
See [preservation guarantees](../docs/preserving_edits.mdx#guarantees-and-residual-limits).

## Diagnostic reads

These prior-output reads may produce stderr warnings but must change no output bytes:

- Legacy-root check: an existing root lacks `mod generated;`.
- Stale-file scan: orphaned `.rs` files remain under generated trees.
- Missing root re-export: a seed-skipped `lib.rs` lacks a name required by own-spec extern glue.
- New static-runtime file notice: `--export-static-crate` writes a previously absent runtime file into a hand-owned crate whose root needs a manual `pub mod <module>;` declaration.
  This notice checks pre-write existence, names the module, and stays silent on idempotent re-export.

## Explicit cross-crate inputs

Committed files read from another crate are explicit inputs, not this run's prior output:

| Input | Source |
| --- | --- |
| `--wrapper-requests` | Consumer's `borrowed_collections.rs` |
| `--key-requests` | Consumer's `borrowed_key_types.rs` |
| `--extern-import` | Dependency's `extern-interface/<dep>/**` |
| `--common-import-flavor` | Runtime exporter's `cddl-codegen-runtime-flavor.toml` |

[component_wit_deps.rs](../../src/component_wit_deps.rs) materializes consumer-side `wit/deps` for `--component` with `--extern-import`; these are explicit cross-crate inputs of the same determinism class as extern-interface reads.

## Config convergence and committed-state verdict

Config mode adds two observations of output after a pass:

1. Compare each consumed sidecar's bytes before and after the pass.
2. Check every `deps` edge's consumer `borrowed_collections.rs` against the dependency's `collections.rs` wrapper index.

Neither observation changes the bytes an individual generation emits from a given `Cli` and explicit inputs.
The committed-state verdict alone can change the exit code: it reports whether the committed tree builds, rather than changing generation.

The sidecar comparison also selects a convergence pass: rerun, once and in the same generation order, crates whose consumed sidecars the first pass rewrote.
Announce each rerun with the sidecar responsible.
Each rerun uses the identical `Cli` with settled inputs; this is necessary for “run twice = run once = clean run.”
Do not remove this scheduling effect under the claim that the comparison is diagnostic-only.

One extra pass suffices because a requested wrapper is all-one-dependency-owned; ownerless-wrapper index feedback cannot arise.
Sidecars depend only on a crate's own spec and its dependencies' `extern-interface/` exports, which the convergence pass does not change.
The retained warning fires only if a sidecar moves again across that pass, covering unsettled subsets or counterparties in another repository.
See [generation order and convergence](../docs/config_file.mdx#generation-order-and-the-convergence-pass) and [the committed-tree verdict](../docs/config_file.mdx#the-verdict-on-the-committed-tree).
