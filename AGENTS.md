# AGENTS.md

Orientation for AI agents working in this repo. The durable value here is the **pipeline shape** and
the **invariants/gotchas** — things you can't easily re-derive by reading the code. Treat file and
function names as starting points to grep from, not guarantees (the code moves; this doc lags).
Anything with its own self-updating home — the CLI flags, the exact CI gate, what CDDL is supported
— is linked, not restated.

`cddl-codegen` is a CLI + library that generates a Rust crate (plus optional WASM bindings and JSON
helpers) implementing CBOR serialize/deserialize from a CDDL specification.

## Architecture (the mental model)

Pipeline — `CDDL text → AST → IR → emitted source`:

1. The `cddl` crate parses the spec to an AST.
2. `parsing.rs` walks the AST and builds the **intermediate representation**, `IntermediateTypes`
   (in `intermediate.rs`). This IR is the central data structure everything else works against.
3. `generation.rs` walks the IR and emits the per-type Rust/WASM/JSON source (built with the
   `codegen` builder crate, then run through `rustfmt`). It's the largest area of the codebase.
4. `api.rs` orchestrates the pipeline; `main.rs` is the CLI entry, `lib.rs` the library entry.
   Other modules: `cli.rs` (flags), `comment_ast.rs` (the `@name`/`@doc`/`@newtype` comment DSL),
   `dep_graph.rs` (rule ordering).

**`static/` is not generated code.** It holds the hand-written serialization *runtime* and the
crate/package templates, which get copied/concatenated into the generated crate. Consequence:
changing the *runtime behaviour* of generated code usually means editing `static/`; changing the
*per-type emitted code* means editing the generator. Figure out which your task needs.

## Invariants & gotchas (the things that bite)

- **Output must be deterministic.** Emission uses `BTreeMap`/`BTreeSet` throughout — never
  `HashMap` — so generated output never depends on hash iteration order. Introducing a `HashMap` on
  an IR/emission path makes output nondeterministic and the snapshot suite flaky. This is a hard
  rule, not a style preference.
- **The IR borrows the AST.** `IntermediateTypes<'a>` holds references into the parsed CDDL AST, so
  it can't be returned from a function that parses internally. The pipeline is driven through a
  scoped callback in `api.rs` that owns the AST for the duration of the call — use that pattern
  rather than trying to hand the IR back to a caller.
- **bin/lib module duplication.** `main.rs` and `lib.rs` each declare the module list, and the
  tests live in the bin crate. A new module must be added to **both**; test-only library API is
  `#[cfg(test)]`.
- **Generated source is `rustfmt`-formatted by the generator**, so output formatting tracks the
  pinned toolchain (`rust-toolchain.toml`).
- **`codegen` is a git-fork dependency** (`dcSpark/codegen`), not a crates.io crate — it's the
  source-builder API the generator uses.
- **The CLI flags change codegen substantially** (preserve-encodings, canonical, json, wasm, …).
  When behaviour depends on a flag, check `cli.rs` and `docs/docs/command_line_flags.mdx`.

## Build & verify

The toolchain is pinned, so plain `cargo` uses the right version. The authoritative "what must pass"
is the CI workflow (`.github/workflows/build.yml`) — run the same things before considering a change
done: formatting (`cargo fmt`), `clippy`, build, the test suite, and the snapshot orphan check
(`cargo insta test --unreferenced=reject`, which needs `cargo-insta`).

If a change *intentionally* alters generated output, snapshots fail by design; re-bless with
`INSTA_UPDATE=always cargo test` and then **review the snapshot diff** — that diff is the main
signal your change did what you intended and nothing more.

## Testing & further docs

- `tests/README.md` — how the test layers work and how to add/bless snapshots.
- `tests/TESTING_ROADMAP.md` — prioritized plan for the next testing improvements.
- `docs/docs/*.mdx` — authoritative user-facing reference: `current_capacities` (supported CDDL +
  limitations), `command_line_flags`, `comment_dsl`, `output_format`, `wasm_differences`.
- `supported.cddl` and `example/` — example specs to run the tool against.
- `GENERATING_SERIALIZATION_LIB.md` — the pre/post-processing workflow for regenerating
  cardano-serialization-lib with this tool.
