# AGENTS.md

Orientation for AI agents working in this repo. The durable value here is the **pipeline shape** and
the **invariants/gotchas** — things you can't easily re-derive by reading the code. Treat file and
function names as starting points to grep from, not guarantees:
- the code moves
- docs lag
- some concepts have their own home in other documents

`cddl-codegen` is a CLI + library that generates a Rust crate (plus optional WASM bindings and JSON
helpers) implementing CBOR serialize/deserialize from a CDDL specification.

**Languages.**:
- Rust for the project itself (notably `src/`)
- TypeScript-on-Bun (`bun run <script>.ts`) for scripting

## Architecture (the mental model)

Pipeline — `CDDL text → AST → IR → emitted source`:

1. The `cddl` crate parses the spec to an AST.
2. `parsing.rs` walks the AST and builds the intermediate representation
3. `intermediate.rs` has the IR data structures that everything else works against.
3. `generation.rs` walks the IR and emits the per-type Rust/WASM/JSON source (Rust built with the `codegen` builder crate). It's the largest area of the codebase.
4. `api.rs` orchestrates the pipeline; `main.rs` is the CLI entry, `lib.rs` the library entry.
5. Other modules:
    1. `cli.rs` (flags)
    2. `comment_ast.rs` (the `@name`/`@doc`/`@newtype` comment DSL)
    3. `dep_graph.rs` (rule ordering)

**Which "tests" are which.** The app's own test suite lives under `src/tests/` (bin-crate-only,
`#[cfg(test)]`); everything else in `src/` is production — including `emit_tests.rs` /
`emit_tests_wasm.rs`, which are the shipped `--emit-tests` feature (they generate tests *into*
output crates, they don't test this app).

**`static/` is not generated code.** It holds the hand-written serialization *runtime* and the
crate/package templates, which get copied/concatenated into the generated crate. Consequence:
changing the *runtime behaviour* of generated code usually means editing `static/`; changing the
*per-type emitted code* means editing the generator. Figure out which your task needs.

## Invariants & gotchas (the things that bite)

- **Deterministic output** — two distinct properties:
  - *Reproducibility* (same input → byte-identical output): always `BTreeMap`/`BTreeSet`, never
    `HashMap` — hash iteration order breaks it.
  - *Canonical layout*: stable item ordering via `codegen`'s sort + `rustfmt` post-processing.
  - *No prior-output dependence* — generation must not read the prior contents of the output
    directory. Two bounded exceptions, both existence-only and neither feeding back into *what* is
    generated: (1) the generated `Cargo.toml`s — `export()` merges a declarative changeset
    (`cargo_manifest.rs`) onto the existing manifest so user edits survive, bounded to "keys the op
    set doesn't mention pass through; `SeedOnce` keys check existence only"; (2) each generated crate
    root `src/lib.rs` (rust, wasm, json-gen) is a seed-once thin root — written on a first export,
    then skipped if the file exists (existence check only, same bounded wording as the manifest
    `SeedOnce`; all generated code lives under the always-clobbered `src/generated/**`). A
    diagnostic-only stderr warning reads a legacy root to detect the missing `mod generated;` and name
    the migration, but changes no output bytes. Nothing reads prior *tool* output to decide what to
    generate, so "run twice = run once = clean run" still holds.
- **The IR borrows the AST.** `IntermediateTypes<'a>` can't be returned from a function that parses
  internally — drive the pipeline through the scoped callback in `api.rs` (it owns the AST).
- **bin/lib module duplication.** `main.rs` and `lib.rs` each declare the module list — a new
  production module goes in **both** (`src/tests/` is bin-only; test-only library API is
  `#[cfg(test)]`). Keep `snapshot_tests`/`robustness_tests`/`integration_tests` in test module
  paths — CI and documented commands select tests by substring.
- **The CLI flags change codegen substantially** (preserve-encodings, canonical, json, wasm, …).
  When behaviour depends on a flag, check `cli.rs` and `docs/docs/command_line_flags.mdx`.

## Git workflow

- New features should be built on master directly instead of branching unless justified (ex: a worktree)
- Commit unsigned to avoid GPG prompts

## Build & verify

`bun run check.ts` at the repo root is the one entry point for verification — a self-checking gate
registry with three tiers (details + wall times: `tests/README.md` § "Running everything"):
- `bun run check.ts fast` — what CI runs: fmt + clippy + snapshot tests + the drift gates.
- `bun run check.ts` (`local`, default) — fast + workspace build + full `cargo test`.
  Run this before considering work done.
- `bun run check.ts full` — local + every manual-only gate. Run this before shipping a feature.

Rules:
- **CI runs the `fast` tier ONLY** (cost policy — see `tests/README.md` § "CI policy"). Never add
  steps to `build.yml` or promote a gate into `fast` — that's a maintainer decision; new gates
  default to `local`/`full`. The heavy correctness gates are therefore a LOCAL responsibility: run
  the appropriate tier yourself; CI won't catch what fast doesn't cover.
- **Run multi-minute gates in the foreground** with an extended tool timeout (up to 10 min), never
  detached into background monitors — detached runs strand their results when the agent stops.
- **TDD.** For every failure, ask what could have systematically caught it: add the missing test
  vector, or record the missing system in `tests/TESTING_ROADMAP.md`.

## Which AI model to use:

- Use Sonnet 5 only if Claude Code internals themselves are recommending its use (sometimes happens for tool calls, etc.)
- use Opus 4.8 for
    - implementing anything with a clear implementation plan
    - doing any investigation/implementation that is mostly mechanical
- Use Fable 5 for
    - session orchestration
    - implementation plan creation
    - review of implementation/plan
    - any problem deemed very hard

For workflows:
- Do NOT run a workflow with many parallel Fable agents unless explicit permission is given. Fable jobs are almost always better done inline
- Permission to run a workflow with Fable is different for permission to run a multi-Fable workflow
- Generally avoid running tests at the same time in parallel agent (can happen if not careful using parallel Opus 4.8 agents for implementation)

## Markdown formatting

A lot of components of this library have markdown files following two different structures:
1. `README.md` which stores the *current* state of the project. It shouldn't contain historical notes, unless important for backwards-compatibility
2. `ROADMAP.md` which stores the *future* state of the project. It shouldn't contain "done" marks (always be future-facing) unless context for a partially completed item is important for a future item

Entries in both projects should generally avoid "we tried X, then we did Y", and instead prefer "we did Y, to avoid issues like X". Otherwise, it's unclear if Y was the proper fix, whereas if you start with Y and properly justify it, it's easier to understand as an approach reached through thinking from first principles and easier to verify for correctness (important for our test-driven development)

Given this means we actively prune ROADMAP as features are implemented, code should generally not store references to roadmap items long-term. They can be acceptable as an intermediate step (i.e. call-outs so reviewing agents know how to code maps to implementation plans), but should generally be fixed up before features are shipped.

Note: there is no roadmap that isn't related to the testing framework. That's because a "feature" roadmap is encoded indirectly in tests: any test that fail is a feature we need to support, and any new feature we decide to add should be encoded as a test (that first fails, then passes when the test is implemented)

Additionally, `draft/` is the recommended location for scratchpads (for agents to write/iterate on investigations, etc.)

## Testing & further docs

- `tests/README.md` — how the test layers work and how to add/bless snapshots.
- `tests/TESTING_ROADMAP.md` — prioritized plan for the next testing improvements.
- `docs/docs/*.mdx` — authoritative user-facing reference: `current_capacities` (supported CDDL +
  limitations), `command_line_flags`, `comment_dsl`, `output_format`, `wasm_differences`.
- `supported.cddl` and `example/` — example specs to run the tool against.
- `GENERATING_MULTIPLATFORM_LIB.md` — an example document provided by CML - a consumer of this library
