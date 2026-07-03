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

1. Output must be deterministic. Note: There are two *different* properties are in play.
    1. *Reproducibility* (same input → byte-identical output)
        1. We use stable data structures like `BTreeMap`/`BTreeSet` throughout — never `HashMap` (otherwise it would depend on hash iteration order)
    2. *Canonical layout*
        1. Stable item ordering of concepts (structs, modules), which is done by `codegen`'s sort
        2. any modifications done by a `rustfmt` post-processing
- **The IR borrows the AST.** `IntermediateTypes<'a>` holds references into the parsed CDDL AST, so
  it can't be returned from a function that parses internally. The pipeline is driven through a
  scoped callback in `api.rs` that owns the AST for the duration of the call — use that pattern
  rather than trying to hand the IR back to a caller.
- **bin/lib module duplication.** `main.rs` and `lib.rs` each declare the module list, and the
  test suite (`src/tests/`) is declared from `main.rs` only (bin crate). A new production module
  must be added to **both**; test-only library API is `#[cfg(test)]`. Test module *names* are
  load-bearing: CI and documented commands select tests by substring (`cargo insta test --
  snapshot_tests robustness`, `cargo test --bin cddl-codegen <name>`), so keep
  `snapshot_tests`/`robustness_tests`/`integration_tests` in the module paths.
- **The CLI flags change codegen substantially** (preserve-encodings, canonical, json, wasm, …).
  When behaviour depends on a flag, check `cli.rs` and `docs/docs/command_line_flags.mdx`.

## Git workflow

- New features should be built on master directly instead of branching unless justified (ex: a worktree)
- Commit unsigned to avoid GPG prompts
- Agent worktrees can be cut a few commits behind master. If you're an agent working in a worktree,
  first verify your task's referenced commits are ancestors (`git merge-base --is-ancestor <sha> HEAD`)
  and merge master in if not — file:line references in your task spec assume master's tip.

## Build & verify

`bun run check.ts` at the repo root is the one entry point for verification: a gate registry with
three tiers, each a superset of the previous.
- `bun run check.ts fast` — what CI runs: fmt + clippy + snapshot tests + the drift gates.
- `bun run check.ts` (`local`, default) — fast + the workspace build and full `cargo test` suite.
  Run this before considering work done.
- `bun run check.ts full` — `local` + every manual-only gate (the `#[ignore]`d gates, `verify.ts`,
  `corpus_detect.ts`, the fuzz compile-rot check). Run this before shipping a feature.

Every run prints the full registry as a PASS/FAIL/SKIPPED/STUB table and exits non-zero on any fail;
its first gate self-checks that every manual gate, IOU stub, and `cddl-matrix` script is registered,
so the "run everything" set can't silently rot. See `tests/README.md` § "Running everything".

**CI runs the `fast` tier ONLY.** `.github/workflows/build.yml` is a thin invoker of
`bun run check.ts fast` — it must never grow its own steps (enforced: check.ts's `self_checks` gate
fails on any other run step in the workflow). Keep the fast tier the absolute minimum: this project
has a sole maintainer, and the rate at which AI makes commits means there is no budget for anything
beyond a quick CI. New gates default to the `local` or `full` tier; promoting a gate into `fast` is
a maintainer decision, not something an agent does in passing. The heavy correctness gates (full
`cargo test`, corpus + wasm-matrix compiles) are therefore a LOCAL responsibility — run the
appropriate tier yourself before finishing work; do not rely on CI to catch anything the fast tier
doesn't cover.

This repo follows test-driven development (TDD).
That means that for every failure, we generally want to think about what could have systematically caught that failure.
Sometimes it could have been caught systematically, but the system didn't have the right test vector, in which case we can add it if we think that's the best approach

Generally, for any test failure, we have to think from first principles about how we could have avoided this in the first place.
If no system exists that could have got it, a description of the missing system may exist (or have to be added) in the testing roadmap: `tests/TESTING_ROADMAP.md`

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

Creating a workflow with many parallel Fable agents needs permission to run

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
