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
    directory. Three bounded exceptions, none feeding back into *what code* is generated: (1) the
    generated `Cargo.toml`s — `export()` merges a declarative changeset (`cargo_manifest.rs`) onto the
    existing manifest so user edits survive, bounded to "keys the op set doesn't mention pass through;
    `SeedOnce` keys check existence only"; (2) each generated crate root `src/lib.rs` (rust, wasm,
    json-gen) is a seed-once thin root — written on a first export, then skipped if the file exists
    (existence check only, same bounded wording as the manifest `SeedOnce`; all generated code lives
    under the always-clobbered `src/generated/**`); (3) the comment-preservation overlay
    (`comment_preserve.rs`) — `export()` reads a prior generated `src/generated/**` `.rs` solely to
    carry the user's comments onto the fresh content, bounded to "prior output contributes only comment
    bytes and tagged `cddl-codegen:unpreserved-comment` compile_error blocks — never a code token
    outside those tagged blocks" (default on, `--no-preserve-comments` disables it). Two
    diagnostic-only stderr warnings read prior output but change no output bytes: the legacy-root
    check (missing `mod generated;`) and the stale-file scan (orphaned `.rs` under the generated
    trees). Nothing reads prior *tool* output to decide what code to generate, so "run twice = run
    once = clean run" still holds.
- **The IR borrows the AST.** `IntermediateTypes<'a>` can't be returned from a function that parses
  internally — drive the pipeline through the scoped callback in `api.rs` (it owns the AST).
- **bin/lib module duplication.** `main.rs` and `lib.rs` each declare the module list — a new
  production module goes in **both** (`src/tests/` is bin-only; test-only library API is
  `#[cfg(test)]`; this mismatch is gated by `bin_and_lib_production_module_declarations_match`).
  Keep `snapshot_tests`/`robustness_tests`/`integration_tests` in test module
  paths — CI and documented commands select tests by substring.
- **The CLI flags change codegen substantially** (preserve-encodings, canonical, json, wasm, …).
  When behaviour depends on a flag, check `cli.rs` and `docs/docs/command_line_flags.mdx`.

## Git workflow

- New features should be built on master directly instead of branching unless justified (ex: a worktree)
- Commit unsigned to avoid GPG prompts
- **The conversation-start git snapshot can be stale — never baseline against it.** The harness's
  session-start "Recent commits"/HEAD snapshot once lagged 14 commits behind the repo's real HEAD;
  a "pre-feature baseline" repro run at the snapshot's HEAD attributed another commit's behavior
  change to the session's own work, and the wrong root-cause shipped in a commit before review
  caught it. Before any attribution that depends on a commit range (bisecting, baselining a repro,
  writing a root-cause or a ledger-retirement comment), read the actual topology from the repo —
  `git rev-parse <your-first-commit>^` / `git log` — and baseline against the TRUE parent.

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
- **A gate too long for a foreground timeout (e.g. `check.ts full`) may run as a harness-tracked
  background task, but redirect its FULL output to a file** — piping through `tail` both truncates
  the failure detail (check.ts gates inherit stdout, so the detail exists nowhere else) and masks
  the exit code (the pipeline reports `tail`'s). A one-line summary of a failed run is unactionable;
  you end up re-running the gate blind.
- **TDD.** For every failure, ask what could have systematically caught it: add the missing test
  vector, or record the missing system in `tests/TESTING_ROADMAP.md`.

## Which AI model to use

Fable 5 is the session orchestrator. The following should be inline in the main session:
- session orchestration
- implementation plan creation
- review of implementation/plan
- any problem deemed very hard
- tasks cheaper to inline (ex: run a single command)

### Delegation to other agents

- Never use Haiku
- Do not choose Sonnet 5 manually (only when Claude Code itself selects it or, or is used by tool)
- `codex` (via skill) for any investigation/implementation that is mostly mechanical
- Opus 4.8 for implementing anything with a clear implementation plan

For workflows:
- Always pass an explicit `model:` in normal `agent()` calls to avoid auto-inheriting Fable as the model.
- Exception: for Codex inside a workflow, use `agentType: 'codex:codex-rescue'` instead of `model:`, because `agent()` only accepts Anthropic models directly.
- Never fan out multiple Fable agents without explicit user permission. A single Fable workflow agent also needs separate permission. If Fable is really needed, prefer using it inline.
- Generally avoid running tests in parallel agents unless explicitly intended, since this can happen accidentally when using multiple parallel high-capability agents for implementation.

For sessions that spawn their own sub-agents (an orchestrating session, or a subagent delegating to
codex/Opus):
- **Never end your turn to "stand by" for a sub-agent's completion** — a stopped agent is only
  resumed by an explicit message from its spawner, so "armed watchers"/"completion callbacks" never
  fire and the session stalls until a human (or the coordinator) manually nudges it. Poll the
  sub-agent's transcript/output with bounded foreground waits (extended tool timeouts) and end the
  turn only when reporting completed, reviewed results. Two polling gotchas (each misread once
  before being learned): the harness's task `.output` paths are SYMLINKS to the real transcript
  (`stat -L`, or a bare `stat` measures the 150-byte link and reads as a dead agent); and an idle
  transcript does NOT mean stalled/done — nothing is written for the whole duration of a long
  foreground tool call (a `check.ts local` run is silent for ~4 min), so before invoking recovery,
  check the last entry's type (a trailing `tool_use` = mid-call) and for live build processes.
  Same rationale as the foreground rule for
  multi-minute gates above.
- The codex background runner can die silently mid-task (log stalls, pid dead, status stuck
  `running`) — detection signal + recovery procedure: `draft/codex-background-runner-silent-death.md`.
- Two codex-sandbox filesystem facts to bake into delegation prompts (each cost a failed step when
  learned): `.git` is mounted READ-ONLY (a codex agent cannot commit — tell it to leave work in the
  worktree and commit after in-session review), and `~/.npm` is read-only too, so npm-backed gates
  (the `test` gate's json2ts/wasm steps) need `npm_config_cache=/tmp/<something>` in the agent's
  environment or the local tier fails on EROFS mid-verification.
- The codex plugin runs write tasks under codex's `workspace-write` sandbox regardless of
  `~/.codex/config.toml`'s `sandbox_mode` (the companion hard-codes the per-job sandbox), and
  workspace-write turns network OFF unless the config's
  `[sandbox_workspace_write] network_access = true` is set — it is on this machine (verified by an
  in-sandbox crates.io probe). If a codex report shows registry/npm fetch failures
  (`index.crates.io` / npm unreachable) on dependency-resolving gates (generated-crate `cargo
  test`, the replay gate, check.ts's `test` gate), that flag has regressed: fix the config and
  re-run those gates in-session rather than treating the delegated work as verified.


## Markdown formatting

A lot of components of this library have markdown files following two different structures:
1. `README.md` which stores the *current* state of the project. It shouldn't contain historical notes, unless important for backwards-compatibility
2. `ROADMAP.md` which stores the *future* state of the project. It shouldn't contain "done" marks (always be future-facing) unless context for a partially completed item is important for a future item

Entries in both projects should generally avoid "we tried X, then we did Y", and instead prefer "we did Y, to avoid issues like X". Otherwise, it's unclear if Y was the proper fix, whereas if you start with Y and properly justify it, it's easier to understand as an approach reached through thinking from first principles and easier to verify for correctness (important for our test-driven development)

Given this means we actively prune ROADMAP as features are implemented, code should generally not store references to roadmap items long-term. They can be acceptable as an intermediate step (i.e. call-outs so reviewing agents know how to code maps to implementation plans), but should generally be fixed up before features are shipped. Never cite a roadmap item by NUMBER or position ("ROADMAP item <N>") in any document or comment: pruning/renumbering retargets a positional citation silently — it never dangles, so no existence check can flag it. Cite a stable identifier instead (a pin/test/gate name, the delivered system's doc section, or the item's exact title): those fail loudly and greppably when the referent goes away. Both halves are mechanically enforced by the `lint_doc_citations` gate (check.ts `local` tier): it bans the positional form tree-wide (outside `draft/`) and asserts hand-doc citations still resolve.

Note: there is no roadmap that isn't related to the testing framework. That's because a "feature" roadmap is encoded indirectly in tests: any test that fail is a feature we need to support, and any new feature we decide to add should be encoded as a test (that first fails, then passes when the test is implemented)

Additionally, `draft/` is the recommended location for scratchpads (for agents to write/iterate on investigations, etc.)

## Testing & further docs

- `tests/README.md` — how the test layers work and how to add/bless snapshots.
- `tests/TESTING_ROADMAP.md` — prioritized plan for the next testing improvements.
- `docs/docs/*.mdx` — authoritative user-facing reference: `current_capacities` (supported CDDL +
  limitations), `command_line_flags`, `comment_dsl`, `output_format`, `wasm_differences`.
- `supported.cddl` and `example/` — example specs to run the tool against.
- `GENERATING_MULTIPLATFORM_LIB.md` — an example document provided by CML - a consumer of this library
- specifications (ex: RFCs) for CBOR and CDDL can be found here in cddl-matrix/sources/
