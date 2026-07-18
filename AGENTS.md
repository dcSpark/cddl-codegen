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
3. `intermediate/` has the IR data structures that everything else works against — split into `mod.rs` (`IntermediateTypes` + scopes), `idents.rs`, `rust_type.rs` (`RustType`/`ConceptualRustType`), and `structs.rs` (`RustStruct`/`RustRecord`/`EnumVariant`/generics).
3. `generation/` walks the IR and emits the per-type Rust/WASM/JSON source (Rust built with the `codegen` builder crate). It's the largest area of the codebase — split by concern into `mod.rs` (the `GenerationScope` orchestration) plus `serialize`, `deserialize`, `records`, `enums`, `wrappers`, `collections`, `requests`, `bounds`, and `export`.
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
    `SeedOnce` keys check existence only" (the `--export-static-crate` target's `Cargo.toml` is the
    same exception class applied to a crate *outside* the output dir — same changeset machinery,
    with a co-owned contract: deps only asserted never removed, package identity seed-only); (2) each generated crate root `src/lib.rs` (rust, wasm,
    json-gen) is a seed-once thin root — written on a first export, then skipped if the file exists
    (existence check only, same bounded wording as the manifest `SeedOnce`; all generated code lives
    under the always-clobbered `src/generated/**`, alongside the dep-side extern-interface export
    `extern-interface/<dep>/**` — a committed, tool-owned, always-clobbered sibling tree
    delete-and-recreated each run, freshly projected from the finalized IR with no prior-output read);
    (3) the comment/code-preservation overlay
    (`comment_preserve.rs`) — `export()` reads a prior generated `src/generated/**` `.rs` whose only
    effects on fresh content are (a) inserting comment bytes and tagged regions
    (`cddl-codegen:unpreserved-comment` compile_error blocks and `cddl-codegen:replace`/`insert` user
    blocks), and (b) removing exactly the token span that a replace block's recorded original
    identifies — never any other code token, in either direction (default on, `--no-preserve-comments`
    disables it). Two diagnostic-only stderr warnings read prior output but change no output bytes: the
    legacy-root
    check (missing `mod generated;`) and the stale-file scan (orphaned `.rs` under the generated
    trees). Nothing reads prior *tool* output to decide what code to generate, so "run twice = run
    once = clean run" still holds. (Cross-crate request sidecars — `--wrapper-requests` /
    `--key-requests` reading a CONSUMER's committed `borrowed_collections.rs` /
    `borrowed_key_types.rs`, and `--extern-import` reading a DEPENDENCY's committed
    `extern-interface/<dep>/**` export — are explicit INPUTS from another crate, not this run's prior
    output; same inputs → same bytes still holds.)
- **Never regenerate a downstream CONSUMER repo (e.g. a CML checkout) to validate a change.**
  Generation clobbers the consumer's `src/generated/**`, and those working trees may hold large
  uncommitted migrations — a "just regen it to prove the fix" step nearly destroyed one. Validate
  against a synthetic fixture mirroring the consumer's shape, generated into a scratch/throwaway
  directory (the `@used_as_key` flavor delivery did exactly this: an `Ord`-refusing extern + a
  flavored union, generated to scratch, `cargo check`ed both directions). Regenerate a real
  consumer only when its owner explicitly asks, with their tree committed.
- **The IR borrows the AST.** `IntermediateTypes<'a>` can't be returned from a function that parses
  internally — drive the pipeline through the scoped callback in `api.rs` (it owns the AST).
- **bin/lib module duplication.** `main.rs` and `lib.rs` each declare the module list — a new
  production module goes in **both** (`src/tests/` is bin-only; test-only library API is
  `#[cfg(test)]`; this mismatch is gated by `bin_and_lib_production_module_declarations_match`).
  Keep `snapshot_tests`/`robustness_tests`/`integration_tests` in test module
  paths — CI and documented commands select tests by substring.
- **The CLI flags change codegen substantially** (preserve-encodings, canonical, json, wasm, …).
  When behaviour depends on a flag, check `cli.rs` and `docs/docs/command_line_flags.mdx`.
- **Some comments and panic messages are load-bearing test keys.** `LOCKSTEP`-paired comments and
  the panic-class ledgers in `src/tests/recombination_tests.rs` (keys are substrings of
  `<message> @ <file> @ fn <symbol>`) mean: moved code carries its comments verbatim; a pinned
  panic message is never reworded; relocating or deleting a guarded site fires the stale-pin guard
  by design — update/prune the ledger entry in the same commit (two keys are enforced only by
  full-tier `#[ignore]`d gates, so reason about them explicitly rather than trusting a local run).
- **Deliberately-unrefactored structure — don't re-litigate without new evidence** (decided during
  the 2026-07 `src/` refactoring series; rationale in that series' commit messages): string-based
  emission stays (the rustfmt post-pass + snapshot corpus + comment-preservation overlay all key on
  emitted-token stability — an AST/quote emitter breaks the overlay); the `codegen`-crate
  workarounds (newline-smuggled attrs, the `derivative)]` hack) stay isolated — the real fix is
  upstream; no `Ctx { types, cli }` param-pair struct (borrow-splitting `&mut GenerationScope` +
  `&IntermediateTypes` gets harder behind one struct); `api::with_types` stays one linear
  narrative; `print!` progress logging stays (humans and tests consume it as-is); the twin
  non-empty collision detectors stay twins unless a third container kind appears (their message
  texts differ meaningfully and are pinned).

## Git workflow

- New features should be built on master directly instead of branching unless justified (ex: a worktree)
- Commit unsigned to avoid GPG prompts
- **Another session can COMMIT to master while yours runs — re-read `git status`/`git log` at every
  commit point, not just at session start.** Proven 2026-07-18: a concurrent editor session landed
  `c7a3289` between one agent session's own commits, and that session's sub-agent separately watched
  the same foreign edits appear and vanish in the working tree mid-task. Two concrete traps: a
  `git add -A` without an immediately-preceding fresh `git status` read can sweep a concurrent
  session's uncommitted edits into your commit; and any commit-range reasoning done at session start
  (attribution, "my parent commit is X") silently retargets when a foreign commit interleaves.
- **The conversation-start git snapshot can be stale — never baseline against it.** The harness's
  session-start "Recent commits"/HEAD snapshot once lagged 14 commits behind the repo's real HEAD
  (and its dirty-status flavor has also fired: a snapshot listing ~30 modified files over a tree
  that `git status` showed clean — run `git status` fresh before reasoning about tree state);
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

The heavy gates memoize their nested-cargo work per generated-crate content hash (the gate cache —
`tests/README.md` § "The gate cache (memoize-and-skip for nested cargo)"): unchanged cells skip
with a visible `[gate-cache] … cached PASS` line, so re-runs after small changes cost far less
than the cold wall times. `GATE_CACHE=0` forces everything to run.

Rules:
- **CI runs the `fast` tier ONLY** (cost policy — see `tests/README.md` § "CI policy"). Never add
  steps to `build.yml` or promote a gate into `fast` — that's a maintainer decision; new gates
  default to `local`/`full`. The heavy correctness gates are therefore a LOCAL responsibility: run
  the appropriate tier yourself; CI won't catch what fast doesn't cover.
- **Run multi-minute gates in the foreground** with an extended tool timeout (up to 10 min), never
  detached into background monitors — detached runs strand their results when the agent stops.
- **A gate too long for a foreground timeout (e.g. `check.ts full`) may run as a harness-tracked
  background task** — and note the death-with-turn failure
  mode is specific to SUB-AGENTS' backgrounded runs: a MAIN-session harness-tracked background task
  survives the session's turn ends (proven 2026-07-17 — a `check.ts` local run kept executing across
  a turn boundary to a clean exit-0; `pgrep` the process before assuming orphaning and re-running a
  multi-minute gate).
- **Evidence preservation: every multi-minute run leaves its FULL output in a file under
  `draft/logs/`.** `check.ts` does this ITSELF — every run tees its complete output to a
  timestamped `draft/logs/check-<tier>-<stamp>.log` and prints the path at start and end; cite
  that path, never re-pipe the run. For everything ELSE that runs minutes (an isolated
  `cargo test --bin cddl-codegen <gate>` confirm, a standalone `bun run verify.ts`, corpus
  mints), redirect full output to a `draft/logs/` file yourself from the FIRST run. Rationale,
  learned expensively: piping through `tail` truncates the failure detail and masks the exit code
  (the pipeline reports `tail`'s); a one-line summary of a failed run is unactionable, and a
  transient failure whose only sighting went through `tail`/`grep` is evidence burned — reruns
  come back green and the flake stays unattributed. Proven end to end by the
  `acquire_scratch_lock_serializes` watch in `tests/TESTING_ROADMAP.md`: four unattributed
  sightings (three lost to `tail`/`grep`/truncation), then the fifth — full-logged under this
  rule — attributed and retired the flake in the same session. Logs are run evidence, not
  write-ups: they live in `draft/logs/` (gitignored, bulk-cleanable), never loose in `draft/`,
  which is reserved for investigation/proposal documents.
- **A fail-fast FAIL plus a single-gate retry is NOT a tier pass.** Fail-fast SKIPS every gate
  after the failure point, so "the failed gate passed on isolated retry" leaves the rest unrun —
  re-run the tier before claiming it green (the gate cache keeps already-passed cells cheap). A
  shipped "check.ts full green" commit claim was falsified exactly this way (2026-07-18, caught in
  review: 8 full-tier gates never ran on the shipped tree).
- **When the tier CANNOT go green (e.g. another session's uncommitted state fails an early gate),
  a "my work is green" decomposition must enumerate every gate fail-fast skipped and run each
  one's underlying suite — omitting one is the same falsified-claim class.** Proven 2026-07-18: a
  commit adding a comment-DSL directive ran its tier, fail-fasted on a foreign `build_matrix_check`
  failure BEFORE `project_corpus` ever ran, and shipped with the directive missing from
  `corpus_detect.ts`'s LOCKSTEP mirror — the drift its skipped gate exists to catch — masked for
  two further commits until a clean-tree tier run surfaced it. The skipped-gate list is in the
  tier's own output; walk it, don't sample it.
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
- Opus 4.8 for implementing anything with a clear implementation plan

For workflows:
- Always pass an explicit `model:` in normal `agent()` calls to avoid auto-inheriting Fable as the model.
- Never fan out multiple Fable agents without explicit user permission. A single Fable workflow agent also needs separate permission. If Fable is really needed, prefer using it inline.
- Generally avoid running tests in parallel agents unless explicitly intended, since this can happen accidentally when using multiple parallel high-capability agents for implementation.

For sessions that spawn their own sub-agents (an orchestrating session, or a subagent delegating to Opus):
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
  Same rationale as the foreground rule for multi-minute gates above.
- **Write the operational rules INTO the delegation prompt — sub-agents don't reliably act on this
  file even when instructed to read it (and the review must diff the report against the plan
  item-by-item).** Two same-session instances from the corpus-decode-leg delivery: an implementing
  sub-agent that had read this file still ended its turn to "stand by" for its own backgrounded
  gate run (the exact stall above — the run died with its turn, leaving no log and no process); and
  another silently dropped one item of a reviewed plan (an ENOSPC preflight), visible only by
  checking its completion report against the plan point-by-point — a report reads complete on its
  own terms. So: spell out the multi-minute-gate run discipline (foreground, extended timeout,
  full-output-to-file) in every delegation prompt that runs gates, re-assert it in mid-task
  corrections, and treat plan-vs-report diffing as a mandatory review step, not a spot check.


## Markdown formatting

A lot of components of this library have markdown files following two different structures:
1. `README.md` which stores the *current* state of the project. It shouldn't contain historical notes, unless important for backwards-compatibility
2. `ROADMAP.md` which stores the *future* state of the project. It shouldn't contain "done" marks (always be future-facing) unless context for a partially completed item is important for a future item

Entries in both projects should generally avoid "we tried X, then we did Y", and instead prefer "we did Y, to avoid issues like X". Otherwise, it's unclear if Y was the proper fix, whereas if you start with Y and properly justify it, it's easier to understand as an approach reached through thinking from first principles and easier to verify for correctness (important for our test-driven development)

Given this means we actively prune ROADMAP as features are implemented, code should generally not store references to roadmap items long-term. They can be acceptable as an intermediate step (i.e. call-outs so reviewing agents know how to code maps to implementation plans), but should generally be fixed up before features are shipped. Never cite a roadmap item by NUMBER or position ("ROADMAP item <N>") in any document or comment: pruning/renumbering retargets a positional citation silently — it never dangles, so no existence check can flag it. Cite a stable identifier instead (a pin/test/gate name, the delivered system's doc section, or the item's exact title): those fail loudly and greppably when the referent goes away. Both halves are mechanically enforced by the `lint_doc_citations` gate (check.ts `local` tier): it bans the positional form tree-wide (outside `draft/`) and asserts hand-doc citations still resolve.

Note: there is no roadmap that isn't related to the testing framework. That's because a "feature" roadmap is encoded indirectly in tests: any test that fail is a feature we need to support, and any new feature we decide to add should be encoded as a test (that first fails, then passes when the test is implemented)

Additionally, `draft/` is the recommended location for scratchpads (for agents to write/iterate on investigations, etc.). Run LOGS do not go in the `draft/` root — they go in `draft/logs/` (`check.ts` writes its own there automatically; put ad-hoc command logs there too), so the root stays readable as documents-only.

## Testing & further docs

- `tests/README.md` — how the test layers work and how to add/bless snapshots.
- `tests/TESTING_ROADMAP.md` — prioritized plan for the next testing improvements.
- `docs/docs/*.mdx` — authoritative user-facing reference: `current_capacities` (supported CDDL +
  limitations), `command_line_flags`, `comment_dsl`, `output_format`, `wasm_differences`.
- `supported.cddl` and `example/` — example specs to run the tool against.
- `GENERATING_MULTIPLATFORM_LIB.md` — an example document provided by CML - a consumer of this library
- specifications (ex: RFCs) for CBOR and CDDL can be found here in cddl-matrix/sources/
