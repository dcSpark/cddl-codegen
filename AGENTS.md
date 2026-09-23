# AGENTS.md

`cddl-codegen` is a Rust CLI and library that generates Rust CBOR implementations from CDDL, with optional WASM, JSON, and component surfaces.
Use Rust for production code and TypeScript on Bun for scripts.

## Architecture

Pipeline: CDDL text → `cddl` AST → `src/parsing.rs` → `src/intermediate/` IR → `src/generation/` emitted source.
`src/api.rs` orchestrates it; `src/main.rs` and `src/lib.rs` expose the CLI and library.
Treat filenames as search starting points; verify behavior in the current tree.

- `static/` contains handwritten runtime code and templates copied into generated crates; runtime behavior changes usually belong there. Generated Rust is `no_std`-capable.
- `src/tests/` tests this application and is bin-only. `src/emit_tests*.rs` is production code that generates tests into output crates.
- The IR borrows the AST. Use the scoped callback in `api.rs`; a function that parses internally cannot return its `IntermediateTypes<'a>`.
- **bin/lib module duplication:** declare new production modules in both `main.rs` and `lib.rs`; keep test-only library API under `#[cfg(test)]`. This is gated by `bin_and_lib_production_module_declarations_match`.
- Preserve `snapshot_tests` / `robustness_tests` / `integration_tests` in test module paths because commands select tests by substring.

## Read before the relevant action

Read the matching guide before acting, including when a task expands into a new area.
Read only the relevant sections of larger references; do not load every linked document at startup.

| Action | Required guide |
| --- | --- |
| Change generation, export, preservation, manifests, cross-crate inputs, or config convergence | [Generation contract](docs/development/generation-contract.md) |
| Change architecture, emission, logging, wrapper collision detection, or config | [Settled decisions](docs/development/decisions.md) |
| Set up a checkout, run verification, recover a run, or publish matrix annotations | [Verification](docs/development/verification.md) |
| Reproduce a defect, build scratch output, add fixtures, or rely on a code-behavior premise | [Probes and evidence](docs/development/probes.md) |
| Delegate work | [Delegation](docs/development/delegation.md) |
| Edit docs or roadmaps, finish a delivery, or change these instructions | [Documentation](docs/development/documentation.md) |

## Invariants and precautions

- Preserve byte-reproducible output and canonical layout. Use `BTreeMap` / `BTreeSet`, never `HashMap`; retain stable item ordering and the rustfmt post-pass.
- Prior-output reads are restricted to the generation contract's bounded exceptions. Preserve its manifest, seed-once, overlay, diagnostic, and config-convergence distinctions.
- Validate against synthetic fixtures generated into scratch directories. Never regenerate a real downstream consumer to validate a change; its owner must explicitly request regeneration and its tree must be committed.
- Preserve `LOCKSTEP` comments when moving code and never reword pinned panic messages. Relocating or deleting a guarded site requires updating its ledger in `src/tests/recombination_tests.rs` in the same commit; some pins run only in full-tier ignored gates.
- Check `src/cli.rs` and the relevant section of [command-line flags](docs/docs/command_line_flags.mdx) for flag-dependent behavior. Spell every flag and environment coordinate on which a probe's conclusion depends: `--wasm` defaults to true, `--static-dir` resolves against the process working directory, and scratch-directory cargo can bypass the repository's toolchain pin.
- The config `[runtime]` carrier derivation is maintainer-closed: do not investigate, reopen, or change it without explicit maintainer permission.

## Git workflow

Build features on `master` unless isolation justifies a branch/worktree.
Commit unsigned.
Another session can edit or commit concurrently: read fresh `git status` and `git log` before staging and at every commit point, and stage only your changes.
Before bisecting, baselining, or attributing behavior to commits, inspect actual repository topology (`git log`, `git rev-parse <first-commit>^`); a conversation-start snapshot is not a baseline.

## Build & verify

Use `bun run check.ts` from the repo root:

| Command | Requirement |
| --- | --- |
| `bun run check.ts fast` | CI tier: fmt, clippy, snapshots, and drift gates |
| `bun run check.ts` | Local tier; run before considering work done |
| `bun run check.ts full` | All tiers; run before shipping a feature |

CI runs `fast` only. Adding CI steps or promoting gates to `fast` requires a maintainer decision; new gates default to `local` / `full`.
The main session must run `full`; do not delegate it.
Coordinate heavy runs across sessions and check memory as well as disk before launching.
Never kill by a generic tool pattern; shared harness ancestry cannot prove ownership, so identify the invocation and coordinate.
Keep full output from every multi-minute run under `draft/logs/`; `check.ts` does this automatically.
A fail-fast run plus an isolated retry is not a tier pass: rerun the tier before claiming success.
For each failure, add the missing regression vector or record the missing system in `tests/testing-roadmap.toml`.

## Documentation and navigation

Write the current choice first, followed by the failure it prevents.
Keep README files about current behavior and TOML roadmaps about future work; use stable citations.
Finish each delivery with the documentation guide's confirm-or-fix sweep.
Use `draft/` for disposable investigation notes and `draft/logs/` for run output.

For feature details, find the relevant section in `docs/docs/`: `current_capacities`, `command_line_flags`, `comment_dsl`, `output_format`, `preserving_edits`, `config_file`, `wasm_differences`, or `component_differences`.
For test-layer design and adding/blessing fixtures, find the relevant section in `tests/README.md`; for matrix work use `cddl-matrix/README.md`.
Example specs live in `supported.cddl` and `example/`; `GENERATING_MULTIPLATFORM_LIB.md` is a consumer example, and `cddl-matrix/sources/` contains specification sources.

Keep this entry point within 1,200 words.
New guidance must change an actionable decision; place task-specific detail in its owning guide and add a read trigger here only when needed.
Preserve rules, exceptions, and useful rationale when editing; do not accumulate incident narratives or duplicate authoritative procedures.
