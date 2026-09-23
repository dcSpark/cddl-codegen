# Verification operations

Read this before running verification, preparing a fresh checkout for tests, recovering a failed run, or changing the verification harness.
Use [probe discipline](probes.md) for scratch generation and behavioral claims.

## Preparation and execution

A fresh checkout needs `./fuzz/generate.sh` before its first tier run: the workspace references ignored `fuzz/generated` crates before the fuzz gate can create them.
Run `bun install` in `cddl-matrix/` for `matrix_typecheck`.
The declared Rust toolchain normally provisions `thumbv7m-none-eabi`; otherwise provision that target for the exact repository pin (`rustup target add thumbv7m-none-eabi` under that toolchain).
Targets are per-toolchain; a missing target makes `no_std_check` skip loudly at `local` and fail at `full`.

Check both `free` and `df` before launching a tier, and coordinate with other sessions running heavy gates.
Bound peak memory by `(gates in flight) × (rustc per gate) × (per-rustc resident set)`; no factor may scale with `nproc`.
`check.ts` preflights memory and scratch, degrades concurrency or refuses below its floors, and supplies memory-derived `CARGO_BUILD_JOBS` to batched gates.
Its start-time preflight cannot reserve resources against another session's later run.
For harness changes, consult only [Gate-level concurrency](../../tests/README.md#gate-level-concurrency-registry-declared-opt-in).

Run multi-minute gates in the foreground with an extended timeout, up to ten minutes; never detach them into background monitors.
A longer run may use a main-session harness-tracked task where the harness supports it.
The main session runs `full`; sub-agents may run only gates fitting their foreground timeout and must report remaining verification to the parent.
Before restarting an apparently orphaned run, check whether its process is still alive.
Claude-specific task lifetimes and recovery are in [Claude operation](claude.md).

Serialize publishing gates against implementation edits.
`verify.ts` rewrites `cddl-matrix/annotations/cddl_codegen.toml`: coordinate with editors and inspect the dirty marker/status before running it; inspect its annotation diff and rebuild the derived matrix afterward before using the result as evidence.

## Recovery and evidence

Before killing an orphan, derive its PIDs from the stopped invocation's process tree and named scratch paths, confirm its parent is dead, and kill by PID.
Never use tool-generic process patterns.
When agents share a harness, ancestry cannot attribute ownership: identify the invocation by its log class (`check-only-*` versus `check-(fast|local|full)-*`), banner (tier/jobs/`--only`), and issuing party; do not kill—coordinate.
Treat an unexplained exit `-15` as possible cross-session termination before attributing a harness flake.

Capture every multi-minute command's full output under `draft/logs/` from its first run.
`check.ts` captures its own output; other commands need explicit capture.
Do not use `tail`/`grep` as the only capture: it loses failure evidence and can mask the command's exit status.

Rerun the tier after a fail-fast failure before claiming a tier pass; an isolated retry or `--only` selection is never a tier verdict.
If foreign state prevents a green tier, any separate claim that your work is green must enumerate every skipped gate and run every underlying suite; report the unresolved tier failure explicitly.
Heavy gates cache passes by generated-crate content hash and print `[gate-cache] … cached PASS`; `GATE_CACHE=0` forces execution.
For selection semantics, consult [Running a subset](../../tests/README.md#running-a-subset---only-gategate).
For every failure, add the missing regression vector or record the missing system in `tests/testing-roadmap.toml`.

A log path is a disposable working artifact, not evidence of record.
Reports, commits, and documents must carry the conclusion and relevant numbers: wall time, exit signature, and tier verdict.
Session reports may additionally link their logs.
If retention warns that a committed document cites a log, put the fact in the document rather than retaining the log as its substitute.
Current gate durations belong in `tests/timings.json`, not prose.
The ignored `draft/timings.jsonl`, `draft/timing-cells.jsonl`, and `draft/memory-peaks.jsonl` are disposable local ledgers.
For ledger maintenance, consult [Measured gate durations](../../tests/README.md#measured-gate-durations-teststimingsjson).
