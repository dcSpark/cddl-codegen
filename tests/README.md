# Tests

cddl-codegen is tested in two complementary layers. Keep them distinct — they answer different
questions.

## Editing the testing roadmap

`tests/testing-roadmap.toml` is the testing roadmap — the authored TOML is the only committed
form. Edit it directly, format it with `cd cddl-matrix && bun run project_roadmaps.ts
--format-source tests/testing-roadmap.toml`, and validate with `bun run project_roadmaps.ts
--roadmap all --check` (the check renders both projections in memory, so a fresh edit is
check-clean once the TOML itself validates — there is no committed render to fall out of sync
with). A human-review markdown render can be generated on demand with `bun run
project_roadmaps.ts --roadmap testing --write`; it lands in the gitignored `draft/roadmaps/`
directory, may go stale between writes, and must never be committed (`lint_doc_citations` refuses
a tracked copy — agents read the TOML, not the render).

Selection and pickup state — which entries are picked up, by whom, and in what order — is
plan-internal and is not tracked in the repository at all: it lives in the gitignored `draft/`
directory, and no committed file records it and no gate reads it. Roadmap entries record only
durable facts about the work itself.

## Running everything

`check.ts` at the repo root is the single entry point for "run everything that verifies this repo".
It's a dependency-free Bun script built around a gate **registry** — one entry per verification gate
— with three tiers, each a superset of the previous:

| Tier | Command | What it runs | Wall time (warm) |
|------|---------|--------------|------------------|
| `fast` | `bun run check.ts fast` | what CI runs: fmt + clippy + snapshot tests + the drift gates | <!-- gen:sh:tests-tier-fast -->~43s<!-- /gen:sh:tests-tier-fast --> |
| `local` (default) | `bun run check.ts` | `fast` + workspace build + the full `cargo test` suite | <!-- gen:sh:tests-tier-local -->~13 min<!-- /gen:sh:tests-tier-local --> |
| `full` | `bun run check.ts full` | `local` + every manual-only gate | <!-- gen:sh:tests-tier-full -->~66 min<!-- /gen:sh:tests-tier-full --> |

Those three are **sliding-window medians (up to 20 runs) on the dev machine**, projected off
`tests/timings.json` by generated spans (`project_status_headers.ts` — hand-corrected stale twice
before being projected, which is that doc-rot class's trigger to derive rather than re-audit). See
"Measured gate durations" below for where that file comes from and why a warm gate cache does not
make `full` cheap.

`fast` is exactly what CI runs (`build.yml` is a thin `bun run check.ts fast` invoker — see the CI
policy below). Beside `fmt`/`clippy`/snapshot tests it carries the whole **sub-second no-cargo
file-scanner class**: the matrix-drift gates plus the decode-conformance catalog, recombination
ingredients, `query_q*`, status-header count, doc-citation, and tracked-text cleanliness gates
(`project_decode_conformance`, `project_recombination_check`, `query_q4_directional`,
`query_q1_gaps`, `query_q5_completeness`, `query_q6_diff`, `project_status_headers`,
`lint_doc_citations`, `lint_tracked_text`). The tracked-text lint
reads only `git ls-files -z` paths, strictly decodes authored text extensions (including `.mdx`),
rejects C0/DEL controls except tab/LF/CR, and scans snapshot doc lines for doubled rustdoc markers.
What makes a gate a member — and what a new
gate must satisfy to join, rather than defaulting to `local` — is that it reads only committed
files: no cargo, no network, no `cddl-matrix/node_modules`, no `draft/` ledger. The separately
approved `roadmap_projection_check` joined `fast` on
2026-08-11 with the same pure committed-file boundary; it runs the hermetic roadmap self-tests and
then fully validates both roadmap TOML sources (rendering each projection in memory), so it is
not included in the sub-second timing claim above. `local` is "run
before considering work done" — the heavy correctness gates (full
`cargo test`, corpus + wasm-matrix compiles) plus `matrix_typecheck` (`tsc --noEmit` over the
`cddl-matrix` scripts, via a dev-only local `typescript`/`@types/bun` — run `bun install` in
`cddl-matrix/` once; the runtime stays dependency-free — which is also why it is NOT in the class
above: CI cannot install it without a second `run:` step), `verify_selftest` (`verify.ts`'s
assert-at-startup deciders, run standalone in tens of milliseconds — their own gate is `full`-tier,
and an ignored flag, wrong compiler identity, verdict token, evidence-stage name, policy-mint
classifier, or drifted component row-selection/name mirror is silent in production, so the cheap
tier is where it must fail), `no_std_check` (the
no_std drift gate — see its section below),
`no_silent_directive` (which
spawns `cargo build` plus generator runs) and `timings_digest_check` live here, NOT in CI. The
doc-citation gate
checks that gap prose's cited pins still exist, reports each path that is still tracked by git but
absent from the working tree with restore-or-stage-deletion remedies, and rejects positional
roadmap/list citations. Every backticked `testing.*` / `matrix.*` token in durable markdown must
likewise resolve to a section-placed roadmap record (`matrix.json` is the explicit filename
exception), so retiring a record cannot leave a hand doc calling it remaining work. An always-run
pure canary models a successful initial scan followed by a
missing hand-doc reread, then repeats that read to pin one deduplicated path-bearing verdict. The
gate also bans ephemeral plan-internal references (delivery-phase probe/ruling id spellings,
spec-file names, the plan scratchpad path — matched phase-generically so a new delivery phase's
letter is covered by construction), bans user-doc examples that put a `; @<directive>` comment on
the same line as the container's closing `}`/`]` (a CDDL comment runs to end of line and swallows the
closer, so such an illustration parses differently than the doc describes; the warning bullet's
deliberate counterexamples are allowlisted by exact span text), and enforces
blank lines before headings in the hand docs. The conventions it backs: gap-tracking prose names its
pin by exact identifier ("pinned by/tracked by/gated by `name`") — the resolvable spelling for a
test is its bare fn name; a module-qualified `config_tests::name` does not resolve and fails the
gate — and a *behavioral* claim ("construct
X panics/rejects") gets a robustness-catalog row FIRST — the panic/reject catalogs flip loudly on a
behavior change, where prose-only claims rot silently. Those catalogs are **generate-only by
design**, which bounds what a row discharges: a PANIC row flipped to `ok` asserts that generation
exited 0 and nothing whatever about whether the emitted crate compiles. So when a change turns an
abort into generated code, `cargo check` the emitted crate under **every** profile before believing
the fix — not the default one alone — and pair the flipped row with an integration fixture (or, if
the crate genuinely does not compile, with the matching compile-side ledger entry). The profiles are
not interchangeable: a nominal reference to a collection typedef needed an `encoding_fields_impl`
fix that exists ONLY under `--preserve-encodings`, and every other profile was green while it was
missing (`integration_tests::recursive_collection_ref` / `recursive_collection_ref_preserve`).
`full` additionally runs the
manual gates (<!-- status-header gate roll-call is generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:tests-ignored-gates -->the 20 `#[ignore]`d gates `regen_over_prior_output_corpus` / `wasm_matrix_roundtrips` / `multifile_matrix_roundtrips` / `identifier_hazard_crates_compile` / `generated_local_scope_wide_crates_compile` / `recombination_crates_execute` / `recombination_preserve_crates_execute` / `recombination_json_crates_execute` / `recombination_wasm_crates_check` / `ir_conformance_corpus` / `ir_conformance_multifile` / `rust_oracle_fingerprint` / `decode_conformance_replay` / `corpus_decode_replay` / `all_supported_constructs_generate_all_profiles` / `feature_corpus_roundtrips_nondefault_profiles` / `component_corpus_compiles` / `wrapper_participation_mode_floors` / `wrapper_participation_requested_host_floor` / `regen_over_prior_output_corpus_compiles`<!-- /gen:sh:tests-ignored-gates --> — that roll-call is every `#[ignore]`d gate the registry classifies, so it includes the one that is `local` rather than `full` (`regen_over_prior_output_corpus`, `#[ignore]`d for its 40 s wall, not for fragility) — plus `cddl-matrix/verify.ts`, the `corpus_detect` gate, and
the two byte-fuzzer gates (`fuzz_compile_rot`, the compile-rot check, and `fuzz_bounded_run`, a time-boxed live libFuzzer walk of both targets — `fuzz/README.md`), `pin_cold_fetch` (every git `rev` mentioned in a pin-carrying surface must resolve against its remote from a scratch `CARGO_HOME` — the tier's one deliberately-online gate, because a warm local cargo DB answers "does this rev exist?" wrongly and confidently, which is how a never-pushed rev once passed three cycles of green gates), plus the two gate-cache soundness gates — the input-closure audit `gate_cache_closure_audit` and the flag-gated `verify_cache_transparency` — see the gate-cache section below) — run it before shipping a feature. Every run ends with the **full registry** printed as a table (`PASS` / `FAIL` /
`SKIPPED(reason)` / `STUB` / `not-in-tier` / `NOT RUN (--only)` + per-gate durations), so a gate that
didn't run is always
*visibly* not-run. Exit is non-zero on any `FAIL`; the run fails fast by default (`--keep-going` runs
every in-tier gate first). Every run also tees its FULL output to a timestamped
`draft/logs/check-<tier>-<stamp>.log` (path printed at start and end) — evidence preservation is
the tool's job, so never pipe a run through `tail`/`grep` as its only capture; cite the printed
path.

### Operational incident attribution and evidence capture

An intermittent gate failure is not a durable finding until the first failing run's full output is
captured and the competing explanations are separated. The `acquire_scratch_lock_serializes`
incident took five sightings to attribute because three early sightings survived only through
`tail`/`grep` and one had incomplete output. The fifth full log exposed the exact transient
`WouldBlock` signature: a fork-to-exec child briefly inherited the scratch-lock descriptor while
the blocking production lock remained safe. The regression helper now retries that nonblocking
probe only up to a five-second deadline. Keep the full command, commit, date, environment, versions,
signature, scope, and explicitly unprobed remainder in the durable conclusion; a disposable
`draft/logs/**` path is working evidence for its producing session, never the conclusion itself.

### Running a SUBSET: `--only <gate>[,<gate>]`

`bun run check.ts full --only verify,corpus_detect` runs exactly those gates, in registry order,
among the named tier's in-tier set (`--only a,b` and `--only=a,b` are both accepted). The case it
serves is the tier SUFFIX: the two longest gates sit at the end of `full` (one order-constrained,
one trace-purity-constrained), so a run that dies or fail-fasts leaves exactly them unrun, and
covering them otherwise costs a whole tier again.

**A `--only` run is not a tier run, and the output makes that unrepresentable rather than a matter
of discipline:**

- the **full registry still prints**, with the deselected in-tier gates under their own status word,
  `NOT RUN (--only)` — never `SKIPPED` (a deliberate omission must not read as an incidental one)
  and never `not-in-tier` (which says the opposite);
- the **summary header itself** carries the partiality — `SUMMARY (PARTIAL — --only …)` — so a
  quoted table cannot shed it;
- the **self-log is `draft/logs/check-only-<stamp>.log`**, deliberately outside the
  `check-(fast|local|full)-` shape: `splitLogName` derives a row's tier from the filename, so this
  is what keeps a 14-minute partial run out of the `full` tier's median wall, out of `--backfill`'s
  tier attribution and out of the per-tier retention window. Per-**gate** timing rows are still
  emitted (they measure the same work a tier run measures); the run-level tier row is not, because a
  partial run has no tier wall to give. Retention keeps the last 10 `check-only-*` logs as their own
  class;
- **dependency-splitting selections are refused**, naming the prerequisite (registry field
  `requires:`, checked by `self_checks` meta-check 5). Two pairs today: `coverage_md_diff` requires
  `project_corpus` (it diffs the COVERAGE.md that gate rewrites — alone it passes vacuously against
  the committed file) and `verify_cache_transparency` requires `verify` (it audits a cache that gate
  warms). Unknown gate ids and out-of-tier selections are refused the same way, exit 2;
- **the last line is a receipt, never the tier verdict**:
  `check.ts --only <gates> @ <commit>: N/N selected PASS; M in-tier gates NOT RUN — no tier verdict`.
  `RESULT: PASS — all in-tier gates green` stays reserved for a complete tier.

**Reporting rule:** a `--only` run is citable only as "gates X, Y ran green" — never as a tier
verdict, in a commit message, a doc, or a report. That is the same rule AGENTS.md states for
fail-fast partial runs, and the receipt is written so it can be pasted verbatim.

### Sharded sweeps (inside the `test` gate)

The five fixture sweeps that used to set the `test` gate's wall — `feature_corpus_compiles`,
`wasm_matrix_compiles`, `multifile_matrix_compiles`, `snapshot_tests::feature_corpus` and
`wasm_api_parity` — are each split into `<name>_shard_NN` `#[test]`s over a round-robin slice of the
sweep's sorted fixture list, so libtest's own 32-thread pool runs the cells instead of one test
walking them. The shard names CONTAIN the original name, so `cargo test <name>` and
`CLOSURE_AUDIT_GATE=<name>` still select the whole sweep.

Two things a shard structurally cannot do, and where they live instead:

- **Whole-axis assertions.** Every "this skip-list entry names a fixture that no longer exists" guard
  is only decidable by a test that sees EVERY fixture — a shard cannot tell a deleted fixture from
  one another shard owns. Each sweep therefore keeps a `<name>_pins_are_live` test (`wasm_api_parity`:
  `wasm_api_parity_axes_and_pins_are_live`) holding those guards plus the sweep's vacuity counts. They
  read a fixture directory and nothing else, so they cost milliseconds. **Add a new whole-axis
  assertion there, never in a shard**, or it silently becomes vacuous while the suite stays green.
- **The shared scratch root.** The compile sweeps wipe their root on entry (generation is only
  hermetic against a clean tree) and on exit (the root holds a crate per cell plus the shared
  `CARGO_TARGET_DIR`). `SharedScratch` keys both wipes on occupancy — first shard in wipes, last shard
  out wipes — so the shards keep one target dir rather than paying the dependency build each.

Shard counts are sized against **measured** scaling, not core count. The three gate-cache sweeps run
`cargo generate-lockfile` per cell, which serializes on cargo's process-wide
`$CARGO_HOME/.package-cache` lock, so they stop scaling early (6 shards each measured the same tier
wall as 4; 12 each was markedly worse). The two in-process sweeps take no such lock and scale freely.
That same lock is why a hand-run `cargo test` is much slower than the `test` gate: check.ts forces
`CARGO_NET_OFFLINE=true`, which cuts the preflight's cost about fivefold — always set it when timing
this gate by hand, or you are measuring a different machine.

### Gate-level concurrency (registry-declared, opt-in)

Gates run **one at a time unless the registry says otherwise**. A gate may declare a `concurrent`
group; the gates of one group, contiguous in the registry, form a batch that runs with at most
`CHECK_JOBS` gates in flight (default 4, `CHECK_JOBS=1` restores fully sequential execution). Any gate
that declares nothing is a **barrier**, so registry order still means what it says — the
`fmt → clippy → build → test` chain stays strictly ordered, and `verify` still finishes before
`verify_cache_transparency` observes the cache it warmed.

The `#[ignore]`d manual-only heavy gates form the concurrency group; they are `cmd`-shaped
and each own a `temp_dir()` scratch root nothing else touches. (The membership is the registry's
`concurrent: "manual_heavy"` declarations and the runner reports its runtime shape as `parallel batch: N
gate(s) in group 'manual_heavy'`. A count stated here would be a second source of truth that drifts
silently whenever membership changes.) `gate_cache_closure_audit` is deliberately outside it — it is
an strace input-closure audit, and ambient concurrent file activity is precisely what it must not
observe. `self_checks`' concurrency meta-check rejects a group declared on an `fn` gate (their output
would interleave and their cell rows would be mislabelled) and a group whose members are not contiguous
(such a member would run alone while declaring otherwise).

Why it pays, and what bounds it. The heavy gates are internally serial loops over catalog rows, so
solo they leave most of a many-core box idle — measured at 16.1 % CPU, 5.1 of 32 cores, for the
tier's second-largest gate. A controlled same-session A/B over four of the batch's gates
(`ir_conformance_corpus`, `all_supported_constructs_generate_all_profiles`,
`recombination_json_crates_execute`, `recombination_wasm_crates_check`) measured **412 s at
`CHECK_JOBS=1` against 167 s at `CHECK_JOBS=4` — 2.46×**, with per-gate inflation of +12 % on the two
long gates and +28 % on the two short ones. The ceiling is not "sum ÷ jobs": under perfect
parallelism a batch's wall is its **longest** gate, and this batch is badly skewed, so 2.77× was the
achievable ideal for that subset and 2.46× is 89 % of it. That is why dispatch is **longest-measured
first**, ordered by `tests/timings.json` as a hint (a gate with no row sorts last; getting the order
wrong costs wall time and nothing else).

**What must stay bounded is the PRODUCT, not the gate count.** `CHECK_JOBS` bounds how many gates
overlap; it says nothing about how many `rustc` each one spawns, and a nested cargo defaults to
`-j $(nproc)`. The quantity that consumes memory is

```
(gates in flight) × (rustc per gate) × (per-rustc resident set)
```

and its second factor must never scale with core count, because core count is unrelated to the
machine's memory — a 32-core box with a 32 GiB cap went unresponsive for ~10 minutes under a full
tier and had to be power-cycled, and two later whole-machine freezes (~1 h and ~1.5 h at 100 %
memory and swap) went through every check the first incident's fix installed. So **every** gate the
runner starts — batched or sequential — is handed a memory-derived `CARGO_BUILD_JOBS`:
`floor(MemAvailable × 0.5 ÷ 4 GiB per slot) ÷ gates in flight`, re-measured **per batch** so later
batches see the machine as it is, with MemTotal only a fallback where MemAvailable is unreadable.
Available, not total: a budget struck against MemTotal assumes the tier owns half a machine that
other processes already partly hold, which is how the two later freezes got through. The slot
constant is 4 GiB because a slot's peak is not one `rustc`: a nested-cargo gate compiles and then
RUNS what it built, and the old 2 GiB carried a comfortable margin over the wrong (never-binding)
quantity — the constant stays deliberately pessimistic until the per-run sampler's reports (below)
price a slot's true peak. `CHECK_CARGO_JOBS=<n>` overrides the
derivation (a bigger machine should raise it); an inherited `CARGO_BUILD_JOBS` can only hold it
*down*. And because the runner's own invocations are a minority of this repo's cargo runs, a repo
`.cargo/config.toml` floors everything else (`[build] jobs = 4`) — bare `cargo test`/`build`/
`clippy` from a shell or an agent was the one path with no bound at all, and an exported
`CARGO_BUILD_JOBS` still beats the file, which keeps nested cargo in scratch dirs (outside config
discovery, which walks up from the CWD) covered by the env var.

**The product has a THIRD factor: nested-child count.** `CARGO_BUILD_JOBS` divides ONE nested
cargo's compilers; it says nothing about how many nested cargos a gate holds open at once, and for
a `cargo test` gate that count is the libtest thread count — `nproc` by default — so the true peak
was `test threads × CARGO_BUILD_JOBS` compilers plus a spawned test binary per thread. The bound
lives at the one helper every nested spawn goes through: `tool_cmd`
(`src/tests/integration_tests.rs`) acquires a counting-semaphore permit held across the child's
whole execution, with the permit count read from `CDDL_NESTED_TOOL_PERMITS`. The runner exports 1
per gate (`CHECK_NESTED_PERMITS=<n>` overrides): a gate's whole `-j` share is each child's internal
parallelism, so at N children the gate spends N × its share — permits and jobs multiply, and any
pair that both track the share overshoots the budget quadratically. For invocations the runner
never sees (a bare `cargo test`), the same helper defaults to 2 permits and sets
`CARGO_BUILD_JOBS` to the `.cargo/config.toml` floor when the environment has neither — a nested
cargo runs from a scratch directory where config discovery finds nothing, so before this the
"floored" bare path still ran its nested children at `-j $(nproc)`, up to a test-thread count of
them concurrently.

The bound is close to free, measured on a 4-gate batch in one warm regime, sampled at 1 s:

| `CARGO_BUILD_JOBS` | peak concurrent `rustc` | peak Σ `rustc` RSS | batch wall |
|---|---|---|---|
| 32 (unbounded) | 27 | 2.25 GiB | 63.6 s |
| **2 (the default)** | **3** | **0.53 GiB** | **60.3 s** |
| 1 | 2 | 0.37 GiB | 60.0 s |

That the wall barely moves is the same fact as the win above: these gates are internally serial loops
over catalog rows, so the throughput came from overlapping **gates**, never from cargo's own `-j`.
Only a gate that is a single large crate compile pays (`rust_oracle_fingerprint`: 17.4 s → 34.0 s),
and it is not the batch's tail. Process count is in any case the wrong thing to reason about alone:
the largest single `rustc` resident set observed across that batch and a whole `local` tier was
**455 MiB**, so a count-based bound would still admit a very different peak from a gate that compiles
one big crate rather than many small ones. Hence the memory derivation — and hence deriving it from
a deliberately pessimistic per-slot peak rather than from that measurement, which sampled only the
compile half of what a slot holds.

**The per-run memory sampler (report-only, asserted by NOTHING).** Every run samples its own
descendant process tree at 1 s ticks and reports at the end — peak Σ RSS (test processes included),
peak concurrent `rustc`, the largest single process, and the machine-wide `MemAvailable` floor —
printed above the summary table and appended to the gitignored `draft/memory-peaks.jsonl`
(`CHECK_MEM_SAMPLER=0` disables). Peaks and floors are nondeterministic, so no gate ever fails on
one; what the numbers buy is replacing the assumed constants above with measured ones (the 4 GiB
slot, the one-permit nested bound), and splitting the NEXT whole-machine incident into "the memory
bound was wrong again" vs "memory was healthy, something else saturated". A freeze itself is still
something no passive report can prevent: a machine at 100 % memory and swap thrashes without ever
crossing the OOM killer's threshold, so containment on a shared dev box is an OPERATOR measure —
an early-OOM daemon or a cgroup memory cap around the tier — not a gate.

The sampler also guards the negative premise behind the arithmetic: every nested cargo spawn must
remain covered by `tool_cmd`, the runner's environment exports, or Cargo config discovery. A future
spawn path outside all three can silently escape the bounds; a sampled peak far above the budget's
product arithmetic is the tell. That is why the report keeps being produced even when no memory
incident is under investigation.

**What stays unmeasured: disk bandwidth.** The preflight below floors free scratch, which is a
capacity check, not a rate one — `CHECK_JOBS` gates writing in parallel to their own target dirs
still share one device, and the device's sustained-write ceiling is an environment property the
tier does not measure, lowest on virtual-disk hosts (WSL2 mounts `/` and `/tmp` on one virtual
device). No incident is attributed to bandwidth alone — the freezes that motivated this section's
bounds were memory (swap thrashing), with disk saturation as their symptom — but a
bandwidth-driven stall would share their signature: whole-machine, observable by no gate. The
watch entry, with candidate mitigations and what would make it buildable, is in
`tests/testing-roadmap.toml` § Operational watches.

**Resource preflight (`local`/`full` only).** A tier commits to its peak in its first seconds and
cannot discover a memory cap mid-run; the failure mode is not a red gate but a machine that stops
responding. So a run checks two floors up front and says which it saw:

- **MemAvailable below 8 GiB → degrades to `jobs=1`**, loudly. A slow tier beats no tier.
- **MemAvailable below 2 GiB → refuses**, because even one cargo would swap.
- **Free scratch below 10 GiB → refuses**, naming the largest `/tmp/cddl*` offenders and the cleanup
  command. Going sequential creates no space, and every nested-cargo gate downstream would die on
  ENOSPC tens of minutes in.

An *unmeasurable* machine proceeds — a preflight that refused whenever it could not read
`/proc/meminfo` would be unusable exactly where the measurement is what is missing. `fast` (what CI
runs) is never gated: it spawns no batch, mints no scratch, and must not acquire a way to refuse to
start on a runner whose disk this tool cannot reason about. `CHECK_SKIP_PREFLIGHT=1` bypasses both
floors.

Each batched gate's output is
**buffered** and emitted as one block on completion: interleaved cargo output is unreadable, and the
log is a data source (`project_timings.ts` attributes each `<n> run, <m> cached` rollup to the
`=== [tier] <gate> — …` section above it, so a rollup landing under another gate's header would be a
wrong measurement rather than a missing one).

Fail-fast with gates in flight means: nothing **new** starts, gates already running finish and report
their real verdicts, and everything never started is reported `SKIPPED (earlier failure; fail-fast)`
— so "ran and failed", "ran and passed under load", and "never ran" stay three distinguishable things
in the summary table. Per-gate durations recorded during a batch are **contended**, by accepted
design; the timings digest absorbs them and re-baselines, and no gate asserts a duration.

`verify.ts` needs two oracles (ruby `cddl`, rust `cddl`); the runner preflights them and prints
install one-liners on failure (`--skip-missing` downgrades a missing oracle to `SKIPPED`). It is the
slowest single gate but not prohibitive: ~170 examples × generate + `cargo test` × 2 crates —
measured ~10-11 min on the dev machine when every cell runs (a `GATE_CACHE=0` or first/all-miss
run; wasm + decode-foreign on), collapsing to ~4-5 min on a hit-heavy re-run against an unchanged
tree (~715 of ~740 cells proven by key — see the gate-cache section below); hours cold, the
shared-target warm-up dominating. The component-execution leg (a BOUNDED thirteen-row selection —
`cddl-matrix/README.md`'s annotations-table row describes it) adds ≈79 s to an all-miss run (a
lazily-built wasmtime host plus thirteen wasip2 cells over a shared target) and near-zero to a
hit-heavy one; its per-cell builds are defended against the same-name shared-target fingerprint
hazard by an mtime touch of each cell's tree before its builds (the `COMPONENT_TARGET` comment in
`verify.ts` carries the proof — sequential same-name cells cross-bound without it). The fuzz
gates re-run `fuzz/generate.sh` only when `fuzz/generated` is absent or `--refresh-fuzz` is passed —
either gate provisions it, whichever runs first, so neither depends on the other. `fuzz_bounded_run`
then fuzzes each target for `FUZZ_BUDGET_S` seconds (default 120, one libFuzzer process at a time,
`-rss_limit_mb=2048`); it is the one heavy gate deliberately outside the gate cache, because a
randomized exploration is not a pure function of the tree's bytes.
`--cache-transparency` enables the otherwise-`SKIPPED` `verify_cache_transparency` gate (two verify
runs, cached vs `GATE_CACHE=0`, asserted byte-identical — see the gate-cache section).

> **Fold before committing after a `full` run.** The `verify` gate rewrites
> `cddl-matrix/annotations/cddl_codegen.toml`, and it runs AFTER `build_matrix_check` already
> passed earlier in the same run — so a green full-tier summary does not prove the committed
> `matrix.json` matches the refreshed annotations (evidence strings change whenever decode vectors
> were minted since the last run). Run `bun run build_matrix.ts` from `cddl-matrix/` and re-run
> `bun run check.ts fast` before committing; this exact miss has produced a red-on-HEAD CI drift
> gate twice.

The runner's **first gate runs six self-completeness meta-checks**: every `#[ignore]` test must be
registered as a manual gate or a known-failing stub; every `cddl-matrix/*.ts` (minus `lib.ts`) must
be wired to a tier; `build.yml` must invoke `bun run check.ts fast` with no other run step;
concurrency declarations must be command-shaped and contiguous; every `requires:` edge must name an
earlier runnable gate available in the same tier; and every registry gate must appear as an exact
inline-code id in this README, with no authored cardinal count beside a named concurrency group.
Together these are the systematic catch for the disease the runner cures — a gate that exists but
is in nobody's habit — so a new manual gate or IOU stub is a conscious registry edit, not a silent
omission.

### Run-start scratch sweep and retained-scratch incidents

Nested-cargo gates and the in-process suites mint per-run scratch under the system temp dir and rely
on end-of-run cleanup that a killed or crashed run never reaches, so debris accumulates across
sessions until a disk fills — measured once at 3316 leaked entries / 43 GB / 0 bytes free, which
killed a full tier mid-gate and broke harness plumbing for every concurrent session. Every run
therefore sweeps first, under three bounds and no fourth:

- **An explicit prefix registry** (`SCRATCH_PREFIXES` in `check.ts`), never a `cddl*` glob. The temp
  dir is shared with the whole machine, so a glob matching one unrelated directory would be a
  data-loss bug reported as a test-runner bug. The registry is an enumeration of the repo's
  `temp_dir()`/`tmpdir()` join sites, and it stays one: `the_scratch_prefix_registry_covers_every_temp_dir_mint_site`
  (in the `timings_digest_check` gate) re-derives the sites on every run and fails on a mint site no
  prefix covers, or on one it cannot resolve. A new mint site outside the registry LEAKS rather than
  deleting a stranger's directory — the failure direction is deliberate.
- **Age**, at 24 h — more than 40× the longest measured tier, so no live run's scratch can reach it.
  Age is the newest mtime among an entry and its immediate children, because a long-lived
  `CARGO_TARGET_DIR` root keeps its own mtime while cargo rewrites the files inside it.
- **A live-process guard**: an entry named in any process's `cmdline`, `environ` or `cwd` survives
  (a scratch root reaches its gate through `CARGO_TARGET_DIR` at least as often as through argv). The
  guard fails **closed** — an unreadable `/proc` skips the sweep rather than deleting unguarded — and
  it is consulted only once deletion is on the table, so a machine with nothing stale stays silent.

`.lock` files are never swept: they are `acquire_scratch_lock`'s sibling flocks, they hold no bytes,
and unlinking one a live run holds would let a third run acquire a fresh inode. The sweep runs before
the resource preflight, so space it recovers counts toward the disk floor, and prints one line naming
entries removed and bytes freed — nothing at all when nothing qualified.

### Measured gate durations (`tests/timings.json`)

Wall times in this document are **measured, not estimated**, and they refresh themselves. Three
artifacts, three lifetimes:

- `draft/logs/check-*.log` — the prose each run tees. Gitignored, and deleted on a retention window.
  Never cite one as evidence of record: because the directory is gitignored, such a citation is
  dangling-by-construction and cannot fail loudly. The conclusion and its numbers belong in the
  message/commit/doc; the log is a working artifact for the session that produced it.
- `draft/timings.jsonl` — the local ledger, one JSON object per gate-run. Gitignored.
- `draft/memory-peaks.jsonl` — the memory sampler's ledger, one JSON object per RUN (peak Σ RSS,
  peak concurrent `rustc`, largest single process, MemAvailable floor), kept to the last 200 rows.
  Gitignored, and asserted by nothing — see "Gate-level concurrency" above for what the numbers
  are for.
- `tests/timings.json` — the committed digest, one row per registry gate plus one per tier. It is
  committed so a fresh checkout has real numbers on its FIRST run, before any local ledger exists.

`cddl-matrix/project_timings.ts` maintains them: `--backfill` recovers history from the prose logs
(it parses the incremental `--- <gate>: <STATUS>  [<dur>]` lines, so a run that was KILLED before
its summary still contributes every gate it finished), and `--update` recomputes the digest.

**A digest value is rewritten only when the median of a sliding 20-run window moves past
`max(2s, 20% of stored)`.** The median is the anti-flap mechanism — roughly half the window has to
shift before it moves, so one loaded-machine outlier cannot move it at all, and a write takes
several runs of sustained change. Comparing against the *stored* value rather than the previous run
is what lets slow monotonic creep accumulate until it trips instead of staying invisible forever.
When it does write, it prints one attributable line naming the gate and the old and new values.

**A digest write and the spans DERIVED from it are one commit or neither.** The tier table above
reads its wall times out of `tests/timings.json` through `project_status_headers.ts`'s generated
spans, so a re-measurement committed alone leaves the next tier run to fail-fast on the
`project_status_headers` drift gate — a full tier iteration spent on a one-line writer run. `check.ts`
therefore calls the update in-process (`runDigestUpdate`, which returns whether the digest moved) and,
when it did, runs `project_status_headers.ts --write` in the same flow before printing a commit hint
naming every file the pair touched. If that writer fails, the run says so loudly and names the manual
step; it never changes the tier verdict, because a failure to write a number is not a failure of the
repo. Standalone `project_timings.ts --update` has no such flow, so its own hint names the writer
command to run beside it.

The `timings_digest_check` gate (`local`) asserts **structure only** — a digest row per non-stub
registry gate, and no orphan rows — plus the update rule's pure-function pins. **No gate anywhere
asserts a duration value**: durations are nondeterministic (machine load, cross-session gate
contention), so a drift gate on the numbers would add a flaky gate to the suite this measurement
exists to make cheaper. `warm_ms` and `cold_ms` are separate fields and never merge; `cold_ms` stays
absent until a `GATE_CACHE=0` run supplies one, because absent is honest and a synthesized number is
not.

**Every run prints its expected duration before doing anything expensive.** The estimate is a median
of the digest's run-level *wall* times for the tier — never a sum of per-gate medians, which omits
inter-gate overhead and is unavailable for the runs that matter most (a killed run carries gate
timings but deliberately no wall) — followed by the spread of the local ledger's recent runs, and,
for a tier whose own measurements support it, the cache-warmth counter-example below. It prints
before the `cargo fetch` warm-up deliberately: the warm-up is where a run is most likely to hang, so
an estimate printed after it is one the interrupted agent never saw. A tier the digest has not yet
seeded prints `no baseline yet for tier=<t>` rather than a fabricated number.

**Old logs are deleted at run start.** The last 10 `check-<tier>-*.log` per tier survive and the
rest go, with one line naming the count and the bytes reclaimed — bounded and predictable, which a
time window is not (a quiet fortnight followed by a busy day should not change how much history
survives). Three interlocks guard the irreversibility, and the important one is **per-file**: a log
is deleted only once `draft/timings.jsonl` actually holds rows recovered *from that log*. A
non-empty ledger proves some history was scraped, not that this log's was — and while scraping is a
manual `--backfill` step, every run leaves a log that would otherwise reach the end of the window
with its durations never captured. So an expired log absent from the ledger is kept and counted in a
line pointing at `--backfill`: the failure direction is logs accumulating, never history vanishing.
The one exception is a log carrying no timings at all (a run that died before its first gate
finished, which can never appear in the ledger); "does this log carry timings?" is asked of the
parser that would have scraped it, so the two cannot drift apart. On top of that, `draft/timings.jsonl`
must exist and be non-empty at all, a failed citation scan fails closed, and a log whose basename any
committed file cites is kept and named in a warning — so that guard's own workload stays visible
rather than becoming a bridge nobody removes. Only `check-<tier>-<stamp>.log` is a candidate; the
hand-written ad-hoc logs in `draft/logs/`, including the `check-`-prefixed ones that name no real
tier, are left alone.

**A warm gate cache does not make `full` cheap** — the run-start line says so from measurement
rather than memory, naming the most cache-heavy `full` run on record and how long it still took. As
of this writing that is 866 cached cells and 72m20s: the cache removes nested-cargo *cells*, not the four gates that dominate
the tier. As of the seeding measurement those four — `gate_cache_closure_audit` (15m37s),
`corpus_decode_replay` (10m36s), `wasm_matrix_roundtrips` (7m24s) and `decode_conformance_replay`
(7m7s) — are 40 of the tier's 70 minutes between them, and everything below the `test` gate (3m37s)
is noise by comparison. Run `bun run project_timings.ts` from `cddl-matrix/` for the current
ranking. A cold build adds the one-time dependency + test-binary compile on top of all of it.

### Offline-after-warmup (nested cargo never touches the network)

Local/full runs start each retried online warm-up by running `cargo update --manifest-path
tests/warmup/Cargo.toml`, then fetch the workspace (`--locked`), the fuzz crate, and
`tests/warmup/Cargo.toml` (the dep-universe manifest: the union of every crates-io dep the generated
crates can declare) before setting `CARGO_NET_OFFLINE=true` for every gate. The update refreshes only
the gitignored per-checkout dep-universe lock; it never touches the committed workspace lock. Fuzz's
ignored lock deliberately is not refreshed: fuzz consumes that same lock itself after warm-up, while
the dep universe is the one with fresh-resolving scratch consumers that can otherwise outrun it. The env
propagates through `cargo test` → the suite's nested `Command` spawns and the cddl-matrix scripts,
so every nested-cargo cell resolves from the cargo cache instead of hitting crates.io per temp
crate. This removes the registry-transient flake class by construction (a flaky network/proxy used
to kill otherwise-green runs at a random cell with `unable to update registry crates-io` /
`curl [56] Proxy CONNECT aborted` — cargo's own transient retry never engages on that flavor), and
drops the per-cell `Updating crates.io index` latency as a side effect. The fast tier (CI) is
untouched.

The warm-up manifest is drift-gated: `warmup_manifest_covers_registry_dep_universe`
(`src/cargo_manifest.rs`) asserts every dep the manifest ops can emit appears there with the same
version req and features (features gate optional transitive deps, which `cargo fetch` only pulls
when enabled). Fixture crates under `tests/` with hand-written manifests are the manual tail: a
fixture-only dep missing from the warm-up manifest fails offline cells loudly with
`no matching package named <dep>` — add it to `tests/warmup/Cargo.toml`. Escape hatches:
`CHECK_ONLINE=1` keeps the run online (no offline forcing); a pre-set `CARGO_NET_OFFLINE=true`
skips the fetch and trusts the cache. The warm-up is the ONE place a network retry is honest (pure
cache-population/update work, with no assertions behind it); if it fails all attempts the run stops
before any gate.

### The no_std drift gate (`no_std_check`, local tier)

The generated rust crate is `no_std`-capable, and the tool emits a `no-std-check/` shim crate into
every output root plus a pointer to it in the seeded crate root. That pointer is a promise: a red
shim in a consumer's tree attributes to *their* hand-written additions — once the generated crate has
actually been reached, since cargo aborts at the first failing crate and a broken dependency reds the
check without ever compiling what it is about (the emitted shim's own two files carry that caveat).
This gate is what makes the promise true of tool output.

`cddl-matrix/no_std_check.ts`, driven by a `fn` registry entry and also invocable on its own as
`bun run cddl-matrix/no_std_check.ts [tier]` (`check.ts` has no single-gate selector). It generates
five single-crate profiles plus one multi-crate `--config` tree **fresh** into `mkdtemp` scratch —
the `tests/<dir>/export*` trees are unusable here because the integration harness splices
module-scope `println!` helpers into them outside `#[cfg(test)]`, so they would fail for reasons
that are not the tool's — then runs each profile's cells: every profile's emitted shim gets a
`thumbv7m-none-eabi` `cargo check` (inverted for one profile — see the table), and four profiles
carry a host-side cell beside it:

| Profile | Flags | Surface it exists for |
|---|---|---|
| `preserve_canonical` | `--preserve-encodings --canonical-form` | `OrderedHashMap`/`MapHashBuilder`, the derivative-derived key traits in all four `@used_as_key` flavors, a bytes wrapper, and a float member — its preserve head-width path reaches the `write_float` runtime helper, a `core`-only fn that has to stay no_std-clean |
| `raw_bytes` | `--preserve-encodings` + a `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule | `RawBytesEncoding`, the `decode_canonical_hex` door its `from_raw_hex` reads through, and both `hex::` call sites — all concatenated into `generated/serialization.rs`, and none of it reachable from the snapshot corpus. Also the only cell that compiles the `hex` key's package (`const-hex`) for a bare-metal target |
| `json_schema` | `--preserve-encodings --json-serde-derives --json-schema-export` | `json_schema_gen.rs` (the Registrar runtime, the exported macro), `json_value_ser.rs`, serde/schemars derives — plus the `any` × `--json-serde-derives` composition, which no flag list alone reaches: `any_cbor.rs` is emitted only when the finalized IR holds `any`, and only then is the whole of `static/any_cbor_json.rs` appended to it. Its spec therefore carries an `any_members` rule with one member of every nested `natural_any_cbor_*` adapter shape (plain, optional, seq, optional seq, table, optional table), because those nested inline modules carry their alloc imports BY HAND and a missed one is invisible under `std` |
| `depth_limit` | `--deserialize-depth-limit 64` + a recursive rule | The one flag whose output is deliberately **not** `no_std`-capable (`thread_local!` guard). Its shim cell INVERTS: expected FAIL carrying the pinned ``--deserialize-depth-limit output requires the `std` feature`` substring (LOCKSTEP with `generation::export::DEPTH_LIMIT_REQUIRES_STD`); a `host_default_check` `cargo check` beside it proves the refusal is confined to `not(std)` |
| `split_config` | a `--config` tree: two crates joined by a `deps` edge over a shared `[runtime]` runtime crate | The only profile where `default-features = false` has more than one package to reach. The `std` feature has to FORWARD across each hop — consumer → dependency → runtime — or it stops at the first, and on a target with no `std` at all a leaked `std` feature anywhere in the chain is a compile error. It is therefore the reachability proof for the runtime crate's `not(std)` hasher arm through a real topology, and it covers both forwarding derivations at once (`deps` and `[runtime].lib-name`). Its `host_std_arm` cell is the tripwire below, pointed at the runtime crate, which owns the cfg pair in this layout |
| `emit_tests` | `--emit-tests --preserve-encodings` | The emitted `#[cfg(test)]` module, which no shim cell can see (test code is not compiled when a crate is built as a dependency): `host_test_nostd` runs `cargo test --no-default-features --lib` on the generated crate, compiling and RUNNING the module — including its module-local `std` restore and the nested fidelity mod's hand-carried copy — under the `not(std)` root; the shim cell beside it guards restore-leakage into crate scope |

Profile `preserve_canonical` also gets a **host**-target cell: a consumer crate carrying
`fn _h(h: MapHashBuilder) -> std::collections::hash_map::RandomState { h }`. `MapHashBuilder` is a
cfg pair (`RandomState` under `feature = "std"`, `hashlink::DefaultHashBuilder` without it) and the
thumb cells exercise only the `not(std)` arm, so this signature is the one place in the repo where a
silent hasher flip fails. Profile `raw_bytes` additionally gets a hand-written consumer module for
its extern type — the documented contract makes zero-hand-code impossible there, so the module is
kept minimal and deliberately `no_std`-clean, and it is appended to the *seed-once crate root*, never
patched into a generated file. Profile `split_config` hand-writes the co-owned runtime crate's two
hand-owned halves for the same reason — its `[package]` table before generating (the export's
`package.name` op is seed-once, and the name must match `[runtime].lib-name`, which the tool
deliberately does not read that manifest to check) and its crate root afterwards (`pub mod` lines
plus the documented `#![cfg_attr(not(feature = "std"), no_std)]`; the module list is the export's,
not ours to predict).

**Each shim is checked alone**, via the emitted empty `[workspace]` table and a `--manifest-path`
invocation. That is a correctness requirement, not tidiness: cargo unifies features across a whole
dependency graph, and a check run inside a tree holding (say) a test-only oracle dep can have `std`
re-enabled transitively for a shared dependency — which masked a real break in this repo until a
sibling cell failed. Any future convenience refactor that moves these checks into an oracle-carrying
tree silently voids the gate's verdict.

**Per-cell verdicts, via accept predicates.** Every cell that asserts a crate *builds* requires
`exit == 0` **and** every `warning:` line falling in one of three classes (below). The one inverted
cell (`depth_limit.shim_thumb_expect_fail`) requires nonzero exit **and** the pinned
`compile_error!` substring in stderr, with no warning policy at all — a failing compilation emits
whatever resolution noise the missing `std` produces, and that noise is the reason the
`compile_error!` exists. The cache stores cell-SUCCESS, not cargo-exit-0, so the inverted cell
memoizes like any other. The three allowed warning classes: the cdylib drop (the generated crate's `crate-type` includes `cdylib`, which cargo
drops with a warning on a no-std target rather than erroring), the documented `Serialize` trait
residue (matched on the backticked path's *leaf*, exactly as `integration_tests`'
`UNUSED_IMPORT_TRAIT_RESIDUE` scanner does, since rustc renders it both fully-qualified and as a bare
leaf), and cargo's per-crate roll-up line. Anything else fails, printing the offending lines and the
full stderr. Presence is never asserted, only membership — `preserve_canonical` emits no residue at
all, so requiring it would fail that profile. Asserting warning-free output would make the gate red
on day one for non-product reasons.

**Absent target: loud SKIP in `local`, hard FAIL in `full`.** The skip prints a multi-line banner
naming the gate, the target, the fix and what went unchecked; a silent skip would void the
attribution guarantee with nothing else positioned to notice, and CI runs `fast` only.

**Cache participation:** each cell goes through the shared gate cache (key = tree hash over the whole
scratch output root + `rustc -vV` + RUSTFLAGS + path-normalized argv carrying the verdict marker
`no-std-check-v3`; `cargo generate-lockfile` for each manifest a cell checks runs *before* hashing).
Both hand-written crates the gate writes live INSIDE the hashed output root, so editing either
invalidates the key rather than serving a stale PASS. Bump the verdict marker on any change to the
VERDICT logic — the warning classifier or an accept predicate, not just the allowed-warning set. Because keys are content-derived (scratch paths normalized out), a
standalone `bun run cddl-matrix/no_std_check.ts` primes the same cache a tier run reads — a gate's
maiden in-tier run may legitimately serve every cell cached, and trusting that is sound exactly
because the red/green landing proof showed a content change MISSES (the injected-`std::` run
re-keyed and failed).

**Fresh-checkout setup:** `thumbv7m-none-eabi` is declared in `rust-toolchain.toml`, so a
rustup-managed checkout (and CI's `setup-rust-toolchain`, which reads that file) provisions it
automatically. `rustup target add thumbv7m-none-eabi` is the backstop for everything else — note
targets install *per toolchain*, so it must land on the pinned one.

### The gate cache (memoize-and-skip for nested cargo)

The heavy gates spend nearly all their wall time cargo-compiling/testing GENERATED crates whose
bytes did not change since the last green run, so those nested cargo invocations are memoized:
generation always re-runs (cheap, and it is what computes the impact of a change — never a
change→test map, which rots silently), then each nested cargo step hashes everything it consumes
and skips on a key that matched a previously-passing run. The key is sha256 over the whole
generated output tree (all crates — path deps are inputs — hashed AFTER `cargo generate-lockfile`,
so dependency resolution is pinned into the tree and the skipped build would have used the same
resolution by construction), the full `rustc -vV`, `RUSTFLAGS` as the nested invocation sees it,
the command sequence in path-normalized form (scratch paths are run- or checkout-local, so keys
carry the command SHAPE — subcommand + crate role within the hashed tree — never a literal
scratch path, which would make every key unique to its run), and a schema version. A gate whose
cached closure ALSO asserts something beyond the cargo exit code versions that extra verdict logic
into the key as an explicit argv marker (`feature_corpus_compiles`' `lint=unused-imports-v3`), so
changing what the closure checks re-runs every previously-cached cell instead of laundering old
PASSes past the new check. Soundness
rests on the same enforced determinism
invariant the rest of the repo leans on (byte-identical regeneration; `generated_code_clippy_clean`
already relies on the identical-bytes→identical-verdict form of it): an unchanged key means
re-running would provably reproduce the recorded verdict.

`verify.ts` reads `[toolchain].channel` from the repository's `rust-toolchain.toml` once at startup
and explicitly passes it as `RUSTUP_TOOLCHAIN` to every nested cargo/rustc child. Its cache-key
`rustc -vV` probe inherits that same environment, so the compiler fingerprint is the compiler that
performed the scratch-CWD build rather than the machine default.

Mechanics: entries live in the gitignored `.gate-cache/` at the repo root (one
`<key>.json` per green verdict — existence is the verdict, the body is for debugging which
component moved); only PASSES are cached (a failing or expected-red cell re-runs every time);
corrupt entries read as misses and self-heal on the next green run; `GATE_CACHE=0` disables
read+write entirely and `GATE_CACHE_DIR` relocates the dir (unit tests use it). Skips are never
silent: each covered gate prints a `[gate-cache] <cell>: cached PASS (key …)` line per hit and a
`N run, M cached` summary. CI is unaffected (the fast tier reaches no cached site, and CI starts
from a clean checkout with no cache dir). There is deliberately NO time-based invalidation of any
kind: the industry "nightly cold run" guardrail compensates for unchecked input closures, and this
repo's stance is mechanical per-run enforcement instead — the closure is reviewed at each call
site, mutation-verified red-first at landing (comment-only fixture edit still hits; a rule rename
misses exactly its cells; a corrupted entry re-runs), and `GATE_CACHE=0` exists for suspicion.

The lockfile preflight: before a cell is keyed, its generated crates need a `Cargo.lock` — that lock
is part of the hashed tree, so dependency resolution is part of what the verdict is cached against.

One discipline the mechanics above cannot enforce for you: **every input a cached cell reads from
scratch must live INSIDE the hashed root — including files the gate itself writes** (hand consumer
crates, appended modules). A gate-authored input parked beside the hashed tree is invisible to the
key, so editing it serves the stale PASS forever — and the closure audit cannot flag it, because its
allowed-read classes treat everything under scratch as derived-from-hashed (true only by this
discipline; the gap is ledgered in `tests/testing-roadmap.toml`). Shipped exemplar of the rule
applied: `no_std_check.ts` writes every crate it hand-authors inside the hashed output root — the two
single-crate consumer crates, and the split profile's co-owned runtime `[package]` table, crate root
and host consumer — with relative path deps so the hashed bytes stay run-independent.
The resolution itself is memoized in-process (`src/tests/gate_cache.rs`), keyed on every file in the
generated tree that resolution reads, because the generated manifests are identical across fixtures
and differ only by generation profile: one measured `cargo test --all-features --all-targets` run
went from 952 `cargo generate-lockfile` processes to 27. What the gate-cache key covers is unchanged
— only the derivation is shared. `GATE_CACHE_LOCKFILE_VERIFY=1` is the enforcement: it makes every
cell re-derive its lockfiles with cargo and fails, naming the differing files, unless the resulting
tree is byte-identical to the memoized one. It is off by default because it doubles the preflight
cost, and it is the check to reach for whenever a resolution input is suspected to be missing from
the memo key.

Covered sites: `verify.ts`'s per-example rust/wasm probe tests, failure-classifying checks,
decode-foreign replays (its warm-ups turn lazy — first miss only — behind an always-run
generation-only self-test, so a generator that doesn't build still aborts the run before any
verdict is written), and the component-execution cells (`verify.component_probe`: the wasip2
build + native oracle + wasmtime drive memoized as ONE unit per selected row, with the generic
host crate's CONTENT HASH folded into the key because the host is an input living outside the
hashed generated tree — its copied scratch crate runs `cargo test` before the executable build, and
editing what that host checks re-runs every cell instead of serving stale PASSes; the explicit
test-before-build preparation marker likewise prevents reuse across verifier-side policy changes);
and, via `src/tests/gate_cache.rs`, one cached unit per cell in
`feature_corpus_compiles`, `wasm_matrix_compiles`, `multifile_matrix_compiles`,
`wasm_matrix_roundtrips`, `multifile_matrix_roundtrips`, and the recombination layer-2 batches.
`decode_conformance_replay` is deliberately NOT cached: its success path parses libtest stdout
into per-vector verdicts and completeness counts, so exit status alone is not the consumed result
(the cached-unit rule: a site qualifies only when the harness consumes nothing but exit codes
from a fixed command sequence). `run_test`-based fixture suites are also uncached in v1 — they
reuse export dirs and already replay warm-incrementally through cargo.

**Soundness gates.** The NO-time-based-invalidation stance rests on two obligations, each with a
mechanical full-tier gate INSTEAD of an industry cold run. `gate_cache_closure_audit`
(`cddl-matrix/audit_gate_cache_closure.ts`) protects the KEY side: it traces a real cached gate
under `strace -f` and asserts every file-content read made by a nested-cargo subtree falls in a
class the key provably covers (the generated tree under `$TMPDIR`, `$CARGO_HOME`, `$RUSTUP_HOME`,
system prefixes, and exactly the two user git-config files cargo consults at startup — fetch-side
only, checksum-fenced by the hashed lockfile, so verdict-inert) — a read under the repo checkout is
exactly "a cached site grew an unhashed input"
and FAILs, naming the path, pid, and owning nested-cargo argv. It traces `multifile_matrix_compiles`
by default (its nested `cargo check` transitively builds the `../rust` path dep — the highest-risk
read pattern); `CLOSURE_AUDIT_GATE=<test name>` extends coverage to the other cached gates as
configuration, not code. It prints a visible `SKIPPED` when `strace` is absent, refuses to pass a
trace with zero nested-cargo subtrees (vacuity floor), and statically asserts the repo carries no
`.cargo/config` (an unhashed input for the TS-side sites whose nested cargo runs with cwd = the
repo). Two nested cargos are deliberately NOT audited, both builds of the TOOL UNDER TEST rather
than of a generated crate, and neither work any cell's verdict can be skipped on: the traced root
(itself a `cargo test`, whose subtree legitimately compiles the harness) and the once-per-process
generator freshness build (`isOwnGeneratorFreshnessBuild` — `cargo build --bin cddl-codegen` into
this checkout's own target dir, keyed that tightly and pinned by the `--self-test` fixtures; the
generator's identity still reaches the key through the generated crate tree hash). Anything else
that reads the repo checkout still FAILs. `verify_cache_transparency` (`cddl-matrix/cache_transparency.ts`, flag-gated by
`--cache-transparency`) protects the OUTPUT side: it asserts `verify.ts`'s
`annotations/cddl_codegen.toml` and `verify_report.json` are byte-identical between a cached run
(≥1 hit required — vacuity floor) and a `GATE_CACHE=0` run, the direct check that the hit path's
reconstructed verdicts can never leak into output bytes differently than real execution (this
gate has already earned its keep: it exposed cached-vs-uncached divergences down to single lines,
each attributed and fixed — the defenses below exist because of what it found).

verify.ts carries three defenses against nested-cargo verdicts leaning on state OUTSIDE the
hashed tree, all forced by the shared `CARGO_TARGET_DIR`: cargo's leaf fingerprint there is keyed
by package name+version, NOT manifest path, so a `cddl-lib` built AFTER another cell's sources
were written makes cargo declare those older sources "fresh" and reuse the wrong crate's
artifacts — `cargo test` then exits 0 without compiling the cell's bytes (a lazy warm-up runs in
exactly that window: on a cache miss, between the cell's generation and its `cargo test`; the
eager-warm `GATE_CACHE=0` path never can, which made the poison a pure cached-run asymmetry). The
defenses: every generation gets a fresh, counter-suffixed output dir (keep-last-1 deletion; the
Rust gates' per-cell-dir design). This invariant covers an emission-profile base generation and its
synthetic `embedFallback` generation as separate calls; the startup self-test drives that exact pair
through `runCodegen` and pins distinct output paths. `touchTree` bumps every tree file's mtime right
before each MISSED nested cargo (after any warm-up), so the cell's sources are always newer than any
same-name fingerprint and the rebuild is honest; and the warm-ups write their spec to their OWN
`warm.cddl`, never the cell's probe file — a lazy warm-up runs mid-cell, and a shared spec file
would make the cell's later legs (the wasm probe reuses the spec file) silently generate the WARM
crate instead of the cell. None of the layers moves a key: the tree hash is
content-over-relative-paths and the key argv is path-normalized.

| Layer | File | Question it answers | Speed |
|-------|------|---------------------|-------|
| **Golden snapshots** | `src/tests/snapshot_tests.rs` | "Did the *generated source* change?" | fast (~5s, in-process) |
| **Integration** | `src/tests/integration_tests.rs` | "Does the generated code *compile and round-trip*?" | slow (compiles generated crates) |

Snapshots are the fast inner loop and the primary safety net for refactors; integration tests are
the correctness gate. A refactor that doesn't intend to change output should leave every snapshot
untouched — if one moves, you see exactly what changed.

A third in-process layer sits on a different axis: `src/tests/write_tail_tests.rs` drives
`export()`'s write tail (`generation::write_tail`) directly, with a synthetic file map and a temp
dir — no CDDL parse, no `IntermediateTypes`, no `GenerationScope` — and asserts on the resulting
tree. Snapshots stop at the file map (`generated_strings`) and integration cells reach disk only
incidentally, so this is where the write path's own contracts are pinned: the seed-once crate
roots, the manifest changeset merge, the family-wide post-overlay import re-prune, never-silent
comment handling, run-twice = run-once over a replace-block-bearing prior, the no-prior-output
bound, the stale-file scan's report-never-delete rule, and the byte-inertness of every diagnostic
prior-output read. It also covers the per-file preservation and fixed point of composed runtime
statics, plus hand-owned `--export-static-crate` `src/` writes: their untouched root, manifest
merge, fixed point, and existence-gated new-file notice. Larger integration cases still assert the
generation-level flag sets that select those writes; the direct cases complement rather than replace
that coverage.

**CI policy — fast tier only.** CI (`.github/workflows/build.yml`) runs exactly
`bun run check.ts fast` and nothing else (CI minutes cost real money — sole maintainer, AI-velocity
commits). The fast tier of the registry is the single definition of what CI does,
and check.ts's `self_checks` gate fails if the workflow grows any other run step. Keep the fast
tier the absolute minimum: new gates default to `local` or `full`; promoting one into `fast` is a
maintainer decision. Everything heavier than the fast tier runs locally, and is documented as a
local/manual run. The workflow's `paths:` filter has to cover every tree a fast-tier gate READS —
it includes `docs/**` and the root-level `*.md` because `lint_doc_citations` scans every tracked
`.md`/`.mdx` and `query_q1_gaps` byte-compares a generated block in
`docs/docs/current_capacities.mdx`; a promoted gate whose inputs fall outside the filter is blind
to exactly the commit class it exists to catch, and would first go red on an unrelated later push.

## Golden snapshots (`snapshot_tests.rs`)

Drives the generator as a library (`crate::api`) and snapshots the post-rustfmt generated source
with [`insta`]. No subprocess, no compilation, no `target/` bloat. Three sub-suites:

- **`feature_corpus`** — one tiny CDDL file per language construct in [`tests/corpus/`](corpus),
  generated under every flag profile in `ALL_PROFILES` (`default`, `preserve`, `json`, `component`),
  plus an IR dump. A one-feature regression yields a one-file diff. Snapshots are grouped per feature
  in `tests/corpus/snapshots/<feature>/`. The `component` profile snapshots only its `component/**`
  files and ASSERTS every other emitted file byte-identical to the `default` profile's — the
  component face is purely additive, so pinning the rust and wasm trees a fourth time would store
  duplicates by construction where the assertion states the invariant and fails loudly the day the
  face starts leaking into the other two. The generated `Cargo.toml` and json-gen `main.rs` are
  *skipped* here — they barely vary by construct, so they'd be repeated noise; they're covered by
  `whole_program` and `serialization_prelude` instead.
- **`whole_program`** — the larger integration inputs (`core`, `preserve-encodings`, `canonical`,
  `json`, `json-float`, and the `multifile` directory) each under one known-safe profile, capturing
  the *full* output incl. `Cargo.toml`s. Covers cross-feature interactions, the scope/module path,
  and the edition/deps logic. It's also the home for inputs that need a *profile-limited* snapshot,
  which routes a single CONSTRUCT into such an input's `.cddl` rather than the corpus:
  `tagged_type_choice` (tag over a whole type choice) lives in `core` as the direct-deserialize
  annotation control; anonymous enum-rule tags now also execute under preserve in the rich preserve
  and canonical fixtures. The preserve fixture deliberately names one group-choice arm `tag`: its
  integer-width sidecar already claims `tag_encoding`, so the rule-owned tag width must take the
  shared collision-free name `tag_encoding2`. The compiled/runtime
  `tagged_anonymous_choices_replay_the_rule_tag_head` test gives the outer tag, array head, and inner
  integer independent non-minimal widths and requires byte-exact replay. That is the standing pin
  for a codegen-injected enum field colliding with an arm-owned field; the ordinary identifier-hazard
  and recombination axes do not construct this post-IR name interaction.
  No float constraint of that kind survives — floats carry their head width as an encoding variable,
  so `tests/corpus/homogeneous_array.cddl`'s `float_holder` and `tests/corpus/optional_fixed_float.cddl`
  are ordinary corpus fixtures snapshotted under preserve like everything else. What keeps
  `json-float` here is its fixture SHAPE, not any float gap: its hand-written
  `tests/json-float/tests.rs` validates a serialization against the crate's own emitted schema
  (a `jsonschema` dev-dep the corpus harness has no slot for) and asserts an f64 survives a JSON
  round-trip bit-exactly, which holds only while the generated manifest carries serde_json's
  `float_roundtrip` feature — a manifest this suite captures and the corpus deliberately skips.
  Both the fold-back into `tests/json/` (split from it only for the retired float reason) and the
  promotion of the bare `[f: float64]` construct into the corpus are open, unperformed work.
  And it's the home for inputs whose output *can't compile
  standalone* (`extern_deps`/`extern_deps_wasm`/`raw_bytes` reference user-supplied types; their
  behavioral coverage is their integration fixtures) — this suite never compiles, so neither constraint bites here, which
  is why such inputs are pinned here rather than via corpus skip-lists that would weaken the corpus
  invariant that every fixture is fully gated.
- **`cargo_toml_matrix`** — a small curated `input × profile` matrix that snapshots every distinct
  generated `Cargo.toml` dependency combination (the type-conditional `hex`/`wasm-bindgen` deps
  toggled independently). The per-feature corpus skips `Cargo.toml` as near-constant noise, and
  `whole_program` doesn't produce every combination, so this is where they're all pinned. Beyond the
  snapshots it asserts each conditional dep is present *exactly* when its flag/type condition holds —
  the absence half guards the manifest changeset's set-or-**remove** contract (a dep whose condition
  turned off must be removed from an existing manifest, not skipped; see `cargo_manifest.rs` — the
  one deliberate exception is the `--export-static-crate` target's changeset,
  `ops_for_static_runtime`, whose conditional deps are set-or-SKIP because that manifest is co-owned
  with a hand-owned crate whose hand code may need a dep the current flavor doesn't). The manifest's
  one tool-owned NON-dep conditional key — the `--rust-wasm-feature` `[features]` leaf paired with
  the now-optional `wasm-bindgen` dep (set under `--wasm` with `["dep:wasm-bindgen"]` or `[]`
  content by c-style-enum presence, removed without `--wasm`) — is pinned byte-wise by these same
  snapshots; its lifecycle/merge contract (including the CML-shaped regen and the legacy
  feature-list repair) lives in the `feature_gate_*` unit tests beside `ops_for_rust`/`ops_for_wasm`
  in `cargo_manifest.rs`. These snapshots also pin the `# cddl-codegen:` ownership-comment block
  above `features.std` byte-wise (`STD_OWNERSHIP_COMMENT` — its wording deliberately spells no TOML
  table header, and a snapshot diff is where a careless reword would surface); its strip/append and
  user-comment-survival contract lives in the `std_ownership`/`user_comment` unit tests beside the
  constant. The
  unconditional keys come from a per-manifest append-only change log (`static/manifest_changes/*.toml`,
  the single source of truth — format and editing rules in `static/manifest_changes/README.md`);
  its fold reader hard-errors on non-contiguous ids or a malformed
  entry, so a key the tool ever managed can never be silently unmentioned (removals become permanent
  tombstones by appending a `remove` entry). Its sibling `manifest_template_drift` pins the derived
  `static/Cargo_*.toml` templates — generated snapshots of the logs, never read at runtime —
  byte-for-byte, failing with `BLESS_MANIFEST_TEMPLATES=1 cargo test manifest_template_drift` when a
  log changes without regenerating them.
- **`serialization_prelude`** — the static serialization runtime, snapshotted once per flag
  combination (it ships verbatim into every crate but is assembled differently per flag).

The module also carries non-snapshot **invariant gates** — each sweeps every file the
`whole_program` inputs generate and asserts a property snapshots can't judge (snapshots pin that
emitted bytes don't *change*, not that they satisfy an invariant; a violation just gets blessed):

- `emitted_bounds_site_differential_and_wart_scan` — freshly generates the bounded fixture, parses
  its emitted Rust with `syn`, and selects one labelled record constructor, optional-member WASM
  setter, wrapper `new`/decode, primitive `.and_then`, and collection-length site by file and
  enclosing context. It canonically compares their condition shapes and complete `RangeCheck`
  payloads, while an explicit ledger consumes only wrapper location, return framing, and signed-wire
  nint versus stored-magnitude differences. The same fast test syntax-scans every tracked Rust corpus
  snapshot for the two canonicalization warts: `< N || > N` with the same bound and a dead unsigned
  or `.len() < 0` leg; seeded positive and negative controls protect the detector itself.
- `comment_dsl` / `corpus_detect.ts` — the matrix's DSL feature floor batches comment attachments
  through `examples/comment_dsl.rs`, which calls public `comment_ast::metadata_from_comments` and
  projects accepted metadata into matrix ids and argument facts. TypeScript retains the RFC text scan
  and comment lexer but no directive grammar mirror; malformed strict arguments are uncreditable
  authority rows, while separate rule attachments (including duplicate `@name` spellings) never
  merge into one metadata owner.
- `generated_files_start_with_header` — every generated `.rs` in the tool-owned trees
  (`rust/src/generated/**`, `wasm/src/generated/**`) must LEAD with the codegen provenance banner;
  only blank lines, `//` comments, and crate `#![…]` attributes may precede it. It asserts with the
  same banner constant and path predicate the stamper uses (`generation::CODEGEN_HEADER` /
  `is_header_stamped_path` — the stamping is file-level in `generated_files`, so scope-internal
  ordering can't outrank it), over the `whole_program` inputs plus the wasm-list-macro fixture under
  both its profiles. It exists because `codegen`'s `Scope::raw` hoists raw text above everything in
  insertion order: any raw pushed during generation (the class that put `impl_wasm_list!`
  invocations and merged-root module declarations above the header) beats an end-of-run banner raw.
- `deserialize_converts_error_at_most_once` — a generated error-conversion chain maps to
  `DeserializeError` at most once per read (an emission site prepending the conversion without
  checking whether an earlier chain stage already converted emits a redundant identity `map_err`).
- `ok_pattern_parenthesizes_only_tuples` — a generated `Ok` match pattern parenthesizes its payload
  only when it is a real tuple, matching the `final_expr` shaping on the expression side
  (`Ok((x))` on a single binding is redundant grouping parens).
- `no_anonymous_text_list_wrapper` — text arrays cross the wasm boundary as bare `Vec<String>`
  (supported by wasm-bindgen; strings are copied at the boundary, so the by-value ownership hazard
  that justifies struct `*List` wrappers doesn't apply), so no anonymous `TextList` wrapper may be
  emitted.
- `rust_tree_wasm_bindgen_only_feature_gated` — the RUST tree may carry `wasm_bindgen` only in the
  c-style-enum `#[cfg_attr(feature = …)]` form (`--rust-wasm-feature`), never ungated — the
  corpus-wide placement half of the standalone-compile invariant (the feature-off `cargo check`
  half lives in integration, see § "Integration tests"). Carries a positive control: it FAILS if
  no whole_program input emits the gated form, so the sweep can't silently scan a corpus that
  stopped exercising the construct (the fixture-blind-spot class that once graded the rust crate
  bindgen-free from a fixture lacking any c-style enum).

One gate in the module reads the GENERATOR's own source rather than its output, because the class it
catches is invisible in output that no composition happens to reach:

- `emitter_overload_no_bare_default_tokens` — `generate_deserialize` and `generate_serialize` each
  thread an OVERLOADABLE name (the deserializer, `raw` by default but the payload's own cursor under
  a `bytes .cbor`; the serializer, `serializer` by default but an inner buffer under the same
  payload, or `buf` for a canonical map key). A leaf that spells the DEFAULT inline compiles,
  snapshots green, and mis-frames the buffer only where an overload is actually live — four such
  deserialize leaves were found by composition luck and code-reading, not by any gate. The lint fails
  any emitted string literal containing a bare `raw` / `serializer` token inside an *overload-scoped*
  fn: one that RECEIVES the name (a `DeserializeConfig`/`SerializeConfig` parameter, a
  `deserializer_name:`/`serializer_use:`/`serializer_pass:` parameter, or a `&self` method of the
  config types). Root emitters are deliberately out of scope — they emit the signature that BINDS
  the name. Two justified allowlist entries (the accessors' own defaults) and two siblings guarding
  the scoping rule from both sides: `emitter_overload_lint_sees_its_anchors` pins the scoped fn set,
  a floor on the literals scanned, and that every allowlist entry still matches a live site;
  `emitter_overload_lint_scopes_every_name_param` pins the converse — every fn parameter whose
  identifier contains `serializ` and whose type is one a name is carried in (`&str`,
  `(&str, bool)`, or either wrapped in `Option`) must be spelled one of the three the rule knows, so
  a helper receiving the name under a fourth spelling fails here instead of going silently unlinted.
  A root-binding `serializer: &mut Serializer` is type-distinguished and exempt.

The emission-hygiene gates pin specific shapes found by review; `generated_code_clippy_clean`
provides the systematic lint axis, while needle gates remain for source-shape classes no rustc or
clippy lint can see.

`canonical` is a serialization sub-mode of `preserve` (differs only where maps/sets exist), so it's
covered at whole-program scale rather than duplicated per feature.

### Adding a feature

1. Drop a tiny `tests/corpus/<feature>.cddl` exercising exactly one construct (see existing files).
   The stem must not collide with a `whole_program` label (asserted by the test).
2. `INSTA_UPDATE=always cargo test snapshot_tests` to generate its snapshots.
3. Eyeball the new files under `tests/corpus/snapshots/<feature>/`, then commit them.

### Blessing changes

After an intentional generation change:

```sh
INSTA_UPDATE=always cargo test        # accept all, then review the git diff
# or, with cargo-insta installed:
cargo insta review                    # interactive per-snapshot accept/reject
```

`*.snap` files are committed (they're the golden reference); `*.snap.new` / `*.pending-snap` are
gitignored.

To audit a MASS re-bless (hundreds of snapshots, e.g. a dependency upgrade), classify the changed
lines by frequency instead of eyeballing files:

```sh
git diff tests/corpus/snapshots/ | grep '^[+-]' | grep -v '^[+-][+-]' | sort | uniq -c | sort -rn
```

The intended change classes surface as high-count lines; anything unexpected hides in the
singleton tail, so read that tail line by line — an audit that stops at the common classes proves
nothing about strays.

The local `insta_orphan` gate runs `cargo insta test --unreferenced=reject` so a snapshot orphaned by a refactor (one
that stops generating a file) fails the build instead of lingering unnoticed.

## Preservation-merge fixtures (`tests/preserve-fixtures/` + `src/tests/preserve_fixture_tests.rs`)

The edit-preservation overlay (`comment_preserve::preserve` — user comments, insert blocks,
replace blocks; user docs: `docs/docs/preserving_edits.mdx`) is a pure function of
`(old, new) → merged`, so its behavioral tests are **fixture triples independent of codegen** —
they never churn when the generator changes. Each `tests/preserve-fixtures/<case>/` holds:

- `old.rs` — the prior on-disk file (user comments / tagged blocks / carried sentinel blocks);
- `new.rs` — the freshly generated pristine content;
- exactly one expectation: `expected.rs` (byte-exact merge output) or `error.txt` (a substring
  the hard `PreserveError` must contain — used for malformed-tag cases, authored by hand).

One test (`preserve_fixture_tests::preserve_fixtures`) globs the directory. Byte-exact matching
is deliberate — a misplacement that keeps a substring cannot pass — and on top of the blessed
bytes the harness asserts three properties **independent of the blessed content**, so a wrong
`expected.rs` is hard to bless:

- *idempotent fixed point* (pre-rustfmt): `preserve(expected, new).content == expected` — re-running
  the merge on its own output is a no-op (this also covers block carry-forward across regens);
- *never-silent*: every own-line non-doc user comment and tagged block in `old.rs` appears in the
  output either placed or `escape_for_rust_string`-transformed inside a `compile_error!`;
- `changed == false` ⇒ output byte-identical to `new`.

A second glob test (`preserve_fixture_tests::preserve_fixtures_rustfmt_cycle_stability`) sweeps
every expected-case fixture through the tool's exact rustfmt pass and asserts the POST-rustfmt
on-disk fixed point — `rustfmt(preserve(rustfmt(expected), new)) == rustfmt(expected)`, the form
the tool actually writes — plus never-silent survival baselined on `old.rs` (nothing user-authored
lost across old → merge → format → merge). Its ordinary triples span keep/insert/replace at a last
block statement, if/else tail, struct-literal last field, last enum variant, nested-module closing
brace, and impl closing brace, alongside the folded match-tail family. The current pinned formatter
keeps the six new fixed-point geometries own-line (the raw comma-less unit-enum probe can fold, but
is not the preserved fixed-point geometry); they are version-bump/re-ownership tripwires. It never
pins a specific folded spelling, so the property remains a regression net over the corpus's actual
fold/format classes, not a discovery instrument for formatter behavior outside it.

Bless with `BLESS_PRESERVE_FIXTURES=1 cargo test --bin cddl-codegen preserve_fixtures`, then
review the diff like a snapshot. Blessing never creates `error.txt` cases. The rustfmt-cycle sweep
skips itself under `BLESS_PRESERVE_FIXTURES=1` — cargo runs tests as parallel threads in one
process while the bless path rewrites `expected.rs` files, so a sibling test reading fixture files
mid-bless would flake on half-written reads; any future test that reads this corpus must adopt the
same skip. The directory's
`.gitattributes` (`* -text`) pins CRLF fixture bytes against checkout conversion; per-case
intents live in `tests/preserve-fixtures/README.md`.

What the pure fixtures CANNOT see — assumptions about real generator output (the header banner,
doc ownership) and the disk-level write / toolchain-formatter seams — is
pinned by exactly three integration tests (the rustfmt-cycle sweep above adds corpus BREADTH over
the formatter seam, but only for merge inputs the fixtures express; these three pin the
real-pipeline assumptions no fixture can): `comment_preservation_disk_round_trip` (real pipeline;
injects comments + an insert block + a replace block, regenerates twice, asserts the post-rustfmt
fixed point), `comment_preserve_lexer_round_trip_over_corpus` (lexer assumptions vs everything
the generator emits across flag profiles), and `preserve_markers_survive_rustfmt_fold_roundtrip`
(the formatter seam the first test's mid-function replace block cannot reach: rustfmt folds a
match-TAIL block's markers into trailing position — `} // cddl-codegen:replaces` — so this runs
the tool's exact rustfmt pass over a match-tail block and asserts the result re-parses, staying
meaningful across rustfmt versions because "both spellings parse" is the assertion, while the
`replace_rustfmt_folded_tail_arm_markers` fixture family pins the merge semantics of today's known
folded shape). A fourth seam — the overlay's ORDERING against the usage-derived import prune
(`export()` applies the overlay to the in-memory file map, then re-derives the import set from the
post-overlay content, so an import whose last user a replace block removed vanishes from the final
bytes) — is pinned by `comment_preservation_replace_orphans_import_same_file` (the re-prune direction) and
`comment_preservation_replace_in_descendant_orphans_parent_import` (since the core/alloc delivery:
the post-overlay alloc-import RECOMPUTE direction — generated files self-bind the injector-owned
names, so the orphaned import lives in the file the replace block edits and the recompute must
remove it, the add-and-remove half that also covers trait imports the pruner's name-scan model can
never own). The cross-file family-wide re-prune MODEL is pinned shape-independently by the
`import_prune::tests` super-glob family (`keeps_parent_import_consumed_by_child_via_super_glob`
and siblings) — the real output no longer holds a glob-only import to pin it end-to-end. That
property lives in the export driver, not in the merge, so it cannot be a fixture. Keep both sets thin; new merge behavior belongs in fixtures.

One generator-output assumption is invisible to all three, because none of them regenerates over a
CHANGED spec: "the generator emits no comment on a row a spec change can delete" (which subsumes
"no trailing comments"). The disk round-trip regenerates the SAME spec — no row is ever deleted —
and the corpus round-trip self-preserves (`preserve(content, content)`), a no-op for any comment
whether or not a real regen would strand it. That blindness shipped a real trap once (the
`extern_interface_check.rs` / `key_demand_assertions.rs` per-row markers — see
`testing.corpus-wide-regen-over-prior-output-sweep-reaches`), and the two pins minted then —
`extern_interface_check_regen_over_deletion_no_trap` (a real regen-over-prior-output with a rule
deletion, over a hand spec whose two `@used_as_key` rules let one tag be deleted while the file
survives) and `extern_interface_check_has_no_trailing_row_comments` (a source-shape floor) — name
those two FILES. The by-shape layer that owns the class corpus-wide, for files nobody named in
advance, is `regen_over_prior_output_corpus` (§ "Regen over prior output at corpus breadth" below);
it caught a third instance in `wasm/src/generated/mod.rs` on its first run.

The other comment-loss path the overlay cannot see is a file it never merges: a `.rs` left under a
generated tree by a PRIOR run, which nothing declares any more, so it and its comments drop out of
the build silently. `export()` warns about that (and about a crate-root `lib.rs` that predates the
thin-root layout), and since the warning IS the whole mitigation, both are pinned as texts by
subprocess tests — `stale_generated_file_warning_names_the_orphan` and
`legacy_root_warning_fires_only_for_legacy_shape` (integration; a real CLI spawn, per § "Design
rules" on why output assertions spawn). Each pins three things a silent regression would move
independently: the warning fires with its text and names the offending file, it does NOT fire on the
clean shape, and it is absent under `--verbosity=error` — the level gate is a second way either
could go quiet. Both also assert the diagnostic changed no bytes (the legacy root is left verbatim,
the orphan is reported rather than deleted).

Lexer-level tests (char-vs-lifetime, raw identifiers, in-string `//`) stay inline in
`comment_preserve.rs` — they test `lex`, not the merge.

The overlay's one out-of-crate surface — `--export-static-crate`, which writes the composed static
runtime into a consumer-named crate's `src/` and merges that crate's `Cargo.toml` (the upgrade
path for `--common-import-override` runtime crates) — is pinned by
`export_static_crate_writes_composed_runtime_and_manifest` (integration): the flag-set-pure file
set (non_empty\*/raw_bytes always included, prelude-only serialization.rs carrying its own import
header), insert-block survival across a re-export in that dir, flag-off leaving a same-named dir
untouched, the fresh-manifest seed, and the hand-manifest merge (identity/hand deps survive, a
stale `cbor_event` pin is bumped to what the exported source requires).

## Input robustness catalog (`robustness_tests::input_robustness_catalog`)

The generate-only malformed/edge-input catalog (`tests/robustness/*.cddl`) runs **every** sorted
row in a fresh test-binary child process. The parent owns the four snapshot labels — `ok`,
`error (graceful)`, `PANIC`, and `ABORTED (signal n)` — so a future non-unwinding crash becomes the
catalog's verdict instead of terminating it. Each ordinary child exit must print the helper
sentinel before the parent accepts its label; an exact helper filter that goes stale can therefore
not silently bless every row as `ok`. The catalog deliberately tests generation only, not emitted
crate compilation. A Unix-only synthetic child abort keeps the signal-classification branch live
even though every current fixture exits normally, gracefully, or through an unwinding panic. The
snapshot owns only the outcome category: whenever a row flips, review its fixture header in the same
change so a former failure explanation does not survive as a current claim.

## Integration tests (`integration_tests.rs`)

Each test generates a crate via the CLI (`cargo run`), appends hand-written round-trip tests
(`tests/deser_test` + each dir's `tests.rs`), then compiles and runs it — plus a wasm build and a
json-schema build where applicable. Each config (`preserve`, `canonical`, `json`, multifile,
raw-bytes, extern-deps, …) exercises a distinct compile path, so they aren't redundant.
A fixture dir may also ship a `tests_wasm.rs`: its contents are appended into the generated
*wasm* crate and `cargo test`ed there (host target — the wasm-bindgen wrapper types are plain Rust,
so no node/wasm-pack is needed).

`fixed_singletons_execute_across_default_preserve_and_canonical_profiles` is the executable
fixed-value ownership cross-product. It generates scratch crates for the default, preserve and
canonical profiles and exercises direct singleton codecs, composition, fixed/null and bare
`null / null`, wrong-value reasons, and non-minimal fixed/tag/`.cbor` heads. It also executes
uppercase-hex, raw UTF-8, and empty fixed bytes; fixed byte members (including optional presence),
same-major and mixed text/bytes choices, `.default` over `bytes`, and tagged/`.cbor` byte literals.
Wrong bytes must report `FixedValueMismatch` through `Key::Bytes`; default, preserve, and canonical
respectively write preferred bytes, replay recorded byte-string heads, and minimize them. Its encoded-null leg is
deliberately class-wide rather than one incident pin: it crosses both non-empty wire operations
currently reachable on a `FixedValue` (`Tagged` and `CBORBytes`) with both nullable-collapse sites,
both source orders and both legal wire arms, then checks reason-bearing rejection, preserve replay
and canonical minimization. `OptionallyTagged` is produced only by the collection tag-set collapse
and cannot currently inhabit a fixed-value arm; if another encoding operation becomes reachable on
`FixedValue`, this table is extended in the same change.

**Negative decode vectors carry their rejection REASON, enforced by a ratchet.** A hand-derived
reject vector asserted with a bare `T::from_cbor_bytes(&bytes).is_err()` passes for ANY failure —
a vector one byte off fails on the wrong boundary and stays green while the boundary it claims to
pin goes unexercised (this shipped once, and the conversion pass that retired the pattern found
three more). The discriminated form is `assert_decode_reject_reason::<T>(&bytes, "<substring>")`,
a helper defined per fixture file (spelled identically in each `tests.rs` that uses it — the files
are appended standalone into their generated crates, so nothing can be imported). The substring
must be DERIVED from the boundary the vector claims and then confirmed against the real message —
that confirmation, not the helper, is what catches a wrong-reason vector.
`src/tests/decode_reject_reason_tests.rs` (`local` tier) holds every `tests/*/tests*.rs` file's
count of remaining bare same-statement `from_cbor_bytes(..)…is_err()` sites at an exact per-file
baseline (`BARE_DECODE_REJECT_BASELINE`), failing in BOTH directions: a new bare site names the
helper to use, a converted site (or a deleted file) says lower/delete the row in the same commit.
The match rule is statement-scoped (rustfmt wrapping cannot hide a site) and is pinned by the
module's own synthetic self-test; constructor range checks and closure-routed decodes are
deliberately out of scope — the module doc states the full rule and both residues.

A hand fragment compiled under BOTH profiles (any `tests.rs` in a fixture that has default and
preserve export crates) must bind union enum variants profile-invariantly: payload-carrying
variants are tuple variants under the default profile and STRUCT variants under
`--preserve-encodings` (per-variant encoding state), so tuple destructuring is E0164 in exactly one
of the two crates. Use `matches!(v, Foo::Bar { .. })` plus emitted-byte assertions — see
`tests/recursive-collection-ref/tests.rs`'s union-rooted vectors for the worked example, and
[Output format](../docs/docs/output_format.mdx) § "Union (type-choice) enum variants change shape
with the profile" for the user-facing statement.

The spliced hand files (each dir's `tests.rs`/`tests_wasm.rs` and shared fragments like
`tests/custom_serialization{,_preserve}`) are CONSUMER-shaped code: they compile against the
generated crate's own dependency features, so they must model the post-reshape world (a dependency
whose `Error` impl is gated on its own `std` feature cannot be boxed as `Box<dyn Error>` once the
crate takes it with `default-features = false` — the two `custom_serialization` fragments carry the
local-newtype migration exemplar). A delivery that reshapes a dep's features therefore
sweeps the committed hand-file set as part of its battery — the reshape's owning family includes
every fixture whose hand code uses that dep, which no ops-table enumeration can produce. And a
green compile inside a fixture tree is weak evidence for that sweep: the oracle-carrying trees
unify features graph-wide (the `cddl` dep's own `hex` re-enabled `hex/std` in the preserve tree and
masked one of the two instances — unification keys on the PACKAGE, so that particular masking ended
when the `hex` key moved to the `const-hex` package, while the mechanism stands for every dep two
manifests name identically), the same unification caveat
`rust_wasm_bindgen_feature_gated_crate_compiles_standalone` guards against below — full class
ledger in `testing.fixture-s-verdict-accident-cargo-feature-unification-harness`.
`tests/core/tests_wasm.rs` (default profile) and
`tests/canonical/tests_wasm.rs` (preserve-encodings/canonical, whose map wrappers wrap
`OrderedHashMap`) execute a representative sample of the wasm-ABI shape axis (the
`project_wasm_matrix.ts` `SHAPES` list): construct through the wasm wrapper API, round-trip
`to_cbor_bytes`/`from_cbor_bytes`, read every accessor back. That's the *behavioral* half the
`wasm_matrix_compiles` gate below can't see — a semantically wrong accessor or boundary conversion
compiles green. The rust-side value round-trips are `--emit-tests`' job; these files own the
boundary. `tests/extern-deps-wasm/tests_wasm.rs` extends that behavioral floor across CRATES: it
constructs the consumer's wrappers over the mapped dep's types (`--extern-wasm-crate` — the eight
collection wrappers plus the non-root `nested::NestedHolder`), round-trips to byte-identity, and
value-anchors every getter through the dep's `From`/`AsRef` boundary impls, so a semantically wrong
cross-crate conversion fails rather than merely building.

### Recursive-type boundary — test map

The boundary is tested as a verdict, an emitted API, and a generated-crate build; none of those
layers substitutes for the others:

- `input_robustness_catalog` keeps the alias-first hop
  `recursive_alias_hop_collection_entry.cddl` classified `ok` under the default wasm-bearing
  profile. This is generate-only evidence.
- `recursive_type_boundary_refuses_uncompilable_cycles` pins the E0391/E0072 refusal classes under
  both rule permutations, including identical cycle membership and diagnostics.
- `recursive_alias_cycle_auto_newtype_matches_the_hand_written_directive` compares every repaired
  shape byte-for-byte with the same spec carrying explicit `@newtype`, under both `--wasm=false`
  and `--wasm=true`. Its alias-hop pair includes both the collection-first and alias-first naming
  orders, so losing the declared alias target or a late alias leaf fails generation rather than
  being blessed as a different API.
- `recursive_alias_cycle_auto_newtype_announces_itself_on_stderr` pins the user-visible repair
  notice, while `recursive_type_boundary_shapes_compile` batches the repaired and supported cycle
  classes and `cargo check`s the rust-only crate plus both rust and wasm crates from the
  wasm-bearing run. That compile floor catches a generator exit 0 whose late alias spelling still
  names a type with no struct or impl.

The supported/refused boundary and its remedies are documented for users in
[Current capacities](../docs/docs/current_capacities.mdx#recursive-types).

This map closes the known recursive alias-hop escape and, together with
`generic_collection_tests::alias_of_instance_chains_generate`, the known generic-instance alias
escape. Their broader registration-class × reference-position floor is now
`registration_reference_tests::registration_class_reference_position_sweep`: seven explicit IR
registration classes (own-ident record, transparent and forward aliases, a tag-258 generic-set
instance plus its second alias hop, an extern, and the distinct collection-first/alias-first
recursive registration identities) cross eleven
explicit positions (the directive sweep's ten semantic positions plus a direct-member control).
Every prefixed cell receives one independent generation verdict and must emit its holder, so a
silently omitted reference context cannot pass; the authored current boundary is all 77 `Accept`
cells, with exact-cover, duplicate-id, stale-name, and accepted-participation guards. The same
accepted batch then generates under rust-only and wasm-bearing profiles and
`cargo check`s rust once plus wasm once; externally registered cells get real hand-owned root
definitions, never generated-tree edits. There are currently no refusal cells: a future genuine
language/product boundary must be registered as `Reject` with its stable diagnostic, rather than
being excluded or inferred from the implementation's result. The focused incident fixtures remain
because they provide the stronger byte/behavior assertions this registration floor intentionally
does not duplicate.

`named_group_reference_tests::named_group_reference_live_cells_match_authored_verdicts` is the
parallel denominator for a reference to one single-choice plain group. Its eight placement ids
(homogeneous-array element, array-record splice, struct-map member, table key/value, array/map
group-choice arm, and type-choice arm) cross the six modifier ids (direct, keyed, optional, final
rest, tagged, and transparent alias) in one authored 48-coordinate table. Forty cells are
live and retain their representation plus actual parser/entry kind, a stable collision-free prefix,
focused-test/probe evidence, and either `Accept` with a cell-local emitted-root predicate or
`Reject` with a stable diagnostic fragment. All eight tagged cells are live `Type2::TaggedData`
rejections: a tag payload is one type item, while a plain group only splices members. The other eight
are explicit `NotApplicable` rows: they name parser-invalid spellings, a modifier that became a
sibling node, or a duplicate table occurrence rather than silently shrinking the denominator.
Every live cell runs independently through `api::generated_strings` with `--wasm=false`, catches a
panic as a failure, and cannot accept after silently dropping its holder/root. The eight accepted
cells then batch into collision-free native-only and wasm-bearing scratch crates; the floor uses the
scratch lock, explicit static directory, separate per-profile targets, and checks rust once plus
wasm once. It deliberately does not multiply preserve/JSON/component profiles: the focused
plain-group occurrence, tag-payload, table/member, and group-choice-arm tests retain their
profile-sensitive wire, remedy, and guard-order assertions.

`rust_wasm_bindgen_feature_gated_crate_compiles_standalone` guards the rust crate's
`--rust-wasm-feature` gate from the one direction no other build can witness: every
workspace-style build enables the feature through the wasm crate's path dep (cargo feature
unification), so only a standalone feature-off `cargo check` of the generated `rust/` proves the
crate compiles without the optional `wasm-bindgen` dep. It also scans the generated rust tree for
any ungated `#[wasm_bindgen…]` (the c-style-enum `cfg_attr` form is the only sanctioned
appearance) — per-fixture here; the corpus-wide placement half is the
`rust_tree_wasm_bindgen_only_feature_gated` invariant gate (snapshot suite, fast tier).

`json_arbitrary_precision` covers a dimension no other fixture varies: a cargo **feature on a
dependency** of the generated crate, which can change what emitted code MEANS without any spec or
manifest change of the consumer's own. `serde_json/arbitrary_precision` makes `serde_json::Number`'s
`Serialize` emit a private token struct that only serde_json's own serializer collapses, so an impl
routing through a `serde_json::Value` ships that token to every other serializer. The feature arrives
by cargo feature UNIFICATION from the one-line [`tests/arbitrary-precision-crate`](arbitrary-precision-crate)
path dep — a second `serde_json` key spliced into the generated manifest would be a TOML duplicate-key
error, since the tool owns that key — and both the rust and the wasm crate assert the feature actually
arrived, because a silently-off feature would make every other assertion in the fixture pass
vacuously. Its Rust oracle is the only place the suite runs a generated `Serialize` through a
NON-serde_json serializer (`ciborium`); that is the whole point, since serde_json is precisely the
serializer that cannot observe this class of dishonesty. The `roundtrip.mjs` half adds the JS view
and pins that a `> 2^53` integer still reaches `to_json_value()`'s loud refusal.

The three external-macro flags (`--wasm-list-macro`/`--wasm-conversions-macro` and
`--wasm-cbor-json-api-macro`) emit invocations of a *user-supplied* macro, so the output can't
compile standalone and a source snapshot can't judge invocation semantics; `wasm_list_macro_compiles`
and `wasm_cbor_json_api_macro_compiles` compile-gate them against the real macro definitions in
[`tests/wasm-macro-crate`](wasm-macro-crate) (wired in as a path dependency, the same way
extern-deps wires `tests/extern-dep-crate`). Those macros' arms mirror the inline emission, so the
wrong-emission classes a snapshot would bless — swapped args, wrong `needs_into`/`is_copy`, an
unreachable combination, a wrong arity — fail to compile (see the crate's README).

Those two gates
feed the generator a single `.cddl`; the DIRECTORY-INPUT axis is gated by
`wasm_macros_multifile_compiles`, which runs all three flags together over a temp-written two-module
input and guards what only multifile emission produces: the submodule's own
`use wasm_macro_crate::…;` import (the root module's does not reach it), invocations minted inside
the submodule's file, and the SCOPED rust path each invocation must carry
(`cddl_lib::sub::rec::Rec`, never a bare `cddl_lib::Rec`) — including the list wrapper, which is
minted at the container's root scope while naming a cross-module element. Its input is temp-written
rather than committed so it earns no fixture-registry obligations, and its verdict is compile-only,
which is the decided permanent posture for macro-mode wasm surfaces (see the wasm-crate test module
section's macro-mode skip).

`extern_wrapper_index_defers_to_dep` pins the `--extern-wrapper-index` deferral surface (a consumer
skips re-minting collection wrappers a dependency's committed `generated/collections.rs` index says
the dep already owns) over `tests/extern-deps-wasm-index` and the dedicated wasm-clean dep pair
`tests/index-dep-crate{,-wasm}` — dedicated because the shared `tests/extern-dep-crate` pair
intentionally double-defines its `#[wasm_bindgen]` class across both crates (the single-crate
convention `extern_deps` needs) and so can never link for the real wasm target. The
`extern-dep-crate` pair also carries the common `Int` the `--common-import-override` cells
re-export instead of minting: `tests/extern-dep-crate` a single `#[wasm_bindgen]` `Int` serving
both faces (the same single-crate convention), `tests/extern-dep-crate-wasm` a wrapper over it
with the `From`/`AsRef` boundary contract. The same pair backs two further cells:
`common_override_wasm_int` (the PURE override consumer — no `_CDDL_CODEGEN_EXTERN_DEPS_DIR_`, the
`--extern-wasm-crate` key naming the override crate itself, with a content assertion pinning the
`Int` wasm face to the WASM crate because the rust stand-in's `#[wasm_bindgen]` `Int` makes a
wrong-direction re-export compile-indistinguishable) and `dep_owned_named_collection_compiles`
(the pair's `DepWithdrawals`/`DepCerts` — transparent `BTreeMap`/`Vec` aliases plus thin wasm
faces — give the dep-owned named-collection cell a full cross-crate compile). The extern-dep crate
pair's two `Int` faces are hand mirrors of generated `Int` — the preserve-encodings `Uint`/`Nint`
representation, wire impls, and encoding-insensitive key
semantics. `preserve_encodings` now decodes the same irregular negative `Int` with the generated
and hand-written codecs, requires both to preserve its bytes, then cross-decodes and re-emits each
other's output; a one-sided signed-arm, value-conversion, or retained-width drift therefore fails.
The `extern_wrapper_index_defers_to_dep` bespoke harness, rather than `run_test`, asserts the CLI's
stderr warning for an all-extern wrapper
absent from the index, the deferred `use <dep_wasm>::collections::…;` imports (plain `use`, never
re-exported), the local-mint cells (not-in-index and mixed-element), a cross-crate behavioral
round-trip via the fixture's `tests_wasm.rs` (constructing through the DEP's wrapper classes — the
DEFERRED-wrapper sibling of `tests/extern-deps-wasm`'s cross-crate `tests_wasm.rs` above), and the
honest link gate: a real
`cargo build --target wasm32-unknown-unknown` of consumer+dep — the only place duplicate
`#[wasm_bindgen]` classes actually fail — asserted GREEN with the flag and RED
(`duplicate symbol`) without it, with a loud skip (hard assert under CI) when the target isn't
installed. It was the suite's first gate compiling a generated crate for the actual wasm target
(the workspace-mode gates below now do too), so the fixture also deliberately INCLUDES a
control-constrained signed-int member
(`local_thing.c: (int .ne 1)`): its emitted i64-window width guard pins `RangeCheck`'s `i128`
fields on a 32-bit target — the class where `isize` fields overflowed the `i64::MIN`/`MAX`
literals, which 64-bit host builds can never see.

Two sibling cells over `tests/extern-deps-index-named` reuse the same dep pair for the reference
positions and companion wrappers that fixture does not reach.
`extern_wrapper_index_named_rule_reference_unifies_with_dep` drives the NAMED-RULE reference
position (`inputs_named`): a user rule whose ident coincides with a structural wrapper name the dep
indexes, referenced from a record field by rule name only. Both flavors are asserted with their
stderr texts — the ARRAY flavor defers (no local class) and the by-name reference must route the
dep import, warned as a UNIFICATION of the rule with the dep's class; the TABLE flavor is screened
by `exists_in_rust`, keeps the consumer's own class, and is warned as the duplicate-`#[wasm_bindgen]`
configuration it is. `extern_wrapper_index_deferred_try_from_sources` (`inputs_sources`) covers the
companion `try_from` SOURCES: the map-side deferred source (`{+ uint => idx_foo}` over a dep-indexed
loose `MapU64ToIdxFoo`), its sole-owner-screened variant (a loose shape with a sole table-rule owner
resolves to that owner's local alias, never a dep import), and the reject-set combination (a
`@duplicates reject` RULE whose ident differs from the structural name is the consumer's own class,
so it mints locally and is indexed here, while its loose source defers). Both floor on the generated CONSUMER wasm crate's `cargo check` through
`gate_cache::run_cached`; the wasm32 link is deliberately not re-run, since
`extern_wrapper_index_defers_to_dep`'s RED leg already demonstrates that failure mode.
`inputs_sources` generates at the preserve profile because its cross-crate MAP conversion resolves
against the hand pair's `OrderedHashMap`-flavored wrapper contract.
`extern_wrapper_index_local_mint_under_indexed_name_warns` (`inputs_nested`) is the third cell and
covers the two `try_defer_wrapper` arms that decline BEFORE any index is consulted, so the class
they mint collides with a dep-indexed name that the defer seam never saw: the ident≠structural
screen (`arr_idx_foo_list = [* idx_foo_list]` derives `IdxFooListList` but emits `ArrIdxFooList`)
and the R3c constituent screen (`idx_hash_list = [* idx_hash]` over a consumer-owned element, ident
and structural name both `IdxHashList`). Both mint exactly as before — the cell asserts the classes
and their own-index rows — and both are now announced by the mint-seam backstop
(`warn_local_mint_shadows_index`, keyed on the EMITTED ident rather than on the arm that declined),
whose text is pinned verbatim beside the deferring rule's unification warning to prove the shared
once-per-ident set lets all three coexist.

#### The wrapper-participation grid (`src/tests/wrapper_participation_tests.rs`)

The three cells above and their ~20 siblings elsewhere are INCIDENT-shaped — each records the exact
configuration of one past escape. `wrapper_participation_tests` is the ENUMERATION beside them:
`PARTICIPATION_TABLE` is the grid AS DATA over emission MODE (`Local` control / `IndexDeferred` /
`WorkspaceBorrowed` / `RequestedHosted`) × wrapper SHAPE (loose list, loose map, NonEmpty list,
NonEmpty map, named table rule, `@duplicates reject` set, `@duplicates preserve` pair map) ×
reference POSITION (inline-anonymous member, named-rule DECLARATION whose ident equals the structural
name, named-rule REFERENCE, non-root declaring scope). Each row states its expected `Outcome`
(`Defer` / `Borrow` / `Host` / `LocalWarned(<which warning>)` / `LocalSilent(<why silence is
correct>)`), and a row an existing test already pins carries that test in a `pinned_by` column and is
REFERENCED rather than rebuilt — so the module's generated crates cover exactly the rows nothing else
did, and a new shape or mode is one table row rather than a new function. Each row's CDDL is DERIVED
from its axes, and each owns a distinct element ident, so one generated crate carries a whole mode.

Two participation facts the grid encodes rather than assumes: a reject set participates in every mode
like the loose and NonEmpty twins (one seam, so an inline occurrence defers/borrows and a
rule-declared one is either the index mode's name-only unification or criterion 9's local shadow),
and a reject wrapper that DEFERS borrows the dependency's `try_from` door with it, so the
loose-source companion belongs only to the rows that mint locally; and the name-only index is
flavor-SAFE by construction, because the structural name
carries the container (`PairMapKToV` vs `MapKToV`), which makes a preserve table an ordinary shape
row rather than a hazard cell. `wrapper_participation_table_is_complete_and_live` is the grid's own
guard: rows are unique, every mode covers every shape that participates in it (the one documented
gap — a table RULE cannot be requested — is spelled, not left as a silent absence), and every
`pinned_by` resolves to a test that still exists, so a referenced row cannot go on reading as
coverage after its pin is renamed away.

The four per-mode sweeps are always-on and GENERATION-only (emitted source, this crate's own
collection index, the workspace sidecar, and the run's stderr). The compile/link floors are
`#[ignore]`d and batched per (mode, floor), each memoized by `gate_cache::run_cached`:
`wrapper_participation_mode_floors` (check.ts gate `wrapper_participation_floors`) does a `cargo
check` of the standalone `Local` column plus a real `cargo build --target wasm32-unknown-unknown` of
the index and workspace columns against the committed wasm-clean dep pair — GREEN only, since
`extern_wrapper_index_defers_to_dep`'s RED leg already demonstrates that a non-deferring consumer
duplicate-symbols; and `wrapper_participation_requested_host_floor` (gate
`wrapper_participation_host_floor`) checks the HOST crate a `--wrapper-requests` run emits, whose
mints come from a sidecar rather than from its own spec, including both NonEmpty twins. Its import
walk follows each support class's actual emission home: co-hosted classes stay local, own-spec
classes come from their real root/module, and deferred classes come from the dependency collections
surface. The always-on controls pin each non-local branch rather than relying on the host compile to
infer it: `wrapper_participation_requested_non_empty_root_sources_stay_imported` covers the list/map
root homes, `wrapper_participation_requested_non_empty_map_source_can_defer` covers the reachable
primitive-map case whose loose source stays in a dependency index, and
`wrapper_participation_requested_workspace_dep_keeps_local_and_extern_homes` composes one co-hosted
source with a genuine mapped extern import. In the deferred control the dependency import lives in
generated root and `requested_collections.rs` reaches it through `use super::*;`; asserting both the
absent local body and that import is what keeps a future ownership refactor honest. What the link
legs add over the incident
cells is the POSITION crossing: a named-rule declaration, a by-name reference and a non-root
declaring scope had never reached a wasm32 link, and an import routed into the wrong module is
exactly the class every host-target check survives.

The grid's table and compile floors currently establish those participation decisions at the
default profile. Preserve-profile behavior has focused incident controls (including deferred map
sources), but there is no preserve/JSON profile cross-product over every row. That is a measured
scope boundary of the current system;
`testing.add-profile-wrapper-participation-grid-participation-differs-across` records the signal
that would justify multiplying the grid by profiles.

### Hand-vector suites (`tests/<dir>/tests.rs`) — the assertions no other layer can make

Each fixture dir's `tests.rs` is appended into the generated crate and `cargo test`ed, so it runs
against a real generated API with real wire bytes. That makes it the only home for five assertion
shapes, each of which a snapshot, a compile gate and a round-trip mint all structurally miss. Write
new hand vectors in these shapes; a vector that merely EXERCISES the code certifies nothing.

1. **Assert the member AFTER the thing under test.** A framing/cursor bug inside a nested decode is
   invisible in the nested value itself — it shows up as the FOLLOWING member reading garbage. So a
   vector for a nested decode pins the value of the member that comes after it. Exemplars:
   `cbor_payload_leaves` / `cbor_payload_collections` in `tests/core/tests.rs`, whose final assert is
   the `tail` member — the four `bytes .cbor` leaves that read the OUTER buffer instead of the
   payload's own cursor all decoded their own payload correctly and mis-framed everything after it.
   **A snapshot pin cannot make this assertion at all:** text blessed while such a bug is live stays
   green forever, because the emitted text is exactly what the generator (wrongly) intends.
2. **Hand-build wire bytes the emitted serializer can never produce.** A round-trip mint only reaches
   encodings the generator writes, so decoder branches gated on any other encoding are unreachable by
   construction. Exemplar: `cbor_payload_indefinite_inner` (`tests/core/tests.rs`) hand-writes an
   INDEFINITE-length array and map inside a `bytes .cbor` payload — the emitted serializer only ever
   writes definite lengths, so the payload's break probe has no other way to be reached.
3. **Assert the REASON a rejection gives, not just that it rejects.** `is_err()` passes for every
   reason, including the wrong one. Exemplar: `opt_fixed_member_map` (`tests/core/tests.rs`) feeds a
   present `nint` key carrying the wrong constant and asserts the message reads
   `Expected fixed value -7 found -8` — the value the CDDL AUTHORED, on both sides. Routing the nint
   through its CBOR wire form (`-1-N`) instead rendered the same vector as "Expected fixed value 6
   found 7": arithmetically correct and findable nowhere in the spec the user wrote.
4. **Extend the value anchors when the type grows a field.** A suite that decodes new data while
   asserting nothing about it keeps passing and quietly goes vacuous for its own shape. A change that
   adds fields to a type appearing in an existing hand-vector suite extends that suite's anchors and
   its serialize side in the same change, and review walks the suite's assert list against the type's
   new field list. (Mutation scoring cannot substitute: a generator mutant breaking the new behaviour
   dies to any OTHER fixture asserting the same arm.)
5. **Pin a re-exposed dependency's behavior at the wire, decode→MUTATE→re-serialize.** Round-trip
   suites never mutate between decode and re-serialize, so semantics a third-party type contributes
   through our public surface — a `Deref` target's mutation-order behavior, an entry API's
   position side effects — are exercised by no other layer, and a dependency swap or version bump
   that changes them passes every compile gate and round-trip while silently rewriting consumers'
   re-serialized bytes. Exemplars: the `ordered_hash_map_*` pins in
   `tests/preserve-encodings/tests.rs` (insert-overwrite moves to back; `or_insert` keeps position;
   `from_iter` duplicate keys; entry match shapes), written and committed green against the
   incumbent backing BEFORE the linked-hash-map→hashlink swap, so the swap had to reproduce bytes
   rather than bless new ones. The shape also covers a dependency's ACCEPTED INPUT GRAMMAR and
   RENDERED ERROR TEXT, which round-trips are equally blind to (they only feed back what the
   encoder produced): the hex wire pins in `tests/raw-bytes/tests.rs` and `tests/json/tests.rs`
   pin case handling, `0x`-prefix rejection, and the error Display texts a `DeserializeError`
   renders, committed green against the incumbent before the `hex` key moved to the `const-hex`
   package. That swap, and the deliberate narrowing that followed it, are what the shape is for.
   The pins made every grammar decision an explicit line in a diff rather than a property of
   whichever decoder happened to be linked: the new decoder's `0x` leniency was refused at both
   sites instead of silently widening the read side; the Display casing that DID move flipped
   loudly; and when the maintainer then chose a canonical-only grammar — rejecting the uppercase
   input the surface used to normalize — the pins that had to flip named exactly the behavior
   being traded away, in the commit trading it.
   When to write these: any change to the version floor or identity of
   a dependency whose types the generated public API re-exposes lands pins of the incumbent's
   consumer-reachable behavior first, in their own commit (the full working rule and its trigger
   ledger live in `testing.dependency-swap-pass-compile-parity-existing-gate-silently`).

`tests/recursive-collection-ref/` exists purely to run one hand suite across two profiles rather
than to add a shape: a nominal reference to a collection typedef, generated under BOTH the default
and preserve profiles (`recursive_collection_ref` / `recursive_collection_ref_preserve`), because
the encoding-sidecar half of that shape exists only under `--preserve-encodings`. Its sibling
`tests/emit-tests-bounded-key/` is the opposite case and is included here for the contrast: it has
no `tests.rs` at all, because the values under test are the ones the `--emit-tests` minter chooses,
so its discipline is about the MINT rather than about hand anchors (see § "Authoring standard for a
bounded-domain emit-tests fixture").

**A new `tests/<dir>/input.cddl` owes the wasm-parity registry an entry.** The corpus axis of
`wasm_api_parity` enumerates `tests/*/input.cddl` at runtime and requires every dir to be in either
its corpus table or its exclusion table; the guard lives in
`wasm_parity_tests::wasm_api_parity_axes_and_pins_are_live`, a plain `#[test]` and therefore
**local** tier. `fast`'s only cargo test invocation is `cargo test --bin cddl-codegen snapshot_tests`,
so `fast` catches a new `#[test]` that fails to COMPILE and never one that FAILS — adding a fixture
dir under a `fast`-only workflow will not surface the missing row.

### Workspace mode (`--workspace-dep` / `--wrapper-requests` / `--key-requests`)

One cross-crate system: dep-owned placement of all-one-dep collection wrappers via request
sidecars, plus the map-key-derive channel — the consumer's `borrowed_key_types.rs` sidecar and
dep-side pre-finalize `used_as_key` seeding from both request channels, so a dep type keyed only
by a consumer still derives `Eq/Ord/PartialOrd` (+`Hash` under preserve-encodings). User docs:
`docs/docs/command_line_flags.mdx` and `docs/docs/output_format.mdx` § "Workspace mode". Its
facets, each with its own pins:

- **Key flavors.** A borrow carrying a `@used_as_key` flavor (`hash`/`ord`) requests exactly that
  trait family via an optional third row column with per-flavor compiled self-checks; all-bare
  sidecars keep the two-column form byte-identically. The column/parse legs are covered by the
  `wrapper_requests` unit suite (`key_types_accepts_flavor_column`,
  `key_types_rejects_unknown_flavor`); the compiled cross-crate seam by
  `workspace_key_requests_flavored_contract`: a `@used_as_key hash`-tagged dep extern emits the
  three-column row + per-flavor self-check, the dep's `--key-requests` regen derives exactly the
  named family (a hash-only borrow does NOT force `Ord` through the dep's Ord-refusing field),
  both crates compile against each other, and widening the flavor to `bare` fails the dep compile
  naming `Ord`.
- **Rest-row key domains.** An open struct-map's rest row (`* K => V`) is a key-demand source that
  no conceptual IR walk sees: the row stores its key/value flat, so its `Map(K, V)` container exists
  only through `RestRow::container_type`, and both cross-crate channels read that container.
  `workspace_key_requests_rest_row_contract` is the compiled round trip over
  `tests/workspace-requests/consumer_inputs_rest` + `dep_inputs_rest`: a DEFAULT row's key demands
  `bare` (its `BTreeMap` needs the full bundle), a `@duplicates preserve` row's key demands the `ord`
  relaxation (its `PairMap` compares by linear scan), an Ord-refusing `@used_as_key hash` borrow
  beside them keeps its narrower flavor, the dep derives per row through `--key-requests`, and all
  four wrapper classes the rows name (both containers plus their keys-lists) defer to the dep through
  `--wrapper-requests` — with both rust crates compiling against each other and both wasm crates
  linking for wasm32, which is what proves ONE `#[wasm_bindgen]` definition of each class across the
  pair.
- **Scoped self-check paths.** The self-check asserts each borrowed key at the dep's REAL module
  path — scoped (`wr_dep::sub::module::ScopedKey`) when the type lives in a non-root scope, the
  same path the consumer's own generated `use` lines take — while rows stay the bare
  `(dep, cddl ident)` the dep resolves scope-agnostically (no scope column, so the sidecar format
  is unchanged and root-only sidecars are byte-identical). Pinned by
  `borrowed_key_types_self_check_carries_scoped_dep_path` (emission: scoped path present,
  root-path bug form absent, rows bare), `key_types_skips_scoped_self_check_body` (an OLD parser
  reading a NEW scoped sidecar — the self-check body is skipped wholesale), and
  `workspace_key_requests_scoped_contract` (the two-crate compile contract over
  `tests/workspace-requests/consumer_inputs_scoped` +
  `tests/workspace-requests/dep_inputs_scoped`: scoped emit, dep-side bare-ident resolution
  deriving on the SCOPED rule, both directions compile).
- **Mode-independence.** `--workspace-dep` is honored under `--wasm=false` exactly as under
  `--wasm=true` — the key sidecar and the flag's startup validation apply in both modes (only the
  wasm-side deferral surfaces are wasm-gated) — pinned by the flavored contract's rust-only leg
  (byte-identical sidecar, no `wasm/` tree) and
  `workspace_dep_unknown_is_rejected_under_wasm_false` (an unknown dep exits nonzero in rust-only
  mode, never a silent ignore). `--extern-wrapper-index`'s startup validation is likewise
  mode-independent — an unknown dep or a malformed index line is a hard error under
  `--wasm=false`, even though the deferral it feeds is wasm-gated — pinned by
  `extern_wrapper_index_is_validated_under_wasm_false` (both malformation classes exit nonzero in
  rust-only mode).

The whole surface is pinned by three sibling gates plus the parser's unit suite
(`src/wrapper_requests.rs` — both the strict sidecar grammars and the lenient shape-key
extractor):

- `workspace_dep_defers_to_dep` — consumer side over `tests/workspace-dep-wasm/`: unconditional
  all-one-dep deferral incl. NonEmpty and nested shapes, the byte-frozen `borrowed_collections.rs`
  sidecar format asserted as full-file equality — it is a cross-crate contract — plus
  ownerless/mixed composition with `--extern-wrapper-index` and the rule-declared shadowing
  warning (the AUTHORED-rule TRUE positive: `idx_foo_list = [* idx_foo]` warns and mints locally).
  `workspace_dep_named_table_deferred_keys_list` is its FALSE-positive companion (same fixture
  family, isolated input set): a NAMED table over a dep-owned key in a non-root scope
  (`{ * idx_foo => local_thing }`) synthesizes its `keys()`-list `IdxFooList` at registration time,
  so that keys-list must borrow cleanly — no criterion-9 shadow warning (it is not rule-declared)
  and the deferred `use <dep_wasm>::collections::IdxFooList;` import present in the module holding
  the table class (previously stranded — the issue's warned-shadowed + sidecar-borrowed +
  never-imported three-way contradiction, E0412).
- `workspace_requests_hosts_borrowed_wrappers` + the hard-error tests (including
  `workspace_key_requests_derive_effect_and_hard_errors`, the `--key-requests` intake: derive
  effect, unknown-ident hard error, other-dep row filtering; and
  `workspace_key_requests_flavored_contract`, the flavored `@used_as_key hash` cross-crate
  contract over `tests/workspace-requests/*_flavored`: three-column emit, per-family dep derive,
  both-directions compile, and the bare-widening red proof) +
  `workspace_requests_alias_elements_host` — dep side over `tests/workspace-requests/`: strict
  sidecar intake, union-by-shape with sorted requester attribution, own-spec-shape satisfaction,
  flag-order byte-identity, the criterion-8 hard errors plus the review-hardened classes — the
  stub-fidelity diagnosis for directly-exposable shapes, reserved element idents, the
  shape-nesting depth cap, and the element-resolution appendix on name↔shape mismatches — and
  alias-element hosting: request leaves resolve through the pipeline's `resolve_alias`, the single
  owner of the alias-substitution rule, so requested wrappers over
  `stake_credential = credential`-style aliases, primitive aliases, and externs generate exactly
  what the dep's own spec would. `workspace_requests_hosts_cross_scope_elements` is the host-side
  element-import contract: a hosted wrapper's body names its element wasm classes bare, and the
  requested-collections module computes explicit imports for them instead of relying on the
  root-only `use super::*;` — a cross-scope generated element compiles via its true
  `crate::generated::<scope>::…` import (full `cargo check`), and a scoped extern element is
  imported through its crate-root re-export glue (generation assertion — a bare-stub extern has no
  hand-written runtime to compile against).
  `workspace_requests_cohosted_keys_list_no_self_import` is the keys-list twin of that walk's loose-
  `try_from`-source guards: when a hosted map's `keys()`-list wrapper is itself co-requested into the
  same `requested_collections.rs` (the normal case — borrowing `{* k => v}` also borrows `[* k]`), the
  walk must NOT emit `use crate::generated::<KeysList>;` for it (it is minted in that very file, so
  the root defines no such name — E0432); the map's genuine root-hosted element class is still
  imported, and the wasm crate `cargo check`s (RED pre-fix: E0432 unresolved import).
- `workspace_regen_two_consumer_contract` — the regen-contract gate over `tests/workspace-regen/`:
  an umbrella wasm cdylib linking one dep + TWO consumers, RED with duplicate symbols when both
  consumers mint and GREEN after a reverse-dependency-order holistic regen, then the in-place
  lifecycle — zero-diff unchanged regen, requester churn without preservation traps, last-borrower
  removal, and the new-borrow-before-dep-regen unresolved-import failure. The regen gate runs
  every generation IN PLACE over prior output precisely so the edit-preservation overlay
  participates — it is what caught the sidecar's in-const legend comment trapping on borrow
  removal (since relocated to the file banner, where comments anchor to structure that always
  exists).

### Extern-interface export & `--extern-import` (the machine-generated stub channel)

Every regen emits `extern-interface/<dep>/**` — a committed CDDL projection of the crate's
extern-visible surface (opaque `_CDDL_CODEGEN_EXTERN_TYPE_` rows, truthful transparent spellings,
`@rust_name` pins, `; unexported:` exclude-with-record + reference-closure) that a consumer feeds
back via `--extern-import` in place of a hand-stub tree (user docs:
`docs/docs/output_format.mdx` § the export tree, `docs/docs/integration-other.mdx`,
`docs/docs/command_line_flags.mdx` § `--extern-import`). Its test layers:

- **Renderer floor** (`src/generation/extern_interface.rs` in-file vectors): IR→CDDL spelling per
  shape, never-lossy by construction — an unrenderable shape is a hard `Err` the projection turns
  into an exclusion record, never a guessed spelling; the `RustStructType`/`ConceptualRustType`
  matches carry no `_ =>` arm, so a new IR variant fails compilation until it chooses a spelling.
- **Projection snapshots** (`snapshot_tests.rs`, fast tier): `extern_interface_emit` pins the full
  emitted tree over `tests/extern-interface-emit/` (every projection row, depth-1 exclusion of the
  spec's own extern deps, closure records naming the chain root, prelude refs rendering rather than
  excluding); `extern_interface_emit_is_deterministic` (double-emit byte-compare),
  `extern_interface_emit_same_in_both_modes` (rust-only = wasm — emission is mode-unconditional),
  `extern_interface_emit_empty_surface` (header-only file keeps stable presence),
  `extern_interface_emit_exclusions_and_closure`, and `extern_interface_check_emit` (the
  self-check file's content).
- **Compiled self-check** (`integration_tests.rs`, nested-cargo, local tier): every generated rust
  crate carries `src/generated/extern_interface_check.rs` asserting each exported name is a real,
  correctly-bounded type (`Serialize`, per-type-weakened `Deserialize`, `RawBytesEncoding`,
  existence-`use` for transparent rows) — `extern_interface_check_compiles` is the green half;
  `extern_interface_check_mutation_fails_build` deletes one generated type and requires the dep's
  own build to go RED naming it (the stale/hand-edited-export failure mode, proven not assumed);
  `extern_interface_check_weakens_deserialize_bound` / `extern_interface_check_skips_generic_base`
  pin the two soundness carve-outs.
- **Consumer seam** (`src/tests/extern_import_tests.rs`): the acceptance criterion in two halves —
  `extern_import_matches_hand_stub_byte_for_byte` (seam identity: identical rule text through flag
  vs physical stub lands identical bytes) and `extern_import_matches_pinless_hand_stub_byte_for_byte`
  (migration identity: a pin agreeing with today's derivation changes nothing — the half that
  caught pin==derived emitting `use dep::Foo as Foo;`). Plus the strict-seam vectors (missing
  header / unknown version / unknown `@`-annotation / flag-vs-physical double declaration / empty
  path / malformed value all hard-error; an export whose `; unexported:` records mention DSL tags
  still parses cleanly), the wrapped staleness diagnostic, and single-file-consumer ROOT_SCOPE
  preservation.
- **Writer vocabulary** (same file, `EXTERN_INTERFACE_WRITER_VOCABULARY` +
  `extern_interface_writer_vocabulary_matches_the_writers`): every `@…` annotation
  `generation/extern_interface.rs`'s rule-line assembly can WRITE into an export, paired with the
  acceptance vector proving this crate's own parse still accepts what its writer emits. The scan is
  a TOTAL verdict over the emitter's `@…` literals — each is classified writer or diagnostic, so a
  new writer literal with no row fails, and a row no writer emits fails the other way; a
  spelling-by-spelling list would not have that property, since a new writer could hide in the
  unclassified remainder. It closes
  the cross-crate skew class in BOTH directions at once: a directive that should travel and does
  not (the forward direction), and a projection emitting a spelling the parser refuses (the
  converse, which a refusal delivery has no reason to look for, because the writer already exists
  and no representation changed). Adding a writer means adding a row AND its consumer vector.
- **Transitive floor** (same file, `transitive_*` over `tests/extern-import-transitive/`): depth-1
  export purity (a mid-dep transparent rule referencing a base-dep type is closure-excluded, and
  no base-dep ident appears in any exported body), two-flag composition with right-crate `use`
  targeting, the opaque boundary hiding the dep-of-dep, byte-identity at three-crate scale, and
  `transitive_wasm_sidecars_carry_dep_cddl_idents` — the workspace sidecars above
  (`borrowed_collections.rs` / `borrowed_key_types.rs`) byte-identical through either channel,
  rows still keyed by the dep's ORIGINAL CDDL idents (the `--wrapper-requests`/`--key-requests`
  read-back contract).
- **`@rust_name` floor**: `comment_ast.rs` unit vectors plus `src/tests/rust_name_tests.rs`
  (import-seam aliasing, the wasm full-path bypass site, exported-rule rejection, reserved-name
  pin rejection); `cddl-matrix/corpus_detect.ts` obtains its `dsl.rust_name` credit from the
  parser-backed `comment_dsl` authority helper, and the matrix feature row is compile-exempt because
  it pins a dependency-crate type name that cannot compile standalone.

`flag_value_smoke` generate + `cargo check`s a rich extern-free input (`tests/canonical`) under each
documented flag *value* that no named profile exercises (`--annotate-fields=false`,
`--to-from-bytes-methods=false`, `--binary-wrappers=true`) — each selects a whole alternative emit
path. `--canonical-form=true` requires `--preserve-encodings` (on its own it emits a non-compiling
crate); that combination is rejected in `api::with_types` and pinned by
`flag_value_rejects_canonical_without_preserve`.

`binary_wrappers_round_trip_byte_exactly_and_enforce_bounds` is the semantic companion for
`--binary-wrappers=true`: a bespoke scratch spec generates bounded byte-string wrappers under both
default and preserve rows, then its generated crate decodes/re-encodes hand-derived direct and
nested vectors. The vectors prove range/exact-size rejection, a nested wrapper's one-frame ownership
(not a byte string containing an encoded byte string), and preserve-mode fidelity for independently
widened byte-string heads (8-, 16-, and 32-bit). It closes the byte-level gap that a compile smoke
cannot see.

`generated_code_clippy_clean` runs `cargo clippy` over every generated crate a case mints, for four
cases. Two run the rich extern-free input `flag_value_smoke` uses, under default flags and under
`--preserve-encodings --canonical-form`, generated into the gate's own temp dir (so it can't race
the fixtures' reused `tests/<dir>/export` outputs). A third swaps the input for a minimal spec the
gate writes into that same temp dir, under `--preserve-encodings --annotate-fields=false`: verify-only
fixed bool/null/undefined in member position and in all three arm positions (map-rep, array-rep,
type-choice), including their mixed-special brute-force dispatch.
Those shapes are a table-driven cross-product of `FixedValue`'s unit-like major-7 values — both
bool spellings, null, and undefined — across mandatory/optional array and map members plus every
arm representation. A parsed-AST LOCKSTEP check over `FixedValue` refuses a new unit-like variant
until it receives a row; `nil` has separate mandatory/optional identity witnesses for the same
`FixedValue::Null`. `--annotate-fields=false` is what makes the member position emit its unbound
value, and the mixed-special choice reaches brute-force dispatch. The rich input additionally
contains a deliberately asymmetric ~30-`uint`-field record choice; the gate asserts both generated
owners mint, so its permanent generated-module `large_enum_variant` allow stays exercised. It does
not claim that input crosses `result_large_err`'s static-error threshold; that adjacent permanent
allow remains policy for layout-dependent consumers. The fourth runs the rich input under `--json-serde-derives
--json-schema-export`, the flag pair `ALL_PROFILES` spells for its `json` row, and lints the THIRD
generated crate: `wasm/json-gen`, whose emitted `add_schemas`/`export_schemas` bodies and
registration rows no other lint gate reaches (it is an independent nested crate, not a dependency of
`wasm/`, so linting `wasm/` never compiles it). A case whose crate is missing fails rather than
skipping. What it proves: emitted source is lint-clean for
the covered profiles, modulo the permanent input-dependent allow described below — the
emission-quality class that snapshots and round-trip suites are blind to (they pin bytes and
behavior, not idiomatic-ness; a degenerate `();` statement compiles and round-trips green but
degrades every consumer's `cargo clippy`). What it can't prove: semantic correctness — a
wrong-but-idiomatic deserializer passes.

All three generated crates are denied under `clippy::all` with an empty emission-quality burn-down;
the only allow is permanent and input-dependent: `clippy::disallowed_names` (the fixture's own
`foo`/`bar` rule names become generated parameter names — not a generator defect). The gate also
denies a curated rustc style-lint set (`unused_parens`, `unused_braces`, `unused_allocation`,
`unused_variables`) that catches redundant emitted grouping/allocation and dead emitted bindings
without denying `unused_imports`. That asymmetry is deliberate: `unused_imports` keeps
the one residue the usage-derived import prune (`import_prune::prune_generated_files`)
deliberately leaves — trait imports (`cbor_event::se::Serialize`), exercised via method calls whose
ident never appears, so name-scanning cannot prove them unused — while `unused_variables` has NO
legitimate residue, so an emitted binding nothing reads is a generator defect every time. The
corpus-wide owner of that same class is `feature_corpus_compiles`'
`unused_generated_variable_lines` scan. Everything else — the concrete
collection/encoding idents, the `super::*` glob and enumerable `error::*`/`cbor_encodings::*`
globs, cross-scope type imports, wasm macro/prelude imports, and every private import of a
re-export-only extern-glue file — IS pruned at generation time (the contract lives in
`docs/docs/output_format.mdx`; the warning-severity detector is `feature_corpus_compiles`'
`unused_generated_import_lines` scan). Two model choices are what make the glob half sound, and
because the real output no longer holds the shapes that would exercise them end-to-end they are
pinned by `import_prune`'s own map-level unit tests
(`keeps_parent_import_consumed_by_child_via_super_glob` and siblings): a file's protector set is the
super-glob EDGE graph (`reachable_via_super`), not every structural descendant — so a sub-scope
`serialization.rs` no longer protects the root `mod.rs` — and a descendant that reaches the name
through its OWN glob of the same module disqualifies rather than protects the ancestor's copy (the
`--common-import-override` `serialization::*` shape). The used-ident scan counts only a `::`-path's
LEADING segment, since a tail segment collides with the names a parent binds by `pub mod` and kept a
dead `use super::*;` alive in the workspace-requests sidecars until the exclusion landed; both
directions are pinned by `path_tail_segments_not_counted` and its map-level twin
`sidecar_super_glob_pruned_when_sub_only_path_qualified`. It is intentionally not `-D warnings` (see `tool_cmd`'s doc comment). The wasm leg uses the same deny/allow set as the rust leg; any new
`clippy::all` lint class is hard-red on both profiles and both generated crates.
Tier: check.ts `local` as a plain non-ignored test, kept below the ~90s warm wall-clock threshold.
A warm run measures ~2s, which looks vacuous but is not: regeneration is byte-identical, so cargo's
incremental compilation replays content-hashed lint results instead of re-checking — and any real
content change goes Dirty and re-lints (verified by injecting a `();` `no_effect` canary into the
generated source and watching the gate command fail). Re-prove it that way, not by timing, if the
speed ever raises the suspicion again.

Distinct from this generated-code gate, the fast-tier WORKSPACE clippy gate (check.ts's `clippy`)
denies `clippy::all` PLUS the restriction lint `clippy::assertions_on_result_states` over the
repo's own code: an `assert!(r.is_ok())` / `assert!(r.is_err())` discards the payload that would
attribute the failure, so a Result assert takes the `.expect()` / `match` form whose panic carries
the error (a red run — above all a transient one — must be actionable from its first capture). A
genuine can't-Debug case takes a site-local `#[allow]` with a reason — the visible, reviewable form
of the tradeoff. The `is_ok`/`is_err` fragments in `emit_tests.rs` and the replay harness are
EMITTED text compiled in generated crates, outside this lint's scope.

Fixture-appended tests under `tests/*/tests*.rs` are also outside workspace clippy because they
compile only inside generated crates. A textual sweep in the default integration suite bans fresh
`assert!(...is_ok())` there, gated by `fixture_appended_tests_do_not_assert_is_ok`; positive
Result checks should use `.unwrap()` or `.expect()` so the generated-crate failure includes the
error payload.

Deserialize-error annotation contract (the `error_annotation_*` tests in `tests/core/tests.rs`,
plus `error_annotation_tag_mismatch_single_name` in `tests/preserve-encodings/tests.rs`): every
fallible part of a record's header parsing — container major type, definite-length checks, tag
reads — errors with the type name as the location (`Deserialization failed in Foo because: …`),
the same way field-level failures always have, and a tag mismatch carries the name exactly *once*
(inside the `.annotate(name)` closure the tag check is emitted locationless; a name-carrying form
there would read "Foo.Foo"). The two `_control` cases anchor that field-level and
missing-mandatory-field annotation is not lost when the header code is restructured. The
*enum-direct* tag check — a tag over a whole top-level type choice
(`tagged_type_choice = #6.11(uint / text)`), which deserializes directly with no container rep —
is pinned to the same once-only contract by `error_annotation_tag_mismatch_type_choice_direct`,
and the `generate_tag_check_arms` unit test in `snapshot_tests.rs` renders both arms to pin the
name-carrying `--annotate-fields=false` form that no fixture exercises.

An enum `NoVariantMatched` failure on a directly-deserializing choice — where the `_ => NoVariantMatched`
(and group-choice `NoVariantMatchedWithCauses`) arm sits inside the `.annotate(name)` closure —
carries the name exactly *once*: the arm emits the locationless `DeserializeFailure::…into()` form
and lets the closure supply the name, pinned by `error_annotation_no_variant_single_name`.

The contract now covers the two paths that previously sat outside it — embedded/plain-group
`deserialize()` header scaffolding and newtype wrappers' container reads. A standalone plain group's
header reads sit inside a `.annotate(name)` closure that returns the `(len, read_len)` bindings (the
delegated `deserialize_as_embedded_group` call stays OUTSIDE it — its body is already annotated
per-field, so wrapping it would double-annotate as "Type.Type.field"), and its post-delegation
final-len check gets its own annotate closure; a newtype wrapper's whole deserialize body is wrapped
in one such closure while its `new()`/`TryFrom` range check keeps the name-carrying
`DeserializeError::new` form (no closure wraps those). Pinned at fixture granularity by
`error_annotation_plain_group_header_single_name`,
`error_annotation_wrapper_wrong_container_single_name`, and
`error_annotation_bounded_wrapper_range_single_name` in `tests/core/tests.rs` (with an
encoding-fields sibling `error_annotation_wrapper_and_plain_group_single_name` in
`tests/preserve-encodings/tests.rs`), and at catalog breadth by the replay gate: its
`HEADER_MUTANT_LOCATION_SKIP` ledger is EMPTY at HEAD (stale-guarded), the only known-legitimate
locationless resident being the `from_cbor_bytes` `TrailingData` path (pinned by
`error_display_formatting`'s TrailingData no-location case).

`cargo_manifest_disk_round_trip` and `cargo_manifest_rejects_unparseable_existing` pin the
manifest merge contract on real disk (the only place generation reads prior output — see
`cargo_manifest.rs` and AGENTS.md's determinism note): user edits outside tool-owned keys survive a
regen, the seeded `package.version` stays bumped, tool-owned keys (incl. the version stamp) are
restored, a further regen is a byte-identical fixed point, and an unparseable existing manifest is a
hard error naming the file rather than a clobber. Note for harness authors: because manifests merge
rather than clobber, `run_test` deletes the three manifests in its reused export dirs before
regenerating — its raw-appended `test_deps` would otherwise accumulate across runs.

The names those manifests carry are checked against the SURROUNDING workspace — an input read, not a
prior-output one — by `workspace_package_name_collision_warning_names_both_manifests` (integration,
a real CLI spawn): a `package.name` another member of the enclosing workspace already holds is
announced on stderr naming both manifests and the `--lib-name` remedy, since cargo would otherwise
report it at the consumer's next build. Pinned with all three negative controls (no workspace above
the output, a workspace with no clash, and a workspace whose colliding member IS this generated
crate) and `--verbosity=error` silence, because the warning IS the whole mitigation.

`getting_started_example` pins the documented first-run experience: it generates from
`example/test.cddl` — the spec `docs/docs/getting_started.mdx` tells a newcomer to run verbatim —
and `cargo check`s both the rust and wasm crates, so that command can't rot silently.

### Independent conformance oracle (`tests/deser_test_conformance.rs`)

A round-trip only proves our encoder and decoder agree with *each other* — a symmetric bug passes.
For a second oracle whose **decode + constraint-evaluation** path is independent of ours,
`deser_test_conformance.rs` validates our serialized bytes against the source `.cddl` using the `cddl`
crate's validator (`validate_cbor_from_slice`, which decodes with ciborium and evaluates constraints
itself). A **failure is a strong signal** (our bytes don't match the spec the generator was built
from); a **pass is weak** — the validator has known gaps (they come and go with the pinned fork
rev: the current ledger is `cddl-matrix/README.md` § "Upstream oracle gaps"; e.g. released 0.10.x
does not enforce control ops over a `uint` target) AND it is *not fully decorrelated*: it parses
the `.cddl` with the same dcSpark `cddl` fork at the same pinned rev as the generator's own front end
(`CDDL_ORACLE_DEP`), so a **fork-level misparse** (grammar/AST bug that corrupts generator IR and this
oracle's spec-interpretation identically) escapes it. `CDDL_ORACLE_DEP` is behaviorally checked before
the corpus gate by `rust_oracle_fingerprint_preflight`, using the same
`cddl-matrix/oracle_fingerprint.json` probe set that `verify.ts` uses for the `RUST_CDDL` binary; a
wrong rev, a stale gap pin, or an emptied probe file fails as a harness error before fixture generation.
That still does not decorrelate parser lineage, so the specific fork-misparse class is covered by the
harness-side ruby `cddl` gem in `ir_conformance_corpus` (below), which shares no parser with the fork.
Because the validator validates against a spec's first type rule only, the helper prepends a synthetic
root aliasing the rule under test.

It's wired into the `preserve-encodings` fixture (the richest hand-written round-trip surface, and the
one whose whole point — irregular definite/indefinite encodings — most needs an independent structural
check): `run_test` appends the helper and adds the `cddl` git dep to that generated crate. Broadening
to more fixtures is a compile-cost trade-off (the `cddl` dep is heavy), not a limitation of the helper.
See `tests::cddl_crate_conformance` in `tests/preserve-encodings/tests.rs`.

### Spec-anchored golden vectors (`tests/golden_hex*`)

Three fixtures assert exact CBOR bytes hand-derived from RFC 8949 rather than built with any
encoding helper — the only oracle class that catches a *symmetric* encode+decode bug (both sides
wrong in compensating ways round-trips green everywhere else):

- **`tests/golden_hex`** — default flags; RFC 8949 Appendix A known-answer vectors, both
  directions. Coverage map: [`tests/golden_hex/COVERAGE.md`](golden_hex/COVERAGE.md), projected and
  CI-drift-gated by the `project_golden_hex_check` gate (`cddl-matrix/project_golden_hex.ts`).
- **`tests/golden_hex_preserve`** — `--preserve-encodings`; irregular §3 encodings (non-minimal
  header arguments, indefinite/chunked items, map key order) must re-encode byte-identically.
- **`tests/golden_hex_canonical`** — `--canonical-form`; the same irregular inputs must re-encode
  to hand-derived §4.2 minimal bytes (and those bytes must be a canonical fixed point).

The preserve/canonical suites' *other* byte assertions are built with `tests/deser_test`'s
cbor_event helpers — the same `write_*_sz` primitives the generated code encodes with — so these
raw-hex sets are the independent spec anchor for those modes. The projection validates every
golden byte array in all three dirs (two-digit `0x??` literals, exactly one well-formed CBOR item)
and hard-fails otherwise; regenerate + commit `COVERAGE.md` after editing any of them.

The `opt_set`/`opt_set_holder` vectors in the preserve and canonical suites are the wire anchor for
the **transparent tag-set idiom** (`#6.258([* a]) / [* a]`, REQUEST-08): the preserve suite pins
both arms round-tripping byte-exact (untagged, tagged sz-Two, tagged non-minimal sz-Eight), and the
canonical suite pins that `--canonical-form` normalizes the tag's SIZE (wide → sz-Two) but never its
PRESENCE (tagged stays tagged, untagged stays untagged — presence is not canonicalized). These are
the untagged-arm and size-not-presence directions `--emit-tests` alone cannot reach (it mints from
construction defaults, which are always tagged).

### Transparent tag-set idiom (`#6.N([* a]) / [* a]`) — test map (REQUEST-08)

The tag-258 set-idiom collapse (user doc: `docs/docs/current_capacities.mdx` § "Transparent tag-set
idiom") is verified across the layers:

- **Recognition + IR/source shape** — `src/tests/optional_tag_set_tests.rs` (in-process, fast):
  the collapse to a transparent `Vec`/`NonEmptyVec` alias (arm order irrelevant, any tag number,
  preserve tri-state + non-preserve default-tagged), the near-misses that KEEP the enum (mismatched
  bounds, different element types, both arms tagged, 3+ arms), generic-def instances, the
  reference-site outer-tag PARITY invariant (generic instance vs non-generic equivalent generate
  byte-identically), and a collapsed set used as a type-choice variant discriminating coherently on
  the two-entry `cbor_types()` (`Type::Tag | Type::Array`). Generic-instance field convergence
  (Phase 2.5) is pinned in `src/tests/generic_collection_tests.rs`.
- **Anonymous-instance wasm convergence** (REQUEST-09) — `src/tests/generic_collection_tests.rs`
  (in-process source assertions): a SYNTHESIZED anonymous instance at a field site lowers exactly
  like the inline collection it denotes. A wrapper-needing element (`[pool_owners: set<key_hash>]`)
  lowers to the STRUCTURAL name (`KeyHashList`/`NonEmptyKeyHashList`, not the rule-named
  `SetKeyHash`) with a passthrough alias, so the instance and its inline `[* key_hash]` twin are ONE
  wasm class; a directly-exposable element (`set<uint>`) lowers to a bare by-value `Vec<u64>` with
  no class, its wasm output byte-identical to the inline `[* uint]` equivalent
  (`anonymous_exposable_instance_wasm_matches_inline`). Only a NAMED instance rule
  (`named_set = set<key_hash>`) KEEPS its own class. End-to-end against `--wrapper-requests` in
  `integration_tests::workspace_requests_anonymous_collapsed_set_satisfies_from_own_spec`: the
  structural request is satisfied by own-spec (no criterion-8 #3 collision on the synthesized name),
  and the named-rule boundary still hard-errors naming `NamedSet`.
- **Occurrence-aware instance identity + recursive collection boundary** —
  `integration_tests::generic_instance_occurrence_bounds_have_distinct_nominals_and_compile`
  crosses loose, outer-bounded, inner-bounded, and doubly bounded arguments through anonymous uses
  and named bindings, source-asserts each distinct native/WASM nominal and carrier, then checks both
  generated crates. `integration_tests::nested_restricted_map_wasm_boundaries_have_distinct_nominals_and_compile`
  applies the same identity rule recursively to map keys and values, including preserve-pair flavor,
  and checks both source orders compile with distinct native carriers and WASM classes. The deliberate
  table-key exception loosens only an outer array or map occurrence/policy restriction for a bounded
  table's checked builder and the `keys()` structural wrapper; nested array/map restrictions still
  contribute to their identities. The focused regressions cover emitter,
  mint/reference/import/collision, and wrapper-request reconstruction seams; the standing table-key
  carrier, synthesized-name interaction, and workspace wrapper-routing tests keep their respective
  seams.
- **Compile + round-trip** — the `tag_set_idiom` / `tag_set_generic` / `tag_set_near_miss` corpus
  fixtures (`feature_corpus` snapshots + `feature_corpus_compiles`' three-profile compile and the
  default-profile `--emit-tests` byte-exact round-trip of the tagged arm). `tag_set_generic` carries
  a BYTES instance, so its wasm-face compile cell also covers the list-door ABI contract below.
- **The list-taking wasm doors' ABI contract** — a `try_from`/`try_opt_from` taking `Vec<Elem>` is
  legal only when a `Vec` **of** the element crosses the wasm boundary, which is strictly stronger
  than the element itself crossing it: `bytes` (already `Vec<u8>`) and `bool` (no
  `VectorFromWasmAbi`) are scalars with no bare-`Vec` form, so every such door borrows the loose
  `<Elem>List` class instead. The predicate is `RustType::vec_of_self_directly_wasm_exposable`,
  spelled as the same array-level probe `name_as_wasm_array` uses to decide whether a loose wrapper
  class exists at all — so a door that is not a bare `Vec` always has a class to borrow, by
  construction. Floor:
  `integration_tests::bytes_and_bool_element_list_doors_compile_and_round_trip` — both offending
  primitives × the set-nominal flattened doors, the `@duplicates reject` companion set wrapper and
  the restricted `NonEmptyVec` list wrapper, generated under the plain and `--preserve-encodings`
  profiles, with the plain cell running a behavioral round-trip through the doors inside the
  generated wasm crate. It is a compile floor because the defect class was exit-0 generation whose
  wasm crate then failed its own `cargo check` with `E0271 … <Vec<u8> as ErasableGeneric>::Repr ==
  JsValue`.
- **Wire bytes** — the `opt_set` golden vectors above (both arms + canonical size-not-presence).
- **Matrix cells (choice-member axis)** — `contain.choice-member.type2.tag.set_idiom` /
  `contain.choice-member.type2.tag.set_idiom_near_miss`
  (`cddl-matrix/containment/choice-member.toml`): the bare collapse and the mismatched-bounds
  NON-collapse as execution-probed (role × feature) cells, each with per-wire-arm decode-foreign
  vectors in `tests/decode_conformance/catalog.toml` (arm floor `["4","6"]` pinned in
  `project_decode_conformance.ts` — the untagged major-4 arm is the direction the default encoder
  never emits, so those vectors are the independent decode evidence). The reject-flavored idiom is
  pinned separately by the vendor feature row `dsl.duplicates.reject`.
- **Alias-of-instance chains** — a generic collection instance bound to a rule
  (`xs_int = xs<uint>`), that binding re-bound again (`bar = xs_int`), and the second binding used
  as a member or an element. Every collection flavor of the definition works — loose/non-empty
  transparent arrays and the tag-258 set idiom in both arm spellings and both duplicates policies —
  emitting `pub type Bar = XsInt;` with the use site serializing through whatever the instance
  resolved to. It works because the finalize-time field-convergence walk that repairs a use site's
  bare instance leaf descends `Alias` boxes too, for exactly the leaves that name no registered
  struct: a NAMED set-idiom binding mints its struct under the instantiation canonical (`XsU64`) and
  gives the binding ident only a transparent alias, so a second hop's `Alias(Bar, Rust(XsInt))`
  otherwise carried a leaf naming nothing. Generation is pinned per flavor × position × emission
  profile by `generic_collection_tests::alias_of_instance_chains_generate`, and the compile floor —
  the half a source assertion cannot see, since this shape's defect class was "exit 0, crate does
  not compile" as well as an outright abort — by
  `integration_tests::alias_of_instance_chain_member_compiles`. The embedded-element flavor carries
  independent decode evidence: the corpus decode-conformance row `tag_set_reject_anon_generic.outer`
  (`outer = [* oset<uint>]` under holder mode, which embeds the rule).
- **Boundary limitations** — `tests/testing-roadmap.toml` § "Deferred features", entry "Transparent
  tag-set idiom — recognized-shape boundary": non-idiom choice-bodied generic defs (refused at
  parse, not supported) and inline/anonymous two-arm choices (not recognized).

### Open struct-maps (dynamic rest rows and exact-zero fixed keys) — test map

The trailing-`K => V`-after-fixed-keys capture feature, with its own occurrence window (user docs:
`docs/docs/output_format.mdx` § "Open struct-maps", `docs/docs/comment_dsl.mdx` § rest-row
directives) is verified across the layers:

- **Front end + guards** — `robustness_tests::open_struct_map_rest_row_front_end`: recognition,
  representative loose / `NonEmptyMap` / `BoundedMap` carrier spellings, every remaining
  graceful-rejection boundary with polarity fixtures (non-final / multiple / plain-group /
  lone-non-fixed / group-choice-arm rest rows, unsupported key domains), the
  directive-slot disjointness probes (entry-level `@name`/`@duplicates` vs rule-position reads),
  the marker-slot trap pinned loud, and the lone-`* K => V`-stays-a-TABLE no-drift assertion.
- **Value-level e2e** — `tests/open-struct-map-e2e` (compiled, non-preserve): capture round-trip,
  typed-domain wrong-key errors, checked `insert_<row>` mutation through the private captured
  carrier, duplicate declared/rest CBOR-key rejection, the fixed-keys-win-on-content-mismatch
  ruling, empty-rest ≡ closed-struct bytes.
- **Wasm parent-mutation e2e** — `tests/zero-permitting-map` executes ordinary and
  `@duplicates preserve` loose-row mutation through `insert_rest`, proving the getter remains a
  detached snapshot while reread/serialize/decode observe the same parent; typed and `any`
  declared/rest collisions plus its exact-zero bounded control prove duplicate-, forbidden-key-,
  and maximum failures leave that parent unchanged. The separate
  preserve-only `tests/wasm-open-rest-mutation` fixture decodes non-minimal ordinary and pair-map
  entries, mutates the decoded wasm parent, then proves replay retains old encodings and every entry.
- **Preserve/canonical e2e** — `tests/open-struct-map-preserve-e2e` (compiled): byte-exact
  wire-order interleave at non-minimal widths, concrete key/value encoding sidecars, the
  value-duplicate rejection under BOTH key domains that reject (`0x01`-vs-`0x1801` under `* uint =>
  any`, `0x05`-vs-`0x1805` under `* any => any`), empty-rest ≡ closed
  canonical bytes, the runtime canonical merge, the `24`-vs-`10` codegen↔runtime comparator
  divergence-agreement vector, the `@duplicates preserve` pair-list twin over the concrete uint
  domain (byte-exact with duplicates present, positional sidecars, canonical stable sort keeping
  wire order), and the composition of the two — `@duplicates preserve` over the `any` KEY domain
  (`dup_any_rest`, a `PairMap<AnyCbor, AnyCbor>`): duplicate and composite (array) keys byte-exact
  with canonical re-canonicalizing to itself, indefinite map framing around a duplicated non-minimal
  key whose value payload is a run of major-7 head bytes, and float VALUES replayed at their wire
  widths whose canonical output applies each RFC 8949 § 4.2 rule (`f93c00` shrink, `f97e00` NaN
  payload drop, `f98000` signed zero).
- **JSON e2e** — `tests/open-struct-map-json-e2e` (compiled): flatten round-trip, declared-names-
  bind-first loose read, CBOR-value collision rejection after member-name coercion (including
  sized/typed keys), and the write-error postures (declared-name collision, identical
  stringifications — which is also how a pair-list holding real duplicates errors — complex `any`
  keys/values). The **TS-projection leg** (`assert_schema_projects_to_legal_ts`, § "JSON-schema →
  TypeScript JS-side pipeline") runs for every `--json-schema-export` fixture from `run_test`
  itself; what distinguishes this fixture is the extra SHAPE pins its test adds on the projected
  `.d.ts` — its rest rows cover both open-region spellings with declared members the range does
  not admit, so it asserts the emitted index signatures are the widened unions, not just that the
  file compiles.
- **Snapshots** — `open_struct_map_default` / `open_struct_map_preserve` profiles over
  `tests/open-struct-map`, which also rides the wasm-parity sweep (the e2e fixtures stay off that
  axis: their integration gates generate `--wasm=false`, so they emit no wasm surface to
  differential).
- **Wire KATs** — the `open_map` rule in all three `tests/golden_hex*` fixtures (default /
  preserve verbatim rest-key head / canonical minimized under the merge), populated only through
  the checked record API. `tests/custom-serialize-canonical-e2e` additionally proves that key
  validation executes the authored custom codec, rejects its errors atomically, and compares its
  successful CBOR image with declared keys. The workspace key-request contract compiles the same
  validator through a common-runtime override, covering the cross-crate import seam.
- **emit-tests** — the `emit_tests_open_struct_rest_execute` gate (see the emit-tests section
  above): the round-trip mint populates protected rows through checked insertion and the fidelity
  classes exercise captured entries.
- **Runtime JSON laws** — the natural-walk shims + corpus biconditional in the static-runtime
  property layer (below), which the flatten surface composes.

Exactly-zero fixed members (`0*0 key: value` / `*0 key: value`) share the record path but are
forbidden-key constraints rather than rest-row carriers:

- **Front end and every face** —
  `robustness_tests::zero_permitting_occurrence_on_keyed_map_field_uses_optional_carrier` keeps the
  positive-upper `Option` controls, exact-zero no-field representation, structured CBOR error,
  checked open-rest door, closed-JSON sentinel, directive refusals, wasm getter absence, and
  component/WIT projection in one polarity test. Component constructor compilation also lives in
  `tests/component-bounds`; extern projection/import is pinned by the group fixtures.
- **Executed default/JSON/wasm carriers** — `tests/zero-permitting-map`, driven by
  `zero_permitting_keyed_map_fields` and registered in the wasm-parity fixture
  set, executes absent round trips and forbidden-present rejection for closed/open records; checked
  complete construction and insertion; uint/text, typed-union and `any` value-equality (including a
  non-minimal text key); `@ignore` ordering; bounded, non-empty and duplicate-preserving carriers;
  JSON property rejection/schema omission; and the wasm optional/forbidden-accessor boundary plus
  its checked parent insertion door.
- **Preserve modes** —
  `integration_tests::exact_zero_typed_key_comparison_executes_in_both_preserve_modes` executes the
  typed comparator at decode, complete-construction and insertion doors under preserve and
  preserve+canonical, so their different serialization traits cannot hide behind source checks.
- **Matrix/decode boundary** —
  `contain.occurrence-target.memberkey.bareword.zero_exact_map` is support-probed across
  default/JSON/preserve/wasm and projected into `tests/matrix_supported`. Its decode-conformance row
  is deliberately vectorless/pinned because the two upstream validators disagree in both needed
  directions; `cddl-matrix/README.md` upstream oracle gap #18 records the exact outcomes. The direct
  runtime tests above, not an oracle-backed catalog vector, prove product behavior.

A **typed key domain** — anything the decode loop's own key dispatch cannot faithfully rebuild, so
everything except bare `uint`/`text`/`any` — routes the row to the seek path instead, and gets its
own layer coverage because the two paths share no emitted code:

- **Front end + guards** — the domain legs of `open_struct_map_rest_row_front_end`: the two standing
  rejections (a float-containing `K`, a `null`-admitting `K`) with their polarity fixtures, plus
  recognition for the domains the routing rule newly serves.
- **Snapshots** — `open_struct_map_typed_{default,preserve,json,wasm,wasm_json}` over
  `tests/open-struct-map-typed`, whose rules are chosen so the seek path is *observable* in the
  emitted text (a union `K`, a sidecar-bearing `bytes` `K`, a sized-int `K`, an encoding-op `K`, and
  a `@duplicates preserve` twin). It rides the wasm-parity sweep too, which is where the rest
  accessor's `MapKToV`/`PairMapKToV` class and its `<K>List` keys mint are proved generic over a
  struct-typed `K`.
- **Value-level e2e** — the typed mods of the three e2e fixtures: capture across both declared-key
  arms with a repeated DECLARED key still a `DuplicateKey`, refinement as a hard error (a float key,
  an out-of-range `.size` key both text- and uint-side, a wrong-typed value), duplicates in wire
  order under `preserve`; byte-exact interleave, a NON-minimal `bytes` key header replayed from the
  key's own sidecar, and the canonical merge sorting typed rest keys among declared ones; and on the
  JSON side both emitted image routes (nominal `K` through its CBOR bytes, primitive `K` stated
  directly) plus the imageless `bytes` row asserting both faces error loudly.
- **Extern K** — `tests/json-extern`'s rest row: a `@no_json_schema_export` extern keys a row in a
  fixture whose json-gen crate is built and run, which is the compile proof that the region's schema
  helper asks nothing of `K`.
- **Cross-crate** — `workspace_key_requests_rest_row_contract` (see the workspace-requests section
  above): the row's key demand reaches the dependency in the flavor its container needs, and all
  four wrapper classes the row names defer.

### Open struct-maps — the `@ignore` (tolerate-and-drop) flavor — test map

The [`@ignore`](../docs/docs/comment_dsl.mdx) rest-row directive drops unknown entries instead of
capturing them (no `rest` field; serialize emits declared members only; deliberately lossy;
rejected under `--preserve-encodings`). User docs: `docs/docs/comment_dsl.mdx` § "@ignore",
`docs/docs/output_format.mdx` § "The @ignore (tolerate-and-drop) flavor". Verified across the layers:

- **Front end + guards** — `robustness_tests::open_struct_map_rest_row_front_end` (the `@ignore`
  legs): the happy path (closed struct, drop binding present), the three combination rejections
  (`--preserve-encodings` / `@duplicates` / `@name`), placement-before-semantics ordering (a
  non-final `@ignore` gets the LAST-entry rejection), the never-silent misplacements (plain type
  rule, table rule, struct field, rule-position on an open struct), and the marker-slot trap (an
  `@ignore` on the `*` marker's own comment slot is NOT honored — the row stays capture, pinned loud).
- **Value-level e2e** — `tests/open-struct-map-ignore-e2e` (compiled, non-preserve,
  `--json-serde-derives`): definite and indefinite maps with extra unknown entries (incl. a
  nested-container value) decode + re-serialize declared-only, wrong-domain key errors, fixed-keys-
  win-on-content-mismatch, duplicate-unknown consumed silently while duplicate-fixed errors,
  no-unknown ≡ closed-struct bytes, and the closed-serde-struct JSON posture (declared-only write,
  unknown-key tolerance on read).
- **Snapshot** — `open_struct_map_ignore` (whole-program, non-preserve): the cip25 pair plus a
  fully-typed ignore row emit as CLOSED structs (no `rest` field, `write_map(Len::Len(N))` with the
  declared count) while deserialize stays dynamic-length and drops each unknown entry; also pins the
  deliberate-lossiness rustdoc on the type and its `serialize` fn.
- **Corpus** — `tests/corpus/dsl_ignore.cddl` isolates the directive, registered as the `dsl.ignore`
  `[[cover]]` (default/json profiles; the preserve profile is generation-skipped via
  `EXPECTED_GENERATION_FAIL` / `PROFILE_GENERATION_SKIP`).
- **Wire KATs** — the `ignore_map` rule in `tests/golden_hex` (default flags): decode-with-unknowns →
  declared-only golden bytes, the multi-unknown drop, and empty-rest identity (plain `#[test]`s, not
  `kat!`s — the drop breaks the macro's identity round-trip).
- **emit-tests** — the `emit_tests_open_struct_ignore_execute` gate (see the emit-tests section
  above): each ignore type gets an ordinary `roundtrip_<type>` with no ignore-specific gating, and
  `cargo test` runs them green (the mint goes through `new()`, so a minted value carries no unknown
  entries and byte-identity is trivial).

### Open tables (a typed row plus a catch-all) — test map

A named rule of exactly two `* k => v` rows (`t = { * K_t => V_t, * K_r => V_r }`) routed by WIRE
MAJOR TYPE (user docs: `docs/docs/output_format.mdx` § "Open tables", `docs/docs/comment_dsl.mdx`
§ "Open tables" and § `@custom_wire_major`). It lowers to a Record with zero fixed fields and two
`RestRow` members, so most of its emission is the open-struct-map rest row's, verified above; the
layers below cover what is NEW — the major dispatch, the tagged two-sequence order encoding, the
`@custom_wire_major` declaration, the flattened wasm surface, and the composition all of that has to
survive:

- **Front end + guards** — `robustness_tests::open_table_front_end`: recognition of the two-row
  shape, every SHAPE rejection the parse walk owns (>2 rows, fixed keys mixed in, inline anonymous,
  group-choice arm, plain group, `any`-keyed typed row, null-admitting key, `@ignore`, colliding row
  names, the occurrence classifier's whole grid) and every STATICNESS rejection `finalize` owns (a
  multi-major typed key, a custom-codec key with no `@custom_wire_major`, a declaration nothing
  consumes, an exhausted-complement catch-all, the bare-`text` typed key under the JSON flags), each
  with its polarity fixture.
- **CBOR e2e** — `tests/open-table-e2e` (`integration_tests::open_table_e2e`, compiled,
  preserve + canonical): the major dispatch partitioning an interleaved map, byte-exact replay across
  the tagged order encoding, the canonical merge sorting both regions into one order, per-row
  encoding sidecars at non-minimal widths, the typed-row duplicate naming its own position, the
  typed-major-but-refused hard error beside its positive control, `@duplicates preserve` on both
  rows, `@name` on each row independently, and the NonEmpty (`{+ …}`) twin's restricted
  `NonEmptyMap`/`NonEmptyPairMap` field, checked decode conversion, construction door, and public
  last-entry mutation refusal.
- **JSON e2e** — `tests/open-table-json-e2e` (`integration_tests::open_table_json_e2e`, compiled,
  `--json-serde-derives --json-schema-export`): one flattened object over both regions, the two key
  images, the typed-first read partition and the T2 rebinding carve-out, the cross-region write
  collision, the explicit duplicate-member detection, the three-attempt read failure, and the
  NonEmpty twin's staged first-entry construction plus the schema's deliberate silence about it.
- **Bounded dynamic rows** — `integration_tests::bounded_dynamic_map_rows_wasm_compile` executes
  the emitted rust and wasm round-trip modules for bounded open-struct rest, typed open-table, and
  catch-all rows (including a zero-minimum typed row keyed by a generated record) through the wasm
  boundary; `open_table_e2e` and
  `open_table_json_e2e` execute each row's below/in/above window, duplicate-preserving pair count,
  and a single registry-derived JSON-schema position oracle: standalone bounded maps retain root
  property counts while flattened open-struct/open-table partitions remain silent, serialize their
  four-property local-window values and validate them against `schema_for!(T)`, and reject
  below/above JSON directly at `serde_json::from_str` under both default and preserve JSON profiles;
  `component_glue_reenters_bounded_dynamic_map_row_doors` and
  `workspace_key_requests_rest_row_contract` pin component and cross-crate wrapper hosting.
  These are focused residents, not four generalized negative claims. The sibling scope-wide
  generated-local collision system is registry-derived, and the wasm collection-wrapper registry
  closes every rendered reference over a locally minted class/alias, declared provider, or
  dependency-owned extern source before a source map escapes.
- **Acceptance** — `tests/open-table-cip25-acceptance`
  (`integration_tests::open_table_cip25_acceptance`, compiled, preserve + canonical; extern
  definitions `tests/external_rust_raw_bytes_cip25`, hand codecs
  `tests/custom_serialization_cip25_v1`): the series' proof obligation. CIP-25 spelled as the
  consumer's end-state spec spells it — open tables at all four payload levels, alias-of-marker
  codecs keying two of them, `@custom_wire_major` steering their dispatch, a v1/v2 type choice
  discriminated by nothing but the v1 typed row's refusal — measured against a real ON-CHAIN mainnet
  golden (copied with provenance from the consumer's own pin vectors) that must round-trip byte for
  byte through all four generated levels, and against the consumer's two BUILDER-emitted noisy pin
  vectors (`NOISY_V1_HEX` / `NOISY_V2_HEX`, same provenance), which walk all six capture sites a
  CIP-25 payload has and carry duplicate keys both inside a captured metadatum map — the inline
  table's own `@duplicates preserve` — and on the details rest row itself. Plus the version
  discrimination executed in both
  directions, a float-keyed entry failing both arms, and the typed-major-but-invalid class at every
  level beside its positive control (that class is where the generated semantics deliberately
  diverge from the consumer's hand reader, so the fixture states the divergence rather than
  asserting around it).
- **wasm** — `robustness_tests::open_table_wasm_class_flattens_the_typed_row` /
  `_carries_every_row_flavor` / `_keys_list_is_named_off_the_typed_key_alias` /
  `_wasm_wrapper_ident_collisions_reject_gracefully` /
  `_catch_all_named_for_a_flattened_accessor_rejects_gracefully`, plus the `otbl__*` cells in
  `project_wasm_matrix` and the `otblrec__*` dirs in `project_multifile_matrix` (both FAST tier; the
  latter's drift gate is `project_multifile_matrix_check`).
- **Component/WIT** — `robustness_tests::open_table_component_face_projects_both_rows`.
- **Cross-crate** — `extern_import_tests::extern_import_open_table_borrows_its_typed_key_like_a_table`
  and `config_tests::a_config_workspace_settles_an_open_table_in_one_pass`.
- **emit-tests** — `robustness_tests::open_table_emit_tests_mint_both_rows`.

### Open arrays (rest tails) — test map

An open array (the array analog of the open struct-map rest row) has one occurrence-bearing segment:
it may be final, or leading/middle only before an immediate mandatory, single-item fixed suffix. A
variable window needs either the existing field-codec-free, effective, CBOR-major-disjoint boundary
(its heads are generator-proven or a transparent custom alias declares one with `@custom_wire_major`)
or generator-owned untagged finite fixed-value domains with no CDDL value in common; the latter
retries only the repeated decoder and restores the cursor when it fails. An exact window stops by
count and may share a major or have custom-/extern-owned boundary heads. This
does not prove an optional-prefix dispatch boundary: its optional/reachable-follower heads must both
be generator-proven and major-disjoint, so a custom codec or opaque extern on either side is
serialize-only unless mandatory outer tag/`.cbor` framing proves the distinction. Loose `* t` / `0* t` uses default-empty `Vec<T>`; one-or-more
`+ t` / `1* t` uses `NonEmptyVec<T>` and its first-element construction ABI; every other window uses
a complete checked `BoundedVec<T, MIN, MAX>` constructor argument. The middle boundary deliberately
honors RFC 8610 greedy non-backtracking decoding: general same-major/value-discriminator suffixes
beyond finite fixed domains need a future design rather than a guessed decoder. User docs: `docs/docs/output_format.mdx` § "Open arrays",
`docs/docs/comment_dsl.mdx` § "@ignore". It is verified across the layers:

- **Front end + guards** — `robustness_tests::open_array_front_end` recognizes final loose,
  min-one, finite, max-only, min-only, and exact-zero forms. The polarity and carrier assertions in
  `robustness_tests::occurrence_on_array_record_field_rejects_gracefully` cover leading/middle
  loose/min-one success plus exact same-major/zero success without a suffix wire-head discriminator;
  the variable-zero-minimum/non-empty/exact-zero optional-prefix distinctions; two-sided
  unproven-head optional-dispatch refusals and mandatory-framing controls; and
  declared repeated/suffix success, re-alias inheritance, emitted-major replacement, and
  declared-overlap refusal alongside overlap, optional-suffix, multi-item/plain-group-suffix,
  field-local-codec, undeclared custom-codec-owned, and opaque-extern wire-head refusals. They also retain the
  multiple/group/group-choice/fixed-value boundaries and the entry-vs-rule directive
  slot/marker-slot cases.
- **Value-level e2e** — `tests/open-array-e2e` (`integration_tests::open_array_e2e`, compiled,
  non-preserve) drives leading and middle loose, min-one, finite/max-only, exact-zero, and exact
  same-major segments, finite fixed-domain retries (including bool/null), and declared custom repeated/suffix heads through definite and indefinite
  bytes: zero/in-window/below/above windows, max-bound stop before the suffix, wrong repeated type,
  absent/wrong suffix, suffix preservation, trailing-extra rejection, and nested stream position.
  It also keeps the shared constructor and carrier door tests.
- **Preserve/canonical e2e** — `tests/open-array-preserve-e2e`
  (`integration_tests::open_array_preserve_e2e`, compiled) proves a non-canonical middle repeated
  element, exact same-major segment, and finite fixed-domain retry re-emit byte-exactly and normalize canonically without moving their suffix, beside the
  final-tail positional-sidecar, self-carried `any`, and nested-stream vectors.
- **JSON e2e** — `tests/open-array-json-e2e` (`integration_tests::open_array_json_e2e`, compiled)
  checks bounded and exact same-major middle carriers' JSON/schema bounds and constructor doors, beside the loose,
  required, and natural-fallible `any` tail surfaces.
- **Snapshots / cross-face** — `open_array_default` / `open_array_json` / `open_array_wasm` profile
  rows retain final-tail byte/API compatibility; `open_array_preserve` snapshots the middle capture
  input. The shared component build fixture wires the declared-major helper fragment and, together
  with the extern-interface self-check assertion, keeps the position-independent
  wrapper/projection seams exercised.
- **Wire KATs and emit-tests** — the final-tail `open_list` / `ignore_list` rules remain in
  `tests/golden_hex` and `tests/golden_hex_preserve`; `emit_tests_open_array_execute` keeps loose,
  restricted, and ignored capture construction ordinary. Middle `@ignore` remains loose-only and
  re-serializes the fixed members without a getter.
- **Corpus / matrix** — `tests/corpus/occurrence.cddl` contains the canonical
  `middle_occurrence = [prefix: uint, * bytes, suffix: tstr]` and count-delimited
  `exact_middle_occurrence = [prefix: uint, 2*2 uint, suffix: uint]`; the matrix bounded-array
  containment note records both boundary classes. The variable-overlap refusal is executable in
  `occurrence_on_array_record_field_rejects_gracefully`. `tests/corpus/dsl_ignore.cddl` retains the
  final-tail `@ignore` catalog/projection coverage (including its preserve rejection ledger).

### Custom (de)serializer pairs (`@custom_serialize`/`@custom_deserialize`) — test map

User doc: `docs/docs/comment_dsl.mdx` § `@custom_serialize`/`@custom_deserialize` — the honored
positions (type-level alias, named record rule, record field, whole-table rule, table key/value), the signature contracts (including
the `force_canonical` trailing argument and the by-value table-position encoding), and the
"Positions that are rejected" list (every position that parses but cannot be honored is on it —
there is no accepted-and-unhonored remainder). Hand-fn fragments
spliced into generated trees: `tests/custom_serialization` (core), `tests/custom_serialization_preserve`
(preserve, incl. the table-position pair), `tests/custom_serialization_canonical` (canonical
`force_canonical` signatures); the component whole-table smoke uses its small dedicated fragment
because it compiles the guest resource glue rather than the general fixture suite.

- **Field positions, both reps** — `struct_with_custom_serialization` (array-rep) and its map-rep
  twin `map_struct_with_custom_serialization` in BOTH `tests/core` and `tests/preserve-encodings`.
  The map-rep twin is the regression pin for the field-config carry (a dropped custom WRITER fails
  its byte-exact vector, and its reader rejects the default writer's shape, so writer/reader
  drift cannot pass as cosmetic).
- **Named record rule, direct + embedded** — `custom_record` and `custom_record_holder` in BOTH
  `tests/core` and `tests/preserve-encodings`: the pair writes text instead of the record's declared
  array, so exact direct and holder bytes plus generated-array rejection prove that both API doors
  use the same whole-record codec. `core_with_wasm` also compiles its wasm surface. The
  `single_half_custom_codec_on_record_rule_rejects_gracefully` robustness test covers the verdict
  seam that byte vectors alone cannot: a complete pair over an ambiguous record, and over a record
  containing an undecodable field, retains `Deserialize` for the record and holder plus their
  extern-interface and wasm decode surfaces, beside unannotated controls that retain the refusal.
  `a_custom_record_pair_keeps_from_cbor_bytes_despite_ambiguous_fields` separately pins the WIT and
  component-glue doors. This closes the review-found class where generated-field decode verdicts
  leaked through a complete-item custom reader.
- **Whole-table rule, one nominal owner** — `custom_table` / `custom_table_holder` in the core,
  preserve, and canonical fixtures prove direct and embedded delegation to the deliberate
  non-map wire; their vectors include malformed and duplicate polarity, while the position grid
  covers loose/preserve/non-empty/generic DSL shapes and the self-carrying declaration rejection.
  `component-custom-table` compiles the WIT/guest resource surface, its default row in
  `wasm_api_parity` differentials the generated wasm wrapper, and
  `extern_interface_projects_whole_custom_table_pair_opaque` proves opaque projection plus
  consumer re-import rather than reconstruction as a transparent map alias.
- **Named collection-rule rejection** — the `dsl_position_tests` array-rule family covers loose,
  non-empty, bounded, `@duplicates reject`, and `@duplicates preserve` collections with a complete
  pair. Each is loudly rejected because the rule remains a transparent collection typedef with no
  whole-item owner; unlike the supported whole-table rule, no complete-pair nominalization exists.
- **Table key/value positions** — `custom_table_positions` (+ `_sidecar_shape`,
  `_reject_default_shape`) in `tests/preserve-encodings`: a width sweep byte-exact through the
  custom fns in both positions, the decoded-key sidecar keying, and the reject-default-shape
  polarity pair.
- **Canonical e2e** — `tests/custom-serialize-canonical-e2e`
  (`integration_tests::custom_serialize_canonical_e2e`, compiled, rust-only): the fixture
  COMPILING is itself the regression pin for the free-function call form at both scratch-buffer
  sites (the table canonical key sort and the open-struct-map canonical merge); its vectors pin
  that the merge sorts by the bytes the write arm emits, the table-VALUE leg's `force_canonical`
  re-minimization, and a serializer refusal surfacing from both call sites.
- **Complete-pair coherence denominator** — `tests/custom-codec-coherence-e2e`
  (`integration_tests::custom_codec_coherence_e2e`, local, rust-only): one holder crate per
  explicit default, preserve, and canonical profile executes its 17 unmodified complete-pair
  carriers:
  transparent alias; array/map record fields and owners; ordinary, generic, bounded,
  duplicate-rejecting, duplicate-preserving, non-empty, and bounded/non-empty pair-map nominal
  tables; table key/value aliases; and open-map rest key/value aliases.
  Its text-over-bytes/record/map codecs are non-default, profile-correct (including inferred
  preserve encodings and canonical flags), and assert both semantic decode and byte replay through
  the holder, so a single direction reverting to a generated codec cannot pass. The row-entry
  spelling itself remains an explicit rejection control in `dsl_position_tests`. Its companion
  `custom_pair_modifier_placement_matrix` is an 18 × 2 generator-only modifier-context matrix:
  it adds the open-table typed-row dispatcher to the 17 base carriers, because that context
  necessarily carries `@custom_wire_major` and cannot be modifier-free. `@custom_encodings` must
  create the declared preserve signature/sidecar (zero-demand bases make that observable) or
  reject; `@custom_wire_major` must drive open-table dispatch or reject. The matrix caught a
  field-level major declaration that generated successfully but had no
  consumer; it is now an explicit graceful rejection. The middle-array controls additionally pin
  the declaration's second real consumer and that final/exact placements, optional lookahead, and
  mandatory generated framing remain inert.
- **Declared wire framing (`@custom_encodings`) e2e** — `tests/custom-encodings-e2e`
  (`integration_tests::custom_encodings_e2e`, compiled, rust-only, preserve + canonical; hand-fn
  fragment `tests/custom_serialization_encodings`): the declaration that lets a codec state its OWN
  wire's encoding variables instead of inheriting the replaced type's inferred demand. Every rule in
  it is one where inference is wrong — a zero-demand `bool` whose codec writes a width-carrying uint
  (the case that round-trips SILENTLY wrong without a declaration, since the codec is handed nothing
  and both directions re-minimize together), the explicit `none` assertion on the same type, a
  declaration overriding a NON-empty inference (`bytes` infers one `StringEncoding`; the wire is
  `#6.42(bytes)`), the field-level spelling, and the by-value table-key twin. The crate COMPILING is
  the three-way agreement gate (declaration vs emitted call vs emitted sidecar slot — a mismatch is
  E0308); the vectors pin the non-minimal byte-exact round trip, the recorded slot values, canonical
  minimization, and that a fresh value still goes through the custom writers. The refusal that makes
  the declaration load-bearing — a pair over a zero-demand type under `--preserve-encodings` with no
  declaration — is a `dsl_position_tests` cell beside its without-the-flag control.
- **Alias of a MARKER rule e2e** — `tests/alias-of-marker-e2e`
  (`integration_tests::alias_of_marker_e2e`, compiled, rust-only, preserve + canonical; hand-fn
  fragment `tests/custom_serialization_alias_of_marker`, extern definition
  `tests/external_rust_raw_bytes_policy_id`): the pair on an alias whose BODY references a
  `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule — the "this rule IS that type, written differently on the
  wire" spelling, which escapes the on-the-marker rejection by being the general type-level
  override applied to a type the crate does not define. The static half asserts the composition (the
  alias resolves to the marker's type; the marker's inferred one-`StringEncoding` demand is what
  gives the custom TEXT header a sidecar slot, keyed by the DECODED key); the vectors execute a
  byte-exact non-minimal-header round trip at all three positions the alias reaches (record field,
  table key, table value), canonical ordering keyed on the CUSTOM-WRITTEN bytes (the wire's two keys
  sort oppositely by their decoded bytes, so a sort keyed on the wrong thing fails the vector), a
  sidecar mutation replayed under the decoded key, and the custom reader's refinement rejections
  (non-hex text, wrong major).
- **One codec across two positions → loud compile failure** — `tests/custom-pair-shared-codec`
  (`integration_tests::custom_pair_shared_codec_across_positions_fails_to_compile`, rust-only): the
  deliberately-uncompilable twin of the fixture above. One pair reached from a record field AND a
  table key, with a single hand-written codec carrying the record-field signature: generation
  succeeds (the tool cannot see the hand signature) and the generated crate fails `cargo build` with
  an E0308 naming `&StringEncoding` against `StringEncoding`. What it pins is that the documented
  two-functions rule fails LOUDLY rather than as a silently different wire.
- **Placement axis** — `dsl_position_tests` cells: the rejected placements (extern/raw-bytes
  rules, row-entry slots, `@no_alias`, `@newtype`, enum rules in every spelling, record-rule
  single-half, named collection rule in every spelling incl. a generic def's instance) as
  `Expect::Reject` beside the honored controls. A complete named-table pair is the contrasting
  accepted control, including generic, preserve-pair-map and non-empty forms; it asserts a nominal
  wrapper, thin calls in both directions and no structural map write. The record-rule BOTH-set
  spelling is the other accepted-behavior control; the alias-of-marker spelling's honored cells
  (raw-bytes flavor at type level and at
  a table key domain, under `--preserve-encodings`) sit beside the marker-rule rejections that are
  their placement controls. Message texts are
  pinned by the `custom_codec`/`single_half_custom_codec` robustness tests.
- **Oracle exclusion** — the emit-tests encoding-fidelity oracle excludes custom-carrying types
  rep-independently (their wire format isn't the generated serializer's — see the emit-tests
  section below), so the fixtures above are the fidelity evidence for these positions.

### Per-rule duplicates policy (`@duplicates`) — test map

The **`@duplicates reject` flavor** (set/array collections — user doc:
`docs/docs/output_format.mdx` § "Reject-duplicates containers", `docs/docs/comment_dsl.mdx`
§ `@duplicates`) uses `OrderedSet`/`NonEmptyOrderedSet` for loose/min-one windows and
`BoundedOrderedSet` for every other bounded window, verified across the same layers as the idiom
plus the cross-crate ones:

- **Corpus fixture + compile** — `tests/corpus/tag_set_reject.cddl` (five reject shapes + a `holder`
  embedding them incl. an optional field: `int_set` `[*]` idiom, `int_neset` `[+]` idiom, `text_set`,
  `oset_u64` a named generic-instance of a reject generic def, `plain` a non-idiom array) drives the
  `feature_corpus` snapshots and `feature_corpus_compiles`' three-profile compile. Its sibling
  `tests/corpus/tag_set_alias_instance.cddl` pins the ALIAS-of-instantiation shape
  (`required_signers = nonempty_set<...>` with a second anonymous use — the CML regen shape): the
  `pub type` alias to the minted nominal, its resolved-policy self-doc, the opaque
  extern-interface row, and the `typescript_custom_section` TS alias line; its decode-conformance
  rows ride the same catalog as `tag_set_reject`'s below. In-process polarity/seam pins:
  `optional_tag_set_tests::alias_binding_set_nominal_documents_resolved_reject_policy` (doc says
  `NonEmptyOrderedSet` + reject, never the inverted `NonEmptyVec`/preserve texts),
  `snapshot_tests::extern_interface_projects_alias_to_set_nominal_as_opaque`, and
  `optional_tag_set_tests::alias_binding_set_nominal_wasm_surface_flattens_and_names_the_rekey`
  (the flat wasm nominal surface + the JS re-key naming).
- **Preserve-mode floor + reject KATs** — `tests/golden_hex_preserve/tests.rs`: the duplicate-carrying
  `opt_set_{untagged,tagged}_duplicate` / `opt_neset_{untagged,tagged}_duplicate` KATs pin that the
  DEFAULT (`preserve`) accepts and re-emits duplicates byte-exactly (the regression floor `reject`
  narrows from), while `reject_set_untagged` / `reject_set_tagged_wide` pin a duplicate-FREE reject set
  round-tripping byte-exactly and the in-process `reject_set_duplicate_wire_and_api_identical` pins the
  wire door and the API door reporting the same `DuplicateKey`. The std set contract on the runtime
  twins (`insert -> bool`, `contains`, keep-first `Extend`/`FromIterator`, `sort`, `try_opt_from`,
  the `OrderedSet` ↔ `NonEmptyOrderedSet` refinement doors) and the set nominals' emitted
  `try_opt_from` are covered e2e by `reject_set_std_contract_and_refinement_doors` in the same
  suite.
- **Decode-conformance (composition depth)** — the `tests/decode_conformance/corpus_catalog.toml` rows
  `tag_set_reject.{holder,int_set,int_neset,oset,oset_u64,plain,text_set}` (duplicate-free spec-derived
  vectors the generated decoder must accept), replayed by the `corpus_decode_replay` gate.
- **Extern-interface projection** —
  `src/tests/extern_import_tests.rs::extern_import_projects_duplicates_reject_no_cross_crate_skew`: the
  `dep-reject/lib.cddl` → `consumer-reject/lib.cddl` two-crate fixture proves the directive travels on
  the export so the consumer rebuilds the reject door (not a preserve-mode `Vec` that would accept what
  the dep rejects), with a negative-control skew check.
- **Workspace-requests hosting** —
  `src/tests/integration_tests.rs::workspace_requests_hosts_reject_ordered_set_twins` pins the
  hand-authored loose/non-empty sidecar, while
  `src/tests/integration_tests.rs::workspace_dep_defers_reject_ordered_set_twins` drives the real
  consumer export/import/request loop and makes the dependency host `IdxFooOrderedSet` /
  `NonEmptyIdxFooOrderedSet` plus bounded `IdxFooBoundedOrderedSetMin2Max3` /
  `IdxBarBoundedOrderedSetMax4` classes in `requested_collections.rs`.
- **Bounded mint boundary** —
  `robustness_tests::emit_tests_mint_all_bounded_reject_occurrence_windows` keeps `0*0`, `?`,
  max-only, and min-only emitted-test mints on the `BoundedOrderedSet` checked door for both Rust
  and wasm output. (Pedantic `1*1` is a fixed array/group spelling rather than a homogeneous
  collection eligible for `@duplicates`.)
- **Preserve-encodings byte-fuzzer leg** — `tests/preserve-encodings/input.cddl`'s `oset_p<a>` generic
  reject collapsed set (used by `reject_set_preserve`), regenerated by `fuzz/generate.sh` into the
  `from_cbor_bytes` fixture — so the optionally-tagged deserialize peek path and the uniqueness
  reject door are both in the probe set that `fuzz_compile_rot` compiles and `fuzz_bounded_run`
  actually walks (`fuzz/README.md` § "Two layers: a gated smoke-walk, an unbounded manual run").
- **Graceful-rejection matrix** — `src/tests/robustness_tests.rs`:
  `duplicates_directive_rejects_gracefully` (permanent no-policy placements) and
  `duplicates_directive_accepts_live_and_default_noops` (live set/array `reject` plus the accepted
  default no-ops).

The table-side explicit `reject` default has a separate component regression floor because it
changes no rust representation and therefore cannot ride the set fixtures above:
`tests/corpus/component_reject_table.cddl` embeds both a named and an anonymous-inline reject table.
Its IR snapshot proves the inline spelling retains `Some(Reject)` rather than passing by silently
dropping the directive; its component snapshot proves both maps collect directly and leave the
constructor infallible; and
`component_reject_tables_stay_plain_maps_while_reject_sets_reenter_try_from` keeps a named reject
set beside them as the positive fallible control. The local representative wasip2 smoke compiles
this fixture, while full-tier `component_corpus_compiles` owns the same claim at corpus breadth.

The **`@duplicates preserve` flavor** (tables — user doc:
`docs/docs/output_format.mdx` § "Preserve-duplicates tables", `docs/docs/current_capacities.mdx`
§ "Preserve-mode tables", `docs/docs/wasm_differences.mdx` § "Preserve-duplicates tables",
`docs/docs/comment_dsl.mdx` § `@duplicates`) is the TABLE mirror of the reject flavor: a table rule
carrying `@duplicates preserve` swaps its transparent alias to the `Vec<(K, V)>`-backed pair-map twin
(`{*}` → `PairMap`, `{+}` → `NonEmptyPairMap`, every other homogeneous window → checked
`BoundedPairMap`), the only shape faithful to both entry order and duplicate keys (driver: byte-exact
round-trip of pre-Conway Cardano `transaction_metadata`). Verified
across the same layers:

- **Wire bytes (byte-exact dup round-trip)** — `tests/golden_hex_preserve/tests.rs`:
  `pmap_duplicate_key` and `pmap_duplicate_key_nonminimal_head` (a duplicate-keyed map decodes AND
  re-emits byte-exactly, the second proving per-entry POSITIONAL encoding — a non-minimal head on one
  entry re-emits faithfully), plus the in-process `pair_map_surface_and_nonempty_door` (the pair-map
  read surface — `get` first-match / `get_all` — and the `{+}` NonEmptyPairMap min-1 door). The
  headline keys its metadatum map by `tstr` (recursion in the map VALUE); the recursive union-KEYED
  spelling (`{* md => md}`) is held instead by the `upres_` block of
  `tests/recursive-collection-ref/input.cddl`, which carries the same directive on both profiles.
- **Canonical stable-sort** — `tests/golden_hex_canonical/tests.rs`: `canon_dup_pmap_key_sort` and
  `canon_dup_pmap_nonminimal_head` pin that `--canonical-form` stable-sorts entries by encoded key
  bytes with duplicates adjacent in first-appearance order and minimizes per-entry heads independently
  (the positional sidecar is what lets same-key entries canonicalize separately) — the deterministic
  best-effort for data with no RFC 8949 canonical form.
- **JSON (array-of-pairs)** — `tests/json/tests.rs`: `preserve_pair_map_json` (a preserve table
  serializes as a JSON ARRAY of `[k, v]` pairs — order and duplicates intact — not an object),
  `ne_preserve_pair_map_json_door` (the `{+}` door refuses an empty `[]` with the same min-1 error),
  and `schemas_reject_wrong_shapes` (the emitted `schemars` schema is an array-of-pairs that REJECTS
  an object shape for the field).
- **Core wasm (appending insert)** — `tests/core/tests_wasm.rs::wasm_preserve_pair_map_insert_appends`:
  the pair-map wasm wrapper's `insert` APPENDS (a repeated key grows `len`, never replaces) and returns
  `Option`, the opposite of the reject set's fallible `add`. The emit-tests PairMap synthesis leg
  round-trips it.
- **Robustness pins** — `src/tests/robustness_tests.rs`:
  `duplicates_directive_accepts_live_and_default_noops` (the core lowering: `{*}` → `PairMap`, `{+}` →
  `NonEmptyPairMap`, alongside the live set/array `reject` and the accepted default no-ops),
  `duplicates_preserve_nonempty_table_lowers_to_twin_under_wasm` (the `{+}` NonEmptyPairMap wrapper
  crosses the wasm boundary over the flavored `PairMapU64ToText` loose source),
  `generic_preserve_table_instance_lowers_to_pair_map_under_wasm` (an anonymous generic table
  instance takes the pair-map flavor from its alias base type's own carried policy),
  `mixed_policy_map_shapes_mint_distinct_flavored_wasm_wrappers` (the flavored-name payoff: a
  preserve and a non-preserve rest row of the identical `K`/`V` mint two distinct classes, each with
  its own `From` and getter — before the flavored names this emitted one class and a wasm crate that
  failed `cargo check` with E0277), and the two rule-ident-vs-wrapper-ident siblings
  `preserve_pair_map_loose_wrapper_ident_collision_rejects_gracefully` /
  `preserve_pair_map_non_empty_wrapper_ident_collision_rejects_gracefully` (a user rule spelling
  `PairMapKToV` / `NonEmptyPairMapKToV` is a distinctly-worded graceful rejection).
- **Extern-interface projection** —
  `src/tests/extern_import_tests.rs::extern_import_projects_duplicates_preserve_no_cross_crate_skew`:
  the `dep-preserve/lib.cddl` → `consumer-preserve/lib.cddl` two-crate fixture proves the directive
  travels on the export so the consumer rebuilds the pair-map twins (not a reject-default `BTreeMap`
  that would REJECT the duplicate keys the dep preserves — the mirror skew), with a negative-control
  skew check.
- **Matrix feature rows + decode conformance** — `dsl.duplicates.{reject,preserve}` are registered
  vendor feature rows (flavored siblings, no bare row — the bare directive panics on its missing
  argument; recipe notes: `cddl-matrix/README.md` § "Registering a new vendor (CDDL_CODEGEN)
  feature row"), with minted decode-foreign catalog rows and the `table_preserve.cddl` corpus
  fixture's minted corpus decode rows.
- **wasm-ABI + multifile matrix grid rows (both flavors)** — the per-role grid layer on top of the
  per-fixture pins above: the reject twins as `rset`/`nerset`/`rseta`/`nerseta`, plus bounded
  reject `brset`/`brseta`, and the preserve pair-map twins as `pmap`/`nepmap`/`pmapa`/`nepmapa` in
  `project_wasm_matrix.ts`'s `SHAPES`
  (named-rule + anonymous-instance flavors, each × all 8 boundary roles, compile floor +
  three-profile round-trips) and the same shapes' eligible subset × reference modes in the
  multifile placement matrix. Enumerating them found + fixed the `[*]`-reject wasm-boundary conversion gap (E0308,
  pinned by `newtype_over_plain_reject_ordered_set_converts_wasm_boundary`) and the cross-module
  restricted-wrapper placement class — every collection occurrence resolves its wasm wrapper name +
  home scope through `wasm_collection_wrapper`, and a field referencing a named/dep-owned collection
  rule keeps only the rule ident (the `Alias` arm's structural-wrapper suppression).

### Decode-direction conformance (`tests/decode_conformance/` — accept what the spec accepts)

The fourth gate direction. The three above are all blind to an **over-strict decoder**: round-trips
only decode what they themselves encoded, the conformance oracles validate *our emitted* bytes
(encode side), and the reject tests check that spec-INVALID input is refused. A generated decoder
that rejects spec-VALID CBOR passes all of them — proven twice on this layer's first sweep (below).
This layer feeds SPEC-DERIVED CBOR instances our code did *not* produce into the generated decoders
and asserts they are accepted. Two mechanically-projected obligation sets drive it, each with its own
committed catalog under `tests/decode_conformance/`: the matrix's per-construct BREADTH (the bullets
below) and the corpus fixtures' composition DEPTH (§ "Composition-depth (corpus) leg").

- **The committed corpus** — `tests/decode_conformance/catalog.toml`, machine-produced (same
  artifact class as `cddl-matrix/matrix.json`). The obligation set is PROJECTED from the matrix's
  `supported` rows (features + containment cells + control-ops), never hand-curated: every
  supported row carries ≥1 committed vector or a mechanical `pinned_reason` (no silent skips).
  Vectors are minted by the ruby `cddl` gem's instance generator (`cddl <spec> generate` →
  `diag2cbor.rb`) and committed only after validating against BOTH oracles — ruby `cddl validate`
  AND the rust CLI as `cddl --ci validate` (without `--ci` the rust CLI prints the error but exits
  0; a mint-time negative control feeds both oracles a known-bad instance so that trap can't
  silently vacate the cross-check). Contested vectors are dropped, never committed, except an exact
  `<row>/<hex>` resident in the separate matrix or corpus accept-oracle-gap ledger in
  `cddl-matrix/lib.ts`: it remains certifiable only while every non-exempt oracle accepts and each
  named failing oracle still has its recorded nonzero exit and diagnostic signature. The mint owns
  that live-oracle stale guard; an oracle that starts accepting or drifts in exit/signature retains
  the vector in reviewed output but makes the mint exit 1. `project_decode_conformance.ts` owns the
  catalog-side twin: each map's key must still name an ordinary spec-valid accept in that map's own
  catalog (never its sibling's). A rule with no
  standalone decode surface (transparent alias / named table / c-enum — no nominal
  `impl Deserialize`) is minted in **holder mode**: vectors wrap the rule in
  `__probe_holder = [0, <rule>]` (prepended FIRST — both oracles root validation at a spec's first
  rule) so decoding routes through the *generated* field-decode path rather than cbor_event's
  blanket impls. The catalog has a consumer beyond this layer: the matrix's component-execution
  leg draws each selected row's `spec`/`type_name`/vectors from the SAME committed rows (the
  annotations-table row in `cddl-matrix/README.md` describes the leg), so a re-mint that pins a
  selected row, changes its `type_name`, or drops its last spec-valid accept vector fails
  `verify.ts`'s startup selection self-test loudly rather than silently shrinking that leg.
- **Refresh flow** — `cd cddl-matrix && bun run verify.ts --mint-decode-foreign` (or
  `--only=<id,…>` to re-mint a subset, preserving the rest byte-identically; an `--only` id that
  has LEFT the supported set but still has a committed row is DROPPED — the support-boundary
  removal flow, e.g. a construct newly rejected at generation — while an id that is neither
  supported nor an existing row still hard-fails as a typo). The mint phase is
  mint-ONLY (writes the catalog, never annotations) and takes "supported" from the committed
  `matrix.json` — so a row whose verdict just flipped needs the plain probe run and a
  `build_matrix.ts` fold BEFORE it can mint; symmetrically, a plain probe run AFTER the mint (plus
  another fold) refreshes the row's decode-foreign evidence clause, which otherwise still reads
  "no committed decode vectors" from the pre-mint probe. Generation is
  randomized, so verdict stability comes from the COMMIT: the deterministic gates below replay
  committed bytes only. Before either full foreign or corpus refresh writes, the mint compares each
  shared row's randomized, spec-valid `source = "ruby-generate"`, `expect = "accept"` structural
  classes with its committed predecessor. The bounded classifier retains nested array/map shape and
  empty/singleton/multi cardinality (while ignoring scalar payload values); losing an old class
  refuses the mint and reports the row plus missing and proposed classes in deterministic order.
  `--only` deliberately bypasses that full-refresh loss check so it remains the review/recovery route
  for restoring the specific row's diversity, while preserving every unselected row byte-identically.
  `project_decode_conformance.ts` also compares each supported row's committed
  evidence clause with the catalog's spec-valid accept-vector count, excluding
  `class="over-acceptance"`; this catches the proven scoped-mint-after-probe drift where
  `record_array_tagged` minted vectors while its evidence still claimed none. A spec-valid vector the
  decoder rejects is normally written as a **class-less `expect = "reject"` pin and the mint exits 1**;
  the drift gate stays red until a human triages it into `class = "bug"` (ledger it in
  `cddl-matrix/roadmap.toml` § findings) or `class = "limitation"` (cite `current_capacities.mdx` / the
  overlay note). The sole narrow exception deduplicates an already-proven policy rejection: the mint
  omits a generated candidate only when its marked generated-error Display matches an existing
  same-row `class = "policy-rejected"` pin's `expect_err` (that pin is hand-authored and already
  both-oracle-validated). An unmatched rejection reason remains class-less and RED for triage.
  `source = "hand"`
  supplement vectors survive re-mints and are re-validated like any candidate. The catalogs are
  **writer-canonical** (machine-rewritten wholesale: no in-row `#` comments, each row's vectors in
  ascending hex order) — a hand-authored corpus row in any other form fails the
  `project_decode_conformance.ts` drift gate, and a hand comment in either file is dropped by the
  next mint's rewrite — so put per-vector rationale in the surrounding docs/test map, not in the
  TOML, and let vector order fall where the sort puts it.
- **Reject vectors split by class** — three distinct contracts live under `expect="reject"`:
  - `class = "bug" | "limitation"` — spec-VALID CBOR the decoder WRONGLY rejects (the wrong-rejection
    pins above). Re-validated **spec-VALID** (both oracles accept) at each mint; PRUNED when the gap
    closes.
  - `class = "constraint"` — spec-INVALID CBOR (`source = "hand"`) that VIOLATES a constraint the row
    enforces (an over/under-`.size` string, a numeric-op boundary violation like `11` against
    `int .le 10`, a non-uint `.cbor` payload, a cut-violating map value) and that the generated
    decoder must **durably reject**. Re-validated **spec-INVALID** (both oracles reject — the
    inverse gate) at each mint; NEVER pruned. Two hand-authored fields pin the rejection's identity:
    `reason` names the violated constraint (prose, for humans), and `expect_err` is a substring the
    generated decoder's error Display must contain — the replay gate asserts it, so a decoder that
    rejects for a subtly WRONG reason (a stray length check, an unrelated error path) fails the gate
    instead of passing as it would under a bare `is_err` check. The drift gate REQUIRES `expect_err`
    on `class="constraint"` and `class="policy-rejected"`, and forbids it elsewhere; a mint
    round-trips both fields verbatim.
    Authoring `reason`: describe the violating instance hex-free (decoded values plus what was
    mutated — "`[false, 68]` with only the constant byte flipped"), never by citing a sibling
    ACCEPT vector's hex — `--only` re-mints regenerate the ruby accepts (the reference generator
    draws fresh random instances per run), so a cited byte string silently stops matching any
    committed vector while the prose keeps reading as if it did.
    Authoring `expect_err`: pick a generous discriminating fragment of the generator-emitted Display
    including the bound and the vector's own found value (both deterministic — same bytes, same
    decoder), e.g. `11 not at most 10`, `not in float range (>=0.5, <=10.5)` — formats in
    `static/error.rs`; if the captured Display does NOT name the violated constraint, that is a
    wrong-reason rejection to investigate, never a string to pin. Capture the fragment from the
    DEFAULT profile, which is the leg the replay gate asserts it on — a rejection's SPELLING can
    differ by profile even when its cause does not. The float value-class rows report through
    `DeserializeFailure::FloatWidth` in BOTH profiles (`Expected a float32 value, found a float16
    value`): reads go through `read_float_width`/`read_float_sz_width` for every one of the five
    constrained names, and neither cbor_event blanket impl is reachable for them (the crate's `f32`
    impl asks whether the value is binary32-representable, the NESTED reading, which is not the
    partition our classes implement). This class is Q4's
    `enforce = yes (bounded-reject)` evidence (`query_q4_directional.ts` counts `class="constraint"`
    only). NOTE: the numeric range/eq rows carry these vectors only because their probe examples
    target `int` with literal, non-vacuous bounds — the rust corroborating oracle (`cddl` 0.10.x)
    does not enforce these ops over a `uint` target, so a `uint`-targeted form can't pass the both-reject
    gate; `query_q4_directional.ts --check` pins the exact green set against such a decay. The
    `rangeop` rows with non-uint endpoints (`.int`/`.nint`/`.float`) sat on a SECOND rust-oracle gap:
    released 0.10.x `validate` blanket-rejects every instance
    of a float or negative-int range); the fork's `885c61c` fix closed it, so those rows carry real
    accept vectors and discriminating rust reject corroboration (the float vectors' `reason` records
    the provenance).
    **Authoring rule — vector SHAPE is load-bearing:** a constraint vector for a `standalone` row is
    a BARE in-type instance of the row's type (`0b`, `fb…`), decodable up to the constraint itself so
    the emitted range/size check is the only possible rejection. A holder-wrapped scalar
    (`8200…` = `[0, x]`) against a standalone row rejects as a TYPE mismatch before any bounds check
    runs — the reason assert would catch that behaviorally (the TYPE-mismatch Display doesn't contain
    the range/size fragment), but `project_decode_conformance.ts` § 6 also bans it STATICALLY at the
    cheap drift-gate tier. The `8200` holder prefix belongs only to `mode = "holder"` rows; a row's
    accept and reject vectors must share their outer CBOR shape. § 6 enforces this mechanically
    (leading major-type class vs the row's accepts, majors 0/1 merged; the holder preamble banned on
    accept-less standalone rows).
  - `class = "policy-rejected"` — hand-authored, spec-VALID CBOR (`source = "hand"`) that BOTH
    reference oracles accept but the generated decoder must durably reject under a documented library
    narrowing policy. `reason` names that policy for humans; `expect_err` pins the generated error
    Display's durable reason. These vectors are re-validated spec-VALID at mint, never become Q4
    authored-CDDL enforcement evidence, and never enter accept-derived replay legs; under preserve they
    still reject, without a byte-identity claim.
- **The vector-class 2×2 (current decoder behavior × spec validity).** `expect` always pins CURRENT
  behavior (what the replay asserts); `class` carries the spec-validity/triage label:

  | | spec-VALID bytes | spec-INVALID bytes |
  |---|---|---|
  | decoder **accepts** | plain `expect="accept"` (no class) | `expect="accept"` + `class="over-acceptance"` |
  | decoder **rejects** | `expect="reject"` + `class="bug"\|"limitation"`, or intentional hand `class="policy-rejected"` (+ `reason`, `expect_err`) | `expect="reject"` + `class="constraint"` (+ `expect_err`) |

  `policy-rejected` deliberately shares the spec-VALID/reject cell with bug/limitation, but is not a
  decoder gap: both reference oracles accept it and the library's documented policy intentionally
  rejects it. That is why it is hand-authored and durable rather than auto-pruned, while never counting
  as Q4 enforcement or accept-derived evidence.

  The fourth cell is `class="over-acceptance"` — certified-spec-INVALID CBOR (both oracles reject at
  mint, the same inverse gate as `class="constraint"`) that the generated decoder CURRENTLY (wrongly)
  ACCEPTS: a known silent-acceptance bug with no enforcing fix yet. It is `source="hand"`, requires a
  `reason` (citing the ledgered finding + the promotion flow), is FORBIDDEN `expect_err`, survives
  re-mints VERBATIM, and is re-validated spec-INVALID at each mint (never pruned mechanically). The
  replay gate asserts it STILL decodes Ok ("still wrongly accepts"), so when a fix lands the pin flips
  LOUDLY — the signal to PROMOTE it to `class="constraint"` (+ `expect_err`) and flip the row's Q4
  projection green (the `KNOWN_SILENT_DROP` / `EXPECTED_COMPILE_FAIL` pattern applied to decode). Q4
  projects a carrying row as the honest `enforce = no (over-accepts: M)` (dominating `yes`/`unverified`;
  pinned by `query_q4_directional.ts --check`'s `EXPECTED_ENFORCE_OVERACCEPTS`). A spec-INVALID accept
  vector NEVER counts as spec-valid decode evidence: it is excluded from the verify.ts decode-foreign
  corroboration count, from Q4's foreign-decode count, and from the replay gate's encoding-variant /
  header-mutation / preserve legs. Zero instances at HEAD — the class stays armed for the next
  certified instance. Its historical residents were the widened-occurrence-marker table rows
  `contain.occurrence-target.memberkey.type1.{plus,optional,bounded}_table` and the omitted-window
  `contain.map-key.memberkey.type1.tstr_arrow_nooccur`: a single non-literal arrow table once
  widened every occurrence spelling to the loose 0..N map. That widening is retired. `+`/`1*` now
  uses `NonEmptyMap`; every other unique-key window, including omitted exact-once and `?`/`n*m`, uses
  checked `BoundedMap`; their catalog boundary vectors are `class="constraint"` enforcement pins.
  Bounded `@duplicates preserve` tables use `BoundedPairMap` and their catalog boundary vectors are
  `class="constraint"` enforcement pins. (`8200a0` also remains the seeded-control *accept* on `type2.map` —
  `{ * tstr => int }`, a spec-VALID empty table there.)
- **The replay gate** — `integration_tests::decode_conformance_replay` (`#[ignore]`d, check.ts
  `full` tier, ~6 min): per active row it generates a crate from the committed `spec` and `cargo
  test`s it under two profiles (default + preserve), plus a third json/wasm-surface generation
  (§ "json/wasm surface legs" below). Oracle-free and deterministic — the bytes were spec-cross-validated
  at mint time, so the gate replays commitments, never re-derives them. Three assertion legs run on
  the DEFAULT-profile build, sharing one failure-attribution grammar. Shared across every leg body
  that captures an error Display (the constraint/policy and header-mutant Err arms, both profiles): an
  emitted helper asserts the displayed location chain has no adjacent-duplicate segment — a doubled
  location ("Foo.Foo", the generator double-annotation class) *satisfies* a bare `failed in {name}`
  contains, so without this check the location asserts below cannot see it. Justified exceptions go
  in the stale-guarded, empty-at-HEAD `DOUBLED_LOCATION_SKIP` ledger; the helper ships an emitted
  self-check per replayed crate, counted by the per-crate completeness check so it can't silently
  vanish. The legs:
  - *Base replay* — every accept vector decodes Ok and every reject pin still Errs (**a pin that
    starts decoding green FAILS the gate** — a re-bless can't silently launder a bug). Each
    `class="constraint"` or `class="policy-rejected"` vector additionally asserts the error Display
    CONTAINS the catalog's `expect_err`, pinning the rejection REASON — a wrong-reason rejection fails
    the gate with the captured Display. The four unique-key bounded-table boundary pins additionally
    have a cheap runtime↔catalog lockstep test in `bounded_map_runtime_tests`, so a `BoundedMap`
    diagnostic drift fails before the multi-minute replay (a vacuity floor keeps ≥ 40 constraint
    reason asserts live, with policy pins counted separately). A `class="over-acceptance"`
    vector emits its own `over_accept_N` test asserting the decoder STILL (wrongly) decodes it Ok; a
    rejection is the pin FLIP (the fix landed), attributed by `classify_over_acceptance_failure` with a
    marker naming the promotion flow, and a completeness guard asserts the emitted `over_accept_*` count
    equals the catalog's over-acceptance vector count. These vectors are excluded from the two legs
    below (spec-invalid bytes evidence nothing about the spec's shape).
  - *Encoding-variant leg* — each accept vector is replayed through mechanically-derived spec-EQUAL
    re-encodings (the shipped `cddl_encoding_fidelity::variants` mutator, reused harness-side:
    indefinite framing, non-minimal int/len widths, chunked strings, reversed maps): a re-encoding
    the decoder REJECTS (over-strict, the motivating class) or mis-decodes to a different value
    fails the gate. `ENCODING_VARIANT_SKIP` (stale-guarded, empty at HEAD) ledgers any
    (row, label) that legitimately fails against a `cddl-matrix/roadmap.toml` finding — a claim about
    the DECODER, which stays hand-reviewed; a variant-test vacuity floor keeps the leg live. If its
    stale guard stops reproducing a listed entry, remove the pin only after confirming the decoder gap
    closed; if randomized committed vectors no longer exercise the variant, restore the structural
    diversity with a scoped `--only` re-mint and keep the live pin.
    A second exemption class is DERIVED rather than listed: the two map-reordering labels
    (`reverse_maps`, `everything`) assume entry order is an encoding detail, which an
    `@duplicates preserve` pair-map's contract makes false — its order is part of the value, so the
    reordered vector is a genuinely DIFFERENT value. `encoding_variant_skip_kind` reads that off the
    row's own `spec` (the directive in a line's comment half, a `=>` in its code half), so a new
    preserve row owes no ledger entries and a re-minted vector cannot make a live exemption look
    stale. It is one-way and narrow on purpose: `@duplicates preserve` on a SET derives nothing
    (reordering a map is the identity on an array, so a failure there would be a real finding), and
    a row that only mentions the directive in prose derives nothing — which is what keeps the
    loose-container control rows (`open_table.open_table` and its `{+ …}` twin) replaying their
    reordering variants beside the pair-map rows. Derived skips are recorded in the run output and
    carry NO stale guard, because a derived suppression that never fires is correct (a vector whose
    reordering is the identity — an empty or single-entry map).
  - *Header-mutation leg* — each accept vector also derives spec-INVALID reject mutants
    (`header_mutants`, pure byte transforms of the item-under-test's leading CBOR head; holder rows
    mutate past the `82 00` = `[0, _]` preamble): `wrong_major` flips the major type, `trunc_head`
    re-encodes the head with an 8-byte argument then drops its final byte (ill-formed by
    construction). A `wrong_major` flip landing on a major the row's own accept vectors evidence
    (majors 0/1 merged, the drift gate's § 6 merge) is skipped at DERIVATION time: such a mutant is
    ambiguous (possibly spec-valid — `type.choice`'s bstr↔tstr flip lands on the other
    `uint / tstr / bytes` arm), and skipping only the ambiguous flips keeps the row's non-ambiguous
    mutants live where a (row, label)-wide ledger entry would swallow a future over-acceptance.
    Each emitted mutant must be REJECTED **and** the error Display must carry a location naming the
    decoding type (`failed in {type_name}` — the annotation analogue of the base leg's
    `expect_err`, at catalog breadth rather than the fixture-granularity `error_annotation_*`
    tests; a bare `type_name` contains is deliberately NOT used, since single-letter type names
    like `T` would vacuously match "TagMismatch"). Two stale-guarded ledgers hold the honest
    exceptions: `HEADER_MUTANT_ACCEPT_SKIP` — a mutant the row's spec genuinely accepts WITHOUT any
    accept vector evidencing that major (an `any`-typed row, an unsampled choice arm; one resident
    at HEAD: `(prelude.any, wrong_major)`, since `x = any` accepts every major by definition;
    `trunc_head` can never be here, asserted as a hard error) — and
    `HEADER_MUTANT_LOCATION_SKIP` — a rejection carrying no location: EMPTY at HEAD now that
    embedded/plain-group header scaffolding and newtype-wrapper container reads are annotated, with
    the locationless `from_cbor_bytes` `TrailingData` path the only known-legitimate resident (no
    header mutant reaches it here, so the ledger stays empty). A header-mutant vacuity floor keeps
    the leg live.
  - *Failure attribution* — a FAILED replay test's cause is attributed by pure
    marker-classification functions (`classify_constraint_failure` / `classify_policy_failure` / `classify_variant_failure` /
    `classify_header_mutant_failure` / `classify_over_acceptance_failure`) whose needles own the
    trailing ':' that disambiguates prefix-colliding libtest names (`reject_1` vs `reject_10`,
    `over_accept_1` vs `over_accept_10`); that grammar is pinned unit-side (no crate build) by
    `integration_tests::classify_constraint_failure_disambiguates_prefix_colliding_names` and its
    variant/header-mutant/over-acceptance siblings
    (`classify_variant_failure_owns_the_delimiter_and_maps_each_marker`,
    `classify_header_mutant_failure_disambiguates_prefix_colliding_names`,
    `classify_over_acceptance_failure_disambiguates_prefix_colliding_names`); the header mutator
    itself is pinned by `header_mutants_pin_hand_derived_bytes`.

  Finally it regenerates under `--preserve-encodings=true`: spec-valid accept vectors decode AND
  re-encode **byte-identically** (the preserve contract is itself decode-direction evidence), while
  `class="policy-rejected"` vectors must still reject (without a byte-identity claim). This is the
  deliberate three-way reject split: `bug`/`limitation` are wrong rejections that may be unpinned when
  fixed; `constraint` is spec-invalid authored-CDDL enforcement; policy-rejected is spec-valid input
  intentionally narrowed by a documented library policy and must never enter Q4 enforcement.
  `PRESERVE_SKIP` (stale-guarded) currently carries only the BY-DESIGN rejection class
  (`dsl.ignore` — `@ignore` under `--preserve-encodings` is a contract rejection, not a gap, so its
  stale-entry guard is a regression tripwire: that leg starting to generate is the finding). The
  former tag-over-a-type-choice gap is supported and no longer ledgered. Anything new there is a
  finding either way. The same stale guard also
  requires every listed id to name an ACTIVE (vectored) row, so a skip entry cannot be pre-landed
  against a still-PINNED one — it fails the gate outright. A row whose preserve leg is a by-design
  rejection therefore lands ACTIVE, with a hand-derived accept vector, so its skip entry is valid in
  the same commit that adds the row rather than becoming due at activation. It stays a hand list on
  purpose, for two reasons that outlive any single entry. WHICH class an entry belongs to is what its
  stale guard means — a gap closing versus a contract regressing — and the matrix's
  `emission.preserve` verdict is one boolean that cannot carry that distinction: today's
  `dsl.ignore` resident is annotated `unsupported`, as a future gap-class resident would be. And
  that verdict comes from a bare-alias probe while the replay specs
  embed rows as members, so a shape the alias never reaches can be preserve-broken under a
  `supported` verdict; the retired float class was exactly that divergence, and nothing about the
  probe shape has changed since. The corpus twin gate below has no annotation axis at all — its rows
  are keyed to corpus fixtures.

  What the annotation CAN carry one-directionally is checked without running the gate:
  `preserve_unsupported_rows_carry_a_preserve_skip_entry` (a plain `#[test]`, two TOML reads, no
  generation — it runs at `local` via the full `cargo test`, since `fast`'s only cargo-test
  invocation filters on `snapshot_tests`) asserts every catalog row annotated
  `emission.preserve.status = "unsupported"` appears in the ledger, which lives at module scope
  (`DECODE_CONFORMANCE_PRESERVE_SKIP`) so the check can read it. It covers PINNED rows as well as
  active ones, and that is the whole point: a pinned row is never replayed, so its ledger
  obligations are otherwise invisible until the distant commit that activates it — `dsl.ignore`
  shipped pinned with its `unsupported` annotation already in place and the missing entry surfaced
  only at activation, two work packages later. A SUBSET assertion, never equality: extra ledger
  entries are legitimate (the bare-alias probe divergence above is exactly how one arises).
- **The drift gate** — `cddl-matrix/project_decode_conformance.ts` (check.ts `local` tier, pure
  file reads): matrix-supported ↔ catalog completeness, example-drift staleness (a drifted example
  means the vectors were validated against a spec the matrix no longer describes — re-mint),
  reject-pin class/reason/`expect_err` shape (including hand-authored policy-rejected vectors, whose
  source, reason, and error pin are mandatory), accept-vector class (no class, or exactly
  `class="over-acceptance"` with a `reason` and no `expect_err`), the § 6 shape rule extended to
  over-acceptance vectors (same-shape as the row's SPEC-VALID accepts, which now EXCLUDE over-acceptance
  from the shape-class set), and the hard-coded **seeded regression controls** — the
  absent-instance vectors (`occur.optional` holder `[0, []]`, `type2.map` holder `[0, {}]`,
  `occur.zero_or_more` holder `[0, []]`) that anchor the over-strict-decoder class TDD-style. It ALSO
  runs the **writer↔reader identity check** (§ 8): `composeCatalog` (in `cddl-matrix/lib.ts`) is the
  SOLE serializer of the hand-authored vector fields, so the gate asserts `compose(parse(catalog.toml))`
  is byte-identical to the committed file — a writer that drops or reorders a field (the silent-strip
  bug class: `class`/`reason` once emitted only under an `expect === "reject"` guard, which would have
  stripped every over-acceptance annotation) goes red before any re-mint corrupts the catalog. A
  synthetic all-fields sample round-trips through `parse∘compose` in the same section, so a dropped
  field is caught even when the committed catalog does not currently exercise it. It ALSO
  runs the **arm-coverage floor** (§ 7): the mint's `generate` is randomized, so a multi-arm CHOICE row
  can land with a whole arm unsampled and its decode verdict silently under-claims (the seed instance:
  `prelude.number` = `int / float` carried only int-headed accepts — the float arm had zero
  decode-direction evidence). For each active choice row whose root RHS statically resolves to arm head
  major-classes (`resolveChoiceArmClasses` in `cddl-matrix/lib.ts` — the ONE resolver the mint's
  resample loop shares), the floor requires ≥1 spec-valid accept vector per resolvable arm class.
  **Majors 0/1 merge into one "int" class**, so `prelude.integer` / `prelude.unsigned` don't flag their
  unsampled plain-uint side (nint already covers int). Two decay pins guard it: `EXPECTED_FLOOR_SCOPE`
  pins the EXACT (row → sorted arm classes) set the resolver fires on (a silent widen/narrow fails
  got/want), and `DECODE_FLOOR_ARM_EXEMPT` (`lib.ts`, stale-guarded) ledgers a genuinely unmintable arm
  class with a citation. Mint side (`verify.ts mintRow`): a **resample-until-covered loop** draws extra
  bounded `generate` batches for any missing class, keeping only two-oracle-valid candidates; on cap
  exhaustion with an unledgered missing class the mint exits 1, naming the row and class. At HEAD the ledger is EMPTY — its one past resident (`prelude.number`'s float arm, unmintable while
  the rust reference rejected a float against the prelude `number` keyword) was re-minted with real
  f32/f64 accept vectors once the fork fix landed at the `ac1b98e` pin; the stale-guard is what forces
  that removal whenever a ledgered gap closes.
- **The verify.ts oracle** — normal `verify.ts` runs replay each supported row's committed vectors
  as a default-on corroborating oracle (`--no-decode-foreign` / `VERIFY_DECODE_FOREIGN=0` opt-out),
  recording an `accepts_foreign` evidence clause in the annotations. Corroboration only — it never
  downgrades a support verdict; failures surface in the report's `decode_foreign_failures`. A
  replay that produces NO per-test verdict (a compile error, or the shifting-cell registry
  transient — the "Registry-fetch transients in nested-cargo cells" watch in
  `tests/testing-roadmap.toml`) regenerates and retries once before recording FAILED, the same
  absorber the mint paths carry, so one transient cannot flip a row's committed evidence clause.

First-sweep payoff — two miscompiles invisible to every self-consistent gate, each caught here by
feeding spec-valid CBOR our code did not produce. Map-representation group-choice single-field
variants emitted **malformed CBOR** (member key dropped) that our decoder symmetrically round-tripped
while rejecting the spec-valid form; that is now **fixed**, and the fix is pinned decode-direction by
the `group.choice` row's accept vectors (a reverted key-dropping decoder mis-decodes the spec-valid
`{"a": n}` foreign bytes and fails the replay gate), with the emitted key-write/key-verify guarded
against an unreviewed re-bless by `integration_tests::corpus_group_choice_map_key_written_and_verified`.
The array-side sibling — `[* (int, tstr)]` silently narrowing the inline-group occurrence to
exactly-once, rejecting the spec-valid `[]` — is now **fixed** too: an occurrence marker on an inline
group is rejected gracefully at generation time. The projected robustness fixtures
(`tests/matrix_reject/contain.occurrence-target.grpent.inline_group.*.cddl`) pin the unsupported cells,
so they project no decode-conformance obligation (no catalog row) rather than a `class="bug"` reject.
The bare-TYPE array-field instance of the same class — `[uint, tstr, * bytes]` narrowing `* bytes` to
one mandatory item, rejecting spec-valid zero- and two-bytes instances — was this sweep's third catch
(mintable only once the fully-fixed rust oracle stopped contesting the candidates) and is rejected
gracefully the same way; `occurrence_on_array_record_field_rejects_gracefully` pins the boundaries
(`+`/bounded/any position reject; `?`, `1*1`, and single-entry homogeneous `[* t]` keep generating).

#### Composition-depth (corpus) leg

The catalog above keys its obligation set on the matrix's minimal per-construct examples — breadth.
The **composition depth** those examples lack lives in the `tests/corpus/*.cddl` fixtures, so a
sibling catalog — `tests/decode_conformance/corpus_catalog.toml` — mints spec-derived decode vectors
for them. Its obligation set is `tests/corpus/*.cddl` **× the shared rule enumerator** (every
top-level rule of every fixture), mechanically derived and never a hand-picked fixture list, so every
(fixture, rule) carries ≥1 committed vector XOR a `pinned_reason` — the same no-silent-skips rule as
the matrix catalog. The enumerator and the per-rule dependency-closure builder live in
`cddl-matrix/lib.ts`, shared by the mint and the drift gate, so the gate re-derives exactly what the
mint derived. Refresh flow: `cd cddl-matrix && bun run verify.ts --mint-decode-corpus` (`--only=`
takes row ids AND bare fixture stems — a stem expands to the fixture's rows — preserving every
unselected row byte-identically; mint-ONLY: it writes this catalog and nothing else, never
annotations or the matrix catalog). **Use scoped `--only` for additive, review, and recovery mints**:
a bare mint
re-mints the WHOLE catalog, re-rolling every ruby-generated random vector in it (~1,900 changed
lines observed for a four-row addition, and 20 minutes against 23 seconds) — replacing the
committed regression vectors wholesale, which is a lossy trade even before the gap-11 caveat in
`cddl-matrix/README.md` § "Upstream oracle gaps" (an open-gap re-mint can demote live rows to
pinned).

Every active corpus row is **holder mode**: the probe spec is `__probe_holder = [0, <rule>]` plus the
rule's dependency closure (the target rule's span + every fixture rule transitively referenced from
it, in fixture order), `type_name = ProbeHolder`. Holder mode routes decode through the generated
member/field-decode path — the surface composition depth actually exercises — and covers bare-GROUP
rules too: `inner = (a: uint, b: uint)` splices into the holder array, so its vector is the wider
`83 00 …` = `[0, a, b]` rather than the single-item `82 00` (the header-mutation leg strips the same
2-byte preamble and mutates the spliced item at byte 2). The per-rule closure — rather than a
whole-fixture spec — quarantines one un-mintable rule from poisoning its fixture-mates.

Rows that can't be minted mechanically carry a `pinned_reason` instead of vectors, in stable classes:
the ruby generator's **inline-composite `.cbor`-controller parse gap** (gem 0.12.14 exit 65 —
`cddl-matrix/upstream-reports/ruby-cddl-inline-composite-control-arg.md`, re-mint when the gem fix ships), **generic
rules** (a `<…>` head can't be holder-wrapped bare — instantiations are covered via referencing
rules), and **`dsl_custom`** (references user-supplied (de)serialize code — can't compile standalone).
A distinct, decoder-clean class is the **named-rule / parenthesized-choice map-KEY over-rejection** in
the rust oracle (`cddl-matrix/upstream-reports/rust-cddl-named-key-map.md`): its affected table rows (`table_enum_key.*`,
`c_style_enum_map_key.enum_keyed_map`, and the adjacent-signature siblings on `composite_map_key.*`
and `wasm_nested_alias.passthru_tags_map`) keep only
their empty-instance accept vectors, because the rust reference contests every non-empty instance
while the ruby reference and our own decoder accept — an oracle-side drop, not a cddl-codegen gap.

Two gates mirror the matrix legs:
- **Drift gate** — `cddl-matrix/project_decode_conformance.ts` (check.ts `local` tier, pure file
  reads): its corpus half re-derives the glob × enumerator obligation set and asserts completeness
  (vectors XOR `pinned_reason`), staleness (each active row's committed `spec` byte-equals the
  reconstruction from the current fixture via the shared enumerator/closure builder — a drifted
  fixture reads "re-mint"), and the holder `82 00` / wider-`83 00` preamble shape.
- **Replay gate** — `integration_tests::corpus_decode_replay` (`#[ignore]`d, check.ts `full` tier —
  one of the generated `#[ignore]`d-gate roll-call in "Running everything"). It reuses
  `decode_conformance_replay`'s `decode_replay_generate` / `decode_replay_run` helpers and every leg
  verbatim (base accept, `cddl_encoding_fidelity::variants` encoding variants, `header_mutants` header
  mutation at holder offset 2, over-acceptance completeness, the `--preserve-encodings`
  byte-identity leg, and the json/wasm surface legs below), differing only in the catalog path, its
  own scratch target, its own skip-ledger instances, and vacuity floors pinned from actuals. The corpus
  carries plain accept vectors plus the tag-258 policy-rejected pins at HEAD; the authored-constraint
  and over-acceptance axes remain matrix-owned, so their machinery stays armed while policy-rejected
  vectors use the same
  default reason assertion and preserve rejection path as their matrix sibling (the tag-258
  `tag_set_default.default_set` row is the exemplar). `PRESERVE_SKIP` holds only the by-design
  `dsl_ignore.ignored` / `dsl_ignore.ignored_list` rows
  (`@ignore` under `--preserve-encodings` is a contract rejection, so its stale-entry guard is a
  regression tripwire), the
  json/wasm surface ledgers hold this gate's corpus residents (listed in § "json/wasm surface
  legs"), and every other ledger is empty and stale-guarded. Its `ENCODING_VARIANT_SKIP` is empty
  too: the `table_preserve.*` and `open_table.open_table_dup` reordering exemptions it used to carry
  are DERIVED from each row's own `spec` (see the encoding-variant leg above), so this catalog's
  pair-map rows and its loose-container control rows need no hand entries to stay distinguished.

#### json/wasm surface legs

The two replay gates above pin the RUST CBOR decoder. Two OTHER decode entry points ship with the
generated crate: the `--json-serde-derives` json surface
(`serde_json::from_str` over the serde-derived rust types) and the `--wasm` wrapper surface (the thin
`#[wasm_bindgen]` `from_cbor_bytes` / `from_json` delegators in `create_base_wasm_struct`). A json/wasm
boundary that is over-strict about spec-valid input the rust decoder already accepts would pass every
other gate, so each replay gate runs a **third generation per row** — `--wasm=true
--json-serde-derives=true`, default profile otherwise (NO `--json-schema-export`, NO preserve) — and
two accept-only legs off it (`decode_replay_json_wasm_legs` in `integration_tests.rs`, shared verbatim
by both gates). Only the PLAIN accept vectors are replayed: reject / constraint / policy-rejected / over-acceptance /
encoding-variant / header-mutant vectors evidence nothing about these boundaries (the reject direction
is rust-decoder territory, and wasm-side is `JsError`-blocked — see below).

**No external json oracle.** CDDL has no json generation target, so the obligation is defined against
the rust CBOR decoder's accepted values: *every value the rust CBOR decoder accepts from a committed
vector must survive the boundary.*

- **json leg** (`__foreign_decode_replay_json`, appended to the third-generation RUST crate's
  `generated/mod.rs`): per accept vector, `let v = T::from_cbor_bytes(BYTES)` (Ok), `serde_json::to_string(&v)`
  (Ok — the value must be json-SERIALIZABLE), `serde_json::from_str::<T>(&s)` (Ok — the over-strictness
  assert), `assert_eq!(v2.to_cbor_bytes(), v.to_cbor_bytes())` (the `to_cbor_bytes` value-fidelity
  proxy, since generated types don't uniformly derive `PartialEq` — the same proxy the encoding-variant
  leg uses).
- **wasm leg** (`__foreign_decode_replay_wasm`, appended to the WASM crate's `generated/mod.rs`, `cargo
  test`ed on the HOST target like `tests/*/tests_wasm.rs`): per accept vector,
  `T::from_cbor_bytes(BYTES).ok().expect(marker)` (accept direction ONLY — the wasm wrapper builds
  `JsError` on rejection, which PANICS under host `cargo test`, so a wrongful rejection surfaces as the
  loud `WASM_REJECTED` panic rather than an inspectable Err), plus a **cross-crate byte differential**
  (`wv.to_cbor_bytes()` == the rust crate's re-encode of the same bytes — the wasm crate path-deps on
  `../rust`), and where `from_json` is emitted, `T::from_json(&wv.to_json())` Ok with the same
  differential.

**Skip ledgers** (per gate, both REPRODUCTION-guarded like `WASM_SURFACE_SKIP`'s compile check — a
skip row's leg still RUNS, and the entry is consumed only if the leg still fails; a run where every
emitted test passes fails the gate as a stale pin): a row skipped on one leg still runs the other.
`JSON_SURFACE_SKIP` — rows whose json boundary legitimately can't round-trip; each resident cites
its owning record — a decided posture in `cddl-matrix/README.md` § Gotchas, or a
`cddl-matrix/roadmap.toml` finding for a defect — and it also suppresses the wasm `from_json` sub-leg
(same serde path). Resident classes at HEAD (both decided postures, recorded in
`cddl-matrix/README.md` § Gotchas):
**`@custom_json`** omitting the serde derives the leg's serde_json usage needs (`dsl.custom_json` /
`dsl_custom.custom_newtype` — can't compile standalone); and **non-string map keys** serde_json
can't serialize (`bytes_map_key.*`, `composite_map_key.holder`, corpus). `WASM_SURFACE_SKIP` — rows whose `--wasm`
generation or wasm-crate compile legitimately fails; also cited; sole resident class at HEAD is the
same `@custom_json` can't-compile-standalone class (the wrapper's `to_json`/`from_json` need the
impls the directive hands to the spec author, and these legs supply none — the contract working as
documented, not a defect). Distinct
from a **mechanical** skip: a type with NO `from_cbor_bytes` wasm wrapper surface (a bare primitive
alias, or a wrapper without the deserialize method — `deserialize_generated` gating) is classified
MECHANICALLY (`wasm_impl_has_fn` scans the generated wasm source for that type's inherent impl),
never hand-listed — a hand list of that class would rot. Loudly-logged, and paired with a "rows DO
exercise the wasm leg" vacuity floor.

**Vacuity floors** (pinned from actuals with ~10% headroom): a json-round-trip assert count floor and a
wasm-accept assert count floor per gate, plus per-crate emitted-test completeness (the run helper
returns `None` — treated as a compile finding — if the emitted test count doesn't match the expected
per-row accept-vector count). **Failure attribution**: grep-stable markers
(`JSON_SERIALIZE_FAILED` / `JSON_REJECTED` / `JSON_VALUE_MISMATCH`; `WASM_REJECTED` /
`WASM_VALUE_MISMATCH` / `WASM_JSON_REJECTED` / `WASM_JSON_VALUE_MISMATCH`) + `classify_json_failure` /
`classify_wasm_failure`, the same trailing-':' prefix-collision grammar as the base-leg classifiers
(pinned unit-side by `classify_json_failure_disambiguates_prefix_colliding_names` and its wasm sibling).

**Out of scope:** the `--wasm-cbor-json-api-macro` escape hatch (it replaces the wrapper surface with a
user-supplied macro; flag-gated, unexercised by these catalogs). The wasm reject direction (the
`JsError`-panic class). And json laxness (serde derives don't re-enforce CDDL bounds — an
enforcement-axis question for a future item, not this accept-direction leg).

### The directive×rule-shape sweep (`cddl-matrix/no_silent_directive.ts`)

A comment-DSL directive is only carried to a marking site by the parse path its rule's SHAPE takes,
and those paths differ — so a written directive can produce output byte-identical to omitting it,
with nothing acknowledging it. This gate is the systematic catch for that class, and it sweeps the
whole product rather than a hand corpus, so a shape whose parse path nobody thought about is covered
by construction. `local` tier (never `fast` — CI cost policy), ~37 s warm.

Per (shape, directive) cell it generates the built binary twice into throwaway scratch dirs — once
with the base directives, once with the toggled directive ADDED — and renders one of four verdicts:
**effect visible** (bytes differ), **loudly rejected** (nonzero exit whose output names the
directive), **acknowledged notice** (byte-identical, but the output names the directive), or
**allowlisted inert**. Anything else FAILS. The byte surface is every generated file under the output
dir, so the wasm and json-gen crates are compared too.

Three axes, each with its own authority:

- **Directive** — extracted at run time from `comment_ast.rs`'s `KNOWN_RULE_METADATA_TAGS`. A
  directive there with no canonical-spelling row or no witness-profile row in the gate's own tables
  FAILS it: a new directive must demand classification rather than silently skip the product.
- **Shape** — hand-enumerated (shapes change far more slowly than directives). The mandatory parse
  paths, the extras each of which proved interesting in a prior delivery, and the arm-position axis
  folded in as two shapes of its own.
- **Profile** — the cheapest flag set under which the directive's surface exists at all
  (`@used_as_elem` is a documented no-op without `--wasm`; `@no_json_schema_export` suppresses a
  json-gen row only). Generating a cell outside its witness profile measures nothing.

Each cell is swept under two holder embeddings and takes its BEST verdict, because the requirements
point in opposite directions: a container use (`g: [* foo]`) is what mints the class
`@extern_companions` defers and the getter `@copy` elides, while the ABSENCE of one is what
`@used_as_elem` exists to force. A FAIL therefore means "inert under every embedding", which is what
a finding must mean to be actionable.

The ALLOWLIST is the honest inert inventory — one justification per entry, and a stale entry naming
no cell fails the gate. Adding one is a claim that the cell is a legitimate accepted no-op (an
explicit spelling of a default, a directive whose target already satisfies it, a structurally
excluded emission site), never a place to park a real drop. The 17 hand cells above the product each
pin a specific shipped regression or placement control and are kept.

The gate renders six placements: the rule slot, an arm's trailing comment (`armPlacement`), the
ROW-ENTRY slot of an inline table (`rowEntryPlacement`) — a comment *inside* the braces, which the
others structurally cannot reach and which was where the inline-table `@duplicates` drop hid —
the closing-paren line of a multi-line plain group (`multilineGroupEntries`), an ordinary FIELD's
trailing slot in both reps (`record_field_{array,map}` — the member position every other shape
reaches only incidentally through a holder, swept in its own right as the CONTROL for the arm
shapes), and the ENTRY slot of a SINGLE-ENTRY group-choice arm
(`single_entry_group_choice_arm_{array,map}`). That last placement exists because a one-entry arm
mints no record, so the entry's trailing slot is the one member slot the record field walk never
reaches — the whole field-directive family dropped there at exit 0 while this gate passed 413/413,
the measurement that the position was ABSENT from the product, not tolerated by it. Both member
placements now measure refusals for the whole family — the rule-scoped directives are refused at
every member seam (with the plain-group LAST-entry slot exempt, because that slot doubles as the
group rule's own directive slot), and an entry-slot `@name` is refused wherever the
anonymous-inline-array reader does not consume it. `KNOWN_POSITION_DROPS` is the stale-pin
mechanism for any future ledgered drop (an entry fails the gate the moment its drop is fixed); it
is EMPTY today, and the one verdict this gate structurally cannot hold — the `@name` reader's
HONORED leg, whose without-directive baseline does not generate and so reads as a fixture bug —
is pinned in-process by the `robustness_tests` agreement test instead.

The multi-line plain group's closing-paren placement measures a REFUSAL, not an effect. For a spliced plain group the pinned cddl
AST binds a rule-position directive to the LAST ENTRY's trailing slot — indistinguishable from field
metadata inside the parens — so the product measures the real field/rule effects, targeted
rejections and justified redundant no-ops of that shared slot, and there is no separate rule-only
position to sweep. Writing the closing paren on its own line puts the trailing comment past that
slot entirely, where the parser can deliver nothing; generation refuses the spelling, so all 18 cells
of `plain_group_spliced_multiline_paren` go green through the nonzero with-directive exit and need no
allowlist rows. The unpublished parser fix tracked in `tests/testing-roadmap.toml` adds a
`RuleTrailing` anchor for that spelling. Adopting it must convert those cells from rejections to
rule-only honor/reject classifications in the same delivery, not reinterpret the working last-entry
slot ahead of the parser change.

### Sibling-crate companion classes (`@extern_companions`) — test map

The directive (user doc: `docs/docs/comment_dsl.mdx` § `@extern_companions`,
`docs/docs/wasm_differences.mdx` § the not-always-minted note) makes a locally-declared marker rule —
either user-supplied flavor, `_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_` —
REFERENCE a listed structural companion class from a sibling wasm crate instead of minting a
duplicate. Its defect class is a LINK-time duplicate `#[wasm_bindgen]` symbol, so its acceptance
sits at the one layer no other fixture family reaches:

- **Two-crate link gate** — `src/tests/integration_tests.rs::
  extern_companions_defers_to_sibling_wasm_crate` over `tests/extern-companions/`: the dep pair
  ships HAND-written `#[wasm_bindgen] IdxFooList` / `IdxHashList` classes beside its generated tree
  (hand-written, so no wrapper index could list them — the reported consumer case), and the gate
  builds consumer + dep wasm crates into ONE wasm32 target — the duplicate-`__wbg_*_free` half with
  the directives stripped, link-clean with them present, plus the native compile. One spec carries
  both markers over the same shapes (table key + list element), so the link verdict is attributable
  to the marker KIND: the RED stderr must name `__wbg_idxfoolist_free` **and**
  `__wbg_idxhashlist_free`.
- **Directive grammar + positions** — `comment_ast` malformed-arg panics (family convention:
  missing arg/`=`, bad prefix, bad class ident, duplicate directive); 9 `dsl_position_tests` GRID
  cells (honored on each marker; unlisted-companion still mints; rust-only inertness; five
  position rejections); 3 `no_silent_directive` cells.
- **Per-marker honoring + scope bar** — `src/tests/extern_companions_tests.rs` drives the generator
  over synthetic scratch trees: the CML-shaped extern case, the raw-bytes twin
  (`raw_bytes_marker_defers_its_listed_companion`), multi-class lists, the NonEmpty twin, the
  mixed-constituent fall-through, and the dep-scoped rejection for BOTH markers (the scope check is
  orthogonal to which marker the rule spells).
- **Collision seam** — `robustness_tests`: a user rule claiming a LISTED class is the
  `extern_companion_rule_name_collisions` graceful rejection (the `use` + a local class would be
  E0255), and the dep-scoped / neither-marker placements reject naming the flags that own
  those cases.
- **Matrix row** — `dsl.extern_companions` in `cddl-matrix/features/cddl_codegen.toml`, its
  `[[support]]` verdict via the `COMPILE_GATE_EXEMPT` ledger (the standalone example cannot
  compile by construction — same class as `ext.extern`; the exemption row cites the link gate
  above), `tests/matrix_supported/dsl.extern_companions.cddl`, and a decode-catalog
  `pinned_reason` row on the same precedent.

### JSON-schema document — Rust-side coverage (`run_test`'s per-fixture assertions + the emitted name guard)

`--json-schema-export` is covered in three layers, and this is the first two. The document
(`wasm/json-gen/schemas/<lib>.schema.json`) is written by a *program the tool emits*, so its content
is a property of a RUN, not of the emitted source — every cheap verdict ("generates", "compiles",
"the `.d.ts` type-checks") is satisfied by a document that publishes one type's shape under another
type's name. So the layers are: our suite runs the json-gen crate and asserts the document; the
emitted crate carries a guard that fires in a *consumer's* own run; the JS pipeline (next section)
turns the document into the shipped `.d.ts`.

**Per-fixture, in `integration_tests::run_test`.** Every fixture passing `--json-schema-export=true`
gets its json-gen crate built and `cargo run`, not merely built — a broken `export_schemas()` body
exits the build green. `schemas/` is deleted first, so a previous local run's output cannot satisfy
the asserts. Then: exactly one file in `schemas/`, named `<lib>.schema.json`; a non-empty `$defs`;
`$defs.len()` at least the `reg.add::<` row count (a row's type silently failing to land is the
counting proxy — `$defs` is never smaller, since the runtime helper publishes an inline-schema type
under its own name rather than letting `subschema_for` drop it); **reference closure** — every
`$ref` anywhere in the document is an internal `#/$defs/<key>` naming a key of that same document
(`decode_schema_ref_name` inverts schemars' percent/JSON-pointer ref encoding), which turns "which
types can dangle?" from an argument into a check and is what catches a hand-written impl returning a
bare `Schema::new_ref("SomeType")`; **extra roots** — for each `--json-schema-root` the fixture
passes, the named type's `$defs` key must be present, since a type no CDDL rule describes has the
flag's registration row as its only route into the document (the expected key is derived from the
path, which holds while every fixture root derives its `schema_name()`; a root with a hand-written
`schema_name()` needs a per-fixture expectation instead); and **byte-identity across two runs** of the same binary, because
the document is built by walking a live `SchemaGenerator` rather than by printing a sorted list, so
determinism is a runtime property here, not only an emitter one (the emitter's own byte-stability is
pinned in the fast tier by `snapshot_tests::json_gen_rows_are_byte_stable`).

**The runtime guard, which runs where the consumer runs.** A `$defs` key is a published API name
(json2ts emits it, suffixed, as the TypeScript type name), so the common crate's `add_schema` helper
— which every emitted `reg.add::<T>()` row reaches through the `Registrar` that owns its ledger —
enforces that the document's published names are injective, panicking with both offenders named. It carries
three checks: a **name ledger** keyed on `core::any::type_name` — the only thing that can see a
*merge*, where two hand-written impls return one name and `schema_id()`'s default makes them one type
to `schemars`, so both returned refs equal the shared name; a **kept-its-own-name** comparison against
the ref `subschema_for` returned — which sees an order-dependent `<name>2` even when the type that
claimed the name has no row of its own; and a **conflict check** on the inline branch's `definitions`
insert. Pinned by `snapshot_tests::json_gen_extern_schema_rows` (fast tier: the emitted wiring, plus
the twin pairs under "Design rules" below) and executed by two integration vectors,
`json_schema_name_merge_fails` and `json_schema_name_stolen_fails`. Those two need their own fixtures
and their own harness (`run_json_gen_failure_test`) rather than riding `run_test`, because `run_test`
asserts the json-gen run SUCCEEDS and the whole point of these is a spec whose run must fail; the
harness mirrors `run_test` in every respect but the verdict, and asserts on a message FRAGMENT so a
fixture that starts failing for an unrelated reason still fails the test.

The emitted crate carries the **reference closure** too, not only the name guard: `export_schemas()`
walks the finished document and panics — listing every offender, sorted — when a `$ref` is not an
internal pointer at one of that document's own definitions, *before* it writes, so a failing run
never emits a broken document (it also writes nothing, leaving any earlier export in place). Both
failure classes are executed by `json_schema_ref_dangling_fails` over `tests/json-ref-dangling` (a
bare `Schema::new_ref("SomeType")`, and an internal pointer at an undefined key). Its green
direction — the ref DECODE, which must invert schemars' percent + JSON-pointer encoding — is a
vector inside `tests/json-extern`: a hand-written `schema_name()` of `Odd<K>/~1café`, chosen because
it exercises all three escape classes, including multi-byte UTF-8 percent encoding, *and*
distinguishes the unescape ORDER (the literal `~1` encodes to `~01`, which the wrong order decodes to
`Odd<K>//café`). Do not simplify that name — a simpler one passes under both orders or misses the
encoder's UTF-8 path and certifies nothing.

**`--json-schema-export` without `--json-serde-derives`.** The two flags are independent — nothing
implies or rejects either from the other — but every other json-schema cell passes both, so the
uncoupled combination gets one of its own: `json_schema_export_without_serde_derives` over
`tests/json-schema-no-serde` generates with the schema flag alone and runs the json-gen crate. What
it holds is the generated `rust/Cargo.toml`'s `serde_json` condition, which is the OR of the two
flags rather than `--json-serde-derives` alone: under the schema flag the rust crate hosts
`json_schema_gen.rs`, whose closure check walks a `serde_json::Value`, while no type in the crate
derives `serde::Serialize`. Narrowing the condition back makes the crate an `E0433`, which only a
real compile sees — hence a nested `cargo run` rather than a manifest assertion alone. Its
`--export-static-crate` twin is generation-level only, a leg of
`export_static_crate_writes_composed_runtime_and_manifest`.

`--json-schema-root`'s input contract is pinned separately and without cargo by
`json_schema_root_input_contract`: the flag requires `--json-schema-export`, a repeated value is a
hard error, and the value parser accepts a rust type path (generics included) while rejecting
anything that could inject tokens into the generated file. An extra root is emitted as an ordinary
registration row through the same `Registrar`, so it inherits all three of the guard's checks by
construction — an inheritance asserted by reading the emitter rather than by a fixture putting an
extra root on the LOSING side of a collision, which would cost another nested-cargo failure cell.

**`--json-schema-dep` (threading a dependency's registrar) is pinned across three layers.** Its
emitted SHAPE is the fast-tier tail of `snapshot_tests::json_gen_extern_schema_rows` — the calls are
present verbatim, dashes in a cargo package name are normalised to underscores, they appear in flag
order rather than sorted, and they precede both the name ledger and every row of the crate's own.
That ordering is the deliberate mirror of the `--json-schema-root` rows asserted just above it in the
same test, and the reason is the guard: registering the dependency first is what makes a cross-crate
collision hand the CONSUMER's row the `<name>2` and blame the side whose owner can change it. Its
input contract is pinned without cargo by `json_schema_dep_input_contract` (requires
`--json-schema-export`; a repeated label and one lib name under two labels are both hard errors; the
value parser accepts a crate name, a module path and a dashed package name while rejecting a token
injection, a missing `=`, and either side empty). Its SUCCESS direction is the nested-cargo
`json_schema_dep_threading` over `tests/json-schema-dep`, which appends a `vendored_dep` module to
the seed-once `wasm/json-gen/src/lib.rs`, runs the crate, and asserts a root NOTHING in the fixture
spec references reached `$defs` beside the fixture's own — the unreferenced-roots gap is the entire
point of the flag, since everything referenced is already there through the closure. That cell's
"dependency" is a module of the json-gen crate itself, so it proves the emitted call site compiles,
runs first, and lands unreferenced roots — the SEPARATE-crate half is its `--json-gen-dep` sibling
below.

**`--json-gen-dep` (the `[dependencies]` entry that makes a threaded call link) is pinned across
three layers.** The manifest CHANGESET is pinned without cargo by three unit tests in
`cargo_manifest.rs`: `json_gen_dep_writes_a_path_dependency_entry` (one path entry per value, in
package-name order rather than flag order, beside the change log's own keys),
`json_gen_dep_converges_on_a_hand_added_entry` (an entry a user already wrote by hand converges on
ONE entry — our `path` wins, their `optional`/`features`/comment survive, unrelated keys and
`[profile.*]` pass through — and re-applying over our own output is a fixed point, the
run-twice-equals-run-once contract asserted on the MERGED form, which is the only form that can
drift), and `json_gen_dep_entry_is_asserted_never_removed` (dropping the flag LEAVES the entry: the
package name lives only in the flag value, so nothing names it to tombstone — pinned because the
alternative reading would take a hand-added dependency with it). Its input contract is pinned by
`json_gen_dep_input_contract` (requires `--json-schema-export`; a repeated package name is a hard
error; the package side carries cargo's package-name charset while the path side deliberately
carries none, since it is quoted by the TOML writer rather than spliced). Its SUCCESS direction is
the nested-cargo `json_gen_dep_links_a_threaded_dependency`, which generates TWO crates into a
scratch directory, threads the second to the first with `--json-schema-dep` *and* `--json-gen-dep`,
and builds and runs the consumer's json-gen crate — the only layer that can see whether cargo
actually resolves the path the flag wrote, since the manifest text reads correct either way.

**`--wasm-dep` (the same move on `wasm/Cargo.toml`) is pinned the same three ways**, deliberately as
parallel siblings rather than one parameterized test: "it is the same code path" is exactly the claim
a later refactor could quietly falsify. `wasm_dep_writes_a_path_dependency_entry`,
`wasm_dep_converges_on_a_hand_added_entry` and `wasm_dep_entry_is_asserted_never_removed` in
`cargo_manifest.rs` mirror their json-gen counterparts above; `wasm_dep_input_contract` mirrors the
input contract (requires `--wasm`, not `--json-schema-export`). Its success direction is the
nested-cargo `a_config_generated_workspace_builds_with_wasm_on` in `config_tests.rs` — see the config
section below, since the derivation is where the flag is actually used.

What these layers cannot see — a collision whose loser has no row and whose `schema_id`s match, a
cross-crate collision between two `add_schemas` calls whose `schema_id`s match, and a collision
between two types that BOTH lack rows — is enumerated in `tests/testing-roadmap.toml`, along with
the extra-root-on-the-losing-side cell that is recorded rather than minted. The kept-its-own-name
guard now decodes the ref Schemars actually returned, so percent-encoded names are covered without
reproducing its private encoder.

### JSON-schema → TypeScript JS-side pipeline (`js_schema_to_ts`, `js_d_ts_merge`, `package_json_pipeline`, `json_schema_scripts_without_package_json`)

`--json-schema-export` ships a JS toolchain that turns the exported schema document into TypeScript
and merges the result into the wasm-pack `.d.ts` (`static/run-json2ts.js` + `static/json-ts-types.js`, copied
by `--package-json --json-schema-export` or by `--json-schema-scripts` on its own).

These two scripts are *tool-owned shipped artifacts*, not test scaffolding: they are where the
product value of `--json-schema-export` is realized, and every way they can be wrong is invisible
from the Rust side. So each failure mode gets its own vector, and the three that are silent get one
that pins the LOUD behaviour (a non-zero exit) rather than just the happy path — a silently-wrong
script produces a `.d.ts` that looks plausible and a build that exits 0. Four tests cover it,
cheapest-in-isolation first.

Every leg below that needs npm packages resolves them through ONE locked install
(`shared_ts_toolchain`), not one per call: the effective manifest is the shipped
`static/package_json_schemas.json` plus a test-only `typescript` range, and the committed
`tests/ts-toolchain/package-lock.json` fixes the exact test resolution (`json-schema-to-typescript`
15.0.4 and TypeScript 5.9.3 today). The shipped package retains its caret ranges; this lock makes
only the shared test universe reproducible. The install root under the gitignored `/.ts-toolchain/`
is keyed by both effective-manifest and lock bytes, so changing either lands in a new root and
reinstalls rather than silently reusing the old tree. The install is serialized on
`acquire_scratch_lock` (`cargo test` runs the legs as parallel threads, and separate checkouts run
concurrently), and its `.installed` stamp is written only after `npm ci` exits 0, which is what
lets every later caller skip the lock entirely. Each work dir gets a `node_modules` symlink into it;
`tsc` is invoked by explicit path. Sharing it is what makes the projection leg affordable per
fixture rather than per opt-in.

- **`js_schema_to_ts`** runs the shipped `run-json2ts.js` over the committed schema document
  (`tests/json2ts/schemas`) using the shipped `json-schema-to-typescript` range (`^15.0.4`) under
  the shared test lock's exact 15.0.4 resolution, asserting the emitted
  `.d.ts`: every definition declared exactly once and JSON-suffixed (including one that nothing but
  another definition references — the shape that ships as an undeclared `TS2304` unless the whole
  document is compiled as one unit), resolved refs, enum → union, the `additionalProperties` guard on both a struct and a map
  definition, a definition whose own `title` does not become its emitted name, no near-duplicate
  `FooJSON1` from a `$ref` with a sibling annotation, and no synthetic root. It pins the
  declaration-NAME guarantee too — `<$defs key>JSON` exactly, for every key spellable as a TypeScript
  identifier, whatever json2ts's own title normalization would have made of it (`Blake2b256JSON`, not
  `Blake2B256JSON`), including a pair of keys that normalization used to conflate into one name plus a
  `JSON1`, and the two controls that make the map-back's safety argument checkable: a `description`
  and an enum member string both naming the awkward type survive VERBATIM, which is what rules out
  renaming identifiers in json2ts's output. A `$defs` key that is not itself an identifier
  (`OrderedHashMap<K, V>`) keeps the normalized spelling, since there is no `<key>JSON` to promise. It also pins the
  catch-all widening — named properties beside a catch-all, in BOTH spellings (`patternProperties`
  and `additionalProperties`, which is which is a property of the rest row's key domain, not of the
  projection), top level and nested, with the optional-property case that needs `undefined` in the
  widened union, plus the two catch-alls that must NOT be widened because they are already legal and
  exact (one a declared member's schema is structurally equal to, and an everything-admitting one) —
  and then
  type-checks the emitted file with `tsc --noEmit --strict --target esnext`, the oracle the substring
  asserts cannot be. `--skipLibCheck` stays off (it would make the check vacuous over a `.d.ts`) and
  `--strict` is what makes the optional-property case fail at all; `typescript` is injected into the
  shared install's manifest rather than the shipped one, so `static/package_json_schemas.json` keeps
  pinning only what a consumer's package actually needs. A `$defs` key that is also REFERENCED
  (`Odd<K>/~1café`, the spelling `tests/json-extern` really emits) pins the pointer escaping: a
  `#/$defs/...` token carries the JSON-Pointer escapes under the URI-fragment percent-encoding, and
  matching it against a key without decoding both layers leaves the reference pointing at the
  pre-rename key — which kills the whole document inside json2ts's resolver, not just that one type.
  It then bridges the two shipped scripts once, over
  the defs file that run just produced: the declaration-name guarantee only pays off where
  `json-ts-types.js` keys the splice on that exact name, and `js_d_ts_merge`'s hand-written defs
  cannot see whether the two agree, and asserts a second run over the same document is
  byte-identical — the synthetic tokens are index-derived so a consumer's committed `.d.ts` cannot
  churn between builds. Five further phases pin
  the failure directions, each a non-zero exit that leaves the last-good `json-types.d.ts` on disk: a
  document that cannot compile, a stale per-type schema beside the document, two documents, a
  document with no definitions, and two definitions landing on one declaration name. (Any of them
  exiting 0 ships a `.d.ts` that silently drops — or silently MERGES — part of the JSON surface, with
  nothing in the build output saying so. The last one cannot be delegated to `tsc`: TypeScript merges
  same-named `interface` declarations without a diagnostic.)
- **`assert_schema_projects_to_legal_ts`** is the same two steps over a fixture's REAL emitted
  document, and is a helper rather than a test of its own: `run_test`'s json-gen block calls it for
  EVERY fixture whose json-gen crate ran, so the oracle needs no registration and a future
  `--json-schema-export` fixture inherits it by existing. There is deliberately no exclusion list —
  a fixture whose real document does not project is the finding the leg exists to produce, not a
  cell to opt out of. (It is memoized per fixture, so the two callers that additionally pin shapes
  of the emitted `.d.ts` — `open_struct_map_json_e2e`, `open_table_json_e2e` — read `run_test`'s
  projection instead of paying a second `node` + `tsc`.) What it adds over `js_schema_to_ts` is the only thing a hand-written
  document cannot supply — that the GENERATOR still emits the shapes that document mimics — and the
  class it exists for is invisible everywhere else: a schema can be exactly right, with every
  `tests/*/tests.rs` schema assertion passing, and still project to TypeScript that does not compile,
  because an index signature ranges over every property while `additionalProperties` ranges only over
  the unnamed ones. Nothing but a `tsc` run says so, and the consumer's npm build is where it
  otherwise surfaces. The json-e2e fixture carries both spellings and both failing directions (a
  typed key domain's `additionalProperties` under a numeric declared member, and a `text` domain's
  under a string one), and the caller pins that each open type's index signature is the union form
  rather than the bare range. Running it at breadth is what found the pointer-escaping gap
  `js_schema_to_ts` now pins: `tests/json-extern`'s hand-written `JsonSchema` impl publishes a
  `$defs` key holding `<`, `>`, `/` and multi-byte UTF-8, and it was the only REFERENCED
  non-identifier key anywhere in the tree.
- **`js_d_ts_merge`** runs `json-ts-types.js` in isolation over hand-written fixtures — no
  wasm-pack/json2ts needed — laid out in the shipped `<root>/scripts/*.js` shape the script resolves
  its own paths from. Six cases, one per failure mode: the happy path (specialize + append); a class
  with no emitted JSON type exiting non-zero — naming the class and the `--allow-untyped=` escape,
  with the `.d.ts` untouched — rather than silently shipping `any`, plus the escape's two halves (an
  allowed class keeps `any` rather than gaining a `TS2304` dangling name, and a stale escape is
  itself an error so the list cannot rot) and the near-miss diagnostic (a class whose declaration
  differs from `<Class>JSON` only by json2ts's identifier normalization is named with both spellings
  and excluded from the suggested escape, because its type is published already — and, because the
  shipped `run-json2ts.js` emits declaration names verbatim and cannot produce such a near-miss
  itself, told to re-run that script (the name can only come from a stale or foreign defs file)
  rather than to rename their rule); a second run
  being byte-identical (the appended block is marker-delimited and truncated each run, so re-running
  without an intervening `rimraf ./pkg` can't duplicate every declaration); a method name the script
  cannot find exiting non-zero with the `--method=` override named and the `.d.ts` untouched (the
  `--wasm-cbor-json-api-macro` case, where the macro body names the methods); and a non-default
  wasm-pack crate name (i.e. `--lib-name`) still being found, because `pkg/` is scanned for its single
  non-`_bg` `.d.ts`; and a wasm `export class FooJSON` colliding with the appended `FooJSON`
  declaration failing before the write, with the bindings `.d.ts` byte-for-byte untouched. Under the
  shipped `<key>JSON` projection contract, that declaration corresponds to `$defs` key `Foo`; a
  foreign or hand-written declaration is identified only by its observed spelling. `tsc` cannot
  cover that last cross-half collision: TypeScript legally declaration-merges a class and an interface
  with the same name, so the resulting wrong public type checks.
- **`json_schema_scripts_without_package_json`** pins `--json-schema-scripts`: the two scripts land in
  `<output>/scripts/`, no `package.json` is written, the bare layout puts the wasm crate at
  `<output>/wasm` (the second candidate the scripts' wasm-dir detection exists for), and the flag
  without `--json-schema-export` is rejected up front.
- **`package_json_pipeline`** is the end-to-end gate: it generates a small extern-free fixture
  (`tests/package-json/input.cddl`) with `--wasm --package-json --json-serde-derives
  --json-schema-export` and runs the SHIPPED `npm run rust:build-nodejs` script VERBATIM — `wasm-pack
  build --target=nodejs` → json-gen `cargo +stable run` → `run-json2ts.js` → `json-ts-types.js` →
  `wasm-pack pack`. Running the script line itself (its `cd`/`;` shell shape, its dependency pins, its
  `cargo +stable`) is the point; replicating the steps in Rust would let the script rot. This is the
  ONLY layer that exercises `#[wasm_bindgen]` macro-expansion → a real wasm-pack `.d.ts` → the JS-side
  merge end-to-end — the systematic wasm gates `cargo check` on the host target and can't see any of
  it. Asserts pin each stage: the layout copy block, a wasm-pack `.d.ts`, a json-gen `schemas/` holding
  exactly one `<lib>.schema.json`, `to_json_value(): FooJSON;` + `export interface FooJSON` in the
  merged `.d.ts` (proving the merge ran on real output, not a fixture), and a `.tgz` from `wasm-pack
  pack`. It also carries the end-to-end dangling-type pin: the fixture's `inner_no_row` gets no
  registration row (`@no_json_schema_export`) but IS referenced by `foo`, so the document must carry
  it in `$defs` and the merged `.d.ts` must DECLARE it — the pin exists because a suppressed-but-
  referenced type reaching the shipped package as a `TS2304` is invisible to every Rust-side gate.
  Because it runs the shipped script line verbatim, it also runs `json-ts-types.js` with **no
  `--allow-untyped`** — so it is the only layer asserting that a default generated package satisfies
  the untyped-class check end to end. What keeps that true is which classes declare the JSON method
  at all: a record mints one, while a collection wrapper and a c-style enum do not, so the fixture's
  table, list and enum rules are outside the check by construction and only `foo` (rowed) and
  `inner_no_row` (in `$defs` by reference) have to be typed.
  The merged file is then TYPE-CHECKED (`tsc --noEmit --strict --target esnext`, `--skipLibCheck`
  off): the substring asserts can only catch the wrongness they were told to look for, and the
  merge joins two independently-produced halves — wasm-bindgen's class declarations and json2ts's
  interfaces — so a name resolving in neither half alone is exactly what a bad merge produces.
  `--target esnext` is required rather than stylistic, because wasm-bindgen emits `[Symbol.dispose]`
  members that only TypeScript ≥ 5.2 parses. The compiler comes from the shared install rather than
  from the generated `package.json`: injecting a dev-dep into the SHIPPED manifest would change what
  this test's own `npm install` proves. A committed negative case keeps the leg from going vacuous —
  one reference in the merged file is renamed (the declaration keeps its name, so it dangles) and
  the same call must reject it, which is what a bad path, a flag typo or a future `--skipLibCheck`
  would otherwise pass forever.
  It builds the
  generated crate with the user's `+stable` toolchain (faithful to the shipped consumer experience),
  so a `+stable` failure here is a real finding about shipped output, not a test bug. Needs
  node+npm+wasm-pack + a rustup `stable` toolchain; skips locally when absent (asserts their presence
  under CI, though CI's fast tier never reaches it). Plain `#[test]`, so it runs in the `local` tier
  like `wasm_json_roundtrip` (~20s warm).

## Generated-test harness (`--emit-tests`, `src/emit_tests.rs`)

The generator can emit a `#[cfg(test)] mod cddl_generated_tests` into the generated rust crate:
per-type **round-trip** tests (IR-derived cases — baseline, bound boundaries, one per choice
variant, each optional field present — asserted byte-identical through the full wire cycle, and
— outside preserve-encodings, where wire-populated encoding fields legitimately differ — the
deserialized value asserted `Debug`-equal to the minted original: byte-identity alone is a fixed
point for an information-losing projection serializer, so it can't see that miscompile class) and
**bounded-reject** tests. Values are minted deterministically from each type's IR (no
proptest/`Arbitrary` deps in generated crates — the repo's determinism ethos would force a fixed
seed anyway, and a fixed-seed sampler is a deterministic enumerator with extra machinery; both
designs share the same per-IR-shape derivation surface in `emit_tests.rs`, which is the single
maintained thing); unmintable shapes are skipped with a logged notice. Two consumers run it in CI: `integration_tests::emit_tests_execute` (the rich
preserve-encodings fixture, with emitted-test count floors) and `feature_corpus_compiles`'
default profile (below). This is the "output is right, not just unchanged" oracle — it caught two
snapshot-blessed miscompiles (`.ne` bounds, preserve-encodings default-field serialization) on
its first corpus sweep. It shares the generator's IR, so IR-level bugs (wrong bounds computed at
parse time) are the spec-anchored oracles' job (`tests/golden_hex/`).

### The corpus compile gate (`feature_corpus_compiles`)

The corpus gate `feature_corpus_compiles` `cargo check`s every `tests/corpus/*.cddl` crate (rust +
wasm + json-gen) under all three profiles, and under the **default profile** additionally
generates with `--emit-tests` and `cargo test`s **both** the rust and the wasm crate — so a corpus
construct must round-trip, not just compile, on both the rust and the wasm side. (`cargo check`
never compiles `#[cfg(test)]` code, so nothing but `cargo test` type-checks or runs the emitted
`cddl_generated_wasm_tests` module below; the preserve/json profiles and json-gen stay check-only.)

`preserve_pair_map_self_encoding` is the positional `@duplicates preserve` table where both
key and value are nominal records and therefore own their encoding sidecars. Its enclosing record
forces the serialize loop to bind `_i` with neither positional `.get(i)` lookup live, pinning the
unused-binding/E0425 boundary in snapshots and the ordinary Rust/wasm corpus compile floor. Its
standalone `entry` rust conformance call remains live. The scheduled Cycle-3 full-tier
`ir_conformance_corpus` run also found an upstream rust-`cddl` facet on `holder`: valid `[{}]`
(`81a0`) for `{ * entry => entry }` is misclassified as `expected array type, got Map([])` when
both domains resolve to named composite arrays. The gate therefore neutralizes only `holder`'s
exact rust call; ruby, dumped-byte, and reference-codec structural checks remain live, and
`self_map` is transparent so has no standalone call. See
`cddl-matrix/upstream-reports/rust-cddl-named-key-map.md`; the existing systematic gate caught the
class, so this needs no new testing-roadmap item.

**A fixture that names user-supplied code is SEEDED, not skipped.** `CORPUS_DEF_SPLICE` keys the
definitions each such fixture needs by stem, rendered per fixture from the shared
`tests/def_templates/` and appended to the thin `rust/src/lib.rs` / `wasm/src/lib.rs` crate roots
(never `generated/**`, which is clobbered every run and where the tool's own extern re-export glue
would collide) — so a stem absent from that list and from `COMPILE_SKIP` needs no user code at all.
`COMPILE_SKIP` is currently empty: every corpus fixture has either no user-code dependency or a
complete shared-template splice. `extern_generic_raw_bytes` is now the sharpest seeded contract: Rust
gets `ExtSet`, `ExtSetRawBytes`, and `PubKey`, while wasm gets only the concrete
`ExtSetPlain`/`ExtSetPubKey` wrappers that its generated boundary names. This both compile-gates the
fixture on all faces/profiles and makes a regression to a generic-argument-only `PubKey` wasm re-export
fail loudly. The sibling gates share the empty skip list and the splice table rather than restating
either one.

**Why `tests/corpus/` is the input of record for a shape whose defect is not visible in-process.**
Each cell SHELLS the real CLI (a subprocess writing a real crate to a scratch dir), so it exercises
`export()`'s whole disk-write path — including the rustfmt post-pass, whose non-0/3 exit is fatal by
design, so an emission rustfmt refuses is a non-zero generation exit and a red cell. That seam does
not exist for `api::generated_strings`, the in-process library API every other suite drives — the
snapshot corpus, the panic/reject catalogs, `wasm_api_parity`, and
`all_supported_constructs_generate_all_profiles` (which is generation-only over
`tests/matrix_supported/` and writes nothing, despite taking an `--output`). Combined with the
three-profile sweep here and the full-tier execution sweep
`feature_corpus_roundtrips_nondefault_profiles`, that makes a corpus fixture the cheapest way to
cover the three classes an in-process suite structurally cannot: an emission rustfmt rejects, an
emission rustc rejects, and an emission that is only wrong under a non-default profile. Promote such
a shape into `tests/corpus/`; do not build a harness beside it.
A fixture whose generation deliberately aborts under ONE profile is ledgered per-profile in the
gate's `EXPECTED_GENERATION_FAIL` (`(stem, profile, reason)`), for either of two reasons: it reaches
a tracked unimplemented path, or the profile REFUSES it by design. Its one resident is the by-design
kind — `dsl_ignore`/preserve, where an `@ignore` open struct-map is rejected under
`--preserve-encodings` because a preserve crate's byte-exact round-trip contract cannot hold for a
type that drops unknown entries. Stale-guarded both directions: a listed cell that starts generating
fails as "gap closed — remove the pin", which for a by-design entry is read the other way round (the
contract regressed — investigate rather than delete the pin); an unlisted generation failure fails
normally. The same cells are mirrored where the other corpus
walkers would trip over them: the snapshot suite's `PROFILE_GENERATION_SKIP` (`snapshot_tests.rs` —
no snapshot exists for a profile that never generates) and
`feature_corpus_roundtrips_nondefault_profiles`' `SKIP`, each with its own stale guard.

Because these crates are purely generated (no hand-appended scaffolding), the gate also doubles as
the rustc-warning detector for the usage-derived import prune (`import_prune`): after each nested
cargo invocation it scans stderr (`unused_generated_import_lines`) and fails on ANY `unused import`
warning in the generated crates — collection/encoding idents,
`super::*`/`error::*`/`cbor_encodings::*` globs, cross-scope type imports, and wasm macro/prelude
imports — minus a documented trait residue
(`UNUSED_IMPORT_TRAIT_RESIDUE`, the `cbor_event::se::Serialize` trait the name-scan model can't
prove unused). It also fails on ANY `unused variable` warning (`unused_generated_variable_lines`):
a named binding rustc reports unused in a purely-generated crate is generator imprecision (a
count-match arm that should bind `_`), with no trait-residue analogue. This catches a
warning-severity under-prune (or unused-binding emission) the compile-error gates (E0412/E0433,
over-prune only) cannot see. The scan is versioned into the gate-cache key via a
`lint=unused-imports-v3` marker so a change to its verdict re-runs every cached cell.

Those two scans reach beyond this gate's own cells, in two shapes. The corpus cells never generate
under the cross-crate workspace flags, so both scans also run — through
`assert_no_unused_generated_warnings` — over the nested cargo stderr the workspace-requests gates
already capture: `workspace_requests_hosts_cross_scope_elements`,
`workspace_requests_cohosted_keys_list_no_self_import` and
`workspace_requests_hosts_borrowed_wrappers`. That is where the requested-collections sidecar's own
imports are first observable. That call shape is restricted to crates that are 100% generated,
where no attribution is needed.

Every `run_test` fixture is scanned too, by the location-aware sibling
`assert_no_generator_owned_unused_warnings`, wired on all six cargo-driving stages `run_test` runs
(rust `cargo test`; wasm `cargo test` / `cargo build`; `wasm-pack build`; both json-gen
`cargo run`s). These crates are NOT 100% generated — the harness appends its `tests.rs`/`deser_test`
modules INSIDE the generated `generated/mod.rs`, and the extern-deps family path-depends on the
hand-written `tests/extern-dep-crate` — so the restriction is positional rather than per-file: the
`GeneratedOwnership` the run builds holds the export's generated `src/` roots plus each appended
file's pre-append line count, and rustc's separate `--> path:line:col` line attributes each warning
against them. Exempt: anything past an append boundary, anything outside those roots (path-dep
warnings, which rustc renders ABSOLUTE where the crate's own files render relative to the cargo
cwd), and a warning with no location to pair. `KNOWN_GENERATOR_OWNED_WARNINGS` is the escape hatch
for a generator-owned warning that cannot be fixed in the same change — a pin by file and exact
warning text, asserted still-live at the end of the run that owns its export so a fixed emission
fails its pin as stale instead of blinding the scan forever. It ships EMPTY with its enforcement
live (`KNOWN_POSITION_DROPS`' shape): the one warning this wiring surfaced was a one-line emission
fix. The harness's own `use serialization::*;` append carries `#[allow(unused_imports)]` — it is
convenience glue some pasted-in tests need and some do not, and the generator emits that glob
nowhere. Those fixtures' generated output lands in `tests/<dir>/export*/` — disposable, gitignored,
and safe to `git clean -fdx tests` if the ~GBs of build artifacts pile up locally. CI starts clean
each run.

**The `--annotate-fields=false` leg.** A second set of shards,
`feature_corpus_compiles_no_annotate_shard_NN`, sweeps the same corpus under two gate-local flavor
rows — plain `--annotate-fields=false`, and `--preserve-encodings=true --annotate-fields=false` —
with each row's other flags DERIVED from the `ALL_PROFILES` row it extends (`NO_ANNOTATE_FLAVORS`).
The rows are gate-local rather than `ALL_PROFILES` entries because that const is the shared
snapshot/matrix axis, and the flag's only effect is on emitted rust deserialize bodies: the emitted
`wasm/` tree is byte-identical with the flag on or off across every corpus fixture under both rows,
so this leg `cargo check`s `rust/` alone. It still generates `--wasm=true`, so
`--annotate-fields=false` is the ONE variable between its crate and the base leg's and a red cell is
attributable to the flag; it applies the same `CORPUS_DEF_SPLICE` seeding, honours the same
`COMPILE_SKIP`, runs the same two warning scans, and reads `EXPECTED_GENERATION_FAIL` under its row's
BASE profile name. The names contain `feature_corpus_compiles`, so `cargo test
feature_corpus_compiles` selects both legs; `feature_corpus_compiles_no_annotate` selects this one
alone. Measured on top of the base leg: +37 s cold (264 s → 301 s with `GATE_CACHE=0`), +14–19 s warm
(33 s → 47–52 s).

Two floors keep it honest. `NO_ANNOTATE_FLOOR_STEMS` names the four fixtures whose shapes are why
the leg exists (`bounds_spellings`' bounded `nint` member, `fixed_bool_member` and
`optional_fixed_member`'s encoding-less fixed members, and `c_style_enum_choice_arm`'s c-style
enum DATA type-choice arm plus group-choice sibling); the whole-corpus pin test asserts each is
still in `tests/corpus`, and the shard that completes the set asserts each was actually swept — so
neither pruning a fixture nor newly skipping one can silently delete the coverage.
`NO_ANNOTATE_KNOWN_RED` ledgers the cells whose rust crate does NOT compile under a flavor row
today, as `(stem, flavor, rustc error class, root cause)`, with a four-state verdict: listed + red
with the pinned class is expected; listed + GREEN fails as "the fix landed, retire the entry";
listed + red with a DIFFERENT class fails as "the failure changed shape"; unlisted + red is an
ordinary gate failure. The ledger is a finding, not an exemption, so EMPTY is its healthy state and
the machinery stays for the next red cell rather than being retired with the last one. The seam a
new entry is most likely to sit on is the one `records.rs` already flags in a comment: with
`--annotate-fields=false` there is no per-type scaffolding closure, so an emission that assumed one
— to re-shape a value, to isolate an inline binding, or to `return` early — lands in
`deserialize()` itself.

**The semantic floor beside it.**
`no_annotate_reframed_emissions_round_trip_non_canonical_wire_byte_exactly` generates a bespoke spec
covering the three emissions the flag re-frames (a c-style enum's inlined dispatch, an optional
field's `Option`-over-the-value-slot distribution, a map field's inner temporaries) and round-trips
non-canonical wire — indefinite lengths, non-minimal heads — through the built crate. It exists
because this seam's two failure modes are not the same failure: losing a frame is a build error the
compile floor catches, while losing an ENCODING compiles and is invisible. The map-field emission is
where that bites — its inner deserialize's working variables can shadow the arm's encoding
accumulators, so the trailing reassignments write the shadow and the outer value keeps its
`default()` — and typing the binding to silence the resulting inference error would leave the loss
in place. The non-canonical vectors fail against exactly that shape, measured.

All four sharded compile legs — the base corpus, its no-annotate sibling, the wasm matrix and the
multifile matrix — give every participating generated crate a deterministic per-cell package name
before the gate-cache tree hash and nested Cargo command. Cargo's root-package fingerprint omits the
manifest path, so same-named generated cells sharing one `CARGO_TARGET_DIR` could otherwise borrow a
fresh fingerprint and be reported `Finished` without compiling. For a cell that builds `wasm/` or
`wasm/json-gen`, its dependent manifest retains the source-level `cddl-lib` dependency alias but adds
`package = "<renamed rust package>"`, so Cargo resolves that alias to the renamed local rust package
without changing emitted Rust identifiers; each renamed manifest also fixes its original `[lib].name`
(`cddl_lib`, `cddl_lib_wasm`, or `cddl_lib_json_schema_gen`) so Cargo's package-derived default cannot
rename a self-reference. Each shard also pre-lays out a dependency-free,
gate/shard-uniquely-named `compile_error!` canary and checks it after its cell loop against the same
target dir. The expected red must fail *and* carry its exact marker in stderr: it is deliberately
uncached and old enough to exercise any false-fresh channel even when every real cell is a cache hit.

### Encoding-fidelity oracle (`--emit-tests` × `--preserve-encodings`)

The round-trip harness above feeds `from_cbor_bytes` only the generator's *own* canonical output, so
the "decode an irregular encoding and preserve it" direction — the whole point of
`--preserve-encodings` — went untested at scale (only the hand-picked `tests/golden_hex_preserve/`
KATs covered it). When both flags are set, each round-trip case now also runs an **encoding-fidelity**
block: a self-contained, deterministic CBOR mutator (`static/emit_tests_encoding_fidelity.rs`, spliced
into the emitted test module via `include_str!`) derives seven whole-tree irregular re-encodings of the
minted value's canonical bytes — `widen_step`/`widen_max` (non-minimal header widths), `widen_float`
(a major-type-7 float head re-encoded one IEEE width up, f16→f32→f64 — reachable since the `any`
(`AnyCbor`) mint deliberately includes a float head), `indef_containers`, `chunk_strings`,
`reverse_maps`, and `everything` (all composed) — and asserts each
decodes and re-encodes byte-identically. Whole-tree (not per-position) because a single dropped
encoding-capture fails the whole variant anyway; identity variants are skipped so the loop never
asserts vacuously. With `--canonical-form` also set it adds the canonical **differential** (every
encoding canonicalizes to the same bytes) plus a per-case canonical fixed point — the KATs stay the
spec anchor for *what* the canonical bytes are; this layer buys breadth. Types with user-supplied
`@custom_serialize`/`@custom_deserialize` are excluded (their wire format isn't the generated
serializer's). The emitted mutator ships a `#[test] encoding_mutator_self_check` pinning each mutation
class against hand-derived RFC 8949 bytes *and* pinning `variants()` end-to-end on two inputs — a
composite (int + string + map) and a float-carrying `[5, 1.5]`, the shape the `any` mint produces
(the vacuity guard). Executions: `emit_tests_execute` (local, with a fidelity-assertion floor),
`emit_tests_any_float_execute` (local — generates `tests/any-positions` under
`--preserve-encodings --emit-tests` and runs the crate, proving the `any` mint feeds a real float
head through `widen_float`), `emit_tests_open_struct_rest_execute` (local, the open-struct-map
sibling — generates `tests/open-struct-map-e2e` under `--preserve-encodings --emit-tests` and runs
the crate, proving the round-trip mint populates `.rest` through the generated API and the fidelity
classes — `widen_float` included, via the `any`-range composite — exercise captured rest entries),
`emit_tests_open_struct_ignore_execute` (local, the tolerate-and-drop twin — generates
`tests/open-struct-map-ignore-e2e` under `--emit-tests` non-preserve and runs the crate, proving each
`@ignore` type gets an ordinary `roundtrip_<type>` with no ignore-specific gating and mints into no
`.rest` map),
`emit_tests_bounded_map_key_execute` (local — generates `tests/emit-tests-bounded-key` under
`--emit-tests` non-preserve and runs the crate, proving a table key whose DOMAIN carries a value
window is minted inside that window through one canonical `FixedValue` CDDL-value projection; N64
candidate selection still uses cheap magnitude endpoints, while acceptance independently applies the
original value-space bounds), and
`feature_corpus_roundtrips_nondefault_profiles` (full tier, corpus × preserve breadth); the canonical
differential runs once at whole-program scale via the `canonical` fixture's `--emit-tests`.

#### Authoring standard for a bounded-domain emit-tests fixture

A green round-trip does not distinguish an INTENDED minted value from a wrong one that happens to
land inside the window, so a fixture over a bounded domain owes two things beyond "it passes":

1. **Both endpoints/signs of the window**, never one. The minter lays keys down from a base, and a
   base computed in the wrong space can satisfy one side of a window while violating the other — so
   a one-sided fixture certifies the defect in whichever direction it happens to point. Proven by
   the `nint` key rows in `tests/emit-tests-bounded-key/`: the identical wrong base made `.ge -5`
   fail loudly and `.le -5` pass on a garbage key (magnitude `-5 as u64`, wire value ~ -1.8e19,
   which does satisfy `<= -5`).
2. **A pin on the minted SPELLING, not just the round-trip verdict** — compared
   whitespace-stripped, so rustfmt line-breaking cannot quietly weaken it. This is what makes a
   wrong-but-passing mint fail.

Every primitive map-key coordinate is first projected to the canonical `FixedValue` it denotes.
The projection supplies fixed-key equality and independently checks each candidate against the
original value-space bounds; transformed N64 magnitude bounds choose inexpensive candidates only.
The bounded-key fixture pins N64 magnitude 0 beside fixed uint 0 and an ordinary uint collision
shift. Fixed nint record keys are deliberately rejected before generation, so the direct
magnitude-0-equals-nint--1 assertion is a focused unit control rather than a fixture row.

`--emit-tests-conformance` does not substitute for either: it validates minted bytes against the
source rule, and a degenerate-but-in-window value is genuinely spec-VALID.

Note which gate can carry such evidence. The snapshot corpus contains **no** emitted-test map-key
text (no `.snap` holds an `__i as …` key expression), so `snapshot_quick` — and therefore the whole
`fast` tier — cannot witness a change to key rendering; only the `*_execute` gates above run that
code. Cite one of them, not `check.ts fast`.

### wasm-crate test module (`--emit-tests` + `--wasm=true`, `src/emit_tests_wasm.rs`)

With `--wasm=true`, `--emit-tests` also emits a `#[cfg(test)] mod cddl_generated_wasm_tests` into the
generated **wasm** crate. It's a *second renderer* over the same `emit_tests::MintValue` derivation
surface the rust harness uses (the derivation is the single maintained thing; the two renderers —
rust-API strings vs wasm-wrapper-API strings — read from it). The teeth, per mintable type:

1. **Cross-crate byte differential** — build the value through the wasm wrapper ctor/`new_*` AND,
   independently, through the `cddl_lib::` rust ctor (the wasm crate path-depends on it), then assert
   `to_cbor_bytes()` is byte-equal. A wrong conversion in a wasm `new`/`new_<variant>`/`set_*` can't
   cancel here (the rust build is independent), so this catches the identity-`.into()`-where-a-transform-
   was-needed class — the exact wasm-boundary bug the compile gate can't see.
2. **Wire round-trip** — `from_cbor_bytes(bytes)` then `to_cbor_bytes()` byte-identical.
3. **Accessor read-back against emit-time literals** — primitive getters compared to the exact minted
   literal (not original-vs-back, which lets a wrong getter conversion cancel); enum `kind()`/`as_<var>()`
   pinned to the minted variant. One arm class reads back via `kind()` only: a nullable-payload arm
   (`opt = uint / null` as an arm) gets no `as_<var>()` self-readback, because that getter flattens
   `Option<Option<T>>` and reads `None` for the minted inner-null — the assertion would be
   unsatisfiable, not informative (read protocol: `docs/docs/wasm_differences.mdx` § nullable values;
   skip site: `emit_tests_wasm.rs`'s `nullable_payload`). Read on the freshly-*built* wasm value, not
   the post-wire one, so a wire-ambiguous choice (core's uint-`0` vs a fixed `i0` variant) can't
   false-fail.
4. **Boundary acceptance only** (`wasm_bounds_<type>`) — the accepted boundary value constructs
   (`.ok().is_some()`). The beyond-boundary REJECT direction is deliberately **not** emitted: a wasm
   ctor's error path builds a `JsError` through a wasm-bindgen import that panics under host `cargo
   test`; rejection is already pinned as `RangeCheck` on the wire by the rust `--emit-tests` module, so
   this half only confirms the acceptance plumbing.

wasm-API idioms baked in: `JsError: !Debug`, so a wasm `Result` is unwrapped `.ok().expect(..)`, never
`.unwrap()`; composite ctor params cross as `&Wrapper`; c-style enums cross by value; every
`@newtype`/tag/bounded wrapper exposes a wasm `new(inner)` ctor (`Result`-returning when the bound makes
it fallible) plus an inner-value getter (`get`, or the `@newtype <name>` rename), so a wrapper ENTRY type
is built through that public `new` — its minted inner rendered by the same ctor-arg machinery and (for a
primitive inner) read back through the getter against the minted literal. A wrapper CTOR ARG is instead
built via its `From<cddl_lib::Native>` impl (a convenience — the wrapper's own `new` is covered by its
top-level entry test); if the inner has no wasm build the entry type falls back to
decoding the rust twin's bytes with a loud skip of the ctor differential. A wrapper COLLECTION arg
(`FooList`/`FooMap`/`&Nums`) is a `new`/`add`/`insert` block expression. **Loud skips (never silent):**
a ctor arg with no wasm build (a name-erased wrapper collection, a `Fixed`/`Alias`/`any` inner) and
the same-class wrapper-entry ctor differential, plus the whole module under any
`--wasm-*-macro` flag (those replace the wrapper method surface) — each a `crate::warn!` to stderr.

The tagged-`any` fallback still renders its independently minted rust twin through the private
`__AnyCborMint` spelling. The module therefore imports
`cddl_lib::any_cbor::AnyCbor as __AnyCborMint` only when the finalized IR uses `AnyCbor`, keeping
any-free output byte-identical. `expected_conversion_wasm_emit_tests_import_the_rust_any_mint`
pins the import directly; the ordinary `cddl-matrix/verify.ts` wasm round-trip leg is the systematic
execution catcher — before the import was added, all three `eb64url`/`eb64legacy`/`eb16` rows failed
to compile with E0433.

**Extern / raw-bytes never reach this renderer at all**, so their skip is the RUST half's, not this
one's: the shared minter (`emit_tests::mint_struct`) produces no `MintValue` for
`RustStructType::Extern` (other than the reserved `Int`) or `RustStructType::RawBytesType`, so a type
with such a ctor arg — or a wrapper around one — fails to mint upstream and is dropped with the rust
half's own loud "not cheaply mintable" warn. Of the two `emit_tests_wasm.rs` arms that named the
class, `wasm_named`'s is a variant-specific backstop nothing can reach (kept as the site a future
extern-minting change must teach); the wrapper-entry from_cbor_bytes fallback IS live, but for a
different cause — an inner the rust minter can mint while the wasm renderer can't express it,
verified on `#6.42(any)`, whose `AnyCbor` inner has no value-destructuring wasm ctor. (Optional-nullable
flatten points need no skip: optional fields are not ctor args, so no mint constructs a present-null
state — the three-state surface is covered by the hand-written `tests/nullable-wasm/` fixture.) The
macro-mode skip is a **decided posture, not a gap** (2026-08-03): those flags replace the wrapper
method surface with user-supplied macro definitions, so an emitted assertion there would judge the
fixture's macro bodies rather than the generator's output — the compile verdict is the honest floor,
and the observable that would reopen it is a consumer-reported behavioral defect in a macro-mode wasm
surface that the compile verdict passed. The extern / raw-bytes non-mint is the **same decided
posture** (2026-08-04, completing the class): what was decided is whether the AUTO-MINTER learns
these classes, NOT whether the feature is tested — hand fixtures cover raw-bytes behaviorally on
both sides (`tests/raw-bytes/tests.rs`, `tests/raw-bytes/tests_wasm.rs`, and the extern-generic
fixtures round-tripping `from_raw_bytes`). An extern type carries no contract a minter could
construct against, and raw-bytes has a knowable trait door (`RawBytesEncoding::from_raw_bytes`) but
no knowable accepted LENGTH, so an emitted mint would be runtime-red against a correct generator;
the once-considered def-file mint hook is impossible harness-side (the mint decision is taken
DURING generation off the IR struct variant, before any def splice runs — the falsification
`6ce3b6e0` records), and the feature-shaped alternative was declined (full record + reopening
signals: `cddl-matrix/roadmap.toml` § "Explicitly out of scope (decided, not overlooked)").
Mutation-verified
red-first (three `generation/` wasm-boundary mutations each turned exactly the intended assertion class
red; see the `src/emit_tests_wasm.rs` header).

Two consumers run it:
- **`integration_tests::emit_wasm_tests_execute`** (default suite, ~10s) — generates the rich `core`
  fixture `--wasm=true --emit-tests=true` and `cargo test`s the **wasm** crate (alongside the
  hand-written `tests_wasm.rs` as a plausibility cross-check), with emitted-test count floors. It
  `cargo test`s only the wasm crate: `core` is not `--emit-tests`-clean on the *rust* side (two
  hand-written source-inspection tests truncate `lib.rs` at the first `#[cfg(test)]`; its
  wire-ambiguous `TypeChoice` also tripped the value-equality oracle until the emitted round-trip
  learned the first-match assertion), but the wasm crate builds the rust
  crate as a *non-test* dependency, so none of that compiles here.
- **`integration_tests::wasm_matrix_roundtrips`** (`#[ignore]`d, manual — the round-trip upgrade of the
  wasm-ABI matrix compile gate, swept across `ALL_PROFILES` minus the component row — so default /
  preserve / json; see that section below).

Run the manual gate with:

```sh
cargo test --bin cddl-codegen wasm_matrix_roundtrips -- --ignored   # ~8-10 min (258 cells x 3 profiles)
```

### IR-bug conformance oracle at breadth (`--emit-tests-conformance` + `integration_tests::ir_conformance_corpus`)

The round-trip harness mints its values from the **same IR** as the code under test, so an IR-level
miscompile — a bound or member computed wrong at *parse* time — mints a spec-violating value and then
asserts it round-trips green (encoder and decoder agree with each other, both from the same bad IR).
Illustrative shape of the class (now fixed): `tests/corpus/exclusive_range.cddl` (`[v: 0...10]`) once
mis-computed the exclusive upper bound, so the minter minted `v = 11` (spec max valid = 9) and the
round-trip passed anyway — an IR-level bound bug invisible to the round-trip harness.

`--emit-tests-conformance` closes that residual. When on, each emitted round-trip case gets one extra
line right after its bytes are computed: `cddl_conformance::validate(&bytes, "<rule>")`, validating
the minted bytes against the type's **source `.cddl` rule** via the `cddl` crate's independent
decode+constraint path (the same validator, and the same shared helpers, as
`deser_test_conformance.rs` — the emitter reuses them, it does not duplicate the validator). The
Rust type name is mapped back to its source rule via `convert_to_snake_case`, gated on the ident
being a real top-level rule (`IntermediateTypes::is_toplevel_rule`) and on the reversal round-tripping
faithfully — a synthesized struct or a lossy name gets no call.

**What it proves / can't.** A conformance failure is a strong signal (our bytes violate the spec the
generator was built from). Same caveats as `deser_test_conformance.rs`: it shares the dcSpark fork's
*parser* with the generator, so it catches wrong **values**, not fork-level misparses; and the minted
values are shallow (None arms, empty tables, depth-capped recursion), so it's breadth across fixtures,
not exhaustive per-type depth. One exception to the degenerate baseline: for a CBOR tag whose RFC 8949
content the validator *semantically* enforces (tag 0 = tdate must be an RFC 3339 date-time), the minter
emits a fixed valid literal instead of the generic `"a"` — otherwise a spec-violating baseline would
round-trip byte-identically yet be (correctly) rejected by this oracle. Only tags the validator actually
enforces get a constant (`semantic_tag_content` in `emit_tests.rs`); every other tag mints the baseline.

**The gate** (`integration_tests::ir_conformance_corpus`, `#[ignore]`d — **manual/local only**, kept
out of even the local tier's `cargo test` because it adds the heavy `cddl` dep to every corpus crate):

```sh
cargo test --bin cddl-codegen ir_conformance_corpus -- --ignored --nocapture   # ~1 min
cargo test --bin cddl-codegen rust_oracle_fingerprint -- --ignored --nocapture # preflight only
cargo test --bin cddl-codegen ir_conformance_multifile -- --ignored --nocapture # directory-input leg
```

Before the corpus loop, the gate generates a tiny `fingerprint_probe` crate under the same scratch root,
injects `CDDL_ORACLE_DEP`, and executes every shared fingerprint probe through the exact parser and
validator entrypoints the conformance oracle trusts. A mismatch panics with the failing probe names,
the same recovery guidance as the matrix verifier. Then, for every
`tests/corpus/*.cddl`, it generates with `--emit-tests --emit-tests-conformance`, appends
`CDDL_ORACLE_DEP` + the shared oracle helpers, copies the fixture in as
`cddl_conformance_source.cddl`, and `cargo test`s the crate under one shared `CARGO_TARGET_DIR` (so
`cddl` compiles once). The scratch root is keyed by checkout path and wiped at start, so the gate holds
an advisory lock (`acquire_scratch_lock`) for its whole run: a second invocation from the same checkout
waits for the first (printing a grep-stable "waiting for it to finish" message) rather than
`remove_dir_all`ing its crates mid-run — same-checkout concurrent runs serialize while the shared target
cache is preserved. The fixture loop uses curated ledgers, each empirically justified:

- **`EXPECTED_FAIL`** — fixtures with a known IR bug whose minted value the oracle *must* reject. Their
  `cargo test` must fail **and** the output must carry the oracle's distinctive message (so it failed
  for the right reason). An expected-fail fixture that *passes* turns the gate RED ("IR bug apparently
  fixed or oracle lost teeth — investigate, then remove from `EXPECTED_FAIL`"). Empty whenever no
  corpus fixture mints a spec-violating value; the machinery stays armed — the next IR-level bug's
  fixture will trip this list. Its last resident, kept here as the illustrative case, was
  `exclusive_range` (`[v: 0...10]`): the validator rejected the minted `11` as out of range
  `0 <= value < 10`, and it was removed once `parsing.rs` was corrected to emit `max = b-1` and the
  minted value became in-spec.
  `inline_group` (`[(uint, tstr)]`) and `occurrence` (`[+ uint]` / `[2*5 uint]`) are earlier
  residents' siblings that never joined the list: both are **fixed at HEAD** (inline_group emits a
  2-field struct that reads 2 elems; finite/non-zero occurrence bounds now live on the ARRAY type's
  `BoundedVec::try_from` door, which generated tests probe directly before passing accepted values
  into infallible record/type-choice/group-choice constructors, while minted round-trips exercise
  the same door on decode — the bounds were once misread as element VALUE bounds).
- **`GEN_SKIP`** — fixtures excluded from the sweep for a concrete *validator/minter* gap
  (never to hide a real bug): `dsl_custom` (references user-supplied code, can't compile
  standalone). `sized_int` is a past resident, off the list twice over: its negative-lower-bound
  range stopped being a validator gap at the fork's `885c61c` non-uint-range fix, and its
  `int .size 8` member was dropped when cddl-codegen made `.size`-on-signed-`int` a graceful
  rejection (per the RFC author's clarified semantics — cbor-wg/cddl#32 — the construct means the
  `uint .size` window, which the old `i{8N}` mapping mis-enforced; the rust validator's hard error
  on it remains an upstream over-rejection gap, scoreboard in
  `cddl-matrix/upstream-reports/rust-cddl-size-on-int-divergence.md`).

Outside `EXPECTED_FAIL`, every generated-suite failure turns the gate RED with the minted bytes +
rule named. `GEN_SKIP` fixtures do not run; fixture-level `RUST_ORACLE_SKIP` disables only the rust
validator half; and a per-rule exemption neutralizes only its listed exact call — every unledgered
call and every other test surface remains red. A vacuity floor asserts a nonzero number of fixtures
actually emitted a conformance call, so a silent no-op sweep can't pass.

**Which rules get rooted at all.** Both oracle halves share one seam —
`emit_tests.rs::conformance_rule_name` — and it roots every top-level rule EXCEPT a bare-GROUP rule
(`inner = (a: uint, b: uint)`): a group is a reusable fragment, not a validatable instance type, so
both oracles reject any bytes offered against it *by design* (the ruby gem always did; the rust
fork's validator since its `773b723` array-sequence rewrite). Such a rule gets NO conformance call
and NO minted-bytes dump — a design exclusion, not a ledger entry, which is why its absence never
shows up in `DUMP_EXEMPT`. Its *embedders* stay fully judged: `tests/corpus/nested_group.cddl`
pins the shape (`inner` unrooted, its array-splicing sibling `outer` rooted by both halves).

**Decorrelated (ruby `cddl` gem) second oracle.** The rust oracle above shares the dcSpark fork's
parser with the generator, so a **fork misparse** (which corrupts generator IR and the oracle's spec
reading identically) mints well-formed-but-spec-wrong bytes and passes green. To catch that class this
gate re-validates the *same* minted bytes through the ruby `cddl` gem — the RFC author's reference
tool, sharing no parser, decoder, language, or lineage with the fork. The bridge is a dump hook: with
`--emit-tests`, when `CDDL_CODEGEN_DUMP_MINTED` names a directory each round-trip case writes its bytes
to `<rule>__case<i>.cbor` (pure `std`, inert when the var is unset, no CLI flag — see
`emit_tests.rs::roundtrip_body`); the gate points it at a per-fixture dir, then sweeps in sorted order,
invoking `<gem> <synthetic-rooted-spec> validate <case.cbor>` (the gem targets a spec's first rule, so
the same `__cddl_oracle_root = <rule>` trick aims it). The gem is **harness-side only** — never a crate
dep, so shipped output stays ruby-free. Teeth and posture:

- **`RUBY_EXPECTED_FAIL`** — `(fixture, rule, reason)` triples the gem diverges on for a documented,
  non-bug reason (a gem construct gap the fork legitimately supports). Three at HEAD —
  `(cbor_wrapped_group_array, holder)` and `(cbor_bignint_table, holder)`, both the gem's
  inline-composite `.cbor`-controller parse gap (exit 65 poisons the whole spec;
  `cddl-matrix/upstream-reports/ruby-cddl-inline-composite-control-arg.md`), and `(assignt_extend_ext_first,
  ext_first)`, the gem's extension-first `/=` chain crash ("Duplicate rule definition", a parse
  RuntimeError that equally poisons the whole spec — the committed paste-ready report is
  `cddl-matrix/upstream-reports/ruby-cddl-ext-first-incremental-chain-crash.md`, and the entry's
  reason carries the conformance-transfer argument for the ordering, since that fixture's rust
  half rides `RUST_ORACLE_SKIP` too). Ledgering is **per (fixture, rule)**, not per
  fixture: a fixture may have one rule the gem can't judge while its *other* rules must still be
  sound. A divergence is *signal*: an unledgered one is either a gem gap to record here **with a
  reason**, or — the class this oracle exists to catch — a fork misparse minting spec-violating bytes.
  **Investigate before ledgering.** A ledgered `(fixture, rule)` that stops diverging while still
  being swept turns the gate RED (stale entry), mirroring `EXPECTED_FAIL`.
- **`GEN_SKIP` vs fixture-level `RUST_ORACLE_SKIP` vs per-rule `RUST_ORACLE_RULE_SKIP`** — three
  deliberately different scopes. `GEN_SKIP` (e.g. `dsl_custom`) can't
  be generated standalone at all, so it's skipped entirely. `RUST_ORACLE_SKIP` holds fixtures with a
  *rust*-validator gap that still generate, round-trip, and dump fine: they are generated **without**
  `--emit-tests-conformance` (rust validate half off) while their minted bytes are **still** swept by
  the ruby gem — a rust-validator blind spot must not cost the decorrelated oracle its coverage.
  Four at HEAD (each entry's comment cites its gap): `alias_positions` and `open_table` on the
  named-rule map-key over-rejection (gap #11), `assignt_extend` and `assignt_extend_ext_first` on
  the referenced-`/=`-chain arm drop (gap #15) — the latter fixture is on BOTH ledgers, so
  neither cddl oracle judges it and its conformance rides the equivalence pin's byte-identity
  transfer to the folded spelling (the fixture's own header comment and the ledger reasons say
  so). A past resident, `cbor_bignint_table`, came off the list when the fork's bignum map-key
  fix shipped (`cddl-matrix/README.md` § "Upstream oracle gaps" gap #6; its ruby half is
  separately on `RUBY_EXPECTED_FAIL` above, so it keeps the decode-side reference-codec
  differential AND the rust conformance half as its checks).
  `RUST_ORACLE_RULE_SKIP` is intentionally narrower: normal conformance generation remains ON and
  the scratch generated module neutralizes only the exact emitted
  `cddl_conformance::validate(&bytes, "undefined_value");` call for
  `(fixed_singletons, undefined_value)`. At pinned dcSpark/cddl
  `ac1b98ec07184236517da4511b1bbea239e35190`, valid `x = undefined` bytes `f7` reject with
  `expected type undefined, got Null`. The cost is exactly **one of fixed_singletons' eight** rust
  validator calls; its other seven calls, every ordinary round trip, minted-byte dump, ruby sweep,
  and reference-codec differential remain enforced. Ciborium's generic value model collapses `f7`
  to null while minicbor preserves undefined, so that differential normalizes this one
  representational discrepancy after both codecs fully consume the bytes; its `f7` self-check keeps
  that accommodation explicit. Fixture/rule existence, a nonempty reason,
  pair uniqueness, one-or-more exact matches, no target left behind, and unchanged unledgered-call
  count are all guarded. The same scratch preflight directly asserts that reject signature, so an
  acceptance or signature change fails loudly: remove or re-investigate the one-rule ledger rather
  than broadening it to the fixture. The same ledger also has
  `(preserve_pair_map_self_encoding, holder)`: at the pinned revision the valid rooted holder
  `[{}]` (`81a0`) for `{ * entry => entry }`, where both domains resolve to named composite arrays,
  returns `expected array type, got Map([])`. Only that call is neutralized; standalone `entry`,
  ruby, dumps, and structural checks remain live (`self_map` is transparent). A separate
  preflight asserts the exact returned signature, so acceptance or a signature change fails with
  instructions to investigate/remove the new per-rule skip after upstream repair.
- **Dump-coverage (`DUMP_EXEMPT`)** — per fixture, every rule the generator *intended* to dump (its
  hook is present in `lib.rs`) must land a `.cbor` on disk. An intended-but-undumped rule fails the
  gate unless ledgered in `DUMP_EXEMPT` **with a justification** — so a dump hook that silently stops
  firing (or a lossy rule name dropping a top-level rule from the sweep) is visible per fixture, not
  only via the corpus-wide case floor. Empty at HEAD: source rule names are always recoverable.
- **Negative control** — after the sweep, one known-good case is truncated (final byte dropped =
  guaranteed malformed) and the gem *must* reject it; a gem invocation that exits 0 regardless of input
  can never pass the gate.
- **Case floor** — a minimum total swept-case count, so a dump hook that silently stops firing fails
  rather than shrinking to a vacuous no-op.
- **Gem REQUIRED (opt-out `CDDL_RUBY_ORACLE=skip`)** — gem discovery mirrors `verify.ts`'s
  `resolveRubyCddl` (`RUBY_CDDL` env pin, fail-loud if the pin is bad; else the gem-dir probe — never
  `$PATH`/`which cddl`, which is the unrelated *rust* `cddl`). The decorrelated oracle must not
  silently, permanently degrade to a no-op just because a machine lacks the gem, so **a missing gem
  FAILS this gate** with install instructions (`gem install --user-install cddl`). To run the gate
  without the decorrelated half — accepting the fork-misparse class goes uncovered — set
  `CDDL_RUBY_ORACLE=skip`, which prints the grep-stable `RUBY ORACLE: SKIPPED (...)` marker and runs
  only the rust + dump-coverage halves.

**Decode-side reference-codec differential (CDDL-blind, dependency-free).** Both cddl oracles above
prove our bytes match the *spec*; neither is a raw structural decode. Piggybacking on the same dumped
`.cbor` files, this gate also decodes every minted case through **two independent CBOR codecs**
(`ciborium` and `minicbor`, harness-side dev-deps — `minicbor` is used nowhere else in the pipeline,
which is its decorrelation value) and requires both to fully consume the bytes (no trailing garbage)
and agree on the decoded structure. What it proves: two decorrelated decoders structurally agree on
our output — a well-formedness regression a spec validator wouldn't see (a validator can accept bytes
a raw decoder chokes on, or vice-versa). What it can't: nothing about spec conformance — that's the two
cddl oracles' job. It has no external dependency, so it runs for `RUST_ORACLE_SKIP` fixtures and even
under `CDDL_RUBY_ORACLE=skip`, with its own case floor (`DIFF_CASE_FLOOR`) and a truncation negative
control (a malformed case must fail both codecs). Two representational discrepancies are canonicalized
only after both codecs fully consume the bytes: RFC 8949 §3.4.3 bignum tags 2/3, which `ciborium`
folds into integers and `minicbor` leaves as `Tag(2/3, Bytes)` (our `biguint`/`bignint` prelude
types), and `undefined` `f7`, which ciborium's generic value model collapses to null while minicbor
preserves as undefined. The differential's hand-derived self-checks pin both accommodations, so only
a genuine structural divergence turns the gate red.

**The DIRECTORY-INPUT leg** is the sibling gate `ir_conformance_multifile`, and it asks this oracle's
question of an input the corpus cannot express: every `tests/corpus/*.cddl` is a single file, so the
multi-module emission — the generated test module minted at the crate's generated ROOT, naming
submodule types bare through `use super::<scope>::*;` globs — sat outside the oracle's reach.
(`emit_tests_multifile_scope_imports` pins that emission in-process; this gate executes it with the
oracle on.) It generates the committed placement cell `tests/matrix_multifile/struct__named` with
`--emit-tests --emit-tests-conformance`, wires the same `CDDL_ORACLE_DEP` + shared helpers, and hands
the oracle a spec that is the **concatenation of the input tree's `.cddl` files** in sorted
relative-path order — rule names are global across a multi-module input, so concatenating is lossless
and its order does not change meaning, which is the contract `docs/docs/command_line_flags.mdx`
states for the flag. Its vacuity guards are the multifile-specific pair: the generated root module
must carry the scope glob for the non-root module, and must validate a rule *defined in* that module
— plus a stale-pin guard, since the cell is committed and shared. Same `#[ignore]`d manual posture
and the same first-fetch network need as its sibling; one cell and one crate, so solo iteration is
seconds rather than the corpus sweep's minute. It does NOT re-run the fingerprint preflight (that is
`rust_oracle_fingerprint`'s own gate) and it has no ruby half — breadth and decorrelation stay the
corpus gate's.

## Declared-type spelling (`src/tests/declared_spelling_tests.rs`)

The rule (user doc: `docs/docs/output_format.mdx` § "Type spelling at member positions") is that a
type-DECLARATION position naming a member's type spells it as declared, keeping the outermost alias
ident. It needs its own suite because the rule is a property of many emission paths at once: one
function spells member types (`ConceptualRustType::for_rust_member_ct`, which keeps the alias), so
every resolved spelling in the output comes from a caller that resolved for STRUCTURAL DISPATCH and
then reused the dispatch-normalized value as a naming input. Nothing about that is visible at any one
site, which is how the emission paths drifted from each other.

- **The ordering guard** — `preserve_table_member_keeps_positional_encoding_sidecars`: a
  `@duplicates preserve` named table referenced by a record member keeps POSITIONAL (`Vec<..>`)
  encoding sidecars, asserted at all three sites that express that one decision (the encoding-struct
  field DECLARATION, the deserialize-side `Vec::new()` construction, the serialize-side `.get(i)`
  positional read). This is the guard on the ORDER of the spelling change: un-resolving an
  encoding-field caller reaches `encoding_fields_impl`'s `Alias` arm, which must thread the OUTER
  `RustTypeSerializeConfig` — the `Map` arm reads `cfg.duplicates` to choose positional over
  key-VALUE-keyed. An `Alias`'s inner is a bare `ConceptualRustType` with no config of its own, so
  recursing through `(&**ty).into()` DEFAULTS the config and drops the policy, turning the sidecar
  into a `BTreeMap` that structurally cannot hold the repeated keys the table exists to round-trip.
  A wire-behaviour skew wearing a respelling's clothes is exactly what a large bless diff hides, so
  it is pinned rather than reviewed.
- **Cross-path agreement at every depth** — `declared_field_and_rest_row_sidecars_agree_at_every_depth`:
  a declared map field and an open-struct rest row over the same alias spell it the same way. The
  depth axis is the load-bearing part. The two paths agreed at depth 1 (the rest row's key domain was
  never resolved, so it kept its alias by accident) while disagreeing one level down INSIDE THE SAME
  TYPE EXPRESSION — a rest row over a container-typed value spelled
  `BTreeMap<Epoch, (.., BTreeMap<Vec<u8>, StringEncoding>, ..)>` against a data field typed
  `OrderedHashMap<Epoch, OrderedHashMap<PolicyId, String>>`. A depth-1-only pin passes over exactly
  that, which is why this asserts both and additionally asserts that NO sidecar names a structural
  target (`BTreeMap<Vec<u8>,` / `BTreeMap<u64,`).
- **The multi-scope route** — `cross_scope_alias_is_spelled_and_routed_in_the_referring_scope`
  (source) plus `integration_tests::declared_spelling_cross_scope_encoding_crate_compiles`
  (compile). With a directory input the alias can be declared in a different module than the record
  referring to it, so `<scope>/cbor_encodings.rs` names an ident from another scope, reached through
  its own `mod.rs` plus `use super::*`. The compile leg is not redundant: a missing route is
  E0412/E0433, which no source assertion is guaranteed to see. It runs `--wasm=false` because a
  cross-scope NESTED-map wrapper fails to emit its wasm class (E0425) — pre-existing, reproducible
  with every `resolve_aliases` call restored, and unrelated to spelling; a cross-scope FLAT map
  wrapper compiles.
- **Wasm wrapper names** — `wasm_collection_wrapper_names_keep_the_declared_alias`: wrapper names are
  keyed by STRUCTURAL identity and are deliberately outside the rule, but they were already minted
  from declared alias idents before the rule existed, and changing them in either direction is a
  consumer-visible wasm API break. Pinned so a later "make it uniform" pass has to argue with a test.

The in-process cells (`api::generated_strings` source assertions) and the compile cell are all
`local`-tier: `fast`'s only cargo TEST invocation is `cargo test --bin cddl-codegen snapshot_tests`.

The corpus-side companion is `tests/corpus/alias_positions.cddl` — one aliased member exercised at
every naming position at once (data field, ctor param, alias target, sidecar index key at both rest-row
depths, deserialize call target, wasm wrapper name), so a re-resolution at any single path shows up as
a snapshot diff on a fixture whose whole purpose is this rule. Its committed snapshot is also the diff
surface for the call-target position, which stays resolved for now.

## Static-runtime property layer (`src/tests/any_cbor_tests.rs`)

The `AnyCbor` self-describing CBOR value type (`static/any_cbor_preserve.rs` + its two
per-assembly serialize fragments, `static/any_cbor_non_preserve.rs`) is hand-written runtime
code whose contract — byte-identical re-serialization of ANY well-formed CBOR item under
`--preserve-encodings` — no snapshot can judge, so it gets a dedicated property layer instead
of fixture coverage. `src/tests/any_cbor_tests.rs` `include!`s the static files into one shim
module per static assembly (non-preserve / preserve / preserve+force-canonical — the same
technique the fidelity mutator uses via `include!` in `integration_tests.rs`) and runs under
plain `cargo test` (`cargo test --bin cddl-codegen any_cbor`; in-tier via the local tier's
workspace `cargo test`, no nested cargo, no dedicated gate). Two further shims
(`json_non_preserve` / `json_preserve`) additionally compile the `static/any_cbor_json.rs`
serde fragment and pin BOTH JSON surfaces. The **tagged value codec** (`AnyCbor`'s own serde,
total): the exact rendering table (`docs/docs/output_format.mdx` § "The `AnyCbor` value codec"),
`from_json(to_json(x)) == x` for the non-preserve variant over finite floats,
value-equal-modulo-encodings for preserve, and the read-side tolerance/error cases. The
**natural-fallible walk** (`to_natural_json`/`from_natural_json` — the PRIMARY rendering every
*generated* type routes an `any`-typed value through): one shim per success row and per
strict-fail row, the RFC 8949 §6.2 read conventions (lexical numbers, prefer-numeric keys,
the full-nint-domain key reading), and the round-trip law where `to_natural_json` succeeds.
At corpus breadth (`well_formed + appendix-A + seeded-random`), `natural_json_law_over_corpus`
asserts the strong biconditional both directions — Ok ⟹ JSON fixed point + value equality +
no failure node, Err ⟹ the input contains one — against `contains_natural_json_failure_node`, an
INDEPENDENT recursive failure-set oracle written against the inspection API (vacuity-floored
on both branches); the tagged codec gets matching corpus laws, and
`hostile_deep_json_read_is_graceful` pins that a 2000-deep JSON read is a graceful serde_json
`Err`, never a stack abort. The corpus explicitly includes map-as-key, tag-as-key,
array-as-key, duplicate-key, and near-depth-limit items.

The core assertion is a **span oracle**: deserialize one item, recover its true byte extent
from `Deserializer::position()` diffs, and require `serialize(deserialize(span)) == span`
byte-identically (preserve variant) / value-fixed-point (non-preserve variant), plus
canonical-serialization fixed points and equal-value-different-encoding → identical canonical
bytes. Corpus: the RFC 8949 appendix-A vectors (`cddl-matrix/sources/appendix_a.json`; one
principled skip — `f818`, `simple(24)`, is ill-formed per RFC 8949 §3.3), hand vectors per
`Sz` width, NaN payloads at every float width (the fork-supplied `float_sz` fidelity), mixed
chunk-width indefinite strings, duplicate map keys (identical AND differently-encoded),
malformed/truncated prefixes (must `Err`, never panic), and seeded-PRNG random structural
values for both variants — the seed prints on failure, so a red run reproduces by pasting the
seed.

Since A2, generated crates whose spec uses `any` also compile these files — the usage-gated
`any_cbor` module assembled in `export.rs` (`--export-static-crate` exports it always, as a
pure function of flags) — so this layer is no longer their only coverage; it remains the only
layer that judges the byte-exactness contract itself. The depth guard is wired through the
single `read` recursion seam via an includer-supplied `any_cbor_recursion_guard!()` macro:
the three per-assembly shims here define it as a no-op, and a fourth `depth_guard` shim
includes the guard runtime with a small baked limit and pins the at/under/over-limit vectors
(over errors `DeserializeFailure::DepthLimitExceeded`, no SIGABRT), with an e2e counterpart
(`integration_tests::deserialize_depth_limit_guards_any_member`) proving a generated
`--deserialize-depth-limit` crate rejects a pathologically deep value in an `any` position.

## json-gen helper runtime (`src/tests/json_schema_gen_tests.rs`)

`static/json_schema_gen.rs` holds what a generated `wasm/json-gen` crate imports from the rust
runtime crate: `Registrar` (the row registrar — one `reg.add::<T>();` per row, owning the
published-name ledger and delegating each row to the `add_schema` helper beside it, which stays
public for a hand-written row) and `check_schema_ref_closure` (the document's reference-closure
check). It also holds the two items a *consumer's own* code calls rather than anything emitted:
`custom_schema_impl!`, the macro writing the `schemars::JsonSchema` impl a hand-authored schema body
needs, and `custom_schema_body` under it. All of it runs in a *consumer's* `cargo run` of their
json-gen crate, so without this layer its only compile proof is a nested-cargo run — a local-tier
cost to catch a syntax error, and no lint sees it at all.

`src/tests/json_schema_gen_tests.rs` `include!`s the file into one shim module, the same technique
`any_cbor_tests` / `ordered_set_runtime_tests` use. That buys:

- **Lint and compile coverage in the fast tier.** `cargo clippy --workspace --all-features
  --all-targets` reaches the bin crate's test binary, so the shipped helper is linted like any other
  code. (`schemars` is a `[dev-dependencies]` entry of cddl-codegen for exactly this shim; it is
  never a dependency of the tool itself.)
- **`Registrar` proven to be a pure re-spelling of the row.** Every emitted row goes through it, so
  the vectors assert what actually ships: that a registrar-driven registration builds the same
  `$defs` map a direct `add_schema` call with a hand-threaded ledger does, that the ledger still
  fires through `reg.add` (a `Registrar::add` that reimplemented the row without the guard would pass
  every other vector here), and that a second registrar starts with a fresh ledger — the same scope
  the ledger had as a local of one crate's `add_schemas`, and what keeps a cross-crate collision its
  blind spot rather than a new false positive.
- **Real unit vectors for `decode_schema_ref_name`**, the inverse of a schemars-private encoder,
  which is reachable no other way. The vectors cover both escape layers (percent / JSON-Pointer),
  the truncated- and non-hex-escape passthroughs, and the ORDER the two layers must be undone in: a
  name holding a literal `~1` encodes to `~01`, so `~1`-before-`~0` is the only order that recovers
  it. `tests/json-extern` carries the end-to-end private-encoder proof of that order alongside
  multi-byte UTF-8; the shim's unsafe-name regression proves the emitted kept-its-own-name guard
  compares the decoded assigned ref rather than skipping it. `check_schema_ref_closure` gets the
  accept path (including a definitions namespace that is not `#/$defs`, and an unresolvable one,
  which must SKIP rather than fail) plus both failure classes.
- **Both arms of `custom_schema_impl!` expanded.** A macro body is text until something expands it,
  so the shim invokes each arm over a committed JSON file under `tests/json-schema-custom/unit/` —
  which also asserts that `include_str!` resolves relative to the INVOKING file rather than to
  `static/json_schema_gen.rs`, since a resolution against the latter would not find them. The
  reference retarget under it gets vectors against `custom_schema_body` directly, including the case
  the tool's own emitted generator can never reach: a generator whose `definitions_path` is not
  `/$defs` (`export_schemas()` always builds a default one, but `add_schemas` takes the generator as
  a parameter). The deliberate non-rewrites — a bare name, an `http(s)://` URL, a pointer into
  another document — have their own vector, because those are exactly what `check_schema_ref_closure`
  exists to report.

Runs under plain `cargo test` (`cargo test --bin cddl-codegen json_schema_gen_tests`) — no nested
cargo, no dedicated gate. The end-to-end proofs stay where they were: `integration_tests::
json_schema_name_merge_fails` / `..._stolen_fails` / `json_schema_ref_dangling_fails` run a real
json-gen crate and assert it panics, and `snapshot_tests::json_gen_extern_schema_rows` pins that the
generated crate still *imports* the helpers rather than inlining copies of them.

**`custom_schema_impl!` in a real generated crate** is `integration_tests::
custom_schema_impl_writes_a_closing_document` over `tests/json-schema-custom`, and it is the only
layer that can see the macro's placement contract: the shim above proves the expansion COMPILES, not
that `$crate::json_schema_gen::…` resolves where the tool actually puts the module. The cell
generates the fixture, installs the committed hand-owned module (`hand/custom_ext.rs` plus the schema
file it includes) at `rust/src/`, declares it from the SEED-ONCE `rust/src/lib.rs` — the only home
the orphan rule leaves, and outside the `src/generated/**` every run clobbers — then `cargo run`s the
json-gen crate and reads the document. `cargo run` rather than `cargo check` because the closure
check runs when the document is built. Scope: the in-crate layout only, where the expansion resolves
through the seed-once root's `pub use generated::*;`; nothing in-tree compiles the
`--common-import-override` arrangement, where the macro's crate and the invocation's crate differ.

## wasm-ABI matrix (`tests/matrix_wasm/` + `integration_tests::wasm_matrix_compiles`)

A **coverage-by-construction** gate for the generated wasm-bindgen bindings: it compiles the wasm crate
for every cell of a `{wasm-ABI type-shape} × {boundary role}` grid, so any cell whose bindings don't
type-check is a specific red cell. It exists because the wasm ABI — accessor return types, boundary
`.into()`/`.clone()`/by-ref conversions, map typedefs — is a concern the CBOR-serialization suites don't
compile-check by construction: the rust crate can type-check while the generated wasm crate does not, so
without an enumerated gate that class of bug is only caught by whichever fixtures happen to hit it. Gating
the whole grid makes the coverage systematic instead of incidental.

Coverage equals **both** hand-curated axes — the `SHAPES` type-shape list and the `ROLES` boundary-role
list: a wasm representation not in `SHAPES`, or a boundary position not in `ROLES`, is not gated. Treat
each as a living list — when a type reaches the wasm boundary in a representation no existing shape
captures, add a shape; when the emitter places types in a boundary position no existing role captures,
add a role (see "Adding / changing cells") — and periodically ask "which representation, and which
boundary position, are we *not* enumerating?", because a missing shape *or* role is a silent hole, not a
red cell (the E0599 bounded-wrapper-arm bug lived in the un-enumerated `tchoice-variant` role while its
`bwrap` shape was gated all along).

Pipeline (projection → fixtures → gate), the same shape as the robustness projection:

```
cddl-matrix/project_wasm_matrix.ts  ─►  tests/matrix_wasm/<shape>__<role>.cddl  ─►  integration_tests::wasm_matrix_compiles
     enumerate {shape × role}            one minimal fixture per cell             generate --wasm=true, cargo check the wasm crate
```

- **The projection** (`cddl-matrix/project_wasm_matrix.ts`, `bun run`) emits one minimal `.cddl` per
  `(type-shape × boundary role)` cell. Output is deterministic — **never hand-edit `tests/matrix_wasm/`**;
  edit the projection and re-run. `--check` is the `project_wasm_matrix_check` drift gate (fails on a stale/missing/orphaned fixture)
  and runs in CI's `matrix-drift` job.
- **The two axes** — the authoritative list + copy-paste CDDL live in the projection's `SHAPES`/`ROLES`:
  - **Type-shape**: how a type crosses the wasm boundary — `prim`, `palias`, `talias`, `coll`/`collmap`
    (array/map wrapper structs), `necoll`/`necollrec`/`nemap` (restricted non-empty wrappers over
    `NonEmptyVec`/`NonEmptyMap` — the failable `try_from` door beside infallible
    `new(first)`+`add`/`insert`; `necoll` takes a bare `Vec` by value, `necollrec`/`nemap` borrow +
    clone their loose builder wrapper), `passthru`/`passthrumap` (transparent `pub type`s), `ralias` (transparent
    alias to a Record struct), `struct`, `mstruct`
    (map-representation Record struct — bareword-keyed map), `cborwrap`/`cborwrap2`, `tag` (a CBOR-tag
    wrapper struct — crosses via a wasm `new(inner)` ctor and an inner-value `get()` accessor, plus
    `From<cddl_lib::Tg>` / cbor bytes), `bwrap` (a bounded/range wrapper struct — the only
    `Result`-returning wasm `new`: `new(inner)` enforces the `.size` bound, alongside `get()`),
    `cenum` (Copy c-style enum), `denum` (data-carrying type-choice enum),
    `nullable` (`Option<T>`), `generic` (a monomorphized RECORD-generic instance),
    `gcolla`/`gcollexp`/`gcolln`/`gtbla` (anonymous generic-COLLECTION/TABLE-instance lowerings —
    wrapper-needing element → structural class, exposable element → bare `Vec`, plus the
    named-instance-rule own-name control), `rset`/`nerset`/`rseta`/`nerseta` (`@duplicates reject`
    uniqueness twins over `OrderedSet`/`NonEmptyOrderedSet` — the FALLIBLE `add` door plus the
    std-set `insert -> bool`/`contains` doors, named-rule and anonymous-instance flavors), and
    `brset`/`brseta` (bounded reject `BoundedOrderedSet` wrappers — named zero-minimum `new()` and
    anonymous generic bounds-bearing structural class, each with checked `add` but no normalizing
    `insert`/`try_opt_from`); a loose/non-empty reject set NOMINAL class delegates its surface flat — `len`/`get(index)`/`add`/`insert`/
    `contains`/`try_from`/`try_opt_from` — instead of the two-layer `get() -> companion` shape,
    which only `@duplicates preserve` nominals keep), `pmap`/`nepmap`/`pmapa`/`nepmapa` (`@duplicates preserve` pair-map
    twins over `PairMap`/`NonEmptyPairMap` — the APPENDING `insert` and the `{+}` borrow-clone
    `try_from` door, same two flavors), `chain`, `extern`, `rawbytes` (a user-supplied
    `RawBytesEncoding` type). This is the
    `is_copy × directly_wasm_exposable × has-a-wrapper-RustStruct` axis the CBOR feature matrix
    deliberately does *not* individuate (wrapper-vs-transparent is a struct-table fact, not a shape fact
    — see the docstrings in `src/intermediate/`).
  - **Role**: where the type sits — `array-element`, `map-value`, `map-key`, `struct-field`,
    `struct-field-opt`, `newtype-inner`, `tchoice-variant` (the shape placed as one arm of a
    type-choice enum — the per-variant wasm ctor emission path via
    `generate_type_choices_from_variants`), `gchoice-variant` (the shape placed as one named-field arm
    of a `//` GROUP choice, `[ f0: T // f1: nint ]` — the group-choice sibling of `tchoice-variant`,
    minting one `new_<field>` wasm ctor per arm through the DISTINCT `codegen_group_choices` emitter
    path; array representation only, since the map-rep spelling emits byte-identical wasm). Each drives distinct accessor emission
    (`get`/`add`/`insert`/`keys`/`new_<arm>`, by-value vs by-ref). Struct roles use the **array representation** (`[field0: T]`,
    `[pre: uint, ? field0: T]`); the map representation is covered on the shape axis instead by the
    `mstruct` representative cell. Map-rep field holders (a bareword-keyed map with a mandatory or
    `?`-optional field) are deliberately not enumerated as separate roles because their wasm emission is
    byte-identical to these array-rep roles (the representation only changes rust-side serialization). A
    shape may likewise skip a role
    that would only pin a permanent red — `nullable` skips `map-key`: a nullable key is degenerate
    CDDL and its wasm bindings don't compile (`Option<u64>` fails `ErasableGeneric`), see the
    prune comment in the projection.
- **The gate** (`integration_tests::wasm_matrix_compiles`) globs the fixtures, generates each
  `--wasm=true`, and `cargo check`s the wasm crate. The wasm crate path-depends on the rust crate, so
  rust-side type errors surface here too — which means some skip-listed reds are rust-crate generation
  bugs rather than wasm-boundary ones. A cell whose spec NAMES code the spec does not contain is a cell
  that needs that code written, so both marker families are seeded — before this gate's `cargo check`
  and before the round-trip gate's `cargo test`, both legs — rather than
  skipped: a `rawbytes__*` cell resolves `_CDDL_CODEGEN_RAW_BYTES_TYPE_` to a user-supplied type
  (`PubKey`) and gets the in-repo defs (`tests/external_{rust,wasm}_raw_bytes_def`) spliced into the
  generated rust + wasm crates via `append_raw_bytes_defs` — mirroring `run_test`'s external-file
  append — while an `extern__*` cell's `_CDDL_CODEGEN_EXTERN_TYPE_` gets the same treatment from the
  shared `tests/def_templates/` via `append_extern_defs` (templated because the type name is the
  cell's rule name). Seeding costs no
  extra cargo invocation (same per-cell generate + check). It follows `feature_corpus_compiles`' shared-target-dir *pattern*,
  including per-cell package identities for both generated `rust/` and `wasm/` manifests (the latter
  keeps its `cddl-lib` alias with Cargo's `package = ...` selector) and one uncached old-enough
  expected-red canary per shard. It uses its **own** scratch + `CARGO_TARGET_DIR` (`cddl_codegen_wasm_matrix`), separate so the two
  tests don't collide when `cargo test` runs them in parallel. The verdict is **compile**: a cell can
  compile green while emitting *semantically* wrong bindings (e.g. an identity `.into()` where a transform
  was needed). Catching those is the job of the **round-trip** upgrade — `integration_tests::wasm_matrix_roundtrips`
  (`#[ignore]`d, manual): same cell enumeration, but each cell is generated `--emit-tests=true` and
  `cargo test`ed so the emitted `cddl_generated_wasm_tests` module (see § "wasm-crate test module" above)
  RUNS its cross-crate byte differential + accessor read-back. It sweeps every cell across
  `ALL_PROFILES` minus the component row — so default / preserve / json (`--preserve-encodings` and
  the json flags substantially change codegen, so the wasm behavioural verdict must hold under each,
  whereas `--component` adds no wasm surface at all); the compile floor above stays
  **default-profile only** by cost policy (non-default compile coverage is subsumed by this gate's
  `cargo test` at full tier). It has its own scratch dir (`cddl_codegen_wasm_matrix_rt`) with one
  shared `CARGO_TARGET_DIR` across all profiles/cells and frees each per-cell output dir after its
  verdict. It uses the module-level `WASM_MATRIX_SKIP` (red in every profile) plus a
  `WASM_MATRIX_PROFILE_SKIP` (this gate only — `(profile, cell, reason)`), each with the four-state
  resurfaced-guard verdict; BOTH are empty at HEAD, as every cell compiles and round-trips green
  across all three profiles. Every skip/pin ledger validates its keys up front against
  its gate's swept universe, so dead fixture/cell/profile pins fail before heavy work (when adding a
  guard, verify it the way these were: temporarily poison a key and watch the gate fail fast, then
  revert). Run it with
  `cargo test --bin cddl-codegen wasm_matrix_roundtrips --
  --ignored` (~7 min warm); a cell whose shape mints no wasm surface (loud emitter skip) passes with
  zero emitted tests, which is a legitimate green (the compile gate already pins its ABI compiles).

**Wrapper-vs-transparent — route through one predicate.** The recurring wasm-boundary bug source was
naming, boundary conversion, and exposability each *separately* deciding whether an ident is exposed as a
`#[wasm_bindgen]` wrapper struct or a transparent `pub type` — a *struct-table* property, not a
`ConceptualRustType` shape (a named collection `nums = [* uint]` is a wrapper; a passthrough `arr2 = arr`
is transparent — same IR shape). The single source of truth is `IntermediateTypes::has_wasm_wrapper(ident)`;
new decision sites should consult it instead of re-deriving. Gotcha it encodes: an exposable named array
has a wrapper struct *and* is used transparently as `Vec<T>`, so a passthrough-alias emission must gate on
`has_wasm_wrapper(target) && !base_type.directly_wasm_exposable()` (maps are never directly exposable;
exposable arrays are — that split is what keeps `passthrumap` pointing at the wrapper while `passthru`
stays a transparent `Vec`).

**Fixing a red cell (the TDD loop).** A red cell is a bug the matrix *wants* fixed. Known reds sit in the
gate's `WASM_MATRIX_SKIP` list, with the shared reason comment and a ledger entry in
[`cddl-matrix/roadmap.toml`](../cddl-matrix/roadmap.toml) (which shape/role, the exact `E####`, root cause).
At HEAD the list is EMPTY — its one former resident, `extern__array-element`, came off it when the
extern defs above started being seeded — so any red appearing is a regression to fix, not a backlog
item. The round-trip gate's `WASM_MATRIX_PROFILE_SKIP` (compile-clean cells red only under some
profiles) is likewise empty. To close one:

1. Remove its `<shape>__<role>` entry from `WASM_MATRIX_SKIP`.
2. Fix the emitter; `cargo test wasm_matrix_compiles` until green.
3. A `WASM_MATRIX_SKIP` cell that starts compiling *fails* the gate (the "resurfaced" guard) — so you can't forget
   step 1 and the list can't rot.

A *new* red cell (red but not in `WASM_MATRIX_SKIP`) also fails the gate: fix it, or skip-list it
**deliberately** with a roadmap entry — never silently.

**Adding / changing cells.** Edit `SHAPES`/`ROLES` in the projection, `bun run project_wasm_matrix.ts`,
review the new fixtures, run the gate. Prune cells whose emission duplicates an existing one — the
projection already restricts redundant shapes (`chain`, `cborwrap2`, `extern`, `mstruct`) to one representative role.

> Sibling system: `tests/matrix_{supported,panic,reject}/` (projected by `cddl-matrix/project_robustness.ts`,
> driven by `src/tests/robustness_tests.rs`) is the same projection→fixtures→gate shape on a different axis —
> "does a construct *generate*?" rather than "does its wasm *compile*?". Three generation-outcome
> catalogs, one per matrix verdict class: **supported** (`all_supported_constructs_generate` — must
> generate clean), **panic** (`unsupported_construct_panic_catalog` — tracked-known generator panics),
> and **reject** (`unsupported_construct_reject_catalog` — the rows the matrix marks off-limits that mint
> no other test: parse-rejected control ops, constructs with no standalone Rust representation such as
> the `#` any-type (`type2.any`), and
> out-of-profile constructs). Containment cells (`contain.*`) are included in this projection; spec-disallowed
> cells without annotation rows are naturally absent, while supported/reject/panic cells get generated
> fixtures and subsume the older hand pins for map-key spelling/arity, group-choice-arm, and
> occurrence-target coverage. The reject catalog's payoff is catching a parser/codegen change that
> *silently* makes a rejected construct parse — the exact regression a past cddl-fork bump caused for 14
> control ops — as a snapshot diff in the default `cargo test` run instead of only on a manual verify.ts
> sweep; the `project_robustness_check` gate (`project_robustness.ts --check`) independently pins each reject row's expected label to its matrix
> evidence class, so a re-bless can't quietly launder such a flip. Every graceful reject row also
> snapshots its rejection-line count, with a non-vacuous graceful-row floor; separately authored
> nodes with equal rendered text remain distinct reports under the node-identity regression.

> Sibling system: `src/tests/identifier_hazard_tests.rs` is the same catalog+gate shape on a
> **NAME-shaped** axis a construct enumeration can never catch — collisions between a user-chosen CDDL
> *name* and the Rust the generator *emits* (the axis IS the name). It sweeps a static hazard table
> (`RUST_KEYWORDS` reused from `parsing.rs`, the single-letter names `r`/`w`, and prelude/std type names
> like `Option`/`Vec`/`Int`) × six name positions (rule name in BOTH emitted type shapes — record
> struct and type-choice enum, since the historical generic collision was shape-dependent and a
> struct-only sweep would launder enum-shaped `w` as clean — bareword map key, bareword array key, plain group name,
> `@name` directive value). It is a Rust module rather than a `project_robustness.ts`
> projection **on purpose**: the hazard × position table has no matrix verdict upstream to drift from,
> so a TS layer would only copy a constant into fixtures. Two layers: `identifier_hazard_robustness_catalog`
> (default `cargo test` — the `robustness` substring in its name keeps the `cargo insta test --
> snapshot_tests robustness` orphan gate selecting it) snapshots each cell's *generation* outcome
> (`ok` / `error (graceful)` / `PANIC`, a scorecard — a committed `PANIC` is a tracked-known gap, a NEW
> one is a regression); `identifier_hazard_crates_compile` (`#[ignore]`, check.ts full tier) *compiles*
> the `ok` cells — bundling each position's non-pinned hazards into one crate to avoid ~hundreds of
> `cargo check`s, minus a pinned `EXPECTED_COMPILE_FAIL` set of known does-not-compile cells asserted
> to fail INDIVIDUALLY so a pin flips loudly when its fix lands (currently EMPTY: the shape-dependent
> `r`/`w` generic-collision pins it launched with dissolved when cbor_event 3.x de-generified the
> emitted `serialize`/`deserialize` signatures — no fn type parameters remain to shadow, pinned by
> `emitted_signatures_carry_no_reader_writer_generics`).
> A non-pinned bundle that fails to compile is a NEW hazard finding to add to the pin list (with a
> reason) and report — not to paper over by editing the generator.
>
> The same module carries the **generated-local** vocabulary sweep, a distinct hazard class: names the
> *emitter itself* binds inside a generated fn body, which a field of the same name shadows into a
> crate that does not compile. Its registry is `parsing::GENERATED_LOCAL_RESERVED` (refused at parse
> time, each entry carrying the shape × profile × error class it was measured to break) plus
> `GENERATED_LOCAL_PROBED_SAFE` (swept, broke nothing, deliberately accepted), and
> `generated_local_registry_covers_emitter_locals` holds the pair LOCKSTEP with a scan of the six
> emitter sources so a NEW emitter local fails the suite until it is swept and verdicted. Membership
> is uniform across PROFILES and scoped by SHAPE (`ReservedScope`): no flag may rescue a refused
> field, but a name is reserved only where the emitter binds the local — which is what keeps the
> `tag: 0` group-choice discriminant generating. Both halves of every scope are asserted:
> `generated_local_hazard_robustness_catalog` snapshots refused-inside / still-generating-outside per
> field position. `generated_local_scope_wide_crates_compile` (`#[ignore]`, check.ts full tier)
> derives the full reserved + probed-safe denominator, requires every scoped reserved cell to remain
> a graceful error with the normal `reserved name` / resolved-field / `@name <other>` diagnostic,
> stale-guards any deliberately position-specific refusal behind an exact cell, diagnostic substring,
> and reason, and *compiles* every legal original/named/newtype/`.cbor`/bounded-member cell (with
> both `uint` and `bytes` payload variants where their carrier can alter the generated path)
> across native profiles, JSON/WASM/json-gen, and component output. This is the only way a scope that
> is too NARROW or a safe local that collides only on another face can fail loudly; a generation-only
> sweep reads either as a clean `ok`. Its `hazards()` sibling is left untouched by design: that list
> feeds the recombination fuzzer's deterministic composition set.

> Second sibling, same argument on a **DOCS-CONTRACT** axis: `src/tests/dsl_position_tests.rs`
> hard-asserts the comment-DSL directive × attachment-position grid against
> `docs/docs/comment_dsl.mdx`'s claims (plus error-message-advertised remedies). A directive that
> silently no-ops in an unenumerated position still generates, compiles, and round-trips — invisible
> to every execution-gated probe — so each cell asserts the OBSERVABLE effect as a string-level
> check on the generated source (a renamed field, a `///` comment, a missing serde impl beside a
> positive control; cells whose directive's effect is wasm-side — `@used_as_elem`'s minted list
> wrapper — opt into `--wasm` string generation, still no static dir) or a graceful rejection. Unlike the hazard catalog it is hard-asserted, not a
> blessable snapshot (blessing a decay to silent-drop would defeat the purpose); discovered drops
> are pinned in `KNOWN_SILENT_DROP` (mirroring `EXPECTED_COMPILE_FAIL`) — asserted to STILL be
> dropped so a pin flips loudly when a fix lands, and a pin is a finding to report, not a license
> to re-author the expectation. Pins carry a vacuity hazard Effect cells don't: a pin asserts
> "expectation NOT satisfied", which a MISPLACED directive comment satisfies vacuously (the DSL's
> comma-placement rules are finicky), so a pin is authored only after hand-verifying the placement
> against the docs' comma rules — ideally beside a control cell using the same placement in a
> position where the directive works, isolating *position* as the variable. The pin list is
> currently EMPTY: every cell meets its docs-claimed expectation, by honoring the directive or by
> refusing it with a message naming the spelling that works, so a new entry is a new finding.

> Third sibling, and the axis the one above is structurally blind to:
> `src/tests/referencing_context_tests.rs` writes the directive on a **BASE rule** and makes its
> assertion through a **REFERENCE** to that rule. Every cell of the position grid places the toggled
> directive ON the rule under test, so a directive honored at a rule's own position and dropped at a
> wrapping context of it is invisible there by construction — a class whose every known instance (the
> custom codec pair reached through a tag head, a `.cbor` payload, a transparent re-alias and a
> rule-body `.cbor` alias) had to be found by a hand probe. A cell is
> `(directive, base shape it is HONORED on, wrapping context)`: `BASES` carries one row per directive
> family, `CONTEXTS` ten wrapping contexts (tag-head payload, `.cbor` payload, rule-body `.cbor`
> alias, transparent re-alias, generic argument, map value, map key, array element, type-choice arm,
> optional record member), and the sweep runs their product — **one generation per cell and a verdict
> PER-CONTEXT, never best-of-embeddings**, since context is the cell's variable and an effect in one
> embedding must not absorb a drop in another. A cell passes on the directive's effect **or a loud
> refusal** crossing the reference; silence is the failure. The directive axis is LOCKSTEP with
> `comment_ast::KNOWN_RULE_METADATA_TAGS` (`every_directive_is_swept_or_excluded`): a tag has a cell
> in every context or an entry in the `EXCLUSIONS` registry whose reason must be STRUCTURAL — the
> directive has no base shape a reference observes, or the context IS the directive's effect so no
> anchor can attribute it — so a NEW directive fails the module until its author classifies it.
> Findings are pinned in `KNOWN_REFERENCE_DROP` on the sibling sweep's terms (asserted to STILL be
> dropped; a pin is a finding to report, never a license to fix opportunistically or to re-author the
> expectation). Its vacuity hazard is answered structurally rather than by care alone:
> `every_context_row_is_live` requires each context to carry at least one directive's effect and
> `every_base_row_is_live` requires each base shape to be honored through at least one reference —
> which is the cell definition's "base shape it is honored on" made mechanical, and it is what stops a
> mis-authored row from being recorded as a whole row of findings. The pin list is currently EMPTY: no directive
> is dropped at any of the ten contexts.

> Fourth sibling, and the one whose axis is NAMES rather than directives:
> `src/tests/refused_name_closure_tests.rs` is the closure gate for the **side-door** class — a name
> refused at ONE name-resolution seam still reaching generation through ANOTHER. Its worked example
> is the narrower-float delivery: the refusal shipped at `IntermediateTypes::new_type`'s
> unresolved-reserved fallback, and `x = float16 .size 4` still generated at exit 0 because a control
> operator resolves its head through `parsing::ident_to_primitive` and never calls `new_type`. The
> per-name position sweeps (`undefined_prelude_generates_in_every_position` plus the refusal siblings)
> vary the POSITION and hold the resolution MECHANISM constant, so a second path is invisible to them
> by construction; those sweeps stay as the WORDING pins and this module owns only the closure
> property, which keeps its cells cheap (one-rule specs, generation-only, one profile — a refusal
> short-circuits `finalize` before any emission, so no flag can rescue a name). A cell is
> `(refused name, syntactic context)` and its verdict must be a **graceful refusal NAMING the type,
> or a loud rejection** — never exit-0 generation, never a panic.
> **Both axes are DERIVED at HEAD rather than transcribed**, because the recorded inventories go
> stale by design (the three narrower float names were on the roadmap's refused list and are
> registrations now). The name axis is `IntermediateTypes::REFUSED_PRELUDE_NAMES`, a constant the
> interception ARMS themselves read, and `the_refused_name_axis_is_the_refusal_inventory` re-derives
> it empirically — probing every member of the enumerable `utils::RESERVED_IDENTS` (plus `any`, which
> is not reserved but is intercepted one arm earlier) at the canonical member position and requiring
> the names that refuse to BE that constant. A new refusal arm therefore fails the derivation until
> the inventory names it, and naming it demands a cell in every context: the forcing shape
> `KNOWN_RULE_METADATA_TAGS` uses for directives. The context axis is a `SEAMS` registry whose
> members' CALLER CLASSES are listed rather than grepped (`new_type`, `ident_to_primitive`, the
> field-name family, and `extern_narrow::scan_consumer` — an input-assembly posture rather than a
> position, so it has its own test with an imported-rule positive control), and every context row
> names the seam it reaches (`every_seam_row_is_reached`).
> **Attribution is mechanical.** A column is swept only if a CONTROL head (`uint`) generates in it
> (`every_swept_context_is_live`) — that is what makes a refused name's verdict the NAME's doing and
> not the context's, and it is also how "the `cddl` crate refuses the text" is kept apart from "our
> walk refuses the shape". Columns where even a supported head does not generate are
> `Attribution::ContextOwned`, excluded with the control's own verdict RECORDED and re-checked every
> run (`every_context_owned_exclusion_is_still_context_owned`), so an exclusion goes stale loudly the
> day its underlying defect is fixed. Findings are pinned in `KNOWN_CLOSURE_BREACH` with their KIND
> (exit-0 or panic) and an evidence substring, on the sibling sweeps' terms. The pin list is EMPTY
> at HEAD: its founding residents — the ten `<refused name> .default 1` cells that aborted at
> `RustType::default`, destroying a refusal `new_type` had already recorded, plus two
> name-independent control-operator aborts the context enumeration turned up — were all converted
> to graceful refusals in the same delivery, so their robustness-catalog rows record
> `error (graceful)` and a new entry here is a new finding.

### Synthesized-name interaction sweep + duplicate-ident backstop

The generator mints structural wasm-boundary classes whose names derive from user type names — the
loose `{Elem}List` / `Map{K}To{V}` builders, the restricted `NonEmpty*` wrappers, and the table
`keys()` list wrappers. How those names interact with USER rule names (and with each other) is a
NAME-shaped axis the shape catalogs never reach: they mint one rule per shape and never spell a
colliding user name or a named+inline coexistence, so a bug in this class ships as **generation exits
0 but the wasm crate doesn't compile**. Three standing layers own it:

- **Duplicate-ident backstop** (`generation/export.rs::top_level_type_ident` + the scan in
  `generated_files`). Before export, every generated `src/generated/**` file (all three crates) is
  scanned for line-anchored top-level type-namespace definitions (`pub struct`/`enum`/`type`); any
  ident defined twice within one file returns an `Err` at the `generated_files` seam naming the file
  and the duplicated ident(s). This observes the ACTUAL emitted source rather than an IR prediction,
  so it is the backstop for every mint path present and future — turning the silent E0428
  redefinition (a user rule colliding with a synthesized ident) into a loud, graceful generator
  error. The plain F1/F2/F5 families have no IR-level collision scan (only the `NonEmpty*` families
  do, in `intermediate/mod.rs`), so for them the backstop is the sole pinned layer;
  `loose_builder_name_claimed_plain_message_names_ident_and_file` pins its message identity and its
  robustness-catalog row pins the outcome label.
- **`synthesized_name_interaction_sweep`** (`integration_tests.rs`). A table-driven sweep crossing
  each synthesized-name FAMILY (F1 plain list, F2 table builder, F3 `NonEmpty*` list, F4 `NonEmpty*`
  map, F5 table `keys()` list) with each INTERACTION (I-a different-shape rule claims the synthesized
  name; I-b named + inline same-shape coexistence; I-c self-named rule; I-d different-shape claim of a
  needed auxiliary builder — expressible only for F3/F4). The per-cell **invariant: no cell may be
  exit-0 with a non-compiling crate.** Each cell is pinned to either a graceful `Reject(ident)` (an
  IR-scan or backstop rejection whose message names the colliding ident, asserted in-process, no
  cargo) or `Ok` — the generating cells are batched (cell-prefixed rule names, so name-local classes
  can't mask across cells) into ONE crate whose wasm binding is `cargo check`ed (the
  `feature_corpus_compiles` shared-target pattern), with per-cell `present`/`absent` assertions
  pinning the dedup semantics (dedup target defined once, the deduped-away twin never emitted).
- **Wasm collection-wrapper registry closure** (`generation/mod.rs` +
  `synthesized_name_registry_tests`). Each real wasm member/parameter/return render door records
  the collection wrapper it writes, its owner, and its door. Before either source producer returns,
  the registry requires a local class/alias or declared dependency provider; a reference emitted
  inside a non-exported extern-dependency scope remains deliberately dependency-owned. This is the
  reference-side E0425 invariant, so a missing mint is a deterministic generator error naming the
  wrapper and owner/door before rustc.

  Provider identity is scope-exact rather than name-only. An unrelated dependency class or alias
  with the same synthesized spelling cannot close a local structural reference, and a local class,
  alias, or declared deferral cannot stand in for a renderer-selected dependency provider from a
  different scope. Alias references follow the exact target the emitted `pub type` line writes;
  configured aliases resolve through `AliasInfo::base_type`, because their conceptual inner has
  already lost occurrence bounds and `@duplicates` policy. Requested wrappers are classified from
  the exported `requested_collections` emission scope rather than from a same-spelling IR rule's
  scope. `unrelated_dependency_class_with_structural_spelling_does_not_close_local_reference`,
  `emitted_collection_aliases_register_the_target_spelling_they_write`,
  `named_restricted_list_and_set_alias_fields_register_their_emitted_providers`, the exact-provider
  cases in `synthesized_name_registry_provider_kinds_and_missing_rows_are_deterministic`, and the
  collision-bearing requested bounded-map resident pin these review-discovered distinctions.

`table_keys_list_syntheses_share_the_established_loose_boundary_carrier`
(`robustness_tests.rs`) is the cheap minimized cross-composition pin for the batch-19/batch-33
failures found by the full-tier wasm recombination leg. It combines loose, non-empty, bounded,
scalar-ranged, and `.cbor`-wrapped array keys and asserts that the established `ArrU64List` ABI is
minted once over `Vec<Vec<u64>>`; restricted `keys()` surfaces convert into that boundary carrier,
while their map classes retain checked key doors. A focused generated-WASM `cargo check` is required
when changing this seam, and the full-tier recombination wasm leg remains the broad compile oracle.
`nested_table_key_keeps_its_native_array_carrier_at_the_outer_boundary` is the complementary
read-derived pin: `rc1508 = { { [*5 uint] => uint } => uint }` distinguishes the inner bounded
table's loose checked source (`MapArrU64ToU64` over `Vec`) from its native recursive identity
(`MapU64ListMax5ToU64` over `BoundedVec`), then asserts that the outer table conversion retains the
native bounded carrier. Its sibling
`direct_and_nested_table_keys_share_one_canonical_structural_builder_carrier` crosses both traversal
orders and pins `MapArrU64ToU64` as the one loose builder regardless of mint order.
`ordinary_inline_map_keeps_its_native_restricted_key_carrier` pins the recursive native name and
carrier for the ordinary-map boundary;
`loose_map_and_named_bounded_table_mint_distinct_builder_and_native_classes` and
`open_table_catchall_and_named_bounded_table_mint_distinct_classes` prove the formerly refused
one-name/two-carrier compositions now generate as distinct default/preserve classes.
`bounded_table_loose_builder_collision_uses_actual_source_identity` keeps the collision detectors
on the exact loose-source class, including self-named bounded sources. The source assertions catch
an otherwise exit-0 E0277 conversion mismatch; focused generated-WASM `cargo check`s remain required
after edits.

Expectations are seeded by the **probe-then-pin** rule: run the generator on the cell's CDDL, inspect
the outcome, then pin the observed-AND-correct behavior. A cell that lands exit-0 + non-compiling is a
NEW instance of the class — fix it if the fix is small and clearly correct, otherwise pin it in a
cited, vacuity-guarded known-bad ledger and REPORT it; never bless it by loosening the row to `Ok`.
The compile gates still provide broad whole-crate confirmation, but the E0425 flavor is now rejected
at generation time by the registry closure rather than waiting for a downstream rustc error.

### rust↔wasm API-surface parity (`wasm_parity_tests::wasm_api_parity`)

The compile gate above proves the emitted wasm bindings *type-check*; it cannot prove they *exist*. A
member emitted on the rust side of the crate boundary with no wasm counterpart is invisible to every
oracle here — snapshots pin whatever was emitted, the compile gates compile whatever was emitted, and
the wasm test mint is *written against* the surface that exists, so it exercises what's there and
can't demand what's missing. The proven instance is `4e5b837`: wrapper types shipped for years with a
rust `new`/`From` but no wasm ctor/getter — `generate_wrapper_struct` built a `wasm_new` and never
pushed it, caught only by reading the generator. `wasm_api_parity` closes that class structurally.

It parses the emitted `rust/src/generated/mod.rs` and `wasm/src/generated/mod.rs` with `syn` (a
harness-side dev-dep) and asserts a **one-directional rust→wasm** correspondence — only rust members
impose obligations, so wasm-side extras (`kind`/`as_*`/`has_*`/`set_*`/`len`/`insert`/`keys`/
`to_cbor_bytes`/…) are unchecked by design. Four rules:

1. Every rust `pub struct`/`enum` has a wasm counterpart (same-named wasm struct/enum, `pub use`
   re-export, or **public** `pub type` alias).
2. Every rust `pub type` alias has a same-named wasm public alias or wasm type — a **private** wasm
   `type` alias does *not* count (that is exactly the finding class the generator fix below closed).
3. Every rust `pub` field on `T` has a wasm getter of the same name (no setter obligation: wasm emits
   `set_*` only for optional fields). One structural exemption: a field whose type is `Option<X>`/`X`
   with `X` a pub struct defined in the emitted `cbor_encodings.rs` (the preserve profile's
   `pub encodings: Option<XEncoding>` capture fields) is rust-only round-trip metadata, not boundary
   API — no wasm getter obligation. Obligations still come from `mod.rs` only, so the `*Encoding`
   structs themselves never impose any.
4. Every rust inherent `pub fn` on `T` has a wasm inherent fn of the same name **and arity** (`self`
   excluded; return types unchecked — boundary conversions differ by construction). Rules 3–4 run
   only when a same-named wasm struct/enum is *defined*; a `pub use`/alias counterpart is full parity
   under rules 1–2 (a `pub use` *is* the same type; a rust alias has no inherent members).
5. **JS-name visibility.** wasm_bindgen exports no type aliases, so a rust type whose ONLY wasm
   counterpart is a `pub type` alias never reaches JS under its CDDL rule name. Rule 5 resolves the
   alias's target and flags iff the target is a struct/enum *defined* in the wasm mod (a real
   `#[wasm_bindgen]` class) whose name is NOT itself on the rust surface — the (since-fixed)
   usage-dependent JS-class-name class, where a named table rule's wrapper degraded to
   `pub type Mp = MapU64ToText;` pointing at the generator-invented structural class (rule 5 stays
   the live catcher for any recurrence). Carved out (not findings): a target that is
   not wasm-defined (transparent alias to a primitive/std/`Option` type — native in JS); a
   wasm-defined target that IS a rust-surface rule name (a genuine CDDL-level alias on both sides);
   and a **synthesized anonymous generic-collection/table instance alias** (`gcoll<foo>` →
   `GcollFoo`, `gcoll<uint>` → `GcollU64`, `gtbl<uint, text>` → `GtblU64Text`) — the user wrote an
   anonymous instance, not a rule, so it correctly crosses as its inline equivalent's STRUCTURAL
   class (`FooList` / bare `Vec` / `MapU64ToText`, the documented lowering) with no rule name at
   stake. Rules 2 and 5 both skip these. The discriminator is **provenance, not shape**: the
   generator emits a doc marker (`generation::SYNTHESIZED_INSTANCE_ALIAS_DOC`) on synthesized instance
   idents only, and the gate reads it from the rust item's rustdoc — a shape heuristic ("aliases a
   std collection") was rejected because a sole-owner named-table alias (`pub type Mp = MapU64ToText;`)
   is a bare-collection alias too and must STAY gated (else rule 5 goes blind to the degradation bug
   it exists to catch). The marker emission is pinned by `synthesized_instance_alias_marker_provenance`.
   `pub use` counterparts stay JS-visible by design (`#[wasm_bindgen]` c-enums re-exported).

Legitimate rust→wasm asymmetries are baked into those rules, not ledgered: the "`pub use`d Copy
enums", "rust-only trait impls" (only inherent impls are walked — `From`/`AsRef`/`Serialize`/… are
never counted), collection-API-inheritance (a transparent `pub type Nums = Vec<u64>` has no
enumerable members), and tag-over-struct-folding classes all fall out structurally. What it does **not**
check: *semantic* wrongness — an identity `.into()` where a transform was needed — stays
`wasm_matrix_roundtrips`' job; this is a *presence* differential.

The six flag-conditional trait/serde re-export doors sit outside that differential by construction:
their rust homes are runtime traits or serde derives, while wasm-bindgen requires inherent methods.
`wasm_door_vocabulary_matches_the_posture_that_owes_it` therefore uses the production
`generation::wasm_door_members` contract — the same ordered descriptor vector that constructs and
then structurally pushes the wasm methods — for its expected set, and parses the emitted wasm module
with `syn` against a matched door-disabled baseline. The output door is the complete parsed inherent
method difference, so an unregistered direct extra cannot hide behind a vocabulary filter. Its seven
postures cover default, disabled bytes,
JSON, preserve, preserve+canonical, preserve+canonical+JSON, and canonical with bytes disabled;
both a record and map record owe the set, a collection wrapper is the negative control, and the
macro branch is separately pinned as invocation-only. The test also checks the composed runtime
declares every emitted CBOR door and that any JSON door has serde derives, so the contract cannot
quietly drift from the surface it mirrors.

Inputs are every `tests/matrix_wasm/*.cddl` cell (even `WASM_MATRIX_SKIP` ones — parity is
parse-only, and their emitted sources parse even when they don't standalone *compile*) plus the two
depth fixtures `tests/core/input.cddl` and `example/test.cddl` (kitchen-sink shapes the minimal cells
don't reach), each swept across `ALL_PROFILES` minus the component row — so default / preserve /
json (the flags substantially change the rust surface, whereas `--component` changes neither side of
the rust↔wasm boundary this gate diffs). A second corpus axis sweeps every committed `tests/*/input.cddl` fixture
dir under that dir's committed generation profile rows from `integration_tests.rs` (dropping only
flags irrelevant to the emitted `src/generated` API surface, such as `--emit-tests`,
`--wasm=false`, and `--package-json`). A completeness guard enumerates `tests/*/input.cddl` at
runtime and requires every dir to be either in the corpus table or in the exclusion table; the two
excluded dirs are `core` (already swept as a depth fixture across all profiles) and
`wasm-list-macro` (its committed wasm members are emitted as user-macro invocations, invisible to a
`syn` presence differential). Directory-input fixtures such as `tests/multifile/inputs` and
`tests/extern-deps*/inputs` are out of scope for this axis: multifile emission writes per-module
files under `src/generated/`, outside this differential's `mod.rs`-only parse scope, and is covered
by the separate multifile placement sweep. Vacuity guards pin the matrix/depth count plus total
corpus profile rows so the sweep can't silently shrink.

Generation is **in-process** (`api::generated_strings` via `Cli::parse_from`, wrapped in
`catch_unwind` — no subprocess, no scratch dirs) and **parse-only** (no cargo check/test of the
generated crates), so the sweep stays always-on (no `#[ignore]`) in the default `cargo test` /
check.ts local tier. It scopes to `src/generated/mod.rs`; a key-set guard over the returned file map
fails loudly on any `.rs` name outside the per-profile allowlist (the rust base list includes
`key_demand_assertions.rs` — any `@used_as_key`-tagged crate's private compile-time-only
`_demand_*` self-checks, zero pub items so nothing to parse; preserve additionally allows
`cbor_encodings.rs`/`ordered_hash_map.rs`, both optional; the wasm side allows `mod.rs` plus
`collections.rs` — the wrapper re-export index every wasm crate now emits, a `pub use` inventory of
classes already defined in `mod.rs`, so it introduces no boundary API for the differential to
parse), so a future emission surface can't silently escape the differential — it caught
`key_demand_assertions.rs`'s widening to bare roots exactly this way before the file was
classified. One (profile, input)
the `EXPECTED_GENERATION_FAIL` ledger is currently empty; an unlisted generation abort is a normal
parity failure, while any future deliberate pin is guarded in both directions.

Findings reconcile against a `PARITY_EXEMPT` ledger keyed `(profile, input, item, reason)`, the same
`WASM_MATRIX_SKIP` idiom: a finding matching an entry is expected (no failure); an entry matching no
live finding fails as "resurfaced" (a fix landed — remove it); an unexempted finding fails with the
remedy spelled out (fix the emitter, or deliberately ledger it with a reason). The ledger is
**empty** — every finding class the gate has surfaced was fixed at the emitter rather than ledgered:
the named-table wasm alias emitted as a private `type` instead of `pub type` (`generation/`'s
already-generated-map branch now carries `.vis("pub")`, matching its sibling passthrough-alias site);
the preserve-profile wrapper `inner` field emitted `pub` (caught by the profile sweep's first run;
now `pub(crate)` like the default profile's tuple field — deliberately crate-visible so hand
modules outside the generated subtree can reach it, while EXTERNAL code still can't
literal-construct or mutate a wrapper past the bound check `new()` enforces and goes through the
getter; the finding class stays retired because the sweep's `is_pub` matches only
`syn::Visibility::Public`, so `pub(crate)` reads as non-pub to it); and the rule-5 usage-dependent JS-class-name bug, where a named table rule's wrapper
degraded to a `pub type` alias pointing at the generator-invented structural map class (so the CDDL
rule name never reached JS and the shape's class name flipped with unrelated spec content). The fix
(`generation/`'s up-front table-shape ownership pass): a shape owned by a SINGLE named rule now
surfaces its class under the CDDL rule name, with the structural `MapKToV` name a `pub type` alias to
it; same-shape rule PAIRS keep the structural fallback for embedded uses while each named rule still
gets its own class. The corpus-axis landing also surfaced the built-in `int` prelude wrapper gap:
rust exposed `Int::new_uint`, `Int::new_nint`, and `IntError` for `int` map keys, while wasm exposed
only the signed `Int::new(i64)` constructor and mapped parse failures to `JsError`; wasm now emits
the two raw-CBOR-argument constructors and a source-level `pub type IntError = JsError` counterpart.

### WIT validity, four stages (`component_tests`, gate `component_wit`)

Every component fixture's emitted `.wit` package goes through resolve → package-encode →
**package-validate** in-process, at the pinned `wit-parser`/`wit-component`/`wasmparser` floor —
never through the ambient `wasm-tools` binary, which predates the floor and cannot read these
packages at all. Stage four is load-bearing rather than belt-and-braces: the strong-uniqueness
collision class survives resolve AND `wit_component::encode` and fails only at component
validation, so a gate stopping at encode would pass an unbuildable package. The pinned control is
`component_wit_validity_gate_fails_a_strong_uniqueness_collision_at_stage_four`, which asserts
both halves — the collision fixture passes stages 2–3, then fails validation with the pinned
message — so the control cannot go vacuous in either direction. The posture-purity pin
(`component_wit_is_wasm_posture_independent`) asserts the emitted WIT is byte-equal under
`--wasm=true` and `--wasm=false`: the projection resolves through the wasm-gated IR convergence
rather than reflecting it.

The same gate carries the **wasip2 build smoke**
(`component_crate_builds_for_wasm32_wasip2`), which compiles `BUILD_SMOKE_FIXTURES` from the
**workspace root** — every member's own lib targets, not just the component package reached as a
dependency — over manifests exactly **as emitted**. Both halves are the point. A root build is the
posture a consumer building their workspace for that target has, and it is the only one that
reaches the rust crate's `[lib]`; and nothing about the tree is edited first, so the assertion that
a component-only tree carries `crate-type = ["rlib"]` fails here rather than being arranged away.
The sibling component build gates (`component_host`, `component_compose`, `component_jco`,
`component_import`) make the same assertion for the same reason. The one both-faces sweep,
full-tier `component_corpus_compiles`, is the exception that proves it: the `ALL_PROFILES`
component row leaves `--wasm` at its default `true`, so its manifests keep the wasm face's
`cdylib` and it applies the hand narrowing the [`--component` flag
doc](../docs/docs/command_line_flags.mdx) prescribes for that shape — after asserting the wide form
is what it was handed.

### rust↔WIT API-surface parity (gate `component_parity`; `component_parity_tests::component_api_parity`)

The component face's sibling of the gate above, asking the same one-directional question of the
other boundary. It matters for the same structural reason and one sharper one: the component gates
in `component_tests` all judge what was emitted *against itself* — the four-stage validity gate
resolves/encodes/validates the `.wit`, the wasip2 build smoke compiles whatever glue implements
whatever that `.wit` declared — so a member missing from BOTH halves yields a package that resolves,
validates and builds. Nothing else can see it.

The rust half is `syn` over **every** emitted per-scope `rust/src/generated/**/mod.rs` (the wasm
sibling parses one `mod.rs` and excludes directory inputs; the component face is inherently
multi-interface — one WIT interface per input file — so multifile is the point rather than an
exclusion). The WIT half is `wit-parser` over the emitted package, the same pinned resolver the
validity gate uses, so what is compared is the surface the toolchain sees. The rust→WIT name map is
`utils::convert_to_kebab_case`, a pure function of the rust name, plus the one non-kebab mapping
(rust `new` ⇒ the resource's `constructor`); the projection's `WitPackage` does carry the pairing,
but reading it would make this an intent check instead of an output check. Three rules:

1. Every rust `pub struct`/`enum` is either a WIT type of the kebab name, or carries a
   `// unexported: <Ident> — <reason>` record in the emitted `.wit`. Neither is the silent-drop class.
2. Every rust `pub` field on a type whose counterpart is a **resource** has a WIT getter of the kebab
   name — with the same structural encoding-capture exemption the wasm sibling has
   (`pub encodings: Option<XEncoding>` under `--preserve-encodings` is round-trip metadata).
3. Every rust inherent `pub fn` on such a type has a WIT member of the kebab name. Signatures are
   unchecked by design: the two ABIs differ by construction (borrows in, owned handles out, every
   failure as `result<_, string>`).

A `pub type` alias imposes nothing — a CDDL alias and a named collection are resolved THROUGH at
their use sites and never surfaced, which is the documented type-mapping row. A counterpart that is a
WIT **value** type (`enum`, the `int` variant, an alias) has no member namespace, so rules 2–3 have
nothing to compare — but a rust type with inherent fns *and* a value-type counterpart is reported
rather than carved out, which is what keeps the `Int` class visible.

Findings reconcile against `COMPONENT_PARITY_EXEMPT` keyed `(label, item, reason)` with the same
two-way staleness guard: an unexempted finding fails with the remedy; an entry matching no live
finding fails as "resurfaced". Its live entries are the `Int` value-type class (`Int::new_uint` /
`Int::new_nint` have nowhere to land in a `variant`, so a caller constructs the arm directly) and
`IntError`, the rust-only error enum minted beside `Int` — not an IR type at all, and the WIT face
carries every failure as a `string`. The sweep is the component fixture set plus
`tests/component-cycle/inputs`, whose deliberate refusal is pinned in `EXPECTED_GENERATION_FAIL` (a
listed label that starts generating fails as "the refusal is gone").
`component_api_parity_axes_and_pins_are_live` holds the whole-axis assertions, including that every
fixture `component_tests` compiles or validates is differentialled here too. Two vacuity guards: a
floor on the number of rust-surface obligations compared, and a stray-key guard over both generated
trees so a new emission surface fails loudly instead of escaping the differential.

### component behavior (`component_host_tests::component_host_behavior`, gate `component_host`)

The only gate that **runs** the component face. Its two siblings judge emitted bytes; a `.wit` that
resolves, encodes and validates over glue that compiles is still silent about every claim the
boundary actually makes at runtime — so this gate builds a real `wasm32-wasip2` component, loads it
into a `wasmtime` host and drives it through one `#[test]` per assertion class:

- **construction and accessor read-back**, every field type in the fixture;
- **byte-equality with the rust crate's own serialization**, both directions — the oracle is a path
  dep on the generated `rust` crate, so "the boundary agrees with the library it wraps" is checkable
  where "the boundary produced some bytes" is not;
- **fallible doors return `Err` and never trap**, and the instance is still usable afterwards. That
  last clause is the real assertion: a trap poisons the whole component instance, so in a composed
  topology one bad call kills a shared dependency for every consumer. The trap TEXT is deliberately
  never pinned;
- **`option<option<T>>` has three observable, distinct states** — the wasm face's flatten/`has_*`
  workaround is not needed here;
- **returned collections and handles are snapshots**, in the return direction and the parameter
  direction both;
- **the same handle as receiver and argument** (`x.set-children([x])`), the class with no wasm-face
  precedent and the reason the never-two-`RefCell`-guards emission invariant exists. Its structural
  twin is `component_tests::component_glue_never_holds_two_refcell_guards`, which reads the glue;
  this is the half that runs it;
- **the `any-cbor` edges**: canonical re-encode under the non-preserve posture (`9f0102ff` in,
  `820102` out — byte-exact against the rust crate, not against what the caller passed), and the
  one-item rule, where trailing data is rejected rather than truncated to the first item.

`wasmtime-wasi` in the linker is load-bearing rather than precautionary, and
`wasi_is_required_in_the_linker` is the negative control that says so: a wasip2 reactor imports
`wasi:*` interfaces even for a pure codec, so an empty linker fails instantiation before an
assertion can run.

The host crate is a **nested scratch crate the gate builds** (`tests/component-host/host/**`), never
a dev-dep of `cddl-codegen` — wasmtime in the bin crate's dev-graph would tax every `local`-tier
compile. It is COPIED into the generated output root before the gate-cache key is taken, because
every input a cached cell reads from scratch must live inside the hashed root (see "The gate cache"
above; `no_std_check.ts` is the shipped exemplar). Its `bindgen!` resolves the WIT by a relative
path and takes the built `.wasm` from an env var, so the hashed bytes are run-independent, and
`component_host_tests::the_host_crate_carries_the_files_the_gate_copies` asserts both properties
in-process. The cached closure checks more than a cargo exit code, so that verdict logic is
versioned into the key as an argv marker.

Measured on the delivering machine: **81 s cold** (a fresh scratch root, so wasmtime builds), **3 s
warm** (gate-cache hit), **9 s on a cache miss with the scratch root warm**. The scratch root is
checkout-hash keyed, serialized with `acquire_scratch_lock` and deliberately NOT deleted between
runs — that last number is why. Per-cell output trees are freed; the shared `target/` (≈3.5 GiB) is
what survives. A machine below the stated scratch or memory floor makes the gate print a loud SKIP
naming the measured number, never a silent pass.

### cross-crate composition (`component_compose_tests::component_compose_acceptance`, gate `component_compose`)

**The acceptance gate for the whole component face.** Two independently generated crates
(`tests/component-compose/dep` and `.../consumer`), built as two `wasm32-wasip2` components, composed
into one world, and driven through the flow the face exists for. Every gate above stops one step
short of it: `component_import_wasip2_build` proves the consumer's imported-resource glue *compiles*,
`component_host` proves *one* component behaves. Only here do two separately generated crates agree
at runtime — which is the claim, because without it a consumer's dependency types are its own private
resources, structurally identical to the dependency's and interchangeable with nothing.

- **the composed world exports BOTH interfaces**, read back out of the encoded artifact rather than
  out of the composer's graph. This is what `wac plug` destroys (it satisfies the consumer's import
  from the dependency's export and then drops that export) and what a host needs in order to mint a
  dependency object at all;
- **a dependency object crosses in and comes back live**: minted on the dependency's exported
  interface, passed into a consumer constructor, returned by a consumer getter, then read AND MUTATED
  through the dependency's own interface with the mutation visible on a later read. That last step is
  what distinguishes a live resource in the dependency instance's table from a value the host is
  holding. The returned handle is also asserted DISTINCT from the one passed in, and the borrowed
  original unaffected by the mutation — the seam is CBOR bytes, so the object is live and the value
  is a copy, and neither half may be read as the other;
- **the repeated position works end to end**, through the accumulator: elements pushed one at a time,
  read back as owned dependency handles, order preserved;
- **the composed boundary's bytes are the native crates' bytes** — the oracle is a path dep on both
  generated `rust` crates;
- **the instantiate-once mistake is invisible at compose time and fatal at runtime**, both halves
  pinned. A second dependency instance composes at exit 0 into a world whose exported interfaces are
  indistinguishable from the correct one's, and then the first handle crossing fails
  (`mismatched resource types`, wasmtime's text, asserted loosely). The first half is why the
  single-instantiation shape is a documented integrator obligation rather than something the composer
  enforces.

Composition runs **in-process via `wac-graph`**, never a `wac` binary whose version would sit outside
the lockfile between the gate and its verdict; it resolves `wasmparser`/`wasm-encoder`/`wasm-metadata`
at the same 0.247 floor the WIT gates pin, so composition adds no version skew.

The host crate is a **nested scratch crate the gate builds** (`tests/component-compose/host/**`),
copied into the generated output root before the gate-cache key is taken, for the same input-closure
reason as `component_host`. Its three path deps stay relative and both artifacts arrive by env var;
the two generated WIT packages are copied into `host/wit/deps/**` inside the same root, so a change to
either projection reaches the hand-written composed world the host binds against.
`component_compose_tests::the_compose_fixture_carries_the_files_the_gate_copies` asserts the copy list
in-process, and also that the package identifiers agree across the three places they are spelled:
`--lib-name X` mints `cddl:X@0.1.0`, which the composed world exports and the composer registers.

Measured on the delivering machine: **91 s cold** (a fresh scratch root, so wasmtime and wac-graph
build), **4 s warm** (gate-cache hit), **7 s on a forced run with the scratch root warm**. Same cost
class as `component_host` and cheaper on the re-run path, which is why it sits at `local`. The scratch
root is checkout-hash keyed, serialized with `acquire_scratch_lock` and deliberately NOT deleted;
per-cell output trees are freed, the shared `target/` (3.3 GiB) survives. It shares `component_host`'s
scratch and memory floors and its loud-SKIP behaviour, never a silent pass.

### the JS host (`component_jco_tests::component_jco_js_host`, gate `component_jco`)

**The face as its real audience meets it.** Every gate above judges the component from Rust; the
motivating consumer is a JS dApp, which reaches it through a *transpiler* (`jco`) rather than a
runtime — and the surface jco synthesizes is not the wasmtime one. The gate builds real
`wasm32-wasip2` components, transpiles them, and drives the result with node's built-in test runner.

Fixtures are **reused, never duplicated**: the specs are `tests/component-host/inputs` and
`tests/component-compose/{dep,consumer}`, the same ones the two wasmtime gates drive. So a
disagreement between this gate and them is a finding about the *host*, not about the emitter. Only
the JS drivers in `tests/component-jco/js/**` are new.

Three legs:

- **surface** (one component, transpiled alone) — every consumer-visible row of the JS face, asserted
  at run time rather than read off the emitted `.d.ts`, because the two disagree: a WIT `enum` arrives
  as a *string label* and rejects the numeric discriminant the wasm-bindgen face takes. Also the
  `{tag, val}` `int` bridge, the three distinct states of `option<option<T>>`, `any` as a
  `Uint8Array` with trailing data refused, a fallible door *throwing* a `ComponentError` whose
  `.payload` is the WIT string, the instance surviving that failure, the despecialized NonEmpty bound
  re-imposed at the consuming setter, snapshot-not-alias getters, the same-handle re-entrancy case,
  and `[Symbol.dispose]` as an **own property** of each instance (never on the prototype — a consumer
  must feature-detect on the instance) with the post-dispose `TypeError` observable;
- **crosscrate** (two components, transpiled *separately* and wired by jco's `--map`) — the full
  acceptance flow, and the shape the docs prescribe. Instantiate-once, which `component_compose` has
  to pin by hand because a composer cannot promise it, falls out of ES module semantics here;
- **composed-is-broken** — a **known-broken pin**. The `wac`-composed dual-export world
  `component_compose` drives correctly through wasmtime is broken under jco 1.26.1: jco allocates a
  separate handle table per component *instance* for the same resource type, so a dependency handle
  throws on the way in and a dependency-typed getter silently returns a *different object* on the way
  out. Pinned because the second symptom is silent. Its failure messages read as the good news they
  would be. This leg **loud-skips alone** when the ambient `wac` is absent or below 0.9 — it is the
  gate's only ambient-binary dependency, and the other two legs never wait on it.

The npm dependencies are `@bytecodealliance/jco` and `@bytecodealliance/preview2-shim`, pinned
**exact** in a committed `package-lock.json` and installed with `npm ci` (never `npm install`, which
would resolve a newer jco and answer a question about a version nobody committed; the shared
TS-toolchain now follows the same exact-resolution posture in its test-only committed lock, while
the generated package it tests retains its shipped caret ranges). The shim is
installed but **not mapped**: jco rewrites the `wasi:*` imports to it by default, and a hand-written
`wasi:*` map breaks the output. There is no test-framework dependency — `node --test` and
`node:assert`.

Scratch layout is load-bearing. `node_modules/` sits at the scratch **root**, outside the hashed
tree (a ~200 MB install that has nothing to do with any assertion); node finds it by walking up from
the transpiled modules. The fixture's `package.json`/`package-lock.json` therefore land in *two*
places — at the root, where `npm ci` consumes them, and inside the hashed `out/js/`, so a version
bump moves the gate-cache key. That second copy is the input-closure rule (see "The gate cache"): a
lockfile bump that did not move the key would serve the stale PASS forever. The ambient `wac` version
is in the key too, because it decides *which* legs a cell covers.
`component_jco_tests::the_jco_fixture_carries_the_files_the_gate_copies` asserts the copy list, the
two exact version pins, that no driver imports by an absolute path, and that the interface
identifiers the drivers spell agree with the `--lib-name` values the gate generates with.

Measured on the delivering machine: **17 s cold** (a fresh scratch root, so `npm ci` and the three
guest builds run), **4 s warm** (gate-cache hit — which still pays the six-manifest
`cargo generate-lockfile` preflight), **3 s on a forced run with the scratch root warm**. The
cheapest gate in the component group: no wasmtime, no composer crate, no native host crate.

It sits at `local` on both counts a placement has to answer — cost (above) and **provisioning**. node,
npm and a cold run's npm-registry access are more fragile than a rustup target, so the outcome is
tier-dependent: a loud SKIP at `local`, a hard FAIL at `full` via `CDDL_JCO_REQUIRED=1`, which the
`fn`-shaped registry row sets when the tier is `full`. Same mechanism and same reason as
`no_std_check`'s absent-target outcome: a silent skip in the tier that *ships* the feature would void
the guarantee that tier exists to give. It reuses `component_host`'s scratch and memory floors.

### component compilation at corpus breadth (`component_tests::component_corpus_compiles`, gate `component_corpus_compiles`, `full`)

Every `tests/corpus/*.cddl` fixture's emitted component crate, type-checked for `wasm32-wasip2`.

It exists because nothing else can reach that breadth.
`integration_tests::feature_corpus_compiles` structurally cannot: it hardcodes its crate list to the
rust and wasm trees and runs a HOST `cargo check` with no `--target`, so the wasip2 component crate
is invisible to it — which is why the `ALL_PROFILES` component row filters OUT of that gate rather
than flowing into it. `component_tests::component_crate_builds_for_wasm32_wasip2` compiles its
`BUILD_SMOKE_FIXTURES` set of representative fixtures; this asks the same question of all of them,
and the answer differs, which is the whole argument for it.

**`check`, not `build`.** The link is already asserted on the representative fixtures by the build
smoke; the class corpus breadth catches is glue naming a trait, method or macro the bindings never
minted, and that is a type-check failure. Probed rather than assumed — `cargo check` expands
`wit_bindgen::generate!` and reports every class the ledger below records.

**No sharding**, sized from this gate's own measurement rather than from
`feature_corpus_compiles`' curve: the first cell costs ~10 s (it builds the shared dependency graph),
then ~0.4 s each. Measured **140 s cold / 97 s warm** — a warm run still pays generation and the
lockfile preflight per cell, which is most of what is left.

`EXPECTED_COMPILE_FAIL` is the ledger of fixtures whose glue does NOT compile, keyed
`(stem, reason)` and guarded both ways: a listed fixture that starts compiling fails as "the bug is
fixed — remove the pin", an unlisted one that stops fails as a regression. Every entry is a FINDING
this gate made rather than a decision. No fixture is ledgered today: the typed conversion walk
threads the rust shape through nested lists and table rows, stages despecialized collections in
their loose carriers, and re-enters `NonEmptyVec`/`NonEmptyMap`/`BoundedVec`/`BoundedOrderedSet`/
`BoundedMap` through
`TryFrom`.
The two classes the gate found in the GUEST BLOCK
itself — a world exporting no interface still emitting `export!(Component);`, and an interface of
only value types still emitting `impl wit_types::Guest for Component {}` — are fixed, and the
emission conditions that replaced them are pinned in `src/tests/component_tests.rs` by
`component_glue_emits_the_guest_block_only_where_generate_mints_one`,
`a_value_only_interface_gets_no_guest_impl_beside_an_interface_that_does` and
`a_free_function_alone_mints_a_guest_trait`. Fixtures that cannot generate belong in
`snapshot_tests::PROFILE_GENERATION_SKIP` instead. Fixtures whose RUST crate references
user-supplied code get that code SEEDED before the check (`integration_tests::append_corpus_defs_for`
over `tests/def_templates/`, because the component crate takes the rust crate as a path dependency
and needs exactly the seeding `feature_corpus_compiles` does); it also shares the currently-empty
`integration_tests::COMPILE_SKIP` rather than restating a future blocker list.

### component build sweep over the decode catalog (`cddl-matrix/verify.ts --component-build-sweep`, gate `component_build_sweep`, `full`)

Every drivable row of `tests/decode_conformance/catalog.toml`, generated `--component=true
--wasm=false` and BUILT for `wasm32-wasip2`. 197 of the catalog's 221 rows are drivable; the other 24
are pinned entries, which are vector-less AND spec-less by construction and so have nothing to
generate from.

It is the class-level finder for component-face compile classes, and it exists because a hand-picked
compile corpus cannot be assumed to cover a class just because the class is ordinary. The two
component-glue compile classes closed in 2026-08 — a rule whose WIT resource name is exactly `t`, and
an `any` member reached through a transparent alias — both sat in ORDINARY matrix rows
(`type.choice`, `prelude.any`) that no fixture spelled, and both were found by hand-running
generation+build over candidate rows. The sweep is that procedure made repeatable over an enumeration
nobody curated for the component face. The section above asks the same question of the corpus
fixtures; the two enumerations are independent by construction, which is the whole argument for
having both. The breadth BOUND is the catalog itself: the sweep sees exactly the combinations the
catalog spells, and the catalog carries control operators only as their dedicated `ctl.*` rows —
one position each; no `contain.*` row crosses a control op with a containment position — so a
compile class living only in an unspelled combination is invisible to it. That bound is proven,
not hypothetical: the 2026-08 `.default` coherence cycle found FOUR such classes by hand
boundary-probing — among them `.default` on an array-record member (exit-0 non-compiling RUST
output) and on a mandatory member (exit-0 E0061 on the wasm and component faces) — all invisible
to the sweep and to every other breadth gate until the product was spelled into the corpus
(fixed; `default_mandatory`, `default_array_rep`, `default_scalar_kinds`). Growing that
cross-product is the matrix's own enumeration program, not a gap in the sweep — owned by
`cddl-matrix/roadmap.toml` § Expansion's "A supported control's HOST-PLACEMENT spelling space"
entry.

**Depth of exactly two stages**, deliberately: generate, then build. No host, no oracle, no vectors,
no drive — that is `verify.ts`'s component EXECUTION leg, which asks whether thirteen chosen rows
BEHAVE. Breadth and depth are separate instruments and this is the breadth one. A consequence worth
stating: the sweep consults no CDDL oracle at all (its per-row verdict is a cargo exit code over
committed `spec` strings), so it is the one `verify.ts` path that skips the oracle-identity
fingerprint — requiring the pinned fork would refuse the sweep on a machine that builds components
perfectly well, to validate a tool the run never invokes.

**Writes nothing** — no annotations, no `verify_report.json`, no catalog. A sweep leaves the
committed tree byte-identical, which is what makes it safe to run at any time.

**Verdicts.** A row the generator REFUSES is counted and printed but is NOT a failure: the sweep
enumerates every drivable row, including shapes cddl-codegen declines, and refusal is the matrix's
own business. A row that GENERATES and does not BUILD is the finding; the run exits nonzero listing
each such cell with its compiler stderr tail and the harness-free repro recipe. The classifier behind
that split is pinned by a pure startup self-test (gate `verify_selftest`), because a build failure
misread as a refusal is a green run with no other symptom. Two further floors: a catalog yielding
fewer than 120 drivable rows is refused rather than swept (a breadth instrument that covers almost
nothing must not pass), and a whole-catalog sweep in which NO row generated is a harness failure
rather than a pass.

**`SWEEP_EXPECTED_BUILD_FAIL`** holds the rows that generate and do not build today — the sibling of
`component_tests`' `EXPECTED_COMPILE_FAIL`, on the same terms. Every entry is a FINDING this
instrument made and an open defect in `cddl-matrix/roadmap.toml` § "Findings — open", never a decision
to stop looking, and each is guarded BOTH ways: a listed row that starts building — or stops
generating, which makes its expectation unreachable — fails the sweep as a stale entry to retire
alongside its findings entry, and an unlisted row that stops building fails as the new finding. An
entry naming a row the catalog no longer offers fails the same way. That is what keeps the ledger
from degrading into a skip-list, and the ledger dimension rides the same pure self-test the
classifier does.

No row is ledgered today — every drivable row's component crate builds. Empty is the healthy state
for this ledger, not an unused one: the both-ways guard above is what keeps it that way, and an
entry is added the moment the sweep finds a row that generates and does not build.

**Subsetting**: `--only=<row-id>[,<row-id>…]`, validated against the drivable set, so a typo fails
loudly instead of sweeping nothing.

**Cost** is tracked under `component_build_sweep` in `tests/timings.json`; the gate reports the current
row count and elapsed time on every run. A warm run still pays generation, the lockfile preflight and
the tree hash per cell, which is nearly all of what is left.

**Memoization** under the cache label `verify.component_build_sweep` — a namespace DISJOINT from the
execution leg's `verify.component_probe` on purpose: this closure asserts strictly less about the
same tree, so serving one leg's hit to the other would overclaim in one direction and re-run
needlessly in the other. Its argv carries the verdict marker `component-build-sweep-v1`. The whole
closure lives inside the hashed tree — the generated crates plus the workspace `Cargo.lock` the
preflight writes before the hash, with toolchain, `RUSTFLAGS` and cargo config folded in by the key
function; unlike the execution leg there is no host binary to hash, which is why its argv carries no
`host=` component. `cddl-matrix/audit_gate_cache_closure.ts` does not trace TS-side `verify.*`
labels, so this label shares its siblings' posture: the closure argument rests on review at the call
site plus `GATE_CACHE=0`, not on the strace audit.

### Regen over prior output at corpus breadth (`src/tests/regen_over_prior_tests.rs`, gates `regen_over_prior_output_corpus` `local` + `regen_over_prior_output_corpus_compiles` `full`)

The comment-preservation overlay runs on exactly one path — `export()` over a tree a previous run
wrote — so every gate that generates into a clean directory is blind to it, and the two gates that
were not (§ "Preservation-merge fixtures") name their two files by hand. This pair asks the same
questions of every `tests/corpus/*.cddl` fixture, with no file named in advance. It found a third
instance on its first run: the wasm `Int` wrapper's `from_str` body carried an own-line
`// have to redefine so it's visible in WASM`, which a rule deletion stranded into a
self-perpetuating `compile_error!` sentinel in `wasm/src/generated/mod.rs` on three fixtures. Fixed
where the class is always fixed — emitter-side (`generation/wrappers.rs`); the rationale moved into
the generator's own source, which is where a maintainer note belongs.

The same static floor later caught six more generated-row violations when the expected-conversion
fixture first exercised `AnyCbor`: comments beside the full-domain `NInt` variant, the
wire-order/duplicate-preserving `Map` variant, and the bounded-allocation `Vec::new()` appeared on a
code row in each of the preserve and non-preserve runtime flavors. They were moved onto standalone
rows in `static/any_cbor_{preserve,non_preserve}.rs`. No new roadmap item was needed: the local-tier
`regen_over_prior_output_corpus` gate caught the entire class before wrap-up.

Three legs, all over the tool-owned trees (`rust/src/generated`, `wasm/src/generated`,
`wasm/json-gen/src/generated`). Legs 1 and 2 sweep TWO emission profiles — the default and
`--preserve-encodings`, which emits a file the default does not (`cbor_encodings.rs`, one struct per
rule that carries encodings: a per-rule surface is exactly where a deletable-row comment would live).
Leg 3 stays on the default profile, because an injected replace block costs three generations plus,
for its compile gate, nested cargo, where a floor-and-deletion profile costs one generation each:

1. **The static floor.** Every generated `.rs` of a FRESH generation, scanned for a comment that
   shares its row with code — the trap SOURCE, catchable before any deletion exists. Lexer-grade via
   `comment_preserve::comments_sharing_a_code_row`, not a `line.find("//")` scan: this corpus emits
   a URL inside a string literal and a `/*` inside an own-line banner, and a textual scan calls both
   comments.
2. **The rule-DELETION variant.** One rule (and the transitive closure of rules referencing it) is
   removed from the spec, the variant is regenerated IN PLACE over the fresh output, and any
   `cddl-codegen:unpreserved-comment` anywhere in the trees fails the cell. Deletion by whole LINES
   rather than by the `cddl` crate's AST span, because a rule's comment-DSL directives live in the
   `;` comment trailing it — an AST-span deletion would leave `; @used_as_key` behind to re-attach
   to a different rule, silently changing what the variant means while still generating.
3. **The user-EDIT variant.** One canonical `cddl-codegen:replace` block (the pinned grammar, the
   recorded original taken verbatim from this run's own output) stubs a `self`-only function body,
   after which the regen must apply the block rather than strand it, and a SECOND regen must reach a
   byte-identical fixed point. Sites with parameters are excluded on purpose: a stubbed body would
   leave them unused, and the compile gate's `unused variable` scan must stay a statement about the
   generator.

**Why the compile half is its own gate.** Whether the regenerated crate still BUILDS warning-clean
is the only leg that pays nested cargo, and it is the one that catches the orphaned-`use` class —
`unused import` is a WARNING, so no assertion about generation exiting 0 can see it. It reuses
`feature_corpus_compiles`' scans, its currently-empty `COMPILE_SKIP` (a future whole-fixture blocker
would hold after regeneration for the same reason) and its def splice, and is gate-cached per
generated-crate content hash with its own `regen-edit=v1`
verdict-logic marker. The splice is applied ONCE, before the regeneration, which makes the seed-once
thin `lib.rs` contract part of what this gate asserts: a consumer's hand-written extern definitions
must still be there, and still resolve, after the tool has run over their tree a second time. Its own
`REGEN_SKIP` holds the other half of that contract — a bare (non-path-qualified) `@custom_serialize`
name is resolved through the CLOBBERED `generated/` scope, so the hand `use` the docs prescribe does
not survive a regen and the durable spelling is the fully-qualified codec path.

A secondary profile's GENERATION verdict is deliberately not this gate's: a fixture that does not
generate under `--preserve-encodings` is recorded as a skip, because `feature_corpus_compiles` owns
that question both ways through `EXPECTED_GENERATION_FAIL` (today, `dsl_ignore`).

**Vacuity floors**, in the module and asserted from the summed run: files scanned, deletion cells,
deletion cells that delete a `@used_as_key` rule, edit cells, edit cells landing in a per-type
surface (rather than in a composed static runtime file, whose bytes are the same for every fixture),
and edit cells that orphan an import. The last is the one that is genuinely low rather than merely
conservative — whether a `self`-only body happens to hold a same-file import's last use is a
property of what the corpus emits, not something the sweep can arrange.

**Measured on the delivering machine:** the generation sweep **55 s** (six worker THREADS over
generator subprocesses — threads rather than sibling `#[test]` shards because every `#[ignore]`d
test needs its own registry entry, and six shards would be six gates for one question), which is
what places it at `local`; the compile gate **159 s cold / ~90 s fully cache-hit** — a warm run
still pays the per-cell generation, the injection and the lockfile preflight, which is most of what
is left.

**Residual, a property of this corpus:** no fixture carries two `@used_as_key` tags, so "a
`key_demand_assertions.rs` ROW disappears while the file survives" is not expressible here —
deleting the sole tagged rule stops the file being generated at all, and the overlay then never
runs over it. The chooser still prefers a tagged rule in a tagged fixture, and that shape stays
covered by `extern_interface_check_regen_over_deletion_no_trap`'s hand spec.

## multifile placement matrix (`tests/matrix_multifile/` + `integration_tests::multifile_matrix_{compiles,roundtrips}`)

A **coverage-by-construction** gate for the axis every OTHER construct gate is blind to: **module
placement**. The corpus gates, the wasm-ABI matrix, and the parity differential all feed the
generator SINGLE-file specs, so every construct is only ever verified in root scope. Multifile
emission branches on scope — `mark_refs` (`intermediate/mod.rs`) resolves each collection
occurrence's wasm wrapper NAME and HOME scope through `IntermediateTypes::wasm_collection_wrapper`
(the emitter's `for_wasm_member` twin, `table_shape_sole_owners`-aware), while the wrapper/alias
definitions land wherever `types.scope(ident)` puts them — and that region had exactly one hand
fixture
(`tests/multifile`, which covers NAMED cross-module refs but no structural-wrapper-ownership
cells). This sweep enumerates the placement grid, compile-floors it (always-on), and round-trips it
(manual, full tier). Three placement vectors the grid does NOT enumerate are hand-fixture-owned:
the group-choice-VARIANT reference position (`tests/multifile`, see Axis 2 below); the
extern-shaped type-alias-TARGET position (`tests/extern-generic-scoped` — a generic-EXTERN
instance aliased from a non-root scope decomposes into a base import at the base's declaring
scope plus argument imports, never the whole `Base<Args>` type expression; extern shapes sit in
this grid's SHAPES exclusion, so the compile floor can never enumerate them — see Axis 1); and the
open-rest CONTAINER position (`tests/multifile`'s `open_flat` / `open_nested` / `open_tail` in
`qux.cddl`, whose key/value/element live in `a`, `a/c/foo` and `b/bar`). The last one is a SHAPES
exclusion of a different kind: a rest row/tail exists only INSIDE a record, so it has no
self-contained rule spelling to occupy an `a.cddl` cell with, while its container — re-assembled
by `RestRow::container_type`, since the IR stores the inner types flat — needs the same
wrapper-into-the-using-scope + inners-at-the-emission-scope routing a map/array FIELD gets. Both
sides of that routing dangled (`E0425` on the wrapper in `qux` and on its key/value/element at
root) until the rest arm of `scope_references` marked the container instead of the inner types.

Pipeline (projection → fixtures → gates), the same two-gate shape as the wasm-ABI matrix:

```
cddl-matrix/project_multifile_matrix.ts  ─►  tests/matrix_multifile/<shape>__<mode>/{lib,a,b}.cddl  ─►  integration_tests::multifile_matrix_compiles
     enumerate {shape × ref-mode}             two-module DIRECTORY fixture per cell                     generate --wasm=true (dir input), cargo check the wasm crate
                                                                                                    ─►  integration_tests::multifile_matrix_roundtrips (#[ignore]d)
                                                                                                        generate --wasm=true --emit-tests=true × ALL_PROFILES, cargo test rust/ + wasm/
```

- **The two-module template.** Each cell is a DIRECTORY fixture. `lib.cddl` (file stem `lib` ==
  `ROOT_SCOPE`) is the root — one trivial rule (`rt = [uint]`), constant across cells; `a.cddl` (scope
  `a`) holds the shape's defs; `b.cddl` (scope `b`) holds the reference. The `rootref` mode is the one
  exception and the reason the REFERENCING module is an axis at all: its reference lands in `lib.cddl`
  beside the root rule and the cell carries no `b.cddl`. Root-owner direction (shape
  in root, referenced from a module) is deliberately NOT enumerated — root-module owners probed fine
  in both directions, so the non-root-owner cells are the discriminating ones.
- **Axis 1 — type-shape** (`SHAPES`, checked as an executable superset of the bounded top-level
  declaration in `project_wasm_matrix.ts`, parsed without importing that side-effecting projection;
  multifile-specific placement-only shapes are also allowed when their cross-module class has no
  wasm root-scope counterpart — `collrec` (`[* <record>]`) and `tblrec` (`{ * <record> => text }`)
  are representative examples, not a closed list). Every self-contained shape that HAS defs is
  included. The exact live exclusion ledger is `prim` (no defs — nothing to place in a module) and
  `extern`/`rawbytes` (user-supplied types, can't compile standalone); the projection asserts each
  remains present in wasm and absent locally. The exclusion bounds the
  GATE, not the generator: extern shapes still have placement behavior (re-export glue routing;
  generic-instance alias decomposition), and its alias-position residue escaped to a production
  regen as feature request 07 — now hand-pinned by `tests/extern-generic-scoped`
  (`extern_generic_scoped` + `extern_generic_scoped_alias_imports`).
- **Axis 2 — cross-module reference mode.** `named` — `b` references the shape's named rule
  (`bholder = [field0: <ty>]`); `aliased` — `b` ALIASES it (`bal = <ty>`, a plain rule alias whose
  emitted `pub type Bal = …;` names the cross-module target with no field reference in sight — the
  `scope_references` type-alias walk, the reference position a consumer's
  `policy_id = script_hash`-style domain aliasing hits, proven E0412 in production before the walk
  landed; module `b` is alias-only on purpose, that being the production shape, with the
  alias-only-module E0583 class independently pinned by the alias shapes' `unref` cells); `anon` —
  `b` embeds the shape's inline anonymous same-shape spelling
  (the `mark_refs` structural class); `anonb` — `anon` plus a ballast record rule in `a`
  (`ballast = [bal0: uint]`), so `a` emits `serialization.rs` and an alias-only-module abort can't
  mask the b-side import verdict (the discrimination that isolates structural-import placement
  regressions); `unref` — `b`
  references nothing (`[field0: uint]`), so an alias/table-only module `a` still gets emitted.
  `named`/`aliased`/`unref` apply to every shape; `anon` exists ONLY for a shape whose anon holder
  `holder = [field0: <anonForm>]` compiles GREEN as a **single-file control** — otherwise the red
  would be a single-file limitation, not a placement finding, and the shape carries no `anonForm`
  (the controls are throwaway, not committed). All 13 candidates
  (`coll`/`collmap`/`collrec`/`tblrec`/`tag`/`nullable`/`bwrap`/`cborwrap`, the restricted
  non-empty shapes `necoll`/`necollrec`/`nemap`, and the synthesized-NonEmpty shapes
  `nesyncoll`/`nesynmap`) probed green. `anonb` applies to
  exactly the anon shapes whose plain `anon` cell would be masked by an alias-only module `a`
  emitting no serialization (`coll`/`collmap`/`nullable`/`necoll`/`nemap`); the other anon shapes'
  module `a` already emits serialization, so nothing masks their b-side verdict and a ballast
  variant adds no discrimination (their `anon` cells are green).
  `rootref` — the anon spelling placed in the ROOT scope (`rootholder = [field0: <anonForm>]` in
  `lib.cddl`, no module `b`), which is the REFERENCING-MODULE variation every other mode holds
  constant at `b`. Root is where `mark_refs` resolves a structural wrapper differently, in two ways a
  `b`-side reference cannot reach: a wrapper sole-owned by module `a` must be IMPORTED into root
  (`use a::MapU64ToText;` for `collmap`, `use a::{Foo, MapFooToText};` for `tblrec`), while one the
  root holder itself owns is minted AT root and named bare with no import (`FooList` for
  `collrec`/`necollrec`, `MapU64ToText` for `nemap`, and the synthesized `NonEmptyFooList` /
  `NonEmptyMapU64ToFoo` for `nesyncoll`/`nesynmap`). It applies to exactly those seven
  WRAPPER-MINTING anon shapes (`EXPECTED_ROOTREF_SHAPES`): a shape whose anon form lowers to a
  transparent core type (`coll`, `necoll`, `nullable`, `tag`, `bwrap`) or to a plain user-named rule
  (`cborwrap`) puts nothing structural in root's way, so its rootref cell would only repeat `anon`
  with the holder moved. All seven probed green.
  The field-embedding modes reference the shape from a record-FIELD position; `aliased` is the
  type-alias-TARGET position (added when its import class escaped to production — the second
  position-keyed escape after the group-ctor one below); other reference POSITIONS are
  not enumerated. The one known position-keyed import class — a group-choice VARIANT over a
  foreign-scope Record, whose expanded `new_<variant>` ctor names the record's field types in the
  choice's module (marked by `scope_references` via the shared
  `EnumVariant::group_ctor_record_fields` helper) — is pinned by the hand fixture instead
  (`tests/multifile`: `relay` in `qux.cddl` over `relay_host` in `b/bar.cddl`, test
  `cross_module_group_choice_ctor`, compiled rust+wasm under both fixture profiles); the mode-axis
  extension is recorded recur-first in `tests/testing-roadmap.toml` ("Multifile reference-POSITION
  coverage").
- **The compile floor** (`integration_tests::multifile_matrix_compiles`) globs the cell dirs,
  generates each with DIRECTORY input `--wasm=true`, and `cargo check`s the wasm crate ONLY (which
  path-depends on the rust crate, so rust-side breakage surfaces transitively). Its shared target uses
  distinct per-cell rust/wasm package identities, preserving the `cddl-lib` source alias with Cargo's
  `package = ...` dependency selector, plus one uncached old-enough marker-bearing expected-red
  canary per shard. Own scratch + `CARGO_TARGET_DIR` (`cddl_codegen_multifile_matrix`). Always-on (no `#[ignore]`): it joins the
  default `cargo test` / check.ts local tier. Its measured duration belongs in `tests/timings.json`;
  fixture breadth is derived by the projection rather than hand-counted here.
- **The round-trip gate** (`integration_tests::multifile_matrix_roundtrips`, `#[ignore]`d, check.ts
  **full** tier — the behavioural upgrade, mirroring `wasm_matrix_roundtrips`): same cell
  enumeration, but each cell is generated `--wasm=true --emit-tests=true` across `ALL_PROFILES`
  minus the component row (default / preserve / json) and `cargo test`ed, so the minted `cddl_generated_tests` /
  `cddl_generated_wasm_tests` modules RUN the cross-module wiring — module `b`'s holder is
  constructed from module `a`'s shape (`Bholder::new(St::new(..))`) and round-tripped, and the wasm
  twin byte-differentials against the fully-qualified `cddl_lib::b::…`/`cddl_lib::a::…` natives.
  (`aliased` cells have no `bholder` — module `b` is a lone `pub type`, whose transparent alias
  mints no standalone test surface, so their round-trip value is module `a`'s own surface plus the
  compile proof that the alias line's cross-module import resolves; the compile floor is those
  cells' discriminating gate.)
  BOTH generated subcrates are `cargo test`ed (`rust/` then `wasm/`): the rust crate's
  `#[cfg(test)]` module is not compiled when it's built merely as the wasm crate's dep, and the
  proven placement classes are rust-side. Own scratch (`cddl_codegen_multifile_rt`) +
  `acquire_scratch_lock`, one shared `CARGO_TARGET_DIR`, each per-cell dir freed after its verdict.
  Loud-skip contract as the wasm round-trip gate: a cell minting no test surface passes with zero
  tests (the emitter eprintln!s the skip; the floor still pins its ABI) — and a minted-module
  vacuity floor (each generated crate's root `generated/mod.rs` is grepped for its test module)
  bounds the aggregate so green can't quietly go vacuous.
  The multifile `--emit-tests` emission itself (root-level test module + `use super::<m>::*;` scope
  globs, without which every multifile cell is E0433-uncompilable) is pinned always-on by the
  in-process `emit_tests_multifile_scope_imports`, so a regression there doesn't wait for full
  tier. Run with `cargo test --bin cddl-codegen multifile_matrix_roundtrips -- --ignored`
  (every run is effectively cold — the scratch root, shared target
  included, is cleared at start and end — with the deps built once up front and the remainder
  dominated by the per-cell-per-profile generate + two nested `cargo test` invocations (3 profiles x the cell count each).
- **Skip ledgers (round-trip gate).** `MULTIFILE_ROUNDTRIP_SKIP: &[(&str, &str)]` (cell stem,
  reason) holds cells red in EVERY profile — currently EMPTY (every cell compiles and round-trips).
  No rustc-error-code class assertion here: the compile floor's `MULTIFILE_MATRIX_SKIP` already pins
  each cell's exact class. `MULTIFILE_ROUNDTRIP_PROFILE_SKIP: &[(&str, &str, &str)]` (profile, cell
  stem, reason) holds profile-specific reds — also EMPTY: the sweep is green across all three
  profiles. Both are four-state (red+listed = expected; red+unlisted = fail; green+listed =
  resurfaced — remove the pin; green+unlisted = pass) with up-front stale-key guards (unknown stem or
  profile fails before any heavy work). Verify a new guard the poison way: pin a green cell →
  resurfaced failure; add a bogus stem → stale-key failure; revert.
- **Skip ledger (compile floor).** `MULTIFILE_MATRIX_SKIP: &[(&str, &[&str], &str)]` (cell stem, expected rustc
  error codes, reason) holds the deliberately-red cells, four-state like `WASM_MATRIX_SKIP`:
  red+listed = expected; red+unlisted = a new placement finding to fix or (deliberately, with a
  roadmap entry) pin; green+listed = "resurfaced — remove the pin (a fix landed)"; green+unlisted =
  pass. **Class assertion:** a red+listed cell is NOT satisfied by any redness — the observed rustc
  error-code set (`rustc_error_codes` scans the captured cargo stderr for `error[E####]` headers)
  must EQUAL the pin's declared set, or the gate fails loud with "the cell's failure class changed —
  re-triage the pin" (set equality is the contract, never subset — pin the full honest observed set
  if a cell co-emits multiple codes); a listed cell whose GENERATION aborts is likewise a class
  mismatch (the pin claims a rustc compile error, and a generation abort produces none). Author a
  new pin's codes from the observed evidence, not the expected diagnosis: the gate's red-cell
  failure output prints the captured cargo stderr, whose `error[E####]` headers are exactly the set
  to pin. An up-front stale-key
  guard rejects a listed stem absent from the projected set, and a missing wasm crate is handled
  symmetrically. Verify a new guard the way these were: temporarily poison a key (bogus stem →
  stale-key fail; drop a real pin → the red cell fails with the remedy; pin a green cell → resurfaced;
  change a real pin's error code to a bogus one, e.g. `E9999` → the class-changed message fires),
  watch it fail, revert.

**What it guards today.** Every projected cell compiles and round-trips — both skip ledgers are
empty. Greenness rests on emitter invariants this matrix guards, each once a loud cross-module
failure class: every cross-module collection occurrence imports the SAME wasm wrapper the emitter
names, from the module it is minted in (`scope_references`/`mark_refs` resolve the wrapper name +
home scope through `IntermediateTypes::wasm_collection_wrapper`, the `for_wasm_member` twin, so
import and emission placement cannot disagree — the restricted `[+ T]`/`{+ k => v}`/`@duplicates
preserve` `NonEmpty*` family and the synthesized-NonEmpty facet `nesyncoll`/`nesynmap` all name the
`NonEmpty*` wrapper the emitter uses, not the pre-NonEmpty spelling); each restricted wrapper's loose
`try_from(&Loose)` source is imported at the wrapper's emission scope
(`register_root_non_empty_{list,map}_source`, the non-deferred analogues of the keys-list/deferred
helpers — the E0425 class the `necollrec`/`nemap`/`nepmap` cells guard); and a field referencing a
NAMED collection rule (`recs = [* foo]` / `gcn = gcoll<foo>`, or a DEP-owned `@rust_name`-pinned map)
imports only the rule ident, never a structural wrapper the rule's own class subsumes (the `Alias`
arm suppresses the structural-wrapper import when the alias names a collection rule — the E0432 class
`collrec__named`/`gcolln__named` guard, and the dep-owned flavor
`dep_owned_named_collection_no_local_structural_import` guards, with its cross-crate compile
companion `dep_owned_named_collection_compiles` building both generated crates against the
stand-in dep pair so the pinned-absent dangling import is also caught as the E0432 it would be).
Greenness also rests on four further
emitter invariants this matrix guards: a module
declares `pub mod serialization;` only when that file is written (the module-declaration loop in
`generation/export.rs` shares the `serialize_scopes` predicate with the file-write, so an alias/enum-only
non-root module cannot declare a phantom module — the E0583 class); an anonymous same-shape table
used cross-module imports the structural wrapper from the sole owner's module
(`scope_references`/`mark_refs` consult `IntermediateTypes::table_shape_sole_owners`, the same
helper the wasm emit path uses, so import and emission placement cannot disagree — the E0432
class); a cross-module *named* reference to a `.cbor` wrapper imports the inner named type into
the referencing module (`mark_refs`' Alias arm recurses into the alias target so idents the inlined
serialization names get imported — the E0433 class); and a non-root table class whose KEY is
non-exposable imports the keys-list wrapper its `keys()` accessor names bare — the root-minted
`<Key>List` (`register_root_keys_list`) OR, when that keys-list workspace/index-defers to a
dependency, `use <dep_wasm>::collections::<KeysList>;` (`register_deferred_keys_list`) — with BOTH
homes registered at both the inline-`Map` and the NAMED-`Table` struct-walk arms, mirroring
`codegen_table_type`'s emission condition (the `tblrec` E0425 class, guarded green by all three
`tblrec` cells and exercised end-to-end by `wasm_collections_index`'s record-keyed non-root table;
the deferred flavor for a NAMED table over a dep-owned key — E0412 when its import is stranded —
by `workspace_dep_named_table_deferred_keys_list`). The two gates split the verdict: the
always-on **compile floor** pins that every non-pinned cell's cross-module wiring type-checks (its
four-state class-asserting verdict stays live so any regression re-pins with the observed
error-code evidence), and the full-tier **round-trip gate** executes that wiring across all three
profiles — a green placement cell is semantically verified once both hold (first full sweep:
every non-collrec cell green under default, preserve, and json).

**Adding / changing cells.** Edit `SHAPES`/`MODES` in the projection, `bun run
project_multifile_matrix.ts`, review the new fixtures, run the gate. Output is deterministic — **never
hand-edit `tests/matrix_multifile/`**; `--check` is the drift gate (stale/missing/orphaned dir or
file). `EXPECTED_CELLS`, `EXPECTED_ANON_SHAPES`, and `EXPECTED_ANONB_SHAPES` guard the grid, so a
shrink/growth is an explicit reviewed edit.

## Shape-recombination fuzzer (`tests/recomb/` + `src/tests/recombination_tests.rs`)

Deterministically recombines the matrix's per-feature examples into composed CDDL specs that no
single-example gate samples, and runs them through the generator with escalating oracles. The
motivating gap is proven, not speculative: every other gate samples ONE minimal example per feature
row, and the map-rep group-choice fix found three defects hiding in unsampled shape *variants* of a
single "supported" row. The harness varies exactly the axes that mattered there: multi-member shape
variation inside one construct (a member-kind table: fixed uint/text/bool/null values, keyed
scalars, optional / zero-permitting occurrences, inline groups, filler-typed members, and a
tagged-optional kind — a tag head over a named `T / null` rule, minted through the table's
per-kind aux-rule slot (`%A%`, a deterministic per-(composition, member-index) rule name) —
composed 1–3
at a time into struct maps, array records, and both group-choice representations), depth-2 nesting
of constructs in container roles (a role-template table: array element, map key/value, choice
member, group-choice arm, occurrence target, tag content, `.cbor` payload, generic arg, top level),
and — low-weight — identifier choice drawn from `identifier_hazard_tests::hazards()` (never
rediscovered; the hazard sweep owns that axis systematically).

Stage A is a TypeScript projection, `cddl-matrix/project_recombination.ts`: it reduces each matrix
feature's `example` to a reusable hole-fillable expression (primary-rule RHS + auxiliary rules;
irreducible examples are recorded in a `skipped` list with reasons) and projects the containment
legality data, writing the committed `tests/recomb/ingredients.json` (`--check` is the
`project_recombination_check` drift gate, check.ts local tier). Legality semantics: the containment
matrix enumerates only structurally interesting cells and deliberately omits trivial
primitive-as-member cells as implicitly allowed, so the composer treats the projected `disallowed`
pairs as a BLACKLIST (anything unlisted composes) and uses the `legal` (spec="allowed") pairs only
as template↔matrix drift protection — every role template must name a role with at least one
modelled allowed cell.

Stage B is the Rust harness, seeded (fixed `SEED` + splitmix64) and enumeration-deterministic
(systematic cross-products where cheap, seeded sampling where the product explodes; the sweep
asserts two back-to-back enumerations are identical). It is a **corpus generator, not a CI gate**:
the standing harness detects NEW divergence classes; each finding is promoted into the existing
pinned collections after review. Two layers, mirroring the identifier-hazard split:

- `recombination_generation_sweep` (default `cargo test`, check.ts local tier, ~5 s wall —
  classification is parallelized across worker threads; thread count never changes WHAT is swept):
  classifies every composition's generation outcome in-process (`catch_unwind` + the shared
  silenced-hook idiom, extended with a per-worker capturing hook that records the normalized panic
  key `<message> @ <file> @ fn <symbol>` — the panicking production frame's function symbol, from a
  backtrace captured only on a panic, so two bare `unimplemented!()` sites in different functions no
  longer collapse to one class; line numbers stay excluded for refactor resilience). A PANIC whose
  normalized key matches no `KNOWN_PANIC_CLASSES` entry is a NEW
  finding and FAILS the sweep, printing the spec + message + promotion instructions (minimize by
  hand → pin as a matrix row if the matrix can express the cell, else a `tests/robustness/*.cddl`
  fixture → ledger it in `cddl-matrix/roadmap.toml` § findings → add the ledger entry citing the
  pin). Every ledger entry cites a committed pin AND is asserted actually observed (stale-pin
  guard). The CITATION is guarded too, by `known_panic_classes_cite_fixtures_that_produce_them`
  (always-on, same tier): it generates each `tests/**/*.cddl` fixture an entry cites and requires at
  least one to produce that entry's substring, so a citation that resolves while naming the wrong
  site — the shape no citation lint can see — is a red test rather than a mislead during triage. An
  entry citing no runnable fixture fails there as well, since prose alone leaves the claim
  uncheckable. Key SHAPE is floor-gated (`ledger_key_shape_floor`, always-on): panic-ledger keys must
  lead with message text — a file/function-only key would silently absorb every future distinct
  panic class at that site — and layer-2 known-bad keys must carry a desc-axis label so a generic
  word cannot absorb unrelated compositions. Graceful rejections are the designed boundary, tallied
  but never findings. Vacuity floors (swept count, ok count) are derived from the executed artifact.

  The sweep's outcome counts are also held EXACTLY, against the committed datum
  `tests/recombination-counts.json` — `swept` / `ok` / `graceful` / `panic` plus a per-class panic
  breakdown (each observed `KNOWN_PANIC_CLASSES` key → how many compositions landed in it). The two
  mechanisms answer different questions and both stay: the floors ask *did the composer rot* and
  tolerate ordinary movement; the datum asks *did a class migrate* and tolerates none, so an
  abort-to-rejection conversion or an ingredient addition becomes a reviewed bless-diff carrying its
  reason instead of a silent slide. Re-bless with `BLESS_RECOMB_COUNTS=1 cargo test --bin
  cddl-codegen recombination_generation_sweep`, in the same commit as the change that moved the
  numbers and with the reason in that commit's message; the failure message names the command and
  prints what it measured. The datum is trustworthy because the sweep asserts its own enumeration is
  deterministic — the same property that lets the floors be read off the executed artifact.
- `recombination_crates_execute` (`#[ignore]`, check.ts full tier — the `recombination_crates_execute`
  gate): executes the sweep's `ok` compositions under TWO deterministic, decorrelated greedy plans
  (~40 rules/batch; the budget is a ceiling except that an intrinsically oversized composition is
  preserved exactly once as a singleton; per-composition `rc<num>_` rule prefixes make names
  collision-free by construction). The natural plan preserves composition order; the transposed plan
  walks each item
  position across natural batches, then greedily re-batches that fixed order. Thus most natural
  batchmates are separated without random/hash ordering, breaking most opportunities for a missing
  CRATE-GLOBAL definition to be supplied by a batchmate. Every plan has separate labelled scratch/cache
  cells and its own executed-composition and alias-root floors. Each batch generates with
  `--emit-tests=true --wasm=false` (default profile) via the `tool_cmd`/shared-`CARGO_TARGET_DIR`
  pattern of `feature_corpus_compiles`, then `cargo test`s the generated rust crate — executing the
  emitted round-trip/reject tests, not just compiling. A failing batch is re-attributed by rerunning
  members individually; a failing member outside the cited `LAYER2_KNOWN_BAD` ledger (desc-substring
  keyed, vacuity-guarded like the layer-1 ledger) is a NEW finding with the same promotion flow. The
  fixed undefined-`Int` `.cbor`-payload-table predecessor remains pinned by
  `tests/corpus/int_alias.cddl`. Two plans are not the exhaustive singleton oracle: a standalone-
  proven class still belongs in the known-bad ledger when its provider remains a batchmate in both.
  The shared runner used by this and every non-default layer-2 profile first generates each batch
  as a discovery pass and reads the authoritative `rust/src/generated/mod.rs`. Each transparent
  public root alias spelled exactly `Rc<digits>` gets one collision-free holder rule (`rcN_embed =
  [e: rcN]`); renamed auxiliary aliases are excluded. If any holders are needed, the runner removes
  only that batch's explicit scratch output and regenerates the augmented spec from clean state
  before the single cargo oracle runs. The reported alias-root count has a positive floor per
  profile, while the composition/execution counts remain about original compositions: holders are
  oracle scaffolding, not new corpus cases.
- `recombination_preserve_crates_execute` (`#[ignore]`, check.ts full tier): the PRESERVE escalation
  of layer 2, driven by the SAME shared dual-plan runner (`run_layer2_profile`) parameterized with a different
  `Layer2Profile`. Its profile flags are sourced from `src/tests/mod.rs`'s `ALL_PROFILES` by name
  (asserted found, never re-hard-coded), so `classify_all` runs the composition set under
  `--preserve-encodings=true`; the batches then generate `--preserve-encodings=true
  --emit-tests=true --wasm=false` and `cargo test`. Motivation is a proven escaped regression: a
  preserve-only E0308 on tag-wrapped fixed-value members (`[v: #6.1(null)]`) passed every
  default-profile gate and was caught only by review — a preserve batch over the same compositions
  fails loudly on it. Preserve-only panic classes belong in `PRESERVE_ONLY_PANIC_CLASSES` (checked
  after the shared `KNOWN_PANIC_CLASSES` allowlist, with entries citing a
  `cddl-matrix/roadmap.toml` § findings entry and vacuity-guarded). That ledger is currently empty:
  every former class gained preserve support or a graceful refusal. A preserve panic matching
  neither ledger is a NEW finding. Per-profile scratch root + `CARGO_TARGET_DIR`
  (`cddl_codegen_recomb_<profile>_<hash>`) keep profiles from clobbering each other. Exclusion set =
  the shared `LAYER2_KNOWN_BAD` ∪ the profile's own `LAYER2_PRESERVE_KNOWN_BAD`; only the profile's
  own ledger is vacuity-guarded here (a shared entry can legitimately match zero preserve-ok
  compositions because preserve generation panics for that class earlier — the shared ledger's guard
  stays in the default gate). `LAYER2_PRESERVE_KNOWN_BAD` is empty at HEAD — the escalation's first
  sweep's two preserve-only compile classes (tag/`.cbor`-wrapped constrained-int deserialize tuple
  arity; composite map-key move-then-reuse) are fixed, with their preserve compile + round-trip
  pinned by the `tagged_constrained_int` / `composite_map_key` corpus fixtures — so a preserve-only
  compile failure surfaces as a NEW finding. The authoritative classification totals live only in
  `tests/recombination-counts.json` and are held exactly by `recombination_generation_sweep`; every
  `ok` composition is executed in both plans, and the shared `LAYER2_KNOWN_BAD` contributes 0 exclusions. NAMING
  GOTCHA: the name deliberately does NOT contain the
  `recombination_crates_execute` needle, and both check.ts gate cmds pass `--exact` on the full test
  path so cargo's substring selection can't cross-select.
- `recombination_json_crates_execute` (`#[ignore]`, check.ts full tier): the JSON escalation of
  layer 2, using `ALL_PROFILES["json"]` (`--json-serde-derives=true
  --json-schema-export=true`) plus `--emit-tests=true --wasm=false`, then `cargo test` on the
  generated `rust/` crate. This is the broad shape gate for serde derive / schemars derive compile
  failures while still executing the emitted CBOR tests. `--json-schema-export=true --wasm=false`
  also emits an independent `wasm/json-gen/` crate; this recombination leg deliberately leaves that
  crate to the existing json profile compile/schema gates rather than running it per batch. Both
  json-only ledgers (`JSON_ONLY_PANIC_CLASSES`, `LAYER2_JSON_KNOWN_BAD`) are empty at HEAD — json
  derives do not rewire the panic surface, so classification matches the default profile exactly.
  The authoritative totals live in `tests/recombination-counts.json`; every `ok` composition is
  executed in both plans, with 0 shared known-bad exclusions.
- `recombination_wasm_crates_check` (`#[ignore]`, check.ts full tier): the WASM escalation of
  layer 2, using explicit `--wasm=true` for both in-process classification and out-of-process batch
  generation. It does not pass `--emit-tests`: the oracle is `cargo check` on the generated `wasm/`
  crate, which depends on the generated `rust/` crate by path, so rust-side compile failures surface
  through the same command. Both wasm-only ledgers (`WASM_ONLY_PANIC_CLASSES`,
  `LAYER2_WASM_KNOWN_BAD`) are empty at HEAD — tagged tables and alias-only-reachable table wrappers
  generate and check (pinned by the `tagged_table` / `cbor_bignint_table` corpus fixtures) — so a
  wasm-only panic or compile class surfaces as a NEW finding. The authoritative totals live in
  `tests/recombination-counts.json`; every `ok` composition is checked in both plans, with 0 known-bad exclusions.
  This is a fuzz-recombination cross-check for wasm generation paths; the wasm-ABI matrix remains
  the systematic per-shape wasm surface owner.

Adding a member kind / role template / construct shape extends the swept surface; re-tune the
executed-artifact floors when doing so deliberately. Changing `SEED` re-rolls every sampled
composition — do it deliberately and re-triage.

## Config-file front end (`src/tests/config_tests.rs`)

`--config <file.toml>` (`src/config.rs`) is a pure expansion layer: it turns one TOML document into
`Vec<(crate name, Cli)>` and hands each `Cli` to the same `api::generate_to_disk` a command line
would have reached. Its whole claim is "a config key IS its flag", so the suite is organised around
the three ways that claim breaks, and deliberately re-tests nothing about what a flag MEANS (that is
covered wherever the flag is).

**A key that does not reach the flag.** The merge (built-in → `[defaults]` → profiles in the crate's
listed order → the crate's own keys) is pinned per layer boundary, each by a key only that boundary
decides, so collapsing two layers fails rather than coincidentally passing; profile order is proven
by asserting a REVERSED list gives the reversed answer. Arrays concatenate across layers, and the sub-tables that spell a repeatable
key/value flag union per key.
`expansion_equals_the_equivalent_hand_written_flag_list` sets every key at once and compares the
result field by field against `Cli::parse_from` of the flag list — both sides destructured
EXHAUSTIVELY, so a new `Cli` field fails it at compile time.

**A key that reaches the wrong value.**
`path_keys_resolve_against_the_config_file_not_the_process_cwd` is discriminating by construction:
the config sits in a temp directory and names `tests/core/input.cddl`, a path that also exists
relative to the process CWD with different content, so a CWD-relative implementation succeeds with
the WRONG file rather than merely failing; `load_resolves_a_relative_config_path_to_absolute_keys`
pins the companion property that the CWD participates exactly once (locating the config file), so no
derived byte can depend on it. `--no-preserve-comments` is the one negated flag behind a
positive key, so both directions of that inversion are pinned. Values are emitted as single
`--name=value` tokens so a leading-dash value is representable
(`a_leading_dash_value_reaches_the_flag_verbatim`; that no `Cli` argument accepts hyphen-led values
under the two-token spelling is itself pinned by enumeration in
`no_cli_argument_accepts_hyphen_led_values`), and the two sub-table spellings — header and inline —
are pinned equivalent by `an_inline_sub_table_expands_exactly_as_the_header_spelling_does`.

**A key that exists on one side only.** `config_keys_match_cli_fields` reads `struct Cli` and
`struct Settings` from SOURCE with `syn` and asserts a bijection modulo a documented exclusion list
(the three per-crate keys, which live on the crate entry; `profiles` and the three graph keys, which
are the config's own structure), and additionally pins the runtime mirror `SETTINGS_KEYS` — the
const the pre-serde unknown-key check consults — to the same field set, so the const is hand-written
but not hand-maintained. A flag added without a config key fails it, and so does a key
invented with no flag. The same syn read drives `editor_schema_matches_the_config_surface`, which
builds the editor JSON Schema (`docs/editor/cddl-codegen-config.schema.json`) from the three config
structs and fails byte-for-byte against the committed file — bless with
`BLESS_EDITOR_SCHEMA=1 cargo test --bin cddl-codegen editor_schema_matches_the_config_surface`. A
`Settings` field whose Rust type maps onto no TOML type fails the build with a decide-this message
rather than being approximated.

**What the user is told when it breaks.** The refusal surface is tested as its own concern, because
a config's error is the config's UI: an unknown key gets a nearest-match suggestion computed against
the table's real vocabulary (`an_unknown_key_suggests_the_nearest_key_that_table_accepts` — a crate
table may be sent to a per-crate key, a shared table must not be); an empty `input`/`output` is
refused naming the key (`an_empty_input_or_output_is_a_hard_error_naming_the_key`) instead of
degrading the clobber diagnostic; a clap rejection is replayed fragment-by-fragment so the blame
lands on the TOML key that caused it even when path values start with a dash
(`a_rejection_is_attributed_to_its_key_even_when_a_path_starts_with_a_dash`); post-parse errors
carry the `--config <path>` prefix parse errors always had
(`errors_after_parsing_still_name_the_config_file`); and the binary prints all of it through
`Display`, pinned end to end because no unit test sees `main`'s termination path
(`binary_errors_print_the_message_verbatim_not_debug_escaped`). The exit code is part of the same
contract: `the_committed_state_verdict_exits_2_and_other_failures_exit_1` pins 0/1/2 (the verdict —
a statement about the committed TREE that re-running cannot settle — is the one exit-2, classified
by `VerdictError`'s type rather than by message match), and
`a_mid_run_generation_failure_names_the_crates_already_regenerated` pins that a failure partway
through a run states what is already rewritten on disk, checking the earlier crate's output really
exists rather than only the wording. Its sibling
`a_mid_run_sidecar_refusal_names_the_crates_already_regenerated` pins the same statement for the
other way a crate fails mid-run — a cross-crate sidecar (`--wrapper-requests` / `--key-requests`)
this run refuses — which reaches that wrapper only because every sidecar-reader refusal travels the
error channel rather than aborting the process (exit codes for the two sites nothing else covered:
`integration_tests::sidecar_reader_refusals_exit_through_the_error_channel`).

**The command-line surface of config mode.** `--static-dir` is the one generation flag accepted
alongside `--config` (a machine-local tool location with no per-crate precedence question);
`static_dir_is_accepted_alongside_config_and_reaches_every_crate` pins all three spellings, the
override-vs-key precedence, and that the exemption is by arg id so it cannot silently widen.
`--with-deps` closes the positional selection transitively over `deps` — dependencies only, in
generation order regardless of how the names were typed
(`with_deps_closes_the_selection_over_dependencies_only`), and
`with_deps_settles_the_subset_case_in_one_command` proves the workflow it exists for: the
edit-one-consumer case that previously needed a run, a verdict, and a second run now exits 0 in one
command.

**A derived value that names something that is not there.** The cross-crate sugar (`deps`,
`wasm-reexports`, `json-schema-deps`, `[runtime]`) turns one declaration into flag values pointing at
another crate's files, so each derivation is asserted against the flag values a hand-written
invocation spells — and the derived paths are then walked on real disk, because a path that is
well-formed and wrong looks identical in an argv assertion. The JSON-threading derivation
(`--json-schema-dep` + `--json-gen-dep` from `deps ∪ wasm-reexports`) additionally pins that the
derived cargo path dependency is RELATIVE under all four `package-json` layouts: it is one of the
three derived paths written into a *committed* manifest, where an absolute value would make the same
config produce different bytes in a different clone. The wasm-manifest derivation (`--wasm-dep`, from
the same two edges) pins the same property, plus the asymmetry that is its whole content: a `deps`
edge contributes BOTH of the dependency's packages (the wasm one for the boundary `use` lines, the
rust one for a mixed-dep wrapper's inner storage), a `wasm-reexports` edge only the wasm one. The
rust-manifest derivation (`--rust-dep`) pins the third: `deps` alone feeds it, one entry per edge,
and — the leg a wasm-gated derivation would silently drop — under every combination of `wasm` on
either end of the edge, since the rust crate is the one crate every run generates.

**The compile proof for a config-generated workspace.**
`a_config_generated_workspace_builds_with_wasm_on` generates a two-crate config with `wasm = true`, a
`deps` edge and a `[runtime]` table into a scratch directory with ONE invocation, asserts a second
invocation leaves byte-identical output, and `cargo check`s the whole workspace. The single
invocation is load-bearing: before the convergence pass this test needed two `generate` calls and
discarded the first one's result, so "one command produces a workspace that builds" is asserted here
rather than assumed.
It exists because no manifest-TEXT assertion can see whether a `[dependencies]` path resolves or
whether the package it names is the one the generated `use` lines need — every derivation test above
would pass over a workspace that does not build, and before `--wasm-dep` one did not. Its three
MUTATION legs delete one derived entry each — two in the consumer's wasm manifest, one in its rust
manifest — and require the build to fail naming the crate that entry provided, which is what stops it
passing for a reason other than the one it is about. It is also the
only place in the suite that builds a config-generated workspace with wasm ON (the `[runtime]`
compile test runs `wasm = false`, the acceptance test compares bytes without building, and the `deps`
e2e asserts the generated source references the dependency without compiling it). Two hand edits
stand between "generated" and "builds" — the workspace `Cargo.toml`, and the shared runtime's crate
root plus each crate's dependency on it — and each is asserted ABSENT before being written, so a tool
that starts writing one fails there rather than silently making the edit redundant. The third entry a
`deps` edge needs, the dependency's rust package in the CONSUMER's `rust/Cargo.toml`, was a hand edit
asserted absent for exactly that reason until `--rust-dep` derived it; it is now asserted PRESENT,
which is the inversion that assertion was built to force.

The rust-only shared-runtime sibling,
`a_runtime_table_exports_a_runtime_the_other_flavor_compiles_against`, checks the deliberately
directional flavor accommodation rather than carrier derivation. It exports one preserve +
canonical runtime selected explicitly with `[runtime].flavor-from`, wires a reduced crate to that
hand-owned runtime, and `cargo check`s three isolated reduced specs against it: an ordinary-shape
control, `{+ uint => text}` (the preserve runtime's `BTreeMap` → `NonEmptyMap` bridge), and `any`
(the canonical runtime's one-argument `cbor_event::se::Serialize` bridge). Keeping the two feature
legs separate means removing either shim fails for its own compiler reason. The integration-level
composition assertions pin the other half of the contract on both callers of
`composed_runtime_static_files`: preserve `NonEmptyMap` still stores an `OrderedHashMap`, and both
the in-crate and `--export-static-crate` files append the intentional `BTreeMap` bridge. They assert
those two facts positively rather than treating the mere presence of the token `BTreeMap` as proof
that preserve storage regressed.

**The acceptance proof.** Two tests pin "config = flag expansion, nothing more" at the level of
emitted bytes rather than at the `Cli` struct.
`config_expansion_generates_byte_identical_output_to_the_flag_invocation` does it for ONE crate
through `api::generated_strings` (no disk), which keeps a cheap version of the claim in the suite.
`a_whole_config_generates_what_the_hand_written_flags_generate` does it for a whole PROJECT on
disk — four crates, `[defaults]` plus a profile three of them apply, a `deps` edge, a
`wasm-reexports` edge, a `json-schema-root`, a `[runtime]` whose carrier is derived, and a crate at a
different runtime flavor than the rest — generated once from a config and once from a hand-written
flag list into two different scratch roots, then compared file for file. The compared set spans the
`extern-interface/` exports, both borrowed sidecars, the json-gen manifests, and the exported shared
runtime, which lives outside the output directories.

**If you add a config key that DERIVES a flag, add it to the hand-written side of that fixture**
(`acceptance_hand_invocations`). This is the only thing protecting the test from quietly weakening:
a derived flag the hand list forgets still produces identical trees whenever this fixture's specs do
not reach that flag's output effect, and the byte comparison then passes while covering nothing.
Measured, not argued — dropping `--extern-wrapper-index` from the hand list leaves the two trees
byte-identical. What catches it is the test's FIRST assertion, which compares the whole expanded
`Cli` (every field, each run's own root erased) against `Cli::parse_from` of the hand argv and names
the field that moved. Keep both assertions: the struct comparison is what makes the hand list
provably complete, the byte comparison is what proves nothing in the emitted output depends on where
the run happened.

`a_config_run_converges_and_then_repeats_byte_for_byte` covers the convergence pass, which is what
makes "run twice = run once" true of a config run: one invocation over a cold tree exits 0 and
settles the workspace, because after the first ordered pass the run re-runs exactly the crates whose
consumed sidecars that pass rewrote. The shape asserted is run 1 exits 0, runs 2 and 3 are
byte-identical to it, and — the guard that keeps the fixture honest, replacing the old
`assert_ne!(run 1, run 2)` — the dependency's wrapper index really hosts what the consumer borrows,
which it can only do having been generated after the consumer recorded the borrow. Byte-identity at
run 2 is also what pins the absence of the residual convergence WARNING rather than a proxy for it: a
sidecar that moved across run 1's pass would leave a crate generating different bytes when run 2 ran
it against the settled one.

The one-pass argument itself is pinned above depth 1 by
`a_two_edge_dependency_chain_converges_in_one_invocation` (a middle crate that is consumer AND
dependency at once — a shape no single edge produces) and
`a_diamond_dependency_graph_converges_in_one_invocation`, each with real borrows on both sidecar
channels at every edge and the same runs-2-and-3-byte-identical assertion; the invariant bounding
the argument (export non-transitivity) is cited at the pass itself. The residual instrument watches
EVERY consumed sidecar, not only the re-run crates' — the discriminating shape (a sidecar moving
under a not-re-run crate) is constructible only if the one-pass argument is false, so the breadth is
pinned directly instead: the chain fixture's incremental leg asserts `watched_crates()` against what
a stale-restricted capture would watch, at the depth where the two come apart.

Everything here runs under plain `cargo test` (`cargo test --bin cddl-codegen config_tests`; in-tier
via the local tier's workspace `cargo test`, no dedicated gate). The binary-level tests (the error
rendering, exit codes, `--with-deps` and mid-run-failure pins above) spawn the real generator via
`codegen_cmd`, which builds it once per test process. Three cells additionally nest a full cargo
build/run inside that, each for a verdict no generation-time assertion can reach: a two-crate `deps` config whose
consumer really imports its dependency's type; a `[runtime]` export compiled against BOTH flavors
that import it (with a mutation leg proving the accepted gap is real); and
`a_derived_thread_links_and_a_collision_blames_the_consumer`, which builds and RUNS a consumer's
json-gen crate so the derived thread's two halves are shown to agree, then makes both crates publish
one schema name and asserts the injectivity guard names the consumer's row.

## Design rules (review-owned; each with a shipped exemplar)

Six rules govern how guards, graceful-rejection refactors, and directive-effect pins are written.
Review is their current owner; the conditional mechanical layers (built only if a class recurs) are a
`tests/testing-roadmap.toml` item.

- **Invariant-softening refactors keep impossible states loud.** When a panic/assert is converted
  into a graceful rejection, enumerate the states the assert covered and downgrade ONLY the
  reachable, user-triggerable ones; states the assert made impossible stay `unreachable!`. A
  catch-all soft arm silently absorbs the impossible state, and no gate can see that (a mutation
  sweep would at best surface the arm as a survivor that triage then plausibly waves through as
  equivalent — it cannot distinguish "kept loud" from "absorbed"). Shipped exemplar:
  `set_rep_if_plain_group`'s multi-rep match in intermediate/mod.rs (conflicting-rep = graceful
  rejection; non-Record/non-GroupChoice materialization = still `unreachable!`).
- **Vacuity floors witness the guarded artifact, not a proxy for it.** A floor whose count derives
  from an INPUT correlated with the guarded behavior — rather than from the behavior's own
  artifact — is satisfied by any regression that preserves the input (a floor counting catalog
  `expect_err` presence stays green while the emitted assert regresses to a plain `is_err`,
  leaving the pin vacuous). Derive the floor's count from the emitted/executed artifact, or place
  an assert at the emission site itself, outside the branch being guarded. Shipped exemplars:
  `decode_replay_run`'s CONSTRAINT_WRONG_REASON body assert, and the pipeline-boundary
  rejection-drain assert in `api.rs` (both generation exits assert no `record_rejection` survives
  past `finalize`'s drains — a post-drain record site would otherwise be silently swallowed with
  the tool exiting 0; mutation-proven by injecting a post-drain record and observing the snapshot
  suite go red).
- **A directive's effect is pinned as a TWIN PAIR — the annotated rule beside an unannotated control
  of the SAME rule shape, generated under the flags where the effect is visible.** A directive that
  suppresses something is asserted with a negative (`the row is absent`), and a bare negative passes
  for every reason including the ones the test exists to exclude: the shape never produced the
  artifact, the flag profile never emitted the surface, or the directive never reached that parse
  path at all. Pairing it with a same-shape control that MUST still show the artifact converts the
  vacuous pass into a failure. The shape half matters as much as the flag half: rule-position
  directives reach only the shapes whose parse path carries a marking site, so each shape a
  directive claims to support needs its own pair rather than one pair standing in for all of them.
  Shipped exemplar: `snapshot_tests::json_gen_extern_schema_rows`'s `quiet_group`/`loud_group` and
  `quiet_extern`/`my_extern` pairs — added after `@no_json_schema_export` shipped silently inert on
  plain group rules, a drop that a control-free negative assertion would have reported as green.
- **A walk over registered roots witnesses each root it claims to cover.** A sweeping test that
  `read_dir`s a list of registered directories and skips unreadable ones (`Ok(..) else continue`)
  makes a mistyped, renamed, or never-written root INVISIBLE — the walk finds nothing there and the
  test reads as a pass, which is indistinguishable from the surface being clean. An aggregate count
  floor does not fix this unless its arithmetic provably requires every root, which drifts as
  surfaces grow. So each registered root asserts a file it must have reached (per-root witness),
  with the floor kept only as a belt. Shipped exemplar:
  `every_written_surface_is_rustfmt_stable`'s per-root reached assertions (added when the emitted
  `no-std-check/src` surface joined the walk and the old floor's "cannot be met without the
  original roots" claim turned out to be unverified arithmetic).
- **Output governed by a process global is asserted from a SUBPROCESS, never in-process.** When what
  a run prints depends on process-global state — the verbosity level in `src/log.rs` is the shipped
  instance — an in-process assertion on that output is wrong twice over. The global one test installs
  is visible to every `#[test]` sharing the process, and `cargo test` runs them on parallel threads,
  so a test that lowers the level can silence a message a concurrent test is asserting. Worse, the
  resulting flake is invisible under the conditions used to investigate it: an in-process output
  assertion passes reliably when run alone, which is exactly how a failing test gets re-run
  (`--test-threads=1`, or a name-filtered single-test invocation), so the failure reads as
  intermittent infrastructure noise rather than as the design error it is. Spawning the binary gives
  each assertion its own process and its own global. Shipped exemplar: the eight verbosity tests
  (`verbosity_does_not_change_generated_bytes`, `raising_verbosity_is_monotonic`,
  `verbosity_is_per_crate_under_config` and siblings) all drive the built binary through
  `generator_bin()` / `codegen_cmd()`, with the reason stated in a comment above each group so the
  next author does not "simplify" them into in-process calls. The crate has no in-process output
  capture at all (no `gag`/`BufferRedirect`/`set_output_capture`, nothing in `Cargo.toml`), so the
  wrong version is currently not even expressible — adding such a dependency is what would make this
  rule violable.
- **A delivery that ships a gate covering its own regression classes needs no per-bug ledger
  entries for those classes.** When the failure analysis for a pre-ship catch shows the
  delivery's own gate would have gone red on it, record the coverage fact once and stop —
  per-instance roadmap entries for self-caught classes are noise that dilutes the ledger's
  recur-first signal. The analysis still runs per catch (that's how you learn the gate would
  have caught it); only the ledgering collapses. Shipped exemplar: the `no_std_check` gate's own
  cells cover the emitted-shim shape and warning-class regressions found while building it, so
  those probe catches produced no entries — where the same delivery's cache-closure near-miss,
  which NO gate covered, did.

## Coverage

The in-process snapshot suite alone covers ~81% of the codebase (generation/ ~86%). To measure
(requires `cargo install cargo-llvm-cov` + `rustup component add llvm-tools-preview`):

```sh
cargo llvm-cov --summary-only -- snapshot_tests
```

Note: the integration tests run the generator in a subprocess, so llvm-cov (which instruments the
test binary) does not attribute their coverage — the 81% is from snapshots + in-process unit tests
only.

## Mutation testing (`cargo-mutants`, historical experiment — permanently declined)

Mutates the emit core (`src/generation*`) and scores each mutant against the **behavioral layers
only** — nextest with `-E 'not test(/snapshot_tests::/)'`. Snapshots trivially "kill" almost every
emit-core mutant (any text change fails a snapshot), which measures snapshot *text-sensitivity*,
not whether a human-blessed wrong emission would be caught — the failure mode that actually ships.
Scored behaviorally, the survivor list is a direct map of emit logic no behavioral oracle observes.

All settings from the experiment (scope, nextest filterset, timeouts) remain pinned in
`.cargo/mutants.toml` as historical/probing machinery. **Do not run or resume the full sweep.** It
is permanently declined, will never become a `check.ts` gate, and has no reopening signal; see
`tests/testing-roadmap.toml` § Declined. In particular, do not interpret the retained config or the
partial measurements below as an unfinished obligation.

Measured scale (first survey): **1040 mutants**, ~1.8 min/mutant average (the behavioral suite
shells nested cargo per mutant) — a complete sweep would be a **~30 h unattended job**.
First-survey sample (33 tested: 13 caught, 14 unviable, 6 missed): all 6
misses triaged as *behaviorally equivalent by construction*, not oracle gaps — the
`container_encoding_lookup` arity branch exists only for `clippy::redundant_closure` (both branches
emit semantically identical code), and `encoding_var_is_copy -> false` only adds redundant
`.clone()`s to generated code (the dangerous direction, `-> true`, is caught behaviorally on all
three impls). That result is the experiment's closed conclusion, not a reason to enumerate the
remaining mutants.

## Known gap: `number`/`time` are missing from the prelude fixture

The CDDL standard prelude (`biguint`, `tdate`, `uri`, …) is covered by `tests/corpus/prelude.cddl`.
The float-bearing choice types (`number`, `time`) are **not** — and there is no technical reason for
it. They generate and round-trip under **every** profile (floats support `--preserve-encodings`, and
each prelude float name is its own wire-acceptance class — the `float_heads`/`float_widths` suites).

The omission is a leftover, and its stated justifications do not hold up:

- It was originally recorded as blocked by preserve-mode floats. That blocker is gone.
- It was then re-justified on **coverage economy** — the generated shape (a type-choice enum) being
  covered by other members, and the float arm getting wire vectors elsewhere (the matrix
  decode-conformance `prelude.number`/`prelude.time` rows, `homogeneous_array.cddl`'s `float_holder`,
  `optional_fixed_float.cddl`). That is an argument about corpus size, which is not a scarce
  resource here: the fixture is a flat list and the addition is two lines.

What the fixture is FOR is the reason economy does not settle it. It is the one place that answers
"does this tool cover the RFC 8610 Appendix D prelude?", and it currently cannot answer for two of
the names — the only two that carry a float, immediately after a delivery that changed what every
float name means. A reader checking prelude coverage does not know that `number` is covered by
inference from `bigint`'s shape plus a matrix row two directories away; they read the list.

**The work:** add `number` and `time` to `prelude.cddl`, re-bless the corpus, and check whether the
matrix's `prelude.number`/`prelude.time` decode-conformance rows become redundant or stay as the
wire-vector half. Small, and unblocked.

[`insta`]: https://insta.rs
