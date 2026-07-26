//! Per-cell duration rows for the gates whose wall time is nested cargo.
//!
//! A gate's `--- <gate>: PASS [6m 16s]` line is one number for work that is really dozens of
//! independent cells. This appends one row per cell so "which cell costs the six minutes" is a
//! question the ledger can answer, instead of one a session re-derives by hand.
//!
//! **A file, not stdout.** Five of `gate_cache::run_cached`'s six call sites run under libtest's
//! stdout capture — their check.ts registry entries pass `--ignored` with no `--nocapture` — so a
//! `println!` marker is *discarded* precisely at the slowest cells (a passing libtest test's captured
//! output is never printed). Adding `--nocapture` to those entries would work, but it changes what
//! those gates put into the shared terminal stream for a reason unrelated to what they verify.
//!
//! **Inert unless asked.** With `CDDL_TIMING_CELLS` unset this performs no syscall at all — not an
//! open, not a stat — because `env::var_os` is a memory read of the process environment. That is a
//! requirement, not an optimization: `gate_cache_closure_audit` traces a cached gate under `strace`
//! and fails on any file read it cannot attribute to a hashed key input, so instrumentation that
//! touched the filesystem unconditionally would put new syscalls inside the very gate that audits
//! syscalls. Standalone `cargo test` therefore behaves exactly as it did before.
//!
//! **Durations are never a gate** (`cddl-matrix/project_timings.ts` header): nothing here can fail a
//! test. Every error path is discarded — a measurement that cannot be written is a measurement lost,
//! not a red gate.

use std::io::Write;

/// Append one cell row, or do nothing at all when `CDDL_TIMING_CELLS` is unset.
///
/// `outcome` is a free label rather than the gate-cache enum because not every instrumented cell is
/// cached: `hit`/`run_pass`/`run_fail` come from `run_cached`, while `ran` comes from the replay
/// gates, which memoize nothing and so have no cache outcome to report. Collapsing the two would
/// make a replay row indistinguishable from a cache miss, and "how much of this gate was cached" is
/// exactly the question the rows exist to answer.
///
/// A row is emitted for a MISS as well as a hit. The gate cache's own `[gate-cache] …: cached PASS`
/// line covers only hits and carries no duration, so the expensive half — the cells that actually
/// ran — has never been visible per-cell at all.
pub(crate) fn emit(emitter: &str, cell: &str, outcome: &str, ms: u128) {
    emit_to(
        std::env::var_os("CDDL_TIMING_CELLS").as_deref(),
        emitter,
        cell,
        outcome,
        ms,
    );
}

/// The destination as a PARAMETER, so both halves — writes a row / writes nothing — are pinned
/// without touching the process environment. A test that instead asserted `CDDL_TIMING_CELLS` is
/// absent would pass standalone and fail under check.ts, which sets the variable for the whole
/// `test` gate; and one that set the variable would race every other test in a 32-thread suite.
fn emit_to(path: Option<&std::ffi::OsStr>, emitter: &str, cell: &str, outcome: &str, ms: u128) {
    let Some(path) = path else {
        return;
    };
    // Read at emit time, not cached: check.ts rewrites `CDDL_TIMING_GATE` per gate, and clears the
    // destination outright around the closure audit.
    let log = std::env::var("CDDL_TIMING_LOG").ok();
    let gate = std::env::var("CDDL_TIMING_GATE").ok();
    let row = row_json(
        log.as_deref(),
        gate.as_deref(),
        emitter,
        cell,
        outcome,
        ms,
        unix_seconds(),
    );
    // ONE `write_all` of ONE line, opened `O_APPEND`. Cells run from parallel libtest threads and
    // each row is far below PIPE_BUF, so appends cannot interleave into a corrupt line and no lock
    // is needed; a cell that panics mid-gate cannot leave a half-row behind either.
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
    {
        let _ = f.write_all(row.as_bytes());
    }
}

/// Split out so the row shape is pinned without an env var or a filesystem.
///
/// `emitter` and `gate` are SEPARATE fields, and conflating them is a trap this repo has already
/// walked into once. Three of the labels `run_cached` is called with — `feature_corpus_compiles`,
/// `wasm_matrix_compiles`, `multifile_matrix_compiles` — are cells INSIDE the `test` gate and are not
/// registry gate ids at all; the prose parser attributes by section header for exactly that reason,
/// so that the digest cannot invent three gates that do not exist. A field named `gate` holding an
/// emitter label would re-assert that false mapping through a different door, so `gate` carries only
/// what check.ts told us the registry gate is, and is ABSENT when nobody did — a consumer grouping by
/// it then gets nothing rather than a wrong answer.
///
/// `log` is the run's own log basename, the same key W5's rows carry. Without it a run's cells cannot
/// be attributed to that run except by guessing at timestamp windows, and the end-of-run aggregation
/// that feeds `cells_run`/`cells_cached` back onto the gate rows would have nothing to join on.
#[allow(clippy::too_many_arguments)]
fn row_json(
    log: Option<&str>,
    gate: Option<&str>,
    emitter: &str,
    cell: &str,
    outcome: &str,
    ms: u128,
    ts: u64,
) -> String {
    let s = |v: &str| serde_json::to_string(v).unwrap_or_else(|_| "\"?\"".to_string());
    let mut row = String::from("{\"v\":1,\"kind\":\"cell\"");
    if let Some(log) = log {
        row.push_str(&format!(",\"log\":{}", s(log)));
    }
    if let Some(gate) = gate {
        row.push_str(&format!(",\"gate\":{}", s(gate)));
    }
    row.push_str(&format!(
        ",\"emitter\":{},\"cell\":{},\"outcome\":{},\"ms\":{},\"ts\":{}}}\n",
        s(emitter),
        s(cell),
        s(outcome),
        ms,
        ts,
    ));
    row
}

/// Time one unit of work in a gate that has NO memoization to hang the measurement off.
///
/// `corpus_decode_replay` (11m) and `decode_conformance_replay` (7m) route through no cache — probed
/// by reading both bodies and every helper they share, which document themselves as "uncached in
/// gate-cache v1" — so their minutes have never decomposed into anything. Their unit of work is a
/// catalog row.
///
/// Emitting on DROP rather than at the end of the loop body is the whole point: both bodies
/// `continue` past rows, and an emission placed after that branch would silently record only the
/// rows that took the long path. Bind it to a NAMED binding (`let _cell = …`, never `let _ = …`,
/// which drops immediately and would time nothing).
pub(crate) struct CellTimer {
    emitter: &'static str,
    cell: String,
    dest: Option<std::ffi::OsString>,
    started: std::time::Instant,
}

impl CellTimer {
    pub(crate) fn start(emitter: &'static str, cell: &str) -> Self {
        Self::start_to(std::env::var_os("CDDL_TIMING_CELLS"), emitter, cell)
    }

    /// Destination injected, for the same reason `emit_to` takes one: the guard's contract — one row,
    /// on drop, not before — is then pinned without a test mutating the environment of a 32-thread
    /// suite.
    fn start_to(dest: Option<std::ffi::OsString>, emitter: &'static str, cell: &str) -> Self {
        Self {
            emitter,
            cell: cell.to_string(),
            dest,
            started: std::time::Instant::now(),
        }
    }
}

impl Drop for CellTimer {
    fn drop(&mut self) {
        emit_to(
            self.dest.as_deref(),
            self.emitter,
            &self.cell,
            "ran",
            self.started.elapsed().as_millis(),
        );
    }
}

/// Wall-clock ordering within a run. Attribution to a run is `log`, not this — a timestamp window is
/// a guess, and the end-of-run aggregation needs a key it can join on exactly.
fn unix_seconds() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every row is one line of valid JSON with the keys the ledger reads. A cell id containing a
    /// quote or a backslash is the case a hand-rolled `format!` of the value would silently corrupt
    /// — one unparseable line, in a file nothing validates until an investigation needs it.
    #[test]
    fn a_cell_row_is_one_line_of_escaped_json() {
        let row = row_json(
            Some("check-full-2026-07-26T00-00-00Z.log"),
            Some("wasm_matrix_roundtrips"),
            "wasm_matrix_roundtrips",
            "batch\"00\\1",
            "run_pass",
            6_160,
            1_785_000_000,
        );
        assert_eq!(row.matches('\n').count(), 1, "exactly one line: {row}");
        assert!(row.ends_with('\n'));
        let v: serde_json::Value = serde_json::from_str(&row).expect("row parses as JSON");
        assert_eq!(v["kind"], "cell");
        assert_eq!(v["log"], "check-full-2026-07-26T00-00-00Z.log");
        assert_eq!(v["cell"], "batch\"00\\1");
        assert_eq!(v["outcome"], "run_pass");
        assert_eq!(v["ms"], 6_160);
    }

    /// The label≠gate trap, pinned. `feature_corpus_compiles` is a cell INSIDE the `test` gate, so a
    /// row must never present it as `gate`: `emitter` names who emitted, `gate` names the registry
    /// gate check.ts was running, and they legitimately differ. Anything that groups cells by `gate`
    /// and joins them to a ledger row depends on this being the registry id and nothing else.
    #[test]
    fn the_emitter_label_is_never_presented_as_the_registry_gate() {
        let row = row_json(
            Some("check-local-x.log"),
            Some("test"),
            "feature_corpus_compiles",
            "array/default",
            "hit",
            927,
            1_785_040_309,
        );
        let v: serde_json::Value = serde_json::from_str(&row).unwrap();
        assert_eq!(v["gate"], "test", "the registry gate check.ts was running");
        assert_eq!(v["emitter"], "feature_corpus_compiles", "who emitted it");
    }

    /// Standalone (`cargo test` by hand): no run scoping is known, so `log`/`gate` are ABSENT rather
    /// than guessed. A consumer grouping by `gate` gets nothing for such a row — the fail-safe
    /// direction — instead of an emitter label masquerading as a registry id.
    #[test]
    fn an_unattributed_row_omits_log_and_gate_rather_than_guessing() {
        let row = row_json(None, None, "wasm_matrix_compiles", "c/default", "hit", 5, 1);
        let v: serde_json::Value = serde_json::from_str(&row).unwrap();
        assert!(v.get("log").is_none(), "no fabricated log: {row}");
        assert!(v.get("gate").is_none(), "no fabricated gate: {row}");
        assert_eq!(v["emitter"], "wasm_matrix_compiles");
    }

    fn scratch(name: &str) -> std::path::PathBuf {
        let dir =
            std::env::temp_dir().join(format!("cddl_timing_cells_{}_{}", name, std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    /// The inert path: no destination => no write, and specifically no file CREATED. The gate this
    /// protects (`gate_cache_closure_audit`, 15m+) cannot be run cheaply, so the property it rests on
    /// — instrumentation adds no filesystem syscall unless asked — is pinned here rather than
    /// discovered by a red strace audit.
    #[test]
    fn no_destination_writes_nothing() {
        let dir = scratch("inert");
        emit_to(None, "gate", "cell", "hit", 1);
        assert!(
            std::fs::read_dir(&dir).unwrap().next().is_none(),
            "an unset destination must not create anything",
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// The other half: rows APPEND, one line each, and the file is created on first write. Appending
    /// (never truncating) is what lets cells from parallel libtest threads share one destination.
    #[test]
    fn rows_append_one_line_each() {
        let dir = scratch("append");
        let path = dir.join("cells.jsonl");
        emit_to(Some(path.as_os_str()), "g", "cell-a", "hit", 1);
        emit_to(Some(path.as_os_str()), "g", "cell-b", "run_pass", 2);
        let body = std::fs::read_to_string(&path).unwrap();
        let lines: Vec<&str> = body.lines().collect();
        assert_eq!(lines.len(), 2, "one line per row: {body:?}");
        assert!(body.ends_with('\n'));
        for line in &lines {
            serde_json::from_str::<serde_json::Value>(line).expect("each line parses");
        }
        assert!(lines[0].contains("cell-a") && lines[1].contains("cell-b"));
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// The guard emits on DROP, so a loop body that `continue`s past a row still records it — the
    /// property that makes the replay gates' decomposition cover every row instead of the slow ones.
    #[test]
    fn the_cell_timer_emits_exactly_one_row_on_drop() {
        let dir = scratch("timer");
        let path = dir.join("cells.jsonl");
        {
            let _cell =
                CellTimer::start_to(Some(path.clone().into_os_string()), "replay_gate", "row.id");
            assert!(
                !path.exists(),
                "nothing is written while the guard is alive"
            );
        }
        let body = std::fs::read_to_string(&path).unwrap();
        assert_eq!(
            body.lines().count(),
            1,
            "exactly one row per guard: {body:?}"
        );
        let v: serde_json::Value = serde_json::from_str(body.trim()).unwrap();
        assert_eq!(v["emitter"], "replay_gate");
        assert_eq!(v["cell"], "row.id");
        assert_eq!(v["outcome"], "ran");
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// ...and a guard with no destination stays inert, which is the shape the replay gates run in
    /// whenever the suite is invoked by hand.
    #[test]
    fn a_cell_timer_with_no_destination_writes_nothing() {
        let dir = scratch("timer_inert");
        drop(CellTimer::start_to(None, "replay_gate", "row.id"));
        assert!(std::fs::read_dir(&dir).unwrap().next().is_none());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
