//! Run verbosity: which of the tool's own messages a run prints, and where each one goes.
//!
//! Two axes, decided independently:
//!
//! * **Level** decides WHETHER a message appears. Five ordered names — `error`, `warn` (the
//!   default), `info`, `debug`, `trace` — each adding to the one before it.
//! * **Stream** decides WHERE it appears, and is a property of the message's KIND rather than of
//!   its level: **diagnostics** (errors, warnings, behaviour-change notices) go to **stderr**, and
//!   **run output** (what the run is doing, what it produced, what you must now act on) goes to
//!   **stdout**. A caller pipes stdout and watches stderr; a message on the wrong stream is lost by
//!   exactly the caller who needed it.
//!
//! That second rule is the thing a call-site author needs and cannot read off the macro names, so
//! it is stated here: [`note!`] is not a redundant [`warn!`]. Both are visible at the default level;
//! they differ in the stream, because a `note!` is user-facing run output rather than a diagnostic.
//!
//! No `log`/`tracing` dependency. Those render a level tag and a prefix INTO the message, which
//! changes text that humans, the Rust suite and `cddl-matrix/no_silent_directive.ts` consume as-is,
//! for no gain here. The macros below take the same argument shape as `println!`/`eprintln!` and
//! emit exactly the same bytes — the only thing that changes is when.

use std::sync::atomic::{AtomicU8, Ordering};

/// How much a run prints. Ordered: each level shows everything the levels below it show.
///
/// Spelled the same in all three places it is written — the `--verbosity` flag value (via
/// `clap::ValueEnum`), the `verbosity` config key (via `serde`), and [`Verbosity::as_str`]. The type
/// lives here rather than in `cli.rs` because all three consumers need it: `cli::Cli`,
/// `config::Settings`, and the macros below.
#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum, serde::Deserialize,
)]
#[serde(rename_all = "lowercase")]
pub enum Verbosity {
    /// Nothing beyond fatal errors — which are the exit path rather than logging, so they are
    /// printed unconditionally.
    Error,
    /// Warnings, behaviour-change notices, and run output. The default.
    Warn,
    /// Per-file / per-scope / per-phase progress.
    Info,
    /// The per-rule handling banners.
    Debug,
    /// The full IR dump.
    Trace,
}

/// Hand-written rather than `#[derive(Default)]`, which would take the FIRST variant: the default is
/// `Warn`, and `Error` is first because the variants are ordered by how much they print. `Cli`
/// derives `Default`, so this is what `Cli::default().verbosity` is.
impl Default for Verbosity {
    fn default() -> Self {
        Verbosity::Warn
    }
}

impl Verbosity {
    /// The one spelling of each level: what `--verbosity` accepts, what the `verbosity` config key
    /// accepts, and what `--print-flags` lists back.
    pub fn as_str(&self) -> &'static str {
        match self {
            Verbosity::Error => "error",
            Verbosity::Warn => "warn",
            Verbosity::Info => "info",
            Verbosity::Debug => "debug",
            Verbosity::Trace => "trace",
        }
    }

    /// The inverse of the `as u8` cast the atomic stores. Total by construction — every value the
    /// atomic can hold was written by [`set`], which only ever writes a `Verbosity` — so an
    /// out-of-range byte is a bug in this module rather than an input to validate.
    fn from_u8(raw: u8) -> Verbosity {
        match raw {
            0 => Verbosity::Error,
            1 => Verbosity::Warn,
            2 => Verbosity::Info,
            3 => Verbosity::Debug,
            _ => Verbosity::Trace,
        }
    }
}

/// The level in force, as a process global.
///
/// A global even though the VALUE is per-crate: the value is chosen per crate and installed for the
/// duration of that crate's generation (see [`scoped`]). A global is what avoids threading a
/// parameter through every emission site in `generation/`, `parsing/` and `intermediate/` — none of
/// which has a `Cli` to hand at the point it wants to print.
///
/// `Ordering::Relaxed` throughout, deliberately: no other memory is published through this flag, it
/// only decides whether a line is printed. Nothing reads a value that a store to it makes valid, so
/// there is no happens-before edge to establish and nothing for `SeqCst` to buy.
static LEVEL: AtomicU8 = AtomicU8::new(Verbosity::Warn as u8);

/// The level in force right now.
//
// `allow(dead_code)`, here and on `enabled` below, because the BIN crate reaches both only through
// the six exported macros, and rustc's dead-code pass does not expand a macro nothing invokes: a
// level accessor every macro body calls still reads as unused until some module in this crate
// invokes one. (The lib crate exports them, so there they are public API either way.) Drop the
// attribute once a call site does.
#[allow(dead_code)]
pub fn verbosity() -> Verbosity {
    Verbosity::from_u8(LEVEL.load(Ordering::Relaxed))
}

/// Is a message at `at_least` shown? What every macro below tests, and what a site gating an
/// expensive format (the IR dump) tests by hand.
#[allow(dead_code)]
pub fn enabled(at_least: Verbosity) -> bool {
    verbosity() >= at_least
}

/// Install `v`, returning the previous level.
fn set(v: Verbosity) -> Verbosity {
    Verbosity::from_u8(LEVEL.swap(v as u8, Ordering::Relaxed))
}

/// Install `v` for as long as the returned guard lives, restoring the previous level on drop.
///
/// A guard rather than a bare setter because config mode has two levels in play: the RUN level (the
/// per-crate banner, the `[runtime]` notes, the convergence lines) and each crate's own. Restoring
/// on drop is what keeps the run level intact across a crate that set its own — without it the run
/// level would be whatever the last crate happened to be generated at.
#[must_use = "the level is restored when the guard drops, so dropping it immediately is a no-op"]
pub fn scoped(v: Verbosity) -> LevelGuard {
    LevelGuard { previous: set(v) }
}

/// Restores the level [`scoped`] replaced. See there.
pub struct LevelGuard {
    previous: Verbosity,
}

impl Drop for LevelGuard {
    fn drop(&mut self) {
        set(self.previous);
    }
}

// The six emission macros. Each takes the argument shape `println!`/`eprintln!` takes, so converting
// a call site is a one-token edit and no message text changes — no prefixes, no level tags, no
// timestamps.
//
// Each body tests the level BEFORE formatting, so the format arguments are not evaluated when the
// level is off. That laziness is the whole point at `trace`, where the arguments are `{:?}` of the
// entire IR: the formatting is the cost there, not the write.
//
// `#[macro_export]` (with `$crate::log::…` bodies) so every one is reachable as `crate::warn!(…)`
// from any module, regardless of where `mod log;` sits in the root's declaration order — `macro_rules!`
// are otherwise textually scoped, and `log` sorts after `intermediate`.

/// stderr, always. A fatal error, which no level hides.
#[macro_export]
macro_rules! err {
    ($($arg:tt)*) => { eprintln!($($arg)*) };
}

/// stderr, from `warn`. A diagnostic: something the user should probably act on.
#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => {
        if $crate::log::enabled($crate::log::Verbosity::Warn) { eprintln!($($arg)*) }
    };
}

/// stdout, from `warn`. User-facing run output that is NOT a diagnostic — visible at the default
/// level, but on stdout because a caller pipes it. See the module docs.
#[macro_export]
macro_rules! note {
    ($($arg:tt)*) => {
        if $crate::log::enabled($crate::log::Verbosity::Warn) { println!($($arg)*) }
    };
}

/// stdout, from `info`. Per-file / per-scope / per-phase progress.
#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {
        if $crate::log::enabled($crate::log::Verbosity::Info) { println!($($arg)*) }
    };
}

/// stdout, from `debug`. Per-rule progress.
#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if $crate::log::enabled($crate::log::Verbosity::Debug) { println!($($arg)*) }
    };
}

/// stdout, from `trace`. The IR dump.
#[macro_export]
macro_rules! trace {
    ($($arg:tt)*) => {
        if $crate::log::enabled($crate::log::Verbosity::Trace) { println!($($arg)*) }
    };
}
