//! The emitted `no-std-check/` shim crate: a tiny `#![no_std]` crate that depends on the generated
//! rust crate with `default-features = false`, so a consumer can prove in one command that the crate
//! still builds without `std`.
//!
//! # Why the tool emits it at all
//!
//! The generated subtree (`src/generated/**`) is held `no_std`-clean by this repo's own gates. The
//! crate root `src/lib.rs` is seeded once and then owned by the consumer, so hand-added utils are the
//! one thing that can break `no_std` for a generated crate — and detecting that is not something the
//! generator can do from here. The shim converts it into a check the consumer runs, and the seeded
//! root's header comment points at it (`export::SEEDED_RUST_CRATE_ROOT`).
//!
//! # What it opts out of
//!
//! Everything per-crate. The shim is NOT a fourth generated crate: no seed-once root, no manifest
//! changeset, no comment/code-preservation overlay, no extern-interface export, no `--config`
//! sidecars. Both files are built from `Cli` alone and the whole directory is delete-and-recreated
//! every run (see [`super::export`]'s emission site), which is why its `Cargo.toml` is a hand-built
//! string rather than a `cargo_manifest` op set — nothing in it is co-owned, so there is nothing to
//! merge.

use crate::cli::Cli;
use std::collections::BTreeMap;

/// The shim's directory, a sibling of `rust/`/`wasm/` under `--output` and outside the
/// `--package-json` nesting (the shim reaches the crate it checks by a computed relative path rather
/// than moving with it).
///
/// A constant rather than a literal at one site because two files spell it: this module keys its
/// output with it, and `export()` clobbers the directory by it. It stays HERE rather than in
/// `generation::layout` because both readers are inside the emitter — `layout` owns only the facts
/// something outside `generation/` reads back, and says so.
pub(crate) const NO_STD_CHECK_DIR: &str = "no-std-check";

/// The shim's cargo package name suffix: `--lib-name` plus this. It exists so a `--config`
/// multi-crate tree, whose crates have distinct lib names, gets distinct shim package names.
pub(crate) const NO_STD_CHECK_PACKAGE_SUFFIX: &str = "-no-std-check";

/// The documented one-liner, verbatim. LOCKSTEP: the same command text appears in the seeded rust
/// crate root (`export::SEEDED_RUST_CRATE_ROOT`, hence in every blessed corpus snapshot of it) and in
/// `docs/docs/output_format.mdx`. A consumer reads the seeded root, copies the line, and runs it
/// against the file this module writes — so the three spellings must stay byte-identical.
const CHECK_COMMAND: &str = "cargo check --manifest-path <output-root>/no-std-check/Cargo.toml \
                             --target thumbv7m-none-eabi";

/// The two files of the shim crate, keyed by path RELATIVE TO THE OUTPUT ROOT (`no-std-check/…`) —
/// the same keying `extern_interface_files` uses, and the reason both land as top-level siblings of
/// `rust/` rather than inside it.
///
/// A pure function of `Cli`: it needs no IR at all, because nothing about the shim depends on what
/// the spec declares. That is deliberate — the shim asserts a property of the CRATE, and a spec-shaped
/// shim would make the check's meaning vary by input.
pub(crate) fn no_std_check_files(cli: &Cli) -> BTreeMap<String, String> {
    let mut files = BTreeMap::new();
    files.insert(format!("{NO_STD_CHECK_DIR}/Cargo.toml"), cargo_toml(cli));
    files.insert(format!("{NO_STD_CHECK_DIR}/src/lib.rs"), lib_rs(cli));
    files
}

/// The relative path from `<output>/no-std-check/` to the generated rust crate.
///
/// LOCKSTEP: this is the third reader of the `--package-json` nesting rule, whose other two are
/// `GenerationScope::export`'s `rust_dir` (where the one-level-down decision is actually made) and
/// `config::crate_relative` (which restates it for a crate reading ANOTHER crate's output). The shim
/// does NOT move with the crates — it stays at the output root — so it absorbs the nesting into its
/// dep path instead. Change all three together.
fn dep_path(cli: &Cli) -> &'static str {
    if cli.package_json {
        "../rust/rust"
    } else {
        "../rust"
    }
}

/// The attribution header, and the caveat that keeps it honest.
///
/// Both files say the same two things, in their own comment syntax — LOCKSTEP, because a consumer
/// reads whichever one they opened: a red check over an unmodified crate attributes to hand-written
/// additions, and a RED check is only ever a verdict about the FIRST crate cargo failed on. The
/// second half exists because the first one, alone, is read as a verdict about the crate under test
/// even when that crate was never compiled: cargo aborts at the first failing crate, so a broken
/// DEPENDENCY produces a red check whose output never mentions the generated crate at all, and the
/// header above it then reads as an accusation. Reported by the first consumer to take the no_std
/// path — their shim failed on a third-party dependency that does not declare `#![no_std]`, and the
/// six real errors in the generated crate only surfaced after that dependency was patched. Hence
/// the `Checking <name>` line as the reached-test: it is the one thing in cargo's output that
/// distinguishes "compiled clean" from "never reached".
fn cargo_toml(cli: &Cli) -> String {
    let lib_name = &cli.lib_name;
    format!(
        "\
# Generated by cddl-codegen. This whole directory is tool-owned and is DELETED AND REWRITTEN on
# every regeneration — edits here do not survive, and nothing in it is merged with a previous run.
#
# What it is for: proving the generated crate still compiles without `std`. Everything under
# `src/generated/**` is held no_std-clean by the generator; hand-written code added to the generated
# crate's own `src/lib.rs` is the part nobody but you can check. Run:
#
#   {CHECK_COMMAND}
#
# A failure here with an otherwise unmodified generated crate is caused by hand-written additions —
# which, in a split layout, includes the hand-owned half of a runtime crate this one depends on: its
# crate root, and any dependency it adds. The tool-written half forwards: every dependency this crate
# takes with `default-features = false` is named by its own `std` feature, so turning `std` off here
# turns it off all the way down.
#
# One caveat when reading a RED check: cargo stops at the first crate that fails, so everything after
# it — the crate under test included — is never compiled at all. A failure in a dependency is a
# verdict about that dependency and clears nothing behind it. A crate was actually reached only if a
# `Checking <name>` line for it appears in the output; the rest becomes visible once the first
# failure is fixed.

[package]
name = \"{lib_name}{NO_STD_CHECK_PACKAGE_SUFFIX}\"
version = \"0.0.0\"
edition = \"2024\"
publish = false

[dependencies]
{lib_name} = {{ path = \"{dep_path}\", default-features = false }}

# Deliberately standalone. Cargo unifies features across a whole dependency graph, so if this crate
# were a member of the surrounding workspace, ANY other member turning `std` back on for a shared
# dependency would silently satisfy this check while the crate is not in fact no_std-clean. An empty
# `[workspace]` table also means adding this directory can never disturb a workspace that does not
# list it.
[workspace]
",
        dep_path = dep_path(cli),
    )
}

fn lib_rs(cli: &Cli) -> String {
    // Under `--common-import-override` the runtime modules (`error`, `ordered_hash_map`, …) are not
    // emitted at all — the override crate owns them — so there is no generated type here to name.
    // The fallback names the crate itself, which still forces its whole `no_std` build.
    let lib_name_code = cli.lib_name_code();
    let body = if cli.export_static_files() {
        format!(
            "\
// Asserts more than \"the crate builds\": naming a generated type through the dependency's crate root
// also proves that root still re-exports `generated::*`, which is what hand edits to it can break.
pub type _NoStdCheckDeserializeError = {lib_name_code}::error::DeserializeError;
"
        )
    } else {
        format!(
            "\
// This crate was generated with `--common-import-override`, so it owns no runtime modules to name a
// type from; depending on it is what forces its whole `no_std` build. NOTE: the override crate must
// be in this crate's `[dependencies]` for the check to run at all — `--rust-dep` (or a `deps` edge
// under `--config`) writes that entry, and a hand-added one does just as well.
use {lib_name_code} as _;
"
        )
    };
    // `--deserialize-depth-limit` is the one flag whose output CANNOT pass this check, and the shim
    // is emitted unconditionally — so without this paragraph the consumer of such a crate meets a red
    // check that the file right above it calls their own fault. The crate's recursion guard is
    // `thread_local!`-based and has no core/alloc equivalent, so its serialization prelude carries a
    // `#[cfg(not(feature = "std"))] compile_error!`, which is exactly what a
    // `default-features = false` build triggers. The shim keeps EMITTING (maintainer ruling:
    // always-emit — a conditionally-absent directory is a worse surprise than an explained failure)
    // and explains itself instead, quoting the message the consumer will actually read in cargo's
    // output so a search for either text lands here.
    let depth_limit_note = if cli.deserialize_depth_limit.is_some() {
        format!(
            "\
//!
//! THIS CHECK FAILS BY DESIGN FOR THIS CRATE. It was generated with `--deserialize-depth-limit`,
//! whose recursion guard is `thread_local!`-based and therefore std-only, so a
//! `default-features = false` build of it stops at:
//!
//!   {}
//!
//! That is the documented incompatibility rather than a defect in the crate or in your additions —
//! see the `--deserialize-depth-limit` entry in the tool's command-line-flags documentation.
//! Regenerate without the flag if you need a no_std build of this crate.
",
            crate::generation::export::DEPTH_LIMIT_REQUIRES_STD
        )
    } else {
        String::new()
    };
    format!(
        "\
//! Generated by cddl-codegen. This crate is tool-owned and is DELETED AND REWRITTEN on every
//! regeneration — do not edit it.
//!
//! It depends on the generated crate with `default-features = false`, so a green check here means
//! that crate builds with no `std`. Host targets always link `std`, so the check needs a no-std
//! target:
//!
//!   {CHECK_COMMAND}
//!
//! A failure here with an otherwise unmodified generated crate is caused by hand-written additions —
//! which, in a split layout, includes the hand-owned half of a runtime crate this one depends on: its
//! crate root, and any dependency it adds. The tool-written half forwards: every dependency this crate
//! takes with `default-features = false` is named by its own `std` feature, so turning `std` off here
//! turns it off all the way down.
//!
//! One caveat when reading a RED check: cargo stops at the first crate that fails, so everything after
//! it — the crate under test included — is never compiled at all. A failure in a dependency is a
//! verdict about that dependency and clears nothing behind it. A crate was actually reached only if a
//! `Checking <name>` line for it appears in the output; the rest becomes visible once the first
//! failure is fixed.
{depth_limit_note}
#![no_std]

{body}"
    )
}
