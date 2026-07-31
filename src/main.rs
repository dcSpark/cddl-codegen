// The `static/*.rs` serialization runtime now uses `alloc::` paths (they resolve identically in a
// std crate, so this is not a no_std build). Several of those files are `include!`d verbatim into
// this crate's own test harness (`src/tests/any_cbor_tests.rs`, `json_schema_gen_tests.rs`,
// `ordered_set_runtime_tests.rs`), so the crate that compiles them has to link `alloc`. It is
// declared at the crate ROOT deliberately: only a root `extern crate` is reachable from nested
// inline modules, and the included runtime files contain some (`natural_any_cbor_btreemap`).
extern crate alloc;

pub(crate) mod alloc_import_inject;
pub(crate) mod api;
pub(crate) mod cargo_manifest;
pub(crate) mod cli;
pub(crate) mod comment_ast;
pub(crate) mod comment_preserve;
pub(crate) mod component_wit_deps;
pub(crate) mod config;
pub(crate) mod dep_graph;
pub(crate) mod emit_tests;
pub(crate) mod emit_tests_wasm;
pub(crate) mod extern_narrow;
pub(crate) mod generation;
pub(crate) mod import_prune;
pub(crate) mod intermediate;
pub(crate) mod log;
pub(crate) mod parsing;
pub(crate) mod rust_reserved;
pub(crate) mod utils;
pub(crate) mod wrapper_requests;

use clap::Parser;
use cli::Cli;

/// Errors are printed with `Display`, not by returning them from `main`: the default `Result`
/// termination renders through `Debug`, which wraps a `String` message in quotes, backslash-escapes
/// every `"` inside it, and prints embedded newlines as literal `\n` — mangling exactly the
/// multi-sentence diagnostics this tool writes.
fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let result = if config::is_config_mode(&argv) {
        generate_from_config(&argv)
    } else {
        api::generate_to_disk(&Cli::parse())
    };
    if let Err(error) = result {
        // `err!`, so "no level ever gates this" is an explicit classification rather than an absence:
        // it expands to a bare `eprintln!`, and the terminal error path is the exit path rather than
        // logging — `--verbosity error` hides warnings, never failure.
        crate::err!("Error: {error}");
        // Two failures a caller must be able to tell apart, which one exit code cannot. `1` says the
        // RUN failed: a config that would not expand, a spec that would not generate — the tool did
        // not do what it was asked, and fixing the input is the remedy. `2` says the run did exactly
        // what it was asked and the committed TREE it wrote into does not build; the message names
        // the dependency to regenerate, and repeating this command settles nothing. A CI job
        // legitimately treats those differently, and the exit code is the only channel it reads.
        // Downcast rather than a message match, so the classification is the error's own type: a
        // future second exit-2 condition declares itself the same way instead of by wording.
        std::process::exit(if error.downcast_ref::<config::VerdictError>().is_some() {
            2
        } else {
            1
        });
    }
}

/// `cddl-codegen --config <file.toml> [CRATE...]`: the command-line half of config mode.
///
/// Only the argv handling lives here — the run itself is `config::generate`, so the test suite drives
/// exactly the sequence a real invocation does rather than a re-implementation of it.
fn generate_from_config(argv: &[String]) -> Result<(), Box<dyn std::error::Error>> {
    config::reject_generation_flags(argv)?;
    let invocation = config::ConfigCli::parse_from(argv);
    let static_dir = invocation.static_dir.as_deref();
    // `--with-deps` is resolved into the selection before anything else sees it, so the run, the
    // listing, and the diagnostics that quote the selection back all mean the same set of crates.
    let selected = if invocation.with_deps {
        config::selection_with_deps(&invocation.config, &invocation.crates)?
    } else {
        invocation.crates.clone()
    };
    if invocation.print_flags {
        return config::print_flags(
            &invocation.config,
            &selected,
            static_dir,
            invocation.verbosity,
        );
    }
    config::generate(
        &invocation.config,
        &selected,
        static_dir,
        invocation.verbosity,
    )
}

#[cfg(test)]
mod tests;

/// Test-harness plumbing, not production API. `static/json_schema_gen.rs` exports
/// `custom_schema_impl!` with `#[macro_export]`, and its expansion reaches back into the hosting
/// crate as `$crate::json_schema_gen::…` — the path a generated crate really has. The suite compiles
/// that shipped file into THIS crate's test binary (`tests::json_schema_gen_tests`), so `$crate` is
/// this root, and the module has to be reachable from it or the macro cannot be invoked here at all.
/// One alias buys the macro's expansion the same rustc/clippy pass as the rest of the file, instead
/// of leaving it provable only by a nested-cargo run of a generated crate.
#[cfg(test)]
pub(crate) use tests::json_schema_gen_tests::json_schema_gen;
