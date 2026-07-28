pub(crate) mod api;
pub(crate) mod cargo_manifest;
pub(crate) mod cli;
pub(crate) mod comment_ast;
pub(crate) mod comment_preserve;
pub(crate) mod config;
pub(crate) mod dep_graph;
pub(crate) mod emit_tests;
pub(crate) mod emit_tests_wasm;
pub(crate) mod extern_narrow;
pub(crate) mod generation;
pub(crate) mod import_prune;
pub(crate) mod intermediate;
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
        eprintln!("Error: {error}");
        std::process::exit(1);
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
    if invocation.print_flags {
        return config::print_flags(&invocation.config, &invocation.crates, static_dir);
    }
    config::generate(&invocation.config, &invocation.crates, static_dir)
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
