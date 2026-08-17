// Mirrors `src/main.rs`: the `static/*.rs` runtime uses `alloc::` paths, and the bin crate's test
// harness `include!`s several of those files, so the crate roots link `alloc`. Kept on both roots
// so the two stay aligned (see AGENTS.md § bin/lib module duplication).
extern crate alloc;

pub mod alloc_import_inject;
pub mod api;
pub mod cargo_manifest;
pub mod cli;
pub mod comment_ast;
pub mod comment_preserve;
pub mod component_wit_deps;
pub mod config;
pub mod dep_graph;
pub mod emit_tests;
pub mod emit_tests_wasm;
pub mod extern_narrow;
pub mod generation;
pub mod import_prune;
pub mod intermediate;
pub mod log;
pub mod parsing;
pub mod recursion_boundary;
pub mod runtime_flavor;
pub mod rust_reserved;
pub mod utils;
pub mod wrapper_requests;
