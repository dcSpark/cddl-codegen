pub(crate) mod api;
pub(crate) mod cli;
pub(crate) mod comment_ast;
pub(crate) mod dep_graph;
pub(crate) mod generation;
pub(crate) mod intermediate;
pub(crate) mod parsing;
pub(crate) mod rust_reserved;
pub(crate) mod utils;

use clap::Parser;
use cli::Cli;
use std::sync::LazyLock;

pub static CLI_ARGS: LazyLock<Cli> = LazyLock::new(Cli::parse);

fn main() -> Result<(), Box<dyn std::error::Error>> {
    api::generate_to_disk(&CLI_ARGS)
}

#[cfg(test)]
mod integration_tests;
#[cfg(test)]
mod robustness_tests;
#[cfg(test)]
mod snapshot_tests;
