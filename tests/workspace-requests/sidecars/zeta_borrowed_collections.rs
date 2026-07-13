// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every collection wrapper this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled
// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.
// Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).
#[allow(unused_imports)]
mod borrowed {
    use wr_dep_wasm::collections::IdxBarList;
    use wr_dep_wasm::collections::MapU64ToIdxFoo;
}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    ("wr_dep", "IdxBarList", "[* idx_bar]"),
    ("wr_dep", "MapU64ToIdxFoo", "{* uint => idx_foo}"),
];
