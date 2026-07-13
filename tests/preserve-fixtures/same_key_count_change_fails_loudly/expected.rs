// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user comment across regeneration.\nIt was attached to `impl A`, but the number of same-named items changed in the regenerated code.\nOriginal comment:\n// note");
impl A {
    fn f(&self) {
        go();
    }
}

impl A {
    fn g(&self) {
        go();
    }
}
