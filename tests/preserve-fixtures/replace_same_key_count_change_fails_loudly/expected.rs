// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user code block across regeneration.\nIt was attached to `impl Foo`, but the number of same-named items changed in the regenerated code.\nOriginal code block:\n        // cddl-codegen:replace-start\n        custom();\n        // cddl-codegen:replaces\n        //   old();\n        // cddl-codegen:replace-end");
impl Foo {
    fn go(&self) {
        old();
        write_len(self.a);
    }
}

impl Foo {
    fn other(&self) {
        noop();
    }
}
