// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user code block across regeneration.\nThe generated code for `impl Foo` changed, so its recorded original no longer appears (drift). Re-review the block and re-record the original under `replaces`.\nOriginal code block:\n        // cddl-codegen:replace-start\n        custom();\n        // cddl-codegen:replaces\n        //   old();\n        // cddl-codegen:replace-end");
impl Foo {
    fn go(&self) {
        renamed();
        write_len(self.a);
    }
}
