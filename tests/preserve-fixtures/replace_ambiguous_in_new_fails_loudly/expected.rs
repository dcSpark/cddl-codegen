// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user code block across regeneration.\nIts recorded original appears more than once in the regenerated `impl Foo`, so which occurrence it replaces is ambiguous.\nOriginal code block:\n        // cddl-codegen:replace-start\n        custom();\n        // cddl-codegen:replaces\n        //   old();\n        // cddl-codegen:replace-end");
impl Foo {
    fn go(&self) {
        old();
        old();
        write_len(self.a);
    }
}
