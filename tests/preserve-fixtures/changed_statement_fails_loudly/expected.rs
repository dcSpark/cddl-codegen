// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user comment across regeneration.\nIt was attached inside `impl Foo`, whose generated code changed.\nOriginal comment:\n        // cddl-codegen:keep\n        // note");
impl Foo {
    fn go(&self) {
        write_len(self.b);
    }
}
