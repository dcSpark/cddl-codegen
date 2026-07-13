// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user comment across regeneration.\nIt was attached inside `impl Foo`, whose generated code changed.\nOriginal comment:\n// which one");
impl Foo {
    fn go(&self) {
        push(x);
        other();
        push(x);
    }
}
