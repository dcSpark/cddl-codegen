// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:insert-start
        // cddl-codegen:keep why the extra call is needed
        extra();
        // cddl-codegen:insert-end
        go();
    }
}
