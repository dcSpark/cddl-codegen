// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:insert-start
        pre();
        // cddl-codegen:insert-end
        // cddl-codegen:replace-start
        new_body();
        // cddl-codegen:replaces
        //   old_body();
        // cddl-codegen:replace-end
        tail();
    }
}
