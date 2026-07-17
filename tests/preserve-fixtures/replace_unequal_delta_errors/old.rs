// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:replace-start
        custom(x)
        // cddl-codegen:replaces
        //   emit(x) {
        // cddl-codegen:replace-end
        rest();
    }
}
