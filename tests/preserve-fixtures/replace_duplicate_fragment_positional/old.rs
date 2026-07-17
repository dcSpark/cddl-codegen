// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        emit(field);
        // cddl-codegen:replace-start
        custom(field);
        // cddl-codegen:replaces
        //   emit(field);
        // cddl-codegen:replace-end
        emit(field);
    }
}
