// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:replace-start
        first(field);
        // cddl-codegen:replaces
        //   emit(field);
        // cddl-codegen:replace-end
        emit(field);
        // cddl-codegen:replace-start
        second(field);
        // cddl-codegen:replaces
        //   emit(field);
        // cddl-codegen:replace-end
    }
}
