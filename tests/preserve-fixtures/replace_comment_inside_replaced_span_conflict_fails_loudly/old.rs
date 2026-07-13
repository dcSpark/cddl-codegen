// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:replace-start
        new_ab();
        // cddl-codegen:replaces
        //   a();
        //   b();
        // cddl-codegen:replace-end
        // MOVE ME
        c();
    }
}
