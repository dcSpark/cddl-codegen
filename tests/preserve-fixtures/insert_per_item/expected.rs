// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl A {
    fn f(&self) {
        // cddl-codegen:insert-start
        self.check();
        // cddl-codegen:insert-end
        go();
    }
}

impl B {
    fn g(&self) {
        one();
        two();
    }
}
