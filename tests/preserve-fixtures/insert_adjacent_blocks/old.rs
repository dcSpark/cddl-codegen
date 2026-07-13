// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:insert-start
        self.first();
        // cddl-codegen:insert-end
        // cddl-codegen:insert-start
        self.second();
        // cddl-codegen:insert-end
        write_len(self.a);
    }
}
