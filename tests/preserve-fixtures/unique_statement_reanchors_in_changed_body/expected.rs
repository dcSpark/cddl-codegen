// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        write_tag(self.t);
        // cddl-codegen:keep
        // the length write
        write_len(self.a);
    }
}
