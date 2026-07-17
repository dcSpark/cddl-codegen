// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:replace-start
        } else {
        // cddl-codegen:replaces
        //   old();
        // cddl-codegen:replace-end
        write_len(self.a);
    }
}
