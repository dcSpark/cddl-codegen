// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:replace-start
        custom();
        // cddl-codegen:replaces
        //   dup();
        // cddl-codegen:replace-end
        dup();
        write_len(self.a);
    }
}
