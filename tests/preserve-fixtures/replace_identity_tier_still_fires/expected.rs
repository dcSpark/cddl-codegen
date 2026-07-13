// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// KEEP THIS NOTE
pub struct Bar {
    pub b: u64,
}

impl Foo {
    fn go(&self) {
        // cddl-codegen:replace-start
        custom();
        // cddl-codegen:replaces
        //   old();
        // cddl-codegen:replace-end
        write_len(self.a);
    }
}
