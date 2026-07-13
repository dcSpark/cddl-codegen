// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Gone {
    fn go(&self) {
        // cddl-codegen:replace-start
        custom();
        // cddl-codegen:replaces
        //   old();
        // cddl-codegen:replace-end
        write_len(self.a);
    }
}

pub struct Stay {
    pub b: u64,
}
