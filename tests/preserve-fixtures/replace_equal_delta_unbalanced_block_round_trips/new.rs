// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn serialize(&self, k: u8) {
        let len = 4 + (
            if self.vp != 0 { 1 } else { 0 }
        );
        let ordered = 4 + (
            if self.vp != 0 { 1 } else { 0 }
        );
        match k {
            4 => {
                if self.vp != 0 {
                    write_vp();
                }
            }
            _ => {}
        }
    }
}
