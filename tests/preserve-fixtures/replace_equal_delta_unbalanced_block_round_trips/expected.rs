// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn serialize(&self, k: u8) {
        // cddl-codegen:insert-start
        let flag = self.should_include();
        // cddl-codegen:insert-end
        let len = 4 + (
            // cddl-codegen:replace-start
            if flag { 1 } else { 0 }
            // cddl-codegen:replaces
            //   if self.vp != 0 { 1 } else { 0 }
            // cddl-codegen:replace-end
        );
        let ordered = 4 + (
            // cddl-codegen:replace-start
            if flag { 1 } else { 0 }
            // cddl-codegen:replaces
            //   if self.vp != 0 { 1 } else { 0 }
            // cddl-codegen:replace-end
        );
        match k {
            4 => {
                // cddl-codegen:replace-start
                if flag {
                // cddl-codegen:replaces
                //   if self.vp != 0 {
                // cddl-codegen:replace-end
                    write_vp();
                }
            }
            _ => {}
        }
    }
}
