// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self, x: u8) -> u8 {
        match x {
            0 => 
            // cddl-codegen:replace-start
            big(x)
            // cddl-codegen:replaces
            //   small(x)
            // cddl-codegen:replace-end
            ,
            _ => x,
        }
    }
}
