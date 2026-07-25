// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn deserialize(&self, key: u8) -> Result<u8, ()> {
        match key {
            0 => Ok(key),
            unknown => {
                let _ = unknown;
                Ok(key)
            } // cddl-codegen:keep the fallthrough arm is deliberate
        }
    }
}
