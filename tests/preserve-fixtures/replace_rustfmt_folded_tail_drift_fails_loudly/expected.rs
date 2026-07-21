// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user code block across regeneration.\nThe generated code for `impl Foo` changed, so its recorded original no longer appears (drift). Re-review the block and re-record the original under `replaces`.\nOriginal code block:\n            // cddl-codegen:replace-start\n            unknown => {\n                let _ = unknown;\n                Ok(key)\n            } \n            // cddl-codegen:replaces\n              // unknown => return Err(()),\n              // cddl-codegen:replace-end");
impl Foo {
    fn deserialize(&self, key: u8) -> Result<u8, ()> {
        match key {
            0 => Ok(key),
            other => return Err(()),
        }
    }
}
