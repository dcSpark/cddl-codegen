// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user comment across regeneration.\nIt was attached inside `impl Envelope`, whose generated code changed.\nOriginal comment:\n        // cddl-codegen:keep\n        // Keep this wire-version override while upgrading generated serializers.");
extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::vec::Vec;

pub struct Envelope {
    fields: BTreeMap<u64, u64>,
    version: u64,
    extensions: Vec<u64>,
}

impl Envelope {
    pub fn serialize(&self, writer: &mut Writer) -> Result<(), Error> {
        writer.write_map(3)?;
        writer.write_unsigned(0)?;
        writer.write_tag(7)?;
        writer.write_unsigned(self.version)?;
        writer.write_unsigned(1)?;
        writer.write_array(self.extensions.len() as u64)?;
        for extension in &self.extensions {
            writer.write_unsigned(*extension)?;
        }
        writer.write_unsigned(2)?;
        for (key, value) in &self.fields {
            writer.write_unsigned(*key)?;
            writer.write_unsigned(*value)?;
        }
        Ok(())
    }

    pub fn schema_version(&self) -> u64 {
        self.version
    }
}
