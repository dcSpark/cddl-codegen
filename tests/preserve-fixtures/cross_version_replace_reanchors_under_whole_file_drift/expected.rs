// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

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
        // cddl-codegen:replace-start
        writer.write_special(19)?;
        // cddl-codegen:replaces
        //   writer.write_special(7)?;
        // cddl-codegen:replace-end
        writer.write_unsigned(1)?;
        writer.write_unsigned(self.version)?;
        writer.write_unsigned(2)?;
        writer.write_array(self.extensions.len() as u64)?;
        for extension in &self.extensions {
            writer.write_unsigned(*extension)?;
        }
        for (key, value) in &self.fields {
            writer.write_unsigned(*key)?;
            writer.write_unsigned(*value)?;
        }
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty() && self.extensions.is_empty()
    }
}
