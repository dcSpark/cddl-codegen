// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

use std::collections::BTreeMap;

pub struct Envelope {
    fields: BTreeMap<u64, u64>,
    version: u64,
}

impl Envelope {
    pub fn serialize(&self, writer: &mut Writer) -> Result<(), Error> {
        writer.write_map(self.fields.len() as u64 + 1)?;
        writer.write_unsigned(0)?;
        // cddl-codegen:keep
        // Keep this wire-version override while upgrading generated serializers.
        writer.write_special(7)?;
        writer.write_unsigned(self.version)?;
        for (key, value) in &self.fields {
            writer.write_unsigned(*key)?;
            writer.write_unsigned(*value)?;
        }
        Ok(())
    }
}
