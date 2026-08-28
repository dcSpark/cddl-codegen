// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;

pub struct Ledger {
    entries: BTreeMap<u64, u64>,
    schema_version: u64,
}

impl Ledger {
    pub fn serialize(&self, serializer: &mut Serializer) -> Result<(), Error> {
        serializer.write_map(2)?;
        serializer.write_unsigned(0)?;
        serializer.write_unsigned(self.schema_version)?;
        serializer.write_unsigned(1)?;
        let sorted_keys: Vec<_> = self.entries.keys().copied().collect();
        let seen: BTreeSet<_> = sorted_keys.iter().copied().collect();
        debug_assert_eq!(seen.len(), sorted_keys.len());

        serializer.write_array(self.entries.len() as u64)?;
        for key in sorted_keys {
            serializer.write_unsigned(key)?;
            serializer.write_unsigned(self.entries[&key])?;
        }
        Ok(())
    }

    pub fn entry_count(&self) -> usize {
        self.entries.len()
    }
}
