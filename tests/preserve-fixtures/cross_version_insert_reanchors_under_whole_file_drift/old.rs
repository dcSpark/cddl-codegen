// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

use std::collections::{BTreeMap, BTreeSet};

pub struct Ledger {
    entries: BTreeMap<u64, u64>,
}

impl Ledger {
    pub fn serialize(&self, serializer: &mut Serializer) -> Result<(), Error> {
        serializer.write_map(self.entries.len() as u64)?;
        let seen: BTreeSet<_> = self.entries.keys().copied().collect();
        debug_assert_eq!(seen.len(), self.entries.len());

        // cddl-codegen:insert-start
        self.write_compatibility_extension(serializer)?;
        // cddl-codegen:insert-end
        serializer.write_array(self.entries.len() as u64)?;
        for (key, value) in &self.entries {
            serializer.write_unsigned(*key)?;
            serializer.write_unsigned(*value)?;
        }
        Ok(())
    }
}
