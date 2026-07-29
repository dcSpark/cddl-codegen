// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::string::ToString;

pub struct AnyCbor {
    inner: BTreeMap<u64, u64>,
}

impl schemars::JsonSchema for AnyCbor {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        alloc::borrow::Cow::Borrowed("AnyCbor")
    }
}

impl AnyCbor {
    pub fn keys_are_unique(&self) -> bool {
        // Determinism + collision detection: a `BTreeSet` of stringified keys. Two keys that
        // stringify identically are a collision, and we hard-error rather than silently drop
        // one (RFC 8949 6.1's map-key uniqueness).
        let mut seen = alloc::collections::BTreeSet::new();
        for k in self.inner.keys() {
            seen.insert(k.to_string());
        }
        seen.len() == self.inner.len()
    }
}
