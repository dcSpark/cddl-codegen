// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen found a comment it cannot classify.\nOutside a `cddl-codegen:` block every comment in a generated file is tool-owned, and this one is neither emitted by this run nor marked as yours. It is one of two things:\n(1) stale tool output whose text changed or was removed upstream — delete this whole block;\n(2) your own comment — delete this whole block and re-add the comment with a marker:\n// cddl-codegen:keep <your text>\nor, for a run of comment lines (the only form that can carry `///`/`//!` doc comments), a bare marker directly above the run:\n// cddl-codegen:keep\n/// <your text>\nOriginal comment:\n// stringify identically are a collision, and we strict-fail rather than silently drop");
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
