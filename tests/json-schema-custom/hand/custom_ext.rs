// The hand-owned half of the `custom_schema_impl!` fixture, installed by
// `integration_tests::custom_schema_impl_writes_a_closing_document` at `rust/src/custom_ext.rs` and
// declared from the seed-once `rust/src/lib.rs`. Committed as a real file rather than written from a
// string literal in the test, so the hand-authored side of the feature is reviewable as the code a
// consumer would actually write.
//
// Its home is the point: `schemars::JsonSchema` is a foreign trait, so the orphan rule allows this
// impl only in the crate that DEFINES `CustomExt` — and inside that crate only outside
// `src/generated/**`, which every regeneration clobbers.

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct CustomExt {
    pub bytes: Vec<u8>,
}

impl cbor_event::se::Serialize for CustomExt {
    fn serialize<'a>(
        &self,
        serializer: &'a mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'a mut cbor_event::se::Serializer> {
        serializer.write_bytes(&self.bytes)
    }
}

impl crate::generated::serialization::Deserialize for CustomExt {
    fn deserialize(
        raw: &mut cbor_event::de::Deserializer,
    ) -> Result<Self, crate::generated::error::DeserializeError> {
        Ok(Self {
            bytes: raw.bytes()?,
        })
    }
}

// The whole subject of the cell. `crate::` because this spec is generated without
// `--common-import-override`, so the module hosting the macro is a module of THIS crate; the
// expansion's `$crate::json_schema_gen::…` resolves through the seed-once root's
// `pub use generated::*;`. The path is relative to THIS file, not to the file defining the macro.
crate::custom_schema_impl!(CustomExt, "custom_schemas/CustomExt.json");
