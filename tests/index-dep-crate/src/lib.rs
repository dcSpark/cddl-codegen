// Minimal, wasm-CLEAN dependency rust crate for the `--extern-wrapper-index` link gate. A real
// cddl-codegen-generated `<dep>` rust crate has NO `#[wasm_bindgen]` (those bindings live in the
// separate `<dep>-wasm` crate), so this crate is plain rust — the property that lets the consumer's
// wasm crate link its dep's wasm crate for wasm32 without a duplicate `__wbg_*` symbol. It re-exports
// the shared serialization runtime from `extern-dep-crate` (so a consumer can point
// `--common-import-override=index_dep_crate` at it) and defines one plain extern type `IdxFoo`,
// serialized as a bare uint and Hash+Ord so it works as an `OrderedHashMap` KEY as well as a value.
// Its wasm wrapper and the collection wrappers live in `index-dep-crate-wasm`.
pub use extern_dep_crate::{
    error, non_empty, non_empty_map, ordered_hash_map, ordered_set, serialization,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdxFoo {
    inner: u64,
}

impl IdxFoo {
    pub fn new(inner: u64) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> u64 {
        self.inner
    }
}

impl cbor_event::se::Serialize for IdxFoo {
    fn serialize<'se>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {
        serializer.write_unsigned_integer(self.inner)
    }
}

impl serialization::Deserialize for IdxFoo {
    fn deserialize(
        raw: &mut cbor_event::de::Deserializer,
    ) -> Result<Self, error::DeserializeError> {
        Ok(Self {
            inner: raw.unsigned_integer()?,
        })
    }
}

// Second extern element, for the NonEmpty-wrapper cells whose LOOSE list (IdxBarList) is indexed
// while the restricted wrapper is NOT: the consumer mints the restricted class locally and its
// `try_from` must keep resolving against THIS crate's deferred loose class. Same shape as IdxFoo.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdxBar {
    inner: u64,
}

impl IdxBar {
    pub fn new(inner: u64) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> u64 {
        self.inner
    }
}

impl cbor_event::se::Serialize for IdxBar {
    fn serialize<'se>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {
        serializer.write_unsigned_integer(self.inner)
    }
}

impl serialization::Deserialize for IdxBar {
    fn deserialize(
        raw: &mut cbor_event::de::Deserializer,
    ) -> Result<Self, error::DeserializeError> {
        Ok(Self {
            inner: raw.unsigned_integer()?,
        })
    }
}

// Third extern element, for the INLINE `[+ idx_baz]` cell (synthesized NonEmptyIdxBazList minted
// locally, loose IdxBazList deferred here) — kept separate from IdxBar because a named `[+ idx_bar]`
// rule exists in the consumer spec and an inline `[+ idx_bar]` would dedup onto that rule's class.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdxBaz {
    inner: u64,
}

impl IdxBaz {
    pub fn new(inner: u64) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> u64 {
        self.inner
    }
}

impl cbor_event::se::Serialize for IdxBaz {
    fn serialize<'se>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {
        serializer.write_unsigned_integer(self.inner)
    }
}

impl serialization::Deserialize for IdxBaz {
    fn deserialize(
        raw: &mut cbor_event::de::Deserializer,
    ) -> Result<Self, error::DeserializeError> {
        Ok(Self {
            inner: raw.unsigned_integer()?,
        })
    }
}

// A BYTE-BACKED element, for the `@extern_companions`-on-a-raw-bytes-marker arm: the consumer
// declares `idx_hash = _CDDL_CODEGEN_RAW_BYTES_TYPE_` in its OWN spec and borrows this crate's
// wasm `IdxHashList`, exactly as `idx_foo` borrows `IdxFooList` through the extern marker. The
// `RawBytesEncoding` impl lives HERE rather than in the consumer because this crate owns the type
// and the trait is foreign to both (it comes from `extern_dep_crate`, which the consumer reaches
// through `--common-import-override=index_dep_crate`) — a consumer-side impl would be an orphan.
// `Hash + Ord` so it works as an `OrderedHashMap` KEY as well as a list element.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdxHash([u8; 4]);

impl IdxHash {
    pub fn new(inner: [u8; 4]) -> Self {
        Self(inner)
    }
}

impl serialization::RawBytesEncoding for IdxHash {
    fn to_raw_bytes(&self) -> &[u8] {
        &self.0
    }

    fn from_raw_bytes(bytes: &[u8]) -> Result<Self, error::DeserializeError> {
        <[u8; 4]>::try_from(bytes).map(Self).map_err(|_| {
            error::DeserializeFailure::CBOR(cbor_event::Error::WrongLen(
                4,
                cbor_event::Len::Len(bytes.len() as u64),
                "IdxHash",
            ))
            .into()
        })
    }
}
