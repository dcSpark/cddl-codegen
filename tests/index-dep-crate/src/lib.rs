// Minimal, wasm-CLEAN dependency rust crate for the `--extern-wrapper-index` link gate. A real
// cddl-codegen-generated `<dep>` rust crate has NO `#[wasm_bindgen]` (those bindings live in the
// separate `<dep>-wasm` crate), so this crate is plain rust — the property that lets the consumer's
// wasm crate link its dep's wasm crate for wasm32 without a duplicate `__wbg_*` symbol. It re-exports
// the shared serialization runtime from `extern-dep-crate` (so a consumer can point
// `--common-import-override=index_dep_crate` at it) and defines one plain extern type `IdxFoo`,
// serialized as a bare uint and Hash+Ord so it works as an `OrderedHashMap` KEY as well as a value.
// Its wasm wrapper and the collection wrappers live in `index-dep-crate-wasm`.
pub use extern_dep_crate::{error, non_empty, ordered_hash_map, serialization};

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
    fn serialize<'se, W: std::io::Write>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer<W>,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer<W>> {
        serializer.write_unsigned_integer(self.inner)
    }
}

impl serialization::Deserialize for IdxFoo {
    fn deserialize<R: std::io::BufRead + std::io::Seek>(
        raw: &mut cbor_event::de::Deserializer<R>,
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
    fn serialize<'se, W: std::io::Write>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer<W>,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer<W>> {
        serializer.write_unsigned_integer(self.inner)
    }
}

impl serialization::Deserialize for IdxBar {
    fn deserialize<R: std::io::BufRead + std::io::Seek>(
        raw: &mut cbor_event::de::Deserializer<R>,
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
    fn serialize<'se, W: std::io::Write>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer<W>,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer<W>> {
        serializer.write_unsigned_integer(self.inner)
    }
}

impl serialization::Deserialize for IdxBaz {
    fn deserialize<R: std::io::BufRead + std::io::Seek>(
        raw: &mut cbor_event::de::Deserializer<R>,
    ) -> Result<Self, error::DeserializeError> {
        Ok(Self {
            inner: raw.unsigned_integer()?,
        })
    }
}
