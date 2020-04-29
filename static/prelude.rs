use cbor_event::{self, de::{Deserialize, Deserializer}, se::{Serialize, Serializer}};
use std::io::{BufRead, Write};
use wasm_bindgen::prelude::*;

// we should probably just generate this directly at the top of serialization.rs
pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}

pub trait DeserializeEmbeddedGroup {
    fn deserialize_as_embedded_group<R: BufRead>(
        raw: &mut Deserializer<R>,
        len: cbor_event::Len,
    ) -> cbor_event::Result<Self> where Self: Sized;
}

// CBOR has int = int / nint
#[wasm_bindgen]
#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Int(i128);

#[wasm_bindgen]
impl Int {
    pub fn new(x: u64) -> Self {
        Self(x as i128)
    }

    pub fn new_negative(x: u64) -> Self {
        Self(-(x as i128))
    }
}

impl cbor_event::se::Serialize for Int {
    fn serialize<'se, W: Write>(&self, serializer: &'se mut Serializer<W>) -> cbor_event::Result<&'se mut Serializer<W>> {
        if self.0 < 0 {
            serializer.write_negative_integer((-self.0) as i64)
        } else {
            serializer.write_unsigned_integer(self.0 as u64)
        }
    }
}