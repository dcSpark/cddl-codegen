use cbor_event::{self, de::Deserializer, se::{Serialize, Serializer}};
use std::io::{BufRead, Write};
use wasm_bindgen::prelude::*;

pub enum Key {
    Str(String),
    Uint(u64),
}

// we might want to add more info like which field, etc
pub enum DeserializeError {
    BreakInDefiniteLen,
    CBOR(cbor_event::Error),
    ExpectedNull,
    MandatoryFieldMissing(Key),
    TagMismatch{
        found: u64,
        expected: u64,
    },
    UnknownKey(Key),
    UnexpectedKeyType(cbor_event::Type),
}

impl From<cbor_event::Error> for DeserializeError {
    fn from(err: cbor_event::Error) -> DeserializeError {
        DeserializeError::CBOR(err)
    }
}

// we should probably just generate this directly at the top of serialization.rs
pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}

// same as cbor_event::de::Deserialize but with our DeserializeError
pub trait Deserialize {
    fn deserialize<R: BufRead>(
        raw: &mut Deserializer<R>,
    ) -> Result<Self, DeserializeError> where Self: Sized;
}

impl<T: cbor_event::de::Deserialize> Deserialize for T {
    fn deserialize<R: BufRead>(raw: &mut Deserializer<R>) -> Result<T, DeserializeError> {
        T::deserialize(raw).map_err(|e| DeserializeError::from(e))
    }
}

pub trait DeserializeEmbeddedGroup {
    fn deserialize_as_embedded_group<R: BufRead>(
        raw: &mut Deserializer<R>,
        len: cbor_event::Len,
    ) -> Result<Self, DeserializeError> where Self: Sized;
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
// TODO: deserialize