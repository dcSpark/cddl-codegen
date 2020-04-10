use cbor_event::{self, de::{Deserialize, Deserializer}, se::{Serialize, Serializer}};
use std::io::Write;
use wasm_bindgen::prelude::*;

// if we don't have anything else here anymore, we should probably just
// generate this directly at the top of serialization.rs

pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}