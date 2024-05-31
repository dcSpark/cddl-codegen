use std::collections::BTreeMap;
use wasm_bindgen::prelude::{wasm_bindgen, JsError};
use cbor_encodings::ExternCrateFooEncoding;

pub mod cbor_encodings;
pub mod error;
pub mod ordered_hash_map;
pub mod serialization;
pub mod sub;

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct ExternCrateFoo {
    index_0: u64,
    index_1: String,
    index_2: Vec<u8>,
    encodings: Option<ExternCrateFooEncoding>,
}

#[wasm_bindgen]
impl ExternCrateFoo {
    pub fn new(index_0: u64, index_1: String, index_2: Vec<u8>) -> Self {
        Self {
            index_0,
            index_1,
            index_2,
            encodings: None,
        }
    }

    pub fn to_cbor_bytes(&self) -> Vec<u8> {
        serialization::ToCBORBytes::to_cbor_bytes(&self)
    }

    pub fn from_cbor_bytes(cbor_bytes: &[u8]) -> Result<ExternCrateFoo, JsError> {
        serialization::Deserialize::from_cbor_bytes(cbor_bytes)
            .map_err(|e| JsError::new(&format!("from_bytes: {}", e)))
    }

    pub fn index_0(&self) -> u64 {
        self.index_0
    }

    pub fn index_1(&self) -> String {
        self.index_1.clone()
    }

    pub fn index_2(&self) -> Vec<u8> {
        self.index_2.clone()
    }
}
