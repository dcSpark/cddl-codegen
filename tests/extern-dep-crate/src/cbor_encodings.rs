use crate::serialization::{LenEncoding, StringEncoding};

#[derive(Clone, Debug, Default)]
pub struct ExternCrateFooEncoding {
    pub len_encoding: LenEncoding,
    pub tag_encoding: Option<cbor_event::Sz>,
    pub index_0_encoding: Option<cbor_event::Sz>,
    pub index_1_encoding: StringEncoding,
    pub index_2_encoding: StringEncoding,
}
