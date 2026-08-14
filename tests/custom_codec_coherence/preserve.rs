use crate::generated::error::{DeserializeError, DeserializeFailure};

pub fn coherence_bytes(value: &Holder) -> Vec<u8> {
    let mut serializer = cbor_event::se::Serializer::new_vec();
    cbor_event::se::Serialize::serialize(value, &mut serializer).unwrap();
    serializer.finalize()
}

fn read_text(raw: &mut cbor_event::de::Deserializer) -> Result<(String, StringEncoding), DeserializeError> {
    let (text, encoding) = raw.text_sz()?;
    Ok((text, encoding.into()))
}

macro_rules! ref_bytes_codec {
    ($write:ident, $read:ident, $text:literal) => {
        pub fn $write<'se>(serializer: &'se mut cbor_event::se::Serializer, bytes: &[u8], enc: &StringEncoding) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> { serializer.write_text_sz(core::str::from_utf8(bytes).unwrap(), enc.to_str_len_sz(bytes.len() as u64)) }
        pub fn $read(raw: &mut cbor_event::de::Deserializer) -> Result<(Vec<u8>, StringEncoding), DeserializeError> { let (text, enc) = read_text(raw)?; if text == $text { Ok((text.into_bytes(), enc)) } else { Err(DeserializeFailure::CBOR(cbor_event::Error::CustomError("unexpected coherence marker".into())).into()) } }
    };
}
macro_rules! value_bytes_codec {
    ($write:ident, $read:ident, $text:literal) => {
        pub fn $write<'se>(serializer: &'se mut cbor_event::se::Serializer, bytes: &[u8], enc: StringEncoding) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> { serializer.write_text_sz(core::str::from_utf8(bytes).unwrap(), enc.to_str_len_sz(bytes.len() as u64)) }
        pub fn $read(raw: &mut cbor_event::de::Deserializer) -> Result<(Vec<u8>, StringEncoding), DeserializeError> { let (text, enc) = read_text(raw)?; if text == $text { Ok((text.into_bytes(), enc)) } else { Err(DeserializeFailure::CBOR(cbor_event::Error::CustomError("unexpected coherence marker".into())).into()) } }
    };
}
ref_bytes_codec!(write_alias, read_alias, "a");
ref_bytes_codec!(write_array_field, read_array_field, "b");
ref_bytes_codec!(write_map_field, read_map_field, "c");
value_bytes_codec!(write_key, read_key, "k");
value_bytes_codec!(write_value, read_value, "v");
value_bytes_codec!(write_rest_key, read_rest_key, "r");
value_bytes_codec!(write_rest_value, read_rest_value, "s");

macro_rules! complete_codec {
    ($write:ident, $read:ident, $type:ident, $text:literal, $value:expr) => {
        pub fn $write<'se>(serializer: &'se mut cbor_event::se::Serializer, _: &$type) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> { serializer.write_text($text) }
        pub fn $read(raw: &mut cbor_event::de::Deserializer) -> Result<$type, DeserializeError> { if raw.text()? == $text { Ok($value) } else { Err(DeserializeFailure::CBOR(cbor_event::Error::CustomError("coherence marker".into())).into()) } }
    };
}
complete_codec!(write_array_record, read_array_record, ArrayRecord, "ar", ArrayRecord::new(1));
complete_codec!(write_map_record, read_map_record, MapRecord, "mr", MapRecord::new(1));
complete_codec!(write_whole_table, read_whole_table, WholeTable, "tw", WholeTable::from(crate::generated::ordered_hash_map::OrderedHashMap::from_iter([("w".to_owned(), 1)])));
complete_codec!(write_generic_table, read_generic_table, GenericTableUse, "gt", GenericTableUse::from(crate::generated::ordered_hash_map::OrderedHashMap::from_iter([("g".to_owned(), 1)])));
complete_codec!(write_pair_table, read_pair_table, PairTable, "pt", PairTable::from(PairMap::from(vec![("p".to_owned(), 1)])));
complete_codec!(write_non_empty_table, read_non_empty_table, NonEmptyTable, "nt", NonEmptyTable::from(NonEmptyMap::new("n".to_owned(), 1)));
complete_codec!(write_bounded_table, read_bounded_table, BoundedTable, "bt", BoundedTable::from(BoundedMap::try_from(crate::generated::ordered_hash_map::OrderedHashMap::from_iter([("b".to_owned(), 1), ("c".to_owned(), 2)])).unwrap()));
complete_codec!(write_reject_table, read_reject_table, RejectTable, "rt", RejectTable::from(crate::generated::ordered_hash_map::OrderedHashMap::from_iter([("r".to_owned(), 1)])));
complete_codec!(write_non_empty_pair_table, read_non_empty_pair_table, NonEmptyPairTable, "np", NonEmptyPairTable::from(NonEmptyPairMap::try_from(vec![("n".to_owned(), 1)]).unwrap()));
complete_codec!(write_bounded_pair_table, read_bounded_pair_table, BoundedPairTable, "bp", BoundedPairTable::from(BoundedPairMap::try_from(vec![("b".to_owned(), 1), ("c".to_owned(), 2)]).unwrap()));
