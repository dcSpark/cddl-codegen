use crate::generated::error::{DeserializeError, DeserializeFailure};

pub fn coherence_bytes(value: &Holder) -> Vec<u8> {
    let mut serializer = cbor_event::se::Serializer::new_vec();
    cbor_event::se::Serialize::serialize(value, &mut serializer).unwrap();
    serializer.finalize()
}

fn write_text<'se>(
    serializer: &'se mut cbor_event::se::Serializer,
    text: &str,
) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {
    serializer.write_text(text)
}

fn read_text(raw: &mut cbor_event::de::Deserializer) -> Result<String, DeserializeError> {
    Ok(raw.text()?)
}

macro_rules! bytes_codec {
    ($write:ident, $read:ident, $text:literal) => {
        pub fn $write<'se>(serializer: &'se mut cbor_event::se::Serializer, _: &[u8]) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> { write_text(serializer, $text) }
        pub fn $read(raw: &mut cbor_event::de::Deserializer) -> Result<Vec<u8>, DeserializeError> { let text = read_text(raw)?; if text == $text { Ok(text.into_bytes()) } else { Err(DeserializeFailure::CBOR(cbor_event::Error::CustomError("unexpected coherence marker".into())).into()) } }
    };
}
bytes_codec!(write_alias, read_alias, "a");
bytes_codec!(write_array_field, read_array_field, "b");
bytes_codec!(write_map_field, read_map_field, "c");
bytes_codec!(write_key, read_key, "k");
bytes_codec!(write_value, read_value, "v");
bytes_codec!(write_rest_key, read_rest_key, "r");
bytes_codec!(write_rest_value, read_rest_value, "s");

pub fn write_array_record<'se>(serializer: &'se mut cbor_event::se::Serializer, _: &ArrayRecord) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> { write_text(serializer, "ar") }
pub fn read_array_record(raw: &mut cbor_event::de::Deserializer) -> Result<ArrayRecord, DeserializeError> { if read_text(raw)? == "ar" { Ok(ArrayRecord::new(1)) } else { Err(DeserializeFailure::CBOR(cbor_event::Error::CustomError("array record marker".into())).into()) } }
pub fn write_map_record<'se>(serializer: &'se mut cbor_event::se::Serializer, _: &MapRecord) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> { write_text(serializer, "mr") }
pub fn read_map_record(raw: &mut cbor_event::de::Deserializer) -> Result<MapRecord, DeserializeError> { if read_text(raw)? == "mr" { Ok(MapRecord::new(1)) } else { Err(DeserializeFailure::CBOR(cbor_event::Error::CustomError("map record marker".into())).into()) } }

macro_rules! table_codec {
    ($write:ident, $read:ident, $type:ident, $text:literal, $value:expr) => {
        pub fn $write<'se>(serializer: &'se mut cbor_event::se::Serializer, _: &$type) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> { write_text(serializer, $text) }
        pub fn $read(raw: &mut cbor_event::de::Deserializer) -> Result<$type, DeserializeError> { if read_text(raw)? == $text { Ok($value) } else { Err(DeserializeFailure::CBOR(cbor_event::Error::CustomError("table marker".into())).into()) } }
    };
}
table_codec!(write_whole_table, read_whole_table, WholeTable, "tw", WholeTable::from(std::collections::BTreeMap::from([("w".to_owned(), 1)])));
table_codec!(write_generic_table, read_generic_table, GenericTableUse, "gt", GenericTableUse::from(std::collections::BTreeMap::from([("g".to_owned(), 1)])));
table_codec!(write_pair_table, read_pair_table, PairTable, "pt", PairTable::from(PairMap::from(vec![("p".to_owned(), 1)])));
table_codec!(write_non_empty_table, read_non_empty_table, NonEmptyTable, "nt", NonEmptyTable::from(NonEmptyMap::new("n".to_owned(), 1)));
table_codec!(write_bounded_table, read_bounded_table, BoundedTable, "bt", BoundedTable::from(BoundedMap::try_from(std::collections::BTreeMap::from([("b".to_owned(), 1), ("c".to_owned(), 2)])).unwrap()));
table_codec!(write_reject_table, read_reject_table, RejectTable, "rt", RejectTable::from(std::collections::BTreeMap::from([("r".to_owned(), 1)])));
table_codec!(write_non_empty_pair_table, read_non_empty_pair_table, NonEmptyPairTable, "np", NonEmptyPairTable::from(NonEmptyPairMap::try_from(vec![("n".to_owned(), 1)]).unwrap()));
table_codec!(write_bounded_pair_table, read_bounded_pair_table, BoundedPairTable, "bp", BoundedPairTable::from(BoundedPairMap::try_from(vec![("b".to_owned(), 1), ("c".to_owned(), 2)]).unwrap()));
