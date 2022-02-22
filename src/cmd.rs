// TODO: add these as cmd args
// TODO: make non-annotation generate different DeserializeError that is simpler
//       and works with From<cbor_event:Error> only
pub const ANNOTATE_FIELDS: bool = true;
pub const GENERATE_TO_FROM_BYTES: bool = true;
pub const USE_EXTENDED_PRELUDE: bool = true;
pub const BINARY_WRAPPERS: bool = true;