// TODO: add these as cmd args
// TODO: make non-annotation generate different DeserializeError that is simpler
//       and works with From<cbor_event:Error> only
pub const ANNOTATE_FIELDS: bool = true;
// TODO: fix and/or allow both options (take to_from_bytes!() from csl)
pub const GENERATE_TO_FROM_BYTES: bool = false;
pub const USE_EXTENDED_PRELUDE: bool = true;
pub const BINARY_WRAPPERS: bool = true;
// Preservs CBOR encoding upon deserialization e.g. definite vs indefinite, map ordering
pub const PRESERVE_ENCODINGS: bool = true;
pub const CANONICAL_FORM: bool = true;