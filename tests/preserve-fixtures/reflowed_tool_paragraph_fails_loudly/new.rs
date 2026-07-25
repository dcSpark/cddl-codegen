// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// the serializer writes each field in the canonical order the
// spec declares, then closes the map. Deserialization mirrors this
// exactly, so a round trip is byte-stable.
pub struct Foo {
    pub a: u64,
}
