// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

pub struct Foo {
    // cddl-codegen:keep
    // the bound below is deliberate:
    // upstream never sends more than this,
    // and a wider type would change the encoding.
    pub a: u64,
}
