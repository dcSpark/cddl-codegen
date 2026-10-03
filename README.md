# cddl-codegen

cddl-codegen is a library to generate Rust, WASM and JSON code from CDDL specifications

See docs [here](https://dcspark.github.io/cddl-codegen/)

## Library IR compatibility

Direct users of the public `intermediate` API must select conceptual types and bound roles explicitly.
`RustType` no longer implements `Deref<Target = ConceptualRustType>`; replace conceptual-only queries such as `ty.is_fixed_value()` with `ty.conceptual_type.is_fixed_value()` and conceptual borrows/coercions with `&ty.conceptual_type`.
Complete-type queries such as `for_rust_member`, `cbor_types`, and `has_value_bounds` remain on `RustType`.
Replace the former public `with_bounds` method with `with_value_bounds` or `with_occurrence_bounds` for the intended role.
`RustTypeSerializeConfig::bounds` uses `Option<TypeBounds>`, array/table IR bounds use `Option<OccurrenceWindow>`, and dynamic-row bounds use `Option<RestOccurrenceWindow>`.
These are library source compatibility changes; generated collection runtime APIs remain unchanged.
The owning [RustType and typed-bounds rustdoc](src/intermediate/rust_type.rs) and [array/table/RestRow rustdoc](src/intermediate/structs.rs) describe raw projections, normalization, and validation boundaries.
