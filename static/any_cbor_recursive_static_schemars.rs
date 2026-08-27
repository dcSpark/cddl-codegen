// The schema half follows the serde half's conditional assembly: this refers to static_array and
// therefore cannot be appended to an ordinary AnyCbor-only schema runtime.
impl super::static_array::RecursiveSchema<AnyCbor> for super::static_array::NaturalAny {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        natural_any_cbor_schema(generator)
    }
}
