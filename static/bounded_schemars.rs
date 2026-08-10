impl<T: schemars::JsonSchema, const MIN: u64, const MAX: u64> schemars::JsonSchema
    for BoundedVec<T, MIN, MAX>
{
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("BoundedVec<{}, {MIN}, {MAX}>", T::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = Vec::<T>::json_schema(generator);
        schema.insert("minItems".to_owned(), MIN.into());
        if MAX != u64::MAX { schema.insert("maxItems".to_owned(), MAX.into()); }
        schema
    }
    fn inline_schema() -> bool { Vec::<T>::inline_schema() }
}

#[cfg(test)]
mod schemars_tests {
    use super::*;

    #[test]
    fn schema_carries_exact_item_window() {
        let schema = schemars::schema_for!(BoundedVec<u64, 2, 3>);
        let rendered = serde_json::to_value(schema).unwrap();
        assert_eq!(rendered["minItems"], 2);
        assert_eq!(rendered["maxItems"], 3);
    }
}
