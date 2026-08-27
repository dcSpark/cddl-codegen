// Appended only when `include_non_empty_vec`; cddl-matrix/no_std_check.ts's `json_schema` profile
// reaches this fragment with a wide exact array under `[+ ...]`.
impl<Inner, T> RecursiveSchema<super::non_empty::NonEmptyVec<T>> for NonEmpty<Inner>
where
    Inner: RecursiveSchema<T>,
{
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = self::sequence_schema::<Inner, T>(generator);
        schema.insert("minItems".to_owned(), 1.into());
        schema
    }
}
