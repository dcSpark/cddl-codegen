// Appended only when `include_bounded_vec`; cddl-matrix/no_std_check.ts's `json_schema` profile
// reaches this fragment with a wide exact array under a bounded occurrence.
impl<Inner, T, const MIN: u64, const MAX: u64> RecursiveSchema<super::bounded::BoundedVec<T, MIN, MAX>>
    for Bounded<Inner, MIN, MAX>
where
    Inner: RecursiveSchema<T>,
{
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = self::sequence_schema::<Inner, T>(generator);
        schema.insert("minItems".to_owned(), MIN.into());
        if MAX != u64::MAX {
            schema.insert("maxItems".to_owned(), MAX.into());
        }
        schema
    }
}
