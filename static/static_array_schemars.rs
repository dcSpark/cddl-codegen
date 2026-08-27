// Generic schema adapter for `[T; N]`, matching the serde static-array handover.
pub fn static_array_schema<T: schemars::JsonSchema, const N: usize>(
    generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let mut schema = generator.subschema_for::<alloc::vec::Vec<T>>();
    schema.insert("minItems".to_owned(), (N as u64).into());
    schema.insert("maxItems".to_owned(), (N as u64).into());
    schema
}

/// Optional direct `[T; N]`: serde writes `None` as JSON `null`, so the schema must retain the
/// exact-array branch and add a null branch rather than making the field look array-only.
pub fn static_array_opt_schema<T: schemars::JsonSchema, const N: usize>(
    generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let array = self::static_array_schema::<T, N>(generator);
    schemars::json_schema!({
        "anyOf": [array.to_value(), { "type": "null" }],
    })
}

/// Schema for `Vec<[T; N]>`: the outer occurrence is unconstrained, while every inner array has
/// the exact native cardinality.
pub fn static_array_seq_schema<T: schemars::JsonSchema, const N: usize>(
    generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let item = self::static_array_schema::<T, N>(generator);
    let mut schema = generator.subschema_for::<alloc::vec::Vec<T>>();
    schema.insert("items".to_owned(), item.into());
    schema
}

/// Recursive counterpart to the direct exact-array schema helpers above. Its descriptor tree is
/// the same one the serde half walks, so nested native arrays never make schemars request
/// `JsonSchema` for `[T; N]` while every exact layer owns its cardinality bounds.
pub trait RecursiveSchema<T> {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema;
}

impl<T: schemars::JsonSchema> RecursiveSchema<T> for Leaf {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        generator.subschema_for::<T>()
    }
}

impl<Inner, T, const N: usize> RecursiveSchema<[T; N]> for Exact<Inner, N>
where
    Inner: RecursiveSchema<T>,
{
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let item = Inner::schema(generator);
        let mut schema = schemars::json_schema!({ "type": "array", "items": item.to_value() });
        schema.insert("minItems".to_owned(), (N as u64).into());
        schema.insert("maxItems".to_owned(), (N as u64).into());
        schema
    }
}

fn sequence_schema<Inner, T>(generator: &mut schemars::SchemaGenerator) -> schemars::Schema
where
    Inner: RecursiveSchema<T>,
{
    let item = Inner::schema(generator);
    schemars::json_schema!({ "type": "array", "items": item.to_value() })
}

impl<Inner, T> RecursiveSchema<alloc::vec::Vec<T>> for Loose<Inner>
where
    Inner: RecursiveSchema<T>,
{
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        self::sequence_schema::<Inner, T>(generator)
    }
}

impl<Inner, T> RecursiveSchema<Option<T>> for Optional<Inner>
where
    Inner: RecursiveSchema<T>,
{
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let value = Inner::schema(generator);
        schemars::json_schema!({
            "anyOf": [value.to_value(), { "type": "null" }],
        })
    }
}

pub fn recursive_schema<Shape, T>(generator: &mut schemars::SchemaGenerator) -> schemars::Schema
where
    Shape: RecursiveSchema<T>,
{
    Shape::schema(generator)
}
