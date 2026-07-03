// float JSON coverage (see input.cddl for why this is not part of tests/json/): serde round-trip
// plus the same serde-vs-schema agreement checks tests/json/tests.rs runs for its types.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn float_holder() {
        let holder = FloatHolder::new(1.5);
        let json = serde_json::to_string_pretty(&holder).unwrap();
        let from_json: FloatHolder = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string_pretty(&from_json).unwrap());
        assert_eq!(from_json.f, 1.5);
    }

    #[test]
    fn schema_validates_serialization() {
        let schema = serde_json::to_value(schemars::schema_for!(FloatHolder)).unwrap();
        let validator = jsonschema::validator_for(&schema).unwrap();
        let value = serde_json::to_value(FloatHolder::new(1.5)).unwrap();
        let errors: Vec<String> = validator.iter_errors(&value).map(|e| e.to_string()).collect();
        assert!(
            errors.is_empty(),
            "FloatHolder: {value} does not validate against its own emitted schema: {errors:?}"
        );
        // negative half: an over-permissive schema must still reject a wrong-shaped field
        assert!(!validator.is_valid(&serde_json::json!({"f": "nope"})));
    }
}
