// The emitted JSON schema must track the same checked scalar carrier as serde deserialization.
// This deliberately does not use the generic emitted-schema mutation harness: that harness mints
// shape mutations, not every numeric boundary, and text byte length cannot be exact JSON Schema.
#[cfg(test)]
mod tests {
    use super::*;

    fn schema<T: schemars::JsonSchema>() -> serde_json::Value {
        serde_json::to_value(schemars::schema_for!(T)).unwrap()
    }

    fn validator<T: schemars::JsonSchema>() -> jsonschema::Validator {
        jsonschema::validator_for(&schema::<T>()).unwrap()
    }

    #[test]
    fn integer_windows_and_exclusions_use_the_effective_carrier_bounds() {
        let int_ne_schema = schema::<IntNe>();
        assert_eq!(int_ne_schema["not"]["const"], 1, "{int_ne_schema}");
        let int_ne = validator::<IntNe>();
        assert!(!int_ne.is_valid(&serde_json::json!(1)));
        assert!(int_ne.is_valid(&serde_json::json!(2)));
        assert!(serde_json::from_value::<IntNe>(serde_json::json!(1)).is_err());
        serde_json::from_value::<IntNe>(serde_json::json!(2)).unwrap();

        // An exact integer window projects to `const`, the dual of the exclusion's `not: { const
        // ... }` shape above.
        let exact_schema = schema::<IntExact>();
        assert_eq!(exact_schema["const"], 7, "{exact_schema}");
        let exact = validator::<IntExact>();
        assert!(exact.is_valid(&serde_json::json!(7)));
        assert!(!exact.is_valid(&serde_json::json!(6)));
        serde_json::from_value::<IntExact>(serde_json::json!(7)).unwrap();
        assert!(serde_json::from_value::<IntExact>(serde_json::json!(6)).is_err());

        // Keep a negative literal in the compiled fixture: this is specifically what catches the
        // generated `(-10i64).into()` parenthesization rather than accidentally emitting an
        // unconstrained `-10i64.into()` conversion.
        let negative_schema = schema::<NegativeMin>();
        assert_eq!(negative_schema["minimum"], -10, "{negative_schema}");
        let negative = validator::<NegativeMin>();
        assert!(negative.is_valid(&serde_json::json!(-10)));
        assert!(!negative.is_valid(&serde_json::json!(-11)));
        serde_json::from_value::<NegativeMin>(serde_json::json!(-10)).unwrap();
        assert!(serde_json::from_value::<NegativeMin>(serde_json::json!(-11)).is_err());

        // `3...10` normalizes to the inclusive JSON interval [3, 9] during parsing.
        let uint_schema = schema::<UintExclusive>();
        assert_eq!(uint_schema["minimum"], 3, "{uint_schema}");
        assert_eq!(uint_schema["maximum"], 9, "{uint_schema}");
        let uint = validator::<UintExclusive>();
        assert!(!uint.is_valid(&serde_json::json!(2)));
        assert!(uint.is_valid(&serde_json::json!(3)));
        assert!(!uint.is_valid(&serde_json::json!(10)));

        // nint's JSON serde carrier is its stored magnitude m = -value - 1: nint .ge -5
        // therefore accepts magnitudes m <= 4, and nint .ne -5 excludes m == 4.
        let nint_ge_schema = schema::<NintGe>();
        assert_eq!(nint_ge_schema["maximum"], 4, "{nint_ge_schema}");
        let nint_ge = validator::<NintGe>();
        assert!(nint_ge.is_valid(&serde_json::json!(4)));
        assert!(!nint_ge.is_valid(&serde_json::json!(5)));
        serde_json::from_value::<NintGe>(serde_json::json!(4)).unwrap();
        assert!(serde_json::from_value::<NintGe>(serde_json::json!(5)).is_err());

        let nint_ne_schema = schema::<NintNe>();
        assert_eq!(nint_ne_schema["not"]["const"], 4, "{nint_ne_schema}");
        let nint_ne = validator::<NintNe>();
        assert!(!nint_ne.is_valid(&serde_json::json!(4)));
        assert!(nint_ne.is_valid(&serde_json::json!(3)));
        assert!(serde_json::from_value::<NintNe>(serde_json::json!(4)).is_err());
        serde_json::from_value::<NintNe>(serde_json::json!(3)).unwrap();

        // Boundary case: CDDL -1 is stored as magnitude 0. Mapping the `.ne -1` sentinel's two
        // signed endpoints independently used to collapse them both onto magnitude 1, advertising
        // and enforcing `const: 1` instead of excluding 0.
        let boundary_schema = schema::<NintNeMinusOne>();
        assert_eq!(boundary_schema["not"]["const"], 0, "{boundary_schema}");
        let boundary = validator::<NintNeMinusOne>();
        assert!(!boundary.is_valid(&serde_json::json!(0)));
        assert!(boundary.is_valid(&serde_json::json!(1)));
        assert!(serde_json::from_value::<NintNeMinusOne>(serde_json::json!(0)).is_err());
        serde_json::from_value::<NintNeMinusOne>(serde_json::json!(1)).unwrap();
    }

    #[test]
    fn text_byte_size_schema_is_sound_but_deliberately_broad() {
        let schema = schema::<SizedText>();
        // The runtime bounds are bytes [2, 4]; JSON Schema counts characters, so the sound lower
        // bound is ceil(2 / 4) = 1 rather than a false `minLength: 2` claim.
        assert_eq!(schema["minLength"], 1, "{schema}");
        assert_eq!(schema["maxLength"], 4, "{schema}");
        let validator = validator::<SizedText>();
        assert!(!validator.is_valid(&serde_json::json!("")));
        assert!(validator.is_valid(&serde_json::json!("é")));
        serde_json::from_value::<SizedText>(serde_json::json!("é")).unwrap();

        // One ASCII character is necessarily a false positive of this portable conservative
        // projection: schema accepts it, while the byte-counting deserializer refuses it.
        assert!(validator.is_valid(&serde_json::json!("a")));
        assert!(serde_json::from_value::<SizedText>(serde_json::json!("a")).is_err());
    }

    #[test]
    fn float_window_uses_2020_12_numeric_exclusive_keywords() {
        let schema = schema::<FloatWindow>();
        assert_eq!(schema["minimum"], 1.5, "{schema}");
        assert_eq!(schema["exclusiveMaximum"], 4.5, "{schema}");
        let validator = validator::<FloatWindow>();
        assert!(validator.is_valid(&serde_json::json!(1.5)));
        assert!(validator.is_valid(&serde_json::json!(4.49)));
        assert!(!validator.is_valid(&serde_json::json!(4.5)));
        serde_json::from_value::<FloatWindow>(serde_json::json!(1.5)).unwrap();
        assert!(serde_json::from_value::<FloatWindow>(serde_json::json!(4.5)).is_err());
    }
}
