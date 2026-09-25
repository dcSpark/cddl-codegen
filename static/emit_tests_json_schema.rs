    mod cddl_json_schema {
        use serde::de::DeserializeOwned;

        fn push_unique(out: &mut Vec<serde_json::Value>, value: serde_json::Value) {
            if !out.contains(&value) {
                out.push(value);
            }
        }

        fn shapes() -> [serde_json::Value; 6] {
            [
                serde_json::Value::Null,
                serde_json::Value::Bool(false),
                serde_json::Value::Number(serde_json::Number::from(0)),
                serde_json::Value::String(String::new()),
                serde_json::Value::Array(Vec::new()),
                serde_json::Value::Object(serde_json::Map::new()),
            ]
        }

        // Deterministic, bounded top-level and one-level mutations. The deserialize oracle below
        // means a schema may intentionally accept a broad JSON form; only a candidate Rust rejects
        // becomes a required schema rejection. Eight immediate children cap a wide object/array at
        // 70 candidates (six top-level shapes + eight removals + eight times six replacements +
        // eight deterministic duplicate-array candidates). The direct-array duplicate is within
        // that bound too.
        fn mutations(value: &serde_json::Value) -> Vec<serde_json::Value> {
            let mut out = Vec::new();
            for shape in shapes() {
                push_unique(&mut out, shape);
            }
            match value {
                serde_json::Value::Array(values) => {
                    if let Some(first) = values.first() {
                        let mut duplicated = values.clone();
                        duplicated.push(first.clone());
                        push_unique(&mut out, serde_json::Value::Array(duplicated));
                    }
                    for index in 0..values.len().min(8) {
                        let mut removed = values.clone();
                        removed.remove(index);
                        push_unique(&mut out, serde_json::Value::Array(removed));
                        for shape in shapes() {
                            if shape != values[index] {
                                let mut changed = values.clone();
                                changed[index] = shape;
                                push_unique(&mut out, serde_json::Value::Array(changed));
                            }
                        }
                    }
                }
                serde_json::Value::Object(values) => {
                    for key in values.keys().take(8).cloned().collect::<Vec<_>>() {
                        let mut removed = values.clone();
                        removed.remove(&key);
                        push_unique(&mut out, serde_json::Value::Object(removed));
                        let original = &values[&key];
                        if let serde_json::Value::Array(array) = original {
                            if let Some(first) = array.first() {
                                let mut duplicated = array.clone();
                                duplicated.push(first.clone());
                                let mut changed = values.clone();
                                changed.insert(key.clone(), serde_json::Value::Array(duplicated));
                                push_unique(&mut out, serde_json::Value::Object(changed));
                            }
                        }
                        for shape in shapes() {
                            if shape != *original {
                                let mut changed = values.clone();
                                changed.insert(key.clone(), shape);
                                push_unique(&mut out, serde_json::Value::Object(changed));
                            }
                        }
                    }
                }
                _ => {}
            }
            out
        }

        #[allow(dead_code)]
        pub fn assert_case<T>(value: serde_json::Value, rust_type: &str, case: &str)
        where
            T: schemars::JsonSchema + DeserializeOwned,
        {
            check::<T>(value, rust_type, case, None);
        }

        // A dynamic map row's occurrence minimum is deliberately absent from the schema:
        // `minProperties` would also count declared members. Only a candidate that drops dynamic
        // members of the actual serialization is exempt from schema/deserializer agreement.
        #[allow(dead_code)]
        pub fn assert_case_unpublished_row_minimum<T>(value: serde_json::Value, rust_type: &str, case: &str, declared: &[&str])
        where
            T: schemars::JsonSchema + DeserializeOwned,
        {
            check::<T>(value, rust_type, case, Some(declared));
        }

        fn drops_only_dynamic_members(value: &serde_json::Value, candidate: &serde_json::Value, declared: &[&str]) -> bool {
            let (serde_json::Value::Object(original), serde_json::Value::Object(candidate)) = (value, candidate) else {
                return false;
            };
            candidate.len() < original.len()
                && candidate.iter().all(|(key, member)| original.get(key) == Some(member))
                && original.keys().filter(|key| !candidate.contains_key(*key)).all(|key| !declared.contains(&key.as_str()))
        }

        fn check<T>(value: serde_json::Value, rust_type: &str, case: &str, declared: Option<&[&str]>)
        where
            T: schemars::JsonSchema + DeserializeOwned,
        {
            let schema = serde_json::to_value(schemars::schema_for!(T))
                .unwrap_or_else(|error| panic!("{rust_type} ({case}): could not serialize schemars::schema_for! output: {error}"));
            let validator = jsonschema::validator_for(&schema)
                .unwrap_or_else(|error| panic!("{rust_type} ({case}): schema validator could not compile the generated schema: {error}"));
            assert!(validator.is_valid(&value), "{rust_type} ({case}): schema rejected a real serialization: {value}");
            for candidate in mutations(&value) {
                if declared.is_some_and(|declared| drops_only_dynamic_members(&value, &candidate, declared)) {
                    continue;
                }
                if serde_json::from_value::<T>(candidate.clone()).is_err() {
                    assert!(!validator.is_valid(&candidate), "{rust_type} ({case}): schema accepted a shape the JSON deserializer rejects: {candidate}");
                }
            }
        }
    }
