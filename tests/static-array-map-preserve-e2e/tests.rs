#[cfg(test)]
mod static_array_map_preserve {
    use super::*;

    #[test]
    fn ordered_map_values_use_the_recursive_static_array_adapter() {
        // `collect` lets inference select the generated OrderedHashMap backend instead of baking
        // the default BTreeMap into a preserve-encodings execution test.
        let holder = WideMapHolder::new([(1, [7; 64])].into_iter().collect());
        let json = serde_json::to_string(&holder).unwrap();
        assert!(json.contains(r#""value":{"1":[7,7"#), "ordered map JSON: {json}");
        let back: WideMapHolder = serde_json::from_str(&json).unwrap();
        assert_eq!(back.value.get(&1).unwrap()[63], 7);

        let schema = serde_json::to_value(schemars::schema_for!(WideMapHolder)).unwrap();
        let value = &schema["properties"]["value"]["patternProperties"]["^\\d+$"];
        assert_eq!(value["minItems"], 64);
        assert_eq!(value["maxItems"], 64);
    }
}
