//! Only --json-serde-derives=true, selected by the host registry.

use component_host::load;
use serde_json::{json, Value};
use wasmtime::Result;

#[test]
fn json_input_and_output_preserve_nested_values() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let node = api.node();
    let input = json!({"label": "json-parent", "children": [{"label": "json-child"}]});
    // Ordinary Option<Vec<Node>> derives serialize the absent field as null.
    let expected = json!({"label": "json-parent", "children": [{"label": "json-child", "children": null}]});
    let handle = node
        .call_from_json(&mut *store, &input.to_string())?
        .expect("the independently authored node JSON must decode");
    assert_eq!(node.call_label(&mut *store, handle)?, "json-parent");
    let kids = node
        .call_children(&mut *store, handle)?
        .expect("the authored JSON has one child");
    assert_eq!(kids.len(), 1);
    assert_eq!(node.call_label(&mut *store, kids[0])?, "json-child");
    assert_eq!(node.call_children(&mut *store, kids[0])?, None);
    let output = node
        .call_to_json(&mut *store, handle)?
        .expect("this string-only tree must serialize to JSON");
    let observed: Value = serde_json::from_str(&output).expect("to-json must return valid JSON");
    assert_eq!(observed, expected);

    // A constructor-originated value gives to-json an oracle independent of from-json.
    let constructed = node.call_constructor(&mut *store, "constructed")?;
    let output = node
        .call_to_json(&mut *store, constructed)?
        .expect("a freshly constructed node must serialize");
    let observed: Value = serde_json::from_str(&output).expect("to-json must return valid JSON");
    assert_eq!(observed, json!({"label": "constructed", "children": null}));
    Ok(())
}

#[test]
fn invalid_json_returns_errors_and_preserves_instance_usability() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let node = api.node();
    let retained = node.call_constructor(&mut *store, "retained")?;
    for bad in ["{", r#"{"label":42}"#] {
        let error = node
            .call_from_json(&mut *store, bad)?
            .expect_err("syntax and field-type errors must be inner Err, never traps");
        assert!(!error.is_empty());
        assert_eq!(node.call_label(&mut *store, retained)?, "retained");
        let valid = node
            .call_from_json(&mut *store, r#"{"label":"after-error"}"#)?
            .expect("a valid call must succeed after every rejection");
        assert_eq!(node.call_label(&mut *store, valid)?, "after-error");
    }
    Ok(())
}
