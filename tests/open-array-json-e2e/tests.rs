// Open array (loose CBOR "rest tail") JSON end-to-end vectors. The captured tail renders as an
// ORDINARY JSON array under the field name; to_json on an `any` tail is fallible on data (RFC 8949
// §6.1's injective subset — a non-injective node like a byte string errors, never a silent
// substitute); an empty tail ≡ closed-struct JSON (skip-if-empty on write, default-on-read).
#[cfg(test)]
mod open_array_json {
    use super::*;
    use crate::generated::any_cbor::AnyCbor;

    #[test]
    fn typed_tail_renders_as_array_and_round_trips() {
        // Cap = [uint, tstr, * uint]: the tail is an ordinary JSON array of numbers under `rest`.
        let mut cap = Cap::new(7, "hi".to_string());
        cap.rest = vec![2, 3];
        let json = serde_json::to_string(&cap).unwrap();
        assert!(json.contains("\"rest\":[2,3]"), "typed tail as JSON array: {json}");
        let back: Cap = serde_json::from_str(&json).unwrap();
        assert_eq!(back.index_0, 7);
        assert_eq!(back.index_1, "hi");
        assert_eq!(back.rest, vec![2, 3]);
    }

    #[test]
    fn empty_tail_equals_closed_struct_json() {
        // No trailing elements: the `rest` field is skipped on write (empty tail ≡ closed-struct JSON),
        // and an absent `rest` key defaults to empty on read.
        let cap = Cap::new(7, "hi".to_string());
        let json = serde_json::to_string(&cap).unwrap();
        assert!(!json.contains("rest"), "empty tail omits the field: {json}");
        let back: Cap = serde_json::from_str(r#"{"index_0":7,"index_1":"hi"}"#).unwrap();
        assert!(back.rest.is_empty(), "absent rest defaults to empty");
    }

    #[test]
    fn any_tail_renders_naturally_and_round_trips() {
        // CapAny = [uint, * any]: the `any` elements render NATURALLY (a uint 5 as the JSON number 5,
        // not a tagged `{"uint":5}`), reusing the homogeneous `[* any]` member surface.
        let mut cap = CapAny::new(7);
        cap.rest = vec![AnyCbor::new_uint(5), AnyCbor::new_text("x".to_string())];
        let json = serde_json::to_string(&cap).unwrap();
        assert!(json.contains("\"rest\":[5,\"x\"]"), "any tail natural: {json}");
        let back: CapAny = serde_json::from_str(&json).unwrap();
        assert_eq!(back.rest.len(), 2);
    }

    #[test]
    fn any_tail_non_injective_node_errors_loudly() {
        // A byte string has no injective natural-JSON rendering (RFC 8949 §6.1) -> to_json errors
        // rather than inventing a spelling. Loud, by design.
        let mut cap = CapAny::new(7);
        cap.rest = vec![AnyCbor::new_bytes(vec![1, 2, 3])];
        assert!(
            serde_json::to_string(&cap).is_err(),
            "a bytes tail element must error on to_json (non-injective node)"
        );
    }
}
