// Content-fallthrough vector for the bare `any` type-choice catch-all (loose-CBOR A3 WP1).
// Generated under --preserve-encodings so the round-trip asserts BYTE-exact fidelity through the
// `any` arm (AnyCbor is self-carried — it owns its own encodings), including a non-minimal header.
//
// The rule is `x = uint .le 5 / any`. The typed arm matches on wire TYPE (unsigned) but can fail on
// CONTENT (the `.le 5` bound). Correct CDDL dispatch is forced backtracking: a wire uint > 5 must
// fall THROUGH the typed arm to the `any` arm. The `cbor_type()`-dispatch form would instead route
// the item into the uint arm by its major type and reject it — the bug this vector guards against.
#[cfg(test)]
mod any_choice_content_fallthrough {
    use super::*;
    use serialization::Deserialize;

    #[test]
    fn wire_le5_matches_typed_arm() {
        // 0x05 = uint 5; within `.le 5`, so the typed arm matches.
        let x = X::from_cbor_bytes(&[0x05]).unwrap();
        assert!(
            matches!(x, X::U64 { uint: 5, .. }),
            "uint 5 must match the typed `uint .le 5` arm"
        );
        assert_eq!(x.to_cbor_bytes(), vec![0x05], "typed arm must round-trip");
    }

    #[test]
    fn wire_gt5_falls_through_to_any() {
        // 0x06 = uint 6; matches the typed arm on TYPE but fails its `.le 5` CONTENT bound, so it
        // must fall through to the catch-all `any` arm rather than erroring inside the typed arm.
        let x = X::from_cbor_bytes(&[0x06]).unwrap();
        assert!(
            matches!(x, X::Any(_)),
            "uint 6 exceeds `.le 5` and must fall through to the `any` arm"
        );
        assert_eq!(
            x.to_cbor_bytes(),
            vec![0x06],
            "the `any` arm must round-trip the fallen-through item byte-exact"
        );
    }

    #[test]
    fn any_arm_preserves_non_minimal_encoding() {
        // 0x18 0x06 = uint 6 written with a redundant 1-byte argument (non-minimal per RFC 8949 §3).
        // It fails `.le 5` on content and falls through to `any`; AnyCbor carries its own encoding,
        // so preserve mode must re-emit the exact non-minimal bytes.
        let x = X::from_cbor_bytes(&[0x18, 0x06]).unwrap();
        assert!(matches!(x, X::Any(_)), "non-minimal uint 6 must fall through to `any`");
        assert_eq!(
            x.to_cbor_bytes(),
            vec![0x18, 0x06],
            "the `any` arm must preserve the non-minimal header byte-exact"
        );
    }
}
