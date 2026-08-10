// Open array (loose CBOR "rest tail") value-level end-to-end vectors. A final-position `* t` after
// the fixed members captures the trailing elements into a `pub rest: Vec<T>` (CAPTURE) or drops them
// (`@ignore`). These pin the plain-mode (non-preserve) semantics:
//   * empty tail ≡ closed-struct bytes (adding a rest tail is backward compatible on the wire);
//   * definite AND indefinite arrays with extra elements (incl. a nested-container element);
//   * typing enforced on a typed tail (a wrong-type trailing element errors);
//   * an optional member + a type-distinct tail (present/absent × empty/non-empty);
//   * `@ignore` re-serializes the declared prefix only;
//   * stream position: an open array as a member of an outer array — the tail loop stops at the open
//     array's end and leaves the sibling for the outer decoder (the cip36 skip-arm bug class).
// Every `wire` byte string is hand-written from the CBOR grammar, not copied from generator output.
#[cfg(test)]
mod open_array {
    use super::*;
    use serialization::Deserialize;

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    fn assert_decode_reject_reason<T: Deserialize>(bytes: &[u8], reason: &str) {
        let err = match T::from_cbor_bytes(bytes) {
            Ok(_) => panic!("vector must reject"),
            Err(err) => err,
        };
        assert!(
            err.to_string().contains(reason),
            "expected reason `{reason}`, got `{err}`"
        );
    }

    // --- CAPTURE, typed tail (`cap = [uint, tstr, * uint]`) ---

    #[test]
    fn cap_empty_tail_equals_closed_bytes() {
        // No trailing elements: serialize as the closed `[7, "hi"]` (array of 2).
        let c = Cap::new(7, "hi".to_string());
        assert_eq!(c.rest.len(), 0);
        let closed = bytes("82 07 62 6869");
        assert_eq!(c.to_cbor_bytes(), closed);
        let round = Cap::from_cbor_bytes(&closed).unwrap();
        assert_eq!(round.index_0, 7);
        assert_eq!(round.index_1, "hi");
        assert!(round.rest.is_empty());
    }

    #[test]
    fn cap_definite_tail_round_trips_bytewise() {
        // [7, "hi", 2, 3] — the trailing 2 and 3 land in `rest`.
        let wire = bytes("84 07 62 6869 02 03");
        let c = Cap::from_cbor_bytes(&wire).unwrap();
        assert_eq!(c.index_0, 7);
        assert_eq!(c.index_1, "hi");
        assert_eq!(c.rest, vec![2u64, 3u64]);
        assert_eq!(c.to_cbor_bytes(), wire);
    }

    #[test]
    fn cap_indefinite_tail_value_round_trips() {
        // Indefinite [7, "hi", 2, 3, break]. Plain mode re-emits definite, so assert VALUE identity.
        let wire = bytes("9f 07 62 6869 02 03 ff");
        let c = Cap::from_cbor_bytes(&wire).unwrap();
        assert_eq!(c.index_0, 7);
        assert_eq!(c.rest, vec![2u64, 3u64]);
        // Re-serialize is the definite form of the same value.
        let reparsed = Cap::from_cbor_bytes(&c.to_cbor_bytes()).unwrap();
        assert_eq!(reparsed.index_0, 7);
        assert_eq!(reparsed.rest, vec![2u64, 3u64]);
    }

    #[test]
    fn cap_wrong_type_tail_element_errors() {
        // [7, "hi", "x"] — a text trailing element is not a uint, so the typed tail rejects it.
        let wire = bytes("83 07 62 6869 6178");
        assert!(Cap::from_cbor_bytes(&wire).is_err());
    }

    // --- one-or-more typed tail (`required = [uint, + byte_alias]`) ---

    #[test]
    fn required_tail_is_valid_by_construction_and_round_trips() {
        let r = Required::new(7, vec![0xaa]);
        assert_eq!(r.rest.as_slice(), &[vec![0xaa]]);
        assert_eq!(r.to_cbor_bytes(), bytes("82 07 41aa"));
        let multiple = bytes("83 07 41aa 41bb");
        let decoded = Required::from_cbor_bytes(&multiple).unwrap();
        assert_eq!(decoded.rest.as_slice(), &[vec![0xaa], vec![0xbb]]);
        assert_eq!(decoded.to_cbor_bytes(), multiple);
    }

    #[test]
    fn required_tail_empty_definite_and_indefinite_share_the_non_empty_door() {
        assert_decode_reject_reason::<Required>(&bytes("81 07"), "0 not at least 1");
        assert_decode_reject_reason::<Required>(&bytes("9f 07 ff"), "0 not at least 1");
    }

    #[test]
    fn named_required_tail_renames_the_field_and_constructor_argument() {
        let r = NamedRequired::new(7, vec![0xaa]);
        assert_eq!(r.extras.as_slice(), &[vec![0xaa]]);
        assert_eq!(r.to_cbor_bytes(), bytes("82 07 41aa"));
    }

    #[test]
    fn bounded_required_tail_checks_its_first_constructor_element() {
        let valid = BoundedRequired::new(7, 5).expect("the inclusive upper bound is valid");
        assert_eq!(valid.rest.as_slice(), &[5]);
        let err = BoundedRequired::new(7, 6).expect_err("the first tail element exceeds .le 5");
        assert!(err.to_string().contains("6 not at most 5"));
    }

    // --- CAPTURE, `any` tail with a nested-container element (`cap_any = [uint, * any]`) ---

    #[test]
    fn cap_any_nested_container_round_trips_bytewise() {
        // [7, [1, 2], {}] — the tail captures an array element and a map element (nested containers).
        let wire = bytes("83 07 82 0102 a0");
        let c = CapAny::from_cbor_bytes(&wire).unwrap();
        assert_eq!(c.index_0, 7);
        assert_eq!(c.rest.len(), 2);
        assert_eq!(c.rest[0].as_array().map(|a| a.len()), Some(2));
        assert_eq!(c.to_cbor_bytes(), wire);
    }

    // --- optional member + type-distinct tail (`opt = [uint, ? tstr, * uint]`) ---

    #[test]
    fn opt_present_optional_empty_tail() {
        let wire = bytes("82 07 62 6869");
        let o = Opt::from_cbor_bytes(&wire).unwrap();
        assert_eq!(o.index_0, 7);
        assert_eq!(o.index_1.as_deref(), Some("hi"));
        assert!(o.rest.is_empty());
        assert_eq!(o.to_cbor_bytes(), wire);
    }

    #[test]
    fn opt_present_optional_nonempty_tail() {
        let wire = bytes("84 07 62 6869 02 03");
        let o = Opt::from_cbor_bytes(&wire).unwrap();
        assert_eq!(o.index_1.as_deref(), Some("hi"));
        assert_eq!(o.rest, vec![2u64, 3u64]);
        assert_eq!(o.to_cbor_bytes(), wire);
    }

    #[test]
    fn opt_absent_optional_empty_tail() {
        let wire = bytes("81 07");
        let o = Opt::from_cbor_bytes(&wire).unwrap();
        assert_eq!(o.index_0, 7);
        assert!(o.index_1.is_none());
        assert!(o.rest.is_empty());
        assert_eq!(o.to_cbor_bytes(), wire);
    }

    #[test]
    fn opt_absent_optional_nonempty_tail() {
        // [7, 2, 3] — 2 is a uint (not a tstr), so the optional member is absent and the tail is [2, 3].
        let wire = bytes("83 07 02 03");
        let o = Opt::from_cbor_bytes(&wire).unwrap();
        assert_eq!(o.index_0, 7);
        assert!(o.index_1.is_none());
        assert_eq!(o.rest, vec![2u64, 3u64]);
        assert_eq!(o.to_cbor_bytes(), wire);
    }

    // --- `@ignore` tail (`ign = [uint, * any] ; @ignore`) ---

    #[test]
    fn ign_empty_tail_equals_closed_bytes() {
        let closed = bytes("81 07");
        let i = Ign::from_cbor_bytes(&closed).unwrap();
        assert_eq!(i.index_0, 7);
        assert_eq!(i.to_cbor_bytes(), closed);
    }

    #[test]
    fn ign_definite_tail_dropped() {
        // [7, 1, 2, 3] — the trailing elements are consumed and DROPPED; re-serialize is [7].
        let wire = bytes("84 07 01 02 03");
        let i = Ign::from_cbor_bytes(&wire).unwrap();
        assert_eq!(i.index_0, 7);
        assert_eq!(i.to_cbor_bytes(), bytes("81 07"));
    }

    #[test]
    fn ign_nested_container_tail_dropped() {
        // [7, [1, 2]] — a nested-container trailing element must be fully consumed (stream advances).
        let wire = bytes("82 07 82 0102");
        let i = Ign::from_cbor_bytes(&wire).unwrap();
        assert_eq!(i.index_0, 7);
        assert_eq!(i.to_cbor_bytes(), bytes("81 07"));
    }

    #[test]
    fn ign_indefinite_tail_dropped() {
        // Indefinite [7, 1, 2, break] — dropped; re-serialize is the closed [7].
        let wire = bytes("9f 07 01 02 ff");
        let i = Ign::from_cbor_bytes(&wire).unwrap();
        assert_eq!(i.index_0, 7);
        assert_eq!(i.to_cbor_bytes(), bytes("81 07"));
    }

    // --- stream position: an open array as a member of an outer array (cip36 skip-arm bug class) ---

    #[test]
    fn capture_stream_position_sibling_after_open_array() {
        // outer = [inner, uint], inner = [uint, * uint]. [[1, 2, 3], 99] — inner's tail is [2, 3],
        // and the sibling 99 must decode into outer.index_1 (the tail loop left it on the wire).
        let wire = bytes("82 83 01 02 03 1863");
        let o = Outer::from_cbor_bytes(&wire).unwrap();
        assert_eq!(o.inner.index_0, 1);
        assert_eq!(o.inner.rest, vec![2u64, 3u64]);
        assert_eq!(o.index_1, 99);
        assert_eq!(o.to_cbor_bytes(), wire);
    }

    #[test]
    fn required_tail_stream_position_leaves_the_outer_sibling_after_definite_and_indefinite_inner() {
        // [[1, 2, 3], 99] — `inner_required` owns 2 and 3 as its non-empty tail; 99 remains for
        // the enclosing record after BOTH the definite and indefinite inner-array boundaries.
        for wire in [bytes("82 83 01 02 03 1863"), bytes("82 9f 01 02 03 ff 1863")] {
            let outer = OuterRequired::from_cbor_bytes(&wire).unwrap();
            assert_eq!(outer.inner_required.index_0, 1);
            assert_eq!(outer.inner_required.rest.as_slice(), &[2, 3]);
            assert_eq!(outer.index_1, 99);
        }
    }

    #[test]
    fn ignore_stream_position_sibling_after_open_array() {
        // outer_ign = [inner_ign, uint], inner_ign = [uint, * any] ; @ignore. [[1, 2, 3], 99] —
        // inner_ign drops [2, 3], and the sibling 99 decodes into outer_ign.index_1.
        let wire = bytes("82 83 01 02 03 1863");
        let o = OuterIgn::from_cbor_bytes(&wire).unwrap();
        assert_eq!(o.inner_ign.index_0, 1);
        assert_eq!(o.index_1, 99);
        // Re-serialize drops inner_ign's tail: [[1], 99].
        assert_eq!(o.to_cbor_bytes(), bytes("82 81 01 1863"));
    }
}
