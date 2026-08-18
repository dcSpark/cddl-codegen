// Open-array occurrence-segment value-level end-to-end vectors. A final occurrence or a
// major-disjoint middle occurrence captures elements in its loose, min-one, or bounded carrier;
// loose `@ignore` drops final trailing elements.
// These pin the plain-mode (non-preserve) semantics:
//   * empty tail ≡ closed-struct bytes (adding a rest tail is backward compatible on the wire);
//   * definite AND indefinite arrays with extra elements (incl. a nested-container element);
//   * typing enforced on a typed tail (a wrong-type trailing element errors);
//   * an optional member + a type-distinct tail (present/absent × empty/non-empty);
//   * leading/middle major-disjoint segments, including bounded greedy stop-before-suffix;
//   * `@ignore` re-serializes the declared prefix only;
//   * stream position: an open array as a member of an outer array — the tail loop stops at the open
//     array's end and leaves the sibling for the outer decoder (the cip36 skip-arm bug class).
// Every `wire` byte string is hand-written from the CBOR grammar, not copied from generator output.
#[cfg(test)]
mod open_array {
    use super::*;
    use crate::generated::bounded::BoundedVec;
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

    // --- finite/min-only/exact-zero bounded tails ---

    #[test]
    fn finite_bounded_tail_uses_one_checked_carrier_at_constructor_and_decode() {
        let rest = BoundedVec::try_from(vec![vec![0xaa], vec![0xbb]])
            .expect("two elements are inside the 2..=3 tail window");
        let b = Bounded::new(7, rest);
        assert_eq!(b.rest.as_slice(), &[vec![0xaa], vec![0xbb]]);
        assert_eq!(b.to_cbor_bytes(), bytes("83 07 41aa 41bb"));

        for wire in [bytes("82 07 41aa"), bytes("9f 07 41aa ff")] {
            assert_decode_reject_reason::<Bounded>(&wire, "1 not in range 2 - 3");
        }
        for wire in [
            bytes("85 07 41aa 41bb 41cc 41dd"),
            bytes("9f 07 41aa 41bb 41cc 41dd ff"),
        ] {
            assert_decode_reject_reason::<Bounded>(&wire, "4 not in range 2 - 3");
        }
        let definite = Bounded::from_cbor_bytes(&bytes("83 07 41aa 41bb")).unwrap();
        let indefinite = Bounded::from_cbor_bytes(&bytes("9f 07 41aa 41bb ff")).unwrap();
        assert_eq!(definite.rest.as_slice(), indefinite.rest.as_slice());
        assert_eq!(definite.to_cbor_bytes(), bytes("83 07 41aa 41bb"));
    }

    #[test]
    fn min_only_and_exact_zero_bounded_tails_keep_their_windows() {
        let min = BoundedMin::new(7, BoundedVec::try_from(vec![2, 3]).unwrap());
        assert_eq!(min.rest.as_slice(), &[2, 3]);
        assert_decode_reject_reason::<BoundedMin>(&bytes("81 07"), "0 not at least 2");

        let zero = BoundedZero::new(7, BoundedVec::try_from(Vec::<u64>::new()).unwrap());
        assert_eq!(zero.to_cbor_bytes(), bytes("81 07"));
        assert_decode_reject_reason::<BoundedZero>(&bytes("82 07 01"), "1 not in range 0 - 0");
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

    // --- major-disjoint leading/middle segments ---

    #[test]
    fn leading_and_middle_loose_segments_leave_the_text_suffix_in_place() {
        let leading_empty = Lead::from_cbor_bytes(&bytes("81 6178")).unwrap();
        assert!(leading_empty.rest.is_empty());
        assert_eq!(leading_empty.index_1, "x");
        assert_eq!(leading_empty.to_cbor_bytes(), bytes("81 6178"));

        for (wire, expected_rest) in [
            (bytes("82 07 6178"), vec![]),
            (bytes("84 07 41aa 41bb 6178"), vec![vec![0xaa], vec![0xbb]]),
            (
                bytes("9f 07 41aa 41bb 6178 ff"),
                vec![vec![0xaa], vec![0xbb]],
            ),
        ] {
            let middle = Middle::from_cbor_bytes(&wire).unwrap();
            assert_eq!(middle.index_0, 7);
            assert_eq!(middle.index_2, "x");
            assert_eq!(middle.rest, expected_rest);
            let reparsed = Middle::from_cbor_bytes(&middle.to_cbor_bytes()).unwrap();
            assert_eq!(reparsed.rest, middle.rest);
            assert_eq!(reparsed.index_2, "x");
        }
    }

    #[test]
    fn middle_restricted_segments_use_the_shared_checked_carrier_door() {
        let required = MiddleRequired::from_cbor_bytes(&bytes("83 07 41aa 6178")).unwrap();
        assert_eq!(required.rest.as_slice(), &[vec![0xaa]]);
        assert_eq!(required.index_2, "x");
        assert_decode_reject_reason::<MiddleRequired>(&bytes("82 07 6178"), "0 not at least 1");

        let bounded = MiddleBounded::from_cbor_bytes(&bytes("84 07 41aa 41bb 6178")).unwrap();
        assert_eq!(bounded.rest.as_slice(), &[vec![0xaa], vec![0xbb]]);
        assert_eq!(bounded.index_2, "x");
        for wire in [bytes("83 07 41aa 6178"), bytes("9f 07 41aa 6178 ff")] {
            assert_decode_reject_reason::<MiddleBounded>(&wire, "1 not in range 2 - 3");
        }
        for wire in [
            bytes("86 07 41aa 41bb 41cc 41dd 6178"),
            bytes("9f 07 41aa 41bb 41cc 41dd 6178 ff"),
        ] {
            assert_decode_reject_reason::<MiddleBounded>(
                &wire,
                "expected `Text' byte received `Bytes'",
            );
        }

        let zero = MiddleZero::from_cbor_bytes(&bytes("82 07 6178")).unwrap();
        assert!(zero.rest.as_slice().is_empty());
        assert_eq!(zero.index_2, "x");
    }

    #[test]
    fn middle_maximum_stops_before_its_major_disjoint_suffix() {
        // The maximum is two; after the second bytes item the loop MUST stop and leave the text
        // for the suffix rather than reading/rejecting it as another occurrence.
        let max = MiddleMax::from_cbor_bytes(&bytes("84 07 41aa 41bb 6178")).unwrap();
        assert_eq!(max.rest.as_slice(), &[vec![0xaa], vec![0xbb]]);
        assert_eq!(max.index_2, "x");
        assert_eq!(max.to_cbor_bytes(), bytes("84 07 41aa 41bb 6178"));
    }

    #[test]
    fn middle_segment_rejects_wrong_item_and_trailing_extra() {
        // A uint neither belongs to the bytes segment nor satisfies the text suffix.  An extra
        // post-suffix item remains visible to the owner's final length check.
        assert_decode_reject_reason::<Middle>(
            &bytes("83 07 02 6178"),
            "expected `Text' byte received `UnsignedInteger'",
        );
        assert_decode_reject_reason::<Middle>(
            &bytes("84 07 41aa 6178 00"),
            "Definite length mismatch: found 4, expected: 3",
        );
    }

    #[test]
    fn multiple_exact_segments_own_adjacent_same_major_boundaries() {
        let wire = bytes("88 07 41aa 41bb 6178 01 02 03 09");
        let decoded = ExactSegments::from_cbor_bytes(&wire).unwrap();
        assert_eq!(decoded.prefix, 7);
        assert_eq!(decoded.chunks.as_slice(), &[vec![0xaa], vec![0xbb]]);
        assert_eq!(decoded.separator, "x");
        assert!(decoded.absent.as_slice().is_empty());
        assert_eq!(decoded.values.as_slice(), &[1, 2, 3]);
        assert_eq!(decoded.suffix, 9);
        assert_eq!(decoded.to_cbor_bytes(), wire);

        let indefinite = bytes("9f 07 41aa 41bb 6178 01 02 03 09 ff");
        let decoded_indefinite = ExactSegments::from_cbor_bytes(&indefinite).unwrap();
        assert_eq!(decoded_indefinite.chunks.as_slice(), decoded.chunks.as_slice());
        assert_eq!(decoded_indefinite.values.as_slice(), decoded.values.as_slice());

        let constructed = ExactSegments::new(
            7,
            BoundedVec::try_from(vec![vec![0xaa], vec![0xbb]]).unwrap(),
            "x".to_owned(),
            BoundedVec::try_from(Vec::<u64>::new()).unwrap(),
            BoundedVec::try_from(vec![1, 2, 3]).unwrap(),
            9,
        );
        assert_eq!(constructed.to_cbor_bytes(), wire);

        // A third bytes item belongs to the fixed text separator position, not `chunks`; the
        // decoder must reject there instead of greedily taking it as residue.  Conversely an item
        // at exact-zero's position belongs to `values`, proving the zero segment consumes none.
        assert_decode_reject_reason::<ExactSegments>(
            &bytes("89 07 41aa 41bb 41cc 6178 01 02 03 09"),
            "expected `Text' byte received `Bytes'",
        );
        assert_decode_reject_reason::<ExactSegments>(
            &bytes("89 07 41aa 41bb 6178 00 01 02 03 09"),
            "Definite length mismatch",
        );
    }

    #[test]
    fn trailing_exact_segments_delimit_every_boundary_by_count() {
        let wire = bytes("85 01 02 03 04 05");
        let decoded = TrailingExactSegments::from_cbor_bytes(&wire).unwrap();
        assert_eq!(decoded.first.as_slice(), &[1, 2]);
        assert_eq!(decoded.second.as_slice(), &[3, 4, 5]);
        assert_eq!(decoded.to_cbor_bytes(), wire);

        let constructed = TrailingExactSegments::new(
            BoundedVec::try_from(vec![1, 2]).unwrap(),
            BoundedVec::try_from(vec![3, 4, 5]).unwrap(),
        );
        assert_eq!(constructed.to_cbor_bytes(), wire);

        let indefinite = TrailingExactSegments::from_cbor_bytes(&bytes("9f 01 02 03 04 05 ff"))
            .expect("an indefinite owner still delimits the final exact segment");
        assert_eq!(indefinite.first.as_slice(), &[1, 2]);
        assert_eq!(indefinite.second.as_slice(), &[3, 4, 5]);
    }

    // --- fixed-domain same-major middle segments ---

    #[test]
    fn fixed_domain_middle_greedily_retries_only_the_repeat_and_leaves_suffix() {
        // Both repeat (`0 / 1`) and suffix (`2 / 3`) are uint-major, but their complete
        // generator-owned fixed domains are disjoint. The repeat decoder therefore gets one
        // attempt per candidate; its failure on 2 restores the cursor for the suffix.
        for wire in [bytes("82 6170 02"), bytes("84 6170 00 01 02")] {
            let decoded = FixedMiddle::from_cbor_bytes(&wire).unwrap();
            assert_eq!(decoded.prefix, "p");
            assert!(decoded.rest.len() <= 2);
            assert_eq!(decoded.to_cbor_bytes(), wire);
        }
        // Plain mode retains value identity but emits a definite owner for an indefinite source.
        let indefinite = FixedMiddle::from_cbor_bytes(&bytes("9f 6170 00 01 03 ff")).unwrap();
        assert_eq!(indefinite.rest.len(), 2);
        assert_eq!(indefinite.to_cbor_bytes(), bytes("84 6170 00 01 03"));

        // The aliases on both sides carry the same proof, and native construction serializes to
        // bytes the retry loop reads back without changing the shared loose carrier.
        let mut native = FixedMiddle::new("p".to_owned(), Suffix::I2);
        native.rest.extend([Repeat::I0, Repeat::I1]);
        let wire = bytes("84 6170 00 01 02");
        assert_eq!(native.to_cbor_bytes(), wire);
        let reparsed = FixedMiddle::from_cbor_bytes(&native.to_cbor_bytes()).unwrap();
        assert_eq!(reparsed.rest.len(), 2);

        // The retry cursor is generated from the segment name and must not shadow a legal
        // `@name initial_position` carrier (or the fixed member matching its preferred spelling).
        let named_position = FixedMiddleNamedPosition::from_cbor_bytes(&bytes("83 6170 00 02"))
            .expect("the named segment must compile and decode through the retry loop");
        assert_eq!(named_position.initial_position, vec![Repeat::I0]);
        let derived_name_collision = FixedMiddleCursorNameCollision::from_cbor_bytes(&bytes(
            "83 6170 01 03",
        ))
        .expect("the derived cursor name must avoid fixed record members too");
        assert_eq!(derived_name_collision.initial_position, vec![Repeat::I1]);

        // The ordinary suffix read owns wrong/missing suffix failures after the retry restores
        // the candidate cursor; no successful repeat decode is ever reconsidered.
        assert_decode_reject_reason::<FixedMiddle>(
            &bytes("83 6170 00 04"),
            "No variant matched",
        );
        assert_decode_reject_reason::<FixedMiddle>(&bytes("82 6170 00"), "No variant matched");
    }

    #[test]
    fn fixed_domain_middle_windows_and_simple_values_keep_their_existing_carriers() {
        let required = FixedMiddleRequired::from_cbor_bytes(&bytes("83 6170 00 02")).unwrap();
        assert_eq!(required.rest.as_slice(), &[Repeat::I0]);
        assert_decode_reject_reason::<FixedMiddleRequired>(&bytes("82 6170 02"), "0 not at least 1");

        let bounded = FixedMiddleBounded::from_cbor_bytes(&bytes("84 6170 00 01 02")).unwrap();
        assert_eq!(bounded.rest.as_slice(), &[Repeat::I0, Repeat::I1]);
        // At the maximum, a third repeated-domain value is deliberately left for the suffix, which
        // rejects it. This proves the maximum window still guards before a retry attempt.
        assert_decode_reject_reason::<FixedMiddleBounded>(
            &bytes("85 6170 00 01 00 02"),
            "No variant matched",
        );

        let literal = FixedLiteralSuffix::from_cbor_bytes(&bytes("83 00 01 02")).unwrap();
        assert_eq!(literal.rest, vec![Repeat::I0, Repeat::I1]);
        assert_eq!(literal.to_cbor_bytes(), bytes("83 00 01 02"));

        // This exercises the non-uint fixed domain classifier: bool repeats stop before a null
        // suffix, despite both living in CBOR major 7.
        let simple = FixedSimpleMiddle::from_cbor_bytes(&bytes("83 f4 f5 f6")).unwrap();
        assert_eq!(simple.rest.len(), 2);
        assert_eq!(simple.to_cbor_bytes(), bytes("83 f4 f5 f6"));
    }

    #[test]
    fn fixed_domain_middle_preserves_nested_stream_position() {
        // [["p", 0, 2], 99] — after its failed attempt on the suffix, the inner record consumes
        // only its own array and leaves the outer sibling exactly where the outer decoder expects.
        let outer = OuterFixedMiddle::from_cbor_bytes(&bytes("82 83 6170 00 02 1863")).unwrap();
        assert_eq!(outer.fixed_middle.rest, vec![Repeat::I0]);
        assert_eq!(outer.index_1, 99);
        assert_eq!(outer.to_cbor_bytes(), bytes("82 83 6170 00 02 1863"));
    }

    // --- declared custom-head middle segments ---

    #[test]
    fn declared_repeated_major_greedily_reads_custom_bytes_and_leaves_text_suffix() {
        // `middle_elem` replaces uint but its codec writes a one-byte bytes item. The declaration
        // makes the loop peek bytes (not uint), including through `middle_elem_alias`.
        for (wire, expected_rest) in [
            (bytes("82 07 6178"), vec![]),
            (bytes("84 07 41aa 41bb 6178"), vec![0xaa, 0xbb]),
            (bytes("9f 07 41aa 41bb 6178 ff"), vec![0xaa, 0xbb]),
        ] {
            let decoded = MiddleDeclared::from_cbor_bytes(&wire).unwrap();
            assert_eq!(decoded.index_0, 7);
            assert_eq!(decoded.rest, expected_rest);
            assert_eq!(decoded.index_2, "x");
            // The generated writer's bytes must take the same declared-major read path.
            let reparsed = MiddleDeclared::from_cbor_bytes(&decoded.to_cbor_bytes()).unwrap();
            assert_eq!(reparsed.rest, decoded.rest);
            assert_eq!(reparsed.index_2, "x");
        }

        let mut native = MiddleDeclared::new(7, "x".to_owned());
        native.rest.extend([0xaa, 0xbb]);
        assert_eq!(native.to_cbor_bytes(), bytes("84 07 41aa 41bb 6178"));

        // A uint is neither a declared bytes repeat nor the text suffix; missing/wrong suffixes
        // must remain visible to the ordinary suffix read instead of being guessed or backtracked.
        assert_decode_reject_reason::<MiddleDeclared>(
            &bytes("83 07 08 6178"),
            "expected `Text' byte received `UnsignedInteger'",
        );
        assert_decode_reject_reason::<MiddleDeclared>(
            &bytes("81 07"),
            "Definite length mismatch",
        );
        assert_decode_reject_reason::<MiddleDeclared>(
            &bytes("83 07 41aa 08"),
            "expected `Text' byte received `UnsignedInteger'",
        );
    }

    #[test]
    fn declared_suffix_major_proves_boundary_but_its_codec_reads_the_suffix() {
        // The repeated side is built-in bytes. `middle_suffix` replaces uint but writes text, so
        // only the declaration proves disjointness; after the loop its normal custom reader owns
        // the `s99` text item.
        for wire in [bytes("82 07 63733939"), bytes("83 07 41aa 63733939")] {
            let decoded = MiddleDeclaredSuffix::from_cbor_bytes(&wire).unwrap();
            assert_eq!(decoded.index_0, 7);
            assert_eq!(decoded.middle_suffix, 99);
            assert_eq!(decoded.to_cbor_bytes(), wire);
        }
    }

    #[test]
    fn declared_middle_keeps_the_outer_sibling_at_its_stream_position() {
        // `outer_declared_middle = [middle_declared, uint]`: the inner greedy loop stops at its
        // declared-major-disjoint text suffix AND the owner boundary, leaving the outer uint.
        let wire = bytes("82 83 07 41aa 6178 09");
        let outer = OuterDeclaredMiddle::from_cbor_bytes(&wire).unwrap();
        assert_eq!(outer.middle_declared.rest, vec![0xaa]);
        assert_eq!(outer.middle_declared.index_2, "x");
        assert_eq!(outer.index_1, 9);
        assert_eq!(outer.to_cbor_bytes(), wire);
    }

    // --- exact same-major middle segment (`exact_middle = [uint, 2*2 uint, uint]`) ---

    #[test]
    fn exact_middle_is_count_delimited_and_uses_the_checked_carrier_door() {
        let native = ExactMiddle::new(7, 99, BoundedVec::try_from(vec![2, 3]).unwrap());
        assert_eq!(native.rest.as_slice(), &[2, 3]);
        assert_eq!(native.to_cbor_bytes(), bytes("84 07 02 03 1863"));

        for wire in [bytes("84 07 02 03 1863"), bytes("9f 07 02 03 1863 ff")] {
            let exact = ExactMiddle::from_cbor_bytes(&wire).unwrap();
            assert_eq!(exact.rest.as_slice(), &[2, 3]);
            assert_eq!(exact.index_2, 99, "the third uint is the suffix, not a repetition");
        }

        // An indefinite owner cannot reserve an arity slot for the suffix. With only one intended
        // repeated value, its same-major intended suffix is consumed as the second exact value and
        // the subsequent suffix read rejects at the break.
        assert_decode_reject_reason::<ExactMiddle>(
            &bytes("9f 07 02 1863 ff"),
            "expected `UnsignedInteger' byte received `Special'",
        );

        assert_decode_reject_reason::<ExactMiddle>(&bytes("83 07 02 1863"), "1 not in range 2 - 2");
        assert_decode_reject_reason::<ExactMiddle>(
            &bytes("84 07 02 6178 1863"),
            "expected `UnsignedInteger' byte received `Text'",
        );
        assert_decode_reject_reason::<ExactMiddle>(
            &bytes("85 07 02 03 1863 00"),
            "Definite length mismatch: found 5, expected: 4",
        );
    }

    #[test]
    fn exact_zero_middle_leaves_the_same_major_suffix_in_place_for_both_owner_forms() {
        let native = ExactZeroMiddle::new(7, 99, BoundedVec::try_from(Vec::<u64>::new()).unwrap());
        assert_eq!(native.to_cbor_bytes(), bytes("82 07 1863"));
        for wire in [bytes("82 07 1863"), bytes("9f 07 1863 ff")] {
            let exact = ExactZeroMiddle::from_cbor_bytes(&wire).unwrap();
            assert!(exact.rest.as_slice().is_empty());
            assert_eq!(exact.index_2, 99);
        }
        assert_decode_reject_reason::<ExactZeroMiddle>(
            &bytes("83 07 01 1863"),
            "Definite length mismatch: found 3, expected: 2",
        );
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
    fn required_tail_stream_position_leaves_the_outer_sibling_after_definite_and_indefinite_inner()
    {
        // [[1, 2, 3], 99] — `inner_required` owns 2 and 3 as its non-empty tail; 99 remains for
        // the enclosing record after BOTH the definite and indefinite inner-array boundaries.
        for wire in [
            bytes("82 83 01 02 03 1863"),
            bytes("82 9f 01 02 03 ff 1863"),
        ] {
            let outer = OuterRequired::from_cbor_bytes(&wire).unwrap();
            assert_eq!(outer.inner_required.index_0, 1);
            assert_eq!(outer.inner_required.rest.as_slice(), &[2, 3]);
            assert_eq!(outer.index_1, 99);
        }
    }

    #[test]
    fn bounded_tail_stream_position_leaves_the_outer_sibling_after_definite_and_indefinite_inner() {
        // outer_bounded = [inner_bounded, uint], inner_bounded = [uint, 2*3 uint]. The inner
        // tail consumes exactly 2 and 3; the enclosing sibling 99 must remain unread for both
        // definite and indefinite inner arrays.
        for wire in [
            bytes("82 83 01 02 03 1863"),
            bytes("82 9f 01 02 03 ff 1863"),
        ] {
            let outer = OuterBounded::from_cbor_bytes(&wire).unwrap();
            assert_eq!(outer.inner_bounded.rest.as_slice(), &[2, 3]);
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

    #[test]
    fn middle_segment_stream_position_leaves_the_outer_sibling() {
        // [[1, h'aa', "x"], 99] — the inner middle segment stops at its text suffix, then its
        // own array boundary; the outer sibling must still deserialize as 99.
        for wire in [
            bytes("82 83 01 41aa 6178 1863"),
            bytes("82 9f 01 41aa 6178 ff 1863"),
        ] {
            let outer = OuterMiddle::from_cbor_bytes(&wire).unwrap();
            assert_eq!(outer.inner_middle.rest, vec![vec![0xaa]]);
            assert_eq!(outer.inner_middle.index_2, "x");
            assert_eq!(outer.index_1, 99);
        }
    }

    #[test]
    fn exact_middle_stream_position_leaves_the_outer_sibling() {
        // [[1, 2, 3, 4], 99] — the exact count takes 2 and 3, the same-major suffix is 4,
        // and the enclosing 99 remains unread after both definite and indefinite inner arrays.
        for wire in [
            bytes("82 84 01 02 03 04 1863"),
            bytes("82 9f 01 02 03 04 ff 1863"),
        ] {
            let outer = OuterExactMiddle::from_cbor_bytes(&wire).unwrap();
            assert_eq!(outer.inner_exact_middle.rest.as_slice(), &[2, 3]);
            assert_eq!(outer.inner_exact_middle.index_2, 4);
            assert_eq!(outer.index_1, 99);
        }
    }
}
