// Open table (`t = { * K_t => V_t, * K_r => V_r }`) CBOR fidelity vectors. These pin the core the
// shape adds over an open struct-map: dispatch by WIRE MAJOR TYPE (the typed row claims exactly its
// key's single statically-known major, the catch-all sees the complement), the TAGGED
// `orig_deser_order` slot encoding that lets TWO dynamic sequences share one order vector, and the
// canonical key merge across both regions. Every `wire` byte string is hand-written from the CBOR
// grammar, not copied from generator output.
#[cfg(test)]
mod open_table {
    use super::*;
    use serialization::{Deserialize, Serialize};

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    // {1: "a", h'aabbccdd': {"name": "x"}, "z": 2} — a captured uint entry, then the TYPED bytes
    // entry, then a captured text entry. The typed entry sits BETWEEN two captured ones, so a
    // byte-exact round trip is only possible if the order vector records which SEQUENCE each slot
    // came from (an untagged `base + i` scheme cannot: the second sequence's base is a runtime
    // length that is not final while the loop is still pushing).
    const INTERLEAVED: &str = "a3 01 6161 44aabbccdd a1646e616d656178 617a 02";

    #[test]
    fn major_dispatch_partitions_the_two_rows() {
        let v = Labels::from_cbor_bytes(&bytes(INTERLEAVED)).unwrap();
        assert_eq!(v.entries.len(), 1, "the typed row claims major 2 only");
        assert_eq!(v.rest.len(), 2, "the catch-all sees the complement");
    }

    #[test]
    fn interleave_byte_exact() {
        let wire = bytes(INTERLEAVED);
        let v = Labels::from_cbor_bytes(&wire).unwrap();
        assert_eq!(
            v.to_cbor_bytes(),
            wire,
            "byte-exact round trip across the tagged two-sequence order encoding"
        );
    }

    #[test]
    fn canonical_merges_both_regions_into_one_sort() {
        let v = Labels::from_cbor_bytes(&bytes(INTERLEAVED)).unwrap();
        // Length-first over the ENCODED key bytes, spanning both containers: 0x01 (1 byte),
        // 0x617a (2), 0x44aabbccdd (5). The typed entry sorts LAST despite being minted first.
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            bytes("a3 01 6161 617a 02 44aabbccdd a1646e616d656178"),
            "canonical sorts typed and captured keys together by encoded bytes"
        );
    }

    #[test]
    fn canonical_output_re_reads_and_keeps_its_own_order() {
        let canonical = Labels::from_cbor_bytes(&bytes(INTERLEAVED))
            .unwrap()
            .to_canonical_cbor_bytes();
        let reread = Labels::from_cbor_bytes(&canonical).unwrap();
        assert_eq!(reread.entries.len(), 1);
        assert_eq!(reread.rest.len(), 2);
        assert_eq!(
            reread.to_cbor_bytes(),
            canonical,
            "the re-read records the canonical wire order as ITS order"
        );
    }

    #[test]
    fn empty_map_round_trips_both_ways() {
        let v = Labels::from_cbor_bytes(&bytes("a0")).unwrap();
        assert!(v.entries.is_empty() && v.rest.is_empty());
        assert_eq!(v.to_cbor_bytes(), bytes("a0"));
        assert_eq!(Labels::new().to_canonical_cbor_bytes(), bytes("a0"));
    }

    #[test]
    fn indefinite_length_is_preserved() {
        // bf <typed> <captured> ff
        let wire = bytes("bf 44aabbccdd a1646e616d656178 01 6161 ff");
        let v = Labels::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.entries.len(), 1);
        assert_eq!(v.rest.len(), 1);
        assert_eq!(v.to_cbor_bytes(), wire, "indefinite header + break replay");
    }

    #[test]
    fn non_minimal_key_widths_replay_from_the_sidecars() {
        // The captured uint key 1 written as a two-byte head (0x190001), and the typed row's
        // 4-byte key with a one-byte-arg length head (0x5804). Both encodings ride their row's OWN
        // sidecar pair, so the two rows cannot borrow each other's.
        let wire = bytes("a2 5804 aabbccdd a1646e616d656178 190001 6161");
        let v = Labels::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.to_cbor_bytes(), wire, "per-row encoding sidecars replay");
        // canonical rewrites both heads minimally
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            bytes("a2 01 6161 44aabbccdd a1646e616d656178")
        );
    }

    #[test]
    fn duplicate_typed_key_names_its_own_position() {
        let wire = bytes(
            "a2 44aabbccdd a1646e616d656178 44aabbccdd a1646e616d656179",
        );
        let err = Labels::from_cbor_bytes(&wire).unwrap_err();
        assert!(
            format!("{err:?}").contains("<open-table typed key>"),
            "a duplicate in the TYPED row names the typed position, got {err:?}"
        );
    }

    #[test]
    fn duplicate_captured_key_still_rejects() {
        let wire = bytes("a2 01 6161 01 6162");
        assert!(
            Labels::from_cbor_bytes(&wire).is_err(),
            "the catch-all keeps the delivered value-duplicate rejection"
        );
    }

    #[test]
    fn typed_major_but_refused_is_a_hard_error_not_a_capture() {
        // A 3-byte bstr: the typed row CLAIMS major 2, then `.size 4` refuses the value. Dispatch is
        // by major, NOT by success — the catch-all never sees it (and could not take it: `md` admits
        // majors 0 and 3 only). This is the property that makes an open table compose with a
        // backtracking type choice: the failure propagates out instead of being swallowed.
        assert!(Labels::from_cbor_bytes(&bytes("a1 43aabbcc a1646e616d656178")).is_err());
        // positive control, same shape with a conforming key
        let ok = Labels::from_cbor_bytes(&bytes("a1 44aabbccdd a1646e616d656178"))
            .expect("a conforming 4-byte key parses");
        assert_eq!(ok.entries.len(), 1);
    }

    #[test]
    fn a_major_no_row_admits_is_a_hard_error() {
        // A nint key (major 1): the typed row claims major 2, and the catch-all's `md` admits only
        // majors 0 and 3. The entry reaches the catch-all's `K::deserialize`, which refuses.
        assert!(Labels::from_cbor_bytes(&bytes("a1 20 6161")).is_err());
    }

    #[test]
    fn a_text_keyed_typed_row_partitions_against_a_uint_or_text_catch_all() {
        // {"k": 7, 1: "a"} — "k" is major 3 so the TYPED row takes it; 1 is major 0 so the
        // catch-all does. Both rows can read text, and the major alone decides.
        let wire = bytes("a2 616b 07 01 6161");
        let v = TextKeyed::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.entries.len(), 1, "every text key goes to the typed row");
        assert_eq!(v.rest.len(), 1, "the catch-all sees only the uint key");
        assert_eq!(v.to_cbor_bytes(), wire);
    }

    #[test]
    fn duplicates_preserve_on_both_rows_keeps_wire_order() {
        // Two identical typed keys AND two identical captured keys, all retained.
        let wire = bytes("a4 44aabbccdd 01 01 6161 44aabbccdd 02 01 6162");
        let v = DupBoth::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.entries.len(), 2, "the typed PairMap keeps both");
        assert_eq!(v.rest.len(), 2, "the catch-all PairMap keeps both");
        assert_eq!(v.to_cbor_bytes(), wire, "positional sidecars replay in order");
    }

    #[test]
    fn name_directive_renames_each_row_independently() {
        let wire = bytes("a2 44aabbccdd 01 01 6161");
        let v = Named::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.typed.len(), 1);
        assert_eq!(v.captured.len(), 1);
        assert_eq!(v.to_cbor_bytes(), wire);
    }

    #[test]
    fn non_empty_counts_typed_entries_only() {
        // The min-1 is a statement about the TYPED row: an object of purely captured entries is not
        // a non-empty table, so it refuses with the very error `NonEmptyMap`'s `TryFrom` door raises.
        for empty in ["a0", "a1 01 6161", "a2 01 6161 617a 02"] {
            let err = NonEmpty::from_cbor_bytes(&bytes(empty)).unwrap_err();
            assert!(
                format!("{err:?}").contains("RangeCheck"),
                "a table with no TYPED entry must fail the min-1 bound, got {err:?}"
            );
        }
        // one typed entry is enough, captured entries beside it or not
        let wire = bytes("a2 44aabbccdd 01 01 6161");
        let v = NonEmpty::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.entries.len(), 1);
        assert_eq!(v.rest.len(), 1);
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact under the bound");
    }

    #[test]
    fn non_empty_door_takes_the_first_typed_entry() {
        // `new` is the only door, and it cannot build a bound-violating value: the constructed table
        // already holds the entry it was given, and its canonical bytes are that one entry's.
        let v = NonEmpty::new(Pid::new(vec![0xaa, 0xbb, 0xcc, 0xdd]).unwrap(), 1);
        assert_eq!(v.entries.len(), 1);
        assert!(v.rest.is_empty());
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            bytes("a1 44aabbccdd 01"),
            "the seeded entry is the whole map"
        );
        // and what it writes re-reads through the same bound
        assert_eq!(
            NonEmpty::from_cbor_bytes(&v.to_canonical_cbor_bytes())
                .unwrap()
                .entries
                .len(),
            1
        );
    }

    #[test]
    fn non_empty_public_typed_row_cannot_drop_its_last_entry() {
        let key = Pid::new(vec![0xaa, 0xbb, 0xcc, 0xdd]).unwrap();
        let mut v = NonEmpty::new(key.clone(), 1);
        let err = v
            .entries
            .remove(&key)
            .expect_err("the restricted public carrier must refuse removing its last entry");
        assert!(
            format!("{err:?}").contains("RangeCheck"),
            "the public mutation door must retain the shared min-1 payload, got {err:?}"
        );
        assert_eq!(v.entries.len(), 1, "the failed removal leaves the carrier intact");
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            bytes("a1 44aabbccdd 01"),
            "a public mutation attempt cannot produce invalid empty-table CBOR"
        );
    }

    #[test]
    fn non_empty_preserve_uses_the_pair_carrier() {
        let key = Pid::new(vec![0xaa, 0xbb, 0xcc, 0xdd]).unwrap();
        let mut v = NonEmptyPreserve::new(key.clone(), 1);
        v.entries.insert(key.clone(), 2);
        assert_eq!(v.entries.len(), 2, "preserve retains duplicate typed keys");
        // The public restricted pair-map has no removal/clear operation, so the only mutator grows
        // it. The two-entry vector re-emits as a valid duplicate-keyed CBOR map.
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            bytes("a2 44aabbccdd 01 44aabbccdd 02")
        );
    }

    #[test]
    fn bounded_dynamic_rows_refuse_below_and_above_at_each_wire_partition() {
        // Open struct: fixed key 1 plus the rest row. Its window counts ONLY the unknown rest
        // keys, never the declared member that shares the enclosing map.
        for (wire, label) in [
            ("a2 01 00 02 6161", "below"),
            ("a5 01 00 02 6161 03 6162 04 6163 05 6164", "above"),
        ] {
            let err = BoundedRest::from_cbor_bytes(&bytes(wire)).unwrap_err();
            assert!(
                format!("{err:?}").contains("RangeCheck"),
                "the open-struct {label}-window wire must fail its rest carrier: {err:?}"
            );
        }
        let open = BoundedRest::from_cbor_bytes(&bytes("a3 01 00 02 6161 03 6162")).unwrap();
        assert_eq!(open.rest.len(), 2);
        assert_eq!(
            open.to_cbor_bytes(),
            bytes("a3 01 00 02 6161 03 6162"),
            "the in-window open-struct row still round-trips"
        );

        // The typed row counts only bytes-key entries. A catch-all entry beside them neither helps
        // the lower bound nor consumes its finite maximum.
        for (wire, label) in [
            ("a1 01 6161", "below"),
            (
                "a5 44aaaaaaaa 01 44bbbbbbbb 02 44cccccccc 03 44dddddddd 04 01 6161",
                "above",
            ),
        ] {
            let err = BoundedTyped::from_cbor_bytes(&bytes(wire)).unwrap_err();
            assert!(
                format!("{err:?}").contains("RangeCheck"),
                "the typed-row {label}-window wire must fail its own carrier: {err:?}"
            );
        }
        let typed = BoundedTyped::from_cbor_bytes(&bytes(
            "a3 44aaaaaaaa 01 44bbbbbbbb 02 01 6161",
        ))
        .unwrap();
        assert_eq!(typed.entries.len(), 2);
        assert_eq!(typed.rest.len(), 1);

        // Conversely, the catch-all row counts only the complement of the bytes major. The one
        // typed entry is deliberately present in every vector so the two partitions cannot leak
        // into one another through the common enclosing map loop.
        for (wire, label) in [
            ("a2 44aaaaaaaa 01 01 6161", "below"),
            (
                "a5 44aaaaaaaa 01 01 6161 02 6162 03 6163 04 6164",
                "above",
            ),
        ] {
            let err = BoundedCatchAll::from_cbor_bytes(&bytes(wire)).unwrap_err();
            assert!(
                format!("{err:?}").contains("RangeCheck"),
                "the catch-all {label}-window wire must fail its own carrier: {err:?}"
            );
        }
        let captured = BoundedCatchAll::from_cbor_bytes(&bytes(
            "a3 44aaaaaaaa 01 01 6161 617a 6162",
        ))
        .unwrap();
        assert_eq!(captured.entries.len(), 1);
        assert_eq!(captured.rest.len(), 2);
    }

    #[test]
    fn bounded_preserve_row_counts_retained_duplicate_pairs() {
        // BoundedPairMap counts PAIRS, not distinct keys: two byte-identical typed keys are two
        // admitted rows; one and four are the below/above boundary failures for 2*3.
        for (wire, label) in [
            ("a1 44aaaaaaaa 01", "below"),
            (
                "a4 44aaaaaaaa 01 44aaaaaaaa 02 44aaaaaaaa 03 44aaaaaaaa 04",
                "above",
            ),
        ] {
            let err = BoundedPreserve::from_cbor_bytes(&bytes(wire)).unwrap_err();
            assert!(
                format!("{err:?}").contains("RangeCheck"),
                "the preserve row's {label}-window pair count must fail: {err:?}"
            );
        }
        let in_window = BoundedPreserve::from_cbor_bytes(&bytes(
            "a2 44aaaaaaaa 01 44aaaaaaaa 02",
        ))
        .unwrap();
        assert_eq!(in_window.entries.len(), 2, "both duplicate pairs are retained");
        assert_eq!(
            in_window.to_cbor_bytes(),
            bytes("a2 44aaaaaaaa 01 44aaaaaaaa 02"),
            "the pair-map replay keeps the duplicate count and order"
        );
    }
}
