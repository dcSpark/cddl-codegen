// Golden known-answer vectors for --preserve-encodings. Every byte string below is hand-derived
// from RFC 8949 §3 (integer/length/tag argument forms, indefinite-length items), NOT copied from
// generator output and NOT built with the tests/deser_test cbor_event helpers. That second
// independence is the point of this suite: the preserve/canonical fixtures construct their
// expected bytes with the same cbor_event write_*_sz primitives the generated code encodes with,
// so a symmetric bug in that shared layer corrupts expected and actual identically and
// round-trips green. Raw spec-anchored hex is the oracle that catches it.
//
// Each vector asserts TWO things over an IRREGULAR (but valid) encoding:
//   * value anchor — a decoded field equals the value hand-read from the input bytes. Identity
//     alone can't catch an exactly-compensating decode+encode bug (both halves wrong the same
//     way); the anchor ties the decode half to the spec independently.
//   * preserve identity — bytes -> T -> bytes is byte-identical, i.e. the generated code must
//     preserve non-minimal header arguments, indefinite-length framing, chunked strings, and
//     map key order.
//
// AUTHORING CONVENTION: write byte strings as two-digit `0x??` literals only (no decimal, no
// single-digit hex). cddl-matrix/project_golden_hex.ts validates every byte array in a
// kat_preserve! invocation (complete 0x?? tokenization + exactly one well-formed CBOR item) and
// hard-fails on any other spelling. Run it (and commit the regenerated
// tests/golden_hex/COVERAGE.md) after changing this file.
#[cfg(test)]
mod golden_hex_preserve {
    use super::*;
    use serialization::Deserialize;

    macro_rules! kat_preserve {
        ($name:ident, $t:ty, $bytes:expr, $anchor:expr) => {
            #[test]
            fn $name() {
                let spec: &[u8] = $bytes;
                let decoded = <$t>::from_cbor_bytes(spec).unwrap();
                // value anchor: pin the decode half to the hand-read spec value
                ($anchor)(&decoded);
                assert_eq!(
                    decoded.to_cbor_bytes(),
                    spec,
                    "preserve KAT mismatch (left = re-encoded, right = RFC 8949 input)"
                );
            }
        };
    }

    // ---- RFC 8949 §3 argument forms on unsigned integers (major type 0) ----
    // Minimal input must stay minimal (baseline; also §A: 1 -> 0x01, 2 -> 0x02).
    kat_preserve!(pair_minimal, Pair, &[0x82, 0x01, 0x02], |d: &Pair| {
        assert_eq!((d.a, d.b), (1, 2));
    });
    // 23 in the 1-byte argument form (0x18 0x17; minimal would be 0x17) next to a minimal 24
    // (0x18 0x18, the smallest value that *needs* the 1-byte argument).
    kat_preserve!(
        pair_arg_1byte,
        Pair,
        &[0x82, 0x18, 0x17, 0x18, 0x18],
        |d: &Pair| {
            assert_eq!((d.a, d.b), (23, 24));
        }
    );
    // 100 in the 2-byte form (minimal is 0x18 0x64) next to 1000, whose minimal form IS 2-byte
    // (§A: 0x19 0x03 0xe8).
    kat_preserve!(
        pair_arg_2byte,
        Pair,
        &[0x82, 0x19, 0x00, 0x64, 0x19, 0x03, 0xe8],
        |d: &Pair| {
            assert_eq!((d.a, d.b), (100, 1000));
        }
    );
    // 0 in the 4-byte form, 1 in the 8-byte form — maximally-padded arguments.
    kat_preserve!(
        pair_arg_wide,
        Pair,
        &[
            0x82, 0x1a, 0x00, 0x00, 0x00, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x01
        ],
        |d: &Pair| {
            assert_eq!((d.a, d.b), (0, 1));
        }
    );

    // ---- RFC 8949 §3 argument forms on negative integers (major type 1) ----
    // The wire value v encodes ARGUMENT -1-v (§3), and the generated struct STORES that argument
    // as u64 (serialize emits write_negative_integer_sz(-(n+1))), so the anchors assert the
    // argument: wire -24 -> n == 23. -24 is the largest-magnitude nint with an immediate head
    // (0x37 = 0x20|23); -25 is the smallest that needs the 1-byte argument (0x38 0x18) —
    // the major-type-1 mirror of the 23/24 boundary.
    kat_preserve!(npair_minimal, Npair, &[0x82, 0x37, 0x38, 0x18], |d: &Npair| {
        assert_eq!((d.n, d.m), (23, 24)); // wire -24, -25
    });
    // -24 padded to the 1-byte argument form (0x38 0x17; minimal is 0x37) next to a minimal -25.
    kat_preserve!(
        npair_arg_1byte,
        Npair,
        &[0x82, 0x38, 0x17, 0x38, 0x18],
        |d: &Npair| {
            assert_eq!((d.n, d.m), (23, 24)); // wire -24, -25
        }
    );
    // -24 in the 2-byte form next to -1000, whose minimal form IS 2-byte (§A: -1000 -> 0x39 0x03
    // 0xe7, argument 999).
    kat_preserve!(
        npair_arg_2byte,
        Npair,
        &[0x82, 0x39, 0x00, 0x17, 0x39, 0x03, 0xe7],
        |d: &Npair| {
            assert_eq!((d.n, d.m), (23, 999)); // wire -24, -1000
        }
    );
    // -24 in the 4-byte form, -100 in the 8-byte form (§A minimal: -100 -> 0x38 0x63) —
    // maximally-padded major-type-1 arguments.
    kat_preserve!(
        npair_arg_wide,
        Npair,
        &[
            0x82, 0x3a, 0x00, 0x00, 0x00, 0x17, 0x3b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x63
        ],
        |d: &Npair| {
            assert_eq!((d.n, d.m), (23, 99)); // wire -24, -100
        }
    );

    // ---- array framing (major type 4): indefinite + non-minimal count ----
    kat_preserve!(pair_indef_array, Pair, &[0x9f, 0x01, 0x02, 0xff], |d: &Pair| {
        assert_eq!((d.a, d.b), (1, 2));
    });
    // element count 2 in a 1-byte argument (0x98 0x02; minimal is 0x82).
    kat_preserve!(
        pair_array_len_1byte,
        Pair,
        &[0x98, 0x02, 0x01, 0x02],
        |d: &Pair| {
            assert_eq!((d.a, d.b), (1, 2));
        }
    );

    // ---- strings (major types 2 + 3): length arguments + indefinite chunks ----
    // Baseline minimal: "IETF" = 0x64 0x49 0x45 0x54 0x46 (§A), h'0102' = 0x42 0x01 0x02.
    kat_preserve!(
        strs_minimal,
        Strs,
        &[0x82, 0x64, 0x49, 0x45, 0x54, 0x46, 0x42, 0x01, 0x02],
        |d: &Strs| {
            assert_eq!(d.s, "IETF");
            assert_eq!(d.v, vec![0x01, 0x02]);
        }
    );
    // "IETF" with its length in a 1-byte argument (0x78 0x04), h'01' with a 2-byte argument
    // (0x59 0x00 0x01).
    kat_preserve!(
        strs_len_args,
        Strs,
        &[0x82, 0x78, 0x04, 0x49, 0x45, 0x54, 0x46, 0x59, 0x00, 0x01, 0x01],
        |d: &Strs| {
            assert_eq!(d.s, "IETF");
            assert_eq!(d.v, vec![0x01]);
        }
    );
    // RFC 8949 §3.2.3's own examples: (_ "strea", "ming") = 0x7f657374726561646d696e67ff and
    // (_ h'0102', h'030405') = 0x5f42010243030405ff — chunk boundaries are data under preserve.
    kat_preserve!(
        strs_indef_chunks,
        Strs,
        &[
            0x82, 0x7f, 0x65, 0x73, 0x74, 0x72, 0x65, 0x61, 0x64, 0x6d, 0x69, 0x6e, 0x67, 0xff,
            0x5f, 0x42, 0x01, 0x02, 0x43, 0x03, 0x04, 0x05, 0xff
        ],
        |d: &Strs| {
            assert_eq!(d.s, "streaming");
            assert_eq!(d.v, vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        }
    );
    // Zero-length chunks inside an indefinite string are legal and must be preserved:
    // s = (_ "", "hi"), v = (_ h'', h'ff').
    kat_preserve!(
        strs_indef_empty_chunk,
        Strs,
        &[0x82, 0x7f, 0x60, 0x62, 0x68, 0x69, 0xff, 0x5f, 0x40, 0x41, 0xff, 0xff],
        |d: &Strs| {
            assert_eq!(d.s, "hi");
            assert_eq!(d.v, vec![0xff]);
        }
    );

    // ---- struct map (major type 5) ----
    // {"a": 1, "b": 2} minimal, declaration order.
    kat_preserve!(
        kv_minimal,
        Kv,
        &[0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02],
        |d: &Kv| {
            assert_eq!((d.a, d.b), (1, 2));
        }
    );
    // indefinite-map framing (0xbf … 0xff).
    kat_preserve!(
        kv_indef_map,
        Kv,
        &[0xbf, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02, 0xff],
        |d: &Kv| {
            assert_eq!((d.a, d.b), (1, 2));
        }
    );
    // key ORDER is data under preserve: input with b before a must re-encode b before a.
    kat_preserve!(
        kv_key_order_swapped,
        Kv,
        &[0xa2, 0x61, 0x62, 0x02, 0x61, 0x61, 0x01],
        |d: &Kv| {
            assert_eq!((d.a, d.b), (1, 2));
        }
    );
    // entry count in a 1-byte argument (0xb8 0x02) + non-minimal VALUE arguments
    // (1 as 0x18 0x01, 2 as 0x19 0x00 0x02).
    kat_preserve!(
        kv_len_and_value_args,
        Kv,
        &[0xb8, 0x02, 0x61, 0x61, 0x18, 0x01, 0x61, 0x62, 0x19, 0x00, 0x02],
        |d: &Kv| {
            assert_eq!((d.a, d.b), (1, 2));
        }
    );
    // text KEY length in a 1-byte argument ("a" as 0x78 0x01 0x61).
    kat_preserve!(
        kv_key_len_arg,
        Kv,
        &[0xa2, 0x78, 0x01, 0x61, 0x01, 0x61, 0x62, 0x02],
        |d: &Kv| {
            assert_eq!((d.a, d.b), (1, 2));
        }
    );

    // ---- table (`{ * uint => text }`, wrapped in a 1-element record) ----
    // Key ORDER and per-key header arguments are data under preserve: an indefinite map holding
    // 256 => "a" (minimal 3-byte key head) then 10 => "b" in the NON-minimal 1-byte-argument form
    // (0x18 0x0a; minimal is 0x0a) must re-encode byte-identically — order, framing, and the
    // padded key head all preserved.
    kat_preserve!(
        table_key_order_and_args,
        TableHolder,
        &[0x81, 0xbf, 0x19, 0x01, 0x00, 0x61, 0x61, 0x18, 0x0a, 0x61, 0x62, 0xff],
        |d: &TableHolder| {
            assert_eq!(d.t.len(), 2);
            assert_eq!(d.t.get(&256u64).map(|s| s.as_str()), Some("a"));
            assert_eq!(d.t.get(&10u64).map(|s| s.as_str()), Some("b"));
        }
    );

    // ---- homogeneous array (the `*` occurrence), wrapped in a 1-element record ----
    kat_preserve!(
        seq_indef,
        SeqHolder,
        &[0x81, 0x9f, 0x01, 0x02, 0x03, 0xff],
        |d: &SeqHolder| {
            assert_eq!(d.xs, vec![1, 2, 3]);
        }
    );
    // element count in a 1-byte argument + a non-minimal element (1 as 0x18 0x01).
    kat_preserve!(
        seq_len_and_elem_args,
        SeqHolder,
        &[0x81, 0x98, 0x03, 0x18, 0x01, 0x02, 0x03],
        |d: &SeqHolder| {
            assert_eq!(d.xs, vec![1, 2, 3]);
        }
    );

    // ---- tag head (major type 6) argument forms ----
    // tag 11 minimal = 0xcb (§3: tag number in the head's argument).
    kat_preserve!(tagged_minimal, TaggedOne, &[0xcb, 0x81, 0x05], |d: &TaggedOne| {
        assert_eq!(d.x, 5);
    });
    // tag 11 in a 1-byte argument (0xd8 0x0b).
    kat_preserve!(
        tagged_tag_arg_1byte,
        TaggedOne,
        &[0xd8, 0x0b, 0x81, 0x05],
        |d: &TaggedOne| {
            assert_eq!(d.x, 5);
        }
    );
    // tag 11 in a 2-byte argument, indefinite body, non-minimal element (5 as 0x18 0x05).
    kat_preserve!(
        tagged_tag_arg_2byte_indef,
        TaggedOne,
        &[0xd9, 0x00, 0x0b, 0x9f, 0x18, 0x05, 0xff],
        |d: &TaggedOne| {
            assert_eq!(d.x, 5);
        }
    );

    // ---- indefinite container of major-type-7 (bool) elements/keys ----
    // The deserialize-loop break-check peeks `cbor_type()` and only reads the 0xff break when the
    // next item is genuinely a break (`special_break`, non-consuming) — a bool element (0xf4/0xf5)
    // shares major type 7 with the break but must fall through to `bool::deserialize`. Hand-derived
    // from RFC 8949 §3: false = 0xf4, true = 0xf5, indefinite array 0x9f … 0xff.
    // Outer holder is a 1-element array (0x81) wrapping the inner indefinite `[_ false, true]`.
    kat_preserve!(
        bool_seq_indef,
        BoolSeqHolder,
        &[0x81, 0x9f, 0xf4, 0xf5, 0xff],
        |d: &BoolSeqHolder| {
            assert_eq!(d.bs, vec![false, true]);
        }
    );
    // indefinite MAP with bool KEYS: the map break-check peeks the key type, so a bool key
    // (0xf4/0xf5) must reach the key deserializer, not be read as the 0xff break.
    // 1-element holder (0x81) wrapping bf { false: "a", true: "b" } ff.
    kat_preserve!(
        bool_table_indef,
        BoolTableHolder,
        &[0x81, 0xbf, 0xf4, 0x61, 0x61, 0xf5, 0x61, 0x62, 0xff],
        |d: &BoolTableHolder| {
            assert_eq!(d.bt.len(), 2);
            assert_eq!(d.bt.get(&false).map(|s| s.as_str()), Some("a"));
            assert_eq!(d.bt.get(&true).map(|s| s.as_str()), Some("b"));
        }
    );

    // ---- tag-258 set idiom: tag PRESENCE + non-minimal tag SIZE are both encoding data ----
    // Both arms denote the same set of one text element "a"; which arm (and at what header size) is
    // preserved byte-exact. Outer holder is a 1-element array (0x81) wrapping the set 0x81 0x61 0x61.
    // untagged arm.
    kat_preserve!(
        opt_set_untagged,
        OptSetHolder,
        &[0x81, 0x81, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(d.s, vec!["a".to_string()]);
        }
    );
    // tagged arm, minimal tag head (258 = 0xd9 0x01 0x02, the smallest form for a 2-byte argument).
    kat_preserve!(
        opt_set_tagged_minimal,
        OptSetHolder,
        &[0x81, 0xd9, 0x01, 0x02, 0x81, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(d.s, vec!["a".to_string()]);
        }
    );
    // tagged arm, NON-minimal 8-byte tag head (0xdb + 0x0000000000000102): the wide header is
    // preserved verbatim under --preserve-encodings, not silently minimized.
    kat_preserve!(
        opt_set_tagged_wide,
        OptSetHolder,
        &[0x81, 0xdb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x02, 0x81, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(d.s, vec!["a".to_string()]);
        }
    );

    // ---- DUPLICATE-carrying set instances: the preserve-mode default's load-bearing guarantee ----
    // A tag-258 set idiom accepts duplicate wire entries and, under --preserve-encodings, MUST
    // re-emit them byte-exact including their order — the multi-era byte-exact-reader contract that
    // makes `preserve` the correct set default (see input.cddl). The value anchor asserts the
    // duplicate is actually PRESENT (len == 2, both elements "a"), so the KAT can't pass by a silent
    // dedup that happened to re-encode to the same bytes; the macro's byte-identity assert then pins
    // the round-trip. Both wire arms (untagged / tagged) of both occurrence flavors (`[*]` / `[+]`).
    //
    // `[*]` flavor (OptSetHolder, s: Vec<String>): untagged `[["a","a"]]`.
    kat_preserve!(
        opt_set_untagged_duplicate,
        OptSetHolder,
        &[0x81, 0x82, 0x61, 0x61, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(d.s.len(), 2);
            assert_eq!(d.s, vec!["a".to_string(), "a".to_string()]);
        }
    );
    // `[*]` flavor, tagged arm `[258(["a","a"])]` (258 = 0xd9 0x01 0x02).
    kat_preserve!(
        opt_set_tagged_duplicate,
        OptSetHolder,
        &[0x81, 0xd9, 0x01, 0x02, 0x82, 0x61, 0x61, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(d.s.len(), 2);
            assert_eq!(d.s, vec!["a".to_string(), "a".to_string()]);
        }
    );
    // `[+]` flavor (OptNesetHolder, s: NonEmptyVec<String>): untagged `[["a","a"]]`. The
    // NonEmptyVec door admits duplicates today (Vec-backed, no uniqueness check) — this pins that.
    kat_preserve!(
        opt_neset_untagged_duplicate,
        OptNesetHolder,
        &[0x81, 0x82, 0x61, 0x61, 0x61, 0x61],
        |d: &OptNesetHolder| {
            assert_eq!(d.s.len(), 2);
            assert_eq!(d.s.get(0).map(|x| x.as_str()), Some("a"));
            assert_eq!(d.s.get(1).map(|x| x.as_str()), Some("a"));
        }
    );
    // `[+]` flavor, tagged arm `[258(["a","a"])]`.
    kat_preserve!(
        opt_neset_tagged_duplicate,
        OptNesetHolder,
        &[0x81, 0xd9, 0x01, 0x02, 0x82, 0x61, 0x61, 0x61, 0x61],
        |d: &OptNesetHolder| {
            assert_eq!(d.s.len(), 2);
            assert_eq!(d.s.get(0).map(|x| x.as_str()), Some("a"));
            assert_eq!(d.s.get(1).map(|x| x.as_str()), Some("a"));
        }
    );

    // ---- `@duplicates reject` set: accept-path byte identity (the twin is order-preserving) ----
    // A duplicate-FREE instance of a reject-mode set (`reject_set`, s: OrderedSet<String>) must
    // re-emit byte-exact under --preserve-encodings, including a non-minimal tag header — `reject`
    // narrows the accepted set, it never sorts or reshapes the accepted bytes. Two distinct elements
    // "a","b" so the vector is genuinely unique (not vacuously single). Untagged and tagged arms.
    // (The wire-DUPLICATE reject case is pinned in-process by duplicates_reject_set_wire_and_api.)
    kat_preserve!(
        reject_set_untagged,
        RejectSetHolder,
        &[0x81, 0x82, 0x61, 0x61, 0x61, 0x62],
        |d: &RejectSetHolder| {
            assert_eq!(d.s.len(), 2);
            assert_eq!(d.s.get(0).map(|x| x.as_str()), Some("a"));
            assert_eq!(d.s.get(1).map(|x| x.as_str()), Some("b"));
        }
    );
    // tagged arm, NON-minimal 8-byte tag head — preserved verbatim (like opt_set_tagged_wide).
    kat_preserve!(
        reject_set_tagged_wide,
        RejectSetHolder,
        &[0x81, 0xdb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x02, 0x82, 0x61, 0x61, 0x61, 0x62],
        |d: &RejectSetHolder| {
            assert_eq!(d.s.len(), 2);
            assert_eq!(d.s.get(0).map(|x| x.as_str()), Some("a"));
            assert_eq!(d.s.get(1).map(|x| x.as_str()), Some("b"));
        }
    );

    // The `@duplicates reject` reject PATH: a duplicate on the WIRE and a duplicate built through
    // the public API report the IDENTICAL error, because the generated deserialize routes the
    // collected Vec through the SAME `OrderedSet::try_from` door the API uses — they can never drift.
    // A duplicate is REFUSED (not silently deduped), and the error names the duplicate's INDEX.
    #[test]
    fn reject_set_duplicate_wire_and_api_identical() {
        // wire: untagged holder [["a","a"]] — a duplicate element in the reject set
        let dup_wire: &[u8] = &[0x81, 0x82, 0x61, 0x61, 0x61, 0x61];
        let wire_err = RejectSetHolder::from_cbor_bytes(dup_wire)
            .expect_err("a duplicate set element on the wire must be rejected");
        // API: the same duplicate built through the public uniqueness door
        let api_err = crate::generated::ordered_set::OrderedSet::<String>::try_from(vec![
            "a".to_string(),
            "a".to_string(),
        ])
        .expect_err("a duplicate built through the API must be rejected");
        // Both route through the SAME OrderedSet door, so both name the identical `DuplicateKey`
        // with the identical INDEX (1 — the second "a"). The wire path additionally wraps the failure
        // in a field-context prefix (`… in RejectSetHolder.s because:`), which is the only difference —
        // the door-level payload is byte-identical.
        assert!(
            wire_err.to_string().contains("Duplicate key: 1"),
            "wire error should name the duplicate key and its index: {wire_err}"
        );
        assert!(
            api_err.to_string().contains("Duplicate key: 1"),
            "API error should name the duplicate key and its index: {api_err}"
        );

        // checked push: adding an already-present element is refused, the set is unchanged.
        let mut set =
            crate::generated::ordered_set::OrderedSet::<String>::try_from(vec!["a".to_string()])
                .unwrap();
        set.push("b".to_string())
            .expect("a new element is accepted");
        assert!(
            set.push("a".to_string()).is_err(),
            "an already-present element is refused"
        );
        assert_eq!(set.len(), 2, "the refused push left the set unchanged");
    }
}
