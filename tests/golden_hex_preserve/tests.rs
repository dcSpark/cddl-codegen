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
            assert_eq!(*d.s, vec!["a".to_string()]);
        }
    );
    // tagged arm, minimal tag head (258 = 0xd9 0x01 0x02, the smallest form for a 2-byte argument).
    kat_preserve!(
        opt_set_tagged_minimal,
        OptSetHolder,
        &[0x81, 0xd9, 0x01, 0x02, 0x81, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(*d.s, vec!["a".to_string()]);
        }
    );
    // tagged arm, NON-minimal 8-byte tag head (0xdb + 0x0000000000000102): the wide header is
    // preserved verbatim under --preserve-encodings, not silently minimized.
    kat_preserve!(
        opt_set_tagged_wide,
        OptSetHolder,
        &[0x81, 0xdb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x02, 0x81, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(*d.s, vec!["a".to_string()]);
        }
    );

    // ---- DUPLICATE-carrying set instances: the preserve-mode default's load-bearing guarantee ----
    // A tag-258 set idiom under the EXPLICIT `@duplicates preserve` opt-out accepts duplicate wire
    // entries and, under --preserve-encodings, MUST re-emit them byte-exact including their order —
    // the multi-era byte-exact-reader contract that lives on the `preserve` spelling now that the 258
    // default is `reject` (see input.cddl). The value anchor asserts the duplicate is actually PRESENT
    // (len == 2, both elements "a"), so the KAT can't pass by a silent dedup that happened to
    // re-encode to the same bytes; the macro's byte-identity assert then pins the round-trip. Both
    // wire arms (untagged / tagged) of both occurrence flavors (`[*]` / `[+]`).
    //
    // `[*]` flavor (OptSetHolder, s: Vec<String>): untagged `[["a","a"]]`.
    kat_preserve!(
        opt_set_untagged_duplicate,
        OptSetHolder,
        &[0x81, 0x82, 0x61, 0x61, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(d.s.len(), 2);
            assert_eq!(*d.s, vec!["a".to_string(), "a".to_string()]);
        }
    );
    // `[*]` flavor, tagged arm `[258(["a","a"])]` (258 = 0xd9 0x01 0x02).
    kat_preserve!(
        opt_set_tagged_duplicate,
        OptSetHolder,
        &[0x81, 0xd9, 0x01, 0x02, 0x82, 0x61, 0x61, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(d.s.len(), 2);
            assert_eq!(*d.s, vec!["a".to_string(), "a".to_string()]);
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
    // (The wire-DUPLICATE reject case is pinned in-process by reject_set_duplicate_wire_and_api_identical.)
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

    // The std set contract (`insert`/`contains`/`Extend`/`FromIterator`/`sort`), the empty-means-absent
    // `try_opt_from` door (runtime + emitted nominal), and the two twin refinement doors. These are the
    // consumer-facing set operations (union, dedup-normalize, empty-or-set construction) the wrappers
    // ship so a downstream crate never hand-rolls a `let _ = push(…)` loop.
    #[test]
    fn reject_set_std_contract_and_refinement_doors() {
        use crate::generated::ordered_set::{NonEmptyOrderedSet, OrderedSet};

        // insert -> bool (std set contract): true = newly added, false = already present (no-op).
        let mut s = OrderedSet::<String>::new();
        assert!(s.insert("a".to_string()), "a fresh element is newly inserted");
        assert!(!s.insert("a".to_string()), "an already-present element is a no-op");
        assert_eq!(s.len(), 1, "the no-op insert left the set unchanged");
        assert!(s.contains(&"a".to_string()), "contains reports membership");
        assert!(!s.contains(&"z".to_string()));

        // Extend dedups keep-first (a set union): "a" already present is dropped, "b"/"c" added.
        s.extend(vec!["a".to_string(), "b".to_string(), "c".to_string(), "b".to_string()]);
        assert_eq!(
            s.iter().map(String::as_str).collect::<Vec<_>>(),
            vec!["a", "b", "c"],
            "extend is a keep-first union"
        );

        // FromIterator dedups keep-first (IndexSet::from_iter semantics).
        let collected: OrderedSet<u64> = vec![3u64, 1, 3, 2, 1].into_iter().collect();
        assert_eq!(collected.as_slice(), &[3, 1, 2], "collect dedups keep-first, order preserved");

        // sort() preserves uniqueness AND changes the (re-)emitted order.
        let mut to_sort: OrderedSet<u64> = vec![3u64, 1, 2].try_into().unwrap();
        to_sort.sort();
        assert_eq!(to_sort.as_slice(), &[1, 2, 3], "sort reorders in place");
        assert_eq!(to_sort.len(), 3, "sort cannot create a duplicate");

        // try_opt_from: empty -> None, non-empty unique -> Some, duplicate -> Err (NOT swallowed).
        assert!(OrderedSet::<u64>::try_opt_from(vec![]).unwrap().is_none());
        assert_eq!(
            OrderedSet::<u64>::try_opt_from(vec![1, 2]).unwrap().unwrap().len(),
            2
        );
        assert!(
            OrderedSet::<u64>::try_opt_from(vec![1, 1]).is_err(),
            "the duplicate error surfaces through try_opt_from, not silently swallowed"
        );
        // the non-empty twin: empty is None (the min-1 RangeCheck deliberately does NOT fire).
        assert!(NonEmptyOrderedSet::<u64>::try_opt_from(vec![]).unwrap().is_none());
        assert_eq!(
            NonEmptyOrderedSet::<u64>::try_opt_from(vec![7]).unwrap().unwrap().len(),
            1
        );

        // Refinement doors between the twins.
        let os: OrderedSet<u64> = vec![1u64, 2].try_into().unwrap();
        let ne: NonEmptyOrderedSet<u64> = os.try_into().expect("non-empty OrderedSet narrows");
        assert_eq!(ne.len(), 2);
        assert!(
            NonEmptyOrderedSet::<u64>::try_from(OrderedSet::<u64>::new()).is_err(),
            "an empty OrderedSet is refused by the min-1 narrowing door"
        );
        let widened: OrderedSet<u64> = ne.into(); // infallible widening
        assert_eq!(widened.len(), 2);

        // Emitted nominal `try_opt_from` (e2e): RejectSet wraps OrderedSet<String>, so its inherent
        // door re-wraps each accepted set via `new` and yields the nominal.
        assert!(crate::generated::RejectSet::try_opt_from(vec![]).unwrap().is_none());
        let nominal = crate::generated::RejectSet::try_opt_from(vec!["a".to_string(), "b".to_string()])
            .unwrap()
            .expect("a non-empty unique vec builds the nominal");
        assert_eq!(nominal.len(), 2, "the nominal Derefs to its inner set's len");
        assert!(
            crate::generated::RejectSet::try_opt_from(vec!["a".to_string(), "a".to_string()]).is_err(),
            "the nominal door surfaces the duplicate error"
        );
    }

    // ---- `@duplicates preserve` table (PairMap): duplicate-key byte-exact round-trip ----
    // The consensus-critical property for Cardano `transaction_metadata`: a duplicate-keyed map is
    // spec-valid CBOR and MUST re-emit byte-exact (the aux-data hash is over the original bytes). The
    // value anchor asserts the duplicate is genuinely PRESENT (two entries under key 1, values "a"
    // then "b", in wire order) so the KAT can't pass by a silent collapse that happened to re-encode
    // the same; the macro's byte-identity assert then pins the round-trip. Holder is `[m: pmap]`, a
    // 1-element array (0x81) wrapping the 2-entry map (0xa2).
    kat_preserve!(
        pmap_duplicate_key,
        PmapHolder,
        &[0x81, 0xa2, 0x01, 0x61, 0x61, 0x01, 0x61, 0x62],
        |d: &PmapHolder| {
            assert_eq!(d.m.len(), 2);
            // get() is the FIRST match; get_all() every match in entry order.
            assert_eq!(d.m.get(&1).map(String::as_str), Some("a"));
            assert_eq!(
                d.m.get_all(&1).into_iter().map(String::as_str).collect::<Vec<_>>(),
                vec!["a", "b"]
            );
        }
    );
    // Same duplicate-keyed map, but the FIRST entry's key uses a NON-MINIMAL 1-byte argument head
    // (0x18 0x01 for 1, minimal would be 0x01) while the second uses the minimal 0x01. The POSITIONAL
    // encoding sidecar preserves each entry's header independently — a keyed encoding map could not,
    // since the two same-key entries would share (and clobber) one slot.
    kat_preserve!(
        pmap_duplicate_key_nonminimal_head,
        PmapHolder,
        &[0x81, 0xa2, 0x18, 0x01, 0x61, 0x61, 0x01, 0x61, 0x62],
        |d: &PmapHolder| {
            assert_eq!(d.m.len(), 2);
            assert_eq!(
                d.m.get_all(&1).into_iter().map(String::as_str).collect::<Vec<_>>(),
                vec!["a", "b"]
            );
        }
    );

    // The `transaction_metadata` headline: a duplicate-keyed metadata map inside the RECURSIVE
    // metadatum union must decode AND re-emit byte-exact through the recursive-descent (de)serializer.
    // Wire: `[ { "a": 1, "a": 2 } ]` — the holder array (0x81) wraps the union's map arm (0xa2) with a
    // duplicate text key "a" whose values are the union's int arm. This is the consensus-critical
    // property: Cardano's auxiliary-data hash is over these original bytes.
    kat_preserve!(
        metadatum_duplicate_key_recursive,
        MdHolder,
        &[0x81, 0xa2, 0x61, 0x61, 0x01, 0x61, 0x61, 0x02],
        |d: &MdHolder| {
            match &d.meta {
                Md::Mdmap { mdmap, .. } => {
                    assert_eq!(mdmap.len(), 2, "both duplicate-keyed entries survive");
                    assert_eq!(
                        mdmap.get_all(&"a".to_string()).len(),
                        2,
                        "key \"a\" maps to two entries in wire order"
                    );
                }
                other => panic!("expected the map arm, got {other:?}"),
            }
        }
    );

    // The pair-map's public read surface and the `{+ …}` (NonEmptyPairMap) min-1 door, tested
    // in-process against the static runtime (available because `pmap` pulls in the `pair_map` module).
    // `insert` APPENDS (never replaces — that would drop a duplicate); `get` is the first match;
    // `get_all` every match. The non-empty door refuses an empty vec with the same RangeCheck the CBOR
    // decoder raises for `{+ …}`, so wire-side and API-side rejection are identical.
    #[test]
    fn pair_map_surface_and_nonempty_door() {
        use crate::generated::pair_map::{NonEmptyPairMap, PairMap};
        let mut m: PairMap<u64, String> = PairMap::new();
        assert!(m.insert(1, "a".to_string()).is_none(), "insert appends, displaces nothing");
        assert!(m.insert(1, "b".to_string()).is_none(), "a repeated key is appended, not replaced");
        assert_eq!(m.len(), 2, "both duplicate-keyed entries are kept");
        assert_eq!(m.get(&1).map(String::as_str), Some("a"), "get is the FIRST match");
        assert_eq!(
            m.get_all(&1).into_iter().map(String::as_str).collect::<Vec<_>>(),
            vec!["a", "b"],
            "get_all returns every match in entry order"
        );

        // the non-empty door
        assert!(
            NonEmptyPairMap::try_from(Vec::<(u64, String)>::new()).is_err(),
            "an empty map is refused by the min-1 door"
        );
        let ne = NonEmptyPairMap::try_from(vec![(1u64, "a".to_string()), (1u64, "b".to_string())])
            .expect("a non-empty duplicate-keyed vec is accepted");
        assert_eq!(ne.len(), 2, "the door keeps duplicate keys");
    }

    // ---- open struct-map (rest row) under --preserve-encodings ----
    // A captured rest entry's KEY header argument is data: the rest key 7 written non-minimally in the
    // 1-byte form (0x18 0x07 instead of the minimal 0x07) must re-emit VERBATIM through the loose
    // container's per-key encoding sidecar — the declared field 1 stays minimal. Spec bytes
    // {1: 5, 7: 9} with the rest key at 1-byte width. Independent of the generator (hand-derived from
    // RFC 8949 §3's 1-byte argument form).
    kat_preserve!(
        open_map_nonminimal_rest_key,
        OpenMap,
        &[0xa2, 0x01, 0x05, 0x18, 0x07, 0x09],
        |d: &OpenMap| {
            assert_eq!(d.key_1, 5, "declared field decoded");
            assert_eq!(d.rest.get(&7).copied(), Some(9), "rest entry 7 => 9 captured");
        }
    );

    // ---- open array (rest tail) under --preserve-encodings ----
    // A captured TAIL element's header argument is data: the tail element 7 written non-minimally in
    // the 1-byte form (0x18 0x07 instead of the minimal 0x07) must re-emit VERBATIM through the
    // positional per-element encoding sidecar — the declared members (uint 5, tstr "hi") stay minimal.
    // Spec bytes [5, "hi", 7] with the tail element at 1-byte width. Independent of the generator
    // (hand-derived from RFC 8949 §3's 1-byte argument form).
    kat_preserve!(
        open_list_nonminimal_tail_elem,
        OpenList,
        &[0x83, 0x05, 0x62, 0x68, 0x69, 0x18, 0x07],
        |d: &OpenList| {
            assert_eq!(d.index_0, 5, "declared uint member decoded");
            assert_eq!(d.index_1, "hi", "declared tstr member decoded");
            assert_eq!(d.rest, vec![7u64], "tail element 7 captured");
        }
    );

    // ---- native floats (major type 7, heads 0xf9/0xfa/0xfb): the head WIDTH is the data ----
    // Every other family above varies a header ARGUMENT; a float varies the head itself. RFC 8949 §3
    // admits the same value at every width that holds it losslessly, so the width cannot be derived
    // from the value — it must be recorded and replayed. All byte strings are RFC 8949 Appendix A
    // rows or IEEE 754 widenings of them (the table in the delivery spec), never generator output.
    //
    // NaN appears only in the `dbl` (float64) member, deliberately: the `sgl` member round-trips
    // through `x as f32` / `x as f64`, and Rust leaves the payload and sign of a NaN float-cast
    // UNSPECIFIED — a NaN vector there would pin platform behaviour rather than the spec. For the
    // same reason `sgl`'s values are all f32-exact: a double the f32 domain cannot hold would narrow
    // lossily on decode, which is a different (and out-of-scope) question from width preservation.

    // 1.0 at each of the three legal widths, one vector per width — the family's whole point. The
    // decoded value is bit-identical in all three, so NOTHING about it can pick the head; only the
    // recorded Sz can. An implementation that re-derives the width from the value (or that always
    // writes the widest, or always the narrowest) passes at most one of these three.
    // §A: 1.0 -> f9 3c00 / fa 3f800000 / fb 3ff0000000000000.
    kat_preserve!(
        floats_width_f16,
        Floats,
        &[0x82, 0xf9, 0x3c, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert_eq!(d.dbl, 1.0);
            assert_eq!(d.sgl, 1.0);
        }
    );
    kat_preserve!(
        floats_width_f32,
        Floats,
        &[0x82, 0xfa, 0x3f, 0x80, 0x00, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert_eq!(d.dbl, 1.0);
            assert_eq!(d.sgl, 1.0);
        }
    );
    kat_preserve!(
        floats_width_f64,
        Floats,
        &[
            0x82, 0xfb, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf9, 0x3c, 0x00
        ],
        |d: &Floats| {
            assert_eq!(d.dbl, 1.0);
            assert_eq!(d.sgl, 1.0);
        }
    );
    // The other direction: a value whose width the spec FIXES, so preservation must not narrow it.
    // 1.1 (§A: fb 3ff199999999999a) has no exact f16 or f32 form — 0xfb is its only legal head — and
    // 100000.0 (§A: fa 47c35000) has no f16 form but IS f32-exact, so 0xfa is its narrowest. Paired
    // with the trio above, a writer that ignores the recorded width and always emits the shortest
    // lossless head fails the trio; one that always emits 0xfb fails this vector's `sgl`.
    kat_preserve!(
        floats_width_bound_to_value,
        Floats,
        &[
            0x82, 0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a, 0xfa, 0x47, 0xc3, 0x50,
            0x00
        ],
        |d: &Floats| {
            assert_eq!(d.dbl, 1.1);
            assert_eq!(d.sgl, 100000.0);
        }
    );
    // Signed zero: -0.0 compares EQUAL to 0.0, so only the sign BIT distinguishes the two encodings.
    // A round-trip that rebuilt the value through any comparison-based path would emit 0x0000 here.
    // §A: -0.0 -> f9 8000, widened to f32 -> fa 80000000 (the `sgl` member's cast is exact for
    // finite values, signed zero included).
    kat_preserve!(
        floats_negative_zero,
        Floats,
        &[0x82, 0xf9, 0x80, 0x00, 0xfa, 0x80, 0x00, 0x00, 0x00],
        |d: &Floats| {
            assert!(d.dbl == 0.0 && d.dbl.is_sign_negative());
            assert!(d.sgl == 0.0 && d.sgl.is_sign_negative());
        }
    );
    // The two ends of the f16 range, where "does this value fit the narrow head" is decided:
    // 5.960464477539063e-8 is the min f16 SUBNORMAL (§A: f9 0001; its f64 form is
    // 3e70000000000000, which the anchor asserts bit-exactly rather than through a 16-digit decimal
    // literal) and 65504.0 is the max finite f16 (§A: f9 7bff). A width check written as a magnitude
    // range rather than an exactness test mis-handles the subnormal end.
    kat_preserve!(
        floats_f16_edges,
        Floats,
        &[0x82, 0xf9, 0x00, 0x01, 0xf9, 0x7b, 0xff],
        |d: &Floats| {
            assert_eq!(d.dbl.to_bits(), 0x3e70_0000_0000_0000u64);
            assert_eq!(d.sgl, 65504.0);
        }
    );
    // Non-finite values at the narrow head (§A: Infinity -> f9 7c00, -Infinity -> f9 fc00).
    kat_preserve!(
        floats_infinity_f16,
        Floats,
        &[0x82, 0xf9, 0x7c, 0x00, 0xf9, 0xfc, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_infinite() && d.dbl.is_sign_positive());
            assert!(d.sgl.is_infinite() && d.sgl.is_sign_negative());
        }
    );
    // …and the same two infinities at the WIDE heads (§A: fb 7ff0000000000000 / fa ff800000). An
    // infinity is exactly representable at every width, so all three heads are legal for it and the
    // recorded one is the only thing that decides — the non-finite twin of the 1.0 trio.
    kat_preserve!(
        floats_infinity_wide,
        Floats,
        &[
            0x82, 0xfb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfa, 0xff, 0x80, 0x00,
            0x00
        ],
        |d: &Floats| {
            assert!(d.dbl.is_infinite() && d.dbl.is_sign_positive());
            assert!(d.sgl.is_infinite() && d.sgl.is_sign_negative());
        }
    );
    // NaN at each of the three widths (§A: f9 7e00 / fa 7fc00000 / fb 7ff8000000000000 — all the
    // same quiet NaN, the payload left-aligns on widening). These are the vectors that pin
    // --preserve-encodings as NOT normalizing: the canonical suite's twins take the identical inputs
    // to f9 7e00 (RFC 8949 §4.2.2), so a serializer that normalized unconditionally would pass every
    // canonical NaN vector and fail these two wide ones. An `assert_eq!` on a NaN is always false, so
    // the anchor is `is_nan()`.
    kat_preserve!(
        floats_nan_f16,
        Floats,
        &[0x82, 0xf9, 0x7e, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.sgl, 1.0);
        }
    );
    kat_preserve!(
        floats_nan_f32,
        Floats,
        &[0x82, 0xfa, 0x7f, 0xc0, 0x00, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.sgl, 1.0);
        }
    );
    kat_preserve!(
        floats_nan_f64,
        Floats,
        &[
            0x82, 0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf9, 0x3c, 0x00
        ],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.sgl, 1.0);
        }
    );
    // A NaN carrying a PAYLOAD (mantissa 0x8000000000001): the low bit does not fit an f16 or f32
    // mantissa, so 0xfb is its only lossless head AND the payload bits are part of what must survive.
    // Preserve keeps both; the canonical twin drops the payload to f9 7e00, which is why the anchor
    // asserts the exact bit pattern rather than just `is_nan()`.
    kat_preserve!(
        floats_nan_payload_f64,
        Floats,
        &[
            0x82, 0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xf9, 0x3c, 0x00
        ],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.dbl.to_bits(), 0x7ff8_0000_0000_0001u64);
        }
    );
    // The payload case at the OTHER end of the width range: `f9 7e01` is a quiet NaN whose payload
    // (f16 mantissa 0x201 — quiet bit plus low bit) DOES fit the 2-byte head, so the shortest
    // lossless width is 0xf9 and the recorded width is honored directly. Its f64 form is the
    // left-aligned widening 0x7ff8040000000000 (mantissa 0x201 << 42), which the anchor asserts
    // bit-exactly. Paired with floats_nan_payload_f64 above, the two pin BOTH branches of the
    // payload-fits-a-narrow-mantissa question: that one needs 8 bytes, this one needs 2. Without it a
    // width rule that sent every payload-carrying NaN to 0xfb would still pass every other vector.
    kat_preserve!(
        floats_nan_payload_f16,
        Floats,
        &[0x82, 0xf9, 0x7e, 0x01, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.dbl.to_bits(), 0x7ff8_0400_0000_0000u64);
        }
    );
    // A NEGATIVE NaN (§3.3 leaves the sign bit of a NaN free; IEEE 754 `fb fff8000000000000` is the
    // zero-payload quiet NaN with the sign set). Preserve keeps the sign AND the 8-byte head; the
    // canonical twin drops both (§4.2.2's canonical NaN is the POSITIVE zero-payload quiet NaN, so
    // canonicalization is the only place the sign of a NaN is touched). `is_nan()` says nothing about
    // the sign, so the anchor asserts the bit pattern.
    kat_preserve!(
        floats_nan_negative,
        Floats,
        &[
            0x82, 0xfb, 0xff, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf9, 0x3c, 0x00
        ],
        |d: &Floats| {
            assert!(d.dbl.is_nan() && d.dbl.is_sign_negative());
            assert_eq!(d.dbl.to_bits(), 0xfff8_0000_0000_0000u64);
        }
    );
    // A SIGNALING NaN: `f9 7c01` has the all-ones exponent with the quiet bit (mantissa MSB) CLEAR
    // and a non-zero payload, so it is a NaN but not a quiet one. Its f64 form is
    // 0x7ff0040000000000 (mantissa 0x001 << 42). It survives the round trip because the runtime's
    // NaN handling is software bit-ops end to end — `float_sz()` reads bits, `write_float_sz` writes
    // bits — and any float ARITHMETIC on the way through would quiet it (setting the mantissa MSB and
    // turning this into `f9 7e01`). That is what the vector pins: not a spec guarantee about sNaN
    // semantics, but that no arithmetic sneaks into the width/value path.
    kat_preserve!(
        floats_nan_signaling,
        Floats,
        &[0x82, 0xf9, 0x7c, 0x01, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.dbl.to_bits(), 0x7ff0_0400_0000_0000u64);
        }
    );

    // ---- a FIXED float member: the VALUE is spec-pinned, the WIDTH is still data ----
    // `fixed_float = [v: 1.5]` generates a struct with no value field at all (the constant lives in
    // the emitted serialize/deserialize), so the head width is the ONLY thing left to preserve — and
    // it is, at all three of 1.5's legal widths (§A: f9 3e00 / fa 3fc00000 / fb 3ff8000000000000).
    // With no field to anchor, the decode half is pinned instead by fixed_float_wrong_value_rejected
    // below: the constant IS compared, so acceptance is a value assertion.
    kat_preserve!(fixed_float_minimal, FixedFloat, &[0x81, 0xf9, 0x3e, 0x00], |_d: &FixedFloat| {});
    kat_preserve!(
        fixed_float_f32,
        FixedFloat,
        &[0x81, 0xfa, 0x3f, 0xc0, 0x00, 0x00],
        |_d: &FixedFloat| {}
    );
    kat_preserve!(
        fixed_float_wide,
        FixedFloat,
        &[0x81, 0xfb, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        |_d: &FixedFloat| {}
    );

    // The fixed member's value anchor: the constant is genuinely compared, so a different float in
    // that slot is refused (FixedValueMismatch) rather than silently accepted and re-emitted. Without
    // this the three width vectors above would pass against a decoder that read the head and ignored
    // the payload. 1.0 (f9 3c00) is a legal float at the same width — only the VALUE differs.
    #[test]
    fn fixed_float_wrong_value_rejected() {
        assert!(
            FixedFloat::from_cbor_bytes(&[0x81, 0xf9, 0x3c, 0x00]).is_err(),
            "a float other than the spec's 1.5 must not be accepted in a fixed-value slot"
        );
    }

    // ---- per-element float widths in a homogeneous array (`[* float64]`, wrapped in a record) ----
    // Two different values at two different widths: 1.0 at the narrow head, 1.5 at the wide one
    // (§A: f9 3c00, fb 3ff8000000000000). The widths are per-ELEMENT, held in the positional sidecar.
    kat_preserve!(
        float_seq_mixed_widths,
        FloatHolder,
        &[
            0x81, 0x82, 0xf9, 0x3c, 0x00, 0xfb, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        ],
        |d: &FloatHolder| {
            assert_eq!(d.fs, vec![1.0, 1.5]);
        }
    );
    // The discriminating version: ONE indefinite array holding the SAME value three times at three
    // different widths. The three elements are indistinguishable after decoding, so a sidecar keyed
    // by anything but position — or a single width recorded for the whole array — collapses them to
    // one head and fails. Also re-frames nothing: the indefinite framing (0x9f … 0xff) is preserved
    // alongside the per-element widths.
    kat_preserve!(
        float_seq_same_value_three_widths,
        FloatHolder,
        &[
            0x81, 0x9f, 0xf9, 0x3c, 0x00, 0xfa, 0x3f, 0x80, 0x00, 0x00, 0xfb, 0x3f, 0xf0, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0xff
        ],
        |d: &FloatHolder| {
            assert_eq!(d.fs, vec![1.0, 1.0, 1.0]);
        }
    );

    // ---- head-CONSTRAINED float names: the width is the TYPE's, not the input's ----
    // `float_widths = [h: float16, s: float32, d: float64]`. Where `Floats` above pins "the recorded
    // width is data", these pin its complement: a name that declares one head has no width left to
    // record, so the only in-set input round-trips byte-exactly and every other head is refused.
    // §A: 1.0 -> f9 3c00 / fa 3f800000 / fb 3ff0000000000000.
    kat_preserve!(
        float_widths_declared_heads,
        FloatWidths,
        &[
            0x83, 0xf9, 0x3c, 0x00, 0xfa, 0x3f, 0x80, 0x00, 0x00, 0xfb, 0x3f, 0xf0, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00
        ],
        |d: &FloatWidths| {
            assert_eq!((d.h, d.s), (1.0f32, 1.0f32));
            assert_eq!(d.d, 1.0f64);
        }
    );

    #[test]
    fn float_widths_refuse_every_out_of_set_head() {
        let f9: &[u8] = &[0xf9, 0x3c, 0x00];
        let fa: &[u8] = &[0xfa, 0x3f, 0x80, 0x00, 0x00];
        let fb: &[u8] = &[0xfb, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let build = |items: [&[u8]; 3]| {
            let mut v = vec![0x83u8];
            for i in items {
                v.extend_from_slice(i);
            }
            v
        };
        FloatWidths::from_cbor_bytes(&build([f9, fa, fb])).unwrap();
        for bad in [
            [fa, fa, fb],
            [fb, fa, fb],
            [f9, f9, fb],
            [f9, fb, fb],
            [f9, fa, f9],
            [f9, fa, fa],
        ] {
            assert!(
                FloatWidths::from_cbor_bytes(&build(bad)).is_err(),
                "a head outside the member's declared set must be a decode error"
            );
        }
    }

    #[test]
    fn float_widths_write_the_declared_head_for_a_fresh_value() {
        // 1.0's narrowest lossless head is f9; `s`/`d` write theirs anyway.
        let v = FloatWidths::new(1.0, 1.0, 1.0);
        assert_eq!(
            v.to_cbor_bytes(),
            &[
                0x83, 0xf9, 0x3c, 0x00, 0xfa, 0x3f, 0x80, 0x00, 0x00, 0xfb, 0x3f, 0xf0, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00
            ]
        );
    }
}
