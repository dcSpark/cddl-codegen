// Golden known-answer vectors for --canonical-form. Every `canonical` byte string below is
// hand-derived from RFC 8949 §4.2.1 (smallest-argument "preferred serialization") and §4.2.2
// (indefinite-length items re-framed definite), NOT copied from generator output and NOT built
// with the tests/deser_test cbor_event helpers: the canonical suite's other expected bytes come
// from the same write_*_sz/Sz::canonical layer the generated code encodes with, so a symmetric
// bug there passes every one of those assertions. Raw spec-anchored hex is the independent check.
//
// NOTE on map-key ordering: this project's --canonical-form implements RFC 7049 §3.9 "Canonical
// CBOR" key order (length-first, then bytewise — documented in docs/docs/command_line_flags.mdx),
// not RFC 8949 §4.2.1's pure bytewise order. For keys of a SINGLE major type in minimal form the
// two rules coincide (head bytes grow with encoded length), so the uint_table vectors pin that
// the sort exists, sorts CANONICAL key bytes (not preserved ones), and tie-breaks bytewise; the
// int_table vector pins the cross-major cell where the two rules DISAGREE (-1 vs 256), the only
// vector that discriminates length-first from bytewise.
//
// Each vector decodes an IRREGULAR (but valid) encoding and asserts four things:
//   * value anchor — a decoded field equals the value hand-read from the input bytes (identity
//     alone can't catch an exactly-compensating decode+encode bug),
//   * preserve    — to_cbor_bytes() re-encodes the irregular input byte-identically,
//   * canonical   — to_canonical_cbor_bytes() equals the hand-derived §4.2 minimal bytes, and
//   * fixed point — decoding the canonical bytes and re-canonicalizing returns them unchanged.
//
// AUTHORING CONVENTION: write byte strings as two-digit `0x??` literals only (no decimal, no
// single-digit hex). cddl-matrix/project_golden_hex.ts validates every byte array in a
// kat_canonical! invocation (complete 0x?? tokenization + exactly one well-formed CBOR item) and
// hard-fails on any other spelling. Run it (and commit the regenerated
// tests/golden_hex/COVERAGE.md) after changing this file.
#[cfg(test)]
mod golden_hex_canonical {
    use super::*;
    use serialization::Deserialize;

    macro_rules! kat_canonical {
        ($name:ident, $t:ty, $irregular:expr, $canonical:expr, $anchor:expr) => {
            #[test]
            fn $name() {
                let irregular: &[u8] = $irregular;
                let canonical: &[u8] = $canonical;
                let decoded = <$t>::from_cbor_bytes(irregular).unwrap();
                // value anchor: pin the decode half to the hand-read spec value
                ($anchor)(&decoded);
                assert_eq!(
                    decoded.to_cbor_bytes(),
                    irregular,
                    "preserve identity broke (left = re-encoded, right = irregular input)"
                );
                assert_eq!(
                    decoded.to_canonical_cbor_bytes(),
                    canonical,
                    "canonical KAT mismatch (left = generated, right = RFC 8949 §4.2)"
                );
                let re = <$t>::from_cbor_bytes(canonical).unwrap();
                assert_eq!(
                    re.to_canonical_cbor_bytes(),
                    canonical,
                    "canonical bytes are not a fixed point"
                );
            }
        };
    }

    // ---- §4.2.1: integer arguments shrink to the smallest form that holds the value ----
    // [23 (1-byte argument), 24 (2-byte argument)] -> [0x17, 0x18 0x18] (§A: 23 -> 0x17,
    // 24 -> 0x1818 — 24 is the smallest value that keeps the 1-byte argument).
    kat_canonical!(
        canon_pair_min_args,
        Pair,
        &[0x82, 0x18, 0x17, 0x19, 0x00, 0x18],
        &[0x82, 0x17, 0x18, 0x18],
        |d: &Pair| {
            assert_eq!((d.a, d.b), (23, 24));
        }
    );
    // Boundary above the 1-byte argument: 255 (4-byte form) shrinks to 0x18 0xff; 256 already
    // needs the 2-byte argument and must stay 0x19 0x01 0x00.
    kat_canonical!(
        canon_pair_boundary_255_256,
        Pair,
        &[0x82, 0x1a, 0x00, 0x00, 0x00, 0xff, 0x19, 0x01, 0x00],
        &[0x82, 0x18, 0xff, 0x19, 0x01, 0x00],
        |d: &Pair| {
            assert_eq!((d.a, d.b), (255, 256));
        }
    );
    // u64::MAX genuinely needs the 8-byte argument — canonical must KEEP it (§A:
    // 18446744073709551615 -> 0x1bffffffffffffffff); the padded 0 shrinks to the immediate 0x00.
    kat_canonical!(
        canon_pair_u64_max,
        Pair,
        &[
            0x82, 0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1a, 0x00, 0x00, 0x00,
            0x00
        ],
        &[0x82, 0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00],
        |d: &Pair| {
            assert_eq!((d.a, d.b), (u64::MAX, 0));
        }
    );

    // ---- §4.2.1 on negative integers (major type 1): argument = -1 - value; the generated
    // struct stores that argument (see the npair note in tests/golden_hex_preserve/tests.rs) ----
    // The major-type-1 boundary mirroring 23/24: -24 (1-byte argument form) shrinks to the
    // immediate head 0x37; -25's argument is 24, so 0x38 0x18 is already minimal and must stay.
    kat_canonical!(
        canon_npair_boundary,
        Npair,
        &[0x82, 0x38, 0x17, 0x39, 0x00, 0x18],
        &[0x82, 0x37, 0x38, 0x18],
        |d: &Npair| {
            assert_eq!((d.n, d.m), (23, 24)); // wire -24, -25
        }
    );
    // Maximally-padded 8-byte arguments: -24 shrinks all the way to 0x37; -4294967297's argument
    // is 4294967296 = 2^32, one past the 4-byte maximum, so the 8-byte form IS minimal and stays.
    kat_canonical!(
        canon_npair_8byte_pad,
        Npair,
        &[
            0x82, 0x3b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17, 0x3b, 0x00, 0x00, 0x00,
            0x01, 0x00, 0x00, 0x00, 0x00
        ],
        &[0x82, 0x37, 0x3b, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00],
        |d: &Npair| {
            assert_eq!((d.n, d.m), (23, 4294967296)); // wire -24, -4294967297
        }
    );

    // ---- §4.2.2: indefinite-length items re-frame definite ----
    // indefinite array of wide-argument ints -> definite array(2) of immediates.
    kat_canonical!(
        canon_pair_indef_wide,
        Pair,
        &[
            0x9f, 0x1a, 0x00, 0x00, 0x00, 0x01, 0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x02, 0xff
        ],
        &[0x82, 0x01, 0x02],
        |d: &Pair| {
            assert_eq!((d.a, d.b), (1, 2));
        }
    );
    // Chunked indefinite strings coalesce to definite: RFC §3.2.3's (_ "strea", "ming") and
    // (_ h'0102', h'030405') become "streaming" (9 bytes, head 0x69) and h'0102030405' (0x45).
    kat_canonical!(
        canon_strs_chunks,
        Strs,
        &[
            0x82, 0x7f, 0x65, 0x73, 0x74, 0x72, 0x65, 0x61, 0x64, 0x6d, 0x69, 0x6e, 0x67, 0xff,
            0x5f, 0x42, 0x01, 0x02, 0x43, 0x03, 0x04, 0x05, 0xff
        ],
        &[
            0x82, 0x69, 0x73, 0x74, 0x72, 0x65, 0x61, 0x6d, 0x69, 0x6e, 0x67, 0x45, 0x01, 0x02,
            0x03, 0x04, 0x05
        ],
        |d: &Strs| {
            assert_eq!(d.s, "streaming");
            assert_eq!(d.v, vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        }
    );
    // Indefinite map with entries reordered (b before a) -> definite map(2) with keys in
    // canonical order: "a"/"b" encode to equal-length 0x6161/0x6162, so length-first and bytewise
    // ordering agree here — a comes first. (WHICH rule the generator implements is pinned by the
    // discriminating pair in tests/canonical mixed_len_keys_ordering_rule, not re-tested here.)
    kat_canonical!(
        canon_kv_sorted,
        Kv,
        &[0xbf, 0x61, 0x62, 0x02, 0x61, 0x61, 0x01, 0xff],
        &[0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02],
        |d: &Kv| {
            assert_eq!((d.a, d.b), (1, 2));
        }
    );

    // ---- the RUNTIME table key sort (`{ * uint => text }` in a 1-element record) ----
    // This is the per-table sort the generated serialize emits under force_canonical — a
    // different code path from the codegen-time STRUCT key sort above. Insertion order
    // 256-then-10 (indefinite framing) must re-frame definite AND reorder: key 10 (1-byte
    // encoding 0x0a) sorts before 256 (3-byte 0x19 0x01 0x00) — length-first, with which
    // single-major bytewise agrees (see the file header note on the blocked cross-major cell).
    kat_canonical!(
        canon_table_sorted,
        TableHolder,
        &[0x81, 0xbf, 0x19, 0x01, 0x00, 0x61, 0x61, 0x0a, 0x61, 0x62, 0xff],
        &[0x81, 0xa2, 0x0a, 0x61, 0x62, 0x19, 0x01, 0x00, 0x61, 0x61],
        |d: &TableHolder| {
            assert_eq!(d.t.len(), 2);
            assert_eq!(d.t.get(&256u64).map(|s| s.as_str()), Some("a"));
            assert_eq!(d.t.get(&10u64).map(|s| s.as_str()), Some("b"));
        }
    );
    // The sort must compare CANONICAL key bytes, not the preserved input encodings: key 10
    // arrives PADDED to the 2-byte form (0x19 0x00 0x0a, 3 bytes) after a minimal key 11 (0x0b,
    // 1 byte). A sort over preserved bytes would order 11 first (1 < 3 length-first); the correct
    // canonical output re-minimizes 10 to 0x0a and sorts it BEFORE 11 (equal length, bytewise).
    kat_canonical!(
        canon_table_sorts_canonical_bytes,
        TableHolder,
        &[0x81, 0xa2, 0x0b, 0x61, 0x62, 0x19, 0x00, 0x0a, 0x61, 0x61],
        &[0x81, 0xa2, 0x0a, 0x61, 0x61, 0x0b, 0x61, 0x62],
        |d: &TableHolder| {
            assert_eq!(d.t.len(), 2);
            assert_eq!(d.t.get(&10u64).map(|s| s.as_str()), Some("a"));
            assert_eq!(d.t.get(&11u64).map(|s| s.as_str()), Some("b"));
        }
    );

    // cross-major keys — the length-first vs bytewise DISCRIMINATOR. Canonical key bytes:
    // -1 -> 0x20 (major 1, argument 0 inline; 1 byte), 256 -> 0x19 0x01 0x00 (major 0, 2-byte
    // argument; 3 bytes). RFC 7049 §3.9 length-first (the documented rule): -1 sorts FIRST.
    // RFC 8949 §4.2.1 bytewise would sort 256 first (0x19 < 0x20) — a bytewise sort, or a sort
    // of signed VALUES (-1 < 256 coincides here, so the anchor also checks both lookups), flips
    // this vector. Irregular input: indefinite map, keys in the OPPOSITE (bytewise) order, both
    // arguments non-minimal (256 as 8-byte 0x1b, -1 as 1-byte-argument 0x38 0x00), so
    // canonicalization must reorder AND minimize.
    kat_canonical!(
        canon_table_cross_major_key_order_length_first,
        IntHolder,
        &[
            0x81, 0xbf, 0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x61, 0x70, 0x38,
            0x00, 0x61, 0x6e, 0xff,
        ],
        &[
            0x81, 0xa2, 0x20, 0x61, 0x6e, 0x19, 0x01, 0x00, 0x61, 0x70,
        ],
        |d: &IntHolder| {
            assert_eq!(d.it.len(), 2);
            // new_nint takes the WIRE magnitude: -1 is |−1 + 1| = 0
            assert_eq!(
                d.it.get(&Int::new_nint(0)).map(|s| s.as_str()),
                Some("n")
            );
            assert_eq!(
                d.it.get(&Int::new_uint(256)).map(|s| s.as_str()),
                Some("p")
            );
        }
    );

    // homogeneous array: indefinite framing + non-minimal elements -> definite minimal.
    kat_canonical!(
        canon_seq,
        SeqHolder,
        &[0x81, 0x9f, 0x18, 0x01, 0x19, 0x00, 0x02, 0x03, 0xff],
        &[0x81, 0x83, 0x01, 0x02, 0x03],
        |d: &SeqHolder| {
            assert_eq!(d.xs, vec![1, 2, 3]);
        }
    );
    // tag 11 in a 2-byte argument + indefinite body -> minimal tag head 0xcb + definite array
    // (§4.2.1's smallest-argument rule covers tag heads too).
    kat_canonical!(
        canon_tagged,
        TaggedOne,
        &[0xd9, 0x00, 0x0b, 0x9f, 0x18, 0x05, 0xff],
        &[0xcb, 0x81, 0x05],
        |d: &TaggedOne| {
            assert_eq!(d.x, 5);
        }
    );

    // ---- tag-258 set idiom: --canonical-form minimizes the tag SIZE, never its PRESENCE ----
    // tagged, non-minimal 8-byte head (0xdb + 0x0000000000000102) -> tagged, minimal 2-byte head
    // (0xd9 0x01 0x02): the SIZE is minimized, the tag is KEPT. Holder = 1-element array (0x81)
    // wrapping the set of one text element "a" (0x81 0x61 0x61).
    kat_canonical!(
        canon_opt_set_tagged_size_minimized,
        OptSetHolder,
        &[0x81, 0xdb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x02, 0x81, 0x61, 0x61],
        &[0x81, 0xd9, 0x01, 0x02, 0x81, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(*d.s, vec!["a".to_string()]);
        }
    );
    // untagged stays untagged: canonicality governs encoding MINIMALITY, not which arm the author
    // wrote, so the tag is never forced on. Bytes are already minimal, so canonical == input — the
    // point is that PRESENCE is not canonicalized to tagged (and untagged is a canonical fixed point).
    kat_canonical!(
        canon_opt_set_untagged_presence_preserved,
        OptSetHolder,
        &[0x81, 0x81, 0x61, 0x61],
        &[0x81, 0x81, 0x61, 0x61],
        |d: &OptSetHolder| {
            assert_eq!(*d.s, vec!["a".to_string()]);
        }
    );

    // ---- `@duplicates preserve` table (PairMap) under --canonical-form ----
    // A duplicate-keyed pair-map has NO RFC 8949 canonical form (deterministic encoding requires
    // unique keys), so the canonical path does the documented deterministic best-effort: a STABLE
    // sort by encoded key bytes with DUPLICATES kept adjacent in first-appearance order. This vector
    // pins all three halves the sort must get right at once:
    //   * sort-by-key-bytes: irregular key order 2,1,1 canonicalizes to 1,1,2 (0x01 < 0x02),
    //   * dup-adjacency + first-appearance order: the two key-1 entries stay "a" (2nd overall) then
    //     "b" (3rd overall) — the stable sort must not swap them, and the `i`-carrying key_order
    //     tuple is what keeps the positional value encoding aligned after the sort,
    //   * determinism: re-decoding the canonical bytes and re-canonicalizing is a fixed point.
    // Holder `[p: dup_pmap]` is a 1-element array (0x81) wrapping the 3-entry map (0xa3).
    kat_canonical!(
        canon_dup_pmap_key_sort,
        DupPmapHolder,
        &[0x81, 0xa3, 0x02, 0x61, 0x63, 0x01, 0x61, 0x61, 0x01, 0x61, 0x62],
        &[0x81, 0xa3, 0x01, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02, 0x61, 0x63],
        |d: &DupPmapHolder| {
            assert_eq!(d.p.len(), 3);
            // get() is the FIRST match; get_all() every match in entry order
            assert_eq!(d.p.get(&1).map(String::as_str), Some("a"));
            assert_eq!(
                d.p.get_all(&1).into_iter().map(String::as_str).collect::<Vec<_>>(),
                vec!["a", "b"]
            );
            assert_eq!(d.p.get(&2).map(String::as_str), Some("c"));
        }
    );
    // Per-entry header minimization on a duplicate-keyed pair-map: the FIRST key-1 entry uses a
    // NON-MINIMAL 1-byte-argument head (0x18 0x01) while the second uses the minimal 0x01. Canonical
    // minimizes BOTH heads to 0x01 while the duplicate keys stay adjacent in first-appearance order
    // ("a" then "b"). The positional encoding sidecar means each entry's head canonicalizes
    // independently — a keyed encoding map could not, since the two same-key entries would collide.
    kat_canonical!(
        canon_dup_pmap_nonminimal_head,
        DupPmapHolder,
        &[0x81, 0xa2, 0x18, 0x01, 0x61, 0x61, 0x01, 0x61, 0x62],
        &[0x81, 0xa2, 0x01, 0x61, 0x61, 0x01, 0x61, 0x62],
        |d: &DupPmapHolder| {
            assert_eq!(d.p.len(), 2);
            assert_eq!(
                d.p.get_all(&1).into_iter().map(String::as_str).collect::<Vec<_>>(),
                vec!["a", "b"]
            );
        }
    );

    // ---- open struct-map (rest row) under --canonical-form ----
    // The runtime canonical key merge minimizes each rest key's header and orders the declared key
    // together with the rest keys. Irregular input {1: 5, 7: 9} with the rest key 7 in the 1-byte
    // form (0x18 0x07): preserve identity keeps it verbatim, canonical minimizes it to 0x07 (and 1
    // sorts before 7, so no reordering — the declared field stays first). Hand-derived from
    // RFC 8949 §4.2.1 (minimal-length arguments), independent of the generator.
    kat_canonical!(
        open_map_rest_key_minimized,
        OpenMap,
        &[0xa2, 0x01, 0x05, 0x18, 0x07, 0x09],
        &[0xa2, 0x01, 0x05, 0x07, 0x09],
        |d: &OpenMap| {
            assert_eq!(d.key_1, 5, "declared field decoded");
            assert_eq!(d.rest.get(&7).copied(), Some(9), "rest entry 7 => 9 captured");
        }
    );
}
