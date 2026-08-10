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
    // The bounded pair-map uses the SAME stable key sort after entering its checked [2, 3] door.
    // Equal key-1 entries arrive in "a", then "b" order and canonical output MUST retain that
    // relative order: this executes the bounds-bearing carrier path, not just the loose PairMap
    // sibling above. Holder `[p: bounded_dup_pmap]` is a one-element array (0x81).
    kat_canonical!(
        canon_bounded_dup_pmap_stable_equal_keys,
        BoundedDupPmapHolder,
        &[0x81, 0xa2, 0x01, 0x61, 0x61, 0x01, 0x61, 0x62],
        &[0x81, 0xa2, 0x01, 0x61, 0x61, 0x01, 0x61, 0x62],
        |d: &BoundedDupPmapHolder| {
            assert_eq!(d.p.len(), 2);
            assert_eq!(
                d.p.get_all(&1).into_iter().map(String::as_str).collect::<Vec<_>>(),
                vec!["a", "b"],
                "equal encoded keys retain first-appearance order under the stable canonical sort"
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

    // ---- native floats (major type 7, heads 0xf9/0xfa/0xfb) ----
    // §4.2.1's smallest-argument rule applies to the float HEAD too, with one twist that makes it
    // stricter than the integer case: the shortest head must preserve the value BIT-FOR-BIT, not
    // merely approximate it. So "shrink" is bounded by exact representability, and a value with no
    // narrower exact form must stay wide (canon_floats_shortest_lossless). Every byte string is an
    // RFC 8949 Appendix A row or an IEEE 754 widening of one (the table in the delivery spec).
    //
    // The `sgl` (float32) member's values are all f32-exact, so its decode-side `x as f32` narrowing
    // is never itself lossy — that is a separate question from canonical width choice. NaN vectors
    // use `dbl` only: Rust leaves the payload and sign of a NaN float-cast unspecified.

    // Every width of 1.0 canonicalizes to the shortest head that holds it, 0xf9 3c00 (§A). The
    // irregular input carries the SAME value at 0xfb and 0xfa, so this one vector covers both
    // widenings; the f9 -> f9 case is the vector's own fixed-point half (decode the canonical bytes,
    // re-canonicalize, get them back). Preserve identity is asserted first, so the vector also proves
    // the two profiles genuinely differ on the same input rather than one of them being unreachable.
    kat_canonical!(
        canon_floats_widths_shrink,
        Floats,
        &[
            0x82, 0xfb, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfa, 0x3f, 0x80, 0x00,
            0x00
        ],
        &[0x82, 0xf9, 0x3c, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert_eq!(d.dbl, 1.0);
            assert_eq!(d.sgl, 1.0);
        }
    );
    // The bound on shrinking, both halves in one vector. 1.1 (§A: fb 3ff199999999999a) has no exact
    // f16 or f32 form, so 0xfb IS its preferred head and canonical must leave it alone — a writer
    // that picked the narrowest head holding an APPROXIMATION would emit fa 3f8ccccd here and change
    // the value. 100000.0 arrives widened to 0xfb (fb 40f86a0000000000) and must shrink to exactly
    // 0xfa (§A: fa 47c35000), not to 0xf9: it exceeds the f16 range. Together they pin the rule as
    // "shortest LOSSLESS", which neither "never shrink" nor "always shrink to f16" satisfies.
    kat_canonical!(
        canon_floats_shortest_lossless,
        Floats,
        &[
            0x82, 0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a, 0xfb, 0x40, 0xf8, 0x6a,
            0x00, 0x00, 0x00, 0x00, 0x00
        ],
        &[
            0x82, 0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a, 0xfa, 0x47, 0xc3, 0x50,
            0x00
        ],
        |d: &Floats| {
            assert_eq!(d.dbl, 1.1);
            assert_eq!(d.sgl, 100000.0);
        }
    );
    // The two ends of the f16 range decide where shrinking stops, so both must shrink all the way:
    // 65504.0 is the max finite f16 (§A: f9 7bff, widened fb 40effc0000000000) and 2^-24 is the min
    // f16 SUBNORMAL (§A: f9 0001, widened fa 33800000). A width choice written as a magnitude range
    // rather than an exactness test refuses the subnormal and leaves it at 0xfa.
    kat_canonical!(
        canon_floats_f16_edges,
        Floats,
        &[
            0x82, 0xfb, 0x40, 0xef, 0xfc, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfa, 0x33, 0x80, 0x00,
            0x00
        ],
        &[0x82, 0xf9, 0x7b, 0xff, 0xf9, 0x00, 0x01],
        |d: &Floats| {
            assert_eq!(d.dbl, 65504.0);
            assert_eq!(d.sgl.to_bits(), 0x3e70_0000_0000_0000u64);
        }
    );
    // Signed zero shrinks but is NOT normalized: §4.2 minimizes the ENCODING, and -0.0 and 0.0 are
    // different values (only §4.2.2's NaN rule changes a value). -0.0 at 0xfb -> f9 8000, 0.0 at
    // 0xfa -> f9 0000. A canonicalizer that rebuilt the value through a comparison would emit
    // f9 0000 for both, since -0.0 == 0.0.
    kat_canonical!(
        canon_floats_negative_zero,
        Floats,
        &[
            0x82, 0xfb, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfa, 0x00, 0x00, 0x00,
            0x00
        ],
        &[0x82, 0xf9, 0x80, 0x00, 0xf9, 0x00, 0x00],
        |d: &Floats| {
            assert!(d.dbl == 0.0 && d.dbl.is_sign_negative());
            assert!(d.sgl == 0.0 && d.sgl.is_sign_positive());
        }
    );
    // Infinities are exactly representable at every width, so both shrink to the f16 head and the
    // SIGN survives (§A: Infinity -> f9 7c00, -Infinity -> f9 fc00). Irregular input carries them at
    // 0xfb and 0xfa.
    kat_canonical!(
        canon_floats_infinity,
        Floats,
        &[
            0x82, 0xfb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfa, 0xff, 0x80, 0x00,
            0x00
        ],
        &[0x82, 0xf9, 0x7c, 0x00, 0xf9, 0xfc, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_infinite() && d.dbl.is_sign_positive());
            assert!(d.sgl.is_infinite() && d.sgl.is_sign_negative());
        }
    );
    // ---- §4.2.2 NaN normalization: the one place canonicalization changes a VALUE ----
    // RFC 8949 §4.2.2 makes the deterministic encoding of every NaN the zero-payload quiet NaN
    // f9 7e00. That is NOT a consequence of the shortest-lossless width rule — it is a value rewrite
    // that must happen FIRST, with the width derived from the REWRITTEN value. Deriving the width
    // before normalizing is exactly the bug the implementation's `write_float` special-cases against
    // (a payload-carrying NaN's smallest lossless width is 8 bytes, so the canonical NaN would be
    // written at 0xfb). The vectors below pin it from every side: an already-quiet NaN at 0xfa and at
    // 0xfb, a PAYLOAD-carrying NaN at each of the two widths its payload can occupy (0xfb, where the
    // payload forces the wide head, and 0xf9, where it does not — so the width rule alone changes
    // nothing and only the value rewrite can), and a NEGATIVE NaN, whose SIGN the rewrite drops too.
    // Their preserve-identity
    // halves are the same inputs' twins in tests/golden_hex_preserve/tests.rs, which must NOT
    // normalize — so a serializer that normalized in both profiles fails there, and one that
    // normalized in neither fails here.
    kat_canonical!(
        canon_floats_nan_f32_normalized,
        Floats,
        &[0x82, 0xfa, 0x7f, 0xc0, 0x00, 0x00, 0xf9, 0x3c, 0x00],
        &[0x82, 0xf9, 0x7e, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.sgl, 1.0);
        }
    );
    kat_canonical!(
        canon_floats_nan_f64_normalized,
        Floats,
        &[
            0x82, 0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf9, 0x3c, 0x00
        ],
        &[0x82, 0xf9, 0x7e, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.sgl, 1.0);
        }
    );
    // The payload case: mantissa 0x8000000000001, whose low bit fits no narrower mantissa. Its
    // shortest LOSSLESS head is 0xfb, so a width-only canonicalizer leaves the input untouched;
    // §4.2.2 requires f9 7e00. The anchor asserts the payload bits are genuinely present after
    // decoding, so the vector cannot pass by having lost them on the way in.
    kat_canonical!(
        canon_floats_nan_payload_normalized,
        Floats,
        &[
            0x82, 0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xf9, 0x3c, 0x00
        ],
        &[0x82, 0xf9, 0x7e, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.dbl.to_bits(), 0x7ff8_0000_0000_0001u64);
        }
    );
    // The payload case whose payload ALREADY fits the canonical width. `f9 7e01` (f64 form
    // 0x7ff8040000000000, mantissa 0x201 << 42) has shortest-lossless width 0xf9 — so a width-only
    // canonicalizer leaves it byte-identical and passes every OTHER canonical float vector. Only the
    // §4.2.2 value rewrite takes it to f9 7e00. Its preserve twin (floats_nan_payload_f16) keeps the
    // payload, so the pair also pins that the rewrite is canonical-only.
    kat_canonical!(
        canon_floats_nan_payload_f16_normalized,
        Floats,
        &[0x82, 0xf9, 0x7e, 0x01, 0xf9, 0x3c, 0x00],
        &[0x82, 0xf9, 0x7e, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan());
            assert_eq!(d.dbl.to_bits(), 0x7ff8_0400_0000_0000u64);
        }
    );
    // The SIGN half of §4.2.2, which no other vector reaches: the canonical NaN is the POSITIVE
    // zero-payload quiet NaN, so a negative NaN (`fb fff8000000000000`) canonicalizes to f9 7e00 —
    // the sign is dropped along with the width. This is the one place canonicalization discards a
    // float's sign bit; canon_floats_negative_zero pins the converse for -0.0, whose sign SURVIVES
    // (§4.2 minimizes the encoding, and only the NaN rule changes a value). A canonicalizer that
    // normalized the payload but carried the sign through would emit f9 fe00 here.
    kat_canonical!(
        canon_floats_nan_negative_normalized,
        Floats,
        &[
            0x82, 0xfb, 0xff, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf9, 0x3c, 0x00
        ],
        &[0x82, 0xf9, 0x7e, 0x00, 0xf9, 0x3c, 0x00],
        |d: &Floats| {
            assert!(d.dbl.is_nan() && d.dbl.is_sign_negative());
            assert_eq!(d.dbl.to_bits(), 0xfff8_0000_0000_0000u64);
        }
    );
    // A FIXED float member (`fixed_float = [v: 1.5]`): the value is spec-pinned, so the width is all
    // canonicalization can touch — 0xfb 3ff8000000000000 minimizes to f9 3e00 (§A). The struct has no
    // value field, so there is nothing to anchor; the decode half is pinned by
    // fixed_float_wrong_value_rejected in tests/golden_hex_preserve/tests.rs (the constant IS
    // compared, so acceptance is a value assertion).
    kat_canonical!(
        canon_fixed_float_width_minimized,
        FixedFloat,
        &[0x81, 0xfb, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        &[0x81, 0xf9, 0x3e, 0x00],
        |_d: &FixedFloat| {}
    );
    // Per-element minimization inside one array: the same value at three different widths, in
    // indefinite framing, becomes a definite array of three identical minimal heads (§4.2.1 + the
    // §4.2.2-adjacent definite re-framing). Each element minimizes INDEPENDENTLY, so a single width
    // recorded for the whole array — or one carried over from the first element — cannot produce this.
    kat_canonical!(
        canon_float_seq_per_element,
        FloatHolder,
        &[
            0x81, 0x9f, 0xf9, 0x3c, 0x00, 0xfa, 0x3f, 0x80, 0x00, 0x00, 0xfb, 0x3f, 0xf0, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0xff
        ],
        &[
            0x81, 0x83, 0xf9, 0x3c, 0x00, 0xf9, 0x3c, 0x00, 0xf9, 0x3c, 0x00
        ],
        |d: &FloatHolder| {
            assert_eq!(d.fs, vec![1.0, 1.0, 1.0]);
        }
    );
}
