// Golden known-answer vectors for --canonical-form. Every `canonical` byte string below is
// hand-derived from RFC 8949 §4.2.1 (smallest-argument "preferred serialization") and §4.2.2
// (indefinite-length items re-framed definite), NOT copied from generator output and NOT built
// with the tests/deser_test cbor_event helpers: the canonical suite's other expected bytes come
// from the same write_*_sz/Sz::canonical layer the generated code encodes with, so a symmetric
// bug there passes every one of those assertions. Raw spec-anchored hex is the independent check.
//
// Each vector decodes an IRREGULAR (but valid) encoding and asserts three things:
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
        ($name:ident, $t:ty, $irregular:expr, $canonical:expr) => {
            #[test]
            fn $name() {
                let irregular: &[u8] = $irregular;
                let canonical: &[u8] = $canonical;
                let decoded = <$t>::from_cbor_bytes(irregular).unwrap();
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
        &[0x82, 0x17, 0x18, 0x18]
    );
    // Boundary above the 1-byte argument: 255 (4-byte form) shrinks to 0x18 0xff; 256 already
    // needs the 2-byte argument and must stay 0x19 0x01 0x00.
    kat_canonical!(
        canon_pair_boundary_255_256,
        Pair,
        &[0x82, 0x1a, 0x00, 0x00, 0x00, 0xff, 0x19, 0x01, 0x00],
        &[0x82, 0x18, 0xff, 0x19, 0x01, 0x00]
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
        &[0x82, 0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00]
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
        &[0x82, 0x01, 0x02]
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
        ]
    );
    // Indefinite map with entries reordered (b before a) -> definite map(2) with keys in
    // canonical order: "a"/"b" encode to equal-length 0x6161/0x6162, so length-first and bytewise
    // ordering agree here — a comes first. (WHICH rule the generator implements is pinned by the
    // discriminating pair in tests/canonical mixed_len_keys_ordering_rule, not re-tested here.)
    kat_canonical!(
        canon_kv_sorted,
        Kv,
        &[0xbf, 0x61, 0x62, 0x02, 0x61, 0x61, 0x01, 0xff],
        &[0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02]
    );
    // homogeneous array: indefinite framing + non-minimal elements -> definite minimal.
    kat_canonical!(
        canon_seq,
        SeqHolder,
        &[0x81, 0x9f, 0x18, 0x01, 0x19, 0x00, 0x02, 0x03, 0xff],
        &[0x81, 0x83, 0x01, 0x02, 0x03]
    );
    // tag 11 in a 2-byte argument + indefinite body -> minimal tag head 0xcb + definite array
    // (§4.2.1's smallest-argument rule covers tag heads too).
    kat_canonical!(
        canon_tagged,
        TaggedOne,
        &[0xd9, 0x00, 0x0b, 0x9f, 0x18, 0x05, 0xff],
        &[0xcb, 0x81, 0x05]
    );
}
