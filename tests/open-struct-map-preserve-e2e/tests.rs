// Open struct-map (loose CBOR "rest row") PRESERVE fidelity vectors. These pin the
// preserve/canonical core: wire-position (interleave) fidelity, per-entry encoding sidecars for
// concrete key/value domains, the value-duplicate rejection (keyed on CBOR VALUE equality) under both key domains, and the
// runtime canonical key merge (with codegen<->runtime comparator agreement). Every `wire` byte
// string is hand-written from the CBOR grammar, not copied from generator output.
#[cfg(test)]
mod open_struct_map_preserve {
    use super::*;
    use serialization::{Deserialize, Serialize};

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    #[test]
    fn interleave_byte_exact() {
        // {1: 5(two-byte), "x": 9(two-byte), 2: "hi"} — the rest entry "x" sits BETWEEN declared
        // keys 1 and 2 on the wire, with non-minimal widths. Preserve must reproduce the exact
        // bytes AND the exact wire order (orig_deser_order interleave, N + i index space).
        let wire = bytes("a3 01 190005 6178 190009 02 626869");
        let v = OpenAny::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.rest.len(), 1);
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact interleave round-trip");
    }

    #[test]
    fn concrete_key_and_value_sidecars_byte_exact() {
        // { 4: 0, 7(two-byte key): "z" } — concrete uint key (non-minimal 0x1807) + concrete text
        // value. Both encodings must ride the rest_key_encodings / rest_value_encodings sidecars.
        let wire = bytes("a2 04 00 1807 617a");
        let v = Concrete::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.to_cbor_bytes(), wire, "concrete key+value sidecar byte-exact");
    }

    #[test]
    fn dup_rejected_concrete_uint_domain() {
        // * uint => any: keys 0x01 and 0x1801 both decode to uint 1 -> DuplicateKey (the concrete
        // u64 container's Eq IS value equality).
        let wire = bytes("a3 0300 01 6161 1801 6162");
        assert!(UintAny::from_cbor_bytes(&wire).is_err(), "0x01 vs 0x1801 dup under * uint => any");
    }

    #[test]
    fn dup_rejected_any_domain() {
        // * any => any: rest uint keys 0x05 and 0x1805 both decode to uint 5 -> DuplicateKey via the
        // value_eq side scan (the preserve AnyCbor container's Eq is REPRESENTATIONAL, so a plain
        // insert would accept both). Reject is a function of the wire value, not the domain's spelling.
        let wire = bytes("a3 0100 05 6161 1805 6162");
        assert!(OpenAny::from_cbor_bytes(&wire).is_err(), "0x05 vs 0x1805 dup under * any => any");
    }

    #[test]
    fn empty_rest_canonical_equals_closed_bytes() {
        // No captured entries: canonical output must match the closed encoding (empty-rest ≡ closed).
        let v = OpenAny::from_cbor_bytes(&bytes("a1 0105")).unwrap();
        assert!(v.rest.is_empty());
        assert_eq!(v.to_canonical_cbor_bytes(), bytes("a1 0105"), "empty-rest canonical ≡ closed");
    }

    #[test]
    fn canonical_merge_length_first_interleaves_declared_and_rest() {
        // div_open {24: uint, * uint => any}: input non-canonical {24:100, 10:99}. Canonical must
        // sort length-first: 10 (0x0a, len 1) BEFORE 24 (0x1818, len 2) though 0x18 > 0x0a bytewise.
        let v = DivOpen::from_cbor_bytes(&bytes("a2 1818 1864 0a 1863")).unwrap();
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            bytes("a2 0a 1863 1818 1864"),
            "runtime canonical merge orders a rest key among declared keys"
        );
    }

    #[test]
    fn canonical_codegen_runtime_agreement_divergence_vector() {
        // The shared length-first comparator must make an OPEN struct's runtime key merge agree with
        // the CLOSED struct's codegen-time baked order for the SAME keys (24 = 0x1818 vs 10 = 0x0a —
        // the shorter encoding is bytewise-greater, so pure-bytewise would disagree).
        let closed = DivClosed::from_cbor_bytes(&bytes("a2 1818 04 0a 03")).unwrap();
        assert_eq!(closed.to_canonical_cbor_bytes(), bytes("a2 0a 03 1818 04"), "closed bakes 10 before 24");
        let open = DivOpen::from_cbor_bytes(&bytes("a2 1818 04 0a 03")).unwrap();
        assert_eq!(
            open.to_canonical_cbor_bytes(),
            closed.to_canonical_cbor_bytes(),
            "open runtime merge agrees with closed baked order"
        );
    }

    // --- @duplicates preserve: the vec-of-pairs (PairMap) twin ---

    #[test]
    fn dup_pairlist_keeps_duplicate_keys_byte_exact() {
        // dup_pair {9: uint, * uint => any @duplicates preserve}: {9: 0, 0x01: "a", 0x1801: "b"} —
        // TWO rest entries both uint 1 (the DEFAULT-reject container would refuse this pair). The
        // pair-list keeps BOTH, in wire order, byte-exact incl. the non-minimal 0x1801 key width.
        let wire = bytes("a3 0900 01 6161 1801 6162");
        let v = DupPair::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.rest.len(), 2, "both duplicate keys survive");
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact with duplicate keys present");
    }

    #[test]
    fn dup_pairlist_concrete_positional_sidecars_byte_exact() {
        // dup_pair_concrete {5: uint, * uint => text @duplicates preserve}: dup key 7 (minimal +
        // two-byte) with concrete text values — the POSITIONAL Vec encoding sidecars keep each
        // entry's key+value widths aligned (a keyed sidecar would collide on the repeated key).
        let wire = bytes("a3 0500 07 6161 1807 6162");
        let v = DupPairConcrete::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.rest.len(), 2);
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact dup concrete via positional sidecars");
    }

    #[test]
    fn dup_pairlist_canonical_stable_sort_keeps_wire_order() {
        // Canonical re-encodes both key-1 entries minimal, sorts key 1 before key 9, and the STABLE
        // sort keeps the duplicates in their wire order (a before b) — RFC 8949 has no canonical form
        // for duplicate keys, so this is the deterministic best-effort the pair-map twin defines.
        let wire = bytes("a3 0900 01 6161 1801 6162");
        let v = DupPair::from_cbor_bytes(&wire).unwrap();
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            bytes("a3 01 6161 01 6162 09 00"),
            "canonical: dup keys minimal, sorted, wire order preserved"
        );
    }

    #[test]
    fn dup_pairlist_empty_equals_closed() {
        let wire = bytes("a1 0900");
        let v = DupPair::from_cbor_bytes(&wire).unwrap();
        assert!(v.rest.is_empty());
        assert_eq!(v.to_cbor_bytes(), wire, "empty pair-list ≡ closed");
    }
}
