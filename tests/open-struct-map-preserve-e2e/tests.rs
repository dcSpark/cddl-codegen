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

    // --- the `any` KEY domain crossed with @duplicates preserve (dup_any_rest) ---
    // The rules above cover the two halves separately: `dup_pair` pairs `preserve` with a CONCRETE
    // uint key domain, `open_any` pairs the `any` key domain with the default REJECT (see
    // dup_rejected_any_domain, where 0x05 and 0x1805 are refused as the same key). Here both apply
    // at once, so the rest field is a PairMap<AnyCbor, AnyCbor>: duplicate keys must be KEPT, and a
    // key may be any CBOR item at all rather than a uint.

    #[test]
    fn dup_any_keeps_duplicate_and_composite_keys_byte_exact() {
        // {"name": "x", 5: "a", 5: "b", [1]: 0} — three rest entries: a duplicated uint key (which
        // dup_rejected_any_domain shows the REJECT container refuses) and an ARRAY key, which no
        // concrete-domain rule can hold at all. All three survive, in wire order.
        let wire = bytes("a4 646e616d65 6178 05 6161 05 6162 8101 00");
        let v = DupAnyRest::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.name, "x", "declared text member decoded");
        assert_eq!(
            v.rest.len(),
            3,
            "duplicate uint keys AND the composite array key all captured"
        );
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact with duplicate + composite any keys");
        // RFC 8949 gives duplicate-keyed data no canonical form, so the pair-map's deterministic
        // best-effort is pinned as a FIXED POINT rather than against hand-derived bytes: the
        // canonical output must re-parse and re-canonicalize to itself.
        let canon = v.to_canonical_cbor_bytes();
        let re = DupAnyRest::from_cbor_bytes(&canon).unwrap();
        assert_eq!(re.rest.len(), 3, "canonical output keeps every entry");
        assert_eq!(
            re.to_canonical_cbor_bytes(),
            canon,
            "canonical of canonical == canonical"
        );
    }

    #[test]
    fn dup_any_indefinite_framing_and_dup_keys_byte_exact() {
        // {_ "name": "x", 24(two-byte): "a", 24(two-byte): h'f5f4f6f5f5' _} — indefinite MAP framing
        // around a duplicated non-minimal key, with a byte-string value whose content is itself a run
        // of major-7 head bytes (0xf5/0xf4/0xf6). The framing, both key widths, and the payload must
        // all replay verbatim; a reader that mistook a payload 0xf5 for a value would desync.
        let wire = bytes("bf 646e616d65 6178 1818 6161 1818 45f5f4f6f5f5 ff");
        let v = DupAnyRest::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.rest.len(), 2, "both key-24 entries kept");
        assert_eq!(v.to_cbor_bytes(), wire, "indefinite framing + dup keys byte-exact");
    }

    #[test]
    fn dup_any_float_junk_values_byte_exact_and_canonical() {
        // Float VALUES in the any domain, one per interesting float class: 1.0 at the widest head
        // (RFC 8949 §A: fb 3ff0000000000000), a payload-carrying NaN (fb 7ff8000000000001), and -0.0
        // already minimal (f9 8000). Preserve replays every head width verbatim; canonical applies
        // the three different §4.2 rules — 1.0 SHRINKS to f9 3c00, the NaN NORMALIZES to f9 7e00
        // (§4.2.2, payload dropped), and -0.0 KEEPS its sign at f9 8000 (§4.2 minimizes the encoding,
        // not the value). The canonical assertion is by containment because duplicate-capable
        // key ordering is the pair-map's own best-effort, pinned above.
        let wire = bytes(
            "a4 646e616d65 6178 01 fb3ff0000000000000 02 fb7ff8000000000001 03 f98000",
        );
        let v = DupAnyRest::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.rest.len(), 3);
        assert_eq!(v.to_cbor_bytes(), wire, "float junk values replay at their wire widths");
        let canon = v.to_canonical_cbor_bytes();
        for (needle, why) in [
            (bytes("f93c00"), "1.0 shrinks to the f16 head"),
            (bytes("f97e00"), "the NaN payload is dropped (§4.2.2)"),
            (bytes("f98000"), "-0.0 keeps its sign"),
        ] {
            assert!(
                canon.windows(needle.len()).any(|w| w == &needle[..]),
                "canonical output must contain {needle:02x?}: {why}"
            );
        }
    }
}

// TYPED key domains on a rest row (`* K => V` where `K` is not bare `uint`/`text`/`any`) crossed with
// the preserve/canonical core. What is new here versus the peeked domains above: the key's encodings
// come from `K::deserialize` itself rather than from the record loop's peek, so a sidecar-bearing `K`
// (`bytes`) only replays byte-exactly if those vars reach `rest_key_encodings`; and a self-carrying
// `K` (a union) contributes no key sidecar at all, which is what makes the two shapes worth pinning
// side by side. Every `wire` byte string is hand-written from the CBOR grammar.
#[cfg(test)]
mod open_struct_map_typed_preserve {
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
    fn typed_interleave_byte_exact() {
        // typed_open { 1: uint, "nm": text, * md => uint }.
        // { 1: 5(two-byte), "zz": 9, "nm": "hi" } — the typed rest entry sits BETWEEN the two declared
        // keys, so wire order is only reproducible through `orig_deser_order`'s N + i index space.
        let wire = bytes("a3 01 190005 627a7a 09 626e6d 626869");
        let v = TypedOpen::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.rest.len(), 1);
        assert_eq!(*v.rest.get(&Md::new_text("zz".to_string())).unwrap(), 9);
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact typed interleave");
    }

    #[test]
    fn typed_key_sidecar_byte_exact() {
        // typed_bytes { 3: uint, * bstr => text }. The key h'ab' is written with a NON-minimal length
        // header (0x58 0x01 rather than 0x41), which only replays if the `StringEncoding` the key's
        // own deserialize produced reached `rest_key_encodings` — the peeked path's `key_enc` does not
        // exist on this row.
        let wire = bytes("a2 0300 5801ab 617a");
        let v = TypedBytes::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.rest.len(), 1);
        assert_eq!(v.rest.get(&vec![0xab]).unwrap(), "z");
        assert_eq!(v.to_cbor_bytes(), wire, "typed key encoding sidecar byte-exact");
        // Canonical rewrites the key head minimally (the merge writes with default encodings).
        assert_eq!(v.to_canonical_cbor_bytes(), bytes("a2 03 00 41ab 617a"));
    }

    #[test]
    fn typed_dup_rejected_on_value_equality() {
        // The loose typed container's `Eq` is VALUE equality (the preserve derives ignore encoding
        // members), so rest keys 0x05 and 0x1805 — the same `Md::Int(Int::Uint(5))` at two wire
        // widths — are a DuplicateKey, exactly as for the concrete uint domain.
        let wire = bytes("a4 0100 626e6d 6161 05 01 1805 02");
        assert!(
            TypedOpen::from_cbor_bytes(&wire).is_err(),
            "0x05 vs 0x1805 are one typed key"
        );
    }

    #[test]
    fn typed_canonical_merge_orders_typed_keys_against_declared() {
        // typed_open, non-canonical input { "nm": "hi", 1: 0, "zz": 9, -3: 4 }. The runtime merge
        // serializes each key (declared AND typed rest) and sorts length-first, then bytewise:
        // 0x01 and 0x22 (one byte) before 0x626e6d and 0x627a7a (three).
        let v =
            TypedOpen::from_cbor_bytes(&bytes("a4 626e6d 626869 01 00 627a7a 09 22 04")).unwrap();
        assert_eq!(v.rest.len(), 2, "a nint and a text rest key");
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            bytes("a4 01 00 22 04 626e6d 626869 627a7a 09"),
            "typed rest keys sort among the declared keys length-first"
        );
    }

    #[test]
    fn typed_dup_pairlist_keeps_duplicates_byte_exact() {
        // typed_dup { 9: uint, * md => uint ; @duplicates preserve }: { 9: 0, 5: 1, 5(two-byte): 2 }.
        // Both entries are the same `Md` VALUE, which the loose container above refuses; the pair-map
        // keeps both, in wire order, and the POSITIONAL value sidecar keeps each value width aligned.
        // The key widths ride `Md`'s own encodings (a union self-carries — there is no key sidecar).
        let wire = bytes("a3 0900 05 01 1805 1802");
        let v = TypedDup::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.rest.len(), 2, "both duplicate typed keys survive");
        assert_eq!(
            v.rest.get_all(&Md::new_int(Int::new_uint(5))),
            vec![&1u64, &2u64]
        );
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact typed duplicates");
    }
}
