// Wire vectors for a rule cycle entered at the collection rule.
//
// Each `<x>_holder` rooted at the collection leaves its union rule holding a NOMINAL reference to a
// collection typedef; the `res_` block is the same shape rooted at the union, which resolves the
// reference through the alias table instead. Both must decode the same bytes and re-emit them
// byte-exact — that equivalence is the whole claim, so the vectors are byte literals rather than
// constructor round-trips: a constructor round-trip cannot tell "wrote a map" from "wrote something
// that happens to decode again".
#[cfg(test)]
mod recursive_collection_ref {
    use super::*;
    use serialization::Deserialize;

    // `[ {"a": {"b": 2}} ]` — the value arm recurses back into the table, which is the reference
    // that stays nominal. Two levels deep so the recursion is exercised, not just entered.
    const NESTED_TABLE: &[u8] = &[
        0x81, 0xa1, 0x61, 0x61, 0xa1, 0x61, 0x62, 0x02,
    ];

    #[test]
    fn nominal_table_reference_round_trips() {
        let h = NomHolder::from_cbor_bytes(NESTED_TABLE).unwrap();
        assert_eq!(h.nom_map.len(), 1, "outer table must decode one entry");
        assert!(
            matches!(h.nom_map.get("a"), Some(NomVal::NomMap { .. })),
            "the recursive value arm must decode as the table arm, not fall through to `int`"
        );
        assert_eq!(h.to_cbor_bytes(), NESTED_TABLE);
    }

    #[test]
    fn resolved_alias_reference_decodes_identically() {
        // The control spelling: identical rules, rooted at the union so the reference resolves
        // through the alias table. It compiled before the nominal path did; if a future change
        // re-resolves or un-nominalizes references, this is the side that must not move.
        let h = ResHolder::from_cbor_bytes(NESTED_TABLE).unwrap();
        assert!(
            matches!(h.res_val, ResVal::ResMap { .. }),
            "the control's union must decode the map arm"
        );
        assert_eq!(
            h.to_cbor_bytes(),
            NomHolder::from_cbor_bytes(NESTED_TABLE).unwrap().to_cbor_bytes(),
            "both reference paths must emit the same bytes for the same wire input"
        );
    }

    #[test]
    fn nominal_reference_still_dispatches_the_other_arm() {
        // `[ 1 ]` — the union's `int` arm. The table arm's cbor_types answer (CBOR map) is what
        // keeps the two arms distinguishable, so a wrong answer there shows up as a misdispatch
        // rather than as a compile error.
        let h = NomHolder::from_cbor_bytes(&[0x81, 0xa1, 0x61, 0x61, 0x01]).unwrap();
        assert!(
            matches!(h.nom_map.get("a"), Some(NomVal::Int { .. })),
            "a uint value must take the `int` arm"
        );
    }

    #[test]
    fn nominal_array_reference_round_trips() {
        // `[ [1, [2]] ]` — the `RustStructType::Array` flavor of the same class.
        let bytes: &[u8] = &[0x81, 0x82, 0x01, 0x81, 0x02];
        let h = ArrHolder::from_cbor_bytes(bytes).unwrap();
        assert_eq!(h.arr_list.len(), 2);
        assert!(
            matches!(h.arr_list[1], ArrVal::ArrList { .. }),
            "the nested array must decode as the list arm"
        );
        assert_eq!(h.to_cbor_bytes(), bytes);
    }

    #[test]
    fn nominal_reference_carries_duplicates_preserve() {
        // `[ {"a": 1, "a": 2} ]` — duplicate keys. The per-rule `@duplicates preserve` policy lives
        // on the STRUCT, and a nominal reference carries none of it, so emission has to read it
        // back off the struct. If it does not, the loose-table code collapses the two entries and
        // re-emits a 1-entry map (`0xa1 …`) — which this byte comparison catches and a
        // decode-succeeds assertion would not.
        let bytes: &[u8] = &[0x81, 0xa2, 0x61, 0x61, 0x01, 0x61, 0x61, 0x02];
        let h = PresHolder::from_cbor_bytes(bytes).unwrap();
        assert_eq!(h.pres_map.len(), 2, "both duplicate-keyed entries must survive");
        assert_eq!(h.to_cbor_bytes(), bytes);
    }

    #[test]
    fn nominal_reference_carries_occurrence_bounds() {
        // The `{+ }` twin: the bounds ride the struct the same way the duplicates policy does, and
        // select `NonEmptyMap` — whose single `TryFrom` door is what rejects the empty map.
        let ok: &[u8] = &[0x81, 0xa1, 0x61, 0x61, 0x01];
        let h = NemHolder::from_cbor_bytes(ok).unwrap();
        assert_eq!(h.to_cbor_bytes(), ok);
        assert!(
            NemHolder::from_cbor_bytes(&[0x81, 0xa0]).is_err(),
            "an empty `{{+ k => v}}` table must be rejected, not decoded as empty"
        );
    }
}
