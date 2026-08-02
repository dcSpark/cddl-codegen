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
    fn nominal_union_keyed_table_round_trips() {
        // `[ { {1: 2}: 3 } ]` — the map's DOMAIN is the recursive union, so the OUTER key is itself
        // a table. The keys-list wasm wrapper is minted from the domain, which is where this shape
        // used to abort generation entirely; rust-side the payoff is that the union must dispatch
        // its own table arm from the KEY position, not just the value position the `nom_` block
        // covers. Byte comparison rather than a decode assertion: a domain lowered to the wrong
        // collection still decodes and re-emits differently.
        let bytes: &[u8] = &[0x81, 0xa1, 0xa1, 0x01, 0x02, 0x03];
        let h = KeyHolder::from_cbor_bytes(bytes).unwrap();
        assert_eq!(h.key_map.len(), 1, "outer table must decode one entry");
        let (k, v) = h.key_map.iter().next().unwrap();
        assert!(
            matches!(k, KeyVal::KeyMap { .. }),
            "the KEY must decode as the table arm, not fall through to bytes/text"
        );
        assert!(
            matches!(v, KeyVal::Int { .. }),
            "the value must still dispatch the int arm"
        );
        assert_eq!(h.to_cbor_bytes(), bytes);
    }

    #[test]
    fn union_rooted_union_keyed_table_round_trips() {
        // The same `[ { {1: 2}: 3 } ]` payload through the UNION-rooted ordering of the union-keyed
        // table: the cycle is rooted so the table registers before its named domain exists, which is
        // the ordering whose keys-list wrapper can only be named after the union does. The rooting
        // must not be observable on the wire, so the claim is byte equality with the
        // collection-rooted twin above, not merely a successful round-trip.
        let bytes: &[u8] = &[0x81, 0xa1, 0xa1, 0x01, 0x02, 0x03];
        //
        // Arm assertions are `matches!` with `{ .. }` and the entry counts are read off the emitted
        // HEAD byte, because the two profiles give the union different variant shapes (preserve
        // carries per-variant encoding sidecars, so the same variant is a struct variant there) —
        // one destructuring spelling cannot serve both, and the head byte is what the wire claim is
        // about anyway.
        let h = UkeyHolder::from_cbor_bytes(bytes).unwrap();
        assert!(
            matches!(h.ukey_val, UkeyVal::UkeyMap { .. }),
            "the union-rooted holder's union must decode the table arm"
        );
        assert_eq!(
            h.ukey_val.to_cbor_bytes()[0],
            0xa1,
            "the outer table must re-emit as a 1-entry map"
        );
        assert_eq!(h.to_cbor_bytes(), bytes);
        assert_eq!(
            h.to_cbor_bytes(),
            KeyHolder::from_cbor_bytes(bytes).unwrap().to_cbor_bytes(),
            "both rootings of the union-keyed table must emit the same bytes for the same wire input"
        );
    }

    #[test]
    fn union_rooted_preserve_table_carries_duplicate_union_keys() {
        // `[ { {1: 2}: 3, {1: 2}: 4 } ]` — the consumer's CIP-25 shape: duplicate keys that are
        // THEMSELVES tables, under a rule-position `@duplicates preserve` on a union-rooted cycle.
        // Recursion in the DOMAIN and the per-rule policy have to compose: a loose container would
        // collapse the two entries and re-emit a 1-entry map (`0xa1 …`), which the byte comparison
        // catches and a decode-succeeds assertion would not.
        let bytes: &[u8] = &[
            0x81, 0xa2, 0xa1, 0x01, 0x02, 0x03, 0xa1, 0x01, 0x02, 0x04,
        ];
        let h = UpresHolder::from_cbor_bytes(bytes).unwrap();
        assert!(
            matches!(h.upres_val, UpresVal::UpresMap { .. }),
            "the preserve table's union must decode the pair-map arm"
        );
        assert_eq!(
            h.upres_val.to_cbor_bytes()[0],
            0xa2,
            "both duplicate table-KEYED entries must survive — a collapsed container re-emits 0xa1"
        );
        assert_eq!(h.to_cbor_bytes(), bytes);
    }

    #[test]
    fn union_rooted_preserve_union_dispatches_its_array_arm() {
        // `[ [ 1 ] ]` — the same union carries an ARRAY arm beside the table arm (`md_map / [* md] /
        // …`), so the two container arms must stay distinguishable by cbor_type. A domain-driven
        // mint that disturbed the union's dispatch would show up here as a misdispatch.
        let bytes: &[u8] = &[0x81, 0x81, 0x01];
        let h = UpresHolder::from_cbor_bytes(bytes).unwrap();
        assert!(
            matches!(h.upres_val, UpresVal::ArrUpresVal { .. }),
            "an array payload must take the array arm, not the pair-map arm"
        );
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
