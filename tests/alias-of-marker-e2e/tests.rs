// The alias-of-marker custom pair, executed: what "this rule IS that type, written differently on the
// wire" buys once the wire is a hex-text rendering of a `_CDDL_CODEGEN_RAW_BYTES_TYPE_`.
//
// Every vector below is a property of the COMPOSITION, not of either half: the alias resolves to the
// marker's type (so the codecs take `PolicyId`), while the pair owns the wire at all three positions
// the alias reaches (a record field, a table key, a table value). The encoding-variable signature is
// inferred from the replaced type — a raw-bytes marker demands one `StringEncoding` — which is what
// gives the custom TEXT header a sidecar slot to be recorded in.
//
// Each CBOR item is written from the grammar (the headers are literal bytes with their meaning beside
// them); the 56-character payloads are assembled rather than spelled as hex so the text the codec
// writes is readable as text.
#[cfg(test)]
mod alias_of_marker {
    use super::*;
    use serialization::{Deserialize, Serialize};

    // The four policy ids, as they appear ON THE WIRE (hex text, reversed-byte rendering). The
    // DECODED value of each is that text's bytes, reversed — asserted below, which is what pins that
    // the reader is the writer's inverse rather than a second convention.
    //
    // `A_TEXT`/`B_TEXT` are the canonical-ordering pair: written, A < B ("00…" < "02…"); DECODED,
    // B < A (`00…02` < `01…00`). The two orders disagree, so a canonical sort keyed on the decoded
    // bytes would produce a different map order than one keyed on the bytes the custom codec wrote.
    const P_TEXT: &str = "bbaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    const A_TEXT: &str = "00000000000000000000000000000000000000000000000000000001";
    const B_TEXT: &str = "02000000000000000000000000000000000000000000000000000000";
    const V_TEXT: &str = "eedddddddddddddddddddddddddddddddddddddddddddddddddddddd";

    // text(56) headers. `0x78 0x38` is the MINIMAL form (56 > 23 forces one following byte); the
    // four-byte-following `0x7a 00 00 00 38` is genuinely NON-MINIMAL, which is the whole point of a
    // preserve round trip through a custom codec — the width is the codec's own wire framing, and it
    // survives only because the replaced type's inferred `StringEncoding` slot records it.
    const MINIMAL_HEAD: &[u8] = &[0x78, 0x38];
    const NON_MINIMAL_HEAD: &[u8] = &[0x7a, 0x00, 0x00, 0x00, 0x38];

    fn text_item(head: &[u8], text: &str) -> Vec<u8> {
        let mut item = head.to_vec();
        item.extend_from_slice(text.as_bytes());
        item
    }

    /// The decoded value behind a wire text: its bytes, reversed.
    fn decoded(text: &str) -> PolicyId {
        let mut bytes: Vec<u8> = (0..text.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&text[i..i + 2], 16).unwrap())
            .collect();
        bytes.reverse();
        PolicyId::from_raw_bytes(&bytes).unwrap()
    }

    /// ```text
    /// 83                          array(3)
    ///   7a 00 00 00 38 <P_TEXT>   p:  text(56), NON-MINIMAL four-byte-following header
    ///   a2                        t:  map(2), written in DECODED-bytes order (B before A)
    ///     78 38 <B_TEXT>  02        key B (minimal header — the control) => uint 2
    ///     7a 00 00 00 38 <A_TEXT>   key A (non-minimal header)
    ///     01                        => uint 1
    ///   a1                        v:  map(1), the custom pair in the table VALUE position
    ///     05                        key uint 5
    ///     78 38 <V_TEXT>            => value (minimal header)
    /// ```
    fn wire() -> Vec<u8> {
        let mut wire = vec![0x83];
        wire.extend(text_item(NON_MINIMAL_HEAD, P_TEXT));
        wire.push(0xa2);
        wire.extend(text_item(MINIMAL_HEAD, B_TEXT));
        wire.push(0x02);
        wire.extend(text_item(NON_MINIMAL_HEAD, A_TEXT));
        wire.push(0x01);
        wire.push(0xa1);
        wire.push(0x05);
        wire.extend(text_item(MINIMAL_HEAD, V_TEXT));
        wire
    }

    #[test]
    fn the_alias_decodes_to_the_markers_type_through_the_custom_wire() {
        let v = Holder::from_cbor_bytes(&wire()).unwrap();
        // `policy_id_v1`/`policy_id_v1_entry` ARE `policy_id` — the alias resolved to the marker's
        // type, so these comparisons are against the hand-written extern, not against some minted
        // wrapper. The alias names no RUST type to annotate with (a pair-carrying alias mints none,
        // so its name cannot carry a standalone codec contradicting the custom wire below), so the
        // annotation spells what it resolves to.
        let p: PolicyId = v.p;
        assert_eq!(p, decoded(P_TEXT));
        assert_eq!(v.t.get(&decoded(A_TEXT)).copied(), Some(1));
        assert_eq!(v.t.get(&decoded(B_TEXT)).copied(), Some(2));
        assert_eq!(v.v.get(&5).copied(), Some(decoded(V_TEXT)));
        // …and the values are genuinely reversed renderings, not the text's bytes in order.
        assert_eq!(p.to_raw_bytes()[27], 0xbb);
        assert_eq!(p.to_raw_bytes()[0], 0xaa);
    }

    #[test]
    fn a_non_minimal_text_header_round_trips_byte_exact() {
        let wire = wire();
        let v = Holder::from_cbor_bytes(&wire).unwrap();
        // Both non-minimal headers (the record field's and one table key's) and both minimal controls
        // replay exactly, at all three positions the alias reaches.
        assert_eq!(
            v.to_cbor_bytes(),
            wire,
            "byte-exact round trip through the alias-of-marker custom wire"
        );
    }

    #[test]
    fn the_sidecar_is_keyed_by_the_decoded_key() {
        let mut v = Holder::from_cbor_bytes(&wire()).unwrap();
        let encs = v
            .encodings
            .as_mut()
            .expect("preserve records an encoding struct");
        // The record field's slot exists because the REPLACED type (a raw-bytes marker) demands one
        // `StringEncoding`; it holds the width the custom TEXT header actually used.
        assert_eq!(
            encs.p_encoding,
            StringEncoding::Definite(cbor_event::Sz::Four)
        );
        // Every per-entry lookup is by the DECODED key — the `PolicyId` the custom reader returned,
        // never the 56-character text it consumed.
        assert_eq!(
            encs.t_key_encodings.get(&decoded(A_TEXT)).cloned(),
            Some(StringEncoding::Definite(cbor_event::Sz::Four))
        );
        assert_eq!(
            encs.t_key_encodings.get(&decoded(B_TEXT)).cloned(),
            Some(StringEncoding::Definite(cbor_event::Sz::One))
        );
        assert_eq!(
            encs.v_value_encodings.get(&5).cloned(),
            Some(StringEncoding::Definite(cbor_event::Sz::One))
        );
        // Writing a NEW encoding under the decoded key is the executed half of the same claim: the
        // replay changes, so the write path looked the entry up under exactly that key.
        encs.t_key_encodings.insert(
            decoded(B_TEXT),
            StringEncoding::Definite(cbor_event::Sz::Four),
        );
        let mut expected = vec![0x83];
        expected.extend(text_item(NON_MINIMAL_HEAD, P_TEXT));
        expected.push(0xa2);
        expected.extend(text_item(NON_MINIMAL_HEAD, B_TEXT));
        expected.push(0x02);
        expected.extend(text_item(NON_MINIMAL_HEAD, A_TEXT));
        expected.push(0x01);
        expected.push(0xa1);
        expected.push(0x05);
        expected.extend(text_item(MINIMAL_HEAD, V_TEXT));
        assert_eq!(
            v.to_cbor_bytes(),
            expected,
            "the mutated entry's header widened, so the sidecar lookup keys on the decoded key"
        );
    }

    #[test]
    fn canonical_sorts_by_the_custom_written_bytes() {
        let v = Holder::from_cbor_bytes(&wire()).unwrap();
        // The wire order is B, A — which is ALSO the decoded-bytes order, so a canonical pass that
        // sorted keys by the decoded value (or left them alone) would emit B first and this vector
        // would fail. The keys are sorted by the bytes the CUSTOM serializer wrote into the scratch
        // buffer, and written, A ("00…01") precedes B ("02…00").
        let mut canonical = vec![0x83];
        canonical.extend(text_item(MINIMAL_HEAD, P_TEXT));
        canonical.push(0xa2);
        canonical.extend(text_item(MINIMAL_HEAD, A_TEXT));
        canonical.push(0x01);
        canonical.extend(text_item(MINIMAL_HEAD, B_TEXT));
        canonical.push(0x02);
        canonical.push(0xa1);
        canonical.push(0x05);
        canonical.extend(text_item(MINIMAL_HEAD, V_TEXT));
        // …and canonical also re-minimizes both non-minimal text headers THROUGH the custom codec,
        // which only works because the codec was handed the encoding and `force_canonical`.
        assert_eq!(v.to_canonical_cbor_bytes(), canonical);
        // canonical output is a fixed point through the same codecs
        let reparsed = Holder::from_cbor_bytes(&canonical).unwrap();
        assert_eq!(reparsed.to_cbor_bytes(), canonical);
        assert_eq!(reparsed.to_canonical_cbor_bytes(), canonical);
    }

    #[test]
    fn a_fresh_value_still_goes_through_the_custom_writers() {
        // No sidecar at all, so every encoding takes its default — the codecs must still own the
        // wire, which is observable because the marker type's OWN impls would write a byte string.
        let mut t = OrderedHashMap::new();
        t.insert(decoded(A_TEXT), 1u64);
        let mut map_v = OrderedHashMap::new();
        map_v.insert(5u64, decoded(V_TEXT));
        let fresh = Holder::new(decoded(P_TEXT), t, map_v);
        let mut expected = vec![0x83];
        expected.extend(text_item(MINIMAL_HEAD, P_TEXT));
        expected.push(0xa1);
        expected.extend(text_item(MINIMAL_HEAD, A_TEXT));
        expected.push(0x01);
        expected.push(0xa1);
        expected.push(0x05);
        expected.extend(text_item(MINIMAL_HEAD, V_TEXT));
        assert_eq!(fresh.to_cbor_bytes(), expected);
    }

    #[test]
    fn the_custom_readers_refinements_are_deserialize_errors() {
        // A key that is text of the right width but not hex: the custom reader's own refinement.
        let not_hex = "zz000000000000000000000000000000000000000000000000000000";
        let mut wire = vec![0x83];
        wire.extend(text_item(MINIMAL_HEAD, P_TEXT));
        wire.push(0xa1);
        wire.extend(text_item(MINIMAL_HEAD, not_hex));
        wire.push(0x01);
        wire.push(0xa1);
        wire.push(0x05);
        wire.extend(text_item(MINIMAL_HEAD, V_TEXT));
        assert!(
            Holder::from_cbor_bytes(&wire).is_err(),
            "a non-hex key must be a deserialize error, not a panic or a silent accept"
        );

        // A key of the WRONG MAJOR — the byte string the marker type's own impls would have written.
        // The custom reader owns the wire at this position, so the type's default encoding is not
        // admissible here.
        let mut wire = vec![0x83];
        wire.extend(text_item(MINIMAL_HEAD, P_TEXT));
        wire.push(0xa1);
        wire.push(0x58);
        wire.push(0x1c);
        wire.extend_from_slice(decoded(A_TEXT).to_raw_bytes());
        wire.push(0x01);
        wire.push(0xa1);
        wire.push(0x05);
        wire.extend(text_item(MINIMAL_HEAD, V_TEXT));
        assert!(
            Holder::from_cbor_bytes(&wire).is_err(),
            "a bytes-major key must be a deserialize error: the custom pair owns this position's wire"
        );
    }
}
