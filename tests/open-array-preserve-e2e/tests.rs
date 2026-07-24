// Open array (loose CBOR "rest tail") PRESERVE fidelity vectors. These pin the preserve/canonical
// tail core: per-element encoding sidecars for a typed tail (byte-exact non-canonical widths),
// self-carried encodings for an `any` tail, indefinite owner-array + tail, empty-tail ≡ closed
// bytes, the canonical normalization of a non-canonical-width tail element (position order, no sort),
// and stream position (a sibling after an open array). Every `wire` byte string is hand-written from
// the CBOR grammar, not copied from generator output.
#[cfg(test)]
mod open_array_preserve {
    use super::*;
    use serialization::{Deserialize, Serialize};

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    // --- typed tail (`cap = [uint, tstr, * uint]`): per-element encoding sidecar ---

    #[test]
    fn typed_tail_non_canonical_widths_byte_exact() {
        // [7, "hi", 2(as 0x1802, one-byte arg), 1000(as 0x1903e8)] — the trailing 2 and 1000 land in
        // `rest` with their exact wire widths. Preserve must reproduce those widths byte-for-byte via
        // the positional `rest_elem_encodings` sidecar.
        let wire = bytes("84 07 62 6869 1802 1903e8");
        let c = Cap::from_cbor_bytes(&wire).unwrap();
        assert_eq!(c.index_0, 7);
        assert_eq!(c.index_1, "hi");
        assert_eq!(c.rest, vec![2u64, 1000u64]);
        assert_eq!(c.to_cbor_bytes(), wire, "byte-exact non-canonical tail widths");
    }

    #[test]
    fn typed_tail_canonical_normalizes_in_position_order() {
        // Same input; --canonical-form re-emits each element at its minimal width in position order
        // (0x1802 -> 0x02; 1000 already needs the two-byte arg 0x1903e8). No sort — arrays are
        // positional.
        let wire = bytes("84 07 62 6869 1802 1903e8");
        let c = Cap::from_cbor_bytes(&wire).unwrap();
        assert_eq!(
            c.to_canonical_cbor_bytes(),
            bytes("84 07 62 6869 02 1903e8"),
            "canonical normalizes the non-minimal tail element in place"
        );
    }

    #[test]
    fn empty_tail_equals_closed_bytes_both_modes() {
        // No trailing elements: preserve AND canonical output match the closed `[7, "hi"]` (array of
        // 2). Empty tail ≡ closed-struct bytes.
        let closed = bytes("82 07 62 6869");
        let c = Cap::from_cbor_bytes(&closed).unwrap();
        assert!(c.rest.is_empty());
        assert_eq!(c.to_cbor_bytes(), closed, "empty-tail preserve ≡ closed");
        assert_eq!(
            c.to_canonical_cbor_bytes(),
            closed,
            "empty-tail canonical ≡ closed"
        );
    }

    #[test]
    fn indefinite_owner_and_tail_byte_exact() {
        // Indefinite [7, "hi", 2, 3, break]. Preserve reproduces the indefinite owner-array header AND
        // the tail elements byte-for-byte (the tail loop stops on the peeked 0xff break, which the
        // final length check then consumes).
        let wire = bytes("9f 07 62 6869 02 03 ff");
        let c = Cap::from_cbor_bytes(&wire).unwrap();
        assert_eq!(c.rest, vec![2u64, 3u64]);
        assert_eq!(c.to_cbor_bytes(), wire, "indefinite owner + tail byte-exact");
    }

    // --- `any` tail (`cap_any = [uint, * any]`): self-carried encodings (no sidecar) ---

    #[test]
    fn any_tail_self_carried_encoding_byte_exact() {
        // [7, 2(as 0x1802)] — the `any` tail element carries its own encoding (AnyCbor), so a
        // non-minimal width round-trips byte-exactly without a per-element sidecar.
        let wire = bytes("82 07 1802");
        let c = CapAny::from_cbor_bytes(&wire).unwrap();
        assert_eq!(c.index_0, 7);
        assert_eq!(c.rest.len(), 1);
        assert_eq!(c.to_cbor_bytes(), wire, "any tail self-carried byte-exact");
    }

    // --- stream position: an open array as a member of an outer array (cip36 skip-arm bug class) ---

    #[test]
    fn stream_position_sibling_after_open_array_byte_exact() {
        // outer = [inner, uint], inner = [uint, * uint]. [[1, 2, 3], 99] — inner's tail is [2, 3] and
        // the sibling 99 must decode into outer.index_1; preserve re-emits the whole thing byte-exact.
        let wire = bytes("82 83 01 02 03 1863");
        let o = Outer::from_cbor_bytes(&wire).unwrap();
        assert_eq!(o.inner.index_0, 1);
        assert_eq!(o.inner.rest, vec![2u64, 3u64]);
        assert_eq!(o.index_1, 99);
        assert_eq!(o.to_cbor_bytes(), wire, "stream position preserve byte-exact");
    }
}
