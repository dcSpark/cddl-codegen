// `@custom_encodings` end-to-end vectors: what a custom codec's DECLARED wire framing buys, executed.
//
// Every rule in `input.cddl` is one where inference from the REPLACED type gives the wrong answer, so
// each vector below fails without the declaration in a specific, named way — and the ones over `bool`
// fail SILENTLY without it (the codec is handed nothing, so it re-minimizes and both directions
// agree, which is precisely why a round-trip test alone could never have caught the defect). Every
// `wire` byte string is hand-written from the CBOR grammar.
#[cfg(test)]
mod custom_encodings {
    use super::*;
    use serialization::{Deserialize, Serialize};

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    // [ 1(one-byte-following), false, 42(bytes "hi", non-minimal len head), 1(inline), { 42("k"): 7 } ]
    //
    //   85          array(5)
    //   18 01       f: uint 1 written one-byte-following — NON-MINIMAL, and recordable only through
    //               the declared `sz` (the replaced `bool` infers nothing at all)
    //   f4          s: false as a simple value — the `none` declaration's wire, no framing to record
    //   d8 2a       n: tag(42), one-byte-following (minimal for 42) — the declared `sz`
    //   58 02 6869  n: bytes "hi" with a NON-MINIMAL one-byte-following length head — the declared
    //               `str`, which inference WOULD have supplied; the tag width beside it is what it
    //               could not
    //   01          g: uint 1 inline (minimal) — the FIELD-level declaration, same codec
    //   a1          t: map(1)
    //   d8 2a 41 6b   key: tag(42) + bytes "k" (minimal both) — the by-value table twin
    //   07            value: uint 7
    const WIRE: &str = "85 1801 f4 d82a 58026869 01 a1 d82a416b 07";

    #[test]
    fn declared_framing_round_trips_byte_exact() {
        let wire = bytes(WIRE);
        let v = Holder::from_cbor_bytes(&wire).unwrap();
        assert!(v.f, "the uint-written flag decodes to its value");
        assert!(!v.s);
        assert_eq!(v.n, bytes("6869"));
        assert!(v.g);
        assert_eq!(v.t.get(&bytes("6b")).copied(), Some(7));
        // The whole point: the non-minimal head on `f` and the non-minimal length head on `n` survive.
        // Without the declaration `f` has no sidecar slot at all and comes back `01`.
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact declared-framing round trip");
    }

    #[test]
    fn the_declared_slots_exist_and_are_what_was_read() {
        let v = Holder::from_cbor_bytes(&bytes(WIRE)).unwrap();
        let encs = v.encodings.as_ref().expect("preserve records an encoding struct");
        // `f` is the zero-demand witness: a slot exists for it ONLY because the pair declared one,
        // and it holds the width the wire actually used.
        assert_eq!(encs.f_encoding, Some(cbor_event::Sz::One));
        // `n` declared two, positionally: the tag head's width first, then the bytes header.
        assert_eq!(encs.n_encoding, Some(cbor_event::Sz::One));
        assert_eq!(
            encs.n_encoding2,
            StringEncoding::Definite(cbor_event::Sz::One)
        );
        // the field-level declaration on `g`, and the by-value table-key twin, land the same way
        assert_eq!(encs.g_encoding, Some(cbor_event::Sz::Inline));
        assert_eq!(
            encs.t_key_encodings.get(&bytes("6b")).cloned(),
            Some((Some(cbor_event::Sz::One), StringEncoding::Definite(cbor_event::Sz::Inline)))
        );
    }

    #[test]
    fn canonical_minimizes_the_declared_framing() {
        let v = Holder::from_cbor_bytes(&bytes(WIRE)).unwrap();
        // Canonical re-minimizes exactly the widths the declaration made visible: `f`'s head collapses
        // to inline `01` and `n`'s length head to `42`. The tag heads stay `d8 2a` (one-byte-following
        // IS minimal for 42), which is what makes this a minimization rather than a rewrite.
        let canonical = bytes("85 01 f4 d82a 426869 01 a1 d82a416b 07");
        assert_eq!(v.to_canonical_cbor_bytes(), canonical);
        // …and canonical output is a fixed point through the same custom codecs.
        let reparsed = Holder::from_cbor_bytes(&canonical).unwrap();
        assert_eq!(reparsed.to_cbor_bytes(), canonical);
        assert_eq!(reparsed.to_canonical_cbor_bytes(), canonical);
    }

    #[test]
    fn a_fresh_value_still_goes_through_the_custom_writers() {
        // No sidecar at all (the encoding struct is `None`), so every declared variable takes its
        // default — the codecs must still be on the write path, which is observable because the
        // DEFAULT writers for these types would emit a bool as `f5`/`f4` and the bytes without a tag.
        let mut table = OrderedHashMap::new();
        table.insert(bytes("6b"), 7u64);
        let fresh = Holder::new(true, false, bytes("6869"), true, table);
        assert_eq!(
            fresh.to_cbor_bytes(),
            bytes("85 01 f4 d82a 426869 01 a1 d82a416b 07"),
            "a fresh value writes the custom wire with minimal declared framing"
        );
    }
}
