#[cfg(test)]
mod tests {
    use super::*;
    use crate::serialization::RawBytesEncoding;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        let orig_bytes = orig.to_cbor_bytes();
        print_cbor_types("orig", &orig_bytes);
        let mut deserializer = Deserializer::from(orig_bytes.clone());
        let deser = T::deserialize(&mut deserializer).unwrap();
        print_cbor_types("deser", &deser.to_cbor_bytes());
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.position(), orig_bytes.len());
    }

    #[test]
    fn foo() {
        deser_test(&Foo::new(PubKey([0; 32])));
    }

    // --- `hex`-dependency wire pins, at the `RawBytesEncoding` surface ---
    //
    // `to_raw_hex` / `from_raw_hex` are trait DEFAULT methods (`static/raw_bytes_encoding.rs`), so
    // the backing hex crate's encode/decode behavior IS the consumer-facing behavior of every
    // raw-bytes type this tool emits. A swap or version bump of that dependency rewrites this
    // public surface without touching a line of generated code, and passes every round-trip and
    // compile gate on the way through: round-trips only ever feed back what the encoder produced,
    // so neither the ACCEPTED INPUT GRAMMAR nor the RENDERED ERROR TEXT is observed anywhere else.
    //
    // These pins state the behavior and are asserted only through `RawBytesEncoding` and
    // `DeserializeError` — no `hex::` path is named — so they stay meaningful across a swap, and a
    // swap that changes any of them must flip the pin in its own diff instead of shipping silently.
    // (tests/README.md hand-vector shape 5: pin a re-exposed dependency's behavior BEFORE the
    // baseline moves.)
    //
    // The accepted grammar is CANONICAL hex: bare, even-length, lowercase — exactly what
    // `to_raw_hex` emits. That is a DELIBERATE NARROWING (the surface used to take uppercase and
    // normalize it) made when the backing decoder was swapped, and it is what buys the round-trip
    // property on the ENCODING rather than only on the bytes. The grammar lives in one place,
    // `decode_canonical_hex` in the composed `serialization.rs`, which this surface and the emitted
    // JSON bytes newtypes both route through.
    //
    // No twin in `tests/raw-bytes-preserve/tests.rs`: `raw_bytes_encoding.rs` is concatenated into
    // the composed `serialization.rs` unconditionally of `--preserve-encodings`
    // (`generation/export.rs`), so the preserve profile runs these identical trait defaults and a
    // duplicate suite would pin nothing new.

    // Wire pin 1: `to_raw_hex` emits PLAIN LOWERCASE hex — no prefix, no separators, no uppercase —
    // and `from_raw_hex` reads exactly that back. (Asserted below against a literal expected string,
    // so both the alphabet and the absence of any prefix are pinned by the equality itself.)
    #[test]
    fn raw_bytes_encoding_round_trip() {
        // PubKey wraps a fixed [u8; 32]; build a recognizable byte pattern.
        let mut bytes = [0u8; 32];
        for (i, b) in bytes.iter_mut().enumerate() {
            *b = i as u8;
        }
        let key = PubKey(bytes);
        // to_raw_hex is the trait default: hex::encode over to_raw_bytes (lowercase).
        let hex = key.to_raw_hex();
        assert_eq!(hex.len(), 64);
        assert_eq!(
            hex,
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        );
        // from_raw_hex is the trait default: hex::decode then from_raw_bytes.
        let decoded = PubKey::from_raw_hex(&hex).unwrap();
        assert_eq!(key.to_raw_bytes(), decoded.to_raw_bytes());
    }

    #[test]
    fn raw_bytes_encoding_reject() {
        // Non-hex characters: hex::decode fails -> InvalidStructure branch of from_raw_hex.
        assert!(PubKey::from_raw_hex("zz").is_err());
        // Odd-length hex string is likewise an invalid hex encoding.
        assert!(PubKey::from_raw_hex("abc").is_err());
        // Valid hex but wrong decoded length: from_raw_bytes try_into ([u8; 32]) fails.
        assert!(PubKey::from_raw_hex("00").is_err());
        assert!(PubKey::from_raw_bytes(&[0u8; 31]).is_err());
    }

    // Wire pin 2: decoding is case-SENSITIVE — an uppercase digit is rejected as an invalid
    // character at its own index. Case-insensitivity is GONE: this surface used to accept an
    // uppercase string and normalize it to lowercase on the way out, and that leniency was removed
    // by maintainer ruling when the backing decoder was swapped, in exchange for the round-trip
    // property pinned below. A consumer holding hex from another tool lowercases it at the call
    // site (`s.to_ascii_lowercase()`).
    //
    // Two vectors, each with an unambiguous FIRST offending character at a computable index: an
    // all-uppercase body, and an otherwise-canonical body carrying one uppercase digit.
    #[test]
    fn raw_bytes_hex_uppercase_rejected() {
        // The uppercase twin of pin 1's body. Its first 21 characters are digits `0`-`9`, which the
        // canonical alphabet contains, so the first offender is the `A` at index 21.
        let upper = "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F";
        assert_eq!(
            PubKey::from_raw_hex(upper).err().unwrap().to_string(),
            "Deserialization: Invalid internal structure: invalid character 'A' at position 21"
        );
        // ...and a single uppercase digit in an otherwise-canonical body is enough, at its index.
        let one_upper = "000102030405060708090a0B0c0d0e0f101112131415161718191a1b1c1d1e1f";
        assert_eq!(
            PubKey::from_raw_hex(one_upper).err().unwrap().to_string(),
            "Deserialization: Invalid internal structure: invalid character 'B' at position 23"
        );
    }

    // Wire pin (new with the canonical grammar): MIXED case is rejected too, and on the first
    // offending character — a lowercase prefix does not buy a mixed-case tail any leniency.
    #[test]
    fn raw_bytes_hex_mixed_case_rejected() {
        // 64 chars, canonical except `B` at index 2 (`a1B2…`)
        let mixed = "a1B2030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20";
        assert_eq!(
            PubKey::from_raw_hex(mixed).err().unwrap().to_string(),
            "Deserialization: Invalid internal structure: invalid character 'B' at position 2"
        );
    }

    // Wire pin (new with the canonical grammar): the ROUND-TRIP PROPERTY the narrowing bought —
    // for every ACCEPTED string, re-encoding the decoded bytes reproduces that string byte for
    // byte. This is strictly stronger than pin 1's bytes-level round trip: it says the accepted
    // grammar and the emitted grammar are the SAME grammar, so no accepted spelling exists that
    // `to_raw_hex` would not itself have written. A read side wider than the write side (any
    // uppercase or prefixed spelling) falsifies exactly this test.
    #[test]
    fn raw_bytes_hex_canonical_round_trip() {
        for s in [
            "0000000000000000000000000000000000000000000000000000000000000000",
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
            "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
            "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
        ] {
            let key = PubKey::from_raw_hex(s)
                .unwrap_or_else(|e| panic!("{s} must be accepted canonical hex: {e}"));
            assert_eq!(key.to_raw_hex(), s);
        }
    }

    // Wire pin 3: a `0x` / `0X` PREFIX is REJECTED — the accepted grammar is bare hex digits only,
    // with the prefix character reported as an invalid character at index 1. This is the pin a more
    // lenient hex implementation flips from Err to Ok, widening the accepted input grammar of a
    // shipped public surface while `to_raw_hex` keeps emitting the unprefixed form — an asymmetry no
    // round-trip test can see. The backing decoder IS one of those lenient implementations: it
    // strips the prefix and accepts the rest, so the grammar is enforced by
    // `decode_canonical_hex` rather than delegated to it. The prefix needs no rule of its own —
    // `x`/`X` are simply outside the canonical alphabet, so the same scan that rejects uppercase
    // (pin 2) reports the prefix character here — and this pin's VERDICT and error TEXT both
    // survived the decoder swap and the later narrowing unchanged.
    #[test]
    fn raw_bytes_hex_prefix_rejected() {
        let body = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
        for (prefixed, ch) in [
            (format!("0x{body}"), 'x'),
            (format!("0X{}", body.to_uppercase()), 'X'),
        ] {
            let err = PubKey::from_raw_hex(&prefixed)
                .err()
                .unwrap_or_else(|| {
                    panic!("{prefixed} must be rejected: `0x` is not part of the accepted grammar")
                })
                .to_string();
            assert_eq!(
                err,
                format!(
                    "Deserialization: Invalid internal structure: invalid character '{ch}' at position 1"
                )
            );
        }
    }

    // Wire pin 4: the rendered error TEXT for the two malformed-input classes, exactly as a consumer
    // reads it through `DeserializeError`'s Display — the hex error is boxed into
    // `DeserializeFailure::InvalidStructure`, whose arm renders the inner error inline, so the
    // dependency's wording reaches consumer output verbatim. Pinned so that a swap rewording them
    // is a visible diff rather than a silent change in shipped messages.
    #[test]
    fn raw_bytes_hex_error_display() {
        // non-hex character: reported with the offending character and its 0-based position
        assert_eq!(
            PubKey::from_raw_hex("a1g2").err().unwrap().to_string(),
            "Deserialization: Invalid internal structure: invalid character 'g' at position 2"
        );
        // odd digit count: reported as a whole-string shape error, carrying no position
        assert_eq!(
            PubKey::from_raw_hex("abc").err().unwrap().to_string(),
            "Deserialization: Invalid internal structure: odd number of digits"
        );
    }
}
