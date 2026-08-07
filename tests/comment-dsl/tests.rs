// Behavioral coverage for the comment DSL (this fixture runs under --preserve-encodings=true):
// every directive that renames rust-side items is checked to leave the WIRE FORMAT untouched
// (hand-written RFC 8949 hex literals, NOT copied from generator output), and the directives
// that restructure types (@newtype, group/type-choice naming) get byte pins + round-trips.
#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        let orig_bytes = orig.to_cbor_bytes();
        print_cbor_types("orig", &orig_bytes);
        let mut deserializer = Deserializer::from(orig_bytes.clone());
        let deser = T::deserialize(&mut deserializer).unwrap();
        print_cbor_types("deser", &deser.to_cbor_bytes());
        assert_eq!(orig_bytes, deser.to_cbor_bytes());
        assert_eq!(deserializer.position(), orig_bytes.len());
    }

    // @name on array members renames the RUST fields only; the wire stays [bytes, uint].
    #[test]
    fn group_type() {
        let addr = Address::new(vec![0xAB, 0xCD], 5);
        assert_eq!(addr.address, vec![0xAB, 0xCD]);
        assert_eq!(addr.checksum, 5);
        // array(2), bytes(2) h'ABCD', uint 5
        assert_eq!(addr.to_cbor_bytes(), vec![0x82, 0x42, 0xAB, 0xCD, 0x05]);
        deser_test(&addr);
        let decoded = Address::from_cbor_bytes(&[0x82, 0x42, 0xAB, 0xCD, 0x05]).unwrap();
        assert_eq!(decoded.address, vec![0xAB, 0xCD]);
        assert_eq!(decoded.checksum, 5);
    }

    // @name on KEYLESS map members has no test because the construct is rejected by design
    // ("map fields need keys", src/parsing.rs) — pinned by tests/robustness/map_entry_no_key.cddl.

    // @name on int-keyed map members renames the RUST fields only; the wire keys stay 0/1.
    #[test]
    fn with_key_group() {
        let ppu = ProtocolParamUpdate::new(1, 2);
        assert_eq!(ppu.minfee_a, 1);
        assert_eq!(ppu.minfee_b, 2);
        // map(2) { 0: 1, 1: 2 }
        assert_eq!(ppu.to_cbor_bytes(), vec![0xA2, 0x00, 0x01, 0x01, 0x02]);
        deser_test(&ppu);
        let decoded = ProtocolParamUpdate::from_cbor_bytes(&[0xA2, 0x00, 0x01, 0x01, 0x02]).unwrap();
        assert_eq!(decoded.minfee_a, 1);
        assert_eq!(decoded.minfee_b, 2);
    }

    // Group-choice restructuring: each @name'd choice serializes as [discriminant, bytes] and
    // deserialization picks the variant from the wire discriminant, into the @name'd variant/field.
    #[test]
    fn group_choice() {
        let ebb_block = Block::new_ebb_block_wrapper(vec![0xAB]);
        // array(2), uint 0, bytes(1) h'AB'
        assert_eq!(ebb_block.to_cbor_bytes(), vec![0x82, 0x00, 0x41, 0xAB]);
        deser_test(&ebb_block);
        let main_block = Block::new_main_block_wrapper(vec![0xAB]);
        assert_eq!(main_block.to_cbor_bytes(), vec![0x82, 0x01, 0x41, 0xAB]);
        deser_test(&main_block);
        match Block::from_cbor_bytes(&[0x82, 0x00, 0x41, 0xAB]).unwrap() {
            Block::EbbBlockWrapper { ebb_block_cbor, .. } => assert_eq!(ebb_block_cbor, vec![0xAB]),
            other => panic!("expected EbbBlockWrapper, got {other:?}"),
        }
        match Block::from_cbor_bytes(&[0x82, 0x01, 0x41, 0xAB]).unwrap() {
            Block::MainBlockWrapper { main_block_cbor, .. } => {
                assert_eq!(main_block_cbor, vec![0xAB])
            }
            other => panic!("expected MainBlockWrapper, got {other:?}"),
        }
    }

    // Type-choice restructuring: the @name'd enum adds NO wrapper of its own — each variant's
    // bytes are exactly the underlying [discriminant, bytes] struct's encoding.
    #[test]
    fn type_choice() {
        let case1 = Typechoice::new_case1(Case1::new(vec![0xAB]));
        assert_eq!(case1.to_cbor_bytes(), vec![0x82, 0x00, 0x41, 0xAB]);
        assert_eq!(Case1::new(vec![0xAB]).to_cbor_bytes(), case1.to_cbor_bytes());
        deser_test(&case1);
        let case2 = Typechoice::new_case2(Case2::new(vec![0xAB]));
        assert_eq!(case2.to_cbor_bytes(), vec![0x82, 0x01, 0x41, 0xAB]);
        deser_test(&case2);
        match Typechoice::from_cbor_bytes(&[0x82, 0x01, 0x41, 0xAB]).unwrap() {
            Typechoice::Case2(inner) => assert_eq!(inner.index_1, vec![0xAB]),
            other => panic!("expected Case2, got {other:?}"),
        }
    }

    // These variants differ in TYPE (text vs [* text]), so deserialization discriminates on the
    // CBOR major type rather than an embedded discriminant.
    #[test]
    fn type_choice_variants() {
        let case1 = TypechoiceVariants::new_case1(String::from("hi"));
        // text(2) "hi" — no wrapping
        assert_eq!(case1.to_cbor_bytes(), vec![0x62, 0x68, 0x69]);
        deser_test(&case1);
        let case2 = TypechoiceVariants::new_case2(vec![String::from("hi")]);
        // array(1), text(2) "hi"
        assert_eq!(case2.to_cbor_bytes(), vec![0x81, 0x62, 0x68, 0x69]);
        deser_test(&case2);
        match TypechoiceVariants::from_cbor_bytes(&[0x62, 0x68, 0x69]).unwrap() {
            TypechoiceVariants::Case1 { case1, .. } => assert_eq!(case1, "hi"),
            other => panic!("expected Case1, got {other:?}"),
        }
        match TypechoiceVariants::from_cbor_bytes(&[0x81, 0x62, 0x68, 0x69]).unwrap() {
            TypechoiceVariants::Case2 { case2, .. } => assert_eq!(case2, vec![String::from("hi")]),
            other => panic!("expected Case2, got {other:?}"),
        }
    }

    // @newtype: the wrapper is code-level only — the wire bytes are the RAW INNER uint encoding
    // (no array/tag/map wrapping).
    #[test]
    fn newtype() {
        let pm = ProtocolMagic::new(5);
        assert_eq!(pm.get(), 5);
        assert_eq!(pm.to_cbor_bytes(), vec![0x05]);
        deser_test(&pm);
        let big = ProtocolMagic::new(1337);
        assert_eq!(big.to_cbor_bytes(), vec![0x19, 0x05, 0x39]);
        deser_test(&big);
        assert_eq!(
            ProtocolMagic::from_cbor_bytes(&[0x19, 0x05, 0x39])
                .unwrap()
                .get(),
            1337
        );
        // preserve-encodings: a non-minimally-encoded inner uint (5 as two-byte 0x19 0x0005)
        // round-trips byte-identically through the newtype
        let non_minimal = ProtocolMagic::from_cbor_bytes(&[0x19, 0x00, 0x05]).unwrap();
        assert_eq!(non_minimal.get(), 5);
        assert_eq!(non_minimal.to_cbor_bytes(), vec![0x19, 0x00, 0x05]);
    }

    // @name on a member-position anonymous inline array reaches THROUGH the tag wrapper: the name
    // mints `TaggedPoint` and the tag wraps it. Rust-side naming only — the wire is the tag over
    // the array, identical to the named-rule spelling (`p = [x: uint]` / `#6.42(p)`).
    #[test]
    fn tagged_anon_array_member_name() {
        let holder = TaggedAnonHolder::new(TaggedPoint::new(1), String::from("hi"));
        assert_eq!(holder.tagged_point.x, 1);
        assert_eq!(holder.label, "hi");
        // array(2), tag(42) d8 2a, array(1), uint 1, text(2) "hi"
        let bytes = vec![0x82, 0xd8, 0x2a, 0x81, 0x01, 0x62, 0x68, 0x69];
        assert_eq!(holder.to_cbor_bytes(), bytes);
        deser_test(&holder);
        let decoded = TaggedAnonHolder::from_cbor_bytes(&bytes).unwrap();
        assert_eq!(decoded.tagged_point.x, 1);
        assert_eq!(decoded.label, "hi");
        assert_eq!(decoded.to_cbor_bytes(), bytes);
        // preserve-encodings: a non-minimally-encoded inner uint inside the tagged array survives
        // the round trip, so the minted struct carries its own encoding sidecar like any other.
        let non_minimal = vec![0x82, 0xd8, 0x2a, 0x81, 0x19, 0x00, 0x01, 0x62, 0x68, 0x69];
        let kept = TaggedAnonHolder::from_cbor_bytes(&non_minimal).unwrap();
        assert_eq!(kept.tagged_point.x, 1);
        assert_eq!(kept.to_cbor_bytes(), non_minimal);
    }
}
