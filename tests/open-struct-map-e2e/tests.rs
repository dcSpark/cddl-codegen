// Open struct-map (loose CBOR "rest row") value-level end-to-end vectors. A trailing `* k => v` after
// fixed keys captures unknown map entries into a `pub rest` map instead of erroring. These pin the
// plain-mode (non-preserve) capture semantics of Phase B WP2:
//   * empty rest ≡ closed-struct bytes (adding a rest row is backward compatible on the wire);
//   * unknown uint/text keys captured (per the key domain), wrong-type keys rejected (typing enforced);
//   * duplicate fixed key and duplicate rest key both rejected;
//   * fixed keys win even on a value-type mismatch (ruling §10.10);
//   * an `any` key domain captures every wire key type (uint/text/bytes/bool/…).
// Every `wire` byte string is hand-written from the CBOR grammar, not copied from generator output.
#[cfg(test)]
mod open_struct_map {
    use super::*;
    use crate::generated::any_cbor::AnyCbor;
    use serialization::Deserialize;

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    #[test]
    fn empty_rest_equals_closed_struct_bytes() {
        // A `foo` with no captured entries must serialize as a closed `{ 1: 7, 2: "hi" }` (map of 2).
        let f = Foo::new(7, "hi".to_string());
        assert_eq!(f.rest.len(), 0);
        let closed = bytes("a2 01 07 02 6268 69");
        assert_eq!(f.to_cbor_bytes(), closed);
        let round = Foo::from_cbor_bytes(&closed).unwrap();
        assert_eq!(round.key_1, 7);
        assert_eq!(round.rest.len(), 0);
    }

    #[test]
    fn uint_rest_capture_round_trips_bytewise() {
        // { 1: 7, 2: "hi", 99: 5 } — the unknown uint key 99 lands in `rest`.
        let wire = bytes("a3 01 07 02 6268 69 1863 05");
        let f = Foo::from_cbor_bytes(&wire).unwrap();
        assert_eq!(f.key_1, 7);
        assert_eq!(f.key_2, "hi");
        assert_eq!(f.rest.len(), 1);
        assert_eq!(f.rest.get(&99).unwrap().as_uint(), Some(5));
        // Re-serialize: declared fields first, then the rest entry — byte-identical here.
        assert_eq!(f.to_cbor_bytes(), wire);
    }

    #[test]
    fn wrong_type_rest_key_errors() {
        // { 1: 7, 2: "hi", "x": 5 } — a text key is not in the `* uint => any` domain, so it errors.
        let wire = bytes("a3 01 07 02 6268 69 6178 05");
        assert!(Foo::from_cbor_bytes(&wire).is_err());
    }

    #[test]
    fn duplicate_fixed_key_errors() {
        // { 1: 7, 1: 8, 2: "hi" } — a repeated declared key is a DuplicateKey error, as for closed maps.
        let wire = bytes("a3 01 07 01 08 02 6268 69");
        assert!(Foo::from_cbor_bytes(&wire).is_err());
    }

    #[test]
    fn duplicate_rest_key_rejected() {
        // { 1: 7, 2: "hi", 99: 5, 99: 6 } — a repeated rest key is rejected (default @duplicates reject).
        let wire = bytes("a4 01 07 02 6268 69 1863 05 1863 06");
        assert!(Foo::from_cbor_bytes(&wire).is_err());
    }

    #[test]
    fn fixed_keys_win_on_content_mismatch() {
        // bar = { ? 3: text, * uint => any }. Wire { 3: 5 } dispatches key 3 to the (text) optional
        // field, whose deserialize fails on the uint value — an ERROR, not a fallthrough to the rest
        // row (ruling §10.10: fixed-key dispatch wins even on content mismatch).
        assert!(Bar::from_cbor_bytes(&bytes("a1 03 05")).is_err());
        // { 3: "ok" } binds the optional field; rest stays empty.
        let b = Bar::from_cbor_bytes(&bytes("a1 03 626f6b")).unwrap();
        assert_eq!(b.rest.len(), 0);
        // { 7: 5 } (key 3 absent) captures 7 => 5 into rest.
        let b2 = Bar::from_cbor_bytes(&bytes("a1 07 05")).unwrap();
        assert_eq!(b2.rest.get(&7).unwrap().as_uint(), Some(5));
    }

    #[test]
    fn any_key_domain_captures_every_wire_key_type() {
        // baz = { 1: uint, * any => any }. Wire { 1: 0, "t": 1, h'ff': 2, true: 3 } — text, bytes, and
        // a special (bool) key all captured as AnyCbor keys.
        let wire = bytes("a4 01 00 6174 01 41ff 02 f5 03");
        let b = Baz::from_cbor_bytes(&wire).unwrap();
        assert_eq!(b.key_1, 0);
        assert_eq!(b.rest.len(), 3);
        assert_eq!(
            b.rest.get(&AnyCbor::new_text("t".to_string())).unwrap().as_uint(),
            Some(1)
        );
        assert_eq!(
            b.rest.get(&AnyCbor::new_bytes(vec![0xff])).unwrap().as_uint(),
            Some(2)
        );
        assert_eq!(
            b.rest.get(&AnyCbor::new_bool(true)).unwrap().as_uint(),
            Some(3)
        );
    }
}
