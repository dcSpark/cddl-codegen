// Open struct-map (loose CBOR "rest row") value-level end-to-end vectors. A trailing `* k => v` after
// fixed keys captures unknown map entries behind the checked `rest()` / `insert_rest` API. These pin
// plain-mode (non-preserve) capture semantics:
//   * empty rest ≡ closed-struct bytes (adding a rest row is backward compatible on the wire);
//   * unknown uint/text keys captured (per the key domain), wrong-type keys rejected (typing enforced);
//   * duplicate fixed key and duplicate rest key both rejected;
//   * fixed keys win even on a value-type mismatch (a declared key dispatches to its field even on a
//     content mismatch, rather than falling through to the rest row);
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
        assert_eq!(f.rest().len(), 0);
        let closed = bytes("a2 01 07 02 6268 69");
        assert_eq!(f.to_cbor_bytes(), closed);
        let round = Foo::from_cbor_bytes(&closed).unwrap();
        assert_eq!(round.key_1, 7);
        assert_eq!(round.rest().len(), 0);
    }

    #[test]
    fn uint_rest_capture_round_trips_bytewise() {
        // { 1: 7, 2: "hi", 99: 5 } — the unknown uint key 99 lands in `rest`.
        let wire = bytes("a3 01 07 02 6268 69 1863 05");
        let f = Foo::from_cbor_bytes(&wire).unwrap();
        assert_eq!(f.key_1, 7);
        assert_eq!(f.key_2, "hi");
        assert_eq!(f.rest().len(), 1);
        assert_eq!(f.rest().get(&99).unwrap().as_uint(), Some(5));
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
        // row (fixed-key dispatch wins even on content mismatch).
        assert!(Bar::from_cbor_bytes(&bytes("a1 03 05")).is_err());
        // { 3: "ok" } binds the optional field; rest stays empty.
        let b = Bar::from_cbor_bytes(&bytes("a1 03 626f6b")).unwrap();
        assert_eq!(b.rest().len(), 0);
        // { 7: 5 } (key 3 absent) captures 7 => 5 into rest.
        let b2 = Bar::from_cbor_bytes(&bytes("a1 07 05")).unwrap();
        assert_eq!(b2.rest().get(&7).unwrap().as_uint(), Some(5));
    }

    #[test]
    fn any_key_domain_captures_every_wire_key_type() {
        // baz = { 1: uint, * any => any }. Wire { 1: 0, "t": 1, h'ff': 2, true: 3 } — text, bytes, and
        // a special (bool) key all captured as AnyCbor keys.
        let wire = bytes("a4 01 00 6174 01 41ff 02 f5 03");
        let b = Baz::from_cbor_bytes(&wire).unwrap();
        assert_eq!(b.key_1, 0);
        assert_eq!(b.rest().len(), 3);
        assert_eq!(
            b.rest().get(&AnyCbor::new_text("t".to_string())).unwrap().as_uint(),
            Some(1)
        );
        assert_eq!(
            b.rest().get(&AnyCbor::new_bytes(vec![0xff])).unwrap().as_uint(),
            Some(2)
        );
        assert_eq!(
            b.rest().get(&AnyCbor::new_bool(true)).unwrap().as_uint(),
            Some(3)
        );
    }
}

// TYPED key domains on a rest row (`* K => V` where `K` is not bare `uint`/`text`/`any`): the seek
// path. Nothing is reconstructed from the record loop's peek — a declared-key match arm that falls
// through rewinds to the loop-body anchor and `K::deserialize` reads the key itself. These pin the
// value-level consequences of that:
//   * declared keys still peel first (a second occurrence is a DuplicateKey, never a capture), and
//     every OTHER wire key is refined by `K` (nint, bytes and text keys all land as `Md` variants);
//   * refinement is a hard error, never a capture-on-failure: a float key, an out-of-bounds `.size`
//     key and a wrong-typed VALUE each fail the parse;
//   * `@duplicates preserve` keeps typed duplicates in wire order.
// Every `wire` byte string is hand-written from the CBOR grammar, not copied from generator output.
#[cfg(test)]
mod open_struct_map_typed {
    use super::*;
    use serialization::Deserialize;

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    #[test]
    fn typed_union_key_captures_every_arm() {
        // qux = { 1: uint, "nm": text, * md => uint }, md = int / bstr / text.
        // { 1: 7, "nm": "hi", 5: 1, -3: 2, h'ff': 3, "zz": 4 } — a uint key past the declared arm, a
        // NINT key, a bytes key and an unknown TEXT key, each refined into its `Md` arm.
        let wire = bytes("a6 01 07 626e6d 626869 05 01 22 02 41ff 03 627a7a 04");
        let q = Qux::from_cbor_bytes(&wire).unwrap();
        assert_eq!(q.key_1, 7);
        assert_eq!(q.nm, "hi");
        assert_eq!(q.rest().len(), 4);
        assert_eq!(*q.rest().get(&Md::new_int(Int::new_uint(5))).unwrap(), 1);
        assert_eq!(*q.rest().get(&Md::new_int(Int::new_nint(2))).unwrap(), 2);
        assert_eq!(*q.rest().get(&Md::new_bytes(vec![0xff])).unwrap(), 3);
        assert_eq!(*q.rest().get(&Md::new_text("zz".to_string())).unwrap(), 4);
        // The `BTreeMap<Md, u64>` order (Int < Bytes < Text; Uint < Nint) is the wire order here.
        assert_eq!(q.to_cbor_bytes(), wire);
    }

    #[test]
    fn declared_keys_peel_before_the_typed_domain() {
        // A repeated DECLARED key is a DuplicateKey even though `md` would accept its value as a rest
        // key — declared-key dispatch wins, so a second `1:` never falls through to the capture.
        assert!(Qux::from_cbor_bytes(&bytes("a3 01 07 626e6d 626869 01 08")).is_err());
        // Same on the text side: a second `"nm"` is a duplicate, not a captured `Md::Text("nm")`.
        assert!(Qux::from_cbor_bytes(&bytes("a3 01 07 626e6d 6161 626e6d 6162")).is_err());
        // …while an unknown text key IS captured, as the text arm of `md`.
        let q = Qux::from_cbor_bytes(&bytes("a3 01 07 626e6d 626869 627a7a 04")).unwrap();
        assert_eq!(*q.rest().get(&Md::new_text("zz".to_string())).unwrap(), 4);
    }

    #[test]
    fn refinement_errors_rather_than_capturing() {
        // A float key (CBOR special) reaches the Special arm, is not a break, and fails `Md`.
        assert!(Qux::from_cbor_bytes(&bytes("a3 01 07 626e6d 626869 f93c00 01")).is_err());
        // A wrong-typed VALUE fails too — the row types the value as strictly as the key.
        assert!(Qux::from_cbor_bytes(&bytes("a3 01 07 626e6d 626869 627a7a 6178")).is_err());
        // A repeated typed rest key is the default (reject) container's DuplicateKey.
        assert!(Qux::from_cbor_bytes(&bytes("a4 01 07 626e6d 626869 627a7a 01 627a7a 02")).is_err());
    }

    #[test]
    fn bytes_key_domain_round_trips() {
        // quux = { 1: uint, * bstr => uint }. A bytes key is CBOR major type 2, so it lands in the
        // `_` dispatch arm with nothing consumed and no rewind needed.
        // { 1: 0, h'': 6, h'ab': 5 } — written in `BTreeMap<Vec<u8>, _>` order so it round-trips.
        let wire = bytes("a3 01 00 40 06 41ab 05");
        let q = Quux::from_cbor_bytes(&wire).unwrap();
        assert_eq!(q.rest().len(), 2);
        assert_eq!(*q.rest().get(&vec![]).unwrap(), 6);
        assert_eq!(*q.rest().get(&vec![0xab]).unwrap(), 5);
        assert_eq!(q.to_cbor_bytes(), wire);
        // A uint key is outside the domain: the declared arm's catch-all rewinds and `bytes` fails.
        assert!(Quux::from_cbor_bytes(&bytes("a2 01 00 05 01")).is_err());
    }

    #[test]
    fn size_bounded_key_domain_refines() {
        // grault = { 1: uint, * five => uint }, five = tstr .size 5. There is no declared TEXT key,
        // so the text arm captures directly (nothing read, nothing to rewind) — and the newtype's
        // length check runs on the way in.
        let wire = bytes("a2 01 00 656162636465 01");
        let g = Grault::from_cbor_bytes(&wire).unwrap();
        assert_eq!(g.rest().len(), 1);
        assert_eq!(
            *g.rest()
                .get(&Five::new("abcde".to_string()).unwrap())
                .unwrap(),
            1
        );
        assert_eq!(g.to_cbor_bytes(), wire);
        // A 2-character key is not in the domain — a parse error, not a capture.
        assert!(Grault::from_cbor_bytes(&bytes("a2 01 00 626162 01")).is_err());
    }

    #[test]
    fn sized_int_key_domain_refines_on_the_uint_arm() {
        // waldo = { 1: uint, * uint .size 1 => text }. The declared uint key 1 makes the uint arm a
        // match, so an unknown uint key reaches the catch-all, rewinds, and is re-read as a `u8`.
        // { 1: 0, 255: "a" } round-trips; { 1: 0, 256: "a" } is out of the domain and errors.
        let wire = bytes("a2 0100 18ff 6161");
        let w = Waldo::from_cbor_bytes(&wire).unwrap();
        assert_eq!(w.rest().len(), 1);
        assert_eq!(w.rest().get(&255u8).unwrap(), "a");
        assert_eq!(w.to_cbor_bytes(), wire);
        assert!(
            Waldo::from_cbor_bytes(&bytes("a2 0100 190100 6161")).is_err(),
            "a key past the u8 range is a parse error, not a capture"
        );
    }

    #[test]
    fn duplicates_preserve_keeps_typed_duplicates_in_wire_order() {
        // garply = { 1: uint, * md => uint ; @duplicates preserve } — the PairMap twin.
        // { 1: 7, 5: 1, 5: 2, "a": 3 }: the repeated `5` is kept, and the pair list re-emits in wire
        // order (no key sort), so the bytes round-trip exactly.
        let wire = bytes("a4 01 07 05 01 05 02 6161 03");
        let g = Garply::from_cbor_bytes(&wire).unwrap();
        assert_eq!(g.rest().len(), 3);
        assert_eq!(
            g.rest().get_all(&Md::new_int(Int::new_uint(5))),
            vec![&1u64, &2u64]
        );
        assert_eq!(g.to_cbor_bytes(), wire);
    }
}
