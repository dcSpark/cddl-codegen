// Open struct-map IGNORE flavor (`@ignore` on the rest row) value-level end-to-end vectors. A
// trailing `* k => v ; @ignore` after fixed keys tolerate-and-DROPS unknown map entries: each unknown
// entry is typed-deserialized (key AND value — so the stream advances past nested containers, the
// cip36 hand-skip bug class) and then discarded. There is NO `rest` field, and serialize re-emits
// ONLY the declared members, so byte round-trips do NOT hold for wire data carrying unknown entries
// (lossy by declaration). These pin:
//   * a definite-length map with extra unknown entries (one with a NESTED container value) decodes,
//     the declared fields are correct (stream position), and re-serialize emits declared-only bytes;
//   * the same for an indefinite-length map;
//   * a wrong-domain key (`* uint => any` + a text key) still ERRORS (typing enforced, C-R1);
//   * fixed keys win over the rest row even on a value-type mismatch (§10.10 held);
//   * duplicate UNKNOWN keys are consumed silently (no dup tracking among dropped entries), while a
//     duplicate FIXED key still errors (existing DuplicateKey machinery, unchanged);
//   * a map with NO unknown entries is byte-identical to the closed-struct output;
//   * an `any` key domain drops every wire key type (uint/text/bytes/special/…).
// Every `wire` byte string is hand-written from the CBOR grammar, not copied from generator output.
#[cfg(test)]
mod open_struct_map_ignore {
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
    fn no_unknown_entries_equals_closed_struct_bytes() {
        // Foo::new carries only declared fields; there is no `rest` field to populate. Serializing it
        // is the closed `{ 1: 7, 2: "hi" }` (map of 2) — adding an `@ignore` row is wire-compatible.
        let f = Foo::new(7, "hi".to_string());
        let closed = bytes("a2 01 07 02 6268 69");
        assert_eq!(f.to_cbor_bytes(), closed);
        let round = Foo::from_cbor_bytes(&closed).unwrap();
        assert_eq!(round.key_1, 7);
        assert_eq!(round.key_2, "hi");
    }

    #[test]
    fn definite_extra_entries_dropped_stream_position_correct() {
        // { 1: 7, 2: "hi", 99: 5, 100: [1, 2] } — the two unknown uint entries (one with a NESTED
        // array value) are consumed and DROPPED. The declared fields must still read correctly (the
        // value deserialize advanced the stream past the nested container), and re-serialize emits the
        // declared-only `{ 1: 7, 2: "hi" }`.
        let wire = bytes("a4 01 07 02 6268 69 1863 05 1864 820102");
        let f = Foo::from_cbor_bytes(&wire).unwrap();
        assert_eq!(f.key_1, 7);
        assert_eq!(f.key_2, "hi");
        assert_eq!(f.to_cbor_bytes(), bytes("a2 01 07 02 6268 69"));
    }

    #[test]
    fn indefinite_extra_entries_dropped_stream_position_correct() {
        // The same content as an INDEFINITE-length map: bf … ff. The break terminator must be reached
        // after the dropped entries, and re-serialize normalizes to the declared-only definite map.
        let wire = bytes("bf 01 07 02 6268 69 1863 05 ff");
        let f = Foo::from_cbor_bytes(&wire).unwrap();
        assert_eq!(f.key_1, 7);
        assert_eq!(f.key_2, "hi");
        assert_eq!(f.to_cbor_bytes(), bytes("a2 01 07 02 6268 69"));
    }

    #[test]
    fn wrong_domain_key_errors() {
        // { 1: 7, 2: "hi", "x": 5 } — a text key is not in the `* uint => any` domain, so it ERRORS
        // (dropping is not skipping typing: `@ignore` runs the same typed deserialization as capture).
        let wire = bytes("a3 01 07 02 6268 69 6178 05");
        assert!(Foo::from_cbor_bytes(&wire).is_err());
    }

    #[test]
    fn fixed_keys_win_on_content_mismatch() {
        // bar = { ? 3: text, * uint => any ; @ignore }. Wire { 3: 5 } dispatches key 3 to the (text)
        // optional field, whose deserialize fails on the uint value — an ERROR, not a fallthrough to
        // the rest row (fixed-key dispatch wins even on content mismatch, §10.10, held for ignore).
        assert!(Bar::from_cbor_bytes(&bytes("a1 03 05")).is_err());
        // { 3: "ok" } binds the optional field; serialize re-emits it.
        let b = Bar::from_cbor_bytes(&bytes("a1 03 626f6b")).unwrap();
        assert_eq!(b.to_cbor_bytes(), bytes("a1 03 626f6b"));
        // { 7: 5 } (key 3 absent) DROPS the unknown 7 => 5; re-serialize is the empty map.
        let b2 = Bar::from_cbor_bytes(&bytes("a1 07 05")).unwrap();
        assert_eq!(b2.to_cbor_bytes(), bytes("a0"));
    }

    #[test]
    fn duplicate_unknown_keys_consumed_silently_fixed_still_errors() {
        // { 1: 7, 2: "hi", 99: 5, 99: 6 } — two UNKNOWN entries with the same key. `@ignore` keeps no
        // container, so there is nothing to detect a duplicate against: both are dropped, decode OK.
        let dup_unknown = bytes("a4 01 07 02 6268 69 1863 05 1863 06");
        let f = Foo::from_cbor_bytes(&dup_unknown).unwrap();
        assert_eq!(f.key_1, 7);
        assert_eq!(f.to_cbor_bytes(), bytes("a2 01 07 02 6268 69"));
        // { 1: 7, 1: 8, 2: "hi" } — a repeated DECLARED key is still a DuplicateKey error (unchanged).
        assert!(Foo::from_cbor_bytes(&bytes("a3 01 07 01 08 02 6268 69")).is_err());
    }

    #[test]
    fn any_domain_drops_every_wire_key_type() {
        // baz = { 1: uint, * any => any ; @ignore }. Wire { 1: 0, "t": 1, h'ff': 2, true: 3 } — a
        // text key, a bytes key, and a special (bool) key are all consumed and dropped through the
        // AnyCbor skip primitive. Only the declared `1: 0` survives re-serialization.
        let wire = bytes("a4 01 00 6174 01 41ff 02 f5 03");
        let b = Baz::from_cbor_bytes(&wire).unwrap();
        assert_eq!(b.key_1, 0);
        assert_eq!(b.to_cbor_bytes(), bytes("a1 01 00"));
    }

    // JSON side (C-R6): an `@ignore` struct is a CLOSED struct — serialize emits declared fields only
    // (no flatten machinery), and on read unknown JSON keys are tolerated per serde's default
    // ignore-unknown-fields behavior (the documented JSON-side mirror of the CBOR looseness).
    #[test]
    fn json_is_closed_struct_and_tolerates_unknown_keys() {
        let f = Foo::new(7, "hi".to_string());
        let json = serde_json::to_string(&f).unwrap();
        assert!(json.contains("\"key_1\":7"), "declared field present: {json}");
        assert!(json.contains("\"key_2\":\"hi\""), "declared field present: {json}");
        assert!(!json.contains("rest"), "no rest surface on an ignore struct: {json}");
        // An unknown JSON key on read is tolerated (serde default) and dropped — declared fields bind.
        let read: Foo = serde_json::from_str(r#"{"key_1":1,"key_2":"hey","99":5}"#).unwrap();
        assert_eq!(read.key_1, 1);
        assert_eq!(read.key_2, "hey");
        assert_eq!(serde_json::to_string(&read).unwrap(), r#"{"key_1":1,"key_2":"hey"}"#);
    }
}
