// Custom-serialize CANONICAL vectors. Under `--canonical-form` a map key is serialized twice: once
// into a LOCAL scratch `Serializer` (`buf`) to produce the sort key, and once into the real
// serializer for the write. A `@custom_serialize` target is a free function, so the scratch call
// must pass `&mut buf` — emitting the bare `buf` there is an E0308 that makes the whole crate
// uncompilable, which is why this fixture COMPILING is the regression pin for both positions below.
// The vectors then pin the property the two calls must share: the bytes the merge sorts by are the
// bytes the write arm emits. Every `wire` byte string is hand-written from the CBOR grammar.
#[cfg(test)]
mod custom_serialize_canonical {
    use super::*;
    use serialization::{Deserialize, Serialize};

    fn bytes(hex: &str) -> Vec<u8> {
        let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
        (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect()
    }

    // ---------------------------------------------------------------------------------------
    // Table position: `{ * hexid => uint }`, key written as lowercase hex TEXT by the custom pair.
    // ---------------------------------------------------------------------------------------

    #[test]
    fn table_custom_key_byte_exact() {
        // [ { "0a0b": 1(two-byte), "ff"(two-byte key head): 5 } ] with a non-minimal array head and a
        // non-minimal map head. Byte-exactness needs every width to ride a sidecar THROUGH the custom
        // key codec: the string encoding handed to `write_hex_key` is the one `read_hex_key` recorded
        // off the wire text, and the hex text's length (2n) — not the decoded byte count (n) — is what
        // reconstitutes the head.
        let wire = bytes("9801 b802 6430613062 1801 78026666 05");
        let v = TableHolder::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.table.len(), 2);
        assert_eq!(v.table.get(&bytes("0a0b")).copied(), Some(1));
        assert_eq!(v.table.get(&bytes("ff")).copied(), Some(5));
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact table round-trip");
    }

    #[test]
    fn table_canonical_sorts_by_custom_written_key_bytes() {
        // The canonical key-sort pass writes each key into the scratch `buf` through `write_hex_key`,
        // so the order is over the HEX TEXT encodings, not the in-memory bytes: "ff" is `62 6666`
        // (3 bytes) and "0a0b" is `64 30613062` (5), so length-first puts "ff" first — the reverse of
        // the wire order the value was parsed in.
        let wire = bytes("9801 b802 6430613062 1801 78026666 05");
        let v = TableHolder::from_cbor_bytes(&wire).unwrap();
        let canonical = bytes("81 a2 626666 05 6430613062 01");
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            canonical,
            "canonical table reorders by the custom-written key bytes"
        );
        // The sort key and the written key are the same call: re-parsing the canonical output and
        // re-canonicalizing must be a fixed point, and the plain round-trip of canonical input is the
        // canonical bytes themselves.
        let reparsed = TableHolder::from_cbor_bytes(&canonical).unwrap();
        assert_eq!(reparsed.to_cbor_bytes(), canonical, "canonical re-parse round-trips");
        assert_eq!(
            reparsed.to_canonical_cbor_bytes(),
            canonical,
            "canonical output is a fixed point"
        );
    }

    #[test]
    fn table_custom_key_deserialize_rejects_non_hex() {
        // The deserialize leg is the custom pair's other half: a text key that is not hex fails in
        // `read_hex_key` rather than silently landing in the map.
        assert!(
            TableHolder::from_cbor_bytes(&bytes("81 a1 627a7a 01")).is_err(),
            "non-hex table key rejected by the custom deserializer"
        );
    }

    // ---------------------------------------------------------------------------------------
    // Table VALUE position: `{ * uint => hexid }`, the value written as lowercase hex TEXT. The
    // value leg is off the canonical sort path (a map sorts by its keys) but on the write path, so
    // it is the position a key-only fix would leave behind.
    // ---------------------------------------------------------------------------------------

    #[test]
    fn table_custom_value_byte_exact() {
        // [ { 2(two-byte key head): "cafe"(one-byte text head), 1: "f00d" } ] with a non-minimal
        // array head and a non-minimal map head. The value is decoded bytes in memory and hex TEXT
        // on the wire, so byte-exactness needs the VALUE sidecar — keyed by the entry's uint key,
        // not by the value — to ride back through `write_hex_key`.
        let wire = bytes("9801 b802 190002 780463616665 01 6466303064");
        let v = ValueTableHolder::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.table.len(), 2);
        assert_eq!(v.table.get(&2).cloned(), Some(bytes("cafe")));
        assert_eq!(v.table.get(&1).cloned(), Some(bytes("f00d")));
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact table-value round-trip");
    }

    #[test]
    fn table_canonical_reminimizes_custom_written_values() {
        // Canonical reorders by the KEY bytes (`01` before `02`) and re-minimizes every head —
        // including the head the custom VALUE writer emits, since `force_canonical` reaches
        // `write_hex_key` as its trailing argument. So the 1-byte text head recorded for "cafe"
        // collapses to the inline head, which a value leg that ignored `force_canonical` would not
        // do.
        let wire = bytes("9801 b802 190002 780463616665 01 6466303064");
        let v = ValueTableHolder::from_cbor_bytes(&wire).unwrap();
        let canonical = bytes("81 a2 01 6466303064 02 6463616665");
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            canonical,
            "canonical re-minimizes the custom-written value heads and orders by key"
        );
        let reparsed = ValueTableHolder::from_cbor_bytes(&canonical).unwrap();
        assert_eq!(reparsed.to_cbor_bytes(), canonical, "canonical re-parse round-trips");
        assert_eq!(
            reparsed.to_canonical_cbor_bytes(),
            canonical,
            "canonical output is a fixed point"
        );
    }

    #[test]
    fn table_custom_value_deserialize_rejects_default_shape() {
        // The DEFAULT writer for a `hexid` value emits a CBOR byte string; the custom reader wants
        // TEXT. So a value leg that reverted to the default writer fails the round-trip outright
        // rather than differing cosmetically. The control isolates the position as the variable.
        assert!(
            ValueTableHolder::from_cbor_bytes(&bytes("81 a1 01 42cafe")).is_err(),
            "the default writer's byte-string shape is rejected in the table VALUE range"
        );
        assert!(
            ValueTableHolder::from_cbor_bytes(&bytes("81 a1 01 6463616665")).is_ok(),
            "control: the custom writer's own hex-TEXT shape parses"
        );
    }

    // ---------------------------------------------------------------------------------------
    // Open struct-map rest row whose key domain alias carries the custom pair — the SHIPPED
    // open-struct-map feature's exposure to the same scratch-buffer call site (its canonical merge).
    // ---------------------------------------------------------------------------------------

    #[test]
    fn rest_row_custom_key_byte_exact() {
        // {"ab": 7(two-byte), "zzz": 1, "abcd": 2} with a non-minimal map head, the declared key
        // sitting BETWEEN two rest entries on the wire. Preserve must reproduce the exact bytes and
        // the exact wire order (orig_deser_order interleave) with the rest keys written through
        // `write_ext_key`.
        let wire = bytes("b803 626162 1807 637a7a7a 01 6461626364 02");
        let v = OpenExt::from_cbor_bytes(&wire).unwrap();
        assert_eq!(v.zzz, 1);
        assert_eq!(v.rest.len(), 2);
        assert_eq!(v.to_cbor_bytes(), wire, "byte-exact rest-row round-trip");
    }

    #[test]
    fn rest_row_canonical_merge_interleaves_declared_key() {
        // The merge sorts the declared key's baked bytes against each rest key's SCRATCH-serialized
        // bytes (the `&mut buf` call site). Length-first: "ab" `62 6162` (3) < "zzz" `63 7a7a7a` (4) <
        // "abcd" `64 61626364` (5), so one rest entry lands on each side of the declared key.
        let wire = bytes("b803 626162 1807 637a7a7a 01 6461626364 02");
        let v = OpenExt::from_cbor_bytes(&wire).unwrap();
        let canonical = bytes("a3 626162 07 637a7a7a 01 6461626364 02");
        assert_eq!(
            v.to_canonical_cbor_bytes(),
            canonical,
            "canonical merge orders rest keys around the declared key"
        );
        let reparsed = OpenExt::from_cbor_bytes(&canonical).unwrap();
        assert_eq!(reparsed.to_cbor_bytes(), canonical, "canonical re-parse round-trips");
        assert_eq!(
            reparsed.to_canonical_cbor_bytes(),
            canonical,
            "canonical output is a fixed point"
        );
    }

    #[test]
    fn rest_row_custom_serializer_refusal_surfaces_from_both_call_sites() {
        // `write_ext_key` refuses a non-lowercase key. That refusal is observable from BOTH sites the
        // fix is about: under `force_canonical` the merge's scratch pass reaches it before anything is
        // written, and without it the write arm does. A key built in memory (not parsed) keeps this a
        // statement about the serialize path only.
        let mut v = OpenExt::new(1);
        v.rest.insert("AB".to_owned(), 2);
        let mut canonical_buf = cbor_event::se::Serializer::new_vec();
        assert!(
            v.serialize(&mut canonical_buf, true).is_err(),
            "canonical merge's scratch pass runs the custom serializer"
        );
        let mut plain_buf = cbor_event::se::Serializer::new_vec();
        assert!(
            v.serialize(&mut plain_buf, false).is_err(),
            "write arm runs the custom serializer"
        );
    }

    // ---------------------------------------------------------------------------------------
    // Whole-table rule: a wrapper owns a complete custom codec rather than a table's key/value
    // aliases owning their positions. This has no inferred encoding tuple; the canonical flag is
    // nevertheless required by its hand-code signature, and the direct and embedded paths must
    // both call it.
    // ---------------------------------------------------------------------------------------

    #[test]
    fn whole_table_custom_pair_is_canonical_direct_and_embedded() {
        let map = OrderedHashMap::from_iter([("left".to_owned(), 3), ("right".to_owned(), 7)]);
        let table = CustomTable::from(map);
        let direct = bytes("84 646c656674 03 657269676874 07");
        assert_eq!(table.to_cbor_bytes(), direct, "direct custom table wire");
        assert_eq!(
            table.to_canonical_cbor_bytes(),
            direct,
            "the canonical wrapper call reaches the complete custom pair"
        );
        assert_eq!(
            CustomTable::from_cbor_bytes(&direct).unwrap().get(),
            table.get(),
            "direct custom table decode retains all entries"
        );

        let holder = CustomTableHolder::new(table);
        let embedded = bytes("81 84 646c656674 03 657269676874 07");
        assert_eq!(holder.to_canonical_cbor_bytes(), embedded);
        assert_eq!(
            CustomTableHolder::from_cbor_bytes(&embedded)
                .unwrap()
                .table
                .get(),
            holder.table.get(),
            "embedded custom table decode retains all entries"
        );
    }
}
