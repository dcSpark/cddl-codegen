// CORE_TESTS_TRUNCATION_ANCHOR — do NOT remove.
// This file is appended to the generated crate's `generated/mod.rs`. The `no_alias` and `docs` tests below
// read that file and inspect the *production* source, so they must strip everything from this line down
// (their own asserted literals live below it). They truncate at this unique anchor rather than at the
// first `#[cfg(test)]`, which is brittle: an emitted `#[cfg(test)]` module (e.g. --emit-tests) or any
// earlier literal can move that boundary. The tests build the anchor via `concat!` of split fragments
// so the contiguous marker text appears ONLY here (nowhere in generated production output, and not in
// the tests' own source), making this the sole match and the "anchor missing" failure path real.
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
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.position(), orig_bytes.len());
    }

    /// Assert that `bytes` is REJECTED, and that the rejection is the one the vector claims — the
    /// discriminated form of a bare `assert!(T::from_cbor_bytes(&bytes).is_err())`.
    ///
    /// A bare `is_err()` accepts ANY failure, so a hand-derived vector that fails EARLIER than the
    /// boundary it was written to prove (one byte off; a wrong major type reached before the check
    /// ever runs) stays green while the pinned boundary goes unexercised — outcome right,
    /// provenance wrong, invisible to every gate by construction. Pinning a distinctive substring
    /// of the message makes the provenance part of what the test asserts, and prints the real
    /// message when it moves. A substring only discriminates failures whose messages DIFFER: two
    /// defects sharing one message remain indistinguishable to it.
    ///
    /// Spelled IDENTICALLY in every fixture `tests.rs` that uses it, deliberately: each such file
    /// is appended standalone into its own generated crate, so there is no module a shared
    /// definition could live in and no import that could reach one.
    fn assert_decode_reject_reason<T: Deserialize>(bytes: &[u8], reason_substring: &str) {
        let err = T::from_cbor_bytes(bytes)
            .map(|_| ())
            .expect_err("expected this input to be REJECTED, but it decoded successfully");
        let msg = err.to_string();
        assert!(
            msg.contains(reason_substring),
            "rejected for the WRONG reason: expected a message containing \
             {reason_substring:?}, got: {msg}"
        );
    }

    #[test]
    fn hash() {
        let hash = Hash::new(vec![0xBA, 0xAD, 0xF0, 0x0D, 0xDE, 0xAD, 0xBE, 0xEF]).unwrap();
        deser_test(&hash);
        assert!(Hash::new(vec![0x00, 0xBA, 0xAD, 0xF0, 0x0D, 0xDE, 0xAD, 0xBE, 0xEF]).is_err());
    }

    #[test]
    fn foo() {
        deser_test(&Foo::new(436, String::from("jfkdsjfd"), vec![1, 1, 1]));
    }

    #[test]
    fn foo2_some() {
        deser_test(&Foo2::new(143546, Some(String::from("afdjfkjsiefefe").into())));
    }

    #[test]
    fn foo2_none() {
        deser_test(&Foo2::new(143546, None));
    }

    // Optional fixed-value members: a `pub <name>: bool` presence field (false=absent, true=present).
    // Array rep spans bool (non-final), uint (final), null (final Special-peek); the constant is on
    // the wire iff the bool is true, and the length term flips with it. Wrong constant -> reject.
    #[test]
    fn opt_fixed_member_array() {
        // absent: presence defaults false -> wire is [a, b] (no bool element)
        let absent = OptFixedArr::new(5, String::from("hi"));
        assert!(!absent.bfix);
        let absent_bytes = absent.to_cbor_bytes();
        assert_eq!(
            absent_bytes,
            [
                arr_def(2),
                cbor_int(5, cbor_event::Sz::Inline),
                cbor_string("hi")
            ]
            .concat()
        );
        deser_test(&absent);
        assert!(!OptFixedArr::from_cbor_bytes(&absent_bytes).unwrap().bfix);
        // present: set true -> wire is [a, true, b] (the fixed `true`, 0xf5, appears)
        let mut present = OptFixedArr::new(5, String::from("hi"));
        present.bfix = true;
        let present_bytes = present.to_cbor_bytes();
        assert_eq!(
            present_bytes,
            [
                arr_def(3),
                cbor_int(5, cbor_event::Sz::Inline),
                vec![0xf5u8],
                cbor_string("hi")
            ]
            .concat()
        );
        deser_test(&present);
        assert!(OptFixedArr::from_cbor_bytes(&present_bytes).unwrap().bfix);
        // the WRONG constant (false where true expected) rejects with FixedValueMismatch —
        // reason-asserted so an earlier unrelated rejection can't silently absorb this pin
        let wrong = [
            arr_def(3),
            cbor_int(5, cbor_event::Sz::Inline),
            vec![0xf4u8],
            cbor_string("hi"),
        ]
        .concat();
        let wrong_err = OptFixedArr::from_cbor_bytes(&wrong).unwrap_err();
        assert!(
            wrong_err.to_string().contains("Expected fixed value"),
            "{wrong_err}"
        );

        // null in final position (the possibly-last Special-peek path)
        let null_absent = OptFixedArrNull::new(7);
        assert_eq!(
            null_absent.to_cbor_bytes(),
            [arr_def(1), cbor_int(7, cbor_event::Sz::Inline)].concat()
        );
        deser_test(&null_absent);
        let mut null_present = OptFixedArrNull::new(7);
        null_present.nfix = true;
        assert_eq!(
            null_present.to_cbor_bytes(),
            [arr_def(2), cbor_int(7, cbor_event::Sz::Inline), vec![0xf6u8]].concat()
        );
        deser_test(&null_present);

        // uint in final position
        let uint_absent = OptFixedArrLast::new(String::from("k"));
        deser_test(&uint_absent);
        let mut uint_present = OptFixedArrLast::new(String::from("k"));
        uint_present.ufix = true;
        assert_eq!(
            uint_present.to_cbor_bytes(),
            [
                arr_def(2),
                cbor_string("k"),
                cbor_int(5, cbor_event::Sz::Inline)
            ]
            .concat()
        );
        deser_test(&uint_present);
    }

    // Map rep: each optional fixed value is a keyed entry; presence is per-key. Spans uint/text/nint/
    // bool/null. Absent keys drop the entry (and shrink the map length); a present key with the wrong
    // constant rejects.
    #[test]
    fn opt_fixed_member_map() {
        // absent: all presence false -> wire is {a: 5} (single entry)
        let absent = OptFixedMap::new(5);
        assert!(
            !absent.m_uint && !absent.m_text && !absent.m_nint && !absent.m_bool && !absent.m_null
        );
        assert_eq!(
            absent.to_cbor_bytes(),
            [
                map_def(1),
                cbor_string("a"),
                cbor_int(5, cbor_event::Sz::Inline)
            ]
            .concat()
        );
        deser_test(&absent);
        // all present -> byte round-trips and every presence bit survives
        let mut all = OptFixedMap::new(5);
        all.m_uint = true;
        all.m_text = true;
        all.m_nint = true;
        all.m_bool = true;
        all.m_null = true;
        deser_test(&all);
        let all_deser = OptFixedMap::from_cbor_bytes(&all.to_cbor_bytes()).unwrap();
        assert!(
            all_deser.m_uint
                && all_deser.m_text
                && all_deser.m_nint
                && all_deser.m_bool
                && all_deser.m_null
        );
        // one present -> only that key's bit is set
        let mut one = OptFixedMap::new(5);
        one.m_bool = true;
        deser_test(&one);
        let one_deser = OptFixedMap::from_cbor_bytes(&one.to_cbor_bytes()).unwrap();
        assert!(one_deser.m_bool && !one_deser.m_uint && !one_deser.m_null);
        // present key with the WRONG constant (m_uint => 6, expected 5) rejects
        let wrong = [
            map_def(2),
            cbor_string("a"),
            cbor_int(5, cbor_event::Sz::Inline),
            cbor_string("m_uint"),
            cbor_int(6, cbor_event::Sz::Inline),
        ]
        .concat();
        assert_decode_reject_reason::<OptFixedMap>(&wrong, "Expected fixed value 5 found 6");
        // present NINT key with the WRONG constant (m_nint => -8, expected -7). The message must
        // name the value the CDDL AUTHORED on BOTH sides, so it is greppable against the spec.
        // A `Key` with no signed variant forces the nint through its CBOR wire representation
        // (`-1-N`), which rendered this exact vector as "Expected fixed value 6 found 7" —
        // arithmetically correct for the wire, and findable nowhere in the .cddl the user wrote.
        let wrong_nint = [
            map_def(2),
            cbor_string("a"),
            cbor_int(5, cbor_event::Sz::Inline),
            cbor_string("m_nint"),
            cbor_int(-8, cbor_event::Sz::Inline),
        ]
        .concat();
        let wrong_nint_err = OptFixedMap::from_cbor_bytes(&wrong_nint).unwrap_err();
        assert!(
            wrong_nint_err
                .to_string()
                .contains("Expected fixed value -7 found -8"),
            "{wrong_nint_err}"
        );
    }

    // Optional fixed FLOAT member: identical `bool` presence model, but the constant is a Special
    // float — written at the smallest head that preserves it (RFC 8949 §4.1) and verified
    // `raw.float()? != 2.5` -> FixedValueMismatch. The read is a VALUE comparison at any head, so
    // the write is free to take the preferred width. Default profile only (floats are unimplemented
    // under --preserve-encodings). Present/absent byte round-trip + wrong-value reject, array + map.
    #[test]
    fn opt_fixed_member_float() {
        // 2.5 = f9 41 00 (its shortest form); 1.5 = f9 3e 00
        let fb_2_5 = [0xf9u8, 0x41, 0x00];
        let fb_1_5 = [0xf9u8, 0x3e, 0x00];

        // array: absent -> [a, b] (no float element)
        let absent = OptFixedArrFloat::new(5, String::from("hi"));
        assert!(!absent.ffix);
        assert_eq!(
            absent.to_cbor_bytes(),
            [
                arr_def(2),
                cbor_int(5, cbor_event::Sz::Inline),
                cbor_string("hi")
            ]
            .concat()
        );
        deser_test(&absent);
        assert!(
            !OptFixedArrFloat::from_cbor_bytes(&absent.to_cbor_bytes())
                .unwrap()
                .ffix
        );
        // present: set true -> [a, 2.5, b] (the fixed float appears) and the presence bit survives
        let mut present = OptFixedArrFloat::new(5, String::from("hi"));
        present.ffix = true;
        let present_bytes = present.to_cbor_bytes();
        assert_eq!(
            present_bytes,
            [
                arr_def(3),
                cbor_int(5, cbor_event::Sz::Inline),
                fb_2_5.to_vec(),
                cbor_string("hi")
            ]
            .concat()
        );
        deser_test(&present);
        assert!(
            OptFixedArrFloat::from_cbor_bytes(&present_bytes)
                .unwrap()
                .ffix
        );
        // WRONG constant (1.5 where 2.5 expected) -> FixedValueMismatch, reason-asserted so an
        // earlier unrelated rejection can't silently absorb this pin
        let wrong = [
            arr_def(3),
            cbor_int(5, cbor_event::Sz::Inline),
            fb_1_5.to_vec(),
            cbor_string("hi"),
        ]
        .concat();
        let wrong_err = OptFixedArrFloat::from_cbor_bytes(&wrong).unwrap_err();
        assert!(
            wrong_err.to_string().contains("Expected fixed value"),
            "{wrong_err}"
        );

        // map: absent -> {a: 5}; present -> the float entry appears and the presence bit survives
        let map_absent = OptFixedMapFloat::new(5);
        assert!(!map_absent.m_float);
        assert_eq!(
            map_absent.to_cbor_bytes(),
            [
                map_def(1),
                cbor_string("a"),
                cbor_int(5, cbor_event::Sz::Inline)
            ]
            .concat()
        );
        deser_test(&map_absent);
        let mut map_present = OptFixedMapFloat::new(5);
        map_present.m_float = true;
        assert_eq!(
            map_present.to_cbor_bytes(),
            [
                map_def(2),
                cbor_string("a"),
                cbor_int(5, cbor_event::Sz::Inline),
                cbor_string("m_float"),
                fb_2_5.to_vec(),
            ]
            .concat()
        );
        deser_test(&map_present);
        assert!(
            OptFixedMapFloat::from_cbor_bytes(&map_present.to_cbor_bytes())
                .unwrap()
                .m_float
        );
    }

    // Round-trip tests only ever feed well-formed CBOR; these pin that *malformed* input is
    // rejected rather than silently accepted. Structural cases the
    // bounds test doesn't reach: wrong shape, wrong element type, wrong/missing tag. Each case has
    // an accept baseline so a reject can't pass for the wrong reason (e.g. garbage encoding).
    #[test]
    fn structural_rejects() {
        // Foo = [uint, text, bytes]
        let bytes3 = vec![0x43u8, 1, 2, 3]; // cbor bytes(3)
        let foo_ok = [arr_def(3), cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone()].concat();
        Foo::from_cbor_bytes(&foo_ok).unwrap();
        assert_decode_reject_reason::<Foo>(&[], "not enough bytes"); // empty input
        // trailing bytes after a complete value are rejected, not silently ignored (from_cbor_bytes
        // checks the cursor reached the end of the buffer)
        let foo_trailing_err = Foo::from_cbor_bytes(&[foo_ok.clone(), vec![0xff]].concat()).unwrap_err();
        assert!(foo_trailing_err.to_string().contains("trailing data"), "{foo_trailing_err}");
        // array too short: the bytes field is missing
        assert_decode_reject_reason::<Foo>(&[arr_def(2), cbor_int(1, cbor_event::Sz::Inline), cbor_string("a")].concat(), "Definite length mismatch: found 2");
        // wrong outer container: a map where the array is required
        assert_decode_reject_reason::<Foo>(&[map_def(3), cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone()].concat(), "expected `Array' byte received `Map'");
        // wrong type in the uint slot (text where a uint is required)
        assert_decode_reject_reason::<Foo>(&[arr_def(3), cbor_string("x"), cbor_string("a"), bytes3.clone()].concat(), "expected `UnsignedInteger' byte received `Text'");
        // wrong type in the text slot (bytes where text is required)
        assert_decode_reject_reason::<Foo>(&[arr_def(3), cbor_int(1, cbor_event::Sz::Inline), bytes3.clone(), bytes3.clone()].concat(), "expected `Text' byte received `Bytes'");
        // wrong type in the bytes slot (uint where bytes is required)
        assert_decode_reject_reason::<Foo>(&[arr_def(3), cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), cbor_int(7, cbor_event::Sz::Inline)].concat(), "expected `Bytes' byte received `UnsignedInteger'");

        // `bytes .cbor T` says the byte string IS T's encoding, so bytes left over after the payload
        // are not a value the type admits — the same fact the top-level `from_cbor_bytes` check
        // above states, one level in, and raising the same error. Silently accepting them was worse
        // than over-acceptance under --preserve-encodings: the value re-encoded WITHOUT the leftover
        // bytes, so an accepted input round-tripped to different bytes. Found by the byte fuzzer
        // (`fuzz/README.md` § "Findings disposition").
        //
        // Both `.cbor` seams are pinned, because they are separate spellings of the payload:
        // `foo_bytes = bytes .cbor foo` is a `.cbor` RULE BODY (a wrapper struct of its own), and
        // `uint_bytes: bytes .cbor uint` inside `cbor_in_cbor` is a `.cbor` MEMBER EXPRESSION.
        // Each has an accept baseline one byte shorter, so only the leftover byte can reject it.
        let foo_bytes_ok = [vec![0x40 | foo_ok.len() as u8], foo_ok.clone()].concat();
        FooBytes::from_cbor_bytes(&foo_bytes_ok).unwrap();
        let foo_bytes_trailing = [vec![0x40 | (foo_ok.len() + 1) as u8], foo_ok.clone(), vec![0x00]].concat();
        let foo_bytes_trailing_err = FooBytes::from_cbor_bytes(&foo_bytes_trailing).unwrap_err();
        assert!(foo_bytes_trailing_err.to_string().contains("trailing data"), "{foo_bytes_trailing_err}");

        // CborInCbor = [foo_bytes, uint_bytes: bytes .cbor uint, tagged_foo_bytes]
        let cbor_in_cbor = |uint_payload: Vec<u8>| {
            CborInCbor::from_cbor_bytes(&[
                arr_def(3),
                foo_bytes_ok.clone(),
                [vec![0x40 | uint_payload.len() as u8], uint_payload].concat(),
                cbor_tag_sz(20, cbor_event::Sz::Inline),
                foo_bytes_ok.clone(),
            ].concat())
        };
        cbor_in_cbor(cbor_int(9, cbor_event::Sz::Inline)).unwrap();
        let uint_bytes_trailing_err =
            cbor_in_cbor([cbor_int(9, cbor_event::Sz::Inline), vec![0x00]].concat()).unwrap_err();
        assert!(uint_bytes_trailing_err.to_string().contains("trailing data"), "{uint_bytes_trailing_err}");

        // Foo2 = #6.23([uint, opt_text]): the tag must be present and correct.
        let foo2 = |tag: Option<u64>| {
            let mut b = Vec::new();
            if let Some(t) = tag {
                b.extend(cbor_tag_sz(t, cbor_event::Sz::Inline));
            }
            b.extend([arr_def(2), cbor_int(1, cbor_event::Sz::Inline), vec![NULL]].concat());
            Foo2::from_cbor_bytes(&b)
        };
        foo2(Some(23)).unwrap();
        assert!(foo2(Some(22)).is_err()); // wrong tag
        assert!(foo2(None).is_err()); // missing tag

        // Hash = bytes .size (0..8): wrong major type (uint where bytes is required).
        Hash::from_cbor_bytes(&bytes3).unwrap();
        assert_decode_reject_reason::<Hash>(&cbor_int(5, cbor_event::Sz::Inline), "expected `Bytes' byte received `UnsignedInteger'");

        // WrapperTable = { * uint => uint }: wrong major type (array where a map is required).
        let wrapper_table_ok = [map_def(1), cbor_int(1, cbor_event::Sz::Inline), cbor_int(2, cbor_event::Sz::Inline)].concat();
        WrapperTable::from_cbor_bytes(&wrapper_table_ok).unwrap();
        assert_decode_reject_reason::<WrapperTable>(&arr_def(0), "expected `Map' byte received `Array'");

        // Duplicate map keys are rejected (DeserializeFailure::DuplicateKey).
        // WrapperTable = { * uint => uint } is a definite-map table (no read_elems pre-check), so the
        // duplicate is caught directly when the second identical key fails to insert. Baseline: the
        // same two-entry map with distinct keys round-trips, so only the repeated key can reject it.
        let wrapper_table_two_keys = [
            map_def(2),
            cbor_int(1, cbor_event::Sz::Inline), cbor_int(2, cbor_event::Sz::Inline),
            cbor_int(7, cbor_event::Sz::Inline), cbor_int(8, cbor_event::Sz::Inline),
        ].concat();
        WrapperTable::from_cbor_bytes(&wrapper_table_two_keys).unwrap();
        let wrapper_table_dup = [
            map_def(2),
            cbor_int(1, cbor_event::Sz::Inline), cbor_int(2, cbor_event::Sz::Inline),
            cbor_int(1, cbor_event::Sz::Inline), cbor_int(8, cbor_event::Sz::Inline),
        ].concat();
        let wrapper_table_dup_err = WrapperTable::from_cbor_bytes(&wrapper_table_dup).unwrap_err();
        assert!(wrapper_table_dup_err.to_string().contains("Duplicate key"), "{wrapper_table_dup_err}");

        // TableArrMembers is a struct-map keyed by its text field names. A *definite* map can't carry
        // a duplicate while staying complete (the extra entry trips DefiniteLenMismatch before the
        // loop), so the DuplicateKey path is reached only via an indefinite map. Empty inner map/array
        // are valid values for the field types, so the baseline round-trips.
        let table_arr_members_ok = [
            vec![MAP_INDEF],
                cbor_string("tab"), map_def(0),
                cbor_string("arr"), arr_def(0),
                cbor_string("arr2"), arr_def(0),
            vec![BREAK],
        ].concat();
        TableArrMembers::from_cbor_bytes(&table_arr_members_ok).unwrap();
        let table_arr_members_dup = [
            vec![MAP_INDEF],
                cbor_string("tab"), map_def(0),
                cbor_string("tab"), map_def(0),
            vec![BREAK],
        ].concat();
        let table_arr_members_dup_err = TableArrMembers::from_cbor_bytes(&table_arr_members_dup).unwrap_err();
        assert!(table_arr_members_dup_err.to_string().contains("Duplicate key"), "{table_arr_members_dup_err}");

        // A required key absent from an indefinite struct-map trips MandatoryFieldMissing. A definite
        // map would fail its declared length first (DefiniteLenMismatch), so the omission is reached
        // only via an indefinite map. The all-keys map above is the accept baseline, so only dropping
        // "arr2" can reject this one.
        let table_arr_members_missing = [
            vec![MAP_INDEF],
                cbor_string("tab"), map_def(0),
                cbor_string("arr"), arr_def(0),
            vec![BREAK],
        ].concat();
        let table_arr_members_missing_err = TableArrMembers::from_cbor_bytes(&table_arr_members_missing).unwrap_err();
        assert!(table_arr_members_missing_err.to_string().contains("Mandatory field"), "{table_arr_members_missing_err}");

        // Length-framing errors. Foo = [uint, text, bytes] is read with read_elems(3) + finish().
        // A definite array whose header counts MORE than the 3 fields passes read_elems but trips
        // DefiniteLenMismatch at finish() (the "array too short" case above covers the under-count,
        // which fails earlier in read_elems). foo_ok (arr_def(3)) above is the accept baseline.
        let foo_too_long = [
            arr_def(4),
            cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone(),
            cbor_int(9, cbor_event::Sz::Inline),
        ].concat();
        let foo_too_long_err = Foo::from_cbor_bytes(&foo_too_long).unwrap_err();
        assert!(foo_too_long_err.to_string().contains("Definite length mismatch"), "{foo_too_long_err}");

        // An indefinite array must be terminated by a CBOR Break; any other special in the tail slot
        // trips EndingBreakMissing. The Break-terminated form is the accept baseline.
        let foo_indef_ok = [
            vec![ARR_INDEF],
            cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone(),
            vec![BREAK],
        ].concat();
        Foo::from_cbor_bytes(&foo_indef_ok).unwrap();
        let foo_indef_no_break = [
            vec![ARR_INDEF],
            cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone(),
            vec![NULL],
        ].concat();
        let foo_indef_no_break_err = Foo::from_cbor_bytes(&foo_indef_no_break).unwrap_err();
        assert!(foo_indef_no_break_err.to_string().contains("Missing ending CBOR Break"), "{foo_indef_no_break_err}");

        // A CBOR Break encountered while iterating a *definite*-length struct-map trips
        // BreakInDefiniteLen. The header must still count the 3 struct fields (or finish() rejects
        // the length first), so the definite map declares 3 and a Break is fed in element position.
        // A complete definite map of the 3 required keys is the accept baseline.
        let table_arr_members_def_ok = [
            map_def(3),
                cbor_string("tab"), map_def(0),
                cbor_string("arr"), arr_def(0),
                cbor_string("arr2"), arr_def(0),
        ].concat();
        TableArrMembers::from_cbor_bytes(&table_arr_members_def_ok).unwrap();
        let table_arr_members_break = [map_def(3), vec![BREAK]].concat();
        let table_arr_members_break_err = TableArrMembers::from_cbor_bytes(&table_arr_members_break).unwrap_err();
        assert!(table_arr_members_break_err.to_string().contains("Break while reading definite length sequence"), "{table_arr_members_break_err}");

        // Regression (fuzz-found DoS): the *collection* element loop (`[* uint]`, `{* uint => uint}`),
        // distinct from the struct-map path above, once did `assert_eq!(special, Break)` on ANY special
        // in element position — so a definite-length collection holding a non-Break special (e.g. a
        // `null`, `0x81 0xf6`) aborted the process instead of returning an error to the untrusted-input
        // parser this library's consumers rely on. It must now be a graceful Err. accept baselines
        // differ only in the offending element, so a reject can't pass for the wrong reason.
        // WrapperList = [ * uint ] ; @newtype
        // A non-Break major-type-7 value (here `null`) in a definite `[* uint]` element slot is a
        // graceful *type* rejection at the element (the uint read sees a `Special`), NOT a process
        // abort (the fuzz-found DoS this guards) and NOT the misleading "Break in definite length"
        // the previous over-broad `Type::Special` match produced (that same over-match rejected
        // legitimate `[* float64]`/`[* bool]` too). Definite collections no longer special-case a
        // break in element position; only indefinite ones look for the terminator. `unwrap_err()`
        // already proves graceful (no abort); the token pins the received-type so a regression to a
        // wrong reason is still caught.
        WrapperList::from_cbor_bytes(&[arr_def(1), cbor_int(1, cbor_event::Sz::Inline)].concat()).unwrap();
        let wrapper_list_null_elem = WrapperList::from_cbor_bytes(&[arr_def(1), vec![NULL]].concat()).unwrap_err();
        assert!(wrapper_list_null_elem.to_string().contains("Special"), "{wrapper_list_null_elem}");
        // WrapperTable = { * uint => uint }: same loop, null fed in key position.
        let wrapper_table_null_key = WrapperTable::from_cbor_bytes(&[map_def(1), vec![NULL], cbor_int(2, cbor_event::Sz::Inline)].concat()).unwrap_err();
        assert!(wrapper_table_null_key.to_string().contains("Special"), "{wrapper_table_null_key}");
    }

    // exercise the shipped Display formatting in error.rs (DeserializeError::fmt_indent and
    // Key::Display) that round-trip tests never reach. Each case pins stable substrings of
    // unwrap_err().to_string() rather than whole strings, so cosmetic wording tweaks don't break it
    // while the distinct formatting branches stay covered.
    #[test]
    fn error_display_formatting() {
        // TagMismatch with a Some(location): Foo2 = #6.23(...) builds its tag error via
        // DeserializeError::new("Foo2", ..), so Display takes the "failed in {loc} because:" branch
        // and the TagMismatch arm ("Expected tag {expected}, found {found}").
        let foo2_wrong_tag = [
            cbor_tag_sz(22, cbor_event::Sz::Inline),
            arr_def(2),
            cbor_int(1, cbor_event::Sz::Inline),
            vec![NULL],
        ]
        .concat();
        let foo2_tag_err = Foo2::from_cbor_bytes(&foo2_wrong_tag)
            .unwrap_err()
            .to_string();
        assert!(foo2_tag_err.contains("Foo2"), "{foo2_tag_err}");
        assert!(
            foo2_tag_err.contains("Expected tag 23, found 22"),
            "{foo2_tag_err}"
        );

        // annotate() chaining: a wrong inner tag in Foo2's opt_text field (= #6.42(text)) is
        // annotated "opt_text" then "Foo2", so the location reads "Foo2.opt_text". Tag 10 keeps the
        // header in the inline form (Sz::Inline only encodes 0..=23) while still mismatching 42.
        let foo2_inner_tag = [
            cbor_tag_sz(23, cbor_event::Sz::Inline),
            arr_def(2),
            cbor_int(1, cbor_event::Sz::Inline),
            cbor_tag_sz(10, cbor_event::Sz::Inline),
            cbor_string("x"),
        ]
        .concat();
        let foo2_inner_err = Foo2::from_cbor_bytes(&foo2_inner_tag)
            .unwrap_err()
            .to_string();
        assert!(foo2_inner_err.contains("Foo2.opt_text"), "{foo2_inner_err}");
        assert!(
            foo2_inner_err.contains("Expected tag 42, found 10"),
            "{foo2_inner_err}"
        );

        // DefiniteLenMismatch from finish() with a location: Foo = [uint, text, bytes] declared as
        // a 4-element array reads 3 then trips finish(). The record's header/length reads sit inside
        // the annotate closure, so this carries the "Foo" location (the Some(loc) Display branch);
        // the DefiniteLenMismatch still prints its ", expected:" sub-branch.
        let foo_too_long = [
            arr_def(4),
            cbor_int(1, cbor_event::Sz::Inline),
            cbor_string("a"),
            vec![0x43u8, 1, 2, 3],
            cbor_int(9, cbor_event::Sz::Inline),
        ]
        .concat();
        let foo_len_err = Foo::from_cbor_bytes(&foo_too_long).unwrap_err().to_string();
        assert!(foo_len_err.contains("Foo"), "{foo_len_err}");
        assert!(foo_len_err.contains("found 4"), "{foo_len_err}");
        assert!(foo_len_err.contains("expected: 3"), "{foo_len_err}");

        // None branch ("Deserialization: " — no location): TrailingData is raised by
        // `from_cbor_bytes` AFTER a complete value decodes, as
        // `DeserializeFailure::CBOR(cbor_event::Error::TrailingData).into()` with no annotate closure
        // anywhere in the call path — so it is locationless by construction and Display takes the
        // None branch. Feed a valid single-element WrapperList followed by a stray trailing byte.
        let trailing_no_loc_err = WrapperList::from_cbor_bytes(
            &[arr_def(1), cbor_int(1, cbor_event::Sz::Inline), vec![0x00]].concat(),
        )
        .unwrap_err()
        .to_string();
        assert!(
            trailing_no_loc_err.starts_with("Deserialization: "),
            "{trailing_no_loc_err}"
        );

        // Positive pin for the CLOSED newtype-wrapper container-read annotation gap: WrapperList's
        // whole deserialize body now sits inside an `.annotate("WrapperList")` closure, so a
        // wrong-container read (a bare uint where the `[* uint]` array is required) carries the
        // "WrapperList" location (was the old None-branch witness before the gap closed).
        let wrapper_wrong_container_err = WrapperList::from_cbor_bytes(&[0x00u8])
            .unwrap_err()
            .to_string();
        assert!(
            wrapper_wrong_container_err.contains("WrapperList"),
            "{wrapper_wrong_container_err}"
        );

        // MandatoryFieldMissing with a Key::Str: an empty indefinite Bar map drops every key; "foo"
        // is the first required field checked, so Key::Display wraps it in quotes ("\"foo\"") and the
        // outer annotate("Bar") supplies the location.
        let bar_empty = [vec![MAP_INDEF], vec![BREAK]].concat();
        let bar_err = Bar::from_cbor_bytes(&bar_empty).unwrap_err().to_string();
        assert!(bar_err.contains("Bar"), "{bar_err}");
        assert!(bar_err.contains("Mandatory field"), "{bar_err}");
        assert!(bar_err.contains("\"foo\""), "{bar_err}");
    }

    #[test]
    fn bar() {
        let mut bar = Bar::new(Foo::new(436, String::from("jfkdf"), vec![6, 4]), None, 3.3);
        deser_test(&bar);
        // tests @name
        bar.one = Some(10);

    }

    #[test]
    fn plain() {
        deser_test(&Plain::new(7576, String::from("wiorurri34h").into()));
    }

    #[test]
    fn plain_arrays() {
        let plain = Plain::new(7576, String::from("wiorurri34h").into());
        let plain_arrays = PlainArrays::new(
            plain.clone(),
            plain.clone(),
            vec![plain.clone(), plain.clone()]
        );
        deser_test(&plain_arrays);
        // need to make sure they are actually inlined!
        let bytes = vec![
            arr_def(4),
                // embedded
                cbor_tag(23),
                    cbor_int(7576, cbor_event::Sz::Two),
                cbor_tag_sz(42, cbor_event::Sz::One),
                    cbor_string("wiorurri34h"),
                // single
                arr_def(2),
                    cbor_tag(23),
                        cbor_int(7576, cbor_event::Sz::Two),
                    cbor_tag_sz(42, cbor_event::Sz::One),
                        cbor_string("wiorurri34h"),
                // multiple
                arr_def(2),
                    arr_def(2),
                        cbor_tag(23),
                            cbor_int(7576, cbor_event::Sz::Two),
                        cbor_tag_sz(42, cbor_event::Sz::One),
                            cbor_string("wiorurri34h"),
                    arr_def(2),
                        cbor_tag(23),
                            cbor_int(7576, cbor_event::Sz::Two),
                        cbor_tag_sz(42, cbor_event::Sz::One),
                            cbor_string("wiorurri34h"),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let from_bytes = PlainArrays::from_cbor_bytes(&bytes).unwrap();
        assert_eq!(from_bytes.to_cbor_bytes(), bytes);
        assert_eq!(plain_arrays.to_cbor_bytes(), bytes);
    }

    #[test]
    fn outer() {
        deser_test(&Outer::new(2143254, Plain::new(7576, String::from("wiorurri34h").into())));
    }

    #[test]
    fn table_arr_members() {
        let mut tab = std::collections::BTreeMap::new();
        tab.insert(String::from("43266556"), String::from("2k2j343"));
        tab.insert(String::from("213543254546565"), String::from("!!fjdj"));
        let mut foos = vec![
            Foo::new(0, String::from("Zero"), vec![]),
            Foo::new(2, String::from("Two"), vec![2, 2]),
        ];
        let u64s = vec![0, 1, 2, 3, 4, 6];
        deser_test(&TableArrMembers::new(tab, u64s, foos));
    }

    #[test]
    fn type_choice_0() {
        deser_test(&TypeChoice::I0);
    }

    #[test]
    fn type_choice_hello_world() {
        deser_test(&TypeChoice::Helloworld);
    }
    
    #[test]
    fn type_choice_uint() {
        deser_test(&TypeChoice::U64(53435364));
    }

    #[test]
    fn type_choice_text() {
        deser_test(&TypeChoice::Text(String::from("jdfidsf83j3  jkrjefdfk !!")));
    }

    #[test]
    fn type_choice_bytes() {
        deser_test(&TypeChoice::Bytes(vec![0x00, 0x01, 0xF7, 0xFF]));
    }

    #[test]
    fn type_choice_tagged_arr() {
        deser_test(&TypeChoice::ArrU64(vec![1, 2, 3, 4]));
    }

    #[test]
    fn enums() {
        let enums = Enums::new(CEnum::I3, TypeChoice::U64(53435364));
        deser_test(&enums);
    }

    #[test]
    fn group_choice_foo() {
        deser_test(&GroupChoice::new_foo(0, String::new(), vec![]));
    }

    #[test]
    fn group_choice_0() {
        deser_test(&GroupChoice::GroupChoice1(37));
    }

    #[test]
    fn group_choice_plain() {
        deser_test(&GroupChoice::Plain(Plain::new(354545, String::from("fdsfdsfdg").into())));
    }

    #[test]
    fn cbor_in_cbor() {
        let foo = Foo::new(0, String::new(), vec![]);
        // Both `.cbor` ROOTS (`foo_bytes`, `tagged_foo_bytes`) are wrapper structs, so each member
        // is constructed through its own `From<Foo>` — a `.cbor` rule body is a type of its own,
        // never a transparent alias for its payload.
        deser_test(&CborInCbor::new(foo.clone().into(), 9, foo.into()))
    }

    // The two WRAPPING contexts over a named alias — a tag head and a `.cbor` payload — round-trip,
    // and each is constructed through the alias's own declared spelling. The `new` signatures are
    // themselves the assertion the corpus cannot make from bytes alone: a wrapping seam that
    // re-resolved the alias would still round-trip identically, and would still compile, because
    // the alias is transparent — so what this executes is the round trip and what the blessed
    // snapshot beside it pins is the spelling.
    #[test]
    fn scalar_alias_wrappings() {
        let tagged: TaggedScalarAlias = TaggedScalarAlias::new(41);
        deser_test(&tagged);
        assert_eq!(tagged.to_cbor_bytes(), vec![0xc7, 0x18, 0x29]);

        let holder = ScalarAliasHolder::new(41);
        deser_test(&holder);
        // one array element, itself a byte string holding the payload's own encoding
        assert_eq!(holder.to_cbor_bytes(), vec![0x81, 0x42, 0x18, 0x29]);
    }

    // A `bytes .cbor <X>` payload is decoded from a deserializer built over the byte string's
    // contents. A leaf that names the OUTER reader instead consumes the next outer item, so the
    // damage is invisible in the payload's own value and shows up as the FOLLOWING member decoding
    // garbage (or, more usually, an `Expected(...)` a field later). These tests therefore assert the
    // following member's value, not merely that decode succeeded — a snapshot of the emitted text
    // cannot catch this class at all, since text blessed while the bug was live stays green forever.
    #[test]
    fn cbor_payload_leaves() {
        // Each payload value must be a MEMBER of its own float class (the six CDDL float names
        // partition the values by shortest lossless form), so the `float64` slots take values that
        // need all eight bytes and the `float32` slots values that need exactly four.
        let orig = CborPayloadLeaves::new(
            1.1,
            1.1f32,
            3.3,
            3.3f32,
            true,
            CEnum::I4,
            String::from("framing"),
        )
        .unwrap();
        deser_test(&orig);
        let deser = CborPayloadLeaves::from_cbor_bytes(&orig.to_cbor_bytes()).unwrap();
        assert_eq!(deser.f64_payload, 1.1);
        assert_eq!(deser.f32_payload, 1.1f32);
        assert_eq!(deser.bounded64_payload, 3.3);
        assert_eq!(deser.bounded32_payload, 3.3f32);
        assert!(deser.bool_payload);
        assert!(matches!(deser.enum_payload, CEnum::I4));
        // the member AFTER the last payload: proves nothing upstream over-read the outer buffer
        assert_eq!(deser.tail, "framing");
    }

    #[test]
    fn cbor_payload_collections() {
        let orig = CborPayloadCollections::new(
            vec![1, 2, 3],
            BTreeMap::from([(7u64, String::from("seven"))]),
            String::from("framing"),
        );
        deser_test(&orig);
        let deser = CborPayloadCollections::from_cbor_bytes(&orig.to_cbor_bytes()).unwrap();
        assert_eq!(deser.arr_payload, vec![1, 2, 3]);
        assert_eq!(deser.map_payload.get(&7).map(String::as_str), Some("seven"));
        assert_eq!(deser.tail, "framing");
    }

    // The break probe inside a `.cbor` payload's collection loop is only reached for an INDEFINITE
    // inner length, and the emitted serializer only ever writes definite lengths — so a round trip
    // cannot reach it and the payload has to be built by hand. If the probe reads the outer buffer
    // it looks at the NEXT outer item (here a byte string, then a text), never sees Special, and the
    // loop runs one iteration too many straight into the `0xff` break as if it were an element.
    #[test]
    fn cbor_payload_indefinite_inner() {
        let bytes = [
            0x83, // outer array(3)
            0x44, 0x9f, 0x01, 0x02, 0xff, // arr_payload: bstr(4) = [_ 1, 2]
            0x45, 0xbf, 0x01, 0x61, 0x61, 0xff, // map_payload: bstr(5) = {_ 1: "a"}
            0x61, 0x7a, // tail: "z"
        ];
        let deser = CborPayloadCollections::from_cbor_bytes(&bytes).unwrap();
        assert_eq!(deser.arr_payload, vec![1, 2]);
        assert_eq!(deser.map_payload.get(&1).map(String::as_str), Some("a"));
        assert_eq!(deser.tail, "z");
    }

    // A `.cbor` payload nested one level INSIDE another `.cbor` payload's collection. The oracle is
    // hand-derived per RFC 8949 (spelled out byte-by-byte below) rather than minted from
    // `to_cbor_bytes`, because the encoder and the decoder disagreed here: encoding was always
    // spec-correct, so a self-minted oracle would have agreed with the encoder and hidden the
    // decode break. The three legs are therefore: encode == oracle, decode(oracle) == the values,
    // and an empty-collection control (the loop body never runs, so it decoded fine even while the
    // body was reading the wrong buffer — it is the contrast that localises the defect to the body).
    #[test]
    fn cbor_payload_nested_payloads() {
        // Holder { elem_payloads: [5], value_payloads: {1: 9}, after_payload: 7, tail: "z" }
        let oracle = [
            0x84, // outer array(4)
            0x43, 0x81, 0x41, 0x05, // elem_payloads: bstr(3) = [ bstr(1) = 5 ]
            0x44, 0xa1, 0x01, 0x41, 0x09, // value_payloads: bstr(4) = { 1: bstr(1) = 9 }
            0x41, 0x07, // after_payload: bstr(1) = 7
            0x61, 0x7a, // tail: "z"
        ];
        let orig = CborPayloadNestedPayloads::new(
            vec![5],
            BTreeMap::from([(1u64, 9u64)]),
            7,
            String::from("z"),
        );
        assert_eq!(orig.to_cbor_bytes(), oracle);
        deser_test(&orig);
        let deser = CborPayloadNestedPayloads::from_cbor_bytes(&oracle).unwrap();
        assert_eq!(deser.elem_payloads, vec![5]);
        assert_eq!(deser.value_payloads.get(&1), Some(&9));
        // the members AFTER the nested payloads: proves the element/value reads did not eat them
        assert_eq!(deser.after_payload, 7);
        assert_eq!(deser.tail, "z");

        // empty control: both loop bodies are skipped
        let empty_oracle = [
            0x84, // outer array(4)
            0x41, 0x80, // elem_payloads: bstr(1) = []
            0x41, 0xa0, // value_payloads: bstr(1) = {}
            0x41, 0x07, // after_payload: bstr(1) = 7
            0x61, 0x7a, // tail: "z"
        ];
        let empty = CborPayloadNestedPayloads::new(
            vec![],
            BTreeMap::new(),
            7,
            String::from("z"),
        );
        assert_eq!(empty.to_cbor_bytes(), empty_oracle);
        let deser = CborPayloadNestedPayloads::from_cbor_bytes(&empty_oracle).unwrap();
        assert!(deser.elem_payloads.is_empty());
        assert!(deser.value_payloads.is_empty());
        assert_eq!(deser.after_payload, 7);
        assert_eq!(deser.tail, "z");
    }

    // The INLINE spelling — both `.cbor` depths in ONE encoding chain, so both are emitted into one
    // scope and are kept apart only by the depth suffix on every name the payload machinery mints.
    // The oracle is hand-derived per RFC 8949 rather than taken from `to_cbor_bytes`, for the same
    // reason `cbor_payload_nested_payloads` above does it: a self-minted oracle agrees with the
    // encoder by construction, and the failure mode this shape had was a decoder reading the wrong
    // reader. The `tail` assertion is the framing control — it is the member a mis-framed payload
    // read shows up in.
    #[test]
    fn cbor_payload_inline_nesting() {
        // CborPayloadInlineNesting { pair: 5, triple: 7, <fixed: 42, unstored>, tail: "z" }
        let oracle = [
            0x84, // outer array(4)
            0x42, 0x41, 0x05, // pair:   bstr(2) = bstr(1) = 5
            0x43, 0x42, 0x41, 0x07, // triple: bstr(3) = bstr(2) = bstr(1) = 7
            0x43, 0x42, 0x18, 0x2a, // fixed:  bstr(3) = bstr(2) = 42 (0x18 0x2a)
            0x61, 0x7a, // tail: "z"
        ];
        // `fixed` carries no value of its own, so it is not a constructor argument: the constant is
        // still written on the way out and still verified on the way in.
        let orig = CborPayloadInlineNesting::new(5, 7, String::from("z"));
        assert_eq!(orig.to_cbor_bytes(), oracle);
        deser_test(&orig);
        let deser = CborPayloadInlineNesting::from_cbor_bytes(&oracle).unwrap();
        assert_eq!(deser.pair, 5);
        assert_eq!(deser.triple, 7);
        assert_eq!(deser.tail, "z");

        // Each byte string is the payload's WHOLE encoding, so bytes left over inside EITHER level
        // are refused. Both vectors keep `pair`'s level-1 byte string at 3 bytes and move the stray
        // byte between the levels, so the only thing distinguishing them is which reader is left
        // non-empty — which is what proves the level-1 check probes the level-1 reader rather than
        // re-probing the level-2 one.
        //
        // The error KIND is asserted, not merely that decode failed: a leftover-byte vector that is
        // one byte off decodes into a WRONG MAJOR TYPE and rejects for an unrelated reason, passing
        // an `is_err()` assertion while exercising nothing. `TrailingData` is the deliberately-shared
        // error the payload arm raises (same spelling as the top-level check in `structural_rejects`
        // above), so naming it is what makes each vector prove its own level.
        let trailing_level2 = [
            0x84, // level-2 leftover: bstr(3) = bstr(2) whose content is `05 00` — the uint plus a
            0x43, 0x42, 0x05, 0x00, // stray byte, so level 2 decodes and then has bytes left
            0x43, 0x42, 0x41, 0x07, 0x43, 0x42, 0x18, 0x2a, 0x61, 0x7a,
        ];
        let err = CborPayloadInlineNesting::from_cbor_bytes(&trailing_level2).unwrap_err();
        assert!(err.to_string().contains("trailing data"), "{err}");
        let trailing_level1 = [
            0x84, // level-1 leftover: bstr(3) = bstr(1) = 5, then a stray byte — level 2 consumes
            0x43, 0x41, 0x05, 0x00, // its byte string exactly, so only level 1 is left non-empty
            0x43, 0x42, 0x41, 0x07, 0x43, 0x42, 0x18, 0x2a, 0x61, 0x7a,
        ];
        let err = CborPayloadInlineNesting::from_cbor_bytes(&trailing_level1).unwrap_err();
        assert!(err.to_string().contains("trailing data"), "{err}");

        // The rule-BODY spelling: the wrapper struct's own serialize fn holds both depths.
        let body = CborPayloadInlineBody::new(5);
        assert_eq!(body.to_cbor_bytes(), vec![0x42, 0x41, 0x05]);
        deser_test(&body);
        assert_eq!(
            CborPayloadInlineBody::from_cbor_bytes(&[0x42, 0x41, 0x05])
                .unwrap()
                .get(),
            5
        );
    }

    #[test]
    fn test_prelude_numbers() {
        assert_eq!(0u8, U8::from(0u8));
        assert_eq!(0u16, U16::from(0u16));
        assert_eq!(0u32, U32::from(0u32));
        assert_eq!(0i8, I8::from(0i8));
        assert_eq!(0u64, U64::from(0u64));
        assert_eq!(0i64, I64::from(0i64));
    }

    #[test]
    fn signed_ints() {
        let min = SignedInts::new(u8::MIN, u16::MIN, u32::MIN, u64::MIN, i8::MIN, i16::MIN, i32::MIN, i64::MIN, u64::MIN);
        deser_test(&min);
        let max = SignedInts::new(u8::MAX, u16::MAX, u32::MAX, u64::MAX, i8::MAX, i16::MAX, i32::MAX, i64::MAX, u64::MAX);
        deser_test(&max);
        // Width-correct canonical nint bytes at the encoder boundaries (the plain
        // write_negative_integer endpoint negates internally in i128 since cbor_event 3.x):
        // i_64 = i64::MIN -> 3b 7fff..., n_64 = -2^64 (magnitude u64::MAX) -> 3b ffff...
        let i64_min_nint = [0x3bu8, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff];
        assert!(min.to_cbor_bytes().windows(9).any(|w| w == i64_min_nint));
        let n64_floor_nint = [0x3bu8, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff];
        assert!(max.to_cbor_bytes().windows(9).any(|w| w == n64_floor_nint));
    }

    #[test]
    fn signed_ints_width_rejects() {
        // The exact-width collapses (u_8 0..255 -> u8, u_16 uint .size 2 -> u16, u_32 -> u32,
        // i_8/i_16/i_32 -> i8/i16/i32, i_64 int .size 8 -> i64) carry no residual bounds, so the
        // member deserializer's only guard is the type width itself. The wire readers return
        // WIDER values (u64 from unsigned_integer, i64/i128 from the nint readers); a bare
        // truncating cast (`raw.unsigned_integer()? as u16`) decoded 65536 "successfully" as 0 —
        // a silent-corruption class invisible to round-trips (the encoder can only produce
        // in-width values), surfaced by the matrix's ctl.size.uint enforcement row.
        // Field order: u_8, u_16, u_32, u_64, i_8, i_16, i_32, i_64, n_64, then the fixed
        // u64_max / i64_min tail. Baseline: zeros (n_64 = -1). Sz::Eight fits every value and
        // default-mode decoding is minimality-agnostic, so one width serves all vectors.
        let base: [i128; 9] = [0, 0, 0, 0, 0, 0, 0, 0, -1];
        let make = |idx: usize, v: i128| {
            let mut vals = base;
            vals[idx] = v;
            let mut cbor = arr_def(11);
            for x in vals.iter() {
                cbor.extend(cbor_int(*x, cbor_event::Sz::Eight));
            }
            cbor.extend(cbor_int(u64::MAX as i128, cbor_event::Sz::Eight));
            cbor.extend(cbor_int(i64::MIN as i128, cbor_event::Sz::Eight));
            SignedInts::from_cbor_bytes(&cbor)
        };
        // Boundary (exactly-representable) values decode on every field.
        make(0, 255).unwrap();
        make(1, 65535).unwrap();
        make(2, 4294967295).unwrap();
        make(3, u64::MAX as i128).unwrap();
        make(4, 127).unwrap();
        make(4, -128).unwrap();
        make(5, 32767).unwrap();
        make(5, -32768).unwrap();
        make(6, 2147483647).unwrap();
        make(6, -2147483648).unwrap();
        make(7, i64::MAX as i128).unwrap();
        make(7, i64::MIN as i128).unwrap();
        // One-past-width values must REJECT (pre-fix: silently truncate-decoded).
        assert!(make(0, 256).is_err());
        assert!(make(1, 65536).is_err());
        assert!(make(2, 4294967296).is_err());
        assert!(make(4, 128).is_err());
        assert!(make(4, -129).is_err());
        assert!(make(5, 32768).is_err());
        assert!(make(5, -32769).is_err());
        assert!(make(6, 2147483648).is_err());
        assert!(make(6, -2147483649).is_err());
        // i64 itself: the uint arm reads a u64 and must not wrap 2^63 to a negative; the nint
        // arm reads an i128 and must not wrap below i64::MIN.
        assert!(make(7, (i64::MAX as i128) + 1).is_err());
        assert!(make(7, (i64::MIN as i128) - 1).is_err());
        // The nint domain floor (-2^64) into NARROW int fields: cbor_event 2.4.0's
        // negative_integer() silently WRAPPED -2^64 to 0 and these fields ACCEPTED the corrupted
        // value; 3.2.0 errors instead (cbor_event 3.2.0 upgrade flip vectors).
        assert!(make(4, -(1i128 << 64)).is_err());
        assert!(make(5, -(1i128 << 64)).is_err());
        assert!(make(6, -(1i128 << 64)).is_err());
        assert!(make(7, -(1i128 << 64)).is_err());
        // ...while the full-range nint field still accepts the domain floor.
        make(8, -(1i128 << 64)).unwrap();
    }

    #[test]
    fn float_fixed_whole() {
        // Fixed float members 3.0 / 0x1.8p+1 (= 3.0) / 3.5: whole values once emitted integer
        // literals in f64 positions (three E0308s per member — the crate didn't compile), so
        // this test COMPILING and running is most of the point. The 3.5 control field pins the
        // already-valid non-whole formatting.
        let ffw = FloatFixedWhole::new();
        deser_test(&ffw);
        let expected: Vec<u8> = [arr_def(3), cbor_float(3.0), cbor_float(3.0), cbor_float(3.5)].concat();
        assert_eq!(ffw.to_cbor_bytes(), expected);
        FloatFixedWhole::from_cbor_bytes(&expected).unwrap();
        // A wrong value in a whole-fixed slot rejects (FixedValueMismatch).
        let wrong: Vec<u8> = [arr_def(3), cbor_float(3.5), cbor_float(3.0), cbor_float(3.5)].concat();
        assert_decode_reject_reason::<FloatFixedWhole>(&wrong, "Expected fixed value 3 found 3.5");
    }

    #[test]
    fn defaults() {
        let mut md = MapWithDefaults::new();
        deser_test(&md);
        md.key_1 = 0;
        deser_test(&md);
        md.key_2 = "not two".into();
        deser_test(&md);
    }

    #[test]
    fn no_alias() {
        use std::str::FromStr;
        // we can use this test compiling as a test for the presence of an alias by referencing e.g. I8::MIN
        // but we need to read the actual code to test that we're NOT using an alias somewhere and are indeed
        // using a raw rust primitive instead
        let lib_rs_with_tests = std::fs::read_to_string(std::path::PathBuf::from_str("src").unwrap().join("generated").join("mod.rs")).unwrap();
        // generated/mod.rs includes this very test (and thus those strings we're searching for), so strip from the
        // unique anchor at the top of tests/core/tests.rs down. Robust vs the first `#[cfg(test)]` (an emitted
        // test module could shift it). The marker is split so it's contiguous only at the anchor, not here.
        let anchor = concat!("// CORE_TESTS", "_TRUNCATION_ANCHOR");
        let lib_rs = &lib_rs_with_tests[..lib_rs_with_tests
            .find(anchor)
            .expect("truncation anchor missing from generated/mod.rs — tests/core/tests.rs must open with it so the source-inspection tests can strip their own literals")];
        // these don't have @no_alias
        assert!(lib_rs.contains("pub type I8 = i8;"));
        assert!(lib_rs.contains("pub type I64 = i64;"));
        assert!(lib_rs.contains("pub type U8 = u8;"));
        assert!(lib_rs.contains("pub type U16 = u16;"));
        assert!(lib_rs.contains("pub type U32 = u32;"));
        assert!(lib_rs.contains("pub type U64 = u64;"));
        // these do
        assert!(lib_rs.contains("no_alias_u32: u32"));
        assert!(lib_rs.contains("no_alias_u64: u64"));
        assert!(!lib_rs.contains("pub type NoAliasU32"));
        assert!(!lib_rs.contains("pub type NoAliasU64"));
    }

    #[test]
    fn externs() {
        let ext_foo = ExternalFoo::new(436, String::from("jfkdsjfd"), vec![1, 1, 1]);
        let mut externs = Externs::new(ext_foo.clone());
        deser_test(&externs);
        externs.opt = Some(ext_foo);
        deser_test(&externs);
    }

    #[test]
    fn externs_generic() {
        deser_test(&UsingExternGeneric::new(
            ExternGeneric::new(ExternalFoo::new(u64::MAX, String::from("asdfghjkl"), vec![0])),
        ));
    }

    #[test]
    fn top_level_arrays() {
        // this part of the test just tests that the resulting code compiles
        // e.g. the presence of the typedef instead of a new array struct by being able to asign to it.
        let arr: TopLevelArray = vec![3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5];

        // this part is to make sure that single-element arrays still work too and aren't vecs
        let mut arr2 = TopLevelSingleElem::new(9);
        deser_test(&arr2);
        arr2.index_0 *= arr2.index_0;
        assert_eq!(arr2.index_0, 81);
    }

    #[test]
    fn overlapping() {
        let overlap0 = Overlapping::new_a(Overlapping0::new());
        deser_test(&overlap0);
        let overlap1 = Overlapping::new_b(Overlapping1::new(9));
        deser_test(&overlap1);
        let overlap2 = Overlapping::new_c(Overlapping2::new(5, "overlapping".into()));
        deser_test(&overlap2);
    }

    #[test]
    fn overlapping_inlined() {
        let overlap0 = OverlappingInlined::new_one();
        deser_test(&overlap0);
        let overlap1 = OverlappingInlined::new_two(9);
        deser_test(&overlap1);
        let overlap2 = OverlappingInlined::new_three(5, "overlapping".into());
        deser_test(&overlap2);
    }

    #[test]
    fn overlapping_type_choice_all() {
        deser_test(&NonOverlappingTypeChoiceAll::U64(100));
        deser_test(&NonOverlappingTypeChoiceAll::N64(10000));
        deser_test(&NonOverlappingTypeChoiceAll::Text("Hello, World!".into()));
        deser_test(&NonOverlappingTypeChoiceAll::Bytes(vec![0xBA, 0xAD, 0xF0, 0x0D]));
        deser_test(&NonOverlappingTypeChoiceAll::Helloworld);
        deser_test(&NonOverlappingTypeChoiceAll::ArrU64(vec![0, u64::MAX]));
        deser_test(&NonOverlappingTypeChoiceAll::MapTextToU64(
            BTreeMap::from([("two".into(), 2), ("four".into(), 4)]))
        );
    }

    #[test]
    fn overlapping_type_choice_some() {
        deser_test(&NonOverlappingTypeChoiceSome::U64(100));
        deser_test(&NonOverlappingTypeChoiceSome::N64(10000));
        deser_test(&NonOverlappingTypeChoiceSome::Text("Hello, World!".into()));
    }
    
    #[test]
    fn overlap_basic_embed() {
        deser_test(&OverlapBasicEmbed::new_identity());
        deser_test(&OverlapBasicEmbed::new_x(vec![85; 32]).unwrap());
    }

    #[test]
    fn non_overlap_basic_embed() {
        deser_test(&NonOverlapBasicEmbed::new_first(100));
        deser_test(&NonOverlapBasicEmbed::new_second("cddl".to_owned()));
    }

    #[test]
    fn non_overlap_basic_embed_multi_fields() {
        deser_test(&NonOverlapBasicEmbedMultiFields::new_first(100, 1_000_000));
        deser_test(&NonOverlapBasicEmbedMultiFields::new_second("cddl".to_owned(), 0));
    }
    
    #[test]
    fn non_overlap_basic_embed_mixed() {
        deser_test(&NonOverlapBasicEmbedMixed::new_first(100));
        deser_test(&NonOverlapBasicEmbedMixed::new_second("cddl".to_owned(), 0));
    }

    #[test]
    fn non_overlap_basic_embed_mixed_explicit() {
        deser_test(&NonOverlapBasicEmbedMixedExplicit::new_first(100));
        deser_test(&NonOverlapBasicEmbedMixedExplicit::new_second("cddl".to_owned(), 0));
        deser_test(&NonOverlapBasicEmbedMixedExplicit::new_third(vec![0xBA, 0xAD, 0xF0, 0x0D], 4));
    }

    #[test]
    fn non_overlap_basic_not_basic() {
        deser_test(&NonOverlapBasicNotBasic::new_group(4, "basic".to_owned()));
        deser_test(&NonOverlapBasicNotBasic::new_group_arr(Basic::new(4, "".to_owned())));
        deser_test(&NonOverlapBasicNotBasic::new_group_tagged(Basic::new(0, " T A G G E D ".to_owned())));
        deser_test(&NonOverlapBasicNotBasic::new_group_bytes(u64::MAX, "bytes .cbor basic".to_owned()));
    }

    #[test]
    fn array_opt_fields() {
        let mut foo = ArrayOptFields::new(10);
        for e in [None, Some(NonOverlappingTypeChoiceSome::U64(5)), Some(NonOverlappingTypeChoiceSome::N64(4)), Some(NonOverlappingTypeChoiceSome::Text("five".to_owned()))] {
            for a in [false, true] {
                for b in [false, true] {
                    for d in [false, true] {
                        // round-trip on non-constants — plus the x/z presence bits (the optional
                        // fixed floats), piggybacked on the a/d booleans so all four presence
                        // combinations round-trip through THIS long-optional-chain shape (the
                        // formerly-dropped serialize side; the isolated shape is
                        // opt_fixed_member_float's)
                        foo.x = a;
                        foo.z = d;
                        foo.a = if a { Some(0) } else { None };
                        foo.b = if b { Some("hello, world".to_owned()) } else { None };
                        foo.d = if d { Some("cddl-codegen".to_owned()) } else { None };
                        foo.e = e.clone();
                        deser_test(&foo);
                        // deser for constants too
                        for x in [false, true] {
                            for y in [false, true] {
                                for z in [false, true] {
                                    let mut components = Vec::new();
                                    if x {
                                        components.push(cbor_float(1.010101));
                                    }
                                    if a {
                                        components.push(cbor_int(0, cbor_event::Sz::One));
                                    }
                                    if b {
                                        components.push(cbor_string("hello, world"));
                                    }
                                    // c
                                    components.push(cbor_int(-10, cbor_event::Sz::One));
                                    if d {
                                        components.push(cbor_string("cddl-codegen"));
                                    }
                                    // y
                                    components.push(cbor_float(3.14159265));
                                    if let Some(e) = &e {
                                        components.push(e.to_cbor_bytes());
                                    }
                                    if z {
                                        components.push(cbor_float(2.71828));
                                    }
                                    components.insert(0, arr_def(components.len() as u8));
                                    let bytes = components.into_iter().flatten().clone().collect::<Vec<u8>>();
                                    // value anchors: decode-accepts alone proved nothing — pin every
                                    // field to what the hand-built bytes above encode (y is the
                                    // MANDATORY fixed float constant — no field; the OPTIONAL fixed
                                    // floats x/z are `bool` presence fields since the float
                                    // presence-field delivery, asserted below; c: nint -10 is stored
                                    // as the magnitude m = |v + 1| = 9)
                                    let decoded = ArrayOptFields::from_cbor_bytes(&bytes).unwrap();
                                    assert_eq!(decoded.x, x);
                                    assert_eq!(decoded.z, z);
                                    assert_eq!(decoded.a, if a { Some(0) } else { None });
                                    assert_eq!(decoded.b, if b { Some("hello, world".to_owned()) } else { None });
                                    assert_eq!(decoded.c, 9);
                                    assert_eq!(decoded.d, if d { Some("cddl-codegen".to_owned()) } else { None });
                                    // no PartialEq on generated types in this profile: anchor e by bytes
                                    assert_eq!(
                                        decoded.e.as_ref().map(|v| v.to_cbor_bytes()),
                                        e.as_ref().map(|v| v.to_cbor_bytes())
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn bounds() {
        deser_test(&Bounds::new(10, 5, 4, "abc".to_owned(), vec![5].try_into().unwrap(), vec![(0, 1), (2, 3)].try_into().unwrap()).unwrap());
        // y is `nint .ge -5`, stored as the u64 magnitude m = |v + 1| (m = 4 ⇒ v = -5). new() enforces
        // the bound in magnitude space; regression for the inverted-nint-constructor-bound bug where the
        // check was `m < 4` (rejecting valid values, accepting invalid ones) instead of `m > 4`.
        Bounds::new(10, 5, 0, "abc".to_owned(), vec![5].try_into().unwrap(), vec![(0, 1), (2, 3)].try_into().unwrap()).unwrap(); // m=0 ⇒ v=-1, in range
        Bounds::new(10, 5, 4, "abc".to_owned(), vec![5].try_into().unwrap(), vec![(0, 1), (2, 3)].try_into().unwrap()).unwrap(); // m=4 ⇒ v=-5, boundary
        assert!(Bounds::new(10, 5, 5, "abc".to_owned(), vec![5].try_into().unwrap(), vec![(0, 1), (2, 3)].try_into().unwrap()).is_err()); // m=5 ⇒ v=-6, below min
        // Same magnitude-space bound on the Wrapper (`@newtype`) path — regression for the standalone
        // bounded-nint-newtype bug, where new()/deserialize emitted `if inner < -5` on a u64 `inner`
        // (E0600, didn't compile). Also round-trips a valid value through the deserializer's own check.
        NintGeNewtype::new(0).unwrap(); // m=0 ⇒ v=-1, in range (v >= -5)
        NintGeNewtype::new(4).unwrap(); // m=4 ⇒ v=-5, boundary
        assert!(NintGeNewtype::new(5).is_err()); // m=5 ⇒ v=-6, out (v < -5)
        deser_test(&NintGeNewtype::new(4).unwrap());
        NintLeNewtype::new(5).unwrap(); // m=5 ⇒ v=-6, in range (v <= -5)
        NintLeNewtype::new(4).unwrap(); // m=4 ⇒ v=-5, boundary
        assert!(NintLeNewtype::new(0).is_err()); // m=0 ⇒ v=-1, out (v > -5)
        deser_test(&NintLeNewtype::new(5).unwrap());
        enum OOB {
            Below,
            Lower,
            Upper,
            Above,
        }
        let make_bounds = |w_out: OOB, x_out: OOB, y_out: OOB, z_out: OOB, a_out: OOB, b_out: OOB| {
            let cbor = vec![
                arr_def(6),
                    cbor_int(match w_out {
                        OOB::Below => -1001,
                        OOB::Lower => -1000,
                        OOB::Upper => 1000,
                        OOB::Above => 1001,
                    }, cbor_event::Sz::Two),
                    cbor_int(match x_out {
                        OOB::Below => panic!(),
                        OOB::Lower => panic!(),
                        OOB::Upper => 7,
                        OOB::Above => 8,
                    }, cbor_event::Sz::Inline),
                    cbor_int(match y_out {
                        OOB::Below => -6,
                        OOB::Lower => -5,
                        OOB::Upper => panic!(),
                        OOB::Above => panic!(),
                    }, cbor_event::Sz::Inline),
                    cbor_string(match z_out {
                        OOB::Below => "ab",
                        OOB::Lower => "abc",
                        OOB::Upper => "abcdefghijklmn",
                        OOB::Above => "abcdefghijklmno",
                    }),
                    vec![ARR_INDEF],
                        match a_out {
                            OOB::Below => vec![],
                            OOB::Lower => vec![0x00],
                            OOB::Upper => vec![0x00, 0x01, 0x02],
                            OOB::Above => vec![0x00, 0x01, 0x02, 0x03],
                        },
                    vec![BREAK],
                    vec![MAP_INDEF],
                        match b_out {
                            OOB::Below => panic!(),
                            OOB::Lower => panic!(),
                            OOB::Upper => vec![0x00, 0x00, 0x01, 0x01, 0x02, 0x02],
                            OOB::Above => vec![0x00, 0x00, 0x01, 0x01, 0x02, 0x02, 0x03, 0x03],
                        },
                    vec![BREAK],
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            Bounds::from_cbor_bytes(&cbor)
        };
        let good1 = make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Lower, OOB::Lower, OOB::Upper).unwrap();
        deser_test(&good1);
        let good2 = make_bounds(OOB::Upper, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Upper).unwrap();
        deser_test(&good2);
        // w oob
        assert!(make_bounds(OOB::Below, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Upper).is_err());
        assert!(make_bounds(OOB::Above, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Upper).is_err());
        // x oob
        assert!(make_bounds(OOB::Lower, OOB::Above, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Upper).is_err());
        // y oob
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Below, OOB::Upper, OOB::Upper, OOB::Upper).is_err());
        // z oob
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Below, OOB::Upper, OOB::Upper).is_err());
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Above, OOB::Upper, OOB::Upper).is_err());
        // a oob
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Below, OOB::Upper).is_err());
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Above, OOB::Upper).is_err());
        // b oob
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Above).is_err());

        // type and group choices share the same deserialization code so we only check the API
        BoundsTypeChoice::new_bytes(vec![0; 64]).unwrap();
        assert!(BoundsTypeChoice::new_bytes(vec![0; 65]).is_err());
        BoundsGroupChoice::new_a(0, "four".to_owned()).unwrap();
        assert!(BoundsGroupChoice::new_a(0, "hello".to_owned()).is_err());
        deser_test(&BoundsGroupChoice::new_c(Hash::new(vec![]).unwrap(), Hash::new(vec![]).unwrap()));
    }

    #[test]
    fn sign_bounds() {
        // `SignBounds` exercises the per-CBOR-sign-arm partition of a signed-int (i64) value window.
        // Fields (in order): all_neg -10..-3, upto_zero -10..0, le_neg int .le -3, le_pos int .le 10,
        // ge_pos int .ge 3, ne_pos int .ne 5, ne_neg int .ne -5, straddle -10..3,
        // ne_one int .ne 1, ne_zero int .ne 0.
        // Baseline: every field in range (ne_* avoid their excluded value).
        let base: [i128; 10] = [-5, -5, -5, 10, 3, 4, -4, 0, 0, 1];
        // Build the wire array from an override of the in-range baseline. Sz::Eight fits every value
        // and default-mode decoding is minimality-agnostic, so one width serves all vectors.
        let make = |idx: usize, v: i128| {
            let mut vals = base;
            vals[idx] = v;
            let mut cbor = arr_def(10);
            for x in vals.iter() {
                cbor.extend(cbor_int(*x, cbor_event::Sz::Eight));
            }
            SignBounds::from_cbor_bytes(&cbor)
        };
        // Baseline round-trips through both the constructor and the deserializer.
        let baseline = SignBounds::new(-5, -5, -5, 10, 3, 4, -4, 0, 0, 1).unwrap();
        deser_test(&baseline);
        make(0, -5).unwrap();

        // all_neg (-10..-3): rejects ANY uint, rejects either side; accepts both endpoints.
        assert!(make(0, 5).is_err()); // uint arm entirely excluded
        assert!(make(0, -2).is_err()); // above upper
        assert!(make(0, -11).is_err()); // below lower
        make(0, -3).unwrap();
        make(0, -10).unwrap();

        // upto_zero (-10..0): the upper endpoint 0 is constraining (kills the naive drop-the-0 fix).
        make(1, 0).unwrap();
        make(1, -10).unwrap();
        assert!(make(1, 1).is_err());
        assert!(make(1, -11).is_err());

        // le_neg (int .le -3): rejects any uint; nint arm keeps the upper.
        assert!(make(2, 5).is_err());
        assert!(make(2, -2).is_err());
        make(2, -3).unwrap();
        make(2, -10).unwrap();

        // le_pos (int .le 10): the nint arm is VACUOUS and must NOT reject a large negative.
        make(3, -999999).unwrap();
        make(3, 10).unwrap();
        assert!(make(3, 11).is_err());

        // ge_pos (int .ge 3): the nint arm is EMPTY (every negative rejected).
        make(4, 3).unwrap();
        make(4, 100).unwrap();
        assert!(make(4, 2).is_err());
        assert!(make(4, -1).is_err());

        // ne_pos (int .ne 5): the excluded value is non-negative, so only the uint arm checks it.
        make(5, -5).unwrap();
        make(5, 4).unwrap();
        make(5, 6).unwrap();
        assert!(make(5, 5).is_err());

        // ne_neg (int .ne -5): the excluded value is negative, so only the nint arm checks it
        // (the uint arm must NOT try to compare a u64 against -5).
        make(6, 5).unwrap();
        make(6, -4).unwrap();
        make(6, -6).unwrap();
        assert!(make(6, -5).is_err());

        // straddle (-10..3): unchanged survivor — accepts across the sign boundary, rejects outside.
        make(7, -10).unwrap();
        make(7, 3).unwrap();
        make(7, 0).unwrap();
        assert!(make(7, -11).is_err());
        assert!(make(7, 4).is_err());

        // ne_one (int .ne 1): the excluded-value boundary where the (N+1, N-1) exclusion encoding's
        // max hits 0 — a per-side partition of (2, 0) once emitted `x < 2`, silently rejecting 0.
        make(8, 0).unwrap(); // the value the mis-check rejected
        make(8, 2).unwrap();
        make(8, -1).unwrap(); // nint arm is unconstrained by a non-negative exclusion
        assert!(make(8, 1).is_err());

        // ne_zero (int .ne 0): encoding (1, -1) has a bound on each side of the sign split; only 0
        // may reject.
        make(9, 1).unwrap();
        make(9, -1).unwrap();
        assert!(make(9, 0).is_err());
    }

    #[test]
    fn top_level_ranges() {
        // Literal-headed top-level range rules wrap into a bounds-enforcing struct (mirroring the
        // `int .op`-headed top-level wrappers), so their standalone from_cbor_bytes rejects
        // out-of-window values and a tagged rule writes/requires its tag. Pre-fix these emitted a
        // bare `pub type` alias with no ctor/deserialize, silently dropping the bounds (and the tag).

        // top_level_neg_range = -10..-3, an i64 wrapper. Its deserializer reads BOTH CBOR sign arms
        // and checks the whole window over i64, so this is also a full-window regression.
        let neg = |v: i128| TopLevelNegRange::from_cbor_bytes(&cbor_int(v, cbor_event::Sz::Eight));
        assert!(neg(5).is_err()); // any uint is out of an all-negative window
        assert!(neg(-11).is_err()); // below lower
        assert!(neg(-2).is_err()); // above upper
        neg(-3).unwrap();
        neg(-10).unwrap();
        deser_test(&TopLevelNegRange::new(-3).unwrap());
        deser_test(&TopLevelNegRange::new(-10).unwrap());
        assert!(TopLevelNegRange::new(5).is_err());
        assert!(TopLevelNegRange::new(-11).is_err());

        // top_level_pos_range = 3..10, a u64 wrapper.
        let pos = |v: i128| TopLevelPosRange::from_cbor_bytes(&cbor_int(v, cbor_event::Sz::Eight));
        assert!(pos(2).is_err());
        assert!(pos(11).is_err());
        pos(3).unwrap();
        pos(10).unwrap();
        deser_test(&TopLevelPosRange::new(3).unwrap());
        deser_test(&TopLevelPosRange::new(10).unwrap());

        // top_level_tagged_range = #6.5(3..10): the wrapper must write tag 5 on the wire and require
        // it on the way in, plus enforce the window. A bare alias would drop the tag entirely.
        let tagged_ok = TopLevelTaggedRange::new(7).unwrap();
        let tagged_bytes = tagged_ok.to_cbor_bytes();
        // Byte-check the tag head: 0xc5 = major type 6 (tag) with argument 5.
        assert_eq!(tagged_bytes[0], 0xc5);
        assert_eq!(
            tagged_bytes,
            [cbor_tag(5), cbor_int(7, cbor_event::Sz::Inline)].concat()
        );
        deser_test(&tagged_ok);
        TopLevelTaggedRange::from_cbor_bytes(&tagged_bytes).unwrap();
        // untagged input is rejected (a bare `pub type = u64` alias would have accepted it)
        assert_decode_reject_reason::<TopLevelTaggedRange>(
            &cbor_int(7, cbor_event::Sz::Inline),
            "expected `Tag' byte received `UnsignedInteger'",
        );
        // out-of-window tagged input is rejected
        assert_decode_reject_reason::<TopLevelTaggedRange>(
            &[cbor_tag(5), cbor_int(11, cbor_event::Sz::Inline)].concat(),
            "11 not in range 3 - 10",
        );
        // wrong tag is rejected
        assert_decode_reject_reason::<TopLevelTaggedRange>(
            &[cbor_tag(4), cbor_int(7, cbor_event::Sz::Inline)].concat(),
            "Expected tag 5, found 4",
        );
        assert!(TopLevelTaggedRange::new(11).is_err());
    }

    #[test]
    fn float_bounds() {
        // `FloatBounds` fields (in order): incl 0.5..10.5, excl 0.5...10.5 (excludes 10.5),
        // lt float64 .lt 10.5, ge float64 .ge 0.5, eq float64 .eq 3.3, f32le float32 .le 10.5.
        // Every emitted check is NaN-safe accept-form (`!(x >= min && x <= max)`), so NaN — for
        // which every comparison is false — is rejected on every field.
        //
        // Two constraints compose on the last four fields, and the vectors keep them apart. The
        // BOUND is what these cases vary. The CLASS is a standing requirement: a CDDL float name is
        // a set of VALUES partitioned by shortest lossless form, so a `float64` field admits only
        // values that need all eight bytes and a `float32` field only values that need exactly
        // four. `5.5` is a `float16` and belongs to neither, which is why the values here are
        // deliberately un-round. The first two fields are bare literal ranges, which name no class
        // (they parse as the unconstrained `float`), so they keep the round numbers.
        let base: [f64; 6] = [5.5, 5.5, 5.1, 5.1, 3.3, f64::from(5.1f32)];
        let make = |idx: usize, v: f64| {
            let mut vals = base;
            vals[idx] = v;
            let mut cbor = arr_def(6);
            for (i, x) in vals.iter().enumerate() {
                cbor.extend(if i == 5 {
                    // narrowed first: the `float32` field's carrier is an f32, and these vectors are
                    // about the WINDOW, so each case has to be the f32 image of its literal
                    cbor_float(f64::from(*x as f32))
                } else {
                    cbor_float(*x)
                });
            }
            FloatBounds::from_cbor_bytes(&cbor)
        };
        // baseline round-trips through both ctor and deserializer
        let baseline =
            FloatBounds::new(5.5, 5.5, 5.1, 5.1, 3.3, 5.1f32).unwrap();
        deser_test(&baseline);
        make(0, 5.5).unwrap();

        // incl (0.5..10.5): both endpoints accepted, just-outside rejected, NaN rejected.
        make(0, 0.5).unwrap();
        make(0, 10.5).unwrap();
        assert!(make(0, 0.4).is_err());
        assert!(make(0, 10.6).is_err());
        assert!(make(0, f64::NAN).is_err());
        assert!(FloatBounds::new(f64::NAN, 5.5, 5.1, 5.1, 3.3, 5.1f32).is_err());
        FloatBounds::new(0.5, 5.5, 5.1, 5.1, 3.3, 5.1f32).unwrap();
        FloatBounds::new(10.5, 5.5, 5.1, 5.1, 3.3, 5.1f32).unwrap();
        assert!(FloatBounds::new(10.6, 5.5, 5.1, 5.1, 3.3, 5.1f32).is_err());

        // excl (0.5...10.5): the exclusive upper endpoint 10.5 is REJECTED; the min stays inclusive.
        make(1, 0.5).unwrap();
        assert!(make(1, 10.5).is_err());
        make(1, 10.4).unwrap();
        assert!(make(1, f64::NAN).is_err());

        // lt (float64 .lt 10.5): one-sided exclusive max; no lower bound.
        make(2, -100.1).unwrap();
        make(2, 10.4).unwrap();
        assert!(make(2, 10.500000000000002).is_err());
        assert!(make(2, f64::NAN).is_err());

        // ge (float64 .ge 0.5): one-sided inclusive min; no upper bound.
        make(3, 0.5000000000000001).unwrap();
        make(3, 1000.1).unwrap();
        assert!(make(3, 0.4).is_err());
        assert!(make(3, f64::NAN).is_err());

        // eq (float64 .eq 3.3): only 3.3 accepted.
        make(4, 3.3).unwrap();
        assert!(make(4, 3.2).is_err());
        assert!(make(4, 3.4).is_err());
        assert!(make(4, f64::NAN).is_err());

        // f32le (float32 .le 10.5): the f32 carrier's value is compared as f64.
        make(5, 10.4).unwrap();
        assert!(make(5, 10.6).is_err());
        make(5, -5.1).unwrap();
        assert!(make(5, f64::NAN).is_err());
        FloatBounds::new(5.5, 5.5, 5.1, 5.1, 3.3, 10.4f32).unwrap();
        assert!(FloatBounds::new(5.5, 5.5, 5.1, 5.1, 3.3, 10.6f32).is_err());

        // The class is enforced independently of the bound, in the same direction on both sides:
        // `10.5` is inside every one of these windows and is still refused by the `float64` and
        // `float32` fields, because its shortest lossless form is `f9` — it is a `float16` value.
        assert!(make(2, 10.5).is_err(), "10.5 is a float16, not a float64");
        assert!(make(3, 10.5).is_err(), "10.5 is a float16, not a float64");
        assert!(make(5, 10.5).is_err(), "10.5 is a float16, not a float32");
    }

    #[test]
    fn top_level_float_ranges() {
        // float_range = 0.5..10.5 wraps into a bounds-enforcing newtype: 10.5 accepted, 10.6/NaN
        // rejected at BOTH new() and from_cbor_bytes. A bare `pub type = f64` alias would enforce
        // nothing.
        let fr = |v: f64| FloatRange::from_cbor_bytes(&cbor_float(v));
        fr(0.5).unwrap();
        fr(10.5).unwrap();
        fr(5.5).unwrap();
        assert!(fr(0.4).is_err());
        assert!(fr(10.6).is_err());
        assert!(fr(f64::NAN).is_err());
        deser_test(&FloatRange::new(0.5).unwrap());
        deser_test(&FloatRange::new(10.5).unwrap());
        assert!(FloatRange::new(10.6).is_err());
        assert!(FloatRange::new(f64::NAN).is_err());

        // float_range_excl = 0.5...10.5: the exclusive upper endpoint 10.5 is rejected.
        let fre = |v: f64| FloatRangeExcl::from_cbor_bytes(&cbor_float(v));
        fre(10.4).unwrap();
        assert!(fre(10.5).is_err());
        assert!(FloatRangeExcl::new(10.5).is_err());
        FloatRangeExcl::new(10.4).unwrap();

        // tagged_float_range = #6.5(0.5..10.5): the wrapper writes tag 5 AND enforces the window.
        let tagged = TaggedFloatRange::new(7.5).unwrap();
        let tagged_bytes = tagged.to_cbor_bytes();
        assert_eq!(tagged_bytes[0], 0xc5); // major type 6 (tag), argument 5
        assert_eq!(tagged_bytes, [cbor_tag(5), cbor_float(7.5)].concat());
        deser_test(&tagged);
        TaggedFloatRange::from_cbor_bytes(&tagged_bytes).unwrap();
        // untagged input rejected (a bare alias would drop the tag)
        assert_decode_reject_reason::<TaggedFloatRange>(&cbor_float(7.5), "expected `Tag' byte received `Special'");
        // out-of-window tagged input rejected
        assert_decode_reject_reason::<TaggedFloatRange>(&[cbor_tag(5), cbor_float(10.6)].concat(), "10.6 not in float range (>=0.5, <=10.5)");
        // wrong tag rejected
        assert_decode_reject_reason::<TaggedFloatRange>(&[cbor_tag(4), cbor_float(7.5)].concat(), "Expected tag 5, found 4");
        assert!(TaggedFloatRange::new(10.6).is_err());
    }

    #[test]
    fn used_as_key() {
        // this is just here to make sure this compiles (i.e. Ord traits are derived)
        let mut set_outer: std::collections::BTreeSet<Outer> = std::collections::BTreeSet::new();
        set_outer.insert(Outer::new(2143254, Plain::new(7576, String::from("wiorurri34h").into())));
        let mut set_type_choice: std::collections::BTreeSet<TypeChoice> = std::collections::BTreeSet::new();
        set_type_choice.insert(TypeChoice::Helloworld);
        let mut set_group_choice: std::collections::BTreeSet<GroupChoice> = std::collections::BTreeSet::new();
        set_group_choice.insert(GroupChoice::GroupChoice1(37));
    }

    #[test]
    fn enum_opt_embed_fields() {
        let a = EnumOptEmbedFields::new_ea();
        deser_test(&a);
        let b1 = EnumOptEmbedFields::new_eb(Some("Hello".to_owned()));
        deser_test(&b1);
        let b2 = EnumOptEmbedFields::new_eb(None);
        deser_test(&b2);
        let c = EnumOptEmbedFields::new_ec(100);
        deser_test(&c);
        let mut d1 = EnumOptEmbedFields::new_ed(1);
        match &mut d1 {
            EnumOptEmbedFields::Ed(ed) => ed.index_2 = Some("Goodbye".to_owned()),
            _ => panic!(),
        }
        deser_test(&d1);
        let d2 = EnumOptEmbedFields::new_ed(2);
        deser_test(&d2);
        let mut e1 = EnumOptEmbedFields::new_ee(0, 0);
        match &mut e1 {
            EnumOptEmbedFields::Ee(ee) => ee.index_2 = Some(vec![0xBA, 0xAD, 0xF0, 0x0D]),
            _ => panic!(),
        }
        deser_test(&e1);
        let e2 = EnumOptEmbedFields::new_ee(u64::MAX, u64::MAX);
        deser_test(&e2);
        let f1 = EnumOptEmbedFields::new_ef(Some(NonOverlappingTypeChoiceSome::U64(5)));
        deser_test(&f1);
        let f2 = EnumOptEmbedFields::new_ef(None);
        deser_test(&f2);
        let g1 = EnumOptEmbedFields::new_eg(Some(OverlappingInlined::new_two(0)));
        deser_test(&g1);
        let g2 = EnumOptEmbedFields::new_eg(None);
        deser_test(&g2);
    }

    #[test]
    fn casing_test() {
        // these are just testing that these exist under these names
        let _ = CasingTest::new_nft();
        let _ = CasingTest::NFT;
        let _ = CasingTest::new_ip_address();
        let _ = CasingTest::IPAddress;
        let _ = CasingTest::new_shelley_ma();
        let _ = CasingTest::ShelleyMA;
        let _ = CasingTest::new_vrf_vkey();
        let _ = CasingTest::VRFVkey;
    }
  
    #[test]
    fn custom_serialization() {
        let struct_with_custom_bytes = StructWithCustomSerialization::new(
            vec![0xCA, 0xFE, 0xF0, 0x0D],
            vec![0x03, 0x01, 0x04, 0x01],
            vec![0xBA, 0xAD, 0xD0, 0x0D],
            vec![0xDE, 0xAD, 0xBE, 0xEF],
            1024,
        );
        use cbor_event::{Sz, StringLenSz};
        let bytes_special_enc = StringLenSz::Indefinite(vec![(1, Sz::Inline), (1, Sz::Inline), (1, Sz::Inline), (1, Sz::Inline)]);
        deser_test(&struct_with_custom_bytes);
        let expected_bytes = vec![
            arr_def(5),
                cbor_bytes_sz(vec![0xCA, 0xFE, 0xF0, 0x0D], bytes_special_enc.clone()),
                cbor_bytes_sz(vec![0x03, 0x01, 0x04, 0x01], bytes_special_enc.clone()),
                cbor_string("baadd00d"),
                cbor_tag(9),
                    cbor_bytes_sz(vec![0xDE, 0xAD, 0xBE, 0xEF], bytes_special_enc.clone()),
                cbor_tag(9),
                    cbor_string("1024")
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(expected_bytes, struct_with_custom_bytes.to_cbor_bytes());
        // the custom read hook owns the WHOLE tagged2 field ("must include the tag"), so it is
        // the only place a wrong tag can be rejected — tag 10 on a #6.9(uint) must fail
        let bad_tag_bytes = vec![
            arr_def(5),
                cbor_bytes_sz(vec![0xCA, 0xFE, 0xF0, 0x0D], bytes_special_enc.clone()),
                cbor_bytes_sz(vec![0x03, 0x01, 0x04, 0x01], bytes_special_enc.clone()),
                cbor_string("baadd00d"),
                cbor_tag(9),
                    cbor_bytes_sz(vec![0xDE, 0xAD, 0xBE, 0xEF], bytes_special_enc.clone()),
                cbor_tag(10),
                    cbor_string("1024")
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let err = StructWithCustomSerialization::from_cbor_bytes(&bad_tag_bytes).unwrap_err();
        assert!(
            format!("{:?}", err).contains("TagMismatch"),
            "wrong tag must fail as TagMismatch, got {:?}",
            err
        );
    }

    // The named record's declared form is `[uint]`, but its pair owns a text item. These exact
    // bytes prove both direct trait APIs and a holder's Root(Rust) dispatch reach the same writer;
    // the wrong-shape vector makes a fallback to the generated record reader fail loudly.
    #[test]
    fn custom_record_rule_delegates_direct_and_embedded() {
        let record = CustomRecord::new(42);
        let direct = cbor_string("42");
        assert_eq!(record.to_cbor_bytes(), direct);
        assert_eq!(CustomRecord::from_cbor_bytes(&direct).unwrap().value, 42);
        // the custom reader must reject the record's generated array wire
        assert_decode_reject_reason::<CustomRecord>(
            &[arr_def(1), cbor_int(42, cbor_event::Sz::One)].concat(),
            "expected `Text' byte received `Array'",
        );

        let holder = CustomRecordHolder::new(record);
        let embedded = [arr_def(1), cbor_string("42")].concat();
        assert_eq!(holder.to_cbor_bytes(), embedded);
        assert_eq!(
            CustomRecordHolder::from_cbor_bytes(&embedded)
                .unwrap()
                .nested
                .value,
            42
        );
    }

    // The homogeneous TABLE twin: custom wire is an ARRAY, so the generated map reader/writer must
    // never leak through. `From<BTreeMap>` is the nominal table construction door and the holder
    // proves Root(Rust) reaches the same free pair as the direct bytes APIs.
    #[test]
    fn custom_table_rule_delegates_direct_and_embedded() {
        let map = [("left".to_owned(), 3), ("right".to_owned(), 7)]
            .into_iter()
            .collect::<std::collections::BTreeMap<_, _>>();
        let table = CustomTable::from(map);
        let direct = [arr_def(4), cbor_string("left"), cbor_int(3, cbor_event::Sz::Inline), cbor_string("right"), cbor_int(7, cbor_event::Sz::Inline)].concat();
        assert_eq!(table.to_cbor_bytes(), direct);
        assert_eq!(CustomTable::from_cbor_bytes(&direct).unwrap().get(), table.get());
        assert_decode_reject_reason::<CustomTable>(&[0xa0], "expected `Array' byte received `Map'");
        assert_decode_reject_reason::<CustomTable>(&[arr_def(3), cbor_string("left"), cbor_int(3, cbor_event::Sz::Inline), cbor_string("right")].concat(), "table-as-array codec: odd item count");
        assert_decode_reject_reason::<CustomTable>(&[arr_def(4), cbor_string("left"), cbor_int(3, cbor_event::Sz::Inline), cbor_string("left"), cbor_int(7, cbor_event::Sz::Inline)].concat(), "table-as-array codec: duplicate key");

        let holder = CustomTableHolder::new(table);
        let embedded = [arr_def(1), direct.clone()].concat();
        assert_eq!(holder.to_cbor_bytes(), embedded);
        assert_eq!(
            CustomTableHolder::from_cbor_bytes(&embedded)
                .unwrap()
                .nested
                .get(),
            holder.nested.get()
        );
    }

    // The MAP-rep twin of the test above. A map-rep field's serialize is built from ONE config that
    // also serves the member-key write, and that config used to be built WITHOUT the field's
    // @custom_serialize — so the custom WRITER was dropped while @custom_deserialize kept being
    // honored on the read side. Both halves of that asymmetry are pinned here: the byte-exact vector
    // fails if the writer reverts to the default (`chunked`/`hexed` would come out as plain definite byte
    // strings), and the round-trip fails outright because the custom READER rejects exactly those
    // default-shaped bytes.
    #[test]
    fn map_custom_serialization() {
        use cbor_event::{Sz, StringLenSz};
        let v = MapStructWithCustomSerialization::new(
            vec![0xCA, 0xFE, 0xF0, 0x0D],
            vec![0xBA, 0xAD, 0xD0, 0x0D],
            vec![0x03, 0x01, 0x04, 0x01],
            1024,
            7,
        );
        deser_test(&v);
        let chunked_enc = StringLenSz::Indefinite(vec![(1, Sz::Inline); 4]);
        // member keys go out length-first then lexicographic, not in declaration order
        let expected_bytes = [
            map_def(5),
                cbor_string("hexed"),
                    cbor_string("baadd00d"),
                cbor_string("plain"),
                    cbor_int(7, Sz::Inline),
                cbor_string("tagged"),
                    cbor_tag(9),
                    cbor_string("1024"),
                cbor_string("aliased"),
                    cbor_bytes_sz(vec![0x03, 0x01, 0x04, 0x01], chunked_enc.clone()),
                cbor_string("chunked"),
                    cbor_bytes_sz(vec![0xCA, 0xFE, 0xF0, 0x0D], chunked_enc.clone()),
        ].concat();
        assert_eq!(expected_bytes, v.to_cbor_bytes());
        // and the reader agrees with that writer: the bytes the DEFAULT writer would have produced
        // for `chunked` (a definite byte string) are REJECTED, so a dropped custom writer cannot go
        // unnoticed as a merely-cosmetic difference
        let default_shaped = [
            map_def(5),
                cbor_string("hexed"),
                    cbor_string("baadd00d"),
                cbor_string("plain"),
                    cbor_int(7, Sz::Inline),
                cbor_string("tagged"),
                    cbor_tag(9),
                    cbor_string("1024"),
                cbor_string("aliased"),
                    cbor_bytes_sz(vec![0x03, 0x01, 0x04, 0x01], chunked_enc.clone()),
                cbor_string("chunked"),
                    cbor_bytes_sz(vec![0xCA, 0xFE, 0xF0, 0x0D], StringLenSz::Len(Sz::Inline)),
        ].concat();
        // the custom reader must reject the default writer's shape for `chunked`
        assert_decode_reject_reason::<MapStructWithCustomSerialization>(
            &default_shaped,
            "needs indefinite chunking",
        );
    }

    #[test]
    fn wrapper_table() {
        use cbor_event::Sz;
        let bytes = vec![
            map_sz(3, Sz::Inline),
                cbor_int(5, Sz::Inline),
                    cbor_int(4, Sz::Inline),
                cbor_int(3, Sz::Inline),
                    cbor_int(2, Sz::Inline),
                cbor_int(1, Sz::Inline),
                    cbor_int(0, Sz::Inline),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let from_bytes = WrapperTable::from_cbor_bytes(&bytes).unwrap();
        deser_test(&from_bytes);
    }

    #[test]
    fn wrapper_list() {
        use cbor_event::Sz;
        let bytes = vec![
            arr_sz(5, Sz::Inline),
                cbor_int(5, Sz::Inline),
                cbor_int(4, Sz::Inline),
                cbor_int(3, Sz::Inline),
                cbor_int(2, Sz::Inline),
                cbor_int(1, Sz::Inline),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let from_bytes = WrapperList::from_cbor_bytes(&bytes).unwrap();
        deser_test(&from_bytes);
    }

    #[test]
    fn wrapper_getter() {
        let x = WrapperInt::new(128);
        assert_eq!(128, x.custom_getter());
    }

    #[test]
    fn docs() {
        use std::str::FromStr;
        // reading the file is the only way to test for comments being generated
        let lib_rs_with_tests = std::fs::read_to_string(std::path::PathBuf::from_str("src").unwrap().join("generated").join("mod.rs")).unwrap();
        // generated/mod.rs includes this very test (and thus those strings we're searching for), so strip from the
        // unique anchor at the top of tests/core/tests.rs down. Robust vs the first `#[cfg(test)]` (an emitted
        // test module could shift it). The marker is split so it's contiguous only at the anchor, not here.
        let anchor = concat!("// CORE_TESTS", "_TRUNCATION_ANCHOR");
        let lib_rs = &lib_rs_with_tests[..lib_rs_with_tests
            .find(anchor)
            .expect("truncation anchor missing from generated/mod.rs — tests/core/tests.rs must open with it so the source-inspection tests can strip their own literals")];
        assert!(lib_rs.contains("this is a field-level comment"));
        assert!(lib_rs.contains("bar is a u64"));
        assert!(lib_rs.contains("struct documentation here"));
        assert!(lib_rs.contains("comment-about-first"));
        assert!(lib_rs.contains("comments about second"));
        assert!(lib_rs.contains("type-level comment"));
    }

    // Deserialize errors from a record's container header / length parsing must name the type that
    // failed, the same way field-level failures already do. Today the header reads
    // (`raw.map()?`/`raw.array()?`) and the `read_elems`/`finish` length checks sit OUTSIDE the
    // `.annotate("TypeName")` closure, so a wrong container major type or a wrong entry count yields
    // an error whose Display carries no type-name location (`Deserialize::from_cbor_bytes` does not
    // re-annotate). Each case below pins `err.to_string().contains("<TypeName>")`. Split into one
    // test per case so each red/green outcome is observed independently rather than the first assert
    // masking the rest. The `_control` cases are GREEN today (their failures arise INSIDE the
    // annotate closure) and anchor that any fix must not drop the annotation that already works.
    // See static/error.rs: a `Some(location)` prints "Deserialization failed in {location} because:",
    // a `None` prints "Deserialization:".
    //
    // map-rep struct exercised: TableArrMembers = { tab, arr, arr2 } (3 mandatory keys).
    // array-rep struct exercised: Foo = [uint, text, bytes] (read via read_elems(3) + finish()).

    // (a) Wrong major type at the map header: a bare uint (0x00) where a map is required. `raw.map()?`
    // runs before the annotate closure, so the CBOR type error has no type-name location.
    #[test]
    fn error_annotation_map_wrong_major_type() {
        let err = TableArrMembers::from_cbor_bytes(&[0x00u8])
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("TableArrMembers"),
            "map-rep wrong-major-type error must name the type, got: {err}"
        );
    }

    // (b) Right major type, wrong length: a definite map with 1 entry ({"tab": {}}) against the 3
    // mandatory fields. `read_len.read_elems(3)?` (3 > 1) trips DefiniteLenMismatch before the
    // annotate closure, so again no type-name location.
    #[test]
    fn error_annotation_map_wrong_length() {
        let bytes = [
            map_def(1), // definite map, 1 entry
            cbor_string("tab"),
            map_def(0), // "tab" => {}
        ]
        .concat();
        let err = TableArrMembers::from_cbor_bytes(&bytes)
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("TableArrMembers"),
            "map-rep wrong-length error must name the type, got: {err}"
        );
    }

    // (c1) Wrong major type at the array header: a bare uint (0x00) where an array is required.
    // `raw.array()?` runs before the annotate closure.
    #[test]
    fn error_annotation_array_wrong_major_type() {
        let err = Foo::from_cbor_bytes(&[0x00u8]).unwrap_err().to_string();
        assert!(
            err.contains("Foo"),
            "array-rep wrong-major-type error must name the type, got: {err}"
        );
    }

    // (c2) Right major type, wrong length: a definite array declaring 2 elements against 3 fields.
    // `read_elems(3)?` (3 > 2) trips DefiniteLenMismatch before the annotate closure.
    #[test]
    fn error_annotation_array_wrong_length() {
        let err = Foo::from_cbor_bytes(&arr_def(2)).unwrap_err().to_string();
        assert!(
            err.contains("Foo"),
            "array-rep wrong-length error must name the type, got: {err}"
        );
    }

    // (d1) CONTROL (green today): a field-level decode failure inside the value-decode path. An
    // indefinite map (so the header length checks are no-ops) whose "tab" value is a uint instead of
    // the required inner map — the inner `raw.map()?` error is annotated "tab" then "TableArrMembers".
    #[test]
    fn error_annotation_field_level_control() {
        let bytes = [
            vec![MAP_INDEF],
            cbor_string("tab"),
            cbor_int(5, cbor_event::Sz::Inline), // tab => 5 (not a map)
            vec![BREAK],
        ]
        .concat();
        let err = TableArrMembers::from_cbor_bytes(&bytes)
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("TableArrMembers"),
            "field-level failure must (still) name the type, got: {err}"
        );
    }

    // (d2) CONTROL (green today): a missing mandatory field. An indefinite map dropping the required
    // "arr2" — the MandatoryFieldMissing is raised INSIDE the annotate closure, so it is annotated.
    #[test]
    fn error_annotation_missing_field_control() {
        let bytes = [
            vec![MAP_INDEF],
            cbor_string("tab"),
            map_def(0),
            cbor_string("arr"),
            arr_def(0),
            vec![BREAK], // no "arr2"
        ]
        .concat();
        let err = TableArrMembers::from_cbor_bytes(&bytes)
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("TableArrMembers"),
            "missing-mandatory-field failure must name the type, got: {err}"
        );
    }

    // A tag mismatch on a tagged top-level TYPE CHOICE (`tagged_type_choice = #6.11(uint / text)`)
    // must name the type EXACTLY ONCE. This enum deserializes DIRECTLY (no container rep), so the
    // tag check `generate_tag_check` emits sits inside the `.annotate("TaggedTypeChoice")` closure;
    // if it used the location-carrying form (`DeserializeError::new("TaggedTypeChoice", ..)`) the
    // closure's map_err would PREPEND the name again, reading "TaggedTypeChoice.TaggedTypeChoice".
    // Mirrors `error_annotation_tag_mismatch_single_name` in tests/preserve-encodings, but pins the
    // enum-direct tag-check path that no other fixture exercises.
    //
    // 0xcb is tag 11 (major 6, value 11) and 0xcc is tag 12; 0x05 is uint 5. So `[0xcc, 0x05]`
    // decodes the tag successfully and trips `tag != 11` → TagMismatch, while `[0xcb, 0x05]` decodes.
    #[test]
    fn error_annotation_tag_mismatch_type_choice_direct() {
        // Happy path: correct tag decodes Ok — guards against the test passing because the type
        // rejects everything. Unwrapped (DeserializeError: Debug) so a regression's first capture
        // carries the error, not just "is_ok was false".
        TaggedTypeChoice::from_cbor_bytes(&[0xcb, 0x05]).unwrap();
        let err = TaggedTypeChoice::from_cbor_bytes(&[0xcc, 0x05])
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("TaggedTypeChoice"),
            "tag-mismatch error must name the type, got: {err}"
        );
        assert!(
            !err.contains("TaggedTypeChoice.TaggedTypeChoice"),
            "tag-mismatch error must not double-annotate, got: {err}"
        );
        // The Display must also identify the tag mismatch (static/error.rs formats TagMismatch as
        // "Expected tag {expected}, found {found}").
        assert!(
            err.contains("Expected tag 11"),
            "tag-mismatch error must state the expected tag, got: {err}"
        );
        assert!(
            err.contains("found 12"),
            "tag-mismatch error must state the found tag, got: {err}"
        );
    }

    // A NoVariantMatched failure on a directly-deserializing type choice must name the type EXACTLY
    // ONCE. This enum deserializes DIRECTLY, so the `_ => NoVariantMatched` arm sits inside the
    // `.annotate("TaggedTypeChoice")` closure; the arm therefore emits the LOCATIONLESS
    // `DeserializeFailure::NoVariantMatched.into()` form and lets the closure supply the name (was a
    // double-annotation gap: the name-carrying `DeserializeError::new("TaggedTypeChoice", ..)` form
    // let the closure prepend the name again, reading "TaggedTypeChoice.TaggedTypeChoice"). Sibling
    // of the tag-mismatch once-only contract above; pins the NoVariantMatched arm, which no other
    // fixture exercises for the double-annotation shape.
    //
    // `[0xcb, 0x80]` = tag 11 (correct) then an empty ARRAY — neither the uint nor the text variant
    // matches, so the `_ => NoVariantMatched` arm fires.
    #[test]
    fn error_annotation_no_variant_single_name() {
        let err = TaggedTypeChoice::from_cbor_bytes(&[0xcb, 0x80])
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("No variant matched"),
            "the array payload must fail as NoVariantMatched, got: {err}"
        );
        assert!(
            err.contains("failed in TaggedTypeChoice"),
            "NoVariantMatched error must name the type, got: {err}"
        );
        assert!(
            !err.contains("TaggedTypeChoice.TaggedTypeChoice"),
            "NoVariantMatched error must not double-annotate, got: {err}"
        );
    }

    // A plain-group standalone deserialize() must name the group EXACTLY ONCE on a header error.
    // `plain = (d: #6.23(uint), e: tagged_text)` decodes standalone as a 2-element array; its
    // pre-delegation scaffolding (the `raw.array()?` container read + the read_len length checks)
    // now sits inside an `.annotate("Plain")` closure, while the delegated
    // deserialize_as_embedded_group() body stays OUTSIDE it (already annotated per-field). A bare
    // uint where the array is required trips `raw.array()?` inside that closure.
    #[test]
    fn error_annotation_plain_group_header_single_name() {
        let err = Plain::from_cbor_bytes(&[0x00u8]).unwrap_err().to_string();
        assert!(
            err.contains("Plain"),
            "plain-group header error must name the group, got: {err}"
        );
        assert!(
            !err.contains("Plain.Plain"),
            "plain-group header error must not double-annotate, got: {err}"
        );
    }

    // A newtype wrapper's wrong-container read must name the wrapper EXACTLY ONCE. `wrapper_list =
    // [* uint]` (@newtype) reads an array; the whole deserialize body is wrapped in an
    // `.annotate("WrapperList")` closure, so a bare uint (0x00) where the array is required carries
    // the "WrapperList" location without doubling.
    #[test]
    fn error_annotation_wrapper_wrong_container_single_name() {
        let err = WrapperList::from_cbor_bytes(&[0x00u8])
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("WrapperList"),
            "wrapper wrong-container error must name the wrapper, got: {err}"
        );
        assert!(
            !err.contains("WrapperList.WrapperList"),
            "wrapper wrong-container error must not double-annotate, got: {err}"
        );
    }

    // A BOUNDED wrapper's out-of-range decode must name the wrapper EXACTLY ONCE. This pins the 3a
    // locationless-switch: inside the annotate closure the range check emits the LOCATIONLESS
    // `DeserializeFailure::RangeCheck.into()` form (the closure supplies the name); the name-carrying
    // `DeserializeError::new("NintGeNewtype", ..)` form there would let the closure prepend the name
    // AGAIN, reading "NintGeNewtype.NintGeNewtype". `nint_ge_newtype = nint .ge -5` accepts -5..=-1;
    // -6 (nint magnitude 5, 0x25) is out of range.
    #[test]
    fn error_annotation_bounded_wrapper_range_single_name() {
        // -5 (the inclusive lower bound) decodes Ok — guards against the type rejecting everything.
        NintGeNewtype::from_cbor_bytes(&[0x24u8]).unwrap(); // 0x24 = nint 4 = -5
        let err = NintGeNewtype::from_cbor_bytes(&[0x25u8]) // 0x25 = nint 5 = -6, below -5
            .unwrap_err()
            .to_string();
        // nint bounds map to a SWAPPED u64 magnitude window (`nint .ge -5` -> magnitude `<= 4`), so
        // the RangeCheck Display reads "5 not at most 4" (static/error.rs formats (None, Some) so).
        assert!(
            err.contains("not at most"),
            "bounded-wrapper decode must fail as a RangeCheck, got: {err}"
        );
        assert!(
            err.contains("NintGeNewtype"),
            "bounded-wrapper range error must name the wrapper, got: {err}"
        );
        assert!(
            !err.contains("NintGeNewtype.NintGeNewtype"),
            "bounded-wrapper range error must not double-annotate, got: {err}"
        );
    }

    // --- WI-1: NonEmptyVec two-type enforcement for `[+ T]` ---

    #[test]
    fn non_empty_vec_try_from_enforces_and_from_lossless() {
        // singleton accepted, empty rejected through the SINGLE TryFrom door
        NonEmptyVec::try_from(vec![1u64]).expect("singleton must be accepted");
        let empty: Vec<u64> = Vec::new();
        let err = NonEmptyVec::<u64>::try_from(empty).unwrap_err();
        // identical Display to the wire door (see non_empty_vec_wire_rejects_empty_same_error)
        assert!(
            err.to_string().contains("0 not at least 1"),
            "TryFrom empty must be a RangeCheck, got: {err}"
        );
        // From back is lossless
        let nev = NonEmptyVec::try_from(vec![1u64, 2, 3]).unwrap();
        let back: Vec<u64> = nev.into();
        assert_eq!(back, vec![1u64, 2, 3]);
    }

    #[test]
    fn non_empty_vec_holder_roundtrip_and_infallible_new() {
        let tags = NonEmptyVec::try_from(vec![NevBar::new(7)]).unwrap();
        let nested =
            NonEmptyVec::try_from(vec![NonEmptyVec::try_from(vec![1u64, 2]).unwrap()]).unwrap();
        let ints: NevInts = NonEmptyVec::try_from(vec![9u64]).unwrap();
        // new() is INFALLIBLE (no Result) and takes the restricted NonEmptyVec types by value; the
        // control field `plain` stays a bare Vec<String>.
        let mut holder = NevHolder::new(tags, nested, vec!["hi".to_string()], ints);
        deser_test(&holder);
        // Option<NonEmptyVec<_>>: the optional field is absence, not "empty container"
        holder.maybe = Some(NonEmptyVec::try_from(vec![NevBar::new(1)]).unwrap());
        deser_test(&holder);
    }

    #[test]
    fn non_empty_vec_wire_rejects_empty_same_error() {
        // valid: outer array(1) [ inner [+ uint] array(1) [ 1 ] ] = 81 81 01
        let mut ok = Deserializer::from(vec![0x81u8, 0x81, 0x01]);
        NevWire::deserialize(&mut ok).expect("valid single-element [+ uint] wire must deserialize");
        // invalid: outer array(1) [ EMPTY inner array(0) ] = 81 80 — the `[+ uint]` field routes the
        // empty Vec through the same NonEmptyVec::try_from door, so the wire error text MATCHES the
        // API error text asserted in non_empty_vec_try_from_enforces_and_from_lossless.
        let mut bad = Deserializer::from(vec![0x81u8, 0x80]);
        let err = NevWire::deserialize(&mut bad).unwrap_err();
        assert!(
            err.to_string().contains("0 not at least 1"),
            "empty wire array must reject as a RangeCheck, got: {err}"
        );
    }

    // The named/inline `[+ elem]` combinations (free-named rule, self-named rule, inline dedup)
    // share the SAME rust-side representation — every member is a bare NonEmptyVec<Elem> and the
    // named rules are transparent aliases to it — so one round-trip covers the rust leg; the wasm
    // surface differences live in tests_wasm.rs.
    #[test]
    fn non_empty_vec_named_and_deduped_members_roundtrip() {
        let pts: NevPts = NonEmptyVec::try_from(vec![NevPt::new(1)]).unwrap();
        let pts_inline = NonEmptyVec::try_from(vec![NevPt::new(2), NevPt::new(3)]).unwrap();
        let qs: NevQList = NonEmptyVec::try_from(vec![NevQ::new(7)]).unwrap();
        let holder = NevHolder2::new(pts, pts_inline, qs);
        deser_test(&holder);
    }

    // --- WI-2: NonEmptyMap two-type enforcement for `{+ k => v}` ---

    #[test]
    fn non_empty_map_try_from_enforces_and_from_lossless() {
        // one entry accepted, empty rejected through the SINGLE TryFrom door
        let one = std::collections::BTreeMap::from([("a".to_string(), 1u64)]);
        NonEmptyMap::try_from(one).expect("one-entry map must be accepted");
        let empty: std::collections::BTreeMap<String, u64> = std::collections::BTreeMap::new();
        let err = NonEmptyMap::try_from(empty).unwrap_err();
        // identical Display to the wire door (see non_empty_map_wire_rejects_empty_same_error)
        assert!(
            err.to_string().contains("0 not at least 1"),
            "TryFrom empty must be a RangeCheck, got: {err}"
        );
        // From back is lossless
        let src = std::collections::BTreeMap::from([("a".to_string(), 1u64), ("b".to_string(), 2)]);
        let nem = NonEmptyMap::try_from(src.clone()).unwrap();
        let back: std::collections::BTreeMap<String, u64> = nem.into();
        assert_eq!(back, src);
    }

    #[test]
    fn non_empty_map_holder_roundtrip_and_infallible_new() {
        let inline =
            NonEmptyMap::try_from(std::collections::BTreeMap::from([("x".to_string(), 7u64)]))
                .unwrap();
        let named: NemNamed = NonEmptyMap::try_from(std::collections::BTreeMap::from([(
            "k".to_string(),
            NemVal::new(3),
        )]))
        .unwrap();
        // new() is INFALLIBLE (no Result) and takes the restricted NonEmptyMap types by value; the
        // control field `plain` stays a bare BTreeMap<String, u64>.
        let mut holder = NemHolder::new(
            inline,
            std::collections::BTreeMap::from([("p".to_string(), 9u64)]),
            named,
        );
        deser_test(&holder);
        // Option<NonEmptyMap<_>>: the optional field is absence, not "empty container"
        holder.maybe =
            Some(NonEmptyMap::try_from(std::collections::BTreeMap::from([(1u64, "v".to_string())]))
                .unwrap());
        deser_test(&holder);
    }

    #[test]
    fn non_empty_map_wire_rejects_empty_same_error() {
        // valid: outer map(1) { "m": inner map(1) { 0: 1 } } = a1 61 6d a1 00 01
        let mut ok = Deserializer::from(vec![
            0xa1u8, 0x61, 0x6d, 0xa1, 0x00, 0x01,
        ]);
        NemWire::deserialize(&mut ok).expect("valid single-entry {+ uint => uint} wire");
        // invalid: outer map(1) { "m": EMPTY inner map(0) } = a1 61 6d a0 — the `{+ uint => uint}`
        // field routes the empty map through the same NonEmptyMap::try_from door, so the wire error
        // text MATCHES the API error text asserted above.
        let mut bad = Deserializer::from(vec![0xa1u8, 0x61, 0x6d, 0xa0]);
        let err = NemWire::deserialize(&mut bad).unwrap_err();
        assert!(
            err.to_string().contains("0 not at least 1"),
            "empty wire map must reject as a RangeCheck, got: {err}"
        );
    }

    #[test]
    fn non_empty_map_value_mutation_and_checked_remove() {
        // Value-level `&mut` needs no checked door — a `&mut V` cannot change the map's LENGTH.
        // The nested shape is the motivating case: update an inner NonEmptyMap in place.
        let mut outer = NonEmptyMap::new("voter".to_string(), NonEmptyMap::new(1u64, 10u64));
        outer.get_mut(&"voter".to_string()).unwrap().insert(2, 20);
        for v in outer.values_mut() {
            v.insert(3, 30);
        }
        for (_, v) in outer.iter_mut() {
            v.insert(4, 40);
        }
        assert_eq!(outer.get(&"voter".to_string()).unwrap().len(), 4);
        // Length-shrinking stays checked: refused at length 1, allowed above it.
        let mut m = NonEmptyMap::new(1u64, "a".to_string());
        let err = m.remove(&1).unwrap_err();
        assert!(
            err.to_string().contains("0 not at least 1"),
            "remove at length 1 must be refused, got: {err}"
        );
        m.insert(2, "b".to_string());
        assert_eq!(m.remove(&1).unwrap(), Some("a".to_string()));
        assert_eq!(m.len(), 1);
    }

    #[test]
    fn nullable_specials() {
        // The `T / null` null-peek over Special-typed inners must rewind by the ACTUAL width
        // `special()` consumed (1/2/3/5/9 bytes), not a hardcoded 1 byte (cbor_event 3.2.0 upgrade
        // flip vectors — the 2.4.0-era emission both rejected valid multi-byte specials and
        // accepted malformed two-byte simples).
        // [true, 1.5(fb)] — the 9-byte float after the peek; rejected by the old 1-byte rewind.
        let fb_1_5 = [0xfbu8, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let good: Vec<u8> = [arr_def(2), vec![0xf5], fb_1_5.to_vec()].concat();
        let d = NullableSpecials::from_cbor_bytes(&good).unwrap();
        assert_eq!(d.b, Some(true));
        assert_eq!(d.f, Some(1.5));
        // `f` is the unconstrained `float`, and every float write is the value's shortest form
        // (RFC 8949 §4.1), so the fb-headed 1.5 comes back out as f9
        assert_eq!(
            d.to_cbor_bytes(),
            [arr_def(2), vec![0xf5], vec![0xf9, 0x3e, 0x00]].concat()
        );
        // [null, null]
        let nulls: Vec<u8> = [arr_def(2), vec![0xf6, 0xf6]].concat();
        let d = NullableSpecials::from_cbor_bytes(&nulls).unwrap();
        assert!(d.b.is_none() && d.f.is_none());
        // half-precision decode correctness (the f16 flip): f9 3e00 = 1.5, not the raw bit pattern
        let f16v: Vec<u8> = [arr_def(2), vec![0xf6, 0xf9, 0x3e, 0x00]].concat();
        let d = NullableSpecials::from_cbor_bytes(&f16v).unwrap();
        assert_eq!(d.f, Some(1.5));
        // two-byte simple `f8 f5` in the nullable-bool slot: the 2.4.0 peek consumed 2 bytes,
        // rewound 1, and re-read the PAYLOAD byte f5 as `true` — accepting malformed input. Reject.
        assert_decode_reject_reason::<NullableSpecials>(&[0x82, 0xf8, 0xf5, 0xf6], "Expected Special::Bool, received Unassigned(245)");
        // RFC 8949 §3.3: fc/fd/fe and two-byte simples < 0x20 are not well-formed — reject
        for bad in [
            &[0x82u8, 0xfc, 0xf6][..],
            &[0x82, 0xfd, 0xf6],
            &[0x82, 0xfe, 0xf6],
            &[0x82, 0xf8, 0x1f, 0xf6],
        ] {
            assert_decode_reject_reason::<NullableSpecials>(bad, "non-well-formed encoding of simple value");
        }
        // a lone Break where the bool item should be — an error, not a mis-decode
        assert_decode_reject_reason::<NullableSpecials>(&[0x82, 0xff, 0xf6], "Expected Special::Bool, received Break");
    }

    #[test]
    fn invalid_utf8_text_rejects() {
        // UTF-8 strictness fence: 2.4.0 and 3.1.0+ are strict (the yanked 3.0.0 was lossy) — an
        // invalid-UTF-8 major-type-3 payload must reject, never decode lossily.
        // foo = [uint, text, bytes] → [0, <2-byte text ff fe>, h'']
        assert_decode_reject_reason::<Foo>(&[0x83, 0x00, 0x62, 0xff, 0xfe, 0x40], "expected a valid utf8 string text");
    }

    #[test]
    fn hostile_inputs_error_not_panic() {
        // Absorbed-fix fences for the cbor_event 3.2.0 upgrade (hostile/truncated input must yield
        // Err — never panic, never pre-allocate a claimed length).
        // truncated 8-byte length argument on an array head
        assert_decode_reject_reason::<Foo>(&[0x9b, 0xff, 0xff], "not enough bytes");
        // definite bytes head claiming ~2 GiB with no payload: must Err promptly instead of
        // pre-allocating the claimed length (the 2.4.0 over-allocation class)
        let huge: Vec<u8> = [
            &[0x83u8, 0x00, 0x60][..],
            &[0x5b, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00],
        ]
        .concat();
        assert_decode_reject_reason::<Foo>(&huge, "not enough bytes");
        // a lone Break as the whole document
        assert_decode_reject_reason::<Foo>(&[0xff], "expected `Array' byte received `Special'");
        // Break where the first element of a definite array should be
        assert_decode_reject_reason::<Foo>(&[0x83, 0xff, 0x60, 0x40], "expected `UnsignedInteger' byte received `Special'");
        // ALIAS-typed target: `top_level_array = [* uint]` is a bare `pub type = Vec<u64>`, so its
        // from_cbor_bytes routes through cbor_event's own generic container impls (the one
        // generated-code route into the upstream indefinite-loop helper).
        // truncated indefinite array (no Break)
        assert_decode_reject_reason::<TopLevelArray>(&[0x9f, 0x00], "not enough bytes");
        // a reserved simple value in an element slot. This lands on the ELEMENT's type check,
        // not on the RFC 8949 §3.3 well-formedness check the byte itself violates: a `[* uint]`
        // element can never reach the latter, because nothing decodes the special before its major
        // type is judged. `nullable_specials` owns the well-formedness boundary, on a member whose
        // reader does call `special()`.
        assert_decode_reject_reason::<TopLevelArray>(&[0x9f, 0xfc, 0xff], "expected `UnsignedInteger' byte received `Special'");
        // truncated definite element list
        assert_decode_reject_reason::<TopLevelArray>(&[0x83, 0x00], "not enough bytes");
    }

    // ---- the six float prelude names: six VALUE classes -----------------------------------------
    // `float_heads = [h: float16, u: float16-32, w: float32-64, s: float32, d: float64, f: float]`.
    // A CDDL float name is a set of VALUES, not of encodings: RFC 8610 §2.2.3 says the `#7.x`
    // notation "is about a set of values at the data model level … it does not mandate that these
    // values also do have to be serialized as half-precision floats". The six names PARTITION the
    // floats by shortest lossless form, so `1.5` is a `float16` and NOT a `float32`, whatever head
    // it arrived under, and the classes are disjoint.
    //
    // These vectors are that boundary in both directions: a read accepts ANY head and judges the
    // decoded value, and a write emits the value's shortest form (RFC 8949 §4.1) — which for a
    // member IS its declared width, so the two rules meet without a special case.

    // 1.5 — shortest form `f9`, so a `float16` value.
    const F9_1_5: &[u8] = &[0xf9, 0x3e, 0x00];
    const FA_1_5: &[u8] = &[0xfa, 0x3f, 0xc0, 0x00, 0x00];
    const FB_1_5: &[u8] = &[0xfb, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    // 1e10 — binary32-exact but far outside binary16's range, so shortest form `fa`: a `float32`.
    const FA_1E10: &[u8] = &[0xfa, 0x50, 0x15, 0x02, 0xf9];
    const FB_1E10: &[u8] = &[0xfb, 0x42, 0x02, 0xa0, 0x5f, 0x20, 0x00, 0x00, 0x00];
    // 1.1 — needs the full binary64 mantissa, so shortest form `fb`: a `float64`.
    const FB_1_1: &[u8] = &[0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a];

    fn float_heads_bytes(items: &[&[u8]]) -> Vec<u8> {
        let mut v = arr_def(6);
        for i in items {
            v.extend_from_slice(i);
        }
        v
    }

    /// The shortest form of each member's value — what every write produces.
    fn float_heads_shortest() -> Vec<u8> {
        float_heads_bytes(&[F9_1_5, F9_1_5, FA_1E10, FA_1E10, FB_1_1, F9_1_5])
    }

    #[test]
    fn float_heads_accept_any_head_and_judge_the_value() {
        // Each member is fed a value ITS OWN class admits, at every head that can carry it — an
        // `fb`-headed 1.5 is a perfectly good `float16`, which is the half that a head-strict
        // reader gets wrong (it would reject the output of every conforming preferred-serialization
        // encoder).
        for (h, u, w, sfa, f) in [
            (F9_1_5, F9_1_5, FA_1E10, FA_1E10, F9_1_5),
            (FA_1_5, FA_1_5, FB_1E10, FB_1E10, FA_1_5),
            (FB_1_5, FB_1_5, FB_1E10, FA_1E10, FB_1_5),
        ] {
            let bytes = float_heads_bytes(&[h, u, w, sfa, FB_1_1, f]);
            let d = FloatHeads::from_cbor_bytes(&bytes).unwrap();
            assert_eq!((d.h, d.u, d.s), (1.5f32, 1.5f32, 1e10f32));
            assert_eq!((d.w, d.d, d.f), (1e10f64, 1.1f64, 1.5f64));
            // and the re-encode is the value's SHORTEST form, not the head that was read — the
            // default profile carries no recorded width, so this is the whole write rule at once.
            assert_eq!(d.to_cbor_bytes(), float_heads_shortest());
        }
    }

    #[test]
    fn float_heads_reject_values_outside_their_class() {
        FloatHeads::from_cbor_bytes(&float_heads_shortest()).unwrap();
        // (member index, a value that member's class does NOT contain, at some head, the
        // rejection it must produce). `f` (`float`) is every float value, so it has none. The
        // reason column is the runtime's own spelling of each row's `why`, kept beside it as a
        // comment: naming the width class BOTH sides is what keeps a row from passing on some
        // other failure that happens to reach the same outcome.
        for (idx, bad, expect) in [
            // 1e10 is a float32, not a float16
            (0usize, FA_1E10, "Expected a float16 value, found a float32 value"),
            // 1.1 is a float64, not a float16
            (0, FB_1_1, "Expected a float16 value, found a float64 value"),
            // 1.1 is a float64, not a float16-32
            (1, FB_1_1, "Expected a float16 - float32 value, found a float64 value"),
            // 1.5 is a float16, not a float32-64
            (2, F9_1_5, "Expected a float32 - float64 value, found a float16 value"),
            // an fb-headed 1.5 is still a float16
            (2, FB_1_5, "Expected a float32 - float64 value, found a float16 value"),
            // an fa-headed 1.5 is still a float16, not a float32
            (3, FA_1_5, "Expected a float32 value, found a float16 value"),
            // 1.1 is a float64, not a float32
            (3, FB_1_1, "Expected a float32 value, found a float64 value"),
            // an fb-headed 1.5 is still a float16, not a float64
            (4, FB_1_5, "Expected a float64 value, found a float16 value"),
            // 1e10 is a float32, not a float64
            (4, FA_1E10, "Expected a float64 value, found a float32 value"),
        ] {
            let mut items: Vec<&[u8]> = vec![F9_1_5, F9_1_5, FA_1E10, FA_1E10, FB_1_1, F9_1_5];
            items[idx] = bad;
            assert_decode_reject_reason::<FloatHeads>(&float_heads_bytes(&items), expect);
        }
    }

    #[test]
    fn float_heads_union_names_write_the_shortest_form() {
        // A union name spans two widths, and writes whichever one its value's shortest form needs.
        // 1.5 is f16-exact; 1e10 is f32-exact but not f16-exact; 1.1 is neither.
        let b = FloatHeads::new(1.5, 1.5, 1.1, 1e10, 1.1, 1.5).to_cbor_bytes();
        assert_eq!(&b[4..7], F9_1_5, "float16-32 holding a float16 value -> f9");
        assert_eq!(b[7], 0xfb, "float32-64 holding a float64 value -> fb");
        let b = FloatHeads::new(1.5, 100000.0, 1e10, 1e10, 1.1, 1.5).to_cbor_bytes();
        assert_eq!(b[4], 0xfa, "float16-32 holding a float32 value -> fa");
        assert_eq!(b[9], 0xfa, "float32-64 holding a float32 value -> fa");
    }

    #[test]
    fn float_heads_non_member_fails_serialize_loudly() {
        // `float16` is the values whose shortest form is `f9`, and 1.1 is not one of them. There is
        // no head at which writing it would be right: a wider one emits bytes this crate's own
        // reader rejects for that member, a narrower one rounds the value. So the write FAILS.
        let value = FloatHeads::new(1.1, 1.5, 1e10, 1e10, 1.1, 1.5);
        let mut buf = cbor_event::se::Serializer::new_vec();
        assert!(cbor_event::se::Serialize::serialize(&value, &mut buf).is_err());
        // the same in the other direction: 1.5 is a float16, so it is not a `float32` value
        let value = FloatHeads::new(1.5, 1.5, 1e10, 1.5, 1.1, 1.5);
        let mut buf = cbor_event::se::Serializer::new_vec();
        assert!(cbor_event::se::Serialize::serialize(&value, &mut buf).is_err());
    }

    #[test]
    fn float_heads_preserve_nan_payloads_at_every_width() {
        // A NaN PAYLOAD is data: it survives the read, the f32 carriers' narrowing, and the write.
        // (Both conversions are done in software — a hardware cast may quiet a signaling NaN.)
        let f9_nan: &[u8] = &[0xf9, 0x7e, 0x01];
        let fa_nan: &[u8] = &[0xfa, 0x7f, 0xc0, 0x00, 0x01];
        let fb_nan: &[u8] = &[0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
        let bytes = float_heads_bytes(&[f9_nan, f9_nan, fa_nan, fa_nan, fb_nan, fb_nan]);
        let d = FloatHeads::from_cbor_bytes(&bytes).unwrap();
        assert!(d.h.is_nan() && d.u.is_nan() && d.s.is_nan());
        assert!(d.w.is_nan() && d.d.is_nan() && d.f.is_nan());
        assert_eq!(d.to_cbor_bytes(), bytes);
    }
}
