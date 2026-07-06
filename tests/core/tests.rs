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
        let mut deserializer = Deserializer::from(std::io::Cursor::new(orig_bytes.clone()));
        let deser = T::deserialize(&mut deserializer).unwrap();
        print_cbor_types("deser", &deser.to_cbor_bytes());
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.as_ref().position(), orig_bytes.len() as u64);
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

    // Round-trip tests only ever feed well-formed CBOR; these pin that *malformed* input is
    // rejected rather than silently accepted. Structural cases the
    // bounds test doesn't reach: wrong shape, wrong element type, wrong/missing tag. Each case has
    // an is_ok() baseline so a reject can't pass for the wrong reason (e.g. garbage encoding).
    #[test]
    fn structural_rejects() {
        // Foo = [uint, text, bytes]
        let bytes3 = vec![0x43u8, 1, 2, 3]; // cbor bytes(3)
        let foo_ok = [arr_def(3), cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone()].concat();
        assert!(Foo::from_cbor_bytes(&foo_ok).is_ok());
        assert!(Foo::from_cbor_bytes(&[]).is_err()); // empty input
        // trailing bytes after a complete value are rejected, not silently ignored (from_cbor_bytes
        // checks the cursor reached the end of the buffer)
        let foo_trailing_err = Foo::from_cbor_bytes(&[foo_ok.clone(), vec![0xff]].concat()).unwrap_err();
        assert!(foo_trailing_err.to_string().contains("trailing data"), "{foo_trailing_err}");
        // array too short: the bytes field is missing
        assert!(Foo::from_cbor_bytes(&[arr_def(2), cbor_int(1, cbor_event::Sz::Inline), cbor_string("a")].concat()).is_err());
        // wrong outer container: a map where the array is required
        assert!(Foo::from_cbor_bytes(&[map_def(3), cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone()].concat()).is_err());
        // wrong type in the uint slot (text where a uint is required)
        assert!(Foo::from_cbor_bytes(&[arr_def(3), cbor_string("x"), cbor_string("a"), bytes3.clone()].concat()).is_err());
        // wrong type in the text slot (bytes where text is required)
        assert!(Foo::from_cbor_bytes(&[arr_def(3), cbor_int(1, cbor_event::Sz::Inline), bytes3.clone(), bytes3.clone()].concat()).is_err());
        // wrong type in the bytes slot (uint where bytes is required)
        assert!(Foo::from_cbor_bytes(&[arr_def(3), cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), cbor_int(7, cbor_event::Sz::Inline)].concat()).is_err());

        // Foo2 = #6.23([uint, opt_text]): the tag must be present and correct.
        let foo2 = |tag: Option<u64>| {
            let mut b = Vec::new();
            if let Some(t) = tag {
                b.extend(cbor_tag_sz(t, cbor_event::Sz::Inline));
            }
            b.extend([arr_def(2), cbor_int(1, cbor_event::Sz::Inline), vec![NULL]].concat());
            Foo2::from_cbor_bytes(&b)
        };
        assert!(foo2(Some(23)).is_ok());
        assert!(foo2(Some(22)).is_err()); // wrong tag
        assert!(foo2(None).is_err()); // missing tag

        // Hash = bytes .size (0..8): wrong major type (uint where bytes is required).
        assert!(Hash::from_cbor_bytes(&bytes3).is_ok());
        assert!(Hash::from_cbor_bytes(&cbor_int(5, cbor_event::Sz::Inline)).is_err());

        // WrapperTable = { * uint => uint }: wrong major type (array where a map is required).
        let wrapper_table_ok = [map_def(1), cbor_int(1, cbor_event::Sz::Inline), cbor_int(2, cbor_event::Sz::Inline)].concat();
        assert!(WrapperTable::from_cbor_bytes(&wrapper_table_ok).is_ok());
        assert!(WrapperTable::from_cbor_bytes(&arr_def(0)).is_err());

        // Duplicate map keys are rejected (DeserializeFailure::DuplicateKey).
        // WrapperTable = { * uint => uint } is a definite-map table (no read_elems pre-check), so the
        // duplicate is caught directly when the second identical key fails to insert. Baseline: the
        // same two-entry map with distinct keys round-trips, so only the repeated key can reject it.
        let wrapper_table_two_keys = [
            map_def(2),
            cbor_int(1, cbor_event::Sz::Inline), cbor_int(2, cbor_event::Sz::Inline),
            cbor_int(7, cbor_event::Sz::Inline), cbor_int(8, cbor_event::Sz::Inline),
        ].concat();
        assert!(WrapperTable::from_cbor_bytes(&wrapper_table_two_keys).is_ok());
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
        assert!(TableArrMembers::from_cbor_bytes(&table_arr_members_ok).is_ok());
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
        // only via an indefinite map. The all-keys map above is the is_ok() baseline, so only dropping
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
        // which fails earlier in read_elems). foo_ok (arr_def(3)) above is the is_ok() baseline.
        let foo_too_long = [
            arr_def(4),
            cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone(),
            cbor_int(9, cbor_event::Sz::Inline),
        ].concat();
        let foo_too_long_err = Foo::from_cbor_bytes(&foo_too_long).unwrap_err();
        assert!(foo_too_long_err.to_string().contains("Definite length mismatch"), "{foo_too_long_err}");

        // An indefinite array must be terminated by a CBOR Break; any other special in the tail slot
        // trips EndingBreakMissing. The Break-terminated form is the is_ok() baseline.
        let foo_indef_ok = [
            vec![ARR_INDEF],
            cbor_int(1, cbor_event::Sz::Inline), cbor_string("a"), bytes3.clone(),
            vec![BREAK],
        ].concat();
        assert!(Foo::from_cbor_bytes(&foo_indef_ok).is_ok());
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
        // A complete definite map of the 3 required keys is the is_ok() baseline.
        let table_arr_members_def_ok = [
            map_def(3),
                cbor_string("tab"), map_def(0),
                cbor_string("arr"), arr_def(0),
                cbor_string("arr2"), arr_def(0),
        ].concat();
        assert!(TableArrMembers::from_cbor_bytes(&table_arr_members_def_ok).is_ok());
        let table_arr_members_break = [map_def(3), vec![BREAK]].concat();
        let table_arr_members_break_err = TableArrMembers::from_cbor_bytes(&table_arr_members_break).unwrap_err();
        assert!(table_arr_members_break_err.to_string().contains("Break while reading definite length sequence"), "{table_arr_members_break_err}");

        // Regression (fuzz-found DoS): the *collection* element loop (`[* uint]`, `{* uint => uint}`),
        // distinct from the struct-map path above, once did `assert_eq!(special, Break)` on ANY special
        // in element position — so a definite-length collection holding a non-Break special (e.g. a
        // `null`, `0x81 0xf6`) aborted the process instead of returning an error to the untrusted-input
        // parser this library's consumers rely on. It must now be a graceful Err. is_ok() baselines
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
        assert!(WrapperList::from_cbor_bytes(&[arr_def(1), cbor_int(1, cbor_event::Sz::Inline)].concat()).is_ok());
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

        // DefiniteLenMismatch from finish() carries no location (From<DeserializeFailure>), so Display
        // takes the None branch ("Deserialization: ") and the Some(expected) sub-branch (", expected:").
        // Foo = [uint, text, bytes] declared as a 4-element array reads 3 then trips finish().
        let foo_too_long = [
            arr_def(4),
            cbor_int(1, cbor_event::Sz::Inline),
            cbor_string("a"),
            vec![0x43u8, 1, 2, 3],
            cbor_int(9, cbor_event::Sz::Inline),
        ]
        .concat();
        let foo_len_err = Foo::from_cbor_bytes(&foo_too_long).unwrap_err().to_string();
        assert!(
            foo_len_err.starts_with("Deserialization: "),
            "{foo_len_err}"
        );
        assert!(foo_len_err.contains("found 4"), "{foo_len_err}");
        assert!(foo_len_err.contains("expected: 3"), "{foo_len_err}");

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
                arr_def(4),
                    cbor_tag(23),
                        cbor_int(7576, cbor_event::Sz::Two),
                    cbor_tag_sz(42, cbor_event::Sz::One),
                        cbor_string("wiorurri34h"),
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
        deser_test(&CborInCbor::new(foo.clone(), 9, foo.into()))
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
        assert!(make(0, 255).is_ok());
        assert!(make(1, 65535).is_ok());
        assert!(make(2, 4294967295).is_ok());
        assert!(make(3, u64::MAX as i128).is_ok());
        assert!(make(4, 127).is_ok());
        assert!(make(4, -128).is_ok());
        assert!(make(5, 32767).is_ok());
        assert!(make(5, -32768).is_ok());
        assert!(make(6, 2147483647).is_ok());
        assert!(make(6, -2147483648).is_ok());
        assert!(make(7, i64::MAX as i128).is_ok());
        assert!(make(7, i64::MIN as i128).is_ok());
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
        assert!(FloatFixedWhole::from_cbor_bytes(&expected).is_ok());
        // A wrong value in a whole-fixed slot rejects (FixedValueMismatch).
        let wrong: Vec<u8> = [arr_def(3), cbor_float(3.5), cbor_float(3.0), cbor_float(3.5)].concat();
        assert!(FloatFixedWhole::from_cbor_bytes(&wrong).is_err());
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
        deser_test(&NonOverlapBasicNotBasic::new_group_tagged(0, " T A G G E D ".to_owned()));
        deser_test(&NonOverlapBasicNotBasic::new_group_bytes(u64::MAX, "bytes .cbor basic".to_owned()));
    }

    #[test]
    fn array_opt_fields() {
        let mut foo = ArrayOptFields::new(10);
        for e in [None, Some(NonOverlappingTypeChoiceSome::U64(5)), Some(NonOverlappingTypeChoiceSome::N64(4)), Some(NonOverlappingTypeChoiceSome::Text("five".to_owned()))] {
            for a in [false, true] {
                for b in [false, true] {
                    for d in [false, true] {
                        // round-trip on non-constants
                        foo.a = if a { Some(0) } else { None };
                        foo.b = if b { Some("hello, world".to_owned()) } else { None };
                        foo.d = if d { Some("cddl-codegen".to_owned()) } else { None };
                        foo.e = e.clone();
                        deser_test(&foo);
                        // deser for constants too
                        for x in [false, true] {
                            for y in [false, true] {
                                for z in [false, true] {
                                    let mut components = vec![vec![ARR_INDEF]];
                                    let bytes = vec![
                                        vec![ARR_INDEF]
                                    ];
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
                                    components.push(vec![BREAK]);
                                    let bytes = components.into_iter().flatten().clone().collect::<Vec<u8>>();
                                    // value anchors: decode-accepts alone proved nothing — pin every
                                    // field to what the hand-built bytes above encode (x/y/z are the
                                    // fixed float constants, not fields; c: nint -10 is stored as the
                                    // magnitude m = |v + 1| = 9)
                                    let decoded = ArrayOptFields::from_cbor_bytes(&bytes).unwrap();
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
        deser_test(&Bounds::new(10, 5, 4, "abc".to_owned(), vec![5], [(0, 1), (2, 3)].into()).unwrap());
        // y is `nint .ge -5`, stored as the u64 magnitude m = |v + 1| (m = 4 ⇒ v = -5). new() enforces
        // the bound in magnitude space; regression for the inverted-nint-constructor-bound bug where the
        // check was `m < 4` (rejecting valid values, accepting invalid ones) instead of `m > 4`.
        assert!(Bounds::new(10, 5, 0, "abc".to_owned(), vec![5], [(0, 1), (2, 3)].into()).is_ok()); // m=0 ⇒ v=-1, in range
        assert!(Bounds::new(10, 5, 4, "abc".to_owned(), vec![5], [(0, 1), (2, 3)].into()).is_ok()); // m=4 ⇒ v=-5, boundary
        assert!(Bounds::new(10, 5, 5, "abc".to_owned(), vec![5], [(0, 1), (2, 3)].into()).is_err()); // m=5 ⇒ v=-6, below min
        // Same magnitude-space bound on the Wrapper (`@newtype`) path — regression for the standalone
        // bounded-nint-newtype bug, where new()/deserialize emitted `if inner < -5` on a u64 `inner`
        // (E0600, didn't compile). Also round-trips a valid value through the deserializer's own check.
        assert!(NintGeNewtype::new(0).is_ok()); // m=0 ⇒ v=-1, in range (v >= -5)
        assert!(NintGeNewtype::new(4).is_ok()); // m=4 ⇒ v=-5, boundary
        assert!(NintGeNewtype::new(5).is_err()); // m=5 ⇒ v=-6, out (v < -5)
        deser_test(&NintGeNewtype::new(4).unwrap());
        assert!(NintLeNewtype::new(5).is_ok()); // m=5 ⇒ v=-6, in range (v <= -5)
        assert!(NintLeNewtype::new(4).is_ok()); // m=4 ⇒ v=-5, boundary
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
        assert!(BoundsTypeChoice::new_bytes(vec![0; 64]).is_ok());
        assert!(BoundsTypeChoice::new_bytes(vec![0; 65]).is_err());
        assert!(BoundsGroupChoice::new_a(0, "four".to_owned()).is_ok());
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
        assert!(make(0, -5).is_ok());

        // all_neg (-10..-3): rejects ANY uint, rejects either side; accepts both endpoints.
        assert!(make(0, 5).is_err()); // uint arm entirely excluded
        assert!(make(0, -2).is_err()); // above upper
        assert!(make(0, -11).is_err()); // below lower
        assert!(make(0, -3).is_ok());
        assert!(make(0, -10).is_ok());

        // upto_zero (-10..0): the upper endpoint 0 is constraining (kills the naive drop-the-0 fix).
        assert!(make(1, 0).is_ok());
        assert!(make(1, -10).is_ok());
        assert!(make(1, 1).is_err());
        assert!(make(1, -11).is_err());

        // le_neg (int .le -3): rejects any uint; nint arm keeps the upper.
        assert!(make(2, 5).is_err());
        assert!(make(2, -2).is_err());
        assert!(make(2, -3).is_ok());
        assert!(make(2, -10).is_ok());

        // le_pos (int .le 10): the nint arm is VACUOUS and must NOT reject a large negative.
        assert!(make(3, -999999).is_ok());
        assert!(make(3, 10).is_ok());
        assert!(make(3, 11).is_err());

        // ge_pos (int .ge 3): the nint arm is EMPTY (every negative rejected).
        assert!(make(4, 3).is_ok());
        assert!(make(4, 100).is_ok());
        assert!(make(4, 2).is_err());
        assert!(make(4, -1).is_err());

        // ne_pos (int .ne 5): the excluded value is non-negative, so only the uint arm checks it.
        assert!(make(5, -5).is_ok());
        assert!(make(5, 4).is_ok());
        assert!(make(5, 6).is_ok());
        assert!(make(5, 5).is_err());

        // ne_neg (int .ne -5): the excluded value is negative, so only the nint arm checks it
        // (the uint arm must NOT try to compare a u64 against -5).
        assert!(make(6, 5).is_ok());
        assert!(make(6, -4).is_ok());
        assert!(make(6, -6).is_ok());
        assert!(make(6, -5).is_err());

        // straddle (-10..3): unchanged survivor — accepts across the sign boundary, rejects outside.
        assert!(make(7, -10).is_ok());
        assert!(make(7, 3).is_ok());
        assert!(make(7, 0).is_ok());
        assert!(make(7, -11).is_err());
        assert!(make(7, 4).is_err());

        // ne_one (int .ne 1): the excluded-value boundary where the (N+1, N-1) exclusion encoding's
        // max hits 0 — a per-side partition of (2, 0) once emitted `x < 2`, silently rejecting 0.
        assert!(make(8, 0).is_ok()); // the value the mis-check rejected
        assert!(make(8, 2).is_ok());
        assert!(make(8, -1).is_ok()); // nint arm is unconstrained by a non-negative exclusion
        assert!(make(8, 1).is_err());

        // ne_zero (int .ne 0): encoding (1, -1) has a bound on each side of the sign split; only 0
        // may reject.
        assert!(make(9, 1).is_ok());
        assert!(make(9, -1).is_ok());
        assert!(make(9, 0).is_err());
    }

    #[test]
    fn top_level_ranges() {
        // Literal-headed top-level range rules wrap into a bounds-enforcing struct (mirroring the
        // `int .op`-headed top-level wrappers), so their standalone from_cbor_bytes rejects
        // out-of-window values and a tagged rule writes/requires its tag. Pre-fix these emitted a
        // bare `pub type` alias with no ctor/deserialize, silently dropping the bounds (and the tag).

        // top_level_neg_range = -10..-3, an i64 wrapper. Its deserializer reads BOTH CBOR sign arms
        // and checks the whole window over i64, so this is also a WP1 full-window regression.
        let neg = |v: i128| TopLevelNegRange::from_cbor_bytes(&cbor_int(v, cbor_event::Sz::Eight));
        assert!(neg(5).is_err()); // any uint is out of an all-negative window
        assert!(neg(-11).is_err()); // below lower
        assert!(neg(-2).is_err()); // above upper
        assert!(neg(-3).is_ok());
        assert!(neg(-10).is_ok());
        deser_test(&TopLevelNegRange::new(-3).unwrap());
        deser_test(&TopLevelNegRange::new(-10).unwrap());
        assert!(TopLevelNegRange::new(5).is_err());
        assert!(TopLevelNegRange::new(-11).is_err());

        // top_level_pos_range = 3..10, a u64 wrapper.
        let pos = |v: i128| TopLevelPosRange::from_cbor_bytes(&cbor_int(v, cbor_event::Sz::Eight));
        assert!(pos(2).is_err());
        assert!(pos(11).is_err());
        assert!(pos(3).is_ok());
        assert!(pos(10).is_ok());
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
        assert!(TopLevelTaggedRange::from_cbor_bytes(&tagged_bytes).is_ok());
        // untagged input is rejected (a bare `pub type = u64` alias would have accepted it)
        assert!(
            TopLevelTaggedRange::from_cbor_bytes(&cbor_int(7, cbor_event::Sz::Inline)).is_err()
        );
        // out-of-window tagged input is rejected
        assert!(TopLevelTaggedRange::from_cbor_bytes(
            &[cbor_tag(5), cbor_int(11, cbor_event::Sz::Inline)].concat()
        )
        .is_err());
        // wrong tag is rejected
        assert!(TopLevelTaggedRange::from_cbor_bytes(
            &[cbor_tag(4), cbor_int(7, cbor_event::Sz::Inline)].concat()
        )
        .is_err());
        assert!(TopLevelTaggedRange::new(11).is_err());
    }

    #[test]
    fn float_bounds() {
        // `FloatBounds` fields (in order): incl 0.5..10.5, excl 0.5...10.5 (excludes 10.5),
        // lt float64 .lt 10.5, ge float64 .ge 0.5, eq float64 .eq 3.5, f32le float32 .le 10.5.
        // Every emitted check is NaN-safe accept-form (`!(x >= min && x <= max)`), so NaN — for
        // which every comparison is false — is rejected on every field.
        let base: [f64; 6] = [5.5, 5.5, 5.0, 5.0, 3.5, 5.0];
        let make = |idx: usize, v: f64| {
            let mut vals = base;
            vals[idx] = v;
            let mut cbor = arr_def(6);
            for x in vals.iter() {
                cbor.extend(cbor_float(*x));
            }
            FloatBounds::from_cbor_bytes(&cbor)
        };
        // baseline round-trips through both ctor and deserializer
        let baseline = FloatBounds::new(5.5, 5.5, 5.0, 5.0, 3.5, 5.0).unwrap();
        deser_test(&baseline);
        assert!(make(0, 5.5).is_ok());

        // incl (0.5..10.5): both endpoints accepted, just-outside rejected, NaN rejected.
        assert!(make(0, 0.5).is_ok());
        assert!(make(0, 10.5).is_ok());
        assert!(make(0, 0.4).is_err());
        assert!(make(0, 10.6).is_err());
        assert!(make(0, f64::NAN).is_err());
        assert!(FloatBounds::new(f64::NAN, 5.5, 5.0, 5.0, 3.5, 5.0).is_err());
        assert!(FloatBounds::new(0.5, 5.5, 5.0, 5.0, 3.5, 5.0).is_ok());
        assert!(FloatBounds::new(10.5, 5.5, 5.0, 5.0, 3.5, 5.0).is_ok());
        assert!(FloatBounds::new(10.6, 5.5, 5.0, 5.0, 3.5, 5.0).is_err());

        // excl (0.5...10.5): the exclusive upper endpoint 10.5 is REJECTED; the min stays inclusive.
        assert!(make(1, 0.5).is_ok());
        assert!(make(1, 10.5).is_err());
        assert!(make(1, 10.4).is_ok());
        assert!(make(1, f64::NAN).is_err());

        // lt (float64 .lt 10.5): one-sided exclusive max; no lower bound.
        assert!(make(2, -100.0).is_ok());
        assert!(make(2, 10.4).is_ok());
        assert!(make(2, 10.5).is_err());
        assert!(make(2, f64::NAN).is_err());

        // ge (float64 .ge 0.5): one-sided inclusive min; no upper bound.
        assert!(make(3, 0.5).is_ok());
        assert!(make(3, 1000.0).is_ok());
        assert!(make(3, 0.4).is_err());
        assert!(make(3, f64::NAN).is_err());

        // eq (float64 .eq 3.5): only 3.5 accepted.
        assert!(make(4, 3.5).is_ok());
        assert!(make(4, 3.4).is_err());
        assert!(make(4, 3.6).is_err());
        assert!(make(4, f64::NAN).is_err());

        // f32le (float32 .le 10.5): f32 value compared as f64 so 10.5 (exact in f32) is the boundary.
        assert!(make(5, 10.5).is_ok());
        assert!(make(5, 10.6).is_err());
        assert!(make(5, -5.0).is_ok());
        assert!(make(5, f64::NAN).is_err());
        assert!(FloatBounds::new(5.5, 5.5, 5.0, 5.0, 3.5, 10.5).is_ok());
        assert!(FloatBounds::new(5.5, 5.5, 5.0, 5.0, 3.5, 10.6).is_err());
    }

    #[test]
    fn top_level_float_ranges() {
        // float_range = 0.5..10.5 wraps into a bounds-enforcing newtype: 10.5 accepted, 10.6/NaN
        // rejected at BOTH new() and from_cbor_bytes. A bare `pub type = f64` alias would enforce
        // nothing.
        let fr = |v: f64| FloatRange::from_cbor_bytes(&cbor_float(v));
        assert!(fr(0.5).is_ok());
        assert!(fr(10.5).is_ok());
        assert!(fr(5.5).is_ok());
        assert!(fr(0.4).is_err());
        assert!(fr(10.6).is_err());
        assert!(fr(f64::NAN).is_err());
        deser_test(&FloatRange::new(0.5).unwrap());
        deser_test(&FloatRange::new(10.5).unwrap());
        assert!(FloatRange::new(10.6).is_err());
        assert!(FloatRange::new(f64::NAN).is_err());

        // float_range_excl = 0.5...10.5: the exclusive upper endpoint 10.5 is rejected.
        let fre = |v: f64| FloatRangeExcl::from_cbor_bytes(&cbor_float(v));
        assert!(fre(10.4).is_ok());
        assert!(fre(10.5).is_err());
        assert!(FloatRangeExcl::new(10.5).is_err());
        assert!(FloatRangeExcl::new(10.4).is_ok());

        // tagged_float_range = #6.5(0.5..10.5): the wrapper writes tag 5 AND enforces the window.
        let tagged = TaggedFloatRange::new(7.5).unwrap();
        let tagged_bytes = tagged.to_cbor_bytes();
        assert_eq!(tagged_bytes[0], 0xc5); // major type 6 (tag), argument 5
        assert_eq!(tagged_bytes, [cbor_tag(5), cbor_float(7.5)].concat());
        deser_test(&tagged);
        assert!(TaggedFloatRange::from_cbor_bytes(&tagged_bytes).is_ok());
        // untagged input rejected (a bare alias would drop the tag)
        assert!(TaggedFloatRange::from_cbor_bytes(&cbor_float(7.5)).is_err());
        // out-of-window tagged input rejected
        assert!(TaggedFloatRange::from_cbor_bytes(&[cbor_tag(5), cbor_float(10.6)].concat()).is_err());
        // wrong tag rejected
        assert!(TaggedFloatRange::from_cbor_bytes(&[cbor_tag(4), cbor_float(7.5)].concat()).is_err());
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
}
