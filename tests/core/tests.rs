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
        deser_test(&CborInCbor::new(foo.clone(), 9, foo))
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
        let lib_rs_with_tests = std::fs::read_to_string(std::path::PathBuf::from_str("src").unwrap().join("lib.rs")).unwrap();
        // lib.rs includes this very test (and thus those strings we're searching for) so we need to strip that part
        let lib_rs = &lib_rs_with_tests[..lib_rs_with_tests.find("#[cfg(test)]").unwrap()];
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
            ExternGeneric::new(Foo::new(u64::MAX, String::from("asdfghjkl"), vec![0])),
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
                                    let _ = ArrayOptFields::from_cbor_bytes(&bytes).unwrap();
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
}
