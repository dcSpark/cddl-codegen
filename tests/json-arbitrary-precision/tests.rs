// Rust oracle for the `serde_json/arbitrary_precision` regression. Stronger than the JS one in
// `roundtrip.mjs`, and it runs without a wasm toolchain: it proves the emitted `Serialize` impls are
// honest in the SERDE DATA MODEL generally, not merely that one particular serde-wasm-bindgen
// configuration happens to agree. `ciborium` is the probe serializer — maintained, pure Rust, no C
// deps, and (unlike serde_json itself) it has no special case for the private
// `$serde_json::private::Number` token, so a token reaching it decodes as a MAP where a number was
// meant. That difference is the entire assertion.
#[cfg(test)]
mod tests {
    use super::*;

    /// Round-trip a value through ciborium (a NON-serde_json serializer) and hand back what it saw.
    fn through_ciborium<T: serde::Serialize>(value: &T) -> ciborium::Value {
        let mut bytes = Vec::new();
        ciborium::into_writer(value, &mut bytes).unwrap();
        ciborium::from_reader(&bytes[..]).unwrap()
    }

    /// Every other test here is vacuous if the feature is off, and it arrives by cargo FEATURE
    /// UNIFICATION from a dependency (`tests/arbitrary-precision-crate`) rather than from anything
    /// this crate's own manifest says — an indirection that could silently stop working (a resolver
    /// change, someone dropping the dep, cargo's v2 resolver splitting the graph). So assert the
    /// feature is live, using the defect itself as the detector: a bare `serde_json::Value` holding a
    /// number reaches a non-serde_json serializer as a MAP exactly when `arbitrary_precision` is on.
    #[test]
    fn arbitrary_precision_is_live() {
        let probe = through_ciborium(&serde_json::Value::from(1000u64));
        assert!(
            matches!(probe, ciborium::Value::Map(_)),
            "serde_json/arbitrary_precision is NOT enabled in this build graph, so every other \
             assertion in this fixture is vacuous — check that the `arbitrary-precision-crate` \
             path dependency is still in the generated Cargo.toml (got {probe:?})"
        );
    }

    /// The half we own: an `any`-carrying member routes through `natural_any_cbor`, which builds a
    /// `serde_json::Value`. Every magnitude must reach a non-serde_json serializer as an INTEGER.
    #[test]
    fn any_member_numbers_are_numbers() {
        for n in [0u64, 1, 1000, (1u64 << 53) - 1, (1u64 << 53) + 1, u64::MAX] {
            let holder = AnyHolder::new(any_cbor::AnyCbor::new_uint(n));
            match through_ciborium(&holder) {
                ciborium::Value::Map(entries) => {
                    let (_, payload) = entries.into_iter().next().expect("one member");
                    assert!(
                        matches!(payload, ciborium::Value::Integer(_)),
                        "any member holding uint {n} reached a non-serde_json serializer as \
                         {payload:?}, not an integer"
                    );
                }
                other => panic!("AnyHolder must be a map, got {other:?}"),
            }
        }
    }

    /// Negative `any` values take the `as_i64` arm of the ladder.
    #[test]
    fn any_member_negative_numbers_are_numbers() {
        for n in [-1i64, -3, i64::MIN] {
            let holder = AnyHolder::new(any_cbor::AnyCbor::new_nint(n as i128));
            match through_ciborium(&holder) {
                ciborium::Value::Map(entries) => {
                    let (_, payload) = entries.into_iter().next().expect("one member");
                    assert!(
                        matches!(payload, ciborium::Value::Integer(_)),
                        "any member holding nint {n} reached a non-serde_json serializer as \
                         {payload:?}, not an integer"
                    );
                }
                other => panic!("AnyHolder must be a map, got {other:?}"),
            }
        }
    }

    /// A CBOR float in an `any` member becomes a `serde_json::Number` built by `Number::from_f64`,
    /// whose spelling under `arbitrary_precision` is exactly serde_json's own print of that `f64` —
    /// so it has a lossless serde image and must reach a non-serde_json serializer as a FLOAT, not a
    /// token. Without the float arm of the ladder this is the one number kind that still ships the
    /// token, which would make `to_json_value()` on any float-carrying `any` unusable.
    #[test]
    fn any_member_floats_are_floats() {
        for f in [0.0f64, 1.5, -0.25, 1e300, f64::MIN_POSITIVE] {
            let holder = AnyHolder::new(any_cbor::AnyCbor::new_float(f));
            match through_ciborium(&holder) {
                ciborium::Value::Map(entries) => {
                    let (_, payload) = entries.into_iter().next().expect("one member");
                    assert!(
                        matches!(payload, ciborium::Value::Float(_)),
                        "any member holding float {f} reached a non-serde_json serializer as \
                         {payload:?}, not a float"
                    );
                }
                other => panic!("AnyHolder must be a map, got {other:?}"),
            }
        }
    }

    /// The consumer-shaped half: a `@custom_json` newtype whose hand-written `Serialize` builds a
    /// `serde_json::Value` and routes it through the shipped helper.
    #[test]
    fn custom_json_numbers_are_numbers() {
        for n in [0u64, 1, 1000, (1u64 << 53) + 1, u64::MAX] {
            match through_ciborium(&CustomNum::new(n)) {
                ciborium::Value::Map(entries) => {
                    let (key, payload) = entries.into_iter().next().expect("one member");
                    assert_eq!(key, ciborium::Value::Text("int".into()));
                    assert!(
                        matches!(payload, ciborium::Value::Integer(_)),
                        "custom_json number {n} reached a non-serde_json serializer as \
                         {payload:?}, not an integer"
                    );
                }
                other => panic!("CustomNum must be a map, got {other:?}"),
            }
        }
    }

    /// The published helper's own ladder, driven directly over the magnitudes the `any` and
    /// `@custom_json` surfaces above cannot reach on their own (`to_natural_json`'s number model
    /// bottoms out at u64/i64), plus the spellings only `arbitrary_precision` can hold.
    #[test]
    fn helper_number_ladder() {
        use json_value_ser::serialize_json_number;

        struct Num(serde_json::Number);
        impl serde::Serialize for Num {
            fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                serialize_json_number(&self.0, s)
            }
        }
        let num = |text: &str| -> serde_json::Number {
            match serde_json::from_str::<serde_json::Value>(text).unwrap() {
                serde_json::Value::Number(n) => n,
                other => panic!("{text} is not a JSON number: {other:?}"),
            }
        };

        // In the serde integer range (u64 / i64 / u128 / i128): honest integers.
        for text in [
            "0",
            "1000",
            "-3",
            "18446744073709551615",                    // u64::MAX
            "-9223372036854775808",                    // i64::MIN
            "18446744073709551616",                    // u64::MAX + 1 -> the u128 arm
            "340282366920938463463374607431768211455", // u128::MAX
            "-170141183460469231731687303715884105728", // i128::MIN
        ] {
            let seen = through_ciborium(&Num(num(text)));
            // CBOR's major-type-0/1 integers stop at u64, so a serde `u128`/`i128` beyond that is
            // encoded by ciborium as a tag-2/tag-3 bignum. Both are NUMBERS on the wire; a token
            // would decode as a Map instead, which is the distinction under test.
            assert!(
                matches!(seen, ciborium::Value::Integer(_))
                    || matches!(&seen, ciborium::Value::Tag(tag, _) if *tag == 2 || *tag == 3),
                "{text} must reach a non-serde_json serializer as an integer, got {seen:?}"
            );
        }

        // Beyond ±2^127 the serde data model genuinely has no integer, so serde_json's own token
        // rendering is kept: `to_json()` stays lossless, which is the point of not substituting.
        let beyond = num("340282366920938463463374607431768211456"); // u128::MAX + 1
        assert_eq!(
            serde_json::to_string(&Num(beyond.clone())).unwrap(),
            "340282366920938463463374607431768211456",
            "an integer past the serde data model must still serialize LOSSLESSLY through serde_json"
        );
    }

    /// The round-trip guard: `arbitrary_precision` keeps a number's decimal spelling verbatim, and
    /// some spellings parse as an integer whose re-printed bytes differ (`1e+3`, `1.0`, a decimal
    /// carrying more precision than `f64`). Substituting the integer there would change `to_json()`,
    /// so those keep serde_json's own rendering. The reference is serde_json's OWN output for the
    /// same `Number` rather than the input text, because its parser normalizes some spellings on the
    /// way in (`-0` is stored as `0`, `1e3` as `1e+3`) — the invariant that matters is "the honest
    /// walk writes exactly what serde_json would have written", which is what makes `to_json()`
    /// byte-identical to the pre-fix output for every input.
    #[test]
    fn spellings_keep_to_json_byte_exact() {
        use json_value_ser::serialize_json_number;

        struct Num(serde_json::Number);
        impl serde::Serialize for Num {
            fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                serialize_json_number(&self.0, s)
            }
        }
        for text in [
            "-0",
            "1e3",
            "1.0",
            "0.1234567890123456789",
            "0",
            "-3",
            "18446744073709551615",
            "340282366920938463463374607431768211456",
        ] {
            let n = match serde_json::from_str::<serde_json::Value>(text).unwrap() {
                serde_json::Value::Number(n) => n,
                other => panic!("{text} is not a JSON number: {other:?}"),
            };
            assert_eq!(
                serde_json::to_string(&Num(n.clone())).unwrap(),
                serde_json::to_string(&serde_json::Value::Number(n)).unwrap(),
                "the honest walk must write exactly what serde_json's own impl writes for {text:?}"
            );
        }
    }

    /// `to_json()` itself is untouched by any of this — it is the lossless surface, and the whole
    /// design constraint is that only the *other* serializers' view changes.
    #[test]
    fn to_json_is_unchanged() {
        assert_eq!(
            serde_json::to_string(&AnyHolder::new(any_cbor::AnyCbor::new_uint(
                u64::MAX
            )))
            .unwrap(),
            r#"{"payload":18446744073709551615}"#
        );
        assert_eq!(
            serde_json::to_string(&CustomNum::new(1000)).unwrap(),
            r#"{"int":1000}"#
        );
    }

    /// Both surfaces still read back what they wrote.
    #[test]
    fn json_round_trips() {
        let holder = AnyHolder::new(any_cbor::AnyCbor::new_uint(9007199254740993));
        let json = serde_json::to_string(&holder).unwrap();
        let back: AnyHolder = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);

        let custom = CustomNum::new(u64::MAX);
        let json = serde_json::to_string(&custom).unwrap();
        let back: CustomNum = serde_json::from_str(&json).unwrap();
        assert_eq!(u64::from(back), u64::MAX);
    }
}
