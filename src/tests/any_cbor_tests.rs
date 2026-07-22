//! Span-oracle property layer for the `AnyCbor` runtime type (loose-CBOR Phase A1).
//!
//! The one genuinely new load-bearing artifact of the loose-CBOR feature is the hand-written
//! recursive `AnyCbor` codec whose contract is "byte-identical over ALL well-formed CBOR"
//! (preserve variant) / "value-identical" (non-preserve variant). This module compiles the
//! `static/any_cbor_*.rs` files INTO the bin-crate test binary — the same `include!`-a-static-file
//! technique `integration_tests.rs` already uses for `emit_tests_encoding_fidelity.rs` — inside
//! shim modules that reproduce the context a generated crate provides (the `Deserialize` trait,
//! error types, the serialization fragments the file depends on), one shim per `--preserve-encodings`
//! assembly. `src/tests/` is bin-crate-only, so nothing is added to `lib.rs` (the bin/lib module
//! gate is untouched).
//!
//! The oracle: for every corpus item, deserialize one `AnyCbor` from a `Deserializer`, assert the
//! read cursor lands exactly at the item's end (the cip36 over-read bug class), then re-serialize
//! and assert byte-identity (preserve) / value-identity (non-preserve). Malformed inputs must
//! return `Err`, never panic. A seeded structural generator (no in-tree property-test crate — see
//! the A1 report's PROBE-6) drives 10k+ random well-formed items through the same oracle.
//!
//! Depth-guard participation is deferred to A2 (a reported PROBE-4 conflict: the guard runtime is
//! only present under `--deserialize-depth-limit` and its limit is a generation-time literal, so an
//! unconditionally-present static file cannot reference it without generator support). The depth
//! corpus therefore exercises the recursion at a stack-safe depth for round-trip and a truncated
//! deep item for the error path; the guard-limit-exceeded case is an A2 item.

// ---------------------------------------------------------------------------------------------
// Module-independent corpus + generator (operate on raw bytes / cbor_event only; no AnyCbor).
// ---------------------------------------------------------------------------------------------

fn hex_to_bytes(s: &str) -> Vec<u8> {
    let s: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    assert!(s.len().is_multiple_of(2), "odd hex length: {s}");
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).expect("valid hex"))
        .collect()
}

/// SplitMix64 — a tiny seeded PRNG. Reproducible failures: the seed is printed by the random tests.
struct Rng(u64);
impl Rng {
    fn next_u64(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
    /// uniform-ish in `0..n` (n > 0)
    fn below(&mut self, n: u64) -> u64 {
        self.next_u64() % n
    }
}

fn sz_max(sz: cbor_event::Sz) -> u64 {
    match sz {
        cbor_event::Sz::Inline => 23,
        cbor_event::Sz::One => u8::MAX as u64,
        cbor_event::Sz::Two => u16::MAX as u64,
        cbor_event::Sz::Four => u32::MAX as u64,
        cbor_event::Sz::Eight => u64::MAX,
    }
}

const SZ_LADDER: [cbor_event::Sz; 5] = [
    cbor_event::Sz::Inline,
    cbor_event::Sz::One,
    cbor_event::Sz::Two,
    cbor_event::Sz::Four,
    cbor_event::Sz::Eight,
];

/// A random `Sz` (possibly non-canonical, to exercise width fidelity) that can hold `v`.
fn random_valid_sz(rng: &mut Rng, v: u64) -> cbor_event::Sz {
    let valid: Vec<cbor_event::Sz> = SZ_LADDER
        .iter()
        .copied()
        .filter(|s| sz_max(*s) >= v)
        .collect();
    valid[rng.below(valid.len() as u64) as usize]
}

/// Emit exactly one well-formed CBOR item into `ser`, with randomly (validly) chosen widths and
/// definite/indefinite forms. `depth_left == 0` forces a leaf.
fn emit_random_item(ser: &mut cbor_event::se::Serializer, rng: &mut Rng, depth_left: u32) {
    use cbor_event::{StringLenSz, Sz};
    let n_choices = if depth_left == 0 { 6 } else { 8 };
    match rng.below(n_choices) {
        0 => {
            let v = rng.next_u64() >> rng.below(64);
            ser.write_unsigned_integer_sz(v, random_valid_sz(rng, v))
                .unwrap();
        }
        1 => {
            let mag = rng.next_u64() >> rng.below(64);
            let v = -(mag as i128) - 1;
            ser.write_negative_integer_sz(v, random_valid_sz(rng, mag))
                .unwrap();
        }
        2 => {
            let len = rng.below(8) as usize;
            let data: Vec<u8> = (0..len).map(|_| rng.next_u64() as u8).collect();
            ser.write_bytes_sz(&data, StringLenSz::Len(random_valid_sz(rng, len as u64)))
                .unwrap();
        }
        3 => {
            let len = rng.below(8) as usize;
            let s: String = (0..len)
                .map(|_| (b'a' + rng.below(26) as u8) as char)
                .collect();
            ser.write_text_sz(&s, StringLenSz::Len(random_valid_sz(rng, s.len() as u64)))
                .unwrap();
        }
        4 => {
            // f64 (any bit pattern, incl. NaN/inf/subnormal) — Sz::Eight is always exact.
            let f = f64::from_bits(rng.next_u64());
            ser.write_float_sz(f, Sz::Eight).unwrap();
        }
        5 => {
            let special = match rng.below(5) {
                0 => cbor_event::Special::Bool(false),
                1 => cbor_event::Special::Bool(true),
                2 => cbor_event::Special::Null,
                3 => cbor_event::Special::Undefined,
                _ => cbor_event::Special::Unassigned(if rng.below(2) == 0 {
                    rng.below(20) as u8 // single-byte simple value
                } else {
                    32 + rng.below(224) as u8 // two-byte form (>= 32)
                }),
            };
            ser.write_special(special).unwrap();
        }
        6 => {
            let len = rng.below(4);
            let indef = rng.below(2) == 1;
            if indef {
                ser.write_array_sz(cbor_event::LenSz::Indefinite).unwrap();
            } else {
                ser.write_array_sz(cbor_event::LenSz::Len(len, random_valid_sz(rng, len)))
                    .unwrap();
            }
            for _ in 0..len {
                emit_random_item(ser, rng, depth_left - 1);
            }
            if indef {
                ser.write_special(cbor_event::Special::Break).unwrap();
            }
        }
        _ => {
            let len = rng.below(4);
            let indef = rng.below(2) == 1;
            if indef {
                ser.write_map_sz(cbor_event::LenSz::Indefinite).unwrap();
            } else {
                ser.write_map_sz(cbor_event::LenSz::Len(len, random_valid_sz(rng, len)))
                    .unwrap();
            }
            for _ in 0..len {
                emit_random_item(ser, rng, depth_left - 1);
                emit_random_item(ser, rng, depth_left - 1);
            }
            if indef {
                ser.write_special(cbor_event::Special::Break).unwrap();
            }
        }
    }
}

fn random_corpus(seed: u64, count: usize, max_depth: u32) -> Vec<(String, Vec<u8>)> {
    let mut rng = Rng(seed);
    (0..count)
        .map(|i| {
            let mut ser = cbor_event::se::Serializer::new_vec();
            emit_random_item(&mut ser, &mut rng, max_depth);
            (format!("random#{i}"), ser.finalize())
        })
        .collect()
}

/// RFC 8949 Appendix A vectors. All but ONE are single well-formed items. The skip:
/// `f818` (`simple(24)`) is a two-byte simple-value encoding of a value < 32, which RFC 8949 §3.3
/// (Appendix C: `if (ib == 0xf8 && val < 0x20) fail`) declares ill-formed — the value 24 has no
/// well-formed encoding at all. The table retains it from RFC 7049; the cbor_event fork rejects it
/// (also covered as a rejection in `malformed_corpus`). Skipping it here keeps this a
/// well-formed-only corpus; any OTHER deserialize failure would surface (the two variant tests
/// panic on error rather than silently skipping).
fn appendix_a_corpus() -> Vec<(String, Vec<u8>)> {
    let raw = include_str!("../../cddl-matrix/sources/appendix_a.json");
    let entries: Vec<serde_json::Value> =
        serde_json::from_str(raw).expect("appendix_a.json parses");
    entries
        .iter()
        .enumerate()
        .filter_map(|(i, e)| {
            let hex = e["hex"].as_str().expect("entry has hex string");
            if hex == "f818" {
                return None; // ill-formed per RFC 8949 §3.3 — see the doc comment
            }
            Some((format!("appendix_a#{i} ({hex})"), hex_to_bytes(hex)))
        })
        .collect()
}

/// Hand-written well-formed corpus, one group per corpus class in the A1 spec (floats, integers,
/// strings/bytes, containers, tags, specials, depth).
fn well_formed_corpus() -> Vec<(String, Vec<u8>)> {
    let mut v: Vec<(&str, &str)> = vec![
        // 2. floats — every width, edge values, non-canonical wide encodings, NaN payloads
        ("f16 +0", "f90000"),
        ("f16 -0", "f98000"),
        ("f16 1.0", "f93c00"),
        ("f16 +inf", "f97c00"),
        ("f16 -inf", "f9fc00"),
        ("f16 quiet NaN", "f97e00"),
        ("f16 NaN payload", "f97e01"),
        ("f16 subnormal", "f90001"),
        ("f32 1.0", "fa3f800000"),
        ("f32 +inf", "fa7f800000"),
        ("f32 NaN payload", "fa7fc00001"),
        ("f64 1.0", "fb3ff0000000000000"),
        ("f64 -inf", "fbfff0000000000000"),
        ("f64 quiet NaN", "fb7ff8000000000000"),
        ("f64 NaN payload", "fb7ff8000000000001"),
        ("f64 non-canon holding 1.0", "fb3ff0000000000000"),
        // 3. integers — every Sz width for the same value, extremes, i65 boundary
        ("uint 0 inline", "00"),
        ("uint 0 as 1byte", "1800"),
        ("uint 0 as 2byte", "190000"),
        ("uint 0 as 4byte", "1a00000000"),
        ("uint 0 as 8byte", "1b0000000000000000"),
        ("uint 23 inline", "17"),
        ("uint 24 1byte", "1818"),
        ("u64::MAX", "1bffffffffffffffff"),
        ("nint -1", "20"),
        ("nint -1 as 1byte", "3800"),
        ("nint -2^64 (min)", "3bffffffffffffffff"),
        ("nint -(2^63) i65 boundary", "3b7fffffffffffffff"),
        // 4. strings/bytes — definite widths, indefinite mixed chunks, empty chunks, indef-empty
        ("bytes empty", "40"),
        ("bytes 4 inline-len", "4401020304"),
        ("bytes len as 1byte", "5804deadbeef"),
        ("text empty", "60"),
        ("text hello", "6568656c6c6f"),
        ("text len as 1byte", "780568656c6c6f"),
        ("bytes indefinite two chunks", "5f42010243030405ff"),
        ("bytes indefinite empty chunk", "5f40410aff"),
        ("bytes indefinite-empty", "5fff"),
        ("text indefinite-empty", "7fff"),
        ("text indefinite two chunks", "7f6161626263ff"),
        // 5. containers — definite/indefinite arrays and maps, duplicate & differently-encoded keys
        ("array empty definite", "80"),
        ("array [1,2,3]", "83010203"),
        ("array indefinite", "9f010203ff"),
        ("array indefinite empty", "9fff"),
        ("map empty", "a0"),
        ("map {1:2,3:4}", "a201020304"),
        ("map indefinite", "bf01020304ff"),
        ("map dup identical keys", "a201000100"),
        ("map dup diff-encoded keys (1 vs 1801)", "a20100180100"),
        (
            "map length-first key order divergent (24 vs 10)",
            "a21818000a02",
        ),
        ("nested uneven", "82018201820180"),
        // 6. tags — nested, width variations, wrapping each class
        ("tag 0 text", "c06a323031332d30332d3231"),
        ("tag 2 bignum bytes", "c249010000000000000000"),
        ("tag nested", "d818d81800"),
        ("tag width 2byte", "d90100820102"),
        ("tag wrapping map", "c1a10102"),
        // 7. specials — bool/null/undefined, unassigned single + two-byte
        ("false", "f4"),
        ("true", "f5"),
        ("null", "f6"),
        ("undefined", "f7"),
        ("unassigned simple 16", "f0"),
        ("unassigned simple 19", "f3"),
        ("unassigned two-byte 32", "f820"),
        ("unassigned two-byte 255", "f8ff"),
    ];
    // 8. depth — nested at a stack-safe depth (round-trip only; guard-exceeded is A2).
    let deep = depth_bytes(64);
    let out: Vec<(String, Vec<u8>)> = v
        .drain(..)
        .map(|(n, h)| (n.to_string(), hex_to_bytes(h)))
        .chain(std::iter::once((
            "deep indefinite array x64".to_string(),
            deep.0,
        )))
        .chain(std::iter::once((
            "deep definite array x64".to_string(),
            deep.1,
        )))
        .collect();
    out
}

/// `(indefinite N-deep array, definite N-deep array)`, both wrapping a single `0` leaf.
fn depth_bytes(n: usize) -> (Vec<u8>, Vec<u8>) {
    let mut indef = vec![0x9fu8; n];
    indef.push(0x00);
    indef.extend(std::iter::repeat_n(0xffu8, n));
    let mut def = vec![0x81u8; n];
    def.push(0x00);
    (indef, def)
}

/// Malformed / non-well-formed inputs: deserialize must return `Err`, never panic, never OOM.
fn malformed_corpus() -> Vec<(String, Vec<u8>)> {
    let items: Vec<(&str, &str)> = vec![
        ("dangling break alone", "ff"),
        ("break mid definite array", "8301ff02"),
        ("truncated uint 8byte", "1b00000000"),
        ("truncated array missing elem", "8301"),
        ("truncated map missing value", "a101"),
        ("truncated bytes", "4401"),
        ("reserved additional-info 28", "1c"),
        ("reserved additional-info 29", "1d"),
        ("reserved additional-info 30", "1e"),
        ("indefinite uint (0x1f) not allowed", "1f"),
        ("reserved simple 0xfc", "fc"),
        ("reserved simple 0xfd", "fd"),
        ("reserved simple 0xfe", "fe"),
        ("two-byte simple 0 < 32 (ill-formed)", "f800"),
        (
            "two-byte simple 24 < 32 (appendix f818, ill-formed)",
            "f818",
        ),
        ("nested indefinite string chunk", "5f5f4101ffff"),
        ("truncated deep definite x64 (no leaf)", ""), // filled below
    ];
    let mut out: Vec<(String, Vec<u8>)> = items
        .iter()
        .filter(|(_, h)| !h.is_empty())
        .map(|(n, h)| (n.to_string(), hex_to_bytes(h)))
        .collect();
    out.push(("truncated deep definite x64".to_string(), vec![0x81u8; 64]));
    out
}

// ---------------------------------------------------------------------------------------------
// Preserve variant (canonical assembly). Full byte-exact + canonical oracle.
// ---------------------------------------------------------------------------------------------
// The `#[allow]`s cover lints the INCLUDED static runtime files legitimately trigger when hosted
// raw in a test module without a generated crate's crate-level allows: `dead_code` (only `AnyCbor`
// is exercised; `CBORReadLen`, the `Serialize`/`Deserialize` machinery, `fit_sz`, etc. are unused
// here), `upper_case_acronyms` (`error.rs`'s `CBOR` variant), and `wrong_self_convention`
// (`LenEncoding::to_len_sz` on a `Copy` type). These are warnings — not errors — in real generated
// crates; the AnyCbor code itself is clippy-clean under `--deny clippy::all`.
#[allow(dead_code, clippy::upper_case_acronyms, clippy::wrong_self_convention)]
mod preserve {
    use cbor_event::de::Deserializer;
    // NOTE: NOT importing `cbor_event::se::Serialize` — this assembly defines the crate-local
    // `Serialize` trait (with `force_canonical`), matching what a preserve+canonical crate emits.
    use cbor_event::se::Serializer;

    include!("../../static/error.rs");
    include!("../../static/serialization.rs");
    include!("../../static/serialization_preserve.rs");
    include!("../../static/serialization_preserve_force_canonical.rs");
    include!("../../static/any_cbor_preserve.rs");
    include!("../../static/any_cbor_preserve_force_canonical.rs");

    fn parse(bytes: &[u8]) -> (AnyCbor, usize) {
        let mut raw = Deserializer::from(bytes.to_vec());
        let v = AnyCbor::deserialize(&mut raw)
            .unwrap_or_else(|e| panic!("deserialize failed on well-formed input: {e}"));
        (v, raw.position())
    }

    /// Byte-exact replay + exact span + canonical fixed point.
    fn oracle(name: &str, bytes: &[u8]) {
        let (v, end) = parse(bytes);
        assert_eq!(
            end,
            bytes.len(),
            "must consume exactly one item spanning the input: {name}"
        );
        assert_eq!(v.to_cbor_bytes(), bytes, "byte-exact replay: {name}");
        // canonical is a fixed point
        let c1 = v.to_canonical_cbor_bytes();
        let v2 = AnyCbor::from_cbor_bytes(&c1)
            .unwrap_or_else(|e| panic!("canonical output not re-readable ({name}): {e}"));
        assert_eq!(
            v2.to_canonical_cbor_bytes(),
            c1,
            "canonical output is a fixed point: {name}"
        );
    }

    #[test]
    fn appendix_a_byte_exact() {
        for (name, bytes) in super::appendix_a_corpus() {
            oracle(&name, &bytes);
        }
    }

    #[test]
    fn well_formed_byte_exact() {
        for (name, bytes) in super::well_formed_corpus() {
            oracle(&name, &bytes);
        }
    }

    #[test]
    fn random_byte_exact() {
        let seed = 0x1234_5678_9abc_def0;
        let corpus = super::random_corpus(seed, 12_000, 4);
        for (name, bytes) in &corpus {
            oracle(name, bytes);
        }
        eprintln!("preserve random: {} cases, seed {seed:#x}", corpus.len());
    }

    #[test]
    fn trailing_bytes_untouched() {
        let item = super::hex_to_bytes("8100"); // [0]
        let mut buf = item.clone();
        buf.extend_from_slice(&[0xAA, 0xBB, 0xCC]);
        let mut raw = Deserializer::from(buf);
        let v = AnyCbor::deserialize(&mut raw).unwrap();
        assert_eq!(
            raw.position(),
            item.len(),
            "cursor must stop at item end, not over-read"
        );
        assert_eq!(v.to_cbor_bytes(), item);
    }

    #[test]
    fn canonical_equal_value_differently_encoded() {
        // Same value (integer 1), three encodings. Canonical output must be identical; each must
        // byte-round-trip in the preserve variant.
        let encs = ["01", "1801", "190001", "1a00000001", "1b0000000000000001"];
        let mut canon: Option<Vec<u8>> = None;
        for h in encs {
            let bytes = super::hex_to_bytes(h);
            oracle(h, &bytes);
            let (v, _) = parse(&bytes);
            let c = v.to_canonical_cbor_bytes();
            match &canon {
                None => canon = Some(c),
                Some(prev) => assert_eq!(&c, prev, "canonical of equal values must match ({h})"),
            }
        }
        assert_eq!(
            canon.unwrap(),
            super::hex_to_bytes("01"),
            "canonical of 1 is 0x01"
        );
    }

    #[test]
    fn canonical_map_key_length_first_order() {
        // Keys 24 (0x1818, len 2) and 10 (0x0a, len 1). Length-first ordering puts 10 before 24
        // even though 0x18 > 0x0a bytewise. Input has them reversed; canonical must reorder.
        let input = super::hex_to_bytes("a21818000a02"); // {24:0, 10:2}
        let (v, _) = parse(&input);
        let canon = v.to_canonical_cbor_bytes();
        assert_eq!(
            canon,
            super::hex_to_bytes("a20a02181800"), // {10:2, 24:0}
            "length-first-then-bytewise canonical key order"
        );
    }

    #[test]
    fn representational_inequality() {
        // 0x01 vs 0x1801 — same value, different width — UNEQUAL in the preserve variant.
        let (a, _) = parse(&super::hex_to_bytes("01"));
        let (b, _) = parse(&super::hex_to_bytes("1801"));
        assert_ne!(
            a, b,
            "differently-encoded equal values are representationally unequal"
        );
        // Ord is total and consistent; Hash keys differ.
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut ha = DefaultHasher::new();
        let mut hb = DefaultHasher::new();
        a.hash(&mut ha);
        b.hash(&mut hb);
        assert_ne!(
            ha.finish(),
            hb.finish(),
            "unequal values hash differently here"
        );
        assert_ne!(a.cmp(&b), std::cmp::Ordering::Equal, "Ord separates them");
    }

    #[test]
    fn depth_roundtrip_no_crash() {
        // Recursion works at a stack-safe depth (guard-limit-exceeded is deferred to A2).
        let (indef, def) = super::depth_bytes(64);
        oracle("deep indefinite x64", &indef);
        oracle("deep definite x64", &def);
    }

    #[test]
    fn malformed_errors_never_panic() {
        for (name, bytes) in super::malformed_corpus() {
            let result = std::panic::catch_unwind(|| AnyCbor::from_cbor_bytes(&bytes));
            match result {
                Ok(Ok(_)) => panic!("malformed input accepted: {name}"),
                Ok(Err(_)) => {} // graceful error — good
                Err(_) => panic!("malformed input PANICKED (must return Err): {name}"),
            }
        }
    }

    #[test]
    fn ord_totality_over_floats() {
        // Every float width incl. NaN patterns compares total and reflexive.
        let floats = [
            "f97e00",
            "f97e01",
            "fa7fc00001",
            "fb7ff8000000000001",
            "f90000",
            "f98000",
        ];
        let vals: Vec<AnyCbor> = floats
            .iter()
            .map(|h| parse(&super::hex_to_bytes(h)).0)
            .collect();
        for a in &vals {
            assert_eq!(a.cmp(a), std::cmp::Ordering::Equal, "reflexive");
            for b in &vals {
                assert_eq!(a.cmp(b), b.cmp(a).reverse(), "antisymmetric");
            }
        }
    }

    #[test]
    fn canonical_nan_drops_payload() {
        // RFC 8949 §4.2.2: a payload-carrying NaN at ANY width canonicalizes to the zero-payload
        // quiet NaN f9 7e00 (dropping the payload) — and the canonical fixed point still holds.
        for h in [
            "f97e00",
            "f97e01",
            "fa7fc00001",
            "fb7ff8000000000001",
            "fb7ff8000000000000",
        ] {
            let (v, _) = parse(&super::hex_to_bytes(h));
            let c = v.to_canonical_cbor_bytes();
            assert_eq!(
                c,
                super::hex_to_bytes("f97e00"),
                "canonical NaN drops payload: {h}"
            );
            let v2 = AnyCbor::from_cbor_bytes(&c).unwrap();
            assert_eq!(
                v2.to_canonical_cbor_bytes(),
                c,
                "canonical NaN fixed point: {h}"
            );
        }
        // A None-encoding (Rust-constructed) NaN keeps its payload at the smallest exact width
        // (payload-preserving), distinct from the canonical (payload-dropping) path above.
        let payload = AnyCbor::Special(AnySpecial::Float(
            f64::from_bits(0x7ff8_0000_0000_0001),
            None,
        ));
        assert_eq!(
            payload.to_cbor_bytes(),
            super::hex_to_bytes("fb7ff8000000000001"),
            "None-encoding NaN preserves payload at smallest exact width"
        );
    }

    #[test]
    fn inspection_surface() {
        assert_eq!(
            parse(&super::hex_to_bytes("18ff")).0.kind(),
            AnyCborKind::UInt
        );
        assert_eq!(parse(&super::hex_to_bytes("18ff")).0.as_uint(), Some(255));
        assert_eq!(parse(&super::hex_to_bytes("20")).0.as_nint(), Some(-1));
        assert_eq!(
            parse(&super::hex_to_bytes("4401020304")).0.as_bytes(),
            Some(&[1, 2, 3, 4][..])
        );
        assert_eq!(
            parse(&super::hex_to_bytes("6568656c6c6f")).0.as_text(),
            Some("hello")
        );
        assert_eq!(
            parse(&super::hex_to_bytes("83010203"))
                .0
                .as_array()
                .map(<[_]>::len),
            Some(3)
        );
        assert_eq!(
            parse(&super::hex_to_bytes("a201020304"))
                .0
                .as_map()
                .map(<[_]>::len),
            Some(2)
        );
        let tag = parse(&super::hex_to_bytes("c00f")).0;
        let (num, inner) = tag.as_tag().unwrap();
        assert_eq!((num, inner.as_uint()), (0, Some(15)));
        assert_eq!(parse(&super::hex_to_bytes("f5")).0.as_bool(), Some(true));
        assert!(parse(&super::hex_to_bytes("f6")).0.is_null());
        assert!(parse(&super::hex_to_bytes("f7")).0.is_undefined());
        assert_eq!(
            parse(&super::hex_to_bytes("f93c00")).0.as_float(),
            Some(1.0)
        );
        assert_eq!(
            parse(&super::hex_to_bytes("f820")).0.kind(),
            AnyCborKind::Unassigned
        );
        // negative cases: wrong-kind accessors return None
        assert_eq!(parse(&super::hex_to_bytes("18ff")).0.as_text(), None);
        assert_eq!(parse(&super::hex_to_bytes("f6")).0.as_bool(), None);
    }
}

// ---------------------------------------------------------------------------------------------
// Preserve variant (NON-canonical assembly): compile-check the other trait-impl fragment and
// confirm byte-exact replay through the `cbor_event::se::Serialize` path.
// ---------------------------------------------------------------------------------------------
#[allow(dead_code, clippy::upper_case_acronyms, clippy::wrong_self_convention)]
mod preserve_non_canonical {
    use cbor_event::de::Deserializer;
    use cbor_event::se::{Serialize, Serializer};

    include!("../../static/error.rs");
    include!("../../static/serialization.rs");
    include!("../../static/serialization_preserve.rs");
    include!("../../static/serialization_preserve_non_force_canonical.rs");
    include!("../../static/serialization_non_force_canonical.rs");
    include!("../../static/any_cbor_preserve.rs");
    include!("../../static/any_cbor_preserve_non_force_canonical.rs");

    #[test]
    fn byte_exact_via_trait() {
        for (name, bytes) in super::well_formed_corpus() {
            let mut raw = Deserializer::from(bytes.to_vec());
            let v = AnyCbor::deserialize(&mut raw)
                .unwrap_or_else(|e| panic!("deserialize failed ({name}): {e}"));
            assert_eq!(raw.position(), bytes.len(), "exact span: {name}");
            // through the cbor_event::se::Serialize impl (no force_canonical arg in this assembly)
            let mut ser = Serializer::new_vec();
            v.serialize(&mut ser).unwrap();
            assert_eq!(
                ser.finalize(),
                bytes,
                "byte-exact via Serialize trait: {name}"
            );
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Non-preserve variant. Value-level fixed-point oracle.
// ---------------------------------------------------------------------------------------------
#[allow(dead_code, clippy::upper_case_acronyms, clippy::wrong_self_convention)]
mod non_preserve {
    use cbor_event::de::Deserializer;
    use cbor_event::se::Serializer;

    include!("../../static/error.rs");
    include!("../../static/serialization.rs");
    include!("../../static/serialization_non_preserve.rs");
    include!("../../static/serialization_non_force_canonical.rs");
    include!("../../static/any_cbor_non_preserve.rs");

    fn parse(bytes: &[u8]) -> (AnyCbor, usize) {
        let mut raw = Deserializer::from(bytes.to_vec());
        let v = AnyCbor::deserialize(&mut raw)
            .unwrap_or_else(|e| panic!("deserialize failed on well-formed input: {e}"));
        (v, raw.position())
    }

    /// Value fixed point: deserialize(serialize(deserialize(x))) == deserialize(x), exact span.
    fn oracle(name: &str, bytes: &[u8]) {
        let (v, end) = parse(bytes);
        assert_eq!(end, bytes.len(), "exact span: {name}");
        let re = v.to_cbor_bytes();
        let (v2, _) = parse(&re);
        assert_eq!(v, v2, "value-level fixed point: {name}");
        // serialize is idempotent at the value level
        assert_eq!(
            v2.to_cbor_bytes(),
            re,
            "canonical-ish serialize is stable: {name}"
        );
    }

    #[test]
    fn appendix_a_value_fixed_point() {
        for (name, bytes) in super::appendix_a_corpus() {
            oracle(&name, &bytes);
        }
    }

    #[test]
    fn well_formed_value_fixed_point() {
        for (name, bytes) in super::well_formed_corpus() {
            oracle(&name, &bytes);
        }
    }

    #[test]
    fn random_value_fixed_point() {
        let seed = 0x0fed_cba9_8765_4321;
        let corpus = super::random_corpus(seed, 12_000, 4);
        for (name, bytes) in &corpus {
            oracle(name, bytes);
        }
        eprintln!(
            "non-preserve random: {} cases, seed {seed:#x}",
            corpus.len()
        );
    }

    #[test]
    fn value_equality_ignores_encoding() {
        // 0x01 vs 0x1801 — same value — EQUAL in the non-preserve variant (the cross-variant
        // counterpart to preserve::representational_inequality).
        let (a, _) = parse(&super::hex_to_bytes("01"));
        let (b, _) = parse(&super::hex_to_bytes("1801"));
        assert_eq!(a, b, "value-only equality ignores wire width");
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut ha = DefaultHasher::new();
        let mut hb = DefaultHasher::new();
        a.hash(&mut ha);
        b.hash(&mut hb);
        assert_eq!(ha.finish(), hb.finish(), "equal values hash equally");
    }

    #[test]
    fn depth_roundtrip_no_crash() {
        let (indef, def) = super::depth_bytes(64);
        oracle("deep indefinite x64", &indef);
        oracle("deep definite x64", &def);
    }

    #[test]
    fn malformed_errors_never_panic() {
        for (name, bytes) in super::malformed_corpus() {
            let result = std::panic::catch_unwind(|| AnyCbor::from_cbor_bytes(&bytes));
            match result {
                Ok(Ok(_)) => panic!("malformed input accepted: {name}"),
                Ok(Err(_)) => {}
                Err(_) => panic!("malformed input PANICKED (must return Err): {name}"),
            }
        }
    }

    #[test]
    fn inspection_surface() {
        // Same surface as the preserve variant (variant parity of the inspection API).
        assert_eq!(
            parse(&super::hex_to_bytes("18ff")).0.kind(),
            AnyCborKind::UInt
        );
        assert_eq!(parse(&super::hex_to_bytes("18ff")).0.as_uint(), Some(255));
        assert_eq!(parse(&super::hex_to_bytes("20")).0.as_nint(), Some(-1));
        assert_eq!(
            parse(&super::hex_to_bytes("4401020304")).0.as_bytes(),
            Some(&[1, 2, 3, 4][..])
        );
        assert_eq!(
            parse(&super::hex_to_bytes("6568656c6c6f")).0.as_text(),
            Some("hello")
        );
        assert_eq!(
            parse(&super::hex_to_bytes("83010203"))
                .0
                .as_array()
                .map(<[_]>::len),
            Some(3)
        );
        assert_eq!(
            parse(&super::hex_to_bytes("a201020304"))
                .0
                .as_map()
                .map(<[_]>::len),
            Some(2)
        );
        let tag = parse(&super::hex_to_bytes("c00f")).0;
        let (num, inner) = tag.as_tag().unwrap();
        assert_eq!((num, inner.as_uint()), (0, Some(15)));
        assert_eq!(parse(&super::hex_to_bytes("f5")).0.as_bool(), Some(true));
        assert!(parse(&super::hex_to_bytes("f6")).0.is_null());
        assert!(parse(&super::hex_to_bytes("f7")).0.is_undefined());
        assert_eq!(
            parse(&super::hex_to_bytes("f93c00")).0.as_float(),
            Some(1.0)
        );
        assert_eq!(
            parse(&super::hex_to_bytes("f820")).0.kind(),
            AnyCborKind::Unassigned
        );
        assert_eq!(parse(&super::hex_to_bytes("18ff")).0.as_text(), None);
        assert_eq!(parse(&super::hex_to_bytes("f6")).0.as_bool(), None);
    }
}
