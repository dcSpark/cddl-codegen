// `--emit-tests` × `--preserve-encodings` encoding-fidelity mutator.
//
// A dependency-free, deterministic CBOR re-encoder spliced into the generated `cddl_generated_tests`
// module. Its input is always a minted value's own `to_cbor_bytes()` output — well-formed,
// definite-length, minimal-header CBOR — and it derives a small set of WHOLE-TREE irregular
// re-encodings of that value (one per mutation class, not per-position combinations). The emitted
// round-trip loop feeds each variant back through `from_cbor_bytes` and asserts the preserve
// contract (byte-identical re-encode) and, under `--canonical-form`, the canonical differential.
//
// No `HashMap`, no randomness — the transforms are structural, so output is a deterministic
// function of the input bytes (reproducibility invariant). Its `std` names arrive through the
// module's own restore pair below, which is what lets a generated crate's
// `cargo test --no-default-features --lib` compile this.
//
// The seven classes (see `variants`): `widen_step`, `widen_max` (non-minimal header arguments),
// `widen_float` (widen a major-type-7 float head to the next IEEE width — f16→f32, f32→f64; an f64
// head is already maximal, so it is left alone), `indef_containers` (indefinite array/map framing),
// `chunk_strings` (indefinite two-chunk strings), `reverse_maps` (reversed map entry order), and
// `everything` (structure transforms first, then `widen_step` AND `widen_float` over every head of
// the resulting tree — chunk headers and heads inside indefinite containers included, break bytes
// excluded). Non-float major-type-7 heads (bool/null/undefined/simple) each carry a single wire
// form, so they are copied verbatim.
//
// Float heads reach this mutator through the emit-tests mint path for `any` (`AnyCbor`): an
// `AnyCbor`-typed (`any`) member mints a composite value carrying a width-preserving f16/f32/f64
// head (cbor_event fork `float_sz` API, exercised by `any_cbor_tests`), which the preserve
// serializer writes at the value-preserving smallest width. `widen_float` re-encodes that head one
// IEEE width wider — always value-preserving, since f32 exactly represents every f16 and f64 every
// f32 — and under preserve, `from_cbor_bytes` records the wider width and re-encodes it
// byte-identically, so the round-trip loop asserts the widened head survives. Native-float members
// remain unreachable here (they still panic generation under preserve,
// `preserve_encodings_supports_floats` stub), so every float head this class exercises arrives
// through `AnyCbor`.
//
// `bytes .cbor T` wrappers are treated as opaque byte strings — the outer string is mutated, the
// inner CBOR is left untouched (mutating the inner encoding is a deliberate out-of-scope extension).
//
// Self-check: `encoding_mutator_self_check` pins each builder (including `widen_float` across
// f16→f32→f64 and the f64/non-float verbatim cases) against hand-derived RFC 8949 byte fixtures AND
// pins `variants()` end-to-end on two inputs — a composite (int + string + 2-entry map) and a
// float-carrying `[5, 1.5]` (the shape the `any` mint produces) — the vacuity guard: a `variants()`
// that returned empty/all-skipped would turn every emitted loop green while executing nothing, and
// no source-grep floor could see that. Byte fixtures follow the golden-hex `0x??`-literal
// authoring convention.
#[allow(dead_code)]
#[allow(clippy::all)]
mod cddl_encoding_fidelity {
    // This module's own `std` restore, and the one hand-written copy of the pair `emit_tests.rs`
    // emits for the enclosing `cddl_generated_tests` module (see `STD_RESTORE` there for the full
    // rationale). It is hand-carried because a non-crate-root `extern crate` does not reach a
    // NESTED inline `mod` body — the same constraint that makes the four `natural_any_cbor_*` serde
    // adapters in `static/any_cbor_json*.rs` hand-carry their `use super::alloc::…`. Without it,
    // this module's `Vec`/`vec!`/`Box` are unresolved in a `--no-default-features` build of a
    // generated crate, whose root is `#![cfg_attr(not(feature = "std"), no_std)]`.
    //
    // No disambiguating `use std::panic;` here: nothing in this module uses `panic!` (its
    // assertions are core-prelude `assert*!`, whose std and core spellings are the same item).
    extern crate std;
    use std::prelude::rust_2024::*;

    /// A parsed CBOR item. `Str` holds the payload bytes verbatim (text is valid UTF-8, but the
    /// splitter only needs the bytes); `Other` holds a major-7 head verbatim (no children).
    enum Item {
        Int { major: u8, arg: u64 },
        Str { major: u8, data: Vec<u8> },
        Array(Vec<Item>),
        Map(Vec<(Item, Item)>),
        Tag { tag: u64, inner: Box<Item> },
        Other(Vec<u8>),
    }

    /// Read the argument of the head at `pos`, returning `(value, head_len_in_bytes)`. Input is
    /// definite minimal CBOR, so `info` is always 0..=27.
    fn read_arg(b: &[u8], pos: usize) -> (u64, usize) {
        let info = b[pos] & 0x1f;
        if info < 24 {
            (info as u64, 1)
        } else {
            let n = match info {
                24 => 1,
                25 => 2,
                26 => 4,
                27 => 8,
                _ => 0,
            };
            let mut v = 0u64;
            for i in 0..n {
                v = (v << 8) | b[pos + 1 + i] as u64;
            }
            (v, 1 + n)
        }
    }

    /// Parse one item starting at `pos`, returning it and the position just past it. The input is a
    /// single well-formed definite-length CBOR value, so no indefinite framing is encountered here.
    fn parse_item(b: &[u8], pos: usize) -> (Item, usize) {
        let head = b[pos];
        let major = head >> 5;
        let (arg, hlen) = read_arg(b, pos);
        match major {
            0 | 1 => (Item::Int { major, arg }, pos + hlen),
            2 | 3 => {
                let start = pos + hlen;
                let end = start + arg as usize;
                (
                    Item::Str {
                        major,
                        data: b[start..end].to_vec(),
                    },
                    end,
                )
            }
            4 => {
                let mut items = Vec::new();
                let mut p = pos + hlen;
                for _ in 0..arg {
                    let (it, np) = parse_item(b, p);
                    items.push(it);
                    p = np;
                }
                (Item::Array(items), p)
            }
            5 => {
                let mut pairs = Vec::new();
                let mut p = pos + hlen;
                for _ in 0..arg {
                    let (k, np) = parse_item(b, p);
                    let (v, np2) = parse_item(b, np);
                    pairs.push((k, v));
                    p = np2;
                }
                (Item::Map(pairs), p)
            }
            6 => {
                let (inner, np) = parse_item(b, pos + hlen);
                (
                    Item::Tag {
                        tag: arg,
                        inner: Box::new(inner),
                    },
                    np,
                )
            }
            // major 7: bool/null/simple/float head, verbatim (no argument to mutate, no children)
            _ => (Item::Other(b[pos..pos + hlen].to_vec()), pos + hlen),
        }
    }

    #[derive(Clone, Copy, PartialEq)]
    enum Widen {
        None,
        Step,
        Max,
    }

    /// The `Sz` level a minimal encoding of `arg` uses: 0=inline, 1..=4 = 1/2/4/8-byte.
    fn minimal_level(arg: u64) -> u8 {
        if arg <= 23 {
            0
        } else if arg <= u8::MAX as u64 {
            1
        } else if arg <= u16::MAX as u64 {
            2
        } else if arg <= u32::MAX as u64 {
            3
        } else {
            4
        }
    }

    fn target_level(arg: u64, w: Widen) -> u8 {
        let m = minimal_level(arg);
        match w {
            Widen::None => m,
            Widen::Step => (m + 1).min(4),
            Widen::Max => 4,
        }
    }

    /// Emit a head for `major` carrying `arg`, at the width `w` selects (minimal, one step wider, or
    /// straight to 8-byte; a head already at 8 bytes is left alone). Level 0 (inline) is only ever
    /// selected for `Widen::None` on `arg <= 23`, so `arg as u8` there is always in range.
    fn emit_head(out: &mut Vec<u8>, major: u8, arg: u64, w: Widen) {
        let mm = major << 5;
        match target_level(arg, w) {
            0 => out.push(mm | (arg as u8)),
            1 => {
                out.push(mm | 24);
                out.push(arg as u8);
            }
            2 => {
                out.push(mm | 25);
                out.extend_from_slice(&(arg as u16).to_be_bytes());
            }
            3 => {
                out.push(mm | 26);
                out.extend_from_slice(&(arg as u32).to_be_bytes());
            }
            _ => {
                out.push(mm | 27);
                out.extend_from_slice(&arg.to_be_bytes());
            }
        }
    }

    #[derive(Clone, Copy)]
    struct Cfg {
        widen: Widen,
        /// widen a major-type-7 float head to the next IEEE width (f16→f32, f32→f64; an f64 head is
        /// already maximal). Non-float major-7 heads are untouched.
        widen_float: bool,
        indef: bool,
        chunk: bool,
        /// chunk strings too short to midpoint-split into two non-degenerate chunks anyway, via an
        /// empty first chunk (`(_ "", "x")`). On only for `everything`.
        chunk_fallback: bool,
        reverse: bool,
    }

    const OFF: Cfg = Cfg {
        widen: Widen::None,
        widen_float: false,
        indef: false,
        chunk: false,
        chunk_fallback: false,
        reverse: false,
    };

    /// The midpoint split index for a string chunk, backed off DOWN to a UTF-8 char boundary for
    /// text (a boundary byte is any non-continuation byte, `b & 0xc0 != 0x80`). Bytes split at the
    /// raw midpoint. Minted strings are ASCII, so the backoff is a no-op there, but the mutator must
    /// not assume it.
    fn split_point(data: &[u8], is_text: bool) -> usize {
        let mut sp = data.len() / 2;
        if is_text {
            while sp > 0 && (data[sp] & 0xc0) == 0x80 {
                sp -= 1;
            }
        }
        sp
    }

    fn emit_item(out: &mut Vec<u8>, item: &Item, cfg: &Cfg) {
        match item {
            Item::Int { major, arg } => emit_head(out, *major, *arg, cfg.widen),
            Item::Str { major, data } => {
                let do_chunk =
                    cfg.chunk && (data.len() >= 2 || (cfg.chunk_fallback && data.len() >= 1));
                if do_chunk {
                    // indefinite string marker (major | 0x1f), two definite chunks, break
                    out.push((major << 5) | 0x1f);
                    let sp = if data.len() >= 2 {
                        split_point(data, *major == 3)
                    } else {
                        0 // fallback: empty first chunk
                    };
                    for chunk in [&data[..sp], &data[sp..]] {
                        emit_head(out, *major, chunk.len() as u64, cfg.widen);
                        out.extend_from_slice(chunk);
                    }
                    out.push(0xff);
                } else {
                    emit_head(out, *major, data.len() as u64, cfg.widen);
                    out.extend_from_slice(data);
                }
            }
            Item::Tag { tag, inner } => {
                emit_head(out, 6, *tag, cfg.widen);
                emit_item(out, inner, cfg);
            }
            Item::Array(items) => {
                if cfg.indef {
                    out.push(0x9f);
                    for it in items {
                        emit_item(out, it, cfg);
                    }
                    out.push(0xff);
                } else {
                    emit_head(out, 4, items.len() as u64, cfg.widen);
                    for it in items {
                        emit_item(out, it, cfg);
                    }
                }
            }
            Item::Map(pairs) => {
                // canonical input has no duplicate keys, so reversal stays valid
                let ordered: Vec<&(Item, Item)> = if cfg.reverse {
                    pairs.iter().rev().collect()
                } else {
                    pairs.iter().collect()
                };
                if cfg.indef {
                    out.push(0xbf);
                    for (k, v) in &ordered {
                        emit_item(out, k, cfg);
                        emit_item(out, v, cfg);
                    }
                    out.push(0xff);
                } else {
                    emit_head(out, 5, pairs.len() as u64, cfg.widen);
                    for (k, v) in &ordered {
                        emit_item(out, k, cfg);
                        emit_item(out, v, cfg);
                    }
                }
            }
            Item::Other(raw) => {
                // A major-7 head. Only float heads (info 25/26/27) are widened, and only under the
                // `widen_float` class; every other major-7 head (bool/null/undefined/simple) and an
                // already-maximal f64 head are copied verbatim.
                match cfg.widen_float.then(|| widen_float_head(raw)).flatten() {
                    Some(widened) => out.extend_from_slice(&widened),
                    None => out.extend_from_slice(raw),
                }
            }
        }
    }

    /// Widen a major-type-7 IEEE float head one width up (f16→f32, f32→f64), returning the new head
    /// bytes. `None` for a non-float major-7 head (bool/null/undefined/simple/unassigned) or an
    /// already-maximal f64 head — the caller then copies the input bytes verbatim. Widening is
    /// value-preserving (f32 exactly represents every f16, f64 every f32), and under preserve the
    /// deserializer records the widened width and re-encodes it byte-identically.
    fn widen_float_head(raw: &[u8]) -> Option<Vec<u8>> {
        match raw[0] {
            0xf9 => {
                // f16 -> f32
                let bits = u16::from_be_bytes([raw[1], raw[2]]);
                let mut out = vec![0xfa];
                out.extend_from_slice(&f16_to_f32_bits(bits).to_be_bytes());
                Some(out)
            }
            0xfa => {
                // f32 -> f64: the widening cast is exact for every f32 (minted floats are finite).
                let bits = u32::from_be_bytes([raw[1], raw[2], raw[3], raw[4]]);
                let d = f32::from_bits(bits) as f64;
                let mut out = vec![0xfb];
                out.extend_from_slice(&d.to_bits().to_be_bytes());
                Some(out)
            }
            // 0xfb: f64 is already the widest IEEE form; every other major-7 head is non-float.
            _ => None,
        }
    }

    /// IEEE half (f16) bit pattern to the equivalent single (f32) bit pattern — pure integer math
    /// (std has no `f16`), value-preserving over zero/subnormal/normal/inf/NaN.
    fn f16_to_f32_bits(h: u16) -> u32 {
        let sign = ((h & 0x8000) as u32) << 16;
        let exp = (h >> 10) & 0x1f;
        let mant = (h & 0x3ff) as u32;
        match exp {
            0 => {
                if mant == 0 {
                    sign // +/- zero
                } else {
                    // subnormal f16 -> normal f32: normalize the mantissa, tracking the shift.
                    let mut e: i32 = -1;
                    let mut m = mant;
                    loop {
                        e += 1;
                        m <<= 1;
                        if m & 0x400 != 0 {
                            break;
                        }
                    }
                    let exp32 = ((127 - 15 - e) as u32) << 23;
                    let mant32 = (m & 0x3ff) << 13;
                    sign | exp32 | mant32
                }
            }
            0x1f => sign | (0xff << 23) | (mant << 13), // inf / NaN (NaN payload widened in place)
            _ => sign | ((exp as u32 + (127 - 15)) << 23) | (mant << 13),
        }
    }

    fn re_encode(input: &[u8], cfg: Cfg) -> Vec<u8> {
        let (item, _) = parse_item(input, 0);
        let mut out = Vec::new();
        emit_item(&mut out, &item, &cfg);
        out
    }

    fn widen_step(b: &[u8]) -> Vec<u8> {
        re_encode(
            b,
            Cfg {
                widen: Widen::Step,
                ..OFF
            },
        )
    }
    fn widen_max(b: &[u8]) -> Vec<u8> {
        re_encode(
            b,
            Cfg {
                widen: Widen::Max,
                ..OFF
            },
        )
    }
    fn widen_float(b: &[u8]) -> Vec<u8> {
        re_encode(
            b,
            Cfg {
                widen_float: true,
                ..OFF
            },
        )
    }
    fn indef_containers(b: &[u8]) -> Vec<u8> {
        re_encode(b, Cfg { indef: true, ..OFF })
    }
    fn chunk_strings(b: &[u8]) -> Vec<u8> {
        re_encode(b, Cfg { chunk: true, ..OFF })
    }
    fn reverse_maps(b: &[u8]) -> Vec<u8> {
        re_encode(
            b,
            Cfg {
                reverse: true,
                ..OFF
            },
        )
    }
    fn everything(b: &[u8]) -> Vec<u8> {
        re_encode(
            b,
            Cfg {
                widen: Widen::Step,
                widen_float: true,
                indef: true,
                chunk: true,
                chunk_fallback: true,
                reverse: true,
            },
        )
    }

    /// Seven whole-tree irregular re-encodings of `input` (a minted value's canonical bytes), one per
    /// mutation class. Variants byte-identical to the input (nothing mutatable for that class) are
    /// skipped, so the emitted loop never asserts vacuously on a no-op.
    ///
    /// The input is parsed ONCE and every class emits from the shared item tree (the per-class
    /// `Cfg`s mirror the `widen_step`/… builders the self-check pins byte-for-byte).
    pub fn variants(input: &[u8]) -> Vec<(&'static str, Vec<u8>)> {
        let (item, _) = parse_item(input, 0);
        let classes: [(&'static str, Cfg); 7] = [
            (
                "widen_step",
                Cfg {
                    widen: Widen::Step,
                    ..OFF
                },
            ),
            (
                "widen_max",
                Cfg {
                    widen: Widen::Max,
                    ..OFF
                },
            ),
            (
                "widen_float",
                Cfg {
                    widen_float: true,
                    ..OFF
                },
            ),
            ("indef_containers", Cfg { indef: true, ..OFF }),
            ("chunk_strings", Cfg { chunk: true, ..OFF }),
            (
                "reverse_maps",
                Cfg {
                    reverse: true,
                    ..OFF
                },
            ),
            (
                "everything",
                Cfg {
                    widen: Widen::Step,
                    widen_float: true,
                    indef: true,
                    chunk: true,
                    chunk_fallback: true,
                    reverse: true,
                },
            ),
        ];
        let mut out = Vec::new();
        for (label, cfg) in classes {
            let mut m = Vec::new();
            emit_item(&mut m, &item, &cfg);
            if m != input {
                out.push((label, m));
            }
        }
        out
    }

    #[test]
    fn encoding_mutator_self_check() {
        // ---- per-builder pins (hand-derived RFC 8949 bytes) ----
        // widen: uint 1 (0x01, inline) -> one step (0x18 0x01), or straight to 8-byte.
        assert_eq!(widen_step(&[0x01]), vec![0x18, 0x01]);
        assert_eq!(
            widen_max(&[0x01]),
            vec![0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
        );
        // a head that genuinely needs 8 bytes (value > u32::MAX = 2^32) is left alone by
        // widen_step — the step cap. (Inputs are always minimal, so an 8-byte head means an
        // 8-byte value.)
        let eight = vec![0x1b, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00];
        assert_eq!(widen_step(&eight), eight);
        // indefinite framing: [1,2] and {1:2}.
        assert_eq!(
            indef_containers(&[0x82, 0x01, 0x02]),
            vec![0x9f, 0x01, 0x02, 0xff]
        );
        assert_eq!(
            indef_containers(&[0xa1, 0x01, 0x02]),
            vec![0xbf, 0x01, 0x02, 0xff]
        );
        // two-chunk text split at the midpoint: "abcd" -> (_ "ab", "cd").
        assert_eq!(
            chunk_strings(&[0x64, 0x61, 0x62, 0x63, 0x64]),
            vec![0x7f, 0x62, 0x61, 0x62, 0x62, 0x63, 0x64, 0xff]
        );
        // reversed map entry order: {1:2, 3:4} -> {3:4, 1:2}.
        assert_eq!(
            reverse_maps(&[0xa2, 0x01, 0x02, 0x03, 0x04]),
            vec![0xa2, 0x03, 0x04, 0x01, 0x02]
        );
        // widen_float: a float head widens one IEEE width. 1.5 is exactly representable at every
        // width, so f16 0xf9 3e00 -> f32 0xfa 3fc00000 -> f64 0xfb 3ff8000000000000.
        assert_eq!(
            widen_float(&[0xf9, 0x3e, 0x00]),
            vec![0xfa, 0x3f, 0xc0, 0x00, 0x00]
        );
        assert_eq!(
            widen_float(&[0xfa, 0x3f, 0xc0, 0x00, 0x00]),
            vec![0xfb, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        );
        // an f64 head is already the widest IEEE form — left alone.
        let f64_one_and_a_half = vec![0xfb, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        assert_eq!(widen_float(&f64_one_and_a_half), f64_one_and_a_half);
        // a non-float major-7 head (true = 0xf5) is copied verbatim.
        assert_eq!(widen_float(&[0xf5]), vec![0xf5]);

        // ---- end-to-end vacuity pin: variants() on a composite (int + string + 2-entry map) ----
        // input = [1, "ab", {1:2, 3:4}]
        let input = vec![
            0x83, 0x01, 0x62, 0x61, 0x62, 0xa2, 0x01, 0x02, 0x03, 0x04,
        ];
        let vs = variants(&input);
        // every class EXCEPT widen_float is non-identity for this float-free input, in builder order
        // (widen_float touches only major-7 float heads, of which this input has none — so skipped).
        let labels: Vec<&str> = vs.iter().map(|(l, _)| *l).collect();
        assert_eq!(
            labels,
            vec![
                "widen_step",
                "widen_max",
                "indef_containers",
                "chunk_strings",
                "reverse_maps",
                "everything",
            ]
        );
        let by = |name: &str| -> Vec<u8> {
            vs.iter().find(|(l, _)| *l == name).unwrap().1.clone()
        };
        assert_eq!(
            by("widen_step"),
            vec![
                0x98, 0x03, // array(3)
                0x18, 0x01, // 1
                0x78, 0x02, 0x61, 0x62, // "ab"
                0xb8, 0x02, // map(2)
                0x18, 0x01, 0x18, 0x02, // 1: 2
                0x18, 0x03, 0x18, 0x04, // 3: 4
            ]
        );
        assert_eq!(
            by("widen_max"),
            vec![
                0x9b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, // array(3)
                0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, // 1
                0x7b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x61, 0x62, // "ab"
                0xbb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, // map(2)
                0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, // 1
                0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, // 2
                0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, // 3
                0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, // 4
            ]
        );
        assert_eq!(
            by("indef_containers"),
            vec![
                0x9f, 0x01, 0x62, 0x61, 0x62, // array(*) [1, "ab",
                0xbf, 0x01, 0x02, 0x03, 0x04, 0xff, //  {1:2, 3:4}
                0xff, // ]
            ]
        );
        assert_eq!(
            by("chunk_strings"),
            vec![
                0x83, 0x01, // [1,
                0x7f, 0x61, 0x61, 0x61, 0x62, 0xff, //  (_ "a", "b"),
                0xa2, 0x01, 0x02, 0x03, 0x04, //  {1:2, 3:4}]
            ]
        );
        assert_eq!(
            by("reverse_maps"),
            vec![
                0x83, 0x01, 0x62, 0x61, 0x62, // [1, "ab",
                0xa2, 0x03, 0x04, 0x01, 0x02, //  {3:4, 1:2}]
            ]
        );
        // everything: indef + chunk + reverse, then widen_step over every resulting head
        // (chunk headers and heads inside indefinite containers included; break bytes excluded).
        assert_eq!(
            by("everything"),
            vec![
                0x9f, // array(*)
                0x18, 0x01, //  1 (widened)
                0x7f, 0x78, 0x01, 0x61, 0x78, 0x01, 0x62, 0xff, //  (_ "a", "b") chunk heads widened
                0xbf, 0x18, 0x03, 0x18, 0x04, 0x18, 0x01, 0x18, 0x02, 0xff, //  {3:4,1:2} widened
                0xff, // ]
            ]
        );

        // ---- float-carrying vacuity pin: variants() on [5, 1.5] (an f16 float head) ----
        // input = [5, 1.5] = array(2) [ uint 5, f16 1.5 ]. This is the shape the `any` emit-tests
        // mint produces (`new_array([new_uint(5), new_float(1.5)])`), so it exercises `widen_float`
        // end-to-end. No strings and no maps, so chunk_strings / reverse_maps are identity + skipped.
        let finput = vec![0x82, 0x05, 0xf9, 0x3e, 0x00];
        let fvs = variants(&finput);
        let flabels: Vec<&str> = fvs.iter().map(|(l, _)| *l).collect();
        assert_eq!(
            flabels,
            vec![
                "widen_step",
                "widen_max",
                "widen_float",
                "indef_containers",
                "everything",
            ]
        );
        let fby = |name: &str| -> Vec<u8> {
            fvs.iter().find(|(l, _)| *l == name).unwrap().1.clone()
        };
        // widen_float touches ONLY the float head: the array head and the uint stay, f16 -> f32.
        assert_eq!(
            fby("widen_float"),
            vec![0x82, 0x05, 0xfa, 0x3f, 0xc0, 0x00, 0x00]
        );
        // everything: indefinite array + widen_step over the uint + widen_float over the float head.
        assert_eq!(
            fby("everything"),
            vec![0x9f, 0x18, 0x05, 0xfa, 0x3f, 0xc0, 0x00, 0x00, 0xff]
        );
    }
}
