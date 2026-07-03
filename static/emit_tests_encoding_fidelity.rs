// `--emit-tests` × `--preserve-encodings` encoding-fidelity mutator.
//
// A dependency-free, deterministic CBOR re-encoder spliced into the generated `cddl_generated_tests`
// module. Its input is always a minted value's own `to_cbor_bytes()` output — well-formed,
// definite-length, minimal-header CBOR — and it derives a small set of WHOLE-TREE irregular
// re-encodings of that value (one per mutation class, not per-position combinations). The emitted
// round-trip loop feeds each variant back through `from_cbor_bytes` and asserts the preserve
// contract (byte-identical re-encode) and, under `--canonical-form`, the canonical differential.
//
// Pure `std`, no `HashMap`, no randomness — the transforms are structural, so output is a
// deterministic function of the input bytes (reproducibility invariant).
//
// The six classes (see `variants`): `widen_step`, `widen_max` (non-minimal header arguments),
// `indef_containers` (indefinite array/map framing), `chunk_strings` (indefinite two-chunk
// strings), `reverse_maps` (reversed map entry order), and `everything` (structure transforms
// first, then `widen_step` over every head of the resulting tree — chunk headers and heads inside
// indefinite containers included, break bytes excluded). Major-type-7 heads (bool/null/simple;
// floats can't appear under preserve) are copied verbatim: they carry a single wire form.
//
// `bytes .cbor T` wrappers are treated as opaque byte strings — the outer string is mutated, the
// inner CBOR is left untouched (mutating the inner encoding is a deliberate out-of-scope extension).
//
// Self-check: `encoding_mutator_self_check` pins each builder against hand-derived RFC 8949 byte
// fixtures AND pins `variants()` end-to-end on a composite input (int + string + 2-entry map) — the
// latter is the vacuity guard: a `variants()` that returned empty/all-skipped would turn every
// emitted loop green while executing nothing, and no source-grep floor could see that. Byte
// fixtures follow the golden-hex `0x??`-literal authoring convention.
#[allow(dead_code)]
#[allow(clippy::all)]
mod cddl_encoding_fidelity {
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
        indef: bool,
        chunk: bool,
        /// chunk strings too short to midpoint-split into two non-degenerate chunks anyway, via an
        /// empty first chunk (`(_ "", "x")`). On only for `everything`.
        chunk_fallback: bool,
        reverse: bool,
    }

    const OFF: Cfg = Cfg {
        widen: Widen::None,
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
            Item::Other(raw) => out.extend_from_slice(raw),
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
                indef: true,
                chunk: true,
                chunk_fallback: true,
                reverse: true,
            },
        )
    }

    /// Six whole-tree irregular re-encodings of `input` (a minted value's canonical bytes), one per
    /// mutation class. Variants byte-identical to the input (nothing mutatable for that class) are
    /// skipped, so the emitted loop never asserts vacuously on a no-op.
    ///
    /// The input is parsed ONCE and every class emits from the shared item tree (the per-class
    /// `Cfg`s mirror the `widen_step`/… builders the self-check pins byte-for-byte).
    pub fn variants(input: &[u8]) -> Vec<(&'static str, Vec<u8>)> {
        let (item, _) = parse_item(input, 0);
        let classes: [(&'static str, Cfg); 6] = [
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

    /// `variants` with the named mutation classes filtered out. The generated loop uses this when
    /// the type reaches a variable-length container of major-type-7 elements/keys (bool/null/float),
    /// whose indefinite re-encode the generated break-check can't round-trip — the generator passes
    /// the affected class labels so this mutator stays type-blind.
    pub fn variants_filtered(input: &[u8], exclude: &[&str]) -> Vec<(&'static str, Vec<u8>)> {
        variants(input)
            .into_iter()
            .filter(|(label, _)| !exclude.contains(label))
            .collect()
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

        // ---- end-to-end vacuity pin: variants() on a composite (int + string + 2-entry map) ----
        // input = [1, "ab", {1:2, 3:4}]
        let input = vec![
            0x83, 0x01, 0x62, 0x61, 0x62, 0xa2, 0x01, 0x02, 0x03, 0x04,
        ];
        let vs = variants(&input);
        // all six classes are non-identity for this input, in builder order.
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
    }
}
