// `AnyCbor` — a structured, self-describing CBOR value (the runtime lowering of CDDL `any`).
//
// PRESERVE variant (this file is concatenated only when `--preserve-encodings` is on). Every
// value carries the encoding detail needed to re-emit the ORIGINAL bytes exactly: integer/tag
// argument widths (`Sz`), string chunking (`StringEncoding`), and array/map length encoding
// (`LenEncoding`). Contract:
//   * `serialize` with `force_canonical = false` reproduces the deserialized bytes BYTE-EXACTLY;
//   * `serialize` with `force_canonical = true` produces RFC 8949 §4.2 deterministic encoding
//     (smallest widths, recursively length-first-then-bytewise map key ordering);
//   * `deserialize` accepts any single well-formed CBOR item and leaves the read cursor exactly
//     at the item's end (never over-reads trailing bytes — the cip36 skip-bug class).
//
// Equality/ordering/hashing are REPRESENTATIONAL: two values with equal CBOR content but
// different encoding (`0x01` vs `0x1801`, both the integer 1) compare UNEQUAL here, because a
// byte-preserving map key must not silently collide. This matches the in-tree precedent of
// `@used_as_key` preserve structs, whose derived `Eq`/`Hash` include their `encodings` field.
// (The non-preserve variant, `any_cbor_non_preserve.rs`, compares VALUE only.)
//
// Depth: `deserialize` recurses for nested arrays/maps/tags. It routes ALL recursion through the
// single `read` seam below, whose first line invokes `any_cbor_recursion_guard!()`. The includer
// supplies that macro: the static assembly in generated crates expands it to
// `DepthGuard::acquire(<baked limit>)?` under `--deserialize-depth-limit` (and to nothing without
// the flag, so no-flag crates carry no dead runtime code and keep byte-identical output); the
// property-harness shims supply their own definition to exercise the guard. The guard shares the
// generated composite deserializers' thread-local depth counter, so the whole nesting — struct
// and its `any` members alike — is bounded uniformly. What IS guaranteed here without the flag
// matches the rest of the crate: bounded allocation (never `Vec::with_capacity` from an
// attacker-claimed length), a dangling `Break` rejected as a value, and truncated/malformed input
// returning `Err`, never a panic.
#[derive(Clone, Debug)]
pub enum AnyCbor {
    UInt(u64, Option<cbor_event::Sz>),
    NInt(i128, Option<cbor_event::Sz>), // full CBOR nint domain (-2^64..=-1), from negative_integer_sz
    Bytes(Vec<u8>, StringEncoding),
    Text(String, StringEncoding),
    Array(Vec<AnyCbor>, LenEncoding),
    Map(Vec<(AnyCbor, AnyCbor)>, LenEncoding), // wire order AND duplicate keys preserved
    Tag(u64, Box<AnyCbor>, Option<cbor_event::Sz>),
    Special(AnySpecial),
}

#[derive(Clone, Debug)]
pub enum AnySpecial {
    Bool(bool),
    Null,
    Undefined,
    /// unassigned simple value: 0..=19 (single-byte) or 32..=255 (two-byte `0xf8` form); each
    /// has exactly one well-formed encoding, so no width slot is needed for byte-exactness.
    Unassigned(u8),
    /// `Sz::Two`/`Four`/`Eight` = the wire width (byte-exact replay via `write_float_sz`);
    /// `None` = emit the canonical smallest-width form (a value constructed in Rust).
    Float(f64, Option<cbor_event::Sz>),
}

/// Lightweight discriminant for `AnyCbor::kind()`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AnyCborKind {
    UInt,
    NInt,
    Bytes,
    Text,
    Array,
    Map,
    Tag,
    Bool,
    Null,
    Undefined,
    Unassigned,
    Float,
}

impl AnyCbor {
    /// The CBOR major-type/special discriminant of this value.
    pub fn kind(&self) -> AnyCborKind {
        match self {
            AnyCbor::UInt(..) => AnyCborKind::UInt,
            AnyCbor::NInt(..) => AnyCborKind::NInt,
            AnyCbor::Bytes(..) => AnyCborKind::Bytes,
            AnyCbor::Text(..) => AnyCborKind::Text,
            AnyCbor::Array(..) => AnyCborKind::Array,
            AnyCbor::Map(..) => AnyCborKind::Map,
            AnyCbor::Tag(..) => AnyCborKind::Tag,
            AnyCbor::Special(AnySpecial::Bool(_)) => AnyCborKind::Bool,
            AnyCbor::Special(AnySpecial::Null) => AnyCborKind::Null,
            AnyCbor::Special(AnySpecial::Undefined) => AnyCborKind::Undefined,
            AnyCbor::Special(AnySpecial::Unassigned(_)) => AnyCborKind::Unassigned,
            AnyCbor::Special(AnySpecial::Float(..)) => AnyCborKind::Float,
        }
    }

    pub fn as_uint(&self) -> Option<u64> {
        match self {
            AnyCbor::UInt(v, _) => Some(*v),
            _ => None,
        }
    }

    pub fn as_nint(&self) -> Option<i128> {
        match self {
            AnyCbor::NInt(v, _) => Some(*v),
            _ => None,
        }
    }

    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            AnyCbor::Bytes(v, _) => Some(v),
            _ => None,
        }
    }

    pub fn as_text(&self) -> Option<&str> {
        match self {
            AnyCbor::Text(v, _) => Some(v),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&[AnyCbor]> {
        match self {
            AnyCbor::Array(v, _) => Some(v),
            _ => None,
        }
    }

    pub fn as_map(&self) -> Option<&[(AnyCbor, AnyCbor)]> {
        match self {
            AnyCbor::Map(v, _) => Some(v),
            _ => None,
        }
    }

    pub fn as_tag(&self) -> Option<(u64, &AnyCbor)> {
        match self {
            AnyCbor::Tag(t, inner, _) => Some((*t, inner)),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            AnyCbor::Special(AnySpecial::Float(f, _)) => Some(*f),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            AnyCbor::Special(AnySpecial::Bool(b)) => Some(*b),
            _ => None,
        }
    }

    /// The simple-value code of an `unassigned` special (0..=19 or 32..=255), else `None`. Needed by
    /// the JSON surface's reverse mapping (there is no other way to read the code back out).
    pub fn as_unassigned(&self) -> Option<u8> {
        match self {
            AnyCbor::Special(AnySpecial::Unassigned(v)) => Some(*v),
            _ => None,
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, AnyCbor::Special(AnySpecial::Null))
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self, AnyCbor::Special(AnySpecial::Undefined))
    }

    // --- constructors (mode-paired with the non-preserve variant; SAME names/signatures). Each fills
    // the DEFAULT encoding (`None`/`Canonical`), i.e. the encoding a Rust-constructed value carries:
    // serialize (non-canonical) then emits the canonical smallest-width form, exactly like a value the
    // generated code built itself. These are the value-building surface the JSON deserialize path, the
    // wasm wrapper, and the emit-tests mint all construct through. ---

    pub fn new_uint(value: u64) -> Self {
        AnyCbor::UInt(value, None)
    }

    /// `value` must lie in the CBOR nint domain `-2^64..=-1`; out-of-domain is a debug-assert (no
    /// clamping — the caller is responsible, mirroring the crate-wide "no silent coercion" stance).
    pub fn new_nint(value: i128) -> Self {
        debug_assert!(
            (-(1i128 << 64)..=-1).contains(&value),
            "AnyCbor::new_nint: {value} outside the CBOR nint domain -2^64..=-1"
        );
        AnyCbor::NInt(value, None)
    }

    pub fn new_bytes(bytes: Vec<u8>) -> Self {
        AnyCbor::Bytes(bytes, StringEncoding::Canonical)
    }

    pub fn new_text(text: String) -> Self {
        AnyCbor::Text(text, StringEncoding::Canonical)
    }

    pub fn new_array(elems: Vec<AnyCbor>) -> Self {
        AnyCbor::Array(elems, LenEncoding::Canonical)
    }

    pub fn new_map(pairs: Vec<(AnyCbor, AnyCbor)>) -> Self {
        AnyCbor::Map(pairs, LenEncoding::Canonical)
    }

    pub fn new_tag(tag: u64, inner: AnyCbor) -> Self {
        AnyCbor::Tag(tag, Box::new(inner), None)
    }

    pub fn new_bool(b: bool) -> Self {
        AnyCbor::Special(AnySpecial::Bool(b))
    }

    pub fn new_null() -> Self {
        AnyCbor::Special(AnySpecial::Null)
    }

    pub fn new_undefined() -> Self {
        AnyCbor::Special(AnySpecial::Undefined)
    }

    pub fn new_unassigned(code: u8) -> Self {
        AnyCbor::Special(AnySpecial::Unassigned(code))
    }

    /// Canonical-width float: stores `None` per the `Float` slot's documented contract (emit the
    /// canonical smallest-width form for a Rust-constructed value).
    pub fn new_float(f: f64) -> Self {
        AnyCbor::Special(AnySpecial::Float(f, None))
    }

    /// Byte-exact re-encoding (replays stored encodings). Never fails for a value produced by
    /// `deserialize`; the `Result` is for symmetry with the serializer.
    pub fn to_cbor_bytes(&self) -> Vec<u8> {
        let mut buf = cbor_event::se::Serializer::new_vec();
        self.serialize_ref(&mut buf, false).unwrap();
        buf.finalize()
    }

    /// RFC 8949 §4.2 deterministic re-encoding (smallest widths, recursively sorted map keys).
    pub fn to_canonical_cbor_bytes(&self) -> Vec<u8> {
        let mut buf = cbor_event::se::Serializer::new_vec();
        self.serialize_ref(&mut buf, true).unwrap();
        buf.finalize()
    }

    /// Value-level (encoding-independent) equality — the duplicate-key comparison an `any`-domain
    /// open struct-map rest row / table needs under `--preserve-encodings`, where the derived `Eq`
    /// is REPRESENTATIONAL (encoding fields participate, so two equal values with different wire
    /// widths compare `!=`). Two values are value-equal iff their CANONICAL encodings match:
    /// canonicalization is the encoding-independent normal form (RFC 7049 §3.9 minimal widths,
    /// recursively normalized map key order), so `0x01` and `0x1801` (both uint 1) are value-equal
    /// though representationally distinct. Accept/reject of a duplicate wire key must
    /// be a function of the wire VALUE, not of the key domain's spelling (`* uint => any` and
    /// `* any => any` reject the same duplicates), so the default-reject dup check compares this,
    /// separately from the container's representational keying.
    pub fn value_eq(&self, other: &Self) -> bool {
        self.to_canonical_cbor_bytes() == other.to_canonical_cbor_bytes()
    }

    /// The (de)serialization workhorse. The mode-appropriate trait impls
    /// (`any_cbor_preserve_non_force_canonical.rs` / `any_cbor_preserve_force_canonical.rs`)
    /// delegate here. Self-contained: computes every `Sz`/`LenSz`/`StringLenSz` from the stored
    /// encoding fields, so it does not depend on the `fit_sz`/`to_len_sz` fragment helpers whose
    /// arity differs between the two preserve assemblies.
    pub fn serialize_ref<'a>(
        &self,
        serializer: &'a mut cbor_event::se::Serializer,
        force_canonical: bool,
    ) -> cbor_event::Result<&'a mut cbor_event::se::Serializer> {
        match self {
            AnyCbor::UInt(v, sz) => {
                serializer.write_unsigned_integer_sz(*v, fit_int_sz(*v, sz, force_canonical))?;
            }
            AnyCbor::NInt(v, sz) => {
                // encoded argument = -(v) - 1 in the range 0..=2^64-1
                let arg = (-((*v) + 1)) as u64;
                serializer.write_negative_integer_sz(*v, fit_int_sz(arg, sz, force_canonical))?;
            }
            AnyCbor::Bytes(bytes, enc) => {
                serializer.write_bytes_sz(
                    bytes,
                    str_len_sz(enc, bytes.len() as u64, force_canonical),
                )?;
            }
            AnyCbor::Text(text, enc) => {
                serializer.write_text_sz(
                    text,
                    str_len_sz(enc, text.len() as u64, force_canonical),
                )?;
            }
            AnyCbor::Array(elems, enc) => {
                serializer.write_array_sz(len_sz(enc, elems.len() as u64, force_canonical))?;
                for elem in elems.iter() {
                    elem.serialize_ref(serializer, force_canonical)?;
                }
                len_end(serializer, enc, force_canonical)?;
            }
            AnyCbor::Map(pairs, enc) => {
                serializer.write_map_sz(len_sz(enc, pairs.len() as u64, force_canonical))?;
                if force_canonical {
                    // Sort by canonically-encoded key, length-first-then-bytewise. STABLE sort so
                    // equal-keyed (duplicate) entries keep first-appearance order.
                    let mut ordered: Vec<(Vec<u8>, &AnyCbor)> = pairs
                        .iter()
                        .map(|(k, v)| {
                            let mut key_buf = cbor_event::se::Serializer::new_vec();
                            k.serialize_ref(&mut key_buf, true)?;
                            Ok((key_buf.finalize(), v))
                        })
                        .collect::<cbor_event::Result<Vec<_>>>()?;
                    ordered.sort_by(|(lhs, _), (rhs, _)| cbor_canonical_key_cmp(lhs, rhs));
                    for (key_bytes, value) in ordered.iter() {
                        serializer.write_raw_bytes(key_bytes)?;
                        value.serialize_ref(serializer, true)?;
                    }
                } else {
                    for (key, value) in pairs.iter() {
                        key.serialize_ref(serializer, false)?;
                        value.serialize_ref(serializer, false)?;
                    }
                }
                len_end(serializer, enc, force_canonical)?;
            }
            AnyCbor::Tag(tag, inner, sz) => {
                serializer.write_tag_sz(*tag, fit_int_sz(*tag, sz, force_canonical))?;
                inner.serialize_ref(serializer, force_canonical)?;
            }
            AnyCbor::Special(special) => {
                serialize_special(serializer, special, force_canonical)?;
            }
        }
        Ok(serializer)
    }

    /// Recursion seam for deserialize. All nested reads go through here so a depth guard can be
    /// threaded at one place (see the file header).
    fn read(raw: &mut cbor_event::de::Deserializer) -> Result<Self, DeserializeError> {
        // Depth-guard hook: expands to a `DepthGuard::acquire(<baked limit>)?` RAII binding when the
        // generated crate is built with `--deserialize-depth-limit`, or to nothing otherwise. The
        // macro is supplied by the includer (the static assembly for generated crates; the test
        // shims for the property harness) so this one file serves every flag combination unsplit.
        any_cbor_recursion_guard!();
        match raw.cbor_type()? {
            cbor_event::Type::UnsignedInteger => {
                let (v, sz) = raw.unsigned_integer_sz()?;
                Ok(AnyCbor::UInt(v, Some(sz)))
            }
            cbor_event::Type::NegativeInteger => {
                let (v, sz) = raw.negative_integer_sz()?;
                Ok(AnyCbor::NInt(v, Some(sz)))
            }
            cbor_event::Type::Bytes => {
                let (bytes, len_sz) = raw.bytes_sz()?;
                Ok(AnyCbor::Bytes(bytes, len_sz.into()))
            }
            cbor_event::Type::Text => {
                let (text, len_sz) = raw.text_sz()?;
                Ok(AnyCbor::Text(text, len_sz.into()))
            }
            cbor_event::Type::Array => {
                let len = raw.array_sz()?;
                let mut elems = Vec::new(); // never with_capacity(claimed len)
                read_sequence(raw, len, |raw| {
                    elems.push(AnyCbor::read(raw)?);
                    Ok(())
                })?;
                Ok(AnyCbor::Array(elems, len.into()))
            }
            cbor_event::Type::Map => {
                let len = raw.map_sz()?;
                let mut pairs = Vec::new();
                read_sequence(raw, len, |raw| {
                    let key = AnyCbor::read(raw)?;
                    let value = AnyCbor::read(raw)?;
                    pairs.push((key, value));
                    Ok(())
                })?;
                Ok(AnyCbor::Map(pairs, len.into()))
            }
            cbor_event::Type::Tag => {
                let (tag, sz) = raw.tag_sz()?;
                let inner = AnyCbor::read(raw)?;
                Ok(AnyCbor::Tag(tag, Box::new(inner), Some(sz)))
            }
            cbor_event::Type::Special => {
                // Distinguish float (0x19/0x1a/0x1b) from other specials by the header low bits;
                // reject a dangling Break (0x1f) used as a value.
                let head = *raw
                    .as_slice()
                    .first()
                    .ok_or_else(|| DeserializeFailure::CBOR(cbor_event::Error::NotEnough(0, 1)))?;
                match head & 0b0001_1111 {
                    0x19..=0x1b => {
                        let (f, sz) = raw.float_sz()?;
                        Ok(AnyCbor::Special(AnySpecial::Float(f, Some(sz))))
                    }
                    0x1f => Err(DeserializeFailure::CBOR(cbor_event::Error::UnexpectedBreak).into()),
                    _ => match raw.special()? {
                        cbor_event::Special::Bool(b) => Ok(AnyCbor::Special(AnySpecial::Bool(b))),
                        cbor_event::Special::Null => Ok(AnyCbor::Special(AnySpecial::Null)),
                        cbor_event::Special::Undefined => {
                            Ok(AnyCbor::Special(AnySpecial::Undefined))
                        }
                        cbor_event::Special::Unassigned(v) => {
                            Ok(AnyCbor::Special(AnySpecial::Unassigned(v)))
                        }
                        // floats were routed above; Break was rejected above
                        cbor_event::Special::Float(_) | cbor_event::Special::Break => {
                            Err(DeserializeFailure::CBOR(cbor_event::Error::UnexpectedBreak).into())
                        }
                    },
                }
            }
        }
    }
}

impl Deserialize for AnyCbor {
    fn deserialize(raw: &mut cbor_event::de::Deserializer) -> Result<Self, DeserializeError> {
        AnyCbor::read(raw)
    }
}

/// Drive a definite- or indefinite-length CBOR sequence, invoking `read_one` for each element
/// (one item for arrays, called twice per entry by the map caller). Definite lengths bound the
/// loop by COUNT only — allocation grows with actual items read, so a hostile huge claimed length
/// errors when the buffer is exhausted rather than pre-allocating.
fn read_sequence<F>(
    raw: &mut cbor_event::de::Deserializer,
    len: cbor_event::LenSz,
    mut read_one: F,
) -> Result<(), DeserializeError>
where
    F: FnMut(&mut cbor_event::de::Deserializer) -> Result<(), DeserializeError>,
{
    match len {
        cbor_event::LenSz::Len(n, _) => {
            for _ in 0..n {
                read_one(raw)?;
            }
        }
        cbor_event::LenSz::Indefinite => loop {
            if raw.cbor_type()? == cbor_event::Type::Special && raw.special_break()? {
                break;
            }
            read_one(raw)?;
        },
    }
    Ok(())
}

// --- serialize helpers (self-contained; mirror fit_sz/to_len_sz/to_str_len_sz without depending
//     on the arity-varying fragment helpers) ---

fn fit_int_sz(arg: u64, sz: &Option<cbor_event::Sz>, force_canonical: bool) -> cbor_event::Sz {
    match sz {
        Some(sz) if !force_canonical && arg <= sz_max(*sz) => *sz,
        _ => cbor_event::Sz::canonical(arg),
    }
}

fn len_sz(enc: &LenEncoding, len: u64, force_canonical: bool) -> cbor_event::LenSz {
    if force_canonical {
        return cbor_event::LenSz::Len(len, cbor_event::Sz::canonical(len));
    }
    match enc {
        LenEncoding::Canonical => cbor_event::LenSz::Len(len, cbor_event::Sz::canonical(len)),
        LenEncoding::Definite(sz) if sz_max(*sz) >= len => cbor_event::LenSz::Len(len, *sz),
        LenEncoding::Definite(_) => cbor_event::LenSz::Len(len, cbor_event::Sz::canonical(len)),
        LenEncoding::Indefinite => cbor_event::LenSz::Indefinite,
    }
}

fn len_end<'a>(
    serializer: &'a mut cbor_event::se::Serializer,
    enc: &LenEncoding,
    force_canonical: bool,
) -> cbor_event::Result<&'a mut cbor_event::se::Serializer> {
    if !force_canonical && *enc == LenEncoding::Indefinite {
        serializer.write_special(cbor_event::Special::Break)?;
    }
    Ok(serializer)
}

fn str_len_sz(enc: &StringEncoding, len: u64, force_canonical: bool) -> cbor_event::StringLenSz {
    if force_canonical {
        return cbor_event::StringLenSz::Len(cbor_event::Sz::canonical(len));
    }
    match enc {
        StringEncoding::Canonical => cbor_event::StringLenSz::Len(cbor_event::Sz::canonical(len)),
        StringEncoding::Definite(sz) if sz_max(*sz) >= len => cbor_event::StringLenSz::Len(*sz),
        StringEncoding::Definite(_) => cbor_event::StringLenSz::Len(cbor_event::Sz::canonical(len)),
        StringEncoding::Indefinite(lens) => cbor_event::StringLenSz::Indefinite(lens.clone()),
    }
}

fn serialize_special<'a>(
    serializer: &'a mut cbor_event::se::Serializer,
    special: &AnySpecial,
    force_canonical: bool,
) -> cbor_event::Result<&'a mut cbor_event::se::Serializer> {
    match special {
        AnySpecial::Bool(b) => serializer.write_special(cbor_event::Special::Bool(*b)),
        AnySpecial::Null => serializer.write_special(cbor_event::Special::Null),
        AnySpecial::Undefined => serializer.write_special(cbor_event::Special::Undefined),
        AnySpecial::Unassigned(v) => serializer.write_special(cbor_event::Special::Unassigned(*v)),
        AnySpecial::Float(f, sz) => {
            if force_canonical && f.is_nan() {
                // RFC 8949 §4.2.2: the canonical NaN is the zero-payload quiet NaN `f9 7e00`
                // (drop any payload). `cbor_event::se::smallest_float_sz` would instead shorten to
                // the narrowest width that preserves the PAYLOAD (per its own doc comment), so a
                // strictly-canonical writer must special-case NaN first.
                // (write_float_sz(f64::NAN, Two) narrows the standard quiet NaN to f16 0x7e00.)
                return serializer.write_float_sz(f64::NAN, cbor_event::Sz::Two);
            }
            // LOCKSTEP: the width rule below is `serialization`'s `write_float`, restated here. That
            // helper's ARITY varies with `--canonical-form` (the force-canonical fragment takes a
            // `force_canonical` argument, the other does not) while this workhorse is shared by both
            // assemblies, so it cannot be called with one spelling — the same reason `fit_int_sz` /
            // `len_sz` / `str_len_sz` above are local mirrors. Keep the two in step.
            //
            // `smallest_float_sz` is the RFC 8949 §4.1 value-preserving smallest width (NaN payload
            // included); the NaN-drops-payload canonical case is handled above. A recorded width is
            // honored only while it still represents the value EXACTLY, because `write_float_sz`
            // ERRORS (`InvalidLenPassed`) on a lossy width rather than emitting a merely non-minimal
            // head — so a width the value has OUTGROWN widens to `smallest_float_sz` instead of
            // failing the write. A DECODED value never reaches that branch (its recorded width came
            // from `float_sz()` and is exact by construction); a hand-built or mutated value does —
            // `AnySpecial::Float(1.1, Some(Sz::Two))` would otherwise panic through `to_cbor_bytes`'s
            // unwrap. The float-invalid `Sz::Inline`/`Sz::One` fall out of the same comparison.
            // `sz_max` is reused purely as the width ORDERING (monotonic in head size); its integer
            // magnitude carries no meaning for a float.
            let smallest = cbor_event::se::smallest_float_sz(*f);
            let width = match sz {
                Some(sz) if !force_canonical && sz_max(*sz) >= sz_max(smallest) => *sz,
                _ => smallest,
            };
            serializer.write_float_sz(*f, width)
        }
    }
}

// --- representational Eq / Ord / Hash (encoding fields participate) ---

fn sz_rank(sz: cbor_event::Sz) -> u8 {
    match sz {
        cbor_event::Sz::Inline => 0,
        cbor_event::Sz::One => 1,
        cbor_event::Sz::Two => 2,
        cbor_event::Sz::Four => 3,
        cbor_event::Sz::Eight => 4,
    }
}

fn opt_sz_rank(sz: &Option<cbor_event::Sz>) -> (u8, u8) {
    match sz {
        None => (0, 0),
        Some(sz) => (1, sz_rank(*sz)),
    }
}

fn len_enc_rank(enc: &LenEncoding) -> (u8, u8) {
    match enc {
        LenEncoding::Canonical => (0, 0),
        LenEncoding::Definite(sz) => (1, sz_rank(*sz)),
        LenEncoding::Indefinite => (2, 0),
    }
}

fn str_enc_rank(enc: &StringEncoding) -> (u8, Vec<(u64, u8)>) {
    match enc {
        StringEncoding::Canonical => (0, Vec::new()),
        StringEncoding::Definite(sz) => (1, vec![(0, sz_rank(*sz))]),
        StringEncoding::Indefinite(lens) => {
            (2, lens.iter().map(|(l, sz)| (*l, sz_rank(*sz))).collect())
        }
    }
}

impl PartialEq for AnySpecial {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AnySpecial::Bool(a), AnySpecial::Bool(b)) => a == b,
            (AnySpecial::Null, AnySpecial::Null) => true,
            (AnySpecial::Undefined, AnySpecial::Undefined) => true,
            (AnySpecial::Unassigned(a), AnySpecial::Unassigned(b)) => a == b,
            (AnySpecial::Float(a, sa), AnySpecial::Float(b, sb)) => a.to_bits() == b.to_bits() && sa == sb,
            _ => false,
        }
    }
}
impl Eq for AnySpecial {}

impl PartialEq for AnyCbor {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AnyCbor::UInt(a, sa), AnyCbor::UInt(b, sb)) => a == b && sa == sb,
            (AnyCbor::NInt(a, sa), AnyCbor::NInt(b, sb)) => a == b && sa == sb,
            (AnyCbor::Bytes(a, ea), AnyCbor::Bytes(b, eb)) => a == b && ea == eb,
            (AnyCbor::Text(a, ea), AnyCbor::Text(b, eb)) => a == b && ea == eb,
            (AnyCbor::Array(a, ea), AnyCbor::Array(b, eb)) => a == b && ea == eb,
            (AnyCbor::Map(a, ea), AnyCbor::Map(b, eb)) => a == b && ea == eb,
            (AnyCbor::Tag(a, ia, sa), AnyCbor::Tag(b, ib, sb)) => a == b && ia == ib && sa == sb,
            (AnyCbor::Special(a), AnyCbor::Special(b)) => a == b,
            _ => false,
        }
    }
}
impl Eq for AnyCbor {}

impl core::hash::Hash for AnySpecial {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            AnySpecial::Bool(b) => b.hash(state),
            AnySpecial::Null | AnySpecial::Undefined => {}
            AnySpecial::Unassigned(v) => v.hash(state),
            AnySpecial::Float(f, sz) => {
                f.to_bits().hash(state);
                opt_sz_rank(sz).hash(state);
            }
        }
    }
}

impl core::hash::Hash for AnyCbor {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            AnyCbor::UInt(v, sz) => {
                v.hash(state);
                opt_sz_rank(sz).hash(state);
            }
            AnyCbor::NInt(v, sz) => {
                v.hash(state);
                opt_sz_rank(sz).hash(state);
            }
            AnyCbor::Bytes(v, enc) => {
                v.hash(state);
                str_enc_rank(enc).hash(state);
            }
            AnyCbor::Text(v, enc) => {
                v.hash(state);
                str_enc_rank(enc).hash(state);
            }
            AnyCbor::Array(v, enc) => {
                v.hash(state);
                len_enc_rank(enc).hash(state);
            }
            AnyCbor::Map(v, enc) => {
                v.hash(state);
                len_enc_rank(enc).hash(state);
            }
            AnyCbor::Tag(t, inner, sz) => {
                t.hash(state);
                inner.hash(state);
                opt_sz_rank(sz).hash(state);
            }
            AnyCbor::Special(s) => s.hash(state),
        }
    }
}

fn any_special_ord_rank(s: &AnySpecial) -> u8 {
    match s {
        AnySpecial::Bool(_) => 0,
        AnySpecial::Null => 1,
        AnySpecial::Undefined => 2,
        AnySpecial::Unassigned(_) => 3,
        AnySpecial::Float(..) => 4,
    }
}

impl Ord for AnySpecial {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        any_special_ord_rank(self)
            .cmp(&any_special_ord_rank(other))
            .then_with(|| match (self, other) {
                (AnySpecial::Bool(a), AnySpecial::Bool(b)) => a.cmp(b),
                (AnySpecial::Unassigned(a), AnySpecial::Unassigned(b)) => a.cmp(b),
                (AnySpecial::Float(a, sa), AnySpecial::Float(b, sb)) => {
                    a.total_cmp(b).then_with(|| opt_sz_rank(sa).cmp(&opt_sz_rank(sb)))
                }
                _ => core::cmp::Ordering::Equal,
            })
    }
}
impl PartialOrd for AnySpecial {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn any_cbor_ord_rank(v: &AnyCbor) -> u8 {
    match v {
        AnyCbor::UInt(..) => 0,
        AnyCbor::NInt(..) => 1,
        AnyCbor::Bytes(..) => 2,
        AnyCbor::Text(..) => 3,
        AnyCbor::Array(..) => 4,
        AnyCbor::Map(..) => 5,
        AnyCbor::Tag(..) => 6,
        AnyCbor::Special(..) => 7,
    }
}

impl Ord for AnyCbor {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        any_cbor_ord_rank(self)
            .cmp(&any_cbor_ord_rank(other))
            .then_with(|| match (self, other) {
                (AnyCbor::UInt(a, sa), AnyCbor::UInt(b, sb)) => {
                    a.cmp(b).then_with(|| opt_sz_rank(sa).cmp(&opt_sz_rank(sb)))
                }
                (AnyCbor::NInt(a, sa), AnyCbor::NInt(b, sb)) => {
                    a.cmp(b).then_with(|| opt_sz_rank(sa).cmp(&opt_sz_rank(sb)))
                }
                (AnyCbor::Bytes(a, ea), AnyCbor::Bytes(b, eb)) => {
                    a.cmp(b).then_with(|| str_enc_rank(ea).cmp(&str_enc_rank(eb)))
                }
                (AnyCbor::Text(a, ea), AnyCbor::Text(b, eb)) => {
                    a.cmp(b).then_with(|| str_enc_rank(ea).cmp(&str_enc_rank(eb)))
                }
                (AnyCbor::Array(a, ea), AnyCbor::Array(b, eb)) => {
                    a.cmp(b).then_with(|| len_enc_rank(ea).cmp(&len_enc_rank(eb)))
                }
                (AnyCbor::Map(a, ea), AnyCbor::Map(b, eb)) => {
                    a.cmp(b).then_with(|| len_enc_rank(ea).cmp(&len_enc_rank(eb)))
                }
                (AnyCbor::Tag(a, ia, sa), AnyCbor::Tag(b, ib, sb)) => a
                    .cmp(b)
                    .then_with(|| ia.cmp(ib))
                    .then_with(|| opt_sz_rank(sa).cmp(&opt_sz_rank(sb))),
                (AnyCbor::Special(a), AnyCbor::Special(b)) => a.cmp(b),
                _ => core::cmp::Ordering::Equal,
            })
    }
}
impl PartialOrd for AnyCbor {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
