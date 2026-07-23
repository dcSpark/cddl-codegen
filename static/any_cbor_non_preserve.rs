// `AnyCbor` — a structured CBOR value (the runtime lowering of CDDL `any`).
//
// NON-PRESERVE variant (concatenated when `--preserve-encodings` is off). No encoding metadata:
// the round-trip contract is VALUE-level (re-serialization is canonical-ish, exactly as
// non-preserve generated code behaves — smallest integer widths, definite lengths, floats always
// written f64), matching the crate-wide non-preserve contract. Equality/ordering/hashing compare
// VALUE only (floats by `total_cmp`/bit pattern so the relations are total and NaN is
// self-consistent). The preserve variant (`any_cbor_preserve.rs`) is representational instead.
//
// Depth: `deserialize` routes all recursion through the single `read` seam, whose first line
// invokes `any_cbor_recursion_guard!()`. The includer supplies that macro: the static assembly in
// generated crates expands it to `DepthGuard::acquire(<baked limit>)?` under
// `--deserialize-depth-limit` (and to nothing without the flag, so no-flag crates keep
// byte-identical output); the property-harness shims supply their own definition to exercise the
// guard. Without the flag: bounded allocation, dangling-`Break` rejection, `Err` (never panic) on
// malformed input — same as the rest of the crate.
#[derive(Clone, Debug)]
pub enum AnyCbor {
    UInt(u64),
    NInt(i128), // full CBOR nint domain (-2^64..=-1)
    Bytes(Vec<u8>),
    Text(String),
    Array(Vec<AnyCbor>),
    Map(Vec<(AnyCbor, AnyCbor)>), // wire order AND duplicate keys preserved
    Tag(u64, Box<AnyCbor>),
    Special(AnySpecial),
}

#[derive(Clone, Debug)]
pub enum AnySpecial {
    Bool(bool),
    Null,
    Undefined,
    Unassigned(u8),
    Float(f64),
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

/// Length-first-then-bytewise comparison of two encoded CBOR map keys — the RFC 7049 §3.9
/// "canonical CBOR" key ordering. Shared runtime helper (see the preserve variant for the fuller
/// note); present in both variants so callers can rely on it regardless of `--preserve-encodings`.
pub fn any_cbor_canonical_key_cmp(lhs: &[u8], rhs: &[u8]) -> std::cmp::Ordering {
    match lhs.len().cmp(&rhs.len()) {
        std::cmp::Ordering::Equal => lhs.cmp(rhs),
        diff_ord => diff_ord,
    }
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
            AnyCbor::UInt(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_nint(&self) -> Option<i128> {
        match self {
            AnyCbor::NInt(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            AnyCbor::Bytes(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_text(&self) -> Option<&str> {
        match self {
            AnyCbor::Text(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&[AnyCbor]> {
        match self {
            AnyCbor::Array(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_map(&self) -> Option<&[(AnyCbor, AnyCbor)]> {
        match self {
            AnyCbor::Map(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_tag(&self) -> Option<(u64, &AnyCbor)> {
        match self {
            AnyCbor::Tag(t, inner) => Some((*t, inner)),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            AnyCbor::Special(AnySpecial::Float(f)) => Some(*f),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            AnyCbor::Special(AnySpecial::Bool(b)) => Some(*b),
            _ => None,
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, AnyCbor::Special(AnySpecial::Null))
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self, AnyCbor::Special(AnySpecial::Undefined))
    }

    /// Canonical-ish value re-encoding (smallest widths, definite lengths, f64 floats).
    pub fn to_cbor_bytes(&self) -> Vec<u8> {
        let mut buf = cbor_event::se::Serializer::new_vec();
        self.serialize_ref(&mut buf).unwrap();
        buf.finalize()
    }

    /// The serialization workhorse; the `cbor_event::se::Serialize` impl delegates here.
    pub fn serialize_ref<'a>(
        &self,
        serializer: &'a mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'a mut cbor_event::se::Serializer> {
        match self {
            AnyCbor::UInt(v) => {
                serializer.write_unsigned_integer(*v)?;
            }
            AnyCbor::NInt(v) => {
                let arg = (-((*v) + 1)) as u64;
                serializer.write_negative_integer_sz(*v, cbor_event::Sz::canonical(arg))?;
            }
            AnyCbor::Bytes(bytes) => {
                serializer.write_bytes(bytes)?;
            }
            AnyCbor::Text(text) => {
                serializer.write_text(text)?;
            }
            AnyCbor::Array(elems) => {
                serializer.write_array(cbor_event::Len::Len(elems.len() as u64))?;
                for elem in elems.iter() {
                    elem.serialize_ref(serializer)?;
                }
            }
            AnyCbor::Map(pairs) => {
                serializer.write_map(cbor_event::Len::Len(pairs.len() as u64))?;
                for (key, value) in pairs.iter() {
                    key.serialize_ref(serializer)?;
                    value.serialize_ref(serializer)?;
                }
            }
            AnyCbor::Tag(tag, inner) => {
                serializer.write_tag(*tag)?;
                inner.serialize_ref(serializer)?;
            }
            AnyCbor::Special(special) => match special {
                AnySpecial::Bool(b) => {
                    serializer.write_special(cbor_event::Special::Bool(*b))?;
                }
                AnySpecial::Null => {
                    serializer.write_special(cbor_event::Special::Null)?;
                }
                AnySpecial::Undefined => {
                    serializer.write_special(cbor_event::Special::Undefined)?;
                }
                AnySpecial::Unassigned(v) => {
                    serializer.write_special(cbor_event::Special::Unassigned(*v))?;
                }
                AnySpecial::Float(f) => {
                    serializer.write_special(cbor_event::Special::Float(*f))?;
                }
            },
        }
        Ok(serializer)
    }

    /// Recursion seam for deserialize (see the file header re: the depth guard).
    fn read(raw: &mut cbor_event::de::Deserializer) -> Result<Self, DeserializeError> {
        // Depth-guard hook: expands to a `DepthGuard::acquire(<baked limit>)?` RAII binding when the
        // generated crate is built with `--deserialize-depth-limit`, or to nothing otherwise. The
        // macro is supplied by the includer (the static assembly for generated crates; the test
        // shims for the property harness) so this one file serves every flag combination unsplit.
        any_cbor_recursion_guard!();
        match raw.cbor_type()? {
            cbor_event::Type::UnsignedInteger => Ok(AnyCbor::UInt(raw.unsigned_integer_sz()?.0)),
            cbor_event::Type::NegativeInteger => Ok(AnyCbor::NInt(raw.negative_integer_sz()?.0)),
            cbor_event::Type::Bytes => Ok(AnyCbor::Bytes(raw.bytes_sz()?.0)),
            cbor_event::Type::Text => Ok(AnyCbor::Text(raw.text_sz()?.0)),
            cbor_event::Type::Array => {
                let len = raw.array_sz()?;
                let mut elems = Vec::new(); // never with_capacity(claimed len)
                read_sequence(raw, len, |raw| {
                    elems.push(AnyCbor::read(raw)?);
                    Ok(())
                })?;
                Ok(AnyCbor::Array(elems))
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
                Ok(AnyCbor::Map(pairs))
            }
            cbor_event::Type::Tag => {
                let (tag, _sz) = raw.tag_sz()?;
                Ok(AnyCbor::Tag(tag, Box::new(AnyCbor::read(raw)?)))
            }
            cbor_event::Type::Special => {
                let head = *raw
                    .as_slice()
                    .first()
                    .ok_or_else(|| DeserializeFailure::CBOR(cbor_event::Error::NotEnough(0, 1)))?;
                match head & 0b0001_1111 {
                    0x19..=0x1b => Ok(AnyCbor::Special(AnySpecial::Float(raw.float_sz()?.0))),
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

impl cbor_event::se::Serialize for AnyCbor {
    fn serialize<'se>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {
        self.serialize_ref(serializer)
    }
}

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

// --- value-level Eq / Ord / Hash (floats by bits / total_cmp) ---

impl PartialEq for AnySpecial {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AnySpecial::Bool(a), AnySpecial::Bool(b)) => a == b,
            (AnySpecial::Null, AnySpecial::Null) => true,
            (AnySpecial::Undefined, AnySpecial::Undefined) => true,
            (AnySpecial::Unassigned(a), AnySpecial::Unassigned(b)) => a == b,
            (AnySpecial::Float(a), AnySpecial::Float(b)) => a.to_bits() == b.to_bits(),
            _ => false,
        }
    }
}
impl Eq for AnySpecial {}

impl PartialEq for AnyCbor {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AnyCbor::UInt(a), AnyCbor::UInt(b)) => a == b,
            (AnyCbor::NInt(a), AnyCbor::NInt(b)) => a == b,
            (AnyCbor::Bytes(a), AnyCbor::Bytes(b)) => a == b,
            (AnyCbor::Text(a), AnyCbor::Text(b)) => a == b,
            (AnyCbor::Array(a), AnyCbor::Array(b)) => a == b,
            (AnyCbor::Map(a), AnyCbor::Map(b)) => a == b,
            (AnyCbor::Tag(a, ia), AnyCbor::Tag(b, ib)) => a == b && ia == ib,
            (AnyCbor::Special(a), AnyCbor::Special(b)) => a == b,
            _ => false,
        }
    }
}
impl Eq for AnyCbor {}

impl std::hash::Hash for AnySpecial {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            AnySpecial::Bool(b) => b.hash(state),
            AnySpecial::Null | AnySpecial::Undefined => {}
            AnySpecial::Unassigned(v) => v.hash(state),
            AnySpecial::Float(f) => f.to_bits().hash(state),
        }
    }
}

impl std::hash::Hash for AnyCbor {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            AnyCbor::UInt(v) => v.hash(state),
            AnyCbor::NInt(v) => v.hash(state),
            AnyCbor::Bytes(v) => v.hash(state),
            AnyCbor::Text(v) => v.hash(state),
            AnyCbor::Array(v) => v.hash(state),
            AnyCbor::Map(v) => v.hash(state),
            AnyCbor::Tag(t, inner) => {
                t.hash(state);
                inner.hash(state);
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
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        any_special_ord_rank(self)
            .cmp(&any_special_ord_rank(other))
            .then_with(|| match (self, other) {
                (AnySpecial::Bool(a), AnySpecial::Bool(b)) => a.cmp(b),
                (AnySpecial::Unassigned(a), AnySpecial::Unassigned(b)) => a.cmp(b),
                (AnySpecial::Float(a), AnySpecial::Float(b)) => a.total_cmp(b),
                _ => std::cmp::Ordering::Equal,
            })
    }
}
impl PartialOrd for AnySpecial {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
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
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        any_cbor_ord_rank(self)
            .cmp(&any_cbor_ord_rank(other))
            .then_with(|| match (self, other) {
                (AnyCbor::UInt(a), AnyCbor::UInt(b)) => a.cmp(b),
                (AnyCbor::NInt(a), AnyCbor::NInt(b)) => a.cmp(b),
                (AnyCbor::Bytes(a), AnyCbor::Bytes(b)) => a.cmp(b),
                (AnyCbor::Text(a), AnyCbor::Text(b)) => a.cmp(b),
                (AnyCbor::Array(a), AnyCbor::Array(b)) => a.cmp(b),
                (AnyCbor::Map(a), AnyCbor::Map(b)) => a.cmp(b),
                (AnyCbor::Tag(a, ia), AnyCbor::Tag(b, ib)) => a.cmp(b).then_with(|| ia.cmp(ib)),
                (AnyCbor::Special(a), AnyCbor::Special(b)) => a.cmp(b),
                _ => std::cmp::Ordering::Equal,
            })
    }
}
impl PartialOrd for AnyCbor {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
