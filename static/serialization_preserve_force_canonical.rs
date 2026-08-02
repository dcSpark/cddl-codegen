#[inline]
pub fn fit_sz(len: u64, sz: Option<cbor_event::Sz>, force_canonical: bool) -> cbor_event::Sz {
    match sz {
        Some(sz) => if !force_canonical && len <= sz_max(sz) {
            sz
        } else {
            cbor_event::Sz::canonical(len)
        },
        None => cbor_event::Sz::canonical(len),
    }
}

/// The float twin of `fit_sz` + the write it feeds, as ONE function: for a float the head width and
/// the written VALUE are not independent (see the NaN case below), so choosing them apart is how they
/// silently disagree.
///
/// A recorded wire width is honored whenever it still represents the value exactly. Two things differ
/// from the integer widths. Floats have only the 2/4/8-byte heads (RFC 8949 §3.3), and
/// `write_float_sz` ERRORS (`InvalidLenPassed`) on a width that would round the value — where an
/// integer encodes at any width at or above its own. So the fit test is against `smallest_float_sz`
/// (the narrowest LOSSLESS width for this value, NaN payload included) rather than a value range, and
/// a recorded width below it widens rather than failing serialization. That widening is reachable
/// whenever a decoded value is REPLACED: `1.5` read as `f9` keeps `Sz::Two`, and assigning `1.1` to
/// that field would otherwise error at serialize time. The float-invalid `Sz::Inline`/`Sz::One` —
/// which no `float_sz()` read produces, but a hand-built encoding struct can hold — fall out of the
/// same comparison. `sz_max` is reused purely as the width ORDERING (it is monotonic in head size);
/// its integer magnitude carries no meaning for a float.
///
/// A CANONICAL write drops a NaN payload first (RFC 8949 §4.2.2 makes the deterministic encoding of
/// NaN the zero-payload quiet NaN `f9 7e00`), and the width must then be derived from the NORMALIZED
/// value — `smallest_float_sz` of the payload-carrying value is `Sz::Eight`, which would write the
/// canonical NaN in a non-canonical width. Otherwise the canonical width IS `smallest_float_sz`: RFC
/// 8949 §4.1 preferred serialization for a float is the shortest form that preserves the value.
///
/// LOCKSTEP: `AnyCbor`'s float serializer (`static/any_cbor_preserve.rs`, `serialize_special`)
/// restates this rule rather than calling it — that workhorse is shared by both canonical
/// assemblies, and this helper's ARITY varies between them. The two must agree; keep them in step.
pub fn write_float(
    serializer: &mut Serializer,
    value: f64,
    sz: Option<cbor_event::Sz>,
    force_canonical: bool,
) -> cbor_event::Result<&mut Serializer> {
    let value = if force_canonical && value.is_nan() {
        f64::NAN
    } else {
        value
    };
    let smallest = cbor_event::se::smallest_float_sz(value);
    let width = match sz {
        Some(sz) if !force_canonical && sz_max(sz) >= sz_max(smallest) => sz,
        _ => smallest,
    };
    serializer.write_float_sz(value, width)
}

impl LenEncoding {
    pub fn to_len_sz(&self, len: u64, force_canonical: bool) -> cbor_event::LenSz {
        if force_canonical {
            cbor_event::LenSz::Len(len, cbor_event::Sz::canonical(len))
        } else {
            match self {
                Self::Canonical => cbor_event::LenSz::Len(len, cbor_event::Sz::canonical(len)),
                Self::Definite(sz) => if sz_max(*sz) >= len {
                    cbor_event::LenSz::Len(len, *sz)
                } else {
                    cbor_event::LenSz::Len(len, cbor_event::Sz::canonical(len))
                },
                Self::Indefinite => cbor_event::LenSz::Indefinite,
            }
        }
    }

    pub fn end<'a>(&self, serializer: &'a mut Serializer, force_canonical: bool) -> cbor_event::Result<&'a mut Serializer> {
        if !force_canonical && *self == Self::Indefinite {
            serializer.write_special(cbor_event::Special::Break)?;
        }
        Ok(serializer)
    }
}

impl StringEncoding {
    pub fn to_str_len_sz(&self, len: u64, force_canonical: bool) -> cbor_event::StringLenSz {
        if force_canonical {
            cbor_event::StringLenSz::Len(cbor_event::Sz::canonical(len))
        } else {
            match self {
                Self::Canonical => cbor_event::StringLenSz::Len(cbor_event::Sz::canonical(len)),
                Self::Definite(sz) => if sz_max(*sz) >= len {
                    cbor_event::StringLenSz::Len(*sz)
                } else {
                    cbor_event::StringLenSz::Len(cbor_event::Sz::canonical(len))
                },
                Self::Indefinite(lens) => cbor_event::StringLenSz::Indefinite(lens.clone()),
            }
        }
    }
}

pub trait Serialize {
    fn serialize<'a>(
        &self,
        serializer: &'a mut Serializer,
        force_canonical: bool,
    ) -> cbor_event::Result<&'a mut Serializer>;

    /// Bytes of a structure using the CBOR bytes as per the CDDL spec
    /// which for foo = bytes will include the CBOR bytes type/len, etc.
    /// This gives the original bytes in the case where this was created
    /// from bytes originally, or will use whatever the specific encoding
    /// details are present in any encoding details struct for the type.
    fn to_cbor_bytes(&self) -> Vec<u8> {
        let mut buf = Serializer::new_vec();
        self.serialize(&mut buf, false).unwrap();
        buf.finalize()
    }

    /// Bytes of a structure using the CBOR bytes as per the CDDL spec
    /// which for foo = bytes will include the CBOR bytes type/len, etc.
    /// This gives the canonically encoded CBOR bytes always
    fn to_canonical_cbor_bytes(&self) -> Vec<u8> {
        let mut buf = Serializer::new_vec();
        self.serialize(&mut buf, true).unwrap();
        buf.finalize()
    }
}

pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a>(
        &self,
        serializer: &'a mut Serializer,
        force_canonical: bool,
    ) -> cbor_event::Result<&'a mut Serializer>;
}

/// The class-constrained twin of `write_float`: the same recorded-width fit rule and the same
/// canonical NaN normalization, over a value the CDDL class spanning `min_width..=max_width` must
/// ADMIT.
///
/// Membership is decided first and a non-member fails loudly (`float_class_width`); the recorded
/// width is then honored on exactly the rule `write_float` uses — whenever it still represents the
/// value — and anything else falls back to the value's shortest form, which for a member is the
/// class's declared width. A canonical write ignores the recorded width entirely and takes that
/// shortest form (RFC 8949 §4.1).
///
/// Membership is judged on the NORMALIZED value, after the canonical NaN rule has replaced any
/// payload: the bytes a canonical write emits must be bytes this crate reads back as a member, and
/// the canonical NaN `f9 7e00` is a `float16` value.
///
/// The recorded width is deliberately NOT required to lie inside the class window. Reads accept any
/// head and judge the VALUE, so an `fb`-headed `1.5` is a perfectly good `float16` and records
/// `Sz::Eight`; clamping that back into the window on write would break the byte-exact round-trip
/// this flag exists for.
pub fn write_float_width(
    serializer: &mut Serializer,
    value: f64,
    sz: Option<cbor_event::Sz>,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
    force_canonical: bool,
) -> cbor_event::Result<&mut Serializer> {
    let value = if force_canonical && value.is_nan() {
        f64::NAN
    } else {
        value
    };
    let smallest = float_class_width(value, min_width, max_width)?;
    let width = match sz {
        Some(sz) if !force_canonical && sz_max(sz) >= sz_max(smallest) => sz,
        _ => smallest,
    };
    serializer.write_float_sz(value, width)
}
