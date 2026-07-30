#[inline]
pub fn fit_sz(len: u64, sz: Option<cbor_event::Sz>) -> cbor_event::Sz {
    match sz {
        Some(sz) => if len <= sz_max(sz) {
            sz
        } else {
            cbor_event::Sz::canonical(len)
        },
        None => cbor_event::Sz::canonical(len),
    }
}

/// The float twin of `fit_sz` + the write it feeds, as ONE function (the force-canonical flavor of
/// this file has a canonical NaN rule that couples the head width to the written VALUE, so both
/// flavors keep the choice in one place).
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
/// LOCKSTEP: `AnyCbor`'s float serializer (`static/any_cbor_preserve.rs`, `serialize_special`)
/// restates this rule rather than calling it — that workhorse is shared by both canonical
/// assemblies, and this helper's ARITY varies between them. The two must agree; keep them in step.
pub fn write_float(
    serializer: &mut Serializer,
    value: f64,
    sz: Option<cbor_event::Sz>,
) -> cbor_event::Result<&mut Serializer> {
    let smallest = cbor_event::se::smallest_float_sz(value);
    let width = match sz {
        Some(sz) if sz_max(sz) >= sz_max(smallest) => sz,
        _ => smallest,
    };
    serializer.write_float_sz(value, width)
}

impl LenEncoding {
    pub fn to_len_sz(&self, len: u64) -> cbor_event::LenSz {
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

    pub fn end<'a>(&self, serializer: &'a mut Serializer) -> cbor_event::Result<&'a mut Serializer> {
        if *self == Self::Indefinite {
            serializer.write_special(cbor_event::Special::Break)?;
        }
        Ok(serializer)
    }
}

impl StringEncoding {
    pub fn to_str_len_sz(&self, len: u64) -> cbor_event::StringLenSz {
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
