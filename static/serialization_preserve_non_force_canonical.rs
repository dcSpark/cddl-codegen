#[inline]
fn fit_sz(len: u64, sz: Option<cbor_event::Sz>) -> cbor_event::Sz {
    match sz {
        Some(sz) => if len <= sz_max(sz) {
            sz
        } else {
            cbor_event::Sz::canonical(len)
        },
        None => cbor_event::Sz::canonical(len),
    }
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

    pub fn end<'a, W: Write + Sized>(&self, serializer: &'a mut Serializer<W>) -> cbor_event::Result<&'a mut Serializer<W>> {
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
