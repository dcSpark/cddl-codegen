#[inline]
fn fit_sz(len: u64, sz: Option<cbor_event::Sz>, force_canonical: bool) -> Sz {
    match sz {
        Some(sz) => if !force_canonical && len <= sz_max(sz) {
            sz
        } else {
            Sz::canonical(len)
        },
        None => Sz::canonical(len),
    }
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

    pub fn end<'a, W: Write + Sized>(&self, serializer: &'a mut Serializer<W>, force_canonical: bool) -> cbor_event::Result<&'a mut Serializer<W>> {
        if !force_canonical && *self == Self::Indefinite {
            serializer.write_special(CBORSpecial::Break)?;
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
    fn serialize<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
        force_canonical: bool,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}

impl<T: cbor_event::se::Serialize> Serialize for T {
    fn serialize<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
        _force_canonical: bool,
    ) -> cbor_event::Result<&'a mut Serializer<W>> {
        <T as cbor_event::se::Serialize>::serialize(self, serializer)
    }
}

pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
        force_canonical: bool,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}


pub trait ToBytes {
    fn to_bytes(&self, force_canonical: bool) -> Vec<u8>;
}

impl<T: Serialize> ToBytes for T {
    fn to_bytes(&self, force_canonical: bool) -> Vec<u8> {
        let mut buf = Serializer::new_vec();
        self.serialize(&mut buf, force_canonical).unwrap();
        buf.finalize()
    }
}
