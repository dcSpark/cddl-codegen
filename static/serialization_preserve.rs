pub struct CBORReadLen {
    deser_len: cbor_event::LenSz,
    read: u64,
}

impl CBORReadLen {
    pub fn new(len: cbor_event::LenSz) -> Self {
        Self {
            deser_len: len,
            read: 0,
        }
    }

    pub fn read(&self) -> u64 {
        self.read
    }

    // Marks {n} values as being read, and if we go past the available definite length
    // given by the CBOR, we return an error.
    pub fn read_elems(&mut self, count: usize) -> Result<(), DeserializeFailure> {
        match self.deser_len {
            cbor_event::LenSz::Len(n, _) => {
                self.read += count as u64;
                if self.read > n {
                    Err(DeserializeFailure::DefiniteLenMismatch(n, None))
                } else {
                    Ok(())
                }
            },
            cbor_event::LenSz::Indefinite => Ok(()),
        }
    }

    pub fn finish(&self) -> Result<(), DeserializeFailure> {
        match self.deser_len {
            cbor_event::LenSz::Len(n, _) => {
                if self.read == n {
                    Ok(())
                } else {
                    Err(DeserializeFailure::DefiniteLenMismatch(n, Some(self.read)))
                }
            },
            cbor_event::LenSz::Indefinite => Ok(()),
        }
    }
}

// allows a preserve-flavored crate to serve as a --common-import-override target for
// preserve-encodings=false crates: they construct CBORReadLen via From<cbor_event::Len>, so we
// promote the size-less Len to a canonical-sized LenSz
impl From<cbor_event::Len> for CBORReadLen {
    fn from(len: cbor_event::Len) -> Self {
        let len_sz = match len {
            cbor_event::Len::Len(n) => cbor_event::LenSz::Len(n, cbor_event::Sz::canonical(n)),
            cbor_event::Len::Indefinite => cbor_event::LenSz::Indefinite,
        };
        Self::new(len_sz)
    }
}

pub trait DeserializeEmbeddedGroup {
    fn deserialize_as_embedded_group(
        raw: &mut Deserializer,
        read_len: &mut CBORReadLen,
        len: cbor_event::LenSz,
    ) -> Result<Self, DeserializeError> where Self: Sized;
}

#[inline]
pub fn sz_max(sz: cbor_event::Sz) -> u64 {
    match sz {
        cbor_event::Sz::Inline => 23u64,
        cbor_event::Sz::One => u8::MAX as u64,
        cbor_event::Sz::Two => u16::MAX as u64,
        cbor_event::Sz::Four => u32::MAX as u64,
        cbor_event::Sz::Eight => u64::MAX,
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Default)]
pub enum LenEncoding {
    #[default]
    Canonical,
    Definite(cbor_event::Sz),
    Indefinite,
}

impl From<cbor_event::LenSz> for LenEncoding {
    fn from(len_sz: cbor_event::LenSz) -> Self {
        match len_sz {
            cbor_event::LenSz::Len(len, sz) => if cbor_event::Sz::canonical(len) == sz {
                Self::Canonical
            } else {
                Self::Definite(sz)
            },
            cbor_event::LenSz::Indefinite => Self::Indefinite,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub enum StringEncoding {
    #[default]
    Canonical,
    Indefinite(Vec<(u64, cbor_event::Sz)>),
    Definite(cbor_event::Sz),
}

impl From<cbor_event::StringLenSz> for StringEncoding {
    fn from(len_sz: cbor_event::StringLenSz) -> Self {
        match len_sz {
            cbor_event::StringLenSz::Len(sz) => Self::Definite(sz),
            cbor_event::StringLenSz::Indefinite(lens) => Self::Indefinite(lens),
        }
    }
}
