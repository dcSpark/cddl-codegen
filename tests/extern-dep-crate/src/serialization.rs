// same as cbor_event::de::Deserialize but with our DeserializeError
pub trait Deserialize {
    fn from_cbor_bytes(data: &[u8]) -> Result<Self, DeserializeError>
    where
        Self: Sized,
    {
        let mut raw = Deserializer::from(std::io::Cursor::new(data));
        Self::deserialize(&mut raw)
    }

    fn deserialize<R: BufRead + Seek>(raw: &mut Deserializer<R>) -> Result<Self, DeserializeError>
    where
        Self: Sized;
}

impl<T: cbor_event::de::Deserialize> Deserialize for T {
    fn deserialize<R: BufRead + Seek>(raw: &mut Deserializer<R>) -> Result<T, DeserializeError> {
        T::deserialize(raw).map_err(DeserializeError::from)
    }
}
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
            }
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
            }
            cbor_event::LenSz::Indefinite => Ok(()),
        }
    }
}

pub trait DeserializeEmbeddedGroup {
    fn deserialize_as_embedded_group<R: BufRead + Seek>(
        raw: &mut Deserializer<R>,
        read_len: &mut CBORReadLen,
        len: cbor_event::LenSz,
    ) -> Result<Self, DeserializeError>
    where
        Self: Sized;
}

#[inline]
pub(crate) fn sz_max(sz: cbor_event::Sz) -> u64 {
    match sz {
        cbor_event::Sz::Inline => 23u64,
        cbor_event::Sz::One => u8::MAX as u64,
        cbor_event::Sz::Two => u16::MAX as u64,
        cbor_event::Sz::Four => u32::MAX as u64,
        cbor_event::Sz::Eight => u64::MAX,
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum LenEncoding {
    Canonical,
    Definite(cbor_event::Sz),
    Indefinite,
}

impl Default for LenEncoding {
    fn default() -> Self {
        Self::Canonical
    }
}

impl From<cbor_event::LenSz> for LenEncoding {
    fn from(len_sz: cbor_event::LenSz) -> Self {
        match len_sz {
            cbor_event::LenSz::Len(len, sz) => {
                if cbor_event::Sz::canonical(len) == sz {
                    Self::Canonical
                } else {
                    Self::Definite(sz)
                }
            }
            cbor_event::LenSz::Indefinite => Self::Indefinite,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StringEncoding {
    Canonical,
    Indefinite(Vec<(u64, cbor_event::Sz)>),
    Definite(cbor_event::Sz),
}

impl Default for StringEncoding {
    fn default() -> Self {
        Self::Canonical
    }
}

impl From<cbor_event::StringLenSz> for StringEncoding {
    fn from(len_sz: cbor_event::StringLenSz) -> Self {
        match len_sz {
            cbor_event::StringLenSz::Len(sz) => Self::Definite(sz),
            cbor_event::StringLenSz::Indefinite(lens) => Self::Indefinite(lens),
        }
    }
}
#[inline]
pub fn fit_sz(len: u64, sz: Option<cbor_event::Sz>) -> cbor_event::Sz {
    match sz {
        Some(sz) => {
            if len <= sz_max(sz) {
                sz
            } else {
                cbor_event::Sz::canonical(len)
            }
        }
        None => cbor_event::Sz::canonical(len),
    }
}

impl LenEncoding {
    pub fn to_len_sz(&self, len: u64) -> cbor_event::LenSz {
        match self {
            Self::Canonical => cbor_event::LenSz::Len(len, cbor_event::Sz::canonical(len)),
            Self::Definite(sz) => {
                if sz_max(*sz) >= len {
                    cbor_event::LenSz::Len(len, *sz)
                } else {
                    cbor_event::LenSz::Len(len, cbor_event::Sz::canonical(len))
                }
            }
            Self::Indefinite => cbor_event::LenSz::Indefinite,
        }
    }

    pub fn end<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
    ) -> cbor_event::Result<&'a mut Serializer<W>> {
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
            Self::Definite(sz) => {
                if sz_max(*sz) >= len {
                    cbor_event::StringLenSz::Len(*sz)
                } else {
                    cbor_event::StringLenSz::Len(cbor_event::Sz::canonical(len))
                }
            }
            Self::Indefinite(lens) => cbor_event::StringLenSz::Indefinite(lens.clone()),
        }
    }
}
pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}

pub trait ToCBORBytes {
    fn to_cbor_bytes(&self) -> Vec<u8>;
}

impl<T: cbor_event::se::Serialize> ToCBORBytes for T {
    fn to_cbor_bytes(&self) -> Vec<u8> {
        let mut buf = Serializer::new_vec();
        self.serialize(&mut buf).unwrap();
        buf.finalize()
    }
}

// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

use super::cbor_encodings::*;
use super::*;
use crate::error::*;
use cbor_event::de::Deserializer;
use cbor_event::se::{Serialize, Serializer};
use std::io::{BufRead, Seek, SeekFrom, Write};

impl cbor_event::se::Serialize for ExternCrateFoo {
    fn serialize<'se, W: Write>(
        &self,
        serializer: &'se mut Serializer<W>,
    ) -> cbor_event::Result<&'se mut Serializer<W>> {
        serializer.write_tag_sz(
            11u64,
            fit_sz(
                11u64,
                self.encodings
                    .as_ref()
                    .map(|encs| encs.tag_encoding)
                    .unwrap_or_default(),
            ),
        )?;
        serializer.write_array_sz(
            self.encodings
                .as_ref()
                .map(|encs| encs.len_encoding)
                .unwrap_or_default()
                .to_len_sz(3),
        )?;
        serializer.write_unsigned_integer_sz(
            self.index_0,
            fit_sz(
                self.index_0,
                self.encodings
                    .as_ref()
                    .map(|encs| encs.index_0_encoding)
                    .unwrap_or_default(),
            ),
        )?;
        serializer.write_text_sz(
            &self.index_1,
            self.encodings
                .as_ref()
                .map(|encs| encs.index_1_encoding.clone())
                .unwrap_or_default()
                .to_str_len_sz(self.index_1.len() as u64),
        )?;
        serializer.write_bytes_sz(
            &self.index_2,
            self.encodings
                .as_ref()
                .map(|encs| encs.index_2_encoding.clone())
                .unwrap_or_default()
                .to_str_len_sz(self.index_2.len() as u64),
        )?;
        self.encodings
            .as_ref()
            .map(|encs| encs.len_encoding)
            .unwrap_or_default()
            .end(serializer)
    }
}

impl Deserialize for ExternCrateFoo {
    fn deserialize<R: BufRead + Seek>(raw: &mut Deserializer<R>) -> Result<Self, DeserializeError> {
        let (tag, tag_encoding) = raw.tag_sz()?;
        if tag != 11 {
            return Err(DeserializeError::new(
                "ExternCrateFoo",
                DeserializeFailure::TagMismatch {
                    found: tag,
                    expected: 11,
                },
            ));
        }
        let len = raw.array_sz()?;
        let len_encoding: LenEncoding = len.into();
        let mut read_len = CBORReadLen::new(len);
        read_len.read_elems(3)?;
        read_len.finish()?;
        (|| -> Result<_, DeserializeError> {
            let (index_0, index_0_encoding) = raw
                .unsigned_integer_sz()
                .map_err(Into::<DeserializeError>::into)
                .map(|(x, enc)| (x, Some(enc)))
                .map_err(|e: DeserializeError| e.annotate("index_0"))?;
            let (index_1, index_1_encoding) = raw
                .text_sz()
                .map_err(Into::<DeserializeError>::into)
                .map(|(s, enc)| (s, StringEncoding::from(enc)))
                .map_err(|e: DeserializeError| e.annotate("index_1"))?;
            let (index_2, index_2_encoding) = raw
                .bytes_sz()
                .map_err(Into::<DeserializeError>::into)
                .map(|(bytes, enc)| (bytes, StringEncoding::from(enc)))
                .map_err(|e: DeserializeError| e.annotate("index_2"))?;
            match len {
                cbor_event::LenSz::Len(_, _) => (),
                cbor_event::LenSz::Indefinite => match raw.special()? {
                    cbor_event::Special::Break => (),
                    _ => return Err(DeserializeFailure::EndingBreakMissing.into()),
                },
            }
            Ok(ExternCrateFoo {
                index_0,
                index_1,
                index_2,
                encodings: Some(ExternCrateFooEncoding {
                    len_encoding,
                    tag_encoding: Some(tag_encoding),
                    index_0_encoding,
                    index_1_encoding,
                    index_2_encoding,
                }),
            })
        })()
        .map_err(|e| e.annotate("ExternCrateFoo"))
    }
}
