use cbor_event::{self, de::Deserializer, se::{Serialize, Serializer}};
use std::io::{BufRead, Seek, Write};

#[derive(Debug)]
pub enum Key {
    Str(String),
    Uint(u64),
}

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Key::Str(x) => write!(f, "\"{}\"", x),
            Key::Uint(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug)]
pub enum DeserializeFailure {
    BreakInDefiniteLen,
    CBOR(cbor_event::Error),
    DefiniteLenMismatch(u64, Option<u64>),
    DuplicateKey(Key),
    EndingBreakMissing,
    ExpectedNull,
    FixedValueMismatch{
        found: Key,
        expected: Key,
    },
    MandatoryFieldMissing(Key),
    NoVariantMatched,
    RangeCheck{
        found: usize,
        min: Option<isize>,
        max: Option<isize>,
    },
    TagMismatch{
        found: u64,
        expected: u64,
    },
    UnknownKey(Key),
    UnexpectedKeyType(cbor_event::Type),
}

// we might want to add more info like which field,
#[derive(Debug)]
pub struct DeserializeError {
    location: Option<String>,
    failure: DeserializeFailure,
}

impl DeserializeError {
    pub fn new<T: Into<String>>(location: T, failure: DeserializeFailure) -> Self {
        Self {
            location: Some(location.into()),
            failure,
        }
    }

    pub fn annotate<T: Into<String>>(self, location: T) -> Self {
        match self.location {
            Some(loc) => Self::new(format!("{}.{}", location.into(), loc), self.failure),
            None => Self::new(location, self.failure),
        }
    }
}

impl std::fmt::Display for DeserializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.location {
            Some(loc) => write!(f, "Deserialization failed in {} because: ", loc),
            None => write!(f, "Deserialization: "),
        }?;
        match &self.failure {
            DeserializeFailure::BreakInDefiniteLen => write!(f, "Encountered CBOR Break while reading definite length sequence"),
            DeserializeFailure::CBOR(e) => e.fmt(f),
            DeserializeFailure::DefiniteLenMismatch(found, expected) => {
                write!(f, "Definite length mismatch: found {}", found)?;
                if let Some(expected_elems) = expected {
                    write!(f, ", expected: {}", expected_elems)?;
                }
                Ok(())
            },
            DeserializeFailure::DuplicateKey(key) => write!(f, "Duplicate key: {}", key),
            DeserializeFailure::EndingBreakMissing => write!(f, "Missing ending CBOR Break"),
            DeserializeFailure::ExpectedNull => write!(f, "Expected null, found other type"),
            DeserializeFailure::FixedValueMismatch{ found, expected } => write!(f, "Expected fixed value {} found {}", expected, found),
            DeserializeFailure::MandatoryFieldMissing(key) => write!(f, "Mandatory field {} not found", key),
            DeserializeFailure::NoVariantMatched => write!(f, "No variant matched"),
            DeserializeFailure::RangeCheck{ found, min, max } => match (min, max) {
                (Some(min), Some(max)) => write!(f, "{} not in range {} - {}", found, min, max),
                (Some(min), None) => write!(f, "{} not at least {}", found, min),
                (None, Some(max)) => write!(f, "{} not at most {}", found, max),
                (None, None) => write!(f, "invalid range (no min nor max specified)"),
            },
            DeserializeFailure::TagMismatch{ found, expected } => write!(f, "Expected tag {}, found {}", expected, found),
            DeserializeFailure::UnknownKey(key) => write!(f, "Found unexpected key {}", key),
            DeserializeFailure::UnexpectedKeyType(ty) => write!(f, "Found unexpected key of CBOR type {:?}", ty),
        }
    }
}

impl From<DeserializeFailure> for DeserializeError {
    fn from(failure: DeserializeFailure) -> DeserializeError {
        DeserializeError {
            location: None,
            failure,
        }
    }
}

impl From<cbor_event::Error> for DeserializeError {
    fn from(err: cbor_event::Error) -> DeserializeError {
        DeserializeError {
            location: None,
            failure: DeserializeFailure::CBOR(err),
        }
    }
}

// same as cbor_event::de::Deserialize but with our DeserializeError
pub trait Deserialize {
    fn deserialize<R: BufRead + Seek>(
        raw: &mut Deserializer<R>,
    ) -> Result<Self, DeserializeError> where Self: Sized;
}

impl<T: cbor_event::de::Deserialize> Deserialize for T {
    fn deserialize<R: BufRead + Seek>(raw: &mut Deserializer<R>) -> Result<T, DeserializeError> {
        T::deserialize(raw).map_err(|e| DeserializeError::from(e))
    }
}

pub trait DeserializeEmbeddedGroup {
    fn deserialize_as_embedded_group<R: BufRead + Seek>(
        raw: &mut Deserializer<R>,
        read_len: &mut CBORReadLen,
        len: cbor_event::Len,
    ) -> Result<Self, DeserializeError> where Self: Sized;
}

pub trait FromBytes {
    fn from_bytes(data: Vec<u8>) -> Result<Self, DeserializeError> where Self: Sized;
}

impl<T: Deserialize + Sized> FromBytes for T {
    fn from_bytes(data: Vec<u8>) -> Result<Self, DeserializeError> {
        let mut raw = Deserializer::from(std::io::Cursor::new(data));
        Self::deserialize(&mut raw)
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Int {
    Uint(u64),
    Nint(u64),
}

impl cbor_event::se::Serialize for Int {
    fn serialize<'se, W: Write>(&self, serializer: &'se mut Serializer<W>) -> cbor_event::Result<&'se mut Serializer<W>> {
        match self {
            Self::Uint(x) => serializer.write_unsigned_integer(*x),
            Self::Nint(x) => serializer.write_negative_integer(-(*x as i128) as i64),
        }
    }
}

impl Deserialize for Int {
    fn deserialize<R: BufRead + Seek>(raw: &mut Deserializer<R>) -> Result<Self, DeserializeError> {
        (|| -> Result<_, DeserializeError> {
            match raw.cbor_type()? {
                cbor_event::Type::UnsignedInteger => Ok(Self::Uint(raw.unsigned_integer()?)),
                cbor_event::Type::NegativeInteger => Ok(Self::Nint(-raw.negative_integer()? as u64)),
                _ => Err(DeserializeFailure::NoVariantMatched.into()),
            }
        })().map_err(|e| e.annotate("Int"))
    }
}

pub struct CBORReadLen {
    deser_len: cbor_event::Len,
    read: u64,
}

impl CBORReadLen {
    pub fn new(len: cbor_event::Len) -> Self {
        Self {
            deser_len: len,
            read: 0,
        }
    }

    // Marks {n} values as being read, and if we go past the available definite length
    // given by the CBOR, we return an error.
    pub fn read_elems(&mut self, count: usize) -> Result<(), DeserializeFailure> {
        match self.deser_len {
            cbor_event::Len::Len(n) => {
                self.read += count as u64;
                if self.read > n {
                    Err(DeserializeFailure::DefiniteLenMismatch(n, None))
                } else {
                    Ok(())
                }
            },
            cbor_event::Len::Indefinite => Ok(()),
        }
    }

    pub fn finish(&self) -> Result<(), DeserializeFailure> {
        match self.deser_len {
            cbor_event::Len::Len(n) => {
                if self.read == n {
                    Ok(())
                } else {
                    Err(DeserializeFailure::DefiniteLenMismatch(n, Some(self.read)))
                }
            },
            cbor_event::Len::Indefinite => Ok(()),
        }
    }
}
