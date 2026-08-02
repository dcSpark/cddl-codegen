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

    pub fn read(&self) -> u64 {
        self.read
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

// allows a crate exported by cddl-codegen to serve as a --common-import-override target for
// preserve-encodings=false crates, which construct CBORReadLen via From<cbor_event::Len>
impl From<cbor_event::Len> for CBORReadLen {
    fn from(len: cbor_event::Len) -> Self {
        Self::new(len)
    }
}

pub trait DeserializeEmbeddedGroup {
    fn deserialize_as_embedded_group(
        raw: &mut Deserializer,
        read_len: &mut CBORReadLen,
        len: cbor_event::Len,
    ) -> Result<Self, DeserializeError> where Self: Sized;
}

/// Write a float whose CDDL type declares the head widths `min_width..=max_width`, at the narrowest
/// admitted head that encodes `value` losslessly (RFC 8949 §4.2.2 preferred serialization
/// restricted to the type's own set). A single-width class writes exactly that head.
///
/// Fails (`InvalidLenPassed`, through `write_float_sz`) when NO admitted head represents the value
/// exactly — a `float16` member holding a value that is not f16-exact is the reachable case. Loud
/// by design: rounding to fit the declared width would be a silent value mutation.
pub fn write_float_width(
    serializer: &mut Serializer,
    value: f64,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
) -> cbor_event::Result<&mut Serializer> {
    serializer.write_float_sz(value, float_head_width(value, min_width, max_width))
}
