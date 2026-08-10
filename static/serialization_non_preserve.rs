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

/// Write a float at the smallest head that preserves its value exactly — RFC 8949 §4.1 preferred
/// serialization, uniformly, the same rule the integer writes follow. The
/// `--preserve-encodings` twin of this name additionally replays a recorded wire width; without that
/// flag there is none to replay, so the smallest form is the whole rule.
pub fn write_float(serializer: &mut Serializer, value: f64) -> cbor_event::Result<&mut Serializer> {
    serializer.write_float_sz(value, cbor_event::se::smallest_float_sz(value))
}

/// Write a float belonging to a CDDL class spanning the widths `min_width..=max_width`, at the
/// smallest head that preserves its value (RFC 8949 §4.1) — which for a member of the class IS its
/// declared width, since membership means the value's shortest lossless form lands in the window.
///
/// A NON-member — `1.5` assigned to a `float32` field — fails LOUDLY in `float_class_width` rather
/// than being written at a head the class admits: those bytes would decode to a value this crate's
/// own reader rejects for that field. See [`float_class_width`] for the value-set semantics.
pub fn write_float_width(
    serializer: &mut Serializer,
    value: f64,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
) -> cbor_event::Result<&mut Serializer> {
    let width = float_class_width(value, min_width, max_width)?;
    serializer.write_float_sz(value, width)
}
