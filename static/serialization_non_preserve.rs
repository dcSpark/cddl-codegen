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
