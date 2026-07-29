/// Decodes a **canonical** hex string: lowercase digits only, even length, no `0x`/`0X` prefix.
///
/// This is the single hex-reading door of a generated crate — `RawBytesEncoding::from_raw_hex` and
/// every emitted bytes-newtype JSON deserializer route through it — so the accepted grammar is
/// stated in one place and cannot drift between the two surfaces.
///
/// **Canonical in, canonical out.** The accepted grammar is exactly the grammar the WRITE side
/// emits (`hex::encode`, and `to_raw_hex` built on it: bare, even-length, lowercase). That buys the
/// round-trip property on the hex ENCODING and not merely on the bytes: for every accepted `s`,
/// re-encoding the decoded bytes reproduces `s` byte-for-byte. A grammar admitting spellings the
/// writer never produces could only offer the weaker bytes-level round trip, and would leave a read
/// side quietly wider than the write side.
///
/// Callers holding hex  produced elsewhere lowercase it themselves (`s.to_ascii_lowercase()`) before
/// handing it over.
///
/// The scan is what makes the grammar ours rather than the backing decoder's: that decoder has no
/// strict mode — its `decode` strips a `0x` prefix and accepts either case, and its `check_raw`
/// predicate accepts both case and odd length while yielding no typed error. Reporting the first
/// byte outside `[0-9a-f]` here leaves `hex::decode` with exactly one reachable failure,
/// `OddLength`, so a doubly-malformed input (bad character *and* odd length) reports the character.
/// The byte index and the `b as char` spelling match the backing decoder's own convention for the
/// position and character it names, so both error paths read identically to a consumer.
pub fn decode_canonical_hex(s: &str) -> Result<Vec<u8>, hex::FromHexError> {
    for (index, b) in s.as_bytes().iter().enumerate() {
        if !matches!(b, b'0'..=b'9' | b'a'..=b'f') {
            return Err(hex::FromHexError::InvalidHexCharacter {
                c: *b as char,
                index,
            });
        }
    }
    hex::decode(s)
}
