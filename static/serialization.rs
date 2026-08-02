// same as cbor_event::de::Deserialize but with our DeserializeError
pub trait Deserialize {
    fn deserialize(
        raw: &mut Deserializer,
    ) -> Result<Self, DeserializeError> where Self: Sized;

    // cbor_event's Deserializer owns its buffer, so this copies `data` once (O(n))
    /// Deserialize from this type's CBOR form.
    /// For a type that also has a raw-bytes form (e.g. one implementing `RawBytesEncoding`),
    /// this expects the CBOR byte-string encoding (type/length header plus payload),
    /// where `from_raw_bytes` takes the bare payload.
    fn from_cbor_bytes(data: &[u8]) -> Result<Self, DeserializeError> where Self: Sized {
        let mut raw = Deserializer::from(data.to_vec());
        let value = Self::deserialize(&mut raw)?;
        // Reject leftover bytes after a complete value instead of silently ignoring them: otherwise a
        // truncated/corrupt or accidentally-concatenated buffer would deserialize as Ok.
        // Hand-rolled rather than cbor_event's deserialize_complete(): that helper is bounded to
        // cbor_event's own Deserialize trait and error type, while this trait must surface the
        // annotated DeserializeError.
        if !raw.as_slice().is_empty() {
            return Err(DeserializeFailure::CBOR(cbor_event::Error::TrailingData).into());
        }
        Ok(value)
    }
}

impl<T: cbor_event::de::Deserialize> Deserialize for T {
    fn deserialize(raw: &mut Deserializer) -> Result<T, DeserializeError> {
        T::deserialize(raw).map_err(DeserializeError::from)
    }
}

/// Rank of a CBOR float head in WIDTH order (`#7.25` < `#7.26` < `#7.27`, RFC 8949 §3.3). The
/// float-invalid `Sz::Inline`/`Sz::One` — which no `float_sz()` read produces, but a hand-built
/// encoding struct can hold — sort with the widest head, so they can never pass a head-set check
/// that the widest head itself would not.
fn float_head_rank(sz: cbor_event::Sz) -> u8 {
    match sz {
        cbor_event::Sz::Two => 0,
        cbor_event::Sz::Four => 1,
        _ => 2,
    }
}

/// The narrowest CBOR float head in `min_width..=max_width` that encodes `value` with no loss —
/// RFC 8949 §4.2.2 preferred serialization restricted to the head set the CDDL type DECLARES.
///
/// When no head in the set is lossless (only reachable for a single-width class, e.g. a `float16`
/// member holding a value that is not f16-exact) this returns the widest admitted head, and the
/// write then FAILS loudly with `InvalidLenPassed` instead of rounding the value. That is the whole
/// point of a declared width: silently writing a different value is exactly the mutation a
/// head-strict type exists to prevent.
pub fn float_head_width(
    value: f64,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
) -> cbor_event::Sz {
    let smallest = cbor_event::se::smallest_float_sz(value);
    if float_head_rank(smallest) < float_head_rank(min_width) {
        min_width
    } else if float_head_rank(smallest) > float_head_rank(max_width) {
        max_width
    } else {
        smallest
    }
}

/// Exact `f32` -> `f64` widening, NaN payloads included.
///
/// The NaN case is done in software because a hardware widening (`value as f64` / `f64::from`) may
/// quiet a signaling NaN or drop its payload depending on the platform — and a declared-width float
/// round-trips byte-exactly, payload included. Every other value widens exactly under `f64::from`.
/// This restates cbor_event's own inbound widening, which is not public.
pub fn widen_f32(value: f32) -> f64 {
    if value.is_nan() {
        let bits = value.to_bits();
        f64::from_bits(
            u64::from(bits >> 31) << 63
                | 0x7ff0_0000_0000_0000
                | u64::from(bits & 0x007f_ffff) << 29,
        )
    } else {
        f64::from(value)
    }
}

/// Exact `f64` -> `f32` narrowing for a value a head-set check has ALREADY proven fits: every head
/// an `f32`-carried CDDL float class admits (`#7.25`, `#7.26`) is f32-exact by construction, so the
/// miss arm is unreachable. It panics rather than erroring because reaching it would mean the head
/// check above it is wrong — a generator bug, not malformed input. Never an `as` cast: that rounds
/// silently and its NaN-payload behaviour is platform-dependent.
pub fn narrow_f32(value: f64) -> f32 {
    cbor_event::se::f64_to_f32_exact(value)
        .expect("a float head an f32-carried CDDL type admits is f32-exact by construction")
}

/// Read a float whose CDDL type admits only the head widths `min_width..=max_width` (`float32` is
/// `#7.26` alone; `float16-32` is `#7.25`/`#7.26`; `float` admits all three). A head outside the set
/// is a decode ERROR — never a silent widening or narrowing of the value, which would make a
/// `float32` member's decoded value depend on bytes its own type forbids.
pub fn read_float_sz_width(
    raw: &mut Deserializer,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
) -> Result<(f64, cbor_event::Sz), DeserializeError> {
    let (value, sz) = raw.float_sz()?;
    if float_head_rank(sz) < float_head_rank(min_width)
        || float_head_rank(sz) > float_head_rank(max_width)
    {
        return Err(DeserializeFailure::FloatWidth {
            found: sz,
            min: min_width,
            max: max_width,
        }
        .into());
    }
    Ok((value, sz))
}

/// [`read_float_sz_width`] dropping the head width — the non-`--preserve-encodings` form, where the
/// width is checked but not recorded.
pub fn read_float_width(
    raw: &mut Deserializer,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
) -> Result<f64, DeserializeError> {
    read_float_sz_width(raw, min_width, max_width).map(|(value, _)| value)
}
