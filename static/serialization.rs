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

/// Rank of a CBOR float WIDTH in width order (2 < 4 < 8 bytes, RFC 8949 §3.3). It ranks the width
/// of a VALUE's shortest lossless form against the window a CDDL float class spans — never the head
/// a value happened to arrive under. The float-invalid `Sz::Inline`/`Sz::One` — which neither
/// `smallest_float_sz` nor a `float_sz()` read produces, but a hand-built encoding struct can hold —
/// sort with the widest width, so they can never pass a window check the widest width itself would
/// not.
fn float_width_rank(sz: cbor_event::Sz) -> u8 {
    match sz {
        cbor_event::Sz::Two => 0,
        cbor_event::Sz::Four => 1,
        _ => 2,
    }
}

/// Whether `value` belongs to a CDDL float class spanning the widths `min_width..=max_width`, and
/// therefore the width it writes at — the two are one question.
///
/// The CDDL float prelude names constrain VALUES, not encodings: RFC 8610 §2.2.3 says the `#7.x`
/// notation "is about a set of values at the data model level … it does not mandate that these
/// values also do have to be serialized as half-precision floats: CDDL does not provide any language
/// means to restrict the choice of serialization variants", and §3.3 defines `float32` as "a number
/// representable as a single-precision float". The six names PARTITION the float values by their
/// shortest lossless form: `float16` is the values whose shortest form is `#7.25`, `float32` `#7.26`,
/// `float64` `#7.27`, with `float16-32`/`float32-64` spanning two and `float` all three. So
/// membership is `smallest_float_sz(value)` landing inside the class's window, and the classes are
/// DISJOINT — `1.5` is a `float16` and not a `float32`, whatever head it arrived under.
///
/// The width a MEMBER writes at is exactly that shortest form (RFC 8949 §4.1 preferred
/// serialization), which is its declared width by construction: membership IS "shortest form lands
/// in the window". A NON-member — `1.5` assigned to a `float32` field, say — has no width to write
/// at and fails LOUDLY here rather than being encoded, because a wider head would emit bytes this
/// crate's own decoder rejects and a narrower one would round the value.
///
/// The failure is `cbor_event::Error::InvalidLenPassed` carrying the width the value REQUIRES, which
/// is the actionable fact and the one the class does not admit. That error rather than a
/// message-carrying `CustomError`: this is a `core`-only path (a `String` would pull in `alloc`), and
/// it is the same error identity the lossy-narrowing case has always failed with.
pub fn float_class_width(
    value: f64,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
) -> cbor_event::Result<cbor_event::Sz> {
    let smallest = cbor_event::se::smallest_float_sz(value);
    if float_width_rank(smallest) < float_width_rank(min_width)
        || float_width_rank(smallest) > float_width_rank(max_width)
    {
        return Err(cbor_event::Error::InvalidLenPassed(smallest));
    }
    Ok(smallest)
}

/// Exact `f64` -> `f32` narrowing for a value a class-membership check has ALREADY proven fits:
/// every value an `f32`-carried CDDL float class admits has a shortest lossless form of `#7.25` or
/// `#7.26` and is therefore f32-exact by construction, so the miss arm is unreachable. It panics
/// rather than erroring because reaching it would mean the membership check above it is wrong — a
/// generator bug, not malformed input.
///
/// Never an `as` cast: that rounds silently, and its NaN-payload behaviour is not merely
/// platform-dependent — LLVM const-folds the conversion to a canonical quiet NaN, so `as` can differ
/// between the const-evaluated and runtime paths of ONE binary. The widening direction goes through
/// `cbor_event::se::f32_to_f64_exact` for the same reason.
pub fn narrow_f32(value: f64) -> f32 {
    cbor_event::se::f64_to_f32_exact(value)
        .expect("a value an f32-carried CDDL float class admits is f32-exact by construction")
}

/// Read a float belonging to a CDDL class spanning the widths `min_width..=max_width` (`float32` is
/// `#7.26` alone; `float16-32` is `#7.25`/`#7.26`; `float` spans all three).
///
/// ANY float head is accepted — `f9`, `fa` and `fb` alike — and the decoded VALUE is then tested for
/// membership (see [`float_class_width`]). Acceptance cannot key on the head: a CDDL float name is a
/// set of values, and RFC 8949 §4.1 preferred serialization lets any conforming encoder write `1.5`
/// as `f9 3e00`, so a head-strict reader would reject its own class's canonical bytes. The returned
/// `sz` is the real WIRE width, which is what `--preserve-encodings` replays; it is deliberately not
/// constrained to the window, since an `fb`-headed `1.5` is a `float16` whose byte-exact round-trip
/// needs the `fb` back.
///
/// A non-member is a decode ERROR reporting the width the VALUE requires — never a silent widening
/// or narrowing, which would make a `float32` member's decoded value depend on a value its own type
/// excludes.
pub fn read_float_sz_width(
    raw: &mut Deserializer,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
) -> Result<(f64, cbor_event::Sz), DeserializeError> {
    let (value, sz) = raw.float_sz()?;
    let smallest = cbor_event::se::smallest_float_sz(value);
    if float_width_rank(smallest) < float_width_rank(min_width)
        || float_width_rank(smallest) > float_width_rank(max_width)
    {
        return Err(DeserializeFailure::FloatWidth {
            found: smallest,
            min: min_width,
            max: max_width,
        }
        .into());
    }
    Ok((value, sz))
}

/// [`read_float_sz_width`] dropping the wire width — the non-`--preserve-encodings` form, where the
/// value's class membership is checked but no width is recorded.
pub fn read_float_width(
    raw: &mut Deserializer,
    min_width: cbor_event::Sz,
    max_width: cbor_event::Sz,
) -> Result<f64, DeserializeError> {
    read_float_sz_width(raw, min_width, max_width).map(|(value, _)| value)
}
