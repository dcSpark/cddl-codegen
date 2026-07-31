// Open struct-map rest FLATTEN helpers. An open struct's
// captured unknown entries render at the SAME JSON object level as the declared fields (serde
// `flatten` merges the rest map into the parent object), and read back symmetrically (declared field
// names bind first, every other key lands in rest). The struct keeps its derive; the rest field
// carries `#[serde(flatten, serialize_with = …, deserialize_with = …)]` pointing at GENERATED
// per-struct wrappers that close over the declared JSON names (for the write-side collision check)
// and the key domain, delegating the mechanics to the two helpers below.
//
// These are `any`-free by construction (generic over the key type, the key-stringify closure's error,
// and the value view), so a fully-typed rest row (`* uint => text`, no `any` anywhere) uses them
// without pulling in the `AnyCbor` runtime. The `any`-domain wrappers supply the natural key/value
// views from `any_cbor.rs`; the typed-domain wrappers supply plain closures / the value's own serde.

/// Serialize an open struct-map's rest entries FLATTENED. `reserved` is the declared fields' JSON
/// names. Errors (no silent duplicate/shadow, per RFC 8949 §6.1's strict-fail): a rest key equal to a declared name (would shadow
/// it, and most JSON parsers are last-wins); two rest keys that stringify identically (this is how a
/// `@duplicates preserve` PairMap rest holding ACTUAL duplicate keys makes `to_json` fail —
/// duplicates stringify identically by definition); a key whose string form does not exist (a complex
/// `any` key — bytes/array/map/tag/float — surfaced as the `key_to_string` closure's `Err`). Collision
/// detection is over ALL keys before any byte is emitted (a `BTreeSet` — determinism).
pub fn serialize_flattened_rest<'a, S, K, W, E, I>(
    reserved: &[&str],
    key_to_string: impl Fn(&K) -> Result<String, E>,
    entries: I,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    W: serde::Serialize,
    E: core::fmt::Display,
    K: 'a,
    I: IntoIterator<Item = (&'a K, W)>,
{
    use serde::ser::SerializeMap;
    let mut pairs: Vec<(String, W)> = Vec::new();
    let mut seen = alloc::collections::BTreeSet::new();
    for (k, w) in entries {
        let ks = key_to_string(k).map_err(serde::ser::Error::custom)?;
        if reserved.contains(&ks.as_str()) {
            return Err(serde::ser::Error::custom(format!(
                "open struct-map rest key {ks:?} stringifies to a declared field's JSON name \
                 (would shadow the declared field)"
            )));
        }
        if !seen.insert(ks.clone()) {
            return Err(serde::ser::Error::custom(format!(
                "two open struct-map rest keys stringify identically to {ks:?}"
            )));
        }
        pairs.push((ks, w));
    }
    let mut map = serializer.serialize_map(Some(pairs.len()))?;
    for (ks, w) in &pairs {
        map.serialize_entry(ks, w)?;
    }
    map.end()
}

/// Error from [`typed_rest_key_string`]: a typed rest key whose CBOR head is not uint/nint/text has
/// no JSON member-name image, so `to_json` strict-fails (RFC 8949 §6.1 — no substitutes). The message
/// names the offending CBOR major type, mirroring the `any` domain's key-image error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestKeyImageError(pub String);

impl core::fmt::Display for RestKeyImageError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(
            f,
            "open struct-map rest key has no JSON member-name image: {}",
            self.0
        )
    }
}

impl core::error::Error for RestKeyImageError {}

/// The JSON object-member name of a TYPED rest key, read off the key's own CBOR bytes: the
/// `any`-domain convention (`any_cbor_natural_key_string`) applied to `K`'s WIRE image — text
/// verbatim, uint/nint in decimal, every other major type strict-fails naming its kind (a tag is
/// refused here exactly as the `any` domain refuses it).
///
/// Only the HEAD is read, so a composite key costs nothing to reject. Indefinite-length (chunked)
/// text concatenates (`cbor_event`'s `text` reads every chunk), which is what makes the image
/// encoding-INDEPENDENT: a preserve-flavored key replaying a chunked encoding images to the same
/// member name as its definite twin, so the write side's collision check compares VALUES rather than
/// bytes. Trailing bytes are not checked — the bytes come from `K`'s own serializer.
pub fn typed_rest_key_string(key_cbor: &[u8]) -> Result<String, RestKeyImageError> {
    let mut raw = cbor_event::de::Deserializer::from(key_cbor.to_vec());
    let cbor_err = |e: cbor_event::Error| RestKeyImageError(format!("{e}"));
    match raw.cbor_type().map_err(cbor_err)? {
        cbor_event::Type::UnsignedInteger => {
            Ok(raw.unsigned_integer().map_err(cbor_err)?.to_string())
        }
        // `negative_integer_sz` reads the full CBOR nint domain as `i128` (-2^64..=-1), matching what
        // the `any` domain's key image writes — a key is a STRING, so there is no `i64` ceiling here.
        cbor_event::Type::NegativeInteger => {
            Ok(raw.negative_integer_sz().map_err(cbor_err)?.0.to_string())
        }
        cbor_event::Type::Text => raw.text().map_err(cbor_err),
        other => Err(RestKeyImageError(format!(
            "map key of CBOR kind {other:?} is not text/uint/nint"
        ))),
    }
}

/// The member-name image of a CDDL `nint`-domain rest key. The rust member of a `nint` holds the
/// nint's ENCODED ARGUMENT (`value = -1 - arg`), so its own decimal is NOT the key's image — the wire
/// value's is, which is what a reader of the JSON sees and what the `any` domain would have written
/// for the same key.
pub fn nint_arg_key_string(arg: u64) -> String {
    (-1i128 - arg as i128).to_string()
}

/// Reverse of [`nint_arg_key_string`]: the `nint` rust member a member name denotes, or `None` when
/// the name is not a canonical decimal spelling inside the CBOR nint domain (`-2^64..=-1`).
pub fn nint_arg_key_from_string(key: &str) -> Option<u64> {
    key.parse::<i128>()
        .ok()
        .filter(|i| (-(1i128 << 64)..=-1).contains(i) && i.to_string() == key)
        .map(|i| (-(i + 1)) as u64)
}

/// Canonical CBOR of the NUMERIC reading of a JSON member name, if the name is a canonical decimal
/// spelling; `None` otherwise. The filters are the `any` domain's
/// (`any_cbor_natural_key_from_string`) so the two key conventions admit exactly the same numeric
/// names: `"012"`, `"+5"`, `"-0"` and `"5.0"` all fall through to the text reading.
pub fn numeric_key_cbor(key: &str) -> Option<Vec<u8>> {
    let mut buf = cbor_event::se::Serializer::new_vec();
    if let Some(u) = key.parse::<u64>().ok().filter(|u| u.to_string() == key) {
        buf.write_unsigned_integer(u).unwrap();
        return Some(buf.finalize());
    }
    if let Some(i) = key
        .parse::<i128>()
        .ok()
        .filter(|i| (-(1i128 << 64)..=-1).contains(i) && i.to_string() == key)
    {
        // the nint's encoded argument is `-1 - value`, which is what fixes the canonical head width
        let arg = (-(i + 1)) as u64;
        buf.write_negative_integer_sz(i, cbor_event::Sz::canonical(arg))
            .unwrap();
        return Some(buf.finalize());
    }
    None
}

/// Canonical CBOR of the TEXT reading of a JSON member name. Always exists.
pub fn text_key_cbor(key: &str) -> Vec<u8> {
    let mut buf = cbor_event::se::Serializer::new_vec();
    buf.write_text(key).unwrap();
    buf.finalize()
}

/// Both readings of a member name that `K` refused. Reported together so a uint-only `K` handed
/// `"abc"` does not read as if the numeric reading had never been tried.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestKeyReadError {
    /// `K`'s error on the numeric reading, or `None` when the name is not a canonical decimal
    /// spelling (so there was no numeric reading to try).
    pub numeric: Option<String>,
    /// `K`'s error on the text reading.
    pub text: String,
}

impl core::fmt::Display for RestKeyReadError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match &self.numeric {
            Some(e) => write!(f, "as uint/nint: {e}; as text: {}", self.text),
            None => write!(
                f,
                "as uint/nint: the member name is not a canonical decimal spelling; as text: {}",
                self.text
            ),
        }
    }
}

impl core::error::Error for RestKeyReadError {}

/// The TYPED-domain reading of a JSON object member name (RFC 8949 §6.2): prefer the numeric
/// reading, and FALL BACK to the text reading when `K` refuses it. `from_cbor` is `K`'s own CBOR
/// decoder, supplied by the generated wrapper.
///
/// The fallback is what makes the JSON fixed point TOTAL: without it a text-only `K` holding the key
/// `"12"` writes a member name our own reader would reject (`K` refuses uint `12`), i.e. `to_json`
/// could emit a document `from_json` refuses. The rule stays deterministic (a pure function of the
/// name and `K`'s decoder) and MONOTONE (numeric-first is unchanged, so every key the strict rule
/// bound is bound identically — a `K` admitting both readings still takes the numeric one, which is
/// the `any` domain's documented `"12"`-rebinds-as-uint ambiguity, unchanged here). A name both
/// readings refuse is a hard parse error, never a capture.
pub fn rest_key_from_string<K, E: core::fmt::Display>(
    key: &str,
    from_cbor: impl Fn(&[u8]) -> Result<K, E>,
) -> Result<K, RestKeyReadError> {
    let numeric = match numeric_key_cbor(key) {
        Some(bytes) => match from_cbor(&bytes) {
            Ok(k) => return Ok(k),
            Err(e) => Some(format!("{e}")),
        },
        None => None,
    };
    from_cbor(&text_key_cbor(key)).map_err(|e| RestKeyReadError {
        numeric,
        text: format!("{e}"),
    })
}

/// Read the flattened rest entries serde's `flatten` buffering hands back as a MAP of string keys (it
/// never applies serde_json's numeric-key coercion, verified against current serde). Returns the raw
/// `(String, value)` pairs; the GENERATED wrapper then coerces each key to
/// the domain type per RFC 8949 §6.2 (typed domains parse per the domain type; `any` domains prefer the
/// numeric reading) and unwraps the value view. A duplicate declared field name in the input is
/// already rejected loudly upstream by serde's own flatten machinery (the read-side collision guard).
pub fn read_flattened_rest_pairs<'de, D, VDe>(
    deserializer: D,
) -> Result<Vec<(String, VDe)>, D::Error>
where
    D: serde::Deserializer<'de>,
    VDe: serde::Deserialize<'de>,
{
    struct Vis<VDe>(core::marker::PhantomData<VDe>);
    impl<'de, VDe: serde::Deserialize<'de>> serde::de::Visitor<'de> for Vis<VDe> {
        type Value = Vec<(String, VDe)>;
        fn expecting(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            f.write_str("a map of open struct-map rest entries")
        }
        fn visit_map<M: serde::de::MapAccess<'de>>(
            self,
            mut access: M,
        ) -> Result<Self::Value, M::Error> {
            let mut out = Vec::new();
            while let Some((k, v)) = access.next_entry::<String, VDe>()? {
                out.push((k, v));
            }
            Ok(out)
        }
    }
    deserializer.deserialize_map(Vis(core::marker::PhantomData))
}
