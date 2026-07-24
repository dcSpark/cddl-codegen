// Manual serde `Serialize`/`Deserialize` for `AnyCbor` — a JSON *representation of CBOR*, not
// "natural" JSON. Written ONCE against the mode-independent surface (`kind()`/`as_*` accessors +
// the `new_*` constructors), so this single fragment serves BOTH the preserve and non-preserve
// assemblies (mirroring how `ordered_hash_map_json.rs` is mode-agnostic). Encoding fields NEVER
// appear in JSON — JSON is the deliberately lossy side of the contract (a preserve value survives
// value-equal modulo encodings; a non-preserve value survives exactly for all finite floats).
//
// Rendering: every value is a single-key object whose key is the snake_case kind name:
//   uint       {"uint": 5}                 JSON number (u64 range, as serde_json emits crate-wide)
//   nint       {"nint": -3}                JSON number when the value fits i64,
//              {"nint": "-18446744073709551616"}  else a decimal string (the nint domain exceeds i64)
//   bytes      {"bytes": "a1b2"}           lowercase hex
//   text       {"text": "…"}
//   array      {"array": [ … ]}            recursive
//   map        {"map": [[K, V], …]}        array of pairs — wire order + duplicate keys preserved,
//                                          non-string keys representable
//   tag        {"tag": [11, V]}
//   bool       {"bool": true}
//   null       {"null": null}
//   undefined  {"undefined": null}
//   unassigned {"unassigned": 250}
//   float      {"float": 1.5}              finite floats as numbers;
//              {"float": "NaN"|"Infinity"|"-Infinity"}  non-finite as strings (serde_json cannot
//                                          represent them as numbers). NaN payload bits are NOT
//                                          round-tripped through JSON (the lossy-side charter).
//
// Map-key note (matches the crate-wide non-string-key-table posture, verified 2026-07-23): serde_json
// requires MAP keys to be strings. A `{* uint => any}` / `{* text => any}` table works (uint keys
// stringify, text keys verbatim); a `{* any => any}` (or any non-string-keyed) table serializes each
// key as an OBJECT, so `to_json` errors at runtime with "key must be a string" — exactly as a
// `{* bytes => uint}` table already does today. This is intentional consistency, not a new rule:
// generation accepts such tables and the runtime serde error is the honest signal. The demanded
// shape (`{* uint => any}` metadata tables) is unaffected.
impl serde::Serialize for AnyCbor {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(1))?;
        match self.kind() {
            AnyCborKind::UInt => {
                map.serialize_entry("uint", &self.as_uint().unwrap())?;
            }
            AnyCborKind::NInt => {
                let v = self.as_nint().unwrap();
                match i64::try_from(v) {
                    Ok(n) => map.serialize_entry("nint", &n)?,
                    Err(_) => map.serialize_entry("nint", &v.to_string())?,
                }
            }
            AnyCborKind::Bytes => {
                map.serialize_entry("bytes", &any_cbor_hex_encode(self.as_bytes().unwrap()))?;
            }
            AnyCborKind::Text => {
                map.serialize_entry("text", self.as_text().unwrap())?;
            }
            AnyCborKind::Array => {
                // &[AnyCbor] → JSON array, each element recursing through this impl.
                map.serialize_entry("array", self.as_array().unwrap())?;
            }
            AnyCborKind::Map => {
                // &[(AnyCbor, AnyCbor)] → JSON array of 2-element arrays (each tuple is a seq).
                map.serialize_entry("map", self.as_map().unwrap())?;
            }
            AnyCborKind::Tag => {
                let (tag, inner) = self.as_tag().unwrap();
                // (u64, &AnyCbor) → JSON array [tag, value].
                map.serialize_entry("tag", &(tag, inner))?;
            }
            AnyCborKind::Bool => {
                map.serialize_entry("bool", &self.as_bool().unwrap())?;
            }
            AnyCborKind::Null => {
                map.serialize_entry("null", &())?;
            }
            AnyCborKind::Undefined => {
                map.serialize_entry("undefined", &())?;
            }
            AnyCborKind::Unassigned => {
                map.serialize_entry("unassigned", &self.as_unassigned().unwrap())?;
            }
            AnyCborKind::Float => {
                let f = self.as_float().unwrap();
                if f.is_finite() {
                    map.serialize_entry("float", &f)?;
                } else if f.is_nan() {
                    map.serialize_entry("float", "NaN")?;
                } else if f > 0.0 {
                    map.serialize_entry("float", "Infinity")?;
                } else {
                    map.serialize_entry("float", "-Infinity")?;
                }
            }
        }
        map.end()
    }
}

/// A JSON nint payload: a number (must fit `i64`) or a decimal string (any magnitude in the domain).
#[derive(serde::Deserialize)]
#[serde(untagged)]
enum AnyCborJsonNint {
    Num(i64),
    Str(String),
}

/// A JSON float payload: a finite number, or one of the three non-finite string sentinels.
#[derive(serde::Deserialize)]
#[serde(untagged)]
enum AnyCborJsonFloat {
    Num(f64),
    Str(String),
}

impl<'de> serde::Deserialize<'de> for AnyCbor {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_map(AnyCborJsonVisitor)
    }
}

struct AnyCborJsonVisitor;

impl<'de> serde::de::Visitor<'de> for AnyCborJsonVisitor {
    type Value = AnyCbor;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("a single-key CBOR-tagged JSON object (e.g. {\"uint\": 5})")
    }

    fn visit_map<A: serde::de::MapAccess<'de>>(self, mut access: A) -> Result<AnyCbor, A::Error> {
        use serde::de::Error;
        let key: String = access
            .next_key()?
            .ok_or_else(|| A::Error::custom("expected a single-key AnyCbor object, got {}"))?;
        let value = match key.as_str() {
            "uint" => AnyCbor::new_uint(access.next_value()?),
            "nint" => {
                let raw: i128 = match access.next_value::<AnyCborJsonNint>()? {
                    AnyCborJsonNint::Num(n) => n as i128,
                    AnyCborJsonNint::Str(s) => s
                        .parse::<i128>()
                        .map_err(|e| A::Error::custom(format!("nint decimal string: {e}")))?,
                };
                if !(-(1i128 << 64)..=-1).contains(&raw) {
                    return Err(A::Error::custom(format!(
                        "nint {raw} outside the CBOR nint domain -2^64..=-1"
                    )));
                }
                AnyCbor::new_nint(raw)
            }
            "bytes" => {
                let hex: String = access.next_value()?;
                AnyCbor::new_bytes(any_cbor_hex_decode(&hex).map_err(A::Error::custom)?)
            }
            "text" => AnyCbor::new_text(access.next_value()?),
            "array" => AnyCbor::new_array(access.next_value()?),
            "map" => AnyCbor::new_map(access.next_value()?),
            "tag" => {
                let (tag, inner): (u64, AnyCbor) = access.next_value()?;
                AnyCbor::new_tag(tag, inner)
            }
            "bool" => AnyCbor::new_bool(access.next_value()?),
            "null" => {
                access.next_value::<()>()?;
                AnyCbor::new_null()
            }
            "undefined" => {
                access.next_value::<()>()?;
                AnyCbor::new_undefined()
            }
            "unassigned" => AnyCbor::new_unassigned(access.next_value()?),
            "float" => {
                let f = match access.next_value::<AnyCborJsonFloat>()? {
                    AnyCborJsonFloat::Num(n) => n,
                    AnyCborJsonFloat::Str(s) => match s.as_str() {
                        "NaN" => f64::NAN,
                        "Infinity" => f64::INFINITY,
                        "-Infinity" => f64::NEG_INFINITY,
                        other => {
                            return Err(A::Error::custom(format!(
                                "unrecognized float string {other:?} (want a number or \
                                 \"NaN\"/\"Infinity\"/\"-Infinity\")"
                            )))
                        }
                    },
                };
                AnyCbor::new_float(f)
            }
            other => {
                return Err(A::Error::custom(format!(
                    "unknown AnyCbor kind key {other:?}"
                )))
            }
        };
        if access.next_key::<String>()?.is_some() {
            return Err(A::Error::custom(
                "an AnyCbor object must have exactly one key",
            ));
        }
        Ok(value)
    }
}

// =================================================================================================
// Natural-fallible JSON — the PRIMARY surface every *generated* type uses for an `any`-typed value
// (loose-CBOR Phase B, ruling R1). This is a SEPARATE surface from the tagged codec above: the
// tagged impls stay `AnyCbor`'s own `Serialize`/`Deserialize` (R2, the value-level escape hatch and
// the `AnyCbor` wasm-wrapper codec); generated members / enum arms / newtype wrappers instead route
// through `natural_any_cbor` (the `#[serde(with = …)]` adapter below), which renders the CBOR value
// as the JSON value it *naturally is* — `{ "count": 3 }` rather than `{ "map": [[{"text":"count"},
// {"uint":3}]] }`. Only natural rendering composes with static typing on the read side and only
// natural rendering is what a human-authored `from_json` document looks like.
//
// `to_natural_json` implements RFC 8949 §6.1's INJECTIVE subset and STRICT-FAILS everywhere §6.1
// would substitute a value (bytes, tags, `undefined`, unassigned simples, non-finite floats,
// out-of-i64 nints, complex/colliding map keys). No substitutes, ever: our output feeds a symmetric
// `from_natural_json`, so a silent substitution is write-back corruption. Consequence, by design:
// `to_json` on a generated type that *contains* an `any` is FALLIBLE on data — loudly (the error
// names the offending node kind), with `AnyCbor`'s tagged codec as the value-level escape hatch.
//
// `from_natural_json` is TOTAL (every JSON value has a CBOR home) and implements RFC 8949 §6.2:
// lexically-integral numbers become uint/nint (else float), object keys follow the `any`-domain
// prefer-numeric rule (a text key `"12"` JSON-round-trips to uint `12` — the documented JSON-only
// ambiguity; CBOR is authoritative).

/// Error from [`to_natural_json`]: a CBOR node whose kind has no injective JSON image (RFC 8949
/// §6.1 strict-fail — no substitute values). The message names the offending kind.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnyToNaturalJsonError(pub String);

impl std::fmt::Display for AnyToNaturalJsonError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "AnyCbor value has no natural JSON representation: {}",
            self.0
        )
    }
}

impl std::error::Error for AnyToNaturalJsonError {}

/// The natural-JSON string form of an `any` MAP KEY (used both for a key's object-property name and
/// for collision detection): text verbatim, uint/nint in decimal. Any other kind (bytes, array,
/// map, tag, float, bool, null, undefined, unassigned) has no key image → strict-fail.
pub fn any_cbor_natural_key_string(key: &AnyCbor) -> Result<String, AnyToNaturalJsonError> {
    match key.kind() {
        AnyCborKind::Text => Ok(key.as_text().unwrap().to_owned()),
        AnyCborKind::UInt => Ok(key.as_uint().unwrap().to_string()),
        AnyCborKind::NInt => Ok(key.as_nint().unwrap().to_string()),
        other => Err(AnyToNaturalJsonError(format!(
            "map key of kind {other:?} is not text/uint/nint"
        ))),
    }
}

/// Render an [`AnyCbor`] as the [`serde_json::Value`] it naturally is, or error naming the node kind
/// with no injective JSON image (ruling R3). Big uints (> 2^53) stay JSON numbers (R5, I-JSON
/// precision caveat documented, not a failure).
pub fn to_natural_json(value: &AnyCbor) -> Result<serde_json::Value, AnyToNaturalJsonError> {
    use serde_json::Value;
    match value.kind() {
        AnyCborKind::UInt => Ok(Value::from(value.as_uint().unwrap())),
        AnyCborKind::NInt => {
            // serde_json's number model bottoms out at i64; a nint below i64::MIN has no image.
            let n = value.as_nint().unwrap();
            match i64::try_from(n) {
                Ok(i) => Ok(Value::from(i)),
                Err(_) => Err(AnyToNaturalJsonError(format!(
                    "nint {n} is below i64::MIN (no JSON number image)"
                ))),
            }
        }
        AnyCborKind::Text => Ok(Value::from(value.as_text().unwrap().to_owned())),
        AnyCborKind::Bool => Ok(Value::from(value.as_bool().unwrap())),
        AnyCborKind::Null => Ok(Value::Null),
        AnyCborKind::Float => {
            let f = value.as_float().unwrap();
            // serde_json cannot hold non-finite numbers; NaN/±Inf strict-fail (R6: JSON is lossy for
            // NaN payloads anyway; here the whole non-finite value has no number image).
            serde_json::Number::from_f64(f)
                .map(Value::Number)
                .ok_or_else(|| {
                    AnyToNaturalJsonError(format!("non-finite float {f} has no JSON number image"))
                })
        }
        AnyCborKind::Array => {
            let arr = value.as_array().unwrap();
            let mut out = Vec::with_capacity(arr.len());
            for elem in arr {
                out.push(to_natural_json(elem)?);
            }
            Ok(Value::Array(out))
        }
        AnyCborKind::Map => {
            let pairs = value.as_map().unwrap();
            let mut obj = serde_json::Map::new();
            // Determinism + collision detection: a `BTreeSet` of stringified keys. Two keys that
            // stringify identically (uint `12` + text `"12"`, or two equal keys) are a collision →
            // strict-fail (§6.1's "danger of key collision"): our JSON feeds a symmetric read.
            let mut seen = std::collections::BTreeSet::new();
            for (key, val) in pairs {
                let key_string = any_cbor_natural_key_string(key)?;
                if !seen.insert(key_string.clone()) {
                    return Err(AnyToNaturalJsonError(format!(
                        "map key {key_string:?} stringifies identically to an earlier key"
                    )));
                }
                obj.insert(key_string, to_natural_json(val)?);
            }
            Ok(Value::Object(obj))
        }
        AnyCborKind::Bytes => Err(AnyToNaturalJsonError("bytes".into())),
        AnyCborKind::Tag => Err(AnyToNaturalJsonError("tag".into())),
        AnyCborKind::Undefined => Err(AnyToNaturalJsonError("undefined".into())),
        AnyCborKind::Unassigned => Err(AnyToNaturalJsonError("unassigned simple value".into())),
    }
}

/// The `any`-domain reading of a JSON object key (ruling R4): prefer the numeric reading for a
/// CANONICAL decimal spelling (round-trips through `to_string`), else text. So `"12"` → uint `12`,
/// `"-5"` → nint `-5`, but `"012"`/`"+5"`/`"5.0"`/`"abc"` → text. Total.
pub fn any_cbor_natural_key_from_string(key: &str) -> AnyCbor {
    // `.ok().filter(round-trips)` keeps this a single `if let` (no nested-if / no let-chain, so it
    // stays clippy-clean AND edition-agnostic in the generated crate). A canonical decimal spelling
    // round-trips through `to_string`; `"012"`/`"+3"`/`"-0"`/non-round-tripping forms fall to text.
    if let Some(u) = key.parse::<u64>().ok().filter(|u| u.to_string() == key) {
        return AnyCbor::new_uint(u);
    }
    // Keys are STRINGS, so a nint key has no `i64` ceiling (unlike a nint VALUE, bounded by
    // serde_json's number model): parse the whole CBOR nint domain (-2^64..=-1) as `i128`, matching
    // what `any_cbor_natural_key_string` writes. The domain check subsumes the sign check.
    if let Some(i) = key
        .parse::<i128>()
        .ok()
        .filter(|i| (-(1i128 << 64)..=-1).contains(i) && i.to_string() == key)
    {
        return AnyCbor::new_nint(i);
    }
    AnyCbor::new_text(key.to_owned())
}

/// The reverse of [`to_natural_json`]: every JSON value has a CBOR home (ruling R4, RFC 8949 §6.2).
/// TOTAL — never fails. Lexically-integral numbers become uint (non-negative) / nint (negative),
/// anything else (fractional / out-of-i64-magnitude) becomes a float; object keys use the
/// `any`-domain prefer-numeric rule.
pub fn from_natural_json(value: serde_json::Value) -> AnyCbor {
    use serde_json::Value;
    match value {
        Value::Null => AnyCbor::new_null(),
        Value::Bool(b) => AnyCbor::new_bool(b),
        Value::Number(n) => {
            if let Some(u) = n.as_u64() {
                AnyCbor::new_uint(u)
            } else if let Some(i) = n.as_i64() {
                // as_i64 with a non-u64 number is negative → in the nint domain.
                AnyCbor::new_nint(i as i128)
            } else {
                // Not lexically integral (has a fraction/exponent, or out of i64 magnitude).
                AnyCbor::new_float(n.as_f64().expect("serde_json number is u64/i64/f64"))
            }
        }
        Value::String(s) => AnyCbor::new_text(s),
        Value::Array(arr) => {
            AnyCbor::new_array(arr.into_iter().map(from_natural_json).collect())
        }
        Value::Object(obj) => AnyCbor::new_map(
            obj.into_iter()
                .map(|(k, v)| (any_cbor_natural_key_from_string(&k), from_natural_json(v)))
                .collect(),
        ),
    }
}

/// `#[serde(with = "…::natural_any_cbor")]` adapter: the serde face a generated type puts on an
/// `any`-typed field/arm so it renders NATURALLY (not through `AnyCbor`'s tagged codec). Serialize
/// is fallible-on-data (R3's failure set surfaces as a serde error naming the node kind); deserialize
/// is total (R4).
pub mod natural_any_cbor {
    use super::{from_natural_json, to_natural_json, AnyCbor};

    pub fn serialize<S>(value: &AnyCbor, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let json = to_natural_json(value).map_err(serde::ser::Error::custom)?;
        serde::Serialize::serialize(&json, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<AnyCbor, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let json = <serde_json::Value as serde::Deserialize>::deserialize(deserializer)?;
        Ok(from_natural_json(json))
    }
}

/// The `Option<AnyCbor>` companion to [`natural_any_cbor`], for an OPTIONAL `any` member (`? N: any`
/// → `Option<AnyCbor>`). A generated optional field pairs this with `#[serde(default)]` so a missing
/// key reads back as `None`, matching the derive's ordinary optional handling.
pub mod natural_any_cbor_opt {
    use super::{from_natural_json, to_natural_json, AnyCbor};

    pub fn serialize<S>(value: &Option<AnyCbor>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match value {
            Some(v) => {
                let json = to_natural_json(v).map_err(serde::ser::Error::custom)?;
                serializer.serialize_some(&json)
            }
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<AnyCbor>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let opt = <Option<serde_json::Value> as serde::Deserialize>::deserialize(deserializer)?;
        Ok(opt.map(from_natural_json))
    }
}

// A serde-only view of an `AnyCbor` that renders NATURALLY. Used purely to let serde's own
// container handling (`Vec`/`BTreeMap`/`OrderedHashMap` serde) walk the collection element-wise —
// this is serde composition, NOT a parallel JSON path (serde drives the container; the wrapper only
// swaps the per-element codec from tagged to natural).
pub struct NaturalAnyCborSer<'a>(pub &'a AnyCbor);

impl serde::Serialize for NaturalAnyCborSer<'_> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        natural_any_cbor::serialize(self.0, serializer)
    }
}

pub struct NaturalAnyCborDe(pub AnyCbor);

impl<'de> serde::Deserialize<'de> for NaturalAnyCborDe {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        natural_any_cbor::deserialize(deserializer).map(NaturalAnyCborDe)
    }
}

/// `#[serde(with = …)]` adapter for a `Vec<AnyCbor>` member (homogeneous `[* any]` array as a
/// struct field), rendering each element naturally (R1). Rides serde's own seq handling.
pub mod natural_any_cbor_seq {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};

    pub fn serialize<S>(value: &[AnyCbor], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(NaturalAnyCborSer))
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<AnyCbor>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elems = <Vec<NaturalAnyCborDe> as serde::Deserialize>::deserialize(deserializer)?;
        Ok(elems.into_iter().map(|e| e.0).collect())
    }
}

/// `#[serde(with = …)]` adapter for a `BTreeMap<K, AnyCbor>` member (a `{* K => any}` table with a
/// stringifiable key as a struct field, non-preserve). The KEY renders through its own serde (the
/// crate-wide table-key posture — a non-stringifiable `any` key still errors at runtime, ruling R3);
/// only the VALUE flips to natural. Generic over `K` so one module serves every key type.
pub mod natural_any_cbor_btreemap {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use std::collections::BTreeMap;

    pub fn serialize<K, S>(value: &BTreeMap<K, AnyCbor>, serializer: S) -> Result<S::Ok, S::Error>
    where
        K: serde::Serialize,
        S: serde::Serializer,
    {
        serializer.collect_map(value.iter().map(|(k, v)| (k, NaturalAnyCborSer(v))))
    }

    pub fn deserialize<'de, K, D>(deserializer: D) -> Result<BTreeMap<K, AnyCbor>, D::Error>
    where
        K: serde::Deserialize<'de> + Ord,
        D: serde::Deserializer<'de>,
    {
        let map =
            <BTreeMap<K, NaturalAnyCborDe> as serde::Deserialize>::deserialize(deserializer)?;
        Ok(map.into_iter().map(|(k, v)| (k, v.0)).collect())
    }
}

/// `#[serde(with = …)]` adapter for an OPTIONAL homogeneous-array member (`? N: [* any]` →
/// `Option<Vec<AnyCbor>>`), paired with `#[serde(default)]`. `None` → JSON null / missing.
pub mod natural_any_cbor_opt_seq {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};

    pub fn serialize<S>(value: &Option<Vec<AnyCbor>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match value {
            Some(v) => {
                let wrapped: Vec<NaturalAnyCborSer> = v.iter().map(NaturalAnyCborSer).collect();
                serializer.serialize_some(&wrapped)
            }
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<Vec<AnyCbor>>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let opt =
            <Option<Vec<NaturalAnyCborDe>> as serde::Deserialize>::deserialize(deserializer)?;
        Ok(opt.map(|v| v.into_iter().map(|e| e.0).collect()))
    }
}

/// `#[serde(with = …)]` adapter for an OPTIONAL non-preserve table member (`? N: {* K => any}` →
/// `Option<BTreeMap<K, AnyCbor>>`), paired with `#[serde(default)]`.
pub mod natural_any_cbor_opt_btreemap {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use std::collections::BTreeMap;

    pub fn serialize<K, S>(
        value: &Option<BTreeMap<K, AnyCbor>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        K: serde::Serialize + Ord,
        S: serde::Serializer,
    {
        match value {
            Some(m) => {
                let wrapped: BTreeMap<&K, NaturalAnyCborSer> =
                    m.iter().map(|(k, v)| (k, NaturalAnyCborSer(v))).collect();
                serializer.serialize_some(&wrapped)
            }
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, K, D>(deserializer: D) -> Result<Option<BTreeMap<K, AnyCbor>>, D::Error>
    where
        K: serde::Deserialize<'de> + Ord,
        D: serde::Deserializer<'de>,
    {
        let opt = <Option<BTreeMap<K, NaturalAnyCborDe>> as serde::Deserialize>::deserialize(
            deserializer,
        )?;
        Ok(opt.map(|m| m.into_iter().map(|(k, v)| (k, v.0)).collect()))
    }
}

/// Lowercase hex of a byte slice (self-contained — the `hex` crate is not always a dep here).
fn any_cbor_hex_encode(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push(char::from_digit((b >> 4) as u32, 16).unwrap());
        s.push(char::from_digit((b & 0x0f) as u32, 16).unwrap());
    }
    s
}

/// Parse lowercase/uppercase hex into bytes; errors on odd length or a non-hex nibble.
fn any_cbor_hex_decode(s: &str) -> Result<Vec<u8>, String> {
    if s.len() & 1 == 1 {
        return Err(format!("odd-length hex string (len {})", s.len()));
    }
    let bytes = s.as_bytes();
    (0..bytes.len())
        .step_by(2)
        .map(|i| {
            let hi = (bytes[i] as char)
                .to_digit(16)
                .ok_or_else(|| format!("invalid hex nibble {:?}", bytes[i] as char))?;
            let lo = (bytes[i + 1] as char)
                .to_digit(16)
                .ok_or_else(|| format!("invalid hex nibble {:?}", bytes[i + 1] as char))?;
            Ok(((hi << 4) | lo) as u8)
        })
        .collect()
}
