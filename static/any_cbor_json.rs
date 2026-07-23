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
