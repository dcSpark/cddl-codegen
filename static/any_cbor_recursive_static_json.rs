// The recursive exact-array descriptor lives in `static_array`, which is intentionally absent
// from a plain-`any` crate. Keep this natural leaf implementation in its own conditionally
// assembled fragment so ordinary AnyCbor JSON remains independent of static-array support.
impl super::static_array::RecursiveSerialize<AnyCbor> for super::static_array::NaturalAny {
    fn serialize<S>(value: &AnyCbor, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        natural_any_cbor::serialize(value, serializer)
    }
}

impl super::static_array::RecursiveDeserialize<AnyCbor> for super::static_array::NaturalAny {
    fn deserialize<'de, D>(deserializer: D) -> Result<AnyCbor, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        natural_any_cbor::deserialize(deserializer)
    }
}
