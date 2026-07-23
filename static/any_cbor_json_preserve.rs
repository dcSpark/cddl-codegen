// Preserve-only natural-JSON companions for `any`-valued map MEMBERS. Assembled ONLY under
// `--preserve-encodings` + `--json-serde-derives`, appended right after `any_cbor_json.rs`, so both
// `OrderedHashMap` (always present under `--preserve-encodings`) and the `NaturalAnyCbor{Ser,De}`
// serde-only wrappers (from `any_cbor_json.rs`) are in scope. These are the preserve counterparts of
// `natural_any_cbor_btreemap` / `natural_any_cbor_opt_btreemap`: a `{* K => any}` table member under
// preserve is an `OrderedHashMap<K, AnyCbor>`. Key ordering mirrors `ordered_hash_map_json.rs`'s own
// sort-into-`BTreeMap` serde so a preserve map member and its non-preserve equivalent render the same
// JSON. Rides serde's own container handling — not a parallel JSON path.
use super::ordered_hash_map::OrderedHashMap;

/// `#[serde(with = …)]` adapter for a preserve `{* K => any}` member (`OrderedHashMap<K, AnyCbor>`).
pub mod natural_any_cbor_orderedmap {
    use super::OrderedHashMap;
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use std::collections::BTreeMap;

    pub fn serialize<K, S>(
        value: &OrderedHashMap<K, AnyCbor>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        K: serde::Serialize + std::hash::Hash + Eq + Ord,
        S: serde::Serializer,
    {
        let sorted: BTreeMap<&K, NaturalAnyCborSer> =
            value.iter().map(|(k, v)| (k, NaturalAnyCborSer(v))).collect();
        serde::Serialize::serialize(&sorted, serializer)
    }

    pub fn deserialize<'de, K, D>(deserializer: D) -> Result<OrderedHashMap<K, AnyCbor>, D::Error>
    where
        K: serde::Deserialize<'de> + std::hash::Hash + Eq + Ord,
        D: serde::Deserializer<'de>,
    {
        let map =
            <BTreeMap<K, NaturalAnyCborDe> as serde::Deserialize>::deserialize(deserializer)?;
        Ok(map.into_iter().map(|(k, v)| (k, v.0)).collect())
    }
}

/// `#[serde(with = …)]` adapter for an OPTIONAL preserve table member
/// (`? N: {* K => any}` → `Option<OrderedHashMap<K, AnyCbor>>`), paired with `#[serde(default)]`.
pub mod natural_any_cbor_opt_orderedmap {
    use super::OrderedHashMap;
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use std::collections::BTreeMap;

    pub fn serialize<K, S>(
        value: &Option<OrderedHashMap<K, AnyCbor>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        K: serde::Serialize + std::hash::Hash + Eq + Ord,
        S: serde::Serializer,
    {
        match value {
            Some(m) => {
                let sorted: BTreeMap<&K, NaturalAnyCborSer> =
                    m.iter().map(|(k, v)| (k, NaturalAnyCborSer(v))).collect();
                serializer.serialize_some(&sorted)
            }
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, K, D>(
        deserializer: D,
    ) -> Result<Option<OrderedHashMap<K, AnyCbor>>, D::Error>
    where
        K: serde::Deserialize<'de> + std::hash::Hash + Eq + Ord,
        D: serde::Deserializer<'de>,
    {
        let opt = <Option<BTreeMap<K, NaturalAnyCborDe>> as serde::Deserialize>::deserialize(
            deserializer,
        )?;
        Ok(opt.map(|m| m.into_iter().map(|(k, v)| (k, v.0)).collect()))
    }
}
