
// Open TABLE (`t = { * K_t => V_t, * K_r => V_r }`) JSON helpers — the flattened-object face of a
// shape whose entries live in TWO containers. An open table publishes ONE JSON object holding both
// regions: the typed row's entries and the catch-all row's captured entries, at the same level and
// indistinguishable to a reader. The generated type carries a HAND-WRITTEN
// `Serialize`/`Deserialize` pair (never a derive with two `#[serde(flatten)]` members: serde hands
// every unmatched member to BOTH flattened fields on read, and writes both fields' entries into one
// object with no dedup on write — so a derive would silently mis-read and emit duplicate member
// names), and those impls delegate their mechanics here.
//
// The two regions use two different key images, and that is forced rather than accidental. The
// captured region keeps the delivered rest-row convention (`K_r`'s own CBOR bytes: text verbatim,
// uint/nint decimal — `typed_rest_key_string` above). The typed region uses `K_t`'s OWN serde string
// image, which is the CLOSED-table convention: a `{ * K_t => V_t }` table embedded in a struct
// renders through `BTreeMap<K_t, V_t>`'s blanket serde, whose member names are exactly what
// serde_json's map-key serializer makes of `K_t::serialize`. Routing the typed row through the CBOR
// image instead would make `to_json` fail on every bytes-keyed typed row (a bytes key has no CBOR
// member-name image at all) — i.e. on exactly the shape this feature exists for — while the serde
// image is a lowercase-hex string for such a key. It also makes two spellings of one key type
// (a hex-TEXT wire alias and a BYTES wire alias of the same rust type) image IDENTICALLY in JSON,
// which is the property that keeps a versioned wire's JSON stable.
//
// Unlike the rest-row helpers above, these are serde_json-specific by construction (the typed image
// is DEFINED by serde_json's map-key serializer, and the reading by its map-key deserializer), so
// they route through `serde_json::Value` rather than through the caller's serializer.

/// Error from [`open_table_typed_key_string`]: a typed key whose serde rendering is not a JSON
/// object member name (serde_json admits a string, an integer, a bool and a unit variant as map
/// keys, and refuses everything else). Unlike the captured region's image error this one reports a
/// defect in `K_t`'s OWN `Serialize` impl — a bare `bytes` key (`Vec<u8>` → a JSON array) or a
/// derive over a byte array reaches it, a hand impl writing a hex string does not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenTableKeyImageError(pub String);

impl core::fmt::Display for OpenTableKeyImageError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(
            f,
            "open table typed key has no JSON member-name image: {} — a typed row's key images \
             through its own `Serialize`, which must produce a JSON string (or an integer/bool \
             serde_json renders as one)",
            self.0
        )
    }
}

impl core::error::Error for OpenTableKeyImageError {}

/// The JSON object-member name of a TYPED open-table key: exactly what serde_json's map-key
/// serializer makes of `K::serialize`.
///
/// Serializing a ONE-ENTRY map through `serde_json::to_value` and reading the resulting object's
/// single member name back is what makes this the map-key serializer's answer BY CONSTRUCTION,
/// rather than a re-implementation of it that could drift from the closed-table convention it is
/// defined to match.
pub fn open_table_typed_key_string<K: serde::Serialize>(
    key: &K,
) -> Result<String, OpenTableKeyImageError> {
    struct OneEntry<'a, K>(&'a K);
    impl<K: serde::Serialize> serde::Serialize for OneEntry<'_, K> {
        fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            serializer.collect_map(core::iter::once((self.0, ())))
        }
    }
    let value = serde_json::to_value(OneEntry(key))
        .map_err(|e| OpenTableKeyImageError(format!("{e}")))?;
    match value {
        serde_json::Value::Object(members) => members
            .into_iter()
            .next()
            .map(|(name, _)| name)
            .ok_or_else(|| {
                OpenTableKeyImageError(String::from("the key serialized to no member at all"))
            }),
        other => Err(OpenTableKeyImageError(format!(
            "the key serialized to {other} instead of one object member"
        ))),
    }
}

/// The TYPED reading of a JSON object member name: exactly what serde_json's map-key deserializer
/// hands `K::deserialize` for a member of a `BTreeMap<K, V>` — the read twin of
/// [`open_table_typed_key_string`], and the predicate the open table's typed-first partition asks
/// ("does `K_t`'s reading admit this name?").
///
/// Built the same way round: a one-member `serde_json::Value` object, deserialized through a visitor
/// that keeps only the key. So a `K` expecting a string sees the name verbatim, a numeric `K` sees
/// serde_json's decimal parse of it, and a `K` refusing it reports `K`'s OWN error — which is what
/// the three-attempt failure message quotes.
pub fn open_table_typed_key_read<K: serde::de::DeserializeOwned>(name: &str) -> Result<K, String> {
    struct KeyOnly<K>(core::marker::PhantomData<K>);
    impl<'de, K: serde::Deserialize<'de>> serde::de::Visitor<'de> for KeyOnly<K> {
        type Value = K;

        fn expecting(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            f.write_str("a one-member JSON object")
        }

        fn visit_map<A: serde::de::MapAccess<'de>>(self, mut access: A) -> Result<K, A::Error> {
            let key = access
                .next_key::<K>()?
                .ok_or_else(|| serde::de::Error::custom("no member to read the key from"))?;
            let _: serde::de::IgnoredAny = access.next_value()?;
            Ok(key)
        }
    }
    let mut members = serde_json::Map::new();
    members.insert(name.to_owned(), serde_json::Value::Null);
    serde::Deserializer::deserialize_map(
        serde_json::Value::Object(members),
        KeyOnly(core::marker::PhantomData),
    )
    .map_err(|e| format!("{e}"))
}

/// Every reading an open table tried on a member name that NO row admits — the third clause the
/// two-clause [`RestKeyReadError`] cannot carry.
///
/// Both attempts are reported together for the reason the captured region already reports two: a
/// message naming only the LAST reading reads as if the earlier ones had never been tried, and the
/// typed-first partition means the typed attempt is the one a caller most often needs to see.
/// `captured` is whatever the catch-all's own reading reported — for a nominal `K_r` that is a
/// `RestKeyReadError`'s own two clauses, so the rendered message names all three attempts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenTableKeyReadError {
    /// `K_t`'s error on the typed (serde member-name) reading.
    pub typed: String,
    /// The catch-all row's error on its own reading.
    pub captured: String,
}

impl core::fmt::Display for OpenTableKeyReadError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(
            f,
            "as the typed row's key: {}; as the catch-all's key: {}",
            self.typed, self.captured
        )
    }
}

impl core::error::Error for OpenTableKeyReadError {}

/// Serialize an open table's TWO regions into ONE flattened JSON object: the typed row's entries
/// first, then the catch-all's captured entries.
///
/// Collision detection spans BOTH regions — one `BTreeSet` over every member name, filled before any
/// byte is emitted (determinism, and the same strict-fail posture as the rest row's own check per RFC
/// 8949 §6.1). That span is the point: the two regions share one JSON object, so a typed key and a
/// captured key imaging identically would otherwise emit a literally duplicated member name that no
/// reader could partition back. It is also what bounds the value-fixed-point carve-out — a table can
/// never hold both spellings of one member name, so a rebinding on read loses an entry's ROW but
/// never merges two entries.
///
/// There is no `reserved` list here (an open table has zero declared members), so the rest row's
/// declared-name shadow check has nothing to check and is deliberately absent rather than passed an
/// empty slice.
pub fn serialize_open_table<'a, S, Kt, Wt, Et, It, Kr, Wr, Er, Ir>(
    typed_key_to_string: impl Fn(&Kt) -> Result<String, Et>,
    typed_entries: It,
    captured_key_to_string: impl Fn(&Kr) -> Result<String, Er>,
    captured_entries: Ir,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    Wt: serde::Serialize,
    Wr: serde::Serialize,
    Et: core::fmt::Display,
    Er: core::fmt::Display,
    Kt: 'a,
    Kr: 'a,
    It: IntoIterator<Item = (&'a Kt, Wt)>,
    Ir: IntoIterator<Item = (&'a Kr, Wr)>,
{
    use serde::ser::SerializeMap;
    let mut seen = alloc::collections::BTreeSet::new();
    let mut typed_pairs: Vec<(String, Wt)> = Vec::new();
    for (k, w) in typed_entries {
        let ks = typed_key_to_string(k).map_err(serde::ser::Error::custom)?;
        if !seen.insert(ks.clone()) {
            return Err(serde::ser::Error::custom(open_table_key_collision(&ks)));
        }
        typed_pairs.push((ks, w));
    }
    let mut captured_pairs: Vec<(String, Wr)> = Vec::new();
    for (k, w) in captured_entries {
        let ks = captured_key_to_string(k).map_err(serde::ser::Error::custom)?;
        if !seen.insert(ks.clone()) {
            return Err(serde::ser::Error::custom(open_table_key_collision(&ks)));
        }
        captured_pairs.push((ks, w));
    }
    let mut map = serializer.serialize_map(Some(typed_pairs.len() + captured_pairs.len()))?;
    for (ks, w) in &typed_pairs {
        map.serialize_entry(ks, w)?;
    }
    for (ks, w) in &captured_pairs {
        map.serialize_entry(ks, w)?;
    }
    map.end()
}

/// The write-side collision wording, shared by both regions so a same-region and a cross-region
/// collision read alike (they are the same defect: one JSON object cannot carry the name twice).
pub fn open_table_key_collision(name: &str) -> String {
    format!(
        "two open table keys stringify identically to {name:?} — the typed row and the catch-all \
         share ONE JSON object, so their member names must be distinct across both regions"
    )
}

/// The read-side duplicate wording: a JSON object carrying one member name twice. serde_json's
/// parser is last-wins on duplicate members, so an open table detects them itself rather than
/// silently dropping an entry — the read-side counterpart of [`open_table_key_collision`].
pub fn open_table_duplicate_member(name: &str) -> String {
    format!("the open table object carries the member name {name:?} twice")
}

/// The NonEmpty open table's min-1 refusal, in JSON words: an object that bound NO member to the
/// typed row. The bound counts TYPED entries only, so an object full of captured members reaches
/// this — which is the whole reason the wording says so rather than "empty object".
///
/// The CBOR face raises `DeserializeFailure::RangeCheck { found: 0, min: Some(1), max: None }` for
/// the same condition (as does `NonEmptyMap`'s `TryFrom` door). serde's error type is opaque to us,
/// so the JSON door cannot carry that value — it carries the same STATEMENT instead.
pub fn open_table_min_one_typed() -> String {
    "the open table needs at least one TYPED entry (CDDL `{ + k1 => v1, * k2 => v2 }`): no member \
     name bound the typed row"
        .to_owned()
}
