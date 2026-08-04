//! The serde face of an OPTIONAL member whose own type is NULLABLE (`? f: (T / null)`), whose rust
//! member is therefore a nested `Option<Option<T>>`. Used as `#[serde(with = "…::double_option")]`,
//! so this MODULE is the adapter — its two functions are serde's `with` pair.
//!
//! WHY THIS EXISTS. serde's plain derive cannot tell the two `Option`s apart, in either direction.
//! Reading, `Option<Option<T>>`'s own `Deserialize` maps a JSON `null` to the OUTER `None` — so a
//! present-but-null field reads back as ABSENT and the inner null is gone. Writing, the outer `None`
//! (absent) renders as `null` — so absent and present-null produce the SAME JSON text. The CBOR
//! surface distinguishes all three states, so the plain derive leaves the two decode surfaces
//! disagreeing about one value.
//!
//! THE CONVENTION. Paired with `#[serde(default)]` (a missing key supplies the outer `None` — a
//! `#[serde(with)]` field is otherwise REQUIRED on read) and
//! `#[serde(skip_serializing_if = "Option::is_none")]` (absent omits the key rather than writing
//! `null`), this adapter makes the JSON surface carry every state the CBOR surface does:
//!
//! | state          | rust            | JSON        |
//! |----------------|-----------------|-------------|
//! | absent         | `None`          | key OMITTED |
//! | present, null  | `Some(None)`    | `"f": null` |
//! | present, value | `Some(Some(v))` | `"f": v`    |
//!
//! Generic over the inner type, so one module serves every such member; `alloc`-clean (serde only).

pub fn serialize<T, S>(value: &Option<Option<T>>, serializer: S) -> Result<S::Ok, S::Error>
where
    T: serde::Serialize,
    S: serde::Serializer,
{
    // `skip_serializing_if` means the outer `None` never reaches here. Writing it as `null` anyway
    // keeps the adapter correct on its own terms (a hand caller pairing it differently gets the
    // plain-derive rendering rather than a panic or an error).
    match value {
        Some(inner) => serde::Serialize::serialize(inner, serializer),
        None => serializer.serialize_none(),
    }
}

pub fn deserialize<'de, T, D>(deserializer: D) -> Result<Option<Option<T>>, D::Error>
where
    T: serde::Deserialize<'de>,
    D: serde::Deserializer<'de>,
{
    // serde only calls this for a key that IS present, so a `null` here is the INNER null and the
    // outer `Option` is `Some`. A missing key never reaches this fn — `#[serde(default)]` supplies
    // the outer `None` for it.
    <Option<T> as serde::Deserialize<'de>>::deserialize(deserializer).map(Some)
}
