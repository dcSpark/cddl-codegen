impl<T: serde::Serialize> serde::Serialize for NonEmptyVec<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // a NonEmptyVec is a plain JSON array — serialize as the loose Vec would
        self.as_slice().serialize(serializer)
    }
}

impl<'de, T: serde::Deserialize<'de>> serde::de::Deserialize<'de> for NonEmptyVec<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        // never derive a structural Deserialize (it would bypass the invariant): deserialize the
        // loose Vec and route through the same TryFrom door as every other construction path.
        let vec = <Vec<T> as serde::de::Deserialize>::deserialize(deserializer)?;
        NonEmptyVec::try_from(vec).map_err(serde::de::Error::custom)
    }
}
