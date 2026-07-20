impl<T: serde::Serialize> serde::Serialize for OrderedSet<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // an OrderedSet is a plain JSON array — serialize as the loose Vec would
        self.as_slice().serialize(serializer)
    }
}

impl<'de, T: serde::Deserialize<'de> + PartialEq> serde::de::Deserialize<'de> for OrderedSet<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        // never derive a structural Deserialize (it would bypass the uniqueness invariant):
        // deserialize the loose Vec and route through the same TryFrom door as every other path.
        let vec = <Vec<T> as serde::de::Deserialize>::deserialize(deserializer)?;
        OrderedSet::try_from(vec).map_err(serde::de::Error::custom)
    }
}

impl<T: serde::Serialize> serde::Serialize for NonEmptyOrderedSet<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.as_slice().serialize(serializer)
    }
}

impl<'de, T: serde::Deserialize<'de> + PartialEq> serde::de::Deserialize<'de>
    for NonEmptyOrderedSet<T>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let vec = <Vec<T> as serde::de::Deserialize>::deserialize(deserializer)?;
        NonEmptyOrderedSet::try_from(vec).map_err(serde::de::Error::custom)
    }
}
