impl<T: serde::Serialize, const MIN: u64, const MAX: u64> serde::Serialize
    for BoundedVec<T, MIN, MAX>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        self.as_slice().serialize(serializer)
    }
}

impl<'de, T: serde::Deserialize<'de>, const MIN: u64, const MAX: u64>
    serde::de::Deserialize<'de> for BoundedVec<T, MIN, MAX>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: serde::de::Deserializer<'de> {
        let vec = <Vec<T> as serde::de::Deserialize>::deserialize(deserializer)?;
        BoundedVec::try_from(vec).map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod json_tests {
    use super::*;

    #[test]
    fn json_uses_the_same_checked_door() {
        assert!(serde_json::from_str::<BoundedVec<u64, 2, 3>>("[1,2]").is_ok());
        assert!(serde_json::from_str::<BoundedVec<u64, 2, 3>>("[1]").is_err());
        assert!(serde_json::from_str::<BoundedVec<u64, 2, 3>>("[1,2,3,4]").is_err());
    }
}
