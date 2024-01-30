#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;

    #[test]
    fn bytes_wrapper() {
        let bytes = vec![0xBA, 0xAD, 0xF0, 0x0D];
        let hex = format!("\"{}\"", hex::encode(&bytes));
        let from_bytes = BytesWrapper::new(bytes.clone());
        let from_hex: BytesWrapper = serde_json::from_str(&hex).unwrap();
        assert_eq!(hex, serde_json::to_string_pretty(&from_bytes).unwrap());
        assert_eq!(hex, serde_json::to_string_pretty(&from_hex).unwrap());
    }

    #[test]
    fn str_wrapper() {
        let text = "hello, world";
        let json_str = format!("\"{text}\"");
        let from_str = StrWrapper::new(text.to_owned());
        let from_json: StrWrapper = serde_json::from_str(&json_str).unwrap();
        assert_eq!(json_str, serde_json::to_string_pretty(&from_str).unwrap());
        assert_eq!(json_str, serde_json::to_string_pretty(&from_json).unwrap());
    }

    fn json_wrapper_test<W, V>(value: V)
        where W: TryFrom<V> + serde::Serialize + for <'de> serde::Deserialize<'de>,
              V: std::fmt::Display,
              <W as std::convert::TryFrom<V>>::Error: std::fmt::Debug
    {
        let json_str = value.to_string();
        let from_value = W::try_from(value).unwrap();
        let from_json: W = serde_json::from_str(&json_str).unwrap();
        assert_eq!(json_str, serde_json::to_string_pretty(&from_value).unwrap());
        assert_eq!(json_str, serde_json::to_string_pretty(&from_json).unwrap());
    }

    #[test]
    fn u8_wrapper() {
        json_wrapper_test::<U8Wrapper, u8>(u8::MIN);
        json_wrapper_test::<U8Wrapper, u8>(u8::MAX);
    }

    #[test]
    fn u64_wrapper() {
        json_wrapper_test::<U64Wrapper, u64>(u64::MIN);
        json_wrapper_test::<U64Wrapper, u64>(u64::MAX);
    }

    #[test]
    fn i16_wrapper() {
        json_wrapper_test::<I16Wrapper, i16>(i16::MIN);
        json_wrapper_test::<I16Wrapper, i16>(i16::MAX);
    }

    #[test]
    fn i64_wrapper() {
        json_wrapper_test::<I64Wrapper, i64>(i64::MIN);
        json_wrapper_test::<I64Wrapper, i64>(i64::MAX);
    }

    #[test]
    fn nint_wrapper() {
        json_wrapper_test::<NintWrapper, u64>(u64::MIN);
        json_wrapper_test::<NintWrapper, u64>(u64::MAX);
    }

    // #[test]
    // fn bool_wrapper() {
    //     json_wrapper_test::<BoolWrapper, bool>(false);
    //     json_wrapper_test::<BoolWrapper, bool>(true);
    // }

    #[test]
    fn struct_wrapper() {
        let json_str = u64::MAX.to_string();
        let from_value = StructWrapper::from(U64Wrapper::from(u64::MAX));
        let from_json: StructWrapper = serde_json::from_str(&json_str).unwrap();
        assert_eq!(json_str, serde_json::to_string_pretty(&from_value).unwrap());
        assert_eq!(json_str, serde_json::to_string_pretty(&from_json).unwrap());
    }

    #[test]
    fn custom_wrapper() {
        let json_str = "\"1234\"";
        let from_value = CustomWrapper::from(1234u64);
        let from_json: CustomWrapper = serde_json::from_str(&json_str).unwrap();
        assert_eq!(json_str, serde_json::to_string_pretty(&from_value).unwrap());
        assert_eq!(json_str, serde_json::to_string_pretty(&from_json).unwrap());
    }
}
