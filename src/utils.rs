use std::collections::BTreeMap;

pub fn cbor_type_code_str(cbor_type: cbor_event::Type) -> &'static str {
    match cbor_type {
        cbor_event::Type::UnsignedInteger => "cbor_event::Type::UnsignedInteger",
        cbor_event::Type::NegativeInteger => "cbor_event::Type::NegativeInteger",
        cbor_event::Type::Bytes => "cbor_event::Type::Bytes",
        cbor_event::Type::Text => "cbor_event::Type::Text",
        cbor_event::Type::Array => "cbor_event::Type::Array",
        cbor_event::Type::Map => "cbor_event::Type::Map",
        cbor_event::Type::Tag => "cbor_event::Type::Tag",
        cbor_event::Type::Special => "cbor_event::Type::Special",
    }
}

pub fn convert_to_snake_case(ident: &str) -> String {
    let mut snake_case = String::new();
    for c in ident.chars() {
        match c {
            '-' => {
                snake_case.push('_');
            }
            '$' | '@' => {
                // ignored
            }
            c => {
                if c.is_ascii_uppercase() && !snake_case.is_empty() {
                    snake_case.push('_')
                }
                snake_case.push(c.to_ascii_lowercase());
            }
        }
    }
    snake_case
}

pub fn convert_to_camel_case(ident: &str) -> String {
    let mut camel_case = String::new();
    let mut uppercase = true;
    for c in ident.chars() {
        match c {
            '_' | '-' => {
                uppercase = true;
            }
            '$' | '@' => {
                // ignored
            }
            c => {
                if uppercase {
                    camel_case.push(c.to_ascii_uppercase());
                    uppercase = false;
                } else {
                    camel_case.push(c);
                }
            }
        }
    }
    camel_case
}

pub fn cddl_prelude(name: &str) -> Option<&str> {
    match name {
        // custom implemented types like uint, bool, etc
        // are handled in the alias system and shouldn't reach here
        "uint" | "nint" | "int" | "bool" | "tstr" | "text" |
        "bstr" | "bytes" | "null" | "nil" | "true"  | "false" |
        "float16" | // #7.25
        "float32" | // #7.26
        "float64" | // #7.27
        "float16-32" | // float16 / float32
        "float32-64" | // float32 / float64
        "float" => unreachable!("{} should be handled by the alias system instead", name),
        "tdate" => Some("#6.0(tstr)"),
        "time" => Some("#6.1(number)"),
        "number" => Some("int / float"),
        "biguint" => Some("#6.2(bstr)"),
        "bignint" => Some("#6.3(bstr)"),
        "bigint" => Some("biguint / bignint"),
        "integer" => Some("int / bigint"),
        "unsigned" => Some("uint / biguint"),
        "decfrac" => Some("#6.4([e10: int), m: integer])"),
        "bigfloat" => Some("#6.5([e2: int), m: integer])"),
        "encoded-cbor" => Some("#6.24(bstr)"),
        "uri" => Some("#6.32(tstr)"),
        "b64url" => Some("#6.33(tstr)"),
        "b64legacy" => Some("#6.34(tstr)"),
        "regexp" => Some("#6.35(tstr)"),
        "mime-message" => Some("#6.36(tstr)"),
        // TODO: we don't support any (yet) - could we use message-signing's code?
        "any" | // #
        "cbor-any" | // #6.55799(any)
        "eb64url" | // #6.21(any)
        "eb64legacy" | // #6.22(any)
        "eb16" | // #6.23(any)"),
        // TODO: nor undefined (yet)
        "undefined" => panic!("unsupported cddl prelude type: {}", name), // #7.23
        _ => None,
    }
}

#[rustfmt::skip]
pub fn is_identifier_reserved(name: &str) -> bool {
    match name {
        // These are all possible reserved identifiers, even if we don't support them
        "uint"       |
        "int"        |
        "nint"       |
        "text"       |
        "tstr"       |
        "bytes"      |
        "bstr"       |
        "bool"       |
        "float"      |
        "float16"    |
        "float32"    |
        "float64"    |
        "float16-32" |
        "float32-64" |
        "tdate"      |
        "time"       |
        "number"     |
        "biguint"    |
        "bignint"    |
        "bigint"     | 
        "integer"    |
        "unsigned"   |
        "decfrac"    |
        "bigfloat"   |
        "eb64url"    |
        "eb64legacy" |
        "eb16"       |
        "encoded-cbor" |
        "uri"        |
        "b64url"     |
        "b64legacy"  |
        "regexp"     |
        "mime-messag e" |
        "cbor-any"   |
        "null"       |
        "nil"        |
        "undefined"  |
        "true"       |
        "false" => true,
        _ => false,
    }
}

// as we also support our own identifiers for selecting integer precision, we need this too
#[rustfmt::skip]
#[allow(unused)]
pub fn is_identifier_in_our_prelude(name: &str) -> bool {
    matches!(name,
        "u8" |
        "i8" |
        "u16" |
        "i16" |
        "u32" |
        "i32" |
        "f32" |
        "u64" |
        "i64" |
        "f64"
    )
}

pub fn is_identifier_user_defined(name: &str) -> bool {
    !is_identifier_reserved(name)
}

pub fn append_number_if_duplicate(used_names: &mut BTreeMap<String, u32>, name: String) -> String {
    let entry = used_names.entry(name.clone()).or_default();
    *entry += 1;
    if *entry > 1 {
        format!("{}{}", name, *entry)
    } else {
        name
    }
}
