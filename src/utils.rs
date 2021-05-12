use cbor_event::Type as CBORType;
use std::collections::{BTreeMap};

use crate::cmd::USE_EXTENDED_PRELUDE;

pub fn _cbor_type_code_str(cbor_type: CBORType) -> &'static str {
    match cbor_type {
        CBORType::UnsignedInteger => "CBORType::UnsignedInteger",
        CBORType::NegativeInteger => "CBORType::NegativeInteger",
        CBORType::Bytes => "CBORType::Bytes",
        CBORType::Text => "CBORType::Text",
        CBORType::Array => "CBORType::Array",
        CBORType::Map => "CBORType::Map",
        CBORType::Tag => "CBORType::Tag",
        CBORType::Special => "CBORType::Special",
    }
}

pub fn convert_to_snake_case(ident: &str) -> String {
    let mut snake_case = String::new();
    for c in ident.chars() {
        match c {
            '-' => {
                snake_case.push('_');
            },
            '$' | '@' => {
                // ignored
            },
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
            },
            '$' | '@' => {
                // ignored
            },
            c => {
                if uppercase {
                    camel_case.push(c.to_ascii_uppercase());
                    uppercase = false;
                } else {
                    camel_case.push(c);
                }
            },
        }
    }
    camel_case
}

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
pub fn is_identifier_in_our_prelude(name: &str) -> bool {
    match name {
        "u32" |
        "i32" |
        "u64" |
        "i64" => true,
        _ => false,
    }
}

pub fn is_identifier_user_defined(name: &str) -> bool {
    !is_identifier_reserved(name) && (!USE_EXTENDED_PRELUDE || !is_identifier_in_our_prelude(name))
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