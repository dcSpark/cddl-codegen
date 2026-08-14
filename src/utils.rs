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
    let mut in_uppercase_run = false;
    let mut iter = ident.chars().peekable();
    while let Some(c) = iter.next() {
        match c {
            '-' => {
                snake_case.push('_');
            }
            '$' | '@' => {
                // ignored
            }
            c => {
                // NFT -> nft
                // IPAddress -> ip_address
                // shelley_MA -> shelley_ma
                // some_DNS_name -> some_dns_name (an existing separator absorbs
                // the inserted one — never emit a double underscore)
                let needs_sep = |s: &String| !s.is_empty() && !s.ends_with('_');
                if in_uppercase_run {
                    if c.is_ascii_uppercase() {
                        if let Some(next) = iter.peek()
                            && next.is_ascii_lowercase()
                        {
                            if needs_sep(&snake_case) {
                                snake_case.push('_');
                            }
                            in_uppercase_run = false;
                        }
                    } else {
                        in_uppercase_run = false;
                    }
                } else if c.is_ascii_uppercase() {
                    if needs_sep(&snake_case) {
                        snake_case.push('_');
                    }
                    in_uppercase_run = true;
                }
                snake_case.push(c.to_ascii_lowercase());
            }
        }
    }
    snake_case
}

/// A rust identifier (a `RustIdent`, a field name, a module-scope component) as a **WIT
/// identifier**: lowercase ASCII words joined with `-`.
///
/// Built on [`convert_to_snake_case`] rather than as a second case-walk, deliberately: WIT's word
/// boundaries are the same ones rust's snake case already computes (uppercase runs, `-`, the `$`/`@`
/// drops, no double separator), and a parallel walk would be a drift source the moment either side
/// gained a rule. So this is that walk plus exactly one WIT-specific step — the digit rule.
///
/// **Digit rule**: a word whose first character is an ASCII digit is MERGED into the word before it
/// with no separator (`index_0` → `index0`, which the generator emits for unnamed array members).
/// This is a **consumer-compatibility floor, not a legality constraint**: `index-0` resolves,
/// encodes, validates and builds at the pinned toolchain floor, and is rejected only by
/// wasm-tools ≤ 1.231-era consumer tooling. Emitting `index-0` would break every consumer on that
/// tooling, so the merge stays — but nothing here may be pinned as "`index-0` is illegal WIT".
///
/// Keyword escaping is deliberately NOT applied here: the `%` of `%map` is WIT SYNTAX, not part of
/// the name, and the name-collision detector and the rust↔WIT parity gate both have to compare
/// UNESCAPED names. See `generation::wit::wit_escape`, applied at render time.
///
/// Merging is non-injective (`index_0` and a sibling literally named `index0` converge), which is
/// why a post-conversion collision detector exists rather than a converter that tries to be clever.
///
/// **This conversion is MIRRORED outside rust**: `cddl-matrix/verify.ts`'s component-execution leg
/// re-implements it as `toKebabCase` to name the WIT resource its mint check and its wasmtime host
/// look for. The mirror's startup self-test parses the fixture table in
/// [`convert_to_kebab_case_table`] below and requires every TypeScript fixture to be present with
/// the same expected output, so a rule changed HERE cannot leave the component leg on an old rule.
/// Drift fails in the understating direction on the matrix side (the resource lookup misses, the
/// row reads "no minted component surface" and its round trip silently stops running), which is why
/// the mirror is asserted at verify.ts startup rather than left to the annotations diff.
pub fn convert_to_kebab_case(ident: &str) -> String {
    let snake = convert_to_snake_case(ident);
    let mut kebab = String::with_capacity(snake.len());
    for word in snake.split('_').filter(|word| !word.is_empty()) {
        let digit_led = word.starts_with(|c: char| c.is_ascii_digit());
        if kebab.is_empty() {
            // A leading digit is unreachable from any identifier this is called on: `RustIdent`s are
            // camel-cased (letter-led), and rust field/module idents cannot start with a digit
            // either. Assert rather than invent a rule for a shape that cannot arrive — a silent
            // `0foo` would be an invalid WIT identifier discovered three stages later, at encode.
            assert!(
                !digit_led,
                "cannot convert {ident:?} to a WIT identifier: it begins with the digit-led word \
                 {word:?}, and a WIT identifier must start with a letter. Rename the rule or field \
                 (the `@name` comment-DSL directive renames a rule without touching the spec's \
                 wire format)."
            );
        } else if !digit_led {
            kebab.push('-');
        }
        kebab.push_str(word);
    }
    kebab
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

/// Whether `name` is LEXICALLY shaped like a Rust identifier. This is the `IDENTIFIER_OR_KEYWORD`
/// half of the grammar only — `Self` passes here and is still unspellable as an item name, so a
/// caller deciding whether a MINTED name can be emitted verbatim wants
/// `parsing::is_spellable_variant_name`, which subtracts the keyword list too.
///
/// Deliberately ASCII-only rather than the full XID grammar Rust accepts: every name-minting path
/// in this crate already lands in ASCII (`convert_to_camel_case` upper-cases ASCII only, and the
/// fixed-text minter filters to `is_ascii_alphanumeric`), so the two agree on everything the tool
/// can actually produce, and a checker that is narrower than the emitter can only ever refuse a
/// name early — never wave an unspellable one through to rustfmt.
pub fn is_valid_rust_ident(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c == '_' || c.is_ascii_alphabetic() => {}
        _ => return false,
    }
    chars.all(|c| c == '_' || c.is_ascii_alphanumeric())
}

pub fn cddl_prelude(name: &str) -> Option<&str> {
    match name {
        // Custom implemented types like uint, bool, etc are handled in the alias system and
        // shouldn't reach here. The arm stays a guard rather than being deleted: reaching it
        // re-earns the `KNOWN_PANIC_CLASSES` entry retired when these names stopped arriving.
        // All six float prelude names are alias-handled — `IntermediateTypes::aliases()` registers
        // one primitive per name, because each names a different set of float VALUES.
        "uint" | "nint" | "int" | "bool" | "tstr" | "text" |
        "bstr" | "bytes" | "null" | "nil" | "undefined" | "true"  | "false" |
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
        "decfrac" => Some("#6.4([e10: int, m: integer])"),
        "bigfloat" => Some("#6.5([e2: int, m: integer])"),
        // RFC 8610 expected-conversion advice: these tags retain their fixed CBOR tag around an
        // arbitrary item. Their generated representation is the shared tagged-`AnyCbor` wrapper,
        // not a base16/base64 rendering API.
        "eb64url" => Some("#6.21(any)"),
        "eb64legacy" => Some("#6.22(any)"),
        "eb16" => Some("#6.23(any)"),
        "encoded-cbor" => Some("#6.24(bstr)"),
        "uri" => Some("#6.32(tstr)"),
        "b64url" => Some("#6.33(tstr)"),
        "b64legacy" => Some("#6.34(tstr)"),
        "regexp" => Some("#6.35(tstr)"),
        "mime-message" => Some("#6.36(tstr)"),
        // Unreachable from the pipeline, and kept as the guard that would catch a route around it.
        // `any` resolves to the `AnyCbor` runtime type and `cbor-any` is refused one level up, at
        // `IntermediateTypes::new_type`'s unresolved-reserved fallback — the only caller that can
        // record the role-neutral permanent-exclusion diagnostic. A future refactor that routes a
        // position around `new_type` would reach this panic again and re-earn the retired
        // `KNOWN_PANIC_CLASSES` entry, which is why the arm and its wording stay put rather than
        // being deleted.
        "any" | // #
        "cbor-any" => panic!("unsupported cddl prelude type: {}", name), // #6.55799(any)
        _ => None,
    }
}

/// Every reserved (CDDL prelude) identifier, supported or not — the universe
/// [`is_identifier_reserved`] answers over, spelled as a LIST rather than as `match` arms so it can
/// be ENUMERATED as well as queried.
///
/// The enumerable form is what lets the refused-name closure sweep
/// (`tests::refused_name_closure_tests`) derive its name axis by probing the whole universe instead
/// of transcribing a list from prose: the names that refuse ARE the refusal inventory, and a new
/// refusal arm for any of these fails that derivation until the inventory
/// (`IntermediateTypes::REFUSED_PRELUDE_NAMES`) is updated to match.
///
/// `any` is deliberately NOT here — it is not reserved (it classes as `AliasIdent::Rust`, so a user
/// rule named `any` shadows it), and `new_type` intercepts it one arm earlier. The sweep adds it to
/// its probe universe by name for exactly that reason.
#[rustfmt::skip]
pub const RESERVED_IDENTS: &[&str] = &[
    "uint",
    "int",
    "nint",
    "text",
    "tstr",
    "bytes",
    "bstr",
    "bool",
    "float",
    "float16",
    "float32",
    "float64",
    "float16-32",
    "float32-64",
    "tdate",
    "time",
    "number",
    "biguint",
    "bignint",
    "bigint",
    "integer",
    "unsigned",
    "decfrac",
    "bigfloat",
    "eb64url",
    "eb64legacy",
    "eb16",
    "encoded-cbor",
    "uri",
    "b64url",
    "b64legacy",
    "regexp",
    "mime-message",
    "cbor-any",
    "null",
    "nil",
    "undefined",
    "true",
    "false",
];

pub fn is_identifier_reserved(name: &str) -> bool {
    RESERVED_IDENTS.contains(&name)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snake_case_never_generates_double_underscore() {
        // uppercase directly after an existing separator must not insert a second one
        assert_eq!(convert_to_snake_case("some_DNS_name"), "some_dns_name");
        assert_eq!(convert_to_snake_case("hello_MA"), "hello_ma");
        assert_eq!(convert_to_snake_case("foo-Bar"), "foo_bar");
        // uppercase-run handling unchanged
        assert_eq!(convert_to_snake_case("NFT"), "nft");
        assert_eq!(convert_to_snake_case("IPAddress"), "ip_address");
        assert_eq!(convert_to_snake_case("DNSName"), "dns_name");
        // user-written doubles pass through untouched (their choice, not our insertion)
        assert_eq!(
            convert_to_snake_case("already__doubled"),
            "already__doubled"
        );
    }

    /// The WIT identifier converter's pinned table. Every row is a name the component face actually
    /// emits, and the two interesting classes are the digit merge and the acronym runs.
    ///
    /// LOCKSTEP: `cddl-matrix/verify.ts` mirrors this converter (`toKebabCase`) and its startup
    /// self-test parses these exact literal `assert_eq!` rows. Every TypeScript fixture must appear
    /// here with the same expectation; malformed or duplicate rows deliberately fail the verifier
    /// rather than being skipped. See [`convert_to_kebab_case`]'s doc comment for why the matrix
    /// side cannot notice drift on its own.
    #[test]
    fn convert_to_kebab_case_table() {
        // component execution reaches its synthetic holder, and `Foo` is the seeded external-type
        // name in the component matrix; both are TS fixture rows as well as emitted Rust names.
        assert_eq!(convert_to_kebab_case("ProbeHolder"), "probe-holder");
        assert_eq!(convert_to_kebab_case("Foo"), "foo");
        // digit merge: the generator emits `index_0`/`index_1` accessors for unnamed array members
        // (see tests/extern-deps-wasm), and `index-0` is rejected by wasm-tools <= 1.231 consumers.
        assert_eq!(convert_to_kebab_case("index_0"), "index0");
        assert_eq!(convert_to_kebab_case("index_1"), "index1");
        // acronym runs lowercase, inheriting convert_to_snake_case's boundaries verbatim
        assert_eq!(convert_to_kebab_case("TxID"), "tx-id");
        assert_eq!(convert_to_kebab_case("IPAddress"), "ip-address");
        assert_eq!(convert_to_kebab_case("NFT"), "nft");
        assert_eq!(convert_to_kebab_case("some_DNS_name"), "some-dns-name");
        // a WIT keyword converts like any other name — escaping is `wit::wit_escape`'s job, applied
        // at render time, because the collision detector and the parity gate compare UNESCAPED names
        assert_eq!(convert_to_kebab_case("Record"), "record");
        // the fixed method vocabulary
        assert_eq!(convert_to_kebab_case("to_cbor_bytes"), "to-cbor-bytes");
        assert_eq!(convert_to_kebab_case("from_cbor_bytes"), "from-cbor-bytes");
        // an interior digit is a word CONTINUATION, not a word: `Foo2Bar` snake-cases to `foo2_bar`
        // (the `2` never starts a word), so the digit merge does not fire and the `B` boundary does.
        // Pinned off what convert_to_snake_case already does rather than invented as a second rule.
        assert_eq!(convert_to_kebab_case("Foo2Bar"), "foo2-bar");
    }

    #[test]
    #[should_panic(expected = "must start with a letter")]
    fn convert_to_kebab_case_rejects_a_digit_led_name() {
        // Unreachable from a RustIdent or a rust field/module ident; asserted rather than handled,
        // so a future caller that finds a way to reach it fails here instead of at WIT encode.
        let _ = convert_to_kebab_case("0abc");
    }
}
