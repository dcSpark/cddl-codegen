//! Encoding-field declarations, inference, depth names and encoding-struct helpers.

use super::*;

#[derive(Debug)]
pub(super) struct EncodingField {
    pub(super) field_name: String,
    /// The type this encoding field is DECLARED as. Callers that push it into an encoding struct (or
    /// an enum variant's field list) must therefore hand `encoding_fields` the member's **declared**
    /// type — never `.resolve_aliases()`d — so the declaration keeps the alias ident the data-struct
    /// field for the same member already keeps (`docs/docs/output_format.mdx` § "Type spelling at
    /// member positions"). Resolving is a STRUCTURAL-DISPATCH normalization; reusing its result as a
    /// NAMING input is how `BTreeMap<Vec<u8>, ..>` came to index a field typed
    /// `OrderedHashMap<PolicyId, ..>`. Callers that consume only `field_name`/`default_expr` are
    /// spelling-irrelevant and may pass whatever shape is convenient.
    pub(super) type_name: String,
    /// this MUST be equivalent to the Default trait of the encoding field.
    /// This can be more concise though e.g. `None` for `Option<T>::default()`
    pub(super) default_expr: &'static str,
    pub(super) enc_conversion_before: &'static str,
    pub(super) enc_conversion_after: &'static str,
    pub(super) is_copy: bool,
}

impl EncodingField {
    /// An `Option<cbor_event::Sz>` integer/float/tag size slot, filled from a decoded `Sz`.
    pub(super) fn sz(field_name: String) -> Self {
        Self {
            field_name,
            type_name: "Option<cbor_event::Sz>".to_owned(),
            default_expr: "None",
            enc_conversion_before: "Some(",
            enc_conversion_after: ")",
            is_copy: true,
        }
    }

    /// A text/bytes `StringEncoding` slot, converted from the decoded string length encoding.
    fn string(field_name: String) -> Self {
        Self {
            field_name,
            type_name: "StringEncoding".to_owned(),
            default_expr: "StringEncoding::default()",
            enc_conversion_before: "StringEncoding::from(",
            enc_conversion_after: ")",
            is_copy: false,
        }
    }

    /// An array/map `LenEncoding` slot.
    pub(super) fn len(field_name: String) -> Self {
        Self {
            field_name,
            type_name: "LenEncoding".to_owned(),
            default_expr: "LenEncoding::default()",
            enc_conversion_before: "",
            enc_conversion_after: "",
            is_copy: true,
        }
    }

    /// The tri-state `TagPresenceEncoding` slot of an optionally tagged value. The deserialize
    /// preamble produces it fully formed, so no conversion is applied.
    fn tag_presence(field_name: String) -> Self {
        Self {
            field_name,
            type_name: "TagPresenceEncoding".to_owned(),
            default_expr: "TagPresenceEncoding::default()",
            enc_conversion_before: "",
            enc_conversion_after: "",
            is_copy: true,
        }
    }

    /// A collection sidecar (`Vec<..>` / `BTreeMap<..>`) holding the inner encodings of each
    /// element or entry.
    fn sidecar(field_name: String, type_name: String, default_expr: &'static str) -> Self {
        Self {
            field_name,
            type_name,
            default_expr,
            enc_conversion_before: "",
            enc_conversion_after: "",
            is_copy: false,
        }
    }

    pub fn enc_conversion(&self, expr: &str) -> String {
        format!(
            "{}{}{}",
            self.enc_conversion_before, expr, self.enc_conversion_after
        )
    }
}

pub(super) fn key_encoding_field(name: &str, key: &FixedValue) -> EncodingField {
    let field_name = format!("{name}_key_encoding");
    match key {
        FixedValue::Text(_) => EncodingField::string(field_name),
        FixedValue::Uint(_) => EncodingField::sz(field_name),
        _ => unimplemented!("preserve-encodings key encoding for fixed map key {key:?}"),
    }
}

/// THE mint for a `@custom_encodings` declaration: the codec-visible encoding variables the
/// declaration names, in declared order, under `name`.
///
/// Positional naming — the first slot keeps the bare `{name}_encoding` spelling every inferred
/// single-variable member already has (so a one-`str` declaration over an alias-of-bytes reproduces
/// today's names and types exactly), and further slots append their 1-based index
/// (`{name}_encoding2`, `{name}_encoding3`, …), keeping `_encoding` non-terminal only where a
/// declaration made it ambiguous. Deterministic and derivable from the declaration alone, which is
/// what lets the two carrier channels (the emission configs, and the sidecar/LHS derivation) agree
/// without either consulting the other.
///
/// Types, defaults and `is_copy` are the SAME values `encoding_fields_impl` mints for the inferred
/// flavors of these kinds (`encoding_fields.rs`'s `Primitive`/`Array`/`Map` arms) — a declaration fixes WHICH
/// variables a codec sees and in what order, never how one of them is spelled or passed.
pub(super) fn declared_encoding_fields(name: &str, kinds: &[EncodingKind]) -> Vec<EncodingField> {
    kinds
        .iter()
        .enumerate()
        .map(|(i, kind)| {
            let field_name = if i == 0 {
                format!("{name}_encoding")
            } else {
                format!("{name}_encoding{}", i + 1)
            };
            match kind {
                EncodingKind::Sz => EncodingField::sz(field_name),
                EncodingKind::Str => EncodingField::string(field_name),
                EncodingKind::Len => EncodingField::len(field_name),
            }
        })
        .collect()
}

/// Whether an `Alias` node reached during encoding-variable derivation may honor its own rule's
/// `@custom_encodings` declaration.
///
/// A declaration describes the wire of the codec written BESIDE it, so it is honored at exactly the
/// node where its own pair governs. Once some OUTER pair has taken over the position (a field-level
/// pair shadowing the alias it is written over, or an outer alias's pair), everything under it is
/// inside that codec's opaque wire and its declarations describe a codec nobody calls — so the
/// derivation switches to `Blind` and reports what INFERENCE alone says, which is exactly what the
/// governing codec is handed. `docs/docs/comment_dsl.mdx` states this as the precedence rule.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum AliasDeclarations {
    /// No pair governs above this point: an alias's own declaration is the answer.
    Honor,
    /// A pair already governs: ignore every declaration below (identical to the pre-directive
    /// behaviour, and therefore byte-identical for any spec that declares nothing).
    Blind,
}

pub(super) fn encoding_fields(
    types: &IntermediateTypes,
    name: &str,
    ty: &RustType,
    include_default: bool,
    cli: &Cli,
) -> Vec<EncodingField> {
    encoding_fields_decls(
        types,
        name,
        ty,
        include_default,
        cli,
        AliasDeclarations::Honor,
    )
}

/// `encoding_fields` for a MEMBER position whose own comment may carry a custom pair — every record
/// field site, which is where a field-level `@custom_serialize`/`@custom_deserialize` is read.
///
/// A field-level pair governs the member from the top of the recursion (it fires BEFORE any encoding
/// operation is consumed, so it is handed the tag/`.cbor` variables too), which makes the three
/// answers here exhaustive:
///   * pair + declaration → the declared list IS the member's whole codec-visible list;
///   * pair, no declaration → inference, blind to any declaration underneath (the pair shadows them);
///   * no pair → ordinary inference, honoring a declaration the member's own TYPE rule carries.
///
/// `_default_present` is appended as `encoding_fields` appends it: it is generated-code-owned, never
/// part of the codec's tuple, so it survives a declaration untouched.
pub(super) fn field_encoding_fields(
    types: &IntermediateTypes,
    name: &str,
    ty: &RustType,
    field_metadata: Option<&RuleMetadata>,
    include_default: bool,
    cli: &Cli,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    let field_pair = field_metadata
        .filter(|rmd| rmd.custom_serialize.is_some() && rmd.custom_deserialize.is_some());
    match field_pair {
        Some(rmd) => match rmd.custom_encodings.as_ref() {
            Some(kinds) => {
                let mut encs = declared_encoding_fields(name, kinds);
                if include_default && ty.config.default.is_some() {
                    encs.push(default_present_encoding_field(name));
                }
                encs
            }
            None => encoding_fields_decls(
                types,
                name,
                ty,
                include_default,
                cli,
                AliasDeclarations::Blind,
            ),
        },
        None => encoding_fields(types, name, ty, include_default, cli),
    }
}

/// The generated-code-owned `{name}_default_present` slot a `.default`-carrying member gets on top of
/// its encoding variables. Never part of a codec's argument or return tuple (the codec is called
/// only when the value is present), so it is minted the same way whether the list around it was
/// inferred or declared.
fn default_present_encoding_field(name: &str) -> EncodingField {
    EncodingField {
        field_name: format!("{name}_default_present"),
        type_name: "bool".to_owned(),
        default_expr: "false",
        enc_conversion_before: "",
        enc_conversion_after: "",
        is_copy: true,
    }
}

/// Whether a custom (de)serializer pair placed over `ty` would be handed NO encoding variables at
/// all — the state `@custom_encodings` exists to make declarable, and which
/// `IntermediateTypes::finalize` refuses under `--preserve-encodings` when nothing is declared.
///
/// This asks the SAME derivation the emission sites build their argument lists from, so "empty
/// demand" cannot come to mean two different things; the alternative (a twin predicate over
/// `encoding_fields_impl`'s empty arms) would be a second, unpaired derivation of the same fact.
/// `Blind` because a pair governs its whole subtree — a declaration underneath describes a codec
/// whose wire this one has swallowed.
pub(crate) fn custom_codec_demand_is_empty(
    types: &IntermediateTypes,
    ty: &RustType,
    cli: &Cli,
) -> bool {
    encoding_fields_decls(types, "wire", ty, false, cli, AliasDeclarations::Blind).is_empty()
}

/// `encoding_fields` with an explicit declaration mode — see [`AliasDeclarations`].
fn encoding_fields_decls(
    types: &IntermediateTypes,
    name: &str,
    ty: &RustType,
    include_default: bool,
    cli: &Cli,
    decls: AliasDeclarations,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    // TODO: how do we handle defaults for nested things? e.g. inside of a ConceptualRustType::Map
    let mut encs = encoding_fields_impl(types, name, ty.into(), cli, 0, 0, decls);
    if include_default && ty.config.default.is_some() {
        encs.push(default_present_encoding_field(name));
    }
    encs
}

/// The tag-level infix for a stacked tag's encoding member name. Tag levels count OUTSIDE-IN:
/// level 1 (the outermost tag) keeps the historical `tag` spelling so all existing single-tag
/// output stays byte-identical; each deeper level appends its 1-based number (`tag2`, `tag3`, …),
/// keeping the `_encoding` suffix terminal like every other member. Callers combine it as
/// `{name}_{infix}_encoding`. Shared by the member declaration (`encoding_fields_impl`), the
/// serialize read, and the deserialize write so the three can never drift on the scheme.
pub(crate) fn tag_encoding_infix(tag_level: usize) -> String {
    if tag_level <= 1 {
        "tag".to_owned()
    } else {
        format!("tag{tag_level}")
    }
}

/// The LOCAL a mandatory `Tagged` level's `match .tag_sz()?` pattern binds its head size to, under
/// `--preserve-encodings`. Depth-suffixed for the same reason [`tag_encoding_infix`] is: stacked
/// levels nest their `match` blocks, so an un-suffixed binding would let the inner level shadow the
/// outer and both final exprs would read the innermost size.
///
/// Shared rather than spelled twice because two emitters must agree on it: the `Tagged` arm that
/// BINDS it, and the `Optional` arm's `None` branch, which re-states the already-consumed tag size
/// for a null payload (the `Some` branch gets it threaded through the child's `.map(..)` instead).
/// A drift between the two is not a compile error at generation time — it is an E0425 in the
/// consumer's crate, or worse, a silently dropped head width.
pub(crate) fn tag_enc_binding(tag_level: usize) -> String {
    if tag_level <= 1 {
        "tag_enc".to_owned()
    } else {
        format!("tag_enc{tag_level}")
    }
}

/// The `.cbor`-level names a payload's byte string owns, for a chain that crosses more than one
/// `CBORBytes` operation on ONE member name (the INLINE spelling `bytes .cbor (bytes .cbor T)`).
/// Levels count OUTSIDE-IN, exactly as [`tag_encoding_infix`]'s do, and level 1 keeps the historical
/// spelling so all existing single-payload output stays byte-identical; each deeper level appends
/// its 1-based number.
///
/// Four names move together per level, which is why they share one derivation: the encoding member
/// infix (`{name}_bytes_encoding`), the serializer's staging buffer and the byte vector it
/// finalizes into (`{var}_inner_se`, `{var}_bytes`), the deserializer's reader over those bytes
/// (`inner_de`) and the local a non-statement payload is staged in (`{var}_payload`). All of them
/// are minted per OWNING VARIABLE, so at two depths of one chain the undepthed spellings collide:
/// the buffer is used after `finalize()` moved it (E0382), the sidecar declares one field twice
/// (E0124), and the outer reader's leftover-bytes check silently re-reads the INNER reader.
///
/// Shared by the member declaration (`encoding_fields_impl`), the serialize write and the
/// deserialize read so the three can never drift on the scheme — the same reason
/// [`tag_encoding_infix`] is shared.
fn cbor_level_name(base: &str, cbor_level: usize) -> String {
    if cbor_level <= 1 {
        base.to_owned()
    } else {
        format!("{base}{cbor_level}")
    }
}

/// The `.cbor` payload byte string's encoding-member infix: `bytes` / `bytes2` / … Callers combine
/// it as `{name}_{infix}_encoding` (declaration, serialize read) and as `{var}_{infix}` for the
/// serialized/deserialized byte vector itself. See [`cbor_level_name`].
pub(crate) fn cbor_bytes_infix(cbor_level: usize) -> String {
    cbor_level_name("bytes", cbor_level)
}

/// The serializer's payload staging buffer suffix: `{var}_inner_se` / `{var}_inner_se2` / …
/// See [`cbor_level_name`].
pub(crate) fn cbor_payload_buffer_suffix(cbor_level: usize) -> String {
    cbor_level_name("inner_se", cbor_level)
}

/// The deserializer's reader over the payload bytes: `inner_de` / `inner_de2` / … Unlike the other
/// three this one is NOT prefixed by the owning variable (it is a reader overload, not a member
/// name), so the depth suffix is the only thing keeping two levels of one chain apart. See
/// [`cbor_level_name`].
pub(crate) fn cbor_payload_reader(cbor_level: usize) -> String {
    cbor_level_name("inner_de", cbor_level)
}

/// The local a payload read at a non-statement position is staged in: `{var}_payload` /
/// `{var}_payload2` / … See [`cbor_level_name`].
pub(crate) fn cbor_payload_binding_suffix(cbor_level: usize) -> String {
    cbor_level_name("payload", cbor_level)
}

/// `tag_depth` is the number of tag levels already crossed on THIS member name (0 at the member
/// root, incremented each time a `Tagged`/`OptionallyTagged` op recurses into its child under the
/// same name). It drives `tag_encoding_infix` so stacked tags get distinct members. Name-changing
/// recursions (array element, map key/value) start a fresh sub-member and reset it to 0.
///
/// `cbor_depth` is the same counter for `.cbor` payload levels, driving `cbor_bytes_infix` so the
/// INLINE `bytes .cbor (bytes .cbor T)` spelling declares one byte-string sidecar per depth instead
/// of the same field twice (E0124). It threads and resets at exactly the same boundaries
/// `tag_depth` does — the two are independent counters over the same name, so a tag between two
/// payloads advances only the tag one.
///
/// `decls` decides whether an `Alias` node may answer with its rule's own `@custom_encodings`
/// declaration instead of recursing — see [`AliasDeclarations`]. It threads UNCHANGED through every
/// recursion (the shadow a governing codec casts covers its whole subtree, including across the
/// array-element / map-key / map-value name boundaries that reset `tag_depth`).
pub(super) fn encoding_fields_impl(
    types: &IntermediateTypes,
    name: &str,
    ty: SerializingRustType,
    cli: &Cli,
    tag_depth: usize,
    cbor_depth: usize,
    decls: AliasDeclarations,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    match ty {
        SerializingRustType::Root(ConceptualRustType::Array(elem_ty), _cfg) => {
            let base = EncodingField::len(format!("{name}_encoding"));
            let inner_encs = encoding_fields_impl(
                types,
                &format!("{name}_elem"),
                (&**elem_ty).into(),
                cli,
                0,
                0,
                decls,
            );
            if inner_encs.is_empty() {
                vec![base]
            } else {
                let type_name_elem = tuple_type_name(&inner_encs);
                vec![
                    base,
                    EncodingField::sidecar(
                        format!("{name}_elem_encodings"),
                        format!("Vec<{type_name_elem}>"),
                        "Vec::new()",
                    ),
                ]
            }
        }
        SerializingRustType::Root(ConceptualRustType::Map(k, v), cfg) => {
            let mut encs = vec![EncodingField::len(format!("{name}_encoding"))];
            let key_encs = encoding_fields_impl(
                types,
                &format!("{name}_key"),
                (&**k).into(),
                cli,
                0,
                0,
                decls,
            );
            let val_encs = encoding_fields_impl(
                types,
                &format!("{name}_value"),
                (&**v).into(),
                cli,
                0,
                0,
                decls,
            );

            // `@duplicates preserve` (the pair-map twin): a `BTreeMap` keyed by key VALUE is
            // structurally incapable of holding two entries with the same key, so the encoding
            // sidecar must be POSITIONAL — a `Vec<tuple>` parallel to the entries, indexed by
            // position exactly like the array `_elem_encodings` sidecar (serialize reads `.get(i)`,
            // deserialize `.push(..)`s per entry). The loose (reject/default) table stays keyed by
            // key value.
            let preserve_pair_map =
                cfg.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve);

            // Both sidecars are indexed the same way (by position, or by the entry's KEY), so the
            // value sidecar is keyed by `k` too.
            for (suffix, inner_encs) in [("key", &key_encs), ("value", &val_encs)] {
                if inner_encs.is_empty() {
                    continue;
                }
                let type_name_value = tuple_type_name(inner_encs);
                let (type_name, default_expr) = if preserve_pair_map {
                    (format!("Vec<{type_name_value}>"), "Vec::new()")
                } else {
                    (
                        format!(
                            "BTreeMap<{}, {}>",
                            k.for_rust_member(types, false, cli),
                            type_name_value
                        ),
                        "BTreeMap::new()",
                    )
                };
                encs.push(EncodingField::sidecar(
                    format!("{name}_{suffix}_encodings"),
                    type_name,
                    default_expr,
                ));
            }
            encs
        }
        SerializingRustType::Root(ConceptualRustType::Primitive(p), _cfg) => match p {
            Primitive::Bytes | Primitive::Str => {
                vec![EncodingField::string(format!("{name}_encoding"))]
            }
            Primitive::I8
            | Primitive::I16
            | Primitive::I32
            | Primitive::I64
            | Primitive::N64
            | Primitive::U8
            | Primitive::U16
            | Primitive::U32
            | Primitive::U64
            | Primitive::Float
            | Primitive::F16
            | Primitive::F32
            | Primitive::F64
            | Primitive::F16To32
            | Primitive::F32To64 => vec![EncodingField::sz(format!("{name}_encoding"))],
            Primitive::Bool =>
            /* bool only has 1 encoding */
            {
                vec![]
            }
        },
        SerializingRustType::Root(ConceptualRustType::Fixed(f), _cfg) => {
            // A fixed value encodes exactly as its carrying primitive does.
            let primitive = match f {
                FixedValue::Bool(_) | FixedValue::Null | FixedValue::Undefined => return vec![],
                FixedValue::Nint(_) => Primitive::I64,
                FixedValue::Uint(_) => Primitive::U64,
                FixedValue::Float(_) => Primitive::Float,
                FixedValue::Text(_) => Primitive::Str,
                FixedValue::Bytes(_) => Primitive::Bytes,
            };
            encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(primitive)).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            )
        }
        SerializingRustType::Root(ConceptualRustType::Alias(alias_ident, ty), cfg) => {
            // A type-level custom codec OWNS the wire from this node down, so when its rule declares
            // the wire's encoding variables (`@custom_encodings`) the declaration IS the answer here
            // — replacing the whole inferred subtree, which is what makes a zero-demand replaced type
            // (a self-carrying extern, `bool`, `any`) able to carry framing at all. Reached only in
            // `Honor` mode and only for a COMPLETE pair: a lone half is refused elsewhere, and under
            // a governing outer codec (`Blind`) the declaration describes a codec nobody calls.
            // A pair WITHOUT a declaration still shadows its subtree — it governs the position, so
            // what it is handed is what inference alone says.
            let alias_pair = types
                .type_aliases()
                .get(alias_ident)
                .and_then(|info| info.rule_metadata.as_ref())
                .filter(|rmd| rmd.custom_serialize.is_some() && rmd.custom_deserialize.is_some());
            if decls == AliasDeclarations::Honor
                && let Some(rmd) = alias_pair
            {
                if let Some(kinds) = rmd.custom_encodings.as_ref() {
                    return declared_encoding_fields(name, kinds);
                }
                return encoding_fields_impl(
                    types,
                    name,
                    SerializingRustType::Root(ty, cfg),
                    cli,
                    tag_depth,
                    cbor_depth,
                    AliasDeclarations::Blind,
                );
            }
            // Keep the OUTER RustTypeSerializeConfig (`cfg`): an Alias's inner is a bare
            // ConceptualRustType with no config of its own, so recursing with `(&**ty).into()`
            // would DEFAULT the config and drop the per-rule policy the alias carries — notably
            // `@duplicates preserve`, which the `Map` arm above reads to pick the POSITIONAL
            // (`Vec<..>`) encoding sidecar instead of the key-VALUE-keyed `BTreeMap<..>`. Dropping
            // it there is not a spelling difference but a wire-behaviour skew: a `BTreeMap` cannot
            // hold the repeated keys a preserve table exists to round-trip. (`generate_serialize`
            // and `generate_deserialize` keep the config at their own `Alias` arms for the same
            // reason.) Masked for as long as every caller whose `type_name` reaches a declaration
            // pre-resolved aliases; it stops being masked the moment one of them spells the
            // member's declared type instead.
            encoding_fields_impl(
                types,
                name,
                SerializingRustType::Root(ty, cfg),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            )
        }
        SerializingRustType::Root(ConceptualRustType::Optional(ty), _cfg) => {
            // same-name recursion (a nullable can still carry a tagged inner), so thread the depth
            // rather than resetting it via the `encoding_fields` wrapper.
            encoding_fields_impl(
                types,
                name,
                (&**ty).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            )
        }
        SerializingRustType::Root(ConceptualRustType::Rust(rust_ident), cfg) => {
            match &types.rust_struct(rust_ident).unwrap().variant() {
                // for c-style enums we push those up to where they are used instead of self-containing
                RustStructType::CStyleEnum { variants } => {
                    // earlier we are guaranteed that all variants will have the same encoding types
                    // or else it wouldn't end up as a c-style enum in the first place in IntermediateTypes
                    encoding_fields_decls(types, name, variants[0].rust_type(), false, cli, decls)
                }
                // also push them out for RawBytesType as they're not stored there, as if we had `bytes` directly here
                RustStructType::RawBytesType => encoding_fields_impl(
                    types,
                    name,
                    (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                    cli,
                    tag_depth,
                    cbor_depth,
                    decls,
                ),
                // a named table/array rule is a bare rust typedef onto a collection — there is no
                // struct for the encodings to live inside, so they must be pushed OUT to the
                // referring member exactly as the CStyleEnum/RawBytesType cases above do. Reached
                // only from a NOMINAL reference to such a rule (parse-order makes one when a rule
                // cycle is entered at the collection rule); the resolved-alias reference path
                // reaches the `Alias` arm and lands on the same `Map`/`Array` arms. Without this
                // the referrer mints no `{name}_encoding` sidecar while serialize (which DOES
                // recurse into the collection) reads one — E0425 on generated code.
                RustStructType::Table { domain, range, .. } => {
                    let structural =
                        ConceptualRustType::Map(Box::new(domain.clone()), Box::new(range.clone()));
                    let cfg = nominal_collection_cfg(types, rust_ident, &cfg);
                    encoding_fields_impl(
                        types,
                        name,
                        SerializingRustType::Root(&structural, cfg),
                        cli,
                        tag_depth,
                        cbor_depth,
                        decls,
                    )
                }
                RustStructType::Array { element_type, .. } => {
                    let structural = ConceptualRustType::Array(Box::new(element_type.clone()));
                    let cfg = nominal_collection_cfg(types, rust_ident, &cfg);
                    encoding_fields_impl(
                        types,
                        name,
                        SerializingRustType::Root(&structural, cfg),
                        cli,
                        tag_depth,
                        cbor_depth,
                        decls,
                    )
                }
                // no encodings here. they're contained inside the struct
                _ => vec![],
            }
        }
        // `any` is self-carried: the `AnyCbor` value stores its own encodings, so it contributes no
        // owner encoding fields (the member's ordinary KEY encoding slot mints separately via
        // `key_encoding_field`, so it is unaffected). Mirrors the `Rust(ident)` self-carried case.
        SerializingRustType::Root(ConceptualRustType::Any, _cfg) => vec![],
        SerializingRustType::EncodingOperation(CBOREncodingOperation::Tagged(tag), child) => {
            // This tag is the (tag_depth + 1)th level crossed on this member name; its member keeps
            // `tag` at level 1 and gains a numeric infix deeper, so stacked tags don't collide.
            let tag_level = tag_depth + 1;
            let tag_infix = tag_encoding_infix(tag_level);
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_{tag_infix}"),
                (&ConceptualRustType::Fixed(FixedValue::Uint(*tag as u64))).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            );
            encs.append(&mut encoding_fields_impl(
                types, name, *child, cli, tag_level, cbor_depth, decls,
            ));
            encs
        }
        SerializingRustType::EncodingOperation(
            CBOREncodingOperation::OptionallyTagged(_tag),
            child,
        ) => {
            // the tri-state tag-presence var (absent | present(sz)); the deserialize preamble
            // produces a fully-formed `TagPresenceEncoding`, so no enc conversion is applied.
            let tag_level = tag_depth + 1;
            let tag_infix = tag_encoding_infix(tag_level);
            let mut encs = vec![EncodingField::tag_presence(format!(
                "{name}_{tag_infix}_encoding"
            ))];
            encs.append(&mut encoding_fields_impl(
                types, name, *child, cli, tag_level, cbor_depth, decls,
            ));
            encs
        }
        SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
            // This byte string is the (cbor_depth + 1)th `.cbor` level crossed on this member name;
            // its member keeps `bytes` at level 1 and gains a numeric infix deeper, so the INLINE
            // `bytes .cbor (bytes .cbor T)` spelling declares one sidecar per depth instead of
            // `{name}_bytes_encoding` twice. The child recurses one level deeper.
            let cbor_level = cbor_depth + 1;
            let bytes_infix = cbor_bytes_infix(cbor_level);
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_{bytes_infix}"),
                (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            );
            encs.append(&mut encoding_fields_impl(
                types, name, *child, cli, tag_depth, cbor_level, decls,
            ));
            encs
        }
    }
}

pub(super) fn encoding_var_names_str(
    types: &IntermediateTypes,
    field_name: &str,
    rust_type: &RustType,
    cli: &Cli,
) -> String {
    encoding_var_names_str_for_field(types, field_name, rust_type, None, cli)
}

/// `encoding_var_names_str` for a position that may carry a FIELD-level custom pair: the tuple LHS a
/// custom deserializer's return is destructured into must name exactly the variables the codec
/// returns, which its own declaration fixes (see [`field_encoding_fields`]).
pub(super) fn encoding_var_names_str_for_field(
    types: &IntermediateTypes,
    field_name: &str,
    rust_type: &RustType,
    field_metadata: Option<&RuleMetadata>,
    cli: &Cli,
) -> String {
    assert!(cli.preserve_encodings);
    // `is_fixed_value` is a STRUCTURAL question (does this position bind a value at all), so it
    // still asks the resolved type. The encoding list below deliberately does NOT resolve: a
    // declaration rides on the alias node `resolve_aliases()` deletes, and the `Alias` arm is a pure
    // pass-through for every undeclared type, so the two are identical wherever nothing declares.
    let mut var_names = if rust_type
        .clone()
        .resolve_aliases()
        .conceptual_type
        .is_fixed_value()
    {
        vec![]
    } else {
        vec![field_name.to_owned()]
    };
    for enc in
        field_encoding_fields(types, field_name, rust_type, field_metadata, false, cli).into_iter()
    {
        var_names.push(enc.field_name);
    }
    tuple_str(var_names)
}

// Value-level twin of `tuple_type_name`: joins encoding VAR names into a parenthesized tuple.
pub(super) fn tuple_str(strs: Vec<String>) -> String {
    if strs.len() > 1 {
        format!("({})", strs.join(", "))
    } else {
        strs.join(", ")
    }
}

// Type-level twin of `tuple_str`: joins encoding fields' `type_name`s into a parenthesized tuple
// type unless there is exactly one (then the lone type_name stands alone, unparenthesized).
pub(super) fn tuple_type_name(encs: &[EncodingField]) -> String {
    if encs.len() == 1 {
        encs[0].type_name.clone()
    } else {
        format!(
            "({})",
            encs.iter()
                .map(|enc| enc.type_name.clone())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// True iff every encoding field's `default_expr` is a trivial literal (`None`/`false`) rather than
/// a function call (`LenEncoding::default()`, `Vec::new()`, `BTreeMap::new()`,
/// `StringEncoding::default()`). Trivial-literal tuple defaults may be emitted with `unwrap_or(..)`;
/// a call-bearing default must stay behind `unwrap_or_else(|| ..)` or clippy::or_fun_call fires.
/// Centralized so every tuple-default emission site agrees on the same decision.
pub(super) fn encoding_defaults_all_trivial(encoding_fields: &[EncodingField]) -> bool {
    encoding_fields
        .iter()
        .all(|enc| matches!(enc.default_expr, "None" | "false"))
}

pub(super) fn make_encoding_struct(encoding_name: &str) -> codegen::Struct {
    let mut encoding_struct = codegen::Struct::new(encoding_name.to_string());
    encoding_struct
        .vis("pub")
        .derive("Clone")
        .derive("Debug")
        .derive("Default");
    encoding_struct
}

/// clippy's default `type-complexity-threshold`. A type in a lint-scored position (struct field, fn
/// signature, ...) whose structural score exceeds this trips `clippy::type_complexity`. Type
/// *aliases* are not scored by the lint, so hoisting an over-threshold encoding-struct field type
/// into a `pub type` alias silences it without an `#[allow]` and without changing any emitted bytes
/// or round-trip semantics.
const TYPE_COMPLEXITY_THRESHOLD: u64 = 250;

/// Reproduce clippy's `type_complexity` scoring closely enough to decide, deterministically,
/// whether an emitted encoding field type would trip the lint. clippy walks the type and adds
/// `10 * nest` for every path / tuple / array / slice / reference node, incrementing `nest` by one
/// when descending into that node's children. The emitted encoding types use only paths (`Foo`,
/// `Foo<..>`, `a::b`) and tuples (no refs/slices), so scoring those node kinds suffices: every
/// other node (an exact-byte `[u8; N]` key, for one) scores as a childless `10 * nest` leaf.
/// Over-estimating here is harmless (it only mints an extra alias); the clippy gate is the backstop
/// if the real boundary ever shifts. Text that does not parse as a type scores as over the
/// threshold, so it is hoisted rather than risking the lint.
fn type_complexity_score(ty: &str) -> u64 {
    fn score(ty: &syn::Type, nest: u64) -> u64 {
        match ty {
            // A single `(T)` grouping is just `T` (no HIR node).
            syn::Type::Paren(paren) => score(&paren.elem, nest),
            syn::Type::Group(group) => score(&group.elem, nest),
            // `()` is a unit.
            syn::Type::Tuple(tuple) if tuple.elems.is_empty() => 1,
            // A tuple is one node whose elements are children.
            syn::Type::Tuple(tuple) => {
                10 * nest + tuple.elems.iter().map(|e| score(e, nest + 1)).sum::<u64>()
            }
            // A path (`u64`, `cbor_event::Sz`, `Vec<..>`) is one node whose generic type
            // arguments are children.
            syn::Type::Path(path) => {
                10 * nest
                    + path
                        .path
                        .segments
                        .iter()
                        .filter_map(|segment| match &segment.arguments {
                            syn::PathArguments::AngleBracketed(args) => Some(&args.args),
                            _ => None,
                        })
                        .flatten()
                        .filter_map(|arg| match arg {
                            syn::GenericArgument::Type(arg) => Some(score(arg, nest + 1)),
                            _ => None,
                        })
                        .sum::<u64>()
            }
            _ => 10 * nest,
        }
    }
    syn::parse_str::<syn::Type>(ty).map_or(u64::MAX, |ty| score(&ty, 1))
}

/// Add one field to an encoding struct, hoisting an over-`type_complexity` field type into a
/// deterministic `pub type <Owner><FieldCamel> = ..;` alias in the same `cbor_encodings` scope so
/// `clippy::type_complexity` stays quiet without an `#[allow]`. Alias names can't collide with each
/// other: `owner` (the owning encoding struct's base type name) is distinct per struct and
/// `field_name` is distinct within a struct, so identical anonymous shapes in different rules never
/// collide. An alias CAN in principle collide with another rule's encoding-struct name:
/// owner `Foo` + field `bar_encoding` aliases to `FooBarEncoding`, which a rule named `foo-bar`
/// also claims. That needs an over-threshold field AND the exact sibling rule name, and it fails
/// LOUD (E0428 in the generated crate, caught by every compile gate), so it is not disambiguated
/// preemptively.
/// Aliases are collected (not pushed) so the caller can push them into the scope alongside the
/// struct.
pub(super) fn push_encoding_struct_field(
    encoding_struct: &mut codegen::Struct,
    aliases: &mut Vec<(String, String)>,
    owner: &RustIdent,
    field_name: &str,
    type_name: &str,
) {
    let field_type = if type_complexity_score(type_name) > TYPE_COMPLEXITY_THRESHOLD {
        let alias = format!("{}{}", owner, convert_to_camel_case(field_name));
        aliases.push((alias.clone(), type_name.to_owned()));
        alias
    } else {
        type_name.to_owned()
    };
    encoding_struct.field(format!("pub {field_name}"), field_type);
}

#[cfg(test)]
mod type_complexity_tests {
    use super::type_complexity_score;

    /// clippy's structural score for the node shapes encoding field types use: a leaf path is
    /// `10 * nest`, generic arguments and tuple elements are children one level deeper, a `(T)`
    /// grouping is transparent, and `()` scores 1.
    #[test]
    fn type_complexity_score_vectors() {
        for (ty, expected) in [
            ("u64", 10),
            ("()", 1),
            ("(u64)", 10),
            ("(LenEncoding, StringEncoding)", 50),
            ("Option<cbor_event::Sz>", 30),
            ("BTreeMap<[u8; 4], StringEncoding>", 50),
            (
                "Vec<(LenEncoding, BTreeMap<PolicyId, StringEncoding>)>",
                170,
            ),
        ] {
            assert_eq!(type_complexity_score(ty), expected, "{ty}");
        }
        assert_eq!(type_complexity_score("Vec<"), u64::MAX);
    }
}
