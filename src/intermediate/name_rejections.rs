use super::*;

impl<'a> IntermediateTypes<'a> {
    pub(super) fn validate_emitted_name_surface(&self) -> Vec<String> {
        fn spellable(name: &str) -> bool {
            is_valid_rust_ident(name) && !crate::parsing::RUST_KEYWORDS.contains(&name)
        }
        fn check_name(messages: &mut BTreeSet<String>, name: &str, family: &str, provenance: &str) {
            if !spellable(name) {
                messages.insert(format!(
                    "emitted {family} `{name}` is not a spellable Rust identifier (mint site: {provenance}). Rename it to an ASCII Rust identifier; where this spelling comes from a supported directive, use `@name <new_name>`."
                ));
            }
        }
        fn check_record(messages: &mut BTreeSet<String>, record: &RustRecord, provenance: &str) {
            let mut names = BTreeSet::new();
            for field in &record.fields {
                check_name(messages, &field.name, "record field", provenance);
                if !names.insert(field.name.clone()) {
                    messages.insert(format!(
                        "emitted record field `{}` is duplicated in its record namespace (mint site: {provenance})",
                        field.name
                    ));
                }
            }
            for row in record.captured_dynamic_rows() {
                check_name(messages, &row.field_name, "dynamic-row field", provenance);
                if !names.insert(row.field_name.clone()) {
                    messages.insert(format!(
                        "emitted dynamic-row field `{}` duplicates a field in its record namespace (mint site: {provenance})",
                        row.field_name
                    ));
                }
            }
        }

        let mut messages = BTreeSet::new();
        for (ident, rust_struct) in &self.rust_structs {
            if !ident.is_type_expression() {
                let provenance = self
                    .nominal_mint_claims
                    .get(ident)
                    .map(|claim| claim.site.to_string())
                    .or_else(|| self.source_rule_name(ident).map(str::to_owned))
                    .unwrap_or_else(|| "IR nominal registration".to_owned());
                check_name(
                    &mut messages,
                    ident.as_ref(),
                    "nominal Rust type",
                    &provenance,
                );
            }
            let enum_provenance = format!("enum `{ident}`");
            let variants = match rust_struct.variant() {
                RustStructType::TypeChoice { variants }
                | RustStructType::GroupChoice { variants, .. }
                | RustStructType::CStyleEnum { variants } => Some(variants),
                RustStructType::Record(record) => {
                    check_record(&mut messages, record, &format!("record `{ident}`"));
                    None
                }
                _ => None,
            };
            if let Some(variants) = variants {
                let mut names = BTreeSet::new();
                for variant in variants {
                    let name = variant.name.to_string();
                    let variant_provenance = [
                        VariantMintContext::TypeChoice(ident.clone()),
                        VariantMintContext::GroupChoice(ident.clone()),
                    ]
                    .into_iter()
                    .find_map(|context| {
                        self.variant_mint_claims.get(&context).and_then(|claims| {
                            claims
                                .iter()
                                .find(|claim| claim.emitted_name == name)
                                .map(|claim| {
                                    format!(
                                        "{context}, arm {} (`{}`; {})",
                                        claim.arm_ordinal,
                                        claim.source_name,
                                        if claim.explicit {
                                            "explicit @name"
                                        } else {
                                            "derived"
                                        },
                                    )
                                })
                        })
                    })
                    .unwrap_or_else(|| enum_provenance.clone());
                    check_name(&mut messages, &name, "enum variant", &variant_provenance);
                    if !names.insert(name.clone()) {
                        messages.insert(format!(
                            "emitted enum variant `{ident}::{name}` is duplicated in its enum namespace (mint site: {variant_provenance})"
                        ));
                    }
                    if let EnumVariantData::Inlined(record) = &variant.data {
                        check_record(
                            &mut messages,
                            record,
                            &format!("inlined variant `{ident}::{name}`"),
                        );
                    }
                }
            }
        }
        for (alias, info) in &self.type_aliases {
            if let AliasIdent::Rust(ident) = alias
                && (info.emits_rust_alias() || info.emits_wasm_alias())
                && !ident.is_type_expression()
            {
                check_name(
                    &mut messages,
                    ident.as_ref(),
                    "type alias",
                    self.source_rule_name(ident).unwrap_or("alias registration"),
                );
            }
        }
        messages.into_iter().collect()
    }
}

/// A graceful-rejection message if `source_name` (a user-chosen rule / plain-group name, as spelled
/// in the CDDL) cannot be used as a Rust type name, else `None`. This mirrors the two `assert!`
/// guards in `RustIdent::new` exactly — a camel-cased form that collides with a reserved Rust
/// std/prelude type (`option` → `Option`, `box` → `Box`, `fn` → `Fn`, `self`/`Self` → `Self`), or a
/// CDDL keyword (`true` / `false` / prelude type names) — so the same names those asserts would
/// panic on are instead rejected gracefully when caught at the parse-walk seam (`api::with_types`).
/// Exact lowercase `int` is excluded from the keyword branch identically to `RustIdent::new`: at
/// `api::with_types`' pre-parse lifecycle seam it releases the project's built-in `Int` marker, so
/// an authored `int` rule may become that type's owner. A different source spelling that camel-cases
/// to `Int` does not release the marker and is rejected by global struct registration instead.
///
/// The asserts stay as a backstop for synthesized/internal idents (which never route through here);
/// this function is only for user-chosen names, where a panic on valid CDDL is the bug being fixed.
/// A third, non-panicking class — cddl-codegen runtime types and by-name imports
/// (`rust_reserved::RUNTIME_TYPES`, `rust_reserved::IMPORTED_TYPES`) — is
/// refused only here, never by `RustIdent::new`.
pub fn reserved_ident_rejection(source_name: &str) -> Option<String> {
    let Some(kind) = RustIdent::reserved_reason(source_name) else {
        return runtime_type_rejection(source_name);
    };
    match kind {
        ReservedIdentKind::RustTypeName => {
            let camel = convert_to_camel_case(source_name);
            Some(format!(
                "rule `{source_name}`: its name camel-cases to `{camel}`, a reserved Rust std/prelude \
                 type the generated code depends on — emitting a type by that name would shadow it. A \
                 rule/group name becomes the emitted Rust type name directly, so (unlike a struct field, \
                 which a `; @name` comment renames) the CDDL identifier itself must be renamed to a \
                 non-reserved name."
            ))
        }
        ReservedIdentKind::CddlKeyword => Some(format!(
            "rule `{source_name}`: `{source_name}` is a reserved CDDL keyword and cannot be used as \
             a rule/group name. A rule/group name becomes the emitted Rust type name directly, so \
             (unlike a struct field, which a `; @name` comment renames) the CDDL identifier itself \
             must be renamed to a non-reserved name."
        )),
    }
}

/// Refuse a user-chosen rule/group name that camel-cases to a cddl-codegen runtime type or a
/// type the generated code imports by name.
/// This stays outside `RustIdent::reserved_reason`, which also guards internally minted idents
/// with panicking asserts.
fn runtime_type_rejection(source_name: &str) -> Option<String> {
    let camel = convert_to_camel_case(source_name);
    if crate::rust_reserved::RUNTIME_TYPES.contains(&camel.as_str()) {
        return Some(format!(
            "rule `{source_name}`: its name camel-cases to `{camel}`, a type of the \
             cddl-codegen runtime (the `error`/`serialization` modules) that the generated code \
             imports into every module — emitting a type by that name would collide with it. A \
             rule/group name becomes the emitted Rust type name directly, so (unlike a struct \
             field, which a `; @name` comment renames) the CDDL identifier itself must be \
             renamed to a non-reserved name."
        ));
    }
    crate::rust_reserved::IMPORTED_TYPES
        .contains(&camel.as_str())
        .then(|| {
            format!(
                "rule `{source_name}`: its name camel-cases to `{camel}`, a cddl-codegen runtime \
                 or dependency-crate type that the generated code imports by name wherever the spec \
                 uses it — emitting a type by that name would collide with it. A rule/group name \
                 becomes the emitted Rust type name directly, so (unlike a struct field, which a \
                 `; @name` comment renames) the CDDL identifier itself must be renamed to a \
                 non-reserved name."
            )
        })
}

/// A graceful-rejection message if a `@rust_name` PIN cannot be used as a Rust type name, else
/// `None`. A pin becomes the emitted Rust name for the dependency's type verbatim (the consumer
/// imports `use dep::<pin> as <derived>;`), so it must clear the SAME reserved-ident bar a derived
/// name does — a `@rust_name Option` pin describes a type the dependency could never have emitted
/// (its own `reserved_ident_rejection` would have fired), so the pin can never be honored. Mirrors
/// `reserved_ident_rejection` but names the pin and the rule it sits on.
///
/// Unlike `RustIdent::reserved_reason`, this has no exact-`int` carve-out, so a `@rust_name int`
/// pin is refused. That carve-out exists for `api::with_types` releasing the built-in `Int` marker
/// to an authored `int` rule, a lifecycle a pin never passes through.
pub fn reserved_pin_rejection(pin: &str, rule: &str) -> Option<String> {
    let camel = convert_to_camel_case(pin);
    if crate::rust_reserved::STD_TYPES.contains(&camel.as_str())
        || crate::rust_reserved::RUNTIME_TYPES.contains(&camel.as_str())
        || crate::rust_reserved::IMPORTED_TYPES.contains(&camel.as_str())
        || is_identifier_reserved(pin)
    {
        return Some(format!(
            "@rust_name `{pin}` on rule `{rule}`: the pinned Rust name is a reserved Rust \
             std/prelude type, a cddl-codegen runtime or generator-imported type, or a CDDL keyword — a dependency could never have emitted a type by that \
             name, so this pin can never be honored. Choose a non-reserved name."
        ));
    }
    None
}

/// A rule/group name containing `.` is rejected at the reserved-name pre-scan seam. RFC 8610 allows
/// dots in identifiers, but `convert_to_camel_case` passes `.` straight through (`cose.label` →
/// `Cose.label`, invalid Rust) and `RustIdent::new` adds no dot check — so a dotted name would flow
/// silently into a crate that does not compile. Dotted idents chiefly arise from `cddlc`'s
/// `as`-namespacing expansion (`import … as cose` rewrites imported rules to `cose.<name>`), which
/// cddl-codegen does not yet support; rejecting them loudly is the interop-honest behavior until
/// scope-qualified idents land.
pub fn dotted_ident_rejection(source_name: &str) -> Option<String> {
    if source_name.contains('.') {
        return Some(format!(
            "rule `{source_name}`: its name contains a `.`, which cddl-codegen does not support in \
             a rule/group name — it camel-cases to invalid Rust (`{source_name}` → \
             `{}`). Dotted identifiers typically come from `cddlc`'s `as`-namespacing expansion \
             (`import … as <prefix>` rewrites rules to `<prefix>.<name>`); rename the rule to a \
             dot-free identifier.",
            convert_to_camel_case(source_name)
        ));
    }
    None
}

/// The `--preserve-encodings` refusal for a custom (de)serializer pair whose replaced type demands no
/// encoding variables and which declares none of its own. `position` names where the pair is written
/// (a rule, or a field of a struct); `replaced_is_named_type` selects the extra remedy that only
/// applies when the replaced type is one this crate does not codegen — its own impls already own the
/// wire, so the pair has nothing to add.
///
/// Message text is pinned by `dsl_position_tests` and by the fixture suite; keep the three remedies
/// (declare the wire, declare `none`, drop the pair) present in any rewording.
pub(super) fn custom_codec_zero_demand_rejection(
    position: &str,
    replaced_is_named_type: bool,
) -> String {
    let extern_remedy = if replaced_is_named_type {
        " If the replaced type has ONE wire — a hand-written extern, say — it does not need the pair \
         at all: its own `Serialize`/`Deserialize` impls own the wire, encodings included, and \
         dropping the pair records them."
    } else {
        ""
    };
    format!(
        "@custom_serialize/@custom_deserialize on {position}: under `--preserve-encodings` this \
         pair replaces the codec of a type that demands NO encoding variables, so the custom wire's \
         framing (int/tag widths, string headers, container lengths) is recorded nowhere and the \
         round trip silently normalizes it — both directions agree, so no round-trip test can see \
         it. Declare what the custom wire needs beside the pair (`@custom_encodings <kinds>`, a \
         comma-separated list of `sz` / `str` / `len`), or state that it needs nothing \
         (`@custom_encodings none`) if the wire genuinely has no framing.{extern_remedy}"
    )
}
