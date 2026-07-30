//! The declared-type spelling rule at TYPE-DECLARATION positions, and the config-threading
//! invariant that rule depends on (user doc: `docs/docs/output_format.mdx` § "Type spelling at
//! member positions").
//!
//! The rule: wherever generated code DECLARES the type of a member position — a data-struct field,
//! an encoding-struct field, a constructor/accessor signature, a named collection rule's own alias
//! target — it spells that type as declared, keeping the outermost alias ident. One function spells
//! member types (`ConceptualRustType::for_rust_member_ct`) and it already keeps the alias, so a
//! resolved spelling in the output is never a decision to resolve for NAMING: it is a caller that
//! resolved for STRUCTURAL DISPATCH and then reused the dispatch-normalized value as a naming input.
//!
//! Pinned here:
//!
//! * **Agreement between the sidecar paths, at every depth.** A declared map field and an
//!   open-struct rest row are the same member position emitted by different code, and they must
//!   spell one alias one way. The rest-row path was nominal by accident (nobody resolved it) rather
//!   than by rule, so without a pin the next refactor re-resolves it and nothing fails. The pin
//!   spans DEPTHS because the defect's sharpest form is intra-expression: a rest row over a
//!   container-typed value once spelled depth 1 declared and depth 2 resolved inside one type
//!   expression (`BTreeMap<Epoch, (.., BTreeMap<Vec<u8>, StringEncoding>, ..)>`), which a
//!   depth-1-only pin passes straight over.
//!
//! * **The multi-scope route.** With a directory input the alias can be declared in a different
//!   module than the record referring to it, so a declared spelling in `<scope>/cbor_encodings.rs`
//!   names an ident from another scope. That routing is a different emission surface from the
//!   serialization file's, and a missing route is a compile error rather than a source diff — hence
//!   the companion compile cell
//!   `integration_tests::declared_spelling_cross_scope_encoding_crate_compiles`.
//!
//! * **The ordering guard.** Un-resolving an encoding-field caller reaches
//!   `encoding_fields_impl`'s `Alias` arm, which must thread the OUTER
//!   `RustTypeSerializeConfig` — the `Map` arm reads `cfg.duplicates` to pick a POSITIONAL
//!   (`Vec<..>`) sidecar for a `@duplicates preserve` table instead of the key-VALUE-keyed
//!   `BTreeMap<..>`. An `Alias`'s inner is a bare `ConceptualRustType` with no config of its own, so
//!   recursing through `(&**ty).into()` DEFAULTS the config and silently drops the policy. That is
//!   not a spelling difference: a `BTreeMap` cannot hold the repeated keys a preserve table exists
//!   to round-trip, so the sidecar would reject duplicates the wire carries.
//!
//! These drive the full in-process generation pipeline (`api::generated_strings`) and assert the
//! emitted SOURCE, so a regression at any emission path surfaces here rather than in a consumer's
//! regen diff. Call TARGETS (`Credential::deserialize`, `ScriptHash::from_raw_bytes`) are a separate
//! position class and are deliberately NOT asserted here.

use crate::cli::Cli;
use clap::Parser;

/// Run the whole generation pipeline in-process and return every emitted file's source joined, or
/// the graceful error string.
fn generate(spec: &str, tag: &str, extra_flags: &[&str]) -> Result<String, String> {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_declspell_{}_{}.cddl",
        tag,
        std::process::id()
    ));
    std::fs::write(&path, spec).unwrap();
    let mut args = vec![
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "declared_spelling_unused",
    ];
    args.extend_from_slice(extra_flags);
    let cli = Cli::parse_from(args);
    let result = crate::api::generated_strings(&cli)
        .map(|files| files.into_values().collect::<Vec<_>>().join("\n"))
        .map_err(|e| e.to_string());
    std::fs::remove_file(&path).ok();
    result
}

/// As [`generate`], but for a DIRECTORY input (one `.cddl` per module) and keeping the per-file
/// keying — the multi-scope pin has to say WHICH file a spelling landed in, which a joined string
/// cannot.
fn generate_multifile(
    modules: &[(&str, &str)],
    tag: &str,
    extra_flags: &[&str],
) -> Result<std::collections::BTreeMap<String, String>, String> {
    let dir = std::env::temp_dir().join(format!(
        "cddl_codegen_declspell_dir_{}_{}",
        tag,
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    for (name, body) in modules {
        std::fs::write(dir.join(format!("{name}.cddl")), body).unwrap();
    }
    let mut args = vec![
        "cddl-codegen",
        "--input",
        dir.to_str().unwrap(),
        "--output",
        "declared_spelling_unused",
    ];
    args.extend_from_slice(extra_flags);
    let cli = Cli::parse_from(args);
    let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
    let _ = std::fs::remove_dir_all(&dir);
    result
}

const PRESERVE: &[&str] = &["--preserve-encodings=true", "--wasm", "false"];

/// A `@duplicates preserve` named table referenced by a record member keeps POSITIONAL (`Vec<..>`)
/// encoding sidecars — the DECLARATION, the deserialize-side construction and the serialize-side
/// lookup all three, because the type alone does not prove the wire behaviour.
///
/// This is the guard on the ORDER of the declared-spelling change: un-resolving the record encoding
/// struct's `encoding_fields` caller without threading the config through
/// `encoding_fields_impl`'s `Alias` arm turns every field here into the keyed `BTreeMap<Eon, ..>`
/// form, which structurally cannot hold the repeated keys the table round-trips. The failure is a
/// wire-behaviour skew wearing a respelling's clothes, so it must be caught by a test rather than by
/// reading a large bless diff.
#[test]
fn preserve_table_member_keeps_positional_encoding_sidecars() {
    let src = generate(
        "eon = uint\nmeta = {* eon => text} ; @duplicates preserve\nholder = [m: meta]\n",
        "preserve_positional",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub m_key_encodings: Vec<Option<cbor_event::Sz>>,")
            && src.contains("pub m_value_encodings: Vec<StringEncoding>,"),
        "a `@duplicates preserve` table member's encoding sidecars must be DECLARED positional \
         (`Vec<..>`, indexed by entry position) — a key-VALUE-keyed `BTreeMap` cannot hold the \
         repeated keys the table exists to round-trip, got:\n{src}"
    );
    assert!(
        !src.contains("pub m_key_encodings: BTreeMap")
            && !src.contains("pub m_value_encodings: BTreeMap"),
        "a preserve table's sidecar must not be keyed by key VALUE:\n{src}"
    );
    assert!(
        src.contains("let mut m_key_encodings = Vec::new();")
            && src.contains("let mut m_value_encodings = Vec::new();"),
        "deserialize must BUILD the positional sidecars as `Vec`s (the declaration and the \
         construction are two expressions of one decision):\n{src}"
    );
    assert!(
        src.contains("m_key_encodings.get(i)") && src.contains("m_value_encodings.get(i)"),
        "serialize must read the positional sidecars BY POSITION (`.get(i)`), not by key \
         value:\n{src}"
    );
}

/// A declared map field and an open-struct rest row over the SAME alias spell it the same way, at
/// depth 1 AND at depth 2.
///
/// This is the regression that would have caught the whole class. It is asserted across depths on
/// purpose: the two paths agreed at depth 1 (the rest row's key domain was never resolved, so it kept
/// `Epoch` by accident) while disagreeing one level down inside the SAME type expression, where the
/// rest VALUE's nested container spelled its key `Vec<u8>` against a data field typed
/// `OrderedHashMap<Epoch, OrderedHashMap<PolicyId, String>>`. A depth-1-only version of this pin
/// passes over exactly that.
#[test]
fn declared_field_and_rest_row_sidecars_agree_at_every_depth() {
    let src = generate(
        "epoch = uint\npolicy_id = bytes\n\
         keyed = {1: {* epoch => {* policy_id => text}}}\n\
         open_holder = {1: uint, * epoch => {* policy_id => text}}\n\
         open_tail = [uint, * {* policy_id => text}]\n",
        "both_depths",
        PRESERVE,
    )
    .expect("must generate");

    // Depth 1 — the sidecar's index key is the data field's DECLARED key type, in both paths. It has
    // to be: serialize looks the sidecar up with a key borrowed straight out of the data map, so the
    // two are one type expressed twice.
    assert!(
        src.contains("pub key_1_key_encodings: BTreeMap<Epoch, Option<cbor_event::Sz>>,"),
        "the DECLARED map field's sidecar must index by the declared key type:\n{src}"
    );
    assert!(
        src.contains("pub rest_key_encodings: BTreeMap<Epoch, Option<cbor_event::Sz>>,"),
        "the REST ROW's sidecar must index by the declared key type:\n{src}"
    );

    // Depth 2 — the container inside the rest value / tail element spells ITS key declared too.
    // Both of these `type_name`s reach a declaration through `tuple_type_name`, which is what made
    // them the paths the four-caller scoping missed.
    assert!(
        src.matches("BTreeMap<PolicyId, StringEncoding>").count() >= 4,
        "every nested sidecar container must spell its key declared (`PolicyId`) — expected the \
         nested key/value pair in each of the declared field, the rest value and the tail \
         element:\n{src}"
    );
    assert!(
        !src.contains("BTreeMap<Vec<u8>,"),
        "no sidecar may spell an aliased key by its structural target — depth-1-declared / \
         depth-2-resolved inside one type expression is the defect in its purest form:\n{src}"
    );
    assert!(
        !src.contains("BTreeMap<u64,"),
        "no sidecar may spell the `epoch` alias as `u64`:\n{src}"
    );
}

/// A member whose alias is DECLARED IN ANOTHER MODULE still spells it declared in the referring
/// scope's `cbor_encodings.rs`, and the ident is routed there rather than dangling.
///
/// The encoding file is a different emission surface from the serialization file, so `mark_refs`
/// marking both an alias ident and its target for the latter does not by itself carry the former.
/// The route works via the scope's own `mod.rs` import plus `cbor_encodings.rs`'s `use super::*`,
/// which is what this asserts; that it actually COMPILES is the companion cell
/// `integration_tests::declared_spelling_cross_scope_encoding_crate_compiles`, because a missing
/// route is E0412/E0433 and not a source diff.
#[test]
fn cross_scope_alias_is_spelled_and_routed_in_the_referring_scope() {
    let files = generate_multifile(
        &[
            ("lib", "rt = [uint]\n"),
            ("a", "epoch = uint\npolicy_id = bytes\n"),
            (
                "b",
                "holder = [m: {* epoch => text}]\n\
                 open_holder = {1: uint, * epoch => {* policy_id => text}}\n",
            ),
        ],
        "cross_scope",
        PRESERVE,
    )
    .expect("must generate");

    let find = |suffix: &str| -> String {
        files
            .iter()
            .find(|(path, _)| path.ends_with(suffix))
            .map(|(_, src)| src.clone())
            .unwrap_or_else(|| {
                panic!(
                    "no generated file ending in `{suffix}`; got {:?}",
                    files.keys().collect::<Vec<_>>()
                )
            })
    };
    let b_enc = find("b/cbor_encodings.rs");
    let b_mod = find("b/mod.rs");

    assert!(
        b_enc.contains("BTreeMap<Epoch, Option<cbor_event::Sz>>")
            && b_enc.contains("BTreeMap<PolicyId, StringEncoding>"),
        "scope `b`'s encoding file must spell scope `a`'s aliases as declared, at both \
         depths:\n{b_enc}"
    );
    assert!(
        !b_enc.contains("BTreeMap<u64,") && !b_enc.contains("BTreeMap<Vec<u8>,"),
        "no cross-scope sidecar may fall back to the structural target:\n{b_enc}"
    );
    // The route: `cbor_encodings.rs` names the idents through `use super::*`, so scope `b`'s own
    // module must bind them. Asserted on the OWNING module rather than on the encoding file so the
    // pin does not encode which of the two files holds the `use`.
    assert!(
        b_mod.contains("Epoch") && b_mod.contains("PolicyId"),
        "scope `b`'s module must bring scope `a`'s aliases into the scope its encoding file \
         inherits via `use super::*`:\n{b_mod}"
    );
}

/// The wasm collection wrapper NAME is minted from the declared alias idents and stays that way.
///
/// Wrapper names are structural identity in the collections machinery and are deliberately NOT part
/// of the declared-spelling change (one wrapper may serve members declared under different aliases of
/// the same shape). They are pinned here anyway because they are the one member-adjacent position
/// that was ALREADY declared before this rule existed, and a later "make it uniform" pass in either
/// direction is a consumer-visible wasm API break.
#[test]
fn wasm_collection_wrapper_names_keep_the_declared_alias() {
    let src = generate(
        "epoch = uint\npolicy_id = bytes\nlabel = text\n\
         nested = [arr: [* policy_id], deep: {* epoch => {* policy_id => label}}]\n",
        "wasm_names",
        &["--preserve-encodings=true", "--wasm", "true"],
    )
    .expect("must generate");
    for expected in [
        "pub struct PolicyIdList(",
        "pub struct MapPolicyIdToLabel(",
        "pub struct MapEpochToMapPolicyIdToLabel(",
    ] {
        assert!(
            src.contains(expected),
            "wasm wrapper names are minted from declared alias idents; expected `{expected}` \
             in:\n{src}"
        );
    }
    assert!(
        !src.contains("pub struct VecU8List(") && !src.contains("pub struct MapU64To"),
        "no wasm wrapper name may be minted from a resolved structural target:\n{src}"
    );
}
