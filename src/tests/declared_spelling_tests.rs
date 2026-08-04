//! The declared-type spelling rule at member positions, and the config-threading invariant that
//! rule depends on (user doc: `docs/docs/output_format.mdx` § "Type spelling at member positions").
//!
//! The rule: wherever generated code NAMES the type of a member position — a data-struct field, an
//! encoding-struct field, a constructor/accessor signature, a named collection rule's own alias
//! target, a member-level deserialize CALL TARGET — it spells that type as declared, keeping the
//! outermost alias ident. One function spells member types
//! (`ConceptualRustType::for_rust_member_ct`) and it already keeps the alias, so a resolved spelling
//! in the output is never a decision to resolve for NAMING: it is a caller that resolved for
//! STRUCTURAL DISPATCH and then reused the dispatch-normalized value as a naming input.
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
//! * **The call targets, and the two positions in the SAME code that must not move with them.** A
//!   member typed `sc: stake_credential` is filled by `StakeCredential::deserialize`; the arm that
//!   emits that also interpolates the struct ident into a `DeserializeError::new("..")` string
//!   literal (runtime-observable output a consumer matches on) and into `{ident}::{variant}`
//!   enum-variant paths (which name the struct's own variants). Respelling either would break the
//!   "spelling-only" property the whole rule claims, and both compile, so they are asserted
//!   NEGATIVELY here rather than left to review.
//!
//! * **Where a declared spelling must NOT reach**, each a silently-wrong spelling rather than a
//!   failure: the payload of an alias whose own RULE carries the encoding operation (the `bytes
//!   .cbor` carve-out — and its counterpart, an operation owned by the MEMBER's type expression,
//!   which the spelling DOES survive, since testing "did we cross an operation" instead gets that
//!   case wrong), and an `Optional` inner.
//!
//! These drive the full in-process generation pipeline (`api::generated_strings`) and assert the
//! emitted SOURCE, so a regression at any emission path surfaces here rather than in a consumer's
//! regen diff.

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

/// A member-level deserialize CALL TARGET names the member's type as DECLARED, at each of the three
/// call-target families the `Rust(ident)` arm emits.
///
/// `sc: StakeCredential` used to be filled by `Credential::deserialize` while the field, the
/// constructor parameter and the accessor all said `StakeCredential`. Only one function spells a
/// member's type and it keeps the alias, so the resolved spelling here was the residue of a
/// structural dispatch (the arm matched on `Rust(..)` and reused the matched ident as a name), never
/// a decision to name the structural target.
///
/// Both encoding profiles are exercised because `from_raw_bytes` has a SEPARATE format string per
/// profile (the preserve one threads the `StringEncoding::from(enc)` final expr through), so a
/// one-profile pin covers one of the two.
#[test]
fn member_call_targets_spell_the_declared_alias() {
    for (profile, extra) in [
        ("preserve", PRESERVE),
        (
            "plain",
            &["--preserve-encodings=false", "--wasm", "false"] as &[&str],
        ),
    ] {
        let src = generate(
            "hash = _CDDL_CODEGEN_RAW_BYTES_TYPE_\n\
             script_hash = hash\n\
             credential = [idx: uint]\n\
             stake_credential = credential\n\
             alias_of_alias = stake_credential\n\
             delta_coin = int\n\
             holder = [sc: stake_credential, sh: script_hash, aa: alias_of_alias, dc: delta_coin]\n",
            &format!("call_targets_{profile}"),
            extra,
        )
        .expect("must generate");

        for expected in [
            // `T::deserialize` — the nominal record member
            "StakeCredential::deserialize(raw)",
            // `T::from_raw_bytes` — one format string per profile
            "ScriptHash::from_raw_bytes(&bytes)",
            // OUTERMOST wins: an alias of an alias keeps the MEMBER's declared name
            "AliasOfAlias::deserialize(raw)",
            // `int` resolves to a generated `Int` struct, so this is the CML-reported shape
            "DeltaCoin::deserialize(raw)",
        ] {
            assert!(
                src.contains(expected),
                "[{profile}] a member call target must spell its type as declared; expected \
                 `{expected}` in:\n{src}"
            );
        }
        // Counted rather than `!contains`: `StakeCredential::deserialize` CONTAINS
        // `Credential::deserialize`, so a substring ban is unwritable here. Equal counts say every
        // occurrence of the structural target's name is part of the declared one.
        assert_eq!(
            src.matches("Credential::deserialize").count(),
            src.matches("StakeCredential::deserialize").count(),
            "[{profile}] no member call target may name the alias's structural target — every \
             `credential` member here is declared through an alias:\n{src}"
        );
        // same superstring trap: `ScriptHash::from_raw_bytes` contains `Hash::from_raw_bytes`
        assert_eq!(
            src.matches("Hash::from_raw_bytes").count(),
            src.matches("ScriptHash::from_raw_bytes").count(),
            "[{profile}] a raw-bytes member may not fall back to the structural target:\n{src}"
        );
        assert!(
            !src.contains("Int::deserialize(raw)"),
            "[{profile}] an `int` member declared through an alias may not fall back to the \
             generated `Int` struct's own name:\n{src}"
        );
    }
}

/// A `bytes .cbor` RULE BODY is a wrapper struct, so no ident ever names the bytes-wrapped form
/// transparently — and a `.cbor` payload written in a MEMBER's own type expression lifts the
/// payload's alias, because there the ident denotes exactly the value being read.
///
/// The shape this pins used to be the `bytes .cbor` CARVE-OUT: `cred_bytes = bytes .cbor credential`
/// emitted `pub type CredBytes = Credential;`, so `CredBytes` named the bytes-wrapped thing while its
/// standalone codec was `Credential`'s — one type, two wire forms — and the payload read had to be
/// sealed away from the alias's own ident, which would otherwise have compiled and lied. The rule
/// body now force-wraps, so the ident denotes a real type whose codec IS the bytes-wrapped form:
/// `CredBytes::deserialize` is the truth at the member position, and there is no transparent
/// bytes-wrapped ident left for a payload read to be sealed from.
///
/// The seal MECHANISM (an alias whose own `base_type` carries encodings does not lift) stays live
/// for the remaining rule-owned-operation aliases — see
/// `encoding_operation_ownership_decides_whether_the_spelling_survives`, whose seal half is a tagged
/// collection.
#[test]
fn cbor_bytes_root_wraps_and_a_member_cbor_lifts_the_payloads_alias() {
    let src = generate(
        "credential = [idx: uint]\n\
         stake_credential = credential\n\
         cred_bytes = bytes .cbor credential\n\
         holder = [cb: cred_bytes, payload: bytes .cbor stake_credential]\n",
        "cbor_carve_out",
        PRESERVE,
    )
    .expect("must generate");

    assert!(
        // `PRESERVE` mints the wrapper with named fields (an `encodings` sidecar rides beside the
        // inner), so the shape assertion is "a struct, not an alias" rather than the tuple spelling.
        src.contains("pub struct CredBytes")
            && !src.contains("pub type CredBytes")
            && src.contains("pub cb: CredBytes,"),
        "a `.cbor` rule body must mint a wrapper struct, never a transparent alias naming the \
         bytes-wrapped form:\n{src}"
    );
    assert!(
        src.contains("Credential::deserialize(inner_de)"),
        "the wrapper's own payload read names its target — the position is the payload, not the \
         bytes-wrapped value:\n{src}"
    );
    assert!(
        src.contains("CredBytes::deserialize(raw)"),
        "the MEMBER read names the wrapper, whose codec is the bytes-wrapped form — the spelling \
         that used to be a lie is now the truth:\n{src}"
    );
    assert!(
        src.contains("StakeCredential::deserialize(inner_de)"),
        "a `.cbor` written in the MEMBER's own type expression lifts the payload's alias — the \
         ident denotes exactly the value read there:\n{src}"
    );
}

/// A container inner spells from ITS OWN declaration, never from the outer member's.
///
/// `{ * stake_credential => delta_coin }` reads its key with `StakeCredential::deserialize` and its
/// value with `DeltaCoin::deserialize` — each from its own `Alias` arm — while the member itself is
/// declared through a further alias (`m: CmapAlias`) whose spelling must not leak down. This works
/// because the `Array`/`Map` arms recurse with a FRESH `DeserializeConfig`; a future refactor that
/// threads the outer config into them instead (to carry some new per-member policy) would leak the
/// spelling, and this is the pin that catches it.
#[test]
fn container_inners_spell_their_own_declaration() {
    let src = generate(
        "credential = [idx: uint]\n\
         stake_credential = credential\n\
         delta_coin = int\n\
         cmap = {* stake_credential => delta_coin}\n\
         cmap_alias = cmap\n\
         carr = [* stake_credential]\n\
         holder = [m: cmap_alias, a: carr]\n",
        "container_inners",
        PRESERVE,
    )
    .expect("must generate");

    assert!(
        src.contains("pub m: CmapAlias,") && src.contains("pub a: Carr,"),
        "the fixture must declare both members through their own aliases, or a leak of the OUTER \
         spelling has nothing to leak:\n{src}"
    );
    for expected in [
        "let m_key = StakeCredential::deserialize(raw)?;",
        "let m_value = DeltaCoin::deserialize(raw)?;",
        "a_arr.push(StakeCredential::deserialize(raw)?);",
    ] {
        assert!(
            src.contains(expected),
            "a container inner must spell its own declaration; expected `{expected}` in:\n{src}"
        );
    }
    assert!(
        !src.contains("CmapAlias::deserialize") && !src.contains("Carr::deserialize"),
        "the OUTER member's declared spelling leaked into a container inner — `Carr::deserialize` \
         would even compile as an inherent-method-less path error only at the element type, so \
         this must be asserted rather than reviewed:\n{src}"
    );
}

/// An `Optional`'s inner spells from its own declaration too — and the outer member's spelling must
/// be CLEARED on that descent, which is a compile error rather than a cosmetic slip.
///
/// `maybe_cred = credential / null` emits `pub type MaybeCred = Option<Credential>;`, and the arm
/// wraps the inner read in `Some(..)`. Carrying the member's spelling down yields
/// `Some(MaybeCred::deserialize(raw)?)` — `Some(Option<Credential>)` against a field typed
/// `Option<Credential>` (E0599/E0308 depending on the impl in reach). The inner of a member declared
/// `opt: stake_credential / null` is the OPPOSITE case: nothing to clear, and its own `Alias` arm
/// supplies `StakeCredential`.
///
/// `Array`/`Map` inners get this for free (fresh config); `Optional` recurses with the outer config,
/// so it is the one container that needs the clear.
#[test]
fn optional_inner_spells_its_own_declaration() {
    let src = generate(
        "credential = [idx: uint]\n\
         stake_credential = credential\n\
         maybe_cred = credential / null\n\
         holder = [mc: maybe_cred, opt: stake_credential / null]\n",
        "optional_inner",
        PRESERVE,
    )
    .expect("must generate");

    assert!(
        src.contains("pub type MaybeCred = Option<Credential>;")
            && src.contains("pub mc: MaybeCred,"),
        "the fixture must produce an alias whose TARGET is the optional, or the clear below has \
         nothing to clear:\n{src}"
    );
    assert!(
        src.contains("true => Some(Credential::deserialize(raw)?),"),
        "an alias-of-optional member's inner read names the OPTION'S INNER type — the member's own \
         spelling names the whole `Option<..>` and does not type-check inside the `Some(..)` the \
         arm wraps it in:\n{src}"
    );
    assert!(
        !src.contains("MaybeCred::deserialize"),
        "the member's declared spelling leaked into its `Optional` inner:\n{src}"
    );
    assert!(
        src.contains("true => Some(StakeCredential::deserialize(raw)?),"),
        "an `Optional` whose INNER is the aliased type still spells that inner declared:\n{src}"
    );
}

/// The two things in the SAME arm that must NOT move with the call targets: the error-message
/// STRING LITERAL and the enum-variant PATHS.
///
/// `DeserializeError::new("Cenum", ..)` is runtime-observable output — a consumer matches on that
/// text, so respelling it would break the "spelling-only" property this rule claims — and
/// `Cenum::I0` names the enum's own variants, not the member's type. Both would compile if
/// respelled (`type CenumAlias = Cenum;` admits `CenumAlias::I0`), which is exactly why they are
/// asserted here: getting a string literal by accident is the way this change goes wrong, and
/// nothing else fails when it does.
#[test]
fn aliased_member_leaves_error_strings_and_variant_paths_alone() {
    let src = generate(
        "cenum = 0 / 1 / 2\n\
         cenum_alias = cenum\n\
         holder = [ce: cenum_alias]\n",
        "negative_positions",
        PRESERVE,
    )
    .expect("must generate");

    assert!(
        src.contains("pub type CenumAlias = Cenum;") && src.contains("pub ce: CenumAlias,"),
        "the fixture must declare the c-style enum member through an alias, or the negative \
         assertions below are vacuous:\n{src}"
    );
    assert!(
        src.contains("\"Cenum\""),
        "the NoVariantMatched error text names the enum STRUCT and is runtime-observable \
         output:\n{src}"
    );
    assert!(
        !src.contains("\"CenumAlias\""),
        "an error-message string literal must never carry the member's declared spelling — that \
         changes consumers' error text, which is not a spelling-only change:\n{src}"
    );
    assert!(
        src.contains("Cenum::I0") && !src.contains("CenumAlias::I"),
        "enum-variant construction paths name the enum's OWN variants, not the member's declared \
         type:\n{src}"
    );
}

/// WHO OWNS the encoding operation decides whether the member's declared spelling survives it — not
/// whether the descent crossed one.
///
/// The seal exists because an alias ident can name the WRAPPED form: `tagged_creds =
/// #6.11([* stake_credential])` means `TaggedCreds` IS the tagged array, so the position that reads
/// the array body is not what the ident denotes. That is true exactly when the operation belongs to
/// the alias RULE. When the operation comes from the MEMBER's own type expression
/// (`f: #6.9(stake_credential)`) the alias still denotes precisely the value being read there — and
/// the field is typed `StakeCredential`, so lifting is what closes the disagreement rather than what
/// creates one.
///
/// The premise "a tag never has an alias in play, because `x = #6.9(y)` auto-wraps into a newtype"
/// holds only for the RULE form. The FIELD form keeps the alias, which is why `f` below was a live
/// instance of the reported defect: a field typed `StakeCredential` filled by
/// `Credential::deserialize`.
///
/// The seal half uses a TAGGED COLLECTION because that is the rule-owned-operation alias class that
/// still exists: a `bytes .cbor` rule body force-wraps (its ident is a real type, so the question
/// does not arise — see `cbor_bytes_root_wraps_and_a_member_cbor_lifts_the_payloads_alias`), while
/// the named-array/named-table kind-walk still registers `#6.n([* t])` as an alias whose `base_type`
/// carries the tag.
#[test]
fn encoding_operation_ownership_decides_whether_the_spelling_survives() {
    let src = generate(
        "credential = [idx: uint]\n\
         stake_credential = credential\n\
         tagged_creds = #6.11([* stake_credential])\n\
         holder = [f: #6.9(stake_credential), g: tagged_creds, h: stake_credential, i: bytes .cbor stake_credential]\n",
        "op_ownership",
        PRESERVE,
    )
    .expect("must generate");

    // The fixture's own premises: `f` must really carry a MEMBER-owned tag over an alias, and `g` a
    // RULE-owned one, or the two halves below are testing nothing.
    for (field, ty) in [
        ("pub f:", "StakeCredential,"),
        ("pub g:", "TaggedCreds,"),
        ("pub h:", "StakeCredential,"),
        ("pub i:", "StakeCredential,"),
    ] {
        assert!(
            src.contains(&format!("{field} {ty}")),
            "fixture premise: expected `{field} {ty}` — a member-expression tag must keep the \
             alias on the field, or this pin is vacuous:\n{src}"
        );
    }

    // LIFT — the tag is the MEMBER's, so the alias denotes the value read inside it, and the call
    // target now agrees with the field's own type.
    assert!(
        src.contains("(9, tag_enc) => Ok((StakeCredential::deserialize(raw)?, Some(tag_enc)))"),
        "a member-expression tag over an alias must LIFT the declared spelling — the alias denotes \
         exactly the value read inside the tag:\n{src}"
    );

    // LIFT — a `.cbor` written in the MEMBER's own type expression is the member's operation too,
    // so the payload's alias denotes the payload and survives the descent.
    assert!(
        src.contains("StakeCredential::deserialize(inner_de)"),
        "a member-expression `.cbor` over an alias must LIFT at the payload read:\n{src}"
    );

    // SEAL — the tag on `g` is the alias RULE's, so `TaggedCreds` names the tagged array and does
    // not denote the position that reads the array body; the member is read structurally instead.
    assert!(
        !src.contains("TaggedCreds::deserialize"),
        "a rule-owned encoding alias must never spell its own ident inside the operation it \
         names:\n{src}"
    );
}
