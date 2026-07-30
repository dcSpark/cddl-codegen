//! The declared-type spelling rule at TYPE-DECLARATION positions, and the config-threading
//! invariant that rule depends on.
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
