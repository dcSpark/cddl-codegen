//! Generic defs with COLLECTION bodies must generate without panicking.
//!
//! A generic def whose body is an array/map (`xs<T> = [* T]`, `{* k => T}`, tagged or not, empty
//! or non-empty), once instanced (`xs_int = xs<uint>`), registers a TRANSPARENT structural type
//! alias for the instance (`pub type XsInt = Vec<u64>;`). `scope_references`' type-alias walk once
//! assumed EVERY alias keyed by a generic-instance ident was a generic-EXTERN instance (whose base
//! is the opaque `<Base>[RawBytes]<Args>` type expression) and asserted that shape — so a collection
//! instance's `Array`/`Map` base tripped the assert at generation time. Record-bodied generic defs
//! (the only shape `tests/corpus/generics.cddl` exercises) register no instance alias and so never
//! reached that walk, which is why the regression went unseen. These tests drive the full in-process
//! generation pipeline (`api::generated_strings`, reaching `scope_references`) and assert it
//! succeeds for every collection shape; the record-bodied case is kept as the boundary regression.

use crate::cli::Cli;
use clap::Parser;

/// Run the whole generation pipeline in-process (no disk output, no nested cargo) and return the
/// emitted source keyed by file, or the graceful error string. Reaches `scope_references` — the
/// site the collection-generic fix lives in — so a regression there surfaces as a panic here.
fn generate(
    spec: &str,
    tag: &str,
    extra_flags: &[&str],
) -> Result<std::collections::BTreeMap<String, String>, String> {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_gencoll_{}_{}.cddl",
        tag,
        std::process::id()
    ));
    std::fs::write(&path, spec).unwrap();
    let mut args = vec![
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "generic_collection_unused",
    ];
    args.extend_from_slice(extra_flags);
    let cli = Cli::parse_from(args);
    let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
    std::fs::remove_file(&path).ok();
    result
}

/// Every collection-bodied generic def + instance + use-site generates cleanly. Each `(def, tag)`
/// row is a body shape the fix must cover; each flag profile is one generation path (the panic was
/// flag-independent, so both are asserted). Before the fix every row panics in `scope_references`;
/// after it, all generate.
#[test]
fn collection_bodied_generic_defs_generate() {
    let shapes = [
        ("xs<a0> = [* a0]", "array_star"),
        ("xs<a0> = [+ a0]", "array_plus"),
        ("xs<a0> = #6.258([* a0])", "tagged_array_star"),
        ("xs<a0> = #6.258([+ a0])", "tagged_array_plus"),
        ("xs<a0> = {* uint => a0}", "map_star"),
        ("xs<a0> = {+ uint => a0}", "map_plus"),
    ];
    let profiles: &[(&str, &[&str])] = &[
        ("default", &["--wasm", "false"]),
        (
            "preserve_canonical",
            &[
                "--preserve-encodings=true",
                "--canonical-form=true",
                "--wasm",
                "false",
            ],
        ),
        // the wasm pass reaches the same alias walk; cover it once so a wasm-only regression can't hide
        ("wasm", &["--wasm", "true"]),
    ];
    for (def, shape_tag) in shapes {
        let spec = format!("{def}\nxs_int = xs<uint>\nuses = [a: xs_int]\n");
        for (profile_tag, flags) in profiles {
            let tag = format!("{shape_tag}_{profile_tag}");
            let result = generate(&spec, &tag, flags);
            assert!(
                result.is_ok(),
                "collection-bodied generic `{def}` ({profile_tag}) must generate, got: {:?}",
                result.err()
            );
        }
    }
}

/// The instance of a collection-bodied generic resolves to the SAME transparent alias a bare
/// collection rule of the resolved element produces (`Vec<u64>` for `[*]`, `NonEmptyVec<u64>` for
/// `[+]`) — i.e. it falls through to the normal collection-alias path rather than the generic-extern
/// decomposition. Pins the intended shape so the fix can't regress into dropping the alias.
#[test]
fn collection_generic_instance_is_a_transparent_alias() {
    let star = generate(
        "xs<a0> = [* a0]\nxs_int = xs<uint>\nuses = [a: xs_int]\n",
        "alias_star",
        &["--wasm", "false"],
    )
    .expect("array-star generic must generate");
    let star_src = star.into_values().collect::<Vec<_>>().join("\n");
    assert!(
        star_src.contains("pub type XsInt = Vec<u64>;"),
        "`xs<uint>` of `xs<a0> = [* a0]` should be a transparent `Vec<u64>` alias; got:\n{star_src}"
    );

    let plus = generate(
        "xs<a0> = [+ a0]\nxs_int = xs<uint>\nuses = [a: xs_int]\n",
        "alias_plus",
        &["--wasm", "false"],
    )
    .expect("array-plus generic must generate");
    let plus_src = plus.into_values().collect::<Vec<_>>().join("\n");
    assert!(
        plus_src.contains("pub type XsInt = NonEmptyVec<u64>;"),
        "`xs<uint>` of `xs<a0> = [+ a0]` should be a transparent `NonEmptyVec<u64>` alias; got:\n{plus_src}"
    );
}

/// The value of the `rust/src/generated/<basename>` file, or "" if absent (e.g. `cbor_encodings.rs`
/// only exists under `--preserve-encodings`).
fn file_ending(files: &std::collections::BTreeMap<String, String>, basename: &str) -> String {
    files
        .iter()
        .find(|(k, _)| k.ends_with(basename))
        .map(|(_, v)| v.clone())
        .unwrap_or_default()
}

/// A field typed as a generic-instance transparent collection alias must emit the SAME inline
/// collection code the NON-GENERIC equivalent emits — not `self.field.serialize(..)` /
/// `Alias::deserialize(..)` method calls, which a bare `Vec`/`NonEmptyVec` alias has no impls for
/// (so they never compile). This is the convergence the fix delivers: the generic instance's alias
/// is registered only at finalize (after use-site fields were parsed), so the field kept an
/// unresolved `Rust(instance)` type; re-resolving it onto the alias's `Alias(ident, Array)` shape
/// routes both paths through the single collection code path. Before the fix the generic serialize
/// carries the method calls and the two sources differ; after it they are identical modulo the alias
/// name. Covers the Phase-2 collapsed tagged-or-untagged choice AND the single-arm tagged form,
/// `[*]` and `[+]`, plain and preserve+canonical.
#[test]
fn generic_instance_collection_field_converges_with_nongeneric() {
    // (generic def, non-generic rule, tag) — same element (`uint`) both sides, so the only expected
    // source difference is the alias name (`XsInt` vs `Foo`).
    let cases: &[(&str, &str, &str)] = &[
        (
            "xs<a0> = #6.258([* a0]) / [* a0]",
            "foo = #6.258([* uint]) / [* uint]",
            "choice_star",
        ),
        (
            "xs<a0> = #6.258([+ a0]) / [+ a0]",
            "foo = #6.258([+ uint]) / [+ uint]",
            "choice_plus",
        ),
        (
            "xs<a0> = #6.258([* a0])",
            "foo = #6.258([* uint])",
            "tagged_star",
        ),
        (
            "xs<a0> = #6.258([+ a0])",
            "foo = #6.258([+ uint])",
            "tagged_plus",
        ),
        ("xs<a0> = [* a0]", "foo = [* uint]", "plain_star"),
        ("xs<a0> = [+ a0]", "foo = [+ uint]", "plain_plus"),
    ];
    let profiles: &[(&str, &[&str])] = &[
        ("default", &["--wasm", "false"]),
        (
            "preserve_canonical",
            &[
                "--preserve-encodings=true",
                "--canonical-form=true",
                "--wasm",
                "false",
            ],
        ),
    ];
    for (gdef, nrule, ctag) in cases {
        for (ptag, flags) in profiles {
            let g = generate(
                &format!("{gdef}\nxs_int = xs<uint>\nuses = [a: xs_int]\n"),
                &format!("conv_g_{ctag}_{ptag}"),
                flags,
            )
            .unwrap_or_else(|e| panic!("generic `{gdef}` ({ptag}) must generate: {e:?}"));
            let n = generate(
                &format!("{nrule}\nuses = [a: foo]\n"),
                &format!("conv_n_{ctag}_{ptag}"),
                flags,
            )
            .unwrap_or_else(|e| panic!("non-generic `{nrule}` ({ptag}) must generate: {e:?}"));

            let g_ser = file_ending(&g, "serialization.rs");
            // The exact pre-fix breakage: a bare-`Vec` alias field routed through method calls.
            assert!(
                !g_ser.contains("self.a.serialize("),
                "generic `{gdef}` ({ptag}) must inline the collection, not call `self.a.serialize()`:\n{g_ser}"
            );
            assert!(
                !g_ser.contains("XsInt::deserialize("),
                "generic `{gdef}` ({ptag}) must inline the collection, not call `XsInt::deserialize()`:\n{g_ser}"
            );

            // Full behavioral convergence: the serialize/deserialize impls and the encoding struct
            // are byte-identical to the non-generic equivalent once the alias names are unified.
            let norm = |s: &str| s.replace("XsInt", "ALIAS").replace("Foo", "ALIAS");
            for basename in ["serialization.rs", "cbor_encodings.rs"] {
                assert_eq!(
                    norm(&file_ending(&g, basename)),
                    norm(&file_ending(&n, basename)),
                    "generic vs non-generic `{basename}` diverged for `{gdef}` ({ptag})"
                );
            }
            // mod.rs carries the same declarations, but the generic instance's alias is registered at
            // finalize (vs parse for the named rule), so the `pub type` line and the consumer struct
            // land in a different relative order. That ordering is immaterial (deterministic per input,
            // both compile), so compare the type declarations order-independently.
            let decls = |s: &str| {
                let mut lines: Vec<String> = norm(s)
                    .lines()
                    .map(|l| l.trim().to_owned())
                    .filter(|l| !l.is_empty())
                    .collect();
                lines.sort();
                lines
            };
            assert_eq!(
                decls(&file_ending(&g, "mod.rs")),
                decls(&file_ending(&n, "mod.rs")),
                "generic vs non-generic `mod.rs` declarations diverged for `{gdef}` ({ptag})"
            );
        }
    }
}

/// Under `--preserve-encodings`, the consumer's encoding struct must carry the collection's
/// tag/len/elem encoding vars for a generic-instance field, exactly as the non-generic path does —
/// otherwise the tagged and untagged wire arms could not roundtrip byte-exact. Pins the tag-presence
/// var specifically (the Phase-2 `TagPresenceEncoding`), which the pre-fix bare-alias field omitted.
#[test]
fn generic_instance_collection_field_carries_preserve_encoding_vars() {
    let files = generate(
        "xs<a0> = #6.258([* a0]) / [* a0]\nxs_int = xs<uint>\nuses = [a: xs_int]\n",
        "preserve_vars",
        &[
            "--preserve-encodings=true",
            "--canonical-form=true",
            "--wasm",
            "false",
        ],
    )
    .expect("must generate");
    let encs = file_ending(&files, "cbor_encodings.rs");
    for field in [
        "a_tag_encoding: TagPresenceEncoding",
        "a_encoding: LenEncoding",
        "a_elem_encodings",
    ] {
        assert!(
            encs.contains(field),
            "UsesEncoding must carry `{field}` for the generic-instance collection field; got:\n{encs}"
        );
    }
}

/// Boundary regression: a RECORD-bodied generic def registers no instance alias, so it never
/// reached the panicking walk and generated fine both before and after the fix. Pinning it
/// documents exactly which body shapes are (and are not) affected, so a future change to the
/// generic-instance walk can't silently start routing record instances through it.
#[test]
fn record_bodied_generic_def_still_generates() {
    let result = generate(
        "xs<a0> = [x: a0]\nxs_int = xs<uint>\nuses = [a: xs_int]\n",
        "record_body",
        &["--preserve-encodings=true", "--wasm", "false"],
    );
    assert!(
        result.is_ok(),
        "record-bodied generic must keep generating, got: {:?}",
        result.err()
    );
}
