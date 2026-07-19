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
