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
            // A 258 set NOMINALIZES on BOTH sides now (Phase 2.3: the generic instantiation mints a
            // nominal exactly like the non-generic named rule), so BOTH consumers DELEGATE to their
            // nominal (`self.a.serialize()`); parity is restored. A non-258 (plain) collection
            // nominalizes on neither side, so both INLINE and converge byte-identically modulo name.
            let nominalizes = nrule.contains("258");
            let n_ser = file_ending(&n, "serialization.rs");
            let norm = |s: &str| s.replace("XsInt", "ALIAS").replace("Foo", "ALIAS");
            if nominalizes {
                assert!(
                    g_ser.contains("self.a.serialize("),
                    "the generic 258 instantiation must nominalize and delegate `self.a.serialize()` \
                     for `{gdef}` ({ptag}):\n{g_ser}"
                );
                assert!(
                    n_ser.contains("self.a.serialize("),
                    "the non-generic 258 set must nominalize and delegate `self.a.serialize()` \
                     for `{nrule}` ({ptag}):\n{n_ser}"
                );
            } else {
                assert!(
                    !g_ser.contains("self.a.serialize(") && !g_ser.contains("XsInt::deserialize("),
                    "a non-258 generic instance stays transparent and INLINES the collection, not \
                     `self.a.serialize()` for `{gdef}` ({ptag}):\n{g_ser}"
                );
                // Full behavioral convergence: the serialize/deserialize impls and the encoding struct
                // are byte-identical to the non-generic equivalent once the alias names are unified.
                for basename in ["serialization.rs", "cbor_encodings.rs"] {
                    assert_eq!(
                        norm(&file_ending(&g, basename)),
                        norm(&file_ending(&n, basename)),
                        "generic vs non-generic `{basename}` diverged for `{gdef}` ({ptag})"
                    );
                }
                // mod.rs carries the same declarations, but the generic instance's alias is registered
                // at finalize (vs parse for the named rule), so the `pub type` line and the consumer
                // struct land in a different relative order. That ordering is immaterial (deterministic
                // per input, both compile), so compare the type declarations order-independently.
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
}

/// Under `--preserve-encodings`, a 258 generic instantiation NOMINALIZES (Phase 2.3), so its
/// tag/len/elem encoding vars move OFF the consumer's encoding struct and INTO the instantiation
/// nominal's OWN encoding struct (`XsU64Encoding`) — exactly as the non-generic named nominal does.
/// The consumer's field just delegates. Pins the tag-presence var specifically (the
/// `TagPresenceEncoding`), which the nominal owns.
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
    // The nominal `XsU64` owns its `{tag, len, elem}` encoding record; the consumer no longer carries
    // per-field `a_*` collection encoding vars.
    for field in [
        "inner_tag_encoding: TagPresenceEncoding",
        "inner_encoding: LenEncoding",
        "inner_elem_encodings",
    ] {
        assert!(
            encs.contains(field),
            "XsU64Encoding must carry `{field}` on the instantiation nominal; got:\n{encs}"
        );
    }
    assert!(
        encs.contains("pub struct XsU64Encoding"),
        "the instantiation nominal must own its encoding struct; got:\n{encs}"
    );
    assert!(
        !encs.contains("a_tag_encoding"),
        "the consumer must NOT flatten the set's tag encoding onto its own field; got:\n{encs}"
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

/// Phase 2.3: an ANONYMOUS collapsed-set instance at a field site (`[pool_owners: set<key_hash>]`,
/// element a non-exposable `@newtype`) NOMINALIZES per instantiation — `set<key_hash>` mints ONE
/// nominal `SetKeyHash` (rust wrapper struct + a `#[wasm_bindgen]` class over it), NOT a transparent
/// alias converging onto the structural `KeyHashList`. The inner reject/preserve twin still rides the
/// structural boundary class (`KeyHashList` for the preserve `Vec`), so that structural wrapper still
/// exists — but the instance is its OWN nominal concept now.
#[test]
fn anonymous_collapsed_set_instance_nominalizes() {
    // Explicit `; @duplicates preserve` opt-out keeps the inner on the plain `Vec`/`KeyHashList`
    // representation (tag 258 now defaults to `@duplicates reject`); the nominalization under test is
    // orthogonal to the duplicates policy.
    let spec = "key_hash = bytes ; @newtype\n\
                set<a0> = #6.258([* a0]) / [* a0] ; @duplicates preserve\n\
                cert = [pool_owners: set<key_hash>]\n";
    let files = generate(
        spec,
        "anon_set",
        &["--wasm", "true", "--preserve-encodings=true"],
    )
    .expect("anonymous collapsed-set instance must generate");
    let wasm = file_ending(&files, "wasm/src/generated/mod.rs");
    assert!(
        wasm.contains("pub struct SetKeyHash(pub(crate) cddl_lib::SetKeyHash)"),
        "the anonymous instance mints its OWN nominal wasm class over the rust nominal:\n{wasm}"
    );
    assert!(
        !wasm.contains("pub type SetKeyHash = KeyHashList;"),
        "the nominal must NOT be a passthrough alias to the structural class any more:\n{wasm}"
    );
    // rust side nominalizes too: a wrapper struct, not a transparent alias.
    let rust = file_ending(&files, "rust/src/generated/mod.rs");
    assert!(
        rust.contains("pub struct SetKeyHash {")
            && rust.contains("pub(crate) inner: Vec<KeyHash>,")
            && !rust.contains("pub type SetKeyHash ="),
        "the rust-side instance nominalizes to a wrapper over the preserve `Vec` inner:\n{rust}"
    );
}

/// The `[+]` flavor of the anonymous collapse: `nonempty_set<key_hash>` likewise nominalizes to
/// `NonemptySetKeyHash` (over the restricted `NonEmptyVec` inner), never a passthrough alias to the
/// structural `NonEmptyKeyHashList`.
#[test]
fn anonymous_collapsed_nonempty_set_nominalizes() {
    let spec = "key_hash = bytes ; @newtype\n\
                nonempty_set<a0> = #6.258([+ a0]) / [+ a0] ; @duplicates preserve\n\
                signers = [required: nonempty_set<key_hash>]\n";
    let files = generate(
        spec,
        "anon_neset",
        &["--wasm", "true", "--preserve-encodings=true"],
    )
    .expect("anonymous nonempty collapsed-set instance must generate");
    let wasm = file_ending(&files, "wasm/src/generated/mod.rs");
    assert!(
        wasm.contains("pub struct NonemptySetKeyHash(pub(crate) cddl_lib::NonemptySetKeyHash)"),
        "the nonempty anonymous instance mints its own nominal wasm class:\n{wasm}"
    );
    assert!(
        !wasm.contains("pub type NonemptySetKeyHash = NonEmptyKeyHashList;"),
        "the nonempty nominal must NOT be a passthrough alias any more:\n{wasm}"
    );
}

/// Two spellings of one anonymous collapsed-set shape — the generic instance `set<key_hash>` AND the
/// inline `[* key_hash]` — must define exactly ONE `KeyHashList` wasm class, not two.
#[test]
fn anonymous_instance_and_inline_collapsed_set_are_one_wasm_class() {
    // Explicit `; @duplicates preserve` opt-out: the "two spellings, ONE class" convergence pins the
    // plain `KeyHashList` shape; without the opt-out the 258 default (`reject`) would give the
    // instance its own `OrderedSet` wrapper, diverging from the inline `[* key_hash]` twin.
    let spec = "key_hash = bytes ; @newtype\n\
                set<a0> = #6.258([* a0]) / [* a0] ; @duplicates preserve\n\
                cert = [pool_owners: set<key_hash>, extra: [* key_hash]]\n";
    let files = generate(
        spec,
        "anon_both",
        &["--wasm", "true", "--preserve-encodings=true"],
    )
    .expect("both spellings must generate");
    let wasm = file_ending(&files, "wasm/src/generated/mod.rs");
    assert_eq!(
        wasm.matches("pub struct KeyHashList(").count(),
        1,
        "the anonymous instance and the inline `[* key_hash]` must be ONE KeyHashList class:\n{wasm}"
    );
}

/// Phase 2.3: even the directly-EXPOSABLE cell nominalizes — a `set<uint>` instance mints its own
/// nominal `SetU64` (rust wrapper + wasm class), DIVERGING from the inline `[* uint]` equivalent
/// (which stays a bare `Vec<u64>`). The author's spelling is the identity: `set<uint>` is a named
/// concept, `[* uint]` is an anonymous shape. The field getter returns the nominal wasm class, and
/// the field type is the nominal (not the bare `Vec`).
#[test]
fn anonymous_exposable_instance_nominalizes() {
    // Explicit `; @duplicates preserve` opt-out keeps the inner on the plain `Vec<u64>` (tag 258 now
    // defaults to `reject`); the nominalization under test is orthogonal to the duplicates policy.
    let instance = generate(
        "set<a0> = #6.258([* a0]) / [* a0] ; @duplicates preserve\ncert = [nums: set<uint>]\n",
        "anon_expo_inst",
        &["--wasm", "true", "--preserve-encodings=true"],
    )
    .expect("exposable anonymous set instance must generate");
    let inline = generate(
        "cert = [nums: [* uint]]\n",
        "anon_expo_inline",
        &["--wasm", "true", "--preserve-encodings=true"],
    )
    .expect("inline equivalent must generate");
    // The instance no longer matches the inline equivalent: it mints a nominal class the inline lacks.
    let wasm = file_ending(&instance, "wasm/src/generated/mod.rs");
    let inline_wasm = file_ending(&inline, "wasm/src/generated/mod.rs");
    assert!(
        wasm.contains("pub struct SetU64(pub(crate) cddl_lib::SetU64)"),
        "the exposable instance must mint its own nominal SetU64 wasm class:\n{wasm}"
    );
    assert!(
        !inline_wasm.contains("SetU64"),
        "the inline `[* uint]` equivalent must NOT mint a SetU64 (it stays a bare Vec):\n{inline_wasm}"
    );
    assert!(
        wasm.contains("pub fn nums(&self) -> SetU64"),
        "the getter must return the nominal class, not a bare Vec<u64>:\n{wasm}"
    );
    // rust-side: the field is typed as the nominal wrapper, not a transparent alias.
    let rust = file_ending(&instance, "rust/src/generated/mod.rs");
    assert!(
        rust.contains("pub struct SetU64 {")
            && rust.contains("pub nums: SetU64")
            && !rust.contains("pub type SetU64 ="),
        "rust nominalizes the instance; the field is typed as the nominal:\n{rust}"
    );
}

/// Phase 2.3: a named rule BINDING an instance (`named_set = set<key_hash>`, ident `NamedSet`)
/// becomes a transparent alias TO the instantiation nominal — `pub type NamedSet = SetKeyHash;` on
/// BOTH the rust and wasm sides (the instantiation is the identity; wasm keeps ONE class + this
/// passthrough alias). It does NOT mint a second nominal for the binding spelling.
#[test]
fn named_collapsed_set_instance_rule_aliases_the_instantiation_nominal() {
    let spec = "key_hash = bytes ; @newtype\n\
                set<a0> = #6.258([* a0]) / [* a0]\n\
                named_set = set<key_hash>\n\
                cert = [pool_owners: named_set]\n";
    let files = generate(
        spec,
        "named_set",
        &["--wasm", "true", "--preserve-encodings=true"],
    )
    .expect("named collapsed-set instance rule must generate");
    let rust = file_ending(&files, "rust/src/generated/mod.rs");
    let wasm = file_ending(&files, "wasm/src/generated/mod.rs");
    assert!(
        rust.contains("pub type NamedSet = SetKeyHash;")
            && rust.contains("pub struct SetKeyHash {")
            && !rust.contains("pub struct NamedSet"),
        "the named binding aliases the instantiation nominal (rust), minting no second nominal:\n{rust}"
    );
    assert!(
        wasm.contains("pub type NamedSet = SetKeyHash;")
            && wasm.contains("pub struct SetKeyHash(pub(crate) cddl_lib::SetKeyHash)")
            && !wasm.contains("pub struct NamedSet("),
        "the named binding is a wasm passthrough alias to the ONE instantiation class:\n{wasm}"
    );
}
