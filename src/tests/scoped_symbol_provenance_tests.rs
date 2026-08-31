//! Local-tier matrix for exact-source generic bindings.  The production conversion function owns
//! the normalization pairs; this table deliberately derives its collision predicate from it rather
//! than freezing a second spelling table in the tests.

use std::sync::atomic::{AtomicUsize, Ordering};

use clap::Parser;

use crate::{
    api,
    cli::Cli,
    intermediate::{ConceptualRustType, GenericParamBinding, RustIdent, RustStructType},
    utils::convert_to_camel_case,
};

static NEXT_SCRATCH: AtomicUsize = AtomicUsize::new(0);

const NORMALIZATION_PAIRS: &[(&str, &str)] =
    &[("a", "A"), ("foo-bar", "foo_bar"), ("foo-bar", "fooBar")];

const SEAMS: &[(&str, &str, &str)] = &[
    (
        "homogeneous occurrence",
        "[* p]",
        "pub type SubjectUint = Vec<u64>;",
    ),
    (
        "tag payload",
        "[value: #6.10(p)]",
        "pub struct SubjectUint {",
    ),
    (
        "homogeneous table key",
        "{ * p => uint }",
        "pub type SubjectUint = BTreeMap<u64, u64>;",
    ),
    (
        "homogeneous table value",
        "{ * uint => p }",
        "pub type SubjectUint = BTreeMap<u64, u64>;",
    ),
];

#[derive(Clone, Copy, Debug)]
enum Claimant {
    None,
    AuthoredType,
    AuthoredPlainGroup,
    SynthesizedGroupChoiceArm,
}

impl Claimant {
    const ALL: &[Self] = &[
        Self::None,
        Self::AuthoredType,
        Self::AuthoredPlainGroup,
        Self::SynthesizedGroupChoiceArm,
    ];

    fn source(self, name: &str) -> String {
        match self {
            Self::None => String::new(),
            Self::AuthoredType => format!("{name} = uint\n"),
            Self::AuthoredPlainGroup => format!("{name} = (x: uint)\n"),
            // The group-choice parser registers the first arm under this derived identifier.  It
            // is the production-created claimant that first exposed this class of collision.
            Self::SynthesizedGroupChoiceArm => format!("claim = [{name}: uint // z: tstr]\n"),
        }
    }
}

fn cli_for(source: &str) -> Cli {
    cli_for_flags(source, &[])
}

fn cli_for_flags(source: &str, flags: &[&str]) -> Cli {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_scoped_provenance_{}_{}",
        std::process::id(),
        NEXT_SCRATCH.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&root).expect("create scoped-provenance scratch directory");
    let input = root.join("input.cddl");
    std::fs::write(&input, source).expect("write scoped-provenance input");
    let mut args = vec![
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        input.to_str().expect("utf8 scratch path").to_owned(),
        "--output".to_owned(),
        root.join("out")
            .to_str()
            .expect("utf8 scratch output")
            .to_owned(),
        "--wasm=false".to_owned(),
    ];
    args.extend(flags.iter().map(|flag| (*flag).to_owned()));
    Cli::parse_from(args)
}

fn generated(source: &str) -> std::collections::BTreeMap<String, String> {
    api::generated_strings(&cli_for(source)).unwrap_or_else(|error| {
        panic!("the scoped-provenance cell must generate: {error}\nsource:\n{source}")
    })
}

fn rust_mod(files: &std::collections::BTreeMap<String, String>) -> &str {
    files
        .get("rust/src/generated/mod.rs")
        .expect("generated rust module")
}

fn target_projection(files: &std::collections::BTreeMap<String, String>, fragment: &str) -> String {
    let text = rust_mod(files);
    let fragment_start = text
        .find(fragment)
        .expect("target projection fragment must be present");
    let start = text[..fragment_start]
        .rfind('\n')
        .map_or(0, |index| index + 1);
    let end = text[fragment_start..]
        .find('\n')
        .map_or(text.len(), |index| fragment_start + index);
    text[start..end].to_owned()
}

fn codec_impl_from_files(
    files: &std::collections::BTreeMap<String, String>,
    name: &str,
    trait_name: &str,
) -> String {
    let text = files
        .get("rust/src/generated/serialization.rs")
        .expect("generated serialization module");
    let start = text
        .find(&format!("impl {trait_name} for {name}"))
        .expect("target codec impl");
    let rest = &text[start..];
    let end = rest.find("\nimpl ").unwrap_or(rest.len());
    rest[..end].replace(name, "$TARGET")
}

fn codec_impl(source: &str, name: &str, trait_name: &str) -> String {
    codec_impl_from_files(&generated(source), name, trait_name)
}

fn codec_impl_flags(source: &str, name: &str, trait_name: &str, flags: &[&str]) -> String {
    let files = api::generated_strings(&cli_for_flags(source, flags))
        .expect("tagged generic preserve control must generate");
    codec_impl_from_files(&files, name, trait_name)
}

#[test]
fn scoped_symbol_provenance_grid() {
    // Completeness is asserted by the product count rather than trusting a hand-maintained test
    // list.  None has no meaningful before/after order, while every real claimant is tested in
    // both source orders at every currently-supported seam.
    let expected = NORMALIZATION_PAIRS.len() * SEAMS.len() * (1 + (Claimant::ALL.len() - 1) * 2);
    let mut executed = 0usize;

    for &(parameter, claimant_name) in NORMALIZATION_PAIRS {
        assert_eq!(
            convert_to_camel_case(parameter),
            convert_to_camel_case(claimant_name),
            "the grid pair must be derived from production normalization"
        );
        for &(seam, body, target_fragment) in SEAMS {
            let subject = format!(
                "subject<{parameter}> = {}\nsubject-uint = subject<uint>\n",
                body.replace('p', parameter)
            );
            let baseline = generated(&subject);
            assert!(
                rust_mod(&baseline).contains(target_fragment),
                "no-claimant {seam} control lost its target projection:\n{}",
                rust_mod(&baseline)
            );
            let baseline_projection = target_projection(&baseline, target_fragment);

            for claimant in Claimant::ALL {
                let orders: &[bool] = match claimant {
                    Claimant::None => &[true],
                    _ => &[true, false],
                };
                for &claimant_first in orders {
                    let declaration = claimant.source(claimant_name);
                    let source = if claimant_first {
                        format!("{declaration}{subject}")
                    } else {
                        format!("{subject}{declaration}")
                    };
                    let files = generated(&source);
                    assert_eq!(
                        target_projection(&files, target_fragment),
                        baseline_projection,
                        "{claimant:?} (claimant_first={claimant_first}) changed the {seam} target projection"
                    );
                    executed += 1;
                }
            }
        }
    }
    assert_eq!(
        executed, expected,
        "every declared provenance-grid cell ran"
    );

    // The silent tag-loss bug needs non-generic codec parity controls, not merely an accepted
    // generic cell. Compare each target implementation after replacing its owner.
    let generic = "tagged<p> = [value: #6.10(p)]\ntagged-uint = tagged<uint>\n";
    let concrete = "concrete = [value: #6.10(uint)]\n";
    assert_eq!(
        codec_impl(generic, "TaggedUint", "cbor_event::se::Serialize"),
        codec_impl(concrete, "Concrete", "cbor_event::se::Serialize"),
        "generic substitution must retain the tagged occurrence's outer codec"
    );
    assert_eq!(
        codec_impl_flags(
            generic,
            "TaggedUint",
            "cbor_event::se::Serialize",
            &["--preserve-encodings=true"],
        ),
        codec_impl_flags(
            concrete,
            "Concrete",
            "cbor_event::se::Serialize",
            &["--preserve-encodings=true"],
        ),
        "the preserve encoding variables must stay on the substituted occurrence"
    );
    let nested_argument = "tagged<p> = [value: #6.10(p)]\ntagged-uint = tagged<#6.20(uint)>\n";
    assert_eq!(
        codec_impl(generic, "TaggedUint", "Deserialize"),
        codec_impl(concrete, "Concrete", "Deserialize"),
        "generic substitution must retain the tagged occurrence's outer decoder"
    );
    assert_eq!(
        codec_impl_flags(
            generic,
            "TaggedUint",
            "Deserialize",
            &["--preserve-encodings=true"],
        ),
        codec_impl_flags(
            concrete,
            "Concrete",
            "Deserialize",
            &["--preserve-encodings=true"],
        ),
        "the preserve decoding variables must stay on the substituted occurrence"
    );
    let nested = codec_impl(nested_argument, "TaggedUint", "cbor_event::se::Serialize");
    assert!(
        nested.find("write_tag(10u64)") < nested.find("write_tag(20u64)"),
        "the occurrence tag must be outer to the argument tag:\n{nested}"
    );
}

#[test]
fn exact_source_binding_survives_ir_then_substitution() {
    let source = "A = tstr\nboth<a> = [parameter: a, outer: A]\nboth-uint = both<uint>\n";
    let cli = cli_for(source);
    api::with_types(&cli, |types, _| {
        let generic = types
            .generic_def(&RustIdent::new(crate::intermediate::CDDLIdent::new("both")))
            .expect("generic definition remains inspectable in the finalized IR");
        let RustStructType::Record(original) = generic.original().variant() else {
            panic!("generic body did not lower to a record")
        };
        assert_eq!(
            original.fields[0]
                .rust_type
                .generic_param_binding
                .as_ref()
                .copied(),
            Some(GenericParamBinding::new(0)),
            "the exact lowercase parameter reference must retain its binding"
        );
        assert!(
            original.fields[1].rust_type.generic_param_binding.is_none(),
            "outer uppercase A must not be inferred as parameter a from its RustIdent"
        );
        let resolved = types
            .rust_structs()
            .get(&RustIdent::new(crate::intermediate::CDDLIdent::new(
                "both-uint",
            )))
            .expect("generic instance was resolved");
        let RustStructType::Record(resolved) = resolved.variant() else {
            panic!("generic instance did not lower to a record")
        };
        assert!(
            resolved
                .fields
                .iter()
                .all(|field| field.rust_type.generic_param_binding.is_none()),
            "finalized concrete fields must not retain parser binding provenance"
        );
        assert_eq!(
            resolved.fields[0]
                .rust_type
                .for_rust_member(types, false, &cli),
            "u64"
        );
        assert_eq!(
            resolved.fields[1]
                .rust_type
                .for_rust_member(types, false, &cli),
            "A"
        );
    })
    .expect("exact source binding fixture must finalize");
}

#[test]
fn generic_argument_config_survives_and_occurrence_config_rejects_gracefully() {
    let argument_config = "bounded<p> = [value: p]\nbounded-bytes = bounded<(bytes .size 4)>\n";
    let cli = cli_for(argument_config);
    api::with_types(&cli, |types, _| {
        let resolved = types
            .rust_structs()
            .get(&RustIdent::new(crate::intermediate::CDDLIdent::new(
                "bounded-bytes",
            )))
            .expect("bounded generic instance was resolved");
        let RustStructType::Record(record) = resolved.variant() else {
            panic!("bounded generic instance did not lower to a record")
        };
        assert_eq!(
            record.fields[0].rust_type.config.bounds,
            Some((Some(4), Some(4))),
            "argument-local configuration must survive substitution"
        );
    })
    .expect("argument-local generic configuration must finalize");

    let error = api::generated_strings(&cli_for(
        "bounded<p> = [value: p .size 3]\nbounded-uint = bounded<uint>\nbounded-bytes = bounded<(bytes .size 4)>\n",
    ))
    .expect_err("generic occurrence configuration must reject instead of panic")
    .to_string();
    assert!(
        error.contains("generic parameter occurrence has unsupported configuration"),
        "unexpected generic-config rejection: {error}"
    );
}

#[test]
fn scoped_parameter_never_materializes_a_colliding_plain_group() {
    for body in ["[* a]", "{* a => uint}", "{* uint => a}"] {
        let control = generated(&format!(
            "A = (x: uint)\nxs<b> = {}\nxs-uint = xs<uint>\nholder = {{ y: tstr // A }}\n",
            body.replace('a', "b")
        ));
        let colliding = generated(&format!(
            "A = (x: uint)\nxs<a> = {body}\nxs-uint = xs<uint>\nholder = {{ y: tstr // A }}\n"
        ));
        let target = rust_mod(&control)
            .lines()
            .find(|line| line.contains("pub type XsUint "))
            .expect("control must emit the concrete generic alias");
        assert_eq!(
            target_projection(&colliding, target),
            target_projection(&control, target),
            "a scoped parameter must not materialize its colliding outer plain group ({body})"
        );
    }
}

#[test]
fn exact_outer_plain_group_references_retain_unsupported_seam_diagnostics() {
    for claimant_first in [true, false] {
        let claimant = "A = (x: uint)\n";
        let subject = "subject<a> = [* A]\nsubject-uint = subject<uint>\n";
        let source = if claimant_first {
            format!("{claimant}{subject}")
        } else {
            format!("{subject}{claimant}")
        };
        let files = generated(&source);
        let serialization = files
            .get("rust/src/generated/serialization.rs")
            .expect("flat repeated plain group serialization module");
        assert!(
            serialization.contains("element.serialize_as_embedded_group(serializer)?")
                && !serialization.contains("element.serialize(serializer)?"),
            "homogeneous occurrence (claimant_first={claimant_first}) must retain flat wire encoding:\n{serialization}"
        );
    }

    let seams = [
        (
            "tag payload",
            "[value: #6.10(A)]",
            "a CBOR tag payload cannot be the plain group `A`",
        ),
        (
            "homogeneous table key",
            "{ * A => uint }",
            "uses the bare plain group `A` as its KEY domain",
        ),
        (
            "homogeneous table value",
            "{ * uint => A }",
            "uses the bare plain group `A` as its VALUE domain",
        ),
    ];
    for (seam, body, diagnostic) in seams {
        for claimant_first in [true, false] {
            let claimant = "A = (x: uint)\n";
            let subject = format!("subject<a> = {body}\nsubject-uint = subject<uint>\n");
            let source = if claimant_first {
                format!("{claimant}{subject}")
            } else {
                format!("{subject}{claimant}")
            };
            let error = api::generated_strings(&cli_for(&source))
                .expect_err("an exact outer plain-group reference must retain its refusal")
                .to_string();
            assert!(
                error.contains(diagnostic),
                "{seam} (claimant_first={claimant_first}) lost its real-group diagnostic: {error}"
            );
        }
    }
}

#[test]
fn inline_generic_choice_substitutes_exact_bindings_and_reuses_concrete_unions() {
    let source = "A = tstr\nchoice<a> = [value: a / A]\nchoice-uint = choice<uint>\n";
    let cli = cli_for(source);
    api::with_types(&cli, |types, _| {
        let generic = types
            .generic_def(&RustIdent::new(crate::intermediate::CDDLIdent::new(
                "choice",
            )))
            .expect("generic definition must retain its inline-choice template");
        let [template] = generic.inline_type_choices() else {
            panic!(
                "generic definition must retain exactly one inline-choice template; original: {:?}",
                generic.original()
            )
        };
        let variants = match template.template().variant() {
            RustStructType::TypeChoice { variants } | RustStructType::CStyleEnum { variants } => {
                variants
            }
            other => panic!("inline choice template was not an enum: {other:?}"),
        };
        assert_eq!(
            variants[0].rust_type().generic_param_binding,
            Some(GenericParamBinding::new(0)),
            "the direct parameter arm must retain its exact lexical binding in the template"
        );
        assert!(
            variants[1].rust_type().generic_param_binding.is_none(),
            "an outer authored A must not be inferred as parameter a from its Rust identifier"
        );

        let resolved = types
            .rust_structs()
            .get(&RustIdent::new(crate::intermediate::CDDLIdent::new(
                "choice-uint",
            )))
            .expect("concrete generic record must be registered");
        let RustStructType::Record(record) = resolved.variant() else {
            panic!("generic instance did not lower to a record")
        };
        let field = &record.fields[0].rust_type;
        assert!(field.generic_param_binding.is_none());
        assert_eq!(field.for_rust_member(types, false, &cli), "U64OrA");
        let union = types
            .rust_structs()
            .get(&RustIdent::new(crate::intermediate::CDDLIdent::new(
                "u64_or_a",
            )))
            .expect("substitution must materialize the concrete anonymous union");
        let variants = match union.variant() {
            RustStructType::TypeChoice { variants } | RustStructType::CStyleEnum { variants } => {
                variants
            }
            other => panic!("concrete inline choice was not an enum: {other:?}"),
        };
        assert_eq!(
            variants[0].rust_type().for_rust_member(types, false, &cli),
            "u64"
        );
    })
    .expect("direct generic inline choice must finalize");

    // The existing provenance denominator applies at this new seam too: every claimant that
    // normalizes to the parameter's Rust name, in either source order, must leave the concrete
    // union exactly as the no-claimant control.
    for &(parameter, claimant_name) in NORMALIZATION_PAIRS {
        let subject = format!(
            "choice<{parameter}> = [value: {parameter} / tstr]\nchoice-uint = choice<uint>\n"
        );
        let baseline = generated(&subject);
        let baseline_mod = rust_mod(&baseline);
        assert!(
            baseline_mod.contains("pub value: U64OrText,")
                && baseline_mod.contains("pub enum U64OrText")
                && baseline_mod.contains("Text(String)"),
            "direct parameter control must emit a U64 union:\n{baseline_mod}"
        );
        for claimant in Claimant::ALL
            .iter()
            .copied()
            .filter(|c| !matches!(c, Claimant::None))
        {
            for claimant_first in [true, false] {
                let declaration = claimant.source(claimant_name);
                let source = if claimant_first {
                    format!("{declaration}{subject}")
                } else {
                    format!("{subject}{declaration}")
                };
                let files = generated(&source);
                let text = rust_mod(&files);
                assert!(
                    text.contains("pub value: U64OrText,")
                        && text.contains("pub enum U64OrText")
                        && text.contains("Text(String)"),
                    "{claimant:?} (claimant_first={claimant_first}) changed the concrete generic choice:\n{text}"
                );
            }
        }
    }

    let concrete_instances = "choice<p> = [value: p / tstr]\n\
        choice-uint = choice<uint>\n\
        choice-bytes = choice<bstr>\n\
        choice-uint-again = choice<uint>\n";
    let cli = cli_for(concrete_instances);
    api::with_types(&cli, |types, _| {
        let field_type = |name: &str| {
            let record = types
                .rust_structs()
                .get(&RustIdent::new(crate::intermediate::CDDLIdent::new(name)))
                .expect("concrete generic instance")
                .variant();
            let RustStructType::Record(record) = record else {
                panic!("generic instance was not a record")
            };
            record.fields[0]
                .rust_type
                .for_rust_member(types, false, &cli)
        };
        let uint = field_type("choice-uint");
        assert_eq!(uint, field_type("choice-uint-again"));
        assert_ne!(uint, field_type("choice-bytes"));
        assert!(
            types
                .rust_structs()
                .contains_key(&RustIdent::new(crate::intermediate::CDDLIdent::new(uint))),
            "the repeated concrete union must be registered exactly once under its field identifier"
        );
    })
    .expect("distinct generic arguments must materialize distinct compatible unions");

    // Parser-only placeholders are ordinary `RustIdent`s until generic finalization. An authored
    // rule with the derived first-placeholder spelling must therefore stay an authored field in
    // either source order, never be retargeted to the concrete inline union.
    let authored = "choice_generic_inline_choice1 = bstr\n";
    let generic = "choice<p> = [authored: choice_generic_inline_choice1, value: p / tstr]\n\
        choice_uint = choice<uint>\n";
    for authored_first in [true, false] {
        let source = if authored_first {
            format!("{authored}{generic}")
        } else {
            format!("{generic}{authored}")
        };
        let files = generated(&source);
        let generated = rust_mod(&files);
        assert!(
            generated.contains("pub type ChoiceGenericInlineChoice1 = Vec<u8>;")
                && generated.contains("pub authored: ChoiceGenericInlineChoice1,")
                && generated.contains("pub value: U64OrText,"),
            "an authored placeholder-shaped rule must remain bytes (authored_first={authored_first}):\n{generated}"
        );
    }

    // The inline-choice template is a new deferred substitution seam. Its parameter-arm
    // occurrence tag must remain OUTER to the concrete argument tag just as it does for an
    // ordinary generic record field, including when preserve-encoding bookkeeping is enabled.
    let tagged_generic = "choice<p> = [value: #6.10(p) / tstr]\n\
        choice_tagged = choice<#6.20(uint)>\n";
    let tagged_concrete = "concrete = [value: #6.10(#6.20(uint)) / tstr]\n";
    let generic_codec = codec_impl(tagged_generic, "U64OrText", "cbor_event::se::Serialize");
    let concrete_codec = codec_impl(tagged_concrete, "U64OrText", "cbor_event::se::Serialize");
    // The direct generic arm intentionally retains the public `P` variant name while the concrete
    // equivalent derives `U64`; compare the tag ordering rather than that unrelated spelling.
    assert!(
        generic_codec.find("write_tag(10u64)") < generic_codec.find("write_tag(20u64)")
            && concrete_codec.find("write_tag(10u64)") < concrete_codec.find("write_tag(20u64)"),
        "deferred inline-choice substitution must retain occurrence/argument tag ordering:\n\
         generic:\n{generic_codec}\nconcrete:\n{concrete_codec}"
    );
    let preserve_codec = codec_impl_flags(
        tagged_generic,
        "U64OrText",
        "cbor_event::se::Serialize",
        &["--preserve-encodings=true"],
    );
    let concrete_preserve_codec = codec_impl_flags(
        tagged_concrete,
        "U64OrText",
        "cbor_event::se::Serialize",
        &["--preserve-encodings=true"],
    );
    assert!(
        preserve_codec.find("write_tag_sz(10u64,") < preserve_codec.find("write_tag_sz(20u64,")
            && concrete_preserve_codec.find("write_tag_sz(10u64,")
                < concrete_preserve_codec.find("write_tag_sz(20u64,"),
        "preserve encoding must keep the template occurrence tag outer to the concrete argument tag:\n\
         generic:\n{preserve_codec}\nconcrete:\n{concrete_preserve_codec}"
    );
}

#[test]
fn generic_nested_record_field_containers_substitute_parameters() {
    let source = "nested<p> = [\n\
        values: [* p],\n\
        lookup: { * p => p },\n\
        maybe: p / null,\n\
    ]\n\
    nested-uint = nested<uint>\n";
    let cli = cli_for(source);
    api::with_types(&cli, |types, _| {
        let resolved = types
            .rust_structs()
            .get(&RustIdent::new(crate::intermediate::CDDLIdent::new(
                "nested-uint",
            )))
            .expect("concrete generic record must be registered");
        let RustStructType::Record(record) = resolved.variant() else {
            panic!("generic instance did not lower to a record")
        };
        let [values, lookup, maybe] = record.fields.as_slice() else {
            panic!("nested generic record fields changed shape: {record:?}")
        };
        let ConceptualRustType::Array(element) = &values.rust_type.conceptual_type else {
            panic!("nested generic array field did not remain an array")
        };
        assert_eq!(element.for_rust_member(types, false, &cli), "u64");
        let ConceptualRustType::Map(domain, range) = &lookup.rust_type.conceptual_type else {
            panic!("nested generic table field did not remain a map")
        };
        assert_eq!(domain.for_rust_member(types, false, &cli), "u64");
        assert_eq!(range.for_rust_member(types, false, &cli), "u64");
        let ConceptualRustType::Optional(element) = &maybe.rust_type.conceptual_type else {
            panic!("nested generic optional field did not remain optional")
        };
        assert_eq!(element.for_rust_member(types, false, &cli), "u64");
    })
    .expect("nested generic field containers must substitute their parameter leaves");

    let files = generated(source);
    let generated = rust_mod(&files);
    for expected in [
        "pub values: Vec<u64>,",
        "pub lookup: BTreeMap<u64, u64>,",
        "pub maybe: Option<u64>,",
    ] {
        assert!(
            generated.contains(expected),
            "nested generic field container must use its concrete argument ({expected}):\n{generated}"
        );
    }
}

#[test]
fn generic_inline_choices_in_both_map_sides_rewrite_and_compile() {
    let source = "map_choices<p> = [lookup: { * (p / tstr) => (p / bstr) }]\n\
        map_choices_uint = map_choices<uint>\n";
    let cli = cli_for(source);
    api::with_types(&cli, |types, _| {
        let resolved = types
            .rust_structs()
            .get(&RustIdent::new(crate::intermediate::CDDLIdent::new(
                "map_choices_uint",
            )))
            .expect("concrete generic record must be registered after both map sides resolve");
        let RustStructType::Record(record) = resolved.variant() else {
            panic!("generic map-choice instance did not lower to a record")
        };
        let ConceptualRustType::Map(domain, range) = &record.fields[0].rust_type.conceptual_type
        else {
            panic!("map-choice field did not remain a map")
        };
        assert_eq!(domain.for_rust_member(types, false, &cli), "U64OrText");
        assert_eq!(range.for_rust_member(types, false, &cli), "U64OrBytes");
        assert!(
            types
                .rust_structs()
                .keys()
                .all(|ident| !ident.to_string().contains("GenericInlineChoice")),
            "both parser-only placeholders must be rewritten before registration"
        );
    })
    .expect("both deferred map-side choices must finalize");

    let files = generated(source);
    let generated = files.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        generated.contains("pub lookup: BTreeMap<U64OrText, U64OrBytes>,")
            && !generated.contains("GenericInlineChoice"),
        "both map-side choices must reach emitted source as concrete unions:\n{generated}"
    );
}

#[test]
fn generic_inline_choice_nested_shapes_generate_and_keep_remaining_boundaries_loud() {
    for body in [
        "p / tstr",
        "#6.10(p) / tstr",
        "(p) / tstr",
        "[* p] / tstr",
        "{ * p => uint } / tstr",
        "{ * uint => p } / tstr",
    ] {
        let files = generated(&format!(
            "choice<p> = [value: {body}]\nchoice-uint = choice<uint>\n"
        ));
        let generated = files.values().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            !generated.contains("GenericInlineChoice"),
            "the parser-only template placeholder must never reach generated source ({body}):\n{generated}"
        );
    }

    let nested_choice =
        generated("choice<p> = [value: (p / tstr) / bstr]\nchoice-uint = choice<uint>\n");
    let nested_generated = nested_choice
        .values()
        .cloned()
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        !nested_generated.contains("GenericInlineChoice")
            && nested_generated.contains("pub enum U64OrText")
            && nested_generated.contains("pub enum U64OrTextOrBytes"),
        "nested generic inline choices must resolve their definition-owned template dependencies:\n{nested_generated}"
    );

    let nested_child = generated(
        "inner<x> = [x]\nchoice<p> = [value: inner<p> / tstr]\nchoice-uint = choice<uint>\n",
    );
    assert!(
        rust_mod(&nested_child).contains("pub struct InnerU64")
            && rust_mod(&nested_child).contains("pub enum InnerU64OrText")
            && !rust_mod(&nested_child).contains("GenericChildInstance"),
        "nested generic application must materialize its concrete child before the inline choice:\n{}",
        rust_mod(&nested_child)
    );

    let cross_template_error = api::generated_strings(&cli_for(
        "inner<x> = [x]\nchoice<p> = [value: inner<(p / tstr)>]\nchoice-uint = choice<uint>\n",
    ))
    .expect_err("an inline choice passed as a child generic argument stays a narrow refusal")
    .to_string();
    assert!(
        cross_template_error.contains("definition-owned inline type choice"),
        "cross-template child argument must remain explicit: {cross_template_error}"
    );

    let group_error =
        api::generated_strings(&cli_for("A = (x: uint)\nchoice = [value: A / tstr]\n"))
            .expect_err("real plain group type-choice arm must retain its established rejection")
            .to_string();
    assert!(
        group_error.contains("a type-choice arm cannot be the plain group `A`"),
        "real-group TYPE-choice control changed diagnostic: {group_error}"
    );

    for source in [
        "u64_or_text = uint .le 10 / tstr\nchoice<p> = [value: p / tstr]\nchoice-uint = choice<uint>\n",
        "choice<p> = [value: p / tstr]\nchoice-uint = choice<uint>\nu64_or_text = uint .le 10 / tstr\n",
    ] {
        let error = api::generated_strings(&cli_for(source))
            .expect_err("an authored union keeps its anonymous-choice collision authority")
            .to_string();
        assert!(
            error.contains("generated Rust type `U64OrText` has incompatible registrations"),
            "authored collision must remain loud and deterministic: {error}"
        );
    }
}

#[test]
fn definition_owned_nested_generic_children_materialize_through_the_ordinary_instance_queue() {
    let cases: &[(&str, &str, &[&str])] = &[
        (
            "direct record field",
            "inner<x> = [x]\nouter<p> = [value: inner<p>]\nouter-uint = outer<uint>\n",
            &[
                "pub struct InnerU64",
                "pub struct OuterUint",
                "pub value: InnerU64",
            ],
        ),
        (
            "direct record field with forward child definition",
            "outer<p> = [value: inner<p>]\nouter-uint = outer<uint>\ninner<x> = [x]\n",
            &[
                "pub struct InnerU64",
                "pub struct OuterUint",
                "pub value: InnerU64",
            ],
        ),
        (
            "repeated and distinct child arguments",
            "inner<x> = [x]\nouter<p> = [left: inner<p>, right: inner<p>]\nouter-uint = outer<uint>\nouter-text = outer<tstr>\n",
            &[
                "pub struct InnerU64",
                "pub struct InnerText",
                "pub left: InnerU64",
                "pub right: InnerU64",
            ],
        ),
        (
            "two outer arguments",
            "pair<a, b> = [a, b]\nouter<p, q> = [value: pair<p, q>]\nouter-uint-text = outer<uint, tstr>\n",
            &[
                "pub struct PairU64Text",
                "pub struct OuterUintText",
                "pub value: PairU64Text",
            ],
        ),
        (
            "two-level child chain",
            "leaf<x> = [x]\nmid<x> = [value: leaf<x>]\nouter<p> = [value: mid<p>]\nouter-uint = outer<uint>\n",
            &[
                "pub struct LeafU64",
                "pub struct MidU64",
                "pub struct OuterUint",
            ],
        ),
        (
            "transparent collection child",
            "items<x> = [* x]\nouter<p> = [value: items<p>]\nouter-uint = outer<uint>\n",
            &["pub type ItemsU64 = Vec<u64>;", "pub value: ItemsU64"],
        ),
    ];
    for (name, source, expected) in cases {
        let files = generated(source);
        let module = rust_mod(&files);
        for fragment in (*expected).iter() {
            assert!(
                module.contains(fragment),
                "{name} lost `{fragment}`:\n{module}"
            );
        }
        assert!(
            !module.contains("GenericChildInstance") && !module.contains("GenericInlineChoice"),
            "{name} leaked a parser-private deferred placeholder:\n{module}"
        );
    }

    let source = "inner<x> = [x]\nouter<p> = [value: inner<p>]\nouter-uint = outer<uint>\n";
    api::with_types(&cli_for(source), |types, _| {
        let generic = types
            .generic_def(&RustIdent::new(crate::intermediate::CDDLIdent::new("outer")))
            .expect("outer generic definition");
        assert_eq!(
            generic.child_instances().len(),
            1,
            "the parser must retain one definition-owned child rather than globally registering InnerP"
        );
        assert!(
            types.rust_structs().keys().all(|ident| {
                !ident.to_string().contains("GenericChildInstance")
                    && !ident.to_string().contains("GenericInlineChoice")
            }),
            "finalized products must contain no deferred placeholders"
        );
    })
    .expect("direct nested child must finalize without the old generation panic");
}

#[test]
fn nested_child_instance_cannot_overwrite_a_completed_incompatible_instance() {
    let outer = "foo<x> = [x]\nouter<p> = [value: foo<p>]\nouter-uint = outer<uint>\n";
    let conflicting_binding = "foo-u64 = bar<uint>\nbar<x> = [left: x, right: x]\n";
    for source in [
        format!("{outer}{conflicting_binding}"),
        format!("{conflicting_binding}{outer}"),
    ] {
        let error = api::generated_strings(&cli_for(&source))
            .expect_err("an incompatible completed generic instance must not be overwritten")
            .to_string();
        assert!(
            error.contains("generated Rust type `FooU64` has incompatible registrations")
                && error.contains("generic instance of `Bar`")
                && error.contains("generic instance of `Foo`"),
            "the completed-instance collision must be loud rather than silently miscompile: {error}"
        );
    }
}

#[test]
fn nested_child_templates_keep_exact_bindings_and_recursive_argument_identity() {
    // The child-template path must use the same exact-source authority as direct parameter
    // occurrences: a normalized claimant is never substituted for the lexical parameter.
    for &(parameter, claimant_name) in NORMALIZATION_PAIRS {
        for claimant in Claimant::ALL {
            let orders: &[bool] = match claimant {
                Claimant::None => &[true],
                _ => &[true, false],
            };
            for &claimant_first in orders {
                let subject = format!(
                    "inner<x> = [x]\nouter<{parameter}> = [value: inner<{parameter}>]\nouter-uint = outer<uint>\n"
                );
                let declaration = claimant.source(claimant_name);
                let source = if claimant_first {
                    format!("{declaration}{subject}")
                } else {
                    format!("{subject}{declaration}")
                };
                let files = generated(&source);
                let module = rust_mod(&files);
                assert!(
                    module.contains("pub value: InnerU64"),
                    "{claimant:?} (claimant_first={claimant_first}) must not capture `{parameter}` as `{claimant_name}`:\n{module}"
                );
            }
        }
    }

    // Each recursively carried argument resolves before the child identity is derived. The tagged
    // and bounded versions intentionally differ only in argument-local codec/configuration, so one
    // canonical fragment cannot overwrite the other.
    let source = "inner<x> = [x]\nouter<p> = [tagged: inner<#6.10([* p])>, bounded: inner<([*2 p])>]\nouter-uint = outer<uint>\n";
    let files = generated(source);
    let module = rust_mod(&files);
    let child_structs = module
        .lines()
        .filter(|line| line.starts_with("pub struct Inner"))
        .count();
    assert!(
        child_structs >= 2,
        "tagged/configured recursive arguments need distinct concrete child identities:\n{module}"
    );
    assert!(
        module.contains("write_tag(10u64)")
            || files.values().any(|file| file.contains("write_tag(10u64)")),
        "the tagged child argument must retain its codec operation"
    );

    api::with_types(&cli_for(source), |types, _| {
        fn has_binding(ty: &crate::intermediate::RustType) -> bool {
            if ty.generic_param_binding.is_some() {
                return true;
            }
            match &ty.conceptual_type {
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    has_binding(inner)
                }
                ConceptualRustType::Map(key, value) => has_binding(key) || has_binding(value),
                _ => false,
            }
        }
        for rust_struct in types.rust_structs().values() {
            match rust_struct.variant() {
                RustStructType::Record(record) => assert!(
                    record
                        .fields
                        .iter()
                        .all(|field| !has_binding(&field.rust_type)),
                    "finalized record `{}` retains a generic binding",
                    rust_struct.ident()
                ),
                RustStructType::Table { domain, range, .. } => assert!(
                    !has_binding(domain) && !has_binding(range),
                    "finalized table `{}` retains a generic binding",
                    rust_struct.ident()
                ),
                RustStructType::Array { element_type, .. } => assert!(
                    !has_binding(element_type),
                    "finalized array `{}` retains a generic binding",
                    rust_struct.ident()
                ),
                RustStructType::Wrapper { wrapped, .. } => assert!(
                    !has_binding(wrapped),
                    "finalized wrapper `{}` retains a generic binding",
                    rust_struct.ident()
                ),
                RustStructType::TypeChoice { variants }
                | RustStructType::CStyleEnum { variants } => {
                    for variant in variants {
                        if let crate::intermediate::EnumVariantData::RustType(ty) = &variant.data {
                            assert!(
                                !has_binding(ty),
                                "finalized choice `{}` retains a generic binding",
                                rust_struct.ident()
                            );
                        }
                    }
                }
                RustStructType::GroupChoice { .. }
                | RustStructType::Extern
                | RustStructType::RawBytesType => {}
            }
        }
    })
    .expect("recursive nested child arguments must fully substitute their bindings");
}
