//! Local/full custody of finite generator source scanners across module extractions.
//! Register a destination, its actual members and its role together with the move.
//! The local/overload roster intentionally differs from the WASM rendering roster.
use std::collections::{BTreeMap, BTreeSet};

use super::identifier_hazard_tests::{EMITTER_SOURCES, ident_at, is_ident_char, scan_rust};
use super::snapshot_tests::{EmitterFn, emitter_fns};
use super::synthesized_name_registry_tests::WASM_RENDERING_EMITTERS;

#[derive(Clone, Copy)]
struct SourceRole {
    file: &'static str,
    emitter: bool,
    wasm: bool,
    reason: &'static str,
    members: &'static [&'static str],
}

// Every current source is classified, including deliberately unscanned files.
// The registry, schema-claim, encoding, JSON-annotation and key-demand owners each
// have member anchors here and conservative coverage in EMITTER_SOURCES.
// Only actual WASM rendering owners belong in WASM_RENDERING_EMITTERS.
// The coordinator mod.rs is classified here without joining the emitter roster.
const SOURCE_ROLES: &[SourceRole] = &[
    SourceRole {
        file: "bounds.rs",
        emitter: false,
        wasm: false,
        reason: "bound expression fragments; current body scanner excludes this helper",
        members: &["bounds_check_expr"],
    },
    SourceRole {
        file: "collections.rs",
        emitter: true,
        wasm: true,
        reason: "collection bodies and WASM signatures",
        members: &["generate_array_type"],
    },
    SourceRole {
        file: "component.rs",
        emitter: false,
        wasm: false,
        reason: "wit-bindgen guest glue; no user field beside fixed local",
        members: &["component_glue"],
    },
    SourceRole {
        file: "deser_verdicts.rs",
        emitter: false,
        wasm: false,
        reason: "generator verdict bookkeeping",
        members: &["seed_no_deserialize_verdicts"],
    },
    SourceRole {
        file: "deserialize.rs",
        emitter: true,
        wasm: false,
        reason: "deserialization bodies and overload config",
        members: &[
            "deser_map",
            "deser_array",
            "deser_optional",
            "deser_alias",
            "deser_rust_ident",
            "deser_cbor_bytes",
            "deser_optionally_tagged",
            "deser_tagged",
            "deser_primitive",
            "emit_primitive_read",
            "cbor_payload_trailing_check",
            "deser_fixed",
            "deser_any",
            "generate_deserialize",
            "make_deser_loop_break_check",
            "final_expr",
            "final_result_expr_complete",
        ],
    },
    SourceRole {
        file: "encoding_fields.rs",
        emitter: true,
        wasm: false,
        reason: "encoding declarations/default/type/tuple fragments and builders; no WASM type rendering",
        members: &[
            "sz",
            "string",
            "len",
            "tag_presence",
            "sidecar",
            "enc_conversion",
            "key_encoding_field",
            "declared_encoding_fields",
            "encoding_fields",
            "field_encoding_fields",
            "default_present_encoding_field",
            "custom_codec_demand_is_empty",
            "encoding_fields_decls",
            "tag_encoding_infix",
            "tag_enc_binding",
            "cbor_level_name",
            "cbor_bytes_infix",
            "cbor_payload_buffer_suffix",
            "cbor_payload_reader",
            "cbor_payload_binding_suffix",
            "encoding_fields_impl",
            "encoding_var_names_str",
            "encoding_var_names_str_for_field",
            "tuple_str",
            "tuple_type_name",
            "encoding_defaults_all_trivial",
            "make_encoding_struct",
            "type_complexity_score",
            "score",
            "push_encoding_struct_field",
            "type_complexity_score_vectors",
        ],
    },
    SourceRole {
        file: "enums.rs",
        emitter: true,
        wasm: true,
        reason: "enum bodies and WASM signatures",
        members: &["make_enum_variant_return_if_deserialized"],
    },
    SourceRole {
        file: "export.rs",
        emitter: false,
        wasm: false,
        reason: "export/schema generator; no user field beside fixed local",
        members: &["generated_files"],
    },
    SourceRole {
        file: "extern_interface.rs",
        emitter: false,
        wasm: false,
        reason: "extern interface CDDL rendering; separate annotation scan",
        members: &["render_rust_type"],
    },
    SourceRole {
        file: "json_annotations.rs",
        emitter: true,
        wasm: false,
        reason: "audited conservative serde/schema attribute and descriptor fragments; no WASM rendering",
        members: &[
            "natural_any_position",
            "natural_any_serde_annotations",
            "recursive_exact_array_descriptor",
            "alias_base",
            "contains_exact_natural_any",
            "contains_wide_static_array",
            "contains_exact_node",
            "is_legacy_direct_typed_static_array_sequence",
            "shape",
            "dynamic_row_exact_array_descriptor",
            "static_array_serde_annotations",
            "static_array_double_option_serde_annotations",
            "static_array_sequence_serde_annotations",
            "double_option_serde_annotations",
        ],
    },
    SourceRole {
        file: "json_schema_claims.rs",
        emitter: true,
        wasm: false,
        reason: "audited conservative private ClaimWalker state and schema-claim traversal with registrar type fragments; no WASM rendering",
        members: &[
            "json_schema_reachable_claims",
            "new",
            "claim_named",
            "walk_subschema",
            "walk_descriptor_leaf",
            "walk_schema_body",
        ],
    },
    SourceRole {
        file: "key_demands.rs",
        emitter: true,
        wasm: false,
        reason: "audited conservative key trait/bound/attribute fragments and assertion selection; no WASM rendering",
        members: &[
            "key_trait_list",
            "key_bound",
            "key_flavor_token",
            "assertion_roots",
            "add_struct_derives",
        ],
    },
    SourceRole {
        file: "layout.rs",
        emitter: false,
        wasm: false,
        reason: "output paths",
        members: &["is_under"],
    },
    SourceRole {
        file: "mod.rs",
        emitter: false,
        wasm: true,
        reason: "coordinator/type fragments; currently omitted from body roster",
        members: &[
            "generate",
            "clone_with_conceptual_type",
            "initialize_generation",
            "emit_type_aliases",
            "emit_structs_and_alias_wrappers",
            "emit_used_as_elem_wrappers",
            "emit_json_schema_rows",
            "emit_rust_runtime_declarations",
            "emit_rust_extern_reexports",
            "declare_rust_scope_roots_and_checks",
            "emit_rust_scope_imports",
            "emit_serialization_imports",
            "emit_wasm_imports_and_reexports",
            "emit_component_surface",
            "emit_optional_tests",
        ],
    },
    SourceRole {
        file: "no_std_check.rs",
        emitter: false,
        wasm: false,
        reason: "no_std check crate producer",
        members: &["no_std_check_files"],
    },
    SourceRole {
        file: "records.rs",
        emitter: true,
        wasm: true,
        reason: "record/constructor/codec bodies and WASM signatures",
        members: &[
            "codegen_struct",
            "emit_record_wasm",
            "prepare_record_native",
            "attach_record_encodings",
            "emit_record_protected_rest",
            "emit_record_codecs",
            "generate_record_map_codecs",
            "prepare_record_map_fields",
            "generate_record_map_serialization",
            "generate_record_map_deserialization",
            "push_single",
            "push",
            "apply_to",
        ],
    },
    SourceRole {
        file: "reference_closure.rs",
        emitter: false,
        wasm: false,
        reason: "generator reference closure",
        members: &["exclude_dangling_refs"],
    },
    SourceRole {
        file: "requests.rs",
        emitter: false,
        wasm: false,
        reason: "request parsing and mint orchestration",
        members: &["emit_requested_collections"],
    },
    SourceRole {
        file: "sidecars.rs",
        emitter: false,
        wasm: false,
        reason: "pure sidecar and schema crate rendering; no record field locals or WASM signatures",
        members: &[
            "render_json_gen_main",
            "render_json_gen_module",
            "render_borrowed_key_types",
            "render_extern_interface_check",
            "render_key_demand_assertions",
            "render_borrowed_collections",
            "render_collections_index",
        ],
    },
    SourceRole {
        file: "serialize.rs",
        emitter: true,
        wasm: false,
        reason: "serialization bodies and overload config",
        members: &[
            "ser_map",
            "ser_array",
            "ser_optional",
            "ser_alias",
            "ser_rust_ident",
            "ser_cbor_bytes",
            "ser_optionally_tagged",
            "ser_tagged",
            "ser_primitive",
            "ser_fixed",
            "ser_any",
            "generate_serialize",
            "start_len",
            "end_len",
        ],
    },
    SourceRole {
        file: "wit.rs",
        emitter: false,
        wasm: false,
        reason: "component WIT projection",
        members: &["wit_escape"],
    },
    SourceRole {
        file: "wrappers.rs",
        emitter: true,
        wasm: true,
        reason: "wrapper bodies and WASM signatures",
        members: &[
            "generate_wrapper_struct",
            "emit_wrapper_wasm_face",
            "emit_wrapper_json_impls",
            "emit_wrapper_struct_and_encodings",
            "emit_wrapper_codec_impls",
            "emit_set_nominal_ergonomics",
            "make_wrapper_decoded_ctor_block",
            "make_wrapper_initial_ctor_block",
        ],
    },
    SourceRole {
        file: "wasm_wrapper_registry.rs",
        emitter: true,
        wasm: true,
        reason: "audited conservative type/signature rendering destination and provider/reference registry",
        members: &[
            "wrapper",
            "door",
            "dependency_owned",
            "dependency_provider_scope",
            "record_local_class",
            "record_local_alias",
            "record_deferred",
            "record_dependency_class",
            "record_dependency_alias",
            "record_reference",
            "local_classes",
            "local_class_scope",
            "own_wrapper_shape",
            "deferred",
            "definition_kind",
            "references",
            "remove_local_class_for_test",
            "closure_check",
            "wasm_collection_reference_ident",
            "wasm_collection_reference",
            "wasm_collection_reference_inner",
            "raw_collection_dependency_provider_scope",
            "record_wasm_type_reference",
            "record_wasm_collection_alias_definition",
            "wasm_member_type",
            "wasm_param_type",
            "wasm_return_type",
            "wasm_collection_wrapper_registry",
            "remove_wasm_collection_local_class_for_test",
        ],
    },
    SourceRole {
        file: "write_tail.rs",
        emitter: false,
        wasm: false,
        reason: "post-pass/write orchestration",
        members: &["run"],
    },
];

fn inventory_errors(
    sources: &BTreeMap<String, String>,
    roles: &[SourceRole],
    emitters: &[&str],
    wasm: &[&str],
) -> Vec<String> {
    let mut errors = Vec::new();
    let mut files = BTreeSet::new();
    for role in roles {
        if !files.insert(role.file) {
            errors.push(format!("duplicate role {}", role.file));
        }
        if role.reason.is_empty() || role.members.is_empty() {
            errors.push(format!(
                "missing role rationale/member anchors {}",
                role.file
            ));
        }
        let Some(source) = sources.get(role.file) else {
            errors.push(format!("stale role {}", role.file));
            continue;
        };
        let masked: Vec<char> = scan_rust(source).masked.chars().collect();
        let functions = emitter_fns(&masked);
        for member in role.members {
            if !functions.iter().any(|function| function.name == *member) {
                errors.push(format!("missing member {}::{member}", role.file));
            }
        }
    }
    for file in sources.keys() {
        if !files.contains(file.as_str()) {
            errors.push(format!("unclassified source {file}"));
        }
    }
    let expected_emitters: BTreeSet<_> =
        roles.iter().filter(|r| r.emitter).map(|r| r.file).collect();
    let actual_emitters: BTreeSet<_> = emitters.iter().copied().collect();
    if expected_emitters != actual_emitters || emitters.len() != actual_emitters.len() {
        errors.push("emitter roster differs from audited roles".to_owned());
    }
    let expected_wasm: BTreeSet<_> = roles
        .iter()
        .filter(|r| r.wasm)
        .map(|r| format!("src/generation/{}", r.file))
        .collect();
    let actual_wasm: BTreeSet<_> = wasm.iter().map(|p| (*p).to_owned()).collect();
    if expected_wasm != actual_wasm || wasm.len() != actual_wasm.len() {
        errors.push("WASM roster differs from audited roles".to_owned());
    }
    errors
}

fn sources() -> BTreeMap<String, String> {
    fn collect(root: &std::path::Path, dir: &std::path::Path, out: &mut BTreeMap<String, String>) {
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                collect(root, &path, out);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                out.insert(
                    path.strip_prefix(root)
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .replace('\\', "/"),
                    std::fs::read_to_string(path).unwrap(),
                );
            }
        }
    }
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/generation");
    let mut out = BTreeMap::new();
    collect(&root, &root, &mut out);
    out
}

#[test]
fn generation_source_scan_inventory_is_complete() {
    assert_eq!(
        inventory_errors(
            &sources(),
            SOURCE_ROLES,
            EMITTER_SOURCES,
            WASM_RENDERING_EMITTERS
        ),
        Vec::<String>::new()
    );
}

// Calls are found in masked Rust, not strings/comments or the exact current emitted line.
// Whitespace and receiver spelling cannot disguise the direct rendering operation.
fn method_calls(masked: &[char], method: &str) -> Vec<usize> {
    (0..masked.len())
        .filter(|&at| {
            (at == 0 || !is_ident_char(masked[at - 1]))
                && ident_at(masked, at).as_deref() == Some(method)
                && masked[..at].iter().rev().find(|c| !c.is_whitespace()) == Some(&'.')
                && masked[at + method.len()..]
                    .iter()
                    .find(|c| !c.is_whitespace())
                    == Some(&'(')
        })
        .collect()
}

fn owner(functions: &[EmitterFn], at: usize) -> Option<&EmitterFn> {
    functions
        .iter()
        .filter(|f| f.start <= at && at <= f.end)
        .min_by_key(|f| f.end - f.start)
}

#[derive(Clone, Copy)]
struct RenderingDoor {
    file: &'static str,
    function: &'static str,
    method: &'static str,
}
const RENDERING_DOORS: &[RenderingDoor] = &[
    RenderingDoor {
        file: "wasm_wrapper_registry.rs",
        function: "wasm_member_type",
        method: "for_wasm_member",
    },
    RenderingDoor {
        file: "wasm_wrapper_registry.rs",
        function: "wasm_param_type",
        method: "for_wasm_param",
    },
    RenderingDoor {
        file: "wasm_wrapper_registry.rs",
        function: "wasm_return_type",
        method: "for_wasm_return",
    },
];

fn rendering_errors(
    sources: &BTreeMap<String, String>,
    roster: &[&str],
    doors: &[RenderingDoor],
) -> Vec<String> {
    let mut errors = Vec::new();
    let mut seen = vec![0; doors.len()];
    for path in roster {
        let file = path.strip_prefix("src/generation/").unwrap();
        let Some(source) = sources.get(file) else {
            errors.push(format!("missing rendering source {file}"));
            continue;
        };
        let masked: Vec<char> = scan_rust(source).masked.chars().collect();
        let functions = emitter_fns(&masked);
        let references = method_calls(&masked, "record_wasm_type_reference");
        for method in ["for_wasm_member", "for_wasm_param", "for_wasm_return"] {
            for at in method_calls(&masked, method) {
                let enclosing = owner(&functions, at);
                let approved = doors.iter().position(|door| {
                    door.file == file
                        && door.method == method
                        && enclosing.is_some_and(|f| f.name == door.function)
                });
                if let Some(index) = approved {
                    seen[index] += 1;
                    if !references.iter().any(|&reference| {
                        owner(&functions, reference)
                            .is_some_and(|f| enclosing.is_some_and(|door| f.start == door.start))
                    }) {
                        errors.push(format!(
                            "unrecorded rendering door {file}::{}",
                            doors[index].function
                        ));
                    }
                } else {
                    errors.push(format!("unauthorized rendering {file}::{method}"));
                }
            }
        }
    }
    for (door, count) in doors.iter().zip(seen) {
        if count != 1 {
            errors.push(format!(
                "rendering door {}::{} observed {count} times",
                door.file, door.function
            ));
        }
    }
    errors
}

#[test]
fn wasm_rendering_doors_have_named_owners_and_reference_recording() {
    assert_eq!(
        rendering_errors(&sources(), WASM_RENDERING_EMITTERS, RENDERING_DOORS),
        Vec::<String>::new()
    );
}

#[test]
fn inventory_predicate_rejects_unclassified_unscanned_and_displaced_members() {
    let role = SourceRole {
        file: "body.rs",
        emitter: true,
        wasm: false,
        reason: "synthetic body emitter",
        members: &["moved"],
    };
    let mut source = BTreeMap::from([("body.rs".to_owned(), "fn moved() {}".to_owned())]);
    assert!(inventory_errors(&source, &[role], &["body.rs"], &[]).is_empty());
    assert!(
        inventory_errors(&source, &[role], &[], &[])
            .iter()
            .any(|e| e.contains("emitter roster"))
    );
    source.insert("new.rs".to_owned(), "fn extra() {}".to_owned());
    assert!(
        inventory_errors(&source, &[role], &["body.rs"], &[])
            .iter()
            .any(|e| e.contains("unclassified source"))
    );
    source.remove("new.rs");
    source.insert(
        "body.rs".to_owned(),
        "// fn moved() {}\nfn renamed() { let text = \"fn moved() {}\"; }".to_owned(),
    );
    assert!(
        inventory_errors(&source, &[role], &["body.rs"], &[])
            .iter()
            .any(|e| e.contains("missing member"))
    );
    source.clear();
    assert!(
        inventory_errors(&source, &[role], &["body.rs"], &[])
            .iter()
            .any(|e| e.contains("stale role"))
    );
}

#[test]
fn rendering_predicate_rejects_wrong_owner_nested_bypass_and_unrecorded_calls() {
    let door = RenderingDoor {
        file: "door.rs",
        function: "approved",
        method: "for_wasm_param",
    };
    let roster = ["src/generation/door.rs"];
    let check = |text: &str| {
        rendering_errors(
            &BTreeMap::from([("door.rs".to_owned(), text.to_owned())]),
            &roster,
            &[door],
        )
    };
    let allowed = "fn approved() { value . for_wasm_param \n (types); self.record_wasm_type_reference(types); }";
    assert!(check(allowed).is_empty());
    assert!(
        check(&allowed.replace("approved", "bypass"))
            .iter()
            .any(|e| e.contains("unauthorized"))
    );
    assert!(check("fn approved() { self.record_wasm_type_reference(types); fn nested() { value.for_wasm_param(types); } }").iter().any(|e| e.contains("unauthorized")));
    assert!(
        check("fn approved() { value.for_wasm_param(types); }")
            .iter()
            .any(|e| e.contains("unrecorded"))
    );
    assert!(check("fn approved() { self.record_wasm_type_reference(types); /* value.for_wasm_param(types); */ let fake = \"value.for_wasm_param(types)\"; }").iter().any(|e| e.contains("observed 0")));
    assert!(
        rendering_errors(
            &BTreeMap::from([("door.rs".to_owned(), allowed.to_owned())]),
            &[],
            &[door]
        )
        .iter()
        .any(|e| e.contains("observed 0"))
    );
}

#[test]
fn overload_predicates_detect_default_leaks_and_unknown_name_carriers() {
    use super::snapshot_tests::{
        overload_default_violation, overload_name_parameter, overload_scoped_literals_for_source,
    };
    let source = r#"fn moved(deserializer_name: &str, serializer_use: &str) {
        emit("raw.read()"); emit("serializer.write()"); emit("raw_bytes");
    }"#;
    let scoped = overload_scoped_literals_for_source("synthetic.rs", source);
    assert_eq!(scoped.len(), 3);
    for token in ["raw", "serializer"] {
        assert!(scoped.iter().any(|(file, function, _, literal, de, se)| {
            overload_default_violation(file, function, literal, *de, *se, token)
        }));
    }
    assert!(!scoped.iter().any(|(file, function, _, literal, de, se)| {
        literal == "raw_bytes"
            && overload_default_violation(file, function, literal, *de, *se, "raw")
    }));
    assert_eq!(
        overload_name_parameter("serializer_buffer", "Option<(&str,bool)>"),
        Some(false)
    );
    assert_eq!(
        overload_name_parameter("mutserializer_use", "&str"),
        Some(true)
    );
    assert_eq!(overload_name_parameter("door", "&str"), None);
    assert_eq!(
        overload_name_parameter("serializing_rust_type", "SerializingRustType"),
        None
    );
    assert!(
        overload_scoped_literals_for_source("synthetic.rs", "fn root() { emit(\"raw.read()\"); }")
            .is_empty()
    );
}
