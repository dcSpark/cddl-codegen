//! Registration-class × reference-position sweep.
//!
//! `referencing_context_tests` varies metadata written on a referenced rule.  This module instead
//! varies how the referenced rule reaches the IR: an own struct, aliases registered at distinct
//! times, a generic instance, an extern, and both recursive collection registration orders.  The
//! two axes are deliberately separate registries; a new class or position cannot become covered by
//! an accidental neighbour in a hand-written fixture.

use crate::cli::Cli;
use crate::tests::integration_tests::{acquire_scratch_lock, checkout_hash, codegen_cmd, tool_cmd};
use crate::tests::robustness_tests::with_thread_silenced_panics;
use clap::Parser;

#[derive(Clone, Copy)]
struct Registration {
    id: &'static str,
    /// Renders the registration class; `p` is a cell-local, collision-free prefix.
    render: fn(&str) -> String,
    /// The registered name every position must reference. Recursive rows intentionally use their
    /// actual collection/alias names rather than adding a normalizing third alias.
    reference: fn(&str) -> String,
    extern_type: bool,
}

#[derive(Clone, Copy)]
struct ReferencePosition {
    id: &'static str,
    /// Renders a live use of `reference`, plus the cell-local holder root where appropriate.
    render: fn(&str, &str) -> String,
}

fn base_reference(p: &str) -> String {
    format!("{p}base")
}

fn collection_first_reference(p: &str) -> String {
    format!("{p}x")
}

fn alias_first_reference(p: &str) -> String {
    format!("{p}hop_alias")
}

fn own_ident_record(p: &str) -> String {
    format!("{p}base = [value: bool]\n")
}

fn transparent_alias(p: &str) -> String {
    format!("{p}base = bool\n")
}

fn forward_alias(p: &str) -> String {
    format!("{p}base = {p}later\n{p}later = [value: bool]\n")
}

fn generic_instance_binding(p: &str) -> String {
    // The historical escape was a tag-258 SET nominal: `instance` is a named binding to the
    // canonical `GenericU64` struct, so it is registered as an alias, not another record. `base`
    // is the second alias whose unresolved Rust leaf used to remain dead at member/element sites.
    format!(
        "{p}generic<T> = #6.258([* T]) / [* T]\n{p}instance = {p}generic<uint>\n{p}base = {p}instance\n"
    )
}

fn extern_type(p: &str) -> String {
    format!("{p}base = _CDDL_CODEGEN_EXTERN_TYPE_\n")
}

// These are the two actual recursive alias-hop naming orders. They differ in WHICH ident is first
// registered and referenced, not merely in source ordering: a source permutation with the same
// idents can be topologically equivalent and would miss the second escape.
fn recursive_collection_first(p: &str) -> String {
    format!("{p}y = {p}x\n{p}x = [* {p}y]\n")
}

fn recursive_alias_first(p: &str) -> String {
    format!("{p}hop_alias = {p}hop_arr\n{p}hop_arr = [* {p}hop_alias]\n")
}

const REGISTRATIONS: &[Registration] = &[
    Registration {
        id: "own-ident-record",
        render: own_ident_record,
        reference: base_reference,
        extern_type: false,
    },
    Registration {
        id: "transparent-alias",
        render: transparent_alias,
        reference: base_reference,
        extern_type: false,
    },
    Registration {
        id: "forward-alias",
        render: forward_alias,
        reference: base_reference,
        extern_type: false,
    },
    Registration {
        id: "generic-instance-binding",
        render: generic_instance_binding,
        reference: base_reference,
        extern_type: false,
    },
    Registration {
        id: "extern-type",
        render: extern_type,
        reference: base_reference,
        extern_type: true,
    },
    Registration {
        id: "recursive-collection-first",
        render: recursive_collection_first,
        reference: collection_first_reference,
        extern_type: false,
    },
    Registration {
        id: "recursive-alias-first",
        render: recursive_alias_first,
        reference: alias_first_reference,
        extern_type: false,
    },
];

fn tag_head_payload(p: &str, reference: &str) -> String {
    format!("{p}ctx = #6.9({reference})\n{p}holder = [f: {p}ctx]\n")
}
fn member_cbor_payload(p: &str, reference: &str) -> String {
    format!("{p}holder = [f: bytes .cbor {reference}]\n")
}
fn rule_body_cbor_alias(p: &str, reference: &str) -> String {
    format!("{p}ctx = bytes .cbor {reference}\n{p}holder = [f: {p}ctx]\n")
}
fn transparent_realias_hop(p: &str, reference: &str) -> String {
    format!("{p}ctx = {reference}\n{p}holder = [f: {p}ctx]\n")
}
fn generic_argument(p: &str, reference: &str) -> String {
    format!("{p}wrap<T> = [value: T]\n{p}ctx = {p}wrap<{reference}>\n{p}holder = [f: {p}ctx]\n")
}
fn map_value(p: &str, reference: &str) -> String {
    format!("{p}ctx = {{ * bool => {reference} }}\n{p}holder = [f: {p}ctx]\n")
}
fn map_key(p: &str, reference: &str) -> String {
    format!("{p}ctx = {{ * {reference} => bool }}\n{p}holder = [f: {p}ctx]\n")
}
fn loose_array_element(p: &str, reference: &str) -> String {
    format!("{p}holder = [f: [* {reference}]]\n")
}
fn type_choice_arm(p: &str, reference: &str) -> String {
    format!("{p}ctx = {reference} / bool\n{p}holder = [f: {p}ctx]\n")
}
fn optional_record_member(p: &str, reference: &str) -> String {
    format!("{p}ctx = [? x: {reference}]\n{p}holder = [f: {p}ctx]\n")
}
fn direct_record_member(p: &str, reference: &str) -> String {
    format!("{p}holder = [f: {reference}]\n")
}

const POSITIONS: &[ReferencePosition] = &[
    ReferencePosition {
        id: "tag-head-payload",
        render: tag_head_payload,
    },
    ReferencePosition {
        id: "member-cbor-payload",
        render: member_cbor_payload,
    },
    ReferencePosition {
        id: "rule-body-cbor-alias",
        render: rule_body_cbor_alias,
    },
    ReferencePosition {
        id: "transparent-realias-hop",
        render: transparent_realias_hop,
    },
    ReferencePosition {
        id: "generic-argument",
        render: generic_argument,
    },
    ReferencePosition {
        id: "map-value",
        render: map_value,
    },
    ReferencePosition {
        id: "map-key",
        render: map_key,
    },
    ReferencePosition {
        id: "loose-array-element",
        render: loose_array_element,
    },
    ReferencePosition {
        id: "type-choice-arm",
        render: type_choice_arm,
    },
    ReferencePosition {
        id: "optional-record-member",
        render: optional_record_member,
    },
    // The directive sibling deliberately omits this control because its attachment-position sweep
    // already holds it.  This product has no such direct-reference row otherwise.
    ReferencePosition {
        id: "direct-record-member",
        render: direct_record_member,
    },
];

#[derive(Clone, Copy)]
enum Verdict {
    Accept,
    // The present table has no semantically valid refusals, but the registry's type deliberately
    // makes a future boundary carry its stable diagnostic rather than adding an untracked skip.
    #[allow(dead_code)]
    Reject(&'static str),
}

struct CellVerdict {
    registration: &'static str,
    position: &'static str,
    verdict: Verdict,
}

// Every entry is deliberately authored rather than inferred from the result. The first probe
// established that all 77 combinations are supported (including map keys), so the current boundary
// is explicitly ALL ACCEPT rather than concealing a run-derived expectation in test logic. If a
// future product boundary is a real graceful refusal, replace its authored `Accept` with
// `Reject(stable diagnostic)` and keep the exact-cover / stale-name / participation checks below.
// Keeping the complete table adjacent to the axes makes an unsupported combination a reviewed
// product boundary rather than a hole in the product.
macro_rules! accept_row {
    ($out:expr, $registration:literal) => {
        $out.extend([
            CellVerdict {
                registration: $registration,
                position: "tag-head-payload",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "member-cbor-payload",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "rule-body-cbor-alias",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "transparent-realias-hop",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "generic-argument",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "map-value",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "map-key",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "loose-array-element",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "type-choice-arm",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "optional-record-member",
                verdict: Verdict::Accept,
            },
            CellVerdict {
                registration: $registration,
                position: "direct-record-member",
                verdict: Verdict::Accept,
            },
        ]);
    };
}

fn verdicts() -> Vec<CellVerdict> {
    let mut out = Vec::new();
    accept_row!(out, "own-ident-record");
    accept_row!(out, "transparent-alias");
    accept_row!(out, "forward-alias");
    accept_row!(out, "generic-instance-binding");
    accept_row!(out, "extern-type");
    accept_row!(out, "recursive-collection-first");
    accept_row!(out, "recursive-alias-first");
    out
}

fn prefix(registration: &Registration, position: &ReferencePosition) -> String {
    format!(
        "rr_{}_{}_",
        registration.id.replace('-', "_"),
        position.id.replace('-', "_")
    )
}

fn cell_spec(registration: &Registration, position: &ReferencePosition) -> String {
    let prefix = prefix(registration, position);
    let reference = (registration.reference)(&prefix);
    format!(
        "{}{}",
        (registration.render)(&prefix),
        (position.render)(&prefix, &reference)
    )
}

enum Outcome {
    Source(String),
    Error(String),
    Panic,
}

fn generate_one(registration: &Registration, position: &ReferencePosition) -> Outcome {
    let spec = cell_spec(registration, position);
    let tag = prefix(registration, position);
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_registration_reference_{tag}_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, spec).expect("write independent cell input");
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().expect("utf8 temp path"),
        "--output",
        "registration_reference_unused",
        "--wasm=false",
    ]);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::api::generated_strings(&cli)
            .map(|files| files.into_values().collect())
            .map_err(|error| error.to_string())
    }));
    std::fs::remove_file(path).ok();
    match result {
        Ok(Ok(source)) => Outcome::Source(source),
        Ok(Err(error)) => Outcome::Error(error),
        Err(_) => Outcome::Panic,
    }
}

fn authored_verdict(registration: &Registration, position: &ReferencePosition) -> Verdict {
    verdicts()
        .iter()
        .find(|cell| cell.registration == registration.id && cell.position == position.id)
        .map(|cell| cell.verdict)
        .expect("registry test ensures every base × position pair has one verdict")
}

#[test]
fn registration_class_reference_position_sweep() {
    let mut failures = Vec::new();
    with_thread_silenced_panics(|| {
        for registration in REGISTRATIONS {
            for position in POSITIONS {
                let outcome = generate_one(registration, position);
                match (authored_verdict(registration, position), outcome) {
                    (Verdict::Accept, Outcome::Source(source)) => {
                        let holder = crate::utils::convert_to_camel_case(&format!(
                            "{}holder",
                            prefix(registration, position)
                        ));
                        if !source.contains(&format!("pub struct {holder}")) {
                            failures.push(format!(
                                "[{} × {}] authored Accept generated no holder `{holder}`; the \
                                 referenced context may have been silently omitted",
                                registration.id, position.id
                            ));
                        }
                    }
                    (Verdict::Reject(expected), Outcome::Error(actual)) if actual.contains(expected) => {}
                    (Verdict::Accept, Outcome::Error(error)) => failures.push(format!(
                        "[{} × {}] authored Accept but generation gracefully refused:\n{error}",
                        registration.id, position.id
                    )),
                    (Verdict::Accept, Outcome::Panic) => failures.push(format!(
                        "[{} × {}] authored Accept but generation PANICKED",
                        registration.id, position.id
                    )),
                    (Verdict::Reject(expected), Outcome::Source(_)) => failures.push(format!(
                        "[{} × {}] authored Reject({expected:?}) but generation succeeded; remove the \
                         refusal only after its compile-floor cell is green",
                        registration.id, position.id
                    )),
                    (Verdict::Reject(expected), Outcome::Error(actual)) => failures.push(format!(
                        "[{} × {}] authored Reject({expected:?}) but got another graceful diagnostic:\n{actual}",
                        registration.id, position.id
                    )),
                    (Verdict::Reject(expected), Outcome::Panic) => failures.push(format!(
                        "[{} × {}] authored Reject({expected:?}) but generation PANICKED",
                        registration.id, position.id
                    )),
                }
            }
        }
    });
    assert!(
        failures.is_empty(),
        "registration × reference sweep failures:\n\n{}",
        failures.join("\n\n")
    );
}

#[test]
fn registration_reference_registries_are_complete_and_live() {
    let mut problems = Vec::new();
    let ids_unique = |ids: Vec<&str>, axis: &str, problems: &mut Vec<String>| {
        let mut seen = std::collections::BTreeSet::new();
        for id in ids {
            if !seen.insert(id) {
                problems.push(format!("duplicate {axis} id `{id}`"));
            }
        }
    };
    ids_unique(
        REGISTRATIONS.iter().map(|row| row.id).collect(),
        "registration",
        &mut problems,
    );
    ids_unique(
        POSITIONS.iter().map(|row| row.id).collect(),
        "reference-position",
        &mut problems,
    );

    let mut cells = std::collections::BTreeSet::new();
    let verdicts = verdicts();
    for cell in &verdicts {
        if !REGISTRATIONS.iter().any(|row| row.id == cell.registration) {
            problems.push(format!(
                "verdict names dead registration `{}`",
                cell.registration
            ));
        }
        if !POSITIONS.iter().any(|row| row.id == cell.position) {
            problems.push(format!("verdict names dead position `{}`", cell.position));
        }
        if !cells.insert((cell.registration, cell.position)) {
            problems.push(format!(
                "duplicate verdict for {} × {}",
                cell.registration, cell.position
            ));
        }
        if let Verdict::Reject(reason) = cell.verdict
            && reason.trim().is_empty()
        {
            problems.push(format!(
                "refusal {} × {} has an empty diagnostic",
                cell.registration, cell.position
            ));
        }
    }
    for registration in REGISTRATIONS {
        for position in POSITIONS {
            if !cells.contains(&(registration.id, position.id)) {
                problems.push(format!(
                    "missing authored verdict for {} × {}",
                    registration.id, position.id
                ));
            }
        }
        if !verdicts.iter().any(|cell| {
            cell.registration == registration.id && matches!(cell.verdict, Verdict::Accept)
        }) {
            problems.push(format!(
                "registration `{}` participates in no accepted cell",
                registration.id
            ));
        }
    }
    for position in POSITIONS {
        if !verdicts
            .iter()
            .any(|cell| cell.position == position.id && matches!(cell.verdict, Verdict::Accept))
        {
            problems.push(format!(
                "reference position `{}` participates in no accepted cell",
                position.id
            ));
        }
    }
    assert!(
        problems.is_empty(),
        "registration/reference registry failures:\n{}",
        problems.join("\n")
    );
}

fn accepted_cells() -> Vec<(&'static Registration, &'static ReferencePosition)> {
    REGISTRATIONS
        .iter()
        .flat_map(|registration| {
            POSITIONS.iter().filter_map(move |position| {
                matches!(authored_verdict(registration, position), Verdict::Accept)
                    .then_some((registration, position))
            })
        })
        .collect()
}

fn extern_names(cells: &[(&Registration, &ReferencePosition)]) -> Vec<String> {
    cells
        .iter()
        .filter(|(registration, _)| registration.extern_type)
        .map(|(registration, position)| {
            crate::utils::convert_to_camel_case(&(registration.reference)(&prefix(
                registration,
                position,
            )))
        })
        .collect()
}

fn append_native_extern_definitions(writer: &mut impl std::io::Write, names: &[String]) {
    for name in names {
        writeln!(
            writer,
            "\n#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]\npub struct {name};\n\
             impl cbor_event::se::Serialize for {name} {{\n\
                 fn serialize<'se>(&self, serializer: &'se mut cbor_event::se::Serializer) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {{ serializer.write_special(cbor_event::Special::Bool(false)) }}\n\
             }}\n\
             impl serialization::Deserialize for {name} {{\n\
                 fn deserialize(raw: &mut cbor_event::de::Deserializer) -> Result<Self, error::DeserializeError> {{ raw.bool().map(|_| Self).map_err(Into::into) }}\n\
             }}"
        )
        .unwrap();
    }
}

fn append_extern_definitions(
    output: &std::path::Path,
    cells: &[(&Registration, &ReferencePosition)],
) {
    use std::io::Write;

    let extern_names = extern_names(cells);
    if extern_names.is_empty() {
        return;
    }
    let mut rust = std::fs::OpenOptions::new()
        .append(true)
        .open(output.join("rust/src/lib.rs"))
        .expect("open seed-owned rust crate root for extern definitions");
    let mut wasm = std::fs::OpenOptions::new()
        .append(true)
        .open(output.join("wasm/src/lib.rs"))
        .expect("open seed-owned wasm crate root for extern definitions");
    writeln!(
        wasm,
        "\nuse wasm_bindgen::prelude::{{wasm_bindgen, JsError}};"
    )
    .unwrap();
    append_native_extern_definitions(&mut rust, &extern_names);
    for name in &extern_names {
        writeln!(
            wasm,
            "\n#[wasm_bindgen]\npub struct {name}(cddl_lib::{name});\n\
             #[wasm_bindgen]\nimpl {name} {{\n\
                 pub fn to_cbor_bytes(&self) -> Vec<u8> {{ cddl_lib::serialization::ToCBORBytes::to_cbor_bytes(&self.0) }}\n\
                 pub fn from_cbor_bytes(bytes: &[u8]) -> Result<{name}, JsError> {{ cddl_lib::serialization::Deserialize::from_cbor_bytes(bytes).map(Self).map_err(Into::into) }}\n\
             }}\n\
             impl From<cddl_lib::{name}> for {name} {{ fn from(value: cddl_lib::{name}) -> Self {{ Self(value) }} }}\n\
             impl From<{name}> for cddl_lib::{name} {{ fn from(value: {name}) -> Self {{ value.0 }} }}\n\
             impl From<&{name}> for cddl_lib::{name} {{ fn from(value: &{name}) -> Self {{ value.0.clone() }} }}\n\
             impl AsRef<cddl_lib::{name}> for {name} {{ fn as_ref(&self) -> &cddl_lib::{name} {{ &self.0 }} }}"
        )
        .unwrap();
    }
}

/// Each accepted product cell first generated independently above, then joins a single
/// collision-free crate per profile.  This pays one cargo invocation per face while preserving the
/// individual generation verdict that identifies a bad registration/position pair.  Extern marker
/// rules receive genuine hand-owned root definitions; the generated tree remains untouched.
#[test]
fn registration_reference_accepted_cells_compile() {
    if std::process::Command::new("cargo")
        .arg("--version")
        .output()
        .is_err()
    {
        return;
    }
    let cells = accepted_cells();
    let cell_ids = cells
        .iter()
        .map(|(registration, position)| format!("{} × {}", registration.id, position.id))
        .collect::<Vec<_>>()
        .join(", ");
    let scratch_name = format!(
        "cddl_codegen_registration_reference_{:016x}",
        checkout_hash()
    );
    let _scratch_lock = acquire_scratch_lock(&scratch_name); // serialize same-checkout runs
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).expect("create registration/reference scratch root");
    let input = root.join("all-accepted.cddl");
    let spec = cells
        .iter()
        .map(|(registration, position)| cell_spec(registration, position))
        .collect::<String>();
    std::fs::write(&input, spec).expect("write accepted product spec");
    let target = root.join("target");
    for (profile, wasm, faces) in [
        ("rust-only", "--wasm=false", &[("rust")][..]),
        ("wasm-bearing", "--wasm=true", &[("rust"), ("wasm")][..]),
    ] {
        let output = root.join(profile);
        let generated = codegen_cmd()
            .args([
                "--input",
                input.to_str().expect("utf8 input"),
                "--output",
                output.to_str().expect("utf8 output"),
                wasm,
                "--static-dir",
                concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
            ])
            .output()
            .expect("spawn generator for accepted product batch");
        assert!(
            generated.status.success(),
            "{profile}: generation failed for accepted cells [{cell_ids}]:\n{}\n{}",
            String::from_utf8_lossy(&generated.stdout),
            String::from_utf8_lossy(&generated.stderr)
        );
        if wasm == "--wasm=true" {
            append_extern_definitions(&output, &cells);
        } else {
            // The rust-only profile still requires the native hand definitions.  The helper also
            // writes wasm definitions, so seed wasm first by generating the profile's normal face
            // is not available; append the small native set directly through the same helper only
            // when a wasm crate exists.
            let names = extern_names(&cells);
            let mut rust = std::fs::OpenOptions::new()
                .append(true)
                .open(output.join("rust/src/lib.rs"))
                .unwrap();
            append_native_extern_definitions(&mut rust, &names);
        }
        for face in faces {
            let checked = tool_cmd("cargo")
                .arg("check")
                .current_dir(output.join(face))
                .env("CARGO_TARGET_DIR", &target)
                .output()
                .expect("spawn cargo check for accepted product batch");
            assert!(
                checked.status.success(),
                "{profile}/{face}: cargo check failed for accepted cells [{cell_ids}]:\n{}\n{}",
                String::from_utf8_lossy(&checked.stdout),
                String::from_utf8_lossy(&checked.stderr)
            );
        }
    }
    let _ = std::fs::remove_dir_all(root);
}
