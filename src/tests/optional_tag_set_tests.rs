//! The transparent tag-set idiom (`x = #6.N([* a]) / [* a]`).
//!
//! A two-arm type choice whose arms are the SAME collection but for exactly one `Tagged(N)`
//! encoding op denotes one logical value; which arm was used is an encoding detail, not a type
//! distinction. `parse_type_choices` recognizes this structurally and collapses it into the SAME
//! registration a bare `#6.N([* a])` array rule gets — a transparent `Vec`/`NonEmptyVec` alias plus
//! an `Array`-variant `RustStruct` — with the tag flagged OPTIONAL so it rides a
//! `TagPresenceEncoding` var under `--preserve-encodings` (either wire arm roundtrips byte-exact)
//! and defaults to tagged otherwise. Near misses keep today's two-variant enum.
//!
//! These tests drive the full in-process generation pipeline (`api::generated_strings`) and assert
//! the emitted SOURCE shape. Generic-def rows assert the collapse reaches the transparent-alias IR
//! (mirroring `generic_collection_tests`' no-panic contract — a collection-bodied generic def's
//! emitted (de)serialize is a separate pre-existing concern, so these do not compile the output).

use crate::cli::Cli;
use clap::Parser;

/// Run the whole generation pipeline in-process and return the emitted source of every file joined,
/// or the graceful error string.
fn generate(spec: &str, tag: &str, extra_flags: &[&str]) -> Result<String, String> {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_optset_{}_{}.cddl",
        tag,
        std::process::id()
    ));
    std::fs::write(&path, spec).unwrap();
    let mut args = vec![
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "optional_tag_set_unused",
    ];
    args.extend_from_slice(extra_flags);
    let cli = Cli::parse_from(args);
    let result = crate::api::generated_strings(&cli)
        .map(|files| files.into_values().collect::<Vec<_>>().join("\n"))
        .map_err(|e| e.to_string());
    std::fs::remove_file(&path).ok();
    result
}

const PRESERVE: &[&str] = &[
    "--preserve-encodings=true",
    "--canonical-form=true",
    "--wasm",
    "false",
];

// ---------------------------------------------------------------------------------------------
// Recognition-positive (the collapse fires)
// ---------------------------------------------------------------------------------------------

/// The canonical `nonempty_set` idiom collapses to a transparent `NonEmptyVec` alias — NOT an enum —
/// and the `[+]` bound is enforced through `NonEmptyVec`'s single `TryFrom` door.
#[test]
fn nonempty_set_collapses_to_transparent_alias() {
    let src = generate(
        "my_set = #6.258([+ uint]) / [+ uint]\nholder = [items: my_set]\n",
        "nonempty",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub type MySet = NonEmptyVec<u64>;"),
        "expected transparent NonEmptyVec alias, got:\n{src}"
    );
    assert!(
        !src.contains("pub enum MySet"),
        "must not emit a type-choice enum for the collapsed idiom:\n{src}"
    );
    // the `[+]` invariant rides the same TryFrom door the API uses
    assert!(
        src.contains("NonEmptyVec::try_from(items_arr)?"),
        "wire-side `[+]` enforcement must route through the NonEmptyVec door:\n{src}"
    );
}

/// The `[*]` (empty-allowed) flavor collapses to a DISTINCT `Vec` alias — the empty and non-empty
/// flavors must never be conflated (the defect the CML hand impl carried).
#[test]
fn empty_allowed_set_collapses_to_vec_alias() {
    let src = generate(
        "my_set = #6.258([* uint]) / [* uint]\nholder = [items: my_set]\n",
        "star",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub type MySet = Vec<u64>;"),
        "expected transparent Vec alias, got:\n{src}"
    );
    assert!(
        !src.contains("pub enum MySet"),
        "must not be an enum:\n{src}"
    );
}

/// Arm order is irrelevant: untagged-first still collapses.
#[test]
fn arm_order_is_irrelevant() {
    let src = generate(
        "my_set = [+ uint] / #6.258([+ uint])\nholder = [items: my_set]\n",
        "swapped",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub type MySet = NonEmptyVec<u64>;") && !src.contains("pub enum MySet"),
        "untagged-first arm order must collapse identically:\n{src}"
    );
}

/// The tag number is taken from the arm, never hardcoded to 258.
#[test]
fn any_tag_number_is_recognized() {
    let src = generate(
        "my_set = #6.42([+ uint]) / [+ uint]\nholder = [items: my_set]\n",
        "tag42",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub type MySet = NonEmptyVec<u64>;") && !src.contains("pub enum MySet"),
        "any tag number must collapse:\n{src}"
    );
    assert!(
        src.contains("write_tag_sz(42u64") && src.contains("expected: 42"),
        "the arm's tag number must drive serialize + deserialize:\n{src}"
    );
}

/// Under `--preserve-encodings` the tag becomes a tri-state `TagPresenceEncoding` var; serialize
/// branches on it (presence preserved, size normalized), deserialize peeks the major type.
#[test]
fn preserve_encodings_emits_tag_presence_tristate() {
    let src = generate(
        "my_set = #6.258([+ uint]) / [+ uint]\nholder = [items: my_set]\n",
        "tristate",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("items_tag_encoding: TagPresenceEncoding"),
        "the encoding struct must carry a TagPresenceEncoding field:\n{src}"
    );
    // serialize: conditional on the tri-state, size normalized via fit_sz
    assert!(
        src.contains("if let TagPresenceEncoding::Tagged(tag_sz) =")
            && src.contains("write_tag_sz(258u64, fit_sz(258u64, tag_sz, force_canonical))"),
        "serialize must branch on the tri-state and normalize only the size:\n{src}"
    );
    // deserialize: peek the major type, record which arm
    assert!(
        src.contains("match raw.cbor_type()?")
            && src.contains("cbor_event::Type::Tag =>")
            && src.contains("TagPresenceEncoding::Untagged"),
        "deserialize must peek Type::Tag and record the arm:\n{src}"
    );
}

/// Without `--preserve-encodings` there is no encoding var: serialize defaults NEW values to tagged
/// (matches the first/tagged arm and current-era ledger emission), deserialize accepts either arm.
#[test]
fn non_preserve_defaults_to_tagged_and_accepts_either() {
    let src = generate(
        "my_set = #6.258([+ uint]) / [+ uint]\nholder = [items: my_set]\n",
        "np",
        &["--wasm", "false"],
    )
    .expect("must generate");
    assert!(
        src.contains("pub type MySet = NonEmptyVec<u64>;"),
        "still a transparent alias without preserve-encodings:\n{src}"
    );
    assert!(
        src.contains("write_tag(258u64)"),
        "new values default to tagged:\n{src}"
    );
    assert!(
        src.contains("if raw.cbor_type()? == cbor_event::Type::Tag"),
        "deserialize peeks and consumes an optional tag:\n{src}"
    );
}

// ---------------------------------------------------------------------------------------------
// Generic defs (collapse reaches the transparent-alias IR — no panic, no enum)
// ---------------------------------------------------------------------------------------------

/// `set<a0> = #6.258([* a0]) / [* a0]` (choice-bodied generic def) collapses BEFORE the generic
/// machinery into an Array-bodied generic def, so instances resolve to transparent aliases and the
/// finalize-time `is_enum` panic (which a non-collapsed choice-bodied generic def hits) never fires.
#[test]
fn generic_set_defs_collapse_to_transparent_instances() {
    let src = generate(
        "set<a0> = #6.258([* a0]) / [* a0]\n\
         nonempty_set<a0> = #6.258([+ a0]) / [+ a0]\n\
         holder = [a: set<uint>, b: nonempty_set<text>, c: nonempty_set<uint>]\n",
        "generic",
        PRESERVE,
    )
    .expect("choice-bodied generic set defs must collapse and generate (no is_enum panic)");
    assert!(
        src.contains("pub type SetU64 = Vec<u64>;"),
        "empty-allowed generic instance is a transparent Vec alias:\n{src}"
    );
    assert!(
        src.contains("pub type NonemptySetText = NonEmptyVec<String>;")
            && src.contains("pub type NonemptySetU64 = NonEmptyVec<u64>;"),
        "non-empty generic instances are distinct NonEmptyVec aliases:\n{src}"
    );
    assert!(
        !src.contains("pub enum Set") && !src.contains("pub enum NonemptySet"),
        "the generic set bases must not be enums:\n{src}"
    );
}

// ---------------------------------------------------------------------------------------------
// Near misses (the collapse must NOT fire — today's enum is retained)
// ---------------------------------------------------------------------------------------------

fn assert_stays_enum(spec: &str, tag: &str, enum_name: &str) {
    let src = generate(spec, tag, PRESERVE).expect("near-miss spec must still generate");
    assert!(
        src.contains(&format!("pub enum {enum_name}")),
        "near-miss `{tag}` must keep its type-choice enum `{enum_name}`:\n{src}"
    );
    assert!(
        !src.contains(&format!("pub type {enum_name} =")),
        "near-miss `{tag}` must NOT collapse to a transparent alias:\n{src}"
    );
}

/// Mismatched occurrence bounds (`#6.258([+ a]) / [* a]`) are two different logical values — keep
/// the enum.
#[test]
fn near_miss_mismatched_bounds_stays_enum() {
    assert_stays_enum(
        "nm = #6.258([+ uint]) / [* uint]\nholder = [x: nm]\n",
        "mismatched_bounds",
        "Nm",
    );
}

/// Different element types are two different types — keep the enum.
#[test]
fn near_miss_different_element_types_stays_enum() {
    assert_stays_enum(
        "nm = #6.258([+ uint]) / [+ text]\nholder = [x: nm]\n",
        "different_elements",
        "Nm",
    );
}

/// Both arms tagged (with different tags) is not the tagged-or-untagged idiom — keep the enum.
#[test]
fn near_miss_both_arms_tagged_stays_enum() {
    assert_stays_enum(
        "nm = #6.258([+ uint]) / #6.259([+ uint])\nholder = [x: nm]\n",
        "both_tagged",
        "Nm",
    );
}

/// Three or more arms is a genuine choice — keep the enum.
#[test]
fn near_miss_three_arms_stays_enum() {
    assert_stays_enum(
        "nm = #6.258([+ uint]) / [+ uint] / [+ text]\nholder = [x: nm]\n",
        "three_arms",
        "Nm",
    );
}
