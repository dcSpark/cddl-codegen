//! The tag-set idiom (`x = #6.N([* a]) / [* a]`) and its nominalization.
//!
//! A two-arm type choice whose arms are the SAME collection but for exactly one `Tagged(N)`
//! encoding op denotes one logical value; which arm was used is an encoding detail, not a type
//! distinction. `parse_type_choices` recognizes this structurally and collapses it into one
//! collection, with the tag flagged OPTIONAL so it rides a `TagPresenceEncoding` var under
//! `--preserve-encodings` (either wire arm roundtrips byte-exact) and defaults to tagged otherwise.
//! Near misses keep today's two-variant enum.
//!
//! What the collapsed body becomes depends on the tag. Tag 258 is the IANA set tag, so the
//! well-known-tag registry (`parsing::well_known_tag_default_duplicates`) gives a 258 idiom set
//! semantics — and a 258 set is NOMINAL: instead of a transparent alias it emits a wrapper `struct`
//! that OWNS its `{tag, len, elem}` encodings (grammar decides the tag record — the two-arm optional
//! tag rides a `TagPresenceEncoding`, the single-arm mandatory tag an `Option<Sz>`) with the set
//! ergonomics and always-on encodings-ignored comparisons. This holds for named non-generic rules,
//! for each generic instantiation (`set<uint>` → `SetU64`), and for inline `#6.258([* a])`
//! occurrences (shape-derived `Set<Elem>` names). The `@duplicates` policy selects only the inner
//! collection: reject (the 258 default) ⇒ the `OrderedSet`/`NonEmptyOrderedSet` uniqueness twin;
//! `@duplicates preserve` opts back out to plain `Vec`/`NonEmptyVec` (today's wire behavior
//! verbatim) while STILL nominalizing. A NON-258 idiom keeps the plain transparent alias (the same
//! registration a bare `#6.N([* a])` array rule gets), preserve default.
//!
//! These tests drive the full in-process generation pipeline (`api::generated_strings`) and assert
//! the emitted SOURCE shape. Generic-def rows assert the collapse reaches the set-nominal IR (no
//! panic, no enum — a collection-bodied generic def collapses BEFORE the generic machinery, so each
//! instantiation mints one nominal wrapper) rather than the `is_enum` panic a non-collapsing
//! choice-bodied generic def would hit.

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

/// The canonical `nonempty_set` idiom (tag 258, no directive) NOMINALIZES (Phase 2.2) into a wrapper
/// struct owning its encodings — NOT an enum, NOT a transparent alias — with a `NonEmptyOrderedSet`
/// inner (258 defaults to `reject` via the well-known-tag registry); the `[+]` bound is enforced
/// through `NonEmptyOrderedSet`'s single `TryFrom` door (which composes uniqueness with the min-1
/// check).
#[test]
fn nonempty_set_nominalizes_to_wrapper() {
    let src = generate(
        "my_set = #6.258([+ uint]) / [+ uint]\nholder = [items: my_set]\n",
        "nonempty",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub struct MySet {")
            && src.contains("pub(crate) inner: NonEmptyOrderedSet<u64>,"),
        "expected a nominal set wrapper over NonEmptyOrderedSet (258 defaults to reject), got:\n{src}"
    );
    assert!(
        !src.contains("pub type MySet ="),
        "the named set rule must no longer be a transparent alias:\n{src}"
    );
    assert!(
        !src.contains("pub enum MySet"),
        "must not emit a type-choice enum for the collapsed idiom:\n{src}"
    );
    // the `[+]` + uniqueness invariant rides the same TryFrom door the API uses
    assert!(
        src.contains("NonEmptyOrderedSet::try_from(inner_arr)?"),
        "wire-side `[+]` + uniqueness enforcement must route through the NonEmptyOrderedSet door:\n{src}"
    );
}

/// The `[*]` (empty-allowed) flavor nominalizes to a wrapper with a DISTINCT inner from the `[+]`
/// flavor — the empty and non-empty flavors must never be conflated (the defect the CML hand impl
/// carried). Tag 258 with no directive defaults to `reject`, so the inner is `OrderedSet<u64>` (not
/// `NonEmptyOrderedSet`).
#[test]
fn empty_allowed_set_nominalizes_with_ordered_set_inner() {
    let src = generate(
        "my_set = #6.258([* uint]) / [* uint]\nholder = [items: my_set]\n",
        "star",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub struct MySet {") && src.contains("pub(crate) inner: OrderedSet<u64>,"),
        "expected a nominal set wrapper over OrderedSet (258 defaults to reject), got:\n{src}"
    );
    assert!(
        !src.contains("pub type MySet =") && !src.contains("pub enum MySet"),
        "must be neither a transparent alias nor an enum:\n{src}"
    );
}

/// Arm order is irrelevant: untagged-first still collapses (tag 258 → reject default).
#[test]
fn arm_order_is_irrelevant() {
    let src = generate(
        "my_set = [+ uint] / #6.258([+ uint])\nholder = [items: my_set]\n",
        "swapped",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub struct MySet {")
            && src.contains("pub(crate) inner: NonEmptyOrderedSet<u64>,")
            && !src.contains("pub type MySet =")
            && !src.contains("pub enum MySet"),
        "untagged-first arm order must nominalize identically:\n{src}"
    );
}

/// The tag number is taken from the arm, never hardcoded to 258. This also pins the non-258 registry
/// boundary: tag 42 has no well-known-tag entry, so it keeps the plain `NonEmptyVec` PRESERVE default
/// (only 258 acquires set semantics — the structural collapse itself stays tag-agnostic).
///
/// The rule WRAPS either way (T1-13: a tagged rule body owns its tag, so its standalone codec cannot
/// drop it); what 258 adds on top is NOMINALIZATION — the set semantics and the `OrderedSet` twin —
/// which is what this boundary is about, so the assertion is on the INNER representation.
#[test]
fn any_tag_number_is_recognized() {
    let src = generate(
        "my_set = #6.42([+ uint]) / [+ uint]\nholder = [items: my_set]\n",
        "tag42",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub(crate) inner: NonEmptyVec<u64>,")
            && !src.contains("OrderedSet")
            && !src.contains("pub enum MySet"),
        "any tag number must collapse; a non-258 tag keeps the plain NonEmptyVec default:\n{src}"
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
    // The tag encoding now lives on the NOMINAL set's own encoding struct (`inner_tag_encoding`), not
    // flattened onto the holder (`items_tag_encoding`) — holder-side flattening disappears for set
    // fields (Phase 2.2).
    assert!(
        src.contains("inner_tag_encoding: TagPresenceEncoding")
            && !src.contains("items_tag_encoding"),
        "the set's own encoding struct must carry the TagPresenceEncoding field:\n{src}"
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
        src.contains("pub struct MySet(pub(crate) NonEmptyOrderedSet<u64>);"),
        "a nominal tuple-struct set wrapper without preserve-encodings (258 defaults to reject):\n{src}"
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
// Generic defs (collapse reaches the set-nominal IR — no panic, no enum; Phase 2.3 nominalizes
// per instantiation)
// ---------------------------------------------------------------------------------------------

/// `set<a0> = #6.258([* a0]) / [* a0]` (choice-bodied generic def) collapses BEFORE the generic
/// machinery into a set-nominal Wrapper-bodied generic def, so each instantiation mints ONE nominal
/// wrapper (`set<uint>` → `SetU64`, Phase 2.3) — NOT a transparent alias — and the finalize-time
/// `is_enum` panic (which a non-collapsed choice-bodied generic def would hit) never fires. The
/// duplicates policy selects only the inner type (no directive ⇒ the reject `OrderedSet` twin).
#[test]
fn generic_set_defs_nominalize_per_instantiation() {
    let src = generate(
        "set<a0> = #6.258([* a0]) / [* a0]\n\
         nonempty_set<a0> = #6.258([+ a0]) / [+ a0]\n\
         holder = [a: set<uint>, b: nonempty_set<text>, c: nonempty_set<uint>]\n",
        "generic",
        PRESERVE,
    )
    .expect("choice-bodied generic set defs must collapse and generate (no is_enum panic)");
    assert!(
        src.contains("pub struct SetU64 {")
            && src.contains("pub(crate) inner: OrderedSet<u64>,")
            && !src.contains("pub type SetU64 ="),
        "the empty-allowed generic instance nominalizes to a wrapper over the reject OrderedSet twin \
         (258 defaults to reject), not a transparent alias:\n{src}"
    );
    assert!(
        src.contains("pub struct NonemptySetText {")
            && src.contains("pub(crate) inner: NonEmptyOrderedSet<String>,")
            && src.contains("pub struct NonemptySetU64 {")
            && src.contains("pub(crate) inner: NonEmptyOrderedSet<u64>,"),
        "each non-empty generic instantiation mints a DISTINCT nominal over the NonEmptyOrderedSet \
         twin:\n{src}"
    );
    assert!(
        !src.contains("pub enum Set") && !src.contains("pub enum NonemptySet"),
        "the generic set bases must not be enums:\n{src}"
    );
}

/// A named rule BINDING a generic set-nominal instantiation used at more than one site
/// (`required_signers = nonempty_set<uint>`, with a second anonymous use forcing the instantiation
/// to mint under its own canonical ident) lowers to a transparent `pub type` alias TO the nominal —
/// and that alias must SELF-DOCUMENT the bound nominal's resolved policy. The alias carries a bare
/// `Rust(<nominal>)` base_type, so the doc block resolves the nominal's registered config: the door
/// is `NonEmptyOrderedSet` (min-1) and the policy line is the `@duplicates reject` blurb. This pins
/// the POLARITY (CML's regen once emitted the inverted `NonEmptyVec` door + a preserve blurb on
/// exactly this shape — the one decode-time breaking change, misdocumented).
#[test]
fn alias_binding_set_nominal_documents_resolved_reject_policy() {
    let src = generate(
        "nonempty_set<a0> = #6.258([+ a0]) / [+ a0]\n\
         required_signers = nonempty_set<uint>\n\
         other = [rs: required_signers, more: nonempty_set<uint>]\n",
        "alias_instance",
        PRESERVE,
    )
    .expect("alias-of-instantiation must generate");
    // The named binding is a transparent alias to the instantiation nominal, not the identity mint.
    assert!(
        src.contains("pub type RequiredSigners = NonemptySetU64;"),
        "the multi-use named binding must alias the instantiation nominal:\n{src}"
    );
    // Correct door: `NonEmptyOrderedSet` (the min-1 uniqueness twin), NOT the inverted `NonEmptyVec`.
    assert!(
        src.contains(
            "`[+ u64]`: at least one element, enforced at the `NonEmptyOrderedSet` `TryFrom<Vec<_>>` door"
        ),
        "the alias doc must name the resolved `NonEmptyOrderedSet` door, not `NonEmptyVec`:\n{src}"
    );
    assert!(
        !src.contains("enforced at the `NonEmptyVec`"),
        "the alias doc must NOT emit the inverted `NonEmptyVec` door:\n{src}"
    );
    // Correct policy: the reject blurb, NOT the preserve blurb.
    assert!(
        src.contains("`@duplicates reject`: a repeated element is refused"),
        "the alias doc must emit the reject blurb (the resolved nominal policy):\n{src}"
    );
    assert!(
        !src.contains("Duplicate elements are preserved and re-emitted byte-exactly"),
        "the alias doc must NOT emit the inverted preserve blurb:\n{src}"
    );
}

/// The wasm surface for the CML alias shape (`required_signers = nonempty_set<uint>`), under
/// `--wasm=true`. Two deliverables are pinned here:
///   - C1 (flattened nominal class): the nominal wasm class DELEGATES the companion collection
///     surface directly (`get(index)`, `insert`, `try_from`, `try_opt_from`), so a JS read is
///     `set.get(i)` — not the old two-layer `set.get().get(i)` unwrap. The `try_opt_from` mirror is
///     what let the four `PARITY_EXEMPT` entries retire.
///   - C2 (rule-name collapse visibility): the alias emits a `typescript_custom_section` re-exporting
///     the rule name as a TS type alias to the nominal class (so TS callers compile through the
///     rename), and the rust alias doc names the JS-side re-key explicitly.
#[test]
fn alias_binding_set_nominal_wasm_surface_flattens_and_names_the_rekey() {
    let src = generate(
        "nonempty_set<a0> = #6.258([+ a0]) / [+ a0]\n\
         required_signers = nonempty_set<uint>\n\
         other = [rs: required_signers, more: nonempty_set<uint>]\n",
        "alias_instance_wasm",
        &["--wasm", "true"],
    )
    .expect("alias-of-instantiation must generate with wasm");
    // C1: the nominal class delegates the flat collection surface (indexed get + the set doors).
    assert!(
        src.contains("pub fn get(&self, index: usize) -> u64 {") && src.contains("self.0[index]"),
        "the nominal wasm class must delegate an indexed `get(index)` (flattened read surface):\n{src}"
    );
    assert!(
        src.contains("pub fn insert(&mut self, elem: u64) -> bool {")
            && src.contains("pub fn try_opt_from(elements: Vec<u64>) -> Result<Option<NonemptySetU64>, JsError> {"),
        "the nominal wasm class must delegate `insert -> bool` and the empty-means-absent \
         `try_opt_from` (the mirror that retired the PARITY_EXEMPT entries):\n{src}"
    );
    // C2: the rule name survives into TypeScript via a typescript_custom_section alias.
    assert!(
        src.contains("#[wasm_bindgen(typescript_custom_section)]")
            && src.contains("export type RequiredSigners = NonemptySetU64;"),
        "the alias must emit a typescript_custom_section re-exporting the rule name as a TS alias to \
         the nominal class:\n{src}"
    );
    // C2: the rust alias doc names the JS-side re-key (rule name -> nominal class name).
    assert!(
        src.contains("the wasm surface is the nominal class `NonemptySetU64`")
            && src.contains("JS call sites re-key to `NonemptySetU64`"),
        "the collapse notice must name the JS-side re-key to the nominal class:\n{src}"
    );
}

/// A BYTES-element set collapses to a transparent `NonEmptyOrderedSet<Vec<u8>>` alias (258 defaults
/// to reject) and, under `--preserve-encodings`, its byte-string elements ride the EXISTING per-element `StringEncoding`
/// machinery (`..._elem_encodings: Vec<StringEncoding>`) — so `@raw_bytes_flavor` is moot for the
/// generated type (it stays extern-only). This is asserted in-process on the RUST side only; the wasm
/// side of a bytes-element set — whose list-taking doors must borrow the loose `BytesList` class,
/// because a bare `Vec<Vec<u8>>` is not ABI-legal — is a compile floor,
/// `integration_tests::bytes_and_bool_element_list_doors_compile_and_round_trip`, and the
/// `tag_set_generic` corpus fixture carries a bytes instance for the same reason.
#[test]
fn bytes_element_set_collapses_with_elem_encodings() {
    let src = generate(
        "byte_set = #6.258([+ bytes]) / [+ bytes]\nholder = [s: byte_set]\n",
        "bytes_elem",
        PRESERVE,
    )
    .expect("a bytes-element set must collapse and generate");
    assert!(
        src.contains("pub struct ByteSet {")
            && src.contains("pub(crate) inner: NonEmptyOrderedSet<Vec<u8>>,")
            && !src.contains("pub type ByteSet =")
            && !src.contains("pub enum ByteSet"),
        "bytes-element set nominalizes to a wrapper over NonEmptyOrderedSet<Vec<u8>> (258 defaults to reject):\n{src}"
    );
    // the per-element StringEncoding + tag encoding now live on the set's OWN encoding struct (moved
    // off the holder), so they're keyed on the wrapper's `inner` member name.
    assert!(
        src.contains("inner_elem_encodings: Vec<StringEncoding>"),
        "byte-string elements must carry per-element StringEncoding preservation:\n{src}"
    );
    assert!(
        src.contains("inner_tag_encoding: TagPresenceEncoding"),
        "the optional tag still rides a TagPresenceEncoding var:\n{src}"
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

// ---------------------------------------------------------------------------------------------
// Reference-site encoding parity (Phase 2.5's alias walk appends reference-site encodings OUTSIDE
// the alias's own) + the collapsed set as a type-choice variant
// ---------------------------------------------------------------------------------------------

/// Extract a top-level `impl <trait> for <ty> { .. }` block from joined generated source (rustfmt
/// emits the closing brace at column 0, so the block runs from the marker to the next `\n}\n`).
fn extract_impl(src: &str, marker: &str) -> String {
    let start = src
        .find(marker)
        .unwrap_or_else(|| panic!("missing `{marker}` in:\n{src}"));
    let rest = &src[start..];
    let end = rest
        .find("\n}\n")
        .unwrap_or_else(|| panic!("unterminated `{marker}`"));
    rest[..end + 3].to_string()
}

/// A field whose reference site adds an OUTER tag (`#6.24(..)`) over a collapsed-set reference.
///
/// Phase 2.3 RESTORES parity between the two reference paths — a generic instance now nominalizes
/// exactly like the non-generic named rule (Phase 2.2 had deliberately split them):
/// - the NON-GENERIC named set rule (`ys_int = #6.258([* uint]) / [* uint]`) nominalizes, so the
///   reference-site outer `#6.24` tag wraps a NOMINAL delegation (`write_tag_sz(24u64, ..)` then
///   `self.g.serialize(..)`) — the inner optional-258 tag lives inside `YsInt::serialize`;
/// - the GENERIC INSTANCE (`xs_int = xs<uint>`, a named binding of `xs<uint>` → nominal `XsU64`)
///   ALSO nominalizes, so `#6.24(xs_int)` likewise wraps a nominal delegation — the inner
///   optional-258 tag lives inside the instantiation nominal, NOT flattened onto the holder.
///
/// Either way the outer `#6.24` tag is emitted OUTSIDE (before) the inner optional-258 tag. Standalone
/// compilation of a stacked outer-over-inner tag is pinned by the `double_tag` corpus fixture and
/// `robustness_tests::stacked_tag_encoding_members_are_depth_disambiguated`.
#[test]
fn outer_tag_over_set_reference_orders_outer_before_inner() {
    let generic = generate(
        "xs<a0> = #6.258([* a0]) / [* a0]\nxs_int = xs<uint>\nholder = [g: #6.24(xs_int)]\n",
        "outer_generic",
        PRESERVE,
    )
    .expect("generic outer-tag instance must generate");
    let non_generic = generate(
        "ys_int = #6.258([* uint]) / [* uint]\nholder = [g: #6.24(ys_int)]\n",
        "outer_nongeneric",
        PRESERVE,
    )
    .expect("non-generic outer-tag ref must generate");

    // Both the non-generic named set AND the generic instantiation nominalize: outer tag 24 wraps a
    // nominal delegation, and the inner 258 tag lives inside the nominal (never on the holder).
    for (label, src) in [("non-generic", &non_generic), ("generic", &generic)] {
        let ser = extract_impl(src, "impl Serialize for Holder");
        assert!(
            ser.contains("24u64") && ser.contains("self.g.serialize(serializer, force_canonical)?"),
            "the {label} set nominalizes: outer tag 24 then a nominal `self.g.serialize`:\n{ser}"
        );
        assert!(
            !ser.contains("g_tag2_encoding"),
            "the inner 258 tag must live inside the nominal, not flattened onto the holder \
             ({label}):\n{ser}"
        );
    }
}

/// A collapsed set used as a VARIANT of a larger type choice (`thing = my_set / uint`) generates a
/// coherent discriminator: the optionally-tagged collection variant contributes a TWO-entry
/// `cbor_types()` (`[Tag, Array]`), which the choice deserializer merges into ONE arm
/// (`Type::Tag | Type::Array`) routing either wire form to the set variant, while `uint` routes to
/// its own `UnsignedInteger` arm — no collision, no ambiguity.
#[test]
fn collapsed_set_as_type_choice_variant_discriminates_coherently() {
    let src = generate(
        "my_set = #6.258([+ uint]) / [+ uint]\nthing = my_set / uint\nholder = [t: thing]\n",
        "tc_variant",
        PRESERVE,
    )
    .expect("a collapsed set as a type-choice variant must generate");
    let de = extract_impl(&src, "impl Deserialize for Thing");
    assert!(
        de.contains("cbor_event::Type::Tag | cbor_event::Type::Array =>"),
        "the optionally-tagged set variant must merge Tag+Array into one discriminator arm:\n{de}"
    );
    assert!(
        de.contains("cbor_event::Type::UnsignedInteger =>"),
        "the uint variant must keep its own discriminator arm:\n{de}"
    );
    // the set arm now DELEGATES to the nominal set's own deserialize (Phase 2.2) rather than inlining
    // the tag peek + door — the `[+]` + uniqueness enforcement moved into `MySet::deserialize`.
    assert!(
        de.contains("Thing::MySet(MySet::deserialize(raw)?)"),
        "the set arm must delegate to the nominal set's deserialize:\n{de}"
    );
    assert!(
        de.contains("DeserializeFailure::NoVariantMatched"),
        "an unrecognized major type must fall through to NoVariantMatched:\n{de}"
    );
}

// ---------------------------------------------------------------------------------------------
// Out-of-scope byte-identity pins (Phase 2.2 nominalizes ONLY named non-generic 258 set rules)
// ---------------------------------------------------------------------------------------------

/// A NON-258 tag carries no SET SEMANTICS, so a non-258 tagged collection rule (single-arm or the
/// two-arm idiom) keeps the plain `Vec` inner and is never a set nominal. This pins the scope
/// boundary: only the 258 registry entry nominalizes; the structural collapse itself stays
/// tag-agnostic.
///
/// Both spellings WRAP (T1-13 — a tagged rule body owns its tag, so a transparent
/// `pub type Foo = Vec<u64>;` carrying the tag could not: `Foo::to_cbor_bytes` would have written a
/// BARE array while every embed site wrote `write_tag(42)` first). Wrapping is therefore NOT what
/// distinguishes 258 here — the inner representation is, which is what these assert.
#[test]
fn non_258_tagged_collections_keep_the_plain_vec_inner() {
    // single-arm non-258 tagged array
    let single = generate(
        "foo = #6.42([* uint])\nholder = [f: foo]\n",
        "non258_single",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        single.contains("pub(crate) inner: Vec<u64>,") && !single.contains("OrderedSet"),
        "a non-258 single-arm tagged array keeps the plain Vec inner:\n{single}"
    );
    // two-arm non-258 idiom
    let two_arm = generate(
        "foo = #6.42([* uint]) / [* uint]\nholder = [f: foo]\n",
        "non258_twoarm",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        two_arm.contains("pub(crate) inner: Vec<u64>,") && !two_arm.contains("OrderedSet"),
        "a non-258 two-arm idiom keeps the plain Vec inner:\n{two_arm}"
    );
}

/// An UNTAGGED array carrying `@duplicates reject` (a pre-Conway Cardano set) is NOT a tagged set
/// rule, so it stays a TRANSPARENT `OrderedSet` alias — nominalization is gated on the 258 tag, not on
/// the `reject` policy or the `OrderedSet` inner alone.
#[test]
fn untagged_reject_array_stays_transparent_alias() {
    let src = generate(
        "foo = [* uint] ; @duplicates reject\nholder = [f: foo]\n",
        "untagged_reject",
        PRESERVE,
    )
    .expect("must generate");
    assert!(
        src.contains("pub type Foo = OrderedSet<u64>;") && !src.contains("pub struct Foo"),
        "an untagged reject array stays a transparent OrderedSet alias (no tag = no nominalization):\n{src}"
    );
}

/// JSON stays TRANSPARENT: a nominal set serializes AS the bare inner array, not as a
/// `{inner, encodings}` object. The wrapper path hand-writes a `self.inner.serialize(serializer)`
/// serde `Serialize` (never a derive over the struct), and schemars routes through the inner type — so
/// the JSON shape is byte-for-byte the inner collection's, matching the transparent-alias behavior.
#[test]
fn set_nominal_json_is_transparent() {
    let src = generate(
        "foo = #6.258([* uint]) / [* uint]\nholder = [f: foo]\n",
        "json_transparent",
        &[
            "--preserve-encodings=true",
            "--json-serde-derives=true",
            "--json-schema-export=true",
            "--wasm=false",
        ],
    )
    .expect("must generate");
    assert!(
        src.contains("pub struct Foo {"),
        "the 258 set must nominalize under the json profile too:\n{src}"
    );
    // transparent serde: the Serialize delegates to the inner, and the struct is NOT #[derive(Serialize)]
    assert!(
        src.contains("impl serde::Serialize for Foo")
            && src.contains("self.inner.serialize(serializer)"),
        "the set nominal must hand-write a transparent serde Serialize delegating to the inner:\n{src}"
    );
    // schemars routes through the inner collection type (transparent), not a struct schema. The 258
    // reject default makes the inner `OrderedSet<u64>`, whose own schemars impl is the bare array.
    assert!(
        src.contains("<OrderedSet<u64> as schemars::JsonSchema>::json_schema(generator)"),
        "schemars must route through the inner collection type (transparent JSON):\n{src}"
    );
}
