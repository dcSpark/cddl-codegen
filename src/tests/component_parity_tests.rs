//! `component_api_parity` — the rust↔WIT public-surface differential.
//!
//! **What it catches.** A member emitted on the *rust* side of the generated crate boundary with no
//! *WIT* counterpart is invisible to every component gate that exists: the four-stage validity gate
//! judges the `.wit` against itself, the wasip2 build smoke compiles whatever glue was emitted for
//! whatever the `.wit` declared, and the snapshots pin both. None of them can demand a member that
//! is missing from BOTH. This is the structural net for that class, and it is the direct sibling of
//! `wasm_api_parity` — same one-directional rust→WIT question, same parsed-from-emitted-sources
//! principle, same ledger-with-a-two-way-staleness-guard.
//!
//! **Two deltas from the wasm sibling, by design.**
//! 1. *Multi-file by construction.* The wasm gate parses `wasm/src/generated/mod.rs` only and
//!    excludes directory inputs. The component face is inherently MULTI-INTERFACE — one WIT
//!    interface per input file — so this gate parses the RESOLVED WIT package (through the same
//!    pinned `wit-parser` the validity gate uses) against EVERY per-scope rust file.
//! 2. *Value types carry no funcs.* `Int` projects to a WIT `variant`, not a resource, so its rust
//!    inherent constructors have nowhere to land. That is a pre-declared ledger class rather than a
//!    structural carve-out, precisely so it stays visible and so the ledger's resurfaced guard has
//!    something live to reconcile.
//!
//! **Parsed from emitted sources, never from generator metadata.** The rust half is `syn` over the
//! emitted `.rs`; the WIT half is `wit-parser` over the emitted `.wit`; the exclusion records are
//! read out of the `.wit`'s own `// unexported:` comment rows. The rust→WIT name map is
//! [`crate::utils::convert_to_kebab_case`], a pure function of the rust name — the projection's
//! `WitPackage` does carry the pairing, but reading it would make this an intent check rather than
//! an output check, and the whole point is to catch emission bugs. (The kebab conversion's
//! `index_0`/`index_1` → `index0`/`index1` non-injectivity cannot mislead the match in valid output:
//! the in-generator collision detector rejects any spec where two names of one interface converge.)
//!
//! **Why each rust→WIT asymmetry class is legitimate (baked into the rules, not the ledger):**
//! - *`pub type` aliases impose nothing.* A CDDL alias and a named collection are RESOLVED THROUGH
//!   at their use sites and never surfaced as a WIT type — the documented type-mapping row. A rust
//!   `pub type Names = NonEmptyVec<String>` therefore has no WIT counterpart by design, and no
//!   members of its own to check either.
//! - *An EXCLUDED type is loud, not missing.* A type whose shape phase 1 cannot project is recorded
//!   in the emitted `.wit` as a `// unexported: <Ident> — <reason>` row. Rule 1 accepts that record
//!   as the counterpart; what it refuses is a rust type that is in NEITHER the WIT nor the
//!   exclusion list, which is the silent-drop class.
//! - *Member rules run only against a RESOURCE counterpart.* A WIT `enum` / `variant` / alias is a
//!   value type with no member namespace at all, so there is nothing for a getter to be missing
//!   from. `Int` is the one such type with rust inherent fns, and it is ledgered rather than
//!   carved out.
//! - *Encoding-capture fields are rust-only (preserve profile).* Under `--preserve-encodings` every
//!   encoding-capturing struct gains a `pub encodings: Option<XEncoding>` whose type is defined in
//!   `cbor_encodings.rs` — round-trip byte-fidelity metadata, never boundary API. Recognised
//!   structurally, exactly as the wasm sibling recognises it.
//!
//! **What it does NOT check.** Semantics. A getter that returns the wrong field, or a conversion
//! that aliases where it should clone, is the emission invariants' job (`component_tests`) and the
//! build smoke's. This is a PRESENCE differential, parse-only.

use std::collections::{BTreeMap, BTreeSet};
use std::panic::AssertUnwindSafe;

use crate::cli::Cli;
use crate::utils::convert_to_kebab_case;
use clap::Parser;

/// The sweep's axis: `(label, input, extra flags)`. `--component=true --wasm=false` is forced by the
/// harness — the wasm face adds no WIT surface, and turning it off keeps the rust half of the
/// differential to the types the spec actually declares.
///
/// It is a SUPERSET of `component_tests::COMPONENT_FIXTURES` (asserted below): every fixture the
/// component gates compile or validate is also differentialled, plus the cycle fixture, whose
/// deliberate generation refusal is what keeps [`EXPECTED_GENERATION_FAIL`]'s two-way guard live.
const PARITY_CASES: &[(&str, &str, &[&str])] = &[
    ("component-core", "tests/component-core/input.cddl", &[]),
    (
        "component-core-preserve",
        "tests/component-core/input.cddl",
        &["--preserve-encodings=true"],
    ),
    (
        "component-core-canonical",
        "tests/component-core/input.cddl",
        &["--preserve-encodings=true", "--canonical-form=true"],
    ),
    (
        "component-core-json",
        "tests/component-core/input.cddl",
        &["--json-serde-derives=true"],
    ),
    (
        "component-choices",
        "tests/component-choices/input.cddl",
        &[],
    ),
    (
        "component-choices-preserve",
        "tests/component-choices/input.cddl",
        &["--preserve-encodings=true"],
    ),
    (
        "component-choices-canonical",
        "tests/component-choices/input.cddl",
        &["--preserve-encodings=true", "--canonical-form=true"],
    ),
    (
        "component-choices-json",
        "tests/component-choices/input.cddl",
        &["--json-serde-derives=true"],
    ),
    ("component-bounds", "tests/component-bounds/input.cddl", &[]),
    (
        "component-bounds-preserve",
        "tests/component-bounds/input.cddl",
        &["--preserve-encodings=true"],
    ),
    (
        "component-bounds-canonical",
        "tests/component-bounds/input.cddl",
        &["--preserve-encodings=true", "--canonical-form=true"],
    ),
    // The bridging classes. A user-owned extern has no generated rust `pub struct`, so it owes the
    // differential nothing at all — which is itself worth sweeping, since a bridging resource that
    // started minting rust types would show up here.
    ("component-extern", "tests/component-extern/inputs", &[]),
    (
        "component-extern-canonical",
        "tests/component-extern/inputs",
        &["--preserve-encodings=true", "--canonical-form=true"],
    ),
    (
        "component-extern-json",
        "tests/component-extern/inputs",
        &["--json-serde-derives=true"],
    ),
    // The behavioral fixture. Its runtime claims belong to
    // `component_host_tests::component_host_behavior`; what THIS row adds is the one question that
    // gate cannot ask — whether a rust member exists that the boundary never offered, which a
    // harness driving the boundary would simply never think to call.
    ("component-host", "tests/component-host/inputs", &[]),
    (
        "component-multifile",
        "tests/component-multifile/inputs",
        &[],
    ),
    (
        "component-collection-refs",
        "tests/component-collection-refs/inputs",
        &[],
    ),
    ("component-rename", "tests/component-rename/input.cddl", &[]),
    (
        "component-any-alias",
        "tests/component-any-alias/input.cddl",
        &[],
    ),
    // The ident-hazard fixture is differentialled for the exclusion RULE's sake: `T` and the type
    // reaching it are rust structs with no WIT counterpart, and what makes that legitimate rather
    // than a silent drop is the `// unexported:` record the gate reads out of the emitted `.wit`.
    (
        "component-ident-hazard",
        "tests/component-ident-hazard/input.cddl",
        &[],
    ),
    // Externs and type choices: the exclusion path, on a spec nobody wrote for this face.
    ("multifile", "tests/multifile/inputs", &[]),
    // Mutually-referencing scopes — rejected under `--component` by the cycle detector.
    ("component-cycle", "tests/component-cycle/inputs", &[]),
];

/// `(label, reason)` whose generation deliberately refuses under `--component`. Four-state verdict
/// like the wasm sibling's: a listed label that now generates fails ("the refusal is gone — remove
/// the pin"), an unlisted refusal fails as a regression.
const EXPECTED_GENERATION_FAIL: &[(&str, &str)] = &[(
    "component-cycle",
    "two scopes reference each other, which is a WIT INTERFACE cycle — `wit_scope_cycles` refuses \
     it at IR finalization, and the same spec generates fine on the rust face",
)];

/// Deliberately-accepted rust→WIT asymmetries: `(label, "Type" | "Type::member", reason)`. A live
/// finding not covered by an entry fails the gate; an entry with no matching live finding fails as
/// "resurfaced" — a fix landed, or the rust member is gone.
const COMPONENT_PARITY_EXEMPT: &[(&str, &str, &str)] = &[
    // The `Int` value-type class (delta 2 in the module header). `Int` projects to a WIT `variant`
    // whose two arms carry the payload directly, so the rust constructors have no member namespace
    // to land in — a WIT caller writes `int.uint(n)` instead of calling a function.
    (
        "component-core",
        "Int::new_uint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    (
        "component-core",
        "Int::new_nint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    (
        "component-core-preserve",
        "Int::new_uint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    (
        "component-core-preserve",
        "Int::new_nint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    // The same class in the two flag postures item G adds. Neither seam changes the rust surface of
    // `Int` — `to_canonical_cbor_bytes` is a TRAIT method and the serde derives add no inherent fn —
    // so the findings are the pre-existing ones and no new class appeared.
    (
        "component-core-canonical",
        "Int::new_uint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    (
        "component-core-canonical",
        "Int::new_nint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    (
        "component-core-json",
        "Int::new_uint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    (
        "component-core-json",
        "Int::new_nint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    // The same class, on the behavioral fixture (its `record` carries a `delta: int` too).
    (
        "component-host",
        "Int::new_uint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    (
        "component-host",
        "Int::new_nint",
        "`Int` projects to the WIT `variant int { uint(u64), nint(u64) }` — a VALUE type with no \
         member namespace; a caller constructs the arm directly",
    ),
    // The `Int` parse-error enum the rust crate mints beside `Int` (for its `FromStr`/`TryFrom`
    // impls). It is not an IR type at all, so the projection never sees it; the WIT face reports
    // every failure as the `string` of the rust error's `Display`, so there is nothing for it to be.
    (
        "component-host",
        "IntError",
        "the rust-only error enum minted beside `Int` for its `FromStr`/`TryFrom` impls — not an IR \
         type, and the WIT face carries every failure as `result<_, string>`",
    ),
    (
        "component-core",
        "IntError",
        "the rust-only error enum minted beside `Int` for its `FromStr`/`TryFrom` impls — not an IR \
         type, and the WIT face carries every failure as `result<_, string>`",
    ),
    (
        "component-core-preserve",
        "IntError",
        "the rust-only error enum minted beside `Int` for its `FromStr`/`TryFrom` impls — not an IR \
         type, and the WIT face carries every failure as `result<_, string>`",
    ),
    (
        "component-core-canonical",
        "IntError",
        "the rust-only error enum minted beside `Int` for its `FromStr`/`TryFrom` impls — not an IR \
         type, and the WIT face carries every failure as `result<_, string>`",
    ),
    (
        "component-core-json",
        "IntError",
        "the rust-only error enum minted beside `Int` for its `FromStr`/`TryFrom` impls — not an IR \
         type, and the WIT face carries every failure as `result<_, string>`",
    ),
    // Exact-zero open records add a checked native insertion door because their rest map is private.
    // The component face deliberately retains the documented constructor-plus-snapshot posture; a
    // returned list never mutates its parent resource. These three profile rows keep that asymmetry
    // explicit and anti-rot guarded rather than pretending the native method crossed WIT.
    (
        "component-bounds",
        "ExactZeroOpen::insert_rest",
        "native exact-zero invariant door; the component face exposes constructor validation plus a detached rest snapshot",
    ),
    (
        "component-bounds-preserve",
        "ExactZeroOpen::insert_rest",
        "native exact-zero invariant door; the component face exposes constructor validation plus a detached rest snapshot",
    ),
    (
        "component-bounds-canonical",
        "ExactZeroOpen::insert_rest",
        "native exact-zero invariant door; the component face exposes constructor validation plus a detached rest snapshot",
    ),
];

/// `.rs` basenames the differential does NOT parse, under `rust/src/generated/`. Everything else
/// there is per-spec boundary surface and must be parsed — a file outside both sets means a new
/// emission surface, which fails loudly rather than being silently skipped.
const RUST_RUNTIME_GENERATED: &[&str] = &[
    "serialization.rs",
    "error.rs",
    "cbor_encodings.rs",
    "ordered_hash_map.rs",
    "any_cbor.rs",
    "key_demand_assertions.rs",
    "extern_interface_check.rs",
    "json_schema_gen.rs",
    "json_value_ser.rs",
];

/// The component crate's own generated tree carries exactly one file. It is the GLUE, not boundary
/// API — its surface is the WIT world — so the differential does not parse it; the guard exists so a
/// future per-scope split announces itself here instead of slipping past.
const ALLOWED_COMPONENT_GENERATED: &[&str] = &["mod.rs"];

// -------------------------------------------------------------------------------------------------
// The rust half (syn over every per-scope file)
// -------------------------------------------------------------------------------------------------

/// The rust crate's public per-spec surface, unioned across every emitted per-scope `mod.rs`.
#[derive(Default)]
struct RustSurface {
    /// `pub struct` / `pub enum` names.
    types: BTreeSet<String>,
    /// type -> its `pub` named fields as `field -> inner type ident` (the inner unwraps one
    /// `Option<..>` so the preserve encoding-capture exemption can recognise `Option<XEncoding>`).
    fields: BTreeMap<String, BTreeMap<String, Option<String>>>,
    /// type -> its inherent `pub fn` names.
    inherent_fns: BTreeMap<String, BTreeSet<String>>,
}

fn is_pub(vis: &syn::Visibility) -> bool {
    matches!(vis, syn::Visibility::Public(_))
}

/// Last path segment ident of a `Type::Path`, if any.
fn type_leaf_ident(ty: &syn::Type) -> Option<String> {
    match ty {
        syn::Type::Path(p) => p.path.segments.last().map(|s| s.ident.to_string()),
        _ => None,
    }
}

/// The "inner" type ident of a field: the last path segment, unwrapping a single `Option<..>` layer
/// so `Option<XEncoding>` reports `XEncoding`.
fn type_inner_ident(ty: &syn::Type) -> Option<String> {
    let syn::Type::Path(p) = ty else {
        return None;
    };
    let seg = p.path.segments.last()?;
    if seg.ident == "Option"
        && let syn::PathArguments::AngleBracketed(ab) = &seg.arguments
        && let Some(syn::GenericArgument::Type(inner)) = ab.args.first()
    {
        return type_inner_ident(inner);
    }
    Some(seg.ident.to_string())
}

fn parse_rust_surface_into(src: &str, s: &mut RustSurface) {
    let file = syn::parse_file(src).expect("generated rust source must parse");
    for item in &file.items {
        match item {
            syn::Item::Struct(st) if is_pub(&st.vis) => {
                let name = st.ident.to_string();
                s.types.insert(name.clone());
                if let syn::Fields::Named(named) = &st.fields {
                    let entry = s.fields.entry(name).or_default();
                    for f in &named.named {
                        if is_pub(&f.vis)
                            && let Some(id) = &f.ident
                        {
                            entry.insert(id.to_string(), type_inner_ident(&f.ty));
                        }
                    }
                }
            }
            syn::Item::Enum(en) if is_pub(&en.vis) => {
                s.types.insert(en.ident.to_string());
            }
            syn::Item::Impl(im) if im.trait_.is_none() => {
                if let Some(ty) = type_leaf_ident(&im.self_ty) {
                    let entry = s.inherent_fns.entry(ty).or_default();
                    for it in &im.items {
                        if let syn::ImplItem::Fn(f) = it
                            && is_pub(&f.vis)
                        {
                            entry.insert(f.sig.ident.to_string());
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// Pub struct names defined in the emitted `cbor_encodings.rs` files (the `*Encoding` set the
/// preserve encoding-capture exemption keys off). Empty for profiles that don't emit them.
fn parse_encoding_structs(files: &BTreeMap<String, String>) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for (path, src) in files {
        if !path.ends_with("/cbor_encodings.rs") {
            continue;
        }
        let file = syn::parse_file(src).expect("generated cbor_encodings.rs must parse");
        for item in &file.items {
            if let syn::Item::Struct(st) = item
                && is_pub(&st.vis)
            {
                out.insert(st.ident.to_string());
            }
        }
    }
    out
}

// -------------------------------------------------------------------------------------------------
// The WIT half (wit-parser over the emitted package) + the exclusion records
// -------------------------------------------------------------------------------------------------

/// The resolved WIT package's surface, flattened across interfaces.
#[derive(Default)]
struct WitSurface {
    /// Every type name declared by any interface (resources, enums, variants, aliases).
    types: BTreeSet<String>,
    /// Resource name -> its member item-names (`constructor` included under that literal name).
    resources: BTreeMap<String, BTreeSet<String>>,
    /// Rust idents the projection recorded as excluded, read from the `// unexported:` rows.
    excluded: BTreeSet<String>,
}

/// Resolve the emitted `.wit` files through `wit-parser` and read the surface back out.
///
/// Two passes over the same bytes, deliberately: the resolver is the authority on what the toolchain
/// SEES, and the raw text is the only place the exclusion records survive (a comment is not part of
/// a resolved package).
fn wit_surface(files: &BTreeMap<String, String>, label: &str) -> WitSurface {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_component_parity_{}_{}",
        std::process::id(),
        label.replace(['/', ' '], "_")
    ));
    let wit_dir = root.join(crate::generation::layout::COMPONENT_WIT_DIR);
    let mut surface = WitSurface::default();
    for (path, content) in files {
        if !path.ends_with(".wit") {
            continue;
        }
        let full = root.join(path);
        std::fs::create_dir_all(full.parent().unwrap()).unwrap();
        std::fs::write(&full, content).unwrap();
        for line in content.lines() {
            if let Some(rest) = line.trim_start().strip_prefix("// unexported: ") {
                surface
                    .excluded
                    .insert(rest.split(' ').next().unwrap_or(rest).to_owned());
            }
        }
    }

    let mut resolve = wit_parser::Resolve::default();
    let (package, _) = resolve
        .push_path(&wit_dir)
        .unwrap_or_else(|e| panic!("{label}: the emitted WIT does not resolve: {e:?}"));
    std::fs::remove_dir_all(&root).ok();

    let interfaces: Vec<wit_parser::InterfaceId> = resolve.packages[package]
        .interfaces
        .values()
        .copied()
        .collect();
    for id in interfaces {
        let iface = &resolve.interfaces[id];
        for (name, ty) in &iface.types {
            surface.types.insert(name.clone());
            if matches!(resolve.types[*ty].kind, wit_parser::TypeDefKind::Resource) {
                surface.resources.entry(name.clone()).or_default();
            }
        }
        for func in iface.functions.values() {
            let owner = match &func.kind {
                wit_parser::FunctionKind::Method(id)
                | wit_parser::FunctionKind::Static(id)
                | wit_parser::FunctionKind::Constructor(id)
                | wit_parser::FunctionKind::AsyncMethod(id)
                | wit_parser::FunctionKind::AsyncStatic(id) => *id,
                wit_parser::FunctionKind::Freestanding
                | wit_parser::FunctionKind::AsyncFreestanding => continue,
            };
            let Some(owner_name) = resolve.types[owner].name.clone() else {
                continue;
            };
            surface
                .resources
                .entry(owner_name)
                .or_default()
                .insert(func.item_name().to_owned());
        }
    }
    surface
}

// -------------------------------------------------------------------------------------------------
// The differential
// -------------------------------------------------------------------------------------------------

struct Finding {
    label: String,
    item: String,
    msg: String,
}

/// The WIT member name a rust inherent fn is owed. `new` is the one non-kebab mapping: it lowers to
/// the resource's `constructor`, which `wit-parser` reports under that literal item name.
fn wit_member_name(rust_fn: &str) -> String {
    if rust_fn == "new" {
        "constructor".to_owned()
    } else {
        convert_to_kebab_case(rust_fn)
    }
}

fn diff_surfaces(
    label: &str,
    rust: &RustSurface,
    wit: &WitSurface,
    encoding_structs: &BTreeSet<String>,
    out: &mut Vec<Finding>,
) {
    for t in &rust.types {
        let wit_name = convert_to_kebab_case(t);
        // Rule 1: every rust pub struct/enum is either projected (a WIT type of the kebab name) or
        // EXCLUDED AND RECORDED. Neither is the silent-drop class this gate exists for.
        if !wit.types.contains(&wit_name) {
            if !wit.excluded.contains(t) {
                out.push(Finding {
                    label: label.to_owned(),
                    item: t.clone(),
                    msg: format!(
                        "rust pub struct/enum has no WIT type `{wit_name}` and no \
                         `// unexported: {t}` record — it vanished from the WIT silently"
                    ),
                });
            }
            continue;
        }
        // Member rules run only against a RESOURCE counterpart: a WIT enum/variant/alias is a value
        // type with no member namespace, so there is nothing for a member to be missing from.
        let Some(members) = wit.resources.get(&wit_name) else {
            // …except that a rust type WITH inherent fns whose counterpart is a value type has
            // nowhere to put them, which is a real (ledgerable) asymmetry rather than a carve-out.
            if let Some(fns) = rust.inherent_fns.get(t) {
                for name in fns {
                    out.push(Finding {
                        label: label.to_owned(),
                        item: format!("{t}::{name}"),
                        msg: format!(
                            "rust inherent pub fn `{name}` has nowhere to land: `{t}` projects to \
                             the WIT VALUE type `{wit_name}`, which has no member namespace"
                        ),
                    });
                }
            }
            continue;
        };

        // Rule 2: every rust pub field has a WIT getter of the kebab name — except the preserve
        // encoding-capture field, which is round-trip metadata and never boundary API.
        if let Some(fields) = rust.fields.get(t) {
            for (f, inner) in fields {
                if let Some(inner_ident) = inner
                    && encoding_structs.contains(inner_ident)
                {
                    continue;
                }
                let getter = convert_to_kebab_case(f);
                if !members.contains(&getter) {
                    out.push(Finding {
                        label: label.to_owned(),
                        item: format!("{t}::{f}"),
                        msg: format!(
                            "rust pub field has no WIT getter `{getter}` on `resource {wit_name}`"
                        ),
                    });
                }
            }
        }

        // Rule 3: every rust inherent pub fn has a WIT member of the same kebab name (`new` ⇒ the
        // resource's `constructor`). Signatures are unchecked by design — the two ABIs differ by
        // construction (borrows in, owned handles out, `result<_, string>` for every failure).
        if let Some(fns) = rust.inherent_fns.get(t) {
            for name in fns {
                let member = wit_member_name(name);
                if !members.contains(&member) {
                    out.push(Finding {
                        label: label.to_owned(),
                        item: format!("{t}::{name}"),
                        msg: format!(
                            "rust inherent pub fn `{name}` has no WIT member `{member}` on \
                             `resource {wit_name}`"
                        ),
                    });
                }
            }
        }
    }
}

/// Collect `.rs` basenames under `prefix` outside `allowed`, so a new emission surface fails loudly
/// instead of escaping the differential.
fn stray_keys(files: &BTreeMap<String, String>, prefix: &str, allowed: &[&str]) -> Vec<String> {
    let mut stray = vec![];
    for k in files.keys() {
        if let Some(rest) = k.strip_prefix(prefix) {
            let base = rest.rsplit('/').next().unwrap_or(rest);
            if base.ends_with(".rs") && !allowed.contains(&base) {
                stray.push(base.to_owned());
            }
        }
    }
    stray.sort();
    stray.dedup();
    stray
}

/// The axis, the pins and the fixture-coverage guard. Kept in its own test for the same reason the
/// wasm sibling keeps its axis assertions apart: each is only correct when ONE test sees the WHOLE
/// axis, and none of them generates anything, so keeping them whole costs microseconds.
#[test]
fn component_api_parity_axes_and_pins_are_live() {
    let labels: BTreeSet<&str> = PARITY_CASES.iter().map(|(l, _, _)| *l).collect();
    assert_eq!(
        labels.len(),
        PARITY_CASES.len(),
        "PARITY_CASES labels must be unique — they are the ledger keys"
    );
    for (label, _) in EXPECTED_GENERATION_FAIL {
        assert!(
            labels.contains(label),
            "EXPECTED_GENERATION_FAIL names label `{label}` the sweep never visits — stale pin"
        );
    }
    let generating: BTreeSet<&str> = labels
        .iter()
        .copied()
        .filter(|l| !EXPECTED_GENERATION_FAIL.iter().any(|(p, _)| p == l))
        .collect();
    for (label, item, _) in COMPONENT_PARITY_EXEMPT {
        assert!(
            generating.contains(label),
            "COMPONENT_PARITY_EXEMPT entry ({label}, {item}) names a label the sweep never \
             differentials, so its resurfaced check is vacuous — stale pin"
        );
    }
    // Every fixture the component gates compile or validate is differentialled too: a fixture added
    // there and not here would be checked by every gate EXCEPT the one that asks what the boundary
    // dropped.
    let swept: BTreeSet<(&str, &[&str])> = PARITY_CASES
        .iter()
        .map(|(_, input, extra)| (*input, *extra))
        .collect();
    for (input, extra) in super::component_tests::COMPONENT_FIXTURES {
        assert!(
            swept.contains(&(*input, *extra)),
            "component fixture ({input}, {extra:?}) is compiled/validated by component_tests but \
             never differentialled — add it to PARITY_CASES"
        );
    }
}

/// The differential itself. One test, not sharded: the whole sweep is in-process generation plus
/// parsing (measured at ~2 s), which is well under the point where splitting buys anything.
#[test]
fn component_api_parity() {
    let mut findings: Vec<Finding> = vec![];
    let mut strays: Vec<String> = vec![];
    let mut gen_failures: Vec<String> = vec![];
    let mut refusal_gone: Vec<String> = vec![];
    // The vacuity guard's counter: a differential that compared nothing would pass on an empty set,
    // which is worse than no gate at all.
    let mut obligations = 0usize;

    for (label, input, extra) in PARITY_CASES {
        let expected_fail = EXPECTED_GENERATION_FAIL.iter().any(|(l, _)| l == label);
        let mut args = vec![
            "cddl-codegen",
            "--input",
            input,
            "--output",
            "component_parity_unused",
            "--component=true",
            "--wasm=false",
        ];
        args.extend(extra.iter().copied());
        let cli = Cli::parse_from(args);
        let generated = match std::panic::catch_unwind(AssertUnwindSafe(|| {
            crate::api::generated_strings(&cli)
        })) {
            Ok(Ok(files)) => Ok(files),
            Ok(Err(e)) => Err(format!("error: {e}")),
            Err(payload) => Err(format!(
                "PANIC: {}",
                payload
                    .downcast_ref::<String>()
                    .map(String::as_str)
                    .or_else(|| payload.downcast_ref::<&str>().copied())
                    .unwrap_or("<non-string payload>")
            )),
        };
        let files = match (expected_fail, generated) {
            (true, Err(_)) => continue,
            (true, Ok(_)) => {
                refusal_gone.push((*label).to_owned());
                continue;
            }
            (false, Err(detail)) => {
                gen_failures.push(format!("{label} ({input}): {detail}"));
                continue;
            }
            (false, Ok(files)) => files,
        };

        for base in stray_keys(&files, "rust/src/generated/", &{
            let mut allowed = RUST_RUNTIME_GENERATED.to_vec();
            allowed.push("mod.rs");
            allowed
        }) {
            strays.push(format!("{label} rust: {base}"));
        }
        for base in stray_keys(
            &files,
            "component/src/generated/",
            ALLOWED_COMPONENT_GENERATED,
        ) {
            strays.push(format!("{label} component: {base}"));
        }

        let mut rust = RustSurface::default();
        for (path, src) in &files {
            let Some(rest) = path.strip_prefix("rust/src/generated/") else {
                continue;
            };
            let base = rest.rsplit('/').next().unwrap_or(rest);
            if base != "mod.rs" {
                continue;
            }
            parse_rust_surface_into(src, &mut rust);
        }
        let encoding_structs = parse_encoding_structs(&files);
        let wit = wit_surface(&files, label);
        obligations += rust.fields.values().map(BTreeMap::len).sum::<usize>()
            + rust.inherent_fns.values().map(BTreeSet::len).sum::<usize>()
            + rust.types.len();
        diff_surfaces(label, &rust, &wit, &encoding_structs, &mut findings);
    }

    assert!(
        strays.is_empty(),
        "unexpected file(s) under a generated dir — a new emission surface the component parity \
         differential does not parse; extend component_api_parity to cover it:\n{}",
        strays.join("\n")
    );
    assert!(
        refusal_gone.is_empty(),
        "these EXPECTED_GENERATION_FAIL pins now generate — the refusal is gone; remove them:\n{}",
        refusal_gone.join("\n")
    );
    assert!(
        gen_failures.is_empty(),
        "generation failed for these cases (a regression, or — if a genuine new refusal — pin it in \
         EXPECTED_GENERATION_FAIL with a reason):\n{}",
        gen_failures.join("\n")
    );
    assert!(
        obligations >= 60,
        "the differential compared only {obligations} rust-surface obligations — far below what the \
         fixtures declare, so it has gone vacuous (a parse or path filter regressed)"
    );

    let exempt: BTreeSet<(&str, &str)> = COMPONENT_PARITY_EXEMPT
        .iter()
        .map(|(l, i, _)| (*l, *i))
        .collect();
    let live: BTreeSet<(&str, &str)> = findings
        .iter()
        .map(|f| (f.label.as_str(), f.item.as_str()))
        .collect();
    let resurfaced: Vec<&(&str, &str, &str)> = COMPONENT_PARITY_EXEMPT
        .iter()
        .filter(|(l, i, _)| !live.contains(&(*l, *i)))
        .collect();
    let unexempted: Vec<&Finding> = findings
        .iter()
        .filter(|f| !exempt.contains(&(f.label.as_str(), f.item.as_str())))
        .collect();

    assert!(
        resurfaced.is_empty(),
        "these COMPONENT_PARITY_EXEMPT entries no longer match any live finding — a fix landed (or \
         the rust member is gone); remove them from the ledger:\n{}",
        resurfaced
            .iter()
            .map(|(l, i, r)| format!("  ({l}, {i}) — was: {r}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    assert!(
        unexempted.is_empty(),
        "rust→WIT API-surface parity gaps (fix the projection/emitter, or — deliberately — add a \
         COMPONENT_PARITY_EXEMPT entry with a reason):\n{}",
        unexempted
            .iter()
            .map(|f| format!("  [{}] {}: {}", f.label, f.item, f.msg))
            .collect::<Vec<_>>()
            .join("\n")
    );
}
