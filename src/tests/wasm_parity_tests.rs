//! `wasm_api_parity` — the rust↔wasm public-API-surface differential.
//!
//! **What it catches.** A member emitted on the *rust* side of the generated crate boundary with NO
//! *wasm* counterpart is invisible to every other oracle: snapshots pin whatever was emitted, the
//! compile gates compile whatever was emitted, and the wasm test mint is *written against* the
//! surface that exists — it exercises what's there, it can't demand what's missing. The proven
//! instance is `4e5b837`: wrapper types shipped for years with a rust `new`/`From` but no wasm ctor
//! or getter — `generate_wrapper_struct` built a `wasm_new` and never pushed it (dead code), caught
//! only by reading the generator. This gate closes that class structurally: per generated type it
//! enumerates the rust crate's public ctor/accessor/type surface and asserts the wasm crate exposes
//! a corresponding boundary member (or a **ledgered exemption**), parsing the emitted sources with
//! `syn` rather than asking the generator to self-report — an output-side check catches emission
//! bugs, not just intent drift.
//!
//! **One-directional (rust → wasm).** Only rust-side members impose obligations. Wasm-side extras
//! (`kind`/`as_*`/`has_*`/`set_*`/`len`/`insert`/`keys`/`to_cbor_bytes`/`from_cbor_bytes`, …) are
//! deliberately unchecked — the wasm ABI legitimately adds surface the rust type doesn't have.
//!
//! **Why each rust→wasm asymmetry class is legitimate (baked into the rules, not the ledger):**
//! - *`pub use`d / aliased types have no members to check.* When a rust struct/enum surfaces on the
//!   wasm side as a `pub use` re-export (Copy c-enums) or a `pub type` alias, rules 1–2 count that as
//!   full parity and the member rules (3–4) do NOT run: a `pub use` *is* the same type, and a rust
//!   alias has no inherent members. This is item 8's "`pub use`d Copy enums" exemption, structural.
//! - *No setter obligation.* A rust `pub` field yields a wasm getter (rule 3), never a setter: wasm
//!   emits `set_*` only for optional fields, so rust pub-field mutability has no uniform wasm
//!   counterpart by design.
//! - *Return types unchecked (rule 4).* Boundary conversions differ by construction
//!   (`Result<Self, DeserializeError>` vs `Result<T, JsError>`, by-ref args, `.into()`), so a
//!   same-name/same-arity wasm fn satisfies the obligation; only ABSENCE is a finding.
//! - *Trait impls excluded on both sides.* `From`/`TryFrom`/`AsRef`/`Serialize`/`Deserialize` are
//!   never counted (the walk only reads inherent impls), so the "rust-only trait impls" class, the
//!   collection-API-inheritance class (a transparent `pub type Nums = Vec<u64>` has no enumerable
//!   members), and the tag-over-struct-folding class all fall out structurally.
//!
//! **What it does NOT check.** Semantic wrongness — an identity `.into()` where a transform was
//! needed — stays `wasm_matrix_roundtrips`' job (this gate is a *presence* differential, parse-only).
//! It also scopes to `src/generated/mod.rs`: `serialization.rs`/`error.rs` are trait impls + runtime
//! plumbing (`CBORReadLen` etc.), not per-type boundary API. A file-set guard fails loudly if a
//! future multi-file emission mode grows the generated dir, so the differential can't silently escape.
//!
//! **Inputs & cost.** Every `tests/matrix_wasm/*.cddl` cell (the wasm-ABI shape × role grid — even
//! `WASM_MATRIX_SKIP` ones, whose emitted sources still *parse* even when they don't standalone
//! *compile*) plus the two depth fixtures `tests/core/input.cddl` and `example/test.cddl` (kitchen-
//! sink shapes the minimal cells don't reach). Each is generated `--wasm=true` and parsed — no cargo
//! check/test of the generated crates, so the whole gate is ~100 generations (tens of seconds),
//! far lighter than its compile sibling `wasm_matrix_compiles`. Always-on (no `#[ignore]`), so it
//! joins the plain `cargo test` / check.ts local tier.
//!
//! **Ledger + anti-rot (the `WASM_MATRIX_SKIP` idiom).** `PARITY_EXEMPT` holds deliberately-accepted
//! asymmetries by `(input, "Type" or "Type::member", reason)`. A finding matching a ledger entry is
//! expected (no failure); a ledger entry matching NO live finding fails as "resurfaced" (a fix
//! landed, or the rust member is gone — remove the entry); an unexempted finding fails with the
//! remedy spelled out (fix the emitter, or — deliberately — ledger it with a reason).

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use super::integration_tests::{checkout_hash, tool_cmd};

/// Deliberately-accepted rust→wasm asymmetries: `(input label, "Type" | "Type::member", reason)`.
/// Starts EMPTY — every legitimate asymmetry class is baked into the correspondence rules above, not
/// listed here (see the module header). A live finding not covered by an entry fails the gate; an
/// entry with no matching live finding fails as "resurfaced".
const PARITY_EXEMPT: &[(&str, &str, &str)] = &[
    // TEMPORARY: the named-table wasm alias is emitted private (`type Mp = MapU64ToText;`) at
    // `generation.rs`'s already-generated-map branch, which omits `.vis("pub")`. These three entries
    // document that pre-fix finding so the gate lands green here; the very next commit adds the
    // one-line `.vis("pub")` fix and REMOVES these — at which point the anti-rot guard fires
    // ("resurfaced") if either half is forgotten.
    (
        "collmap__newtype-inner",
        "Mp",
        "named-table wasm alias emitted private (missing .vis(\"pub\") in generation.rs)",
    ),
    (
        "passthrumap__newtype-inner",
        "Mp",
        "named-table wasm alias emitted private (missing .vis(\"pub\") in generation.rs)",
    ),
    (
        "tests/core",
        "StandaloneText",
        "named-table wasm alias emitted private (missing .vis(\"pub\") in generation.rs)",
    ),
];

/// Only these files may appear under `rust/src/generated/`; only `mod.rs` under `wasm/src/generated/`.
/// A file outside these sets means a new emission surface the differential doesn't parse — fail with
/// "extend wasm_api_parity" rather than silently skip it. `serialization.rs`/`error.rs` are
/// deliberately out of scope (runtime plumbing, not per-type boundary API).
const ALLOWED_RUST_GENERATED: &[&str] = &["mod.rs", "serialization.rs", "error.rs"];
const ALLOWED_WASM_GENERATED: &[&str] = &["mod.rs"];

/// The rust crate's public API surface, parsed from `rust/src/generated/mod.rs`.
#[derive(Default)]
struct RustSurface {
    /// `pub struct` / `pub enum` names.
    types: BTreeSet<String>,
    /// type -> its `pub` named fields (structs only; enums have no top-level named fields).
    fields: BTreeMap<String, BTreeSet<String>>,
    /// type -> inherent `pub fn`s as (name, self-excluded arity).
    inherent_fns: BTreeMap<String, BTreeSet<(String, usize)>>,
    /// `pub type` alias names.
    type_aliases: BTreeSet<String>,
}

/// The wasm crate's public API surface, parsed from `wasm/src/generated/mod.rs`.
#[derive(Default)]
struct WasmSurface {
    /// `pub struct` / `pub enum` DEFINED here (member rules run only against these).
    defined_types: BTreeSet<String>,
    /// `pub use` re-export leaf idents.
    reexports: BTreeSet<String>,
    /// `pub type` alias names (public visibility only — a PRIVATE alias does not satisfy rule 2).
    pub_type_aliases: BTreeSet<String>,
    /// type -> inherent `pub fn`s as (name, self-excluded arity).
    members: BTreeMap<String, BTreeSet<(String, usize)>>,
}

fn is_pub(vis: &syn::Visibility) -> bool {
    matches!(vis, syn::Visibility::Public(_))
}

/// Last path segment of a `Type::Path` (the type an `impl` block is *for*), if any.
fn impl_self_ident(ty: &syn::Type) -> Option<String> {
    match ty {
        syn::Type::Path(p) => p.path.segments.last().map(|s| s.ident.to_string()),
        _ => None,
    }
}

/// Count of non-receiver args (arity with `self` excluded).
fn self_excluded_arity(sig: &syn::Signature) -> usize {
    sig.inputs
        .iter()
        .filter(|arg| matches!(arg, syn::FnArg::Typed(_)))
        .count()
}

/// Collect every leaf ident a `use` tree binds (final path segment / rename target), so both
/// `pub use crate::Fe;` and a grouped `pub use crate::{A, B};` contribute their names.
fn collect_use_leaves(tree: &syn::UseTree, out: &mut BTreeSet<String>) {
    match tree {
        syn::UseTree::Path(p) => collect_use_leaves(&p.tree, out),
        syn::UseTree::Name(n) => {
            out.insert(n.ident.to_string());
        }
        syn::UseTree::Rename(r) => {
            out.insert(r.rename.to_string());
        }
        syn::UseTree::Group(g) => {
            for t in &g.items {
                collect_use_leaves(t, out);
            }
        }
        syn::UseTree::Glob(_) => {}
    }
}

fn parse_rust_surface(src: &str) -> RustSurface {
    let file = syn::parse_file(src).expect("generated rust mod.rs must parse");
    let mut s = RustSurface::default();
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
                            entry.insert(id.to_string());
                        }
                    }
                }
            }
            syn::Item::Enum(en) if is_pub(&en.vis) => {
                s.types.insert(en.ident.to_string());
            }
            syn::Item::Type(ty) if is_pub(&ty.vis) => {
                s.type_aliases.insert(ty.ident.to_string());
            }
            syn::Item::Impl(im) if im.trait_.is_none() => {
                if let Some(ty) = impl_self_ident(&im.self_ty) {
                    let entry = s.inherent_fns.entry(ty).or_default();
                    for it in &im.items {
                        if let syn::ImplItem::Fn(f) = it
                            && is_pub(&f.vis)
                        {
                            entry.insert((f.sig.ident.to_string(), self_excluded_arity(&f.sig)));
                        }
                    }
                }
            }
            _ => {}
        }
    }
    s
}

fn parse_wasm_surface(src: &str) -> WasmSurface {
    let file = syn::parse_file(src).expect("generated wasm mod.rs must parse");
    let mut s = WasmSurface::default();
    for item in &file.items {
        match item {
            syn::Item::Struct(st) if is_pub(&st.vis) => {
                s.defined_types.insert(st.ident.to_string());
            }
            syn::Item::Enum(en) if is_pub(&en.vis) => {
                s.defined_types.insert(en.ident.to_string());
            }
            syn::Item::Type(ty) => {
                if is_pub(&ty.vis) {
                    s.pub_type_aliases.insert(ty.ident.to_string());
                }
            }
            syn::Item::Use(u) if is_pub(&u.vis) => {
                collect_use_leaves(&u.tree, &mut s.reexports);
            }
            syn::Item::Impl(im) if im.trait_.is_none() => {
                if let Some(ty) = impl_self_ident(&im.self_ty) {
                    let entry = s.members.entry(ty).or_default();
                    for it in &im.items {
                        if let syn::ImplItem::Fn(f) = it
                            && is_pub(&f.vis)
                        {
                            entry.insert((f.sig.ident.to_string(), self_excluded_arity(&f.sig)));
                        }
                    }
                }
            }
            _ => {}
        }
    }
    s
}

/// A single rust→wasm parity gap. `item` is `"Type"` (rules 1–2) or `"Type::member"` (rules 3–4).
struct Finding {
    label: String,
    item: String,
    msg: String,
}

/// Run the four correspondence rules for one input's parsed surfaces, appending any gaps.
fn diff_surfaces(label: &str, rust: &RustSurface, wasm: &WasmSurface, out: &mut Vec<Finding>) {
    // A rust struct/enum has a wasm counterpart if a wasm struct/enum is defined, a `pub use`
    // re-exports it, or a PUBLIC `pub type` aliases it.
    let wasm_has_type = |name: &str| {
        wasm.defined_types.contains(name)
            || wasm.reexports.contains(name)
            || wasm.pub_type_aliases.contains(name)
    };

    // Rule 1: every rust pub struct/enum has a wasm counterpart.
    for t in &rust.types {
        if !wasm_has_type(t) {
            out.push(Finding {
                label: label.to_string(),
                item: t.clone(),
                msg: "rust pub struct/enum has no wasm counterpart (no same-named wasm \
                      struct/enum, `pub use` re-export, or `pub type` alias)"
                    .to_string(),
            });
        }
    }

    // Rule 2: every rust `pub type` alias has a same-named wasm PUBLIC alias or wasm type. A PRIVATE
    // wasm alias does not satisfy this — that's exactly the named-table-alias finding class.
    for a in &rust.type_aliases {
        if !wasm_has_type(a) {
            out.push(Finding {
                label: label.to_string(),
                item: a.clone(),
                msg: "rust `pub type` alias has no PUBLIC wasm counterpart (a private wasm `type` \
                      alias does not count — emit it `pub`)"
                    .to_string(),
            });
        }
    }

    // Member rules 3–4 run only when a same-named wasm TYPE is DEFINED (struct/enum). A `pub use` or
    // alias counterpart is full parity under rules 1–2 (no members to check).
    for t in &rust.types {
        if !wasm.defined_types.contains(t) {
            continue;
        }
        let wasm_members = wasm.members.get(t);
        let wasm_names: BTreeSet<&str> = wasm_members
            .map(|m| m.iter().map(|(n, _)| n.as_str()).collect())
            .unwrap_or_default();

        // Rule 3: every rust pub field `f` on `T` has a wasm inherent getter `f` on `T`.
        if let Some(fields) = rust.fields.get(t) {
            for f in fields {
                if !wasm_names.contains(f.as_str()) {
                    out.push(Finding {
                        label: label.to_string(),
                        item: format!("{t}::{f}"),
                        msg: "rust pub field has no wasm getter of the same name".to_string(),
                    });
                }
            }
        }

        // Rule 4: every rust inherent pub fn on `T` has a wasm inherent fn of the SAME name AND arity
        // (self excluded; return types unchecked by design).
        if let Some(fns) = rust.inherent_fns.get(t) {
            for (name, arity) in fns {
                let matched = wasm_members
                    .map(|m| m.contains(&(name.clone(), *arity)))
                    .unwrap_or(false);
                if !matched {
                    out.push(Finding {
                        label: label.to_string(),
                        item: format!("{t}::{name}"),
                        msg: format!(
                            "rust inherent pub fn `{name}` (arity {arity}, self excluded) has no \
                             wasm inherent fn of the same name and arity"
                        ),
                    });
                }
            }
        }
    }
}

/// Assert the generated dir contains no `.rs` file outside `allowed`, so a future multi-file
/// emission mode can't silently escape the differential.
fn assert_file_set(dir: &Path, allowed: &[&str], crate_label: &str) {
    let mut stray = vec![];
    if let Ok(rd) = std::fs::read_dir(dir) {
        for e in rd.flatten() {
            let p = e.path();
            if p.extension().and_then(|x| x.to_str()) == Some("rs") {
                let name = p.file_name().unwrap().to_str().unwrap().to_string();
                if !allowed.contains(&name.as_str()) {
                    stray.push(name);
                }
            }
        }
    }
    stray.sort();
    assert!(
        stray.is_empty(),
        "unexpected file(s) under {crate_label} generated dir {dir:?}: {} — a new emission surface \
         the parity differential doesn't parse; extend wasm_api_parity to cover it (allowed: {:?})",
        stray.join(", "),
        allowed
    );
}

/// The full input set: every wasm-matrix cell (by file stem) plus the two depth fixtures under
/// descriptive labels. Labels are the ledger keys, so they must be stable and unique.
fn parity_inputs() -> Vec<(String, PathBuf)> {
    let mut cells: Vec<PathBuf> = std::fs::read_dir("tests/matrix_wasm")
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    cells.sort();
    assert!(
        !cells.is_empty(),
        "no wasm-matrix fixtures in tests/matrix_wasm (run `bun run project_wasm_matrix.ts`)"
    );
    let mut inputs: Vec<(String, PathBuf)> = cells
        .into_iter()
        .map(|p| (p.file_stem().unwrap().to_str().unwrap().to_string(), p))
        .collect();
    // Depth fixtures — kitchen-sink shapes the minimal cells don't reach.
    inputs.push((
        "tests/core".to_string(),
        PathBuf::from("tests/core/input.cddl"),
    ));
    inputs.push(("example".to_string(), PathBuf::from("example/test.cddl")));
    inputs
}

#[test]
fn wasm_api_parity() {
    let inputs = parity_inputs();

    let root =
        std::env::temp_dir().join(format!("cddl_codegen_wasm_parity_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&root);

    let mut findings: Vec<Finding> = vec![];
    for (label, input) in &inputs {
        // A `/` in a depth-fixture label would nest; flatten for the scratch subdir.
        let out = root.join(label.replace('/', "_"));
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=true")
            .output()
            .unwrap();
        assert!(
            gen_out.status.success(),
            "generation failed for {label} ({input:?}):\n{}",
            String::from_utf8_lossy(&gen_out.stderr)
        );

        let rust_gen = out.join("rust/src/generated");
        let wasm_gen = out.join("wasm/src/generated");
        assert!(
            rust_gen.join("mod.rs").exists(),
            "{label}: no rust/src/generated/mod.rs"
        );
        assert!(
            wasm_gen.join("mod.rs").exists(),
            "{label}: no wasm/src/generated/mod.rs (expected a wasm crate for every input)"
        );
        assert_file_set(&rust_gen, ALLOWED_RUST_GENERATED, "rust");
        assert_file_set(&wasm_gen, ALLOWED_WASM_GENERATED, "wasm");

        let rust_src = std::fs::read_to_string(rust_gen.join("mod.rs")).unwrap();
        let wasm_src = std::fs::read_to_string(wasm_gen.join("mod.rs")).unwrap();
        let rust = parse_rust_surface(&rust_src);
        let wasm = parse_wasm_surface(&wasm_src);
        diff_surfaces(label, &rust, &wasm, &mut findings);
    }
    let _ = std::fs::remove_dir_all(&root);

    // Reconcile findings against the ledger (the `WASM_MATRIX_SKIP` idiom).
    let exempt: BTreeSet<(&str, &str)> = PARITY_EXEMPT.iter().map(|(l, i, _)| (*l, *i)).collect();
    let live: BTreeSet<(&str, &str)> = findings
        .iter()
        .map(|f| (f.label.as_str(), f.item.as_str()))
        .collect();

    let unexempted: Vec<&Finding> = findings
        .iter()
        .filter(|f| !exempt.contains(&(f.label.as_str(), f.item.as_str())))
        .collect();
    let resurfaced: Vec<&(&str, &str, &str)> = PARITY_EXEMPT
        .iter()
        .filter(|(l, i, _)| !live.contains(&(*l, *i)))
        .collect();

    assert!(
        resurfaced.is_empty(),
        "these PARITY_EXEMPT entries no longer match any live finding — a fix landed (or the rust \
         member is gone); remove them from the ledger:\n{}",
        resurfaced
            .iter()
            .map(|(l, i, r)| format!("  ({l}, {i}) — was: {r}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    assert!(
        unexempted.is_empty(),
        "rust→wasm API-surface parity gaps (fix the emitter, or — deliberately — add a \
         PARITY_EXEMPT entry with a reason):\n{}",
        unexempted
            .iter()
            .map(|f| format!("  [{}] {}: {}", f.label, f.item, f.msg))
            .collect::<Vec<_>>()
            .join("\n")
    );
}
