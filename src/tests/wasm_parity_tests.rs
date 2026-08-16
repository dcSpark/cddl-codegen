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
//!   alias has no inherent members. This is the structural "`pub use`d Copy enums" exemption.
//! - *No setter obligation.* A rust `pub` field yields a wasm getter (rule 3), never a setter: wasm
//!   emits `set_*` only for optional fields, so rust pub-field mutability has no uniform wasm
//!   counterpart by design.
//! - *Encoding-capture fields are rust-only (preserve profile).* Under `--preserve-encodings` every
//!   encoding-capturing struct gains a `pub encodings: Option<XEncoding>` field whose `XEncoding`
//!   type is defined in `cbor_encodings.rs`, never on the wasm boundary — round-trip byte-fidelity
//!   metadata, not user-facing API. Rule 3 recognises this structurally (a `pub` field whose type is
//!   `Option<X>`/`X` with `X` a struct defined in the emitted `cbor_encodings.rs`) and imposes no
//!   wasm getter obligation, so the class needs no per-type ledger entries.
//! - *An open table's TYPED row is flattened, not hung off a getter.* `t = { * K_t => V_t, * K_r =>
//!   V_r }` mints a struct whose wasm class carries the typed row's map surface DIRECTLY
//!   (`insert`/`get`/`len`/`keys`) — the set nominal's call, because a wasm class has no `Deref` —
//!   so the typed row's `pub` rust field has no same-named getter and never will. Rule 3 recognises
//!   it by the field's provenance markers (`generation::OPEN_TABLE_TYPED_ROW_DOC`,
//!   `generation::OPEN_TABLE_NON_EMPTY_TYPED_ROW_DOC`, and
//!   `generation::OPEN_TABLE_BOUNDED_TYPED_ROW_DOC`), not by shape: a shape test would also
//!   swallow the CATCH-ALL row, which does owe its `rest()` getter. Being a design decision rather
//!   than a shape accident, it belongs in the rules and not in `PARITY_EXEMPT` — a ledger entry per
//!   fixture × profile would grow with the fixture set and say the same thing each time.
//! - *Return types unchecked (rule 4).* Boundary conversions differ by construction
//!   (`Result<Self, DeserializeError>` vs `Result<T, JsError>`, by-ref args, `.into()`), so a
//!   same-name/same-arity wasm fn satisfies the obligation; only ABSENCE is a finding.
//! - *Trait impls excluded on both sides.* `From`/`TryFrom`/`AsRef`/`Serialize`/`Deserialize` are
//!   never counted (the walk only reads inherent impls), so the "rust-only trait impls" class, the
//!   collection-API-inheritance class (a transparent `pub type Nums = Vec<u64>` has no enumerable
//!   members), and the tag-over-struct-folding class all fall out structurally.
//!
//! **Rule 5 — JS-name visibility (an ADDITIONAL finding class layered on rules 1–2).** Rules 1–2
//! accept a *public* `pub type` alias as a rust type's wasm counterpart, but that is rust-source-level
//! parity only: wasm_bindgen exports NO type aliases, so an alias-only counterpart means the CDDL rule
//! name never reaches JS. For every rust-surface name (rust pub struct/enum ∪ rust `pub type` alias)
//! whose ONLY wasm counterpart is a `pub type` alias, rule 5 resolves the alias's TARGET (last path
//! segment ident) and emits a finding iff (a) the target is a struct/enum DEFINED in the wasm mod (a
//! real `#[wasm_bindgen]` class) AND (b) that target name is NOT itself on the rust surface. Both
//! carve-outs are structural, not ledgered:
//! - *Transparent alias to a non-wasm-defined target* (primitive/std/`Option`/…, e.g.
//!   `pub type U8 = u8;`, `pub type OptText = Option<TaggedText>;`, `pub type ParenCbor = String;`):
//!   JS represents the value natively — no class exists for the shape (`docs/docs/wasm_differences.mdx`).
//!   Not a finding. (The target-leaf resolution does NOT unwrap `Option`, so `OptText`'s target is the
//!   std `Option`, not the wasm-defined inner.)
//! - *Alias to a wasm-defined target that IS on the rust surface* (`pub type FooBytes = Foo;`, `Foo` a
//!   rust pub struct): a pure CDDL-level alias present identically on both sides — the JS class carries
//!   a genuine CDDL rule name. Not a finding.
//! - *SYNTHESIZED anonymous generic-collection/table INSTANCE alias* (doc-marked with
//!   `generation::SYNTHESIZED_INSTANCE_ALIAS_DOC`): `gcoll<foo>` → `pub type GcollFoo = Vec<Foo>`
//!   (wasm alias to the structural `FooList`), `gtbl<uint, text>` → `GtblU64Text` (→ `MapU64ToText`),
//!   `gcoll<uint>` → `GcollU64` (exposable, inlined to a bare `Vec` on the wasm side — the rule-2
//!   twin of this carve-out). There is NO CDDL rule name at stake: the user wrote an anonymous
//!   instance, which crosses the boundary exactly as its inline equivalent's STRUCTURAL class, the
//!   documented lowering (`docs/docs/wasm_differences.mdx`). Rules 2 AND 5 skip these. The
//!   discriminator is PROVENANCE (the doc marker the generator emits on synthesized instance idents
//!   only, never on a user rule like `gcn = gcoll<foo>`), NOT a source-shape heuristic: a sole-owner
//!   named-table alias (`pub type Mp = MapU64ToText;`) is rust-side a bare-collection alias too, so a
//!   "aliases a std collection" test would blind rule 5 to a recurrence of the (fixed) named-table
//!   degradation bug it exists to catch. Pinned by `synthesized_instance_alias_marker_provenance`.
//!
//! What remains — alias to a wasm-defined target whose name is generator-invented (`MapU64ToText`, not
//! on the rust surface) AND NOT provenance-marked as a synthesized instance — is exactly the
//! usage-dependent-JS-class-name bug: a genuine CDDL RULE name that reaches JS only as an invented
//! class, so its JS name flips with unrelated spec content (`cddl-matrix/roadmap.toml` § findings).
//! `pub use` counterparts stay JS-visible by design (Copy c-enums carry `#[wasm_bindgen]` at their
//! rust-crate definition and are re-exported — extern re-exports are the user's contract); defined
//! wasm structs/enums are themselves `#[wasm_bindgen]` classes.
//!
//! **What it does NOT check.** Semantic wrongness — an identity `.into()` where a transform was
//! needed — stays `wasm_matrix_roundtrips`' job (this gate is a *presence* differential, parse-only).
//! It also scopes to `src/generated/mod.rs`: `serialization.rs`/`error.rs`/`cbor_encodings.rs`/
//! `ordered_hash_map.rs` are trait impls + runtime plumbing (`CBORReadLen`, encoding structs, …),
//! not per-type boundary API. A key-set guard fails loudly if a future multi-file emission mode grows
//! the generated dir, so the differential can't silently escape.
//!
//! **Inputs, profiles & cost.** Every `tests/matrix_wasm/*.cddl` cell (the wasm-ABI shape × role
//! grid — even `WASM_MATRIX_SKIP` ones, whose emitted sources still *parse* even when they don't
//! standalone *compile*) plus the two depth fixtures `tests/core/input.cddl` and `example/test.cddl`
//! (kitchen-sink shapes the minimal cells don't reach). Each of those inputs is swept across
//! `super::ALL_PROFILES` minus the component row (so: default / preserve / json —
//! `--preserve-encodings` and the json flags substantially change the rust surface, whereas
//! `--component` changes neither side of this boundary; see `parity_profiles`). A second corpus axis
//! sweeps every committed
//! `tests/*/input.cddl` fixture directory under that directory's committed generation profile rows
//! (the `run_test`/pipeline invocations in `integration_tests.rs`), with a completeness guard that
//! fails when a new fixture dir is not either added to the table or deliberately excluded. Exclusions
//! are narrow: `tests/core` is already swept as a depth fixture across every profile, and
//! `tests/wasm-list-macro` emits wasm members as user-macro invocations invisible to a `syn`
//! presence differential. Directory-input fixtures (`tests/multifile/inputs`,
//! `tests/extern-deps*/inputs`) are out of this axis by design: multifile emission writes per-module
//! files under `src/generated/`, outside this differential's `mod.rs`-only parse scope; that surface
//! is owned by the separate multifile placement sweep. Generation is in-process via
//! `api::generated_strings` (`Cli::parse_from`, wrapped in `catch_unwind`) — no subprocess, no
//! scratch dirs, no cargo check/test of the generated crates. Always-on (no `#[ignore]`), so it joins
//! the plain `cargo test` / check.ts local tier.
//!
//! **Ledger + anti-rot (the `WASM_MATRIX_SKIP` idiom).** `PARITY_EXEMPT` holds deliberately-accepted
//! asymmetries by `(profile, input, "Type" | "Type::member", reason)`. A finding matching a ledger
//! entry is expected (no failure); a ledger entry matching NO live finding fails as "resurfaced" (a
//! fix landed, or the rust member is gone — remove the entry); an unexempted finding fails with the
//! remedy spelled out (fix the emitter, or — deliberately — ledger it with a reason).

use std::collections::{BTreeMap, BTreeSet};
use std::panic::AssertUnwindSafe;
use std::path::PathBuf;

use crate::cli::Cli;
use clap::Parser;

/// Deliberately-accepted rust→wasm asymmetries: `(profile, input label, "Type" | "Type::member",
/// reason)`. Most legitimate asymmetry classes are baked into the correspondence rules above rather
/// than listed here (see the module header). A live finding not covered by an entry fails the gate;
/// an entry with no matching live finding fails as "resurfaced".
///
/// The current entries are the top-level `any`-alias class: a rule `top_alias = any` lowers to a
/// bare `pub type TopAlias = AnyCbor`, and wasm_bindgen exports no type aliases, so the rule name is
/// JS-invisible and the value is handled through the generator's `AnyCbor` wrapper class directly.
/// This is the accepted posture (the `AnyCbor` wrapper IS the JS surface; a
/// distinct per-alias JS class follows demand) and the same alias-only class as any `x = <wrapper>`
/// top-level alias. Every OTHER `any` position (member, array, table, tagged, choice arm) carries a
/// real AnyCbor / AnyList / MapAnyTo* / enum-variant wasm counterpart and needs no exemption.
const PARITY_EXEMPT: &[(&str, &str, &str, &str)] = &[
    (
        "default",
        "tests/any-positions",
        "TopAlias",
        "top-level `any` alias -> `pub type TopAlias = AnyCbor` (no wasm type-alias export); use the `AnyCbor` class",
    ),
    (
        "preserve",
        "tests/any-positions",
        "TopAlias",
        "top-level `any` alias -> `pub type TopAlias = AnyCbor` (no wasm type-alias export); use the `AnyCbor` class",
    ),
    (
        "json",
        "tests/any-positions",
        "TopAlias",
        "top-level `any` alias -> `pub type TopAlias = AnyCbor` (no wasm type-alias export); use the `AnyCbor` class",
    ),
    // The SAME class, on the component face's alias-of-`any` fixture: its two rules exist to make the
    // member reach `any` through a CHAIN, and each link is a top-level alias of exactly the shape
    // above. The chain is the point — one link would not distinguish a walk that resolves one from a
    // walk that resolves all — so both links are ledgered rather than the fixture being reshaped to
    // dodge a known-accepted class.
    (
        "default",
        "tests/component-any-alias",
        "ShallowAny",
        "top-level `any` alias -> `pub type ShallowAny = AnyCbor` (no wasm type-alias export); use the `AnyCbor` class",
    ),
    (
        "default",
        "tests/component-any-alias",
        "DeepAny",
        "top-level `any` alias chained onto another -> `pub type DeepAny = AnyCbor` (no wasm type-alias export); use the `AnyCbor` class",
    ),
];

/// `(profile, input label, reason)` pairs whose generation deliberately aborts. Four-state verdict
/// with a resurfaced guard: a listed pair that now generates fails ("gap closed — remove the pin");
/// an unlisted abort fails as a normal generation failure.
const EXPECTED_GENERATION_FAIL: &[(&str, &str, &str)] = &[];

/// `tests/<dir>` fixture dirs swept by the corpus axis: (dir, per-dir committed profile rows).
/// Each row is (profile label, flags) — the flag set the dir's integration gate commits to
/// (run_test invocations in integration_tests.rs, or package_json_pipeline for package-json), minus
/// flags irrelevant to the emitted `src/generated` surface (see per-entry comments). `--wasm=true`
/// is always forced by the harness.
type CorpusParityProfile = (&'static str, &'static [&'static str]);
type CorpusParityInput = (&'static str, &'static [CorpusParityProfile]);

const CORPUS_PARITY_INPUTS: &[CorpusParityInput] = &[
    (
        // The component face's phase-1 fixture. Ordinary CDDL with no component-only construct, so
        // it is swept here on the same terms as every other input; the component gates over it live
        // in `component_tests`.
        "component-core",
        &[("default", &[])],
    ),
    (
        // The component face's phase-2 CHOICE fixture. Ordinary CDDL (a type choice, a group choice
        // and a despecialized arm) carrying no component-only construct, so it is swept here on the
        // same terms as `component-core`; the component gates over it live in `component_tests`.
        "component-choices",
        &[("default", &[])],
    ),
    (
        // The component face's phase-2 VALUE-WINDOW fixture. Every row is ordinary bounded CDDL —
        // `.le`, a float window, `.size`, a bounded array/map, `[+ T]`, `@duplicates reject` — so it
        // is swept here on the same terms as `component-core`; the component gates over it live in
        // `component_tests`.
        "component-bounds",
        &[("default", &[])],
    ),
    (
        // The component face's `@name` remedy fixture. The `@name` directives are what keep it
        // generating under `--component`; they are plain comment-DSL rows on every other face.
        "component-rename",
        &[("default", &[])],
    ),
    (
        // The component face's alias-of-`any` fixture. Plain CDDL — two transparent aliases and a
        // record — carrying no component-only construct, so it is swept here on the same terms as
        // `component-core`; the component gates over it live in `component_tests`. Its two alias
        // rules are the top-level `any`-alias class the ledger above already documents.
        "component-any-alias",
        &[("default", &[])],
    ),
    (
        // The component face's WIT ident-hazard fixture. The hazard is a WIT-side fact only: on the
        // wasm face `t` is an ordinary rule with an ordinary wrapper, which is exactly why this row
        // is worth having — it pins that the component-face exclusion took nothing off this face.
        "component-ident-hazard",
        &[("default", &[])],
    ),
    (
        // The component whole-table custom-pair fixture. Its nominal map owner is ordinary CDDL and
        // has a real wasm wrapper surface, so sweep the committed default profile here; the separate
        // component build-smoke supplies the hand-written codec definitions and owns guest compile.
        "component-custom-table",
        &[("default", &[])],
    ),
    (
        "canonical",
        &[(
            "canonical",
            &[
                "--preserve-encodings=true",
                "--canonical-form=true",
                // Drop `--emit-tests=true`: generated test-module emission is not boundary API.
            ],
        )],
    ),
    (
        "comment-dsl",
        &[("preserve", &["--preserve-encodings=true"])],
    ),
    (
        "golden_hex",
        &[(
            "default",
            &[
                // Its integration gate passes `--wasm=false` because it needs no wasm build; this
                // parity question forces wasm on.
            ],
        )],
    ),
    (
        "golden_hex_preserve",
        &[("preserve", &["--preserve-encodings=true"])],
    ),
    (
        "golden_hex_canonical",
        &[(
            "canonical",
            &["--preserve-encodings=true", "--canonical-form=true"],
        )],
    ),
    (
        "json",
        &[
            (
                "json",
                &["--json-serde-derives=true", "--json-schema-export=true"],
            ),
            (
                "json_preserve",
                &[
                    "--preserve-encodings=true",
                    "--json-serde-derives=true",
                    "--json-schema-export=true",
                ],
            ),
        ],
    ),
    (
        "json-arbitrary-precision",
        &[("json_serde", &["--json-serde-derives=true"])],
    ),
    (
        "json-float",
        &[(
            "json",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        )],
    ),
    (
        // The fixture's integration gate enables both JSON outputs; parity therefore exercises the
        // identical generated wasm surface rather than an uncommitted default-profile variant.
        "zero-permitting-map",
        &[(
            "json",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        )],
    ),
    (
        // The generated wasm test executes decoded-parent insertion under this exact non-canonical
        // preserve profile; parity keeps the source-surface sweep aligned with that committed ABI.
        "wasm-open-rest-mutation",
        &[("preserve", &["--preserve-encodings=true"])],
    ),
    ("nullable-wasm", &[("default", &[])]),
    (
        "package-json",
        &[(
            "json",
            &[
                "--json-serde-derives=true",
                "--json-schema-export=true",
                // Drop `--package-json=true`: it is a packaging/layout flag. Keeping it moves the
                // generated crates under `rust/{rust,wasm}` and changes generated-file keys, while
                // the per-type `src/generated` surface is identical under the json flags above.
            ],
        )],
    ),
    (
        "preserve-encodings",
        &[("preserve", &["--preserve-encodings=true"])],
    ),
    ("raw-bytes", &[("default", &[])]),
    (
        "raw-bytes-preserve",
        &[("preserve", &["--preserve-encodings=true"])],
    ),
    (
        // A raw-bytes marker reached through a nested list is the positive reachability control for
        // own-spec wasm extern glue. Its committed regression generates the ordinary default wasm
        // surface, so the parity axis must keep the marker's rust type paired with that live re-export.
        "wasm-extern-reexport-reachability",
        &[("default", &[])],
    ),
    ("rust-wasm-split", &[("default", &[])]),
    (
        "wasm_json",
        &[("json_serde", &["--json-serde-derives=true"])],
    ),
    // Loose-CBOR `any` fixtures: full-surface citizens (the wasm AnyCbor wrapper class), so they
    // join the parity sweep. The differential parses `mod.rs` only (AnyCbor's rich rust-only runtime
    // internals live in the allowed `any_cbor.rs`), and every per-spec type carries its AnyCbor /
    // AnyList / MapAnyTo* wasm counterpart.
    (
        "any-positions",
        &[
            ("default", &[]),
            ("preserve", &["--preserve-encodings=true"]),
            (
                "json",
                &["--json-serde-derives=true", "--json-schema-export=true"],
            ),
        ],
    ),
    (
        "any-choice",
        &[("preserve", &["--preserve-encodings=true"])],
    ),
    ("any-shadow", &[("default", &[])]),
    // Loose-CBOR open struct-maps: full-surface citizens via the wasm rest
    // accessor (a getter returning the captured entries as the wasm map wrapper). The differential
    // parses `mod.rs` only and validates that each rest field's rust member carries its
    // `MapKToV`/PairMap-backed wasm counterpart.
    (
        "open-struct-map",
        &[
            ("default", &[]),
            ("preserve", &["--preserve-encodings=true"]),
            (
                "json",
                &["--json-serde-derives=true", "--json-schema-export=true"],
            ),
        ],
    ),
    // TYPED rest-row key domains: same rest-accessor question as `open-struct-map` above, asked of a
    // key type that is a rust struct (a union, a sized-int newtype, a `bytes` vector) rather than a
    // primitive — the differential validates that each rest field's rust member still carries its
    // `MapKToV`/`PairMapKToV` counterpart, and that the `<K>List` keys mint follows `K`. Swept under
    // `default`, `preserve` and `json`. Every profile on this axis carries wasm ON (it is
    // a wasm differential), so the `json` row is the same flag combination the fixture's `wasm_json`
    // snapshot profile pins — that one pins the emitted text, this one the rust→wasm correspondence.
    (
        "open-struct-map-typed",
        &[
            ("default", &[]),
            ("preserve", &["--preserve-encodings=true"]),
            (
                "json",
                &["--json-serde-derives=true", "--json-schema-export=true"],
            ),
        ],
    ),
    // open ARRAYS: each capture rule's loose, NonEmpty, or Bounded `rest` carrier must carry its
    // `rest()` wasm list-wrapper getter (`TList`/`AnyList`/bounded list wrapper). Swept under `default` and `json` (the two
    // profiles the snapshot fixture commits): the fixture mixes capture and `@ignore` rules, and
    // `@ignore` is rejected under --preserve-encodings, so a preserve row can't generate the whole
    // file — and the getter's wasm surface is profile-invariant (per-element encodings are rust-only),
    // so default+json fully cover it; preserve byte-fidelity is covered by open-array-preserve-e2e.
    (
        "open-array",
        &[
            ("default", &[]),
            (
                "json",
                &["--json-serde-derives=true", "--json-schema-export=true"],
            ),
        ],
    ),
    // The e2e fixture is the only committed corpus input that places loose, NonEmpty and bounded
    // dynamic MAP rows on one wasm-visible owner.  Keep its preserve/canonical profile (the same
    // semantic face its execution vectors use) but turn wasm ON for this differential: the typed
    // row's flattened no-getter provenance and each catch-all/rest getter are distinct seams.
    (
        "open-table-e2e",
        &[(
            "preserve_canonical",
            &["--preserve-encodings=true", "--canonical-form=true"],
        )],
    ),
    // NOMINAL references to a collection typedef (a rule cycle entered at the collection rule). Its
    // two integration gates pass `--wasm=false` — they assert wire vectors, not a wasm build — so
    // this axis is where the wasm side of those references gets differentialled at all. Both
    // committed profiles are swept: the preserve one is the only place the encoding-sidecar path
    // exists, and that path is per-member, so it can move the wasm surface independently.
    (
        "recursive-collection-ref",
        &[
            ("default", &[]),
            ("preserve", &["--preserve-encodings=true"]),
        ],
    ),
];

const CORPUS_PARITY_EXCLUDED: &[(&str, &str)] = &[
    (
        "core",
        "already swept as a depth fixture across ALL_PROFILES",
    ),
    (
        "wasm-list-macro",
        "committed flags are --wasm-list-macro/--wasm-conversions-macro: the wasm members are \
         emitted as user-macro invocations, invisible to a syn presence differential (same class \
         as the wasm-mint macro loud-skip)",
    ),
    (
        "used-as-key-flavor",
        "rust-only compile-fail fixture (a `@used_as_key ord` root over an extern lacking Ord): its \
         integration gate generates with --wasm=false and injects a hand-written extern, so there is \
         no wasm surface to differential and the crate is intended NOT to compile",
    ),
    (
        "custom-codec-coherence-e2e",
        "rust-only custom-codec coherence fixture: its three integration profiles generate with \
         --wasm=false and inject profile-specific hand-written Rust codec signatures, while wasm \
         wrappers delegate to Rust and do not consume those functions",
    ),
    (
        "extern-generic-raw-bytes",
        "rust-only `@raw_bytes_flavor` fixture (an extern generic instantiated with a raw-bytes \
         element): its integration gate generates with --wasm=false and injects hand-written extern \
         wrappers + a raw-bytes impl, so there is no generated wasm surface to differential",
    ),
    (
        "json-extern",
        "rust-only json-gen extern-row regression fixture (a plain extern + a generic extern \
         instance under the json flags): its integration gate generates with --wasm=false and \
         injects hand-written extern defs, so there is no generated wasm surface to differential",
    ),
    (
        "json-schema-name-merge",
        "rust-only NEGATIVE json-gen fixture (a generic extern whose hand-written `JsonSchema` impl \
         returns a constant name, instantiated twice): its integration gate generates --wasm=false \
         and asserts the json-gen run FAILS on the schema-name ledger, so there is no wasm surface \
         to differential and the crate is intended NOT to produce a document",
    ),
    (
        "json-schema-name-stolen",
        "rust-only NEGATIVE json-gen fixture (a row-less extern claiming a later row's schema name): \
         its integration gate generates --wasm=false and asserts the json-gen run FAILS on the \
         kept-its-own-name check, so there is no wasm surface to differential",
    ),
    (
        "json-ref-dangling",
        "rust-only NEGATIVE json-gen fixture (externs whose hand-written `JsonSchema` impls return \
         references the document does not define): its integration gate generates --wasm=false and \
         asserts the json-gen run FAILS on the document's reference-closure check, so there is no \
         wasm surface to differential and the crate is intended NOT to produce a document",
    ),
    (
        "emit-tests-bounded-key",
        "rust-only bounded-table-key `--emit-tests` e2e fixture: its integration gate \
         (`emit_tests_bounded_map_key_execute`) generates --wasm=false and `cargo test`s the rust \
         crate, because what it proves is that a MINTED map key is a value the emitted decoder \
         ACCEPTS — a property of the shared `MintValue` derivation, decided before either renderer \
         runs (the wasm renderer consumes the same `key_base` through the shared `map_key_expr` / \
         `map_key_literal`), so a wasm leg would re-assert the same key bases against a second API \
         for no added signal. There is no generated wasm surface here to differential",
    ),
    (
        "open-struct-map-e2e",
        "loose-CBOR open struct-map (rest row) e2e round-trip fixture: its integration gate \
         generates --wasm=false (it exercises CBOR round-trip, not the wasm boundary); the wasm rest \
         surface is validated by the `open-struct-map` snapshot fixture's parity rows above",
    ),
    (
        "open-array-e2e",
        "open array (rest tail) e2e round-trip fixture: its integration gate generates \
         --wasm=false (it exercises CBOR round-trip, not the wasm boundary); the bounded/loose wasm rest getter is \
         validated by the `open-array` snapshot fixture's parity rows above",
    ),
    (
        "open-array-preserve-e2e",
        "open array (rest tail) PRESERVE fidelity e2e fixture: its integration gate \
         generates --preserve-encodings --canonical-form --wasm=false (CBOR fidelity, not the wasm \
         boundary); the wasm rest getter is validated by the `open-array` snapshot fixture's rows",
    ),
    (
        "open-array-json-e2e",
        "open array (rest tail) JSON e2e fixture: its integration gate generates \
         --json-serde-derives --json-schema-export --wasm=false (it exercises the JSON boundary); the \
         wasm rest getter is validated by the `open-array` snapshot fixture's parity rows",
    ),
    (
        "open-struct-map-preserve-e2e",
        "loose-CBOR open struct-map PRESERVE fidelity e2e fixture: its integration gate generates \
         --preserve-encodings --canonical-form --wasm=false (CBOR fidelity, not the wasm boundary); \
         the wasm rest surface is validated by the `open-struct-map` snapshot fixture's parity rows \
         above",
    ),
    (
        "custom-serialize-canonical-e2e",
        "custom-serialize CANONICAL scratch-buffer e2e fixture: its integration gate generates \
         --preserve-encodings --canonical-form --wasm=false (the call form of a free-function \
         custom serializer inside a rust scratch `Serializer`, not the wasm boundary). Its custom \
         targets are hand-written RUST free functions appended into the generated rust scope, and a \
         wasm wrapper never calls them — it delegates to the rust type's impl — so there is no wasm \
         surface here to differential; the `dsl_custom` corpus fixture snapshots the directive's \
         wasm-side emission",
    ),
    (
        "custom-encodings-e2e",
        "@custom_encodings (a custom codec declaring its own wire's encoding variables) e2e fixture: \
         its integration gate generates --preserve-encodings --canonical-form --wasm=false (the \
         declaration drives CBOR encoding variables, which have no wasm surface at all — a wasm \
         wrapper delegates to the rust type's impl and never calls a custom codec). Its custom \
         targets are hand-written RUST free functions appended into the generated rust scope, the \
         same shape as the `custom-serialize-canonical-e2e` row above",
    ),
    (
        "alias-of-marker-e2e",
        "alias-of-marker custom-pair e2e fixture (a type-level pair on an alias of a \
         `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule): its integration gate generates \
         --preserve-encodings --canonical-form --wasm=false and injects BOTH a hand-written \
         raw-bytes extern definition and hand-written RUST custom codecs into the generated rust \
         scope, so there is no generated wasm surface to differential — the same shape as the two \
         custom-serialization rows above, doubled by the raw-bytes extern's own hand definition \
         (the `extern-generic-raw-bytes` row's reason)",
    ),
    (
        "open-table-cip25-acceptance",
        "the open-table series' ACCEPTANCE fixture (CIP-25 spelled with generated open tables at all \
         four payload levels, measured against a real on-chain golden): its integration gate \
         generates --preserve-encodings --canonical-form --wasm=false and injects BOTH hand-written \
         raw-bytes extern definitions and hand-written RUST custom codecs into the generated rust \
         scope, so there is no generated wasm surface to differential — the `alias-of-marker-e2e` \
         row's reason, with two marker types instead of one. The open table's own wasm surface is \
         covered by the `open-struct-map` snapshot fixture's parity rows and by the wasm-ABI \
         matrices' `otbl__*` cells",
    ),
    (
        "custom-pair-shared-codec",
        "rust-only compile-fail fixture (ONE custom codec pair reached from a record field AND a \
         table key, whose encoding argument the two positions pass by reference and by value): its \
         integration gate generates --preserve-encodings --wasm=false, injects a hand-written codec \
         with the record-field signature, and asserts the generated crate does NOT compile — so \
         there is no wasm surface to differential and the crate is intended not to build (the \
         `used-as-key-flavor` row's reason)",
    ),
    (
        "open-struct-map-json-e2e",
        "loose-CBOR open struct-map FLATTENED-JSON e2e fixture: its integration gate generates \
         --json-serde-derives --json-schema-export --wasm=false (it exercises the JSON boundary); \
         the `open-struct-map` snapshot fixture's parity rows cover the wasm rest surface",
    ),
    (
        "open-table-json-e2e",
        "open table (one typed table row plus one trailing typed catch-all rest row) FLATTENED-JSON \
         e2e fixture: its integration gate generates --json-serde-derives --json-schema-export \
         --wasm=false (it exercises the JSON boundary — the hand-written serde pair's typed-first \
         partition and cross-region collision check — not the wasm boundary); the wasm surface of \
         the two rows is two container-class getters, minted by the same path the `open-struct-map` \
         snapshot fixture's parity rows cover",
    ),
    (
        "open-struct-map-ignore",
        "loose-CBOR open struct-map IGNORE flavor (`@ignore`) snapshot fixture: its snapshot profile \
         generates --wasm=false. The ignore flavor tolerate-and-drops unknown entries, so it emits a \
         CLOSED struct with NO rest field and NO wasm rest surface (its wasm class is an ordinary \
         closed struct's) — there is no rest accessor to differential, and closed-struct wasm parity \
         is covered by the capture `open-struct-map` snapshot fixture's rows above",
    ),
    (
        "open-struct-map-ignore-e2e",
        "loose-CBOR open struct-map IGNORE flavor (`@ignore`) e2e round-trip fixture: its integration \
         gate generates --json-serde-derives --wasm=false (it exercises CBOR/JSON round-trip, not the \
         wasm boundary). The ignore flavor emits a CLOSED struct with no wasm rest surface, so there \
         is nothing to differential",
    ),
];

/// Only these `.rs` basenames may appear under `rust/src/generated/` (default/json profiles); only
/// these under `wasm/src/generated/`. A file outside these sets means a new emission surface the
/// differential doesn't parse — fail with "extend wasm_api_parity" rather than silently skip it.
/// `serialization.rs`/`error.rs` are deliberately out of scope (runtime plumbing, not per-type
/// boundary API); `collections.rs` is the wasm wrapper re-export index — a `pub use` inventory of
/// classes defined in `mod.rs`, so it introduces no new boundary API for the differential to parse.
/// `key_demand_assertions.rs` (any `@used_as_key`-tagged root) holds private compile-time-only
/// `_demand_*` self-checks — zero pub items, so nothing for the differential to parse either.
/// `extern_interface_check.rs` (emitted unconditionally — the dep-side extern-interface self-check)
/// is likewise private compile-time-only assertions, no boundary API.
const ALLOWED_RUST_GENERATED: &[&str] = &[
    "mod.rs",
    "serialization.rs",
    "error.rs",
    "key_demand_assertions.rs",
    "extern_interface_check.rs",
    // The AnyCbor runtime module (CDDL `any`). A runtime type like serialization/ordered_hash_map,
    // not per-spec-type surface, so the differential does not parse it — but it must be an ALLOWED
    // key so a fixture using `any` does not trip the stray-file guard.
    "any_cbor.rs",
    // The json-gen helper module (the row `Registrar` + the reference-closure check) this crate HOSTS under
    // `--json-schema-export` for the `wasm/json-gen` crates that import it. This crate never calls
    // it and it carries no per-type boundary API, so the differential does not parse it — but it
    // must be an ALLOWED key or every json-profile fixture trips the stray-file guard.
    "json_schema_gen.rs",
    // The honest `serde_json::Value`/`Number` serializer walk, emitted under `--json-serde-derives`
    // (flag-gated, never spec-gated — it is a published API for hand-written `Serialize` impls). Pure
    // runtime helpers, no per-type boundary API, so the differential does not parse it — but it must
    // be an ALLOWED key or every json-profile fixture trips the stray-file guard.
    "json_value_ser.rs",
];
const ALLOWED_WASM_GENERATED: &[&str] = &["mod.rs", "collections.rs"];

/// The rust crate's public API surface, parsed from `rust/src/generated/mod.rs`.
#[derive(Default)]
struct RustSurface {
    /// `pub struct` / `pub enum` names.
    types: BTreeSet<String>,
    /// type -> its `pub` named fields as `field name -> inner type ident` (structs only; enums have
    /// no top-level named fields). The inner ident unwraps one `Option<..>` so the preserve
    /// encoding-capture exemption (rule 3) can recognise `pub encodings: Option<XEncoding>`.
    fields: BTreeMap<String, BTreeMap<String, Option<String>>>,
    /// type -> inherent `pub fn`s as (name, self-excluded arity).
    inherent_fns: BTreeMap<String, BTreeSet<(String, usize)>>,
    /// `pub type` alias names.
    type_aliases: BTreeSet<String>,
    /// The subset of `type_aliases` whose rustdoc carries `SYNTHESIZED_INSTANCE_ALIAS_DOC` — a
    /// generator-synthesized anonymous generic-collection/table INSTANCE alias (no CDDL rule name).
    /// Rules 2 and 5 skip these: their rust→wasm asymmetry (the synthesized name is JS-invisible,
    /// the shape crosses as its inline equivalent's structural class) is legitimate and documented
    /// (`docs/docs/wasm_differences.mdx`), and provenance — not a source-shape heuristic — is what
    /// tells them apart from a real rule alias (see the const's doc in `generation/mod.rs`).
    synthesized_instance_aliases: BTreeSet<String>,
    /// `(type, field)` pairs whose rustdoc carries either open-table TYPED-row marker — loose
    /// `OPEN_TABLE_TYPED_ROW_DOC`, min-one `OPEN_TABLE_NON_EMPTY_TYPED_ROW_DOC`, or bounded
    /// `OPEN_TABLE_BOUNDED_TYPED_ROW_DOC`. Rule 3 skips
    /// these: the wasm class FLATTENS this row's map surface onto itself (`insert`/`get`/`len`/`keys`)
    /// instead of emitting a whole-map getter, so the missing getter is the design rather than an
    /// omission. Structural like the encoding-capture carve-out beside it, and provenance-driven
    /// like the synthesized-alias one above — a shape heuristic ("a `pub` map field on a fieldless
    /// struct") would also cover the CATCH-ALL row, which DOES owe its getter.
    flattened_typed_rows: BTreeSet<(String, String)>,
}

/// The wasm crate's public API surface, parsed from `wasm/src/generated/mod.rs`.
#[derive(Default)]
struct WasmSurface {
    /// `pub struct` / `pub enum` DEFINED here (member rules run only against these).
    defined_types: BTreeSet<String>,
    /// `pub use` re-export leaf idents.
    reexports: BTreeSet<String>,
    /// `pub type` alias name -> its TARGET's leaf ident (last path segment, `None` for non-path
    /// targets like tuples). Public visibility only — a PRIVATE alias does not satisfy rule 2. The
    /// target drives rule 5 (JS-name visibility): an alias-only counterpart whose target is a
    /// wasm-defined struct/enum with a generator-invented name is JS-invisible.
    pub_type_aliases: BTreeMap<String, Option<String>>,
    /// type -> inherent `pub fn`s as (name, self-excluded arity).
    members: BTreeMap<String, BTreeSet<(String, usize)>>,
}

fn is_pub(vis: &syn::Visibility) -> bool {
    matches!(vis, syn::Visibility::Public(_))
}

/// Whether any `#[doc = "…"]` attribute's string CONTAINS `needle` — the provenance-marker read for
/// synthesized-instance aliases. Substring (not equality): the generator may prepend the marker line
/// to further mechanical doc lines joined with `\n` into one `#[doc]` value.
fn doc_contains(attrs: &[syn::Attribute], needle: &str) -> bool {
    attrs.iter().any(|attr| {
        attr.path().is_ident("doc")
            && matches!(
                &attr.meta,
                syn::Meta::NameValue(nv)
                    if matches!(
                        &nv.value,
                        syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. })
                            if s.value().contains(needle)
                    )
            )
    })
}

/// Last path segment ident of a `Type::Path`, if any (`None` for tuples, references, …). Used both
/// for the type an `impl` block is *for* and for a `pub type` alias's TARGET. Unlike
/// `type_inner_ident`, this does NOT unwrap `Option<..>`: `Option<TaggedText>` reports `Option`, so a
/// transparent-alias target (`pub type OptText = Option<TaggedText>;`) resolves to the std `Option`,
/// not the wasm-defined inner — exactly what rule 5's "target not wasm-defined" carve-out needs.
fn type_leaf_ident(ty: &syn::Type) -> Option<String> {
    match ty {
        syn::Type::Path(p) => p.path.segments.last().map(|s| s.ident.to_string()),
        _ => None,
    }
}

/// The "inner" type ident of a field: the last path segment, unwrapping a single `Option<..>`
/// layer so `Option<XEncoding>` reports `XEncoding` (the preserve encoding-capture exemption keys
/// off this). Returns `None` for non-path types (tuples, references, …).
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
                    let mut flattened = Vec::new();
                    let entry = s.fields.entry(name.clone()).or_default();
                    for f in &named.named {
                        if is_pub(&f.vis)
                            && let Some(id) = &f.ident
                        {
                            entry.insert(id.to_string(), type_inner_ident(&f.ty));
                            if doc_contains(&f.attrs, crate::generation::OPEN_TABLE_TYPED_ROW_DOC)
                                || doc_contains(
                                    &f.attrs,
                                    crate::generation::OPEN_TABLE_NON_EMPTY_TYPED_ROW_DOC,
                                )
                                || doc_contains(
                                    &f.attrs,
                                    crate::generation::OPEN_TABLE_BOUNDED_TYPED_ROW_DOC,
                                )
                            {
                                flattened.push((name.clone(), id.to_string()));
                            }
                        }
                    }
                    s.flattened_typed_rows.extend(flattened);
                }
            }
            syn::Item::Enum(en) if is_pub(&en.vis) => {
                s.types.insert(en.ident.to_string());
            }
            syn::Item::Type(ty) if is_pub(&ty.vis) => {
                let name = ty.ident.to_string();
                if doc_contains(&ty.attrs, crate::generation::SYNTHESIZED_INSTANCE_ALIAS_DOC) {
                    s.synthesized_instance_aliases.insert(name.clone());
                }
                s.type_aliases.insert(name);
            }
            syn::Item::Impl(im) if im.trait_.is_none() => {
                if let Some(ty) = type_leaf_ident(&im.self_ty) {
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
                    s.pub_type_aliases
                        .insert(ty.ident.to_string(), type_leaf_ident(&ty.ty));
                }
            }
            syn::Item::Use(u) if is_pub(&u.vis) => {
                collect_use_leaves(&u.tree, &mut s.reexports);
            }
            syn::Item::Impl(im) if im.trait_.is_none() => {
                if let Some(ty) = type_leaf_ident(&im.self_ty) {
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

/// Pub struct names defined in the emitted `cbor_encodings.rs` (the `*Encoding` set the preserve
/// encoding-capture exemption keys off). Empty for profiles that don't emit the file.
fn parse_encoding_structs(src: &str) -> BTreeSet<String> {
    let file = syn::parse_file(src).expect("generated cbor_encodings.rs must parse");
    let mut out = BTreeSet::new();
    for item in &file.items {
        if let syn::Item::Struct(st) = item
            && is_pub(&st.vis)
        {
            out.insert(st.ident.to_string());
        }
    }
    out
}

/// A single rust→wasm parity gap. `item` is `"Type"` (rules 1–2) or `"Type::member"` (rules 3–4).
struct Finding {
    profile: String,
    label: String,
    item: String,
    msg: String,
}

/// Run the four correspondence rules for one input's parsed surfaces, appending any gaps.
/// `encoding_structs` are the pub structs defined in the emitted `cbor_encodings.rs` (preserve
/// profile); a rust pub field of type `Option<X>`/`X` with `X` in that set is exempt from rule 3.
fn diff_surfaces(
    profile: &str,
    label: &str,
    rust: &RustSurface,
    wasm: &WasmSurface,
    encoding_structs: &BTreeSet<String>,
    out: &mut Vec<Finding>,
) {
    // A rust struct/enum has a wasm counterpart if a wasm struct/enum is defined, a `pub use`
    // re-exports it, or a PUBLIC `pub type` aliases it.
    let wasm_has_type = |name: &str| {
        wasm.defined_types.contains(name)
            || wasm.reexports.contains(name)
            || wasm.pub_type_aliases.contains_key(name)
    };

    // Rule 1: every rust pub struct/enum has a wasm counterpart.
    for t in &rust.types {
        if !wasm_has_type(t) {
            out.push(Finding {
                profile: profile.to_string(),
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
    // EXCEPT a SYNTHESIZED anonymous generic-collection instance alias (doc-marked): an exposable
    // instance (`gcoll<uint>` → `pub type GcollU64 = Vec<u64>`) is inlined to a bare `Vec` on the
    // wasm side (no counterpart alias), which is the documented lowering, not a missing type.
    for a in &rust.type_aliases {
        if rust.synthesized_instance_aliases.contains(a) {
            continue;
        }
        if !wasm_has_type(a) {
            out.push(Finding {
                profile: profile.to_string(),
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

        // Rule 3: every rust pub field `f` on `T` has a wasm inherent getter `f` on `T`, EXCEPT
        // encoding-capture fields (`pub encodings: Option<XEncoding>` under preserve), which are
        // rust-only round-trip metadata defined in `cbor_encodings.rs` — no wasm boundary member —
        // and an open table's TYPED row, whose map surface the wasm class FLATTENS onto itself
        // (recognised by either field provenance marker, loose or NonEmpty typed row).
        if let Some(fields) = rust.fields.get(t) {
            for (f, inner) in fields {
                if let Some(inner_ident) = inner
                    && encoding_structs.contains(inner_ident)
                {
                    continue;
                }
                if rust.flattened_typed_rows.contains(&(t.clone(), f.clone())) {
                    continue;
                }
                if !wasm_names.contains(f.as_str()) {
                    out.push(Finding {
                        profile: profile.to_string(),
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
                        profile: profile.to_string(),
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

    // Rule 5 (JS-name visibility): rules 1–2 accept a PUBLIC `pub type` alias as a rust type's wasm
    // counterpart, but that is rust-source-level parity only — wasm_bindgen exports NO type aliases,
    // so an alias-only counterpart means the CDDL rule name never reaches JS. For every rust-surface
    // name (rust pub struct/enum ∪ rust `pub type` alias) whose ONLY wasm counterpart is a `pub type`
    // alias (not a defined struct/enum, not a `pub use` re-export), resolve the alias's target leaf
    // ident and flag iff (a) the target is a struct/enum DEFINED in the wasm mod (a real
    // `#[wasm_bindgen]` class) AND (b) that target name is NOT itself on the rust surface. The
    // carve-outs (both "not a finding") are structural:
    //   - target NOT wasm-defined (primitive/std/`Option`/…, e.g. `pub type U8 = u8;`,
    //     `pub type OptText = Option<TaggedText>;`, `pub type ParenCbor = String;`): the
    //     transparent-alias design — JS represents the value natively, no class exists for the shape;
    //     documented in `docs/docs/wasm_differences.mdx`.
    //   - target wasm-defined AND on the rust surface (e.g. `pub type FooBytes = Foo;`, `Foo` a rust
    //     pub struct): a pure CDDL-level alias present identically on both sides — the JS class carries
    //     a genuine CDDL rule name.
    // What remains — alias to a wasm-defined type whose name is generator-invented (`MapU64ToText`,
    // not on the rust surface) — is the usage-dependent-JS-class-name bug: the CDDL rule name is
    // JS-invisible and the shape's JS class name flips with unrelated spec content
    // (`cddl-matrix/roadmap.toml` § findings). `pub use` counterparts stay JS-visible by design
    // (c-style enums carry `#[wasm_bindgen]` at their definition and are re-exported — the user's
    // contract), and defined wasm structs/enums are themselves `#[wasm_bindgen]` classes.
    let rust_surface: BTreeSet<&str> = rust
        .types
        .iter()
        .chain(rust.type_aliases.iter())
        .map(String::as_str)
        .collect();
    for name in rust_surface.iter().copied() {
        // A SYNTHESIZED anonymous generic-collection/table instance alias (doc-marked): its
        // JS-invisibility is BY DESIGN — the user wrote an anonymous instance, which crosses as its
        // inline equivalent's structural class (`FooList` / `MapU64ToText`), never a rule name at
        // stake, so rule 5's "the CDDL rule name is JS-invisible" premise is vacuous for it. The
        // discriminator is provenance (the marker), not shape: a sole-owner named-table alias
        // (`pub type Mp = MapU64ToText;`) is a bare-collection alias too and must STAY gated.
        if rust.synthesized_instance_aliases.contains(name) {
            continue;
        }
        // Alias-only counterpart: satisfied by a wasm `pub type` alias, and NOT by a defined
        // struct/enum or a `pub use` re-export (those are JS-visible classes / the user's contract).
        if wasm.defined_types.contains(name) || wasm.reexports.contains(name) {
            continue;
        }
        let Some(target) = wasm.pub_type_aliases.get(name) else {
            continue; // no wasm counterpart at all — rule 1/2 already flagged it
        };
        let Some(target_ident) = target else {
            continue; // (a) non-path target (tuple, reference, …): no wasm-defined class
        };
        if !wasm.defined_types.contains(target_ident) {
            continue; // (a) transparent alias to a primitive/std/Option type — native in JS
        }
        if rust_surface.contains(target_ident.as_str()) {
            continue; // (b) target is itself a rust-surface CDDL rule name — JS class is genuine
        }
        out.push(Finding {
            profile: profile.to_string(),
            label: label.to_string(),
            item: name.to_string(),
            msg: format!(
                "rust surface name reaches JS only as the generator-invented `{target_ident}` \
                 class: its wasm counterpart is an alias-only `pub type {name} = {target_ident};` \
                 (wasm_bindgen exports no type aliases), so the CDDL rule name is JS-invisible and \
                 the shape's JS class name is usage-dependent — emit the wrapper under the rule \
                 name, or ledger it"
            ),
        });
    }
}

/// Collect the `.rs` basenames under `prefix` in the generated-files map that fall outside
/// `allowed`, so a future multi-file emission mode can't silently escape the differential.
fn stray_keys(files: &BTreeMap<String, String>, prefix: &str, allowed: &[&str]) -> Vec<String> {
    let mut stray = vec![];
    for k in files.keys() {
        if let Some(rest) = k.strip_prefix(prefix) {
            let base = rest.rsplit('/').next().unwrap_or(rest);
            if base.ends_with(".rs") && !allowed.contains(&base) {
                stray.push(base.to_string());
            }
        }
    }
    stray.sort();
    stray.dedup();
    stray
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

fn corpus_input_dirs_on_disk() -> BTreeSet<String> {
    std::fs::read_dir("tests")
        .unwrap()
        .flatten()
        .filter_map(|e| {
            let path = e.path();
            if path.join("input.cddl").is_file() {
                Some(e.file_name().to_str().unwrap().to_string())
            } else {
                None
            }
        })
        .collect()
}

fn assert_corpus_axis_complete() {
    let disk = corpus_input_dirs_on_disk();
    let included: BTreeSet<&str> = CORPUS_PARITY_INPUTS.iter().map(|(dir, _)| *dir).collect();
    let excluded: BTreeSet<&str> = CORPUS_PARITY_EXCLUDED.iter().map(|(dir, _)| *dir).collect();

    let overlap: Vec<&str> = included.intersection(&excluded).copied().collect();
    assert!(
        overlap.is_empty(),
        "corpus parity dirs cannot be both included and excluded: {overlap:?}"
    );

    let missing_exclusions: Vec<&str> = excluded
        .iter()
        .copied()
        .filter(|dir| !disk.contains(*dir))
        .collect();
    assert!(
        missing_exclusions.is_empty(),
        "CORPUS_PARITY_EXCLUDED names dir(s) with no tests/<dir>/input.cddl: {missing_exclusions:?}"
    );

    let expected: BTreeSet<String> = included
        .iter()
        .chain(excluded.iter())
        .map(|dir| (*dir).to_string())
        .collect();
    assert_eq!(
        disk, expected,
        "tests/*/input.cddl corpus parity coverage changed — add the new dir to \
         CORPUS_PARITY_INPUTS with its committed flags, or exclude it with a reason"
    );
}

fn total_corpus_profile_rows() -> usize {
    CORPUS_PARITY_INPUTS
        .iter()
        .map(|(_, rows)| rows.len())
        .sum()
}

fn flags_enable_preserve(flags: &[&str]) -> bool {
    flags.contains(&"--preserve-encodings=true")
}

/// The [`super::ALL_PROFILES`] rows this sweep visits. The component profile is filtered out BY
/// NAME: this differential's subject is the rust↔wasm boundary, and `--component` leaves both sides
/// byte-identical (it mints a separate wasip2 crate and changes nothing else), so a component column
/// would re-derive the `default` column's verdict — and would need a fourth `PARITY_EXEMPT` row for
/// every exemption it duplicated. The rust↔WIT differential is `component_parity_tests`' job.
///
/// Used by BOTH the sweep and its vacuity arithmetic below, so the two cannot disagree about how
/// many profiles a swept input contributes.
fn parity_profiles() -> Vec<&'static super::Profile> {
    super::ALL_PROFILES
        .iter()
        .filter(|(profile, _)| *profile != super::COMPONENT_PROFILE)
        .collect()
}

/// The sweep's (label, input, profile, flags) axis product, built once so the pin guards and the
/// shards agree on exactly what "a swept pair" means.
fn parity_sweep_cases() -> Vec<(String, PathBuf, &'static str, &'static [&'static str])> {
    let inputs = parity_inputs();
    let mut sweep_cases: Vec<(String, PathBuf, &'static str, &'static [&'static str])> = vec![];
    for (label, input) in &inputs {
        for (profile, extra) in parity_profiles() {
            sweep_cases.push((label.clone(), input.clone(), *profile, *extra));
        }
    }
    for (dir, rows) in CORPUS_PARITY_INPUTS {
        for (profile, extra) in *rows {
            sweep_cases.push((
                format!("tests/{dir}"),
                PathBuf::from(format!("tests/{dir}/input.cddl")),
                *profile,
                *extra,
            ));
        }
    }
    sweep_cases
}

/// Every whole-axis assertion of the parity sweep, kept in ONE test because each is only correct
/// when a single test sees the WHOLE axis product: a shard walking a slice cannot distinguish "this
/// pin names a pair that was deleted" from "this pin names a pair another shard owns", so splitting
/// them across shards would leave them vacuous while the suite stayed green. It builds the axis and
/// reads the corpus directory — no generation — so keeping it whole costs milliseconds.
///
/// The `PARITY_EXEMPT` axis guard is NEW with the shard split and is what keeps the ledger's
/// resurfaced-check honest: each shard reconciles only the exempt entries whose (profile, label) is
/// one of ITS OWN cases, so an entry naming a pair no shard sweeps would be reconciled by nobody.
/// Asserting here that every entry names a swept pair closes that hole by construction.
#[test]
fn wasm_api_parity_axes_and_pins_are_live() {
    assert_corpus_axis_complete();
    let sweep_cases = parity_sweep_cases();

    // A pin naming a (profile, input) pair the sweep never visits would rot silently (its two-way
    // guard only fires on visited pairs) — validate every pin against the live axes up front.
    let swept_profile_labels: BTreeSet<&str> = sweep_cases.iter().map(|(_, _, p, _)| *p).collect();
    let swept_input_labels: BTreeSet<&str> =
        sweep_cases.iter().map(|(l, _, _, _)| l.as_str()).collect();
    for (p, l, _) in EXPECTED_GENERATION_FAIL {
        assert!(
            swept_profile_labels.contains(p),
            "EXPECTED_GENERATION_FAIL pin names unknown profile `{p}` — stale pin, remove or fix it"
        );
        assert!(
            swept_input_labels.contains(l),
            "EXPECTED_GENERATION_FAIL pin names input `{l}` that is no longer swept — stale pin, \
             remove or fix it"
        );
    }
    let swept_pairs: BTreeSet<(&str, &str)> = sweep_cases
        .iter()
        .map(|(l, _, p, _)| (*p, l.as_str()))
        .collect();
    for (p, l, i, _) in PARITY_EXEMPT {
        assert!(
            swept_pairs.contains(&(*p, *l)),
            "PARITY_EXEMPT entry ({p}, {l}, {i}) names a (profile, input) pair the sweep never \
             visits — no shard would ever reconcile it, so its resurfaced-check is vacuous; stale \
             pin, remove or fix it"
        );
    }

    // Vacuity guard: every (input, profile) pair must be in the product. A filter bug that shrinks
    // the input set (or a dropped profile) fails here rather than passing a hollow sweep.
    let cell_count = std::fs::read_dir("tests/matrix_wasm")
        .unwrap()
        .flatten()
        .filter(|e| e.path().extension().and_then(|x| x.to_str()) == Some("cddl"))
        .count();
    assert_eq!(
        parity_inputs().len(),
        cell_count + 2,
        "input enumeration drifted from (matrix_wasm cells + 2 depth fixtures)"
    );
    // The multiplier is `parity_profiles()` — the rows the sweep actually visits — NOT
    // `ALL_PROFILES`: the two differ by the filtered component row, and using the wrong one here
    // would fail this assertion on a sweep that is doing exactly what it should.
    assert_eq!(
        sweep_cases.len(),
        (cell_count + 2) * parity_profiles().len() + total_corpus_profile_rows(),
        "sweep shrank: expected every (input, profile) pair to be visited"
    );
}

/// How many `#[test]`s the parity sweep is split across. The sweep is pure in-process generation
/// (`api::generated_strings`) plus string parsing — no cargo, no scratch dir, no lock — so its cells
/// are the one kind in this suite that libtest's 32-thread pool can genuinely absorb; the count is
/// sized so a shard lands well under the ~63 s the rest of the suite takes.
const PARITY_SHARDS: usize = 12;

macro_rules! wasm_api_parity_shards {
    ($($name:ident = $shard:expr;)+) => {
        $(
            #[test]
            fn $name() {
                wasm_api_parity_shard($shard);
            }
        )+
    };
}

wasm_api_parity_shards! {
    wasm_api_parity_shard_00 = 0;
    wasm_api_parity_shard_01 = 1;
    wasm_api_parity_shard_02 = 2;
    wasm_api_parity_shard_03 = 3;
    wasm_api_parity_shard_04 = 4;
    wasm_api_parity_shard_05 = 5;
    wasm_api_parity_shard_06 = 6;
    wasm_api_parity_shard_07 = 7;
    wasm_api_parity_shard_08 = 8;
    wasm_api_parity_shard_09 = 9;
    wasm_api_parity_shard_10 = 10;
    wasm_api_parity_shard_11 = 11;
}

/// One slice of [`parity_sweep_cases`]. Named `wasm_api_parity_shard_NN` so every substring selector
/// that named the old single test still selects the whole sweep.
///
/// **Reporting contract.** The unsharded test accumulated four batches and asserted once at the end,
/// so one failure named every problem. Each shard keeps that batching for ITS OWN cases, and libtest
/// reports every failing shard in a run — so a run still surfaces every problem, grouped by shard
/// rather than in one list. The whole-axis assertions moved to
/// [`wasm_api_parity_axes_and_pins_are_live`]; only the ledger reconciliation, which is decidable
/// per case, stayed here.
fn wasm_api_parity_shard(shard: usize) {
    let all_cases = parity_sweep_cases();
    // Round-robin over the axis product, which `parity_sweep_cases` builds in a deterministic order,
    // so which case lands in which shard is reproducible from the fixtures alone.
    let sweep_cases: Vec<&(String, PathBuf, &'static str, &'static [&'static str])> = all_cases
        .iter()
        .enumerate()
        .filter(|(i, _)| i % PARITY_SHARDS == shard)
        .map(|(_, c)| c)
        .collect();
    // The ledger half this shard owns: an exempt entry is "resurfaced" when no live finding matches
    // it, and findings for a (profile, label) can only come from the shard sweeping that pair — so
    // the reconciliation is decidable per shard once the entries are partitioned the same way.
    // `wasm_api_parity_axes_and_pins_are_live` asserts every entry names a swept pair, so no entry
    // falls between the shards.
    let mine: BTreeSet<(&str, &str)> = sweep_cases
        .iter()
        .map(|(l, _, p, _)| (*p, l.as_str()))
        .collect();
    let shard_exempt: Vec<&(&str, &str, &str, &str)> = PARITY_EXEMPT
        .iter()
        .filter(|(p, l, _, _)| mine.contains(&(*p, *l)))
        .collect();

    let mut findings: Vec<Finding> = vec![];
    let mut strays: Vec<String> = vec![]; // new emission surface the differential doesn't parse
    let mut gen_failures: Vec<String> = vec![]; // unlisted generation aborts (real regressions)
    let mut gap_closed: Vec<String> = vec![]; // EXPECTED_GENERATION_FAIL that now generates

    for (label, input, profile, extra) in &sweep_cases {
        let input_str = input.to_str().unwrap();
        let expected_fail = EXPECTED_GENERATION_FAIL
            .iter()
            .any(|(p, l, _)| p == profile && l == label);

        // In-process generation: build the Cli via clap and run the `#[cfg(test)]`
        // `generated_strings` producer, guarded against the float `unimplemented!` panic.
        let mut args = vec![
            "cddl-codegen",
            "--input",
            input_str,
            "--output",
            "wasm_api_parity_unused",
            "--wasm=true",
        ];
        args.extend(extra.iter().copied());
        let cli = Cli::parse_from(args);
        // Keep the abort detail (error string / panic payload) so an unexpected generation failure
        // reports its cause, not just its coordinates.
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
            (true, Err(_)) => continue, // expected abort — nothing to diff
            (true, Ok(_)) => {
                gap_closed.push(format!("({profile}, {label})"));
                continue;
            }
            (false, Err(detail)) => {
                gen_failures.push(format!("{profile}/{label} ({input:?}): {detail}"));
                continue;
            }
            (false, Ok(files)) => files,
        };

        // Key-set guard: preserve additionally emits cbor_encodings.rs / ordered_hash_map.rs.
        let allowed_rust: Vec<&str> = if flags_enable_preserve(extra) {
            ALLOWED_RUST_GENERATED
                .iter()
                .copied()
                .chain(["cbor_encodings.rs", "ordered_hash_map.rs"])
                .collect()
        } else {
            ALLOWED_RUST_GENERATED.to_vec()
        };
        for base in stray_keys(&files, "rust/src/generated/", &allowed_rust) {
            strays.push(format!("{profile}/{label} rust: {base}"));
        }
        for base in stray_keys(&files, "wasm/src/generated/", ALLOWED_WASM_GENERATED) {
            strays.push(format!("{profile}/{label} wasm: {base}"));
        }

        let rust_src = files
            .get("rust/src/generated/mod.rs")
            .unwrap_or_else(|| panic!("{profile}/{label}: no rust/src/generated/mod.rs"));
        let wasm_src = files.get("wasm/src/generated/mod.rs").unwrap_or_else(|| {
            panic!("{profile}/{label}: no wasm/src/generated/mod.rs (expected a wasm crate)")
        });
        let encoding_structs = files
            .get("rust/src/generated/cbor_encodings.rs")
            .map(|s| parse_encoding_structs(s))
            .unwrap_or_default();

        let rust = parse_rust_surface(rust_src);
        let wasm = parse_wasm_surface(wasm_src);
        diff_surfaces(
            profile,
            label,
            &rust,
            &wasm,
            &encoding_structs,
            &mut findings,
        );
    }

    // Structural guards (the emission surface / generation-abort verdicts) before the parity diff.
    assert!(
        strays.is_empty(),
        "unexpected file(s) under a generated dir — a new emission surface the parity differential \
         doesn't parse; extend wasm_api_parity to cover it:\n{}",
        strays.join("\n")
    );
    assert!(
        gap_closed.is_empty(),
        "these EXPECTED_GENERATION_FAIL pins now generate — the gap closed; remove them:\n{}",
        gap_closed.join("\n")
    );
    assert!(
        gen_failures.is_empty(),
        "generation failed for these (profile, input) pairs (a regression, or — if a genuine new \
         gap — pin it in EXPECTED_GENERATION_FAIL with a reason):\n{}",
        gen_failures.join("\n")
    );

    // Reconcile findings against the ledger (the `WASM_MATRIX_SKIP` idiom). The exempt SET is the
    // whole ledger — an exemption must silence its finding whichever shard raised it — while the
    // resurfaced scan is over this shard's slice of the ledger only.
    let exempt: BTreeSet<(&str, &str, &str)> = PARITY_EXEMPT
        .iter()
        .map(|(p, l, i, _)| (*p, *l, *i))
        .collect();
    let live: BTreeSet<(&str, &str, &str)> = findings
        .iter()
        .map(|f| (f.profile.as_str(), f.label.as_str(), f.item.as_str()))
        .collect();

    let unexempted: Vec<&Finding> = findings
        .iter()
        .filter(|f| !exempt.contains(&(f.profile.as_str(), f.label.as_str(), f.item.as_str())))
        .collect();
    let resurfaced: Vec<&&(&str, &str, &str, &str)> = shard_exempt
        .iter()
        .filter(|(p, l, i, _)| !live.contains(&(*p, *l, *i)))
        .collect();

    assert!(
        resurfaced.is_empty(),
        "these PARITY_EXEMPT entries no longer match any live finding — a fix landed (or the rust \
         member is gone); remove them from the ledger:\n{}",
        resurfaced
            .iter()
            .map(|(p, l, i, r)| format!("  ({p}, {l}, {i}) — was: {r}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    assert!(
        unexempted.is_empty(),
        "rust→wasm API-surface parity gaps (fix the emitter, or — deliberately — add a \
         PARITY_EXEMPT entry with a reason):\n{}",
        unexempted
            .iter()
            .map(|f| format!("  [{}/{}] {}: {}", f.profile, f.label, f.item, f.msg))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

/// The synthesized-instance-alias provenance MARKER (the discriminator rules 2 & 5 read to skip a
/// legitimate anonymous-collection-instance asymmetry) must land on generator-synthesized instance
/// aliases ONLY, never on a user rule alias. If the marker silently stopped being emitted, the whole
/// anonymous-instance class would re-red the parity gate confusingly (63 findings) with no code
/// change to point at; this pins the emission so that regression is loud and local. `GcollU64` is an
/// anonymous inline `gcoll<uint>` instance (marked); `Gcn` is the user rule `gcn = gcoll<foo>`
/// (unmarked — a named instance becomes its rule alias directly, minting no separate marked ident).
#[test]
fn synthesized_instance_alias_marker_provenance() {
    const CDDL: &str = "foo = [a0: uint]\n\
                        gcoll<e0> = [* e0]\n\
                        gcn = gcoll<foo>\n\
                        holder = [a: gcoll<uint>, b: gcn]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_synth_marker_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "synth_marker_unused",
        "--wasm=true",
    ]))
    .expect("fixture must generate under --wasm");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    let marker = crate::generation::SYNTHESIZED_INSTANCE_ALIAS_DOC;
    // marked: the anonymous `gcoll<uint>` instance alias
    assert!(
        src.contains(&format!("/// {marker}\npub type GcollU64")),
        "the synthesized anonymous-instance alias `GcollU64` must carry the provenance marker, got:\n{src}"
    );
    // NOT marked: the user rule alias `gcn` (and it must NOT mint a separate marked `GcollFoo`)
    assert!(
        !src.contains(&format!("/// {marker}\npub type Gcn")),
        "the user rule alias `Gcn` must NOT carry the synthesized-instance marker, got:\n{src}"
    );
    assert!(
        !src.contains("pub type GcollFoo"),
        "a NAMED instance rule (`gcn = gcoll<foo>`) becomes its rule alias directly — no separate \
         `GcollFoo` alias should be minted, got:\n{src}"
    );
}

// -------------------------------------------------------------------------------------------------
// The wasm face's flag-conditional door vocabulary
// -------------------------------------------------------------------------------------------------

/// Two rules that own the door (a record and a map-rep record — both are rust types the wasm class
/// wraps, so `create_base_wasm_struct` sees `exists_in_rust`), plus a named collection whose wasm
/// class is built with `exists_in_rust = false` and therefore owes no door member in ANY posture.
/// `door_ambiguous` owns a rust wrapper but intentionally cannot derive `Deserialize`, so its
/// output-side set pins the `from_cbor_bytes` predicate rather than merely asking the contract.
/// The collection is the control: without it a sweep that only ever asserts presence could pass
/// while the emitter handed the door to everything.
const DOOR_SPEC: &str = "\
door_record = [a: uint, b: tstr]\n\
door_map = {c: uint}\n\
door_ambiguous = [? b: uint, c: uint]\n\
door_list = [* door_record]\n";

/// Every parsed inherent public method of `class` in emitted wasm source.
fn inherent_members_of(wasm: &WasmSurface, class: &str) -> BTreeSet<String> {
    wasm.members
        .get(class)
        .map(|members| members.iter().map(|(name, _)| name.clone()).collect())
        .unwrap_or_default()
}

/// The output-observed door is the exact set of methods a posture adds over its matched
/// door-disabled baseline. This deliberately knows NO door names: a future method pushed directly
/// in `create_base_wasm_struct` appears in the difference and fails unless the production contract
/// also owns it.
fn observed_door_of(wasm: &WasmSurface, baseline: &WasmSurface, class: &str) -> BTreeSet<String> {
    inherent_members_of(wasm, class)
        .difference(&inherent_members_of(baseline, class))
        .cloned()
        .collect()
}

/// The wasm face's door vocabulary must be exactly what the posture owes — in BOTH directions.
///
/// **Why a dedicated gate rather than a `wasm_api_parity` axis.** That differential is rust→wasm and
/// reads inherent rust impls only, a deliberate structural exemption (`From`/`AsRef`/`Serialize` are
/// never counted). Every door member's rust-side home is outside that walk — `to_cbor_bytes` /
/// `from_cbor_bytes` / `to_canonical_cbor_bytes` are trait methods on the generated runtime's
/// `ToCBORBytes` / `Serialize` / `Deserialize`, and the JSON three are backed by serde derives — so
/// a missing door member contributes no rust-side row and nothing can be reported absent. That is
/// exactly how `to_canonical_cbor_bytes` shipped missing: `create_base_wasm_struct` BUILT its
/// `codegen::Function` under `--preserve-encodings --canonical-form` and never pushed it onto the
/// impl, so the rust crate declared the method and the wasm boundary did not. The production door
/// contract now constructs a complete vector and the emitter pushes that vector structurally; this
/// test derives its expected vocabulary from that contract, then obtains the actual output door by
/// subtracting a matched door-disabled generated baseline from the parsed emitted wasm source. That
/// differential has no vocabulary filter, so an unregistered direct extra cannot hide.
///
/// **The reverse direction is not decoration.** A member emitted in a posture whose runtime does not
/// declare it is a compile error in the consumer's crate, not a harmless extra:
/// `to_canonical_cbor_bytes` lives on `Serialize`, which the runtime composes ONLY from
/// `static/serialization_preserve_force_canonical.rs` (every other posture composes a `ToCBORBytes`
/// declaring `to_cbor_bytes` alone). So each posture's emitted cbor door is checked against the
/// runtime prelude that same `Cli` composes — through `GenerationScope::serialization_prelude`, the
/// producer `export` itself calls, so the tested and shipped composition cannot drift. The component
/// face's sibling of this gate is
/// `component_wit_carries_the_canonical_seam_only_where_the_runtime_composes_it`.
///
/// **Scope: the non-macro path.** Under `--wasm-cbor-json-api-macro` the emitter pushes a macro
/// invocation (`my_macro!(Foo);`) and NO fns of its own, so the door vocabulary there is the macro
/// author's contract and not the generator's. The last case pins that branch's emptiness rather than
/// its content, which is what makes the six-member sweep above legitimately non-macro-only.
#[test]
fn wasm_door_vocabulary_matches_the_posture_that_owes_it() {
    // The postures exercise the cbor, canonical, JSON, and no-bytes capability gates. The owed
    // vocabulary is deliberately NOT listed here: it is derived from the production contract below.
    const CASES: &[(&str, &[&str])] = &[
        ("default", &[]),
        ("no_bytes_methods", &["--to-from-bytes-methods=false"]),
        ("json", &["--json-serde-derives=true"]),
        ("preserve", &["--preserve-encodings=true"]),
        (
            "preserve_canonical",
            &["--preserve-encodings=true", "--canonical-form=true"],
        ),
        (
            "preserve_canonical_json",
            &[
                "--preserve-encodings=true",
                "--canonical-form=true",
                "--json-serde-derives=true",
            ],
        ),
        (
            "canonical_no_bytes_methods",
            &[
                "--preserve-encodings=true",
                "--canonical-form=true",
                "--to-from-bytes-methods=false",
            ],
        ),
    ];
    let dir = std::env::temp_dir().join(format!("cddl_codegen_wasm_door_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("input.cddl");
    std::fs::write(&path, DOOR_SPEC).unwrap();
    let cli_for = |extra: &[&str]| {
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "wasm_door_unused",
            "--wasm=true",
        ];
        args.extend(extra.iter().copied());
        Cli::parse_from(args)
    };
    // Keep every posture coordinate but normalize the two door flags before appending their false
    // baseline values. Passing a conflicting duplicate to clap would test parser precedence, not
    // the same generation posture with its door disabled.
    let door_disabled_cli_for = |extra: &[&str]| {
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "wasm_door_unused",
            "--wasm=true",
        ];
        args.extend(extra.iter().copied().filter(|arg| {
            !arg.starts_with("--to-from-bytes-methods=")
                && !arg.starts_with("--json-serde-derives=")
        }));
        args.extend([
            "--to-from-bytes-methods=false",
            "--json-serde-derives=false",
        ]);
        Cli::parse_from(args)
    };

    let mut failures: Vec<String> = vec![];
    for (label, extra) in CASES {
        let cli = cli_for(extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("{label}: generating the door spec failed: {e}"));
        let wasm_src = files
            .get("wasm/src/generated/mod.rs")
            .unwrap_or_else(|| panic!("{label}: no wasm/src/generated/mod.rs"));
        let wasm = parse_wasm_surface(wasm_src);
        let baseline_cli = door_disabled_cli_for(extra);
        let baseline_files = crate::api::generated_strings(&baseline_cli).unwrap_or_else(|e| {
            panic!("{label}: generating the door-disabled baseline failed: {e}")
        });
        let baseline_src = baseline_files
            .get("wasm/src/generated/mod.rs")
            .unwrap_or_else(|| panic!("{label}: no baseline wasm/src/generated/mod.rs"));
        let baseline = parse_wasm_surface(baseline_src);
        // Both door-owning fixture rules generate `Deserialize`; the production contract makes that
        // predicate explicit, retaining the distinct `from_cbor_bytes` gate without another table.
        let owed = crate::generation::wasm_door_members(&cli, true)
            .into_iter()
            .map(crate::generation::WasmDoorMember::name)
            .map(str::to_owned)
            .collect::<BTreeSet<_>>();
        for class in ["DoorRecord", "DoorMap"] {
            let got = observed_door_of(&wasm, &baseline, class);
            if got != owed {
                failures.push(format!(
                    "  [{label}] {class}: door is {got:?}, the posture owes {owed:?} \
                     (missing {:?}, unowed {:?})",
                    owed.difference(&got).collect::<Vec<_>>(),
                    got.difference(&owed).collect::<Vec<_>>()
                ));
            }
        }
        let ambiguous_owed = crate::generation::wasm_door_members(&cli, false)
            .into_iter()
            .map(crate::generation::WasmDoorMember::name)
            .map(str::to_owned)
            .collect::<BTreeSet<_>>();
        let ambiguous = observed_door_of(&wasm, &baseline, "DoorAmbiguous");
        if ambiguous != ambiguous_owed {
            failures.push(format!(
                "  [{label}] DoorAmbiguous: door is {ambiguous:?}, the no-Deserialize posture \
                 owes {ambiguous_owed:?} (missing {:?}, unowed {:?})",
                ambiguous_owed.difference(&ambiguous).collect::<Vec<_>>(),
                ambiguous.difference(&ambiguous_owed).collect::<Vec<_>>(),
            ));
        }
        let control = observed_door_of(&wasm, &baseline, "DoorList");
        if !control.is_empty() {
            failures.push(format!(
                "  [{label}] DoorList: a collection wrapper is built with `exists_in_rust = false` \
                 and owes NO door member, got {control:?}"
            ));
        }
        // Reverse direction: every cbor door member the posture emits must be DECLARED by the
        // runtime that same posture composes. (The JSON three are backed by the serde derives, so
        // their runtime witness is the derive on the rust type, checked below.)
        let prelude = crate::generation::GenerationScope::serialization_prelude(false, false, &cli)
            .expect("the static runtime prelude must compose");
        for member in crate::generation::wasm_door_members(&cli, true)
            .into_iter()
            .filter(|member| member.is_cbor())
        {
            let emitted = owed.contains(member.name());
            let declared = prelude.contains(&format!("fn {}(", member.name()));
            if emitted && !declared {
                failures.push(format!(
                    "  [{label}] the door re-exports `{}`, which this posture's composed \
                     runtime does not declare — a compile error in the consumer's crate",
                    member.name()
                ));
            }
        }
        let rust_src = files
            .get("rust/src/generated/mod.rs")
            .unwrap_or_else(|| panic!("{label}: no rust/src/generated/mod.rs"));
        let serde_derived = rust_src.contains("serde::Serialize");
        if crate::generation::wasm_door_members(&cli, true)
            .into_iter()
            .any(|member| !member.is_cbor())
            && !serde_derived
        {
            failures.push(format!(
                "  [{label}] the door re-exports the JSON three over a rust type carrying no serde \
                 derive"
            ));
        }
    }

    // The macro branch is a different emission path: `create_base_wasm_struct` pushes the user's
    // macro invocation INSTEAD of any fn, so the door there is the macro author's contract. Pinned
    // as emptiness-plus-invocation so the sweep above stays honest about what it does not cover.
    let macro_cli = cli_for(&[
        "--preserve-encodings=true",
        "--canonical-form=true",
        "--wasm-cbor-json-api-macro=door_macro",
    ]);
    let macro_files =
        crate::api::generated_strings(&macro_cli).expect("the macro posture must generate");
    let macro_src = macro_files.get("wasm/src/generated/mod.rs").unwrap();
    let macro_wasm = parse_wasm_surface(macro_src);
    let macro_baseline_cli =
        door_disabled_cli_for(&["--preserve-encodings=true", "--canonical-form=true"]);
    let macro_baseline_files = crate::api::generated_strings(&macro_baseline_cli)
        .expect("the macro door-disabled baseline must generate");
    let macro_baseline_src = macro_baseline_files
        .get("wasm/src/generated/mod.rs")
        .expect("the macro baseline must have wasm/src/generated/mod.rs");
    let macro_baseline = parse_wasm_surface(macro_baseline_src);
    for class in ["DoorRecord", "DoorMap"] {
        let got = observed_door_of(&macro_wasm, &macro_baseline, class);
        if !got.is_empty() {
            failures.push(format!(
                "  [macro] {class}: the macro branch emitted door fns of its own ({got:?}) — the \
                 vocabulary sweep above no longer covers the whole emitter"
            ));
        }
        if !macro_src.contains(&format!("door_macro!({class});")) {
            failures.push(format!(
                "  [macro] {class}: no `door_macro!({class});` invocation — the macro branch stopped \
                 handing the door to the macro author"
            ));
        }
    }

    std::fs::remove_dir_all(&dir).ok();
    assert!(
        failures.is_empty(),
        "the wasm face's flag-conditional door vocabulary does not match the posture that owes it \
         (fix the production door contract or its runtime capability witness):\n{}",
        failures.join("\n")
    );
}
