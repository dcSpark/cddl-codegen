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
//! class, so its JS name flips with unrelated spec content (`cddl-matrix/ROADMAP.md` § findings).
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
//! `super::ALL_PROFILES` (default / preserve / json — `--preserve-encodings` and the json flags
//! substantially change the rust surface). A second corpus axis sweeps every committed
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
//! **Generation-fail pin (the `WASM_MATRIX_SKIP` idiom).** One `(profile, input)` pair aborts
//! generation — a CBOR tag over a type-choice under `--preserve-encodings` (the `assert!` class in
//! `generation/enums.rs`). It is pinned in `EXPECTED_GENERATION_FAIL` with a resurfaced guard both
//! directions: a listed pair that now generates fails ("gap closed — remove the pin"); an unlisted
//! abort fails as a normal generation failure.
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
];

/// `(profile, input label, reason)` pairs whose generation deliberately aborts. Four-state verdict
/// with a resurfaced guard: a listed pair that now generates fails ("gap closed — remove the pin");
/// an unlisted abort fails as a normal generation failure.
const EXPECTED_GENERATION_FAIL: &[(&str, &str, &str)] = &[(
    "preserve",
    "tests/core",
    "`tagged_type_choice = #6.11(uint / text)` — a CBOR tag over a type-choice hits the tagged-enum \
     serialize path's explicit `assert!(!cli.preserve_encodings)` in generation/enums.rs (the \
     per-variant encoding metadata has no home on the enum); the SAME class recombination_tests' \
     PRESERVE_ONLY_PANIC_CLASSES ledgers. Note this pin used to name the float member on this input, \
     which no longer aborts — floats carry their head width as an encoding variable",
)];

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
        // The component face's `@name` remedy fixture. The `@name` directives are what keep it
        // generating under `--component`; they are plain comment-DSL rows on every other face.
        "component-rename",
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
    // loose-CBOR open ARRAYS: each capture rule's rust `pub rest: Vec<T>` field must carry its
    // `rest()` wasm list-wrapper getter (`TList`/`AnyList`). Swept under `default` and `json` (the two
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
        "loose-CBOR open array (rest tail) e2e round-trip fixture: its integration gate generates \
         --wasm=false (it exercises CBOR round-trip, not the wasm boundary); the wasm rest getter is \
         validated by the `open-array` snapshot fixture's parity rows above",
    ),
    (
        "open-array-preserve-e2e",
        "loose-CBOR open array (rest tail) PRESERVE fidelity e2e fixture: its integration gate \
         generates --preserve-encodings --canonical-form --wasm=false (CBOR fidelity, not the wasm \
         boundary); the wasm rest getter is validated by the `open-array` snapshot fixture's rows",
    ),
    (
        "open-array-json-e2e",
        "loose-CBOR open array (rest tail) JSON e2e fixture: its integration gate generates \
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
        "open-struct-map-json-e2e",
        "loose-CBOR open struct-map FLATTENED-JSON e2e fixture: its integration gate generates \
         --json-serde-derives --json-schema-export --wasm=false (it exercises the JSON boundary); \
         the `open-struct-map` snapshot fixture's parity rows cover the wasm rest surface",
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
        // rust-only round-trip metadata defined in `cbor_encodings.rs` — no wasm boundary member.
        if let Some(fields) = rust.fields.get(t) {
            for (f, inner) in fields {
                if let Some(inner_ident) = inner
                    && encoding_structs.contains(inner_ident)
                {
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
    // (`cddl-matrix/ROADMAP.md` § findings). `pub use` counterparts stay JS-visible by design
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

/// The sweep's (label, input, profile, flags) axis product, built once so the pin guards and the
/// shards agree on exactly what "a swept pair" means.
fn parity_sweep_cases() -> Vec<(String, PathBuf, &'static str, &'static [&'static str])> {
    let inputs = parity_inputs();
    let mut sweep_cases: Vec<(String, PathBuf, &'static str, &'static [&'static str])> = vec![];
    for (label, input) in &inputs {
        for (profile, extra) in super::ALL_PROFILES {
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
    assert_eq!(
        sweep_cases.len(),
        (cell_count + 2) * super::ALL_PROFILES.len() + total_corpus_profile_rows(),
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
