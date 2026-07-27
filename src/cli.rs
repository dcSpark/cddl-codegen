use clap::Parser;
// TODO: make non-annotation generate different DeserializeError that is simpler
//       and works with From<cbor_event:Error> only

/// clap value parser for `--rust-wasm-feature`: a cargo feature name is non-empty and restricted to
/// cargo's feature-name charset (`[A-Za-z0-9_+.-]`), so the name can be written into `[features]` and
/// a `cfg(feature = "…")` gate verbatim. Rejects anything else with a message naming the bad char.
fn parse_rust_wasm_feature(s: &str) -> Result<String, String> {
    if s.is_empty() {
        return Err("must be a non-empty cargo feature name".to_owned());
    }
    if let Some(c) = s
        .chars()
        .find(|c| !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '+' | '.' | '-'))
    {
        return Err(format!(
            "invalid character {c:?}; cargo feature names use only [A-Za-z0-9_+.-]"
        ));
    }
    Ok(s.to_owned())
}

/// clap value parser for `--json-schema-root`. What the flag takes is a rust TYPE PATH — a name,
/// optionally with generic arguments — and not an arbitrary type expression. That boundary is forced
/// by the emission: the value goes VERBATIM into generated rust inside a turbofish
/// (`reg.add::<VALUE>();`), so the accepted characters are the ones a type
/// path needs and nothing that could introduce a comment, a statement separator, or a string literal
/// into a generated file. `[A-Za-z0-9_]`, `:`, `<`, `>`, `,` and space cover a scoped path with
/// nested generics and a qualified `<Foo as Trait>::Assoc`; they cannot express an array
/// (`[u8; 32]`), a tuple, or a reference — and the array case is the clearest reason the two goals
/// are not jointly satisfiable, since `;` IS the statement separator the guard exists to exclude. A
/// user who needs one of those spells it as a named alias in their own crate and registers that.
///
/// It deliberately does NOT check that the path RESOLVES: cddl-codegen does not typecheck Rust, so
/// an unresolvable path is an `E0433`/`E0412` in the consumer's own json-gen build, never a
/// generation-time reject — and a stricter grammar would reject legitimate spellings (a leading
/// `::`, generic arguments, spaces after commas).
fn parse_json_schema_root(s: &str) -> Result<String, String> {
    if s.is_empty() {
        return Err("must be a non-empty rust type path".to_owned());
    }
    if let Some(c) = s.chars().find(
        |c| !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | ':' | '<' | '>' | ',' | ' '),
    ) {
        return Err(format!(
            "invalid character {c:?}; --json-schema-root takes a rust TYPE PATH (generic arguments \
             allowed, e.g. `my_crate::sub::Ext<u64, String>`), not an arbitrary type expression: the \
             value is emitted verbatim into generated rust as `reg.add::<PATH>()`, so only \
             [A-Za-z0-9_], `:`, `<`, `>`, `,` and spaces can be accepted — every other character \
             could introduce a comment, a statement separator, or a string literal into a generated \
             file. For an array/tuple/reference type, give it a named alias in your crate and \
             register that"
        ));
    }
    Ok(s.to_owned())
}

/// clap value parser for `--json-schema-dep`, whose value is `<dep>=<dep-json-gen-lib-name>`.
///
/// The two sides answer to different rules on purpose. The LEFT side is a pure LABEL: cddl-codegen
/// never resolves it against the extern-dep set, because the emitted line depends only on the right
/// side, so a wrong label is inert rather than wrong. Its jobs are duplicate detection, error
/// messages, and keeping the line readable next to the `--extern-wasm-crate=<dep>=<dep>_wasm` line
/// for that same dependency. (Same class as `--wrapper-requests` / `--key-requests`, whose
/// `<consumer>` is likewise a label; `--workspace-dep` validates its `<dep>` because there the name
/// DRIVES behaviour.) So the only requirement is non-empty.
///
/// The RIGHT side lands VERBATIM in generated rust as a call path
/// (`<rhs>::add_schemas(generator);`), so it carries exactly the injection argument
/// `parse_json_schema_root` carries. Accepted: `[A-Za-z0-9_]`, `:` (a module path — a dep's
/// registrar may be reached as `cml_chain_json_schema_gen` or as `crate::vendored`) and `-`
/// (normalised to `_` by `Cli::json_schema_deps`, mirroring `extern_wasm_crate_map`, so a cargo
/// package name works verbatim). NOT accepted: `<`, `>`, `,` and space — a module path has no
/// generic arguments — and everything else, since any other character could introduce a comment, a
/// statement separator, or a string literal into a generated file.
fn parse_json_schema_dep(s: &str) -> Result<String, String> {
    let (dep, lib) = s.split_once('=').ok_or_else(|| {
        format!("--json-schema-dep value must be <dep>=<dep_json_gen_lib_name>, got: {s:?}")
    })?;
    let dep = dep.trim();
    let lib = lib.trim();
    if dep.is_empty() || lib.is_empty() {
        return Err(format!(
            "--json-schema-dep value must be <dep>=<dep_json_gen_lib_name> with both sides \
             non-empty, got: {s:?}"
        ));
    }
    if let Some(c) = lib
        .chars()
        .find(|c| !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | ':' | '-'))
    {
        return Err(format!(
            "invalid character {c:?}; the <dep_json_gen_lib_name> side of --json-schema-dep takes a \
             rust MODULE PATH (a crate name, or a path to a re-export — e.g. \
             `cml_chain_json_schema_gen` or `crate::vendored`), not a type and not an arbitrary \
             expression: the value is emitted verbatim into generated rust as \
             `PATH::add_schemas(generator);`, so only [A-Za-z0-9_], `:` and `-` (normalised to `_`, \
             so a cargo package name works verbatim) can be accepted — a module path has no generic \
             arguments, and every other character could introduce a comment, a statement separator, \
             or a string literal into a generated file"
        ));
    }
    Ok(s.to_owned())
}

/// clap value parser for `--json-gen-dep`, whose value is `<cargo-package-name>=<path>`.
///
/// Unlike every sibling in this file, NEITHER side lands in generated rust — both land in a TOML
/// manifest, as `<name> = { path = "<path>" }` under `[dependencies]` of `wasm/json-gen/Cargo.toml`.
/// That changes what each side has to guard against:
///
/// * The LEFT side is a **cargo package name**, so it carries cargo's package-name charset
///   (`[A-Za-z0-9_-]`). It is also a TOML key and a `[dependencies]` path segment, and restricting it
///   here is what keeps it from having to be quoted or escaped anywhere downstream. Note the
///   direction: this is the dashed PACKAGE name (`cml-chain-json-schema-gen`), the opposite side of
///   the `-`/`_` normalisation from `--json-schema-dep`'s right-hand side, which is the underscored
///   rust LIB path (`cml_chain_json_schema_gen`).
/// * The RIGHT side is a filesystem path with no charset restriction: it is written through
///   `toml_edit`, which quotes and escapes it, so it cannot inject structure into the manifest the
///   way a verbatim-into-rust value could. The only requirement is non-empty.
fn parse_json_gen_dep(s: &str) -> Result<String, String> {
    let (name, path) = s.split_once('=').ok_or_else(|| {
        format!("--json-gen-dep value must be <cargo_package_name>=<path>, got: {s:?}")
    })?;
    let name = name.trim();
    let path = path.trim();
    if name.is_empty() || path.is_empty() {
        return Err(format!(
            "--json-gen-dep value must be <cargo_package_name>=<path> with both sides non-empty, \
             got: {s:?}"
        ));
    }
    if let Some(c) = name
        .chars()
        .find(|c| !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '-'))
    {
        return Err(format!(
            "invalid character {c:?}; the <cargo_package_name> side of --json-gen-dep is a CARGO \
             PACKAGE NAME (e.g. `cml-chain-json-schema-gen`), which uses only [A-Za-z0-9_-] — it \
             becomes a `[dependencies]` key in the generated `wasm/json-gen/Cargo.toml`. Note this \
             is the DASHED package name, not the underscored rust lib path \
             `--json-schema-dep` takes on its right-hand side"
        ));
    }
    Ok(s.to_owned())
}

/// The flags below describe ONE generated crate. A project that generates several — the shape that
/// makes the flag lists long and mostly-repeated — can instead put them in a config file:
/// `cddl-codegen --config <file.toml> [CRATE...]`, where every flag here is a key. That mode is
/// mutually exclusive with these flags (see `docs/docs/config_file.mdx`), which is why `--config` is
/// not itself listed among them; this note is how it stays discoverable from `--help`.
const CONFIG_MODE_HELP: &str = "Multi-crate projects: `cddl-codegen --config <file.toml> [CRATE...]` \
                                takes every flag above as a key in a TOML file, with shared values \
                                declared once. Paths in it resolve against the config file rather \
                                than the current directory. `--config` cannot be combined with the \
                                flags above.";

#[derive(Debug, Default, Parser)]
#[clap(after_help = CONFIG_MODE_HELP, after_long_help = CONFIG_MODE_HELP)]
pub struct Cli {
    /// Input .cddl file to generate from. If this is a directory then it will read all *.cddl files and generate one output for each.
    #[clap(short, long, value_parser, value_name = "INPUT_FILE/INPUT_DIR")]
    pub input: std::path::PathBuf,

    /// Output directory for the generated code.
    #[clap(short, long, value_parser, value_name = "OUTPUT_DIR")]
    pub output: std::path::PathBuf,

    /// Change the directory of the static files
    #[clap(short, long, value_parser, value_name = "STATIC_DIR", default_value_os_t = std::path::PathBuf::from("static"))]
    pub static_dir: std::path::PathBuf,

    /// Name to use for exported library.
    /// Will be used directly for rust lib and will have -wasm appended for the wasm bindings.
    /// This will appear EXACTLY as-is in the Cargo.toml's. use Cli::lib_name_code() for use in rust code
    #[clap(
        long,
        value_parser,
        value_name = "EXPORT_LIB_NAME",
        default_value = "cddl-lib"
    )]
    pub lib_name: String,

    /// Include additional information about where deserialization errors are encountered. This will slightly increase code size.
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = true)]
    pub annotate_fields: bool,

    /// Generate to_bytes() / from_bytes() methods on all types
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = true)]
    pub to_from_bytes_methods: bool,

    /// Generate byte string definitions as new rust types (TODO: look into this or remove it)
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub binary_wrappers: bool,

    /// Preserves CBOR encoding upon deserialization e.g. definite vs indefinite, map ordering
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub preserve_encodings: bool,

    /// Allows serialization to canonical CBOR. if preserve-encodings is enabled, this will be as a toggle on serialization functions
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub canonical_form: bool,

    /// Generates a wasm_bindgen crate for wasm bindings
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = true)]
    pub wasm: bool,

    /// Name of the cargo feature the generated RUST crate's `#[wasm_bindgen]` attribute (emitted
    /// only on c-style enums under `--wasm`) is gated behind, so the rust crate compiles standalone
    /// without the optional `wasm-bindgen` dependency. The generated wasm crate enables this feature
    /// via its path dependency on the rust crate. Must be a valid cargo feature name (non-empty,
    /// chars in `[A-Za-z0-9_+.-]`). Defaults to `wasm`.
    // NB: the derived `Default` yields `""` here — clap's `default_value` applies only to parsed
    // invocations (same quirk as `wasm: bool`, whose parse default is true) — so a test built with
    // `..Default::default()` must set this explicitly if it also sets `wasm: true`.
    #[clap(long, value_parser = parse_rust_wasm_feature, default_value = "wasm")]
    pub rust_wasm_feature: String,

    /// Derives serde::Serialize/serde::Deserialize for types to allow to/from JSON
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub json_serde_derives: bool,

    /// Emit a `#[cfg(test)]` module of reject tests into the generated rust crate: for every type
    /// with a bounded (RangeCheck) field, a test that pushes that field out of bounds and asserts
    /// deserialization rejects it. Off by default; the existing suite generates without it.
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub emit_tests: bool,

    /// Add an independent conformance oracle to every `--emit-tests` round-trip case: right after
    /// the value's CBOR bytes are computed, validate them against the SOURCE `.cddl` rule using the
    /// `cddl` crate's validator (decode + constraint evaluation independent of our encoder/decoder).
    /// This catches IR-level miscompiles that mint a spec-violating value and then assert it
    /// round-trips green (the round-trip harness shares the generator's IR, so it can't catch those
    /// on its own). Requires `--emit-tests`. Off by default. The generated test crate then needs the
    /// `cddl` dependency and its source spec on disk next to the crate (see the manual IR-conformance
    /// gate, `integration_tests::ir_conformance_corpus`).
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub emit_tests_conformance: bool,

    /// Opt-in recursion depth guard for generated deserializers. When set to N, every generated
    /// composite `deserialize` acquires an RAII depth guard and returns a graceful
    /// `DeserializeError` (never overflows the stack) once nesting exceeds N. OFF by default: a
    /// depth limit necessarily rejects spec-valid documents deeper than N, so cddl-codegen must not
    /// invent a data limit the spec doesn't have. Enable it when parsing untrusted input, where an
    /// unbounded recursive type (e.g. `tree = [value: uint, children: [* tree]]`) would otherwise
    /// let hostile deeply-nested CBOR overflow the stack and abort the process.
    #[clap(long, value_parser, value_name = "DEPTH")]
    pub deserialize_depth_limit: Option<u32>,

    /// Tags types with sonSchema derives and generates a crate to export them
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub json_schema_export: bool,

    /// Generates a npm package.json along with build scripts
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub package_json: bool,

    /// Copies the shipped JSON-schema -> TypeScript scripts (`run-json2ts.js`, `json-ts-types.js`)
    /// into `<output>/scripts/` WITHOUT writing a `package.json`. `--package-json
    /// --json-schema-export` already copies them alongside its own manifest; this flag is the
    /// opt-in for a consumer that hand-maintains its npm manifests and only wants the canonical
    /// scripts. The scripts resolve their own paths from their location (`<root>/scripts/*.js`), so
    /// they work in both the `--package-json` layout (wasm crate at `<root>/rust/wasm`) and the bare
    /// one (wasm crate at `<root>/wasm`); `--root=`/`--wasm-dir=`/`--dts=`/`--method=` override.
    /// Requires `--json-schema-export` (the scripts read the schemas the json-gen crate writes).
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub json_schema_scripts: bool,

    /// Register an ADDITIONAL type as a JSON-schema root: one extra `reg.add::<RUST_PATH>();` row
    /// in the json-gen crate's `add_schemas`, for a type that is part of the published surface but that the CDDL never
    /// describes (a hand-written address/key type whose JSON form is API while its bytes are not a
    /// CDDL rule). The value is a RUST path rooted anywhere the json-gen crate can reach — the own
    /// rust crate (`cddl_lib::byron::ByronAddress`) or another crate the consumer declares in the
    /// generated `wasm/json-gen/Cargo.toml` — with `--json-gen-dep`, or by hand (that manifest is
    /// MERGED, never clobbered, so either survives regeneration). Emitted VERBATIM, so the value's charset is
    /// restricted to `[A-Za-z0-9_]`, `:`, `<`, `>`, `,` and space; cddl-codegen does not typecheck
    /// Rust, so a path that does not resolve is an `E0433`/`E0412` in the consumer's json-gen build,
    /// never a generation-time reject. Extra roots are emitted AFTER every spec-derived row, in flag
    /// order (never sorted), and go through the same registrar — so they are subject to the
    /// same published-name injectivity guard as any other row. A path naming a type whose CDDL rule
    /// carries `@no_json_schema_export` re-registers it (the flag consults no IR at all). Repeatable;
    /// two identical values are a hard error. Requires `--json-schema-export` (without it there is no
    /// json-gen crate and no `add_schemas` for the row to land in).
    #[clap(
        long = "json-schema-root",
        value_parser = parse_json_schema_root,
        value_name = "RUST_PATH"
    )]
    pub json_schema_root: Vec<String>,

    /// Thread a DEPENDENCY's whole row set into this crate's schema document: one
    /// `<DEP_JSON_GEN_LIB>::add_schemas(generator);` call emitted into the json-gen crate's
    /// `add_schemas`. With one document per crate, anything this crate's own types reference is
    /// already present through the closure — what this adds is a dependency's UNREFERENCED roots,
    /// which no closure over this document can reach and which `--json-schema-root` can only cover
    /// by hand-restating the dep's root list (a duplicate that silently drifts from it). Each value
    /// is `<dep>=<dep_json_gen_lib_name>`. The LEFT side is a LABEL only — it is never resolved
    /// against the extern-dep set, since the emission depends solely on the right side; it exists for
    /// duplicate detection, error messages, and readability beside the `--extern-wasm-crate` line for
    /// the same dependency. The RIGHT side is a rust MODULE PATH emitted VERBATIM (charset
    /// `[A-Za-z0-9_]`, `:` and `-`, the last normalised to `_` so a cargo package name works
    /// verbatim). This flag alone does not touch `wasm/json-gen/Cargo.toml`: it knows the crate NAME
    /// but not where the crate lives, so the `[dependencies]` entry comes from `--json-gen-dep`
    /// (which carries the path) or from a hand edit — that manifest is MERGED rather than clobbered,
    /// so either survives regeneration (same story as `--json-schema-root`'s cross-crate roots). A
    /// name the manifest does not depend on is an
    /// `E0433` in your own json-gen build naming the crate, never a generation-time reject. Dep calls
    /// are emitted FIRST, before this crate's own rows, in flag order (never sorted): a dep's names
    /// are already shipped in the dep's own package, so on a cross-crate collision the consumer's row
    /// is the one that should be renamed — the deliberate mirror of why `--json-schema-root` rows
    /// come last. Repeatable; a repeated label, or one lib name under two labels, is a hard error.
    /// Requires `--json-schema-export`.
    #[clap(
        long = "json-schema-dep",
        value_parser = parse_json_schema_dep,
        value_name = "DEP=DEP_JSON_GEN_LIB"
    )]
    pub json_schema_dep: Vec<String>,

    /// Declare a `[dependencies]` entry in the generated `wasm/json-gen/Cargo.toml`:
    /// `<cargo-package-name> = { path = "<path>" }`. This is the manifest half of every cross-crate
    /// reference the json-gen crate can make — the half the tool cannot derive from the reference
    /// itself, since a rust path names a crate but not where it lives. Three flags produce such a
    /// reference and none of them writes the entry: `--json-schema-dep` (the dep's registrar call),
    /// `--json-schema-root` with a path rooted in another crate, and `--common-import-override`
    /// (the `Registrar`/`check_schema_ref_closure` import). Pass this flag once per crate they
    /// need; without it the name is an `E0433` in your own json-gen build.
    ///
    /// The LEFT side is the **cargo package name** (`cml-chain-json-schema-gen`) — the dashed
    /// spelling that goes in a manifest, NOT the underscored rust lib path
    /// (`cml_chain_json_schema_gen`) that `--json-schema-dep`'s right-hand side takes. Getting the
    /// two backwards is the obvious mistake and the resulting error does not point at it: a
    /// mis-spelled package name is a cargo resolution failure, and a missing one is an `E0433` on a
    /// crate whose name looks right.
    ///
    /// The RIGHT side is written into the manifest VERBATIM, so a relative value means what a cargo
    /// path dependency always means: relative to the directory holding that manifest, i.e.
    /// `<output>/wasm/json-gen/`. (`../../rust`, the entry the tool already writes for this crate's
    /// own rust crate, is the shape to count from.) The tool does not check that the path exists —
    /// an unresolvable one is a cargo error naming it.
    ///
    /// Merge contract: the entry is ASSERTED, never removed. It merges field-level into whatever
    /// `[dependencies]` entry is already there (so a hand-added entry for the same package converges
    /// rather than duplicating, with this flag's `path` winning and the user's other fields — a
    /// `version`, `optional`, `features` — surviving), and dropping the flag LEAVES THE ENTRY
    /// BEHIND: the flag's absence carries no package name, so there is nothing for the tool to
    /// tombstone. Remove a no-longer-wanted dependency by hand, as you would in any manifest.
    ///
    /// Repeatable; a repeated package name is a hard error. Requires `--json-schema-export` (without
    /// it there is no json-gen crate and no manifest for the entry to land in).
    #[clap(
        long = "json-gen-dep",
        value_parser = parse_json_gen_dep,
        value_name = "PACKAGE=PATH"
    )]
    pub json_gen_dep: Vec<String>,

    /// Location override for default common types (error, serialization, etc)
    /// This is useful for integrating into an exisitng project that is based on
    /// these types.
    #[clap(long, value_parser, value_name = "COMMON_IMPORT_OVERRIDE")]
    pub common_import_override: Option<String>,

    /// An external macro to be called instead of manually emitting functions for
    /// conversions to/from CBOR bytes or JSON.
    /// If the macro is scoped it will be imported using the supplied path.
    /// e.g. foo::bar::qux will result in importing foo::bar::qux and then
    /// calling qux!(A); for every struct A with a CBOR/JSON API
    #[clap(long, value_parser)]
    pub wasm_cbor_json_api_macro: Option<String>,

    /// An external macro to be called instead of manually emitting traits for
    /// WASM conversions to/from the inner rust type + AsRef.
    /// If the macro is scoped it will be imported using the supplied path.
    /// e.g. foo::bar::qux will result in importing foo::bar::qux and then
    /// calling qux!(rust::path::A, A); for every struct A with a CBOR/JSON API
    #[clap(long, value_parser)]
    pub wasm_conversions_macro: Option<String>,

    /// Disable carrying user-added comments across regeneration. By default, when a generated
    /// `src/generated/**` file already exists on disk, own-line comments a user added to it are
    /// re-anchored onto the freshly generated output by symbol identity; a comment that cannot be
    /// safely re-placed becomes a `compile_error!` block (loud, never a silent drop). Pass this flag
    /// to skip that overlay entirely and clobber the file with pristine output.
    #[clap(long = "no-preserve-comments", action = clap::ArgAction::SetFalse, default_value_t = true)]
    pub preserve_comments: bool,

    /// An external macro to be called instead of manually emitting the struct +
    /// accessor block (new/len/get/add) + conversion traits for each generated
    /// WASM list wrapper. Lists only - map wrappers are unaffected.
    /// If the macro is scoped it will be imported using the supplied path.
    /// e.g. foo::bar::qux will result in importing foo::bar::qux and then calling
    /// qux!(rust_elem, wasm_elem, WasmName, needs_into, is_copy); for every list wrapper.
    /// (The no-argument form of this flag is reserved for a future built-in default.)
    #[clap(long, value_parser)]
    pub wasm_list_macro: Option<String>,

    /// Suppress emission of RUST `pub type` aliases for generator-SYNTHESIZED collection wrappers
    /// (currently the auto-named keys-list of a table rule, e.g. `pub type FooList = Vec<Foo>;`
    /// minted for `tbl = { * foo => uint }`). Rule-declared aliases are NEVER suppressed, even when
    /// structurally transparent — an explicitly authored `foo_list = [* foo]` or `signature = bytes
    /// .size 32` is a human-written name and always stays. Emission-only: generated code references
    /// collections structurally (`Vec<Foo>`), never via the alias, so no field type or serialization
    /// changes. Wasm-side wrappers/aliases are untouched. Off by default — the aliases are public API
    /// some downstreams may depend on.
    #[clap(long, value_parser, action = clap::ArgAction::Set, default_value_t = false)]
    pub no_synthesized_rust_collection_aliases: bool,

    /// Map an `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` dependency to the crate that holds its
    /// wasm-bindgen wrappers, for deps whose wasm bindings live in a separate crate (the layout
    /// cddl-codegen itself generates: `<dep>` / `<dep>-wasm`). In the wasm pass, imports and
    /// wasm-boundary type paths for that dep are qualified through the wasm crate instead of the
    /// rust crate. Repeatable; each value is `<dep>=<wasm_crate>` (e.g.
    /// `--extern-wasm-crate cml_core=cml_core_wasm`). Without a mapping the dep keeps using its
    /// rust crate name for both passes (the single-crate convention).
    #[clap(long = "extern-wasm-crate", value_parser)]
    pub extern_wasm_crate: Vec<String>,

    /// Point the consumer at a dependency's committed collection-wrapper index
    /// (`generated/collections.rs`, emitted by every wasm run) so it DEFERS to the dep's wasm
    /// wrappers instead of re-minting them (a wasm duplicate-symbol link error otherwise). For each
    /// collection wrapper the consumer would mint whose element/key/value types are all extern types
    /// of `<dep>`, if the wrapper's structurally-derived name appears in `<dep>`'s index the consumer
    /// emits a plain `use <dep_wasm>::collections::<Name>;` (routed through `--extern-wasm-crate`)
    /// instead of a local class; an all-extern wrapper NOT in the index is minted locally with an
    /// stderr warning; mixed-element wrappers are always local and silent. Repeatable; each value is
    /// `<dep>=<path/to/collections.rs>` (e.g.
    /// `--extern-wrapper-index cml_core=../cml-core/wasm/src/generated/collections.rs`). Regenerate
    /// the dep BEFORE the consumer — the index is committed generated output and part of the dep's
    /// cross-crate interface.
    #[clap(long = "extern-wrapper-index", value_parser)]
    pub extern_wrapper_index: Vec<String>,

    /// Mark an `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` dependency as a co-generated workspace member.
    /// For every collection wrapper whose element types are ALL owned (transitively) by that single
    /// dep, the consumer DEFERS UNCONDITIONALLY — emits `use <dep_wasm>::collections::<Name>;` at use
    /// sites (routed through `--extern-wasm-crate`) and never mints a local `#[wasm_bindgen]` class,
    /// regardless of any `--extern-wrapper-index`. This closes the sibling-collision class two
    /// consumers minting the same wrapper both define `pub struct FooList` and collide in one cdylib.
    /// The consumer additionally emits `wasm/src/generated/borrowed_collections.rs` recording what it
    /// borrows and from whom (read by the dep's own generation via a future `--wrapper-requests`).
    /// Ownerless (primitives-only) and mixed-dep wrappers are unaffected — they keep the shipped
    /// index-deferral / local-mint behavior. Repeatable; each value is a bare `<dep>` name (the
    /// `<dep>=<host>` host form for unmodifiable external deps is reserved but not yet supported).
    /// Each named dep must be a configured extern dependency AND have an `--extern-wasm-crate`
    /// mapping (the deferral imports and sidecar `use` lines need the wasm crate name).
    #[clap(long = "workspace-dep", value_parser)]
    pub workspace_dep: Vec<String>,

    /// W2 dep-side companion to `--workspace-dep`: one `<consumer>=<path>` per consumer, pointing at
    /// that consumer's committed `wasm/src/generated/borrowed_collections.rs` sidecar. The dep parses
    /// each sidecar strictly, takes the entries addressed to itself (dep column == this crate's
    /// normalized `--lib-name`), unions the requested collection-wrapper shapes across consumers, and
    /// emits every requested wrapper the dep does not already produce into
    /// `wasm/src/generated/requested_collections.rs` (indexed in the dep's own `collections.rs`, each
    /// carrying a `/// Generated at the request of: …` attribution doc). This hosts the wrapper in the
    /// dep so sibling consumers import one definition instead of each minting a colliding
    /// `#[wasm_bindgen]` class. Repeatable; `<consumer>` is a label (used only in the attribution and
    /// error messages). A `<path>` with NO FILE means that consumer borrows nothing — what a consumer
    /// which has never generated records — and is a loud stderr warning rather than an error, since
    /// otherwise a cold workspace cannot bootstrap in either direction (see
    /// `wrapper_requests::read_request_sidecar`); a file that exists but cannot be read or parsed
    /// stays a hard error. With no `--wrapper-requests` flags the output is byte-identical to today
    /// (the file is not emitted).
    #[clap(long = "wrapper-requests", value_parser)]
    pub wrapper_requests: Vec<String>,

    /// Dep-side companion to a consumer's `rust/src/generated/borrowed_key_types.rs` sidecar (the
    /// in-workspace map-key-derive channel). One `<consumer>=<path>` per consumer. The dep parses each
    /// sidecar strictly, takes the rows addressed to itself (dep column == this crate's normalized
    /// `--lib-name`), resolves each borrowed CDDL ident to a type in this dep's spec, and marks it
    /// used-as-key BEFORE finalize computes the key-derive set — so a consumer map mixing this dep's
    /// key with a consumer-owned value (`{* dep_key => my_local}`, which never enters
    /// `borrowed_collections.rs`) still gets `Eq/Ord/PartialOrd` (plus `Hash` under
    /// `--preserve-encodings`) derived on `dep_key`. A row naming a type the dep no longer defines is a
    /// hard error naming the consumer and file. Repeatable; `<consumer>` is a label (used only in error
    /// messages). A `<path>` with NO FILE is the cold-workspace case — a warning, not an error; same
    /// contract as `--wrapper-requests` above. With no `--key-requests` flags the output is
    /// byte-identical to today.
    #[clap(long = "key-requests", value_parser)]
    pub key_requests: Vec<String>,

    /// Consume a dependency's committed extern-interface export (`extern-interface/<dep>/**`, emitted
    /// by the dep's own regen) instead of hand-maintaining a stub under
    /// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/`. Each mapped path is read and concatenated with
    /// EXTERN_DEPS_DIR scope markers so its rules land in the same non-exported scope a physical stub
    /// tree would — after which the whole extern-deps pathway is unchanged. The export carries the
    /// dep's final Rust names as `@rust_name` pins, so the consumer READS names instead of re-deriving
    /// them (killing the cross-version naming-skew class). Flag-fed files are STRICTLY parsed: each
    /// must begin with the versioned seam header (`; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1`) and carry
    /// only recognized `@`-annotations — a missing/unknown version or an unknown token is a hard error
    /// naming the file. Declaring `<dep>` here AND as a physical `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/`
    /// input directory is a hard error (ambiguous double declaration, never a merge). Same INPUT
    /// category and determinism wording as `--extern-wrapper-index` (explicit cross-crate input; same
    /// inputs -> same bytes). Regenerate the dependency BEFORE the consumer so its export is fresh.
    /// Repeatable; each value is `<dep>=<path/to/extern-interface/<dep>>` (e.g.
    /// `--extern-import cml_core=../cml-core/extern-interface/cml_core`).
    #[clap(long = "extern-import", value_parser)]
    pub extern_import: Vec<String>,

    /// Additionally export the composed rust static runtime into the CRATE at `<dir>` (created if
    /// needed), regardless of whether in-crate static export happens: the runtime files (error.rs,
    /// the serialization.rs prelude, ordered_hash_map.rs, non_empty.rs, non_empty_map.rs) go to
    /// `<dir>/src/`, and `<dir>/Cargo.toml` gets the static-runtime manifest changeset merged in
    /// (`cargo_manifest::ops_for_static_runtime`) so the dependency versions the exported source
    /// requires can never skew from the source itself — source and the manifest that satisfies it
    /// are one artifact. The upgrade path for `--common-import-override` users, who own their
    /// runtime copy and otherwise get no static export. The exported set is a PURE FUNCTION OF THE
    /// FLAG SET, never of the spec: unlike the in-crate path (which gates
    /// non_empty/non_empty_map/raw_bytes on spec usage), the export ALWAYS includes non_empty.rs,
    /// non_empty_map.rs, and raw_bytes_encoding — a shared runtime crate serves many specs, so
    /// which spec was run must not change the output. Flavor selection (preserve-encodings /
    /// canonical / json / schemars / depth-guard) is identical to the in-crate composer. Which
    /// OTHER crates that runtime can COMPILE is not symmetric across those flags, so a workspace
    /// sharing one runtime must pick the exporting flag set deliberately: preserve-encodings /
    /// canonical-form / deserialize-depth-limit must MATCH (the canonical and non-canonical
    /// preludes differ in the arity of fit_sz/to_len_sz/SerializeEmbeddedGroup and in which crate
    /// defines Serialize; the preserve accommodation stops at `{+ K => V}`'s OrderedHashMap-backed
    /// NonEmptyMap and at canonical `AnyCbor::serialize`; the depth limit is baked BY VALUE into
    /// the exported AnyCbor guard, so a mismatch compiles while silently guarding one crate's `any`
    /// values at the exporting crate's limit), while json-serde-derives and json-schema-export
    /// genuinely nest (a runtime carrying them serves a crate that does not). The config file's
    /// `[runtime]` table derives the exporting crate from exactly those rules — see
    /// docs/config_file. EXACTLY ONE invocation may export into a given dir: two at differing
    /// flavors ACCUMULATE rather than overwrite, and the run stops being idempotent (the other
    /// flavor's files linger outside the stale-file scan, the manifest merge accumulates both
    /// flavors' deps, and the preservation overlay cannot classify the other flavor's file so it
    /// injects a fresh compile_error! block every run — measured: 62 -> 143 -> 224 -> 305 blocks
    /// over four runs of one unchanged command pair, exit 0 each time). A hand invocation cannot
    /// see the other one; a config file can, and refuses the shape. No
    /// mod.rs/lib.rs is written — the target crate owns its module declarations; static files
    /// reference siblings via `super::…`. Files pass through the same comment-preservation overlay
    /// as in-crate output. The crate is OUTSIDE the output crate and is not part of the stale-file
    /// bookkeeping. (This flag replaced `--export-static-dir`, which took the src dir itself and
    /// left the manifest untouched — the rename is deliberately a loud break, since reinterpreting
    /// the old value as a crate root would silently write `src/src/` and seed a stray Cargo.toml.)
    #[clap(
        long = "export-static-crate",
        value_parser,
        value_name = "EXPORT_STATIC_CRATE"
    )]
    pub export_static_crate: Option<std::path::PathBuf>,
}

impl Cli {
    /// lib name from code i.e. with underscores
    pub fn lib_name_code(&self) -> String {
        self.lib_name.replace('-', "_")
    }

    /// Parsed `--extern-wasm-crate` mappings: extern-deps directory name -> wasm crate name in code
    /// form. BTreeMap (never HashMap) for deterministic output. Malformed values are a hard error.
    pub fn extern_wasm_crate_map(&self) -> std::collections::BTreeMap<String, String> {
        let mut map = std::collections::BTreeMap::new();
        for entry in &self.extern_wasm_crate {
            let (dep, wasm_crate) = entry.split_once('=').unwrap_or_else(|| {
                panic!("--extern-wasm-crate value must be <dep>=<wasm_crate>, got: {entry:?}")
            });
            let dep = dep.trim();
            let wasm_crate = wasm_crate.trim();
            if dep.is_empty() || wasm_crate.is_empty() {
                panic!(
                    "--extern-wasm-crate value must be <dep>=<wasm_crate> with both sides non-empty, got: {entry:?}"
                );
            }
            map.insert(dep.to_owned(), wasm_crate.replace('-', "_"));
        }
        map
    }

    /// Parsed `--extern-wrapper-index` mappings: extern-deps directory name -> path to the dep's
    /// committed `collections.rs` index file. BTreeMap (never HashMap) for deterministic output.
    /// Malformed values are a hard error, mirroring `extern_wasm_crate_map`.
    pub fn extern_wrapper_index_files(&self) -> std::collections::BTreeMap<String, String> {
        let mut map = std::collections::BTreeMap::new();
        for entry in &self.extern_wrapper_index {
            let (dep, path) = entry.split_once('=').unwrap_or_else(|| {
                panic!(
                    "--extern-wrapper-index value must be <dep>=<path/to/collections.rs>, got: {entry:?}"
                )
            });
            let dep = dep.trim();
            let path = path.trim();
            if dep.is_empty() || path.is_empty() {
                panic!(
                    "--extern-wrapper-index value must be <dep>=<path/to/collections.rs> with both sides non-empty, got: {entry:?}"
                );
            }
            map.insert(dep.to_owned(), path.to_owned());
        }
        map
    }

    /// Parsed `--workspace-dep` values: the set of extern-deps directory names marked co-generated
    /// workspace members. BTreeSet (never HashMap) for deterministic output. An empty value is a hard
    /// error; a value containing `=` is a hard error naming the future `<dep>=<host>` host form as not
    /// yet supported (flag-syntax reservation without implementing it). The names are further
    /// validated against the extern-dep set and `--extern-wasm-crate` mappings at generation time.
    pub fn workspace_deps(&self) -> std::collections::BTreeSet<String> {
        let mut set = std::collections::BTreeSet::new();
        for entry in &self.workspace_dep {
            if entry.contains('=') {
                panic!(
                    "--workspace-dep value {entry:?} contains '='; the <dep>=<host> host form (for \
                     unmodifiable external deps) is reserved but not yet supported — pass a bare \
                     <dep> name"
                );
            }
            let dep = entry.trim();
            if dep.is_empty() {
                panic!("--workspace-dep value must be a non-empty <dep> name, got: {entry:?}");
            }
            set.insert(dep.to_owned());
        }
        set
    }

    /// Parsed `--wrapper-requests` mappings: consumer label -> path to that consumer's committed
    /// `borrowed_collections.rs` sidecar. BTreeMap (never HashMap) for deterministic output — the
    /// requested-wrapper union must not depend on flag order. A malformed value (no `=`, or an empty
    /// side) is a hard error naming the flag; an unreadable path is a hard error at load time (see the
    /// generation-side loader). Mirrors `extern_wrapper_index_files`.
    pub fn wrapper_requests(&self) -> std::collections::BTreeMap<String, String> {
        let mut map = std::collections::BTreeMap::new();
        for entry in &self.wrapper_requests {
            let (consumer, path) = entry.split_once('=').unwrap_or_else(|| {
                panic!(
                    "--wrapper-requests value must be <consumer>=<path/to/borrowed_collections.rs>, got: {entry:?}"
                )
            });
            let consumer = consumer.trim();
            let path = path.trim();
            if consumer.is_empty() || path.is_empty() {
                panic!(
                    "--wrapper-requests value must be <consumer>=<path/to/borrowed_collections.rs> with both sides non-empty, got: {entry:?}"
                );
            }
            map.insert(consumer.to_owned(), path.to_owned());
        }
        map
    }

    /// Parsed `--key-requests` mappings: consumer label -> path to that consumer's committed
    /// `borrowed_key_types.rs` sidecar. BTreeMap (never HashMap) for deterministic output — the
    /// seeded key-derive set must not depend on flag order. A malformed value (no `=`, or an empty
    /// side) is a hard error naming the flag; an unreadable path is a hard error at seed time (see
    /// `wrapper_requests::seed_used_as_key_from_key_requests`). Mirrors `wrapper_requests`.
    pub fn key_requests(&self) -> std::collections::BTreeMap<String, String> {
        let mut map = std::collections::BTreeMap::new();
        for entry in &self.key_requests {
            let (consumer, path) = entry.split_once('=').unwrap_or_else(|| {
                panic!(
                    "--key-requests value must be <consumer>=<path/to/borrowed_key_types.rs>, got: {entry:?}"
                )
            });
            let consumer = consumer.trim();
            let path = path.trim();
            if consumer.is_empty() || path.is_empty() {
                panic!(
                    "--key-requests value must be <consumer>=<path/to/borrowed_key_types.rs> with both sides non-empty, got: {entry:?}"
                );
            }
            map.insert(consumer.to_owned(), path.to_owned());
        }
        map
    }

    /// Parsed `--extern-import` mappings: extern-deps directory name -> path to the dep's committed
    /// `extern-interface/<dep>/` export tree. BTreeMap (never HashMap) for deterministic output — the
    /// concatenation order of imported deps must not depend on flag order. A malformed value (no `=`,
    /// or an empty side) is a hard error naming the flag; a missing/empty-of-cddl path and a
    /// double-declaration against a physical stub dir are hard errors at load time (see the api input
    /// assembly). Mirrors `extern_wrapper_index_files`.
    pub fn extern_import_paths(&self) -> std::collections::BTreeMap<String, String> {
        let mut map = std::collections::BTreeMap::new();
        for entry in &self.extern_import {
            let (dep, path) = entry.split_once('=').unwrap_or_else(|| {
                panic!(
                    "--extern-import value must be <dep>=<path/to/extern-interface/dep>, got: {entry:?}"
                )
            });
            let dep = dep.trim();
            let path = path.trim();
            if dep.is_empty() || path.is_empty() {
                panic!(
                    "--extern-import value must be <dep>=<path/to/extern-interface/dep> with both sides non-empty, got: {entry:?}"
                );
            }
            map.insert(dep.to_owned(), path.to_owned());
        }
        map
    }

    /// Parsed `--json-schema-dep` mappings: `(dep label, dep json-gen lib name in code form)`, in
    /// FLAG ORDER.
    ///
    /// A `Vec` rather than a `BTreeMap`/`BTreeSet` — deliberately the one accessor in this impl that
    /// is not a sorted collection. Every sibling sorts because its consumer must not depend on flag
    /// order; here flag order IS the input being preserved, because it decides the order the dep
    /// registrars run in, and that order is observable through the emitted injectivity guard's
    /// messages. Sorting would silently rewrite an input; a `Vec` keeps "same inputs -> same bytes"
    /// without inventing an ordering. Duplicate detection therefore does not fall out of the
    /// collection type and lives in `api::with_types` instead.
    ///
    /// Dashes in the lib name are normalised to underscores here (mirroring `extern_wasm_crate_map`)
    /// so a cargo package name can be passed verbatim. A malformed value (no `=`, or an empty side)
    /// is a hard error naming the flag, mirroring `extern_wasm_crate_map`; a parsed invocation cannot
    /// reach that panic, since `parse_json_schema_dep` rejects the same shapes gracefully first.
    pub fn json_schema_deps(&self) -> Vec<(String, String)> {
        self.json_schema_dep
            .iter()
            .map(|entry| {
                let (dep, lib) = entry.split_once('=').unwrap_or_else(|| {
                    panic!(
                        "--json-schema-dep value must be <dep>=<dep_json_gen_lib_name>, got: {entry:?}"
                    )
                });
                let dep = dep.trim();
                let lib = lib.trim();
                if dep.is_empty() || lib.is_empty() {
                    panic!(
                        "--json-schema-dep value must be <dep>=<dep_json_gen_lib_name> with both sides non-empty, got: {entry:?}"
                    );
                }
                (dep.to_owned(), lib.replace('-', "_"))
            })
            .collect()
    }

    /// Parsed `--json-gen-dep` mappings: `cargo package name -> path`, SORTED by package name.
    ///
    /// A `BTreeMap`, unlike its `--json-schema-dep` sibling directly above, and for the reason that
    /// one gives inverted: there flag order IS the input, because it decides the order dependency
    /// registrars run in and that order is observable in the emitted guard's messages. Here the
    /// values become `[dependencies]` keys in a TOML manifest, where nothing observes the order they
    /// were declared in — so sorting invents no semantics and gives the manifest a stable key order
    /// independent of how the flags happened to be spelled. Duplicate detection therefore cannot
    /// fall out of the map (a duplicate would silently collapse) and lives in `api::with_types`,
    /// which reads the raw flag list.
    ///
    /// No dash normalisation, unlike `json_schema_deps`: the left side is already the cargo package
    /// name, which is where the dashes belong. A malformed value is a hard error naming the flag,
    /// mirroring the siblings; a parsed invocation cannot reach that panic, since
    /// `parse_json_gen_dep` rejects the same shapes gracefully first.
    pub fn json_gen_deps(&self) -> std::collections::BTreeMap<String, String> {
        let mut map = std::collections::BTreeMap::new();
        for entry in &self.json_gen_dep {
            let (name, path) = entry.split_once('=').unwrap_or_else(|| {
                panic!("--json-gen-dep value must be <cargo_package_name>=<path>, got: {entry:?}")
            });
            let name = name.trim();
            let path = path.trim();
            if name.is_empty() || path.is_empty() {
                panic!(
                    "--json-gen-dep value must be <cargo_package_name>=<path> with both sides non-empty, got: {entry:?}"
                );
            }
            map.insert(name.to_owned(), path.to_owned());
        }
        map
    }

    /// If someone override the common imports, we don't want to export them
    pub fn export_static_files(&self) -> bool {
        self.common_import_override.is_none()
    }

    pub fn common_import_rust(&self) -> &str {
        // Generated code lives under `src/generated/**`, so the crate-local runtime modules
        // (error/serialization/ordered_hash_map) are reached via `crate::generated::…`. An explicit
        // `--common-import-override` points at a separate crate and is used verbatim.
        self.common_import_override
            .as_deref()
            .unwrap_or("crate::generated")
    }

    pub fn common_import_wasm(&self) -> String {
        self.common_import_override
            .clone()
            .unwrap_or_else(|| self.lib_name_code())
    }

    /// The path prefix the `wasm/json-gen` crate reaches the common runtime through — the crate that
    /// hosts `json_schema_gen` (the row `Registrar`, the `add_schema` guard it delegates to, and the
    /// reference-closure check).
    ///
    /// Its body coincides with [`Self::common_import_wasm`] because both name the **rust** runtime
    /// crate: an override points at it verbatim, and with no override the json-gen crate reaches the
    /// generated rust crate by package name through its path dep, resolving into `generated` via
    /// that crate's seed-once `pub use generated::*;` root. It is its own accessor rather than a call
    /// to the wasm-named one so json-gen emission never reads as depending on the WASM crate's
    /// naming: the two answers are equal today for a reason that is entirely about the rust crate,
    /// and the wasm face already diverges elsewhere (`--extern-wasm-crate` routes the built-in
    /// `Int`'s wasm face away from the bare override), so a future divergence must not silently
    /// retarget the json-gen imports.
    pub fn common_import_json_gen(&self) -> String {
        self.common_import_override
            .clone()
            .unwrap_or_else(|| self.lib_name_code())
    }
}
