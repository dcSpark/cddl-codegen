# CML migration: hand-stub dirs → `--extern-import` (extern-interface exports)

Audience: the agent (or maintainer) updating cardano-multiplatform-lib (CML) to the
extern-interface export system that shipped in cddl-codegen series `47ba2f3`..`8942963`
(2026-07-18). Read this whole document before touching CML; the per-crate loop in §3 is the
actual work, and §4 is that loop applied to the one edge — multi-era consuming chain — that has been
executed end to end and measured.

Reference docs in this repo (read on demand, not prerequisites):
- `docs/docs/integration-other.mdx` — the consumer-facing contract (primary path + escape hatch).
- `docs/docs/command_line_flags.mdx` § `--extern-import` — the flag's full error contract.
- `docs/docs/config_file.mdx` § "The workspace edge" — declaring the same edge as a `deps` key,
  and everything else that key derives with it.
- `docs/docs/output_format.mdx` — the `extern-interface/<dep>/**` tree and the compiled
  self-check (`rust/src/generated/extern_interface_check.rs`).
- `docs/docs/comment_dsl.mdx` § `@rust_name` — the name-pin annotation exports carry.

## 1. What changed and what it replaces

Every cddl-codegen regeneration now also emits `extern-interface/<crate>/…` next to `rust/` and
`wasm/` — a machine-generated CDDL description of that crate's extern-visible surface: opaque
`_CDDL_CODEGEN_EXTERN_TYPE_` rows for class-backed types, truthful transparent spellings for
aliases/c-style enums/named collections, `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rows, and a
`; @rust_name <Name>` pin on each rule recording the FINAL Rust name this crate's build actually
has. A consumer passes `--extern-import <dep>=<path/to/extern-interface/<dep>>` instead of
maintaining a hand-written stub tree under `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/`.

Why CML wants this: the hand-stub trees are the drift surface the stub-fidelity contract exists
to police (spell transparent types truthfully, declare-then-delete, wrapper-name agreement). The
export is projected from the dependency's finalized IR at its own regen time, so fidelity is a
property of reading a committed machine-written file — and the `@rust_name` pins mean a future
cddl-codegen naming-rule change cannot skew consumer-derived names against the dep artifact.

**The consumer's half of the statement is computed.** A dependency's export is complete — it states
that dependency's whole surface once, independent of how many consumers read it — while a consumer
means a slice of it, and `--extern-import` derives that slice: the rule names your spec references
and does not define itself, closed over the export's own rule bodies. Only transparent rows (aliases,
named collections, value enums, plain-group bodies) carry closure edges, so the closure is shallow. An
export rule nothing reaches never enters your document at all, which is what a hand-written stub
always approximated by hand — and it is why a consumer may define a rule name the dependency also
defines, as long as it does not need the dependency's one.

What does NOT change: `--extern-wasm-crate`, `--extern-wrapper-index`, `--workspace-dep`,
`--wrapper-requests`, `--key-requests`, `--common-import-override`, and the wasm macros all keep
their existing semantics and dep keying (the extern-deps directory name = the dep's snake_case
lib name). The regen-order discipline ("regenerate the dependency before the consumer") is
unchanged — the export simply rides it. Hand-written types declared in a crate's OWN spec
(top-level `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rules, e.g. the crypto
types) are not part of this migration; they stay exactly as they are — and they appear in that
crate's export automatically, so downstream crates import them like everything else.

## 2. Preconditions (do not skip)

1. **cddl-codegen at `8942963` or later** (the whole series must be present; check
   `git log --oneline` for `feat(api): consumer-side --extern-import`). For the multi-era edge, a rev
   carrying need-based narrowing and the corrected I64 export spelling as well. Check the tool by its
   output rather than by a rev: regenerate `chain` and look at the `non_zero_int_64` line of its
   export. `non_zero_int_64 = int` is the old spelling, and it round-trips to the big-`Int` extern
   class rather than to `i64`; `non_zero_int_64 = -9223372036854775808..9223372036854775807` is the
   corrected one.
2. **CML working tree fully committed.** Generation clobbers `src/generated/**`; regenerating
   over uncommitted work destroys it. Commit (or stash) everything first, and re-check
   `git status` immediately before each regen — this rule has teeth (a near-miss on a large
   uncommitted CML migration is why cddl-codegen's own AGENTS.md forbids casual consumer regens).
3. Migrate **one crate at a time, in forward dependency order** (dependency before consumer):
   for CML that means `cml_core` (and `cml_crypto` if spec-generated) → `cml_chain` →
   `cml_multiera` → anything downstream. Commit after each green step.

## 3. The per-crate loop

For each crate `D` in forward dependency order:

1. **Regenerate `D` with the new tool, flags unchanged.** Two things appear beyond the usual
   output: `extern-interface/<D>/**` (commit it — it is tool-owned, always clobbered, and is now
   part of `D`'s public artifact exactly like the wasm `collections.rs` index) and
   `rust/src/generated/extern_interface_check.rs` (a compiled self-check; if `D`'s own build
   fails IN THIS FILE, the export and the crate disagree — that is a cddl-codegen bug, report it
   upstream with the failing type name; do not hand-edit either file).
2. **Inspect `D`'s export for `; unexported:` records** (comment lines after the header in each
   file). Each names a rule the projection could not spell faithfully (typically a
   `@custom_serialize` transparent alias, or a rule referencing one — records name the chain
   root). Cross-check: does any CONSUMER crate's spec reference an unexported ident?
   - If no: proceed.
   - If yes: the fix is on `D`'s side, because `D` is the authority on its own surface and a
     dependency is declared exactly once — a stub beside the import is the double-declaration hard
     error, and one under any other directory name resolves those rules to a different crate,
     since the directory name IS the crate they resolve to. So, in order: restructure `D`'s spec so
     the custom-serialize handling sits on a class-backed (opaque) type, which exports cleanly; or
     have `D` declare the type it hand-owns in its own spec as `_CDDL_CODEGEN_EXTERN_TYPE_`, which
     the export then carries; or report the projection limitation upstream. Only if `D` cannot be
     changed at all does that consumer keep its FULL hand-stub for `D` — which means not passing
     `--extern-import <D>` rather than adding a stub next to it. Record whichever choice you make in
     CML's own notes.
3. **For each consumer `C` of `D` (already-migrated deps stay as flags):** delete
   `specs/<…>/_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<D>/`, add
   `--extern-import <D>=<path/to/D's committed extern-interface/<D>>` to `C`'s generation
   command, regenerate `C`, and **diff the generated output against the previous commit**:
   - **Zero diff** is the expected result for a stub that already spelled `D`'s surface the way `D`
     itself does — the shipped acceptance tests pin byte-identity for exactly this swap (pins that
     agree with derived names change nothing, including import grouping).
   - **A non-empty diff is the gap between what the stub said and what `D` is**, surfacing. Two
     things produce it and both are explicable type-by-type: drift proper (a stale transparent
     spelling, a wrapper-class mismatch), and a stub that declared one of `D`'s *transparent*
     aliases as an opaque type of its own, where the export resolves the alias through to its
     target and every use site is respelled accordingly. If a diff line is explicable as neither,
     stop and report it upstream before committing. The multi-era edge is the worked example —
     see "The multi-era edge, measured" below.
4. **Failure modes at step 3 and what they mean:**
   - "undefined reference" wrapped with the extern-import dep list and a staleness hint → `C`
     references an ident absent from `D`'s export: either `D`'s committed export predates its
     current spec (regen `D` first — you skipped the order) or the ident is in `D`'s
     `; unexported:` records (step 2's decision applies).
   - Missing/unknown-version header or unknown-annotation hard error → the export files were
     hand-edited or truncated; restore them from `D`'s regen (never edit exports by hand — `D`'s
     self-check exists to catch exactly this).
   - A stub-fidelity diagnostic naming "tool bug — please report" → report it upstream verbatim;
     it should be unreachable against machine-generated exports.
5. Run `C`'s own build/tests, commit, move on.

## 4. The multi-era edge, measured

This is §3's loop applied to the one CML edge that has been executed end to end and measured. The
run was made on **scratch copies** of `specs/multiera` and of chain's committed export, in a
throwaway directory, never against the CML checkout — so treat everything here as a procedure
someone has reproduced on copies, not as a result already in your tree. Re-verify it the same way,
on a copy, at the rev you are about to use, before you run it against the real tree.

The edge is worth its own section because it is the one the collision was on. multi-era's spec
defines `block = _CDDL_CODEGEN_EXTERN_TYPE_` — its hand-owned re-export of chain's Conway `Block` —
and chain's export carries a `block` rule of its own. The two names cannot both be in one CDDL
document. Need-based narrowing is what settles it: multi-era's spec never references chain's `block`,
so chain's `block` never enters multi-era's document, and multi-era's own definition stands.

### Where the two sides stand

Measured against CML's committed tree:

- multi-era's stub is `specs/multiera/_CDDL_CODEGEN_EXTERN_DEPS_DIR_/cml_chain/`, **10 files**.
- chain's committed export is `chain/extern-interface/cml_chain/`, **139 rules across 10 files**,
  with **3** `; unexported:` records: `pool_params`, `pool_registration`, `update_committee`.
- Rule names defined by **both** multi-era's spec and chain's export: exactly `{ block }`.
- Export rules multi-era's spec references: **54**; their reference closure inside the export:
  **63 of 139**, against a hand stub of roughly 64 referenced-plus-support rules. The stub is a
  hand-computed approximation of that closure.
- That closure's intersection with multi-era's own rule names: **empty**. The one shared name is on
  a rule multi-era never needs from chain.
- Nothing multi-era needs is absent from the export: none of the three unexported rules is
  referenced by the multiera spec, so §3.2's decision does not arise on this edge.

### The steps

1. **Regenerate `chain`** at a rev carrying the corrected I64 spelling (§2). Its committed export
   changes by one line — `non_zero_int_64 = int` becomes the explicit full range — and that one line
   is what makes a consumer type mint values as `i64` rather than as the big-`Int` extern class.
   Commit it as chain's own artifact before touching multi-era.
2. **Delete `specs/multiera/_CDDL_CODEGEN_EXTERN_DEPS_DIR_/cml_chain/`** (10 files), and delete
   nothing else. `specs/multiera-byron/_CDDL_CODEGEN_EXTERN_DEPS_DIR_/cml_chain/` belongs to the
   byron pass, a separate opt-in invocation over its own input tree; `specs/cip36/…` holds cip36's
   own two declarations, one of them for a hand-written crate that has no export to import. Each is
   a separate decision, to be taken separately or not at all.
3. **Declare the edge.** Under a config, `deps = ["chain"]` on `[crates.multi-era]`. On the script as
   it stands, `--extern-import cml_chain=chain/extern-interface/cml_chain` added to multi-era's
   invocation, with the four existing extern flags unchanged.
4. **Regenerate multi-era and review the diff against the five classes below.** Anything outside
   them is a finding — either a feature that landed after these measurements changed the output, or
   the classification is wrong. Stop and report it rather than absorbing it as noise.
5. **Check the manifests** against what the derivation asserts (below).

### The five diff classes

Roughly **278 diff-lines of rust and 25 of wasm**, all of it in these five:

1. **Alias spelling at use sites.** The stub declares several of chain's *transparent* aliases as
   opaque types of their own — `policy_id = _CDDL_CODEGEN_RAW_BYTES_TYPE_` where chain's export says
   `policy_id = script_hash`, `shelley_format_aux_data = _CDDL_CODEGEN_EXTERN_TYPE_` where the export
   says `shelley_format_aux_data = metadata`, and likewise for `stake_credential` against the
   export's `stake_credential = credential`. The export resolves each alias through to its target, so
   use sites read `ScriptHash` where the stub produced `PolicyId`, `Metadata::deserialize` for
   `ShelleyFormatAuxData::deserialize`, `Credential` for `StakeCredential`. Same types throughout;
   what changes is the nominal spelling.
2. **Parameter names improve.** `pool: &Ed25519KeyHash` replaces the type-derived
   `ed25519_key_hash`, because the export's plain-group bodies carry chain's real field names and the
   stub's opaque rows could not. It changes `.d.ts` argument names, which is cosmetic for JS/TS
   callers since those arguments are positional.
3. **`borrowed_key_types.rs` ident respelling** — `policy_id` → `script_hash`,
   `stake_credential` → `credential`: class 1's resolution reaching the cross-crate request sidecar.
   Compile-verified against committed chain, whose substituted types already carry the required key
   traits; chain's next regen re-resolves the respelled requests to the same structs, so its own diff
   is confined to the label spelling in its `/// Generated at the request of: …` attribution
   comments. The wasm-side sidecar `borrowed_collections.rs` came out **byte-identical**.
4. **One benign wasm-root addition** — `pub use cml_core::Int;` and `pub type IntError = JsError;` —
   which is essentially the whole of the wasm diff.
5. **The overlay replace blocks apply unchanged.** multi-era's committed generated tree carries
   hand-maintained `cddl-codegen:replace` blocks at the `mint` sites (`mint: mint.map(Into::into)`),
   which exist because chain's real Rust surface hand-replaces the suppressed collection alias with
   `AssetBundle<T>` — a divergence no spec-derived description, stub or export, can see. With the
   committed generated tree seeding the preservation overlay, those blocks re-apply, and both
   `cml-multi-era` and `cml-multi-era-wasm` `cargo check` clean against the real
   cml-chain/cml-core/cml-crypto. The two `E0308`s at those sites appear under **both** mechanisms —
   a fresh stub-based generation through the same binary fails identically — so they say nothing
   about the edge.

### The manifests

`deps` also derives the `[dependencies]` entries multi-era hand-maintains today
(`cml-chain` in `multi-era/rust/Cargo.toml`, `cml-chain` and `cml-chain-wasm` in the wasm one). They
are **asserted, never removed**, and an entry that already exists is merged field-level rather than
replaced: a `version =` pin and any `features` the entry carries survive, while `path` is
re-asserted at the derived value — so a hand path that differs from the derived one is silently
replaced by it. A dependency name the tool manages conditionally (`hex`, `serde`, …) is still
tombstoned when the flags no longer want it, exactly as it is today.

### What was measured, and what was not

- The figures above were measured on scratch copies with the full CML flag set
  (`--preserve-encodings`, `--canonical-form`, both JSON flags,
  `--common-import-override=cml_core`, the three wasm macros, `--extern-wasm-crate`,
  `--extern-wrapper-index` against chain's committed collections index, `--workspace-dep`), with the
  narrowing and the I64 correction in place — applied by hand at measurement time, since neither had
  landed as code yet.
- They cover generated **source** only: the measurement predates the `deps` edge deriving manifest
  entries, so the manifest behaviour above was established separately, on a synthetic two-crate
  fixture rather than at CML scale.
- Nothing here has been run against the CML checkout, deliberately. That execution is the CML
  owner's, on his schedule, with his tree committed.

## 5. End state

- The only `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/` directories left in CML specs are the ones a stub is
  now *for* — a dependency with no export to import: a hand-written crate such as `cml_crypto`, a
  spec set generated by a deliberately separate pass such as `multiera-byron`, and any dep kept on a
  full hand-stub by a §3.2 decision (revisit that last kind when the export dialect grows the
  relevant spelling — tracked in cddl-codegen's `tests/TESTING_ROADMAP.md` § Deferred features).
- Every crate's `extern-interface/<crate>/**` is committed and regenerates as part of the normal
  regen flow; consumers' generation commands carry one `--extern-import` per dep that has one.
- Nothing else in the command lines changed, and no transition machinery exists to clean up on
  the cddl-codegen side: the physical-stub dialect is the same input channel the flag feeds, not
  parallel legacy code.

## 6. If something is underspecified

Prefer stopping over improvising: the invariant the whole migration leans on is that **every diff
line is explicable** — §3.3's classes for a generic edge, §4's five for multi-era — so a hunk that
fits none of them is worth a report to the cddl-codegen side with the crate, the command line, and
the hunk itself. cddl-codegen's own synthetic fixtures (`tests/extern-import/`,
`tests/extern-import-transitive/`) mirror this migration's shape and are the right place for a
reduced repro.
