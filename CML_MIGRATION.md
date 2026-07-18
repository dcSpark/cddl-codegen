# CML migration: hand-stub dirs → `--extern-import` (extern-interface exports)

Audience: the agent (or maintainer) updating cardano-multiplatform-lib (CML) to the
extern-interface export system that shipped in cddl-codegen series `47ba2f3`..`8942963`
(2026-07-18). Read this whole document before touching CML; the per-crate loop in §3 is the
actual work.

Reference docs in this repo (read on demand, not prerequisites):
- `docs/docs/integration-other.mdx` — the consumer-facing contract (primary path + escape hatch).
- `docs/docs/command_line_flags.mdx` § `--extern-import` — the flag's full error contract.
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
   `git log --oneline` for `feat(api): consumer-side --extern-import`).
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
   - If yes: that consumer must keep its FULL hand-stub for `D` (the flag and a physical
     `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<D>/` dir for the same dep is a deliberate hard error —
     there is no partial mixing, because a supplement channel would reintroduce the drift
     surface this migration removes). Alternatives worth one look before accepting that:
     restructure the spec so the custom-serialize handling sits on a class-backed (opaque) type,
     which exports cleanly. Record whichever choice you make in CML's own notes.
3. **For each consumer `C` of `D` (already-migrated deps stay as flags):** delete
   `specs/<…>/_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<D>/`, add
   `--extern-import <D>=<path/to/D's committed extern-interface/<D>>` to `C`'s generation
   command, regenerate `C`, and **diff the generated output against the previous commit**:
   - **Zero diff** is the expected result for a faithful stub — the shipped acceptance tests
     pin byte-identity for exactly this swap (pins that agree with derived names change nothing,
     including import grouping).
   - **A non-empty diff means the old hand-stub had drifted from `D`'s real surface** — the diff
     IS the drift, surfacing. Review it (it should be explicable type-by-type: a stale
     transparent spelling, a wrapper-class mismatch); if a diff line is NOT explicable as stub
     drift, stop and report it upstream before committing.
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

## 4. End state

- No `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/` directories remain in CML specs, EXCEPT any dep kept on a
  full hand-stub by a §3.2 unexported-record decision (revisit those when the export dialect
  grows the relevant spelling — tracked in cddl-codegen's `tests/TESTING_ROADMAP.md` § Deferred
  features).
- Every crate's `extern-interface/<crate>/**` is committed and regenerates as part of the normal
  regen flow; consumers' generation commands carry one `--extern-import` per dep.
- Nothing else in the command lines changed, and no transition machinery exists to clean up on
  the cddl-codegen side: the physical-stub pathway the escape hatch uses is the same input
  channel the flag feeds, not parallel legacy code.

## 5. If something is underspecified

Prefer stopping over improvising: the byte-identity expectation (§3.3) is the invariant the
whole migration leans on, so any surprise there is worth a report to the cddl-codegen side with
the crate, the command line, and the unexplained diff hunk. cddl-codegen's own synthetic
fixtures (`tests/extern-import/`, `tests/extern-import-transitive/`) mirror this migration's
shape and are the right place for a reduced repro.
