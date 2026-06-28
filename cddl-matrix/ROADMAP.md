# CDDL master matrix — roadmap (what's remaining)

Read `README.md` first. This is the forward-looking list. Status today: **v1 complete and gate-green**
(81 features, all axes reconciled, execution-grounded support, deterministic; see `README.md` §"What
works today"). What follows is everything *not* yet done, roughly in priority order.

> **The `F#` labels** below are findings from the independent cold critique (a fresh agent given only
> the problem + specs, no codebase — full write-up in git history). **This is the canonical status
> ledger.** Already **done**: F7 (the query contract), F1 (profile axis), F2 (semantic-individuation +
> grammar-as-lint), F4 (tags modeled parametrically — not pinned/enumerated), F5 (encoding axis is
> major-type-dependent, no over-count), F6 (synthesis snapshot guard). **Deferred**: F3. **Out of
> scope**: F8–F11. Only the still-open findings appear below.

## 1. Wire the projections — turn the reference into the source of truth (highest value)

The master exists to be *projected from*.

**Done — the golden_hex (encoding-axis / Q3) projection.** `project_golden_hex.ts` generates
`tests/golden_hex/COVERAGE.md` from `matrix.json` + `sources/appendix_a.json` + `tests.rs`, joining the
human rationale in `annotations/golden_hex/cddl_codegen.toml`. The join is a **drift check**: it fails on
a note that contradicts the derived coverage, a stale note, or any uncovered-and-unexplained vector. This
realizes Q3 at the encoding-cell level (per-construct attribution is the F5 expansion, item 4).

**Remaining:**
- **Corpus (feature-axis) projection.** `tests/corpus/COVERAGE.md` is still hand-maintained and can
  drift from `matrix.json`. Generate/cross-check it from the master + annotations, mirroring the
  `project_golden_hex.ts` pattern (derive the mechanical half, join an authored-notes overlay, drift-check).
- **Q1/Q2/Q4/Q5/Q6 query scripts** from `QUERIES.md`. **➡ Start here: Q1** — "constructs tool X doesn't
  support but are in its target profile" (the everyday "what to implement next" gap). It's the
  highest-value and unblocked *today*: all its inputs (per-tool support + the profile axis) already live
  in `matrix.json` + `annotations/cddl_codegen.toml`. (Q4 is blocked on item 3.)

## 2. Wire the gate into CI

`build_matrix.ts` (synthesis + snapshot `--check`) and `verify.ts` (reconcile + triangulated probes) are
**Bun/TypeScript, run manually** (`bun run build_matrix.ts --check`, `bun run verify.ts`); `lib.ts` holds
the shared loaders + the stable-JSON serializer. Oracle paths are env-overridable (`RUST_CDDL`,
`RUBY_CDDL`; `CODEGEN_DIR` derives from the repo root). Remaining:
- **Wire both into CI** so the drift-check, snapshot guard, and reconciliation run automatically.
  `verify.ts` needs the three oracle tools present (ruby `cddl`, rust `cddl` CLI, `cddl-codegen`); have
  the CI lane provide them or skip that probe gracefully when absent.
- **Typecheck enforcement.** The scripts are strict-typed but nothing runs `tsc` yet — the types are only
  validated ad hoc. Add a `tsconfig.json` + `@types/bun` (dev-only) and a `tsc --noEmit` step beside the
  gate. (Needs an ambient `declare module "*.toml"` for the `project_golden_hex.ts` import.) This is the
  one place a (dev) dependency is worth it; the runtime stays dependency-free.

## 3. F3 — directional / enforcement support (deferred, needs execution)

Today "support" is essentially one bit (accept vs reject/panic, from the exit-code probe). `QUERIES.md`
Q4 wants the 5-way split **{accept, encode, decode, round-trip, enforce-constraint}**. That needs
per-feature *execution* of generated code (compile + round-trip + constraint checks), not just the
parse/accept probe — real machinery we deferred. Do it when execution/corpus wiring exists; Q4 is
blocked until then.

## 4. F4 / F5 follow-ons (only when their consumer exists)

- **F5 (encoding precision):** the encoding axis is already major-type-dependent (no impossible cells).
  The golden-vector projection (item 1) is now wired and lists uncovered legal cells *globally*; the
  remaining F5 work — per-*construct* legal-cell enumeration so Q3 can say "for construct C, these legal
  encodings are untested" — is now unblocked. Link `features[].encodings` to the leaf cells each construct
  can emit and intersect with golden coverage.
- **F4 (tag registry):** deliberately not pinned/enumerated (cddl-codegen is tag-parametric). Revisit
  only if a *tag-semantic* consumer of the master appears.

## 5. Expansion (when relevant)

- **Profiles/versions:** v1 targets the RFC 8610/9682 grammar + the IANA control-op registry (which
  already spans RFC 8610/9090/9165/9741). Add other CDDL profiles (e.g. the modules drafts) if needed,
  and bump cddl-codegen's declared target profile if it updates its `cddl` dependency.
- **More tools:** the master is implementation-agnostic; add `annotations/<other-tool>.toml` if another
  consumer adopts it.

## Explicitly out of scope (decided, not overlooked)

Per the `QUERIES.md` query-map, no consumer query needs these, so they're out of v1 (revisit only if a
concrete need surfaces):
- **F8 — matching semantics** (prioritized-choice ordering, greedy occurrence, implicit `:` cut). More a
  validation concern than a cddl-codegen serialization concern; not needed by any query yet.
- **F9 — interaction tuples** (A-in-B-in-role-C-with-operator-D): a richer-than-binary containment;
  stretch query (Q7) only.
- **F10 / F11** — note-only (over-acceptance denominator; AST cross-check is weak corroboration).

## Pending decisions (need a human call)

- The judgment-call critique findings — e.g. the **over-acceptance denominator**: should coverage be
  measured against the grammar's full superset, or only realistically-implementable constructs?
- (Resolved this session) Doc consolidation is done: `README.md` is the entry; `ROADMAP.md` /
  `QUERIES.md` / `sources/MANIFEST.md` are the rest. The session-process docs (the scale report and the
  cold-critique write-up) were folded into these and removed — recoverable from git history if needed.

## Maintenance

Upstream specs churn (IANA registries, the grammar). Refresh with `sources/fetch.sh` (re-fetches +
verifies against `SHA256SUMS`); a checksum mismatch flags upstream drift to review before re-pinning and
regenerating.

## Related

Broader testing work (clear-wins, roadmap items, the trailing-bytes runtime change) lives in
`tests/TESTING_ROADMAP.md` — the matrix is the reference that testing effort *projects from*.
