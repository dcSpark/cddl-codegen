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

The master exists to be *projected from*, but the projections are not wired yet:
- `tests/corpus/COVERAGE.md` and `tests/golden_hex/COVERAGE.md` are still **hand-maintained
  separately** — they can drift from `matrix.json`. Generate them (or at least cross-check them) **from**
  the master + annotations, so the master is genuinely the single source of truth.
- Implement the **Q1–Q6 query scripts** from `QUERIES.md` (e.g. "constructs unsupported by tool X but
  in-profile"; "supported but untested"). These are the matrix's actual point; until they exist, the
  master is a well-formed reference that nothing consumes.

Until this lands, the master and the coverage docs are independent artifacts that *describe* the same
thing rather than one deriving from the other.

## 2. Reimplement the gate as a Rust CI test

`build_matrix.py` (synthesis + snapshot `--check`) and `verify.py` (reconcile + triangulated probes) are
**Python prototypes**, run manually. Port them to a Rust test wired into the suite (serde `toml` +
`serde_json`, no Python dependency) so the drift-check, snapshot guard, and reconciliation run in CI.
The oracle commands (ruby `cddl`, rust `cddl` CLI, `cddl-codegen` probe) are documented in `verify.py`.

## 3. F3 — directional / enforcement support (deferred, needs execution)

Today "support" is essentially one bit (accept vs reject/panic, from the exit-code probe). `QUERIES.md`
Q4 wants the 5-way split **{accept, encode, decode, round-trip, enforce-constraint}**. That needs
per-feature *execution* of generated code (compile + round-trip + constraint checks), not just the
parse/accept probe — real machinery we deferred. Do it when execution/corpus wiring exists; Q4 is
blocked until then.

## 4. F4 / F5 follow-ons (only when their consumer exists)

- **F5 (encoding precision):** the encoding axis is already major-type-dependent (no impossible cells).
  The remaining work — per-construct *legal-cell* enumeration so Q3 can list "legal encodings minus
  covered" — is a query-time expansion to build *when the golden-vector projection is wired* (step 1).
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
