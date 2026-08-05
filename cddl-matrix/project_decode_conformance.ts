#!/usr/bin/env bun
/**
 * Decode-conformance catalog drift gate (design doc D6) — PURE FILE READS, no cargo, no oracles.
 *
 * Cross-checks the committed decode corpus `tests/decode_conformance/catalog.toml` against the matrix:
 *   1. Completeness — every matrix `supported` row id has a catalog row, and each catalog row carries
 *      EITHER >=1 vector OR a `pinned_reason` (never both, never neither).
 *   2. Staleness    — every catalog row id is still `supported` in matrix.json, and the catalog
 *      `example` string-equals the matrix row's `example` (a drifted example means the vectors were
 *      validated against a spec the matrix no longer describes — re-mint).
 *   3. Shape        — every `expect="reject"` vector has `class` ∈ {bug, limitation, constraint} AND a
 *      nonempty `reason` (a class-less pin is the mint's triage-pending state — RED); a class="constraint"
 *      vector additionally carries a nonempty `expect_err` (the rejection-reason substring the rust replay
 *      gate asserts), which is forbidden on every other vector. An `expect="accept"` vector carries EITHER
 *      no class (spec-VALID, correctly accepted) OR exactly `class="over-acceptance"` (spec-INVALID CBOR
 *      the decoder wrongly accepts — a certified silent-acceptance pin) with a nonempty `reason` and NO
 *      `expect_err`; any other class on an accept vector is a schema error. Every hex is well-formed (nonempty, even
 *      length, lowercase); `spec`/`mode`/`type_name` are present together on an active
 *      row and consistent (mode ∈ {standalone, holder}; holder ⇒ spec starts with the holder prefix and
 *      type_name === "ProbeHolder"; standalone ⇒ spec === example); a pinned row carries none of them.
 *   4. Seeded controls — a hard-coded list of (row id, hex) that MUST exist as accept vectors: the
 *      absent-instance TDD anchors that catch an over-strict-decoder reintroduction. These are positive
 *      controls: they must PASS today.
 *   5. Vacuity floor — >= 80 supported matrix rows, so a broken matrix read can't pass an empty check.
 *   6. Constraint / over-acceptance vector shape — a `class="constraint"` vector must be decodable up to
 *      the constraint itself, so the emitted range/size check is the ONLY thing that can reject it (a
 *      wrong-shape vector rejects as a TYPE mismatch first — vacuous enforcement evidence the replay gate
 *      cannot distinguish, the escape that shipped holder-wrapped `8200…` scalars on the first rangeop
 *      mint). The SAME rule guards a `class="over-acceptance"` vector: it must be a same-shape instance
 *      the decoder wrongly accepts, not a bare type mismatch. Both share the row's SPEC-VALID accepts'
 *      leading major-type class (over-acceptance vectors are excluded from that class set — spec-INVALID
 *      bytes evidence nothing about the spec's shape).
 *      Mechanically: its leading CBOR major type must match the row's spec-valid accept vectors' (majors 0/1
 *      merged — int-family instances legitimately span both signs); on a standalone row with no
 *      accepts (an oracle gap can leave a row's accept side un-mintable — the non-uint-endpoint
 *      range rows sat that way until the fork's `885c61c` fix) it must not carry the mint's `8200`
 *      holder preamble. If a standalone row's instances genuinely begin `[0, …]`, add an accept
 *      vector sharing the shape.
 *   7. Accept-vector ARM-coverage floor for choice rows — a randomized mint can land a multi-arm choice
 *      row with a whole arm UNSAMPLED, silently under-claiming the row's decode verdict (at HEAD
 *      `prelude.number` = `int / float` carried only int-headed accepts; the float arm had ZERO
 *      decode-direction evidence). For each ACTIVE catalog row that (a) has >=1 spec-VALID accept
 *      vector and (b) whose matrix `example`'s root rule RHS is a type CHOICE with statically-resolvable
 *      arm head major-classes (`resolveChoiceArmClasses` in lib.ts — the ONE resolver the mint's
 *      resample-until-covered loop shares), require >=1 spec-valid accept vector per resolvable arm
 *      class. Majors 0/1 merge into one "int" class (the § 6 convention — int-family instances span both
 *      signs), so `prelude.integer`'s plain-uint-unsampled gap does NOT flag (nint covers int); only a
 *      genuinely-unsampled DISTINCT class flags. Resolution is CONSERVATIVE (the floor must not guess):
 *      an explicit-choice arm whose head is unresolvable is exempt per-arm; a `/=` incremental extension
 *      or a control-op-decorated RHS is out of scope; a prelude alias resolves only when the root RHS is
 *      EXACTLY a bare prelude choice-type name. Two decay pins guard it: EXPECTED_FLOOR_SCOPE pins the
 *      EXACT (row id -> sorted arm classes) set the resolver fires on (a silent widen/narrow fails
 *      got/want), and DECODE_FLOOR_ARM_EXEMPT (lib.ts, stale-guarded) ledgers a genuinely unmintable arm
 *      class (an oracle gap) with a citation. At HEAD it is EMPTY — its one past resident
 *      (`prelude.number`'s float arm, unmintable while the rust reference rejected floats against the
 *      prelude `number` keyword) was re-minted once the fork fix landed; the stale-guard is what prunes
 *      an entry (forcing a re-mint) whenever a ledgered gap closes.
 *   8. Catalog writer↔reader identity — compose(parse(catalog.toml)) must be byte-identical to the
 *      committed file. `composeCatalog` (lib.ts) is the SOLE serializer of the hand-authored vector
 *      fields (class/reason/expect_err); a field the writer forgets to emit is stripped SILENTLY at the
 *      next re-mint (the bug class caught once by review: accept-vector class/reason were emitted only
 *      under an `expect === "reject"` guard, which would have stripped every over-acceptance
 *      annotation). This pure round-trip of the committed bytes goes red BEFORE any mint runs and also
 *      catches unknown/extra keys (the reader drops them, so the recompose omits them → mismatch). A
 *      synthetic all-fields sample (constructed in-code, exercising EVERY schema field) additionally
 *      round-trips through parse∘compose independent of what the committed catalog happens to exercise,
 *      so a writer that drops a field currently unused by the committed rows still fails.
 *   9. Annotation-evidence ↔ decode-catalog coherence — every supported row's top-level
 *      `matrix.json` evidence must carry exactly one decode-foreign clause, and that clause's
 *      count/presence must match the committed catalog count used by `verify.ts`'s decode-foreign
 *      probe: active, current rows count `expect="accept"` vectors except `class="over-acceptance"`;
 *      missing/pinned/unusable rows expect "no committed decode vectors"; stale catalog examples are
 *      left to § 2 so the drift is not double-reported. A `FAILED (N)` clause is count-checked only;
 *      the replay gate owns whether decode actually succeeds.
 *
 * CORPUS half (composition-depth leg) — the same contract re-derived against the sibling
 * `tests/decode_conformance/corpus_catalog.toml`, whose obligation set is `tests/corpus/*.cddl` ×
 * the SHARED rule enumerator (lib.ts `enumerateCorpusRules` — never a hand list; a fixture rule
 * name colliding with a prelude name fails loud here AND at the mint):
 *   - Completeness (§ 1): every enumerated (fixture, rule) has a catalog row; each row carries
 *     EITHER >=1 vector XOR a `pinned_reason`, plus `fixture`/`rule` provenance matching its id.
 *   - Spec-reconstruction staleness (§ 2): an ACTIVE row's committed `spec`/`example` must
 *     byte-equal the reconstruction from the CURRENT fixture via the shared enumerator + closure
 *     builder (`corpusProbeSpec`/`corpusClosureBody`) — a drifted fixture reads "re-mint". A
 *     PINNED row's `example` is checked the same way (a fixture edit can invalidate the pin's
 *     justification); pinned rows carry no `spec`.
 *   - Shape (§ 3): every active corpus row is HOLDER mode (`type_name = "ProbeHolder"`, spec
 *     starts with the holder prefix); every vector begins with a major-4 array head + literal-0
 *     first element (`8200…`, or `8N00…` for a spliced bare group).
 *   - Arm-coverage floor trio (§ 7): CORPUS_EXPECTED_FLOOR_SCOPE decay pin (resolver fires via
 *     `corpusArmExample`, target rule first), per-class coverage floor, and the stale-exempt
 *     guard over CORPUS_DECODE_FLOOR_ARM_EXEMPT (lib.ts — a SEPARATE ledger from the matrix one:
 *     each stale-guard iterates its own uncovered set, so cross-ledger keys would falsely read
 *     stale).
 *   - Writer↔reader identity (§ 8): compose(parse(corpus_catalog.toml), CORPUS_CATALOG_INTRO)
 *     byte-identical to the committed file; the synthetic all-fields sample covers the
 *     fixture/rule fields on both active and pinned rows.
 *   - Vacuity floors (§ 5): >= 55 fixtures enumerated, >= 120 obligation rows.
 *   - Self-checks: the shared enumerator/closure builder is exercised against
 *     synthetic samples (strings with escaped quotes/semicolons, comments, generics, hyphens) so a
 *     regression in the ONE shared implementation is caught here regardless of what the committed
 *     corpus exercises.
 *
 * This script NEVER writes (there is nothing to project/rewrite in v1), so the DEFAULT run IS the check
 * — no `--check` flag is needed. A `--check` arg is accepted and ignored for symmetry with the
 * projection family (`--check` = CI drift mode elsewhere). Exit nonzero with a per-problem list on any
 * failure; a clean run prints a one-line summary (rows / vectors / pins by class).
 *
 * Run from cddl-matrix/:
 *   bun run project_decode_conformance.ts   -> the drift gate (default)
 */
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { join } from "node:path";
import type { CatalogRow, CorpusRule } from "./lib";
import { CORPUS_CATALOG_INTRO, CORPUS_DECODE_FLOOR_ARM_EXEMPT, CORPUS_HOLDER_RULE, DECODE_FLOOR_ARM_EXEMPT, DECODE_REJECT_ORACLE_GAP_EXEMPT, PRELUDE_NAMES, composeCatalog, corpusClosureBody, corpusProbeSpec, corpusArmExample, dependencyClosure, enumerateCorpusRules, parseCatalogContent, resolveChoiceArmClasses, vectorShapeClass } from "./lib";

const HERE = import.meta.dir;
const CATALOG_REL = "tests/decode_conformance/catalog.toml";
const HOLDER_PREFIX = "__probe_holder = [0, ";

// The absent-instance regression anchors (design D6 §4): each is a REAL minted holder/standalone
// instance whose acceptance proves the decoder still admits the "nothing there" shape an over-strict
// decoder would wrongly reject. They must be present as expect="accept" vectors on their row.
const SEEDED_CONTROLS: { id: string; hex: string; comment: string }[] = [
  {
    id: "occur.optional",
    hex: "820080",
    comment: "holder [0, []] — the `? name: tstr` optional member is ABSENT; anchors the over-strict " +
      "class that rejects a legally-absent optional field",
  },
  {
    id: "type2.map",
    hex: "8200a0",
    comment: "holder [0, {}] — an EMPTY `* tstr => int` table; nearest live relative of the fixed " +
      "single-field-map zero-occurrence narrowing, anchors rejection of an empty map",
  },
  {
    id: "occur.zero_or_more",
    hex: "820080",
    comment: "holder [0, []] — ZERO repetitions of a `* uint` element; anchors the array-side " +
      "over-strict class that rejects the empty-repetition instance",
  },
];

// --- matrix.json: supported ids + their examples --------------------------------------------------
interface MatrixLookupRow { id: string; example: string }
interface Annotation { id: string; status: string; evidence?: unknown }
interface MatrixJson {
  annotations: { cddl_codegen: Annotation[] };
  features: MatrixLookupRow[];
  containment: MatrixLookupRow[];
  control_operators: MatrixLookupRow[];
}
const matrix = (await Bun.file(`${HERE}/matrix.json`).json()) as MatrixJson;
const exampleById = new Map<string, string>();
for (const arr of [matrix.features, matrix.containment, matrix.control_operators])
  for (const r of arr) exampleById.set(r.id, r.example);
const supportedAnnotations = matrix.annotations.cddl_codegen.filter(a => a.status === "supported");
const supported = new Set(
  supportedAnnotations.map(a => a.id),
);

// --- catalog.toml ---------------------------------------------------------------------------------
interface CatVector { hex?: unknown; source?: unknown; expect?: unknown; class?: unknown; reason?: unknown; expect_err?: unknown }
interface CatRow {
  id?: unknown; axis?: unknown; example?: unknown; fixture?: unknown; rule?: unknown; pinned_reason?: unknown;
  spec?: unknown; mode?: unknown; type_name?: unknown; vector?: CatVector[];
}
const catalogText = readFileSync(`${HERE}/../${CATALOG_REL}`, "utf8");
const catalog = Bun.TOML.parse(catalogText) as { row?: CatRow[] };
const rows = catalog.row ?? [];

const problems: string[] = [];
const catalogById = new Map<string, CatRow>();
const HEX_RE = /^[0-9a-f]+$/;

// --- §9: annotation-evidence ↔ decode-catalog coherence helpers -----------------------------------
const DECODE_EVIDENCE_CLAUSE_RE =
  /; no committed decode vectors \(see catalog\)(?=;|$)|; accepts ([0-9]+) foreign spec-derived vector\(s\)(?=;|$)|; foreign-vector decode FAILED \(([0-9]+) vector\(s\)\)(?=;|$)/g;
const DECODE_EVIDENCE_FIX =
  "re-run the full verify.ts after a scoped mint — mint BEFORE probe, or re-probe after";

type DecodeEvidenceClause =
  | { kind: "none"; text: string }
  | { kind: "accepts"; count: number; text: string }
  | { kind: "failed"; count: number; text: string };
type DecodeCatalogExpectation =
  | { kind: "none" }
  | { kind: "vectors"; count: number };

function parseDecodeEvidenceClauses(evidence: unknown): DecodeEvidenceClause[] {
  if (typeof evidence !== "string") return [];
  return [...evidence.matchAll(DECODE_EVIDENCE_CLAUSE_RE)].map(m => {
    const text = m[0];
    if (text === "; no committed decode vectors (see catalog)") return { kind: "none", text };
    if (m[1] !== undefined) return { kind: "accepts", count: Number(m[1]), text };
    return { kind: "failed", count: Number(m[2]), text };
  });
}

function describeCommittedDecodeClause(clauses: DecodeEvidenceClause[]): string {
  if (clauses.length === 0) return "NO decode-foreign clause";
  if (clauses.length === 1) return JSON.stringify(clauses[0].text);
  return `${clauses.length} decode-foreign clauses [${clauses.map(c => JSON.stringify(c.text)).join(", ")}]`;
}

function describeExpectedDecodeClause(expected: DecodeCatalogExpectation): string {
  if (expected.kind === "none") return JSON.stringify("; no committed decode vectors (see catalog)");
  return `count ${expected.count} via either ${JSON.stringify(`; accepts ${expected.count} foreign spec-derived vector(s)`)} or ` +
    `${JSON.stringify(`; foreign-vector decode FAILED (${expected.count} vector(s))`)}`;
}

function decodeCatalogExpectation(row: CatRow | undefined, matrixExample: string): DecodeCatalogExpectation | "skip" {
  if (!row || row.pinned_reason !== undefined || row.spec === undefined || row.type_name === undefined)
    return { kind: "none" };
  if (row.example !== matrixExample) return "skip";
  const n = (row.vector ?? []).filter(v => v.expect === "accept" && v.class !== "over-acceptance").length;
  return n === 0 ? { kind: "none" } : { kind: "vectors", count: n };
}

function evidenceCatalogClauseProblem(id: string, evidence: unknown, expected: DecodeCatalogExpectation): string | null {
  const clauses = parseDecodeEvidenceClauses(evidence);
  const fail = () =>
    `\`${id}\`: annotation evidence decode-foreign clause drift — committed clause: ` +
    `${describeCommittedDecodeClause(clauses)}; expected: ${describeExpectedDecodeClause(expected)}; fix: ${DECODE_EVIDENCE_FIX}`;
  if (clauses.length !== 1) return fail();
  const clause = clauses[0];
  if (expected.kind === "none") return clause.kind === "none" ? null : fail();
  if ((clause.kind === "accepts" || clause.kind === "failed") && clause.count === expected.count) return null;
  return fail();
}

for (const r of rows) {
  const id = typeof r.id === "string" ? r.id : undefined;
  if (id === undefined) { problems.push(`catalog row is missing a string \`id\`: ${JSON.stringify(r)}`); continue; }
  if (catalogById.has(id)) problems.push(`duplicate catalog row id \`${id}\``);
  catalogById.set(id, r);

  const vectors = r.vector ?? [];
  const hasVectors = vectors.length > 0;
  const pinned = typeof r.pinned_reason === "string" && r.pinned_reason.length > 0;

  // Completeness §1: exactly one of {>=1 vector, pinned_reason}.
  if (hasVectors === pinned)
    problems.push(`\`${id}\`: a catalog row must have EITHER >=1 vector OR a nonempty pinned_reason, not ${hasVectors ? "both" : "neither"}`);

  // Staleness §2: id still supported, and the example matches the matrix verbatim.
  if (!supported.has(id))
    problems.push(`\`${id}\`: catalog row is not a \`supported\` matrix row (construct no longer supported — remove or re-mint)`);
  const matrixExample = exampleById.get(id);
  const catExample = typeof r.example === "string" ? r.example : undefined;
  if (matrixExample !== undefined && catExample !== matrixExample)
    problems.push(`\`${id}\`: catalog example drifted from the matrix example — re-mint\n    matrix : ${JSON.stringify(matrixExample)}\n    catalog: ${JSON.stringify(catExample)}`);

  if (pinned) {
    // A pinned row is vectorless AND carries none of the active-row fields.
    for (const f of ["spec", "mode", "type_name"] as const)
      if (r[f] !== undefined)
        problems.push(`\`${id}\`: pinned row must not carry \`${f}\` (it has no vectors to decode)`);
    continue;
  }
  if (!hasVectors) continue; // already reported above; nothing more to shape-check

  // Shape §3: an active row needs spec/mode/type_name, all consistent.
  const spec = typeof r.spec === "string" ? r.spec : undefined;
  const mode = typeof r.mode === "string" ? r.mode : undefined;
  const typeName = typeof r.type_name === "string" ? r.type_name : undefined;
  if (spec === undefined) problems.push(`\`${id}\`: active row is missing \`spec\``);
  if (typeName === undefined) problems.push(`\`${id}\`: active row is missing \`type_name\``);
  if (mode !== "standalone" && mode !== "holder")
    problems.push(`\`${id}\`: \`mode\` must be "standalone" or "holder" (got ${JSON.stringify(mode)})`);
  else if (mode === "holder") {
    if (spec !== undefined && !spec.startsWith(HOLDER_PREFIX))
      problems.push(`\`${id}\`: holder-mode spec must start with \`${HOLDER_PREFIX}\` (oracle root-rule targeting)`);
    if (typeName !== undefined && typeName !== "ProbeHolder")
      problems.push(`\`${id}\`: holder-mode type_name must be "ProbeHolder" (got ${JSON.stringify(typeName)})`);
  } else if (mode === "standalone") {
    if (spec !== undefined && catExample !== undefined && spec !== catExample)
      problems.push(`\`${id}\`: standalone-mode spec must equal the example verbatim`);
  }

  // Shape §3: per-vector hex + reject-pin class/reason.
  vectors.forEach((v, i) => {
    const where = `\`${id}\` vector[${i}]`;
    const hex = typeof v.hex === "string" ? v.hex : undefined;
    if (hex === undefined || hex.length === 0)
      problems.push(`${where}: missing/empty hex`);
    else if (hex.length % 2 !== 0)
      problems.push(`${where}: hex \`${hex}\` has odd length (not whole bytes)`);
    else if (!HEX_RE.test(hex))
      problems.push(`${where}: hex \`${hex}\` is not lowercase hex ([0-9a-f])`);
    const expect = v.expect;
    if (expect !== "accept" && expect !== "reject")
      problems.push(`${where}: \`expect\` must be "accept" or "reject" (got ${JSON.stringify(expect)})`);
    // An accept vector may carry NO class (spec-VALID, correctly accepted) or EXACTLY
    // class="over-acceptance" (spec-INVALID CBOR the decoder wrongly accepts — a certified silent-
    // acceptance pin). Any other class on an accept vector is a schema error.
    if (expect === "accept") {
      if (v.class !== undefined && v.class !== "over-acceptance")
        problems.push(`${where}: an accept vector may carry no class or class="over-acceptance" (got ${JSON.stringify(v.class)})`);
      if (v.class === "over-acceptance" && (typeof v.reason !== "string" || v.reason.length === 0))
        problems.push(`${where}: class="over-acceptance" vector needs a nonempty \`reason\` (cite the ledgered finding + the promotion flow: flips to class="constraint" with an expect_err when the fix lands)`);
    }
    if (expect === "reject") {
      if (v.class !== "bug" && v.class !== "limitation" && v.class !== "constraint")
        problems.push(`${where}: reject vector \`class\` must be "bug", "limitation" or "constraint" (got ${JSON.stringify(v.class)}) — a class-less reject is triage-pending`);
      if (typeof v.reason !== "string" || v.reason.length === 0)
        problems.push(`${where}: reject vector needs a nonempty \`reason\` (the ledgered bug / doc citation, or the violated constraint for class="constraint")`);
    }
    // Shape §3: `expect_err` — the rejection-reason substring the rust replay gate asserts. REQUIRED on
    // class="constraint" (that gate names the violated constraint, not just that it rejects); FORBIDDEN
    // everywhere else, so its meaning stays tight to durable-reject enforcement evidence.
    if (v.class === "constraint" && expect === "reject") {
      if (typeof v.expect_err !== "string" || v.expect_err.length === 0)
        problems.push(`${where}: class="constraint" vector needs a nonempty \`expect_err\` (a substring the generated decoder's error Display must contain — pins the rejection reason, asserted by the rust replay gate)`);
    } else if (v.expect_err !== undefined) {
      problems.push(`${where}: only class="constraint" reject vectors may carry \`expect_err\` (got ${JSON.stringify(v.expect_err)}) — it pins the constraint-rejection reason and is meaningless elsewhere`);
    }
  });

  // Constraint-vector shape §6: enforcement evidence must be rejectable ONLY by the constraint.
  // Leading-major-type class of a vector (majors 0/1 merged: int-family instances span both signs).
  const shapeClass = (hex: string): string => {
    const major = parseInt(hex.slice(0, 2), 16) >> 5;
    return major <= 1 ? "int" : String(major);
  };
  // acceptClasses is computed from SPEC-VALID accepts only — an over-acceptance vector (spec-INVALID)
  // is not evidence of what the spec's shape is, so it must not seed the shape-class set it is checked
  // against.
  const acceptClasses = new Set(
    vectors
      .filter(v => v.expect === "accept" && v.class !== "over-acceptance" && typeof v.hex === "string" && (v.hex as string).length >= 2)
      .map(v => shapeClass(v.hex as string)),
  );
  vectors.forEach((v, i) => {
    // The same shape rule guards BOTH spec-INVALID vector kinds: a class="constraint" reject vector
    // (must be rejectable ONLY by the constraint) and a class="over-acceptance" accept vector (must be
    // a same-shape instance the decoder wrongly accepts, not a bare type mismatch). Both must share the
    // row's spec-valid accepts' leading major-type class.
    const isConstraint = v.expect === "reject" && v.class === "constraint";
    const isOverAccept = v.expect === "accept" && v.class === "over-acceptance";
    if (!isConstraint && !isOverAccept) return;
    const kind = isConstraint ? "constraint" : "over-acceptance";
    const hex = typeof v.hex === "string" ? v.hex : undefined;
    if (hex === undefined || hex.length < 2) return; // hex problems already reported above
    const where = `\`${id}\` vector[${i}]`;
    if (acceptClasses.size > 0) {
      if (!acceptClasses.has(shapeClass(hex)))
        problems.push(
          `${where}: ${kind} vector \`${hex}\` has leading CBOR major-type class "${shapeClass(hex)}" but the row's spec-valid accept vectors are {${[...acceptClasses].sort().join(", ")}} — a wrong-shape vector rejects as a TYPE mismatch before the ${isConstraint ? "constraint check runs (vacuous enforcement evidence)" : "row's own decode path is reached (it evidences a type mismatch, not the widening over-acceptance)"}`,
        );
    } else if (mode === "standalone" && hex.startsWith("8200")) {
      problems.push(
        `${where}: ${kind} vector \`${hex}\` carries the mint's \`8200\` holder preamble on a standalone row with no accept vectors — holder-wrapped scalars reject as a TYPE mismatch (if the row's type genuinely begins [0, …], add an accept vector sharing the shape)`,
      );
    }
  });
}

// --- §7: accept-vector ARM-coverage floor for choice rows -----------------------------------------
// The EXACT (row id -> sorted arm classes) set the CONSERVATIVE resolver (lib.ts, shared with the mint)
// fires on at HEAD. Decay pin (the query_q4 EXPECTED_ENFORCE_YES pattern): a resolver change that
// silently widens or narrows the in-scope set fails got/want here — growing/shrinking it must be a
// conscious edit. Only `prelude.number` is under-covered at HEAD (float arm class 7 unsampled); the
// re-mint's resample loop closes it. Classes are majors-0/1-merged ("int"); `prelude.integer` /
// `.unsigned` therefore read {6, int} (nint covers int, tagged bignum covers 6) and do NOT flag on their
// unsampled plain-uint side.
const EXPECTED_FLOOR_SCOPE: Record<string, string[]> = {
  // The `any`-arm POSITION cells. A bare `any` arm has no wire class of its own (it accepts every
  // major), so the resolver sees only the `tstr` sibling and the floor asks for a major-3 accept —
  // which is the honest ask: no vector can witness "the catch-all was reached" from the outside,
  // that evidence is the content-fallthrough fixture's (tests/any-choice). The TAGGED arm DOES have
  // a class (major 6, which is exactly why it is not a catch-all and may sit non-last), so its row
  // owes an accept per arm and carries both.
  "contain.choice-member.prelude.any.last": ["3"],
  "contain.choice-member.type2.tag.any_non_last": ["3", "6"],
  "contain.choice-member.prelude.null": ["3", "7"],
  // The same-major-type (brute-force) fixed-arm row: `true` and `null` BOTH resolve to major 7, so
  // the two-class floor {3, 7} is satisfied by a single major-7 accept and cannot, by construction,
  // demand one per fixed arm. That is the arm floor's known blind spot on this shape rather than a
  // gap in the row — the per-arm evidence is the emitted round-trip (`fixed_special_type_choice_brute`
  // in tests/preserve-encodings), and the row carries `f5` AND `f6` anyway.
  "contain.choice-member.prelude.true.same_major_brute": ["3", "7"],
  "contain.choice-member.type2.tag": ["6"],
  // The tag-set idiom rows spell the two WIRE arms (`#6.258([..]) → major 6`, `[..] → major 4`)
  // regardless of whether codegen collapses (set_idiom) or retains the enum (set_idiom_near_miss), so
  // the floor demands a spec-valid accept per arm — the untagged (major 4) direction our default
  // encoder never emits is the independent decode evidence the verdict rests on.
  "contain.choice-member.type2.tag.set_idiom": ["4", "6"],
  "contain.choice-member.type2.tag.set_idiom_near_miss": ["4", "6"],
  // `@duplicates reject`'s example spells the tag-set idiom's two WIRE arms (`#6.258([..]) / [..]`
  // — tag → major 6, array → major 4) even though codegen collapses it to one transparent type, so
  // the floor demands an accept vector per wire arm — the same per-arm evidence the corpus
  // tag_set_* rows pin. (The preserve sibling's example is a single-arm table; no floor entry.)
  "dsl.duplicates.reject": ["4", "6"],
  "prelude.bigint": ["6"],
  "prelude.bool": ["7"],
  "prelude.float": ["7"],
  // The two spanning float names spell a choice of two float TYPES (`float16 / float32`,
  // `float32 / float64`), which is one wire class — every arm is major 7 — so the floor asks only
  // that the row carry a major-7 accept. The per-CLASS evidence those names actually need is not a
  // shape the arm floor can see: the arms partition the float VALUES by shortest lossless form, and a
  // value class is not a major type (it is not even a head — reads accept every float head and judge
  // the decoded value). It lives in the byte-exact vectors instead: `float_heads` in tests/core and
  // tests/preserve-encodings, plus this catalog's own per-class constraint vectors.
  "prelude.float16-32": ["7"],
  "prelude.float32-64": ["7"],
  "prelude.int": ["int"],   // bare `x = int` — the uint/nint arm merges to "int"
  "prelude.integer": ["6", "int"],
  "prelude.number": ["7", "int"],
  "prelude.unsigned": ["6", "int"],
  "type.choice": ["2", "3", "int"],
  "type.enum": ["int"],
};
const floorScope: Record<string, string[]> = {};       // resolver-fired set (id -> sorted classes)
const uncoveredInScope = new Set<string>();             // "<id>/<class>" pairs genuinely uncovered
for (const r of rows) {
  const id = typeof r.id === "string" ? r.id : undefined;
  if (id === undefined) continue;
  const vectors = r.vector ?? [];
  if (vectors.length === 0 || !supported.has(id)) continue;  // pinned/vectorless or non-live (other §§ flag)
  const example = typeof r.example === "string" ? r.example : "";
  const res = resolveChoiceArmClasses(example);
  if (!res) continue;                                        // out of scope (not a resolvable choice)
  const holder = r.mode === "holder";
  const specValidAccepts = vectors.filter(
    v => v.expect === "accept" && v.class !== "over-acceptance" &&
      typeof v.hex === "string" && (v.hex as string).length >= (holder ? 6 : 2),
  );
  if (specValidAccepts.length === 0) continue;               // scope requires >=1 spec-valid accept vector
  floorScope[id] = res.classes;
  const covered = new Set(specValidAccepts.map(v => vectorShapeClass(v.hex as string, holder)));
  for (const cls of res.classes) if (!covered.has(cls)) uncoveredInScope.add(`${id}/${cls}`);
}
// Scope pin: the resolver-fired set must equal EXPECTED_FLOOR_SCOPE exactly.
const gotScope = JSON.stringify(Object.fromEntries(Object.keys(floorScope).sort().map(k => [k, floorScope[k]])));
const wantScope = JSON.stringify(Object.fromEntries(Object.keys(EXPECTED_FLOOR_SCOPE).sort().map(k => [k, EXPECTED_FLOOR_SCOPE[k]])));
if (gotScope !== wantScope)
  problems.push(`arm-coverage floor scope drifted (the resolver fires on a different (row -> arm classes) set than pinned):\n    got : ${gotScope}\n    want: ${wantScope}`);
// Coverage floor: each in-scope arm class needs >=1 spec-valid accept vector, unless ledgered exempt.
for (const key of [...uncoveredInScope].sort()) {
  if (Object.hasOwn(DECODE_FLOOR_ARM_EXEMPT, key)) continue;
  const [id, cls] = key.split("/");
  problems.push(
    `\`${id}\`: choice arm class "${cls}" has ZERO spec-valid accept vector(s) (required {${(floorScope[id] ?? []).join(", ")}}) — ` +
      `re-mint \`bun run verify.ts --mint-decode-foreign --only=${id}\` (resample-until-covered), or add a cited DECODE_FLOOR_ARM_EXEMPT entry for a genuine oracle gap`,
  );
}
// Exemption-ledger stale guard: every ledgered (row, class) must still be a genuinely-uncovered in-scope
// pair — an entry for a now-covered / out-of-scope pair is stale and fails the gate.
for (const key of Object.keys(DECODE_FLOOR_ARM_EXEMPT).sort())
  if (!uncoveredInScope.has(key))
    problems.push(`DECODE_FLOOR_ARM_EXEMPT names \`${key}\` which is no longer a genuinely-uncovered in-scope arm class (covered now, or the row left the floor's scope) — stale ledger entry, remove it`);

// Oracle-gap exemption stale guard, the DRIFT-GATE half (the half that holds the CATALOG; the mint
// half, which holds the oracles, lives in verify.ts and fires when a ledgered oracle starts rejecting).
// Every DECODE_REJECT_ORACLE_GAP_EXEMPT key must still name a live `class="constraint"` reject vector
// in its row: a vector that was re-minted away, re-classed, or whose row was dropped leaves an entry
// that certifies nothing while still reading as a live upstream claim.
{
  const constraintHexes = new Set<string>();
  for (const r of rows)
    for (const v of r.vector ?? [])
      if (v.expect === "reject" && v.class === "constraint") constraintHexes.add(`${r.id}/${v.hex}`);
  for (const key of Object.keys(DECODE_REJECT_ORACLE_GAP_EXEMPT).sort()) {
    const e = DECODE_REJECT_ORACLE_GAP_EXEMPT[key];
    if (!constraintHexes.has(key))
      problems.push(`DECODE_REJECT_ORACLE_GAP_EXEMPT names \`${key}\`, which is no longer a class="constraint" reject vector in that row — stale ledger entry, remove it`);
    if (e.oracles.length === 0)
      problems.push(`DECODE_REJECT_ORACLE_GAP_EXEMPT \`${key}\` names no accepting oracle — an exemption that exempts nothing; remove it`);
    if (!existsSync(join(HERE, "..", e.writeup)))
      problems.push(`DECODE_REJECT_ORACLE_GAP_EXEMPT \`${key}\` cites writeup \`${e.writeup}\`, which does not exist — an exemption's spec argument must be readable by the party it accuses`);
  }
}

// Completeness §1: every supported matrix row must have a catalog row.
for (const id of [...supported].sort())
  if (!catalogById.has(id))
    problems.push(`supported matrix row \`${id}\` has no catalog row (mint it or pin it)`);

// Seeded controls §4: each must be present as an accept vector on its row.
for (const c of SEEDED_CONTROLS) {
  const row = catalogById.get(c.id);
  if (!row) { problems.push(`seeded control \`${c.id}\` (${c.hex}): no catalog row — ${c.comment}`); continue; }
  const present = (row.vector ?? []).some(v => v.hex === c.hex && v.expect === "accept");
  if (!present)
    problems.push(`seeded control \`${c.id}\`: accept vector \`${c.hex}\` is missing — ${c.comment}`);
}

// Vacuity floor §5.
if (supported.size < 80)
  problems.push(`only ${supported.size} supported matrix rows (expected >= 80) — matrix read looks broken/empty`);

// --- §8: catalog writer↔reader identity -----------------------------------------------------------
// compose(parse(committed bytes)) must reproduce the committed file exactly. `composeCatalog` is the
// SOLE serializer of the hand-authored vector fields; a field it forgets to emit is stripped silently
// at the next re-mint. Round-tripping the ALREADY-READ bytes (no second file read) makes that loud
// here, before any mint runs. Also catches unknown/extra keys (the reader drops them → recompose omits
// them → mismatch).
{
  const recomposed = composeCatalog([...parseCatalogContent(catalogText).values()]);
  if (recomposed !== catalogText) {
    const a = catalogText.split("\n");
    const b = recomposed.split("\n");
    let i = 0;
    while (i < a.length && i < b.length && a[i] === b[i]) i++;
    problems.push(
      `catalog writer↔reader identity: compose(parse(catalog.toml)) is NOT byte-identical to the committed file — ` +
        `first divergence at line ${i + 1}:\n` +
        `    committed  : ${a[i] === undefined ? "<EOF>" : JSON.stringify(a[i])}\n` +
        `    recomposed : ${b[i] === undefined ? "<EOF>" : JSON.stringify(b[i])}\n` +
        `    Either \`composeCatalog\` dropped/reordered a field (fix the writer — the silent-strip bug class), ` +
        `or a hand edit is not writer-canonical (match the writer's field order and JSON-style string escaping, ` +
        `or re-mint that row with \`bun run verify.ts --mint-decode-foreign --only=<id>\`). ` +
        `Unknown/extra keys also trip this (the reader drops them, so the recompose omits them).`,
    );
  }
}

// §8 (cont.): synthetic all-fields roundtrip. The committed-bytes identity above only exercises the
// fields the committed catalog currently uses. This in-code sample covers EVERY schema field, so a
// writer that silently drops a field unused by the committed rows still fails. Construction is
// order-insensitive (composeCatalog sorts rows by id and vectors by hex); we deep-compare per key.
{
  const sample: CatalogRow[] = [
    { id: "synthetic.pinned", axis: "synthetic", example: "x = uint", pinned_reason: "synthetic pin — names the cause", vectors: [] },
    {
      id: "synthetic.standalone", axis: "synthetic", example: "x = uint", mode: "standalone", spec: "x = uint", type_name: "X",
      vectors: [
        { hex: "00", source: "spec", expect: "accept" },
        { hex: "01", source: "hand", expect: "accept", class: "over-acceptance", reason: "synthetic over-acceptance — ledgered finding + promotion flow" },
        { hex: "02", source: "spec", expect: "reject", class: "bug", reason: "synthetic bug — spec-valid wrongly rejected" },
        { hex: "03", source: "spec", expect: "reject", class: "limitation", reason: "synthetic limitation — known gap" },
        { hex: "04", source: "hand", expect: "reject", class: "constraint", reason: "synthetic constraint — violated bound", expect_err: "out of range" },
        { hex: "05", source: "hand", expect: "accept" },
      ],
    },
    {
      id: "synthetic.holder", axis: "synthetic", example: "y = uint", mode: "holder", spec: "__probe_holder = [0, y]", type_name: "ProbeHolder",
      vectors: [
        { hex: "820006", source: "spec", expect: "accept" },
        { hex: "820007", source: "hand", expect: "reject", class: "constraint", reason: "synthetic holder constraint", expect_err: "too large" },
      ],
    },
    // A corpus-shaped row exercising the fixture/rule provenance fields (emitted only when present, so
    // catalog.toml's bytes are unchanged) through parse∘compose. Both active and pinned corpus rows carry
    // them; this active one plus the pinned one below cover both branches.
    {
      id: "synthetic-fixture.rule_name", axis: "corpus", example: "rule_name = uint", fixture: "synthetic-fixture", rule: "rule_name",
      mode: "holder", spec: "__probe_holder = [0, rule_name]\nrule_name = uint", type_name: "ProbeHolder",
      vectors: [{ hex: "820000", source: "spec", expect: "accept" }],
    },
    {
      id: "synthetic-fixture.pinned_rule", axis: "corpus", example: "pinned_rule<t> = [t]", fixture: "synthetic-fixture", rule: "pinned_rule",
      pinned_reason: "synthetic corpus pin — generic rule", vectors: [],
    },
  ];
  const back = parseCatalogContent(composeCatalog(sample));
  const vKey = (v: { hex: string }) => v.hex;
  const vEq = (x: any, y: any) =>
    x.hex === y.hex && x.source === y.source && x.expect === y.expect &&
    x.class === y.class && x.reason === y.reason && x.expect_err === y.expect_err;
  if (back.size !== sample.length)
    problems.push(`catalog synthetic roundtrip: parse∘compose produced ${back.size} rows, expected ${sample.length} — the writer dropped/merged a row`);
  for (const row of sample) {
    const got = back.get(row.id);
    if (!got) { problems.push(`catalog synthetic roundtrip: row \`${row.id}\` vanished through parse∘compose — the writer dropped it`); continue; }
    for (const f of ["axis", "example", "fixture", "rule", "pinned_reason", "spec", "mode", "type_name"] as const)
      if (row[f] !== got[f])
        problems.push(`catalog synthetic roundtrip: row \`${row.id}\` field \`${f}\` did not survive parse∘compose (${JSON.stringify(row[f])} -> ${JSON.stringify(got[f])}) — the writer dropped/mangled it`);
    const wantV = [...row.vectors].sort((p, q) => (vKey(p) < vKey(q) ? -1 : 1));
    const gotV = [...got.vectors].sort((p, q) => (vKey(p) < vKey(q) ? -1 : 1));
    if (wantV.length !== gotV.length)
      problems.push(`catalog synthetic roundtrip: row \`${row.id}\` has ${gotV.length} vectors after parse∘compose, expected ${wantV.length} — the writer dropped a vector`);
    else
      wantV.forEach((v, i) => {
        if (!vEq(v, gotV[i]))
          problems.push(`catalog synthetic roundtrip: row \`${row.id}\` vector \`${v.hex}\` lost a field through parse∘compose (${JSON.stringify(v)} -> ${JSON.stringify(gotV[i])}) — the writer silently strips it (the class/reason/expect_err emission bug class)`);
      });
  }
}

// --- §9: annotation-evidence ↔ decode-catalog coherence -------------------------------------------
{
  const selfProblems: string[] = [];
  const assertProblem = (name: string, problem: string | null, wantNeedle: string) => {
    if (problem === null || !problem.includes(wantNeedle))
      selfProblems.push(`${name}: expected a problem containing ${JSON.stringify(wantNeedle)}, got ${JSON.stringify(problem)}`);
  };
  const assertOk = (name: string, problem: string | null) => {
    if (problem !== null) selfProblems.push(`${name}: expected OK, got ${JSON.stringify(problem)}`);
  };
  const expectFrom = (row: CatRow | undefined, matrixExample: string): DecodeCatalogExpectation => {
    const got = decodeCatalogExpectation(row, matrixExample);
    if (got === "skip") throw new Error("synthetic §9 expectation unexpectedly skipped");
    return got;
  };
  const overOnly: CatRow = {
    id: "synthetic.over-only", example: "x = uint", spec: "x = uint", type_name: "X",
    vector: [{ hex: "00", expect: "accept", class: "over-acceptance" }],
  };
  const overPlusSpecValid: CatRow = {
    id: "synthetic.over-plus-valid", example: "x = uint", spec: "x = uint", type_name: "X",
    vector: [
      { hex: "00", expect: "accept", class: "over-acceptance" },
      { hex: "01", expect: "accept" },
    ],
  };

  assertProblem(
    "missing clause",
    evidenceCatalogClauseProblem("synthetic.missing", "probe: cddl-codegen exit 0; ruby=ok rust=ok", { kind: "none" }),
    "NO decode-foreign clause",
  );
  assertProblem(
    "stale no committed with vectors",
    evidenceCatalogClauseProblem(
      "synthetic.stale-no",
      "probe: cddl-codegen exit 0; no committed decode vectors (see catalog); ruby=ok rust=ok",
      { kind: "vectors", count: 10 },
    ),
    "count 10",
  );
  assertProblem(
    "stale accepts count",
    evidenceCatalogClauseProblem(
      "synthetic.stale-count",
      "probe: cddl-codegen exit 0; accepts 10 foreign spec-derived vector(s); ruby=ok rust=ok",
      { kind: "vectors", count: 3 },
    ),
    "count 3",
  );
  assertOk(
    "FAILED with matching count",
    evidenceCatalogClauseProblem(
      "synthetic.failed-ok",
      "probe: cddl-codegen exit 0; foreign-vector decode FAILED (3 vector(s)); ruby=ok rust=ok",
      { kind: "vectors", count: 3 },
    ),
  );
  assertOk(
    "over-acceptance-only vectors excluded",
    evidenceCatalogClauseProblem(
      "synthetic.over-only",
      "probe: cddl-codegen exit 0; no committed decode vectors (see catalog); ruby=ok rust=ok",
      expectFrom(overOnly, "x = uint"),
    ),
  );
  assertOk(
    "over-acceptance excluded from mixed count",
    evidenceCatalogClauseProblem(
      "synthetic.over-plus-valid",
      "probe: cddl-codegen exit 0; accepts 1 foreign spec-derived vector(s); ruby=ok rust=ok",
      expectFrom(overPlusSpecValid, "x = uint"),
    ),
  );
  assertOk(
    "clause present and correct",
    evidenceCatalogClauseProblem(
      "synthetic.correct",
      "probe: cddl-codegen exit 0; accepts 3 foreign spec-derived vector(s); ruby=ok rust=ok",
      { kind: "vectors", count: 3 },
    ),
  );
  for (const p of selfProblems)
    problems.push(`annotation evidence↔catalog synthetic self-test: ${p}`);
}

let evidenceCatalogChecked = 0;
for (const a of supportedAnnotations) {
  const matrixExample = exampleById.get(a.id);
  if (matrixExample === undefined) continue; // impossible for current matrix shape; other checks guard emptiness.
  const expected = decodeCatalogExpectation(catalogById.get(a.id), matrixExample);
  if (expected === "skip") continue; // §2 reports stale catalog examples; avoid double-reporting the row.
  evidenceCatalogChecked++;
  const problem = evidenceCatalogClauseProblem(a.id, a.evidence, expected);
  if (problem !== null) problems.push(problem);
}

// ==================================================================================================
// CORPUS decode-conformance half (composition-depth leg) — mirrors §§ 1-9 where applicable against the
// sibling `tests/decode_conformance/corpus_catalog.toml` and the CURRENT tests/corpus/*.cddl fixtures.
// The obligation set is glob × the SHARED rule enumerator (never a hand list); every active row is
// HOLDER mode. The matrix sections above are untouched — this is an independent block feeding the same
// `problems` array and the same final report/exit.
// ==================================================================================================

// The EXACT (corpus row id -> sorted arm classes) set the shared resolver fires on at HEAD (decay pin,
// the matrix § 7 EXPECTED_FLOOR_SCOPE pattern): a resolver change that silently widens/narrows the
// in-scope set fails got/want. Classes are majors-0/1-merged ("int"). Pinned from the mint's actuals.
const CORPUS_EXPECTED_FLOOR_SCOPE: Record<string, string[]> = {
  // Declared-spelling fixture: `delta_coin = int` is the one type choice among its rules (an alias of
  // a nominal, there so the member's deserialize CALL TARGET is exercised) — the uint/nint arm merges
  // to "int", same single-class shape as `int_alias.bare_int`. Its alias/table/record siblings are not
  // type choices and so are out of floor scope.
  "alias_positions.delta_coin": ["int"],
  "bool_choice.bool_or_text": ["3", "7"],   // `bool / tstr` — tstr → major 3, bool → major 7
  "bool_choice.uint_or_bool": ["7", "int"], // `uint / bool` — uint → int (0/1-merged), bool → major 7
  "c_style_enum.fixed_enum": ["int"],
  "c_style_enum_map_key.fixed_enum": ["int"],
  "c_style_enum_newtype.fixed_enum": ["int"],
  // A c-style enum used as the CONTENT of encoding-carrying wrappers (`bytes .cbor` / a tag). Only
  // the enum RULE is a type choice and so in floor scope; its holder is a record. All three arms are
  // fixed uints, so the whole choice merges to one class — the same single-class shape as every
  // other `fixed_enum` row above; the wrappers ride on the HOLDER, not on the arms.
  "cbor_enum_payload.payload_enum": ["int"],
  "dsl_used_as_key_hash_cstyle.fixed_enum": ["int"],
  // Fixed bool/null in CHOICE-ARM position under --preserve-encodings. Only the TYPE-choice rule is
  // in floor scope; the fixture's map-rep/array-rep group-choice rules are not type choices. The
  // fixed `true` arm is a wire-level bool (major 7) beside the tstr arm (major 3) — same two classes
  // as `bool_choice.bool_or_text`, which spells the arm as the `bool` TYPE rather than the literal.
  "group_choice_fixed_special.type_choice_bool": ["3", "7"],
  "int_alias.bare_int": ["int"],   // `bare_int = int` — the uint/nint arm merges to "int"
  "nullable.maybe_text": ["3", "7"],
  "nullable_nested.data_enum": ["3", "int"],
  "nullable_nested.maybe_coll": ["7"],   // `nums_arr / null` — the array arm is exempt (a named collection), null → 7
  "nullable_nested.maybe_denum": ["7"],  // `data_enum / null` — the enum arm is exempt (a named choice), null → 7
  "nullable_nested.maybe_uint": ["7", "int"],
  // Open-table corpus fixture: its catch-all's key/value type `md = uint / text` is the one type
  // choice among the fixture's rules (the three open-table rules are records) — uint → "int"
  // (majors 0/1 merged), text → major 3. Pinned from the mint's actuals.
  "open_table.md": ["3", "int"],
  "table_enum_key.ikey": ["int"],
  // `@duplicates preserve` corpus fixture (matrix-registration packet): the recursive metadatum
  // union `md = mdmap / [* md] / int / bytes / text`. The mdmap arm is exempt (a named table);
  // the remaining arms pin per wire class: bytes → 2, text → 3, the anonymous `[* md]` array → 4,
  // and int → "int" (majors 0/1 merged). Pinned from the mint's actuals.
  "table_preserve.md": ["2", "3", "4", "int"],
  // REQUEST-08 tag-set rows: the CDDL still SPELLS two arms (`#6.258([..]) / [..]` — tag → major 6,
  // array → major 4) even though codegen collapses the idiom to one transparent type, so the floor
  // demands an accept vector per WIRE arm — exactly the both-arms decode evidence the collapse needs.
  // The near-miss row stays a real two-arm choice (mismatched bounds) with the same two majors.
  "tag_set_idiom.int_neset": ["4", "6"],
  "tag_set_idiom.int_set": ["4", "6"],
  "tag_set_idiom.text_set": ["4", "6"],
  "tag_set_near_miss.near_miss": ["4", "6"],
  // `@duplicates reject` set idiom (WP2): same two-arm WIRE spelling as the preserve idiom above
  // (`#6.258([..]) / [..]`) — the policy is invisible to the oracle, so the same per-arm floor holds.
  "tag_set_reject.int_neset": ["4", "6"],
  "tag_set_reject.int_set": ["4", "6"],
  "tag_set_reject.text_set": ["4", "6"],
  // Stacked-tag fixture: `heterogeneous_set` is itself the two-arm idiom spelling (tag → 6,
  // array → 4); the outer-tag rules wrapping it are single-arm and out of floor scope.
  "double_tag.heterogeneous_set": ["4", "6"],
  // Well-known-tag registry fixture: the no-directive rules and the explicit-preserve opt-out all
  // keep the two-arm WIRE spelling (`#6.258([..]) / [..]`) — the registry default, like the
  // directive, is invisible to the oracle, so the same per-arm floor holds. `single_arm` is
  // single-arm (tag only) and out of floor scope.
  "tag_set_default.default_neset": ["4", "6"],
  "tag_set_default.default_set": ["4", "6"],
  "tag_set_default.opt_out": ["4", "6"],
  "type_choice.type_choice": ["2", "3", "int"],
};
const CORPUS_MIN_FIXTURES = 55;   // Conservative decay floor; the exact current count prints below.
const CORPUS_MIN_ROWS = 120;      // Same for the derived (fixture, rule) obligation set.

const CORPUS_CATALOG_REL = "tests/decode_conformance/corpus_catalog.toml";
const CORPUS_DIR = `${HERE}/../tests/corpus`;
const corpusProblems: string[] = [];

function missingCorpusRowProblem(id: string, fixture: string): string {
  return `enumerated corpus (fixture, rule) \`${id}\` has no catalog row (mint it or pin it) — ` +
    `run \`bun run verify.ts --mint-decode-corpus --only=${fixture}\``;
}

// --- self-checks for the shared enumerator / closure builder (mirrors the § 8/§ 9 pattern):
// a synthetic multi-rule sample exercising strings, comments, generics, and hyphens, so a regression in
// the ONE shared implementation the mint and this gate both call is caught here regardless of what the
// committed corpus happens to exercise.
{
  const selfProblems: string[] = [];
  const SAMPLE = [
    "; leading file comment — dropped (spans start at the first head)",
    'lit-holder = [k: "he\\"llo; not a comment", g: gen<uint>]  ; @name Foo',
    "gen<t> = [t]",
    "unref = uint",  // defined but never referenced from lit-holder → not in its closure
  ].join("\n");
  const rules = enumerateCorpusRules(SAMPLE);
  const names = rules.map(r => `${r.name}${r.generic ? "<>" : ""}`).join(",");
  if (names !== "lit-holder,gen<>,unref")
    selfProblems.push(`enumerator names/generic-flags drifted: got ${JSON.stringify(names)} (want "lit-holder,gen<>,unref")`);
  // The `; not a comment` inside the string, and the `-` in `lit-holder`, must not corrupt spans/refs.
  const closure = dependencyClosure("lit-holder", rules).map(r => r.name).join(",");
  if (closure !== "lit-holder,gen")  // fixture order; `unref` excluded (never referenced), string content ignored
    selfProblems.push(`closure drifted: got ${JSON.stringify(closure)} (want "lit-holder,gen" — string content must not add a ref, unref must be excluded)`);
  const missingHint = missingCorpusRowProblem("new-fixture.rule", "new-fixture");
  if (!missingHint.includes("--mint-decode-corpus --only=new-fixture") ||
      missingHint.includes("--mint-decode-corpus`") || missingHint.includes("--only=new-fixture.rule"))
    selfProblems.push(`missing-row diagnostic lost its fixture-scoped mint command: ${JSON.stringify(missingHint)}`);
  for (const p of selfProblems) corpusProblems.push(`corpus enumerator/closure self-test: ${p}`);
}

// --- enumerate the CURRENT fixtures (the obligation-set source of truth) --------------------------
interface CorpusEnumRow { fixture: string; rule: CorpusRule; allRules: CorpusRule[] }
const corpusEnum = new Map<string, CorpusEnumRow>();   // id -> enumeration
const corpusFixtureStems = new Set<string>();
{
  const files = existsSync(CORPUS_DIR) ? readdirSync(CORPUS_DIR).filter(f => f.endsWith(".cddl")).sort() : [];
  for (const file of files) {
    const stem = file.replace(/\.cddl$/, "");
    corpusFixtureStems.add(stem);
    const allRules = enumerateCorpusRules(readFileSync(join(CORPUS_DIR, file), "utf8"));
    for (const rule of allRules) {
      // Prelude-name collision assert (the mint's enumeration-time check, mirrored here so the gate
      // fails loud on the day a colliding rule lands, not at the next mint): a corpus rule named like a
      // prelude type would make reference extraction ambiguous.
      if (PRELUDE_NAMES.has(rule.name))
        corpusProblems.push(`corpus fixture \`${stem}.cddl\` rule \`${rule.name}\` collides with a prelude type name — reference extraction would be ambiguous (rename the rule)`);
      corpusEnum.set(`${stem}.${rule.name}`, { fixture: stem, rule, allRules });
    }
  }
}

// --- read the committed corpus catalog ------------------------------------------------------------
const corpusCatalogText = existsSync(`${HERE}/../${CORPUS_CATALOG_REL}`) ? readFileSync(`${HERE}/../${CORPUS_CATALOG_REL}`, "utf8") : null;
if (corpusCatalogText === null) {
  corpusProblems.push(`${CORPUS_CATALOG_REL} is missing — run \`bun run verify.ts --mint-decode-corpus\` to mint it`);
}
const corpusCatalog = corpusCatalogText !== null ? (Bun.TOML.parse(corpusCatalogText) as { row?: CatRow[] }) : { row: [] };
const corpusRows = corpusCatalog.row ?? [];
const corpusById = new Map<string, CatRow>();

// --- §8 corpus: writer↔reader identity (the SOLE serializer round-trips the committed bytes) -------
if (corpusCatalogText !== null) {
  const recomposed = composeCatalog([...parseCatalogContent(corpusCatalogText).values()], CORPUS_CATALOG_INTRO);
  if (recomposed !== corpusCatalogText) {
    const a = corpusCatalogText.split("\n"), b = recomposed.split("\n");
    let i = 0;
    while (i < a.length && i < b.length && a[i] === b[i]) i++;
    corpusProblems.push(
      `corpus catalog writer↔reader identity: compose(parse(corpus_catalog.toml)) is NOT byte-identical — first divergence at line ${i + 1}:\n` +
        `    committed  : ${a[i] === undefined ? "<EOF>" : JSON.stringify(a[i])}\n    recomposed : ${b[i] === undefined ? "<EOF>" : JSON.stringify(b[i])}\n` +
        `    fix the writer, hand-align to writer-canonical form, or re-mint that row.`,
    );
  }
}

const CORPUS_HOLDER_PREFIX = `${CORPUS_HOLDER_RULE} = [0, `;
const corpusFloorScope: Record<string, string[]> = {};
const corpusUncoveredInScope = new Set<string>();

for (const r of corpusRows) {
  const id = typeof r.id === "string" ? r.id : undefined;
  if (id === undefined) { corpusProblems.push(`corpus row is missing a string \`id\`: ${JSON.stringify(r)}`); continue; }
  if (corpusById.has(id)) corpusProblems.push(`duplicate corpus catalog row id \`${id}\``);
  corpusById.set(id, r);

  const vectors = r.vector ?? [];
  const hasVectors = vectors.length > 0;
  const pinned = typeof r.pinned_reason === "string" && r.pinned_reason.length > 0;

  // §1 completeness: exactly one of {>=1 vector, pinned_reason}.
  if (hasVectors === pinned)
    corpusProblems.push(`\`${id}\`: a corpus row must have EITHER >=1 vector OR a nonempty pinned_reason, not ${hasVectors ? "both" : "neither"}`);

  // §1/§2: fixture/rule provenance present and consistent with the id + still enumerated.
  const fixture = typeof r.fixture === "string" ? r.fixture : undefined;
  const rule = typeof r.rule === "string" ? r.rule : undefined;
  if (fixture === undefined || rule === undefined) {
    corpusProblems.push(`\`${id}\`: corpus row must carry \`fixture\` and \`rule\` provenance (got fixture=${JSON.stringify(fixture)} rule=${JSON.stringify(rule)})`);
  } else {
    if (`${fixture}.${rule}` !== id) corpusProblems.push(`\`${id}\`: id must equal "<fixture>.<rule>" but fixture=${JSON.stringify(fixture)} rule=${JSON.stringify(rule)}`);
    if (!corpusFixtureStems.has(fixture)) corpusProblems.push(`\`${id}\`: fixture \`${fixture}\` no longer exists in tests/corpus/ (remove or re-mint)`);
  }
  const enumRow = corpusEnum.get(id);
  if (!enumRow) corpusProblems.push(`\`${id}\`: (fixture, rule) is no longer enumerated from tests/corpus/ (construct removed/renamed — re-mint the corpus catalog)`);

  if (pinned) {
    for (const f of ["spec", "mode", "type_name"] as const)
      if (r[f] !== undefined) corpusProblems.push(`\`${id}\`: pinned corpus row must not carry \`${f}\``);
    // §2 staleness applies to PINNED rows too (example half only — a pinned row carries no spec): a
    // fixture edit that changes a pinned row's closure may invalidate the pin's justification (e.g. a
    // ruby-unparseable controller removed), and without this check the stale pin stays green until
    // someone happens to re-mint.
    if (enumRow && rule !== undefined && typeof r.example === "string") {
      const wantExample = corpusClosureBody(rule, enumRow.allRules);
      if (r.example !== wantExample)
        corpusProblems.push(`\`${id}\`: PINNED row's committed example drifted from the fixture reconstruction — the pin's justification may be stale; re-mint (\`bun run verify.ts --mint-decode-corpus --only=${id}\`)`);
    }
    continue;
  }
  if (!hasVectors) continue;

  // §3 shape: an active corpus row is ALWAYS holder mode with a reconstructable spec.
  const spec = typeof r.spec === "string" ? r.spec : undefined;
  const mode = typeof r.mode === "string" ? r.mode : undefined;
  const typeName = typeof r.type_name === "string" ? r.type_name : undefined;
  if (mode !== "holder") corpusProblems.push(`\`${id}\`: corpus rows are always holder mode (got ${JSON.stringify(mode)})`);
  if (typeName !== "ProbeHolder") corpusProblems.push(`\`${id}\`: holder-mode type_name must be "ProbeHolder" (got ${JSON.stringify(typeName)})`);
  if (spec !== undefined && !spec.startsWith(CORPUS_HOLDER_PREFIX)) corpusProblems.push(`\`${id}\`: holder-mode spec must start with \`${CORPUS_HOLDER_PREFIX}\``);

  // §2 spec-reconstruction staleness: the committed spec/example must byte-equal a fresh reconstruction
  // from the CURRENT fixture via the shared enumerator/closure builder. A drifted fixture reads "re-mint".
  if (enumRow && rule !== undefined) {
    const wantSpec = corpusProbeSpec(rule, enumRow.allRules);
    const wantExample = corpusClosureBody(rule, enumRow.allRules);
    if (spec !== undefined && spec !== wantSpec)
      corpusProblems.push(`\`${id}\`: committed spec drifted from the fixture reconstruction — re-mint\n    committed: ${JSON.stringify(spec)}\n    fixture  : ${JSON.stringify(wantSpec)}`);
    if (typeof r.example === "string" && r.example !== wantExample)
      corpusProblems.push(`\`${id}\`: committed example drifted from the fixture reconstruction — re-mint`);
  }

  // §3 per-vector shape (corpus subset; the matrix loop owns the matrix catalog).
  vectors.forEach((v, i) => {
    const where = `\`${id}\` vector[${i}]`;
    const hex = typeof v.hex === "string" ? v.hex : undefined;
    if (hex === undefined || hex.length === 0) corpusProblems.push(`${where}: missing/empty hex`);
    else if (hex.length % 2 !== 0) corpusProblems.push(`${where}: hex \`${hex}\` has odd length`);
    else if (!HEX_RE.test(hex)) corpusProblems.push(`${where}: hex \`${hex}\` is not lowercase hex`);
    const expect = v.expect;
    if (expect !== "accept" && expect !== "reject") corpusProblems.push(`${where}: \`expect\` must be "accept" or "reject" (got ${JSON.stringify(expect)})`);
    // Holder-array preamble: every vector is an instance of `[0, <rule>]`, so it begins with a CBOR
    // array head (major 4) whose first element is the literal 0. A NORMAL rule gives `8200…`; a bare
    // GROUP rule SPLICES its members, widening the array count (`8300…` for a 2-member group), so the
    // check is "major-4 head + `00` first element", NOT a literal `8200` (the plan's shorthand).
    if (typeof hex === "string" && hex.length >= 4) {
      const major = parseInt(hex.slice(0, 2), 16) >> 5;
      if (major !== 4 || hex.slice(2, 4) !== "00")
        corpusProblems.push(`${where}: holder vector \`${hex}\` must begin with a CBOR array head (major 4) and the literal 0 first element (\`8200…\`, or \`8N00…\` for a spliced bare group)`);
    }
    if (expect === "accept") {
      if (v.class !== undefined && v.class !== "over-acceptance")
        corpusProblems.push(`${where}: an accept vector may carry no class or class="over-acceptance" (got ${JSON.stringify(v.class)})`);
      if (v.class === "over-acceptance" && (typeof v.reason !== "string" || (v.reason as string).length === 0))
        corpusProblems.push(`${where}: class="over-acceptance" vector needs a nonempty \`reason\``);
    }
    if (expect === "reject") {
      if (v.class !== "bug" && v.class !== "limitation" && v.class !== "constraint")
        corpusProblems.push(`${where}: reject vector \`class\` must be "bug", "limitation" or "constraint" (got ${JSON.stringify(v.class)}) — a class-less reject is triage-pending`);
      if (typeof v.reason !== "string" || (v.reason as string).length === 0)
        corpusProblems.push(`${where}: reject vector needs a nonempty \`reason\``);
    }
    if (v.class === "constraint" && expect === "reject") {
      if (typeof v.expect_err !== "string" || (v.expect_err as string).length === 0)
        corpusProblems.push(`${where}: class="constraint" vector needs a nonempty \`expect_err\``);
    } else if (v.expect_err !== undefined) {
      corpusProblems.push(`${where}: only class="constraint" reject vectors may carry \`expect_err\` (got ${JSON.stringify(v.expect_err)})`);
    }
  });

  // §7 arm-coverage floor for choice rows — resolve against the TARGET-FIRST closure example so the
  // root scan lands on the target rule's RHS. All corpus rows are holder (strip the array preamble).
  if (enumRow && rule !== undefined) {
    const res = resolveChoiceArmClasses(corpusArmExample(rule, enumRow.allRules));
    const specValidAccepts = vectors.filter(
      v => v.expect === "accept" && v.class !== "over-acceptance" && typeof v.hex === "string" && (v.hex as string).length >= 6,
    );
    if (res && specValidAccepts.length > 0) {
      corpusFloorScope[id] = res.classes;
      const covered = new Set(specValidAccepts.map(v => vectorShapeClass(v.hex as string, true)));
      for (const cls of res.classes) if (!covered.has(cls)) corpusUncoveredInScope.add(`${id}/${cls}`);
    }
  }
}

// §7 scope decay pin + coverage floor + stale-exempt guard (corpus, mirroring the matrix § 7 trio).
// The exemption ledger is the CORPUS one (CORPUS_DECODE_FLOOR_ARM_EXEMPT, lib.ts): the two ledgers are
// SEPARATE because each stale-guard iterates its own uncovered-in-scope set — a corpus-keyed entry in
// the matrix ledger could never match the matrix guard's uncovered set and would falsely read stale.
{
  const got = JSON.stringify(Object.fromEntries(Object.keys(corpusFloorScope).sort().map(k => [k, corpusFloorScope[k]])));
  const want = JSON.stringify(Object.fromEntries(Object.keys(CORPUS_EXPECTED_FLOOR_SCOPE).sort().map(k => [k, CORPUS_EXPECTED_FLOOR_SCOPE[k]])));
  if (got !== want) corpusProblems.push(`corpus arm-coverage floor scope drifted:\n    got : ${got}\n    want: ${want}`);
  for (const key of [...corpusUncoveredInScope].sort()) {
    if (Object.hasOwn(CORPUS_DECODE_FLOOR_ARM_EXEMPT, key)) continue;
    const [id, cls] = key.split("/");
    corpusProblems.push(`\`${id}\`: choice arm class "${cls}" has ZERO spec-valid accept vector(s) (required {${(corpusFloorScope[id] ?? []).join(", ")}}) — re-mint \`bun run verify.ts --mint-decode-corpus --only=${id}\` or add a cited CORPUS_DECODE_FLOOR_ARM_EXEMPT entry for a genuine oracle gap`);
  }
  // Stale-exempt guard (the matrix trio's third leg, corpus side): every ledgered (row, class) must
  // still be a genuinely-uncovered in-scope pair; an entry for a now-covered / out-of-scope pair is
  // stale and fails the gate, forcing the re-mint when the underlying oracle gap closes.
  for (const key of Object.keys(CORPUS_DECODE_FLOOR_ARM_EXEMPT).sort())
    if (!corpusUncoveredInScope.has(key))
      corpusProblems.push(`CORPUS_DECODE_FLOOR_ARM_EXEMPT names \`${key}\` which is no longer a genuinely-uncovered in-scope arm class (covered now, or the row left the floor's scope) — stale ledger entry, remove it`);
}

// §1 completeness: every enumerated (fixture, rule) has a catalog row. The repair command scopes
// to the missing fixture: an additive full-catalog mint re-rolls every random vector and is both
// slower and destructive of committed evidence diversity.
for (const id of [...corpusEnum.keys()].sort())
  if (!corpusById.has(id))
    corpusProblems.push(missingCorpusRowProblem(id, corpusEnum.get(id)!.fixture));

// §5 vacuity floors: implausibly small enumeration/catalog reads must fail loud, not pass empty.
if (corpusFixtureStems.size < CORPUS_MIN_FIXTURES)
  corpusProblems.push(`only ${corpusFixtureStems.size} corpus fixtures enumerated (expected >= ${CORPUS_MIN_FIXTURES}) — glob/enumerator read looks broken`);
if (corpusEnum.size < CORPUS_MIN_ROWS)
  corpusProblems.push(`only ${corpusEnum.size} enumerated corpus (fixture, rule) rows (expected >= ${CORPUS_MIN_ROWS}) — enumerator read looks broken`);

for (const p of corpusProblems) problems.push(p);

// --- report ---------------------------------------------------------------------------------------
const activeRows = rows.filter(r => (r.vector ?? []).length > 0);
const pinnedRows = rows.filter(r => typeof r.pinned_reason === "string" && r.pinned_reason.length > 0);
const allVectors = activeRows.flatMap(r => r.vector ?? []);
const overAccepts = allVectors.filter(v => v.expect === "accept" && v.class === "over-acceptance").length;
const accepts = allVectors.filter(v => v.expect === "accept").length;
const rejects = allVectors.filter(v => v.expect === "reject");
const rejectBug = rejects.filter(v => v.class === "bug").length;
const rejectLimitation = rejects.filter(v => v.class === "limitation").length;
const rejectConstraint = rejects.filter(v => v.class === "constraint").length;
const constraintWithExpectErr = rejects.filter(v => v.class === "constraint" && typeof v.expect_err === "string" && v.expect_err.length > 0).length;

if (problems.length) {
  console.log(`decode-conformance drift gate: ${problems.length} problem(s)`);
  for (const p of problems) console.log(`  FAIL ${p}`);
  process.exit(1);
}
const corpusActive = corpusRows.filter(r => (r.vector ?? []).length > 0);
const corpusPinned = corpusRows.filter(r => typeof r.pinned_reason === "string" && (r.pinned_reason as string).length > 0);
const corpusVectorCount = corpusActive.reduce((n, r) => n + (r.vector ?? []).length, 0);
console.log(
  `decode-conformance catalog OK — ${rows.length} rows (${activeRows.length} active / ${allVectors.length} vectors: ` +
    `${accepts} accept [${overAccepts} over-acceptance], ${rejects.length} reject) · ${pinnedRows.length} pinned · ` +
    `reject vectors: ${rejectBug} bug, ${rejectLimitation} limitation, ${rejectConstraint} constraint (${constraintWithExpectErr} with expect_err) · ` +
    `${Object.keys(floorScope).length} arm-coverage-floor rows (${Object.keys(DECODE_FLOOR_ARM_EXEMPT).length} ledgered-exempt arm class) · ` +
    `evidence↔catalog clauses coherent on ${evidenceCatalogChecked} supported rows · ${supported.size} supported matrix rows`,
);
console.log(
  `corpus decode-conformance OK — ${corpusFixtureStems.size} fixtures enumerated / ${corpusEnum.size} obligation rows · ` +
    `${corpusRows.length} catalog rows (${corpusActive.length} active / ${corpusVectorCount} vectors · ${corpusPinned.length} pinned) · ` +
    `${Object.keys(corpusFloorScope).length} arm-coverage-floor rows (${Object.keys(CORPUS_DECODE_FLOOR_ARM_EXEMPT).length} ledgered-exempt arm class)`,
);
process.exit(0);
