#!/usr/bin/env bun
/**
 * Q4 directional-support query (QUERIES.md Q4) — PURE FILE READS, no cargo, no oracles.
 *
 * Projects the per-direction evidence F3 already grounded (see README.md § "Directional support
 * evidence") into Q4's 5-way answer
 * **{accept, encode, decode, round-trip, enforce-constraint}**, one row per modelled construct. It reads
 * exactly two committed files and derives — it never probes:
 *   1. `matrix.json` — `annotations.cddl_codegen` (`{id, status, evidence}`, the execution-gated verdict
 *      + its machine-generated evidence string) and `features`/`containment`/`control_operators` (the
 *      construct universe + per-id `example`, used to bucket each row by axis).
 *   2. `tests/decode_conformance/catalog.toml` — the decode corpus: spec-derived CBOR our own encoder
 *      never produced. `expect="accept"` vectors are the encoder-INDEPENDENT decode evidence. Reject
 *      vectors split by class: `class="constraint"` = spec-INVALID CBOR that violates a constraint the
 *      row enforces and the decoder DURABLY rejects — THE bounded-reject enforcement evidence; `class` ∈
 *      {bug, limitation} = spec-VALID CBOR the decoder WRONGLY rejects (a wrong-rejection pin, pruned
 *      when the gap closes) — NOT enforcement evidence, so it never projects enforce=yes.
 *
 * The 5-way derivation (per annotation row):
 *   - **accept**  — from status + evidence: an `exit 0`/`standalone-compile N/A` supported/uncertain row
 *     is `yes`; `out_of_profile` is `no (out of profile)`; an unsupported row that `generates but does
 *     not compile` is `partial (generates)`; a panic / parse-reject is `no`.
 *   - **round-trip** — `round-trips` (incl. "when embedded", annotated `(embedded)`) is `yes`; a shape
 *     with no minted round-trip surface / `standalone-compile N/A` is `n/a (no surface)`; else `no`.
 *   - **encode**  — the ENCODE HALF of round-trip: our generated encoder produced bytes that decoded back,
 *     so `encode = yes iff round-trip = yes`, mirroring round-trip's n/a / no otherwise.
 *   - **decode**  — two tiers, catalog is the strong source: `expect="accept"` vectors ⇒
 *     `yes (foreign: N)` (the encoder-INDEPENDENT signal, the whole point of Q4); else a round-tripping
 *     row is `yes (via round-trip)` (weak — conflated with encode); else mirrors round-trip.
 *   - **enforce-constraint** — a `class="over-acceptance"` accept vector DOMINATES ⇒ `no (over-accepts: M)`
 *     (a certified spec-INVALID instance the decoder wrongly accepts — an enforcement claim with a proven
 *     hole is not "yes", and it beats both `yes` and `unverified`); else `class="constraint"` reject
 *     vectors ⇒ `yes (bounded-reject: N)`; else a supported enforcement-bearing row (`ctl.*` except
 *     `ctl.default`, plus `memberkey.cut` — Q4's prose names `.size`/cut enforcement) is
 *     `unverified (no reject vector)` — an honest gap, NOT `yes`; else `n/a` (the construct carries no
 *     rejectable constraint — e.g. `ctl.default` governs an absent field).
 *
 * THE ENCODE/DECODE ASYMMETRY (the honest label — read before trusting a column):
 *   - **decode** has INDEPENDENT per-construct evidence: the `catalog.toml` foreign vectors are CBOR our
 *     own encoder never emitted, so a `yes (foreign: N)` decode fact is not conflated with encode. This
 *     is exactly the direction a round-trip collapses away, and the reason Q4 splits the two.
 *   - **encode** has NO independent per-construct oracle. Its only independent evidence is CORPUS-LEVEL
 *     (`golden_hex`, the `ir_conformance_corpus`), not keyed per Q4 row. So this script reports encode
 *     strictly as the round-trip half — it does NOT claim a stronger per-construct encode fact than
 *     round-trip supports. A `decode: yes (foreign)` row with `encode: yes` means "decodes foreign bytes
 *     AND round-trips its own", not "we independently verified encode against a foreign fixture".
 *
 * A NOTE ON `enforce = yes` AND THE NUMERIC-OP GAP: enforcement evidence is a `class="constraint"` reject
 * vector — spec-INVALID CBOR whose ONLY invalidity is the constraint itself (the instance is valid for
 * the base type), durably rejected by the generated decoder. Spec-invalidity is normally certified by
 * BOTH oracles at mint time; an oracle that does not implement the rule at all cannot join that
 * consensus, so a vector may instead be certified by the remaining oracle plus a per-vector
 * `DECODE_REJECT_ORACLE_GAP_EXEMPT` entry (lib.ts) citing a committed spec argument. The tag-payload
 * and float-value families each have narrow rust-only entries; both halves of every entry are
 * stale-guarded. The detailed vacuity ledger below names the exact current vector families and the
 * pinned green set. Control operators, fixed selectors, and the remaining constraint-bearing rows all
 * follow the same rule: an in-base-type violation needs a reason-pinned generated rejection, while
 * `ctl.default` remains non-rejectable because it governs an absent field.
 *
 * Run from cddl-matrix/:
 *   bun run query_q4_directional.ts            -> the full Q4 table (grouped by axis) + the asymmetry footnote
 *   bun run query_q4_directional.ts ctl.size   -> only rows whose id contains "ctl.size" (Q4's "for construct C")
 *   bun run query_q4_directional.ts --check     -> consistency invariants + vacuity floor only; exit nonzero on any
 */
import { readFileSync } from "node:fs";

const HERE = import.meta.dir;
const CATALOG_REL = "tests/decode_conformance/catalog.toml";

// --- matrix.json ----------------------------------------------------------------------------------
interface Annotation { id: string; status: string; evidence: string }
/** `role`/`feature` are carried by containment rows only — a cell IS a (role × feature) pair. */
interface UnivRow { id: string; role?: string; feature?: string }
interface MatrixJson {
  annotations: { cddl_codegen: Annotation[] };
  features: UnivRow[];
  containment: UnivRow[];
  control_operators: UnivRow[];
}
const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as MatrixJson;
const annotations = matrix.annotations.cddl_codegen;
const statusById = new Map(annotations.map(a => [a.id, a.status]));

// Axis buckets, keyed by which universe array owns the id (an annotation id is in exactly one).
type Axis = "feature" | "containment-cell" | "control-op";
const axisById = new Map<string, Axis>();
for (const r of matrix.features) axisById.set(r.id, "feature");
for (const r of matrix.containment) axisById.set(r.id, "containment-cell");
// A containment cell's enforcement class is its FEATURE's, read in the POSITION its role names — both
// out of the universe row rather than pattern-matched off the id. A role name carries dots and a
// variant suffix can spell anything, so string surgery on `contain.<role>.<feature>[.<variant>]` is a
// guess where the row is an answer.
const cellRole = new Map(matrix.containment.map(r => [r.id, r.role]));
const cellFeature = new Map(matrix.containment.map(r => [r.id, r.feature]));
for (const r of matrix.control_operators) axisById.set(r.id, "control-op");

// --- catalog.toml: per-id accept / classified-reject vector counts --------------------------------
interface CatVector { expect?: unknown; class?: unknown; reason?: unknown }
interface CatRow { id?: unknown; vector?: CatVector[] }
const catalog = Bun.TOML.parse(readFileSync(`${HERE}/../${CATALOG_REL}`, "utf8")) as { row?: CatRow[] };
const acceptVecById = new Map<string, number>();
// constraintVecById: ONLY class="constraint" reject vectors — spec-INVALID CBOR that violates a
// constraint the row enforces and the decoder DURABLY rejects. This is the enforcement evidence.
// bug/limitation reject vectors are the OPPOSITE (spec-VALID CBOR the decoder wrongly rejects — a
// wrong-rejection pin, pruned when fixed); they are NOT enforcement evidence and must not project
// enforce=yes (the conflation this query previously carried).
const constraintVecById = new Map<string, number>();
// overAcceptVecById: ONLY class="over-acceptance" accept vectors — spec-INVALID CBOR the decoder
// CURRENTLY (wrongly) accepts (a certified silent-acceptance pin, no fix yet). This is NOT foreign
// decode evidence (so it is excluded from acceptVecById), and it DOMINATES the enforce projection: a
// row carrying one projects `enforce = no (over-accepts)`, the honest fact that an enforcement claim
// with a certified hole is not "yes".
const overAcceptVecById = new Map<string, number>();
for (const r of catalog.row ?? []) {
  const id = typeof r.id === "string" ? r.id : undefined;
  if (id === undefined) continue;
  let accepts = 0;
  let constraints = 0;
  let overAccepts = 0;
  for (const v of r.vector ?? []) {
    if (v.expect === "accept" && v.class === "over-acceptance") overAccepts++;
    else if (v.expect === "accept") accepts++;
    else if (v.expect === "reject" && v.class === "constraint") constraints++;
  }
  if (accepts) acceptVecById.set(id, accepts);
  if (constraints) constraintVecById.set(id, constraints);
  if (overAccepts) overAcceptVecById.set(id, overAccepts);
}
const anyConstraintVectors = constraintVecById.size > 0;

// --- the 5-way derivation -------------------------------------------------------------------------
interface Directional {
  id: string;
  axis: Axis;
  status: string;
  evidence: string;
  accept: string;
  encode: string;
  decode: string;
  roundTrip: string;
  enforce: string;
  note: string; // a short evidence snippet for the table's trailing column
}

// The machine-generated evidence vocabulary is stable (verify.ts); match its exact phrasings.
function deriveAccept(status: string, ev: string): string {
  if (status === "out_of_profile") return "no (out of profile)";
  if (status === "supported" || status === "uncertain") {
    if (ev.includes("exit 0") || ev.includes("standalone-compile N/A")) return "yes";
    return "no";
  }
  // unsupported (or any other): distinguish generates-but-fails-compile from panic/reject.
  if (ev.includes("generates but does not compile")) return "partial (generates)";
  return "no";
}

// Round-trip is a RUST fact, so the derivation reads only the RUST SEGMENT of the evidence —
// everything before the first corroborating clause. The corroborating legs (wasm, component,
// decode-foreign) append to the same string and several of them speak the same words the rust
// verdict does ("round-trips", "round-trip FAILED"), so matching over the whole string would let a
// sibling leg's clause decide this projection: a row whose rust round-trip failed reads `yes` the
// moment any leg reports its own success. Anchoring costs nothing (the projection is byte-identical
// at HEAD) and removes the coupling.
const CORROBORATING_CLAUSE = /; (?:wasm |component |accepts \d|foreign-vector decode |no committed decode vectors|ruby=)/;
function rustSegment(ev: string): string {
  const m = CORROBORATING_CLAUSE.exec(ev);
  return m ? ev.slice(0, m.index) : ev;
}

function deriveRoundTrip(evidence: string): string {
  const ev = rustSegment(evidence);
  if (/round-trip FAILED|round-trips=fail/.test(ev)) return "no";
  if (ev.includes("round-trips when embedded")) return "yes (embedded)";
  if (ev.includes("round-trips=n/a") || ev.includes("no minted round-trip surface")) return "n/a (no surface)";
  if (ev.includes("standalone-compile N/A")) return "n/a (no surface)";
  if (ev.includes("round-trips=ok") || ev.includes("round-trips")) return "yes";
  return "no";
}

// encode = the ENCODE HALF of round-trip; there is NO independent per-construct encode oracle (see header).
function deriveEncode(roundTrip: string): string {
  if (roundTrip.startsWith("yes")) return "yes";
  if (roundTrip.startsWith("n/a")) return "n/a";
  return "no";
}

function deriveDecode(id: string, evidence: string, roundTrip: string): string {
  // verify.ts's own replay outcome wins over the catalog count: a recorded per-row decode failure
  // on the committed foreign vectors ("; foreign-vector decode FAILED (N vector(s))") must not
  // project as an independent decode fact just because the catalog SHIPS accept vectors.
  if (evidence.includes("foreign-vector decode FAILED")) return "no (foreign decode FAILED)";
  const n = acceptVecById.get(id);
  if (n) return `yes (foreign: ${n})`; // encoder-INDEPENDENT — the point of Q4
  if (roundTrip.startsWith("yes")) return "yes (via round-trip)"; // weak: conflated with encode
  if (roundTrip.startsWith("n/a")) return "n/a";
  return "no";
}

// The enforcement-bearing rows: the ctl.* axis plus the cut feature — Q4's prose names exactly
// "`.size`/cut enforcement" as where generators cut corners, so cut must not read n/a (no constraint).
// Also enforcement-bearing (the variation-row classification-gap lesson (roadmap.toml § Expansion) — an enforced constraint invisible as
// n/a is indistinguishable from "carries no constraint", so a vector loss would silently shed
// enforcement evidence instead of reading as a gap):
//   - `occur.bounded*` — occurrence bounds are a rejectable count constraint (the generated decoder's
//     Vec len check);
//   - `value.*` fixed-literal feature rows — a fixed value is a rejectable equality constraint. The
//     classification matters even when a row already has a reject vector: a future supported,
//     vectorless literal kind must read `unverified`, not silently decay to n/a. The member forms are
//     the CONTAINMENT CELLS carrying those
//     features, classified by `cellCarriesConstraint` below — the FEATURE-id tests here match no
//     `contain.*` id, so before that branch existed the classification the sentence claims was
//     absent from the code, and every fixed-value cell derived `n/a (no constraint)`.
// EXCEPTION: `ctl.default` governs an ABSENT field (`? b: uint .default 0` supplies a value when the
// member is missing) — there is no rejectable instance, so it carries no enforcement constraint and
// derives n/a rather than an honest-gap `unverified`.
// (`ctl.size.uint` once sat here as a KNOWN-UNENFORCED `unverified` exception — the member decode
// silently truncated via a bare `as u16` cast, so its boundary vector decoded cleanly. The decode
// path now width-guards every narrowing cast and the row carries its committed constraint vector,
// so it projects `enforce = yes` like the rest; the episode is kept in README.md § "Gotchas" (the
// over-acceptance gotcha) as the motivating example for the SHIPPED over-acceptance vector class.)
// A row carrying a class="over-acceptance" vector (a CERTIFIED, unfixed silent-acceptance bug)
// projects the stronger honest fact `no (over-accepts: M)` (deriveEnforce, dominating) instead of
// hiding the hole as `unverified`. The populated set is the three widened-occurrence-marker table
// rows (`contain.occurrence-target.memberkey.type1.{plus,optional,bounded}_table` — the `+`/`?`/`n*m`
// count-permitting markers table-detected to an unbounded 0..N map; cddl-matrix/roadmap.toml § findings),
// each carrying its certified out-of-window map pin. (The seed instance — the no-occurrence type-domain
// arrow widening — took the other branch: closed by graceful rejection at generation, so its row left
// the supported set and the catalog.)
/**
 * Fixed-selector CELLS have three rejection semantics. A supported selector without a reject vector
 * must read `unverified (no reject vector)` rather than `n/a (no constraint)` — n/a would erase the
 * distinction the exact unverified-set pin exists to make loud:
 *   - member equality — a fixed member's decoded value differs from its constant;
 *   - arm selection — a fixed member/key selects a group- or type-choice alternative and no arm
 *     accepts the mutated selector; and
 *   - required-key lookup — a fixed map key is absent from an indefinite map whose valid siblings
 *     remain, so decoding reaches required-key lookup rather than a definite-length precheck.
 *
 * Classification is by (role, feature) wherever that grammar-level relation is sufficient. The tagged
 * `type2.tag` cells below are the deliberate exceptions: `type2.tag` normally means an arbitrary
 * tagged TYPE, while these rows' authored examples wrap a fixed value. Keep their ids explicit and
 * self-checked rather than guessing from an id suffix or an example substring.
 */
const isFixedValueFeature = (f: string): boolean =>
  f === "type2.value" || f.startsWith("value.") ||
  f === "prelude.true" || f === "prelude.false" || f === "prelude.null";

type FixedSelectorKind = "member-equality" | "arm-selection" | "required-key-lookup";

const TAGGED_FIXED_MEMBER_ROLES = new Map([
  ["contain.array-element.type2.tag.fixed_bool", "role.array-element"],
  ["contain.array-element.type2.tag.fixed_null", "role.array-element"],
  ["contain.map-value.type2.tag.fixed_bool", "role.map-value"],
  ["contain.map-value.type2.tag.fixed_null", "role.map-value"],
]);
const TAGGED_FIXED_MEMBER_IDS = new Set(TAGGED_FIXED_MEMBER_ROLES.keys());
const TAGGED_FIXED_ARM_ROLES = new Map([
  ["contain.group-choice-arm.type2.tag.fixed_array", "role.group-choice-arm"],
]);
const TAGGED_FIXED_ARM_IDS = new Set(TAGGED_FIXED_ARM_ROLES.keys());
// `1 => uint` and `"k" => uint` share a required-key decode path with the colon spelling but are
// represented in the model as `memberkey.type1`, so the supported literal-arrow cells are an
// explicit inventory rather than a feature-wide classification of arbitrary type-domain arrow keys.
const FIXED_LITERAL_MAP_KEY_ARROW_IDS = new Set([
  "contain.map-key.memberkey.type1.uint_arrow_single",
  "contain.map-key.memberkey.type1.uint_arrow_multi",
  "contain.map-key.memberkey.type1.text_arrow_single",
  "contain.map-key.memberkey.type1.text_arrow_multi",
]);

function fixedSelectorKind(id: string): FixedSelectorKind | undefined {
  const role = cellRole.get(id);
  const feature = cellFeature.get(id);
  if (role === undefined || feature === undefined) return undefined; // not a containment cell
  if (role === "role.map-key") {
    if (feature === "memberkey.value" || FIXED_LITERAL_MAP_KEY_ARROW_IDS.has(id))
      return "required-key-lookup";
    return undefined;
  }
  if (role === "role.group-choice-arm") {
    if (feature === "memberkey.bareword" || feature === "memberkey.value" ||
        feature === "type2.value" || TAGGED_FIXED_ARM_IDS.has(id))
      return "arm-selection";
    return undefined;
  }
  if (role === "role.choice-member")
    return isFixedValueFeature(feature) ? "arm-selection" : undefined;
  return isFixedValueFeature(feature) || TAGGED_FIXED_MEMBER_IDS.has(id)
    ? "member-equality"
    : undefined;
}

function exceptionalSelectorInventoryProblems(): string[] {
  const problems: string[] = [];
  const expect = (ids: Map<string, string>, name: string) => {
    for (const [id, role] of ids) {
      if (cellRole.get(id) !== role || cellFeature.get(id) !== "type2.tag")
        problems.push(`${name} id \`${id}\` is no longer a \`${role}\` / \`type2.tag\` containment cell — update the explicit fixed-selector inventory`);
    }
  };
  expect(TAGGED_FIXED_MEMBER_ROLES, "tagged fixed member");
  expect(TAGGED_FIXED_ARM_ROLES, "tagged fixed arm");
  for (const id of FIXED_LITERAL_MAP_KEY_ARROW_IDS)
    if (cellRole.get(id) !== "role.map-key" || cellFeature.get(id) !== "memberkey.type1")
      problems.push(`fixed literal arrow id \`${id}\` is no longer a \`role.map-key\` / \`memberkey.type1\` containment cell — update the explicit fixed-selector inventory`);
  return problems;
}

function cellCarriesConstraint(id: string): boolean {
  return fixedSelectorKind(id) !== undefined;
}

function carriesConstraint(id: string): boolean {
  if (id === "ctl.default") return false; // no rejectable constraint (governs an absent field)
  return id.startsWith("ctl.") || id === "memberkey.cut"
    || id.startsWith("occur.bounded") || id.startsWith("value.")
    || cellCarriesConstraint(id);
}

function deriveEnforce(id: string, status: string): string {
  // Over-acceptance DOMINATES: a certified spec-INVALID instance the decoder wrongly accepts is a
  // proven enforcement hole, so it wins over `yes (bounded-reject)` and over `unverified`.
  const over = overAcceptVecById.get(id);
  if (over) return `no (over-accepts: ${over})`;
  const n = constraintVecById.get(id);
  if (n) return `yes (bounded-reject: ${n})`;
  if (carriesConstraint(id) && status === "supported") return "unverified (no reject vector)";
  return "n/a";
}

// The CORE clause of an evidence string: the corroboration tails (wasm, ruby/rust, decode-foreign)
// carry their own "FAILED" vocabulary and by design NEVER downgrade the verdict (e.g. "; wasm
// round-trip FAILED (…)" must not flip round-trip to no), so the rust-verdict columns derive from
// the core only.
function coreOf(evidence: string): string {
  return evidence.split("; wasm")[0].split("; ruby=")[0].split("; accepts ")[0]
    .split("; foreign-vector")[0].split("; no committed decode vectors")[0];
}

const rows: Directional[] = annotations.map(a => {
  const core = coreOf(a.evidence);
  const accept = deriveAccept(a.status, core);
  const roundTrip = deriveRoundTrip(core);
  const encode = deriveEncode(roundTrip);
  const decode = deriveDecode(a.id, a.evidence, roundTrip);
  const enforce = deriveEnforce(a.id, a.status);
  // Trailing note: the same core signal, without the probe prefix.
  const note = core.replace(/^probe(?: \([^)]*\))?:\s*/, "");
  return {
    id: a.id, axis: axisById.get(a.id) ?? "feature", status: a.status, evidence: a.evidence,
    accept, encode, decode, roundTrip, enforce, note,
  };
});

// --- consistency invariants (the gate) ------------------------------------------------------------
function invariantProblems(rs: Directional[]): string[] {
  const problems: string[] = [];
  for (const r of rs) {
    // round-trip = yes ⇒ accept = yes.
    if (r.roundTrip.startsWith("yes") && !r.accept.startsWith("yes"))
      problems.push(`\`${r.id}\`: round-trip=${JSON.stringify(r.roundTrip)} but accept=${JSON.stringify(r.accept)} — cannot round-trip without accepting`);
    // encode = yes ⇔ round-trip = yes (assert the derivation didn't drift).
    if ((r.encode === "yes") !== r.roundTrip.startsWith("yes"))
      problems.push(`\`${r.id}\`: encode=${JSON.stringify(r.encode)} but round-trip=${JSON.stringify(r.roundTrip)} — encode must mirror round-trip exactly`);
    // enforce = yes ⇒ the id has ≥1 class="constraint" reject vector (the only enforcement evidence).
    if (r.enforce.startsWith("yes") && !(constraintVecById.get(r.id)! > 0))
      problems.push(`\`${r.id}\`: enforce=${JSON.stringify(r.enforce)} but the catalog has no class="constraint" reject vector for it`);
    // A row must never project enforce=yes while carrying an over-acceptance vector — an enforcement
    // claim with a certified hole is not "yes" (the over-acceptance dominance rule in deriveEnforce).
    if (r.enforce.startsWith("yes") && (overAcceptVecById.get(r.id) ?? 0) > 0)
      problems.push(`\`${r.id}\`: enforce=${JSON.stringify(r.enforce)} but the row carries a class="over-acceptance" vector — a certified over-acceptance hole must project \`no (over-accepts)\`, never \`yes\``);
    // decode foreign:N (the independent signal) ⇒ the id is a `supported` matrix row (else stale catalog).
    if (r.decode.startsWith("yes (foreign") && statusById.get(r.id) !== "supported")
      problems.push(`\`${r.id}\`: decode reads foreign vectors but the matrix row is \`${statusById.get(r.id)}\`, not \`supported\` — the catalog is stale (re-mint or drop)`);
    // Cross-check the TWO foreign-vector sources the query reads: when the evidence carries verify.ts's
    // decode-foreign clause ("; accepts N foreign spec-derived vector(s)"), its N must equal the
    // catalog's accept-vector count for the id — a mismatch means one of them is stale.
    const clause = /accepts (\d+) foreign spec-derived vector/.exec(r.evidence)
      ?? /foreign-vector decode FAILED \((\d+) vector/.exec(r.evidence);
    if (clause && Number(clause[1]) !== (acceptVecById.get(r.id) ?? 0))
      problems.push(`\`${r.id}\`: evidence records ${clause[1]} foreign vector(s) but the catalog ships ${acceptVecById.get(r.id) ?? 0} — evidence and catalog drifted (re-run verify.ts or fix the catalog)`);
  }
  return problems;
}

// Vocabulary self-check on SYNTHETIC evidence strings, pinning derivation properties no committed row
// currently exercises (so a derive regression fails the gate even before the phrasing appears in data):
//   1. a "; wasm round-trip FAILED (…)" corroboration tail must NOT downgrade the rust verdict;
//   2. per-cell "round-trips=fail" IS the rust verdict and must read no;
//   3. a "; foreign-vector decode FAILED (…)" clause must override the catalog count in decode.
function vocabularyProblems(): string[] {
  const problems: string[] = [];
  const cases: { name: string; ev: string; wantRoundTrip: string }[] = [
    { name: "wasm corroboration failure must not downgrade",
      ev: "probe: cddl-codegen exit 0; compiles; round-trips; wasm round-trip FAILED (cargo test exit 101); ruby=ok rust=ok",
      wantRoundTrip: "yes" },
    { name: "per-cell round-trip failure is the rust verdict",
      ev: "probe (cell): cddl-codegen exit 0; compiles=ok; round-trips=fail; ruby=ok rust=ok",
      wantRoundTrip: "no" },
  ];
  for (const c of cases) {
    const got = deriveRoundTrip(coreOf(c.ev));
    if (got !== c.wantRoundTrip)
      problems.push(`vocabulary self-check "${c.name}": deriveRoundTrip(core) = ${JSON.stringify(got)}, want ${JSON.stringify(c.wantRoundTrip)}`);
  }
  const foreignId = [...acceptVecById.keys()][0];
  if (foreignId !== undefined) {
    const got = deriveDecode(foreignId, "probe: cddl-codegen exit 0; compiles; round-trips; foreign-vector decode FAILED (2 vector(s)); ruby=ok rust=ok", "yes");
    if (got !== "no (foreign decode FAILED)")
      problems.push(`vocabulary self-check "foreign FAILED overrides catalog count": deriveDecode = ${JSON.stringify(got)}, want "no (foreign decode FAILED)"`);
  }
  return problems;
}

function vacuityProblems(rs: Directional[]): string[] {
  const problems: string[] = [];
  const foreignDecode = rs.filter(r => r.decode.startsWith("yes (foreign")).length;
  const enforceYes = rs.filter(r => r.enforce.startsWith("yes")).length;
  const enforceAxisReached = rs.filter(r => r.enforce !== "n/a").length;
  // ≥80 constructs processed (matrix read is non-empty).
  if (rs.length < 80)
    problems.push(`only ${rs.length} constructs processed (expected >= 80) — the matrix read looks broken/empty`);
  // ≥1 independent decode fact (catalog accept-vector read is non-empty).
  if (foreignDecode < 1)
    problems.push(`no row has decode=foreign — the catalog accept-vector read looks broken/empty (Q4's independent decode signal is gone)`);
  // The enforcement axis is REACHED — the derivation ran over the ctl.* rows (matrix ctl read is non-empty).
  if (enforceAxisReached < 1)
    problems.push(`no row reaches the enforcement axis (every enforce is n/a) — the ctl.* enforcement-axis read looks broken/empty`);
  // With class="constraint" vectors present, ≥1 row MUST project enforce=yes, and the green set must be
  // EXACTLY the rows whose vector's ONLY invalidity is the constraint itself (base-type-valid instance,
  // spec-invalidity certified, decoder durably rejects). Certification is normally "both oracles
  // reject", but an oracle that does not implement the rule AT ALL cannot join that consensus — see
  // family (e), where the certifying evidence is the remaining oracle plus a per-vector
  // DECODE_REJECT_ORACLE_GAP_EXEMPT entry (lib.ts) citing a committed spec argument. Families:
  //   (a) The control ops. `ctl.size` / `ctl.cbor` / `memberkey.cut`, plus the numeric range/eq ops
  //       (`.le/.lt/.gt/.eq/.ne/.ge`) whose probe examples target `int` with literal, non-vacuous
  //       bounds — over `int` BOTH oracles enforce them, so each row carries an in-type boundary
  //       violation. That `int` targeting is load-bearing: the rust oracle (cddl 0.10.x) does not
  //       enforce these ops over a `uint` target.
  //   (b) The range rows. `rangeop.{inclusive,exclusive}` (uint-headed base, `0..10`/`0...10`) and their
  //       head-type × sign variation rows `.int` / `.nint` / `.float`. Each carries a boundary-violation
  //       reject vector (out-of-window / excluded endpoint / NaN). BOTH oracles reject the violations:
  //       the released rust 0.10.x CLI blanket-rejected EVERY instance of a non-uint-endpoint range,
  //       leaving the `.int`/`.nint`/`.float` rows accept-less
  //       with ruby-only reject certification, but the fork's `885c61c` fix made rust discriminating —
  //       those rows now carry minted accepts too; the enforcement oracle that matters remains
  //       cddl-codegen's own generated decoder
  //       (the emitted range check, executed by the replay gate). Assert the exact set so a widening
  //       (a type-violation vector engineered onto a numeric row) or a narrowing (a range row losing its
  //       reject vector, re-hiding a silent-acceptance hole) fails this gate.
  //   (c) The controller-value / occurrence-bound variation rows (the variation-row enumeration, roadmap.toml § Expansion).
  //       `ctl.ne.zero` / `ctl.ne.one` carry the NE sign-boundary violations (`00` / `01` — the `(1,-1)`
  //       and degenerate `(2,0)` encodings), int-targeted so both oracles certify like family (a).
  //       `occur.bounded{,.lower,.upper}` carry holder-wrapped out-of-count arrays (below the lower
  //       bound / above the upper bound); both oracles certify (the sole-primitive-entry array shape
  //       keeps repetition count == item count, dodging the rust group-occurrence gap).
  //       `ctl.size.uint` carries the 65536-over-the-u16-window violation, DURABLY rejected by the
  //       width-guarded member decode (the guard replaced the silent truncating `as u16` cast this
  //       row's first vector attempt exposed — README.md § "Gotchas"). Its certification leans on RUBY
  //       plus the local-fixes oracle @ 773b723, which rejects the holder-wrapped violation
  //       discriminatingly (the released 0.10.x CLI misvalidates any control-op-carrying rule
  //       referenced as an array entry — oracle gap #4; the fork fix also let the accept side mint).
  //       `value.number.hexfloat` carries a wrong-float-value violation ([3.5] vs the fixed 3.0),
  //       rejected as FixedValueMismatch; both oracles certify.
  //       `value.number.{hex,bin}` carry hand wrong-fixed-value violations ([0] — the silent-zero
  //       radix-conversion trap — plus an off-by-one/truncated-digit guard each), rejected as
  //       FixedValueMismatch; both oracles certify.
  //   (d) The fixed-selector containment cells, in their three rejection semantics. MEMBER equality
  //       covers literal members plus the explicit tagged-fixed members: each vector changes ONLY the
  //       fixed payload while retaining its tag, outer head, keys, and siblings. The undefined rows
  //       additionally prove that the special-value decoder compares the exact simple value, rather
  //       than accepting any CBOR special. ARM selection covers
  //       group-choice selectors (bareword/fixed keys and fixed members) plus `tstr / null` and the
  //       same-major `true / null / tstr` choice; a wrong selector leaves no legal alternative. REQUIRED
  //       KEY lookup covers supported literal colon and literal-arrow spellings: each vector omits the
  //       target from an indefinite map, retaining a valid sibling when applicable, so it bypasses only
  //       the definite-cardinality precheck and reaches MandatoryFieldMissing. Ordinary vectors are
  //       spec-invalid per BOTH oracles; the exact tag-11 fixed-payload arm is the narrow exception
  //       (Ruby rejects, the pinned rust oracle accepts) named by DECODE_REJECT_ORACLE_GAP_EXEMPT with a
  //       reversible RFC 8610 §3.6 report. Each has a generated-error pin, so a future vectorless
  //       supported selector appears in the exact unverified set rather than reading n/a. The explicit
  //       tagged/arrow inventories are stale-checked for the current coarse-feature exceptions;
  //       role/feature-derived families remain automatic.
  //   (e) The value-class-constrained float prelude names. The six names PARTITION the float values by
  //       their shortest lossless form (RFC 8610 § 2.2.3 / § 3.3): `float16` is the values whose
  //       shortest form is `#7.25`, `float32` `#7.26`, `float64` `#7.27`, with `float16-32` and
  //       `float32-64` spanning two adjacent classes. Each row carries reject vectors whose VALUE
  //       belongs to a class the name excludes — never a head violation, since reads accept every
  //       float head and judge the decoded value (`prelude.float64` carries the same 1.5 at two
  //       different heads precisely to make that head-independence observable). `prelude.float` spans
  //       all three classes and so has no excludable value — vectorless by definition, not an
  //       enforcement blind spot. Their certification, like the tag-11 arm above, is a narrow
  //       rust-only exception: the pinned rust oracle collapses the names into an is-float test while
  //       the ruby gem rejects the out-of-class values. Each vector carries a
  //       DECODE_REJECT_ORACLE_GAP_EXEMPT entry citing
  //       cddl-matrix/upstream-reports/rust-cddl-float-name-blindness.md, which states the spec
  //       reading and the branch that would retract it. Both halves of the ledger are stale-guarded,
  //       so an oracle fix pulls these vectors back onto the ordinary consensus route rather than
  //       leaving a permanent carve-out.
  const EXPECTED_ENFORCE_YES = ["ctl.cbor", "ctl.eq", "ctl.ge", "ctl.gt", "ctl.le", "ctl.lt", "ctl.ne",
    "ctl.ne.one", "ctl.ne.zero", "ctl.size", "ctl.size.uint",
    "dsl.duplicates.preserve",
    "contain.array-element.prelude.false", "contain.array-element.prelude.null",
    "contain.array-element.prelude.true", "contain.array-element.prelude.undefined", "contain.array-element.type2.value",
    "contain.array-element.type2.tag.fixed_bool", "contain.array-element.type2.tag.fixed_null",
    "contain.array-element.type2.value.bare_exactly_once", "contain.array-element.value.number",
    "contain.array-element.value.number.float",
    "contain.array-element.value.number.nint", "contain.array-element.value.text",
    "contain.choice-member.prelude.null", "contain.choice-member.prelude.null.fixed-kind",
    "contain.choice-member.prelude.true.fixed-kind", "contain.choice-member.prelude.true.same_major_brute",
    "contain.choice-member.type2.value.text.fixed-kind", "contain.choice-member.type2.value.uint.fixed-kind",
    "contain.choice-member.type2.value.fixed_null",
    "contain.group-choice-arm.memberkey.bareword.map",
    "contain.group-choice-arm.memberkey.bareword.record_map",
    "contain.group-choice-arm.memberkey.value.map",
    "contain.group-choice-arm.memberkey.value.text_map",
    "contain.group-choice-arm.type2.tag.fixed_array", "contain.group-choice-arm.type2.value.map",
    "contain.group-choice-arm.type2.value.float_array",
    "contain.group-choice-arm.type2.value.float_same_major_array",
    "contain.group-choice-arm.type2.value.nint_array",
    "contain.map-key.memberkey.type1.text_arrow_multi",
    "contain.map-key.memberkey.type1.text_arrow_single",
    "contain.map-key.memberkey.type1.uint_arrow_multi",
    "contain.map-key.memberkey.type1.uint_arrow_single",
    "contain.map-key.memberkey.value.text_colon_multi",
    "contain.map-key.memberkey.value.text_colon_single",
    "contain.map-key.memberkey.value.uint_colon_multi",
    "contain.map-key.memberkey.value.uint_colon_single",
    "contain.map-value.prelude.false", "contain.map-value.prelude.null",
    "contain.map-value.prelude.true", "contain.map-value.prelude.undefined", "contain.map-value.type2.value",
    "contain.map-value.type2.tag.fixed_bool", "contain.map-value.type2.tag.fixed_null",
    "contain.map-value.value.number", "contain.map-value.value.text",
    "contain.occurrence-target.grpent.member.plus_array",
    "contain.occurrence-target.memberkey.type1.plus_table",
    "contain.occurrence-target.memberkey.type1.optional_table",
    "contain.occurrence-target.memberkey.type1.bounded_table",
    "contain.occurrence-target.memberkey.type1.open_struct_bounded",
    "contain.occurrence-target.memberkey.type1.open_table_catchall_bounded",
    "contain.occurrence-target.memberkey.type1.open_table_typed_bounded",
    "contain.map-key.memberkey.type1.tstr_arrow_nooccur",
    "contain.occurrence-target.type2.value.optional_keyed_array",
    "contain.occurrence-target.type2.value.optional_keyed_map", "memberkey.cut",
    "occur.bounded", "occur.bounded.lower", "occur.bounded.upper", "occur.one_or_more",
    "prelude.false", "prelude.float16", "prelude.float16-32", "prelude.float32", "prelude.float32-64", "prelude.float64",
    "prelude.nil", "prelude.null", "prelude.true", "prelude.undefined",
    "rangeop.exclusive", "rangeop.exclusive.float", "rangeop.exclusive.int", "rangeop.exclusive.nint",
    "rangeop.inclusive", "rangeop.inclusive.float", "rangeop.inclusive.int", "rangeop.inclusive.nint",
    "type2.value", "value.number", "value.number.bin", "value.number.hex", "value.number.hexfloat", "value.text"];
  // The unverified set is pinned EXACTLY like the green set (same decay argument, opposite
  // direction): a NEW supported enforcement-bearing row landing vectorless would otherwise slide
  // into `unverified` with no gate noticing — the variation-row lesson (roadmap.toml § Expansion) is that an unenumerated/unvectored
  // constraint is an enforcement blind spot, so growing this set must be a conscious pin edit.
  // The four fixed-byte rows are temporarily vectorless because the pinned rust-cddl validator
  // panics on both valid and invalid instances before it can corroborate the spec verdict. Generated
  // execution separately pins exact byte equality and reason-bearing rejection; when the upstream
  // validator repair ships, re-mint both accept and constraint vectors and move these ids into the
  // green set above (cddl-matrix/README.md upstream gap #17).
  const EXPECTED_ENFORCE_UNVERIFIED: string[] = [
    "contain.array-element.value.bytes",
    "contain.choice-member.type2.value.bytes.fixed-kind",
    "contain.map-value.value.bytes",
    "value.bytes",
  ];
  // The over-accepts set is pinned the SAME way: a row carrying a class="over-acceptance" vector
  // projects `enforce = no (over-accepts: M)` — the SHIPPED over-acceptance vector class (catalog pin
  // + rust replay leg asserting "still wrongly accepts" + this projection). EMPTY today: the
  // widened-occurrence-marker table class (cddl-matrix/roadmap.toml § findings) is CLOSED. Both promotion
  // branches fired for a COUNT-PERMITTING occurrence marker on a single non-literal arrow map entry:
  //   - `+` / `1*` is now HONORED — the entry decodes as a `NonEmptyMap` whose single TryFrom door
  //     rejects the empty map, so `plus_table`'s empty-map vector was promoted to class="constraint"
  //     and its row id moved to EXPECTED_ENFORCE_YES above (the decoder-fix branch);
  //   - `?` / `n*m` / `*n` / `0*n` and omitted exact-once arrows are now BoundedMap windows, while
  //     the preserve flavor enters the same inclusive window through BoundedPairMap; their hand
  //     below/above-window vectors are class="constraint" and project enforce=yes.
  // The machinery stays armed: a NEW certified over-acceptance lands as a class="over-acceptance"
  // vector and its row id here; a decoder FIX flips the replay pin loudly, promotes the vector to
  // class="constraint", and moves the row id to EXPECTED_ENFORCE_YES.
  const EXPECTED_ENFORCE_OVERACCEPTS: string[] = [];
  if (anyConstraintVectors) {
    if (enforceYes < 1)
      problems.push(`the catalog ships class="constraint" vectors but no row projects enforce=yes — the enforce derivation drifted`);
    const greenSet = rs.filter(r => r.enforce.startsWith("yes")).map(r => r.id).sort();
    const want = [...EXPECTED_ENFORCE_YES].sort();
    if (JSON.stringify(greenSet) !== JSON.stringify(want))
      problems.push(`enforce=yes green set drifted:\n    got : ${JSON.stringify(greenSet)}\n    want: ${JSON.stringify(want)}\n    (the exact pin requires a conscious update for any deliberate classification change)`);
    const unverifiedSet = rs.filter(r => r.enforce.startsWith("unverified")).map(r => r.id).sort();
    const wantUnverified = [...EXPECTED_ENFORCE_UNVERIFIED].sort();
    if (JSON.stringify(unverifiedSet) !== JSON.stringify(wantUnverified))
      problems.push(`enforce=unverified set drifted:\n    got : ${JSON.stringify(unverifiedSet)}\n    want: ${JSON.stringify(wantUnverified)}\n    (a new supported enforcement-bearing row must land WITH its reject vector — or be consciously pinned here with a reason, like ctl.size.uint's verified truncation gap)`);
  }
  // The over-accepts set is asserted UNCONDITIONALLY (not gated on constraint vectors): it has its own
  // committed vector class, so a broken catalog read (empty set) fails against the non-empty pin, and a
  // NEW certified over-acceptance landing (or the seed's replay pin flipping to a fix) must be a
  // conscious edit here, exactly like the green/unverified sets.
  const overAcceptSet = rs.filter(r => r.enforce.startsWith("no (over-accepts")).map(r => r.id).sort();
  const wantOverAccept = [...EXPECTED_ENFORCE_OVERACCEPTS].sort();
  if (JSON.stringify(overAcceptSet) !== JSON.stringify(wantOverAccept))
    problems.push(`enforce=no (over-accepts) set drifted:\n    got : ${JSON.stringify(overAcceptSet)}\n    want: ${JSON.stringify(wantOverAccept)}\n    (a certified over-acceptance pin lands WITH its class="over-acceptance" catalog vector; when the fix lands and the replay pin flips, promote the vector to class="constraint" and move the row id to EXPECTED_ENFORCE_YES)`);
  return problems;
}

// --- report ---------------------------------------------------------------------------------------
const FOOTNOTE =
  "encode is the ENCODE HALF of round-trip (our encoder's bytes decoded back) — there is NO independent\n" +
  "per-construct encode oracle, so encode=yes iff round-trip=yes and never claims more. decode, by\n" +
  "contrast, has INDEPENDENT per-construct evidence: `foreign: N` counts spec-derived CBOR our own\n" +
  "encoder never produced (tests/decode_conformance/catalog.toml) — the direction a round-trip conflates\n" +
  "away, and the reason Q4 splits the two. `via round-trip` decode is the weak tier (no foreign vector).";

const isCheck = process.argv.slice(2).includes("--check");
const positional = process.argv.slice(2).filter(a => !a.startsWith("--"));

if (isCheck) {
  const problems = [
    ...invariantProblems(rows),
    ...exceptionalSelectorInventoryProblems(),
    ...vacuityProblems(rows),
    ...vocabularyProblems(),
  ];
  if (problems.length) {
    console.log(`Q4 directional-support gate: ${problems.length} problem(s)`);
    for (const p of problems) console.log(`  FAIL ${p}`);
    process.exit(1);
  }
  const foreignDecode = rows.filter(r => r.decode.startsWith("yes (foreign")).length;
  const enforceYes = rows.filter(r => r.enforce.startsWith("yes")).length;
  const enforceUnverified = rows.filter(r => r.enforce.startsWith("unverified")).length;
  const enforceOverAccepts = rows.filter(r => r.enforce.startsWith("no (over-accepts")).length;
  console.log(
    `Q4 directional gate OK — ${rows.length} constructs projected · ` +
      `${foreignDecode} decode=foreign (independent) · ` +
      `${enforceYes} enforce=yes (bounded-reject), ${enforceUnverified} enforce=unverified (no reject vector yet), ` +
      `${enforceOverAccepts} enforce=no (over-accepts, certified over-acceptance pin)`,
  );
  process.exit(0);
}

// Default (and filtered) run: the readable Q4 table, grouped by axis.
const filter = positional[0]?.toLowerCase();
const shown = filter ? rows.filter(r => r.id.toLowerCase().includes(filter)) : rows;
if (filter && shown.length === 0) {
  console.log(`Q4 directional support — no construct id contains ${JSON.stringify(positional[0])}`);
  process.exit(0);
}

const COLS: { key: keyof Directional; head: string }[] = [
  { key: "id", head: "id" },
  { key: "accept", head: "accept" },
  { key: "encode", head: "encode" },
  { key: "decode", head: "decode" },
  { key: "roundTrip", head: "round-trip" },
  { key: "enforce", head: "enforce" },
];
const AXES: Axis[] = ["feature", "containment-cell", "control-op"];
const AXIS_TITLE: Record<Axis, string> = {
  feature: "FEATURE axis",
  "containment-cell": "CONTAINMENT-CELL axis (role × feature)",
  "control-op": "CONTROL-OP axis (the enforcement axis)",
};

// Column widths over everything shown, so the axis groups align to one grid.
const width: Record<string, number> = {};
for (const c of COLS) width[c.key] = c.head.length;
for (const r of shown) for (const c of COLS) width[c.key] = Math.max(width[c.key], String(r[c.key]).length);

const pad = (s: string, w: number) => s.padEnd(w);
const headerLine = COLS.map(c => pad(c.head, width[c.key])).join("  ") + "  note";
const rule = "-".repeat(headerLine.length);

console.log(`\nQ4 — directional / enforcement support for cddl-codegen  {accept, encode, decode, round-trip, enforce-constraint}`);
console.log(`(${shown.length}${filter ? ` of ${rows.length}` : ""} constructs${filter ? `, filtered by ${JSON.stringify(positional[0])}` : ""})\n`);

for (const axis of AXES) {
  const group = shown.filter(r => r.axis === axis);
  if (group.length === 0) continue;
  console.log(`### ${AXIS_TITLE[axis]} — ${group.length} construct(s)`);
  console.log(headerLine);
  console.log(rule);
  for (const r of group) {
    const cells = COLS.map(c => pad(String(r[c.key]), width[c.key])).join("  ");
    console.log(`${cells}  ${r.note}`);
  }
  console.log("");
}

console.log("footnote (encode/decode asymmetry):");
for (const l of FOOTNOTE.split("\n")) console.log(`  ${l}`);
