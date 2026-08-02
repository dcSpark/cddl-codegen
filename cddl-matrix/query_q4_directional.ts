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
 * `DECODE_REJECT_ORACLE_GAP_EXEMPT` entry (lib.ts) citing a committed spec argument — the float
 * head-set rows are the only residents, and both halves of that ledger are stale-guarded.
 * Three rows carry one today and project `enforce = yes (bounded-reject)`: `ctl.size` (over- AND
 * under-size strings — valid bstrs, only `.size 4` rejects them), `ctl.cbor` (`h'f6'` is a valid bstr
 * whose payload is CBOR null, not a uint — only `.cbor uint` rejects it), and `memberkey.cut` (a
 * cut-violating map value; scoped to the row's single-member example, where the value-type check IS the
 * constraint surface). The numeric range/equality ops (`.le/.lt/.gt/.eq/.ne/.ge`) do NOT — deliberately:
 * cddl-codegen's decoder DOES enforce them (it emits a RangeCheck), but the rust corroborating oracle
 * (`cddl` 0.10.x) does NOT enforce numeric control ops over a `uint` target in validation (the
 * committed probe examples are all uint-based; `int`-target controls ARE enforced — see
 * `draft/rust-cddl-uint-control-op-gap.md`) — it accepts a boundary violation
 * like `11` for `uint .le 10` — so an in-type boundary-violating vector cannot pass the two-oracle
 * spec-invalid gate the catalog requires (ruby rejects it, rust does not). Rather than engineer green
 * with a type-violation vector (a negative for `uint .ge 0` — which tests the uint base type, not the
 * bound; that example's bound is vacuous over its base type, so NO in-type violation even exists),
 * those rows honestly read `unverified (no reject vector)`; the oracle-coverage gap is recorded in
 * README.md § "Upstream oracle gaps". The `--check` vacuity floor asserts the EXACT green set so neither an
 * accidental widening (a numeric row engineered green via a type violation) nor a narrowing slips
 * through.
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
interface UnivRow { id: string }
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

function deriveRoundTrip(ev: string): string {
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
// Also enforcement-bearing (the variation-row classification-gap lesson (ROADMAP § Expansion) — an enforced constraint invisible as
// n/a is indistinguishable from "carries no constraint", so a vector loss would silently shed
// enforcement evidence instead of reading as a gap):
//   - `occur.bounded*` — occurrence bounds are a rejectable count constraint (the generated decoder's
//     Vec len check);
//   - `value.number*` (and the other `value.*` fixed-value rows' member forms) — a fixed value is a
//     rejectable equality constraint. (Today all `value.number*` rows are `unsupported`, so they
//     derive n/a by status — the classification still matters so a future supported+vectorless state
//     reads `unverified`, not n/a.)
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
// count-permitting markers table-detected to an unbounded 0..N map; cddl-matrix/ROADMAP.md § findings),
// each carrying its certified out-of-window map pin. (The seed instance — the no-occurrence type-domain
// arrow widening — took the other branch: closed by graceful rejection at generation, so its row left
// the supported set and the catalog.)
function carriesConstraint(id: string): boolean {
  if (id === "ctl.default") return false; // no rejectable constraint (governs an absent field)
  return id.startsWith("ctl.") || id === "memberkey.cut"
    || id.startsWith("occur.bounded") || id.startsWith("value.number");
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
  //       enforce these ops over a `uint` target (draft/rust-cddl-uint-control-op-gap.md).
  //   (b) The range rows. `rangeop.{inclusive,exclusive}` (uint-headed base, `0..10`/`0...10`) and their
  //       head-type × sign variation rows `.int` / `.nint` / `.float`. Each carries a boundary-violation
  //       reject vector (out-of-window / excluded endpoint / NaN). BOTH oracles reject the violations:
  //       the released rust 0.10.x CLI blanket-rejected EVERY instance of a non-uint-endpoint range
  //       (draft/rust-cddl-float-range-gap.md), leaving the `.int`/`.nint`/`.float` rows accept-less
  //       with ruby-only reject certification, but the fork's `885c61c` fix made rust discriminating —
  //       those rows now carry minted accepts too; the enforcement oracle that matters remains
  //       cddl-codegen's own generated decoder
  //       (the emitted range check, executed by the replay gate). Assert the exact set so a widening
  //       (a type-violation vector engineered onto a numeric row) or a narrowing (a range row losing its
  //       reject vector, re-hiding a silent-acceptance hole) fails this gate.
  //   (c) The controller-value / occurrence-bound variation rows (the variation-row enumeration, ROADMAP § Expansion).
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
  //   (d) The fixed-value MEMBER cells — the 15 supported `contain.{array-element,map-value,
  //       occurrence-target}` cells whose member is a constant (`v: 5`, `k: "x"`, `v: true/false/null`,
  //       the bare unkeyed `5`, and the two OPTIONAL `? v: 5` forms). Each carries one WRONG-CONSTANT
  //       instance derived from one of the row's own accepts by mutating ONLY the constant's byte(s),
  //       so the constant is the sole difference (same outer head, same key order, same siblings) — the
  //       vector that makes the row's green test the CONSTANT and not just the shape. The uint/text/bool
  //       cells reject as FixedValueMismatch (`Expected fixed value 5 found 6`, `… "x" found "y"`,
  //       `… true found false`); the two null cells pin `ExpectedNull` instead, because for null the
  //       constant IS the type, so the member's check is a type check — the vector still catches a
  //       decoder that stops checking the member. The optional cells' instances are PRESENT-wrong
  //       (absence is a legal accept shape), which routes through the optional peek path and still
  //       lands on the constant check. Both oracles certify every one.
  //   (e) The head-constrained float prelude names. `float16`/`float32`/`float64` are `#7.25`/`#7.26`/
  //       `#7.27` (RFC 8610 App. D), `float16-32` and `float32-64` the two-head unions; each row
  //       carries out-of-set-head reject vectors whose value is deliberately in range at every width,
  //       so the HEAD is the sole invalidity. `prelude.float` has no out-of-set head and so no vector
  //       — it is width-unconstrained by definition, not an enforcement blind spot. These five are the
  //       first rows certified WITHOUT a two-oracle consensus: the pinned rust oracle performs no float
  //       head validation at all, and the ruby gem classifies a float by the narrowest IEEE width its
  //       VALUE fits (so it accepts four of the eight vectors — and, symmetrically, rejects canonical
  //       in-set encodings like an `fa`-headed 1.5 against `float32`). Each affected vector carries a
  //       DECODE_REJECT_ORACLE_GAP_EXEMPT entry naming the accepting oracle(s) and citing
  //       cddl-matrix/upstream-reports/ruby-cddl-float-width-validation.md, which states the spec
  //       reading and the branch that would retract it. Both halves of that ledger are stale-guarded,
  //       so an oracle fix pulls these rows back onto the ordinary consensus route rather than leaving
  //       a permanent carve-out.
  const EXPECTED_ENFORCE_YES = ["ctl.cbor", "ctl.eq", "ctl.ge", "ctl.gt", "ctl.le", "ctl.lt", "ctl.ne",
    "ctl.ne.one", "ctl.ne.zero", "ctl.size", "ctl.size.uint",
    "contain.array-element.prelude.false", "contain.array-element.prelude.null",
    "contain.array-element.prelude.true", "contain.array-element.type2.value",
    "contain.array-element.type2.value.bare_exactly_once", "contain.array-element.value.number",
    "contain.array-element.value.text",
    "contain.map-value.prelude.false", "contain.map-value.prelude.null",
    "contain.map-value.prelude.true", "contain.map-value.type2.value",
    "contain.map-value.value.number", "contain.map-value.value.text",
    "contain.occurrence-target.memberkey.type1.plus_table",
    "contain.occurrence-target.type2.value.optional_keyed_array",
    "contain.occurrence-target.type2.value.optional_keyed_map", "memberkey.cut",
    "occur.bounded", "occur.bounded.lower", "occur.bounded.upper", "occur.one_or_more",
    "prelude.float16", "prelude.float16-32", "prelude.float32", "prelude.float32-64", "prelude.float64",
    "rangeop.exclusive", "rangeop.exclusive.float", "rangeop.exclusive.int", "rangeop.exclusive.nint",
    "rangeop.inclusive", "rangeop.inclusive.float", "rangeop.inclusive.int", "rangeop.inclusive.nint",
    "value.number.bin", "value.number.hex", "value.number.hexfloat"];
  // The unverified set is pinned EXACTLY like the green set (same decay argument, opposite
  // direction): a NEW supported enforcement-bearing row landing vectorless would otherwise slide
  // into `unverified` with no gate noticing — the variation-row lesson (ROADMAP § Expansion) is that an unenumerated/unvectored
  // constraint is an enforcement blind spot, so growing this set must be a conscious pin edit.
  // EMPTY today: the one former entry (ctl.size.uint, a verified truncation gap) is fixed — the
  // member decode width-guards the narrowing cast and the row's constraint vector is committed —
  // so every supported enforcement-bearing row is green. The pin stays as the decay gate.
  const EXPECTED_ENFORCE_UNVERIFIED: string[] = [];
  // The over-accepts set is pinned the SAME way: a row carrying a class="over-acceptance" vector
  // projects `enforce = no (over-accepts: M)` — the SHIPPED over-acceptance vector class (catalog pin
  // + rust replay leg asserting "still wrongly accepts" + this projection). EMPTY today: the
  // widened-occurrence-marker table class (cddl-matrix/ROADMAP.md § findings) is CLOSED. Both promotion
  // branches fired for a COUNT-PERMITTING occurrence marker on a single non-literal arrow map entry:
  //   - `+` / `1*` is now HONORED — the entry decodes as a `NonEmptyMap` whose single TryFrom door
  //     rejects the empty map, so `plus_table`'s empty-map vector was promoted to class="constraint"
  //     and its row id moved to EXPECTED_ENFORCE_YES above (the decoder-fix branch);
  //   - `?` / `n*m` / `*n` / `0*n` is REJECTED gracefully at generation (a bounded cardinality this
  //     phase does not honor — silently widening it to 0..N was the bug), so `optional_table` /
  //     `bounded_table` flipped unsupported and their catalog rows were dropped (the graceful-rejection
  //     branch, same as the seed no-occurrence type-domain arrow row before them).
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
      problems.push(`enforce=yes green set drifted:\n    got : ${JSON.stringify(greenSet)}\n    want: ${JSON.stringify(want)}\n    (numeric range/eq ops stay unverified — the rust oracle does not enforce them; see the header note)`);
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
  const problems = [...invariantProblems(rows), ...vacuityProblems(rows), ...vocabularyProblems()];
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
