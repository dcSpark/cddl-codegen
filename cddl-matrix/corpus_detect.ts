#!/usr/bin/env bun
/**
 * D3 "appears" derivation for the corpus projection — the MECHANICAL FLOOR.
 *
 * `featuresIn(cddl)` returns the matrix ids whose construct syntactically *appears* in a CDDL fixture.
 * In D3 this is NOT the authority (the overlay names the canonical isolating fixture per construct, and
 * the projection drift-checks the named fixture really contains it) — this function is (a) the floor /
 * over-credit diagnostic and (b) the primitive that backs that drift-check. So it is allowed to be
 * approximate; it must err toward RECALL (don't miss a construct that's there). Run directly for the
 * diagnostic: per-fixture detections + how badly each fixture over-credits (the D1 problem, quantified).
 *
 *   bun run corpus_detect.ts
 *
 * ponytail: text-scan detection (comment-stripped for RFC constructs, comment-only for the @-DSL), not a
 * real parse — there is no CDDL→AST dump available (the rust `cddl` CLI only compiles; the parsing `cddl`
 * crate is Rust, this tooling is TS). Consistent with verify.ts, which also hand-scans CDDL text. Upgrade
 * path if precision ever bites: a `cargo run` helper that dumps the `cddl` crate AST as JSON. Prelude and
 * control-op ids are matched by NAME token (auto-covers ~90 ids); structural constructs use a small hand
 * table. Features with no detector are reported, not silently treated as absent.
 */
import { readFileSync } from "node:fs";
import { Glob } from "bun";

const HERE = import.meta.dir;
const CORPUS = `${HERE}/../tests/corpus`;

const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as {
  features: { id: string; production?: string }[];
  control_operators: { id: string }[];
};
const preludeNames = matrix.features.filter(f => f.production === "prelude").map(f => f.id.slice("prelude.".length));
const ctlNames = matrix.control_operators.map(c => c.id.slice("ctl.".length));
const reEsc = (s: string) => s.replace(/[.*+?^${}()|[\]\\-]/g, "\\$&");

// --- comment handling: CDDL comment is `;`..EOL (but not inside a "string"). Split each line into
//     code (RFC constructs live here) and comment (the @-DSL lives here). ---
function split(line: string): { code: string; comment: string } {
  let inQ = false;
  for (let i = 0; i < line.length; i++) {
    if (line[i] === '"') inQ = !inQ;
    else if (line[i] === ";" && !inQ) return { code: line.slice(0, i), comment: line.slice(i + 1) };
  }
  return { code: line, comment: "" };
}
const codeOf = (t: string) => t.split(/\r?\n/).map(l => split(l).code).join("\n");
const commentsOf = (t: string) => t.split(/\r?\n/).map(l => split(l).comment).join("\n");

// rule LHS names (so a reference to one is a type2.typename "appears"); strip generics on the LHS.
const ruleNames = (code: string): string[] =>
  [...code.matchAll(/^[ \t]*([A-Za-z_]\w*)\s*(?:<[^>]*>)?\s*=/gm)].map(m => m[1]);

// --- structural detectors over the comment-stripped code. Approximate by design (see header). ---
const STRUCT: { id: string; hit: (code: string) => boolean }[] = [
  { id: "type2.array", hit: c => c.includes("[") },
  { id: "type2.map", hit: c => c.includes("{") },
  { id: "type2.tag", hit: c => /#6(\.\d+)?\s*\(/.test(c) },
  { id: "type2.parenthesized", hit: c => /\([^)]*:/.test(c) },           // a (group) with a key, not a ctl-arg paren
  { id: "type.choice", hit: c => /(?<!\/)\/(?!\/)/.test(c) },             // single `/`, not `//`
  // all-fixed-value choice (c-style enum): `= v / v [/ v]…` where every alternative is a literal value
  { id: "type.enum", hit: c => /=\s*(-?\d+|"[^"]*")(\s*\/\s*(-?\d+|"[^"]*"))+/.test(c) },
  { id: "group.choice", hit: c => c.includes("//") },
  { id: "memberkey.bareword", hit: c => /\b[A-Za-z_]\w*\s*:/.test(c) },
  { id: "memberkey.value", hit: c => /(^|[\s,{])(-?\d+|"[^"]*")\s*:/.test(c) },
  { id: "memberkey.type1", hit: c => c.includes("=>") },
  { id: "memberkey.cut", hit: c => c.includes("^") },
  { id: "occur.optional", hit: c => c.includes("?") },
  { id: "occur.one_or_more", hit: c => /(^|\s)\+\s/.test(c) },
  { id: "occur.bounded", hit: c => /\d+\*\d+/.test(c) },
  { id: "occur.zero_or_more", hit: c => /(^|[\s,{[])\*(?!\d)/.test(c) },  // `*` occurrence (excl. the n*m form)
  { id: "rangeop.exclusive", hit: c => c.includes("...") },
  { id: "rangeop.inclusive", hit: c => /(?<!\.)\.\.(?!\.)/.test(c) },
  { id: "value.text", hit: c => /"[^"]*"/.test(c) },
  { id: "value.bytes", hit: c => /\b(h|b64)'/.test(c) },
  { id: "value.number", hit: c => /(^|[\s,/[(])-?\d+\b/.test(c) },        // incl. range/ctl-arg numbers (over-credits — that's the point)
  { id: "genericparm.type", hit: c => /^[ \t]*[A-Za-z_]\w*<[^>]+>\s*=/m.test(c) },
  { id: "genericarg.type", hit: c => /\b[A-Za-z_]\w*<[^>=]+>(?!\s*=)/.test(c) },
  { id: "type2.value", hit: c => /(^|[\s,/[(])(-?\d+\b|"[^"]*"|(h|b64)')/.test(c) }, // a literal at a type position
];

export interface Detected { rfc: Set<string>; ctl: Set<string>; dsl: Set<string> }

export function featuresIn(cddl: string): Detected {
  const code = codeOf(cddl);
  const comments = commentsOf(cddl);
  const rfc = new Set<string>();
  const ctl = new Set<string>();
  const dsl = new Set<string>();

  // prelude types by name token
  for (const n of preludeNames) if (new RegExp(`\\b${reEsc(n)}\\b`).test(code)) rfc.add(`prelude.${n}`);
  // control operators by `.name` token (guard against the `.b64u` ⊂ `.b64u-sloppy` prefix trap)
  for (const n of ctlNames) if (new RegExp(`\\.${reEsc(n)}(?![\\w-])`).test(code)) { ctl.add(`ctl.${n}`); rfc.add("type1.ctlop"); }
  // structural constructs
  for (const d of STRUCT) if (d.hit(code)) rfc.add(d.id);
  // references to locally-defined rules -> a typename appears
  const names = new Set(ruleNames(code));
  for (const n of names) {
    const refs = [...code.matchAll(new RegExp(`\\b${reEsc(n)}\\b`, "g"))].length;
    const defs = [...code.matchAll(new RegExp(`^[ \\t]*${reEsc(n)}\\s*(?:<[^>]*>)?\\s*=`, "gm"))].length;
    if (refs > defs) rfc.add("type2.typename");
  }
  // CDDL_CODEGEN profile: @-DSL directives (in comments) -> dsl.<name>; sentinel typenames -> ext.<name>
  for (const m of comments.matchAll(/@([A-Za-z_]\w*)/g)) dsl.add(`dsl.${m[1]}`);
  if (code.includes("_CDDL_CODEGEN_EXTERN_TYPE_")) dsl.add("ext.extern");
  if (code.includes("_CDDL_CODEGEN_RAW_BYTES_TYPE_")) dsl.add("ext.raw_bytes");

  return { rfc, ctl, dsl };
}

// --- self-check (ponytail: one runnable check on the non-trivial logic) ---
function selfCheck() {
  const a = featuresIn("arr = [uint, text, bytes]");
  for (const id of ["type2.array", "prelude.uint", "prelude.text", "prelude.bytes"])
    if (!a.rfc.has(id)) throw new Error(`selfCheck: expected ${id} in array.cddl`);
  if (a.rfc.has("prelude.int")) throw new Error("selfCheck: \\bint\\b false-matched inside uint");
  const t = featuresIn("tagged = #6.42(text)");
  if (!t.rfc.has("type2.tag")) throw new Error("selfCheck: missing type2.tag");
  const s = featuresIn("hash = bytes .size (0..32)");
  if (!s.ctl.has("ctl.size")) throw new Error("selfCheck: missing ctl.size");
  if (!s.rfc.has("rangeop.inclusive")) throw new Error("selfCheck: missing rangeop.inclusive");
  const d = featuresIn("x = uint ; @newtype @custom_json");
  if (!d.dsl.has("dsl.newtype") || !d.dsl.has("dsl.custom_json")) throw new Error("selfCheck: missing DSL id");
  if (d.rfc.has("type1.ctlop")) throw new Error("selfCheck: DSL `@custom...` leaked into code as a ctlop");
  if (!featuresIn("foo = _CDDL_CODEGEN_EXTERN_TYPE_\nbar = [x: foo]").dsl.has("ext.extern")) throw new Error("selfCheck: missing ext.extern");
}

if (import.meta.main) {
  selfCheck();
  const files = [...new Glob("*.cddl").scanSync({ cwd: CORPUS })].sort();
  const allFeatureIds = new Set(matrix.features.map(f => f.id));
  const allCtlIds = new Set(matrix.control_operators.map(c => c.id));

  const perFixture = files.map(f => {
    const det = featuresIn(readFileSync(`${CORPUS}/${f}`, "utf8"));
    return { f, ...det, count: det.rfc.size + det.ctl.size + det.dsl.size };
  });

  // how often each id is credited across fixtures -> "incidental" = credited widely (the D1 over-credit)
  const freq = new Map<string, number>();
  for (const p of perFixture) for (const id of [...p.rfc, ...p.ctl]) freq.set(id, (freq.get(id) ?? 0) + 1);

  console.log("=== per-fixture detections (the D1 'appears anywhere' floor) ===\n");
  for (const p of perFixture) {
    console.log(`${p.f}  — credits ${p.count} construct(s)${p.dsl.size ? `, DSL: ${[...p.dsl].join(" ")}` : ""}`);
    console.log(`    ${[...p.rfc, ...p.ctl].sort().join(", ")}`);
  }

  const overCredit = perFixture.filter(p => p.count > 3).sort((a, b) => b.count - a.count);
  console.log(`\n=== over-crediting (why pure D1 is wrong; D3 overlay must name the *canonical* fixture) ===`);
  console.log(`fixtures crediting >3 constructs: ${overCredit.length}/${files.length}`);
  for (const p of overCredit.slice(0, 6)) console.log(`  ${p.f}: ${p.count}`);
  const incidental = [...freq].filter(([, n]) => n >= 6).sort((a, b) => b[1] - a[1]);
  console.log(`incidental constructs (credited in ≥6 fixtures — the noise D1 can't tell from real coverage):`);
  for (const [id, n] of incidental) console.log(`  ${id}: in ${n} fixtures`);

  // the floor: union detected vs the matrix universe (feature ids + ctl ids)
  const seen = new Set<string>();
  for (const p of perFixture) for (const id of [...p.rfc, ...p.ctl, ...p.dsl]) seen.add(id);
  const universe = new Set([...allFeatureIds, ...allCtlIds]);
  const undetected = [...universe].filter(id => !seen.has(id)).sort();
  // ids the detector emitted that aren't real matrix ids (would be detector bugs)
  const spurious = [...seen].filter(id => !universe.has(id)).sort();
  console.log(`\n=== floor vs matrix universe ===`);
  console.log(`detected (D1 'covered' floor): ${seen.size} / ${universe.size} ids`);
  console.log(`NOT detected anywhere (➕ supported-untested or ➖ unsupported candidates): ${undetected.length}`);
  console.log(`  ${undetected.join(", ")}`);
  if (spurious.length) console.log(`SPURIOUS (detected but not a matrix id — detector bug): ${spurious.join(", ")}`);
  console.log(`\nCDDL_CODEGEN profile ids seen across corpus: ${[...new Set(perFixture.flatMap(p => [...p.dsl]))].sort().join(" ")}`);
}
