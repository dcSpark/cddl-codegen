#!/usr/bin/env bun
/**
 * Recombination-fuzzer INGREDIENTS projection — matrix examples -> reusable composition parts.
 *
 * The shape-recombination grammar fuzzer (src/tests/recombination_tests.rs) composes the matrix's
 * per-feature `example`s into deeper/wider specs than any single row samples, and runs them through
 * the generator with escalating oracles. This script is stage A: it distils the matrix into the
 * things the Rust composer needs, as a committed, deterministic artifact (tests/recomb/ingredients.json):
 *
 *   - `fillers`: one entry per feature whose `example` reduces to a reusable hole-fillable expression —
 *     `{ feature, expr, aux }`, where `expr` is the primary (root) rule's RHS and `aux` is the list of
 *     auxiliary rule definitions it depends on (multi-rule examples like `h = [uint]\na = [~h]` project
 *     to primary `a` -> expr `[~h]`, aux `["h = [uint]"]`). Features whose example can't be reduced to a
 *     single standalone expression are recorded in `skipped` (with a reason) so the harness floor can
 *     count them rather than silently lose coverage.
 *   - `legal`: the `(role, feature)` pairs the containment matrix marks `spec = "allowed"` — used to
 *     validate that every hand-written role template names a role the matrix actually models.
 *   - `disallowed`: the `(role, feature)` pairs the matrix marks `spec = "disallowed"` — the blacklist
 *     the composer obeys (it composes any (role, filler) EXCEPT these; the matrix omits trivial
 *     primitive-as-member cells as implicitly allowed, so an allow-list would erase most breadth).
 *
 * The "primary rule" heuristic: the ROOT rule (the one no OTHER rule references) is the reusable one —
 * its RHS is the expression, every other rule is auxiliary. Examples that don't reduce cleanly (generic-
 * parameterized roots, DSL-comment bodies, choice-extension duplicates, extern placeholders) are skipped
 * with a recorded reason; the `--check` drift gate keeps the projection stable.
 *
 * Run from cddl-matrix/:
 *   bun run project_recombination.ts          -> (re)writes tests/recomb/ingredients.json
 *   bun run project_recombination.ts --check  -> drift gate: fails if the committed file is stale/missing
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { ROOT, stableJson } from "./lib.ts";

const OUT_DIR = `${ROOT}/../tests/recomb`;
const OUT = `${OUT_DIR}/ingredients.json`;
const CHECK = process.argv.includes("--check");

interface Ex { id: string; example: string }
const matrix = JSON.parse(readFileSync(`${ROOT}/matrix.json`, "utf8")) as {
  features: Ex[];
  containment: (Ex & { role: string; feature: string; spec?: string })[];
};

// --- rule splitting -------------------------------------------------------------------------------
// Split a (possibly multi-line, possibly multi-rule) CDDL example into its top-level rules. A rule
// header only starts at bracket-depth 0; a continuation line inside `[]`/`{}`/`()` is part of the
// current rule's body, so multi-line composite bodies stay intact.
const RULE_HEADER = /^[A-Za-z][A-Za-z0-9_-]*\s*(<[^>]*>)?\s*(\/\/=|\/=|=)/;
function netDepth(line: string): number {
  let d = 0;
  for (const ch of line) {
    if (ch === "(" || ch === "[" || ch === "{") d++;
    else if (ch === ")" || ch === "]" || ch === "}") d--;
  }
  return d;
}
function splitRules(example: string): string[] {
  const rules: string[] = [];
  let cur = "";
  let depth = 0;
  for (const line of example.split("\n")) {
    const isHeader = depth === 0 && RULE_HEADER.test(line.trim());
    if (isHeader && cur.trim() !== "") {
      rules.push(cur);
      cur = "";
    }
    cur += (cur ? "\n" : "") + line;
    depth += netDepth(line);
  }
  if (cur.trim() !== "") rules.push(cur);
  return rules.map(r => r.trim()).filter(r => r.length);
}

interface ParsedRule { name: string; generic: boolean; op: string; rhs: string; text: string }
function parseRule(text: string): ParsedRule | null {
  const m = text.match(/^([A-Za-z][A-Za-z0-9_-]*)\s*(<[^>]*>)?\s*(\/\/=|\/=|=)\s*([\s\S]*)$/);
  if (!m) return null;
  return { name: m[1], generic: m[2] !== undefined, op: m[3], rhs: m[4].trim(), text };
}

// A rule name is REFERENCED if it appears as a word-boundary token in any OTHER rule's text (its own
// header self-mention excluded). The root is the sole rule referenced by nobody.
function referencedBy(name: string, others: ParsedRule[]): boolean {
  const re = new RegExp(`\\b${name.replace(/[-]/g, "\\-")}\\b`);
  return others.some(o => re.test(o.text));
}

interface Filler { feature: string; expr: string; aux: string[] }
interface Skip { feature: string; reason: string }

const fillers: Filler[] = [];
const skipped: Skip[] = [];

for (const f of matrix.features) {
  const ex = (f.example ?? "").trim();
  const skip = (reason: string) => skipped.push({ feature: f.id, reason });
  if (ex === "") { skip("empty example"); continue; }
  if (f.id.startsWith("ext.")) { skip("extern/raw-bytes placeholder needs a user-provided type, not a self-contained filler"); continue; }
  // An extern-scope directive's example depends on its `example_extern_stub` (a DIRECTORY input the
  // single-file composer cannot express) — the bare example dangles an undefined reference, so every
  // composition would be a vacuous parse error. Registered as skipped, same principle as `ext.*`.
  if (f.example_extern_stub !== undefined) { skip("extern-scope directive: example depends on a directory-input stub, not self-contained"); continue; }

  const ruleTexts = splitRules(ex);
  const parsed = ruleTexts.map(parseRule);
  if (parsed.some(p => p === null) || parsed.length === 0) { skip("could not parse into rules"); continue; }
  const rules = parsed as ParsedRule[];

  const names = rules.map(r => r.name);
  if (new Set(names).size !== names.length) { skip("duplicate rule names (choice-extension / redefinition)"); continue; }

  const roots = rules.filter(r => !referencedBy(r.name, rules.filter(o => o !== r)));
  if (roots.length !== 1) { skip(`expected exactly one root rule, found ${roots.length}`); continue; }
  const root = roots[0];

  if (root.op !== "=") { skip(`root uses \`${root.op}\` (not a plain type/group assignment)`); continue; }
  if (root.generic) { skip("generic-parameterized root; RHS is not a standalone expression"); continue; }
  if (root.rhs.includes(";")) { skip("DSL comment in root RHS; not a clean reusable expression"); continue; }
  if (root.rhs.includes("\n")) { skip("multi-line root RHS; not a single reusable expression"); continue; }

  const aux = rules.filter(r => r !== root).map(r => r.text);
  if (aux.some(a => a.includes("\n"))) { skip("multi-line auxiliary rule; kept out to preserve batch renaming"); continue; }
  if (aux.some(a => a.includes(";"))) { skip("DSL comment in an auxiliary rule (may reference user-supplied code); not self-contained"); continue; }

  fillers.push({ feature: f.id, expr: root.rhs, aux });
}

// --- legality data --------------------------------------------------------------------------------
// The containment matrix enumerates only the structurally INTERESTING (role, feature) cells; trivial
// primitive-as-member cells are deliberately omitted as implicitly allowed. The composer therefore
// treats the matrix as a BLACKLIST: any (role, filler) pair composes UNLESS it appears in `disallowed`.
// `legal` (the spec="allowed" rows) is exported for template↔matrix drift protection: every role
// template in the Rust harness must name a role that has at least one modelled allowed cell.
const pairKey = (role: string, feature: string) => `${role}\t${feature}`;
const legalSet = new Set<string>();
const disallowedSet = new Set<string>();
for (const c of matrix.containment) {
  if (c.spec === "allowed") legalSet.add(pairKey(c.role, c.feature));
  else if (c.spec === "disallowed") disallowedSet.add(pairKey(c.role, c.feature));
}
const toPairs = (s: Set<string>) =>
  [...s]
    .map(k => { const [role, feature] = k.split("\t"); return { role, feature }; })
    .sort((a, b) => (a.role < b.role ? -1 : a.role > b.role ? 1 : a.feature < b.feature ? -1 : a.feature > b.feature ? 1 : 0));
const legal = toPairs(legalSet);
const disallowed = toPairs(disallowedSet);

fillers.sort((a, b) => (a.feature < b.feature ? -1 : a.feature > b.feature ? 1 : 0));
skipped.sort((a, b) => (a.feature < b.feature ? -1 : a.feature > b.feature ? 1 : 0));

const artifact = stableJson({ disallowed, fillers, legal, skipped });

console.log(
  `recombination ingredients: ${fillers.length} filler(s), ${legal.length} legal + ${disallowed.length} disallowed (role,feature) pair(s), ${skipped.length} skipped feature(s)`,
);

if (CHECK) {
  if (!existsSync(OUT)) {
    console.log(`SNAPSHOT DRIFT: ${OUT} is missing — run \`bun run project_recombination.ts\``);
    process.exit(1);
  }
  const cur = readFileSync(OUT, "utf8");
  if (cur !== artifact) {
    console.log(`SNAPSHOT DRIFT: tests/recomb/ingredients.json is stale — run \`bun run project_recombination.ts\` and review`);
    process.exit(1);
  }
  console.log("drift check OK: ingredients.json matches the matrix");
} else {
  mkdirSync(OUT_DIR, { recursive: true });
  writeFileSync(OUT, artifact);
  console.log(`wrote tests/recomb/ingredients.json`);
}
