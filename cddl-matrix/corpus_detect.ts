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
 * real parse. Consistent with verify.ts, which also hand-scans CDDL text. The upgrade path EXISTS in this
 * file (`rolesIn` shells `examples/ast_roles.rs`, the real cddl-crate parse, and project_corpus already
 * uses it for role-keyed covers); if text-scan precision bites again, move the feature-axis evidence onto
 * that floor rather than growing the regexes. Prelude and control-op ids are matched by NAME token
 * (auto-covers ~90 ids); structural constructs use a small hand table. Supported ids with no possible
 * text detector are declared in NO_DETECTOR (exported), not silently treated as absent.
 *
 * The @-DSL channel is the ONE exception to the recall-first contract: `scanDslDirectives` is a
 * PRECISE sequential mirror of comment_ast.rs (not a regex approximation), because the dsl set backs
 * check A's cover drift-check where a FALSE CREDIT — not a miss — is the failure mode. The mirror's
 * directive VOCABULARY is lockstep-gated: selfCheck compares `MIRRORED_DIRECTIVES` against
 * comment_ast.rs's `tag("@…")` literals, so adding/removing a directive on either side fails every
 * importer (project_corpus = fast tier). ARG-GRAMMAR drift within an unchanged set has no detector —
 * that residual is why the AST-floor upgrade below wins over growing this mirror.
 *
 * Known-approximate cases audited and accepted under the above (each would be fixed by the AST-floor
 * upgrade, not by more regex): hyphenated rule names; ctl-name credit from dotted ids / range bounds;
 * `type.enum`'s missing end anchor; hex-literal / inline-group / prelude-name-as-key over-credit; the
 * role floor missing `genericarg.multiple`.
 */
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { Glob } from "bun";

const HERE = import.meta.dir;
const CORPUS = `${HERE}/../tests/corpus`;
const CODEGEN = resolve(HERE, ".."); // the cddl-codegen repo (hosts examples/ast_roles.rs)

const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as {
  features: { id: string; production?: string }[];
  control_operators: { id: string }[];
};
const preludeNames = matrix.features.filter(f => f.production === "prelude").map(f => f.id.slice("prelude.".length));
const ctlNames = matrix.control_operators.map(c => c.id.slice("ctl.".length));
const reEsc = (s: string) => s.replace(/[.*+?^${}()|[\]\\-]/g, "\\$&");

// --- comment handling: CDDL comment is `;`..EOL (but not inside a "text" or '…' bytes literal).
//     Split each line into code (RFC constructs live here) and comment (the @-DSL lives here).
//     String-literal INTERIORS are masked out of the code channel: literal content is data, not
//     code — a doc string mentioning `uint` or a URL's `//` must not credit a feature. The quote
//     delimiters themselves are kept so the value.text / value.bytes / quoted-key detectors still
//     see that a literal is present. Handles \-escapes and CDDL's '…' byte strings (whose BCHAR
//     set legally contains `;`). ---
// A single stateful pass over the WHOLE text (not per-line): a CDDL `'…'` byte string's BCHAR set
// legally spans newlines, so quote state must persist ACROSS line breaks. A per-line reset scanned a
// multi-line literal's continuation lines as code and fabricated comment/dsl credit from a `;` inside
// the literal. Newlines are preserved in each channel (so line-anchored code detectors still work),
// except inside a literal, where the interior — newlines included — is masked out of the code channel.
function scan(text: string): { code: string; comment: string } {
  let code = "";
  let comment = "";
  let q: string | null = null;
  let inComment = false;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (q) {
      if (ch === "\\") i++; // escaped char: stays masked, don't let \" close the literal
      else if (ch === q) {
        code += ch; // closing delimiter kept
        q = null;
      }
      // interior (newlines included) masked
      continue;
    }
    if (ch === "\n") {
      inComment = false;
      code += "\n";
      comment += "\n";
    } else if (inComment) {
      comment += ch;
    } else if (ch === '"' || ch === "'") {
      q = ch;
      code += ch;
    } else if (ch === ";") {
      inComment = true;
    } else {
      code += ch;
    }
  }
  return { code, comment };
}
const codeOf = (t: string) => scan(t).code;
const commentsOf = (t: string) => scan(t).comment;

// rule LHS names (so a reference to one is a type2.typename "appears"); strip generics on the LHS.
const ruleNames = (code: string): string[] =>
  [...code.matchAll(/^[ \t]*([A-Za-z_]\w*)\s*(?:<[^>]*>)?\s*=/gm)].map(m => m[1]);

// --- structural detectors over the comment-stripped code. Approximate by design (see header). ---
const STRUCT: { id: string; hit: (code: string) => boolean }[] = [
  { id: "type2.array", hit: c => c.includes("[") },
  { id: "type2.map", hit: c => c.includes("{") },
  { id: "type2.tag", hit: c => /#6(\.\d+)?\s*\(/.test(c) },
  // a parenthesized TYPE: parens with no member key (`:`) and no entry list (`,`) — those are group
  // parens. The digit lookbehind excludes tag parens `#6.n(…)`; the `[`/`{` lookbehind excludes a
  // keyless inline GROUP directly inside a container (`[(uint)]`, `[(a // b)]`), which the cddl-crate
  // AST classifies as grpent.inline_group, not type2.parenthesized. A ctl arg like `.size (0..32)` IS
  // a ParenthesizedType, so crediting it (preceded by a space) is correct, not an over-match.
  { id: "type2.parenthesized", hit: c => /(?<![\d\[{])\((?![^)]*[:,])[^)]*\)/.test(c) },
  { id: "type.choice", hit: c => /(?<!\/)\/(?![\/=])/.test(c) },          // single `/`, not `//` or the `/=` extend op
  // all-fixed-value choice (c-style enum): `= v / v [/ v]…` where every alternative is a literal value
  { id: "type.enum", hit: c => /=\s*(-?\d+|"[^"]*")(\s*\/\s*(-?\d+|"[^"]*"))+/.test(c) },
  { id: "group.choice", hit: c => /\/\/(?!=)/.test(c) },                  // `//`, not the `//=` extend op
  { id: "assignt.extend", hit: c => /(?<!\/)\/=/.test(c) },               // `a /= …` type-socket extension
  { id: "assigng.extend", hit: c => /\/\/=/.test(c) },                    // `g //= …` group-socket extension
  { id: "memberkey.bareword", hit: c => /\b[A-Za-z_]\w*\s*:/.test(c) },
  { id: "memberkey.value", hit: c => /(^|[\s,{])(-?\d+|"[^"]*")\s*:/.test(c) },
  { id: "memberkey.type1", hit: c => c.includes("=>") },
  { id: "memberkey.cut", hit: c => c.includes("^") },
  // occurrence per RFC 8610 `occur = [uint] "*" [uint] / "+" / "?"`: whitespace after `+`/around
  // `*` is optional, and either `*` bound may be absent — a one-sided `2*`/`*5` is still bounded.
  { id: "occur.optional", hit: c => c.includes("?") },
  { id: "occur.one_or_more", hit: c => /(^|[\s,[({])\+/.test(c) },        // leading class keeps `1e+9`-style exponents out
  { id: "occur.bounded", hit: c => /\d\*|\*\d/.test(c) },                 // a digit directly adjacent to `*` (bounds are adjacent per the ABNF)
  { id: "occur.zero_or_more", hit: c => /(?<!\d)\*(?!\d)/.test(c) },      // bare `*` (no bound on either side)
  { id: "rangeop.exclusive", hit: c => c.includes("...") },
  { id: "rangeop.inclusive", hit: c => /(?<!\.)\.\.(?!\.)/.test(c) },
  { id: "value.text", hit: c => /"[^"]*"/.test(c) },
  { id: "value.bytes", hit: c => /'/.test(c) },  // h'…' / b64'…' / bare '…' — post-masking, `'` only occurs as a bytes-literal delimiter
  { id: "value.number", hit: c => /(^|[\s,/[(])-?\d+\b/.test(c) },        // incl. range/ctl-arg numbers (over-credits — that's the point)
  { id: "genericparm.type", hit: c => /^[ \t]*[A-Za-z_]\w*<[^>]+>\s*=/m.test(c) },
  { id: "genericarg.type", hit: c => /\b[A-Za-z_]\w*<[^>=]+>(?!\s*=)/.test(c) },
  { id: "genericparm.multiple", hit: c => /^[ \t]*[A-Za-z_]\w*<[^>]*,[^>]*>\s*=/m.test(c) },  // generic DEF with 2+ params (comma in `<…>`)
  { id: "genericarg.multiple", hit: c => /\b[A-Za-z_]\w*<[^>=]*,[^>=]*>(?!\s*=)/.test(c) },    // instantiation with 2+ args
  { id: "genericarg.type1", hit: c => /\b[A-Za-z_]\w*<[^>=]*\.\.[^>=]*>(?!\s*=)/.test(c) },     // instantiation whose arg is a type1 expr (a range)
  { id: "grpent.inline_group", hit: c => /[[{]\s*\(/.test(c) },                                 // a parenthesized group inline as an entry: `[(…)]`
  { id: "type2.value", hit: c => /(^|[\s,/[(])(-?\d+\b|"[^"]*"|(h|b64)')/.test(c) }, // a literal at a type position
];

// Matrix-`supported` ids featuresIn structurally CANNOT detect (no STRUCT / prelude / ctl /
// typename / dsl path can produce them — they're implicit in ordinary syntax the scan can't
// individuate). Declared so the blindness is stated instead of silent: check D can never demand
// covers for these, and a feature-axis [[cover]] naming one would always fail check A until a
// detector (or the AST floor) takes over. The AST role floor below already classifies
// grpent.groupname / grpent.member correctly — the gap is only in the text-scan channel.
export const NO_DETECTOR = new Set(["grpchoice.sequence", "grpent.groupname", "grpent.member"]);

// --- @-DSL directive scanner: a faithful mirror of comment_ast.rs's
//     `rule_metadata = many0(whitespace_then_tag)` (comment_ast.rs:200-221).
//
// Fed PER COMMENT LINE: in the cddl crate a `Comments` is `Vec<&str>` with one element per `;`-line
// (parser.rs pushes each comment line separately), and `metadata_from_comments` runs `rule_metadata`
// on EACH element independently then merges. So a directive cannot span two comment lines, and @doc
// prose on one line does not run into the next line's directives — the per-line split here matches.
//
// The scanner replaces a `matchAll(/@\w+/g)` that credited every `@word` on a directive-leading line:
// that over-credited a real directive id buried in trailing prose, which could keep a dsl cover green
// in check A. many0 instead parses SEQUENTIALLY and STOPS at the first token that isn't a recognized
// directive (an unknown leading `@foo …` therefore credits nothing and kills the rest of the line).
//
// Each tag is nom `tag(..)` = PREFIX match (comment_ast has no word boundary): `@namefoo` parses as
// `@name` + arg `foo`, exactly as comment_ast credits it. The table below is in comment_ast's `alt`
// order; arg grammars mirror each tag_* fn:
//   @name / @custom_serialize / @custom_deserialize : ws* then take_while1(!ws) — arg REQUIRED (fails if absent)
//   @newtype                                         : OPTIONAL ws* then take_while1(!ws && !@)
//   @no_alias / @used_as_elem / @custom_json         : none
//   @used_as_key                                     : optional flavor words `hash`/`ord` up to the next `@`/EOL
//                                                      (comment_ast PANICS on any other word, so a fixture with
//                                                      prose there cannot generate — the mirror credits nothing)
//   @doc                                             : take_while1(c != '@') — prose runs to the next `@` (arg REQUIRED)
type TagParse = (s: string) => { id: string; rest: string } | null;
// The directive VOCABULARY DSL_TAGS models, kept beside it so the selfCheck lockstep tripwire can
// demand set equality with comment_ast.rs's `tag("@…")` literals. Adding a directive to either
// side without the other fails every importer's selfCheck (project_corpus runs in the fast tier).
const MIRRORED_DIRECTIVES = new Set([
  "@name", "@newtype", "@no_alias", "@used_as_key", "@used_as_elem",
  "@raw_bytes_flavor", "@custom_json", "@custom_serialize", "@custom_deserialize", "@doc",
]);
const ws = (s: string) => s.replace(/^\s+/, ""); // take_while(char::is_whitespace)
const argRequired = (id: string, tag: string): TagParse => s => {
  if (!s.startsWith(tag)) return null;
  const after = ws(s.slice(tag.length));
  const m = after.match(/^\S+/); // take_while1(!ws) — must consume ≥1
  return m ? { id, rest: after.slice(m[0].length) } : null;
};
const noArg = (id: string, tag: string): TagParse => s => (s.startsWith(tag) ? { id, rest: s.slice(tag.length) } : null);
const DSL_TAGS: TagParse[] = [
  argRequired("dsl.name", "@name"),
  // @newtype: optional getter arg (chars that are neither ws nor `@`); on no arg, comment_ast returns
  // NewType(None) with the input trim_start()'d (so a following directive is still reachable).
  s => {
    if (!s.startsWith("@newtype")) return null;
    const after = s.slice("@newtype".length);
    const m = ws(after).match(/^[^\s@]+/);
    return m ? { id: "dsl.newtype", rest: ws(after).slice(m[0].length) } : { id: "dsl.newtype", rest: ws(after) };
  },
  noArg("dsl.no_alias", "@no_alias"),
  // @used_as_key: consumes the optional flavor words (`hash`/`ord`) so a directive AFTER them is
  // still reachable, mirroring tag_used_as_key's loop. On any OTHER word comment_ast panics (the
  // fixture couldn't have generated), so the mirror refuses the credit rather than false-crediting.
  // The credited id mirrors comment_ast's DemandSet, whose `bare`/`hash`/`ord` flags are mutually
  // exclusive between bare and flavored (`demand.bare` is set only when NO flavor word followed):
  // no flavor -> dsl.used_as_key; otherwise the narrowed sibling id for the OR-merged flavor set
  // present (hash / ord / hash_ord — order- and duplicate-insensitive, exactly like the flags).
  s => {
    if (!s.startsWith("@used_as_key")) return null;
    let rest = s.slice("@used_as_key".length);
    let hash = false, ord = false;
    while (true) {
      const afterWs = ws(rest);
      if (afterWs === "" || afterWs.startsWith("@")) {
        const id = hash && ord ? "dsl.used_as_key.hash_ord"
          : hash ? "dsl.used_as_key.hash"
            : ord ? "dsl.used_as_key.ord"
              : "dsl.used_as_key";
        return { id, rest: afterWs };
      }
      const m = afterWs.match(/^[^\s@]+/)!;
      if (m[0] === "hash") hash = true;
      else if (m[0] === "ord") ord = true;
      else return null; // comment_ast panics here — no credit
      rest = afterWs.slice(m[0].length);
    }
  },
  noArg("dsl.used_as_elem", "@used_as_elem"),
  // @raw_bytes_flavor: bare no-arg tag (valid only on a `_CDDL_CODEGEN_EXTERN_TYPE_` generic).
  noArg("dsl.raw_bytes_flavor", "@raw_bytes_flavor"),
  noArg("dsl.custom_json", "@custom_json"),
  argRequired("dsl.custom_serialize", "@custom_serialize"),
  argRequired("dsl.custom_deserialize", "@custom_deserialize"),
  // @doc: take_while1(c != '@') — prose (incl. leading ws) runs to the next `@` or EOL; fails if the
  // very next char is `@` (so `@doc@newtype` credits nothing, matching comment_ast).
  s => {
    if (!s.startsWith("@doc")) return null;
    const after = s.slice("@doc".length);
    const m = after.match(/^[^@]+/);
    return m ? { id: "dsl.doc", rest: after.slice(m[0].length) } : null;
  },
];
function scanDslDirectives(line: string): string[] {
  const out: string[] = [];
  let s = line;
  while (true) {
    s = ws(s); // whitespace_then_tag skips leading whitespace before the alt
    let hit: { id: string; rest: string } | null = null;
    for (const p of DSL_TAGS) if ((hit = p(s))) break; // alt: first full match wins
    if (!hit) break; // many0 stops at the first token that isn't a recognized directive
    out.push(hit.id);
    s = hit.rest;
  }
  return out;
}

export interface Detected { rfc: Set<string>; ctl: Set<string>; dsl: Set<string> }

export function featuresIn(cddl: string): Detected {
  const code = codeOf(cddl);
  const comments = commentsOf(cddl);
  const rfc = new Set<string>();
  const ctl = new Set<string>();
  const dsl = new Set<string>();

  // prelude types by name token (same hyphen guard as the ctl loop below: CDDL ids may contain `-`,
  // so `\b` alone would credit `float16` inside `float16-32` and `any` inside `cbor-any`)
  for (const n of preludeNames) if (new RegExp(`(?<![\\w-])${reEsc(n)}(?![\\w-])`).test(code)) rfc.add(`prelude.${n}`);
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
  // CDDL_CODEGEN profile: @-DSL directives (in comments) -> dsl.<name>; sentinel typenames -> ext.<name>.
  // Precision channel (the file's one RECALL exception, justified in the header): the dsl set must
  // PREDICT exactly what comment_ast credits, because it backs check A's cover drift-check where a
  // FALSE CREDIT is the failure mode. So we run scanDslDirectives — a faithful mirror of
  // comment_ast.rs's `rule_metadata = many0(whitespace_then_tag)` — per comment line.
  for (const line of comments.split(/\r?\n/)) for (const id of scanDslDirectives(line)) dsl.add(id);
  if (code.includes("_CDDL_CODEGEN_EXTERN_TYPE_")) dsl.add("ext.extern");
  if (code.includes("_CDDL_CODEGEN_RAW_BYTES_TYPE_")) dsl.add("ext.raw_bytes");

  return { rfc, ctl, dsl };
}

// ============================================================================================
// ROLE-AWARE detection (the per-cell role × feature layer). featuresIn (above) is a text scan: it sees THAT a construct
// appears, not in WHICH container role. A regex can't track the enclosing role across nesting, so this
// shells out to a real parse — examples/ast_roles.rs walks the `cddl` crate AST (=0.9.1, the exact one
// cddl-codegen builds with) and emits (role, node-kind) records. Here we map those node-kinds onto
// matrix feature ids (the editorial "what is a feature" mapping stays in TS), yielding the role-keyed
// floor: per fixture, the set of `<feature-id>@<role-id>` cells it exercises.
//
// Wired into project_corpus.ts (check A's role-keyed cover branch imports rolesIn). dsl.*/ext.* still
// come from the text scan above (they live in comments / are sentinel typenames, which the parser
// strips/erases); the AST floor covers the RFC + control-op constructs, where roles matter.

interface RoleRec { role: string; kind: string; name?: string }

// Map one AST record to a `<feature-id>@<role-id>` cell, or null if it has no matrix feature.
// Most node-kinds ARE matrix feature ids already; only `typename`/`ctlop` need name resolution.
function recToCell(r: RoleRec): string | null {
  const role = `role.${r.role}`;
  let feature: string | null;
  if (r.kind === "typename") {
    if (!r.name) return null;
    if (r.name === "_CDDL_CODEGEN_EXTERN_TYPE_") feature = "ext.extern";
    else if (r.name === "_CDDL_CODEGEN_RAW_BYTES_TYPE_") feature = "ext.raw_bytes";
    else if (preludeNames.includes(r.name)) feature = `prelude.${r.name}`;
    else feature = "type2.typename";
  } else if (r.kind === "ctlop") {
    feature = r.name ? `ctl.${r.name}` : null;
  } else {
    feature = r.kind; // type2.array, value.number, occur.optional, rangeop.inclusive, ...
  }
  return feature ? `${feature}@${role}` : null;
}

// Invoke the AST-role helper. With file args -> { path: records[] }; with stdin -> records[].
function astRoles(args: string[], stdin?: string): unknown {
  const proc = Bun.spawnSync(["cargo", "run", "-q", "--example", "ast_roles", "--", ...args], {
    cwd: CODEGEN,
    stdout: "pipe",
    stderr: "pipe",
    stdin: stdin === undefined ? undefined : new TextEncoder().encode(stdin),
  });
  if (!proc.success)
    throw new Error(`ast_roles failed (exit ${proc.exitCode}):\n${proc.stderr.toString()}`);
  return JSON.parse(proc.stdout.toString());
}

// Role-keyed floor for a set of corpus fixtures (filenames under tests/corpus). One batched cargo run.
export function rolesIn(files: string[]): Map<string, Set<string>> {
  const raw = astRoles(files.map(f => `${CORPUS}/${f}`)) as Record<string, RoleRec[]>;
  const out = new Map<string, Set<string>>();
  for (const f of files) {
    const cells = new Set<string>();
    for (const rec of raw[`${CORPUS}/${f}`] ?? []) { const c = recToCell(rec); if (c) cells.add(c); }
    out.set(f, cells);
  }
  return out;
}

// Role-keyed cells for a single inline CDDL doc (used by the self-check; stdin mode).
export function rolesInStr(cddl: string): Set<string> {
  const cells = new Set<string>();
  for (const rec of astRoles([], cddl) as RoleRec[]) { const c = recToCell(rec); if (c) cells.add(c); }
  return cells;
}

// --- self-check (ponytail: one runnable check on the non-trivial logic) ---
function selfCheck() {
  const a = featuresIn("arr = [uint, text, bytes]");
  for (const id of ["type2.array", "prelude.uint", "prelude.text", "prelude.bytes"])
    if (!a.rfc.has(id)) throw new Error(`selfCheck: expected ${id} in array.cddl`);
  if (a.rfc.has("prelude.int")) throw new Error("selfCheck: \\bint\\b false-matched inside uint");
  const t = featuresIn("tagged = #6.42(text)");
  if (!t.rfc.has("type2.tag")) throw new Error("selfCheck: missing type2.tag");
  if (!featuresIn("g = [+ uint]").rfc.has("occur.one_or_more")) throw new Error("selfCheck: missing occur.one_or_more in `[+ uint]`");
  const g = featuresIn("pair<k, v> = [k, v]\nx = pair<uint, tstr>\ninst = foo<1..10>");
  for (const id of ["genericparm.multiple", "genericarg.multiple", "genericarg.type1"])
    if (!g.rfc.has(id)) throw new Error(`selfCheck: missing ${id}`);
  if (featuresIn("u = foo<uint>").rfc.has("genericarg.type1")) throw new Error("selfCheck: genericarg.type1 false-matched a plain typename arg");
  if (!featuresIn("g = [(uint, tstr)]").rfc.has("grpent.inline_group")) throw new Error("selfCheck: missing grpent.inline_group in `[(…)]`");
  const s = featuresIn("hash = bytes .size (0..32)");
  if (!s.ctl.has("ctl.size")) throw new Error("selfCheck: missing ctl.size");
  if (!s.rfc.has("rangeop.inclusive")) throw new Error("selfCheck: missing rangeop.inclusive");
  const d = featuresIn("x = uint ; @newtype @custom_json");
  if (!d.dsl.has("dsl.newtype") || !d.dsl.has("dsl.custom_json")) throw new Error("selfCheck: missing DSL id");
  if (d.rfc.has("type1.ctlop")) throw new Error("selfCheck: DSL `@custom...` leaked into code as a ctlop");
  if (!featuresIn("foo = _CDDL_CODEGEN_EXTERN_TYPE_\nbar = [x: foo]").dsl.has("ext.extern")) throw new Error("selfCheck: missing ext.extern");

  // regression cases from the test-framework audit (draft/test-setup/bugs-corpus-detect.md)
  if (!featuresIn("foo = (uint)").rfc.has("type2.parenthesized")) throw new Error("selfCheck: `(uint)` (the matrix's own example) not detected as a parenthesized type");
  if (featuresIn("inner = (a: uint, b: uint)").rfc.has("type2.parenthesized")) throw new Error("selfCheck: group-definition parens false-credited type2.parenthesized");
  if (featuresIn("g = [(uint, tstr)]").rfc.has("type2.parenthesized")) throw new Error("selfCheck: inline group false-credited type2.parenthesized");
  if (featuresIn("tagged = #6.42(text)").rfc.has("type2.parenthesized")) throw new Error("selfCheck: tag parens false-credited type2.parenthesized");
  const lit = featuresIn('note = "contains uint and null, see http://example.com"');
  for (const id of ["prelude.uint", "prelude.null", "group.choice"])
    if (lit.rfc.has(id)) throw new Error(`selfCheck: string-literal content credited ${id}`);
  if (!lit.rfc.has("value.text")) throw new Error("selfCheck: literal masking broke value.text");
  if (!featuresIn('x = "\\"" ; @newtype').dsl.has("dsl.newtype")) throw new Error("selfCheck: escaped quote swallowed a real comment directive");
  if (featuresIn("x = 'ab;@newtype cd'").dsl.has("dsl.newtype")) throw new Error("selfCheck: `;` inside a bytes literal fabricated a comment directive");
  if (!featuresIn("magic = 'rawbytes'").rfc.has("value.bytes")) throw new Error("selfCheck: bare '…' byte string not detected as value.bytes");
  const ext2 = featuresIn("a = int\na /= tstr");
  if (!ext2.rfc.has("assignt.extend")) throw new Error("selfCheck: missing assignt.extend for `/=`");
  if (ext2.rfc.has("type.choice")) throw new Error("selfCheck: `/=` false-credited type.choice");
  const extg = featuresIn("tcpopts //= (2: tstr)");
  if (!extg.rfc.has("assigng.extend")) throw new Error("selfCheck: missing assigng.extend for `//=`");
  if (extg.rfc.has("group.choice")) throw new Error("selfCheck: `//=` false-credited group.choice");
  if (featuresIn("x = float16-32").rfc.has("prelude.float16")) throw new Error("selfCheck: hyphen-prefix prelude name false credit (float16 in float16-32)");
  if (!featuresIn("x = float16-32").rfc.has("prelude.float16-32")) throw new Error("selfCheck: hyphen guard broke exact hyphenated prelude match");
  for (const [src, id] of [
    ["g = [+uint]", "occur.one_or_more"],
    ["h = {+ tstr => uint}", "occur.one_or_more"],
    ["i = [2* uint]", "occur.bounded"],
    ["j = [*5 uint]", "occur.bounded"],
    ["k = [((* uint))]", "occur.zero_or_more"],
  ] as const)
    if (!featuresIn(src).rfc.has(id)) throw new Error(`selfCheck: ${id} missing in \`${src}\``);
  if (featuresIn("x = 1e+9").rfc.has("occur.one_or_more")) throw new Error("selfCheck: exponent `+` false-credited occur.one_or_more");
  if (featuresIn("x = uint ; unlike @newtype, this is plain").dsl.has("dsl.newtype")) throw new Error("selfCheck: prose comment mention credited a dsl directive");
  if (featuresIn("x = uint ; ask user@example about it").dsl.size) throw new Error("selfCheck: mid-prose @word invented a dsl id");
  // dsl-prose residual: the sequential scanner mirrors comment_ast's many0(whitespace_then_tag) —
  // a real directive id buried in trailing prose after a NON-@doc directive must NOT be credited
  // (comment_ast's many0 stops at the first non-directive token). @used_as_key goes further: its
  // flavor loop PANICS on a non-flavor word, so trailing prose there kills the whole credit (the
  // fixture couldn't have generated) — the mirror must credit NOTHING, not stop-after-crediting.
  {
    const r = featuresIn("x = uint ; @used_as_key see @newtype for the alternative").dsl;
    if (r.has("dsl.used_as_key")) throw new Error("selfCheck: @used_as_key with trailing prose was credited (comment_ast panics on a non-flavor word — no credit)");
    if (r.has("dsl.newtype")) throw new Error("selfCheck: @newtype in trailing prose after @used_as_key was over-credited (the dsl-prose residual)");
  }
  // @used_as_key flavor words are consumed as its ARGS, so a directive after them is still parsed;
  // the credited id narrows to the flavor set (mirroring DemandSet), and bare @used_as_elem chains
  // like any no-arg directive.
  {
    const r = featuresIn("x = uint ; @used_as_key hash ord @newtype").dsl;
    if (!r.has("dsl.used_as_key.hash_ord") || !r.has("dsl.newtype")) throw new Error("selfCheck: @used_as_key flavor words must be consumed (crediting the flavor sibling) with the directive after them still parsed");
    if (r.has("dsl.used_as_key")) throw new Error("selfCheck: a flavored @used_as_key must NOT credit the bare id (bare/flavored are mutually exclusive in DemandSet)");
  }
  // Each flavor set credits exactly its narrowed sibling id; bare (no flavor word) stays dsl.used_as_key.
  {
    const bare = featuresIn("x = uint ; @used_as_key").dsl;
    if (!bare.has("dsl.used_as_key") || bare.has("dsl.used_as_key.hash") || bare.has("dsl.used_as_key.ord") || bare.has("dsl.used_as_key.hash_ord"))
      throw new Error("selfCheck: bare @used_as_key must credit dsl.used_as_key only");
    if (!featuresIn("x = uint ; @used_as_key hash").dsl.has("dsl.used_as_key.hash")) throw new Error("selfCheck: @used_as_key hash must credit dsl.used_as_key.hash");
    if (!featuresIn("x = uint ; @used_as_key ord").dsl.has("dsl.used_as_key.ord")) throw new Error("selfCheck: @used_as_key ord must credit dsl.used_as_key.ord");
    // flavor set is order- and duplicate-insensitive, exactly like the OR-merged DemandSet flags.
    if (!featuresIn("x = uint ; @used_as_key ord hash").dsl.has("dsl.used_as_key.hash_ord")) throw new Error("selfCheck: @used_as_key ord hash must credit dsl.used_as_key.hash_ord (order-insensitive)");
  }
  {
    const r = featuresIn("x = uint ; @used_as_elem @newtype").dsl;
    if (!r.has("dsl.used_as_elem") || !r.has("dsl.newtype")) throw new Error("selfCheck: @used_as_elem (+ chained directive) must be credited");
  }
  // LOCKSTEP tripwire (directive-SET drift): DSL_TAGS is a hand mirror of comment_ast.rs's
  // directive grammar, and its `@used_as_elem` gap shipped invisibly because nothing compared the
  // two vocabularies (the selfCheck vectors above are hand-picked, so they drift WITH the mirror).
  // Extract the authority's `tag("@…")` literals and demand set equality with MIRRORED_DIRECTIVES;
  // this fires in every importer (project_corpus = fast tier) the moment a directive is added or
  // removed on either side. ARG-GRAMMAR drift within an unchanged set is NOT catchable here — the
  // AST floor is that residual's fix (tests/TESTING_ROADMAP.md, the twin-implementation drift entry).
  {
    const rust = readFileSync(`${CODEGEN}/src/comment_ast.rs`, "utf8");
    const rustSet = new Set([...rust.matchAll(/\btag\("(@[a-z_]+)"\)/g)].map(m => m[1]));
    const missing = [...rustSet].filter(d => !MIRRORED_DIRECTIVES.has(d)).sort();
    const extra = [...MIRRORED_DIRECTIVES].filter(d => !rustSet.has(d)).sort();
    if (missing.length || extra.length)
      throw new Error(
        `selfCheck: DSL_TAGS drifted from comment_ast.rs's directive set — in authority but not mirrored: ` +
        `[${missing.join(", ")}] · mirrored but not in authority: [${extra.join(", ")}]. Update DSL_TAGS + ` +
        `MIRRORED_DIRECTIVES (+ selfCheck vectors for the new grammar), or better, move the dsl channel onto the AST floor.`,
      );
  }
  // the asymmetric @doc grammar: @doc's prose runs to the next `@`, so a directive AFTER @doc prose
  // IS still parsed (comment_ast.rs tag_comment = take_while1(c != '@')). A naive stop-at-first rule
  // would miss the @newtype here.
  {
    const r = featuresIn("x = uint ; @doc explains things then @newtype").dsl;
    if (!r.has("dsl.doc") || !r.has("dsl.newtype")) throw new Error("selfCheck: @doc prose then @newtype must credit BOTH (asymmetric @doc grammar)");
  }
  // a @newtype getter token is consumed as its ARG, so the following directive is still parsed
  // (mirrors comment_ast.rs parse_comment_newtype_getter_before).
  {
    const r = featuresIn("x = uint ; @newtype my_getter @used_as_key").dsl;
    if (!r.has("dsl.newtype") || !r.has("dsl.used_as_key")) throw new Error("selfCheck: @newtype <getter> @used_as_key must credit BOTH");
  }
  // keyless inline group directly inside a container is grpent.inline_group, NOT type2.parenthesized
  // (the comma form is covered above at line ~265; this is the keyless single-type form).
  if (featuresIn("g = [(uint)]").rfc.has("type2.parenthesized")) throw new Error("selfCheck: keyless inline group `[(uint)]` false-credited type2.parenthesized");
  if (!featuresIn("g = [(uint)]").rfc.has("grpent.inline_group")) throw new Error("selfCheck: keyless inline group `[(uint)]` not detected as grpent.inline_group");
  // a `;` inside a MULTI-LINE `'…'` byte literal must not fabricate a comment/dsl directive (quote
  // state persists across newlines now, not reset per line).
  if (featuresIn("x = 'ab\n;@newtype cd'").dsl.has("dsl.newtype")) throw new Error("selfCheck: `;@newtype` inside a multi-line bytes literal fabricated a comment directive");
}

// Role-aware self-check: the headline case — in `text / null`, `null` is covered as a
// CHOICE-MEMBER, and must NOT be credited at top-level (the exact lie the text-scan floor told).
function roleSelfCheck() {
  const r = rolesInStr("maybe_text = text / null\nholder = [val: maybe_text]");
  const must = [
    "prelude.null@role.choice-member",
    "prelude.text@role.choice-member",
    "type.choice@role.top-level",
    "type2.array@role.top-level",
    "memberkey.bareword@role.map-key",
    "type2.typename@role.array-element",
  ];
  for (const c of must)
    if (!r.has(c)) throw new Error(`roleSelfCheck: expected ${c} — got {${[...r].sort().join(", ")}}`);
  if (r.has("prelude.null@role.top-level"))
    throw new Error("roleSelfCheck: null wrongly credited at top-level (the text-scan lie the role-aware floor fixes)");
}

// Pure-regex regression suite, ~ms: run on EVERY import (not just the CLI), so the CI drift job —
// which consumes featuresIn via project_corpus.ts — executes it too. A detector regression then
// fails the gate itself instead of waiting for someone to run the CLI diagnostic by hand.
// (roleSelfCheck stays CLI-only below: it shells cargo.)
selfCheck();

if (import.meta.main) {
  roleSelfCheck();
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
  console.log(`  ${undetected.map(id => (NO_DETECTOR.has(id) ? `${id} [NO DETECTOR — undetectable by the text scan, see NO_DETECTOR]` : id)).join(", ")}`);
  if (spurious.length) console.log(`SPURIOUS (detected but not a matrix id — detector bug): ${spurious.join(", ")}`);
  console.log(`\nCDDL_CODEGEN profile ids seen across corpus: ${[...new Set(perFixture.flatMap(p => [...p.dsl]))].sort().join(" ")}`);

  // ==========================================================================================
  // ROLE-AWARE floor — the AST walk's (feature, role) cells, vs the text-scan floor above.
  // ==========================================================================================
  console.log(`\n=== role-aware floor (examples/ast_roles.rs, real cddl-crate parse) ===`);
  const roleFloor = rolesIn(files);
  const allCells = new Set<string>();
  for (const cells of roleFloor.values()) for (const c of cells) allCells.add(c);
  const astFeatureIds = new Set([...allCells].map(c => c.split("@")[0]));

  // recall vs the text scan: AST-floor feature ids should (mostly) superset the text-scan RFC+ctl floor.
  // Differences are usually the AST being MORE PRECISE (the text scan over-credits a digit inside a tag
  // number / range bound as value.number); a genuine under-credit would be a finding to surface.
  const textIds = new Set([...seen].filter(id => !id.startsWith("dsl.") && !id.startsWith("ext.")));
  const astOnly = [...astFeatureIds].filter(id => !textIds.has(id)).sort();
  const textOnly = [...textIds].filter(id => !astFeatureIds.has(id)).sort();
  console.log(`role cells: ${allCells.size} across ${roleFloor.size} fixtures; distinct feature ids: ${astFeatureIds.size}`);
  if (textOnly.length) console.log(`  text-scan-only (AST more precise, or a real under-credit — review): ${textOnly.join(", ")}`);
  if (astOnly.length) console.log(`  AST-only (parse catches what the regex missed): ${astOnly.join(", ")}`);

  // the payoff: constructs whose ROLE set differs — the context axis the text scan flattened.
  const byFeature = new Map<string, Set<string>>();
  for (const c of allCells) {
    const [ft, ro] = c.split("@");
    if (!byFeature.has(ft)) byFeature.set(ft, new Set());
    byFeature.get(ft)!.add(ro);
  }
  const multiRole = [...byFeature].filter(([, rs]) => rs.size > 1).sort();
  console.log(`\nconstructs exercised in >1 role (the context axis the flat scan couldn't see):`);
  for (const [ft, rs] of multiRole) console.log(`  ${ft}: ${[...rs].sort().join(", ")}`);

  // the canonical item-6 example: null / fixed-value cells, keyed by role.
  const nullCells = [...allCells].filter(c => /^(prelude\.null|value\.(number|text|bytes)|type2\.value)@/.test(c)).sort();
  console.log(`\nnull / fixed-value cells (item-6 canonical — note the ROLE):`);
  for (const c of nullCells) console.log(`  ${c}`);
}
