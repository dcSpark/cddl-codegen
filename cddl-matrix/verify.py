#!/usr/bin/env python3
"""Mechanical verification gate for the CDDL master matrix (ABNF-authority semantics).

This is the reproducible CI gate that the future Rust test mirrors. It does what the prototype
build_matrix.py does NOT: it RECONCILES the authored overlay against the pinned native sources (the
completeness spine), it PROBES every feature's `example` through the three oracles (execution-
grounded, not code-read), and it WRITES annotations/cddl_codegen.toml from those probe results. It
emits verify_report.json and exits nonzero on a HARD FAILURE.

AUTHORITY MODEL (RFC 9682 ABNF + the reference parser):
  - The ABNF in sources/cddl-1-1-update.abnf is the grammar AUTHORITY.
  - The REFERENCE (ruby) parser is authoritative for example VALIDITY: spec_valid := ruby accepts.
  - The rust `cddl` crate is CORROBORATION only. ruby-accepts-but-rust-rejects is a PARSER
    LIMITATION (recorded, NOT a hard fail, NOT spec-invalid) — e.g. `h'cafe'` (lowercase hex).

HARD failures (exit nonzero):
  - FABRICATED production : a feature.production that is neither a real ABNF production name (from
                           sources/cddl-1-1-update.abnf) nor the recognized `prelude` pseudo-prod.
  - COMPLETENESS GAP      : (a) a prelude type name with no `prelude.<name>` feature (or an orphan
                           `prelude.*`); OR (b) a `type2` ABNF *alternative* with no covering
                           feature row (per-ALTERNATIVE completeness — this is what missed the #7
                           gap; per-production-name checks could not).
  - LINK-INTEGRITY error  : a `feature.encodings` entry that resolves to no `enc.*` id, a
                           `feature.roles` entry that resolves to no `role.*` id, or a containment
                           `role`/`feature` that resolves to no master id.
  - SPEC-INVALID example  : a feature example the REFERENCE parser rejects (we authored it as valid
                           CDDL but ruby disagrees). Unless explicitly allow-listed as a genuine
                           reference-vs-ABNF conflict (CONFLICT_ALLOWLIST -> uncertain), this fails.
  - CONTAINMENT CONTRADICTION : a containment example whose REFERENCE-observed allow/disallow
                           differs from the declared `spec`.

Oracles (paths/commands per the matrix runbook):
  A (reference, authoritative) ruby `cddl <f> generate 1`            exit 0 = valid CDDL
  B (corroborating)            rust `cddl compile-cddl --cddl <f>`   exit 0 = valid CDDL
  support                      cddl-codegen `--input=<f> --wasm=false` 0=supported 101=panic/unsupported other=error

Run from cddl-matrix/:  python3 build_matrix.py && python3 verify.py
"""
import csv, json, sys, glob, os, re, subprocess, tempfile

try:
    import tomllib as toml  # py3.11+
except ModuleNotFoundError:
    import tomli as toml     # pip install tomli

HERE = os.path.dirname(os.path.abspath(__file__))
os.chdir(HERE)

# --- oracle locations -----------------------------------------------------------------------------
CODEGEN_DIR = "/home/sebdev20/Documents/git/cddl-codegen"
RUST_CDDL = "/home/sebdev20/Documents/git/cddl/target/debug/cddl"
PRELUDE_PSEUDO = "prelude"  # not an ABNF production; reconciled separately against cddl.prelude
PROBE_TIMEOUT = 120  # seconds per oracle invocation

# --- F1: language-profile axis --------------------------------------------------------------------
# `profile` is the RFC that introduced a feature's CAPABILITY — NOT merely the grammar line. A pure
# grammar-line refactor keeps the older profile: e.g. RFC 9682 SPLIT `#7` into its own production, but
# `#7.n` was always expressible via RFC 8610's generic `#DIGIT` alternative, so `type2.major7` stays
# RFC8610. The ONLY genuinely-new RFC 9682 capability is the type-valued tag head-number
# (`head-number = uint / ("<" type ">")`, `type2.tag_head_type`) — inexpressible in 8610.
# cddl-codegen targets the RFC 8610 capability set. A feature whose `profile` is newer than this target
# AND that cddl-codegen rejects is `out_of_profile` (outside what the tool targets), NOT `unsupported`
# (a gap WITHIN the target). Control-op extension RFCs (9090/9165/9741) are a separate registry axis
# whose support is probed per operator, so they are deliberately NOT part of this grammar-version rank.
CDDL_CODEGEN_TARGET_PROFILE = "RFC8610"
GRAMMAR_PROFILE_RANK = {"RFC8610": 0, "RFC9682": 1}


def profile_newer_than_target(profile):
    return (GRAMMAR_PROFILE_RANK.get(profile or "RFC8610", 0)
            > GRAMMAR_PROFILE_RANK.get(CDDL_CODEGEN_TARGET_PROFILE, 0))

# Genuine reference-vs-ABNF conflicts: examples the ABNF permits but the REFERENCE parser rejects,
# which we deliberately keep flagged as `uncertain` (human review) rather than hard-failing. Empty
# today — every authored example parses under the reference oracle. (id -> reason).
CONFLICT_ALLOWLIST = {}


def resolve_ruby_cddl():
    """Literal path, else $(ruby -e 'puts Gem.user_dir')/bin/cddl."""
    literal = "/home/sebdev20/.local/share/gem/ruby/3.0.0/bin/cddl"
    if os.path.exists(literal):
        return literal
    try:
        user_dir = subprocess.run(
            ["ruby", "-e", "puts Gem.user_dir"], capture_output=True, text=True
        ).stdout.strip()
        cand = os.path.join(user_dir, "bin", "cddl")
        if os.path.exists(cand):
            return cand
    except FileNotFoundError:
        pass
    return None


RUBY_CDDL = resolve_ruby_cddl()


def load_toml(path):
    with open(path, "rb") as fh:
        return toml.load(fh)


# ==================================================================================================
# 1. LOAD the merged matrix exactly as build_matrix.py does.
# ==================================================================================================
features = [f for p in sorted(glob.glob("features/*.toml")) for f in load_toml(p).get("feature", [])]
roles = load_toml("roles.toml").get("role", [])
contain = [c for p in sorted(glob.glob("containment/*.toml")) for c in load_toml(p).get("contain", [])]
encodings = load_toml("encodings.toml").get("encoding", [])

feature_ids = {f["id"] for f in features}
role_ids = {r["id"] for r in roles}
enc_ids = {e["id"] for e in encodings}

control_ops = []
with open("sources/cddl-control-operators.csv", newline="") as fh:
    for row in csv.DictReader(fh):
        name = row["Name"].strip()
        control_ops.append({
            "id": "ctl." + name.lstrip("."),
            "name": name,
            "rfc": row["Reference"].strip().strip("[]"),
        })

# ==================================================================================================
# 2. RECONCILE against sources/ (the completeness spine) — BIDIRECTIONAL grammar lint (F2).
#
# The ABNF is a bidirectional completeness LINT, not the closed-world spine; the real feature space
# is grammar ∪ prelude ∪ control-op registry (∪ tag registry, later). The two directions are:
#   FORWARD  (source -> feature): every `type2` ABNF alternative has >=1 covering feature row.
#            (Hard-gated in §2d via `type2_uncovered`; soft per-alternative logging for the rest.)
#   BACKWARD (feature -> source): every feature's `production` resolves to a real ABNF production,
#            the `prelude` pseudo-production, or the IANA control-op registry — i.e. no feature is
#            invented with no upstream source. (Hard-gated here via `fabricated`.)
# ==================================================================================================
with open("sources/cddl-1-1-update.abnf", encoding="utf-8") as fh:
    abnf_text = fh.read()

# 2a. ABNF production names: lines matching `^name =`.
abnf_productions = set()
for line in abnf_text.splitlines():
    m = re.match(r"^([A-Za-z][A-Za-z0-9_-]*)\s*=", line)
    if m:
        abnf_productions.add(m.group(1))

# BACKWARD lint: a feature's `production` must resolve to one of the three first-party sources
# (ABNF grammar production / `prelude` pseudo-production / IANA control-op registry name).
controlop_prod_names = ({co["name"] for co in control_ops}
                        | {co["name"].lstrip(".") for co in control_ops})
fabricated = []
for f in features:
    prod = f.get("production")
    if prod == PRELUDE_PSEUDO:
        continue  # resolves to the prelude (validated against cddl.prelude in 2b, not the ABNF)
    if prod in abnf_productions:
        continue  # resolves to a grammar production
    if prod in controlop_prod_names:
        continue  # resolves to the IANA control-op registry
    fabricated.append({"id": f.get("id"), "production": prod})

# 2b. Prelude type names: lines matching `^name =` in sources/cddl.prelude.
with open("sources/cddl.prelude", encoding="utf-8") as fh:
    prelude_text = fh.read()
prelude_names = []
for line in prelude_text.splitlines():
    m = re.match(r"^([A-Za-z][A-Za-z0-9_.-]*)\s*=", line)
    if m:
        prelude_names.append(m.group(1))
prelude_name_set = set(prelude_names)

prelude_feature_ids = {f["id"] for f in features if f.get("production") == PRELUDE_PSEUDO}

gaps = []
for name in prelude_names:
    if f"prelude.{name}" not in prelude_feature_ids:
        gaps.append({"kind": "missing_prelude_feature", "name": name,
                     "expected_id": f"prelude.{name}"})
for fid in sorted(prelude_feature_ids):
    nm = fid[len("prelude."):]
    if nm not in prelude_name_set:
        gaps.append({"kind": "orphan_prelude_feature", "id": fid, "name": nm})

# 2c. LINK INTEGRITY — every feature.encodings / feature.roles entry, and every containment
# role/feature, must resolve to a defined master id. (Blind spot the report flagged: a typo'd
# `encodings`/`roles` link or containment ref used to pass silently.)
link_errors = []
for f in features:
    for eid in f.get("encodings", []):
        if eid not in enc_ids:
            link_errors.append({"kind": "encoding", "id": f.get("id"), "ref": eid})
    for rid in f.get("roles", []):
        if rid not in role_ids:
            link_errors.append({"kind": "role", "id": f.get("id"), "ref": rid})
for c in contain:
    if c.get("role") not in role_ids:
        link_errors.append({"kind": "containment_role", "id": c.get("id"), "ref": c.get("role")})
    if c.get("feature") not in feature_ids:
        link_errors.append({"kind": "containment_feature", "id": c.get("id"), "ref": c.get("feature")})

# 2d. PER-ALTERNATIVE completeness for the grammar axis.
# Parse the ABNF into {production -> [alternative strings]} for the alternation productions, then
# assert each alternative is covered by >=1 feature row of that production. `type2` is the HARD gate
# (this is the axis that missed the #7 alternative). Other productions are BEST-EFFORT: coverage is
# computed and logged so nothing passes silently, but they do not break the gate (one ABNF
# alternative legitimately maps to several feature rows, or to repetition sub-rows the matcher can't
# align textually).


def strip_comment(s):
    out, in_q = [], False
    for ch in s:
        if ch == '"':
            in_q = not in_q
        if ch == ';' and not in_q:
            break
        out.append(ch)
    return "".join(out).rstrip()


def split_top_alts(s):
    """Split an ABNF RHS on top-level `/` (outside quotes and outside ()/[]/{} groups)."""
    alts, buf, depth, in_q = [], [], 0, False
    for ch in s:
        if in_q:
            buf.append(ch)
            if ch == '"':
                in_q = False
        elif ch == '"':
            in_q = True
            buf.append(ch)
        elif ch in "([{":
            depth += 1
            buf.append(ch)
        elif ch in ")]}":
            depth -= 1
            buf.append(ch)
        elif ch == "/" and depth == 0:
            alts.append("".join(buf))
            buf = []
        else:
            buf.append(ch)
    if buf:
        alts.append("".join(buf))
    return [a.strip() for a in alts if a.strip()]


def production_alternatives(name):
    """Extract the alternatives of an ABNF production from sources/cddl-1-1-update.abnf, relying on
    the one-alternative-per-line layout (first alt on the `=` line; continuations begin with `/`)."""
    out, in_block = [], False
    for raw in abnf_text.splitlines():
        m = re.match(r"^([A-Za-z][A-Za-z0-9_-]*)\s*=\s*(.*)$", raw)
        if m:
            if m.group(1) == name:
                in_block = True
                out.append(strip_comment(m.group(2)))
            elif in_block:
                break  # a different production starts -> block ends
            continue
        if in_block:
            s = raw.strip()
            if s == "":
                break
            out.append(strip_comment(s))  # keep leading '/' so join+split recovers alternatives
    if not in_block:
        return None
    return split_top_alts(" ".join(x for x in out if x))


def normalize_alt(s):
    """Canonicalize an alternative for matching: drop the inline comment, drop every `S`
    (optional-whitespace) production token wherever it appears (standalone or glued to a paren,
    e.g. `*(S`), and join the rest with no spaces. Feature `alt` strings are verbatim-ish from the
    ABNF, so this aligns the two. (`S` is never an identifier in CDDL's ABNF, so `\\bS\\b` is safe.)"""
    s = re.sub(r"\bS\b", "", strip_comment(s))
    return "".join(s.split())


# productions to analyze for alternative coverage (type2 hard; the rest best-effort/logged).
ALT_PRODUCTIONS = ["type2", "value", "rangeop", "occur", "memberkey", "group", "grpchoice",
                   "grpent", "type", "type1", "assignt", "assigng", "rule", "genericparm",
                   "genericarg", "head-number"]

alt_coverage = {}
for prod in ALT_PRODUCTIONS:
    alts = production_alternatives(prod)
    feat_norms = {normalize_alt(f["alt"]) for f in features
                  if f.get("production") == prod and f.get("alt")}
    rows = [f["id"] for f in features if f.get("production") == prod]
    covered, uncovered = [], []
    for a in (alts or []):
        (covered if normalize_alt(a) in feat_norms else uncovered).append(a)
    alt_coverage[prod] = {
        "abnf_alternatives": alts or [],
        "feature_rows": sorted(rows),
        "covered": covered,
        "uncovered": uncovered,
        "modeled": len(rows) > 0,
    }

# HARD: type2 must have every ABNF alternative covered by a feature row.
type2_uncovered = alt_coverage["type2"]["uncovered"]

# ==================================================================================================
# 3. PROBE each feature's example through the three oracles.
# ==================================================================================================
probe_dir = tempfile.mkdtemp(prefix="cddl_verify_")
probe_file = os.path.join(probe_dir, "probe.cddl")
cc_out = os.path.join(probe_dir, "cc_out")


def run_exit(cmd, cwd=None):
    try:
        r = subprocess.run(cmd, cwd=cwd, stdout=subprocess.DEVNULL,
                           stderr=subprocess.DEVNULL, timeout=PROBE_TIMEOUT)
        return r.returncode
    except subprocess.TimeoutExpired:
        return -1


def oracles(example):
    """Run all three oracles on a CDDL snippet; return (ruby_exit, rust_exit, codegen_exit)."""
    with open(probe_file, "w", encoding="utf-8") as fh:
        fh.write(example + "\n")
    a = run_exit([RUBY_CDDL, probe_file, "generate", "1"]) if RUBY_CDDL else -2
    b = run_exit([RUST_CDDL, "compile-cddl", "--cddl", probe_file])
    c = run_exit(["cargo", "run", "-q", "--",
                  "--input=" + probe_file, "--output=" + cc_out, "--wasm=false"],
                 cwd=CODEGEN_DIR)
    return a, b, c


def derive(feature_id, profile, ruby_exit, rust_exit, codegen_exit):
    valid_a = ruby_exit == 0            # REFERENCE (authoritative)
    valid_b = rust_exit == 0            # corroborating
    spec_valid = valid_a               # ABNF-authority: the reference decides validity
    parser_limitation = valid_a and not valid_b   # ruby/ABNF accept, rust rejects
    # Support classification. On spec-valid input, ANY nonzero cddl-codegen exit means the construct
    # is not handled: a panic (101) and a parse/lex reject (other, e.g. its bundled cddl-crate lexer
    # rejecting a newer-RFC syntax) are both "unsupported" -- just different failure modes. There is
    # no "uncertain" support bucket: nonzero-on-valid == unsupported.
    if codegen_exit == 0:
        support, support_detail = "supported", "exit 0"
    elif codegen_exit == 101:
        support, support_detail = "unsupported", "panic (exit 101)"
    else:
        support, support_detail = "unsupported", f"rejected at parse/lex (exit {codegen_exit})"
    out_of_profile = spec_valid and support != "supported" and profile_newer_than_target(profile)
    if not spec_valid:
        # reference rejects an example we authored as valid CDDL.
        status = "uncertain" if feature_id in CONFLICT_ALLOWLIST else "spec_invalid"
    elif out_of_profile:
        # F1: spec-valid, but its grammar profile is newer than cddl-codegen's target AND
        # cddl-codegen rejects it -> outside what the tool targets, NOT a within-profile gap.
        status = "out_of_profile"
    else:
        status = support   # supported | unsupported (no spec-valid-but-uncertain bucket)
    return {
        "valid_a": valid_a, "valid_b": valid_b, "spec_valid": spec_valid,
        "parser_limitation": parser_limitation, "support": support,
        "support_detail": support_detail, "out_of_profile": out_of_profile, "status": status,
    }


probe_results = []
for f in sorted(features, key=lambda x: x["id"]):
    a, b, c = oracles(f["example"])
    profile = f.get("profile", "RFC8610")
    d = derive(f["id"], profile, a, b, c)
    probe_results.append({
        "id": f["id"], "production": f.get("production"), "profile": profile,
        "example": f["example"], "ruby": a, "rust": b, "codegen": c, **d,
    })

# Containment corroboration (spec oracles only), REFERENCE-authority: observed = allowed iff the
# reference accepts. A rust disagreement is a parser limitation (recorded, non-fatal). A
# contradiction (reference-observed != declared spec) is a hard fail.
containment_corroboration = []
for c in sorted([x for x in contain if x.get("example")], key=lambda x: x["id"]):
    with open(probe_file, "w", encoding="utf-8") as fh:
        fh.write(c["example"] + "\n")
    a = run_exit([RUBY_CDDL, probe_file, "generate", "1"]) if RUBY_CDDL else -2
    b = run_exit([RUST_CDDL, "compile-cddl", "--cddl", probe_file])
    observed = "allowed" if a == 0 else "disallowed"      # reference is authoritative
    parser_limitation = (a == 0) != (b == 0)              # rust disagrees with the reference
    contradiction = observed != c.get("spec")
    containment_corroboration.append({
        "id": c["id"], "spec_declared": c.get("spec"), "spec_observed": observed,
        "ruby": a, "rust": b, "parser_limitation": parser_limitation,
        "contradiction": contradiction, "example": c["example"],
    })

# ==================================================================================================
# 4. WRITE annotations/cddl_codegen.toml from the probe results (execution-grounded).
# ==================================================================================================
def ok(exit_code):
    return "ok" if exit_code == 0 else "fail"


def toml_str(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


anno_lines = [
    "# cddl-codegen support, keyed by master feature id. EXECUTION-GROUNDED: generated by verify.py",
    "# from live oracle probes (NOT hand-read from the generator source). Do not edit by hand — re-run",
    "#   python3 build_matrix.py && python3 verify.py",
    "# to regenerate. Each row is the result of running the feature's minimal `example` through:",
    "#   ruby  cddl ... generate 1            (spec-validity A, authoritative / reference)",
    "#   rust  cddl compile-cddl              (spec-validity B, corroborating only)",
    "#   cddl-codegen --input=... --wasm=false (support: exit 0=supported, 101=panic/unsupported, else error)",
    "# status: supported | unsupported | out_of_profile | uncertain.",
    "#   out_of_profile = the feature's grammar profile is NEWER than cddl-codegen's TARGET profile AND",
    "#         cddl-codegen rejects it (it is outside what the tool targets, NOT a gap within it).",
    "#   uncertain = spec-valid but a genuine reference-vs-ABNF conflict. A `rust parser limitation`",
    "#         note means the reference (ruby/ABNF) accepts the example but the rust cddl crate rejects",
    "#         it (e.g. lowercase `h'cafe'`); that is corroboration noise, not a support/validity verdict.",
    "#",
    "# TARGET PROFILE: cddl-codegen tracks ~RFC 8610 (the RFC 8610 grammar). It does NOT implement the",
    "#   RFC 9682 grammar additions (the `#7` split; the type-valued tag head-number,",
    "#   `head-number = uint / (\"<\" type \">\")`). Features tagged `profile = \"RFC9682\"` that cddl-codegen",
    "#   rejects are therefore `out_of_profile`, not `unsupported`. (Control-op extension RFCs",
    "#   9090/9165/9741 are a separate registry axis whose support is probed per operator.)",
    "#",
    "# CONSUMER NOTES (cddl-codegen-specific facts kept OUT of the pure-spec master, recorded here):",
    "#   * `T / null` type choice -> cddl-codegen emits Option<T> (a consumer special-case of the",
    "#     ordinary `type = type1 *(\"/\" type1)` production, NOT a distinct ABNF alternative).",
    "#   * prelude `float` (float16-32 / float64) -> cddl-codegen maps to Rust f64.",
    "",
]
for pr in probe_results:
    ev = f"probe: cddl-codegen {pr.get('support_detail', 'exit ' + str(pr['codegen']))}; ruby={ok(pr['ruby'])} rust={ok(pr['rust'])}"
    if pr["parser_limitation"]:
        ev += " (rust parser limitation: reference/ABNF accept)"
    if pr["status"] == "out_of_profile":
        ev += (f" (out of profile: feature profile {pr['profile']} is newer than cddl-codegen target "
               f"{CDDL_CODEGEN_TARGET_PROFILE})")
    anno_lines.append("[[support]]")
    anno_lines.append(f"id = {toml_str(pr['id'])}")
    anno_lines.append(f"status = {toml_str(pr['status'])}")
    anno_lines.append(f"evidence = {toml_str(ev)}")
    anno_lines.append("")

os.makedirs("annotations", exist_ok=True)
with open("annotations/cddl_codegen.toml", "w", encoding="utf-8") as fh:
    fh.write("\n".join(anno_lines).rstrip() + "\n")

# ==================================================================================================
# 5. EMIT verify_report.json, print summary + UNCERTAIN list, exit nonzero on hard failures.
# ==================================================================================================
spec_invalid = [pr for pr in probe_results if pr["status"] == "spec_invalid"]
parser_limitations = [pr["id"] for pr in probe_results if pr["parser_limitation"]]
containment_parser_limitations = [c["id"] for c in containment_corroboration if c["parser_limitation"]]
containment_contradictions = [c for c in containment_corroboration if c["contradiction"]]

uncertain = sorted({pr["id"] for pr in probe_results if pr["status"] == "uncertain"})
out_of_profile = sorted({pr["id"] for pr in probe_results if pr["status"] == "out_of_profile"})

report = {
    "gaps": gaps,
    "fabricated": fabricated,
    "link_errors": link_errors,
    "type2_uncovered_alternatives": type2_uncovered,
    "alternative_coverage": alt_coverage,
    "spec_invalid": [pr["id"] for pr in spec_invalid],
    "out_of_profile": out_of_profile,
    "parser_limitations": sorted(parser_limitations),
    "probe_results": probe_results,
    "containment_corroboration": containment_corroboration,
    "containment_contradictions": [c["id"] for c in containment_contradictions],
    "containment_parser_limitations": sorted(containment_parser_limitations),
    "target_profile": CDDL_CODEGEN_TARGET_PROFILE,
    "summary": {
        "features": len(features),
        "roles": len(roles),
        "containment": len(contain),
        "encodings": len(encodings),
        "control_ops": len(control_ops),
        "abnf_productions": len(abnf_productions),
        "prelude_names": len(prelude_names),
        "supported": sum(1 for pr in probe_results if pr["status"] == "supported"),
        "unsupported": sum(1 for pr in probe_results if pr["status"] == "unsupported"),
        "out_of_profile": len(out_of_profile),
        "uncertain": len(uncertain),
        "fabricated": len(fabricated),
        "gaps": len(gaps),
        "link_errors": len(link_errors),
        "type2_alternatives": len(alt_coverage["type2"]["abnf_alternatives"]),
        "type2_covered": len(alt_coverage["type2"]["covered"]),
        "type2_uncovered": len(type2_uncovered),
        "spec_invalid": len(spec_invalid),
        "parser_limitations": len(parser_limitations),
        "containment_contradictions": len(containment_contradictions),
        "containment_parser_limitations": len(containment_parser_limitations),
    },
}
with open("verify_report.json", "w", encoding="utf-8") as fh:
    json.dump(report, fh, indent=2, sort_keys=True)
    fh.write("\n")

s = report["summary"]
print("=" * 80)
print("CDDL matrix verify gate (ABNF-authority)")
print("=" * 80)
print(f"features probed     : {s['features']}")
print(f"target profile      : {CDDL_CODEGEN_TARGET_PROFILE} (out-of-profile features excluded from gaps)")
print(f"ABNF productions    : {s['abnf_productions']}  prelude names: {s['prelude_names']}")
print(f"support (codegen)   : supported={s['supported']} unsupported={s['unsupported']} "
      f"out_of_profile={s['out_of_profile']} uncertain={s['uncertain']}")
print(f"reconcile (BIDIRECTIONAL grammar lint):")
print(f"  forward  (source->feature): type2 alternatives covered "
      f"{s['type2_covered']}/{s['type2_alternatives']} (uncovered={s['type2_uncovered']})")
print(f"  backward (feature->source): fabricated={s['fabricated']} "
      f"(feature.production resolving to no ABNF/prelude/control-op source)")
print(f"  prelude gaps={s['gaps']}  link_errors={s['link_errors']}")
print(f"type2 per-alt       : {s['type2_covered']}/{s['type2_alternatives']} covered "
      f"(uncovered={s['type2_uncovered']})")
print(f"spec-invalid (ref-rejected examples): {s['spec_invalid']}")
print(f"parser limitations (rust): features={s['parser_limitations']} "
      f"containment={s['containment_parser_limitations']}")
print(f"containment         : contradictions={s['containment_contradictions']}")

# Best-effort alternative-coverage log (informational; only type2 gates).
print("\nALTERNATIVE COVERAGE (type2 gates; others best-effort/logged):")
for prod in ALT_PRODUCTIONS:
    cov = alt_coverage[prod]
    n_alt = len(cov["abnf_alternatives"])
    n_cov = len(cov["covered"])
    tag = "HARD" if prod == "type2" else "soft"
    if not cov["modeled"]:
        print(f"  - {prod:12s} [{tag}] NOT MODELED (0 feature rows) — {n_alt} ABNF alternative(s)")
    else:
        extra = ""
        if cov["uncovered"]:
            extra = "  uncovered: " + "; ".join(cov["uncovered"])
        print(f"  - {prod:12s} [{tag}] {n_cov}/{n_alt} alternatives covered{extra}")

if fabricated:
    print("\nFABRICATED productions (backward lint: not in ABNF, not `prelude`, not control-op registry):")
    for x in fabricated:
        print(f"  - {x['id']}: production '{x['production']}'")
if gaps:
    print("\nCOMPLETENESS GAPS (prelude):")
    for g in gaps:
        print(f"  - {g}")
if link_errors:
    print("\nLINK-INTEGRITY ERRORS:")
    for e in link_errors:
        print(f"  - {e['kind']}: {e['id']} -> unknown '{e['ref']}'")
if type2_uncovered:
    print("\nTYPE2 PER-ALTERNATIVE GAPS (no covering feature row):")
    for a in type2_uncovered:
        print(f"  - {a}")
if spec_invalid:
    print("\nSPEC-INVALID EXAMPLES (REFERENCE parser rejects an authored example):")
    for pr in spec_invalid:
        print(f"  - {pr['id']}: ruby={ok(pr['ruby'])} rust={ok(pr['rust'])}  ex={pr['example']!r}")
if containment_contradictions:
    print("\nCONTAINMENT CONTRADICTIONS (reference-observed spec != declared spec):")
    for c in containment_contradictions:
        print(f"  - {c['id']}: declared={c['spec_declared']} observed={c['spec_observed']}")
if parser_limitations or containment_parser_limitations:
    print("\nPARSER LIMITATIONS (reference/ABNF accept, rust rejects — informational, non-fatal):")
    for u in sorted(parser_limitations):
        print(f"  - {u}")
    for u in sorted(containment_parser_limitations):
        print(f"  - {u} (containment)")

print("\nOUT_OF_PROFILE (" + str(len(out_of_profile)) + f"; profile newer than {CDDL_CODEGEN_TARGET_PROFILE} "
      "and cddl-codegen rejects — excluded from gaps, NOT unsupported):")
for u in out_of_profile:
    pr = next(p for p in probe_results if p["id"] == u)
    print(f"  - {u} (profile {pr['profile']}; {pr['support_detail']})")

print("\nUNCERTAIN (" + str(len(uncertain)) + "):")
for u in uncertain:
    print(f"  - {u}")

print("\nwrote annotations/cddl_codegen.toml and verify_report.json")

hard_fail = (bool(fabricated) or bool(gaps) or bool(link_errors) or bool(type2_uncovered)
             or bool(spec_invalid) or bool(containment_contradictions))
if hard_fail:
    print("\nRESULT: FAIL (hard failure — see above)")
    sys.exit(1)
print("\nRESULT: PASS")
sys.exit(0)
