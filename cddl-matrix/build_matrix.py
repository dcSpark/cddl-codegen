#!/usr/bin/env python3
"""Prototype generator + drift-check for the CDDL master matrix.

Joins the authored TOML overlay with the native sources/ artifacts into a single matrix.json for
universal/cross-language consumption, and verifies invariants (no dangling annotation ids).

This is a PROTOTYPE to demonstrate the 3-layer architecture (native sources + TOML overlay +
generated view). The production version should be a Rust test wired into the suite (serde `toml` +
`serde_json`), so the drift-check runs in CI with no Python dependency.

Run from cddl-matrix/:  python3 build_matrix.py        # writes matrix.json, checks invariants
"""
import csv, json, sys, glob, os
try:
    import tomllib as toml  # py3.11+
except ModuleNotFoundError:
    import tomli as toml     # pip install tomli

HERE = os.path.dirname(os.path.abspath(__file__))
os.chdir(HERE)

def load_toml(path):
    with open(path, "rb") as fh:
        return toml.load(fh)

# --- authored overlay (TOML) --- features/ and containment/ are directories (one file per partition,
# so fan-out authoring agents write disjoint files with no races); merged here deterministically.
features  = [f for p in sorted(glob.glob("features/*.toml")) for f in load_toml(p).get("feature", [])]
roles     = load_toml("roles.toml").get("role", [])
contain   = [c for p in sorted(glob.glob("containment/*.toml")) for c in load_toml(p).get("contain", [])]
encodings = load_toml("encodings.toml").get("encoding", [])
annos     = {os.path.basename(p)[:-5]: load_toml(p).get("support", [])
             for p in glob.glob("annotations/*.toml")}

# --- imported axis (native): control operators derived from the IANA CSV, NOT transcribed ---
control_ops = []
with open("sources/cddl-control-operators.csv", newline="") as fh:
    for row in csv.DictReader(fh):
        name = row["Name"].strip()
        rfc = row["Reference"].strip().strip("[]")
        control_ops.append({
            "id": "ctl." + name.lstrip("."),
            "name": name,
            "rfc": rfc,
            # F1: control ops already carry their introducing RFC; mirror it into `profile` so the
            # profile axis is uniform across features and control operators in matrix.json.
            "profile": rfc,
        })

# --- the unified view ---
matrix = {
    "features": features, "roles": roles, "containment": contain,
    "encodings": encodings, "control_operators": control_ops,
    "annotations": annos,
}

# --- invariant: every annotation id resolves to a real master id ---
master_ids = {x["id"] for x in (features + roles + contain + encodings + control_ops)}
errors = []
for tool, rows in annos.items():
    for r in rows:
        if r["id"] not in master_ids:
            errors.append(f"annotations/{tool}: '{r['id']}' resolves to no master id")

out = json.dumps(matrix, indent=2, sort_keys=True) + "\n"
summary = (f"{len(features)} features, {len(roles)} roles, {len(contain)} containment, "
           f"{len(encodings)} encodings, {len(control_ops)} control-ops (from IANA CSV), "
           f"{sum(len(v) for v in annos.values())} annotations")

# annotation-id invariant (always): every annotation id resolves to a real master id
if errors:
    print("DRIFT CHECK FAILED:")
    for e in errors:
        print("  -", e)
    sys.exit(1)

# F6 — snapshot the synthesis. matrix.json is the committed golden view of the editorial join;
# `--check` regenerates it in-memory and fails if the on-disk golden is stale (an un-regenerated
# overlay edit, or a change in the join logic) — the insta discipline applied to the matrix synthesis,
# not just to the inputs' checksums. CI / verify can run `build_matrix.py --check` as a gate.
if "--check" in sys.argv:
    current = open("matrix.json").read() if os.path.exists("matrix.json") else None
    if current != out:
        print(f"SNAPSHOT DRIFT: matrix.json is stale vs the authored overlay ({summary}).")
        print("Run `python3 build_matrix.py` and review the diff before committing.")
        sys.exit(1)
    print(f"snapshot OK: matrix.json matches the authored overlay ({summary}); annotation ids resolve")
else:
    with open("matrix.json", "w") as fh:
        fh.write(out)
    print(f"matrix.json written: {summary}")
    print("drift check OK: all annotation ids resolve to master ids")
