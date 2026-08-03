# Manifest change logs

The **single source of truth** for the generated crates' `Cargo.toml`s. One append-only log per
manifest:

| Log | Generates | Derived human-readable view |
|---|---|---|
| `rust.toml` | `rust/Cargo.toml` | `static/Cargo_rust.toml` |
| `wasm.toml` | `wasm/Cargo.toml` | `static/Cargo_wasm.toml` |
| `json_gen.toml` | `wasm/json-gen/Cargo.toml` | `static/Cargo_json_gen.toml` |
| `component.toml` | `component/Cargo.toml` | `static/Cargo_component.toml` |
| `static_runtime.toml` | the `--export-static-crate` target's `Cargo.toml` | `static/Cargo_static_runtime.toml` |

At runtime, cddl-codegen folds a log per-path **last-write-wins** and *merges* the result into
whatever `Cargo.toml` the user already has (keys the log never mentions are never touched — see
`docs/docs/output_format.mdx` for the user-facing contract, and the `src/cargo_manifest.rs` module
docs for the model). Because the log keeps its full history, a key the tool ever managed can never
be silently forgotten: a removed key stays in the log as a permanent tombstone, so users
regenerating over output from any old tool version converge.

## Entry format

```toml
[[change]]
id = 7                                # contiguous from 1 — a gap is a hard error at runtime
path = "dependencies.cbor_event"      # dotted TOML key path
set = '"2.4.0"'                       # a TOML value literal, as a string
```

Each entry has `id`, `path`, and **exactly one** of:

- `set = '<value>'` — tool-owned key, written every run (overwrites user edits by design; the
  exception is `dependencies.*` keys, which merge field-level into an existing entry — the user's
  `optional`/features/compatible pins survive; see `docs/docs/output_format.mdx`)
- `seed = '<value>'` — written only if the key is absent (e.g. `package.version`: the user's bump
  survives)
- `remove = true` — tombstone: delete the key from user manifests (kept in the log forever)

Values are TOML value literals inside a string, so inline tables work:
`set = '{ version = "1.0", features = ["derive"] }'`. The literal name `cddl-lib` anywhere in a log
is substituted with `--lib-name` at runtime.

### The `assert_source` modifier

One optional extra field, valid **only beside `set` on a `dependencies.<name>` path** (either misuse
is a hard error naming the log):

```toml
[[change]]
id = 10
path = "dependencies.cbor_event"
set = '"3.3.0"'
assert_source = true
```

It declares the spec the owner of that dependency's **source axis**: `git`, `rev`, `branch`, `tag`,
`path` and `registry` keys the spec does not itself name are cleared off the user's existing entry
before the field-level merge, and a merge that ends up version-only renders as a plain string, so a
converged manifest is byte-identical to a freshly generated one. The version keeps its normal floor
semantics (a satisfying hand pin survives), and non-source fields (`optional`, extra features) are
untouched.

Reach for it when a `set` **changes where a dependency comes from**. A spec carrying `git` asserts
the source implicitly — nothing else a git spec could mean — so the modifier is only needed going the
other way, back to a registry version: a crates.io version has no cargo key that could carry the
intent, and without the assertion `merge_dep_spec` reads a version-only `set` as a floor and
*preserves* the `{ git, rev }` entry already written into every user manifest. The flip would then
land on fresh output only, which is precisely the failure the log's append-only convergence
guarantee exists to prevent.

## Editing rules (append-only)

- **NEVER edit, delete, or renumber an existing entry.** The log is history; ids must stay
  contiguous from 1 (enforced at runtime and by tests).
- **Change a key** → append a new entry with the same `path` (last write wins).
- **Remove a key** → append a `remove = true` entry. Do *not* delete the original addition —
  deleting history would strand the stale key in existing user manifests, since unmentioned keys
  are deliberately never touched.
- **Exception — correcting a never-valid value.** A recorded value that was never resolvable
  anywhere (e.g. a git `rev` reachable from no remote) is corrected **in place**, not shadowed by
  an append. The append-only rule exists so user manifests CONVERGE, and convergence is a property
  of PATHS: an in-place `set` on the same path still overwrites every already-written manifest, so
  nothing is stranded. Keeping the bad value alive instead makes correctness order-dependent —
  delete or reorder the shadowing entry and a pin nothing can fetch silently returns. Note the
  correction in the entry's comment. This is *not* license to rewrite legitimate value history —
  only values that were defects the day they were written (the `pin_cold_fetch` gate is what
  proves a mentioned rev against its remote).

## Regenerating the derived views

The `static/Cargo_*.toml` files are **generated** from these logs (header included) and are never
read at runtime — they exist so humans and tools browsing `static/` see the current effective
manifest. After editing a log:

```
BLESS_MANIFEST_TEMPLATES=1 cargo test manifest_template_drift
```

The drift gate (`manifest_template_drift` in `src/tests/snapshot_tests.rs`) fails CI if a log and
its derived view disagree. The rendering reuses the exact runtime fold, so the view cannot drift
from the behavior.

Note the logs cover only the *unconditional* keys. Flag/type-conditional dependencies (e.g.
`hashlink` under `--preserve-encodings`, `hex` for byte wrappers) and the
`package.metadata.cddl-codegen.generated-with` version stamp are conditions on the user's input,
not history, so they live in code: `ops_for_*` in `src/cargo_manifest.rs`.
