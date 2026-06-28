# Clear-wins mindmap (testing)

> A living reference for an unattended testing session, distilled and de-duped from a parallel
> discovery pass. It is a **starting map, not gospel** — re-verify against the code before acting,
> and prefer the listed `verify_cmd`. "Auto" = safe for a fresh agent to implement + verify + commit
> unattended (low risk, test-only, no human judgment). "Deferred" = needs a human call, a generator
> change, data sourcing, or it's medium/large.

How the test layers work (see `tests/README.md`): **snapshot** suite = "did generated source
change?" (fast, in-process, `cargo test snapshot_tests`); **integration** suite = "does generated
code compile + round-trip?" (slow, compiles crates, e.g. `cargo test core_no_wasm`). Corpus files
auto-bless via `INSTA_UPDATE=always cargo test snapshot_tests`; the orphan gate is
`cargo insta test --unreferenced=reject`.

---

## Session status

- **Done (committed):** cw1 `sized_int`, cw2 `bool`, cw3 `fixed_value` corpus files (plus the
  earlier roadmap-7/8 baseline + this mindmap). HEAD verified clean: fmt, clippy, `snapshot_tests`,
  orphan gate, and `feature_corpus_compiles` all green.
- **Rejected (NOT clean wins):** cw4 `extern_type` and cw5 `raw_bytes` corpus files. Both emit an
  undefined *user-supplied* type (`ExternFoo` / the raw-bytes type), and `feature_corpus_compiles`
  `cargo check`s **every** corpus file with no skip mechanism — so they break that gate. The extern
  and raw-bytes emit paths are already compile+round-trip tested in `tests/extern-deps` and
  `tests/raw-bytes`. Snapshot-only corpus coverage of them would need a `feature_corpus_compiles`
  skip-list — a test-strategy call → **deferred to maintainer** (see Deferred).
- **Remaining auto queue:** cw6–cw17.

---

## Auto queue (best-first: cheapest + highest-confidence first)

### Theme: corpus coverage (new single-construct snapshots — fastest verify, pure-additive)
Each: drop one tiny `tests/corpus/<stem>.cddl`, then
`INSTA_UPDATE=always cargo test snapshot_tests && cargo test snapshot_tests && cargo insta test --unreferenced=reject`,
then eyeball the new `tests/corpus/snapshots/<stem>/`. No stem collisions with whole-program labels.
Avoid floats (preserve profile hits `unimplemented!`) and fixed `true`/`false` (panics).

- **cw1 — sized/bounded integers** (`sized_int.cddl`):
  `bounded_ints = [u_8: uint .size 1, u_16: uint .le 65535, u_32: 0..4294967295, i_8: -128..127, i_64: int .size 8]`.
  Locks the int-bound → `u8/u16/u32/i8/i64` codegen path (only covered today inside the big `core`).
- **cw2 — bool primitive** (`bool.cddl`): `has_bool = [flag: bool]`. `primitives.cddl` omits bool.
  Use the array form (single-field map / fixed bool both panic today).
- **cw3 — fixed/constant field in a plain struct** (`fixed_value.cddl`):
  `fixed_fields = [a: uint, b: "marker", c: 5]`. Constant-field serialize + `FixedValueMismatch`
  deser, outside the group-choice path. Only `a` becomes a struct field.
- **cw4 — `_CDDL_CODEGEN_EXTERN_TYPE_`** (`extern_type.cddl`):
  `extern_foo = _CDDL_CODEGEN_EXTERN_TYPE_` + `uses_extern = [x: uint, y: extern_foo]`. Snapshots
  don't compile, so the undefined `ExternFoo` is fine; localizes the extern-reference path.
- **cw5 — `_CDDL_CODEGEN_RAW_BYTES_TYPE_`** (`raw_bytes.cddl`):
  `raw = _CDDL_CODEGEN_RAW_BYTES_TYPE_` + `uses_raw = [field: raw]`. Exercises the
  `to_raw_bytes`/`from_raw_bytes` (RawBytesEncoding-trait) emit path, distinct from plain `bytes`.

### Theme: test hygiene (one-liners that fix real defects / dead tests)
- **cw6 — remove duplicated `#[test]` on `string1632`** (`tests/preserve-encodings/tests.rs`,
  the consecutive `#[test]` at ~295-296). Pure noise removal, zero behaviour change.
  Verify: `cargo test preserve_encodings`.
- **cw7 — activate the dead `custom_serialization` test** (`tests/core/tests.rs` ~553): add the
  missing `#[test]`. It's the only round-trip + known-byte coverage of the
  `@custom_serialize`/`@custom_deserialize` DSL and has never run. Body already matches the
  generated serializer. Verify: `cargo test core_no_wasm`.
- **cw8 — replace `#[should_panic]`+`.unwrap()` with `is_err()`** in
  `tests/raw-bytes-preserve/tests.rs` (`foo_too_big`, `foo_too_small`). Bare `#[should_panic]`
  passes on *any* panic; `from_raw_bytes` returns `Err` (try_into) on wrong length, so `is_err()` is
  exact and strictly stronger. Verify: `cargo test raw_bytes_preserve`.

### Theme: assertion strengthening (turn no-op constructions into real assertions)
- **cw9 — complete the no-op `enums` round-trips** (preserve + core). Both currently build a value
  and assert nothing (`irregular_bytes` unused in preserve ~676; unused `enums` binding in core
  ~171). Add `from_cbor_bytes`→`to_cbor_bytes` byte-equality (preserve) and `deser_test(&enums)`
  (core), mirroring sibling tests. Verify: `cargo test preserve_encodings && cargo test core_no_wasm`.

### Theme: negative-path / rejection coverage (TESTING_ROADMAP item 8 "room to grow")
All extend `tests/core/tests.rs` using the `tests/deser_test` byte helpers; each keeps an
`is_ok()` baseline so a reject can't pass for the wrong reason. Verify: `cargo test core_no_wasm`.
Note: cw10 and cw11 both touch `structural_rejects` — apply additively / one at a time.
- **cw10 — broaden wrong-major-type rejection** (absorbs the "text+bytes slots of Foo" residual):
  outer container type (map where array expected), Foo's text slot fed bytes, Foo's bytes slot fed
  uint, `Hash` (bytes) fed uint, `WrapperTable` (map) fed array.
- **cw11 — duplicate map keys rejected** (`DuplicateKey`): `WrapperTable` dup with a definite map
  (no read_elems pre-check); struct-map (`TableArrMembers`) dup needs an **indefinite** map (a
  definite one fails earlier as a length mismatch).
- **cw12 — missing required map key** (`MandatoryFieldMissing`) on `TableArrMembers`, again via an
  **indefinite** map (definite → `DefiniteLenMismatch` first). Empty inner `map_def(0)`/`arr_def(0)`
  are valid for the field types.
- **cw13 — indefinite/definite length-framing errors**: `DefiniteLenMismatch` (header too long via
  finish()), `EndingBreakMissing` (indefinite tail not a Break), `BreakInDefiniteLen` (Break inside a
  definite struct-map). The existing "array too short" already covers the short case.

### Theme: static-runtime coverage (shipped runtime never executed by any test)
- **cw14 — `DeserializeError` / `Key` Display formatting** (`error.rs` `fmt_indent` + `Key::Display`):
  in `tests/core/tests.rs`, drive `unwrap_err().to_string()` and assert on **stable substrings**
  (not whole strings) — TagMismatch+Some-location (`Foo2` wrong tag), annotate-chaining
  (`Foo2.opt_text`), `DefiniteLenMismatch` None-location + `expected:` sub-branch (`Foo`),
  `MandatoryFieldMissing`+`Key::Str` (`Bar` empty indefinite map). Verify: `cargo test core_no_wasm`.
- **cw15 — `RawBytesEncoding::to_raw_hex`/`from_raw_hex`** (`raw_bytes_encoding.rs` default methods,
  incl. the invalid-hex → `InvalidStructure` branch): in `tests/raw-bytes/tests.rs`, `use crate::RawBytesEncoding;`
  then a round-trip on `PubKey` plus an invalid-hex and wrong-length reject. `hex` is already a dep.
  Verify: `cargo test raw_bytes`.

### Theme: test-infra one-liners / env-dependent (last — heavier or toolchain-bound verify)
- **cw16 — run, not just build, the json-gen `export_schemas()`** (`src/integration_tests.rs`,
  the json-gen block ~222-237): change `.arg("build")` → `.arg("run")` (+ rename vars/strings).
  Executes the schema-export `main()` that's currently never run. Affects json / json_preserve /
  multifile_json_preserve. Verify: `cargo test json_preserve`.
- **cw17 — pin u64 round-trip at the JS safe-integer max (2^53-1)** in
  `tests/wasm_json/roundtrip.mjs`: one `check(...)` line with `maybe_num: 9007199254740991` (no CDDL
  change; `maybe_num` is already `Option<u64>`). Judgment-free lock *below* the cliff.
  **Requires wasm-pack + node on PATH** (CI has them). Verify: `cargo test wasm_json_roundtrip`.

---

## Deferred (human judgment / generator change / data sourcing / medium+)

### Behaviour-contract decisions (pin only after a human blesses the intended behaviour)
- **Trailing bytes after a complete value are silently accepted** (`from_cbor_bytes` never checks
  cursor==len). Reject vs accept is a runtime change affecting every generated crate → maintainer
  call. (Asserting `is_err()` today would fail.)
- **u64 > 2^53 JSON divergence**: `to_json` keeps precision, `JSON.parse` truncates,
  `to_json_value()` *throws*. TESTING_ROADMAP item 6's "gap with teeth" — decide whether throw
  (vs bigint vs lossy) is the blessed contract before freezing it. (Merges the two duplicate
  roadmap entries.) The safe sub-part (≤2^53-1) is cw17.
- **Missing-key / indefinite-vs-definite on the `bar` tagged-1337 map** (item 8 residual): fragile
  hand-built valid baseline (tag-1337 nesting, int/text keys, `five:5` constant, float64) **and** an
  indefinite-encoding accept/reject behaviour question. The clean versions are already cw11/cw12/cw13.

### Generator / spec changes (re-bless snapshots; not test-only)
- **Float under the json profile**: floats are snapshotted only under `default` (corpus skips them —
  preserve `unimplemented!`). Covering f64 + serde/schemars needs a json fixture change (+snapshot
  re-bless) or per-file profile selection in `feature_corpus`.
- **OrderedHashMap JSON serde + JsonSchema impls** ship only under json+preserve, but
  `tests/json/input.cddl` has no map type, so they're compiled-but-never-run. Needs a map-bearing
  fixture (re-bless) or a dedicated json+preserve test dir.
- **Re-enable `bool_wrapper` JSON newtype** — blocked on generator issue #223; would re-bless json
  snapshots.
- **Map/set per-feature canonical snapshot** — canonical is whole-program-only by a *documented*
  decision; adding it per-feature either explodes near-duplicate snapshots or needs per-file profile
  selection. Re-litigating the README's choice is a human call.

### Assertion upgrades needing value choices (medium-ish)
- **comment-dsl `assert!(true)` → round-trip** (`group_choice`/`type_choice`/`type_choice_variants`):
  some fixtures take raw CBOR / placeholder data, so a human must pick valid round-trippable values.
- **multifile / extern-deps compile-only dirs → round-trip**: contradicts the explicit maintainer
  "compile-only by design" comment; scope (leaf types vs full `everything` incl. ExternalFoo +
  tagged/map) is a product call.
- **Stubbed `no_key_group` comment-dsl test**: needs identifying which CDDL construct routes through
  the no-key-group emit path before an assertion can be written.

### Test-infra strategy (maintainer call)
- **Snapshot-only corpus entries via a `feature_corpus_compiles` skip-list** (would unblock the
  rejected cw4 `extern_type` / cw5 `raw_bytes`). These emit undefined user-supplied types so they
  can't `cargo check` standalone, but they're the *only* missing **snapshot** coverage of the
  extern + raw-bytes-trait emit paths (the dedicated `tests/extern-deps` / `tests/raw-bytes` dirs
  give compile+round-trip, not snapshots). Adding a small skip-list is ~5 test-only lines, but it
  introduces a "snapshot-only corpus" policy the current docs don't have — bless it first.

### New harness / vectors (medium)
- **Preserve-encodings golden known-answer set**: a new `golden_hex_preserve` mirror with
  hand-derived RFC 8949 indefinite-length vectors — the only known-byte oracle for the preserve path.
  Requires authoring spec-anchored vectors + a new harness entry.

### Note — no action (correcting a mistaken premise)
- **`tests/robustness` is NOT an orphan.** It's wired via `src/robustness_tests.rs`
  (`input_robustness_catalog`, declared in `src/main.rs`) and gated in CI
  (`.github/workflows/build.yml`, `cargo insta test --unreferenced=reject -- snapshot_tests robustness`).
  Do **not** delete it and do **not** add a `run_test` call (wrong layout; inputs are meant to fail
  generation gracefully, not compile).
