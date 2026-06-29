# Corpus coverage map — CDDL constructs (GENERATED)

> **GENERATED** by `cddl-matrix/project_corpus.ts` — do not hand-edit. Status (✅/➕/➖/⚠️) is the
> execution-grounded matrix support verdict joined with the corpus overlay (canonical fixture, nuance
> notes, findings) in `cddl-matrix/annotations/corpus/cddl_codegen.toml`. Regenerate after changing
> either; CI fails on overlay drift (a note/cover that contradicts the matrix or the fixtures).

Tracks which CDDL constructs the snapshot **corpus** (`tests/corpus/*.cddl`) exercises, what's
supported-but-untested (a corpus gap to fill), and what the generator does **not** support (the
boundary). The feature universe + support are anchored to the spec (RFC 8610 grammar/prelude + the
IANA control-op registry) and cddl-codegen's vendor profile — not to a self-feature-list, which is what
makes the ➖ boundary rows visible. Sections are derived: **profile → production → id**.

## How this map works

- **Test:** `tests/corpus/<construct>.cddl`, driven by `snapshot_tests::feature_corpus` — each file is
  generated under every flag profile (`default`/`preserve`/`json`) plus an IR dump, and the generated
  *source* is snapshotted. Bless with `INSTA_UPDATE=always cargo test snapshot_tests`.
- **Compile gate:** `integration_tests::feature_corpus_compiles` `cargo check`s every corpus file under
  all three profiles, so a ✅ entry must produce **compiling** Rust under *all* of them.
- **Axis:** the corpus snapshots generated *source*, not wire bytes — wire encodings are golden_hex's
  axis (`tests/golden_hex/COVERAGE.md`, RFC 8949). A ✅ here means "a fixture isolates this construct,"
  not "every encoding of it is asserted."
- **Evidence convention** (stable grep-able anchors, never line numbers) and the spec-anchoring
  rationale live in `cddl-matrix/README.md` — this doc is one projection of that master.
- **RFC reference:** RFC 8610 — <https://www.rfc-editor.org/rfc/rfc8610>; control ops from the IANA
  CDDL control-operators registry (spans RFC 8610/9090/9165/9741).

## Legend

| mark | meaning |
|------|---------|
| ✅ | covered — a corpus fixture isolates this construct |
| ➕ | **supported but untested** — accepted by the generator, no corpus fixture yet (an actionable gap) |
| ➖ | **not supported** — rejected / `panic!` / no handling branch (documents the boundary) |
| ⚠️ | partial — parsed but the semantics aren't honored (accepted, not modeled) |

## RFC 8610 / 9682 grammar + prelude (the spec backbone)

### `assigng` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `assigng.extend` | ⚠️ | Incremental group-choice extension (//=) | group socket plug (//=) is parsed but ignored — same socket-stripping story as the type socket; the Rule::Group arm of parse_rule processes it as a plain inline group with no plug semantics (no distinct code site to anchor) |

### `assignt` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `assignt.extend` | ⚠️ | Incremental type-choice extension (/=) | type socket plug (/=) is parsed but ignored; a $-socket reference is merely stripped ($x -> x)  [`is_type_choice_alternate`] |

### `genericarg` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `genericarg.multiple` | ✅ | Multiple generic arguments | `generics.cddl` |
| `genericarg.type1` | ✅ | Type-expression argument | `generics.cddl` |

### `genericparm` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `genericparm.multiple` | ✅ | Multiple generic parameters | `generics.cddl` |

### `group` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `group.choice` | ✅ | Group choice (//) | `group_choice.cddl` |

### `grpchoice` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `grpchoice.sequence` | ➕ | Group-entry sequence | supported, no corpus fixture (cddl-codegen exit 0) |

### `grpent` (3)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `grpent.groupname` | ➕ | Group-name reference entry | supported, no corpus fixture (cddl-codegen exit 0) |
| `grpent.inline_group` | ⚠️ | Inline (parenthesized) group entry | an inline parenthesized group spliced as an array entry drops all but its FIRST member: `[(uint, tstr)]` generates a 1-field `InlineGroup { index_0: u64 }` (`read_elems(1)`), silently losing the `tstr` (inline_group.cddl snapshot). It parses + compiles (so the matrix probe marks it supported), but the output is wrong — silent data loss. Candidate cddl-codegen fix: inline-group entries aren't flattened into the record.  [`GroupEntry::InlineGroup { .. } => None`] |
| `grpent.member` | ➕ | Member entry (optional occur + optional memberkey + type) | supported, no corpus fixture (cddl-codegen exit 0) |

### `memberkey` (4)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `memberkey.bareword` | ✅ | Bareword memberkey (k:) | `map_struct.cddl` |
| `memberkey.cut` | ➖ | Cut in a => memberkey (^) | explicit cut `^` attaches to a `=>` (Type1) memberkey, which cddl-codegen doesn't support in this form — the example panics. (The IMPLICIT cut on `:`/bareword keys is a separate, supported-but-dropped story — see finding.)  [`Encountered Type1 member key in multi-field map`] |
| `memberkey.type1` | ✅ | Type memberkey (t =>) | `table.cddl` |
| `memberkey.value` | ✅ | Value memberkey (1:) | `value_key.cddl` |

### `occur` (4)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `occur.bounded` | ✅ | Bounded occurrence (n*m) | `occurrence.cddl` |
| `occur.one_or_more` | ✅ | One-or-more occurrence (+) | `occurrence.cddl` |
| `occur.optional` | ✅ | Optional occurrence (?) | `optional.cddl` |
| `occur.zero_or_more` | ✅ | Zero-or-more occurrence (*) | `homogeneous_array.cddl` |

### `prelude` (40)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `prelude.any` | ➖ | any | `x = any` generates `pub type X = Any;` referencing a type defined nowhere — does not compile (caught by the compile-gate; the exit-code-only probe formerly over-credited it). `any` is absent from is_identifier_reserved, so it's treated as an undefined user type.  [`is_identifier_reserved`] |
| `prelude.b64legacy` | ✅ | b64legacy | `prelude.cddl` |
| `prelude.b64url` | ✅ | b64url | `prelude.cddl` |
| `prelude.bigfloat` | ✅ | bigfloat | `prelude.cddl` |
| `prelude.bigint` | ✅ | bigint | `prelude.cddl` |
| `prelude.bignint` | ✅ | bignint | `prelude.cddl` |
| `prelude.biguint` | ✅ | biguint | `prelude.cddl` |
| `prelude.bool` | ✅ | bool | `bool.cddl` |
| `prelude.bstr` | ✅ | bstr | `prelude.cddl` |
| `prelude.bytes` | ✅ | bytes | `primitives.cddl` |
| `prelude.cbor-any` | ➖ | cbor-any | reduces to `any`; rejected  [`unsupported cddl prelude type`] |
| `prelude.decfrac` | ✅ | decfrac | `prelude.cddl` |
| `prelude.eb16` | ➖ | eb16 | extended-bytes prelude type reduces to `any`; rejected  [`unsupported cddl prelude type`] |
| `prelude.eb64legacy` | ➖ | eb64legacy | extended-bytes prelude type reduces to `any`; rejected  [`unsupported cddl prelude type`] |
| `prelude.eb64url` | ➖ | eb64url | extended-bytes prelude type reduces to `any`; rejected  [`unsupported cddl prelude type`] |
| `prelude.encoded-cbor` | ✅ | encoded-cbor | `prelude.cddl` |
| `prelude.false` | ➖ | false | the fixed boolean `false` used as a standalone type panics — same Fixed-type gap as `true`/`null` (fails as a struct member too).  [`should not expose Fixed type in member`] |
| `prelude.float` | ⚠️ | float | de/ser works under default/json, but --preserve-encodings and bounds are unimplemented for floats (so float-bearing types can't be corpus entries — the corpus runs preserve)  [`preserve_encodings is not implemented for float`] |
| `prelude.float16` | ➖ | float16 | no native Rust f16 — the float alias system doesn't handle float16, so it panics even as a struct member (float32/float64 work).  [`should be handled by the alias system instead`] |
| `prelude.float16-32` | ➖ | float16-32 | the float16/float32 choice alias isn't handled by the float alias system (it includes the unsupported float16); panics even as a member.  [`should be handled by the alias system instead`] |
| `prelude.float32` | ⚠️ | float32 | works under default/json as a member, but --preserve-encodings is unimplemented for floats — same limitation as `float` (verified: `holder = [x: float32]` compiles default, fails preserve)  [`preserve_encodings is not implemented for float`] |
| `prelude.float32-64` | ➖ | float32-64 | the float32/float64 choice alias isn't handled by the float alias system (the float-choice aliases are unsupported, though float32/float64 work on their own); panics even as a member.  [`should be handled by the alias system instead`] |
| `prelude.float64` | ⚠️ | float64 | works under default/json as a member, but --preserve-encodings is unimplemented for floats — same limitation as `float` (verified: `holder = [x: float64]` compiles default, fails preserve)  [`preserve_encodings is not implemented for float`] |
| `prelude.int` | ✅ | int | `primitives.cddl` |
| `prelude.integer` | ✅ | integer | `prelude.cddl` |
| `prelude.mime-message` | ✅ | mime-message | `prelude.cddl` |
| `prelude.nil` | ➖ | nil | top-level `x = nil` (fixed null value) panics — same Fixed-type gap as `null`; works as a struct member (`[x: nil]`) but not as a standalone type.  [`should not expose Fixed type in member`] |
| `prelude.nint` | ✅ | nint | `primitives.cddl` |
| `prelude.null` | ➖ | null | top-level `x = null` type panics — cddl-codegen exposes Fixed only as a struct member, not as a standalone type (same Fixed-type gap as the literal values). Its supported choice-member role is the [[cover]] above.  [`should not expose Fixed type in member`] — also ✅ @choice-member (`nullable.cddl`: the `T / null` -> Option<T> nullable pattern) |
| `prelude.number` | ➕ | number | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.regexp` | ✅ | regexp | `prelude.cddl` |
| `prelude.tdate` | ✅ | tdate | `prelude.cddl` |
| `prelude.text` | ✅ | text | `primitives.cddl` |
| `prelude.time` | ➕ | time | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.true` | ➖ | true | the fixed boolean `true` used as a standalone type panics; cddl-codegen exposes fixed values only for serialization, not as types (fails as a struct member too). Same Fixed-type gap as `null`.  [`should not expose Fixed type in member`] |
| `prelude.tstr` | ✅ | tstr | `prelude.cddl` |
| `prelude.uint` | ✅ | uint | `primitives.cddl` |
| `prelude.undefined` | ➖ | undefined | the `undefined` simple value is rejected  [`unsupported cddl prelude type`] |
| `prelude.unsigned` | ✅ | unsigned | `prelude.cddl` |
| `prelude.uri` | ✅ | uri | `prelude.cddl` |

### `rangeop` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `rangeop.exclusive` | ⚠️ | Exclusive range (a...b) | the exclusive upper bound is mis-computed: `a...b` excludes `b` (max valid = b-1), but cddl-codegen emits `max = b+1` — `[v: 0...10]` generates `max: Some(11)`, accepting 10 and 11 which the spec excludes (see exclusive_range.cddl snapshot). It parses + compiles; only the bound is wrong (candidate fix: `range_end + 1` -> `range_end - 1`).  [`range_end + 1`] |
| `rangeop.inclusive` | ✅ | Inclusive range (a..b) | `sized_int.cddl` |

### `rule` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `genericparm.group` | ➖ | Generic group definition | a generic GROUP definition (`set<a> = (* a)`) is rejected — generics are supported on type rules, not on plain groups.  [`Generics not supported on plain groups`] |
| `genericparm.type` | ✅ | Generic type definition | `generics.cddl` |

### `type` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `type.choice` | ✅ | Type choice (/) | `type_choice.cddl` |
| `type.enum` | ✅ | All-fixed-value type choice (c-style enum) | `c_style_enum.cddl` |

### `type1` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `type1.ctlop` | ✅ | Control-operator application (.op) | `bounded_bytes.cddl` |

### `type2` (13)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `genericarg.type` | ✅ | Generic type instantiation | `generics.cddl` |
| `type2.any` | ➖ | Any (#) | bare `#` (any) — Type2 variant unmatched, falls into the catch-all panic  [`Ignoring Type2`] |
| `type2.array` | ✅ | Array | `array.cddl` |
| `type2.choice_from_group` | ➖ | Choice from named group (&) | choice-from-group `&groupname` — unmatched  [`Type2::ChoiceFromGroup`] |
| `type2.choice_from_inline_group` | ➖ | Choice from inline group (&) | choice-from-inline-group `&(...)` — unmatched  [`Type2::ChoiceFromInlineGroup`] |
| `type2.major` | ➖ | Major-type sigil (#N, #N.n) | major-type sigils `#N` / `#N.n` — Type2::DataMajorType unmatched, catch-all panic  [`Ignoring Type2`] |
| `type2.major7` | ➖ | Major-type 7 / simple sigil (#7, #7.n) | `#7` / `#7.n` simple/float sigils — unmatched, catch-all panic  [`Ignoring Type2`] |
| `type2.map` | ✅ | Map | `map_struct.cddl` — canonical = pure struct map; table-style is table.cddl; MIXED struct+table ({a: uint, * k => v}) is unsupported (parsing.rs) |
| `type2.parenthesized` | ✅ | Parenthesized type | `nested_group.cddl` |
| `type2.tag` | ✅ | Tagged data item (#6.n) | `tagged.cddl` |
| `type2.typename` | ✅ | Type reference (with optional generic args) | `type_alias.cddl` |
| `type2.unwrap` | ➖ | Unwrap (~) | unwrap `~` — Type2::Unwrap unmatched, catch-all panic  [`Type2::Unwrap`] |
| `type2.value` | ➖ | Literal value as a type | a literal used as a top-level type (`answer = 42`) panics; cddl-codegen exposes Fixed only as a struct member, not as a standalone type. A real gap. Its supported array-element role is the [[cover]] above.  [`should not expose Fixed type in member`] — also ✅ @array-element (`fixed_value.cddl`: a literal as a fixed array-element value (`c: 5`)) |

### `value` (3)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `value.bytes` | ➖ | Byte-string literal value | byte-string literal (h'..'/b64'..'/'..') as a value — Type2 unmatched (also a rust-parser limitation: ruby/ABNF accept)  [`Ignoring Type2`] |
| `value.number` | ➖ | Numeric literal value | top-level numeric-literal type (`version = 5`) panics — same Fixed-type gap. Its supported array-element role is the [[cover]] above.  [`should not expose Fixed type in member`] — also ✅ @array-element (`fixed_value.cddl`: numeric literal member (`c: 5`)) |
| `value.text` | ➖ | Text literal value | top-level text-literal type (`marker = "v1"`) panics — same Fixed-type gap. Its supported array-element role is the [[cover]] above.  [`should not expose Fixed type in member`] — also ✅ @array-element (`fixed_value.cddl`: text literal member (`b: "marker"`)) |

## RFC 9682 additions (newer than cddl-codegen's RFC 8610 target — out of profile)

### `type2` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `type2.tag_head_type` | ➖ | Tagged data item, type-valued tag number (#6.<T>) | out of profile — cddl-codegen rejected at parse/lex (exit 1) |

## cddl-codegen vendor profile (comment DSL + sentinels — not RFC 8610)

### `comment_dsl` (8)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `dsl.custom_deserialize` | ✅ | @custom_deserialize — override deserialization | `dsl_custom.cddl` |
| `dsl.custom_json` | ✅ | @custom_json — suppress generated JSON traits | `dsl_custom.cddl` |
| `dsl.custom_serialize` | ✅ | @custom_serialize — override serialization | `dsl_custom.cddl` |
| `dsl.doc` | ✅ | @doc — rust doc comment | `dsl_doc.cddl` |
| `dsl.name` | ✅ | @name — explicit field/variant name | `dsl_name.cddl` |
| `dsl.newtype` | ✅ | @newtype — wrapper struct instead of alias | `dsl_newtype.cddl` |
| `dsl.no_alias` | ✅ | @no_alias — inline the type, emit no alias | `dsl_no_alias.cddl` |
| `dsl.used_as_key` | ✅ | @used_as_key — force Ord/Hash derives | `dsl_used_as_key.cddl` |

### `sentinel` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `ext.extern` | ➕ | _CDDL_CODEGEN_EXTERN_TYPE_ — compose in a hand-written type | supported; requires a user-provided extern type; integration-tested in tests/extern-deps |
| `ext.raw_bytes` | ➕ | _CDDL_CODEGEN_RAW_BYTES_TYPE_ — bytes with hand-written constraints | supported; requires a user-provided raw-bytes impl; integration-tested in tests/raw-bytes |

## Control operators (`ctlop`, §3.8 + IANA registry)

> Support is execution-probed per operator (generate + compile), keyed `ctl.<name>` — same probe as
> features. cddl-codegen implements **9 of the 37** IANA operators (`.size .cbor .default .eq .ne .le
> .lt .ge .gt`); the rest panic or parse-reject. The generic "a control op is applied" feature
> (`type1.ctlop`) appears under `RFC8610 / type1` above.

| operator | | evidence |
|----------|---|----------|
| `.abnf` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.abnfb` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.and` | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.b32` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.b45` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.b64c` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.b64c-sloppy` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.b64u` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.b64u-sloppy` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.base10` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.bits` | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.cat` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.cbor` | ✅ | `cbor_in_bytes.cddl` |
| `.cborseq` | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.default` | ✅ | `default_value.cddl` |
| `.det` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.eq` | ✅ | `comparison_controls.cddl` |
| `.feature` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.ge` | ✅ | `comparison_controls.cddl` |
| `.gt` | ✅ | `comparison_controls.cddl` |
| `.h32` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.hex` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.hexlc` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.hexuc` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.join` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.json` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.le` | ✅ | `sized_int.cddl` |
| `.lt` | ✅ | `comparison_controls.cddl` |
| `.ne` | ✅ | `comparison_controls.cddl` |
| `.oid` _(RFC9090)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.plus` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.printf` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.regexp` | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.sdnv` _(RFC9090)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.sdnvseq` _(RFC9090)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.size` | ✅ | `bounded_bytes.cddl` |
| `.within` | ➖ | probe (control-op): cddl-codegen panic (exit 101) |

## Notable findings

1. Unsupported constructs `panic!` instead of erroring gracefully — valid CDDL using an unsupported construct crashes the generator (the two catch-all arms + the control-op panic). A graceful 'unsupported construct X' error would be friendlier (relates to tests/robustness).
2. Misleading panic message — the control-op catch-all says 'not seen in RFC-8610' even for RFC-8610 operators like `.bits`/`.regexp` (they're in the spec, just unimplemented here).
3. The IMPLICIT cut on `:`/bareword keys is parsed but silently dropped, not enforced — a potential correctness gap (`// TODO: Do we need to handle cuts` in parse_group_type). The EXPLICIT `^` cut is unsupported (see the memberkey.cut note).
4. Sockets aren't really implemented — `$`/`$$` are stripped to plain identifiers, so `$x` silently aliases to `x`; the `/=` / `//=` plug mechanism is ignored (see the assignt/assigng.extend notes).
5. Float is fine until `--preserve-encodings` or bounds (the two `unimplemented!` sites) — the reason the corpus avoids floats.
6. Methodology — the support probe is COMPILE-GATED (generate + `cargo check`), not exit-code-only. This caught a former false positive: `x = any` exits 0 but emits `pub type X = Any;` (a type defined nowhere) which fails to compile, so `prelude.any` is correctly ➖ (root cause: `any` is absent from `is_identifier_reserved` in src/utils.rs, so it's treated as an undefined user type). The same standalone-compile failure is expected-by-design for the extern/raw-bytes sentinels and @custom_serialize/@custom_deserialize — those are exempt (supported, but compile only with user-provided code; integration-tested).
7. Bug — a type choice containing `bool` generates non-compiling Rust (`error[E0282]: type annotations needed`): `bool / tstr` and `uint / bool` fail, while `int / tstr` and `uint / text / bytes` compile. Surfaced by the compile-gate (the `type.choice` example was changed off `bool` to isolate the construct). Candidate cddl-codegen fix.
8. Bug — a single-letter rule named `r` capitalizes to a struct `R` that collides with the generated deserializer's reader generic parameter `R` (`error[E0574]: ... found type parameter R`), so the crate fails to compile. Surfaced incidentally by the compile-gate; avoid `r` as a rule name. Candidate cddl-codegen fix.
9. Bug — the generator's `Int` wrapper isn't emitted for a bare alias, so a top-level `x = int` emits `pub type X = Int;` and an `int` payload (`bytes .cbor int`) emits an undefined `Int` too (`cannot find type Int`) — both fail the compile-gate. This is the SAME false-positive class as `any` (the compile-gate caught it). `int` works as a struct member / array element (its normal use, e.g. `[x: int]` and `int / tstr` compile), so `prelude.int`'s probe example is member-form and `ctl.cbor`'s payload is `uint` to isolate each construct. Candidate cddl-codegen fix.
10. Gap — top-level fixed-value / null TYPES panic (`answer = 42`, `x = null` -> `should not expose Fixed type in member`), even though fixed values serialize fine as struct members. A singleton-value type is a reasonable feature; candidate cddl-codegen fix. (Surfaced by the matrix, not hidden by editing the example.)
11. Bug — single-field STRUCT maps panic: `{ a: uint }` hits the table-detection path (`unsupported table map key`), so the minimal bareword-key / optional examples use single-field ARRAYS instead. Single-field structs should work.
12. Bug — the exclusive range `a...b` mis-computes its upper bound: it should EXCLUDE `b` (max valid = b-1), but cddl-codegen emits `max = b+1`, so `[v: 0...10]` accepts 10 and 11 (exclusive_range.cddl snapshot: `max: Some(11)`). `rangeop.exclusive` is marked ⚠️ for this. Candidate cddl-codegen fix: `range_end + 1` -> `range_end - 1` in parsing.rs. (Surfaced by the matrix, not hidden by editing the example.)
13. Gap — occurrence-count constraints on homogeneous arrays aren't enforced: `[+ uint]` (>=1) and `[2*5 uint]` (2..5) both emit a plain `Vec<u64>` with NO length check, so any count (incl. empty) is accepted. Bare `*` (zero-or-more) is faithfully a `Vec` — which is why it stays ✅ — but `+`/`n*m` silently drop a real constraint (analogous to the implicit-cut non-enforcement above). Candidate cddl-codegen fix.
14. Bug — an inline parenthesized group as an array entry drops all but its FIRST member: `[(uint, tstr)]` generates a 1-field `InlineGroup { index_0: u64 }` (`read_elems(1)`), silently losing the `tstr` (inline_group.cddl snapshot). It compiles (matrix probe = supported), but loses data — `grpent.inline_group` is marked ⚠️. Candidate cddl-codegen fix: inline-group entries aren't flattened into the record.

## Summary

- Features: **92** — ✅ 52 covered · ➕ 7 supported-untested · ⚠️ 7 partial · ➖ 26 not supported
- Control operators: **37** — ✅ 9 covered · ➕ 0 supported-untested · ➖ 28 not supported (cddl-codegen implements 9 of 37)
- Corpus fixtures: 33

**Per-cell coverage (role × feature) — ROADMAP item 6.** Where a construct's support *differs by role*,
coverage is keyed on the (role × feature) cell, derived from a real `cddl`-crate AST walk
(`cddl-matrix/examples/ast_roles.rs`) and cross-checked against the matrix's per-cell support verdict — so a
➖ standalone type still surfaces its supported member/choice role (e.g. `prelude.null` ➖ as a top-level
type, ✅ as a choice-member). **4 such cells** are mapped (appended as "also ✅ @role" on the
rows above); constructs whose support doesn't vary by role stay feature-axis (the role is unremarkably
top-level). A full role × feature coverage grid for *every* construct is future work — the floor data
(`rolesIn`) already supports it.
