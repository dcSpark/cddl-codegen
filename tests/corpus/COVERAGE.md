# Corpus coverage map — CDDL constructs (GENERATED)

> **GENERATED** by `cddl-matrix/project_corpus.ts` — do not hand-edit. Status (✅/➕/➖/⚠️) is the
> execution-grounded matrix support verdict joined with the corpus overlay (canonical fixture, nuance
> notes, findings) in `cddl-matrix/annotations/corpus/cddl_codegen.toml`. Regenerate after changing
> either; CI fails on overlay drift: a cover whose fixture stops exercising its construct, a note that
> contradicts the matrix support verdict, or a stale id/fixture/anchor. A cover on a construct the
> matrix marks not-supported is a SUPPORT SEAM — annotated on the row, reported by the validator, not
> (yet) fatal.

Tracks which CDDL constructs the snapshot **corpus** (`tests/corpus/*.cddl`) exercises, what's
supported-but-untested (a corpus gap to fill), and what the generator does **not** support (the
boundary). The feature universe + support are anchored to the spec (RFC 8610 grammar/prelude + the
IANA control-op registry) and cddl-codegen's vendor profile — not to a self-feature-list, which is what
makes the ➖ boundary rows visible. Sections are derived: **profile → production → id**.

## How this map works

- **Test:** `tests/corpus/<construct>.cddl`, driven by `snapshot_tests::feature_corpus` — each file is
  generated under every flag profile (`default`/`preserve`/`json`) plus an IR dump, and the generated
  *source* is snapshotted. Bless with `INSTA_UPDATE=always cargo test snapshot_tests`.
- **Compile + execution gate:** `integration_tests::feature_corpus_compiles` `cargo check`s every corpus
  file under all three profiles, so a ✅ entry must produce **compiling** Rust under *all* of them —
  and under the default profile it also generates with `--emit-tests` and `cargo test`s the crate, so
  every constructible corpus type must **round-trip byte-identically** (plus bounded-reject checks),
  not just compile. Exception: fixtures on the harness's `COMPILE_SKIP` list (user-supplied-code
  constructs, e.g. `dsl_custom.cddl`), which are snapshot-only here and compile-exercised via their
  integration fixtures instead.
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
| `assigng.extend` | ➖ | Incremental group-choice extension (//=) | incremental group-choice extension (//=) that extends an already-defined ident is rejected gracefully at generation — same silent last-wins drop as the type socket; a plain group rule cannot itself carry a group choice, so give each arm its own named group and select at the use site (`t = [ grpA // grpB ]`)  [`incremental_choice_extension_rejection`] |

### `assignt` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `assignt.extend` | ➖ | Incremental type-choice extension (/=) | incremental type-choice extension (/=) that extends an already-defined ident is rejected gracefully at generation — silently dropping every arm but the last (only the final extension arm survives) generated a wrong type, so api.rs rejects the second definition; fold the arms into one rule (`a = int / tstr`) instead  [`incremental_choice_extension_rejection`] |

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
| `grpent.inline_group` | ✅ | Inline (parenthesized) group entry | `inline_group.cddl` |
| `grpent.member` | ➕ | Member entry (optional occur + optional memberkey + type) | supported, no corpus fixture (cddl-codegen exit 0) |

### `memberkey` (4)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `memberkey.bareword` | ✅ | Bareword memberkey (k:) | `map_struct.cddl` |
| `memberkey.cut` | ⚠️ | Cut in a => memberkey (^) | a literal-key `k ^ => v` routes to the record path as a 1-field struct (the example generates and round-trips), but the cut SEMANTICS (a matched key not being offered to later wildcard entries) is parsed and dropped, exactly like the implicit `:`/bareword cut — a no-op for a closed single-key map, so wire-correct here, but not enforced in general (see finding).  [`Do we need to handle cuts`] |
| `memberkey.type1` | ✅ | Type memberkey (t =>) | `table.cddl` |
| `memberkey.value` | ✅ | Value memberkey (1:) | `value_key.cddl` |

### `ne` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `ctl.ne.one` | ➕ | Not-equal control at the one boundary (.ne 1) | supported, no corpus fixture (cddl-codegen exit 0) |
| `ctl.ne.zero` | ➕ | Not-equal control at the zero boundary (.ne 0) | supported, no corpus fixture (cddl-codegen exit 0) |

### `occur` (6)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `occur.bounded` | ✅ | Bounded occurrence (n*m) | `occurrence.cddl` |
| `occur.bounded.lower` | ➕ | Lower-bound-only occurrence (n*) | supported, no corpus fixture (cddl-codegen exit 0) |
| `occur.bounded.upper` | ➕ | Upper-bound-only occurrence (*m) | supported, no corpus fixture (cddl-codegen exit 0) |
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
| `prelude.false` | ➖ | false | the fixed boolean `false` used as a standalone top-level type is rejected gracefully — same graceful path as `true`/`null`; works as a struct/array member (`tests/corpus/fixed_bool_member.cddl`). Pinned by `tests/matrix_reject/prelude.false.cddl`.  [`a top-level rule whose entire body is a bare fixed value`] |
| `prelude.float` | ⚠️ | float | de/ser works under default/json, but --preserve-encodings and bounds are unimplemented for floats (so float-bearing types can't be corpus entries — the corpus runs preserve; float JSON emission is covered by tests/json-float/ + its whole_program snapshot instead)  [`preserve_encodings is not implemented for float`] |
| `prelude.float16` | ➖ | float16 | no native Rust f16 — the float alias system doesn't handle float16, so it panics even as a struct member (float32/float64 work).  [`should be handled by the alias system instead`] |
| `prelude.float16-32` | ➖ | float16-32 | the float16/float32 choice alias isn't handled by the float alias system (it includes the unsupported float16); panics even as a member.  [`should be handled by the alias system instead`] |
| `prelude.float32` | ⚠️ | float32 | works under default/json as a member, but --preserve-encodings is unimplemented for floats — same limitation as `float` (verified: `holder = [x: float32]` compiles default, fails preserve)  [`preserve_encodings is not implemented for float`] |
| `prelude.float32-64` | ➖ | float32-64 | the float32/float64 choice alias isn't handled by the float alias system (the float-choice aliases are unsupported, though float32/float64 work on their own); panics even as a member.  [`should be handled by the alias system instead`] |
| `prelude.float64` | ⚠️ | float64 | works under default/json as a member, but --preserve-encodings is unimplemented for floats — same limitation as `float` (verified: `holder = [x: float64]` compiles default, fails preserve)  [`preserve_encodings is not implemented for float`] |
| `prelude.int` | ✅ | int | `primitives.cddl` |
| `prelude.integer` | ✅ | integer | `prelude.cddl` |
| `prelude.mime-message` | ✅ | mime-message | `prelude.cddl` |
| `prelude.nil` | ➖ | nil | top-level `x = nil` (fixed null value) is rejected gracefully — same graceful path as `null`; works as a struct member (`[x: nil]`) but not as a standalone type. Pinned by `tests/matrix_reject/prelude.nil.cddl`.  [`a top-level rule whose entire body is a bare fixed value`] |
| `prelude.nint` | ✅ | nint | `primitives.cddl` |
| `prelude.null` | ➖ | null | top-level `x = null` type is rejected gracefully — cddl-codegen exposes Fixed only as a struct member, not as a standalone type (same Fixed-type gap as the literal values); pinned by `tests/matrix_reject/prelude.null.cddl`. Its supported choice-member role is the [[cover]] above.  [`a top-level rule whose entire body is a bare fixed value`] — also ✅ @choice-member (`nullable.cddl`: the `T / null` -> Option<T> nullable pattern) |
| `prelude.number` | ➕ | number | supported, no corpus fixture (cddl-codegen exit 0); --preserve-encodings unsupported (cddl-codegen panic (exit 101)) |
| `prelude.regexp` | ✅ | regexp | `prelude.cddl` |
| `prelude.tdate` | ✅ | tdate | `prelude.cddl` |
| `prelude.text` | ✅ | text | `primitives.cddl` |
| `prelude.time` | ➕ | time | supported, no corpus fixture (cddl-codegen exit 0); --preserve-encodings unsupported (cddl-codegen panic (exit 101)) |
| `prelude.true` | ➖ | true | the fixed boolean `true` used as a standalone top-level type is rejected gracefully (it used to panic); a fixed value has no standalone type representation, only meaning as a struct/array member — which DOES work (`tests/corpus/fixed_bool_member.cddl`). Pinned by `tests/matrix_reject/prelude.true.cddl`. Same Fixed-type gap as `null`.  [`a top-level rule whose entire body is a bare fixed value`] |
| `prelude.tstr` | ✅ | tstr | `prelude.cddl` |
| `prelude.uint` | ✅ | uint | `primitives.cddl` |
| `prelude.undefined` | ➖ | undefined | the `undefined` simple value is rejected  [`unsupported cddl prelude type`] |
| `prelude.unsigned` | ✅ | unsigned | `prelude.cddl` |
| `prelude.uri` | ✅ | uri | `prelude.cddl` |

### `rangeop` (8)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `rangeop.exclusive` | ✅ | Exclusive range (a...b) | `exclusive_range.cddl` |
| `rangeop.exclusive.float` | ➕ | Exclusive range, float head (a...b) | supported, no corpus fixture (cddl-codegen exit 0); --preserve-encodings unsupported (cddl-codegen panic (exit 101)) |
| `rangeop.exclusive.int` | ➕ | Exclusive range, signed-int head spanning both CBOR sign arms (a...b) | supported, no corpus fixture (cddl-codegen exit 0) |
| `rangeop.exclusive.nint` | ➕ | Exclusive range, all-negative head (uint arm empty) (a...b) | supported, no corpus fixture (cddl-codegen exit 0) |
| `rangeop.inclusive` | ✅ | Inclusive range (a..b) | `sized_int.cddl` |
| `rangeop.inclusive.float` | ➕ | Inclusive range, float head (a..b) | supported, no corpus fixture (cddl-codegen exit 0); --preserve-encodings unsupported (cddl-codegen panic (exit 101)) |
| `rangeop.inclusive.int` | ➕ | Inclusive range, signed-int head spanning both CBOR sign arms (a..b) | supported, no corpus fixture (cddl-codegen exit 0) |
| `rangeop.inclusive.nint` | ➕ | Inclusive range, all-negative head (uint arm empty) (a..b) | supported, no corpus fixture (cddl-codegen exit 0) |

### `rule` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `genericparm.group` | ➖ | Generic group definition | a generic GROUP definition (`set<a> = (* a)`) is rejected — generics are supported on type rules, not on plain groups.  [`Generics not supported on plain groups`] |
| `genericparm.type` | ✅ | Generic type definition | `generics.cddl` |

### `size` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `ctl.size.uint` | ➕ | Size control over a uint target (.size over uint) | supported, no corpus fixture (cddl-codegen exit 0) |

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
| `type2.parenthesized` | ✅ | Parenthesized type | `parenthesized.cddl` — canonical = the fixture isolating `(T)` at type position; nested_group.cddl (the previous cover) contains a parenthesized GROUP RULE, not the type2 production — the old detector conflated the two |
| `type2.tag` | ✅ | Tagged data item (#6.n) | `tagged.cddl` |
| `type2.typename` | ✅ | Type reference (with optional generic args) | `type_alias.cddl` |
| `type2.unwrap` | ➖ | Unwrap (~) | unwrap `~` — Type2::Unwrap unmatched, catch-all panic  [`Type2::Unwrap`] |
| `type2.value` | ➖ | Literal value as a type | a literal used as a top-level type (`answer = 42`) is rejected gracefully; cddl-codegen exposes Fixed only as a struct member, not as a standalone type. A real gap; pinned by `tests/matrix_reject/type2.value.cddl`. Its supported array-element role is the [[cover]] above.  [`a top-level rule whose entire body is a bare fixed value`] — also ✅ @array-element (`fixed_value.cddl`: a literal as a fixed array-element value (`c: 5`)) |

### `value` (6)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `value.bytes` | ➖ | Byte-string literal value | byte-string literal (h'..'/b64'..'/'..') as a value — Type2 unmatched (also a rust-parser limitation: ruby/ABNF accept)  [`Ignoring Type2`] |
| `value.number` | ➖ | Numeric literal value | top-level numeric-literal type (`version = 5`) is rejected gracefully — same Fixed-type gap; pinned by `tests/matrix_reject/value.number.cddl`. Its supported array-element role is the [[cover]] above.  [`a top-level rule whose entire body is a bare fixed value`] — also ✅ @array-element (`fixed_value.cddl`: numeric literal member (`c: 5`)) |
| `value.number.bin` | ➕ | Binary integer literal (0b…) | supported, no corpus fixture (cddl-codegen exit 0) |
| `value.number.hex` | ➕ | Hexadecimal integer literal (0x…) | supported, no corpus fixture (cddl-codegen exit 0) |
| `value.number.hexfloat` | ➕ | Hexadecimal float literal (hexfloat) | supported, no corpus fixture (cddl-codegen exit 0); --preserve-encodings unsupported (cddl-codegen panic (exit 101)) |
| `value.text` | ➖ | Text literal value | top-level text-literal type (`marker = "v1"`) is rejected gracefully — same Fixed-type gap; pinned by `tests/matrix_reject/value.text.cddl`. Its supported array-element role is the [[cover]] above.  [`a top-level rule whose entire body is a bare fixed value`] — also ✅ @array-element (`fixed_value.cddl`: text literal member (`b: "marker"`)) |

## RFC 9682 additions (newer than cddl-codegen's RFC 8610 target — out of profile)

### `type2` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `type2.tag_head_type` | ➖ | Tagged data item, type-valued tag number (#6.<T>) | out of profile — cddl-codegen panic (exit 101) |

## cddl-codegen vendor profile (comment DSL + sentinels — not RFC 8610)

### `comment_dsl` (17)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `dsl.copy` | ✅ | @copy — extern/raw-bytes type derives Copy, drop boundary clones | `dsl_copy.cddl` |
| `dsl.custom_deserialize` | ✅ | @custom_deserialize — override deserialization | `dsl_custom.cddl` |
| `dsl.custom_json` | ✅ | @custom_json — suppress generated JSON traits | `dsl_custom.cddl` |
| `dsl.custom_serialize` | ✅ | @custom_serialize — override serialization | `dsl_custom.cddl` |
| `dsl.doc` | ✅ | @doc — rust doc comment | `dsl_doc.cddl` |
| `dsl.duplicates.preserve` | ✅ | @duplicates preserve — duplicate-preserving pair-map tables | `table_preserve.cddl` |
| `dsl.duplicates.reject` | ✅ | @duplicates reject — duplicate-free set/array collections | `tag_set_reject.cddl` |
| `dsl.name` | ✅ | @name — explicit field/variant name | `dsl_name.cddl` |
| `dsl.newtype` | ✅ | @newtype — wrapper struct instead of alias | `dsl_newtype.cddl` |
| `dsl.no_alias` | ✅ | @no_alias — inline the type, emit no alias | `dsl_no_alias.cddl` |
| `dsl.raw_bytes_flavor` | ✅ | @raw_bytes_flavor — extern generic raw-bytes wrapper flavor | `extern_generic_raw_bytes.cddl` |
| `dsl.rust_name` | ➕ | @rust_name — dependency-pinned Rust type name | supported; pins a dependency-crate type name, so the generated `use extern_dep::…` cannot compile standalone; integration-tested in src/tests/rust_name_tests.rs and the extern_import byte-identity pair |
| `dsl.used_as_elem` | ✅ | @used_as_elem — mint the canonical loose-list wasm wrapper | `dsl_used_as_elem.cddl` |
| `dsl.used_as_key` | ✅ | @used_as_key — force Ord/Hash derives | `dsl_used_as_key.cddl` |
| `dsl.used_as_key.hash` | ✅ | @used_as_key hash — narrowed Hash derive family | `dsl_used_as_key_hash.cddl` |
| `dsl.used_as_key.hash_ord` | ✅ | @used_as_key hash ord — union of the hash and ord families | `dsl_used_as_key_hash_ord.cddl` |
| `dsl.used_as_key.ord` | ✅ | @used_as_key ord — narrowed Ord derive family | `dsl_used_as_key_ord.cddl` |

### `sentinel` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `ext.extern` | ✅ | _CDDL_CODEGEN_EXTERN_TYPE_ — compose in a hand-written type | `extern_generic_raw_bytes.cddl` |
| `ext.raw_bytes` | ✅ | _CDDL_CODEGEN_RAW_BYTES_TYPE_ — bytes with hand-written constraints | `extern_generic_raw_bytes.cddl` |

## Control operators (`ctlop`, §3.8 + IANA registry)

> Support is execution-probed per operator (generate + compile), keyed `ctl.<name>` — same probe as
> features. cddl-codegen implements **9 of the 37** IANA operators (`.size .cbor .default .eq .ne .le
> .lt .ge .gt`); the rest panic or parse-reject. The generic "a control op is applied" feature
> (`type1.ctlop`) appears under `RFC8610 / type1` above.

| operator | | evidence |
|----------|---|----------|
| `.abnf` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.abnfb` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.and` | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.b32` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.b45` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.b64c` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.b64c-sloppy` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.b64u` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.b64u-sloppy` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.base10` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.bits` | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.cat` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.cbor` | ✅ | `cbor_in_bytes.cddl` |
| `.cborseq` | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.default` | ✅ | `default_value.cddl` |
| `.det` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.eq` | ✅ | `comparison_controls.cddl` |
| `.feature` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.ge` | ✅ | `comparison_controls.cddl` |
| `.gt` | ✅ | `comparison_controls.cddl` |
| `.h32` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.hex` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.hexlc` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.hexuc` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.join` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.json` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.le` | ✅ | `sized_int.cddl` |
| `.lt` | ✅ | `comparison_controls.cddl` |
| `.ne` | ✅ | `comparison_controls.cddl` |
| `.oid` _(RFC9090)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.plus` _(RFC9165)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.printf` _(RFC9741)_ | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.regexp` | ➖ | probe (control-op): cddl-codegen panic (exit 101) |
| `.sdnv` _(RFC9090)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.sdnvseq` _(RFC9090)_ | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |
| `.size` | ✅ | `bounded_bytes.cddl` |
| `.within` | ➖ | probe (control-op): cddl-codegen rejected at parse/lex (exit 1) |

## Notable findings

1. Unsupported constructs `panic!` instead of erroring gracefully — valid CDDL using an unsupported construct crashes the generator (the two catch-all arms + the control-op panic). A graceful 'unsupported construct X' error would be friendlier (relates to tests/robustness).
2. Misleading panic message — the control-op catch-all says 'not seen in RFC-8610' even for RFC-8610 operators like `.bits`/`.regexp` (they're in the spec, just unimplemented here).
3. Both the IMPLICIT cut on `:`/bareword keys and the EXPLICIT `^` cut are parsed but their semantics are silently dropped, not enforced — a potential correctness gap (`// TODO: Do we need to handle cuts` in parse_group_type). The explicit-cut example still generates (a literal-key `k ^ => v` routes to the record path; see the memberkey.cut note), so this is a semantics gap, not a generation gap.
4. Sockets aren't really implemented — `$`/`$$` are stripped to plain identifiers, so `$x` silently aliases to `x`. Incremental choice extension via the `/=` / `//=` plug (extending an already-defined ident) is rejected gracefully at generation rather than silently narrowing to the last arm (see the assignt/assigng.extend notes).
5. Float is fine until `--preserve-encodings` or bounds (the two `unimplemented!` sites). The corpus no longer avoids floats entirely: `tests/corpus/optional_fixed_float.cddl` carries an optional fixed FLOAT member (default/json generate the `bool` presence field; its preserve leg is generation-fail-ledgered in `feature_corpus_compiles`'s `EXPECTED_GENERATION_FAIL`). The residual preserve-only float gap rides the `preserve_encodings_supports_floats` stub class.
6. Methodology — the support probe is COMPILE-GATED (generate + `cargo check`), not exit-code-only. This caught a former false positive: `x = any` exits 0 but emits `pub type X = Any;` (a type defined nowhere) which fails to compile, so `prelude.any` is correctly ➖ (root cause: `any` is absent from `is_identifier_reserved` in src/utils.rs, so it's treated as an undefined user type). The same standalone-compile failure is expected-by-design for the extern/raw-bytes sentinels and @custom_serialize/@custom_deserialize — those are exempt (supported, but compile only with user-provided code; integration-tested).
7. Gap — top-level fixed-value / null TYPES (`answer = 42`, `x = null`) have no standalone type representation, so the generator REJECTS them gracefully (not a panic), pinned by the `tests/matrix_reject/` expect-reject catalog (`tests/matrix_reject/prelude.null.cddl`, `tests/matrix_reject/type2.value.cddl`, `tests/matrix_reject/value.number.cddl`, `tests/matrix_reject/value.text.cddl`) via `robustness_tests::unsupported_construct_reject_catalog`. The same fixed values serialize fine as struct/array MEMBERS (`tests/corpus/fixed_bool_member.cddl`). A singleton-value type that materializes the constant is still a reasonable feature; candidate cddl-codegen fix. (Surfaced by the matrix, not hidden by editing the example.)
8. Single-field STRUCT maps are supported: `{ a: uint }` is a 1-field struct (a bareword key is sugar for the equivalent text-string value key), identical in wire shape to the multi-field `{ a: uint, b: text }` form. MIXED struct+table maps (`{ a: uint, * k => v }`) remain unsupported — a map is detected as EITHER a struct or a homogenous table, never both (now rejected gracefully). Candidate cddl-codegen feature.

## Summary

- Features: **115** — ✅ 64 covered · ➕ 20 supported-untested · ⚠️ 4 partial · ➖ 27 not supported
- Control operators: **37** — ✅ 9 covered · ➕ 0 supported-untested · ➖ 28 not supported (cddl-codegen implements 9 of 37)
- Corpus fixtures: 83

**Per-cell coverage (role × feature).** Where a construct's support *differs by role*,
coverage is keyed on the (role × feature) cell, derived from a real `cddl`-crate AST walk
(`cddl-matrix/examples/ast_roles.rs`) and cross-checked against the matrix's per-cell support verdict — so a
➖ standalone type still surfaces its supported member/choice role (e.g. `prelude.null` ➖ as a top-level
type, ✅ as a choice-member). **4 such cells** are mapped (appended as "also ✅ @role" on the
rows above); constructs whose support doesn't vary by role stay feature-axis (the role is unremarkably
top-level). A full role × feature coverage grid for *every* construct is future work — the floor data
(`rolesIn`) already supports it.
