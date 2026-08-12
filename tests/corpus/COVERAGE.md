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
| `assigng.extend` | ➖ | Incremental group-choice extension (//=) | incremental group-choice extension (//=) is refused gracefully at generation, in EITHER statement order (the classification keys on the name's whole statement set, not on the repeat's own operator). Honoring it would merge the arms into a plain group rule carrying 2+ group choices — exactly the shape `mark_plain_group` asserts against — so it is blocked on the choice-of-bodies design rather than merely unbuilt; give each arm its own named group and select at the use site (`t = [ grpA // grpB ]`). Its type-side sibling `/=` IS honored: those statements merge into one type-choice rule  [`repeated_rule_definition_rejections`] |

### `assignt` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `assignt.extend` | ✅ | Incremental type-choice extension (/=) | `assignt_extend.cddl` |

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
| `prelude.any` | ➕ | any | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.b64legacy` | ✅ | b64legacy | `prelude.cddl` |
| `prelude.b64url` | ✅ | b64url | `prelude.cddl` |
| `prelude.bigfloat` | ✅ | bigfloat | `prelude.cddl` |
| `prelude.bigint` | ✅ | bigint | `prelude.cddl` |
| `prelude.bignint` | ✅ | bignint | `prelude.cddl` |
| `prelude.biguint` | ✅ | biguint | `prelude.cddl` |
| `prelude.bool` | ✅ | bool | `bool.cddl` |
| `prelude.bstr` | ✅ | bstr | `prelude.cddl` |
| `prelude.bytes` | ✅ | bytes | `primitives.cddl` |
| `prelude.cbor-any` | ➖ | cbor-any | the self-describe tag `#6.55799(any)` marks a byte stream as CBOR — a property of the stream, not of a value a generated struct could hold — so it is rejected gracefully in every position; support is permanently excluded by ruling (`tests/TESTING_ROADMAP.md` § North star). Pinned by `cbor_any_prelude_tag_rejects_gracefully_in_every_position`.  [`self-described STREAM marker`] |
| `prelude.decfrac` | ✅ | decfrac | `prelude.cddl` |
| `prelude.eb16` | ✅ | eb16 | `expected_conversion_tags.cddl` |
| `prelude.eb64legacy` | ✅ | eb64legacy | `expected_conversion_tags.cddl` |
| `prelude.eb64url` | ✅ | eb64url | `expected_conversion_tags.cddl` |
| `prelude.encoded-cbor` | ✅ | encoded-cbor | `prelude.cddl` |
| `prelude.false` | ✅ | false | `fixed_singletons.cddl` |
| `prelude.float` | ➕ | float | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.float16` | ➕ | float16 | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.float16-32` | ➕ | float16-32 | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.float32` | ➕ | float32 | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.float32-64` | ➕ | float32-64 | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.float64` | ✅ | float64 | `homogeneous_array.cddl` |
| `prelude.int` | ✅ | int | `primitives.cddl` |
| `prelude.integer` | ✅ | integer | `prelude.cddl` |
| `prelude.mime-message` | ✅ | mime-message | `prelude.cddl` |
| `prelude.nil` | ✅ | nil | `fixed_singletons.cddl` |
| `prelude.nint` | ✅ | nint | `primitives.cddl` |
| `prelude.null` | ✅ | null | `fixed_singletons.cddl` — also ✅ @choice-member (`nullable.cddl`: the `T / null` -> Option<T> nullable pattern) |
| `prelude.number` | ➕ | number | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.regexp` | ✅ | regexp | `prelude.cddl` |
| `prelude.tdate` | ✅ | tdate | `prelude.cddl` |
| `prelude.text` | ✅ | text | `primitives.cddl` |
| `prelude.time` | ➕ | time | supported, no corpus fixture (cddl-codegen exit 0) |
| `prelude.true` | ✅ | true | `fixed_singletons.cddl` |
| `prelude.tstr` | ✅ | tstr | `prelude.cddl` |
| `prelude.uint` | ✅ | uint | `primitives.cddl` |
| `prelude.undefined` | ✅ | undefined | `fixed_singletons.cddl` |
| `prelude.unsigned` | ✅ | unsigned | `prelude.cddl` |
| `prelude.uri` | ✅ | uri | `prelude.cddl` |

### `rangeop` (8)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `rangeop.exclusive` | ✅ | Exclusive range (a...b) | `exclusive_range.cddl` |
| `rangeop.exclusive.float` | ➕ | Exclusive range, float head (a...b) | supported, no corpus fixture (cddl-codegen exit 0) |
| `rangeop.exclusive.int` | ➕ | Exclusive range, signed-int head spanning both CBOR sign arms (a...b) | supported, no corpus fixture (cddl-codegen exit 0) |
| `rangeop.exclusive.nint` | ➕ | Exclusive range, all-negative head (uint arm empty) (a...b) | supported, no corpus fixture (cddl-codegen exit 0) |
| `rangeop.inclusive` | ✅ | Inclusive range (a..b) | `sized_int.cddl` |
| `rangeop.inclusive.float` | ➕ | Inclusive range, float head (a..b) | supported, no corpus fixture (cddl-codegen exit 0) |
| `rangeop.inclusive.int` | ➕ | Inclusive range, signed-int head spanning both CBOR sign arms (a..b) | supported, no corpus fixture (cddl-codegen exit 0) |
| `rangeop.inclusive.nint` | ➕ | Inclusive range, all-negative head (uint arm empty) (a..b) | supported, no corpus fixture (cddl-codegen exit 0) |

### `rule` (2)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `genericparm.group` | ➖ | Generic group definition | a generic GROUP definition (`set<a> = (* a)`, and the bare-paren group-choice spelling `g<T> = ((a: T) // (b: uint))`) is refused gracefully at parse time — a plain group registers no struct of its own for an instance's arguments to substitute into. Generics are supported on type rules (array, map/record, the tag-set idiom). Pinned by `generic_plain_group_def_rejects_gracefully`.  [`Generics not supported on plain groups`] |
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
| `type2.any` | ➖ | Any (#) | bare `#` (any) — no storable representation, rejected gracefully in both rule-body and member position (the prelude NAME `any` is supported and is what the message points at)  [`the `any` type (`#`)`] |
| `type2.array` | ✅ | Array | `array.cddl` |
| `type2.choice_from_group` | ➖ | Choice from named group (&) | choice-from-group `&groupname` — unmatched  [`Type2::ChoiceFromGroup`] |
| `type2.choice_from_inline_group` | ➖ | Choice from inline group (&) | choice-from-inline-group `&(...)` — unmatched  [`Type2::ChoiceFromInlineGroup`] |
| `type2.major` | ➖ | Major-type sigil (#N, #N.n) | major-type sigils `#N` / `#N.n` — no storable representation, rejected gracefully in both rule-body and member position  [`a bare major-type constraint`] |
| `type2.major7` | ➖ | Major-type 7 / simple sigil (#7, #7.n) | `#7` / `#7.n` simple/float sigils — same bare major-type constraint, rejected gracefully in both rule-body and member position  [`a bare major-type constraint`] |
| `type2.map` | ✅ | Map | `map_struct.cddl` — canonical = pure struct map; table-style is table.cddl; MIXED struct+table ({a: uint, * k => v}) is unsupported (parsing.rs) |
| `type2.parenthesized` | ✅ | Parenthesized type | `parenthesized.cddl` — canonical = the fixture isolating `(T)` at type position; nested_group.cddl (the previous cover) contains a parenthesized GROUP RULE, not the type2 production — the old detector conflated the two |
| `type2.tag` | ✅ | Tagged data item (#6.n) | `tagged.cddl` |
| `type2.typename` | ✅ | Type reference (with optional generic args) | `type_alias.cddl` |
| `type2.unwrap` | ➖ | Unwrap (~) | unwrap `~` — the construct splices a group's contents, so there is no type to store; rejected gracefully in both rule-body and member position, pointing at the one remedy that works (inline the referenced rule by hand)  [`Type2::Unwrap`] |
| `type2.value` | ✅ | Literal value as a type | `fixed_singletons.cddl` — also ✅ @array-element (`fixed_value.cddl`: a literal as a fixed array-element value (`c: 5`)) |

### `value` (6)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `value.bytes` | ✅ | Byte-string literal value | `fixed_singletons.cddl` |
| `value.number` | ✅ | Numeric literal value | `fixed_singletons.cddl` — also ✅ @array-element (`fixed_value.cddl`: numeric literal member (`c: 5`)) |
| `value.number.bin` | ➕ | Binary integer literal (0b…) | supported, no corpus fixture (cddl-codegen exit 0) |
| `value.number.hex` | ➕ | Hexadecimal integer literal (0x…) | supported, no corpus fixture (cddl-codegen exit 0) |
| `value.number.hexfloat` | ➕ | Hexadecimal float literal (hexfloat) | supported, no corpus fixture (cddl-codegen exit 0) |
| `value.text` | ✅ | Text literal value | `fixed_singletons.cddl` — also ✅ @array-element (`fixed_value.cddl`: text literal member (`b: "marker"`)) |

## RFC 9682 additions (newer than cddl-codegen's RFC 8610 target — out of profile)

### `type2` (1)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `type2.tag_head_type` | ➖ | Tagged data item, type-valued tag number (#6.<T>) | out of profile — cddl-codegen panic (exit 101) |

## cddl-codegen vendor profile (comment DSL + sentinels — not RFC 8610)

### `comment_dsl` (22)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `dsl.copy` | ✅ | @copy — extern/raw-bytes type derives Copy, drop boundary clones | `dsl_copy.cddl` |
| `dsl.custom_deserialize` | ✅ | @custom_deserialize — override deserialization | `dsl_custom.cddl` |
| `dsl.custom_encodings` | ✅ | @custom_encodings — the custom codec declares its own wire's encoding variables | `dsl_custom.cddl` |
| `dsl.custom_json` | ✅ | @custom_json — suppress generated JSON traits | `dsl_custom.cddl` |
| `dsl.custom_serialize` | ✅ | @custom_serialize — override serialization | `dsl_custom.cddl` |
| `dsl.custom_wire_major` | ➕ | @custom_wire_major — the custom codec declares which CBOR major type its wire starts with | supported, no corpus fixture (cddl-codegen exit 0) |
| `dsl.doc` | ✅ | @doc — rust doc comment | `dsl_doc.cddl` |
| `dsl.duplicates.preserve` | ✅ | @duplicates preserve — duplicate-preserving pair-map tables | `table_preserve.cddl` |
| `dsl.duplicates.reject` | ✅ | @duplicates reject — duplicate-free set/array collections | `tag_set_reject.cddl` |
| `dsl.extern_companions` | ➕ | @extern_companions — reference a sibling crate's wasm companion classes | supported; the directive defers the wasm companion classes to a SIBLING WASM CRATE, so the generated `use <path>::<Class>;` needs that crate to exist and a local definition would defeat the deferral it declares; integration-tested in src/tests/extern_companions_tests.rs and the two-crate wasm32 link gate extern_companions_defers_to_sibling_wasm_crate |
| `dsl.ignore` | ✅ | @ignore — tolerate-and-drop open struct-map rest row | `dsl_ignore.cddl` |
| `dsl.name` | ✅ | @name — explicit field/variant name | `dsl_name.cddl` |
| `dsl.newtype` | ✅ | @newtype — wrapper struct instead of alias | `dsl_newtype.cddl` |
| `dsl.no_alias` | ✅ | @no_alias — inline the type, emit no alias | `dsl_no_alias.cddl` |
| `dsl.no_json_schema_export` | ➕ | @no_json_schema_export — not a published JSON-schema root | supported, no corpus fixture (cddl-codegen exit 0) |
| `dsl.raw_bytes_flavor` | ✅ | @raw_bytes_flavor — extern generic raw-bytes wrapper flavor | `extern_generic_raw_bytes.cddl` |
| `dsl.rust_name` | ➕ | @rust_name — dependency-pinned Rust type name | supported; the directive pins a DEPENDENCY crate's type name, so the generated `use extern_dep::…` needs that whole crate on the path — a local definition cannot supply a foreign crate root; integration-tested in src/tests/rust_name_tests.rs and the extern_import byte-identity pair |
| `dsl.used_as_elem` | ✅ | @used_as_elem — mint the canonical loose-list wasm wrapper | `dsl_used_as_elem.cddl` |
| `dsl.used_as_key` | ✅ | @used_as_key — force Ord/Hash derives | `dsl_used_as_key.cddl` |
| `dsl.used_as_key.hash` | ✅ | @used_as_key hash — narrowed Hash derive family | `dsl_used_as_key_hash.cddl` |
| `dsl.used_as_key.hash_ord` | ✅ | @used_as_key hash ord — union of the hash and ord families | `dsl_used_as_key_hash_ord.cddl` |
| `dsl.used_as_key.ord` | ✅ | @used_as_key ord — narrowed Ord derive family | `dsl_used_as_key_ord.cddl` |

### `sentinel` (5)

| construct | | description | evidence |
|-----------|---|-------------|----------|
| `ext.extern` | ✅ | _CDDL_CODEGEN_EXTERN_TYPE_ — compose in a hand-written type | `extern_generic_raw_bytes.cddl` |
| `ext.extern.generic` | ➕ | _CDDL_CODEGEN_EXTERN_TYPE_ generic base with NO instances — re-export only | supported, no corpus fixture (cddl-codegen exit 0) |
| `ext.extern.generic_instance` | ➕ | _CDDL_CODEGEN_EXTERN_TYPE_ generic base WITH an instance — per-instance alias | supported, no corpus fixture (cddl-codegen exit 0) |
| `ext.raw_bytes` | ✅ | _CDDL_CODEGEN_RAW_BYTES_TYPE_ — bytes with hand-written constraints | `extern_generic_raw_bytes.cddl` |
| `ext.raw_bytes.generic` | ➖ | _CDDL_CODEGEN_RAW_BYTES_TYPE_ generic base — refused at parse time | a generic raw-bytes BASE (`foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_`) is rejected by name at parse time: a raw-bytes type is exactly its own bytes and carries no element type a parameter could name, so a parameterized base would emit extern-interface self-check rows and (under --json-schema-export) json-gen registration rows spelling a bare `Foo` — each E0107 against the parameterized type the marker promises, at exit 0 with empty stderr. The remedy the message names is to declare it non-generic. Deliberately the OPPOSITE disposition to a generic EXTERN base (`ext.extern.generic` / `ext.extern.generic_instance`), which names an arbitrary hand-written type that MAY legitimately be parameterized and is therefore recorded-and-skipped by both emitters. Pinned by `generic_raw_bytes_base_rejects_gracefully` and `extern_interface_check_refuses_generic_raw_bytes_base`.  [`element type for a parameter to name`] |

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

## Role × feature containment grid

> Every construct the corpus exercises in ≥1 role, or the containment relation models in ≥1 role.
> Columns are the `role` axis in **grammar order** (`cddl-matrix/roles.toml`, mirrored into
> `matrix.json` `roles`) — top-level outward through the nesting positions. That order is derived, not
> alphabetical; do not "fix" it to a sort.

| mark | meaning |
|------|---------|
| ✅ | the matrix models this cell and every probed shape in it is **supported** |
| ➖ | the matrix models this cell and every probed shape in it is **not supported** |
| ◐ | the matrix models this cell and the probed shapes **disagree** — a support boundary *inside* the cell |
| ? | the matrix models this cell but a spec-allowed row has **no support verdict yet** (awaiting a `verify.ts` grounding run) |
| ✗ | the matrix models this cell only as **spec-disallowed** — the grammar forbids the nesting, so it is never support-probed |
| · | **the corpus exercises this cell and the matrix models nothing here** — no row, so no verdict |
| _(blank)_ | neither modelled nor exercised |

The grid's denominator is "cells the matrix models, plus cells this corpus exercises". A blank cell is
**not** a claim that the nesting is illegal or unsupported — it is a claim that nothing here has an
opinion about it.

**Do not read a `·` next to a `➖` sibling as a contradiction, and do not cross-check the two axes.**
The floor is FEATURE-granular ("an array appears in array-element role"); a containment row is
SHAPE-granular (`contain.array-element.type2.array`'s example is `a = [[int]]`, an *anonymous inline*
array, which is `unsupported`). The corpus exercises `type2.array` as an array element in 5 fixtures and
every corpus fixture compiles under all three profiles (`integration_tests::feature_corpus_compiles`) —
because those fixtures use a *named rule reference*, a different shape. A cell being exercised by the
corpus and marked unsupported by the matrix is therefore two different shapes, never a contradiction.

| construct | Top-level rule body `top-level` | Array element `array-element` | Map member value `map-value` | Map member key `map-key` | Tag content `tag-content` | Choice alternative `choice-member` | Group-choice arm `group-choice-arm` | Occurrence target `occurrence-target` | .cbor / .cborseq payload `cbor-payload` | Generic argument `generic-arg` |
|---|---|---|---|---|---|---|---|---|---|---|
| `ctl.cbor` | · | · | · |  | · |  |  | · | · |  |
| `ctl.default` | · | · | · |  | · |  |  | · |  |  |
| `ctl.eq` |  | · |  |  |  |  |  |  |  |  |
| `ctl.ge` | · | · |  |  |  |  |  |  |  |  |
| `ctl.gt` |  | · |  |  |  |  |  |  |  |  |
| `ctl.le` |  | · | · |  | · |  |  |  |  |  |
| `ctl.lt` |  | · |  |  |  |  |  |  |  |  |
| `ctl.ne` |  | · |  |  | · |  |  |  | · |  |
| `ctl.size` | · | · |  |  |  |  |  |  |  |  |
| `ext.extern` | · |  |  |  |  |  |  |  |  |  |
| `ext.raw_bytes` | · |  |  |  |  |  |  |  |  |  |
| `genericarg.type` | · | · |  |  |  |  |  |  |  |  |
| `genericparm.type` | · |  |  |  |  |  |  |  |  |  |
| `group.choice` | · |  |  |  |  |  |  |  |  |  |
| `grpchoice.sequence` |  |  |  |  |  |  | ✅ |  |  |  |
| `grpent.groupname` |  | ✅ |  |  |  |  | ✅ | ◐ |  |  |
| `grpent.inline_group` | · | ✅ |  |  |  |  | ➖ | ◐ |  |  |
| `grpent.member` |  |  |  |  |  |  | ✅ | ✅ |  |  |
| `memberkey.bareword` |  |  |  | · |  |  | ✅ | ➖ |  |  |
| `memberkey.type1` |  |  |  | ◐ |  |  | ➖ | ◐ |  |  |
| `memberkey.value` |  |  |  | ◐ |  |  | ◐ |  |  |  |
| `occur.bounded` |  | · |  |  |  |  |  |  |  |  |
| `occur.one_or_more` |  | · | · |  |  |  |  |  |  |  |
| `occur.optional` |  | · | · |  |  |  |  |  |  |  |
| `occur.zero_or_more` |  | · | · |  |  |  |  |  |  |  |
| `prelude.any` |  |  |  |  |  | ◐ |  |  |  |  |
| `prelude.b64legacy` |  | · |  |  |  |  |  |  |  |  |
| `prelude.b64url` |  | · |  |  |  |  |  |  |  |  |
| `prelude.bigfloat` |  | · |  |  |  |  |  |  |  |  |
| `prelude.bigint` |  | · |  |  |  |  |  |  |  |  |
| `prelude.bignint` |  | · |  | · |  |  |  |  |  |  |
| `prelude.biguint` |  | · |  |  |  |  |  |  |  |  |
| `prelude.bool` |  | · | · | · |  | · |  | · |  |  |
| `prelude.bstr` |  | · |  | · |  |  |  |  |  |  |
| `prelude.bytes` | · | · | · | · | · | · |  | · | · | · |
| `prelude.decfrac` |  | · |  |  |  |  |  |  |  |  |
| `prelude.eb16` |  | · |  |  |  |  |  |  |  |  |
| `prelude.eb64legacy` |  | · |  |  |  |  |  |  |  |  |
| `prelude.eb64url` |  | · |  |  |  |  |  |  |  |  |
| `prelude.encoded-cbor` |  | · |  |  |  |  |  |  |  |  |
| `prelude.false` | · | ✅ | ✅ |  |  |  |  | · |  |  |
| `prelude.float64` |  | · | · |  |  |  |  | · |  |  |
| `prelude.int` | · | · | · | · | · | · |  | · | · |  |
| `prelude.integer` |  | · |  |  |  |  |  |  |  |  |
| `prelude.mime-message` |  | · |  |  |  |  |  |  |  |  |
| `prelude.nil` | · |  |  |  |  |  |  |  |  |  |
| `prelude.nint` | · | · |  |  |  | · |  |  |  |  |
| `prelude.null` | · | ✅ | ✅ |  | · | ✅ |  | · |  |  |
| `prelude.regexp` |  | · |  |  |  |  |  |  |  |  |
| `prelude.tdate` |  | · |  |  |  |  |  |  |  |  |
| `prelude.text` | · | · | · | · | · | · |  | · |  | · |
| `prelude.true` | · | ✅ | ✅ |  | · | ✅ |  | · |  |  |
| `prelude.tstr` |  | · | · | · |  | · |  | · |  | · |
| `prelude.uint` | · | · | · | · | · | · |  | · | · | · |
| `prelude.undefined` | · | ✅ | ✅ |  |  | ✅ |  |  |  |  |
| `prelude.unsigned` |  | · |  |  |  |  |  |  |  |  |
| `prelude.uri` |  | · |  |  |  |  |  |  |  |  |
| `rangeop.exclusive` |  | · |  |  | · |  |  |  |  |  |
| `rangeop.inclusive` | · | · |  |  | · |  |  |  |  | · |
| `type.choice` | · | ✅ | ✅ | ✗ | ✅ |  |  |  | ✅ | ✗ |
| `type.enum` | · |  |  |  |  |  |  |  |  |  |
| `type1.ctlop` | · | · | · |  | · |  |  | · | · |  |
| `type2.array` | · | ➖ | ➖ | ➖ | ✅ | ➖ |  | ➖ | ➖ |  |
| `type2.map` | · | ➖ | ➖ | · | ✅ | ➖ | ➖ | ➖ | ➖ | ➖ |
| `type2.parenthesized` | · | · |  |  |  |  |  |  | · |  |
| `type2.tag` | · | ✅ | ✅ | ✅ | ➖ | ✅ | ✅ | · | ✅ |  |
| `type2.typename` | · | · | · | · | · | · |  | · | · | · |
| `type2.unwrap` | ➖ | ➖ | ➖ |  |  |  |  |  |  |  |
| `type2.value` | · | ✅ | ✅ | · | · | ◐ | ✅ | ◐ | · | · |
| `value.bytes` | · | ✅ | ✅ |  | · | · |  | · | · |  |
| `value.number` | · | ✅ | ✅ | · | · | · |  | · | · | · |
| `value.text` | · | ✅ | ✅ | · |  | · |  | · |  |  |

- Modelled `(role × feature)` cells: **70** (over 144 shape-granular containment rows).
- Exercised by the corpus **and** modelled: **37**.
- Exercised by the corpus, modelled by **nothing**: **172** (the `·` cells).
- Modelled but not exercised by any corpus fixture: **33**.

## Notable findings

1. Unsupported constructs `panic!` instead of erroring gracefully — valid CDDL using an unsupported construct crashes the generator (the two catch-all arms + the control-op panic). A graceful 'unsupported construct X' error would be friendlier (relates to tests/robustness).
2. Misleading panic message — the control-op catch-all says 'not seen in RFC-8610' even for RFC-8610 operators like `.bits`/`.regexp` (they're in the spec, just unimplemented here).
3. Both the IMPLICIT cut on `:`/bareword keys and the EXPLICIT `^` cut are parsed but their semantics are silently dropped, not enforced — a potential correctness gap (`// TODO: Do we need to handle cuts` in parse_group_type). The explicit-cut example still generates (a literal-key `k ^ => v` routes to the record path; see the memberkey.cut note), so this is a semantics gap, not a generation gap.
4. Socket NAMES aren't really implemented — `$`/`$$` are stripped to plain identifiers, so `$x` silently aliases to `x`. The `/=` half of the plug idiom IS honored: every `/=` statement for one name contributes its arms to a single type-choice rule, in statement order, generating byte-identically to the folded spelling (`tests/corpus/assignt_extend.cddl`; pinned by `incremental_type_choice_extension_equals_the_folded_spelling`). The `//=` group half stays refused gracefully — in either statement order — because merging its arms mints the multi-choice plain-group shape that is itself unsupported (see the assigng.extend note).
5. Float works in every position under every profile, `--preserve-encodings` included: the CBOR head width (`0xf9`/`0xfa`/`0xfb`) is an `Option<cbor_event::Sz>` encoding variable, and a float window is enforced on the same value in both profiles. The corpus carries floats accordingly — `tests/corpus/optional_fixed_float.cddl` (an optional fixed FLOAT member, presence bit plus width) and `homogeneous_array.cddl`'s `float_holder` (per-element widths). Spec-anchored wire vectors live in the `golden_hex_preserve` / `golden_hex_canonical` KAT suites.
6. Methodology — the support probe is EXECUTION-GATED (generate + `cargo test` of the emitted round-trip surface), not exit-code-only, so a spec that exits 0 but emits code that does not compile (or does not round-trip) is correctly ➖. A row whose generated code names USER-SUPPLIED items (the extern/raw-bytes sentinels, a @custom_serialize/@custom_deserialize pair) gets that code written for it rather than an exemption — the probe appends a name-parameterized definition from tests/def_templates/ into the crate roots and then runs the ordinary verdict on both faces plus, under the json profile, the emitted json-gen crate. Only a row whose missing piece is a whole OTHER CRATE stays exempt (see cddl-matrix/README.md § the execution-gate discussion).
7. Single-field STRUCT maps are supported: `{ a: uint }` is a 1-field struct (a bareword key is sugar for the equivalent text-string value key), identical in wire shape to the multi-field `{ a: uint, b: text }` form. MIXED struct+table maps (`{ a: uint, * k => v }`) remain unsupported — a map is detected as EITHER a struct or a homogenous table, never both (now rejected gracefully). Candidate cddl-codegen feature.

## Summary

- Features: **123** — ✅ 80 covered · ➕ 31 supported-untested · ⚠️ 1 partial · ➖ 11 not supported
- Control operators: **37** — ✅ 9 covered · ➕ 0 supported-untested · ➖ 28 not supported (cddl-codegen implements 9 of 37)
- Corpus fixtures: 103

**Per-cell coverage (role × feature).** Where a construct's support *differs by role*,
coverage is keyed on the (role × feature) cell, derived from a real `cddl`-crate AST walk
(`cddl-matrix/examples/ast_roles.rs`) and cross-checked against the matrix's per-cell support verdict — so a
➖ standalone type still surfaces its supported member/choice role (e.g. `prelude.null` ➖ as a top-level
type, ✅ as a choice-member). **4 such cells** are mapped (appended as "also ✅ @role" on the
rows above); constructs whose support doesn't vary by role stay feature-axis (the role is unremarkably
top-level). The full role × feature picture — every construct the corpus exercises or the containment
relation models, in every role — is rendered above in **§ Role × feature containment grid**, joined from
the whole-corpus floor; that is where a cell nothing models shows up.
