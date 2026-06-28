# Corpus coverage map — CDDL (RFC 8610) constructs

Tracks which CDDL language constructs the snapshot **corpus** exercises, so a future revisit can see
at a glance what's covered, what's *supported but untested* (a corpus gap to fill), and what the
generator deliberately does **not** support (the boundary). This is the input-language analog of
`tests/golden_hex/COVERAGE.md`.

- **Test:** `tests/corpus/<construct>.cddl`, driven by `snapshot_tests::feature_corpus` — each file is
  generated under every flag profile (`default`/`preserve`/`json`) plus an IR dump, and the generated
  *source* is snapshotted. Bless with `INSTA_UPDATE=always cargo test snapshot_tests`.
- **Compile gate:** `integration_tests::feature_corpus_compiles` `cargo check`s every corpus file under
  all three profiles, so a corpus entry must produce **compiling** Rust under *all* of them.
- **RFC reference:** RFC 8610 (CDDL) — <https://www.rfc-editor.org/rfc/rfc8610>. Offline copy at
  `draft/golden-vectors/rfc8610.txt` (gitignored): `curl -O https://www.rfc-editor.org/rfc/rfc8610.txt`.

## Axis nuance (read before the tables)

This map is anchored to the **external spec (RFC 8610)**, not to our own feature list
(`docs/docs/current_capacities.mdx`) — anchoring to the spec is what makes the *unsupported* rows
visible, which a self-feature-list structurally can't show. Two things stay out of this RFC matrix and
get their own sections below: **(a)** the corpus snapshots generated *source*, not wire bytes — wire
encodings are golden_hex's axis (RFC 8949); **(b)** cddl-codegen's invented features (comment DSL,
extern/raw-bytes sentinels) aren't RFC 8610 and are tracked separately.

## How evidence is cited (keep it robust)

Evidence is a **stable, grep-able anchor — never a line number** (line numbers rot on the first edit
above them). Pick the most specific stable thing:
- **✅ covered** → the **corpus file** (`tagged.cddl`). It's the artifact; rename/delete it and the
  snapshot test fails, so the claim can't silently rot.
- **➕ supported, untested** → the **handler function** (e.g. `parse_control_operator`) and/or presence
  in **`supported.cddl`**.
- **➖ not supported** → the **quoted `panic!`/`todo!`/`unimplemented!` message**, or the **unmatched
  AST variant** (e.g. `Type2::Unwrap`) plus the catch-all it falls into. A quoted message is the best
  anchor: greppable, and if someone changes it they're almost certainly changing the behavior, so it
  self-invalidates *meaningfully*.

The two catch-all arms most ➖ rows fall into (both grep-able by message): `parse_type`'s
`panic!("…ignored typename…")` (top-level rule bodies) and `rust_type_from_type2`'s
`panic!("Ignoring Type2: …")` (inline members).

## Legend

| mark | meaning |
|------|---------|
| ✅ | covered — a corpus file isolates this construct |
| ➕ | **supported but untested** — handled in code, no corpus file yet (a real gap) |
| ➖ | **not supported** — errors / `panic!` / `todo!` / no handling branch (documents the boundary) |
| ⚠️ | partial / parsed-but-not-honored — accepted but the semantics aren't modeled |
| ❓ | uncertain — needs verification |

## RFC 8610 coverage

### Primitives & representation (§2.2.3, Appendix D prelude)
| construct | status | corpus / evidence |
|-----------|--------|-------------------|
| `uint` `nint` `int` `bstr`/`bytes` `tstr`/`text` `bool` `null`/`nil` | ✅ | `primitives.cddl`, `bool.cddl`, `nullable.cddl` (`int` is special-cased — `apply_type_aliases` returns `None` for it) |
| `float` / `float16` / `float32` / `float64` | ⚠️ | de/ser works under `default`/`json`, but `--preserve-encodings` and bounds are unsupported: `unimplemented!("preserve_encodings is not implemented for float")` and `unimplemented!("bounds not supported for floats")` (`generation.rs`). **Can't be a corpus entry** (corpus runs `preserve`); same reason `prelude.cddl` omits float-bearing types |
| extended prelude `biguint` `uri` `bigint` `unsigned` | ✅ | `prelude.cddl` (expanded from raw RFC CDDL by `emit_prelude`) |
| extended prelude `tdate` `time` `number` `bignint` `integer` `decfrac` `bigfloat` `encoded-cbor` `b64url` `b64legacy` `regexp` `mime-message` | ➕ | handled by `emit_prelude`, no corpus (`time`/`number` pull in `float`, so blocked like float) |
| `eb64url` `eb64legacy` `eb16` `cbor-any` | ➖ | `panic!("unsupported cddl prelude type")` (they reduce to `any`) |
| `undefined`, `any`, simple values `#7.n` | ➖ | `any`/`undefined` → `panic!("unsupported cddl prelude type")`; `#7.n` / bare `#` → catch-all panic |
| major-type sigils `#` `#0`..`#7`, bare `#6.n` (no parens) | ➖ | `Type2::Any` / `Type2::DataMajorType` unmatched → catch-all panic. Only `#6.n(T)` (`Type2::TaggedData`) is handled (see Tags) |
| byte/text string literals `h'…'` `b64'…'` `'…'` | ➖ | `Type2::UTF8ByteString`/`B16ByteString`/`B64ByteString` unmatched → catch-all panic |

### Composition & structure (§2.1, §3.4, §3.5)
| construct | status | corpus / evidence |
|-----------|--------|-------------------|
| Arrays `[…]` (fixed & variable) | ✅ | `array.cddl` (`[uint,text,bytes]`), `homogeneous_array.cddl` (`[* uint]`) |
| Maps — struct-style `{ a: T }` & table `{ * K => V }` | ✅ | `map_struct.cddl`, `table.cddl` |
| Groups `(…)` + embedding groups in groups | ✅ | `nested_group.cddl`; also `supported.cddl` (`pool_params`) |
| Inline group at root `foo = (a: uint, b: uint)` | ✅ | `nested_group.cddl` |
| Cuts in maps (`^`, implicit) (§3.5.4) | ⚠️ | parsed but **dropped, not enforced** — `// TODO: Do we need to handle cuts for what we're doing?` (`parse_group_type`, `parsing.rs`) |

### Occurrence (§3.2)
| construct | status | corpus / evidence |
|-----------|--------|-------------------|
| `?` optional | ✅ | `optional.cddl`, `default_value.cddl` |
| `*` zero-or-more | ✅ | `homogeneous_array.cddl`, `table.cddl` |
| `+` one-or-more | ➕ | `Occur::OneOrMore` in `parse_group_type`, no corpus |
| `n*m` bounded occurrence | ➕ | `Occur::Exact` in `parse_group_type`, no corpus |

### Choices (§2.2.2)
| construct | status | corpus / evidence |
|-----------|--------|-------------------|
| Type choice `/` (and `T / null` → `Option<T>`) | ✅ | `type_choice.cddl`, `nullable.cddl` |
| Group choice `//` | ✅ | `group_choice.cddl`; also `supported.cddl` (`multisig_script`, `certificate`) |
| All-fixed-value type choice → c-style enum | ✅ | `c_style_enum.cddl` (`0 / 1 / 2`) |
| Choice-from-group `&` (`&basecolors`, `&(…)`) (§2.2.2.2) | ➖ | `Type2::ChoiceFromGroup` / `Type2::ChoiceFromInlineGroup` unmatched → catch-all panic |

### Ranges (§2.2.2.1)
| construct | status | corpus / evidence |
|-----------|--------|-------------------|
| Inclusive `..` | ✅ | `sized_int.cddl` (`0..4294967295`, `-128..127`) |
| Exclusive `...` | ➕ | same `RangeCtlOp::RangeOp` arm in `parse_control_operator` (`is_inclusive:false`), no corpus |

### Tags, unwrap, generics, sockets (§3.6, §3.7, §3.10, §3.9)
| construct | status | corpus / evidence |
|-----------|--------|-------------------|
| Tagged `#6.n(T)` | ✅ | `tagged.cddl` (`#6.42(text)`); `supported.cddl` (`unit_interval`). Doubly-nested tags panic → ➖ sub-case |
| Generics `foo<T>` | ✅ | `generics.cddl`. Limits are `todo!`: generic-of-generic, `foo<T>=T/null`, generic group choices |
| Unwrap `~` (§3.7) | ➖ | `Type2::Unwrap` unmatched → catch-all panic |
| Type socket `$` / group socket `$$` (§3.9) | ⚠️/❓ | `$`/`$$` are merely *stripped* from identifiers (`$hash32`→`hash32`) so a socket *reference* aliases cosmetically (`supported.cddl` uses `$hash32`); true plugging (`/=`, `//=`) is ignored in `parse_rule`. `$$` group-socket semantics ❓ untested |

### Control operators (§3.8)
Dispatched in `parse_control_operator`; an unmatched operator hits
`panic!("Unknown (not seen in RFC-8610) range control operator: …")`.
| operator | status | corpus / evidence |
|----------|--------|-------------------|
| `.size` (§3.8.1) | ✅ | `bounded_bytes.cddl`, `sized_text.cddl`, `sized_int.cddl` (signed-int `.size` panics) |
| `.cbor` (§3.8.4) | ✅ | `cbor_in_bytes.cddl` |
| `.default` (§3.8.6) | ✅ | `default_value.cddl` |
| `.le` (§3.8.6) | ✅ | `sized_int.cddl`; `supported.cddl` (`uint .le 65535`) |
| `.lt` `.gt` `.ge` `.eq` `.ne` (§3.8.6) | ➕ | handled in `parse_control_operator`, no corpus (`.ne` is a `(v+1,v-1)` hack) |
| `.bits` (§3.8.2) | ➖ | no arm → `panic!("Unknown (not seen in RFC-8610) range control operator: …")` |
| `.regexp` / `.pcre` (§3.8.3) | ➖ | no arm → same control-op panic |
| `.cborseq` (§3.8.4) | ➖ | `todo!("control operator cborseq not supported")` |
| `.within` `.and` (§3.8.5) | ➖ | `todo!("control operator {} not supported", …)` |

## cddl-codegen extensions (not RFC 8610)
| construct | status | corpus / evidence |
|-----------|--------|-------------------|
| Comment DSL `@name` `@doc` `@newtype` `@no_alias` `@used_as_key` `@custom_serialize`/`@custom_deserialize` `@custom_json` | ✅ | `dsl_name/doc/newtype/no_alias/used_as_key/custom.cddl` (parsed in `comment_ast.rs`) |
| `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_` sentinels | ✅ (integration, no corpus) | handlers `new_extern` / `new_raw_bytes`; tests in `tests/raw-bytes/`, `tests/extern-deps/`. Snapshot-only corpus coverage is blocked (they emit undefined user types → break `feature_corpus_compiles`); see `CLEAR_WINS_PLAN.md` skip-list note |

## Other documents to process later
This pass covers **RFC 8610 only**. Known follow-ups (verify numbers/contents before relying on them):
- **RFC 9165 — Additional Control Operators for CDDL** (`.plus` `.cat` `.det` `.abnf` `.abnfb` `.feature`):
  the cddl crate has tokens for all of them, but cddl-codegen has **no arms**, so each hits the
  `parse_control_operator` "Unknown … range control operator" panic — i.e. currently ➖. Worth its own
  pass (fetch to `draft/golden-vectors/rfc9165.txt`).
- **CDDL modules / later updates** (e.g. the `draft-ietf-cbor-cddl-modules` line of work): TBD — confirm
  the exact RFC/draft before adding rows.

## Summary
- **Covered (✅):** all the core building blocks — primitives, fixed/variable arrays & maps (struct +
  table), groups & embedding, inline root groups, type/group choices (+ `T/null`→Option, all-fixed
  c-enum), inclusive ranges, `#6.n` tags, generics, `?`/`*` occurrence, `.size`/`.cbor`/`.default`/`.le`,
  the tested prelude subset, and all comment-DSL features.
- **Corpus gaps (➕) — actionable, all profile-safe to add:** `+` and `n*m` occurrence, exclusive range
  `...`, `.lt`/`.gt`/`.ge`/`.eq`/`.ne`. (Float and float-bearing prelude types are ➕ in principle but
  **blocked from the corpus** by the `preserve` `unimplemented!`, so they need a default/json-only home,
  not a standard corpus file.)
- **Boundary (➖):** `any`/`undefined`/`#7.n`/bare `#`/`#0..#7`, string literals, the `eb*`/`cbor-any`
  prelude, choice-from-group `&`, unwrap `~`, `.bits`/`.regexp`/`.cborseq`/`.within`/`.and`, and all
  RFC 9165 operators. Sockets are ⚠️ (reference-only), cuts are ⚠️ (ignored).

## Notable findings (fell out of the gap analysis)
1. **Unsupported constructs `panic!` instead of erroring gracefully** — feeding otherwise-valid CDDL
   that uses an unsupported construct crashes the generator (the two catch-all arms + the control-op
   panic). Relates to `tests/robustness`; a graceful "unsupported construct X" error would be friendlier.
2. **Misleading panic message** — the control-op panic says *"not seen in RFC-8610"* even for RFC-8610
   operators like `.bits`/`.regexp` (they're in the spec, just unimplemented here).
3. **Cuts in maps are parsed but not enforced** — a potential correctness gap.
4. **Sockets aren't really implemented** — `$`/`$$` are stripped to plain identifiers, so `$x` silently
   aliases to `x`; the extensible plug mechanism (`/=`, `//=`) is ignored.
5. **Float is fine until `--preserve-encodings` or bounds** (the `unimplemented!` sites above) — known,
   and the reason the corpus avoids floats.
