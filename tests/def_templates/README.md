# `tests/def_templates/` — name-parameterized hand-written extern defs

Templates for the user-supplied code a `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_`
rule declares. They exist so a compile gate can seed a *trivial but real* definition for ANY spec
that carries those markers, instead of exempting that spec from compiling at all: the rust type name
a marker rule mints is derived from the rule name, so a fixed-name def file (`tests/external_rust_defs`
and friends) only ever serves the one fixture it was written for, while a compile floor needs one per
shape at breadth.

Substitution is a literal replace of these tokens; nothing else is templated:

| token | meaning |
| --- | --- |
| `__NAME__` | the rust type name the marker rule mints (`foo` → `Foo`) |
| `__DERIVES__` | the derive list, spliced by the consumer per emission profile (json adds `serde::Serialize, serde::Deserialize, schemars::JsonSchema`; `@copy` adds `Copy`) |

Residence is the documented user contract, not the generated tree
(`docs/docs/output_format.mdx` § Generated crate roots): the `.rust` / `.wasm` templates are appended
to the seed-once thin `rust/src/lib.rs` / `wasm/src/lib.rs`, where the generator's `pub use crate::<Name>;`
glue resolves against them. A definition inside `src/generated/**` would collide E0255 with that glue
and models a layout no real consumer can keep (that subtree is clobbered every run).

Consumers: `cddl-matrix/verify.ts` (the matrix compile gate's def splice) and
`src/tests/integration_tests.rs` (the feature-corpus and wasm-matrix compile floors).
