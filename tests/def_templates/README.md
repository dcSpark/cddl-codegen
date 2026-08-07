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
| `__SER__` / `__DESER__` | the `@custom_serialize` / `@custom_deserialize` targets a spec names |

A codec template comes in two flavors because `--preserve-encodings` changes its SIGNATURE, not its
body: the base file takes the value alone, the `_preserve` twin takes and returns the encoding
variables the rule demands (and the `_declared_preserve` twin the ones `@custom_encodings` DECLARES,
which is the whole point of that directive). A codec also needs a hand `use` into the generated
scope — the remedy `docs/docs/comment_dsl.mdx` names for a bare, non-path-qualified codec name — so
its consumers emit `pub use crate::{…};` into `generated/mod.rs` alongside the definition. That
import does NOT survive a regeneration (the subtree is clobbered); the durable spelling for a
consumer who regenerates is a fully-qualified codec path in the CDDL.

Residence is the documented user contract, not the generated tree
(`docs/docs/output_format.mdx` § Generated crate roots): the `.rust` / `.wasm` templates are appended
to the seed-once thin `rust/src/lib.rs` / `wasm/src/lib.rs`, where the generator's `pub use crate::<Name>;`
glue resolves against them. A definition inside `src/generated/**` would collide E0255 with that glue
and models a layout no real consumer can keep (that subtree is clobbered every run).

Consumers, all over the same files so a definition cannot be right in one gate and wrong in another:
`cddl-matrix/verify.ts`'s `DEF_SPLICE` (the matrix rows), and `src/tests/integration_tests.rs`'s
`CORPUS_DEF_SPLICE` / `append_extern_defs` (the feature-corpus, component-corpus, regen and
wasm-matrix compile floors).
