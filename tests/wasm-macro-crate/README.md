# wasm-macro-crate

Real macro definitions backing the `--wasm-list-macro` / `--wasm-conversions-macro` compile gate
(`integration_tests::wasm_list_macro_compiles`), mirroring how `tests/extern-dep-crate` supplies a
real dependency for the extern-deps test. It is not a test itself but is used by one.

The macros expand to the same shape the generator would emit inline with the flags off (wrapper
struct + `new`/`len`/`get`/`add` accessors + `From`/`AsRef` conversions), and are written so that a
malformed emission — the exact bug class a source snapshot cannot judge — fails to compile:

- `needs_into`/`is_copy` are matched as **literal tokens** per macro arm, so an emission with a
  combination that has no arm (e.g. the unreachable `(true, true)`) is rejected outright.
- the `needs_into = false` arms pin `rust_elem == wasm_elem` with an identity-coercion check, so
  wrongly claiming a wrapper type needs no conversion fails.
- the `needs_into = true` arm requires `wasm_elem: AsRef<rust_elem>` (the conversion contract every
  generated wasm wrapper satisfies), so wrongly claiming a primitive needs conversion fails.
- the `is_copy = true` arms move out of the backing `Vec` and assert `Copy`, so a non-`Copy`
  element wrongly marked copyable fails. (The reverse — `Copy` marked `is_copy = false` — only
  costs a redundant clone and is not detectable at compile time.)
- `impl_wasm_conversions!` takes the wasm side as a bare `:ident` and builds `Self(native)` /
  `wasm.0`, so swapped arguments (a path where the ident belongs, or a non-newtype wasm side)
  fail.
