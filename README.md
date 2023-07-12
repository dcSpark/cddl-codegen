# Library to generate Rust and WASM code from CDDL specifications

See docs [here](https://dcspark.github.io/cddl-codegen/)

# Old docs (deprecated)

### Purpose ###

Instead of hand-writing CBOR code and writing tests to make sure it matches your CDDL spec, it's much faster to just generate the code from the spec! It will save time and make it easier to keep all your code in sync with any changes to your specification.

You can learn more about CDDL [here](https://github.com/cbor-wg/cddl)

To run execute `cargo run -- --input=input.cddl --output=EXPORT_DIR` to read `./input.cddl` and produce output code in `./EXPORT_DIR/`.

### Output format

Inside of the output directly we always produce a `rust/` directory (including Cargo.toml, etc). Unless we pass in `--wasm=false` we also generate a corresponding `wasm/` directory.
The default format for `rust/` is to have a `lib.rs` containing the structs and `serialization.rs` containing their (de)serialization implementations/corresponding types.
The `wasm/` directory is full of wasm_bindgen-annotated wrappers all in `lib.rs` for the corresponding rust-use-only structs in `rust/` and can be compiled for WASM builds by running `wasm-pack build` on it.

The output format can change slightly if you pass in different command line flags and will be explained in that section.

### Command line flags

* `--input` - Specifies the input CDDL file(s). If it's a single file the output is as above. If a directory is specified e.g. `--input=some_dir` then it will read all files in this directory (non-recursively). The output format changes here. If there's a `lib.cddl` the types contained there are output just as before, and any other file e.g. `foo.cddl` will have its own module `foo/mod.rs` with its own `foo/serialization.rs`, etc.
* `--output` - Specifies the output directory. Nothing fancy here.
* `--lib-name` - Specify the rust crate name for the output library. The wasm crate will have `-wasm` appended.
* `--to-from-bytes-methods` - Generates `to_cbor_bytes()` / `from_cbor_bytes()` methods on all WASM objects. The rust code doesn't need this as you can directly use the `Serialize`/`Deserialize` traits on them.
* `--wasm` - Whether to output a wasm crate. On by default.
* `--preserve-encodings` - Preserves CBOR encoding upon deserialization e.g. definite vs indefinite, map ordering. For each module this will also create a `cbor_encodings.rs` file to potentially store any structs for storing these encodings. This option is useful if you need to preserve the deserialized format for round-tripping (e.g. hashes) or if you want to modify the format to conincide with a specific tool for hashing.
* `--canonical-form` - Used primarily with `--preserve-encodings` to provide a way to override the specific deserialization format and to instead output canonical CBOR. This will have `Serialize`'s trait have an extra `to_canonical_cbor_bytes()` method. Likewise the wasm wrappers (with `--to-from-bytes-methods`) will contain one too.
* `--json-serde-derives` - Derives serde::Serialize/serde::Deserialize for types to allow to/from JSON
* `--json-schema-export` - Tags types with sonSchema derives and generates a crate (in wasm/json-gen) to export them. This requires `--json-serde-derives`.
* `--package-json` - Generates a npm package.json along with build scripts (some of these scripts require `--json-serde-derives`/`--json-schema-export` to work).

### Current capacities

* Primitives - `bytes`, `bstr`, `tstr`, `text`, `uint`, `nint`
* Fixed values - `null`, `nil`, `true`, `false`
* Array values - `[uint]`
* Table types as members - `foo = ( x: { * a => b } )`
* Inline groups at root level - `foo = ( a: uint, b: uint)`
* Array groups - `foo = [uint, tstr, 0, bytes]`
* Map groups (both struct-type and table-type) - `foo = { a: uint, b: tstr }` or `bar = { * uint => tstr }`
* Embedding groups in other groups - `foo = (0, bstr) bar = [uint, foo, foo]`
* Group choices - `foo = [ 0, uint // 1, tstr, uint // tstr }`
* Tagged major types - `rational =  #6.30([ numerator : uint, denominator : uint])`
* Optional fields - `foo = { ? 0 : bytes }`
* Type aliases - `foo = bar`
* Type choices - `foo = uint / tstr`
* Serialization for all supported types.
* Deserialization for almost all supported types (see limitations section).
* CDDL Generics - `foo<T> = [T]`, `bar = foo<uint>`
* Length bounds - `foo = bytes .size (0..32)`
* cbor in bytes - `foo_bytes = bytes .cbor foo`
* Support for the CDDL standard prelude (using raw CDDL from the RFC) - `biguint`, etc
* default values - `? key : uint .default 0`

We generate getters for all fields, and setters for optional fields. Mandatory fields are set via the generated constructor. All wasm-facing functions are set to take references for non-primitives and clone when needed. Returns are also cloned. This helps make usage from wasm more memory safe.

Identifiers and fields are also changed to rust style. ie `foo_bar = { Field-Name: text }` gets converted into `struct FooBar { field_name: String }`

#### Group choices

Group choices are handled as an enum with each choice being a variant. This enum is then wrapped around a wasm-exposed struct as `wasm_bindgen` does not support rust enums with members/values.
Group choices that have only a single non-fixed-value field use just that field as the enum variant, otherwise we create a `GroupN` for the `Nth` variant enum with the fields of that group choice. Any fixed values are resolved purely in serialization code, so `0, "hello", uint` puts the `uint` in the enum variant directly instead of creating a new struct.

#### Type choices

Type choices are handled via enums as well with the name defaulting to `AOrBOrC` for `A / B / C` when inlined as a field/etc, and will take on the type identifier if provided ie `foo = A / B / C` would be `Foo`.
Any field that is `T / null` is transformed as a special case into `Option<T>` rather than creating a `TOrNull` enum.

A special case for this is when all types are fixed values e.g. `foo = 0 / 1 / "hello"`, in which case we generate a special c-style enum in the rust. This will have wasm_bindgen tags so it can be directly used in the wasm crate. Encoding variables (for `--preserve-encodings=true`) are stored where the enum is used like with other primitives.

### Wasm Differences

In the wasm crate we can't always go one to one with the rust crate. Here are some differences/extra types in the WASM create. `AsRef` `From` and `Into` are implemented to go between the rust and wasm crate types to help.

#### Heterogeneous Arrays

`wasm_bindgen` cannot expose doubly-nested types like `Vec<Vec<T>` which can be a limitation if `T` was a non-byte primtive.
Any array of non-primitives such as `[foo]` will generate another type called `FooList` which supports all basic array operations.
This lets us get around the `wasm_bindgen` limitation (without implementing cross-boundary traits which could be inefficient/tedious/complicated).
This array wrapper implements `len() -> self`, `get(usize) -> T` and `add(T)`.

#### Tables

Map literals also generate a type for them with `len() -> usize` and `insert(K, V) -> Option<V>`. The table type will have a `MapKeyToValue` name for whichever `Key` and `Value` types it's exposed as if it's anonymously inlined a as a member, or will take on the identifier if it's a named one.

#### Enums

Both type/group choices generate rust-style enums. On the wasm side we can't do that so we directly wrap the rust type, and then provide a `FooKind` c-style enum for each rust enum `Foo` just for checking which variant it is.

### Comment DSL

We have a comment DSL to help annotate the output code beyond what is possible just with CDDL.

#### @name

For example in an array-encoded group you can give explicit names just by the keys e.g.:
```cddl
foo = [
    bar: uint,
    baz: text
]
```
but with map-encoded structs the keys are stored and for things like integer keys this isn't very helpful e.g.:
```cddl
tx = {
  ? 0: [* input],
  ? 1" [* outputs],
}
```
we would end up with two fields: `key_0` and `key_1`. We can instead end up with fields named `inputs` and `outputs` by doing:
```cddl
tx = {
  ? 0: [* input],   ; @name inputs
  ? 1" [* outputs], ; @name outputs
}
```
Note: the parsing can be finicky. For struct fields you must put the comment AFTER the comma, and the comma must exist even for the last field in a struct.

It is also possible to use `@name` with type choices:
```cddl
foo = 0 ; @name mainnet
    / 1 ; @name testnet
```
and also for group choices:
```cddl
script = [
  ; @name native
  tag: 0, script: native_script //
  ; @name plutus_v1
  tag: 1, script: plutus_v1_script //
  ; @name plutus_v2
  tag: 2, script: plutus_v2_script
]
```

#### @newtype

With code like `foo = uint` this creates an alias e.g. `pub type Foo = u64;` in rust. When we use `foo = uint ; @newtype` it instead creates a `pub struct Foo(u64);`.

#### @no_alias

```cddl
foo = uint
bar = [
  field: foo
]
```
This would normally result in:
```rust
pub type Foo = u64;
pub struct Bar {
    field: Foo,
}
```
but if we use `@no_alias` it skips generating an alias and uses it directly e.g.:
```cddl
foo = uint ; @no_alias
bar = [
  field: foo
]
```
to
```rust
pub struct Bar {
    field: u64,
}
```

#### _CDDL_CODEGEN_EXTERN_TYPE_

While not as a comment, this allows you to compose in hand-written structs into a cddl spec.
```cddl
foo = _CDDL_CODEGEN_EXTERN_TYPE_
bar = [
    x: uint,
    y: foo,
]
```
This will treat `Foo` as a type that will exist and that has implemented the `Serialize` and `Deserialize` traits, so the (de)serialization logic in `Bar` here will call `Foo::serialize()` and `Foo::deserialize()`.
This can also be useful when you have a spec that is either very awkward to use (so you hand-write or hand-modify after generation) in some type so you don't generate those types and instead manually merge those hand-written/hand-modified structs back in to the code afterwards. This saves you from having to manually remove all code that is generated regarding `Foo` first before merging in your own.
