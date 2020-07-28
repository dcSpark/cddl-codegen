# Experimental library for generating rust code for CBOR serialization from CDDL specs.

### Purpose ###

Instead of hand-writing CBOR code and writing tests to make sure it matches your CDDL spec, it's much faster to just generate the code from the spec! It will save time and make it easier to keep all your code in sync with any changes to your specification.

You can learn more about CDDL [here](https://github.com/cbor-wg/cddl)

To run, execute `cargo run` from within this directory, and it will read `input.cddl` and generate the rust code in `./export/`.

### Current capacities

Generates a `/export/` folder with wasm-compilable rust code (including Cargo.toml, etc) which can then be compiled with `wasm-pack build`.
The `lib.rs` contains all wasm-exposable code that clients of the generated code can use, and `serialization.rs` contians internal implementations for serialization/deserialization.
All structs have a `new(...)` constructor as well as a `to_bytes()`, and all supported ones have a `from_bytes()` exposed within their `lib.rs` impls that call these which (de)serialize to/from byte buffers the CBOR structure.
The constructor will contain all mandatory fields as arguments, whereas optional parameters will have a `set_*` function generated.
There is also a `prelude.rs` for helper code used by both (errors, traits, etc).

* Primitives - `bytes`, `bstr`, `tstr`, `text`, `uint`, `nint` (last two truncated to 32 bytes for now for convenience instead of going through BigInt for everything)
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

We generate getters for all fields, and setters for optional fields. Mandatory fields are set via the generated constructor. All wasm-facing functions are set to take references for non-primitives and clone when needed. Returns are also cloned. This helps make usage from wasm more memory safe.

Identifiers and fields are also changed to rust style. ie `foo_bar = { Field-Name: text }` gets converted into `struct FooBar { field_name: String }`

There are several arguments that are set at the top of `main.rs` to configure code generation:
* `ANNOTATE_FIELDS` - Annotates errors with locational context if set. On by default.
* `BINARY_WRAPPERS` - When we encounter a type that is an alias or transitively an alias for binary bytes, we create a wrapper type for it, as in some use cases those should not be mixed and are crypto keys, hashes, and so on. Otherwise generates a type alias. On by default.
* `GENERATE_TO_FROM_BYTES` - Generates `to_bytes()` and `from_bytes()` usable from wasm in addition to the `Serialize` and `Deserialize` traits. Off by default.

#### Heterogeneous Arrays

`wasm_bindgen` cannot expose doubly-nested types like `Vec<Vec<T>` which can be a limitation if `T` was a non-byte primtive.
Any array of non-primitives such as `[foo]` will generate another type called `foos` which supports all basic array operations.
This lets us get around the `wasm_bindgen` limitation (without implementing cross-boundary traits which could be inefficient/tedious/complicated).
This array wrapper implements `len() -> self`, `get(usize) -> T` and `add(T)`.

#### Tables

Map literals also generate a type for them with `len() -> usize` and `insert(K, V) -> Option<V>`. The table type will have a `MapKeyToValue` name for whichever `Key` and `Value` types it's exposed as if it's anonymously inlined a as a member, or will take on the identifier if it's a named one.

#### Group choices

Group choices are handled as an enum with each choice being a variant. This enum is then wrapped around a wasm-exposed struct as `wasm_bindgen` does not support rust enums with members/values.
Group choices that are a single field use just that field as the enum variant, otherwise we create a `GroupN` for the `Nth` variant enum with the fields of that group choice.

#### Type choices

Type choices are handled via enums as well with the name defaulting to `AOrBOrC` for `A / B / C` when inlined as a field/etc, and will take on the type identifier if provided ie `foo = A / B / C` would be `Foo`.
Any field that is `T / null` is transformed as a special case into `Option<T>` rather than creating a `TOrNull` enum.

### Limitations

* Does not support optional group `[(...)]` or `{(...)}` syntax - must use `[...]` for `{...}` for groups
* Ignores occurrence specifiers: `*`, `+` or `n*m`
* No support for sockets
* No inlined heterogeneous maps as fields - `foo = ( x: { y: uint, z: uint } )`, but is fine for `bar = { y: uint, z: uint }` then `foo = ( x: bar )`.
* No inlined heterogeneous arrays as fields - `foo: [uint]` is fine but `foo: [uint, tstr]` is not.
* CDDL generics not supported - just edit the cddl to inline it yourself for now.
* Keys in struct-type maps are limited to `uint` and text. Other types are not found anywhere in `shelley.cddl`.
* Optional fixed-value fields not properly supported - `(? foo: 5)`
* Deserialization not supported for maps with nested plain groups - `foo = (uint, uint), bar = { foo, text }` due to maps not being ordered.
* Deserialization not supported for optional types inside of arrays - `foo = [uint, ?uint, uint, text, ?text, text, uint]` because it could require a combinatorial backtrack for ambiguous examples like this.
* Cannot read multi-file CDDL definitions. Only reads from `input.cddl` right now.
* No CLI arguments to configure the codegen yet - use the constants at top of `main.rs`.
* Does not validate finite array length in deserialization.
