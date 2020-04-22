# Experimental proof of concept for generating rust code for CBOR serialization from CDDL specs.

### Purpose ###

Instead of hand-writing CBOR code and writing tests to make sure it matches your CDDL spec, it's much faster to just generate the code from the spec! It will save time and make it easier to keep all your code in sync with any changes to your specification.

You can learn more about CDDL [here](https://github.com/cbor-wg/cddl)

### Current capacities

Generates a `/export/` folder with wasm-compilable rust code (including Cargo.toml, etc) which can then be compiled with `wasm-pack build`.
The `lib.rs` contains all wasm-exposable code that clients of the generated code can use, and `groups.rs` contians internal implementations for serialization, structure and such that are not exposed to clients. There is also a `prelude.rs` for helper code used by both.

All generated types contain a `new(...)` constructor as well as a `to_bytes()` function that serializes to a byte buffer as the CBOR structure.

* Primitives - `bytes`, `bstr`, `tstr`, `text`, `uint`, `nint` (last two truncated to 32 bytes for now)
* Fixed values - `null`, `nil`, `true`, `false`
* Array values - `[uint]`
* Inline groups at root level - `foo = ( a: uint, b: uint)`
* Array groups - `foo = [uint, tstr, 0, bytes]`
* Map groups (both struct-type and table-type) - `foo = { a: uint, b: tstr }` or `bar = { * uint => tstr }`
* Embedding groups in other groups - `foo = (0, bstr) bar = [uint, foo, foo]`
* Group choices - `foo = [ 0, uint // 1, tstr, uint // tstr }`
* Tagged major types - `rational =  #6.30([ numerator : uint, denominator : uint])`
* Optional fields - `foo = { ? 0 : bytes }`
* Type aliases - `foo = bar`
* Type choices - `foo = uint / tstr`.

It should be noted that for our purposes when we encounter a type that is an alias or transitiviely an alias for binary bytes, we always create a wrapper type for it, as in our use cases those should not be mixed and are crypto keys, hashes, and so on.

Any array of non-primitives such as `[foo]` will generate another type called `foos` which supports all basic array operations.
This lets us get around the `wasm_bindgen` limitation (without implementing cross-boundary traits which could be inefficient/tedious/complicated) discussed in the limitations section.

Identifiers and fields are also changed to rust style. ie `foo_bar = { Field-Name: text }` gets converted into `struct FooBar { field_name: String }`

Group choices are handled as an enum with each choice being a variant. This enum is then wrapped around a wasm-exposed struct as `wasm_bindgen` does not support rust enums with members/values.
Group choices that are a single field use just that field as the enum variant, otherwise we create a `GroupN` enum with the fields of that group choice.

Type choices are handled via enums as well with the name defaulting to `AOrBOrC` for `A / B / C` when inlined as a field/etc, and will take on the type identifier if provided ie `foo = A / B / C` would be `Foo`.
Any field that is `T / null` is transformed as a special case into `Option<T>` rather than creating a `TOrNull` enum.

### Limitations

* Primitive `int` not supported due to no type choice support (defined as `uint / nint`)
* There is no support for deserialization as this is not of immediate use for us.
* No accessor functions (easily added but we don't need them yet as the focus is constructing CBOR not deserializing)
* Does not support optional group `[(...)]` or `{(...)}` syntax - must use `[...]` for `{...}` for groups
* Ignores occurence specifiers: `*`, `+` or `n*m`
* No support for sockets
* No inline maps as fields - `foo = ( x: { y: uint, z: uint } )`, but is fine for `bar = { y: uint, z: uint }` then `foo = ( x: bar )`. Oonly found in block definition for `shelley.cddl`
* No heterogenous arrays as fields - `foo: [uint]` is fine but `foo: [uint, tstr]` is not. Not found anywhere in `shelley.cddl`
* CDDL generics not supported - just edit the cddl to inline it yourself for now
* Keys in struct-type maps are limited to `uint` and text. Other types are not found anywhere in `shelley.cddl`.
* Optional fixed-value fields not properly supported - `(? foo: 5)`


`wasm_bindgen` also cannot expose doubly-nested types like `Vec<Vec<T>` which can be a limitation if `T` was a non-byte primtive.
