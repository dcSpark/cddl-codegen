# Generating/updating cardano-serialization-lib using this library

We generated the bulk of the base CBOR struct code for our [cardano-serialization-lib](https://github.com/Emurgo/cardano-serialization-lib/). However, there are things that are not supported that IOHK have used, and some things are supported but might need editing post-generation. This document is useful mostly for people maintaining `cardano-serialization-lib`, but parts can be useful for anyone who wants to use this codegen tool on more complex CDDL types.



## pre-processing


Remember that we don't (yet) support multi-file CDDL so everything will have to be inlined into one file. The order technically doesn't matter as we do multi-phase parsing + code generation.


### Specific to IOHK's CDDL

Before we can generate using for example `alonzo.cddl` we must realize that this is not the complete CDDL. Inside of that directory's `mock/` we can find `crypto.cddl` and `extras.cddl` which contain important types. The crypto ones are partially incorrect and are just mocked crypto testing sizes, so care must be taken to ensure that they are of the appropriate size. We mostly do not directly generate these types but instead have some macros inside of the serialization lib's `crypto.rs` + `util.rs` to implement some of them for us. Some other types such as `positive_interval = #6.30([1, 2])` are purely mocked out as I believe IOHK tests against/using the CDDL directly and this made it easier for them.

### General tips

As noted in the readme, not every aspect of CDDL is fully supported by this codegen tool. To work around this it might be necessary to edit the CDDL beforehand. You can define these complex types as something simple like `foo == [0]` and it will generate that base struct which you can then fill in the complex details later. If it is just a single field that is not supported, changing just that one and hand-writing it later is a good approach. Before generics were implemented we used to just inline the types directly into the generic implementation. This could still be done for more complex generics as we only support the normal cases.


### Integer precision

CDDL gives us the `uint` type which can be up to 64 bits, but especially for wasm/js interop, this isn't very easy to use. We provide our own rust-specific types: `u32`, `i32`, `u64`, `i64` when `USE_EXTENDED_PRELUDE` is enabled to have more control over this. This can be a problem as `u64` for example converts to `bigint` in JS when built via `wasm_bindgen`, but your environment might not support this yet. Note that the structs will fail to deserialize if an integer out of these smaller bounds is encountered even if it's valid according to the CDDL, as it won't fit in the `u32` or whichever you selected.



## post-processing

### Deserialization not supported?

Some types such as array-represented records with optional types do not support deserialization but will still generate the rest of the code, including serialization. It might be useful to generate the cddl of these specific types with the optional fields removed so that the library can output a deserialize trait that can help you work to implement the rest. This can be non-trivial, especially if there are multiple optional types, or there is some overalap of types for example the type `foo = [uint, uint?, uint, text? text]` would be non-trivial as we can't just check the next type, as if upon reading the 2nd `uint` we can't know if it was meant for the 2nd or 3rd field until we've parsed more. We also can't always rely on a length check since that is harder for indefinite encoded types causing us to need to read more to figure that out, as well as the case where there are multiple optional types. This non-triviality is precisely why a 100% general solution was not implemented in cddl-codegen.

### Extra checks?

While the tool now supports the `.size` specifier, i.e. `.size (0..32)`, we don't support other modifiers such as `n*m`, `*`, `+`, etc, so these checks will need to be hand-written after-the-fact. Regular `.size` ones are now done for you.

### Constant enums?

cddl-codegen can generate arbitrary type choices, which encompass enums, but this generality leads to extra code generation when we just need a simple constant-valued enumeration. In these cases you might get both a `FooEnum` and `FooKind` from `foo = 0 / 1 / 2` where only one would do the job. This will hopefully be implemented in the tool as a special case, but in the meantime just get rid of `FooKind` and try and edit the code so that you can use `FooEnum` for both, which requires a bit of editing. For a reference see `NetworkId` in `cardano-serialization-lib`. Single-value enums are also ambiguous as is the case for `language = 0 ; Plutus v1` which will generate a function like `pub fn language() -> u32 { return 0; }` whereas what we wanted was an enum with 1 value. To generate that just do `language = 0 / 1` then remove the other variant by hand later, or cddl-codegen will see it as a single isolated constant.

### wasm_bindgen issues?

Type aliases for primitives can potentially lead to the generator to generate `&T` instead of `T` for a parameter even when `T` is a primitive. We should investigate this at some point if it's not fixed by now. `wasm_bindgen` also does not properly support `Option<&T>` so if you have a field like `foo = { ? 0 : &T }` then you will likely want to rewrite the accessors/ctor for that field, or change it to a `Option<T>` and do some cloning. It's unfortunate but until `wasm_bindgen` is improved, there will be a lot of needless cloning to make a memory-safe library.

### to_bytes() / from_bytes()

This is only specific to `cardano-serialization-lib` - We don't use those `ToBytes`/`FromBytes` traits from this codegen tool as we need to provide good errors to both JS consumers of wasm builds, as well as people using the rust code (e.g. mobile bindings). Instead we have a `to_from_bytes!()` macro which we call on every type we wish to have those functions which auto-converts to appropriate error types.
