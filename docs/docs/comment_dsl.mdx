---
sidebar_position: 7
---


# Comment DSL

We have a comment DSL to help annotate the output code beyond what is possible just with CDDL.

## @name

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
  ? 1: [* outputs],
}
```
we would end up with two fields: `key_0` and `key_1`. We can instead end up with fields named `inputs` and `outputs` by doing:
```cddl
tx = {
  ? 0: [* input],   ; @name inputs
  ? 1: [* outputs], ; @name outputs
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

## @newtype

With code like `foo = uint` this creates an alias e.g. `pub type Foo = u64;` in rust. When we use `foo = uint ; @newtype` it instead creates a `pub struct Foo(u64);`.

## @no_alias

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

## _CDDL_CODEGEN_EXTERN_TYPE_

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