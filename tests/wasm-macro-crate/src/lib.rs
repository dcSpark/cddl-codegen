//! Working `impl_wasm_list!` / `impl_wasm_conversions!` definitions matching the invocation
//! signatures `--wasm-list-macro` / `--wasm-conversions-macro` emit (see `generate_array_type` and
//! `add_conversion_methods` in `src/generation.rs`). See README.md for how each arm is armed to
//! reject malformed emissions.

/// Expands to the list-wrapper struct + accessors + conversions the generator would otherwise emit
/// inline. Invocation shape (emitted by `generate_array_type`):
///
/// ```text
/// impl_wasm_list!(rust_elem, wasm_elem, WasmName, needs_into, is_copy);
/// ```
///
/// `needs_into`/`is_copy` are matched as literal tokens; there is deliberately no `(true, true)`
/// arm because that combination is unreachable (only primitives/c-enums are `Copy`, and neither
/// needs `.into()`) — emitting it is a generator bug and fails to compile here.
#[macro_export]
macro_rules! impl_wasm_list {
    // needs_into = true: the element is wrapped at the wasm boundary; conversions go through
    // `From`/`Into`.
    ($rust_elem:ty, $wasm_elem:ty, $wasm_name:ident, true, false) => {
        const _: () = {
            // needs_into must mean "the wasm element wraps the rust element": every generated
            // wasm wrapper carries `AsRef<rust>`, while identity boundaries (u64, String, ...) do
            // not, so a spurious needs_into = true fails this bound.
            fn assert_wrapper<Wasm: AsRef<Rust> + Clone, Rust>() {}
            #[allow(dead_code)]
            fn check() {
                assert_wrapper::<$wasm_elem, $rust_elem>();
            }
        };

        #[derive(Clone, Debug)]
        #[wasm_bindgen::prelude::wasm_bindgen]
        pub struct $wasm_name(pub(crate) Vec<$rust_elem>);

        #[wasm_bindgen::prelude::wasm_bindgen]
        impl $wasm_name {
            pub fn new() -> Self {
                Self(Vec::new())
            }

            pub fn len(&self) -> usize {
                self.0.len()
            }

            pub fn get(&self, index: usize) -> $wasm_elem {
                self.0[index].clone().into()
            }

            pub fn add(&mut self, elem: &$wasm_elem) {
                self.0.push(elem.clone().into());
            }
        }

        $crate::impl_wasm_conversions!(Vec<$rust_elem>, $wasm_name);
    };
    // needs_into = false, is_copy = true: the element crosses the boundary as itself, by value.
    ($rust_elem:ty, $wasm_elem:ty, $wasm_name:ident, false, true) => {
        const _: () = {
            // needs_into = false must mean the rust and wasm element types are the SAME type...
            #[allow(dead_code)]
            fn identity(x: $rust_elem) -> $wasm_elem {
                x
            }
            // ...and is_copy = true must mean it actually is Copy.
            fn assert_copy<T: Copy>() {}
            #[allow(dead_code)]
            fn check() {
                assert_copy::<$rust_elem>();
            }
        };

        #[derive(Clone, Debug)]
        #[wasm_bindgen::prelude::wasm_bindgen]
        pub struct $wasm_name(pub(crate) Vec<$rust_elem>);

        #[wasm_bindgen::prelude::wasm_bindgen]
        impl $wasm_name {
            pub fn new() -> Self {
                Self(Vec::new())
            }

            pub fn len(&self) -> usize {
                self.0.len()
            }

            pub fn get(&self, index: usize) -> $wasm_elem {
                // copies out of the Vec: E0507 if the element is not actually Copy
                self.0[index]
            }

            pub fn add(&mut self, elem: $wasm_elem) {
                self.0.push(elem);
            }
        }

        $crate::impl_wasm_conversions!(Vec<$rust_elem>, $wasm_name);
    };
    // needs_into = false, is_copy = false: identity boundary for a non-Copy element (String, ...).
    ($rust_elem:ty, $wasm_elem:ty, $wasm_name:ident, false, false) => {
        const _: () = {
            // needs_into = false must mean the rust and wasm element types are the SAME type.
            #[allow(dead_code)]
            fn identity(x: $rust_elem) -> $wasm_elem {
                x
            }
        };

        #[derive(Clone, Debug)]
        #[wasm_bindgen::prelude::wasm_bindgen]
        pub struct $wasm_name(pub(crate) Vec<$rust_elem>);

        #[wasm_bindgen::prelude::wasm_bindgen]
        impl $wasm_name {
            pub fn new() -> Self {
                Self(Vec::new())
            }

            pub fn len(&self) -> usize {
                self.0.len()
            }

            pub fn get(&self, index: usize) -> $wasm_elem {
                self.0[index].clone()
            }

            pub fn add(&mut self, elem: $wasm_elem) {
                self.0.push(elem);
            }
        }

        $crate::impl_wasm_conversions!(Vec<$rust_elem>, $wasm_name);
    };
}

/// Expands to the CBOR (and, with `--json-serde-derives`, JSON) API `--wasm-cbor-json-api-macro`
/// would otherwise emit inline on each wasm wrapper. Invocation shape (emitted by the wasm CBOR/JSON
/// API codegen): `cbor_json_api!(WasmName);` — a single `:ident`, so a wrong arity fails to expand.
///
/// The bodies mirror the inline emission verbatim (newtype `self.0` / `.map(Self)`), so if that
/// contract diverges — the wrapper stops being a newtype over `cddl_lib::T`, or the trait paths
/// change — this fails to compile rather than silently blessing a broken invocation. The fixture is
/// generated with the default lib name, so `cddl_lib` is the rust crate here.
#[macro_export]
macro_rules! cbor_json_api {
    ($wasm_name:ident) => {
        #[wasm_bindgen::prelude::wasm_bindgen]
        impl $wasm_name {
            pub fn to_cbor_bytes(&self) -> Vec<u8> {
                cddl_lib::serialization::ToCBORBytes::to_cbor_bytes(&self.0)
            }

            pub fn from_cbor_bytes(cbor_bytes: &[u8]) -> Result<$wasm_name, wasm_bindgen::JsError> {
                cddl_lib::serialization::Deserialize::from_cbor_bytes(cbor_bytes)
                    .map(Self)
                    .map_err(|e| wasm_bindgen::JsError::new(&format!("from_bytes: {}", e)))
            }
        }
    };
}

/// Expands to the `From`/`AsRef` conversion impls the generator would otherwise emit inline.
/// Invocation shape (emitted by `add_conversion_methods`):
///
/// ```text
/// impl_wasm_conversions!(rust_type, WasmName);
/// ```
///
/// The wasm side is a bare `:ident` (a crate-qualified path there means the arguments were
/// swapped), and the bodies build/unwrap the newtype (`Self(native)` / `wasm.0`), which only
/// compiles when the wasm side really is a newtype over the rust side.
#[macro_export]
macro_rules! impl_wasm_conversions {
    ($rust:ty, $wasm:ident) => {
        impl From<$rust> for $wasm {
            fn from(native: $rust) -> Self {
                Self(native)
            }
        }

        impl From<$wasm> for $rust {
            fn from(wasm: $wasm) -> Self {
                wasm.0
            }
        }

        impl AsRef<$rust> for $wasm {
            fn as_ref(&self) -> &$rust {
                &self.0
            }
        }
    };
}
