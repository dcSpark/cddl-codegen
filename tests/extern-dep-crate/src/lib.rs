use std::collections::BTreeMap;
use wasm_bindgen::prelude::{wasm_bindgen, JsError};
use cbor_encodings::ExternCrateFooEncoding;

// `alloc` is what the SHIPPED runtime files below spell (the generated crate is no_std-capable),
// so the two `static/` copies are carried VERBATIM rather than re-pathed to `std::` — one less
// hand transformation to keep in sync when the runtime changes.
extern crate alloc;

pub mod cbor_encodings;
pub mod error;
pub mod non_empty;
// The restricted-map and reject-set runtime twins, carried for the same reason `non_empty` is:
// a consumer pointing `--common-import-override` at a dependency resolves EVERY runtime type
// through it, so a dependency that hosts wrappers over `{+ k => v}` / `@duplicates reject`
// shapes must publish these modules exactly as a generated dependency crate would.
pub mod non_empty_map;
pub mod ordered_hash_map;
pub mod ordered_set;
pub mod serialization;
pub mod sub;

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct ExternCrateFoo {
    index_0: u64,
    index_1: String,
    index_2: Vec<u8>,
    encodings: Option<ExternCrateFooEncoding>,
}

// Dep-OWNED named collection rules (`dep_withdrawals = { * uint => extern_crate_foo } ; @rust_name
// DepWithdrawals`, `dep_certs = [* extern_crate_foo] ; @rust_name DepCerts`), declared under the
// consumer's `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/`. A consumer referencing them BY NAME emits code that
// treats each as its structural shape — its `serialize`/`deserialize` build a `BTreeMap`/`Vec` and
// assign directly into the field typed `DepWithdrawals`/`DepCerts`, and `.len()`/`.iter()` are called
// on the field. So the dep-owned face of these rules is exactly a transparent alias to the collection
// (the `dep_owned_named_collection_no_local_structural_import` cross-crate compile pins that the
// consumer must NOT ALSO mint a local `MapU64ToExternCrateFoo`/`ExternCrateFooList` structural
// wrapper — those are dep-owned and defined only here).
pub type DepWithdrawals = BTreeMap<u64, ExternCrateFoo>;
pub type DepCerts = Vec<ExternCrateFoo>;

#[wasm_bindgen]
impl ExternCrateFoo {
    pub fn new(index_0: u64, index_1: String, index_2: Vec<u8>) -> Self {
        Self {
            index_0,
            index_1,
            index_2,
            encodings: None,
        }
    }

    pub fn to_cbor_bytes(&self) -> Vec<u8> {
        serialization::ToCBORBytes::to_cbor_bytes(&self)
    }

    pub fn from_cbor_bytes(cbor_bytes: &[u8]) -> Result<ExternCrateFoo, JsError> {
        serialization::Deserialize::from_cbor_bytes(cbor_bytes)
            .map_err(|e| JsError::new(&format!("from_bytes: {}", e)))
    }

    pub fn index_0(&self) -> u64 {
        self.index_0
    }

    pub fn index_1(&self) -> String {
        self.index_1.clone()
    }

    pub fn index_2(&self) -> Vec<u8> {
        self.index_2.clone()
    }
}

// `Int` is common scaffolding a `--common-import-override` consumer RE-EXPORTS rather than mints (see
// the generator's `generate_int` override path): a spec referencing `int` emits
// `pub use extern_dep_crate::{Int, IntError};` (rust) and `pub use extern_dep_crate::Int;` (wasm)
// instead of a crate-local copy, so both crates share ONE `Int` identity. This fixture is single-crate
// (rust + wasm in one, like `ExternCrateFoo` above), so its `Int` is a single `#[wasm_bindgen]` type
// serving both faces. The inner representation mirrors what cddl-codegen emits for a preserve-encodings
// `Int` enum.
#[derive(Clone, Debug)]
pub(crate) enum IntEnum {
    Uint {
        value: u64,
        encoding: Option<cbor_event::Sz>,
    },
    Nint {
        value: u64,
        encoding: Option<cbor_event::Sz>,
    },
}

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct Int(pub(crate) IntEnum);

#[wasm_bindgen]
impl Int {
    pub fn new(x: i64) -> Self {
        if x >= 0 {
            Self::new_uint(x as u64)
        } else {
            Self::new_nint((x + 1).unsigned_abs())
        }
    }

    pub fn new_uint(value: u64) -> Self {
        Self(IntEnum::Uint {
            value,
            encoding: None,
        })
    }

    /// * `value` - Value as encoded in CBOR - note: a negative `x` here would be `|x + 1|` due to CBOR's `nint` encoding e.g. to represent -5, pass in 4.
    pub fn new_nint(value: u64) -> Self {
        Self(IntEnum::Nint {
            value,
            encoding: None,
        })
    }

    pub fn to_str(&self) -> String {
        self.to_string()
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(string: &str) -> Result<Int, JsError> {
        std::str::FromStr::from_str(string)
            .map_err(|e| JsError::new(&format!("Int.from_str({}): {:?}", string, e)))
    }
}

impl std::fmt::Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            IntEnum::Uint { value, .. } => write!(f, "{}", value),
            IntEnum::Nint { value, .. } => write!(f, "{}", -((*value as i128) + 1)),
        }
    }
}

impl std::str::FromStr for Int {
    type Err = IntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let x = i128::from_str(s).map_err(IntError::Parsing)?;
        Self::try_from(x).map_err(IntError::Bounds)
    }
}

impl TryFrom<i128> for Int {
    type Error = std::num::TryFromIntError;

    fn try_from(x: i128) -> Result<Self, Self::Error> {
        if x >= 0 {
            u64::try_from(x).map(Self::new_uint)
        } else {
            u64::try_from((x + 1).unsigned_abs()).map(Self::new_nint)
        }
    }
}

#[derive(Clone, Debug)]
pub enum IntError {
    Bounds(std::num::TryFromIntError),
    Parsing(std::num::ParseIntError),
}

// Key traits so a `--common-import-override` consumer keying a map on `int` (whose
// borrowed_key_types.rs self-check is `_assert_key_traits::<extern_dep_crate::Int>()`) compiles.
// Semantics MATCH cddl-codegen's key-flavored `Int`: it derives Eq/Ord/PartialOrd (+Hash under
// --preserve-encodings) via the derivative machinery comparing VARIANT + VALUE only, ignoring the
// `encoding` fields (two Ints equal iff the same signed integer under any CBOR encoding). Hand-impl
// the same over `IntEnum::cmp_key()` rather than pulling `derivative` into the fixture.
impl IntEnum {
    fn cmp_key(&self) -> (u8, u64) {
        match self {
            IntEnum::Uint { value, .. } => (0, *value),
            IntEnum::Nint { value, .. } => (1, *value),
        }
    }
}

impl PartialEq for Int {
    fn eq(&self, other: &Self) -> bool {
        self.0.cmp_key() == other.0.cmp_key()
    }
}

impl Eq for Int {}

impl Ord for Int {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp_key().cmp(&other.0.cmp_key())
    }
}

impl PartialOrd for Int {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::hash::Hash for Int {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.cmp_key().hash(state);
    }
}
