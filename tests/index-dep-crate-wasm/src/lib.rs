// The wasm-bindgen wrapper crate for `index-dep-crate` (the `<dep>-wasm` half of the split layout
// `--extern-wasm-crate`/`--extern-wrapper-index` target). It holds the SOLE `#[wasm_bindgen] IdxFoo`
// (the rust crate is wasm-clean, so there is no duplicate `__wbg_idxfoo_free`) plus the collection
// wrappers a consumer's `[* idx_foo]` / `{* … => idx_foo}` shapes would otherwise re-mint. The
// wrapper bodies are byte-for-byte the shape cddl-codegen emits (new/len/get/add for the list,
// new/len/insert/get/keys for the map) with the `From<Vec<..>>`/`From<OrderedHashMap<..>>` +
// `AsRef` boundary contract the consumer relies on to convert its own inner `Vec`/`OrderedHashMap`
// of the dep's RUST type into these wrappers with `.clone().into()`. The `collections` module is the
// index a consumer points `--extern-wrapper-index index_dep_crate=<this file>` at.
use index_dep_crate::ordered_hash_map::OrderedHashMap;
use wasm_bindgen::prelude::wasm_bindgen;

pub mod collections;

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct IdxFoo(index_dep_crate::IdxFoo);

impl From<index_dep_crate::IdxFoo> for IdxFoo {
    fn from(native: index_dep_crate::IdxFoo) -> Self {
        Self(native)
    }
}

impl From<IdxFoo> for index_dep_crate::IdxFoo {
    fn from(wasm: IdxFoo) -> Self {
        wasm.0
    }
}

impl AsRef<index_dep_crate::IdxFoo> for IdxFoo {
    fn as_ref(&self) -> &index_dep_crate::IdxFoo {
        &self.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct IdxFooList(Vec<index_dep_crate::IdxFoo>);

#[wasm_bindgen]
impl IdxFooList {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> IdxFoo {
        self.0[index].clone().into()
    }

    pub fn add(&mut self, elem: &IdxFoo) {
        self.0.push(elem.clone().into());
    }
}

impl From<Vec<index_dep_crate::IdxFoo>> for IdxFooList {
    fn from(native: Vec<index_dep_crate::IdxFoo>) -> Self {
        Self(native)
    }
}

impl From<IdxFooList> for Vec<index_dep_crate::IdxFoo> {
    fn from(wasm: IdxFooList) -> Self {
        wasm.0
    }
}

impl AsRef<Vec<index_dep_crate::IdxFoo>> for IdxFooList {
    fn as_ref(&self) -> &Vec<index_dep_crate::IdxFoo> {
        &self.0
    }
}

// Second element (`idx_bar`) plus its LOOSE list wrapper. The dep index lists IdxBarList but NOT
// any restricted `[+ idx_bar]` wrapper, so a consumer's `[+ idx_bar]` rule/shape mints its
// restricted class locally while deferring THIS loose class as the `try_from` source — the
// conversion must resolve cross-crate through the public From/AsRef contract below.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct IdxBar(index_dep_crate::IdxBar);

impl From<index_dep_crate::IdxBar> for IdxBar {
    fn from(native: index_dep_crate::IdxBar) -> Self {
        Self(native)
    }
}

impl From<IdxBar> for index_dep_crate::IdxBar {
    fn from(wasm: IdxBar) -> Self {
        wasm.0
    }
}

impl AsRef<index_dep_crate::IdxBar> for IdxBar {
    fn as_ref(&self) -> &index_dep_crate::IdxBar {
        &self.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct IdxBarList(Vec<index_dep_crate::IdxBar>);

#[wasm_bindgen]
impl IdxBarList {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> IdxBar {
        self.0[index].clone().into()
    }

    pub fn add(&mut self, elem: &IdxBar) {
        self.0.push(elem.clone().into());
    }
}

impl From<Vec<index_dep_crate::IdxBar>> for IdxBarList {
    fn from(native: Vec<index_dep_crate::IdxBar>) -> Self {
        Self(native)
    }
}

impl From<IdxBarList> for Vec<index_dep_crate::IdxBar> {
    fn from(wasm: IdxBarList) -> Self {
        wasm.0
    }
}

impl AsRef<Vec<index_dep_crate::IdxBar>> for IdxBarList {
    fn as_ref(&self) -> &Vec<index_dep_crate::IdxBar> {
        &self.0
    }
}

// Third element (`idx_baz`) plus its LOOSE list wrapper — the inline `[+ idx_baz]` twin of the
// named-rule IdxBar cell. Indexed: IdxBazList. NOT indexed: NonEmptyIdxBazList.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct IdxBaz(index_dep_crate::IdxBaz);

impl From<index_dep_crate::IdxBaz> for IdxBaz {
    fn from(native: index_dep_crate::IdxBaz) -> Self {
        Self(native)
    }
}

impl From<IdxBaz> for index_dep_crate::IdxBaz {
    fn from(wasm: IdxBaz) -> Self {
        wasm.0
    }
}

impl AsRef<index_dep_crate::IdxBaz> for IdxBaz {
    fn as_ref(&self) -> &index_dep_crate::IdxBaz {
        &self.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct IdxBazList(Vec<index_dep_crate::IdxBaz>);

#[wasm_bindgen]
impl IdxBazList {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> IdxBaz {
        self.0[index].clone().into()
    }

    pub fn add(&mut self, elem: &IdxBaz) {
        self.0.push(elem.clone().into());
    }
}

impl From<Vec<index_dep_crate::IdxBaz>> for IdxBazList {
    fn from(native: Vec<index_dep_crate::IdxBaz>) -> Self {
        Self(native)
    }
}

impl From<IdxBazList> for Vec<index_dep_crate::IdxBaz> {
    fn from(wasm: IdxBazList) -> Self {
        wasm.0
    }
}

impl AsRef<Vec<index_dep_crate::IdxBaz>> for IdxBazList {
    fn as_ref(&self) -> &Vec<index_dep_crate::IdxBaz> {
        &self.0
    }
}

// The RESTRICTED `[+ idx_foo]` wrapper — the NonEmpty twin of IdxFooList, wrapping
// `NonEmptyVec<IdxFoo>`. Hand-written to match cddl-codegen's synthesized `NonEmpty*List` emission
// (new(first)/len/get/add plus the `From<NonEmptyVec<..>>` / `From<.. for NonEmptyVec>` / `AsRef`
// boundary contract a consumer's deferred `[+ idx_foo]` field relies on), and indexed in
// `collections.rs` so a consumer pointing `--extern-wrapper-index` here DEFERS it instead of
// re-minting a colliding `#[wasm_bindgen] NonEmptyIdxFooList`.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct NonEmptyIdxFooList(
    index_dep_crate::non_empty::NonEmptyVec<index_dep_crate::IdxFoo>,
);

#[wasm_bindgen]
impl NonEmptyIdxFooList {
    pub fn new(first: &IdxFoo) -> Self {
        Self(index_dep_crate::non_empty::NonEmptyVec::new(first.clone().into()))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> IdxFoo {
        self.0[index].clone().into()
    }

    pub fn add(&mut self, elem: &IdxFoo) {
        self.0.push(elem.clone().into());
    }
}

impl From<index_dep_crate::non_empty::NonEmptyVec<index_dep_crate::IdxFoo>> for NonEmptyIdxFooList {
    fn from(native: index_dep_crate::non_empty::NonEmptyVec<index_dep_crate::IdxFoo>) -> Self {
        Self(native)
    }
}

impl From<NonEmptyIdxFooList> for index_dep_crate::non_empty::NonEmptyVec<index_dep_crate::IdxFoo> {
    fn from(wasm: NonEmptyIdxFooList) -> Self {
        wasm.0
    }
}

impl AsRef<index_dep_crate::non_empty::NonEmptyVec<index_dep_crate::IdxFoo>> for NonEmptyIdxFooList {
    fn as_ref(&self) -> &index_dep_crate::non_empty::NonEmptyVec<index_dep_crate::IdxFoo> {
        &self.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct MapU64ToIdxFoo(OrderedHashMap<u64, index_dep_crate::IdxFoo>);

#[wasm_bindgen]
impl MapU64ToIdxFoo {
    pub fn new() -> Self {
        Self(OrderedHashMap::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn insert(&mut self, key: u64, value: &IdxFoo) -> Option<IdxFoo> {
        self.0.insert(key, value.clone().into()).map(Into::into)
    }

    pub fn get(&self, key: u64) -> Option<IdxFoo> {
        self.0.get(&key).map(|v| v.clone().into())
    }

    pub fn keys(&self) -> Vec<u64> {
        self.0.keys().copied().collect::<Vec<_>>()
    }
}

impl From<OrderedHashMap<u64, index_dep_crate::IdxFoo>> for MapU64ToIdxFoo {
    fn from(native: OrderedHashMap<u64, index_dep_crate::IdxFoo>) -> Self {
        Self(native)
    }
}

impl From<MapU64ToIdxFoo> for OrderedHashMap<u64, index_dep_crate::IdxFoo> {
    fn from(wasm: MapU64ToIdxFoo) -> Self {
        wasm.0
    }
}

impl AsRef<OrderedHashMap<u64, index_dep_crate::IdxFoo>> for MapU64ToIdxFoo {
    fn as_ref(&self) -> &OrderedHashMap<u64, index_dep_crate::IdxFoo> {
        &self.0
    }
}

// Nested list-of-list wrapper (`[* [* idx_foo]]`) — the structural name a consumer's nested shape
// derives (`ArrIdxFooList`). Its inner is `Vec<Vec<IdxFoo>>`; `get`/`add` speak the INNER loose
// wrapper `IdxFooList` across the boundary. Hand-provided (like the others) so a `--workspace-dep`
// consumer that DEFERS its nested wrapper links against this class instead of re-minting it.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct ArrIdxFooList(Vec<Vec<index_dep_crate::IdxFoo>>);

#[wasm_bindgen]
impl ArrIdxFooList {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> IdxFooList {
        self.0[index].clone().into()
    }

    pub fn add(&mut self, elem: &IdxFooList) {
        self.0.push(elem.clone().into());
    }
}

impl From<Vec<Vec<index_dep_crate::IdxFoo>>> for ArrIdxFooList {
    fn from(native: Vec<Vec<index_dep_crate::IdxFoo>>) -> Self {
        Self(native)
    }
}

impl From<ArrIdxFooList> for Vec<Vec<index_dep_crate::IdxFoo>> {
    fn from(wasm: ArrIdxFooList) -> Self {
        wasm.0
    }
}

impl AsRef<Vec<Vec<index_dep_crate::IdxFoo>>> for ArrIdxFooList {
    fn as_ref(&self) -> &Vec<Vec<index_dep_crate::IdxFoo>> {
        &self.0
    }
}

// An OWNERLESS map wrapper (`{* uint => text}` — no named element types). Indexed below, so a
// consumer passing BOTH `--workspace-dep` and `--extern-wrapper-index` for this dep INDEX-defers it
// (ownerless wrappers are never workspace-borrowed — criterion 2). Its keys are `u64`, value String.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct MapU64ToText(OrderedHashMap<u64, String>);

#[wasm_bindgen]
impl MapU64ToText {
    pub fn new() -> Self {
        Self(OrderedHashMap::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn insert(&mut self, key: u64, value: String) -> Option<String> {
        self.0.insert(key, value)
    }

    pub fn get(&self, key: u64) -> Option<String> {
        self.0.get(&key).cloned()
    }

    pub fn keys(&self) -> Vec<u64> {
        self.0.keys().copied().collect::<Vec<_>>()
    }
}

impl From<OrderedHashMap<u64, String>> for MapU64ToText {
    fn from(native: OrderedHashMap<u64, String>) -> Self {
        Self(native)
    }
}

impl From<MapU64ToText> for OrderedHashMap<u64, String> {
    fn from(wasm: MapU64ToText) -> Self {
        wasm.0
    }
}

impl AsRef<OrderedHashMap<u64, String>> for MapU64ToText {
    fn as_ref(&self) -> &OrderedHashMap<u64, String> {
        &self.0
    }
}

// The byte-backed element (`idx_hash`) and its LOOSE list wrapper, for the
// `@extern_companions`-on-a-raw-bytes-marker arm. Same hand-written shape as `IdxFoo`/`IdxFooList`
// above — the point of the pair is that the consumer's marker KIND is the only thing that differs,
// so the wrapper contract the deferral relies on (`From<Vec<..>>` / `From<.. for Vec>` / `AsRef`,
// `new`/`len`/`get`/`add`) is byte-for-byte the same one.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct IdxHash(index_dep_crate::IdxHash);

impl From<index_dep_crate::IdxHash> for IdxHash {
    fn from(native: index_dep_crate::IdxHash) -> Self {
        Self(native)
    }
}

impl From<IdxHash> for index_dep_crate::IdxHash {
    fn from(wasm: IdxHash) -> Self {
        wasm.0
    }
}

impl AsRef<index_dep_crate::IdxHash> for IdxHash {
    fn as_ref(&self) -> &index_dep_crate::IdxHash {
        &self.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct IdxHashList(Vec<index_dep_crate::IdxHash>);

#[wasm_bindgen]
impl IdxHashList {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> IdxHash {
        self.0[index].clone().into()
    }

    pub fn add(&mut self, elem: &IdxHash) {
        self.0.push(elem.clone().into());
    }
}

impl From<Vec<index_dep_crate::IdxHash>> for IdxHashList {
    fn from(native: Vec<index_dep_crate::IdxHash>) -> Self {
        Self(native)
    }
}

impl From<IdxHashList> for Vec<index_dep_crate::IdxHash> {
    fn from(wasm: IdxHashList) -> Self {
        wasm.0
    }
}

impl AsRef<Vec<index_dep_crate::IdxHash>> for IdxHashList {
    fn as_ref(&self) -> &Vec<index_dep_crate::IdxHash> {
        &self.0
    }
}
