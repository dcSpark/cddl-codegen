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
