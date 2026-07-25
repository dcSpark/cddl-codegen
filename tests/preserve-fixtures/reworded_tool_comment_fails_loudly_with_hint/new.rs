// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl OrderedSet {
    fn insert(&mut self, v: u64) {
        // careful: the insert order is the serialization order
        self.inner.push(v);
    }
}
