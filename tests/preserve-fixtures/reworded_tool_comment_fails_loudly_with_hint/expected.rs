// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen found a comment it cannot classify.\nOutside a `cddl-codegen:` block every comment in a generated file is tool-owned, and this one is neither emitted by this run nor marked as yours. It is one of two things:\n(1) stale tool output whose text changed or was removed upstream — delete this whole block;\n(2) your own comment — delete this whole block and re-add the comment with a marker:\n// cddl-codegen:keep <your text>\nor, for a run of comment lines (the only form that can carry `///`/`//!` doc comments), a bare marker directly above the run:\n// cddl-codegen:keep\n/// <your text>\nThis run emits a comment at the same position, so this is most likely a tool comment whose text changed upstream — compare against: // careful: the insert order is the serialization order\nOriginal comment:\n// ponytail: the insert order is the serialization order");
impl OrderedSet {
    fn insert(&mut self, v: u64) {
        // careful: the insert order is the serialization order
        self.inner.push(v);
    }
}
