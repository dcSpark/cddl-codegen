// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user code block across regeneration.\nIts recorded original is not unique within `impl Foo` (a deleted duplicate?), so which occurrence it replaces is ambiguous.\nOriginal code block:\n        // cddl-codegen:replace-start\n        custom(field);\n        // cddl-codegen:replaces\n        //   emit(field);\n        // cddl-codegen:replace-end");
impl Foo {
    fn go(&self) {
        log("start");
        emit(field);
        emit(field);
        emit(field);
    }
}
