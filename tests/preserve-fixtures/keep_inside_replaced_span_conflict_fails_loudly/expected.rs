// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("cddl-codegen could not preserve a user comment across regeneration.\nIts anchor lies inside code replaced by a `// cddl-codegen:replace-start` block; move it into that block.\nOriginal comment:\n        // cddl-codegen:keep\n        // MOVE ME");
impl Foo {
    fn go(&self) {
        // cddl-codegen:replace-start
        new_ab();
        // cddl-codegen:replaces
        //   a();
        //   b();
        // cddl-codegen:replace-end
         c();
    }
}
