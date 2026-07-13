// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // exterior generator note (survives)
        // cddl-codegen:replace-start
        let a = compute_custom();
        let b = a * 2;
        // cddl-codegen:replaces
        //   let a = compute();
        //   let b = a + 1;
        // cddl-codegen:replace-end
        finish(b);
    }
}
