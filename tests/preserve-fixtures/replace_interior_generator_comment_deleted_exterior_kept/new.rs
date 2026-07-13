// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // exterior generator note (survives)
        let a = compute();
        // interior generator note (deleted with the span)
        let b = a + 1;
        finish(b);
    }
}
