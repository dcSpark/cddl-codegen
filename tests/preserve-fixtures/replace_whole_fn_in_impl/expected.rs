// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    // cddl-codegen:replace-start
    fn go(&self) -> u64 {
        custom_result()
    }
    // cddl-codegen:replaces
    //   fn go(&self) -> u64 {
    //       self.a + self.b
    //   }
    // cddl-codegen:replace-end

    fn other(&self) {
        noop();
    }
}
