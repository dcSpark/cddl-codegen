// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:replace-start
impl Foo {
    fn go(&self) -> u64 {
        custom()
    }
}
// cddl-codegen:replaces
//   impl Foo {
//       fn go(&self) -> u64 {
//           self.a
//       }
//   }
// cddl-codegen:replace-end

pub struct Bar {
    pub b: u64,
}
