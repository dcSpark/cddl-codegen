// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:unpreserved-comment (delete this block after review)
compile_error!("trapped user comment: // rip");
pub struct Bar {
    pub b: u64,
}

impl Foo {
    fn go(&self) {
        // cddl-codegen:insert-start
        pre();
        // cddl-codegen:insert-end
        // cddl-codegen:replace-start
        let n = custom_len(&self.items);
        writer.write(n)?;
        // cddl-codegen:replaces
        //   old();
        // cddl-codegen:replace-end
        // cddl-codegen:replace-start
        custom();
        // cddl-codegen:replaces
        //   let n = self.items.len() as u64;
        //   writer.write_array(n)?;
        // cddl-codegen:replace-end
        // cddl-codegen:keep
        // TAIL NOTE
        tail();
    }
}
// cddl-codegen:insert-start
fn helper() {}
// cddl-codegen:insert-end
// cddl-codegen:keep
// end note
