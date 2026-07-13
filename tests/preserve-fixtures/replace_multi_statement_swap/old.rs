// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

impl Foo {
    fn go(&self) {
        // cddl-codegen:replace-start
        let n = custom_len(&self.items);
        writer.write(n)?;
        // cddl-codegen:replaces
        //   let n = self.items.len() as u64;
        //   writer.write_array(n)?;
        // cddl-codegen:replace-end
        Ok(())
    }
}
