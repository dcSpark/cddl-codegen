pub (crate) mod cli;
pub (crate) mod generation;
pub (crate) mod intermediate;
pub (crate) mod parsing;
pub (crate) mod utils;
#[cfg(test)]
mod test;

use generation::GenerationScope;
use intermediate::{
    CDDLIdent,
    IntermediateTypes,
    RustIdent,
};
use parsing::{parse_rule};

use cli::CLI_ARGS;

fn concat_files(paths: Vec<&str>) -> std::io::Result<String> {
    let mut buf = String::new();
    for path in paths {
        buf.push_str(&std::fs::read_to_string(path).map_err(|_| panic!("can't read: {}", path)).unwrap());
    }
    Ok(buf)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cddl_in = std::fs::read_to_string(CLI_ARGS.input.clone()).expect("input.cddl file not present or could not be opened");
    let mut lexer = cddl::lexer::lexer_from_str(&cddl_in);
    let cddl = cddl::parser::cddl_from_str(&mut lexer, &cddl_in, true)?;
    // println!("{:#?}", cddl);
    let mut types = IntermediateTypes::new();
    let mut gen_scope = GenerationScope::new();
    // TODO: this is a quick hack to get around out-of-order declarations in the cddl file
    // e.g. foo = [bar], bar = ... where bar can't deserialize would still generate foo's deserialize
    // if we just do everything independently and it's our of order, so as a super quick fix just do multiple
    // passes until the number of unsupported structs doesn't change.
    // We should probably construct some dependency graph instead of doing this.
    let mut not_deserialized = None;
    let mut pass_count = 0;
    // Need to know beforehand which are plain groups so we can serialize them properly
    // ie x = (3, 4), y = [1, x, 2] would be [1, 3, 4, 2] instead of [1, [3, 4], 2]
    for cddl_rule in &cddl.rules {
        if let cddl::ast::Rule::Group{ rule, .. } = cddl_rule {
            // Freely defined group - no need to generate anything outside of group module
            match &rule.entry {
                cddl::ast::GroupEntry::InlineGroup{ group, .. } => {
                    types.mark_plain_group(RustIdent::new(CDDLIdent::new(rule.name.to_string())), Some(group.clone()));
                },
                x => panic!("Group rule with non-inline group? {:?}", x),
            }
        }
    }
    // TODO: handle out-of-order definition properly instead of just mandating at least two passes.
    // If we separate codegen and parsing this would probably be a lot easier too.
    // This is probably hiding some other issues too.
    while pass_count < 2 || gen_scope.reset_except_not_deserialized(&mut not_deserialized) {
        println!("Pass #{}", pass_count);
        pass_count += 1;
        // Can't generate groups of imports with codegen::Import so we just output this as raw text
        // since we don't need it to be dynamic so it's fine. codegen::Impl::new("a", "{z::b, z::c}")
        // does not work.

        // Rust
        gen_scope.rust().raw("// This library was code-generated using an experimental CDDL to rust tool:\n// https://github.com/Emurgo/cddl-codegen");
        if CLI_ARGS.preserve_encodings && CLI_ARGS.canonical_form {
            gen_scope.rust().raw("use cbor_event::{self, de::Deserializer, se::Serializer};");
        } else {
            gen_scope.rust().raw("use cbor_event::{self, de::Deserializer, se::{Serialize, Serializer}};");
        }
        gen_scope.rust().import("std::io", "{BufRead, Seek, Write}");
        gen_scope.rust().import("prelude", "*");
        gen_scope
            .rust()
            .raw("use cbor_event::Type as CBORType;")
            .raw("use cbor_event::Special as CBORSpecial;")
            .raw("use serialization::*;")
            .raw("pub mod prelude;")
            .raw("pub mod serialization;")
            .raw("use std::collections::BTreeMap;")
            .raw("use std::convert::{From, TryFrom};");
        if CLI_ARGS.preserve_encodings {
            gen_scope.rust().raw("use linked_hash_map::LinkedHashMap;");
        }
        gen_scope.rust_serialize().import("super", "*");
        gen_scope.rust_serialize().import("std::io", "{Seek, SeekFrom}");

        // Wasm
        if CLI_ARGS.wasm {
            gen_scope.wasm().import("wasm_bindgen::prelude", "*");
            gen_scope
                .wasm()
                .raw("mod prelude;")
                .raw("use std::collections::BTreeMap;");
            if CLI_ARGS.preserve_encodings {
                gen_scope.wasm().raw("use linked_hash_map::LinkedHashMap;");
            }
        }
        for cddl_rule in &cddl.rules {
            println!("\n\n------------------------------------------\n- Handling rule: {}\n------------------------------------", cddl_rule.name());
            parse_rule(&mut types, cddl_rule);
        }
        types.finalize();
        gen_scope.generate(&types);
    }
    match std::fs::remove_dir_all("export/src") {
        Ok(()) => (),
        Err(_) => (),
    };
    std::fs::create_dir_all(CLI_ARGS.output.join("core/src"))?;
    std::fs::write(CLI_ARGS.output.join("core/src/lib.rs"), gen_scope.rust().to_string())?;
    let mut serialize_paths = vec!["static/serialization.rs"];
    if CLI_ARGS.preserve_encodings {
        serialize_paths.push("static/serialization_preserve.rs");
        if CLI_ARGS.canonical_form {
            serialize_paths.push("static/serialization_preserve_force_canonical.rs");
        } else {
            serialize_paths.push("static/serialization_preserve_non_force_canonical.rs");
            serialize_paths.push("static/serialization_non_force_canonical.rs");
        }
    } else {
        serialize_paths.push("static/serialization_non_preserve.rs");
        serialize_paths.push("static/serialization_non_force_canonical.rs");
    }
    let mut serialize_contents = concat_files(serialize_paths)?;
    serialize_contents.push_str(&gen_scope.rust_serialize().to_string());
    std::fs::write(CLI_ARGS.output.join("core/src/serialization.rs"), serialize_contents)?;
    std::fs::copy("static/Cargo_rust.toml", CLI_ARGS.output.join("core/Cargo.toml"))?;
    std::fs::copy("static/prelude.rs", CLI_ARGS.output.join("core/src/prelude.rs"))?;
    if CLI_ARGS.wasm {
        std::fs::create_dir_all(CLI_ARGS.output.join("wasm/src"))?;
        std::fs::write(CLI_ARGS.output.join("wasm/src/lib.rs"), gen_scope.wasm().to_string())?;
        std::fs::copy("static/prelude_wasm.rs", CLI_ARGS.output.join("wasm/src/prelude.rs"))?;
        std::fs::copy("static/Cargo_wasm.toml", CLI_ARGS.output.join("wasm/Cargo.toml"))?;
    }

    types.print_info();

    gen_scope.print_structs_without_deserialize();

    Ok(())
}
