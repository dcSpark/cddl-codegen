pub (crate) mod cli;
pub (crate) mod generation;
pub (crate) mod intermediate;
pub (crate) mod parsing;
pub (crate) mod utils;
pub (crate) mod rust_reserved;
pub (crate) mod comment_ast;

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
    let cddl = cddl::parser::cddl_from_str(&cddl_in, true)?;
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
            gen_scope
                .rust()
                .raw("pub mod ordered_hash_map;")
                .raw("use ordered_hash_map::OrderedHashMap;")
                .raw("use cbor_event::{Sz, LenSz, StringLenSz};")
                .raw("pub mod cbor_encodings;")
                .raw("use cbor_encodings::*;")
                .raw("extern crate derivative;")
                .raw("use derivative::Derivative;");
            gen_scope.cbor_encodings().raw("use super::*;");
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
                gen_scope
                    .wasm()
                    .raw("use core::ordered_hash_map::OrderedHashMap;")
                    .raw("use core::serialization::{LenEncoding, StringEncoding};");
            }
        }
        for cddl_rule in &cddl.rules {
            println!("\n\n------------------------------------------\n- Handling rule: {}\n------------------------------------", cddl_rule.name());
            parse_rule(&mut types, cddl_rule);
        }
        types.finalize();
        println!("\n-----------------------------------------\n- Generating code...\n------------------------------------");
        gen_scope.generate(&types);
    }

    // package.json / scripts
    let rust_dir = if CLI_ARGS.package_json {
        if CLI_ARGS.json_schema_export {
            std::fs::create_dir_all(CLI_ARGS.output.join("scripts"))?;
            std::fs::copy("static/run-json2ts.js", CLI_ARGS.output.join("scripts/run-json2ts.js"))?;
            std::fs::copy("static/json-ts-types.js", CLI_ARGS.output.join("scripts/json-ts-types.js"))?;
            std::fs::copy("static/package_json_schemas.json", CLI_ARGS.output.join("package.json"))?;
        } else {
            std::fs::copy("static/package.json", CLI_ARGS.output.join("package.json"))?;
        }
        CLI_ARGS.output.join("rust")
    } else {
        CLI_ARGS.output.clone()
    };

    // lib.rs
    std::fs::create_dir_all(rust_dir.join("core/src"))?;
    std::fs::write(rust_dir.join("core/src/lib.rs"), gen_scope.rust().to_string())?;
    // serialiation.rs
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
    std::fs::write(rust_dir.join("core/src/serialization.rs"), serialize_contents)?;
    // Cargo.toml
    let mut rust_cargo_toml = std::fs::read_to_string("static/Cargo_rust.toml")?;
    if CLI_ARGS.preserve_encodings {
        rust_cargo_toml.push_str("linked-hash-map = \"0.5.3\"\n");
        rust_cargo_toml.push_str("derivative = \"2.2.0\"\n");
    }
    if CLI_ARGS.json_serde_derives {
        rust_cargo_toml.push_str("serde = { version = \"1.0\", features = [\"derive\"] }\n");
        rust_cargo_toml.push_str("serde_json = \"1.0.57\"\n");
    }
    if CLI_ARGS.json_schema_export {
        rust_cargo_toml.push_str("schemars = \"0.8.8\"\n");
    }
    std::fs::write(rust_dir.join("core/Cargo.toml"), rust_cargo_toml)?;
    // prelude.rs
    std::fs::copy("static/prelude.rs", rust_dir.join("core/src/prelude.rs"))?;
    // cbor_encodings.rs + ordered_hash_map.rs
    if CLI_ARGS.preserve_encodings {
        std::fs::write(rust_dir.join("core/src/cbor_encodings.rs"), gen_scope.cbor_encodings().to_string())?;
        let mut ordered_hash_map_rs = std::fs::read_to_string("static/ordered_hash_map.rs")?;
        if CLI_ARGS.json_serde_derives {
            ordered_hash_map_rs.push_str(&std::fs::read_to_string("static/ordered_hash_map_json.rs")?);
        }
        if CLI_ARGS.json_schema_export {
            ordered_hash_map_rs.push_str(&std::fs::read_to_string("static/ordered_hash_map_schemars.rs")?);
        }
        std::fs::write(rust_dir.join("core/src/ordered_hash_map.rs"), ordered_hash_map_rs)?;
    }

    // wasm crate
    if CLI_ARGS.wasm {
        std::fs::create_dir_all(rust_dir.join("wasm/src"))?;
        std::fs::write(rust_dir.join("wasm/src/lib.rs"), gen_scope.wasm().to_string())?;
        let mut wasm_toml = std::fs::read_to_string("static/Cargo_wasm.toml")?;
        if CLI_ARGS.json_serde_derives {
            wasm_toml.push_str("serde_json = \"1.0.57\"\n");
        }
        std::fs::write(rust_dir.join("wasm/Cargo.toml"), wasm_toml)?;
        std::fs::copy("static/prelude_wasm.rs", rust_dir.join("wasm/src/prelude.rs"))?;
    }

    // json-gen crate for exporting JSON schemas
    if CLI_ARGS.json_schema_export {
        std::fs::create_dir_all(rust_dir.join("json-gen/src"))?;
        std::fs::copy("static/Cargo_json_gen.toml", rust_dir.join("json-gen/Cargo.toml"))?;
        std::fs::write(rust_dir.join("json-gen/src/main.rs"), gen_scope.json().to_string())?;
    }

    types.print_info();

    gen_scope.print_structs_without_deserialize();

    Ok(())
}
