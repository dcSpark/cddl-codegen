pub (crate) mod cmd;
pub (crate) mod generation;
pub (crate) mod intermediate;
pub (crate) mod parsing;
pub (crate) mod utils;

use generation::GenerationScope;
use intermediate::{
    CDDLIdent,
    IntermediateTypes,
    RustIdent,
};
use parsing::{parse_type, parse_type_choices};


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cddl_in = std::fs::read_to_string("test.cddl").expect("input.cddl file not present or could not be opened");
    let cddl = cddl::parser::cddl_from_str(&cddl_in)?;
    //println!("CDDL file: {}", cddl);
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
        gen_scope.scope().raw("// This library was code-generated using an experimental CDDL to rust tool:\n// https://github.com/Emurgo/cddl-codegen");
        gen_scope.scope().raw("use cbor_event::{self, de::Deserializer, se::{Serialize, Serializer}};");
        gen_scope.scope().import("std::io", "{BufRead, Seek, Write}");
        gen_scope.scope().import("wasm_bindgen::prelude", "*");
        gen_scope.scope().import("prelude", "*");
        gen_scope.scope().raw("use cbor_event::Type as CBORType;");
        gen_scope.scope().raw("use cbor_event::Special as CBORSpecial;");
        gen_scope.scope().raw("mod prelude;");
        gen_scope.scope().raw("mod serialization;");
        gen_scope.serialize_scope().import("super", "*");
        gen_scope.serialize_scope().import("std::io", "{Seek, SeekFrom}");
        for cddl_rule in &cddl.rules {
            println!("\n\n------------------------------------------\n- Handling rule: {}\n------------------------------------", cddl_rule.name());
            match cddl_rule {
                cddl::ast::Rule::Type{ rule, .. } => {
                    // (1) does not handle optional generic parameters
                    // (2) is_type_choice_alternate ignored since shelley.cddl doesn't need it
                    //     It's used, but used for no reason as it is the initial definition
                    //     (which is also valid cddl), but it would be fine as = instead of /=
                    // (3) ignores control operators - only used in shelley spec to limit string length for application metadata
                    let rust_ident = RustIdent::new(CDDLIdent::new(rule.name.to_string()));
                    if rule.value.type_choices.len() == 1 {
                        let choice = &rule.value.type_choices.first().unwrap();
                        parse_type(&mut types, &rust_ident, &choice.type2, None);
                    } else {
                        parse_type_choices(&mut types, &rust_ident, &rule.value.type_choices, None);
                    }
                },
                cddl::ast::Rule::Group{ rule, .. } => {
                    // Freely defined group - no need to generate anything outside of group module
                    match &rule.entry {
                        cddl::ast::GroupEntry::InlineGroup{ .. } => (),// already handled above
                        x => panic!("Group rule with non-inline group? {:?}", x),
                    }
                },
            }
        }
        gen_scope.generate(&types);
    }
    match std::fs::remove_dir_all("export/src") {
        Ok(()) => (),
        Err(_) => (),
    };
    std::fs::create_dir_all("export/src").unwrap();
    std::fs::write("export/src/lib.rs", gen_scope.scope().to_string()).unwrap();
    std::fs::write("export/src/serialization.rs", gen_scope.serialize_scope().to_string()).unwrap();
    std::fs::copy("static/Cargo.toml", "export/Cargo.toml").unwrap();
    std::fs::copy("static/prelude.rs", "export/src/prelude.rs").unwrap();

    types.print_info();

    gen_scope.print_structs_without_deserialize();

    Ok(())
}
