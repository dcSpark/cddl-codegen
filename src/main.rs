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
use parsing::{parse_rule, rule_is_scope_marker};

use cli::CLI_ARGS;

fn cddl_paths(output: &mut Vec<std::path::PathBuf>, cd: &std::path::PathBuf) -> std::io::Result<()> {
    for dir_entry in std::fs::read_dir(cd)? {
        let path = dir_entry?.path();
        if path.is_dir() {
            cddl_paths(output, &path)?;
        } else if path.as_path().extension().unwrap() == "cddl" {
            output.push(path);
        } else {
            println!("Skipping file: {}", path.as_path().to_str().unwrap());
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input_files = if CLI_ARGS.input.is_dir() {
        let mut cddl_paths_buf = Vec::new();
        cddl_paths(&mut cddl_paths_buf, &CLI_ARGS.input)?;
        cddl_paths_buf
    } else {
        vec![CLI_ARGS.input.clone()]
    };

    // To get around an issue with cddl where you can't parse a partial cddl fragment
    // we must group all files together. To mark scope we insert string constants with
    // a specific, unlikely to ever be used, prefix. The names contain a number after
    // to avoid a parsing error (rule with same identifier already defined).
    // This approeach was chosen over comments as those were finicky when not attached
    // to specific structs, and the existing comment parsing ast was not suited for this.
    // If, in the future, cddl released a feature flag to allow partial cddl we can just
    // remove all this and revert back the commit before this one for scope handling.
    let input_files_content = input_files
        .iter()
        .enumerate()
        .map(|(i, input_file)| {
            let scope = if input_files.len() > 1 {
                input_file.file_stem().unwrap().to_str().unwrap()
            }  else {
                "lib"
            };
            std::fs::read_to_string(input_file)
                .map(|raw| format!("\n_CDDL_CODEGEN_SCOPE_MARKER_{}_ = \"{}\"\n{}\n", i, scope, raw))
        }).collect::<Result<String, _>>()?;
    let cddl = cddl::parser::cddl_from_str(&input_files_content, true)?;
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
    for cddl_rule in cddl.rules.iter() {
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
    let mut scope = "lib".to_owned();
    // TODO: handle out-of-order definition properly instead of just mandating at least two passes.
    // If we separate codegen and parsing this would probably be a lot easier too.
    // This is probably hiding some other issues too.
    while pass_count < 2 || gen_scope.reset_except_not_deserialized(&mut not_deserialized) {
        println!("Pass #{}", pass_count);
        pass_count += 1;
        // Can't generate groups of imports with codegen::Import so we just output this as raw text
        // since we don't need it to be dynamic so it's fine. codegen::Impl::new("a", "{z::b, z::c}")
        // does not work.

        for cddl_rule in cddl.rules.iter() {
            // We inserted string constants with specific prefixes earlier to mark scope
            if let Some(new_scope) = rule_is_scope_marker(cddl_rule) {
                println!("Switching from scope '{}' to '{}'", scope, new_scope);
                scope = new_scope;
            } else {
                println!("\n\n------------------------------------------\n- Handling rule: {}\n------------------------------------", cddl_rule.name());
                parse_rule(&mut types, cddl_rule, scope.clone());
            }
        }
        types.finalize();
        println!("\n-----------------------------------------\n- Generating code...\n------------------------------------");
        gen_scope.generate(&types);
    }

    gen_scope.export()?;

    types.print_info();

    gen_scope.print_structs_without_deserialize();

    Ok(())
}
