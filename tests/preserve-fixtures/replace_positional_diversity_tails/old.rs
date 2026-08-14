struct PositionalDiversityReplaceStructTail {
    generated_struct_field_tail_payload: u8,
}

enum PositionalDiversityReplaceEnumTail {
    // cddl-codegen:replace-start
    PositionalDiversityReplaceEnumVariantTail,
    // cddl-codegen:replaces
    // GeneratedEnumVariantTail,
    // cddl-codegen:replace-end
}

struct PositionalDiversityReplaceImplTail;

fn positional_diversity_replace_block_statement_tail() {
    // cddl-codegen:replace-start
    {
        let positional_diversity_replace_block_statement_tail_payload = 7;
        let _ = positional_diversity_replace_block_statement_tail_payload;
    }
    // cddl-codegen:replaces
    // {
    //     let generated_block_statement_tail_payload = 0;
    //     let _ = generated_block_statement_tail_payload;
    // }
    // cddl-codegen:replace-end
}

fn positional_diversity_replace_if_else_chain_tail() {
    // cddl-codegen:replace-start
    if true {
        let positional_diversity_replace_if_else_chain_tail_payload = 8;
        let _ = positional_diversity_replace_if_else_chain_tail_payload;
    } else {
        let positional_diversity_replace_if_else_chain_tail_payload = 9;
        let _ = positional_diversity_replace_if_else_chain_tail_payload;
    }
    // cddl-codegen:replaces
    // if true {
    //     let generated_if_else_tail_payload = 0;
    //     let _ = generated_if_else_tail_payload;
    // } else {
    //     let generated_if_else_tail_payload = 1;
    //     let _ = generated_if_else_tail_payload;
    // }
    // cddl-codegen:replace-end
}

fn positional_diversity_replace_struct_literal_last_field_tail() {
    let _ = PositionalDiversityReplaceStructTail {
        // cddl-codegen:replace-start
        positional_diversity_replace_struct_literal_last_field_tail_payload: 10,
        // cddl-codegen:replaces
        // generated_struct_field_tail_payload: 0,
        // cddl-codegen:replace-end
    };
}

mod positional_diversity_replace_outer_module_tail {
    // cddl-codegen:replace-start
    mod positional_diversity_replace_nested_module_closing_brace_tail {
        pub fn positional_diversity_replace_nested_module_closing_brace_tail_payload() {}
    }
    // cddl-codegen:replaces
    // mod generated_nested_module_tail {
    //     pub fn generated_nested_module_tail_payload() {}
    // }
    // cddl-codegen:replace-end
}

mod positional_diversity_replace_containing_module_for_impl_tail {
    use super::PositionalDiversityReplaceImplTail;

    // cddl-codegen:replace-start
    impl PositionalDiversityReplaceImplTail {
        fn positional_diversity_replace_impl_closing_brace_tail_payload(&self) {}
    }
    // cddl-codegen:replaces
    // impl PositionalDiversityReplaceImplTail {
    //     fn generated_impl_tail_payload(&self) {}
    // }
    // cddl-codegen:replace-end
}
