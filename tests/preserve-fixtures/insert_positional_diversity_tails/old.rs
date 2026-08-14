struct PositionalDiversityInsertStructTail {
    generated_struct_field_tail_payload: u8,
}

enum PositionalDiversityInsertEnumTail {
    GeneratedEnumVariantTail,
    // cddl-codegen:insert-start
    PositionalDiversityInsertEnumVariantTail,
    // cddl-codegen:insert-end
}

struct PositionalDiversityInsertImplTail;

fn positional_diversity_insert_block_statement_tail() {
    {
        let generated_block_statement_tail_payload = 0;
        let _ = generated_block_statement_tail_payload;
    }
    // cddl-codegen:insert-start
    let positional_diversity_insert_block_statement_tail_payload = 7;
    let _ = positional_diversity_insert_block_statement_tail_payload;
    // cddl-codegen:insert-end
}

fn positional_diversity_insert_if_else_chain_tail() {
    if true {
        let generated_if_else_tail_payload = 0;
        let _ = generated_if_else_tail_payload;
    } else {
        let generated_if_else_tail_payload = 1;
        let _ = generated_if_else_tail_payload;
    }
    // cddl-codegen:insert-start
    let positional_diversity_insert_if_else_chain_tail_payload = 8;
    let _ = positional_diversity_insert_if_else_chain_tail_payload;
    // cddl-codegen:insert-end
}

fn positional_diversity_insert_struct_literal_last_field_tail() {
    let _ = PositionalDiversityInsertStructTail {
        generated_struct_field_tail_payload: 0,
        // cddl-codegen:insert-start
        positional_diversity_insert_struct_literal_last_field_tail_payload: 9,
        // cddl-codegen:insert-end
    };
}

mod positional_diversity_insert_outer_module_tail {
    mod generated_nested_module_tail {
        pub fn generated_nested_module_tail_payload() {}
    }
    // cddl-codegen:insert-start
    pub fn positional_diversity_insert_nested_module_closing_brace_tail_payload() {}
    // cddl-codegen:insert-end
}

mod positional_diversity_insert_containing_module_for_impl_tail {
    use super::PositionalDiversityInsertImplTail;

    impl PositionalDiversityInsertImplTail {
        fn generated_impl_tail_payload(&self) {}
    }
    // cddl-codegen:insert-start
    impl PositionalDiversityInsertImplTail {
        fn positional_diversity_insert_impl_closing_brace_tail_payload(&self) {}
    }
    // cddl-codegen:insert-end
}
