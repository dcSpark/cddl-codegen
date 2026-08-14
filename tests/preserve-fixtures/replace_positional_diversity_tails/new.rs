struct PositionalDiversityReplaceStructTail {
    generated_struct_field_tail_payload: u8,
}

enum PositionalDiversityReplaceEnumTail {
    GeneratedEnumVariantTail,
}

struct PositionalDiversityReplaceImplTail;

fn positional_diversity_replace_block_statement_tail() {
    {
        let generated_block_statement_tail_payload = 0;
        let _ = generated_block_statement_tail_payload;
    }
}

fn positional_diversity_replace_if_else_chain_tail() {
    if true {
        let generated_if_else_tail_payload = 0;
        let _ = generated_if_else_tail_payload;
    } else {
        let generated_if_else_tail_payload = 1;
        let _ = generated_if_else_tail_payload;
    }
}

fn positional_diversity_replace_struct_literal_last_field_tail() {
    let _ = PositionalDiversityReplaceStructTail {
        generated_struct_field_tail_payload: 0,
    };
}

mod positional_diversity_replace_outer_module_tail {
    mod generated_nested_module_tail {
        pub fn generated_nested_module_tail_payload() {}
    }
}

mod positional_diversity_replace_containing_module_for_impl_tail {
    use super::PositionalDiversityReplaceImplTail;

    impl PositionalDiversityReplaceImplTail {
        fn generated_impl_tail_payload(&self) {}
    }
}
