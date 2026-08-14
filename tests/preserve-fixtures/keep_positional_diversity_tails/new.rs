struct PositionalDiversityKeepStructTail {
    generated_struct_field_tail_payload: u8,
}

enum PositionalDiversityKeepEnumTail {
    GeneratedEnumVariantTail,
}

struct PositionalDiversityKeepImplTail;

fn positional_diversity_keep_block_statement_tail() {
    {
        let generated_block_statement_tail_payload = 0;
        let _ = generated_block_statement_tail_payload;
    }
}

fn positional_diversity_keep_if_else_chain_tail() {
    if true {
        let generated_if_else_tail_payload = 0;
        let _ = generated_if_else_tail_payload;
    } else {
        let generated_if_else_tail_payload = 1;
        let _ = generated_if_else_tail_payload;
    }
}

fn positional_diversity_keep_struct_literal_last_field_tail() {
    let _ = PositionalDiversityKeepStructTail {
        generated_struct_field_tail_payload: 0,
    };
}

mod positional_diversity_keep_outer_module_tail {
    mod generated_nested_module_tail {
        pub fn generated_nested_module_tail_payload() {}
    }
}

mod positional_diversity_keep_containing_module_for_impl_tail {
    use super::PositionalDiversityKeepImplTail;

    impl PositionalDiversityKeepImplTail {
        fn generated_impl_tail_payload(&self) {}
    }
}
