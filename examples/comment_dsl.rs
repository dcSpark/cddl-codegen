//! Comment-DSL authority helper for `cddl-matrix/corpus_detect.ts`.
//!
//! stdin is a NUL-separated batch of CDDL comment-owner blocks; lines in one block are separated by
//! ASCII record separator (`0x1e`). The output is one JSON row per owner, either accepted facts from
//! `comment_ast::metadata_from_comments` or
//! `{"ok":false}` for a malformed/panicking grammar input. Keeping this tiny protocol
//! dependency-free makes the matrix invoke cargo once per batch, not once per corpus fixture.

use std::io::{self, Read};

use cddl_codegen::comment_ast::{
    DuplicatesPolicy, EncodingKind, MatrixDslFacts, WireMajor, metadata_from_comments,
};

fn json_str(s: &str) -> String {
    let mut out = String::from("\"");
    for ch in s.chars() {
        match ch {
            '\"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('\"');
    out
}

fn option_json(value: Option<&str>) -> String {
    value.map_or_else(|| "null".to_owned(), json_str)
}

fn encoding_names(kinds: &[EncodingKind]) -> String {
    format!(
        "[{}]",
        kinds
            .iter()
            .map(|kind| json_str(kind.token()))
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn facts_json(facts: MatrixDslFacts) -> String {
    let ids = facts
        .ids
        .iter()
        .map(|id| json_str(id))
        .collect::<Vec<_>>()
        .join(",");
    let demand = facts.key_demand.map_or_else(
        || "null".to_owned(),
        |d| {
            format!(
                "{{\"bare\":{},\"hash\":{},\"ord\":{}}}",
                d.bare, d.hash, d.ord
            )
        },
    );
    let getter = match facts.newtype_getter {
        None => "null".to_owned(),
        Some(None) => "\"default\"".to_owned(),
        Some(Some(name)) => json_str(&name),
    };
    let duplicates = facts.duplicates.map_or_else(
        || "null".to_owned(),
        |p| {
            json_str(match p {
                DuplicatesPolicy::Preserve => "preserve",
                DuplicatesPolicy::Reject => "reject",
            })
        },
    );
    let encodings = facts
        .custom_encodings
        .as_deref()
        .map_or_else(|| "null".to_owned(), encoding_names);
    let major = option_json(facts.custom_wire_major.map(WireMajor::token));
    let companions = facts.extern_companions.map_or_else(
        || "null".to_owned(),
        |c| {
            format!(
                "{{\"path\":{},\"classes\":[{}]}}",
                json_str(&c.path_prefix),
                c.classes
                    .iter()
                    .map(|class| json_str(class))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        },
    );
    format!(
        "{{\"ok\":true,\"ids\":[{ids}],\"keyDemand\":{demand},\"newtypeGetter\":{getter},\"duplicates\":{duplicates},\"customEncodings\":{encodings},\"customWireMajor\":{major},\"externCompanions\":{companions},\"doc\":{}}}",
        option_json(facts.doc.as_deref())
    )
}

fn main() {
    // The parser intentionally uses panics for malformed strict directive arguments.  A matrix
    // detector must treat those fixtures as uncreditable without losing the rest of the batch.
    std::panic::set_hook(Box::new(|_| {}));
    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input).expect("read stdin");
    let records: Vec<&[u8]> = if input.is_empty() {
        Vec::new()
    } else {
        input.split(|byte| *byte == 0).collect()
    };
    let rows = records
        .into_iter()
        .map(|record| {
            let text = std::str::from_utf8(record).expect("comment batch is UTF-8");
            let comments: Vec<&str> = text.split('\x1e').collect();
            match std::panic::catch_unwind(|| metadata_from_comments(&comments).matrix_dsl_facts())
            {
                Ok(facts) => facts_json(facts),
                Err(_) => "{\"ok\":false}".to_owned(),
            }
        })
        .collect::<Vec<_>>();
    println!("[{}]", rows.join(","));
}
