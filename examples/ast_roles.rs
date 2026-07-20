//! AST role-walk helper for `cddl-matrix` — the role floor behind `project_corpus.ts`'s role-keyed
//! `[[cover]]` verification (per-cell role × feature corpus coverage).
//!
//! Parses each CDDL file with the SAME `cddl` crate cddl-codegen builds against (=0.9.1) and emits,
//! per file, the `(role, construct)` pairs it exercises — where `role` is the enclosing container
//! context (cddl-matrix/roles.toml) the construct sits in. The matrix's text-scan floor
//! (cddl-matrix/corpus_detect.ts) can see THAT a construct appears but not in WHICH role; this gives
//! the role, which a regex cannot track across nesting.
//!
//! Division of labour: this emits structural node-KINDS + the role (the part TS cannot get without a
//! real parse). The KIND -> matrix-feature-id mapping (and prelude / control-op name resolution) stays
//! in TS where the "what is a feature" editorial decision already lives.
//!
//! Output JSON: with file args, an object `{ "<path>": [ {"role","kind","name"?}, ... ], ... }`; with
//! no args, a single `[ ... ]` array parsed from stdin. Hand-emitted (no serde_json dependency).
//!
//! Run: `cargo run -q --example ast_roles -- <file.cddl>...`
//!
//! reuses the crate's `Visitor` trait (its `walk_*` defaults do the recursion); we override
//! only the visits that open a role boundary and carry a tiny role stack. Roles reset on each descent,
//! so they do not accumulate down the tree.

use std::convert::Infallible;
use std::io::{self, Read};

use cddl::ast::*;
use cddl::cddl_from_str;
use cddl::token::{ControlOperator, Value};
use cddl::visitor::{self, Visitor};

type R = Result<(), Infallible>;

struct Rec {
    role: &'static str,
    kind: String,
    name: Option<String>,
}

struct RoleWalk {
    out: Vec<Rec>,
    // Roles the node about to be recorded occupies. Usually one; two only for an occurrence target
    // (e.g. `[* [int]]` — the inner array is both array-element and occurrence-target). Set by the
    // parent before descending; reset on every structural descent so roles do not leak down the tree.
    roles: Vec<&'static str>,
    // Role a group entry's value takes — "array-element" inside `[ ]`, "map-value" inside `{ }`.
    entry_role: &'static str,
}

impl RoleWalk {
    fn rec(&mut self, kind: &str, name: Option<String>) {
        // index-based to record under each active role without holding a borrow across the push
        for i in 0..self.roles.len() {
            let role = self.roles[i];
            self.out.push(Rec {
                role,
                kind: kind.to_string(),
                name: name.clone(),
            });
        }
    }
}

fn is_value_literal(t2: &Type2) -> bool {
    matches!(
        t2,
        Type2::IntValue { .. }
            | Type2::UintValue { .. }
            | Type2::FloatValue { .. }
            | Type2::TextValue { .. }
            | Type2::UTF8ByteString { .. }
            | Type2::B16ByteString { .. }
            | Type2::B64ByteString { .. }
    )
}

fn occur_kind(o: &Occur) -> &'static str {
    match o {
        Occur::Optional { .. } => "occur.optional",
        Occur::ZeroOrMore { .. } => "occur.zero_or_more",
        Occur::OneOrMore { .. } => "occur.one_or_more",
        Occur::Exact { .. } => "occur.bounded",
    }
}

fn value_kind(v: &Value) -> &'static str {
    match v {
        Value::INT(_) | Value::UINT(_) | Value::FLOAT(_) => "value.number",
        Value::TEXT(_) => "value.text",
        Value::BYTE(_) => "value.bytes",
    }
}

impl<'a, 'b> Visitor<'a, 'b, Infallible> for RoleWalk {
    fn visit_type_rule(&mut self, tr: &'b TypeRule<'a>) -> R {
        self.roles = vec!["top-level"];
        self.entry_role = "top-level";
        if tr.generic_params.is_some() {
            self.rec("genericparm.type", None);
        }
        self.visit_type(&tr.value)
    }

    fn visit_group_rule(&mut self, gr: &'b GroupRule<'a>) -> R {
        self.roles = vec!["top-level"];
        self.entry_role = "top-level";
        if gr.generic_params.is_some() {
            self.rec("genericparm.group", None);
        }
        self.visit_group_entry(&gr.entry)
    }

    fn visit_type(&mut self, t: &'b Type<'a>) -> R {
        if t.type_choices.len() > 1 {
            self.rec("type.choice", None);
            let all_values = t
                .type_choices
                .iter()
                .all(|tc| tc.type1.operator.is_none() && is_value_literal(&tc.type1.type2));
            if all_values {
                self.rec("type.enum", None);
            }
            let saved = self.roles.clone();
            self.roles = vec!["choice-member"];
            for tc in &t.type_choices {
                self.visit_type_choice(tc)?;
            }
            self.roles = saved;
            Ok(())
        } else {
            visitor::walk_type(self, t)
        }
    }

    fn visit_type2(&mut self, t2: &'b Type2<'a>) -> R {
        match t2 {
            Type2::IntValue { .. } | Type2::UintValue { .. } | Type2::FloatValue { .. } => {
                self.rec("type2.value", None);
                self.rec("value.number", None);
                Ok(())
            }
            Type2::TextValue { .. } => {
                self.rec("type2.value", None);
                self.rec("value.text", None);
                Ok(())
            }
            Type2::UTF8ByteString { .. }
            | Type2::B16ByteString { .. }
            | Type2::B64ByteString { .. } => {
                self.rec("type2.value", None);
                self.rec("value.bytes", None);
                Ok(())
            }
            Type2::Typename {
                ident,
                generic_args,
                ..
            } => {
                self.rec("typename", Some(ident.ident.to_string()));
                if let Some(ga) = generic_args {
                    self.rec("genericarg.type", None);
                    let saved = self.roles.clone();
                    self.roles = vec!["generic-arg"];
                    for arg in &ga.args {
                        self.visit_type1(&arg.arg)?;
                    }
                    self.roles = saved;
                }
                Ok(())
            }
            Type2::ParenthesizedType { pt, .. } => {
                self.rec("type2.parenthesized", None);
                self.visit_type(pt)
            }
            Type2::Map { group, .. } => {
                self.rec("type2.map", None);
                let prev = self.entry_role;
                self.entry_role = "map-value";
                let r = self.visit_group(group);
                self.entry_role = prev;
                r
            }
            Type2::Array { group, .. } => {
                self.rec("type2.array", None);
                let prev = self.entry_role;
                self.entry_role = "array-element";
                let r = self.visit_group(group);
                self.entry_role = prev;
                r
            }
            Type2::TaggedData { t, .. } => {
                self.rec("type2.tag", None);
                let saved = self.roles.clone();
                self.roles = vec!["tag-content"];
                let r = self.visit_type(t);
                self.roles = saved;
                r
            }
            Type2::Unwrap { ident, .. } => {
                self.rec("type2.unwrap", Some(ident.ident.to_string()));
                Ok(())
            }
            Type2::ChoiceFromGroup { ident, .. } => {
                self.rec("type2.choice_from_group", Some(ident.ident.to_string()));
                Ok(())
            }
            Type2::ChoiceFromInlineGroup { group, .. } => {
                self.rec("type2.choice_from_inline_group", None);
                self.visit_group(group)
            }
            Type2::DataMajorType { mt, .. } => {
                self.rec("type2.major", None);
                if *mt == 7 {
                    self.rec("type2.major7", None);
                }
                Ok(())
            }
            Type2::Any { .. } => {
                self.rec("type2.any", None);
                Ok(())
            }
        }
    }

    fn visit_range(&mut self, lower: &'b Type2<'a>, upper: &'b Type2<'a>, is_inclusive: bool) -> R {
        self.rec(
            if is_inclusive {
                "rangeop.inclusive"
            } else {
                "rangeop.exclusive"
            },
            None,
        );
        self.visit_type2(lower)?;
        self.visit_type2(upper)
    }

    fn visit_control_operator(
        &mut self,
        target: &'b Type2<'a>,
        ctrl: ControlOperator,
        controller: &'b Type2<'a>,
    ) -> R {
        self.rec("type1.ctlop", None);
        let name = format!("{}", ctrl);
        self.rec("ctlop", Some(name.trim_start_matches('.').to_string()));
        self.visit_type2(target)?;
        // The controller of `.cbor` / `.cborseq` is the cbor-payload role; other controllers keep role.
        if matches!(ctrl, ControlOperator::CBOR | ControlOperator::CBORSEQ) {
            let saved = self.roles.clone();
            self.roles = vec!["cbor-payload"];
            let r = self.visit_type2(controller);
            self.roles = saved;
            r
        } else {
            self.visit_type2(controller)
        }
    }

    fn visit_value_member_key_entry(&mut self, entry: &'b ValueMemberKeyEntry<'a>) -> R {
        let saved = self.roles.clone();
        if let Some(mk) = &entry.member_key {
            self.roles = vec!["map-key"];
            self.visit_memberkey(mk)?;
        }
        if let Some(occ) = &entry.occur {
            self.roles = vec![self.entry_role];
            self.rec(occur_kind(&occ.occur), None);
        }
        self.roles = if entry.occur.is_some() {
            vec![self.entry_role, "occurrence-target"]
        } else {
            vec![self.entry_role]
        };
        self.visit_type(&entry.entry_type)?;
        self.roles = saved;
        Ok(())
    }

    fn visit_type_groupname_entry(&mut self, entry: &'b TypeGroupnameEntry<'a>) -> R {
        let saved = self.roles.clone();
        if let Some(occ) = &entry.occur {
            self.roles = vec![self.entry_role];
            self.rec(occur_kind(&occ.occur), None);
        }
        // A bare groupname entry is almost always a TYPE reference (a prelude type or a type rule) — the
        // parser models `[uint]` / `[someType]` as a TypeGroupnameEntry too. Emit it as a `typename` and
        // let TS resolve prelude-vs-local, so prelude.<name> / type2.typename aren't lost in this role.
        // (A true group splice — name resolving to a *group* rule — is rare and uncovered by the corpus.)
        self.roles = if entry.occur.is_some() {
            vec![self.entry_role, "occurrence-target"]
        } else {
            vec![self.entry_role]
        };
        self.rec("typename", Some(entry.name.ident.to_string()));
        self.roles = saved;
        Ok(())
    }

    fn visit_inline_group_entry(
        &mut self,
        occur: Option<&'b Occurrence<'a>>,
        g: &'b Group<'a>,
    ) -> R {
        let saved = self.roles.clone();
        self.roles = if occur.is_some() {
            vec![self.entry_role, "occurrence-target"]
        } else {
            vec![self.entry_role]
        };
        self.rec("grpent.inline_group", None);
        if let Some(occ) = occur {
            self.roles = vec![self.entry_role];
            self.rec(occur_kind(&occ.occur), None);
        }
        self.roles = saved;
        self.visit_group(g)
    }

    fn visit_memberkey(&mut self, mk: &'b MemberKey<'a>) -> R {
        match mk {
            MemberKey::Bareword { .. } => {
                self.rec("memberkey.bareword", None);
                Ok(())
            }
            MemberKey::Value { value, .. } => {
                self.rec("memberkey.value", None);
                self.rec(value_kind(value), None);
                Ok(())
            }
            MemberKey::Type1 { t1, is_cut, .. } => {
                self.rec("memberkey.type1", None);
                if *is_cut {
                    self.rec("memberkey.cut", None);
                }
                self.visit_type1(t1)
            }
            MemberKey::NonMemberKey { .. } => Ok(()),
        }
    }

    fn visit_group(&mut self, g: &'b Group<'a>) -> R {
        if g.group_choices.len() > 1 {
            self.rec("group.choice", None);
        }
        visitor::walk_group(self, g)
    }
}

fn walk(text: &str) -> Vec<Rec> {
    let cddl = match cddl_from_str(text, false) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("ast_roles: parse error: {e}");
            std::process::exit(2);
        }
    };
    let mut v = RoleWalk {
        out: Vec::new(),
        roles: vec!["top-level"],
        entry_role: "top-level",
    };
    let _ = v.visit_cddl(&cddl);
    v.out
}

fn json_str(s: &str) -> String {
    let mut o = String::from("\"");
    for c in s.chars() {
        match c {
            '"' => o.push_str("\\\""),
            '\\' => o.push_str("\\\\"),
            '\n' => o.push_str("\\n"),
            '\r' => o.push_str("\\r"),
            '\t' => o.push_str("\\t"),
            c if (c as u32) < 0x20 => o.push_str(&format!("\\u{:04x}", c as u32)),
            c => o.push(c),
        }
    }
    o.push('"');
    o
}

fn recs_to_json(recs: &[Rec]) -> String {
    let mut o = String::from("[");
    for (i, r) in recs.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        o.push_str("{\"role\":");
        o.push_str(&json_str(r.role));
        o.push_str(",\"kind\":");
        o.push_str(&json_str(&r.kind));
        if let Some(n) = &r.name {
            o.push_str(",\"name\":");
            o.push_str(&json_str(n));
        }
        o.push('}');
    }
    o.push(']');
    o
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        let mut input = String::new();
        io::stdin().read_to_string(&mut input).expect("read stdin");
        println!("{}", recs_to_json(&walk(&input)));
    } else {
        let mut o = String::from("{");
        for (i, path) in args.iter().enumerate() {
            let text = std::fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("ast_roles: read {path}: {e}"));
            if i > 0 {
                o.push(',');
            }
            o.push_str(&json_str(path));
            o.push(':');
            o.push_str(&recs_to_json(&walk(&text)));
        }
        o.push('}');
        println!("{o}");
    }
}
