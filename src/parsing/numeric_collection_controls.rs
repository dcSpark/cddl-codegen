//! Refuse value comparisons on source collection heads before a numeric window can be
//! mistaken for an occurrence window. Source declarations remain authoritative even when
//! the recursion boundary later nominalizes an alias or generic resolution changes its IR.

use super::control::{
    control_operand_integer, decimal_integer_control_operand_rejection, type2_is_decimal_float,
};
use super::*;
use std::rc::Rc;

type Environment = BTreeMap<String, Binding>;

#[derive(Clone)]
enum Binding {
    Unbound,
    Argument(OuterShape),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum OuterShape {
    Collection,
    Other,
    /// Opaque names, lexical parameters and alias-only cycles remain owned by their
    /// existing admission checks; lack of a proof is not a collection classification.
    Unknown,
}

struct Scan<'a, 'b> {
    declarations: BTreeMap<String, &'b TypeRule<'a>>,
    environment: Rc<Environment>,
    visited_instances: BTreeSet<(String, Vec<OuterShape>)>,
    rule: String,
    top: Option<&'b Type1<'a>>,
    generic_definition: bool,
    found: BTreeMap<Span, String>,
}

impl<'a, 'b> Scan<'a, 'b> {
    fn bind_arguments(
        &self,
        rule: &'b TypeRule<'a>,
        args: &'b GenericArgs<'a>,
        environment: &Rc<Environment>,
    ) -> Option<Rc<Environment>> {
        let params = rule.generic_params.as_ref()?;
        if params.params.len() != args.args.len() {
            // Keep the ordinary generic-arity diagnostic rather than invent a shape.
            return None;
        }
        let shapes = args
            .args
            .iter()
            .map(|arg| self.type1_shape(&arg.arg, environment, &mut BTreeSet::new()))
            .collect::<Vec<_>>();
        Some(Self::argument_environment(params, &shapes))
    }

    fn argument_environment(params: &GenericParams<'_>, shapes: &[OuterShape]) -> Rc<Environment> {
        Rc::new(
            params
                .params
                .iter()
                .zip(shapes)
                .map(|(param, shape)| (param.param.to_string(), Binding::Argument(*shape)))
                .collect(),
        )
    }

    fn type_shape(
        &self,
        ty: &'b Type<'a>,
        environment: &Rc<Environment>,
        active: &mut BTreeSet<(String, Vec<OuterShape>)>,
    ) -> OuterShape {
        let mut result = OuterShape::Other;
        for choice in &ty.type_choices {
            match self.type1_shape(&choice.type1, environment, active) {
                OuterShape::Collection => return OuterShape::Collection,
                OuterShape::Unknown => result = OuterShape::Unknown,
                OuterShape::Other => {}
            }
        }
        result
    }

    fn type1_shape(
        &self,
        ty: &'b Type1<'a>,
        environment: &Rc<Environment>,
        active: &mut BTreeSet<(String, Vec<OuterShape>)>,
    ) -> OuterShape {
        if ty.operator.as_ref().is_some_and(|op| {
            matches!(
                op.operator,
                RangeCtlOp::CtlOp {
                    ctrl: token::ControlOperator::CBOR,
                    ..
                }
            )
        }) {
            // `.cbor` stores its decoded payload in the IR but its outer wire value is bytes.
            return OuterShape::Other;
        }
        self.head_shape(&ty.type2, environment, active)
    }

    fn head_shape(
        &self,
        head: &'b Type2<'a>,
        environment: &Rc<Environment>,
        active: &mut BTreeSet<(String, Vec<OuterShape>)>,
    ) -> OuterShape {
        match head {
            // Outer shape is terminal. Inspecting elements here would confuse a scalar
            // head with a collection elsewhere in its containing type, and recurse in cycles.
            Type2::Array { .. } | Type2::Map { .. } => OuterShape::Collection,
            Type2::ParenthesizedType { pt, .. } => self.type_shape(pt, environment, active),
            Type2::TaggedData { t, .. } => {
                // A tag is not numeric either. Limit this repair to tags whose outer
                // payload is provably a collection; do not reopen tagged scalar support.
                match self.type_shape(t, environment, active) {
                    OuterShape::Collection => OuterShape::Collection,
                    _ => OuterShape::Other,
                }
            }
            Type2::Typename {
                ident,
                generic_args,
                ..
            } => {
                let name = ident.to_string();
                if generic_args.is_none()
                    && let Some(binding) = environment.get(&name)
                {
                    return match binding {
                        Binding::Unbound => OuterShape::Unknown,
                        Binding::Argument(shape) => *shape,
                    };
                }
                let Some(rule) = self.declarations.get(&name).copied() else {
                    return OuterShape::Unknown;
                };
                let (bound, shapes) = match (&rule.generic_params, generic_args) {
                    (None, None) => (Rc::new(BTreeMap::new()), Vec::new()),
                    (Some(params), Some(args)) if params.params.len() == args.args.len() => {
                        let shapes = args
                            .args
                            .iter()
                            .map(|arg| self.type1_shape(&arg.arg, environment, active))
                            .collect::<Vec<_>>();
                        (Self::argument_environment(params, &shapes), shapes)
                    }
                    _ => return OuterShape::Unknown,
                };
                // The finite outer-shape state also permits nested id<id<array>>: each
                // argument is classified before entering its caller, and lexical bindings
                // carry that classification instead of recursively expanding the argument again.
                let key = (name, shapes);
                if !active.insert(key.clone()) {
                    return OuterShape::Unknown;
                }
                let result = self.type_shape(&rule.value, &bound, active);
                active.remove(&key);
                result
            }
            _ => OuterShape::Other,
        }
    }

    /// Only instantiate here. The caller owns traversal of argument source nodes:
    /// Typename needs an explicit argument walk, while TypeGroupnameEntry's stock walker
    /// already visits arguments and occurrences. scan_body restores the caller's bindings.
    fn scan_application(&mut self, ident: &Identifier<'a>, args: &'b GenericArgs<'a>) {
        if let Some(rule) = self.declarations.get(&ident.to_string()).copied()
            && let Some(environment) = self.bind_arguments(rule, args, &self.environment)
        {
            self.scan_body(rule, environment);
        }
    }

    fn scan_body(&mut self, rule: &'b TypeRule<'a>, environment: Rc<Environment>) {
        let name = rule.name.to_string();
        // Collection admission depends only on each parameter's outer shape. This finite
        // abstract key still visits a recursive g<T> -> g<[T]> application when T changes
        // from scalar to collection, without expanding ever-growing argument syntax forever.
        let parameter_shapes = rule
            .generic_params
            .as_ref()
            .map_or_else(Vec::new, |params| {
                params
                    .params
                    .iter()
                    .map(|param| match environment.get(&param.param.to_string()) {
                        Some(Binding::Argument(shape)) => *shape,
                        _ => OuterShape::Unknown,
                    })
                    .collect()
            });
        if !self
            .visited_instances
            .insert((name.clone(), parameter_shapes))
        {
            return;
        }
        let old_environment = std::mem::replace(&mut self.environment, environment);
        let old_rule = std::mem::replace(&mut self.rule, name.clone());
        let old_top = self.top;
        let old_generic = self.generic_definition;
        self.top = match rule.value.type_choices.as_slice() {
            [only] => Some(&only.type1),
            _ => None,
        };
        self.generic_definition = rule.generic_params.is_some();
        let _ = cddl::visitor::Visitor::visit_type(self, &rule.value);
        self.environment = old_environment;
        self.rule = old_rule;
        self.top = old_top;
        self.generic_definition = old_generic;
    }
}

impl<'a, 'b> cddl::visitor::Visitor<'a, 'b, std::fmt::Error> for Scan<'a, 'b> {
    fn visit_type1(&mut self, ty: &'b Type1<'a>) -> cddl::visitor::Result<std::fmt::Error> {
        if let Some(Operator {
            operator: RangeCtlOp::CtlOp { ctrl, .. },
            type2: operand,
            ..
        }) = &ty.operator
            && matches!(
                ctrl,
                token::ControlOperator::LT
                    | token::ControlOperator::LE
                    | token::ControlOperator::GT
                    | token::ControlOperator::GE
                    | token::ControlOperator::EQ
                    | token::ControlOperator::NE
            )
        {
            let at_top = self.top.is_some_and(|top| std::ptr::eq(top, ty));
            let mut parameter_head = &ty.type2;
            while let Type2::ParenthesizedType { pt, .. } = parameter_head
                && let [only] = pt.type_choices.as_slice()
                && only.type1.operator.is_none()
            {
                parameter_head = &only.type1.type2;
            }
            let direct_parameter = matches!(parameter_head, Type2::Typename { ident, generic_args: None, .. }
                if self.environment.contains_key(&ident.to_string()));
            // Direct lexical-parameter configuration and a constrained whole generic
            // definition already have precise unsupported-substitution diagnostics.
            if !(direct_parameter || at_top && self.generic_definition)
                && self.head_shape(&ty.type2, &self.environment, &mut BTreeSet::new())
                    == OuterShape::Collection
            {
                let rule_ident = RustIdent::new(CDDLIdent::new(self.rule.clone()));
                let prefix = at_top.then_some(&rule_ident);
                let message = if type2_is_decimal_float(operand) {
                    decimal_integer_control_operand_rejection(prefix, operand)
                } else if let Err(message) = control_operand_integer(prefix, *ctrl, operand) {
                    message
                } else if at_top
                    && let Type2::Typename {
                        ident,
                        generic_args: None,
                        ..
                    } = &ty.type2
                {
                    // This rule path already refused a non-primitive named head. Preserve
                    // that diagnostic; member/choice/generic paths are the newly refused ones.
                    unmapped_control_head_rejection(&rule_ident, &CDDLIdent::new(ident.to_string()))
                } else {
                    let reason = if matches!(
                        ctrl,
                        token::ControlOperator::EQ | token::ControlOperator::NE
                    ) {
                        "numeric equality/exclusion involving a collection is unsupported; comparing a collection with a numeric value is empty/vacuous, not an entry-count constraint"
                    } else {
                        "RFC 8610 section 3.8.6 defines ordering controls only for numeric values, not collection entry counts"
                    };
                    format!(
                        "rule `{}`: `{ctrl}` on collection head `{}` is unsupported — {reason}. Remove the control; if entry-count bounds were intended, put an occurrence inside the collection (`[0*1 elem]` or `{{1* key => value}}`).",
                        self.rule, ty.type2
                    )
                };
                // Source spans survive generic applications and recursion-boundary reparses.
                // Emit once per written control node, ordered by source position, even if
                // one application is numeric and another proves the head is a collection.
                self.found.entry(ty.span).or_insert(message);
            }
        }
        cddl::visitor::walk_type1(self, ty)
    }

    fn visit_type2(&mut self, ty: &'b Type2<'a>) -> cddl::visitor::Result<std::fmt::Error> {
        if let Type2::Typename {
            ident,
            generic_args: Some(args),
            ..
        } = ty
        {
            // The stock typename walker omits arguments. Their own controls are source nodes too.
            cddl::visitor::walk_generic_args(self, args)?;
            self.scan_application(ident, args);
        }
        cddl::visitor::walk_type2(self, ty)
    }

    fn visit_type_groupname_entry(
        &mut self,
        entry: &'b TypeGroupnameEntry<'a>,
    ) -> cddl::visitor::Result<std::fmt::Error> {
        // Bare [g<arr>] and repeated/rest [* g<arr>] carry no Type2::Typename.
        // Let the stock walker own occurrence, arguments and identifier exactly once.
        cddl::visitor::walk_type_groupname_entry(self, entry)?;
        if let Some(args) = &entry.generic_args {
            self.scan_application(&entry.name, args);
        }
        Ok(())
    }
}

pub(crate) fn rejections(cddl: &CDDL<'_>) -> Vec<String> {
    let mut scan = Scan {
        declarations: cddl
            .rules
            .iter()
            .filter_map(|rule| match rule {
                Rule::Type { rule, .. } => Some((rule.name.to_string(), rule)),
                _ => None,
            })
            .collect(),
        environment: Rc::new(BTreeMap::new()),
        visited_instances: BTreeSet::new(),
        rule: String::new(),
        top: None,
        generic_definition: false,
        found: BTreeMap::new(),
    };
    for rule in &cddl.rules {
        if rule_is_scope_marker(rule).is_some() {
            continue;
        }
        match rule {
            Rule::Type { rule, .. } => {
                let environment =
                    rule.generic_params
                        .as_ref()
                        .map_or_else(BTreeMap::new, |params| {
                            params
                                .params
                                .iter()
                                .map(|param| (param.param.to_string(), Binding::Unbound))
                                .collect()
                        });
                scan.scan_body(rule, Rc::new(environment));
            }
            Rule::Group { .. } => {
                scan.rule = rule.name();
                let _ = cddl::visitor::Visitor::visit_rule(&mut scan, rule);
            }
        }
    }
    scan.found.into_values().collect()
}

#[cfg(test)]
mod robustness_tests {
    use super::rejections;

    /// These source-only cases isolate generic shape classification from the IR's separate
    /// generic-body admission rules, including nested applications of the same declaration.
    #[test]
    fn nested_generic_binding_shapes_and_growing_recursion_are_finite() {
        for spec in [
            "id<T> = T\narr = [* uint]\nholder = [x: id<id<arr>> .le 1]\n",
            "id<T> = T\nwrap<T> = [x: id<T> .le 1]\narr = [* uint]\none = wrap<uint>\ntwo = wrap<arr>\n",
            "id<T> = T\ng<T> = [x: id<T> .le 1, next: g<[T]>]\nholder = g<uint>\n",
        ] {
            let cddl = cddl::ast::CDDL::from_slice(spec.as_bytes()).unwrap();
            let messages = rejections(&cddl);
            assert_eq!(
                messages.len(),
                1,
                "one written offending node: {messages:?}"
            );
            assert!(messages[0].contains("on collection head"), "{messages:?}");
        }
    }

    #[test]
    fn generic_collection_head_checks_match_keyed_bare_and_rest_entry_routes() {
        let definitions = "id<T> = T\ng<T> = [x: id<T> .le 1]\narr = [* uint]\n";
        for holder in [
            "holder = [x: g<arr>]\n",
            "holder = [g<arr>]\n",
            "holder = [* g<arr>]\n",
            "holder = [head: uint, * g<arr>]\n",
            "holder = {0: uint, * tstr => g<arr>}\n",
            "holder = [a: g<arr>, g<arr>, * g<arr>]\n",
        ] {
            let spec = format!("{definitions}{holder}");
            let cddl = cddl::ast::CDDL::from_slice(spec.as_bytes()).unwrap();
            let messages = rejections(&cddl);
            assert_eq!(
                messages.len(),
                1,
                "same written template control, route {holder}: {messages:?}"
            );
            assert!(
                messages[0].contains("rule `g`")
                    && messages[0].contains("on collection head `id<T>`"),
                "route {holder}: {messages:?}"
            );
        }
    }

    #[test]
    fn bare_and_rest_generic_argument_controls_are_walked_once() {
        for holder in ["holder = [g<arr .le 1>]\n", "holder = [* g<arr .le 1>]\n"] {
            let spec = format!("g<T> = [x: T]\narr = [* uint]\n{holder}");
            let cddl = cddl::ast::CDDL::from_slice(spec.as_bytes()).unwrap();
            let messages = rejections(&cddl);
            assert_eq!(
                messages.len(),
                1,
                "argument control once, route {holder}: {messages:?}"
            );
            assert!(
                messages[0].contains("rule `holder`")
                    && messages[0].contains("on collection head `arr`"),
                "route {holder}: {messages:?}"
            );
        }
    }

    #[test]
    fn bare_nested_application_restores_its_callers_lexical_bindings() {
        let spec = "id<T> = T\ng<T> = [x: id<T> .le 1]\nnested<U> = [g<U>, after: id<U> .ge 1]\narr = [* uint]\nholder = [nested<arr>]\n";
        let cddl = cddl::ast::CDDL::from_slice(spec.as_bytes()).unwrap();
        let messages = rejections(&cddl);
        assert_eq!(
            messages.len(),
            2,
            "nested and caller source controls: {messages:?}"
        );
        assert!(
            messages[0].contains("rule `g`") && messages[1].contains("rule `nested`"),
            "original source order and caller bindings: {messages:?}"
        );
    }

    #[test]
    fn parameter_shadowing_and_alias_only_cycles_do_not_invent_collection_heads() {
        for spec in [
            "T = [* uint]\ng<T> = [x: T .le 1]\nholder = g<uint>\n",
            "a = b\nb = a\nholder = [x: a .le 1]\n",
        ] {
            let cddl = cddl::ast::CDDL::from_slice(spec.as_bytes()).unwrap();
            assert!(rejections(&cddl).is_empty());
        }
    }
}
