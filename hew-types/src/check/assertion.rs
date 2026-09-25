//! `assert(condition[, message])`, the one assertion builtin.
//!
//! The builtin is registered with its one-parameter signature; the optional
//! message is the only arity it adds, so both forms are checked here and
//! publish the same call target. HIR desugars them, reporting a compared
//! operand as `{:?}` renders it; an operand whose type has no structural
//! rendering is published here so the report names its type instead.

use super::{
    type_def_for_spelling, BinaryOp, CallArg, CallTarget, Checker, Expr, Span, SpanKey, Ty,
    TypeDefKind, VariantDef,
};

impl Checker {
    /// Check a call of the `assert` builtin. Returns `None` for every other
    /// call, including one to a declaration that shadows the builtin.
    pub(super) fn check_assertion(
        &mut self,
        callee: &Expr,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let (condition, message) = match args {
            [CallArg::Positional(condition)] => (condition, None),
            [CallArg::Positional(condition), CallArg::Positional(message)] => {
                (condition, Some(message))
            }
            _ => return None,
        };
        let Expr::Ident(callee) = callee else {
            return None;
        };
        let assertion = CallTarget::Builtin {
            endpoint: crate::stdlib_catalog_identity::ASSERT.to_string(),
        };
        if self.env.lookup_ref(callee.name.as_str()).is_some() {
            return None;
        }
        // A declaration or an exact file import of that name wins over the
        // builtin, exactly as it does for any other call.
        let key = self
            .visible_fn_signature_key(callee.name.as_str())
            .unwrap_or_else(|| callee.name.to_string());
        if self.call_target_for_signature(&key) != assertion {
            return None;
        }
        self.check_against(&condition.0, &condition.1, &Ty::Bool);
        if let Expr::Binary { left, op, right } = &condition.0 {
            if matches!(
                op,
                BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual
            ) {
                for operand in [left, right] {
                    let key = SpanKey::in_module(&operand.1, self.current_module_idx);
                    let ty = self.expr_types.get(&key).map(|ty| self.subst.resolve(ty));
                    if ty.is_some_and(|ty| !self.renders_through_fields(&ty, &mut Vec::new())) {
                        self.unrendered_assertion_operands.insert(key);
                    }
                }
            }
        }
        if let Some(message) = message {
            self.check_against(&message.0, &message.1, &Ty::String);
        }
        self.record_direct_call_target(span, assertion);
        Some(Ty::Unit)
    }

    /// Whether `ty` renders structurally all the way down: a declared type
    /// through its fields and variant payloads, which an actor or supervisor
    /// handle never does. `visiting` guards recursive declarations.
    fn renders_through_fields<'a>(&'a self, ty: &'a Ty, visiting: &mut Vec<&'a str>) -> bool {
        if !ty.renders_structurally() {
            return false;
        }
        match ty {
            Ty::Tuple(members) => members
                .iter()
                .all(|member| self.renders_through_fields(member, visiting)),
            Ty::Named {
                name,
                args,
                builtin: None,
            } => {
                if visiting.contains(&name.as_str()) {
                    return true;
                }
                let Some(def) = type_def_for_spelling(&self.type_defs, name) else {
                    return true;
                };
                if matches!(def.kind, TypeDefKind::Actor | TypeDefKind::Supervisor) {
                    return false;
                }
                visiting.push(name);
                let fields_render = def
                    .fields
                    .values()
                    .chain(def.variants.values().flat_map(|variant| match variant {
                        VariantDef::Unit => Vec::new(),
                        VariantDef::Tuple(payload) => payload.iter().collect(),
                        VariantDef::Struct(fields) => fields.iter().map(|(_, ty)| ty).collect(),
                    }))
                    .chain(args)
                    .all(|field| self.renders_through_fields(field, visiting));
                visiting.pop();
                fields_render
            }
            Ty::Named { args, .. } => args
                .iter()
                .all(|arg| self.renders_through_fields(arg, visiting)),
            _ => true,
        }
    }
}
