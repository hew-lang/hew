//! `assert(condition[, message])`, the one assertion builtin.
//!
//! The builtin is registered with its one-parameter signature and gains its
//! optional message parameter at the call. HIR desugars both forms, reporting
//! a compared operand as `{:?}` renders it; an operand whose type has no
//! structural rendering is published here so the report names its type.

use super::{
    type_def_for_spelling, BinaryOp, CallArg, Checker, Expr, FnSig, SpanKey, Ty, TypeDefKind,
    VariantDef,
};

/// The signature of `assert(condition, message)`.
pub(super) fn with_message(mut sig: FnSig) -> FnSig {
    sig.params.push(Ty::String);
    if !sig.param_names.is_empty() {
        sig.param_names.push("message".to_string());
    }
    sig
}

impl Checker {
    /// Publish each operand of a compared `assert` condition whose type has no
    /// structural rendering.
    pub(super) fn record_unrendered_assertion_operands(&mut self, args: &[CallArg]) {
        let Some(condition) = args.first() else {
            return;
        };
        let Expr::Binary { left, op, right } = &condition.expr().0 else {
            return;
        };
        if !matches!(
            op,
            BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual
        ) {
            return;
        }
        for operand in [left, right] {
            let key = SpanKey::in_module(&operand.1, self.current_module_idx);
            let ty = self.expr_types.get(&key).map(|ty| self.subst.resolve(ty));
            if ty.is_some_and(|ty| !self.renders_through_fields(&ty, &mut Vec::new())) {
                self.unrendered_assertion_operands.insert(key);
            }
        }
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
