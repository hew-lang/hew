//! Body order for source functions whose result must be inferred before use.

use super::lints::{walk_block, NodeVisitor};
use super::{Expr, FnDecl, HashMap, HashSet, Ident, Item, Span, Spanned, TypeExpr};

fn infers_result(function: &FnDecl) -> bool {
    matches!(function.return_type, Some((TypeExpr::Infer, _)))
}

fn functions(item: &Item) -> &[FnDecl] {
    match item {
        Item::Function(function) => std::slice::from_ref(function),
        Item::Impl(implementation) => &implementation.methods,
        _ => &[],
    }
}

/// Check inferred declarations before the bodies that instantiate them.
/// Names here only determine visitation order. The normal checker resolves
/// each reference in its declaring scope and validates the body exactly once.
pub(super) fn body_order(items: &[Spanned<Item>]) -> Vec<usize> {
    let mut names: HashMap<Ident, Vec<usize>> = HashMap::new();
    let mut inferred = Vec::new();
    let mut remaining = Vec::new();
    for (index, (item, _)) in items.iter().enumerate() {
        let mut has_inferred = false;
        for function in functions(item).iter().filter(|f| infers_result(f)) {
            names.entry(function.name).or_default().push(index);
            has_inferred = true;
        }
        if has_inferred {
            inferred.push(index);
        } else {
            remaining.push(index);
        }
    }
    if inferred.is_empty() {
        return remaining;
    }
    let mut edges = HashMap::new();
    for &index in &inferred {
        let mut dependencies = Dependencies {
            names: &names,
            targets: HashSet::new(),
        };
        for function in functions(&items[index].0) {
            walk_block(&function.body, &mut dependencies);
        }
        edges.insert(index, dependencies.targets);
    }
    crate::cycle::tarjan_scc(&inferred, &edges)
        .into_iter()
        .flatten()
        .chain(remaining)
        .collect()
}

struct Dependencies<'a> {
    names: &'a HashMap<Ident, Vec<usize>>,
    targets: HashSet<usize>,
}

impl NodeVisitor for Dependencies<'_> {
    fn visit_expr(&mut self, expression: &Expr, _: &Span) {
        let name = match expression {
            Expr::Ident(name) => name,
            Expr::MethodCall { method, .. } => &method.0,
            Expr::FieldAccess { field, .. } => &field.0,
            _ => return,
        };
        if let Some(targets) = self.names.get(name) {
            self.targets.extend(targets);
        }
    }
}

/// A recursive call can precede the point at which its callee's body fixes the
/// result. Its result belongs to this call's instantiation, never to the
/// callee's generic scope.
#[derive(Debug, Clone)]
pub(super) struct CallReturn {
    pub(super) declaration_result: super::Ty,
    pub(super) arguments: HashMap<crate::ParamHead, super::Ty>,
    pub(super) site_result: super::Ty,
    pub(super) span: Span,
    pub(super) source: Option<String>,
}

impl super::Checker {
    pub(super) fn defer_inferred_call_return(
        &mut self,
        sig: &super::FnSig,
        result: &super::Ty,
        resolved_type_args: &[super::Ty],
        receiver_type_args: &[super::Ty],
        span: &Span,
    ) {
        if sig.inferred_return && self.subst.resolve(&sig.return_type).has_inference_var() {
            let mut arguments: HashMap<_, _> = sig
                .type_params
                .iter()
                .copied()
                .zip(resolved_type_args.iter().cloned())
                .collect();
            if let Some(receiver) = sig.impl_method.as_ref().and_then(|method| {
                crate::method_resolution::impl_receiver_substitution(
                    &method.receiver_parameters,
                    &method.receiver_args,
                    receiver_type_args,
                )
            }) {
                arguments.extend(receiver);
            }
            self.deferred_call_returns.push(CallReturn {
                declaration_result: sig.return_type.clone(),
                site_result: result.clone(),
                arguments,
                span: span.clone(),
                source: self.current_diagnostic_source_module(),
            });
        }
    }

    pub(super) fn resolve_inferred_call_returns(&mut self) {
        let mut pending = std::mem::take(&mut self.deferred_call_returns);
        loop {
            let before = pending.len();
            pending.retain(|call| {
                let declared = self.subst.resolve(&call.declaration_result);
                if declared.has_inference_var() {
                    return true;
                }
                let instantiated = declared.substitute_type_params_parallel(&call.arguments);
                let actual = self.subst.resolve(&instantiated);
                let expected = self.subst.resolve(&call.site_result);
                if !self.try_unify_with_owner_identity(&expected, &actual) {
                    let mut error =
                        super::TypeError::mismatch(call.span.clone(), &expected, &actual);
                    error.source_module.clone_from(&call.source);
                    self.errors.push(error);
                }
                false
            });
            if pending.len() == before || pending.is_empty() {
                break;
            }
        }
        self.deferred_call_returns = pending;
    }
}
