//! Private-reference scanning for imported impls and functions.

use super::*;

/// Bare call names used only for private helper closure discovery.
#[derive(Default)]
pub(super) struct CallNames {
    pub(super) bare: Vec<String>,
}

/// Collect the names of private helper functions that are referenced by direct
/// `Expr::Call { function: Expr::Ident(name) }` within `body` and whose
/// names appear in `candidate_fns`. Method-call syntax (`foo.bar()`) is not
/// tracked — only bare identifier callees are considered. Returns a sorted,
/// deduplicated list of matching names.
pub(super) fn collect_bare_fn_call_refs(
    body: &Block,
    candidate_fns: &HashSet<String>,
) -> Vec<String> {
    let mut found = CallNames::default();
    scan_block_for_private_refs(body, Some(candidate_fns), &mut found);
    let mut bare = found.bare;
    bare.sort_unstable();
    bare.dedup();
    bare
}

/// Iterate the parameter and return `TypeExpr`s of an impl-block method.
pub(super) fn method_signature_type_exprs(method: &FnDecl) -> impl Iterator<Item = &TypeExpr> {
    method
        .params
        .iter()
        .map(|param| &param.ty.0)
        .chain(method.return_type.as_ref().map(|rt| &rt.0))
}

/// Collect every nominal leaf name mentioned in a surface `TypeExpr`,
/// descending through type arguments and structural composites. Source
/// spellings only — no resolution, no qualification.
pub(super) fn collect_type_expr_named_leaves(ty: &TypeExpr, out: &mut Vec<String>) {
    match ty {
        TypeExpr::QualifiedAssocPath(path) => {
            collect_type_expr_named_leaves(&path.base.0, out);
            out.push(path.trait_path.to_string()); // TRANSITION(P1): deleted by A1 commit 2
        }
        TypeExpr::Named {
            path: named_path,
            type_args,
        } => {
            let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
            out.push(name.clone());
            for arg in type_args.as_deref().unwrap_or(&[]) {
                collect_type_expr_named_leaves(&arg.0, out);
            }
        }
        TypeExpr::Result { ok, err }
        | TypeExpr::Fallible {
            success: ok,
            error: err,
        } => {
            collect_type_expr_named_leaves(&ok.0, out);
            collect_type_expr_named_leaves(&err.0, out);
        }
        TypeExpr::Option(inner)
        | TypeExpr::Slice(inner)
        | TypeExpr::Array { element: inner, .. }
        | TypeExpr::Pointer { pointee: inner, .. }
        | TypeExpr::Borrow(inner) => {
            collect_type_expr_named_leaves(&inner.0, out);
        }
        TypeExpr::Tuple(elements) => {
            for element in elements {
                collect_type_expr_named_leaves(&element.0, out);
            }
        }
        TypeExpr::Function {
            params,
            return_type,
            ..
        }
        | TypeExpr::ActorFn {
            params,
            return_type,
        } => {
            for param in params {
                collect_type_expr_named_leaves(&param.0, out);
            }
            collect_type_expr_named_leaves(&return_type.0, out);
        }
        TypeExpr::TraitObject(_) | TypeExpr::Infer => {}
    }
}

pub(super) fn collect_imported_private_fn_closure<'a>(
    module: &hew_parser::module::Module,
    private_fns: &HashSet<String>,
    default_bodies: impl Iterator<Item = &'a Block>,
) -> HashSet<String> {
    let private_fn_bodies: HashMap<String, &Block> = module
        .items
        .iter()
        .filter_map(|(item, _)| {
            if let Item::Function(func) = item {
                if !func.visibility.is_pub() {
                    return Some((func.name.to_string(), &func.body));
                }
            }
            None
        })
        .collect();
    let mut reachable = HashSet::new();
    let mut worklist = Vec::new();
    let seed_from = |body: &Block, reachable: &mut HashSet<String>, worklist: &mut Vec<String>| {
        for helper in collect_bare_fn_call_refs(body, private_fns) {
            if reachable.insert(helper.clone()) {
                worklist.push(helper);
            }
        }
    };
    for (item, _) in &module.items {
        match item {
            // Pub free fns are importer-callable entry points.
            Item::Function(func) if func.visibility.is_pub() => {
                seed_from(&func.body, &mut reachable, &mut worklist);
            }
            // Impl-block methods are importer-callable entry points too (they
            // are lowered cross-module by `lower_impl_block`). A private helper
            // reachable only through a method body — e.g. `net.set_read_timeout`
            // calling `net_result_from_status` — must still be pulled into the
            // closure so the method's body resolves it. Without this seed the
            // method is skipped as body-unresolvable even though the helper
            // exists in the same module.
            Item::Impl(impl_decl) => {
                for method in &impl_decl.methods {
                    seed_from(&method.body, &mut reachable, &mut worklist);
                }
            }
            // Pub actor bodies (init + receive fns + inherent methods) are
            // importer-reachable entry points once the actor is spawned across
            // a module boundary (see the `Item::Actor` arm in the imported-
            // module walk). A private helper called only from a `receive fn` or
            // `init` body — e.g. an `Account` actor whose `deposit` clamps via a
            // module-private `clamp_nonneg` — must be pulled into the closure so
            // the lowered actor body resolves it to the helper's qualified
            // symbol. Without this seed the bare call fails closed with
            // `UnresolvedSymbol`, even though the helper exists in the module.
            Item::Actor(actor) if actor.visibility.is_pub() => {
                if let Some(init) = &actor.init {
                    seed_from(&init.body, &mut reachable, &mut worklist);
                }
                for receive_fn in &actor.receive_fns {
                    seed_from(&receive_fn.body, &mut reachable, &mut worklist);
                }
                for method in &actor.methods {
                    seed_from(&method.body, &mut reachable, &mut worklist);
                }
            }
            _ => {}
        }
    }
    for body in default_bodies {
        seed_from(body, &mut reachable, &mut worklist);
    }
    while let Some(helper) = worklist.pop() {
        if let Some(body) = private_fn_bodies.get(&helper) {
            for next in collect_bare_fn_call_refs(body, private_fns) {
                if reachable.insert(next.clone()) {
                    worklist.push(next);
                }
            }
        }
    }
    reachable
}

pub(super) fn scan_block_for_private_refs(
    block: &Block,
    pf: Option<&HashSet<String>>,
    out: &mut CallNames,
) {
    for (stmt, _) in &block.stmts {
        scan_stmt_for_private_refs(stmt, pf, out);
    }
    if let Some(e) = &block.trailing_expr {
        scan_expr_for_private_refs(&e.0, pf, out);
    }
}

pub(super) fn scan_stmt_for_private_refs(
    stmt: &Stmt,
    pf: Option<&HashSet<String>>,
    out: &mut CallNames,
) {
    match stmt {
        Stmt::Let { value: Some(v), .. } | Stmt::Var { value: Some(v), .. } => {
            scan_expr_for_private_refs(&v.0, pf, out);
        }
        Stmt::Assign { target, value, .. } => {
            scan_expr_for_private_refs(&target.0, pf, out);
            scan_expr_for_private_refs(&value.0, pf, out);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            scan_expr_for_private_refs(&condition.0, pf, out);
            scan_block_for_private_refs(then_block, pf, out);
            if let Some(eb) = else_block {
                if let Some(b) = &eb.block {
                    scan_block_for_private_refs(b, pf, out);
                }
                if let Some(s) = &eb.if_stmt {
                    scan_stmt_for_private_refs(&s.0, pf, out);
                }
            }
        }
        Stmt::IfLet {
            conditions,
            body,
            else_body,
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_private_refs(&expr.0, pf, out);
            }
            scan_block_for_private_refs(body, pf, out);
            if let Some(eb) = else_body {
                scan_expr_for_private_refs(&eb.0, pf, out);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            scan_expr_for_private_refs(&scrutinee.0, pf, out);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    scan_expr_for_private_refs(&g.0, pf, out);
                }
                scan_expr_for_private_refs(&arm.body.0, pf, out);
            }
        }
        Stmt::Loop { body, .. } => scan_block_for_private_refs(body, pf, out),
        Stmt::For { iterable, body, .. } => {
            scan_expr_for_private_refs(&iterable.0, pf, out);
            scan_block_for_private_refs(body, pf, out);
        }
        Stmt::While {
            condition, body, ..
        } => {
            scan_expr_for_private_refs(&condition.0, pf, out);
            scan_block_for_private_refs(body, pf, out);
        }
        Stmt::WhileLet {
            conditions, body, ..
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_private_refs(&expr.0, pf, out);
            }
            scan_block_for_private_refs(body, pf, out);
        }
        Stmt::Break { value: Some(v), .. } => scan_expr_for_private_refs(&v.0, pf, out),
        Stmt::Return(Some(e)) | Stmt::Expression(e) => {
            scan_expr_for_private_refs(&e.0, pf, out);
        }
        Stmt::Defer(e) => scan_expr_for_private_refs(&e.0, pf, out),
        _ => {}
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive single-pass AST walk; splitting into sub-functions would obscure the traversal structure without adding clarity"
)]
pub(super) fn scan_expr_for_private_refs(
    expr: &Expr,
    pf: Option<&HashSet<String>>,
    out: &mut CallNames,
) {
    match expr {
        Expr::Ident(name)
            if pf.is_some_and(|candidates| candidates.contains(name.name.as_str())) =>
        {
            out.bare.push(name.to_string());
        }
        Expr::GenericApplySuffix { target, .. } => {
            scan_expr_for_private_refs(&target.0, pf, out);
        }

        Expr::Call { function, args, .. } => {
            if let Expr::Ident(name) = &function.0 {
                // `pf == None` collects every bare call name; `Some(set)` records
                // only names present in `set` (the same-module private-fn filter).
                if pf.is_none_or(|set| set.contains(name.name.as_str())) {
                    out.bare.push(name.to_string());
                }
            }
            scan_expr_for_private_refs(&function.0, pf, out);
            for arg in args {
                scan_expr_for_private_refs(&arg.expr().0, pf, out);
            }
        }
        Expr::Binary { left, right, .. }
        | Expr::Coalesce { left, right }
        | Expr::Handle {
            operand: left,
            body: right,
            ..
        } => {
            scan_expr_for_private_refs(&left.0, pf, out);
            scan_expr_for_private_refs(&right.0, pf, out);
        }
        Expr::Unary { operand, .. } | Expr::ReturnError(operand) | Expr::Clone(operand) => {
            scan_expr_for_private_refs(&operand.0, pf, out);
        }
        Expr::Tuple(es) | Expr::Race(es) => {
            for e in es {
                scan_expr_for_private_refs(&e.0, pf, out);
            }
        }
        Expr::Array(elements) => {
            for element in elements {
                scan_expr_for_private_refs(&element.expr().0, pf, out);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            scan_expr_for_private_refs(&value.0, pf, out);
            scan_expr_for_private_refs(&count.0, pf, out);
        }
        Expr::Block(b)
        | Expr::Scope { body: b }
        | Expr::ForkBlock { body: b }
        | Expr::GenBlock { body: b } => {
            scan_block_for_private_refs(b, pf, out);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            scan_expr_for_private_refs(&condition.0, pf, out);
            scan_expr_for_private_refs(&then_block.0, pf, out);
            if let Some(e) = else_block {
                scan_expr_for_private_refs(&e.0, pf, out);
            }
        }
        Expr::IfLet {
            conditions,
            body,
            else_body,
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_private_refs(&expr.0, pf, out);
            }
            scan_block_for_private_refs(body, pf, out);
            if let Some(eb) = else_body {
                scan_expr_for_private_refs(&eb.0, pf, out);
            }
        }
        Expr::Match { scrutinee, arms } => {
            scan_expr_for_private_refs(&scrutinee.0, pf, out);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    scan_expr_for_private_refs(&g.0, pf, out);
                }
                scan_expr_for_private_refs(&arm.body.0, pf, out);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            scan_expr_for_private_refs(&body.0, pf, out);
        }
        Expr::Spawn { target, args, .. } => {
            scan_expr_for_private_refs(&target.0, pf, out);
            for (_, v) in args {
                scan_expr_for_private_refs(&v.0, pf, out);
            }
        }
        Expr::ScopeDeadline { duration, body } => {
            scan_expr_for_private_refs(&duration.0, pf, out);
            scan_block_for_private_refs(body, pf, out);
        }
        Expr::ForkChild { expr, .. } | Expr::Cast { expr, .. } => {
            scan_expr_for_private_refs(&expr.0, pf, out);
        }
        Expr::MethodCall { receiver, args, .. } => {
            scan_expr_for_private_refs(&receiver.0, pf, out);
            for arg in args {
                scan_expr_for_private_refs(&arg.expr().0, pf, out);
            }
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, v) in fields {
                scan_expr_for_private_refs(&v.0, pf, out);
            }
            if let Some(b) = base {
                scan_expr_for_private_refs(&b.0, pf, out);
            }
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                scan_expr_for_private_refs(&k.0, pf, out);
                scan_expr_for_private_refs(&v.0, pf, out);
            }
        }
        Expr::ContextVariant(context) => {
            if let Some(record) = &context.record {
                for (_, value) in &record.fields {
                    scan_expr_for_private_refs(&value.0, pf, out);
                }
                if let Some(base) = &record.base {
                    scan_expr_for_private_refs(&base.0, pf, out);
                }
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let hew_parser::ast::StringPart::Expr(e)
                | hew_parser::ast::StringPart::StructuralExpr(e) = part
                {
                    scan_expr_for_private_refs(&e.0, pf, out);
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                scan_expr_for_private_refs(&arm.source.0, pf, out);
                scan_expr_for_private_refs(&arm.body.0, pf, out);
            }
            if let Some(t) = timeout {
                scan_expr_for_private_refs(&t.duration.0, pf, out);
                scan_expr_for_private_refs(&t.body.0, pf, out);
            }
        }
        Expr::UnsafeBlock(b) => scan_block_for_private_refs(b, pf, out),
        Expr::FieldAccess { object, .. } | Expr::PostfixTry(object) | Expr::Await(object) => {
            scan_expr_for_private_refs(&object.0, pf, out);
        }
        Expr::Index { object, index } => {
            scan_expr_for_private_refs(&object.0, pf, out);
            scan_expr_for_private_refs(&index.0, pf, out);
        }
        Expr::Is { lhs, rhs } => {
            scan_expr_for_private_refs(&lhs.0, pf, out);
            scan_expr_for_private_refs(&rhs.0, pf, out);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                scan_expr_for_private_refs(&s.0, pf, out);
            }
            if let Some(e) = end {
                scan_expr_for_private_refs(&e.0, pf, out);
            }
        }
        Expr::Yield(Some(e)) => scan_expr_for_private_refs(&e.0, pf, out),
        Expr::MachineEmit { fields, .. } => {
            for (_, v) in fields {
                scan_expr_for_private_refs(&v.0, pf, out);
            }
        }
        _ => {}
    }
}

/// V0b-admissible shapes. Returns `None` when the impl is admissible (no
/// where-clause, or a where-clause whose predicates are all `where T: Bound(s)`
/// on the impl's own outer type parameters) and `Some(shape)` describing the
/// offending predicate otherwise. Multi-bound predicates (`where T: A + B`) are
/// admitted; only predicates on parameterised types and non-type-param names
/// remain fail-closed. The bound itself is consumed by the checker
/// (`enter_impl_scope` / `register_impl_method` already harvest
/// `decl.where_clause`); the HIR carries no extra metadata beyond the existing
/// `type_params` list because trait bounds have no runtime artefact.
pub(super) fn classify_unsupported_where_clause(
    decl: &hew_parser::ast::ImplDecl,
) -> Option<String> {
    let where_clause = decl.where_clause.as_ref()?;
    let type_param_names: Vec<&str> = decl
        .type_params
        .as_ref()
        .map(|ps| ps.iter().map(|p| p.name.name.as_str()).collect())
        .unwrap_or_default();
    for predicate in &where_clause.predicates {
        let TypeExpr::Named {
            path: named_path,
            type_args,
        } = &predicate.ty.0
        else {
            return Some("where-clause predicate on non-named type".to_string());
        };
        let pred_ty_name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
        if type_args.is_some() {
            return Some(format!(
                "where-clause predicate on parameterised type `{pred_ty_name}<...>`"
            ));
        }
        if !type_param_names.contains(&pred_ty_name.as_str()) {
            return Some(format!(
                "where-clause predicate on non-type-param `{pred_ty_name}`"
            ));
        }
    }
    None
}
