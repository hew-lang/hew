//! Private-reference scanning for imported impls and functions.

use super::*;

/// Close imported private helpers over checker-resolved declaration uses.
/// Local bindings with the same spelling never enter this graph. Function
/// values and references inside closures use the same checked resolution table
/// as direct calls.
pub(super) fn collect_imported_private_fn_closure(
    ctx: &LowerCtx,
    module: &hew_parser::module::Module,
    indices: &hew_parser::module::FileSpanIndices,
) -> HashSet<usize> {
    let mut private = HashMap::new();
    let mut roots = Vec::new();
    for (ordinal, (item, span)) in module.items.iter().enumerate() {
        let index = indices.item_index(&module.id, ordinal).unwrap_or_default();
        match item {
            Item::Function(function) if !function.visibility.is_pub() => {
                let owner = ctx.declaration_module_by_file_index.get(&index).copied();
                let occurrence = hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                    owner,
                    span,
                    ordinal,
                    hew_types::DeclarationKind::Function,
                    0,
                );
                if let Some(declaration) = ctx.defs.declaration(occurrence) {
                    private.insert(declaration, (ordinal, index, span));
                }
            }
            Item::Function(_) | Item::Impl(_) | Item::Trait(_) => roots.push((index, span)),
            Item::Actor(actor) if actor.visibility.is_pub() => roots.push((index, span)),
            _ => {}
        }
    }
    let mut reachable = HashSet::new();
    while let Some((index, span)) = roots.pop() {
        for (site, resolution) in &ctx.resolutions {
            if site.module_idx != index || site.start < span.start || site.end > span.end {
                continue;
            }
            let (Resolution::Def(declaration) | Resolution::Member(declaration)) = resolution
            else {
                continue;
            };
            if let Some((ordinal, index, span)) = private.get(declaration) {
                if reachable.insert(*ordinal) {
                    roots.push((*index, *span));
                }
            }
        }
    }
    reachable
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
