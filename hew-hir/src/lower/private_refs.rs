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
    test_entries: &[hew_types::TestEntryPlan],
) -> HashSet<usize> {
    let mut private = HashMap::new();
    let mut roots = Vec::new();
    let mut reachable = HashSet::new();
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
                    if test_entries.iter().any(|entry| entry.entry == declaration) {
                        reachable.insert(ordinal);
                        roots.push((index, span));
                    }
                }
            }
            // Every other item is emitted whatever its visibility: a private
            // actor reached through a public supervisor or spawning function
            // still runs its handlers, so a helper they call is live.
            _ => roots.push((index, span)),
        }
    }
    close_private_fn_uses(ctx, &private, roots, reachable)
}

/// Embedded source helpers share imported functions' exact declaration-use
/// closure, including function values and references inside closures.
pub(super) fn collect_builtin_private_fn_closure(
    ctx: &mut LowerCtx,
    program: &Program,
    builtin_item_index: impl Fn(usize) -> u32,
) -> HashSet<usize> {
    let mut private = HashMap::new();
    let mut roots = Vec::new();
    let saved_index = ctx.current_module_idx;
    let saved_ordinal = ctx.current_item_ordinal;
    for (ordinal, (item, span)) in program.items.iter().enumerate() {
        let index = builtin_item_index(ordinal);
        ctx.current_module_idx = index;
        ctx.current_item_ordinal = ordinal;
        match item {
            Item::Function(function) if !function.visibility.is_pub() => {
                if let Some(declaration) =
                    ctx.source_declaration(span, hew_types::DeclarationKind::Function, 0)
                {
                    private.insert(declaration, (ordinal, index, span));
                }
            }
            _ => roots.push((index, span)),
        }
    }
    ctx.current_module_idx = saved_index;
    ctx.current_item_ordinal = saved_ordinal;
    close_private_fn_uses(ctx, &private, roots, HashSet::new())
}

fn close_private_fn_uses<'a>(
    ctx: &LowerCtx,
    private: &HashMap<hew_types::DefId, (usize, u32, &'a Span)>,
    mut roots: Vec<(u32, &'a Span)>,
    mut reachable: HashSet<usize>,
) -> HashSet<usize> {
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
