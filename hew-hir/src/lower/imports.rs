//! Import and module-identity helpers for file and package items.

use super::*;
use hew_parser::ast::Ident;

/// Compute the source-set relationships used to choose one origin for
/// duplicate imported items. Module spelling alone cannot establish that
/// two declarations came from the same file.
///
/// A strict subset is recorded; for equal source sets, the later module in
/// topological order is recorded. Consumers combine these relationships with
/// item identity when deciding which copy to lower. The root and modules
/// without source provenance do not participate.
pub(super) fn preferred_package_module_ids(
    program: &Program,
) -> HashSet<hew_parser::module::ModulePath> {
    use std::path::{Path, PathBuf};

    let mut preferred = HashSet::new();
    let Some(mg) = &program.module_graph else {
        return preferred;
    };

    // Candidate = package module actually visited by the fourth pass. Build in
    // `topo_order` so each carries its position, giving the equal-set tiebreak
    // for free (earlier position survives). Modules absent from `mg.modules`
    // are never lowered, so they cannot subsume or be subsumed.
    let mut candidates: Vec<(&hew_parser::module::ModulePath, HashSet<&Path>, usize)> = Vec::new();
    for (pos, id) in mg.topo_order.iter().enumerate() {
        if *id == mg.root {
            continue;
        }
        let Some(module) = mg.modules.get(id) else {
            continue;
        };
        let files: HashSet<&Path> = module.source_paths.iter().map(PathBuf::as_path).collect();
        // A module with no source paths cannot meaningfully subset another;
        // leave it to lower normally.
        if files.is_empty() {
            continue;
        }
        candidates.push((id, files, pos));
    }

    for i in 0..candidates.len() {
        let (m_files, m_pos) = (&candidates[i].1, candidates[i].2);
        for j in 0..candidates.len() {
            if i == j {
                continue;
            }
            let (n_files, n_pos) = (&candidates[j].1, candidates[j].2);
            if !m_files.is_subset(n_files) {
                continue;
            }
            // files(M) ⊆ files(N). A strict subset is always subsumed; equal
            // sets subsume only the one later in topo order, so exactly one of
            // the pair survives.
            let equal = m_files.len() == n_files.len();
            if !equal || n_pos < m_pos {
                preferred.insert(candidates[i].0.clone());
                break;
            }
        }
    }
    preferred
}

pub(super) fn item_is_duplicated_in_preferred_module(
    program: &Program,
    preferred_modules: &HashSet<hew_parser::module::ModulePath>,
    current_module: &hew_parser::module::ModulePath,
    item: &Item,
) -> bool {
    if preferred_modules.contains(current_module) {
        return false;
    }
    let Some(module_graph) = &program.module_graph else {
        return false;
    };
    preferred_modules.iter().any(|module_id| {
        module_graph
            .modules
            .get(module_id)
            .is_some_and(|module| module.items.iter().any(|(candidate, _)| candidate == item))
    })
}

/// Whether a directory module absorbed an item from a distinct child module.
///
/// A directory import may contain both its same-leaf root file (`codec.hew` for
/// `std.net.http.codec`) and child files such as `request.hew`. The root-file
/// declarations retain the directory module's identity; only declarations
/// duplicated by a preferred module with a different leaf belong to a child
/// owner and must be skipped during directory-module function emission.
pub(super) fn item_is_duplicated_in_distinct_leaf_module(
    program: &Program,
    preferred_modules: &HashSet<hew_parser::module::ModulePath>,
    current_module: &hew_parser::module::ModulePath,
    item: &Item,
    item_span: &Span,
) -> bool {
    if preferred_modules.contains(current_module) {
        return false;
    }
    let Some(module_graph) = &program.module_graph else {
        return false;
    };
    let current_leaf = current_module.segments.last();
    let appears_in = |module_id: &hew_parser::module::ModulePath| {
        module_graph.modules.get(module_id).is_some_and(|module| {
            module
                .items
                .iter()
                .any(|(candidate, candidate_span)| candidate == item && candidate_span == item_span)
        })
    };
    if preferred_modules
        .iter()
        .any(|module_id| module_id.segments.last() == current_leaf && appears_in(module_id))
    {
        return false;
    }
    preferred_modules
        .iter()
        .any(|module_id| module_id.segments.last() != current_leaf && appears_in(module_id))
}

pub(super) fn item_declares_type_name(item: &Item, type_name: &str) -> bool {
    match item {
        Item::TypeDecl(decl) => decl.name == Ident::new(type_name),
        Item::Record(decl) => decl.name == Ident::new(type_name),
        _ => false,
    }
}

pub(super) fn imported_type_name_collides(
    program: &Program,
    preferred_modules: &HashSet<hew_parser::module::ModulePath>,
    type_name: &str,
) -> bool {
    let Some(module_graph) = &program.module_graph else {
        return false;
    };
    module_graph
        .modules
        .iter()
        .filter(|(module_id, module)| {
            // The root module is a declarant too: MIR's `collided_type_names`
            // counts distinct qualified identities across EVERY HirItem,
            // including root, so a root `Result` + one imported `pkg.Result`
            // is a genuine collision MIR keys `pkg.Result`. Excluding root here
            // left the checker/HIR value flow bare against that qualified
            // layout (#2208). Re-export subsumption still dedups a package
            // re-exporting a file module's item.
            module.items.iter().any(|(item, _)| {
                if !item_declares_type_name(item, type_name) {
                    return false;
                }
                if preferred_modules.contains(*module_id) {
                    return true;
                }
                !preferred_modules.iter().any(|preferred_id| {
                    module_graph
                        .modules
                        .get(preferred_id)
                        .is_some_and(|preferred| {
                            preferred
                                .items
                                .iter()
                                .any(|(candidate, _)| candidate == item)
                        })
                })
            })
        })
        .take(2)
        .count()
        > 1
}
