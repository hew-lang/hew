//! Import and module-identity helpers for file and package items.

use super::*;
use hew_parser::ast::Ident;

/// Identify, by PROVENANCE (file-set subsumption), the package-import graph
/// modules whose `impl` blocks must NOT be re-lowered by the fourth pass
/// because a *superset* package module already lowers the identical impl
/// items under the same unqualified `<SelfType>::<method>` symbols.
///
/// WHY this exists: a directory module (`import std::net::http` resolves to
/// `std/net/http/http.hew`, whose stem matches its directory, so it is a
/// *directory module* that peer-absorbs every sibling `.hew`) and an explicit
/// sub-file import of one of those peers (`import std::net::http::http_client`)
/// register as two distinct graph modules whose `source_paths` sets are
/// `{http_client.hew} ⊂ {http.hew, http_client.hew, …}`, so `lower_impl_block` runs on `http_client.hew`'s
/// `impl ResponseMethods for Response` TWICE — once per module — emitting
/// `Response::status` … `Response::free` under identical bare symbols. Two
/// `RawMirFunction`s with one name make codegen declare the LLVM function
/// twice; the first stays a bodiless internal-linkage declaration and
/// `Module::verify()` rejects it ("Global is external, but doesn't have
/// external or weak linkage!"). See #2391.
///
/// SEMANTICS: over imported modules (the root is excluded), treat each module's `source_paths` as a
/// canonical path set. Module M is *preferred for its own duplicate impls* iff
/// some N ≠ M has `files(M) ⊆ files(N)` and EITHER `files(M) ⊊ files(N)` (the
/// source-specific module owns impls duplicated in the directory superset) OR
/// the sets are equal and N precedes M in `topo_order` (a deterministic
/// tiebreak that keeps exactly one of two identical modules). Today's loader
/// only ever yields disjoint, equal, or single-file ⊂ directory sets (peer
/// absorption is a non-recursive `read_dir`), so partial overlap cannot arise;
/// if it ever did, both copies lower and the codegen duplicate-raw-symbol guard
/// catches the collision fail-closed rather than silently.
///
/// WHY only impl methods: pub free fns lower under module-qualified
/// `{module_short}$fn` names (both copies are needed — `http_client.get(...)`
/// binds `http_client$get`), consts under `{module_short}.NAME`, actors under
/// per-module layouts; only impl methods use the bare `<SelfType>::<method>`
/// family, so the subsumed skip is applied ONLY in the `Item::Impl` arm.
///
/// WHEN OBSOLETE / WHAT REPLACES IT: this provenance dedup disappears once
/// impl-method symbols become module-qualified (the foundational fix that also
/// unblocks two DISTINCT same-named types in one unit, e.g. http + websocket
/// each defining `Server`); tracked as the #2391 follow-up. Until then this
/// keeps the #2391 flagship shape (one program importing both `std::net::http`
/// and `std::net::http::http_client`) linking.
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
/// A directory import may contain both its same-leaf root file (`http.hew` for
/// `std.net.http`) and child files such as `http_client.hew`. The root-file
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
