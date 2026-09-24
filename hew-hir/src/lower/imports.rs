//! Import and module-identity helpers for file and package items.

use super::*;

/// Map each *spliced file-import* entry in `program.items` to the `module_idx`
/// the checker stamped its `SpanKey` facts with, so HIR's `mk_key` lookups for
/// those items agree byte-for-byte with the checker.
///
/// File-path imports (`import "x.hew";`) are FLATTENED: after type-checking,
/// `flatten_file_import_items` (hew-compile) appends each file-import decl's
/// resolved items to the tail of `program.items` under their bare names so the
/// unqualified surface (`spawn Counter()`) lowers as a root item. But the
/// checker validated those same items during its `module_graph.topo_order`
/// walk, stamping their `SpanKey` facts with a non-root `current_module_idx`
/// (1-based, incremented per present module — see `Checker::check_program`).
/// If HIR lowers the spliced item at the default root index 0, every
/// `mk_key`-keyed lookup (actor-state guards, closure facts, conn/listener
/// await reads, range bounds, channel/stream rewrites, expr types, …) misses
/// the checker fact recorded at index N and the fail-closed contract fires
/// (e.g. `ActorStateGuardMissing`).
///
/// This reconstructs, deterministically and without a stored marker, which
/// tail entries are the flattened block and which non-root module each came
/// from, keyed by the per-item canonical source path (`resolved_item_source_paths`)
/// matched against each module's `source_paths`. The index assignment mirrors
/// `Checker::check_program` exactly: skip the root, bump a 1-based counter for
/// every module present in `modules`. Returns a map from `program.items` index
/// to module index; absent entries (genuine root items) are index 0.
pub(super) fn file_import_item_module_indices(program: &Program) -> HashMap<usize, u32> {
    let mut map = HashMap::new();
    let Some(mg) = &program.module_graph else {
        return map;
    };

    // Index by canonical source path, from the one allocation the checker
    // stamped `current_module_idx` with during its non-root body-check walk.
    let span_indices = mg.file_span_indices();

    // The spliced tail, from the same walk the frontend appended it with, so
    // each entry keeps its own declaring file's index.
    let appended: Vec<u32> = hew_parser::module::file_import_spliced_items(&program.items)
        .into_iter()
        .map(|(_, source)| {
            source
                .and_then(|path| span_indices.path_index(path))
                .unwrap_or_default()
        })
        .collect();

    let total = appended.len();
    if total == 0 || total > program.items.len() {
        return map;
    }
    let start = program.items.len() - total;
    for (offset, idx) in appended.into_iter().enumerate() {
        if idx != 0 {
            map.insert(start + offset, idx);
        }
    }
    map
}

/// Identify, by PROVENANCE, the module-graph modules whose items were spliced
/// into `program.items` by `flatten_file_import_items` (i.e. reached via a
/// file-path `import "x.hew";`).
///
/// The fourth pass (`lower_program_with_mono_cap`'s module-graph walk) visits
/// every module in `mg.topo_order`, which contains BOTH package-imported and
/// file-imported modules. File-import items are ALSO lowered by the
/// source-order third pass (they live in `program.items` after the splice), so
/// the fourth pass must skip a file-import module's impl blocks to avoid
/// double-lowering them into duplicate `<SelfType>::<method>` symbols.
///
/// The discriminator is the module's IDENTITY, not the bare type/trait name an
/// impl targets: file-import decls (`file_path.is_some()`) carry the canonical
/// `resolved_source_paths` of the files they pulled in; a graph module is
/// file-imported iff one of its `source_paths` is among them. Keying on origin
/// (rather than `"<type>:<trait>"`) is required for soundness: Hew deliberately
/// permits two DISTINCT modules to declare same-bare-named `pub type`s and
/// `impl Trait for T` (see the single semantic authority principle; e.g. std
/// http and websocket each define their own `Server`/`impl ServerMethods for
/// Server`). A bare-name skip would silently drop a package-import impl that
/// merely shares a name with a file-import/root impl; an origin skip cannot.
pub(super) fn file_import_module_ids(program: &Program) -> HashSet<hew_parser::module::ModuleId> {
    let mut ids = HashSet::new();
    let Some(mg) = &program.module_graph else {
        return ids;
    };

    // Canonical source paths contributed by the root's file-import chain: a
    // file the root imports may import a file in turn, and the splice carries
    // that whole chain into `program.items`.
    let file_import_paths = hew_parser::module::file_import_chain_sources(&program.items);
    if file_import_paths.is_empty() {
        return ids;
    }

    // A non-root module is file-imported iff any of its source files were
    // pulled in by a file-path import above. Package-imported modules never
    // appear in `file_import_paths`, so they are excluded and the fourth pass
    // emits their impls exactly once.
    for (mod_id, module) in &mg.modules {
        if *mod_id == mg.root {
            continue;
        }
        if module
            .source_paths
            .iter()
            .any(|p| file_import_paths.contains(p))
        {
            ids.insert(mod_id.clone());
        }
    }
    ids
}

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
/// `{http_client.hew} ⊂ {http.hew, http_client.hew, …}`. Both survive the
/// `file_import_module_ids` guard (both are PACKAGE imports, not file-path
/// imports), so `lower_impl_block` runs on `http_client.hew`'s
/// `impl ResponseMethods for Response` TWICE — once per module — emitting
/// `Response::status` … `Response::free` under identical bare symbols. Two
/// `RawMirFunction`s with one name make codegen declare the LLVM function
/// twice; the first stays a bodiless internal-linkage declaration and
/// `Module::verify()` rejects it ("Global is external, but doesn't have
/// external or weak linkage!"). See #2391.
///
/// SEMANTICS: over package modules only (the root and every
/// `file_import_modules` id are excluded — file-import doubles are already
/// handled by the splice-dedup guard), treat each module's `source_paths` as a
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
    file_import_modules: &HashSet<hew_parser::module::ModuleId>,
) -> HashSet<hew_parser::module::ModuleId> {
    use std::path::{Path, PathBuf};

    let mut preferred = HashSet::new();
    let Some(mg) = &program.module_graph else {
        return preferred;
    };

    // Candidate = package module actually visited by the fourth pass. Build in
    // `topo_order` so each carries its position, giving the equal-set tiebreak
    // for free (earlier position survives). Modules absent from `mg.modules`
    // are never lowered, so they cannot subsume or be subsumed.
    let mut candidates: Vec<(&hew_parser::module::ModuleId, HashSet<&Path>, usize)> = Vec::new();
    for (pos, id) in mg.topo_order.iter().enumerate() {
        if *id == mg.root || file_import_modules.contains(id) {
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
    preferred_modules: &HashSet<hew_parser::module::ModuleId>,
    current_module: &hew_parser::module::ModuleId,
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
    preferred_modules: &HashSet<hew_parser::module::ModuleId>,
    current_module: &hew_parser::module::ModuleId,
    item: &Item,
    item_span: &Span,
) -> bool {
    if preferred_modules.contains(current_module) {
        return false;
    }
    let Some(module_graph) = &program.module_graph else {
        return false;
    };
    let current_leaf = current_module.path.last();
    let appears_in = |module_id: &hew_parser::module::ModuleId| {
        module_graph.modules.get(module_id).is_some_and(|module| {
            module
                .items
                .iter()
                .any(|(candidate, candidate_span)| candidate == item && candidate_span == item_span)
        })
    };
    if preferred_modules
        .iter()
        .any(|module_id| module_id.path.last() == current_leaf && appears_in(module_id))
    {
        return false;
    }
    preferred_modules
        .iter()
        .any(|module_id| module_id.path.last() != current_leaf && appears_in(module_id))
}

pub(super) fn item_declares_type_name(item: &Item, type_name: &str) -> bool {
    match item {
        Item::TypeDecl(decl) => decl.name == type_name,
        Item::Record(decl) => decl.name == type_name,
        _ => false,
    }
}

pub(super) fn imported_type_name_collides(
    program: &Program,
    file_import_modules: &HashSet<hew_parser::module::ModuleId>,
    preferred_modules: &HashSet<hew_parser::module::ModuleId>,
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
            !file_import_modules.contains(*module_id)
                && module.items.iter().any(|(item, _)| {
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
