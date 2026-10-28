//! Compile-time detection of actor reference cycles.
//!
//! Builds a directed graph of actor-to-actor references via actor-handle
//! fields and runs Tarjan's SCC algorithm to find strongly connected
//! components (cycles). Actors in cycles are returned as "cycle-capable"
//! so the runtime can selectively enable cycle scanning for them.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::check::{TypeDef, TypeDefKind, TypeDefView, VariantDef};
use crate::ty::Ty;
use crate::NominalId;

/// Result of cycle detection: the set of cycle-capable actor names and a list
/// of cycles (each cycle is a vec of actor names plus a representative span).
#[must_use]
pub fn detect_actor_ref_cycles(types: TypeDefView<'_>) -> (HashSet<String>, Vec<Vec<String>>) {
    let path = |id: NominalId| types.defs.path(id.declaration()).to_string();
    let mut actors: Vec<_> = types
        .type_defs
        .iter()
        .filter_map(|(&id, definition)| (definition.kind == TypeDefKind::Actor).then_some(id))
        .collect();
    if actors.is_empty() {
        return (HashSet::new(), Vec::new());
    }
    let mut summaries = actor_reference_summaries(types);
    let adj: HashMap<_, _> = actors
        .iter()
        .map(|id| (*id, summaries.remove(id).unwrap_or_default()))
        .collect();

    // Run Tarjan's SCC over a path-ordered node list so diagnostics are
    // deterministic.
    actors.sort_by_cached_key(|id| path(*id));
    let sccs = tarjan_scc(&actors, &adj);

    let mut cycle_capable = HashSet::new();
    let mut cycles = Vec::new();

    for scc in &sccs {
        let is_cycle = if scc.len() >= 2 {
            true
        } else if scc.len() == 1 {
            // Self-loop: actor references itself
            let id = scc[0];
            adj.get(&id).is_some_and(|refs| refs.contains(&id))
        } else {
            false
        };

        if is_cycle {
            let names: Vec<String> = scc.iter().map(|id| path(*id)).collect();
            cycle_capable.extend(names.iter().cloned());
            cycles.push(names);
        }
    }

    (cycle_capable, cycles)
}

fn stored_members(td: &TypeDef) -> Vec<&Ty> {
    let mut values: Vec<_> = td.fields.values().collect();
    for variant in td.variants.values() {
        match variant {
            VariantDef::Unit => {}
            VariantDef::Tuple(fields) => values.extend(fields),
            VariantDef::Struct(fields) => values.extend(fields.iter().map(|(_, ty)| ty)),
        }
    }
    values
}
#[derive(Default)]
struct StrongRcSummary {
    // A present entry records reachability; true means at least one path
    // crossed Rc storage. Parameter paths compose at an instantiated use.
    references: HashMap<NominalId, bool>,
    parameters: HashMap<crate::ParamHead, bool>,
}

fn join_rc_path<K: Eq + std::hash::Hash>(
    paths: &mut HashMap<K, bool>,
    key: K,
    strong: bool,
) -> bool {
    match paths.entry(key) {
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(strong);
            true
        }
        std::collections::hash_map::Entry::Occupied(mut entry) => {
            let changed = strong && !*entry.get();
            if changed {
                entry.insert(true);
            }
            changed
        }
    }
}

fn summarize_strong_rc(
    td: &TypeDef,
    types: TypeDefView<'_>,
    summaries: &HashMap<NominalId, StrongRcSummary>,
) -> (StrongRcSummary, HashSet<NominalId>) {
    let mut summary = StrongRcSummary::default();
    let mut dependencies = HashSet::new();
    let mut pending: Vec<_> = stored_members(td)
        .into_iter()
        .map(|ty| (ty, false))
        .collect();
    while let Some((ty, strong)) = pending.pop() {
        match ty {
            Ty::Slice(inner) | Ty::Array(inner, _) => pending.push((inner, strong)),
            Ty::Tuple(items) => pending.extend(items.iter().map(|ty| (ty, strong))),
            Ty::Named { head, args } => {
                if head.builtin() == Some(crate::BuiltinType::Weak) || head.is_actor() {
                    continue;
                }
                let strong = strong || head.builtin() == Some(crate::BuiltinType::Rc);
                if let crate::TypeHead::Param(parameter) = head {
                    join_rc_path(&mut summary.parameters, *parameter, strong);
                } else if let Some(id) = head.nominal() {
                    join_rc_path(&mut summary.references, id, strong);
                    dependencies.insert(id);
                    if let Some(target) = summaries.get(&id) {
                        for (&reference, &through_rc) in &target.references {
                            join_rc_path(&mut summary.references, reference, strong || through_rc);
                        }
                        if let Some(definition) = types.type_defs.get(&id) {
                            for (parameter, argument) in definition.type_params.iter().zip(args) {
                                if let Some(through_rc) = target.parameters.get(parameter) {
                                    pending.push((argument, strong || *through_rc));
                                }
                            }
                        }
                    }
                } else {
                    pending.extend(args.iter().map(|ty| (ty, strong)));
                }
            }
            _ => {}
        }
    }
    (summary, dependencies)
}

fn strong_rc_summaries(types: TypeDefView<'_>) -> HashMap<NominalId, StrongRcSummary> {
    let mut summaries: HashMap<_, _> = types
        .type_defs
        .keys()
        .map(|id| (*id, StrongRcSummary::default()))
        .collect();
    let mut dependents: HashMap<NominalId, HashSet<NominalId>> = HashMap::new();
    let mut pending: VecDeque<_> = types.type_defs.keys().copied().collect();
    let mut queued: HashSet<_> = pending.iter().copied().collect();
    while let Some(id) = pending.pop_front() {
        queued.remove(&id);
        let (incoming, dependencies) =
            summarize_strong_rc(&types.type_defs[&id], types, &summaries);
        for dependency in dependencies {
            dependents.entry(dependency).or_default().insert(id);
        }
        let summary = summaries
            .get_mut(&id)
            .expect("registered declaration summary");
        let mut changed = false;
        for (reference, strong) in incoming.references {
            changed |= join_rc_path(&mut summary.references, reference, strong);
        }
        for (parameter, strong) in incoming.parameters {
            changed |= join_rc_path(&mut summary.parameters, parameter, strong);
        }
        if changed {
            for dependent in dependents.get(&id).into_iter().flatten() {
                if queued.insert(*dependent) {
                    pending.push_back(*dependent);
                }
            }
        }
    }
    summaries
}

/// Strong self-reach through Rc storage. Weak backedges and actor handles
/// terminate a path. Declaration summaries retain both generic parameter use
/// and Rc strength until the dependency graph reaches its fixed point.
#[must_use]
pub fn detect_strong_rc_cycles(types: TypeDefView<'_>) -> Vec<Vec<String>> {
    let mut nodes: Vec<_> = types
        .type_defs
        .iter()
        .filter_map(|(&id, td)| {
            matches!(
                td.kind,
                TypeDefKind::Record | TypeDefKind::Struct | TypeDefKind::Enum
            )
            .then_some(id)
        })
        .collect();
    nodes.sort_by_key(|id| types.defs.path(id.declaration()));
    let summaries = strong_rc_summaries(types);
    let mut adj = HashMap::new();
    let mut rc_adj = HashMap::new();
    for &id in &nodes {
        let summary = &summaries[&id];
        adj.insert(id, summary.references.keys().copied().collect());
        rc_adj.insert(
            id,
            summary
                .references
                .iter()
                .filter_map(|(id, strong)| strong.then_some(*id))
                .collect::<HashSet<_>>(),
        );
    }
    tarjan_scc(&nodes, &adj)
        .into_iter()
        .filter(|group| {
            group.iter().any(|from| {
                rc_adj
                    .get(from)
                    .is_some_and(|refs| refs.iter().any(|to| group.contains(to)))
            })
        })
        .map(|group| {
            let mut paths: Vec<_> = group
                .iter()
                .map(|id| types.defs.path(id.declaration()).to_string())
                .collect();
            paths.sort();
            paths
        })
        .collect()
}

/// One value-typed field edge that participates in an infinitely-sized type
/// cycle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueTypeCycleEdge {
    /// Declaration whose field/variant payload contains `to` by value.
    pub from_id: NominalId,
    /// Path of `from_id`.
    pub from: String,
    /// Type reached by value from `from`.
    pub to: String,
    /// User-facing member description, e.g. `variant `Node`` or `field `next``.
    pub member_desc: String,
}

/// A recursive value-typed SCC plus the representative edge to diagnose.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueTypeCycle {
    /// All value types in the SCC, sorted for deterministic diagnostics.
    pub type_names: Vec<String>,
    /// Edge from one member of the SCC to another member of the SCC.
    pub edge: ValueTypeCycleEdge,
}

/// Detect infinitely-sized value types.
///
/// The graph contains non-indirect user value types (`enum`, `record`, and
/// layout-backed `struct`) and edges for field/payload containment by value.
/// Pointer/borrow/heap-handle containers break the chain; `Option`/`Result`,
/// tuples, arrays, and generic value-type wrappers are walked because they
/// store payloads inline.
#[must_use]
pub fn detect_recursive_value_type_cycles(types: TypeDefView<'_>) -> Vec<ValueTypeCycle> {
    let path = |id: NominalId| types.defs.path(id.declaration()).to_string();
    let mut adj: HashMap<NominalId, HashSet<NominalId>> = HashMap::new();
    let mut edges: HashMap<(NominalId, NominalId), ValueTypeCycleEdge> = HashMap::new();
    let mut direct_edges: HashSet<(NominalId, NominalId)> = HashSet::new();

    // Graph identity is the declaration; `TypeDef::name` is only the source
    // leaf, and same-leaf types from different modules stay distinct.
    let mut value_types: Vec<NominalId> = types
        .type_defs
        .iter()
        .filter(|(_, td)| is_value_type_node(td))
        .map(|(id, _)| *id)
        .collect();
    value_types.sort_by_cached_key(|id| path(*id));

    for &id in &value_types {
        let td = &types.type_defs[&id];
        let mut refs = HashSet::new();
        collect_value_type_edges_for_def(td, types, &mut refs, &mut edges, &mut direct_edges, id);
        adj.insert(id, refs);
    }

    let sccs = tarjan_scc(&value_types, &adj);
    let mut cycles = Vec::new();

    for scc in &sccs {
        let is_cycle = if scc.len() >= 2 {
            true
        } else if scc.len() == 1 {
            let id = scc[0];
            adj.get(&id).is_some_and(|refs| refs.contains(&id))
        } else {
            false
        };
        if !is_cycle {
            continue;
        }

        let mut members: Vec<NominalId> = scc.clone();
        members.sort_by_cached_key(|id| path(*id));
        let type_names: Vec<String> = members.iter().map(|id| path(*id)).collect();
        let scc_members: HashSet<NominalId> = scc.iter().copied().collect();

        for &from in &members {
            if let Some(edge) = representative_value_cycle_edge(
                types,
                from,
                &scc_members,
                &adj,
                &edges,
                &direct_edges,
            ) {
                cycles.push(ValueTypeCycle {
                    type_names: type_names.clone(),
                    edge,
                });
            }
        }
    }

    cycles.sort_by(|a, b| {
        a.edge
            .from
            .cmp(&b.edge.from)
            .then_with(|| a.edge.to.cmp(&b.edge.to))
    });
    cycles
}

fn representative_value_cycle_edge(
    types: TypeDefView<'_>,
    from: NominalId,
    scc_members: &HashSet<NominalId>,
    adj: &HashMap<NominalId, HashSet<NominalId>>,
    edges: &HashMap<(NominalId, NominalId), ValueTypeCycleEdge>,
    direct_edges: &HashSet<(NominalId, NominalId)>,
) -> Option<ValueTypeCycleEdge> {
    let mut targets: Vec<NominalId> = adj
        .get(&from)?
        .iter()
        .copied()
        .filter(|to| scc_members.contains(to))
        .collect();
    targets.sort_by_cached_key(|id| types.defs.path(id.declaration()).to_string());
    if let Some(edge) = targets.iter().find_map(|&to| {
        direct_edges
            .contains(&(from, to))
            .then(|| edges.get(&(from, to)).cloned())
            .flatten()
    }) {
        return Some(edge);
    }
    targets
        .into_iter()
        .find_map(|to| edges.get(&(from, to)).cloned())
}

fn collect_value_type_edges_for_def(
    td: &TypeDef,
    types: TypeDefView<'_>,
    refs: &mut HashSet<NominalId>,
    edges: &mut HashMap<(NominalId, NominalId), ValueTypeCycleEdge>,
    direct_edges: &mut HashSet<(NominalId, NominalId)>,
    def_key: NominalId,
) {
    match td.kind {
        TypeDefKind::Enum => {
            let mut variants: Vec<_> = td.variants.iter().collect();
            variants.sort_by_key(|(name, _)| *name);
            for (variant_name, variant) in variants {
                match variant {
                    VariantDef::Unit => {}
                    VariantDef::Tuple(fields) => {
                        let member_desc = format!("variant `{variant_name}`");
                        for field_ty in fields {
                            collect_value_type_refs(
                                field_ty,
                                types,
                                refs,
                                edges,
                                direct_edges,
                                def_key,
                                &member_desc,
                                true,
                                &mut HashSet::new(),
                                &mut HashSet::new(),
                            );
                        }
                    }
                    VariantDef::Struct(fields) => {
                        let mut fields = fields.iter().collect::<Vec<_>>();
                        fields.sort_by_key(|(name, _)| name.clone());
                        for (field_name, field_ty) in fields {
                            let member_desc =
                                format!("variant `{variant_name}` field `{field_name}`");
                            collect_value_type_refs(
                                field_ty,
                                types,
                                refs,
                                edges,
                                direct_edges,
                                def_key,
                                &member_desc,
                                true,
                                &mut HashSet::new(),
                                &mut HashSet::new(),
                            );
                        }
                    }
                }
            }
        }
        TypeDefKind::Struct | TypeDefKind::Record => {
            for (field_name, field_ty) in ordered_type_fields(td) {
                let member_desc = format!("field `{field_name}`");
                collect_value_type_refs(
                    field_ty,
                    types,
                    refs,
                    edges,
                    direct_edges,
                    def_key,
                    &member_desc,
                    true,
                    &mut HashSet::new(),
                    &mut HashSet::new(),
                );
            }
        }
        TypeDefKind::Actor | TypeDefKind::Supervisor | TypeDefKind::Machine => {}
    }
}

fn ordered_type_fields(td: &TypeDef) -> Vec<(&str, &Ty)> {
    if td.field_order.is_empty() {
        let mut fields = td
            .fields
            .iter()
            .map(|(name, ty)| (name.as_str(), ty))
            .collect::<Vec<_>>();
        fields.sort_by_key(|(name, _)| *name);
        return fields;
    }

    td.field_order
        .iter()
        .filter_map(|name| td.fields.get(name).map(|ty| (name.as_str(), ty)))
        .collect()
}

#[expect(
    clippy::too_many_arguments,
    reason = "recursive graph walk carries shared edge collection state"
)]
fn collect_value_type_refs(
    ty: &Ty,
    types: TypeDefView<'_>,
    out: &mut HashSet<NominalId>,
    edges: &mut HashMap<(NominalId, NominalId), ValueTypeCycleEdge>,
    direct_edges: &mut HashSet<(NominalId, NominalId)>,
    from: NominalId,
    member_desc: &str,
    is_direct_edge: bool,
    visited_value_types: &mut HashSet<(NominalId, Vec<Ty>)>,
    active: &mut HashSet<NominalId>,
) {
    match ty {
        Ty::Tuple(elems) => {
            for elem in elems {
                collect_value_type_refs(
                    elem,
                    types,
                    out,
                    edges,
                    direct_edges,
                    from,
                    member_desc,
                    is_direct_edge,
                    visited_value_types,
                    active,
                );
            }
        }
        Ty::Array(inner, len) if *len != 0 => {
            collect_value_type_refs(
                inner,
                types,
                out,
                edges,
                direct_edges,
                from,
                member_desc,
                is_direct_edge,
                visited_value_types,
                active,
            );
        }
        Ty::Named { head, args } => match head.builtin() {
            Some(crate::BuiltinType::Option | crate::BuiltinType::Result) => {
                for arg in args {
                    collect_value_type_refs(
                        arg,
                        types,
                        out,
                        edges,
                        direct_edges,
                        from,
                        member_desc,
                        is_direct_edge,
                        visited_value_types,
                        active,
                    );
                }
            }
            Some(_) => {}
            None => {
                let Some(target) = head.nominal() else {
                    return;
                };
                let Some(target_def) = types.type_defs.get(&target) else {
                    return;
                };
                if !is_value_type_node(target_def) {
                    return;
                }

                out.insert(target);
                let edge_key = (from, target);
                if is_direct_edge {
                    direct_edges.insert(edge_key);
                }
                edges.entry(edge_key).or_insert_with(|| ValueTypeCycleEdge {
                    from_id: from,
                    from: types.defs.path(from.declaration()).to_string(),
                    to: types.defs.path(target.declaration()).to_string(),
                    member_desc: member_desc.to_string(),
                });

                let key = (target, args.clone());
                if !visited_value_types.insert(key) {
                    return;
                }
                // Bound the generic-instantiation expansion by type name, not by
                // the `(name, args)` key. A diverging polymorphic self-reference
                // — `Wrap<T> { w: Wrap<Wrap<T>> }` — substitutes to a strictly
                // larger arg vector at every level (`Wrap<i64>` →
                // `Wrap<Wrap<i64>>` → …), so the `(name, args)` set never
                // collides and the walk expands without bound (OOM). The edge
                // `from → target_def.name` is already recorded above, so once a
                // type name is active on the current expansion chain, descending
                // into its fields again only re-derives edges the outer frame
                // already owns. Skipping that descent keeps the SCC graph
                // complete while making the walk terminate.
                if !active.insert(target) {
                    return;
                }
                collect_instantiated_value_type_fields(
                    target_def,
                    args,
                    types,
                    out,
                    edges,
                    direct_edges,
                    from,
                    member_desc,
                    visited_value_types,
                    active,
                );
                active.remove(&target);
            }
        },
        _ => {}
    }
}

#[expect(
    clippy::too_many_arguments,
    reason = "recursive graph walk carries shared edge collection state"
)]
fn collect_instantiated_value_type_fields(
    td: &TypeDef,
    args: &[Ty],
    types: TypeDefView<'_>,
    out: &mut HashSet<NominalId>,
    edges: &mut HashMap<(NominalId, NominalId), ValueTypeCycleEdge>,
    direct_edges: &mut HashSet<(NominalId, NominalId)>,
    from: NominalId,
    member_desc: &str,
    visited_value_types: &mut HashSet<(NominalId, Vec<Ty>)>,
    active: &mut HashSet<NominalId>,
) {
    match td.kind {
        TypeDefKind::Enum => {
            for variant in td.variants.values() {
                match variant {
                    VariantDef::Unit => {}
                    VariantDef::Tuple(fields) => {
                        for field_ty in fields {
                            let instantiated = instantiate_value_field_ty(field_ty, td, args);
                            collect_value_type_refs(
                                &instantiated,
                                types,
                                out,
                                edges,
                                direct_edges,
                                from,
                                member_desc,
                                false,
                                visited_value_types,
                                active,
                            );
                        }
                    }
                    VariantDef::Struct(fields) => {
                        for (_, field_ty) in fields {
                            let instantiated = instantiate_value_field_ty(field_ty, td, args);
                            collect_value_type_refs(
                                &instantiated,
                                types,
                                out,
                                edges,
                                direct_edges,
                                from,
                                member_desc,
                                false,
                                visited_value_types,
                                active,
                            );
                        }
                    }
                }
            }
        }
        TypeDefKind::Struct | TypeDefKind::Record => {
            for (_, field_ty) in ordered_type_fields(td) {
                let instantiated = instantiate_value_field_ty(field_ty, td, args);
                collect_value_type_refs(
                    &instantiated,
                    types,
                    out,
                    edges,
                    direct_edges,
                    from,
                    member_desc,
                    false,
                    visited_value_types,
                    active,
                );
            }
        }
        TypeDefKind::Actor | TypeDefKind::Supervisor | TypeDefKind::Machine => {}
    }
}

fn instantiate_value_field_ty(field_ty: &Ty, td: &TypeDef, args: &[Ty]) -> Ty {
    let map: HashMap<crate::ParamHead, Ty> = td
        .type_params
        .iter()
        .zip(args.iter())
        .map(|(p, a)| (*p, a.clone()))
        .collect();
    field_ty.substitute_type_params_parallel(&map)
}

fn is_value_type_node(td: &TypeDef) -> bool {
    !td.is_indirect
        && matches!(
            td.kind,
            TypeDefKind::Struct | TypeDefKind::Enum | TypeDefKind::Record
        )
}

/// Summarize the finite declaration graph before answering actor field queries.
/// All non-actor type arguments participate in discovery, including phantom
/// arguments. Record fields and enum payloads add declaration dependencies;
/// substituting generic arguments into them would repeat discovery and can
/// create unbounded types.
fn actor_reference_summaries(types: TypeDefView<'_>) -> HashMap<NominalId, HashSet<NominalId>> {
    let mut summaries = HashMap::new();
    let mut dependents: HashMap<NominalId, HashSet<NominalId>> = HashMap::new();
    let mut pending = VecDeque::new();
    for (&id, definition) in types.type_defs {
        if !matches!(
            definition.kind,
            TypeDefKind::Actor | TypeDefKind::Struct | TypeDefKind::Record | TypeDefKind::Enum
        ) {
            continue;
        }
        let mut actors = HashSet::new();
        let mut dependencies = HashSet::new();
        for member in stored_members(definition) {
            collect_actor_reference_edges(member, types, &mut actors, &mut dependencies);
        }
        for actor in &actors {
            pending.push_back((id, *actor));
        }
        for dependency in dependencies {
            dependents.entry(dependency).or_default().insert(id);
        }
        summaries.insert(id, actors);
    }
    // Every (declaration, actor) fact crosses each dependency edge once. SCCs
    // retain all reachable handles without recursive or instantiated expansion.
    while let Some((id, actor)) = pending.pop_front() {
        for dependent in dependents.get(&id).into_iter().flatten() {
            if summaries
                .get_mut(dependent)
                .expect("registered reference summary")
                .insert(actor)
            {
                pending.push_back((*dependent, actor));
            }
        }
    }
    summaries
}

fn collect_actor_reference_edges(
    ty: &Ty,
    types: TypeDefView<'_>,
    actors: &mut HashSet<NominalId>,
    dependencies: &mut HashSet<NominalId>,
) {
    let mut pending = vec![ty];
    while let Some(ty) = pending.pop() {
        match ty {
            Ty::Slice(inner) | Ty::Array(inner, _) => pending.push(inner),
            Ty::Tuple(items) => pending.extend(items),
            Ty::Named { head, args } => {
                if head.is_actor() {
                    if let Some(actor) =
                        head.nominal().filter(|id| types.type_defs.contains_key(id))
                    {
                        actors.insert(actor);
                    }
                    continue;
                }
                if let Some(value) = head.nominal().filter(|id| {
                    types.type_defs.get(id).is_some_and(|definition| {
                        matches!(
                            definition.kind,
                            TypeDefKind::Struct | TypeDefKind::Record | TypeDefKind::Enum
                        )
                    })
                }) {
                    dependencies.insert(value);
                }
                pending.extend(args);
            }
            _ => {}
        }
    }
}

/// Tarjan's strongly connected components algorithm.
///
/// Returns SCCs in reverse topological order (leaf SCCs first).
pub(crate) fn tarjan_scc<K: Copy + Eq + std::hash::Hash>(
    nodes: &[K],
    adj: &HashMap<K, HashSet<K>>,
) -> Vec<Vec<K>> {
    struct State<K> {
        index_counter: usize,
        stack: Vec<K>,
        on_stack: HashSet<K>,
        index: HashMap<K, usize>,
        lowlink: HashMap<K, usize>,
        result: Vec<Vec<K>>,
    }

    fn strongconnect<K: Copy + Eq + std::hash::Hash>(
        v: K,
        adj: &HashMap<K, HashSet<K>>,
        state: &mut State<K>,
    ) {
        state.index.insert(v, state.index_counter);
        state.lowlink.insert(v, state.index_counter);
        state.index_counter += 1;
        state.stack.push(v);
        state.on_stack.insert(v);

        if let Some(neighbours) = adj.get(&v) {
            for &w in neighbours {
                if !state.index.contains_key(&w) {
                    strongconnect(w, adj, state);
                    let w_low = state.lowlink[&w];
                    let v_low = state.lowlink[&v];
                    if w_low < v_low {
                        state.lowlink.insert(v, w_low);
                    }
                } else if state.on_stack.contains(&w) {
                    let w_idx = state.index[&w];
                    let v_low = state.lowlink[&v];
                    if w_idx < v_low {
                        state.lowlink.insert(v, w_idx);
                    }
                }
            }
        }

        if state.lowlink[&v] == state.index[&v] {
            let mut scc = Vec::new();
            loop {
                let w = state.stack.pop().expect("stack should not be empty");
                state.on_stack.remove(&w);
                scc.push(w);
                if w == v {
                    break;
                }
            }
            state.result.push(scc);
        }
    }

    let mut state = State {
        index_counter: 0,
        stack: Vec::new(),
        on_stack: HashSet::new(),
        index: HashMap::new(),
        lowlink: HashMap::new(),
        result: Vec::new(),
    };

    for &node in nodes {
        if !state.index.contains_key(&node) {
            strongconnect(node, adj, &mut state);
        }
    }

    state.result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::DefTable;

    fn actor_cycles(
        type_defs: &HashMap<NominalId, TypeDef>,
    ) -> (HashSet<String>, Vec<Vec<String>>) {
        let defs = DefTable::fixture();
        detect_actor_ref_cycles(TypeDefView::new(&defs, type_defs))
    }

    fn value_cycles(type_defs: &HashMap<NominalId, TypeDef>) -> Vec<ValueTypeCycle> {
        let defs = DefTable::fixture();
        detect_recursive_value_type_cycles(TypeDefView::new(&defs, type_defs))
    }

    #[test]
    fn actor_reference_summaries_share_a_deep_record_dag() {
        let mut definitions = HashMap::from([
            make_struct(
                "T0",
                HashMap::from([("peer".to_string(), actor_handle("B"))]),
            ),
            make_actor(
                "A",
                HashMap::from([("value".to_string(), named_type("T28"))]),
            ),
            make_actor(
                "B",
                HashMap::from([("peer".to_string(), actor_handle("A"))]),
            ),
        ]);
        for level in 1..=28 {
            let child = Ty::builtin_named(
                crate::BuiltinType::Vec,
                vec![named_type(&format!("T{}", level - 1))],
            );
            let (id, definition) = make_struct(
                &format!("T{level}"),
                HashMap::from([
                    ("left".to_string(), child.clone()),
                    ("right".to_string(), child),
                ]),
            );
            definitions.insert(id, definition);
        }
        let (capable, cycles) = actor_cycles(&definitions);
        assert_eq!(capable, HashSet::from(["A".to_string(), "B".to_string()]));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn actor_reference_summaries_terminate_growing_generic_recursion() {
        let parameter = crate::ParamHead::for_test("T");
        let definitions = HashMap::from([
            make_record(
                "Grow",
                vec![parameter],
                HashMap::from([
                    (
                        "next".to_string(),
                        Ty::builtin_named(
                            crate::BuiltinType::Vec,
                            vec![Ty::named_for_test(
                                "Grow",
                                vec![Ty::builtin_named(
                                    crate::BuiltinType::Vec,
                                    vec![Ty::param(parameter)],
                                )],
                            )],
                        ),
                    ),
                    ("peer".to_string(), actor_handle("B")),
                ]),
            ),
            make_actor(
                "A",
                HashMap::from([(
                    "value".to_string(),
                    Ty::named_for_test("Grow", vec![Ty::I64]),
                )]),
            ),
            make_actor(
                "B",
                HashMap::from([("peer".to_string(), actor_handle("A"))]),
            ),
        ]);
        let (capable, cycles) = actor_cycles(&definitions);
        assert_eq!(capable, HashSet::from(["A".to_string(), "B".to_string()]));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn actor_reference_summaries_scan_nested_phantom_arguments() {
        let parameter = crate::ParamHead::for_test("T");
        let definitions = HashMap::from([
            make_record("Phantom", vec![parameter], HashMap::new()),
            make_actor(
                "A",
                HashMap::from([(
                    "value".to_string(),
                    Ty::named_for_test(
                        "Phantom",
                        vec![Ty::option(Ty::Tuple(vec![
                            Ty::builtin_named(crate::BuiltinType::Vec, vec![actor_handle("B")]),
                            Ty::Pointer {
                                is_mutable: false,
                                pointee: Box::new(actor_handle("C")),
                            },
                        ]))],
                    ),
                )]),
            ),
            make_actor(
                "B",
                HashMap::from([("peer".to_string(), actor_handle("A"))]),
            ),
            make_actor(
                "C",
                HashMap::from([("peer".to_string(), actor_handle("A"))]),
            ),
        ]);
        let (capable, cycles) = actor_cycles(&definitions);
        assert_eq!(capable, HashSet::from(["A".to_string(), "B".to_string()]));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn actor_reference_summaries_follow_generic_enum_payloads() {
        let parameter = crate::ParamHead::for_test("T");
        let payload_parameter = crate::ParamHead::for_test("P");
        let mut payload = make_enum(
            "Payload",
            HashMap::from([
                (
                    "Tuple".to_string(),
                    VariantDef::Tuple(vec![Ty::named_for_test(
                        "Wrapper",
                        vec![Ty::option(Ty::param(payload_parameter))],
                    )]),
                ),
                (
                    "Named".to_string(),
                    VariantDef::Struct(vec![("peer".to_string(), actor_handle("C"))]),
                ),
            ]),
        );
        payload.1.type_params.push(payload_parameter);
        let definitions = HashMap::from([
            make_record(
                "Wrapper",
                vec![parameter],
                HashMap::from([
                    ("value".to_string(), Ty::param(parameter)),
                    ("peer".to_string(), actor_handle("B")),
                ]),
            ),
            payload,
            make_actor(
                "A",
                HashMap::from([(
                    "payload".to_string(),
                    Ty::named_for_test("Payload", vec![Ty::I64]),
                )]),
            ),
            make_actor(
                "B",
                HashMap::from([("peer".to_string(), actor_handle("A"))]),
            ),
            make_actor(
                "C",
                HashMap::from([("peer".to_string(), actor_handle("A"))]),
            ),
        ]);
        let (capable, cycles) = actor_cycles(&definitions);
        assert_eq!(
            capable,
            HashSet::from(["A".to_string(), "B".to_string(), "C".to_string()])
        );
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn actor_reference_summaries_stop_at_handles_and_borrowed_values() {
        let parameter = crate::ParamHead::for_test("T");
        let mut actor_b = make_actor("B", HashMap::new());
        actor_b.1.type_params.push(parameter);
        let definitions = HashMap::from([
            actor_b,
            make_actor(
                "A",
                HashMap::from([
                    (
                        "handle".to_string(),
                        Ty::actor_for_test("B", vec![actor_handle("C")]),
                    ),
                    (
                        "borrowed".to_string(),
                        Ty::Borrow {
                            pointee: Box::new(actor_handle("C")),
                        },
                    ),
                    (
                        "pointer".to_string(),
                        Ty::Pointer {
                            is_mutable: false,
                            pointee: Box::new(actor_handle("C")),
                        },
                    ),
                    (
                        "callback".to_string(),
                        Ty::Function {
                            capabilities: crate::CallableCapabilities::default(),
                            params: vec![crate::CallableParameter::borrowed(actor_handle("C"))],
                            ret: Box::new(actor_handle("C")),
                        },
                    ),
                ]),
            ),
            make_actor(
                "C",
                HashMap::from([("peer".to_string(), actor_handle("A"))]),
            ),
        ]);
        let (capable, cycles) = actor_cycles(&definitions);
        assert!(capable.is_empty());
        assert!(cycles.is_empty());
    }

    fn strong_cycles(type_defs: &HashMap<NominalId, TypeDef>) -> Vec<Vec<String>> {
        let defs = DefTable::fixture();
        detect_strong_rc_cycles(TypeDefView::new(&defs, type_defs))
    }

    #[test]
    fn strong_rc_shared_member_graph_retains_completed_reachability() {
        let mut definitions = HashMap::from([make_struct(
            "T0",
            HashMap::from([("value".to_string(), Ty::I64)]),
        )]);
        for level in 1..=28 {
            let child = Ty::builtin_named(
                crate::BuiltinType::Vec,
                vec![named_type(&format!("T{}", level - 1))],
            );
            let (id, definition) = make_struct(
                &format!("T{level}"),
                HashMap::from([
                    ("left".to_string(), child.clone()),
                    ("right".to_string(), child),
                ]),
            );
            definitions.insert(id, definition);
        }
        let defs = DefTable::fixture();
        let summaries = strong_rc_summaries(TypeDefView::new(&defs, &definitions));
        let root = &summaries[&NominalId::for_test("T28")];
        assert_eq!(root.references.len(), 28);
        assert!(root.references.values().all(|strong| !strong));
        assert!(strong_cycles(&definitions).is_empty());
    }

    #[test]
    fn strong_rc_shared_member_source_graph_is_acyclic() {
        use std::fmt::Write as _;

        let mut source = String::from("type T0 { value: i64; }\n");
        for level in 1..=28 {
            writeln!(
                source,
                "type T{level} {{ left: Vec<T{previous}>; right: Vec<T{previous}>; }}",
                previous = level - 1
            )
            .unwrap();
        }
        source.push_str("fn main() {}\n");
        let parsed = hew_parser::parse(&source);
        assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
        let output = crate::Checker::new(crate::module_registry::ModuleRegistry::new(vec![]))
            .check_program(&parsed.program);
        assert!(output.errors.is_empty(), "{:#?}", output.errors);
        assert!(
            !output
                .warnings
                .iter()
                .any(|warning| warning.kind == crate::error::TypeErrorKind::StrongRcCycle),
            "{:#?}",
            output.warnings
        );
    }

    #[test]
    fn strong_rc_cycle_preserves_storage_strength_and_breaks() {
        let a = make_struct(
            "A",
            HashMap::from([(
                "next".to_string(),
                Ty::builtin_named(crate::BuiltinType::Rc, vec![named_type("B")]),
            )]),
        );
        let b = make_struct(
            "B",
            HashMap::from([(
                "next".to_string(),
                Ty::builtin_named(crate::BuiltinType::Vec, vec![named_type("A")]),
            )]),
        );
        assert_eq!(
            strong_cycles(&HashMap::from([a.clone(), b])),
            vec![vec!["A".to_string(), "B".to_string()]]
        );
        for backedge in [
            Ty::builtin_named(crate::BuiltinType::Weak, vec![named_type("A")]),
            Ty::Pointer {
                is_mutable: false,
                pointee: Box::new(named_type("A")),
            },
        ] {
            let b = make_struct("B", HashMap::from([("next".to_string(), backedge)]));
            assert!(strong_cycles(&HashMap::from([a.clone(), b])).is_empty());
        }
        let definitions = HashMap::from([
            make_struct(
                "A",
                HashMap::from([("next".to_string(), actor_handle("B"))]),
            ),
            make_actor(
                "B",
                HashMap::from([(
                    "next".to_string(),
                    Ty::builtin_named(crate::BuiltinType::Rc, vec![named_type("A")]),
                )]),
            ),
        ]);
        assert!(strong_cycles(&definitions).is_empty());
    }

    #[test]
    fn strong_rc_generic_summaries_preserve_used_and_phantom_arguments() {
        let parameter = crate::ParamHead::for_test("T");
        for (field, cycle) in [
            (
                Ty::builtin_named(crate::BuiltinType::Rc, vec![Ty::param(parameter)]),
                true,
            ),
            (
                Ty::builtin_named(crate::BuiltinType::Weak, vec![Ty::param(parameter)]),
                false,
            ),
            (Ty::I64, false),
        ] {
            let definitions = HashMap::from([
                make_record(
                    "Wrapper",
                    vec![parameter],
                    HashMap::from([("value".to_string(), field)]),
                ),
                make_struct(
                    "Owner",
                    HashMap::from([(
                        "wrapped".to_string(),
                        Ty::named_for_test("Wrapper", vec![named_type("Owner")]),
                    )]),
                ),
            ]);
            assert_eq!(
                strong_cycles(&definitions),
                if cycle {
                    vec![vec!["Owner".to_string()]]
                } else {
                    Vec::new()
                }
            );
        }
        let definitions = HashMap::from([
            make_record(
                "Wrapper",
                vec![parameter],
                HashMap::from([("value".to_string(), Ty::param(parameter))]),
            ),
            make_struct(
                "Owner",
                HashMap::from([(
                    "wrapped".to_string(),
                    Ty::Tuple(vec![
                        Ty::named_for_test(
                            "Wrapper",
                            vec![Ty::builtin_named(
                                crate::BuiltinType::Weak,
                                vec![named_type("Owner")],
                            )],
                        ),
                        Ty::named_for_test(
                            "Wrapper",
                            vec![Ty::builtin_named(
                                crate::BuiltinType::Rc,
                                vec![named_type("Owner")],
                            )],
                        ),
                    ]),
                )]),
            ),
        ]);
        assert_eq!(strong_cycles(&definitions), vec![vec!["Owner".to_string()]]);
    }

    #[test]
    fn strong_rc_generic_recursion_uses_finite_parameter_summaries() {
        let parameter = crate::ParamHead::for_test("T");
        let definitions = HashMap::from([
            make_record(
                "Grow",
                vec![parameter],
                HashMap::from([
                    (
                        "value".to_string(),
                        Ty::builtin_named(crate::BuiltinType::Rc, vec![Ty::param(parameter)]),
                    ),
                    (
                        "next".to_string(),
                        Ty::builtin_named(
                            crate::BuiltinType::Vec,
                            vec![Ty::named_for_test(
                                "Grow",
                                vec![Ty::builtin_named(
                                    crate::BuiltinType::Vec,
                                    vec![Ty::param(parameter)],
                                )],
                            )],
                        ),
                    ),
                ]),
            ),
            make_struct(
                "Owner",
                HashMap::from([(
                    "grow".to_string(),
                    Ty::named_for_test("Grow", vec![named_type("Owner")]),
                )]),
            ),
        ]);
        assert_eq!(strong_cycles(&definitions), vec![vec!["Owner".to_string()]]);
    }

    fn make_actor(name: &str, fields: HashMap<String, Ty>) -> (NominalId, TypeDef) {
        (
            NominalId::for_test(name),
            TypeDef {
                kind: TypeDefKind::Actor,
                name: name.to_string(),
                type_params: vec![],
                bounds: crate::check::ParamBounds::default(),
                fields,
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        )
    }

    fn make_struct(name: &str, fields: HashMap<String, Ty>) -> (NominalId, TypeDef) {
        (
            NominalId::for_test(name),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: name.to_string(),
                type_params: vec![],
                bounds: crate::check::ParamBounds::default(),
                fields,
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        )
    }

    fn actor_handle(name: &str) -> Ty {
        Ty::actor_for_test(name, Vec::new())
    }

    fn named_type(name: &str) -> Ty {
        Ty::named_for_test(name, vec![])
    }

    fn make_enum(name: &str, variants: HashMap<String, VariantDef>) -> (NominalId, TypeDef) {
        (
            NominalId::for_test(name),
            TypeDef {
                kind: TypeDefKind::Enum,
                name: name.to_string(),
                type_params: vec![],
                bounds: crate::check::ParamBounds::default(),
                fields: HashMap::new(),
                variants,
                implicit_variant_fields: HashMap::default(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        )
    }

    fn make_record(
        name: &str,
        type_params: Vec<crate::ParamHead>,
        fields: HashMap<String, Ty>,
    ) -> (NominalId, TypeDef) {
        (
            NominalId::for_test(name),
            TypeDef {
                kind: TypeDefKind::Record,
                name: name.to_string(),
                type_params,
                bounds: crate::check::ParamBounds::default(),
                fields,
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        )
    }

    #[test]
    fn no_actors() {
        let type_defs = HashMap::new();
        let (capable, cycles) = actor_cycles(&type_defs);
        assert!(capable.is_empty());
        assert!(cycles.is_empty());
    }

    #[test]
    fn no_cycles_linear() {
        // A -> B -> C (no cycle)
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor("A", HashMap::from([("b".to_string(), actor_handle("B"))])),
            make_actor("B", HashMap::from([("c".to_string(), actor_handle("C"))])),
            make_actor("C", HashMap::from([("x".to_string(), Ty::I32)])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);
        assert!(capable.is_empty());
        assert!(cycles.is_empty());
    }

    #[test]
    fn recursive_value_type_detects_self_recursive_enum() {
        let type_defs: HashMap<NominalId, TypeDef> = [make_enum(
            "Tree",
            HashMap::from([
                ("Leaf".to_string(), VariantDef::Unit),
                (
                    "Node".to_string(),
                    VariantDef::Tuple(vec![Ty::I64, named_type("Tree"), named_type("Tree")]),
                ),
            ]),
        )]
        .into_iter()
        .collect();

        let cycles = value_cycles(&type_defs);

        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0].edge.from, "Tree");
        assert_eq!(cycles[0].edge.to, "Tree");
        assert_eq!(cycles[0].edge.member_desc, "variant `Node`");
    }

    #[test]
    fn qualified_table_keys_need_not_match_source_local_type_names() {
        let (_, crash_kind) = make_enum(
            "CrashKind",
            HashMap::from([("Crashed".to_string(), VariantDef::Unit)]),
        );
        let (_, down_reason) = make_enum(
            "DownReason",
            HashMap::from([(
                "Crashed".to_string(),
                VariantDef::Tuple(vec![named_type("failure.CrashKind")]),
            )]),
        );
        let type_defs = HashMap::from([
            (NominalId::for_test("failure.CrashKind"), crash_kind),
            (NominalId::for_test("link_monitor.DownReason"), down_reason),
        ]);

        let cycles = value_cycles(&type_defs);
        assert!(
            cycles.is_empty(),
            "an imported enum carrying a canonical lifecycle enum is acyclic: {cycles:?}"
        );
    }

    #[test]
    fn recursive_value_type_detects_mutual_enum_cycle() {
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_enum(
                "A",
                HashMap::from([("A1".to_string(), VariantDef::Tuple(vec![named_type("B")]))]),
            ),
            make_enum(
                "B",
                HashMap::from([("B1".to_string(), VariantDef::Tuple(vec![named_type("A")]))]),
            ),
        ]
        .into_iter()
        .collect();

        let cycles = value_cycles(&type_defs);
        let cycle_froms: HashSet<_> = cycles
            .iter()
            .map(|cycle| cycle.edge.from.as_str())
            .collect();

        assert_eq!(cycles.len(), 2);
        assert!(cycle_froms.contains("A"));
        assert!(cycle_froms.contains("B"));
    }

    #[test]
    fn recursive_value_type_allows_nested_non_recursive_enum() {
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_enum(
                "Inner",
                HashMap::from([
                    ("A".to_string(), VariantDef::Unit),
                    ("B".to_string(), VariantDef::Tuple(vec![Ty::I64])),
                ]),
            ),
            make_enum(
                "Outer",
                HashMap::from([(
                    "D".to_string(),
                    VariantDef::Tuple(vec![named_type("Inner")]),
                )]),
            ),
        ]
        .into_iter()
        .collect();

        let cycles = value_cycles(&type_defs);

        assert!(cycles.is_empty());
    }

    #[test]
    fn recursive_value_type_detects_generic_value_wrapper_cycle() {
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_record(
                "Wrapper",
                vec![crate::ParamHead::for_test("T")],
                HashMap::from([(
                    "value".to_string(),
                    Ty::param(crate::ParamHead::for_test("T")),
                )]),
            ),
            make_enum(
                "Tree",
                HashMap::from([(
                    "Node".to_string(),
                    VariantDef::Tuple(vec![Ty::named_for_test(
                        "Wrapper",
                        vec![named_type("Tree")],
                    )]),
                )]),
            ),
        ]
        .into_iter()
        .collect();

        let cycles = value_cycles(&type_defs);

        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0].edge.from, "Tree");
        assert_eq!(cycles[0].edge.to, "Tree");
    }

    #[test]
    fn recursive_value_type_allows_heap_and_pointer_indirection() {
        let type_defs: HashMap<NominalId, TypeDef> = [make_enum(
            "Tree",
            HashMap::from([
                (
                    "VecNode".to_string(),
                    VariantDef::Tuple(vec![Ty::builtin_named(
                        crate::BuiltinType::Vec,
                        vec![named_type("Tree")],
                    )]),
                ),
                (
                    "RcNode".to_string(),
                    VariantDef::Tuple(vec![Ty::builtin_named(
                        crate::BuiltinType::Rc,
                        vec![named_type("Tree")],
                    )]),
                ),
                (
                    "WeakNode".to_string(),
                    VariantDef::Tuple(vec![Ty::builtin_named(
                        crate::BuiltinType::Weak,
                        vec![named_type("Tree")],
                    )]),
                ),
                (
                    "PtrNode".to_string(),
                    VariantDef::Tuple(vec![Ty::Pointer {
                        is_mutable: false,
                        pointee: Box::new(named_type("Tree")),
                    }]),
                ),
                (
                    "BorrowNode".to_string(),
                    VariantDef::Tuple(vec![Ty::Borrow {
                        pointee: Box::new(named_type("Tree")),
                    }]),
                ),
            ]),
        )]
        .into_iter()
        .collect();

        let cycles = value_cycles(&type_defs);

        assert!(cycles.is_empty());
    }

    #[test]
    fn simple_two_actor_cycle() {
        // A holds B's actor handle, B holds A's
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor("A", HashMap::from([("b".to_string(), actor_handle("B"))])),
            make_actor("B", HashMap::from([("a".to_string(), actor_handle("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn self_referential() {
        // A holds its own actor handle
        let type_defs: HashMap<NominalId, TypeDef> = [make_actor(
            "A",
            HashMap::from([("me".to_string(), actor_handle("A"))]),
        )]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn transitive_through_struct() {
        // A has field of struct S, S holds B's actor handle, B holds A's
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor("A", HashMap::from([("s".to_string(), named_type("S"))])),
            make_struct("S", HashMap::from([("b".to_string(), actor_handle("B"))])),
            make_actor("B", HashMap::from([("a".to_string(), actor_handle("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn through_option() {
        // A has Option<B> (B's actor handle), B holds A's actor handle
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor(
                "A",
                HashMap::from([("b".to_string(), Ty::option(actor_handle("B")))]),
            ),
            make_actor("B", HashMap::from([("a".to_string(), actor_handle("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, _cycles) = actor_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
    }

    #[test]
    fn through_vec_named_type() {
        // A has Vec<B> (as Named { name: "Vec", args: [B's actor handle] })
        // B holds A's actor handle
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor(
                "A",
                HashMap::from([(
                    "bs".to_string(),
                    Ty::named_for_test("Vec", vec![actor_handle("B")]),
                )]),
            ),
            make_actor("B", HashMap::from([("a".to_string(), actor_handle("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, _cycles) = actor_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
    }

    #[test]
    fn through_array() {
        // A has [B; 3] (B's actor handle), B holds A's actor handle
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor(
                "A",
                HashMap::from([("bs".to_string(), Ty::Array(Box::new(actor_handle("B")), 3))]),
            ),
            make_actor("B", HashMap::from([("a".to_string(), actor_handle("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, _cycles) = actor_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
    }

    #[test]
    fn three_actor_cycle() {
        // A -> B -> C -> A
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor("A", HashMap::from([("b".to_string(), actor_handle("B"))])),
            make_actor("B", HashMap::from([("c".to_string(), actor_handle("C"))])),
            make_actor("C", HashMap::from([("a".to_string(), actor_handle("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert!(capable.contains("C"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn mixed_cycle_and_no_cycle() {
        // A <-> B cycle, C -> D no cycle
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor("A", HashMap::from([("b".to_string(), actor_handle("B"))])),
            make_actor("B", HashMap::from([("a".to_string(), actor_handle("A"))])),
            make_actor("C", HashMap::from([("d".to_string(), actor_handle("D"))])),
            make_actor("D", HashMap::from([("x".to_string(), Ty::I32)])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert!(!capable.contains("C"));
        assert!(!capable.contains("D"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn struct_without_actor_ref_no_false_positive() {
        // A has struct S with only i32 fields, no actor handle
        let type_defs: HashMap<NominalId, TypeDef> = [
            make_actor("A", HashMap::from([("s".to_string(), named_type("S"))])),
            make_struct("S", HashMap::from([("x".to_string(), Ty::I32)])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);
        assert!(capable.is_empty());
        assert!(cycles.is_empty());
    }

    #[test]
    fn generic_struct_fields_participate_in_actor_cycle_detection() {
        let generic_wrapper = (
            NominalId::for_test("Wrapper"),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Wrapper".to_string(),
                type_params: vec![crate::ParamHead::for_test("T")],
                bounds: crate::check::ParamBounds::default(),
                fields: HashMap::from([(
                    "target".to_string(),
                    Ty::param(crate::ParamHead::for_test("T")),
                )]),
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );
        let type_defs: HashMap<NominalId, TypeDef> = [
            generic_wrapper,
            make_actor(
                "A",
                HashMap::from([(
                    "wrapper".to_string(),
                    Ty::named_for_test("Wrapper", vec![actor_handle("B")]),
                )]),
            ),
            make_actor("B", HashMap::from([("a".to_string(), actor_handle("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);

        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn two_distinct_generic_instantiations_both_walk_fields() {
        // Both actual arguments in one tuple must contribute their actor
        // handles. Declaration summaries are shared, while argument discovery
        // remains specific to each authored use.
        let generic_wrapper = (
            NominalId::for_test("Wrapper"),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Wrapper".to_string(),
                type_params: vec![crate::ParamHead::for_test("T")],
                bounds: crate::check::ParamBounds::default(),
                fields: HashMap::from([(
                    "target".to_string(),
                    Ty::param(crate::ParamHead::for_test("T")),
                )]),
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );
        let type_defs: HashMap<NominalId, TypeDef> = [
            generic_wrapper,
            make_actor(
                "A",
                HashMap::from([(
                    "combo".to_string(),
                    Ty::Tuple(vec![
                        Ty::named_for_test("Wrapper", vec![actor_handle("B")]),
                        Ty::named_for_test("Wrapper", vec![actor_handle("C")]),
                    ]),
                )]),
            ),
            make_actor("B", HashMap::new()),
            make_actor("C", HashMap::from([("a".to_string(), actor_handle("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = actor_cycles(&type_defs);

        // A→Wrapper<C>→C→A cycle must be detected. Before the fix, the
        // Tuple traversal visited Wrapper<B> first, poisoning the bare-name
        // key and skipping Wrapper<C>, so C's actor handle was never recorded
        // and no cycle was found.
        assert!(capable.contains("A"));
        assert!(capable.contains("C"));
        assert!(!capable.contains("B"));
        assert_eq!(cycles.len(), 1);
    }
}
