//! Compile-time detection of actor reference cycles.
//!
//! Builds a directed graph of actor-to-actor references via `ActorRef<X>`
//! fields and runs Tarjan's SCC algorithm to find strongly connected
//! components (cycles). Actors in cycles are returned as "cycle-capable"
//! so the runtime can selectively enable cycle scanning for them.

use std::collections::{HashMap, HashSet};

use crate::check::TypeDef;
use crate::ty::Ty;

/// Result of cycle detection: the set of cycle-capable actor names and a list
/// of cycles (each cycle is a vec of actor names plus a representative span).
#[must_use]
#[expect(
    clippy::implicit_hasher,
    reason = "only called with std HashMap internally"
)]
pub fn detect_actor_ref_cycles(
    type_defs: &HashMap<String, TypeDef>,
) -> (HashSet<String>, Vec<Vec<String>>) {
    // Build adjacency list: actor name -> set of actor names it references
    let mut adj: HashMap<&str, HashSet<&str>> = HashMap::new();

    for (name, td) in type_defs {
        if td.kind != crate::check::TypeDefKind::Actor {
            continue;
        }
        let mut refs = HashSet::new();
        for field_ty in td.fields.values() {
            collect_actor_refs(field_ty, type_defs, &mut refs, &mut HashSet::new());
        }
        adj.insert(name.as_str(), refs);
    }

    // Run Tarjan's SCC
    let actor_names: Vec<&str> = adj.keys().copied().collect();
    let sccs = tarjan_scc(&actor_names, &adj);

    let mut cycle_capable = HashSet::new();
    let mut cycles = Vec::new();

    for scc in &sccs {
        let is_cycle = if scc.len() >= 2 {
            true
        } else if scc.len() == 1 {
            // Self-loop: actor references itself
            let name = scc[0];
            adj.get(name).is_some_and(|refs| refs.contains(name))
        } else {
            false
        };

        if is_cycle {
            for &name in scc {
                cycle_capable.insert(name.to_string());
            }
            cycles.push(scc.iter().map(|s| (*s).to_string()).collect());
        }
    }

    (cycle_capable, cycles)
}

/// Recursively collect actor names referenced via `ActorRef<X>` in a type,
/// looking through containers (`Option`, `Vec`, `Array`, `Slice`, `Tuple`,
/// `Result`, `HashMap`) and transitively through struct fields.
fn collect_actor_refs<'a>(
    ty: &'a Ty,
    type_defs: &'a HashMap<String, TypeDef>,
    out: &mut HashSet<&'a str>,
    visited_structs: &mut HashSet<&'a str>,
) {
    match ty {
        Ty::ActorRef(inner) => {
            if let Ty::Named { name, .. } = inner.as_ref() {
                if type_defs.contains_key(name) {
                    out.insert(name.as_str());
                }
            }
        }
        Ty::Option(inner) | Ty::Slice(inner) | Ty::Array(inner, _) => {
            collect_actor_refs(inner, type_defs, out, visited_structs);
        }
        Ty::Tuple(elems) => {
            for elem in elems {
                collect_actor_refs(elem, type_defs, out, visited_structs);
            }
        }
        Ty::Result { ok, err } => {
            collect_actor_refs(ok, type_defs, out, visited_structs);
            collect_actor_refs(err, type_defs, out, visited_structs);
        }
        Ty::Named { name, args } => {
            // Transitively follow struct fields
            if let Some(td) = type_defs.get(name) {
                if td.kind == crate::check::TypeDefKind::Struct
                    && !visited_structs.contains(name.as_str())
                {
                    visited_structs.insert(name.as_str());
                    for field_ty in td.fields.values() {
                        collect_actor_refs(field_ty, type_defs, out, visited_structs);
                    }
                }
            }
            // Also check type arguments (e.g. Vec<ActorRef<B>>)
            for arg in args {
                collect_actor_refs(arg, type_defs, out, visited_structs);
            }
        }
        _ => {}
    }
}

/// Tarjan's strongly connected components algorithm.
///
/// Returns SCCs in reverse topological order (leaf SCCs first).
fn tarjan_scc<'a>(
    nodes: &[&'a str],
    adj: &HashMap<&'a str, HashSet<&'a str>>,
) -> Vec<Vec<&'a str>> {
    struct State<'a> {
        index_counter: usize,
        stack: Vec<&'a str>,
        on_stack: HashSet<&'a str>,
        index: HashMap<&'a str, usize>,
        lowlink: HashMap<&'a str, usize>,
        result: Vec<Vec<&'a str>>,
    }

    fn strongconnect<'a>(
        v: &'a str,
        adj: &HashMap<&'a str, HashSet<&'a str>>,
        state: &mut State<'a>,
    ) {
        state.index.insert(v, state.index_counter);
        state.lowlink.insert(v, state.index_counter);
        state.index_counter += 1;
        state.stack.push(v);
        state.on_stack.insert(v);

        if let Some(neighbors) = adj.get(v) {
            for &w in neighbors {
                if !state.index.contains_key(w) {
                    strongconnect(w, adj, state);
                    let w_low = state.lowlink[w];
                    let v_low = state.lowlink[v];
                    if w_low < v_low {
                        state.lowlink.insert(v, w_low);
                    }
                } else if state.on_stack.contains(w) {
                    let w_idx = state.index[w];
                    let v_low = state.lowlink[v];
                    if w_idx < v_low {
                        state.lowlink.insert(v, w_idx);
                    }
                }
            }
        }

        if state.lowlink[v] == state.index[v] {
            let mut scc = Vec::new();
            loop {
                let w = state.stack.pop().expect("stack should not be empty");
                state.on_stack.remove(w);
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
        if !state.index.contains_key(node) {
            strongconnect(node, adj, &mut state);
        }
    }

    state.result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::TypeDefKind;

    fn make_actor(name: &str, fields: HashMap<String, Ty>) -> (String, TypeDef) {
        (
            name.to_string(),
            TypeDef {
                kind: TypeDefKind::Actor,
                name: name.to_string(),
                type_params: vec![],
                fields,
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
            },
        )
    }

    fn make_struct(name: &str, fields: HashMap<String, Ty>) -> (String, TypeDef) {
        (
            name.to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: name.to_string(),
                type_params: vec![],
                fields,
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
            },
        )
    }

    fn actor_ref(name: &str) -> Ty {
        Ty::ActorRef(Box::new(Ty::Named {
            name: name.to_string(),
            args: vec![],
        }))
    }

    #[test]
    fn no_actors() {
        let type_defs = HashMap::new();
        let (capable, cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.is_empty());
        assert!(cycles.is_empty());
    }

    #[test]
    fn no_cycles_linear() {
        // A -> B -> C (no cycle)
        let type_defs: HashMap<String, TypeDef> = [
            make_actor("A", HashMap::from([("b".to_string(), actor_ref("B"))])),
            make_actor("B", HashMap::from([("c".to_string(), actor_ref("C"))])),
            make_actor("C", HashMap::from([("x".to_string(), Ty::I32)])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.is_empty());
        assert!(cycles.is_empty());
    }

    #[test]
    fn simple_two_actor_cycle() {
        // A has ActorRef<B>, B has ActorRef<A>
        let type_defs: HashMap<String, TypeDef> = [
            make_actor("A", HashMap::from([("b".to_string(), actor_ref("B"))])),
            make_actor("B", HashMap::from([("a".to_string(), actor_ref("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn self_referential() {
        // A has ActorRef<A>
        let type_defs: HashMap<String, TypeDef> = [make_actor(
            "A",
            HashMap::from([("me".to_string(), actor_ref("A"))]),
        )]
        .into_iter()
        .collect();

        let (capable, cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn transitive_through_struct() {
        // A has field of struct S, S has ActorRef<B>, B has ActorRef<A>
        let type_defs: HashMap<String, TypeDef> = [
            make_actor(
                "A",
                HashMap::from([(
                    "s".to_string(),
                    Ty::Named {
                        name: "S".to_string(),
                        args: vec![],
                    },
                )]),
            ),
            make_struct("S", HashMap::from([("b".to_string(), actor_ref("B"))])),
            make_actor("B", HashMap::from([("a".to_string(), actor_ref("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn through_option() {
        // A has Option<ActorRef<B>>, B has ActorRef<A>
        let type_defs: HashMap<String, TypeDef> = [
            make_actor(
                "A",
                HashMap::from([("b".to_string(), Ty::Option(Box::new(actor_ref("B"))))]),
            ),
            make_actor("B", HashMap::from([("a".to_string(), actor_ref("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, _cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
    }

    #[test]
    fn through_vec_named_type() {
        // A has Vec<ActorRef<B>> (as Named { name: "Vec", args: [ActorRef<B>] })
        // B has ActorRef<A>
        let type_defs: HashMap<String, TypeDef> = [
            make_actor(
                "A",
                HashMap::from([(
                    "bs".to_string(),
                    Ty::Named {
                        name: "Vec".to_string(),
                        args: vec![actor_ref("B")],
                    },
                )]),
            ),
            make_actor("B", HashMap::from([("a".to_string(), actor_ref("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, _cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
    }

    #[test]
    fn through_array() {
        // A has [ActorRef<B>; 3], B has ActorRef<A>
        let type_defs: HashMap<String, TypeDef> = [
            make_actor(
                "A",
                HashMap::from([("bs".to_string(), Ty::Array(Box::new(actor_ref("B")), 3))]),
            ),
            make_actor("B", HashMap::from([("a".to_string(), actor_ref("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, _cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
    }

    #[test]
    fn three_actor_cycle() {
        // A -> B -> C -> A
        let type_defs: HashMap<String, TypeDef> = [
            make_actor("A", HashMap::from([("b".to_string(), actor_ref("B"))])),
            make_actor("B", HashMap::from([("c".to_string(), actor_ref("C"))])),
            make_actor("C", HashMap::from([("a".to_string(), actor_ref("A"))])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert!(capable.contains("C"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn mixed_cycle_and_no_cycle() {
        // A <-> B cycle, C -> D no cycle
        let type_defs: HashMap<String, TypeDef> = [
            make_actor("A", HashMap::from([("b".to_string(), actor_ref("B"))])),
            make_actor("B", HashMap::from([("a".to_string(), actor_ref("A"))])),
            make_actor("C", HashMap::from([("d".to_string(), actor_ref("D"))])),
            make_actor("D", HashMap::from([("x".to_string(), Ty::I32)])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.contains("A"));
        assert!(capable.contains("B"));
        assert!(!capable.contains("C"));
        assert!(!capable.contains("D"));
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn struct_without_actor_ref_no_false_positive() {
        // A has struct S with only i32 fields, no ActorRef
        let type_defs: HashMap<String, TypeDef> = [
            make_actor(
                "A",
                HashMap::from([(
                    "s".to_string(),
                    Ty::Named {
                        name: "S".to_string(),
                        args: vec![],
                    },
                )]),
            ),
            make_struct("S", HashMap::from([("x".to_string(), Ty::I32)])),
        ]
        .into_iter()
        .collect();

        let (capable, cycles) = detect_actor_ref_cycles(&type_defs);
        assert!(capable.is_empty());
        assert!(cycles.is_empty());
    }
}
