//! Imported `std::iter` adapters must retain their exact `Iterator::next`
//! impl registrations.  The `VecIter` cursor is the terminal builtin; adapters
//! themselves always travel through the ordinary static-dispatch index.

use crate::support;
use hew_hir::dispatch::{build_trait_impl_method_index, lookup_trait_impl_entry_by_id};
use hew_parser::{
    ast::{Item, Program},
    module::{Module, ModuleGraph, ModuleId},
};
use hew_types::{DefId, NominalId, NominalInstance};

const ADAPTERS: &[&str] = &["Map", "Filter", "Take", "Skip"];

fn std_iter_output(root_body: &str) -> hew_hir::LowerOutput {
    let iter = support::checker_pipeline::parse_source(include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../std/iter.hew"
    )));
    let iter_items: Vec<_> = iter
        .program
        .items
        .iter()
        .filter(|(item, _)| !matches!(item, Item::Import(_)))
        .cloned()
        .collect();
    let mut root =
        support::checker_pipeline::parse_source(&format!("import std.iter;\n{root_body}"));
    for (item, _) in &mut root.program.items {
        if let Item::Import(import) = item {
            import.resolved_items = Some(iter_items.clone());
        }
    }

    let iter_id = ModuleId::new(vec!["std".to_string(), "iter".to_string()]);
    let root_id = ModuleId::root();
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: iter_id.clone(),
            items: iter_items,
            imports: Vec::new(),
            source_paths: Vec::new(),
            doc: None,
        })
        .expect("add std.iter module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: root.program.items.clone(),
            imports: Vec::new(),
            source_paths: Vec::new(),
            doc: None,
        })
        .expect("add root module");
    graph.topo_order = vec![iter_id, root_id];
    let program = Program {
        items: root.program.items,
        module_graph: Some(graph),
        ..root.program
    };
    support::checker_pipeline::lower_through_checker_from_program(&program)
}

#[test]
fn imported_iter_adapter_next_impls_are_registered_by_exact_owner() {
    let output = std_iter_output("fn main() -> i64 { 0 }");
    let index = build_trait_impl_method_index(&output.module.items);
    let iterator = DefId::new("std.builtins.Iterator");
    let next = DefId::new("std.builtins.Iterator::next");

    for adapter in ADAPTERS {
        let entry = lookup_trait_impl_entry_by_id(
            &index,
            &iterator,
            &NominalInstance {
                nominal: NominalId::new(format!("std.iter.{adapter}")),
                args: Vec::new(),
            },
            &next,
        )
        .unwrap_or_else(|| {
            panic!(
                "std.iter.{adapter} must register its exact Iterator::next impl; index keys: {:#?}",
                index.keys().collect::<Vec<_>>()
            )
        });
        assert!(
            entry
                .method_symbol
                .starts_with(&format!("std.iter.{adapter}::next")),
            "{adapter} must retain its imported owner, got {entry:?}"
        );
    }
}

#[test]
fn compiler_iterator_impls_retain_their_typed_receiver_identities() {
    let output = std_iter_output("fn main() -> i64 { 0 }");
    let index = build_trait_impl_method_index(&output.module.items);
    let iterator = DefId::new("std.builtins.Iterator");
    let next = DefId::new("std.builtins.Iterator::next");
    let receivers: Vec<_> = index
        .keys()
        .filter(|key| key.declaring_trait == iterator && key.method == next)
        .map(|key| key.self_type.nominal.full_path().to_string())
        .collect();

    for expected in [
        "std.builtins.VecIter",
        "std.builtins.HashMapIter",
        "Generator",
        "AsyncGenerator",
    ] {
        assert!(
            receivers.iter().any(|receiver| receiver == expected),
            "missing exact builtin Iterator impl receiver `{expected}`; registered: {receivers:?}"
        );
    }
}

#[test]
fn user_hashmap_iter_shadow_cannot_capture_the_compiler_cursor_impl() {
    let output = std_iter_output(
        r"
type HashMapIter<T> { value: Option<T>; }
impl<T> Iterator for HashMapIter<T> {
    type Item = T;
    fn next(var self) -> Option<T> { None }
}
fn main() -> i64 { 0 }
",
    );
    let index = build_trait_impl_method_index(&output.module.items);
    let iterator = DefId::new("std.builtins.Iterator");
    let next = DefId::new("std.builtins.Iterator::next");
    let builtin = lookup_trait_impl_entry_by_id(
        &index,
        &iterator,
        &NominalInstance {
            nominal: NominalId::new("std.builtins.HashMapIter"),
            args: Vec::new(),
        },
        &next,
    )
    .unwrap_or_else(|| {
        panic!(
            "the compiler cursor keeps its exact std impl; keys: {:#?}",
            index.keys().collect::<Vec<_>>()
        )
    });
    let user = lookup_trait_impl_entry_by_id(
        &index,
        &iterator,
        &NominalInstance {
            nominal: NominalId::new("HashMapIter"),
            args: Vec::new(),
        },
        &next,
    )
    .expect("the user same-leaf nominal keeps an independent impl");

    assert_ne!(builtin.method, user.method);
    assert_ne!(builtin.method_symbol, user.method_symbol);
}

#[test]
fn user_map_shadow_does_not_capture_imported_iter_map_dispatch() {
    let output = std_iter_output(
        r"
type Map<T> { value: Option<T>; }
impl<T> Iterator for Map<T> {
    type Item = T;
    fn next(var self) -> Option<T> { None }
}
fn main() -> i64 { 0 }
",
    );
    let index = build_trait_impl_method_index(&output.module.items);
    let iterator = DefId::new("std.builtins.Iterator");
    let next = DefId::new("std.builtins.Iterator::next");
    let std_entry = lookup_trait_impl_entry_by_id(
        &index,
        &iterator,
        &NominalInstance {
            nominal: NominalId::new("std.iter.Map"),
            args: Vec::new(),
        },
        &next,
    )
    .expect("std.iter.Map remains registered despite a local Map");
    let local_entry = lookup_trait_impl_entry_by_id(
        &index,
        &iterator,
        &NominalInstance {
            nominal: NominalId::new("Map"),
            args: Vec::new(),
        },
        &next,
    )
    .expect("the local Map is independently registered");
    assert_ne!(std_entry.method, local_entry.method);
    assert_ne!(std_entry.method_symbol, local_entry.method_symbol);
}
