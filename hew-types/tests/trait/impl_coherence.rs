use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use hew_parser::ast::{Item, Program, Span, Spanned};
use hew_parser::module::{Module, ModuleGraph, ModulePath};
use hew_types::error::{TypeError, TypeErrorKind};
use hew_types::{DeclarationKind, DeclarationOccurrence, TypeCheckOutput};

use crate::common;

fn assert_clean(source: &str) -> TypeCheckOutput {
    let output = common::typecheck_isolated(source);
    assert!(output.errors.is_empty(), "{source}\n{:#?}", output.errors);
    output
}

fn only_error(output: &TypeCheckOutput) -> &TypeError {
    assert_eq!(output.errors.len(), 1, "{:#?}", output.errors);
    let error = &output.errors[0];
    assert!(
        !error.message.contains("internal compiler error"),
        "{error:?}"
    );
    error
}

fn impl_spans(program: &Program) -> Vec<Span> {
    program
        .items
        .iter()
        .filter(|(item, _)| matches!(item, Item::Impl(_)))
        .map(|(_, span)| span.clone())
        .collect()
}

fn assert_conflict(source: &str) {
    let (program, output) = common::parse_and_typecheck_isolated(source);
    let spans = impl_spans(&program);
    assert_eq!(spans.len(), 2);
    let error = only_error(&output);
    assert!(
        matches!(error.kind, TypeErrorKind::ConflictingTraitImpl { .. }),
        "{error:?}"
    );
    assert_eq!(error.span, spans[1]);
    assert_eq!(
        error.notes,
        vec![(
            spans[0].clone(),
            "previous implementation here".to_string(),
            None
        )]
    );
}

#[test]
fn identical_duplicate_impls_are_rejected() {
    assert_conflict(
        "type R {}\n\
         impl Display for R { fn fmt(self) -> string { \"r\" } }\n\
         impl Display for R { fn fmt(self) -> string { \"r\" } }\n\
         fn main() {}",
    );
}

#[test]
fn explicit_display_impls_override_prelude_without_losing_identity() {
    for receiver in ["string", "NodeId", "Location"] {
        let (program, output) = common::parse_and_typecheck_isolated(&format!(
            "impl Display for {receiver} {{ fn fmt(self) -> string {{ \"override\" }} }}\n\
             fn accepts_display<T: Display>(value: T) {{}}\n\
             fn check(value: {receiver}) {{ accepts_display(value); println(f\"{{value}}\"); }}"
        ));
        assert!(output.errors.is_empty(), "{receiver}: {:#?}", output.errors);
        let Item::Impl(block) = &program.items[0].0 else {
            panic!("fixture has an impl");
        };
        let occurrence = DeclarationOccurrence::new(
            output.defs.root_module(),
            &block.methods[0].fn_span,
            DeclarationKind::ImplMethod,
            0,
        );
        assert!(
            output.defs.declaration(occurrence).is_some(),
            "{receiver} override must own its exact method occurrence"
        );
    }
}

#[test]
fn a_prelude_override_does_not_allow_a_second_source_impl() {
    for receiver in ["string", "NodeId", "Location"] {
        assert_conflict(&format!(
            "impl Display for {receiver} {{ fn fmt(self) -> string {{ \"first\" }} }}\n\
             impl Display for {receiver} {{ fn fmt(self) -> string {{ \"second\" }} }}\n\
             fn main() {{}}"
        ));
    }
}

#[test]
fn imported_prelude_revisits_keep_the_override_provenance() {
    let source = "import std.builtins;\n\
                  impl Display for string { fn fmt(self) -> string { \"override\" } }\n\
                  fn main() { println(f\"{\"value\"}\"); }";
    let output = common::typecheck(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn canonical_io_lazy_registration_completes_impl_identities() {
    let module_id = ModulePath::new(["std", "io"]);
    let root_id = ModulePath::root();
    let source = "extern \"C\" { fn hew_stdin_read_line() -> bytes; }\n\
                  pub fn read() -> bytes { unsafe { hew_stdin_read_line() } }";
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: module_id.clone(),
            items: common::parse_program(source).items,
            imports: vec![],
            source_paths: vec![common::repo_root().join("std/io.hew")],
            doc: None,
        })
        .expect("canonical io fixture");
    graph.topo_order = vec![module_id, root_id];
    let program = Program {
        items: vec![],
        module_graph: Some(graph),
        module_doc: None,
    };
    let output = common::isolated_checker().check_program(&program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let module = output
        .defs
        .module_for_path("std.io")
        .expect("canonical io identity");
    let embedded = common::parse_program(include_str!("../../../std/io.hew"));
    // The receiver impls (`impl bytes`) register lazily; io's own types and
    // their trait impls register when a program imports the module.
    let receiver_impls = |(item, _): &&(Item, Span)| matches!(item, Item::Impl(block) if block.trait_bound.is_none());
    let spans: Vec<Span> = embedded
        .items
        .iter()
        .filter(receiver_impls)
        .map(|(_, span)| span.clone())
        .collect();
    assert!(!spans.is_empty(), "io supplies embedded receiver impls");
    for span in spans {
        let occurrence =
            DeclarationOccurrence::new(Some(module), &span, DeclarationKind::ImplBlock, 0);
        assert!(
            output.defs.declaration(occurrence).is_some(),
            "lazy io registration must inventory its actual source impl"
        );
    }
    for (item, _) in embedded.items.iter().filter(receiver_impls) {
        let Item::Impl(block) = item else { continue };
        for method in &block.methods {
            let occurrence = DeclarationOccurrence::new(
                Some(module),
                &method.fn_span,
                DeclarationKind::ImplMethod,
                0,
            );
            assert!(
                output.defs.declaration(occurrence).is_some(),
                "lazy io method must keep its exact source identity"
            );
        }
    }
}

#[test]
fn same_head_with_different_bodies_is_rejected_for_nominal_and_builtin_receivers() {
    for receiver in ["R", "i64", "Vec<i64>"] {
        assert_conflict(&format!(
            "trait Label {{ fn label(self) -> i64; }}\n\
             type R {{}}\n\
             impl Label for {receiver} {{ fn label(self) -> i64 {{ 1 }} }}\n\
             impl Label for {receiver} {{ fn label(self) -> i64 {{ 2 }} }}\n\
             fn main() {{}}"
        ));
    }
}

#[test]
fn empty_and_associated_type_only_impls_are_rejected() {
    for receiver in ["R", "Vec<i64>"] {
        for (trait_body, first, second) in [
            ("", "", ""),
            (
                "type Output;",
                "type Output = i64;",
                "type Output = string;",
            ),
        ] {
            assert_conflict(&format!(
                "trait Label {{ {trait_body} }}\n\
                 type R {{}}\n\
                 impl Label for {receiver} {{ {first} }}\n\
                 impl Label for {receiver} {{ {second} }}\n\
                 fn main() {{}}"
            ));
        }
    }
}

#[test]
fn alpha_renamed_impl_binders_conflict_positionally() {
    for second_binders in [("T", "U"), ("X", "Y")] {
        let (left, right) = second_binders;
        assert_conflict(&format!(
            "trait Label<A> {{ fn label(self) -> i64; }}\n\
             type Pair<A, B> {{ a: A; b: B; }}\n\
             impl<T, U> Label<U> for Pair<T, U> {{ fn label(self) -> i64 {{ 1 }} }}\n\
             impl<{left}, {right}> Label<{right}> for Pair<{left}, {right}> {{ fn label(self) -> i64 {{ 2 }} }}\n\
             fn main() {{}}"
        ));
    }
}

#[test]
fn duplicate_methods_have_normal_definition_diagnostics() {
    for source in [
        "type R {} impl R { fn f(self) -> i64 { 1 } fn f(self) -> i64 { 2 } }",
        "type R {} impl R { fn f(self) -> i64 { 1 } } impl R { fn f(self) -> i64 { 2 } }",
        "type R {} impl Display for R { fn fmt(self) -> string { \"a\" } fn fmt(self) -> string { \"b\" } }",
        "type Box<T> { value: T; } impl<T> Box<T> { fn f(self) -> i64 { 1 } } impl<U> Box<U> { fn f(self) -> i64 { 2 } }",
    ] {
        let (program, output) = common::parse_and_typecheck_isolated(source);
        let methods: Vec<_> = program.items.iter().filter_map(|(item, _)| {
            let Item::Impl(block) = item else { return None };
            Some(block.methods.iter().map(|method| method.fn_span.clone()))
        }).flatten().collect();
        let error = only_error(&output);
        assert_eq!(error.kind, TypeErrorKind::DuplicateDefinition);
        assert_eq!(error.span, methods[1]);
        assert_eq!(error.notes, vec![(methods[0].clone(), "previous definition here".to_string(), None)]);
    }
}

#[test]
fn distinct_receiver_trait_and_trait_argument_heads_remain_valid() {
    assert_clean(
        "trait Label { fn label(self) -> i64; }\n\
         trait Other { fn label(self) -> i64; }\n\
         type R {} type S {}\n\
         impl Label for R { fn label(self) -> i64 { 1 } }\n\
         impl Label for S { fn label(self) -> i64 { 2 } }\n\
         impl Other for R { fn label(self) -> i64 { 3 } }\n\
         impl R { fn label(self) -> i64 { 4 } }\n\
         impl From<i64> for R { fn from(value: i64) -> R { R {} } }\n\
         impl From<string> for R { fn from(value: string) -> R { R {} } }\n\
         fn main() {}",
    );
    assert_clean(
        "trait Label<A> { fn label(self) -> i64; }\n\
         type Pair<A, B> { a: A; b: B; }\n\
         impl<T, U> Label<T> for Pair<T, U> { fn label(self) -> i64 { 1 } }\n\
         impl<X, Y> Label<Y> for Pair<X, Y> { fn label(self) -> i64 { 2 } }\n\
         fn main() {}",
    );
}

#[test]
fn disjoint_inherent_methods_and_user_record_specialisation_remain_valid() {
    assert_clean(
        "trait Label { fn label(self) -> i64; }\n\
         type Box<T> { value: T; }\n\
         impl<T> Label for Box<T> { fn label(self) -> i64 { 1 } }\n\
         impl Label for Box<i64> { fn label(self) -> i64 { 2 } }\n\
         impl<T> Box<T> { fn first(self) -> i64 { 3 } }\n\
         impl<U> Box<U> { fn second(self) -> i64 { 4 } }\n\
         impl Box<i64> { fn first(self) -> i64 { 5 } }\n\
         fn main() {\n\
             println(Box { value: 1 }.label());\n\
             println(Box { value: \"text\" }.label());\n\
         }",
    );
}

fn resolve_imports(program: &mut Program, resolved: &HashMap<String, Vec<Spanned<Item>>>) {
    for (item, _) in &mut program.items {
        let Item::Import(import) = item else { continue };
        let name = import.path.to_string();
        let items = resolved
            .get(&name)
            .expect("fixture import is topologically available");
        let path = PathBuf::from(format!("impl-coherence/{name}.hew"));
        import.resolved_items = Some(items.clone().into());
        import.resolved_item_source_paths = vec![path.clone(); items.len()];
        import.resolved_source_paths = vec![path];
    }
}

fn module_program(root_source: &str, sources: &[(&str, &str)]) -> Program {
    let root_id = ModulePath::root();
    let mut graph = ModuleGraph::new(root_id.clone());
    let mut resolved = HashMap::new();
    for &(name, source) in sources {
        let mut program = common::parse_program(source);
        resolve_imports(&mut program, &resolved);
        let id = ModulePath::new([name]);
        let path = PathBuf::from(format!("impl-coherence/{name}.hew"));
        graph
            .item_sources
            .insert(name.to_string(), vec![path.clone(); program.items.len()]);
        resolved.insert(name.to_string(), program.items.clone());
        graph
            .add_module(Module {
                id: id.clone(),
                items: program.items,
                imports: vec![],
                source_paths: vec![path],
                doc: None,
            })
            .expect("fixture module");
        graph.topo_order.push(id);
    }
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("fixture root");
    graph.topo_order.push(root_id);
    let mut root = common::parse_program(root_source);
    resolve_imports(&mut root, &resolved);
    root.module_graph = Some(graph);
    root
}

#[test]
fn cross_module_conflict_preserves_both_source_locations() {
    let implementation =
        "import common;\nimpl Display for common.R { fn fmt(self) -> string { \"r\" } }";
    let span = impl_spans(&common::parse_program(implementation))[0].clone();
    let program = module_program(
        "import left; import right; fn main() {}",
        &[
            ("common", "pub type R {}"),
            ("left", implementation),
            ("right", implementation),
        ],
    );
    let output = common::isolated_checker().check_program(&program);
    let error = only_error(&output);
    assert!(matches!(
        error.kind,
        TypeErrorKind::ConflictingTraitImpl { .. }
    ));
    assert_eq!(error.span, span);
    assert!(error
        .source_module
        .as_deref()
        .is_some_and(|path| path.ends_with("right.hew")));
    assert_eq!(error.notes.len(), 1);
    assert_eq!(error.notes[0].0, span);
    assert_eq!(error.notes[0].1, "previous implementation here");
    assert!(error.notes[0]
        .2
        .as_deref()
        .is_some_and(|path| path.ends_with("left.hew")));
}

#[test]
fn repeated_registration_of_one_physical_impl_is_idempotent() {
    let program = module_program(
        "import lib as first; import lib as second;\n\
         fn main() { let a = first.R {}; let b = second.R {}; println(f\"{a}\"); println(f\"{b}\"); }",
        &[("lib", "pub type R {} impl Display for R { fn fmt(self) -> string { \"r\" } }")],
    );
    let output = common::isolated_checker().check_program(&program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let declarations: HashSet<_> = output
        .impl_method_declaration_ids
        .values()
        .filter(|declaration| output.defs.path(**declaration).starts_with("lib.R::<impl "))
        .collect();
    assert_eq!(declarations.len(), 1);
}

#[test]
fn same_named_traits_from_distinct_modules_remain_distinct() {
    let program = module_program(
        "import left; import right;\n\
         type R {}\n\
         impl left.Label for R { fn label(self) -> i64 { 1 } }\n\
         impl right.Label for R { fn label(self) -> i64 { 2 } }\n\
         fn main() {}",
        &[
            ("left", "pub trait Label { fn label(self) -> i64; }"),
            ("right", "pub trait Label { fn label(self) -> i64; }"),
        ],
    );
    let output = common::isolated_checker().check_program(&program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}
