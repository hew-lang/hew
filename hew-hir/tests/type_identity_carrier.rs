//! Tests for the type/record defining-module identity carrier.
//!
//! Type identity is `(defining-module, name)`: a `pub type` lowered from a
//! package module carries `defining_module = Some(module_short)` on its
//! `HirTypeDecl`, while a root-program type carries `None`. The carrier is what
//! lets MIR layout keys and codegen symbol synthesis distinguish two same-named
//! types from different modules; these tests pin the population site and the
//! derived qualified key, including the root → bare mapping that keeps
//! single-module programs free of layout/symbol churn. Mirrors
//! `actor_identity_carrier.rs` for the type/record path.

use hew_hir::{dump_hir, lower_program_host_target, HirItem, HirTypeDecl, ResolutionCtx};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

/// Build a `Program` containing a non-root module `bank` with arbitrary
/// source. Mirrors the imported-free-fn / actor-carrier harness shape.
fn build_program_with_imported_module(imported_src: &str, root_src: &str) -> Program {
    let imported = hew_parser::parse(imported_src);
    assert!(
        imported.errors.is_empty(),
        "imported parse errors: {:?}",
        imported.errors
    );
    let root = hew_parser::parse(root_src);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let imported_id = ModuleId::new(vec!["bank".to_string()]);
    let root_id = ModuleId::root();

    let imported_items: Vec<_> = imported
        .program
        .items
        .iter()
        .filter(|(item, _)| !matches!(item, Item::Import(_)))
        .cloned()
        .collect();

    let imported_module = Module {
        id: imported_id.clone(),
        items: imported_items,
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };
    let root_module = Module {
        id: root_id.clone(),
        items: root.program.items.clone(),
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };

    let mut graph = ModuleGraph::new(root_id.clone());
    graph.add_module(imported_module).expect("add imported");
    graph.add_module(root_module).expect("add root");
    graph.topo_order = vec![imported_id, root_id];

    Program {
        items: root.program.items,
        module_graph: Some(graph),
        ..root.program
    }
}

fn lower_with_checker(program: &Program) -> (hew_hir::LowerOutput, TypeCheckOutput) {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(program);
    let output = lower_program_host_target(program, &tco, &ResolutionCtx);
    (output, tco)
}

fn find_type<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirTypeDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::TypeDecl(decl) = item {
                (decl.name == name).then_some(decl)
            } else {
                None
            }
        })
        .unwrap_or_else(|| {
            let names: Vec<&str> = output
                .module
                .items
                .iter()
                .filter_map(|item| {
                    if let HirItem::TypeDecl(decl) = item {
                        Some(decl.name.as_str())
                    } else {
                        None
                    }
                })
                .collect();
            panic!("missing type `{name}`; types present: {names:?}")
        })
}

/// A `pub type` lowered from a package module carries its defining module; a
/// root-program type carries `None`. The decl `name` stays bare in both cases
/// — qualification lives in the carrier, not the name.
#[test]
fn imported_type_carries_defining_module_and_root_type_carries_none() {
    let program = build_program_with_imported_module(
        "pub type Widget { count: i64 }\n",
        "type Local { total: i64 }\n\
         fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let imported = find_type(&output, "Widget");
    assert_eq!(
        imported.defining_module.as_deref(),
        Some("bank"),
        "package-module type must carry its defining module"
    );
    assert_eq!(imported.name, "Widget", "decl name stays bare");

    let local = find_type(&output, "Local");
    assert_eq!(
        local.defining_module, None,
        "root-program type identity is the root namespace"
    );
}

/// The qualified key derives dotted (`bank.Widget`) for module types and bare
/// for root types, and the symbol mangle of the bare form is the identity
/// mapping — the zero-churn guarantee for single-module programs.
#[test]
fn type_qualified_name_derives_dotted_for_module_and_bare_for_root() {
    let program = build_program_with_imported_module(
        "pub type Widget { count: i64 }\n",
        "type Local { total: i64 }\n\
         fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let imported = find_type(&output, "Widget");
    assert_eq!(imported.qualified_name(), "bank.Widget");
    assert_eq!(
        hew_hir::mangle_dotted_name(&imported.qualified_name()),
        "bank$Widget",
        "module-type symbols mangle through the dotted-name authority"
    );

    let local = find_type(&output, "Local");
    assert_eq!(local.qualified_name(), "Local");
    assert_eq!(
        hew_hir::mangle_dotted_name(&local.qualified_name()),
        "Local",
        "root-type qualified key mangles to the bare name unchanged"
    );
}

/// The HIR dump surfaces the qualified identity on imported type decls and
/// stays silent for root types — the human-readable proving signal for the
/// Stage-1 carrier.
#[test]
fn hir_dump_shows_qualified_identity_only_for_imported_types() {
    let program = build_program_with_imported_module(
        "pub type Widget { count: i64 }\n",
        "type Local { total: i64 }\n\
         fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("defining-module bank qualified=bank.Widget"),
        "imported type must show its qualified identity in the dump; dump:\n{dump}"
    );
    // The root type's line is `defining-module`-free — qualification only
    // appears for module-exported identities.
    assert!(
        !dump.contains("defining-module bank qualified=bank.Local"),
        "root type must not carry a defining-module line; dump:\n{dump}"
    );
}
