//! Tests for HIR lowering of `pub fn` free functions declared in imported
//! modules.
//!
//! Imported public bodies are emitted under qualified native-symbol names
//! (`m$entry`) while their source bodies may call same-module functions by bare
//! identifier (`helper(n)`). These tests cover the dependency closure that
//! rewrites those bare calls to qualified imported names and emits only the
//! private helpers that are actually reachable from exported bodies.

use hew_hir::{
    lower_program_host_target, HirDiagnosticKind, HirExprKind, HirFn, HirItem, ResolutionCtx,
};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::error::TypeErrorKind;
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

/// Build a `Program` containing a non-root module `m` with arbitrary source.
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

    let imported_id = ModuleId::new(vec!["m".to_string()]);
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

fn function_names(output: &hew_hir::LowerOutput) -> Vec<&str> {
    output
        .module
        .items
        .iter()
        .filter_map(|item| {
            if let HirItem::Function(f) = item {
                Some(f.name.as_str())
            } else {
                None
            }
        })
        .collect()
}

fn function_by_name<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirFn {
    output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Function(f) = item {
                (f.name == name).then_some(f)
            } else {
                None
            }
        })
        .unwrap_or_else(|| {
            panic!(
                "missing function `{name}`; names: {:?}",
                function_names(output)
            )
        })
}

fn tail_call_callee_name(function: &HirFn) -> Option<&str> {
    let tail = function.body.tail.as_ref()?;
    let HirExprKind::Call { callee, .. } = &tail.kind else {
        return None;
    };
    if let HirExprKind::BindingRef { name, .. } = &callee.kind {
        Some(name.as_str())
    } else {
        None
    }
}

fn assert_no_imported_free_fn_closure_diagnostics(output: &hew_hir::LowerOutput) {
    let blocked: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::ImportedFreeFnBodyUnresolvedBareCall { .. }
                    | HirDiagnosticKind::ImportedBodyMissingPrivateHelper { .. }
            )
        })
        .collect();
    assert!(
        blocked.is_empty(),
        "imported free-function closure should lower without Stage 1 blockers; got: {blocked:#?}"
    );
}

#[test]
fn imported_pub_free_fn_body_bare_call_to_same_module_pub_resolves_qualified() {
    let program = build_program_with_imported_module(
        r"
pub fn peer(n: i64) -> i64 { n }
pub fn entry(n: i64) -> i64 { peer(n) }
",
        "fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);

    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    assert_no_imported_free_fn_closure_diagnostics(&output);
    let names = function_names(&output);
    assert!(names.contains(&"m$peer"), "names: {names:?}");
    assert!(names.contains(&"m$entry"), "names: {names:?}");
    assert_eq!(
        tail_call_callee_name(function_by_name(&output, "m$entry")),
        Some("m$peer")
    );
}

#[test]
fn imported_pub_free_fn_body_bare_call_to_private_helper_resolves_qualified() {
    let program = build_program_with_imported_module(
        r"
fn helper(n: i64) -> i64 { n + 1 }
pub fn entry(n: i64) -> i64 { helper(n) }
",
        "fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);

    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    assert_no_imported_free_fn_closure_diagnostics(&output);
    let names = function_names(&output);
    assert!(names.contains(&"m$entry"), "names: {names:?}");
    assert!(names.contains(&"m$helper"), "names: {names:?}");
    assert_eq!(
        tail_call_callee_name(function_by_name(&output, "m$entry")),
        Some("m$helper")
    );
}

#[test]
fn imported_private_helper_not_reachable_from_pub_body_is_not_emitted() {
    let program = build_program_with_imported_module(
        r"
fn unused(n: i64) -> i64 { n + 100 }
pub fn entry(n: i64) -> i64 { n }
",
        "fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);

    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    assert_no_imported_free_fn_closure_diagnostics(&output);
    let names = function_names(&output);
    assert!(names.contains(&"m$entry"), "names: {names:?}");
    assert!(
        !names.contains(&"m$unused"),
        "unused private helper must not be emitted; names: {names:?}"
    );
}

#[test]
fn imported_private_helper_recursive_closure_terminates_and_emits_dependencies() {
    let program = build_program_with_imported_module(
        r"
fn a(n: i64) -> i64 { if n == 0 { 0 } else { b(n - 1) } }
fn b(n: i64) -> i64 { if n == 0 { 0 } else { a(n - 1) } }
pub fn entry(n: i64) -> i64 { a(n) }
",
        "fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);

    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    assert_no_imported_free_fn_closure_diagnostics(&output);
    let names = function_names(&output);
    assert!(names.contains(&"m$entry"), "names: {names:?}");
    assert!(names.contains(&"m$a"), "names: {names:?}");
    assert!(names.contains(&"m$b"), "names: {names:?}");
}

#[test]
fn imported_pub_free_fn_unknown_bare_call_still_surfaces_as_unresolved_symbol() {
    let program = build_program_with_imported_module(
        r"
pub fn entry(n: i64) -> i64 { totally_unknown(n) }
",
        "fn main() -> i64 { 0 }",
    );
    let (output, _tco) = lower_with_checker(&program);

    assert_no_imported_free_fn_closure_diagnostics(&output);
    assert!(
        output.diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "totally_unknown"
        )),
        "unknown bare callee must continue to surface as UnresolvedSymbol; diagnostics: {:#?}",
        output.diagnostics
    );
}

#[test]
fn imported_private_helper_is_not_callable_from_importer_user_code() {
    let program = build_program_with_imported_module(
        r"
fn secret(n: i64) -> i64 { n }
pub fn entry(n: i64) -> i64 { secret(n) }
",
        "fn main() -> i64 { m.secret(1) }",
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&program);

    assert!(
        tco.errors
            .iter()
            .any(|err| matches!(err.kind, TypeErrorKind::UndefinedMethod)),
        "private helper must stay hidden from importer module-qualified calls; errors: {:#?}",
        tco.errors
    );
}
