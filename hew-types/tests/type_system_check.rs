/// Integration tests for the stdlib-source diagnostic suppression boundary.
///
/// These tests verify that scope-level lints (`UnusedVariable`, `UnusedMut`)
/// emitted by `emit_scope_warnings` are suppressed when the checker is
/// type-checking a stdlib (or built-in library) source body, but that the
/// same lints still fire for user-code bodies in the same compilation unit.
mod common;

use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::error::TypeErrorKind;
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Build a minimal `Checker` with no module search paths — sufficient for
/// tests that construct synthetic module graphs inline.
fn isolated_checker() -> Checker {
    Checker::new(ModuleRegistry::new(vec![]))
}

/// Construct a program whose module graph contains a single non-root module
/// with the given `mod_path` (e.g. `["std", "iter"]`) and `module_source`.
/// Returns the parsed `Program` ready for `check_program`.
fn program_with_module(mod_path: &[&str], module_source: &str) -> hew_parser::ast::Program {
    let module_result = hew_parser::parse(module_source);
    assert!(
        module_result.errors.is_empty(),
        "module source should parse cleanly: {:?}",
        module_result.errors
    );

    let root_id = ModuleId::root();
    let mod_id = ModuleId::new(mod_path.iter().map(ToString::to_string).collect());

    let module = Module {
        id: mod_id.clone(),
        items: module_result.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(module).expect("module id should be unique");
    // Module first in topo order so it is body-checked before the (empty) root.
    mg.topo_order = vec![mod_id, root_id];

    hew_parser::ast::Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    }
}

/// A stdlib module body with an internal unused `let` binding must not
/// surface an `UnusedVariable` warning to the user.
#[test]
fn stdlib_source_checker_suppresses_unused_variable() {
    let module_source = r"
        pub fn sum(a: i64, b: i64) -> i64 {
            let unused_internal = 99;
            a + b
        }
    ";
    let program = program_with_module(&["std", "iter"], module_source);
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    let unused_var_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::UnusedVariable)
        .collect();

    assert!(
        unused_var_warnings.is_empty(),
        "stdlib module body must not produce UnusedVariable warnings; got: {unused_var_warnings:#?}"
    );
}

/// A stdlib module body with a `var` binding that is never reassigned must
/// not surface an `UnusedMut` warning to the user.
#[test]
fn stdlib_source_checker_suppresses_unused_mut() {
    let module_source = r"
        pub fn identity(x: i64) -> i64 {
            var never_reassigned = x;
            never_reassigned
        }
    ";
    let program = program_with_module(&["std", "sort"], module_source);
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    let unused_mut_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::UnusedMut)
        .collect();

    assert!(
        unused_mut_warnings.is_empty(),
        "stdlib module body must not produce UnusedMut warnings; got: {unused_mut_warnings:#?}"
    );
}

/// User code's own unused variable MUST still warn — stdlib suppression must
/// not accidentally silence diagnostics in the user's root compilation unit.
#[test]
fn user_code_unused_variable_still_warns_with_stdlib_present() {
    // The std.iter module has an unused binding (suppressed),
    // but the root program also has one (must NOT be suppressed).
    let stdlib_source = r"
        pub fn noop() -> i64 {
            let stdlib_internal = 1;
            stdlib_internal
        }
    ";
    let stdlib_parsed = hew_parser::parse(stdlib_source);
    assert!(
        stdlib_parsed.errors.is_empty(),
        "stdlib parse errors: {:?}",
        stdlib_parsed.errors
    );

    // Root program: a function with an unused binding the user wrote.
    let root_source = r"
        fn user_fn() -> i64 {
            let user_unused = 77;
            0
        }

        fn main() {
            user_fn();
        }
    ";
    let root_parsed = hew_parser::parse(root_source);
    assert!(
        root_parsed.errors.is_empty(),
        "root parse errors: {:?}",
        root_parsed.errors
    );

    let root_id = ModuleId::root();
    let mod_id = ModuleId::new(vec!["std".to_string(), "iter".to_string()]);
    let stdlib_module = Module {
        id: mod_id.clone(),
        items: stdlib_parsed.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(stdlib_module).expect("unique");
    mg.topo_order = vec![mod_id, root_id];

    let program = hew_parser::ast::Program {
        module_graph: Some(mg),
        items: root_parsed.program.items,
        module_doc: None,
    };

    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    // stdlib body: no UnusedVariable for `stdlib_internal`
    let stdlib_unused: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| {
            w.kind == TypeErrorKind::UnusedVariable && w.message.contains("stdlib_internal")
        })
        .collect();
    assert!(
        stdlib_unused.is_empty(),
        "stdlib body must not warn about internal bindings; got: {stdlib_unused:#?}"
    );

    // user body: UnusedVariable for `user_unused` MUST still fire
    let user_unused: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::UnusedVariable && w.message.contains("user_unused"))
        .collect();
    assert!(
        !user_unused.is_empty(),
        "user code must still warn about unused variables; warnings: {:#?}",
        output.warnings
    );
}

/// A user file named `std.hew` produces a single-segment `ModuleId` `["std"]`.
/// This must NOT be treated as a stdlib module — the user's own unused
/// variables inside it must still warn.  This is the regression guard for
/// the single-segment over-suppression edge case.
#[test]
fn single_segment_std_user_module_still_warns_unused_variable() {
    // A user file whose name happens to be "std.hew" maps to path=["std"].
    // It is a user module, not the stdlib, so UnusedVariable must still fire.
    let module_source = r"
        pub fn user_fn() -> i64 {
            let user_unused = 42;
            0
        }
    ";
    let program = program_with_module(&["std"], module_source);
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    let unused_var_warnings: Vec<_> = output
        .warnings
        .iter()
        .filter(|w| w.kind == TypeErrorKind::UnusedVariable && w.message.contains("user_unused"))
        .collect();

    assert!(
        !unused_var_warnings.is_empty(),
        "single-segment user module named 'std' must still emit UnusedVariable; warnings: {:#?}",
        output.warnings
    );
}
