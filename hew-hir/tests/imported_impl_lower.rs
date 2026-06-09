//! Tests for HIR lowering of `impl` blocks declared in imported modules.
//!
//! Covers two properties:
//!
//! 1. **Positive**: an imported module's `impl` block whose method bodies and
//!    signatures resolve in the imported context is registered in the pre-pass
//!    and its methods are emitted as `HirItem::Function` entries in the lowered
//!    module.
//!
//! 2. **Skip**: when a method body calls a private (non-pub) function from the
//!    same module that is not reachable through the imported closure, that
//!    method is *skipped* — not emitted, and the module still imports cleanly
//!    (no module-level hard error). This matches the pre-fix behaviour where
//!    every imported impl method was dropped: importing the module compiles,
//!    and only an actual call to the skipped method fails closed downstream.
//!    Lowering such bodies needs the private-helper closure extended to impl
//!    methods (a separate lane).

use hew_hir::{HirDiagnosticKind, HirItem};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};

#[path = "support/mod.rs"]
mod support;

/// Build a `Program` whose non-root `shapes` module contains:
///  - `pub type Foo { n: i64; }`
///  - `impl Foo { pub fn bar(f: Foo) -> i64 { f.n } }`
///
/// When `with_private_helper` is `true` a private `fn helper(n: i64) -> i64`
/// is added and `bar`'s body is rewritten to call it:
///  - `fn helper(n: i64) -> i64 { n }`
///  - `impl Foo { pub fn bar(f: Foo) -> i64 { helper(f.n) } }`
fn build_imported_impl_program(with_private_helper: bool) -> Program {
    let imported_src = if with_private_helper {
        r"
pub type Foo {
    n: i64;
}
fn helper(n: i64) -> i64 { n }
impl Foo {
    pub fn bar(f: Foo) -> i64 { helper(f.n) }
}
"
    } else {
        r"
pub type Foo {
    n: i64;
}
impl Foo {
    pub fn bar(f: Foo) -> i64 { f.n }
}
"
    };

    let root_src = r"
fn main() -> i64 {
    var f = Foo { n: 42 };
    f.n
}
";

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

    let imported_id = ModuleId::new(vec!["shapes".to_string()]);
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

#[test]
fn imported_impl_methods_registered_and_emitted() {
    let program = build_imported_impl_program(false);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    // No imported-body private-helper diagnostic should be emitted.
    let blocked: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::ImportedBodyMissingPrivateHelper { .. }
            )
        })
        .collect();
    assert!(
        blocked.is_empty(),
        "expected no ImportedBodyMissingPrivateHelper diagnostics; got: {blocked:?}"
    );

    // `Foo::bar` must appear as an `HirItem::Function` in the lowered module.
    let bar_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Foo::bar"));
    assert!(
        bar_emitted,
        "expected `HirItem::Function` with name \"Foo::bar\" in lowered module items; \
         got: {:#?}",
        output
            .module
            .items
            .iter()
            .filter_map(|i| if let HirItem::Function(f) = i {
                Some(&f.name)
            } else {
                None
            })
            .collect::<Vec<_>>()
    );
}

#[test]
fn imported_impl_body_calling_private_helper_is_skipped_without_module_error() {
    let program = build_imported_impl_program(true);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    // The method whose body calls the unreachable private helper is skipped:
    // it must NOT be emitted as a `HirItem::Function`.
    let bar_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Foo::bar"));
    assert!(
        !bar_emitted,
        "expected `Foo::bar` to be skipped (not emitted) when its body calls a \
         private same-module helper unreachable across the import boundary; \
         but it was found in lowered items"
    );

    // Skipping must NOT raise a module-level hard error: importing the module
    // still compiles cleanly (matching the prior drop-everything behaviour).
    // An actual call to `Foo::bar` would fail closed downstream — but merely
    // importing the module must not.
    let blocked: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::ImportedBodyMissingPrivateHelper { .. }
            )
        })
        .collect();
    assert!(
        blocked.is_empty(),
        "skipping a private-helper-calling imported impl method must not emit a \
         module-level diagnostic; got: {blocked:?}"
    );

    let result = output.into_result();
    assert!(
        result.is_ok(),
        "into_result() must be Ok when a private-helper-calling imported impl \
         method is skipped; importing the module must compile cleanly. Got: {:#?}",
        result.err()
    );
}
