//! Tests for HIR lowering of `impl` blocks declared in imported modules.
//!
//! Covers two properties introduced by the fix:
//!
//! 1. **Positive**: an imported module's `impl` block whose method bodies do
//!    not call private helpers is registered in the pre-pass and its methods
//!    are emitted as `HirItem::Function` entries in the lowered module.
//!
//! 2. **Negative**: when a method body calls a private (non-pub) function from
//!    the same module the lowering emits
//!    `HirDiagnosticKind::ImportedImplBodyMissingPrivateHelper` instead of
//!    silently failing or producing a bare `UnresolvedSymbol`.

use hew_hir::{lower_program, HirDiagnosticKind, HirItem, ResolutionCtx};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::TypeCheckOutput;

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
    let output = lower_program(
        &program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    // No private-helper diagnostic should be emitted.
    let blocked: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::ImportedImplBodyMissingPrivateHelper { .. }
            )
        })
        .collect();
    assert!(
        blocked.is_empty(),
        "expected no ImportedImplBodyMissingPrivateHelper diagnostics; got: {blocked:?}"
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
fn imported_impl_body_calling_private_helper_emits_diagnostic() {
    let program = build_imported_impl_program(true);
    let output = lower_program(
        &program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    // Must emit exactly one ImportedImplBodyMissingPrivateHelper for `helper`.
    let blocked: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::ImportedImplBodyMissingPrivateHelper { .. }
            )
        })
        .collect();
    assert!(
        !blocked.is_empty(),
        "expected at least one ImportedImplBodyMissingPrivateHelper diagnostic; got none. \
         All diagnostics: {:#?}",
        output.diagnostics
    );

    let has_helper = blocked.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::ImportedImplBodyMissingPrivateHelper {
                module,
                helper_fn,
            } if module == "shapes" && helper_fn == "helper"
        )
    });
    assert!(
        has_helper,
        "expected ImportedImplBodyMissingPrivateHelper {{ module: \"shapes\", \
         helper_fn: \"helper\" }}; got: {blocked:?}"
    );

    // The blocked method must NOT be emitted as HirItem::Function.
    let bar_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Foo::bar"));
    assert!(
        !bar_emitted,
        "expected `Foo::bar` NOT to be emitted when body calls private helper; \
         but it was found in lowered items"
    );

    // Fail-closed boundary contract: `into_result()` must return `Err` when
    // `ImportedImplBodyMissingPrivateHelper` is present — downstream consumers
    // that call `into_result()` must not silently receive a broken module.
    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err when ImportedImplBodyMissingPrivateHelper \
         is present; got Ok — fail-closed boundary is broken"
    );
    let err_diags = result.unwrap_err();
    assert!(
        err_diags.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::ImportedImplBodyMissingPrivateHelper { .. }
        )),
        "Err diagnostics must contain ImportedImplBodyMissingPrivateHelper; \
         got: {err_diags:#?}",
    );
}
