//! Tests for HIR lowering of `pub fn` free functions declared in imported
//! modules — specifically W4.018 Stage 1 diagnostic-precision behavior.
//!
//! Stage 1 introduces two fail-closed diagnostics for imported pub
//! free-function bodies that reference same-module callees the importer
//! emission cannot resolve:
//!
//! - `ImportedBodyMissingPrivateHelper { item_kind: FreeFn, .. }` —
//!   bare call to a same-module *private* helper. Private helpers are not
//!   lowered into the importer's HIR; before Stage 1 this leaked out as a
//!   bare `UnresolvedSymbol`.
//! - `ImportedFreeFnBodyUnresolvedBareCall { module, callee, .. }` —
//!   bare call to another same-module *pub* function. Imported pub fns are
//!   emitted under their mangled `module.callee` name, so the bare-name
//!   call doesn't resolve until Stage 2 rewrites bare callees.
//!
//! Truly unknown names (not same-module private and not same-module pub)
//! must still surface as `UnresolvedSymbol` — the new diagnostics never
//! shadow the existing one.

use hew_hir::{lower_program, HirDiagnosticKind, HirItem, ImportedItemKind, ResolutionCtx};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::TypeCheckOutput;

/// Build a `Program` containing a non-root module `m` with arbitrary source
/// and a trivial root that just calls `m.entry()` (so the program is a
/// well-formed import graph).
fn build_program_with_imported_module(imported_src: &str) -> Program {
    let root_src = r"
fn main() -> i64 { 0 }
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

#[test]
fn imported_pub_free_fn_body_calling_private_helper_emits_fail_closed_diagnostic() {
    // `entry` is a pub fn whose body calls a private same-module helper.
    // Stage 1: emit `ImportedBodyMissingPrivateHelper { FreeFn }` and skip
    // emitting `entry` as an HirItem::Function.
    let src = r"
fn helper(n: i64) -> i64 { n }
pub fn entry(n: i64) -> i64 { helper(n) }
";
    let program = build_program_with_imported_module(src);
    let output = lower_program(
        &program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let matching: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                &d.kind,
                HirDiagnosticKind::ImportedBodyMissingPrivateHelper {
                    module,
                    helper_fn,
                    item_kind: ImportedItemKind::FreeFn,
                } if module == "m" && helper_fn == "helper"
            )
        })
        .collect();
    assert!(
        !matching.is_empty(),
        "expected ImportedBodyMissingPrivateHelper {{ module: \"m\", \
         helper_fn: \"helper\", item_kind: FreeFn }}; got: {:#?}",
        output.diagnostics
    );

    // `entry` must NOT be emitted (mangled `m.entry`) because its body is
    // blocked.
    let entry_emitted = output.module.items.iter().any(
        |item| matches!(item, HirItem::Function(f) if f.name == "m$entry" || f.name == "entry"),
    );
    assert!(
        !entry_emitted,
        "expected `entry` NOT to be emitted when body calls a blocked \
         private helper; lowered items: {:#?}",
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

    // Fail-closed boundary: into_result() must return Err.
    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err when ImportedBodyMissingPrivateHelper \
         (FreeFn) is present"
    );
}

#[test]
fn imported_pub_free_fn_body_bare_call_to_same_module_pub_emits_fail_closed_diagnostic() {
    // `entry` is a pub fn whose body calls another same-module *pub* fn
    // (`peer`) by its bare name. Imported emission mangles `peer` to
    // `m.peer`, so the bare-name call would resolve to nothing. Stage 1
    // emits `ImportedFreeFnBodyUnresolvedBareCall` instead of letting the
    // call leak as bare `UnresolvedSymbol`.
    let src = r"
pub fn peer(n: i64) -> i64 { n }
pub fn entry(n: i64) -> i64 { peer(n) }
";
    let program = build_program_with_imported_module(src);
    let output = lower_program(
        &program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let matching: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                &d.kind,
                HirDiagnosticKind::ImportedFreeFnBodyUnresolvedBareCall {
                    module,
                    callee,
                    suggested_qualified,
                } if module == "m"
                    && callee == "peer"
                    && suggested_qualified == "m$peer"
            )
        })
        .collect();
    assert!(
        !matching.is_empty(),
        "expected ImportedFreeFnBodyUnresolvedBareCall {{ module: \"m\", \
         callee: \"peer\", suggested_qualified: \"m$peer\" }}; got: {:#?}",
        output.diagnostics
    );

    // `entry` must NOT be emitted; `peer` itself has no bare-call problem,
    // so it should still be emitted under its mangled name.
    let names: Vec<_> = output
        .module
        .items
        .iter()
        .filter_map(|i| {
            if let HirItem::Function(f) = i {
                Some(f.name.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        !names.contains(&"m$entry"),
        "expected `m$entry` NOT to be emitted when its body has a blocked \
         bare same-module pub call; names: {names:?}"
    );
    assert!(
        names.contains(&"m$peer"),
        "expected `m$peer` to be emitted (its own body has no blockers); \
         names: {names:?}"
    );

    // Fail-closed boundary.
    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err when ImportedFreeFnBodyUnresolvedBareCall \
         is present"
    );
}

#[test]
fn imported_pub_free_fn_self_recursion_does_not_emit_bare_call_diagnostic() {
    // A pub fn that calls *itself* by bare name must NOT trigger the
    // same-module pub bare-call diagnostic: the bare name lowers through
    // `lower_identifier` to a known reference. Only references to *other*
    // same-module pub fns are blocked in Stage 1.
    let src = r"
pub fn entry(n: i64) -> i64 { if n == 0 { 0 } else { entry(n - 1) } }
";
    let program = build_program_with_imported_module(src);
    let output = lower_program(
        &program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let any_blocked = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::ImportedFreeFnBodyUnresolvedBareCall { .. }
                | HirDiagnosticKind::ImportedBodyMissingPrivateHelper { .. }
        )
    });
    assert!(
        !any_blocked,
        "self-recursion must not trigger imported-body bare-call diagnostics; \
         got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn imported_pub_free_fn_unknown_bare_call_still_surfaces_as_unresolved_symbol() {
    // `entry` calls a bare name that is NOT defined in module `m` at all
    // (neither private nor pub). The new fail-closed diagnostics must NOT
    // fire; the existing `UnresolvedSymbol` path stays in charge of this
    // case (the brief's hard requirement: preserve UnresolvedSymbol for
    // truly unknown names).
    let src = r"
pub fn entry(n: i64) -> i64 { totally_unknown(n) }
";
    let program = build_program_with_imported_module(src);
    let output = lower_program(
        &program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let any_new_kind = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::ImportedFreeFnBodyUnresolvedBareCall { .. }
                | HirDiagnosticKind::ImportedBodyMissingPrivateHelper { .. }
        )
    });
    assert!(
        !any_new_kind,
        "unknown bare callee must not trigger the W4.018 fail-closed kinds; \
         it must continue to surface as UnresolvedSymbol downstream. \
         Diagnostics: {:#?}",
        output.diagnostics
    );
}
