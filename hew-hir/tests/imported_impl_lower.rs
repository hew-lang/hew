//! Tests for HIR lowering of `impl` blocks declared in imported modules.
//!
//! Covers three properties:
//!
//! 1. **Positive**: an imported module's `impl` block whose method bodies and
//!    signatures resolve in the imported context is registered in the pre-pass
//!    and its methods are emitted as `HirItem::Function` entries in the lowered
//!    module.
//!
//! 2. **Private-helper closure**: when a method body calls a private (non-pub)
//!    function from the same module, that helper is pulled into the imported
//!    private-fn closure (impl-method bodies seed the closure alongside pub-fn
//!    bodies), so the method *is* emitted and resolves the helper — e.g.
//!    `net.set_read_timeout` calling `net_result_from_status`.
//!
//! 3. **Skip on genuinely-unresolvable call**: when a method body calls a bare
//!    name that resolves in neither `fn_registry`, the same-module rewrite map,
//!    nor the builtin variant-ctor set, the method is *skipped* — not emitted —
//!    and the module still imports cleanly (no module-level hard error). An
//!    actual call to the skipped method fails closed downstream.

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
    build_imported_impl_program_src(if with_private_helper {
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
    })
}

/// Build a `Program` from a custom `shapes`-module source plus a fixed root
/// that constructs `Foo { n: 42 }` so the imported `Foo` type is exercised.
fn build_imported_impl_program_src(imported_src: &str) -> Program {
    let root_src = r"
fn main() -> i64 {
    var f = Foo { n: 42 };
    f.n
}
";
    build_imported_module_program_src(imported_src, root_src)
}

fn build_imported_module_program_src(imported_src: &str, root_src: &str) -> Program {
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
fn imported_private_struct_typedecl_is_registered_and_emitted() {
    // Regression for std::net::tls: imported modules may keep FFI-result
    // records private, while their emitted private helpers still field-access
    // those records. The MIR boundary needs the private record TypeDecl so its
    // field order is available downstream.
    let imported_src = r"
type FfiResult {
    status: i32;
}
fn lift(raw: FfiResult) -> i64 {
    raw.status as i64
}
pub fn status() -> i64 {
    lift(FfiResult { status: 7 })
}
";
    let root_src = r"
fn main() -> i64 {
    0
}
";
    let program = build_imported_module_program_src(imported_src, root_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    let ffi_result = output.module.items.iter().find_map(|item| {
        if let HirItem::TypeDecl(td) = item {
            (td.name == "FfiResult").then_some(td)
        } else {
            None
        }
    });
    let ffi_result =
        ffi_result.expect("expected private imported `FfiResult` TypeDecl to be emitted");
    assert_eq!(
        ffi_result
            .fields
            .iter()
            .map(|field| field.name.as_str())
            .collect::<Vec<_>>(),
        vec!["status"],
        "private imported `FfiResult` must carry its field order"
    );

    let lift_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name.ends_with("lift")));
    assert!(
        lift_emitted,
        "expected private helper using `FfiResult.status` to be emitted"
    );

    let result = output.into_result();
    assert!(
        result.is_ok(),
        "private imported record field access must lower cleanly. Got: {:#?}",
        result.err()
    );
}

#[test]
fn unused_imported_private_struct_typedecl_is_harmlessly_emitted() {
    // The chosen scope mirrors public imported records: all private imported
    // struct TypeDecls are emitted, even when no emitted body references them.
    let imported_src = r"
type UnusedPrivate {
    status: i32;
}
pub fn ping() -> i64 {
    1
}
";
    let root_src = r"
fn main() -> i64 {
    0
}
";
    let program = build_imported_module_program_src(imported_src, root_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    let unused_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::TypeDecl(td) if td.name == "UnusedPrivate"));
    assert!(
        unused_emitted,
        "expected unused private imported struct TypeDecl to be emitted harmlessly"
    );

    let result = output.into_result();
    assert!(
        result.is_ok(),
        "unused private imported struct TypeDecl must not introduce diagnostics. Got: {:#?}",
        result.err()
    );
}

#[test]
fn imported_private_generic_struct_typedecl_stays_unemitted() {
    // Generic private handles such as std::stream::Stream<T>/Sink<T> have
    // intrinsic codegen paths. Emitting them as ordinary record TypeDecls would
    // misclassify those handles at the MIR/codegen boundary.
    let imported_src = r"
type PrivateBox<T> {
    value: T;
}
pub fn ping() -> i64 {
    1
}
";
    let root_src = r"
fn main() -> i64 {
    0
}
";
    let program = build_imported_module_program_src(imported_src, root_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    let generic_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::TypeDecl(td) if td.name == "PrivateBox"));
    assert!(
        !generic_emitted,
        "private imported generic struct TypeDecls must stay on their existing non-record path"
    );

    let result = output.into_result();
    assert!(
        result.is_ok(),
        "skipping a private imported generic struct TypeDecl must remain clean. Got: {:#?}",
        result.err()
    );
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
fn imported_impl_body_using_ok_err_ctor_is_not_skipped() {
    // A method whose body constructs `Ok(..)` / `Err(..)` must NOT be skipped:
    // the variant constructors resolve through `machine_ctor_registry`, not
    // `fn_registry`, so they are not "unresolvable" bare calls. Regression for
    // the over-aggressive body-unresolvable gate that dropped every ADT-
    // returning imported impl method (e.g. `Conn::try_send`, `Url::port`).
    let imported_src = "
pub type Foo {
    n: i64;
}
impl Foo {
    pub fn try_get(f: Foo) -> Result<i64, string> {
        if f.n < 0 {
            Err(\"negative\")
        } else {
            Ok(f.n)
        }
    }
}
";
    let program = build_imported_impl_program_src(imported_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    let try_get_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Foo::try_get"));
    assert!(
        try_get_emitted,
        "expected `Foo::try_get` to be emitted (not skipped) even though its body \
         constructs `Ok(..)`/`Err(..)`; got: {:#?}",
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
fn imported_impl_signature_returning_same_module_record_is_emitted() {
    // Regression for the imported-impl signature gate used by
    // std::text::regex: methods such as `Pattern::captures` return a public
    // record declared in the same imported module (`CaptureMatches`). Once the
    // imported-module pre-pass has registered that record, the method signature
    // is resolvable at the MIR boundary and must not be skipped.
    let imported_src = r"
pub type Foo {
    n: i64;
}
pub type CaptureMatches {
    groups: Vec<string>;
    group_count: i64;
}
impl Foo {
    pub fn captures(f: Foo) -> CaptureMatches {
        CaptureMatches {
            groups: Vec::<string>::new(),
            group_count: f.n,
        }
    }
}
";
    let program = build_imported_impl_program_src(imported_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    let captures_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Foo::captures"));
    assert!(
        captures_emitted,
        "expected `Foo::captures` to be emitted: its return type is a public \
         same-module record registered by the imported-module pre-pass; got: {:#?}",
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

    let capture_matches_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::TypeDecl(td) if td.name == "CaptureMatches"));
    assert!(
        capture_matches_emitted,
        "expected imported same-module record `CaptureMatches` to be emitted"
    );

    let result = output.into_result();
    assert!(
        result.is_ok(),
        "imported impl method returning a same-module registered record must \
         compile cleanly. Got: {:#?}",
        result.err()
    );
}

#[test]
fn imported_impl_body_calling_private_helper_is_emitted_via_closure() {
    // A method body that calls a private same-module helper is now lowered: the
    // helper is pulled into the imported private-fn closure because impl-method
    // bodies seed the closure (alongside pub-fn bodies). `Foo::bar` calling
    // private `helper` must therefore be EMITTED, not skipped. This is the
    // imported-impl-method analogue of the free-fn private-helper closure, and
    // is what lets `net.set_read_timeout` reach `net_result_from_status`.
    let program = build_imported_impl_program(true);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    let bar_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Foo::bar"));
    assert!(
        bar_emitted,
        "expected `Foo::bar` to be emitted: its private same-module helper is \
         reachable through the impl-method private-fn closure; got: {:#?}",
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

    // The private helper itself must also be emitted under its qualified name so
    // the method body's call resolves.
    let helper_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name.ends_with("helper")));
    assert!(
        helper_emitted,
        "expected the private `helper` to be emitted (qualified) so `Foo::bar` \
         resolves it; got: {:#?}",
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

    let result = output.into_result();
    assert!(
        result.is_ok(),
        "importing a module whose impl method calls a reachable private helper \
         must compile cleanly. Got: {:#?}",
        result.err()
    );
}

#[test]
fn imported_impl_body_with_unresolvable_call_is_skipped_without_module_error() {
    // Negative guard: a method whose body calls a bare name that is NOT a
    // same-module fn, NOT in the rewrite map, and NOT a builtin variant ctor is
    // genuinely unresolvable cross-module and must still be skipped — not
    // emitted — and importing the module must not raise a module-level error.
    let imported_src = r"
pub type Foo {
    n: i64;
}
impl Foo {
    pub fn bar(f: Foo) -> i64 { nonexistent_fn(f.n) }
}
";
    let program = build_imported_impl_program_src(imported_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    let bar_emitted = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(f) if f.name == "Foo::bar"));
    assert!(
        !bar_emitted,
        "expected `Foo::bar` to be skipped when its body calls a genuinely \
         unresolvable bare name; but it was found in lowered items"
    );

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
        "skipping an unresolvable-call imported impl method must not emit a \
         module-level diagnostic; got: {blocked:?}"
    );
}
