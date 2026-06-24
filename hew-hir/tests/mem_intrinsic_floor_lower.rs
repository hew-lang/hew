//! W5.005 / F1b — HIR lowering of the `#[intrinsic("mem.*")]` memory floor.
//!
//! Regression guard for D343: before this fix, `#[intrinsic]` functions
//! declared in an *imported* floor module (`std.mem`) reached HIR lowering via
//! the module-graph path (`lower_imported_fn_with_name`), which never consulted
//! `intrinsic_declarations`. The body-skip only fired on the *root-item* path,
//! so `mem$alloc` was emitted as a defined empty-body function — a silent
//! fail-OPEN no-op returning an uninitialised pointer.
//!
//! These tests assert the corrected behaviour:
//!  - a callable `mem.*` floor intrinsic (catalog linkage
//!    `CalleeNameDispatchOnly`) is still emitted as a `HirItem::Function` (so
//!    its mangled symbol stays in MIR's `module_fn_names` and calls dispatch to
//!    it) AND carries `intrinsic_id = Some("mem.*")` so codegen synthesizes the
//!    body;
//!  - a numeric `math.*` intrinsic (linkage `CompilerIntrinsic`) is NOT emitted
//!    as a function (it routes through builtin method-rewrites);
//!  - an unknown `#[intrinsic("…")]` key fails closed with `UnknownIntrinsic`.

use hew_hir::{lower_program_host_target, HirDiagnosticKind, HirFn, HirItem, ResolutionCtx};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

/// Build a `Program` with a non-root floor module at `module_path`
/// (e.g. `["std", "mem"]`) containing `floor_src`, imported by a trivial root.
fn build_program_with_floor_module(module_path: &[&str], floor_src: &str) -> Program {
    let floor = hew_parser::parse(floor_src);
    assert!(
        floor.errors.is_empty(),
        "floor module parse errors: {:?}",
        floor.errors
    );
    let root = hew_parser::parse("fn main() -> i64 { 0 }");
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let floor_id = ModuleId::new(module_path.iter().map(ToString::to_string).collect());
    let root_id = ModuleId::root();

    let floor_items: Vec<_> = floor
        .program
        .items
        .iter()
        .filter(|(item, _)| !matches!(item, Item::Import(_)))
        .cloned()
        .collect();

    let floor_module = Module {
        id: floor_id.clone(),
        items: floor_items,
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
    graph.add_module(floor_module).expect("add floor");
    graph.add_module(root_module).expect("add root");
    graph.topo_order = vec![floor_id, root_id];

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

fn function_by_name<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> Option<&'a HirFn> {
    output.module.items.iter().find_map(|item| {
        if let HirItem::Function(f) = item {
            (f.name == name).then_some(f)
        } else {
            None
        }
    })
}

const MEM_FLOOR_SRC: &str = r#"
#[intrinsic("mem.alloc")]
pub fn alloc(size: u64, align: u64) -> *mut u8 {}

#[intrinsic("mem.dealloc")]
pub fn dealloc(ptr: *mut u8, size: u64, align: u64) {}
"#;

#[test]
fn imported_mem_intrinsic_is_emitted_and_tagged_with_catalog_id() {
    let program = build_program_with_floor_module(&["std", "mem"], MEM_FLOOR_SRC);
    let (output, tco) = lower_with_checker(&program);

    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    // The checker recorded the qualified intrinsic key.
    assert_eq!(
        tco.intrinsic_declarations
            .get("std.mem.alloc")
            .map(String::as_str),
        Some("mem.alloc"),
        "checker must record std.mem.alloc → mem.alloc; got: {:?}",
        tco.intrinsic_declarations
    );

    // D343 regression: `mem$alloc` must STILL be emitted as a function (so its
    // symbol stays in MIR's module_fn_names and calls dispatch to it) …
    let alloc = function_by_name(&output, "mem$alloc")
        .expect("mem$alloc must be emitted as a HirItem::Function");
    // … and it must be TAGGED so codegen synthesizes the body rather than
    // lowering the bodyless placeholder into a fail-OPEN empty function.
    assert_eq!(
        alloc.intrinsic_id.as_deref(),
        Some("mem.alloc"),
        "mem$alloc must carry intrinsic_id = Some(\"mem.alloc\")"
    );

    let dealloc = function_by_name(&output, "mem$dealloc")
        .expect("mem$dealloc must be emitted as a HirItem::Function");
    assert_eq!(dealloc.intrinsic_id.as_deref(), Some("mem.dealloc"));
}

#[test]
fn imported_math_intrinsic_is_not_emitted_as_a_function() {
    // `math.*` uses catalog linkage `CompilerIntrinsic` — it routes through
    // builtin method-rewrites and must NOT be emitted as a dead empty-body
    // shell (which, post-Slice-3b, would also trip codegen's fail-closed
    // authority on an id it cannot synthesize).
    let program = build_program_with_floor_module(
        &["std", "math"],
        "#[intrinsic(\"math.sqrt\")]\npub fn sqrt(x: f64) -> f64 {}\n",
    );
    let (output, tco) = lower_with_checker(&program);

    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    assert!(
        function_by_name(&output, "math$sqrt").is_none(),
        "math$sqrt must not be emitted as a HirItem::Function"
    );
}

#[test]
fn unknown_intrinsic_key_fails_closed() {
    // A `#[intrinsic("…")]` key with no matching catalog entry must surface
    // `UnknownIntrinsic` rather than silently lowering a no-op body.
    let program = build_program_with_floor_module(
        &["std", "mem"],
        "#[intrinsic(\"mem.does_not_exist\")]\npub fn bogus(x: u64) -> u64 {}\n",
    );
    let (output, _tco) = lower_with_checker(&program);

    assert!(
        output.diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::UnknownIntrinsic { intrinsic_key, .. }
                if intrinsic_key == "mem.does_not_exist"
        )),
        "unknown intrinsic key must fail closed with UnknownIntrinsic; diagnostics: {:#?}",
        output.diagnostics
    );
    assert!(
        function_by_name(&output, "mem$bogus").is_none(),
        "an unknown intrinsic must not be emitted as a callable function"
    );
}
