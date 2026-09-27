//! Imported intrinsic declarations retain checked signatures without executable
//! placeholder bodies. Actual calls require a supported lowering contract;
//! unknown catalogue keys remain boundary errors.

use hew_hir::{lower_program_host_target, HirDiagnosticKind, HirFn, HirItem, ResolutionCtx};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModulePath};
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

    let floor_id = ModulePath::new(module_path.iter());
    let root_id = ModulePath::root();

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
        source_paths: vec![std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-hir has a workspace parent")
            .join("std")
            .join(module_path.iter().skip(1).collect::<std::path::PathBuf>())
            .join(format!(
                "{}.hew",
                module_path.last().expect("floor module leaf")
            ))],
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
fn imported_mem_intrinsics_retain_signatures_without_executable_bodies() {
    let program = build_program_with_floor_module(&["std", "mem"], MEM_FLOOR_SRC);
    let (output, tco) = lower_with_checker(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );

    for (source, key, parameters) in [
        ("std.mem.alloc", "mem.alloc", 2),
        ("std.mem.dealloc", "mem.dealloc", 3),
    ] {
        let declaration = tco.defs.lookup_path(source).expect("checked declaration");
        let signature = &tco.fn_sigs[&declaration];
        assert_eq!(signature.params.len(), parameters);
        assert_eq!(
            tco.intrinsic_declarations.get(source).map(String::as_str),
            Some(key)
        );
        assert!(
            !output.module.items.iter().any(|item| matches!(item,
                HirItem::Function(function) if function.declaration == declaration
            )),
            "{source} must not acquire an executable placeholder body"
        );
    }
    let alloc = tco.defs.lookup_path("std.mem.alloc").unwrap();
    assert!(matches!(
        tco.fn_sigs[&alloc].return_type,
        hew_types::Ty::Pointer {
            is_mutable: true,
            ..
        }
    ));
    let dealloc = tco.defs.lookup_path("std.mem.dealloc").unwrap();
    assert_eq!(tco.fn_sigs[&dealloc].return_type, hew_types::Ty::Unit);
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
        function_by_name(&output, "std$math$sqrt").is_none(),
        "std$math$sqrt must not be emitted as a HirItem::Function"
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
        function_by_name(&output, "std$mem$bogus").is_none(),
        "an unknown intrinsic must not be emitted as a callable function"
    );
}
