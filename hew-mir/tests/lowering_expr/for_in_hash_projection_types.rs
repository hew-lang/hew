//! MIR ownership pins for `HashSet` for-in over projected places.
//!
//! The HIR synthetic `to_vec()` call must arrive with a `HashSet` receiver and a
//! fresh `Vec` result. MIR then builds a complete `VecIter` owner, clones each
//! yield, and releases the snapshot exactly once at loop-scope exit.

use hew_mir::{Instr, IrPipeline, MirDiagnosticKind, Terminator};
use hew_types::{module_registry::ModuleRegistry, BuiltinType, Checker, ResolvedTy};

fn hir_module(source: &str) -> hew_hir::HirModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    let hir = hew_hir::lower_program_host_target(&parsed.program, &tco, &hew_hir::ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR boundary diagnostics: {:#?}",
        hir.diagnostics
    );
    hir.module
}

fn pipeline(source: &str) -> IrPipeline {
    let pipeline = hew_mir::lower_hir_module(&hir_module(source));
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

const CURSOR_LAYOUT_SOURCE: &str = r"
type OwnedRow {
    label: string;
    children: Vec<string>;
}

fn strings(xs: Vec<string>) {
    for value in xs { let _ = value.len(); }
}

fn owned_records(xs: Vec<OwnedRow>) {
    for row in xs { let _ = row.label.len() + row.children.len(); }
}

fn nested_collections(xs: Vec<Vec<string>>) {
    for inner in xs { let _ = inner.len(); }
}

fn map_owned_values(values: HashMap<string, string>) {
    let cursor = values.into_iter();
    let _ = cursor;
}
";

fn function<'a>(pipeline: &'a IrPipeline, name: &str) -> &'a hew_mir::RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("missing raw MIR for `{name}`"))
}

fn call_count(function: &hew_mir::RawMirFunction, symbol: &str) -> usize {
    function
        .blocks
        .iter()
        .filter(|block| {
            matches!(
                &block.terminator,
                Terminator::Call { callee, .. } if callee == symbol
            )
        })
        .count()
}

fn snapshot_drop_count(function: &hew_mir::RawMirFunction) -> usize {
    function
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instruction| {
            matches!(
                instruction,
                Instr::RecordFieldDrop {
                    field_offset: hew_mir::FieldOffset(0),
                    ty: hew_types::ResolvedTy::Named {
                        builtin: Some(hew_types::BuiltinType::Vec),
                        ..
                    },
                    ..
                }
            )
        })
        .count()
}

#[test]
fn projected_hashsets_lower_to_owned_veciter_snapshots_without_mir_cascades() {
    let pipeline = pipeline(
        r"
type SetBox { s: HashSet<i64>; }
type Outer { inner: SetBox; }
type OwnedBox { s: HashSet<string>; }

fn direct(s: HashSet<i64>) {
    for x in s { let _ = x; }
}

fn field(b: SetBox) {
    for x in b.s { let _ = x; }
}

fn nested(o: Outer) {
    for x in o.inner.s { let _ = x; }
}

fn tuple_field(pair: (HashSet<i64>, i64)) {
    for x in pair.0 { let _ = x; }
}

fn owned_field(b: OwnedBox) {
    for x in b.s { let _ = x.len(); }
}
",
    );

    for name in ["direct", "field", "nested", "tuple_field", "owned_field"] {
        let function = function(&pipeline, name);
        assert_eq!(
            call_count(function, "hew_hashset_to_vec_layout"),
            1,
            "`{name}` must convert the HashSet receiver exactly once: {:#?}",
            function.blocks
        );
        assert_eq!(
            call_count(function, "hew_vec_get_clone"),
            1,
            "`{name}` must advance its VecIter through the clone-out choke: {:#?}",
            function.blocks
        );
        assert_eq!(
            snapshot_drop_count(function),
            1,
            "`{name}` synthetic Vec snapshot must have one balanced cursor release: {:#?}",
            function.blocks
        );
    }
}

#[test]
fn typed_cursor_catalog_closes_string_owned_record_and_nested_collection_layouts() {
    let module = hir_module(CURSOR_LAYOUT_SOURCE);
    let owned_row = ResolvedTy::named_user("OwnedRow", vec![]);
    let nested_strings =
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String]);
    let expected = [
        (BuiltinType::VecIter, vec![ResolvedTy::String]),
        (BuiltinType::VecIter, vec![owned_row]),
        (BuiltinType::VecIter, vec![nested_strings.clone()]),
        (
            BuiltinType::HashMapIter,
            vec![ResolvedTy::String, ResolvedTy::String],
        ),
    ];

    for (builtin, args) in expected {
        let key = hew_hir::synthetic_cursor_layout_key(builtin, &args)
            .expect("test enumerates only synthetic cursor builtins");
        assert!(
            module
                .record_layouts
                .iter()
                .any(|layout| layout.mangled_name == key),
            "typed cursor catalog did not publish `{key}`; layouts: {:#?}",
            module.record_layouts
        );
    }

    let pipeline = hew_mir::lower_hir_module(&module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "cursor layouts must remain resolvable through MIR loads/stores: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn removing_a_typed_cursor_layout_fails_closed_at_mir() {
    let mut module = hir_module(CURSOR_LAYOUT_SOURCE);
    let missing = hew_hir::synthetic_cursor_layout_key(BuiltinType::VecIter, &[ResolvedTy::String])
        .expect("VecIter is a synthetic cursor");
    let before = module.record_layouts.len();
    module
        .record_layouts
        .retain(|layout| layout.mangled_name != missing);
    assert_eq!(
        module.record_layouts.len() + 1,
        before,
        "counterfactual must remove exactly the string cursor layout"
    );

    let pipeline = hew_mir::lower_hir_module(&module);
    assert!(
        pipeline.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("not registered in field-order table")
                    || construct.contains("unregistered record type")
        )),
        "missing cursor metadata must be rejected before codegen: {:#?}",
        pipeline.diagnostics
    );
}
