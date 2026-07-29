//! MIR ownership pins for `HashSet` for-in over projected places.
//!
//! The HIR synthetic `to_vec()` call must arrive with a `HashSet` receiver and a
//! fresh `Vec` result. MIR then builds a complete `VecIter` owner, clones each
//! yield, and releases the snapshot exactly once at loop-scope exit.

use hew_mir::{Instr, IrPipeline, Terminator};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline(source: &str) -> IrPipeline {
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
    let pipeline = hew_mir::lower_hir_module(&hir.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

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
