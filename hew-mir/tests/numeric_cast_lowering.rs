use hew_mir::{Instr, IrPipeline};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy, TryConversionKind};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    hew_mir::lower_hir_module(&output.module)
}

#[test]
fn lowers_try_to_numeric_method_to_try_width_cast_instr() {
    let p = pipeline(
        r"
        fn main() -> Option<i32> {
            let x: i64 = 7;
            x.try_to_i32()
        }
        ",
    );
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);

    let main = p
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main function lowered");
    let casts: Vec<_> = main
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::TryWidthCast {
                from_ty,
                to_ty,
                kind,
                ..
            } => Some((from_ty.clone(), to_ty.clone(), *kind)),
            _ => None,
        })
        .collect();

    assert_eq!(
        casts,
        vec![(
            ResolvedTy::I64,
            ResolvedTy::I32,
            TryConversionKind::IntToInt
        )]
    );
}

#[test]
fn lowers_full_checker_admitted_matrix_examples_to_numeric_cast_instrs() {
    let p = pipeline(
        r"
        fn main() -> i64 {
            let x: i64 = 7;
            let y: i32 = x as i32;
            let z: i64 = y as i64;
            let f: f64 = z as f64;
            let back: i64 = f as i64;
            let one: u8 = true as u8;
            let ok: bool = one as bool;
            if ok { back } else { 0 }
        }
        ",
    );
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);

    let main = p
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main function lowered");
    let casts: Vec<_> = main
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::NumericCast { from_ty, to_ty, .. } => Some((from_ty.clone(), to_ty.clone())),
            _ => None,
        })
        .collect();

    assert_eq!(casts.len(), 6, "numeric casts: {casts:#?}");
    for expected in [
        (ResolvedTy::I64, ResolvedTy::I32),
        (ResolvedTy::I32, ResolvedTy::I64),
        (ResolvedTy::I64, ResolvedTy::F64),
        (ResolvedTy::F64, ResolvedTy::I64),
        (ResolvedTy::Bool, ResolvedTy::U8),
        (ResolvedTy::U8, ResolvedTy::Bool),
    ] {
        assert!(
            casts.contains(&expected),
            "missing NumericCast {expected:?}; got {casts:#?}"
        );
    }
}
