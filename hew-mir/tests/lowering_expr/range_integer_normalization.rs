//! Mixed-width for-range operands normalize at the MIR semantic boundary.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{Instr, IrPipeline, Place};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn lower_checked(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let check_output = checker.check_program(&parsed.program);
    assert!(
        check_output.errors.is_empty(),
        "type errors: {:?}",
        check_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &check_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        hir.diagnostics
    );
    hew_mir::lower_hir_module(&hir.module)
}

#[test]
fn signed_bounds_normalize_for_every_range_direction_and_inclusivity() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let narrow_lo: i32 = -2;
            let narrow_hi: i32 = 2;
            let wide_lo: i64 = -1;
            let wide_hi: i64 = 3;
            for value in narrow_lo .. wide_hi { let copy = value; }
            for value in wide_lo ..= narrow_hi { let copy = value; }
            for value in (narrow_lo .. wide_hi).rev() { let copy = value; }
            for value in (wide_lo ..= narrow_hi).rev() { let copy = value; }
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
    let main = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main MIR");
    let casts = main
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter(|instr| {
            matches!(
                instr,
                Instr::NumericCast {
                    from_ty: ResolvedTy::I32,
                    to_ty: ResolvedTy::I64,
                    ..
                }
            )
        })
        .count();
    assert_eq!(
        casts, 4,
        "each narrow start/end must sign-extend exactly once"
    );

    for instr in main.blocks.iter().flat_map(|block| &block.instructions) {
        let Instr::Move {
            dest: Place::Local(dest),
            src: Place::Local(src),
        } = instr
        else {
            continue;
        };
        assert_eq!(
            main.locals[*dest as usize], main.locals[*src as usize],
            "ordinary local Move must not carry an implicit integer-width change: {instr:?}"
        );
    }
}

#[test]
fn unsigned_bounds_use_typed_zero_extending_normalization() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let lo: u16 = 65534;
            let hi: u64 = 65537;
            for value in lo .. hi { let copy = value; }
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    let main = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main MIR");
    assert!(main
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(|instr| matches!(
            instr,
            Instr::NumericCast {
                from_ty: ResolvedTy::U16,
                to_ty: ResolvedTy::U64,
                ..
            }
        )));
}

#[test]
fn numeric_branch_joins_normalize_if_and_if_let_arms() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let flag = true;
            let narrow_signed: i32 = -2;
            let wide_signed: i64 = 4;
            let signed = if flag { narrow_signed } else { wide_signed };
            let narrow_unsigned: u16 = 65534;
            let wide_unsigned: u64 = 65537;
            let unsigned = if flag { narrow_unsigned } else { wide_unsigned };
            let float = if flag { narrow_signed } else { 4.5 };
            let value: Option<i64> = Some(1);
            let signed_if_let = if let .Some(_) = value {
                narrow_signed
            } else {
                wide_signed
            };
            println(signed);
            println(unsigned);
            println(float);
            println(signed_if_let);
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );

    let main = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main MIR");
    let has_cast = |from_ty: &ResolvedTy, to_ty: &ResolvedTy| {
        main.blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| {
                matches!(
                    instr,
                    Instr::NumericCast {
                        from_ty: actual_from,
                        to_ty: actual_to,
                        ..
                    } if actual_from == from_ty && actual_to == to_ty
                )
            })
    };

    assert!(has_cast(&ResolvedTy::I32, &ResolvedTy::I64));
    assert!(has_cast(&ResolvedTy::U16, &ResolvedTy::U64));
    assert!(has_cast(&ResolvedTy::I32, &ResolvedTy::F64));
}
