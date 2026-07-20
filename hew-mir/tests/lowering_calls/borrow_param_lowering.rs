//! Foreign `&T` signature preservation at the HIR-to-MIR boundary.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

#[test]
fn ffi_borrow_signature_survives_in_extern_declarations() {
    let pipeline =
        pipeline_with_tc("extern \"C\" { fn get() -> &i64; fn read(value: &i64) -> i64; }");
    assert_eq!(pipeline.extern_decls.len(), 2);

    let get = pipeline
        .extern_decls
        .iter()
        .find(|decl| decl.name == "get")
        .expect("get extern declaration");
    assert!(get.param_tys.is_empty());
    assert_eq!(
        get.return_ty,
        ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::I64),
        }
    );

    let read = pipeline
        .extern_decls
        .iter()
        .find(|decl| decl.name == "read")
        .expect("read extern declaration");
    assert_eq!(
        read.param_tys,
        vec![ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::I64),
        }]
    );
    assert_eq!(read.return_ty, ResolvedTy::I64);
}
