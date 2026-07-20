//! Foreign borrow declarations remain pointer-shaped and non-owning in LLVM.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{ExternDecl, IrPipeline};
use hew_types::ResolvedTy;

fn emit_borrow_declarations() -> String {
    let borrow_i64 = ResolvedTy::Borrow {
        pointee: Box::new(ResolvedTy::I64),
    };
    let pipeline = IrPipeline {
        extern_decls: vec![
            ExternDecl {
                name: "ffi_borrow_get".to_string(),
                abi: "C".to_string(),
                param_tys: vec![],
                return_ty: borrow_i64.clone(),
                provenance: hew_hir::ExternProvenance::Root,
                runtime_capability: None,
                malloc_string_return: false,
            },
            ExternDecl {
                name: "ffi_borrow_read".to_string(),
                abi: "C".to_string(),
                param_tys: vec![borrow_i64],
                return_ty: ResolvedTy::I64,
                provenance: hew_hir::ExternProvenance::Root,
                runtime_capability: None,
                malloc_string_return: false,
            },
        ],
        ..IrPipeline::default()
    };
    let out_dir = std::env::temp_dir().join("hew-extern-borrow-abi");
    std::fs::create_dir_all(&out_dir).expect("create output directory");
    let options = EmitOptions {
        module_name: "extern_borrow_abi",
        out_dir: &out_dir,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("emit borrow declarations");
    std::fs::read_to_string(artefacts.ll_path.expect("LLVM IR path")).expect("read LLVM IR")
}

#[test]
fn extern_borrow_is_one_pointer_without_ownership_helpers() {
    let ll = emit_borrow_declarations();
    assert!(
        ll.contains("declare ptr @ffi_borrow_get()"),
        "borrow return must be one pointer:\n{ll}",
    );
    assert!(
        ll.contains("declare i64 @ffi_borrow_read(ptr)"),
        "borrow parameter must be one pointer:\n{ll}",
    );
    for forbidden in [
        "call ptr @hew_string_clone",
        "call void @hew_string_drop",
        "call ptr @hew_bytes_clone_ref",
        "call void @hew_bytes_drop",
        "define void @__hew_record_clone",
        "define void @__hew_record_drop",
    ] {
        assert!(
            !ll.contains(forbidden),
            "foreign views must not emit retain/drop helper `{forbidden}`:\n{ll}",
        );
    }
}
