use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn emit_ll(source: &str, module_name: &str) -> String {
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
    let verify = hew_hir::verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir: {:?} verify: {:?}",
        output.diagnostics,
        verify
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-tuple-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("tuple pipeline must emit");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

#[test]
fn tuple_construct_emits_struct_stores() {
    let ll = emit_ll("fn main() -> (i64, bool) { (42, true) }", "tuple_basic");

    // Check that the LLVM IR contains struct stores for tuple construction
    assert!(
        ll.contains("getelementptr") || ll.contains("insertvalue"),
        "tuple construction must emit struct field stores; got:\n{ll}"
    );

    // Check for tuple struct type definition
    assert!(
        ll.contains("{ i64, i8 }") || ll.contains("{ i64, i1 }"),
        "tuple type (i64, bool) must emit as struct; got:\n{ll}"
    );
}

#[test]
fn tuple_swap_function_emits() {
    let ll = emit_ll(
        r"
fn swap(a: i64, b: i64) -> (i64, i64) {
    (b, a)
}

fn main() -> i64 {
    let (x, y) = swap(1, 2);
    x + y
}
",
        "tuple_swap",
    );

    // Check that swap function emits tuple construction
    assert!(
        ll.contains("@swap"),
        "swap function must be emitted; got:\n{ll}"
    );

    // Check for tuple struct type
    assert!(
        ll.contains("{ i64, i64 }"),
        "tuple type (i64, i64) must emit as struct; got:\n{ll}"
    );
}

#[test]
fn tuple_let_destructuring_compiles() {
    let ll = emit_ll(
        r"
fn main() -> i64 {
    let (x, y) = (1, 2);
    x + y
}
",
        "tuple_let",
    );

    // Verify tuple construction IR shape:
    // 1. Struct alloca for the tuple result
    // 2. GEP + store for each element in source order (1, 2)

    assert!(
        ll.contains("@main"),
        "main function must be emitted; got:\n{ll}"
    );

    // The tuple literal (1, 2) should emit as struct construction
    assert!(
        ll.contains("alloca") && (ll.contains("{ i64, i64 }") || ll.contains("%Tuple")),
        "tuple construction must allocate struct storage; got:\n{ll}"
    );

    // GEP for field access during construction
    assert!(
        ll.contains("getelementptr"),
        "tuple construction must use GEP for field stores; got:\n{ll}"
    );

    // The literal constants 1 and 2 should appear in stores
    assert!(
        ll.contains("store")
            && (ll.contains("i64 1") || ll.contains("1,"))
            && (ll.contains("i64 2") || ll.contains("2,")),
        "tuple construction must store literal values 1 and 2; got:\n{ll}"
    );

    // Check for add instruction (x + y)
    assert!(
        ll.contains("add"),
        "tuple element addition must emit; got:\n{ll}"
    );
}
