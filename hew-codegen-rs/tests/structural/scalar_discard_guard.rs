//! Fail-closed guard: pointer/aggregate-returning extern discarded in statement
//! position (`dest == None`, `!returns_unit`) must surface
//! `CodegenError::FailClosed`; scalar (integer/float) discards must succeed.
//!
//! Background: `conn.close()` and `ln.close()` lower to externs that return
//! `i32` (status code) in statement position, producing `Terminator::Call`
//! with `dest = None`. The codegen discard arm was narrowed to permit only
//! scalar (`IntType`/`FloatType`) discards; pointer- and aggregate-returning
//! externs keep the original fail-closed backstop so a future `hew-mir`
//! regression that misroutes an owned/heap return into `dest = None` is
//! caught loudly rather than silently leaking.
//!
//! LESSONS applied: `boundary-fail-closed` (P0).

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, ExternDecl, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// Pipeline builder
// ---------------------------------------------------------------------------

/// Build a minimal `IrPipeline` that calls extern `probe_fn` (declared via
/// `extern_decl`) in statement position with `dest = None`, then returns.
/// The function itself returns `()` so the test exercises only the scalar/
/// aggregate guard inside the callee discard arm.
fn pipeline_discard_extern(
    fn_name: &str,
    extern_return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
    args: Vec<Place>,
) -> IrPipeline {
    pipeline_discard_extern_owned(fn_name, extern_return_ty, param_tys, args, false)
}

/// As [`pipeline_discard_extern`], but lets the caller pin
/// `ExternDecl::malloc_string_return`. A raw malloc-owned string return
/// discarded in statement position (`dest == None`) must STILL fail closed on
/// the pointer/aggregate backstop — the adoption edge only runs when a tracked
/// `dest` is present, so a discard can never silently leak the foreign string.
fn pipeline_discard_extern_owned(
    fn_name: &str,
    extern_return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
    args: Vec<Place>,
    malloc_string_return: bool,
) -> IrPipeline {
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: fn_name.to_string(),
                builtin: None,
                args,
                dest: None, // intentional discard — the arm under test
                next: 1,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];

    let raw = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "probe".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: param_tys.clone(),
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: blocks.clone(),
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let checked = CheckedMirFunction {
        name: "probe".to_string(),
        return_ty: ResolvedTy::Unit,
        blocks: blocks.clone(),
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
    };
    let elab = ElaboratedMirFunction {
        name: "probe".to_string(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![
            ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: Some(1),
            },
            ElabBlock {
                id: 1,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            },
        ],
        drop_plans: vec![(ExitPath::Return { block: 1 }, DropPlan::default())],
        coroutine: None,
        lambda_captures: vec![],
    };

    IrPipeline {
        thir: vec![],
        raw_mir: vec![raw],
        checked_mir: vec![checked],
        elaborated_mir: vec![elab],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: vec![],
        extern_decls: vec![ExternDecl {
            name: fn_name.to_string(),
            abi: "C".to_string(),
            param_tys,
            return_ty: extern_return_ty,
            provenance: hew_hir::ExternProvenance::Root,
            malloc_string_return,
        }],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: vec![],
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

fn emit_options(module_name: &str) -> (EmitOptions<'static>, std::path::PathBuf) {
    let tmp = std::env::temp_dir().join(format!("hew-scalar-discard-{module_name}"));
    // EmitOptions borrows module_name and out_dir; we leak the PathBuf for the
    // 'static lifetime required by EmitOptions.
    let out_dir: &'static std::path::Path = Box::leak(Box::new(tmp.clone())).as_path();
    let module_name_static: &'static str = Box::leak(module_name.to_string().into_boxed_str());
    (
        EmitOptions {
            module_name: module_name_static,
            out_dir,
            native: false,
            wasm: false,
            target_triple: None,
            debug: false,
            opt_level: hew_codegen_rs::OptLevel::O0,
            source_path: None,
        },
        tmp,
    )
}

// ---------------------------------------------------------------------------
// Happy-path: scalar (i32) discard succeeds
// ---------------------------------------------------------------------------

/// A discarded `i32`-returning extern (the `conn.close()` / `ln.close()` shape)
/// must compile successfully — the scalar discard arm permits it.
#[test]
fn discard_i32_extern_in_statement_position_succeeds() {
    let pipeline = pipeline_discard_extern(
        "probe_close",
        ResolvedTy::I32,
        vec![ResolvedTy::I64], // one handle arg
        vec![Place::Local(0)],
    );
    let (options, _tmp) = emit_options("discard_i32");
    let result = emit_module(&pipeline, &options);
    assert!(
        result.is_ok(),
        "discarding an i32-returning extern must succeed (conn.close shape); got {result:?}"
    );
}

/// A discarded `i64`-returning extern must also succeed — it is still a scalar.
#[test]
fn discard_i64_extern_in_statement_position_succeeds() {
    let pipeline = pipeline_discard_extern("probe_status64", ResolvedTy::I64, vec![], vec![]);
    let (options, _tmp) = emit_options("discard_i64");
    let result = emit_module(&pipeline, &options);
    assert!(
        result.is_ok(),
        "discarding an i64-returning extern must succeed (scalar); got {result:?}"
    );
}

// ---------------------------------------------------------------------------
// Fail-closed: pointer/aggregate discard is rejected
// ---------------------------------------------------------------------------

/// A discarded `string`-returning extern (LLVM pointer type) with `dest = None`
/// must surface `CodegenError::FailClosed`. This fires the backstop that
/// guards against a future `hew-mir` regression routing an owned/heap return
/// into `dest = None`.
#[test]
fn discard_string_extern_in_statement_position_fails_closed() {
    let pipeline = pipeline_discard_extern("probe_make_str", ResolvedTy::String, vec![], vec![]);
    let (options, _tmp) = emit_options("discard_string");
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("probe_make_str"),
                "FailClosed message must name the callee; got: {msg}"
            );
            assert!(
                msg.contains("pointer or aggregate"),
                "FailClosed message must explain the pointer/aggregate rejection; got: {msg}"
            );
        }
        Err(other) => {
            panic!("expected CodegenError::FailClosed for pointer-return discard, got {other:?}")
        }
        Ok(_) => {
            panic!("string-returning extern discarded with dest=None must fail closed; got Ok(_)")
        }
    }
}

/// A discarded `string`-returning extern flagged `malloc_string_return = true`
/// (a raw foreign C string) with `dest == None` must STILL fail closed on the
/// pointer/aggregate backstop. The adoption edge only runs for a tracked
/// `dest`, so a discard can never adopt-and-free — an un-adopted foreign
/// pointer dropped on the floor is exactly the owned/heap return the backstop
/// rejects. This pins that `malloc_string_return` never widens the scalar
/// discard allowance.
///
/// LESSONS applied: `boundary-fail-closed` (P0).
#[test]
fn discard_malloc_string_extern_in_statement_position_fails_closed() {
    let pipeline = pipeline_discard_extern_owned(
        "probe_make_owned_str",
        ResolvedTy::String,
        vec![],
        vec![],
        true, // malloc_string_return — raw foreign C string
    );
    let (options, _tmp) = emit_options("discard_malloc_string");
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("probe_make_owned_str"),
                "FailClosed message must name the callee; got: {msg}"
            );
            assert!(
                msg.contains("pointer or aggregate"),
                "FailClosed message must explain the pointer/aggregate rejection; got: {msg}"
            );
        }
        Err(other) => panic!(
            "expected CodegenError::FailClosed for discarded malloc-string extern, got {other:?}"
        ),
        Ok(_) => panic!(
            "malloc-owned string extern discarded with dest=None must fail closed; got Ok(_)"
        ),
    }
}

/// A `bytes`-returning extern discarded in statement position (dest = None)
/// must surface `CodegenError::FailClosed` naming the symbol. The return is now
/// a classified aggregate (BytesTriple), so the guard fires at the classified
/// call edge: an aggregate return has no place to land without a destination
/// slot, so the discard is rejected loud rather than dropping the owned value.
/// The invariant is unchanged: discarding a bytes-return extern fails closed.
///
/// LESSONS applied: `boundary-fail-closed` (P0).
#[test]
fn discard_bytes_extern_in_statement_position_fails_closed() {
    let pipeline = pipeline_discard_extern("probe_make_bytes", ResolvedTy::Bytes, vec![], vec![]);
    let (options, _tmp) = emit_options("discard_bytes");
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("probe_make_bytes"),
                "FailClosed message must name the callee; got: {msg}"
            );
            assert!(
                msg.contains("must carry a Terminator::Call dest")
                    || msg.contains("destination slot"),
                "FailClosed message must explain the classified-return-needs-dest \
                 guard; got: {msg}"
            );
        }
        Err(other) => {
            panic!(
                "expected CodegenError::FailClosed for discarded bytes-return extern, got {other:?}"
            )
        }
        Ok(_) => {
            panic!("bytes-returning extern discarded with dest=None must fail closed; got Ok(_)")
        }
    }
}
