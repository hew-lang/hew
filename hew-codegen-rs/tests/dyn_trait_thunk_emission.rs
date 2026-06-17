//! LLVM emission for erased `dyn Trait` method thunks.
//!
//! Each entry in `IrPipeline::dyn_vtable_registry` causes codegen to
//! synthesise one LLVM function per `(vtable_id, method_index)` pair.
//! The thunk takes the erased fat-pointer data word (a `ptr` under
//! LLVM's opaque-pointer model) as its first argument, forwards every
//! argument verbatim to the concrete impl method named by
//! `method_table[method_index].1`, and returns the impl's return
//! value verbatim. The symbol name comes from
//! `hew_mir::mangle_dyn_thunk_symbol(vtable_id, method_index)`.
//!
//! The vtable static initialiser and the indirect-dispatch arm at
//! `Instr::CallTraitMethod` land in subsequent stages; this test file
//! pins the thunk substrate alone — the symbol names, the receiver-
//! forwarding shape, and the fail-closed paths.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    mangle_dyn_thunk_symbol, mangle_dyn_vtable_symbol, BasicBlock, DynVtableInstance,
    FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::{DynVtableEntry, FnSig, ResolvedTy, Ty};

/// Build a minimal `RawMirFunction` whose LLVM signature is
/// `fn(ptr, <extra_param_tys>...) -> ret`. The body returns a
/// constant `7` (for `i64` returns) or the unit canonical zero (for
/// `Unit` returns). Param 0 is always the receiver-shaped pointer
/// argument; subsequent params let the test exercise multi-argument
/// thunks.
fn impl_method_stub(
    name: &str,
    ret: ResolvedTy,
    extra_param_tys: Vec<ResolvedTy>,
) -> RawMirFunction {
    // Slot 0 = receiver-shaped pointer. `ResolvedTy::String` lowers to
    // `ptr` (see `primitive_to_llvm`), which is what the thunk will
    // forward. Subsequent params come from the test caller.
    let mut params = vec![ResolvedTy::String];
    params.extend(extra_param_tys);
    let local_idx = u32::try_from(params.len()).expect("test scaffold param count fits u32");
    let instructions = match &ret {
        ResolvedTy::I64 => vec![
            Instr::ConstI64 {
                dest: Place::Local(local_idx),
                value: 7,
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(local_idx),
            },
        ],
        ResolvedTy::Unit => vec![],
        other => panic!("impl_method_stub: unsupported return ty {other:?} for test scaffold"),
    };
    let mut locals: Vec<ResolvedTy> = params.clone();
    if matches!(ret, ResolvedTy::I64) {
        locals.push(ret.clone());
    }
    RawMirFunction {
        name: name.to_string(),
        return_ty: ret,
        call_conv: FunctionCallConv::Default,
        params,
        locals,
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
    }
}

fn vtable_entry(
    trait_name: &str,
    method: &str,
    impl_key: &str,
    ret: Ty,
    params: Vec<Ty>,
) -> DynVtableEntry {
    DynVtableEntry {
        trait_name: trait_name.to_string(),
        method_name: method.to_string(),
        impl_fn_key: impl_key.to_string(),
        signature: FnSig {
            params,
            return_type: ret,
            ..FnSig::default()
        },
    }
}

fn vtable_instance(
    vtable_id: u32,
    trait_name: &str,
    concrete: ResolvedTy,
    methods: Vec<(String, String, DynVtableEntry)>,
) -> DynVtableInstance {
    let method_table: Vec<(String, String)> = methods
        .iter()
        .map(|(name, key, _)| (name.clone(), key.clone()))
        .collect();
    let vtable_entries: Vec<DynVtableEntry> = methods.into_iter().map(|(_, _, e)| e).collect();
    DynVtableInstance {
        vtable_id,
        symbol: mangle_dyn_vtable_symbol(vtable_id, trait_name, &concrete),
        trait_name: trait_name.to_string(),
        concrete_type: concrete,
        method_table,
        vtable_entries,
    }
}

fn pipeline_with(raw_mir: Vec<RawMirFunction>, registry: Vec<DynVtableInstance>) -> IrPipeline {
    IrPipeline {
        thir: vec![],
        raw_mir,
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: registry,
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let nonce = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let tmp = std::env::temp_dir().join(format!("hew-dyn-thunk-{module_name}-{nonce}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let artefacts = emit_module(pipeline, &options).expect("emit_module must succeed");
    let ll_path: &Path = artefacts.ll_path.as_deref().expect("emit_module ll_path");
    std::fs::read_to_string(ll_path).expect("read ll")
}

fn try_emit(pipeline: &IrPipeline, _module_name: &str) -> Result<(), hew_codegen_rs::CodegenError> {
    hew_codegen_rs::validate_codegen_front(pipeline)
}

// -- Tests ------------------------------------------------------------

/// Empty `dyn_vtable_registry` → no thunk symbols emitted. The
/// thunk-emission pass must be a no-op when no `Instr::CoerceToDynTrait`
/// site exists anywhere in the module — otherwise every regression test
/// for an unrelated subsystem would pay a fixed cost.
#[test]
fn empty_registry_emits_no_thunks() {
    let p = pipeline_with(vec![], vec![]);
    let ll = emit_ll(&p, "empty_registry");
    assert!(
        !ll.contains("__hew_dyn_thunk_"),
        "no thunk symbols must appear when the registry is empty; got:\n{ll}"
    );
}

/// One vtable entry with one method → exactly one thunk
/// `__hew_dyn_thunk__{trait}__{concrete}__{id}_{method}` is defined,
/// with a single `ptr` parameter, and its body calls the concrete
/// impl. The thunk symbol must be observable in textual IR
/// (`Linkage::Private`) so dispatch-arm wiring is verifiable without
/// crossing the indirect-dispatch fail-closed gate.
#[test]
fn single_entry_single_method_emits_one_forwarding_thunk() {
    let impl_fn = impl_method_stub("i64::fmt", ResolvedTy::I64, vec![]);
    let entry = vtable_entry("Display", "fmt", "i64::fmt", Ty::I64, vec![]);
    let vtable = vtable_instance(
        0,
        "Display",
        ResolvedTy::I64,
        vec![("fmt".to_string(), "i64::fmt".to_string(), entry)],
    );
    let p = pipeline_with(vec![impl_fn], vec![vtable]);
    let ll = emit_ll(&p, "single_thunk");
    let thunk = mangle_dyn_thunk_symbol(0, 0, "Display", &ResolvedTy::I64);
    assert!(
        ll.contains(&format!("define private i64 @{thunk}(ptr")),
        "thunk must be defined as `define private i64 @{thunk}(ptr ...)`; got:\n{ll}"
    );
    assert!(
        ll.contains(&format!("@{thunk}")),
        "thunk symbol must appear in textual IR"
    );
    // The thunk body must `call @i64::fmt` and `ret` the result.
    // Use a substring match scoped to the thunk's body region.
    let body_start = ll
        .find(&format!("@{thunk}"))
        .expect("thunk symbol must be present");
    let body = &ll[body_start..];
    assert!(
        body.contains("call i64 @\"i64::fmt\"") || body.contains("call i64 @i64::fmt"),
        "thunk body must forward to `@i64::fmt`; got:\n{body}"
    );
    assert!(
        body.contains("ret i64"),
        "thunk body must propagate the impl return; got:\n{body}"
    );
}

/// A vtable carrying two methods produces two distinct thunk
/// symbols at the same `vtable_id` differing only in the method index.
#[test]
fn two_methods_in_one_vtable_emit_distinct_thunks() {
    let next = impl_method_stub("Counter::next", ResolvedTy::I64, vec![]);
    let reset = impl_method_stub("Counter::reset", ResolvedTy::Unit, vec![]);
    let entry_next = vtable_entry("Counter", "next", "Counter::next", Ty::I64, vec![]);
    let entry_reset = vtable_entry("Counter", "reset", "Counter::reset", Ty::Unit, vec![]);
    let vtable = vtable_instance(
        0,
        "Counter",
        ResolvedTy::I64,
        vec![
            ("next".to_string(), "Counter::next".to_string(), entry_next),
            (
                "reset".to_string(),
                "Counter::reset".to_string(),
                entry_reset,
            ),
        ],
    );
    let p = pipeline_with(vec![next, reset], vec![vtable]);
    let ll = emit_ll(&p, "two_methods");
    let t0 = mangle_dyn_thunk_symbol(0, 0, "Counter", &ResolvedTy::I64);
    let t1 = mangle_dyn_thunk_symbol(0, 1, "Counter", &ResolvedTy::I64);
    assert_ne!(t0, t1, "method-index distinct → symbol distinct");
    assert!(
        ll.contains(&format!("@{t0}")),
        "thunk 0 must be emitted: {t0} not in IR:\n{ll}"
    );
    assert!(
        ll.contains(&format!("@{t1}")),
        "thunk 1 must be emitted: {t1} not in IR:\n{ll}"
    );
}

/// Two registry entries → two thunks at distinct `vtable_id`s. The
/// `(vtable_id, method_index)` plane fully discriminates thunk names.
#[test]
fn two_registry_entries_emit_thunks_at_distinct_vtable_ids() {
    let i64_fmt = impl_method_stub("i64::fmt", ResolvedTy::I64, vec![]);
    let bool_fmt = impl_method_stub("bool::fmt", ResolvedTy::I64, vec![]);
    let e_i64 = vtable_entry("Display", "fmt", "i64::fmt", Ty::I64, vec![]);
    let e_bool = vtable_entry("Display", "fmt", "bool::fmt", Ty::I64, vec![]);
    let vt0 = vtable_instance(
        0,
        "Display",
        ResolvedTy::I64,
        vec![("fmt".to_string(), "i64::fmt".to_string(), e_i64)],
    );
    let vt1 = vtable_instance(
        1,
        "Display",
        ResolvedTy::Bool,
        vec![("fmt".to_string(), "bool::fmt".to_string(), e_bool)],
    );
    let p = pipeline_with(vec![i64_fmt, bool_fmt], vec![vt0, vt1]);
    let ll = emit_ll(&p, "two_vtables");
    assert!(ll.contains(&format!(
        "@{}",
        mangle_dyn_thunk_symbol(0, 0, "Display", &ResolvedTy::I64)
    )));
    assert!(ll.contains(&format!(
        "@{}",
        mangle_dyn_thunk_symbol(1, 0, "Display", &ResolvedTy::Bool)
    )));
}

/// A `method_table` entry whose `impl_fn_key` does not appear in
/// `fn_symbols` MUST surface as a fail-closed `CodegenError` rather
/// than a dangling-reference at link time. The checker's authority
/// over vtable construction guarantees every entry's `impl_fn_key`
/// is declared upstream; this test pins the codegen-side trip-wire.
#[test]
fn missing_impl_fn_fails_closed() {
    let entry = vtable_entry("Display", "fmt", "Nonexistent::fmt", Ty::I64, vec![]);
    let vtable = vtable_instance(
        0,
        "Display",
        ResolvedTy::I64,
        vec![("fmt".to_string(), "Nonexistent::fmt".to_string(), entry)],
    );
    let p = pipeline_with(vec![], vec![vtable]);
    let err = try_emit(&p, "missing_impl").expect_err("missing impl fn must fail closed");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("Nonexistent::fmt"),
        "fail-closed message must name the missing impl fn; got: {msg}"
    );
    assert!(
        msg.contains("not declared") || msg.contains("undeclared"),
        "fail-closed message must explain `not declared in the module`; got: {msg}"
    );
}

/// An impl whose LLVM parameter count does NOT match `1 +
/// signature.params.len()` MUST fail closed. This guards against a
/// call-conv shift (e.g. an impl gaining a leading execution-context
/// pointer) that the single-receiver thunk cannot transparently
/// forward.
#[test]
fn impl_param_count_mismatch_fails_closed() {
    // Impl fn declared with 2 LLVM params (receiver + one extra),
    // but the vtable entry's signature.params is empty so the thunk
    // expects only 1 (the erased self). Mismatch must trip the
    // fail-closed guard.
    let impl_fn = impl_method_stub("Counter::tick", ResolvedTy::I64, vec![ResolvedTy::I64]);
    let entry = vtable_entry("Counter", "tick", "Counter::tick", Ty::I64, vec![]);
    let vtable = vtable_instance(
        0,
        "Counter",
        ResolvedTy::I64,
        vec![("tick".to_string(), "Counter::tick".to_string(), entry)],
    );
    let p = pipeline_with(vec![impl_fn], vec![vtable]);
    let err = try_emit(&p, "param_mismatch").expect_err("param count mismatch must fail closed");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("Counter::tick"),
        "fail-closed message must name the impl fn; got: {msg}"
    );
    assert!(
        msg.contains("LLVM parameter") || msg.contains("parameters"),
        "fail-closed message must reference parameter-count mismatch; got: {msg}"
    );
}

/// Impl function with a concrete value-type (non-pointer) receiver.
/// Used by `thunk_loads_value_type_receiver_through_data_ptr` to build
/// a scaffold that takes `i64` directly (not `ptr`) as param 0.
fn impl_method_stub_value_recv(name: &str, ret: ResolvedTy) -> RawMirFunction {
    // Receiver is `i64` (value-type), not `ptr`.
    let params = vec![ResolvedTy::I64];
    let local_idx = u32::try_from(params.len()).expect("param count fits u32");
    let instructions = match &ret {
        ResolvedTy::I64 => vec![
            Instr::ConstI64 {
                dest: Place::Local(local_idx),
                value: 77,
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(local_idx),
            },
        ],
        ResolvedTy::Unit => vec![],
        other => panic!("impl_method_stub_value_recv: unsupported ret {other:?}"),
    };
    let mut locals: Vec<ResolvedTy> = params.clone();
    if matches!(ret, ResolvedTy::I64) {
        locals.push(ret.clone());
    }
    RawMirFunction {
        name: name.to_string(),
        return_ty: ret,
        call_conv: FunctionCallConv::Default,
        params,
        locals,
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
    }
}

/// When the impl function's first parameter is a non-pointer value type
/// (e.g. `i64`) the thunk must emit a `load i64, ptr %0` before the
/// forward call, not pass the data pointer directly.
///
/// This pins the `impl_first_param_is_ptr == false` branch in
/// `emit_dyn_trait_thunks`: the CoerceToDynTrait site places the
/// concrete value's alloca address in the fat-pointer data word, so
/// `ptr %0` always dereferences to the correct value.
#[test]
fn thunk_loads_value_type_receiver_through_data_ptr() {
    // Build an impl stub whose first parameter is `i64` (not `ptr`).
    let impl_fn = impl_method_stub_value_recv("i64_area_impl", ResolvedTy::I64);
    let entry = vtable_entry("Shape", "area", "i64_area_impl", Ty::I64, vec![]);
    let vtable = vtable_instance(
        0,
        "Shape",
        ResolvedTy::I64,
        vec![("area".to_string(), "i64_area_impl".to_string(), entry)],
    );
    let p = pipeline_with(vec![impl_fn], vec![vtable]);
    let ll = emit_ll(&p, "value_type_recv");
    let thunk = mangle_dyn_thunk_symbol(0, 0, "Shape", &ResolvedTy::I64);

    // The thunk's own signature: `(ptr) -> i64` — erased receiver.
    assert!(
        ll.contains(&format!("define private i64 @{thunk}(ptr")),
        "thunk must have `ptr` parameter (erased self); got:\n{ll}"
    );

    // The thunk body must contain a `load i64, ptr` instruction to
    // dereference the data pointer before calling the impl.
    let body_start = ll
        .find(&format!("@{thunk}"))
        .expect("thunk symbol must appear in IR");
    let body = &ll[body_start..];
    assert!(
        body.contains("load i64, ptr"),
        "thunk body must emit `load i64, ptr` for a value-type receiver; got:\n{body}"
    );
    assert!(
        body.contains("call i64 @i64_area_impl"),
        "thunk body must forward to `@i64_area_impl`; got:\n{body}"
    );
}

/// A method whose vtable-entry signature carries a non-trivial
/// `params` list produces a thunk whose LLVM signature mirrors it
/// after the leading erased `ptr`. This pins the contract that
/// `vtable_entries[i].signature` is receiver-skipped and that codegen
/// consumes it verbatim — `params[0]` is the first real argument,
/// not a Self.
#[test]
fn thunk_signature_mirrors_receiver_skipped_params() {
    // `fn at(&self, index: i64) -> i64` → impl LLVM sig `(ptr, i64) -> i64`,
    // signature.params = [Ty::I64] (receiver-skipped),
    // thunk LLVM sig `(ptr, i64) -> i64`.
    let impl_fn = impl_method_stub("Vec::at", ResolvedTy::I64, vec![ResolvedTy::I64]);
    let entry = vtable_entry("Index", "at", "Vec::at", Ty::I64, vec![Ty::I64]);
    let vtable = vtable_instance(
        0,
        "Index",
        ResolvedTy::I64,
        vec![("at".to_string(), "Vec::at".to_string(), entry)],
    );
    let p = pipeline_with(vec![impl_fn], vec![vtable]);
    let ll = emit_ll(&p, "receiver_skipped_sig");
    let thunk = mangle_dyn_thunk_symbol(0, 0, "Index", &ResolvedTy::I64);
    assert!(
        ll.contains(&format!("define private i64 @{thunk}(ptr")),
        "thunk must declare `define private i64 @{thunk}(ptr, i64)`; got:\n{ll}"
    );
    // Find the line containing the define and verify it carries i64.
    let define_line = ll
        .lines()
        .find(|l| l.contains(&format!("@{thunk}(")))
        .expect("thunk define line");
    assert!(
        define_line.contains("ptr") && define_line.contains("i64"),
        "thunk define line must take `(ptr, i64)`; got: {define_line}"
    );
}
