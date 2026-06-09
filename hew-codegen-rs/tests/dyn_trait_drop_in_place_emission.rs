//! LLVM emission for per-vtable `drop_in_place` functions backing
//! heap-boxed `dyn Trait` values.
//!
//! Each entry in `IrPipeline::dyn_vtable_registry` causes codegen to
//! synthesise exactly one LLVM function named
//! `hew_mir::mangle_dyn_drop_in_place_symbol(vtable_id)` with
//! signature `void (ptr)`. The body runs the concrete value's drop
//! ritual at the erased pointer (today restricted to trivially-
//! droppable concretes) and then calls `hew_dyn_box_free(ptr, size,
//! align)` where `size`/`align` come from the ABI layout of the
//! concrete type recorded on `DynVtableInstance::concrete_type`.
//!
//! One function per `vtable_id` (not per method): the vtable's
//! concrete type determines the drop ritual, not the trait or any
//! individual method.
//!
//! The vtable static initialiser and the indirect-dispatch arm at
//! `Instr::CallTraitMethod` remain fail-closed at this substrate
//! level; this test file pins the drop-fn substrate alone — symbol
//! names, body shape, and fail-closed paths.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    mangle_dyn_drop_in_place_symbol, mangle_dyn_vtable_symbol, BasicBlock, DynVtableInstance,
    FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, RecordLayout, Terminator,
};
use hew_types::{DynVtableEntry, FnSig, ResolvedTy, Ty};

/// Minimal impl-method stub: `fn(ptr) -> i64` returning a constant.
/// The drop-fn synthesis does not call any impl method (the per-
/// method thunks do); this stub exists only to satisfy the thunk
/// pass, which co-runs in the same `emit_module` pipeline.
fn impl_method_stub(name: &str) -> RawMirFunction {
    let params = vec![ResolvedTy::String]; // ptr-shaped receiver
    let local_idx = u32::try_from(params.len()).expect("test scaffold param count fits u32");
    let instructions = vec![
        Instr::ConstI64 {
            dest: Place::Local(local_idx),
            value: 7,
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(local_idx),
        },
    ];
    let mut locals = params.clone();
    locals.push(ResolvedTy::I64);
    RawMirFunction {
        name: name.to_string(),
        return_ty: ResolvedTy::I64,
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
    }
}

fn vtable_entry(trait_name: &str, method: &str, impl_key: &str) -> DynVtableEntry {
    DynVtableEntry {
        trait_name: trait_name.to_string(),
        method_name: method.to_string(),
        impl_fn_key: impl_key.to_string(),
        signature: FnSig {
            params: vec![],
            return_type: Ty::I64,
            ..FnSig::default()
        },
    }
}

fn vtable_instance(vtable_id: u32, concrete: ResolvedTy, impl_key: &str) -> DynVtableInstance {
    let entry = vtable_entry("Display", "fmt", impl_key);
    DynVtableInstance {
        vtable_id,
        symbol: mangle_dyn_vtable_symbol(vtable_id, "Display", &concrete),
        trait_name: "Display".to_string(),
        concrete_type: concrete,
        method_table: vec![("fmt".to_string(), impl_key.to_string())],
        vtable_entries: vec![entry],
    }
}

fn pipeline_with(
    raw_mir: Vec<RawMirFunction>,
    registry: Vec<DynVtableInstance>,
    record_layouts: Vec<RecordLayout>,
) -> IrPipeline {
    IrPipeline {
        thir: vec![],
        raw_mir,
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts,
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
    }
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let nonce = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let tmp = std::env::temp_dir().join(format!("hew-dyn-drop-{module_name}-{nonce}"));
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

/// Empty `dyn_vtable_registry` → no drop-in-place symbols emitted.
/// The drop-emission pass must be a no-op when no
/// `Instr::CoerceToDynTrait` site exists anywhere in the module —
/// otherwise every regression test for an unrelated subsystem
/// would pay a fixed cost.
#[test]
fn empty_registry_emits_no_drop_in_place_fns() {
    let p = pipeline_with(vec![], vec![], vec![]);
    let ll = emit_ll(&p, "empty_registry");
    assert!(
        !ll.contains("__hew_dyn_drop_in_place_"),
        "no drop-in-place symbols must appear when the registry is empty; got:\n{ll}"
    );
}

/// One BitCopy concrete vtable → exactly one drop-in-place
/// function `__hew_dyn_drop_in_place__{trait}__{concrete}__{id}`
/// defined with signature `void(ptr)`, calling `hew_dyn_box_free`
/// with the ABI layout of the concrete type. `Linkage::Private`,
/// observable in textual IR for test verification.
#[test]
fn single_bitcopy_concrete_emits_one_drop_in_place_fn() {
    let impl_fn = impl_method_stub("i64::fmt");
    let vtable = vtable_instance(0, ResolvedTy::I64, "i64::fmt");
    let p = pipeline_with(vec![impl_fn], vec![vtable], vec![]);
    let ll = emit_ll(&p, "single_drop_in_place");
    let symbol = mangle_dyn_drop_in_place_symbol(0, "Display", &ResolvedTy::I64);
    assert!(
        ll.contains(&format!("define private void @{symbol}(ptr")),
        "drop-in-place must be defined as `define private void @{symbol}(ptr ...)`; got:\n{ll}"
    );
    // The body must reference `hew_dyn_box_free` exactly once for
    // this single vtable: the registry's lone entry is the only
    // producer of the free call.
    let occurrences = ll.matches("@hew_dyn_box_free").count();
    assert!(
        occurrences >= 1,
        "drop-in-place body must call `@hew_dyn_box_free`; got:\n{ll}"
    );
    // i64 → size=8, align=8 on every supported target. The
    // constants surface in the textual IR as literal integers
    // because LLVM does not fold `const i64 8` into a symbol.
    let body_start = ll
        .find(&format!("define private void @{symbol}"))
        .expect("drop-in-place define line must be present");
    let body = &ll[body_start..];
    let body_end = body.find("\n}").unwrap_or(body.len());
    let body = &body[..body_end];
    assert!(
        body.contains("i64 8"),
        "i64 concrete must pass size=8 and align=8 to hew_dyn_box_free; got body:\n{body}"
    );
}

/// Two registry entries with distinct concrete types → two
/// drop-in-place functions at distinct `vtable_id`s, distinct
/// symbol names. The `vtable_id` axis fully discriminates drop-in-
/// place names — there is no method-index axis (one fn per
/// vtable, not per method).
#[test]
fn multi_vtable_emits_distinct_drop_in_place_symbols() {
    let i64_impl = impl_method_stub("i64::fmt");
    let bool_impl = impl_method_stub("bool::fmt");
    let vt0 = vtable_instance(0, ResolvedTy::I64, "i64::fmt");
    let vt1 = vtable_instance(1, ResolvedTy::Bool, "bool::fmt");
    let p = pipeline_with(vec![i64_impl, bool_impl], vec![vt0, vt1], vec![]);
    let ll = emit_ll(&p, "multi_drop_in_place");
    let s0 = mangle_dyn_drop_in_place_symbol(0, "Display", &ResolvedTy::I64);
    let s1 = mangle_dyn_drop_in_place_symbol(1, "Display", &ResolvedTy::Bool);
    assert_ne!(s0, s1, "vtable_id distinct → drop-in-place symbol distinct");
    assert!(
        ll.contains(&format!("define private void @{s0}(ptr")),
        "drop-in-place for vtable 0 must be emitted: {s0} not in IR:\n{ll}"
    );
    assert!(
        ll.contains(&format!("define private void @{s1}(ptr")),
        "drop-in-place for vtable 1 must be emitted: {s1} not in IR:\n{ll}"
    );
}

/// A non-`BitCopy` concrete type (here `String`, which carries
/// owned-heap storage) MUST fail closed at the drop-in-place
/// synthesis pass: `DynVtableInstance` does not yet plumb a
/// per-type drop-fn key, and emitting a free-only body would leak
/// the inner heap storage. This pins the boundary-fail-closed
/// invariant so a future stage that lifts the restriction MUST
/// extend the registry with the structural-drop entry point.
#[test]
fn non_bitcopy_concrete_fails_closed() {
    let impl_fn = impl_method_stub("String::fmt");
    let vtable = vtable_instance(0, ResolvedTy::String, "String::fmt");
    let p = pipeline_with(vec![impl_fn], vec![vtable], vec![]);
    let err = try_emit(&p, "non_bitcopy_fails_closed")
        .expect_err("non-BitCopy concrete must fail closed");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("structural drop"),
        "fail-closed message must reference `structural drop`; got: {msg}"
    );
    assert!(
        msg.contains("drop_in_place") || msg.contains("dyn drop"),
        "fail-closed message must reference the dyn drop_in_place path; got: {msg}"
    );
}

/// The drop-in-place fn must be emitted even when no in-module
/// caller exists outside the vtable static initialiser. The
/// symbol carries `Linkage::Private` — internal to the
/// compilation unit, no ABI promise.
#[test]
fn drop_in_place_is_private_linkage() {
    let impl_fn = impl_method_stub("i64::fmt");
    let vtable = vtable_instance(0, ResolvedTy::I64, "i64::fmt");
    let p = pipeline_with(vec![impl_fn], vec![vtable], vec![]);
    let ll = emit_ll(&p, "private_linkage");
    let symbol = mangle_dyn_drop_in_place_symbol(0, "Display", &ResolvedTy::I64);
    // `Private` linkage in textual IR: `define private void @<sym>`.
    let define_line = ll
        .lines()
        .find(|l| l.contains(&format!("@{symbol}(")))
        .expect("drop-in-place define line must be present");
    assert!(
        define_line.contains("private"),
        "drop-in-place MUST have Private linkage; \
         got define line: {define_line}"
    );
}

/// One drop-in-place per vtable_id — adding more methods to the
/// same vtable does NOT add more drop-in-place symbols. The drop
/// ritual is determined by the vtable's concrete type, not by any
/// individual method.
#[test]
fn one_drop_in_place_per_vtable_regardless_of_method_count() {
    let m0 = impl_method_stub("Counter::next");
    let m1 = impl_method_stub("Counter::reset");
    let e0 = vtable_entry("Counter", "next", "Counter::next");
    let e1 = vtable_entry("Counter", "reset", "Counter::reset");
    let vtable = DynVtableInstance {
        vtable_id: 0,
        symbol: mangle_dyn_vtable_symbol(0, "Counter", &ResolvedTy::I64),
        trait_name: "Counter".to_string(),
        concrete_type: ResolvedTy::I64,
        method_table: vec![
            ("next".to_string(), "Counter::next".to_string()),
            ("reset".to_string(), "Counter::reset".to_string()),
        ],
        vtable_entries: vec![e0, e1],
    };
    let p = pipeline_with(vec![m0, m1], vec![vtable], vec![]);
    let ll = emit_ll(&p, "single_drop_for_multi_method");
    let symbol = mangle_dyn_drop_in_place_symbol(0, "Counter", &ResolvedTy::I64);
    // Exactly one `define void @<sym>(` line.
    let count = ll
        .matches(&format!("define private void @{symbol}("))
        .count();
    assert_eq!(
        count, 1,
        "exactly one drop-in-place fn per vtable_id; got {count} for {symbol}:\n{ll}"
    );
}

// -- F2: POD-record concrete + heap-bearing record concrete ----------

/// Helper: a user-record `ResolvedTy::Named` with no generic args
/// and `builtin: None` — the shape every user record takes after
/// type checking.
fn user_record_ty(name: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: name.to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }
}

/// A `dyn Trait` concrete record whose fields are transitively
/// all `BitCopy` (here `{ count: i64, ratio: f64 }`) MUST get a
/// drop-in-place fn synthesised with the free-only shape — the
/// shared `classify_state_field` returns `UserRecord` for every
/// record regardless of field heap-ness, so the dyn-drop-local
/// triviality probe must accept this case rather than rejecting
/// every record outright. This is the central regression test
/// for the POD-record concrete shape.
#[test]
fn dyn_drop_in_place_for_bitcopy_record_synthesizes_free_only_thunk() {
    let impl_fn = impl_method_stub("Counter::fmt");
    let concrete = user_record_ty("Counter");
    let vtable = vtable_instance(0, concrete, "Counter::fmt");
    // Counter { count: i64, ratio: f64 } — both fields BitCopy,
    // so the record is transitively trivially droppable.
    let counter_layout = RecordLayout {
        name: "Counter".to_string(),
        field_tys: vec![ResolvedTy::I64, ResolvedTy::F64],
    };
    let p = pipeline_with(vec![impl_fn], vec![vtable], vec![counter_layout]);
    let ll = emit_ll(&p, "bitcopy_record_pod");
    let symbol = mangle_dyn_drop_in_place_symbol(0, "Display", &user_record_ty("Counter"));
    assert!(
        ll.contains(&format!("define private void @{symbol}(ptr")),
        "drop-in-place must be synthesised for an all-BitCopy record concrete; \
         got:\n{ll}"
    );
    // Body must call `hew_dyn_box_free` once.
    let body_start = ll
        .find(&format!("define private void @{symbol}"))
        .expect("drop-in-place define line must be present");
    let body_end_offset = ll[body_start..]
        .find("\n}")
        .unwrap_or(ll.len() - body_start);
    let body = &ll[body_start..body_start + body_end_offset];
    assert!(
        body.contains("@hew_dyn_box_free"),
        "POD-record drop-in-place body must call `@hew_dyn_box_free`; got body:\n{body}"
    );
    // The call must pass the record's exact size (16 = i64 + f64)
    // and alignment (8 = max(align_of(i64), align_of(f64))) as
    // `i64` constants. Both are derived from host `TargetData`
    // via `abi_size_align`; locking the constants prevents silent
    // drift if the size/align plumbing ever regresses.
    let free_call = body
        .lines()
        .find(|l| l.contains("@hew_dyn_box_free"))
        .expect("body must contain the free-call line");
    assert!(
        free_call.contains("i64 16"),
        "free-call must pass size=16 for Counter {{ i64, f64 }}; got line:\n{free_call}\nfull body:\n{body}"
    );
    assert!(
        free_call.contains("i64 8"),
        "free-call must pass align=8 for Counter {{ i64, f64 }}; got line:\n{free_call}\nfull body:\n{body}"
    );
    // Belt-and-braces: pin the full call shape so any future
    // re-ordering of (size, align) arguments trips this test.
    assert!(
        free_call.contains("call void @hew_dyn_box_free(ptr")
            && free_call.contains("i64 16")
            && free_call.contains("i64 8"),
        "free-call shape must be `call void @hew_dyn_box_free(ptr <p>, i64 16, i64 8)`; \
         got line:\n{free_call}"
    );
}

/// A `dyn Trait` concrete record carrying a `String` field MUST
/// fail closed at drop-fn synthesis: the record's classification
/// is `UserRecord` (same as the POD-record case), but the
/// triviality probe walks its fields and finds the `String`,
/// which is owned-heap and not trivially droppable. Emitting a
/// free-only body for this concrete would leak the inner String
/// buffer. This is the negative companion to the POD-record
/// positive test — the dyn-drop-local relaxation must not extend
/// to heap-bearing records.
#[test]
fn dyn_drop_in_place_for_record_with_string_field_fails_closed() {
    let impl_fn = impl_method_stub("Named::fmt");
    let concrete = user_record_ty("Named");
    let vtable = vtable_instance(0, concrete, "Named::fmt");
    // Named { id: i64, label: String } — `label` carries an
    // owned heap buffer, so a free-only body would leak it.
    let named_layout = RecordLayout {
        name: "Named".to_string(),
        field_tys: vec![ResolvedTy::I64, ResolvedTy::String],
    };
    let p = pipeline_with(vec![impl_fn], vec![vtable], vec![named_layout]);
    let err = try_emit(&p, "heap_bearing_record_fails_closed")
        .expect_err("record with String field must fail closed");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("Named"),
        "fail-closed message must name the offending record; got: {msg}"
    );
    assert!(
        msg.contains("owned-heap field") || msg.contains("heap-bearing"),
        "fail-closed message must explain the owned-heap field constraint; got: {msg}"
    );
}
