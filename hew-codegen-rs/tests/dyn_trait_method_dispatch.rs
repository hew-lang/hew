//! `Instr::CallTraitMethod` — dyn-trait method dispatch.
//!
//! Each `Instr::CallTraitMethod` lowers to a fixed sequence:
//!
//! 1. Load the `%hew.dyn.fat_ptr` aggregate from the receiver's slot
//!    and `extractvalue` the data word (index 0) and the vtable
//!    pointer (index 1).
//! 2. GEP into the vtable through a synthetic prefix-view struct
//!    `{ ptr, <usize>, <usize>, ptr × (slot - 2) }` at field index
//!    `slot`. The view's prefix layout mirrors the vtable's prefix
//!    bit-for-bit (`ptr_sized_int_type` from `host_target_data()` —
//!    the same authority `emit_dyn_trait_vtable_definitions` uses),
//!    so GEP at field `slot` lands on the same byte as the
//!    corresponding method-thunk pointer slot in the real vtable.
//!    The runtime vtable_id at the call site is irrelevant: the
//!    prefix is identical across every instance, and method slots
//!    are pointer-aligned, pointer-sized.
//! 3. Load `ptr` from that slot — the erased method-thunk pointer.
//! 4. Indirect-call with `[data_ptr, args...]`. The erased
//!    indirect-call signature is derived verbatim from the
//!    `signature` on `Instr::CallTraitMethod` (the checker-substituted
//!    receiver-skipped FnSig — see
//!    `hew_mir::model::Instr::CallTraitMethod::signature`); codegen
//!    prepends a single `ptr` for the erased self word and lowers
//!    `params` / `return_type` normally. Symmetric with the thunk
//!    signature shape emitted by `emit_dyn_trait_thunks`.
//!
//! The dispatch arm does NOT consult `pipeline.dyn_vtable_registry`
//! at the call site — the call-site signature is self-contained on
//! the Instr, and re-walking the registry would skip the checker's
//! associated-type projections (LESSONS: `checker-authority` P0).
//! The registry is the producer authority for vtable *definitions*;
//! at the dispatch site the runtime vtable pointer is read from the
//! receiver, not looked up.
//!
//! Slot convention: `slot = 3 + method_decl_order` per
//! `hew_types::DynMethodCall::slot`. The 3-slot prefix is
//! `drop_in_place / size_of / align_of`. `slot < 3` is an upstream
//! invariant violation and fails closed.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{BasicBlock, FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, Terminator};
use hew_types::{FnSig, ResolvedTraitBound, ResolvedTy, Ty};

// -- Scaffolding ------------------------------------------------------

/// Build a `ResolvedTy::TraitObject` for a single-trait bound with no
/// type args or associated bindings. Used to allocate the
/// fat-pointer-typed local that holds the call receiver.
fn dyn_trait_ty(trait_name: &str) -> ResolvedTy {
    ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: trait_name.to_string(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    }
}

fn empty_pipeline(raw_mir: Vec<RawMirFunction>) -> IrPipeline {
    IrPipeline {
        thir: vec![],
        raw_mir,
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    // RAII temp dir — dropped at function exit (after `.ll` is slurped
    // into memory). Prevents `hew-dyn-dispatch-*` accumulation under
    // `std::env::temp_dir()` across the 3× flake gate (LESSONS:
    // `cleanup-all-exits` P0). `tempfile::TempDir` is a dev-dep of
    // hew-codegen-rs (see `Cargo.toml`); its `Drop` impl runs
    // `remove_dir_all` best-effort and never panics on cleanup failure.
    let tmp = tempfile::Builder::new()
        .prefix(&format!("hew-dyn-dispatch-{module_name}-"))
        .tempdir()
        .expect("create temp dir");
    let options = EmitOptions {
        module_name,
        out_dir: tmp.path(),
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options).expect("emit_module must succeed");
    let ll_path: &Path = artefacts.ll_path.as_deref().expect("emit_module ll_path");
    let ll = std::fs::read_to_string(ll_path).expect("read ll");
    // `tmp` drops here — directory and its contents removed.
    drop(tmp);
    ll
}

fn try_emit(pipeline: &IrPipeline) -> Result<(), hew_codegen_rs::CodegenError> {
    hew_codegen_rs::validate_codegen_front(pipeline)
}

/// Build a caller fn:
/// ```text
///   fn caller(recv: dyn <trait_name>) -> <ret> {
///       let r = recv.<method>(<args...>);
///       return r;
///   }
/// ```
///
/// Locals layout:
/// - `Local(0)` = receiver fat pointer (param 0)
/// - `Local(1..=n)` = caller-side arg locals (params 1..=n), if any
/// - `Local(n+1)` = result of the dispatch when `ret != Unit`
fn caller_with_dispatch(
    trait_name: &str,
    method_name: &str,
    slot: u32,
    ret: ResolvedTy,
    arg_tys: Vec<ResolvedTy>,
    sig: FnSig,
) -> RawMirFunction {
    let fat_ty = dyn_trait_ty(trait_name);
    let mut params: Vec<ResolvedTy> = vec![fat_ty.clone()];
    params.extend(arg_tys.iter().cloned());

    let arg_count = arg_tys.len();
    let arg_places: Vec<Place> = (1..=arg_count)
        .map(|i| Place::Local(u32::try_from(i).expect("arg index fits u32")))
        .collect();

    let mut locals: Vec<ResolvedTy> = params.clone();
    let result_idx = u32::try_from(params.len()).expect("result idx fits u32");
    let returns_value = !matches!(ret, ResolvedTy::Unit);
    if returns_value {
        locals.push(ret.clone());
    }

    let mut instructions = vec![Instr::CallTraitMethod {
        fat_pointer: Place::Local(0),
        dest: if returns_value {
            Some(Place::Local(result_idx))
        } else {
            None
        },
        trait_name: trait_name.to_string(),
        method_name: method_name.to_string(),
        slot,
        args: arg_places,
        signature: Box::new(sig),
    }];
    if returns_value {
        instructions.push(Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(result_idx),
        });
    }

    RawMirFunction {
        name: "caller".to_string(),
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
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }
}

// -- Tests ------------------------------------------------------------

/// Zero-arg method on a `dyn Speak` returning `i64` lowers to the
/// canonical four-step sequence: extract data + vtable, GEP at the
/// method slot, load the fn pointer, indirect-call with `(data_ptr)`.
///
/// Verifies the *minimum-arity dispatch shape* — every later test
/// adds an axis (arg count, method index) over this baseline.
// WINDOWS-TODO: zero-arg vtable dispatch emits different IR on Windows; needs investigation.
#[cfg_attr(windows, ignore)]
#[test]
fn call_trait_method_zero_arg_dispatches_through_vtable() {
    // `fn speak(&self) -> i64`: receiver-skipped sig is `() -> i64`.
    let sig = FnSig {
        params: vec![],
        return_type: Ty::I64,
        ..FnSig::default()
    };
    let caller = caller_with_dispatch("Speak", "speak", 3, ResolvedTy::I64, vec![], sig);
    let p = empty_pipeline(vec![caller]);
    let ll = emit_ll(&p, "zero_arg_dispatch");

    // Fat-pointer load + extractvalue at indices 0 and 1.
    assert!(
        ll.contains("load %hew.dyn.fat_ptr"),
        "dispatch must load the fat-pointer aggregate; got:\n{ll}"
    );
    assert!(
        ll.contains("extractvalue %hew.dyn.fat_ptr") && ll.contains(", 0"),
        "dispatch must extractvalue at index 0 (data word); got:\n{ll}"
    );
    assert!(
        ll.contains(", 1\n") || ll.contains(", 1 "),
        "dispatch must extractvalue at index 1 (vtable word); got:\n{ll}"
    );

    // GEP into the synthetic prefix-view struct at index 3 (first
    // method slot). On a 64-bit host the view at slot 3 is
    // `{ ptr, i64, i64, ptr }`; LLVM textual IR carries this inline
    // at the `getelementptr` site.
    assert!(
        ll.contains("getelementptr") && ll.contains("ptr, i64, i64, ptr"),
        "dispatch must GEP through a prefix-view struct \
         `{{ ptr, i64, i64, ptr }}` at slot 3; got:\n{ll}"
    );

    // Indirect call with one arg (the erased data pointer). The
    // `dyn_call_result` value name comes from `lower_call_trait_method`.
    assert!(
        ll.contains("call i64 %dyn_method_fn_ptr") || ll.contains("call i64 %"),
        "dispatch must indirect-call through the loaded fn pointer \
         returning `i64`; got:\n{ll}"
    );
}

/// One-arg method `Counter::add(i64) -> i64` lowers to a dispatch
/// whose indirect-call carries `(ptr, i64)` — `ptr` is the erased
/// self word and the `i64` is the source-level arg loaded from
/// `Local(1)`. Pins the "prepend ptr, lower params verbatim"
/// invariant for the erased indirect-call signature.
#[test]
fn call_trait_method_one_arg_dispatches_through_vtable() {
    // `fn add(&self, x: i64) -> i64`: receiver-skipped sig is
    // `(i64) -> i64`. Codegen prepends `ptr` → erased call type
    // `(ptr, i64) -> i64`.
    let sig = FnSig {
        params: vec![Ty::I64],
        return_type: Ty::I64,
        ..FnSig::default()
    };
    let caller = caller_with_dispatch(
        "Counter",
        "add",
        3,
        ResolvedTy::I64,
        vec![ResolvedTy::I64],
        sig,
    );
    let p = empty_pipeline(vec![caller]);
    let ll = emit_ll(&p, "one_arg_dispatch");

    // The call site emits `call i64 %fn_ptr(ptr %data, i64 %arg)`.
    // We accept any inkwell-generated SSA names for the values;
    // pinning the *types* in the call signature is the contract.
    let call_lines: Vec<&str> = ll
        .lines()
        .filter(|l| l.contains("call i64") && !l.contains("define"))
        .collect();
    let dispatch_line = call_lines
        .iter()
        .find(|l| l.contains("(ptr ") && l.contains("i64 "))
        .unwrap_or_else(|| {
            panic!(
                "expected an indirect `call i64` line carrying `(ptr, i64)`; \
                 found call lines:\n{call_lines:#?}\nfull IR:\n{ll}"
            )
        });
    assert!(
        dispatch_line.contains("call i64"),
        "indirect call must return `i64`; got: {dispatch_line}"
    );
}

/// A trait with two methods: calling the *second* method must GEP
/// at slot 4, NOT slot 3. Pins the
/// `slot = 3 + method_decl_order` convention from
/// `hew_types::DynMethodCall::slot` — codegen consumes the checker's
/// pre-computed `slot` verbatim and carries no method-order
/// knowledge of its own.
#[test]
fn call_trait_method_multi_method_indexes_correctly() {
    // `Pair::b(&self) -> i64` — second method (slot 4). The
    // view struct extends through field index 4, so on a 64-bit
    // host the view is `{ ptr, i64, i64, ptr, ptr }`.
    let sig = FnSig {
        params: vec![],
        return_type: Ty::I64,
        ..FnSig::default()
    };
    let caller = caller_with_dispatch("Pair", "b", 4, ResolvedTy::I64, vec![], sig);
    let p = empty_pipeline(vec![caller]);
    let ll = emit_ll(&p, "multi_method_slot");

    // The prefix-view struct for slot 4 carries TWO trailing
    // `ptr` slots (indices 3 and 4) so GEP at index 4 lands on
    // the second method slot.
    assert!(
        ll.contains("getelementptr") && ll.contains("ptr, i64, i64, ptr, ptr"),
        "dispatch at slot 4 must GEP through a prefix-view struct \
         `{{ ptr, i64, i64, ptr, ptr }}`; got:\n{ll}"
    );
    // Sanity: a slot-3-only view would NOT appear.
    let view_at_3_only =
        ll.contains("ptr, i64, i64, ptr }") && !ll.contains("ptr, i64, i64, ptr, ptr");
    assert!(
        !view_at_3_only,
        "dispatch at slot 4 must NOT emit a slot-3 view `{{ ptr, i64, i64, ptr }}`; got:\n{ll}"
    );
    // The GEP index `4` must appear on the GEP line — inkwell
    // emits `getelementptr inbounds %0, ptr %vt, i32 0, i32 4`.
    let gep_line = ll
        .lines()
        .find(|l| l.contains("getelementptr") && l.contains("i32 4"))
        .unwrap_or_else(|| panic!("expected a `getelementptr ... i32 4` line; got:\n{ll}"));
    assert!(
        gep_line.contains("i32 4"),
        "GEP into the vtable at slot 4 must index field 4; got: {gep_line}"
    );
}

/// MIR producer drift: receiver `Place` whose slot type is NOT
/// `%hew.dyn.fat_ptr` (e.g. a plain `i64` local) must fail closed
/// before any LLVM is committed. The only producer of `dyn Trait`
/// locals is `primitive_to_llvm`'s `ResolvedTy::TraitObject` arm
/// (which produces `%hew.dyn.fat_ptr`); reaching the dispatch arm
/// with any other slot type is an upstream invariant violation.
#[test]
fn call_trait_method_fails_closed_when_receiver_not_fat_pointer() {
    // Caller fn whose Local(0) is `i64`, not `dyn Trait`. The
    // dispatch arm must reject it instead of emitting a bogus GEP.
    let sig = FnSig {
        params: vec![],
        return_type: Ty::I64,
        ..FnSig::default()
    };
    let bogus = RawMirFunction {
        name: "caller_bad_recv".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![ResolvedTy::I64], // <-- not a TraitObject!
        locals: vec![ResolvedTy::I64, ResolvedTy::I64],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::CallTraitMethod {
                    fat_pointer: Place::Local(0),
                    dest: Some(Place::Local(1)),
                    trait_name: "Speak".to_string(),
                    method_name: "speak".to_string(),
                    slot: 3,
                    args: vec![],
                    signature: Box::new(sig),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let p = empty_pipeline(vec![bogus]);
    let err = try_emit(&p).expect_err("non-fat-pointer receiver must fail closed");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("Speak") && msg.contains("speak"),
        "fail-closed message must name the trait/method; got: {msg}"
    );
    assert!(
        msg.contains("fat_ptr") || msg.contains("fat-pointer"),
        "fail-closed message must reference the fat-pointer slot-type \
         requirement; got: {msg}"
    );
}

/// Defence-in-depth: `slot < 3` collides with the 3-slot vtable
/// prefix (drop_fn / size_of / align_of). The checker's
/// `DynMethodCall::slot = 3 + method_decl_order` rule is the only
/// authority that produces this field; `slot < 3` is an upstream
/// invariant violation and must fail closed at the dispatch arm.
#[test]
fn call_trait_method_fails_closed_when_slot_collides_with_prefix() {
    let sig = FnSig {
        params: vec![],
        return_type: Ty::I64,
        ..FnSig::default()
    };
    let mut caller = caller_with_dispatch("Speak", "speak", 3, ResolvedTy::I64, vec![], sig);
    // Stomp the slot to 2 (the `align_of` prefix slot) post-build,
    // mirroring an upstream producer that miscomputed `slot`.
    for block in &mut caller.blocks {
        for instr in &mut block.instructions {
            if let Instr::CallTraitMethod { slot, .. } = instr {
                *slot = 2;
            }
        }
    }
    let p = empty_pipeline(vec![caller]);
    let err = try_emit(&p).expect_err("slot < 3 must fail closed");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("slot 2") && (msg.contains("prefix") || msg.contains("drop_fn")),
        "fail-closed message must call out the prefix collision; got: {msg}"
    );
}
