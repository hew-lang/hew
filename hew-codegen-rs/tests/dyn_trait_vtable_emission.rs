//! LLVM emission for `dyn Trait` vtable static definitions.
//!
//! Each entry in `IrPipeline::dyn_vtable_registry` causes codegen to
//! finalise the `@__hew_vtable__{trait}__{concrete}__N` `Linkage::Private`
//! constant (declared at every `Instr::CoerceToDynTrait` site with an
//! opaque body) into a defined constant whose body is the 3-slot prefix
//! triple `{ ptr, <usize>, <usize> }` followed by N method-thunk
//! pointer slots — i.e. `{ ptr, i64, i64, ptr × method_count }` on a
//! 64-bit host. The initializer is
//! `{ drop_in_place_fn, size_of<Concrete>, align_of<Concrete>,
//!   thunk_0, ..., thunk_(N-1) }`.
//!
//! The prefix triple mirrors `HewVtable` in
//! `hew-runtime/src/trait_object.rs:108-130` (the runtime ABI commit)
//! and the checker's `DynMethodCall::slot = 3 + method_decl_order`
//! convention. Slot 0 carries the unconditional
//! `__hew_dyn_drop_in_place__*` fn pointer; slots 1 and 2 carry the
//! concrete type's `size_of` / `align_of` as ptr-sized integers
//! (sourced from the LLVM ABI layout of the concrete type via the
//! same authority `emit_dyn_trait_drop_in_place_fns` uses for the
//! `hew_dyn_box_free(ptr, size, align)` call); slots 3..N carry the
//! erased-self thunk pointers in `vtable_entries` declaration order.
//! The vtable definition reuses the SAME named struct
//! `%hew.dyn.vtable.N` the declaration site created — the body is
//! filled by `StructType::set_body` and the global's initializer by
//! `GlobalValue::set_initializer`, so every prior fat-pointer
//! construction remains ABI-stable.
//!
//! Drop-fn routing: slot 0 is always the unconditional
//! `hew_dyn_box_free`-calling drop fn. Frame-owned consumers (the only
//! live storage class today) MUST NOT fire slot 0 at scope exit — the
//! drop-emission consumer at the use site routes by
//! `TraitObjectStorage`, not by the vtable. This keeps the ABI surface
//! a single fn-pointer slot in the prefix and matches the frame-owned
//! data-pointer convention.
//!
//! Linkage: vtable globals, drop fns, and thunks are all emitted with
//! `Linkage::Private`. The symbol names carry trait/concrete substrings
//! for diagnostic legibility and a `vtable_id` numeric tail for
//! uniqueness across registry dedup-key dimensions.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    mangle_dyn_drop_in_place_symbol, mangle_dyn_thunk_symbol, mangle_dyn_vtable_symbol, BasicBlock,
    DynVtableInstance, FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::{DynVtableEntry, FnSig, ResolvedTy, Ty};

// -- Scaffolding ------------------------------------------------------

/// Build a minimal `RawMirFunction` whose LLVM signature is
/// `fn(ptr, <extra_param_tys>...) -> <ret>`. Matches the scaffold in
/// `dyn_trait_thunk_emission.rs` so vtable + thunk + impl-fn shape
/// stay aligned. The body returns a constant `7` for `I64` returns
/// (sufficient for fail-closed checks; no test below asserts a
/// runtime value).
fn impl_method_stub(name: &str, ret: ResolvedTy) -> RawMirFunction {
    // Slot 0 = receiver-shaped pointer (`ResolvedTy::String` lowers
    // to `ptr`, see `primitive_to_llvm`). Single-parameter receiver is
    // the minimal forwardable shape.
    let params = vec![ResolvedTy::String];
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
    }
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let nonce = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let tmp = std::env::temp_dir().join(format!("hew-dyn-vtable-{module_name}-{nonce}"));
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

// -- Tests ------------------------------------------------------------

/// Empty `dyn_vtable_registry` → no vtable definition emitted. The
/// emission pass must be a no-op when no `Instr::CoerceToDynTrait`
/// site exists anywhere in the module.
#[test]
fn empty_registry_emits_no_vtable_definitions() {
    let p = pipeline_with(vec![], vec![]);
    let ll = emit_ll(&p, "empty_registry");
    assert!(
        !ll.contains("@__hew_vtable__"),
        "no vtable symbols must appear when the registry is empty; got:\n{ll}"
    );
    assert!(
        !ll.contains("%hew.dyn.vtable."),
        "no vtable named-struct types must appear when the registry is empty; got:\n{ll}"
    );
}

/// Single registry entry with zero methods → the `Speak`/`i64`
/// vtable is defined with a 3-slot body holding the prefix triple
/// (drop fn ptr, size, align) and no method slots. The opaque
/// struct `%hew.dyn.vtable.0` is finalised with body
/// `{ ptr, i64, i64 }` on the 64-bit host (concrete = `i64`,
/// size = 8, align = 8).
#[test]
fn single_entry_zero_methods_emits_drop_only_vtable() {
    let inst = vtable_instance(0, "Speak", ResolvedTy::I64, vec![]);
    let vtable_sym = mangle_dyn_vtable_symbol(0, "Speak", &ResolvedTy::I64);
    let p = pipeline_with(vec![], vec![inst]);
    let ll = emit_ll(&p, "drop_only_vtable");

    // Definition, not declaration. Linkage is `Private`.
    let def_marker = format!("@{vtable_sym} = private constant");
    assert!(
        ll.contains(&def_marker),
        "vtable must be defined with `Private` linkage \
        ; expected `{def_marker}`; got:\n{ll}"
    );
    assert!(
        !ll.contains(&format!("@{vtable_sym} = external")),
        "vtable static must not appear with any `external` linkage form; got:\n{ll}"
    );

    // Body of the named struct is the 3-slot prefix triple — no
    // method slots when `vtable_entries` is empty.
    assert!(
        ll.contains("%hew.dyn.vtable.0 = type { ptr, i64, i64 }"),
        "vtable named struct body for a zero-method trait must be the prefix triple \
         `{{ ptr, i64, i64 }}` (drop fn ptr + size_of + align_of); got:\n{ll}"
    );

    // Initializer references the drop-in-place fn synthesised by
    // `emit_dyn_trait_drop_in_place_fns`, then the concrete `i64`'s
    // size and alignment (both 8 on the 64-bit host).
    let drop_sym = mangle_dyn_drop_in_place_symbol(0, "Speak", &ResolvedTy::I64);
    let init_marker = format!("@{vtable_sym} = private constant");
    let init_line = ll
        .lines()
        .find(|l| l.contains(&init_marker))
        .unwrap_or_else(|| panic!("vtable definition line not found; full IR:\n{ll}"));
    assert!(
        init_line.contains(&format!("@{drop_sym}")),
        "vtable initializer must reference `@{drop_sym}` at slot 0; got:\n{init_line}"
    );
    // `i64`'s ABI on the 64-bit host: size = 8, align = 8.
    assert!(
        init_line.matches("i64 8").count() >= 2,
        "vtable initializer must encode size_of<i64> = 8 (slot 1) AND \
         align_of<i64> = 8 (slot 2) as `i64 8` constants; got:\n{init_line}"
    );
    // The drop-in-place fn itself is defined.
    assert!(
        ll.contains(&format!("define private void @{drop_sym}(")),
        "drop-in-place fn `@{drop_sym}` must be defined with `Private` \
         linkage; got:\n{ll}"
    );
}

/// Single registry entry with one method → the `Counter`/`i64`
/// vtable body is `{ ptr, i64, i64, ptr }` (3-slot prefix + 1
/// thunk slot), initializer is
/// `{ drop_in_place_fn, 8, 8, thunk_0 }` for the `i64` concrete.
#[test]
fn single_entry_one_method_emits_drop_plus_thunk_vtable() {
    let impl_fn = impl_method_stub("Counter_next_impl", ResolvedTy::I64);
    let entry = vtable_entry("Counter", "next", "Counter_next_impl");
    let inst = vtable_instance(
        0,
        "Counter",
        ResolvedTy::I64,
        vec![("next".to_string(), "Counter_next_impl".to_string(), entry)],
    );
    let vtable_sym = mangle_dyn_vtable_symbol(0, "Counter", &ResolvedTy::I64);
    let p = pipeline_with(vec![impl_fn], vec![inst]);
    let ll = emit_ll(&p, "drop_plus_one_thunk_vtable");

    assert!(
        ll.contains("%hew.dyn.vtable.0 = type { ptr, i64, i64, ptr }"),
        "vtable named struct body for a one-method trait must be \
         `{{ ptr, i64, i64, ptr }}` (prefix triple + method slot); got:\n{ll}"
    );
    assert!(
        ll.contains(&format!("@{vtable_sym} = private constant")),
        "vtable must be a `private` defined constant; got:\n{ll}"
    );

    let drop_sym = mangle_dyn_drop_in_place_symbol(0, "Counter", &ResolvedTy::I64);
    let thunk_sym = mangle_dyn_thunk_symbol(0, 0, "Counter", &ResolvedTy::I64);
    // Initializer must reference both fn pointers and both size/align
    // constants. Substring search on the initializer line is
    // sufficient to pin the layout; full IR contains both fn defs
    // independently which we exclude by scoping to the
    // `<vtable_sym> = private constant` line.
    let init_marker = format!("@{vtable_sym} = private constant");
    let init_line = ll
        .lines()
        .find(|l| l.contains(&init_marker))
        .unwrap_or_else(|| panic!("vtable definition line not found; full IR:\n{ll}"));
    let drop_pos = init_line.find(&format!("@{drop_sym}")).unwrap_or_else(|| {
        panic!("vtable initializer must reference @{drop_sym}; got:\n{init_line}")
    });
    let thunk_pos = init_line.find(&format!("@{thunk_sym}")).unwrap_or_else(|| {
        panic!("vtable initializer must reference @{thunk_sym}; got:\n{init_line}")
    });
    assert!(
        drop_pos < thunk_pos,
        "slot 0 (drop) must precede slot 3 (thunk) in the initializer; got:\n{init_line}"
    );
    // Size and align constants for `i64` (8 / 8) must appear between
    // the drop fn (slot 0) and the thunk (slot 3).
    assert!(
        init_line.matches("i64 8").count() >= 2,
        "initializer must encode size_of<i64> = 8 (slot 1) and \
         align_of<i64> = 8 (slot 2) as `i64 8` constants; got:\n{init_line}"
    );
}

/// Two methods in a single vtable → `Trait`/`i64` body is
/// `{ ptr, i64, i64, ptr, ptr }` (3-slot prefix + 2 thunk slots)
/// and the two thunk pointers appear in `vtable_entries`
/// declaration order at slots 3 and 4.
#[test]
fn two_methods_emit_in_declaration_order() {
    let impl_a = impl_method_stub("Trait_a_impl", ResolvedTy::I64);
    let impl_b = impl_method_stub("Trait_b_impl", ResolvedTy::I64);
    let entry_a = vtable_entry("Trait", "a", "Trait_a_impl");
    let entry_b = vtable_entry("Trait", "b", "Trait_b_impl");
    let inst = vtable_instance(
        0,
        "Trait",
        ResolvedTy::I64,
        vec![
            ("a".to_string(), "Trait_a_impl".to_string(), entry_a),
            ("b".to_string(), "Trait_b_impl".to_string(), entry_b),
        ],
    );
    let vtable_sym = mangle_dyn_vtable_symbol(0, "Trait", &ResolvedTy::I64);
    let p = pipeline_with(vec![impl_a, impl_b], vec![inst]);
    let ll = emit_ll(&p, "two_methods_order");

    assert!(
        ll.contains("%hew.dyn.vtable.0 = type { ptr, i64, i64, ptr, ptr }"),
        "two-method vtable body must be `{{ ptr, i64, i64, ptr, ptr }}` \
         (prefix triple + 2 method slots); got:\n{ll}"
    );

    let init_marker = format!("@{vtable_sym} = private constant");
    let init_line = ll
        .lines()
        .find(|l| l.contains(&init_marker))
        .unwrap_or_else(|| panic!("vtable definition line not found; full IR:\n{ll}"));
    let thunk_a = mangle_dyn_thunk_symbol(0, 0, "Trait", &ResolvedTy::I64);
    let thunk_b = mangle_dyn_thunk_symbol(0, 1, "Trait", &ResolvedTy::I64);
    let pos_a = init_line
        .find(&format!("@{thunk_a}"))
        .unwrap_or_else(|| panic!("missing @{thunk_a}: {init_line}"));
    let pos_b = init_line
        .find(&format!("@{thunk_b}"))
        .unwrap_or_else(|| panic!("missing @{thunk_b}: {init_line}"));
    assert!(
        pos_a < pos_b,
        "method `a` (slot 3) must precede method `b` (slot 4) in initializer; got:\n{init_line}"
    );
    // Prefix triple's size/align (8/8 for the `i64` concrete) must
    // appear between the drop fn and the first thunk.
    assert!(
        init_line.matches("i64 8").count() >= 2,
        "initializer must encode size_of<i64> = 8 (slot 1) and \
         align_of<i64> = 8 (slot 2) as `i64 8` constants; got:\n{init_line}"
    );
}

/// Two registry entries → two independent vtables, each with its own
/// drop-in-place fn and named struct. The vtable_id discriminator
/// must keep the two symbol families distinct. Both vtables carry
/// the 3-slot prefix triple — vtable 0's concrete is `i64`
/// (size = 8, align = 8 ⇒ body `{ ptr, i64, i64 }`); vtable 1's
/// concrete is `bool` (LLVM `i1` lowering, ABI size = 1, align = 1
/// ⇒ body shape `{ ptr, i64, i64 }` with different initializer
/// constants).
#[test]
fn two_registry_entries_emit_distinct_vtables() {
    let inst_0 = vtable_instance(0, "Speak", ResolvedTy::I64, vec![]);
    let inst_1 = vtable_instance(1, "Speak", ResolvedTy::Bool, vec![]);
    let vt0 = mangle_dyn_vtable_symbol(0, "Speak", &ResolvedTy::I64);
    let vt1 = mangle_dyn_vtable_symbol(1, "Speak", &ResolvedTy::Bool);
    let p = pipeline_with(vec![], vec![inst_0, inst_1]);
    let ll = emit_ll(&p, "two_distinct_vtables");

    assert!(
        ll.contains("%hew.dyn.vtable.0 = type { ptr, i64, i64 }"),
        "vtable 0 named struct must carry the prefix triple; got:\n{ll}"
    );
    assert!(
        ll.contains("%hew.dyn.vtable.1 = type { ptr, i64, i64 }"),
        "vtable 1 named struct must carry the prefix triple; got:\n{ll}"
    );
    assert!(
        ll.contains(&format!("@{vt0} = private constant")),
        "vtable 0 must be defined; got:\n{ll}"
    );
    assert!(
        ll.contains(&format!("@{vt1} = private constant")),
        "vtable 1 must be defined; got:\n{ll}"
    );
    // Each vtable references its OWN drop-in-place fn.
    let drop0 = mangle_dyn_drop_in_place_symbol(0, "Speak", &ResolvedTy::I64);
    let drop1 = mangle_dyn_drop_in_place_symbol(1, "Speak", &ResolvedTy::Bool);
    assert!(
        ll.contains(&format!("@{drop0}")),
        "drop-in-place fn for vtable 0 missing; got:\n{ll}"
    );
    assert!(
        ll.contains(&format!("@{drop1}")),
        "drop-in-place fn for vtable 1 missing; got:\n{ll}"
    );
    // Verify the prefix-triple constants are concrete-correct.
    // vtable 0 (concrete = `i64`): size 8 / align 8.
    let init0_marker = format!("@{vt0} = private constant");
    let init0 = ll
        .lines()
        .find(|l| l.contains(&init0_marker))
        .unwrap_or_else(|| panic!("vtable 0 definition line not found; IR:\n{ll}"));
    assert!(
        init0.matches("i64 8").count() >= 2,
        "vtable 0 (concrete i64) initializer must encode size = 8 and align = 8; \
         got:\n{init0}"
    );
    // vtable 1 (concrete = `bool` → LLVM `i1`): LLVM ABI is size 1 /
    // align 1 on the 64-bit host. Both encoded as `i64` constants
    // because the prefix slot type is ptr-sized.
    let init1_marker = format!("@{vt1} = private constant");
    let init1 = ll
        .lines()
        .find(|l| l.contains(&init1_marker))
        .unwrap_or_else(|| panic!("vtable 1 definition line not found; IR:\n{ll}"));
    assert!(
        init1.matches("i64 1").count() >= 2,
        "vtable 1 (concrete bool/i1) initializer must encode size = 1 and align = 1 \
         (`i64 1` constants in the ptr-sized prefix slots); got:\n{init1}"
    );
}

/// Coercion site present in MIR → the same vtable that
/// `lower_coerce_to_dyn_trait` declared at the coercion site is the
/// one `emit_dyn_trait_vtable_definitions` finalises. References
/// emitted at the coercion site must stay bit-identical (same vtable
/// symbol, same `%hew.dyn.vtable.0` named struct) — the body and
/// initializer are the only fields the finalisation changes. This
/// pins the ABI-stability invariant the opaque-named-struct +
/// `set_body` + `set_initializer` path guarantees.
#[test]
fn coercion_site_and_vtable_definition_share_same_symbol() {
    let main = RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![
            ResolvedTy::I64,
            ResolvedTy::TraitObject {
                traits: vec![hew_types::ResolvedTraitBound {
                    trait_name: "Speak".to_string(),
                    args: vec![],
                    assoc_bindings: vec![],
                }],
            },
        ],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 42,
                },
                Instr::CoerceToDynTrait {
                    value: Place::Local(0),
                    dest: Place::Local(1),
                    trait_name: "Speak".to_string(),
                    concrete_type: ResolvedTy::I64,
                    method_table: vec![],
                    vtable_entries: vec![],
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
    };
    let inst = vtable_instance(0, "Speak", ResolvedTy::I64, vec![]);
    let vtable_sym = mangle_dyn_vtable_symbol(0, "Speak", &ResolvedTy::I64);
    let p = pipeline_with(vec![main], vec![inst]);
    let ll = emit_ll(&p, "coercion_plus_definition");

    // The coercion site references the SAME vtable global the
    // definition introduces.
    assert!(
        ll.contains(&format!("@{vtable_sym} = private constant")),
        "vtable definition must exist with `Private` linkage; got:\n{ll}"
    );
    assert!(
        ll.contains(&format!("ptr @{vtable_sym}")),
        "fat-pointer construction at the coercion site must reference \
         `@{vtable_sym}` directly; got:\n{ll}"
    );
    // No `external` form of the vtable must ever appear; the
    // declaration site emits it with `Linkage::Private` from the start.
    assert!(
        !ll.contains(&format!("@{vtable_sym} = external")),
        "vtable static must not appear with any `external` linkage \
         form; got:\n{ll}"
    );
}
