//! MIR producer tests for `bytes.len()` and `bytes.get()` runtime calls.
//!
//! `bytes.len()` and `bytes.get(i)` are declared in `std/io.hew`'s
//! `impl bytes` extern block. `len` binds `#[extern_symbol(hew_vec_len)]` and
//! lowers to an `Instr::CallRuntimeAbi(hew_vec_len)`. `get` binds
//! `#[extern_symbol(hew_bytes_get)]` returning `Option<u8>`; it lowers to a
//! `Terminator::Call { callee: "hew_bytes_get", dest: Some(Option<u8>) }` whose
//! callee codegen intercepts to build `Some(byte)` in bounds / `None` out of
//! bounds (the trapping `b[i]` half stays on the dedicated `hew_bytes_index`
//! getter — `get` is de-aliased from it).
//!
//! After HIR lowers the method call the callee becomes
//! `BindingRef { name, resolved: Unresolved }`, which
//! `runtime_symbol_for_call_expr` recognises as an allowlisted symbol and
//! routes to `lower_runtime_call`.

use std::collections::HashMap;

use hew_hir::{
    ids::IdGen, HirBinding, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule,
    HirStmt, HirStmtKind, IntentKind, ResolvedRef, ScopeId, ValueClass,
};
use hew_mir::{lower_hir_module, Instr, Place, Terminator};
use hew_types::ResolvedTy;

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        diagnostic_source_modules: HashMap::default(),
        root_item_ids: std::collections::HashSet::new(),
        wire_layouts: std::sync::Arc::new(HashMap::default()),
        type_classes: HashMap::default(),
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        vec_generic_element_abi: HashMap::default(),
        record_layouts: vec![],
        enum_layouts: vec![],
        machine_instantiations: vec![],
        supervisor_child_slots: HashMap::default(),
        pool_accessor_sites: HashMap::default(),
        regex_literals: vec![],
    }
}

fn unit_lit(ids: &mut IdGen) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Unit),
        span: 0..0,
    }
}

fn bytes_lit(ids: &mut IdGen) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Bytes,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Unit),
        span: 0..0,
    }
}

fn i64_lit(ids: &mut IdGen) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Integer(0)),
        span: 0..0,
    }
}

/// Build a `BindingRef { name, resolved: Unresolved }` callee expression.
///
/// When `name` is a recognised runtime symbol, `runtime_symbol_for_call_expr`
/// gates on `is_known_runtime_symbol` before checking `resolved`, so
/// `Unresolved` here is correct and matches the HIR shape produced by the
/// method-call rewrite for `bytes.len()` and `bytes.get()`.
fn runtime_callee(ids: &mut IdGen, name: &str, ret_ty: ResolvedTy) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ret_ty,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: name.to_string(),
            resolved: ResolvedRef::Unresolved,
        },
        span: 0..0,
    }
}

fn call_expr(ids: &mut IdGen, callee: HirExpr, args: Vec<HirExpr>, ret_ty: ResolvedTy) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ret_ty,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Call {
            callee: Box::new(callee),
            args,
        },
        span: 0..0,
    }
}

fn module_with_stmt(ids: &mut IdGen, stmt_expr: HirExpr) -> HirModule {
    let stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Expr(stmt_expr),
        span: 0..0,
    };
    let body = HirBlock {
        node: ids.node(),
        scope: ScopeId(0),
        statements: vec![stmt],
        tail: None,
        ty: ResolvedTy::Unit,
        span: 0..0,
    };
    empty_module(vec![HirItem::Function(HirFn {
        id: ids.item(),
        node: ids.node(),
        name: "probe".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: ResolvedTy::Unit,
        body,
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    })])
}

fn find_probe(pipeline: &hew_mir::IrPipeline) -> &hew_mir::RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "probe")
        .expect("probe function must be present in raw_mir")
}

fn find_abi_call<'a>(
    func: &'a hew_mir::RawMirFunction,
    symbol: &str,
) -> Option<&'a hew_mir::RuntimeCall> {
    func.blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .find_map(|i| {
            if let Instr::CallRuntimeAbi(c) = i {
                if c.symbol() == symbol {
                    return Some(c);
                }
            }
            None
        })
}

/// `Option<u8>` — the de-aliased `bytes.get` return type.
fn option_u8_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Option".to_string(),
        args: vec![ResolvedTy::U8],
        builtin: None,
        is_opaque: false,
    }
}

/// Find the first `Terminator::Call` whose callee matches `callee`.
fn find_terminator_call<'a>(
    func: &'a hew_mir::RawMirFunction,
    callee: &str,
) -> Option<&'a Terminator> {
    func.blocks
        .iter()
        .map(|b| &b.terminator)
        .find(|t| matches!(t, Terminator::Call { callee: c, .. } if c == callee))
}

// ---------------------------------------------------------------------------
// bytes.len() — hew_vec_len producer
// ---------------------------------------------------------------------------

/// `bytes.len()` in statement position (discarded result) must emit
/// `CallRuntimeAbi { symbol: "hew_vec_len", args: [buf], dest: None }`.
///
/// Before the fix this fell into the `_ =>` NYI arm and produced
/// `MirDiagnosticKind::NotYetImplemented` without any instruction.
#[test]
fn bytes_len_discarded_emits_call_runtime_abi() {
    let mut ids = IdGen::default();
    let callee = runtime_callee(&mut ids, "hew_vec_len", ResolvedTy::I64);
    let buf = bytes_lit(&mut ids);
    let call = call_expr(&mut ids, callee, vec![buf], ResolvedTy::I64);
    let module = module_with_stmt(&mut ids, call);

    let pipeline = lower_hir_module(&module);

    assert!(
        pipeline.diagnostics.iter().all(|d| !matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("hew_vec_len")
        )),
        "bytes.len() in discarded position must not produce NYI; diagnostics: {:#?}",
        pipeline.diagnostics
    );

    let func = find_probe(&pipeline);
    let call = find_abi_call(func, "hew_vec_len").expect(
        "bytes.len() must emit Instr::CallRuntimeAbi(hew_vec_len); no such instruction found",
    );

    assert_eq!(
        call.args().len(),
        1,
        "hew_vec_len takes 1 arg (bytes receiver)"
    );
    assert!(
        call.dest().is_none(),
        "discarded bytes.len() must have dest=None; got {:?}",
        call.dest()
    );
}

/// `bytes.len()` in value-needed position must allocate an `i64` dest local.
#[test]
fn bytes_len_value_needed_emits_i64_dest() {
    let mut ids = IdGen::default();
    let callee = runtime_callee(&mut ids, "hew_vec_len", ResolvedTy::I64);
    let buf = bytes_lit(&mut ids);
    let rhs = call_expr(&mut ids, callee, vec![buf], ResolvedTy::I64);

    let binding_id = ids.binding();
    let let_stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Let(
            HirBinding {
                id: binding_id,
                name: "_n".to_string(),
                ty: ResolvedTy::I64,
                mutable: false,
                span: 0..0,
                is_consume: false,
            },
            Some(rhs),
        ),
        span: 0..0,
    };
    let ret_stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Return(Some(unit_lit(&mut ids))),
        span: 0..0,
    };
    let body = HirBlock {
        node: ids.node(),
        scope: ScopeId(0),
        statements: vec![let_stmt, ret_stmt],
        tail: None,
        ty: ResolvedTy::Unit,
        span: 0..0,
    };
    let module = empty_module(vec![HirItem::Function(HirFn {
        id: ids.item(),
        node: ids.node(),
        name: "probe".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: ResolvedTy::Unit,
        body,
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    })]);

    let pipeline = lower_hir_module(&module);

    assert!(
        pipeline.diagnostics.iter().all(|d| !matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("hew_vec_len")
        )),
        "bytes.len() in value-needed position must not produce NYI; diagnostics: {:#?}",
        pipeline.diagnostics
    );

    let func = find_probe(&pipeline);
    let call = find_abi_call(func, "hew_vec_len")
        .expect("bytes.len() in value-needed context must emit hew_vec_len CallRuntimeAbi");

    let dest = call
        .dest()
        .expect("value-needed bytes.len() must carry a dest local");
    let Place::Local(dest_idx) = dest else {
        panic!("dest must be Local; got {dest:?}");
    };
    let dest_ty = func
        .locals
        .get(dest_idx as usize)
        .expect("dest local must be in locals table");
    assert_eq!(
        *dest_ty,
        ResolvedTy::I64,
        "bytes.len() dest local must be typed i64; got {dest_ty:?}"
    );
}

// ---------------------------------------------------------------------------
// bytes.get(i) — hew_bytes_get producer (Option<u8>, de-aliased from b[i])
// ---------------------------------------------------------------------------

/// `bytes.get(index)` lowers to a `Terminator::Call` whose callee is the
/// synthetic `hew_bytes_get` (codegen intercepts it to build the `Option<u8>`),
/// carrying the two producer args `[buf, idx]`. It is NOT an
/// `Instr::CallRuntimeAbi`: the result is an `Option`, constructed at the call
/// site, so the call rides the terminator route the move/borrow/drop analyses
/// already understand for Vec/HashMap `.get`.
#[test]
fn bytes_get_emits_terminator_call_to_hew_bytes_get() {
    let mut ids = IdGen::default();
    let callee = runtime_callee(&mut ids, "hew_bytes_get", option_u8_ty());
    let buf = bytes_lit(&mut ids);
    let idx = i64_lit(&mut ids);
    let call = call_expr(&mut ids, callee, vec![buf, idx], option_u8_ty());
    let module = module_with_stmt(&mut ids, call);

    let pipeline = lower_hir_module(&module);

    assert!(
        pipeline.diagnostics.iter().all(|d| !matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("hew_bytes_get")
        )),
        "bytes.get() must not produce NYI; diagnostics: {:#?}",
        pipeline.diagnostics
    );

    let func = find_probe(&pipeline);
    assert!(
        find_abi_call(func, "hew_bytes_get").is_none(),
        "bytes.get() must NOT emit Instr::CallRuntimeAbi — it rides Terminator::Call"
    );

    let term = find_terminator_call(func, "hew_bytes_get")
        .expect("bytes.get() must emit Terminator::Call(hew_bytes_get)");
    let Terminator::Call { args, .. } = term else {
        unreachable!("find_terminator_call returns only Terminator::Call");
    };
    assert_eq!(
        args.len(),
        2,
        "hew_bytes_get takes 2 producer args (bytes receiver, index); got {args:?}"
    );
}

/// `bytes.get(i)` in value-needed context allocates an `Option<u8>` dest local:
/// the de-aliased getter returns `Option<u8>` (the `Some`/`None` is built at the
/// call site by codegen), so the dest follows that type rather than a bare `u8`.
#[test]
fn bytes_get_value_needed_dest_is_option_u8() {
    let mut ids = IdGen::default();
    let callee = runtime_callee(&mut ids, "hew_bytes_get", option_u8_ty());
    let buf = bytes_lit(&mut ids);
    let idx = i64_lit(&mut ids);
    let rhs = call_expr(&mut ids, callee, vec![buf, idx], option_u8_ty());

    let binding_id = ids.binding();
    let let_stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Let(
            HirBinding {
                id: binding_id,
                name: "_b".to_string(),
                ty: option_u8_ty(),
                mutable: false,
                span: 0..0,
                is_consume: false,
            },
            Some(rhs),
        ),
        span: 0..0,
    };
    let ret_stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Return(Some(unit_lit(&mut ids))),
        span: 0..0,
    };
    let body = HirBlock {
        node: ids.node(),
        scope: ScopeId(0),
        statements: vec![let_stmt, ret_stmt],
        tail: None,
        ty: ResolvedTy::Unit,
        span: 0..0,
    };
    let module = empty_module(vec![HirItem::Function(HirFn {
        id: ids.item(),
        node: ids.node(),
        name: "probe".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: ResolvedTy::Unit,
        body,
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    })]);

    let pipeline = lower_hir_module(&module);

    assert!(
        pipeline.diagnostics.iter().all(|d| !matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("hew_bytes_get")
        )),
        "bytes.get() in value-needed position must not produce NYI; diagnostics: {:#?}",
        pipeline.diagnostics
    );

    let func = find_probe(&pipeline);
    let term = find_terminator_call(func, "hew_bytes_get")
        .expect("value-needed bytes.get() must emit Terminator::Call(hew_bytes_get)");
    let Terminator::Call { dest, .. } = term else {
        unreachable!("find_terminator_call returns only Terminator::Call");
    };
    let dest = dest
        .as_ref()
        .expect("value-needed bytes.get() must carry a dest place");
    let Place::Local(dest_idx) = dest else {
        panic!("dest must be Local; got {dest:?}");
    };
    let dest_ty = func
        .locals
        .get(*dest_idx as usize)
        .expect("dest local must be in locals table");
    assert_eq!(
        *dest_ty,
        option_u8_ty(),
        "bytes.get() dest local must be typed Option<u8>; got {dest_ty:?}"
    );
}
