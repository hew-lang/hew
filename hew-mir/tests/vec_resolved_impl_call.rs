use std::collections::HashMap;

use hew_hir::{
    ids::IdGen, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmt,
    HirStmtKind, IntentKind, ScopeId, ValueClass,
};
use hew_mir::lower_hir_module;
use hew_types::{BuiltinType, ImplId, ResolvedTy};

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        diagnostic_source_modules: HashMap::default(),
        type_classes: HashMap::default(),
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        record_layouts: vec![],
        enum_layouts: vec![],
        machine_instantiations: vec![],
        supervisor_child_slots: HashMap::default(),
        regex_literals: vec![],
    }
}

fn unit_expr(ids: &mut IdGen, ty: ResolvedTy) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Unit),
        span: 0..0,
    }
}

#[test]
fn vec_resolved_impl_call_wrong_arity_panics_fail_closed() {
    let mut ids = IdGen::default();
    let vec_i64 = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]);
    let receiver = unit_expr(&mut ids, vec_i64);
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            impl_id: ImplId(2),
            method_name: "len".to_string(),
            target_symbol: "hew_vec_len".to_string(),
            type_args: vec![],
            args: vec![],
            ret_ty: ResolvedTy::Unit,
        },
        span: 0..0,
    };
    let stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Expr(call),
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
    let module = empty_module(vec![HirItem::Function(HirFn {
        id: ids.item(),
        node: ids.node(),
        name: "main".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: ResolvedTy::Unit,
        body,
        span: 0..0,
    })]);

    let panic = std::panic::catch_unwind(|| {
        let _ = lower_hir_module(&module);
    })
    .expect_err("Vec ResolvedImplCall with zero type args must panic at the MIR arity gate");

    let message = panic
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| panic.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");
    assert!(
        message.contains("registers Vec impls with 1 type-arg (T)"),
        "panic should name the Vec populator/MIR drift; got: {message}"
    );
}
