//! Checker-to-HIR type flow for hash-collection iteration.
//!
//! Hash collections use declared `IntoIterator` impls, preserving the real
//! receiver type for binding, field and tuple places.

use hew_hir::{HirExpr, HirExprKind, HirFn, HirItem, HirStmtKind};
use hew_types::ResolvedTy;

use crate::support;

fn lower(source: &str) -> hew_hir::LowerOutput {
    let (parsed, tco) = support::checker_pipeline::typecheck_source(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    hew_hir::lower_program_host_target(&parsed.program, &tco, &hew_hir::ResolutionCtx)
}

fn function<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirFn {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => Some(function),
            _ => None,
        })
        .unwrap_or_else(|| panic!("function `{name}` must be lowered"))
}

fn for_iter_init(function: &HirFn) -> &HirExpr {
    function
        .body
        .statements
        .iter()
        .find_map(|statement| {
            let HirStmtKind::Expr(HirExpr {
                kind: HirExprKind::Block(block),
                ..
            }) = &statement.kind
            else {
                return None;
            };
            block
                .statements
                .iter()
                .find_map(|statement| match &statement.kind {
                    HirStmtKind::Let(binding, Some(init))
                        if binding.name.starts_with("__hew_for_iter_") =>
                    {
                        Some(init)
                    }
                    _ => None,
                })
        })
        .unwrap_or_else(|| {
            panic!(
                "function `{}` must contain a lowered for-in iterator initialiser: {:#?}",
                function.name, function.body
            )
        })
}

fn assert_hashset_into_iter(
    output: &hew_hir::LowerOutput,
    function_name: &str,
    elem_ty: ResolvedTy,
    receiver_shape: impl FnOnce(&HirExprKind) -> bool,
) {
    let iter_init = for_iter_init(function(output, function_name));
    let HirExprKind::Call {
        target: hew_types::CallTarget::ImplMethod(_),
        args,
        ..
    } = &iter_init.kind
    else {
        panic!("`{function_name}` must call its declared IntoIterator impl: {iter_init:#?}");
    };
    assert_eq!(
        iter_init.ty,
        ResolvedTy::named_builtin(hew_types::BuiltinType::VecIter, vec![elem_ty.clone()])
    );
    let [receiver] = args.as_slice() else {
        panic!("HashSet elements must carry exactly its receiver: {args:#?}");
    };
    assert_eq!(
        receiver.ty,
        ResolvedTy::Named {
            args: vec![elem_ty],
            head: hew_types::TypeHead::Builtin(hew_types::BuiltinType::HashSet),
            is_opaque: false
        },
        "the IntoIterator receiver must retain the real iterable HashSet type"
    );
    assert!(
        receiver_shape(&receiver.kind),
        "`{function_name}` receiver shape drifted: {receiver:#?}"
    );
}

fn assert_hashmap_into_iter_field(output: &hew_hir::LowerOutput) {
    let function = function(output, "map_into_iter_field");
    let init = function
        .body
        .statements
        .iter()
        .find_map(|statement| match &statement.kind {
            HirStmtKind::Let(binding, Some(init)) if binding.name == "cursor" => Some(init),
            _ => None,
        })
        .expect("map_into_iter_field must bind its cursor");
    let HirExprKind::Call {
        target: hew_types::CallTarget::ImplMethod(_),
        args,
        ..
    } = &init.kind
    else {
        panic!("HashMap field into_iter must call its declared impl, got {init:#?}");
    };
    assert_eq!(
        init.ty,
        ResolvedTy::named_builtin(
            hew_types::BuiltinType::HashMapIter,
            vec![ResolvedTy::I64, ResolvedTy::I64],
        )
    );
    let [receiver] = args.as_slice() else {
        panic!("Map iteration must evaluate exactly one receiver: {args:#?}");
    };
    assert_eq!(
        receiver.ty,
        ResolvedTy::named_builtin(
            hew_types::BuiltinType::HashMap,
            vec![ResolvedTy::I64, ResolvedTy::I64],
        )
    );
    assert!(
        matches!(
            receiver.kind,
            HirExprKind::FieldAccess { ref field, .. } if field == "m"
        ),
        "Map iteration receiver must remain the original field place: {receiver:#?}"
    );
}

#[test]
fn hash_collection_iteration_preserves_receiver_types_across_place_shapes() {
    let output = lower(
        r"type SetBox {
    s: HashSet<i64>;
}

type Outer {
    inner: SetBox;
}

type OwnedBox {
    s: HashSet<string>;
}

type MapBox {
    m: HashMap<i64, i64>;
}

fn direct(s: HashSet<i64>) {
    for x in s {
        let _ = x;
    }
}

fn field(b: SetBox) {
    for x in b.s {
        let _ = x;
    }
}

fn nested(o: Outer) {
    for x in o.inner.s {
        let _ = x;
    }
}

fn tuple_field(pair: (HashSet<i64>, i64)) {
    for x in pair.0 {
        let _ = x;
    }
}

fn owned_field(b: OwnedBox) {
    for x in b.s {
        let _ = x.len();
    }
}

fn map_into_iter_field(b: MapBox) {
    let cursor = b.m.into_iter();
    let _ = cursor;
}
",
    );
    assert!(
        output.diagnostics.is_empty(),
        "accepted HashSet place shapes must lower without boundary diagnostics: {:#?}",
        output.diagnostics
    );

    assert_hashset_into_iter(
        &output,
        "direct",
        ResolvedTy::I64,
        |kind| matches!(kind, HirExprKind::BindingRef { name, .. } if name == "s"),
    );
    assert_hashset_into_iter(
        &output,
        "field",
        ResolvedTy::I64,
        |kind| matches!(kind, HirExprKind::FieldAccess { field, .. } if field == "s"),
    );
    assert_hashset_into_iter(&output, "nested", ResolvedTy::I64, |kind| {
        matches!(
            kind,
            HirExprKind::FieldAccess { object, field }
                if field == "s"
                    && matches!(
                        &object.kind,
                        HirExprKind::FieldAccess { field, .. } if field == "inner"
                    )
        )
    });
    assert_hashset_into_iter(&output, "tuple_field", ResolvedTy::I64, |kind| {
        matches!(kind, HirExprKind::TupleIndex { index: 0, .. })
    });
    assert_hashset_into_iter(
        &output,
        "owned_field",
        ResolvedTy::String,
        |kind| matches!(kind, HirExprKind::FieldAccess { field, .. } if field == "s"),
    );
    assert_hashmap_into_iter_field(&output);
}
