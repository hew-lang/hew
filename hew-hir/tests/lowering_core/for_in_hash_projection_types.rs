//! Checker-to-HIR type-flow pins for synthetic hash-collection for-in projections.
//!
//! The projection call (`HashSet::to_vec`, `HashMap::keys`/`values`) deliberately
//! has a synthetic zero-width span because its result type must not overwrite the
//! real iterable expression's type. The receiver is still the original place,
//! however, and must keep the original place span. Giving the receiver the
//! projection span substitutes the projection's `Vec<T>` result into a field or
//! tuple projection and reaches MIR as a clone from a non-hash receiver.

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

fn assert_hashset_vec_iter(
    output: &hew_hir::LowerOutput,
    function_name: &str,
    elem_ty: ResolvedTy,
    receiver_shape: impl FnOnce(&HirExprKind) -> bool,
) {
    let iter_init = for_iter_init(function(output, function_name));
    let HirExprKind::StructInit {
        name,
        type_args,
        fields,
        base,
    } = &iter_init.kind
    else {
        panic!("`{function_name}` iterator must be a complete VecIter record, got {iter_init:#?}");
    };
    assert_eq!(name, "VecIter");
    assert_eq!(type_args, std::slice::from_ref(&elem_ty));
    assert!(
        base.is_none(),
        "synthetic VecIter must not use update syntax"
    );
    assert_eq!(
        fields
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>(),
        ["vec", "idx"],
        "synthetic VecIter must initialise every field exactly once"
    );

    let vec = &fields[0].1;
    assert_eq!(
        vec.ty,
        ResolvedTy::Named {
            name: "Vec".into(),
            args: vec![elem_ty.clone()],
            builtin: Some(hew_types::BuiltinType::Vec),
            is_opaque: false,
        },
        "`HashSet::to_vec` must carry its checker-authored Vec result type"
    );
    let HirExprKind::ResolvedImplCall {
        receiver,
        method_name,
        target_symbol,
        ret_ty,
        ..
    } = &vec.kind
    else {
        panic!("VecIter.vec must be the resolved HashSet projection, got {vec:#?}");
    };
    assert_eq!(method_name, "to_vec");
    assert_eq!(target_symbol, "hew_hashset_to_vec_layout");
    assert_eq!(ret_ty, &vec.ty);
    assert_eq!(
        receiver.ty,
        ResolvedTy::Named {
            name: "HashSet".into(),
            args: vec![elem_ty],
            builtin: Some(hew_types::BuiltinType::HashSet),
            is_opaque: false,
        },
        "the projection receiver must retain the real iterable HashSet type"
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
    let HirExprKind::StructInit {
        name, fields, base, ..
    } = &init.kind
    else {
        panic!("HashMap field into_iter must build HashMapIter, got {init:#?}");
    };
    assert_eq!(name, "HashMapIter");
    assert!(base.is_none());
    let cursor_args = [ResolvedTy::I64, ResolvedTy::I64];
    let cursor_key =
        hew_hir::synthetic_cursor_layout_key(hew_types::BuiltinType::HashMapIter, &cursor_args)
            .expect("HashMapIter is a synthetic cursor");
    let cursor_layout = output
        .module
        .record_layouts
        .iter()
        .find(|layout| layout.mangled_name == cursor_key)
        .unwrap_or_else(|| {
            panic!(
                "typed HashMapIter catalog entry `{cursor_key}` was not published: {:#?}",
                output.module.record_layouts
            )
        });
    assert_eq!(
        cursor_layout
            .fields
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>(),
        ["ks", "vs", "idx"],
        "HashMap cursor layout must come from the shared typed catalog"
    );
    assert_eq!(
        fields
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>(),
        ["ks", "vs", "idx"]
    );
    for (field_name, projection) in fields.iter().take(2) {
        let expected_method = match field_name.as_str() {
            "ks" => "keys",
            "vs" => "values",
            _ => unreachable!("the first two HashMapIter fields are ks/vs"),
        };
        let HirExprKind::ResolvedImplCall {
            receiver,
            method_name,
            ret_ty,
            ..
        } = &projection.kind
        else {
            panic!("HashMapIter.{field_name} must be a resolved projection: {projection:#?}");
        };
        assert_eq!(method_name, expected_method);
        assert_eq!(ret_ty, &projection.ty);
        assert!(matches!(
            receiver.ty,
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::HashMap),
                ..
            }
        ));
        assert!(
            matches!(
                receiver.kind,
                HirExprKind::FieldAccess { ref field, .. } if field == "m"
            ),
            "HashMap projection receiver must remain the original field place: {receiver:#?}"
        );
    }
}

#[test]
fn hashset_for_in_preserves_receiver_and_projection_types_across_place_shapes() {
    let output = lower(
        r"
type SetBox { s: HashSet<i64>, }
type Outer { inner: SetBox, }
type OwnedBox { s: HashSet<string>, }
type MapBox { m: HashMap<i64, i64>, }

fn direct(s: HashSet<i64>) {
    for x in s { let _ = x; }
}

fn field(b: SetBox) {
    for x in b.s { let _ = x; }
}

fn nested(o: Outer) {
    for x in o.inner.s { let _ = x; }
}

fn tuple_field(pair: (HashSet<i64>, i64)) {
    for x in pair.0 { let _ = x; }
}

fn owned_field(b: OwnedBox) {
    for x in b.s { let _ = x.len(); }
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

    assert_hashset_vec_iter(
        &output,
        "direct",
        ResolvedTy::I64,
        |kind| matches!(kind, HirExprKind::BindingRef { name, .. } if name == "s"),
    );
    assert_hashset_vec_iter(
        &output,
        "field",
        ResolvedTy::I64,
        |kind| matches!(kind, HirExprKind::FieldAccess { field, .. } if field == "s"),
    );
    assert_hashset_vec_iter(&output, "nested", ResolvedTy::I64, |kind| {
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
    assert_hashset_vec_iter(&output, "tuple_field", ResolvedTy::I64, |kind| {
        matches!(kind, HirExprKind::TupleIndex { index: 0, .. })
    });
    assert_hashset_vec_iter(
        &output,
        "owned_field",
        ResolvedTy::String,
        |kind| matches!(kind, HirExprKind::FieldAccess { field, .. } if field == "s"),
    );
    assert_hashmap_into_iter_field(&output);
}
