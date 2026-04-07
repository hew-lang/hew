#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

pub(super) fn ty_has_unresolved_inference_var(ty: &Ty) -> bool {
    match ty {
        Ty::Var(_) => true,
        Ty::Tuple(elems) => elems.iter().any(ty_has_unresolved_inference_var),
        Ty::Array(elem, _) | Ty::Slice(elem) => ty_has_unresolved_inference_var(elem),
        Ty::Named { args, .. } => args.iter().any(ty_has_unresolved_inference_var),
        Ty::Function { params, ret } => {
            params.iter().any(ty_has_unresolved_inference_var)
                || ty_has_unresolved_inference_var(ret)
        }
        Ty::Closure {
            params,
            ret,
            captures,
        } => {
            params.iter().any(ty_has_unresolved_inference_var)
                || ty_has_unresolved_inference_var(ret)
                || captures.iter().any(ty_has_unresolved_inference_var)
        }
        Ty::Pointer { pointee, .. } => ty_has_unresolved_inference_var(pointee),
        Ty::TraitObject { traits } => traits
            .iter()
            .any(|bound| bound.args.iter().any(ty_has_unresolved_inference_var)),
        _ => false,
    }
}

pub(super) fn collect_unresolved_inference_vars(ty: &Ty, vars: &mut HashSet<TypeVar>) {
    match ty {
        Ty::Var(var) => {
            vars.insert(*var);
        }
        Ty::Tuple(elems) => {
            for elem in elems {
                collect_unresolved_inference_vars(elem, vars);
            }
        }
        Ty::Array(elem, _) | Ty::Slice(elem) | Ty::Pointer { pointee: elem, .. } => {
            collect_unresolved_inference_vars(elem, vars);
        }
        Ty::Named { args, .. } => {
            for arg in args {
                collect_unresolved_inference_vars(arg, vars);
            }
        }
        Ty::Function { params, ret } => {
            for param in params {
                collect_unresolved_inference_vars(param, vars);
            }
            collect_unresolved_inference_vars(ret, vars);
        }
        Ty::Closure {
            params,
            ret,
            captures,
        } => {
            for param in params {
                collect_unresolved_inference_vars(param, vars);
            }
            collect_unresolved_inference_vars(ret, vars);
            for capture in captures {
                collect_unresolved_inference_vars(capture, vars);
            }
        }
        Ty::TraitObject { traits } => {
            for bound in traits {
                for arg in &bound.args {
                    collect_unresolved_inference_vars(arg, vars);
                }
            }
        }
        _ => {}
    }
}

pub(super) fn lookup_scoped_item<'a, T>(
    items: &'a HashMap<String, T>,
    module_name: Option<&str>,
    name: &str,
) -> Option<&'a T> {
    if let Some(module_name) = module_name {
        let qualified = format!("{module_name}.{name}");
        items.get(&qualified).or_else(|| items.get(name))
    } else {
        items.get(name)
    }
}

pub(super) fn scoped_module_item_name(module_name: Option<&str>, name: &str) -> Option<String> {
    if name.contains("::") || name.contains('.') {
        return None;
    }
    module_name.map(|module_name| format!("{module_name}.{name}"))
}

pub(super) fn first_infer_span_in_type_expr(type_expr: &Spanned<TypeExpr>) -> Option<Span> {
    match &type_expr.0 {
        TypeExpr::Infer => Some(type_expr.1.clone()),
        TypeExpr::Named { type_args, .. } => type_args
            .as_ref()
            .and_then(|args| args.iter().find_map(first_infer_span_in_type_expr)),
        TypeExpr::Result { ok, err } => {
            first_infer_span_in_type_expr(ok).or_else(|| first_infer_span_in_type_expr(err))
        }
        TypeExpr::Option(inner)
        | TypeExpr::Slice(inner)
        | TypeExpr::Array { element: inner, .. }
        | TypeExpr::Pointer { pointee: inner, .. } => first_infer_span_in_type_expr(inner),
        TypeExpr::Tuple(elems) => elems.iter().find_map(first_infer_span_in_type_expr),
        TypeExpr::Function {
            params,
            return_type,
        } => params
            .iter()
            .find_map(first_infer_span_in_type_expr)
            .or_else(|| first_infer_span_in_type_expr(return_type)),
        TypeExpr::TraitObject(bounds) => bounds.iter().find_map(|bound| {
            bound
                .type_args
                .as_ref()
                .and_then(|args| args.iter().find_map(first_infer_span_in_type_expr))
        }),
    }
}

pub(super) fn first_infer_span_in_extern_fn(function: &ExternFnDecl) -> Option<Span> {
    function
        .params
        .iter()
        .find_map(|param| first_infer_span_in_type_expr(&param.ty))
        .or_else(|| {
            function
                .return_type
                .as_ref()
                .and_then(first_infer_span_in_type_expr)
        })
}

pub(super) fn integer_type_info(ty: &Ty) -> Option<IntegerTypeInfo> {
    match ty {
        Ty::I8 => Some(IntegerTypeInfo {
            width: 8,
            signed: true,
        }),
        Ty::I16 => Some(IntegerTypeInfo {
            width: 16,
            signed: true,
        }),
        Ty::I32 => Some(IntegerTypeInfo {
            width: 32,
            signed: true,
        }),
        Ty::I64 => Some(IntegerTypeInfo {
            width: 64,
            signed: true,
        }),
        Ty::U8 => Some(IntegerTypeInfo {
            width: 8,
            signed: false,
        }),
        Ty::U16 => Some(IntegerTypeInfo {
            width: 16,
            signed: false,
        }),
        Ty::U32 => Some(IntegerTypeInfo {
            width: 32,
            signed: false,
        }),
        Ty::U64 => Some(IntegerTypeInfo {
            width: 64,
            signed: false,
        }),
        _ => None,
    }
}

/// Check if an expression is an integer literal (including negated integer literals).
pub(super) fn is_integer_literal(expr: &Expr) -> bool {
    match expr {
        Expr::Literal(Literal::Integer { .. }) => true,
        Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } => matches!(operand.0, Expr::Literal(Literal::Integer { .. })),
        _ => false,
    }
}

/// Check if an expression is a float literal (including negated float literals).
pub(super) fn is_float_literal(expr: &Expr) -> bool {
    match expr {
        Expr::Literal(Literal::Float(_)) => true,
        Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } => matches!(operand.0, Expr::Literal(Literal::Float(_))),
        _ => false,
    }
}

/// Extract the value from an integer literal expression (including negated).
pub(super) fn extract_integer_literal_value(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Literal(Literal::Integer { value, .. }) => Some(*value),
        Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } => {
            if let Expr::Literal(Literal::Integer { value, .. }) = &operand.0 {
                Some(-value)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Extract the value from a float literal expression (including negated).
pub(super) fn extract_float_literal_value(expr: &Expr) -> Option<f64> {
    match expr {
        Expr::Literal(Literal::Float(v)) => Some(*v),
        Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } => {
            if let Expr::Literal(Literal::Float(v)) = &operand.0 {
                Some(-v)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Check if an integer value fits in a specific integer type.
pub(super) fn integer_fits_type(value: i64, ty: &Ty) -> bool {
    match ty {
        Ty::I8 => i8::try_from(value).is_ok(),
        Ty::I16 => i16::try_from(value).is_ok(),
        Ty::I32 => i32::try_from(value).is_ok(),
        Ty::I64 => true,
        Ty::U8 => u8::try_from(value).is_ok(),
        Ty::U16 => u16::try_from(value).is_ok(),
        Ty::U32 => u32::try_from(value).is_ok(),
        Ty::U64 => u64::try_from(value).is_ok(),
        _ => false,
    }
}

/// Check if a float value fits in a specific float type (f32 range check).
pub(super) fn float_fits_type(value: f64, ty: &Ty) -> bool {
    match ty {
        Ty::F32 => {
            if value.is_infinite() || value.is_nan() {
                return true; // special values are representable
            }
            let abs = value.abs();
            abs == 0.0 || (abs >= f64::from(f32::MIN_POSITIVE) && abs <= f64::from(f32::MAX))
        }
        Ty::F64 => true,
        _ => false,
    }
}

/// Get the min/max range description for an integer type (for error messages).
pub(super) fn integer_type_range(ty: &Ty) -> Option<(i128, i128)> {
    match ty {
        Ty::I8 => Some((i128::from(i8::MIN), i128::from(i8::MAX))),
        Ty::I16 => Some((i128::from(i16::MIN), i128::from(i16::MAX))),
        Ty::I32 => Some((i128::from(i32::MIN), i128::from(i32::MAX))),
        Ty::I64 => Some((i128::from(i64::MIN), i128::from(i64::MAX))),
        Ty::U8 => Some((0, i128::from(u8::MAX))),
        Ty::U16 => Some((0, i128::from(u16::MAX))),
        Ty::U32 => Some((0, i128::from(u32::MAX))),
        Ty::U64 => Some((0, i128::from(u64::MAX))),
        _ => None,
    }
}

/// Check if a type directly or transitively contains `Rc<T>`, walking into
/// named struct/enum fields via `type_defs`.  `visiting` guards against cycles
/// in recursive type definitions.
pub(super) fn ty_contains_rc_deep(
    ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
        Ty::Named { name, args } if name == "Rc" => !args.is_empty(),
        Ty::Named { name, args } => {
            // First check explicit type arguments (handles Option<Rc<T>>, etc.)
            if args
                .iter()
                .any(|a| ty_contains_rc_deep(a, type_defs, visiting))
            {
                return true;
            }
            // Cycle guard: if we're already walking this type stop here.
            if visiting.contains(name.as_str()) {
                return false;
            }
            // Walk into struct fields / enum variant payloads.
            // Handle module-qualified names like "json.Value" by stripping prefix.
            let td = type_defs
                .get(name.as_str())
                .or_else(|| name.rsplit_once('.').and_then(|(_, u)| type_defs.get(u)));
            let Some(td) = td else {
                return false;
            };
            visiting.insert(name.clone());
            let result = td
                .fields
                .values()
                .any(|f| ty_contains_rc_deep(f, type_defs, visiting))
                || td.variants.values().any(|v| match v {
                    VariantDef::Unit => false,
                    VariantDef::Tuple(tys) => tys
                        .iter()
                        .any(|t| ty_contains_rc_deep(t, type_defs, visiting)),
                    VariantDef::Struct(fields) => fields
                        .iter()
                        .any(|(_, t)| ty_contains_rc_deep(t, type_defs, visiting)),
                });
            visiting.remove(name.as_str());
            result
        }
        Ty::Tuple(elems) => elems
            .iter()
            .any(|e| ty_contains_rc_deep(e, type_defs, visiting)),
        Ty::Array(inner, _) | Ty::Slice(inner) => ty_contains_rc_deep(inner, type_defs, visiting),
        _ => false,
    }
}
