//! Vec index element-type gates.

use super::*;

// ── FC-P1-E: Vec<T> index/slice element-type gates ──────────────────────────
//
// MIR `lower_vec_index` (sites :7265) and `lower_vec_slice` (site :7384)
// already reject unsupported element types via `MirDiagnosticKind::NotYetImplemented`,
// but the diagnostic surfaces deep in MIR and codegen has historically been
// liable to drop downstream expressions when a sub-expression fails to lower.
// Fail-closed per slepp A222 ("compile-time diagnostic, not runtime surprise")
// and A228 ("no silent no-op stubs"): catch the unsupported case at the HIR
// boundary so the user sees a clean diagnostic before any lowering work runs.
//
// Walker shape mirrors `check_wasm_blocking_recv_gate` (above), with the
// difference that we walk `Spanned<Expr>` instead of bare `Expr` because the
// gate needs the object's span (to query the checker's `expr_types`
// side-table) and the surrounding `Expr::Index` span (for the diagnostic
// location).
//
// The gate is target-independent: the runtime ABI lacks
// `hew_vec_get_T` / `hew_vec_slice_range_T` for the rejected element types
// on every target.

pub(super) fn check_vec_index_element_type_gates(ctx: &mut LowerCtx, program: &Program) {
    // Field-split borrow: `expr_types` is read-only; `diagnostics` is appended.
    let expr_types = &ctx.expr_types;
    let diagnostics = &mut ctx.diagnostics;
    for (item, _span) in &program.items {
        scan_item_for_vec_index_gate(item, expr_types, diagnostics);
    }
}

pub(super) fn scan_item_for_vec_index_gate(
    item: &Item,
    expr_types: &HashMap<SpanKey, Ty>,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    match item {
        Item::Function(fn_decl) => {
            scan_block_for_vec_index_gate(&fn_decl.body, expr_types, diagnostics);
        }
        Item::Actor(actor_decl) => {
            if let Some(init) = &actor_decl.init {
                scan_block_for_vec_index_gate(&init.body, expr_types, diagnostics);
            }
            for recv_fn in &actor_decl.receive_fns {
                scan_block_for_vec_index_gate(&recv_fn.body, expr_types, diagnostics);
            }
            for method in &actor_decl.methods {
                scan_block_for_vec_index_gate(&method.body, expr_types, diagnostics);
            }
        }
        Item::Impl(impl_decl) => {
            for method in &impl_decl.methods {
                scan_block_for_vec_index_gate(&method.body, expr_types, diagnostics);
            }
        }
        // Machine bodies never reach here: normalization rewrites a machine
        // into ordinary declarations before checking, and a machine it refuses
        // fails type check before HIR. The expanded bodies are walked through
        // `Item::Impl` like any other method.
        // Const, Trait, Supervisor, Struct, Enum, Use, Module, etc. do not
        // carry user expression bodies that can contain `Expr::Index`.
        _ => {}
    }
}

pub(super) fn scan_block_for_vec_index_gate(
    block: &Block,
    expr_types: &HashMap<SpanKey, Ty>,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    for (stmt, _) in &block.stmts {
        scan_stmt_for_vec_index_gate(stmt, expr_types, diagnostics);
    }
    if let Some(trailing) = &block.trailing_expr {
        scan_expr_for_vec_index_gate(trailing, expr_types, diagnostics);
    }
}

#[allow(
    clippy::match_same_arms,
    reason = "explicit per-Stmt-variant arms mirror scan_stmt_for_blocking_recv for review symmetry"
)]
pub(super) fn scan_stmt_for_vec_index_gate(
    stmt: &Stmt,
    expr_types: &HashMap<SpanKey, Ty>,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    match stmt {
        Stmt::Let { value: Some(v), .. } | Stmt::Var { value: Some(v), .. } => {
            scan_expr_for_vec_index_gate(v, expr_types, diagnostics);
        }
        Stmt::Assign { target, value, .. } => {
            scan_expr_for_vec_index_gate(target, expr_types, diagnostics);
            scan_expr_for_vec_index_gate(value, expr_types, diagnostics);
        }
        Stmt::Expression(e) => {
            scan_expr_for_vec_index_gate(e, expr_types, diagnostics);
        }
        Stmt::Return(Some(e)) => {
            scan_expr_for_vec_index_gate(e, expr_types, diagnostics);
        }
        Stmt::Defer(e) => {
            scan_expr_for_vec_index_gate(e, expr_types, diagnostics);
        }
        Stmt::Break { value: Some(v), .. } => {
            scan_expr_for_vec_index_gate(v, expr_types, diagnostics);
        }
        Stmt::WhileLet {
            conditions, body, ..
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_vec_index_gate(expr, expr_types, diagnostics);
            }
            scan_block_for_vec_index_gate(body, expr_types, diagnostics);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            scan_expr_for_vec_index_gate(condition, expr_types, diagnostics);
            scan_block_for_vec_index_gate(then_block, expr_types, diagnostics);
            if let Some(eb) = else_block {
                scan_else_block_for_vec_index_gate(eb, expr_types, diagnostics);
            }
        }
        Stmt::IfLet {
            conditions,
            body,
            else_body,
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_vec_index_gate(expr, expr_types, diagnostics);
            }
            scan_block_for_vec_index_gate(body, expr_types, diagnostics);
            if let Some(eb) = else_body {
                scan_expr_for_vec_index_gate(eb, expr_types, diagnostics);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            scan_expr_for_vec_index_gate(scrutinee, expr_types, diagnostics);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    scan_expr_for_vec_index_gate(g, expr_types, diagnostics);
                }
                scan_expr_for_vec_index_gate(&arm.body, expr_types, diagnostics);
            }
        }
        Stmt::Loop { body, .. } => {
            scan_block_for_vec_index_gate(body, expr_types, diagnostics);
        }
        Stmt::For { iterable, body, .. } => {
            scan_expr_for_vec_index_gate(iterable, expr_types, diagnostics);
            scan_block_for_vec_index_gate(body, expr_types, diagnostics);
        }
        Stmt::While {
            condition, body, ..
        } => {
            scan_expr_for_vec_index_gate(condition, expr_types, diagnostics);
            scan_block_for_vec_index_gate(body, expr_types, diagnostics);
        }
        // Stmt::Break (no value), Stmt::Continue, Stmt::Return(None), and
        // any other leaf statements carry no sub-expression to scan.
        _ => {}
    }
}

pub(super) fn scan_else_block_for_vec_index_gate(
    eb: &hew_parser::ast::ElseBlock,
    expr_types: &HashMap<SpanKey, Ty>,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    if let Some(stmt) = &eb.if_stmt {
        scan_stmt_for_vec_index_gate(&stmt.0, expr_types, diagnostics);
    }
    if let Some(b) = &eb.block {
        scan_block_for_vec_index_gate(b, expr_types, diagnostics);
    }
}

/// Recursively walk a `Spanned<Expr>` tree, gating `Vec<T>` `Expr::Index`
/// nodes whose element type `T` is not in the runtime ABI's supported set.
#[allow(
    clippy::too_many_lines,
    reason = "exhaustive Expr-variant walker mirrors scan_expr_for_blocking_recv above"
)]
pub(super) fn scan_expr_for_vec_index_gate(
    expr: &Spanned<Expr>,
    expr_types: &HashMap<SpanKey, Ty>,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    let (kind, span) = expr;
    match kind {
        Expr::Index { object, index } => {
            check_vec_index_element_type(object, index, span, expr_types, diagnostics);
            // Recurse into sub-expressions: an `xs[ys[i]]` form must gate
            // `ys[i]` even if `xs[..]` is supported, and vice versa.
            scan_expr_for_vec_index_gate(object, expr_types, diagnostics);
            scan_expr_for_vec_index_gate(index, expr_types, diagnostics);
        }
        Expr::MethodCall { receiver, args, .. } => {
            scan_expr_for_vec_index_gate(receiver, expr_types, diagnostics);
            for arg in args {
                scan_expr_for_vec_index_gate(arg.expr(), expr_types, diagnostics);
            }
        }
        Expr::Call { function, args, .. } => {
            scan_expr_for_vec_index_gate(function, expr_types, diagnostics);
            for arg in args {
                scan_expr_for_vec_index_gate(arg.expr(), expr_types, diagnostics);
            }
        }
        Expr::Binary { left, right, .. }
        | Expr::Coalesce { left, right }
        | Expr::Handle {
            operand: left,
            body: right,
            ..
        } => {
            scan_expr_for_vec_index_gate(left, expr_types, diagnostics);
            scan_expr_for_vec_index_gate(right, expr_types, diagnostics);
        }
        Expr::Unary { operand, .. } => {
            scan_expr_for_vec_index_gate(operand, expr_types, diagnostics);
        }
        Expr::Tuple(es) | Expr::Race(es) => {
            for e in es {
                scan_expr_for_vec_index_gate(e, expr_types, diagnostics);
            }
        }
        Expr::Array(elements) => {
            for element in elements {
                scan_expr_for_vec_index_gate(element.expr(), expr_types, diagnostics);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            scan_expr_for_vec_index_gate(value, expr_types, diagnostics);
            scan_expr_for_vec_index_gate(count, expr_types, diagnostics);
        }
        Expr::Block(b)
        | Expr::Scope { body: b }
        | Expr::ForkBlock { body: b }
        | Expr::GenBlock { body: b } => {
            scan_block_for_vec_index_gate(b, expr_types, diagnostics);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            scan_expr_for_vec_index_gate(condition, expr_types, diagnostics);
            scan_expr_for_vec_index_gate(then_block, expr_types, diagnostics);
            if let Some(e) = else_block {
                scan_expr_for_vec_index_gate(e, expr_types, diagnostics);
            }
        }
        Expr::IfLet {
            conditions,
            body,
            else_body,
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_vec_index_gate(expr, expr_types, diagnostics);
            }
            scan_block_for_vec_index_gate(body, expr_types, diagnostics);
            if let Some(b) = else_body {
                scan_expr_for_vec_index_gate(b, expr_types, diagnostics);
            }
        }
        Expr::Match { scrutinee, arms } => {
            scan_expr_for_vec_index_gate(scrutinee, expr_types, diagnostics);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    scan_expr_for_vec_index_gate(g, expr_types, diagnostics);
                }
                scan_expr_for_vec_index_gate(&arm.body, expr_types, diagnostics);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            scan_expr_for_vec_index_gate(body, expr_types, diagnostics);
        }
        Expr::Spawn { target, args, .. } => {
            scan_expr_for_vec_index_gate(target, expr_types, diagnostics);
            for (_, v) in args {
                scan_expr_for_vec_index_gate(v, expr_types, diagnostics);
            }
        }
        Expr::ScopeDeadline { duration, body } => {
            scan_expr_for_vec_index_gate(duration, expr_types, diagnostics);
            scan_block_for_vec_index_gate(body, expr_types, diagnostics);
        }
        Expr::ForkChild { expr: inner, .. } | Expr::Cast { expr: inner, .. } => {
            scan_expr_for_vec_index_gate(inner, expr_types, diagnostics);
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, v) in fields {
                scan_expr_for_vec_index_gate(v, expr_types, diagnostics);
            }
            if let Some(b) = base {
                scan_expr_for_vec_index_gate(b, expr_types, diagnostics);
            }
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                scan_expr_for_vec_index_gate(k, expr_types, diagnostics);
                scan_expr_for_vec_index_gate(v, expr_types, diagnostics);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let hew_parser::ast::StringPart::Expr(e)
                | hew_parser::ast::StringPart::StructuralExpr(e) = part
                {
                    scan_expr_for_vec_index_gate(e, expr_types, diagnostics);
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                scan_expr_for_vec_index_gate(&arm.source, expr_types, diagnostics);
                scan_expr_for_vec_index_gate(&arm.body, expr_types, diagnostics);
            }
            if let Some(t) = timeout {
                scan_expr_for_vec_index_gate(&t.duration, expr_types, diagnostics);
                scan_expr_for_vec_index_gate(&t.body, expr_types, diagnostics);
            }
        }
        Expr::UnsafeBlock(b) => scan_block_for_vec_index_gate(b, expr_types, diagnostics),
        Expr::FieldAccess { object, .. } | Expr::PostfixTry(object) | Expr::Await(object) => {
            scan_expr_for_vec_index_gate(object, expr_types, diagnostics);
        }
        Expr::Is { lhs, rhs } => {
            scan_expr_for_vec_index_gate(lhs, expr_types, diagnostics);
            scan_expr_for_vec_index_gate(rhs, expr_types, diagnostics);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                scan_expr_for_vec_index_gate(s, expr_types, diagnostics);
            }
            if let Some(e) = end {
                scan_expr_for_vec_index_gate(e, expr_types, diagnostics);
            }
        }
        Expr::Yield(Some(e)) => scan_expr_for_vec_index_gate(e, expr_types, diagnostics),
        Expr::MachineEmit { fields, .. } => {
            for (_, v) in fields {
                scan_expr_for_vec_index_gate(v, expr_types, diagnostics);
            }
        }
        // Leaf nodes (Identifier, literals, etc.) and any other Expr variant
        // without sub-expressions: nothing to scan.
        _ => {}
    }
}

/// Render a `ResolvedTy` for the user-facing diagnostic. Keep the textual
/// form compact (`bool`, `char`, `Vec<i32>`, `Foo`) so the diagnostic reads
/// Map a scalar `ResolvedTy` to its `to_string_*` catalog builtin for Display
/// dispatch. Only the scalar arm of `lower_display_dispatch` reaches here; any
/// non-scalar type is a caller bug (the dispatch match never routes it here).
pub(super) fn scalar_display_builtin(ty: &ResolvedTy) -> (&'static str, ResolvedTy) {
    match ty {
        ResolvedTy::I8 | ResolvedTy::I16 | ResolvedTy::I32 => ("to_string_i32", ResolvedTy::I32),
        ResolvedTy::I64 | ResolvedTy::Isize => ("to_string_i64", ResolvedTy::I64),
        ResolvedTy::U8 => ("to_string_u8", ResolvedTy::U8),
        ResolvedTy::U16 | ResolvedTy::U32 => ("to_string_u32", ResolvedTy::U32),
        ResolvedTy::U64 | ResolvedTy::Usize => ("to_string_u64", ResolvedTy::U64),
        ResolvedTy::F32 | ResolvedTy::F64 => ("to_string_f64", ResolvedTy::F64),
        ResolvedTy::Bool => ("to_string_bool", ResolvedTy::Bool),
        ResolvedTy::Char => ("to_string_char", ResolvedTy::Char),
        _ => unreachable!("scalar_display_builtin called on non-scalar type {ty:?}"),
    }
}

/// naturally; do not use `{:?}` because that leaks variant punctuation
/// (`I32`, `Named { name: "Foo", args: [] }`, etc.).
pub(super) fn render_elem_ty(ty: &ResolvedTy) -> String {
    match ty {
        ResolvedTy::Bool => "bool".to_string(),
        ResolvedTy::Char => "char".to_string(),
        ResolvedTy::I8 => "i8".to_string(),
        ResolvedTy::U8 => "u8".to_string(),
        ResolvedTy::I16 => "i16".to_string(),
        ResolvedTy::U16 => "u16".to_string(),
        ResolvedTy::I32 => "i32".to_string(),
        ResolvedTy::U32 => "u32".to_string(),
        ResolvedTy::I64 => "i64".to_string(),
        ResolvedTy::U64 => "u64".to_string(),
        ResolvedTy::Isize => "isize".to_string(),
        ResolvedTy::Usize => "usize".to_string(),
        ResolvedTy::F32 => "f32".to_string(),
        ResolvedTy::F64 => "f64".to_string(),
        ResolvedTy::String => "string".to_string(),
        ResolvedTy::Bytes => "bytes".to_string(),
        ResolvedTy::Duration => "duration".to_string(),
        ResolvedTy::Unit => "()".to_string(),
        ResolvedTy::Function { params, ret, .. } => {
            let params = params
                .iter()
                .map(render_elem_ty)
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({params}) -> {}", render_elem_ty(ret))
        }
        ResolvedTy::Closure { params, ret, .. } => {
            let params = params
                .iter()
                .map(render_elem_ty)
                .collect::<Vec<_>>()
                .join(", ");
            format!("closure({params}) -> {}", render_elem_ty(ret))
        }
        ResolvedTy::TypeParam { name } => name.clone(),
        ResolvedTy::Named { name, args, .. } if args.is_empty() => name.clone(),
        ResolvedTy::Named { name, args, .. } => {
            let arg_list = args
                .iter()
                .map(render_elem_ty)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{name}<{arg_list}>")
        }
        ResolvedTy::TraitObject { traits } => {
            let bounds = traits
                .iter()
                .map(|bound| bound.trait_name.clone())
                .collect::<Vec<_>>()
                .join(" + ");
            format!("dyn {bounds}")
        }
        other => format!("{other:?}"),
    }
}

pub(super) fn check_vec_index_element_type(
    object: &Spanned<Expr>,
    index: &Spanned<Expr>,
    index_expr_span: &Span,
    expr_types: &HashMap<SpanKey, Ty>,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    // Look up the container type at the object's span. If absent, the
    // checker already emitted an error for this expression (e.g. indexing
    // into a non-Vec) — defer to that diagnostic and skip the gate.
    // This gate scans root-program AST items only (module_idx = 0).
    let Some(container_ty) = expr_types.get(&SpanKey::in_module(&object.1, 0)) else {
        return;
    };
    let Ok(resolved) = ResolvedTy::from_ty(container_ty) else {
        // Boundary violation upstream; do not double-report.
        return;
    };
    let elem_ty = match &resolved {
        ResolvedTy::Named {
            args,
            builtin: Some(BuiltinType::Vec),
            ..
        } if !args.is_empty() => args[0].clone(),
        // Not a Vec<T>: this might be a user-defined type with an `at` method
        // (the HIR lower handles that path) or an array literal (separate
        // lowering). The gate only governs Vec<T> runtime ABI dispatch.
        _ => return,
    };

    let is_range_slice = matches!(&index.0, Expr::Range { .. });

    // Allowlists track the runtime ABI dispatch arms in
    // `hew-mir/src/lower.rs::lower_vec_index` (scalar) and
    // `lower_vec_slice` (range). Keep these in sync with any new
    // `hew_vec_get_T` / `hew_vec_slice_range_T` symbol added to the
    // runtime.
    let is_supported = if is_range_slice {
        matches!(
            elem_ty,
            ResolvedTy::Bool
                | ResolvedTy::Char
                | ResolvedTy::I8
                | ResolvedTy::U8
                | ResolvedTy::I16
                | ResolvedTy::U16
                | ResolvedTy::I32
                | ResolvedTy::U32
                | ResolvedTy::I64
                | ResolvedTy::U64
                | ResolvedTy::Isize
                | ResolvedTy::Usize
                // `duration` is an i64-class element (same `hew_vec_*_i64`
                // family); `instant` reaches here as the canonical `I64`.
                | ResolvedTy::Duration
                | ResolvedTy::F32
                | ResolvedTy::F64
                | ResolvedTy::String
                | ResolvedTy::Bytes
                | ResolvedTy::Named { .. }
                | ResolvedTy::Tuple(_)
                | ResolvedTy::Array(_, _)
                | ResolvedTy::TypeParam { .. }
        )
    } else {
        matches!(
            elem_ty,
            ResolvedTy::Bool
                | ResolvedTy::Char
                | ResolvedTy::I8
                | ResolvedTy::U8
                | ResolvedTy::I16
                | ResolvedTy::U16
                | ResolvedTy::I32
                | ResolvedTy::U32
                | ResolvedTy::I64
                | ResolvedTy::U64
                | ResolvedTy::Isize
                | ResolvedTy::Usize
                // `duration` is an i64-class element (same `hew_vec_*_i64`
                // family); `instant` reaches here as the canonical `I64`.
                | ResolvedTy::Duration
                | ResolvedTy::F32
                | ResolvedTy::F64
                | ResolvedTy::String
                | ResolvedTy::Bytes
                | ResolvedTy::Named { .. }
                | ResolvedTy::Tuple(_)
                | ResolvedTy::Array(_, _)
                | ResolvedTy::TypeParam { .. }
                // Closure-pair elements: `xs[i]` loads the pair out of the
                // slot's heap box (`hew_vec_get_ptr` + codegen unbox). The
                // range-slice form stays unsupported — a sliced vec would
                // shallow-share the element boxes with the source.
                | ResolvedTy::Function { .. }
                | ResolvedTy::Closure { .. }
        )
    };

    if is_supported {
        return;
    }

    let rendered = render_elem_ty(&elem_ty);
    let is_trait_object = matches!(elem_ty, ResolvedTy::TraitObject { .. });
    let note = vec_index_unsupported_note(&rendered, is_range_slice, is_trait_object);
    let kind = if is_range_slice {
        HirDiagnosticKind::VecSliceElementTypeUnsupported {
            element_ty: rendered,
        }
    } else {
        HirDiagnosticKind::VecIndexElementTypeUnsupported {
            element_ty: rendered,
        }
    };
    diagnostics.push(HirDiagnostic::new(kind, index_expr_span.clone(), note));
}

/// Diagnostic note for an unsupported `Vec<T>` scalar-index or range-slice
/// element type. Trait-object elements get a dedicated message naming the
/// supported alternative (`into_iter()` / pop / remove), matching the
/// quality of the reachable checker-level trait-object refusals
/// (`hew-types/src/check/methods.rs`); every other unsupported element type
/// gets the generic supported-set listing.
pub(super) fn vec_index_unsupported_note(
    rendered: &str,
    is_range_slice: bool,
    is_trait_object: bool,
) -> String {
    match (is_range_slice, is_trait_object) {
        (true, true) => format!(
            "Vec<{rendered}> range-slice (xs[a..b]) is not supported: a sliced Vec would \
             share each `HeapBoxed` trait-object owner with the source, and `{rendered}` \
             has no semantic clone operation to give the slice an independent copy; \
             consume the Vec with `into_iter()` instead."
        ),
        (true, false) => format!(
            "Vec<{rendered}> range-slice (xs[a..b]) is not yet supported. \
             Supported element types for Vec range-slicing are: \
             bool, char, i8, u8, i16, u16, i32, u32, i64, u64, isize, \
             usize, f32, f64, string, bytes, tuples, user-defined types, and \
             type-parameter elements."
        ),
        (false, true) => format!(
            "Vec<{rendered}> scalar index (xs[i]) is not supported: returning or \
             overwriting an owned element would require a semantic trait-object clone, \
             and `{rendered}` has none; use pop/remove or consuming iteration \
             (`into_iter()`) to move an owner out instead."
        ),
        (false, false) => format!(
            "Vec<{rendered}> scalar index (xs[i]) is not yet supported. \
             Supported element types for Vec scalar indexing are: \
             bool, char, i8, u8, i16, u16, i32, u32, i64, u64, isize, \
             usize, f32, f64, string, bytes, tuples, type-parameter elements, \
             and user-defined types (records, enums, \
             Duplex, etc.)."
        ),
    }
}
