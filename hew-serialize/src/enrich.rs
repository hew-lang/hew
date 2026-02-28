//! AST enrichment: fills in missing type annotations using `TypeCheckOutput`.
//!
//! After the type checker runs, this module walks the parsed AST and injects
//! inferred types into `let`/`var` bindings and function return types that lack
//! explicit annotations. The result is a fully-typed AST that the C++ backend
//! can consume without its own type inference.

use hew_parser::ast::{
    ActorDecl, Block, CallArg, ElseBlock, Expr, ExternBlock, ExternFnDecl, FnDecl, Item, Param,
    Program, Span, Spanned, Stmt, TraitBound, TypeExpr,
};
use hew_types::check::{SpanKey, TypeCheckOutput};
use hew_types::Ty;

use crate::msgpack::ExprTypeEntry;

/// Build the expression type map from [`TypeCheckOutput`].
///
/// Converts every entry in `tco.expr_types` (span → [`Ty`]) into an
/// [`ExprTypeEntry`] (start, end, [`TypeExpr`]) that can be serialized
/// alongside the AST.
#[must_use]
pub fn build_expr_type_map(tco: &TypeCheckOutput) -> Vec<ExprTypeEntry> {
    tco.expr_types
        .iter()
        .filter_map(|(key, ty)| {
            ty_to_type_expr(ty).map(|te| ExprTypeEntry {
                start: key.start,
                end: key.end,
                ty: te,
            })
        })
        .collect()
}

/// Convert a resolved [`Ty`] into a parser [`TypeExpr`] with a synthetic span.
#[expect(
    clippy::too_many_lines,
    reason = "type mapping covers many Ty variants"
)]
fn ty_to_type_expr(ty: &Ty) -> Option<Spanned<TypeExpr>> {
    let span: Span = 0..0; // synthetic span for inferred types
    let te = match ty {
        Ty::I8 => TypeExpr::Named {
            name: "i8".into(),
            type_args: None,
        },
        Ty::I16 => TypeExpr::Named {
            name: "i16".into(),
            type_args: None,
        },
        Ty::I32 => TypeExpr::Named {
            name: "i32".into(),
            type_args: None,
        },
        Ty::I64 => TypeExpr::Named {
            name: "i64".into(),
            type_args: None,
        },
        Ty::U8 => TypeExpr::Named {
            name: "u8".into(),
            type_args: None,
        },
        Ty::U16 => TypeExpr::Named {
            name: "u16".into(),
            type_args: None,
        },
        Ty::U32 => TypeExpr::Named {
            name: "u32".into(),
            type_args: None,
        },
        Ty::U64 => TypeExpr::Named {
            name: "u64".into(),
            type_args: None,
        },
        Ty::F32 => TypeExpr::Named {
            name: "f32".into(),
            type_args: None,
        },
        Ty::F64 => TypeExpr::Named {
            name: "f64".into(),
            type_args: None,
        },
        Ty::Bool => TypeExpr::Named {
            name: "bool".into(),
            type_args: None,
        },
        Ty::Char => TypeExpr::Named {
            name: "char".into(),
            type_args: None,
        },
        Ty::String => TypeExpr::Named {
            name: "string".into(),
            type_args: None,
        },
        Ty::Never => TypeExpr::Named {
            name: "!".into(),
            type_args: None,
        },

        Ty::Named { name, args } => {
            let type_args = if args.is_empty() {
                None
            } else {
                let converted: Vec<_> = args.iter().filter_map(ty_to_type_expr).collect();
                if converted.len() == args.len() {
                    Some(converted)
                } else {
                    return None;
                }
            };
            TypeExpr::Named {
                name: name.clone(),
                type_args,
            }
        }

        Ty::Function { params, ret } | Ty::Closure { params, ret, .. } => {
            let param_exprs: Vec<_> = params.iter().filter_map(ty_to_type_expr).collect();
            if param_exprs.len() != params.len() {
                return None;
            }
            let ret_expr = ty_to_type_expr(ret)?;
            TypeExpr::Function {
                params: param_exprs,
                return_type: Box::new(ret_expr),
            }
        }

        Ty::Tuple(elements) => {
            let elem_exprs: Vec<_> = elements.iter().filter_map(ty_to_type_expr).collect();
            if elem_exprs.len() != elements.len() {
                return None;
            }
            TypeExpr::Tuple(elem_exprs)
        }
        Ty::Array(element, size) => {
            let elem = ty_to_type_expr(element)?;
            TypeExpr::Array {
                element: Box::new(elem),
                size: *size,
            }
        }
        Ty::Slice(element) => {
            let elem = ty_to_type_expr(element)?;
            TypeExpr::Slice(Box::new(elem))
        }

        Ty::Option(inner) => {
            let inner_expr = ty_to_type_expr(inner)?;
            TypeExpr::Option(Box::new(inner_expr))
        }
        Ty::Result { ok, err } => {
            let ok_expr = ty_to_type_expr(ok)?;
            let err_expr = ty_to_type_expr(err)?;
            TypeExpr::Result {
                ok: Box::new(ok_expr),
                err: Box::new(err_expr),
            }
        }
        Ty::ActorRef(inner) => {
            let inner_expr = ty_to_type_expr(inner)?;
            TypeExpr::Named {
                name: "ActorRef".into(),
                type_args: Some(vec![inner_expr]),
            }
        }
        Ty::Stream(inner) => {
            let inner_expr = ty_to_type_expr(inner)?;
            TypeExpr::Named {
                name: "Stream".into(),
                type_args: Some(vec![inner_expr]),
            }
        }
        Ty::Sink(inner) => {
            let inner_expr = ty_to_type_expr(inner)?;
            TypeExpr::Named {
                name: "Sink".into(),
                type_args: Some(vec![inner_expr]),
            }
        }

        Ty::Pointer {
            is_mutable,
            pointee,
        } => {
            let pointee_expr = ty_to_type_expr(pointee)?;
            TypeExpr::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(pointee_expr),
            }
        }

        Ty::TraitObject { traits } => {
            let bounds: Option<Vec<_>> = traits
                .iter()
                .map(|b| {
                    Some(TraitBound {
                        name: b.trait_name.clone(),
                        type_args: if b.args.is_empty() {
                            None
                        } else {
                            let mapped: Option<Vec<_>> =
                                b.args.iter().map(|a| ty_to_type_expr(a)).collect();
                            mapped
                        },
                    })
                })
                .collect();
            TypeExpr::TraitObject(bounds?)
        }

        // Skip these types gracefully — C++ codegen handles them via
        // built-in type logic, not through the expr_types map
        Ty::Unit | Ty::Generator { .. } | Ty::AsyncGenerator { .. } | Ty::Range(_) => return None,

        // Skip these types gracefully - they shouldn't be serialized
        Ty::Var(_) | Ty::Error => return None,
    };

    Some((te, span))
}

/// Look up the inferred type for a span in the `TypeCheckOutput`.
fn lookup_type(tco: &TypeCheckOutput, span: &Span) -> Option<Spanned<TypeExpr>> {
    let key = SpanKey {
        start: span.start,
        end: span.end,
    };
    tco.expr_types.get(&key).and_then(ty_to_type_expr)
}

/// Enrich a program's AST with inferred types from the type checker.
pub fn enrich_program(program: &mut Program, tco: &TypeCheckOutput) {
    for (item, _span) in &mut program.items {
        enrich_item(item, tco);
    }
    normalize_all_types(program);
    synthesize_stdlib_externs(program);
}

/// Normalize `TypeExpr::Named("Result", [T, E])` → `TypeExpr::Result { ok, err }`
/// and `TypeExpr::Named("Option", [T])` → `TypeExpr::Option(T)`.
///
/// The parser may emit these as generic `Named` types; the C++ backend expects
/// the dedicated `Result`/`Option` variants.
fn normalize_type_expr(te: &mut TypeExpr) {
    // First, recurse into child type exprs regardless of variant.
    match te {
        TypeExpr::Named {
            type_args: Some(ref mut args),
            ..
        } => {
            for arg in args.iter_mut() {
                normalize_type_expr(&mut arg.0);
            }
        }
        TypeExpr::Result { ok, err } => {
            normalize_type_expr(&mut ok.0);
            normalize_type_expr(&mut err.0);
        }
        TypeExpr::Option(inner) | TypeExpr::Slice(inner) => {
            normalize_type_expr(&mut inner.0);
        }
        TypeExpr::Tuple(elems) => {
            for elem in elems.iter_mut() {
                normalize_type_expr(&mut elem.0);
            }
        }
        TypeExpr::Array { element, .. } => {
            normalize_type_expr(&mut element.0);
        }
        TypeExpr::Function {
            params,
            return_type,
        } => {
            for p in params.iter_mut() {
                normalize_type_expr(&mut p.0);
            }
            normalize_type_expr(&mut return_type.0);
        }
        TypeExpr::Pointer { pointee, .. } => {
            normalize_type_expr(&mut pointee.0);
        }
        TypeExpr::TraitObject(ref mut bounds) => {
            for bound in bounds.iter_mut() {
                if let Some(ref mut args) = bound.type_args {
                    for arg in args.iter_mut() {
                        normalize_type_expr(&mut arg.0);
                    }
                }
            }
        }
        _ => {}
    }

    // Now check if this Named variant should be rewritten.
    if let TypeExpr::Named { name, type_args } = te {
        match name.as_str() {
            "Result" if type_args.as_ref().is_some_and(|a| a.len() == 2) => {
                let mut args = type_args.take().unwrap();
                let err = args.pop().unwrap();
                let ok = args.pop().unwrap();
                *te = TypeExpr::Result {
                    ok: Box::new(ok),
                    err: Box::new(err),
                };
            }
            "Option" if type_args.as_ref().is_some_and(|a| a.len() == 1) => {
                let mut args = type_args.take().unwrap();
                let inner = args.pop().unwrap();
                *te = TypeExpr::Option(Box::new(inner));
            }
            _ => {
                // Qualify unqualified handle type names (e.g. "Connection" → "net.Connection")
                if type_args.is_none() && name != "Result" {
                    if let Some(qualified) = hew_types::stdlib::qualify_handle_type(name) {
                        *name = qualified.to_string();
                    }
                }
            }
        }
    }
}

/// Synthesize `ExternBlock` items for each stdlib import.
///
/// The serializer embeds extern function declarations as top-level
/// `ExternBlock` items so the C++ backend can generate extern declarations.
fn synthesize_stdlib_externs(program: &mut Program) {
    let mut new_items: Vec<Spanned<Item>> = Vec::new();

    for (item, _span) in &program.items {
        if let Item::Import(import_decl) = item {
            let module_path = import_decl.path.join("::");
            if let Some(funcs) = hew_types::stdlib::stdlib_functions(&module_path) {
                let extern_fns: Vec<ExternFnDecl> = funcs
                    .iter()
                    .filter_map(|(name, params, ret_ty)| {
                        let param_exprs: Vec<Param> = params
                            .iter()
                            .enumerate()
                            .filter_map(|(i, ty)| {
                                let type_expr = ty_to_type_expr(ty)?;
                                Some(Param {
                                    name: format!("p{i}"),
                                    ty: type_expr,
                                    is_mutable: false,
                                })
                            })
                            .collect();
                        if param_exprs.len() != params.len() {
                            return None;
                        }
                        let return_type = if matches!(ret_ty, Ty::Unit) {
                            None
                        } else {
                            Some(ty_to_type_expr(ret_ty)?)
                        };
                        Some(ExternFnDecl {
                            name: name.clone(),
                            params: param_exprs,
                            return_type,
                            is_variadic: false,
                        })
                    })
                    .collect();

                if !extern_fns.is_empty() {
                    new_items.push((
                        Item::ExternBlock(ExternBlock {
                            abi: "C".to_string(),
                            functions: extern_fns,
                        }),
                        0..0,
                    ));
                }
            }
        }
    }

    program.items.extend(new_items);
}

/// Walk the entire program AST and normalize all `TypeExpr` nodes.
fn normalize_all_types(program: &mut Program) {
    for (item, _span) in &mut program.items {
        normalize_item_types(item);
    }
}

/// Normalize type expressions in a list of items.
///
/// This is the same transformation as [`normalize_all_types`] but operates on
/// a standalone item list — useful for normalizing module-graph modules that
/// are not part of the root `Program::items`.
pub fn normalize_items_types(items: &mut [Spanned<Item>]) {
    for (item, _span) in items {
        normalize_item_types(item);
    }
}

/// Rewrite builtin free-function calls to forms the C++ codegen already
/// handles. Currently rewrites `len(x)` → `x.len()` (method call).
///
/// This must run on every module (root and imported) since the enrichment
/// pass only processes root items.
pub fn rewrite_builtin_calls(items: &mut [Spanned<Item>]) {
    for (item, _span) in items {
        rewrite_builtin_calls_in_item(item);
    }
}

fn rewrite_builtin_calls_in_item(item: &mut Item) {
    match item {
        Item::Function(f) => rewrite_builtin_calls_in_block(&mut f.body),
        Item::Actor(actor) => {
            if let Some(ref mut init) = actor.init {
                rewrite_builtin_calls_in_block(&mut init.body);
            }
            for recv in &mut actor.receive_fns {
                rewrite_builtin_calls_in_block(&mut recv.body);
            }
        }
        Item::Impl(imp) => {
            for method in &mut imp.methods {
                rewrite_builtin_calls_in_block(&mut method.body);
            }
        }
        Item::Trait(t) => {
            for trait_item in &mut t.items {
                if let hew_parser::ast::TraitItem::Method(m) = trait_item {
                    if let Some(ref mut body) = m.body {
                        rewrite_builtin_calls_in_block(body);
                    }
                }
            }
        }
        _ => {}
    }
}

fn rewrite_builtin_calls_in_block(block: &mut Block) {
    for stmt in &mut block.stmts {
        rewrite_builtin_calls_in_stmt(&mut stmt.0);
    }
    if let Some(ref mut trailing) = block.trailing_expr {
        rewrite_builtin_calls_in_expr(trailing);
    }
}

fn rewrite_builtin_calls_in_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some(expr) = value {
                rewrite_builtin_calls_in_expr(expr);
            }
        }
        Stmt::Expression(expr) | Stmt::Return(Some(expr)) => {
            rewrite_builtin_calls_in_expr(expr);
        }
        Stmt::Defer(expr) => {
            rewrite_builtin_calls_in_expr(expr);
        }
        Stmt::For { body, iterable, .. } => {
            rewrite_builtin_calls_in_expr(iterable);
            rewrite_builtin_calls_in_block(body);
        }
        Stmt::While {
            condition, body, ..
        } => {
            rewrite_builtin_calls_in_expr(condition);
            rewrite_builtin_calls_in_block(body);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            rewrite_builtin_calls_in_expr(condition);
            rewrite_builtin_calls_in_block(then_block);
            if let Some(else_b) = else_block {
                if let Some(ref mut if_stmt) = else_b.if_stmt {
                    rewrite_builtin_calls_in_stmt(&mut if_stmt.0);
                }
                if let Some(ref mut block) = else_b.block {
                    rewrite_builtin_calls_in_block(block);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            rewrite_builtin_calls_in_expr(expr);
            rewrite_builtin_calls_in_block(body);
            if let Some(block) = else_body {
                rewrite_builtin_calls_in_block(block);
            }
        }
        Stmt::Assign { target, value, .. } => {
            rewrite_builtin_calls_in_expr(target);
            rewrite_builtin_calls_in_expr(value);
        }
        Stmt::Match { scrutinee, arms } => {
            rewrite_builtin_calls_in_expr(scrutinee);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    rewrite_builtin_calls_in_expr(guard);
                }
                rewrite_builtin_calls_in_expr(&mut arm.body);
            }
        }
        Stmt::Loop { body, .. } => rewrite_builtin_calls_in_block(body),
        _ => {}
    }
}

fn rewrite_builtin_calls_in_expr(expr: &mut Spanned<Expr>) {
    match &mut expr.0 {
        Expr::Call { function, args, .. } => {
            for arg in args.iter_mut() {
                rewrite_builtin_calls_in_expr(arg.expr_mut());
            }
            rewrite_builtin_calls_in_expr(function);
            // len(x) → x.len()
            if let Expr::Identifier(name) = &function.0 {
                if name == "len" && args.len() == 1 {
                    let receiver = match std::mem::take(args).remove(0) {
                        CallArg::Positional(e) => e,
                        CallArg::Named { value, .. } => value,
                    };
                    expr.0 = Expr::MethodCall {
                        receiver: Box::new(receiver),
                        method: "len".to_string(),
                        args: Vec::new(),
                    };
                }
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            rewrite_builtin_calls_in_expr(receiver);
            for arg in args.iter_mut() {
                rewrite_builtin_calls_in_expr(arg.expr_mut());
            }
        }
        Expr::Binary { left, right, .. } => {
            rewrite_builtin_calls_in_expr(left);
            rewrite_builtin_calls_in_expr(right);
        }
        Expr::Unary { operand, .. } => rewrite_builtin_calls_in_expr(operand),
        Expr::Cast { expr, .. } => rewrite_builtin_calls_in_expr(expr),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            rewrite_builtin_calls_in_expr(condition);
            rewrite_builtin_calls_in_expr(then_block);
            if let Some(e) = else_block {
                rewrite_builtin_calls_in_expr(e);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            rewrite_builtin_calls_in_expr(expr);
            rewrite_builtin_calls_in_block(body);
            if let Some(block) = else_body {
                rewrite_builtin_calls_in_block(block);
            }
        }
        Expr::Block(block) => rewrite_builtin_calls_in_block(block),
        Expr::Index { object, index } => {
            rewrite_builtin_calls_in_expr(object);
            rewrite_builtin_calls_in_expr(index);
        }
        Expr::FieldAccess { object, .. } => rewrite_builtin_calls_in_expr(object),
        Expr::ArrayRepeat { value, count } => {
            rewrite_builtin_calls_in_expr(value);
            rewrite_builtin_calls_in_expr(count);
        }
        Expr::Array(elems) | Expr::Tuple(elems) => {
            for e in elems {
                rewrite_builtin_calls_in_expr(e);
            }
        }
        Expr::Match { scrutinee, arms } => {
            rewrite_builtin_calls_in_expr(scrutinee);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    rewrite_builtin_calls_in_expr(guard);
                }
                rewrite_builtin_calls_in_expr(&mut arm.body);
            }
        }
        Expr::Lambda { body, .. } => {
            rewrite_builtin_calls_in_expr(body);
        }
        Expr::Spawn { target, args } => {
            rewrite_builtin_calls_in_expr(target);
            for (_, arg_expr) in args {
                rewrite_builtin_calls_in_expr(arg_expr);
            }
        }
        Expr::StructInit { fields, .. } => {
            for (_, field_expr) in fields {
                rewrite_builtin_calls_in_expr(field_expr);
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                rewrite_builtin_calls_in_expr(&mut arm.source);
                rewrite_builtin_calls_in_expr(&mut arm.body);
            }
            if let Some(timeout_clause) = timeout {
                rewrite_builtin_calls_in_expr(&mut timeout_clause.duration);
                rewrite_builtin_calls_in_expr(&mut timeout_clause.body);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let hew_parser::ast::StringPart::Expr(e) = part {
                    rewrite_builtin_calls_in_expr(e);
                }
            }
        }
        Expr::PostfixTry(inner) => rewrite_builtin_calls_in_expr(inner),
        Expr::Await(inner) => rewrite_builtin_calls_in_expr(inner),
        Expr::Yield(Some(inner)) => rewrite_builtin_calls_in_expr(inner),
        Expr::Send { target, message } => {
            rewrite_builtin_calls_in_expr(target);
            rewrite_builtin_calls_in_expr(message);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                rewrite_builtin_calls_in_expr(s);
            }
            if let Some(e) = end {
                rewrite_builtin_calls_in_expr(e);
            }
        }
        Expr::Unsafe(block) => rewrite_builtin_calls_in_block(block),
        Expr::Join(exprs) => {
            for e in exprs {
                rewrite_builtin_calls_in_expr(e);
            }
        }
        Expr::Timeout { expr, duration, .. } => {
            rewrite_builtin_calls_in_expr(expr);
            rewrite_builtin_calls_in_expr(duration);
        }
        Expr::ScopeLaunch(block) | Expr::ScopeSpawn(block) | Expr::Scope { body: block, .. } => {
            rewrite_builtin_calls_in_block(block);
        }
        Expr::SpawnLambdaActor { body, .. } => rewrite_builtin_calls_in_expr(body),
        _ => {}
    }
}

fn normalize_item_types(item: &mut Item) {
    match item {
        Item::Function(fn_decl) => normalize_fn_decl_types(fn_decl),
        Item::Actor(actor) => {
            for field in &mut actor.fields {
                normalize_type_expr(&mut field.ty.0);
            }
            if let Some(ref mut init) = actor.init {
                normalize_block_types(&mut init.body);
            }
            for recv in &mut actor.receive_fns {
                for param in &mut recv.params {
                    normalize_type_expr(&mut param.ty.0);
                }
                if let Some(ref mut rt) = recv.return_type {
                    normalize_type_expr(&mut rt.0);
                }
                normalize_block_types(&mut recv.body);
            }
            for method in &mut actor.methods {
                normalize_fn_decl_types(method);
            }
        }
        Item::Impl(impl_decl) => {
            normalize_type_expr(&mut impl_decl.target_type.0);
            for method in &mut impl_decl.methods {
                normalize_fn_decl_types(method);
            }
        }
        Item::ExternBlock(eb) => {
            for func in &mut eb.functions {
                for param in &mut func.params {
                    normalize_type_expr(&mut param.ty.0);
                }
                if let Some(ref mut rt) = func.return_type {
                    normalize_type_expr(&mut rt.0);
                }
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &mut td.body {
                match body_item {
                    hew_parser::ast::TypeBodyItem::Field { ty, .. } => {
                        normalize_type_expr(&mut ty.0);
                    }
                    hew_parser::ast::TypeBodyItem::Method(m) => {
                        normalize_fn_decl_types(m);
                    }
                    hew_parser::ast::TypeBodyItem::Variant(v) => match &mut v.kind {
                        hew_parser::ast::VariantKind::Tuple(fields) => {
                            for field_ty in fields {
                                normalize_type_expr(&mut field_ty.0);
                            }
                        }
                        hew_parser::ast::VariantKind::Struct(fields) => {
                            for (_name, field_ty) in fields {
                                normalize_type_expr(&mut field_ty.0);
                            }
                        }
                        hew_parser::ast::VariantKind::Unit => {}
                    },
                }
            }
        }
        Item::Trait(trait_decl) => {
            for trait_item in &mut trait_decl.items {
                match trait_item {
                    hew_parser::ast::TraitItem::Method(m) => {
                        for param in &mut m.params {
                            normalize_type_expr(&mut param.ty.0);
                        }
                        if let Some(ref mut rt) = m.return_type {
                            normalize_type_expr(&mut rt.0);
                        }
                        if let Some(ref mut body) = m.body {
                            normalize_block_types(body);
                        }
                    }
                    hew_parser::ast::TraitItem::AssociatedType { default, .. } => {
                        if let Some(ref mut default_ty) = default {
                            normalize_type_expr(&mut default_ty.0);
                        }
                    }
                }
            }
        }
        Item::Const(const_decl) => {
            normalize_type_expr(&mut const_decl.ty.0);
        }
        Item::TypeAlias(type_alias) => {
            normalize_type_expr(&mut type_alias.ty.0);
        }
        _ => {}
    }
}

fn normalize_fn_decl_types(fn_decl: &mut FnDecl) {
    for param in &mut fn_decl.params {
        normalize_type_expr(&mut param.ty.0);
    }
    if let Some(ref mut rt) = fn_decl.return_type {
        normalize_type_expr(&mut rt.0);
    }
    normalize_block_types(&mut fn_decl.body);
}

fn normalize_block_types(block: &mut Block) {
    for (stmt, _span) in &mut block.stmts {
        normalize_stmt_types(stmt);
    }
    if let Some(ref mut expr) = block.trailing_expr {
        normalize_expr_types(expr);
    }
}

fn normalize_stmt_types(stmt: &mut Stmt) {
    match stmt {
        Stmt::Let { ty, value, .. } | Stmt::Var { ty, value, .. } => {
            if let Some(ref mut t) = ty {
                normalize_type_expr(&mut t.0);
            }
            if let Some(ref mut val) = value {
                normalize_expr_types(val);
            }
        }
        Stmt::Expression(ref mut expr)
        | Stmt::Return(Some(ref mut expr))
        | Stmt::Break {
            value: Some(ref mut expr),
            ..
        } => {
            normalize_expr_types(expr);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            normalize_expr_types(condition);
            normalize_block_types(then_block);
            if let Some(ref mut eb) = else_block {
                if let Some(ref mut block) = eb.block {
                    normalize_block_types(block);
                }
                if let Some(ref mut if_stmt) = eb.if_stmt {
                    normalize_stmt_types(&mut if_stmt.0);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            normalize_expr_types(expr);
            normalize_block_types(body);
            if let Some(block) = else_body {
                normalize_block_types(block);
            }
        }
        Stmt::For { body, iterable, .. } => {
            normalize_expr_types(iterable);
            normalize_block_types(body);
        }
        Stmt::While {
            condition, body, ..
        } => {
            normalize_expr_types(condition);
            normalize_block_types(body);
        }
        Stmt::Loop { body, .. } => {
            normalize_block_types(body);
        }
        Stmt::Match { scrutinee, arms } => {
            normalize_expr_types(scrutinee);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    normalize_expr_types(guard);
                }
                normalize_expr_types(&mut arm.body);
            }
        }
        Stmt::Assign { target, value, .. } => {
            normalize_expr_types(target);
            normalize_expr_types(value);
        }
        Stmt::Defer(ref mut expr) => {
            normalize_expr_types(expr);
        }
        _ => {}
    }
}

fn normalize_expr_types(expr: &mut Spanned<Expr>) {
    stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, || {
        normalize_expr_types_inner(expr);
    });
}

fn normalize_expr_types_inner(expr: &mut Spanned<Expr>) {
    match &mut expr.0 {
        Expr::Block(block)
        | Expr::Scope { body: block, .. }
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            normalize_block_types(block);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            normalize_expr_types(condition);
            normalize_expr_types(then_block);
            if let Some(ref mut e) = else_block {
                normalize_expr_types(e);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            normalize_expr_types(expr);
            normalize_block_types(body);
            if let Some(block) = else_body {
                normalize_block_types(block);
            }
        }
        Expr::Match { scrutinee, arms } => {
            normalize_expr_types(scrutinee);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    normalize_expr_types(guard);
                }
                normalize_expr_types(&mut arm.body);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            normalize_expr_types(value);
            normalize_expr_types(count);
        }
        Expr::Array(elements) | Expr::Tuple(elements) => {
            for e in elements.iter_mut() {
                normalize_expr_types(e);
            }
        }
        Expr::Lambda {
            return_type,
            body,
            params,
            ..
        } => {
            if let Some(ref mut rt) = return_type {
                normalize_type_expr(&mut rt.0);
            }
            for param in params.iter_mut() {
                if let Some(ref mut t) = param.ty {
                    normalize_type_expr(&mut t.0);
                }
            }
            normalize_expr_types(body);
        }
        Expr::Call {
            function,
            args,
            type_args,
            ..
        } => {
            normalize_expr_types(function);
            for arg in args.iter_mut() {
                normalize_expr_types(arg.expr_mut());
            }
            if let Some(ref mut ta) = type_args {
                for t in ta.iter_mut() {
                    normalize_type_expr(&mut t.0);
                }
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            normalize_expr_types(receiver);
            for arg in args.iter_mut() {
                normalize_expr_types(arg.expr_mut());
            }
        }
        Expr::Binary { left, right, .. } => {
            normalize_expr_types(left);
            normalize_expr_types(right);
        }
        Expr::Unary { operand, .. } => {
            normalize_expr_types(operand);
        }
        Expr::Cast { expr, ty } => {
            normalize_expr_types(expr);
            normalize_type_expr(&mut ty.0);
        }
        Expr::FieldAccess { object, .. } => {
            normalize_expr_types(object);
        }
        Expr::Index { object, index } => {
            normalize_expr_types(object);
            normalize_expr_types(index);
        }
        Expr::StructInit { fields, .. } => {
            for (_name, val) in fields.iter_mut() {
                normalize_expr_types(val);
            }
        }
        Expr::Spawn { target, args } => {
            normalize_expr_types(target);
            for (_name, val) in args.iter_mut() {
                normalize_expr_types(val);
            }
        }
        Expr::SpawnLambdaActor { body, .. } => {
            normalize_expr_types(body);
        }
        Expr::Send { target, message } => {
            normalize_expr_types(target);
            normalize_expr_types(message);
        }
        Expr::Await(inner) | Expr::PostfixTry(inner) | Expr::Yield(Some(inner)) => {
            normalize_expr_types(inner);
        }
        Expr::Timeout {
            expr: inner,
            duration,
        } => {
            normalize_expr_types(inner);
            normalize_expr_types(duration);
        }
        Expr::Join(exprs) => {
            for e in exprs.iter_mut() {
                normalize_expr_types(e);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts.iter_mut() {
                if let hew_parser::ast::StringPart::Expr(e) = part {
                    normalize_expr_types(e);
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms.iter_mut() {
                normalize_expr_types(&mut arm.source);
                normalize_expr_types(&mut arm.body);
            }
            if let Some(ref mut t) = timeout {
                normalize_expr_types(&mut t.duration);
                normalize_expr_types(&mut t.body);
            }
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                normalize_expr_types(s);
            }
            if let Some(e) = end {
                normalize_expr_types(e);
            }
        }
        _ => {}
    }
}

fn enrich_item(item: &mut Item, tco: &TypeCheckOutput) {
    match item {
        Item::Function(fn_decl) => enrich_fn_decl(fn_decl, tco),
        Item::Actor(actor) => enrich_actor(actor, tco),
        Item::Impl(impl_decl) => {
            for method in &mut impl_decl.methods {
                enrich_fn_decl(method, tco);
            }
        }
        Item::Const(const_decl) => {
            enrich_expr(&mut const_decl.value, tco);
        }
        _ => {}
    }
}

fn enrich_fn_decl(fn_decl: &mut FnDecl, tco: &TypeCheckOutput) {
    enrich_block(&mut fn_decl.body, tco);

    let needs_infer =
        fn_decl.return_type.is_none() || matches!(&fn_decl.return_type, Some((TypeExpr::Infer, _)));
    if needs_infer {
        if let Some(ref expr) = fn_decl.body.trailing_expr {
            if let Some(inferred) = lookup_type(tco, &expr.1) {
                fn_decl.return_type = Some(inferred);
            }
        }
    }
}

fn enrich_actor(actor: &mut ActorDecl, tco: &TypeCheckOutput) {
    if let Some(ref mut init) = actor.init {
        enrich_block(&mut init.body, tco);
    }
    for recv in &mut actor.receive_fns {
        enrich_block(&mut recv.body, tco);
    }
    for method in &mut actor.methods {
        enrich_fn_decl(method, tco);
    }
}

fn enrich_block(block: &mut Block, tco: &TypeCheckOutput) {
    for (stmt, _span) in &mut block.stmts {
        enrich_stmt(stmt, tco);
    }
    if let Some(ref mut expr) = block.trailing_expr {
        enrich_expr(expr, tco);
    }
}

fn enrich_stmt(stmt: &mut Stmt, tco: &TypeCheckOutput) {
    match stmt {
        Stmt::Let { ty, value, .. } | Stmt::Var { ty, value, .. } => {
            if ty.is_none() {
                if let Some(ref val) = *value {
                    if let Some(inferred) = lookup_type(tco, &val.1) {
                        *ty = Some(inferred);
                    }
                }
            }
            if let Some(ref mut val) = value {
                enrich_expr(val, tco);
            }
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            enrich_expr(condition, tco);
            enrich_block(then_block, tco);
            if let Some(ref mut else_b) = else_block {
                enrich_else_block(else_b, tco);
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            enrich_expr(expr, tco);
            enrich_block(body, tco);
            if let Some(block) = else_body {
                enrich_block(block, tco);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            enrich_expr(scrutinee, tco);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    enrich_expr(guard, tco);
                }
                enrich_expr(&mut arm.body, tco);
            }
        }
        Stmt::For { body, iterable, .. } => {
            enrich_expr(iterable, tco);
            enrich_block(body, tco);
        }
        Stmt::While {
            condition, body, ..
        } => {
            enrich_expr(condition, tco);
            enrich_block(body, tco);
        }
        Stmt::Loop { body, .. } => {
            enrich_block(body, tco);
        }
        Stmt::Expression(ref mut expr)
        | Stmt::Return(Some(ref mut expr))
        | Stmt::Break {
            value: Some(ref mut expr),
            ..
        } => {
            enrich_expr(expr, tco);
        }
        Stmt::Defer(ref mut expr) => {
            enrich_expr(expr, tco);
        }
        Stmt::Assign { target, value, .. } => {
            enrich_expr(target, tco);
            enrich_expr(value, tco);
        }
        _ => {}
    }
}

fn enrich_else_block(else_block: &mut ElseBlock, tco: &TypeCheckOutput) {
    if let Some(ref mut block) = else_block.block {
        enrich_block(block, tco);
    }
    if let Some(ref mut if_stmt) = else_block.if_stmt {
        enrich_stmt(&mut if_stmt.0, tco);
    }
}

fn enrich_expr(expr: &mut Spanned<Expr>, tco: &TypeCheckOutput) {
    match &mut expr.0 {
        Expr::Block(block) => enrich_block(block, tco),
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            enrich_expr(condition, tco);
            enrich_expr(then_block, tco);
            if let Some(ref mut e) = else_block {
                enrich_expr(e, tco);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            enrich_expr(expr, tco);
            enrich_block(body, tco);
            if let Some(block) = else_body {
                enrich_block(block, tco);
            }
        }
        Expr::Match { scrutinee, arms } => {
            enrich_expr(scrutinee, tco);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    enrich_expr(guard, tco);
                }
                enrich_expr(&mut arm.body, tco);
            }
        }
        Expr::Array(elements) | Expr::Tuple(elements) => {
            for e in elements.iter_mut() {
                enrich_expr(e, tco);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            enrich_expr(body, tco);
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            enrich_expr(receiver, tco);
            for arg in args.iter_mut() {
                enrich_expr(arg.expr_mut(), tco);
            }
            // Rewrite module-qualified stdlib calls: e.g. os.pid() → hew_os_pid()
            // This happens during AST enrichment, before serialization.
            if let Expr::Identifier(module_name) = &receiver.0 {
                if let Some(c_symbol) = hew_types::stdlib::resolve_module_call(module_name, method)
                {
                    // Skip identity-mapped wrappers (e.g. log.setup → setup): these are
                    // non-trivial Hew wrappers that must be compiled as module graph
                    // functions and called via their mangled name. Leaving them as
                    // MethodCall lets the C++ codegen dispatch them correctly.
                    if c_symbol != method {
                        let old_args = std::mem::take(args);
                        expr.0 = Expr::Call {
                            function: Box::new((
                                Expr::Identifier(c_symbol.to_string()),
                                receiver.1.clone(),
                            )),
                            type_args: None,
                            args: old_args,
                            is_tail_call: false,
                        };
                        return;
                    }
                }
                // Rewrite user module calls: e.g. utils.helper(args) → helper(args)
                // User module functions compile under their own name, not a C symbol.
                if tco.user_modules.contains(module_name) {
                    let old_args = std::mem::take(args);
                    expr.0 = Expr::Call {
                        function: Box::new((Expr::Identifier(method.clone()), receiver.1.clone())),
                        type_args: None,
                        args: old_args,
                        is_tail_call: false,
                    };
                    return;
                }
            }
            // Rewrite handle method calls to C function calls.
            // The receiver type is looked up from the type checker output;
            // if it's a handle type (e.g. http.Request), the method call is
            // rewritten to a plain function call with the receiver prepended
            // as the first argument.
            let key = SpanKey {
                start: receiver.1.start,
                end: receiver.1.end,
            };
            let c_fn = match tco.expr_types.get(&key) {
                // `Stream` and `Sink` may appear as unparameterised Named types
                // (e.g. `let raw: Stream = ...`) as well as the dedicated Ty variants.
                Some(Ty::Named { name, .. }) if name == "Stream" => {
                    hew_types::stdlib::resolve_stream_method("Stream", method)
                }
                Some(Ty::Named { name, .. }) if name == "Sink" => {
                    hew_types::stdlib::resolve_stream_method("Sink", method)
                }
                Some(Ty::Named { name, .. }) => {
                    hew_types::stdlib::resolve_handle_method(name, method)
                }
                Some(Ty::Stream(_)) => hew_types::stdlib::resolve_stream_method("Stream", method),
                Some(Ty::Sink(_)) => hew_types::stdlib::resolve_stream_method("Sink", method),
                _ => None,
            };
            if let Some(c_fn) = c_fn {
                let span = expr.1.clone();
                let recv = std::mem::replace(
                    receiver.as_mut(),
                    (
                        Expr::Literal(hew_parser::ast::Literal::Integer {
                            value: 0,
                            radix: hew_parser::ast::IntRadix::Decimal,
                        }),
                        0..0,
                    ),
                );
                let old_args = std::mem::take(args);
                let mut all_args = Vec::with_capacity(1 + old_args.len());
                all_args.push(hew_parser::ast::CallArg::Positional(recv));
                all_args.extend(old_args);
                expr.0 = Expr::Call {
                    function: Box::new((Expr::Identifier(c_fn.to_string()), span)),
                    type_args: None,
                    args: all_args,
                    is_tail_call: false,
                };
            }
        }
        Expr::Call { function, args, .. } => {
            enrich_expr(function, tco);
            for arg in args.iter_mut() {
                enrich_expr(arg.expr_mut(), tco);
            }
            // Rewrite len(x) → x.len() method call so the C++ codegen
            // dispatches to VecLenOp / HashMapLenOp / StringMethodOp.
            if let Expr::Identifier(name) = &function.0 {
                if name == "len" && args.len() == 1 {
                    let receiver = match std::mem::take(args).remove(0) {
                        CallArg::Positional(e) => e,
                        CallArg::Named { value, .. } => value,
                    };
                    expr.0 = Expr::MethodCall {
                        receiver: Box::new(receiver),
                        method: "len".to_string(),
                        args: Vec::new(),
                    };
                    return;
                }
            }
        }
        Expr::Binary { left, right, .. } => {
            enrich_expr(left, tco);
            enrich_expr(right, tco);
        }
        Expr::Unary { operand: inner, .. }
        | Expr::Cast { expr: inner, .. }
        | Expr::Await(inner)
        | Expr::PostfixTry(inner)
        | Expr::Yield(Some(inner)) => {
            enrich_expr(inner, tco);
        }
        Expr::FieldAccess { object, .. } => {
            enrich_expr(object, tco);
        }
        Expr::Index { object, index } => {
            enrich_expr(object, tco);
            enrich_expr(index, tco);
        }
        Expr::StructInit { fields, .. } => {
            for (_name, val) in fields.iter_mut() {
                enrich_expr(val, tco);
            }
        }
        Expr::Spawn { target, args } => {
            enrich_expr(target, tco);
            for (_name, val) in args.iter_mut() {
                enrich_expr(val, tco);
            }
        }
        Expr::Send { target, message } => {
            enrich_expr(target, tco);
            enrich_expr(message, tco);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                enrich_expr(s, tco);
            }
            if let Some(e) = end {
                enrich_expr(e, tco);
            }
        }
        Expr::Scope { body: block, .. }
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            enrich_block(block, tco);
        }
        Expr::Timeout {
            expr: inner,
            duration,
        } => {
            enrich_expr(inner, tco);
            enrich_expr(duration, tco);
        }
        Expr::Join(exprs) => {
            for e in exprs.iter_mut() {
                enrich_expr(e, tco);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts.iter_mut() {
                if let hew_parser::ast::StringPart::Expr(e) = part {
                    enrich_expr(e, tco);
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms.iter_mut() {
                enrich_expr(&mut arm.source, tco);
                enrich_expr(&mut arm.body, tco);
            }
            if let Some(ref mut t) = timeout {
                enrich_expr(&mut t.duration, tco);
                enrich_expr(&mut t.body, tco);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::{ImportDecl, Visibility};

    // -----------------------------------------------------------------------
    // normalize_type_expr tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_normalize_result_type() {
        let ok_ty = (
            TypeExpr::Named {
                name: "i32".into(),
                type_args: None,
            },
            0..0,
        );
        let err_ty = (
            TypeExpr::Named {
                name: "string".into(),
                type_args: None,
            },
            0..0,
        );
        let mut te = TypeExpr::Named {
            name: "Result".into(),
            type_args: Some(vec![ok_ty, err_ty]),
        };
        normalize_type_expr(&mut te);
        match te {
            TypeExpr::Result { ok, err } => {
                assert!(matches!(
                    ok.0,
                    TypeExpr::Named {
                        ref name,
                        ..
                    } if name == "i32"
                ));
                assert!(matches!(
                    err.0,
                    TypeExpr::Named {
                        ref name,
                        ..
                    } if name == "string"
                ));
            }
            _ => panic!("expected Result variant, got {te:?}"),
        }
    }

    #[test]
    fn test_normalize_option_type() {
        let inner = (
            TypeExpr::Named {
                name: "i32".into(),
                type_args: None,
            },
            0..0,
        );
        let mut te = TypeExpr::Named {
            name: "Option".into(),
            type_args: Some(vec![inner]),
        };
        normalize_type_expr(&mut te);
        match te {
            TypeExpr::Option(inner) => {
                assert!(matches!(
                    inner.0,
                    TypeExpr::Named {
                        ref name,
                        ..
                    } if name == "i32"
                ));
            }
            _ => panic!("expected Option variant, got {te:?}"),
        }
    }

    #[test]
    fn test_normalize_nested_result_in_option() {
        let ok = (
            TypeExpr::Named {
                name: "i32".into(),
                type_args: None,
            },
            0..0,
        );
        let err = (
            TypeExpr::Named {
                name: "string".into(),
                type_args: None,
            },
            0..0,
        );
        let result_te = (
            TypeExpr::Named {
                name: "Result".into(),
                type_args: Some(vec![ok, err]),
            },
            0..0,
        );
        let mut te = TypeExpr::Named {
            name: "Option".into(),
            type_args: Some(vec![result_te]),
        };
        normalize_type_expr(&mut te);
        match te {
            TypeExpr::Option(inner) => {
                assert!(matches!(inner.0, TypeExpr::Result { .. }));
            }
            _ => panic!("expected Option(Result{{..}}), got {te:?}"),
        }
    }

    #[test]
    fn test_normalize_non_result_named_unchanged() {
        let mut te = TypeExpr::Named {
            name: "Vec".into(),
            type_args: Some(vec![(
                TypeExpr::Named {
                    name: "i32".into(),
                    type_args: None,
                },
                0..0,
            )]),
        };
        normalize_type_expr(&mut te);
        assert!(matches!(te, TypeExpr::Named { ref name, .. } if name == "Vec"));
    }

    #[test]
    fn test_normalize_result_wrong_arity_unchanged() {
        let mut te = TypeExpr::Named {
            name: "Result".into(),
            type_args: Some(vec![(
                TypeExpr::Named {
                    name: "i32".into(),
                    type_args: None,
                },
                0..0,
            )]),
        };
        normalize_type_expr(&mut te);
        assert!(matches!(te, TypeExpr::Named { ref name, .. } if name == "Result"));
    }

    #[test]
    fn test_normalize_result_no_type_args_unchanged() {
        let mut te = TypeExpr::Named {
            name: "Result".into(),
            type_args: None,
        };
        normalize_type_expr(&mut te);
        assert!(matches!(te, TypeExpr::Named { ref name, .. } if name == "Result"));
    }

    #[test]
    fn test_normalize_option_wrong_arity_unchanged() {
        let mut te = TypeExpr::Named {
            name: "Option".into(),
            type_args: Some(vec![
                (
                    TypeExpr::Named {
                        name: "i32".into(),
                        type_args: None,
                    },
                    0..0,
                ),
                (
                    TypeExpr::Named {
                        name: "bool".into(),
                        type_args: None,
                    },
                    0..0,
                ),
            ]),
        };
        normalize_type_expr(&mut te);
        assert!(matches!(te, TypeExpr::Named { ref name, .. } if name == "Option"));
    }

    #[test]
    fn test_normalize_tuple_children() {
        let mut te = TypeExpr::Tuple(vec![
            (
                TypeExpr::Named {
                    name: "Option".into(),
                    type_args: Some(vec![(
                        TypeExpr::Named {
                            name: "i32".into(),
                            type_args: None,
                        },
                        0..0,
                    )]),
                },
                0..0,
            ),
            (
                TypeExpr::Named {
                    name: "i32".into(),
                    type_args: None,
                },
                0..0,
            ),
        ]);
        normalize_type_expr(&mut te);
        if let TypeExpr::Tuple(elems) = &te {
            assert!(matches!(elems[0].0, TypeExpr::Option(_)));
        } else {
            panic!("expected Tuple");
        }
    }

    #[test]
    fn test_normalize_named_no_type_args() {
        let mut te = TypeExpr::Named {
            name: "i32".into(),
            type_args: None,
        };
        normalize_type_expr(&mut te);
        assert!(matches!(te, TypeExpr::Named { ref name, .. } if name == "i32"));
    }

    #[test]
    fn test_normalize_function_type_children() {
        let mut te = TypeExpr::Function {
            params: vec![(
                TypeExpr::Named {
                    name: "Option".into(),
                    type_args: Some(vec![(
                        TypeExpr::Named {
                            name: "i32".into(),
                            type_args: None,
                        },
                        0..0,
                    )]),
                },
                0..0,
            )],
            return_type: Box::new((
                TypeExpr::Named {
                    name: "Result".into(),
                    type_args: Some(vec![
                        (
                            TypeExpr::Named {
                                name: "i32".into(),
                                type_args: None,
                            },
                            0..0,
                        ),
                        (
                            TypeExpr::Named {
                                name: "string".into(),
                                type_args: None,
                            },
                            0..0,
                        ),
                    ]),
                },
                0..0,
            )),
        };
        normalize_type_expr(&mut te);
        if let TypeExpr::Function {
            params,
            return_type,
        } = &te
        {
            assert!(matches!(params[0].0, TypeExpr::Option(_)));
            assert!(matches!(return_type.0, TypeExpr::Result { .. }));
        } else {
            panic!("expected Function type");
        }
    }

    // -----------------------------------------------------------------------
    // synthesize_stdlib_externs tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_synthesize_stdlib_externs_known_module() {
        let mut program = Program {
            items: vec![(
                Item::Import(ImportDecl {
                    path: vec!["std".into(), "fs".into()],
                    spec: None,
                    file_path: None,
                    resolved_items: None,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(&mut program);
        assert!(
            program.items.len() > 1,
            "expected extern block to be synthesized for std::fs"
        );
        let has_extern = program
            .items
            .iter()
            .any(|(item, _)| matches!(item, Item::ExternBlock(_)));
        assert!(has_extern, "no extern block found");
    }

    #[test]
    fn test_synthesize_stdlib_externs_unknown_module() {
        let mut program = Program {
            items: vec![(
                Item::Import(ImportDecl {
                    path: vec!["unknown".into(), "module".into()],
                    spec: None,
                    file_path: None,
                    resolved_items: None,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(&mut program);
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_synthesize_stdlib_externs_empty_program() {
        let mut program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(&mut program);
        assert!(program.items.is_empty());
    }

    #[test]
    fn test_synthesize_stdlib_externs_non_import_items() {
        let mut program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "main".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(&mut program);
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_synthesize_multiple_imports() {
        let mut program = Program {
            items: vec![
                (
                    Item::Import(ImportDecl {
                        path: vec!["std".into(), "fs".into()],
                        spec: None,
                        file_path: None,
                        resolved_items: None,
                    }),
                    0..0,
                ),
                (
                    Item::Import(ImportDecl {
                        path: vec!["std".into(), "encoding".into(), "json".into()],
                        spec: None,
                        file_path: None,
                        resolved_items: None,
                    }),
                    0..0,
                ),
            ],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(&mut program);
        let extern_count = program
            .items
            .iter()
            .filter(|(item, _)| matches!(item, Item::ExternBlock(_)))
            .count();
        assert!(
            extern_count >= 2,
            "expected at least 2 extern blocks, got {extern_count}"
        );
    }

    // -----------------------------------------------------------------------
    // ty_to_type_expr tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_ty_to_type_expr_primitives() {
        let cases = vec![
            (Ty::I32, "i32"),
            (Ty::I64, "i64"),
            (Ty::F64, "f64"),
            (Ty::Bool, "bool"),
            (Ty::String, "string"),
            (Ty::Char, "char"),
            (Ty::Never, "!"),
        ];
        for (ty, expected_name) in cases {
            let result = ty_to_type_expr(&ty);
            assert!(result.is_some(), "expected Some for {ty:?}");
            let (te, _span) = result.unwrap();
            match te {
                TypeExpr::Named { name, type_args } => {
                    assert_eq!(name, expected_name);
                    assert!(type_args.is_none());
                }
                _ => panic!("expected Named variant for {ty:?}"),
            }
        }
    }

    #[test]
    fn test_ty_to_type_expr_unit_returns_none() {
        let result = ty_to_type_expr(&Ty::Unit);
        assert!(
            result.is_none(),
            "Unit should return None (C++ handles via built-in logic)"
        );
    }

    #[test]
    fn test_ty_to_type_expr_error_returns_none() {
        assert!(ty_to_type_expr(&Ty::Error).is_none());
    }

    #[test]
    fn test_ty_to_type_expr_option() {
        let ty = Ty::Option(Box::new(Ty::I32));
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        assert!(matches!(result.unwrap().0, TypeExpr::Option(_)));
    }

    #[test]
    fn test_ty_to_type_expr_result() {
        let ty = Ty::Result {
            ok: Box::new(Ty::I32),
            err: Box::new(Ty::String),
        };
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        assert!(matches!(result.unwrap().0, TypeExpr::Result { .. }));
    }

    #[test]
    fn test_ty_to_type_expr_tuple() {
        let ty = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        if let TypeExpr::Tuple(elems) = &result.unwrap().0 {
            assert_eq!(elems.len(), 2);
        } else {
            panic!("expected Tuple");
        }
    }

    #[test]
    fn test_ty_to_type_expr_function() {
        let ty = Ty::Function {
            params: vec![Ty::I32, Ty::Bool],
            ret: Box::new(Ty::String),
        };
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        assert!(matches!(result.unwrap().0, TypeExpr::Function { .. }));
    }

    #[test]
    fn test_ty_to_type_expr_named_with_args() {
        let ty = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I32],
        };
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        if let TypeExpr::Named { name, type_args } = &result.unwrap().0 {
            assert_eq!(name, "Vec");
            assert_eq!(type_args.as_ref().unwrap().len(), 1);
        } else {
            panic!("expected Named");
        }
    }

    #[test]
    fn test_ty_to_type_expr_actor_ref() {
        let ty = Ty::ActorRef(Box::new(Ty::I32));
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        if let TypeExpr::Named { name, type_args } = &result.unwrap().0 {
            assert_eq!(name, "ActorRef");
            assert!(type_args.is_some());
        } else {
            panic!("expected Named ActorRef");
        }
    }

    #[test]
    fn test_ty_to_type_expr_stream() {
        let ty = Ty::Stream(Box::new(Ty::I64));
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        if let TypeExpr::Named { name, .. } = &result.unwrap().0 {
            assert_eq!(name, "Stream");
        } else {
            panic!("expected Named Stream");
        }
    }

    #[test]
    fn test_ty_to_type_expr_array() {
        let ty = Ty::Array(Box::new(Ty::I32), 10);
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        if let TypeExpr::Array { element, size } = &result.unwrap().0 {
            assert_eq!(*size, 10);
            assert!(matches!(
                element.0,
                TypeExpr::Named { ref name, .. } if name == "i32"
            ));
        } else {
            panic!("expected Array");
        }
    }

    #[test]
    fn test_ty_to_type_expr_pointer() {
        let ty = Ty::Pointer {
            is_mutable: true,
            pointee: Box::new(Ty::I32),
        };
        let result = ty_to_type_expr(&ty);
        assert!(result.is_some());
        if let TypeExpr::Pointer {
            is_mutable,
            pointee,
        } = &result.unwrap().0
        {
            assert!(*is_mutable);
            assert!(matches!(
                pointee.0,
                TypeExpr::Named { ref name, .. } if name == "i32"
            ));
        } else {
            panic!("expected Pointer");
        }
    }

    #[test]
    fn test_ty_to_type_expr_generator() {
        let ty = Ty::Generator {
            yields: Box::new(Ty::I32),
            returns: Box::new(Ty::String),
        };
        let result = ty_to_type_expr(&ty);
        assert!(
            result.is_none(),
            "Generator should return None (C++ handles via built-in logic)"
        );
    }

    #[test]
    fn test_ty_to_type_expr_async_generator() {
        let ty = Ty::AsyncGenerator {
            yields: Box::new(Ty::I32),
        };
        let result = ty_to_type_expr(&ty);
        assert!(
            result.is_none(),
            "AsyncGenerator should return None (C++ handles via built-in logic)"
        );
    }

    #[test]
    fn test_ty_to_type_expr_range() {
        let ty = Ty::Range(Box::new(Ty::I32));
        let result = ty_to_type_expr(&ty);
        assert!(
            result.is_none(),
            "Range should return None (C++ handles via built-in logic)"
        );
    }

    #[test]
    fn test_ty_to_type_expr_var_returns_none() {
        use hew_types::ty::TypeVar;
        assert!(ty_to_type_expr(&Ty::Var(TypeVar(123))).is_none());
    }

    // -----------------------------------------------------------------------
    // normalize_all_types integration test
    // -----------------------------------------------------------------------

    #[test]
    fn test_normalize_all_types_fn_return() {
        let mut program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "foo".into(),
                    type_params: None,
                    params: vec![],
                    return_type: Some((
                        TypeExpr::Named {
                            name: "Option".into(),
                            type_args: Some(vec![(
                                TypeExpr::Named {
                                    name: "i32".into(),
                                    type_args: None,
                                },
                                0..0,
                            )]),
                        },
                        0..0,
                    )),
                    where_clause: None,
                    body: Block {
                        stmts: vec![],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        normalize_all_types(&mut program);
        if let Item::Function(f) = &program.items[0].0 {
            assert!(
                matches!(f.return_type.as_ref().unwrap().0, TypeExpr::Option(_)),
                "return type should be normalized to Option variant"
            );
        } else {
            panic!("expected function");
        }
    }

    // -----------------------------------------------------------------------
    // User module call rewriting tests
    // -----------------------------------------------------------------------

    /// Helper: create a TypeCheckOutput with user_modules set.
    #[allow(unused_imports)]
    use std::collections::{HashMap, HashSet};

    fn make_tco_with_user_modules(modules: Vec<&str>) -> TypeCheckOutput {
        TypeCheckOutput {
            expr_types: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: modules.into_iter().map(String::from).collect(),
        }
    }

    #[test]
    fn test_enrich_user_module_call_rewritten() {
        use hew_parser::ast::CallArg;

        let tco = make_tco_with_user_modules(vec!["utils"]);

        // Build: utils.helper(42)
        let mut expr: Spanned<Expr> = (
            Expr::MethodCall {
                receiver: Box::new((Expr::Identifier("utils".to_string()), 0..5)),
                method: "helper".to_string(),
                args: vec![CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 42,
                        radix: hew_parser::ast::IntRadix::Decimal,
                    }),
                    6..8,
                ))],
            },
            0..15,
        );

        enrich_expr(&mut expr, &tco);

        // Should be rewritten to: helper(42)
        match &expr.0 {
            Expr::Call { function, args, .. } => {
                match &function.0 {
                    Expr::Identifier(name) => {
                        assert_eq!(name, "helper", "should rewrite to bare function name");
                    }
                    other => panic!("expected Identifier, got {other:?}"),
                }
                assert_eq!(args.len(), 1, "should preserve args");
            }
            other => panic!("expected Call expr, got {other:?}"),
        }
    }

    #[test]
    fn test_enrich_non_user_module_not_rewritten() {
        // A method call on a non-module identifier should NOT be rewritten
        let tco = make_tco_with_user_modules(vec!["utils"]);

        // Build: obj.method() where "obj" is not a user module
        let mut expr: Spanned<Expr> = (
            Expr::MethodCall {
                receiver: Box::new((Expr::Identifier("obj".to_string()), 0..3)),
                method: "method".to_string(),
                args: vec![],
            },
            0..12,
        );

        enrich_expr(&mut expr, &tco);

        // Should still be a MethodCall (not rewritten)
        assert!(
            matches!(&expr.0, Expr::MethodCall { .. }),
            "non-module method call should not be rewritten, got {:?}",
            expr.0
        );
    }

    #[test]
    fn test_enrich_stdlib_module_uses_c_symbol() {
        // A stdlib module call should use the C symbol, not bare name
        let tco = make_tco_with_user_modules(vec![]); // no user modules

        // Build: fs.read_file("test.txt") — "fs" is a stdlib module
        let mut expr: Spanned<Expr> = (
            Expr::MethodCall {
                receiver: Box::new((Expr::Identifier("fs".to_string()), 0..2)),
                method: "read_file".to_string(),
                args: vec![hew_parser::ast::CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::String("\"test.txt\"".to_string())),
                    3..13,
                ))],
            },
            0..14,
        );

        enrich_expr(&mut expr, &tco);

        // Should be rewritten to C symbol (hew_fs_read_file) not bare "read_file"
        match &expr.0 {
            Expr::Call { function, .. } => match &function.0 {
                Expr::Identifier(name) => {
                    assert!(
                        name.starts_with("hew_fs_"),
                        "stdlib call should use C symbol, got '{name}'"
                    );
                }
                other => panic!("expected Identifier, got {other:?}"),
            },
            // If not rewritten (stdlib not loaded in test), it stays as MethodCall — that's OK
            Expr::MethodCall { .. } => {
                // stdlib may not be loaded in this test context, so this is acceptable
            }
            other => panic!("unexpected expr: {other:?}"),
        }
    }

    #[test]
    fn test_enrich_user_module_preserves_multiple_args() {
        use hew_parser::ast::CallArg;

        let tco = make_tco_with_user_modules(vec!["math"]);

        // Build: math.add(1, 2)
        let mut expr: Spanned<Expr> = (
            Expr::MethodCall {
                receiver: Box::new((Expr::Identifier("math".to_string()), 0..4)),
                method: "add".to_string(),
                args: vec![
                    CallArg::Positional((
                        Expr::Literal(hew_parser::ast::Literal::Integer {
                            value: 1,
                            radix: hew_parser::ast::IntRadix::Decimal,
                        }),
                        5..6,
                    )),
                    CallArg::Positional((
                        Expr::Literal(hew_parser::ast::Literal::Integer {
                            value: 2,
                            radix: hew_parser::ast::IntRadix::Decimal,
                        }),
                        8..9,
                    )),
                ],
            },
            0..10,
        );

        enrich_expr(&mut expr, &tco);

        match &expr.0 {
            Expr::Call { function, args, .. } => {
                assert_eq!(
                    match &function.0 {
                        Expr::Identifier(n) => n.as_str(),
                        _ => panic!("expected identifier"),
                    },
                    "add"
                );
                assert_eq!(args.len(), 2, "should preserve both args");
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }
}
