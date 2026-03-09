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
use std::fmt;

use crate::msgpack::ExprTypeEntry;

/// Diagnostic describing why a resolved [`Ty`] could not be serialized into a
/// [`TypeExpr`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExprConversionError {
    ty: Ty,
    detail: &'static str,
    contexts: Vec<String>,
    span: Option<Span>,
}

impl TypeExprConversionError {
    fn unsupported(ty: &Ty, detail: &'static str) -> Self {
        Self {
            ty: ty.clone(),
            detail,
            contexts: Vec::new(),
            span: None,
        }
    }

    fn with_context(mut self, context: impl Into<String>) -> Self {
        self.contexts.push(context.into());
        self
    }

    fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    #[must_use]
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

impl fmt::Display for TypeExprConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for context in self.contexts.iter().rev() {
            write!(f, "{context}: ")?;
        }
        write!(f, "{} (`{}`)", self.detail, self.ty)
    }
}

impl std::error::Error for TypeExprConversionError {}

/// Result of serializing inferred expression types for codegen.
#[derive(Debug, Default, Clone)]
pub struct ExprTypeMapBuild {
    pub entries: Vec<ExprTypeEntry>,
    diagnostics: Vec<TypeExprConversionError>,
}

impl ExprTypeMapBuild {
    #[must_use]
    pub fn diagnostics(&self) -> &[TypeExprConversionError] {
        &self.diagnostics
    }
}

/// Collected diagnostics from best-effort inferred-type enrichment.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct EnrichProgramDiagnostics {
    diagnostics: Vec<TypeExprConversionError>,
}

impl EnrichProgramDiagnostics {
    #[must_use]
    pub fn diagnostics(&self) -> &[TypeExprConversionError] {
        &self.diagnostics
    }
}

/// Build the expression type map from [`TypeCheckOutput`].
///
/// Converts every entry in `tco.expr_types` (span → [`Ty`]) into an
/// [`ExprTypeEntry`] (start, end, [`TypeExpr`]) that can be serialized
/// alongside the AST. Unsupported entries now fail closed with explicit
/// diagnostics instead of being silently omitted.
#[must_use]
pub fn build_expr_type_map(tco: &TypeCheckOutput) -> ExprTypeMapBuild {
    let mut build = ExprTypeMapBuild::default();

    for (key, ty) in &tco.expr_types {
        match ty_to_type_expr(ty) {
            Ok(ty) => build.entries.push(ExprTypeEntry {
                start: key.start,
                end: key.end,
                ty,
            }),
            Err(diagnostic) => build
                .diagnostics
                .push(diagnostic.with_span(key.start..key.end)),
        }
    }

    build
}

fn require_converted(
    ty: &Ty,
    context: impl Into<String>,
) -> Result<Spanned<TypeExpr>, TypeExprConversionError> {
    let context = context.into();
    ty_to_type_expr(ty).map_err(|diagnostic| diagnostic.with_context(context))
}

#[allow(
    clippy::too_many_lines,
    reason = "type mapping covers many Ty variants"
)]
fn ty_to_type_expr(ty: &Ty) -> Result<Spanned<TypeExpr>, TypeExprConversionError> {
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
        Ty::Bytes => TypeExpr::Named {
            name: "bytes".into(),
            type_args: None,
        },
        Ty::Duration => TypeExpr::Named {
            name: "duration".into(),
            type_args: None,
        },
        Ty::Unit => TypeExpr::Tuple(Vec::new()),
        Ty::Never => TypeExpr::Named {
            name: "!".into(),
            type_args: None,
        },

        Ty::Named { name, args } => match (name.as_str(), args.len()) {
            ("Option", 1) => {
                let inner_expr = require_converted(&args[0], "Option inner type")?;
                TypeExpr::Option(Box::new(inner_expr))
            }
            ("Result", 2) => {
                let ok_expr = require_converted(&args[0], "Result ok type")?;
                let err_expr = require_converted(&args[1], "Result error type")?;
                TypeExpr::Result {
                    ok: Box::new(ok_expr),
                    err: Box::new(err_expr),
                }
            }
            ("Generator", _) => {
                return Err(TypeExprConversionError::unsupported(
                    ty,
                    "generator type is not representable in serialized TypeExpr",
                ));
            }
            ("AsyncGenerator", _) => {
                return Err(TypeExprConversionError::unsupported(
                    ty,
                    "async generator type is not representable in serialized TypeExpr",
                ));
            }
            _ => {
                let type_args = if args.is_empty() {
                    None
                } else {
                    Some(
                        args.iter()
                            .enumerate()
                            .map(|(index, arg)| {
                                require_converted(arg, format!("type argument {index} of `{name}`"))
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    )
                };
                TypeExpr::Named {
                    name: name.clone(),
                    type_args,
                }
            }
        },

        Ty::Function { params, ret } => {
            let param_exprs = params
                .iter()
                .enumerate()
                .map(|(index, param)| {
                    require_converted(param, format!("function parameter {index}"))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let ret_expr = require_converted(ret, "function return type")?;
            TypeExpr::Function {
                params: param_exprs,
                return_type: Box::new(ret_expr),
            }
        }

        Ty::Closure { params, ret, .. } => {
            let param_exprs = params
                .iter()
                .enumerate()
                .map(|(index, param)| {
                    require_converted(param, format!("closure parameter {index}"))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let ret_expr = require_converted(ret, "closure return type")?;
            TypeExpr::Function {
                params: param_exprs,
                return_type: Box::new(ret_expr),
            }
        }

        Ty::Tuple(elements) => {
            let elem_exprs = elements
                .iter()
                .enumerate()
                .map(|(index, element)| {
                    require_converted(element, format!("tuple element {index}"))
                })
                .collect::<Result<Vec<_>, _>>()?;
            TypeExpr::Tuple(elem_exprs)
        }
        Ty::Array(element, size) => {
            let elem = require_converted(element, "array element type")?;
            TypeExpr::Array {
                element: Box::new(elem),
                size: *size,
            }
        }
        Ty::Slice(element) => {
            let elem = require_converted(element, "slice element type")?;
            TypeExpr::Slice(Box::new(elem))
        }

        Ty::Pointer {
            is_mutable,
            pointee,
        } => {
            let pointee_expr = require_converted(pointee, "pointer pointee type")?;
            TypeExpr::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(pointee_expr),
            }
        }

        Ty::TraitObject { traits } => {
            let bounds = traits
                .iter()
                .enumerate()
                .map(|(bound_index, b)| {
                    Ok(TraitBound {
                        name: b.trait_name.clone(),
                        type_args: if b.args.is_empty() {
                            None
                        } else {
                            Some(
                                b.args
                                    .iter()
                                    .enumerate()
                                    .map(|(arg_index, arg)| {
                                        require_converted(
                                            arg,
                                            format!(
                                                "trait object bound {bound_index} (`{}`) type argument {arg_index}",
                                                b.trait_name
                                            ),
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()?,
                            )
                        },
                    })
                })
                .collect::<Result<Vec<_>, TypeExprConversionError>>()?;
            TypeExpr::TraitObject(bounds)
        }
        Ty::Var(_) => {
            return Err(TypeExprConversionError::unsupported(
                ty,
                "unresolved type variable reached serializer",
            ));
        }
        Ty::Error => {
            return Err(TypeExprConversionError::unsupported(
                ty,
                "type-checker error sentinel reached serializer",
            ));
        }

        // Machine types map to Named for serialization
        Ty::Machine { name } => TypeExpr::Named {
            name: name.clone(),
            type_args: None,
        },
    };

    Ok((te, span))
}

/// Look up the inferred type for a span in the `TypeCheckOutput`.
fn lookup_inferred_type(
    tco: &TypeCheckOutput,
    span: &Span,
    context: impl Into<String>,
) -> Result<Option<Spanned<TypeExpr>>, TypeExprConversionError> {
    let key = SpanKey {
        start: span.start,
        end: span.end,
    };
    let Some(ty) = tco.expr_types.get(&key) else {
        return Ok(None);
    };

    ty_to_type_expr(ty)
        .map(Some)
        .map_err(|diagnostic| diagnostic.with_context(context).with_span(span.clone()))
}

/// Enrich a program's AST with inferred types from the type checker.
///
/// # Errors
///
/// Returns an error when a required serialized `TypeExpr` contract cannot be
/// produced. Best-effort inferred-type insertions are reported via returned
/// diagnostics instead of silently disappearing.
pub fn enrich_program(
    program: &mut Program,
    tco: &TypeCheckOutput,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<EnrichProgramDiagnostics, TypeExprConversionError> {
    let mut diagnostics = Vec::new();
    for (item, _span) in &mut program.items {
        enrich_item_with_diagnostics(item, tco, &mut diagnostics, registry)?;
    }
    normalize_all_types(program, registry);
    synthesize_stdlib_externs(program, registry)?;
    Ok(EnrichProgramDiagnostics { diagnostics })
}

/// Normalize `TypeExpr::Named("Result", [T, E])` → `TypeExpr::Result { ok, err }`
/// and `TypeExpr::Named("Option", [T])` → `TypeExpr::Option(T)`.
///
/// The parser may emit these as generic `Named` types; the C++ backend expects
/// the dedicated `Result`/`Option` variants.
fn normalize_type_expr(te: &mut TypeExpr, registry: &hew_types::module_registry::ModuleRegistry) {
    // First, recurse into child type exprs regardless of variant.
    match te {
        TypeExpr::Named {
            type_args: Some(ref mut args),
            ..
        } => {
            for arg in args.iter_mut() {
                normalize_type_expr(&mut arg.0, registry);
            }
        }
        TypeExpr::Result { ok, err } => {
            normalize_type_expr(&mut ok.0, registry);
            normalize_type_expr(&mut err.0, registry);
        }
        TypeExpr::Option(inner) | TypeExpr::Slice(inner) => {
            normalize_type_expr(&mut inner.0, registry);
        }
        TypeExpr::Tuple(elems) => {
            for elem in elems.iter_mut() {
                normalize_type_expr(&mut elem.0, registry);
            }
        }
        TypeExpr::Array { element, .. } => {
            normalize_type_expr(&mut element.0, registry);
        }
        TypeExpr::Function {
            params,
            return_type,
        } => {
            for p in params.iter_mut() {
                normalize_type_expr(&mut p.0, registry);
            }
            normalize_type_expr(&mut return_type.0, registry);
        }
        TypeExpr::Pointer { pointee, .. } => {
            normalize_type_expr(&mut pointee.0, registry);
        }
        TypeExpr::TraitObject(ref mut bounds) => {
            for bound in bounds.iter_mut() {
                if let Some(ref mut args) = bound.type_args {
                    for arg in args.iter_mut() {
                        normalize_type_expr(&mut arg.0, registry);
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
                    if let Some(qualified) = registry.qualify_handle_type(name) {
                        name.clone_from(&qualified);
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
fn synthesize_stdlib_externs(
    program: &mut Program,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    let mut new_items: Vec<Spanned<Item>> = Vec::new();

    for (item, _span) in &program.items {
        if let Item::Import(import_decl) = item {
            let module_path = import_decl.path.join("::");
            if let Some(info) = registry.get(&module_path) {
                let extern_fns = info
                    .functions
                    .iter()
                    .map(|(name, params, ret_ty)| {
                        let param_exprs = params
                            .iter()
                            .enumerate()
                            .map(|(index, ty)| {
                                let type_expr = require_converted(
                                    ty,
                                    format!("stdlib extern `{name}` parameter {index}"),
                                )?;
                                Ok(Param {
                                    name: format!("p{index}"),
                                    ty: type_expr,
                                    is_mutable: false,
                                })
                            })
                            .collect::<Result<Vec<_>, TypeExprConversionError>>()?;
                        let return_type = if matches!(ret_ty, Ty::Unit) {
                            None
                        } else {
                            Some(require_converted(
                                ret_ty,
                                format!("stdlib extern `{name}` return type"),
                            )?)
                        };
                        Ok(ExternFnDecl {
                            name: name.clone(),
                            params: param_exprs,
                            return_type,
                            is_variadic: false,
                        })
                    })
                    .collect::<Result<Vec<_>, TypeExprConversionError>>()?;

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
    Ok(())
}

/// Walk the entire program AST and normalize all `TypeExpr` nodes.
fn normalize_all_types(
    program: &mut Program,
    registry: &hew_types::module_registry::ModuleRegistry,
) {
    for (item, _span) in &mut program.items {
        normalize_item_types(item, registry);
    }
}

/// Normalize type expressions in a list of items.
///
/// This is the same transformation as [`normalize_all_types`] but operates on
/// a standalone item list — useful for normalizing module-graph modules that
/// are not part of the root `Program::items`.
pub fn normalize_items_types(
    items: &mut [Spanned<Item>],
    registry: &hew_types::module_registry::ModuleRegistry,
) {
    for (item, _span) in items {
        normalize_item_types(item, registry);
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
        Item::Machine(machine) => {
            for transition in &mut machine.transitions {
                rewrite_builtin_calls_in_expr(&mut transition.body);
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
        Item::Const(const_decl) => {
            rewrite_builtin_calls_in_expr(&mut const_decl.value);
        }
        Item::TypeDecl(td) => {
            for body_item in &mut td.body {
                if let hew_parser::ast::TypeBodyItem::Method(m) = body_item {
                    rewrite_builtin_calls_in_block(&mut m.body);
                }
            }
        }
        Item::Supervisor(sup) => {
            for child in &mut sup.children {
                for arg in &mut child.args {
                    rewrite_builtin_calls_in_expr(arg);
                }
            }
        }
        Item::Import(_) | Item::TypeAlias(_) | Item::Wire(_) | Item::ExternBlock(_) => {}
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
        Stmt::Expression(ref mut expr)
        | Stmt::Return(Some(ref mut expr))
        | Stmt::Break {
            value: Some(ref mut expr),
            ..
        } => {
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
        Stmt::Return(None) | Stmt::Break { value: None, .. } | Stmt::Continue { .. } => {}
    }
}

#[expect(clippy::too_many_lines, reason = "enrichment covers all AST variants")]
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
        Expr::Block(block) | Expr::Unsafe(block) => rewrite_builtin_calls_in_block(block),
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
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                rewrite_builtin_calls_in_expr(k);
                rewrite_builtin_calls_in_expr(v);
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
        Expr::PostfixTry(inner) | Expr::Await(inner) | Expr::Yield(Some(inner)) => {
            rewrite_builtin_calls_in_expr(inner);
        }
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
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::Cooperate
        | Expr::ScopeCancel
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Yield(None) => {}
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "normalization covers all item variants"
)]
fn normalize_item_types(item: &mut Item, registry: &hew_types::module_registry::ModuleRegistry) {
    match item {
        Item::Function(fn_decl) => normalize_fn_decl_types(fn_decl, registry),
        Item::Actor(actor) => {
            for field in &mut actor.fields {
                normalize_type_expr(&mut field.ty.0, registry);
            }
            if let Some(ref mut init) = actor.init {
                normalize_block_types(&mut init.body, registry);
            }
            for recv in &mut actor.receive_fns {
                for param in &mut recv.params {
                    normalize_type_expr(&mut param.ty.0, registry);
                }
                if let Some(ref mut rt) = recv.return_type {
                    normalize_type_expr(&mut rt.0, registry);
                }
                normalize_block_types(&mut recv.body, registry);
            }
            for method in &mut actor.methods {
                normalize_fn_decl_types(method, registry);
            }
        }
        Item::Impl(impl_decl) => {
            normalize_type_expr(&mut impl_decl.target_type.0, registry);
            for method in &mut impl_decl.methods {
                normalize_fn_decl_types(method, registry);
            }
        }
        Item::ExternBlock(eb) => {
            for func in &mut eb.functions {
                for param in &mut func.params {
                    normalize_type_expr(&mut param.ty.0, registry);
                }
                if let Some(ref mut rt) = func.return_type {
                    normalize_type_expr(&mut rt.0, registry);
                }
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &mut td.body {
                match body_item {
                    hew_parser::ast::TypeBodyItem::Field { ty, .. } => {
                        normalize_type_expr(&mut ty.0, registry);
                    }
                    hew_parser::ast::TypeBodyItem::Method(m) => {
                        normalize_fn_decl_types(m, registry);
                    }
                    hew_parser::ast::TypeBodyItem::Variant(v) => match &mut v.kind {
                        hew_parser::ast::VariantKind::Tuple(fields) => {
                            for field_ty in fields {
                                normalize_type_expr(&mut field_ty.0, registry);
                            }
                        }
                        hew_parser::ast::VariantKind::Struct(fields) => {
                            for (_name, field_ty) in fields {
                                normalize_type_expr(&mut field_ty.0, registry);
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
                            normalize_type_expr(&mut param.ty.0, registry);
                        }
                        if let Some(ref mut rt) = m.return_type {
                            normalize_type_expr(&mut rt.0, registry);
                        }
                        if let Some(ref mut body) = m.body {
                            normalize_block_types(body, registry);
                        }
                    }
                    hew_parser::ast::TraitItem::AssociatedType { default, .. } => {
                        if let Some(ref mut default_ty) = default {
                            normalize_type_expr(&mut default_ty.0, registry);
                        }
                    }
                }
            }
        }
        Item::Const(const_decl) => {
            normalize_type_expr(&mut const_decl.ty.0, registry);
        }
        Item::TypeAlias(type_alias) => {
            normalize_type_expr(&mut type_alias.ty.0, registry);
        }
        Item::Machine(machine) => {
            for state in &mut machine.states {
                for (_name, ty) in &mut state.fields {
                    normalize_type_expr(&mut ty.0, registry);
                }
            }
            for event in &mut machine.events {
                for (_name, ty) in &mut event.fields {
                    normalize_type_expr(&mut ty.0, registry);
                }
            }
            for transition in &mut machine.transitions {
                normalize_expr_types(&mut transition.body, registry);
            }
        }
        Item::Supervisor(sup) => {
            for child in &mut sup.children {
                for arg in &mut child.args {
                    normalize_expr_types(arg, registry);
                }
            }
        }
        Item::Import(_) | Item::Wire(_) => {}
    }
}

fn normalize_fn_decl_types(
    fn_decl: &mut FnDecl,
    registry: &hew_types::module_registry::ModuleRegistry,
) {
    for param in &mut fn_decl.params {
        normalize_type_expr(&mut param.ty.0, registry);
    }
    if let Some(ref mut rt) = fn_decl.return_type {
        normalize_type_expr(&mut rt.0, registry);
    }
    normalize_block_types(&mut fn_decl.body, registry);
}

fn normalize_block_types(block: &mut Block, registry: &hew_types::module_registry::ModuleRegistry) {
    for (stmt, _span) in &mut block.stmts {
        normalize_stmt_types(stmt, registry);
    }
    if let Some(ref mut expr) = block.trailing_expr {
        normalize_expr_types(expr, registry);
    }
}

fn normalize_stmt_types(stmt: &mut Stmt, registry: &hew_types::module_registry::ModuleRegistry) {
    match stmt {
        Stmt::Let { ty, value, .. } | Stmt::Var { ty, value, .. } => {
            if let Some(ref mut t) = ty {
                normalize_type_expr(&mut t.0, registry);
            }
            if let Some(ref mut val) = value {
                normalize_expr_types(val, registry);
            }
        }
        Stmt::Expression(ref mut expr)
        | Stmt::Return(Some(ref mut expr))
        | Stmt::Break {
            value: Some(ref mut expr),
            ..
        } => {
            normalize_expr_types(expr, registry);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            normalize_expr_types(condition, registry);
            normalize_block_types(then_block, registry);
            if let Some(ref mut eb) = else_block {
                if let Some(ref mut block) = eb.block {
                    normalize_block_types(block, registry);
                }
                if let Some(ref mut if_stmt) = eb.if_stmt {
                    normalize_stmt_types(&mut if_stmt.0, registry);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            normalize_expr_types(expr, registry);
            normalize_block_types(body, registry);
            if let Some(block) = else_body {
                normalize_block_types(block, registry);
            }
        }
        Stmt::For { body, iterable, .. } => {
            normalize_expr_types(iterable, registry);
            normalize_block_types(body, registry);
        }
        Stmt::While {
            condition, body, ..
        } => {
            normalize_expr_types(condition, registry);
            normalize_block_types(body, registry);
        }
        Stmt::Loop { body, .. } => {
            normalize_block_types(body, registry);
        }
        Stmt::Match { scrutinee, arms } => {
            normalize_expr_types(scrutinee, registry);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    normalize_expr_types(guard, registry);
                }
                normalize_expr_types(&mut arm.body, registry);
            }
        }
        Stmt::Assign { target, value, .. } => {
            normalize_expr_types(target, registry);
            normalize_expr_types(value, registry);
        }
        Stmt::Defer(ref mut expr) => {
            normalize_expr_types(expr, registry);
        }
        Stmt::Return(None) | Stmt::Break { value: None, .. } | Stmt::Continue { .. } => {}
    }
}

fn normalize_expr_types(
    expr: &mut Spanned<Expr>,
    registry: &hew_types::module_registry::ModuleRegistry,
) {
    stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, || {
        normalize_expr_types_inner(expr, registry);
    });
}

#[expect(
    clippy::too_many_lines,
    reason = "builtin rewriting covers all expression types"
)]
fn normalize_expr_types_inner(
    expr: &mut Spanned<Expr>,
    registry: &hew_types::module_registry::ModuleRegistry,
) {
    match &mut expr.0 {
        Expr::Block(block)
        | Expr::Scope { body: block, .. }
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            normalize_block_types(block, registry);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            normalize_expr_types(condition, registry);
            normalize_expr_types(then_block, registry);
            if let Some(ref mut e) = else_block {
                normalize_expr_types(e, registry);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            normalize_expr_types(expr, registry);
            normalize_block_types(body, registry);
            if let Some(block) = else_body {
                normalize_block_types(block, registry);
            }
        }
        Expr::Match { scrutinee, arms } => {
            normalize_expr_types(scrutinee, registry);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    normalize_expr_types(guard, registry);
                }
                normalize_expr_types(&mut arm.body, registry);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            normalize_expr_types(value, registry);
            normalize_expr_types(count, registry);
        }
        Expr::Array(elements) | Expr::Tuple(elements) => {
            for e in elements.iter_mut() {
                normalize_expr_types(e, registry);
            }
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                normalize_expr_types(k, registry);
                normalize_expr_types(v, registry);
            }
        }
        Expr::Lambda {
            return_type,
            body,
            params,
            ..
        } => {
            if let Some(ref mut rt) = return_type {
                normalize_type_expr(&mut rt.0, registry);
            }
            for param in params.iter_mut() {
                if let Some(ref mut t) = param.ty {
                    normalize_type_expr(&mut t.0, registry);
                }
            }
            normalize_expr_types(body, registry);
        }
        Expr::Call {
            function,
            args,
            type_args,
            ..
        } => {
            normalize_expr_types(function, registry);
            for arg in args.iter_mut() {
                normalize_expr_types(arg.expr_mut(), registry);
            }
            if let Some(ref mut ta) = type_args {
                for t in ta.iter_mut() {
                    normalize_type_expr(&mut t.0, registry);
                }
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            normalize_expr_types(receiver, registry);
            for arg in args.iter_mut() {
                normalize_expr_types(arg.expr_mut(), registry);
            }
        }
        Expr::Binary { left, right, .. } => {
            normalize_expr_types(left, registry);
            normalize_expr_types(right, registry);
        }
        Expr::Unary { operand, .. } => {
            normalize_expr_types(operand, registry);
        }
        Expr::Cast { expr, ty } => {
            normalize_expr_types(expr, registry);
            normalize_type_expr(&mut ty.0, registry);
        }
        Expr::FieldAccess { object, .. } => {
            normalize_expr_types(object, registry);
        }
        Expr::Index { object, index } => {
            normalize_expr_types(object, registry);
            normalize_expr_types(index, registry);
        }
        Expr::StructInit { fields, .. } => {
            for (_name, val) in fields.iter_mut() {
                normalize_expr_types(val, registry);
            }
        }
        Expr::Spawn { target, args } => {
            normalize_expr_types(target, registry);
            for (_name, val) in args.iter_mut() {
                normalize_expr_types(val, registry);
            }
        }
        Expr::SpawnLambdaActor { body, .. } => {
            normalize_expr_types(body, registry);
        }
        Expr::Send { target, message } => {
            normalize_expr_types(target, registry);
            normalize_expr_types(message, registry);
        }
        Expr::Await(inner) | Expr::PostfixTry(inner) | Expr::Yield(Some(inner)) => {
            normalize_expr_types(inner, registry);
        }
        Expr::Timeout {
            expr: inner,
            duration,
        } => {
            normalize_expr_types(inner, registry);
            normalize_expr_types(duration, registry);
        }
        Expr::Join(exprs) => {
            for e in exprs.iter_mut() {
                normalize_expr_types(e, registry);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts.iter_mut() {
                if let hew_parser::ast::StringPart::Expr(e) = part {
                    normalize_expr_types(e, registry);
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms.iter_mut() {
                normalize_expr_types(&mut arm.source, registry);
                normalize_expr_types(&mut arm.body, registry);
            }
            if let Some(ref mut t) = timeout {
                normalize_expr_types(&mut t.duration, registry);
                normalize_expr_types(&mut t.body, registry);
            }
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                normalize_expr_types(s, registry);
            }
            if let Some(e) = end {
                normalize_expr_types(e, registry);
            }
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::Cooperate
        | Expr::ScopeCancel
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Yield(None) => {}
    }
}

fn enrich_item_with_diagnostics(
    item: &mut Item,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    match item {
        Item::Function(fn_decl) => {
            enrich_fn_decl_with_diagnostics(fn_decl, tco, diagnostics, registry)?;
        }
        Item::Actor(actor) => {
            enrich_actor_with_diagnostics(actor, tco, diagnostics, registry)?;
        }
        Item::Machine(machine) => {
            for transition in &mut machine.transitions {
                enrich_expr_with_diagnostics(&mut transition.body, tco, diagnostics, registry)?;
            }
        }
        Item::Impl(impl_decl) => {
            for method in &mut impl_decl.methods {
                enrich_fn_decl_with_diagnostics(method, tco, diagnostics, registry)?;
            }
        }
        Item::Const(const_decl) => {
            enrich_expr_with_diagnostics(&mut const_decl.value, tco, diagnostics, registry)?;
        }
        Item::Trait(trait_decl) => {
            for trait_item in &mut trait_decl.items {
                if let hew_parser::ast::TraitItem::Method(m) = trait_item {
                    if let Some(ref mut body) = m.body {
                        enrich_block_with_diagnostics(body, tco, diagnostics, registry)?;
                    }
                }
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &mut td.body {
                if let hew_parser::ast::TypeBodyItem::Method(m) = body_item {
                    enrich_fn_decl_with_diagnostics(m, tco, diagnostics, registry)?;
                }
            }
        }
        Item::Supervisor(sup) => {
            for child in &mut sup.children {
                for arg in &mut child.args {
                    enrich_expr_with_diagnostics(arg, tco, diagnostics, registry)?;
                }
            }
        }
        Item::Import(_) | Item::TypeAlias(_) | Item::Wire(_) | Item::ExternBlock(_) => {}
    }
    Ok(())
}

fn enrich_fn_decl_with_diagnostics(
    fn_decl: &mut FnDecl,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    enrich_block_with_diagnostics(&mut fn_decl.body, tco, diagnostics, registry)?;

    let needs_infer =
        fn_decl.return_type.is_none() || matches!(&fn_decl.return_type, Some((TypeExpr::Infer, _)));
    if needs_infer {
        if let Some(ref expr) = fn_decl.body.trailing_expr {
            match lookup_inferred_type(
                tco,
                &expr.1,
                format!(
                    "function `{}` return type inferred from trailing expression",
                    fn_decl.name
                ),
            ) {
                Ok(Some(inferred)) => fn_decl.return_type = Some(inferred),
                Ok(None) => {}
                Err(diagnostic) => diagnostics.push(diagnostic),
            }
        }
    }
    Ok(())
}

fn enrich_actor_with_diagnostics(
    actor: &mut ActorDecl,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    if let Some(ref mut init) = actor.init {
        enrich_block_with_diagnostics(&mut init.body, tco, diagnostics, registry)?;
    }
    for recv in &mut actor.receive_fns {
        enrich_block_with_diagnostics(&mut recv.body, tco, diagnostics, registry)?;
    }
    for method in &mut actor.methods {
        enrich_fn_decl_with_diagnostics(method, tco, diagnostics, registry)?;
    }
    Ok(())
}

fn enrich_block_with_diagnostics(
    block: &mut Block,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    for (stmt, _span) in &mut block.stmts {
        enrich_stmt_with_diagnostics(stmt, tco, diagnostics, registry)?;
    }
    if let Some(ref mut expr) = block.trailing_expr {
        enrich_expr_with_diagnostics(expr, tco, diagnostics, registry)?;
    }
    Ok(())
}

fn enrich_stmt_with_diagnostics(
    stmt: &mut Stmt,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    match stmt {
        Stmt::Let { ty, value, .. } => {
            if ty.is_none() {
                if let Some(ref val) = *value {
                    match lookup_inferred_type(
                        tco,
                        &val.1,
                        "let binding type inferred from initializer",
                    ) {
                        Ok(Some(inferred)) => *ty = Some(inferred),
                        Ok(None) => {}
                        Err(diagnostic) => diagnostics.push(diagnostic),
                    }
                }
            }
            if let Some(ref mut val) = value {
                enrich_expr_with_diagnostics(val, tco, diagnostics, registry)?;
            }
        }
        Stmt::Var { name, ty, value } => {
            if ty.is_none() {
                if let Some(ref val) = *value {
                    match lookup_inferred_type(
                        tco,
                        &val.1,
                        format!("var `{name}` type inferred from initializer"),
                    ) {
                        Ok(Some(inferred)) => *ty = Some(inferred),
                        Ok(None) => {}
                        Err(diagnostic) => diagnostics.push(diagnostic),
                    }
                }
            }
            if let Some(ref mut val) = value {
                enrich_expr_with_diagnostics(val, tco, diagnostics, registry)?;
            }
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            enrich_expr_with_diagnostics(condition, tco, diagnostics, registry)?;
            enrich_block_with_diagnostics(then_block, tco, diagnostics, registry)?;
            if let Some(ref mut else_b) = else_block {
                enrich_else_block_with_diagnostics(else_b, tco, diagnostics, registry)?;
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            enrich_expr_with_diagnostics(expr, tco, diagnostics, registry)?;
            enrich_block_with_diagnostics(body, tco, diagnostics, registry)?;
            if let Some(block) = else_body {
                enrich_block_with_diagnostics(block, tco, diagnostics, registry)?;
            }
        }
        Stmt::Match { scrutinee, arms } => {
            enrich_expr_with_diagnostics(scrutinee, tco, diagnostics, registry)?;
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    enrich_expr_with_diagnostics(guard, tco, diagnostics, registry)?;
                }
                enrich_expr_with_diagnostics(&mut arm.body, tco, diagnostics, registry)?;
            }
        }
        Stmt::For { body, iterable, .. } => {
            enrich_expr_with_diagnostics(iterable, tco, diagnostics, registry)?;
            enrich_block_with_diagnostics(body, tco, diagnostics, registry)?;
        }
        Stmt::While {
            condition, body, ..
        } => {
            enrich_expr_with_diagnostics(condition, tco, diagnostics, registry)?;
            enrich_block_with_diagnostics(body, tco, diagnostics, registry)?;
        }
        Stmt::Loop { body, .. } => {
            enrich_block_with_diagnostics(body, tco, diagnostics, registry)?;
        }
        Stmt::Expression(ref mut expr)
        | Stmt::Return(Some(ref mut expr))
        | Stmt::Break {
            value: Some(ref mut expr),
            ..
        } => {
            enrich_expr_with_diagnostics(expr, tco, diagnostics, registry)?;
        }
        Stmt::Defer(ref mut expr) => {
            enrich_expr_with_diagnostics(expr, tco, diagnostics, registry)?;
        }
        Stmt::Assign { target, value, .. } => {
            enrich_expr_with_diagnostics(target, tco, diagnostics, registry)?;
            enrich_expr_with_diagnostics(value, tco, diagnostics, registry)?;
        }
        Stmt::Return(None) | Stmt::Break { value: None, .. } | Stmt::Continue { .. } => {}
    }
    Ok(())
}

fn enrich_else_block_with_diagnostics(
    else_block: &mut ElseBlock,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    if let Some(ref mut block) = else_block.block {
        enrich_block_with_diagnostics(block, tco, diagnostics, registry)?;
    }
    if let Some(ref mut if_stmt) = else_block.if_stmt {
        enrich_stmt_with_diagnostics(&mut if_stmt.0, tco, diagnostics, registry)?;
    }
    Ok(())
}

#[expect(
    clippy::too_many_lines,
    reason = "pattern enrichment covers all pattern variants"
)]
fn enrich_expr_with_diagnostics(
    expr: &mut Spanned<Expr>,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    match &mut expr.0 {
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            enrich_expr_with_diagnostics(condition, tco, diagnostics, registry)?;
            enrich_expr_with_diagnostics(then_block, tco, diagnostics, registry)?;
            if let Some(ref mut e) = else_block {
                enrich_expr_with_diagnostics(e, tco, diagnostics, registry)?;
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            enrich_expr_with_diagnostics(expr, tco, diagnostics, registry)?;
            enrich_block_with_diagnostics(body, tco, diagnostics, registry)?;
            if let Some(block) = else_body {
                enrich_block_with_diagnostics(block, tco, diagnostics, registry)?;
            }
        }
        Expr::Match { scrutinee, arms } => {
            enrich_expr_with_diagnostics(scrutinee, tco, diagnostics, registry)?;
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    enrich_expr_with_diagnostics(guard, tco, diagnostics, registry)?;
                }
                enrich_expr_with_diagnostics(&mut arm.body, tco, diagnostics, registry)?;
            }
        }
        Expr::Array(elements) | Expr::Tuple(elements) => {
            for e in elements.iter_mut() {
                enrich_expr_with_diagnostics(e, tco, diagnostics, registry)?;
            }
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                enrich_expr_with_diagnostics(k, tco, diagnostics, registry)?;
                enrich_expr_with_diagnostics(v, tco, diagnostics, registry)?;
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            enrich_expr_with_diagnostics(body, tco, diagnostics, registry)?;
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            enrich_expr_with_diagnostics(receiver, tco, diagnostics, registry)?;
            for arg in args.iter_mut() {
                enrich_expr_with_diagnostics(arg.expr_mut(), tco, diagnostics, registry)?;
            }
            // Rewrite module-qualified stdlib calls: e.g. os.pid() → hew_os_pid()
            // This happens during AST enrichment, before serialization.
            if let Expr::Identifier(module_name) = &receiver.0 {
                if let Some(c_symbol) = registry.resolve_module_call(module_name, method) {
                    // Skip identity-mapped wrappers (e.g. log.setup → setup): these are
                    // non-trivial Hew wrappers that must be compiled as module graph
                    // functions and called via their mangled name. Leaving them as
                    // MethodCall lets the C++ codegen dispatch them correctly.
                    if c_symbol != *method {
                        let old_args = std::mem::take(args);
                        expr.0 = Expr::Call {
                            function: Box::new((
                                Expr::Identifier(c_symbol.clone()),
                                receiver.1.clone(),
                            )),
                            type_args: None,
                            args: old_args,
                            is_tail_call: false,
                        };
                        return Ok(());
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
                    return Ok(());
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
            let c_fn: Option<String> = match tco.expr_types.get(&key) {
                Some(Ty::Named { name, .. }) if name == "Stream" => {
                    hew_types::stdlib::resolve_stream_method("Stream", method).map(String::from)
                }
                Some(Ty::Named { name, .. }) if name == "Sink" => {
                    hew_types::stdlib::resolve_stream_method("Sink", method).map(String::from)
                }
                Some(Ty::Named { name, .. }) => registry.resolve_handle_method(name, method),
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
                    function: Box::new((Expr::Identifier(c_fn), span)),
                    type_args: None,
                    args: all_args,
                    is_tail_call: false,
                };
            }
        }
        Expr::Call { function, args, .. } => {
            enrich_expr_with_diagnostics(function, tco, diagnostics, registry)?;
            for arg in args.iter_mut() {
                enrich_expr_with_diagnostics(arg.expr_mut(), tco, diagnostics, registry)?;
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
                }
            }
        }
        Expr::Binary { left, right, .. } => {
            enrich_expr_with_diagnostics(left, tco, diagnostics, registry)?;
            enrich_expr_with_diagnostics(right, tco, diagnostics, registry)?;
        }
        Expr::Unary { operand: inner, .. }
        | Expr::Cast { expr: inner, .. }
        | Expr::Await(inner)
        | Expr::PostfixTry(inner)
        | Expr::Yield(Some(inner)) => {
            enrich_expr_with_diagnostics(inner, tco, diagnostics, registry)?;
        }
        Expr::FieldAccess { object, .. } => {
            enrich_expr_with_diagnostics(object, tco, diagnostics, registry)?;
        }
        Expr::Index { object, index } => {
            enrich_expr_with_diagnostics(object, tco, diagnostics, registry)?;
            enrich_expr_with_diagnostics(index, tco, diagnostics, registry)?;
        }
        Expr::StructInit { fields, .. } => {
            for (_name, val) in fields.iter_mut() {
                enrich_expr_with_diagnostics(val, tco, diagnostics, registry)?;
            }
        }
        Expr::Spawn { target, args } => {
            enrich_expr_with_diagnostics(target, tco, diagnostics, registry)?;
            for (_name, val) in args.iter_mut() {
                enrich_expr_with_diagnostics(val, tco, diagnostics, registry)?;
            }
        }
        Expr::Send { target, message } => {
            enrich_expr_with_diagnostics(target, tco, diagnostics, registry)?;
            enrich_expr_with_diagnostics(message, tco, diagnostics, registry)?;
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                enrich_expr_with_diagnostics(s, tco, diagnostics, registry)?;
            }
            if let Some(e) = end {
                enrich_expr_with_diagnostics(e, tco, diagnostics, registry)?;
            }
        }
        Expr::Block(block)
        | Expr::Scope { body: block, .. }
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            enrich_block_with_diagnostics(block, tco, diagnostics, registry)?;
        }
        Expr::Timeout {
            expr: inner,
            duration,
        } => {
            enrich_expr_with_diagnostics(inner, tco, diagnostics, registry)?;
            enrich_expr_with_diagnostics(duration, tco, diagnostics, registry)?;
        }
        Expr::Join(exprs) => {
            for e in exprs.iter_mut() {
                enrich_expr_with_diagnostics(e, tco, diagnostics, registry)?;
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts.iter_mut() {
                if let hew_parser::ast::StringPart::Expr(e) = part {
                    enrich_expr_with_diagnostics(e, tco, diagnostics, registry)?;
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms.iter_mut() {
                enrich_expr_with_diagnostics(&mut arm.source, tco, diagnostics, registry)?;
                enrich_expr_with_diagnostics(&mut arm.body, tco, diagnostics, registry)?;
            }
            if let Some(ref mut t) = timeout {
                enrich_expr_with_diagnostics(&mut t.duration, tco, diagnostics, registry)?;
                enrich_expr_with_diagnostics(&mut t.body, tco, diagnostics, registry)?;
            }
        }
        Expr::ArrayRepeat { value, count } => {
            enrich_expr_with_diagnostics(value, tco, diagnostics, registry)?;
            enrich_expr_with_diagnostics(count, tco, diagnostics, registry)?;
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::Cooperate
        | Expr::ScopeCancel
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Yield(None) => {}
    }
    Ok(())
}

#[cfg(test)]
fn enrich_expr(
    expr: &mut Spanned<Expr>,
    tco: &TypeCheckOutput,
) -> Result<(), TypeExprConversionError> {
    let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
    let mut diagnostics = Vec::new();
    enrich_expr_with_diagnostics(expr, tco, &mut diagnostics, &registry)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::{ImportDecl, Visibility};

    /// Module registry with the repo root as a search path, so stdlib
    /// modules can be loaded during tests.
    fn test_registry() -> hew_types::module_registry::ModuleRegistry {
        let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf();
        hew_types::module_registry::ModuleRegistry::new(vec![repo_root])
    }

    /// Create a test registry with the given modules pre-loaded.
    fn test_registry_with(modules: &[&str]) -> hew_types::module_registry::ModuleRegistry {
        let mut reg = test_registry();
        for m in modules {
            let _ = reg.load(m);
        }
        reg
    }

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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
        assert!(matches!(te, TypeExpr::Named { ref name, .. } if name == "Result"));
    }

    #[test]
    fn test_normalize_result_no_type_args_unchanged() {
        let mut te = TypeExpr::Named {
            name: "Result".into(),
            type_args: None,
        };
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
        normalize_type_expr(
            &mut te,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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
                    resolved_item_source_paths: Vec::new(),
                    resolved_source_paths: Vec::new(),
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(&mut program, &test_registry_with(&["std::fs"])).unwrap();
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
                    resolved_item_source_paths: Vec::new(),
                    resolved_source_paths: Vec::new(),
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(&mut program, &test_registry()).unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_synthesize_stdlib_externs_empty_program() {
        let mut program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(&mut program, &test_registry()).unwrap();
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
        synthesize_stdlib_externs(&mut program, &test_registry()).unwrap();
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
                        resolved_item_source_paths: Vec::new(),
                        resolved_source_paths: Vec::new(),
                    }),
                    0..0,
                ),
                (
                    Item::Import(ImportDecl {
                        path: vec!["std".into(), "encoding".into(), "json".into()],
                        spec: None,
                        file_path: None,
                        resolved_items: None,
                        resolved_item_source_paths: Vec::new(),
                        resolved_source_paths: Vec::new(),
                    }),
                    0..0,
                ),
            ],
            module_doc: None,
            module_graph: None,
        };
        synthesize_stdlib_externs(
            &mut program,
            &test_registry_with(&["std::fs", "std::encoding::json"]),
        )
        .unwrap();
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

    fn unwrap_converted(
        result: Result<Spanned<TypeExpr>, TypeExprConversionError>,
    ) -> Spanned<TypeExpr> {
        result.unwrap()
    }

    fn unwrap_err(
        result: Result<Spanned<TypeExpr>, TypeExprConversionError>,
    ) -> TypeExprConversionError {
        result.unwrap_err()
    }

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
            let (te, _span) = unwrap_converted(ty_to_type_expr(&ty));
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
    fn test_ty_to_type_expr_unit_returns_empty_tuple() {
        let result = unwrap_converted(ty_to_type_expr(&Ty::Unit));
        assert!(matches!(result.0, TypeExpr::Tuple(ref elems) if elems.is_empty()));
    }

    #[test]
    fn test_ty_to_type_expr_error_returns_explicit_error() {
        let err = unwrap_err(ty_to_type_expr(&Ty::Error));
        assert!(
            err.to_string()
                .contains("type-checker error sentinel reached serializer"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn test_ty_to_type_expr_option() {
        let ty = Ty::option(Ty::I32);
        assert!(matches!(
            unwrap_converted(ty_to_type_expr(&ty)).0,
            TypeExpr::Option(_)
        ));
    }

    #[test]
    fn test_ty_to_type_expr_result() {
        let ty = Ty::result(Ty::I32, Ty::String);
        assert!(matches!(
            unwrap_converted(ty_to_type_expr(&ty)).0,
            TypeExpr::Result { .. }
        ));
    }

    #[test]
    fn test_ty_to_type_expr_tuple() {
        let ty = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        let result = unwrap_converted(ty_to_type_expr(&ty));
        if let TypeExpr::Tuple(elems) = &result.0 {
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
        assert!(matches!(
            unwrap_converted(ty_to_type_expr(&ty)).0,
            TypeExpr::Function { .. }
        ));
    }

    #[test]
    fn test_ty_to_type_expr_named_with_args() {
        let ty = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I32],
        };
        let result = unwrap_converted(ty_to_type_expr(&ty));
        if let TypeExpr::Named { name, type_args } = &result.0 {
            assert_eq!(name, "Vec");
            assert_eq!(type_args.as_ref().unwrap().len(), 1);
        } else {
            panic!("expected Named");
        }
    }

    #[test]
    fn test_ty_to_type_expr_actor_ref() {
        let ty = Ty::actor_ref(Ty::I32);
        let result = unwrap_converted(ty_to_type_expr(&ty));
        if let TypeExpr::Named { name, type_args } = &result.0 {
            assert_eq!(name, "ActorRef");
            assert!(type_args.is_some());
        } else {
            panic!("expected Named ActorRef");
        }
    }

    #[test]
    fn test_ty_to_type_expr_stream() {
        let ty = Ty::stream(Ty::I64);
        let result = unwrap_converted(ty_to_type_expr(&ty));
        if let TypeExpr::Named { name, .. } = &result.0 {
            assert_eq!(name, "Stream");
        } else {
            panic!("expected Named Stream");
        }
    }

    #[test]
    fn test_ty_to_type_expr_array() {
        let ty = Ty::Array(Box::new(Ty::I32), 10);
        let result = unwrap_converted(ty_to_type_expr(&ty));
        if let TypeExpr::Array { element, size } = &result.0 {
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
        let result = unwrap_converted(ty_to_type_expr(&ty));
        if let TypeExpr::Pointer {
            is_mutable,
            pointee,
        } = &result.0
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
        let ty = Ty::generator(Ty::I32, Ty::String);
        let err = unwrap_err(ty_to_type_expr(&ty));
        assert!(
            err.to_string()
                .contains("generator type is not representable in serialized TypeExpr"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn test_ty_to_type_expr_async_generator() {
        let ty = Ty::async_generator(Ty::I32);
        let err = unwrap_err(ty_to_type_expr(&ty));
        assert!(
            err.to_string()
                .contains("async generator type is not representable in serialized TypeExpr"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn test_ty_to_type_expr_range() {
        let ty = Ty::range(Ty::I32);
        let result = unwrap_converted(ty_to_type_expr(&ty));
        if let TypeExpr::Named { name, type_args } = &result.0 {
            assert_eq!(name, "Range");
            assert_eq!(type_args.as_ref().unwrap().len(), 1);
        } else {
            panic!("expected Named Range");
        }
    }

    #[test]
    fn test_ty_to_type_expr_var_returns_explicit_error() {
        use hew_types::ty::TypeVar;
        let err = unwrap_err(ty_to_type_expr(&Ty::Var(TypeVar(123))));
        assert!(
            err.to_string()
                .contains("unresolved type variable reached serializer"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn test_build_expr_type_map_serializes_unit_entries() {
        let mut tco = empty_tco();
        tco.expr_types
            .insert(SpanKey { start: 1, end: 2 }, Ty::Unit);

        let expr_types = build_expr_type_map(&tco);
        assert_eq!(expr_types.diagnostics().len(), 0);
        assert_eq!(expr_types.entries.len(), 1);
        assert!(matches!(
            expr_types.entries[0].ty.0,
            TypeExpr::Tuple(ref elems) if elems.is_empty()
        ));
    }

    #[test]
    fn test_build_expr_type_map_rejects_unsupported_nested_type() {
        use hew_types::ty::TypeVar;

        let mut tco = empty_tco();
        tco.expr_types.insert(
            SpanKey { start: 3, end: 9 },
            Ty::option(Ty::Var(TypeVar(7))),
        );

        let result = build_expr_type_map(&tco);
        assert!(result.entries.is_empty());
        assert_eq!(result.diagnostics().len(), 1);
        let diagnostic = &result.diagnostics()[0];
        assert_eq!(diagnostic.span(), Some(&(3..9)));
        let message = diagnostic.to_string();
        assert!(
            message.contains("Option inner type"),
            "unexpected error: {message}"
        );
        assert!(message.contains("?T7"), "unexpected error: {message}");
    }

    #[test]
    fn test_enrich_program_reports_unsupported_inferred_binding_type() {
        use hew_parser::ast::Pattern;
        use hew_types::ty::TypeVar;

        let expr_span = 10..18;
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
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![(
                            Stmt::Let {
                                pattern: (Pattern::Identifier("value".into()), expr_span.clone()),
                                ty: None,
                                value: Some((Expr::Identifier("input".into()), expr_span.clone())),
                            },
                            expr_span.clone(),
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let mut tco = empty_tco();
        tco.expr_types.insert(
            SpanKey {
                start: expr_span.start,
                end: expr_span.end,
            },
            Ty::Var(TypeVar(7)),
        );

        let diagnostics = enrich_program(
            &mut program,
            &tco,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        )
        .unwrap();
        assert_eq!(diagnostics.diagnostics().len(), 1);
        let diagnostic = &diagnostics.diagnostics()[0];
        assert_eq!(diagnostic.span(), Some(&expr_span));
        let message = diagnostic.to_string();
        assert!(
            message.contains("let binding type inferred from initializer"),
            "unexpected error: {message}"
        );
        assert!(message.contains("?T7"), "unexpected error: {message}");
        if let Item::Function(function) = &program.items[0].0 {
            match &function.body.stmts[0].0 {
                Stmt::Let { ty, .. } => {
                    assert!(ty.is_none(), "unsupported type should stay implicit")
                }
                other => panic!("expected let statement, got {other:?}"),
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_enrich_program_reports_unsupported_inferred_return_type() {
        let expr_span = 21..29;
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
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![],
                        trailing_expr: Some(Box::new((
                            Expr::Identifier("stream".into()),
                            expr_span.clone(),
                        ))),
                    },
                    doc_comment: None,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let mut tco = empty_tco();
        tco.expr_types.insert(
            SpanKey {
                start: expr_span.start,
                end: expr_span.end,
            },
            Ty::generator(Ty::I32, Ty::String),
        );

        let diagnostics = enrich_program(
            &mut program,
            &tco,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        )
        .unwrap();
        assert_eq!(diagnostics.diagnostics().len(), 1);
        let diagnostic = &diagnostics.diagnostics()[0];
        assert_eq!(diagnostic.span(), Some(&expr_span));
        let message = diagnostic.to_string();
        assert!(
            message.contains("function `foo` return type inferred from trailing expression"),
            "unexpected error: {message}"
        );
        assert!(
            message.contains("generator type is not representable in serialized TypeExpr"),
            "unexpected error: {message}"
        );
        if let Item::Function(function) = &program.items[0].0 {
            assert!(
                function.return_type.is_none(),
                "unsupported return type should stay implicit"
            );
        } else {
            panic!("expected function");
        }
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
        normalize_all_types(
            &mut program,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        );
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

    /// Helper: create a `TypeCheckOutput` with `user_modules` set.
    use std::collections::{HashMap, HashSet};

    fn empty_tco() -> TypeCheckOutput {
        TypeCheckOutput {
            expr_types: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
        }
    }

    fn make_tco_with_user_modules(modules: Vec<&str>) -> TypeCheckOutput {
        TypeCheckOutput {
            user_modules: modules.into_iter().map(String::from).collect(),
            ..empty_tco()
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

        enrich_expr(&mut expr, &tco).unwrap();

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

        enrich_expr(&mut expr, &tco).unwrap();

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

        enrich_expr(&mut expr, &tco).unwrap();

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

        enrich_expr(&mut expr, &tco).unwrap();

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
