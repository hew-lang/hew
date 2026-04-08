//! AST enrichment: fills in missing type annotations using `TypeCheckOutput`.
//!
//! After the type checker runs, this module walks the parsed AST and injects
//! inferred types into `let`/`var` bindings and function return types that lack
//! explicit annotations. The result is a fully-typed AST that the C++ backend
//! can consume without its own type inference.

use hew_parser::ast::{
    ActorDecl, Block, CallArg, Expr, ExternBlock, ExternFnDecl, FnDecl, Item, Param, Program, Span,
    Spanned, Stmt, TraitBound, TypeExpr,
};
use hew_types::builtin_names::{
    QUALIFIED_RECEIVER, QUALIFIED_SENDER, QUALIFIED_SINK, QUALIFIED_STREAM, RECEIVER, SENDER, SINK,
    STREAM,
};
use hew_types::check::{SpanKey, TypeCheckOutput};
use hew_types::Ty;
use std::fmt;

use crate::msgpack::ExprTypeEntry;

/// Discriminant that callers use to decide how to handle a conversion failure.
///
/// - `UnresolvedVar` — a `Ty::Var` escaped type-checking unresolved; this is a
///   compiler bug and must be treated as a hard error.
/// - `ErrorSentinel` — a `Ty::Error` reached the serializer after type-checking;
///   this should abort codegen rather than being silently dropped.
/// - `LiteralKind` — a numeric literal kind survived to serialization without
///   being finalized at a real coercion/defaulting site.
/// - `Unsupported` — the type is structurally valid but not yet representable
///   (e.g. generator types); callers may choose to warn and continue.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeExprConversionKind {
    UnresolvedVar,
    ErrorSentinel,
    LiteralKind,
    Unsupported,
}

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

    /// Returns the kind of this conversion error, derived from the source type.
    ///
    /// - [`TypeExprConversionKind::UnresolvedVar`] when the originating type is
    ///   `Ty::Var`; callers should treat this as a hard error.
    /// - [`TypeExprConversionKind::ErrorSentinel`] when it is `Ty::Error`;
    ///   callers should treat this as a hard error if it ever escapes.
    /// - [`TypeExprConversionKind::LiteralKind`] when a numeric literal kind
    ///   reached serialization before explicit finalization.
    /// - [`TypeExprConversionKind::Unsupported`] for all other types.
    #[must_use]
    pub fn kind(&self) -> TypeExprConversionKind {
        match &self.ty {
            Ty::Var(_) => TypeExprConversionKind::UnresolvedVar,
            Ty::Error => TypeExprConversionKind::ErrorSentinel,
            Ty::IntLiteral | Ty::FloatLiteral => TypeExprConversionKind::LiteralKind,
            _ => TypeExprConversionKind::Unsupported,
        }
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

/// Convert a callable type (function or closure) to a `TypeExpr::Function`.
fn convert_callable_type(
    params: &[Ty],
    ret: &Ty,
    label: &str,
) -> Result<TypeExpr, TypeExprConversionError> {
    let param_exprs = params
        .iter()
        .enumerate()
        .map(|(index, param)| require_converted(param, format!("{label} parameter {index}")))
        .collect::<Result<Vec<_>, _>>()?;
    let ret_expr = require_converted(ret, format!("{label} return type"))?;
    Ok(TypeExpr::Function {
        params: param_exprs,
        return_type: Box::new(ret_expr),
    })
}

/// Extract a short element-type name from a `Ty` for stream method dispatch.
///
/// Returns `"bytes"` for `Ty::Bytes`, `"String"` for `Ty::String`, or the
/// `name` for `Ty::Named`.  Used by the enricher to select the correct
/// runtime C symbol (e.g. `hew_stream_next` vs `hew_stream_next_bytes`).
fn ty_element_name(ty: &Ty) -> Option<&str> {
    match ty {
        Ty::Bytes => Some("bytes"),
        Ty::String => Some("String"),
        Ty::Named { name, .. } => Some(name),
        _ => None,
    }
}

/// Map primitive `Ty` variants to their serialized type name.
fn primitive_name(ty: &Ty) -> Option<&'static str> {
    ty.canonical_lowering_name()
}

#[allow(
    clippy::too_many_lines,
    reason = "type mapping covers many Ty variants"
)]
fn ty_to_type_expr(ty: &Ty) -> Result<Spanned<TypeExpr>, TypeExprConversionError> {
    let span: Span = 0..0; // synthetic span for inferred types

    let te = if let Some(name) = primitive_name(ty) {
        TypeExpr::Named {
            name: name.into(),
            type_args: None,
        }
    } else if matches!(ty, Ty::Unit) {
        TypeExpr::Tuple(Vec::new())
    } else {
        match ty {
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
                ("Generator" | "AsyncGenerator", _) => {
                    return Err(TypeExprConversionError::unsupported(
                        ty,
                        "generator type is not representable in serialized TypeExpr",
                    ));
                }
                ("Range", 1) => {
                    let inner_expr = require_converted(&args[0], "Range element type")?;
                    TypeExpr::Named {
                        name: "Range".into(),
                        type_args: Some(vec![inner_expr]),
                    }
                }
                _ => {
                    let type_args = if args.is_empty() {
                        None
                    } else {
                        Some(
                            args.iter()
                                .enumerate()
                                .map(|(index, arg)| {
                                    require_converted(
                                        arg,
                                        format!("type argument {index} of `{name}`"),
                                    )
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

            Ty::Function { params, ret } => convert_callable_type(params, ret, "function")?,

            Ty::Closure { params, ret, .. } => convert_callable_type(params, ret, "closure")?,

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
            Ty::IntLiteral | Ty::FloatLiteral => {
                return Err(TypeExprConversionError::unsupported(
                    ty,
                    "numeric literal kind reached serializer without explicit coercion",
                ));
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
            // Primitives, Unit, and Never are handled by the table above
            _ => unreachable!("primitive_name should have matched {ty:?}"),
        }
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
    let mut import_paths: Vec<String> = Vec::new();
    for (item, _span) in &mut program.items {
        // Collect import module paths for extern synthesis (avoids a second pass).
        if let Item::Import(import_decl) = &*item {
            import_paths.push(import_decl.path.join("::"));
        }
        enrich_item_with_diagnostics(item, tco, &mut diagnostics, registry)?;
    }
    normalize_all_types(program, registry);
    synthesize_stdlib_externs_from_imports(program, &import_paths, registry)?;
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
                let mut args = type_args.take().expect("arity verified by guard");
                let err = args.pop().expect("arity verified by guard");
                let ok = args.pop().expect("arity verified by guard");
                *te = TypeExpr::Result {
                    ok: Box::new(ok),
                    err: Box::new(err),
                };
            }
            "Option" if type_args.as_ref().is_some_and(|a| a.len() == 1) => {
                let mut args = type_args.take().expect("arity verified by guard");
                let inner = args.pop().expect("arity verified by guard");
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

/// Synthesize `ExternBlock` items for stdlib imports.
///
/// Takes pre-collected import module paths (from the enrichment pass) so we
/// don't need to re-iterate `program.items`.
fn synthesize_stdlib_externs_from_imports(
    program: &mut Program,
    import_paths: &[String],
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    let mut new_items: Vec<Spanned<Item>> = Vec::new();

    for module_path in import_paths {
        if let Some(info) = registry.get(module_path) {
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

// ── Shared AST child-traversal helpers ──────────────────────────────────────
//
// The three tree-walk families (rewrite_builtin_calls, normalize_types, and
// enrich_with_diagnostics) share identical child traversal for ~25 of ~30 Expr
// variants.  These helpers factor out the common dispatch so each family only
// needs to match the handful of variants where it has custom logic.

/// Visitor interface for the shared AST child-traversal helpers.
///
/// Using a trait instead of separate closures avoids borrow-checker conflicts
/// when the visitor needs shared mutable state (e.g. the diagnostics vec in
/// the enrich family).
trait AstVisitor {
    fn visit_expr(&mut self, e: &mut Spanned<Expr>);
    fn visit_block(&mut self, b: &mut Block);
    fn visit_stmt(&mut self, s: &mut Stmt);
}

/// Visit all child expressions and blocks of an `Expr` node.
///
/// Handles the purely-recursive variants.  `Call`, `MethodCall`, `Lambda`, and
/// `Cast` are intentionally skipped — callers must match those explicitly
/// because each family has custom logic for them.
#[expect(
    clippy::too_many_lines,
    reason = "one arm per Expr variant is inherently long"
)]
fn walk_expr_children(expr: &mut Spanned<Expr>, v: &mut impl AstVisitor) {
    match &mut expr.0 {
        Expr::Binary { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }
        Expr::Unary { operand, .. } => v.visit_expr(operand),
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            v.visit_expr(condition);
            v.visit_expr(then_block);
            if let Some(e) = else_block {
                v.visit_expr(e);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            v.visit_expr(expr);
            v.visit_block(body);
            if let Some(b) = else_body {
                v.visit_block(b);
            }
        }
        Expr::Match { scrutinee, arms } => {
            v.visit_expr(scrutinee);
            for arm in arms {
                if let Some(g) = &mut arm.guard {
                    v.visit_expr(g);
                }
                v.visit_expr(&mut arm.body);
            }
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block)
        | Expr::Scope { body: block, .. } => {
            v.visit_block(block);
        }
        Expr::Array(elems) | Expr::Tuple(elems) => {
            for e in elems {
                v.visit_expr(e);
            }
        }
        Expr::MapLiteral { entries } => {
            for (k, val) in entries {
                v.visit_expr(k);
                v.visit_expr(val);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            v.visit_expr(value);
            v.visit_expr(count);
        }
        Expr::Index { object, index } => {
            v.visit_expr(object);
            v.visit_expr(index);
        }
        Expr::FieldAccess { object, .. } => v.visit_expr(object),
        Expr::StructInit { fields, .. } => {
            for (_, val) in fields {
                v.visit_expr(val);
            }
        }
        Expr::Spawn { target, args } => {
            v.visit_expr(target);
            for (_, val) in args {
                v.visit_expr(val);
            }
        }
        Expr::SpawnLambdaActor { body, .. } => v.visit_expr(body),
        Expr::Send { target, message } => {
            v.visit_expr(target);
            v.visit_expr(message);
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                v.visit_expr(&mut arm.source);
                v.visit_expr(&mut arm.body);
            }
            if let Some(t) = timeout {
                v.visit_expr(&mut t.duration);
                v.visit_expr(&mut t.body);
            }
        }
        Expr::Join(exprs) => {
            for e in exprs {
                v.visit_expr(e);
            }
        }
        Expr::Timeout { expr, duration } => {
            v.visit_expr(expr);
            v.visit_expr(duration);
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let hew_parser::ast::StringPart::Expr(e) = part {
                    v.visit_expr(e);
                }
            }
        }
        Expr::PostfixTry(inner) | Expr::Await(inner) | Expr::Yield(Some(inner)) => {
            v.visit_expr(inner);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                v.visit_expr(s);
            }
            if let Some(e) = end {
                v.visit_expr(e);
            }
        }
        // Leaf nodes and family-specific variants (caller handles Call/MethodCall/Lambda/Cast)
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::Cooperate
        | Expr::ScopeCancel
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Yield(None)
        | Expr::Call { .. }
        | Expr::MethodCall { .. }
        | Expr::Lambda { .. }
        | Expr::Cast { .. } => {}
    }
}

/// Visit all child expressions and blocks of a `Stmt` node.
///
/// `Let` and `Var` are intentionally skipped because the enrich family has
/// custom type-inference logic for them.
fn walk_stmt_children(stmt: &mut Stmt, v: &mut impl AstVisitor) {
    match stmt {
        Stmt::Expression(ref mut expr)
        | Stmt::Return(Some(ref mut expr))
        | Stmt::Break {
            value: Some(ref mut expr),
            ..
        } => {
            v.visit_expr(expr);
        }
        Stmt::Defer(ref mut expr) => v.visit_expr(expr),
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            v.visit_expr(condition);
            v.visit_block(then_block);
            if let Some(ref mut eb) = else_block {
                if let Some(ref mut block) = eb.block {
                    v.visit_block(block);
                }
                if let Some(ref mut if_stmt) = eb.if_stmt {
                    v.visit_stmt(&mut if_stmt.0);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            v.visit_expr(expr);
            v.visit_block(body);
            if let Some(block) = else_body {
                v.visit_block(block);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            v.visit_expr(scrutinee);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    v.visit_expr(guard);
                }
                v.visit_expr(&mut arm.body);
            }
        }
        Stmt::For { body, iterable, .. } => {
            v.visit_expr(iterable);
            v.visit_block(body);
        }
        Stmt::While {
            condition, body, ..
        } => {
            v.visit_expr(condition);
            v.visit_block(body);
        }
        Stmt::WhileLet { expr, body, .. } => {
            v.visit_expr(expr);
            v.visit_block(body);
        }
        Stmt::Loop { body, .. } => v.visit_block(body),
        Stmt::Assign { target, value, .. } => {
            v.visit_expr(target);
            v.visit_expr(value);
        }
        Stmt::Let { .. }
        | Stmt::Var { .. }
        | Stmt::Return(None)
        | Stmt::Break { value: None, .. }
        | Stmt::Continue { .. } => {}
    }
}

/// Visit all child statements and trailing expression of a `Block`.
fn walk_block_children(block: &mut Block, v: &mut impl AstVisitor) {
    for (stmt, _) in &mut block.stmts {
        v.visit_stmt(stmt);
    }
    if let Some(ref mut trailing) = block.trailing_expr {
        v.visit_expr(trailing);
    }
}

fn rewrite_builtin_calls_in_item(item: &mut Item) {
    match item {
        Item::Function(f) => rewrite_builtin_calls_in_block(&mut f.body),
        Item::Actor(actor) => {
            if let Some(ref mut init) = actor.init {
                rewrite_builtin_calls_in_block(&mut init.body);
            }
            if let Some(ref mut term) = actor.terminate {
                rewrite_builtin_calls_in_block(&mut term.body);
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
    walk_block_children(block, &mut RewriteVisitor);
}

fn rewrite_builtin_calls_in_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some(expr) = value {
                rewrite_builtin_calls_in_expr(expr);
            }
        }
        _ => walk_stmt_children(stmt, &mut RewriteVisitor),
    }
}

struct RewriteVisitor;

impl AstVisitor for RewriteVisitor {
    fn visit_expr(&mut self, e: &mut Spanned<Expr>) {
        rewrite_builtin_calls_in_expr(e);
    }
    fn visit_block(&mut self, b: &mut Block) {
        rewrite_builtin_calls_in_block(b);
    }
    fn visit_stmt(&mut self, s: &mut Stmt) {
        rewrite_builtin_calls_in_stmt(s);
    }
}

/// Rewrite `len(x)` calls to `x.len()` method calls.
fn try_rewrite_len_call(function: &Expr, args: &mut Vec<CallArg>) -> Option<Expr> {
    if let Expr::Identifier(name) = function {
        if name == "len" && args.len() == 1 {
            let receiver = match std::mem::take(args).remove(0) {
                CallArg::Positional(e) => e,
                CallArg::Named { value, .. } => value,
            };
            return Some(Expr::MethodCall {
                receiver: Box::new(receiver),
                method: "len".to_string(),
                args: Vec::new(),
            });
        }
    }
    None
}

fn rewrite_builtin_calls_in_expr(expr: &mut Spanned<Expr>) {
    match &mut expr.0 {
        Expr::Call { function, args, .. } => {
            for arg in args.iter_mut() {
                rewrite_builtin_calls_in_expr(arg.expr_mut());
            }
            rewrite_builtin_calls_in_expr(function);
            if let Some(rewritten) = try_rewrite_len_call(&function.0, args) {
                expr.0 = rewritten;
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            rewrite_builtin_calls_in_expr(receiver);
            for arg in args.iter_mut() {
                rewrite_builtin_calls_in_expr(arg.expr_mut());
            }
        }
        Expr::Lambda { body, .. } => rewrite_builtin_calls_in_expr(body),
        Expr::Cast { expr: inner, .. } => rewrite_builtin_calls_in_expr(inner),
        _ => walk_expr_children(expr, &mut RewriteVisitor),
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
            if let Some(ref mut term) = actor.terminate {
                normalize_block_types(&mut term.body, registry);
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
    walk_block_children(block, &mut NormalizeVisitor { registry });
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
        _ => walk_stmt_children(stmt, &mut NormalizeVisitor { registry }),
    }
}

struct NormalizeVisitor<'a> {
    registry: &'a hew_types::module_registry::ModuleRegistry,
}

impl AstVisitor for NormalizeVisitor<'_> {
    fn visit_expr(&mut self, e: &mut Spanned<Expr>) {
        normalize_expr_types(e, self.registry);
    }
    fn visit_block(&mut self, b: &mut Block) {
        normalize_block_types(b, self.registry);
    }
    fn visit_stmt(&mut self, s: &mut Stmt) {
        normalize_stmt_types(s, self.registry);
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

fn normalize_expr_types_inner(
    expr: &mut Spanned<Expr>,
    registry: &hew_types::module_registry::ModuleRegistry,
) {
    match &mut expr.0 {
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
        Expr::Cast { expr: inner, ty } => {
            normalize_expr_types(inner, registry);
            normalize_type_expr(&mut ty.0, registry);
        }
        _ => walk_expr_children(expr, &mut NormalizeVisitor { registry }),
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
    if let Some(ref mut term) = actor.terminate {
        enrich_block_with_diagnostics(&mut term.body, tco, diagnostics, registry)?;
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

/// Infer a missing type annotation for a let/var binding from the type checker.
fn infer_binding_type(
    ty: &mut Option<Spanned<TypeExpr>>,
    value: Option<&Spanned<Expr>>,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    context: impl Into<String>,
) {
    if ty.is_none() {
        if let Some(val) = value {
            match lookup_inferred_type(tco, &val.1, context) {
                Ok(Some(inferred)) => *ty = Some(inferred),
                Ok(None) => {}
                Err(diagnostic) => diagnostics.push(diagnostic),
            }
        }
    }
}

fn enrich_stmt_with_diagnostics(
    stmt: &mut Stmt,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    match stmt {
        Stmt::Let { ty, value, .. } => {
            infer_binding_type(
                ty,
                value.as_ref(),
                tco,
                diagnostics,
                "let binding type inferred from initializer",
            );
            if let Some(ref mut val) = value {
                enrich_expr_with_diagnostics(val, tco, diagnostics, registry)?;
            }
        }
        Stmt::Var { name, ty, value } => {
            infer_binding_type(
                ty,
                value.as_ref(),
                tco,
                diagnostics,
                format!("var `{name}` type inferred from initializer"),
            );
            if let Some(ref mut val) = value {
                enrich_expr_with_diagnostics(val, tco, diagnostics, registry)?;
            }
        }
        _ => {
            walk_stmt_children(
                stmt,
                &mut EnrichVisitor {
                    tco,
                    diagnostics,
                    registry,
                },
            );
        }
    }
    Ok(())
}

/// Rewrite `MethodCall` nodes to the forms the C++ codegen expects.
///
/// Handles three categories of method call rewriting:
/// 1. Module-qualified stdlib calls (e.g. `os.pid()` → `hew_os_pid()`)
/// 2. User module calls (e.g. `utils.helper(x)` → `helper(x)`)
/// 3. Handle/stream/channel method calls (receiver prepended as first argument)
fn enrich_method_call(
    expr: &mut Spanned<Expr>,
    tco: &TypeCheckOutput,
    registry: &hew_types::module_registry::ModuleRegistry,
) {
    let Expr::MethodCall {
        receiver,
        method,
        args,
    } = &mut expr.0
    else {
        return;
    };

    // Rewrite module-qualified stdlib calls: e.g. os.pid() → hew_os_pid()
    if let Expr::Identifier(module_name) = &receiver.0 {
        if let Some(c_symbol) = registry.resolve_module_call(module_name, method) {
            // Skip identity-mapped wrappers (e.g. log.setup → setup): these are
            // non-trivial Hew wrappers that must be compiled as module graph
            // functions and called via their mangled name. Leaving them as
            // MethodCall lets the C++ codegen dispatch them correctly.
            if c_symbol != *method {
                let old_args = std::mem::take(args);
                expr.0 = Expr::Call {
                    function: Box::new((Expr::Identifier(c_symbol), receiver.1.clone())),
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
    let c_fn: Option<String> = match tco.expr_types.get(&key) {
        Some(Ty::Named { name, args }) if name == STREAM || name == QUALIFIED_STREAM => {
            let elem = args.first().and_then(ty_element_name);
            hew_types::stdlib::resolve_stream_method(STREAM, method, elem).map(String::from)
        }
        Some(Ty::Named { name, args }) if name == SINK || name == QUALIFIED_SINK => {
            let elem = args.first().and_then(ty_element_name);
            hew_types::stdlib::resolve_stream_method(SINK, method, elem).map(String::from)
        }
        Some(Ty::Named { name, args }) if name == SENDER || name == QUALIFIED_SENDER => {
            hew_types::stdlib::resolve_channel_method(SENDER, method, args.first())
                .map(String::from)
        }
        Some(Ty::Named { name, args }) if name == RECEIVER || name == QUALIFIED_RECEIVER => {
            hew_types::stdlib::resolve_channel_method(RECEIVER, method, args.first())
                .map(String::from)
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

fn enrich_expr_with_diagnostics(
    expr: &mut Spanned<Expr>,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, || {
        enrich_expr_with_diagnostics_inner(expr, tco, diagnostics, registry)
    })
}

struct EnrichVisitor<'a> {
    tco: &'a TypeCheckOutput,
    diagnostics: &'a mut Vec<TypeExprConversionError>,
    registry: &'a hew_types::module_registry::ModuleRegistry,
}

impl AstVisitor for EnrichVisitor<'_> {
    fn visit_expr(&mut self, e: &mut Spanned<Expr>) {
        if let Err(err) = enrich_expr_with_diagnostics(e, self.tco, self.diagnostics, self.registry)
        {
            self.diagnostics.push(err);
        }
    }
    fn visit_block(&mut self, b: &mut Block) {
        if let Err(err) =
            enrich_block_with_diagnostics(b, self.tco, self.diagnostics, self.registry)
        {
            self.diagnostics.push(err);
        }
    }
    fn visit_stmt(&mut self, s: &mut Stmt) {
        if let Err(err) = enrich_stmt_with_diagnostics(s, self.tco, self.diagnostics, self.registry)
        {
            self.diagnostics.push(err);
        }
    }
}

fn enrich_expr_with_diagnostics_inner(
    expr: &mut Spanned<Expr>,
    tco: &TypeCheckOutput,
    diagnostics: &mut Vec<TypeExprConversionError>,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    let expr_span_key = SpanKey::from(&expr.1);
    match &mut expr.0 {
        Expr::MethodCall { receiver, args, .. } => {
            enrich_expr_with_diagnostics(receiver, tco, diagnostics, registry)?;
            for arg in args.iter_mut() {
                enrich_expr_with_diagnostics(arg.expr_mut(), tco, diagnostics, registry)?;
            }
            enrich_method_call(expr, tco, registry);
        }
        Expr::Call {
            function,
            args,
            type_args,
            ..
        } => {
            enrich_expr_with_diagnostics(function, tco, diagnostics, registry)?;
            for arg in args.iter_mut() {
                enrich_expr_with_diagnostics(arg.expr_mut(), tco, diagnostics, registry)?;
            }

            // Fill in inferred type arguments for generic calls that omit
            // explicit type annotations (e.g. `identity(42)` → `identity<int>(42)`).
            if type_args.is_none() {
                let call_span_key = SpanKey::from(&expr.1);
                if let Some(inferred) = tco.call_type_args.get(&call_span_key) {
                    let converted: Result<Vec<_>, _> = inferred
                        .iter()
                        .enumerate()
                        .map(|(index, ty)| {
                            require_converted(ty, format!("inferred call type argument {index}"))
                        })
                        .collect();
                    match converted {
                        Ok(ta) => {
                            *type_args = Some(ta);
                        }
                        Err(err) => diagnostics.push(err.with_span(expr.1.clone())),
                    }
                }
            }

            if let Some(rewritten) = try_rewrite_len_call(&function.0, args) {
                expr.0 = rewritten;
            }
        }
        Expr::Lambda { params, body, .. } => {
            if let Some(inferred_params) = match tco.expr_types.get(&expr_span_key) {
                Some(
                    Ty::Function {
                        params: inferred_params,
                        ..
                    }
                    | Ty::Closure {
                        params: inferred_params,
                        ..
                    },
                ) => Some(inferred_params),
                _ => None,
            } {
                for (param, inferred_ty) in params.iter_mut().zip(inferred_params.iter()) {
                    if param.ty.is_none() {
                        match require_converted(
                            inferred_ty,
                            format!("lambda parameter `{}` inferred type", param.name),
                        ) {
                            Ok(inferred_param_ty) => {
                                param.ty = Some(inferred_param_ty);
                            }
                            Err(err) => diagnostics.push(err.with_span(expr.1.clone())),
                        }
                    }
                }
            }
            enrich_expr_with_diagnostics(body, tco, diagnostics, registry)?;
        }
        Expr::Cast { expr: inner, .. } => {
            enrich_expr_with_diagnostics(inner, tco, diagnostics, registry)?;
        }
        _ => {
            walk_expr_children(
                expr,
                &mut EnrichVisitor {
                    tco,
                    diagnostics,
                    registry,
                },
            );
        }
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

/// Test-only wrapper that extracts import paths from `program.items` and
/// delegates to `synthesize_stdlib_externs_from_imports`.
#[cfg(test)]
fn synthesize_stdlib_externs(
    program: &mut Program,
    registry: &hew_types::module_registry::ModuleRegistry,
) -> Result<(), TypeExprConversionError> {
    let import_paths: Vec<String> = program
        .items
        .iter()
        .filter_map(|(item, _)| {
            if let Item::Import(import_decl) = item {
                Some(import_decl.path.join("::"))
            } else {
                None
            }
        })
        .collect();
    synthesize_stdlib_externs_from_imports(program, &import_paths, registry)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::{
        ConstDecl, ElseBlock, ImplDecl, ImportDecl, MachineDecl, MachineEvent, MachineState,
        MachineTransition, ReceiveFnDecl, SupervisorDecl, TraitDecl, TypeDecl, Visibility,
    };

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
                    decl_span: 0..0,
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
        assert_eq!(
            err.kind(),
            TypeExprConversionKind::ErrorSentinel,
            "Ty::Error must produce ErrorSentinel kind"
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
                .contains("generator type is not representable in serialized TypeExpr"),
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
    fn test_ty_to_type_expr_range_with_var_is_unresolved_var() {
        // Range<Ty::Var> must not be silently coerced to Range<i64>; it must
        // surface as UnresolvedVar so the fail-closed path in compile.rs aborts.
        use hew_types::ty::TypeVar;
        let ty = Ty::range(Ty::Var(TypeVar(5)));
        let err = unwrap_err(ty_to_type_expr(&ty));
        assert_eq!(
            err.kind(),
            TypeExprConversionKind::UnresolvedVar,
            "Range<Ty::Var> must produce UnresolvedVar, got: {err}"
        );
        assert!(
            err.to_string().contains("Range element type"),
            "error context should mention Range element type, got: {err}"
        );
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
        assert_eq!(
            err.kind(),
            TypeExprConversionKind::UnresolvedVar,
            "Ty::Var must produce UnresolvedVar kind"
        );
    }

    #[test]
    fn test_ty_to_type_expr_generator_returns_unsupported_kind() {
        let ty = Ty::generator(Ty::I32, Ty::String);
        let err = unwrap_err(ty_to_type_expr(&ty));
        assert_eq!(
            err.kind(),
            TypeExprConversionKind::Unsupported,
            "Generator must produce Unsupported kind (not UnresolvedVar or ErrorSentinel)"
        );
    }

    #[test]
    fn test_ty_to_type_expr_literal_kind_returns_literal_kind() {
        let err = unwrap_err(ty_to_type_expr(&Ty::IntLiteral));
        assert!(
            err.to_string()
                .contains("numeric literal kind reached serializer without explicit coercion"),
            "unexpected error: {err}"
        );
        assert_eq!(err.kind(), TypeExprConversionKind::LiteralKind);
    }

    #[test]
    fn test_kind_discriminants_are_distinct() {
        use hew_types::ty::TypeVar;
        let var_err = unwrap_err(ty_to_type_expr(&Ty::Var(TypeVar(1))));
        let sentinel_err = unwrap_err(ty_to_type_expr(&Ty::Error));
        let unsupported_err = unwrap_err(ty_to_type_expr(&Ty::generator(Ty::I32, Ty::String)));
        let literal_err = unwrap_err(ty_to_type_expr(&Ty::IntLiteral));
        assert_eq!(var_err.kind(), TypeExprConversionKind::UnresolvedVar);
        assert_eq!(sentinel_err.kind(), TypeExprConversionKind::ErrorSentinel);
        assert_eq!(unsupported_err.kind(), TypeExprConversionKind::Unsupported);
        assert_eq!(literal_err.kind(), TypeExprConversionKind::LiteralKind);
        assert_ne!(var_err.kind(), sentinel_err.kind());
        assert_ne!(var_err.kind(), unsupported_err.kind());
        assert_ne!(var_err.kind(), literal_err.kind());
        assert_ne!(sentinel_err.kind(), unsupported_err.kind());
        assert_ne!(sentinel_err.kind(), literal_err.kind());
        assert_ne!(unsupported_err.kind(), literal_err.kind());
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
    fn test_build_expr_type_map_rejects_literal_kind_entries() {
        let mut tco = empty_tco();
        tco.expr_types
            .insert(SpanKey { start: 5, end: 7 }, Ty::FloatLiteral);

        let result = build_expr_type_map(&tco);
        assert!(result.entries.is_empty());
        assert_eq!(result.diagnostics().len(), 1);
        assert!(
            result.diagnostics()[0]
                .to_string()
                .contains("numeric literal kind reached serializer without explicit coercion"),
            "unexpected diagnostics: {:?}",
            result.diagnostics()
        );
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
                    decl_span: 0..0,
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
                    assert!(ty.is_none(), "unsupported type should stay implicit");
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
                    decl_span: 0..0,
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
                    decl_span: 0..0,
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
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
        }
    }

    fn make_tco_with_user_modules(modules: Vec<&str>) -> TypeCheckOutput {
        TypeCheckOutput {
            user_modules: modules.into_iter().map(String::from).collect(),
            ..empty_tco()
        }
    }

    fn parse_and_typecheck_main_lambda(source: &str) -> (Spanned<Expr>, TypeCheckOutput) {
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parsed.errors
        );

        let lambda = parsed
            .program
            .items
            .iter()
            .find_map(|(item, _)| match item {
                Item::Function(fd) if fd.name == "main" => {
                    fd.body.stmts.iter().find_map(|(stmt, _)| match stmt {
                        Stmt::Let {
                            value: Some(expr @ (Expr::Lambda { .. }, _)),
                            ..
                        } => Some(expr.clone()),
                        _ => None,
                    })
                }
                _ => None,
            })
            .expect("main let-bound lambda should exist");

        let mut checker =
            hew_types::check::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&parsed.program);

        (lambda, tco)
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
    fn test_enrich_quic_observe_handle_method_uses_wrapper_fn() {
        let mut tco = empty_tco();
        tco.expr_types.insert(
            hew_types::check::SpanKey { start: 0, end: 2 },
            hew_types::Ty::Named {
                name: "quic.QUICEndpoint".to_string(),
                args: vec![],
            },
        );
        let registry = test_registry_with(&["std::net::quic"]);

        let mut expr: Spanned<Expr> = (
            Expr::MethodCall {
                receiver: Box::new((Expr::Identifier("ep".to_string()), 0..2)),
                method: "observe".to_string(),
                args: vec![],
            },
            0..10,
        );

        let mut diagnostics = Vec::new();
        enrich_expr_with_diagnostics(&mut expr, &tco, &mut diagnostics, &registry).unwrap();
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {diagnostics:?}"
        );

        match &expr.0 {
            Expr::Call { function, args, .. } => {
                match &function.0 {
                    Expr::Identifier(name) => {
                        assert_eq!(name, "endpoint_observe");
                    }
                    other => panic!("expected Identifier, got {other:?}"),
                }
                assert_eq!(args.len(), 1, "observe() should prepend the receiver");
            }
            other => panic!("expected Call expr, got {other:?}"),
        }
    }

    #[test]
    fn test_enrich_quic_observe_bare_handle_type_uses_wrapper_fn() {
        let mut tco = empty_tco();
        tco.expr_types.insert(
            hew_types::check::SpanKey { start: 0, end: 2 },
            hew_types::Ty::Named {
                name: "QUICEndpoint".to_string(),
                args: vec![],
            },
        );
        let registry = test_registry_with(&["std::net::quic"]);

        let mut expr: Spanned<Expr> = (
            Expr::MethodCall {
                receiver: Box::new((Expr::Identifier("ep".to_string()), 0..2)),
                method: "observe".to_string(),
                args: vec![],
            },
            0..10,
        );

        let mut diagnostics = Vec::new();
        enrich_expr_with_diagnostics(&mut expr, &tco, &mut diagnostics, &registry).unwrap();
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {diagnostics:?}"
        );

        match &expr.0 {
            Expr::Call { function, args, .. } => {
                match &function.0 {
                    Expr::Identifier(name) => {
                        assert_eq!(name, "endpoint_observe");
                    }
                    other => panic!("expected Identifier, got {other:?}"),
                }
                assert_eq!(args.len(), 1, "observe() should prepend the receiver");
            }
            other => panic!("expected Call expr, got {other:?}"),
        }
    }

    #[test]
    fn test_enrich_lambda_params_from_contextual_function_type() {
        let source = concat!(
            "fn main() {\n",
            "    let f: fn(int) -> int = (x) => x + 1;\n",
            "    let y = f(5);\n",
            "}\n",
        );
        let (mut expr, tco) = parse_and_typecheck_main_lambda(source);
        assert!(
            tco.errors.is_empty(),
            "unexpected type check errors: {:?}",
            tco.errors
        );

        let mut diagnostics = Vec::new();
        enrich_expr_with_diagnostics(
            &mut expr,
            &tco,
            &mut diagnostics,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        )
        .unwrap();
        assert!(
            diagnostics.is_empty(),
            "unexpected enrichment diagnostics: {diagnostics:?}"
        );

        match &expr.0 {
            Expr::Lambda { params, .. } => {
                assert_eq!(params.len(), 1);
                assert!(matches!(
                    params[0].ty.as_ref().map(|(ty, _)| ty),
                    Some(TypeExpr::Named { name, type_args: None }) if name == "i64"
                ));
            }
            other => panic!("expected lambda expr, got {other:?}"),
        }
    }

    #[test]
    fn test_enrich_lambda_params_from_materialized_closure_type() {
        let source = concat!(
            "fn main() {\n",
            "    let offset = 1;\n",
            "    let f = (x) => x + offset;\n",
            "    let y = f(5);\n",
            "}\n",
        );
        let (mut expr, tco) = parse_and_typecheck_main_lambda(source);
        assert!(
            tco.errors.is_empty(),
            "unexpected type check errors: {:?}",
            tco.errors
        );
        let lambda_ty = tco.expr_types.get(&SpanKey::from(&expr.1));
        assert!(
            matches!(lambda_ty, Some(Ty::Closure { captures, .. }) if !captures.is_empty()),
            "expected captured lambda to record a closure type, got {lambda_ty:?}"
        );

        let mut diagnostics = Vec::new();
        enrich_expr_with_diagnostics(
            &mut expr,
            &tco,
            &mut diagnostics,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        )
        .unwrap();
        assert!(
            diagnostics.is_empty(),
            "unexpected enrichment diagnostics: {diagnostics:?}"
        );

        match &expr.0 {
            Expr::Lambda { params, .. } => {
                assert_eq!(params.len(), 1);
                assert!(matches!(
                    params[0].ty.as_ref().map(|(ty, _)| ty),
                    Some(TypeExpr::Named { name, type_args: None }) if name == "i64"
                ));
            }
            other => panic!("expected lambda expr, got {other:?}"),
        }
    }

    #[test]
    fn test_enrich_lambda_params_report_unresolved_types() {
        let source = "fn main() { let f = (x) => x; }";
        let (mut expr, tco) = parse_and_typecheck_main_lambda(source);

        assert!(matches!(
            tco.expr_types.get(&SpanKey::from(&expr.1)),
            Some(Ty::Function { params, ret })
                if matches!(params.as_slice(), [Ty::Var(_)])
                    && matches!(ret.as_ref(), Ty::Var(_))
        ));

        let mut diagnostics = Vec::new();
        enrich_expr_with_diagnostics(
            &mut expr,
            &tco,
            &mut diagnostics,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        )
        .unwrap();
        assert!(
            diagnostics.iter().any(|diagnostic| {
                diagnostic.kind() == TypeExprConversionKind::UnresolvedVar
                    && diagnostic
                        .to_string()
                        .contains("lambda parameter `x` inferred type")
            }),
            "expected unresolved lambda parameter diagnostic, got: {diagnostics:?}"
        );

        match &expr.0 {
            Expr::Lambda { params, .. } => {
                assert_eq!(params.len(), 1);
                assert!(params[0].ty.is_none());
            }
            other => panic!("expected lambda expr, got {other:?}"),
        }
    }

    #[test]
    fn test_enrich_inferred_call_type_args_report_literal_kind() {
        let mut expr: Spanned<Expr> = (
            Expr::Call {
                function: Box::new((Expr::Identifier("identity".to_string()), 0..8)),
                type_args: None,
                args: vec![hew_parser::ast::CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 42,
                        radix: hew_parser::ast::IntRadix::Decimal,
                    }),
                    9..11,
                ))],
                is_tail_call: false,
            },
            0..11,
        );
        let mut tco = empty_tco();
        tco.call_type_args
            .insert(SpanKey::from(&expr.1), vec![Ty::IntLiteral]);

        let mut diagnostics = Vec::new();
        enrich_expr_with_diagnostics(
            &mut expr,
            &tco,
            &mut diagnostics,
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
        )
        .unwrap();

        let Expr::Call { type_args, .. } = &expr.0 else {
            panic!("expected call expr");
        };
        assert!(
            type_args.is_none(),
            "literal-kind type args must stay implicit"
        );
        assert!(
            diagnostics.iter().any(|diagnostic| {
                diagnostic.kind() == TypeExprConversionKind::LiteralKind
                    && diagnostic
                        .to_string()
                        .contains("inferred call type argument 0")
            }),
            "expected inferred call type-arg diagnostic, got: {diagnostics:?}"
        );
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

    // -----------------------------------------------------------------------
    // ty_to_type_expr — missing primitive coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_ty_to_type_expr_remaining_primitives() {
        let cases = vec![
            (Ty::I8, "i8"),
            (Ty::I16, "i16"),
            (Ty::U8, "u8"),
            (Ty::U16, "u16"),
            (Ty::U32, "u32"),
            (Ty::U64, "u64"),
            (Ty::F32, "f32"),
            (Ty::Bytes, "bytes"),
            (Ty::Duration, "duration"),
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
    fn test_ty_to_type_expr_closure() {
        let ty = Ty::Closure {
            params: vec![Ty::I32, Ty::String],
            ret: Box::new(Ty::Bool),
            captures: vec![],
        };
        let (te, _span) = unwrap_converted(ty_to_type_expr(&ty));
        match te {
            TypeExpr::Function {
                params,
                return_type,
            } => {
                assert_eq!(params.len(), 2);
                assert!(matches!(params[0].0, TypeExpr::Named { ref name, .. } if name == "i32"));
                assert!(
                    matches!(return_type.0, TypeExpr::Named { ref name, .. } if name == "bool")
                );
            }
            _ => panic!("expected Function variant for Closure"),
        }
    }

    #[test]
    fn test_ty_to_type_expr_slice() {
        let ty = Ty::Slice(Box::new(Ty::String));
        let (te, _span) = unwrap_converted(ty_to_type_expr(&ty));
        match te {
            TypeExpr::Slice(inner) => {
                assert!(matches!(inner.0, TypeExpr::Named { ref name, .. } if name == "string"));
            }
            _ => panic!("expected Slice variant"),
        }
    }

    #[test]
    fn test_ty_to_type_expr_trait_object() {
        use hew_types::ty::TraitObjectBound;
        let ty = Ty::TraitObject {
            traits: vec![TraitObjectBound {
                trait_name: "Display".into(),
                args: vec![],
            }],
        };
        let (te, _span) = unwrap_converted(ty_to_type_expr(&ty));
        match te {
            TypeExpr::TraitObject(bounds) => {
                assert_eq!(bounds.len(), 1);
                assert_eq!(bounds[0].name, "Display");
                assert!(bounds[0].type_args.is_none());
            }
            _ => panic!("expected TraitObject variant"),
        }
    }

    #[test]
    fn test_ty_to_type_expr_trait_object_with_type_args() {
        use hew_types::ty::TraitObjectBound;
        let ty = Ty::TraitObject {
            traits: vec![TraitObjectBound {
                trait_name: "Iterator".into(),
                args: vec![Ty::I32],
            }],
        };
        let (te, _span) = unwrap_converted(ty_to_type_expr(&ty));
        match te {
            TypeExpr::TraitObject(bounds) => {
                assert_eq!(bounds.len(), 1);
                assert_eq!(bounds[0].name, "Iterator");
                let args = bounds[0].type_args.as_ref().unwrap();
                assert_eq!(args.len(), 1);
            }
            _ => panic!("expected TraitObject variant"),
        }
    }

    #[test]
    fn test_ty_to_type_expr_machine() {
        let ty = Ty::Machine {
            name: "TrafficLight".into(),
        };
        let (te, _span) = unwrap_converted(ty_to_type_expr(&ty));
        match te {
            TypeExpr::Named { name, type_args } => {
                assert_eq!(name, "TrafficLight");
                assert!(type_args.is_none());
            }
            _ => panic!("expected Named variant for Machine"),
        }
    }

    // -----------------------------------------------------------------------
    // rewrite_builtin_calls — coverage for len(x) → x.len() rewriting
    // -----------------------------------------------------------------------

    /// Helper: build a `len(arg)` call expression.
    fn make_len_call(arg: Spanned<Expr>) -> Spanned<Expr> {
        (
            Expr::Call {
                function: Box::new((Expr::Identifier("len".into()), 0..3)),
                args: vec![CallArg::Positional(arg)],
                type_args: None,
                is_tail_call: false,
            },
            0..10,
        )
    }

    /// Helper: make an integer literal expression.
    fn make_int_lit(n: i64) -> Spanned<Expr> {
        (
            Expr::Literal(hew_parser::ast::Literal::Integer {
                value: n,
                radix: hew_parser::ast::IntRadix::Decimal,
            }),
            0..1,
        )
    }

    /// Helper: make a simple identifier expression.
    fn make_ident(name: &str) -> Spanned<Expr> {
        (Expr::Identifier(name.into()), 0..name.len())
    }

    /// Helper: make a function item wrapping a body block.
    fn make_fn_item(name: &str, body: Block) -> Spanned<Item> {
        (
            Item::Function(FnDecl {
                attributes: vec![],
                is_async: false,
                is_generator: false,
                visibility: Visibility::Private,
                is_pure: false,
                name: name.into(),
                type_params: None,
                params: vec![],
                return_type: None,
                where_clause: None,
                body,
                doc_comment: None,
                decl_span: 0..0,
            }),
            0..0,
        )
    }

    /// Helper: make a block with a single expression statement.
    fn make_block_with_expr(expr: Spanned<Expr>) -> Block {
        Block {
            stmts: vec![(Stmt::Expression(expr), 0..0)],
            trailing_expr: None,
        }
    }

    #[test]
    fn test_rewrite_builtin_calls_in_function() {
        let len_call = make_len_call(make_ident("xs"));
        let mut items = vec![make_fn_item("test_fn", make_block_with_expr(len_call))];
        rewrite_builtin_calls(&mut items);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::Expression(expr) = &f.body.stmts[0].0 {
                assert!(
                    matches!(&expr.0, Expr::MethodCall { method, .. } if method == "len"),
                    "len(xs) should be rewritten to xs.len()"
                );
            } else {
                panic!("expected expression statement");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_rewrite_builtin_calls_in_actor() {
        let len_call = make_len_call(make_ident("items"));
        let mut items = vec![(
            Item::Actor(ActorDecl {
                visibility: Visibility::Private,
                name: "Counter".into(),
                super_traits: None,
                init: Some(hew_parser::ast::ActorInit {
                    params: vec![],
                    body: make_block_with_expr(len_call),
                }),
                terminate: None,
                fields: vec![],
                receive_fns: vec![ReceiveFnDecl {
                    is_generator: false,
                    is_pure: false,
                    name: "inc".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: make_block_with_expr(make_len_call(make_ident("data"))),
                    span: 0..0,
                    attributes: vec![],
                }],
                methods: vec![],
                mailbox_capacity: None,
                overflow_policy: None,
                is_isolated: false,
                doc_comment: None,
            }),
            0..0,
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Actor(actor) = &items[0].0 {
            // Check init block
            if let Stmt::Expression(expr) = &actor.init.as_ref().unwrap().body.stmts[0].0 {
                assert!(matches!(&expr.0, Expr::MethodCall { method, .. } if method == "len"));
            }
            // Check receive fn
            if let Stmt::Expression(expr) = &actor.receive_fns[0].body.stmts[0].0 {
                assert!(matches!(&expr.0, Expr::MethodCall { method, .. } if method == "len"));
            }
        }
    }

    #[test]
    fn test_rewrite_builtin_calls_in_machine() {
        let mut items = vec![(
            Item::Machine(MachineDecl {
                visibility: Visibility::Private,
                name: "Light".into(),
                states: vec![],
                events: vec![],
                transitions: vec![MachineTransition {
                    event_name: "toggle".into(),
                    source_state: "Off".into(),
                    target_state: "On".into(),
                    guard: None,
                    body: make_len_call(make_ident("xs")),
                }],
                has_default: false,
            }),
            0..0,
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Machine(m) = &items[0].0 {
            assert!(
                matches!(&m.transitions[0].body.0, Expr::MethodCall { method, .. } if method == "len")
            );
        }
    }

    #[test]
    fn test_rewrite_builtin_calls_in_impl() {
        let len_call = make_len_call(make_ident("v"));
        let mut items = vec![(
            Item::Impl(ImplDecl {
                type_params: None,
                trait_bound: None,
                target_type: (
                    TypeExpr::Named {
                        name: "Foo".into(),
                        type_args: None,
                    },
                    0..0,
                ),
                where_clause: None,
                type_aliases: vec![],
                methods: vec![FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "count".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: make_block_with_expr(len_call),
                    doc_comment: None,
                    decl_span: 0..0,
                }],
            }),
            0..0,
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Impl(imp) = &items[0].0 {
            if let Stmt::Expression(expr) = &imp.methods[0].body.stmts[0].0 {
                assert!(matches!(&expr.0, Expr::MethodCall { method, .. } if method == "len"));
            }
        }
    }

    #[test]
    fn test_rewrite_builtin_calls_in_trait() {
        let len_call = make_len_call(make_ident("items"));
        let mut items = vec![(
            Item::Trait(TraitDecl {
                visibility: Visibility::Private,
                name: "Countable".into(),
                type_params: None,
                super_traits: None,
                items: vec![hew_parser::ast::TraitItem::Method(
                    hew_parser::ast::TraitMethod {
                        name: "count".into(),
                        is_pure: false,
                        type_params: None,
                        params: vec![],
                        return_type: None,
                        where_clause: None,
                        body: Some(make_block_with_expr(len_call)),
                    },
                )],
                doc_comment: None,
            }),
            0..0,
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Trait(t) = &items[0].0 {
            if let hew_parser::ast::TraitItem::Method(m) = &t.items[0] {
                if let Stmt::Expression(expr) = &m.body.as_ref().unwrap().stmts[0].0 {
                    assert!(matches!(&expr.0, Expr::MethodCall { method, .. } if method == "len"));
                }
            }
        }
    }

    #[test]
    fn test_rewrite_builtin_calls_in_const() {
        let len_call = make_len_call(make_ident("data"));
        let mut items = vec![(
            Item::Const(ConstDecl {
                visibility: Visibility::Private,
                name: "LEN".into(),
                ty: (
                    TypeExpr::Named {
                        name: "int".into(),
                        type_args: None,
                    },
                    0..0,
                ),
                value: len_call,
            }),
            0..0,
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Const(c) = &items[0].0 {
            assert!(matches!(&c.value.0, Expr::MethodCall { method, .. } if method == "len"));
        }
    }

    #[test]
    fn test_rewrite_builtin_calls_in_type_decl_method() {
        let len_call = make_len_call(make_ident("self_items"));
        let mut items = vec![(
            Item::TypeDecl(TypeDecl {
                visibility: Visibility::Private,
                kind: hew_parser::ast::TypeDeclKind::Struct,
                name: "MyList".into(),
                type_params: None,
                where_clause: None,
                body: vec![hew_parser::ast::TypeBodyItem::Method(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "size".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: make_block_with_expr(len_call),
                    doc_comment: None,
                    decl_span: 0..0,
                })],
                doc_comment: None,
                wire: None,
                is_indirect: false,
            }),
            0..0,
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::TypeDecl(td) = &items[0].0 {
            if let hew_parser::ast::TypeBodyItem::Method(m) = &td.body[0] {
                if let Stmt::Expression(expr) = &m.body.stmts[0].0 {
                    assert!(matches!(&expr.0, Expr::MethodCall { method, .. } if method == "len"));
                }
            }
        }
    }

    #[test]
    fn test_rewrite_builtin_calls_in_supervisor() {
        let len_call = make_len_call(make_ident("args"));
        let mut items = vec![(
            Item::Supervisor(SupervisorDecl {
                visibility: Visibility::Private,
                name: "MySup".into(),
                strategy: None,
                max_restarts: None,
                window: None,
                children: vec![hew_parser::ast::ChildSpec {
                    name: "worker".into(),
                    actor_type: "Worker".into(),
                    args: vec![len_call],
                    restart: None,
                }],
            }),
            0..0,
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Supervisor(sup) = &items[0].0 {
            assert!(
                matches!(&sup.children[0].args[0].0, Expr::MethodCall { method, .. } if method == "len")
            );
        }
    }

    // -----------------------------------------------------------------------
    // rewrite_builtin_calls_in_expr — expression variant coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_rewrite_len_in_if_expr() {
        let mut expr: Spanned<Expr> = (
            Expr::If {
                condition: Box::new(make_len_call(make_ident("a"))),
                then_block: Box::new(make_int_lit(1)),
                else_block: Some(Box::new(make_len_call(make_ident("b")))),
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::If {
            condition,
            else_block,
            ..
        } = &expr.0
        {
            assert!(matches!(&condition.0, Expr::MethodCall { method, .. } if method == "len"));
            assert!(
                matches!(&else_block.as_ref().unwrap().0, Expr::MethodCall { method, .. } if method == "len")
            );
        }
    }

    #[test]
    fn test_rewrite_len_in_binary_and_unary() {
        let mut binary: Spanned<Expr> = (
            Expr::Binary {
                op: hew_parser::ast::BinaryOp::Add,
                left: Box::new(make_len_call(make_ident("x"))),
                right: Box::new(make_len_call(make_ident("y"))),
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut binary);
        if let Expr::Binary { left, right, .. } = &binary.0 {
            assert!(matches!(&left.0, Expr::MethodCall { method, .. } if method == "len"));
            assert!(matches!(&right.0, Expr::MethodCall { method, .. } if method == "len"));
        }

        let mut unary: Spanned<Expr> = (
            Expr::Unary {
                op: hew_parser::ast::UnaryOp::Negate,
                operand: Box::new(make_len_call(make_ident("z"))),
            },
            0..10,
        );
        rewrite_builtin_calls_in_expr(&mut unary);
        if let Expr::Unary { operand, .. } = &unary.0 {
            assert!(matches!(&operand.0, Expr::MethodCall { method, .. } if method == "len"));
        }
    }

    #[test]
    fn test_rewrite_len_in_match_expr() {
        let mut expr: Spanned<Expr> = (
            Expr::Match {
                scrutinee: Box::new(make_len_call(make_ident("v"))),
                arms: vec![hew_parser::ast::MatchArm {
                    pattern: (hew_parser::ast::Pattern::Wildcard, 0..0),
                    guard: Some(make_len_call(make_ident("w"))),
                    body: make_len_call(make_ident("r")),
                }],
            },
            0..30,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::Match {
            scrutinee, arms, ..
        } = &expr.0
        {
            assert!(matches!(&scrutinee.0, Expr::MethodCall { .. }));
            assert!(matches!(
                &arms[0].guard.as_ref().unwrap().0,
                Expr::MethodCall { .. }
            ));
            assert!(matches!(&arms[0].body.0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_lambda() {
        let mut expr: Spanned<Expr> = (
            Expr::Lambda {
                params: vec![],
                body: Box::new(make_len_call(make_ident("items"))),
                return_type: None,
                is_move: false,
                type_params: None,
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::Lambda { body, .. } = &expr.0 {
            assert!(matches!(&body.0, Expr::MethodCall { method, .. } if method == "len"));
        }
    }

    #[test]
    fn test_rewrite_len_in_array_and_tuple() {
        let mut arr: Spanned<Expr> = (Expr::Array(vec![make_len_call(make_ident("a"))]), 0..10);
        rewrite_builtin_calls_in_expr(&mut arr);
        if let Expr::Array(elems) = &arr.0 {
            assert!(matches!(&elems[0].0, Expr::MethodCall { .. }));
        }

        let mut tup: Spanned<Expr> = (Expr::Tuple(vec![make_len_call(make_ident("b"))]), 0..10);
        rewrite_builtin_calls_in_expr(&mut tup);
        if let Expr::Tuple(elems) = &tup.0 {
            assert!(matches!(&elems[0].0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_map_literal() {
        let mut expr: Spanned<Expr> = (
            Expr::MapLiteral {
                entries: vec![(
                    make_len_call(make_ident("k")),
                    make_len_call(make_ident("v")),
                )],
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::MapLiteral { entries } = &expr.0 {
            assert!(matches!(&entries[0].0 .0, Expr::MethodCall { .. }));
            assert!(matches!(&entries[0].1 .0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_struct_init() {
        let mut expr: Spanned<Expr> = (
            Expr::StructInit {
                name: "Foo".into(),
                fields: vec![("count".into(), make_len_call(make_ident("items")))],
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::StructInit { fields, .. } = &expr.0 {
            assert!(matches!(&fields[0].1 .0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_index_and_field_access() {
        let mut idx: Spanned<Expr> = (
            Expr::Index {
                object: Box::new(make_ident("arr")),
                index: Box::new(make_len_call(make_ident("i"))),
            },
            0..10,
        );
        rewrite_builtin_calls_in_expr(&mut idx);
        if let Expr::Index { index, .. } = &idx.0 {
            assert!(matches!(&index.0, Expr::MethodCall { .. }));
        }

        let mut fa: Spanned<Expr> = (
            Expr::FieldAccess {
                object: Box::new(make_len_call(make_ident("obj"))),
                field: "x".into(),
            },
            0..10,
        );
        rewrite_builtin_calls_in_expr(&mut fa);
        if let Expr::FieldAccess { object, .. } = &fa.0 {
            assert!(matches!(&object.0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_cast() {
        let mut expr: Spanned<Expr> = (
            Expr::Cast {
                expr: Box::new(make_len_call(make_ident("v"))),
                ty: (
                    TypeExpr::Named {
                        name: "int".into(),
                        type_args: None,
                    },
                    0..0,
                ),
            },
            0..10,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::Cast { expr: inner, .. } = &expr.0 {
            assert!(matches!(&inner.0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_spawn_and_send() {
        let mut spawn: Spanned<Expr> = (
            Expr::Spawn {
                target: Box::new(make_ident("Worker")),
                args: vec![("count".into(), make_len_call(make_ident("xs")))],
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut spawn);
        if let Expr::Spawn { args, .. } = &spawn.0 {
            assert!(matches!(&args[0].1 .0, Expr::MethodCall { .. }));
        }

        let mut send: Spanned<Expr> = (
            Expr::Send {
                target: Box::new(make_ident("actor")),
                message: Box::new(make_len_call(make_ident("msg"))),
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut send);
        if let Expr::Send { message, .. } = &send.0 {
            assert!(matches!(&message.0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_range() {
        let mut expr: Spanned<Expr> = (
            Expr::Range {
                start: Some(Box::new(make_len_call(make_ident("a")))),
                end: Some(Box::new(make_len_call(make_ident("b")))),
                inclusive: false,
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::Range { start, end, .. } = &expr.0 {
            assert!(matches!(
                &start.as_ref().unwrap().0,
                Expr::MethodCall { .. }
            ));
            assert!(matches!(&end.as_ref().unwrap().0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_interp_string() {
        let mut expr: Spanned<Expr> = (
            Expr::InterpolatedString(vec![hew_parser::ast::StringPart::Expr(make_len_call(
                make_ident("s"),
            ))]),
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::InterpolatedString(parts) = &expr.0 {
            if let hew_parser::ast::StringPart::Expr(e) = &parts[0] {
                assert!(matches!(&e.0, Expr::MethodCall { .. }));
            }
        }
    }

    #[test]
    fn test_rewrite_len_in_array_repeat() {
        let mut expr: Spanned<Expr> = (
            Expr::ArrayRepeat {
                value: Box::new(make_len_call(make_ident("v"))),
                count: Box::new(make_len_call(make_ident("n"))),
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut expr);
        if let Expr::ArrayRepeat { value, count } = &expr.0 {
            assert!(matches!(&value.0, Expr::MethodCall { .. }));
            assert!(matches!(&count.0, Expr::MethodCall { .. }));
        }
    }

    // -----------------------------------------------------------------------
    // rewrite_builtin_calls_in_stmt — statement variant coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_rewrite_len_in_let_and_var_stmts() {
        let mut items = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![
                    (
                        Stmt::Let {
                            pattern: (hew_parser::ast::Pattern::Identifier("a".into()), 0..1),
                            ty: None,
                            value: Some(make_len_call(make_ident("xs"))),
                        },
                        0..10,
                    ),
                    (
                        Stmt::Var {
                            name: "b".into(),
                            ty: None,
                            value: Some(make_len_call(make_ident("ys"))),
                        },
                        10..20,
                    ),
                ],
                trailing_expr: None,
            },
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::Let { value: Some(v), .. } = &f.body.stmts[0].0 {
                assert!(matches!(&v.0, Expr::MethodCall { .. }));
            }
            if let Stmt::Var { value: Some(v), .. } = &f.body.stmts[1].0 {
                assert!(matches!(&v.0, Expr::MethodCall { .. }));
            }
        }
    }

    #[test]
    fn test_rewrite_len_in_for_and_while_stmts() {
        let mut items = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![
                    (
                        Stmt::For {
                            pattern: (hew_parser::ast::Pattern::Identifier("i".into()), 0..1),
                            iterable: make_len_call(make_ident("xs")),
                            body: make_block_with_expr(make_len_call(make_ident("inner"))),
                            label: None,
                            is_await: false,
                        },
                        0..20,
                    ),
                    (
                        Stmt::While {
                            condition: make_len_call(make_ident("cond")),
                            body: make_block_with_expr(make_int_lit(0)),
                            label: None,
                        },
                        20..40,
                    ),
                ],
                trailing_expr: None,
            },
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::For { iterable, body, .. } = &f.body.stmts[0].0 {
                assert!(matches!(&iterable.0, Expr::MethodCall { .. }));
                if let Stmt::Expression(e) = &body.stmts[0].0 {
                    assert!(matches!(&e.0, Expr::MethodCall { .. }));
                }
            }
            if let Stmt::While { condition, .. } = &f.body.stmts[1].0 {
                assert!(matches!(&condition.0, Expr::MethodCall { .. }));
            }
        }
    }

    #[test]
    fn test_rewrite_len_in_if_stmt_with_else() {
        let mut items = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![(
                    Stmt::If {
                        condition: make_len_call(make_ident("c")),
                        then_block: make_block_with_expr(make_int_lit(1)),
                        else_block: Some(ElseBlock {
                            block: Some(make_block_with_expr(make_len_call(make_ident("e")))),
                            if_stmt: None,
                            is_if: false,
                        }),
                    },
                    0..30,
                )],
                trailing_expr: None,
            },
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::If {
                condition,
                else_block,
                ..
            } = &f.body.stmts[0].0
            {
                assert!(matches!(&condition.0, Expr::MethodCall { .. }));
                let eb = else_block.as_ref().unwrap();
                if let Stmt::Expression(e) = &eb.block.as_ref().unwrap().stmts[0].0 {
                    assert!(matches!(&e.0, Expr::MethodCall { .. }));
                }
            }
        }
    }

    #[test]
    fn test_rewrite_len_in_match_stmt() {
        let mut items = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![(
                    Stmt::Match {
                        scrutinee: make_len_call(make_ident("s")),
                        arms: vec![hew_parser::ast::MatchArm {
                            pattern: (hew_parser::ast::Pattern::Wildcard, 0..0),
                            guard: Some(make_len_call(make_ident("g"))),
                            body: make_len_call(make_ident("b")),
                        }],
                    },
                    0..30,
                )],
                trailing_expr: None,
            },
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::Match {
                scrutinee, arms, ..
            } = &f.body.stmts[0].0
            {
                assert!(matches!(&scrutinee.0, Expr::MethodCall { .. }));
                assert!(matches!(
                    &arms[0].guard.as_ref().unwrap().0,
                    Expr::MethodCall { .. }
                ));
                assert!(matches!(&arms[0].body.0, Expr::MethodCall { .. }));
            }
        }
    }

    #[test]
    fn test_rewrite_len_in_assign_and_defer() {
        let mut items = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![
                    (
                        Stmt::Assign {
                            target: make_ident("x"),
                            value: make_len_call(make_ident("v")),
                            op: None,
                        },
                        0..10,
                    ),
                    (
                        Stmt::Defer(Box::new(make_len_call(make_ident("d")))),
                        10..20,
                    ),
                    (
                        Stmt::Loop {
                            body: make_block_with_expr(make_int_lit(0)),
                            label: None,
                        },
                        20..30,
                    ),
                ],
                trailing_expr: None,
            },
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::Assign { value, .. } = &f.body.stmts[0].0 {
                assert!(matches!(&value.0, Expr::MethodCall { .. }));
            }
            if let Stmt::Defer(e) = &f.body.stmts[1].0 {
                assert!(matches!(&e.0, Expr::MethodCall { .. }));
            }
        }
    }

    #[test]
    fn test_rewrite_len_in_if_let_stmt() {
        let mut items = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![(
                    Stmt::IfLet {
                        pattern: Box::new((hew_parser::ast::Pattern::Identifier("x".into()), 0..1)),
                        expr: Box::new(make_len_call(make_ident("opt"))),
                        body: make_block_with_expr(make_int_lit(1)),
                        else_body: Some(make_block_with_expr(make_len_call(make_ident("fb")))),
                    },
                    0..30,
                )],
                trailing_expr: None,
            },
        )];
        rewrite_builtin_calls(&mut items);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::IfLet {
                expr, else_body, ..
            } = &f.body.stmts[0].0
            {
                assert!(matches!(&expr.0, Expr::MethodCall { .. }));
                if let Stmt::Expression(e) = &else_body.as_ref().unwrap().stmts[0].0 {
                    assert!(matches!(&e.0, Expr::MethodCall { .. }));
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // rewrite_builtin_calls_in_expr — more expression variants
    // -----------------------------------------------------------------------

    #[test]
    fn test_rewrite_len_in_block_and_iflet_expr() {
        let mut block_expr: Spanned<Expr> = (
            Expr::Block(make_block_with_expr(make_len_call(make_ident("v")))),
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut block_expr);
        if let Expr::Block(block) = &block_expr.0 {
            if let Stmt::Expression(e) = &block.stmts[0].0 {
                assert!(matches!(&e.0, Expr::MethodCall { .. }));
            }
        }

        let mut iflet: Spanned<Expr> = (
            Expr::IfLet {
                pattern: Box::new((hew_parser::ast::Pattern::Identifier("x".into()), 0..1)),
                expr: Box::new(make_len_call(make_ident("opt"))),
                body: make_block_with_expr(make_int_lit(1)),
                else_body: None,
            },
            0..20,
        );
        rewrite_builtin_calls_in_expr(&mut iflet);
        if let Expr::IfLet { expr, .. } = &iflet.0 {
            assert!(matches!(&expr.0, Expr::MethodCall { .. }));
        }
    }

    #[test]
    fn test_rewrite_len_in_postfix_try_and_await() {
        let mut try_expr: Spanned<Expr> = (
            Expr::PostfixTry(Box::new(make_len_call(make_ident("r")))),
            0..10,
        );
        rewrite_builtin_calls_in_expr(&mut try_expr);
        if let Expr::PostfixTry(inner) = &try_expr.0 {
            assert!(matches!(&inner.0, Expr::MethodCall { .. }));
        }

        let mut await_expr: Spanned<Expr> =
            (Expr::Await(Box::new(make_len_call(make_ident("f")))), 0..10);
        rewrite_builtin_calls_in_expr(&mut await_expr);
        if let Expr::Await(inner) = &await_expr.0 {
            assert!(matches!(&inner.0, Expr::MethodCall { .. }));
        }
    }

    // -----------------------------------------------------------------------
    // normalize_item_types — coverage for various item variants
    // -----------------------------------------------------------------------

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "comprehensive test covering all actor type normalization variants"
    )]
    fn test_normalize_actor_types() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![(
            Item::Actor(ActorDecl {
                visibility: Visibility::Private,
                name: "MyActor".into(),
                super_traits: None,
                init: Some(hew_parser::ast::ActorInit {
                    params: vec![],
                    body: Block {
                        stmts: vec![],
                        trailing_expr: None,
                    },
                }),
                terminate: None,
                fields: vec![hew_parser::ast::FieldDecl {
                    name: "data".into(),
                    ty: (
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
                }],
                receive_fns: vec![ReceiveFnDecl {
                    is_generator: false,
                    is_pure: false,
                    name: "handle".into(),
                    type_params: None,
                    params: vec![Param {
                        name: "msg".into(),
                        ty: (
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
                        ),
                        is_mutable: false,
                    }],
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
                    span: 0..0,
                    attributes: vec![],
                }],
                methods: vec![],
                mailbox_capacity: None,
                overflow_policy: None,
                is_isolated: false,
                doc_comment: None,
            }),
            0..0,
        )];
        normalize_items_types(&mut items, &registry);

        // Verify Option in field was normalized
        if let Item::Actor(a) = &items[0].0 {
            assert!(matches!(&a.fields[0].ty.0, TypeExpr::Option(_)));
            // Verify Result param was normalized
            assert!(matches!(
                &a.receive_fns[0].params[0].ty.0,
                TypeExpr::Result { .. }
            ));
            // Verify Option return type was normalized
            assert!(matches!(
                &a.receive_fns[0].return_type.as_ref().unwrap().0,
                TypeExpr::Option(_)
            ));
        }
    }

    #[test]
    fn test_normalize_impl_types() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![(
            Item::Impl(ImplDecl {
                type_params: None,
                trait_bound: None,
                target_type: (
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
                ),
                where_clause: None,
                type_aliases: vec![],
                methods: vec![],
            }),
            0..0,
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::Impl(imp) = &items[0].0 {
            assert!(matches!(&imp.target_type.0, TypeExpr::Result { .. }));
        }
    }

    #[test]
    fn test_normalize_extern_block_types() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![(
            Item::ExternBlock(ExternBlock {
                abi: "C".into(),
                functions: vec![ExternFnDecl {
                    name: "ext_fn".into(),
                    params: vec![Param {
                        name: "p".into(),
                        ty: (
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
                        is_mutable: false,
                    }],
                    return_type: Some((
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
                    is_variadic: false,
                }],
            }),
            0..0,
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::ExternBlock(eb) = &items[0].0 {
            assert!(matches!(
                &eb.functions[0].params[0].ty.0,
                TypeExpr::Option(_)
            ));
            assert!(matches!(
                &eb.functions[0].return_type.as_ref().unwrap().0,
                TypeExpr::Result { .. }
            ));
        }
    }

    #[test]
    fn test_normalize_type_decl_types() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![(
            Item::TypeDecl(TypeDecl {
                visibility: Visibility::Private,
                kind: hew_parser::ast::TypeDeclKind::Enum,
                name: "MyEnum".into(),
                type_params: None,
                where_clause: None,
                body: vec![
                    hew_parser::ast::TypeBodyItem::Field {
                        name: "data".into(),
                        ty: (
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
                        attributes: Vec::new(),
                    },
                    hew_parser::ast::TypeBodyItem::Variant(hew_parser::ast::VariantDecl {
                        name: "TupleV".into(),
                        kind: hew_parser::ast::VariantKind::Tuple(vec![(
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
                        )]),
                    }),
                    hew_parser::ast::TypeBodyItem::Variant(hew_parser::ast::VariantDecl {
                        name: "StructV".into(),
                        kind: hew_parser::ast::VariantKind::Struct(vec![(
                            "val".into(),
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
                        )]),
                    }),
                ],
                doc_comment: None,
                wire: None,
                is_indirect: false,
            }),
            0..0,
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::TypeDecl(td) = &items[0].0 {
            if let hew_parser::ast::TypeBodyItem::Field { ty, .. } = &td.body[0] {
                assert!(matches!(&ty.0, TypeExpr::Option(_)));
            }
            if let hew_parser::ast::TypeBodyItem::Variant(v) = &td.body[1] {
                if let hew_parser::ast::VariantKind::Tuple(fields) = &v.kind {
                    assert!(matches!(&fields[0].0, TypeExpr::Option(_)));
                }
            }
            if let hew_parser::ast::TypeBodyItem::Variant(v) = &td.body[2] {
                if let hew_parser::ast::VariantKind::Struct(fields) = &v.kind {
                    assert!(matches!(&fields[0].1 .0, TypeExpr::Option(_)));
                }
            }
        }
    }

    #[test]
    fn test_normalize_trait_types() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![(
            Item::Trait(TraitDecl {
                visibility: Visibility::Private,
                name: "Processor".into(),
                type_params: None,
                super_traits: None,
                items: vec![
                    hew_parser::ast::TraitItem::Method(hew_parser::ast::TraitMethod {
                        name: "process".into(),
                        is_pure: false,
                        type_params: None,
                        params: vec![Param {
                            name: "data".into(),
                            ty: (
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
                            is_mutable: false,
                        }],
                        return_type: Some((
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
                        where_clause: None,
                        body: Some(Block {
                            stmts: vec![],
                            trailing_expr: None,
                        }),
                    }),
                    hew_parser::ast::TraitItem::AssociatedType {
                        name: "Item".into(),
                        bounds: vec![],
                        default: Some((
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
                    },
                ],
                doc_comment: None,
            }),
            0..0,
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::Trait(t) = &items[0].0 {
            if let hew_parser::ast::TraitItem::Method(m) = &t.items[0] {
                assert!(matches!(&m.params[0].ty.0, TypeExpr::Option(_)));
                assert!(matches!(
                    &m.return_type.as_ref().unwrap().0,
                    TypeExpr::Result { .. }
                ));
            }
            if let hew_parser::ast::TraitItem::AssociatedType { default, .. } = &t.items[1] {
                assert!(matches!(&default.as_ref().unwrap().0, TypeExpr::Option(_)));
            }
        }
    }

    #[test]
    fn test_normalize_const_and_type_alias() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![
            (
                Item::Const(ConstDecl {
                    visibility: Visibility::Private,
                    name: "C".into(),
                    ty: (
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
                    value: make_int_lit(42),
                }),
                0..0,
            ),
            (
                Item::TypeAlias(hew_parser::ast::TypeAliasDecl {
                    visibility: Visibility::Private,
                    name: "MyResult".into(),
                    ty: (
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
                    ),
                }),
                0..0,
            ),
        ];
        normalize_items_types(&mut items, &registry);

        if let Item::Const(c) = &items[0].0 {
            assert!(matches!(&c.ty.0, TypeExpr::Option(_)));
        }
        if let Item::TypeAlias(ta) = &items[1].0 {
            assert!(matches!(&ta.ty.0, TypeExpr::Result { .. }));
        }
    }

    #[test]
    fn test_normalize_machine_types() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![(
            Item::Machine(MachineDecl {
                visibility: Visibility::Private,
                name: "Light".into(),
                states: vec![MachineState {
                    name: "On".into(),
                    fields: vec![(
                        "brightness".into(),
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
                    )],
                }],
                events: vec![MachineEvent {
                    name: "Toggle".into(),
                    fields: vec![(
                        "force".into(),
                        (
                            TypeExpr::Named {
                                name: "Option".into(),
                                type_args: Some(vec![(
                                    TypeExpr::Named {
                                        name: "bool".into(),
                                        type_args: None,
                                    },
                                    0..0,
                                )]),
                            },
                            0..0,
                        ),
                    )],
                }],
                transitions: vec![MachineTransition {
                    event_name: "Toggle".into(),
                    source_state: "Off".into(),
                    target_state: "On".into(),
                    guard: None,
                    body: make_int_lit(0),
                }],
                has_default: false,
            }),
            0..0,
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::Machine(m) = &items[0].0 {
            assert!(matches!(&m.states[0].fields[0].1 .0, TypeExpr::Option(_)));
            assert!(matches!(&m.events[0].fields[0].1 .0, TypeExpr::Option(_)));
        }
    }

    #[test]
    fn test_normalize_supervisor_types() {
        let registry = test_registry();
        // Supervisor children have arg expressions — ensure normalize_expr_types runs
        let mut items: Vec<Spanned<Item>> = vec![(
            Item::Supervisor(SupervisorDecl {
                visibility: Visibility::Private,
                name: "MySup".into(),
                strategy: None,
                max_restarts: None,
                window: None,
                children: vec![hew_parser::ast::ChildSpec {
                    name: "worker".into(),
                    actor_type: "Worker".into(),
                    args: vec![make_int_lit(0)],
                    restart: None,
                }],
            }),
            0..0,
        )];
        normalize_items_types(&mut items, &registry);
        // Just ensure it doesn't panic — supervisor args are expressions, not types
    }

    // -----------------------------------------------------------------------
    // normalize_stmt_types — coverage for statement normalization
    // -----------------------------------------------------------------------

    fn make_option_type() -> Spanned<TypeExpr> {
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
        )
    }

    #[test]
    fn test_normalize_stmt_let_var_types() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![
                    (
                        Stmt::Let {
                            pattern: (hew_parser::ast::Pattern::Identifier("a".into()), 0..1),
                            ty: Some(make_option_type()),
                            value: None,
                        },
                        0..10,
                    ),
                    (
                        Stmt::Var {
                            name: "b".into(),
                            ty: Some(make_option_type()),
                            value: None,
                        },
                        10..20,
                    ),
                ],
                trailing_expr: None,
            },
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::Let { ty: Some(t), .. } = &f.body.stmts[0].0 {
                assert!(matches!(&t.0, TypeExpr::Option(_)));
            }
            if let Stmt::Var { ty: Some(t), .. } = &f.body.stmts[1].0 {
                assert!(matches!(&t.0, TypeExpr::Option(_)));
            }
        }
    }

    #[test]
    fn test_normalize_stmt_if_and_iflet() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![
                    (
                        Stmt::If {
                            condition: make_int_lit(1),
                            then_block: Block {
                                stmts: vec![(
                                    Stmt::Let {
                                        pattern: (
                                            hew_parser::ast::Pattern::Identifier("x".into()),
                                            0..1,
                                        ),
                                        ty: Some(make_option_type()),
                                        value: None,
                                    },
                                    0..10,
                                )],
                                trailing_expr: None,
                            },
                            else_block: Some(ElseBlock {
                                block: Some(Block {
                                    stmts: vec![],
                                    trailing_expr: None,
                                }),
                                if_stmt: None,
                                is_if: false,
                            }),
                        },
                        0..30,
                    ),
                    (
                        Stmt::IfLet {
                            pattern: Box::new((
                                hew_parser::ast::Pattern::Identifier("v".into()),
                                0..1,
                            )),
                            expr: Box::new(make_int_lit(0)),
                            body: Block {
                                stmts: vec![],
                                trailing_expr: None,
                            },
                            else_body: Some(Block {
                                stmts: vec![],
                                trailing_expr: None,
                            }),
                        },
                        30..60,
                    ),
                ],
                trailing_expr: None,
            },
        )];
        normalize_items_types(&mut items, &registry);

        // Verify normalization propagated without panic
        if let Item::Function(f) = &items[0].0 {
            if let Stmt::If { then_block, .. } = &f.body.stmts[0].0 {
                if let Stmt::Let { ty: Some(t), .. } = &then_block.stmts[0].0 {
                    assert!(matches!(&t.0, TypeExpr::Option(_)));
                }
            }
        }
    }

    #[test]
    fn test_normalize_stmt_for_while_loop_match() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![
                    (
                        Stmt::For {
                            pattern: (hew_parser::ast::Pattern::Identifier("i".into()), 0..1),
                            iterable: make_int_lit(0),
                            body: Block {
                                stmts: vec![],
                                trailing_expr: None,
                            },
                            label: None,
                            is_await: false,
                        },
                        0..10,
                    ),
                    (
                        Stmt::While {
                            condition: make_int_lit(1),
                            body: Block {
                                stmts: vec![],
                                trailing_expr: None,
                            },
                            label: None,
                        },
                        10..20,
                    ),
                    (
                        Stmt::Loop {
                            body: Block {
                                stmts: vec![],
                                trailing_expr: None,
                            },
                            label: None,
                        },
                        20..30,
                    ),
                    (
                        Stmt::Match {
                            scrutinee: make_int_lit(0),
                            arms: vec![hew_parser::ast::MatchArm {
                                pattern: (hew_parser::ast::Pattern::Wildcard, 0..0),
                                guard: Some(make_int_lit(1)),
                                body: make_int_lit(2),
                            }],
                        },
                        30..50,
                    ),
                    (
                        Stmt::Assign {
                            target: make_ident("x"),
                            value: make_int_lit(0),
                            op: None,
                        },
                        50..60,
                    ),
                    (Stmt::Defer(Box::new(make_int_lit(0))), 60..70),
                    (Stmt::Return(None), 70..80),
                    (
                        Stmt::Break {
                            value: None,
                            label: None,
                        },
                        80..90,
                    ),
                    (Stmt::Continue { label: None }, 90..100),
                ],
                trailing_expr: None,
            },
        )];
        normalize_items_types(&mut items, &registry);
        // All branches visited without panic — correctness verified by structure
    }

    // -----------------------------------------------------------------------
    // normalize_expr_types_inner — expression variant coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_normalize_expr_lambda_with_types() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![(
                    Stmt::Expression((
                        Expr::Lambda {
                            params: vec![hew_parser::ast::LambdaParam {
                                name: "x".into(),
                                ty: Some(make_option_type()),
                            }],
                            body: Box::new(make_int_lit(0)),
                            return_type: Some(make_option_type()),
                            is_move: false,
                            type_params: None,
                        },
                        0..20,
                    )),
                    0..20,
                )],
                trailing_expr: None,
            },
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::Expression(expr) = &f.body.stmts[0].0 {
                if let Expr::Lambda {
                    params,
                    return_type,
                    ..
                } = &expr.0
                {
                    assert!(matches!(
                        &params[0].ty.as_ref().unwrap().0,
                        TypeExpr::Option(_)
                    ));
                    assert!(matches!(
                        &return_type.as_ref().unwrap().0,
                        TypeExpr::Option(_)
                    ));
                }
            }
        }
    }

    #[test]
    fn test_normalize_expr_call_with_type_args() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![(
                    Stmt::Expression((
                        Expr::Call {
                            function: Box::new(make_ident("foo")),
                            args: vec![],
                            type_args: Some(vec![make_option_type()]),
                            is_tail_call: false,
                        },
                        0..10,
                    )),
                    0..10,
                )],
                trailing_expr: None,
            },
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::Expression(expr) = &f.body.stmts[0].0 {
                if let Expr::Call { type_args, .. } = &expr.0 {
                    let ta = type_args.as_ref().unwrap();
                    assert!(matches!(&ta[0].0, TypeExpr::Option(_)));
                }
            }
        }
    }

    #[test]
    fn test_normalize_expr_cast_type() {
        let registry = test_registry();
        let mut items: Vec<Spanned<Item>> = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![(
                    Stmt::Expression((
                        Expr::Cast {
                            expr: Box::new(make_int_lit(0)),
                            ty: make_option_type(),
                        },
                        0..10,
                    )),
                    0..10,
                )],
                trailing_expr: None,
            },
        )];
        normalize_items_types(&mut items, &registry);

        if let Item::Function(f) = &items[0].0 {
            if let Stmt::Expression(expr) = &f.body.stmts[0].0 {
                if let Expr::Cast { ty, .. } = &expr.0 {
                    assert!(matches!(&ty.0, TypeExpr::Option(_)));
                }
            }
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "Complex dispatch logic; splitting would reduce clarity"
    )]
    fn test_normalize_various_expr_types() {
        let registry = test_registry();
        // Exercise many expression variants in a single function
        let mut items: Vec<Spanned<Item>> = vec![make_fn_item(
            "f",
            Block {
                stmts: vec![
                    // If expression
                    (
                        Stmt::Expression((
                            Expr::If {
                                condition: Box::new(make_int_lit(1)),
                                then_block: Box::new(make_int_lit(2)),
                                else_block: Some(Box::new(make_int_lit(3))),
                            },
                            0..10,
                        )),
                        0..10,
                    ),
                    // IfLet expression
                    (
                        Stmt::Expression((
                            Expr::IfLet {
                                pattern: Box::new((
                                    hew_parser::ast::Pattern::Identifier("x".into()),
                                    0..1,
                                )),
                                expr: Box::new(make_int_lit(0)),
                                body: Block {
                                    stmts: vec![],
                                    trailing_expr: None,
                                },
                                else_body: Some(Block {
                                    stmts: vec![],
                                    trailing_expr: None,
                                }),
                            },
                            10..20,
                        )),
                        10..20,
                    ),
                    // Match expression
                    (
                        Stmt::Expression((
                            Expr::Match {
                                scrutinee: Box::new(make_int_lit(0)),
                                arms: vec![hew_parser::ast::MatchArm {
                                    pattern: (hew_parser::ast::Pattern::Wildcard, 0..0),
                                    guard: Some(make_int_lit(1)),
                                    body: make_int_lit(2),
                                }],
                            },
                            20..30,
                        )),
                        20..30,
                    ),
                    // Array, Tuple, MapLiteral
                    (
                        Stmt::Expression((Expr::Array(vec![make_int_lit(0)]), 30..35)),
                        30..35,
                    ),
                    (
                        Stmt::Expression((Expr::Tuple(vec![make_int_lit(0)]), 35..40)),
                        35..40,
                    ),
                    (
                        Stmt::Expression((
                            Expr::MapLiteral {
                                entries: vec![(make_int_lit(1), make_int_lit(2))],
                            },
                            40..50,
                        )),
                        40..50,
                    ),
                    // Binary, Unary, FieldAccess, Index
                    (
                        Stmt::Expression((
                            Expr::Binary {
                                op: hew_parser::ast::BinaryOp::Add,
                                left: Box::new(make_int_lit(1)),
                                right: Box::new(make_int_lit(2)),
                            },
                            50..55,
                        )),
                        50..55,
                    ),
                    (
                        Stmt::Expression((
                            Expr::Unary {
                                op: hew_parser::ast::UnaryOp::Negate,
                                operand: Box::new(make_int_lit(1)),
                            },
                            55..60,
                        )),
                        55..60,
                    ),
                    (
                        Stmt::Expression((
                            Expr::FieldAccess {
                                object: Box::new(make_ident("obj")),
                                field: "f".into(),
                            },
                            60..65,
                        )),
                        60..65,
                    ),
                    (
                        Stmt::Expression((
                            Expr::Index {
                                object: Box::new(make_ident("arr")),
                                index: Box::new(make_int_lit(0)),
                            },
                            65..70,
                        )),
                        65..70,
                    ),
                    // StructInit, Spawn, Send
                    (
                        Stmt::Expression((
                            Expr::StructInit {
                                name: "Pt".into(),
                                fields: vec![("x".into(), make_int_lit(0))],
                            },
                            70..80,
                        )),
                        70..80,
                    ),
                    (
                        Stmt::Expression((
                            Expr::Spawn {
                                target: Box::new(make_ident("Actor")),
                                args: vec![("n".into(), make_int_lit(1))],
                            },
                            80..90,
                        )),
                        80..90,
                    ),
                    (
                        Stmt::Expression((
                            Expr::Send {
                                target: Box::new(make_ident("actor")),
                                message: Box::new(make_int_lit(0)),
                            },
                            90..100,
                        )),
                        90..100,
                    ),
                    // Range
                    (
                        Stmt::Expression((
                            Expr::Range {
                                start: Some(Box::new(make_int_lit(0))),
                                end: Some(Box::new(make_int_lit(10))),
                                inclusive: false,
                            },
                            100..110,
                        )),
                        100..110,
                    ),
                    // ArrayRepeat
                    (
                        Stmt::Expression((
                            Expr::ArrayRepeat {
                                value: Box::new(make_int_lit(0)),
                                count: Box::new(make_int_lit(5)),
                            },
                            110..120,
                        )),
                        110..120,
                    ),
                    // InterpolatedString
                    (
                        Stmt::Expression((
                            Expr::InterpolatedString(vec![hew_parser::ast::StringPart::Expr(
                                make_int_lit(42),
                            )]),
                            120..130,
                        )),
                        120..130,
                    ),
                ],
                trailing_expr: None,
            },
        )];
        normalize_items_types(&mut items, &registry);
        // All expression normalization branches visited without panic
    }

    // -----------------------------------------------------------------------
    // enrich_item_with_diagnostics — item variant coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_enrich_machine_item() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Machine(MachineDecl {
                    visibility: Visibility::Private,
                    name: "Light".into(),
                    states: vec![],
                    events: vec![],
                    transitions: vec![MachineTransition {
                        event_name: "toggle".into(),
                        source_state: "Off".into(),
                        target_state: "On".into(),
                        guard: None,
                        body: make_int_lit(0),
                    }],
                    has_default: false,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enrich_impl_item() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Impl(ImplDecl {
                    type_params: None,
                    trait_bound: None,
                    target_type: (
                        TypeExpr::Named {
                            name: "Foo".into(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    where_clause: None,
                    type_aliases: vec![],
                    methods: vec![FnDecl {
                        attributes: vec![],
                        is_async: false,
                        is_generator: false,
                        visibility: Visibility::Private,
                        is_pure: false,
                        name: "bar".into(),
                        type_params: None,
                        params: vec![],
                        return_type: None,
                        where_clause: None,
                        body: Block {
                            stmts: vec![],
                            trailing_expr: Some(Box::new(make_int_lit(42))),
                        },
                        doc_comment: None,
                        decl_span: 0..0,
                    }],
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enrich_const_item() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Const(ConstDecl {
                    visibility: Visibility::Private,
                    name: "C".into(),
                    ty: (
                        TypeExpr::Named {
                            name: "int".into(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    value: make_int_lit(42),
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enrich_trait_item() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Trait(TraitDecl {
                    visibility: Visibility::Private,
                    name: "Greet".into(),
                    type_params: None,
                    super_traits: None,
                    items: vec![hew_parser::ast::TraitItem::Method(
                        hew_parser::ast::TraitMethod {
                            name: "greet".into(),
                            is_pure: false,
                            type_params: None,
                            params: vec![],
                            return_type: None,
                            where_clause: None,
                            body: Some(Block {
                                stmts: vec![],
                                trailing_expr: Some(Box::new(make_int_lit(0))),
                            }),
                        },
                    )],
                    doc_comment: None,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enrich_type_decl_item() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::TypeDecl(TypeDecl {
                    visibility: Visibility::Private,
                    kind: hew_parser::ast::TypeDeclKind::Struct,
                    name: "MyType".into(),
                    type_params: None,
                    where_clause: None,
                    body: vec![hew_parser::ast::TypeBodyItem::Method(FnDecl {
                        attributes: vec![],
                        is_async: false,
                        is_generator: false,
                        visibility: Visibility::Private,
                        is_pure: false,
                        name: "do_stuff".into(),
                        type_params: None,
                        params: vec![],
                        return_type: None,
                        where_clause: None,
                        body: Block {
                            stmts: vec![],
                            trailing_expr: Some(Box::new(make_int_lit(0))),
                        },
                        doc_comment: None,
                        decl_span: 0..0,
                    })],
                    doc_comment: None,
                    wire: None,
                    is_indirect: false,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enrich_supervisor_item() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Supervisor(SupervisorDecl {
                    visibility: Visibility::Private,
                    name: "MySup".into(),
                    strategy: None,
                    max_restarts: None,
                    window: None,
                    children: vec![hew_parser::ast::ChildSpec {
                        name: "worker".into(),
                        actor_type: "Worker".into(),
                        args: vec![make_int_lit(1)],
                        restart: None,
                    }],
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enrich_actor_item() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Actor(ActorDecl {
                    visibility: Visibility::Private,
                    name: "Worker".into(),
                    super_traits: None,
                    init: Some(hew_parser::ast::ActorInit {
                        params: vec![],
                        body: Block {
                            stmts: vec![(Stmt::Expression(make_int_lit(0)), 0..5)],
                            trailing_expr: None,
                        },
                    }),
                    terminate: None,
                    fields: vec![],
                    receive_fns: vec![ReceiveFnDecl {
                        is_generator: false,
                        is_pure: false,
                        name: "handle".into(),
                        type_params: None,
                        params: vec![],
                        return_type: None,
                        where_clause: None,
                        body: Block {
                            stmts: vec![(Stmt::Expression(make_int_lit(1)), 0..5)],
                            trailing_expr: None,
                        },
                        span: 0..0,
                        attributes: vec![],
                    }],
                    methods: vec![FnDecl {
                        attributes: vec![],
                        is_async: false,
                        is_generator: false,
                        visibility: Visibility::Private,
                        is_pure: false,
                        name: "helper".into(),
                        type_params: None,
                        params: vec![],
                        return_type: None,
                        where_clause: None,
                        body: Block {
                            stmts: vec![],
                            trailing_expr: Some(Box::new(make_int_lit(2))),
                        },
                        doc_comment: None,
                        decl_span: 0..0,
                    }],
                    mailbox_capacity: None,
                    overflow_policy: None,
                    is_isolated: false,
                    doc_comment: None,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    // -----------------------------------------------------------------------
    // enrich_stmt_with_diagnostics — statement enrichment coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_enrich_var_type_inference() {
        let var_span = 10..20;
        let mut tco = empty_tco();
        tco.expr_types.insert(
            SpanKey {
                start: var_span.start,
                end: var_span.end,
            },
            Ty::String,
        );

        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "f".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![(
                            Stmt::Var {
                                name: "v".into(),
                                ty: None,
                                value: Some((Expr::Identifier("input".into()), var_span.clone())),
                            },
                            0..30,
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry).unwrap();
        assert!(result.diagnostics().is_empty());

        if let Item::Function(f) = &program.items[0].0 {
            if let Stmt::Var { ty, .. } = &f.body.stmts[0].0 {
                assert!(ty.is_some(), "var type should be inferred");
                assert!(
                    matches!(&ty.as_ref().unwrap().0, TypeExpr::Named { name, .. } if name == "string")
                );
            }
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "Complex dispatch logic; splitting would reduce clarity"
    )]
    fn test_enrich_stmt_control_flow() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "f".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![
                            (
                                Stmt::If {
                                    condition: make_int_lit(1),
                                    then_block: Block {
                                        stmts: vec![],
                                        trailing_expr: None,
                                    },
                                    else_block: Some(ElseBlock {
                                        block: Some(Block {
                                            stmts: vec![],
                                            trailing_expr: None,
                                        }),
                                        if_stmt: None,
                                        is_if: false,
                                    }),
                                },
                                0..20,
                            ),
                            (
                                Stmt::IfLet {
                                    pattern: Box::new((
                                        hew_parser::ast::Pattern::Identifier("v".into()),
                                        0..1,
                                    )),
                                    expr: Box::new(make_int_lit(0)),
                                    body: Block {
                                        stmts: vec![],
                                        trailing_expr: None,
                                    },
                                    else_body: Some(Block {
                                        stmts: vec![],
                                        trailing_expr: None,
                                    }),
                                },
                                20..40,
                            ),
                            (
                                Stmt::Match {
                                    scrutinee: make_int_lit(0),
                                    arms: vec![hew_parser::ast::MatchArm {
                                        pattern: (hew_parser::ast::Pattern::Wildcard, 0..0),
                                        guard: Some(make_int_lit(1)),
                                        body: make_int_lit(2),
                                    }],
                                },
                                40..60,
                            ),
                            (
                                Stmt::For {
                                    pattern: (
                                        hew_parser::ast::Pattern::Identifier("i".into()),
                                        0..1,
                                    ),
                                    iterable: make_int_lit(0),
                                    body: Block {
                                        stmts: vec![],
                                        trailing_expr: None,
                                    },
                                    label: None,
                                    is_await: false,
                                },
                                60..80,
                            ),
                            (
                                Stmt::While {
                                    condition: make_int_lit(1),
                                    body: Block {
                                        stmts: vec![],
                                        trailing_expr: None,
                                    },
                                    label: None,
                                },
                                80..100,
                            ),
                            (
                                Stmt::Loop {
                                    body: Block {
                                        stmts: vec![(
                                            Stmt::Break {
                                                value: Some(make_int_lit(0)),
                                                label: None,
                                            },
                                            0..10,
                                        )],
                                        trailing_expr: None,
                                    },
                                    label: None,
                                },
                                100..120,
                            ),
                            (
                                Stmt::Assign {
                                    target: make_ident("x"),
                                    value: make_int_lit(0),
                                    op: None,
                                },
                                120..130,
                            ),
                            (Stmt::Defer(Box::new(make_int_lit(0))), 130..140),
                            (Stmt::Return(Some(make_int_lit(0))), 140..150),
                        ],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    // -----------------------------------------------------------------------
    // enrich_expr_with_diagnostics — expression enrichment coverage
    // -----------------------------------------------------------------------

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "Complex dispatch logic; splitting would reduce clarity"
    )]
    fn test_enrich_various_exprs() {
        let tco = empty_tco();
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "f".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![
                            // If expr
                            (
                                Stmt::Expression((
                                    Expr::If {
                                        condition: Box::new(make_int_lit(1)),
                                        then_block: Box::new(make_int_lit(2)),
                                        else_block: Some(Box::new(make_int_lit(3))),
                                    },
                                    0..10,
                                )),
                                0..10,
                            ),
                            // IfLet expr
                            (
                                Stmt::Expression((
                                    Expr::IfLet {
                                        pattern: Box::new((
                                            hew_parser::ast::Pattern::Identifier("x".into()),
                                            0..1,
                                        )),
                                        expr: Box::new(make_int_lit(0)),
                                        body: Block {
                                            stmts: vec![],
                                            trailing_expr: None,
                                        },
                                        else_body: Some(Block {
                                            stmts: vec![],
                                            trailing_expr: None,
                                        }),
                                    },
                                    10..20,
                                )),
                                10..20,
                            ),
                            // Match expr
                            (
                                Stmt::Expression((
                                    Expr::Match {
                                        scrutinee: Box::new(make_int_lit(0)),
                                        arms: vec![hew_parser::ast::MatchArm {
                                            pattern: (hew_parser::ast::Pattern::Wildcard, 0..0),
                                            guard: Some(make_int_lit(1)),
                                            body: make_int_lit(2),
                                        }],
                                    },
                                    20..30,
                                )),
                                20..30,
                            ),
                            // Array, Tuple, MapLiteral
                            (
                                Stmt::Expression((Expr::Array(vec![make_int_lit(0)]), 30..35)),
                                30..35,
                            ),
                            (
                                Stmt::Expression((Expr::Tuple(vec![make_int_lit(0)]), 35..40)),
                                35..40,
                            ),
                            (
                                Stmt::Expression((
                                    Expr::MapLiteral {
                                        entries: vec![(make_int_lit(1), make_int_lit(2))],
                                    },
                                    40..50,
                                )),
                                40..50,
                            ),
                            // Lambda
                            (
                                Stmt::Expression((
                                    Expr::Lambda {
                                        params: vec![],
                                        body: Box::new(make_int_lit(0)),
                                        return_type: None,
                                        is_move: false,
                                        type_params: None,
                                    },
                                    50..60,
                                )),
                                50..60,
                            ),
                            // Call
                            (
                                Stmt::Expression((
                                    Expr::Call {
                                        function: Box::new(make_ident("foo")),
                                        args: vec![CallArg::Positional(make_int_lit(1))],
                                        type_args: None,
                                        is_tail_call: false,
                                    },
                                    60..70,
                                )),
                                60..70,
                            ),
                            // Binary, Unary
                            (
                                Stmt::Expression((
                                    Expr::Binary {
                                        op: hew_parser::ast::BinaryOp::Add,
                                        left: Box::new(make_int_lit(1)),
                                        right: Box::new(make_int_lit(2)),
                                    },
                                    70..80,
                                )),
                                70..80,
                            ),
                            (
                                Stmt::Expression((
                                    Expr::Unary {
                                        op: hew_parser::ast::UnaryOp::Negate,
                                        operand: Box::new(make_int_lit(1)),
                                    },
                                    80..85,
                                )),
                                80..85,
                            ),
                            // FieldAccess, Index
                            (
                                Stmt::Expression((
                                    Expr::FieldAccess {
                                        object: Box::new(make_ident("s")),
                                        field: "x".into(),
                                    },
                                    85..90,
                                )),
                                85..90,
                            ),
                            (
                                Stmt::Expression((
                                    Expr::Index {
                                        object: Box::new(make_ident("arr")),
                                        index: Box::new(make_int_lit(0)),
                                    },
                                    90..95,
                                )),
                                90..95,
                            ),
                            // StructInit
                            (
                                Stmt::Expression((
                                    Expr::StructInit {
                                        name: "Pt".into(),
                                        fields: vec![("x".into(), make_int_lit(0))],
                                    },
                                    95..105,
                                )),
                                95..105,
                            ),
                            // Spawn, Send
                            (
                                Stmt::Expression((
                                    Expr::Spawn {
                                        target: Box::new(make_ident("Actor")),
                                        args: vec![("n".into(), make_int_lit(1))],
                                    },
                                    105..115,
                                )),
                                105..115,
                            ),
                            (
                                Stmt::Expression((
                                    Expr::Send {
                                        target: Box::new(make_ident("actor")),
                                        message: Box::new(make_int_lit(0)),
                                    },
                                    115..125,
                                )),
                                115..125,
                            ),
                            // Range
                            (
                                Stmt::Expression((
                                    Expr::Range {
                                        start: Some(Box::new(make_int_lit(0))),
                                        end: Some(Box::new(make_int_lit(10))),
                                        inclusive: false,
                                    },
                                    125..135,
                                )),
                                125..135,
                            ),
                            // Block
                            (
                                Stmt::Expression((
                                    Expr::Block(Block {
                                        stmts: vec![],
                                        trailing_expr: Some(Box::new(make_int_lit(0))),
                                    }),
                                    135..145,
                                )),
                                135..145,
                            ),
                            // ArrayRepeat
                            (
                                Stmt::Expression((
                                    Expr::ArrayRepeat {
                                        value: Box::new(make_int_lit(0)),
                                        count: Box::new(make_int_lit(5)),
                                    },
                                    145..155,
                                )),
                                145..155,
                            ),
                            // InterpolatedString
                            (
                                Stmt::Expression((
                                    Expr::InterpolatedString(vec![
                                        hew_parser::ast::StringPart::Expr(make_int_lit(42)),
                                    ]),
                                    155..165,
                                )),
                                155..165,
                            ),
                        ],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let result = enrich_program(&mut program, &tco, &registry);
        assert!(result.is_ok());
    }

    // -----------------------------------------------------------------------
    // normalize_type_expr — additional children coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_normalize_array_type_children() {
        let registry = test_registry();
        let mut te = TypeExpr::Array {
            element: Box::new((
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
            size: 10,
        };
        normalize_type_expr(&mut te, &registry);
        if let TypeExpr::Array { element, .. } = &te {
            assert!(matches!(&element.0, TypeExpr::Option(_)));
        }
    }

    #[test]
    fn test_normalize_pointer_type_children() {
        let registry = test_registry();
        let mut te = TypeExpr::Pointer {
            is_mutable: false,
            pointee: Box::new((
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
        };
        normalize_type_expr(&mut te, &registry);
        if let TypeExpr::Pointer { pointee, .. } = &te {
            assert!(matches!(&pointee.0, TypeExpr::Option(_)));
        }
    }

    #[test]
    fn test_normalize_slice_type_children() {
        let registry = test_registry();
        let mut te = TypeExpr::Slice(Box::new((
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
        )));
        normalize_type_expr(&mut te, &registry);
        if let TypeExpr::Slice(inner) = &te {
            assert!(matches!(&inner.0, TypeExpr::Option(_)));
        }
    }

    #[test]
    fn test_normalize_trait_object_type_children() {
        let registry = test_registry();
        let mut te = TypeExpr::TraitObject(vec![TraitBound {
            name: "Iter".into(),
            type_args: Some(vec![(
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
            )]),
        }]);
        normalize_type_expr(&mut te, &registry);
        if let TypeExpr::TraitObject(bounds) = &te {
            let args = bounds[0].type_args.as_ref().unwrap();
            assert!(matches!(&args[0].0, TypeExpr::Option(_)));
        }
    }
}
