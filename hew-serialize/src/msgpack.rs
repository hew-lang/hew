//! Checker-derived side-table entry types and the AST walkers that build
//! them.
//!
//! These types (`ExprTypeEntry`, `ActorSendAliasingEntry`, etc.) are the
//! current carriers for checker output consumed by `--explain-cow` and by
//! `FrontendArtifacts` in `hew-compile`.  The msgpack/JSON wire encoders
//! that previously fed the C++ codegen backend have been removed; the
//! file name is retained for minimal churn while the post-v0.5
//! stdlib-first wire-codec design is worked out.

use hew_parser::ast::{
    Block, CallArg, Expr, Item, Spanned, Stmt, StringPart, TraitItem, TypeBodyItem, TypeExpr,
};
use hew_types::check::{
    ActorSendAliasing as CheckedActorSendAliasing,
    ActorSendCopyReason as CheckedActorSendCopyReason, AssignTargetKind as CheckedAssignTargetKind,
    MethodCallReceiverKind as CheckedMethodCallReceiverKind, SpanKey, TypeCheckOutput,
};
use hew_types::LoweringFact as CheckedLoweringFact;
use serde::{Deserialize, Serialize};

/// An entry in the expression type map: `(start, end)` → `TypeExpr`.
///
/// Carries the resolved type for a single expression, identified by its source
/// span. The C++ codegen uses this to look up expression types without
/// re-inferring them.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExprTypeEntry {
    /// Byte offset of the expression start.
    pub start: usize,
    /// Byte offset of the expression end.
    pub end: usize,
    /// The resolved type, as a parser `TypeExpr` with a synthetic span.
    pub ty: Spanned<TypeExpr>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum MethodCallReceiverKindData {
    NamedTypeInstance {
        type_name: String,
    },
    ActorInstance {
        actor_name: String,
    },
    HandleInstance {
        type_name: String,
    },
    TraitObject {
        trait_name: String,
    },
    StreamInstance {
        element_kind: String,
    },
    /// Receiver is a primitive or compiler-builtin generic that resolved to
    /// a user trait impl via the Stage A1 side table.  See
    /// [`hew_types::check::MethodCallReceiverKind::PrimitiveTraitImpl`].
    PrimitiveTraitImpl {
        trait_name: String,
        canonical_receiver: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MethodCallReceiverKindEntry {
    pub start: usize,
    pub end: usize,
    #[serde(flatten)]
    pub kind: MethodCallReceiverKindData,
    /// Whether the resolved method declares `consumes_receiver`. Defaults
    /// to `false` for backward compatibility with cached artefacts emitted
    /// before this field existed. Codegen reads this flag to null the
    /// receiver's drop slot after the call. Plumbed by issue #1295; the
    /// recognised set is empty in PR 1, so this field is `false` for every
    /// Hew program until PR 2 introduces `Closable::close`.
    #[serde(default)]
    pub consumes_receiver: bool,
}

/// Wire representation of an assignment target kind variant.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AssignTargetKindData {
    LocalVar,
    ActorField,
    FieldAccess,
    Index,
}

/// A single entry in the assign-target-kind side table.
///
/// Keyed by the assignment target expression's source span.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AssignTargetKindEntry {
    pub start: usize,
    pub end: usize,
    #[serde(flatten)]
    pub kind: AssignTargetKindData,
}

/// Wire representation of assignment target type-shape metadata.
///
/// Carries the signedness flag that MLIR lowering needs for compound
/// assignments.  Keyed by the same target expression span as
/// [`AssignTargetKindEntry`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AssignTargetShapeEntry {
    pub start: usize,
    pub end: usize,
    /// `true` when the target type is an unsigned integer (`u8`/`u16`/`u32`/`u64`).
    pub is_unsigned: bool,
}

/// Wire representation of an actor-send alias-vs-copy decision variant.
///
/// Mirrors [`hew_types::check::ActorSendAliasing`] across the serialization
/// boundary. Keyed by the message expression's source span (the same span
/// the move-checker uses on the sender's binding).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ActorSendAliasingData {
    /// Sender retains the payload independently; runtime deep-copies into
    /// the mailbox. The legacy mailbox path; safe everywhere. Carries the
    /// reason the alias path was rejected so `--explain-cow` can render a
    /// precise diagnostic per site.
    Copy { reason: ActorSendCopyReasonData },
    /// Sender and receiver share a refcounted [`HewMsgEnvelope`] payload.
    /// Requires the move-checker to have invalidated the sender's binding.
    Alias,
}

/// Wire representation of [`hew_types::check::ActorSendCopyReason`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ActorSendCopyReasonData {
    /// Arg was not a bare identifier — alias would not be invalidated by
    /// the move-checker.
    NotIdentifier,
    /// Arg's resolved type implements the `Copy` marker.
    CopyType,
    /// Arg's resolved type implements the stdlib-registered `Drop`
    /// marker.
    StdlibDrop,
    /// Arg's resolved type carries a user `impl Drop for T` recorded in
    /// `trait_impls_set` rather than the marker.
    UserDrop,
}

/// A single entry in the actor-send aliasing side table.
///
/// Keyed by the message expression's source span — codegen looks up this
/// span at every actor-send lowering call site and reads the variant
/// fail-closed (a missing entry for a known send is a hard compile error).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ActorSendAliasingEntry {
    pub start: usize,
    pub end: usize,
    #[serde(flatten)]
    pub kind: ActorSendAliasingData,
}

/// A checker-owned lowering fact keyed by the lowering site's source span.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoweringFactEntry {
    pub start: usize,
    pub end: usize,
    #[serde(flatten)]
    pub fact: CheckedLoweringFact,
}

/// Inferred type arguments for a generic free-function call site, keyed by the
/// call expression's source span.
///
/// Populated from [`hew_types::check::TypeCheckOutput::call_type_args`] for
/// call sites where the caller omitted explicit type arguments and the checker
/// resolved them through unification. C++ codegen uses this side table to
/// monomorphize generic free-function calls without re-running inference.
///
/// The program-level `call_type_args` key is required in both the Rust and C++
/// readers: the C++ reader uses `mapReq`, which throws on a missing key, and
/// schema version is exact-matched before any field is read, so mismatched
/// payloads are rejected by the version check before reaching this field.
/// The `#[serde(default)]` below is on the `type_args` *field* within each
/// entry (protecting against entries serialized without that field), not on the
/// struct or the program-level key.
// `Eq` is intentionally absent: `type_args` is `Vec<Spanned<TypeExpr>>`, and
// `TypeExpr` (hew-parser) only derives `PartialEq`, not `Eq`. Adding `Eq` here
// would require first propagating `Eq` to `TypeExpr` and `TraitBound` upstream.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CallTypeArgsEntry {
    /// Byte offset of the call expression start.
    pub start: usize,
    /// Byte offset of the call expression end.
    pub end: usize,
    /// Resolved type arguments in parameter order, each represented as a
    /// parser `TypeExpr` with a synthetic zero span.
    #[serde(default)]
    pub type_args: Vec<Spanned<TypeExpr>>,
}

// ---- Shared side-table traversal walker ---------------------------------

/// Controls how `Stmt::If` children are ordered during traversal.
///
/// The two assign-target walkers disagree on this order (an existing
/// behavioral difference that must be preserved).
#[derive(Clone, Copy)]
enum IfStmtOrder {
    /// Visit the else branch before the then branch.
    ElseBeforeThen,
    /// Visit the then branch before the else branch.
    ThenBeforeElse,
}

/// Controls which items are visited at the top level.
#[derive(Clone, Copy)]
enum ModuleGraphMode {
    /// Always visit `program.items` first, then non-root modules in topo order.
    ItemsThenNonRootModules,
    /// If a module graph is present, traverse all modules (including root) in
    /// topo order; otherwise visit `program.items`.
    ModulesOrProgramItems,
}

/// Visitor hooks for the shared AST side-table traversal walker.
///
/// Implementors supply entry-extraction logic at two hook points; the shared
/// walker [`walk_program`] handles all recursive traversal.
trait SideTableVisitor {
    type Entry;

    /// Called before recursing into a `Stmt::Assign`'s target and value.
    fn on_assign_stmt(
        &self,
        target: &Spanned<Expr>,
        tco: &TypeCheckOutput,
        out: &mut Vec<Self::Entry>,
    );

    /// Called before recursing into a `Expr::MethodCall`'s receiver and args.
    fn on_method_call_expr(
        &self,
        expr: &Spanned<Expr>,
        tco: &TypeCheckOutput,
        out: &mut Vec<Self::Entry>,
    );

    /// Called before recursing into an `Expr::Call`.
    fn on_call_expr(
        &self,
        _expr: &Spanned<Expr>,
        _tco: &TypeCheckOutput,
        _out: &mut Vec<Self::Entry>,
    ) {
    }

    fn if_stmt_order(&self) -> IfStmtOrder;
    fn module_graph_mode(&self) -> ModuleGraphMode;
}

/// Walk every reachable expression in `program`, calling `visitor` hooks at
/// side-table-relevant nodes and collecting the returned entries.
#[expect(
    clippy::too_many_lines,
    reason = "the shared walker needs one complete recursive pass mirroring every surviving AST shape"
)]
fn walk_program<V: SideTableVisitor>(
    program: &hew_parser::ast::Program,
    tco: &TypeCheckOutput,
    visitor: &V,
) -> Vec<V::Entry> {
    fn collect_fn<V: SideTableVisitor>(
        fn_decl: &hew_parser::ast::FnDecl,
        tco: &TypeCheckOutput,
        visitor: &V,
        out: &mut Vec<V::Entry>,
    ) {
        collect_block(&fn_decl.body, tco, visitor, out);
    }

    fn collect_block<V: SideTableVisitor>(
        block: &Block,
        tco: &TypeCheckOutput,
        visitor: &V,
        out: &mut Vec<V::Entry>,
    ) {
        for stmt in &block.stmts {
            collect_stmt(stmt, tco, visitor, out);
        }
        if let Some(trailing) = &block.trailing_expr {
            collect_expr(trailing, tco, visitor, out);
        }
    }

    fn collect_call_args<V: SideTableVisitor>(
        args: &[CallArg],
        tco: &TypeCheckOutput,
        visitor: &V,
        out: &mut Vec<V::Entry>,
    ) {
        for arg in args {
            collect_expr(arg.expr(), tco, visitor, out);
        }
    }

    fn collect_stmt<V: SideTableVisitor>(
        stmt: &Spanned<Stmt>,
        tco: &TypeCheckOutput,
        visitor: &V,
        out: &mut Vec<V::Entry>,
    ) {
        match &stmt.0 {
            Stmt::Let { value, .. }
            | Stmt::Var { value, .. }
            | Stmt::Break { value, .. }
            | Stmt::Return(value) => {
                if let Some(value) = value {
                    collect_expr(value, tco, visitor, out);
                }
            }
            Stmt::Assign { target, value, .. } => {
                visitor.on_assign_stmt(target, tco, out);
                collect_expr(target, tco, visitor, out);
                collect_expr(value, tco, visitor, out);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                collect_expr(condition, tco, visitor, out);
                match visitor.if_stmt_order() {
                    IfStmtOrder::ElseBeforeThen => {
                        if let Some(eb) = else_block {
                            match (eb.if_stmt.as_ref(), eb.block.as_ref()) {
                                (Some(if_stmt), _) => collect_stmt(if_stmt, tco, visitor, out),
                                (None, Some(block)) => collect_block(block, tco, visitor, out),
                                (None, None) => {}
                            }
                        }
                        collect_block(then_block, tco, visitor, out);
                    }
                    IfStmtOrder::ThenBeforeElse => {
                        collect_block(then_block, tco, visitor, out);
                        if let Some(eb) = else_block {
                            match (eb.if_stmt.as_ref(), eb.block.as_ref()) {
                                (Some(if_stmt), _) => collect_stmt(if_stmt, tco, visitor, out),
                                (None, Some(block)) => collect_block(block, tco, visitor, out),
                                (None, None) => {}
                            }
                        }
                    }
                }
            }
            Stmt::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                collect_expr(expr, tco, visitor, out);
                collect_block(body, tco, visitor, out);
                if let Some(else_body) = else_body {
                    collect_block(else_body, tco, visitor, out);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                collect_expr(scrutinee, tco, visitor, out);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        collect_expr(guard, tco, visitor, out);
                    }
                    collect_expr(&arm.body, tco, visitor, out);
                }
            }
            Stmt::Loop { body, .. } => collect_block(body, tco, visitor, out),
            Stmt::For { iterable, body, .. } => {
                collect_expr(iterable, tco, visitor, out);
                collect_block(body, tco, visitor, out);
            }
            Stmt::While {
                condition, body, ..
            } => {
                collect_expr(condition, tco, visitor, out);
                collect_block(body, tco, visitor, out);
            }
            Stmt::WhileLet { expr, body, .. } => {
                collect_expr(expr, tco, visitor, out);
                collect_block(body, tco, visitor, out);
            }
            Stmt::Defer(expr) => collect_expr(expr, tco, visitor, out),
            Stmt::Expression(expr) => collect_expr(expr, tco, visitor, out),
            Stmt::Continue { .. } => {}
        }
    }

    fn collect_expr<V: SideTableVisitor>(
        expr: &Spanned<Expr>,
        tco: &TypeCheckOutput,
        visitor: &V,
        out: &mut Vec<V::Entry>,
    ) {
        match &expr.0 {
            Expr::Binary { left, right, .. } => {
                collect_expr(left, tco, visitor, out);
                collect_expr(right, tco, visitor, out);
            }
            Expr::Unary { operand, .. }
            | Expr::Cast { expr: operand, .. }
            | Expr::PostfixTry(operand)
            | Expr::Await(operand) => collect_expr(operand, tco, visitor, out),
            Expr::Tuple(exprs) | Expr::Array(exprs) | Expr::Join(exprs) => {
                for elem in exprs {
                    collect_expr(elem, tco, visitor, out);
                }
            }
            Expr::ArrayRepeat { value, count } => {
                collect_expr(value, tco, visitor, out);
                collect_expr(count, tco, visitor, out);
            }
            Expr::MapLiteral { entries } => {
                for (key, value) in entries {
                    collect_expr(key, tco, visitor, out);
                    collect_expr(value, tco, visitor, out);
                }
            }
            Expr::Block(block) => {
                collect_block(block, tco, visitor, out);
            }
            Expr::UnsafeBlock(block) => {
                collect_block(block, tco, visitor, out);
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                collect_expr(condition, tco, visitor, out);
                collect_expr(then_block, tco, visitor, out);
                if let Some(else_block) = else_block {
                    collect_expr(else_block, tco, visitor, out);
                }
            }
            Expr::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                collect_expr(expr, tco, visitor, out);
                collect_block(body, tco, visitor, out);
                if let Some(else_body) = else_body {
                    collect_block(else_body, tco, visitor, out);
                }
            }
            Expr::Match { scrutinee, arms } => {
                collect_expr(scrutinee, tco, visitor, out);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        collect_expr(guard, tco, visitor, out);
                    }
                    collect_expr(&arm.body, tco, visitor, out);
                }
            }
            Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
                collect_expr(body, tco, visitor, out);
            }
            Expr::Spawn { target, args } => {
                collect_expr(target, tco, visitor, out);
                for (_, arg) in args {
                    collect_expr(arg, tco, visitor, out);
                }
            }
            Expr::Scope { body } | Expr::ForkBlock { body } => {
                collect_block(body, tco, visitor, out);
            }
            Expr::ScopeDeadline { duration, body } => {
                collect_expr(duration, tco, visitor, out);
                collect_block(body, tco, visitor, out);
            }
            Expr::ForkChild { expr, .. } => collect_expr(expr, tco, visitor, out),
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr(expr) = part {
                        collect_expr(expr, tco, visitor, out);
                    }
                }
            }
            Expr::Call { function, args, .. } => {
                visitor.on_call_expr(expr, tco, out);
                collect_expr(function, tco, visitor, out);
                collect_call_args(args, tco, visitor, out);
            }
            Expr::MethodCall { receiver, args, .. } => {
                visitor.on_method_call_expr(expr, tco, out);
                collect_expr(receiver, tco, visitor, out);
                collect_call_args(args, tco, visitor, out);
            }
            Expr::StructInit { fields, .. } | Expr::MachineEmit { fields, .. } => {
                for (_, value) in fields {
                    collect_expr(value, tco, visitor, out);
                }
            }
            Expr::Select { arms, timeout } => {
                for arm in arms {
                    collect_expr(&arm.source, tco, visitor, out);
                    collect_expr(&arm.body, tco, visitor, out);
                }
                if let Some(timeout) = timeout {
                    collect_expr(&timeout.duration, tco, visitor, out);
                    collect_expr(&timeout.body, tco, visitor, out);
                }
            }
            Expr::Timeout { expr, duration } => {
                collect_expr(expr, tco, visitor, out);
                collect_expr(duration, tco, visitor, out);
            }
            Expr::Yield(value) => {
                if let Some(value) = value {
                    collect_expr(value, tco, visitor, out);
                }
            }
            Expr::FieldAccess { object, .. } => collect_expr(object, tco, visitor, out),
            Expr::Index { object, index } => {
                collect_expr(object, tco, visitor, out);
                collect_expr(index, tco, visitor, out);
            }
            Expr::Range { start, end, .. } => {
                if let Some(start) = start {
                    collect_expr(start, tco, visitor, out);
                }
                if let Some(end) = end {
                    collect_expr(end, tco, visitor, out);
                }
            }
            Expr::Is { lhs, rhs } => {
                collect_expr(lhs, tco, visitor, out);
                collect_expr(rhs, tco, visitor, out);
            }
            Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::Cooperate
            | Expr::This
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
            Expr::GenBlock { body } => collect_block(body, tco, visitor, out),
        }
    }

    fn collect_item<V: SideTableVisitor>(
        item: &Spanned<Item>,
        tco: &TypeCheckOutput,
        visitor: &V,
        out: &mut Vec<V::Entry>,
    ) {
        match &item.0 {
            Item::Const(const_decl) => collect_expr(&const_decl.value, tco, visitor, out),
            Item::TypeDecl(type_decl) => {
                for body_item in &type_decl.body {
                    if let TypeBodyItem::Method(method) = body_item {
                        collect_fn(method, tco, visitor, out);
                    }
                }
            }
            Item::Trait(trait_decl) => {
                for trait_item in &trait_decl.items {
                    if let TraitItem::Method(method) = trait_item {
                        if let Some(body) = &method.body {
                            collect_block(body, tco, visitor, out);
                        }
                    }
                }
            }
            Item::Impl(impl_decl) => {
                for method in &impl_decl.methods {
                    collect_fn(method, tco, visitor, out);
                }
            }
            Item::Function(fn_decl) => collect_fn(fn_decl, tco, visitor, out),
            Item::Actor(actor_decl) => {
                if let Some(init) = &actor_decl.init {
                    collect_block(&init.body, tco, visitor, out);
                }
                for receive_fn in &actor_decl.receive_fns {
                    collect_block(&receive_fn.body, tco, visitor, out);
                }
                for method in &actor_decl.methods {
                    collect_fn(method, tco, visitor, out);
                }
            }
            Item::Supervisor(supervisor_decl) => {
                for child in &supervisor_decl.children {
                    for arg in &child.args {
                        collect_expr(arg, tco, visitor, out);
                    }
                }
            }
            Item::Machine(machine_decl) => {
                for transition in &machine_decl.transitions {
                    if let Some(guard) = &transition.guard {
                        collect_expr(guard, tco, visitor, out);
                    }
                    collect_expr(&transition.body, tco, visitor, out);
                }
            }
            // Record fields contain only type expressions; no expressions to collect in A-1.
            Item::Record(_)
            | Item::Import(_)
            | Item::TypeAlias(_)
            | Item::Wire(_)
            | Item::ExternBlock(_) => {}
        }
    }

    let mut entries = Vec::new();
    match visitor.module_graph_mode() {
        ModuleGraphMode::ItemsThenNonRootModules => {
            for item in &program.items {
                collect_item(item, tco, visitor, &mut entries);
            }
            if let Some(module_graph) = &program.module_graph {
                for module_id in &module_graph.topo_order {
                    if module_id == &module_graph.root {
                        continue;
                    }
                    if let Some(module) = module_graph.modules.get(module_id) {
                        for item in &module.items {
                            collect_item(item, tco, visitor, &mut entries);
                        }
                    }
                }
            }
        }
        ModuleGraphMode::ModulesOrProgramItems => {
            if let Some(module_graph) = &program.module_graph {
                for module_id in &module_graph.topo_order {
                    if let Some(module) = module_graph.modules.get(module_id) {
                        for item in &module.items {
                            collect_item(item, tco, visitor, &mut entries);
                        }
                    }
                }
            } else {
                for item in &program.items {
                    collect_item(item, tco, visitor, &mut entries);
                }
            }
        }
    }
    entries
}

// ---- Public side-table builders -----------------------------------------

/// Walk `program` and collect an [`AssignTargetKindEntry`] for every
/// `Stmt::Assign` whose target span has an entry in `tco.assign_target_kinds`.
#[must_use]
pub fn build_assign_target_kind_entries(
    program: &hew_parser::ast::Program,
    tco: &TypeCheckOutput,
) -> Vec<AssignTargetKindEntry> {
    struct Visitor;
    impl SideTableVisitor for Visitor {
        type Entry = AssignTargetKindEntry;
        fn on_assign_stmt(
            &self,
            target: &Spanned<Expr>,
            tco: &TypeCheckOutput,
            out: &mut Vec<Self::Entry>,
        ) {
            let key = SpanKey::from(&target.1);
            let Some(kind) = tco.assign_target_kinds.get(&key) else {
                return;
            };
            out.push(AssignTargetKindEntry {
                start: key.start,
                end: key.end,
                kind: match kind {
                    CheckedAssignTargetKind::LocalVar => AssignTargetKindData::LocalVar,
                    CheckedAssignTargetKind::ActorField => AssignTargetKindData::ActorField,
                    CheckedAssignTargetKind::FieldAccess => AssignTargetKindData::FieldAccess,
                    CheckedAssignTargetKind::Index => AssignTargetKindData::Index,
                },
            });
        }
        fn on_method_call_expr(
            &self,
            _expr: &Spanned<Expr>,
            _tco: &TypeCheckOutput,
            _out: &mut Vec<Self::Entry>,
        ) {
        }
        fn if_stmt_order(&self) -> IfStmtOrder {
            IfStmtOrder::ElseBeforeThen
        }
        fn module_graph_mode(&self) -> ModuleGraphMode {
            ModuleGraphMode::ItemsThenNonRootModules
        }
    }
    walk_program(program, tco, &Visitor)
}

/// Walk `program` and collect an [`AssignTargetShapeEntry`] for every
/// `Stmt::Assign` whose target span has an entry in `tco.assign_target_shapes`.
#[must_use]
pub fn build_assign_target_shape_entries(
    program: &hew_parser::ast::Program,
    tco: &TypeCheckOutput,
) -> Vec<AssignTargetShapeEntry> {
    struct Visitor;
    impl SideTableVisitor for Visitor {
        type Entry = AssignTargetShapeEntry;
        fn on_assign_stmt(
            &self,
            target: &Spanned<Expr>,
            tco: &TypeCheckOutput,
            out: &mut Vec<Self::Entry>,
        ) {
            let key = SpanKey::from(&target.1);
            let Some(shape) = tco.assign_target_shapes.get(&key) else {
                return;
            };
            out.push(AssignTargetShapeEntry {
                start: key.start,
                end: key.end,
                is_unsigned: shape.is_unsigned,
            });
        }
        fn on_method_call_expr(
            &self,
            _expr: &Spanned<Expr>,
            _tco: &TypeCheckOutput,
            _out: &mut Vec<Self::Entry>,
        ) {
        }
        fn if_stmt_order(&self) -> IfStmtOrder {
            IfStmtOrder::ThenBeforeElse
        }
        fn module_graph_mode(&self) -> ModuleGraphMode {
            ModuleGraphMode::ItemsThenNonRootModules
        }
    }
    walk_program(program, tco, &Visitor)
}

/// Walk `program` and collect a [`LoweringFactEntry`] for every method-call span
/// with checker-owned lowering metadata.
#[must_use]
pub fn build_lowering_fact_entries(
    program: &hew_parser::ast::Program,
    tco: &TypeCheckOutput,
) -> Vec<LoweringFactEntry> {
    struct Visitor;
    impl SideTableVisitor for Visitor {
        type Entry = LoweringFactEntry;
        fn on_assign_stmt(
            &self,
            _target: &Spanned<Expr>,
            _tco: &TypeCheckOutput,
            _out: &mut Vec<Self::Entry>,
        ) {
        }
        fn on_method_call_expr(
            &self,
            expr: &Spanned<Expr>,
            tco: &TypeCheckOutput,
            out: &mut Vec<Self::Entry>,
        ) {
            let key = SpanKey::from(&expr.1);
            let Some(fact) = tco.lowering_facts.get(&key) else {
                return;
            };
            out.push(LoweringFactEntry {
                start: key.start,
                end: key.end,
                fact: *fact,
            });
        }
        fn on_call_expr(
            &self,
            expr: &Spanned<Expr>,
            tco: &TypeCheckOutput,
            out: &mut Vec<Self::Entry>,
        ) {
            let key = SpanKey::from(&expr.1);
            let Some(fact) = tco.lowering_facts.get(&key) else {
                return;
            };
            out.push(LoweringFactEntry {
                start: key.start,
                end: key.end,
                fact: *fact,
            });
        }
        fn if_stmt_order(&self) -> IfStmtOrder {
            IfStmtOrder::ElseBeforeThen
        }
        fn module_graph_mode(&self) -> ModuleGraphMode {
            ModuleGraphMode::ModulesOrProgramItems
        }
    }
    walk_program(program, tco, &Visitor)
}

#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "single visitor trait impl with parallel match arms per receiver-kind variant"
)]
pub fn build_method_call_receiver_kind_entries(
    program: &hew_parser::ast::Program,
    tco: &TypeCheckOutput,
) -> Vec<MethodCallReceiverKindEntry> {
    struct Visitor;
    impl SideTableVisitor for Visitor {
        type Entry = MethodCallReceiverKindEntry;
        fn on_assign_stmt(
            &self,
            _target: &Spanned<Expr>,
            _tco: &TypeCheckOutput,
            _out: &mut Vec<Self::Entry>,
        ) {
        }
        fn on_method_call_expr(
            &self,
            expr: &Spanned<Expr>,
            tco: &TypeCheckOutput,
            out: &mut Vec<Self::Entry>,
        ) {
            let key = SpanKey::from(&expr.1);
            let Some(kind) = tco.method_call_receiver_kinds.get(&key) else {
                return;
            };
            out.push(MethodCallReceiverKindEntry {
                start: key.start,
                end: key.end,
                kind: match kind {
                    CheckedMethodCallReceiverKind::NamedTypeInstance { type_name } => {
                        MethodCallReceiverKindData::NamedTypeInstance {
                            type_name: type_name.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::ActorInstance { actor_name } => {
                        MethodCallReceiverKindData::ActorInstance {
                            actor_name: actor_name.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::HandleInstance { type_name } => {
                        MethodCallReceiverKindData::HandleInstance {
                            type_name: type_name.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::TraitObject { trait_name } => {
                        MethodCallReceiverKindData::TraitObject {
                            trait_name: trait_name.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::StreamInstance { element_kind } => {
                        MethodCallReceiverKindData::StreamInstance {
                            element_kind: element_kind.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::PrimitiveTraitImpl {
                        trait_name,
                        canonical_receiver,
                    } => MethodCallReceiverKindData::PrimitiveTraitImpl {
                        trait_name: trait_name.clone(),
                        canonical_receiver: canonical_receiver.clone(),
                    },
                },
                consumes_receiver: tco.method_call_consumes_receiver.contains(&key),
            });
        }
        fn on_call_expr(
            &self,
            expr: &Spanned<Expr>,
            tco: &TypeCheckOutput,
            out: &mut Vec<Self::Entry>,
        ) {
            let key = SpanKey::from(&expr.1);
            if !tco.method_call_rewrites.contains_key(&key) {
                return;
            }
            let Some(kind) = tco.method_call_receiver_kinds.get(&key) else {
                return;
            };
            out.push(MethodCallReceiverKindEntry {
                start: key.start,
                end: key.end,
                kind: match kind {
                    CheckedMethodCallReceiverKind::NamedTypeInstance { type_name } => {
                        MethodCallReceiverKindData::NamedTypeInstance {
                            type_name: type_name.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::ActorInstance { actor_name } => {
                        MethodCallReceiverKindData::ActorInstance {
                            actor_name: actor_name.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::HandleInstance { type_name } => {
                        MethodCallReceiverKindData::HandleInstance {
                            type_name: type_name.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::TraitObject { trait_name } => {
                        MethodCallReceiverKindData::TraitObject {
                            trait_name: trait_name.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::StreamInstance { element_kind } => {
                        MethodCallReceiverKindData::StreamInstance {
                            element_kind: element_kind.clone(),
                        }
                    }
                    CheckedMethodCallReceiverKind::PrimitiveTraitImpl {
                        trait_name,
                        canonical_receiver,
                    } => MethodCallReceiverKindData::PrimitiveTraitImpl {
                        trait_name: trait_name.clone(),
                        canonical_receiver: canonical_receiver.clone(),
                    },
                },
                consumes_receiver: tco.method_call_consumes_receiver.contains(&key),
            });
        }
        fn if_stmt_order(&self) -> IfStmtOrder {
            IfStmtOrder::ElseBeforeThen
        }
        fn module_graph_mode(&self) -> ModuleGraphMode {
            ModuleGraphMode::ModulesOrProgramItems
        }
    }
    walk_program(program, tco, &Visitor)
}

/// Build the `call_type_args` side-table entries from `tco.call_type_args`.
///
/// Walks the program AST using the shared [`SideTableVisitor`] to emit entries in
/// source order, one per generic free-function call site where the checker inferred
/// type arguments.
///
/// Returns a pair `(entries, errors)`.  Call sites where any type argument fails to
/// convert to [`Spanned<TypeExpr>`] (e.g. unresolved type variables, error-sentinel
/// types) produce no entry and instead append a [`TypeExprConversionError`] to the
/// errors vec.  Callers **must** treat these errors as diagnostics — silently
/// ignoring them loses information about unconvertible type arguments.
/// Shared emitter for [`build_call_type_args_entries`].  Looks up the outer
/// call span in `tco.call_type_args`, converts each inferred [`Ty`] to a
/// parser [`TypeExpr`], and pushes either a populated entry or a sentinel
/// entry paired with diagnostics when conversion fails.  The caller filters
/// sentinel entries via the `errs.is_empty()` guard.
fn emit_call_type_args_entry(
    expr: &Spanned<Expr>,
    tco: &TypeCheckOutput,
    out: &mut Vec<(
        CallTypeArgsEntry,
        Vec<crate::enrich::TypeExprConversionError>,
    )>,
) {
    let key = SpanKey::from(&expr.1);
    let Some(type_args) = tco.call_type_args.get(&key) else {
        return;
    };
    let mut errors = Vec::new();
    let mut converted_args = Vec::new();
    for ty in type_args {
        match crate::enrich::ty_to_type_expr(ty) {
            Ok(te) => converted_args.push(te),
            Err(e) => errors.push(e),
        }
    }
    if errors.is_empty() {
        out.push((
            CallTypeArgsEntry {
                start: key.start,
                end: key.end,
                type_args: converted_args,
            },
            Vec::new(),
        ));
    } else {
        // Push the errors paired with a sentinel entry so the caller can
        // drain them via `errors.extend(errs)`.  The entry itself is
        // discarded by the `if errs.is_empty()` filter in
        // `build_call_type_args_entries` — a partial entry would be worse
        // than none.
        out.push((
            CallTypeArgsEntry {
                start: key.start,
                end: key.end,
                type_args: Vec::new(),
            },
            errors,
        ));
    }
}

#[must_use]
pub fn build_call_type_args_entries(
    program: &hew_parser::ast::Program,
    tco: &TypeCheckOutput,
) -> (
    Vec<CallTypeArgsEntry>,
    Vec<crate::enrich::TypeExprConversionError>,
) {
    struct Visitor;
    impl SideTableVisitor for Visitor {
        type Entry = (
            CallTypeArgsEntry,
            Vec<crate::enrich::TypeExprConversionError>,
        );
        fn on_assign_stmt(
            &self,
            _target: &Spanned<Expr>,
            _tco: &TypeCheckOutput,
            _out: &mut Vec<Self::Entry>,
        ) {
        }
        fn on_method_call_expr(
            &self,
            expr: &Spanned<Expr>,
            tco: &TypeCheckOutput,
            out: &mut Vec<Self::Entry>,
        ) {
            // Module-qualified generic free-function calls reach codegen as
            // `MethodCall` (e.g. `genericlib.identity(1)`), so the side table
            // must surface their inferred type arguments under the same outer
            // call span the codegen will look up. The checker keys
            // `record_concrete_call_type_args` by that outer span for both
            // `Call` and `MethodCall` paths.
            emit_call_type_args_entry(expr, tco, out);
        }
        fn on_call_expr(
            &self,
            expr: &Spanned<Expr>,
            tco: &TypeCheckOutput,
            out: &mut Vec<Self::Entry>,
        ) {
            emit_call_type_args_entry(expr, tco, out);
        }
        fn if_stmt_order(&self) -> IfStmtOrder {
            IfStmtOrder::ElseBeforeThen
        }
        fn module_graph_mode(&self) -> ModuleGraphMode {
            ModuleGraphMode::ModulesOrProgramItems
        }
    }
    let combined = walk_program(program, tco, &Visitor);
    let mut entries = Vec::with_capacity(combined.len());
    let mut errors = Vec::new();
    for (entry, errs) in combined {
        if errs.is_empty() {
            entries.push(entry);
        }
        errors.extend(errs);
    }
    (entries, errors)
}

/// Build the actor-send aliasing side table from the type-checker output.
///
/// The producer ([`Checker::enforce_actor_boundary_send`]) records every
/// accepted actor send in [`TypeCheckOutput::actor_send_aliasing`], keyed
/// by the message expression's span. This helper just lifts the map into
/// the wire format with deterministic ordering by span — no AST walk is
/// needed because the map already enumerates every relevant span.
///
/// Codegen is expected to look up each lowering site's span in the
/// resulting list fail-closed (per the `serializer-fail-closed` LESSONS
/// row): a missing entry for a span the C++ side believes is an actor
/// send is a hard error.
#[inline]
fn reason_to_wire(r: CheckedActorSendCopyReason) -> ActorSendCopyReasonData {
    match r {
        CheckedActorSendCopyReason::NotIdentifier => ActorSendCopyReasonData::NotIdentifier,
        CheckedActorSendCopyReason::CopyType => ActorSendCopyReasonData::CopyType,
        CheckedActorSendCopyReason::StdlibDrop => ActorSendCopyReasonData::StdlibDrop,
        CheckedActorSendCopyReason::UserDrop => ActorSendCopyReasonData::UserDrop,
    }
}

#[must_use]
pub fn build_actor_send_aliasing_entries(tco: &TypeCheckOutput) -> Vec<ActorSendAliasingEntry> {
    let mut entries: Vec<ActorSendAliasingEntry> = tco
        .actor_send_aliasing
        .iter()
        .map(|(span, decision)| ActorSendAliasingEntry {
            start: span.start,
            end: span.end,
            kind: match decision {
                CheckedActorSendAliasing::Copy(reason) => ActorSendAliasingData::Copy {
                    reason: reason_to_wire(*reason),
                },
                CheckedActorSendAliasing::Alias => ActorSendAliasingData::Alias,
            },
        })
        .collect();
    entries.sort_by_key(|e| (e.start, e.end));
    entries
}
