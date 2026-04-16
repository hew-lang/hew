//! `MessagePack` serialization for the Hew AST.
//!
//! Provides a compact binary serialization of the parsed (and optionally
//! type-enriched) AST using `rmp-serde`. Hew's embedded C++ codegen backend
//! (`hew-codegen/src/msgpack_reader.cpp`) deserializes this format.

use std::collections::{BTreeMap, HashMap};

use hew_parser::ast::{
    Block, CallArg, Expr, Item, Spanned, Stmt, StringPart, TraitItem, TypeBodyItem, TypeExpr,
};
use hew_parser::module::{Module, ModuleId};
use hew_types::check::{
    AssignTargetKind as CheckedAssignTargetKind,
    MethodCallReceiverKind as CheckedMethodCallReceiverKind, SpanKey, TypeCheckOutput,
};
use hew_types::LoweringFact as CheckedLoweringFact;
use serde::{Deserialize, Serialize};

/// Schema version for the msgpack AST boundary.
///
/// Increment when the serialized format changes in a way that older C++
/// codegen cannot understand. The embedded reader requires an explicit
/// `schema_version` field and rejects mismatches instead of carrying
/// fallback decoding for pre-versioned payloads.
pub const SCHEMA_VERSION: u32 = 7;

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
    NamedTypeInstance { type_name: String },
    HandleInstance { type_name: String },
    TraitObject { trait_name: String },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MethodCallReceiverKindEntry {
    pub start: usize,
    pub end: usize,
    #[serde(flatten)]
    pub kind: MethodCallReceiverKindData,
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

/// A checker-owned lowering fact keyed by the lowering site's source span.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoweringFactEntry {
    pub start: usize,
    pub end: usize,
    #[serde(flatten)]
    pub fact: CheckedLoweringFact,
}

/// Top-level serialization wrapper: the program AST plus type-checker and
/// source metadata used by C++ codegen.
///
/// Serialized as a `MessagePack` map with these keys:
/// - `"schema_version"`
/// - `"items"`
/// - `"module_doc"`
/// - `"expr_types"`
/// - `"method_call_receiver_kinds"`
/// - `"assign_target_kinds"`
/// - `"assign_target_shapes"`
/// - `"lowering_facts"`
/// - `"handle_types"`
/// - `"handle_type_repr"`
/// - `"drop_funcs"`
/// - `"module_graph"` when module graph data is available
/// - `"source_path"` when source path metadata is available
/// - `"line_map"` when line mapping metadata is available
#[derive(Debug, Serialize)]
struct TypedProgram<'a, ModuleGraphRepr> {
    /// Schema version — always serialized first so the C++ reader can
    /// reject incompatible payloads before parsing the rest of the AST.
    schema_version: u32,
    items: &'a Vec<Spanned<hew_parser::ast::Item>>,
    module_doc: &'a Option<String>,
    /// Resolved types for every expression the type checker annotated.
    expr_types: &'a [ExprTypeEntry],
    /// Checker-resolved receiver classification for surviving method calls.
    method_call_receiver_kinds: &'a [MethodCallReceiverKindEntry],
    /// Checker-resolved assignment target classifications (keyed by target span).
    /// Missing entries indicate the checker rejected those targets.
    assign_target_kinds: &'a [AssignTargetKindEntry],
    /// Checker-resolved assignment target type-shape metadata (keyed by target span).
    /// Consumed by MLIR lowering fail-closed to determine compound-assignment signedness.
    assign_target_shapes: &'a [AssignTargetShapeEntry],
    /// Checker-resolved lowering metadata for erased runtime types.
    lowering_facts: &'a [LoweringFactEntry],
    /// Names of all known handle types (e.g., `"http.Server"`, `"json.Value"`).
    /// Flows type metadata to C++ codegen so it doesn't need hardcoded type lists.
    handle_types: Vec<String>,
    /// Map of handle type name to its MLIR representation.
    /// Default is `"handle"` (opaque pointer via `HandleType`).
    /// `"i32"` means the type is represented as a 32-bit integer (e.g., file descriptors).
    handle_type_repr: HashMap<String, String>,
    /// C drop functions for stdlib handle types with `impl Drop`.
    ///
    /// Maps qualified type name (e.g. `"http.Request"`) to the C function that
    /// frees it (e.g. `"hew_http_request_free"`). C++ codegen uses this map in
    /// `dropFuncForMLIRType` instead of the hardcoded handle-drop table.
    drop_funcs: Vec<(String, String)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    module_graph: Option<ModuleGraphRepr>,
    /// Absolute path to the source .hew file.
    /// Used for codegen diagnostics and, when enabled, DWARF debug info.
    #[serde(skip_serializing_if = "Option::is_none")]
    source_path: Option<&'a str>,
    /// Byte offset of the start of each line in the source file.
    /// `line_map`[0] = offset of line 1, `line_map`[1] = offset of line 2, etc.
    /// Used by codegen to convert byte-offset spans to line:column for
    /// diagnostics and DWARF.
    #[serde(skip_serializing_if = "Option::is_none")]
    line_map: Option<&'a [usize]>,
}

impl<'a, ModuleGraphRepr> TypedProgram<'a, ModuleGraphRepr> {
    #[expect(
        clippy::too_many_arguments,
        reason = "internal constructor: all args are required fields of the wire format"
    )]
    fn new(
        program: &'a hew_parser::ast::Program,
        expr_types: &'a [ExprTypeEntry],
        method_call_receiver_kinds: &'a [MethodCallReceiverKindEntry],
        assign_target_kinds: &'a [AssignTargetKindEntry],
        assign_target_shapes: &'a [AssignTargetShapeEntry],
        lowering_facts: &'a [LoweringFactEntry],
        handle_types: Vec<String>,
        handle_type_repr: HashMap<String, String>,
        drop_funcs: Vec<(String, String)>,
        module_graph: Option<ModuleGraphRepr>,
        source_path: Option<&'a str>,
        line_map: Option<&'a [usize]>,
    ) -> Self {
        Self {
            schema_version: SCHEMA_VERSION,
            items: &program.items,
            module_doc: &program.module_doc,
            expr_types,
            method_call_receiver_kinds,
            assign_target_kinds,
            assign_target_shapes,
            lowering_facts,
            handle_types,
            handle_type_repr,
            drop_funcs,
            module_graph,
            source_path,
            line_map,
        }
    }
}

/// Serialize a [`Program`](hew_parser::ast::Program) to `MessagePack` bytes,
/// including the resolved expression type map.
///
/// Uses named fields (`to_vec_named`) so the format is self-describing and
/// tolerant of field additions.
///
/// # Panics
///
/// Panics if serialization fails, which should never happen for a valid AST.
#[must_use]
#[expect(
    clippy::needless_pass_by_value,
    clippy::implicit_hasher,
    clippy::too_many_arguments,
    reason = "serialization consumes the map"
)]
pub fn serialize_to_msgpack(
    program: &hew_parser::ast::Program,
    expr_types: Vec<ExprTypeEntry>,
    method_call_receiver_kinds: Vec<MethodCallReceiverKindEntry>,
    assign_target_kinds: Vec<AssignTargetKindEntry>,
    assign_target_shapes: Vec<AssignTargetShapeEntry>,
    lowering_facts: Vec<LoweringFactEntry>,
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
    drop_funcs: Vec<(String, String)>,
    source_path: Option<&str>,
    line_map: Option<&[usize]>,
) -> Vec<u8> {
    let typed = TypedProgram::new(
        program,
        &expr_types,
        &method_call_receiver_kinds,
        &assign_target_kinds,
        &assign_target_shapes,
        &lowering_facts,
        handle_types,
        handle_type_repr,
        drop_funcs,
        program.module_graph.as_ref(),
        source_path,
        line_map,
    );
    rmp_serde::to_vec_named(&typed).expect("AST MessagePack serialization failed")
}

/// JSON-friendly module graph: uses `ModuleId.to_string()` as map keys
/// instead of the struct form (which JSON cannot represent as object keys).
#[derive(Debug, Serialize)]
struct ModuleGraphJson<'a> {
    modules: BTreeMap<String, &'a Module>,
    root: &'a ModuleId,
    topo_order: &'a Vec<ModuleId>,
}

/// Serialize a [`Program`](hew_parser::ast::Program) to pretty-printed JSON.
///
/// Produces the same `TypedProgram` structure as [`serialize_to_msgpack`], but
/// encoded as human-readable JSON for debugging. The module graph's modules
/// map uses `ModuleId` display strings (e.g. `"std::net::http"`) as keys
/// instead of the struct form, since JSON objects require string keys.
///
/// # Panics
///
/// Panics if serialization fails, which should never happen for a valid AST.
#[must_use]
#[expect(
    clippy::needless_pass_by_value,
    clippy::implicit_hasher,
    clippy::too_many_arguments,
    reason = "serialization consumes the map"
)]
pub fn serialize_to_json(
    program: &hew_parser::ast::Program,
    expr_types: Vec<ExprTypeEntry>,
    method_call_receiver_kinds: Vec<MethodCallReceiverKindEntry>,
    assign_target_kinds: Vec<AssignTargetKindEntry>,
    assign_target_shapes: Vec<AssignTargetShapeEntry>,
    lowering_facts: Vec<LoweringFactEntry>,
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
    drop_funcs: Vec<(String, String)>,
    source_path: Option<&str>,
    line_map: Option<&[usize]>,
) -> String {
    let module_graph_json = program.module_graph.as_ref().map(|mg| ModuleGraphJson {
        modules: mg.modules.iter().map(|(k, v)| (k.to_string(), v)).collect(),
        root: &mg.root,
        topo_order: &mg.topo_order,
    });
    let typed = TypedProgram::new(
        program,
        &expr_types,
        &method_call_receiver_kinds,
        &assign_target_kinds,
        &assign_target_shapes,
        &lowering_facts,
        handle_types,
        handle_type_repr,
        drop_funcs,
        module_graph_json,
        source_path,
        line_map,
    );
    serde_json::to_string_pretty(&typed).expect("AST JSON serialization failed")
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
            Expr::Block(block)
            | Expr::Unsafe(block)
            | Expr::ScopeLaunch(block)
            | Expr::ScopeSpawn(block) => {
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
            Expr::Scope { body, .. } => collect_block(body, tco, visitor, out),
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
            Expr::StructInit { fields, .. } => {
                for (_, value) in fields {
                    collect_expr(value, tco, visitor, out);
                }
            }
            Expr::Send { target, message } => {
                collect_expr(target, tco, visitor, out);
                collect_expr(message, tco, visitor, out);
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
            Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::Cooperate
            | Expr::This
            | Expr::ScopeCancel
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
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
                if let Some(terminate) = &actor_decl.terminate {
                    collect_block(&terminate.body, tco, visitor, out);
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
            Item::Import(_) | Item::TypeAlias(_) | Item::Wire(_) | Item::ExternBlock(_) => {}
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

/// Deserialize a [`Program`](hew_parser::ast::Program) from `MessagePack` bytes.
///
/// # Errors
///
/// Returns an error if the bytes do not represent a valid MessagePack-encoded
/// `Program`.
pub fn deserialize_from_msgpack(
    data: &[u8],
) -> Result<hew_parser::ast::Program, rmp_serde::decode::Error> {
    rmp_serde::from_slice(data)
}

#[must_use]
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
                },
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
                },
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

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::{
        ChildSpec, FnDecl, IntRadix, Literal, MachineDecl, MachineEvent, MachineState,
        MachineTransition, NamingCase, Program, RestartPolicy, SupervisorDecl, SupervisorStrategy,
        TypeAliasDecl, TypeDecl, TypeDeclKind, VariantDecl, VariantKind, Visibility, WireDecl,
        WireDeclKind, WireFieldDecl,
    };
    use std::collections::HashSet;

    fn round_trip_program(program: &Program) {
        let bytes = serialize_to_msgpack(
            program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        assert!(!bytes.is_empty());

        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program.clone(), restored);
    }

    fn serialize_to_value(program: &Program, expr_types: Vec<ExprTypeEntry>) -> serde_json::Value {
        let bytes = serialize_to_msgpack(
            program,
            expr_types,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload")
    }

    fn named_type(name: &str) -> Spanned<TypeExpr> {
        (
            TypeExpr::Named {
                name: name.to_string(),
                type_args: None,
            },
            0..0,
        )
    }

    fn wire_field(
        name: &str,
        ty: &str,
        field_number: u32,
        json_name: Option<&str>,
        yaml_name: Option<&str>,
        since: Option<u32>,
    ) -> WireFieldDecl {
        WireFieldDecl {
            name: name.into(),
            ty: ty.into(),
            field_number,
            is_optional: false,
            is_repeated: false,
            is_reserved: false,
            is_deprecated: false,
            json_name: json_name.map(Into::into),
            yaml_name: yaml_name.map(Into::into),
            since,
        }
    }

    fn wire_program(
        json_case: Option<NamingCase>,
        yaml_case: Option<NamingCase>,
        fields: Vec<WireFieldDecl>,
    ) -> Program {
        Program {
            items: vec![(
                Item::Wire(WireDecl {
                    visibility: Visibility::Private,
                    kind: WireDeclKind::Struct,
                    name: "Envelope".into(),
                    fields,
                    variants: vec![],
                    json_case,
                    yaml_case,
                }),
                0..40,
            )],
            module_doc: None,
            module_graph: None,
        }
    }

    /// Round-trip: serialize → deserialize should produce an identical AST.
    #[test]
    fn round_trip_simple_program() {
        let program = Program {
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
                        stmts: vec![(
                            Stmt::Expression((
                                Expr::Literal(Literal::Integer {
                                    value: 42,
                                    radix: IntRadix::Decimal,
                                }),
                                10..12,
                            )),
                            5..15,
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..50,
            )],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        assert!(!bytes.is_empty());

        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    /// Verify that a Program with a `ModuleGraph` round-trips correctly.
    #[test]
    fn round_trip_with_module_graph() {
        use hew_parser::module::{Module, ModuleGraph, ModuleId, ModuleImport};
        use std::collections::HashMap;

        let root_id = ModuleId {
            path: vec!["root".into()],
        };
        let dep_id = ModuleId {
            path: vec!["dep".into()],
        };

        let mut modules = HashMap::new();
        modules.insert(
            root_id.clone(),
            Module {
                id: root_id.clone(),
                items: vec![],
                imports: vec![ModuleImport {
                    target: dep_id.clone(),
                    spec: None,
                    span: 0..0,
                }],
                source_paths: Vec::new(),
                doc: Some("root module".into()),
            },
        );
        modules.insert(
            dep_id.clone(),
            Module {
                id: dep_id.clone(),
                items: vec![],
                imports: vec![],
                source_paths: Vec::new(),
                doc: None,
            },
        );

        let graph = ModuleGraph {
            modules,
            root: root_id.clone(),
            topo_order: vec![dep_id, root_id],
        };

        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: Some(graph),
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        assert!(!bytes.is_empty());

        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    /// Verify that an empty program round-trips correctly.
    #[test]
    fn round_trip_empty_program() {
        let program = Program {
            items: vec![],
            module_doc: Some("Module doc".into()),
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    /// Round-trip a `MachineDecl` through `MessagePack`.
    #[test]
    fn round_trip_machine_decl() {
        let program = Program {
            items: vec![(
                Item::Machine(MachineDecl {
                    visibility: Visibility::Pub,
                    name: "TrafficLight".into(),
                    has_default: false,
                    states: vec![
                        MachineState {
                            name: "Red".into(),
                            fields: vec![],
                        },
                        MachineState {
                            name: "Green".into(),
                            fields: vec![(
                                "duration".into(),
                                (
                                    TypeExpr::Named {
                                        name: "Int".into(),
                                        type_args: None,
                                    },
                                    10..13,
                                ),
                            )],
                        },
                    ],
                    events: vec![MachineEvent {
                        name: "Timer".into(),
                        fields: vec![],
                    }],
                    transitions: vec![MachineTransition {
                        event_name: "Timer".into(),
                        source_state: "Red".into(),
                        target_state: "Green".into(),
                        guard: None,
                        body: (
                            Expr::Block(Block {
                                stmts: vec![],
                                trailing_expr: Some(Box::new((
                                    Expr::Literal(Literal::Integer {
                                        value: 0,
                                        radix: IntRadix::Decimal,
                                    }),
                                    20..21,
                                ))),
                            }),
                            15..25,
                        ),
                    }],
                }),
                0..100,
            )],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        assert!(!bytes.is_empty());

        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    #[test]
    fn round_trip_mutable_function_param() {
        let parsed = hew_parser::parse("fn bump(var x: int) -> int { x = x + 1; x }");
        assert!(parsed.errors.is_empty(), "errors: {:?}", parsed.errors);

        let bytes = serialize_to_msgpack(
            &parsed.program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");

        assert_eq!(parsed.program, restored);
        match &restored.items[0].0 {
            Item::Function(function) => assert!(function.params[0].is_mutable),
            other => panic!("expected function item, got: {other:?}"),
        }
    }

    #[test]
    fn method_call_receiver_kinds_serialize_to_wire_field() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![MethodCallReceiverKindEntry {
                start: 10,
                end: 20,
                kind: MethodCallReceiverKindData::NamedTypeInstance {
                    type_name: "Widget".to_string(),
                },
            }],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
        let kinds = value
            .get("method_call_receiver_kinds")
            .and_then(serde_json::Value::as_array)
            .expect("method_call_receiver_kinds should be present on the wire");
        assert_eq!(kinds.len(), 1);
        assert_eq!(
            kinds[0].get("kind").and_then(serde_json::Value::as_str),
            Some("named_type_instance")
        );
        assert_eq!(
            kinds[0]
                .get("type_name")
                .and_then(serde_json::Value::as_str),
            Some("Widget")
        );
    }

    #[test]
    fn lowering_facts_serialize_to_wire_field() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![LoweringFactEntry {
                start: 10,
                end: 20,
                fact: CheckedLoweringFact {
                    kind: hew_types::LoweringKind::HashSet,
                    element_type: hew_types::HashSetElementType::Str,
                    abi_variant: hew_types::HashSetAbi::String,
                    drop_kind: hew_types::DropKind::HashSetFree,
                },
            }],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
        let facts = value
            .get("lowering_facts")
            .and_then(serde_json::Value::as_array)
            .expect("lowering_facts should be present on the wire");
        assert_eq!(facts.len(), 1);
        assert_eq!(
            facts[0].get("kind").and_then(serde_json::Value::as_str),
            Some("hash_set")
        );
        assert_eq!(
            facts[0]
                .get("element_type")
                .and_then(serde_json::Value::as_str),
            Some("str")
        );
        assert_eq!(
            facts[0]
                .get("abi_variant")
                .and_then(serde_json::Value::as_str),
            Some("string")
        );
        assert_eq!(
            facts[0]
                .get("drop_kind")
                .and_then(serde_json::Value::as_str),
            Some("hash_set_free")
        );
    }

    #[test]
    fn assign_target_kinds_serialize_to_wire_field() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![AssignTargetKindEntry {
                start: 10,
                end: 20,
                kind: AssignTargetKindData::LocalVar,
            }],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
        let kinds = value
            .get("assign_target_kinds")
            .and_then(serde_json::Value::as_array)
            .expect("assign_target_kinds should be present on the wire");
        assert_eq!(kinds.len(), 1);
        assert_eq!(
            kinds[0].get("kind").and_then(serde_json::Value::as_str),
            Some("local_var")
        );
    }

    #[test]
    fn build_method_call_receiver_kind_entries_only_emits_surviving_method_calls() {
        let program = Program {
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
                        stmts: vec![(
                            Stmt::Expression((
                                Expr::Call {
                                    function: Box::new((Expr::Identifier("helper".into()), 30..36)),
                                    type_args: None,
                                    args: vec![],
                                    is_tail_call: false,
                                },
                                30..38,
                            )),
                            30..38,
                        )],
                        trailing_expr: Some(Box::new((
                            Expr::MethodCall {
                                receiver: Box::new((Expr::Identifier("widget".into()), 10..16)),
                                method: "value_plus_one".into(),
                                args: vec![CallArg::Positional((
                                    Expr::Literal(Literal::Integer {
                                        value: 1,
                                        radix: IntRadix::Decimal,
                                    }),
                                    17..18,
                                ))],
                            },
                            10..28,
                        ))),
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..40,
            )],
            module_doc: None,
            module_graph: None,
        };

        let tco = TypeCheckOutput {
            expr_types: HashMap::new(),
            method_call_receiver_kinds: HashMap::from([
                (
                    SpanKey { start: 10, end: 28 },
                    CheckedMethodCallReceiverKind::NamedTypeInstance {
                        type_name: "Widget".to_string(),
                    },
                ),
                (
                    SpanKey { start: 30, end: 38 },
                    CheckedMethodCallReceiverKind::TraitObject {
                        trait_name: "Stale".to_string(),
                    },
                ),
            ]),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
        };

        let entries = build_method_call_receiver_kind_entries(&program, &tco);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].start, 10);
        assert_eq!(entries[0].end, 28);
        assert_eq!(
            entries[0].kind,
            MethodCallReceiverKindData::NamedTypeInstance {
                type_name: "Widget".to_string()
            }
        );
    }

    #[test]
    fn build_assign_target_kind_entries_emits_assignment_targets() {
        use hew_types::check::{AssignTargetKind, SpanKey, TypeCheckOutput};
        use std::collections::{HashMap, HashSet};

        let assign_target_span = 10usize..16usize;

        let program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "set_x".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![(
                            Stmt::Assign {
                                target: (Expr::Identifier("x".into()), assign_target_span.clone()),
                                op: None,
                                value: (
                                    Expr::Literal(Literal::Integer {
                                        value: 1,
                                        radix: IntRadix::Decimal,
                                    }),
                                    17..18,
                                ),
                            },
                            10..18,
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        };

        let tco = TypeCheckOutput {
            expr_types: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            assign_target_kinds: HashMap::from([(
                SpanKey {
                    start: assign_target_span.start,
                    end: assign_target_span.end,
                },
                AssignTargetKind::LocalVar,
            )]),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
        };

        let entries = build_assign_target_kind_entries(&program, &tco);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].start, assign_target_span.start);
        assert_eq!(entries[0].end, assign_target_span.end);
        assert_eq!(entries[0].kind, AssignTargetKindData::LocalVar);
    }

    #[test]
    fn build_lowering_fact_entries_emits_free_call_len_entries() {
        let call_span = 10usize..16usize;
        let program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "len_of_set".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![],
                        trailing_expr: Some(Box::new((
                            Expr::Call {
                                function: Box::new((Expr::Identifier("len".into()), 10..13)),
                                type_args: None,
                                args: vec![CallArg::Positional((
                                    Expr::Identifier("set".into()),
                                    14..16,
                                ))],
                                is_tail_call: false,
                            },
                            call_span.clone(),
                        ))),
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        };

        let key = SpanKey {
            start: call_span.start,
            end: call_span.end,
        };
        let tco = TypeCheckOutput {
            expr_types: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::from([(
                key,
                CheckedLoweringFact {
                    kind: hew_types::LoweringKind::HashSet,
                    element_type: hew_types::HashSetElementType::I64,
                    abi_variant: hew_types::HashSetAbi::Int64,
                    drop_kind: hew_types::DropKind::HashSetFree,
                },
            )]),
            method_call_rewrites: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
        };

        let entries = build_lowering_fact_entries(&program, &tco);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].start, call_span.start);
        assert_eq!(entries[0].end, call_span.end);
        assert_eq!(
            entries[0].fact.element_type,
            hew_types::HashSetElementType::I64
        );
    }

    #[test]
    fn build_method_call_receiver_kind_entries_follow_module_graph_items() {
        use hew_parser::module::{Module, ModuleGraph, ModuleId};

        let root = ModuleId::root();
        let imported = ModuleId::new(vec!["std".into(), "json".into()]);
        let method_call_span = 10..28;

        let imported_fn = (
            Item::Function(FnDecl {
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
                    stmts: vec![(
                        Stmt::Expression((
                            Expr::MethodCall {
                                receiver: Box::new((Expr::Identifier("value".into()), 10..15)),
                                method: "stringify".into(),
                                args: vec![],
                            },
                            method_call_span.clone(),
                        )),
                        method_call_span.clone(),
                    )],
                    trailing_expr: None,
                },
                doc_comment: None,
                decl_span: 0..0,
                fn_span: 0..0,
            }),
            0..30,
        );

        let mut module_graph = ModuleGraph::new(root.clone());
        module_graph.add_module(Module {
            id: root.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        });
        module_graph.add_module(Module {
            id: imported.clone(),
            items: vec![imported_fn],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        });
        module_graph.topo_order = vec![root, imported];

        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: Some(module_graph),
        };

        let tco = TypeCheckOutput {
            expr_types: HashMap::new(),
            method_call_receiver_kinds: HashMap::from([(
                SpanKey {
                    start: method_call_span.start,
                    end: method_call_span.end,
                },
                CheckedMethodCallReceiverKind::HandleInstance {
                    type_name: "json.Value".to_string(),
                },
            )]),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
        };

        let entries = build_method_call_receiver_kind_entries(&program, &tco);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].start, method_call_span.start);
        assert_eq!(entries[0].end, method_call_span.end);
        assert_eq!(
            entries[0].kind,
            MethodCallReceiverKindData::HandleInstance {
                type_name: "json.Value".to_string()
            }
        );
    }

    /// Verify that the serialized msgpack contains a `schema_version` field
    /// set to `SCHEMA_VERSION`, and that a round-trip still produces an
    /// identical `Program` (the version field is metadata, not part of `Program`).
    #[test]
    fn schema_version_round_trip() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );

        // Deserialize into a serde_json::Value to inspect the schema_version field.
        // rmp_serde can deserialize msgpack into any Deserialize type, and
        // serde_json::Value is a convenient untyped representation.
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize to generic value");

        let obj = value.as_object().expect("top-level should be a map");
        let sv = obj
            .get("schema_version")
            .expect("schema_version key should be present");
        assert_eq!(
            sv.as_u64(),
            Some(u64::from(SCHEMA_VERSION)),
            "schema_version should equal SCHEMA_VERSION"
        );

        // The round-trip should still produce the same Program (schema_version
        // is extra metadata that Program doesn't carry, so it's silently ignored).
        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    #[test]
    fn build_method_call_receiver_kind_entries_retains_rewritten_direct_calls() {
        use hew_parser::ast::{CallArg, Expr, FnDecl, Stmt, Visibility};
        use hew_types::check::{SpanKey, TypeCheckOutput};
        use std::collections::{HashMap, HashSet};

        let call_span = 10..28;
        let program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    is_pure: false,
                    visibility: Visibility::Private,
                    name: "use_conn".to_string(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![(
                            Stmt::Expression((
                                Expr::Call {
                                    function: Box::new((
                                        Expr::Identifier("hew_tcp_close".to_string()),
                                        call_span.clone(),
                                    )),
                                    type_args: None,
                                    args: vec![CallArg::Positional((
                                        Expr::Identifier("conn".to_string()),
                                        10..14,
                                    ))],
                                    is_tail_call: false,
                                },
                                call_span.clone(),
                            )),
                            call_span.clone(),
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                call_span.clone(),
            )],
            module_doc: None,
            module_graph: None,
        };

        let tco = TypeCheckOutput {
            expr_types: HashMap::new(),
            method_call_receiver_kinds: HashMap::from([(
                SpanKey {
                    start: call_span.start,
                    end: call_span.end,
                },
                CheckedMethodCallReceiverKind::HandleInstance {
                    type_name: "net.Connection".to_string(),
                },
            )]),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::from([(
                SpanKey {
                    start: call_span.start,
                    end: call_span.end,
                },
                hew_types::MethodCallRewrite::RewriteToFunction {
                    c_symbol: "hew_tcp_close".to_string(),
                },
            )]),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
        };

        let entries = build_method_call_receiver_kind_entries(&program, &tco);
        assert_eq!(entries.len(), 1);
        assert_eq!(
            entries[0].kind,
            MethodCallReceiverKindData::HandleInstance {
                type_name: "net.Connection".to_string()
            }
        );
    }

    /// Verify that `build_assign_target_shape_entries` populates `is_unsigned`
    /// correctly for a single assignment whose target is an unsigned integer.
    #[test]
    fn build_assign_target_shape_entries_unsigned_target() {
        use hew_parser::ast::{Block, Expr, FnDecl, Stmt, Visibility};
        use hew_types::check::{AssignTargetKind, AssignTargetShape, SpanKey, TypeCheckOutput};
        use std::collections::{HashMap, HashSet};

        let assign_target_span = 5..10;

        // Build a minimal AST: fn f() { <target>[5..10] = rhs }
        let program = Program {
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
                            Stmt::Assign {
                                target: (Expr::Identifier("x".into()), assign_target_span.clone()),
                                op: None,
                                value: (Expr::Identifier("v".into()), 12..13),
                            },
                            assign_target_span.clone(),
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        };

        let key = SpanKey {
            start: assign_target_span.start,
            end: assign_target_span.end,
        };

        let tco = TypeCheckOutput {
            expr_types: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            assign_target_kinds: HashMap::from([(key.clone(), AssignTargetKind::LocalVar)]),
            assign_target_shapes: HashMap::from([(
                key.clone(),
                AssignTargetShape { is_unsigned: true },
            )]),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
        };

        let entries = build_assign_target_shape_entries(&program, &tco);
        assert_eq!(entries.len(), 1, "expected one shape entry");
        assert_eq!(entries[0].start, assign_target_span.start);
        assert_eq!(entries[0].end, assign_target_span.end);
        assert!(
            entries[0].is_unsigned,
            "expected is_unsigned = true for unsigned target"
        );
    }

    /// Verify that `build_assign_target_shape_entries` records `is_unsigned = false`
    /// for a signed integer target.
    #[test]
    fn build_assign_target_shape_entries_signed_target() {
        use hew_parser::ast::{Block, Expr, FnDecl, Stmt, Visibility};
        use hew_types::check::{AssignTargetKind, AssignTargetShape, SpanKey, TypeCheckOutput};
        use std::collections::{HashMap, HashSet};

        let assign_target_span = 5..10;

        let program = Program {
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
                            Stmt::Assign {
                                target: (Expr::Identifier("x".into()), assign_target_span.clone()),
                                op: None,
                                value: (Expr::Identifier("v".into()), 12..13),
                            },
                            assign_target_span.clone(),
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        };

        let key = SpanKey {
            start: assign_target_span.start,
            end: assign_target_span.end,
        };

        let tco = TypeCheckOutput {
            expr_types: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            assign_target_kinds: HashMap::from([(key.clone(), AssignTargetKind::LocalVar)]),
            assign_target_shapes: HashMap::from([(
                key.clone(),
                AssignTargetShape { is_unsigned: false },
            )]),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
        };

        let entries = build_assign_target_shape_entries(&program, &tco);
        assert_eq!(entries.len(), 1, "expected one shape entry");
        assert!(!entries[0].is_unsigned, "expected is_unsigned = false");
    }

    /// Verify round-trip: serialize shape entries to msgpack and check that
    /// `assign_target_shapes` is present with the correct `is_unsigned` flag.
    #[test]
    fn assign_target_shapes_msgpack_roundtrip() {
        let shapes = vec![
            AssignTargetShapeEntry {
                start: 5,
                end: 10,
                is_unsigned: true,
            },
            AssignTargetShapeEntry {
                start: 20,
                end: 25,
                is_unsigned: false,
            },
        ];

        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            shapes.clone(),
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );

        // Deserialize to untyped map and inspect the assign_target_shapes array.
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize to generic value");
        let obj = value.as_object().expect("top-level should be a map");
        let arr = obj
            .get("assign_target_shapes")
            .expect("assign_target_shapes key should be present");
        let arr = arr
            .as_array()
            .expect("assign_target_shapes should be array");
        assert_eq!(arr.len(), 2, "expected 2 shape entries");

        // First entry: unsigned
        assert_eq!(arr[0]["start"], 5u64);
        assert_eq!(arr[0]["end"], 10u64);
        assert_eq!(arr[0]["is_unsigned"], true);

        // Second entry: signed
        assert_eq!(arr[1]["start"], 20u64);
        assert_eq!(arr[1]["end"], 25u64);
        assert_eq!(arr[1]["is_unsigned"], false);
    }

    // ── v7 reader-boundary certification tests ────────────────────────────

    /// Certify that `MethodCallReceiverKindData::TraitObject` reaches the wire
    /// with `kind = "trait_object"` and the correct `trait_name` field.
    #[test]
    fn trait_object_receiver_kind_serializes_to_wire_field() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![MethodCallReceiverKindEntry {
                start: 5,
                end: 15,
                kind: MethodCallReceiverKindData::TraitObject {
                    trait_name: "Greeter".to_string(),
                },
            }],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
        let kinds = value
            .get("method_call_receiver_kinds")
            .and_then(serde_json::Value::as_array)
            .expect("method_call_receiver_kinds should be present");
        assert_eq!(kinds.len(), 1);
        assert_eq!(kinds[0]["start"], 5u64);
        assert_eq!(kinds[0]["end"], 15u64);
        assert_eq!(
            kinds[0].get("kind").and_then(serde_json::Value::as_str),
            Some("trait_object"),
            "kind should be 'trait_object'"
        );
        assert_eq!(
            kinds[0]
                .get("trait_name")
                .and_then(serde_json::Value::as_str),
            Some("Greeter"),
            "trait_name should be 'Greeter'"
        );
    }

    #[test]
    fn handle_receiver_kind_serializes_to_wire_field() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![MethodCallReceiverKindEntry {
                start: 5,
                end: 15,
                kind: MethodCallReceiverKindData::HandleInstance {
                    type_name: "net.Connection".to_string(),
                },
            }],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
        let kinds = value
            .get("method_call_receiver_kinds")
            .and_then(serde_json::Value::as_array)
            .expect("method_call_receiver_kinds should be present");
        assert_eq!(kinds.len(), 1);
        assert_eq!(kinds[0]["start"], 5u64);
        assert_eq!(kinds[0]["end"], 15u64);
        assert_eq!(
            kinds[0].get("kind").and_then(serde_json::Value::as_str),
            Some("handle_instance"),
            "kind should be 'handle_instance'"
        );
        assert_eq!(
            kinds[0]
                .get("type_name")
                .and_then(serde_json::Value::as_str),
            Some("net.Connection"),
            "type_name should be 'net.Connection'"
        );
    }

    /// Certify that `AssignTargetKindData` variants `ActorField`, `FieldAccess`,
    /// and `Index` each reach the wire with the correct `kind` string.
    ///
    /// `LocalVar` is already covered by `assign_target_kinds_serialize_to_wire_field`.
    #[test]
    fn assign_target_kind_non_local_variants_serialize_to_wire_field() {
        let cases: &[(&str, AssignTargetKindData)] = &[
            ("actor_field", AssignTargetKindData::ActorField),
            ("field_access", AssignTargetKindData::FieldAccess),
            ("index", AssignTargetKindData::Index),
        ];

        for (expected_kind_str, variant) in cases {
            let program = Program {
                items: vec![],
                module_doc: None,
                module_graph: None,
            };

            let bytes = serialize_to_msgpack(
                &program,
                vec![],
                vec![],
                vec![AssignTargetKindEntry {
                    start: 1,
                    end: 5,
                    kind: variant.clone(),
                }],
                vec![],
                vec![],
                vec![],
                HashMap::new(),
                vec![],
                None,
                None,
            );
            let value: serde_json::Value =
                rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
            let kinds = value
                .get("assign_target_kinds")
                .and_then(serde_json::Value::as_array)
                .expect("assign_target_kinds should be present");
            assert_eq!(
                kinds.len(),
                1,
                "variant {expected_kind_str}: expected 1 entry"
            );
            assert_eq!(
                kinds[0].get("kind").and_then(serde_json::Value::as_str),
                Some(*expected_kind_str),
                "variant {expected_kind_str}: kind field mismatch on wire"
            );
        }
    }

    /// Certify that `LoweringFactEntry` with `I64`/`Int64` (integer `HashSet`)
    /// reaches the wire with `element_type = "i64"` and `abi_variant = "int64"`.
    #[test]
    fn lowering_facts_i64_int64_serialize_to_wire_field() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![LoweringFactEntry {
                start: 0,
                end: 8,
                fact: CheckedLoweringFact {
                    kind: hew_types::LoweringKind::HashSet,
                    element_type: hew_types::HashSetElementType::I64,
                    abi_variant: hew_types::HashSetAbi::Int64,
                    drop_kind: hew_types::DropKind::HashSetFree,
                },
            }],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
        let facts = value
            .get("lowering_facts")
            .and_then(serde_json::Value::as_array)
            .expect("lowering_facts should be present");
        assert_eq!(facts.len(), 1);
        assert_eq!(
            facts[0]
                .get("element_type")
                .and_then(serde_json::Value::as_str),
            Some("i64"),
            "element_type should be 'i64' for I64 HashSet"
        );
        assert_eq!(
            facts[0]
                .get("abi_variant")
                .and_then(serde_json::Value::as_str),
            Some("int64"),
            "abi_variant should be 'int64' for I64 HashSet"
        );
    }

    /// Certify that `LoweringFactEntry` with `U64`/`Int64` (unsigned integer `HashSet`)
    /// reaches the wire with `element_type = "u64"` and `abi_variant = "int64"`.
    #[test]
    fn lowering_facts_u64_int64_serialize_to_wire_field() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![LoweringFactEntry {
                start: 0,
                end: 8,
                fact: CheckedLoweringFact {
                    kind: hew_types::LoweringKind::HashSet,
                    element_type: hew_types::HashSetElementType::U64,
                    abi_variant: hew_types::HashSetAbi::Int64,
                    drop_kind: hew_types::DropKind::HashSetFree,
                },
            }],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
        let facts = value
            .get("lowering_facts")
            .and_then(serde_json::Value::as_array)
            .expect("lowering_facts should be present");
        assert_eq!(facts.len(), 1);
        assert_eq!(
            facts[0]
                .get("element_type")
                .and_then(serde_json::Value::as_str),
            Some("u64"),
            "element_type should be 'u64' for U64 HashSet"
        );
        assert_eq!(
            facts[0]
                .get("abi_variant")
                .and_then(serde_json::Value::as_str),
            Some("int64"),
            "abi_variant should be 'int64' for U64 HashSet"
        );
    }

    /// Certify that a populated `expr_types` entry with a concrete (non-`Infer`)
    /// `TypeExpr::Named` reaches the wire with the correct shape that the C++
    /// reader can parse.
    ///
    /// The wire shape for `Spanned<TypeExpr>` is `[TypeExpr_enum_value, span_map]`.
    /// The wire shape for `TypeExpr::Named { name, type_args: None }` (externally
    /// tagged) is `{"Named": {"name": "Int", "type_args": null}}`.
    #[test]
    fn expr_types_populated_named_entry_serializes_to_wire() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(
            &program,
            vec![ExprTypeEntry {
                start: 3,
                end: 12,
                ty: (
                    TypeExpr::Named {
                        name: "Int".to_string(),
                        type_args: None,
                    },
                    3..12,
                ),
            }],
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            HashMap::new(),
            vec![],
            None,
            None,
        );
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");
        let entries = value
            .get("expr_types")
            .and_then(serde_json::Value::as_array)
            .expect("expr_types should be present on the wire");
        assert_eq!(entries.len(), 1, "expected one expr_types entry");
        assert_eq!(entries[0]["start"], 3u64);
        assert_eq!(entries[0]["end"], 12u64);

        // ty = [TypeExpr, Span] as a 2-element array
        let ty_arr = entries[0]["ty"]
            .as_array()
            .expect("ty should be a 2-element array");
        assert_eq!(ty_arr.len(), 2, "ty should be [TypeExpr_value, span]");

        // TypeExpr::Named → {"Named": {"name": "Int", "type_args": null}}
        let named_payload = ty_arr[0]
            .get("Named")
            .expect("TypeExpr should be externally tagged with 'Named' key");
        assert_eq!(
            named_payload
                .get("name")
                .and_then(serde_json::Value::as_str),
            Some("Int"),
            "Named type_name should be 'Int'"
        );

        // Span in ty
        assert_eq!(ty_arr[1]["start"], 3u64);
        assert_eq!(ty_arr[1]["end"], 12u64);
    }

    #[test]
    fn program_metadata_serializes_to_wire() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };
        let handle_type_repr = HashMap::from([
            ("fd.Socket".to_string(), "i32".to_string()),
            ("http.Server".to_string(), "handle".to_string()),
        ]);
        let line_map = [0usize, 17, 42];

        let bytes = serialize_to_msgpack(
            &program,
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
            vec!["fd.Socket".to_string(), "http.Server".to_string()],
            handle_type_repr,
            vec![(
                "http.Server".to_string(),
                "hew_http_server_close".to_string(),
            )],
            Some("/workspace/examples/service.hew"),
            Some(&line_map),
        );

        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize msgpack payload");

        let handle_types = value
            .get("handle_types")
            .and_then(serde_json::Value::as_array)
            .expect("handle_types should be present on the wire");
        assert_eq!(handle_types.len(), 2);
        assert_eq!(handle_types[0].as_str(), Some("fd.Socket"));
        assert_eq!(handle_types[1].as_str(), Some("http.Server"));

        let handle_type_repr = value
            .get("handle_type_repr")
            .and_then(serde_json::Value::as_object)
            .expect("handle_type_repr should be present on the wire");
        assert_eq!(
            handle_type_repr
                .get("fd.Socket")
                .and_then(serde_json::Value::as_str),
            Some("i32")
        );
        assert_eq!(
            handle_type_repr
                .get("http.Server")
                .and_then(serde_json::Value::as_str),
            Some("handle")
        );

        let drop_funcs = value
            .get("drop_funcs")
            .and_then(serde_json::Value::as_array)
            .expect("drop_funcs should be present on the wire");
        assert_eq!(drop_funcs.len(), 1);
        assert_eq!(drop_funcs[0][0].as_str(), Some("http.Server"));
        assert_eq!(drop_funcs[0][1].as_str(), Some("hew_http_server_close"));
        assert_eq!(
            value.get("source_path").and_then(serde_json::Value::as_str),
            Some("/workspace/examples/service.hew")
        );

        let line_map = value
            .get("line_map")
            .and_then(serde_json::Value::as_array)
            .expect("line_map should be present on the wire");
        assert_eq!(line_map.len(), 3);
        assert_eq!(line_map[0].as_u64(), Some(0));
        assert_eq!(line_map[1].as_u64(), Some(17));
        assert_eq!(line_map[2].as_u64(), Some(42));
    }

    #[test]
    fn round_trip_empty_string_literal() {
        round_trip_program(&Program {
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
                        trailing_expr: Some(Box::new((
                            Expr::Literal(Literal::String(String::new())),
                            10..12,
                        ))),
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        });
    }

    #[test]
    fn round_trip_char_literal() {
        round_trip_program(&Program {
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
                        trailing_expr: Some(Box::new((Expr::Literal(Literal::Char('x')), 10..13))),
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        });
    }

    #[test]
    fn round_trip_duration_literal() {
        round_trip_program(&Program {
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
                        trailing_expr: Some(Box::new((
                            Expr::Literal(Literal::Duration(5_000_000_000)),
                            10..12,
                        ))),
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        });
    }

    #[test]
    fn expr_types_integer_width_names_serialize_to_wire() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };
        let expr_types = [
            "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "f32", "f64",
        ]
        .into_iter()
        .enumerate()
        .map(|(index, name)| ExprTypeEntry {
            start: index * 10,
            end: index * 10 + 2,
            ty: named_type(name),
        })
        .collect();

        let json = serde_json::to_string(&serialize_to_value(&program, expr_types))
            .expect("value should serialize to json");
        for name in [
            "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "f32", "f64",
        ] {
            assert!(
                json.contains(&format!("\"name\":\"{name}\"")),
                "missing {name}: {json}"
            );
        }
    }

    #[test]
    fn round_trip_enum_with_tuple_variant() {
        round_trip_program(&Program {
            items: vec![(
                Item::TypeDecl(TypeDecl {
                    visibility: Visibility::Private,
                    kind: TypeDeclKind::Enum,
                    name: "Message".into(),
                    type_params: None,
                    where_clause: None,
                    body: vec![TypeBodyItem::Variant(VariantDecl {
                        name: "Data".into(),
                        kind: VariantKind::Tuple(vec![named_type("i32"), named_type("String")]),
                    })],
                    doc_comment: None,
                    wire: None,
                    is_indirect: false,
                }),
                0..30,
            )],
            module_doc: None,
            module_graph: None,
        });
    }

    #[test]
    fn round_trip_wire_decl() {
        round_trip_program(&wire_program(
            Some(NamingCase::CamelCase),
            None,
            vec![
                wire_field("id", "i32", 1, None, None, None),
                WireFieldDecl {
                    is_optional: true,
                    ..wire_field("payload", "String", 2, Some("body"), None, Some(2))
                },
            ],
        ));
    }

    #[test]
    fn round_trip_wire_decl_with_yaml_case() {
        round_trip_program(&wire_program(
            Some(NamingCase::CamelCase),
            Some(NamingCase::SnakeCase),
            vec![
                wire_field("requestId", "i32", 1, None, Some("request_id"), None),
                WireFieldDecl {
                    is_optional: true,
                    ..wire_field("payloadBody", "String", 2, Some("body"), None, Some(2))
                },
            ],
        ));
    }

    #[test]
    fn wire_decl_yaml_case_serializes_correct_variant_name() {
        for (yaml_case, expected) in [
            (NamingCase::SnakeCase, "SnakeCase"),
            (NamingCase::KebabCase, "KebabCase"),
        ] {
            let value = serialize_to_value(
                &wire_program(
                    Some(NamingCase::CamelCase),
                    Some(yaml_case),
                    vec![wire_field(
                        "requestId",
                        "i32",
                        1,
                        None,
                        Some("custom_key"),
                        None,
                    )],
                ),
                vec![],
            );
            let items = value
                .get("items")
                .and_then(serde_json::Value::as_array)
                .expect("items should be present on the wire");
            let wire_decl = items[0][0]
                .get("Wire")
                .expect("first item should be a Wire decl");
            assert_eq!(
                wire_decl
                    .get("yaml_case")
                    .and_then(serde_json::Value::as_str),
                Some(expected),
                "yaml_case should serialize as {expected}"
            );
            let fields = wire_decl
                .get("fields")
                .and_then(serde_json::Value::as_array)
                .expect("wire fields should be present");
            assert_eq!(
                fields[0]
                    .get("yaml_name")
                    .and_then(serde_json::Value::as_str),
                Some("custom_key"),
                "yaml_name should serialize with its explicit wire key"
            );
        }
    }

    #[test]
    fn wire_decl_non_camel_json_naming_case_round_trip() {
        for json_case in [
            NamingCase::PascalCase,
            NamingCase::SnakeCase,
            NamingCase::ScreamingSnake,
            NamingCase::KebabCase,
        ] {
            round_trip_program(&wire_program(
                Some(json_case),
                None,
                vec![
                    wire_field("id", "i32", 1, None, None, None),
                    WireFieldDecl {
                        is_optional: true,
                        ..wire_field("payload", "String", 2, Some("body"), None, Some(2))
                    },
                ],
            ));
        }
    }

    #[test]
    fn round_trip_type_alias_decl() {
        round_trip_program(&Program {
            items: vec![(
                Item::TypeAlias(TypeAliasDecl {
                    visibility: Visibility::Pub,
                    name: "UserId".into(),
                    ty: named_type("i64"),
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        });
    }

    #[test]
    fn round_trip_supervisor_decl() {
        round_trip_program(&Program {
            items: vec![(
                Item::Supervisor(SupervisorDecl {
                    visibility: Visibility::Pub,
                    name: "Pool".into(),
                    strategy: Some(SupervisorStrategy::OneForOne),
                    max_restarts: Some(3),
                    window: Some("30".into()),
                    children: vec![ChildSpec {
                        name: "worker".into(),
                        actor_type: "Worker".into(),
                        args: vec![(
                            Expr::Literal(Literal::Integer {
                                value: 1,
                                radix: IntRadix::Decimal,
                            }),
                            0..0,
                        )],
                        restart: Some(RestartPolicy::Permanent),
                    }],
                }),
                0..40,
            )],
            module_doc: None,
            module_graph: None,
        });
    }

    #[test]
    fn expr_types_generic_container_serialize_to_wire() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };
        let expr_types = vec![
            ExprTypeEntry {
                start: 0,
                end: 10,
                ty: (
                    TypeExpr::Named {
                        name: "Vec".into(),
                        type_args: Some(vec![named_type("i32")]),
                    },
                    0..0,
                ),
            },
            ExprTypeEntry {
                start: 11,
                end: 22,
                ty: (
                    TypeExpr::Named {
                        name: "Option".into(),
                        type_args: Some(vec![named_type("String")]),
                    },
                    0..0,
                ),
            },
        ];

        let json = serde_json::to_string(&serialize_to_value(&program, expr_types))
            .expect("value should serialize to json");
        for name in ["Vec", "Option", "i32", "String"] {
            assert!(
                json.contains(&format!("\"name\":\"{name}\"")),
                "missing {name}: {json}"
            );
        }
    }

    #[test]
    fn round_trip_empty_byte_string_expr() {
        round_trip_program(&Program {
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
                        trailing_expr: Some(Box::new((Expr::ByteStringLiteral(vec![]), 10..12))),
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                    fn_span: 0..0,
                }),
                0..20,
            )],
            module_doc: None,
            module_graph: None,
        });
    }

    #[test]
    fn round_trip_nested_struct_type_decls() {
        round_trip_program(&Program {
            items: vec![
                (
                    Item::TypeDecl(TypeDecl {
                        visibility: Visibility::Private,
                        kind: TypeDeclKind::Struct,
                        name: "Inner".into(),
                        type_params: None,
                        where_clause: None,
                        body: vec![TypeBodyItem::Field {
                            name: "value".into(),
                            ty: named_type("i32"),
                            attributes: vec![],
                        }],
                        doc_comment: None,
                        wire: None,
                        is_indirect: false,
                    }),
                    0..20,
                ),
                (
                    Item::TypeDecl(TypeDecl {
                        visibility: Visibility::Private,
                        kind: TypeDeclKind::Struct,
                        name: "Outer".into(),
                        type_params: None,
                        where_clause: None,
                        body: vec![TypeBodyItem::Field {
                            name: "inner".into(),
                            ty: named_type("Inner"),
                            attributes: vec![],
                        }],
                        doc_comment: None,
                        wire: None,
                        is_indirect: false,
                    }),
                    21..40,
                ),
            ],
            module_doc: None,
            module_graph: None,
        });
    }
}
