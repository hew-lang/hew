use hew_parser::ast::{BinaryOp, ResourceMarker, Span};
use hew_types::ResolvedTy;

use crate::ids::{BindingId, HirNodeId, ItemId, ResolvedRef, ScopeId, SiteId};
use crate::value_class::TypeClassTable;
use crate::{IntentKind, ValueClass};

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    pub items: Vec<HirItem>,
    /// Per-named-type classification table populated during HIR lowering from
    /// each `Item::TypeDecl` carrying a `#[resource]` or `#[linear]` marker.
    /// Keyed by type name; value is `(marker, close_method_name)` where
    /// `close_method_name` is `Some(name)` for `@resource` types (the
    /// consuming method named `close`) and `None` for `@linear` and `None`-
    /// marked types.
    ///
    /// This is the single authority for downstream phases asking "is this
    /// Named type a resource/linear?" — `ValueClass::of_ty(ty, &type_classes)`
    /// reads from here. No phase re-derives the answer by walking the parser
    /// AST. LESSONS: `type-info-survival`.
    pub type_classes: TypeClassTable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirItem {
    Function(HirFn),
    TypeDecl(HirTypeDecl),
}

/// Lowered top-level type declaration.
///
/// Carries the `#[resource]` / `#[linear]` marker (if any), the list of
/// consuming-method names lifted from the parser body, and the field set.
/// The field set is not consumed by the v0.5 vertical-slice MIR but is
/// recorded for snapshot stability and future analysis passes.
#[derive(Debug, Clone, PartialEq)]
pub struct HirTypeDecl {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    pub marker: ResourceMarker,
    /// Names of methods declared with a `consuming self` receiver in the
    /// type body. Lifted verbatim from `TypeDecl.consuming_methods`.
    pub consuming_methods: Vec<String>,
    pub fields: Vec<HirField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirField {
    pub name: String,
    pub ty: ResolvedTy,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFn {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<HirBinding>,
    pub return_ty: ResolvedTy,
    pub body: HirBlock,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirBinding {
    pub id: BindingId,
    pub name: String,
    pub ty: ResolvedTy,
    pub mutable: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirBlock {
    pub node: HirNodeId,
    pub scope: ScopeId,
    pub statements: Vec<HirStmt>,
    pub tail: Option<Box<HirExpr>>,
    pub ty: ResolvedTy,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirStmt {
    pub node: HirNodeId,
    pub kind: HirStmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStmtKind {
    Let(HirBinding, Option<HirExpr>),
    Expr(HirExpr),
    Return(Option<HirExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirExpr {
    pub node: HirNodeId,
    pub site: SiteId,
    pub ty: ResolvedTy,
    pub value_class: ValueClass,
    pub intent: IntentKind,
    pub kind: HirExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExprKind {
    Literal(HirLiteral),
    BindingRef {
        name: String,
        resolved: ResolvedRef,
    },
    Binary {
        op: BinaryOp,
        left: Box<HirExpr>,
        right: Box<HirExpr>,
    },
    Call {
        callee: Box<HirExpr>,
        args: Vec<HirExpr>,
    },
    Block(HirBlock),
    If {
        condition: Box<HirExpr>,
        then_expr: Box<HirExpr>,
        else_expr: Option<Box<HirExpr>>,
    },
    StructInit {
        name: String,
        type_args: Vec<ResolvedTy>,
        fields: Vec<(String, HirExpr)>,
    },
    /// A `fork { stmts }` block. Every statement-call inside the body is a
    /// child-task spawn (TI-1). Named bindings (`fork name = call(...)`)
    /// produce `Ty::Task(call_ret)` typed bindings (TI-2). The block joins
    /// all anonymous children implicitly at block exit.
    Fork {
        body: HirBlock,
    },
    /// A call expression that is recognised as a child-task spawn because it
    /// appears as a statement-expression inside a `fork {}` body. The callee
    /// and args are the same as `HirExprKind::Call`; the distinct kind routes
    /// MIR lowering to the task-spawn ABI rather than a direct synchronous
    /// call.
    ///
    /// `task_ty` is always `ResolvedTy::Task(call_return_ty)`. It duplicates
    /// the `HirExpr::ty` field for convenience at codegen sites that pattern-
    /// match on the kind without reaching back to the parent `HirExpr`.
    SpawnedCall {
        callee: Box<HirExpr>,
        args: Vec<HirExpr>,
        task_ty: ResolvedTy,
    },
    /// `await name` consumes a `Task<T>` binding and produces `T`. Legal
    /// positions in v0.5: statement-position inside a `fork{}` body. Future
    /// versions extend this to select-arm source expressions (cluster-5).
    ///
    /// `output_ty` is the inner `T` extracted from the binding's
    /// `ResolvedTy::Task(T)`. Stored here so codegen can emit the result slot
    /// without re-inspecting the binding's type.
    AwaitTask {
        /// The name of the task-handle binding being consumed.
        binding_name: String,
        /// The resolved binding id of the task handle.
        binding_id: BindingId,
        /// The `T` from `Task<T>` — the type produced by this await.
        output_ty: ResolvedTy,
    },
    /// Sealed `select{}` expression.
    ///
    /// The HIR shape carries the per-arm sealed-form discriminator and
    /// the per-arm body. The construct's static type (`HirExpr::ty`) is
    /// the unique common arm-body type — disagreement is rejected at
    /// lowering with `SelectArmTypeMismatch`.
    ///
    /// The four arm forms are exhaustive per HEW-SPEC-2026 §4.11.1; any
    /// other surface shape is rejected with `SelectArmNotSealedForm`
    /// during lowering.
    Select(HirSelect),
    Unsupported(String),
}

/// Lowered `select{}` expression. See `HirSelectArmKind` for the four
/// sealed arm forms.
#[derive(Debug, Clone, PartialEq)]
pub struct HirSelect {
    pub arms: Vec<HirSelectArm>,
}

/// One arm of a sealed `select{}` expression. `binding_name` is `None`
/// for `AfterTimer` arms (timer arms produce no value) and `Some` for
/// the three value-bearing forms. The `body` runs only when this arm
/// wins.
#[derive(Debug, Clone, PartialEq)]
pub struct HirSelectArm {
    pub kind: HirSelectArmKind,
    pub binding_name: Option<String>,
    pub body: HirExpr,
}

/// The four sealed arm forms recognised by HIR lowering. The variants
/// are intentionally minimal — they carry the discriminator plus the
/// expression slots a future MIR / codegen pass needs to know about.
/// The full runtime contracts for each form are documented at the
/// codegen fail-closed match arms (semantic-invariant TODO markers).
#[derive(Debug, Clone, PartialEq)]
pub enum HirSelectArmKind {
    /// `pat from next(<stream-expr>) => body`. The arm waits for a
    /// pending item on `stream`; the binding receives `Option<T>` where
    /// `None` is the EOF-wins signal.
    StreamNext { stream: Box<HirExpr> },
    /// `pat from <actor-expr>.<method>(<args>) => body`. The arm
    /// dispatches an ask to `actor.method(args)` and waits for the
    /// reply; the binding receives the reply value.
    ActorAsk {
        actor: Box<HirExpr>,
        method: String,
        args: Vec<HirExpr>,
    },
    /// `pat from await <task-expr> => body`. The arm waits for `task`
    /// to complete with `Ok(T)`; the binding receives `T`.
    /// Cancellation and trap outcomes propagate through the `select`
    /// site per HEW-SPEC-2026 §4.11.1.
    TaskAwait { task: Box<HirExpr> },
    /// `after <duration-expr> => body`. The arm fires when the
    /// deadline elapses; the body runs with no binding.
    AfterTimer { duration: Box<HirExpr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirLiteral {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    Duration(i64),
    Unit,
}
