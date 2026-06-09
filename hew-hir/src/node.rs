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
    Machine(HirMachineDecl),
}

// ── Machine declarations ─────────────────────────────────────────────────────

/// Lowered machine declaration. Carries the full structural shape needed for
/// static checks and visualisation; transition bodies are not lowered to `HirExpr`
/// in Lane A (codegen/execution is Lane B).
#[derive(Debug, Clone, PartialEq)]
pub struct HirMachineDecl {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    pub states: Vec<HirMachineState>,
    pub events: Vec<HirMachineEvent>,
    pub transitions: Vec<HirMachineTransition>,
    /// Whether an unhandled-event `default` arm is present. When true,
    /// exhaustiveness checking is satisfied for any `(state, event)` pair
    /// without an explicit transition.
    pub has_default: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirMachineState {
    pub name: String,
    pub fields: Vec<HirField>,
    /// Whether this state has an `entry { ... }` lifecycle block.
    pub has_entry: bool,
    /// Whether this state has an `exit { ... }` lifecycle block.
    pub has_exit: bool,
    /// Field names written by the `entry` block, each paired with the span of
    /// the specific `self.field = ...` assignment (used for effect-parity
    /// diagnostics that need to cite the offending entry-block site, not the
    /// whole state).
    pub entry_writes: Vec<(String, Span)>,
    /// Field names written by the `exit` block, each paired with the span of
    /// the specific `self.field = ...` assignment.
    pub exit_writes: Vec<(String, Span)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirMachineEvent {
    pub name: String,
    pub fields: Vec<HirField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirMachineTransition {
    pub event_name: String,
    pub source_state: String,
    pub target_state: String,
    pub has_guard: bool,
    /// True when `source_state == target_state` (self-transition). In a
    /// Moore machine, self-transitions do not re-run entry/exit.
    pub is_self_transition: bool,
    /// True when the transition carries `@reenter`.  Only meaningful for self-
    /// transitions; HIR rejects `@reenter` on non-self-transitions.  When true,
    /// the Lane B codegen must fire `source.exit` and `target.entry` even though
    /// the state identity does not change.
    pub reenter: bool,
    /// Field names written by the transition body (used for effect-parity checking).
    pub body_writes: Vec<String>,
    /// Event names emitted directly from the transition body (used for emit-cycle checking).
    pub body_emits: Vec<String>,
    pub span: Span,
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
    /// A `scope { stmts }` block. Every statement-call inside the body is a
    /// child-task spawn (TI-1). Named bindings (`fork name = call(...)`)
    /// produce `Ty::Task(call_ret)` typed bindings (TI-2). The block joins
    /// all anonymous children implicitly at block exit. `scope` is the
    /// structured-concurrency lifetime boundary; `fork` is the child-start verb.
    Scope {
        body: HirBlock,
    },
    /// A call expression that is recognised as a child-task spawn because it
    /// appears as a statement-expression inside a `scope {}` body. The callee
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
    /// positions in v0.5: statement-position inside a `scope{}` body. Future
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
    /// `actor |params| { body }` — a lambda-actor literal. Produces a
    /// `Duplex<Msg, Reply>` handle at runtime that addresses the
    /// spawned actor's message queue (the surface call syntax dispatches
    /// through this Duplex). The HIR shape carries the parameter
    /// bindings, the optional reply-type annotation, the lambda body,
    /// and the resolved capture set with per-capture strength
    /// (Strong vs Weak).
    ///
    /// Capture-strength discipline (§5.9 ratification 2): each free
    /// variable in the body that resolves to a binding from the
    /// enclosing scope is recorded as a capture. When the captured
    /// binding is the lambda's own let-binding name (the forward-bind
    /// recursive case `let fib = actor |n| { fib(n - 1) }`), the
    /// capture is `Weak` — the body must NOT keep the actor alive
    /// past external refcount zero. Every other capture is `Strong`.
    /// MIR's `LambdaCapture` side-table is populated directly from
    /// this list at lowering.
    SpawnLambdaActor {
        params: Vec<HirBinding>,
        reply_ty: ResolvedTy,
        body: Box<HirExpr>,
        captures: Vec<HirLambdaCapture>,
    },
    /// Project one element out of a tuple value: `expr.<index>`.
    ///
    /// Produced exclusively by tuple-let lowering (`let (a, b) = …`) — not a
    /// surface syntax node.  `index` is the zero-based element position.
    TupleIndex {
        /// The tuple expression being projected.
        tuple: Box<HirExpr>,
        /// Zero-based element index.
        index: usize,
    },
    Unsupported(String),
}

/// One captured binding inside an `HirExprKind::SpawnLambdaActor`
/// body. The `kind` discriminator selects the runtime refcount
/// strength: `Strong` bumps the captured value's refcount so the
/// actor body keeps the captured handle alive; `Weak` is the
/// self-binding-name recursive case (§5.9 ratification 2) and does
/// NOT bump the refcount.
///
/// The HIR `captures` field is the producer for the MIR
/// `LambdaCapture` side-table; the structural fail-closed checker in
/// `hew-mir` rejects malformed shapes (Weak on non-LambdaActorHandle,
/// multiple Weak captures on the same handle) that would otherwise
/// reach codegen.
#[derive(Debug, Clone, PartialEq)]
pub struct HirLambdaCapture {
    /// The captured binding's id in the enclosing scope.
    pub binding: BindingId,
    /// The captured binding's source name. Load-bearing for the
    /// self-binding case — a `Weak` capture's name matches the
    /// lambda's own let-binding name.
    pub name: String,
    /// Capture-strength discriminator.
    pub kind: HirCaptureKind,
}

/// Capture-strength selector for an `HirLambdaCapture`. Mirrors
/// the MIR-layer `CaptureKind` shape — kept HIR-local so the HIR
/// crate doesn't depend on `hew-mir`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirCaptureKind {
    /// Strong capture: bumps the captured value's refcount. Default
    /// for every non-self capture.
    Strong,
    /// Weak capture: does NOT bump the captured value's refcount.
    /// Reserved for the lambda's own let-binding name (the
    /// forward-bind recursive case, §5.9 ratification 2).
    Weak,
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
