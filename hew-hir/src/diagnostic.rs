use hew_parser::ast::Span;
use hew_types::ResolvedTy;

use crate::ids::{BindingId, HirNodeId, ResolvedRef, SiteId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirDiagnostic {
    pub kind: HirDiagnosticKind,
    pub span: Span,
    pub note: String,
    /// Additional source locations referenced by this diagnostic.
    /// Each entry is `(span, label)` — e.g. the entry/exit block site for
    /// effect-parity diagnostics.  Empty for most diagnostics.
    pub secondary_spans: Vec<(Span, String)>,
}

impl HirDiagnostic {
    #[must_use]
    pub fn new(kind: HirDiagnosticKind, span: Span, note: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            note: note.into(),
            secondary_spans: Vec::new(),
        }
    }

    /// Attach secondary spans and return `self` (builder pattern).
    #[must_use]
    pub fn with_secondary_spans(mut self, spans: Vec<(Span, String)>) -> Self {
        self.secondary_spans = spans;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirDiagnosticKind {
    /// A syntactic construct that is outside the current vertical slice.
    /// The `construct` names the unsupported form; `slice_target` says which
    /// slice is planned to add support.  Fail-closed: the driver must stop on
    /// the first `CutoverUnsupported` diagnostic.
    CutoverUnsupported {
        construct: String,
        slice_target: String,
    },
    UnresolvedSymbol {
        name: String,
    },
    UnresolvedInferenceVar,
    DuplicateBindingId {
        id: BindingId,
    },
    DuplicateSiteId {
        id: SiteId,
    },
    DuplicateNodeId {
        id: HirNodeId,
    },
    DanglingRef {
        resolved: ResolvedRef,
    },
    ReturnTypeMismatch {
        expected: ResolvedTy,
        actual: ResolvedTy,
    },
    /// `#[resource]` type body has no method named `close` declared with
    /// `consuming self`. The implicit-drop contract for `@resource` types
    /// requires this method; missing it is a fail-closed compile error.
    ResourceMissingClose {
        name: String,
    },
    /// `#[linear]` type body declares zero `consuming self` methods. A
    /// linear type with no consuming methods cannot be exhausted on an
    /// exit path, which would make `MirCheck::MustConsume` unable to fire
    /// — caught here as a structural error.
    LinearNoConsumingMethods {
        name: String,
    },
    /// `#[resource]` or `#[linear]` type carries type parameters. Generic
    /// resource/linear types are out of scope for the v0.5 vertical slice
    /// (the marker→ValueClass mapping is keyed by type name, not by
    /// instantiated type).
    ResourceGenericUnsupported {
        name: String,
    },
    /// `await` appeared outside a `scope{}` body or `select` arm. In v0.5,
    /// `await` is a statement-only form inside `scope{}` bodies. Future
    /// versions additionally permit it inside `select` arm source expressions.
    AwaitOutOfPosition,
    /// `await` was applied to an expression that does not have type
    /// `Task<T>`. Only task handles produced by `fork name = call(...)` are
    /// awaitable.
    AwaitNonTask {
        found_ty: ResolvedTy,
    },
    /// The right-hand side of a `fork name = expr` binding was not a call
    /// expression. Per the specification, the child form requires a call so
    /// the compiler can spawn a task.
    ForkChildNotACall,
    /// The user wrote `Task<T>` as a type annotation in source code. In v0.5,
    /// `Task<T>` is a compiler-internal value class with no surface syntax;
    /// use `fork name = call(...)` to create a task handle instead.
    TaskNotNameable,
    /// A `Task<T>` handle (inferred or explicit) was used in a `return`
    /// expression, causing it to escape the `scope{}` body. Task handles are
    /// scope-body-scoped; await them inside the body with `await name`.
    TaskCannotEscape,
    /// A `select` arm's source expression is not one of the four sealed
    /// forms (`next(stream)`, `actor.method(args)`, `await task`, or the
    /// timer arm `after duration`). Per HEW-SPEC-2026 §4.11.1 the four
    /// forms are exhaustive; arbitrary expressions are rejected.
    SelectArmNotSealedForm {
        /// Short description of the offending source shape, e.g.
        /// `"literal"`, `"binary expression"`, `"identifier"`. The shape
        /// is recorded for the diagnostic note; no structural data is
        /// preserved from the rejected expression.
        source_shape: String,
    },
    /// Two or more arm body expressions disagree on type. The `select`
    /// expression's static type is the unique common arm-body type; if
    /// arms disagree the construct is rejected (per HEW-SPEC-2026
    /// §4.11.1 "All arm result expressions must have the same type T").
    SelectArmTypeMismatch {
        arm_index: usize,
        expected: ResolvedTy,
        actual: ResolvedTy,
    },
    /// A `select` expression contains two or more `after` arms. The
    /// timer arm is degenerate (no source binding, single deadline) and
    /// is permitted at most once; multiple deadlines would have no
    /// meaningful join semantics.
    SelectMultipleAfterArms,
    /// A `select` expression contains zero arms (neither sealed arms
    /// nor an `after` timer). An empty select cannot fire and is
    /// rejected at the surface.
    SelectNoArms,
    /// A `select` arm names a `next(...)` call with an arity other
    /// than one. The sealed stream form is `next(<stream-expr>)` —
    /// exactly one argument.
    SelectStreamNextArity {
        arg_count: usize,
    },

    // ── Machine static checks ────────────────────────────────────────────
    /// One or more `(state, event)` pairs have no matching transition and
    /// the machine does not declare a `default` arm.
    MachineExhaustivenessViolation {
        machine_name: String,
        missing: Vec<(String, String)>,
    },
    /// A self-transition (`source == target`) has a non-empty body but is not
    /// annotated `@reenter`.  Moore-style self-loops must be empty; Mealy-style
    /// re-entry requires the explicit `@reenter` annotation so the compiler can
    /// enforce that `entry`/`exit` run correctly.
    MachineSelfTransitionNeedsReenter {
        machine_name: String,
        state_name: String,
        event_name: String,
    },
    /// A transition body writes a field that is also written by the target
    /// state's `entry` block or the source state's `exit` block, creating
    /// ambiguous initialization/teardown order.  `secondary_spans` points at
    /// the conflicting entry/exit site.
    MachineEffectParityViolation {
        machine_name: String,
        /// The state whose `entry` or `exit` block conflicts.
        state_name: String,
        field_name: String,
        transition_event: String,
        /// Whether the conflict is with the target `entry` block (`true`) or
        /// the source `exit` block (`false`).
        is_entry_conflict: bool,
    },
    /// A direct `emit(E)` cycle was detected: a transition's `on E` arm
    /// contains `emit E`, which would immediately re-trigger the same handler.
    MachineEmitCycle {
        machine_name: String,
        event_name: String,
    },
    /// A method call expression has no entry in `TypeCheckOutput.method_call_rewrites`
    /// for its span.  Fail-closed per the `checker-output-boundary` invariant:
    /// HIR lowering never re-infers a runtime symbol from the receiver type; every
    /// lowerable method call must be backed by a checker-produced rewrite entry.
    MethodCallNoRewrite {
        /// The method name the user wrote (e.g. `send`).
        method: String,
    },
}
