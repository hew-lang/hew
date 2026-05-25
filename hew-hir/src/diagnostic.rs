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
    /// The `construct` names the unsupported form; `owning_pass` says which
    /// slice is planned to add support.  Fail-closed: the driver must stop on
    /// the first `NotYetImplemented` diagnostic.
    NotYetImplemented {
        construct: String,
        owning_pass: String,
    },
    UnresolvedSymbol {
        name: String,
    },
    ImportMissing {
        module: String,
        name: String,
    },
    UnresolvedBuiltinOverload {
        name: String,
        arg_ty: ResolvedTy,
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
    /// An `impl` block whose shape the V0b lowering arm does not yet handle
    /// (e.g. impl on a builtin nominal, where-clauses, default methods,
    /// blanket impls). Fail-closed per the mission tenet: surface a named
    /// diagnostic rather than dropping the block via the generic
    /// `NotYetImplemented` catch-all.
    ImplBlockShapeNotLowered {
        /// A short descriptor of the unsupported impl shape — used in the
        /// rendered diagnostic, e.g. `"impl on builtin nominal `Vec`"` or
        /// `"impl with where-clause"`.
        shape: String,
    },
    /// A method-call expression whose receiver typed as `Ty::TraitObject`
    /// has no entry in `TypeCheckOutput.dyn_trait_method_calls` for its
    /// span. Fail-closed per `checker-output-boundary`: HIR lowering
    /// never re-derives the trait/slot resolution. Surfaces compile-time,
    /// not as a runtime panic.
    TraitObjectMethodNoSideTableEntry {
        /// The method name the user wrote.
        method: String,
    },
    /// A `T → dyn Trait` coercion site has no entry in
    /// `TypeCheckOutput.dyn_trait_coercions` for the argument expression
    /// span — yet the destination type is `Ty::TraitObject`. Fail-closed
    /// per `checker-output-boundary`: HIR/MIR never construct a fat
    /// pointer without checker-authority resolution of the impl-fn keys.
    TraitObjectCoercionMissing {
        /// Trait name targeted by the coercion (e.g. `"Display"`).
        trait_name: String,
    },
    /// A receive handler has no checker-produced actor-state guard fact.
    /// HIR lowering consumes the checker contract fail-closed rather than
    /// inferring lock requirements from syntax downstream.
    ActorStateGuardMissing {
        /// Receive handler name.
        handler: String,
    },
    /// A checker-owned `expr_types` entry failed the `ResolvedTy::from_ty`
    /// boundary conversion.  This means the checker left an unresolved
    /// inference variable, a `Ty::Error` placeholder, or an unmaterialized
    /// numeric literal in the side-table — all of which are fail-closed per
    /// the `checker-output-boundary` invariant.  Never silently substitute
    /// `Unit`; surface this diagnostic instead so the error is visible.
    CheckerBoundaryViolation {
        /// The expression name or description at the failing site (e.g. the
        /// callee name for a call expression).
        name: String,
        /// Human-readable description of the `BoundaryError` discriminant.
        reason: String,
    },
    /// A generic-function call's recorded `call_type_args` failed the
    /// `ResolvedTy::from_ty` boundary conversion. Same shape as
    /// `CheckerBoundaryViolation` but specific to the monomorphisation
    /// side-table so the diagnostic message can point at the registry
    /// path. Fail-closed per `checker-authority` (P0) — never silently
    /// drop a poisoned `call_type_args` entry.
    MonomorphisationCallTypeArgsViolation {
        /// Callee name at the offending site.
        callee: String,
        /// Human-readable description of the `BoundaryError`.
        reason: String,
    },
    /// HIR-lowering produced more distinct generic-function
    /// monomorphisations than the configured cap admits. Almost always
    /// the symptom of polymorphic recursion that — once G-1.b's
    /// substitution lands — would expand without bound. G-1.a surfaces
    /// the cap at the registry seam so downstream stages don't OOM.
    MonomorphisationCapExceeded {
        /// The configured cap (typically
        /// `MONOMORPHISATION_REGISTRY_CAP`).
        cap: usize,
    },
    /// A generic record-init site's recorded `record_init_type_args`
    /// failed the `ResolvedTy::from_ty` boundary conversion. Same shape
    /// as `MonomorphisationCallTypeArgsViolation` but for the record-
    /// layout registry. Fail-closed per `checker-authority` (P0).
    RecordLayoutTypeArgsViolation {
        /// Record name at the offending site.
        record: String,
        /// Human-readable description of the `BoundaryError`.
        reason: String,
    },
    /// HIR lowering produced more distinct record-layout
    /// monomorphisations than the configured cap admits. Almost always
    /// the symptom of polymorphic-recursive record types whose layout
    /// expansion would not converge (each layer introduces a fresh
    /// arg). Surfaced at the registry seam so codegen never sees an
    /// unbounded layout set.
    RecordLayoutCapExceeded {
        /// The configured cap (typically
        /// `MONOMORPHISATION_REGISTRY_CAP`, shared with the fn
        /// registry).
        cap: usize,
    },
    /// A generic record init site has no `record_init_type_args` entry
    /// despite the checker having accepted the expression (the span
    /// appears in `expr_types`).  This signals a missed re-record path
    /// in the checker: the type arguments exist at the source level but
    /// were never written to the side-table.  Fail-closed:
    /// `unwrap_or_default()` on the missing layout would silently
    /// produce `Named { args: [] }` — an under-instantiated shape that
    /// downstream MIR/codegen would treat as monomorphic.
    RecordLayoutMissing {
        /// The user-declared record name at the offending init site.
        record: String,
    },
    /// A generic record's substituted field shape mentions the same
    /// origin record with *different* concrete type arguments — e.g.
    /// `pub type Node<T> { next: Box<Node<int>> }` instantiated at
    /// `T = string` would force a `Node<int>` layout, which in turn
    /// would force a `Node<int>` re-expansion, never converging unless
    /// `int = string`. Recursive *same-arg* self-reference is fine
    /// (it's an ordinary recursive type and converges to one layout).
    /// Deferred to v0.6.
    RecursiveGenericTypeUnsupported {
        /// Origin record name.
        name: String,
    },
    /// HIR lowering produced more distinct enum-layout monomorphisations
    /// than the configured cap admits. Almost always the symptom of
    /// polymorphic-recursive generic enum types whose layout expansion
    /// would not converge (each layer introduces a fresh arg). Surfaced
    /// at the registry seam so codegen never sees an unbounded layout set.
    EnumLayoutCapExceeded {
        /// The configured cap (typically `MONOMORPHISATION_REGISTRY_CAP`,
        /// shared with the fn and record registries).
        cap: usize,
    },
    /// A function declared with `#[intrinsic("key")]` names an intrinsic
    /// key that does not appear in `stdlib_catalog`. Fail-closed: the
    /// compiler never silently drops a typed declaration — if the key is
    /// wrong the author must fix the declaration or update the catalog.
    UnknownIntrinsic {
        /// The function name that carried the attribute.
        fn_name: String,
        /// The intrinsic key that was not found in the catalog.
        intrinsic_key: String,
    },
    /// An imported `impl` block's method body calls a private helper function
    /// defined in the same module as the impl. Private functions are not
    /// exported across module boundaries, so the body cannot be lowered.
    /// The dependency chain must be resolved (e.g. by making the helper `pub`,
    /// moving the logic inline, or restructuring the module) before cross-module
    /// dispatch on this method will work.
    ///
    /// Emitted instead of a bare `UnresolvedSymbol` so the diagnostic
    /// identifies the root cause at the module boundary.
    ImportedImplBodyMissingPrivateHelper {
        /// Short name of the module that owns the impl block (e.g. `"shapes"`).
        module: String,
        /// Name of the private function that the method body calls.
        helper_fn: String,
    },
    /// Target architecture does not support coroutines (actors/tasks). The
    /// program uses a coroutine-dependent construct (actor decl, task spawn,
    /// fork, async block, await) but the target arch is not in the set
    /// `{x86_64, aarch64}` where `coro_switch`/`coro_init` are implemented.
    /// Fail-closed per slepp A222: compile-time diagnostic instead of
    /// runtime panic at `hew-runtime/src/coro.rs:391` or `:492`.
    TargetCoroutineUnsupported {
        /// Target architecture name (e.g. `"riscv64"`).
        target_arch: String,
        /// User-visible construct name (e.g. `"actor decl"`, `"task spawn"`).
        construct: String,
    },
    /// Blocking channel recv is not supported on wasm32. The program calls
    /// `.recv()` on a channel (or the builtin `hew_channel_recv*`) with target
    /// `wasm32`, which would reach `unreachable!` at
    /// `hew-runtime/src/lib.rs:378` or `:391`. Fail-closed per slepp A222:
    /// compile-time diagnostic instead of runtime panic. Non-blocking
    /// `.try_recv()` is allowed on wasm32.
    BlockingChannelRecvUnsupportedOnWasm {
        /// User-visible construct name (e.g. `".recv()"`).
        construct: String,
    },
}
