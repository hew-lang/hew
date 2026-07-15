use hew_parser::ast::Span;
use hew_types::ResolvedTy;

use crate::ids::{BindingId, HirNodeId, ResolvedRef, SiteId};

/// Which kind of imported item carried a body that failed to lower because
/// of a missing same-module dependency. Used by
/// [`HirDiagnosticKind::ImportedBodyMissingPrivateHelper`] so consumers can
/// distinguish imported impl-method bodies from imported free-function
/// bodies without splitting the diagnostic variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportedItemKind {
    /// The body came from an imported `impl` block's method.
    ImplMethod,
    /// The body came from an imported `pub fn` free function.
    FreeFn,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirDiagnostic {
    pub kind: HirDiagnosticKind,
    pub span: Span,
    pub note: String,
    /// Dotted source module that owns this diagnostic's span.
    ///
    /// Matches `TypeError::source_module`: `None` means the root source file,
    /// `Some("a.b")` means the non-root module with that dotted `ModuleId`.
    pub source_module: Option<String>,
    /// Additional source locations referenced by this diagnostic.
    /// Each entry is `(span, label)` — e.g. the entry/exit block site for
    /// effect-parity diagnostics. Empty for most diagnostics. Secondary
    /// spans currently share the primary diagnostic's `source_module`.
    pub secondary_spans: Vec<(Span, String)>,
}

impl HirDiagnostic {
    #[must_use]
    pub fn new(kind: HirDiagnosticKind, span: Span, note: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            note: note.into(),
            source_module: None,
            secondary_spans: Vec::new(),
        }
    }

    /// Attach a source-module attribution and return `self` (builder pattern).
    #[must_use]
    pub fn with_source_module(mut self, source_module: Option<String>) -> Self {
        self.source_module = source_module;
        self
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
    /// A tuple pattern has a different number of elements than its value.
    TuplePatternArityMismatch {
        expected: usize,
        actual: usize,
    },
    /// A tuple pattern was applied to a value that is not a tuple.
    TuplePatternNonTupleValue,
    /// An enum variant constructor was written with syntax for a different
    /// variant payload shape.
    EnumVariantConstructorShapeMismatch {
        variant: String,
    },
    /// An enum struct-variant constructor omitted a declared field.
    EnumVariantConstructorMissingField {
        variant: String,
        field: String,
    },
    /// An enum struct-variant constructor named a field it does not declare.
    EnumVariantConstructorUnknownField {
        variant: String,
        field: String,
    },
    /// An enum tuple-variant constructor received the wrong number of arguments.
    EnumVariantConstructorArityMismatch {
        variant: String,
        expected: usize,
        actual: usize,
    },
    /// `#[resource]` type body has no method named `close` declared with
    /// `consuming self`. The implicit-drop contract for `@resource` types
    /// requires this method; missing it is a fail-closed compile error.
    ResourceMissingClose {
        name: String,
    },
    /// A `#[resource]`/`#[linear]` value parameter appears at a boundary whose
    /// body is invisible to the caller — an `extern` fn (no body; a foreign C
    /// ABI) or a bodyless trait method signature (a contract whose impls may
    /// diverge) — WITHOUT an explicit `consume` modifier. The borrow-vs-consume
    /// disposition of such a parameter cannot be inferred (there is no body to
    /// scan), so the caller cannot know whether to keep ownership and auto-drop
    /// at the caller scope, or transfer ownership so the callee/ABI owns and
    /// drops it. Guessing either way is a real drop-safety bug: defaulting to
    /// borrow across a body that actually consumes double-drops; defaulting to
    /// move-in across a body that only borrows leaks. Fail closed: an affine
    /// resource parameter at an invisible-body boundary must be declared
    /// `consume` (pinning the by-move transfer), or restructured to pass an
    /// owned handle/id by value instead of the resource itself.
    ///
    /// A receiver (`self`, or a `Self`-typed receiver) is never flagged: it is
    /// not a concrete resource nominal, and its disposition rides the method
    /// dispatch surface (`consuming self`), not borrow-pass.
    ResourceBoundaryParamMustConsume {
        /// Enclosing extern fn / trait method name.
        func: String,
        /// The offending parameter name.
        param: String,
        /// Resource type name of the offending parameter.
        ty: String,
        /// Boundary kind: `"extern fn"` or `"trait method signature"`.
        boundary: String,
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
    /// `#[resource]` type declares its `close` method inline inside the
    /// type body (`TypeBodyItem::Method`). In v0.5 the supported surface
    /// for user-resource `close` lowering is an **inherent impl block**
    /// (`impl T { fn close(self) { ... } }`). The inline form is a silent
    /// trap today — the method body is not lowered to HIR/MIR, the drop
    /// elaborator still emits `drop_fn: Some("T::close")`, and codegen
    /// would resolve to a missing symbol at link time. Fail-closing here
    /// at the HIR boundary forces users onto the supported surface; the
    /// inline form may be admitted in a future surface extension.
    /// (W3.030 Q-α-B ratification.)
    ResourceCloseSourceUnsupported {
        /// Type name carrying the inline `close` method.
        name: String,
    },
    /// `#[linear]` type declares its `consuming self` method inline in the
    /// type body (`type T { fn m(consuming self) { … } }`). The supported
    /// surface that lowers to a callable consume target is a sibling inherent
    /// impl block (`impl T { fn m(consuming self) { … } }`). The inline form
    /// is not lowered to HIR/MIR, so a call to it raises
    /// `IndirectCallUnsupported` at the call site — making the `#[linear]`
    /// type unusable. Fail-close here at the declaration with a directive to
    /// the supported surface instead of trapping the user at every call.
    LinearConsumingMethodSourceUnsupported {
        /// Type name carrying the inline consuming method.
        name: String,
    },
    /// `#[resource]` type's `close` method declares a return type other
    /// than unit. In v0.5 the implicit-drop contract dispatches `close`
    /// on every scope-exit path (return, early-return, branch, trap,
    /// cancel, actor exit) — propagating an error or value from that
    /// dispatch has no defined semantics on a non-return exit. Users
    /// who need fallible cleanup compose it through `defer` instead.
    /// (W3.030 Q-β-C ratification.)
    ResourceCloseMustReturnUnit {
        /// Type name whose `close` returns non-unit.
        name: String,
        /// User-facing rendering of the offending return type.
        return_ty: String,
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
    /// A `select` arm's source expression is not one of the three sealed
    /// forms (`actor.method(args)`, a channel `rx.recv()`, or the timer
    /// arm `after duration`). Per HEW-SPEC-2026 §4.11.1 the three forms
    /// are exhaustive; arbitrary expressions are rejected. (Earlier drafts
    /// also listed `next(stream)` and `await task`; those arms are deferred
    /// with their substrate — see HEW-FUTURE §1.4.)
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
    /// A `join { ... }` branch's source expression is not an actor
    /// method call (`<actor-expr>.<method>(<args>)`). Per HEW-SPEC-2026
    /// §4.11.2 every join branch must be an actor receive-handler call
    /// with a return type; arbitrary expressions are rejected.
    JoinBranchNotActorAsk {
        /// Short description of the offending source shape, e.g.
        /// `"literal"`, `"binary expression"`, `"identifier"`.
        source_shape: String,
    },
    /// A `join { }` expression contains zero branches. An empty join
    /// produces no values and is rejected at the surface.
    JoinNoBranches,

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
    /// A transition body `emit`s an event that is not listed in the machine's
    /// `emits { … }` Mealy-output manifest. When the manifest is present it is
    /// an exhaustive allowlist of permitted outputs; emitting anything else is
    /// rejected so the declared output vocabulary stays auditable.
    MachineEmitNotInManifest {
        machine_name: String,
        event_name: String,
    },
    /// A transition body reads `event.<field>` but `<field>` is not declared
    /// on the triggering event type. This is a user type error — the field
    /// does not exist — not a compiler limitation. The available field names
    /// for the event are included in the note so the user can correct the
    /// access without inspecting the machine declaration.
    MachineEventFieldNotFound {
        /// The machine name (e.g. `"TrafficLight"`).
        machine_name: String,
        /// The event type name (e.g. `"TrafficLightEvent"`).
        event_name: String,
        /// The field name the user wrote (e.g. `"wrong_field"`).
        field_name: String,
        /// Fields actually declared on this event (empty if the event
        /// has no payload fields).
        available_fields: Vec<String>,
    },
    /// A method call expression has no entry in `TypeCheckOutput.method_call_rewrites`
    /// AND no entry in `TypeCheckOutput.resolved_calls` for its span. Fail-closed
    /// per the `checker-output-boundary` invariant: HIR lowering never re-infers
    /// a runtime symbol from the receiver type; every lowerable method call must
    /// be backed by a checker-produced rewrite entry (legacy path) or a
    /// `ResolvedImplCall` (Stage C resolver-authority path, e.g. `HashMap` /
    /// `HashSet` after the W4.001 DI-017 cutover).
    ///
    /// Post-Stage-C3 this diagnostic should be a boundary-violation signal only:
    /// user-reachable `HashMap`/`HashSet` mistakes now surface as resolver-side
    /// `TypeErrorKind::BoundsNotSatisfied` BEFORE lowering. Reaching this arm in
    /// production traffic indicates the checker / HIR boundary contract was
    /// broken — a regression worth investigating, not a missing user feature.
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
    /// The dedicated `MachineMonoPass`
    /// ([`crate::machine_mono::run_machine_mono_pass`]) observed a
    /// machine type whose substituted type args still carry a residual
    /// type-parameter symbol (`ResolvedTy::Named { name, args: [] }`
    /// where `name` is the name of a generic type-parameter declared
    /// somewhere upstream in the program). After function-mono has
    /// completed, every machine instantiation reachable from a
    /// monomorphic entry point must be fully concrete — a residual
    /// abstract symbol is a function-mono defect (the closure pass
    /// failed to reach this site) or a checker-side authority gap
    /// (the type wasn't recorded). Either way the machine-mono pass
    /// fails closed rather than silently propagating an
    /// under-instantiated layout to MIR/codegen.
    ///
    /// Per W3.033c Stage 2 (R244=B): this is the load-bearing
    /// invariant that justifies the dedicated-pass design — the
    /// observation that the pass runs after function-mono completes
    /// is what makes residual abstract symbols an unambiguous defect.
    UnresolvedMachineTypeParamPostMono {
        /// Origin machine name as written in source.
        machine: String,
        /// Human-readable rendering of the residual abstract symbol
        /// (e.g. `"T"`). Kept as a `String` to avoid leaking the full
        /// `ResolvedTy` shape into the diagnostic schema; the surrounding
        /// span pins the site for any deeper triage.
        residual_var: String,
    },
    /// The dedicated `MachineMonoPass` discovered more distinct
    /// machine instantiations than the configured monomorphisation cap
    /// admits. Mirrors `MonomorphisationCapExceeded` for the
    /// function-mono registry and `EnumLayoutCapExceeded` /
    /// `RecordLayoutCapExceeded` for the layout registries.
    MachineMonomorphisationCapExceeded {
        /// The configured cap (shared with the function-mono and layout
        /// registries — see [`crate::monomorph::MONOMORPHISATION_REGISTRY_CAP`]).
        cap: usize,
    },
    /// The dedicated layout-mono pass
    /// ([`crate::layout_mono::run_layout_mono_pass`]) observed a generic
    /// record or enum type whose substituted type arguments still carry a
    /// residual type-parameter symbol after function-mono completed. This is
    /// the record/enum analogue of
    /// [`Self::UnresolvedMachineTypeParamPostMono`]: once the function-mono
    /// closure has finished, every record/enum instantiation reachable from a
    /// monomorphic entry point must be fully concrete. A residual abstract
    /// symbol is a function-mono defect (the closure failed to reach this site
    /// with a concrete substitution map) or a checker-authority gap. The
    /// layout-mono pass fails closed rather than registering an
    /// under-instantiated layout that would leak `T` to MIR/codegen.
    UnresolvedLayoutTypeParamPostMono {
        /// Origin record/enum name as written in source.
        type_name: String,
        /// Human-readable rendering of the residual abstract symbol (e.g.
        /// `"T"`). Kept as a `String` so the diagnostic schema does not leak
        /// the full `ResolvedTy` shape; the surrounding span pins the site.
        residual_var: String,
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
    /// An imported item body (either an `impl` block's method body or a
    /// `pub fn` free-function body) calls a private helper function defined
    /// in the same module as the item. Private functions are not exported
    /// across module boundaries, so the body cannot be lowered. The
    /// dependency chain must be resolved (e.g. by making the helper `pub`,
    /// moving the logic inline, or restructuring the module) before
    /// cross-module dispatch on this item will work.
    ///
    /// Emitted instead of a bare `UnresolvedSymbol` so the diagnostic
    /// identifies the root cause at the module boundary.
    ImportedBodyMissingPrivateHelper {
        /// Short name of the module that owns the item (e.g. `"shapes"`).
        module: String,
        /// Name of the private function that the body calls.
        helper_fn: String,
        /// Which kind of imported item carried the body that referenced the
        /// missing helper. Lets diagnostic consumers tell impl-method gaps
        /// apart from free-function gaps without restoring a separate kind.
        item_kind: ImportedItemKind,
    },
    /// An imported `pub fn` free-function body calls another `pub fn` from
    /// the same imported module by its bare (unqualified) identifier. The
    /// bare name does not resolve in the importer scope — only the
    /// `module.callee` mangled spelling does — so the body cannot be
    /// lowered as-written. The author should rewrite the call as
    /// `module.callee(..)` (the `suggested_qualified` form) or wait for the
    /// full imported-body dependency closure (Stage 2 of W4.018) to land.
    ///
    /// Emitted instead of a bare `UnresolvedSymbol` so the diagnostic
    /// identifies the root cause at the module boundary and points at the
    /// repair form.
    ImportedFreeFnBodyUnresolvedBareCall {
        /// Short name of the module that owns the free-function body
        /// (e.g. `"path"`).
        module: String,
        /// The bare callee identifier that failed to resolve (e.g. `"basename"`).
        callee: String,
        /// The mangled qualified spelling the author should rewrite to
        /// (e.g. `"path.basename"`).
        suggested_qualified: String,
    },
    /// Target architecture does not support actors/supervisors. The program
    /// uses a coroutine-dependent construct (actor decl, supervisor decl) but
    /// the target arch is not in the set `{x86_64, aarch64}` that has the full
    /// actor runtime ABI (scheduler, mailbox, supervisor restart machinery).
    /// Suspension itself is target-agnostic LLVM `llvm.coro.*`; the gap is the
    /// surrounding actor runtime substrate (`hew_actor_*`, `hew_supervisor_*`,
    /// `hew_mailbox_*`) which is not yet ported to wasm32 or other targets.
    /// Fail-closed per slepp A222: compile-time diagnostic instead of a
    /// linker error or silent runtime failure.
    TargetCoroutineUnsupported {
        /// Target architecture name (e.g. `"riscv64"`).
        target_arch: String,
        /// User-visible construct name (e.g. `"actor decl"`, `"task spawn"`).
        construct: String,
    },
    /// Blocking channel recv is not supported on wasm32. The program calls
    /// `.recv()` on a channel (or the builtin `hew_channel_recv_layout`) with target
    /// `wasm32`, which would reach `unreachable!` at
    /// `hew-runtime/src/lib.rs:378` or `:391`. Fail-closed per slepp A222:
    /// compile-time diagnostic instead of runtime panic. Non-blocking
    /// `.try_recv()` is allowed on wasm32.
    BlockingChannelRecvUnsupportedOnWasm {
        /// User-visible construct name (e.g. `".recv()"`).
        construct: String,
    },
    /// Spawned task/fork callee has non-unit signature. The spawned
    /// function must have zero args and unit return type because task result
    /// propagation is not yet wired. Fail-closed per FC-P1-A1 audit.
    TaskSpawnSignatureUnsupported {
        /// Site identifier for error reporting.
        site: SiteId,
    },
    /// Spawned task/fork callee is not a direct module function or supported
    /// closure. The spawned expression must be a direct function call or
    /// zero-arg unit closure. Fail-closed per FC-P1-A1 audit.
    TaskSpawnCalleeUnsupported {
        /// Site identifier for error reporting.
        site: SiteId,
    },
    /// Spawned closure has params, args, or non-unit result. The spawned
    /// closure must be zero-arg and return unit because value/result task
    /// propagation remains fail-closed. Fail-closed per FC-P1-A1 audit.
    SpawnedClosureSignatureUnsupported {
        /// Site identifier for error reporting.
        site: SiteId,
    },
    /// Spawned closure captures non-Send value. All captured values must be
    /// Send for safe task migration. Fail-closed per FC-P1-A1 audit.
    SpawnedClosureNonSendCapture {
        /// Site identifier for error reporting.
        site: SiteId,
        /// Name of the non-Send captured value.
        capture_name: String,
    },
    /// Fork block body is not supported shape. The fork block must contain
    /// exactly one statement that is a direct function call with zero args
    /// and unit return. Fail-closed per FC-P1-A1 audit.
    ForkBlockBodyUnsupported {
        /// Site identifier for error reporting.
        site: SiteId,
        /// Human-readable reason for rejection (e.g. "empty", "multi-statement").
        reason: String,
    },
    /// Deadline has non-empty body. The `after(...)` deadline must have an
    /// empty body in v0.5; deadline body execution is not yet wired.
    /// Fail-closed per FC-P1-A1 audit.
    DeadlineBodyUnsupported {
        /// Site identifier for error reporting.
        site: SiteId,
    },
    /// Dead code: nested-supervisor field access (`root.sub`, including
    /// chained `root.sub.worker`) now lowers end-to-end through the
    /// `hew_supervisor_nested_get` ABI call and never constructs this
    /// variant (`check_supervisor_child_accessor_gates`'s `ChildKind::Static`
    /// arm no longer gates it). Retained because
    /// `LowerOutput::into_result`'s fatal-diagnostic match still lists it.
    NestedSupervisorAccessorUnsupported {
        /// Outer supervisor type name (e.g. `"RootSupervisor"`).
        supervisor: String,
        /// Nested-supervisor child name being accessed (e.g. `"sub"`).
        child: String,
        /// Inner supervisor type name (e.g. `"SubSupervisor"`).
        nested_supervisor: String,
    },
    /// A binary operator was used in value position but the MIR backend has
    /// no lowering for it. Currently covers `..` and `..=` range operators
    /// outside of `for` loops or slice indexing. Closed at HIR pre-pass per
    /// FC-P1-D (audit site `hew-mir/src/lower.rs:5336`) so users see a
    /// compile-time diagnostic instead of MIR's `NotYetImplemented`
    /// late-stage surprise. LESSONS `boundary-fail-closed`.
    BinaryOperatorUnsupportedInMir {
        /// Source-form operator (e.g. `".."`, `"..="`).
        op: String,
    },
    /// A unary operator reached HIR with checker-authoritative types that the
    /// MIR/codegen substrate does not support. This is a fail-closed boundary
    /// diagnostic rather than a fallback to `Unit` or a downstream panic.
    UnaryOperatorUnsupportedInMir {
        /// Source-form operator (`"!"`, `"-"`, or `"~"`).
        op: String,
        /// Checker-resolved operand type.
        operand_ty: String,
        /// Checker-resolved result type.
        result_ty: String,
    },
    /// Call expression resolves to an item that has no MIR body and no
    /// runtime-ABI lowering. Lifted from MIR `hew-mir/src/lower.rs:4194`
    /// per the FC-P1-B audit so the diagnostic surfaces during HIR lowering
    /// instead of after the MIR producer has already begun emitting
    /// instructions for the surrounding function.
    ///
    /// Fires when a `HirExprKind::Call` callee is
    /// `BindingRef { resolved: ResolvedRef::Item(_), name }` and `name`
    /// is not in the module's callable set
    /// (stdlib catalog + monomorphic + generic user functions + extern fns +
    /// monomorphisation mangled names + recognised runtime-ABI bridges
    /// such as `supervisor_stop` and `hew_duplex_*`). In practice this is
    /// defense-in-depth: the upstream fn-registry seeding usually keeps
    /// `Item`-resolved callees inside that set. A surface program that
    /// reaches this gate is one where the producer-bridge between checker
    /// and MIR has drifted.
    CallableUnsupportedInMir {
        /// The unresolved callee name as it appeared at the call site.
        name: String,
    },
    /// Call expression with an indirect / higher-order / unresolved callee
    /// whose static type is callable (`ResolvedTy::Function` or
    /// `ResolvedTy::Closure`) but for which no MIR dispatch path exists.
    /// Lifted from MIR `hew-mir/src/lower.rs:4236` per the FC-P1-B audit.
    ///
    /// Predicate (intentionally narrow to avoid blocking valid programs
    /// such as closure-binding invocations `let f = |x| x + 1; f(2)`):
    /// callee is `BindingRef { resolved: ResolvedRef::Unresolved, .. }`
    /// and `callee.ty` is `Function` / `Closure`. Valid closure-binding
    /// calls reach `BindingRef { resolved: Binding(_), .. }` and are
    /// lowered by MIR's `CallClosure` arm, so they do not trip this gate.
    IndirectCallUnsupported {
        /// User-visible callee description (e.g. `"unresolved binding `foo`"`).
        callee: String,
        /// Rendered callee type for the diagnostic message.
        callee_ty: String,
    },
    /// Supervisor spawn with init args is not supported. `spawn AppSupervisor(...)`
    /// reaches MIR lowering (`hew-mir/src/lower.rs:8852`) as a `NotYetImplemented`
    /// runtime-style diagnostic; raise it to a HIR fatal gate per slepp A222 so
    /// the failure surfaces at compile time with a clear cause. The checker
    /// already rejects supervisor declarations that take init params; this gate
    /// is defense-in-depth catching any future surface that could reach MIR
    /// before the checker guard does. Supervisors take their child specs
    /// declaratively — spawn-time init args have no defined semantics.
    SupervisorSpawnArgsUnsupported {
        /// Supervisor identifier as written at the spawn site.
        supervisor_name: String,
    },
    /// `Vec<T>` scalar index (`xs[i]`) with an element type that the runtime
    /// ABI does not (yet) implement a `hew_vec_get_T` for. Fail-closed per
    /// slepp A222 / A228: surface the unsupported case at compile time
    /// instead of letting MIR emit `NotYetImplemented` and codegen drop the
    /// expression. Supported scalar element types are `bool`, `char`, `i32`,
    /// `u32`, `i64`, `u64`, `f64`, `String` (retained/header-aware owner via
    /// `hew_vec_get_str`, balanced by the caller's scope-exit `hew_string_drop`),
    /// and any user-defined `Named` type (records, enums, `Duplex`,
    /// `LambdaActorHandle`, etc., dispatched via `hew_vec_get_ptr` /
    /// `hew_vec_get_layout` / `hew_vec_get_owned`).
    VecIndexElementTypeUnsupported {
        /// User-facing rendering of the unsupported element type
        /// (e.g. `"i8"`, `"f32"`, `"u16"`).
        element_ty: String,
    },
    /// `Vec<T>` range-slice (`xs[a..b]`, `xs[..b]`, `xs[a..]`, `xs[..]`,
    /// `xs[a..=b]`) with an element type that the runtime ABI does not
    /// (yet) implement a `hew_vec_slice_range_T` for. Fail-closed per
    /// slepp A222 / A228. Supported range-slice element types are `i32`,
    /// `u32`, `i64`, `u64`, `f64`, `String`, and any user-defined `Named`
    /// type (dispatched via `hew_vec_slice_range_ptr`).
    VecSliceElementTypeUnsupported {
        /// User-facing rendering of the unsupported element type
        /// (e.g. `"bool"`, `"char"`, `"f32"`).
        element_ty: String,
    },
    /// `.clone()` called on a value whose runtime copy path is not wired
    /// to the HIR/MIR pipeline.
    ///
    /// Fail-closed per the no-silent-stub invariant (M-COW P0):
    /// `.clone()` must NEVER be a silent no-op. `String`, `Vec`, `HashMap`,
    /// `HashSet`, and records (via `RecordCloneInplace`/`CopyCloneNoop`) all
    /// have wired copy paths and are resolved by the checker before this
    /// gate is reached. This diagnostic is the fail-closed backstop for the
    /// remaining unresolved cases — e.g. a type with no known clone method
    /// at all.
    CloneNotYetSupported {
        /// Human-readable rendering of the receiver's type.
        receiver_ty: String,
    },
}
