use std::collections::HashMap;

use hew_parser::ast::{BinaryOp, OverflowPolicy, Span};
use hew_types::{ChildSlot, ExecutionContextReader, ResolvedTy, VariantMatch};
use hew_types::{NumericMethodFamily, NumericMethodOp, NumericSignedness, NumericWidth};

use crate::ids::{BindingId, HirNodeId, ItemId, ResolvedRef, ScopeId, SiteId};
use crate::monomorph::{EnumLayout, MonomorphizedFn, RecordLayout};
use crate::value_class::{ResourceMarker, TypeClassTable};
use crate::{IntentKind, ValueClass};

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    pub items: Vec<HirItem>,
    /// Per-named-type classification table populated during HIR lowering from
    /// each `Item::TypeDecl` carrying a user marker and from compiler-known
    /// substrate registrations.
    /// Keyed by type name; value is `(marker, close_method_name)` where
    /// `close_method_name` is `Some(name)` for `@resource` types (the
    /// consuming method named `close`) and `None` for `@linear` and `None`-
    /// marked types. `BitCopy` substrate registrations are resolved through
    /// `lookup_type_marker`.
    ///
    /// This is the single authority for downstream phases asking "is this
    /// Named type resource/linear/`BitCopy`?" — `ValueClass::of_ty(ty, &type_classes)`
    /// reads from here. No phase re-derives the answer by walking the parser
    /// AST. LESSONS: `type-info-survival`.
    pub type_classes: TypeClassTable,
    /// Distinct generic-function instantiations observed at call sites in
    /// this module. Populated from the checker's `call_type_args`
    /// side-table during HIR lowering (G-1.a). Each entry pairs a
    /// generic origin `ItemId` with a concrete `Vec<ResolvedTy>` and a
    /// downstream-stable mangled symbol name. Insertion-ordered for
    /// deterministic codegen.
    ///
    /// Empty when no generic-fn callsites exist (a fully monomorphic
    /// program). Downstream MIR (G-1.b) and LLVM (G-1.c) iterate this
    /// list to emit one specialised function per entry.
    ///
    /// LESSONS: `producer-bridge-before-codegen` (P1),
    /// `checker-authority` (P0).
    pub monomorphisations: Vec<MonomorphizedFn>,
    /// Per-call-site type arguments observed at generic function calls.
    /// Keyed by the `SiteId` of the `HirExpr` whose `kind` is
    /// `HirExprKind::Call` and whose callee is a generic top-level user
    /// function.
    ///
    /// The recorded `ResolvedTy`s mirror the checker's `call_type_args`
    /// side-table, with one important nuance: when a call appears
    /// inside a generic function body, its recorded type arguments may
    /// reference the enclosing function's type-parameter symbols as
    /// `ResolvedTy::Named { name: "T", args: [] }`. MIR lowering of a
    /// specialised body substitutes those symbols with concrete types
    /// via the per-monomorphisation substitution map, producing the
    /// concrete mangled callee for the inner call.
    ///
    /// Empty when no generic-fn call sites exist (a fully monomorphic
    /// program).
    pub call_site_type_args: HashMap<SiteId, Vec<ResolvedTy>>,
    /// Distinct record-type instantiations observed at user struct-init
    /// sites against a generic `pub type` / `record`. Populated from the
    /// checker's `record_init_type_args` side-table during HIR lowering.
    /// Each entry pairs a generic origin `ItemId` with a concrete
    /// `Vec<ResolvedTy>`, a mangled symbol name, and the field shape after
    /// type-parameter substitution. Insertion-ordered for deterministic
    /// codegen.
    ///
    /// Empty when no user generic record-init sites exist. Downstream MIR
    /// and LLVM consumers iterate this list to emit one `RecordLayout` per
    /// entry under the mangled name. Builtin-injected generic types (`Vec`,
    /// `Option`, `Result`, `HashMap`, channel / stream handles) never enter
    /// this list — they remain compiler-injected for v0.5 by the generics
    /// scoping decision.
    ///
    /// LESSONS: `producer-bridge-before-codegen` (P1),
    /// `checker-authority` (P0).
    pub record_layouts: Vec<RecordLayout>,
    /// Distinct generic-enum instantiations observed at enum-ctor sites
    /// and match scrutinees in this module. Populated from the HIR mono
    /// pass discovery walker during HIR lowering. Each entry pairs a
    /// generic origin `ItemId` with concrete `Vec<ResolvedTy>` args, a
    /// mangled symbol name, and the variant shape after type-parameter
    /// substitution. Insertion-ordered for deterministic codegen.
    ///
    /// Empty when no user generic enum instantiation sites exist. Downstream
    /// MIR and codegen consumers iterate this list to emit one `EnumLayout`
    /// per entry under the mangled name. The registry is the substrate that
    /// consumed by MIR layout-gather and codegen to emit one `EnumLayout`
    /// per instantiation under the mangled name.
    ///
    /// LESSONS: `producer-bridge-before-codegen` (P1),
    /// `checker-authority` (P0).
    pub enum_layouts: Vec<EnumLayout>,
    /// Per-field-access `SiteId` → `ChildSlot` for supervisor child accessor
    /// expressions. Populated during HIR lowering from the checker's
    /// `supervisor_child_slots` side-table (keyed by span) by translating each
    /// `SpanKey` lookup to the pre-allocated `SiteId` of the lowered
    /// `HirExprKind::FieldAccess` node.
    ///
    /// MIR lowering (S2) reads this map before the `record_field_orders` path
    /// to intercept supervisor-typed LHS and emit the correct
    /// `hew_supervisor_child_get` (Static) or `hew_supervisor_pool_route`
    /// (Pool) ABI call.
    ///
    /// LESSONS: `checker-authority` (P0), `producer-bridge-before-codegen` (P1).
    pub supervisor_child_slots: HashMap<SiteId, ChildSlot>,
    /// Module-level regex literal table. Each distinct compiled pattern
    /// observed in match arms (keyed by normalized pattern string — no flags
    /// in v0.5) is allocated one entry here, deduplicated by string equality.
    ///
    /// Each `HirRegexLiteral` carries the 0-based index (its own position in
    /// this `Vec`) as `literal_id`, so MIR/codegen can use the index directly
    /// as the global-slot reference without scanning the table.
    ///
    /// MIR lowering (slice 4) will emit a module-init call that compiles
    /// each entry and stores the `*HewRegex` handle in a global slot indexed
    /// by `literal_id`. Codegen (slice 5) wires the global-slot reference.
    pub regex_literals: Vec<HirRegexLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirItem {
    Function(HirFn),
    TypeDecl(HirTypeDecl),
    Machine(HirMachineDecl),
    Record(HirRecordDecl),
    Actor(HirActorDecl),
    Supervisor(HirSupervisorDecl),
}

// ── Actor declarations ───────────────────────────────────────────────────────

/// Lowered actor declaration.
///
/// Carries the structural shape of an `actor` item — state fields, optional
/// `init { ... }` block, receive handlers (`receive fn`), regular methods, and
/// `#[on(start|stop|crash|upgrade)]` lifecycle hooks — together with lowered
/// bodies, parameter bindings, runtime-configuration metadata lifted from the
/// parser surface, and checker side-tables (`#[max_heap(N)]` arena cap).
#[derive(Debug, Clone, PartialEq)]
pub struct HirActorDecl {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    /// `let <name>: <ty>;` state fields declared in the actor body. Field
    /// ordering is source order; the runtime layout follows the same order.
    pub state_fields: Vec<HirField>,
    /// `init(params) { body }` — runs once when the actor is spawned. `None`
    /// when the actor has no explicit init block.
    pub init: Option<HirActorInit>,
    /// `receive fn name(params) -> ret { body }` — message handlers. Each
    /// handler corresponds to one variant of the actor's auto-generated
    /// message enum. May carry an `#[every(<duration>)]` attribute for
    /// periodic scheduling (validated by the checker; recorded here as a
    /// structural flag plus the duration in nanoseconds).
    pub receive_handlers: Vec<HirActorReceiveFn>,
    /// Plain methods on the actor (not lifecycle hooks), with lowered HIR
    /// bodies ready for MIR/codegen consumers.
    pub methods: Vec<HirActorMethod>,
    /// `#[on(start|stop|crash|upgrade)]` lifecycle hooks, exhaustively
    /// bucketed by `kind`. The checker (`check_actor_methods`) enforces
    /// uniqueness rules (at most one `#[on(start)]`, etc.); HIR mirrors
    /// the post-validation shape.
    pub lifecycle_hooks: Vec<HirLifecycleHook>,
    /// Per-actor arena cap in bytes, lifted from the checker's
    /// `actor_max_heap` side-table when present. `None` means no
    /// `#[max_heap]` annotation (unbounded arena); `Some(0)` is an explicit
    /// zero which the runtime treats as unbounded; `Some(N)` for `N > 0` is
    /// a hard cap. Codegen reads this to select
    /// `hew_arena_new` vs `hew_arena_new_with_cap`.
    pub max_heap_bytes: Option<u64>,
    /// `actor isolated <name> { ... }` — true when this actor is isolated
    /// (no shared-heap captures admitted at spawn sites). Pass-through from
    /// the parser AST; codegen consumes it.
    pub is_isolated: bool,
    /// `mailbox <N>` — fixed mailbox capacity. `None` means the runtime
    /// default applies.
    pub mailbox_capacity: Option<u32>,
    /// `overflow <policy>` — mailbox overflow policy. `None` means the
    /// runtime default (`Block`) applies. The `Coalesce` variant carries
    /// the keying field name and its fallback policy.
    pub overflow_policy: Option<OverflowPolicy>,
    /// Whether this actor participates in a reference cycle, lifted from
    /// the checker's `cycle_capable_actors` side-table. Codegen consumes
    /// this to select a refcount-cycle-breaking strategy at spawn sites.
    pub cycle_capable: bool,
    /// Checker-derived protocol descriptor: the stable name → `msg_id`
    /// mapping for every `receive fn` plus signature/symbol metadata.
    ///
    /// Lifted from `TypeCheckOutput.actor_protocol_descriptors` during HIR
    /// lowering. `None` means the descriptor was absent — either the actor
    /// declared no receive handlers (vacuous case), or the checker rejected
    /// the actor with an `ActorProtocolCollision` diagnostic. MIR consumers
    /// must treat a `None` on an actor with non-empty `receive_handlers`
    /// as fail-closed; the produced `msg_id` is no longer derivable from
    /// source order.
    pub protocol_descriptor: Option<hew_types::ActorProtocolDescriptor>,
    pub span: Span,
}

/// Lowered `init` block on an actor.
#[derive(Debug, Clone, PartialEq)]
pub struct HirActorInit {
    pub params: Vec<HirBinding>,
    pub body: HirBlock,
}

/// Lowered `receive fn` on an actor.
#[derive(Debug, Clone, PartialEq)]
pub struct HirActorReceiveFn {
    pub name: String,
    pub is_generator: bool,
    pub params: Vec<HirBinding>,
    /// For ordinary receive handlers this is the handler return type. For
    /// generator receive handlers this remains the declared yield element type;
    /// the body itself lowers with unit expectation.
    pub return_ty: ResolvedTy,
    pub body: HirBlock,
    /// Compiler-inserted actor-state guard required around this receive body.
    pub state_guard: HirActorStateGuard,
    /// `#[every(<duration>)]` periodic scheduling annotation. `None` if the
    /// receiver is purely message-driven; `Some(ns)` with the duration in
    /// nanoseconds when the checker has validated the attribute.
    pub every_ns: Option<i64>,
    /// Source span of the `receive fn` declaration (from the parser's
    /// `ReceiveFnDecl.span`).
    pub span: Span,
}

/// Actor-state guard policy carried by dispatchable actor HIR nodes.
///
/// This enum is intentionally closed at one variant.  The checker is the only
/// producer of `HirActorStateGuard` values; MIR lowers the variant to a
/// `requires_state_guard: bool` field on `ActorHandlerLayout` via an
/// exhaustive `match` that is a compile error if a new variant is added.
///
/// Adding a variant (e.g. `Shared` for read-only handlers) requires:
/// 1. A corresponding `match` arm in `hew_mir::lower::lower_actor_handler_layouts`
///    that maps the new policy to the correct `requires_state_guard` value.
/// 2. Checker logic to produce the new variant for the appropriate handler forms.
/// 3. A MIR acceptance test that verifies the layout propagation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirActorStateGuard {
    /// Generated dispatch acquires exclusive actor-state access for the body.
    Exclusive,
}

/// Lowered plain method on an actor (not a lifecycle hook).
#[derive(Debug, Clone, PartialEq)]
pub struct HirActorMethod {
    pub name: String,
    pub params: Vec<HirBinding>,
    pub return_ty: ResolvedTy,
    pub body: HirBlock,
    /// Source span of the `fn` declaration (from the parser's
    /// `FnDecl.fn_span`).
    pub span: Span,
}

/// Lowered `#[on(<kind>)]` lifecycle hook on an actor. The `kind`
/// discriminator was validated upstream by the checker
/// (`resolve_on_hook_kind`); unknown / malformed kinds never reach HIR.
#[derive(Debug, Clone, PartialEq)]
pub struct HirLifecycleHook {
    pub kind: HirLifecycleHookKind,
    pub name: String,
    pub params: Vec<HirBinding>,
    pub return_ty: ResolvedTy,
    pub body: HirBlock,
    /// Source span of the `fn` declaration (from the parser's
    /// `FnDecl.fn_span`).
    pub span: Span,
}

/// The four lifecycle-hook kinds recognised by HEW-SPEC-2026 §9.1.2.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirLifecycleHookKind {
    /// `#[on(start)]` — runs once after the actor is spawned and its
    /// `init` block has completed. At most one per actor.
    Start,
    /// `#[on(stop)]` — runs during actor shutdown. May appear multiple
    /// times per actor; runs in lexical declaration order.
    Stop,
    /// `#[on(crash)]` — runs when the actor body traps. Takes the stdlib
    /// crash-info payload parameter and returns the stdlib crash action enum.
    Crash,
    /// `#[on(upgrade)]` — reserved marker for hot-upgrade flows. No
    /// runtime invocation in v0.5.
    Upgrade,
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
    /// Generic type parameters declared on the machine (e.g. `Lifecycle<T>`).
    ///
    /// Names only — see `MachineDecl::type_params`. Threaded verbatim from
    /// the parser AST; subsequent layers (type checker, MIR, codegen) are
    /// responsible for interpreting these names.
    pub type_params: Vec<String>,
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
    /// Best-effort lowered `entry { ... }` block, populated when the source
    /// state carries an entry block. Slice 1 substrate — downstream consumers
    /// (MIR/codegen) are wired in later slices. Constructs that depend on
    /// later-slice HIR forms (e.g. `emit`, `this`, bare state-name expressions)
    /// lower to `HirExprKind::Unsupported` placeholders; the canonical
    /// machine-body diagnostics still come from the AST-walking summary checks
    /// driven by `entry_writes` / `body_emits`.
    pub entry: Option<HirBlock>,
    /// Best-effort lowered `exit { ... }` block. See `entry` for the
    /// best-effort lowering contract.
    pub exit: Option<HirBlock>,
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
    /// Lowered transition body. Constructs that depend on later-slice HIR
    /// forms (notably `Expr::This` and bare state-name references) lower to
    /// `HirExprKind::Unsupported` placeholders. `emit` expressions lower to
    /// `HirExprKind::MachineEmit` (Slice 2). Downstream MIR/codegen consumers
    /// are wired in later slices.
    pub body: HirExpr,
    pub span: Span,
}

/// Lowered `record` declaration.
///
/// Carries the record name, optional type parameters, and the field set
/// (named-form fields, resolved to `ResolvedTy`). Tuple-form records have an
/// empty `fields` vec at this HIR level — their constructor is reached via
/// `Expr::Call` → `fn_registry`, not via `StructInit`. Methods and `@resource`
/// / `@linear` markers are not permitted on records by the parser; the field
/// guard (below) rejects `@linear`-typed fields at HIR lowering time.
///
/// MIR lowering for record construction, field access, and functional-update
/// is deferred to slice A-7 (codegen-rs layer); this HIR node provides the
/// structural substrate that A-7 will consume.
#[derive(Debug, Clone, PartialEq)]
pub struct HirRecordDecl {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    pub type_params: Vec<String>,
    pub fields: Vec<HirField>,
    pub span: Span,
}

// ── Supervisor declarations ──────────────────────────────────────────────────

/// Lowered supervisor declaration.
///
/// Carries the structural shape of a `supervisor` item — strategy, restart
/// budget, time window, and child/pool specifications. Bodies (MIR producer
/// wiring, codegen) are deferred to slices S-C/S-D. The Rust MIR producer
/// treats `HirItem::Supervisor` as a no-op tier alongside `Record`/`Actor`.
#[derive(Debug, Clone, PartialEq)]
pub struct HirSupervisorDecl {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    pub strategy: Option<HirSupervisorStrategy>,
    pub max_restarts: Option<i64>,
    /// Window duration as a raw string from the parser (e.g. `"60s"`).
    /// Parsed to a concrete `Duration` in S-C when wiring is lowered.
    pub window: Option<String>,
    pub children: Vec<HirSupervisorChild>,
    pub span: Span,
}

/// One child or pool entry within a supervisor declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct HirSupervisorChild {
    pub name: String,
    pub ty: String,
    pub restart_policy: Option<HirRestartPolicy>,
    /// Declarative sibling wiring: init-param name → sibling child name.
    /// `None` means no `wired_to:` clause. S-B validates key correctness.
    pub wired_to: Option<std::collections::HashMap<String, String>>,
    /// `true` when declared with `pool name: Type`; `false` for `child name: Type`.
    pub is_pool: bool,
    /// Compile-time-assigned slot index within the child's own slot space.
    ///
    /// Static children (`is_pool = false`) are indexed into `HewSupervisor.children[]`.
    /// Pool children (`is_pool = true`) are indexed into `HewSupervisor.pool_slots[]`.
    /// Both spaces start at 0 and are disjoint.
    ///
    /// Assigned by the HIR lowering pass by counting each partition in source order.
    /// MIR lowering reads this field to emit the correct runtime ABI call.
    pub slot_index: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirSupervisorStrategy {
    OneForOne,
    OneForAll,
    RestForOne,
    /// Dynamic pool strategy; used with `pool` child declarations.
    SimpleOneForOne,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirRestartPolicy {
    Permanent,
    Transient,
    Temporary,
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
    /// Source-declared generic type-parameter names, in order. Empty for
    /// non-generic type decls. Consumed by the record-layout
    /// registry to (a) decide whether a `StructInit` site needs a
    /// per-instantiation layout and (b) substitute field types when
    /// constructing one.
    pub type_params: Vec<String>,
    /// Struct-form field types in declaration order. Empty for enum-kind
    /// type decls (enums have no struct fields; their variants are in
    /// `variants`).
    pub fields: Vec<HirField>,
    /// All enum variants in declaration order — unit, tuple, and struct shapes.
    /// Empty for struct-kind type decls. The tag ordinal for each variant is
    /// its position within this vec (0-based) and matches the index assigned
    /// in `LowerCtx::machine_ctor_registry` for the qualified `Type::Variant`
    /// key, so MIR's `EnumLayout.variants` lines up 1:1 with this list.
    ///
    /// MIR consumes this to populate `EnumLayout.variants` (including
    /// `field_tys` per variant) without re-reading the parser AST. Generic
    /// enums (those with non-empty `type_params`) are fail-closed at MIR
    /// lowering — they require monomorphisation-keyed layouts which the
    /// next-stage `EnumLayoutRegistry` lane will land.
    pub variants: Vec<HirVariant>,
    pub span: Span,
}

/// One variant of an enum-kind `HirTypeDecl`. Mirrors the shape distinctions
/// in `hew_parser::ast::VariantKind` but with payload types fully resolved
/// against the module's type scope so MIR/codegen never re-walk the parser AST.
///
/// `tag_idx` is implicit: it is this variant's position within
/// `HirTypeDecl.variants` and matches the MIR/codegen `EnumLayout.variants`
/// ordering.
#[derive(Debug, Clone, PartialEq)]
pub struct HirVariant {
    pub name: String,
    pub kind: HirVariantKind,
}

/// Payload shape of an `HirVariant`. Unit variants carry no payload (tag
/// only); tuple variants carry positional payload types in declaration order;
/// struct variants carry `(name, ty)` pairs in declaration order.
///
/// MIR's `EnumLayout` builder flattens both `Tuple` and `Struct` into a single
/// positional `field_tys` list — variant field NAMES are MIR-irrelevant (see
/// D1 in the lane plan). Names are retained here for diagnostics and for the
/// `Expr::StructInit` lowering path which validates source-declared field
/// names against the variant's declared field set before emitting payload
/// store instructions.
#[derive(Debug, Clone, PartialEq)]
pub enum HirVariantKind {
    Unit,
    Tuple(Vec<ResolvedTy>),
    Struct(Vec<(String, ResolvedTy)>),
}

impl HirVariant {
    /// Positional payload types in declaration order. Empty for `Unit`,
    /// returns the tuple element types directly for `Tuple`, and projects
    /// `(_, ty)` from each struct field for `Struct`. MIR's `EnumLayout`
    /// builder consumes this to populate `MachineVariantLayout.field_tys`.
    #[must_use]
    pub fn field_tys(&self) -> Vec<ResolvedTy> {
        match &self.kind {
            HirVariantKind::Unit => Vec::new(),
            HirVariantKind::Tuple(tys) => tys.clone(),
            HirVariantKind::Struct(fields) => fields.iter().map(|(_, ty)| ty.clone()).collect(),
        }
    }

    /// True for `Unit` variants. Used by the ctor-arity gate in the call /
    /// struct-init lowering paths.
    #[must_use]
    pub fn is_unit(&self) -> bool {
        matches!(self.kind, HirVariantKind::Unit)
    }
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
    Assign {
        target: HirExpr,
        value: Box<HirExpr>,
    },
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
    /// A reference to a compiled regex literal in the module's literal table.
    ///
    /// Produced from `Expr::RegexLiteral` when the pattern appears as a
    /// standalone expression (e.g. `let r = re"hello";`). Match-arm patterns
    /// `re"..."` are lowered directly into `HirMatchArmPredicate::Regex`
    /// without emitting a `RegexLiteralRef` node for the pattern itself;
    /// the literal table entry is still allocated via `alloc_regex_literal`.
    ///
    /// `literal_id` is the 0-based index into `HirModule::regex_literals`.
    /// MIR lowering (slice 4) will load the compiled handle from the
    /// corresponding global slot. Codegen (slice 5) wires the global.
    ///
    /// Type: `ResolvedTy::Regex` (the opaque regex handle type).
    RegexLiteralRef {
        literal_id: u32,
        pattern: String,
        captures: Vec<String>,
    },
    BindingRef {
        name: String,
        resolved: ResolvedRef,
    },
    ContextReader {
        reader: ExecutionContextReader,
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
    /// `spawn Actor(field: value, ...)` — named-actor spawn. The checker owns
    /// the result type (`LocalPid<Actor>`); HIR carries only the structural spawn
    /// surface and lowered init arguments for MIR/codegen.
    Spawn {
        actor_name: String,
        args: Vec<(String, HirExpr)>,
    },
    /// Fire-and-forget actor receive dispatch, selected from the checker's
    /// `actor_method_dispatch` side table. HIR does not reclassify receiver
    /// types; absence of a checker discriminator for an actor receiver is a
    /// boundary diagnostic.
    ActorSend {
        receiver: Box<HirExpr>,
        method_id: String,
        args: Vec<HirExpr>,
    },
    /// Request/reply actor receive dispatch, selected from the checker's
    /// `actor_method_dispatch` side table. `reply_ty` is checker-resolved and
    /// crosses the boundary as `ResolvedTy`.
    ActorAsk {
        receiver: Box<HirExpr>,
        method_id: String,
        args: Vec<HirExpr>,
        reply_ty: ResolvedTy,
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
        /// Functional-update base: the record value from which un-overridden
        /// fields are copied. `None` for plain construction (`R { x: 1 }`),
        /// `Some(base_expr)` for `R { x: 1, ..base }`.
        ///
        /// MIR lowering (slice A-7) will desugar this into individual field
        /// reads from the base expression for every field absent from
        /// `fields`. The HIR carries it verbatim for checker-stream coverage.
        base: Option<Box<HirExpr>>,
    },
    /// `object.field` — named-field read on a record or struct type.
    ///
    /// The checker (`check_field_access`) resolves the field type and records
    /// it in `expr_types`; this HIR node is produced only when the checker
    /// has already confirmed the field exists.
    ///
    /// MIR lowering (slice A-7) will emit the field-read instruction. For
    /// A-6 the MIR producer walks the `object` sub-expression for
    /// checker-stream coverage and defers via `NotYetImplemented`.
    FieldAccess {
        object: Box<HirExpr>,
        field: String,
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
    /// `fork { ... }` inside a scope. The block is an anonymous child task
    /// body; later MIR slices attach a derived cancellation token and spawn it.
    ForkBlock {
        body: HirBlock,
        task_ty: ResolvedTy,
    },
    /// `after(duration) { ... }` inside a scope. The clause is the lexical
    /// deadline edge that later MIR slices lower to scope-token cancellation.
    ScopeDeadline {
        duration: Box<HirExpr>,
        body: HirBlock,
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
    /// General closure literal (`|...| ...` / `move |...| ...`).
    ///
    /// The checker owns capture legality and records binding-accurate capture
    /// facts keyed by the closure literal span. HIR lowers those facts into a
    /// resolved capture ledger with HIR binding ids and fully-resolved field
    /// types so MIR can materialise a concrete environment record rather than
    /// rediscovering captures from expression shape.
    Closure {
        params: Vec<HirBinding>,
        ret_ty: ResolvedTy,
        body: Box<HirExpr>,
        captures: Vec<HirClosureCapture>,
    },
    /// `gen { ... }` generator literal.
    ///
    /// The checker owns `Generator<Yield, Return>` inference. HIR carries the
    /// lowered body plus the extracted yield/return parameters so MIR can later
    /// build the generator state machine without re-inferring the signature.
    GenBlock {
        body: HirBlock,
        yield_ty: ResolvedTy,
        return_ty: ResolvedTy,
    },
    /// `yield` inside a generator body.
    ///
    /// `yield_ty` is the enclosing generator's checker-inferred Yield
    /// parameter, not the expression's own result type (`yield` evaluates to
    /// unit in the body).
    Yield {
        value: Option<Box<HirExpr>>,
        yield_ty: ResolvedTy,
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
    /// `xs[i]` — integer-indexed element access on a `Vec<T>` container.
    ///
    /// The checker (`synthesize_index`) validates that `container` is `Vec<T>`
    /// and `index` is `i64`; the expression type is `T`. The HIR node carries
    /// the container and index sub-expressions; MIR lowering emits a
    /// bounds-check CFG (`hew_vec_len` + `IntCmp` + `Branch → Trap/cont`) and
    /// then a `CallRuntimeAbi(hew_vec_get_T)` on the success path.
    ///
    /// LESSONS: `checker-authority` (P0) — the allowance set (Vec<T> only, i64
    /// index) is validated by the checker; this node is produced only when the
    /// checker has already confirmed the shape.
    Index {
        /// The container expression (type `Vec<T>`).
        container: Box<HirExpr>,
        /// The index expression (type `i64`).
        index: Box<HirExpr>,
    },
    /// `xs[a..b]` / `xs[a..=b]` / `xs[..b]` / `xs[a..]` / `xs[..]` —
    /// range-slice on a `Vec<T>` container (C-3). The expression type is
    /// `Vec<T>` (a freshly-allocated copy populated from `[start, end)`).
    ///
    /// Open endpoints (`start: None`, `end: None`) survive into HIR and
    /// are desugared at MIR lowering: open `start` becomes the constant
    /// `0`, open `end` becomes `hew_vec_len(container)`. The inclusive
    /// flag (`a..=b`) is also desugared in MIR via `IntArithChecked(Add)`
    /// on `b + 1` with a `TrapKind::IntegerOverflow` trap on overflow.
    /// MIR additionally inserts a bounds-check pair (`start <= end` and
    /// `end <= len(container)`) before calling
    /// `CallRuntimeAbi(hew_vec_slice_range_T)`.
    ///
    /// LESSONS: `checker-authority` (P0) — produced only after the
    /// checker has confirmed the receiver is `Vec<T>` and the endpoints
    /// are integer-typed.
    Slice {
        /// The container expression (type `Vec<T>`).
        container: Box<HirExpr>,
        /// Lower bound (inclusive). `None` for `xs[..b]` / `xs[..]`.
        start: Option<Box<HirExpr>>,
        /// Upper bound. `None` for `xs[a..]` / `xs[..]`.
        end: Option<Box<HirExpr>>,
        /// `true` for `xs[a..=b]`. MIR adds 1 to `end` with overflow trap.
        inclusive: bool,
    },
    /// `lhs is rhs` — identity comparison on handle-typed or machine-typed
    /// operands. The checker (D-2) validates that both operands are allowable
    /// identity-bearing types and sets the expression type to `ResolvedTy::Bool`.
    ///
    /// MIR lowering emits `Instr::IdentityCompare { dest, lhs, rhs }`, which
    /// codegen lowers to `ptrtoint` + `icmp eq` + `zext` for pointer-shaped
    /// types (LESSONS: `checker-authority` P0 — codegen reads the operand type
    /// from the HIR, never re-infers the identity rule).
    IdentityCompare {
        left: Box<HirExpr>,
        right: Box<HirExpr>,
    },
    /// Wrap a concrete value in a `dyn Trait` fat pointer. Emitted at
    /// every accepted `T → dyn Trait` coercion site (the checker's
    /// `TypeCheckOutput::dyn_trait_coercions` side table). MIR lowers
    /// 1:1 to `Instr::CoerceToDynTrait`.
    ///
    /// The carried `method_table` mirrors `DynCoercion::method_table` —
    /// codegen consumes it to materialise per-trait vtable statics.
    /// `concrete_type` is the resolved `Self` type at the coercion site
    /// (after `materialize_literal_defaults`), which doubles as the
    /// `(Trait, ImplType)` dedup key for the vtable static.
    CoerceToDynTrait {
        value: Box<HirExpr>,
        trait_name: String,
        concrete_type: ResolvedTy,
        method_table: Vec<(String, String)>,
        vtable_entries: Vec<hew_types::DynVtableEntry>,
    },
    /// Dispatch a method call through a `dyn Trait` fat pointer's
    /// vtable. Emitted in place of an `HirExprKind::Call` whenever
    /// the receiver typed as `Ty::TraitObject` (the checker's
    /// `TypeCheckOutput::dyn_trait_method_calls` side table). MIR
    /// lowers 1:1 to `Instr::CallTraitMethod`.
    ///
    /// `slot` is the pre-computed vtable index
    /// (`3 + method_decl_order` for the originating trait — see
    /// `DynMethodCall::slot`). HIR/MIR never re-derive the slot.
    CallDynMethod {
        receiver: Box<HirExpr>,
        trait_name: String,
        method_name: String,
        slot: u32,
        args: Vec<HirExpr>,
        ret_ty: ResolvedTy,
    },
    /// Checker-authoritative integer opt-out method call:
    /// `.wrapping_*`, `.checked_*`, or `.saturating_*` for add/sub/mul.
    ///
    /// Produced only from `TypeCheckOutput::numeric_method_lowerings`.
    /// Downstream phases must consume the carried discriminators rather than
    /// re-matching the surface method name.
    NumericMethod {
        receiver: Box<HirExpr>,
        arg: Box<HirExpr>,
        family: NumericMethodFamily,
        op: NumericMethodOp,
        result_ty: ResolvedTy,
        operand_ty: ResolvedTy,
        signedness: NumericSignedness,
        width: NumericWidth,
    },
    /// `emit EventName { field: value, ... }` inside a machine transition body,
    /// entry block, or exit block.
    ///
    /// `event_idx` is the zero-based index of the emitted event in the enclosing
    /// machine's event list (`HirMachineDecl::events`). Resolved at HIR lowering
    /// time from the `Expr::MachineEmit { event_name }` surface form; an unknown
    /// event name emits `UnresolvedSymbol` and the HIR body is not produced.
    ///
    /// `fields` are the named field initialisers for the event payload. Unit
    /// events (no declared fields) have an empty `fields` vec.
    ///
    /// MIR/codegen consumers: wired in Lane B Slices 4b and 7.
    MachineEmit {
        event_idx: usize,
        fields: Vec<(String, HirExpr)>,
    },
    /// `m.step(event) -> ()` — advance the machine one step.
    ///
    /// The checker has verified that `receiver` is a mutable binding and that
    /// `event` matches the `NameEvent` companion enum.  MIR/codegen consumers
    /// lower this to a call to the internal `<machine_name>__step` helper
    /// followed by a store-back into the receiver's binding slot (slice 6).
    ///
    /// `machine_name` is the unqualified machine type name (e.g. `"TrafficLight"`).
    MachineStep {
        machine_name: String,
        receiver: Box<HirExpr>,
        event: Box<HirExpr>,
    },
    /// `m.state_name() -> String` — current state tag as a string.
    ///
    /// MIR/codegen consumers lower this to a static string-table lookup on
    /// the tag field of the machine value (slice 6).
    ///
    /// `machine_name` is the unqualified machine type name (e.g. `"TrafficLight"`).
    MachineStateName {
        machine_name: String,
        receiver: Box<HirExpr>,
    },
    /// Tagged-union variant constructor — shared by machine states and user-defined
    /// enum unit variants.
    ///
    /// **Machine states**: produced by HIR lowering when an `Expr::Identifier` or
    /// `Expr::StructInit` names a declared state of the enclosing machine — either
    /// a bare state reference like `Green` (unit state, no payload) or a struct-init
    /// form like `SynReceived { remote_port: remote_port }` (state with payload).
    ///
    /// **User-defined enum unit variants**: produced by HIR lowering when an
    /// `Expr::Identifier` resolves to a unit variant of an enum type declared with
    /// `type Colour { enum Red; Green; Blue; }`. The same tagged-union substrate
    /// (`Place::MachineTag` / `EnumLayout`) handles both surface forms.
    ///
    /// Unlike a record `StructInit`, this is HIR-side-resolved: the checker did
    /// not assign a side-table type for this site (machine state names and user
    /// enum variant names are not in the checker's record or type scope). HIR
    /// derives the result type as `ResolvedTy::Named { name: machine_name, args: [] }`
    /// because a state/variant constructor IS the tagged-union value at that variant.
    /// This is intentional deviation from the `checker-authority` pattern and is
    /// documented here to make it findable.
    ///
    /// `machine_name`: the unqualified tagged-union type name. For machine states
    ///   this is the machine type (e.g. `"TrafficLight"`). For user enum variants
    ///   this is the enum type (e.g. `"Colour"`). The MIR consumer looks up the
    ///   layout in the shared `MachineLayoutMap` / `EnumLayout` registry by this name.
    /// `state_idx`: zero-based ordinal of this variant within the tagged union.
    ///   For machine states: index into `HirMachineDecl.states` (declaration order).
    ///   For user enum variants: position within `HirTypeDecl.variants` (declaration
    ///   order across unit + tuple + struct shapes — same index the
    ///   ctor-registry pre-pass assigns).
    /// `payload`: `None` for unit states/variants; `Some(fields)` for states
    ///   and variants with payload. For tuple variants the field NAMES are
    ///   synthetic `"0"`, `"1"`, ... — MIR discards them and indexes by
    ///   position (see lane-plan D1).
    ///
    /// MIR consumers: build a tagged-union value with the given tag and store payload
    /// fields via `Place::MachineTag` / `Place::MachineVariant` primitives.
    MachineVariantCtor {
        machine_name: String,
        state_idx: usize,
        payload: Option<Vec<(String, HirExpr)>>,
    },
    /// Read a payload field from the machine value bound to `self` inside a
    /// transition body. Resolved when `Expr::FieldAccess { object: Expr::This, field }`
    /// appears inside a machine transition body.
    ///
    /// `machine_name`: enclosing machine type name.
    /// `state_idx`: source state index (the transition's `from` state), which
    ///   determines which variant's payload fields are in scope. Tag dominance
    ///   is guaranteed by the transition dispatch context (Slice 4b).
    /// `field_idx`: zero-based index into that state's `HirMachineState.fields`.
    /// `field_name`: the field name for diagnostics and dump output.
    ///
    /// HIR derives the result type from `HirMachineState.fields[field_idx].ty`
    /// (same HIR-side-authority deviation as `MachineVariantCtor`).
    ///
    /// MIR consumers: load via `Place::MachineVariant { binding: <self-binding>,
    /// variant_idx: state_idx, field_idx }` (Slice 4b).
    MachineFieldAccess {
        machine_name: String,
        state_idx: usize,
        field_idx: usize,
        field_name: String,
    },
    /// `while cond { body }` — loops until `cond` evaluates to false.
    ///
    /// The expression type is always `Unit`; a while loop never produces
    /// a value.  MIR lowering emits a header block (condition test) and
    /// a body block with a back-edge to the header.
    While {
        condition: Box<HirExpr>,
        body: HirBlock,
    },
    /// `for binding in start..end { body }` — Range iteration
    /// (exclusive or inclusive) over an integer range.
    ///
    /// This node is produced only for `Range`-typed iterables; other
    /// iterable types (Vec, Stream, etc.) are not yet implemented and
    /// cause HIR lowering to emit `NotYetImplemented`.
    ///
    /// The expression type is always `Unit`.  MIR lowering emits a
    /// counter local (typed `i64`), a header block (bounds check), a
    /// body block, and a back-edge.
    ForRange {
        /// The loop-variable binding.  Immutable, scoped to each
        /// iteration; MIR lowering overwrites the backing local on every
        /// iteration rather than allocating a new one each time.
        binding: HirBinding,
        /// Resolved start expression (the left side of `..`).
        start: Box<HirExpr>,
        /// Resolved end expression (the right side of `..` / `..=`).
        end: Box<HirExpr>,
        /// `true` for `..=` (inclusive upper bound).
        inclusive: bool,
        /// Loop body.
        body: HirBlock,
    },
    /// `match scrutinee { arm; arm; ... }` — tag-dispatch over an enum-typed
    /// scrutinee. Produced exclusively from `Expr::Match` when the type
    /// checker has resolved each accepted arm's pattern via the
    /// `pattern_resolutions` side table.
    ///
    /// **Scope (v0.5 monomorphic enum substrate)**: unit-variant patterns
    /// (`Colour::Red`), payload-bearing constructor patterns over monomorphic
    /// enums (`Shape::Line(x)`, `Shape::Box { w, h }`), and wildcard arms
    /// (`_`) are lowered here. Pattern guards, literal patterns,
    /// tuple-without-ctor patterns, and or-patterns are still rejected at HIR
    /// lowering with a structured `NotYetImplemented` diagnostic. Generic
    /// Generic enums (`Option<T>`, `Result<T,E>`) are fully supported
    /// via the `EnumLayoutRegistry` substrate introduced in the
    /// generic-enum-monomorphisation lane.
    ///
    /// **Exhaustiveness**: enforced by the type checker at
    /// `hew-types/src/check/diagnostics.rs::check_exhaustiveness`. A
    /// non-exhaustive enum match is a hard error before HIR lowering
    /// produces this node. The MIR producer additionally emits a
    /// `Terminator::Trap { kind: ExhaustivenessFallthrough }` block at the
    /// chain's tail as a fail-closed runtime guard
    /// (LESSONS `match-fail-closed`).
    Match {
        /// The matched expression. Its `ty` is the resolved enum type.
        scrutinee: Box<HirExpr>,
        /// Arms in source order. Each arm's `variant_match` is `None` for
        /// a wildcard arm or `Some(VariantMatch)` for a unit-variant arm.
        arms: Vec<HirMatchArm>,
    },
    Unsupported(String),
}

/// A compiled regex literal observed in a match arm.
///
/// Each distinct pattern (by string equality — no flags in v0.5) gets one
/// entry in `HirModule::regex_literals`. The `literal_id` field is the
/// 0-based index of this entry in that `Vec`; it matches the global-slot
/// number that MIR (slice 4) and codegen (slice 5) will use to reference the
/// compiled `*HewRegex` handle at runtime.
///
/// LESSONS: `checker-authority` (P0) — the `captures` list comes from the
/// `PatternKind::Regex { captures }` resolver output in the type checker;
/// HIR lowering never re-derives it.
#[derive(Debug, Clone, PartialEq)]
pub struct HirRegexLiteral {
    /// 0-based position of this entry in `HirModule::regex_literals`. Stable
    /// across the HIR→MIR→codegen pipeline; used as the global-slot index.
    pub literal_id: u32,
    /// The raw pattern string as written in source. No normalisation is
    /// applied in v0.5 — deduplication is by string equality on the raw
    /// pattern. Future flag support will key on `(pattern, flags)`.
    pub pattern: String,
    /// Named capture groups derived from `PatternKind::Regex { captures }`
    /// after the checker compiled and validated the pattern. Positional groups
    /// participate in matching but are not listed here. Each entry is
    /// `(name, group_index)` where `group_index` is the 1-based regex group
    /// position (group 0 is the whole match). This preserves the real group
    /// position across the HIR→MIR boundary so MIR can pass the correct index
    /// to `hew_regex_capture` even when unnamed groups precede named ones.
    pub captures: Vec<(String, u32)>,
}

/// The predicate of one `HirMatchArm`.
///
/// `Wildcard` replaces the `None` dual in the old two-field encoding.
/// `EnumVariant` carries the data that was previously split across
/// `variant_match: Option<VariantMatch>` and `variant_idx: Option<u32>`.
/// `Regex` is the new regex-pattern arm added for string-scrutinee matches.
///
/// MIR lowering (slice 4) dispatches on this enum; codegen (slice 5) drives
/// the predicate to the runtime ABI. Paths that are not yet wired must
/// `todo!("MIR regex predicate — slice 4")` — never silently no-op.
#[derive(Debug, Clone, PartialEq)]
pub enum HirMatchArmPredicate {
    /// `_` wildcard — matches any scrutinee value. At most one per match
    /// expression; the checker enforces this.
    Wildcard,
    /// A unit-variant constructor arm (e.g. `Colour::Red`).
    ///
    /// `variant_match` is the checker-resolved `(type_name, variant_name)`
    /// identity. `variant_idx` is the zero-based declaration-order index
    /// within `HirTypeDecl.unit_variants`; MIR emits the tag-comparison
    /// constant directly from this field.
    EnumVariant {
        variant_match: VariantMatch,
        variant_idx: u32,
    },
    /// A regex literal pattern `re"..."` in a string-scrutinee match arm.
    ///
    /// `literal_id` is the index into `HirModule::regex_literals` for the
    /// compiled pattern. `captures` lists the named capture groups (empty
    /// when the pattern has none). Each entry is `(name, group_index)` where
    /// `group_index` is the 1-based regex group position. MIR lowering uses
    /// these to emit capture-extraction instructions before the arm body,
    /// passing the real group index to `hew_regex_capture`.
    Regex {
        literal_id: u32,
        pattern: String,
        captures: Vec<(String, u32)>,
    },
}

/// One arm of an `HirExprKind::Match` expression.
///
/// `predicate` encodes the arm's matching condition as an explicit enum,
/// replacing the old two-field `(variant_match, variant_idx)` encoding.
/// Wildcard arms (`_`) use `HirMatchArmPredicate::Wildcard`; unit-variant
/// constructor arms use `HirMatchArmPredicate::EnumVariant`; regex-literal
/// arms use `HirMatchArmPredicate::Regex`.
///
/// Payload-bearing constructor patterns carry their per-field bindings in
/// `bindings`; guards still never produce a `HirMatchArm` and are rejected at
/// HIR lowering with a structured diagnostic.
///
/// `body` is the arm's right-hand-side expression. The arm's source span
/// is preserved for diagnostics.
#[derive(Debug, Clone, PartialEq)]
pub struct HirMatchArm {
    /// The matching predicate for this arm.
    pub predicate: HirMatchArmPredicate,
    /// Payload bindings introduced by this arm's constructor pattern.
    pub bindings: Vec<HirMatchArmBinding>,
    /// Arm body expression. Evaluates only when this arm's predicate wins.
    pub body: HirExpr,
    /// Source span of the arm (pattern through body).
    pub span: std::ops::Range<usize>,
}

/// One named payload binding introduced by a constructor-pattern match arm.
///
/// MIR's `lower_match` walks these per arm and emits
/// `Move { dest: <binding_local>, src: Place::EnumVariant { local:
/// scrutinee, variant_idx, field_idx } }` at arm-body entry, registering the
/// `binding_id → local` pair in `binding_locals` so the arm body's identifier
/// reads resolve correctly.
#[derive(Debug, Clone, PartialEq)]
pub struct HirMatchArmBinding {
    /// Fresh binding id allocated for this slot. Same shape as a `let`
    /// binding's id — references in the arm body resolve to it via
    /// `lookup`/`BindingRef`.
    pub binding: BindingId,
    /// 0-based index of the payload slot within the variant's `field_tys`
    /// list. Matches the checker's `PayloadBinding.field_idx`.
    pub field_idx: u32,
    /// Surface binding name. Retained for diagnostics and HIR dumps; MIR
    /// does not look bindings up by name.
    pub name: String,
    /// Fully resolved type of the payload slot.
    pub ty: ResolvedTy,
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

/// One captured binding inside a general closure environment.
#[derive(Debug, Clone, PartialEq)]
pub struct HirClosureCapture {
    /// The captured binding's id in the enclosing HIR scope.
    pub binding: BindingId,
    /// Surface name used at the capture site.
    pub name: String,
    /// Fully-resolved field type stored in the generated environment record.
    pub ty: ResolvedTy,
    /// Checker-selected by-value capture mode.
    pub mode: hew_types::ClosureCaptureMode,
    /// Whether the captured type satisfies the checker-owned Send contract.
    pub is_send: bool,
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
///
/// `binding_id` is the [`BindingId`] HIR allocated for `binding_name`
/// when it pushed the arm-body's scope (see slice 1 — `lower_select` at
/// `lower.rs:3262`). MIR's `Terminator::Select` producer reads this id
/// to register `binding_locals[id] = <reply_dest>` before lowering the
/// arm body so the body's `BindingRef` to `binding_name` resolves to
/// the per-arm reply slot. `None` whenever `binding_name` is `None`
/// (e.g. `AfterTimer` arms; arms without an `<name> from` pattern).
#[derive(Debug, Clone, PartialEq)]
pub struct HirSelectArm {
    pub kind: HirSelectArmKind,
    pub binding_name: Option<String>,
    pub binding_id: Option<BindingId>,
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
