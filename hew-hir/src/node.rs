use std::collections::HashMap;

use hew_parser::ast::{BinaryOp, OverflowPolicy, Span, UnaryOp};
use hew_types::{
    ChildSlot, ExecutionContextReader, ImplId, MethodTargetFamily, ResolvedTy, TyPattern,
    VariantMatch,
};
use hew_types::{
    NumericMethodFamily, NumericMethodOp, NumericSignedness, NumericWidth, WireCodecDirection,
};

use crate::ids::{BindingId, HirNodeId, ItemId, ResolvedRef, ScopeId, SiteId};
use crate::mono::MachineMonoEntry;
use crate::monomorph::{EnumLayout, MonomorphizedFn, RecordLayout};
use crate::value_class::{ResourceMarker, TypeClassTable};
use crate::{IntentKind, ValueClass};

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    pub items: Vec<HirItem>,
    /// Source-module attribution for non-root top-level HIR items, keyed by
    /// item id. Diagnostics emitted while verifying one of these items inherit
    /// the same dotted module key used by `HirDiagnostic::source_module`.
    pub diagnostic_source_modules: HashMap<ItemId, String>,
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
    /// LESSONS: `end-to-end-before-layer-thickening` (P1),
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
    /// LESSONS: `end-to-end-before-layer-thickening` (P1),
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
    /// LESSONS: `end-to-end-before-layer-thickening` (P1),
    /// `checker-authority` (P0).
    pub enum_layouts: Vec<EnumLayout>,
    /// Distinct machine-type instantiations discovered by the dedicated
    /// post-function-mono pass
    /// ([`crate::machine_mono::run_machine_mono_pass`]). Populated after
    /// `monomorphisations` is closed under substitution, before MIR
    /// layout build.
    ///
    /// Per the R246 uniform-path ratification: every machine
    /// declaration produces at least one entry — monomorphic machines
    /// (no type params) get a single entry with empty `type_args`,
    /// generic machines get one entry per concrete `(type_args,
    /// const_args)` reach-through observed in the substituted-body
    /// walk (annotations, struct-state inits, ctor forms,
    /// spawn-target machines).
    ///
    /// Insertion-ordered for deterministic codegen. Downstream MIR
    /// (W3.033c Stage 3) and codegen (Stage 4) iterate this list to
    /// emit one `MachineLayout` per entry under the mangled name.
    ///
    /// Empty when the program contains no machine declarations.
    /// `const_args` is empty on every entry until W3.039 Stage 3
    /// populates the slot with constexpr-evaluated values.
    ///
    /// LESSONS: `end-to-end-before-layer-thickening` (P1),
    /// `checker-authority` (P0).
    pub machine_instantiations: Vec<MachineMonoEntry>,
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
    /// LESSONS: `checker-authority` (P0), `end-to-end-before-layer-thickening` (P1).
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
    /// Lowered `impl [Trait for] TargetType { type X = ...; fn ... }` block.
    ///
    /// Methods are *also* emitted as separate [`HirItem::Function`] entries in
    /// the same module under qualified names (`<self_type_name>::<method>`),
    /// mirroring the pre-existing `impl Index` lowering precedent. This
    /// variant carries the structural metadata (trait/self pair, associated
    /// types) so that checker-side reasoning and future call-site rewrites
    /// can find it without re-parsing.
    Impl(HirImplBlock),
    /// Lowered `extern "<abi>" { fn name(params) -> ret; ... }` declaration.
    ///
    /// One [`HirItem::ExternFn`] is emitted per [`hew_parser::ast::ExternFnDecl`]
    /// inside the surface `Item::ExternBlock`. Distinct from [`HirItem::Function`]
    /// because extern fns have no body — the symbol is satisfied at link time by
    /// the runtime or a sibling stdlib staticlib. Downstream MIR includes the
    /// extern name in `module_fn_names` so `Expr::Call` to it dispatches as
    /// `Terminator::Call`; codegen predeclares the symbol with external linkage
    /// before user functions are declared so the `Terminator::Call` lookup
    /// resolves transparently.
    ExternFn(HirExternFn),
    /// Lowered module-level `const NAME: T = <expr>;` declaration.
    ///
    /// The initializer expression is constant-folded during HIR lowering into
    /// a concrete [`HirConstValue`] (integer or string). A reference to the
    /// const elsewhere in the module lowers to
    /// `HirExprKind::BindingRef { resolved: ResolvedRef::Const(id), .. }`,
    /// where `id` matches this item's [`HirConst::id`]; MIR/codegen resolve
    /// that id back to the descriptor.
    ///
    /// WHY fold in HIR: there is no checker-authored const-value table on
    /// `TypeCheckOutput` and the checker's `ConstValue` is `pub(super)` to
    /// hew-types and only recognises bare literals. The fold here is a
    /// self-contained, fail-closed evaluator over the parser AST (integer
    /// arithmetic + string literals). Any non-foldable initializer emits a
    /// `HirDiagnosticKind::NotYetImplemented` rather than producing a silent
    /// or fabricated value (LESSONS: fail-closed producers).
    Const(HirConst),
}

/// Constant-folded value carried by a [`HirConst`]. v0.5 admits integer,
/// float, and string constants; booleans and aggregate consts are out of
/// scope and fail closed at fold time.
#[derive(Debug, Clone, PartialEq)]
pub enum HirConstValue {
    /// Folded integer value. The declared width lives in [`HirConst::ty`]
    /// (`I32`/`I64`); this carries the value as `i64` and downstream codegen
    /// truncates/zero-extends to the declared width.
    Integer(i64),
    /// Folded string literal value (UTF-8, as written in source).
    String(String),
    /// Folded float literal value. The declared width lives in [`HirConst::ty`]
    /// (`F32`/`F64`); this carries the parser value as `f64`, and downstream
    /// codegen rounds to `f32` when the declared type is `f32`.
    Float(f64),
}

/// Lowered module-level constant declaration — see [`HirItem::Const`].
#[derive(Debug, Clone, PartialEq)]
pub struct HirConst {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    /// Declared, checker-resolved type of the const (`i64`, `String`, ...).
    pub ty: ResolvedTy,
    /// Constant-folded initializer value.
    pub value: HirConstValue,
    pub span: Span,
}

/// Lowered extern function declaration — see [`HirItem::ExternFn`].
#[derive(Debug, Clone, PartialEq)]
pub struct HirExternFn {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    /// `"rt"`, `"C"`, etc. The checker validates `"rt"` against the JIT-stable
    /// symbol allowlist; other ABIs pass through unchanged.
    pub abi: String,
    /// Parameter types in declaration order. Names are dropped — extern fns
    /// have no body, so there is no scope to bind names into.
    pub param_tys: Vec<ResolvedTy>,
    pub return_ty: ResolvedTy,
    pub span: Span,
}

/// Lowered impl block — see [`HirItem::Impl`] for the metadata-versus-method
/// split. `type_aliases` is checker-only metadata (no runtime artifact); it
/// records each `type Item = ...;` projection on the impl.
#[derive(Debug, Clone, PartialEq)]
pub struct HirImplBlock {
    pub id: ItemId,
    pub node: HirNodeId,
    /// `Some("Iterator")` for trait impls, `None` for inherent impls.
    pub trait_name: Option<String>,
    /// User nominal target, e.g. `"VecIter"`. Builtin nominals are rejected
    /// at lowering time with `ImplBlockShapeNotLowered`.
    pub self_type_name: String,
    /// Outer type parameters on the impl, e.g. `["T"]` for
    /// `impl<T> Iterator for VecIter<T>`.
    pub type_params: Vec<String>,
    /// Associated-type bindings declared on the impl
    /// (e.g. `type Item = T;` → `("Item", ResolvedTy::TypeParam("T"))`).
    pub type_aliases: Vec<(String, ResolvedTy)>,
    /// Names of the per-method symbols emitted as separate
    /// `HirItem::Function` entries (`<self_type_name>::<method>`). Order
    /// matches `impl_decl.methods`.
    pub method_symbols: Vec<String>,
    /// Surface method names (e.g. `"show"`), parallel to `method_symbols`
    /// in length and order. Carried as structured metadata so static-dispatch
    /// resolution can look up `(declaring_trait, self_type_name, method_name)
    /// → method_symbol` without reverse-parsing the flattened symbol.
    pub method_names: Vec<String>,
    pub span: hew_parser::ast::Span,
}

impl HirImplBlock {
    /// Build the qualified method symbol used by both the `Impl` metadata
    /// (in `method_symbols`) and the corresponding flattened
    /// `HirItem::Function` entry (in `HirFn::name`). Centralised so call-site
    /// dispatch can re-derive the symbol from the receiver type without
    /// scanning the impl-block table.
    #[must_use]
    pub fn method_symbol(self_type_name: &str, method_name: &str) -> String {
        format!("{self_type_name}::{method_name}")
    }
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
    /// Defining-module identity of this actor declaration.
    ///
    /// `None` means the actor's identity is the root program namespace: actors
    /// declared in the root file, and actors spliced into the root item list
    /// by a file-path import (`import "x.hew";`) — file-import splicing keeps
    /// those root-identical, mirroring the module-origin provenance discipline
    /// used for file-import impl dedup. `Some(module_short)` means the actor
    /// is exported by a package module (`import bank;` → `Some("bank")`),
    /// extending the `(defining-module, name)` identity model that
    /// per-module type identity established for `pub type`s to actors.
    ///
    /// Carried so MIR layout keys and codegen symbol synthesis can tell two
    /// same-named actors from different modules apart. [`Self::qualified_name`]
    /// derives the dotted registry key; root actors qualify to their bare
    /// name, so single-module programs see byte-identical symbols.
    pub defining_module: Option<String>,
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

impl HirActorDecl {
    /// Dotted qualified identity key for this actor: `bank.Account` for an
    /// actor exported by package module `bank`, the bare `Account` for a
    /// root-program actor (`defining_module == None`).
    ///
    /// This is the registry-key form (matching the checker's qualified
    /// `type_defs` keys); native symbols derive from it through
    /// [`crate::mangle_dotted_name`], which maps the root/bare form to itself
    /// — so qualifying is a no-op for single-module programs by construction.
    #[must_use]
    pub fn qualified_name(&self) -> String {
        match &self.defining_module {
            Some(module_short) => format!("{module_short}.{}", self.name),
            None => self.name.clone(),
        }
    }
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

/// Provenance for a single machine-level trait bound: distinguishes
/// inline `<T: Trait>` bounds from `where T: Trait` predicates.
///
/// The checker-side bound enforcement does not branch on origin —
/// a bound is satisfied at the instantiation site iff the substituted
/// type implements the trait, regardless of where the predicate was
/// authored. Origin is preserved for downstream diagnostics that
/// want to point at the predicate's source span (e.g. "the bound on
/// `T` declared on line 12 is not satisfied"). For inline bounds the
/// span is implicit in the surrounding `HirMachineDecl.span`; for
/// where-clause bounds the span carries the LHS type's span lifted
/// from `WherePredicate.ty`.
#[derive(Debug, Clone, PartialEq)]
pub enum WhereOrigin {
    /// The bound was authored inline in the type-parameter list,
    /// e.g. `machine Holder<T: Resource>`.
    Inline,
    /// The bound was authored in a `where` clause, e.g.
    /// `machine Holder<T> where T: Resource`. The carried span is the
    /// `WherePredicate.ty` span (the LHS of the predicate).
    WhereClause(Span),
}

/// One `(param, trait_bound)` entry from a machine's combined inline
/// `<T: Trait>` and `where T: Trait` predicates.
///
/// `param` is the bare type-parameter name (e.g. `"T"`); `trait_bound`
/// reuses the cross-layer `ResolvedTraitBound` carrier from `hew-types`
/// rather than forking a parallel HIR shape. Per Q230-B the carrier
/// is a flat `Vec<HirMachineBound>`, one entry per `(param, trait)` pair —
/// not a `HashMap<param, Vec<bounds>>` — so iteration order is the
/// authored order and duplicate `(param, trait)` pairs from inline+where
/// dedup can be observed by downstream consumers if they care to.
#[derive(Debug, Clone, PartialEq)]
pub struct HirMachineBound {
    /// The bound type-parameter name.
    pub param: String,
    /// The trait constraint.
    pub trait_bound: hew_types::ResolvedTraitBound,
    /// Where the predicate was authored.
    pub origin: WhereOrigin,
}

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
    /// Trait bounds declared on the machine's type parameters, drawn from
    /// both the inline `<T: Trait>` form and the trailing `where T: Trait`
    /// clause. One entry per `(param, trait_bound)` pair in authored order;
    /// `WhereOrigin` preserves which form each entry came from for
    /// diagnostic span recovery.
    ///
    /// Empty when the machine declares no type-param bounds. The checker's
    /// canonical bound-enforcement seam consults a separate, dedup'd side
    /// table during type checking; this HIR field exists so post-checker
    /// passes (static trait dispatch, const-generic substrate, future
    /// const-arg validation) have a structured carrier to read instead of
    /// re-walking the parser AST.
    pub type_param_bounds: Vec<HirMachineBound>,
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
    /// Lowered transition guard expression (`on E: A -> B when <expr> { ... }`).
    /// `None` when the transition has no guard. Lowered through the same
    /// machine-body allowlist + implicit-binding scope as the transition body
    /// (`lower_machine_expr_filtered`) so the guard sees `self`, the source-
    /// state binding, and the event-field aliases.
    ///
    /// Carrying the guard expression in HIR (rather than the prior
    /// `has_guard: bool`) is required for every machine-body walker —
    /// call-shape gates, blocking-recv gates, effect-parity checks — to
    /// actually visit guard sub-expressions instead of silently treating
    /// guarded transitions as if the guard position were empty.
    pub guard: Option<HirExpr>,
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
    /// Defining-module identity of this record declaration.
    ///
    /// `None` means root-program identity (root-file or file-import records);
    /// `Some(module_short)` means a package module exports it. Carried for the
    /// same `(defining-module, name)` reason as [`HirTypeDecl::defining_module`]
    /// and [`HirActorDecl::defining_module`]: MIR layout keys and codegen
    /// symbols must tell two same-named records from different modules apart.
    /// [`Self::qualified_name`] derives the dotted registry key; root records
    /// qualify to their bare name. The decl `name` stays bare in both cases.
    pub defining_module: Option<String>,
    pub type_params: Vec<String>,
    pub fields: Vec<HirField>,
    pub span: Span,
}

impl HirRecordDecl {
    /// Dotted qualified identity key for this record: `bank.Widget` for a
    /// record exported by package module `bank`, the bare `Widget` for a
    /// root-program record (`defining_module == None`).
    ///
    /// Matches the checker's qualified `type_defs` keys and mangles through
    /// [`crate::mangle_dotted_name`], which maps the bare/root form to itself
    /// — so single-module programs are byte-identical by construction.
    #[must_use]
    pub fn qualified_name(&self) -> String {
        match &self.defining_module {
            Some(module_short) => format!("{module_short}.{}", self.name),
            None => self.name.clone(),
        }
    }
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
    /// Named init args from the child declaration, e.g. `child w: Worker(id: 7)`.
    ///
    /// Each entry is `(field_name, expr)`, mirroring `HirSpawnExpr.args`.
    /// Empty when no `(...)` clause appears on the child declaration.
    /// MIR lowering reads these to build `SupervisorChildLayout.init_state_fields`
    /// so codegen can construct the per-child state template.
    pub init_args: Vec<(String, HirExpr)>,
    /// Per-child graceful-stop directive from the `shutdown:` clause.
    /// `None` means the supervisor default applies.
    pub shutdown: Option<HirShutdownDirective>,
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

/// Per-child shutdown directive lowered from the `shutdown:` clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirShutdownDirective {
    /// Graceful-stop deadline as a raw duration source string (e.g. `"30s"`);
    /// codegen interprets the unit.
    Timeout(String),
    /// Skip the deadline; kill immediately.
    BrutalKill,
    /// Wait indefinitely. ACCEPTED-ONLY in v0.5 — there is no per-child
    /// deadline wheel in the runtime yet, so codegen does not enforce it.
    Infinity,
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
    /// Defining-module identity of this type declaration.
    ///
    /// `None` means the type's identity is the root program namespace: types
    /// declared in the root file, and types spliced into the root item list by
    /// a file-path import (`import "x.hew";`) — file-import splicing keeps
    /// those root-identical. `Some(module_short)` means the type is exported by
    /// a package module (`import bank;` → `Some("bank")`).
    ///
    /// Carried so MIR layout keys and codegen symbol synthesis can tell two
    /// same-named types from different modules apart — the same
    /// `(defining-module, name)` identity model [`HirActorDecl`] already uses
    /// for actors. [`Self::qualified_name`] derives the dotted registry key;
    /// root types qualify to their bare name, so single-module programs see
    /// byte-identical layouts and symbols. The decl `name` stays bare in both
    /// cases — qualification lives in the carrier, not the name.
    pub defining_module: Option<String>,
    pub marker: ResourceMarker,
    /// `#[opaque]` — this type is a pointer-width opaque runtime handle.
    /// Distinct from `marker` (which carries `BitCopy` for opaque-only
    /// handles): downstream stages (construction gate, MIR ptr-layout) need
    /// to recognise opacity specifically, since `BitCopy` is also used by
    /// non-opaque substrate records.
    pub is_opaque: bool,
    /// `indirect enum` — this enum is heap-allocated; every variable of this
    /// type holds a pointer to a heap-allocated tagged-union struct. Enables
    /// recursive data types (the enum value itself is a pointer so its size
    /// is finite even when variants reference the type recursively).
    ///
    /// Downstream stages: MIR adds the name to `opaque_handle_names` so
    /// codegen emits `ptr`-typed alloca slots and routes all
    /// `Place::EnumTag` / `Place::EnumVariant` accesses through a pointer
    /// dereference. Construction emits `hew_alloc` + write + ptr-store;
    /// drop emits `hew_dealloc` after recursively freeing sub-trees.
    pub is_indirect: bool,
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

impl HirTypeDecl {
    /// Dotted qualified identity key for this type: `bank.Widget` for a type
    /// exported by package module `bank`, the bare `Widget` for a root-program
    /// type (`defining_module == None`).
    ///
    /// This is the registry-key form (matching the checker's qualified
    /// `type_defs` keys); native symbols derive from it through
    /// [`crate::mangle_dotted_name`], which maps the root/bare form to itself
    /// — so qualifying is a no-op for single-module programs by construction.
    #[must_use]
    pub fn qualified_name(&self) -> String {
        match &self.defining_module {
            Some(module_short) => format!("{module_short}.{}", self.name),
            None => self.name.clone(),
        }
    }
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
    pub default: Option<HirExpr>,
    /// Declared mutability of the field.
    ///
    /// Meaningful for `HirActorDecl::state_fields`, where it carries the
    /// surface `var` (true) vs `let`/bare (false) declaration so downstream
    /// consumers see the same mutability the checker enforced (immutable
    /// fields are assignable only inside `init { }`). Other constructors
    /// pass the context's effective writability: machine state fields are
    /// `true` (written by transition bodies); record, type-decl, and machine
    /// event fields are `false` (writes are governed by the binding root,
    /// not the field).
    pub is_mutable: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFn {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<HirBinding>,
    /// For ordinary functions this is the declared return type. For generator
    /// functions (`is_generator`) this remains the declared `-> T` yield element
    /// type; the body itself lowers with unit expectation and produces a
    /// `Generator<Yield, Return>` value (mirrors `HirActorReceiveFn`).
    pub return_ty: ResolvedTy,
    pub body: HirBlock,
    pub span: Span,
    /// `true` when this function was declared `gen fn`. The body falls off the
    /// end (unit-expectation) and `yield` expressions inside it bind to the
    /// `return_ty` yield element type. Threaded HIR → MIR → codegen so the
    /// generator construction seam (`hew_gen_ctx_create`) materializes a
    /// generator value at the call site (mirrors `HirActorReceiveFn`).
    pub is_generator: bool,
    /// When `Some(catalog_key)`, this function is a `#[intrinsic("key")]`
    /// floor declaration (W5.005 / F1b) whose source body is a bodyless
    /// placeholder. The lowered `body` (and the MIR derived from it) is NOT
    /// the source of truth — codegen synthesizes the trampoline body from
    /// the catalog key. The key is threaded HIR → MIR
    /// (`RawMirFunction::intrinsic_id`) → codegen (`lower_fn`), where a
    /// central authority dispatches it to a real lowering. Fail-closed
    /// (D343): an id codegen does not recognise is a hard `CodegenError`,
    /// never a silent empty-body no-op.
    ///
    /// Only the callable memory-intrinsic floor (`mem.*`, catalog linkage
    /// `CalleeNameDispatchOnly`) is tagged here. Numeric `math.*` intrinsics
    /// (linkage `CompilerIntrinsic`) route through builtin method-rewrites
    /// and are never emitted as a `HirItem::Function`, so they never carry
    /// an `intrinsic_id`.
    pub intrinsic_id: Option<String>,
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
    /// `defer <body>` — register `body` for execution at the exit of the
    /// enclosing lexical scope. Q205-B semantics: bindings are resolved by
    /// lexical reference at execution time (mutable vars observe their final
    /// value); moved/consumed bindings are a compile-time error.
    ///
    /// `scope_id` identifies the HIR scope that owns this defer — the
    /// scope whose exit triggers execution. MIR lowering collects all
    /// defers per scope and materialises them in LIFO order at every
    /// exit from that scope.
    Defer {
        body: Box<HirExpr>,
        scope_id: ScopeId,
    },
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
pub enum HirVarSelfMethodTarget {
    Direct {
        callee: String,
    },
    StaticTrait {
        /// Type-parameter name that carries the bound (e.g. "T").
        receiver_type_param: String,
        /// The bound trait through which the method was reached.
        bound_trait: String,
        /// The trait that directly declares the method (canonical identity for impl lookup).
        declaring_trait: String,
        /// Method name within the declaring trait.
        method_name: String,
    },
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
    /// Checker-authoritative unary expression (`!`, `-`, `~`).
    ///
    /// Produced only when both operand and result spans have resolved entries in
    /// `TypeCheckOutput::expr_types`. The parent [`HirExpr::ty`] carries the
    /// result type; `operand_ty` records the checker-owned operand type so MIR
    /// can select the unary instruction without re-deriving from syntax.
    Unary {
        op: UnaryOp,
        operand: Box<HirExpr>,
        operand_ty: ResolvedTy,
    },
    /// Explicit numeric `as` cast admitted by the checker.
    ///
    /// `from_ty` and `to_ty` carry the checker/HIR boundary types used by MIR
    /// and codegen to choose truncation, extension, and int/float conversion
    /// instructions. Non-numeric casts never construct this node.
    NumericCast {
        value: Box<HirExpr>,
        from_ty: ResolvedTy,
        to_ty: ResolvedTy,
    },
    /// Tuple literal construction (`(1, 2)`, `(a, b, c)`).
    ///
    /// Produced from `Expr::Tuple(elems)` when the checker has populated
    /// `expr_types` for the tuple expression. The parent [`HirExpr::ty`] is
    /// the tuple type (e.g. `ResolvedTy::Tuple(vec![I64, Bool])`); each
    /// element in `elements` is a fully-lowered HIR expression whose type is
    /// already validated by the checker.
    ///
    /// MIR lowering emits `Instr::TupleConstruct` — a stack-local allocation
    /// of the tuple struct followed by per-element stores. Codegen maps the
    /// tuple type to an unnamed LLVM struct with positional fields.
    TupleLiteral {
        elements: Vec<HirExpr>,
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
        /// NEW-6b `await <actor>.<method>(...) | after d` deadline, in nanoseconds.
        /// `Some(ns)` attaches a fail-closed timeout to the suspending ask: when the
        /// deadline elapses before the reply, the in-flight ask is cancelled and the
        /// `Result<R, AskError>` resolves to `Err(AskError::Timeout)`. `None` is a
        /// plain ask. Only literal `Duration` deadlines are carried (codegen-locals
        /// side-table); non-literal durations fail closed at CHECK time.
        deadline_ns: Option<i64>,
    },
    /// Cross-node request/reply dispatch on `RemotePid<T>::ask(msg, timeout_ms)`.
    ///
    /// The expression type is the full `Result<T::Reply, AskError>`; `reply_ty`
    /// carries the decoded Ok payload type for MIR/codegen reply sizing.
    RemoteActorAsk {
        receiver: Box<HirExpr>,
        msg: Box<HirExpr>,
        timeout_ms: Box<HirExpr>,
        reply_ty: ResolvedTy,
    },
    /// `this` inside an actor `receive fn` — the actor's own handle.
    ///
    /// A zero-payload leaf: the `LocalPid<Self>` type recorded by the checker
    /// at the `this` span (`Expr::This` synthesis) rides on the wrapping
    /// `HirExpr.ty`, so this variant carries no fields. MIR lowers it via the
    /// `hew_actor_self()` runtime primitive — the same self-handle synthesis
    /// `link`/`monitor`/`unlink` already use implicitly — yielding the borrowed
    /// `*mut HewActor` the current actor runs on. A self-send (`this.go()`)
    /// flows through the existing `ActorSend` machinery once the receiver
    /// lowers to this handle.
    ActorSelf,
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
    /// `await conn.read()` / `await conn.read_string()` — a non-blocking
    /// suspending socket read (NEW-1). Produced by HIR lowering when an `await`
    /// wraps a `net.Connection::read`/`read_string` method call. A suspendable
    /// caller (actor handler / closure / task entry) lowers this to
    /// `Terminator::SuspendingRead` (suspend, free the worker, resume with the
    /// bytes); a `Default` caller keeps the blocking `hew_tcp_read` call.
    ///
    /// `read_string` is `await conn.read()` + `hew_bytes_to_string`, so the HIR
    /// node carries only the bytes read; the string conversion wraps it. A raw
    /// `read()` may carry a literal deadline and then resolves to
    /// `Result<bytes, IoError>` with MIR/codegen binding `Err(TimedOut)` if the
    /// timer wins.
    ConnAwaitRead {
        /// The connection receiver expression (`conn`).
        conn: Box<HirExpr>,
        /// `true` when the source was `read_string()` (the bytes are converted
        /// to a string after the suspending read); `false` for raw `read()`.
        to_string: bool,
        /// NEW-6c `await conn.read() | after d` deadline, in nanoseconds. `None`
        /// preserves the plain-read bytes result and unconditional read-slot wake.
        deadline_ns: Option<i64>,
    },
    /// `await listener.accept()` — a non-blocking suspending listener accept
    /// (NEW-2). Produced by HIR lowering when an `await` wraps a
    /// `net.Listener::accept` method call. A suspendable caller (actor handler /
    /// closure / task entry) lowers this to `Terminator::SuspendingAccept`
    /// (suspend, free the worker, resume with the accepted `Connection`); a
    /// `Default` caller keeps the blocking `hew_tcp_accept` call. The
    /// listener-readiness sibling of [`HirExprKind::ConnAwaitRead`].
    ListenerAwaitAccept {
        /// The listener receiver expression (`listener`).
        listener: Box<HirExpr>,
        /// NEW-6d `await ln.accept() | after d` deadline, in nanoseconds. `None`
        /// preserves the plain-accept `Connection` result and unconditional read-slot
        /// wake. `Some(ns)` produces `Result<Connection, IoError>` with
        /// `IoError::TimedOut` on the deadline arm — parallel to `ConnAwaitRead`.
        deadline_ns: Option<i64>,
    },
    /// `await rx.recv() | after d` — a suspending channel recv with a deadline
    /// (NEW-6b).  Produced by [`super::lower::lower_await_deadline`] when the
    /// inner expression is a `Receiver<T>::recv()` call.  A suspendable caller
    /// lowers this to `Terminator::SuspendingChannelRecv` with a live
    /// `deadline_result_dest`; a `Default` caller fails closed at MIR time.
    ///
    /// `HirExpr::ty` is `Result<Option<T>, TimeoutError>` (set by the
    /// deadline lowering path).
    ChannelRecvAwait {
        /// The channel receiver expression (`rx`).
        receiver: Box<HirExpr>,
        /// Deadline in nanoseconds (always `Some` when this kind is produced;
        /// present as `Option<i64>` to share the same lowering interface as the
        /// other deadline kinds).
        deadline_ns: Option<i64>,
    },
    /// `await stream.recv() | after d` — a suspending stream recv with a
    /// deadline (NEW-6b).  Produced by [`super::lower::lower_await_deadline`]
    /// when the inner expression is a `Stream<T>::recv()` call.  Symmetric
    /// with [`HirExprKind::ChannelRecvAwait`].
    ///
    /// `HirExpr::ty` is `Result<Option<T>, TimeoutError>`.
    StreamRecvAwait {
        /// The stream handle expression.
        stream: Box<HirExpr>,
        /// Deadline in nanoseconds.
        deadline_ns: Option<i64>,
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
    /// `join { a.m(...), b.n(...) }` — the wait-ALL sibling of `select`.
    ///
    /// Every branch is an actor-ask issued concurrently; the construct
    /// waits for ALL branches to reply and binds a tuple of the per-branch
    /// reply values in declaration order. The construct's static type
    /// (`HirExpr::ty`) is the tuple of branch reply types (or the single
    /// reply type when there is exactly one branch), as the checker
    /// records it via `expr_types`.
    ///
    /// Per HEW-SPEC-2026 §4.11.2 a branch trap cancels the remaining
    /// branches and the trap propagates to the enclosing scope. Any branch
    /// that is not an actor method call is rejected at lowering with
    /// `JoinBranchNotActorAsk`.
    Join(HirJoin),
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
        /// Conservative escape classification recorded by the checker's
        /// `closure_escape_facts` side-table. Consumed by MIR's
        /// `ClosureEnvLayout::allocation_strategy` to dispatch between
        /// stack-allocated (`Local`), heap-boxed (`Escapes`), and
        /// scope-owned (`Forked`) environment storage.
        escape_kind: hew_types::ClosureEscapeKind,
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
    /// Produced by tuple-let lowering (`let (a, b) = …`) and by surface numeric
    /// field access on tuple-typed expressions (`t.0`, `t.1`). `index` is the
    /// zero-based element position.
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
        /// Caller-side method signature after the checker substituted
        /// trait type parameters and associated-type bindings from the
        /// receiver's `Ty::TraitObject` bound (e.g. `Self::Item -> int`).
        /// Mirrors [`hew_types::DynMethodCall::signature`]; MIR lowering
        /// clones it onto `Instr::CallTraitMethod.signature` so codegen
        /// (W3.031 Stage 7) can derive the erased indirect-call type
        /// without re-resolving the trait/method. Receiver parameter is
        /// already filtered out.
        ///
        /// Boxed to keep the `HirExprKind` variant under the
        /// `clippy::large_enum_variant` threshold.
        signature: Box<hew_types::FnSig>,
    },
    /// Static trait dispatch: the method was resolved at type-check time from
    /// trait bounds on a generic type parameter. Unlike `CallDynMethod` (vtable
    /// dispatch), this resolves to a direct call after monomorphization — the
    /// concrete impl function is determined from the substituted receiver type.
    ///
    /// Produced from `MethodCallRewrite::StaticTraitDispatch`. MIR lowering
    /// substitutes `receiver_type_param` via the monomorphization map, looks up
    /// the impl method via `(concrete_receiver_ty, declaring_trait, method_name)`,
    /// and emits an ordinary `Terminator::Call`.
    #[deprecated(
        note = "scheduled for removal once generic builtin dispatch (HashMap/HashSet/Vec \
                migrated; Option/Result still pending) is fully migrated to \
                `HirExprKind::ResolvedImplCall` which consumes \
                `TypeCheckOutput::resolved_calls` (the structured \
                `(ImplId, MethodTarget)` registry) directly. New construction sites are \
                forbidden — see `call_trait_method_static_creation_allowlist` test."
    )]
    CallTraitMethodStatic {
        receiver: Box<HirExpr>,
        /// Type-parameter name that carries the bound (e.g. "T").
        receiver_type_param: String,
        /// The bound trait through which the method was reached.
        bound_trait: String,
        /// The trait that directly declares the method (canonical identity for impl lookup).
        declaring_trait: String,
        /// Method name within the declaring trait.
        method_name: String,
        args: Vec<HirExpr>,
        ret_ty: ResolvedTy,
    },
    /// Var-self method dispatch using the Slice-1 dual-return ABI.
    ///
    /// The lowered callee returns `(method_result, Self)`. MIR consumes the
    /// receiver binding, calls the selected method, stores tuple field 1 back
    /// into the original binding slot, marks the binding live again for the
    /// move-checker, and yields tuple field 0 as the expression value.
    ///
    /// Produced only for method signatures whose checker-owned signature has
    /// `requires_mutable_receiver`; ordinary methods continue through `Call`
    /// / `CallTraitMethodStatic`, and builtin resolved impl calls remain in the
    /// closed `ResolvedImplCall` arm.
    VarSelfMethodCall {
        receiver: Box<HirExpr>,
        target: HirVarSelfMethodTarget,
        args: Vec<HirExpr>,
        ret_ty: ResolvedTy,
        receiver_ty: ResolvedTy,
    },
    /// Builtin-generic trait dispatch resolved at type-check time via the
    /// structured impl registry (`hew_types::check::dispatch::ImplRegistry`).
    ///
    /// Produced by `lower_method_call` when the checker's
    /// `TypeCheckOutput::resolved_calls` side-table records a
    /// [`hew_types::ResolvedCall`] for the call site.
    ///
    /// Carries the resolver's verdict verbatim: the opaque
    /// [`hew_types::ImplId`] of the satisfying impl, the method name within
    /// that impl, the concrete `type_args` resolved at the call site (as
    /// [`hew_types::TyPattern`] — the resolver's data-only descriptor type),
    /// the lowered argument list, and the checker-recorded return type.
    ///
    /// **Substrate status**: the variant is wired through every exhaustive
    /// `HirExprKind` traversal (verify, dump, capture walks, machine-mono,
    /// MIR closure-env suspend walk, etc.) so dataflow is preserved. MIR
    /// `lower_value` lowers it to a `Terminator::Call { callee:
    /// target_symbol, args: [receiver, lowered_args...], ... }` against the
    /// kernel `hew_hashmap_*_layout` / `hew_hashset_*_layout` exports
    /// declared by the C0b catalog and seeded into `module_fn_names`.
    /// This is the production path for builtin generic dispatch of
    /// HashMap/HashSet/Vec today; Option/Result migrate later.
    ///
    /// LESSONS: `checker-authority` (P0) — `impl_id` + `target_symbol`
    /// come straight from the resolver's verdict; HIR never re-derives them.
    ResolvedImplCall {
        receiver: Box<HirExpr>,
        /// Opaque identity of the satisfying impl in the checker's
        /// `ImplRegistry`. Carried for downstream attribution (diagnostics,
        /// drop-plan keying when Stage E lands); not consumed by today's
        /// MIR lowering, which dispatches on `target_family` directly.
        impl_id: ImplId,
        /// Method name within `ImplDef::methods` (e.g. `"insert"`).
        method_name: String,
        /// Runtime symbol name from `MethodTarget.symbol_name` (e.g.
        /// `"hew_hashmap_insert_layout"`). Stashed verbatim from the
        /// resolver's verdict and used as the **linker-edge** identifier
        /// in `Terminator::Call`. Routing decisions (`HashMap` vs
        /// `HashSet` vs Vec, push vs other Vec method) read
        /// [`target_family`] instead — the string is no longer a
        /// dispatch authority. Resolves at link time to the
        /// `#[no_mangle] pub extern "C"` export in `hew-runtime`.
        target_symbol: String,
        /// Typed cross-layer dispatch identity, copied from
        /// `MethodTarget.family`. MIR `lower_resolved_impl_call` uses
        /// this to choose the right arity gate and the right runtime
        /// rewrite without re-parsing `target_symbol`. A future commit
        /// retires `target_symbol` from the variant entirely once
        /// codegen consumes the family directly.
        target_family: MethodTargetFamily,
        /// Concrete type-arguments resolved at the call site, in
        /// first-occurrence order of `TyPattern::Var`s in
        /// `ImplDef::self_pattern`. Mirrors
        /// [`hew_types::ResolvedCall::type_args`]. Consumed by codegen-side
        /// catalog-presence checks (every K/V class the resolver accepts
        /// must have a matching `BuiltinLinkage::LayoutDescriptorSymbol`
        /// row in `stdlib_catalog`).
        type_args: Vec<TyPattern>,
        /// Lowered call arguments (without the receiver). MIR prepends
        /// the receiver as arg[0] when constructing `Terminator::Call`.
        args: Vec<HirExpr>,
        /// Checker-recorded return type for the call site. Read from
        /// `TypeCheckOutput::expr_types` at lowering time; carried here so
        /// downstream consumers do not have to plumb `expr_types` separately.
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
    /// `CancellationToken.is_cancelled() -> bool`.
    ///
    /// The checker records this as a structured intrinsic so frontend lowering
    /// carries the token value; codegen consumes the variant directly and emits
    /// the borrowing `hew_cancel_token_is_requested` runtime call.
    CancellationTokenIsCancelled {
        receiver: Box<HirExpr>,
    },
    /// `Generator<Y, R>.next() -> Option<Y>`.
    ///
    /// The checker records this as a structured rewrite so frontend lowering
    /// carries the borrowed generator handle and the yield element type; MIR
    /// lowers it to `Instr::GeneratorNext` and codegen emits
    /// `hew_gen_next(ctx, &out_size)` and unboxes the returned heap pointer into
    /// `Option<yield_ty>` (null → `None`, else load + `Some` + free the
    /// payload). The receiver is borrowed; the handle is freed by
    /// `hew_gen_free` on its own scope-exit drop.
    GeneratorNext {
        receiver: Box<HirExpr>,
        yield_ty: ResolvedTy,
    },
    /// Binary wire codec call on a `#[wire]` struct.
    ///
    /// `Encode`: `value.encode() -> bytes` — `operand` is the receiver value;
    /// codegen calls `__hew_serialize_<key>(value_ptr, &out_len)` and wraps the
    /// returned malloc'd buffer into a refcounted `bytes` value.
    ///
    /// `Decode`: `Type.decode(bytes) -> Type` — `operand` is the `bytes`
    /// argument; codegen calls `__hew_deserialize_<key>(data, len, &struct_size)`
    /// and loads the reconstructed value out of the malloc'd result.
    ///
    /// `value_ty` is the wire-struct type. Codegen derives the thunk key via
    /// `mangle_resolved_ty` — the same encoder the actor message path uses — so
    /// a direct `.encode()` and an actor send of the same type share one thunk.
    /// Only the binary `encode`/`decode` methods reach here; the text-format
    /// wire methods stay fail-closed (no thunk exists yet).
    WireCodec {
        direction: WireCodecDirection,
        operand: Box<HirExpr>,
        value_ty: ResolvedTy,
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
    /// Read a payload field from the transition's matched `event` value.
    MachineEventFieldAccess {
        machine_name: String,
        event_idx: usize,
        field_idx: usize,
        field_name: String,
    },
    /// `while cond { body }` / `@label: while cond { body }` — loops until
    /// `cond` evaluates to false.
    ///
    /// The expression type is always `Unit`; a while loop never produces
    /// a value.  MIR lowering emits a header block (condition test) and
    /// a body block with a back-edge to the header.
    While {
        /// Optional source label targeted by `break @label` / `continue @label`.
        label: Option<String>,
        condition: Box<HirExpr>,
        body: HirBlock,
    },
    /// `for binding in start..end { body }` / `@label: for ...` — Range iteration
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
        /// Optional source label targeted by `break @label` / `continue @label`.
        label: Option<String>,
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
        /// Per-iteration stride magnitude (always positive).  Defaults to a
        /// literal `1`; set to `k` by the `(a..b).step_by(k)` adapter.  MIR
        /// lowering advances the counter by this value each iteration (adding
        /// it ascending, subtracting it descending).
        step: Box<HirExpr>,
        /// `true` when the loop iterates from the high bound down to the low
        /// bound — produced by the `(a..b).rev()` adapter.  MIR lowering
        /// initialises the counter at the high bound, decrements by `step`, and
        /// flips the header comparison so the sequence is the forward range's
        /// reverse (e.g. `(0..5).rev()` yields `4 3 2 1 0`).
        descending: bool,
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
    /// `while let <PayloadVariant>(bindings) = scrutinee { body }` /
    /// `@label: while let ...` — loops while the scrutinee's tag matches
    /// `variant_idx`. Each iteration
    /// re-evaluates `scrutinee` from scratch (the surface semantics: the
    /// pattern's right-hand side is checked anew on every iteration), then
    /// either binds the variant's payload fields and runs `body` before
    /// looping back, or exits the loop.
    ///
    /// The expression type is always `Unit`; a while-let loop never yields
    /// a value. MIR lowering emits a four-block CFG (entry → header →
    /// body → exit) mirroring `lower_while` + `lower_match_enum_tag`.
    ///
    /// **Scope (v0.5 substrate)**: only a single payload-bearing enum
    /// constructor pattern (e.g. `Some(x)`) is accepted here. Unit-variant
    /// patterns (`None`), or-patterns, guards, and arbitrary patterns are
    /// rejected at HIR lowering with a structured `NotYetImplemented`
    /// diagnostic — the same shape used by `HirExprKind::Match`.
    WhileLet {
        /// Optional source label targeted by `break @label` / `continue @label`.
        label: Option<String>,
        /// Re-evaluated each iteration. Its `ty` is the resolved enum type
        /// (e.g. `Option<i64>`).
        scrutinee: Box<HirExpr>,
        /// Checker-resolved `(type_name, variant_name)` identity of the
        /// continue-arm variant (`Some` for `Option<T>`).
        variant_match: VariantMatch,
        /// Zero-based variant index in declaration order; matches the
        /// `EnumLayout.variants` ordering used by MIR/codegen.
        variant_idx: u32,
        /// Payload bindings introduced by the pattern. Walked at MIR
        /// lowering to emit `Move { src: Place::MachineVariant }` at body
        /// entry — identical to a `Match` arm's payload handling.
        bindings: Vec<HirMatchArmBinding>,
        /// Loop body.
        body: HirBlock,
    },
    /// `if let PAT = scrutinee { then_body } else { else_body }` — a
    /// conditional that succeeds when the scrutinee matches a single enum
    /// constructor pattern.
    ///
    /// MIR lowering emits a three-block CFG: a tag-check block branching to
    /// `then_bb` or `else_bb`, both converging at a `join_bb`.  The `then_bb`
    /// binds payload fields from `bindings` (identical to `Match` arm entry)
    /// and lowers `body`; `else_bb` lowers `else_body` (or falls through as
    /// Unit if absent).
    ///
    /// The expression type is `result_ty`, which is `Unit` when the `if let`
    /// appears in statement position or when no else branch exists, and the
    /// branch-unified type when used as an expression.
    ///
    /// **Scope (v0.5 substrate)**: same shape restrictions as `WhileLet` —
    /// only a single payload-bearing enum-constructor pattern is accepted.
    IfLet {
        /// Evaluated once. Its `ty` is the resolved enum type.
        scrutinee: Box<HirExpr>,
        /// Checker-resolved `(type_name, variant_name)` identity of the
        /// match-arm variant.
        variant_match: VariantMatch,
        /// Zero-based variant index in declaration order.
        variant_idx: u32,
        /// Payload bindings introduced by the pattern; bound for `body` only.
        bindings: Vec<HirMatchArmBinding>,
        /// Then-branch body (executed when the pattern matches).
        body: HirBlock,
        /// Optional else-branch body; `None` when there is no `else` clause.
        else_body: Option<HirBlock>,
        /// The unified result type of both branches (`Unit` when no else or
        /// in statement position).
        result_ty: ResolvedTy,
    },
    /// `break;` / `break @label;` / `break <value>;` — early exit from the
    /// innermost enclosing loop, or from the nearest enclosing loop carrying
    /// the requested label.
    ///
    /// The expression type is always `Unit`; `break` never produces a value
    /// in the surrounding expression position (loop-as-expression value
    /// return is out of scope for v0.5).
    ///
    /// Labeled forms carry `label: Some(name)` to MIR. The type checker rejects
    /// unknown labels; MIR keeps a defense-in-depth diagnostic if malformed HIR
    /// reaches it.
    ///
    /// `value` carries the operand of `break <value>` so MIR can lower it for
    /// its side effects (and move-checker correctness) before emitting the
    /// jump; the produced Place is discarded — loop-value semantics are a
    /// follow-up.
    Break {
        label: Option<String>,
        value: Option<Box<HirExpr>>,
    },
    /// `continue;` / `continue @label;` — skip to the next iteration of the
    /// innermost enclosing loop, or of the nearest enclosing loop carrying the
    /// requested label, transferring control to that loop's continue target (the
    /// condition/bounds-check header for `while`/`while_let`, the dedicated
    /// increment block for `for`-range, or the body block for a bare `loop`).
    ///
    /// The expression type is always `Unit`. Labeled forms carry
    /// `label: Some(name)` to MIR; the type checker rejects unknown labels.
    Continue {
        label: Option<String>,
    },
    /// Bare `loop { body }` / `@label: loop { body }` — an unconditional loop
    /// with no header condition.
    ///
    /// The expression type is always `Unit`; an infinite loop only ends via a
    /// `break` inside its body. MIR lowering emits a body block with an
    /// unconditional back-edge to itself and an exit block that is the target
    /// of every enclosed `break`. The continue target is the body block.
    Loop {
        /// Optional source label targeted by `break @label` / `continue @label`.
        label: Option<String>,
        body: HirBlock,
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
/// `Literal` is the scalar literal arm for top-level i64 / bool / char matches.
/// `RecordProject` / `TupleProject` are irrefutable aggregate destructures whose
/// bindings are materialised from the scrutinee at arm-body entry.
/// `Regex` is the regex-pattern arm added for string-scrutinee matches.
///
/// MIR lowering (slice 4) dispatches on this enum; codegen (slice 5) drives
/// the predicate to the runtime ABI. Paths that are not yet wired must
/// `todo!("MIR regex predicate — slice 4")` — never silently no-op.
#[derive(Debug, Clone, PartialEq)]
pub enum HirMatchArmPredicate {
    /// `_` wildcard — matches any scrutinee value. At most one per match
    /// expression; the checker enforces this.
    Wildcard,
    /// A plain lowercase-identifier binding arm (e.g. `x => ...`).
    ///
    /// Matches any scrutinee value (like `Wildcard`) and additionally
    /// binds the entire scrutinee value to `name` in the arm body scope.
    /// `ty` is the checker-resolved scrutinee type.
    ///
    /// MIR lowering emits a `Move` from the scrutinee local to a fresh
    /// binding local before running the arm body, then registers
    /// `binding_id → local` in `binding_locals` so guard/body
    /// `BindingRef`s resolve correctly.
    Binding {
        /// Stable binding id allocated during HIR lowering. Identical in
        /// role to an `HirMatchArmBinding::binding` id; MIR registers the
        /// local under this id so guard/body refs resolve correctly.
        binding_id: BindingId,
        /// Surface binding name as written in the pattern.
        name: String,
        /// Checker-resolved scrutinee type for the binding.
        ty: ResolvedTy,
    },
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
    /// A top-level scalar literal pattern arm (e.g. `0`, `true`, `'a'`).
    ///
    /// Integer literal arms are coerced to the scrutinee integer type. `ty` is
    /// the checker-resolved scrutinee/comparand type so MIR can allocate the
    /// constant local at the same integer width as the scrutinee before
    /// emitting `IntCmp Eq`.
    Literal { lit: HirLiteral, ty: ResolvedTy },
    /// A plain record match-arm project pattern (e.g. `Point { x, y }`).
    ///
    /// The resolved scrutinee type is carried so HIR verification can guard the
    /// checker/HIR boundary. MIR still uses the scrutinee expression type as the
    /// source of truth for layout lookup and field projection.
    RecordProject { ty: ResolvedTy },
    /// A tuple match-arm project pattern (e.g. `(a, b)`).
    TupleProject { arity: u32 },
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

/// Literal predicate applied to a constructor payload slot after the outer
/// enum tag matches.
///
/// Stage 1 records these predicates in HIR so the parser/checker/HIR boundary
/// no longer rejects `Some(0)` or `Pair(x, 42)`. MIR lowering remains
/// fail-closed until the follow-on match-destructure stages wire the actual
/// payload comparisons.
#[derive(Debug, Clone, PartialEq)]
pub struct HirPayloadPredicate {
    /// 0-based payload slot within the matched variant.
    pub field_idx: u32,
    /// Literal value that the payload slot must equal.
    pub literal: HirLiteral,
    /// Checker-resolved payload type for diagnostics and future MIR lowering.
    pub ty: ResolvedTy,
}

/// Nested constructor subpattern check on one payload slot of an
/// `EnumVariant` match arm (e.g. the `IoError::NotFound` in
/// `Err(IoError::NotFound)` or the inner `Ok(v)` in `Ok(Ok(v))`).
///
/// MIR lowering evaluates these after the outer tag check and any literal
/// payload predicates: it loads the payload slot into a fresh
/// `payload_ty`-typed local (an unregistered transient alias — never entered
/// into `owned_locals`, so ownership stays with the scrutinee), compares
/// `EnumTag` of that local against `variant_idx`, and branches to the arm's
/// fallthrough target on mismatch. On match, `bindings` are materialised
/// from the nested variant's payload slots (registered exactly like
/// top-level arm bindings) and `nested` children recurse with the transient
/// local as their parent.
#[derive(Debug, Clone, PartialEq)]
pub struct HirPayloadVariantPredicate {
    /// 0-based payload slot within the ENCLOSING variant.
    pub field_idx: u32,
    /// Resolved enum type of that payload slot.
    pub payload_ty: ResolvedTy,
    /// Checker-resolved identity of the nested variant.
    pub variant_match: VariantMatch,
    /// Declaration-order tag index of the nested variant within
    /// `payload_ty`'s layout (same registry the outer arm's
    /// `HirMatchArmPredicate::EnumVariant::variant_idx` uses).
    pub variant_idx: u32,
    /// Bindings into THIS nested variant's payload slots.
    pub bindings: Vec<HirMatchArmBinding>,
    /// Deeper nested constructor subpatterns.
    pub nested: Vec<HirPayloadVariantPredicate>,
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
/// `bindings`. Literal payload subpatterns carry their per-field checks in
/// `payload_predicates`; nested constructor subpatterns carry their
/// recursive checks in `payload_variant_predicates`. Both vectors are empty
/// for plain constructor shapes. Arms with a `guard` expression fire only
/// when the pattern matches AND the guard evaluates to `true`; a `false`
/// guard falls through to the next arm exactly as if the pattern had not
/// matched.
///
/// `body` is the arm's right-hand-side expression. The arm's source span
/// is preserved for diagnostics.
#[derive(Debug, Clone, PartialEq)]
pub struct HirMatchArm {
    /// The matching predicate for this arm.
    pub predicate: HirMatchArmPredicate,
    /// Payload bindings introduced by this arm's constructor pattern.
    pub bindings: Vec<HirMatchArmBinding>,
    /// Literal checks for constructor payload fields, evaluated after the
    /// outer tag check and before the arm body.
    pub payload_predicates: Vec<HirPayloadPredicate>,
    /// Nested constructor checks for constructor payload fields, evaluated
    /// after `payload_predicates` and before bindings/guard/body.
    pub payload_variant_predicates: Vec<HirPayloadVariantPredicate>,
    /// Optional guard expression (`Pattern if <guard> => ...`).
    ///
    /// When `Some`, the arm fires only if the pattern matches AND the guard
    /// evaluates to `true`. MIR lowering emits bindings (if any), evaluates
    /// the guard, and branches: true → body block, false → fallthrough to the
    /// next arm's check block.
    ///
    /// `None` for arms without a guard (the common case).
    pub guard: Option<HirExpr>,
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
    /// Whether the captured type satisfies the checker-owned `Sync` contract.
    /// Plumbed from `ClosureCaptureFact::is_sync`; consumed by
    /// `ClosureEnvLayout::lock_slot_for` in MIR to decide whether a
    /// non-`Sync` `BorrowMut` capture needs an auto-lock slot when the
    /// follow-on auto-lock consumer enables lock injection.
    pub is_sync: bool,
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
    /// `pat from <rx>.recv() => body`. The arm waits for a queued item
    /// on a std/channel `Receiver<T>` (NEW-4); the binding receives
    /// `Option<T>` (`None` on channel close). The element type is carried
    /// by the checker-resolved `Receiver<T>` receiver type; the winner
    /// edge materialises it through the element-layout witness.
    ChannelRecv { receiver: Box<HirExpr> },
    /// `after <duration-expr> => body`. The arm fires when the
    /// deadline elapses; the body runs with no binding.
    AfterTimer { duration: Box<HirExpr> },
}

/// Lowered `join { ... }` expression — the wait-ALL sibling of
/// [`HirSelect`]. Each branch is an actor-ask issued concurrently; the
/// construct waits for every branch to reply and materialises a tuple of
/// the per-branch reply values in declaration order. See
/// [`HirJoinBranch`] for the per-branch shape.
#[derive(Debug, Clone, PartialEq)]
pub struct HirJoin {
    pub branches: Vec<HirJoinBranch>,
}

/// One branch of a `join { ... }` expression: a single actor-ask
/// (`<actor-expr>.<method>(<args>)`). `reply_ty` is the
/// checker-authoritative reply type for this branch, harvested from the
/// `actor_method_dispatch` table exactly as `select`'s `ActorAsk` binding
/// type is. MIR's `Terminator::Join` producer allocates a reply slot per
/// branch and writes the branch's reply into the result tuple's matching
/// element.
#[derive(Debug, Clone, PartialEq)]
pub struct HirJoinBranch {
    pub actor: Box<HirExpr>,
    pub method: String,
    pub args: Vec<HirExpr>,
    pub reply_ty: ResolvedTy,
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
    /// A `bytes` literal — `bytes[0x41, 0x42]` or `b"AB"`.
    ///
    /// Produced by `Expr::ByteArrayLiteral` and `Expr::ByteStringLiteral`
    /// in HIR lowering. The byte sequence is already validated and decoded
    /// by the parser (range 0..=255 for array elements; UTF-8 decode for
    /// byte strings). MIR lowering emits `Instr::BytesLit`.
    Bytes(Vec<u8>),
}
