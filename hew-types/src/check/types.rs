use crate::env::{TypeBindingId, TypeEnv};
use crate::error::TypeError;
use crate::lowering_facts::LoweringFact;
use crate::module_registry::ModuleRegistry;
use crate::resolved_ty::ResolvedTy;
use crate::traits::TraitRegistry;
use crate::ty::{Substitution, Ty, TypeVar};
use hew_parser::ast::{NamingCase, Span, Spanned, TraitBound, TraitMethod, TypeExpr, Visibility};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

/// Uniquely identifies an import declaration within the checker.
///
/// Keying only by `short_name` causes collisions when multiple owning modules
/// each import a module with the same short name: the second registration
/// clobbers the first in `import_spans`, and a use in one owner suppresses
/// the unused-import warning for the other owner.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct ImportKey {
    /// The module that owns the `import` declaration, or `None` for
    /// root-level (no-module-graph) programs.
    pub(super) owner_module: Option<String>,
    /// Short (last-segment) name of the imported module, e.g. `"json"`.
    pub(super) short_name: String,
}

impl ImportKey {
    pub(super) fn new(owner_module: Option<String>, short_name: impl Into<String>) -> Self {
        Self {
            owner_module,
            short_name: short_name.into(),
        }
    }
}
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExecutionContextReader {
    ActorId,
    Supervisor,
    TraceSpan,
}

impl ExecutionContextReader {
    #[must_use]
    pub fn from_surface_name(name: &str) -> Option<Self> {
        match name {
            "@actor_id" => Some(Self::ActorId),
            "@supervisor" => Some(Self::Supervisor),
            "@trace_span" => Some(Self::TraceSpan),
            _ => None,
        }
    }

    #[must_use]
    pub fn surface_name(self) -> &'static str {
        match self {
            Self::ActorId => "@actor_id",
            Self::Supervisor => "@supervisor",
            Self::TraceSpan => "@trace_span",
        }
    }

    #[must_use]
    pub fn ty(self) -> Ty {
        match self {
            Self::ActorId | Self::TraceSpan => Ty::U64,
            Self::Supervisor => Ty::Pointer {
                is_mutable: true,
                pointee: Box::new(Ty::Unit),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct ActorInitParamInfo {
    pub(super) name: String,
    pub(super) ty: Ty,
    pub(super) span: Span,
}

/// Result of type-checking a program.
#[derive(Debug, Clone)]
pub struct TypeCheckOutput {
    pub expr_types: HashMap<SpanKey, Ty>,
    /// W4.047 P1.1 — the **typed** checker→HIR handoff side-table.
    ///
    /// Carries the post-substitution, post-literal-defaulting [`ResolvedTy`]
    /// for every accepted expression span whose type is *concrete*. The value
    /// type `ResolvedTy` makes the four checker-internal states (`Ty::Var`,
    /// `Ty::Error`, unmaterialized `Ty::IntLiteral`/`Ty::FloatLiteral`)
    /// unrepresentable by construction, so an entry here is provably clean.
    ///
    /// **Totality contract:** populated at the `check_program` boundary by
    /// running [`ResolvedTy::from_ty`] over every surviving `expr_types` entry
    /// (after `validate_checker_output_contract` has pruned/diagnosed leaked
    /// inference and error types). The map is therefore *total over concrete
    /// accepted spans*. The only spans legitimately absent are pre-monomorphi-
    /// zation generic bodies whose type is a *covered* inference var (a type
    /// parameter `validate_expr_output_contract` retained because its
    /// unresolved vars are a subset of the tracked holes); monomorphization
    /// resolves those later. There is no "no entry → guess" third state for a
    /// concrete accepted expression.
    ///
    /// In Phase 1 (W4.047) this is a transitional *shadow* of `expr_types`:
    /// HIR lowering still drives off `expr_types` and only asserts agreement
    /// (zero behaviour change). Phase 2 promotes this to the primary read path;
    /// Phase 4 removes the `Ty`-typed `expr_types` HIR type-derivation reads.
    pub resolved_expr_types: HashMap<SpanKey, ResolvedTy>,
    /// RHS spans of accepted `lhs is TypeName` type patterns.
    ///
    /// The parser still represents the RHS as an identifier expression; this
    /// checker-owned table tells downstream tooling/lowering not to resolve that
    /// identifier through the value namespace.
    pub is_type_patterns: HashMap<SpanKey, Ty>,
    pub method_call_receiver_kinds: HashMap<SpanKey, MethodCallReceiverKind>,
    /// Spans of method calls whose resolved method declares `consumes_receiver`.
    ///
    /// Per-call-site flag derived from [`MethodSig::consumes_receiver`] at the
    /// dispatch site. Codegen consults this side table to null the receiver's
    /// drop slot after the call, so the scope-exit drop becomes a null-guarded
    /// no-op. PR 1 (issue #1295) ships the plumbing; the recognised set is
    /// empty (no Hew surface syntax sets the flag yet), so this map is empty
    /// for Hew programs — but tests can populate `consume_receiver_methods` on
    /// the [`Checker`] to exercise the path end-to-end.
    pub method_call_consumes_receiver: HashSet<SpanKey>,
    /// Checker-owned lowering metadata keyed by the lowering site's source span.
    ///
    /// Populated for erased runtime types whose lowering must not guess from
    /// codegen argument types. Missing entry means the checker could not produce a
    /// concrete lowering fact and downstream codegen must fail closed.
    pub lowering_facts: HashMap<SpanKey, LoweringFact>,
    /// Checker-owned actor receive-handler state guard policy keyed by the
    /// receive declaration span.
    ///
    /// Every receive handler is `Exclusive` in v0.5. The table is still
    /// checker-owned so HIR/MIR/codegen consume a produced fact instead of
    /// rediscovering actor-state safety from syntax downstream.
    pub actor_handler_state_guards: HashMap<SpanKey, ActorStateGuard>,
    /// Checker-owned method-call lowering decisions keyed by the method call span.
    ///
    /// Populated during type checking for both receiver-based runtime rewrites
    /// and module-qualified stdlib direct-call rewrites so serialization can
    /// consume a single authoritative contract instead of re-resolving C
    /// symbols from receiver types or the module registry.
    pub method_call_rewrites: HashMap<SpanKey, MethodCallRewrite>,
    /// Wire layout metadata keyed by canonical type name.
    ///
    /// Populated by `register_wire_methods` for every accepted `#[wire]` type
    /// so downstream lowering phases consume checker-owned field tags, names,
    /// casing, and version metadata instead of recovering it from source text.
    pub wire_layouts: WireLayoutTable,
    /// Checker-owned numeric method lowering decisions keyed by method-call span.
    ///
    /// Populated for accepted integer opt-out methods:
    /// `.wrapping_{add,sub,mul}`, `.checked_{add,sub,mul}`, and
    /// `.saturating_{add,sub,mul}`. HIR/MIR must consume this table instead of
    /// re-matching method-name strings downstream.
    pub numeric_method_lowerings: HashMap<SpanKey, NumericMethodLowering>,
    /// Checker-owned width-conversion method lowering decisions keyed by
    /// method-call span.
    ///
    /// Populated for accepted `.wrapping_as_<W>()` and `.saturating_as_<W>()`
    /// calls. HIR consumes this to emit `HirExprKind::NumericCast` (wrapping)
    /// or `HirExprKind::SaturatingWidthCast` (saturating) without re-matching
    /// method-name strings downstream.
    pub width_cast_lowerings: HashMap<SpanKey, WidthCastLowering>,
    /// Checker-owned actor mailbox dispatch decisions keyed by the method call span.
    ///
    /// Populated only when a method call resolves to an actor `receive fn`.
    /// HIR lowering consumes this side table before the generic method-call
    /// rewrite bridge and never reclassifies the receiver type downstream.
    pub actor_method_dispatch: HashMap<SpanKey, ActorMethodKind>,
    /// Checker-owned machine method dispatch decisions keyed by the method call span.
    ///
    /// Populated for every accepted `.step()` / `.state_name()` call on a
    /// machine-typed receiver.  HIR lowering checks this table before
    /// `method_call_rewrites` so machine method calls produce dedicated HIR
    /// nodes (`MachineStep` / `MachineStateName`) instead of falling through
    /// to the generic rewrite path (which would emit `MethodCallNoRewrite`).
    ///
    /// MIR/codegen consumers: wired in slice 6.
    pub machine_method_dispatch: HashMap<SpanKey, MachineMethodKind>,
    /// Checker-resolved `await conn.read()` / `await conn.read_string()` sites,
    /// keyed by the inner method-call span (NEW-1). Populated when an `await`
    /// wraps a `net.Connection::read`/`read_string` call. HIR lowering consumes
    /// this to emit `HirExprKind::ConnAwaitRead` (the non-blocking suspending
    /// read) instead of the blocking method call. The `bool` is `true` for
    /// `read_string` (bytes-to-string wrap), `false` for raw `read`.
    pub conn_await_reads: HashMap<SpanKey, bool>,
    /// Checker-resolved `await listener.accept()` sites, keyed by the inner
    /// method-call span (NEW-2). Populated when an `await` wraps a
    /// `net.Listener::accept` call. HIR lowering consumes this to emit
    /// `HirExprKind::ListenerAwaitAccept` (the non-blocking suspending accept)
    /// instead of the blocking method call — the listener-readiness sibling of
    /// [`TypeCheckOutput::conn_await_reads`].
    pub listener_await_accepts: HashSet<SpanKey>,
    /// Function tail expressions the checker Ok-wraps to satisfy an explicit
    /// `-> Result<Ok, Err>` return, keyed by the tail expression's span. A
    /// `Result`-returning function whose tail yields the `Ok` payload type
    /// (e.g. `fn f() -> Result<User, E> { db.find(id)? }`, where `db.find(id)?`
    /// is typed `User`) is coerced to `Ok(tail)` here so the body type-checks.
    /// The coercion is type-directed and strictly tail-only: it fires only at
    /// the function tail and the if/match arm tails that flow to it, and only
    /// when the tail type unifies with the `Ok` payload but NOT with the full
    /// `Result` (so a tail already typed `Result<Ok, Err>` is returned
    /// directly — no double-wrap). HIR lowering consumes this set to wrap the
    /// lowered tail in a synthetic `Ok(..)` variant constructor.
    pub tail_ok_coercions: HashSet<SpanKey>,
    /// Checker-resolved assignment target classification keyed by the target
    /// expression span. Missing entry means the checker rejected the target.
    pub assign_target_kinds: HashMap<SpanKey, AssignTargetKind>,
    /// Checker-resolved assignment target type-shape metadata keyed by the
    /// target expression span.  Populated alongside `assign_target_kinds` for
    /// every accepted assignment.  LLVM lowering consumes this fail-closed to
    /// determine signedness of compound-assignment arithmetic instead of
    /// re-deriving it from the AST.
    pub assign_target_shapes: HashMap<SpanKey, AssignTargetShape>,
    pub errors: Vec<TypeError>,
    pub warnings: Vec<TypeError>,
    /// Canonical record names (unqualified, matching the codegen thunk naming
    /// convention) that appear as `clone` call sites in user code and are NOT
    /// already seeded via actor-state or owned-Vec paths.
    ///
    /// Populated by the checker when it accepts a `RecordCloneInplace` rewrite.
    /// Codegen consumes this via `IrPipeline::user_clone_record_seeds` to seed
    /// `emit_state_clone_drop_synthesis` so the
    /// `__hew_record_clone_inplace_<R>` body is emitted for records first
    /// cloned at user-code call sites.
    ///
    /// Duplicates are harmless (the seed collector deduplicates).
    pub user_clone_record_seeds: Vec<String>,
    pub type_defs: HashMap<String, TypeDef>,
    /// Names of monomorphic builtin enums (e.g. `LookupError`) that were
    /// pre-registered into `type_defs` from `std/builtins.hew` for use in
    /// pattern-matching dispatch (`match Err(LookupError::NotFound) { … }`)
    /// but whose declarations are NOT part of the user program's authored
    /// source.
    ///
    /// Consumers that emit per-program "everything in `type_defs`" outputs
    /// (e.g. the sandbox-WASM bytecode descriptor table) must filter out
    /// these names UNLESS a user `TypeDecl` with the same name was also
    /// registered (in which case the user declaration is the source of
    /// truth and the entry is no longer internal-only).
    ///
    /// Populated from
    /// [`crate::builtin_enums::monomorphic_builtin_enums`] at checker
    /// startup so the marker stays in lockstep with the IR-substrate
    /// catalog consumed by MIR's
    /// `register_builtin_monomorphic_enum_layouts`.
    pub internal_builtin_enum_names: HashSet<String>,
    pub fn_sigs: HashMap<String, FnSig>,
    /// Struct type names whose fields directly or transitively contain opaque
    /// handle values. Used to enforce owned-handle accessor restrictions and
    /// to thread proven-safe field-drop metadata into codegen.
    pub handle_bearing_structs: HashSet<String>,
    /// Actor type names that participate in reference cycles.
    pub cycle_capable_actors: HashSet<String>,
    /// Module short names for user (non-stdlib) imports that have resolved items.
    pub user_modules: HashSet<String>,
    /// Inferred type arguments for generic function calls that lack explicit
    /// type annotations.  Keyed by the call expression's span.
    pub call_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Per-concrete-element `Vec<T>` runtime-ABI verdict for the
    /// monomorphisation re-resolution of element-typed methods under a type
    /// parameter (#1929 Stage 1).
    ///
    /// Populated at the checker output boundary by classifying every concrete
    /// type that appears as a generic call / record-init type-argument through
    /// the same authority that backs the constructor path
    /// ([`crate::stdlib::vec_element_runtime_suffix`] + the `Copy` marker for
    /// the `layout` class). A `Vec<T>` `push`/`get`/`set`/`pop` whose element
    /// is a declared type parameter keeps the `hew_vec_*_FAMILY` placeholder
    /// through HIR; MIR substitutes the concrete element per instantiation,
    /// looks it up here, and maps `(method, token)` to the concrete symbol via
    /// [`crate::stdlib::vec_element_op_symbol`]. An element ABSENT from this
    /// map (non-`Copy`/owned layout, tuples, closures, unresolved nominals)
    /// fails closed at MIR — never a silent accept.
    pub vec_generic_element_abi: HashMap<Ty, crate::stdlib::VecElementToken>,
    /// Inferred or explicit type arguments for record / enum-struct-variant
    /// initialiser sites on user-defined generic types.  Keyed by the
    /// initialiser expression's span.
    ///
    /// Populated for every accepted `StructInit` against a user `TypeDef` whose
    /// `type_params` is non-empty.  Monomorphic record inits produce no entry.
    ///
    /// Entries are emitted unconditionally at the call site (args may still
    /// carry a `Ty::Var` when an outer annotation such as
    /// `let b: Box<int> = Box { value: 1 }` makes them concrete only after
    /// `check_struct_init` returns).  The fail-closed contract (no `Ty::Var`
    /// crosses into HIR) is *established*, not re-asserted, by
    /// `validate_record_init_type_args_output_contract` (`admissibility.rs`)
    /// at the output boundary, after `subst.resolve` and
    /// `materialize_literal_defaults` have settled.
    ///
    /// The downstream HIR monomorphisation registry reads this map to build
    /// per-instantiation record layouts.  Until that consumer lands the
    /// side-table is dormant.
    pub record_init_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Diagnostic-only escape-analysis hints produced by the stack-hint walker.
    ///
    /// One entry per `let` / `var` binding whose right-hand side resolves to a
    /// known heap allocation class (`Vec`, `String`, `HashMap`, `HashSet`,
    /// `Rc`, closure environment). The CLI surfaces these behind `--show-stack-hints` as
    /// `info[HEW-PERF-001]` lines; missing or empty means the walker found no
    /// heap allocations to consider.
    ///
    /// Phase A.0 is intentionally noisy: the walker emits a hint for every
    /// non-`Stack` allocation class without escape filtering. Subsequent
    /// slices (A.1/A.2/A.3) progressively suppress false positives by adding
    /// escape predicates. Hint accuracy is not a stable contract until A.4.
    pub stack_hints: Vec<StackHint>,
    /// Checker-owned alias-vs-copy decision per actor send site.
    ///
    /// Populated during `enforce_actor_boundary_send` for every accepted
    /// actor-send call. Codegen consumes this side table fail-closed: a
    /// missing entry for a known send span is a hard error.
    ///
    /// `Copy` variants carry an [`ActorSendCopyReason`] describing why
    /// the alias path was rejected (non-identifier expression, `Copy`
    /// type, stdlib `Drop`, user `impl Drop`); `Alias` variants are a
    /// unit. Codegen consumes the map fail-closed and propagates the
    /// reason out to `--explain-cow`.
    pub actor_send_aliasing: HashMap<SpanKey, ActorSendAliasing>,
    /// Per-actor arena cap in bytes, populated from `#[max_heap(N)]` annotations.
    ///
    /// Keyed by actor type name. Only actors that carry a `#[max_heap]` attribute
    /// appear in this map; actors without the annotation are absent (unbounded
    /// arena, runtime cap = 0). `Some(0)` is permitted and means "explicit zero",
    /// which the runtime treats as unbounded (same as `hew_arena_new`).
    ///
    /// Codegen reads this map to decide whether to call `hew_arena_new_with_cap`
    /// or `hew_arena_new` when spawning an actor.
    pub actor_max_heap: HashMap<String, u64>,
    /// Compile-time slot assignment for supervisor child accesses.
    ///
    /// Keyed by the `SpanKey` of the field-access expression (e.g. the span of
    /// `app.cache` in `app.cache.query(req)`). Populated during type-checking
    /// of field-access expressions whose object resolves to a `LocalPid<S>`
    /// where `S` is a known supervisor type.
    ///
    /// The `index` field is the position of the child within its own slot space:
    /// - `Static` children index into `HewSupervisor.children[]` (0-based, source order).
    /// - `Pool` children index into `HewSupervisor.pool_slots[]` (0-based, source order).
    ///
    /// Missing entry means the checker could not resolve the child (e.g. unknown
    /// field); MIR lowering must fail closed on a missing entry.
    pub supervisor_child_slots: HashMap<SpanKey, ChildSlot>,
    /// Resolved static-pool accessors (`sup.pool[i]` / `.get(i)` / `.len()`),
    /// keyed by the OUTER expression span (the `Index` or `MethodCall`). The
    /// inner `FieldAccess`'s `supervisor_child_slots` entry is REMOVED when
    /// consumed by one of these forms, so the bare-pool-access HIR gate fires
    /// only for a genuinely bare `sup.pool`.
    pub pool_accessor_sites: HashMap<SpanKey, PoolAccessor>,
    /// Per-call-site `T → dyn Trait` coercion metadata used by the MIR
    /// trait-object lowering and the LLVM vtable emitter.
    ///
    /// Keyed by the `SpanKey` of the argument expression coerced into a
    /// trait-object position. Each entry names the trait the coercion targets,
    /// the resolved concrete `Self` type at that site, and the per-impl
    /// method resolution that codegen will turn into vtable slots. Object
    /// safety is enforced at insertion time: traits with generic methods or
    /// `Self`-returning methods produce a [`TypeErrorKind::TraitNotObjectSafe`]
    /// diagnostic and no entry is inserted for that site.
    ///
    /// Multi-bound `dyn (A + B)` coercion sites flatten into a single
    /// [`DynCoercion`] whose `trait_name` joins the bound names with `+` and
    /// whose `method_table` concatenates the per-bound entries in declaration
    /// order; each entry's method name is prefixed by the originating trait
    /// (`Trait::method`) so downstream consumers can recover the binding.
    pub dyn_trait_coercions: HashMap<SpanKey, DynCoercion>,
    /// Per-method-call-site resolution for `obj.method()` where `obj` has
    /// resolved type `Ty::TraitObject`. Each entry pins the originating trait,
    /// the method name, and the vtable slot index (`3 + method_decl_order` —
    /// see [`DynMethodCall::slot`] for the prefix-triple convention).
    ///
    /// Populated alongside [`MethodCallReceiverKind::TraitObject`] at every
    /// accepted method-call on a trait-object receiver. Downstream HIR / MIR
    /// lowering consume this fail-closed: a method-call whose receiver typed
    /// as a trait object but whose span is absent from this map is a HIR
    /// diagnostic, not a runtime panic.
    pub dyn_trait_method_calls: HashMap<SpanKey, DynMethodCall>,
    /// Checker-authoritative closure capture facts keyed by the closure literal span.
    ///
    /// The checker records the exact lexical binding for every captured name before
    /// HIR/MIR lowering. Downstream stages must consume this ledger rather than
    /// rediscovering capture legality from expression shape.
    pub closure_capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>>,
    /// Checker-authoritative per-closure escape classification.
    ///
    /// Sibling to `closure_capture_facts` (one entry per closure literal,
    /// not per captured binding). Missing entry for a known closure span
    /// is treated as the conservative default (`Escapes`) by downstream
    /// consumers — the absence is itself fail-closed metadata.
    pub closure_escape_facts: HashMap<SpanKey, ClosureEscapeFact>,
    /// Per-actor protocol descriptor — the stable name → `msg_id` mapping
    /// for every `receive fn`. Keyed by actor type name.
    ///
    /// Populated after handler signatures resolve. An actor whose handler
    /// names collide under the default `SipHash-1-3` → low-32-bits hash
    /// gets a `TypeErrorKind::ActorProtocolCollision` diagnostic and is
    /// **absent** from this map: HIR/MIR/codegen must treat a missing
    /// entry for an actor that declared `receive fn`s as fail-closed (the
    /// program does not compile when collisions are present, but the
    /// side-table itself also refuses to advertise a broken protocol).
    ///
    /// This is the canonical source of truth: there is no fallback
    /// `enumerate()` path. Downstream stages route `msg_id` derivation
    /// through this side-table — see `hew_mir::lower` where the actor
    /// layout is constructed.
    pub actor_protocol_descriptors: HashMap<String, crate::actor_protocol::ActorProtocolDescriptor>,
    /// Intrinsic-declaration side-table: function name → intrinsic key.
    ///
    /// Populated for every `#[intrinsic("key")] pub fn name(...)` declaration
    /// seen during type-checking. HIR lowering consults this table when it
    /// encounters a function item: if the name is present here, the body stub
    /// is skipped and the function is treated as a compiler-intrinsic with the
    /// given key. HIR validates the key against the known intrinsic catalog and
    /// fails closed with `HirDiagnosticKind::UnknownIntrinsic` if the key is
    /// absent.
    ///
    /// WHY: the catalog previously conjured `CompilerIntrinsic` entries without
    /// a Hew-side declaration. This table is the checker-owned authority for
    /// the typed-declaration migration path.
    /// WHEN-OBSOLETE: when all catalog `CompilerIntrinsic` rows have migrated
    /// to `#[intrinsic]` declarations (slices 4–7); the table then becomes the
    /// complete intrinsic registry and the catalog rows can be removed.
    pub intrinsic_declarations: HashMap<String, String>,
    /// Per-arm pattern resolution keyed by the arm's pattern span.
    ///
    /// Populated by the checker during `check_match_stmt` and `check_match_expr`
    /// for every arm whose top-level pattern is accepted.  HIR lowering must
    /// consult this table fail-closed: a match arm that is present in the AST
    /// but absent from this map means the checker accepted it without recording
    /// resolution context, which is a HIR diagnostic (analogous to
    /// `MethodCallNoRewrite`).
    ///
    /// `Pattern::Or` arms are intentionally absent from this table; or-pattern
    /// lowering is a future lane.  A missing entry for an or-pattern arm must
    /// surface a typed diagnostic, not a silent fallthrough.
    ///
    /// WHEN-OBSOLETE: never; this table is the checker's authoritative
    /// output for match semantics downstream.
    pub pattern_resolutions: HashMap<SpanKey, ArmResolution>,
    /// Compiler-recognised lang-item registry built from `#[lang_item("…")]`
    /// attributes on traits and trait methods during trait registration.
    /// HIR lowering consults this table to discover the trait/method names
    /// for f-string `Display` dispatch instead of hard-coding `"Display"` /
    /// `"fmt"` symbols. See [`crate::LangItemRegistry`].
    pub lang_items: crate::LangItemRegistry,
    /// Checker-authored layout-key `HashMap` lowering facts keyed by call-site span.
    ///
    /// Populated by `finalize_hashmap_admission` for `HashMap<CopyRecord, V>` sites
    /// after hash-eligibility validation.  Facts begin in the `Pending` state;
    /// codegen (C-3) transitions each to `Finalized` after emitting the key-layout
    /// global.  Absent entry for a Named-key site means the checker rejected the key.
    pub hashmap_layout_facts: HashMap<SpanKey, crate::lowering_facts::HashMapLoweringFact>,
    /// Checker-authored layout-element `HashSet` lowering facts keyed by call-site span.
    ///
    /// Populated by `finalize_lowering_facts` for `HashSet<CopyRecord>` sites
    /// after hash-eligibility validation.  Facts begin in the `Pending` state;
    /// codegen (C-3) transitions each to `Finalized` after emitting the elem-layout
    /// global.  Absent entry for a Named-element site means the checker rejected the element.
    pub hashset_layout_facts: HashMap<SpanKey, crate::lowering_facts::HashSetLoweringFact>,
    /// Per-spawn-site type arguments for generic actor instantiations.
    ///
    /// Keyed by the `SpanKey` of the `spawn` expression. Each entry holds the
    /// actor name and the checker-resolved type arguments supplied at that
    /// spawn site (`spawn Foo<i64>(...)` → `("Foo", [Ty::I64])`).
    ///
    /// Populated by `check_spawn` when non-empty type args are resolved.
    /// Empty for non-generic actors (no entry) or for generic actors that
    /// triggered a `MissingActorTypeArgs` diagnostic (also no entry).
    ///
    /// Consumed by the actor-mono discovery pass (blocked on
    /// `MachineMonoPass` infra) to build per-instantiation `ActorLayout`
    /// records keyed by `mangle_instantiation(SymbolClass::Actor, …)`.
    pub actor_spawn_type_args: HashMap<SpanKey, (String, Vec<Ty>)>,
    /// Checker-authored unified-dispatch table keyed by method-call span.
    ///
    /// This is the substrate introduced by W4.001 Stage A. It will, in
    /// subsequent stages, become the single source of truth for
    /// "how does this method call lower" — replacing the parallel
    /// authorities currently split across `method_call_rewrites`,
    /// `admissibility.rs` per-K allowlists, and `methods.rs` runtime
    /// symbol-family selection.
    ///
    /// **Stage A invariant (DI-003 fail-closed-by-absence):** no
    /// production reader of this field exists yet. The map is empty for
    /// every Hew program checked in Stage A. Stage B populates it from
    /// the unified resolver (dual-emit alongside `method_call_rewrites`);
    /// Stage C makes HIR lowering consult it as the dispatch authority
    /// for HashMap/HashSet; Stage D extends to Vec/Option/Result. A
    /// missing entry — once a Stage B/C/D consumer is wired — is a
    /// bug-class invariant violation, never a silent "no rewrite
    /// needed" fallthrough.
    ///
    /// See [`crate::check::dispatch`] module docs for the full Stage A
    /// substrate ownership rationale and downstream consumer ordering.
    pub resolved_calls: HashMap<SpanKey, crate::check::dispatch::ResolvedCall>,
    /// Import type alias resolution table for HIR lowering.
    ///
    /// Maps each bare alias name to its canonical qualified source identity for
    /// every `import m::{ T as U }` where the alias binding (`U`) differs from
    /// Import type alias map: maps `(module, alias-binding)` → canonical
    /// qualified source identity, for every `import m::{ T as U }` where the
    /// binding name (`U`) differs from the original name (`T`).
    ///
    /// Example: `import hew::aliassrc::{ Payload as Tag }` in module `None`
    /// (root program) → `(None, "Tag") → "aliassrc.Payload"`.
    ///
    /// Keyed by `(Option<String>, String)` — `(importer_module, alias)` — so
    /// an alias introduced in one module cannot overwrite a same-named alias
    /// from another module (the flat-string approach caused last-write-wins
    /// cross-module pollution).
    ///
    /// HIR `lower_type` consults this table in `resolve_named_type_ref` as a
    /// **fallback** (after local / builtin / record-registry lookups), so a
    /// local `type U` in the same module as `import m::{ Payload as U }` wins.
    pub import_type_name_aliases: HashMap<(Option<String>, String), String>,
}

/// Wire layout metadata for a single field, carried from AST through the
/// compilation pipeline so lowering passes never recover metadata from source
/// strings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WireFieldLayout {
    /// Source-level field name.
    pub name: String,
    /// Numeric wire tag (`@N`), the compatibility authority.
    pub tag: u32,
    /// Explicit JSON key override, if provided (`json_name = "..."`).
    pub json_name: Option<String>,
    /// Explicit YAML key override, if provided (`yaml_name = "..."`).
    pub yaml_name: Option<String>,
    /// Whether this field is optional (maps to `Option<T>` absence semantics).
    pub optional: bool,
    /// Whether this field is repeated (maps to `Vec<T>`).
    pub repeated: bool,
}

/// Wire layout metadata for a single type (struct or enum).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WireLayoutEntry {
    /// True for `#[wire] struct`, false for `#[wire] enum`.
    pub is_struct: bool,
    /// Type-level JSON casing override.
    pub json_case: Option<NamingCase>,
    /// Type-level YAML casing override.
    pub yaml_case: Option<NamingCase>,
    /// Wire schema version (from `#[wire(version = N)]`).
    pub version: Option<u32>,
    /// Minimum compatible reader version.
    pub min_version: Option<u32>,
    /// Ordered fields (structs). Empty for enums.
    pub fields: Vec<WireFieldLayout>,
    /// Enum variant tags. Each entry is `(variant_name, discriminant_tag)`.
    /// Empty for structs.
    pub variants: Vec<(String, u32)>,
}

/// All wire types registered during type-checking, keyed by canonical type name.
pub type WireLayoutTable = HashMap<String, WireLayoutEntry>;

/// Capture mode selected by the checker for one closure environment field.
///
/// `Copy` and `Move` are the historical v0.5 variants — `Copy` is an implicit
/// by-value capture of a `Copy`-typed binding, and `Move` is the explicit
/// `move |...|` form that consumes the source binding. `Borrow` and
/// `BorrowMut` are inferred from body usage when the source binding is
/// neither `Copy`-typed nor consumed by `move`: read-only references infer
/// `Borrow`, mutating projections infer `BorrowMut`. There is no surface
/// syntax for `Borrow`/`BorrowMut`; they are checker-substrate output only.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureCaptureMode {
    /// The source value implements `Copy`, so an implicit by-value copy is legal.
    Copy,
    /// The closure was written `move |...|`; the source binding is consumed.
    Move,
    /// The body only reads the captured binding (read-only deref / field
    /// project); the checker classifies this capture as a shared reference
    /// for downstream lowering.
    Borrow,
    /// The body mutates the captured binding (assignment, mutating method
    /// call, or assignment through a projection); the checker classifies
    /// this capture as an exclusive reference for downstream lowering.
    BorrowMut,
}

/// Provenance of a [`ClosureCaptureMode`] decision.
///
/// Records which inference rule produced the mode so that downstream
/// diagnostics (suspend-crossing, escape advisory, future auto-lock
/// wrappers) can explain the choice without re-running inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CaptureModeOrigin {
    /// `Move`: the closure literal carried the `move` keyword.
    ExplicitMove,
    /// `Copy`: the captured binding's resolved type implements `Copy`.
    ImplicitCopy,
    /// `Borrow`: the body uses the binding only in read-only positions.
    InferredBorrow,
    /// `BorrowMut`: the body mutates the binding (assignment or mutating
    /// method call).
    InferredBorrowMut,
}

/// Checker-owned capture record for one binding referenced by a closure body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosureCaptureFact {
    /// Checker-local identity of the captured lexical binding.
    pub binding_id: TypeBindingId,
    /// Surface name used at the capture site.
    pub name: String,
    /// Fully resolved captured type at checker-output time.
    pub ty: Ty,
    /// Capture mode selected by the checker.
    pub mode: ClosureCaptureMode,
    /// Inference-rule provenance for `mode`.
    pub mode_origin: CaptureModeOrigin,
    /// Whether the captured type satisfies the actor/task boundary marker.
    pub is_send: bool,
    /// Whether the captured type satisfies the `Sync` marker. Populated by
    /// the same `TraitRegistry::is_sync` query that the rest of the checker
    /// uses; consumed by the non-Sync-mut-capture-crosses-suspend
    /// diagnostic and by future auto-lock injection.
    pub is_sync: bool,
    /// Source span of this use inside the closure body.
    pub use_span: Span,
    /// Definition span of the captured binding when user-authored.
    pub def_span: Option<Span>,
}

/// Escape classification for one closure literal.
///
/// Conservative by default: a closure is `Escapes` unless the classifier
/// can positively prove `Local` or `Forked`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureEscapeKind {
    /// All use-sites are direct calls within the closure's introducing
    /// lexical scope; the environment never outlives that scope.
    Local,
    /// The closure (or its introducing binding) is returned, stored,
    /// sent through a channel, passed to a higher-order callee whose
    /// escape attribute is not provable, or used in a shape the
    /// classifier cannot prove safe. Conservative default.
    Escapes,
    /// The closure is the body of (or referenced by) a `fork { ... }`
    /// child task inside an enclosing `scope { ... }` block.
    Forked,
}

/// Which inference rule fired to produce a [`ClosureEscapeKind`].
///
/// `Local` and `Forked` carry the positive rule that classified them;
/// `Escapes` carries the conservative-default rule that rejected
/// `Local`/`Forked`. The variant is consumed by the advisory diagnostic
/// (`ClosureEscapeAdvisory`) so the user can see *why* `Local` was not
/// admitted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureEscapeRule {
    /// Every use of the closure-bound name is a direct call `f(args)`.
    DirectCallOnly,
    /// Closure literal sits directly inside a `fork { ... }` body, OR
    /// the closure-bound name is invoked inside such a body.
    InsideForkBlock,
    /// Closure (or its bound name) appears in a return statement / tail
    /// position of the enclosing function body.
    Returned,
    /// Closure (or its bound name) is passed as an argument to a call,
    /// method-call, struct-init, or other higher-order context whose
    /// escape attribute is not provable.
    PassedToHigherOrder,
    /// Closure value is the tail expression of a block, propagating out.
    EscapesViaBlockValue,
    /// Closure (or its bound name) is stored in a `let`/`var` whose
    /// binding outlives the closure's introducing scope (e.g. assigned
    /// to a field, sent through a channel, used by a sibling closure
    /// classified `Escapes`).
    StoredOrSent,
    /// Closure has no statically resolvable introducing `let f = ...`
    /// binding (anonymous literal in a non-fork position).
    NoStaticBinding,
}

/// Per-closure escape classification, keyed in
/// [`TypeCheckOutput::closure_escape_facts`] by the closure literal's span.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClosureEscapeFact {
    /// Conservative escape classification (see [`ClosureEscapeKind`]).
    pub kind: ClosureEscapeKind,
    /// Which classification rule produced `kind`.
    pub rule: ClosureEscapeRule,
}

/// Checker-resolved metadata for a `T → dyn Trait` coercion call site.
///
/// Populated by the checker for every accepted coercion of a concrete
/// receiver into a trait-object argument. Downstream MIR construction and
/// LLVM vtable emission consume this fail-closed: missing entry at a known
/// coercion span is a hard error during lowering.
///
/// The `method_table` is ordered: vtable slot index `i` (after the
/// runtime-fixed `drop_in_place`/`size_of`/`align_of` prefix triple defined
/// in `hew-runtime/src/trait_object.rs`) maps to the i-th entry in
/// `method_table`. For multi-bound coercions the order is the trait bounds'
/// declaration order, with each bound contributing its trait's methods in
/// the trait declaration order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynAssocBinding {
    /// Originating trait name; qualifies `assoc_name` for multi-bound objects.
    pub trait_name: String,
    /// Associated type declared by `trait_name`.
    pub assoc_name: String,
    /// Fully projected binding type.
    pub ty: Ty,
}

/// Canonical vtable intern key for a concrete-to-dyn coercion.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynVtableKey {
    /// Trait name (or `Trait1+Trait2` for multi-bound `dyn (A + B)`).
    pub trait_name: String,
    /// Resolved concrete `Self` type at the coercion site.
    pub concrete_type: Ty,
    /// Canonical associated-type bindings sorted by `(trait_name, assoc_name)`.
    pub assoc_bindings: Vec<DynAssocBinding>,
}

/// Checker-authored vtable slot entry with substituted method signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynVtableEntry {
    /// Originating trait name for this slot.
    pub trait_name: String,
    /// Trait method name as declared in the trait.
    pub method_name: String,
    /// Implementer-side function key (`Type::method`).
    pub impl_fn_key: String,
    /// Caller-side signature after substituting trait type parameters and
    /// associated-type bindings (e.g. `Self::Item` -> `int`).
    pub signature: FnSig,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynCoercion {
    /// Trait name (or `Trait1+Trait2` for multi-bound `dyn (A + B)`).
    pub trait_name: String,
    /// Resolved concrete `Self` type at the coercion site.
    pub concrete_type: Ty,
    /// Canonical vtable key used to distinguish projections such as
    /// `dyn Iterator<Item = int>` from `dyn Iterator<Item = string>`.
    pub vtable_key: DynVtableKey,
    /// Canonical associated-type binding side-table entries, qualified by
    /// originating trait and sorted by `(trait_name, assoc_name)`.
    pub assoc_bindings: Vec<DynAssocBinding>,
    /// Ordered vtable entries. Each entry carries the substituted caller-side
    /// method signature at the trait-object boundary.
    pub vtable_entries: Vec<DynVtableEntry>,
    /// Ordered `(method_name, impl_fn_key)` pairs naming the impl-side
    /// resolution for each trait method.
    ///
    /// * `method_name` is the trait method's declared name. For multi-bound
    ///   coercions it is prefixed by `Trait::` so the originating trait is
    ///   recoverable.
    /// * `impl_fn_key` is the implementer-side identifier in the shape
    ///   `<Type>::<method>` for user types (matches the key under which
    ///   the impl method is registered in [`Checker::fn_sigs`]). For
    ///   primitive and compiler-builtin receivers the key is
    ///   `<canonical>::<method>` where `<canonical>` is
    ///   [`Ty::canonical_lowering_name`] / the builtin-generic name; the
    ///   impl signature itself lives in
    ///   [`Checker::primitive_trait_impls`].
    pub method_table: Vec<(String, String)>,
}

/// Checker-resolved metadata for a method call on a `dyn Trait` receiver.
///
/// Populated by the checker at every accepted `obj.method(args)` where
/// `obj`'s resolved type is [`Ty::TraitObject`]. The MIR producer reads
/// this side-table to emit `Instr::CallTraitMethod` with a pre-computed
/// vtable slot; HIR lowering reads it to choose `HirExprKind::CallDynMethod`
/// over the `method_call_rewrites` direct-call path.
///
/// The slot convention follows
/// `hew-runtime/src/trait_object.rs::HewVtable`:
///
/// | Slot | Contents              |
/// |------|-----------------------|
/// | 0    | `drop_in_place`       |
/// | 1    | `size_of` (data)      |
/// | 2    | `align_of` (data)     |
/// | 3..N | trait method slots, in trait declaration order |
///
/// `slot` is therefore `3 + method_decl_order` for the originating trait.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynMethodCall {
    /// Originating trait name (resolved from the receiver's `Ty::TraitObject`
    /// bound that defined the called method).
    pub trait_name: String,
    /// Trait method name as declared in the trait body.
    pub method_name: String,
    /// Vtable slot index: `3 + 0-based method declaration order` within
    /// the originating trait.
    pub slot: u32,
    /// Caller-side method signature after substituting trait type
    /// parameters and associated-type bindings from the receiver's
    /// `Ty::TraitObject` bound (e.g. `Self::Item -> int`). The receiver
    /// parameter has been filtered out — this is the calling-side shape
    /// MIR / codegen consume to derive the erased indirect-call type
    /// (see W3.031 Stage 7: drop `params[0]` (Self), prepend a single
    /// `ptr` data argument). Mirrors the shape stored on
    /// [`DynVtableEntry::signature`] for the corresponding `(trait,
    /// method)` slot at the coercion site.
    pub signature: FnSig,
}

/// Compile-time slot descriptor for a supervisor child access.
///
/// Produced by the checker at every `supervisor.child_name` field-access site.
/// MIR lowering reads this to emit the correct `hew_supervisor_child_get` (for
/// `Static`) or `hew_supervisor_pool_route` (for `Pool`) ABI call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChildSlot {
    /// Whether this child occupies a static slot or a pool slot.
    pub kind: ChildKind,
    /// 0-based index within the child's slot space (static or pool, disjoint).
    pub index: u32,
    /// Declared actor type of the child (e.g. `"CacheActor"`).
    pub child_ty: String,
    /// Binding name of the child as declared in the supervisor (e.g. `"cache"`
    /// for `child cache: CacheActor`). Authoritative source for diagnostics
    /// — distinguishes two children of the same supervisor that happen to
    /// share an actor type (e.g. `worker_a: Worker` + `worker_b: Worker`).
    pub child_name: String,
    /// Outer supervisor type name owning this slot (e.g. `"RootSupervisor"`).
    /// Authoritative — paired with `child_name` to identify the exact slot.
    pub supervisor: String,
}

/// Discriminates a static child slot from a pool child slot.
///
/// Static and pool indices live in disjoint spaces, matching the runtime layout
/// (`HewSupervisor.children[]` for static, `HewSupervisor.pool_slots[]` for pool).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChildKind {
    /// Child declared with `child name: Type`. Indexed in `HewSupervisor.children[]`.
    Static,
    /// Child declared with `pool name: Type`. Indexed in `HewSupervisor.pool_slots[]`.
    Pool,
}

/// A resolved static-pool accessor: `sup.pool[i]`, `sup.pool.get(i)`, or
/// `sup.pool.len()`. Recorded by the checker at the OUTER expression span (the
/// `Index` or `MethodCall`, not the inner `FieldAccess`) so MIR lowering reads
/// the supervisor + `pool_key` and emits the right runtime call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PoolAccessor {
    /// The supervisor type owning the pool (e.g. `"Pool"`).
    pub supervisor: String,
    /// The pool child name (e.g. `"workers"`).
    pub child_name: String,
    /// The pool's declared member actor type (e.g. `"Worker"`).
    pub child_ty: String,
    /// The pool slot index in `HewSupervisor.pool_slots[]` (the `pool_key`).
    pub pool_key: u32,
    /// Which accessor form this site is.
    pub kind: PoolAccessorKind,
}

/// The three supported static-pool accessor forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PoolAccessorKind {
    /// `sup.pool[i]` — trapping member access (`Vec[i]` OOB parity).
    Index,
    /// `sup.pool.get(i)` — `Option<LocalPid<T>>` member access (`None` on OOB).
    Get,
    /// `sup.pool.len()` — `i64` member count.
    Len,
}

/// Partitioned child lists for a supervisor declaration.
///
/// Static and pool children occupy disjoint slot spaces. The slot index for each
/// child is its 0-based position within its own list (`statics` or `pools`).
/// Supervisor names map to this type via `Checker::supervisor_children`.
#[derive(Debug, Clone, Default)]
pub(crate) struct SupervisorChildren {
    /// Children declared with `child name: Type`, in source order.
    /// Slot index = position in this vec.
    pub(crate) statics: Vec<(String, String)>,
    /// Children declared with `pool name: Type`, in source order.
    /// Slot index = position in this vec.
    pub(crate) pools: Vec<(String, String)>,
}

// ── Pattern-resolution side table ────────────────────────────────────────────

/// Checker-resolved classification of one match arm's top-level pattern.
///
/// Populated by the checker during `check_match_stmt` / `check_match_expr` for
/// every arm whose pattern is accepted.  `Pattern::Or` arms are absent by
/// design: or-pattern lowering is a follow-on lane; HIR lowering must treat a
/// missing entry for an or-pattern arm as a fail-closed diagnostic, not a
/// silent fallthrough.
///
/// This is the top-level kind only — sub-patterns (e.g. the inner pattern of
/// `Some(x)`) are not recorded separately; per-arm is the granularity the
/// match-arm HIR lowering lane will consume.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternKind {
    /// `_`
    Wildcard,
    /// A literal value (`1`, `"hello"`, `true`, …).
    Literal,
    /// A plain lowercase identifier that introduces a binding (e.g. `x`).
    Binding,
    /// An uppercase or path-qualified identifier or `Constructor(…)` form that
    /// resolves to a variant/constructor (e.g. `None`, `Some(x)`, `Err(e)`,
    /// `Shape::Circle { r }`).  Includes built-in `Option`/`Result` variants.
    VariantCtor,
    /// A struct-pattern with named fields that is NOT an enum struct-variant
    /// (i.e. the matched type is a plain struct/record, not an enum).
    StructPattern,
    /// A tuple pattern `(a, b, …)` including the empty-tuple unit pattern `()`.
    TuplePattern,
    /// A regex literal pattern `re"..."` in a match arm.
    ///
    /// `captures` lists the named capture groups derived from
    /// `regex::Regex::capture_names()` after the checker validated the
    /// pattern. Each entry is `(name, group_index)` where `group_index` is
    /// the 1-based regex group position (group 0 is the whole match; named
    /// groups start at 1). Positional-only groups are skipped. Storing the
    /// real group index rather than the named-capture-only position ensures
    /// correct lookup when unnamed groups precede named ones.
    ///
    /// HIR lowering reads this to populate `HirMatchArm::kind` for the
    /// regex arm and to know which capture names (and indices) to bind before the body.
    Regex { captures: Vec<(String, u32)> },
}

/// Checker-resolved identity of the matched enum variant.
///
/// Keyed by the arm's pattern span and consumed by match-arm HIR lowering to
/// emit `MachineVariantCtor` / `EnumVariantCtor` etc. without re-resolving.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantMatch {
    /// Canonical name of the type definition that owns this variant.
    /// For built-in `Option`/`Result` this is `"Option"` / `"Result"`.
    pub type_name: String,
    /// Unqualified variant name (e.g. `"Some"`, `"None"`, `"Ok"`, `"Err"`,
    /// `"Counting"` for a user enum).
    pub variant_name: String,
}

/// Checker-resolved payload binding for one positional or named payload slot
/// within a constructor or struct pattern.
///
/// For `Some(x)`: one entry, `field_idx = 0`, `binding_name = "x"`.
/// For `Counter::Counting { value }`: one entry, `field_idx = 0` (position in
/// the variant's field list), `binding_name = "value"`.
/// For `_` sub-patterns inside a constructor: no entry is emitted for that
/// slot (the binder doesn't introduce a name into scope).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PayloadBinding {
    /// 0-based index of this payload slot within the variant's field list.
    pub field_idx: usize,
    /// Surface name the binding introduces in the arm body scope.
    pub binding_name: String,
    /// Fully resolved type of the payload at this position.
    ///
    /// Carries a `Ty::Var` while inference is in flight; resolved to a
    /// concrete type at the `check_program` output boundary via
    /// `Substitution::resolve`.
    pub ty: Ty,
}

/// Checker-resolved nested constructor subpattern occupying one payload slot
/// of an enclosing constructor pattern.
///
/// For `Err(IoError::NotFound)`: one entry on the `Err` arm with
/// `field_idx = 0`, `payload_ty = IoError`, `variant_match = NotFound`, no
/// bindings, no nested children.
/// For `Ok(Ok(v))` on `Result<Result<i64, E1>, E2>`: one entry with
/// `field_idx = 0`, `payload_ty = Result<i64, E1>`, `variant_match = Ok`,
/// and `bindings = [v @ inner slot 0]`.
///
/// `bindings` and `nested` index into THIS nested variant's payload list,
/// not the enclosing one. The structure is recursive so arbitrary
/// constructor-nesting depth flows through one shape; non-constructor
/// refutable subpatterns (literals, struct/tuple destructures, or-patterns)
/// inside a nested constructor remain fail-closed at the checker.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PayloadVariantPattern {
    /// 0-based slot of this subpattern within the ENCLOSING variant's
    /// payload list.
    pub field_idx: usize,
    /// Checker-resolved enum type of that payload slot (the type the nested
    /// tag check runs against).
    pub payload_ty: Ty,
    /// Resolved identity of the nested variant being matched.
    pub variant_match: VariantMatch,
    /// Bindings introduced from THIS nested variant's payload slots.
    pub bindings: Vec<PayloadBinding>,
    /// Deeper nested constructor subpatterns within this variant's payload.
    pub nested: Vec<PayloadVariantPattern>,
}

/// Checker-resolved summary of one match arm's pattern.
///
/// Keyed by `SpanKey::from(&arm.pattern.1)` in the
/// `TypeCheckOutput::pattern_resolutions` side table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArmResolution {
    /// High-level pattern classification.
    pub pattern_kind: PatternKind,
    /// For constructor / variant patterns: which type-def the ctor belongs to
    /// and which variant.  `None` for `Wildcard`, `Literal`, `Binding`,
    /// `StructPattern` on a plain record, and `TuplePattern`.
    pub variant_match: Option<VariantMatch>,
    /// Ordered payload bindings introduced by this arm's pattern.
    ///
    /// Empty for `Wildcard`, `Literal`, identifier `Binding` (the binding
    /// itself is expressed via the environment, not here), `None` / unit
    /// variants, and struct/tuple patterns that destructure to wildcards only.
    pub payload_bindings: Vec<PayloadBinding>,
    /// Nested constructor subpatterns in this arm's constructor payload
    /// (e.g. the `IoError::NotFound` in `Err(IoError::NotFound)`).
    ///
    /// Populated only for `VariantCtor` arms; consumers that cannot honour
    /// the nested checks (if-let / while-let lowering) MUST fail closed on a
    /// non-empty vector rather than ignore it.
    pub payload_variant_patterns: Vec<PayloadVariantPattern>,
}

impl TypeCheckOutput {
    /// Record an expression's checker type, keeping the `Ty`-typed
    /// `expr_types` side-table and the typed `resolved_expr_types` handoff
    /// map (W4.047) in sync.
    ///
    /// Mirrors the `check_program` boundary's totality contract: a *concrete*
    /// type populates both maps (via the single authorised
    /// [`ResolvedTy::from_ty`] conversion); a non-concrete type (a leaked
    /// inference var, an error placeholder, or an unmaterialized literal)
    /// populates only `expr_types`, leaving the typed map correctly absent.
    ///
    /// Use this from tests/fixtures that hand-build a `TypeCheckOutput` so the
    /// two maps never drift — a direct `expr_types.insert` would leave the
    /// typed handoff under-populated and trip the HIR totality shadow assert.
    pub fn insert_expr_type(&mut self, span: SpanKey, ty: Ty) {
        if let Ok(resolved) = ResolvedTy::from_ty(&ty) {
            self.resolved_expr_types.insert(span.clone(), resolved);
        }
        self.expr_types.insert(span, ty);
    }
}

impl Default for TypeCheckOutput {
    /// Produce an empty `TypeCheckOutput` with no resolved types, rewrites, or
    /// diagnostics. Useful in tests that exercise HIR lowering without
    /// invoking the full type-checker (e.g. programs that contain no method
    /// calls and therefore need no `method_call_rewrites` entries).
    fn default() -> Self {
        Self {
            expr_types: HashMap::new(),
            resolved_expr_types: HashMap::new(),
            is_type_patterns: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            method_call_consumes_receiver: HashSet::default(),
            lowering_facts: HashMap::new(),
            actor_handler_state_guards: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            wire_layouts: HashMap::new(),
            numeric_method_lowerings: HashMap::new(),
            width_cast_lowerings: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            user_clone_record_seeds: Vec::new(),
            type_defs: HashMap::new(),
            internal_builtin_enum_names: HashSet::new(),
            fn_sigs: HashMap::new(),
            handle_bearing_structs: HashSet::default(),
            cycle_capable_actors: HashSet::default(),
            user_modules: HashSet::default(),
            call_type_args: HashMap::new(),
            vec_generic_element_abi: HashMap::new(),
            record_init_type_args: HashMap::new(),
            stack_hints: Vec::new(),
            actor_send_aliasing: HashMap::new(),
            actor_max_heap: HashMap::new(),
            supervisor_child_slots: HashMap::new(),
            pool_accessor_sites: HashMap::new(),
            actor_method_dispatch: HashMap::new(),
            machine_method_dispatch: HashMap::new(),
            conn_await_reads: HashMap::new(),
            listener_await_accepts: HashSet::new(),
            tail_ok_coercions: HashSet::new(),
            dyn_trait_coercions: HashMap::new(),
            dyn_trait_method_calls: HashMap::new(),
            closure_capture_facts: HashMap::new(),
            closure_escape_facts: HashMap::new(),
            actor_protocol_descriptors: HashMap::new(),
            intrinsic_declarations: HashMap::new(),
            pattern_resolutions: HashMap::new(),
            lang_items: crate::LangItemRegistry::new(),
            hashmap_layout_facts: HashMap::new(),
            hashset_layout_facts: HashMap::new(),
            actor_spawn_type_args: HashMap::new(),
            resolved_calls: HashMap::new(),
            import_type_name_aliases: HashMap::new(),
        }
    }
}

/// Classification of a binding's right-hand-side allocation shape.
///
/// Drives the `HEW-PERF-001` diagnostic. Variants cover the genuine heap
/// categories visible in the codegen layer (closure envs, Vec/HashMap/HashSet
/// bodies, Rc, String). `Stack` is the no-hint fallthrough — bindings whose
/// RHS is already stack-shaped (primitives, by-value structs) are not
/// surfaced.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AllocationClass {
    /// RHS resolves to `Vec<T>`.
    Vec,
    /// RHS resolves to `String`.
    String,
    /// RHS resolves to `HashMap<K, V>`.
    HashMap,
    /// RHS resolves to `HashSet<T>`.
    HashSet,
    /// RHS resolves to `Rc<T>`.
    Rc,
    /// RHS is a closure literal whose environment is heap-allocated today.
    ClosureEnv,
    /// Already stack-shaped — no hint should be emitted.
    Stack,
    /// Walker could not classify the RHS expression form. Conservative
    /// silence: no hint emitted.
    Indeterminate,
}

/// One diagnostic-only stack-allocation hint.
///
/// Produced by the stack-hint walker for every binding whose RHS classifies as
/// a heap allocation. The CLI consumes these via `--show-stack-hints`.
///
/// `span_key` is byte-position-keyed (matches `SpanKey`'s contract) so future
/// phases can correlate hints back to the originating expression even after
/// AST-level rewrites.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackHint {
    /// Source span of the binding's defining `let` / `var` statement.
    pub span_key: SpanKey,
    /// Identifier the user wrote (`let f = ...` → `"f"`). For tuple / struct
    /// destructuring patterns where no single name applies, this is empty;
    /// callers may still render the hint at the span location.
    pub binding_name: String,
    /// Allocation class assigned to the binding's RHS.
    pub alloc_class: AllocationClass,
}

/// Codegen choice between aliasing the sender's payload buffer (refcount
/// bump on a [`HewMsgEnvelope`]) and the legacy deep-copy mailbox path.
///
/// `Copy` carries an [`ActorSendCopyReason`] so downstream tooling
/// (notably `--explain-cow`) can render *why* an arg fell off the alias
/// path rather than printing a single placeholder string for every
/// `Copy` site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ActorSendAliasing {
    /// Sender retains the payload independently; runtime deep-copies into
    /// the mailbox.
    Copy(ActorSendCopyReason),
    /// Sender and receiver share a refcounted payload buffer. Requires the
    /// move-checker to have invalidated the sender's binding so no
    /// post-send observation is possible.
    Alias,
}

/// Checker-owned actor-state guard policy for a dispatchable actor handler.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ActorStateGuard {
    /// Handler receives exclusive mutable access to actor state for its body.
    Exclusive,
}

/// Why the type-checker classified an actor-send arg as `Copy` instead
/// of `Alias`.  Flows through the side table to codegen and the
/// `--explain-cow` renderer so users see a precise reason.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ActorSendCopyReason {
    /// Arg expression is not a bare identifier (e.g. a field access,
    /// projection, or freshly constructed value). The move-checker
    /// does not invalidate the parent binding, so aliasing would be
    /// unsound.
    NotIdentifier,
    /// Arg's resolved type implements the `Copy` marker — cloning is
    /// trivial and the alias bit would be ambiguous.
    CopyType,
    /// Arg's resolved type implements the stdlib-registered `Drop`
    /// marker. Sender-side drop suppression has to be coordinated with
    /// the receiver, which Phase α does not handle yet.
    StdlibDrop,
    /// Arg's resolved type carries a user `impl Drop for T` that the
    /// `Drop` marker does not flag (registered only in
    /// `trait_impls_set`). Same Phase α restriction as `StdlibDrop`.
    UserDrop,
}

/// Checker-owned classification of an assignment target.
///
/// Populated once during `check_stmt` for `Stmt::Assign` and threaded through
/// the serialisation boundary so LLVM lowering can consume it fail-closed
/// instead of re-examining the AST expression kind.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignTargetKind {
    /// `name = rhs` — target resolved to a mutable local variable in scope.
    LocalVar,
    /// `field = rhs` — bare field name inside an actor body targeting actor state.
    ActorField,
    /// `expr.field = rhs` — target is a struct/actor field access.
    FieldAccess,
    /// `expr[idx] = rhs` — target is an indexed collection element.
    Index,
}

/// Checker-side record of one actor state field while its actor is being
/// checked.
///
/// Carries the declared mutability (`var` = mutable; `let` and bare
/// declarations = immutable, matching the parser's `FieldDecl::is_mutable`)
/// and the declaration site so the immutable-field assignment diagnostic can
/// point back at the field declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ActorFieldInfo {
    /// Field name as declared in the actor body.
    pub name: String,
    /// `true` when declared with `var`; `false` for `let` and bare fields.
    pub is_mutable: bool,
    /// Span of the field's type annotation (the declaration line).
    pub decl_span: Span,
}

/// Position context for `synthesize_index` (`obj[k]`).
///
/// The result type and recorded runtime call differ between a read (`let x =
/// m[k]`) and an assignment target (`m[k] = v`).  For `HashMap<K, V>` a read
/// yields `Option<V>` and records `hew_hashmap_get_layout`, while an assignment
/// target yields the bare `V` (the RHS checks against `V`) and records
/// `hew_hashmap_insert_layout`.  Other receivers (Vec, string, bytes) are
/// context-insensitive and ignore this flag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum IndexContext {
    /// `obj[k]` appearing in value position (read).
    Read,
    /// `obj[k]` appearing as the target of an assignment (`obj[k] = rhs`).
    AssignTarget,
}

/// Checker-owned type-shape annotation for an assignment target.
///
/// Populated alongside [`AssignTargetKind`] for every `Stmt::Assign` accepted
/// by the checker.  Missing entry means the checker rejected the target; LLVM
/// lowering must fail closed on both maps.
///
/// This captures the information codegen needs for compound-assignment arithmetic
/// signedness so it does not need to re-derive it from the AST or from the
/// `expr_types` side-table (which may be absent when a resolved type is
/// unavailable at lowering time).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignTargetShape {
    /// `true` when the assignment target has an unsigned integer type
    /// (`u8` / `u16` / `u32` / `u64`).  `false` for signed integers, floats,
    /// booleans, and all non-numeric types.
    pub is_unsigned: bool,
}

/// Span used as map key (byte offsets + module index).
///
/// `module_idx` is 0 for the root compilation unit and N for the N-th
/// non-root module in topological order.  Without it, two source files that
/// both have an expression at the same byte offset collide in the flat
/// `expr_types` map, causing the type-checker's annotation for one file to
/// overwrite the other's — the root cause of cross-module span-key collisions
/// (std/fs.hew + std/path.hew unary-minus and `StringLit` defects).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct SpanKey {
    pub start: usize,
    pub end: usize,
    /// 0 = root compilation unit; N = N-th non-root module (1-based topo order).
    pub module_idx: u32,
}

impl From<&Span> for SpanKey {
    /// Constructs a root-module key (`module_idx` = 0).
    /// Use [`SpanKey::in_module`] when recording types for non-root modules.
    fn from(s: &Span) -> Self {
        Self {
            start: s.start,
            end: s.end,
            module_idx: 0,
        }
    }
}

impl SpanKey {
    /// Construct a key for a span in module `module_idx` (0 = root).
    #[must_use]
    pub fn in_module(s: &Span, module_idx: u32) -> Self {
        Self {
            start: s.start,
            end: s.end,
            module_idx,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodCallReceiverKind {
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
    /// Receiver is a primitive (`int`, `bool`, `char`, ...) or compiler-builtin
    /// generic (`Vec`, `HashMap`, `HashSet`, `Bytes`) that resolved to a
    /// user-defined `impl Trait for <kind>` body via the
    /// `primitive_trait_impls` side table.  The `canonical_receiver` field is
    /// the key used to look up the impl (see
    /// `Checker::canonical_primitive_or_builtin_key`); the `trait_name` is the
    /// trait whose method body the call dispatched to.
    PrimitiveTraitImpl {
        trait_name: String,
        canonical_receiver: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptionResultMethod {
    OptionIsSome,
    OptionIsNone,
    OptionUnwrap,
    OptionUnwrapOr,
    ResultIsOk,
    ResultIsErr,
    ResultUnwrap,
    ResultUnwrapOr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodCallRewrite {
    /// Rewrite a receiver-based method call to a runtime function and inject
    /// the receiver as the first argument.
    ///
    /// `elem_ty` is a forward-compatible carry-channel for the element type
    /// of generic containers (e.g. `Vec<T>`). It is currently always `None`
    /// — the per-suffix C symbol (`hew_vec_get_f64`) still encodes the
    /// element type. A later slice will collapse those per-suffix symbols
    /// to a single generic symbol (`hew_vec_get_generic`) and route the
    /// element type through this field instead.
    ///
    /// `descriptor` is the typed cross-layer identity for closed
    /// runtime/builtin calls the checker resolves with first-class family
    /// knowledge — populated by the closed-set builtin rewrite recorder (and a
    /// few explicit-construction sites) for the symbols the
    /// [`crate::runtime_call::RuntimeCallFamily`] catalog enumerates. It is
    /// `None` for every open-set string: user-trait method keys like `i64::fmt`
    /// AND every `#[extern_symbol]` FFI method — including stdlib `duration` /
    /// `instant` / `LambdaActorHandle` bindings — *even when that method's raw
    /// symbol collides with a catalog name* (e.g. `hew_duration_hours`). An
    /// `#[extern_symbol]` method is open-set by mechanism: its family is only
    /// recoverable by reverse-parsing the symbol string, which the
    /// `checker-output-boundary` forbids, so it never carries a typed
    /// descriptor. When `Some`, consumers should dispatch on
    /// `descriptor.family()` rather than re-matching `c_symbol`; the `c_symbol`
    /// field is retained as the concrete linker-edge identifier for now and
    /// will be retired once every downstream consumer has migrated onto the
    /// descriptor.
    ///
    /// `consumes_receiver` is `true` when the rewritten runtime call takes
    /// ownership of (consumes) its receiver — the `.close()`-family handle
    /// release builtins plus the lambda-actor handle release. The single
    /// authority for which symbols consume is
    /// [`crate::builtin_names::runtime_symbol_consumes_receiver`] (currently
    /// `hew_stream_close`, `hew_sink_close`, `hew_channel_sender_close`,
    /// `hew_channel_receiver_close`, `hew_duplex_close`, `hew_duplex_close_half`,
    /// `hew_lambda_actor_release`); consult that function rather than this list,
    /// which is illustrative and must not drift from it. HIR lowers a consuming
    /// receiver with `IntentKind::Consume` so the MIR move-checker marks the
    /// handle moved-out — its scope-exit drop must NOT fire again
    /// (double-`close`/double-free). Borrowing methods (`send`/`recv`) leave
    /// this `false`. The verdict is derived from the resolved `c_symbol` in
    /// the single rewrite-recording authority, keyed on the runtime
    /// discriminant rather than a receiver type name
    /// (LESSONS: drop-allowset-from-value-flow, raii-null-after-move).
    RewriteToFunction {
        c_symbol: String,
        descriptor: Option<crate::runtime_call::RuntimeCallDescriptor>,
        elem_ty: Option<crate::resolved_ty::ResolvedTy>,
        consumes_receiver: bool,
    },
    /// Rewrite a module-qualified stdlib call directly to a runtime function
    /// without injecting the receiver/module identifier as an argument.
    ///
    /// `c_symbol` here remains an honest open-set string: module-qualified
    /// rewrites can reach user-module pass-throughs whose symbols are not
    /// (and cannot be) enumerated by the typed runtime-call catalog. This
    /// is the sole legitimate open-set checker-side symbol after the
    /// typed-descriptor migration (Q182=(b)) and is intentionally NOT
    /// fronted by a typed descriptor — do not attempt to type it away.
    ///
    /// See `RewriteToFunction::elem_ty` for the semantics of `elem_ty`.
    RewriteModuleQualifiedToFunction {
        c_symbol: String,
        elem_ty: Option<crate::resolved_ty::ResolvedTy>,
    },
    /// Rewrite a generic `math.abs/min/max` call to the concrete intrinsic
    /// selected from the lowered argument type in HIR.
    GenericMathIntrinsic {
        op: MathGenericOp,
    },
    DeferToLowering,
    /// Checker-authoritative lowering for builtin `Option<T>` / `Result<T, E>`
    /// receiver methods. HIR consumes this closed marker by synthesising a
    /// generic-enum `match`, so no Option/Result runtime symbol is involved.
    BuiltinOptionResult {
        method: OptionResultMethod,
    },
    /// Checker-authoritative `CancellationToken.is_cancelled()` intrinsic.
    ///
    /// HIR/MIR consume this structured marker without re-checking the
    /// receiver type; codegen lowers it to the borrowing
    /// `hew_cancel_token_is_requested` runtime call.
    CancellationTokenIsCancelled,
    /// Builtin `Vec<T>::into_iter()` iterator constructor. HIR expands this
    /// directly to a `VecIter<T>` record so the Rust MIR pipeline does not need
    /// to lower the generic stdlib impl body.
    BuiltinVecIntoIter {
        elem_ty: crate::resolved_ty::ResolvedTy,
    },
    /// Builtin `HashMap<K, V>::into_iter()` iterator constructor. HIR expands
    /// this directly to a `HashMapIter<K, V>` record built from `keys()` /
    /// `values()` snapshots — the same cursor the `for (k, v) in m` desugar
    /// synthesises — so the pipeline form (`iter::map(m.into_iter(), ..)`)
    /// matches `Vec`'s without a generic stdlib `impl IntoIterator for HashMap`
    /// body (which the checker cannot admit on an abstract receiver).
    BuiltinHashMapIntoIter {
        key_ty: crate::resolved_ty::ResolvedTy,
        val_ty: crate::resolved_ty::ResolvedTy,
    },
    /// Builtin `VecIter<T>::next(var self)` state advance. HIR expands this at
    /// the call site so the caller's mutable iterator binding observes the
    /// cursor update.
    BuiltinVecIterNext {
        elem_ty: crate::resolved_ty::ResolvedTy,
    },
    /// Builtin `Generator<Y, R>::next()` consumption. HIR emits a dedicated
    /// `HirExprKind::GeneratorNext`; MIR lowers it to `Instr::GeneratorNext`,
    /// which codegen turns into a `hew_gen_next(ctx, &out_size)` runtime call
    /// whose returned heap pointer is unboxed into `Option<yield_ty>` (null →
    /// `None`; else load the payload, build `Some`, free the heap pointer).
    /// The receiver is borrowed — the generator handle stays live and is freed
    /// by `hew_gen_free` on its own scope-exit drop, not by this call.
    GeneratorNext {
        yield_ty: crate::resolved_ty::ResolvedTy,
    },
    /// Remote `RemotePid<T>::ask(msg, timeout_ms)` request/reply dispatch.
    ///
    /// HIR consumes this structured marker and emits a dedicated
    /// `HirExprKind::RemoteActorAsk`, preserving the checker-authoritative
    /// `Result<T::Reply, AskError>` return type.
    RemoteActorAsk,
    /// Builtin `Vec<T>` higher-order pipeline call (`map` / `filter` /
    /// `reduce`, spec §3.8.6). HIR expands the call site into a counted
    /// loop over the receiver — bind the receiver and the closure argument
    /// once, then per element call the closure and collect (`map`),
    /// conditionally push the element (`filter`), or fold into a mutable
    /// accumulator (`reduce`). No runtime symbol exists for these; the
    /// loop reuses the established Vec substrate (`hew_vec_len`, the
    /// element getter family, and the `hew_vec_push_*` family), so every
    /// element-type ABI rule and drop discipline is inherited rather than
    /// re-derived.
    BuiltinVecHigherOrder {
        op: VecHigherOrderOp,
        /// Receiver element type.
        elem_ty: crate::resolved_ty::ResolvedTy,
        /// `Map`: the mapped element type; `Filter`: equals `elem_ty`;
        /// `Reduce`: the accumulator type.
        out_ty: crate::resolved_ty::ResolvedTy,
    },
    /// `receiver.field(args)` where `field` is a record field of function
    /// type: dispatches as a field-load + closure call, not a method lookup.
    /// HIR expands the call site into a synthetic let binding the field's
    /// pair value (a borrow — the record keeps env ownership) followed by a
    /// closure call on that binding. `field_ty` is the checker-resolved
    /// function type of the field; its return type is the call's type.
    RecordFnFieldCall {
        field_ty: crate::resolved_ty::ResolvedTy,
    },
    /// Binary wire codec call on a `#[wire]` struct or enum:
    /// `value.encode() -> bytes` (instance) or `Type.decode(bytes) -> Type`
    /// (static).
    ///
    /// The CBOR round-trip is implemented by the `__hew_cbor_serialize_<key>` /
    /// `__hew_cbor_deserialize_<key>` C-ABI thunk pair codegen emits
    /// (`hew-codegen-rs/src/llvm.rs`). A struct rides a tag-keyed CBOR map; an
    /// enum rides the "map-of-one" shape. These thunks have a non-Hew
    /// ABI (an out-length / out-struct-size pointer parameter and a malloc'd
    /// result the caller adopts), so the call cannot lower through the generic
    /// `RewriteToFunction` path — it gets a dedicated HIR node that codegen
    /// wires to the thunk with the correct ABI.
    ///
    /// `value_ty` is the checker-resolved wire type (the receiver type for
    /// `encode`, the produced type for `decode`); codegen derives the thunk key
    /// from it via the same `mangle_resolved_ty` encoder the actor cross-node
    /// path uses, so every site referencing one wire type shares a single CBOR
    /// thunk pair.
    ///
    /// The binary directions (`Encode`/`Decode`) drive the CBOR codec; the text
    /// directions (`ToJson`/`FromJson`/`ToYaml`/`FromYaml`) drive the CBOR↔text
    /// bridge (reuse the binary walk + a generic transcode). `to_toml`/
    /// `from_toml` remain fail-closed (`MethodCallNoRewrite`) until TOML lands.
    WireCodec {
        direction: WireCodecDirection,
        value_ty: crate::resolved_ty::ResolvedTy,
    },
    /// User-record `clone` call: `clone p` or `p.clone()` where `p` has a
    /// user-defined record type that passed `record_clone_admissibility`. HIR
    /// lowers this to a call to the synthesised
    /// `__hew_record_clone_inplace_<record_name>` thunk (non-consuming read of
    /// the source). `record_name` is the canonical (unqualified) name used by
    /// the codegen thunk naming convention.
    RecordCloneInplace {
        record_name: String,
    },
    /// `clone x` or `x.clone()` where `x` has a Copy/BitCopy type (`i64`,
    /// `bool`, `char`, etc.). The checker emits a non-fatal `RedundantClone`
    /// warning and returns the operand type unchanged. HIR lowers this as a
    /// plain read (no extra copy is needed — `BitCopy` semantics already copy).
    CopyCloneNoop,
    /// Static trait dispatch: the method was resolved from the bounds on a
    /// generic type parameter. HIR emits `CallTraitMethodStatic`; MIR
    /// resolves the concrete callee at monomorphization time.
    StaticTraitDispatch {
        /// The type-parameter name on the enclosing function that carries the bound
        /// (e.g. "T" in `fn foo<T: Show>(x: T)`). Used by MIR to look up the
        /// concrete type from the monomorphization substitution map.
        receiver_type_param: String,
        /// The bound on the type parameter through which the method was reached
        /// (e.g. "B" in `T: B` where `trait B: A`). Used for error messages and
        /// for impl lookup when bound != declaring.
        bound_trait: String,
        /// The trait that directly declares the method in its `trait_defs` entry.
        /// If method is inherited, `declaring_trait` != `bound_trait`.
        /// Used as the canonical identity for impl resolution.
        declaring_trait: String,
        /// Method identity within the trait.
        method_name: String,
        /// Checker-owned receiver ABI bit from the declaring trait signature.
        requires_mutable_receiver: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathGenericOp {
    Abs,
    Min,
    Max,
}

/// Direction of a [`MethodCallRewrite::WireCodec`] call.
///
/// The binary directions (`Encode`/`Decode`) drive the CBOR body codec; the
/// text directions (`ToJson`/`FromJson`/`ToYaml`/`FromYaml`) drive the
/// CBOR↔text BRIDGE: a text serialize reuses the binary CBOR walk to build the
/// value tree, then transcodes that tree to JSON/YAML text via a compiler-
/// emitted tag↔name descriptor; a text deserialize parses the text, transcodes
/// the tree back to CBOR, then reuses the binary CBOR decode walk. There is no
/// parallel per-format struct/enum walk — the text codec is the binary codec
/// plus a generic transcode (RATIFIED, Q203).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WireCodecDirection {
    /// `value.encode() -> bytes`: serialize the receiver to CBOR bytes.
    Encode,
    /// `Type.decode(bytes) -> Type`: deserialize CBOR bytes back to the type.
    Decode,
    /// `value.to_json() -> string`: serialize the receiver to JSON text.
    ToJson,
    /// `Type.from_json(string) -> Result<Type, string>`: parse JSON text.
    FromJson,
    /// `value.to_yaml() -> string`: serialize the receiver to YAML text.
    ToYaml,
    /// `Type.from_yaml(string) -> Result<Type, string>`: parse YAML text.
    FromYaml,
}

impl WireCodecDirection {
    /// True for the serialize directions (`value -> text/bytes`): `Encode`,
    /// `ToJson`, `ToYaml`.
    #[must_use]
    pub fn is_serialize(self) -> bool {
        matches!(self, Self::Encode | Self::ToJson | Self::ToYaml)
    }

    /// True for the text-format directions (JSON/YAML); false for the binary
    /// CBOR directions (`Encode`/`Decode`).
    #[must_use]
    pub fn is_text(self) -> bool {
        matches!(
            self,
            Self::ToJson | Self::FromJson | Self::ToYaml | Self::FromYaml
        )
    }

    /// The text format of a text direction, or `None` for the binary CBOR
    /// directions.
    #[must_use]
    pub fn text_format(self) -> Option<WireTextFormat> {
        match self {
            Self::ToJson | Self::FromJson => Some(WireTextFormat::Json),
            Self::ToYaml | Self::FromYaml => Some(WireTextFormat::Yaml),
            Self::Encode | Self::Decode => None,
        }
    }
}

/// The two text wire formats the bridge transcodes between CBOR and text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WireTextFormat {
    /// JSON (`to_json`/`from_json`).
    Json,
    /// YAML (`to_yaml`/`from_yaml`).
    Yaml,
}

/// Which `Vec<T>` pipeline method a
/// [`MethodCallRewrite::BuiltinVecHigherOrder`] expands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VecHigherOrderOp {
    /// `v.map(f)` — `Vec<T> → Vec<U>` via `f: fn(T) -> U`.
    Map,
    /// `v.filter(p)` — `Vec<T> → Vec<T>` keeping elements where
    /// `p: fn(T) -> bool` holds.
    Filter,
    /// `v.reduce(f, init)` — `Vec<T> → A` via `f: fn(A, T) -> A` seeded
    /// with `init: A`.
    Reduce,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumericMethodFamily {
    Wrapping,
    Checked,
    Saturating,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumericMethodOp {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumericSignedness {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumericWidth {
    Bits(u32),
    Pointer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumericMethodLowering {
    pub family: NumericMethodFamily,
    pub op: NumericMethodOp,
    pub result_ty: Ty,
    pub operand_ty: Ty,
    pub signedness: NumericSignedness,
    pub width: NumericWidth,
}

/// Discriminator for width-conversion method lowering.
///
/// `Wrapping` → bit-truncate/extend (semantics: modular wrap, same as `as`-cast).
/// `Saturating` → clamp to `[W::MIN, W::MAX]` before narrowing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WidthCastKind {
    Wrapping,
    Saturating,
}

/// Checker-owned lowering record for `.wrapping_as_<W>()` and `.saturating_as_<W>()`.
///
/// Keyed by the method-call span in [`TypeCheckOutput::width_cast_lowerings`].
/// HIR consumes this to emit `HirExprKind::NumericCast` (wrapping) or
/// `HirExprKind::SaturatingWidthCast` (saturating) instead of falling
/// through to `MethodCallNoRewrite`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WidthCastLowering {
    /// The source integer type (receiver type at the call site).
    pub from_ty: Ty,
    /// The target integer type (parsed from the method-name suffix).
    pub to_ty: Ty,
    /// Whether this is a wrapping or saturating conversion.
    pub kind: WidthCastKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActorMethodKind {
    /// Fire-and-forget dispatch to an actor receive handler that returns `()`.
    Fire(String),
    /// Request/reply dispatch to an actor receive handler with a non-unit reply.
    Ask(String, Ty),
}

/// Checker-authoritative machine method dispatch discriminator.
///
/// Keyed by the method-call span in `machine_method_dispatch`.  HIR lowering
/// checks this table before `method_call_rewrites` so that machine method
/// calls produce dedicated HIR nodes rather than falling through to the generic
/// rewrite path (which would emit `MethodCallNoRewrite`).
///
/// MIR/codegen consumers: `MachineStep` lowers in slice 6; `MachineStateName`
/// lowers in slice 6 (string-table lookup on the tag).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MachineMethodKind {
    /// `.step(event: NameEvent) -> ()` — mutates the machine value in place.
    ///
    /// `machine_name` is the unqualified machine type name (e.g. `"TrafficLight"`).
    Step { machine_name: String },
    /// `.state_name() -> String` — returns the current state tag as a string.
    ///
    /// `machine_name` is the unqualified machine type name (e.g. `"TrafficLight"`).
    StateName { machine_name: String },
}

#[derive(Debug, Clone)]
pub(super) struct PendingLoweringFact {
    pub(super) hashset_element_ty: Ty,
    pub(super) source_module: Option<String>,
}

/// A `HashMap` key/value admission check deferred until after all inference
/// has settled.  Recorded when `validate_hashmap_key_value_types` encounters
/// `Ty::Var` arguments (type still in-flight); drained by
/// `finalize_hashmap_admission` in `check_program`.
#[derive(Debug, Clone)]
pub(super) struct DeferredHashMapAdmission {
    pub(super) span: Span,
    pub(super) key_ty: Ty,
    pub(super) val_ty: Ty,
    pub(super) source_module: Option<String>,
    pub(super) is_abstract_key_param: bool,
}

/// A `HashSet` element admission check deferred until after all inference has
/// settled.  Recorded when `validate_hashset_element_type` encounters a
/// `Ty::Var` element (type still in-flight); drained by
/// `finalize_hashset_admission` in `check_program`.
#[derive(Debug, Clone)]
pub(super) struct DeferredHashSetAdmission {
    pub(super) span: Span,
    pub(super) elem_ty: Ty,
    pub(super) source_module: Option<String>,
}

/// A `Vec` element admission check deferred until after all inference has
/// settled. Recorded when `validate_vec_element_type` encounters an element
/// type that still contains an unresolved inference variable; drained by
/// `finalize_vec_admission` in `check_program`.
#[derive(Debug, Clone)]
pub(super) struct DeferredVecAdmission {
    pub(super) span: Span,
    pub(super) elem_ty: Ty,
    pub(super) source_module: Option<String>,
}

/// A channel method call rewrite deferred until after all inference has settled.
///
/// Recorded when a `Sender<T>::send` / `Receiver<T>::recv` / `try_recv` call is
/// encountered but the inner type `T` is still an unresolved `Ty::Var` at the
/// call site (for example `let v: int = rx.recv()` — the `int` annotation
/// constrains `T` *after* the call is visited).
///
/// Drained by `finalize_channel_rewrites` in `check_program`, after all
/// inference has settled, so the correct type-specific C symbol is selected.
#[derive(Debug, Clone)]
pub(super) struct DeferredChannelMethodRewrite {
    /// The built-in handle kind: `"Sender"` or `"Receiver"`.
    pub(super) handle_kind: String,
    /// The method name: `"send"`, `"recv"`, or `"try_recv"`.
    pub(super) method: String,
    /// The inner element type variable (still unresolved at record time).
    pub(super) inner_ty: Ty,
    /// Module path where this rewrite was recorded (None = root module).
    pub(super) source_module: Option<String>,
}

impl PendingLoweringFact {
    pub(super) fn hashset(hashset_element_ty: Ty, source_module: Option<String>) -> Self {
        Self {
            hashset_element_ty,
            source_module,
        }
    }
}

/// WASM-unsupported feature classes.
///
/// Variants in the **warning group** (`Timers`) are emitted as diagnostics at
/// warning severity because wasm32 has a degraded-but-implemented runtime path.
///
/// Variants in the **reject group** (`SupervisionTrees`, `LinkMonitor`,
/// `StructuredConcurrency`, `Tasks`, `BlockingChannelRecv`,
/// `BlockingSemaphoreAcquire`, `Streams`, `HttpServer`, `TcpNetworking`,
/// `ProcessExecution`) are emitted as compile-time **errors**. Their runtime
/// support is absent on wasm32: some entry points trap via `unreachable!`,
/// while native-only modules such as scope/task/supervisor/link-monitor are
/// gated out of `hew-runtime` entirely. Making them errors ensures WASM
/// programs fail loudly at check time rather than at link time or the first
/// use at runtime.
///
/// `Timers` is now in the **warning group**: `sleep_ms`/`sleep` are implemented
/// with cooperative semantics on wasm32 (park at message boundary rather than
/// mid-handler).  See `hew-runtime/src/scheduler_wasm.rs` for the
/// sleeping-actor queue.
///
/// `link`/`unlink`/`monitor`/`demonitor` are bundled together under
/// `LinkMonitor` because they share the same OS-thread dependency.
///
/// See `docs/wasm-capability-matrix.md` for the authoritative Tier 1 / Tier 2
/// capability split and feature disposition table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum WasmUnsupportedFeature {
    // ── Reject group (no coherent wasm32 runtime support; compile-time error) ─
    SupervisionTrees,
    LinkMonitor,
    StructuredConcurrency,
    Tasks,
    HttpClient,
    Smtp,
    // ── Warning group (implemented with degraded semantics) ─────────────────
    /// `sleep_ms`, `sleep`, `#[every(duration)]`: timers are cooperative on
    /// wasm32. Sleep parks at the *message boundary*, and periodic handlers are
    /// delivered only when the host drives the timer queue via
    /// `hew_wasm_timer_tick` / `hew_wasm_sched_tick`.
    Timers,
    // ── Reject group (runtime unreachable!-trap; compile-time error) ────────
    /// `Receiver<T>::recv`: blocking receive still traps on wasm32 because the
    /// cooperative scheduler does not yet yield and resume on an empty channel
    /// with live senders.
    BlockingChannelRecv,
    /// `Semaphore::acquire` / `Semaphore::acquire_timeout`: blocking permit
    /// waits still depend on native condvar-style parking and have no
    /// cooperative wasm32 scheduler implementation yet.
    BlockingSemaphoreAcquire,
    /// `stream.*` module constructors and `Stream<T>::*` methods: the stream
    /// runtime module is not compiled for wasm32
    /// (`#[cfg(not(target_arch = "wasm32"))]` in hew-runtime/src/lib.rs).
    /// WASM-TODO(#1451): implement I/O-stream adapters over WASI fd/socket APIs.
    Streams,
    /// `http.listen` and `http.Server` / `http.Request` methods: the HTTP
    /// server runtime is backed by native sockets and `tiny_http`, and is not
    /// compiled for wasm32.
    /// WASM-TODO(#1451): design a cooperative WASI-hosted HTTP server surface.
    HttpServer,
    /// `net.*` constructors and `net.Listener` / `net.Connection` methods: the
    /// TCP transport runtime module is not compiled for wasm32.
    /// WASM-TODO(#1451): expose socket-backed listener/connection adapters over WASI.
    TcpNetworking,
    /// `process.*` helpers and `process.Child::*` methods: the process runtime
    /// module depends on the native OS process model and is not compiled for
    /// wasm32.
    /// WASM-TODO(#1451): define a host capability model for subprocess execution.
    ProcessExecution,
    /// `tls.*` constructors and `tls.*Stream` methods: the TLS transport runtime
    /// is backed by `rustls` over native sockets and is not compiled for
    /// wasm32. WASM-TODO(#1451): expose a WASI TLS bridge.
    Tls,
    /// `quic.*` endpoint/connection/stream/event constructors and methods: the
    /// QUIC transport is backed by `quinn` over native sockets and is not
    /// compiled for wasm32. WASM-TODO(#1451): expose a WASI QUIC bridge.
    Quic,
    /// `dns.resolve` / `dns.lookup_host`: the resolver is backed by the native
    /// OS resolver and is not compiled for wasm32. WASM-TODO(#1451): probe whether
    /// wasip1 `sock_addr_*` can cover the common case; relax to warn if so.
    Dns,
    /// `os.*` env / path / process helpers: the OS-env layer relies on native
    /// POSIX APIs and is not compiled for wasm32. WASM-TODO(#1451): route through a
    /// capability-scoped WASI surface.
    OsEnv,
    // ── Crypto reject additions ─────────────────────────────────────────────
    /// `crypto.random_bytes`: the secure entropy source (`ring::SystemRandom`)
    /// is native-only and absent from the wasm32 link set. Reject so callers
    /// cannot accidentally generate key material without cryptographic entropy.
    /// WASM-TODO(#1451): plumb host entropy through WASI `random_get`.
    CryptoRandom,
    /// `encrypt.*` (`std::crypto::encrypt`): AES-256-GCM seal/open helpers are
    /// provided by a native-only staticlib companion crate (`std/crypto/encrypt`)
    /// that is absent from the wasm32 link set. Reject so the caller sees a
    /// structured diagnostic at check time rather than a `wasm-ld` undefined-
    /// symbol error. WASM-TODO(#1451): design a WASI-capable symmetric-
    /// encryption surface.
    CryptoEncrypt,
    /// `sign.*` (`std::crypto::sign`): Ed25519 key-pair generation, signing, and
    /// verification are provided by a native-only staticlib companion crate
    /// (`std/crypto/sign`) that is absent from the wasm32 link set. Reject so
    /// the caller sees a structured diagnostic at check time rather than a
    /// `wasm-ld` undefined-symbol error. WASM-TODO(#1451): design a
    /// WASI-capable signature surface.
    CryptoSign,
}

impl WasmUnsupportedFeature {
    pub(super) fn label(self) -> &'static str {
        match self {
            Self::SupervisionTrees => "Supervision tree operations",
            Self::LinkMonitor => "Link/monitor operations",
            Self::StructuredConcurrency => "Structured concurrency scopes",
            Self::Tasks => "Task handles spawned from scopes",
            Self::HttpClient => "std::net::http::http_client operations",
            Self::Smtp => "std::net::smtp operations",
            Self::BlockingChannelRecv => "Blocking channel receive operations",
            Self::BlockingSemaphoreAcquire => "Blocking semaphore acquire operations",
            Self::Timers => "Timer operations",
            Self::Streams => "Stream operations",
            Self::HttpServer => "HTTP server operations",
            Self::TcpNetworking => "TCP networking operations",
            Self::ProcessExecution => "Process execution operations",
            Self::Tls => "std::net::tls operations",
            Self::Quic => "std::net::quic operations",
            Self::Dns => "std::net::dns resolver operations",
            Self::OsEnv => "std::os environment and path operations",
            Self::CryptoRandom => "std::crypto::crypto.random_bytes operations",
            Self::CryptoEncrypt => "std::crypto::encrypt operations",
            Self::CryptoSign => "std::crypto::sign operations",
        }
    }

    pub(super) fn reason(self) -> &'static str {
        match self {
            Self::SupervisionTrees => {
                "they require OS threads for restart strategies and child supervision"
            }
            Self::LinkMonitor => {
                "they rely on OS threads to watch linked actors and propagate exits"
            }
            Self::StructuredConcurrency => "they schedule child work on dedicated OS threads",
            Self::Tasks => "they need OS threads to drive scope completions",
            Self::HttpClient => {
                "the std::net::http::http_client wrappers are still native-only; \
                 no wasm32 networking bridge exists yet"
            }
            Self::Smtp => {
                "the std::net::smtp transport is still native-only; \
                 no wasm32 SMTP bridge exists yet"
            }
            Self::BlockingChannelRecv => {
                "Receiver<T>::recv still requires cooperative scheduler yield/resume on wasm32; \
                 use try_recv or the actor ask pattern instead"
            }
            Self::BlockingSemaphoreAcquire => {
                "Semaphore::acquire and Semaphore::acquire_timeout still require a blocking \
                 permit wait that has no cooperative wasm32 implementation; use try_acquire \
                 or actor coordination instead"
            }
            Self::Timers => {
                "timers are cooperative on wasm32: sleep parks at the message boundary, \
                 and #[every(duration)] handlers fire only when the host drives the timer queue"
            }
            Self::Streams => {
                "I/O streams require the OS threading and networking stack; \
                 the stream runtime module is not compiled for wasm32"
            }
            Self::HttpServer => {
                "the std::net::http server is backed by native sockets and tiny_http; \
                 no cooperative wasm32 server implementation exists yet"
            }
            Self::TcpNetworking => {
                "hew_tcp_listen / hew_tcp_connect require the native OS socket layer; \
                 the transport runtime module is not compiled for wasm32"
            }
            Self::ProcessExecution => {
                "hew_process_run / hew_process_spawn require the native OS process model; \
                 the process runtime module is not compiled for wasm32"
            }
            Self::Tls => {
                "the std::net::tls transport is backed by rustls over native sockets; \
                 no wasm32 TLS bridge exists yet"
            }
            Self::Quic => {
                "the std::net::quic transport is backed by quinn over native sockets; \
                 no wasm32 QUIC bridge exists yet"
            }
            Self::Dns => {
                "the std::net::dns resolver uses the native OS resolver; \
                 no wasm32 implementation exists yet"
            }
            Self::OsEnv => {
                "the std::os helpers rely on native POSIX APIs; \
                 the os runtime layer is not compiled for wasm32"
            }
            Self::CryptoRandom => {
                "the std::crypto.random_bytes secure entropy source (ring::SystemRandom) is \
                 native-only and absent from the wasm32 link set; no cryptographically secure \
                 wasm32 implementation exists yet; generating key material on wasm32 would \
                 not be secure"
            }
            Self::CryptoEncrypt => {
                "the std::crypto::encrypt module is backed by a native-only staticlib \
                 companion crate (std/crypto/encrypt) that is absent from the wasm32 link \
                 set; no wasm32 AES-GCM seal/open implementation exists yet"
            }
            Self::CryptoSign => {
                "the std::crypto::sign module is backed by a native-only staticlib \
                 companion crate (std/crypto/sign) that is absent from the wasm32 link \
                 set; no wasm32 Ed25519 implementation exists yet"
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub kind: TypeDefKind,
    pub name: String,
    pub type_params: Vec<String>,
    pub bounds: HashMap<String, Vec<String>>,
    pub fields: HashMap<String, Ty>,
    /// Field names in **declaration order** (source order as written by the user).
    ///
    /// This is the canonical ordering used by codegen (MIR `RecordLayout.field_tys`,
    /// HIR `RecordLayout.fields`) and by hash-thunk emitters (C-3+).  Layout
    /// computations that must agree with the binary ABI — such as
    /// `compute_copy_record_layout` — walk this Vec rather than sorting
    /// `fields.keys()` alphabetically.
    ///
    /// Populated in `registration.rs` for `record`, `type`/struct, `actor`, and
    /// `wire` declarations.  Left empty (`vec![]`) for synthetic / builtin `TypeDef`s
    /// that have no user-visible field ordering (pure-method types, enum companions,
    /// machine types, etc.) and for `TypeDef`s whose field set is already empty.
    pub field_order: Vec<String>,
    pub variants: HashMap<String, VariantDef>,
    pub methods: HashMap<String, FnSig>,
    pub doc_comment: Option<String>,
    pub is_indirect: bool,
}

#[derive(Debug, Clone)]
pub(super) struct TraitInfo {
    pub(super) methods: Vec<TraitMethod>,
    pub(super) associated_types: Vec<TraitAssociatedTypeInfo>,
    pub(super) type_params: Vec<String>,
}

#[derive(Debug, Clone)]
pub(super) struct TraitAssociatedTypeInfo {
    pub(super) name: String,
    /// Trait bounds declared on this associated type
    /// (e.g. `type Out: Display` → one `TraitBound { name: "Display", .. }`).
    /// Enforced at impl-registration time: an impl's `type Out = X` must
    /// supply a type `X` that satisfies every bound in this list.
    /// Stored as the full `TraitBound` (with `type_args`) so slice 2 of the
    /// associated-types lane can read `type_args` without a schema migration.
    pub(super) bounds: Vec<TraitBound>,
    pub(super) default: Option<Spanned<TypeExpr>>,
    /// Span of the `type Bar` declaration in the trait body.
    pub(super) span: Span,
}

#[derive(Debug)]
pub(super) struct ImplAliasScope {
    pub(super) span: Span,
    pub(super) entries: HashMap<String, ImplAliasEntry>,
    pub(super) missing_reported: HashSet<String>,
    pub(super) report_missing: bool,
}

#[derive(Debug)]
pub(super) struct ImplAliasEntry {
    pub(super) expr: Spanned<TypeExpr>,
    pub(super) resolved: Option<Ty>,
    pub(super) resolving: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariantDef {
    Unit,
    Tuple(Vec<Ty>),
    Struct(Vec<(String, Ty)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeDefKind {
    Struct,
    Enum,
    Actor,
    Machine,
    /// Immutable value-type record declared with the `record` keyword.
    ///
    /// Named-field form: `record Point { x: int, y: int }`.
    /// Tuple form: `record UserId(int)` — constructor registered as `fn_sig`;
    /// fields map is empty (no `.0`/`.1` access; positional destructuring only).
    Record,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(
    clippy::struct_excessive_bools,
    reason = "FnSig is the canonical fn-signature record; each bool encodes \
              a distinct cross-cutting attribute (async/kwargs/mutable-receiver) \
              that downstream passes need to query individually — collapsing into \
              an enum would force per-flag enum-variant matches at every read site"
)]
pub struct FnSig {
    pub type_params: Vec<String>,
    pub type_param_bounds: HashMap<String, Vec<String>>,
    pub param_names: Vec<String>,
    pub params: Vec<Ty>,
    pub return_type: Ty,
    pub is_async: bool,
    pub accepts_kwargs: bool,
    pub doc_comment: Option<String>,
    /// Structured `#[extern_symbol("…")]` attribute attached to the
    /// declaration that produced this signature, if any.
    ///
    /// Populated by the registration pass at FnSig-ingest time from
    /// the `attributes` slot on [`hew_parser::ast::ExternFnDecl`] /
    /// [`hew_parser::ast::FnDecl`] when the parser recorded an
    /// `extern_symbol` attribute. The Stage-1 attachment validation
    /// already rejects the attribute at invalid positions (free fn,
    /// actor, trait fn, type-decl method), so reaching this field
    /// means the declaration is a valid attachment site (`extern "C"`
    /// fn or `impl` method).
    ///
    /// Stage 2 (this field) records the parsed template — Stage 3
    /// expands it at each reachable monomorphization and emits a
    /// `MethodCallRewrite::RewriteToFunction { c_symbol, .. }`,
    /// replacing the `crate::stdlib::resolve_vec_method` path.
    ///
    /// `None` for every signature that does not carry the attribute.
    pub extern_symbol: Option<crate::extern_symbol::ExternSymbolSpec>,
    /// `true` iff this signature was declared with a mutable receiver
    /// (`fn next(var self)` or a named-receiver variant marked `var`).
    ///
    /// Populated by the registration pass when the first parameter is a
    /// receiver (per [`Self::is_receiver_param`]) AND `param.is_mutable` is
    /// set. Consumed by the method-dispatch site to enforce that the call
    /// receiver is a `var`-bound binding (mirrors the precedent in
    /// `methods.rs::step` — see Q297 Stage 1).
    ///
    /// Default `false` for every signature without a receiver, for
    /// signatures whose receiver was declared by-value, and for free
    /// functions whose first parameter happens to be named `self`.
    pub requires_mutable_receiver: bool,
    /// `true` iff this signature was declared with a `consuming self` receiver
    /// (the terminal single-consume surface: `fn build(consuming self) -> T`, a
    /// `#[linear]` type's consuming method).
    ///
    /// Populated by the registration pass from [`hew_parser::ast::FnDecl`]'s
    /// `consumes_self` flag. Consumed by the inherent-method dispatch site to
    /// mark the receiver moved (a later use surfaces `UseAfterMove`) and to
    /// record the per-call-site `method_call_consumes_receiver` flag so HIR
    /// lowers the receiver with `IntentKind::Consume` and the move-checker
    /// counts a `#[linear]` binding as exhausted. Distinct from
    /// `requires_mutable_receiver` — a consuming receiver is an ownership-move
    /// (`by-value self`), never an in-place mutation (`var self`); the two are
    /// mutually exclusive on a single receiver.
    ///
    /// Default `false` for every signature without a consuming receiver.
    pub consumes_receiver: bool,
    /// `true` iff this `fn_sig` was registered for a builtin enum variant
    /// (e.g. `LookupError::NotFound`, `SendError::Timeout`).
    ///
    /// When a user-declared enum variant shares the same bare name as a
    /// builtin variant, `fn_sigs` can only hold one entry for that bare
    /// name.  Marking the builtin entry lets the resolution pass prefer any
    /// locally-declared user variant that shadows it (local-shadows-global
    /// rule — local-shadows-global).
    ///
    /// Default `false` for every non-builtin-variant signature.
    pub is_builtin_variant: bool,
}

#[derive(Debug, Clone)]
pub(super) struct GenericLambdaSig {
    pub(super) call_sig: FnSig,
    pub(super) type_vars: Vec<TypeVar>,
}

impl Default for FnSig {
    fn default() -> Self {
        Self {
            type_params: vec![],
            type_param_bounds: HashMap::new(),
            param_names: vec![],
            params: vec![],
            return_type: Ty::Unit,
            is_async: false,
            accepts_kwargs: false,
            doc_comment: None,
            extern_symbol: None,
            requires_mutable_receiver: false,
            consumes_receiver: false,
            is_builtin_variant: false,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(super) struct TypeParamScope {
    pub(super) bounds: HashMap<String, Vec<String>>,
    pub(super) assoc_bindings: HashMap<(String, String, String), Ty>,
}

impl TypeParamScope {
    pub(super) fn new(
        bounds: HashMap<String, Vec<String>>,
        assoc_bindings: HashMap<(String, String, String), Ty>,
    ) -> Self {
        Self {
            bounds,
            assoc_bindings,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct DeferredInferenceHole {
    pub(super) span: Span,
    pub(super) context: String,
    pub(super) hole_vars: Vec<TypeVar>,
    /// Module path where this hole was recorded (None = root module).
    pub(super) source_module: Option<String>,
}

#[derive(Debug, Clone)]
pub(super) struct DeferredCastCheck {
    pub(super) span: Span,
    pub(super) actual: Ty,
    pub(super) target: Ty,
    pub(super) target_hole_vars: Vec<TypeVar>,
    /// Module path where this cast check was recorded (None = root module).
    pub(super) source_module: Option<String>,
}

#[derive(Debug, Clone)]
pub(super) struct DeferredMonomorphicSite {
    pub(super) span: Span,
    pub(super) context: String,
    pub(super) ty: Ty,
    pub(super) more_specific_hole_vars: Vec<TypeVar>,
    /// Module path where this site was recorded (None = root module).
    pub(super) source_module: Option<String>,
}

#[derive(Debug, Clone)]
pub(super) struct DeferredBoundCheck {
    pub(super) type_param: String,
    pub(super) bounds: Vec<String>,
    pub(super) assoc_bindings: Vec<(String, String, Ty)>,
    pub(super) type_arg: Ty,
    pub(super) span: Span,
}

/// Result of resolving a bare actor reference (`spawn Account(...)`, a bare
/// `LocalPid<Account>` inner name) against the local-first identity policy.
///
/// Produced by `Checker::resolve_bare_actor_identity`. `Resolved` carries the
/// registered identity key — bare for root/flat actors, dotted
/// `{module_short}.{name}` for module actors. `Ambiguous` carries the sorted
/// candidate module list for the typed diagnostic; resolution is never
/// silent first-wins.
#[derive(Debug, Clone)]
pub(super) enum BareActorResolution {
    Resolved(String),
    Ambiguous(Vec<String>),
    Unknown,
}

/// The main type checker.
#[derive(Debug)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "checker state flags are independent booleans"
)]
pub struct Checker {
    pub(super) env: TypeEnv,
    pub(super) subst: Substitution,
    pub(super) registry: TraitRegistry,
    pub(super) module_registry: ModuleRegistry,
    pub(super) errors: Vec<TypeError>,
    pub(super) warnings: Vec<TypeError>,
    /// Checker-side accumulator for [`TypeCheckOutput::user_clone_record_seeds`].
    pub(super) user_clone_record_seeds: Vec<String>,
    pub(super) expr_types: HashMap<SpanKey, Ty>,
    pub(super) is_type_patterns: HashMap<SpanKey, Ty>,
    pub(super) expr_type_source_modules: HashMap<SpanKey, Option<String>>,
    pub(super) method_call_receiver_kinds: HashMap<SpanKey, MethodCallReceiverKind>,
    pub(super) method_call_consumes_receiver: HashSet<SpanKey>,
    /// Codegen alias-vs-copy decision per actor send site.
    /// Mirrors [`TypeCheckOutput::actor_send_aliasing`]; populated in
    /// `enforce_actor_boundary_send` and moved out at the end of
    /// `check_program`.
    pub(super) actor_send_aliasing: HashMap<SpanKey, ActorSendAliasing>,
    /// Receive-handler actor-state guard policy produced by checker.
    /// Mirrors [`TypeCheckOutput::actor_handler_state_guards`].
    pub(super) actor_handler_state_guards: HashMap<SpanKey, ActorStateGuard>,
    /// Per-actor arena cap in bytes, from `#[max_heap(N)]` annotations.
    /// Mirrors [`TypeCheckOutput::actor_max_heap`]; populated in
    /// `check_actor` and moved out at the end of `check_program`.
    pub(super) actor_max_heap: HashMap<String, u64>,
    /// Qualified method names (e.g. `"Closable::close"`) whose dispatch should
    /// mark the receiver moved and propagate `consumes_receiver` into the
    /// per-call-site side table. Empty in PR 1 (issue #1295); PR 2 populates
    /// this set when `trait Closable` is registered. Tests may insert names
    /// directly to exercise the consume-marker path before PR 2 lands.
    pub(super) consume_receiver_methods: HashSet<String>,
    pub(super) pending_lowering_facts: HashMap<SpanKey, PendingLoweringFact>,
    /// `HashMap` key/value admission checks deferred until after inference
    /// completes.  Keyed by span to suppress duplicates from repeated
    /// traversals of the same site (annotation + method call on the same map).
    pub(super) deferred_hashmap_admission: HashMap<SpanKey, DeferredHashMapAdmission>,
    /// `HashSet` element admission checks deferred until after inference
    /// completes.  Keyed by span to suppress duplicates from repeated
    /// traversals of the same site (annotation + method call on the same set).
    pub(super) deferred_hashset_admission: HashMap<SpanKey, DeferredHashSetAdmission>,
    /// Layout-key `HashMap` lowering facts accumulated by `finalize_hashmap_admission`.
    ///
    /// Keyed by the span of the admission site (type annotation or method call).
    /// Drained into `TypeCheckOutput::hashmap_layout_facts` at the output boundary.
    pub(super) hashmap_layout_facts: HashMap<SpanKey, crate::lowering_facts::HashMapLoweringFact>,
    /// Layout-element `HashSet` lowering facts accumulated by `finalize_lowering_facts`.
    ///
    /// Keyed by the span of the call site that triggered the `HashSet` method.
    /// Drained into `TypeCheckOutput::hashset_layout_facts` at the output boundary.
    pub(super) hashset_layout_facts: HashMap<SpanKey, crate::lowering_facts::HashSetLoweringFact>,
    /// `Vec` element admission checks deferred until after inference
    /// completes. Keyed by span to suppress duplicates from repeated traversals
    /// of the same site.
    pub(super) deferred_vec_admission: HashMap<SpanKey, DeferredVecAdmission>,
    /// Channel method call rewrites deferred until after inference completes.
    /// Keyed by call-site span so repeated traversal of the same site is
    /// idempotent (last write wins, which is fine since the inner type is the
    /// same variable every time).
    pub(super) deferred_channel_rewrites: HashMap<SpanKey, DeferredChannelMethodRewrite>,
    pub(super) method_call_rewrites: HashMap<SpanKey, MethodCallRewrite>,
    /// Checker-side accumulator for [`TypeCheckOutput::wire_layouts`].
    pub(super) wire_layouts: WireLayoutTable,
    /// Checker-side accumulator for [`TypeCheckOutput::resolved_calls`].
    ///
    /// **Stage A:** never populated by production code paths. Reserved
    /// for Stage B's unified resolver. See `dispatch.rs` module docs and
    /// `TypeCheckOutput::resolved_calls`.
    pub(super) resolved_calls: HashMap<SpanKey, crate::check::dispatch::ResolvedCall>,
    pub(super) numeric_method_lowerings: HashMap<SpanKey, NumericMethodLowering>,
    pub(super) width_cast_lowerings: HashMap<SpanKey, WidthCastLowering>,
    pub(super) actor_method_dispatch: HashMap<SpanKey, ActorMethodKind>,
    /// Machine method dispatch side-table. Mirrors [`TypeCheckOutput::machine_method_dispatch`].
    pub(super) machine_method_dispatch: HashMap<SpanKey, MachineMethodKind>,
    /// `await conn.read()` suspending-read sites. Mirrors
    /// [`TypeCheckOutput::conn_await_reads`].
    pub(super) conn_await_reads: HashMap<SpanKey, bool>,
    /// `await listener.accept()` suspending-accept sites. Mirrors
    /// [`TypeCheckOutput::listener_await_accepts`].
    pub(super) listener_await_accepts: HashSet<SpanKey>,
    /// Function-tail Ok-coercion sites. Mirrors
    /// [`TypeCheckOutput::tail_ok_coercions`].
    pub(super) tail_ok_coercions: HashSet<SpanKey>,
    /// `true` while checking an expression that is the tail of a
    /// `Result`-returning function (and the if/match arm tails that flow to
    /// the function return). Armed in `check_fn_decl` only when the declared
    /// return is `Result<_, _>`, threaded through `check_block` /
    /// `check_stmt_as_expr` tail positions, and disarmed on entry to
    /// `synthesize` and around every non-tail sub-expression in
    /// `check_against`. Gates the tail Ok-coercion so it never fires in a
    /// non-tail expression position.
    pub(super) tail_ok_armed: bool,
    pub(super) assign_target_kinds: HashMap<SpanKey, AssignTargetKind>,
    pub(super) assign_target_shapes: HashMap<SpanKey, AssignTargetShape>,
    /// Diagnostic-only stack-allocation hints accumulated by `classify_stack_hints`.
    /// Surfaced through `TypeCheckOutput::stack_hints` and consumed by the CLI's
    /// `--show-stack-hints` printer. See [`StackHint`].
    pub(super) stack_hints: Vec<StackHint>,
    pub(super) type_defs: HashMap<String, TypeDef>,
    pub(super) fn_sigs: HashMap<String, FnSig>,
    pub(super) fn_type_param_assoc_bindings: HashMap<String, HashMap<(String, String, String), Ty>>,
    pub(super) handle_bearing_structs: HashSet<String>,
    /// Names of every user-declared `#[opaque]` type in this module.
    /// Populated by `register_type_decl` whenever `td.is_opaque` is true.
    /// Consumed by `record_clone_admissibility` to detect opaque fields in
    /// record types the user attempts to clone — these are ALWAYS non-cloneable
    /// because a shallow copy aliases the runtime handle.
    pub(super) user_opaque_type_names: HashSet<String>,
    /// `#[wire]` struct type names that carry the binary CBOR codec methods
    /// (`encode`/`decode`). Distinguishes the wire-codec `encode`/`decode` calls
    /// — which lower to the `__hew_cbor_serialize_*` / `__hew_cbor_deserialize_*`
    /// thunks — from a same-named user method, without re-deriving wire-ness in
    /// the method-dispatch arms. Populated by `register_wire_methods` for wire
    /// structs.
    pub(super) wire_struct_types: HashSet<String>,
    /// `#[wire]` enum type names that carry the binary CBOR codec methods
    /// (`encode`/`decode`). The enum body uses the "map-of-one"
    /// shape (`{tag: [payload]}`, unit variants = the bare tag). Parallel to
    /// `wire_struct_types` so the method-dispatch arms recognise the codec call
    /// for an enum receiver as well as a struct.
    pub(super) wire_enum_types: HashSet<String>,
    /// Set on every type registration; cleared once `ensure_handle_bearing_fresh`
    /// runs the fixpoint refresh. Converts O(N²) per-registration rescans to a
    /// single pass before the first lookup — see `ensure_handle_bearing_fresh`.
    pub(super) handle_bearing_dirty: bool,
    /// Incremented inside `refresh_handle_bearing_structs` to let tests verify
    /// the deferred-refresh optimisation holds (should be O(1) across N
    /// registrations, not O(N)).
    pub(super) refresh_call_count: usize,
    /// Qualified `Actor::method` names declared with `receive gen fn`.
    pub(super) receive_generator_methods: HashSet<String>,
    /// Qualified `Actor::method` names declared with `receive fn` (including
    /// generator receives). Used by the actor-mailbox boundary enforcement
    /// to distinguish receive handlers from non-receive `methods` declared
    /// on the same actor (which are also keyed `{Actor}::{name}` in
    /// `fn_sigs` but must NOT cross the mailbox boundary).
    pub(super) actor_receive_methods: HashSet<String>,
    pub(super) type_def_inference_holes: HashMap<String, Vec<TypeVar>>,
    pub(super) fn_sig_inference_holes: HashMap<String, Vec<TypeVar>>,
    pub(super) deferred_inference_holes: Vec<DeferredInferenceHole>,
    pub(super) deferred_cast_checks: Vec<DeferredCastCheck>,
    pub(super) deferred_monomorphic_sites: Vec<DeferredMonomorphicSite>,
    /// Tracks the span and originating module where each function was first defined
    /// (for duplicate detection and dead-code source attribution).
    pub(super) fn_def_spans: HashMap<String, (Span, Option<String>)>,
    /// Declared visibility for each function, keyed by the same registry key used
    /// in `fn_def_spans` (scoped name: `{module}.{name}` or bare for root).
    /// Populated alongside `fn_def_spans` during `collect_function_item` and when
    /// cross-module functions are registered in the import-surface passes.
    pub(super) fn_visibility: HashMap<String, Visibility>,
    /// Tracks the span where each top-level type/trait namespace name was first defined.
    pub(super) type_def_spans: HashMap<String, Span>,
    /// Declared visibility for each top-level type/trait/enum/record/alias/const/
    /// actor/machine, keyed by the qualified registry identity (`{module}.{Name}` or
    /// bare for root).  Populated during both the own-module and cross-module
    /// registration passes so the enforcement check can answer "who declared this
    /// and with what visibility" at every reference site.
    pub(super) type_visibility: HashMap<String, (Visibility, Option<String>)>,
    /// Per-module type-name uniqueness ledger. Keyed by `(defining-module, name)`
    /// so two different modules may each declare a `pub type` with the same bare
    /// name (e.g. `json.Value` and `toml.Value`) without colliding, while a
    /// second declaration of the same name *within one module* is still a
    /// `duplicate_definition` error. `None` is the root/flat-file namespace (the
    /// user's own program and flat file imports share one module). Builtins are
    /// registered without going through this gate and stay globally visible.
    pub(super) type_namespace_owners: HashMap<(Option<String>, String), Span>,
    /// Tracks public top-level names introduced by prior flat file imports so later
    /// flat imports can reject collisions instead of silently overwriting them.
    pub(super) flat_file_import_pub_spans: HashMap<String, Span>,
    /// Canonical source paths for flat file imports already registered in the
    /// current checker run so repeated imports stay idempotent.
    pub(super) registered_flat_file_import_sources: HashSet<PathBuf>,
    /// Tracks stdlib Hew modules whose public Hew items have already been registered.
    /// Uses the canonical entry source path when available, and falls back to the
    /// module path for callers that only populate `resolved_items`.
    pub(super) registered_stdlib_hew_sources: HashSet<String>,
    pub(super) generic_ctx: Vec<HashMap<String, Ty>>,
    /// Parallel stack to `generic_ctx`: bounds declared on each generic type
    /// parameter in the current scope (fn, impl, receive-fn), plus concrete
    /// associated-type bindings from those bounds (e.g. `Item = i64` for
    /// `<I: Iterator<Item = i64>>`). Pushed/popped alongside `generic_ctx` by
    /// signature registration; consulted by the resolver when it encounters
    /// `T::Bar` projections to find which trait declares `Bar` and whether a
    /// where-clause binding should collapse the projection immediately.
    ///
    /// Why a separate stack rather than enriching `generic_ctx`: `generic_ctx`
    /// maps name → `Ty` and is consumed by many call sites that only care
    /// about substitution. Bounds are slice-2-specific. Keeping them apart
    /// avoids invalidating every existing reader.
    pub(super) current_type_param_bounds: Vec<TypeParamScope>,
    /// Trait bounds declared on each machine's generic type parameters, keyed
    /// by machine name. Populated during `register_machine_decl` from
    /// `MachineDecl.type_params`. Consulted at the use site by
    /// `check_struct_init` (struct-state brace constructor path) where no
    /// `FnSig` pipeline carries the bounds.
    pub(super) machine_type_param_bounds: HashMap<String, HashMap<String, Vec<String>>>,
    /// Const-generic parameters declared on each machine, keyed by
    /// machine name. Populated during [`register_machine_decl`] from
    /// `MachineDecl::const_params`. Used at instantiation sites
    /// (Stage 3 — gated on W3.033c Stage 2) for arity matching and
    /// to recover declaration-time default values when a call site
    /// omits a const-arg.
    ///
    /// Each entry stores the parameter declarations in source-declared
    /// order (the same order they appear in the `<...>` list, with
    /// type params preceding const params per the parser convention).
    pub(super) machine_const_params: HashMap<String, Vec<MachineConstParamDecl>>,
    /// Dedup set for `enforce_machine_instantiation_bounds`. Keyed by
    /// `(machine_name, resolved_type_args, span_key)`: the same
    /// annotation can be walked multiple times during checking — for
    /// example once for `FnSig` registration, once for body
    /// resolution, and the recursive walker in `resolve_type_expr`
    /// may also re-visit shared nested positions across overlapping
    /// resolution paths. Without dedup, every duplicate visit emits
    /// an identical `BoundsNotSatisfied` diagnostic. The key
    /// combines machine name, resolved args, and span: two textually
    /// identical instantiations at different source positions are
    /// distinct violations and must each report once.
    pub(super) reported_machine_bound_violations: HashSet<(String, Vec<Ty>, SpanKey)>,
    /// Dedup set for declaration-level generic type bounds. Keyed by
    /// `(type_name, resolved_type_args, span_key)`: type annotations,
    /// constructor synthesis, and expected-type coercion may all observe the
    /// same nominal instantiation, but the reference site should emit one
    /// `BoundsNotSatisfied` diagnostic.
    pub(super) reported_type_def_bound_violations: HashSet<(String, Vec<Ty>, SpanKey)>,
    /// Trait bounds declared on each actor's generic type parameters, keyed by
    /// actor name. Populated during `register_actor_decl` from
    /// `ActorDecl.type_params`. Consulted at the use site by
    /// `check_spawn` to enforce bounds on explicitly supplied type args.
    ///
    /// Mirrors `machine_type_param_bounds` — the clone-pattern is deliberate;
    /// actors and machines share bound-enforcement semantics but are separate
    /// declaration kinds. Do not collapse: actor and machine bound tables have
    /// distinct lookup scopes.
    pub(super) actor_type_param_bounds: HashMap<String, HashMap<String, Vec<String>>>,
    /// Dedup set for `enforce_actor_instantiation_bounds`. Mirrors
    /// `reported_machine_bound_violations` but scoped to actor spawns so
    /// that machine and actor violations cannot accidentally suppress each other.
    pub(super) reported_actor_bound_violations: HashSet<(String, Vec<Ty>, SpanKey)>,
    /// Actors declaring at least one `#[every(duration)]` periodic receive
    /// handler, keyed by actor name; the value is the first periodic
    /// handler's name (for diagnostics). Populated during
    /// `register_actor_decl`, consulted by `check_supervisor` to reject
    /// child specs whose runtime spawn path cannot arm periodic timers.
    pub(super) actors_with_periodic_handlers: HashMap<String, String>,
    pub(super) current_return_type: Option<Ty>,
    pub(super) in_generator: bool,
    /// Set to `true` for the duration of synthesizing the inner expression of
    /// `Expr::Await(inner)`.  Enables `check_named_method_fallback` to
    /// distinguish an actor ask under `await` (valid) from an actor ask without
    /// `await` (rejected: requires explicit `await`).
    pub(super) inside_await_expr: bool,
    pub(super) loop_depth: u32,
    /// Labels of enclosing loops, for validating `break @label` / `continue @label`.
    pub(super) loop_labels: Vec<String>,
    pub(super) modules: HashSet<String>,
    pub(super) known_types: HashSet<String>,
    pub(super) type_aliases: HashMap<String, Ty>,
    pub(super) trait_defs: HashMap<String, TraitInfo>,
    /// Maps trait name → list of super-trait names (e.g., `Pet` → [`Animal`])
    pub(super) trait_super: HashMap<String, Vec<String>>,
    /// A declaring module's trait import bindings:
    /// `(declaring_module_short, name_as_spelled)` → owner-qualified SOURCE
    /// identity (`{owner_short}.{Source}`), always a registered `trait_defs` key.
    ///
    /// This is what resolves a RE-EXPORTED supertrait edge. A supertrait spelled
    /// `Base` inside a module that itself imported `Base` (`import other::{ Base }`)
    /// names `other`'s trait, not the same-named `Base` a downstream importer may
    /// also have in scope. The same-module qualified key (`{declaring}.Base`) only
    /// covers a supertrait declared in the SAME module; when the super was
    /// re-imported there is no `{declaring}.Base` def, and resolving the bare name
    /// in the final importer's namespace binds the wrong owner (the H11 fail-open).
    /// For `reexsub` (`import reexbase::{ Base }`) this records
    /// `("reexsub", "Base") → "reexbase.Base"`; for an aliased re-import
    /// (`import reexbase::{ Base as B }`) it records `("reexsub", "B") → "reexbase.Base"`,
    /// so the alias renames the binding without losing the source identity.
    pub(super) trait_import_bindings: HashMap<(String, String), String>,
    /// Set of (`type_name`, `trait_name`) pairs for concrete impl registrations
    pub(super) trait_impls_set: HashSet<(String, String)>,
    /// Dedup guard so a rejected overlapping primitive/builtin trait impl emits
    /// only one `ConflictingTraitImpl` diagnostic even though
    /// `record_primitive_trait_impl_self_args` runs once per impl method. Keyed
    /// by (`canonical_constructor`, `trait_name`, `span.start`, `span.end`).
    pub(super) conflicting_trait_impl_reported: HashSet<(String, String, usize, usize)>,
    /// Method names provided by each concrete trait impl block, keyed by
    /// (`type_name`, `trait_name`) as written on the impl. This preserves
    /// provenance that is lost when impl methods are flattened onto the type's
    /// receiver-method table.
    pub(super) trait_impl_method_names: HashMap<(String, String), HashSet<String>>,
    /// Trait impls keyed by canonical receiver kind for primitives and
    /// compiler-builtin generics (e.g. `int`, `bool`, `String`, `Vec`,
    /// `HashMap`, `HashSet`, `Bytes`).  Method dispatch on these receivers
    /// has no `type_defs` entry to hang user-impl methods off, so the side
    /// table is the only place those signatures live.
    ///
    /// Outer key: (`canonical_primitive_or_builtin_name`, `trait_name`).
    /// Inner: method name → resolved `FnSig` (receiver already filtered).
    pub(super) primitive_trait_impls: HashMap<(String, String), HashMap<String, FnSig>>,
    /// The impl's `Self` type arguments for each `impl <Trait> for
    /// <PrimitiveOrBuiltinGeneric>`, captured at registration so dispatch can
    /// bind the impl's type parameters from a concrete receiver's type
    /// arguments before applying the signature.
    ///
    /// For `impl<E> Index for Vec<E>` the entry is `[Ty::Named { name: "E" }]`;
    /// at a `v: Vec<i64>` call site, structurally unifying these stored args
    /// against the receiver's `[i64]` yields the substitution `E → i64`, so the
    /// method's `Option<E>` / `Self::Output` return projects to a concrete type
    /// instead of leaking an unresolved inference var past the checker output
    /// boundary. Non-generic primitive impls (`impl Display for i64`) store an
    /// empty vec and are a no-op on dispatch.
    ///
    /// Key: (`canonical_primitive_or_builtin_name`, `trait_name`) — same as
    /// [`Self::primitive_trait_impls`].
    pub(super) primitive_trait_impl_self_args: HashMap<(String, String), Vec<Ty>>,
    /// Maps supervisor name to its partitioned child lists.
    ///
    /// Static children (declared with `child name: Type`) are in `statics`,
    /// pool children (declared with `pool name: Type`) are in `pools`.
    /// The slot index for each child is its 0-based position within its own list,
    /// matching the runtime layout (`HewSupervisor.children[]` for static,
    /// `HewSupervisor.pool_slots[]` for pool).
    pub(super) supervisor_children: HashMap<String, SupervisorChildren>,
    /// Side-table populated during field-access type-checking for supervisor children.
    ///
    /// Keyed by the `SpanKey` of the field-access expression. Moved into
    /// `TypeCheckOutput::supervisor_child_slots` at the end of `check_program`.
    pub(super) supervisor_child_slots: HashMap<SpanKey, ChildSlot>,
    /// Resolved static-pool accessors. Moved into
    /// `TypeCheckOutput::pool_accessor_sites` at the end of `check_program`.
    pub(super) pool_accessor_sites: HashMap<SpanKey, PoolAccessor>,
    /// Side-table populated during `T → dyn Trait` coercion checking.
    ///
    /// Keyed by the `SpanKey` of the call-site argument expression. Moved
    /// into `TypeCheckOutput::dyn_trait_coercions` at the end of
    /// `check_program`.
    pub(super) dyn_trait_coercions: HashMap<SpanKey, DynCoercion>,
    /// Side-table populated during method-call type-checking on a `dyn Trait`
    /// receiver. Keyed by the `SpanKey` of the method-call expression. Moved
    /// into `TypeCheckOutput::dyn_trait_method_calls` at the end of
    /// `check_program`.
    pub(super) dyn_trait_method_calls: HashMap<SpanKey, DynMethodCall>,
    /// Binding-accurate closure capture facts keyed by closure literal span.
    pub(super) closure_capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>>,
    /// Per-closure escape classification keyed by closure literal span.
    /// Moved into `TypeCheckOutput::closure_escape_facts` at `check_program` exit.
    pub(super) closure_escape_facts: HashMap<SpanKey, ClosureEscapeFact>,
    /// Spans that already emitted the `ClosureEscapeAdvisory` warning. The
    /// escape classifier visits a literal more than once (the let-bound block
    /// walk and the anonymous-expression walk, and the top-level item list
    /// plus the module graph both cover the entry module), so the advisory
    /// is gated on first-insert per span — one warning per closure literal,
    /// distinct literals still warn independently.
    pub(super) closure_escape_advisory_spans: HashSet<SpanKey>,
    /// Maps actor name to its resolved `init()` parameter list.
    ///
    /// Used by the supervisor checker (S-B) to validate `wired_to:` type compatibility.
    pub(super) actor_init_params: HashMap<String, Vec<ActorInitParamInfo>>,
    /// When set, records the scope depth at which a lambda was entered.
    /// Variable lookups from scopes below this depth are captures.
    pub(super) lambda_capture_depth: Option<usize>,
    /// Captured variable types accumulated during lambda body checking.
    pub(super) lambda_captures: Vec<Ty>,
    /// Binding-accurate capture facts accumulated during lambda body checking.
    pub(super) lambda_capture_facts: Vec<ClosureCaptureFact>,
    /// Tracks imported module paths with their source spans and originating module for
    /// unused-import detection and source attribution.
    /// Key: (`owner_module`, `short_name`), Value: (import span, source module).
    pub(super) import_spans: HashMap<ImportKey, (Span, Option<String>)>,
    /// Import keys that have actually been referenced in code.
    pub(super) used_modules: RefCell<HashSet<ImportKey>>,
    /// Module short names for user (non-stdlib) imports.
    pub(super) user_modules: HashSet<String>,
    /// Qualified callable names (`module.name`) that are intentionally exported
    /// through a module surface. Keeps module-qualified calls from resolving
    /// against private helper signatures that exist only for body checking/codegen.
    pub(super) module_fn_exports: HashSet<String>,
    /// Per-imported-module set of exported type/actor names. Mirrors the
    /// `module_fn_exports` precedent but for `module.Type` references. Drives
    /// the pre-dispatch in `check_field_access` for module-qualified value
    /// constructors (`m.Type::Variant`) so the diagnostic surface can name the
    /// failure precisely (e.g. `module 'm' has no exported type 'T'`) instead of
    /// leaking through to the bare-identifier "undefined variable" fallback.
    pub(super) module_type_exports: HashMap<String, HashSet<String>>,
    /// Qualified type names (`module.Type`) for which a visibility-violation
    /// diagnostic has already been emitted in the current check pass.  Prevents
    /// duplicate `E_VISIBILITY` errors when the same private/package type appears in
    /// multiple positions (e.g. both a parameter and the return type of one fn).
    pub(super) reported_type_visibility_violations: HashSet<String>,
    /// `(resolved_name, span)` pairs for which an `unknown type` diagnostic has
    /// already been emitted, so a named type that resolves to nothing is reported
    /// exactly once even though signature resolution (`collect_functions`) and
    /// body resolution (`resolve_param_binding_ty`) both visit the same parameter
    /// annotation.
    pub(super) reported_undefined_named_types: HashSet<(String, SpanKey)>,
    /// `false` until `collect_types` has registered every type declaration.
    /// The undefined-named-type guard in `resolve_type_expr_tracking_holes`
    /// only fires once this is `true`: type-declaration member resolution runs
    /// during registration, before forward-referenced sibling types are known,
    /// so emitting there would false-positive on legal forward references.
    /// Signature and body resolution run strictly after registration completes.
    pub(super) type_decls_registered: bool,
    /// When `true`, the undefined-named-type guard in
    /// `resolve_type_expr_tracking_holes` resolves an unknown name to an
    /// opaque `Ty::named` (its pre-F1 behaviour) WITHOUT emitting a diagnostic
    /// or substituting `Ty::Error`. Set around `reresolve_member_types_after_imports`,
    /// the secondary pass that recomputes type-declaration MEMBER types once
    /// imports are visible: a declaration's members are out of this diagnostic's
    /// remit (they keep the existing `E_MIR: unknown type` path), and emitting
    /// there would also let the guard overwrite a member's already-computed type
    /// with `Ty::Error`, corrupting it and tripping the HIR field-access
    /// checker-boundary conversion downstream.
    pub(super) suppress_undefined_type_report: bool,
    /// Every type-parameter name declared anywhere in the program — on type /
    /// record / trait / impl / machine / actor declarations and on every
    /// generic method or free function. The undefined-named-type guard treats
    /// a name in this set as resolvable at SECONDARY re-resolution sites: the
    /// resolver intentionally leaves a generic type parameter opaque
    /// (`Ty::named`) and re-resolves it during signature rebuilds, receiver
    /// probes and trait-conformance comparison without re-pushing its scope, so
    /// a scope-local check alone false-positives there. This set is a uniform
    /// fallback for those sites only — the PRIMARY resolution of a top-level
    /// function's own signature suppresses it (see `scope_local_type_params_only`)
    /// so an out-of-scope generic name is still reported. Populated once, after
    /// `collect_types`, before the guard is armed.
    pub(super) declared_type_param_names: HashSet<String>,
    /// When `true`, the undefined-named-type guard proves a type-parameter name
    /// resolvable ONLY through the in-scope tables (`current_type_param_bounds`
    /// and the current function's registered signature params), ignoring the
    /// program-wide `declared_type_param_names` fallback. Set around the primary
    /// resolution of a top-level free function's own signature, where that
    /// function's type-param scope is reliably reconstructed (its bounds frame
    /// is pushed before the annotations resolve). The program-wide set is too
    /// broad to prove a SOURCE annotation valid there: `fn id<T>(x: T)` declares
    /// `T`, but `fn bad(x: T)` does not, and `bad`'s `T` must be reported as
    /// unknown rather than silently exempted by `id`'s unrelated `T`. Left
    /// `false` (the default) at every secondary re-resolution site, which still
    /// relies on the program-wide fallback.
    pub(super) scope_local_type_params_only: bool,
    /// Every NOMINAL type name declared anywhere in the program and its
    /// `module_graph` — type / type-alias / record / trait / actor / supervisor /
    /// machine declarations (plus the synthesised `<Machine>Event` companion).
    /// The undefined-named-type guard treats a name in this set as resolvable.
    ///
    /// The guard runs after the root module's `collect_types`, but signatures of
    /// imported `module_graph` modules are registered in a LATER pass where that
    /// module's own traits/types are not in the active `trait_defs` / `known_types`
    /// (those carry the root module's declarations) nor yet in the module-scoped
    /// `local_*` sets. A `LocalPid<ConnectionHandler>` inside an imported
    /// `std::net` would therefore false-positive against the per-pass tables.
    /// Consulting this program-wide set makes any declared nominal type resolve
    /// uniformly regardless of which pass is running. A genuinely undefined type
    /// (`Bogus`) is declared nowhere, so it is still caught; cross-module import
    /// VISIBILITY (is the name imported into this scope) is a separate check that
    /// this diagnostic does not own. Populated once, after `collect_types`,
    /// before the guard is armed.
    pub(super) declared_nominal_type_names: HashSet<String>,
    /// Maps (`owner_module`, `unqualified_name`) to the module short name the name
    /// was imported from.  Used to mark the owning import as used when an
    /// unqualified function/type is referenced.
    ///
    /// Single-valued: with two opt-ins of the same bare name it retains only the
    /// last writer. For deciding *whether* a bare reference is ambiguous, use
    /// `published_bare_type_owners` instead, which keeps the full set.
    pub(super) unqualified_to_module: HashMap<(Option<String>, String), String>,
    /// Maps (`importer_module`, `bare_type_binding`) to the full set of SOURCE
    /// identities (`owner.OriginalName`) published under that bare binding into
    /// the importer's scope (via a named / glob / aliased opt-in, or a prelude
    /// bootstrap surface).
    ///
    /// The value is the SOURCE identity, not merely the owner module, so an
    /// aliased import (`import m::{ T as U }`) records `U -> { "m.T" }`: a use of
    /// bare `U` resolves to the type `m` exports under `T`, never a phantom
    /// `m.U`.
    ///
    /// Drives the use-time ambiguity decision: a bare reference is ambiguous only
    /// when more than one source identity is published under the binding — a
    /// plain `import` that exported but did not publish the name does not
    /// contribute, so it cannot poison an explicit named import of the same bare
    /// name from another module.
    pub(super) published_bare_type_owners:
        HashMap<(Option<String>, String), std::collections::BTreeSet<String>>,
    /// Maps (`importer_module`, `trait_binding`) to the full set of SOURCE
    /// trait identities (`owner.OriginalTrait`) published under that binding into
    /// the importer's scope. The trait-namespace analogue of
    /// `published_bare_type_owners`.
    ///
    /// An aliased trait import (`import m::{ Trait as T }`) records
    /// `T -> { "m.Trait" }`. The trait-conformance check (`check_impl_method_
    /// against_trait`) reads this to recover the SOURCE identity for an aliased
    /// trait so it can (a) look up the trait's method signatures under the
    /// source/qualified key (`m.Trait::method`) instead of missing on the alias
    /// key (`T::method`) and accepting the impl unchecked, and (b) qualify bare
    /// type names written in the trait declaration against the SOURCE owner
    /// (`m`), not the alias. A single source identity is the well-formed case;
    /// zero or an ambiguous set falls back to the alias-keyed behaviour.
    pub(super) published_bare_trait_owners:
        HashMap<(Option<String>, String), std::collections::BTreeSet<String>>,
    /// Call graph: maps caller function name → set of callee function names.
    pub(super) call_graph: HashMap<String, HashSet<String>>,
    /// Name of the function currently being checked (for call graph tracking).
    pub(super) current_function: Option<String>,
    /// Whether we are currently inside a for-loop binding (suppress shadowing for loop vars).
    pub(super) in_for_binding: bool,
    /// Names actually bound by each top-level pattern, keyed by the pattern's
    /// span. Recorded by `bind_pattern` — the single binder-vs-constructor
    /// authority — as the env delta it introduces (a constructor identifier
    /// binds nothing and so contributes no name). Consumed by the borrowed-Rc
    /// escape scanner's `shadow_pattern_bindings`, which shadows exactly the
    /// recorded names rather than re-deriving the binder/constructor decision.
    /// Scratch state: cleared at the start of each function body check.
    pub(super) pattern_bound_names: HashMap<SpanKey, Vec<String>>,
    /// Re-entrancy guard ensuring only the *top-level* `bind_pattern` call
    /// records its env delta into `pattern_bound_names`; nested sub-pattern
    /// binds are already part of that single delta.
    pub(super) bind_pattern_recording: bool,
    /// Whether we are currently inside an actor receive function body.
    /// Used to warn about blocking calls that can starve the scheduler.
    pub(super) in_receive_fn: bool,
    /// Whether execution-context surface readers may resolve in the current body.
    ///
    /// Set for actor dispatch handlers and lifecycle hooks; cleared for nested
    /// lambdas so `@actor_id`-style readers cannot silently capture a dispatch
    /// context across an unmodelled closure boundary.
    pub(super) in_actor_handler_context: bool,
    /// Whether we are currently inside a lambda-actor body (`actor |...| { ... }`).
    ///
    /// Set to `true` when `check_lambda` is called from `SpawnLambdaActor`;
    /// cleared when that same `check_lambda` invocation saves/restores state.
    /// Distinct from `in_actor_handler_context` (which is for `receive fn` bodies
    /// inside named actors). Used by `check_call` to permit recursive self-sends
    /// (a Duplex capture called from within its own actor body) while rejecting
    /// fn-closure captures of Duplex handles.
    pub(super) in_lambda_actor_body: bool,
    /// Whether we are currently inside an unsafe block.
    pub(super) in_unsafe: bool,
    /// Nesting depth of `scope { }` structured-concurrency blocks in the
    /// expression currently being checked. `fork name = call(...)` child
    /// bindings are only valid at depth > 0; the counter is cleared for
    /// nested lambda bodies because a closure body does not inherit the
    /// lexical task scope it was written inside (it may run after the
    /// scope has joined).
    pub(super) task_scope_depth: u32,
    /// The module currently being processed (enables per-module scoping in future).
    pub(super) current_module: Option<String>,
    /// 1-based index of the non-root module currently being type-checked.
    /// 0 = root; N = N-th non-root module in topo order. Combined with
    /// `SpanKey.module_idx` to prevent byte-offset collisions across files.
    pub(super) current_module_idx: u32,
    /// Tracks which types are defined locally (in the current compilation unit).
    pub(super) local_type_defs: HashSet<String>,
    /// Tracks source-authored type definitions that can shadow builtin names.
    pub(super) source_type_defs: HashSet<String>,
    /// Tracks which traits are defined locally (in the current compilation unit).
    pub(super) local_trait_defs: HashSet<String>,
    /// The type name and args of the current impl block target (for resolving `Self`).
    pub(super) current_self_type: Option<(String, Vec<Ty>)>,
    /// The actor type currently being checked (for `this` keyword resolution).
    pub(super) current_actor_type: Option<Ty>,
    /// State fields of the current actor: name, declared mutability, and
    /// declaration site. Drives the purity checks on bare field assignment
    /// and the immutable-field assignment diagnostic (a `let` or bare field
    /// may only be assigned inside `init`; handlers and methods must declare
    /// the field with `var` to write it).
    pub(super) current_actor_fields: Vec<ActorFieldInfo>,
    /// Actor protocol descriptors (`receive fn` → stable hash-derived `msg_id`),
    /// built once before body checking so the active-mode
    /// `LocalPid<Actor>` → `LocalPid<ConnectionHandler>` coercion can confirm an
    /// actor's `receive fn`s structurally satisfy a handler trait. Moved into
    /// `TypeCheckOutput::actor_protocol_descriptors` at the end of
    /// `check_program` (no rebuild — see `actor_satisfies_handler_trait`).
    pub(super) actor_protocol_descriptors:
        HashMap<String, crate::actor_protocol::ActorProtocolDescriptor>,
    pub(super) impl_alias_scopes: Vec<ImplAliasScope>,
    /// When set, the resolver is inside a trait-body context that gives
    /// meaning to `Self::Bar` as a projection into this trait's associated
    /// types. Used to materialise a deferred `Ty::AssocType { base: Self,
    /// trait_name, assoc_name }` when no impl scope is active (e.g.
    /// registering a trait method signature). The carrier is collapsed at
    /// call sites where `Self` is substituted by a concrete type.
    pub(super) current_trait_for_self_projection: Option<String>,
    /// Resolved impl-side `type Bar = X` bindings, keyed by
    /// `(impl_type_name, trait_name, assoc_name)`. Populated at impl
    /// registration so that downstream projection-collapse
    /// (`project_assoc_types`) can substitute `Ty::AssocType` carriers once
    /// their `base` becomes concrete.
    ///
    /// Distinct from `ImplAliasScope.entries`, which is the per-impl scope
    /// stack used for `Self::Bar` lookup during impl-body checking.
    pub(super) impl_assoc_type_bindings: HashMap<(String, String, String), Ty>,
    /// Names of functions that require an unsafe block to call.
    pub(super) unsafe_functions: HashSet<String>,
    /// Whether warnings for WASM-only builds should be emitted.
    pub(super) wasm_target: bool,
    /// Whether the program under check is a synthetic `hew eval` REPL fragment.
    ///
    /// When `true`, the whole-program completeness lints (`DeadCode`,
    /// `UnusedImport`, `UnusedVariable`, `UnusedMut`) are suppressed: a binding,
    /// helper, or import that looks unused in one accumulated fragment is
    /// routinely referenced by a later REPL input, so emitting those warnings
    /// is noise rather than signal. Set only by the eval paths.
    pub(super) repl_fragment: bool,
    /// Whether the checker is currently type-checking a stdlib (or built-in
    /// library) source body.
    ///
    /// When `true`, scope-level diagnostic lints (`UnusedVariable`,
    /// `UnusedMut`) emitted by `emit_scope_warnings` are suppressed: they
    /// describe implementation details of the standard library that the user
    /// cannot act on and should not see.  Set transiently in `check_program`
    /// for each non-root module whose path starts with `"std"`, `"hew"`, or
    /// `"ecosystem"`, and cleared once that module's items have been checked.
    /// Distinct from `repl_fragment` (which suppresses all completeness lints
    /// across the whole program): `is_stdlib_source` is scoped per-module so
    /// user code in the same compilation unit still receives its own warnings.
    pub(super) is_stdlib_source: bool,
    /// Set to `true` while `register_stdlib_hew_items` is processing a stdlib
    /// type declaration. Enum-variant `fn_sig` entries inserted in this state carry
    /// `is_builtin_variant: true`, enabling the local-shadows-global rule: if a
    /// user-declared enum in `local_type_defs` has a variant with the same bare
    /// name, body-checking (`check_identifier`) prefers the user's declaration.
    pub(super) in_stdlib_registration: bool,
    /// Tracks (span, feature) pairs we've already warned about for WASM limits.
    pub(super) wasm_warning_spans: HashSet<(SpanKey, WasmUnsupportedFeature)>,
    /// Tracks (span, feature) pairs we've already rejected as errors for WASM.
    /// Separate from `wasm_warning_spans` to allow independent deduplication.
    pub(super) wasm_reject_spans: HashSet<(SpanKey, WasmUnsupportedFeature)>,
    /// Tracks slice annotation spans we've already rejected so repeated
    /// resolution passes don't emit duplicate diagnostics.
    /// Inside a machine transition body, the (`machine_name`, `source_state_name`, `event_name`) tuple.
    pub(super) current_machine_transition: Option<(String, String, String)>,
    /// Inside a machine state `entry` or `exit` lifecycle block, the
    /// (`machine_name`, `state_name`) pair.  Enables payload-state field
    /// access (`state.seq`) without granting transition-event privileges
    /// (no event-enum matching, no `event.field` binding).  Checked after
    /// `current_machine_transition` in the field-access resolver.
    pub(super) current_machine_lifecycle: Option<(String, String)>,
    /// Compile-time known numeric literal values used by later coercion sites.
    pub(super) const_values: HashMap<String, ConstValue>,
    /// Inferred type arguments for generic function calls that omit explicit
    /// type annotations.  Populated in `check_call` after argument unification.
    pub(super) call_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Inferred or explicit type arguments for record / enum-struct-variant
    /// initialiser sites on user-defined generic types. Populated by
    /// `record_concrete_record_init_type_args` at each emission site.
    ///
    /// Entries are emitted unconditionally (args may still carry a `Ty::Var`
    /// when a coercion arm resolves them only after `check_struct_init`
    /// returns). The fail-closed contract (no `Ty::Var` crosses into HIR) is
    /// enforced at the output boundary by
    /// `validate_record_init_type_args_output_contract` in `admissibility.rs`.
    pub(super) record_init_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Builtin `Ok`/`Err` constructor calls whose output type may need
    /// checked-output fallback when one side remains unconstrained.
    pub(super) builtin_result_output_type_args: HashMap<SpanKey, (Ty, Ty)>,
    /// Trait-bound checks deferred until inference/defaulting settles.
    pub(super) deferred_bound_checks: Vec<DeferredBoundCheck>,
    /// Maps a let-binding's definition span to the generic lambda call
    /// signature captured when that binding was type-checked. Used to freshen
    /// type variables per call site and populate `call_type_args`.
    pub(super) lambda_poly_sig_map: HashMap<SpanKey, GenericLambdaSig>,
    /// Scratch field: set by `check_lambda` when it processes a generic lambda
    /// (one with non-empty `type_params`). Consumed immediately in the
    /// enclosing `Stmt::Let` handler to populate `lambda_poly_sig_map`.
    pub(super) last_lambda_generic_sig: Option<GenericLambdaSig>,
    /// Range bounds whose element type is deferred until surrounding inference
    /// settles. Each entry is:
    ///   (outer-span, element-TypeVar, literal-value-if-any, inner-operand-span-if-negated)
    /// The fourth field carries the inner literal span when the bound is a
    /// negated integer literal (`-5`) so `apply_deferred_range_bound_types`
    /// can re-record the inner span too.  Without this, the inner literal's
    /// recorded type remains `IntLiteral` → `I64` (after
    /// `materialize_literal_defaults`), while the outer span is correctly
    /// narrowed to the resolved type (e.g. `I32`), causing a
    /// `UnaryOperatorUnsupportedInMir` width mismatch in HIR.
    /// Processed in `apply_deferred_range_bound_types` after all inference and
    /// literal defaulting is complete.
    // The tuple carries (span, var, literal_value, inner_span, module_idx). A named
    // struct would be cleaner but the field is private and short-lived — it never
    // escapes the deferred-resolution pass.
    #[allow(
        clippy::type_complexity,
        reason = "short-lived tuple; a named struct would be overkill for an internal deferred-resolution pass"
    )]
    pub(super) deferred_range_bounds: Vec<(Span, TypeVar, Option<i64>, Option<Span>, u32)>,
    /// Intrinsic declarations seen during registration: fn name → intrinsic key.
    ///
    /// Populated in `register_fn` for functions with `#[intrinsic("key")]`.
    /// Moved into `TypeCheckOutput::intrinsic_declarations` at `check_program` exit.
    pub(super) intrinsic_declarations: HashMap<String, String>,
    /// Per-arm pattern resolutions accumulated during match checking.
    ///
    /// Mirrors [`TypeCheckOutput::pattern_resolutions`]; keyed by the arm's
    /// pattern span.  `Ty` values may carry inference variables at insertion
    /// time; `Substitution::resolve` is applied at the `check_program` output
    /// boundary before the map is moved into `TypeCheckOutput`.
    pub(super) pending_pattern_resolutions: HashMap<SpanKey, ArmResolution>,
    /// Lang-item registry accumulated during trait registration.
    ///
    /// Mirrors [`TypeCheckOutput::lang_items`]. Populated in the
    /// `Item::Trait(td)` arm of `register_top_level` (registration.rs) by
    /// walking `TraitDecl.lang_item` and each `TraitItem::Method.lang_item`
    /// attribute. Duplicate keys raise a `duplicate_definition` error so
    /// the registry remains one-binding-per-key. Moved into the output at
    /// `check_program` exit.
    pub(super) lang_items: crate::LangItemRegistry,
    /// Spans of previously registered lang-item keys, for duplicate-key
    /// diagnostics. Keyed by lang-item string.
    pub(super) lang_item_spans: HashMap<String, Span>,
    /// When a `let name = |...| ...` is being synthesised, holds `name` so that
    /// `synthesize_identifier` can detect a recursive self-reference inside the
    /// closure body and emit `ClosureRecursive` instead of `UndefinedVariable`.
    ///
    /// WHY: by-value closure capture cannot capture a value before construction,
    ///      so recursive self-reference via the let-binding is unsupported in v0.5.
    /// WHEN OBSOLETE: if a `let rec` or fixed-point surface is ratified.
    /// REAL SOLUTION: a proper `letrec`/`fix`-point binder in the type checker.
    pub(super) pending_let_closure_name: Option<String>,
    /// Per-spawn-site type arguments for generic actor instantiations.
    ///
    /// Mirrors [`TypeCheckOutput::actor_spawn_type_args`]. Populated in
    /// `check_spawn` when explicit type args are resolved for a generic actor.
    /// Moved into the output at `check_program` exit after `subst.resolve`
    /// settles inference variables.
    pub(super) actor_spawn_type_args: HashMap<SpanKey, (String, Vec<Ty>)>,
    /// Canonical builtin `Result`/`Option` receiver method signatures, snapshotted
    /// from the compiled-in `std/result.hew` / `std/option.hew` impl blocks at
    /// builtin-registration time, keyed by `(builtin discriminant, method name)`.
    ///
    /// The builtin `Result<T, E>` / `Option<T>` nominals register their methods
    /// (`is_ok`, `unwrap`, …) into `fn_sigs` under the bare keys
    /// `Result::<method>` / `Option::<method>`. A user package may declare its own
    /// `pub type Result` with colliding methods, which land under the SAME bare
    /// keys and clobber the stdlib entries by registration order — so `fn_sigs`
    /// can no longer name the canonical builtin surface for a builtin receiver.
    ///
    /// This table is populated ONLY from the stdlib source and is never written
    /// by user-impl registration, so it is the origin-based source of truth for
    /// dispatch on a builtin `Result`/`Option` receiver. Each stored `FnSig`
    /// retains its impl-level `type_params` and `extern_symbol`, so call sites
    /// instantiate it against the receiver's type arguments exactly as
    /// `lookup_named_method_sig` would have.
    pub(super) builtin_result_option_method_sigs: HashMap<(crate::BuiltinType, String), FnSig>,
    /// Resolved reporting level for every semantic lint (see [`super::run_lints`]).
    ///
    /// Defaults to [`super::LintLevels::from_defaults`]; the CLI layer threads
    /// `--warn` / `--allow` / `--deny` through [`Checker::set_lint_levels`]
    /// before [`Checker::check_program`] runs the lint sweep.
    pub(super) lint_levels: super::LintLevels,
    /// Per-module source text for in-source lint suppression (see
    /// [`super::LintSources`]).
    ///
    /// Empty by default; the CLI installs the program's source(s) through
    /// [`Checker::set_lint_sources`] before [`Checker::check_program`] so the
    /// lint sweep can resolve `// hew:allow(...)` directives.
    pub(super) lint_sources: super::LintSources,
    /// Import type aliases: maps `(importer_module, alias)` → canonical
    /// qualified source identity, for every `import m::{ T as U }` where the
    /// binding name (`U`) differs from the original name (`T`).
    ///
    /// Keyed per-module so aliases from different modules cannot collide
    /// (flat-string keying caused last-write-wins cross-module pollution).
    /// Moved into [`TypeCheckOutput::import_type_name_aliases`] at
    /// `check_program` exit.
    pub(super) import_type_name_aliases: HashMap<(Option<String>, String), String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct IntegerTypeInfo {
    pub(super) width: u8,
    pub(super) signed: bool,
}

/// Known compile-time numeric literal value (for later coercion checks).
#[derive(Debug, Clone)]
pub(super) enum ConstValue {
    Integer(i64),
    Float(f64),
}

/// Resolved declaration of a machine const-generic parameter.
///
/// Mirrors `hew_parser::ast::ConstParam` at the checker layer so the
/// side table (`machine_const_params` on [`Checker`]) does not pin a
/// downstream subsystem to the parser AST shape. The default value
/// is stored as `u64` because R269=A admits `usize` only — when
/// additional widths are added (deferred work), this evolves into a
/// tagged value type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MachineConstParamDecl {
    pub name: String,
    /// Resolved const-param width. Currently `usize` is the only
    /// admitted variant; the kind is retained so widening can extend
    /// non-breakingly.
    pub ty: MachineConstParamTy,
    /// Optional default value (R270=A). `None` when the parameter
    /// has no `= value` clause at declaration.
    pub default: Option<u64>,
}

/// Resolved const-param element type. R269=A: usize only initially.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MachineConstParamTy {
    Usize,
}

/// Constexpr-evaluated machine const-argument value at an
/// instantiation site, produced by
/// [`Checker::validate_const_param_arg`].
///
/// Parallel to `hew_hir::mono::ConstValue` (defined in
/// `hew-hir/src/mono/machine.rs`) so the checker can resolve const-
/// args without taking a dependency on `hew-hir`. Stage 3 lowering
/// converts this into the HIR variant when populating
/// `MachineMonoKey::const_args`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MachineConstArgValue {
    /// `usize` const-argument (R269=A).
    Usize(u64),
}

impl Checker {
    #[must_use]
    #[allow(
        clippy::too_many_lines,
        reason = "initialises every checker field; splitting would scatter defaults"
    )]
    pub fn new(module_registry: ModuleRegistry) -> Self {
        Self {
            env: TypeEnv::new(),
            subst: Substitution::new(),
            registry: TraitRegistry::new(),
            module_registry,
            errors: Vec::new(),
            warnings: Vec::new(),
            user_clone_record_seeds: Vec::new(),
            expr_types: HashMap::new(),
            is_type_patterns: HashMap::new(),
            expr_type_source_modules: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            method_call_consumes_receiver: HashSet::new(),
            actor_send_aliasing: HashMap::new(),
            actor_handler_state_guards: HashMap::new(),
            actor_max_heap: HashMap::new(),
            consume_receiver_methods: HashSet::new(),
            pending_lowering_facts: HashMap::new(),
            deferred_hashmap_admission: HashMap::new(),
            deferred_hashset_admission: HashMap::new(),
            hashmap_layout_facts: HashMap::new(),
            hashset_layout_facts: HashMap::new(),
            deferred_vec_admission: HashMap::new(),
            deferred_channel_rewrites: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            wire_layouts: HashMap::new(),
            resolved_calls: HashMap::new(),
            numeric_method_lowerings: HashMap::new(),
            width_cast_lowerings: HashMap::new(),
            actor_method_dispatch: HashMap::new(),
            machine_method_dispatch: HashMap::new(),
            conn_await_reads: HashMap::new(),
            listener_await_accepts: HashSet::new(),
            tail_ok_coercions: HashSet::new(),
            tail_ok_armed: false,
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            stack_hints: Vec::new(),
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            fn_type_param_assoc_bindings: HashMap::new(),
            handle_bearing_structs: HashSet::new(),
            user_opaque_type_names: HashSet::new(),
            wire_struct_types: HashSet::new(),
            wire_enum_types: HashSet::new(),
            handle_bearing_dirty: false,
            refresh_call_count: 0,
            receive_generator_methods: HashSet::new(),
            actor_receive_methods: HashSet::new(),
            type_def_inference_holes: HashMap::new(),
            fn_sig_inference_holes: HashMap::new(),
            deferred_inference_holes: Vec::new(),
            deferred_cast_checks: Vec::new(),
            deferred_monomorphic_sites: Vec::new(),
            fn_def_spans: HashMap::new(),
            fn_visibility: HashMap::new(),
            type_def_spans: HashMap::new(),
            type_visibility: HashMap::new(),
            type_namespace_owners: HashMap::new(),
            flat_file_import_pub_spans: HashMap::new(),
            registered_flat_file_import_sources: HashSet::new(),
            registered_stdlib_hew_sources: HashSet::new(),
            generic_ctx: Vec::new(),
            current_type_param_bounds: Vec::new(),
            machine_type_param_bounds: HashMap::new(),
            machine_const_params: HashMap::new(),
            reported_machine_bound_violations: HashSet::new(),
            reported_type_def_bound_violations: HashSet::new(),
            actor_type_param_bounds: HashMap::new(),
            reported_actor_bound_violations: HashSet::new(),
            actors_with_periodic_handlers: HashMap::new(),
            current_return_type: None,
            in_generator: false,
            inside_await_expr: false,
            loop_depth: 0,
            loop_labels: Vec::new(),
            modules: HashSet::new(),
            known_types: HashSet::new(),
            type_aliases: HashMap::new(),
            trait_defs: HashMap::new(),
            trait_super: HashMap::new(),
            trait_import_bindings: HashMap::new(),
            trait_impls_set: HashSet::new(),
            conflicting_trait_impl_reported: HashSet::new(),
            trait_impl_method_names: HashMap::new(),
            primitive_trait_impls: HashMap::new(),
            primitive_trait_impl_self_args: HashMap::new(),
            supervisor_children: HashMap::new(),
            supervisor_child_slots: HashMap::new(),
            pool_accessor_sites: HashMap::new(),
            dyn_trait_coercions: HashMap::new(),
            dyn_trait_method_calls: HashMap::new(),
            closure_capture_facts: HashMap::new(),
            closure_escape_facts: HashMap::new(),
            closure_escape_advisory_spans: HashSet::new(),
            actor_init_params: HashMap::new(),
            lambda_capture_depth: None,
            lambda_captures: Vec::new(),
            lambda_capture_facts: Vec::new(),
            import_spans: HashMap::new(),
            used_modules: RefCell::new(HashSet::new()),
            user_modules: HashSet::new(),
            module_fn_exports: HashSet::new(),
            module_type_exports: HashMap::new(),
            reported_type_visibility_violations: HashSet::new(),
            reported_undefined_named_types: HashSet::new(),
            type_decls_registered: false,
            suppress_undefined_type_report: false,
            declared_type_param_names: HashSet::new(),
            scope_local_type_params_only: false,
            declared_nominal_type_names: HashSet::new(),
            unqualified_to_module: HashMap::new(),
            published_bare_type_owners: HashMap::new(),
            published_bare_trait_owners: HashMap::new(),
            call_graph: HashMap::new(),
            current_function: None,
            in_for_binding: false,
            pattern_bound_names: HashMap::new(),
            bind_pattern_recording: false,
            in_receive_fn: false,
            in_actor_handler_context: false,
            in_lambda_actor_body: false,
            in_unsafe: false,
            task_scope_depth: 0,
            current_module: None,
            current_module_idx: 0,
            local_type_defs: HashSet::new(),
            source_type_defs: HashSet::new(),
            local_trait_defs: HashSet::new(),
            current_self_type: None,
            current_actor_type: None,
            current_actor_fields: Vec::new(),
            actor_protocol_descriptors: HashMap::new(),
            impl_alias_scopes: Vec::new(),
            current_trait_for_self_projection: None,
            impl_assoc_type_bindings: HashMap::new(),
            unsafe_functions: HashSet::new(),
            wasm_target: false,
            repl_fragment: false,
            is_stdlib_source: false,
            in_stdlib_registration: false,
            wasm_warning_spans: HashSet::new(),
            wasm_reject_spans: HashSet::new(),
            current_machine_transition: None,
            current_machine_lifecycle: None,
            const_values: HashMap::new(),
            call_type_args: HashMap::new(),
            record_init_type_args: HashMap::new(),
            builtin_result_output_type_args: HashMap::new(),
            deferred_bound_checks: Vec::new(),
            lambda_poly_sig_map: HashMap::new(),
            last_lambda_generic_sig: None,
            deferred_range_bounds: Vec::new(),
            intrinsic_declarations: HashMap::new(),
            pending_let_closure_name: None,
            pending_pattern_resolutions: HashMap::new(),
            lang_items: crate::LangItemRegistry::new(),
            lang_item_spans: HashMap::new(),
            actor_spawn_type_args: HashMap::new(),
            builtin_result_option_method_sigs: HashMap::new(),
            lint_levels: super::LintLevels::from_defaults(),
            lint_sources: super::LintSources::new(),
            import_type_name_aliases: HashMap::new(),
        }
    }

    /// Enable WASM32-specific validation and warnings.
    pub fn enable_wasm_target(&mut self) {
        self.wasm_target = true;
    }

    /// Override the per-lint reporting levels for the semantic lint sweep.
    ///
    /// The integration seam for the CLI: a front end parses `--warn` /
    /// `--allow` / `--deny` (and any in-source overrides) into a
    /// [`super::LintLevels`] and installs it here before
    /// [`Checker::check_program`]. When left unset the checker uses
    /// [`super::LintLevels::from_defaults`].
    pub fn set_lint_levels(&mut self, levels: super::LintLevels) {
        self.lint_levels = levels;
    }

    /// Install the program's source text for in-source lint suppression.
    ///
    /// The checker only carries byte-offset spans, so the front end hands it
    /// the source(s) here — keyed by module — before [`Checker::check_program`].
    /// The lint sweep then resolves `// hew:allow(...)` directives against the
    /// source owning each finding's span. When left unset, no in-source
    /// suppression is applied (CLI `--allow` still works).
    pub fn set_lint_sources(&mut self, sources: super::LintSources) {
        self.lint_sources = sources;
    }

    /// Target pointer width in bits: 32 on `wasm32`, 64 on every native target.
    ///
    /// Derived from the target the compile was invoked with (`wasm_target` is
    /// set from `FrontendOptions::enable_wasm_target` = `--target wasm32`), NOT
    /// from a host `cfg!(target_pointer_width)` — so `isize`/`usize`
    /// classification and literal-fit ranges stay correct when cross-compiling.
    /// `TargetArch` has only one 32-bit variant (`Wasm32`); the remaining native
    /// variants are all 64-bit, so the wasm flag is a faithful pointer-width
    /// proxy for the checker. Used by the integer-classification helpers
    /// (`integer_type_info` / `integer_type_range` / `integer_fits_type` /
    /// `common_integer_type`).
    #[must_use]
    pub(super) fn pointer_width(&self) -> u8 {
        if self.wasm_target {
            32
        } else {
            64
        }
    }

    /// Mark the program under check as a synthetic `hew eval` REPL fragment,
    /// suppressing the whole-program completeness lints. See
    /// [`Checker::repl_fragment`].
    pub fn set_repl_fragment(&mut self) {
        self.repl_fragment = true;
    }

    /// Mark the checker as currently processing a stdlib or built-in library
    /// source body, suppressing scope-level lints (`UnusedVariable`,
    /// `UnusedMut`) for those bodies.
    ///
    /// Set transiently by `check_program` around each non-root module whose
    /// path starts with `"std"`, `"hew"`, or `"ecosystem"`.  Cleared after
    /// that module's items have been checked.  Distinct from
    /// [`set_repl_fragment`](Self::set_repl_fragment) which suppresses ALL
    /// completeness lints across the whole program; this flag is scoped
    /// per-module so the user's own code in the same compilation unit still
    /// receives its warnings.
    pub fn set_stdlib_source(&mut self) {
        self.is_stdlib_source = true;
    }

    /// Register a qualified `Trait::method` name as one whose dispatch
    /// consumes the receiver. The move-checker marks the receiver moved
    /// after the call, and the per-call-site flag in
    /// [`TypeCheckOutput::method_call_consumes_receiver`] is set so codegen
    /// can null the drop slot.
    ///
    /// PR 1 (issue #1295) ships this seam unused in production: no Hew
    /// surface syntax populates the set, and stdlib trait registration does
    /// not call this until PR 2 introduces `Closable::close`. Tests use it
    /// to exercise the consume-marker pipeline before PR 2 lands.
    pub fn register_consume_receiver_method(&mut self, qualified_name: impl Into<String>) {
        self.consume_receiver_methods.insert(qualified_name.into());
    }

    /// Extract the module registry after type checking is done.
    ///
    /// This transfers ownership of the registry (with its cached module data)
    /// so it can be passed to the enricher.
    #[must_use]
    pub fn into_module_registry(self) -> ModuleRegistry {
        self.module_registry
    }

    /// Borrow the module registry (for read-only access after checking).
    #[must_use]
    pub fn module_registry(&self) -> &ModuleRegistry {
        &self.module_registry
    }
}

impl Default for Checker {
    fn default() -> Self {
        Self::new(ModuleRegistry::new(vec![]))
    }
}
