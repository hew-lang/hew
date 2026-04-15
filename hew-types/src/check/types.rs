use crate::env::TypeEnv;
use crate::error::TypeError;
use crate::lowering_facts::LoweringFact;
use crate::module_registry::ModuleRegistry;
use crate::traits::TraitRegistry;
use crate::ty::{Substitution, Ty, TypeVar};
use hew_parser::ast::{Span, Spanned, TraitMethod, TypeExpr};
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

/// Result of type-checking a program.
#[derive(Debug, Clone)]
pub struct TypeCheckOutput {
    pub expr_types: HashMap<SpanKey, Ty>,
    pub method_call_receiver_kinds: HashMap<SpanKey, MethodCallReceiverKind>,
    /// Checker-owned lowering metadata keyed by the lowering site's source span.
    ///
    /// Populated for erased runtime types whose lowering must not guess from
    /// MLIR argument types. Missing entry means the checker could not produce a
    /// concrete lowering fact and downstream codegen must fail closed.
    pub lowering_facts: HashMap<SpanKey, LoweringFact>,
    /// Checker-owned method-call lowering decisions keyed by the method call span.
    ///
    /// Populated during type checking for both receiver-based runtime rewrites
    /// and module-qualified stdlib direct-call rewrites so serialization can
    /// consume a single authoritative contract instead of re-resolving C
    /// symbols from receiver types or the module registry.
    pub method_call_rewrites: HashMap<SpanKey, MethodCallRewrite>,
    /// Checker-resolved assignment target classification keyed by the target
    /// expression span. Missing entry means the checker rejected the target.
    pub assign_target_kinds: HashMap<SpanKey, AssignTargetKind>,
    /// Checker-resolved assignment target type-shape metadata keyed by the
    /// target expression span.  Populated alongside `assign_target_kinds` for
    /// every accepted assignment.  MLIR lowering consumes this fail-closed to
    /// determine signedness of compound-assignment arithmetic instead of
    /// re-deriving it from the AST.
    pub assign_target_shapes: HashMap<SpanKey, AssignTargetShape>,
    pub errors: Vec<TypeError>,
    pub warnings: Vec<TypeError>,
    pub type_defs: HashMap<String, TypeDef>,
    pub fn_sigs: HashMap<String, FnSig>,
    /// Actor type names that participate in reference cycles.
    pub cycle_capable_actors: HashSet<String>,
    /// Module short names for user (non-stdlib) imports that have resolved items.
    pub user_modules: HashSet<String>,
    /// Inferred type arguments for generic function calls that lack explicit
    /// type annotations.  Keyed by the call expression's span.
    pub call_type_args: HashMap<SpanKey, Vec<Ty>>,
}

/// Checker-owned classification of an assignment target.
///
/// Populated once during `check_stmt` for `Stmt::Assign` and threaded through
/// the serialisation boundary so MLIR lowering can consume it fail-closed
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

/// Checker-owned type-shape annotation for an assignment target.
///
/// Populated alongside [`AssignTargetKind`] for every `Stmt::Assign` accepted
/// by the checker.  Missing entry means the checker rejected the target; MLIR
/// lowering must fail closed on both maps.
///
/// This captures the information MLIR needs for compound-assignment arithmetic
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

/// Span used as map key (byte offsets).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpanKey {
    pub start: usize,
    pub end: usize,
}

impl From<&Span> for SpanKey {
    fn from(s: &Span) -> Self {
        Self {
            start: s.start,
            end: s.end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodCallReceiverKind {
    NamedTypeInstance { type_name: String },
    HandleInstance { type_name: String },
    TraitObject { trait_name: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodCallRewrite {
    /// Rewrite a receiver-based method call to a runtime function and inject
    /// the receiver as the first argument.
    RewriteToFunction {
        c_symbol: String,
    },
    /// Rewrite a module-qualified stdlib call directly to a runtime function
    /// without injecting the receiver/module identifier as an argument.
    RewriteModuleQualifiedToFunction {
        c_symbol: String,
    },
    DeferToLowering,
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
/// `StructuredConcurrency`, `Tasks`, `BlockingChannelRecv`, `Streams`) are emitted as
/// compile-time **errors**. Their runtime support is absent on wasm32: some
/// entry points trap via `unreachable!`, while native-only modules such as
/// scope/task/supervisor/link-monitor are gated out of `hew-runtime` entirely.
/// Making them errors ensures WASM programs fail loudly at check time rather
/// than at link time or the first use at runtime.
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
    // ── Warning group (implemented with degraded semantics) ─────────────────
    /// `sleep_ms`, `sleep`: sleep is cooperative on wasm32 — it takes effect
    /// at the *message boundary*, not mid-handler.  Code after `sleep_ms` in
    /// the same receive handler still executes before the park happens.
    /// Use `hew_wasm_timer_tick` (host) or `hew_wasm_sched_tick` (scheduler)
    /// to advance the timer queue.
    Timers,
    // ── Reject group (runtime unreachable!-trap; compile-time error) ────────
    /// `Receiver<T>::recv`: blocking receive still traps on wasm32 because the
    /// cooperative scheduler does not yet yield and resume on an empty channel
    /// with live senders.
    BlockingChannelRecv,
    /// `stream.*` module constructors and `Stream<T>::*` methods: the stream
    /// runtime module is not compiled for wasm32
    /// (`#[cfg(not(target_arch = "wasm32"))]` in hew-runtime/src/lib.rs).
    /// WASM-TODO: implement I/O-stream adapters over WASI fd/socket APIs.
    Streams,
}

impl WasmUnsupportedFeature {
    pub(super) fn label(self) -> &'static str {
        match self {
            Self::SupervisionTrees => "Supervision tree operations",
            Self::LinkMonitor => "Link/monitor operations",
            Self::StructuredConcurrency => "Structured concurrency scopes",
            Self::Tasks => "Task handles spawned from scopes",
            Self::BlockingChannelRecv => "Blocking channel receive operations",
            Self::Timers => "Timer/sleep operations",
            Self::Streams => "Stream operations",
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
            Self::BlockingChannelRecv => {
                "Receiver<T>::recv still requires cooperative scheduler yield/resume on wasm32; \
                 use try_recv or the actor ask pattern instead"
            }
            Self::Timers => {
                "sleep is cooperative on wasm32: it parks the actor at the message boundary \
                 rather than mid-handler; code after sleep_ms in the same handler still \
                 executes before the park takes effect"
            }
            Self::Streams => {
                "I/O streams require the OS threading and networking stack; \
                 the stream runtime module is not compiled for wasm32"
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub kind: TypeDefKind,
    pub name: String,
    pub type_params: Vec<String>,
    pub fields: HashMap<String, Ty>,
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
    pub(super) default: Option<Spanned<TypeExpr>>,
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

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub type_params: Vec<String>,
    pub type_param_bounds: HashMap<String, Vec<String>>,
    pub param_names: Vec<String>,
    pub params: Vec<Ty>,
    pub return_type: Ty,
    pub is_async: bool,
    pub is_pure: bool,
    pub accepts_kwargs: bool,
    pub doc_comment: Option<String>,
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
            is_pure: false,
            accepts_kwargs: false,
            doc_comment: None,
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
    pub(super) expr_types: HashMap<SpanKey, Ty>,
    pub(super) expr_type_source_modules: HashMap<SpanKey, Option<String>>,
    pub(super) method_call_receiver_kinds: HashMap<SpanKey, MethodCallReceiverKind>,
    pub(super) pending_lowering_facts: HashMap<SpanKey, PendingLoweringFact>,
    /// `HashMap` key/value admission checks deferred until after inference
    /// completes.  Keyed by span to suppress duplicates from repeated
    /// traversals of the same site (annotation + method call on the same map).
    pub(super) deferred_hashmap_admission: HashMap<SpanKey, DeferredHashMapAdmission>,
    /// `HashSet` element admission checks deferred until after inference
    /// completes.  Keyed by span to suppress duplicates from repeated
    /// traversals of the same site (annotation + method call on the same set).
    pub(super) deferred_hashset_admission: HashMap<SpanKey, DeferredHashSetAdmission>,
    /// Channel method call rewrites deferred until after inference completes.
    /// Keyed by call-site span so repeated traversal of the same site is
    /// idempotent (last write wins, which is fine since the inner type is the
    /// same variable every time).
    pub(super) deferred_channel_rewrites: HashMap<SpanKey, DeferredChannelMethodRewrite>,
    pub(super) method_call_rewrites: HashMap<SpanKey, MethodCallRewrite>,
    pub(super) assign_target_kinds: HashMap<SpanKey, AssignTargetKind>,
    pub(super) assign_target_shapes: HashMap<SpanKey, AssignTargetShape>,
    pub(super) type_defs: HashMap<String, TypeDef>,
    pub(super) fn_sigs: HashMap<String, FnSig>,
    /// Qualified `Actor::method` names declared with `receive gen fn`.
    pub(super) receive_generator_methods: HashSet<String>,
    pub(super) type_def_inference_holes: HashMap<String, Vec<TypeVar>>,
    pub(super) fn_sig_inference_holes: HashMap<String, Vec<TypeVar>>,
    pub(super) deferred_inference_holes: Vec<DeferredInferenceHole>,
    pub(super) deferred_cast_checks: Vec<DeferredCastCheck>,
    pub(super) deferred_monomorphic_sites: Vec<DeferredMonomorphicSite>,
    /// Tracks the span and originating module where each function was first defined
    /// (for duplicate detection and dead-code source attribution).
    pub(super) fn_def_spans: HashMap<String, (Span, Option<String>)>,
    /// Tracks the span where each top-level type/trait namespace name was first defined.
    pub(super) type_def_spans: HashMap<String, Span>,
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
    pub(super) current_return_type: Option<Ty>,
    pub(super) in_generator: bool,
    pub(super) loop_depth: u32,
    /// Labels of enclosing loops, for validating `break @label` / `continue @label`.
    pub(super) loop_labels: Vec<String>,
    pub(super) modules: HashSet<String>,
    pub(super) known_types: HashSet<String>,
    pub(super) type_aliases: HashMap<String, Ty>,
    pub(super) trait_defs: HashMap<String, TraitInfo>,
    /// Maps trait name → list of super-trait names (e.g., `Pet` → [`Animal`])
    pub(super) trait_super: HashMap<String, Vec<String>>,
    /// Set of (`type_name`, `trait_name`) pairs for concrete impl registrations
    pub(super) trait_impls_set: HashSet<(String, String)>,
    /// Maps supervisor name to `(child_name, actor_type)` pairs for `supervisor_child`
    pub(super) supervisor_children: HashMap<String, Vec<(String, String)>>,
    /// When set, records the scope depth at which a lambda was entered.
    /// Variable lookups from scopes below this depth are captures.
    pub(super) lambda_capture_depth: Option<usize>,
    /// Captured variable types accumulated during lambda body checking.
    pub(super) lambda_captures: Vec<Ty>,
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
    /// Maps (`owner_module`, `unqualified_name`) to the module short name the name
    /// was imported from.  Used to mark the owning import as used when an
    /// unqualified function/type is referenced.
    pub(super) unqualified_to_module: HashMap<(Option<String>, String), String>,
    /// Call graph: maps caller function name → set of callee function names.
    pub(super) call_graph: HashMap<String, HashSet<String>>,
    /// Name of the function currently being checked (for call graph tracking).
    pub(super) current_function: Option<String>,
    /// Whether we are currently inside a for-loop binding (suppress shadowing for loop vars).
    pub(super) in_for_binding: bool,
    /// Whether we are currently inside a `pure` function body.
    pub(super) in_pure_function: bool,
    /// Whether we are currently inside an actor receive function body.
    /// Used to warn about blocking calls that can starve the scheduler.
    pub(super) in_receive_fn: bool,
    /// Whether we are currently inside an unsafe block.
    pub(super) in_unsafe: bool,
    /// The module currently being processed (enables per-module scoping in future).
    pub(super) current_module: Option<String>,
    /// Tracks which types are defined locally (in the current compilation unit).
    pub(super) local_type_defs: HashSet<String>,
    /// Tracks which traits are defined locally (in the current compilation unit).
    pub(super) local_trait_defs: HashSet<String>,
    /// The type name and args of the current impl block target (for resolving `Self`).
    pub(super) current_self_type: Option<(String, Vec<Ty>)>,
    /// The actor type currently being checked (for `this` keyword resolution).
    pub(super) current_actor_type: Option<Ty>,
    /// Field names of the current actor (for purity checks on bare field assignment).
    pub(super) current_actor_fields: Vec<String>,
    pub(super) impl_alias_scopes: Vec<ImplAliasScope>,
    /// Names of functions that require an unsafe block to call.
    pub(super) unsafe_functions: HashSet<String>,
    /// Whether warnings for WASM-only builds should be emitted.
    pub(super) wasm_target: bool,
    /// Tracks (span, feature) pairs we've already warned about for WASM limits.
    pub(super) wasm_warning_spans: HashSet<(SpanKey, WasmUnsupportedFeature)>,
    /// Tracks (span, feature) pairs we've already rejected as errors for WASM.
    /// Separate from `wasm_warning_spans` to allow independent deduplication.
    pub(super) wasm_reject_spans: HashSet<(SpanKey, WasmUnsupportedFeature)>,
    /// Tracks slice annotation spans we've already rejected so repeated
    /// resolution passes don't emit duplicate diagnostics.
    pub(super) unsupported_slice_spans: HashSet<SpanKey>,
    /// Inside a machine transition body, the (`machine_name`, `source_state_name`, `event_name`) tuple.
    pub(super) current_machine_transition: Option<(String, String, String)>,
    /// Compile-time known numeric literal values used by later coercion sites.
    pub(super) const_values: HashMap<String, ConstValue>,
    /// Inferred type arguments for generic function calls that omit explicit
    /// type annotations.  Populated in `check_call` after argument unification.
    pub(super) call_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Builtin `Ok`/`Err` constructor calls whose output type may need
    /// checked-output fallback when one side remains unconstrained.
    pub(super) builtin_result_output_type_args: HashMap<SpanKey, (Ty, Ty)>,
    /// Maps a let-bound name to the (`TypeParam` name, `TypeVar`) pairs created
    /// when that name was bound to a generic lambda expression.  Used to
    /// populate `call_type_args` when the lambda is called later.
    pub(super) lambda_poly_type_var_map: HashMap<String, Vec<(String, TypeVar)>>,
    /// Scratch field: set by `check_lambda` when it processes a generic lambda
    /// (one with non-empty `type_params`).  Consumed immediately in the
    /// enclosing `Stmt::Let` handler to populate `lambda_poly_type_var_map`.
    pub(super) last_lambda_generic_vars: Option<Vec<(String, TypeVar)>>,
    /// Range bounds whose element type is deferred until surrounding inference
    /// settles. Each entry is (span, element-TypeVar, literal-value-if-any).
    /// Processed in `apply_deferred_range_bound_types` after all inference and
    /// literal defaulting is complete.
    pub(super) deferred_range_bounds: Vec<(Span, TypeVar, Option<i64>)>,
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

impl Checker {
    #[must_use]
    pub fn new(module_registry: ModuleRegistry) -> Self {
        Self {
            env: TypeEnv::new(),
            subst: Substitution::new(),
            registry: TraitRegistry::new(),
            module_registry,
            errors: Vec::new(),
            warnings: Vec::new(),
            expr_types: HashMap::new(),
            expr_type_source_modules: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            pending_lowering_facts: HashMap::new(),
            deferred_hashmap_admission: HashMap::new(),
            deferred_hashset_admission: HashMap::new(),
            deferred_channel_rewrites: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            receive_generator_methods: HashSet::new(),
            type_def_inference_holes: HashMap::new(),
            fn_sig_inference_holes: HashMap::new(),
            deferred_inference_holes: Vec::new(),
            deferred_cast_checks: Vec::new(),
            deferred_monomorphic_sites: Vec::new(),
            fn_def_spans: HashMap::new(),
            type_def_spans: HashMap::new(),
            flat_file_import_pub_spans: HashMap::new(),
            registered_flat_file_import_sources: HashSet::new(),
            registered_stdlib_hew_sources: HashSet::new(),
            generic_ctx: Vec::new(),
            current_return_type: None,
            in_generator: false,
            loop_depth: 0,
            loop_labels: Vec::new(),
            modules: HashSet::new(),
            known_types: HashSet::new(),
            type_aliases: HashMap::new(),
            trait_defs: HashMap::new(),
            trait_super: HashMap::new(),
            trait_impls_set: HashSet::new(),
            supervisor_children: HashMap::new(),
            lambda_capture_depth: None,
            lambda_captures: Vec::new(),
            import_spans: HashMap::new(),
            used_modules: RefCell::new(HashSet::new()),
            user_modules: HashSet::new(),
            module_fn_exports: HashSet::new(),
            unqualified_to_module: HashMap::new(),
            call_graph: HashMap::new(),
            current_function: None,
            in_for_binding: false,
            in_pure_function: false,
            in_receive_fn: false,
            in_unsafe: false,
            current_module: None,
            local_type_defs: HashSet::new(),
            local_trait_defs: HashSet::new(),
            current_self_type: None,
            current_actor_type: None,
            current_actor_fields: Vec::new(),
            impl_alias_scopes: Vec::new(),
            unsafe_functions: HashSet::new(),
            wasm_target: false,
            wasm_warning_spans: HashSet::new(),
            wasm_reject_spans: HashSet::new(),
            unsupported_slice_spans: HashSet::new(),
            current_machine_transition: None,
            const_values: HashMap::new(),
            call_type_args: HashMap::new(),
            builtin_result_output_type_args: HashMap::new(),
            lambda_poly_type_var_map: HashMap::new(),
            last_lambda_generic_vars: None,
            deferred_range_bounds: Vec::new(),
        }
    }

    /// Enable WASM32-specific validation and warnings.
    pub fn enable_wasm_target(&mut self) {
        self.wasm_target = true;
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
