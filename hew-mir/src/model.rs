use std::collections::{HashMap, HashSet};

use hew_hir::{BindingId, IntentKind, ItemId, SiteId, ValueClass};
use hew_types::{NumericWidth, ResolvedTy};

pub use crate::runtime_symbols::UnknownRuntimeSymbol;

/// Distinguishes shared (read-only, may alias) from mutable (unique,
/// no-alias) borrows for the aliasing check. The check itself is
/// declared in `MirCheck::Aliasing` but the spine has no construction
/// surface for borrows yet — the variant exists so the borrow
/// lowering that lands later doesn't have to retrofit the kind enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BorrowKind {
    Shared,
    Mutable,
}

/// Binary alias-vs-copy discriminant carried by [`Terminator::Send`].
///
/// Codegen uses this in Phase P5.2 to branch between the legacy
/// deep-copy mailbox path (`Copy`) and the refcounted alias envelope
/// path (`Alias`). The field is populated by MIR lowering from the
/// checker's `actor_send_aliasing` side table.
///
/// **Fail-closed default**: a missing or unresolved classification in
/// the checker's side table MUST produce `Copy`. `Copy` is the safe
/// fallback — it issues a deep-copy into the mailbox, which is always
/// semantically correct even if sub-optimal. `Alias` is only safe when
/// the move-checker has already invalidated the sender's binding.
///
/// LESSONS: `serializer-fail-closed` (P0) — every fallback / default
/// path MUST be `Copy`. The `Default` impl below enforces this.
///
/// `alias-byte-copy-not-semantic-clone` / `copy ⊥ sendable` (P0):
/// this discriminant is derived SOLELY from the checker's
/// `actor_send_aliasing` classification, never from a `Copy`-marker
/// or `implements_marker(Copy)` check.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SendAliasMode {
    /// Sender and receiver are isolated: the runtime deep-copies the
    /// payload into the mailbox. Always safe; the fail-closed default.
    Copy,
    /// Sender transfers ownership of a refcounted envelope to the
    /// receiver. The move-checker has already invalidated the sender's
    /// binding so no post-send observation is possible.
    Alias,
}

impl Default for SendAliasMode {
    /// Fail-closed: the default mode is `Copy` so any send site that
    /// cannot be resolved by the checker falls back to the safe
    /// deep-copy path rather than producing an unsound alias.
    fn default() -> Self {
        SendAliasMode::Copy
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrPipeline {
    pub thir: Vec<ThirFunction>,
    pub raw_mir: Vec<RawMirFunction>,
    pub checked_mir: Vec<CheckedMirFunction>,
    pub elaborated_mir: Vec<ElaboratedMirFunction>,
    pub diagnostics: Vec<MirDiagnostic>,
    /// Names of every `#[opaque]` runtime handle declared in the module
    /// (W3.020). Codegen resolves these `Named` types to a bare LLVM `ptr`
    /// (pointer-width opaque handle, ABI-compatible with the runtime's
    /// `*mut T`) instead of a record struct layout.
    pub opaque_handle_names: Vec<String>,
    /// Layout descriptors for every named-form `record` declaration in the
    /// module. Populated by `lower_hir_module` from the same `HirItem::Record`
    /// walk that builds the field-order table. Codegen (`hew-codegen-rs`)
    /// consumes this to register LLVM named struct types and resolve the
    /// `ResolvedTy::Named { name, .. }` of record-typed locals to the
    /// corresponding struct layout for alloca / GEP emission.
    ///
    /// Tuple-form records (`record Pair(i64, i64)`) are NOT included here:
    /// their `HirRecordDecl.fields` is empty (the parser keeps positional
    /// fields on the `RecordKind::Tuple` discriminator, which the HIR lowerer
    /// does not promote into `HirField`s). Tuple records construct via
    /// `Expr::Call`, not `StructInit`, so they never produce `RecordInit`
    /// or `RecordFieldLoad` instructions and need no codegen layout entry
    /// in this slice.
    pub record_layouts: Vec<RecordLayout>,
    /// Layout descriptors for every actor declaration in the module. Populated
    /// by `lower_hir_module` from `HirItem::Actor` declarations in source
    /// order. Codegen/runtime dispatch slices consume this to materialise actor
    /// state storage and init-call signatures without re-reading HIR.
    pub actor_layouts: Vec<ActorLayout>,
    /// Layout descriptors for every supervisor declaration in the module.
    /// Populated by `lower_hir_module` from `HirItem::Supervisor` declarations
    /// in source order. Each entry pairs the user-declared supervisor metadata
    /// (strategy, restart budget, window, children) with the synthesized
    /// bootstrap-function symbol whose body spawns and wires the children.
    /// Codegen (S-D) consumes this to emit the per-supervisor registration
    /// table that the runtime supervisor substrate dispatches against.
    pub supervisor_layouts: Vec<SupervisorLayout>,
    /// Layout descriptors for every `machine` declaration in the module.
    /// Populated by `lower_hir_module` from `HirItem::Machine` declarations
    /// in source order. One entry per machine, in declaration order.
    ///
    /// Codegen (Slice 5) consumes this to emit the tagged-union LLVM type
    /// for each machine: the outer struct carries an integer tag of width
    /// `tag_width` bits followed by a union payload that covers all variant
    /// structs. The `variants` vector (populated in Slice 5 from the machine's
    /// `HirMachineState.fields`) provides the per-variant field type list.
    ///
    /// `Place::MachineTag(local)` and `Place::MachineVariant { local, .. }`
    /// address into this layout; the drop-elaborator (Slice 4c) reads
    /// `tag_width` and `variants` to emit tag-dominant field drops at
    /// transition sites.
    pub machine_layouts: Vec<MachineLayout>,
    /// Layout descriptors for every user-defined `enum` declaration in the
    /// module. Populated by `lower_hir_module` from `HirItem::TypeDecl` items
    /// with `TypeDeclKind::Enum`, in declaration order.
    ///
    /// User enums share the same tagged-union substrate as machines.
    /// `Place::MachineTag` / `Place::MachineVariant` address both forms;
    /// codegen registers enum layouts via `register_enum_layouts` alongside
    /// `register_machine_layouts`.
    ///
    /// Only unit variants appear in `EnumLayout.variants` for this slice.
    /// Payload-bearing variants (`VariantKind::Tuple` / `VariantKind::Struct`)
    /// are not yet supported and will emit a `CodegenError::FailClosed`.
    pub enum_layouts: Vec<EnumLayout>,
    /// Regex literals collected from `HirModule::regex_literals`. Each entry
    /// carries a `literal_id` (0-based index) and the compiled pattern string.
    /// Codegen uses this to emit a module-level `[N x ptr]` global handle
    /// array populated by a `@llvm.global_ctors` init function that calls
    /// `hew_regex_compile` once per pattern before `main` runs.
    ///
    /// `literal_id` is the index into this Vec AND the index into the global
    /// array emitted by codegen, so `literal_id → handle` resolution is a
    /// direct GEP. The array is populated by the module-init constructor; any
    /// null result (invalid pattern) traps fail-closed.
    ///
    /// WHY not in codegen directly from HIR: `IrPipeline` is the codegen
    /// substrate boundary; codegen does not re-read HIR. WHEN-OBSOLETE: never
    /// — the pipeline-field pattern is established for all layout descriptors.
    pub regex_literals: Vec<RegexLiteral>,
    /// Module-level `const` descriptors collected from `HirItem::Const`.
    ///
    /// Each descriptor carries the HIR `ItemId`, declared type, and folded
    /// value. Codegen emits one LLVM global per descriptor, and
    /// `Instr::ConstGlobalLoad { item_id, .. }` resolves back through this
    /// table by `item_id` without re-reading HIR.
    pub user_consts: Vec<MirConst>,
    /// State-record layouts synthesised by the S3b cross-yield-liveness
    /// pass, one entry per gen-block body function in the module. Codegen
    /// (S4) consumes this to emit the tagged-union LLVM struct that backs
    /// `Place::GenState { local, field }` projection. The layout's `name`
    /// matches the gen-body's `RawMirFunction.name`, so codegen resolves a
    /// state place by looking up the layout by the enclosing function's
    /// name and indexing into `fields` by the place's `field` ordinal.
    ///
    /// Empty when the module contains no `gen { … }` blocks; the field is
    /// always present so codegen + tests can index it uniformly.
    pub gen_state_layouts: Vec<GenStateLayout>,
    /// User-declared `extern "<abi>" { fn ...; }` functions lowered from HIR.
    ///
    /// Populated by `lower_hir_module` from `HirItem::ExternFn`. Codegen
    /// pre-declares each entry as an LLVM external symbol BEFORE walking user
    /// functions so `Terminator::Call` lookups by name resolve transparently.
    /// The symbol itself is satisfied at link time — by `hew-runtime` for
    /// `extern "rt"` symbols on the stable JIT ABI, or by a sibling stdlib
    /// staticlib that the driver adds to the native link line.
    pub extern_decls: Vec<ExternDecl>,
    /// Deduplicated, deterministically-ordered registry of `dyn Trait`
    /// vtable instances reached by `Instr::CoerceToDynTrait` anywhere
    /// in the module.
    ///
    /// One entry per unique `(trait_name, concrete_type, vtable_entries)`
    /// triple across every function body. Codegen (W3.031 Stage 2.5 /
    /// Stage 6) emits one LLVM private constant per entry, named
    /// `__hew_vtable__{trait}__{concrete}__{vtable_id}` (see
    /// [`mangle_dyn_vtable_symbol`]), and references it from the second
    /// word of every fat pointer produced by the matching
    /// `CoerceToDynTrait` site.
    ///
    /// Ordering is a stable sort by `(trait_name, format!("{concrete_type}"))`
    /// over first-seen order in `raw_mir` traversal, so the assigned
    /// `vtable_id`s are reproducible across builds of the same module.
    ///
    /// Populated by `lower_hir_module` after every function body is
    /// lowered, by walking each `RawMirFunction.blocks` for
    /// `Instr::CoerceToDynTrait` instructions.
    ///
    /// LESSONS: `checker-authority` (P0) — the trait/concrete/method
    /// resolution lives entirely in the checker side table; this registry
    /// only collects what producers emit. `exhaustive-traversal-and-lowering`
    /// (P0) — every `CoerceToDynTrait` site MUST resolve to exactly one
    /// registry entry; codegen looking up a missing key must fail closed.
    pub dyn_vtable_registry: Vec<DynVtableInstance>,
    /// Checker-authored `HashMap` lowering facts (W3.003).
    ///
    /// Cloned from `TypeCheckOutput.hashmap_layout_facts` after type
    /// checking; values are owned by the pipeline so codegen can
    /// transition the per-site `state` `Pending` → `Finalized` as it
    /// emits the matching layout-backed runtime call.
    ///
    /// Codegen looks up the fact for a given operation site by
    /// `(key_record_name, key_size, key_align)` against the entry's
    /// `abi` and `key_*` fields. Reaching an already-`Finalized` fact
    /// on a fresh op site → `CodegenError::FailClosed` (double-consume
    /// violation). Reaching pipeline finalization with any `Pending`
    /// `LayoutKey` fact remaining → `CodegenError::FailClosed` via
    /// `assert_lowering_facts_consistent` (orphan checker fact).
    ///
    /// LESSONS: `codegen-abi-authority` (P0) — codegen does NOT
    /// re-derive layout eligibility from `ResolvedTy`; the fact
    /// authored at the checker boundary is the only source of truth.
    pub hashmap_lowering_facts: Vec<hew_types::HashMapLoweringFact>,
    /// Checker-authored `HashSet` lowering facts (W3.003).
    ///
    /// Cloned from `TypeCheckOutput.hashset_layout_facts`. Same
    /// lifecycle rules as `hashmap_lowering_facts`: `Pending` on
    /// entry, codegen transitions to `Finalized` at each operation
    /// site by `(elem_record_name, elem_size, elem_align)` lookup,
    /// pipeline finalization fails closed on any remaining `Pending`
    /// element layout fact.
    pub hashset_lowering_facts: Vec<hew_types::HashSetLoweringFact>,
    /// Checker-authored alias-vs-copy decision per actor send site.
    ///
    /// Keyed by the source span of each actor-send argument expression
    /// (the same `SpanKey` the checker inserts during
    /// `enforce_actor_boundary_send`). Populated by
    /// `lower_hir_module_with_facts` from `TypeCheckOutput::actor_send_aliasing`
    /// so codegen (Phase P5.2) can branch on the decision without
    /// re-examining the AST.
    ///
    /// This field mirrors the checker's map for codegen's future use.
    /// MIR lowering itself reads from the map it was called with (via
    /// `lower_hir_module_with_facts`) and stamps each
    /// `Terminator::Send.alias_mode` at construction time.
    ///
    /// LESSONS: `serializer-fail-closed` (P0) — a missing entry maps
    /// to `SendAliasMode::Copy`; `Alias` is ONLY set on explicit
    /// `ActorSendAliasing::Alias` entries.
    pub actor_send_aliasing: HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
    /// Polymorphic (un-monomorphised) MIR for every generic origin function
    /// in the module (W5.007a). One entry per generic `HirFn` whose body is
    /// lowered against `ResolvedTy::TypeParam` operands instead of being
    /// skipped — the abstract counterpart of the concrete `$$`-mangled
    /// instances that populate `raw_mir`. Each entry pairs the abstract body
    /// with its type-parameter binder (see [`PolymorphicMirFunction`]).
    ///
    /// **Not consumed by codegen.** Codegen lowers `raw_mir` /
    /// `elaborated_mir` only; the concrete monomorphic instances remain the
    /// sole emission source, so behaviour is unchanged. This bucket exists as
    /// the representation substrate the W5.007b witness-lowering /
    /// shared-generic work builds on. Any diagnostics produced while lowering
    /// these abstract bodies are intentionally dropped here — surfacing them
    /// would change user-visible output for programs that compile today.
    pub polymorphic_mir: Vec<PolymorphicMirFunction>,
}

impl IrPipeline {
    /// Clone the checker-authored layout-backed `HashMap`/`HashSet`
    /// lowering facts from `tco` into this pipeline (W3.003).
    ///
    /// Driver glue (`hew_compile` / CLI) calls this between
    /// `check_program` and codegen.  Tests that build a pipeline by
    /// hand without a `TypeCheckOutput` leave the fact vectors empty
    /// and bypass the lifecycle: codegen treats an empty fact set as
    /// "no checker-authored layout call sites in this module" and
    /// fails closed if it then encounters one.
    ///
    /// LESSONS: `codegen-abi-authority` (P0) — facts → ops, never
    /// `ResolvedTy` shortcuts at the codegen seam.
    pub fn attach_lowering_facts(&mut self, tco: &hew_types::TypeCheckOutput) {
        self.hashmap_lowering_facts = tco.hashmap_layout_facts.values().cloned().collect();
        self.hashset_lowering_facts = tco.hashset_layout_facts.values().cloned().collect();
    }

    /// Store the checker's `actor_send_aliasing` side table in the pipeline
    /// for codegen (Phase P5.2) to consume.
    ///
    /// This is called by driver glue (`hew-cli`) alongside
    /// `attach_lowering_facts`. It mirrors — for codegen's future reference —
    /// the same map that was passed to `lower_hir_module_with_facts` so that
    /// the decisions stamped on each `Terminator::Send.alias_mode` during
    /// lowering are also available as a flat lookup table.
    ///
    /// Calling this after `lower_hir_module_with_facts` is idempotent: the
    /// `alias_mode` values on existing terminators are already correct; this
    /// method just makes the raw map accessible on the pipeline struct for
    /// diagnostic / `--explain-cow` rendering.
    pub fn attach_actor_send_aliasing(&mut self, tco: &hew_types::TypeCheckOutput) {
        self.actor_send_aliasing
            .clone_from(&tco.actor_send_aliasing);
    }
}

/// One user-declared extern fn — see [`IrPipeline::extern_decls`].
#[derive(Debug, Clone, PartialEq)]
pub struct ExternDecl {
    pub name: String,
    pub abi: String,
    pub param_tys: Vec<ResolvedTy>,
    pub return_ty: ResolvedTy,
}

/// One `dyn Trait` vtable instance — see [`IrPipeline::dyn_vtable_registry`].
///
/// Carries every piece of information codegen needs to emit the LLVM
/// private constant backing this vtable, with no need to round-trip
/// through the type checker or re-canonicalise the originating
/// coercion site:
///
/// * `vtable_id` — 0-based stable index; identifies this entry across
///   the pipeline. The corresponding symbol name comes from
///   [`mangle_dyn_vtable_symbol`] and follows
///   `__hew_vtable__{trait}__{concrete}__{vtable_id}`.
/// * `symbol` — precomputed mangle for direct codegen reference.
///   Always equals
///   `mangle_dyn_vtable_symbol(vtable_id, &trait_name, &concrete_type)`.
/// * `trait_name`, `concrete_type`, `method_table`, `vtable_entries` —
///   copies of the matching `Instr::CoerceToDynTrait` payload fields.
///   `vtable_entries` carries the checker-substituted method signatures
///   (W3.031 Stage 1.6) that codegen consumes verbatim to derive the
///   erased indirect-call type for vtable slot N.
///
/// Two `Instr::CoerceToDynTrait` instances map to the *same* registry
/// entry when their `(trait_name, concrete_type, vtable_entries)` triple
/// is structurally equal — the discriminator for associated-type
/// projections (e.g. `dyn Iterator<Item = i64>` vs
/// `dyn Iterator<Item = String>`) lives in the substituted signatures
/// on `vtable_entries`, which is why all three fields participate in
/// the dedup key.
#[derive(Debug, Clone, PartialEq)]
pub struct DynVtableInstance {
    /// 0-based stable index. Codegen reads `dyn_vtable_registry[vtable_id]`
    /// and emits one LLVM private constant per entry.
    pub vtable_id: u32,
    /// LLVM symbol name for the private constant. Always equals
    /// [`mangle_dyn_vtable_symbol`]`(vtable_id)`; precomputed so the
    /// producer (MIR registry build) and the consumer (codegen) never
    /// drift on the naming scheme.
    pub symbol: String,
    /// Trait name (or `Trait1+Trait2` for multi-bound coercions).
    /// Mirrors the field on `Instr::CoerceToDynTrait`.
    pub trait_name: String,
    /// Resolved concrete `Self` type wrapped by this vtable.
    pub concrete_type: ResolvedTy,
    /// Ordered `(method_name, impl_fn_key)` pairs naming the impl-side
    /// resolution for each trait method, in trait declaration order.
    /// For multi-bound coercions, method names are prefixed by
    /// `Trait::`.
    pub method_table: Vec<(String, String)>,
    /// Ordered vtable entries with checker-substituted method
    /// signatures (W3.031 Stage 1.6 — Q-β / council blocking finding
    /// #4). Codegen (W3.031 Stage 7) consumes each entry's `signature`
    /// verbatim to derive the erased indirect-call type for the
    /// corresponding vtable slot. Order matches `method_table` and the
    /// `slot = 3 + index` convention from
    /// [`hew_types::DynMethodCall::slot`].
    pub vtable_entries: Vec<hew_types::DynVtableEntry>,
}

/// Sanitize a free-form name (trait names like `Trait1+Trait2`,
/// concrete-type names like `Vec<i32, String>`) into the subset that
/// is legal everywhere a vtable / thunk / drop symbol is referenced
/// (LLVM IR identifiers, ELF/Mach-O symbol tables, debugger maps).
///
/// Replaces any character outside `[A-Za-z0-9_]` with `_`. The
/// result is deterministic per `(input)` and stable across builds
/// of the same module.
///
/// **Collision avoidance.** Two distinct trait or concrete names that
/// sanitise to the same string (e.g. `A+B` and `A_B`) would alias at
/// the symbol layer. The `vtable_id` suffix on every symbol family
/// (see [`mangle_dyn_vtable_symbol`], [`mangle_dyn_thunk_symbol`],
/// [`mangle_dyn_drop_in_place_symbol`]) is what makes the final name
/// unique — the trait/concrete substrings are diagnostic
/// disambiguators that keep `nm` / debugger output legible, not the
/// primary uniqueness mechanism.
#[must_use]
pub fn sanitize_for_symbol(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// Stable LLVM symbol name for the vtable static identified by
/// `vtable_id`. Producer (`build_dyn_vtable_registry`) and consumer
/// (codegen, later stages) both call this helper so neither side
/// hard-codes the format. Drift here would surface as a codegen
/// fail-closed "vtable missing" at every coercion site.
///
/// **Format:** `__hew_vtable__{trait}__{concrete}__{vtable_id}`,
/// where `{trait}` and `{concrete}` are
/// [`sanitize_for_symbol`]-cleansed forms of the source names. The
/// `vtable_id` suffix guarantees uniqueness across the dedup-key
/// dimensions the registry discriminates on (e.g. distinct assoc
/// projections that share `(trait_name, concrete_type)`); the
/// trait/concrete substrings keep `nm`-output and debugger maps
/// legible.
///
/// **Linkage.** The symbol is emitted with `Linkage::Private`:
/// internal to the compilation unit, no ABI promise. The
/// trait/concrete substrings keep `nm`-output and debugger maps
/// legible while the `vtable_id` tail guarantees uniqueness across
/// dedup-key dimensions.
#[must_use]
pub fn mangle_dyn_vtable_symbol(
    vtable_id: u32,
    trait_name: &str,
    concrete_type: &ResolvedTy,
) -> String {
    let t = sanitize_for_symbol(trait_name);
    let c = sanitize_for_symbol(&format!("{concrete_type}"));
    format!("__hew_vtable__{t}__{c}__{vtable_id}")
}

/// Stable LLVM symbol name for the erased method thunk in vtable slot
/// `1 + method_index` of vtable `vtable_id`.
///
/// Producer (codegen `emit_dyn_trait_thunks`) and consumer (vtable
/// static emission) both call this helper so neither side
/// hard-codes the format; drift would surface as a fail-closed
/// "thunk missing" at the vtable static initializer.
///
/// Each thunk takes the erased fat-pointer data word (`ptr`) as its
/// first argument, followed by the receiver-skipped parameter list
/// recorded on [`DynVtableInstance::vtable_entries`]`[method_index]
/// .signature`. The thunk forwards the erased self pointer (a
/// `*mut Concrete` in LLVM's opaque-pointer world) directly to the
/// concrete impl method named by
/// [`DynVtableInstance::method_table`]`[method_index].1` and
/// propagates the return value verbatim.
///
/// **Format:** `__hew_dyn_thunk__{trait}__{concrete}__{vtable_id}_{method_index}`,
/// where `{trait}` / `{concrete}` are
/// [`sanitize_for_symbol`]-cleansed. Uniqueness is carried by the
/// `(vtable_id, method_index)` numeric tail — the trait/concrete
/// substrings are diagnostic. Linkage is `Private`, mirroring
/// [`mangle_dyn_vtable_symbol`].
#[must_use]
pub fn mangle_dyn_thunk_symbol(
    vtable_id: u32,
    method_index: u32,
    trait_name: &str,
    concrete_type: &ResolvedTy,
) -> String {
    let t = sanitize_for_symbol(trait_name);
    let c = sanitize_for_symbol(&format!("{concrete_type}"));
    format!("__hew_dyn_thunk__{t}__{c}__{vtable_id}_{method_index}")
}

/// Stable LLVM symbol name for the per-vtable `drop_in_place`
/// function that releases a heap-boxed `dyn Trait` value.
///
/// One function per `vtable_id` (not per method): the vtable's
/// concrete type — recorded on [`DynVtableInstance::concrete_type`]
/// — determines the drop ritual, not the trait or any individual
/// method. Codegen synthesises a function with signature
/// `fn(ptr) -> void` whose body runs the concrete value's drop
/// glue at the supplied erased pointer (today only trivially-
/// droppable concretes are supported; a non-`BitCopy` classification
/// fails closed pending a future registry-side drop-fn key) and
/// then releases the heap box via `hew_dyn_box_free(ptr, size,
/// align)`.
///
/// Producer (`emit_dyn_trait_drop_in_place_fns` in codegen) and
/// consumer (vtable static initialiser) both call this helper so
/// neither side hard-codes the format; drift would surface as a
/// fail-closed "`drop_in_place` missing" at the vtable static
/// initializer.
///
/// **Format:** `__hew_dyn_drop_in_place__{trait}__{concrete}__{vtable_id}`,
/// where `{trait}` / `{concrete}` are
/// [`sanitize_for_symbol`]-cleansed. The `vtable_id` suffix
/// carries uniqueness; the trait/concrete substrings are
/// diagnostic. Linkage is `Private`, mirroring
/// [`mangle_dyn_vtable_symbol`].
#[must_use]
pub fn mangle_dyn_drop_in_place_symbol(
    vtable_id: u32,
    trait_name: &str,
    concrete_type: &ResolvedTy,
) -> String {
    let t = sanitize_for_symbol(trait_name);
    let c = sanitize_for_symbol(&format!("{concrete_type}"));
    format!("__hew_dyn_drop_in_place__{t}__{c}__{vtable_id}")
}

/// A regex literal compiled at module-init time.
///
/// One entry per unique pattern in `HirModule::regex_literals`. The
/// `literal_id` is the 0-based index into `IrPipeline::regex_literals`
/// AND the index into the global handle array emitted by codegen. Pattern
/// string validity has already been checked by the type-checker
/// (`hew-types`), so the compile call at module-init must not fail — if
/// it does, the module-init trap fires (fail-closed, not null-propagation).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegexLiteral {
    /// 0-based index into `IrPipeline::regex_literals`. Stable across
    /// the MIR → codegen boundary; MIR producers embed it as `ConstI64`
    /// arguments to `hew_regex_match` and `hew_regex_capture` calls.
    pub literal_id: u32,
    /// The validated regex pattern string. Embedded in the module as a
    /// NUL-terminated i8 constant; passed to `hew_regex_compile` at init.
    pub pattern: String,
}

/// Constant-folded value carried by a [`MirConst`]. Mirrors
/// `hew_hir::HirConstValue`; carried in the descriptor so codegen does not
/// re-read HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirConstValue {
    /// Folded integer value. The declared width lives in [`MirConst::ty`];
    /// codegen truncates/zero-extends the `i64` payload to that width.
    Integer(i64),
    /// Folded UTF-8 string literal value.
    Str(String),
}

/// Module-level constant descriptor lowered from `hew_hir::HirItem::Const`.
///
/// Mirrors the regex-literal handle-array pattern: one entry per module-level
/// `const`, in declaration order, with `const_id` as the 0-based index into
/// the const-descriptor table (the codegen global-slot index). `item_id` ties
/// the descriptor back to the `ResolvedRef::Const(item_id)` that const
/// references carry, so the codegen global-load seam (a later slice) resolves
/// a reference to its slot without re-reading HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirConst {
    /// 0-based index into the const-descriptor table and the codegen global
    /// slot.
    pub const_id: u32,
    /// Stable HIR item id of the declaration; matches
    /// `ResolvedRef::Const(item_id)` at reference sites.
    pub item_id: ItemId,
    /// Source-declared const name (unique within the module).
    pub name: String,
    /// Declared, checker-resolved type of the const.
    pub ty: ResolvedTy,
    /// Constant-folded value.
    pub value: MirConstValue,
}

/// State-record layout for one generator body function, synthesised by
/// the S3b cross-yield-liveness pass.
///
/// One entry per `__hew_gen_body_*` raw MIR function in the module.
/// `function_name` matches the body's `RawMirFunction.name`; codegen
/// resolves a `Place::GenState { local, field }` by looking up the body's
/// layout and indexing `fields[field as usize]`.
///
/// **Field order is load-bearing.** The S3b synthesis pass emits fields
/// in this fixed order so codegen, drop elaboration (S3b2), and the
/// state-machine prologue (S4) all agree on which ordinal addresses
/// which slot:
///
/// 1. `field == 0` — `tag` (`u32`). State discriminant. `0` is the
///    initial pre-first-yield state; `1..=yield_count` are resume points
///    in source yield-site order; `yield_count + 1` is the terminal
///    `Ended` state.
/// 2. `field == 1` — `init_mask` (`u64`). Per-cross-yield-local
///    initialisation bitmap. Bit `i` is set when the local in
///    `live_locals[i]` is currently held in the state record (i.e.
///    checkpointed at the most recent yield and not yet reloaded by
///    the matching resume). The S3b2 set/clear discipline writes
///    `init_mask = SITE_MASK` immediately before every `Terminator::Yield`
///    (after the per-site checkpoint Moves) and writes `init_mask = 0`
///    at every resume entry (after the per-site reload Moves), so the
///    `__drop_in_state` shim invoked from any suspension drops exactly
///    the fields that hold valid data.
/// 3. `field >= 2` — the cross-yield-live locals in deterministic
///    ascending order by their original body-local `u32` id. Index `i`
///    of `live_locals` corresponds to `field == i + 2`.
#[derive(Debug, Clone, PartialEq)]
pub struct GenStateLayout {
    /// Name of the generator body `RawMirFunction` this layout backs.
    /// Matches the `__hew_gen_body_<owner>_<id>` mint produced by
    /// `lower_gen_block`.
    pub function_name: String,
    /// Cross-yield-live locals in deterministic ascending-id order.
    /// `live_locals[i]` is the original body-local `u32` id whose value
    /// is mirrored into `Place::GenState { local: state_local, field: i + 2 }`
    /// at every yield site and reloaded at every resume.
    pub live_locals: Vec<GenStateLiveLocal>,
    /// Number of `Terminator::Yield` sites in the body, in source order.
    /// Used by codegen (S4) to size the entry-block switch and to index
    /// per-state metadata such as `drop_tables`.
    pub yield_count: u32,
    /// Name of the synthesised `__drop_in_state` MIR function for this
    /// generator (one shim per gen-block). The shim is reachable as
    /// `RawMirFunction { name: drop_shim_name, .. }` in
    /// `IrPipeline.raw_mir`. Codegen (S4) wires the End / Cancelled /
    /// Panic / Drop exit paths through this single shim.
    ///
    /// **S3b2 status.** S3b2 emits a fail-closed Trap-only shim body and
    /// surfaces the load-bearing per-state drop information via
    /// `drop_tables` below; S4 replaces the Trap with the cascade-on-tag
    /// dispatch that consumes `drop_tables` (each table entry becomes a
    /// per-state drop block in the regenerated shim). The function name
    /// is stable across S3b2 → S4 so downstream wiring does not move.
    pub drop_shim_name: String,
    /// Per-state drop manifest, keyed by state-tag value. Entry
    /// `drop_tables[k]` lists the `live_locals` indices that hold valid
    /// data when the generator is suspended at state `k`, in
    /// reverse-init drop order (highest index drops first → mirrors
    /// LIFO drop discipline for affine resources). Index identity:
    /// `state_tag == 0` is the initial pre-first-yield state (no lifted
    /// locals valid); `state_tag == j` for `j` in `1..=yield_count` is
    /// the suspension at the `j`-th `Terminator::Yield` in source
    /// order; `state_tag == yield_count + 1` is the terminal `Ended`
    /// state. `drop_tables.len()` is therefore always
    /// `yield_count + 2`.
    ///
    /// Codegen (S4) consumes this to emit the `__drop_in_state` shim's
    /// per-state drop blocks; the runtime calls the shim from the four
    /// exit paths and the shim dispatches on the live state tag.
    pub drop_tables: Vec<GenStateDropTable>,
}

/// One per-state drop manifest entry in `GenStateLayout.drop_tables`.
///
/// The field carries the `live_locals` indices (NOT the field ordinals)
/// in reverse-init drop order. Codegen translates each index `i` to a
/// `Place::GenState { local: state_local, field: i + 2 }` drop in the
/// shim body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenStateDropTable {
    /// State-tag value this drop table applies to. Range
    /// `0..=yield_count + 1`. The element is its own index into
    /// `GenStateLayout.drop_tables` (i.e. `drop_tables[k].state_tag == k`),
    /// duplicated here so debugging dumps are self-describing without a
    /// containing index.
    pub state_tag: u32,
    /// `live_locals` indices to drop in this state, in reverse-init
    /// (LIFO) order. Empty for the initial state (`state_tag == 0`) and
    /// for the terminal `Ended` state (`state_tag == yield_count + 1`)
    /// because no lifted locals are live in either. For intermediate
    /// suspension states, the set is exactly the per-site checkpoint
    /// set at the corresponding `Terminator::Yield`, reversed.
    pub fields_in_drop_order: Vec<u32>,
}

/// One cross-yield-live local entry in a `GenStateLayout`.
#[derive(Debug, Clone, PartialEq)]
pub struct GenStateLiveLocal {
    /// Original MIR-local id in the body function. The S3b pass
    /// preserves this so codegen can address the bookend Move's `src`
    /// (at the yield) and `dest` (at the resume) through the same
    /// `Place::Local(original_local)` shape.
    pub original_local: u32,
    /// Resolved type of the original local. Codegen consults this when
    /// emitting the LLVM struct field type at `field == 2 + i`.
    pub ty: ResolvedTy,
}

/// Layout descriptor for a named-form `record` declaration. The codegen
/// emitter materialises this as an LLVM named struct type whose body is the
/// field-type list in declaration order. Field-name resolution to the
/// `FieldOffset` ordinal has already been performed by the MIR producer at
/// `RecordInit` / `RecordFieldLoad` construction time, so codegen consumes
/// only the positional type list here.
#[derive(Debug, Clone, PartialEq)]
pub struct RecordLayout {
    /// Record type name. Matches the `name` field on
    /// `ResolvedTy::Named { name, .. }` for a record-typed local.
    pub name: String,
    /// Field types in declaration order. Index `i` corresponds to
    /// `FieldOffset(i)`.
    pub field_tys: Vec<ResolvedTy>,
}

/// Layout descriptor for an `actor` declaration. The state field list follows
/// declaration order; the init parameter list follows source parameter order.
#[derive(Debug, Clone, PartialEq)]
pub struct ActorLayout {
    /// Actor type name.
    pub name: String,
    /// Actor state field names in declaration order.
    pub state_field_names: Vec<String>,
    /// Actor state field types in declaration order.
    pub state_field_tys: Vec<ResolvedTy>,
    /// Actor init parameter names in declaration order. Empty when the actor
    /// has no explicit init block.
    pub init_param_names: Vec<String>,
    /// Actor init parameter types in declaration order. Empty when the actor
    /// has no explicit init block.
    pub init_param_tys: Vec<ResolvedTy>,
    /// Actor init handler symbol. `None` when the actor has no explicit init block.
    pub init_symbol: Option<String>,
    /// `#[on(start)]` handler symbol. `None` when the actor has no start hook.
    pub on_start_symbol: Option<String>,
    /// `#[on(stop)]` handler symbols in lexical declaration order.
    /// Empty when the actor has no stop hooks. Multiple hooks are all run
    /// at terminate time in this order via a synthesised fan-out trampoline.
    pub on_stop_symbols: Vec<String>,
    /// `#[on(crash)]` handler symbol. `None` when the actor has no crash hook.
    pub on_crash_symbol: Option<String>,
    /// Per-actor arena cap in bytes from `#[max_heap(N)]`. `None` means no
    /// annotation (unbounded arena). Codegen reads this to select
    /// `hew_actor_spawn` (None) vs `hew_actor_spawn_opts` with
    /// `arena_cap_bytes = N` (Some). Mirrored into `SupervisorChildLayout`
    /// for the supervisor restart path.
    pub max_heap_bytes: Option<u64>,
    /// Whether the checker determined this actor participates in an actor-ref
    /// cycle. Codegen serializes this into spawn opts for the future
    /// cycle-detection / Machine Lane B runtime consumer.
    pub cycle_capable: bool,
    /// Receive handlers in message-type order.
    pub handlers: Vec<ActorHandlerLayout>,
    /// Mangled symbol of the per-actor synthesized C-ABI clone fn that
    /// codegen (Stage 2/3 of W2.002) emits and that the W2.001 runtime
    /// registers via `hew_actor_set_state_clone`. `None` for layouts
    /// produced before W2.002 Stage 2 lands or for actor declarations
    /// whose state shape is not yet classifier-supported (per
    /// `state_field_clone_kinds`). Stage 2 codegen fail-closes
    /// (`CodegenError::FailClosed`) when this is `None` at a spawn or
    /// supervisor-child site — it never silently elides the
    /// registration.
    ///
    /// Substrate-first (dispatch-invariant #1): the field lives on
    /// every `ActorLayout`, populated even for trivial-state actors
    /// where the synthesized body is `malloc + memcpy`. Plan
    /// `.tmp/orchestration/plans/waves/w2/w2.002-state-clone-codegen-
    /// plan.md` §4.2 emission policy.
    pub state_clone_fn_symbol: Option<String>,
    /// Companion to `state_clone_fn_symbol`: mangled symbol of the
    /// per-actor synthesized C-ABI drop fn that the runtime registers
    /// via `hew_actor_set_state_drop`. Paired emission is load-bearing
    /// — `state_clone_fn_symbol = Some` with `state_drop_fn_symbol =
    /// None` would convert a memory leak into a use-after-free once
    /// Q185(c) is lifted (plan §8.8). Stage 2 codegen consumes the
    /// pair atomically.
    pub state_drop_fn_symbol: Option<String>,
    /// Per-state-field clone classification (W2.002 Stage 1). Index
    /// `i` corresponds to `state_field_tys[i]` /
    /// `state_field_names[i]`. The vector is empty iff
    /// `state_field_tys` is empty (zero-state actors); otherwise it
    /// has the same length as `state_field_tys`.
    ///
    /// `None` for layouts where Stage 1 classification failed
    /// (`ClassificationError` surfaced as a `MirDiagnostic` and
    /// `state_clone_fn_symbol` / `state_drop_fn_symbol` are also
    /// `None` for the same actor — paired absence). Stage 2 codegen
    /// must skip clone+drop registration for such actors and rely on
    /// the W2.001 runtime's `state_clone_fn = NULL` fall-through
    /// (which blocks supervisor restart per `actor.rs:766`).
    pub state_field_clone_kinds: Option<Vec<crate::state_clone::StateFieldCloneKind>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActorHandlerLayout {
    pub name: String,
    pub symbol: String,
    pub msg_type: i32,
    pub param_tys: Vec<ResolvedTy>,
    pub return_ty: ResolvedTy,
    /// Whether dispatch of this handler must be bracketed by an exclusive
    /// actor-state-lock acquire/release pair.
    ///
    /// Set to `true` when the checker determined (via `HirActorStateGuard::Exclusive`)
    /// that the handler body requires exclusive access to the actor's state.
    /// The acquire/release calls are currently emitted by the runtime scheduler
    /// (`hew-runtime/src/scheduler.rs:810,870`); this field exists so MIR
    /// analysis passes (e.g. closures' lock-guard-live-across-yield check)
    /// and future generated-code bracketing can read the contract without
    /// re-deriving it from HIR.
    ///
    /// `false` is reserved for future pure/read-only handler variants that
    /// the checker explicitly marks no-mutate.  All handlers produced by the
    /// current checker carry `HirActorStateGuard::Exclusive`, so this field
    /// is always `true` today.
    ///
    /// Carrying the lock contract as layout metadata (rather than emitted
    /// instructions) lets MIR analysis and codegen bracketing consume the
    /// fact without re-examining HIR. Codegen reads this field to decide
    /// whether to bracket a trampoline dispatch with lock ABI calls. The
    /// field becomes redundant once codegen emits lock calls inline rather
    /// than relying on the scheduler acquire/release path.
    pub requires_state_guard: bool,
}

/// Layout descriptor for a `supervisor` declaration.
///
/// Supervisors are spawn-only actor-likes: they carry an execution context
/// (their bootstrap body lowers under `FunctionCallConv::ActorHandler`) and
/// occupy a position in the parent/child tree, but they do not accept open
/// messages. They therefore deliberately have NO `ActorProtocolDescriptor`
/// (Q87) and no per-handler `msg_type` mapping — there are no receive
/// handlers to address. If a future iteration introduces user-facing
/// supervisor messages (for example, programmatic restart APIs that route
/// through a supervisor mailbox), the descriptor must be added at that
/// point; until then, the absence is load-bearing — codegen knows a
/// `SupervisorLayout` carries spawn structure and nothing else.
///
/// The `children` vector is ordered by topological spawn order
/// (`wired_to:` dependencies spawn first), assigned during MIR lowering
/// from the S-A/S-B-validated DAG. Each `SupervisorChildLayout.spawn_order`
/// records the position within this same vector so codegen and the runtime
/// reconstruction can re-derive ordering without re-running Kahn's
/// algorithm.
#[derive(Debug, Clone, PartialEq)]
pub struct SupervisorLayout {
    /// Supervisor type name (e.g. `App`). Matches the `name` field on the
    /// `HirSupervisorDecl` lifted from the parser.
    pub name: String,
    /// Restart strategy (`one_for_one`, `one_for_all`, `rest_for_one`,
    /// `simple_one_for_one`). `None` when the supervisor declaration
    /// omitted an explicit `strategy:` clause — runtime defaults apply at
    /// codegen.
    pub strategy: Option<hew_hir::HirSupervisorStrategy>,
    /// Maximum number of restarts allowed inside `window`. `None` when the
    /// supervisor declaration omitted `max_restarts:` — runtime defaults
    /// apply at codegen.
    pub max_restarts: Option<i64>,
    /// Restart-budget window, retained as the raw parser literal (e.g.
    /// `"60s"`). Codegen parses this to a concrete `Duration` so the
    /// duration-literal lexer remains the single source of truth for
    /// unit interpretation.
    pub window: Option<String>,
    /// Mangled symbol of the bootstrap function whose body spawns and
    /// wires the declared children in topological order. The function
    /// itself is emitted into `IrPipeline.{thir,raw_mir,checked_mir,
    /// elaborated_mir}` like any other `FunctionCallConv::ActorHandler`
    /// function. See `mangle_supervisor_bootstrap`.
    pub bootstrap_symbol: String,
    /// Children in topological spawn order. Dependencies spawn before
    /// dependents; siblings with no dependency relationship preserve
    /// declaration order (Kahn's algorithm queue is FIFO).
    pub children: Vec<SupervisorChildLayout>,
}

/// One child or pool entry on a `SupervisorLayout`. Lifted from
/// `HirSupervisorChild` with the wired-to map preserved verbatim; codegen
/// reads the `actor_name` to resolve the per-child `ActorLayout` for
/// init-arg shape validation and runtime registration.
#[derive(Debug, Clone, PartialEq)]
pub struct SupervisorChildLayout {
    /// Child slot name (e.g. `cache`). Matches the field name used at
    /// `sup.<name>` access sites.
    pub name: String,
    /// Actor type spawned at this slot. `HirSupervisorChild.ty` is a raw
    /// String (no `ResolvedTy` round-trip) so this mirrors the field
    /// verbatim. The bootstrap function's spawn instructions name this
    /// type for the `Instr::SpawnActor.actor_name` field.
    pub actor_name: String,
    /// `with restart: <policy>` clause. `None` when the child declaration
    /// omitted the clause — runtime defaults apply at codegen.
    pub restart_policy: Option<hew_hir::HirRestartPolicy>,
    /// `true` for `pool name: Type`; `false` for `child name: Type`.
    pub is_pool: bool,
    /// Compile-time-assigned slot index within the child's own slot
    /// space. Static children index into the supervisor's static slot
    /// table; pool children index into the dynamic pool slot table.
    /// Both spaces start at 0 and are disjoint.
    pub slot_index: u32,
    /// `wired_to:` declarations preserved verbatim. Each entry maps an
    /// init-param name on this child's actor type to the sibling-child
    /// name whose handle is passed at spawn time. S-A/S-B have validated
    /// key existence, sibling existence, and type compatibility before
    /// this layout is built.
    pub wired_to: std::collections::HashMap<String, String>,
    /// Zero-based topological-spawn-order position within
    /// `SupervisorLayout.children`. Equals the index of this entry in
    /// that vector — duplicated here for codegen sites that pattern-
    /// match on a single child without re-correlating against the
    /// parent layout.
    pub spawn_order: u32,
    /// Mangled symbol of the `#[on(crash)]` handler on this child's actor
    /// type. `None` when the child's actor declares no crash hook. Codegen
    /// (Slice 3) reads this to populate the `on_crash_fn` pointer in the
    /// emitted `HewChildSpec` literal; if `None`, the field is left null.
    pub on_crash_symbol: Option<String>,
    /// Per-actor arena cap in bytes from `#[max_heap(N)]` on this child's
    /// actor type. Mirrored from `ActorLayout.max_heap_bytes` in the
    /// post-loop pass; `None` when the actor declares no `#[max_heap]`.
    /// Codegen populates `HewChildSpec.arena_cap_bytes` from this field so
    /// the supervisor restart path preserves the cap across crashes.
    pub max_heap_bytes: Option<u64>,
    /// Mirrored from `ActorLayout.cycle_capable` for supervised-child spawn
    /// planning. Codegen serializes this into `HewChildSpec` so the runtime
    /// preserves the bit across supervisor restarts.
    pub cycle_capable: bool,
}

/// Layout descriptor for one state variant in a `machine` declaration.
///
/// Each variant corresponds to one `state` declared in the machine's body.
/// The `field_tys` list carries the payload field types in declaration order
/// (`HirMachineState.fields` order). For zero-field states (the common case
/// in v0.5) this vector is empty.
///
/// Populated in Slice 5 (`lower_hir_module`'s machine arm) from
/// `HirMachineState.fields`. Slice 4a declares the struct with an empty
/// `field_tys` as the metadata anchor; Slice 5 fills the field lists in.
///
/// Codegen (Slice 5) uses `field_tys` to emit the per-variant inner
/// struct body inside the tagged-union LLVM representation. The
/// `Place::MachineVariant { variant_idx, field_idx, .. }` addressing
/// primitive indexes into this list.
#[derive(Debug, Clone, PartialEq)]
pub struct MachineVariantLayout {
    /// State name (e.g. `"Red"`, `"Idle"`). Matches `HirMachineState.name`.
    pub name: String,
    /// Payload field types in declaration order. Empty for zero-field states.
    pub field_tys: Vec<ResolvedTy>,
}

/// Layout descriptor for a `machine` declaration.
///
/// Pairs the machine's name and tag-bit-width with its per-state variant
/// list so codegen (Slice 5) can emit the tagged-union LLVM type and the
/// `Place::MachineTag` / `Place::MachineVariant` addressing primitives can
/// be validated without re-reading HIR.
///
/// The `variants` vector is in state declaration order — `variants[i]`
/// corresponds to the i-th `state` in `HirMachineDecl.states` and to
/// `variant_idx == i` in any `Place::MachineVariant` that names this
/// machine.
///
/// **Slice 4a invariant**: `variants` entries have empty `field_tys`
/// vectors. Slice 5 populates them when it walks `HirMachineState.fields`
/// for each state. The layout entry itself (name + `tag_width` + empty
/// variants) is the metadata anchor that Slice 4b and 4c need to
/// correctly size the switch and dominance checks.
#[derive(Debug, Clone, PartialEq)]
pub struct MachineLayout {
    /// Machine type name (e.g. `"TrafficLight"`). Matches `HirMachineDecl.name`.
    pub name: String,
    /// Bit width of the discriminant tag field. Computed as
    /// `u32::max(1, (state_count as f64).log2().ceil() as u32)` —
    /// the minimum number of bits needed to enumerate all states.
    /// A one-state machine uses a 1-bit tag (two encodings; only 0 is valid)
    /// rather than zero bits to keep the tag field present in the LLVM struct
    /// at all times. Codegen selects `iN` where `N = tag_width`.
    pub tag_width: u32,
    /// Per-state variant layouts in declaration order.
    pub variants: Vec<MachineVariantLayout>,
    /// Event-companion variant layouts in `HirMachineDecl.events`
    /// declaration order.
    ///
    /// The companion enum's tag bit width is
    /// `max(1, (events.len() as f64).log2().ceil() as u32)` — computed
    /// on the fly by codegen from `events.len()` so a separate
    /// `event_tag_width` field is unnecessary. Each entry carries the
    /// event variant name and its payload field type list (empty for
    /// unit events like `event Tick;`).
    ///
    /// WHY here (not as a free-standing `EventLayout`): the event
    /// companion enum is constructed 1:1 with its parent machine —
    /// every machine declaration produces exactly one
    /// `<Name>Event` enum, registered by `register_machine_decl` in
    /// `hew-types`. Carrying the event layout alongside the machine
    /// layout keeps both halves of the step-fn surface together for
    /// codegen consumption.
    pub events: Vec<MachineVariantLayout>,
}

/// Layout descriptor for a user-defined `enum` declaration.
///
/// User enums share the tagged-union substrate (`{ tag: iW, payload: [N x i8] }`)
/// with machine states and event companions. `EnumLayout` is a distinct type
/// (not a sub-case of `MachineLayout`) to avoid carrying the `events` field,
/// which has no meaning for user-declared enums.
///
/// `variants` lists all variants in declaration order (matching the index
/// `machine_ctor_registry` assigned as `variant_idx`). Each `MachineVariantLayout`
/// entry carries the variant name and its payload field types; unit variants
/// have empty `field_tys`, while payload-bearing variants are lowered through
/// the tagged-union payload layout.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumLayout {
    /// Enum type name. Matches `ResolvedTy::Named { name, .. }` for
    /// an enum-typed local.
    pub name: String,
    /// Bit width of the discriminant tag. Computed as
    /// `u32::max(1, (variant_count as f64).log2().ceil() as u32)`.
    pub tag_width: u32,
    /// Per-variant layouts in declaration order. Index `i` matches the
    /// `variant_idx` stored in `machine_ctor_registry` for this enum's ctors.
    pub variants: Vec<MachineVariantLayout>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub statements: Vec<MirStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RawMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub call_conv: FunctionCallConv,
    /// Declared parameter types in declaration order. `params[i]` is the
    /// `ResolvedTy` of the i-th function parameter. Codegen uses this to
    /// declare the LLVM function signature and to emit the parameter-prologue
    /// (store each `llvm_fn.get_nth_param(i)` into `locals[i]`).
    ///
    /// Invariant: `params.len()` equals the number of initial `locals` entries
    /// that correspond to parameter slots. The lowering pass allocates one
    /// `Place::Local` per parameter at the top of `function_body` — these
    /// occupy `locals[0..params.len()]` — and subsequent body-local
    /// allocations begin at `locals[params.len()]`.
    pub params: Vec<ResolvedTy>,
    /// Type-indexed local registers consumed by the backend-authority `Instr`
    /// stream. `locals[i]` is the `ResolvedTy` of `Place::Local(i as u32)`.
    /// The lowering pass allocates one local per value-producing HIR
    /// expression and per `Let`-introduced binding. Parameters occupy
    /// `locals[0..params.len()]` (see `params` invariant above).
    pub locals: Vec<ResolvedTy>,
    pub blocks: Vec<BasicBlock>,
    pub decisions: Vec<DecisionFact>,
    /// When `Some(catalog_key)`, this function is a `#[intrinsic("key")]`
    /// memory-floor declaration (W5.005 / F1b). The lowered `blocks` are a
    /// bodyless placeholder (the source body is `{}`) and are NOT the source
    /// of truth — codegen's `lower_fn` discards them and synthesizes the
    /// trampoline body from the catalog key via the central floor-intrinsic
    /// authority. Threaded from `HirFn::intrinsic_id` by the raw-MIR producer.
    /// Fail-closed (D343): a key codegen cannot synthesize is a hard
    /// `CodegenError`, never a silent empty-body no-op.
    pub intrinsic_id: Option<String>,
}

/// A generic origin function lowered against abstract `ResolvedTy::TypeParam`
/// operands, paired with the type-parameter binder it was lowered under
/// (W5.007a — see [`IrPipeline::polymorphic_mir`]).
///
/// `type_params` is the enclosing function's declared type-parameter scope, in
/// declaration order. It is the binder for every `ResolvedTy::TypeParam` that
/// appears inside `raw`: without it the abstract body is not self-describing
/// (a consumer could not tell a declared parameter from a free one, nor
/// alpha-rename, compare, or re-verify the body). The MIR witness-operand
/// verifier consults exactly this scope to reject out-of-scope abstract
/// operands; storing it here lets the substrate be re-verified independently
/// of the originating `HirFn`.
#[derive(Debug, Clone, PartialEq)]
pub struct PolymorphicMirFunction {
    pub type_params: Vec<String>,
    pub raw: RawMirFunction,
}

/// Classify whether the `arg_index`-th declared parameter of `callee` is an
/// immutable borrow (`&T` → [`ResolvedTy::Borrow`]). This is the type-fact the
/// argument-passing convention consults to decide whether a by-value heap
/// argument must be retained at the call site: a borrow is non-owning, so the
/// caller transfers no ownership and the retain (`VWT.copy`) is skipped.
///
/// The lookup is the seam W5.011-P3 consumes when it begins emitting
/// retain-on-copy for by-value heap arguments — P3 gates that emission on
/// `!callee_param_is_borrow(...)`. P4 lands the type fact and this primitive;
/// no v0.5 source can yet *construct* a borrow value to pass (there is no
/// borrow-of-local expression and no `T → &T` coercion at call sites), so the
/// `true` arm is only reachable today from synthetic MIR. The classifier is
/// proven directly by unit tests rather than through a (currently
/// unconstructable) end-to-end call.
///
/// Fail-safe by construction (R5): an unresolved `callee` (not present in
/// `raw_mir`) or an out-of-range `arg_index` returns `false` — the conservative
/// answer that *keeps* the retain rather than risk dropping ownership. The
/// classifier never indexes out of bounds and never panics.
#[must_use]
pub fn callee_param_is_borrow(raw_mir: &[RawMirFunction], callee: &str, arg_index: usize) -> bool {
    raw_mir
        .iter()
        .find(|f| f.name == callee)
        .and_then(|f| f.params.get(arg_index))
        .is_some_and(|ty| matches!(ty, ResolvedTy::Borrow { .. }))
}

/// Decide whether passing a `CowValue` binding as the `arg_index`-th
/// argument of a user-function call *escapes* the binding's heap buffer —
/// i.e. creates an alias the callee may retain or return, so the caller can
/// no longer prove it is the sole owner at scope exit.
///
/// W5-011 P3, Q313 Choice A. Until the M-COW spine emits retain-on-share at
/// call boundaries, a non-borrow by-value heap argument is shared *without*
/// a refcount bump: the callee receives the same `rc==1` pointer the caller
/// holds. If the caller then dropped the binding at scope exit while the
/// callee (or its result) still referenced it, the program would double-free
/// or dangle. The conservative, double-free-complete answer is therefore to
/// *exclude* such bindings from scope-exit drop (they leak; they never
/// double-free).
///
/// A borrow parameter (`&T`, [`ResolvedTy::Borrow`]) is the one exception:
/// it is non-owning by type, transfers no ownership, and cannot be retained
/// or returned as an owning value — so the source binding keeps sole
/// ownership and *stays* drop-eligible. No v0.5 source can construct a
/// borrow argument yet (see [`callee_param_is_borrow`]); this branch is
/// proven by synthetic-MIR unit tests and is the seam P4 lights up.
///
/// Status note: the shipped W5-011 P3 drop derivation
/// (`derive_cow_sole_owner`, hew-mir/src/lower.rs) does NOT consult this
/// classifier. It proves sole ownership structurally — a binding is dropped
/// only if its backing local is never read as a source operand anywhere in
/// the finalised instruction+terminator stream — which is strictly more
/// conservative: a string passed to *any* parameter (borrow or by-value) is
/// read as a call-arg source operand and excluded (it leaks, never
/// double-frees). This primitive remains the borrow-ABI escape contract the
/// retain-on-copy follow-up will consume once a borrow-at-call-site
/// construction form lands and the derivation can safely keep borrow-arg
/// sources drop-eligible.
#[must_use]
pub fn call_arg_source_escapes(callee_param_is_borrow: bool) -> bool {
    !callee_param_is_borrow
}

/// Decide whether a container-ingress runtime method *copies* its incoming
/// `CowValue` element (so the source binding keeps its own buffer and stays
/// drop-eligible) or *moves* it (so the container now owns the buffer and
/// the source must be excluded from scope-exit drop).
///
/// W5-011 P3, Delta (b). `hew_vec_push` / `hew_vec_set` deep-copy the
/// element into vector-owned storage (`copy_string_element_in`), so the
/// source binding is unaffected — copy-in, drop-eligible. Every other known
/// ingress symbol (`hew_hashmap_insert_layout`, `hew_hashset_insert_layout`,
/// …) moves the handle into the container, which then frees it via the
/// container's own release path — move-in, must be excluded. Unknown
/// symbols fail closed to move-in (excluded): conservative direction never
/// double-frees.
///
/// Status note: as with [`call_arg_source_escapes`], the shipped P3
/// derivation does not consult this — a string handed to a container-ingress
/// runtime call surfaces as a `CallRuntimeAbi` source operand and is excluded
/// unconditionally (copy-in sources leak rather than double-free). This
/// primitive documents the per-symbol release contract the future
/// copy-in-aware refinement will reinstate.
#[must_use]
pub fn container_ingress_is_copy_in(target_symbol: &str) -> bool {
    matches!(target_symbol, "hew_vec_push" | "hew_vec_set")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FunctionCallConv {
    #[default]
    Default,
    ActorHandler,
    ClosureInvoke,
    /// Synthetic adapter that gives a default-callconv free function the
    /// execution-context-bearing task-entry ABI required by `SpawnTaskDirect`.
    TaskEntry,
}

impl FunctionCallConv {
    #[must_use]
    pub fn carries_execution_context(self) -> bool {
        matches!(
            self,
            Self::ActorHandler | Self::ClosureInvoke | Self::TaskEntry
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContextState {
    Outside,
    Inside,
    Exited,
    Invalid,
}

impl ContextState {
    fn meet(self, other: Self) -> Self {
        if self == other {
            self
        } else {
            Self::Invalid
        }
    }
}

/// Validate execution-context carrier marker invariants on hand-built or
/// lowered MIR.
///
/// Context-bearing functions must enter exactly at the entry block, exit before
/// every terminal path, and use context-observing instructions only while the
/// marker lattice is inside the context. Contextless functions reject all
/// carrier instructions so the context substrate cannot be smuggled into
/// ordinary code.
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "CFG marker validation keeps entry/exit/use checks together so diagnostics share one dedupe ledger"
)]
pub fn validate_context_markers(func: &RawMirFunction) -> Vec<MirCheck> {
    let mut findings = Vec::new();
    let mut seen: HashSet<(String, u32, &'static str)> = HashSet::new();
    let push = |findings: &mut Vec<MirCheck>,
                seen: &mut HashSet<(String, u32, &'static str)>,
                block: u32,
                kind: &'static str,
                reason: String| {
        if seen.insert((func.name.clone(), block, kind)) {
            findings.push(MirCheck::ContextBoundaryViolation {
                function: func.name.clone(),
                block,
                kind,
                reason,
            });
        }
    };

    if !func.call_conv.carries_execution_context() {
        for block in &func.blocks {
            for instr in &block.instructions {
                if matches!(
                    instr,
                    Instr::EnterContext
                        | Instr::ExitContext
                        | Instr::CheckCancellation
                        | Instr::ContextField { .. }
                ) {
                    push(
                        &mut findings,
                        &mut seen,
                        block.id,
                        "context-marker-outside-handler",
                            "execution-context carrier instructions are only legal in context-bearing functions"
                            .to_string(),
                    );
                }
            }
        }
        return findings;
    }

    let Some(entry) = func.blocks.first() else {
        push(
            &mut findings,
            &mut seen,
            0,
            "missing-enter-context",
            "context-bearing function has no entry block, so EnterContext cannot dominate the body"
                .to_string(),
        );
        return findings;
    };
    if !matches!(entry.instructions.first(), Some(Instr::EnterContext)) {
        push(
            &mut findings,
            &mut seen,
            entry.id,
            "missing-enter-context",
            "context-bearing function entry block must start with EnterContext".to_string(),
        );
    }

    for block in &func.blocks {
        if matches!(
            block.terminator,
            Terminator::Return | Terminator::Trap { .. }
        ) && !matches!(block.instructions.last(), Some(Instr::ExitContext))
        {
            push(
                &mut findings,
                &mut seen,
                block.id,
                "missing-exit-context",
                "context-bearing function terminal block must end with ExitContext before its terminator"
                    .to_string(),
            );
        }
    }

    let by_id: HashMap<u32, &BasicBlock> = func.blocks.iter().map(|b| (b.id, b)).collect();
    let mut entry_states: HashMap<u32, ContextState> = HashMap::new();
    let mut worklist = vec![entry.id];
    entry_states.insert(entry.id, ContextState::Outside);

    while let Some(block_id) = worklist.pop() {
        let Some(block) = by_id.get(&block_id).copied() else {
            continue;
        };
        let mut state = entry_states
            .get(&block_id)
            .copied()
            .unwrap_or(ContextState::Invalid);

        for instr in &block.instructions {
            match instr {
                Instr::EnterContext => {
                    if state == ContextState::Outside {
                        state = ContextState::Inside;
                    } else {
                        push(
                            &mut findings,
                            &mut seen,
                            block.id,
                            "invalid-enter-context",
                            "EnterContext is only legal before the handler context has been entered"
                                .to_string(),
                        );
                        state = ContextState::Invalid;
                    }
                }
                Instr::ExitContext => {
                    if state == ContextState::Inside {
                        state = ContextState::Exited;
                    } else {
                        push(
                            &mut findings,
                            &mut seen,
                            block.id,
                            "invalid-exit-context",
                            "ExitContext is only legal while the handler context is active"
                                .to_string(),
                        );
                        state = ContextState::Invalid;
                    }
                }
                Instr::CheckCancellation
                | Instr::ContextField { .. }
                | Instr::ActorStateFieldLoad { .. }
                | Instr::ActorStateFieldStore { .. }
                    if state != ContextState::Inside =>
                {
                    push(
                        &mut findings,
                        &mut seen,
                        block.id,
                        "context-use-outside-boundary",
                        "context-observing instructions must execute between EnterContext and ExitContext"
                            .to_string(),
                    );
                    state = ContextState::Invalid;
                }
                _ => {}
            }
        }

        if matches!(
            block.terminator,
            Terminator::Return | Terminator::Trap { .. }
        ) && state != ContextState::Exited
        {
            push(
                &mut findings,
                &mut seen,
                block.id,
                "missing-exit-context",
                "context-bearing function terminal path reaches its terminator outside an exited context"
                    .to_string(),
            );
        }

        for succ in block.successors() {
            let next = entry_states
                .get(&succ)
                .copied()
                .map_or(state, |prev| prev.meet(state));
            let changed = entry_states.get(&succ).copied() != Some(next);
            if changed {
                entry_states.insert(succ, next);
                worklist.push(succ);
            }
        }
    }

    findings
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub id: u32,
    /// Checker-authority stream consumed by `check_raw_mir` and the
    /// use-after-consume / D10 passes. Carries every Hew-level statement and
    /// expression site with its `SiteId`, `BindingId`, and `ResolvedTy`.
    pub statements: Vec<MirStatement>,
    /// Backend-authority stream consumed by `hew-codegen-rs::llvm`. One
    /// `Instr` per machine-level value movement. Both streams are populated
    /// by the same `lower::Builder` pass so the checker and the emitter
    /// agree on what each `SiteId` resolves to.
    pub instructions: Vec<Instr>,
    pub terminator: Terminator,
}

impl BasicBlock {
    #[must_use]
    pub fn successors(&self) -> Vec<u32> {
        match &self.terminator {
            Terminator::Return | Terminator::Trap { .. } => Vec::new(),
            Terminator::Goto { target } => vec![*target],
            Terminator::Branch {
                then_target,
                else_target,
                ..
            } => vec![*then_target, *else_target],
            Terminator::Call { next, .. }
            | Terminator::Yield { next, .. }
            | Terminator::Send { next, .. }
            | Terminator::Ask { next, .. }
            | Terminator::Select { next, .. } => vec![*next],
        }
    }
}

/// Failure class carried by `Terminator::Trap`. The discriminant lets
/// diagnostics, tests, and runtime-trap handlers distinguish the five
/// trap causes without re-walking the IR or re-inferring from context.
///
/// All five variants are declared here; producer bridges land in later
/// slices:
/// - `IntegerOverflow`     — wired by B-2 (overflow-trap lowering)
/// - `IndexOutOfBounds`    — wired by C-2 (Vec/array OOB formalisation)
/// - `DivideByZero`        — wired by B-5 (divide-by-zero trap)
/// - `SignedMinDivNegOne`  — wired by B-5 (signed-MIN/-1 trap)
/// - `ShiftOutOfRange`     — wired by B-5 (shift-range trap)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TrapKind {
    /// Integer arithmetic overflow on `+`, `-`, or `*`. Fires on signed
    /// and unsigned overflow when the default (non-wrapping) operators
    /// are used. Producer: B-2.
    IntegerOverflow,
    /// Array or `Vec<T>` index out of bounds. Fires when `xs[i]` has
    /// `i >= xs.len()` or `i < 0`. Producer: C-2.
    IndexOutOfBounds,
    /// Integer division by zero. Fires when the divisor of `/` or `%`
    /// is zero. Producer: B-5.
    DivideByZero,
    /// Signed integer division of the minimum value by -1 (`i64::MIN /
    /// -1`), which would overflow the result width. Producer: B-5.
    SignedMinDivNegOne,
    /// Shift count outside `[0, width)`. Fires when `<<` or `>>` has a
    /// shift amount that is negative or ≥ the operand's bit-width.
    /// Producer: B-5.
    ShiftOutOfRange,
    /// Supervisor child slot is not live (tag 1 = Transient or tag 2 = Dead)
    /// at the time of the field-access lookup. Per LESSONS `fail-closed-not-pretend`
    /// (P0), MIR traps rather than fabricating a null PID. The LLVM exit code
    /// must stay in lock-step with `HEW_TRAP_SUPERVISOR_CHILD_UNAVAILABLE` in
    /// `hew-runtime/src/supervisor.rs`. Producer: S2 (`FieldAccess` intercept).
    SupervisorChildUnavailable,
    /// Machine `<Name>__step` dispatch reached a state×event combination that
    /// has no transition. Per LESSONS `fail-closed-not-pretend` (P0), MIR
    /// surfaces a typed trap rather than silently returning the receiver
    /// unchanged or fabricating a target state. HIR exhaustiveness checks
    /// already guarantee this trap is dead code in well-typed programs; the
    /// trap proves the property at runtime and is the fail-closed surface
    /// that future codegen grows into when the state×event dispatch tree
    /// replaces the synthesised step function's single-block stub.
    MachineDispatchUnreachable,
    /// `match` expression dispatch fell through every arm at runtime. Per
    /// LESSONS `match-fail-closed` (P0), MIR emits a belt-and-braces trap
    /// after the last arm's check even though the type checker already
    /// rejects non-exhaustive enum matches at compile time. The trap is
    /// dead code in well-typed programs; it proves the property at runtime
    /// and absorbs any future producer bug that lets an unreached value
    /// reach the dispatch chain. Producer: match-expression substrate.
    ExhaustivenessFallthrough,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    /// Return whatever has been written into `Place::ReturnSlot`. The
    /// emitter loads the slot and emits `ret`.
    Return,
    /// Unconditional branch to another block in the same function.
    Goto { target: u32 },
    /// Two-way branch on an i1/i8/i32/i64 local treated as a boolean.
    Branch {
        cond: Place,
        then_target: u32,
        else_target: u32,
    },
    /// Call into a sibling function by name, optionally store its return value,
    /// then branch to `next`.
    Call {
        callee: String,
        args: Vec<Place>,
        dest: Option<Place>,
        next: u32,
    },
    /// Hard abort: emit `llvm.trap` followed by `unreachable`. The
    /// `kind` discriminant identifies the failure class so diagnostics,
    /// tests, and future runtime-trap handlers can distinguish overflow
    /// from OOB from divide-by-zero without re-walking the IR.
    ///
    /// Construction discipline: producers that wire arithmetic overflow
    /// (sub-area B), OOB indexing (sub-area C), divide-by-zero, and
    /// shift-range traps each emit this terminator with the appropriate
    /// `TrapKind`. No producer exists yet for any variant — this slice
    /// introduces the consumer-side primitive; the per-variant producer
    /// bridges land in slices B-2, B-5, C-2, and C-3 respectively.
    Trap { kind: TrapKind },
    /// Generator suspension: yield `value` to the resumer and continue
    /// at `next` on resume. The presence of this terminator in a
    /// function's CFG is what makes `MirCheck::GeneratorBorrowAcrossYield`
    /// interesting; the v0.5 integer spine never constructs it.
    /// Declared here so the borrow-liveness check has a place to look.
    Yield { value: Place, next: u32 },
    /// Actor message send. The sent value at `value` crosses the
    /// actor boundary; `MirCheck::ActorSendEscape` checks the value's
    /// transitive references satisfy the `Send` constraint. Declared
    /// here so the escape check has a construction site to look for;
    /// the v0.5 integer spine never constructs it.
    ///
    /// `alias_mode` is the binary alias-vs-copy discriminant derived
    /// from the checker's `actor_send_aliasing` side table. It is
    /// populated by `lower_actor_send` at MIR construction time and
    /// consumed by codegen (Phase P5.2) to select the send path.
    /// **Fail-closed default is `Copy`**: a site with no checker entry
    /// always uses the safe deep-copy path.
    Send {
        actor: Place,
        msg_type: i32,
        value: Place,
        next: u32,
        /// Alias-vs-copy decision from the checker's side table.
        /// Defaults to `Copy` (fail-closed); see [`SendAliasMode`].
        alias_mode: SendAliasMode,
    },
    /// Actor ask: send `value` to `actor` on a caller-owned reply
    /// channel and resume at `next` once the reply has been received.
    /// The terminator carries two distinct Places by design:
    ///
    /// - `channel` — the `HewReplyChannel*` slot allocated by codegen.
    ///   Used for the runtime ABI sequence
    ///   `hew_reply_channel_new` → `hew_actor_ask_with_channel` →
    ///   `hew_reply_wait` on the winning path, and
    ///   `hew_reply_channel_cancel` → `hew_reply_channel_free` on
    ///   loser-cleanup. Codegen-internal; not user-visible.
    /// - `reply_dest` — the user-visible binding that receives the
    ///   reply value. Populated from `hew_reply_wait`'s return on win.
    ///
    /// Declared variant. The v0.5 integer spine has no construction
    /// surface today — HIR-to-MIR lowers `select{}` arms into
    /// `Terminator::Select` with `SelectArmKind::ActorAsk`; per-arm
    /// body-block construction (the seam that would terminate an arm
    /// body with `Terminator::Ask`) is the `select-wait-dispatch`
    /// cluster's responsibility. Non-select `actor.method()` lowering
    /// is the `actor-method-call-lowering` cluster's responsibility.
    /// The variant is declared here so the MIR shape is forward-
    /// compatible with both clusters and so `MirCheck::ActorAskEscape`
    /// has a construction site to look for when actor-call lowering
    /// lands.
    Ask {
        actor: Place,
        msg_type: i32,
        value: Place,
        reply_dest: Place,
        next: u32,
    },
    /// Sealed `select{}` construct. The terminator carries the per-arm
    /// discriminator and per-arm body block ids; the runtime substrate
    /// that decides the winner and runs loser-cleanup is supplied by
    /// codegen + runtime entries that are not yet wired. Declared here
    /// so the construct's MIR shape is forward-compatible with the
    /// runtime substrate; codegen rejects this terminator with a
    /// `FailClosed` error today.
    ///
    /// The arm vector is non-empty (HIR enforces) and contains at most
    /// one `AfterTimer` arm (HIR enforces). The `next` slot is the
    /// block reached after the winning arm body completes — the join
    /// edge that converges the per-arm bodies.
    Select { arms: Vec<SelectArm>, next: u32 },
}

/// One arm of a sealed `select{}` terminator. Declared-only — the v0.5
/// pipeline never constructs a `Terminator::Select` with attached body
/// blocks; codegen fails closed before reaching the per-arm body
/// dispatch. The per-arm `body_block` is reserved for the cleanup-CFG
/// wire-up when the runtime substrate lands.
#[derive(Debug, Clone, PartialEq)]
pub struct SelectArm {
    pub kind: SelectArmKind,
    /// Block id reached when this arm wins. Unused while codegen fails
    /// closed; reserved for the cleanup-CFG wire-up.
    pub body_block: u32,
    /// `Some(place)` for arms that bind a value (stream/ask/await);
    /// `None` for the `AfterTimer` arm.
    pub binding: Option<Place>,
}

/// The four sealed arm forms mirrored from HIR. The MIR layer carries
/// only the discriminator + the place(s) holding the source operand;
/// the per-form runtime contract is documented at the codegen
/// fail-closed match arms.
#[derive(Debug, Clone, PartialEq)]
pub enum SelectArmKind {
    /// `next(<stream>)` — pending read on a stream.
    StreamNext { stream: Place },
    /// `<actor>.<method>(<args>)` — actor ask.
    ///
    /// `msg_type` and `value` mirror `Terminator::Ask` so codegen
    /// consumes the same packed-payload shape per arm (slice 3
    /// resolves the method name to its handler `msg_type` and packs
    /// the args via `lower_actor_payload` at producer time, the same
    /// path single-shot ask lowering uses). `method` is kept for
    /// diagnostics and producer-side tests; codegen reads `msg_type`
    /// and `value` only.
    ActorAsk {
        actor: Place,
        method: String,
        args: Vec<Place>,
        msg_type: i32,
        value: Place,
    },
    /// `await <task>` — task completion.
    TaskAwait { task: Place },
    /// `after <duration>` — timer.
    AfterTimer { duration: Place },
}

/// An addressable target for a load or store in the backend-authority
/// instruction stream. Cluster 1 needs only `Local(N)` and `ReturnSlot`;
/// later clusters add `YieldSlot`, enum-payload projection, field
/// projection, deref, etc.
///
/// ## M2 substrate variants (declared scaffold)
///
/// `DuplexHandle`, `LambdaActorHandle`, `SendHalf`, and `RecvHalf`
/// are the M2 unified-concurrency substrate's MIR addressing surface.
/// Each carries only a discriminator-pointer to a `Local(N)` so the
/// enum stays `Copy`. The S/R type information lives on the parent
/// local's `ResolvedTy` (`Named { name: "Duplex", args: [S, R] }`).
///
/// The half-handle aliases address direction-isolated ends of a
/// `Duplex<S, R>`'s dual queue: `SendHalf(parent)` is the write-only
/// end of the parent's S-direction; `RecvHalf(parent)` is the
/// read-only end of the parent's R-direction. Dropping a half closes
/// only that direction; the Duplex itself ceases when both halves
/// (or the last unified handle) are gone.
///
/// The HIR currently has no construction surface for `LambdaActor` /
/// `Duplex` (the parser flip lives in slice 1, the HIR-lower for it
/// lands later). These variants exist so the drop-elaboration plan
/// and codegen seam don't have to retrofit `Place` when the
/// construction surface lands. The pattern matches the four other
/// declared-but-never-constructed scaffold variants already in this
/// model (`Terminator::Yield`/`Send`, `MirCheck::Aliasing` /
/// `GeneratorBorrowAcrossYield` / `ActorSendEscape`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Place {
    Local(u32),
    ReturnSlot,
    /// A `Duplex<S, R>` handle. The carried `u32` is the `Local(N)`
    /// id whose `locals[N]` is `ResolvedTy::Named { name: "Duplex",
    /// args: [S, R] }`. Drop semantics: dropping the last surviving
    /// handle closes both directions (design §7.3).
    DuplexHandle(u32),
    /// A lambda-actor handle. The carried `u32` is the `Local(N)` id
    /// whose `locals[N]` is the lambda-actor's `Duplex<Msg, Reply>`
    /// (the surface call-syntax dispatches through this Duplex).
    /// Drop semantics: stop-on-last-handle-drop with weak-ref body
    /// capture (§5.9 ratification 2).
    LambdaActorHandle(u32),
    /// A named actor handle returned by `spawn Actor(...)`. The carried `u32`
    /// is the backing local whose resolved type is `LocalPid<Actor>`.
    ActorHandle(u32),
    /// Write-only end of a `Duplex<S, R>`'s S-direction queue. The
    /// carried `u32` is the parent Duplex's `Local(N)` id (the same
    /// local that a `DuplexHandle` would address). Drop closes the
    /// S-direction only; the R-direction stays open until the
    /// matching `RecvHalf` (or last surviving `DuplexHandle`) drops.
    SendHalf(u32),
    /// Read-only end of a `Duplex<S, R>`'s R-direction queue. The
    /// carried `u32` is the parent Duplex's `Local(N)` id (the same
    /// local that a `DuplexHandle` would address). Drop closes the
    /// R-direction only; the S-direction stays open until the
    /// matching `SendHalf` (or last surviving `DuplexHandle`) drops.
    RecvHalf(u32),
    /// Discriminant tag of a machine value held in `local` (an MIR local
    /// register id, matching `Place::Local(local)`).
    ///
    /// Addresses the integer tag field of the tagged-union representation
    /// of a machine instance. The tag encodes the active state variant as
    /// a zero-based ordinal matching the declaration order in the machine's
    /// `states` list.
    ///
    /// **Tag-dominance authority**: machine payload slots are active only
    /// under a matching tag. Keeping machine tags and payload fields as
    /// dedicated places prevents generic struct-field dataflow from treating
    /// inactive payload bytes as live, and gives the later drop-elaborator a
    /// transition-out hook.
    ///
    /// **Drop semantics**: machine values are `BitCopy` by value-class
    /// (the tag itself carries no heap resources). Dropping a machine
    /// binding releases no resources via this place; entry/exit hooks are
    /// separately emitted by the drop-elaborator at transition sites.
    ///
    /// WHY `u32` (not `BindingId`): codegen has no `BindingId → local`
    /// map; the `BindingId` was always producer-supplied as a name for an
    /// MIR local. Keying the place on the local id directly mirrors the
    /// existing handle places (`DuplexHandle(u32)`, `SendHalf(u32)`, …)
    /// and makes codegen consume it the same way as `Place::Local(u32)`.
    /// WHY declared here: Slice 4a's step shell reads and writes machine
    /// state tags without reusing struct-field places; later transition-body
    /// and drop-elaboration slices extend the same primitive.
    /// WHEN-OBSOLETE: never — permanent MIR primitive once tagged-union
    /// machine layout lands in Slice 5.
    MachineTag(u32),
    /// Active variant payload field of a machine value held in `local`,
    /// dominated by `Place::MachineTag(local)`.
    ///
    /// Addresses a single field within the active variant's payload.
    /// `local` identifies the machine MIR-local (same `u32` id as the
    /// corresponding `MachineTag`). `variant_idx` is the zero-based state
    /// ordinal (declaration order in `HirMachineDecl.states`). `field_idx`
    /// is the zero-based field ordinal within that state's payload
    /// (declaration order in `HirMachineState.fields`).
    ///
    /// **Dominance invariant**: a `MachineVariant` load or store is only
    /// legal when `Place::MachineTag(local)` is known to carry
    /// `variant_idx` at the use site (i.e. the tag-dominance CFG property
    /// holds). Slice 4a preserves that relationship in the shell; later
    /// transition-body and drop-elaboration slices enforce it for real
    /// payload movement.
    ///
    /// WHY declared here: Slice 4b needs to materialise next-state field
    /// values without going through codegen-specific layout. Storing into
    /// `MachineVariant` at transition sites is the substrate-correct seam.
    /// WHEN-OBSOLETE: never — permanent MIR primitive paired with
    /// `MachineTag`.
    MachineVariant {
        local: u32,
        variant_idx: u32,
        field_idx: u32,
    },
    /// Discriminant tag of a user-defined enum value held in `local`.
    ///
    /// Addresses the integer tag field of the tagged-union representation
    /// of a user enum (declared via `type T { enum A; B; ... }`). The tag
    /// encodes the active variant as a zero-based ordinal matching the
    /// declaration order in the enum's `unit_variants` list (and the
    /// `MachineVariantLayout` entries inside the corresponding `EnumLayout`).
    ///
    /// Substrate-distinct from `MachineTag` so future enum-specific drop
    /// and lifecycle policies (which differ from machine entry/exit hooks)
    /// have a typed surface to attach to. The codegen seam delegates to
    /// the same outer-struct field-0 GEP that `MachineTag` uses today —
    /// both enums and machines share the
    /// `{ tag: iW, payload: [N x i8] }` layout — but the MIR primitive
    /// keeps the producer authority typed.
    ///
    /// **Tag-dominance authority**: any `Place::EnumVariant` access is
    /// only legal when the corresponding `EnumTag` value has been
    /// compared and the matching branch has been taken. Slice 2 of the
    /// match-expression substrate enforces this structurally — branch
    /// chains over `EqI32(tag, k)` dominate every load of a payload
    /// projected through `EnumVariant`.
    ///
    /// WHEN-OBSOLETE: never — permanent MIR primitive for user enum
    /// tag-dispatch lowering.
    EnumTag(u32),
    /// Active variant payload field of a user enum value held in `local`,
    /// dominated by `Place::EnumTag(local)`.
    ///
    /// `variant_idx` is the zero-based variant ordinal (declaration order
    /// in the enum's `unit_variants` / `EnumLayout.variants`). `field_idx`
    /// is the zero-based field ordinal within that variant's payload.
    ///
    /// This lane's substrate slice introduces the primitive but does not
    /// emit loads through it — only unit-variant matching is supported
    /// here. Payload-bearing variant destructuring (e.g. `Some(x)`,
    /// `Ok(v)`) lands as a follow-on consumer of this primitive (Option<T>
    /// substrate slice 2), so the dominance contract is documented once
    /// and reused unchanged.
    EnumVariant {
        local: u32,
        variant_idx: u32,
        field_idx: u32,
    },
    /// Field of a generator's state record (the per-gen-block resume struct
    /// synthesised by the S3b cross-yield-liveness pass).
    ///
    /// `local` identifies the MIR-local that holds the state-record value
    /// inside the generator body function. The pass allocates a single
    /// `Place::Local(state_local)` of type `ResolvedTy::Named { name:
    /// "Gen$state$<owner>:<id>", .. }` at the top of the body's locals
    /// vector; `local` is that local id, and every `GenState` Place in the
    /// body addresses into the same backing local. `field` is the 0-based
    /// declaration-order ordinal into the corresponding `GenStateLayout`
    /// owned by `IrPipeline.gen_state_layouts`.
    ///
    /// **Layout authority.** The field-to-ordinal mapping is the
    /// `GenStateLayout` keyed by the body function's name. Field 0 is the
    /// state tag (`u32` discriminant), field 1 is the init-mask
    /// (`u64` per-field initialisation bitmap, populated by the S3b2 drop
    /// elaboration), and fields 2..N are the cross-yield-live locals in
    /// the deterministic order chosen by the S3b synthesis pass (by
    /// ascending local id of the original body local).
    ///
    /// **Dominance.** A `GenState` load is well-formed exactly when the
    /// generator state-tag at the dominating yield site identifies a
    /// resume point that initialised the field. The S3b synthesis pass
    /// emits `Move { dest: GenState{local, field}, src: Local(N) }`
    /// immediately before the corresponding `Terminator::Yield` for
    /// every cross-yield-live local, and the symmetric reload
    /// `Move { dest: Local(N), src: GenState{local, field} }` at the
    /// top of the resume block. S3b2 will additionally validate the
    /// init-mask via the per-state drop shim.
    ///
    /// WHY symmetric with `MachineVariant { local, .. }`: the addressing
    /// shape mirrors the existing tagged-union place primitives — a
    /// state record IS a tagged-union over per-resume-point variants —
    /// so codegen reuses the same layout-keyed GEP discipline.
    /// WHEN-OBSOLETE: never — permanent MIR primitive once S4 lowers it.
    /// Codegen rejects this place with `CodegenError::Unsupported` until
    /// S4 wires the LLVM struct GEP path.
    GenState {
        local: u32,
        field: u32,
    },
}

/// Integer comparison predicate. Maps 1:1 to LLVM `IntPredicate`. The
/// signed-ness selector is intentional: Hew's spine treats `i64` as a
/// signed 64-bit integer, so the default cmp lowerings are signed
/// comparisons. Once unsigned types reach value-bearing positions in
/// the spine, the lowering picks the unsigned variant from the same
/// enum; the IR shape doesn't change.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpPred {
    Eq,
    NotEq,
    SignedLess,
    SignedLessEq,
    SignedGreater,
    SignedGreaterEq,
    /// Unsigned ≥: reinterprets both operands as unsigned. Used by
    /// shift-range checking to catch both negative shift counts (which
    /// become large unsigned values) and counts ≥ bit-width in a single
    /// compare. B-5 wires this; prior slices had no unsigned predicate.
    UnsignedGreaterEq,
}

/// A validated runtime-ABI call payload carried by `Instr::CallRuntimeAbi`.
///
/// Construction is only possible via `RuntimeCall::new`, which enforces that
/// `symbol` is in the `runtime_symbols::MIR_EMITTER_RUNTIME_SYMBOLS` allowlist.
/// Direct struct construction is impossible because the fields are private,
/// so the allowlist check cannot be bypassed at any call site — including
/// release builds (LESSONS P0 `boundary-fail-closed`).
///
/// Consumers (codegen, `instr_places`, MIR dump) access fields through the
/// provided getter methods.
#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeCall {
    /// Validated `hew_*` C-ABI symbol name.
    symbol: String,
    /// Argument places in C-ABI order.
    args: Vec<Place>,
    /// Destination place for the return value, or `None` if discarded.
    dest: Option<Place>,
}

impl RuntimeCall {
    /// Construct a validated runtime-ABI call.
    ///
    /// Returns `Err(UnknownRuntimeSymbol)` if `symbol` is not in the
    /// M2 runtime-ABI allowlist — enforcing the allowlist boundary at
    /// construction in all build profiles (LESSONS P0 `boundary-fail-closed`).
    ///
    /// # Errors
    ///
    /// Returns [`UnknownRuntimeSymbol`] when `symbol` is not recognised by
    /// `runtime_symbols::is_known_runtime_symbol`.
    pub fn new(
        symbol: impl Into<String>,
        args: Vec<Place>,
        dest: Option<Place>,
    ) -> Result<Self, UnknownRuntimeSymbol> {
        let symbol = symbol.into();
        if crate::runtime_symbols::is_known_runtime_symbol(&symbol) {
            Ok(RuntimeCall { symbol, args, dest })
        } else {
            Err(UnknownRuntimeSymbol(symbol))
        }
    }

    /// The validated C-ABI symbol name.
    #[must_use]
    pub fn symbol(&self) -> &str {
        &self.symbol
    }

    /// Argument places in C-ABI order.
    #[must_use]
    pub fn args(&self) -> &[Place] {
        &self.args
    }

    /// Destination place for the return value, or `None` if discarded.
    #[must_use]
    pub fn dest(&self) -> Option<Place> {
        self.dest
    }
}

/// Minimal machine-level instruction set for the spine subset (integer
/// literals, integer add, value moves). Each variant maps to a single
/// inkwell builder call in `hew-codegen-rs::llvm`.
///
/// Variants the emitter cannot lower (Drop on a live heap value, anything
/// coroutine-shaped) emit a hard error rather than silently no-op; the
/// per-variant rejection happens at lowering time, not here.
/// Discriminator for the three integer arithmetic operators that B-2
/// wires through the checked-overflow lowering. Carried by
/// `Instr::IntArithChecked` so codegen can select the matching
/// `llvm.{s,u}{add,sub,mul}.with.overflow.iN` intrinsic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntArithOp {
    Add,
    Sub,
    Mul,
}

/// Signedness discriminator for `Instr::IntArithChecked`. Selects the
/// signed-vs-unsigned LLVM with-overflow intrinsic family at codegen
/// time. Producers read this off the operand's `ResolvedTy` (B-1
/// canonicalised operands and the destination to the same width and
/// signedness so a single field is sufficient).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntSignedness {
    Signed,
    Unsigned,
}

/// Width discriminator for float instructions. Carried on every
/// `Instr::FloatLit` and `Instr::Float*` variant so codegen can select
/// the correct LLVM float type (`float` vs `double`) without re-deriving
/// the width from operand locals. Mirrors `IntSignedness` for integers.
///
/// No implicit widening: `f32 + f64` is rejected by the type checker
/// before MIR construction. Same-width operands only reach these
/// variants, so a single `width` field is sufficient — matches the
/// design invariant in `IntArithChecked.signed`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatWidth {
    /// IEEE 754 single-precision (32-bit).
    F32,
    /// IEEE 754 double-precision (64-bit).
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    /// Semantic marker at actor-handler entry. Codegen emits no user-visible
    /// instruction, but validates that the hidden execution-context argument is
    /// bound before any context-dependent carrier op can execute.
    EnterContext,
    /// Semantic marker at actor-handler exit. Bounds context-derived values:
    /// after this marker they may not be read, returned, captured, or otherwise
    /// propagated across the handler boundary.
    ExitContext,
    /// Explicit cancellation observation point. Codegen lowers this through
    /// the same `hew_actor_cooperate` runtime consult used by cooperate-site
    /// injection.
    CheckCancellation,
    /// Load one field from the hidden `*mut HewExecutionContext` actor-handler
    /// argument by stable byte offset. `dest` supplies the expected field type;
    /// codegen validates it against the known execution-context ABI table before
    /// emitting the byte-offset GEP + typed load.
    ContextField { dest: Place, offset: usize },
    /// `dest = const <value>` as i64.
    ConstI64 { dest: Place, value: i64 },
    /// Two's-complement wrapping `dest = lhs + rhs`. No overflow check.
    /// Producers: `&+` operator sugar (B-4); `.wrapping_add()` method
    /// call (B-3, method body lowering). The default `+` operator uses
    /// `Instr::IntArithChecked` (B-2) for trap-on-overflow semantics.
    IntAdd { dest: Place, lhs: Place, rhs: Place },
    /// Two's-complement wrapping `dest = lhs - rhs`. No overflow check.
    /// Producers: `&-` operator sugar (B-4); `.wrapping_sub()` (B-3).
    IntSub { dest: Place, lhs: Place, rhs: Place },
    /// Two's-complement wrapping `dest = lhs * rhs`. No overflow check.
    /// Producers: `&*` operator sugar (B-4); `.wrapping_mul()` (B-3).
    IntMul { dest: Place, lhs: Place, rhs: Place },
    /// Integer division `dest = lhs / rhs` with no implicit trap guard.
    /// Producers that need trap-on-zero and trap-on-signed-MIN/-1 MUST
    /// emit the divisor checks and branch to a `Terminator::Trap` block
    /// BEFORE emitting this instruction (B-5 does this). Direct emission
    /// of `IntDiv` without that guard is a construct-discipline violation
    /// mirroring `IntAdd`/`IntMul`; no runtime check is added here.
    /// `signed` selects `sdiv` vs `udiv`. Unsigned division can never
    /// produce signed-MIN/-1 overflow, but the divisor-zero check is
    /// still required for both signednesses.
    IntDiv {
        signed: IntSignedness,
        dest: Place,
        lhs: Place,
        rhs: Place,
    },
    /// Integer remainder `dest = lhs % rhs` with no implicit trap guard.
    /// Same guard discipline as `IntDiv`: divisor-zero and
    /// signed-MIN/-1 checks must precede this instruction.
    /// `signed` selects `srem` vs `urem`.
    IntRem {
        signed: IntSignedness,
        dest: Place,
        lhs: Place,
        rhs: Place,
    },
    /// Bitwise AND `dest = lhs & rhs`. Well-defined for all bit widths and
    /// signednesses; no traps, no overflow. Operands and dest must share the
    /// same integer type (enforced upstream by the type checker).
    IntBitAnd { dest: Place, lhs: Place, rhs: Place },
    /// Bitwise OR `dest = lhs | rhs`. Same well-defined semantics as
    /// `IntBitAnd`; no traps or overflow conditions.
    IntBitOr { dest: Place, lhs: Place, rhs: Place },
    /// Bitwise XOR `dest = lhs ^ rhs`. Same well-defined semantics as
    /// `IntBitAnd`; no traps or overflow conditions.
    IntBitXor { dest: Place, lhs: Place, rhs: Place },
    /// Logical boolean NOT `dest = !operand`. Produces canonical bool storage
    /// (`0` or `1`) rather than bitwise-complementing the bool byte.
    BoolNot { dest: Place, operand: Place },
    /// Checked integer unary negation `dest = -operand`.
    ///
    /// Codegen lowers this through `llvm.{s,u}sub.with.overflow.iN(0, operand)`
    /// and stores the overflow flag for the producer-emitted trap branch.
    IntNegChecked {
        signed: IntSignedness,
        dest: Place,
        operand: Place,
        overflow_flag: Place,
    },
    /// IEEE 754 unary negation `dest = -operand`.
    FloatNeg {
        dest: Place,
        operand: Place,
        width: FloatWidth,
    },
    /// Bitwise NOT `dest = ~operand` for integer operands.
    IntBitNot { dest: Place, operand: Place },
    /// Left shift `dest = lhs << rhs`. No signedness on the shift
    /// itself (LLVM `shl`). Producers must check `(rhs as unsigned) >=
    /// bit_width(dest)` before emitting this instruction and branch to
    /// a `Terminator::Trap { kind: TrapKind::ShiftOutOfRange }` block
    /// on the out-of-range path (B-5). No implicit guard here.
    IntShl { dest: Place, lhs: Place, rhs: Place },
    /// Right shift `dest = lhs >> rhs`. `signed` selects arithmetic
    /// shift right (`ashr`) vs logical shift right (`lshr`). Same
    /// out-of-range guard discipline as `IntShl`: check-and-trap MUST
    /// precede this instruction.
    IntShr {
        signed: IntSignedness,
        dest: Place,
        lhs: Place,
        rhs: Place,
    },
    /// Checked integer arithmetic with trap-on-overflow. Lowers to
    /// `call {iN, i1} @llvm.{s,u}{add,sub,mul}.with.overflow.iN(lhs, rhs)`
    /// plus two `extractvalue`s: the iN result into `dest` and the i1
    /// overflow flag into `overflow_flag`. The producing block is
    /// terminated with `Terminator::Branch { cond: overflow_flag,
    /// then_target: trap_bb, else_target: cont_bb }` where `trap_bb`
    /// terminates with `Terminator::Trap { kind:
    /// TrapKind::IntegerOverflow }` and `cont_bb` is the continuation.
    ///
    /// Construction discipline: `dest` and both operands MUST share
    /// the same `ResolvedTy` integer width (B-1 mixed-width rejection)
    /// and `signed` MUST agree with the operand type's signedness.
    /// `overflow_flag` is a fresh local typed as `ResolvedTy::Bool`
    /// (i8 in LLVM lowering) — codegen zero-extends the i1 flag into
    /// the i8 slot.
    ///
    /// Why a single variant covering all three ops: codegen disambiguates
    /// by `op` and `signed` to select one of six intrinsics
    /// (`s{add,sub,mul}` × `u{add,sub,mul}`); a per-op variant would
    /// duplicate the surrounding extract-and-branch shape three times.
    /// LESSONS: `boundary-fail-closed` (P0 — default arithmetic is
    /// the boundary; trap-on-overflow is fail-closed for accidental
    /// overflow); `exhaustive-coverage` (every integer width × every op
    /// × every signedness has an explicit lowering arm).
    IntArithChecked {
        op: IntArithOp,
        signed: IntSignedness,
        dest: Place,
        lhs: Place,
        rhs: Place,
        overflow_flag: Place,
    },
    /// Checked integer opt-out method lowering: `dest = Some(lhs op rhs)`
    /// when the arithmetic does not overflow, otherwise `dest = None`.
    ///
    /// Unlike `IntArithChecked`, this instruction does not branch or trap.
    /// Codegen must keep the intrinsic's arithmetic result and overflow bit
    /// together long enough to construct the destination `Option<W>`
    /// explicitly.
    IntArithCheckedOption {
        op: IntArithOp,
        signed: IntSignedness,
        width: NumericWidth,
        dest: Place,
        lhs: Place,
        rhs: Place,
    },
    /// Saturating integer opt-out method lowering: `dest = lhs op rhs`,
    /// clamped to the signed/unsigned min/max boundary for `dest`'s width
    /// when overflow is reported by the arithmetic intrinsic.
    IntArithSaturating {
        op: IntArithOp,
        signed: IntSignedness,
        width: NumericWidth,
        dest: Place,
        lhs: Place,
        rhs: Place,
    },
    /// `dest = (lhs <pred> rhs)` on integers. The result is written into
    /// `dest` as an integer truth value: `1` for true, `0` for false. The
    /// dest's local type controls the result width (today every cmp dest
    /// is allocated as the front-half's `bool` resolved to i64 by the
    /// type checker; the lowering zero-extends the LLVM i1 result so the
    /// stored value matches the dest's width). When a real bool type
    /// lands in v0.6, the dest narrows to i8/i1 without IR-level rework.
    IntCmp {
        dest: Place,
        pred: CmpPred,
        lhs: Place,
        rhs: Place,
    },
    /// `dest = (lhs is rhs)` — pointer/handle identity comparison.
    ///
    /// Produced exclusively from `HirExprKind::IdentityCompare`, which the
    /// HIR lowering emits for `Expr::Is` once the checker (D-2) has
    /// validated that both operands are identity-bearing types (actor refs,
    /// `Vec`, `HashMap`, `HashSet`, `bytes`, machine instances, user named
    /// `type` declarations). The result is a boolean (`1` = same identity,
    /// `0` = distinct identities), stored in `dest`.
    ///
    /// Codegen (D-3): for pointer-shaped LLVM values (`ptr` alloca, i.e.
    /// `ResolvedTy::Named { name: "Duplex", .. }` and future heap-backed
    /// types), `ptrtoint` both operands to `i64`, compare with `icmp eq`,
    /// then `zext` the `i1` result to the dest's stored width. For
    /// machine-id integers (encoded as stable `i64` identifiers by the
    /// machine runtime), the `ptrtoint` step is skipped and `icmp eq` is
    /// applied directly to the loaded integer values.
    ///
    /// LESSONS: `checker-authority` (P0) — codegen reads the operand's
    /// `ResolvedTy` to select between the pointer-path and the integer-path.
    /// The identity allowance set is the checker's sole responsibility; MIR
    /// and codegen never re-check which types are allowed.
    IdentityCompare { dest: Place, lhs: Place, rhs: Place },
    /// `dest = token.is_cancelled()`.
    ///
    /// Stage 4a carries the token value through MIR without lowering it to the
    /// `hew_cancel_token_is_requested` runtime call. Stage 5a owns the codegen
    /// emission site; until then this instruction is frontend-visible only.
    CancellationTokenIsCancelled { dest: Place, token: Place },
    /// `dest = <src>` — load `src`, store into `dest`.
    Move { dest: Place, src: Place },
    /// Call into a `hew_*` runtime-ABI entry by name. The carried
    /// `symbol` names a `#[no_mangle] extern "C" fn` exported by
    /// `hew-runtime/` (the M2 substrate set is listed in
    /// `crate::runtime_symbols::MIR_EMITTER_RUNTIME_SYMBOLS`). One variant
    /// covers every Duplex / lambda-actor / half-handle runtime
    /// call — codegen disambiguates by the `symbol` string at lower
    /// time. Aligns with the runtime ABI shape: each symbol IS the
    /// authoritative discriminator and the variant carries no
    /// additional structural information beyond the argument
    /// places and the optional destination.
    ///
    /// Construction discipline (LESSONS P0 `boundary-fail-closed`):
    /// producers MUST validate `symbol` against
    /// `crate::runtime_symbols::is_known_runtime_symbol` BEFORE
    /// pushing this instruction. A typo or unrecognised symbol
    /// surfaces as a `MirDiagnostic::NotYetImplemented` at MIR
    /// construction, never as a silent link-time failure.
    ///
    /// `dest = None` denotes a runtime call whose return type the
    /// substrate models as `Result<(), _>` and which the producer
    /// has decided not to bind into a Place (a discarded `.send()`
    /// result, or a half-handle `.close()` that consumes the
    /// receiver). `dest = Some(place)` writes the runtime call's
    /// return value into `place`; codegen (slice 5) materialises
    /// the `inkwell` call result into the local backing `place`.
    ///
    /// WHY (M2 slice 4.5c): the typecheck→HIR/MIR bridge that maps
    /// `Duplex<S, R>::send(msg)` (a `MethodCallRewrite` side-table
    /// entry produced by `hew-types` slice 4.5b) to this variant
    /// does not yet reach the Rust MIR pipeline (`hew compile`
    /// never invokes the typechecker). The variant lands first so
    /// slice 5 codegen can wire a real `inkwell::BuildCall` arm and
    /// the producer-side bridge work in a follow-up slice does not
    /// have to retrofit `Instr`. WHEN-OBSOLETE: producers in
    /// `hew-mir/src/lower.rs` start emitting this variant once the
    /// bridge lands. WHAT: a single producer arm in `lower_value`
    /// that walks a Call whose callee resolves to a builtin /
    /// rewritten symbol and pushes `Instr::CallRuntimeAbi`.
    /// Call into a `hew_*` runtime-ABI entry by name. The payload is a
    /// [`RuntimeCall`] whose constructor enforces the symbol allowlist at
    /// construction in all build profiles — direct struct construction is
    /// impossible because `RuntimeCall`'s fields are private
    /// (LESSONS P0 `boundary-fail-closed`).
    CallRuntimeAbi(RuntimeCall),
    /// Acquire an auto-injected mutex around a cross-suspend mutable
    /// shared-capture access. The `lock` operand is a local that holds
    /// the `*mut HewAutoMutex` handle (sourced from the closure-env or
    /// generator-state lock-slot tail; see
    /// `hew-mir::closure_env::ClosureEnvLayout::lock_slot_for`).
    ///
    /// Codegen lowers this to `call hew_auto_mutex_lock(lock)`. The
    /// producer MUST emit this immediately BEFORE the cross-suspend
    /// access and pair it with a single `AutoLockRelease` immediately
    /// AFTER the access — the suspend itself sits OUTSIDE the bracket
    /// so async tasks never park on a held lock.
    ///
    /// Fail-closed: a producer that emits `AutoLockAcquire` without a
    /// matched `AutoLockRelease` on every continuation path violates
    /// the substrate contract — the runtime guard slot is single-shot
    /// and a missing release surfaces as a debug-assert at the next
    /// `_free` (LESSONS P0 `boundary-fail-closed`).
    AutoLockAcquire {
        /// Local holding the `*mut HewAutoMutex` handle.
        lock: Place,
    },
    /// Release an auto-injected mutex. Pairs with a prior
    /// `AutoLockAcquire` on the same `lock` operand. Codegen lowers
    /// to `call hew_auto_mutex_unlock(lock)`.
    AutoLockRelease {
        /// Local holding the `*mut HewAutoMutex` handle.
        lock: Place,
    },
    /// Construct a first-class callable value from a closure invoke shim and
    /// the environment record materialised at the literal site.
    MakeClosure {
        /// Synthetic function symbol whose ABI is `(ctx, env_ptr, user_args...)`.
        fn_symbol: String,
        /// Environment record place. Codegen stores this place's address in
        /// the closure pair; the env layout is registered in `record_layouts`.
        env: Place,
        /// Destination closure-pair value (`{ fn_ptr, env_ptr }`).
        dest: Place,
    },
    /// Load one captured field from a closure invoke shim's environment pointer.
    ClosureEnvFieldLoad {
        /// Local holding the opaque env pointer parameter.
        env: Place,
        /// Named env-record type whose layout defines `field_offset`.
        env_ty: ResolvedTy,
        /// Capture field index in first-use order.
        field_offset: FieldOffset,
        /// Destination place receiving the field value.
        dest: Place,
    },
    ActorStateFieldLoad {
        field_offset: FieldOffset,
        dest: Place,
    },
    ActorStateFieldStore {
        field_offset: FieldOffset,
        src: Place,
    },
    SpawnActor {
        actor_name: String,
        state: Option<Place>,
        init_args: Vec<Place>,
        dest: Place,
        /// Per-actor arena cap in bytes from the actor's `#[max_heap(N)]`
        /// annotation. `None` means uncapped (runtime default). Codegen
        /// routes `Some(N)` through `hew_actor_spawn_opts`; `None` uses
        /// the plain `hew_actor_spawn` path.
        max_heap_bytes: Option<u64>,
        /// Checker-derived cycle capability from the target actor layout.
        /// Codegen routes `true` through spawn opts even without `#[max_heap]`
        /// so the runtime receives the Machine Lane B policy bit.
        cycle_capable: bool,
    },
    /// Call a first-class callable pair. Codegen loads the function pointer and
    /// environment pointer from `callee`, then emits an indirect call with the
    /// current execution context and environment pointer prepended to `args`.
    CallClosure {
        callee: Place,
        args: Vec<Place>,
        ret_ty: ResolvedTy,
        dest: Option<Place>,
    },
    /// Spawn a no-argument, unit-returning user function as a scope-owned task.
    ///
    /// Codegen synthesises the C-ABI task wrapper (`void (*)(HewTask*)`) and
    /// passes it to `hew_task_spawn_thread`. MIR only constructs this after
    /// validating the fork body is directly observable by cancellation.
    SpawnTaskDirect { task: Place, callee_symbol: String },
    /// Spawn a no-argument, unit-returning closure as a scope-owned task.
    ///
    /// The producer materialises the closure environment record in the parent
    /// frame, then codegen copies it into the task-owned Rc environment before
    /// spawning the worker. The worker wrapper fetches the task env and calls
    /// `fn_symbol(ctx, env_ptr)`, inheriting the parent execution context's
    /// cancellation, supervisor-lineage, and trace lanes.
    SpawnTaskClosure {
        task: Place,
        fn_symbol: String,
        env: Place,
        env_ty: ResolvedTy,
    },
    /// Run the drop ritual for `place`. Cluster 3 makes this first-class:
    /// `drop_fn = Some(name)` calls the `@resource` type's declared
    /// `close(consuming self)` method; `drop_fn = None` is a trivial drop
    /// (no side effect — `@linear` types whose move-checker proof is
    /// elsewhere, or value classes with no implicit close). The inkwell
    /// backend treats trivial drops as no-ops on the integer spine; real
    /// emission of the `close` call lands when `@resource` types reach
    /// the spine subset.
    Drop {
        place: Place,
        ty: ResolvedTy,
        drop_fn: Option<String>,
    },
    /// Witness operation: load the runtime **size in bytes** of `ty` into
    /// `dest` (A606). `dest` is an integer-typed local (`ResolvedTy::Usize`).
    ///
    /// `ty` is the witnessed type. It is either a fully-resolved concrete
    /// type or a `ResolvedTy::TypeParam` standing for an as-yet-unsubstituted
    /// generic parameter (A622). For a concrete type codegen folds this to a
    /// constant; for a `TypeParam` it reads the size out of the abstract
    /// type's value-witness table once monomorphisation has supplied one.
    ///
    /// Construction discipline (LESSONS P0 `boundary-fail-closed`): producers
    /// build the operand through [`WitnessOperand::resolve`], which fails
    /// closed on any checker-internal `Ty` (an unresolved `Ty::Var` in
    /// particular) and admits only a resolved type or a declared type
    /// parameter. The MIR verifier re-checks this invariant
    /// ([`crate::dataflow`]); a `WitnessSizeOf` whose `ty` is a `TypeParam`
    /// not declared in the enclosing function's parameter scope is rejected.
    ///
    /// WHEN-OBSOLETE: codegen lowering of witness ops lands in W5.007b; until
    /// then this instruction is frontend-visible only (it is produced solely
    /// into the gated polymorphic-MIR bucket, which never reaches codegen).
    WitnessSizeOf { dest: Place, ty: ResolvedTy },
    /// Witness operation: load the runtime **alignment in bytes** of `ty`
    /// into `dest` (A606). Same operand discipline, value-witness sourcing,
    /// and gating as [`Instr::WitnessSizeOf`]; `dest` is `ResolvedTy::Usize`.
    WitnessAlignOf { dest: Place, ty: ResolvedTy },
    /// Witness operation: run the **drop glue** for a value of type `ty` held
    /// in `place` (A606). This is the abstract-type counterpart of
    /// [`Instr::Drop`]: where `Drop` names a concrete `drop_fn` (or a trivial
    /// no-op), `WitnessDropGlue` defers the drop strategy to `ty`'s value-
    /// witness table, so a `ResolvedTy::TypeParam` value can be dropped
    /// correctly before its concrete type is known.
    ///
    /// Same operand discipline and gating as [`Instr::WitnessSizeOf`].
    WitnessDropGlue { place: Place, ty: ResolvedTy },
    /// Witness operation: relocate a value of type `ty` from `src` to `dest`
    /// using `ty`'s **move semantics** (A606). Distinct from
    /// [`Instr::Move`], which is an unconditional load/store of a value whose
    /// LLVM shape is already known: `WitnessMove` covers an abstract
    /// (`ResolvedTy::TypeParam`) value whose size/move-glue is supplied by
    /// the value-witness table at monomorphisation time.
    ///
    /// Same operand discipline and gating as [`Instr::WitnessSizeOf`].
    WitnessMove {
        dest: Place,
        src: Place,
        ty: ResolvedTy,
    },
    /// `dest = <global_str_ptr>` — emit an LLVM-level global constant for
    /// `bytes` (null-terminated, internal linkage, read-only) and store the
    /// pointer into `dest`. The `dest` local's type is `ResolvedTy::String`,
    /// which codegen maps to an opaque `ptr` (matching the runtime's
    /// `*const c_char` ABI). No runtime call is made: the pointer refers to
    /// data in the compiled binary's read-only data segment, so
    /// `hew_string_drop` safely skips freeing it via its `is_static_string`
    /// guard. This mirrors the C++ codegen's `hew.global_string` →
    /// `llvm.mlir.global` + `llvm.mlir.addressof` pattern (codegen.cpp
    /// `ConstantOpLowering` / `GlobalStringOpLowering`).
    ///
    /// Escape decoding: `bytes` carries the already-decoded UTF-8 byte
    /// sequence from `HirLiteral::String` — the parser's `unescape_string`
    /// function runs at parse time, so MIR sees decoded bytes. No re-decoding
    /// is needed here.
    ///
    /// Embedded NUL: Hew strings are NUL-terminated C strings at the runtime
    /// boundary. A literal with an embedded NUL byte would be truncated
    /// silently at the first NUL by all C-string runtime operations. The
    /// parser does not produce such literals today; this variant makes no
    /// additional guarantee beyond what the runtime's C-string contract implies.
    StringLit {
        /// Decoded UTF-8 bytes of the literal. LLVM global is emitted as
        /// `bytes` + one NUL terminator byte.
        bytes: Vec<u8>,
        dest: Place,
    },
    /// Load a module-level `const` global into `dest`.
    ///
    /// The global descriptor is owned by [`IrPipeline::user_consts`] and keyed
    /// by the original HIR [`ItemId`]. MIR deliberately carries only the stable
    /// item identity here; codegen fails closed if no descriptor/global exists
    /// for the referenced const.
    ConstGlobalLoad { item_id: ItemId, dest: Place },
    /// Construct a record value by storing each field into a freshly
    /// allocated destination place. `fields` carries `(offset, src)`
    /// pairs in declaration order; `dest` receives the completed record.
    ///
    /// The `FieldOffset` is the 0-based index of the field within the
    /// record's declared field order — the same ordinal that
    /// `RecordFieldLoad` uses for reads. Codegen (A-7) uses the offset
    /// to select the struct GEP index for each field store.
    ///
    /// Functional-update (`R { x: 1, ..base }`) is desugared by the MIR
    /// producer: for every field absent from the explicit list it emits a
    /// `RecordFieldLoad` from the base place, then includes the loaded
    /// place here as if it were an explicit field. No `base` field is
    /// needed on this Instr — codegen sees only flat store-each-field.
    ///
    /// WHY flat: keeps codegen dumb, makes the checker stream see every
    /// field read from the base (important for use-after-consume), and
    /// leaves the memcpy optimisation to A-7 pattern recognition.
    /// WHEN-OBSOLETE: if A-7 determines a memcpy path is always better
    ///   for large records, it can introduce a `RecordCopy { base, dest }`
    ///   variant and route functional-update through it instead.
    RecordInit {
        /// Resolved type of the constructed record (used by codegen to
        /// look up the LLVM struct type for the alloca).
        ty: ResolvedTy,
        /// `(field_offset, source_place)` pairs in declaration order.
        fields: Vec<(FieldOffset, Place)>,
        /// Destination place that receives the constructed record value.
        dest: Place,
    },
    /// Store a single field of a record value by its declaration-order
    /// offset. The aggregate `record` stays `Live` after the store (only
    /// one field's bytes are written; ownership of the aggregate does not
    /// transfer).
    ///
    /// Produced from `HirStmtKind::Assign { target: FieldAccess { object,
    /// field }, value }` when `object` resolves to a writable place (a
    /// `var`-bound local — the assignment-target receiver-mutability gate
    /// is enforced at the checker layer). The field name is resolved to
    /// its 0-based `FieldOffset` via the record-field-order table, same
    /// as `RecordFieldLoad`.
    ///
    /// Codegen lowers this to a GEP + store on the record's alloca,
    /// mirroring `RecordFieldLoad`'s GEP + load.
    RecordFieldStore {
        /// The record value to write into.
        record: Place,
        /// 0-based index of the field within the record's declared field order.
        field_offset: FieldOffset,
        /// Source place whose value is stored into the field.
        src: Place,
    },
    /// Load a single field from a record value by its declaration-order
    /// offset. `dest` receives the field value.
    ///
    /// Produced from `HirExprKind::FieldAccess { object, field }` after
    /// the MIR producer resolves the field name to its 0-based
    /// `FieldOffset` via the record-field-order table.
    ///
    /// Codegen (A-7) lowers this to a GEP + load on the record's alloca.
    RecordFieldLoad {
        /// The record value to read from.
        record: Place,
        /// 0-based index of the field within the record's declared field order.
        field_offset: FieldOffset,
        /// Destination place that receives the loaded field value.
        dest: Place,
    },
    /// Load a single element from a tuple value by its 0-based positional index.
    ///
    /// Produced from `HirExprKind::TupleIndex { tuple, index }` when the tuple
    /// sub-expression resolves to a regular tuple-typed local (not a
    /// `tuple_decomp` runtime-call proxy). The tuple local is laid out as a
    /// packed LLVM struct (declaration-order positional fields, `packed = false`
    /// for natural alignment); codegen emits `build_struct_gep(field_index) +
    /// build_load`.
    ///
    /// The `tuple_decomp` proxy path (runtime multi-output calls) bypasses this
    /// instruction entirely and recovers the individual `Place`s from the side-
    /// table without emitting any additional instructions. This variant handles
    /// every other `TupleIndex` site.
    ///
    /// Producer: `lower_value`'s `HirExprKind::TupleIndex` arm (general case).
    /// Codegen: GEP at `field_index` into the tuple's struct alloca + load.
    TupleFieldLoad {
        /// The tuple value to read from.
        tuple: Place,
        /// 0-based element index (positional declaration order).
        field_index: u32,
        /// Destination place that receives the loaded element value.
        dest: Place,
    },
    /// Construct a tuple value from individual element values.
    ///
    /// Produced from `HirExprKind::TupleLiteral { elements }` after the checker
    /// has validated the tuple type and element types. The tuple is allocated on
    /// the stack as an unnamed LLVM struct; codegen emits one `insertvalue` per
    /// element or a sequence of GEP + store instructions.
    ///
    /// `elements` carries the `Place`s for each tuple slot in declaration order.
    /// The tuple type is derived from `dest`'s type in the MIR `locals` table.
    ///
    /// Producer: `lower_value`'s `HirExprKind::TupleLiteral` arm.
    /// Codegen: alloca for the tuple struct + per-element insertvalue or store.
    TupleConstruct {
        /// Element values in tuple-slot order (0-based).
        elements: Vec<Place>,
        /// Destination place that receives the constructed tuple.
        dest: Place,
    },
    /// `dest = const <float>` stored as a bit pattern.
    ///
    /// `value_bits` is the IEEE 754 bit-pattern of the float constant:
    /// - For `F32`: `(value as f32).to_bits() as u64` (upper 32 bits zero).
    /// - For `F64`: `value.to_bits()`.
    ///
    /// Storing the bit pattern avoids f32/f64 coercion in the MIR model
    /// and lets codegen reconstruct the exact constant via
    /// `f32_type().const_float_from_apfloat` / `f64_type().const_float`.
    ///
    /// Producer: `lower_literal` for `HirLiteral::Float`, width from `expr.ty`.
    FloatLit {
        dest: Place,
        value_bits: u64,
        width: FloatWidth,
    },
    /// `dest = const <char>` stored as its Unicode scalar value.
    ///
    /// Hew `char` is a Unicode scalar value (U+0000 to U+D7FF and U+E000 to
    /// U+10FFFF). The scalar value is stored as a `u32` bit pattern; codegen
    /// maps it to an `i32` constant (matching C's `int32_t` convention for
    /// Unicode code points). The `u32` encoding is total — Rust's `char as u32`
    /// never produces a surrogate or out-of-range value.
    ///
    /// Producer: `lower_literal` for `HirLiteral::Char`, cast via `c as u32`.
    CharLit {
        /// Unicode scalar value of the character constant.
        value: u32,
        dest: Place,
    },
    /// `dest =` — a unit value with no data content.
    ///
    /// Unit is zero-sized. The MIR producer emits this variant to give the
    /// dest place a definition point in the instruction stream; codegen may
    /// emit nothing, a zero-size alloca, or an undef constant depending on
    /// whether `dest` is ever read after this point. In practice, unit-typed
    /// bindings are dropped before they reach codegen in well-typed programs,
    /// so the variant is primarily a completeness placeholder.
    ///
    /// NOTE: `HirLiteral::Unit` is currently never produced by the HIR
    /// lowerer (no parser `Literal::Unit` exists; unit expressions reach MIR
    /// via other HIR node kinds). This variant is present for exhaustiveness
    /// so that a future producer arm has a corresponding MIR representation.
    ///
    /// Producer: `lower_literal` for `HirLiteral::Unit`.
    UnitLit { dest: Place },
    /// `dest = const <duration>` stored as nanoseconds in an `i64`.
    ///
    /// Hew duration literals (`100ms`, `5s`, `1h`, `10ns`, etc.) are
    /// resolved to nanoseconds at parse time by `parse_duration_literal`.
    /// The `i64` nanosecond encoding can represent durations from ~−292 years
    /// to ~+292 years, which covers all practical use cases.
    ///
    /// The runtime representation (nanoseconds as `i64`) is consistent with
    /// `HirLiteral::Duration(i64)` and the parser's `Literal::Duration(i64)`,
    /// which both carry nanoseconds. No representation decision is made here;
    /// the upstream has already committed to `i64` nanoseconds.
    ///
    /// Producer: `lower_literal` for `HirLiteral::Duration`, forwarding the
    /// pre-computed nanosecond value directly.
    DurationLit {
        /// Duration value in nanoseconds.
        nanos: i64,
        dest: Place,
    },
    /// IEEE 754 float addition `dest = lhs + fadd rhs`. No overflow trap —
    /// out-of-range results produce `+inf`/`-inf` per IEEE 754 §6.1.
    FloatAdd {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// IEEE 754 float subtraction `dest = lhs - rhs` (`fsub`).
    FloatSub {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// IEEE 754 float multiplication `dest = lhs * rhs` (`fmul`).
    FloatMul {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// IEEE 754 float division `dest = lhs / rhs` (`fdiv`).
    ///
    /// Division by zero yields `+inf`, `-inf`, or `NaN` per IEEE 754 §7.3 —
    /// there is no runtime trap. Producers MUST NOT add a divisor-zero check
    /// (contrast with `IntDiv`, which requires one). No trap blocks are emitted.
    FloatDiv {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// IEEE 754 float remainder `dest = lhs % rhs` (`frem`, equivalent to
    /// C99 `fmod`). IEEE 754 semantics: `frem(x, 0)` → `NaN`; no trap.
    FloatRem {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// Construct a `dyn Trait` fat pointer from a concrete value.
    ///
    /// Produced from `HirExprKind::CoerceToDynTrait` at every accepted
    /// `T → dyn Trait` coercion site (the checker's
    /// `TypeCheckOutput::dyn_trait_coercions` side table — see
    /// `hew-types/src/check/coerce.rs::try_record_dyn_trait_coercion`).
    /// Codegen (TO-4) lowers this to a `(data_ptr, vtable_ptr)` pair
    /// where `vtable_ptr` references the LLVM private constant for the
    /// `(trait_name, concrete_type)` pair: the value is stack-allocated /
    /// pointer-taken into `data_ptr` and the vtable symbol is materialised
    /// from the codegen-side `(Trait, ImplType)` registry.
    ///
    /// The carried `method_table` is the checker-authoritative
    /// `(trait_method_name, impl_fn_key)` resolution: codegen emits one
    /// function-pointer slot per entry, in declaration order, starting at
    /// vtable slot 3 (after the runtime-fixed prefix triple
    /// `drop_in_place`/`size_of`/`align_of`). For multi-bound coercions the
    /// method names are prefixed by their originating trait (`Trait::method`).
    ///
    /// LESSONS: `checker-authority` (P0) — the trait/concrete/method
    /// resolution lives entirely in the checker side table; MIR and codegen
    /// never re-derive impl functions from the type system.
    CoerceToDynTrait {
        /// Source place holding the concrete `Self` value to wrap.
        value: Place,
        /// Destination place that receives the constructed `dyn Trait`
        /// fat pointer.
        dest: Place,
        /// Trait name (or `Trait1+Trait2` for multi-bound coercions) the
        /// fat pointer represents.
        trait_name: String,
        /// Resolved concrete `Self` type at the coercion site. Codegen
        /// keys the vtable static on `(trait_name, concrete_type)`.
        concrete_type: ResolvedTy,
        /// Ordered `(method_name, impl_fn_key)` pairs naming the impl-side
        /// resolution for each trait method. Order matches the trait
        /// declaration order; multi-bound coercions prefix each method
        /// name with `Trait::`.
        method_table: Vec<(String, String)>,
        /// Ordered vtable entries with checker-substituted method signatures.
        vtable_entries: Vec<hew_types::DynVtableEntry>,
    },
    /// Dispatch a method call through a `dyn Trait` fat pointer's vtable.
    ///
    /// Produced from `HirExprKind::CallDynMethod` at every accepted
    /// method-call on a `Ty::TraitObject` receiver (the checker's
    /// `TypeCheckOutput::dyn_trait_method_calls` side table — see
    /// `hew-types/src/check/methods.rs` `TraitObject` arm). Codegen
    /// (TO-4) lowers this to:
    ///
    /// 1. Load `vtable_ptr` from the fat pointer's second word.
    /// 2. GEP to `slot` (the precomputed vtable index).
    /// 3. Load the function pointer at that slot.
    /// 4. Call the function pointer with `fat_pointer.data` as the
    ///    receiver, followed by `args`.
    ///
    /// `slot` is precomputed by the checker (`3 + method_decl_order` for
    /// the originating trait — see `DynMethodCall::slot`) so MIR and
    /// codegen carry no trait-method-order knowledge of their own.
    ///
    /// `dest = None` denotes a discarded return value; `dest = Some(place)`
    /// writes the return into `place`.
    ///
    /// LESSONS: `checker-authority` (P0) — slot index is the checker's
    /// authority, not MIR's; `boundary-fail-closed` (P0) — HIR rejects
    /// a missing side-table entry before MIR sees the call.
    CallTraitMethod {
        /// Fat-pointer place holding the `dyn Trait` receiver
        /// (constructed by `CoerceToDynTrait` or bound from a parameter
        /// of declared type `dyn Trait`).
        fat_pointer: Place,
        /// Destination for the return value, or `None` if discarded.
        dest: Option<Place>,
        /// Trait name from which the method was resolved.
        trait_name: String,
        /// Trait method name being dispatched.
        method_name: String,
        /// Pre-computed vtable slot index (`3 + method_decl_order`).
        slot: u32,
        /// Argument Places in source order (the implicit receiver is
        /// `fat_pointer.data` and is NOT included here).
        args: Vec<Place>,
        /// Caller-side method signature with trait type parameters and
        /// associated-type bindings already substituted by the checker
        /// (e.g. `Self::Item -> int`). **Receiver parameter is already
        /// filtered out** — `Checker::lookup_trait_method` returns the
        /// receiver-skipped form, and that is what flows from
        /// [`hew_types::DynMethodCall::signature`] →
        /// [`hew_hir::HirExprKind::CallDynMethod::signature`] → here.
        /// [`DynVtableEntry::signature`] is constructed from the same
        /// `lookup_trait_method` call (`hew-types/src/check/coerce.rs`
        /// `build_vtable_entries`) so the shape is symmetric — neither
        /// includes Self at `params[0]`.
        ///
        /// W3.031 Stage 1.6 (Q-β / council blocking finding #4): the
        /// typed signature is self-contained on this instruction.
        /// Codegen (Stage 7) consumes it **verbatim** to derive the
        /// erased indirect-call type:
        /// 1. prepend a single `ptr` argument (the fat-pointer data
        ///    word, forwarded to the erased thunk in slot N);
        /// 2. lower `params` and `return_type` normally (do NOT drop
        ///    `params[0]` — it is the first real argument, e.g. the
        ///    `int` index for `dyn Index::at(int)`, NOT a Self
        ///    receiver).
        ///
        /// Codegen MUST NOT re-derive the signature from the impl fn,
        /// re-walk vtable entries, or look it up from a side table —
        /// that path would skip the checker's associated-type
        /// projections (LESSONS: `checker-authority` P0).
        ///
        /// Boxed to keep the `Instr` variant under the
        /// `clippy::large_enum_variant` threshold.
        signature: Box<hew_types::FnSig>,
    },
    /// Typed placeholder for a machine `emit(Event { ... })` expression
    /// inside a transition body.
    ///
    /// WHY this placeholder exists: the emit-queue runtime ABI (async event
    /// delivery from a state-machine transition to its own event queue) is
    /// wired in a later slice. Recording the intent here preserves
    /// type-correct MIR through pipeline stages that would otherwise skip the
    /// expression.
    ///
    /// WHEN-OBSOLETE: replaced by a real runtime-call sequence when the
    /// emit-queue ABI lands. This variant must be searched and replaced at
    /// that time — its presence in final codegen-input MIR is an error.
    ///
    /// WHAT the real solution looks like: `Instr::CallRuntimeAbi` to
    /// `hew_machine_emit` with the machine binding, event-index constant,
    /// and serialised payload — wired when the emit-queue ABI is finalised.
    MachineEmitPlaceholder {
        /// Zero-based index into `HirMachineDecl.events` (declaration order).
        event_idx: usize,
        /// Lowered payload field places in source-declaration order.
        /// Empty for unit events (no payload).
        payload: Vec<Place>,
    },
    /// Load the discriminant tag of an enum-typed value into an integer dest.
    ///
    /// `src` must reference a local whose `ResolvedTy` is a `Named` enum
    /// (e.g. the machine event companion `TrafficLightEvent`). The tag is
    /// the zero-based variant ordinal in declaration order — for the
    /// machine event companion, this matches `HirMachineDecl.events`
    /// declaration order.
    ///
    /// `dest` is an integer-typed local (`ResolvedTy::I64` in Slice 4b);
    /// codegen widens the LLVM enum tag to the dest's width via `zext`.
    ///
    /// # Why declared here
    ///
    /// Slice 4b's machine step function dispatches on `(state_tag, event_tag)`
    /// pairs and needs an event-tag-load primitive symmetric with
    /// `Place::MachineTag`. The event companion is a `TypeDefKind::Enum`
    /// with no MIR-side tag-load surface prior to this slice, so the
    /// dispatch tree would otherwise have no way to compare the event
    /// parameter against a constant event index.
    ///
    /// Codegen lowering lands alongside the Slice 5 tagged-union work that
    /// wires the LLVM enum layout. Until then, codegen fails closed if
    /// this instruction reaches it — matching the existing fail-closed
    /// stub for `Place::MachineTag` / `Place::MachineVariant`.
    EnumTagLoad {
        /// Local holding an enum value (`Ty::Named { name, .. }` where
        /// the named type is a `TypeDefKind::Enum`).
        src: Place,
        /// Integer-typed destination for the tag ordinal.
        dest: Place,
    },
    /// `dest = state-name-table[<tag of src>]`.
    ///
    /// Lowers the user-visible `m.state_name()` surface for a machine
    /// receiver. Codegen reads `Place::MachineTag(src_local)` and uses it
    /// to index a per-machine static string table populated alongside the
    /// machine's tagged-union layout in `register_machine_layouts`. Each
    /// table entry is the address of a private read-only NUL-terminated
    /// global string. `hew_string_drop` skips static-segment pointers via
    /// `is_static_string`, so no clone or heap allocation is needed —
    /// matches `Instr::StringLit`.
    ///
    /// `machine_name` carries the unqualified machine type name so codegen
    /// can look up the layout (which owns the state-name globals) without
    /// re-deriving it from the local's resolved type.
    MachineStateName {
        machine_name: String,
        src_local: u32,
        dest: Place,
    },
}

/// Construction boundary for the type operand of a witness instruction
/// ([`Instr::WitnessSizeOf`] and friends) — W5.007a.
///
/// Witness ops carry a `ResolvedTy` (a resolved concrete type, or a
/// `ResolvedTy::TypeParam` standing for a declared abstract parameter), never
/// a checker-internal `hew_types::Ty`. This type is the single fallible gate
/// that producers funnel a `Ty` through to obtain that operand. It fails
/// closed (LESSONS P0 `boundary-fail-closed`): a `Ty::Var` or any other type
/// that does not resolve — and is not a declared type parameter — yields
/// [`MirCheck::WitnessOperandUnresolved`] rather than a fabricated placeholder.
#[derive(Debug)]
pub struct WitnessOperand;

impl WitnessOperand {
    /// Resolve a frontend `Ty` into a witnessable [`ResolvedTy`] operand
    /// against the enclosing function's declared `type_params` scope.
    ///
    /// * A bare, no-argument `Named` whose name is in `type_params` becomes
    ///   `ResolvedTy::TypeParam` (the abstract operand).
    /// * Any other type is resolved via the normal
    ///   [`ResolvedTy::from_ty_with_type_params`] converter; an unresolved
    ///   `Ty::Var` (or any other non-resolvable form) fails closed with
    ///   [`MirCheck::WitnessOperandUnresolved`].
    ///
    /// # Errors
    ///
    /// Returns [`MirCheck::WitnessOperandUnresolved`] when `ty` is a
    /// checker-internal type that does not resolve to a concrete
    /// [`ResolvedTy`] and is not a declared type parameter — an unresolved
    /// inference variable (`Ty::Var`), an error placeholder, an
    /// unmaterialized literal, or an unresolved associated-type projection.
    pub fn resolve(
        ty: &hew_types::Ty,
        type_params: &HashSet<String>,
    ) -> Result<ResolvedTy, MirCheck> {
        ResolvedTy::from_ty_with_type_params(ty, type_params).map_err(|_| {
            MirCheck::WitnessOperandUnresolved {
                ty: format!("{ty:?}"),
                reason: "witness operand type does not resolve to a concrete \
                         type or a declared type parameter"
                    .to_string(),
            }
        })
    }
}

///
/// For named records (`record Point { x: i64, y: i64 }`) the offset of
/// `x` is `0` and the offset of `y` is `1`, matching the order in which
/// fields were declared. This ordinal is the number codegen passes to the
/// LLVM GEP `struct_gep` call to address the field's alloca slot.
///
/// For tuple records the field order matches the positional declaration
/// (`record Pair(i64, i64)` → field 0 and field 1), but tuple records
/// use the function-call constructor and are NOT reachable via
/// `HirExprKind::StructInit` — they never produce `RecordInit` or
/// `RecordFieldLoad` instructions. The offset type is shared for
/// symmetry and for future use if tuple destructuring is added.
///
/// WHY u32: matches the existing convention for `Place::Local(u32)` and
/// `BasicBlock::id: u32`. A record with > 4 billion fields is impossible
/// in practice; the cast from `usize` at the construction site is checked
/// via `try_from` so an impossibly large offset would panic at MIR time
/// rather than silently truncate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldOffset(pub u32);

/// Discriminates the two kinds of yield-check site the cooperate-site
/// analysis identifies. Consumed by codegen to select the injection point
/// within the LLVM function.
///
/// - `FunctionEntry`: cooperate call emitted in the function prologue,
///   after alloca slots are set up and before the first user instruction.
///   Present for every non-leaf function. Ensures that calling into a
///   non-trivial function always decrements the reductions counter.
///
/// - `LoopBackEdge`: cooperate call emitted in the back-edge block —
///   the block whose terminator is `Goto { target }` where `target` is
///   an ancestor block in the CFG. Armed but dormant in v0.5 because the
///   current MIR lowering only produces acyclic CFGs (loop lowering is
///   deferred). The field is populated by the analysis when synthetic
///   back-edge CFGs are constructed in tests and fires automatically once
///   loop lowering lands.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CooperateKind {
    /// Inject cooperate at function entry (prologue).
    FunctionEntry,
    /// Inject cooperate at the loop back-edge block (before the `Goto`
    /// terminator that returns control to the loop header).
    LoopBackEdge,
}

/// One cooperate-check site identified by the cooperate-site analysis.
/// Carried by `CheckedMirFunction::cooperate_sites` for codegen to consume.
///
/// `bb_id` identifies the block where the cooperate call must be injected;
/// `kind` tells codegen whether this is a function-entry or loop-back-edge
/// site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CooperateSite {
    /// Basic-block id in the owning function's `blocks` vec. For
    /// `FunctionEntry`, this is always block 0 (the entry block). For
    /// `LoopBackEdge`, this is the id of the block whose `Goto`
    /// terminator targets an earlier block.
    pub bb_id: u32,
    /// Injection kind — function-entry prologue or loop back-edge.
    pub kind: CooperateKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CheckedMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    /// CFG basic blocks, mirroring `RawMirFunction.blocks`. Slice 1 of
    /// the CFG-construction lane carries a single entry block (id 0)
    /// terminated by `Terminator::Return`; Slice 2 widens the surface
    /// to multi-block CFGs once `If` lowering builds Branch + Goto +
    /// join terminators. Every consumer that previously read a single
    /// `block` field iterates `blocks[*]` now — the entry block remains
    /// `blocks[0]`.
    pub blocks: Vec<BasicBlock>,
    pub decisions: Vec<DecisionFact>,
    pub checks: Vec<MirCheck>,
    /// Yield-check injection sites identified by the cooperate-site analysis.
    ///
    /// Codegen iterates this vec to decide where to emit
    /// `call @hew_actor_cooperate()`. Empty means the function is a leaf
    /// (< 10 MIR statements, no calls, no loops) or has a yield-equivalent
    /// entry terminator (receive handler) — no cooperate call is emitted.
    ///
    /// WHY here and not on `ElaboratedMirFunction`: cooperate-site injection
    /// is a pure analysis result with no effect on drop elaboration or
    /// type-checker decisions. Placing it on `CheckedMirFunction` lets
    /// codegen read it without threading through the elaboration pass.
    ///
    /// WHEN-OBSOLETE: if a future change decides cooperate injection interacts
    /// with drop elaboration (e.g. a cooperate inside a cleanup block), the
    /// field can be migrated to `ElaboratedMirFunction` at that time.
    pub cooperate_sites: Vec<CooperateSite>,
}

/// Per-function legality findings produced by Checked MIR. A
/// `CheckedMirFunction` with any non-`DecisionMapTotal` `MirCheck`
/// is rejected by `hew compile`; no backend artefact is emitted.
///
/// The variants are exhaustive over the v0.5 move/borrow/init/aliasing
/// surface. Variants whose construction surface doesn't yet exist in
/// the spine (`Aliasing`, `GeneratorBorrowAcrossYield`,
/// `ActorSendEscape`) are declared here so subsequent work that adds
/// borrow ops, `Terminator::Yield` construction, and actor-send
/// lowering doesn't have to retrofit the enum. They cannot be
/// constructed by passes today because the IR has no surface for
/// them; the passes that target them are no-ops on the current spine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirCheck {
    /// A binding is used before any initialising `Bind` for it appears
    /// in the function's statement stream.
    InitialisedBeforeUse {
        binding: BindingId,
        name: String,
        use_site: SiteId,
    },
    /// A non-`BitCopy` binding was consumed and then read again. The
    /// payload carries the consume site and the offending use site so
    /// the diagnostic surface can point at both ends of the bug.
    UseAfterConsume {
        binding: BindingId,
        name: String,
        consumed_at: SiteId,
        used_at: SiteId,
    },
    /// A shared and mutable borrow of the same place are simultaneously
    /// live, violating read-shared XOR mutate-unique. Declared variant;
    /// no construction surface in the v0.5 integer spine — borrow ops
    /// will populate it once they land.
    Aliasing {
        conflicting_borrows: Vec<(SiteId, BorrowKind)>,
    },
    /// A borrow is live across a generator yield point. Declared
    /// variant; `Terminator::Yield` exists as the suspension site
    /// shape, but the generator-body construction surface that builds
    /// it isn't in the v0.5 integer spine.
    GeneratorBorrowAcrossYield { place: Place, yield_point: SiteId },
    /// A non-`Send` value escapes across an actor message boundary.
    /// Declared variant; `Terminator::Send` exists as the boundary
    /// shape, but actor lowering that builds it isn't in the v0.5
    /// integer spine.
    ActorSendEscape { place: Place, send_site: SiteId },
    /// A non-`Send` value escapes across an actor ask boundary as the
    /// request payload. Symmetric to `ActorSendEscape`: that variant
    /// checks the fire-and-forget send boundary, this one checks the
    /// request side of an ask boundary. Declared variant;
    /// `Terminator::Ask` exists as the boundary shape, but actor-call
    /// lowering that constructs it isn't in the v0.5 integer spine.
    ActorAskEscape { place: Place, ask_site: SiteId },
    /// Structural invariant on the lowering: every value-producing
    /// `SiteId` must have a `DecisionFact` with a concrete `Strategy`
    /// (not `UnknownBlocked`). Violation indicates a lowering bug, not
    /// a user error — surface as a hard rejection.
    DecisionMapTotal { offending_sites: Vec<SiteId> },
    /// A `@linear` (`ValueClass::Linear`) binding is live at an exit
    /// without being consumed via a declared consuming method. Symmetric
    /// to `UseAfterConsume`: that variant rejects consume-then-use, this
    /// one rejects bind-but-never-consume. The payload carries the
    /// exit site for diagnostic anchoring (which path forgot to commit).
    MustConsume {
        binding: BindingId,
        name: String,
        exit_site: SiteId,
        ty: ResolvedTy,
    },
    /// Drop-elaboration could not determine the live-set at a `Return`
    /// block — either the meet-lattice produced an ambiguous state
    /// for an M2 substrate handle (`Duplex` / lambda-actor /
    /// half-handle) or the structural invariant ("every drop in the
    /// per-exit plan resolves to a `DuplexHandle` / `LambdaActorHandle`
    /// / `SendHalf` / `RecvHalf` Place that has a recorded
    /// `DropKind`") fails. The elaborator aborts with this finding
    /// rather than emitting a partial drop plan (LESSONS:
    /// boundary-fail-closed, cleanup-all-exits). The payload carries
    /// the offending block id and a short reason so the diagnostic
    /// surface can anchor the rejection.
    DropPlanUndetermined { block: u32, reason: String },
    /// Execution-context carrier invariant failed: actor handlers must bracket
    /// their bodies with EnterContext/ExitContext and ordinary functions must
    /// not contain carrier instructions.
    ContextBoundaryViolation {
        function: String,
        block: u32,
        kind: &'static str,
        reason: String,
    },
    /// A value derived from `Instr::ContextField` crossed an `ExitContext`
    /// boundary by being read or returned after the context had been exited.
    ContextBindingEscapes { place: Place, block: u32 },
    /// A witness instruction ([`Instr::WitnessSizeOf`] and friends) carries a
    /// type operand that is not legally witnessable (A622). Two failure
    /// modes feed this finding:
    ///
    /// * **construction boundary** — a producer tried to build a witness
    ///   operand from a checker-internal `Ty` that does not resolve to a
    ///   concrete `ResolvedTy` (an unresolved `Ty::Var`, a free inference
    ///   hole); [`WitnessOperand::resolve`] returns this rather than
    ///   fabricating a placeholder type (LESSONS P0 `boundary-fail-closed`).
    /// * **verifier** — a `ResolvedTy::TypeParam` operand reached the MIR
    ///   verifier without being declared in the enclosing function's
    ///   type-parameter scope, i.e. an out-of-scope abstract type.
    ///
    /// `reason` carries a short human-readable cause for diagnostic
    /// anchoring; `ty` is the rejected operand rendered for display.
    WitnessOperandUnresolved { ty: String, reason: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElaboratedMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    /// Checker-authority statement stream, retained for compatibility with the
    /// existing `--dump-mir elab` consumers and for snapshot continuity.
    /// Drop-elaboration's authoritative output is `blocks` + `drop_plans` —
    /// once the inkwell emitter consumes those directly, `statements` becomes
    /// pure documentation (council R-C3.1 — staged retirement).
    pub statements: Vec<MirStatement>,
    pub decisions: Vec<DecisionFact>,
    /// Basic-block CFG: drop-elaboration's structural output. One entry per
    /// `BasicBlock` (id-indexed via the block's own `id` field, matching
    /// `RawMirFunction::blocks`). Spine-only functions (no `@resource` /
    /// `@linear` locals) carry a single `Normal` block.
    pub blocks: Vec<ElabBlock>,
    /// Per-`ExitPath` drop plan. One entry per terminator-bearing exit edge
    /// across the function. Spine-only functions carry a single
    /// `ExitPath::Return` with an empty `DropPlan`.
    pub drop_plans: Vec<(ExitPath, DropPlan)>,
    /// Generator state schema. `None` on every non-generator function;
    /// the field is a reserved slot for generator state-machine lowering.
    // DROP-TODO: populate when generator construction surface lands
    pub coroutine: Option<CoroutineSchema>,
    /// Closure-capture metadata for every lambda-actor body in this
    /// function. Empty on non-actor functions; one entry per
    /// captured binding per lambda-actor literal. The codegen layer
    /// (slice 5) consumes this to emit the right capture-strength
    /// (strong refcount bump vs weak-handle allocation) so the
    /// runtime's self-binding weak-ref discipline (§5.9
    /// ratification 2) holds. Declared scaffold; HIR construction
    /// surface for lambda-actor capture-set discovery lands later.
    pub lambda_captures: Vec<LambdaCapture>,
}

/// One captured binding inside a lambda-actor body. The
/// `capture_kind` discriminator is the structural fact codegen needs
/// for the runtime to honour the self-binding weak-ref discipline:
/// a `Weak` capture must NOT bump the actor's external strong
/// refcount, so when external handles drop, the actor stops even
/// though the body still references its own binding name.
///
/// See `CaptureKind::Weak` for the §5.9 ratification 2 narrative.
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaCapture {
    /// The lambda-actor handle this capture belongs to. The Place
    /// is the actor's `LambdaActorHandle(N)` — the spawn-site
    /// binding. Codegen uses this to associate captures with the
    /// right actor's body frame.
    pub actor_handle: Place,
    /// The captured binding's id from the enclosing scope.
    pub captured: BindingId,
    /// The captured binding's name (for diagnostics). The name is
    /// load-bearing for the self-ref case: a `Weak` capture whose
    /// name matches the lambda-actor's own let-binding-name is the
    /// recursive-self case (§5.9 ratification 2).
    pub name: String,
    /// Capture-strength discriminator (Strong vs Weak).
    pub capture_kind: CaptureKind,
}

/// Capture-strength selector for a lambda-actor body capture.
///
/// `Strong` is the default for every non-self capture: the captured
/// value's refcount (for `@resource` types) is bumped so the actor
/// body keeps the captured handle alive. `Weak` is the self-binding
/// recursive case (§5.9 ratification 2): the actor's body
/// references its own binding name, but the reference is held
/// weakly so the body does NOT keep the actor alive past external
/// refcount zero. When the last external handle drops, the actor
/// stops; the body's weak self-ref upgrades fail and recursive
/// self-sends surface as `SendError::ActorStopped` (the runtime
/// contract lands in slice 4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CaptureKind {
    /// Strong capture: bumps the captured value's refcount; the
    /// body keeps the captured handle alive for as long as the
    /// body's own frame lives.
    Strong,
    /// Weak capture: does NOT bump the captured value's refcount.
    /// The body holds a weak handle that upgrades only while the
    /// captured value's strong refcount is non-zero. Used for the
    /// lambda-actor's own self-binding-name to break the
    /// body-keeps-self-alive cycle.
    Weak,
}

/// A basic-block kind. `Normal` blocks carry user-level statements;
/// `Cleanup` blocks run drop plans on panic / cancel / outer-trap edges
/// per HEW-SPEC §3.7.8.4. Cleanup blocks are reachable only via
/// `ExitPath::Panic` / `ExitPath::Cancel` predecessors and always
/// flow to a strictly outer cleanup block or to function-trap; this is
/// enforced structurally — cleanup blocks form a tree, never a cycle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockKind {
    Normal,
    Cleanup,
}

/// Elaborated basic block. Carries the same `id` as the corresponding
/// `RawMirFunction::blocks[id]` for normal blocks; cleanup blocks use
/// fresh ids past the highest normal-block id.
#[derive(Debug, Clone, PartialEq)]
pub struct ElabBlock {
    pub id: u32,
    pub kind: BlockKind,
    /// Drop instructions to fire on entry to this block. Empty for normal
    /// blocks; populated in LIFO declaration order for cleanup blocks.
    pub drops: Vec<ElabDrop>,
    /// Successor block to jump to after firing this block's drops. `None`
    /// indicates the function terminates here (trap / return-from-cleanup).
    pub successor: Option<u32>,
}

/// Exit-path discriminator. One value per outgoing edge a function's
/// CFG can take. Mirrors `Terminator::*` plus the new `Cancel` variant
/// (scope-structural cancellation propagation in `fork{}` blocks per
/// HEW-SPEC §3.7.8.4 "lexical task cancellation"). Generator suspension
/// (`Yield`) and actor send (`Send`) are declared so the elaboration pass
/// is exhaustive; the integer spine never constructs them.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExitPath {
    Return {
        block: u32,
    },
    Goto {
        block: u32,
        target: u32,
    },
    Branch {
        block: u32,
        then_target: u32,
        else_target: u32,
    },
    Call {
        block: u32,
        callee: String,
        next: u32,
    },
    Panic {
        block: u32,
    },
    Cancel {
        block: u32,
    },
    Yield {
        block: u32,
        next: u32,
    },
    Send {
        block: u32,
        actor: String,
        next: u32,
    },
    /// Actor-ask exit. Mirrors `Terminator::Ask`. The `channel` Place
    /// is what the loser-cleanup sequence needs
    /// (`hew_reply_channel_cancel` + `hew_reply_channel_free`); the
    /// `reply_dest` Place is irrelevant in the exit path because the
    /// reply value is only consumed inside the winner body. Declared
    /// so the elaboration pass is exhaustive; the spine never
    /// constructs `Terminator::Ask` today, so this exit is unreachable
    /// in practice until the construction surface lands.
    Ask {
        block: u32,
        actor: Place,
        next: u32,
    },
    /// Sealed `select{}` exit. Mirrors `Terminator::Select`; declared
    /// so the elaboration pass is exhaustive. The spine never
    /// constructs this — codegen rejects `Terminator::Select` before
    /// the elaboration pass would observe a `Select` exit at runtime.
    Select {
        block: u32,
        next: u32,
    },
}

/// Ordered drop sequence for a single exit. Drops fire in
/// reverse-declaration (LIFO) order: the latest-bound `@resource`
/// drops first.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct DropPlan {
    pub drops: Vec<ElabDrop>,
}

/// A single elaborated drop op. Either a `@resource` drop calling the
/// type's `close` method (`drop_fn = Some(name)`), or a trivial drop
/// for a value class with no side effect (`drop_fn = None`).
///
/// `kind` discriminates the M2 substrate's structural drop semantics:
/// generic `@resource` close, Duplex-handle close-both-directions,
/// half-handle close-one-direction, and lambda-actor stop-on-last.
#[derive(Debug, Clone, PartialEq)]
pub struct ElabDrop {
    pub place: Place,
    pub ty: ResolvedTy,
    pub drop_fn: Option<String>,
    /// Drop-kind discriminator. Distinguishes the structural close
    /// semantics that codegen (slice 5) and runtime (slice 4) need
    /// to honour. Generic `@resource` drops use `DropKind::Resource`
    /// (the existing path); M2-substrate drops use the specialised
    /// variants. Defaults to `Resource` so existing call sites that
    /// only populate the pre-M2 fields stay correct.
    pub kind: DropKind,
}

/// Drop-kind discriminator for `ElabDrop`. Each variant pins a
/// distinct structural close-protocol contract that the runtime
/// (slice 4) and codegen (slice 5) layers must implement.
///
/// The pre-M2 path emits `DropKind::Resource` for every owned
/// `@resource` binding — the existing close-method dispatch through
/// `drop_fn = Some("Type::close")`. The three M2 variants encode the
/// dual-queue Duplex protocol's three drop shapes (design §7.3-§7.4
/// + §5.9 ratification 2):
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DropKind {
    /// Generic `@resource` drop: call the type's `close` method (or
    /// no-op if `drop_fn` is `None`). The pre-M2 default — every
    /// owned `AffineResource` local lowers to this kind.
    Resource,
    /// `Duplex<S, R>` handle drop with close-both-directions. When
    /// the last surviving handle for this Duplex drops, both the
    /// S-direction and R-direction queues close. Codegen emits a
    /// `hew_duplex_close(handle)` runtime call (slice 5); the
    /// runtime's refcount + dual-queue protocol decides whether
    /// this drop is the last-handle case (slice 4).
    DuplexClose,
    /// Half-handle drop that closes one direction of a Duplex's
    /// dual queue. The carried `Direction` selects which queue
    /// closes; the other direction stays open until the matching
    /// half (or the last unified handle) drops. Codegen emits a
    /// `hew_duplex_close_half(handle, direction)` runtime call
    /// (slice 5).
    DuplexHalfClose(Direction),
    /// Lambda-actor stop-on-last-handle-drop. When the external
    /// strong refcount reaches zero, the actor stops; the body's
    /// recursive self-ref is held weakly so it does NOT keep the
    /// actor alive past external refcount zero (§5.9 ratification
    /// 2). Codegen emits a `hew_lambda_actor_release(handle)`
    /// runtime call (slice 5); the runtime decides whether this
    /// drop is the last-handle case (slice 4).
    LambdaActorRelease,
    /// `dyn Trait` fat-pointer drop: dispatch through vtable slot 0
    /// (`drop_in_place`) on the pointer's `data` word, then release the
    /// fat-pointer storage according to `storage`. Codegen (TO-4 / Stage
    /// 6 in plan W3.031) emits the GEP-to-slot-0 + load + call sequence,
    /// then either does nothing (`FrameOwned` — the concrete value lives
    /// in a caller-allocated alloca; `drop_in_place` is sufficient) or
    /// calls `hew_dyn_box_free` on the data pointer (`HeapBoxed` — the
    /// concrete value lives in a `hew_dyn_box_alloc`-allocated heap
    /// buffer that must be released after `drop_in_place` returns).
    ///
    /// The vtable static itself has program lifetime and is never freed.
    ///
    /// The `storage` discriminator is populated by the MIR producer at
    /// each `dyn Trait` binding's introducing statement (W3.031 Stage 1):
    /// — coercion sites (`HirExprKind::CoerceToDynTrait`) and direct
    /// parameter bindings flow through `FrameOwned`; call results that
    /// return `dyn Trait` (the heap-box ABI from W3.031 Stage 0) flow
    /// through `HeapBoxed`. Reaching codegen with a `TraitObject` drop
    /// whose storage was never set is a structural fail-closed event —
    /// the MIR builder emits a `TraitObjectStorageUndetermined` diagnostic
    /// instead, so codegen never sees a malformed drop kind.
    TraitObject { storage: TraitObjectStorage },
    /// W5.011 — function-scope drop of a single heap-owning `CowValue`
    /// local (`string`, `Bytes`, `Vec<T>`, `HashMap<K,V>`, `HashSet<K>`).
    /// The carried `drop_fn` is the C-ABI runtime release symbol whose
    /// single argument is the handle pointer loaded from the binding's
    /// alloca: `hew_string_drop`, `hew_vec_free`, `hew_hashmap_free_layout`,
    /// `hew_hashset_free_layout`. Codegen loads the pointer, calls the
    /// symbol, and null-stores the slot (`raii-null-after-move`).
    ///
    /// `ElabDrop::drop_fn` stays `None` for this kind — the symbol lives in
    /// the variant so the runtime-vs-user `resolve_drop_fn` dispatch (which
    /// only fires for `ElabDrop::drop_fn = Some(_)`) is not consulted and
    /// the `CowHeap` arm is a single, self-describing release call. The symbol
    /// is `&'static str` (a fixed runtime release-symbol literal); the
    /// emitter declares it as `void(ptr)` via `get_or_declare_drop_helper`.
    CowHeap { drop_fn: &'static str },
    /// W5.011 — function-scope recursive drop of a heap-owning aggregate
    /// `CowValue` local whose payload transitively contains heap-owning
    /// leaves (`Tuple`, `Array`). There is no separate descriptor registry
    /// in MIR; the structural descriptor IS the `ElabDrop::ty` the variant
    /// travels with — codegen resolves the aggregate's field/element layout
    /// from that `ResolvedTy` via the type-layout path (A609: sizes come
    /// from the resolved LLVM struct/array type, never `size_of`), GEPs each
    /// heap-owning field, and recurses. The walk carries a depth bound and
    /// fails closed past it (cyclic-descriptor guard, `boundary-fail-closed`)
    /// — Hew has no recursive owned value types today, so the bound is a
    /// safety net rather than a live path.
    AggregateRecursive,
}

/// Storage discriminator for `DropKind::TraitObject`. Distinguishes the
/// two well-defined ownership shapes a `dyn Trait` fat-pointer's data
/// word can take in the post-W3.031 ABI:
///
/// - `FrameOwned`: the concrete value lives in a caller-allocated
///   alloca; the fat pointer's `data` word is an interior pointer into
///   that frame slot. Drop fires `drop_in_place` via vtable slot 0 and
///   stops there — the alloca is reclaimed when the stack frame unwinds.
///   This is the shape every `CoerceToDynTrait`-produced fat pointer
///   takes, and the shape every `dyn Trait` parameter takes (the caller
///   owns the alloca it pointed `data` at).
///
/// - `HeapBoxed`: the concrete value lives in a `hew_dyn_box_alloc`-
///   allocated heap buffer; the fat pointer's `data` word is the owning
///   pointer to that buffer. Drop fires `drop_in_place` via vtable slot
///   0, then calls `hew_dyn_box_free(data, vtable)` to release the
///   buffer. This is the shape every call result returning `dyn Trait`
///   takes (the heap-box ABI is the only ownership transfer path across
///   a function return boundary — frame-pointer escape would alias a
///   reclaimed alloca).
///
/// Populated by the MIR builder at each `dyn Trait` binding's
/// introducing statement (W3.031 Stage 1) and threaded into the
/// `DropKind::TraitObject` variant in `build_lifo_drops`. Codegen
/// (W3.031 Stage 6) reads the discriminator to select the post-
/// `drop_in_place` release ritual.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TraitObjectStorage {
    /// Caller-allocated alloca; no post-`drop_in_place` release.
    FrameOwned,
    /// `hew_dyn_box_alloc`-allocated heap buffer; release with
    /// `hew_dyn_box_free` after `drop_in_place`.
    HeapBoxed,
}

/// Direction selector for `DropKind::DuplexHalfClose`. Mirrors the
/// `SendHalf` / `RecvHalf` Place variants: `Send` closes the
/// S-direction queue, `Recv` closes the R-direction queue.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    /// S-direction (send) queue: the write-end the `SendHalf`
    /// addresses.
    Send,
    /// R-direction (recv) queue: the read-end the `RecvHalf`
    /// addresses.
    Recv,
}

/// Generator state-machine schema. Declared scaffold; constructed in
/// Cluster 4.
#[derive(Debug, Clone, PartialEq)]
pub struct CoroutineSchema {
    /// The single resume-state type carrying the generator's locals
    /// across yield points.
    // PROBE-AMBIGUITY: Cluster 4 fills
    pub state: ResolvedTy,
    /// Every yield site in the generator body, in source order.
    // PROBE-AMBIGUITY: Cluster 4 fills
    pub yield_points: Vec<SiteId>,
    /// Places captured into the state record.
    // PROBE-AMBIGUITY: Cluster 4 fills
    pub captured: Vec<Place>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirStatement {
    Bind {
        binding: BindingId,
        name: String,
        site: SiteId,
        ty: ResolvedTy,
    },
    Evaluate {
        site: SiteId,
        ty: ResolvedTy,
    },
    Use {
        binding: BindingId,
        name: String,
        site: SiteId,
        ty: ResolvedTy,
        intent: IntentKind,
    },
    Return {
        site: Option<SiteId>,
        ty: ResolvedTy,
    },
    Drop {
        binding: BindingId,
        name: String,
        ty: ResolvedTy,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirDiagnostic {
    pub kind: MirDiagnosticKind,
    pub note: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirDiagnosticKind {
    UseAfterConsume {
        binding: BindingId,
        name: String,
        consumed_at: SiteId,
        used_at: SiteId,
    },
    /// A binding is read before any initialising `let` for it appears.
    /// Surfaced from `MirCheck::InitialisedBeforeUse` in commit 2.
    InitialisedBeforeUse {
        binding: BindingId,
        name: String,
        use_site: SiteId,
    },
    /// Structural invariant on lowering: a `DecisionFact` carries
    /// `Strategy::UnknownBlocked`. Surfaced from
    /// `MirCheck::DecisionMapTotal`.
    DecisionMapTotal { offending_sites: Vec<SiteId> },
    /// A `@linear` binding reached an exit without being consumed via
    /// a declared consuming method. Symmetric to `UseAfterConsume`.
    /// Surfaced from `MirCheck::MustConsume`.
    MustConsume {
        binding: BindingId,
        name: String,
        exit_site: SiteId,
        ty: ResolvedTy,
    },
    /// D10: a named user type had no known `ValueClass` at the MIR boundary.
    /// Only builtin types are supported in slice 1.
    UnknownType { name: String },
    /// W3.029: a declared user record/type aggregate reached the MIR
    /// decision boundary, but its field closure is not entirely `BitCopy`.
    UnsupportedUserRecordValueClass { name: String, reason: String },
    /// Defense-in-depth: an `HirExprKind::Unsupported` node reached MIR
    /// lowering.  The HIR diagnostic should have stopped the pipeline earlier.
    UnsupportedNode { reason: String },
    /// A `select{}` arm of a kind the current lane does not lower
    /// (today: `StreamNext` and `TaskAwait`). Distinct from
    /// `UnsupportedNode` so the diagnostic names both the arm kind and
    /// the future lane that will close it; the producer-bridge contract
    /// for MIR's `Terminator::Select` is "only `ActorAsk` + `AfterTimer`
    /// arms emit; everything else fails closed with a named pointer to
    /// the lane that lifts the restriction."
    SelectArmNotImplemented {
        arm_kind: String,
        deferred_by: String,
        site: SiteId,
    },
    /// Cluster 1 spine subset rejection: an expression form (e.g. a call, a
    /// non-integer literal, a control-flow construct) is recognised but not
    /// yet lowered to the backend `Instr` stream. Fail-closed so the emitter
    /// never sees a function body with an uninitialised return slot.
    NotYetImplemented { construct: String, site: SiteId },
    /// A `BindingRef` could not be resolved to a backend `Place` (typically
    /// a function parameter — Cluster 1's spine does not yet bind incoming
    /// arguments to local slots). Without a Place, the value cannot be
    /// moved into the return slot, so the function would silently emit a
    /// binary with an uninitialised return.
    UnresolvedPlace {
        binding: BindingId,
        name: String,
        site: SiteId,
    },
    /// A HIR-declared closure/lambda-actor capture could not be mapped to a MIR
    /// backend place. Capture analysis is checker/HIR authority; MIR must not
    /// silently drop a capture and emit a smaller environment.
    CannotMaterializeClosureCapture {
        binding: BindingId,
        name: String,
        site: SiteId,
    },
    /// Drop-elaboration aborted because the M2 substrate's per-exit
    /// drop plan could not be determined for a `Return` block. Surfaced
    /// from `MirCheck::DropPlanUndetermined`; the elaborator never
    /// emits a partial drop (fail-closed per LESSONS
    /// `cleanup-all-exits` / `boundary-fail-closed`).
    DropPlanUndetermined { block: u32, reason: String },
    /// Execution-context carrier marker validation failed.
    ContextBoundaryViolation {
        function: String,
        block: u32,
        kind: &'static str,
        reason: String,
    },
    /// A context-derived place escaped past `ExitContext`.
    ContextBindingEscapes { place: Place, block: u32 },
    /// A hand-built or malformed HIR actor body referenced `self.<field>` for a
    /// field that is not declared in the actor's state layout.
    UnknownActorStateField { actor: String, field: String },
    /// Two actor receive handlers, or a handler and an existing function symbol,
    /// resolved to the same emitted MIR symbol.
    ActorHandlerSymbolCollision {
        symbol: String,
        existing: String,
        duplicate: String,
    },
    /// W2.002 Stage 1: per-actor state-field clone classification could
    /// not place one of the state fields into the closed
    /// `StateFieldCloneKind` variant set. Carries the actor name, the
    /// 0-based field index, the field name (for diagnostic locality),
    /// and the underlying `ClassificationError`'s `Display` rendering.
    ///
    /// Surfaced from `classify_actor_state_fields` during
    /// `lower_hir_module`. Stage 2 codegen MUST NOT emit clone/drop
    /// registration for any actor whose `state_clone_fn_symbol` is
    /// `None`; that field is set to `None` whenever this diagnostic
    /// fires, so the two states track together (paired absence —
    /// substrate-first per dispatch-invariant #1).
    ActorStateCloneClassificationFailed {
        actor: String,
        field_index: usize,
        field_name: String,
        reason: String,
    },
    /// W3.022 V10: A `CallTraitMethodStatic` reached MIR with no concrete
    /// substitution for its `receiver_type_param`. After Stage 3 (impl-level
    /// type params flow into `HirFn::type_params`), unspecialised generic
    /// origins are skipped at the module-emission level (`lower_hir_module`
    /// continues past any `HirFn` with non-empty `type_params`), so any
    /// emitted MIR function reaching this path indicates a checker/HIR
    /// invariant violation: the call survived into a *concrete* function
    /// body without the substitution map carrying the type-param binding.
    /// Fail-closed per `boundary-fail-closed` / `td-debt-not-runtime-surprise`.
    UnresolvedStaticDispatchSubstitution {
        receiver_type_param: String,
        declaring_trait: String,
        method_name: String,
        site: SiteId,
    },
    /// W3.022: structured static-dispatch lookup failed. The substitution
    /// resolved the receiver to a concrete type, but the `(declaring_trait,
    /// self_type_name, method_name)` triple is absent from the
    /// `hew_hir::dispatch::build_trait_impl_method_index` registry built
    /// from `HirItem::Impl` metadata. Indicates either a checker bug
    /// (admitted a static dispatch with no matching impl) or HIR-lowering
    /// drift between impl-block lowering and the registry builder.
    /// Fail-closed; never reconstruct the impl symbol from a display name.
    StaticDispatchImplNotFound {
        declaring_trait: String,
        self_type_name: String,
        method_name: String,
        site: SiteId,
    },
    /// W3.022: structured static-dispatch lookup resolved to a generic
    /// impl method (`impl_type_params` non-empty), but the corresponding
    /// monomorphisation (mangled symbol) was not registered in
    /// `module_fn_names`. Indicates that HIR's `closure_under_substitution`
    /// failed to enqueue the impl-method `MonoKey` for this concrete
    /// instantiation. Fail-closed.
    StaticDispatchMonomorphisationMissing {
        method_symbol: String,
        mangled: String,
        site: SiteId,
    },
    /// W3.031 Stage 1: a `let`-binding whose resolved type is
    /// `ResolvedTy::TraitObject` reached MIR lowering but the producer
    /// could not classify its `TraitObjectStorage` (`FrameOwned` /
    /// `HeapBoxed`) from the RHS expression's shape. The MIR builder
    /// emits this diagnostic instead of fabricating a default storage
    /// — drop elaboration would otherwise pick the wrong release
    /// ritual (`FrameOwned` skips `hew_dyn_box_free`; `HeapBoxed` runs it).
    /// Fail-closed per `boundary-fail-closed` / `cleanup-all-exits`.
    ///
    /// `reason` names the unrecognised RHS expression shape (e.g.
    /// `"HirExprKind::Match"`) so a future stage adding the
    /// classification can locate the gap. The binding is not added
    /// to `owned_locals`, so no drop is elaborated for it; the
    /// diagnostic propagates upward and the pipeline aborts at the
    /// MIR boundary.
    TraitObjectStorageUndetermined {
        binding: BindingId,
        name: String,
        site: SiteId,
        reason: String,
    },
    /// W3.031 Stage 1.6: an `HirExprKind::CallDynMethod` reached MIR
    /// carrying a `signature` whose substitution is incomplete — i.e.
    /// the caller-side `FnSig` still contains an unresolved
    /// `Ty::Var`, a `Ty::Error`, or an unresolved `Ty::AssocType`
    /// (the trait-object bound was missing an assoc binding for one
    /// of the projected associated types). Codegen (Stage 7) consumes
    /// `signature` verbatim to derive the erased indirect-call type;
    /// admitting an unresolved entry would defer the failure to LLVM
    /// type construction where the cause is lost.
    ///
    /// The MIR producer fails closed with this diagnostic rather than
    /// fabricating a default signature, so the checker / HIR boundary
    /// that produced the partial substitution is named in `reason`.
    /// (LESSONS: `checker-output-boundary` P0, `boundary-fail-closed`
    /// P0, copilot-instructions §3 Type Inference Boundary — no
    /// `Ty::Var` survives unresolved into codegen.)
    CallTraitMethodSignatureUnresolved {
        trait_name: String,
        method_name: String,
        site: SiteId,
        reason: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecisionFact {
    pub site: SiteId,
    /// The resolved type of the expression at this decision site.
    pub ty: ResolvedTy,
    pub value_class: ValueClass,
    pub intent: IntentKind,
    pub strategy: Strategy,
    pub why: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Strategy {
    BorrowRead,
    Move,
    CowShare,
    EnsureUnique,
    Materialize,
    ConsumeCall,
    Freeze,
    UnknownBlocked,
}
