use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use hew_hir::{sanitize_for_symbol, BindingId, IntentKind, ItemId, SiteId, ValueClass};
use hew_types::{NumericWidth, ResolvedTy, TryConversionKind, WireCodecDirection, WireLayoutTable};

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

/// Target pointer width threaded into MIR lowering so the `isize`/`usize`
/// trap-guard constants (signed-MIN/-1 for `/` `%`, shift-out-of-range bound
/// for `<<` `>>`) are emitted at the width the target actually uses.
///
/// The width MUST be derived from the compile target (`TargetArch`), never from
/// a host `cfg!(target_pointer_width)`: a cross-compile (e.g. `--target wasm32`
/// on a 64-bit host) would otherwise emit width-64 guards into a 32-bit module,
/// silently admitting out-of-range shifts and the wrong signed-MIN trap — a
/// fail-open soundness hole. `Bits64` is the default because every native
/// target Hew supports (`X86_64`/`Aarch64`/`Other`) is 64-bit; only `Wasm32`
/// is 32-bit. The host-defaulting `lower_hir_module` test wrapper keeps `Bits64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PointerWidth {
    /// 32-bit pointers (wasm32): `isize` == `i32` range, `usize` == `u32` range.
    Bits32,
    /// 64-bit pointers (every native target): `isize` == `i64`, `usize` == `u64`.
    #[default]
    Bits64,
}

impl PointerWidth {
    /// The pointer width in bits — the shift-out-of-range bound and the LLVM
    /// integer width codegen emits for `isize`/`usize`.
    #[must_use]
    pub fn bits(self) -> i64 {
        match self {
            PointerWidth::Bits32 => 32,
            PointerWidth::Bits64 => 64,
        }
    }

    /// The signed minimum value for `isize` at this width, as the `i64` constant
    /// the signed-MIN/-1 div/rem trap compares against.
    #[must_use]
    pub fn isize_min(self) -> i64 {
        match self {
            PointerWidth::Bits32 => i64::from(i32::MIN),
            PointerWidth::Bits64 => i64::MIN,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct IrPipeline {
    pub thir: Vec<ThirFunction>,
    pub raw_mir: Vec<RawMirFunction>,
    pub checked_mir: Vec<CheckedMirFunction>,
    pub elaborated_mir: Vec<ElaboratedMirFunction>,
    pub diagnostics: Vec<MirDiagnostic>,
    /// Checker-authored wire layout metadata forwarded from HIR.
    pub wire_layouts: Arc<WireLayoutTable>,
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
    /// Canonical record names that appear as the receiver of a user-authored
    /// `.clone()` call in this module. Populated from
    /// `TypeCheckOutput::user_clone_record_seeds` via `attach_lowering_facts`.
    ///
    /// Codegen's `collect_reachable_clone_targets` consumes this as an
    /// additional seed so that every record cloned by user code has a
    /// `__hew_record_clone_inplace_<R>` / `__hew_record_drop_inplace_<R>`
    /// thunk pair synthesised, even for records not reachable via actor state
    /// or `Vec<R>` ownership seeds.
    ///
    /// WHY here and not in codegen directly: `IrPipeline` is the
    /// checker→codegen boundary; codegen consumes pipeline fields, never
    /// `TypeCheckOutput` directly. WHEN-OBSOLETE: if user-facing `Clone`
    /// becomes a first-class trait with a dedicated codegen path.
    pub user_clone_record_seeds: Vec<String>,
    /// Warning-severity findings from the MIR-stage lint pass (`liveness.rs`):
    /// `dead_store` today. Kept on a channel separate from [`Self::diagnostics`]
    /// because every `MirDiagnostic` renders as a hard `E_MIR_*` error, whereas
    /// these are level-configurable lints (`--allow`/`--warn`/`--deny`). The MIR
    /// stage records the raw findings here; the CLI applies the user's
    /// [`hew_types::LintLevels`] and `// hew:allow(...)` policy at render time.
    ///
    /// Surfaced through the CLI front end only; the LSP / wasm front ends stop
    /// at HIR and never lower to MIR, so they never populate or read this.
    /// Editor/web surfacing is tracked in issue #2176.
    pub lint_warnings: Vec<MirLint>,
    /// Field-bearing `#[resource]` records that own at least one heap/aggregate
    /// field and therefore reach codegen through the recursive
    /// `__hew_record_drop_inplace_<R>` thunk rather than the single-handle
    /// `AffineResource` close path. Each entry pairs the record's canonical
    /// layout name (`record_inplace_drop_name` key) with the mangled
    /// `<Type>::close` symbol the user `impl` declares.
    ///
    /// Codegen's record-drop thunk synthesis consumes this so a `#[resource]`
    /// record's user `close(self)` runs as the FIRST step of its scope-exit /
    /// nested-field drop (spec §3.7.3 / §10(d)): the resource's own close fires
    /// before the field-wise teardown, and a nested `#[resource]` field's close
    /// fires through the same recursive thunk. Without this seed the field-wise
    /// `RecordInPlace` drop would free heap leaves but silently skip the RAII
    /// `close()` contract.
    ///
    /// Populated by `lower_hir_module` from `HirModule::type_classes` for every
    /// `#[resource]`-marked type that ALSO has a record layout. A single-handle
    /// `#[resource]` (no owned/aggregate field) is NOT listed: it routes to the
    /// `AffineResource` close path and never reaches the record-drop thunk.
    ///
    /// WHY here and not in codegen directly: `IrPipeline` is the checker→codegen
    /// boundary; codegen consumes pipeline fields, never the HIR `type_classes`
    /// table. Mirrors `opaque_handle_names` / `user_clone_record_seeds`.
    pub resource_record_close: Vec<(String, String)>,
    /// RAII-1 opaque-resource close registry — `(opaque_type, "<Type>::<close>")`
    /// for every single-slot `#[resource] #[opaque]` handle (see
    /// `resource_opaque_close_registry`). The COMPLEMENT of
    /// `resource_record_close`: a single-handle opaque `#[resource]` has no
    /// record layout, so it is excluded from `resource_record_close` — that
    /// exclusion is exactly the W3.029 leak this closes.
    ///
    /// Codegen's record-drop thunk synthesis (`collect_reachable_clone_targets`
    /// and the on-demand `emit_aggregate_recursive_drop` named-leaf) consumes
    /// this so a resource handle embedded in an owned record classifies as
    /// `StateFieldCloneKind::Resource` and the owning record's drop spine runs
    /// the handle's `close(self)` exactly once on every exit path. It MUST match
    /// the registry the MIR admission gate used (both built by
    /// `resource_opaque_close_registry` from the same inputs) or admission and
    /// drop-body synthesis would disagree.
    pub resource_opaque_close: Vec<(String, String)>,
}

/// A single warning-severity finding from the MIR-stage lint pass.
///
/// Level-agnostic: `hew-mir` records the finding (which lint fired, where, and
/// the human message); the CLI maps it through [`hew_types::LintLevels`] to a
/// rendered warning, a build-failing error (`--deny`), or nothing (`--allow` /
/// `// hew:allow(...)`). The `span` is a `(start, end)` byte range into the
/// originating module source so the CLI can point at the offending line and run
/// the same suppression-directive check used for HIR-stage lints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirLint {
    /// Which registry lint produced this finding.
    pub lint: hew_types::LintId,
    /// `(start, end)` byte offsets of the offending construct in module source.
    pub span: (u32, u32),
    /// The rendered human-readable message (no severity prefix; the CLI adds
    /// `warning:` / `error:`).
    pub message: String,
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
        self.user_clone_record_seeds
            .clone_from(&tco.user_clone_record_seeds);
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
#[derive(Debug, Clone, PartialEq)]
pub enum MirConstValue {
    /// Folded integer value. The declared width lives in [`MirConst::ty`];
    /// codegen truncates/zero-extends the `i64` payload to that width.
    Integer(i64),
    /// Folded UTF-8 string literal value.
    Str(String),
    /// Folded float literal value. The declared width lives in [`MirConst::ty`].
    Float(f64),
}

/// Module-level constant descriptor lowered from `hew_hir::HirItem::Const`.
///
/// Mirrors the regex-literal handle-array pattern: one entry per module-level
/// `const`, in declaration order, with `const_id` as the 0-based index into
/// the const-descriptor table (the codegen global-slot index). `item_id` ties
/// the descriptor back to the `ResolvedRef::Const(item_id)` that const
/// references carry; the codegen global-load seam (wired in a4656a25) resolves
/// a reference to its slot without re-reading HIR.
#[derive(Debug, Clone, PartialEq)]
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
    /// Field names in declaration order, parallel to `field_tys`
    /// (`field_names[i]` is the source name of `FieldOffset(i)`). Threaded
    /// from the HIR record field declarations at the MIR projection. Codegen
    /// consumes this under `hew build -g` to emit named `DW_TAG_member` DIEs
    /// so gdb prints `pt.x`, not `pt.<field0>`. Empty for layouts built
    /// without names (hand-built test fixtures); codegen falls back to a
    /// positional `field_N` name in that case rather than failing.
    pub field_names: Vec<String>,
}

/// Layout descriptor for an `actor` declaration. The state field list follows
/// declaration order; the init parameter list follows source parameter order.
#[derive(Debug, Clone, PartialEq)]
pub struct ActorLayout {
    /// Actor type name.
    pub name: String,
    /// Defining-module identity carried verbatim from
    /// `HirActorDecl::defining_module`. `None` for root-program actors
    /// (including file-import spliced ones); `Some(module_short)` for actors
    /// exported by a package module. This is the discriminator that makes
    /// qualified layout keys and qualified symbol mangling possible — layout
    /// keys and all `mangle_actor_*` symbols still derive from the bare
    /// `name` until the qualified re-key lands on top of this carrier.
    pub defining_module: Option<String>,
    /// Actor state field names in declaration order.
    pub state_field_names: Vec<String>,
    /// Actor state field types in declaration order.
    pub state_field_tys: Vec<ResolvedTy>,
    /// Actor state field defaults in declaration order.
    pub state_field_defaults: Vec<Option<hew_hir::HirExpr>>,
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
    /// `#[on(exit)]` linked-actor exit-hook symbol (M-7-R). `None` when the
    /// actor has no exit hook. When `Some`, the dispatch trampoline routes
    /// `SYS_MSG_EXIT` (103) to it, unpacking the delivered `CrashNotification`
    /// (`actor_id`, `crash_kind` tag) from the `ExitMessage` payload.
    pub on_exit_symbol: Option<String>,
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
    /// `mailbox <N>;` fixed mailbox capacity, mirrored verbatim from
    /// `HirActorDecl::mailbox_capacity`. `None` means the runtime default
    /// (unbounded) applies. Codegen threads `Some(N)` through
    /// `hew_actor_spawn_opts`'s `mailbox_capacity` field so the runtime's
    /// existing `if opts.mailbox_capacity > 0` bounding path actually
    /// engages — previously this was silently dropped between HIR and
    /// codegen, leaving every declared-bounded mailbox unbounded.
    pub mailbox_capacity: Option<u32>,
    /// `overflow <policy>;` mailbox overflow policy, mirrored verbatim from
    /// `HirActorDecl::overflow_policy`. `None` with `Some(mailbox_capacity)`
    /// means the spec §6.2 default (`Block`) applies. Codegen maps this
    /// enum to the runtime's `HewOverflowPolicy` i32 encoding BY NAME (the
    /// two enums have different discriminant orders — an ordinal cast is
    /// wrong-code). `Coalesce { .. }` fails closed at MIR lowering with a
    /// `MailboxOverflowCoalesceNotYetImplemented` diagnostic rather than
    /// silently threading a policy the codegen key-function ABI cannot
    /// honour yet.
    pub overflow_policy: Option<hew_parser::ast::OverflowPolicy>,
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
    ///
    /// JUSTIFIED string survivor: this carries a per-actor GENERATED
    /// object symbol (`__hew_state_drop_<Actor>`) — an open set minted
    /// by codegen itself, structurally incapable of a closed-descriptor
    /// representation. The string IS the linker-edge name by nature;
    /// nothing dispatches on its content (the runtime receives the
    /// resolved function pointer).
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
    /// `#[every(duration)]` periodic self-send interval in milliseconds.
    ///
    /// `None` for ordinary message-driven handlers. `Some(ms)` when the
    /// HIR handler carried `every_ns`; the ns→ms conversion (truncating
    /// divide) happens once, at `lower_actor_handler_layouts`, because the
    /// runtime ABI (`hew_actor_schedule_periodic`) is millisecond-grained.
    /// The checker rejects intervals that floor to 0 ms, so a populated
    /// value is ≥ 1 for checked programs; if an unchecked input slips a 0
    /// through, the runtime refuses to arm (null handle) and codegen's
    /// spawn-site null check traps — fail-closed at every layer.
    ///
    /// Codegen consumes this at each `Instr::SpawnActor` site to emit one
    /// `hew_actor_schedule_periodic(actor, msg_type, every_ms)` call per
    /// periodic handler, using this row's `msg_type` (the same
    /// protocol-descriptor id the send path uses).
    pub every_ms: Option<u64>,
    /// `true` for a `receive gen fn` handler — the third handler kind beside
    /// tell (`Fire`) and ask (`Ask`): a per-call, channel-backed `Stream<T>`
    /// whose producer is a coro generator driven inside the actor (a PUMP).
    ///
    /// `param_tys` for such a row is the handler's own params PLUS one
    /// trailing pointer-word sink — the single pack/unpack authority for the
    /// tell-shaped "start" message both the dispatch trampoline (unpack) and
    /// the call-site stream construction (pack) read. `return_ty` is `Unit`
    /// (the pump's actual MIR return type, since it never replies) — the
    /// stream element type lives on the checker's
    /// `ActorMethodKind::StreamProducer` fact, not this row.
    ///
    /// `false` for every ordinary tell/ask handler — set explicitly at every
    /// construction site rather than left to a `Default`, so a future new row
    /// producer cannot forget the discriminant (`type-info-survival`: MIR/
    /// codegen consume this flag rather than re-deriving stream-producer-ness
    /// from `is_generator` on a different struct or from `return_ty` shape).
    pub is_stream_producer: bool,
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
    /// Construction-time config param (`supervisor App(config: T)`), when the
    /// supervisor declares one. `None` for a no-config supervisor (the common
    /// case). Codegen reads the param name + config struct type to:
    /// 1. give the bootstrap fn a leading config-pointer parameter,
    /// 2. malloc the supervisor-owned config buffer (size from the config
    ///    struct's `record_layout`) and memcpy the caller's config value in,
    /// 3. pass the buffer to each config-init child's init thunk +
    ///    `hew_supervisor_set_child_init_fn`.
    ///
    /// `Some` only when the declaration has a `(...)` clause; supervisors with
    /// config params but no child that reads `config.field` still carry it so
    /// codegen knows the bootstrap signature must take (and the spawn site must
    /// pass) the config value.
    pub config_param: Option<SupervisorConfigParam>,
}

/// The construction-time config parameter on a `SupervisorLayout`.
///
/// `supervisor App(config: AppConfig)` produces `SupervisorConfigParam {
/// name: "config", config_ty_name: "AppConfig" }`. Codegen looks up
/// `config_ty_name` in `record_layouts` to size the config buffer and to GEP
/// individual config fields inside each child init thunk.
#[derive(Debug, Clone, PartialEq)]
pub struct SupervisorConfigParam {
    /// The config param binding name (e.g. `config`). Matches
    /// `ChildInitArg::ConfigField.config_param_name`.
    pub name: String,
    /// The config struct's type name (e.g. `AppConfig`) for `record_layouts`.
    pub config_ty_name: String,
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
    /// `restart: <policy>` clause. `None` when the child declaration
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
    /// Mangled symbol of the per-actor lifecycle wrapper
    /// (`__hew_lifecycle_<mangled-actor>`) that codegen emits to run the
    /// child's `init()` / `#[on(start)]` hooks under supervision. `Some`
    /// when the child's actor declares an `init` or a start hook; `None`
    /// when it declares neither (the supervised spawn then fires no
    /// lifecycle wrapper, matching direct-spawn's no-hook case).
    ///
    /// The symbol is derived from the actor name (codegen mangles it), so
    /// this carries the *intent* (the actor has hooks to run) rather than a
    /// codegen-minted object symbol. Codegen populates the `lifecycle_fn`
    /// pointer in the emitted `HewChildSpec` literal from this field; if
    /// `None`, the field is left null and the runtime skips the call.
    ///
    /// Mirrors `on_crash_symbol`: the fn-ptr rides IN the `HewChildSpec`
    /// literal (read during `hew_supervisor_add_child_spec`), which is the
    /// load-bearing carrier for the INITIAL supervised spawn — the spawn
    /// fires inside `add_child_spec`, before any post-hoc setter runs.
    pub lifecycle_symbol: Option<String>,
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
    /// Mirrored from `ActorLayout.mailbox_capacity` in the post-loop pass.
    /// Codegen populates `HewChildSpec.mailbox_capacity` from this field so
    /// supervised actors are bounded identically to direct-spawned ones —
    /// without this mirror every supervised actor stays unbounded across
    /// restarts (the "second zero-hardcode site" fixed alongside the
    /// direct-spawn opts path).
    pub mailbox_capacity: Option<u32>,
    /// Mirrored from `ActorLayout.overflow_policy` in the post-loop pass.
    /// Codegen maps this to `HewChildSpec.overflow` via the same by-name
    /// helper used for the direct-spawn opts path — never an ordinal cast.
    pub overflow_policy: Option<hew_parser::ast::OverflowPolicy>,
    /// Per-field literal init args for this child's actor state template.
    ///
    /// Each entry is `(field_name, value)` in actor-state-field declaration order.
    /// Empty means no `(...)` clause on the child declaration; codegen emits
    /// `init_state = NULL` only when the actor's `state_field_names` is also empty
    /// (stateless actor). For a stateful actor with an empty `init_state_fields`,
    /// codegen emits `CodegenError::FailClosed` rather than a null template.
    ///
    /// Populated in the post-loop pass in `lower_hir_module` after the
    /// actor-layout map is available, so declaration order between supervisor
    /// and actor is irrelevant.
    ///
    /// SHIM: first slice supports POD (i64/i32/bool/f64) state only.
    /// WHY: owned-heap fields (String/Vec) require verified semantic clone via
    ///   `state_clone_fn`; byte-copy clone is correct only for plain-old-data.
    /// WHEN obsolete: follow-up slice after clone verification is proven.
    /// WHAT: extend `ChildInitArg` and verify `state_clone_fn` covers owned types.
    pub init_state_fields: Vec<(String, ChildInitArg)>,
    /// `Some(bootstrap_symbol)` when this child's declared type is itself a
    /// supervisor (`child api: AuthSupervisor;`) rather than an actor; `None`
    /// for the common actor-child case.
    ///
    /// A nested-supervisor child is registered with the parent through a
    /// different runtime seam than an actor child: codegen calls the child's
    /// own bootstrap function (which builds and starts the child supervisor and
    /// returns its `*mut HewSupervisor`), then registers it via
    /// `hew_supervisor_add_child_supervisor_with_init`, passing the same
    /// bootstrap symbol as the restart `init_fn`. It must NOT go through the
    /// `HewChildSpec` / `__hew_actor_dispatch_<actor>` path — a supervisor has
    /// no actor dispatch trampoline, so that path fails closed at codegen.
    ///
    /// Populated in the post-loop pass in `lower_hir_module`, where the full
    /// set of declared supervisor names is known, so declaration order between
    /// the parent and the nested supervisor is irrelevant.
    pub nested_bootstrap_symbol: Option<String>,
    /// The static pool size for a pool child (`is_pool = true`), lowered from
    /// the reserved `count:` arg. `None` for a static child or a pool child
    /// whose count failed to lower (the checker already rejected a missing
    /// count). Codegen reads this to emit the member-spawn loop: each member is
    /// registered as a static child and bound into the pool via
    /// `hew_supervisor_pool_member_add_static`.
    pub pool_count: Option<PoolCount>,
}

impl SupervisorChildLayout {
    /// Whether this child occupies a slot in the runtime's STATIC actor-child
    /// table (`HewSupervisor.children[]`) and therefore advances the partitioned
    /// static-slot index.
    ///
    /// This is the SINGLE shared truth tying the two iterations that walk
    /// `SupervisorLayout.children`: MIR `partitioned_static_slot_index` (the
    /// accessor's slot lookup) and the codegen bootstrap registration loop. Both
    /// MUST agree, or a static `sup.child` accessor declared after a pool or a
    /// nested supervisor mis-routes to the wrong runtime slot.
    ///
    /// A child advances the static index iff it is NEITHER a nested supervisor
    /// (those live in `child_supervisors[]`, a disjoint table) NOR a pool (those
    /// live in `pool_slots[]`, also disjoint — the pool axis the B1 spine left
    /// latent). Nested children and pool children each occupy their own 0-based
    /// space; only plain actor children occupy `children[]`.
    #[must_use]
    pub fn occupies_static_child_slot(&self) -> bool {
        !self.is_pool && self.nested_bootstrap_symbol.is_none()
    }
}

/// The size of a static supervisor pool (`pool name: Type(count: N)`).
///
/// Carried on [`SupervisorChildLayout::pool_count`] so codegen can emit the
/// member-spawn loop. A literal count is a compile-time constant N; a
/// config-derived count is loaded from the supervisor's construction-time
/// config buffer inside the bootstrap (mirroring [`ChildInitArg::ConfigField`]),
/// and the bootstrap traps fail-closed on `N <= 0` at runtime.
#[derive(Debug, Clone, PartialEq)]
pub enum PoolCount {
    /// A compile-time integer literal pool size. The checker has proven it is
    /// positive, so codegen spawns exactly this many members unconditionally.
    Literal(i64),
    /// A pool size read from the supervisor config (`count: config.workers`).
    /// Codegen loads `config.<field_name>` (an integer field) and emits a real
    /// `0..N` loop, trapping fail-closed when the loaded value is `<= 0`.
    ConfigField {
        /// The supervisor config param binding name (e.g. `config`).
        config_param_name: String,
        /// The config struct's type name (e.g. `AppConfig`) for `record_layouts`.
        config_ty_name: String,
        /// The integer field read out of the config struct (e.g. `workers`).
        field_name: String,
        /// The resolved type of the config field (the integer load width).
        field_ty: ResolvedTy,
    },
}

/// A self-contained literal value for a supervisor child init arg.
///
/// Kept separate from MIR instructions so codegen can read these directly
/// without a running `FnCtx`. Covers every scalar `BitCopy` primitive width
/// (signed/unsigned 8/16/32/64-bit integers, `bool`, `f64`). Each width gets
/// its own variant so codegen materialises a value of exactly the field's
/// allocated width — a wider store would clobber the adjacent field and write
/// past the end of the state template.
#[derive(Debug, Clone, PartialEq)]
pub enum ChildInitArg {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Bool(bool),
    F64(f64),
    /// A non-literal init value read from the supervisor's construction-time
    /// config inside the codegen-emitted init thunk — the v0.6 init-closure
    /// restart model. The thunk loads `config.<field_name>` (a load for a
    /// scalar `BitCopy` field; a per-type owned deep-clone for `string`/`Vec`)
    /// and stores it into the fresh actor state. Re-run on every restart, this
    /// produces fresh, unaliased owned values per incarnation.
    ///
    /// Carries codegen-emittable data (no MIR `FnCtx`): the config param's type
    /// name (to look up its `record_layout`), the config field name + the
    /// resolved field type (to pick the load width / clone path), and the
    /// `owned` flag (true → emit a deep-clone, false → a plain load).
    ConfigField {
        /// The supervisor config param binding name (e.g. `config`) — matches
        /// the bootstrap fn's config parameter.
        config_param_name: String,
        /// The config struct's type name (e.g. `AppConfig`) for `record_layouts`.
        config_ty_name: String,
        /// The field read out of the config struct (e.g. `cache_size`).
        field_name: String,
        /// The resolved type of the config field, choosing the load width
        /// (scalar) or the clone path (owned).
        field_ty: ResolvedTy,
        /// `true` when the field is owned (`string`/`Vec`) and the thunk must
        /// deep-clone it into the fresh state; `false` for a `BitCopy` scalar
        /// loaded directly.
        owned: bool,
    },
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
    /// Payload field names in declaration order, parallel to `field_tys`.
    /// Threaded from the HIR variant payload field declarations at the MIR
    /// projection so codegen's `-g` enum DIEs name each variant's payload
    /// members (`shape.payload.Circle.radius`). Empty for unit variants and
    /// for hand-built test fixtures; codegen falls back to positional
    /// `field_N` names when a name is absent.
    pub field_names: Vec<String>,
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
    /// True for `indirect enum` declarations. Every variable of this type holds
    /// a `ptr` to a heap-allocated tagged-union struct rather than an inline
    /// struct value. Codegen routes all `Place::EnumTag` / `Place::EnumVariant`
    /// accesses through a pointer load and emits `hew_alloc` on construction
    /// plus `hew_dealloc` on drop. Propagated from `HirTypeDecl::is_indirect`.
    pub is_indirect: bool,
}

/// Project a machine layout into the enum-layout view.
///
/// A machine is an enum at the value-classification layer: both share the
/// tagged-union substrate (`{ tag: iW, payload: [N x i8] }`) and the
/// `MachineVariantLayout` per-variant shape; a machine differs only in
/// carrying the event-companion list, which has no meaning for value
/// classification, cloning, or dropping. Projecting through this view lets
/// the actor-state classifier and the clone/drop thunk synthesis treat a
/// machine-typed value exactly like an enum-typed one — same admissions,
/// same refusals, same `__hew_enum_{clone,drop}_inplace_*` thunk family
/// (codegen's `machine_layout_map` already registers machines by name, so
/// the synthesis finds the tagged-union layout under the same key).
///
/// Machines are never `indirect` (no self-recursive states), so the view's
/// `is_indirect` is always `false`.
#[must_use]
pub fn machine_enum_view(layout: &MachineLayout) -> EnumLayout {
    EnumLayout {
        name: layout.name.clone(),
        tag_width: layout.tag_width,
        variants: layout.variants.clone(),
        is_indirect: false,
    }
}

/// [`machine_enum_view`] over a whole machine-layout list.
#[must_use]
pub fn machine_enum_views(machine_layouts: &[MachineLayout]) -> Vec<EnumLayout> {
    machine_layouts.iter().map(machine_enum_view).collect()
}

/// Unqualified tail of a possibly module-qualified type name
/// (`"mod.Expr"` → `"Expr"`). The single short-name authority shared by
/// the MIR drop elaborator and the codegen layout lookup.
#[must_use]
pub fn short_name(name: &str) -> &str {
    name.rsplit_once('.').map_or(name, |(_, short)| short)
}

/// THE single enum-layout lookup authority for this module.
///
/// Resolve a `Named { name, args }` to its registered [`EnumLayout`]. Every
/// generic instantiation is registered under a mangle of the BARE outer name
/// and the SHORTENED type-arg spine (`EnumLayoutRegistry::insert`, the
/// `layout_mono` pass, and the MIR `mangle_layout_key` all route their
/// `mangled_name` through `hew_hir::shorten_named_arg_qualifiers`). So a probe
/// keyed from a use-site type carrying a module-qualified payload
/// (`Slot<lmonobox.Box>` → naive `Slot$$lmonobox.Box`) diverges from the
/// registered `Slot$$Box` and the lookup falls through the fail-closed gate.
///
/// Routing EVERY enum-layout probe in this module through here makes that miss
/// structurally impossible: the function shortens `args` internally before
/// mangling, so a caller CANNOT pass a raw qualified spine into the key. Nested
/// qualified payloads (`Result<Vec<json.Value>, _>`) are shortened at every
/// depth. The empty-args arm keeps the existing bare-or-short-name match.
///
/// No call site in this module may build a generic enum-layout key with a raw
/// `mangle(.., args)`; the `mangle_feeding_layout_lookup_is_centralised` guard
/// test fails if one reappears.
pub(crate) fn find_enum_layout<'a>(
    name: &str,
    args: &[ResolvedTy],
    enum_layouts: &'a [EnumLayout],
) -> Option<&'a EnumLayout> {
    let short = short_name(name);
    if args.is_empty() {
        enum_layouts
            .iter()
            .find(|el| el.name == name || short_name(&el.name) == short)
    } else {
        let short_args: Vec<ResolvedTy> = args
            .iter()
            .cloned()
            .map(hew_hir::shorten_named_arg_qualifiers)
            .collect();
        let mangled = hew_hir::mangle(short, &short_args);
        enum_layouts
            .iter()
            .find(|el| el.name == mangled || el.name == name)
    }
}

/// THE single record-registry key authority for this module.
///
/// Builds the candidate lookup keys a `Named { name, args }` resolves to in any
/// record registry keyed by `hew_hir::mangle` (`record_layouts` keyed by
/// `RecordLayout::name`, and the MIR `record_field_orders` map — both populated
/// from the same HIR `mangled_name`). The empty-args arm yields the bare name +
/// short name; the generic arm yields the full + short mangle of the SHORTENED
/// type-arg spine (`Holder<i64>` → `Holder$$i64`, `Slot<lmonobox.Box>` →
/// `Slot$$Box`). Shortening the args internally means a caller CANNOT pass a raw
/// module-qualified spine into the key (the C1 qualified-spine miss class).
///
/// Both record-side lookups — [`find_record_layout`] over `&[RecordLayout]` and
/// the [`MirHeapLayouts`] adapter over `record_field_orders` — route through
/// here so the record-key scheme is written once. The
/// `mangle_feeding_layout_lookup_is_centralised` guard fails if a bare
/// `mangle(.., args)` feeding a layout lookup reappears outside this and
/// [`find_enum_layout`].
fn record_lookup_keys(name: &str, args: &[ResolvedTy]) -> Vec<String> {
    let short = short_name(name);
    if args.is_empty() {
        let mut keys = vec![name.to_string()];
        if short != name {
            keys.push(short.to_string());
        }
        keys
    } else {
        let short_args: Vec<ResolvedTy> = args
            .iter()
            .cloned()
            .map(hew_hir::shorten_named_arg_qualifiers)
            .collect();
        vec![
            hew_hir::mangle(name, &short_args),
            hew_hir::mangle(short, &short_args),
            // Bare/short-name fallbacks for any registry that did not key the
            // instantiation by its mangle (defensive; the generic arm above is
            // the hot path).
            name.to_string(),
            short.to_string(),
        ]
    }
}

/// Mangle-aware record layout lookup — the companion of [`find_enum_layout`]
/// for the record side. Resolves both monomorphic records (bare-name or
/// short-name) and generic instantiations (`LocalPid<Socket>` →
/// `LocalPid$$Socket`) using the same mangling scheme as the HIR mono pass.
///
/// Mirrors `state_clone::lookup_record_layout` in authority; kept local to
/// `model.rs` so `ty_contains_unclonable_opaque_inner` can use it without
/// depending on the `state_clone` module.
pub(crate) fn find_record_layout<'a>(
    name: &str,
    args: &[ResolvedTy],
    record_layouts: &'a [RecordLayout],
) -> Option<&'a RecordLayout> {
    let keys = record_lookup_keys(name, args);
    record_layouts.iter().find(|r| keys.contains(&r.name))
}

/// Record-BLIND entry point to the heap-ownership authority [`ty_owns_heap`]:
/// answers over enum/machine variant payloads + tuple/array/type-arg recursion +
/// the builtin leaf set, but does NOT consult user record field types (no record
/// registry is available from an `[EnumLayout]` slice alone).
///
/// Kept for the call sites whose only layout context is `enum_layouts` and which
/// cannot transitively reach a heap-owning user record (the codegen
/// composite-return boundary, which gates on `record_layouts` membership FIRST
/// and only consults this for the already-confirmed-record case). Every site
/// that may reach a nested user record carrying a heap field instead routes
/// through [`ty_owns_heap`] with a record-aware adapter — see the MIR
/// [`MirHeapLayouts`] adapter — so a record-wrapped heap leaf is never missed
/// (DIV-1).
#[must_use]
pub fn ty_contains_heap_owning(ty: &ResolvedTy, enum_layouts: &[EnumLayout]) -> bool {
    ty_owns_heap(ty, &EnumLayoutsOnly(enum_layouts))
}

/// Record-aware [`HeapOwnershipLayouts`] adapter for the MIR layer.
///
/// Supplies record field types from the `Builder`'s `record_field_orders`
/// (name → ordered `(field_name, ty)` pairs) and enum/machine variant payloads
/// from the `Builder`'s `enum_layouts` (the `classification_enum_layouts` view,
/// which already folds machine state payloads in as enum views — so the MIR
/// layer needs no separate machine-layout axis). This is the adapter every MIR
/// drop-allow / move / double-free analysis uses so its "owns heap" verdict is
/// record-aware (DIV-1) and shares the one leaf set (DIV-2).
#[derive(Debug)]
pub struct MirHeapLayouts<'a, S = std::collections::hash_map::RandomState> {
    pub record_field_orders: &'a HashMap<String, Vec<(String, ResolvedTy)>, S>,
    pub enum_layouts: &'a [EnumLayout],
}

impl<S: std::hash::BuildHasher> HeapOwnershipLayouts for MirHeapLayouts<'_, S> {
    fn record_field_tys(&self, name: &str, args: &[ResolvedTy]) -> Option<Vec<ResolvedTy>> {
        // Route the record-field lookup through the single `record_lookup_keys`
        // authority — the same key scheme `find_record_layout` uses and the same
        // `$$`-mangled key `record_field_orders` is registered under (`lower.rs`
        // ← `mangle(name, type_args)`), matching codegen's `CgHeapLayouts` and
        // the deleted `named_elem_owns_heap`.
        //
        // The previous key (`record_or_enum_visit_key`, the `Name<arg,…>` form)
        // is the recursion VISIT key — correct for cycle-guarding distinct
        // instantiations (`Wrap<Wrap<i64>>` vs `Wrap<i64>`) but NOT a registry
        // key. Using it here returned `None` for every generic instantiation, so
        // the authority never saw a generic record's fields and silently
        // classified a generic record carrying a non-type-parameter heap field
        // (`Holder<i64>{ payload: Vec<i64> }`) as non-owning — while codegen
        // (correctly keyed) classified it owning, re-creating the MIR↔codegen
        // adapter divergence DIV-1 exists to eliminate.
        record_lookup_keys(name, args)
            .iter()
            .find_map(|key| self.record_field_orders.get(key))
            .map(|fields| fields.iter().map(|(_, ty)| ty.clone()).collect())
    }

    fn enum_variant_field_tys(
        &self,
        name: &str,
        args: &[ResolvedTy],
    ) -> Option<Vec<Vec<ResolvedTy>>> {
        find_enum_layout(name, args, self.enum_layouts).map(|layout| {
            layout
                .variants
                .iter()
                .map(|v| v.field_tys.clone())
                .collect()
        })
    }
}

/// Record-aware MIR convenience: `ty_owns_heap` over a [`MirHeapLayouts`] built
/// from the loose `record_field_orders` + `enum_layouts` borrows the MIR free
/// functions (the `derive_*` drop-allow derivations, `aggregate_ingress_*`,
/// `ty_is_heap_owning_tuple`) already hold. Closes DIV-1 at every former
/// record-blind call site without converting those free functions to methods.
#[must_use]
pub fn ty_owns_heap_mir<S: std::hash::BuildHasher>(
    ty: &ResolvedTy,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>, S>,
    enum_layouts: &[EnumLayout],
) -> bool {
    ty_owns_heap(
        ty,
        &MirHeapLayouts {
            record_field_orders,
            enum_layouts,
        },
    )
}

/// Layout-lookup input for the single heap-ownership authority [`ty_owns_heap`].
///
/// The MIR drop elaborator, the MIR move/double-free analyses, and the codegen
/// owned-Vec / composite-return boundaries each answer the SAME structural
/// question — "does this type transitively own heap memory?" — over the SAME
/// leaf set, but each holds its layout registries in a different shape: the MIR
/// `Builder` keys record fields by `record_field_orders` (name → ordered
/// `(field_name, ty)` pairs) and carries machine state payloads folded into its
/// `enum_layouts` (the `classification_enum_layouts` view); codegen's `FnCtx`
/// keys record fields by `record_field_resolved_tys` and carries machine state
/// payloads in a separate `machine_layouts` registry. This trait is the single
/// seam over those shapes so [`ty_owns_heap`] is written once and every consumer
/// routes through it — record-aware, enum-aware, machine-aware, with one leaf
/// set — closing the divergence class where the old parallel walkers
/// (`ty_contains_heap_owning`, `named_elem_owns_heap`,
/// `resolved_ty_contains_heap_leaf`) drifted: one consulted record fields and
/// another did not, one treated a `CancellationToken`/`Generator` as a heap
/// leaf and another did not (`dedup-semantic-boundary`).
pub trait HeapOwnershipLayouts {
    /// Field types of a registered user record, resolved from a
    /// `Named { name, args }` (substituted concrete types in declaration order).
    /// `None` when `name`/`args` resolve to no user record layout in scope.
    fn record_field_tys(&self, name: &str, args: &[ResolvedTy]) -> Option<Vec<ResolvedTy>>;

    /// Variant payload field-type lists of a registered enum (or machine, whose
    /// state payloads the consumer surfaces here too). `None` when `name`/`args`
    /// resolve to no enum/machine layout in scope. The outer `Vec` is one entry
    /// per variant; the inner is that variant's payload field types.
    fn enum_variant_field_tys(
        &self,
        name: &str,
        args: &[ResolvedTy],
    ) -> Option<Vec<Vec<ResolvedTy>>>;
}

/// The SINGLE structural heap-ownership authority for the whole compiler.
///
/// Answers "does `ty` transitively own heap memory?" — `true` when `ty` is, or
/// any type reachable through its record fields, enum/machine variant payloads,
/// tuple/array/slice elements, or generic type arguments is, a heap-owning
/// leaf. The leaf set is the `ValueClass::of_ty` heap classification made
/// structural: `string`, `Bytes`, `CancellationToken`, `Generator<Y, R>`,
/// `AsyncGenerator<Y>`, and the `Vec` / `HashMap` / `HashSet` collection
/// handles (each owns a heap buffer / runtime handle regardless of its element
/// type — even `Vec<i64>` owns its backing buffer, even `Generator<i64, ()>`
/// owns its coro frame + heap companion). A record/enum/tuple FIELD of one of
/// these makes the whole composite heap-owning.
///
/// Every "does this own heap" decision — the MIR drop elaborator's composite
/// drop-allow derivation, the move/double-free analyses, the owned-Vec element
/// ABI selection, and the codegen composite-return + owned-Vec-release
/// boundaries — routes through this one walker (over its own
/// [`HeapOwnershipLayouts`] adapter) so the getter, constructor, and release
/// paths cannot reach different verdicts (the `#2191`/`#2150` element-ABI
/// congruence) and a heap-owning record field can no longer be silently missed
/// at a call site that forgot a separate record-aware check
/// (`dedup-semantic-boundary`, `drop-allowset-from-value-flow`).
///
/// # Typed seam (the typed inventory boundary)
///
/// This boolean is the heap axis of the typed
/// [`OwnershipDecision`](crate::OwnershipDecision) — the single typed
/// ownership/drop/ABI decision the consolidation routes the shape-specialised
/// walkers onto (see the walker inventory in `ownership.rs`). The typed entry
/// is [`OwnershipDecision::classify`](crate::OwnershipDecision::classify); the
/// complete carried fact is [`ValueOwnership`](crate::ValueOwnership), whose
/// [`owns_heap`](crate::ValueOwnership::owns_heap) projection **is this
/// authority, `≡` by construction** — `ValueOwnership::classify` seeds it by
/// calling `ty_owns_heap` directly, and the congruence is pinned over every
/// canonical shape by `carried_seed_projections_equal_the_live_authorities`
/// (`ownership.rs`). That equality is the contract a future consolidation seam
/// relies on when it swaps its direct `ty_owns_heap` call for the carried
/// projection:
/// the boolean a consumer reads off the value is exactly the boolean this
/// authority would re-derive, so the swap is byte-identical by construction.
#[must_use]
pub fn ty_owns_heap(ty: &ResolvedTy, layouts: &impl HeapOwnershipLayouts) -> bool {
    ty_owns_heap_inner(ty, layouts, &mut HashSet::new())
}

/// [`HeapOwnershipLayouts`] adapter that supplies enum/machine variant payloads
/// from an `[EnumLayout]` slice but NO record fields. Backs the legacy
/// `ty_contains_heap_owning(ty, enum_layouts)` entry point, which is
/// record-blind by signature — callers that must also consult record fields
/// (every MIR drop-allow / move analysis) use a record-aware adapter instead.
struct EnumLayoutsOnly<'a>(&'a [EnumLayout]);

impl HeapOwnershipLayouts for EnumLayoutsOnly<'_> {
    fn record_field_tys(&self, _name: &str, _args: &[ResolvedTy]) -> Option<Vec<ResolvedTy>> {
        None
    }

    fn enum_variant_field_tys(
        &self,
        name: &str,
        args: &[ResolvedTy],
    ) -> Option<Vec<Vec<ResolvedTy>>> {
        find_enum_layout(name, args, self.0).map(|layout| {
            layout
                .variants
                .iter()
                .map(|v| v.field_tys.clone())
                .collect()
        })
    }
}

fn ty_owns_heap_inner(
    ty: &ResolvedTy,
    layouts: &impl HeapOwnershipLayouts,
    visited: &mut HashSet<String>,
) -> bool {
    match ty {
        // The single heap-leaf set, identical to `ValueClass::of_ty`'s heap
        // classification (`dedup-semantic-boundary`):
        //
        // - `string` / `Bytes` own a refcounted buffer.
        // - `CancellationToken` is an owned, ref-counted runtime handle whose
        //   sole release is `hew_cancel_token_release`.
        // - `Generator<Y, R>` / `AsyncGenerator<Y>` is an owned, affine heap
        //   companion pointer (coro frame + `{ handle, env, out_drop_thunk,
        //   started, pending, out_value }`) whose sole release is
        //   `hew_gen_coro_destroy` — heap-owning regardless of its generic
        //   arguments (`Generator<i64, ()>` owns its coro frame even though
        //   `i64` is bit-copy).
        // - `Vec<T>` / `HashMap<K, V>` / `HashSet<T>` owns a heap buffer for
        //   ANY element type — `Vec<i64>` owns its `hew_vec_free`-released
        //   backing buffer. A record/enum/tuple field of such a type therefore
        //   makes the whole composite heap-owning (`type Boxed { payload:
        //   Vec<i64> }` owns heap through `payload` even though `i64` is
        //   BitCopy).
        //
        // `value_class::of_ty` classifies all of these as `CowValue` /
        // `AffineResource`; this is the matching composite-recursion leaf so
        // the two authorities cannot disagree.
        ResolvedTy::String
        | ResolvedTy::Bytes
        | ResolvedTy::CancellationToken
        | ResolvedTy::Named {
            builtin:
                Some(
                    hew_types::BuiltinType::Generator
                    | hew_types::BuiltinType::AsyncGenerator
                    | hew_types::BuiltinType::Vec
                    | hew_types::BuiltinType::HashMap
                    | hew_types::BuiltinType::HashSet,
                ),
            ..
        } => true,
        ResolvedTy::Named { name, args, .. } => {
            // 1. Type arguments first (fast path: `Option<string>`, etc.).
            if args
                .iter()
                .any(|arg| ty_owns_heap_inner(arg, layouts, visited))
            {
                return true;
            }
            // 2. Record fields (DIV-1: a bare nested user-record carrying a heap
            //    field, e.g. `type Inner { payload: Vec<i64> }`, that the old A
            //    walker answered `false` for because it never looked up record
            //    layouts). The consumer's adapter substitutes the field types.
            if let Some(fields) = layouts.record_field_tys(name, args) {
                let key = record_or_enum_visit_key(name, args);
                if !visited.insert(key.clone()) {
                    // Recursive value type: the checker rejects these; force the
                    // fail-closed path so the drop fires.
                    return true;
                }
                let owns = fields
                    .iter()
                    .any(|ft| ty_owns_heap_inner(ft, layouts, visited));
                visited.remove(&key);
                if owns {
                    return true;
                }
            }
            // 3. Enum / machine variant payloads. Covers non-param heap-owning
            //    fields in generic enums (`Envelope<i64>` where a separate
            //    `Message(string)` variant is unrelated to the type parameter).
            if let Some(variants) = layouts.enum_variant_field_tys(name, args) {
                let key = record_or_enum_visit_key(name, args);
                if !visited.insert(key.clone()) {
                    return true;
                }
                let owns = variants.iter().any(|fields| {
                    fields
                        .iter()
                        .any(|ft| ty_owns_heap_inner(ft, layouts, visited))
                });
                visited.remove(&key);
                if owns {
                    return true;
                }
            }
            false
        }
        ResolvedTy::Tuple(elems) => elems
            .iter()
            .any(|e| ty_owns_heap_inner(e, layouts, visited)),
        ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) => {
            ty_owns_heap_inner(inner, layouts, visited)
        }
        _ => false,
    }
}

/// Visited-set key for a `Named { name, args }` record/enum recursion guard.
///
/// Must DISTINGUISH distinct generic instantiations of one origin: a
/// `Wrap<Wrap<i64>>` field of type `Wrap<i64>` is NOT a recursion — collapsing
/// both to a `short_name`-only key would falsely trip the guard and force the
/// fail-closed `true`, mis-classifying an all-BitCopy nested generic as
/// heap-owning. The key folds the short origin name with each type arg's
/// structural mangle (`mangle_resolved_ty`, the per-type structural key — NOT
/// the `mangle(.., args)` layout-lookup authority, so the
/// `mangle_feeding_layout_lookup_is_centralised` guard stays satisfied). A
/// genuinely recursive value type (checker-rejected) still terminates: its
/// self-referential field keys identically and trips the guard at the
/// fail-closed `true`.
fn record_or_enum_visit_key(name: &str, args: &[ResolvedTy]) -> String {
    if args.is_empty() {
        short_name(name).to_string()
    } else {
        let arg_keys: Vec<String> = args.iter().map(hew_hir::mangle_resolved_ty).collect();
        format!("{}<{}>", short_name(name), arg_keys.join(","))
    }
}

/// Returns `true` if `ty` is, or transitively contains, a `#[opaque]` runtime
/// handle with no clone-dup helper (e.g. `json.Value`, `cron.Expr`).
///
/// This is the SINGLE transitive authority for "does this composite carry an
/// unclonable opaque handle anywhere inside it?" Every actor-state clone-emission
/// and layout-witness decision consults it so ANY type that transitively contains
/// an opaque handle FAILS CLOSED uniformly — there is no per-shape patch surface
/// (`Vec<json.Value>`, `Option<json.Value>`, `HashMap<K, json.Value>`, a record
/// whose field is opaque, an enum whose variant payload is opaque, and arbitrary
/// nestings all answer `true` through the same recursion).
///
/// The classifier (`state_clone::classify_named`) already routes a bare opaque
/// field to `StateFieldCloneKind::OpaqueHandle`, whose clone fails closed in
/// `clone_helper_for_kind`. The gap this authority closes is the GENERIC /
/// COMPOSITE context: `collection_layout_witness` selects a managed clone for any
/// `Vec`/`HashMap`/`HashSet` REGARDLESS of element kind, and `layout_descriptor_ptr`
/// emits a `plain` (bitcopy) element witness regardless of element identity. A
/// `Vec<json.Value>` therefore reached state-clone as a managed vec with a plain
/// element and shallow-copied the opaque pointer — a double-free / UAF on
/// supervisor restart. Consulting this authority at those decision points makes
/// the composite fail closed at MIR classification time instead.
///
/// ## Coverage (recursion shape mirrors [`ty_contains_heap_owning`])
///
/// * `Named { is_opaque: true, .. }` — the unclonable leaf; returns `true`
///   immediately (a `Vec<json.Value>` flags via its element type-arg).
/// * `Named { args, .. }` — recurses into every type argument (the generic
///   container case: `Vec<T>`, `Option<T>`, `Result<T, E>`, `HashMap<K, V>`).
/// * `Named { .. }` matching a `RecordLayout` — recurses into the record's
///   field types (record-with-opaque-field).
/// * `Named { .. }` matching an `EnumLayout` — recurses into every variant's
///   payload field types (enum-payload-opaque, incl. generic instantiations).
/// * `Tuple` / `Array` / `Slice` — recurses into element types.
///
/// Layout lookup keys mirror `ty_contains_heap_owning` / `register_enum_layouts`:
/// bare name (or `short_name`) for non-generic, `mangle(short, args)` for generic.
/// Cycle-safe via a visited set keyed on record/enum layout names.
///
/// ## Identity discriminator vs. name fallback
///
/// The `is_opaque` discriminator is stamped by `lower_type` and is the primary
/// signal. But a `ResolvedTy` can reach a consumer WITHOUT it — e.g. a binding
/// type recorded for a local whose `Named` carries `is_opaque: false` even
/// though it names an `#[opaque]` decl. For those un-stamped `Named`s,
/// [`ty_contains_unclonable_opaque_with_names`] consults the module's
/// `#[opaque]` decl-name set as a fallback, ordered AFTER record/enum-layout
/// resolution exactly like `state_clone::classify_named`: a user record/enum
/// that merely shares a short name with an opaque handle recurses through its
/// own fields (and is admitted when clean) instead of being mis-flagged. The
/// zero-argument [`ty_contains_unclonable_opaque`] keeps the discriminator-only
/// behaviour for callers that have no name set.
#[must_use]
pub fn ty_contains_unclonable_opaque(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> bool {
    ty_contains_unclonable_opaque_inner(ty, record_layouts, enum_layouts, &[], &mut HashSet::new())
}

/// Like [`ty_contains_unclonable_opaque`] but also fails closed on a `Named`
/// that reaches MIR without the `is_opaque` identity discriminator yet names an
/// `#[opaque]` decl in `opaque_handle_names` (the module's opaque decl-name
/// set). The name fallback fires ONLY for a `Named` that resolves to no user
/// record/enum layout, mirroring the record-layouts-first ordering of
/// `state_clone::classify_named`, so a `#[copy]` value record sharing a short
/// name with an opaque handle is not mis-flagged.
#[must_use]
pub fn ty_contains_unclonable_opaque_with_names(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    opaque_handle_names: &[String],
) -> bool {
    ty_contains_unclonable_opaque_inner(
        ty,
        record_layouts,
        enum_layouts,
        opaque_handle_names,
        &mut HashSet::new(),
    )
}

#[allow(
    clippy::too_many_lines,
    reason = "correctness gate: step 1b guard + six dispatch arms need to stay together for audit"
)]
fn ty_contains_unclonable_opaque_inner(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    opaque_handle_names: &[String],
    visited: &mut HashSet<String>,
) -> bool {
    match ty {
        ResolvedTy::Named {
            name,
            args,
            is_opaque,
            builtin,
            ..
        } => {
            // 1. The unclonable opaque leaf — identity discriminator wins first.
            if *is_opaque {
                return true;
            }
            let short = short_name(name);
            // 2. User record layout lookup — mangle-aware so a generic
            //    instantiation (`mymod.LocalPid<Socket>` registered as
            //    `LocalPid$$Socket`) is found before the handle-skip below.
            //    Field types in the layout are already substituted by the
            //    HIR mono pass, so we recurse concrete types directly.
            //
            //    QUALIFIED-NAME GUARD: a short-name-only match does NOT prove
            //    the type IS this user layout if the incoming name is qualified
            //    (`json.Value` matching the short `Value`). Only an EXACT
            //    (qualified) layout-name match or an unqualified-name ANY-match
            //    constitutes a genuine resolution that suppresses the opaque
            //    name-fallback (step 6). See also the old step-5 qualified guard.
            let record_match = find_record_layout(name, args, record_layouts);
            let name_is_qualified = name.contains('.');
            let record_resolved = record_match.is_some_and(|r| {
                if name_is_qualified {
                    r.name == *name
                } else {
                    true
                }
            });
            if let Some(record) = record_match {
                if record_resolved && visited.insert(record.name.clone()) {
                    let found = record.field_tys.iter().any(|ft| {
                        ty_contains_unclonable_opaque_inner(
                            ft,
                            record_layouts,
                            enum_layouts,
                            opaque_handle_names,
                            visited,
                        )
                    });
                    visited.remove(&record.name);
                    if found {
                        return true;
                    }
                }
            }
            // 3. User enum layout lookup — recurse into variant payloads.
            //    `find_enum_layout` shortens the spine so a qualified payload
            //    resolves to its registered bare-arg key.
            //
            //    Same qualified-name guard as step 2: `find_enum_layout` uses
            //    short-name mangling and can match `a.Wrapper<i64>` against the
            //    `Wrapper$$i64` layout by short-outer-name. An exact match on
            //    the layout key (== full-mangled key, not the bare or short
            //    key) proves genuine resolution for a qualified name.
            let enum_found = find_enum_layout(name, args, enum_layouts);
            let enum_resolved = enum_found.is_some_and(|el| {
                if name_is_qualified {
                    el.name == *name
                } else {
                    true
                }
            });
            if let Some(layout) = enum_found {
                if enum_resolved && visited.insert(layout.name.clone()) {
                    let found = layout.variants.iter().any(|v| {
                        v.field_tys.iter().any(|ft| {
                            ty_contains_unclonable_opaque_inner(
                                ft,
                                record_layouts,
                                enum_layouts,
                                opaque_handle_names,
                                visited,
                            )
                        })
                    });
                    visited.remove(&layout.name);
                    if found {
                        return true;
                    }
                }
            }
            // 4. Bit-copy actor-handle type: LocalPid<T>.
            //    Placed AFTER the record/enum-layout lookups (steps 2-3) so a
            //    user record/enum that genuinely shadows this name
            //    (proven by EXACT layout-name match, not short-name-only) is
            //    already handled above and never reaches this arm.
            //    Uses the `builtin` identity discriminator (authoritative) rather
            //    than the name string: a user-declared generic `type LocalPid<T>`
            //    (or an imported `mymod.LocalPid`, `builtin: None`) falls through
            //    here only if it ALSO has no layout in scope. With `builtin: None`
            //    the match below does NOT fire, so the args recursion (step 5)
            //    walks the type args and finds any opaque payload. Only a real
            //    builtin handle (stamped `builtin: Some(LocalPid)` by the
            //    checker) earns the bit-copy skip.
            if matches!(builtin, Some(hew_types::BuiltinType::LocalPid)) {
                return false;
            }
            // 5. Type arguments (generic builtins with no layout: Vec<T>,
            //    Option<T>, Result<T, E>, HashMap<K, V>). Also fires for a
            //    non-builtin name with no layout (e.g. a module-qualified user
            //    generic `mymod.LocalPid<Socket>` with `builtin: None`): its
            //    args are walked and the opaque Socket is detected.
            if args.iter().any(|arg| {
                ty_contains_unclonable_opaque_inner(
                    arg,
                    record_layouts,
                    enum_layouts,
                    opaque_handle_names,
                    visited,
                )
            }) {
                return true;
            }
            // 6. Name fallback — a `Named` that reaches MIR WITHOUT the
            //    `is_opaque` discriminator (step 1) yet names an `#[opaque]`
            //    decl. Fires ONLY when the name resolved to no USER layout that
            //    genuinely identifies the type (steps 2/3, with the
            //    qualified-name guard) and is not a builtin handle (step 4),
            //    mirroring `state_clone::classify_named`'s record-layouts-first
            //    ordering so a value record sharing a short name with an opaque
            //    handle is not mis-flagged. Empty for the discriminator-only
            //    `ty_contains_unclonable_opaque` entry point (no
            //    opaque_handle_names provided), so its behaviour is unchanged.
            //
            //    QUALIFIED-NAME GUARD (security, UAF/double-free): steps 2/3
            //    only suppress the fallback on an EXACT layout-name match for
            //    a qualified incoming name. A short-name-only match (e.g.
            //    `json.Value` → bare `Value` layout) does NOT suppress the
            //    fallback, so the qualified opaque handle still fails closed
            //    even when a clean same-short user record/enum exists.
            let resolved_to_user_layout = record_resolved || enum_resolved;
            if !resolved_to_user_layout
                && opaque_handle_names
                    .iter()
                    .any(|n| n == name || short_name(n) == short)
            {
                return true;
            }
            false
        }
        ResolvedTy::Tuple(elems) => elems.iter().any(|e| {
            ty_contains_unclonable_opaque_inner(
                e,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            )
        }),
        ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) => {
            ty_contains_unclonable_opaque_inner(
                inner,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            )
        }
        _ => false,
    }
}

/// True when `ty` is, or transitively contains, a closure-pair value
/// (`fn(...) -> T` / closure surface type).
///
/// Transitive authority mirroring [`ty_contains_unclonable_opaque`] for the
/// CLONE-refused closure-pair class: a closure pair's environment box has a
/// sole owner and no retain/deep-copy path, so every context whose clone
/// direction is reachable (actor-state classification, owned-Vec element
/// harvesting) consults this walk and fails closed at MIR time rather than
/// refusing late at codegen synthesis or — worse — at runtime on a restart
/// or push. Recursion shape, layout keying, and cycle handling are identical
/// to the opaque walk.
#[must_use]
pub fn ty_contains_closure_value(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> bool {
    ty_contains_closure_value_inner(ty, record_layouts, enum_layouts, &mut HashSet::new())
}

fn ty_contains_closure_value_inner(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    visited: &mut HashSet<String>,
) -> bool {
    match ty {
        ResolvedTy::Function { .. } | ResolvedTy::Closure { .. } => true,
        ResolvedTy::Named { name, args, .. } => {
            if args.iter().any(|arg| {
                ty_contains_closure_value_inner(arg, record_layouts, enum_layouts, visited)
            }) {
                return true;
            }
            let short = short_name(name);
            if let Some(record) = record_layouts
                .iter()
                .find(|r| r.name == *name || short_name(&r.name) == short)
            {
                if visited.insert(record.name.clone()) {
                    let found = record.field_tys.iter().any(|ft| {
                        ty_contains_closure_value_inner(ft, record_layouts, enum_layouts, visited)
                    });
                    visited.remove(&record.name);
                    if found {
                        return true;
                    }
                }
            }
            let enum_found = find_enum_layout(name, args, enum_layouts);
            if let Some(layout) = enum_found {
                if visited.insert(layout.name.clone()) {
                    let found = layout.variants.iter().any(|v| {
                        v.field_tys.iter().any(|ft| {
                            ty_contains_closure_value_inner(
                                ft,
                                record_layouts,
                                enum_layouts,
                                visited,
                            )
                        })
                    });
                    visited.remove(&layout.name);
                    if found {
                        return true;
                    }
                }
            }
            false
        }
        ResolvedTy::Tuple(elems) => elems
            .iter()
            .any(|e| ty_contains_closure_value_inner(e, record_layouts, enum_layouts, visited)),
        ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) => {
            ty_contains_closure_value_inner(inner, record_layouts, enum_layouts, visited)
        }
        _ => false,
    }
}

/// True when `ty` may own — transitively — an affine HANDLE leaf the owned-handle
/// aggregate double-free gate (`detect_unproven_aggregate_handle_double_free`)
/// guards: a `Generator`/`AsyncGenerator` context, a `CancellationToken`, or a
/// `Resource`-marker builtin handle (Stream/Sink/Duplex/SendHalf/RecvHalf/…).
///
/// Unlike [`ty_contains_heap_owning`], the copy-on-write VALUE leaves
/// (`String`/`Bytes`/`Vec`/`HashMap`/`HashSet`) are NOT handles and answer
/// `false` — their exactly-once is proven elsewhere and they cannot trigger the
/// handle double-free this gate guards. The escape gate uses this to decide
/// whether a value aliased out of tracked dataflow could actually carry a
/// handle: a `Vec<i64>` sibling field extracted from a `(Generator, Vec, …)`
/// tuple is provably handle-free, so reading it must NOT poison the tuple's
/// generator origin.
///
/// Fail-closed: an opaque user record/enum whose fields are not visible here (a
/// `Named { builtin: None, .. }` with no resolvable `EnumLayout`), a `dyn` trait
/// object, or a bare type parameter answers `true` — it may hide a handle field,
/// so the gate never skips poisoning a carrier that could alias a handle out.
#[must_use]
pub fn ty_may_carry_owned_handle(ty: &ResolvedTy, enum_layouts: &[EnumLayout]) -> bool {
    ty_may_carry_owned_handle_inner(ty, enum_layouts, &mut HashSet::new())
}

fn ty_may_carry_owned_handle_inner(
    ty: &ResolvedTy,
    enum_layouts: &[EnumLayout],
    visited_enum_layouts: &mut HashSet<String>,
) -> bool {
    let recurse = |t: &ResolvedTy, v: &mut HashSet<String>| {
        ty_may_carry_owned_handle_inner(t, enum_layouts, v)
    };
    #[allow(
        clippy::match_same_arms,
        reason = "the handle-leaf, dyn/type-param, and catch-all arms are kept \
                  distinct for readability and exhaustiveness even where their \
                  boolean answer coincides"
    )]
    match ty {
        // Affine handle leaves — the contexts this gate guards.
        ResolvedTy::CancellationToken => true,
        ResolvedTy::Named {
            builtin: Some(b),
            args,
            name,
            ..
        } => {
            // A NON-OWNING actor-pid leaf (`Pid`/`LocalPid`/`RemotePid`) carries
            // no drop glue (no `close` ABI; its drop is a codegen no-op) and is a
            // copyable by-value reference — it is never an owned-handle origin
            // and can never alias one OUT of this frame. Exclude it before the
            // `Resource`-marker test below so a pid sibling extracted from a
            // MIXED aggregate (e.g. `t.1` of a `(Generator, LocalPid)` tuple,
            // whose origin set the fail-closed field-load over-approximates to
            // include the generator) is treated as provably handle-free and does
            // NOT poison the real generator origin. Mirror of the pid carve in
            // `ty_is_owned_handle_leaf`. The pid's actor-type argument is the
            // referent's identity, not a by-value carried field, so we must not
            // recurse into it — return `false` directly.
            if matches!(
                b.handle_family(),
                Some(hew_types::builtin_type::BuiltinHandleFamily::ActorPid)
            ) && b.close_method().is_none()
            {
                return false;
            }
            if matches!(
                b,
                hew_types::BuiltinType::Generator | hew_types::BuiltinType::AsyncGenerator
            ) || matches!(
                b.marker(),
                hew_types::builtin_type::BuiltinTypeMarker::Resource
            ) {
                return true;
            }
            // A non-handle builtin (`Vec`/`HashMap`/`Option`/`Result`/…): the
            // handle, if any, rides on a type argument or an enum-layout field.
            if args.iter().any(|arg| recurse(arg, visited_enum_layouts)) {
                return true;
            }
            ty_layout_carries_owned_handle(name, args, enum_layouts, visited_enum_layouts, false)
        }
        ResolvedTy::Named {
            builtin: None,
            args,
            name,
            ..
        } => {
            // User record / enum / type-name: check type args and the
            // monomorphised layout; if neither resolves the fields, FAIL CLOSED.
            if args.iter().any(|arg| recurse(arg, visited_enum_layouts)) {
                return true;
            }
            ty_layout_carries_owned_handle(name, args, enum_layouts, visited_enum_layouts, true)
        }
        ResolvedTy::Tuple(elems) => elems.iter().any(|e| recurse(e, visited_enum_layouts)),
        ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) | ResolvedTy::Task(inner) => {
            recurse(inner, visited_enum_layouts)
        }
        ResolvedTy::Closure { captures, .. } => {
            captures.iter().any(|c| recurse(c, visited_enum_layouts))
        }
        // `dyn Trait` and a bare type parameter could erase any owning type.
        ResolvedTy::TraitObject { .. } | ResolvedTy::TypeParam { .. } => true,
        // Scalars, `String`/`Bytes`, `Borrow`/`Pointer` (non-owning),
        // `Function`, `Duration`, `Unit`, `Never`: never an owned handle.
        _ => false,
    }
}

/// Resolve a `Named` type's monomorphised `EnumLayout` and report whether any
/// variant field may carry an owned handle. `unresolved_default` is returned
/// when no layout matches: `true` (fail-closed) for opaque user types,
/// `false` for known handle-free builtins whose args were already cleared.
fn ty_layout_carries_owned_handle(
    name: &str,
    args: &[ResolvedTy],
    enum_layouts: &[EnumLayout],
    visited_enum_layouts: &mut HashSet<String>,
    unresolved_default: bool,
) -> bool {
    let found = find_enum_layout(name, args, enum_layouts);
    let Some(layout) = found else {
        return unresolved_default;
    };
    if !visited_enum_layouts.insert(layout.name.clone()) {
        // Recursive value type — fail closed.
        return true;
    }
    let carries = layout.variants.iter().any(|v| {
        v.field_tys
            .iter()
            .any(|ft| ty_may_carry_owned_handle_inner(ft, enum_layouts, visited_enum_layouts))
    });
    visited_enum_layouts.remove(&layout.name);
    carries
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub statements: Vec<MirStatement>,
}

/// The per-carrier payload of a collapsed suspension point, stored in the
/// [`RawMirFunction::suspend_kinds`] side-table keyed by the id of the basic
/// block whose terminator is the bare [`Terminator::Suspend`].
///
/// The ten PURE-`{resume, cleanup}` suspension carriers
/// (`SuspendingAsk`/`SuspendingRead`/`SuspendingAccept`/`SuspendingCallClosure`/
/// `SuspendingStreamNext`/`SuspendingStreamSend`/`SuspendingChannelRecv`/
/// `SuspendingRemoteAsk`/`SuspendingTaskAwait`/`SuspendingSleep`) carry no CFG
/// successor edge beyond `resume` + `cleanup`, so the suspension SUBSTRATE they
/// share — the `coro.suspend` + 3-way switch — is exactly [`Terminator::Suspend`].
/// Their distinguishing PAYLOAD (which readiness source to register, which
/// result slot to bind on resume) lives here in the side-table rather than on a
/// dedicated terminator variant — following the `await_deadline_ns` precedent
/// (a side-table, not a carrier field). Codegen reads this at the `Suspend`
/// dispatch and routes to the SAME `emit_suspending_*` ramp it always did, so
/// the emitted IR is byte-identical to the dedicated-carrier shape.
///
/// `SuspendingSelect` (per-arm `body_block`s) and `SuspendingScopeDeadline`
/// (`timeout_body_block`) carry real EXTRA CFG successor edges a bare `Suspend`
/// cannot hold, so they stay distinct terminators and are NOT represented here.
///
/// Each variant mirrors its carrier's non-edge fields exactly. The `resume`,
/// `cleanup`, and `is_final` edges live on [`Terminator::Suspend`]; the
/// `await … | after d` deadline (ns) continues to live in
/// [`RawMirFunction::await_deadline_ns`] keyed by the same block id.
#[derive(Debug, Clone, PartialEq)]
pub enum SuspendKind {
    /// Non-blocking `await actor.method(value)` (`SuspendingAsk`).
    Ask {
        actor: Place,
        msg_type: i32,
        value: Place,
        result_dest: Place,
        reply_dest: Place,
        error_dest: Place,
    },
    /// Non-blocking `await conn.read()` (`SuspendingRead`).
    Read {
        conn: Place,
        result_dest: Place,
        deadline_result_dest: Option<Place>,
        error_dest: Option<Place>,
        to_string: bool,
    },
    /// Non-blocking `await listener.accept()` (`SuspendingAccept`).
    Accept {
        listener: Place,
        result_dest: Place,
        deadline_result_dest: Option<Place>,
        error_dest: Option<Place>,
    },
    /// Suspendable-callee driver `await closure(args...)` (`SuspendingCallClosure`).
    CallClosure {
        callee: Place,
        args: Vec<Place>,
        ret_ty: ResolvedTy,
        result_dest: Option<Place>,
    },
    /// Non-blocking `await stream.recv()` (`SuspendingStreamNext`).
    StreamNext {
        stream: Place,
        result_dest: Place,
        elem_ty: ResolvedTy,
        deadline_result_dest: Option<Place>,
        error_dest: Option<Place>,
    },
    /// Non-blocking `await sink.send(value)` (`SuspendingStreamSend`).
    StreamSend { sink: Place, value: Place },
    /// Non-blocking `await rx.recv()` over a `std::channel` (`SuspendingChannelRecv`).
    ChannelRecv {
        receiver: Place,
        result_dest: Place,
        elem_ty: ResolvedTy,
        deadline_result_dest: Option<Place>,
        error_dest: Option<Place>,
    },
    /// Non-blocking `await remote.ask(...)` across nodes (`SuspendingRemoteAsk`).
    RemoteAsk {
        actor: Place,
        msg_type: i32,
        value: Place,
        timeout_ms: Place,
        result_dest: Place,
        reply_dest: Place,
        error_dest: Place,
        reply_ty: ResolvedTy,
    },
    /// Non-blocking `await t` over a scope-owned child `Task<T>` (`SuspendingTaskAwait`).
    TaskAwait {
        scope: Place,
        task: Place,
        result_dest: Option<Place>,
    },
    /// Non-blocking `await_restart sup.child` — park the current actor on the
    /// supervisor restart observer until the static child at `slot_index`
    /// becomes Live again (it restarted), then resume re-fetching the now-Live
    /// `LocalPid<ChildType>` into `result_dest`. The supervisor analogue of
    /// `TaskAwait`: lowered through the SAME cooperative-suspension machinery,
    /// parking against the supervisor's `restart_notify` (NOT the thread-blocking
    /// `hew_supervisor_wait_restart`). A permanently-Dead child fails closed
    /// (resumes immediately) rather than hanging forever.
    ///
    /// `deadline_result_dest` is RESERVED for the future bounded form
    /// (`await_restart sup.w within: 5.seconds → Option<LocalPid<T>>`), mirroring
    /// the deadline slot on `Read`/`Accept`/`StreamNext`; the bare form leaves
    /// it `None`.
    RestartWait {
        sup_place: Place,
        slot_index: u32,
        result_dest: Place,
        deadline_result_dest: Option<Place>,
    },
    /// `sleep(d)` — suspends for the given `duration` (`SuspendingSleep`).
    ///
    /// `duration_ns` is a `Place` holding a `duration` value (i64 nanoseconds).
    /// Codegen converts to milliseconds before arming the timer wheel.
    Sleep { duration_ns: Place },
    /// `sleep_until(i)` — suspends until the given `instant` (`SuspendingSleepUntil`).
    ///
    /// `instant_ns` is a `Place` holding an `instant` value (i64 nanoseconds,
    /// the same ABI as `duration`). Codegen computes `max(0, instant − now) / 1_000_000`
    /// and arms the timer wheel for that many milliseconds.
    SleepUntil { instant_ns: Place },
}

/// A lexical scope's facts for gdb `-g` `DILexicalBlock` emission, threaded from
/// the MIR lowering pass (see [`RawMirFunction::scope_table`]). `id` is the raw
/// HIR `ScopeId` value (matching `RawMirFunction::local_scopes` entries);
/// `parent` is the enclosing scope's id, or `None` for the function-body root;
/// `start`/`end` are the source byte-extent covering every span lowered under
/// this scope. Codegen builds a `DILexicalBlock` per entry and uses the
/// byte-extent → line to scope instruction `DILocation`s by source containment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MirScope {
    pub id: u32,
    pub parent: Option<u32>,
    pub start: u32,
    pub end: u32,
}

/// PROVEN source attribution for a MIR function's byte-offset spans.
///
/// A [`RawMirFunction::span`] and the entries in [`RawMirFunction::instr_spans`]
/// are bare byte offsets (`hew_lexer::Span` carries no source identity). This
/// enum records WHICH source those offsets index, resolved at MIR lowering from
/// the HIR's positive attribution (`HirModule::root_item_ids` /
/// `HirModule::diagnostic_source_modules`) — never inferred by absence from a
/// set.
///
/// The CLI's fail-closed renderer points a `^^^` caret at the source it is
/// rendering (the root compilation unit's `state.source`) ONLY when the
/// offending function carries [`SourceOrigin::RootUnit`]. Every other origin
/// degrades to a bare diagnostic line: a bare line is always safe, a caret
/// against the wrong source is not.
///
/// WHY carried per-function rather than inferred at the CLI: a function can
/// enter `HirModule.items` many ways (root source, module-path import,
/// file-path import, builtin, monomorphisation, generated impl default-method
/// copied from a trait). The generated default-method is the decisive case —
/// its body spans index the TRAIT's source even though it is synthesised while
/// lowering a root-file impl, so no "which module was being lowered" heuristic
/// can attribute it. Carrying the proven origin on the function is the only
/// leak-free authority.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum SourceOrigin {
    /// The function's spans PROVABLY index the root compilation unit's source
    /// (the file the user is compiling, rendered as `state.source`). Recorded
    /// positively in `HirModule::root_item_ids` for functions lowered natively
    /// from the root file at module index 0 — excluding copied-body synths
    /// (trait default-methods). A caret against `state.source` is safe.
    RootUnit,
    /// The function's spans index an imported module's source (named), NOT the
    /// root. The CLI has no buffer for that source, so the diagnostic degrades
    /// to a bare plain-line.
    Foreign(String),
    /// No source attribution established: synthesised functions (actor
    /// handlers, machine dispatch, drop shims, closures), monomorphisations of
    /// a foreign origin, and hand-built test MIR. Degrades to bare — the
    /// fail-closed default.
    #[default]
    Unknown,
}

impl SourceOrigin {
    /// Whether a `^^^` caret may be rendered against the root compilation
    /// unit's source for a fail-closed error carried by this function.
    #[must_use]
    pub fn renders_root_caret(&self) -> bool {
        matches!(self, SourceOrigin::RootUnit)
    }
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
    /// Source-level binding name for each local slot, parallel to `locals`
    /// (`local_names[i]` is the name of `Place::Local(i)`, or `None` for an
    /// anonymous temporary / a slot whose name could not be recovered).
    /// Threaded from the lowering pass — params from the parameter prologue,
    /// `let` bindings from the `MirStatement::Bind` stream. Codegen consumes
    /// this under `hew build -g` to emit `DW_TAG_variable` /
    /// `DW_TAG_formal_parameter` DIEs with the user's name
    /// (`create_auto_variable` / `create_parameter_variable`). Fail-closed: a
    /// `None` entry (or a slot index past the end, for hand-built test MIR
    /// that leaves this empty) means codegen emits no variable DIE for that
    /// slot rather than fabricating a name — `local_N` is never invented.
    /// Empty for synthesised functions and legacy test MIR.
    pub local_names: Vec<Option<String>>,
    /// gdb `-g` lexical scoping, parallel to `locals`/`local_names`.
    /// `local_scopes[i]` is the raw HIR `ScopeId` value the binding occupying
    /// `Place::Local(i)` was declared in, or `None` for params / anonymous
    /// temporaries / slots whose scope could not be recovered. Threaded from the
    /// lowering pass. Codegen consumes this under `-g` to scope each
    /// `DW_TAG_variable` to a `DILexicalBlock` (built from `scope_table`), so a
    /// shadowed inner `let first` gets its OWN DIE in its OWN block and the
    /// outer `first` no longer leaks into the inner breakpoint's frame. Empty for
    /// synthesised functions and legacy test MIR (→ flat subprogram scoping).
    pub local_scopes: Vec<Option<u32>>,
    /// gdb `-g` declaration line, parallel to `locals`/`local_names`.
    /// `local_decl_bytes[i]` is the source start-byte of the `let` that
    /// introduced `Place::Local(i)`, or `None`. Codegen maps it through the
    /// byte→line index so each local DIE carries its own declaration line rather
    /// than the function-declaration line. Empty for synthesised / legacy MIR.
    pub local_decl_bytes: Vec<Option<u32>>,
    /// gdb `-g` lexical-block table. One entry per HIR `ScopeId` referenced by
    /// `local_scopes` (and every ancestor up to the function-body root), giving
    /// the scope's parent and source byte-extent `[start, end)`. Codegen builds
    /// one `DILexicalBlock` per entry, parented per `parent` (rooted at the
    /// subprogram), and assigns each instruction's `DILocation` to the innermost
    /// scope whose byte-extent contains the instruction's start byte. Empty →
    /// codegen falls back to flat subprogram scoping (the pre-`-g`-lexical
    /// behaviour), which is correct for any function with no shadowing.
    pub scope_table: Vec<MirScope>,
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
    /// NEW-6b `await … | after d` deadlines. Maps the id of each basic block
    /// whose terminator is a `Terminator::SuspendingAsk` carrying a deadline to
    /// the deadline value in nanoseconds. Empty for functions with no
    /// deadline-await. Codegen schedules `hew_await_cancel_schedule_deadline_ms`
    /// (ns → ms) against the suspend's cancel registration for these blocks and
    /// shapes the resume edge to `Err(AskError::Timeout)` on expiry. A side-table
    /// (not a carrier field) keeps the eight `Suspending*` terminators unchanged.
    pub await_deadline_ns: std::collections::HashMap<u32, i64>,
    /// Per-block payload for collapsed suspension points. Maps the id of each
    /// basic block whose terminator is the bare [`Terminator::Suspend`] AND
    /// originated from one of the ten PURE-`{resume, cleanup}` suspension
    /// carriers to the carrier's distinguishing payload ([`SuspendKind`]).
    /// Empty for functions with no collapsed suspension carrier (a bare
    /// `Terminator::Suspend` with no entry is a generator / synthetic substrate
    /// suspend, which codegen lowers directly via `coro.suspend`). Codegen reads
    /// this at the `Suspend` dispatch to route to the matching `emit_suspending_*`
    /// ramp. A side-table (not carrier fields) collapses the ten carriers onto
    /// one terminator while keeping the emitted IR byte-identical — the same
    /// reasoning as `await_deadline_ns` above.
    pub suspend_kinds: std::collections::HashMap<u32, SuspendKind>,
    /// User-visible parameter locals for a `FunctionCallConv::LambdaActorBody`
    /// function, in declaration order. Each entry is the index of a
    /// `Place::Local(N)` whose `locals[N]` carries the user-visible type
    /// (e.g. `i64`, `string`). Codegen iterates this list after the standard
    /// five-slot ABI-parameter prologue and emits a per-param deserialise
    /// fragment that loads the value from the message payload pointer
    /// (`Place::Local(1)`) into the user-param alloca. Empty for every other
    /// call convention. WHY a side-channel and not extra `params`: the LLVM
    /// signature of a lambda-actor body is fixed by the runtime ABI to the
    /// five-slot shape; the user params live INSIDE the message payload, not
    /// in the LLVM argument list.
    pub lambda_actor_user_param_locals: Vec<u32>,
    /// Byte-offset span `(start, end)` of this function's declaration in the
    /// originating source file, threaded from `HirFn::span` at MIR lowering.
    /// Codegen maps `start` through a byte-offset → line index to emit the
    /// function-entry `DILocation` / `DISubprogram` declaration line under
    /// `hew build -g`. `None` for synthesised functions (drop shims, machine
    /// dispatch, vtable thunks) and hand-built test MIR that carry no faithful
    /// source span — fail-closed: codegen emits NO location for them rather
    /// than fabricating `line = 0` (a location-free function is legal at -O0).
    pub span: Option<(u32, u32)>,
    /// Per-instruction source spans for the backend-authority `Instr` stream,
    /// keyed by `(block_id, instruction_index)` and valued by the originating
    /// HIR statement/expression's byte-offset span `(start, end)`. Threaded
    /// from each `Instr` push in `lower.rs` via the lowering cursor's
    /// `current_span` (the enclosing statement/tail). A side-table — rather
    /// than a field on `Instr` — keeps the backend `Instr` enum and its many
    /// codegen match sites unchanged.
    ///
    /// Codegen maps `start` through the same byte-offset → line index used for
    /// the function-entry line (Stage 1) to set a per-instruction `DILocation`
    /// under `hew build -g`, so gdb steps line-by-line. Fail-closed: an
    /// instruction with no entry here (a synthesised instruction lowered
    /// outside any statement) inherits the nearest enclosing location rather
    /// than fabricating one; empty for synthesised functions and hand-built
    /// test MIR.
    pub instr_spans: std::collections::BTreeMap<(u32, u32), (u32, u32)>,
    /// PROVEN source attribution for this function's `span` / `instr_spans`
    /// byte offsets (which source they index). Set at MIR lowering from the
    /// HIR's positive attribution: the item loop resolves it from the origin
    /// `HirFn`'s `ItemId`, the monomorphisation loop from the generic origin's
    /// `ItemId`. Defaults to [`SourceOrigin::Unknown`] for the synthesised
    /// builders (actor handlers, drop shims) and hand-built test MIR that do
    /// not carry a faithful HIR origin. Codegen gates whether a fail-closed
    /// error keeps its source span on `source_origin.renders_root_caret()`.
    pub source_origin: SourceOrigin,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LambdaActorShape {
    /// One-way send: lambda body returns no value; the runtime treats the
    /// reply slot as unused.
    Tell,
    /// Request/reply: lambda body produces a user reply value that codegen
    /// serialises into the runtime reply buffer.
    Ask,
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
    /// Synthesised body of a lambda actor (`actor |..| { .. }`). Codegen
    /// emits the runtime ABI signature `extern "C-unwind" fn(*mut c_void state,
    /// *const u8 msg, usize msg_len, *mut *mut u8 reply_out, *mut usize reply_len_out)
    /// -> i32`, materialises the user-visible parameter locals from the message
    /// payload at body entry, and (for `Ask` shape) serialises the body's
    /// return-slot value into a freshly-allocated reply buffer via
    /// `hew_lambda_body_alloc_reply_buf` before returning. The shape
    /// discriminant carries that Tell/Ask selection.
    ///
    /// User-visible parameter locals (those that body HIR `BindingRef`s
    /// resolve to) are listed in `RawMirFunction.lambda_actor_user_param_locals`
    /// in declaration order; codegen iterates that list to emit the per-param
    /// deserialise prologue. The first five MIR locals correspond to the five
    /// runtime ABI parameters (`state`, `msg`, `msg_len`, `reply_out`,
    /// `reply_len_out`), bound by the standard parameter prologue.
    LambdaActorBody(LambdaActorShape),
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
            | Terminator::MakeGenerator { next, .. }
            | Terminator::MakeLambdaActor { next, .. }
            | Terminator::Send { next, .. }
            | Terminator::Ask { next, .. }
            | Terminator::RemoteAsk { next, .. }
            | Terminator::Join { next, .. } => vec![*next],
            // The runtime dispatch jumps to exactly one winning arm's `body_block`;
            // each body reaches `next` (the join) through its own `Goto`. Without
            // the arm body edges, arm bodies are unreachable; any move or split
            // that spans a body (e.g. a DuplexHandle consumed inside an arm) is
            // invisible to every CFG pass that walks `successors()` — including
            // liveness, dominators, and the duplex-split cross-block checker —
            // producing false-safe results. The direct `next` edge is kept as
            // the conservative no-arm path so the join block always has a
            // predecessor entry (`Join` has no body blocks of its own).
            Terminator::Select { arms, next } => {
                let mut succs: Vec<u32> = arms.iter().map(|arm| arm.body_block).collect();
                succs.push(*next);
                succs
            }
            // The default suspend-return edge exits the function (returns to the
            // executor, like a `Return`); only the resume + cleanup arms are
            // in-CFG successors. The ten pure-{resume,cleanup} suspension carriers
            // all lower to this bare `Suspend` — their distinguishing payload
            // lives in the `RawMirFunction::suspend_kinds` side-table, which
            // carries no CFG edge.
            Terminator::Suspend {
                resume, cleanup, ..
            } => vec![*resume, *cleanup],
            // The suspending scope-deadline ramp parks the continuation on the
            // race between the scope join and a timer deadline. The default edge
            // exits to the executor; the timeout-body block (deadline edge),
            // resume (join edge + body convergence), and cleanup (abandon) are
            // all in-CFG successors.
            Terminator::SuspendingScopeDeadline {
                timeout_body_block,
                resume,
                cleanup,
                ..
            } => vec![*timeout_body_block, *resume, *cleanup],
            // The suspending select ramp parks the racing continuation on the
            // first-ready of N readiness sources + an optional deadline. The
            // default suspend-return edge exits to the executor. The resume
            // edge's winner dispatch jumps to exactly one arm's `body_block`
            // (each body reaches `resume` — the join — through its own `Goto`),
            // so the arm bodies are real CFG successors exactly as
            // `Terminator::Select`'s are; without them the arm bodies are
            // unreachable and an aggregate arm binding spanning a body trips a
            // false `InitialisedBeforeUse`. `cleanup` (abandon) is the suspend
            // teardown edge.
            Terminator::SuspendingSelect {
                arms,
                resume,
                cleanup,
            } => {
                let mut succs: Vec<u32> = arms.iter().map(|arm| arm.body_block).collect();
                succs.push(*resume);
                succs.push(*cleanup);
                succs
            }
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
    ///
    /// `builtin` carries the checker/HIR-resolved [`RuntimeCallFamily`]
    /// when the callee is a compiler-known runtime builtin that rides the
    /// `Terminator::Call` route (codegen callee intercepts, the
    /// layout-fact walker, and the double-free escape gate all dispatch
    /// on it instead of re-matching the callee string). `None` for user
    /// functions, mangled monomorphisations, machine-step helpers, and
    /// every other open-set callee.
    ///
    /// Invariant: `builtin == Some(f)` implies `callee == f.c_symbol()`
    /// — the family IS the callee identity; the string is its
    /// presentation. Producers go through `lower_direct_call` (which
    /// asserts the invariant) or construct both sides from the same
    /// family.
    Call {
        callee: String,
        builtin: Option<hew_types::runtime_call::RuntimeCallFamily>,
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
    /// Generator construction at a `gen fn` / `gen { }` call site. Codegen
    /// heap-allocates the `Generator<Y, R>` companion (`{ handle, env,
    /// out_drop_thunk, started, pending, out_value }`, via
    /// `hew_cont_frame_alloc`) and calls the `__hew_gen_body_*` coro ramp
    /// directly — the ramp runs the body to its FIRST `yield` and returns the
    /// `llvm.coro.begin` handle, stored into the companion, whose pointer is
    /// stored into `dest`. Then branches to `next`. `body_fn` is the
    /// deterministic `__hew_gen_body_<owner>_<id>` name minted by
    /// `lower_gen_block`; codegen resolves its address via `get_function`.
    /// Carried explicitly (rather than through `Terminator::Call`) because the
    /// body-fn pointer is not a `Place` and the construction is
    /// self-describing without a call-site side table.
    ///
    /// `env` is the stack `Place` holding the `RecordInit`'d capture-env
    /// record, or `None` for a capture-free generator (the companion's `env`
    /// field is then null). The generator body's free variables — a `gen
    /// fn`'s formal parameters and a `gen { }`'s captured outer locals — live
    /// in this record, in `HirGenCapture` order. Codegen heap-copies the
    /// record (`hew_cont_frame_alloc` + `memcpy`) so it outlives this
    /// constructing frame — the body reads it across suspends, long after
    /// construction returns — and passes the heap copy's address to the ramp,
    /// which reads it back through `Local(1)` via `ClosureEnvFieldLoad`. Only
    /// no-drop capture field classes (`BitCopy` scalars, pids) are
    /// materialised today; owned fields (string/aggregate) fail closed at the
    /// construction site because the flat heap-copy would alias their heap
    /// and risk a double-free / UAF without a per-field clone + env-drop
    /// protocol.
    MakeGenerator {
        dest: Place,
        body_fn: String,
        next: u32,
        env: Option<Place>,
    },
    /// Lambda-actor construction at an `actor |params| { body }` spawn
    /// site. Codegen emits `hew_lambda_actor_new(mailbox_capacity, shape,
    /// &body_fn, null_state, &state_drop_fn)` and stores the returned
    /// `*mut HewLambdaActorHandle` into `dest` (a `Duplex<Msg, Reply>`-
    /// typed `Place::LambdaActorHandle(N)`), then branches to `next`.
    ///
    /// `body_fn` is the deterministic `__hew_lambda_body_<owner>_<id>`
    /// name minted by `lower_spawn_lambda_actor`; codegen resolves its
    /// address via `get_function`. `state_drop_fn` is the symmetric
    /// `__hew_lambda_state_drop_<owner>_<id>` no-op stub (no captures
    /// in the M2 MVP, so `state` is null and the drop fn is a no-op).
    ///
    /// `shape` is the `LambdaShape` discriminant from
    /// `hew-runtime/src/lambda_actor.rs:131`: `0 = Tell` (no reply),
    /// `1 = Ask` (reply payload required from the body).
    ///
    /// Carried as a dedicated terminator (not `Terminator::Call` /
    /// `Instr::CallRuntimeAbi`) because the body/state-drop function
    /// pointer args cannot be expressed as MIR `Place` values — same
    /// constraint as `Terminator::MakeGenerator`'s body-fn pointer.
    /// The producer-side `CallRuntimeAbi("hew_lambda_actor_new")` surface
    /// is fail-closed in codegen for the same reason; routing through
    /// `MakeLambdaActor` is the single sanctioned construction path.
    MakeLambdaActor {
        dest: Place,
        body_fn: String,
        state_drop_fn: String,
        shape: i32,
        mailbox_capacity: u32,
        next: u32,
        /// Capture-environment source: the stack Place holding the
        /// `RecordInit`'d env record, or `None` for a capture-free lambda.
        /// Codegen heap-boxes the record (`malloc` + `memcpy` of
        /// `sizeof(env)`) and passes the heap pointer as
        /// `hew_lambda_actor_new`'s `state` arg; the body prologue reads
        /// captures back through that pointer (`ClosureEnvFieldLoad` on
        /// the state slot).
        env: Option<Place>,
        /// Per-field teardown classes for the capture env, in declared
        /// field order. Codegen synthesizes the real `state_drop_fn` body
        /// from these (field drop then `free(env)`), and `WeakSelfHandle`
        /// fields are nulled at box time and back-filled with the
        /// downgraded weak handle after construction — the weak self
        /// reference cannot exist before the actor it refers to.
        env_field_drops: Vec<LambdaEnvFieldDrop>,
    },
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
        /// `Result<R, AskError>` slot — the user-visible binding type after
        /// the R-ASK unification.  Codegen emits `Ok(reply_value)` on a
        /// successful reply (non-null pointer from `hew_actor_ask`) and
        /// `Err(AskError::<variant>)` on failure (null pointer), obtaining
        /// the error discriminant from `hew_actor_ask_take_last_error`.
        result_dest: Place,
        /// Raw reply slot; holds the unwrapped `R` value written by the
        /// runtime on a successful ask.  Used as the `Ok` payload of
        /// `result_dest`.
        reply_dest: Place,
        /// `AskError` slot populated by `hew_actor_ask_take_last_error` on
        /// the null-return (failure) path, then folded into `result_dest`
        /// as the `Err` variant payload.
        error_dest: Place,
        next: u32,
    },
    /// Remote actor ask: send `value` to a `RemotePid<T>` and construct
    /// `Result<Reply, AskError>` in `result_dest`.
    RemoteAsk {
        actor: Place,
        msg_type: i32,
        value: Place,
        timeout_ms: Place,
        result_dest: Place,
        reply_dest: Place,
        error_dest: Place,
        reply_ty: ResolvedTy,
        next: u32,
    },
    /// Sealed `select{}` construct. The terminator carries the per-arm
    /// discriminator and per-arm body block ids; the runtime substrate
    /// is `hew_select_first` (blocking poll) for non-suspendable callers
    /// and the `SuspendingSelect` coroutine path for callers that carry
    /// an execution context (actor handlers, tasks). Codegen emits this
    /// terminator via `emit_select_terminator` for non-suspendable callers.
    ///
    /// The arm vector is non-empty (HIR enforces) and contains at most
    /// one `AfterTimer` arm (HIR enforces). The `next` slot is the
    /// block reached after the winning arm body completes — the join
    /// edge that converges the per-arm bodies. The arm `body_block`s
    /// are real CFG successors (see `BasicBlock::successors`).
    Select { arms: Vec<SelectArm>, next: u32 },
    /// Sealed `select{}` from a SUSPENDABLE caller (cut-select-waitset).
    /// The coro-suspend sibling of [`Terminator::Select`]: instead of the
    /// blocking `hew_select_first` busy-poll (a `std::thread::sleep(1ms)`
    /// loop over the per-arm readiness flags that PINS the M:N worker),
    /// codegen builds the SAME per-arm readiness waitset (each arm's
    /// channel + readiness observer firing `hew_reply_channel_signal_ready`
    /// on its source becoming readable) but attaches ONE shared
    /// `HewAwaitCancel` arbiter + the parked actor to every arm's channel,
    /// arms the `AfterTimer` arm as a deadline on that arbiter
    /// (`hew_await_cancel_schedule_deadline_ms` on `hew_global_timer_wheel`
    /// — the same shared wheel cut-task-sleep uses), then `coro.suspend`s,
    /// freeing the worker. The FIRST arm to become ready (or the deadline)
    /// wins the arbiter's one-shot CAS and re-enqueues the parked
    /// continuation (`enqueue_resume`); on the resume edge codegen scans
    /// the readiness flags once (non-blocking) to find the winner, binds
    /// it, and CANCELS the losers (deregister the other observers + cancel
    /// the timer). The abandon edge cancels the arbiter (no wake),
    /// deregisters every observer, and frees the registration.
    ///
    /// The `arms` payload is identical to [`Terminator::Select`] — every
    /// arm carries `body_block` (reached on win) and `binding` (the per-arm
    /// value slot; `None` for `AfterTimer`). Carries the same SUSPEND
    /// carrier the codegen boundary reads for `has_suspend` / `is_coroutine`:
    /// any function whose CFG contains this terminator is lowered as a
    /// `presplitcoroutine`. Emitted ONLY when the lowering function carries
    /// the execution context
    /// (`FunctionCallConv::carries_execution_context` — actor handler /
    /// closure / task entry); a `FunctionCallConv::Default` caller (`main`,
    /// free fns) runs on a foreign thread with no parkable continuation and
    /// keeps the blocking [`Terminator::Select`] / `hew_select_first` path.
    SuspendingSelect {
        /// The select arms — the same shape [`Terminator::Select`] carries.
        /// Codegen reads each arm's kind to build its readiness observer and
        /// each arm's `body_block` to route the winner edge.
        arms: Vec<SelectArm>,
        /// Block reached on the coro switch resume / immediate-ready edge
        /// (case 0) — the winner-scan + per-arm dispatch. This is the `next`
        /// join block of the original select; the winner blocks branch into
        /// their arm bodies which converge here.
        resume: u32,
        /// Block reached on the coro switch cleanup edge (case 1) — the
        /// frame's teardown when the parked continuation is abandoned
        /// (`coro.destroy`); every observer is deregistered and the arbiter
        /// freed there. Rides the multi-suspend epilogue, so `cleanup`
        /// equals `resume` exactly as the recv / ask / sleep carriers do.
        cleanup: u32,
    },
    /// Sealed `join { }` construct — the wait-ALL sibling of
    /// [`Terminator::Select`]. Every branch is an actor-ask issued
    /// concurrently; codegen issues each ask (channel alloc + ask issue,
    /// reusing the `select` setup preamble), waits for ALL replies via
    /// `hew_reply_wait`, and materialises the `result` tuple from the
    /// per-branch reply slots in declaration order. Per HEW-SPEC-2026
    /// §4.11.2 a branch trap cancels the remaining branches and the trap
    /// propagates to the enclosing scope.
    ///
    /// The branch vector is non-empty (HIR enforces). `result` is the
    /// tuple local that converges the per-branch replies; `next` is the
    /// block reached after all replies land and the tuple is bound.
    Join {
        branches: Vec<JoinBranch>,
        result: Place,
        next: u32,
    },
    /// Stackless suspend point (R326/R327, W6.007). The carrier for a
    /// `coro.suspend` in a switched-resume LLVM coroutine: any function whose
    /// CFG contains this terminator is lowered by codegen as a
    /// `presplitcoroutine` (the coro prologue wraps the whole body, this
    /// terminator emits `llvm.coro.suspend` + its 3-way switch). It is the
    /// SUBSTRATE carrier the codegen boundary actually reads — distinct from the
    /// `ElaboratedMirFunction.coroutine` descriptor, which the
    /// `RawMirFunction`/`lower_function` codegen path does NOT consume (keying
    /// emission off that field would be a silent no-op — the Lane-B/R2 failure
    /// class). The codegen `lower_terminator` arm maps this directly to the
    /// `coro.rs` `emit_suspend` helper.
    ///
    /// On `coro.suspend` the switch routes control three ways: default returns
    /// to the caller/executor (the suspend edge — the worker is freed), case `0`
    /// (`resume`) continues the body where a `hew_cont_resume` advanced it, and
    /// case `1` (`cleanup`) tears the frame down where a `hew_cont_destroy` is
    /// abandoning it. `is_final` marks the terminal suspend (`coro.suspend(i1
    /// true)`), after which `coro.done` becomes true and the executor reclaims
    /// the frame.
    ///
    /// NO source construct constructs this terminator in production yet — the
    /// suspend SOURCE flip (`await`/blocking-`recv`/`scope`-join) lands paired
    /// with the readiness waker in a later slice (NEW-3). A test-only MIR builder
    /// constructs one for the synthetic substrate validation, so the suspend
    /// edge is production-capable but dormant (nothing can hang).
    Suspend {
        /// Block reached on case `0` of the coro switch — the body continues
        /// here when the executor resumed the continuation.
        resume: u32,
        /// Block reached on case `1` of the coro switch — the frame's cleanup /
        /// teardown when the continuation is abandoned (`coro.destroy`).
        cleanup: u32,
        /// `true` marks the terminal suspend (`coro.suspend(i1 true)`): after it
        /// `coro.done` is true and the executor reclaims the frame.
        is_final: bool,
    },
    /// Scope-deadline `scope { ... } after(d) { body }` with a NON-EMPTY timeout
    /// body, from a SUSPENDABLE caller (cut-task-sleep). The structured-deadline
    /// sibling of [`Terminator::SuspendingSleep`]: codegen arms a deadline on the
    /// process-global timer wheel carrying the parked continuation, the scope's
    /// children run, and the FIRST of {all children joined, deadline fired} wins.
    /// The join-wins resume edge runs the scope-complete path; the deadline-wins
    /// edge cancels the remaining children and routes to `timeout_body_block`
    /// (the lowered `after(...)` body), then converges on `resume`.
    ///
    /// Carries the same SUSPEND carrier the codegen boundary reads for
    /// `has_suspend` / `is_coroutine`. Emitted ONLY when the lowering function
    /// carries the execution context
    /// (`FunctionCallConv::carries_execution_context`); a contextless caller
    /// keeps the legacy per-deadline-thread `hew_task_scope_cancel_after_ns`
    /// path, and an EMPTY `after(...)` body keeps that path on both call-convs
    /// (no body to route to).
    SuspendingScopeDeadline {
        /// The scope token whose children the deadline races. Read by codegen to
        /// join the children and to arm the scope-cancel on the timeout edge.
        scope: Place,
        /// The deadline duration in milliseconds. Read by codegen and passed to
        /// the timer-wheel deadline schedule.
        duration_ms: Place,
        /// Block reached on the deadline-fired edge — the lowered `after(...)`
        /// body. Control flows from here to `resume` once the body completes.
        timeout_body_block: u32,
        /// Block reached on the coro switch resume edge (case 0) — the
        /// scope-complete path when all children joined before the deadline, and
        /// the convergence point the `timeout_body_block` falls through to.
        resume: u32,
        /// Block reached on the coro switch cleanup edge (case 1) — the frame's
        /// teardown when the parked continuation is abandoned (`coro.destroy`);
        /// the scheduled deadline is cancelled and the remaining children
        /// detached there.
        cleanup: u32,
    },
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
    /// `<rx>.recv()` — std/channel receive (NEW-4). The select-flavoured
    /// equivalent of an awaited `rx.recv()`: the arm registers a readiness
    /// poll on the channel core and, on winning, pops the queued item via the
    /// non-blocking `hew_channel_try_recv_layout` and binds `Option<T>`.
    /// `elem_ty` is the checker-resolved element type the winner edge
    /// synthesizes its layout witness from, mirroring
    /// [`Terminator::SuspendingChannelRecv`].
    ChannelRecv {
        receiver: Place,
        elem_ty: ResolvedTy,
    },
    /// `after <duration>` — timer.
    AfterTimer { duration: Place },
}

/// One branch of a sealed `join { }` terminator — a single actor-ask
/// issued concurrently with its siblings. Mirrors
/// [`SelectArmKind::ActorAsk`] (codegen reuses the same packed-payload
/// channel-alloc + ask-issue preamble) and additionally carries the
/// per-branch reply slot the wait-ALL loop writes into and the reply
/// value's resolved type. The branch's position in the
/// [`Terminator::Join`] `branches` vector is the element index of the
/// `result` tuple that this reply materialises.
#[derive(Debug, Clone, PartialEq)]
pub struct JoinBranch {
    pub actor: Place,
    pub method: String,
    pub args: Vec<Place>,
    pub msg_type: i32,
    /// Packed-payload place (one ptr + size threaded through
    /// `hew_actor_ask_with_channel`), built via `lower_actor_payload`.
    pub value: Place,
    /// Reply slot — codegen writes `hew_reply_wait`'s result here, then
    /// composes it into the `result` tuple at this branch's index.
    pub reply_dest: Place,
    /// The reply value's resolved type — sizes the reply slot and the
    /// tuple element it feeds.
    pub reply_ty: ResolvedTy,
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
/// `lower_duplex_half_extract` allocates these places: `.send_half()` /
/// `.recv_half()` consume the unified `DuplexHandle` and bind a fresh
/// `SendHalf` / `RecvHalf` whose carried `u32` is the half binding's own
/// backing local. Codegen reads the `Place` variant to select the
/// per-direction close ABI (`hew_duplex_close_half` with the `SendHalf=0`
/// / `RecvHalf=1` direction discriminant) at drop elaboration.
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
    /// whose `locals[N]` is the lambda-actor's `LambdaPid<Msg, Reply>`
    /// (the user-visible handle the surface call-syntax dispatches through).
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
    /// This substrate slice introduces the primitive but does not
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
}

/// Integer comparison predicate. Maps 1:1 to LLVM `IntPredicate`. The
/// signed-ness selector is intentional: Hew's spine treats `i64` as a
/// signed 64-bit integer, so the default cmp lowerings are signed
/// comparisons. For unsigned integer operands, `lower_binop_to_int_cmp`
/// selects the `Unsigned*` variants so that high-bit-set values compare
/// correctly (`0x8000… > 1` must be true for `u64`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpPred {
    Eq,
    NotEq,
    SignedLess,
    SignedLessEq,
    SignedGreater,
    SignedGreaterEq,
    /// Unsigned <: reinterprets both operands as unsigned. Used by
    /// ordering comparisons on unsigned integer types.
    UnsignedLess,
    /// Unsigned ≤: reinterprets both operands as unsigned. Used by
    /// ordering comparisons on unsigned integer types.
    UnsignedLessEq,
    /// Unsigned >: reinterprets both operands as unsigned. Used by
    /// ordering comparisons on unsigned integer types.
    UnsignedGreater,
    /// Unsigned ≥: reinterprets both operands as unsigned. Used by
    /// ordering comparisons on unsigned integer types AND by shift-range
    /// checking to catch both negative shift counts (which become large
    /// unsigned values) and counts ≥ bit-width in a single compare.
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
    /// Typed identity of the runtime entry. The C-ABI symbol string is
    /// DERIVED (`family.c_symbol()`) at every read — the string is born
    /// at the codegen FFI edge, never carried across the IR.
    family: hew_types::runtime_call::RuntimeCallFamily,
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
    /// The validated symbol is lifted into its typed [`RuntimeCallFamily`]
    /// through the catalog bijection; every allowlist symbol has a family
    /// (pinned by `every_allowlist_symbol_has_a_family`), so a `None` lift
    /// is a catalog/allowlist drift and refuses construction.
    ///
    /// [`RuntimeCallFamily`]: hew_types::runtime_call::RuntimeCallFamily
    ///
    /// # Errors
    ///
    /// Returns [`UnknownRuntimeSymbol`] when `symbol` is not recognised by
    /// `runtime_symbols::is_known_runtime_symbol` or has no catalog family.
    pub fn new(
        symbol: impl Into<String>,
        args: Vec<Place>,
        dest: Option<Place>,
    ) -> Result<Self, UnknownRuntimeSymbol> {
        let symbol = symbol.into();
        if !crate::runtime_symbols::is_known_runtime_symbol(&symbol) {
            return Err(UnknownRuntimeSymbol(symbol));
        }
        match hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(&symbol) {
            Some(family) => Ok(RuntimeCall { family, args, dest }),
            None => Err(UnknownRuntimeSymbol(symbol)),
        }
    }

    /// The validated C-ABI symbol name, derived from the typed family at
    /// the read edge (the catalog bijection is the sole authority).
    #[must_use]
    pub fn symbol(&self) -> &'static str {
        self.family.c_symbol()
    }

    /// The typed runtime-call family.
    #[must_use]
    pub fn family(&self) -> hew_types::runtime_call::RuntimeCallFamily {
        self.family
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
    /// `dest = ctx.next()` — generator consumption.
    ///
    /// `ctx` holds the heap companion pointer (`{ handle, env, out_drop_thunk,
    /// started, pending, out_value }`); `dest` is the `Option<yield_ty>` enum
    /// slot. Codegen drives the coro ramp (`hew_cont_resume` + `hew_cont_done`)
    /// and unboxes the companion's published `out_value` into `dest`: done →
    /// `None` (tag 1); else load the payload, write `Some` (tag 0) + value.
    /// The `ctx` handle is borrowed — its own scope-exit drop frees it via
    /// `hew_gen_coro_destroy`.
    GeneratorNext {
        dest: Place,
        ctx: Place,
        yield_ty: ResolvedTy,
    },
    /// Binary wire codec on a `#[wire]` struct.
    ///
    /// `Encode`: `operand` holds the value to serialize; `dest` is the `bytes`
    /// slot. Codegen calls `__hew_serialize_<key>(value_ptr, &out_len)` and
    /// wraps the returned malloc'd buffer into a refcounted `bytes` value.
    ///
    /// `Decode`: `operand` holds the `bytes` to read; `dest` is the `value_ty`
    /// slot. Codegen calls `__hew_deserialize_<key>(data, len, &struct_size)`
    /// and loads the reconstructed value out of the malloc'd result.
    ///
    /// `value_ty` is the wire-struct type. Codegen derives the thunk key from it
    /// via `mangle_resolved_ty` — the same encoder the actor message path uses —
    /// so a direct call and an actor send of the same type share one thunk. The
    /// `operand` is borrowed; the caller's binding stays live for its own
    /// scope-exit drop.
    WireCodec {
        dest: Place,
        operand: Place,
        direction: WireCodecDirection,
        value_ty: ResolvedTy,
    },
    /// Deep-clone a user record via `__hew_record_clone_inplace_<R>`.
    ///
    /// `dest` is the freshly-alloc'd destination Place (same type as `src`).
    /// Codegen emits:
    ///   1. `memcpy(dst_ptr, src_ptr, sizeof(R))` — `BitCopy` fields correct,
    ///      heap-pointer fields byte-aliased.
    ///   2. `i32 rc = __hew_record_clone_inplace_<R>(src_ptr, dst_ptr)` —
    ///      overwrites each owned-heap field in `dst` with a deep clone; on
    ///      failure drops already-cloned `dst` fields and rolls back.
    ///   3. Trap on `rc != 0` (fail-closed; no partial clone surviving).
    ///
    /// `record_name` is the canonical unqualified record name (sans module
    /// prefix), used to build the thunk symbol and index the clone descriptor.
    ///
    /// WASM-TODO(#2050): not yet lowered in sandbox emitter.
    RecordCloneInplace {
        dest: Place,
        src: Place,
        record_name: String,
    },
    /// Deep-clone a user enum via `__hew_enum_clone_inplace_<E>`.
    ///
    /// The enum twin of [`Instr::RecordCloneInplace`]. `dest` is the
    /// freshly-alloc'd destination Place (same tagged-union type as `src`).
    /// Codegen emits the identical three-step protocol, differing only in the
    /// synthesised helper symbol:
    ///   1. `memcpy(dst_ptr, src_ptr, sizeof(E))` — copies the tag and the
    ///      inactive/`BitCopy` payload bytes; owned-heap payload pointers in
    ///      the active variant are byte-aliased.
    ///   2. `i32 rc = __hew_enum_clone_inplace_<E>(src_ptr, dst_ptr)` —
    ///      tag-dispatches and overwrites only the active variant's owned-heap
    ///      payload fields in `dst` with deep clones; on failure rolls back.
    ///   3. Trap on `rc != 0` (fail-closed; no partial clone surviving).
    ///
    /// `enum_name` is the monomorphised tagged-union layout key (the bare name
    /// for a monomorphic enum, the mangled `Maybe$$i64` for a generic
    /// instantiation), used to build the thunk symbol and index the layout. The
    /// matching `__hew_enum_drop_inplace_<E>` is synthesised as a unit with the
    /// clone helper (clone/drop seeded together per key), so the scope-exit drop
    /// of `dest` stays symmetric with the clone — no leak, no double-free.
    ///
    /// WASM-TODO(#2050): not yet lowered in sandbox emitter (as `RecordCloneInplace`).
    EnumCloneInplace {
        dest: Place,
        src: Place,
        enum_name: String,
    },
    /// `dest = <src>` — load `src`, store into `dest`.
    Move { dest: Place, src: Place },
    /// Explicit checker-admitted numeric `as` cast.
    ///
    /// `from_ty` and `to_ty` are carried from HIR so codegen can choose the
    /// correct truncation, extension, signed/unsigned int-float conversion, or
    /// bool/integer canonicalization without re-deriving semantics from LLVM
    /// storage widths alone.
    NumericCast {
        dest: Place,
        src: Place,
        from_ty: ResolvedTy,
        to_ty: ResolvedTy,
    },
    /// Integer-to-integer saturating width conversion: `.saturating_as_<W>()`.
    ///
    /// `src` is clamped to `[W::MIN, W::MAX]` before narrowing. Both
    /// `from_ty` and `to_ty` are checker-admitted integers. Codegen lowers
    /// this to a compare-and-select clamp sequence followed by a truncation.
    SaturatingWidthCast {
        dest: Place,
        src: Place,
        from_ty: ResolvedTy,
        to_ty: ResolvedTy,
    },
    /// Exact fallible numeric conversion: `.try_to_<W>() -> Option<W>`.
    ///
    /// `dest` is the local enum slot for `Option<W>`. Codegen writes `Some`
    /// only when the source value round-trips exactly through the target type.
    TryWidthCast {
        dest: Place,
        src: Place,
        from_ty: ResolvedTy,
        to_ty: ResolvedTy,
        kind: TryConversionKind,
    },
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
        /// Storage mode for the env pointer stored in the pair. Derived from
        /// `ClosureEnvLayout::allocation_strategy()` at the literal site:
        /// `Stack` keeps the frame alloca address (Local escape class),
        /// `HeapBox` copies the materialised env into a `hew_dyn_box_alloc`
        /// box with a leading free-thunk slot (Escapes class with captures),
        /// and `Null` stores a null env (named-fn shims and capture-free
        /// escaping closures — the shim never loads the env).
        env_mode: ClosureEnvMode,
    },
    /// Load one captured field from a closure invoke shim's environment pointer.
    ///
    /// The field offsets are identical for stack and heap envs: a heap env's
    /// free-thunk slot lives BEFORE the pair's env pointer (at `env_ptr - 8`),
    /// so capture loads never see it.
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
    /// Store one value into a closure invoke shim's environment field — the
    /// write-back twin of [`Instr::ClosureEnvFieldLoad`]. Emitted when a closure
    /// body REASSIGNS a captured `var` (an inferred `BorrowMut` capture): the
    /// env owns the mutable scalar slot, so the store lands in the env field and
    /// accumulates across calls through the persistent env pointer. The caller's
    /// original binding is independent (captures are materialised through the
    /// env, never a surface reference). Restricted to `BitCopy` scalar fields —
    /// an owned captured field would need an overwrite-release on the prior
    /// value, which the lowering fails closed on.
    ClosureEnvFieldStore {
        /// Local holding the opaque env pointer parameter.
        env: Place,
        /// Named env-record type whose layout defines `field_offset`.
        env_ty: ResolvedTy,
        /// Capture field index in first-use order (identical to the matching
        /// load's offset).
        field_offset: FieldOffset,
        /// Place holding the value to store into the field.
        src: Place,
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
        /// `mailbox <N>;` declared capacity, mirrored from
        /// `ActorLayout.mailbox_capacity`. `Some(_)` (or `cycle_capable`, or
        /// `max_heap_bytes.is_some()`) routes the spawn through
        /// `hew_actor_spawn_opts`; `None` alongside the other two `None`/
        /// `false` uses the plain `hew_actor_spawn` path (unbounded, matching
        /// today's behaviour for actors that declare no mailbox clause).
        mailbox_capacity: Option<u32>,
        /// `overflow <policy>;` declared policy, mirrored from
        /// `ActorLayout.overflow_policy`. Codegen maps this to the runtime's
        /// `HewOverflowPolicy` i32 encoding by name (never an ordinal cast —
        /// the two enums' discriminant orders differ).
        overflow_policy: Option<hew_parser::ast::OverflowPolicy>,
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
    /// `drop_fn = Some(spec)` selects the release ritual through the typed
    /// [`DropFnSpec`]; `drop_fn = None` is a trivial drop (no side effect
    /// — `@linear` types whose move-checker proof is elsewhere, or value
    /// classes with no implicit close). The inkwell backend treats trivial
    /// drops as no-ops on the integer spine.
    Drop {
        place: Place,
        ty: ResolvedTy,
        drop_fn: Option<DropFnSpec>,
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
    /// guard.
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
    /// A `bytes` literal — `bytes[0x41, 0x42]` or `b"AB"`.
    ///
    /// Produced from `HirLiteral::Bytes`. Codegen emits an LLVM global
    /// constant holding the raw byte data, then calls
    /// `hew_bytes_from_static_raw(ptr, len, dst)` to build the
    /// refcounted `BytesTriple` at `dest`. The dest local carries
    /// `ResolvedTy::Bytes`.
    BytesLit {
        /// Raw byte content of the literal; elements are in [0, 255].
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
    /// Construct a closure environment record with an explicit per-field
    /// ownership manifest. Codegen lowers this identically to `RecordInit`; MIR
    /// drop/alias analysis reads the manifest to distinguish stack-env borrows
    /// from heap-env ownership transfers.
    ClosureEnvInit {
        /// Resolved synthetic env-record type.
        ty: ResolvedTy,
        /// Field sources plus capture ownership facts in env field order.
        fields: Vec<ClosureEnvFieldInit>,
        /// Destination place that receives the materialised env record.
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
    /// Release an owned field of a record value in-place, without copying it
    /// into an intermediate local first.
    ///
    /// Codegen lowers this to: GEP to the field slot → raw load (no
    /// `hew_string_clone` retain — unlike `RecordFieldLoad`) → call the
    /// `drop_fn` release symbol → null-store the field slot so any
    /// structurally-reachable second drop observes a null and short-circuits.
    ///
    /// ## Ownership invariant
    ///
    /// The field value must be a sole-owner reference (refcount == 1 for
    /// COW-heap scalars) at the point this instruction executes.  Emitting this
    /// instruction for a shared reference (refcount > 1) would decrement below
    /// expected — use `RecordFieldLoad` + `Drop` for shared/borrowed reads.
    ///
    /// ## Why not `RecordFieldLoad` + `Drop`?
    ///
    /// Two reasons, both pointing at `RecordFieldDrop` as the canonical
    /// in-place field destructor for single-pointer COW fields:
    ///
    /// 1. `RecordFieldLoad` retains `string` fields via `hew_string_clone` to
    ///    avoid aliasing the parent record's buffer. When the caller
    ///    immediately drops the loaded copy, the retain and the drop cancel out
    ///    (no-op), leaving the original field value in the record slot un-freed.
    ///    `RecordFieldDrop` bypasses the retain and drops the original directly.
    ///
    /// 2. For `Vec` / map / `Generator` fields `RecordFieldLoad` happens to skip
    ///    the retain TODAY, so `load` + `Drop` is currently correct for them —
    ///    but that is an incidental consequence of the not-yet-landed
    ///    retain-on-share spine. When that spine lands and `RecordFieldLoad`
    ///    starts retaining those types too, a `load` + `Drop` here would silently
    ///    regress to a leak. Routing every single-pointer COW field through
    ///    `RecordFieldDrop` (raw load, never retained) is robust against that
    ///    future change and additionally null-stores the freed slot.
    ///
    /// ## Scope
    ///
    /// Emitted only by functional-update lowering
    /// (`HirExprKind::StructInit { base: Some(_) }`) to release each overridden
    /// owned field of `base` before the new `RecordInit` is constructed.  The
    /// field domain is every SINGLE-POINTER COW-heap type — `string`, `Vec<T>`,
    /// `HashMap`, `HashSet`, and the `Generator` / `AsyncGenerator` companion
    /// handle (see `field_override_uses_record_field_drop` in `lower.rs`).  All
    /// three exit contexts (sync return, async cancel, actor shutdown) are
    /// covered: the instruction is positioned in the basic block that executes
    /// on the hot path, and any error path that bypasses the functional-update
    /// block keeps the original `base` alive (never drops a field that hasn't
    /// been loaded/consumed).  Soundness additionally rests on `..base`
    /// CONSUMING the base: the move-checker rejects any later read of `base`, so
    /// the released old value has no surviving reader.
    ///
    /// ## Type compatibility
    ///
    /// `ty` and `drop_fn` must be mutually consistent: `ty == ResolvedTy::String`
    /// pairs with `DropFnSpec::Release("hew_string_drop")`, a
    /// `Vec`-typed field with `Release("hew_vec_free")` / `"hew_vec_free_owned"`,
    /// etc.  Codegen validates congruence and fails closed on a mismatch.
    ///
    /// ## Bytes fields
    ///
    /// `bytes` fields are **not** lowered via this instruction.  `bytes` is a fat
    /// `{ ptr, len, cap }` triple, not a single pointer, so its destructor takes
    /// the whole by-value value and cannot be expressed as a single-slot GEP +
    /// pointer-load + release.  `bytes` overrides stay on the `RecordFieldLoad` +
    /// `Instr::Drop` path (which materialises the fat value into a temp).  Codegen
    /// fails closed if a non-pointer field slot ever reaches this instruction.
    RecordFieldDrop {
        /// The record value whose field is to be dropped.
        record: Place,
        /// 0-based index of the field within the record's declared field order.
        field_offset: FieldOffset,
        /// Hew type of the field being dropped.  Must match the field's declared
        /// type in the record; used by codegen to resolve the LLVM slot type and
        /// to validate `drop_fn` congruence.
        ty: ResolvedTy,
        /// The release ritual to call.  Must be `DropFnSpec::Release` carrying
        /// a known COW-heap release symbol (e.g. `"hew_string_drop"`).
        drop_fn: DropFnSpec,
    },
    /// Release one owned field of a record or tuple aggregate IN PLACE at its
    /// field address, with the release resolved from `ty` by codegen's
    /// address-based type-directed drop dispatcher (`emit_heap_slot_drop`,
    /// `hew-codegen-rs/src/llvm.rs`).  Carries **no `drop_fn`** — unlike
    /// `RecordFieldDrop`, whose leaf-COW contract requires a literal release
    /// symbol, this op is type-directed: codegen GEPs `base` to the field slot
    /// and dispatches on `ty`'s shape (`is_indirect_enum` FIRST — an indirect
    /// enum field frees through the recursive `__hew_indirect_enum_free_*`
    /// node path, never the inline `__hew_enum_drop_inplace_*` helper — then
    /// the dispatcher).
    ///
    /// ## Ownership invariant (precondition)
    ///
    /// The base aggregate must be the SOLE owner of the addressed field's
    /// value when this instruction executes.  Emitting it against a shared or
    /// escaped field frees a buffer another owner still references.
    ///
    /// ## Per-shape postcondition (type-correct, not blanket)
    ///
    /// - **Pointer leaves** (`string`): raw handle load (no retain) → release
    ///   → null-store of the field's pointer word; a structurally reachable
    ///   second drop observes null and short-circuits.
    /// - **Fat leaves** (`bytes`): the data-pointer word is null-stored after
    ///   release; len/cap words are inert once the data pointer is null.
    /// - **Indirect enums**: node-pointer load → recursive node free →
    ///   null-store of the field's node-pointer word.
    /// - **Inline composites** (record / tuple / inline-enum / fixed-array
    ///   fields): NO null-store exists and none is promised — the in-place
    ///   helpers free the composite's leaves and store nothing.  Idempotence
    ///   rests on exactly-once execution (straight-line pre-body emission,
    ///   never inside a `DropPlan`) plus exactly-once parent suppression: the
    ///   composite-drop provers exclude the base root directly (see below)
    ///   and the drop-plan verifier rejects an inline-composite `ty` whose
    ///   base still receives a composite in-place drop.
    ///
    /// ## Analysis classification
    ///
    /// An interior field operation: it USES `base`, creates NO dest, and
    /// creates NO alias.  It is by construction BOTH the field extraction and
    /// that field's release, so the record/tuple composite provers
    /// (`derive_owned_record_drop_allowed` /
    /// `derive_tuple_composite_drop_allowed` in `lower.rs`) exclude a
    /// candidate root this op addresses — the combined role the safety-drop
    /// temps' `field_binders ∩ release_owner_bases` intersection plays for
    /// the load+drop leaf path — and the enum composite prover exempts it
    /// from the owning-sink scan exactly like the interior `RecordFieldDrop`.
    ///
    /// ## Admission
    ///
    /// `ty` must be `string` (routed here because `RecordFieldLoad` /
    /// `TupleFieldLoad` retain string fields via `hew_string_clone`, so a
    /// load+`Drop` pair retain-cancels and leaks the original slot value) or
    /// a shape the shared fail-closed classifier admits
    /// (`field_drop_in_place_admissible` in `lower.rs`: record / tuple /
    /// fixed array / inline enum / indirect enum over registered layouts,
    /// mirroring `emit_heap_slot_drop`'s dispatch).  Anything else is a
    /// verify error — never a silent skip, never a wrong-ABI free.
    ///
    /// ## Scope
    ///
    /// `RecordFieldDrop` is NOT widened by this op: its leaf-COW congruence
    /// assert and functional-update contract stay narrow.  This op covers
    /// record AND tuple parents with one field-address selector and is the
    /// discharge primitive for partial ownership consumption (a pattern, an
    /// escape, or a lost delivery consuming part of an owned aggregate).
    FieldDropInPlace {
        /// The record- or tuple-typed local whose field is to be released.
        base: Place,
        /// Which field slot of `base` to release.
        field: FieldAddr,
        /// Hew type of the field being released.  Must match the field's
        /// declared type in the base aggregate; codegen resolves the release
        /// ritual from this type and fails closed on a shape it cannot place.
        ty: ResolvedTy,
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
    /// `dest = (lhs <pred> rhs)` on IEEE 754 floats.
    ///
    /// The result is written as an integer truth value, matching `IntCmp`.
    /// Codegen lowers the six source comparison predicates through ordered
    /// LLVM float predicates (`oeq`/`one`/`olt`/`ole`/`ogt`/`oge`).
    FloatCmp {
        dest: Place,
        pred: CmpPred,
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
    /// Typed carrier for a machine `emit(Event { ... })` expression inside a
    /// transition body, or an `entry {}`/`exit {}` lifecycle block.
    ///
    /// Despite the name, this is NOT a dead placeholder: codegen lowers it
    /// directly to a `hew_machine_emit_push` call against the thread-local
    /// emit queue (see `hew-codegen-rs/src/llvm.rs`). The name predates that
    /// wiring and is kept for continuity with the historical MIR dumps this
    /// variant already appears in.
    ///
    /// Non-unit events (`payload` non-empty) are fail-closed NYI in codegen
    /// pending a payload serialisation scheme; only unit `emit EventName {}`
    /// currently lowers.
    MachineEmitPlaceholder {
        /// Zero-based index into `HirMachineDecl.events` (declaration order).
        event_idx: usize,
        /// Lowered payload field places in source-declaration order.
        /// Empty for unit events (no payload).
        payload: Vec<Place>,
        /// Stable machine-type id (`hew_mir::lower::machine_emit_type_id`,
        /// `SipHasher13::new_with_keys(0, 0)` over the machine's unqualified
        /// type name — the single hashing authority, mirroring the actor
        /// `msg_type` precedent). Codegen transports this value verbatim as
        /// the `machine_id` argument to `hew_machine_emit_push`; it never
        /// re-derives it. Distinguishes emits from same-tag events declared
        /// on different machine types so `m.take_emits(ev)` cannot be
        /// misattributed across machine types.
        machine_emit_id: u64,
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
    /// `dest: i64` = removed-count of every queued `EmitEvent` matching
    /// `(machine_emit_id, event_tag)`.
    ///
    /// Lowers the user-visible `m.take_emits(event)` surface. Codegen calls
    /// `hew_machine_emit_take(queue, machine_id: u64, tag: u32) -> i64`
    /// against the thread-local emit queue (null queue = calling thread's
    /// owner, same resolution as the push/step-enter/step-exit ABI).
    /// `event_tag` is the `Place` produced by an `Instr::EnumTagLoad` on the
    /// `take_emits` argument (widened to whatever integer width that
    /// instruction's `dest` uses; codegen narrows/widens to the ABI's `u32`
    /// as needed).
    ///
    /// `machine_emit_id` is the SAME stable machine-type id
    /// (`machine_emit_type_id`) the corresponding `MachineEmitPlaceholder`
    /// carries — computed once in MIR, transported verbatim by codegen.
    MachineEmitTake {
        machine_emit_id: u64,
        event_tag: Place,
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

/// Field-address selector for [`Instr::FieldDropInPlace`]: one op covers both
/// record parents (addressed by declared-field offset) and tuple parents
/// (addressed by positional element index).  The selector must agree with the
/// base place's registered type — a `Record` address on a non-record local (or
/// `Tuple` on a non-tuple local) is a verify error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FieldAddr {
    /// Record parent: 0-based index into the record's declared field order.
    Record(FieldOffset),
    /// Tuple parent: 0-based positional element index.
    Tuple(u32),
}

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
///   an ancestor block in the CFG. Active in v0.5 since `8d878b8e`:
///   loop lowering constructs back-edge `Goto` terminators for `for`,
///   `while`, and `loop` bodies, and `is_back_edge_goto` detects them.
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
        /// Where the binding was introduced — the caret anchors here so the
        /// diagnostic points at the unconsumed binding, not the distant exit.
        bind_site: SiteId,
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
    /// W3.053 fail-closed gate: an owned handle (Generator / Stream / Sink /
    /// Duplex / Cancellation token / actor handle — any `AffineResource` /
    /// `CowHeap`-drop handle) was moved into a local tuple/record and then
    /// extracted-and-consumed back out of that aggregate by a downstream
    /// release-consumer, BUT the value-flow exclusion analysis
    /// ([`super::lower::derive_consumed_local_aggregate_member_bindings`])
    /// could not prove the source binding's standalone scope-exit drop and the
    /// consumer's release free the same context exactly once. The source
    /// binding therefore reaches codegen with more than one live free path for
    /// the same runtime context — a guaranteed double-free / use-after-free.
    ///
    /// Rather than emit the double-free, the elaborator refuses. This is the
    /// catch-all gate for the combinatorial aggregate-extraction shapes (re-
    /// aggregation, nested aggregates, multi-hop extraction) whose precise
    /// exact-once proof is deferred to v0.5.1: the invariant is "no owned handle
    /// reaches codegen with more than one live free path; unprovable → refuse".
    ///
    /// `name` is the source binding's name; `handle_ty` is its rendered type for
    /// the diagnostic.
    OwnedHandleAggregateDoubleFree {
        binding: BindingId,
        name: String,
        handle_ty: String,
        /// `false` = the handle is projected OUT of the aggregate (a
        /// `RecordFieldLoad` / element read — `let d = h.dq`); `true` = the
        /// handle field is OVERWRITTEN in place (a `RecordFieldStore` —
        /// `h.dq = src`), which drops the old handle on the floor (leak) and
        /// byte-copies `src` in with no move/null discipline (double-owner).
        /// Both are the SAME unprovable-exactly-once gate; the flag only
        /// selects the user-facing remediation wording.
        overwrite: bool,
    },
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

/// Per-field teardown class for a lambda-actor capture-env field,
/// carried on [`Terminator::MakeLambdaActor::env_field_drops`] in
/// declared field order. The MIR producer derives the class from the
/// capture's `ValueClass` / `CaptureKind` so codegen can synthesize the
/// env `state_drop_fn` (and the weak-self back-fill) without re-deriving
/// ownership from LLVM types. The runtime calls the synthesized dropper
/// exactly once after the dispatch loop stops (`lifecycle-symmetry`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LambdaEnvFieldDrop {
    /// `BitCopy` field — no teardown.
    None,
    /// Owned `string` field cloned into the env at spawn; dropped via
    /// `hew_string_drop`.
    String,
    /// Weak self-handle (`CaptureKind::Weak`): nulled at box time,
    /// back-filled with `hew_lambda_actor_downgrade(handle)` after
    /// construction, dropped via `hew_lambda_actor_weak_drop`.
    WeakSelfHandle,
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
    /// Sealed `join{}` exit — the wait-ALL sibling of [`ExitPath::Select`].
    /// Mirrors [`Terminator::Join`]; declared so the elaboration pass is
    /// exhaustive. Per-branch loser/cancel cleanup lives at the codegen
    /// join-dispatch site (cancel-rest on a branch trap), not in the
    /// function-wide `DropPlan`, exactly as for `Select`.
    Join {
        block: u32,
        next: u32,
    },
    /// Stackless suspend exit. Mirrors `Terminator::Suspend`; declared so the
    /// elaboration pass is exhaustive. The suspend point itself fires NO
    /// scope-exit drops — the live state is preserved in the coro frame across
    /// the suspend, and the frame's owned values are dropped by the single
    /// `cleanup` outline on `coro.destroy` (the `cleanup` edge), not at the
    /// suspend site. No source construct produces a `Terminator::Suspend` in
    /// production yet (the substrate is dormant; see that terminator's docs).
    Suspend {
        block: u32,
        resume: u32,
        cleanup: u32,
    },
}

/// Typed drop-ritual selector carried by [`Instr::Drop`] and [`ElabDrop`].
///
/// The drop world has exactly four identity domains, and each gets its
/// own arm so no consumer ever re-parses a string to recover which one
/// it is holding (`checker-authority` / `type-info-survival`):
///
/// - [`DropFnSpec::Runtime`] — the closed-set runtime close/release
///   rituals (Duplex/Stream/Sink/half-handles/lambda-actor/
///   `CancellationToken`). The C symbol is born at codegen from
///   [`RuntimeDropDescriptor::c_symbol`]; nothing downstream matches a
///   name string.
/// - [`DropFnSpec::Release`] — a cow-heap / fresh-value release C
///   symbol (`hew_string_drop`, `hew_bytes_drop`, `hew_vec_free`,
///   `hew_vec_free_owned`, `hew_hashmap_free_layout`,
///   `hew_hashset_free_layout`, `hew_gen_coro_destroy`). Selected by a closed
///   MIR-side type-shape authority (`generator_yield_drop_symbol`,
///   `drop_kind_for`'s cow tables) and congruence-checked against the
///   value's type in codegen before any call is emitted — the string is
///   a C-ABI name consumed at the declare edge, never a dispatch key.
/// - [`DropFnSpec::InPlace`] — a whole-value in-place composite release
///   for a registered heap-owning record/enum, routed to the synthesised
///   `__hew_record_drop_inplace_<R>` / `__hew_enum_drop_inplace_<E>`
///   thunk. No symbol travels in MIR: codegen derives the helper from
///   the drop's carried type (the same resolution the
///   `DropKind::RecordInPlace` / `DropKind::EnumInPlace` plan arms use),
///   so type↔helper congruence holds by construction. Emitted only by
///   the yield/recv release seam (`generator_yield_drop_symbol`'s
///   `WiredInPlace` verdict) for the per-yield producer/consumer copies
///   of a composite stream element.
/// - [`DropFnSpec::UserClose`] — a user `#[resource]` `close` method,
///   addressed by its generated `<Type>::<method>` symbol. Open set by
///   nature (the generated-object symbol IS the linker-edge name);
///   codegen resolves it against the declared function table and fails
///   closed when absent.
///
/// [`RuntimeDropDescriptor::c_symbol`]: hew_types::runtime_call::RuntimeDropDescriptor::c_symbol
#[derive(Debug, Clone, PartialEq)]
pub enum DropFnSpec {
    /// Closed-set runtime close/release ritual.
    Runtime(hew_types::runtime_call::RuntimeDropDescriptor),
    /// Cow-heap / fresh-value release C symbol (closed MIR-side
    /// selection; codegen validates type↔symbol congruence).
    Release(&'static str),
    /// In-place composite release through the synthesised record/enum
    /// drop thunk (helper derived from the drop's `ty` at codegen).
    InPlace(crate::ownership::InPlaceReleaseKind),
    /// User `#[resource]` close method (`<Type>::<method>` generated
    /// symbol — the open-set linker-edge arm).
    UserClose(String),
}

/// Ordered drop sequence for a single exit. Drops fire in
/// reverse-declaration (LIFO) order: the latest-bound `@resource`
/// drops first.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct DropPlan {
    pub drops: Vec<ElabDrop>,
}

/// A single elaborated drop op. Either a `@resource` drop selecting its
/// ritual through the typed [`DropFnSpec`] (`drop_fn = Some(spec)`), or
/// a trivial drop for a value class with no side effect
/// (`drop_fn = None`).
///
/// `kind` discriminates the M2 substrate's structural drop semantics:
/// generic `@resource` close, Duplex-handle close-both-directions,
/// half-handle close-one-direction, and lambda-actor stop-on-last.
#[derive(Debug, Clone, PartialEq)]
pub struct ElabDrop {
    pub place: Place,
    pub ty: ResolvedTy,
    pub drop_fn: Option<DropFnSpec>,
    /// Drop-kind discriminator. Distinguishes the structural close
    /// semantics that codegen (slice 5) and runtime (slice 4) need
    /// to honour. Generic `@resource` drops use `DropKind::Resource`
    /// (the existing path); M2-substrate drops use the specialised
    /// variants. Defaults to `Resource` so existing call sites that
    /// only populate the pre-M2 fields stay correct.
    pub kind: DropKind,
    /// Path-sensitive exactly-once gate for a non-idempotent user
    /// `#[resource]` close (#1933 / #1941).
    ///
    /// `None` for every idempotent / null-tolerant drop (Duplex, lambda,
    /// half-handle, `CowHeap`, record/enum/tuple in-place, dyn-trait) — those
    /// either refcount or null-after-free at runtime and never double-free
    /// on a shared (`MaybeConsumed`) control-flow join.
    ///
    /// `Some(flag)` for a `DropKind::Resource` whose `drop_fn` is a
    /// `DropFnSpec::UserClose`: the user `close` ritual is NOT runtime
    /// idempotent (`lower_drop_user_fn` unconditionally loads-calls-zeroes,
    /// and a zero payload is a legitimate field value, not a closed flag).
    /// `flag` is an `i64` local initialised to 0 at the binding's
    /// introduction and set to 1 at each `IntentKind::Consume` use site.
    /// Codegen gates the close on `flag == 0` so a resource reached at a
    /// `MaybeConsumed` join — Live on one predecessor, Consumed on the
    /// other — closes exactly once on the live path and is skipped on the
    /// already-consumed path. The drop-plan validator re-derives only
    /// `kind` (via the Place-driven `drop_kind_for` SSOT) and never
    /// inspects `guard`, so this runtime-gating annotation is orthogonal to
    /// the structural drop-kind contract.
    pub guard: Option<Place>,
}

/// Drop-kind discriminator for `ElabDrop`. Each variant pins a
/// distinct structural close-protocol contract that the runtime
/// (slice 4) and codegen (slice 5) layers must implement.
///
/// The pre-M2 path emits `DropKind::Resource` for every owned
/// `@resource` binding — the existing close-method dispatch through
/// `drop_fn = Some("Type::close")`. The M2 variants encode the
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
    /// `ElabDrop::drop_fn` stays `None` for this kind — the release protocol
    /// lives in the variant so the runtime-vs-user `resolve_drop_fn` dispatch
    /// (which only fires for `ElabDrop::drop_fn = Some(_)`) is not consulted and
    /// the `CowHeap` arm is a single, self-describing release call.
    ///
    /// The protocol is a typed [`CowHeapRelease`] — the heap-leaf identity
    /// folded with the `Vec<E>` element refinement — not a `&'static str`
    /// literal. Codegen resolves the C-ABI symbol from the carried fact
    /// (`CowHeapRelease::release_symbol`, declared `void(ptr)` via
    /// `get_or_declare_drop_helper`); because the fact is typed, an unknown or
    /// type-incongruent release symbol is unrepresentable, so the codegen
    /// congruence re-derivation the literal carrier required is gone.
    CowHeap {
        release: crate::ownership::CowHeapRelease,
    },
    /// owned-string-record — function-scope in-place drop of a stack-local user record whose
    /// direct fields are only `BitCopy` values plus one or more `string` fields.
    /// The record identity lives in the paired [`ElabDrop::ty`], so the kind can
    /// stay Copy while codegen resolves and calls the synthesized
    /// `__hew_record_drop_inplace_<Record>` helper. `ElabDrop::drop_fn` must be
    /// `None`; this path is not the `@resource` close-method dispatcher.
    ///
    /// This is deliberately narrower than [`DropKind::AggregateRecursive`]:
    /// named user records may also be `@resource` or otherwise unsupported on
    /// the value-class surface, so the MIR producer emits this kind only for
    /// the let-bound direct-string record-construction set.
    RecordInPlace,
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
    /// W5.020 — function-scope tag-aware drop of a heap-owning user-enum
    /// composite local (`Result<T, E>` / `Option<T>` / any user `enum`
    /// whose active variant transitively owns a heap allocation). The
    /// composite is a single tagged-union struct stored inline in its
    /// binding's alloca; the active variant's owned payload is the *only*
    /// heap owner, and it transfers to whichever binding owns the composite
    /// at scope exit. Codegen resolves the enum's mangled layout name from
    /// the paired [`ElabDrop::ty`] and calls the synthesised
    /// `__hew_enum_drop_inplace_<Enum>(*mut)` helper (the same tag-dispatch
    /// authority the embedded-field record/actor drop path uses), which
    /// drops only the active variant's owned fields (reverse order) and
    /// frees no wrapper — the enum is embedded in the caller's alloca.
    ///
    /// `ElabDrop::drop_fn` must be `None`; the helper symbol is derived from
    /// `ElabDrop::ty`, not from the runtime-vs-user `resolve_drop_fn`
    /// dispatch. The MIR elaborator emits this kind ONLY for a composite the
    /// fail-closed sole-owner derivation
    /// (`derive_enum_composite_sole_owner`) proves still owns its active
    /// payload at scope exit: a composite whose payload was moved out and
    /// escaped (a destructure binder read into an owning sink, a whole-value
    /// hand-off to another binding) is excluded so exactly one owner drops
    /// the buffer. A binding the prover does not positively clear leaks (as
    /// before W5.020); it never double-frees.
    EnumInPlace,
    /// W5.021 — function-scope per-element drop of a heap-owning **tuple**
    /// local carried by value (the tuple/record-of-owned-handles drop spine).
    /// The tuple is an inline struct in its binding's alloca; its owned
    /// elements (pointer handles like `Stream`/`Sink`, or heap leaves
    /// `string`/`bytes`/`Vec`/nested owned record/enum) are the heap owners,
    /// transferred to whichever binding owns the tuple at scope exit. Codegen
    /// resolves the structural tuple key from the paired [`ElabDrop::ty`] and
    /// calls the synthesised `__hew_tuple_drop_inplace_<key>(*mut)` helper —
    /// the same per-field GEP-and-drop authority the record drop path uses
    /// (`emit_aggregate_drop_inplace_body`) — which drops each owned element in
    /// reverse declaration order and frees no wrapper (the tuple is embedded).
    ///
    /// `ElabDrop::drop_fn` must be `None`; the helper symbol is derived from
    /// `ElabDrop::ty`. The MIR elaborator emits this kind ONLY for a tuple the
    /// fail-closed sole-owner derivation (`derive_tuple_composite_drop_allowed`)
    /// proves still owns its members at scope exit: a tuple whose elements were
    /// moved out (the `__tuple_N` destructure temp) or whose whole value escaped
    /// (returned) is excluded so exactly one owner drops each member. A binding
    /// the prover does not positively clear leaks; it never double-frees.
    TupleInPlace,
    /// Escaping-closure pair drop (the closure env heap-lifetime contract).
    /// The dropped value is the two-pointer closure pair `{ fn_ptr, env_ptr }`
    /// held in a stack `Local`. The env pointer is, by construction at every
    /// pair-producing site, one of:
    ///
    /// - **null** — a named-fn shim pair or a capture-free escaping closure;
    ///   nothing is owned, the drop is a no-op.
    /// - **a heap env** produced by the `Instr::MakeClosure` `HeapBox` mode:
    ///   a `hew_dyn_box_alloc` box whose slot at `env_ptr - 8` holds the
    ///   per-closure free thunk (`__hew_closure_env_free_<shim>`, synthesised
    ///   by codegen with the box's static size/align — the same
    ///   size-align-agreement convention as the dyn-Trait vtable slot 0).
    ///
    /// Codegen loads the env pointer from pair field 1, skips on null,
    /// otherwise calls the thunk loaded from `env_ptr - 8` and null-stores
    /// the pair's env slot (`raii-null-after-move`). A stack env (Local
    /// escape class) NEVER earns this kind: the MIR elaborator admits a
    /// binding only through the fail-closed `closure_pair_drop_allowed`
    /// derivation, whose producing shapes (heap-mode literal, call result,
    /// admitted-binding rebind) exclude stack pairs by construction.
    ///
    /// `ElabDrop::drop_fn` must be `None`; the thunk is carried in-band by
    /// the env box itself, never resolved through the close-method dispatch.
    ClosurePair,
    /// Function-scope drop of an `indirect enum` (spec §3.7.4) owned local.
    /// An indirect-enum value is a single heap pointer to a tagged-union
    /// node (`{ tag, payload }`), so the binding's alloca holds a `ptr`
    /// (see `is_indirect_enum` / the codegen heap-allocation prologue). The
    /// node's payload may itself hold child `indirect enum` pointers (a
    /// recursive `Node(Tree, Tree)`), so the release is a tag-dispatched
    /// recursive free: codegen loads the node pointer, null-checks it, frees
    /// each owned child node first (by reading the active variant's tag and
    /// recursing through the payload's indirect-enum fields), then
    /// `hew_dealloc`s the node and null-stores the slot
    /// (`raii-null-after-move`). The node's size/align come from the same
    /// `is_indirect_enum` layout path the allocation prologue uses, so the
    /// free can never select a mismatched (size, align) for the dealloc.
    ///
    /// `ElabDrop::drop_fn` must be `None`; the variant is self-describing
    /// (the release symbol is the fixed `hew_dealloc`, the recursion is
    /// derived from the paired [`ElabDrop::ty`]'s registered enum layout),
    /// so the runtime-vs-user `resolve_drop_fn` dispatch is never consulted.
    ///
    /// The MIR elaborator emits this kind ONLY for an indirect-enum local
    /// the fail-closed sole-owner derivation
    /// (`derive_indirect_enum_drop_allowed`) proves still solely owns its
    /// heap node at scope exit: a binding that is a destructure/projection
    /// alias of a still-live parent node, that escapes (returned, moved into
    /// an aggregate, passed by value to a callee — a borrow that does NOT
    /// transfer ownership), or that is consumed/maybe-consumed on a path is
    /// excluded so the node is freed by exactly one owner. A binding the
    /// prover does not positively clear leaks (as before this kind); it
    /// never double-frees.
    IndirectEnum,
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

/// Storage mode for the env pointer an `Instr::MakeClosure` stores into the
/// closure pair. Selected at the literal site from
/// `ClosureEnvLayout::allocation_strategy()`:
///
/// - `Stack` — the env record stays a frame alloca and the pair stores its
///   address (Local escape class; the closure provably never outlives the
///   introducing scope). No drop obligation.
/// - `HeapBox` — the materialised env is copied into a `hew_dyn_box_alloc`
///   heap box (Escapes class with at least one capture). The box layout is
///   `[free_thunk: ptr][captures...]` and the pair stores the address of the
///   captures region, so `ClosureEnvFieldLoad` offsets are identical to the
///   stack layout. The last owner of the pair frees the box exactly once via
///   the thunk (`DropKind::ClosurePair`).
/// - `Null` — the pair stores a null env pointer (named-fn invoke shims and
///   capture-free escaping closures; the shim performs zero env loads). A
///   null env is the universal "nothing owned" signal the pair-drop protocol
///   checks before dereferencing, so producers MUST use `Null` rather than a
///   dummy frame address for any pair that can cross a frame boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureEnvMode {
    /// Frame-alloca env address (Local escape class).
    Stack,
    /// `hew_dyn_box_alloc` box with a leading free-thunk slot (Escapes).
    HeapBox,
    /// Null env pointer (named-fn shims, capture-free escaping closures).
    Null,
}

/// Storage strategy recorded on a closure-env field at materialisation time.
/// Mirrors `closure_env::AllocationStrategy` without coupling the serialisable
/// MIR model to that helper module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureEnvAllocation {
    Stack,
    Heap,
    ScopeOwned,
}

/// Ownership verdict for one captured field in a closure env.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureEnvFieldOwnership {
    /// The env field is a borrow/alias; the source binding keeps its drop.
    BorrowsOnly,
    /// The env field takes over the source binding's existing owner.
    OwnsMoved,
    /// Reserved for the future retain/deep-clone capture path.
    OwnsClonedOrRetained,
}

/// One field of a [`Instr::ClosureEnvInit`] ownership manifest.
#[derive(Debug, Clone, PartialEq)]
pub struct ClosureEnvFieldInit {
    pub field_offset: FieldOffset,
    pub src: Place,
    pub source_binding: Option<BindingId>,
    pub capture_mode: hew_types::ClosureCaptureMode,
    pub allocation: ClosureEnvAllocation,
    pub ownership: ClosureEnvFieldOwnership,
    pub source_is_parameter: bool,
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
    /// B1 — an owned single-owner operand (`@resource` / `@linear`) was placed
    /// (aliased) into an aggregate constructor (tuple). PURELY a move-checker
    /// marker: it records the alias so a SUBSEQUENT use of the source binding is
    /// flagged `UseAfterConsume` at CHECK time (the explicit-consume-after-move
    /// double-free, e.g. `(s, r); s.close()`). It deliberately does NOT consume
    /// the binding for DROP purposes — the source keeps its drop obligation and
    /// the alias/escape-scan drop machinery (W3.053 + the per-aggregate
    /// composite-drop derivations) decides the single owner exactly as before.
    /// Modelling it as a `Use { Consume }` instead would suppress the source
    /// drop and break that machinery (it would silently turn the W3.053
    /// fail-closed double-free refusals into leaks).
    AggregateAlias {
        binding: BindingId,
        name: String,
        site: SiteId,
        ty: ResolvedTy,
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
        /// The binding's introduction site — diagnostic caret anchor.
        bind_site: SiteId,
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
    /// A remote dispatch (`RemotePid<T>` ask/tell) resolved to a
    /// multi-parameter receive handler. Multi-arg sends pack into an
    /// anonymous record whose bytes may carry heap pointers — bytes the
    /// cross-node codec is not seeded to serialize (the codec carries
    /// single-param message types only; packed-args seeding is the
    /// cross-node payload serialization's positive path). A
    /// single-value remote payload delivered to a handler that unpacks
    /// the full packed record would read out of bounds on the receiving
    /// node, so the boundary fails closed at compile time instead.
    RemotePayloadUnsupported {
        actor: String,
        handler: String,
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
    /// A named argument at an actor spawn site does not match the accepted
    /// initialization surface: `init()` parameters when an explicit init block
    /// exists, otherwise state fields.
    InvalidActorSpawnArgument {
        actor: String,
        argument: String,
        site: SiteId,
    },
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
    /// `overflow coalesce(key)` parses and type-checks, but codegen has
    /// no coalesce key-function ABI slice to thread through the FFI boundary
    /// yet. Threading a bare `Coalesce` tag through `HewOverflowPolicy`
    /// without a key function would silently miscarry the declared policy —
    /// exactly the "declared-and-accepted surface silently discarded" class
    /// this fence closes — so lowering fails closed here with an honest NYI
    /// diagnostic instead of a silent remap to another policy.
    MailboxOverflowCoalesceNotYetImplemented { actor: String, key_field: String },
    /// An actor reached MIR layout lowering with `receive fn` handlers but no
    /// protocol descriptor, so no handler can be assigned its real
    /// message-kind discriminant. Emitting the `i32::MAX` sentinel instead
    /// collapses every handler onto one tag: with two or more handlers LLVM
    /// rejects the dispatch switch ("Duplicate integer as switch case"), and
    /// with exactly one the program compiles but carries a wrong wire
    /// discriminant — silent protocol corruption. Fail closed here; the
    /// sentinel rows exist only to keep the MIR shape well-formed behind this
    /// hard error. Reachable when a lowering path fails to attach the
    /// checker's descriptor (the checker's own `ActorProtocolCollision`
    /// reject empties the descriptor too, but that path already carries its
    /// user-facing diagnostic).
    ActorProtocolDescriptorMissing { actor: String, handler_count: usize },
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
    /// W3.053 fail-closed gate (projected from
    /// [`MirCheck::OwnedHandleAggregateDoubleFree`]): an owned handle
    /// (Generator / Stream / Sink / Duplex / Cancellation token / actor handle,
    /// or a user `#[resource] #[opaque]` handle) is projected OUT of a local
    /// aggregate (`let d = h.dq`) — or, when `overwrite` is set, OVERWRITTEN in
    /// place within one (`h.dq = src`) — in a shape whose exactly-once free the
    /// drop analysis cannot prove. The compiler refuses rather than emitting a
    /// double-free / leak; full aggregate field-handle support lands in v0.5.1.
    /// `name` is the source/record binding; `handle_ty` is its rendered type;
    /// `overwrite` selects the remediation wording (move-out vs. reassignment).
    OwnedHandleAggregateExtractionUnsupported {
        name: String,
        handle_ty: String,
        overwrite: bool,
    },
    /// Sole-owner closure-pair ingress gate: a function value flowing into an
    /// owning container position (record field, Vec element, machine payload,
    /// tuple element) is a borrow — a parameter, a collection-element or
    /// record-field read, or a binding that never owned its pair. Storing it
    /// would byte-copy the `{fn_ptr, env_ptr}` pair and give the container a
    /// second owner of one closure environment (double free at scope exit).
    /// Closure pairs have no clone path, so only owned pairs (a closure
    /// literal, a fresh fn-typed call result, or an owning binding) may enter
    /// an owning position. `name` is the source binding when the operand is a
    /// binding read; expression-shaped operands carry `None`.
    ClosurePairBorrowedStore { name: Option<String>, site: SiteId },
    /// Defence-in-depth gate at `materialize_closure_env`: a fn-closure
    /// capture env contains a `Duplex<S,R>` (lambda-actor handle) field.
    ///
    /// The authoritative rejection is at the checker boundary
    /// (`TypeErrorKind::ClosureCapturesDuplexHandle` in `check_call`). If MIR
    /// ever sees a Duplex capture it means the checker gate was bypassed —
    /// likely by a new source form that doesn't route through `check_call`.
    /// MIR fails closed here rather than silently misrouting to
    /// `hew_duplex_send` (the wrong runtime ABI for lambda-actor sends).
    ///
    /// When the full env-materialization protocol for Duplex captures is
    /// implemented, remove this guard AND the checker gate in `check_call`.
    ClosureCapturesDuplexHandle { name: String, site: SiteId },
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

#[cfg(test)]
mod heap_owning_tests {
    use super::*;
    use hew_types::BuiltinType;

    fn generator_ty() -> ResolvedTy {
        // `Generator<i64, ()>` — bit-copy generic args, but the handle itself
        // owns a coro frame + heap companion.
        ResolvedTy::named_builtin(
            "Generator",
            BuiltinType::Generator,
            vec![ResolvedTy::I64, ResolvedTy::Unit],
        )
    }

    fn async_generator_ty() -> ResolvedTy {
        ResolvedTy::named_builtin(
            "AsyncGenerator",
            BuiltinType::AsyncGenerator,
            vec![ResolvedTy::I64],
        )
    }

    #[test]
    fn generator_handle_is_heap_owning_despite_bitcopy_args() {
        // Regression: before this arm a `Generator<i64, ()>` was classified
        // non-heap-owning, so a composite carrying it never dropped — leaking
        // the coro frame + heap companion.
        assert!(ty_contains_heap_owning(&generator_ty(), &[]));
    }

    #[test]
    fn async_generator_handle_is_heap_owning() {
        assert!(ty_contains_heap_owning(&async_generator_ty(), &[]));
    }

    #[test]
    fn cancellation_token_is_heap_owning() {
        assert!(ty_contains_heap_owning(&ResolvedTy::CancellationToken, &[]));
    }

    #[test]
    fn tuple_with_generator_member_is_heap_owning() {
        // The exact Leak 2 shape: `(Generator<i64, ()>, i64)`. The tuple-recursion
        // arm must see the generator member as a heap-owning leaf so the caller's
        // tuple member-drop fires `hew_gen_coro_destroy`.
        let tuple = ResolvedTy::Tuple(vec![generator_ty(), ResolvedTy::I64]);
        assert!(ty_contains_heap_owning(&tuple, &[]));
    }

    #[test]
    fn tuple_of_bitcopy_only_is_not_heap_owning() {
        // Guard against over-broad classification: a plain `(i64, bool)` must
        // still be non-heap-owning.
        let tuple = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]);
        assert!(!ty_contains_heap_owning(&tuple, &[]));
    }

    // ── canonical authority: record-aware + shared leaf set ─────────────────

    fn vec_i64() -> ResolvedTy {
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64])
    }

    /// `record_field_orders` map with one record `Boxed { payload: Vec<i64> }`.
    fn boxed_record_orders() -> HashMap<String, Vec<(String, ResolvedTy)>> {
        let mut orders = HashMap::new();
        orders.insert(
            "Boxed".to_string(),
            vec![("payload".to_string(), vec_i64())],
        );
        orders
    }

    #[test]
    fn record_blind_entry_misses_heap_record_field() {
        // The `[EnumLayout]`-only entry point cannot reach a user record's
        // fields — `Boxed` resolves to no enum layout, so it answers `false`.
        // This is WHY DIV-1 existed: any site that only had `enum_layouts` was
        // record-blind. (Pins the documented limitation of the back-compat
        // wrapper, NOT a regression — the record-aware path below is the fix.)
        let boxed = ResolvedTy::named_user("Boxed", vec![]);
        assert!(!ty_contains_heap_owning(&boxed, &[]));
    }

    #[test]
    fn record_aware_authority_sees_heap_record_field() {
        // DIV-1 fix: the canonical authority over a record-aware adapter sees
        // `Boxed`'s `payload: Vec<i64>` field and classifies `Boxed` heap-owning.
        let boxed = ResolvedTy::named_user("Boxed", vec![]);
        assert!(ty_owns_heap_mir(&boxed, &boxed_record_orders(), &[]));
    }

    #[test]
    fn record_aware_authority_sees_heap_record_field_inside_tuple() {
        // The exact DIV-1 leak shape proven at runtime: `(Boxed, i64)` where
        // `Boxed { payload: Vec<i64> }`. A record-blind walker answers `false`
        // and the tuple member-drop never frees the inner Vec.
        let boxed = ResolvedTy::named_user("Boxed", vec![]);
        let tuple = ResolvedTy::Tuple(vec![boxed, ResolvedTy::I64]);
        assert!(ty_owns_heap_mir(&tuple, &boxed_record_orders(), &[]));
        // ... while a bit-copy record field stays non-owning.
        let mut plain = HashMap::new();
        plain.insert(
            "Point".to_string(),
            vec![
                ("x".to_string(), ResolvedTy::I64),
                ("y".to_string(), ResolvedTy::I64),
            ],
        );
        let point = ResolvedTy::named_user("Point", vec![]);
        let plain_tuple = ResolvedTy::Tuple(vec![point, ResolvedTy::I64]);
        assert!(!ty_owns_heap_mir(&plain_tuple, &plain, &[]));
    }

    #[test]
    fn record_carrying_cancellation_token_owns_heap() {
        // DIV-2 + DIV-1 combined: a record field of `CancellationToken` must be
        // seen as heap-owning through the canonical authority (the leaf set is
        // shared, the record field is consulted).
        let mut orders = HashMap::new();
        orders.insert(
            "Holder".to_string(),
            vec![("token".to_string(), ResolvedTy::CancellationToken)],
        );
        let holder = ResolvedTy::named_user("Holder", vec![]);
        assert!(ty_owns_heap_mir(&holder, &orders, &[]));
    }

    #[test]
    fn nested_generic_instantiation_is_not_a_recursion_false_positive() {
        // `Wrap<Wrap<i64>>` where `Wrap<T> { v: T }` is all-BitCopy: the field
        // of `Wrap<Wrap<i64>>` is `Wrap<i64>`, whose field is `i64`. The
        // recursion guard must DISTINGUISH the two instantiations of `Wrap` — a
        // short-name-only visit key would collide them, falsely trip the guard,
        // and force the fail-closed `true`, mis-classifying the all-BitCopy Vec
        // element as heap-owning (it then builds an owned-descriptor Vec while
        // get/iter route to the layout path → runtime abort).
        let mut orders = HashMap::new();
        // `record_field_resolved_tys` stores SUBSTITUTED fields per mono key.
        orders.insert(
            "Wrap$$Wrap$$i64".to_string(),
            vec![(
                "v".to_string(),
                ResolvedTy::named_user("Wrap", vec![ResolvedTy::I64]),
            )],
        );
        orders.insert(
            "Wrap$$i64".to_string(),
            vec![("v".to_string(), ResolvedTy::I64)],
        );
        let nested = ResolvedTy::named_user(
            "Wrap",
            vec![ResolvedTy::named_user("Wrap", vec![ResolvedTy::I64])],
        );
        assert!(
            !ty_owns_heap_mir(&nested, &orders, &[]),
            "an all-BitCopy nested generic must NOT be classified heap-owning"
        );
        // Sanity: the SAME shape but with a heap leaf at the bottom IS owning.
        let mut heap_orders = HashMap::new();
        heap_orders.insert(
            "Wrap$$Wrap$$string".to_string(),
            vec![(
                "v".to_string(),
                ResolvedTy::named_user("Wrap", vec![ResolvedTy::String]),
            )],
        );
        heap_orders.insert(
            "Wrap$$string".to_string(),
            vec![("v".to_string(), ResolvedTy::String)],
        );
        let nested_heap = ResolvedTy::named_user(
            "Wrap",
            vec![ResolvedTy::named_user("Wrap", vec![ResolvedTy::String])],
        );
        assert!(
            ty_owns_heap_mir(&nested_heap, &heap_orders, &[]),
            "a nested generic whose innermost field owns heap must be classified heap-owning"
        );
    }

    #[test]
    fn token_and_generator_share_the_one_leaf_set() {
        // DIV-2: the single leaf set classifies the affine runtime handles as
        // heap-owning regardless of which adapter supplies layouts. Every
        // consumer (MIR drop elaborator, codegen owned-vec) routes through this
        // set, so none can silently disagree about a bare handle.
        let layouts = MirHeapLayouts {
            record_field_orders: &HashMap::new(),
            enum_layouts: &[],
        };
        assert!(ty_owns_heap(&ResolvedTy::CancellationToken, &layouts));
        assert!(ty_owns_heap(&generator_ty(), &layouts));
        assert!(ty_owns_heap(&async_generator_ty(), &layouts));
        // The exact DIV-2 element shape: `(Generator<i64,()>, i64)`.
        let pair = ResolvedTy::Tuple(vec![generator_ty(), ResolvedTy::I64]);
        assert!(ty_owns_heap(&pair, &layouts));
    }

    // ── ty_contains_unclonable_opaque (round-4 transitive authority) ────────

    #[test]
    fn bare_opaque_handle_is_unclonable() {
        let ty = ResolvedTy::named_opaque("Value", vec![]);
        assert!(ty_contains_unclonable_opaque(&ty, &[], &[]));
    }

    #[test]
    fn vec_of_opaque_is_unclonable_via_type_arg() {
        let ty = ResolvedTy::named_user("Vec", vec![ResolvedTy::named_opaque("Value", vec![])]);
        assert!(ty_contains_unclonable_opaque(&ty, &[], &[]));
    }

    #[test]
    fn deeply_nested_option_vec_opaque_is_unclonable() {
        // `Option<Vec<json.Value>>` — the opaque is reached through two layers
        // of generic type-args.
        let vec_op = ResolvedTy::named_user("Vec", vec![ResolvedTy::named_opaque("Value", vec![])]);
        let ty = ResolvedTy::named_user("Option", vec![vec_op]);
        assert!(ty_contains_unclonable_opaque(&ty, &[], &[]));
    }

    #[test]
    fn tuple_with_opaque_member_is_unclonable() {
        let tuple = ResolvedTy::Tuple(vec![
            ResolvedTy::I64,
            ResolvedTy::named_opaque("Value", vec![]),
        ]);
        assert!(ty_contains_unclonable_opaque(&tuple, &[], &[]));
    }

    #[test]
    fn record_with_opaque_field_is_unclonable() {
        // `Holder { v: json.Value }` referenced by name; the authority looks up
        // the record layout and recurses into its fields.
        let records = vec![RecordLayout {
            name: "Holder".to_string(),
            field_tys: vec![ResolvedTy::named_opaque("Value", vec![])],
            field_names: vec![],
        }];
        let ty = ResolvedTy::named_user("Holder", vec![]);
        assert!(ty_contains_unclonable_opaque(&ty, &records, &[]));
    }

    #[test]
    fn enum_with_opaque_payload_is_unclonable() {
        let enums = vec![EnumLayout {
            name: "Wrap".to_string(),
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "V".to_string(),
                field_tys: vec![ResolvedTy::named_opaque("Value", vec![])],
                field_names: vec![],
            }],
            is_indirect: false,
        }];
        let ty = ResolvedTy::named_user("Wrap", vec![]);
        assert!(ty_contains_unclonable_opaque(&ty, &[], &enums));
    }

    #[test]
    fn non_opaque_composites_are_clonable() {
        // Negative controls: nothing opaque anywhere → false. Pins that the
        // authority does not over-fire on plain heap-owning or bitcopy shapes.
        let vec_str = ResolvedTy::named_user("Vec", vec![ResolvedTy::String]);
        assert!(!ty_contains_unclonable_opaque(&vec_str, &[], &[]));
        let tuple = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]);
        assert!(!ty_contains_unclonable_opaque(&tuple, &[], &[]));
        // A user record `Value` (is_opaque: false) that merely shares the short
        // name with an opaque handle is clonable — keyed on identity, not name.
        let records = vec![RecordLayout {
            name: "Value".to_string(),
            field_tys: vec![ResolvedTy::I64],
            field_names: vec![],
        }];
        let user_value = ResolvedTy::named_user("Value", vec![]);
        assert!(!ty_contains_unclonable_opaque(&user_value, &records, &[]));
    }

    // ── ty_contains_unclonable_opaque_with_names (name fallback for a `Named`
    //    that reaches MIR without the `is_opaque` discriminator) ──────────────

    #[test]
    fn name_fallback_flags_unstamped_opaque_named() {
        // A captured local's binding type can name an `#[opaque]` decl while
        // carrying `is_opaque: false` (the discriminator is not stamped on every
        // synthesised `Named`). The discriminator-only entry point therefore
        // MISSES it (would shallow-copy the handle) — the name fallback catches
        // it from the module's opaque decl-name set.
        let unstamped = ResolvedTy::named_user("Dq", vec![]);
        let names = vec!["Dq".to_string()];
        assert!(
            !ty_contains_unclonable_opaque(&unstamped, &[], &[]),
            "discriminator-only path keys on identity, so an un-stamped Named is missed"
        );
        assert!(
            ty_contains_unclonable_opaque_with_names(&unstamped, &[], &[], &names),
            "name fallback flags an un-stamped Named whose name is an opaque decl"
        );
    }

    #[test]
    fn name_fallback_does_not_misflag_same_named_record() {
        // Collision guard: a `#[copy]` value record that merely SHARES a short
        // name with an opaque decl must recurse through its own (clean) fields
        // and stay admissible. The name fallback is ordered AFTER record-layout
        // resolution, mirroring `state_clone::classify_named`, so the record
        // wins and the bare-name match never fires.
        let records = vec![RecordLayout {
            name: "Dq".to_string(),
            field_tys: vec![ResolvedTy::I64],
            field_names: vec![],
        }];
        let user_record = ResolvedTy::named_user("Dq", vec![]);
        let names = vec!["Dq".to_string()];
        assert!(
            !ty_contains_unclonable_opaque_with_names(&user_record, &records, &[], &names),
            "a user record sharing a short name with an opaque decl is not mis-flagged"
        );
    }

    #[test]
    fn name_fallback_flags_qualified_opaque_shadowed_by_same_short_record() {
        // Security regression (UAF / double-free): a QUALIFIED opaque handle
        // whose `is_opaque` discriminator was cleared on the way to MIR
        // (`json.Value`, `is_opaque: false`) must STILL fail closed even when a
        // clean `#[copy]` user record shares its SHORT name (`Value`). The
        // record-layout lookup matches by short name, so the bare-short record
        // resolves first and — under the prior ordering — SUPPRESSED the opaque
        // name-fallback, admitting the qualified handle into the flat-copied
        // generator env (shallow-aliasing the caller's handle → double-free /
        // use-after-free at generator teardown). The gate must distinguish a
        // qualified opaque shadowed by a same-short layout (REJECT) from a plain
        // same-short value record (ADMIT): a short-name-only layout match must
        // NOT suppress the fallback for a qualified name; only an EXACT
        // (qualified) layout-name match resolves the type to a user layout.
        let records = vec![RecordLayout {
            name: "Value".to_string(),
            field_tys: vec![ResolvedTy::I64],
            field_names: vec![],
        }];
        // Qualified, discriminator-cleared opaque handle (`is_opaque: false`),
        // exactly the shape a captured local's binding type reaches MIR with.
        let qualified_opaque = ResolvedTy::named_user("json.Value", vec![]);

        // Opaque decl-name set carries the QUALIFIED (use-site) form.
        let qualified_names = vec!["json.Value".to_string()];
        assert!(
            ty_contains_unclonable_opaque_with_names(
                &qualified_opaque,
                &records,
                &[],
                &qualified_names
            ),
            "a qualified opaque handle must fail closed even when a clean user \
             record shares its short name (the same-short collision must not \
             suppress the opaque name-fallback)"
        );

        // ...and when the opaque decl-name set carries the SHORT (decl) form.
        let short_names = vec!["Value".to_string()];
        assert!(
            ty_contains_unclonable_opaque_with_names(
                &qualified_opaque,
                &records,
                &[],
                &short_names
            ),
            "a qualified opaque handle must also fail closed when the opaque \
             decl-name set carries the bare short name"
        );

        // Negative control preserved: the same-short value record itself,
        // referenced UNqualified, stays admissible — the exact-name record
        // resolves it, so the fallback never fires.
        let user_record = ResolvedTy::named_user("Value", vec![]);
        assert!(
            !ty_contains_unclonable_opaque_with_names(
                &user_record,
                &records,
                &[],
                &qualified_names
            ),
            "an unqualified same-short value record must NOT be mis-flagged"
        );
    }

    #[test]
    fn name_fallback_flags_qualified_opaque_generic_shadowed_by_same_short_enum() {
        // Security regression, GENERIC-ENUM variant (UAF / double-free): a
        // QUALIFIED generic opaque handle whose `is_opaque` discriminator was
        // cleared on the way to MIR (`a.Wrapper<i64>`, `is_opaque: false`) must
        // STILL fail closed even when a clean same-short GENERIC ENUM layout
        // (`Wrapper$$i64`, no opaque payload — a different module's type) exists.
        //
        // `find_enum_layout` resolves `a.Wrapper<i64>` to the bare-key
        // `Wrapper$$i64` by SHORT name (it shortens the spine before mangling),
        // so the prior gate — which treated ANY generic-enum structural match as
        // a resolved user layout via `!args.is_empty()` — marked the type
        // resolved, suppressed the opaque name-fallback, and admitted the
        // qualified handle into the flat-copied generator env (shallow-aliasing
        // the caller's handle → double-free / use-after-free at teardown).
        //
        // A short-name-only structural match is NOT a resolution for a qualified
        // name: only an EXACT (qualified) layout-name match may suppress the
        // fallback. The clean `i64` payload means neither the arg-recursion nor
        // the variant-recursion rejects — proving it is the STEP-5 fallback that
        // must fire, not an incidental nested-opaque hit.
        let clean_same_short_enum = EnumLayout {
            name: hew_hir::mangle("Wrapper", &[ResolvedTy::I64]),
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Full".to_string(),
                field_tys: vec![ResolvedTy::I64],
                field_names: vec![],
            }],
            is_indirect: false,
        };
        let enums = vec![clean_same_short_enum];

        // Qualified, discriminator-cleared generic opaque handle — the shape a
        // captured local's binding type reaches MIR with.
        let qualified_opaque = ResolvedTy::named_user("a.Wrapper", vec![ResolvedTy::I64]);

        // Opaque decl-name set carries the QUALIFIED (use-site) form...
        let qualified_names = vec!["a.Wrapper".to_string()];
        assert!(
            ty_contains_unclonable_opaque_with_names(
                &qualified_opaque,
                &[],
                &enums,
                &qualified_names
            ),
            "a qualified generic opaque handle must fail closed even when a clean \
             same-short generic enum layout exists (the structural same-short \
             match must not suppress the opaque name-fallback)"
        );

        // ...and when the opaque decl-name set carries the bare SHORT name.
        let short_names = vec!["Wrapper".to_string()];
        assert!(
            ty_contains_unclonable_opaque_with_names(&qualified_opaque, &[], &enums, &short_names),
            "a qualified generic opaque handle must also fail closed when the \
             opaque decl-name set carries the bare short name"
        );

        // Negative control: the SAME generic-enum shape, but NOT in the opaque
        // set, still ADMITS. The fallback only flags names IN the opaque set, so
        // requiring an EXACT match (dropping the `!args.is_empty()` escape) does
        // not over-reject a genuine generic enum capture.
        assert!(
            !ty_contains_unclonable_opaque_with_names(&qualified_opaque, &[], &enums, &[]),
            "a genuine generic enum (not in the opaque set) must still be admitted"
        );
    }

    #[test]
    fn name_fallback_flags_opaque_nested_in_bitcopy_record() {
        // The transitive case the generator-env gate actually faces: a record
        // whose field is an un-stamped opaque handle. The record itself has no
        // layout-entry opacity signal — the field resolves through the name
        // fallback during the record-field recursion.
        let records = vec![RecordLayout {
            name: "Pair".to_string(),
            field_tys: vec![ResolvedTy::named_user("Dq", vec![]), ResolvedTy::I64],
            field_names: vec![],
        }];
        let pair = ResolvedTy::named_user("Pair", vec![]);
        let names = vec!["Dq".to_string()];
        assert!(
            ty_contains_unclonable_opaque_with_names(&pair, &records, &[], &names),
            "an un-stamped opaque field inside a record fails closed via the name fallback"
        );
        // Empty name-set ⇒ discriminator-only ⇒ the hidden opaque is missed.
        assert!(!ty_contains_unclonable_opaque(&pair, &records, &[]));
    }

    // ── Qualified-payload layout-key symmetry (C1) ──────────────────────────
    //
    // The MIR ownership/drop authorities probe `enum_layouts` by mangling the
    // outer name + type-arg spine. A generic enum instantiated through an
    // import-use site carries a MODULE-QUALIFIED payload in its args
    // (`Slot<lmonobox.Box>`), while the layout is REGISTERED under the bare
    // spine (`Slot$$Box`). If a probe mangles the raw qualified spine
    // (`Slot$$lmonobox.Box`) it diverges from the registered key, the lookup
    // falls through, and the ownership/drop/clone decision silently wrong-answers
    // (no member-drop synthesised → leak, or no opaque fail-closed → unsound
    // clone). `find_enum_layout` shortens the spine so the probe matches.

    /// A generic enum registered under its bare-arg key resolves when probed
    /// with a QUALIFIED payload, so its opaque variant payload still fails
    /// closed for the actor-state clone direction.
    #[test]
    fn qualified_payload_enum_resolves_for_opaque_fail_closed() {
        // Registered as `Slot$$Box` (bare outer, bare arg) — exactly what
        // `EnumLayoutRegistry::insert` / `layout_mono` emit.
        let registered_key = hew_hir::mangle("Slot", &[ResolvedTy::named_user("Box", vec![])]);
        let enums = vec![EnumLayout {
            name: registered_key,
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Full".to_string(),
                // The substituted payload carries the unclonable opaque handle.
                field_tys: vec![ResolvedTy::named_opaque("Value", vec![])],
                field_names: vec![],
            }],
            is_indirect: false,
        }];
        // The PROBE type carries the qualified payload as the import-use MIR does:
        // `Slot<lmonobox.Box>`. A raw `mangle("Slot", [lmonobox.Box])` would key
        // `Slot$$lmonobox.Box` and MISS the registered `Slot$$Box`.
        let qualified =
            ResolvedTy::named_user("Slot", vec![ResolvedTy::named_user("lmonobox.Box", vec![])]);
        assert!(
            ty_contains_unclonable_opaque(&qualified, &[], &enums),
            "qualified-payload enum must resolve to its bare-key layout and \
             fail closed on the opaque variant payload"
        );
    }

    /// The same divergence for the heap-owning (drop-synthesis) authority: a
    /// generic enum carrying a `string` variant payload must be seen as
    /// heap-owning when probed with a qualified payload spine.
    #[test]
    fn qualified_payload_enum_resolves_for_heap_owning_drop() {
        let registered_key = hew_hir::mangle("Slot", &[ResolvedTy::named_user("Box", vec![])]);
        let enums = vec![EnumLayout {
            name: registered_key,
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Full".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            }],
            is_indirect: false,
        }];
        let qualified =
            ResolvedTy::named_user("Slot", vec![ResolvedTy::named_user("lmonobox.Box", vec![])]);
        assert!(
            ty_contains_heap_owning(&qualified, &enums),
            "qualified-payload enum must resolve to its bare-key layout and be \
             classified heap-owning so its member-drop fires"
        );
    }

    /// A NESTED qualified payload (`Slot<Vec<lmonobox.Box>>`) shortens at every
    /// depth, so the inner qualifier cannot leak into the key.
    #[test]
    fn nested_qualified_payload_enum_resolves() {
        let registered_key = hew_hir::mangle(
            "Slot",
            &[ResolvedTy::named_user(
                "Vec",
                vec![ResolvedTy::named_user("Box", vec![])],
            )],
        );
        let enums = vec![EnumLayout {
            name: registered_key,
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Full".to_string(),
                field_tys: vec![ResolvedTy::named_opaque("Value", vec![])],
                field_names: vec![],
            }],
            is_indirect: false,
        }];
        let qualified = ResolvedTy::named_user(
            "Slot",
            vec![ResolvedTy::named_user(
                "Vec",
                vec![ResolvedTy::named_user("lmonobox.Box", vec![])],
            )],
        );
        assert!(ty_contains_unclonable_opaque(&qualified, &[], &enums));
    }

    /// Negative control: a probe whose payload genuinely differs from the
    /// registered instantiation must NOT resolve. Pins that `find_enum_layout`
    /// is shortening qualifiers, not collapsing distinct payloads.
    #[test]
    fn distinct_payload_enum_does_not_resolve() {
        let registered_key = hew_hir::mangle("Slot", &[ResolvedTy::I64]);
        let enums = vec![EnumLayout {
            name: registered_key,
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Full".to_string(),
                field_tys: vec![ResolvedTy::named_opaque("Value", vec![])],
                field_names: vec![],
            }],
            is_indirect: false,
        }];
        // Probe `Slot<lmonobox.Box>` (→ `Slot$$Box`) against a layout registered
        // as `Slot$$i64`: no match, so the opaque payload is NOT reached.
        let qualified =
            ResolvedTy::named_user("Slot", vec![ResolvedTy::named_user("lmonobox.Box", vec![])]);
        assert!(!ty_contains_unclonable_opaque(&qualified, &[], &enums));
    }

    /// Structural guard: every generic layout key built in this module must
    /// route through either `find_enum_layout` (enum side) or `record_lookup_keys`
    /// (record side — the key authority both `find_record_layout` and the
    /// `MirHeapLayouts` adapter delegate to). A bare `mangle(.., args)` call
    /// outside those functions re-opens the C1 qualified-spine miss class
    /// (incorrect shortening of the type-arg spine → probe misses the registered
    /// key). This self-scan of the source keeps the two-authority invariant from
    /// silently eroding.
    #[test]
    fn mangle_feeding_layout_lookup_is_centralised() {
        let src = include_str!("model.rs");
        // The authorised `mangle(` calls in this module's non-test code live in
        // `find_enum_layout` (1 call: the `mangled` key) and `record_lookup_keys`
        // (2 calls: full + short mangle). Strip the test module (which
        // legitimately mangles bare-arg fixture keys) before scanning.
        let prod = src
            .split("#[cfg(test)]")
            .next()
            .expect("model.rs has a non-test prefix");
        // Count only real call sites: drop comment lines (`//` / `///`) so the
        // doc-comment mentions of `mangle(` in this module's prose don't inflate
        // the count. The 3 remaining calls live in `find_enum_layout` (1) and
        // `record_lookup_keys` (2).
        let mangle_calls = prod
            .lines()
            .filter(|line| !line.trim_start().starts_with("//"))
            .map(|line| line.matches("mangle(").count())
            .sum::<usize>();
        assert_eq!(
            mangle_calls, 3,
            "exactly three `mangle(` calls are allowed in model.rs non-test code: \
             one in `find_enum_layout` and two in `record_lookup_keys` (the two \
             authorised layout-key functions; `find_record_layout` and the \
             `MirHeapLayouts` adapter both delegate to `record_lookup_keys`). A \
             new bare call outside those functions feeds the C1 qualified-spine \
             miss class — route through one of the authority functions instead. \
             Found {mangle_calls}."
        );
    }
}

#[cfg(test)]
mod suspend_terminator_tests {
    //! The MIR `Terminator::Suspend` carrier — the substrate suspend point
    //! (R326/R327, W6.007). These tests pin the CFG-edge + operand contract the
    //! codegen boundary and the dataflow passes rely on, so a future edit cannot
    //! silently regress the carrier's semantics (the Lane-B/R2 silent-no-op
    //! failure class).

    use super::*;

    fn suspend_block(id: u32, resume: u32, cleanup: u32, is_final: bool) -> BasicBlock {
        BasicBlock {
            id,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Suspend {
                resume,
                cleanup,
                is_final,
            },
        }
    }

    /// The suspend point's in-CFG successors are exactly its resume + cleanup
    /// arms; the default suspend-return edge exits the function (returns to the
    /// executor) and is NOT a CFG successor, mirroring `Return`.
    #[test]
    fn suspend_successors_are_resume_and_cleanup_only() {
        let block = suspend_block(0, 1, 2, false);
        assert_eq!(block.successors(), vec![1, 2]);
    }

    /// A final suspend has the same successor shape (the `is_final` flag changes
    /// the emitted `coro.suspend(i1 true)`, not the CFG edges).
    #[test]
    fn final_suspend_keeps_resume_and_cleanup_successors() {
        let block = suspend_block(3, 4, 5, true);
        assert_eq!(block.successors(), vec![4, 5]);
    }

    /// The suspend point reads no `Place` operands — the value channel is the
    /// explicit coro frame out-pointer (the spike-pinned null-promise
    /// constraint), not a `Place`. A drop/escape pass that mis-classified a
    /// suspend as carrying an operand would over- or under-drop frame state.
    #[test]
    fn suspend_reads_no_source_places() {
        let term = Terminator::Suspend {
            resume: 1,
            cleanup: 2,
            is_final: false,
        };
        // A bare `Suspend` with NO side-table entry (a generator / synthetic
        // suspend) reads nothing across the block edge.
        assert!(crate::lower::terminator_source_places(&term, None).is_empty());
    }

    // ── `Terminator::Select` CFG-successor contract ──────────────────────────
    //
    // The canonical `BasicBlock::successors()` must return every arm `body_block`
    // plus `next` for a `Select` terminator. Before the fix it returned only
    // `next`, which made every arm body unreachable to any pass that walks the
    // canonical successor API — including liveness, dataflow, and the
    // duplex-split cross-block checker (the fail-open soundness bug).

    fn select_arm(body_block: u32) -> SelectArm {
        SelectArm {
            body_block,
            kind: SelectArmKind::AfterTimer {
                duration: Place::Local(0),
            },
            binding: None,
        }
    }

    fn select_block(id: u32, arm_body_blocks: &[u32], next: u32) -> BasicBlock {
        BasicBlock {
            id,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Select {
                arms: arm_body_blocks.iter().copied().map(select_arm).collect(),
                next,
            },
        }
    }

    /// A two-arm `Select` must include BOTH arm body blocks plus `next` in
    /// its successor set. The pre-fix code returned only `[next]`, which was
    /// the root cause of the duplex double-consume fail-open.
    #[test]
    fn select_successors_include_arm_bodies_and_next() {
        let block = select_block(0, &[1, 2], 3);
        let succs = block.successors();
        assert!(
            succs.contains(&1),
            "arm body 1 must be a successor; got {succs:?}"
        );
        assert!(
            succs.contains(&2),
            "arm body 2 must be a successor; got {succs:?}"
        );
        assert!(
            succs.contains(&3),
            "next (join) block 3 must be a successor; got {succs:?}"
        );
        assert_eq!(succs.len(), 3, "exactly arm_0, arm_1, next; got {succs:?}");
    }

    /// A single-arm `Select` must include the arm body plus `next`.
    #[test]
    fn select_single_arm_includes_body_and_next() {
        let block = select_block(0, &[5], 9);
        let succs = block.successors();
        assert_eq!(succs, vec![5, 9]);
    }

    /// Regression pin: BEFORE the fix the old arm returned only `[next]` —
    /// arm bodies were absent. This test would have FAILED pre-fix, proving
    /// the fix is non-trivial (not vacuously true).
    #[test]
    fn select_successors_do_not_collapse_to_next_only() {
        let block = select_block(0, &[10, 11], 20);
        let succs = block.successors();
        // The pre-fix code returned `vec![20]` — this assertion was false.
        assert_ne!(
            succs,
            vec![20],
            "successors must NOT collapse to [next] only (pre-fix regression)"
        );
    }
}
