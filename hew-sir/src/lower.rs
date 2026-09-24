#[path = "lower_wire.rs"]
mod wire;

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt;
use std::ops::Range;

use hew_parser::ast::Span;

#[path = "lower_projection.rs"]
mod projection;
#[path = "lower_writable.rs"]
mod writable;

#[path = "lower_actor.rs"]
mod actor;
#[path = "lower_actor_codec.rs"]
mod actor_codec;
#[path = "lower_remote_actor.rs"]
mod remote_actor;

#[path = "lower_supervisor.rs"]
mod supervisor;

#[path = "lower_binding.rs"]
mod binding;

#[path = "lower_defer.rs"]
mod deferred;

#[path = "lower_var_self.rs"]
mod var_self;

#[path = "lower_suspend.rs"]
mod suspend;

#[path = "lower_native_io.rs"]
mod native_io;

#[path = "lower_entry.rs"]
mod entry;

#[path = "lower_generators.rs"]
mod generators;

#[path = "lower_tasks.rs"]
mod tasks;

#[path = "lower_select.rs"]
mod select;

#[path = "lower_match.rs"]
mod match_lowering;

#[path = "lower_numeric.rs"]
mod numeric;

#[path = "lower_loops.rs"]
mod loops;

use hew_hir::{
    BindingId, HirBinding, HirBlock, HirDestructureField, HirDestructureSelector, HirExpr,
    HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmtKind, IntentKind, ResolvedRef,
};
use hew_types::runtime_call::collection_type_arguments;
use hew_types::{
    CallTarget, DefId, EntryExitAction, ResolvedTy, TypeCheckOutput, TypeFactService,
    TypeInstanceKey,
};
use writable::WritableRoot;

use crate::ownership::{
    AggregateFieldRecipe, Binding, BytesLiteralId, OwnKind, StringLiteralId, TypeFactTable,
};
use crate::{
    AggregateShapeId, AggregateShapeRef, BindingTarget, BlockArg, BlockId, CallResult, CallUnwind,
    CallableId, CallableInstance, CheckedFailure, Edge, FunctionSourceOrigin, GenericTemplateId,
    OpId, Operand, PlaceId, PlaceOrigin, Provenance, SemAbiParam, SemAggregateField,
    SemAggregateShape, SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction,
    SemGenericTemplate, SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature, SemTerminator,
    SemVariant, SemVariantArm, SemVariantField, SemVariantKind, SemVariantShape, SirInstanceKey,
    ValueDef, ValueId, VariantShapeId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SirLoweringStatus {
    Lowered,
    /// A generic HIR definition is a canonical template, not a SIR body. Its
    /// closed instances are materialized on demand by the SIR instance
    /// service and reported separately through `callable_statuses`.
    GenericTemplate {
        instances: usize,
        failed_instances: usize,
    },
    Unsupported {
        reason: String,
        /// The declaring function's extent, when one is known. A diagnostic
        /// renders this as its provoking construct's span (#3384); it is the
        /// enclosing declaration, not the exact expression that refused,
        /// since refusal reasons form deep inside body lowering with no span
        /// of their own to report.
        span: Option<Span>,
    },
    /// The declaration has an admitted SIR callable header but the entry
    /// closure never reached it, so no body was attempted.
    ///
    /// This is distinct from [`Self::Unsupported`]: nothing is known about
    /// whether the body would lower, and nothing needed to be. Reporting it as
    /// its own outcome keeps "outside the current semantic surface" from
    /// absorbing "irrelevant to this program".
    NotReached,
}

/// Which bodies a lowering run demands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SirLoweringDemand {
    /// Demand-driven from the module's resolved entry callable: the strict
    /// `--sir-lower` compile route. A declaration the entry never reaches is
    /// reported [`SirLoweringStatus::NotReached`] and is never admitted a
    /// header, so it costs the module no signature, shape or type-fact row.
    ///
    /// Library, test, and export consumers add their resolved declarations
    /// through [`lower_module_with_roots`]; they do not broaden this demand by
    /// scanning names or requesting every callable.
    Entry,
    /// Demand every monomorphic declaration, entry or not: the coverage
    /// inventory. A refused header is reported
    /// [`SirLoweringStatus::Unsupported`] with the refusal reason, because the
    /// question asked is "would SIR take this body", not "does this program
    /// need it".
    EveryCallable,
}

/// Why an exact checker-owned declaration could not seed SIR body demand.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SirRootSelectionError {
    /// The exact declaration requested by the caller.
    pub declaration: DefId,
    /// A stable, human-readable explanation suitable for driver diagnostics.
    pub reason: String,
}

impl fmt::Display for SirRootSelectionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "SIR root `{}` was refused: {}",
            self.declaration.full_path(),
            self.reason
        )
    }
}

impl std::error::Error for SirRootSelectionError {}

/// The lowering outcome for one HIR function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct SirSourceStatus {
    /// Checker-owned identity of the declaration; the key every consumer
    /// joins on. HIR item order is not a key: an item list zipped positionally
    /// against a status list drifts the moment one side filters.
    pub declaration: DefId,
    /// The emitted HIR name, for display only.
    pub name: String,
    pub status: SirLoweringStatus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoweredModule {
    pub module: SemModule,
    /// One status per HIR function, in source order. A generic HIR definition
    /// is reported as [`SirLoweringStatus::GenericTemplate`] because it never
    /// becomes an abstract SIR body.
    pub statuses: Vec<SirSourceStatus>,
    /// Status for every concrete callable header, in `CallableId` order.
    /// This lets strict component selection diagnose a failed concrete generic
    /// instance without pretending that its generic HIR template was a body.
    pub callable_statuses: Vec<(CallableId, SirLoweringStatus)>,
}

impl LoweredModule {
    /// Return the lowering result for one concrete callable header.
    #[must_use]
    pub fn status_for_callable(&self, callable: CallableId) -> Option<&SirLoweringStatus> {
        self.callable_statuses
            .get(usize::try_from(callable.0).ok()?)
            .filter(|(candidate, _)| *candidate == callable)
            .map(|(_, status)| status)
    }

    /// Return the source-level lowering result for one HIR function
    /// declaration.
    #[must_use]
    pub fn status_for_declaration(&self, declaration: &DefId) -> Option<&SirLoweringStatus> {
        self.statuses
            .iter()
            .find(|status| status.declaration == *declaration)
            .map(|status| &status.status)
    }
}

/// Lower the SIR callables the program actually needs.
///
/// Both halves are demand-driven from the module's entry callable: resolved
/// call edges mint the headers they name and queue their bodies. A
/// declaration the entry cannot reach is reported
/// [`SirLoweringStatus::NotReached`] and costs nothing — no header, no
/// signature, no aggregate shape, no type-fact row — so the prelude's own
/// declarations stay out of a program that never calls them, and an
/// unsupported body in an unrelated corner of the module can neither consume
/// lowering effort nor be mistaken for a fact about this program.
///
/// A module with no entry callable is not a program: it lowers no bodies.
#[must_use]
pub fn lower_module(module: &HirModule, facts: &TypeCheckOutput) -> LoweredModule {
    lower_module_with_demand(module, facts, SirLoweringDemand::Entry)
}

/// Lower SIR bodies under an explicit demand policy.
///
/// [`SirLoweringDemand::Entry`] is [`lower_module`].
/// [`SirLoweringDemand::EveryCallable`] asks for every admitted header's body
/// so a coverage inventory can say, per declaration, whether SIR takes it.
/// The strict compile route never asks for that demand, so nothing about it
/// changes here.
#[must_use]
pub fn lower_module_with_demand(
    module: &HirModule,
    facts: &TypeCheckOutput,
    demand: SirLoweringDemand,
) -> LoweredModule {
    // The HIR monomorphisation registry remains deliberately unused here.
    // SIR discovers concrete direct-user instances from each resolved call's
    // `SiteId -> call_site_type_args` fact, applies the enclosing semantic
    // substitution, and creates its own closed instance worklist.
    let mut service = InstanceService::new(module, facts);
    match demand {
        SirLoweringDemand::Entry => service.request_entry(),
        SirLoweringDemand::EveryCallable => service.request_every_callable(),
    }
    service.lower_pending();

    service.finish()
}

/// Lower the resolved entry and a caller-selected set of declaration roots.
///
/// Root identities must come from the checker. SIR admits only exact,
/// monomorphic HIR function declarations, deduplicates them deterministically,
/// and follows resolved call edges from that seed set. Generic templates need
/// a concrete call-site specialization and therefore cannot be selected by
/// their declaration alone.
///
/// Every refused request is returned with its original [`DefId`]. No body is
/// published when root selection fails.
///
/// # Errors
///
/// Returns every requested declaration that is absent, ineligible for a SIR
/// callable header, or a generic template without a concrete specialization.
pub fn lower_module_with_roots(
    module: &HirModule,
    facts: &TypeCheckOutput,
    roots: &[DefId],
) -> Result<LoweredModule, Vec<SirRootSelectionError>> {
    let mut service = InstanceService::new(module, facts);
    service.request_roots(roots)?;
    service.request_entry();
    service.lower_pending();
    Ok(service.finish())
}

impl InstanceService<'_> {
    fn finish(self) -> LoweredModule {
        let statuses = self
            .module
            .items
            .iter()
            .filter_map(|item| match item {
                HirItem::Function(function) => Some(SirSourceStatus {
                    declaration: function.declaration.clone(),
                    name: function.name.clone(),
                    status: self.source_status(function),
                }),
                _ => None,
            })
            .collect();
        let callable_statuses = self.callable_statuses();
        LoweredModule {
            module: self.into_module(),
            statuses,
            callable_statuses,
        }
    }
}

/// Deterministic SIR view of the HIR direct-call projection.
///
/// HIR owns the resolved direct-call projection. SIR assigns its private
/// emitted namespace once from that projection; it never reconstructs a
/// symbol from a declaration's presentation spelling.
#[derive(Debug, Clone)]
struct GenericTemplate<'a> {
    function: &'a HirFn,
    source_origin: FunctionSourceOrigin,
    symbol: String,
    id: GenericTemplateId,
}

/// A canonical type substitution applied while lowering one concrete SIR
/// instance.  It is purely semantic: it rewrites `ResolvedTy` facts but never
/// asks for a size, alignment, ABI class, or layout.
#[derive(Debug, Clone, Default)]
struct TypeSubstitution {
    params: Vec<String>,
    args: Vec<ResolvedTy>,
}

impl TypeSubstitution {
    fn empty() -> Self {
        Self::default()
    }

    fn for_instance(function: &HirFn, args: &[ResolvedTy]) -> Result<Self, String> {
        if function.type_params.len() != args.len() {
            return Err(format!(
                "generic template `{}` expects {} type argument(s), SIR received {}",
                function.declaration.full_path(),
                function.type_params.len(),
                args.len()
            ));
        }
        Ok(Self {
            params: function.type_params.clone(),
            args: args.to_vec(),
        })
    }

    fn apply(&self, ty: &ResolvedTy) -> ResolvedTy {
        hew_hir::substitute_type_params(ty, &self.params, &self.args)
    }
}

#[derive(Debug, Clone)]
struct CallableTable<'a> {
    callables: Vec<SemCallable>,
    generic_templates: Vec<SemGenericTemplate>,
    root_unit_callables: Vec<CallableId>,
    entry_callable: Option<CallableId>,
    entry_exit_plan: Option<hew_types::EntryExitPlan>,
    monomorphic_by_declaration: HashMap<DefId, CallableId>,
    /// Every monomorphic HIR function a call could name, with the emitted
    /// symbol reserved for it. A header is minted from this only when demand
    /// reaches the declaration; until then the declaration costs the module
    /// nothing — no signature, no shapes, no type-fact rows.
    admissible: HashMap<DefId, AdmissibleFn<'a>>,
    /// `admissible` in the deterministic order the coverage inventory walks.
    admissible_order: Vec<DefId>,
    templates: HashMap<DefId, GenericTemplate<'a>>,
    functions_by_item: HashMap<hew_hir::ItemId, &'a HirFn>,
    /// HIR's structured `(declaring trait, self type, trait method) →
    /// implementation` index. A generic template publishes
    /// `CallTarget::StaticTraitMethod` because no concrete implementation
    /// exists at the template; this stage substitutes the receiver type, so
    /// this is where the implementation is selected.
    trait_impls: HashMap<hew_hir::dispatch::TraitImplKey, hew_hir::dispatch::TraitImplMethodEntry>,
    /// Why a declaration was refused a SIR callable header, keyed by the
    /// declaration a call would name.
    ///
    /// A refused declaration has no header, so no resolved call can reach it
    /// and no body is ever demanded of it. The reason is therefore reported at
    /// the call site that needed it — where it is actionable — rather than as
    /// a standing complaint about every unused declaration in the module.
    ineligible: HashMap<DefId, String>,
}

/// One monomorphic HIR function a resolved call may name, before SIR mints a
/// header for it.
#[derive(Debug, Clone)]
struct AdmissibleFn<'a> {
    function: &'a HirFn,
    /// The emitted symbol reserved for this declaration by
    /// [`CallableTable::from_hir`].
    symbol: String,
}

impl<'a> CallableTable<'a> {
    #[allow(
        clippy::too_many_lines,
        reason = "one deterministic HIR collection pass keeps monomorphic and generic callable admission auditable together"
    )]
    fn from_hir(module: &'a HirModule) -> Self {
        let direct_symbols = hew_hir::dispatch::build_direct_call_symbol_index(&module.items);
        let mut pending = Vec::new();
        let mut ineligible = HashMap::new();
        let mut templates = HashMap::new();
        let mut generic_templates = Vec::new();
        let mut functions_by_item = HashMap::new();
        for item in &module.items {
            let HirItem::Function(function) = item else {
                continue;
            };
            functions_by_item.insert(function.id, function);
            let Some(symbol) = direct_symbols.get(&function.declaration) else {
                ineligible.insert(
                    function.declaration.clone(),
                    format!(
                        "HIR direct-call symbol index has no exact symbol for declaration `{}`",
                        function.declaration.full_path()
                    ),
                );
                continue;
            };
            // Every Hew body uses the private status ABI, including root `pub`
            // functions. Reserve its exact emitted name here so a source name
            // such as `open` cannot interpose a native C function. Instances
            // and closures derive their symbols from this same authority.
            let symbol = format!("__hew_fn_{symbol}");
            if !function.type_params.is_empty() {
                let signature = match generic_template_signature(function) {
                    Ok(signature) => signature,
                    Err(reason) => {
                        ineligible.insert(function.declaration.clone(), reason);
                        continue;
                    }
                };
                let id = GenericTemplateId {
                    declaration: function.declaration.clone(),
                };
                let source_origin = function_source_origin(module, function);
                if templates.contains_key(&function.declaration) {
                    ineligible.insert(
                        function.declaration.clone(),
                        format!(
                            "duplicate generic HIR template declaration `{}` has no unambiguous SIR template authority",
                            function.declaration.full_path()
                        ),
                    );
                    continue;
                }
                templates.insert(
                    function.declaration.clone(),
                    GenericTemplate {
                        function,
                        source_origin: source_origin.clone(),
                        symbol: symbol.clone(),
                        id: id.clone(),
                    },
                );
                generic_templates.push(SemGenericTemplate {
                    id,
                    function: function.id,
                    symbol: symbol.clone(),
                    source_origin,
                    type_params: function.type_params.clone(),
                    signature,
                });
                continue;
            }
            pending.push((function, symbol.clone()));
        }
        pending.sort_unstable_by(|(left, left_symbol), (right, right_symbol)| {
            left.declaration
                .cmp(&right.declaration)
                .then_with(|| left_symbol.cmp(right_symbol))
                .then_with(|| left.id.cmp(&right.id))
        });
        // A signature — and the aggregate, variant and type-fact rows it drags
        // in — is computed only when demand reaches the declaration. The
        // prelude declares far more than any one program calls, and an
        // uncalled declaration must not put its record shapes and collection
        // glue into every module's inventory.
        let mut admissible = HashMap::with_capacity(pending.len());
        let mut admissible_order = Vec::with_capacity(pending.len());
        for (function, symbol) in pending {
            if admissible
                .insert(
                    function.declaration.clone(),
                    AdmissibleFn { function, symbol },
                )
                .is_none()
            {
                admissible_order.push(function.declaration.clone());
            }
        }

        generic_templates.sort_by(|left, right| left.id.cmp(&right.id));
        Self {
            callables: Vec::new(),
            generic_templates,
            root_unit_callables: Vec::new(),
            entry_callable: None,
            entry_exit_plan: module.entry_exit_plan.clone(),
            monomorphic_by_declaration: HashMap::new(),
            admissible,
            admissible_order,
            templates,
            functions_by_item,
            trait_impls: hew_hir::dispatch::build_trait_impl_method_index(&module.items),
            ineligible,
        }
    }

    fn callable(&self, id: CallableId) -> Option<&SemCallable> {
        self.callables
            .get(usize::try_from(id.0).ok()?)
            .filter(|callable| callable.id == id)
    }
}

/// The first SIR generic slice deliberately has a finite, closed surface.
/// It is large enough to prove template substitution and call-graph closure,
/// but rejects any type that would force ownership, aggregate, reference,
/// resource, or runtime representation policy into SIR.
const SIR_GENERIC_INSTANCE_CAP: usize = 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallableState {
    /// A header exists but the entry closure has not asked for its body.
    Unreached,
    Queued,
    Lowering,
    Lowered,
    Failed,
}

/// SIR-owned generic specialization service.
///
/// A header is appended to the callable table *before* its body is queued.
/// Recursive and mutually-recursive calls can therefore refer to the stable
/// `CallableId` immediately, while a FIFO worklist keeps the construction
/// deterministic.  The service intentionally never reads
/// `HirModule::monomorphisations` or invokes MIR lowering.
struct InstanceService<'a> {
    module: &'a HirModule,
    /// The checker's §6.2 rows. The lowering reads a decided class out of this
    /// rather than recomputing one, and projects the rows its own bodies
    /// mention onto the module it produces.
    checked_facts: TypeFactService,
    table: CallableTable<'a>,
    states: Vec<CallableState>,
    statuses: Vec<Option<SirLoweringStatus>>,
    by_instance: HashMap<SirInstanceKey, CallableId>,
    closures: Vec<crate::SemClosure>,
    actors: Vec<crate::SemActor>,
    actor_sources: HashMap<CallableId, (HirFn, Vec<HirBinding>, TypeSubstitution)>,
    supervisors: Vec<crate::SemSupervisor>,
    /// Bodies the lowering synthesizes outside the HIR item table.
    synthetic_sources: HashMap<CallableId, (HirFn, TypeSubstitution)>,
    closures_by_instance: HashMap<crate::ClosureInstanceKey, crate::ClosureId>,
    closure_sources: Vec<(Box<HirExpr>, TypeSubstitution)>,
    /// Dispatch tables demanded by the erasure sites this module lowered.
    vtables: Vec<crate::SemVtable>,
    vtables_by_erasure: HashMap<(ResolvedTy, ResolvedTy), crate::SemVtableId>,
    entry_adapter: Option<EntryAdapter>,
    /// Only template headers that back a requested concrete SIR instance are
    /// emitted into the SIR module. HIR remains the authority for unselected
    /// generic definitions, so SIR does not accumulate an unrelated second
    /// template inventory.
    used_templates: std::collections::HashSet<GenericTemplateId>,
    /// How many published aggregate shapes have already been scanned for a
    /// `#[resource]` record whose `close` body drop glue will call.
    scanned_record_closes: usize,
    /// Authored opaque `#[resource]` closes already demanded, so a repeated
    /// projection scan does not queue the same body twice.
    demanded_opaque_closes: std::collections::HashSet<hew_types::DefId>,
    pending: VecDeque<CallableId>,
    functions: Vec<SemFunction>,
    aggregate_shapes: Vec<SemAggregateShape>,
    aggregate_shapes_by_type: HashMap<ResolvedTy, AggregateShapeId>,
    variant_shapes: Vec<SemVariantShape>,
    variant_shapes_by_type: HashMap<ResolvedTy, VariantShapeId>,
    string_literals: BTreeMap<StringLiteralId, String>,
    bytes_literals: BTreeMap<BytesLiteralId, Vec<u8>>,
    wire_plans: HashMap<ResolvedTy, std::sync::Arc<crate::SemWirePlan>>,
    value_capabilities:
        BTreeMap<(ResolvedTy, hew_types::ValueCapability), crate::SemValueMethodPlan>,
    structural_display: BTreeMap<crate::StructuralType, crate::SemStructuralRender>,
}

/// The declared type parameter a receiver-pattern position names, if any.
///
/// Both encodings of a bare parameter reference are accepted: the structural
/// `TypeParam` and the argument-less `Named` spelling some producers still
/// emit.
/// Targets whose lowering reads its arguments by position; the checker binds
/// no names for them.
fn positional_call_target(target: &CallTarget) -> bool {
    match target {
        CallTarget::DeclaredRuntime {
            actor_endpoints: Some(_),
            ..
        }
        | CallTarget::Runtime(hew_types::RuntimeCallFamily::SupervisorStop) => true,
        CallTarget::Builtin { endpoint } => {
            matches!(endpoint.as_str(), "assert" | "sleep" | "sleep_until")
        }
        _ => false,
    }
}

/// The argument indices in the order a call evaluates them. HIR leaves the
/// order empty when it is parameter order.
pub(crate) fn evaluation_sequence(evaluation_order: &[usize], len: usize) -> Vec<usize> {
    if evaluation_order.is_empty() {
        (0..len).collect()
    } else {
        evaluation_order.to_vec()
    }
}

fn declared_type_param_name<'a>(ty: &'a ResolvedTy, declared: &[String]) -> Option<&'a str> {
    match ty {
        ResolvedTy::TypeParam { name } => Some(name.as_str()),
        ResolvedTy::Named { name, args, .. }
            if args.is_empty() && declared.iter().any(|param| param == name) =>
        {
            Some(name.as_str())
        }
        _ => None,
    }
}

/// Resolve one concrete record through the canonical checker type service.
fn concrete_record_fields(
    module: &HirModule,
    facts: &TypeFactService,
    aggregate_ty: &ResolvedTy,
) -> Result<(hew_types::NominalInstance, Vec<SemAggregateField>), String> {
    if matches!(
        facts.declaration_marker(aggregate_ty)?,
        hew_types::DeclarationMarker::Resource
    ) && crate::resource::record_resource_lifecycle(module, aggregate_ty).is_none()
    {
        return Err(format!(
            "`{}` declares a resource cleanup boundary without an admitted release recipe",
            aggregate_ty.user_facing()
        ));
    }
    let (instance, fields) = facts.record_fields(aggregate_ty)?;
    Ok((
        instance,
        fields
            .into_iter()
            .map(|(name, ty)| SemAggregateField { name, ty })
            .collect(),
    ))
}

fn require_type_facts(facts: &mut TypeFactService, ty: &ResolvedTy) -> Result<(), String> {
    facts
        .require(ty)
        .map(|_| ())
        .map_err(|error| format!("type facts refused `{}`: {error}", ty.user_facing()))
}

fn require_aggregate_shape(
    module: &HirModule,
    facts: &mut TypeFactService,
    shapes: &mut Vec<SemAggregateShape>,
    shapes_by_type: &mut HashMap<ResolvedTy, AggregateShapeId>,
    aggregate_ty: &ResolvedTy,
) -> Result<AggregateShapeRef, String> {
    require_type_facts(facts, aggregate_ty)?;
    if let ResolvedTy::Tuple(fields) = aggregate_ty {
        for field in fields {
            require_type_facts(facts, field)?;
        }
        return Ok(AggregateShapeRef::Tuple);
    }
    if let Some(id) = shapes_by_type.get(aggregate_ty).copied() {
        return Ok(AggregateShapeRef::Record(id));
    }
    let (instance, fields) = concrete_record_fields(module, facts, aggregate_ty)?;
    for field in &fields {
        require_type_facts(facts, &field.ty)?;
    }
    let id = AggregateShapeId(
        u32::try_from(shapes.len())
            .map_err(|_| "SIR aggregate shape count exceeds u32".to_string())?,
    );
    shapes.push(SemAggregateShape {
        id,
        aggregate_ty: aggregate_ty.clone(),
        instance,
        marker: facts.declaration_marker(aggregate_ty)?,
        fields,
    });
    shapes_by_type.insert(aggregate_ty.clone(), id);
    Ok(AggregateShapeRef::Record(id))
}

fn concrete_variant_shape(
    module: &HirModule,
    enum_ty: &ResolvedTy,
) -> Result<(bool, Vec<SemVariant>), String> {
    let ResolvedTy::Named { args, builtin, .. } = enum_ty else {
        return Err(format!(
            "`{}` is not a checker-resolved named enum",
            enum_ty.user_facing()
        ));
    };
    if enum_ty.nominal_instance().is_some_and(|instance| {
        module.items.iter().any(|item| {
            matches!(item, HirItem::TypeDecl(decl)
            if decl.declaration == *instance.nominal.declaration()
                && decl.kind == hew_hir::HirTypeDeclKind::Enum)
        })
    }) {
        return concrete_user_variant_shape(module, enum_ty);
    }
    builtin.map_or_else(
        || concrete_user_variant_shape(module, enum_ty),
        |builtin| concrete_builtin_variant_shape(enum_ty, args, builtin),
    )
}

fn concrete_builtin_variant_shape(
    enum_ty: &ResolvedTy,
    args: &[ResolvedTy],
    builtin: hew_types::BuiltinType,
) -> Result<(bool, Vec<SemVariant>), String> {
    if let ResolvedTy::Named { name, .. } = enum_ty {
        if args.is_empty()
            && hew_types::builtin_enums::has_exact_monomorphic_builtin_enum_identity(
                name,
                Some(builtin),
            )
        {
            let declaration = hew_types::builtin_enums::monomorphic_builtin_enum(name)
                .ok_or("canonical builtin enum lost its source declaration")?;
            return Ok((
                false,
                declaration
                    .variants
                    .iter()
                    .map(|variant| SemVariant {
                        name: variant.name.to_string(),
                        kind: SemVariantKind::Unit,
                        fields: Vec::new(),
                    })
                    .collect(),
            ));
        }
    }
    let declaration = builtin.generic_enum().ok_or_else(|| {
        format!(
            "builtin `{}` has no payload-variant SIR declaration",
            enum_ty.user_facing()
        )
    })?;
    if args.len() != declaration.type_params.len() {
        return Err(format!(
            "builtin enum `{}` has incorrect type argument arity",
            enum_ty.user_facing()
        ));
    }
    let variants = declaration
        .variants
        .iter()
        .map(|variant| SemVariant {
            name: variant.name.to_string(),
            kind: if variant.payload_type_args.is_empty() {
                SemVariantKind::Unit
            } else {
                SemVariantKind::Tuple
            },
            fields: variant
                .payload_type_args
                .iter()
                .enumerate()
                .map(|(field, argument)| SemVariantField {
                    name: field.to_string(),
                    ty: args[*argument].clone(),
                })
                .collect(),
        })
        .collect();
    Ok((false, variants))
}

/// A variant no value can inhabit: one of its payload types has no values.
/// `ActorError<Never>.Failed(Never)` is the case this exists for — an
/// infallible handler's completion call can never report a declared failure.
fn is_unconstructable_variant(module: &HirModule, variant: &SemVariant) -> bool {
    variant
        .fields
        .iter()
        .any(|field| is_uninhabited(module, &field.ty))
}

fn is_uninhabited(module: &HirModule, ty: &ResolvedTy) -> bool {
    if matches!(ty, ResolvedTy::Never) {
        return true;
    }
    let Some(instance) = ty.nominal_instance() else {
        return false;
    };
    let declaration = instance.nominal.declaration();
    module.items.iter().any(|item| {
        matches!(item, HirItem::TypeDecl(decl)
            if decl.declaration == *declaration
                && decl.kind == hew_hir::HirTypeDeclKind::Enum
                && decl.variants.is_empty())
    })
}

fn concrete_user_variant_shape(
    module: &HirModule,
    enum_ty: &ResolvedTy,
) -> Result<(bool, Vec<SemVariant>), String> {
    let instance = enum_ty.nominal_instance().ok_or_else(|| {
        format!(
            "`{}` has no checker-minted nominal enum identity",
            enum_ty.user_facing()
        )
    })?;
    let declaration = instance.nominal.declaration();
    let decl = module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::TypeDecl(decl)
                if decl.declaration == *declaration
                    && decl.kind == hew_hir::HirTypeDeclKind::Enum =>
            {
                Some(decl)
            }
            _ => None,
        })
        .ok_or_else(|| {
            format!(
                "enum `{}` has no exact HIR declaration",
                enum_ty.user_facing()
            )
        })?;
    if decl.type_params.len() != instance.args.len() {
        return Err(format!(
            "enum `{}` supplies {} type argument(s), declaration expects {}",
            enum_ty.user_facing(),
            instance.args.len(),
            decl.type_params.len()
        ));
    }

    let layout = exact_user_variant_layout(module, decl, &instance.args, enum_ty)?;
    let variants = sem_variants_from_decl(decl, &instance.args, layout, enum_ty)?;
    Ok((decl.is_indirect, variants))
}

fn exact_user_variant_layout<'a>(
    module: &'a HirModule,
    decl: &hew_hir::HirTypeDecl,
    args: &[ResolvedTy],
    enum_ty: &ResolvedTy,
) -> Result<Option<&'a hew_hir::EnumLayout>, String> {
    let layout = if decl.type_params.is_empty() {
        None
    } else {
        let mut matches = module
            .enum_layouts
            .iter()
            .filter(|layout| layout.key.origin == decl.id && layout.key.type_args == args);
        let layout = matches.next().ok_or_else(|| {
            format!(
                "generic enum `{}` has no exact HIR specialization layout",
                enum_ty.user_facing()
            )
        })?;
        if matches.next().is_some() {
            return Err(format!(
                "generic enum `{}` has more than one exact HIR specialization layout",
                enum_ty.user_facing()
            ));
        }
        Some(layout)
    };
    Ok(layout)
}

fn sem_variants_from_decl(
    decl: &hew_hir::HirTypeDecl,
    args: &[ResolvedTy],
    layout: Option<&hew_hir::EnumLayout>,
    enum_ty: &ResolvedTy,
) -> Result<Vec<SemVariant>, String> {
    decl.variants
        .iter()
        .enumerate()
        .map(|(variant_index, variant)| {
            let names = variant.field_names();
            let tys = layout.map_or_else(
                || {
                    variant
                        .field_tys()
                        .iter()
                        .map(|ty| hew_hir::substitute_type_params(ty, &decl.type_params, args))
                        .collect::<Vec<_>>()
                },
                |layout| {
                    layout
                        .variants
                        .get(variant_index)
                        .map(|variant| variant.field_tys.clone())
                        .unwrap_or_default()
                },
            );
            if names.len() != tys.len() {
                return Err(format!(
                    "enum `{}` variant `{}` has inconsistent HIR field names and types",
                    enum_ty.user_facing(),
                    variant.name
                ));
            }
            if layout.is_some_and(|layout| {
                layout
                    .variants
                    .get(variant_index)
                    .is_none_or(|candidate| candidate.name != variant.name)
            }) {
                return Err(format!(
                    "enum `{}` specialization layout disagrees with variant {} identity",
                    enum_ty.user_facing(),
                    variant_index
                ));
            }
            Ok(SemVariant {
                name: variant.name.clone(),
                kind: match variant.kind {
                    hew_hir::HirVariantKind::Unit => SemVariantKind::Unit,
                    hew_hir::HirVariantKind::Tuple(_) => SemVariantKind::Tuple,
                    hew_hir::HirVariantKind::Struct(_) => SemVariantKind::Struct,
                },
                fields: names
                    .into_iter()
                    .zip(tys)
                    .map(|(name, ty)| SemVariantField { name, ty })
                    .collect(),
            })
        })
        .collect()
}

fn require_variant_shape(
    module: &HirModule,
    facts: &mut TypeFactService,
    shapes: &mut Vec<SemVariantShape>,
    shapes_by_type: &mut HashMap<ResolvedTy, VariantShapeId>,
    enum_ty: &ResolvedTy,
) -> Result<VariantShapeId, String> {
    if let Some(id) = shapes_by_type.get(enum_ty).copied() {
        return Ok(id);
    }
    require_type_facts(facts, enum_ty)?;
    let (is_indirect, variants) = concrete_variant_shape(module, enum_ty)?;
    for variant in &variants {
        for field in &variant.fields {
            require_type_facts(facts, &field.ty)?;
        }
    }
    let id = VariantShapeId(
        u32::try_from(shapes.len())
            .map_err(|_| "SIR variant shape count exceeds u32".to_string())?,
    );
    shapes.push(SemVariantShape {
        id,
        enum_ty: enum_ty.clone(),
        is_indirect,
        variants,
    });
    shapes_by_type.insert(enum_ty.clone(), id);
    Ok(id)
}

fn require_signature_shapes(
    module: &HirModule,
    facts: &mut TypeFactService,
    aggregate_shapes: &mut Vec<SemAggregateShape>,
    aggregate_shapes_by_type: &mut HashMap<ResolvedTy, AggregateShapeId>,
    variant_shapes: &mut Vec<SemVariantShape>,
    variant_shapes_by_type: &mut HashMap<ResolvedTy, VariantShapeId>,
    signature: &SemSignature,
) -> Result<(), String> {
    for ty in signature
        .params
        .iter()
        .map(|parameter| &parameter.ty)
        .chain(std::iter::once(&signature.return_ty))
    {
        require_type_shapes(
            module,
            facts,
            aggregate_shapes,
            aggregate_shapes_by_type,
            variant_shapes,
            variant_shapes_by_type,
            ty,
        )?;
    }
    Ok(())
}

/// Publish the complete shape closure using exact checked type identities.
/// Header admission uses the same walk as demanded bodies, without lowering
/// those bodies merely to discover nested payload shapes.
fn require_type_shapes(
    module: &HirModule,
    facts: &mut TypeFactService,
    aggregate_shapes: &mut Vec<SemAggregateShape>,
    aggregate_shapes_by_type: &mut HashMap<ResolvedTy, AggregateShapeId>,
    variant_shapes: &mut Vec<SemVariantShape>,
    variant_shapes_by_type: &mut HashMap<ResolvedTy, VariantShapeId>,
    ty: &ResolvedTy,
) -> Result<(), String> {
    let mut pending = vec![ty.clone()];
    let mut seen = BTreeSet::new();
    while let Some(ty) = pending.pop() {
        if !seen.insert(ty.clone()) {
            continue;
        }
        require_type_facts(facts, &ty)?;
        if !matches!(ty, ResolvedTy::Unit | ResolvedTy::Never)
            && !is_supported_call_value(module, facts, &ty)
        {
            return Err(format!(
                "nested type `{}` has no semantic value contract",
                ty.user_facing()
            ));
        }
        if let ResolvedTy::Array(element, _) = &ty {
            pending.push((**element).clone());
        } else if let Some(payload) = hew_types::runtime_call::shared_handle_payload(&ty) {
            // The allocation's payload is an ordinary nested value; the walk's
            // own admission decides it.
            pending.push(payload.clone());
        } else if let Some((builtin, arguments)) = collection_type_arguments(&ty) {
            for argument in arguments {
                if !is_supported_call_value(module, facts, argument) {
                    let component = if builtin == hew_types::BuiltinType::Vec {
                        "vector element"
                    } else {
                        "collection component"
                    };
                    return Err(format!(
                        "{component} `{}` has no semantic value contract",
                        argument.user_facing(),
                    ));
                }
            }
            // Execution-domain admission is distinct from copyability.
            // Publish facts/shapes; the shared dependency verifier owns
            // recursive collection copy admissibility.
            pending.extend(arguments.iter().cloned());
        } else if is_concrete_variant_type(module, &ty) {
            let id =
                require_variant_shape(module, facts, variant_shapes, variant_shapes_by_type, &ty)?;
            pending.extend(
                variant_shapes[id.0 as usize]
                    .variants
                    .iter()
                    .flat_map(|variant| &variant.fields)
                    .map(|field| field.ty.clone()),
            );
        } else if is_concrete_aggregate_type(module, facts, &ty) {
            if let AggregateShapeRef::Record(id) = require_aggregate_shape(
                module,
                facts,
                aggregate_shapes,
                aggregate_shapes_by_type,
                &ty,
            )? {
                pending.extend(
                    aggregate_shapes[id.0 as usize]
                        .fields
                        .iter()
                        .map(|field| field.ty.clone()),
                );
            }
        }
        // The actor parameter of `RemotePid<T>` is phantom at the ABI
        // boundary. It selects checked dispatch and codec contracts elsewhere,
        // but is not an independently carried value that needs a SIR shape.
        // A pool view's supervisor and member parameters are the same: they
        // name the role its accessors mint, not a value the view carries.
        if ty.is_builtin(hew_types::BuiltinType::RemotePid)
            || ty.is_builtin(hew_types::BuiltinType::SupervisorPool)
        {
            continue;
        }
        if actor::declaration(module, &ty).is_none()
            && supervisor::declaration(module, &ty).is_none()
        {
            hew_types::push_type_components(&ty, &mut pending);
        }
    }
    Ok(())
}

/// Project the checker's §6.2 rows onto the types one SIR module mentions.
///
/// The module carries the rows its own values and headers need and no others:
/// a consumer keys on the exact type it holds, so the projection is closed
/// under a type's components the same way the checker's table is. A type the
/// checker published no row for gets none here either — there is no default
/// class, and a missing key is the fail-closed case (`MissingTypeFacts`, L2).
#[expect(
    clippy::too_many_arguments,
    reason = "the projection reads every part of the module that names a type; grouping them would only rename the same list"
)]
fn project_type_facts(
    checked: &TypeFactTable,
    callables: &[SemCallable],
    templates: &[SemGenericTemplate],
    functions: &[SemFunction],
    aggregate_shapes: &[SemAggregateShape],
    variant_shapes: &[SemVariantShape],
    vtables: &[crate::SemVtable],
    value_capabilities: &BTreeMap<
        (ResolvedTy, hew_types::ValueCapability),
        crate::SemValueMethodPlan,
    >,
) -> TypeFactTable {
    let mut mentioned: Vec<ResolvedTy> = Vec::new();
    // A selected value capability is a module fact the verifier keys on, so the
    // projection is closed over it like every other. A body that registers a
    // map's key capabilities and then refuses to lower leaves its plans behind;
    // without this the module fails verification for a missing row and the
    // internal diagnostic hides the body's real limitation.
    for (ty, _) in value_capabilities.keys() {
        mentioned.push(ty.clone());
    }
    let push_signature = |signature: &SemSignature, out: &mut Vec<ResolvedTy>| {
        for param in &signature.params {
            out.push(param.ty.clone());
        }
        out.push(signature.return_ty.clone());
    };
    for callable in callables {
        push_signature(&callable.signature, &mut mentioned);
    }
    for template in templates {
        push_signature(&template.signature, &mut mentioned);
    }
    for function in functions {
        mentioned.push(function.return_ty.clone());
        for param in &function.params {
            mentioned.push(param.ty.clone());
        }
        for place in &function.places {
            mentioned.push(place.ty.clone());
        }
        for block in &function.blocks {
            for arg in &block.args {
                mentioned.push(arg.ty.clone());
            }
            for op in &block.ops {
                for result in &op.results {
                    mentioned.push(result.ty.clone());
                }
            }
        }
    }
    for vtable in vtables {
        mentioned.push(vtable.dyn_ty.clone());
        mentioned.push(vtable.concrete_ty.clone());
        for slot in &vtable.slots {
            push_signature(&slot.signature, &mut mentioned);
        }
    }
    for shape in aggregate_shapes {
        mentioned.push(shape.aggregate_ty.clone());
        mentioned.extend(shape.fields.iter().map(|field| field.ty.clone()));
    }
    for shape in variant_shapes {
        mentioned.push(shape.enum_ty.clone());
        mentioned.extend(
            shape
                .variants
                .iter()
                .flat_map(|variant| variant.fields.iter())
                .map(|field| field.ty.clone()),
        );
    }

    let mut projected = TypeFactTable::new();
    let mut seen: std::collections::BTreeSet<TypeInstanceKey> = std::collections::BTreeSet::new();
    while let Some(ty) = mentioned.pop() {
        let key = TypeInstanceKey(ty.clone());
        if !seen.insert(key.clone()) {
            continue;
        }
        if let Some(row) = checked.get(&key) {
            projected.insert(key, *row);
        }
        hew_types::push_type_components(&ty, &mut mentioned);
    }
    projected
}

struct LoweringInput<'a> {
    function: Cow<'a, HirFn>,
    callable: SemCallable,
    substitution: TypeSubstitution,
    source: BodySource,
}

enum BodySource {
    Function,
    Closure(Box<HirExpr>),
    Actor {
        actor: crate::ActorId,
        state_bindings: Vec<HirBinding>,
    },
    EntryAdapter(EntryAdapter),
}

/// The synthesized parameterless body that realizes a `Result` entry exit
/// plan. It reuses the entry's declaration identity and HIR provenance; the
/// checker's action is consumed here and physical lowering sees an integer
/// exit status.
#[derive(Debug, Clone)]
struct EntryAdapter {
    callable: CallableId,
    entry: CallableId,
    action: EntryExitAction,
}

impl BodySource {
    fn parameters<'a>(&'a self, function: &'a HirFn) -> Result<&'a [HirBinding], String> {
        match self {
            Self::Function | Self::Actor { .. } => Ok(&function.params),
            Self::EntryAdapter(_) => Ok(&[]),
            Self::Closure(expression) => match &expression.kind {
                HirExprKind::Closure { params, .. } => Ok(params),
                _ => Err("closure body source is not a checked literal".to_string()),
            },
        }
    }
}

fn function_source_origin(module: &HirModule, function: &HirFn) -> FunctionSourceOrigin {
    if module.root_item_ids.contains(&function.id) {
        FunctionSourceOrigin::RootUnit
    } else if let Some(module_name) = module.diagnostic_source_modules.get(&function.id) {
        FunctionSourceOrigin::Foreign(module_name.clone())
    } else {
        FunctionSourceOrigin::Unknown
    }
}

fn generic_template_admission(function: &HirFn) -> Result<(), String> {
    if function.intrinsic_id.is_some() {
        return Err(
            "floor intrinsics remain outside SIR's ordinary direct-call domain".to_string(),
        );
    }
    Ok(())
}

fn generic_template_signature(function: &HirFn) -> Result<SemSignature, String> {
    generic_template_admission(function)?;
    Ok(SemSignature {
        params: function
            .params
            .iter()
            .map(|parameter| SemAbiParam {
                ty: parameter.ty.clone(),
                passing: if parameter.is_consume || function.var_self_receiver == Some(parameter.id)
                {
                    SemParamPassing::Consume
                } else {
                    SemParamPassing::ReadOnly
                },
                caller_visible_projection: function.var_self_receiver == Some(parameter.id),
            })
            .collect(),
        return_ty: function.return_ty.clone(),
    })
}

fn callable_signature(
    module: &HirModule,
    function: &HirFn,
    facts: &mut TypeFactService,
) -> Result<SemSignature, String> {
    if !function.type_params.is_empty() {
        return Err(
            "generic origin functions are instantiated by the SIR instance service, not admitted as abstract callable bodies"
                .to_string(),
        );
    }
    callable_signature_with_substitution(module, function, &TypeSubstitution::empty(), facts)
}

fn callable_signature_with_substitution(
    module: &HirModule,
    function: &HirFn,
    substitution: &TypeSubstitution,
    facts: &mut TypeFactService,
) -> Result<SemSignature, String> {
    generic_template_admission(function)?;
    let mut params = Vec::with_capacity(function.params.len());
    for (index, parameter) in function.params.iter().enumerate() {
        let ty = substitution.apply(&parameter.ty);
        if !is_supported_call_value(module, facts, &ty) {
            return Err(format!(
                "`{}` parameter {index} has unsupported type `{}` after semantic substitution; SIR calls require an exact scalar, string, bytes, aggregate, or variant contract",
                function.name,
                ty.user_facing()
            ));
        }
        let row = facts
            .require(&ty)
            .map_err(|error| format!("type facts refused `{}`: {error}", ty.user_facing()))?;
        params.push(SemAbiParam {
            ty,
            passing: if OwnKind::of_class(row.class) == OwnKind::Owned {
                if parameter.is_consume || function.var_self_receiver == Some(parameter.id) {
                    SemParamPassing::Consume
                } else {
                    SemParamPassing::Borrow
                }
            } else {
                SemParamPassing::ReadOnly
            },
            // A `var self` receiver returns to its caller on both edges: in
            // the dual return, and handed back when the call fails.
            caller_visible_projection: function.var_self_receiver == Some(parameter.id),
        });
    }
    let return_ty = substitution.apply(&function.return_ty);
    if !is_supported_call_return(module, facts, &return_ty) {
        return Err(format!(
            "return type `{}` is outside SIR's exact call-result domain after semantic substitution",
            return_ty.user_facing()
        ));
    }
    Ok(SemSignature { params, return_ty })
}

/// How one argument crosses an erased dispatch boundary.
///
/// The boundary is uniform across every implementation of a trait object, so
/// it is decided by the value class alone; an implementation that declares a
/// consuming parameter is refused when its table is built rather than
/// silently changing the ABI of one erasure.
fn dyn_boundary_passing(own: OwnKind) -> SemParamPassing {
    if own == OwnKind::Owned {
        SemParamPassing::Borrow
    } else {
        SemParamPassing::ReadOnly
    }
}

/// How the erased receiver crosses the boundary, from the trait's own
/// declaration rather than any one implementer's value class.
fn dyn_receiver_passing(signature: &hew_types::FnSig) -> SemParamPassing {
    if signature.consumes_receiver {
        SemParamPassing::Consume
    } else if signature.requires_mutable_receiver {
        SemParamPassing::BorrowMut
    } else {
        SemParamPassing::Borrow
    }
}

/// Whether an implementation's declared transfer realizes the boundary's.
fn dyn_passing_admits(boundary: SemParamPassing, implementation: SemParamPassing) -> bool {
    match boundary {
        SemParamPassing::Consume => implementation == SemParamPassing::Consume,
        SemParamPassing::BorrowMut => implementation == SemParamPassing::BorrowMut,
        // A borrowed boundary hands the callee a live value it does not own;
        // a bit-copy receiver reads it and an owned one borrows it.
        SemParamPassing::Borrow | SemParamPassing::ReadOnly => matches!(
            implementation,
            SemParamPassing::Borrow | SemParamPassing::ReadOnly
        ),
    }
}

fn is_initial_scalar(ty: &ResolvedTy) -> bool {
    ty.is_integer()
        || ty.is_float()
        || matches!(
            ty,
            ResolvedTy::Bool | ResolvedTy::Char | ResolvedTy::Duration
        )
}

fn is_initial_call_value(ty: &ResolvedTy) -> bool {
    is_initial_scalar(ty)
        || matches!(
            ty,
            ResolvedTy::Borrow { .. } | ResolvedTy::String
                | ResolvedTy::Task(_)
                | ResolvedTy::Bytes
                | ResolvedTy::Array(_, _)
                | ResolvedTy::Function { .. }
                | ResolvedTy::Closure { .. }
                | ResolvedTy::TraitObject { .. }
        )
        || crate::generator_parts(ty).is_some()
        || crate::stream_element(ty).is_some()
        || crate::sink_element(ty).is_some()
        || ty.is_builtin(hew_types::BuiltinType::ActorCall)
        || collection_type_arguments(ty).is_some()
        || ty.is_builtin(hew_types::BuiltinType::ActorHandle)
        || ty.is_builtin(hew_types::BuiltinType::ActorFn)
        || ty.is_builtin(hew_types::BuiltinType::ChildRef)
        // A pool view is the same fixed-width pair a role is: the owning
        // supervisor and a slot.
        || ty.is_builtin(hew_types::BuiltinType::SupervisorPool)
        // Distributed identity carriers are fixed-width BitCopy values. Their
        // source fields are intentionally not constructible, so they enter SIR
        // as exact compiler-owned ABI carriers rather than user records.
        || ty.is_builtin(hew_types::BuiltinType::NodeId)
        || ty.is_builtin(hew_types::BuiltinType::Location)
        || ty.is_builtin(hew_types::BuiltinType::RemotePid)
        || ty.is_builtin(hew_types::BuiltinType::JsonValue)
        || ty.is_builtin(hew_types::BuiltinType::YamlValue)
        // A shared handle is one pointer into a runtime-counted allocation.
        // Its payload is a nested type of its own, published alongside it.
        || hew_types::runtime_call::shared_handle_payload(ty).is_some()
}

/// The runtime operation one checked `Rc`/`Weak` method is.
fn shared_handle_family(op: hew_types::RcIntrinsicOp) -> hew_types::RuntimeCallFamily {
    use hew_types::RcIntrinsicOp as Op;
    use hew_types::RuntimeCallFamily as F;
    match op {
        Op::New => F::RcNew,
        Op::Clone => F::RcClone,
        Op::GetCopy => F::RcGet,
        Op::Set => F::RcSet,
        Op::Downgrade => F::RcDowngrade,
        Op::StrongCount => F::RcStrongCount,
        Op::WeakCount => F::RcWeakCount,
        Op::IsUnique => F::RcIsUnique,
        Op::WeakClone => F::WeakCloneRc,
        Op::WeakUpgrade => F::WeakUpgradeRc,
    }
}

fn is_concrete_aggregate_type(
    module: &HirModule,
    facts: &TypeFactService,
    ty: &ResolvedTy,
) -> bool {
    matches!(ty, ResolvedTy::Tuple(_)) || concrete_record_fields(module, facts, ty).is_ok()
}

fn is_concrete_variant_type(module: &HirModule, ty: &ResolvedTy) -> bool {
    concrete_variant_shape(module, ty).is_ok()
}

/// An `#[opaque]` declaration with no ownership marker: an FFI pass-through
/// id whose lifecycle a `#[resource]` wrapper owns. It is a value of
/// pointer width with no obligation of its own.
fn is_opaque_handle(facts: &TypeFactService, ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            builtin: None,
            is_opaque: true,
            ..
        }
    ) && facts
        .rows()
        .get(&hew_types::TypeInstanceKey(ty.clone()))
        .is_some_and(|row| row.class == hew_types::ValueClass::BitCopy)
}

/// Opaque owners enter ordinary value flow only through an audited lifecycle:
/// a checker-discovered one names the producers that mint the handle, and an
/// authored one is the `close` HIR admitted as the type's own release.
fn is_checked_opaque_resource(module: &HirModule, ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Named { builtin: None, is_opaque: true, args, .. } if args.is_empty())
        && (crate::resource::authored_opaque_lifecycle(module, ty).is_some()
            || module
                .type_classes
                .lifecycle_registry()
                .opaque_resource_for_ty(ty)
                .is_some_and(|lifecycle| !lifecycle.producer_declarations.is_empty()))
}

fn is_supported_call_value(module: &HirModule, facts: &TypeFactService, ty: &ResolvedTy) -> bool {
    is_initial_call_value(ty)
        || is_checked_opaque_resource(module, ty)
        || is_opaque_handle(facts, ty)
        || actor::declaration(module, ty).is_some()
        || supervisor::declaration(module, ty).is_some()
        || is_concrete_aggregate_type(module, facts, ty)
        || is_concrete_variant_type(module, ty)
}

/// Whether a checker-resolved type argument can key a SIR generic instance.
///
/// A type argument is a type, not a value crossing a call boundary: a `gen fn`
/// with no final return binds `Generator<Y, R>`'s `R` to `()`, and the
/// diverging arm of an instance binds `Never`. Neither is a value SIR ever
/// passes, so neither owes a call-value contract; every other argument must
/// name one.
fn is_supported_instance_type_arg(
    module: &HirModule,
    facts: &TypeFactService,
    ty: &ResolvedTy,
) -> bool {
    matches!(ty, ResolvedTy::Unit | ResolvedTy::Never) || is_supported_call_value(module, facts, ty)
}

fn is_supported_call_return(module: &HirModule, facts: &TypeFactService, ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Unit | ResolvedTy::Never) || is_supported_call_value(module, facts, ty)
}

/// The first aggregate value family admitted into SIR.
///
/// These values remain purely semantic until physical MIR decides whether a
/// representation boundary requires storage. Restricting tuple leaves to the
/// existing `BitCopy` scalar domain keeps this slice free of drops,
/// borrowing, reference counts, and layout-dependent behaviour.
fn is_initial_value_type(ty: &ResolvedTy) -> bool {
    is_initial_scalar(ty)
        || matches!(ty, ResolvedTy::Tuple(elements)
            if elements.iter().all(is_initial_value_type))
}

/// The variant name of one HIR expression kind, for refusal diagnostics.
fn hir_expr_kind_name(kind: &HirExprKind) -> String {
    let rendered = format!("{kind:?}");
    rendered
        .split([' ', '{', '('])
        .next()
        .unwrap_or(&rendered)
        .to_string()
}

fn require_initial_scalar_read(intent: IntentKind) -> Result<(), String> {
    match intent {
        IntentKind::Read => Ok(()),
        // HIR's `Modify`, `Consume` and `Discharge` each need a SIR ownership
        // operation - `begin_borrow`, `move`, `destroy_value` - that the
        // initial scalar domain does not emit. Naming the intent rather than a
        // mode keeps the refusal precise without reviving the deleted mode set.
        IntentKind::Modify | IntentKind::Consume | IntentKind::Discharge => Err(format!(
            "HIR {intent:?} intent needs a SIR ownership operation; initial scalar SIR admits only read operands"
        )),
        IntentKind::Capture => Err(
            "HIR Capture intent requires closure/COW capture semantics that the initial scalar SIR slice does not model"
                .to_string(),
        ),
        IntentKind::Yield => Err(
            "HIR Yield intent requires explicit SIR suspension semantics that the initial scalar slice does not model"
                .to_string(),
        ),
        IntentKind::Unknown => Err(
            "HIR Unknown intent is not a legal input to semantic SIR lowering".to_string(),
        ),
    }
}

/// Lower a value flowing into a binding or function return in the initial
/// no-drop scalar/tuple domain.
///
/// HIR intentionally marks these positions `Consume`: their result transfers
/// to a new binding or the caller. For bitcopy scalars, that semantic transfer
/// has no exclusive ownership obligation, so SIR keeps the same virtual value
/// and represents the receiving flow as `Read`. The same applies recursively
/// to tuples made solely from such scalar values. This is a narrow value-class
/// rule, not a general weakening of `Move`: actual operand positions remain
/// read-only in this slice, and every ownership-bearing transfer fails closed
/// until ownership/layout MIR can realize it.
fn require_initial_value_transfer(
    intent: IntentKind,
    ty: &hew_types::ResolvedTy,
    context: &str,
) -> Result<(), String> {
    match intent {
        IntentKind::Read | IntentKind::Consume if is_initial_value_type(ty) => Ok(()),
        IntentKind::Read | IntentKind::Consume => Err(format!(
            "{context}: HIR {intent:?} intent transfers ownership-bearing `{}`; initial SIR only aliases BitCopy scalar/tuple binding/return flow",
            ty.user_facing()
        )),
        other => Err(format!(
            "{context}: HIR {other:?} intent; initial scalar/tuple binding/return flow admits only a read or a BitCopy transfer"
        )),
    }
}

fn lower_initial_value_transfer(
    builder: &mut Builder<'_, '_>,
    expr: &HirExpr,
    context: &str,
    binding_use: OwnedBindingUse,
) -> Result<ValueId, String> {
    let ty = builder.ty(&expr.ty);
    if ty == ResolvedTy::Unit && matches!(expr.intent, IntentKind::Read | IntentKind::Consume) {
        if matches!(
            expr.kind,
            HirExprKind::Literal(HirLiteral::Unit) | HirExprKind::VarSelfMethodCall { .. }
        ) {
            return builder.lower_expr(expr);
        }
        // A unit method result still evaluates its effects before returning Self.
        builder.lower_discarded_expr(expr)?;
        return builder.emit(expr, SemOpKind::ConstUnit);
    }
    if is_initial_value_type(&ty) {
        require_initial_value_transfer(expr.intent, &ty, context)?;
        return builder.lower_expr(expr);
    }
    if !matches!(
        expr.intent,
        IntentKind::Read | IntentKind::Consume | IntentKind::Capture
    ) {
        return Err(format!(
            "{context}: HIR {:?} intent cannot transfer `{}` in the owned SIR slice",
            expr.intent,
            ty.user_facing()
        ));
    }
    builder.service.require_type_facts(&ty)?;
    if !is_initial_call_value(&ty)
        && !is_checked_opaque_resource(builder.service.module, &ty)
        && !is_opaque_handle(&builder.service.checked_facts, &ty)
    {
        if is_concrete_variant_type(builder.service.module, &ty) {
            builder
                .service
                .require_variant_shape(&ty)
                .map_err(|reason| {
                    format!(
                        "{context}: `{}` has no exact variant transfer contract: {reason}",
                        ty.user_facing()
                    )
                })?;
        } else {
            builder
                .service
                .require_aggregate_shape(&ty)
                .map_err(|reason| {
                    format!(
                        "{context}: `{}` has no aggregate transfer contract: {reason}",
                        ty.user_facing()
                    )
                })?;
        }
    }
    builder.lower_owned_transfer(expr, binding_use)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OwnedBindingUse {
    Copy,
    /// A read that never becomes an owner: a match scrutinee, which is probed
    /// and whose payloads name its region. A binding that names a loan may be
    /// read through it here.
    Probe,
    Move,
    /// A final value may move only when cleanup does not still need its binding.
    Return,
}

/// The evaluated receiver is retained separately from the ordinary arguments.
enum PreparedCallee {
    Direct(CallableId),
    Indirect(crate::BoundaryOperand),
    Dyn {
        receiver: crate::BoundaryOperand,
        slot: u32,
        method: hew_types::DefId,
    },
}

/// Where a failing `var self` call's handed-back receiver goes on its unwind
/// edge: back into the caller's place, or released when the call worked on a
/// staged copy that the place never published.
struct FaultHandback {
    ty: ResolvedTy,
    own: OwnKind,
    seat: Option<var_self::ReceiverSeat>,
    provenance: Provenance,
}

impl PreparedCallee {
    #[allow(
        clippy::too_many_arguments,
        reason = "one terminator constructor for every prepared callee shape"
    )]
    fn invoke(
        self,
        id: OpId,
        signature: SemSignature,
        args: Vec<crate::BoundaryOperand>,
        result: CallResult,
        normal: Option<Edge>,
        unwind: CallUnwind,
        handback: Option<ValueDef>,
    ) -> SemTerminator {
        match self {
            Self::Direct(callee) => SemTerminator::Call {
                id,
                callee,
                args,
                result,
                normal,
                unwind,
                handback,
            },
            Self::Indirect(callee) => SemTerminator::IndirectCall {
                id,
                callee,
                signature,
                args,
                result,
                normal,
                unwind,
            },
            Self::Dyn {
                receiver,
                slot,
                method,
            } => SemTerminator::DynCall {
                id,
                receiver,
                slot,
                method,
                signature,
                args,
                result,
                normal,
                unwind,
            },
        }
    }
}

#[derive(Clone)]
struct ControlState {
    block: BlockId,
    bindings: HashMap<BindingId, BindingTarget>,
    binding_declarations: HashMap<BindingId, usize>,
    owned_live: BTreeMap<ValueId, ResolvedTy>,
    loans: Vec<ValueId>,
    scopes: Vec<Vec<BindingId>>,
    scope_loans: Vec<ValueId>,
    scope_loan_floors: Vec<usize>,
    ended_loans: std::collections::HashSet<ValueId>,
    defers: Vec<deferred::PendingDefer>,
    task_scopes: Vec<tasks::TaskScopeFrame>,
    cleanup_may_fail: bool,
    cleanup_draining: bool,
    deferred_initialized: BTreeSet<PlaceId>,
    state_taken: BTreeSet<PlaceId>,
}

/// The scope loans one `let` binding holds on a borrowed collection.
struct BindingLoans {
    root: crate::OwnerRoot,
    loans: Vec<ValueId>,
    /// The loop nesting the binding was declared in. A loan taken inside a
    /// loop is defined by ops that do not dominate the code after it, so it
    /// only ends early at the same loop depth.
    loop_depth: usize,
    /// The conditional nesting the binding was declared in. Ending a loan
    /// inside a branch would leave it live on the sibling path, so it only
    /// ends early where control has not diverged since.
    branch_depth: usize,
}

struct MatchExit {
    state: ControlState,
    result: Option<Operand>,
}

struct VariantBranch {
    variant: u32,
    block: BlockId,
    fields: Vec<BlockArg>,
    owned_live: BTreeMap<ValueId, ResolvedTy>,
}

/// Lower a unit expression transferred by an explicit `return`.
///
/// This is intentionally narrower than [`Builder::lower_discarded_expr`]. A
/// standalone discarded expression is an ordinary effect position and stays
/// read-only in the initial slice. A unit expression in `return` instead
/// transfers control to the caller; HIR marks that transfer `Consume`, which
/// is harmless for `Unit` but must not be rechecked as an ordinary operand use.
fn lower_initial_unit_return(builder: &mut Builder<'_, '_>, expr: &HirExpr) -> Result<(), String> {
    let ty = builder.ty(&expr.ty);
    if !matches!(expr.intent, IntentKind::Read | IntentKind::Consume) || ty != ResolvedTy::Unit {
        return Err(format!(
            "unit return value: HIR {:?} intent for `{}`; initial SIR admits only a read or a Unit transfer return",
            expr.intent,
            ty.user_facing()
        ));
    }
    if matches!(expr.kind, HirExprKind::VarSelfMethodCall { .. }) {
        return builder.lower_var_self_call(expr).map(|_| ());
    }
    if !matches!(
        expr.kind,
        HirExprKind::Call { .. } | HirExprKind::CallTraitMethodStatic { .. }
    ) {
        return Err(
            "unit return values are initially supported only for a resolved direct call"
                .to_string(),
        );
    }
    builder.lower_call(expr, false).map(|_| ())
}

/// Builder-only block state.
///
/// `None` means lowering has not filled the block yet. It is deliberately
/// distinct from `Some(SemTerminator::Unreachable)`: the latter is a completed
/// semantic CFG endpoint and must never be overwritten by later builder work.
struct PendingBlock {
    id: BlockId,
    args: Vec<BlockArg>,
    ops: Vec<SemOp>,
    terminator: Option<SemTerminator>,
    terminator_provenance: Provenance,
}

impl PendingBlock {
    fn new(id: BlockId, args: Vec<BlockArg>) -> Self {
        Self {
            id,
            args,
            ops: Vec::new(),
            terminator: None,
            terminator_provenance: Provenance::Synthesized,
        }
    }

    fn is_open(&self) -> bool {
        self.terminator.is_none()
    }

    fn append_op(&mut self, op: SemOp) -> Result<(), String> {
        if self.terminator.is_some() {
            return Err(format!(
                "SIR builder attempted to append an operation after completed block bb{}",
                self.id.0
            ));
        }
        self.ops.push(op);
        Ok(())
    }

    fn into_sem_block(self) -> Result<SemBlock, String> {
        let terminator = self.terminator.ok_or_else(|| {
            format!(
                "SIR builder left block bb{} without a semantic terminator",
                self.id.0
            )
        })?;
        Ok(SemBlock {
            id: self.id,
            args: self.args,
            ops: self.ops,
            terminator,
            terminator_provenance: self.terminator_provenance,
        })
    }
}

#[derive(Clone)]
struct LoopScope {
    label: Option<String>,
    header: BlockId,
    exit: BlockId,
    carried: Vec<BindingId>,
    preserved: BTreeSet<ValueId>,
    scope_floor: usize,
}

/// A checked local root and concrete aggregate projections. Resolving this
/// path does not read or consume the current binding version.
/// One aggregate selection: the container's type, its shape and the field.
type AggregateSelection = (ResolvedTy, AggregateShapeRef, usize);

#[derive(Clone)]
struct BindingPlace {
    binding: BindingId,
    root_ty: ResolvedTy,
    leaf_ty: ResolvedTy,
    projections: Vec<AggregateSelection>,
}

/// Non-owning aggregate fields retained during a scalar field replacement.
#[derive(Clone)]
struct ScalarAggregateParent {
    ty: ResolvedTy,
    shape: AggregateShapeRef,
    index: usize,
    fields: Vec<ValueDef>,
}

struct Builder<'hir, 'service> {
    function: Cow<'hir, HirFn>,
    service: &'service mut InstanceService<'hir>,
    callable: SemCallable,
    substitution: TypeSubstitution,
    blocks: Vec<PendingBlock>,
    /// The source site of the most recently lowered expression, which is the
    /// point a sealed terminator belongs to.
    current_site: Option<hew_hir::SiteId>,
    current: BlockId,
    values: u32,
    ops: u32,
    bindings: HashMap<BindingId, BindingTarget>,
    binding_declarations: HashMap<BindingId, usize>,
    owned_live: BTreeMap<ValueId, ResolvedTy>,
    /// Definition ancestry derived once from `SemOpKind::borrow_parent`.
    borrow_parents: HashMap<ValueId, crate::PlaceBase>,
    /// Lexical declarations only. Storage activity and payload availability
    /// belong to the verified place lifetime relation.
    scopes: Vec<Vec<BindingId>>,
    /// Loans held for the length of a lexical scope: the receiver loan behind
    /// a borrowed runtime read, whose result stays readable for as long as the
    /// binding that names it.
    scope_loans: Vec<ValueId>,
    /// One entry per open scope: the [`Self::scope_loans`] depth when the scope
    /// opened. A loan taken inside the scope ends at every exit from it,
    /// including a loop back-edge, `break` and `return`.
    scope_loan_floors: Vec<usize>,
    /// Scope loans a `let` binding names. Such a loan ends at the binding's
    /// last use rather than at the scope's exit: the point where the borrowed
    /// collection is next taken is past that last use, so the loan ends there
    /// and a later read of the binding is refused by name.
    binding_loans: Vec<BindingLoans>,
    /// Scope loans already ended ahead of their scope's exit.
    ended_loans: std::collections::HashSet<ValueId>,
    /// Open conditional constructs: `if`, and every form of `match`.
    branch_depth: usize,
    /// Every source binding this body declares, parameters first and then
    /// statement bindings in source order (§1.6).
    source_bindings: Vec<Binding>,
    params: Vec<BlockArg>,
    loops: Vec<Option<LoopScope>>,
    places: Vec<crate::PlaceDecl>,
    capture_places: HashMap<BindingId, crate::PlaceId>,
    /// Receivers already evaluated while their arguments are being lowered,
    /// and nested payload fields borrowed while a match candidate is probed.
    /// A terminating path must end these loans before owner cleanup; a probe
    /// loan also ends before its candidate fails or its owner transfers.
    argument_receiver_loans: Vec<ValueId>,
    defers: Vec<deferred::PendingDefer>,
    defer_bodies: Vec<deferred::BodyBoundary>,
    recovery_bodies: Vec<deferred::BodyBoundary>,
    task_scopes: Vec<tasks::TaskScopeFrame>,
    cleanup_may_fail: bool,
    cleanup_draining: bool,
    /// Deferred actor state seats (D447) this init body has initialized on
    /// the current path. The checker rejects a join whose arms disagree, so
    /// the set is exact at every fault exit and names what init must release.
    deferred_initialized: BTreeSet<PlaceId>,
    /// Mutable actor-state seats consumed on this path and awaiting `StoreInit`.
    state_taken: BTreeSet<PlaceId>,
    /// A stream producer body: the caller's sink it yields into and the
    /// element type each yield transfers.
    stream_sink: Option<(ValueId, ResolvedTy)>,
    /// A `var self` method's dual return while its exit cleanup runs. The
    /// receiver has already moved into it, so a failing cleanup hands the
    /// receiver back out of it.
    dual_return: Option<ValueId>,
}

#[path = "lower_aggregate.rs"]
mod aggregate;
#[path = "lower_builder_core.rs"]
mod builder_core;
#[path = "lower_call.rs"]
mod call;
#[path = "lower_control.rs"]
mod control;
#[path = "lower_expr.rs"]
mod expr;
#[path = "lower_instance.rs"]
mod instance;
#[path = "lower_stmt.rs"]
mod stmt;
#[cfg(test)]
#[path = "lower_tests.rs"]
mod tests;
