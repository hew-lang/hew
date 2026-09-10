#[path = "lower_wire.rs"]
mod wire;

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt;
use std::ops::Range;

#[path = "lower_projection.rs"]
mod projection;

#[path = "lower_actor.rs"]
mod actor;

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

use crate::ownership::{Binding, BytesLiteralId, OwnKind, StringLiteralId, TypeFactTable};
use crate::{
    AggregateShapeId, AggregateShapeRef, BindingTarget, BlockArg, BlockId, CallResult, CallUnwind,
    CallableId, CallableInstance, CheckedFailure, Edge, FunctionSourceOrigin, GenericTemplateId,
    OpId, Operand, PlaceId, PlaceOrigin, Provenance, SemAbiParam, SemAggregateField,
    SemAggregateShape, SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction,
    SemGenericTemplate, SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature, SemTerminator,
    SemVariant, SemVariantArm, SemVariantField, SemVariantShape, SirInstanceKey, ValueDef, ValueId,
    VariantShapeId,
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
}

/// The declared type parameter a receiver-pattern position names, if any.
///
/// Both encodings of a bare parameter reference are accepted: the structural
/// `TypeParam` and the argument-less `Named` spelling some producers still
/// emit.
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
        hew_types::DeclarationMarker::Resource | hew_types::DeclarationMarker::Linear
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

impl<'a> InstanceService<'a> {
    fn new(module: &'a HirModule, facts: &TypeCheckOutput) -> Self {
        let checked_facts =
            TypeFactService::new(facts.type_fact_context.clone(), facts.type_facts.clone());
        Self {
            module,
            checked_facts,
            table: CallableTable::from_hir(module),
            states: Vec::new(),
            statuses: Vec::new(),
            by_instance: HashMap::new(),
            closures: Vec::new(),
            actors: Vec::new(),
            actor_sources: HashMap::new(),
            supervisors: Vec::new(),
            synthetic_sources: HashMap::new(),
            closures_by_instance: HashMap::new(),
            closure_sources: Vec::new(),
            vtables: Vec::new(),
            vtables_by_erasure: HashMap::new(),
            entry_adapter: None,
            used_templates: std::collections::HashSet::new(),
            scanned_record_closes: 0,
            pending: VecDeque::new(),
            functions: Vec::new(),
            aggregate_shapes: Vec::new(),
            aggregate_shapes_by_type: HashMap::new(),
            variant_shapes: Vec::new(),
            variant_shapes_by_type: HashMap::new(),
            string_literals: BTreeMap::new(),
            bytes_literals: BTreeMap::new(),
            value_capabilities: BTreeMap::new(),
            wire_plans: HashMap::new(),
        }
    }

    fn callable(&self, id: CallableId) -> Option<&SemCallable> {
        self.table.callable(id)
    }

    fn require_key_capabilities(&mut self, ty: &ResolvedTy) -> Result<(), String> {
        for capability in [
            hew_types::ValueCapability::Hash,
            hew_types::ValueCapability::Eq,
        ] {
            self.require_value_capability(ty, capability)?;
        }
        Ok(())
    }

    fn require_value_capability(
        &mut self,
        ty: &ResolvedTy,
        capability: hew_types::ValueCapability,
    ) -> Result<(), String> {
        let key = (ty.clone(), capability);
        if self.value_capabilities.contains_key(&key) {
            return Ok(());
        }
        self.require_type_facts(ty)?;
        let selected = self
            .checked_facts
            .capability_plan(ty, capability)
            .map_err(|error| {
                format!(
                    "cannot select {capability:?} for `{}`: {error}",
                    ty.user_facing()
                )
            })?
            .ok_or_else(|| {
                format!(
                    "`{}` has no selected {capability:?} implementation",
                    ty.user_facing()
                )
            })?;
        let callable = match selected.plan() {
            hew_types::ValueMethodPlan::Derived => None,
            hew_types::ValueMethodPlan::User { method, type_args } => {
                let callable = if self.table.templates.contains_key(method) {
                    self.request_instance(method, type_args.clone())?
                } else {
                    if !type_args.is_empty() {
                        return Err("selected nongeneric capability has type arguments".to_string());
                    }
                    let id = self.admit_monomorphic(method).map_err(|reason| {
                        format!(
                            "selected capability `{}` has no admitted HIR callable: {reason}",
                            method.full_path()
                        )
                    })?;
                    self.request_body(id);
                    id
                };
                let metadata = self.callable(callable).ok_or_else(|| {
                    "selected capability callable disappeared from its table".to_string()
                })?;
                let facts = self
                    .checked_facts
                    .rows()
                    .get(&TypeInstanceKey(ty.clone()))
                    .ok_or_else(|| "selected capability type facts disappeared".to_string())?;
                crate::capability::verify_capability_signature(ty, capability, metadata, *facts)?;
                Some(callable)
            }
        };
        let derived = matches!(selected.plan(), hew_types::ValueMethodPlan::Derived);
        self.value_capabilities.insert(
            key.clone(),
            crate::SemValueMethodPlan {
                selection: selected,
                callable,
            },
        );
        if derived {
            let result = crate::derived_capability_components(
                ty,
                &self.aggregate_shapes,
                &self.variant_shapes,
            )
            .and_then(|components| {
                for component in components {
                    self.require_value_capability(&component, capability)?;
                }
                Ok(())
            });
            if let Err(reason) = result {
                self.value_capabilities.remove(&key);
                return Err(reason);
            }
        }
        Ok(())
    }

    fn require_type_facts(&mut self, ty: &ResolvedTy) -> Result<(), String> {
        require_type_shapes(
            self.module,
            &mut self.checked_facts,
            &mut self.aggregate_shapes,
            &mut self.aggregate_shapes_by_type,
            &mut self.variant_shapes,
            &mut self.variant_shapes_by_type,
            ty,
        )
    }

    /// Intern the exact checker-resolved shape of one concrete aggregate.
    ///
    /// Tuples are structural. Named records resolve through the checker type
    /// service and the declaration identity carried by `NominalInstance`.
    fn require_aggregate_shape(
        &mut self,
        aggregate_ty: &ResolvedTy,
    ) -> Result<AggregateShapeRef, String> {
        require_aggregate_shape(
            self.module,
            &mut self.checked_facts,
            &mut self.aggregate_shapes,
            &mut self.aggregate_shapes_by_type,
            aggregate_ty,
        )
    }

    fn require_variant_shape(&mut self, enum_ty: &ResolvedTy) -> Result<VariantShapeId, String> {
        require_variant_shape(
            self.module,
            &mut self.checked_facts,
            &mut self.variant_shapes,
            &mut self.variant_shapes_by_type,
            enum_ty,
        )
    }

    fn require_runtime_variant_result_shapes(
        &mut self,
        kind: hew_types::RuntimeVariantResultKind,
        result_ty: &ResolvedTy,
    ) -> Result<(), String> {
        let (_, error_ty) = kind.payload_types(result_ty).ok_or_else(|| {
            format!(
                "runtime variant result contract does not admit `{}`",
                result_ty.user_facing()
            )
        })?;
        self.require_variant_shape(result_ty)?;
        let AggregateShapeRef::Record(error_shape) = self.require_aggregate_shape(error_ty)? else {
            return Err("runtime variant error must be an exact named record".to_string());
        };
        let error_len_ty = self
            .aggregate_shapes
            .get(usize::try_from(error_shape.0).map_err(|_| {
                format!(
                    "runtime variant error shape {} is out of range",
                    error_shape.0
                )
            })?)
            .and_then(|shape| shape.fields.iter().find(|field| field.name == "error_len"))
            .map(|field| field.ty.clone())
            .ok_or_else(|| "runtime variant error has no error_len field".to_string())?;
        self.require_variant_shape(&error_len_ty)?;
        crate::runtime_variant_shape_refs(
            kind,
            result_ty,
            &self.aggregate_shapes,
            &self.variant_shapes,
        )?;
        Ok(())
    }

    fn require_signature_shapes(&mut self, signature: &SemSignature) -> Result<(), String> {
        let prior_aggregate_count = self.aggregate_shapes.len();
        let prior_variant_count = self.variant_shapes.len();
        let result = require_signature_shapes(
            self.module,
            &mut self.checked_facts,
            &mut self.aggregate_shapes,
            &mut self.aggregate_shapes_by_type,
            &mut self.variant_shapes,
            &mut self.variant_shapes_by_type,
            signature,
        );
        if result.is_err() {
            self.aggregate_shapes.truncate(prior_aggregate_count);
            self.aggregate_shapes_by_type
                .retain(|_, id| usize::try_from(id.0).is_ok_and(|id| id < prior_aggregate_count));
            self.variant_shapes.truncate(prior_variant_count);
            self.variant_shapes_by_type
                .retain(|_, id| usize::try_from(id.0).is_ok_and(|id| id < prior_variant_count));
        }
        result
    }

    fn intern_string(&mut self, value: &str) -> StringLiteralId {
        if let Some((id, _)) = self
            .string_literals
            .iter()
            .find(|(_, existing)| existing.as_str() == value)
        {
            return *id;
        }
        let id = StringLiteralId(
            u32::try_from(self.string_literals.len())
                .expect("SIR string literal count exceeds u32"),
        );
        self.string_literals.insert(id, value.to_string());
        id
    }

    fn intern_bytes(&mut self, value: &[u8]) -> BytesLiteralId {
        if let Some((id, _)) = self
            .bytes_literals
            .iter()
            .find(|(_, existing)| existing.as_slice() == value)
        {
            return *id;
        }
        let id = BytesLiteralId(
            u32::try_from(self.bytes_literals.len()).expect("SIR bytes literal count exceeds u32"),
        );
        self.bytes_literals.insert(id, value.to_vec());
        id
    }

    /// Seed the worklist with the module's resolved entry callable.
    ///
    /// A module without one is not an executable program, so it has no demand
    /// and lowers nothing.
    fn request_entry(&mut self) {
        let Some(declaration) = self
            .table
            .entry_exit_plan
            .as_ref()
            .map(|plan| plan.entry.clone())
        else {
            return;
        };
        let Ok(entry) = self.admit_monomorphic(&declaration) else {
            return;
        };
        let result_plan = self
            .table
            .entry_exit_plan
            .as_ref()
            .is_some_and(|plan| matches!(plan.action, EntryExitAction::Result { .. }));
        if !result_plan {
            self.request_body(entry);
            return;
        }
        // A Result entry exits through a synthesized adapter. SIR consumes the
        // checker's action here; the module publishes the integer status the
        // adapter returns as the physical-facing exit action.
        let source = self
            .table
            .callable(entry)
            .cloned()
            .expect("entry callable exists in its table");
        let id = CallableId(
            u32::try_from(self.table.callables.len())
                .expect("SIR callable count exceeds the module-local ID range"),
        );
        self.table.callables.push(SemCallable {
            id,
            function: source.function,
            declaration: source.declaration,
            instance: CallableInstance::EntryAdapter,
            symbol: "__hew_entry".to_string(),
            source_origin: source.source_origin,
            signature: SemSignature {
                params: Vec::new(),
                return_ty: ResolvedTy::I64,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        self.states.push(CallableState::Unreached);
        self.statuses.push(None);
        self.table.root_unit_callables.push(id);
        self.table.entry_callable = Some(id);
        let plan = self
            .table
            .entry_exit_plan
            .as_mut()
            .expect("a Result entry plan was just observed");
        let action = std::mem::replace(
            &mut plan.action,
            EntryExitAction::Integer(hew_types::EntryIntegerType::I64),
        );
        self.entry_adapter = Some(EntryAdapter {
            callable: id,
            entry,
            action,
        });
        self.request_body(id);
    }

    /// Intern the dispatch table for one `(dyn Trait, concrete type)` erasure.
    ///
    /// Each slot resolves the checker's implementer declaration to a demanded
    /// SIR callable, so no later stage joins a slot to a body by name. The
    /// slot order is the checker's, past the runtime's three-word prefix.
    fn request_vtable(
        &mut self,
        dyn_ty: &ResolvedTy,
        concrete_ty: &ResolvedTy,
        entries: &[hew_types::DynVtableEntry],
    ) -> Result<crate::SemVtableId, String> {
        let key = (dyn_ty.clone(), concrete_ty.clone());
        if let Some(id) = self.vtables_by_erasure.get(&key) {
            return Ok(*id);
        }
        self.require_type_facts(dyn_ty)?;
        self.require_type_facts(concrete_ty)?;
        let mut slots = Vec::with_capacity(entries.len());
        for (index, entry) in entries.iter().enumerate() {
            let slot = 3 + u32::try_from(index)
                .map_err(|_| "trait-object method count exceeds u32".to_string())?;
            let declaration = entry.impl_method.as_ref().ok_or_else(|| {
                format!(
                    "`{}` fills slot {slot} of `{}` with `{}`, which has no source declaration",
                    concrete_ty.user_facing(),
                    dyn_ty.user_facing(),
                    entry.impl_fn_key
                )
            })?;
            let callee = self.admit_monomorphic(declaration).map_err(|reason| {
                format!(
                    "slot {slot} of `{}` names `{}`, which has no monomorphic SIR callable: {reason}",
                    dyn_ty.user_facing(),
                    declaration.full_path()
                )
            })?;
            // Erasure is what obliges the module to carry every slot body:
            // the dispatch edge cannot demand one, because it names an index
            // rather than a declaration.
            self.request_body(callee);
            let target = self
                .callable(callee)
                .cloned()
                .ok_or_else(|| format!("SIR callable {callee:?} is absent from its table"))?;
            let Some((receiver, arguments)) = target.signature.params.split_first() else {
                return Err(format!(
                    "slot {slot} implementation `{}` takes no receiver",
                    target.symbol
                ));
            };
            let receiver_passing = dyn_receiver_passing(&entry.signature);
            if receiver.ty != *concrete_ty
                || !dyn_passing_admits(receiver_passing, receiver.passing)
            {
                return Err(format!(
                    "slot {slot} implementation `{}` does not receive `{}` on the erased boundary",
                    target.symbol,
                    concrete_ty.user_facing()
                ));
            }
            let mut params = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let passing =
                    dyn_boundary_passing(OwnKind::of_ty(&argument.ty, self.checked_facts.rows())?);
                if !dyn_passing_admits(passing, argument.passing) {
                    return Err(format!(
                        "slot {slot} implementation `{}` changes the erased transfer of `{}`",
                        target.symbol,
                        argument.ty.user_facing()
                    ));
                }
                params.push(SemAbiParam {
                    ty: argument.ty.clone(),
                    passing,
                    caller_visible_projection: false,
                });
            }
            slots.push(crate::SemVtableSlot {
                slot,
                trait_name: entry.trait_name.clone(),
                method_name: entry.method_name.clone(),
                callee,
                receiver: receiver_passing,
                signature: SemSignature {
                    params,
                    return_ty: target.signature.return_ty.clone(),
                },
            });
        }
        let id = crate::SemVtableId(
            u32::try_from(self.vtables.len())
                .map_err(|_| "SIR vtable count exceeds u32".to_string())?,
        );
        self.vtables.push(crate::SemVtable {
            id,
            dyn_ty: dyn_ty.clone(),
            concrete_ty: concrete_ty.clone(),
            slots,
        });
        self.vtables_by_erasure.insert(key, id);
        Ok(id)
    }

    /// The checker-selected `Display::fmt` body for the entry error type.
    fn resolve_entry_display(
        &mut self,
        declaration: &DefId,
        instance: &hew_types::EntryCallableInstance,
    ) -> Result<SemCallable, String> {
        let id = match instance {
            hew_types::EntryCallableInstance::Declared => {
                let id = self.admit_monomorphic(declaration).map_err(|reason| {
                    format!(
                        "entry Display target `{}` has no SIR callable: {reason}",
                        declaration.full_path()
                    )
                })?;
                self.request_body(id);
                id
            }
            hew_types::EntryCallableInstance::Generic { type_args } => {
                self.request_instance(declaration, type_args.clone())?
            }
        };
        self.callable(id)
            .cloned()
            .ok_or_else(|| format!("SIR callable {id:?} is absent from its deterministic table"))
    }

    /// Seed exact caller-selected declarations after validating the complete
    /// set. No body is queued until every root is admitted, and a failed
    /// request publishes no module at all, so one bad root cannot leave a
    /// partially selected lowering behind.
    fn request_roots(&mut self, roots: &[DefId]) -> Result<(), Vec<SirRootSelectionError>> {
        let mut callables = Vec::new();
        let mut errors = Vec::new();
        for declaration in roots.iter().collect::<BTreeSet<_>>() {
            if self.table.templates.contains_key(declaration) {
                errors.push(SirRootSelectionError {
                    declaration: (*declaration).clone(),
                    reason: "generic declarations require a concrete call-site specialization"
                        .to_string(),
                });
                continue;
            }
            match self.admit_monomorphic(declaration) {
                Ok(callable) => callables.push(callable),
                Err(reason) => errors.push(SirRootSelectionError {
                    declaration: (*declaration).clone(),
                    reason,
                }),
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        callables.sort_unstable();
        callables.dedup();
        for callable in callables {
            self.request_body(callable);
        }
        Ok(())
    }

    /// Seed the worklist with every admitted header, in `CallableId` order.
    ///
    /// Generic templates have no header of their own; their instances are
    /// still minted only by resolved call edges, so an uncalled template stays
    /// unproven and its status says so.
    fn request_every_callable(&mut self) {
        for declaration in self.table.admissible_order.clone() {
            if let Ok(id) = self.admit_monomorphic(&declaration) {
                self.request_body(id);
            }
        }
    }

    /// Mint the SIR header for one monomorphic declaration, once.
    ///
    /// A header is what a resolved call names, and publishing one obliges the
    /// module to carry its signature's aggregate shapes, variant shapes and
    /// type-fact rows. Admission is therefore demand-driven in the same way
    /// body lowering is: a prelude declaration nothing reachable calls never
    /// becomes a callable, so it puts no record shape, collection glue or row
    /// into a program that does not use it.
    ///
    /// A refusal is recorded once, keyed by the declaration a call would name,
    /// and reported at the call site that wanted it.
    fn admit_monomorphic(&mut self, declaration: &DefId) -> Result<CallableId, String> {
        if let Some(id) = self.table.monomorphic_by_declaration.get(declaration) {
            return Ok(*id);
        }
        if let Some(reason) = self.table.ineligible.get(declaration) {
            return Err(reason.clone());
        }
        let Some(admissible) = self.table.admissible.get(declaration) else {
            return Err(
                "the declaration is not present as a HIR function in this module".to_string(),
            );
        };
        let function = admissible.function;
        let symbol = admissible.symbol.clone();
        let signature = callable_signature(self.module, function, &mut self.checked_facts)
            .and_then(|signature| {
                self.require_signature_shapes(&signature)?;
                Ok(signature)
            });
        let signature = match signature {
            Ok(signature) => signature,
            Err(reason) => {
                self.table
                    .ineligible
                    .insert(declaration.clone(), reason.clone());
                return Err(reason);
            }
        };
        let id = CallableId(
            u32::try_from(self.table.callables.len())
                .expect("SIR callable count exceeds the module-local ID range"),
        );
        let source_origin = function_source_origin(self.module, function);
        if source_origin == FunctionSourceOrigin::RootUnit {
            self.table.root_unit_callables.push(id);
        }
        // Entry selection joins on HIR's resolved entry declaration. SIR never
        // re-applies the language's entry rule, so it never compares a
        // declaration path or an emitted symbol against "main". A fact that
        // names a non-root declaration is admitted here and rejected by the
        // verifier's entry rule rather than silently dropped.
        if self.module.entry_exit_plan.as_ref().map(|plan| &plan.entry) == Some(declaration) {
            self.table.entry_callable = Some(id);
        }
        self.table
            .monomorphic_by_declaration
            .insert(declaration.clone(), id);
        self.table.callables.push(SemCallable {
            id,
            function: function.id,
            declaration: declaration.clone(),
            instance: CallableInstance::Monomorphic,
            symbol,
            source_origin,
            signature,
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        self.states.push(CallableState::Unreached);
        self.statuses.push(None);
        Ok(id)
    }

    /// Record demand for one callable's body, once.
    fn request_body(&mut self, callable: CallableId) {
        if self.state(callable) != Some(CallableState::Unreached) {
            return;
        }
        self.set_state(callable, CallableState::Queued);
        self.pending.push_back(callable);
    }

    /// Demand the `close` body of every newly admitted `#[resource]` record.
    ///
    /// Drop glue is the only caller of a record's `close`, so the demand
    /// cannot arrive through the call graph: admitting the type is what
    /// obliges the module to carry its release.
    fn demand_record_closes(&mut self) {
        let mut closes = Vec::new();
        while self.scanned_record_closes < self.aggregate_shapes.len() {
            let index = self.scanned_record_closes;
            self.scanned_record_closes += 1;
            let shape = &self.aggregate_shapes[index];
            if shape.marker != hew_types::DeclarationMarker::Resource {
                continue;
            }
            let Some(lifecycle) =
                crate::resource::record_resource_lifecycle(self.module, &shape.aggregate_ty)
            else {
                continue;
            };
            closes.push(lifecycle.close_declaration.clone());
        }
        for declaration in closes {
            if let Ok(id) = self.admit_monomorphic(&declaration) {
                self.request_body(id);
            }
        }
    }

    fn lower_pending(&mut self) {
        loop {
            self.demand_record_closes();
            let Some(callable) = self.pending.pop_front() else {
                break;
            };
            if self.state(callable) != Some(CallableState::Queued) {
                continue;
            }
            self.set_state(callable, CallableState::Lowering);
            let result = self.lower_callable(callable);
            self.record_callable_result(callable, result);
        }
    }

    fn lower_callable(&mut self, callable: CallableId) -> Result<SemFunction, String> {
        let input = self.input_for_callable(callable)?;
        Builder::new(
            input.function,
            input.callable,
            input.substitution,
            &input.source,
            self,
        )?
        .lower(input.source)
    }

    fn record_callable_result(
        &mut self,
        callable: CallableId,
        result: Result<SemFunction, String>,
    ) {
        let index = usize::try_from(callable.0).expect("SIR callable id exceeds usize");
        match result {
            Ok(function) => {
                self.states[index] = CallableState::Lowered;
                self.statuses[index] = Some(SirLoweringStatus::Lowered);
                self.functions.push(function);
            }
            Err(reason) => {
                self.states[index] = CallableState::Failed;
                self.statuses[index] = Some(SirLoweringStatus::Unsupported { reason });
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "each callable instance kind selects its own source and substitution together"
    )]
    fn input_for_callable(&self, callable: CallableId) -> Result<LoweringInput<'a>, String> {
        let callable_meta = self.callable(callable).cloned().ok_or_else(|| {
            format!(
                "SIR callable {} is absent from its deterministic table",
                callable.0
            )
        })?;
        if let CallableInstance::Closure(id) = callable_meta.instance {
            let closure = self
                .closures
                .get(id.0 as usize)
                .ok_or_else(|| "closure body has no enclosing callable".to_string())?;
            let (expression, substitution) = self
                .closure_sources
                .get(id.0 as usize)
                .ok_or_else(|| "closure body has no checked literal source".to_string())?;
            // A closure inherits the source of its exact enclosing callable.
            // Actor handlers have synthesized HIR functions outside the
            // ordinary item table; nested closures must retain that source too.
            let parent = self.input_for_callable(closure.instance.enclosing)?;
            return Ok(LoweringInput {
                function: parent.function,
                callable: callable_meta,
                substitution: substitution.clone(),
                source: BodySource::Closure(expression.clone()),
            });
        }
        if let SemCallableKind::HewActor(actor) = callable_meta.kind {
            let (function, state_bindings, substitution) = self
                .actor_sources
                .get(&callable)
                .ok_or_else(|| "actor body has no checked HIR source".to_string())?;
            return Ok(LoweringInput {
                function: Cow::Owned(function.clone()),
                callable: callable_meta,
                substitution: substitution.clone(),
                source: BodySource::Actor {
                    actor,
                    state_bindings: state_bindings.clone(),
                },
            });
        }
        if let Some((function, substitution)) = self.synthetic_sources.get(&callable) {
            return Ok(LoweringInput {
                function: Cow::Owned(function.clone()),
                callable: callable_meta,
                substitution: substitution.clone(),
                source: BodySource::Function,
            });
        }
        let function = *self
            .table
            .functions_by_item
            .get(&callable_meta.function)
            .ok_or_else(|| {
                format!(
                    "SIR callable `{}` has no HIR source template for its provenance item",
                    callable_meta.symbol
                )
            })?;
        if callable_meta.instance == CallableInstance::EntryAdapter {
            let adapter = self
                .entry_adapter
                .clone()
                .filter(|adapter| adapter.callable == callable)
                .ok_or("entry adapter has no exit plan")?;
            return Ok(LoweringInput {
                function: Cow::Borrowed(function),
                callable: callable_meta,
                substitution: TypeSubstitution::empty(),
                source: BodySource::EntryAdapter(adapter),
            });
        }
        let substitution = match &callable_meta.instance {
            CallableInstance::ActorMember
            | CallableInstance::Closure(_)
            | CallableInstance::EntryAdapter
            | CallableInstance::SupervisorChild { .. } => {
                unreachable!("closure, entry adapter and child spawn inputs are resolved above")
            }
            CallableInstance::Monomorphic => {
                if !function.type_params.is_empty() {
                    return Err(format!(
                        "generic HIR template `{}` was incorrectly admitted as a monomorphic SIR body",
                        function.declaration.full_path()
                    ));
                }
                TypeSubstitution::empty()
            }
            CallableInstance::Generic(key) => {
                if key.template.declaration != function.declaration
                    || callable_meta.declaration != function.declaration
                {
                    return Err(format!(
                        "SIR generic callable `{}` does not agree with its source template declaration",
                        callable_meta.symbol
                    ));
                }
                TypeSubstitution::for_instance(function, &key.type_args)?
            }
        };
        Ok(LoweringInput {
            function: Cow::Borrowed(function),
            callable: callable_meta,
            substitution,
            source: BodySource::Function,
        })
    }

    fn resolve_direct_call(
        &mut self,
        declaration: &DefId,
        site: hew_hir::SiteId,
        substitution: &TypeSubstitution,
    ) -> Result<SemCallable, String> {
        if self.table.templates.contains_key(declaration) {
            let raw_args = self.module.call_site_type_args.get(&site).ok_or_else(|| {
                format!(
                    "generic direct call to `{}` is missing checker-resolved type arguments at SIR site {}",
                    declaration.full_path(),
                    site.0
                )
            })?;
            let type_args = raw_args
                .iter()
                .map(|argument| substitution.apply(argument))
                .collect::<Vec<_>>();
            let id = self.request_instance(declaration, type_args)?;
            return self.callable(id).cloned().ok_or_else(|| {
                format!(
                    "requested SIR generic callable {} disappeared from its table",
                    id.0
                )
            });
        }
        let id = self.admit_monomorphic(declaration).map_err(|reason| {
            format!(
                "direct callee `{}` has no scalar default-call SIR callable: {reason}",
                declaration.full_path()
            )
        })?;
        // Resolving a call edge is what makes the callee reachable, so this is
        // where its body becomes demanded. Generic callees go through
        // `request_instance`, which queues the instance it mints.
        self.request_body(id);
        self.callable(id)
            .cloned()
            .ok_or_else(|| format!("SIR callable {id:?} is absent from its deterministic table"))
    }

    /// Select the implementation a static trait call reaches, from the
    /// receiver type this instance's substitution produced.
    ///
    /// The generic template could not name it: `it.next()` under
    /// `I: Iterator<Item = A>` has no implementation until `I` is bound. The
    /// selection reads HIR's structured impl index by declaration identity —
    /// never a symbol spelling — and then enters the ordinary direct-call
    /// admission for the implementation it found.
    fn resolve_static_trait_call(
        &mut self,
        declaring_trait: &DefId,
        method: &DefId,
        receiver_ty: &ResolvedTy,
        site: hew_hir::SiteId,
        substitution: &TypeSubstitution,
    ) -> Result<SemCallable, String> {
        let self_type = hew_hir::dispatch::receiver_self_type_for_impl_lookup_instance(receiver_ty)
            .ok_or_else(|| {
                format!(
                    "static trait receiver `{}` cannot anchor an implementation",
                    receiver_ty.user_facing()
                )
            })?;
        let entry = hew_hir::dispatch::lookup_trait_impl_entry_by_id(
            &self.table.trait_impls,
            declaring_trait,
            &self_type,
            method,
        )
        .cloned()
        .ok_or_else(|| {
            format!(
                "no implementation of `{}` for `{}` provides `{}`",
                declaring_trait.full_path(),
                receiver_ty.user_facing(),
                method.full_path()
            )
        })?;
        if !self.table.templates.contains_key(&entry.method) {
            let id = self.admit_monomorphic(&entry.method).map_err(|reason| {
                format!(
                    "static trait callee `{}` has no scalar default-call SIR callable: {reason}",
                    entry.method.full_path()
                )
            })?;
            self.request_body(id);
            return self.callable(id).cloned().ok_or_else(|| {
                format!("SIR callable {id:?} is absent from its deterministic table")
            });
        }
        let type_args = self.static_trait_instance_args(&entry, &self_type, site, substitution)?;
        let id = self.request_instance(&entry.method, type_args)?;
        self.callable(id).cloned().ok_or_else(|| {
            format!(
                "requested SIR generic callable {} disappeared from its table",
                id.0
            )
        })
    }

    /// Bind impl parameters from the concrete receiver and append the method
    /// parameters selected by the checker at this call site.
    ///
    /// `impl<A, B> Trait for Pair<B, A>` spells its self-type arguments in the
    /// opposite order to its parameter list, so the receiver's arguments are
    /// matched against the implementation's own receiver pattern rather than
    /// handed to the instance positionally.
    fn static_trait_instance_args(
        &self,
        entry: &hew_hir::dispatch::TraitImplMethodEntry,
        self_type: &hew_types::NominalInstance,
        site: hew_hir::SiteId,
        substitution: &TypeSubstitution,
    ) -> Result<Vec<ResolvedTy>, String> {
        let method = &entry.method;
        let function = self
            .table
            .templates
            .get(method)
            .ok_or_else(|| {
                format!(
                    "generic implementation `{}` has no SIR template admission record",
                    method.full_path()
                )
            })?
            .function;
        let impl_param_count = entry.impl_type_params.len();
        if !function.type_params.starts_with(&entry.impl_type_params) {
            return Err(format!(
                "generic implementation `{}` has inconsistent impl parameter declarations",
                method.full_path()
            ));
        }
        let method_param_count = function.type_params.len() - impl_param_count;
        let method_args = self.module.call_site_type_args.get(&site);
        if method_args.map_or(0, Vec::len) != method_param_count {
            return Err(format!(
                "static trait call to `{}` requires {method_param_count} checker-resolved method type argument(s) at SIR site {}, found {}",
                method.full_path(),
                site.0,
                method_args.map_or(0, Vec::len),
            ));
        }
        let method_args = method_args
            .into_iter()
            .flatten()
            .map(|argument| substitution.apply(argument));
        if impl_param_count == 0 {
            return Ok(method_args.collect());
        }
        let Some(ResolvedTy::Named {
            args: pattern_args, ..
        }) = function.params.first().map(|param| &param.ty)
        else {
            return Err(format!(
                "generic implementation `{}` has no nominal receiver pattern",
                method.full_path()
            ));
        };
        if pattern_args.len() != self_type.args.len() {
            return Err(format!(
                "generic implementation `{}` declares {} receiver argument(s), the concrete receiver carries {}",
                method.full_path(),
                pattern_args.len(),
                self_type.args.len()
            ));
        }
        let mut bindings: HashMap<&str, &ResolvedTy> = HashMap::new();
        for (pattern, concrete) in pattern_args.iter().zip(&self_type.args) {
            let name = declared_type_param_name(pattern, &entry.impl_type_params).ok_or_else(|| {
                format!(
                    "generic implementation `{}` receives `{}` in a position SIR cannot bind to a type parameter",
                    method.full_path(),
                    pattern.user_facing()
                )
            })?;
            if bindings
                .insert(name, concrete)
                .is_some_and(|prior| prior != concrete)
            {
                return Err(format!(
                    "generic implementation `{}` binds type parameter `{name}` to two different types",
                    method.full_path()
                ));
            }
        }
        let mut arguments = entry
            .impl_type_params
            .iter()
            .map(|param| {
                bindings.get(param.as_str()).map_or_else(
                    || {
                        Err(format!(
                            "generic implementation `{}` leaves type parameter `{param}` unbound by its receiver",
                            method.full_path()
                        ))
                    },
                    |ty| Ok((*ty).clone()),
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        arguments.extend(method_args);
        Ok(arguments)
    }

    fn request_closure(
        &mut self,
        enclosing: CallableId,
        expression: &HirExpr,
        substitution: &TypeSubstitution,
    ) -> Result<crate::ClosureId, String> {
        let instance = crate::ClosureInstanceKey {
            enclosing,
            literal: expression.node,
        };
        if let Some(id) = self.closures_by_instance.get(&instance) {
            return Ok(*id);
        }
        let HirExprKind::Closure { captures, .. } = &expression.kind else {
            return Err("closure demand requires a checked literal".to_string());
        };
        let parent = self
            .callable(enclosing)
            .cloned()
            .ok_or_else(|| "closure has no enclosing instance".to_string())?;
        let ty = substitution.apply(&expression.ty);
        self.require_type_facts(&ty)?;
        let (_, _, capabilities) = crate::callable_parts(&ty)?;
        let fields: Vec<_> = captures
            .iter()
            .map(|capture| crate::SemCaptureField {
                binding: capture.binding,
                ty: substitution.apply(&capture.ty),
                access: capture.access,
                consumption: capture.consumption,
            })
            .collect();
        for field in &fields {
            self.require_type_facts(&field.ty)?;
        }
        let mut signature = crate::callable_value_signature(&ty, self.checked_facts.rows())?;
        signature.params.insert(
            0,
            SemAbiParam {
                ty: ty.clone(),
                passing: match capabilities.call {
                    hew_types::CallableCallMode::Read => SemParamPassing::Borrow,
                    hew_types::CallableCallMode::Var => SemParamPassing::BorrowMut,
                    hew_types::CallableCallMode::Once => SemParamPassing::Consume,
                },
                caller_visible_projection: capabilities.call == hew_types::CallableCallMode::Var,
            },
        );
        self.require_signature_shapes(&signature)?;
        let id = crate::ClosureId(
            u32::try_from(self.closures.len())
                .map_err(|_| "closure count exceeds u32".to_string())?,
        );
        let body = CallableId(
            u32::try_from(self.table.callables.len())
                .map_err(|_| "callable count exceeds u32".to_string())?,
        );
        let symbol = format!("{}$closure${}", parent.symbol, expression.node.0);
        if self
            .table
            .callables
            .iter()
            .any(|callable| callable.symbol == symbol)
        {
            return Err("closure symbol conflicts with another exact callable".to_string());
        }
        self.closures.push(crate::SemClosure {
            generator_yield: None,
            id,
            instance,
            body,
            ty,
            fields,
        });
        self.closure_sources
            .push((Box::new(expression.clone()), substitution.clone()));
        self.closures_by_instance.insert(instance, id);
        self.table.callables.push(SemCallable {
            id: body,
            instance: CallableInstance::Closure(id),
            symbol,
            signature,
            kind: SemCallableKind::HewClosure,
            ..parent
        });
        self.states.push(CallableState::Queued);
        self.statuses.push(None);
        self.pending.push_back(body);
        Ok(id)
    }

    fn request_instance(
        &mut self,
        declaration: &DefId,
        type_args: Vec<ResolvedTy>,
    ) -> Result<CallableId, String> {
        let template = self
            .table
            .templates
            .get(declaration)
            .cloned()
            .ok_or_else(|| {
                format!(
                    "generic direct callee `{}` has no SIR template admission record",
                    declaration.full_path()
                )
            })?;
        if type_args.len() != template.function.type_params.len() {
            return Err(format!(
                "generic direct callee `{}` expects {} type argument(s), HIR supplied {}",
                declaration.full_path(),
                template.function.type_params.len(),
                type_args.len()
            ));
        }
        for (index, argument) in type_args.iter().enumerate() {
            if *argument != ResolvedTy::Never
                && !is_supported_call_value(self.module, &self.checked_facts, argument)
            {
                return Err(format!(
                    "generic direct callee `{}` type argument {index} is `{}`; SIR generic instances require a concrete semantic value contract",
                    declaration.full_path(),
                    argument.user_facing()
                ));
            }
        }
        for argument in &type_args {
            if *argument != ResolvedTy::Never {
                self.require_type_facts(argument)?;
            }
        }
        let key = SirInstanceKey {
            template: template.id,
            type_args,
        };
        self.used_templates.insert(key.template.clone());
        if let Some(existing) = self.by_instance.get(&key).copied() {
            return Ok(existing);
        }
        if self.by_instance.len() >= SIR_GENERIC_INSTANCE_CAP {
            return Err(format!(
                "SIR generic instance cap ({SIR_GENERIC_INSTANCE_CAP}) exceeded while specializing `{}`; refuse unbounded semantic specialization",
                declaration.full_path()
            ));
        }
        let substitution = TypeSubstitution::for_instance(template.function, &key.type_args)?;
        let signature = callable_signature_with_substitution(
            self.module,
            template.function,
            &substitution,
            &mut self.checked_facts,
        )?;
        self.require_signature_shapes(&signature)?;
        let symbol =
            hew_hir::monomorph::function_monomorph_symbol(&template.symbol, &key.type_args);
        if let Some(existing) = self
            .table
            .callables
            .iter()
            .find(|callable| callable.symbol == symbol)
        {
            return Err(format!(
                "SIR generic instance `{}` would collide with callable {} despite a distinct semantic key",
                symbol, existing.id.0
            ));
        }
        let id = CallableId(
            u32::try_from(self.table.callables.len())
                .map_err(|_| "SIR callable count exceeds the module-local ID range".to_string())?,
        );
        self.table.callables.push(SemCallable {
            id,
            function: template.function.id,
            declaration: template.function.declaration.clone(),
            instance: CallableInstance::Generic(key.clone()),
            symbol,
            source_origin: template.source_origin,
            signature,
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        self.by_instance.insert(key, id);
        self.states.push(CallableState::Queued);
        self.statuses.push(None);
        self.pending.push_back(id);
        Ok(id)
    }

    fn source_status(&self, function: &HirFn) -> SirLoweringStatus {
        if self.table.templates.contains_key(&function.declaration) {
            let (instances, failed_instances) =
                self.template_instance_counts(&function.declaration);
            return SirLoweringStatus::GenericTemplate {
                instances,
                failed_instances,
            };
        }
        if let Some(callable) = self
            .table
            .monomorphic_by_declaration
            .get(&function.declaration)
            .copied()
        {
            return self.callable_status(callable);
        }
        // No minted header. A recorded refusal means demand did reach the
        // declaration and admission refused it; anything else means nothing
        // asked for it.
        self.table.ineligible.get(&function.declaration).map_or(
            SirLoweringStatus::NotReached,
            |reason| SirLoweringStatus::Unsupported {
                reason: reason.clone(),
            },
        )
    }

    /// The recorded outcome for one admitted callable header.
    ///
    /// A header the entry closure never demanded has no recorded status; that
    /// is [`SirLoweringStatus::NotReached`], never a body failure.
    fn callable_status(&self, callable: CallableId) -> SirLoweringStatus {
        let index = usize::try_from(callable.0).expect("SIR callable id exceeds usize");
        self.statuses
            .get(index)
            .cloned()
            .flatten()
            .unwrap_or(SirLoweringStatus::NotReached)
    }

    fn template_instance_counts(&self, declaration: &DefId) -> (usize, usize) {
        let mut instances = 0;
        let mut failed = 0;
        for (key, callable) in &self.by_instance {
            if &key.template.declaration == declaration {
                instances += 1;
                if self.state(*callable) == Some(CallableState::Failed) {
                    failed += 1;
                }
            }
        }
        (instances, failed)
    }

    fn state(&self, callable: CallableId) -> Option<CallableState> {
        self.states.get(usize::try_from(callable.0).ok()?).copied()
    }

    fn set_state(&mut self, callable: CallableId, state: CallableState) {
        let index = usize::try_from(callable.0).expect("SIR callable id exceeds usize");
        self.states[index] = state;
    }

    fn into_module(self) -> SemModule {
        let Self {
            module,
            table,
            checked_facts,
            used_templates,
            mut functions,
            closures,
            actors,
            supervisors,
            vtables,
            aggregate_shapes,
            variant_shapes,
            string_literals,
            bytes_literals,
            value_capabilities,
            ..
        } = self;
        let regex_patterns: Vec<String> = module
            .regex_literals
            .iter()
            .map(|literal| literal.pattern.clone())
            .collect();
        let generic_templates: Vec<SemGenericTemplate> = table
            .generic_templates
            .into_iter()
            .filter(|template| used_templates.contains(&template.id))
            .collect();
        // Bodies are produced in demand order, which depends on the entry's
        // call graph. Publishing them in callable order instead keeps the
        // module — and every dump taken from it — a function of the program,
        // not of the traversal that discovered it.
        functions.sort_unstable_by_key(|function| function.callable);
        let type_facts = project_type_facts(
            checked_facts.rows(),
            &table.callables,
            &generic_templates,
            &functions,
            &aggregate_shapes,
            &variant_shapes,
            &vtables,
            &value_capabilities,
        );
        let mut resources: BTreeMap<ResolvedTy, crate::ResourceRelease> = type_facts
            .keys()
            .filter_map(|key| {
                crate::resource::resource_release_from_hir(module, &key.0)
                    .map(|release| (key.0.clone(), release))
            })
            .collect();
        // A record resource's release is a semantic callable, so it is
        // published here, where the resolved callable table is in hand. A
        // lifecycle whose close body never reached demand publishes no
        // release: the type then has no value contract at all rather than a
        // release nothing can execute.
        for key in type_facts.keys() {
            let Some(lifecycle) = crate::resource::record_resource_lifecycle(module, &key.0) else {
                continue;
            };
            let Some(close) = table
                .monomorphic_by_declaration
                .get(&lifecycle.close_declaration)
                .copied()
            else {
                continue;
            };
            resources.insert(
                key.0.clone(),
                crate::ResourceRelease::RecordClose {
                    lifecycle: Box::new(lifecycle.clone()),
                    close,
                },
            );
        }
        SemModule {
            actors,
            supervisors,
            resources,
            closures,
            vtables,
            callables: table.callables,
            generic_templates,
            root_unit_callables: table.root_unit_callables,
            entry_exit_plan: table.entry_exit_plan,
            entry_callable: table.entry_callable,
            functions,
            aggregate_shapes,
            variant_shapes,
            type_facts,
            string_literals,
            bytes_literals,
            regex_patterns,
            value_capabilities,
        }
    }

    fn callable_statuses(&self) -> Vec<(CallableId, SirLoweringStatus)> {
        self.table
            .callables
            .iter()
            .map(|callable| (callable.id, self.callable_status(callable.id)))
            .collect()
    }
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
                caller_visible_projection: false,
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
            caller_visible_projection: false,
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
            ResolvedTy::String
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
        || ty.is_builtin(hew_types::BuiltinType::Sender)
        || ty.is_builtin(hew_types::BuiltinType::Receiver)
        || ty.is_builtin(hew_types::BuiltinType::ActorCall)
        || collection_type_arguments(ty).is_some()
        || ty.is_builtin(hew_types::BuiltinType::LocalPid)
        || ty.is_builtin(hew_types::BuiltinType::LambdaPid)
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

/// Opaque owners enter ordinary value flow only through an audited lifecycle.
fn is_checked_opaque_resource(module: &HirModule, ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Named { builtin: None, is_opaque: true, args, .. } if args.is_empty())
        && module
            .type_classes
            .lifecycle_registry()
            .opaque_resource_for_ty(ty)
            .is_some_and(|lifecycle| !lifecycle.producer_declarations.is_empty())
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
    if !matches!(expr.intent, IntentKind::Read | IntentKind::Consume) {
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
    },
}

impl PreparedCallee {
    fn invoke(
        self,
        id: OpId,
        signature: SemSignature,
        args: Vec<crate::BoundaryOperand>,
        result: CallResult,
        normal: Option<Edge>,
        unwind: CallUnwind,
    ) -> SemTerminator {
        match self {
            Self::Direct(callee) => SemTerminator::Call {
                id,
                callee,
                args,
                result,
                normal,
                unwind,
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
            Self::Dyn { receiver, slot } => SemTerminator::DynCall {
                id,
                receiver,
                slot,
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
#[allow(
    deprecated,
    reason = "a trait method reached through a where-clause bound is not builtin-generic dispatch, so `ResolvedImplCall` does not carry it; these arms read the node, they do not construct one"
)]
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
}

impl PendingBlock {
    fn new(id: BlockId, args: Vec<BlockArg>) -> Self {
        Self {
            id,
            args,
            ops: Vec::new(),
            terminator: None,
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

struct BindingPlace {
    binding: BindingId,
    root_ty: ResolvedTy,
    leaf_ty: ResolvedTy,
    projections: Vec<AggregateSelection>,
}

/// Non-owning aggregate fields retained during a scalar field replacement.
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
    /// A stream producer body: the caller's sink it yields into and the
    /// element type each yield transfers.
    stream_sink: Option<(ValueId, ResolvedTy)>,
}

impl<'hir, 'service> Builder<'hir, 'service> {
    #[expect(
        clippy::too_many_lines,
        reason = "constructs the complete body state and binds its checked ABI once"
    )]
    fn new(
        function: Cow<'hir, HirFn>,
        callable: SemCallable,
        substitution: TypeSubstitution,
        source: &BodySource,
        service: &'service mut InstanceService<'hir>,
    ) -> Result<Self, String> {
        let source_params = source.parameters(&function)?.to_vec();
        let receiver_count = usize::from(matches!(
            source,
            BodySource::Closure(_) | BodySource::Actor { .. }
        ));
        // A stream producer owns the caller's sink as an implicit trailing
        // parameter with no source binding.
        let stream = match source {
            BodySource::Actor { actor, .. } => service.actors[actor.0 as usize]
                .handlers
                .iter()
                .find(|handler| handler.callable == callable.id)
                .and_then(|handler| handler.stream.clone()),
            BodySource::Function | BodySource::Closure(_) | BodySource::EntryAdapter(_) => None,
        };
        if source_params.len() + receiver_count + usize::from(stream.is_some())
            != callable.signature.params.len()
        {
            return Err(format!(
                "SIR callable `{}` has {} parameter ABI facts, but its HIR template has {} parameter(s)",
                callable.symbol,
                callable.signature.params.len(),
                source_params.len()
            ));
        }
        service.require_signature_shapes(&callable.signature)?;
        let entry = BlockId(0);
        let mut values = u32::try_from(receiver_count).expect("at most one receiver");
        let mut bindings = HashMap::new();
        let params = source_params.iter()
            .zip(callable.signature.params.iter().skip(receiver_count))
            .enumerate()
            .map(|(index, (param, abi))| {
                let ty = substitution.apply(&param.ty);
                if ty != abi.ty {
                    return Err(format!(
                        "SIR callable `{}` parameter {index} has `{}`, but its substituted HIR template has `{}`",
                        callable.symbol,
                        abi.ty.user_facing(),
                        ty.user_facing()
                    ));
                }
                let value = ValueId(values);
                values += 1;
                bindings.insert(param.id, BindingTarget::Value(value));
                // The header decides whether the caller retains the obligation
                // or transfers it to this body's normal and fault cleanup.
                let own = OwnKind::of_param(&ty, abi.passing, service.checked_facts.rows())?;
                Ok((
                    BlockArg { value, ty, own },
                    Binding {
                        id: crate::BindingId(u32::try_from(index).map_err(|_| {
                            "SIR source binding count exceeds u32".to_string()
                        })?),
                        name: param.name.clone(),
                        span: param.span.clone(),
                        mutable: param.mutable,
                        target: crate::BindingTarget::Value(value),
                    },
                ))
            })
            .collect::<Result<Vec<(BlockArg, Binding)>, String>>()?;
        let (mut params, source_bindings): (Vec<BlockArg>, Vec<Binding>) =
            params.into_iter().unzip();
        let stream_sink = stream.map(|element| {
            let sink = BlockArg {
                value: ValueId(values),
                ty: callable.signature.params[callable.signature.params.len() - 1]
                    .ty
                    .clone(),
                own: OwnKind::Owned,
            };
            values += 1;
            let seat = sink.value;
            params.push(sink);
            (seat, element)
        });
        let owned_live = params
            .iter()
            .filter(|param| param.own == OwnKind::Owned)
            .map(|param| (param.value, param.ty.clone()))
            .collect();
        let binding_declarations = source_params
            .iter()
            .enumerate()
            .map(|(index, param)| (param.id, index))
            .collect();
        let mut builder = Self {
            function,
            service,
            callable,
            substitution,
            blocks: vec![PendingBlock::new(entry, Vec::new())],
            current: entry,
            values,
            ops: 0,
            bindings,
            binding_declarations,
            owned_live,
            borrow_parents: HashMap::new(),
            scope_loans: Vec::new(),
            scope_loan_floors: vec![0],
            binding_loans: Vec::new(),
            ended_loans: std::collections::HashSet::new(),
            branch_depth: 0,
            scopes: vec![Vec::new()],
            source_bindings,
            params,
            loops: Vec::new(),
            places: Vec::new(),
            capture_places: HashMap::new(),
            argument_receiver_loans: Vec::new(),
            defers: Vec::new(),
            defer_bodies: Vec::new(),
            recovery_bodies: Vec::new(),
            task_scopes: Vec::new(),
            cleanup_may_fail: false,
            cleanup_draining: false,
            deferred_initialized: BTreeSet::new(),
            stream_sink,
        };
        builder.bind_captures(source)?;
        builder.bind_actor_state(source)?;
        builder.bind_private_value_parameters(&source_params)?;
        for parameter in &source_params {
            if let BindingTarget::Value(value) = builder.binding_target(parameter.id)? {
                if (parameter.mutable && builder.value_own_kind(value) == Some(OwnKind::None))
                    || builder.value_own_kind(value) == Some(OwnKind::Owned)
                {
                    let target = if parameter.mutable {
                        builder.acquire_local_target(value)?
                    } else {
                        builder.acquire_binding_target(value)?
                    };
                    builder.bindings.insert(parameter.id, target);
                    let declaration = builder.binding_declarations[&parameter.id];
                    builder.source_bindings[declaration].target = target;
                }
            }
            builder.declare_in_scope(parameter.id);
        }
        Ok(builder)
    }

    /// Mutable value parameters operate on private values. Use the canonical
    /// copy contract for both direct callables and their aggregate containers;
    /// the incoming borrowed ABI keeps the caller's value intact.
    fn bind_private_value_parameters(&mut self, parameters: &[HirBinding]) -> Result<(), String> {
        for parameter in parameters {
            if !parameter.mutable || parameter.is_consume {
                continue;
            }
            let ty = self.ty(&parameter.ty);
            let BindingTarget::Value(source) = self.binding_target(parameter.id)? else {
                continue;
            };
            if self.value_own_kind(source) != Some(OwnKind::Guaranteed) {
                continue;
            }
            self.service.require_type_facts(&ty)?;
            if self.service.checked_facts.rows()[&hew_types::TypeInstanceKey(ty.clone())].clone
                == hew_types::CloneKind::None
            {
                // Replacements own one parameter-scope slot. The incoming
                // borrow remains the readable value until this path assigns.
                let place = self.allocate_local(ty)?;
                let declaration = self.binding_declarations[&parameter.id];
                self.source_bindings[declaration].target = BindingTarget::Place(place);
                continue;
            }
            let copied = self.emit_typed(
                Provenance::Synthesized,
                &ty,
                SemOpKind::CopyValue {
                    source: Operand { value: source },
                },
            )?;
            let target = self.acquire_binding_target(copied)?;
            self.bindings.insert(parameter.id, target);
            let declaration = self.binding_declarations[&parameter.id];
            self.source_bindings[declaration].target = target;
        }
        Ok(())
    }

    fn bind_captures(&mut self, source: &BodySource) -> Result<(), String> {
        let BodySource::Closure(expression) = source else {
            return Ok(());
        };
        let HirExprKind::Closure { captures, .. } = &expression.kind else {
            unreachable!()
        };
        let abi = &self.callable.signature.params[0];
        let own = OwnKind::of_param(&abi.ty, abi.passing, self.service.checked_facts.rows())?;
        self.params.insert(
            0,
            BlockArg {
                value: ValueId(0),
                ty: abi.ty.clone(),
                own,
            },
        );
        if own == OwnKind::Owned {
            self.owned_live.insert(ValueId(0), abi.ty.clone());
        }
        for (index, capture) in captures.iter().enumerate() {
            let field =
                u32::try_from(index).map_err(|_| "capture count exceeds u32".to_string())?;
            let place =
                PlaceId(u32::try_from(self.places.len()).map_err(|_| "place count exceeds u32")?);
            self.places.push(crate::PlaceDecl {
                id: place,
                ty: self.ty(&capture.ty),
                origin: crate::PlaceOrigin::Capture {
                    environment: ValueId(0),
                    field,
                },
            });
            self.capture_places.insert(capture.binding, place);
            self.bindings
                .insert(capture.binding, BindingTarget::Place(place));
            self.source_bindings.push(Binding {
                id: crate::BindingId(
                    u32::try_from(self.source_bindings.len())
                        .map_err(|_| "binding count exceeds u32".to_string())?,
                ),
                name: capture.name.clone(),
                span: expression.span.clone(),
                mutable: capture.access == hew_types::ClosureCaptureAccess::Var,
                target: crate::BindingTarget::Place(place),
            });
            let declaration = self.source_bindings.len() - 1;
            self.binding_declarations
                .insert(capture.binding, declaration);
            self.declare_in_scope(capture.binding);
        }
        for capture in captures {
            if capture.consumption == hew_types::ClosureCaptureConsumption::Consumed {
                let value = self.load_capture(capture.binding, Provenance::Synthesized, true)?;
                self.capture_places.remove(&capture.binding);
                let target = self.acquire_binding_target(value)?;
                self.bindings.insert(capture.binding, target);
                let declaration = self.binding_declarations[&capture.binding];
                self.source_bindings[declaration].target = target;
            }
        }
        Ok(())
    }

    fn lower(mut self, source: BodySource) -> Result<SemFunction, String> {
        if self.callable.function != self.function.id
            || self.callable.declaration != self.function.declaration
        {
            return Err(
                "SIR callable provenance does not match the HIR function's checked identity"
                    .to_string(),
            );
        }
        if self.function.intrinsic_id.is_some() {
            return Err("floor intrinsic has no checked SIR operation contract".to_string());
        }
        match (
            &self.callable.instance,
            self.function.type_params.is_empty(),
        ) {
            (CallableInstance::Monomorphic | CallableInstance::SupervisorChild { .. }, true) => {}
            (CallableInstance::ActorMember, true) if matches!(source, BodySource::Actor { .. }) => {
            }
            (CallableInstance::Closure(_), _) if matches!(source, BodySource::Closure(_)) => {}
            (CallableInstance::EntryAdapter, _)
                if matches!(source, BodySource::EntryAdapter(_)) => {}
            (CallableInstance::Generic(key), false)
                if key.template.declaration == self.function.declaration
                    && key.type_args == self.substitution.args => {}
            _ => return Err(
                "SIR callable instance does not match its HIR template and semantic substitution"
                    .to_string(),
            ),
        }
        if matches!(source, BodySource::Function)
            && self.callable.signature.return_ty != self.ty(&self.function.return_ty)
        {
            return Err(format!(
                "SIR callable `{}` return type `{}` differs from substituted HIR template return `{}`",
                self.callable.symbol,
                self.callable.signature.return_ty.user_facing(),
                self.ty(&self.function.return_ty).user_facing()
            ));
        }
        self.enter_task_scope()?;
        let result = self.lower_source_body(source)?;
        let result = result
            .map(|operand| {
                self.coerce_value(
                    operand.value,
                    &self.callable.signature.return_ty.clone(),
                    Provenance::Synthesized,
                )
                .map(|value| Operand { value })
            })
            .transpose()?;
        if self.is_open() {
            self.finish_return_value(result.map(|operand| crate::BoundaryOperand {
                operand,
                decision: crate::BoundaryDecision::Move,
            }))?;
        }
        let blocks = std::mem::take(&mut self.blocks)
            .into_iter()
            .map(PendingBlock::into_sem_block)
            .collect::<Result<Vec<_>, _>>()?;
        let mut function = SemFunction {
            id: self.function.id,
            callable: self.callable.id,
            declaration: self.function.declaration.clone(),
            name: self.callable.symbol.clone(),
            span: self.function.span.clone(),
            source_origin: self.callable.source_origin.clone(),
            params: self.params,
            return_ty: self.callable.signature.return_ty.clone(),
            entry: BlockId(0),
            blocks,
            places: self.places,
            bindings: self.source_bindings,
        };
        tasks::remove_empty_scopes(&mut function);
        projection::complete_edge_partitions(
            &mut function,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        Ok(function)
    }

    /// A stream producer reads a snapshot of every state field it captures
    /// (§4.12): the copy is a mutable local of this turn, and the actor's own
    /// seat is untouched.
    fn snapshot_stream_captures(
        &mut self,
        captures: &[hew_hir::HirGenCapture],
        state_bindings: &[HirBinding],
    ) -> Result<(), String> {
        for capture in captures {
            if capture.source != hew_hir::HirGenCaptureSource::ActorStateField {
                continue;
            }
            let binding = state_bindings
                .iter()
                .find(|binding| binding.id == capture.binding)
                .ok_or("stream producer captures an unknown state field")?;
            let BindingTarget::Place(place) = self.binding_target(binding.id)? else {
                return Err("stream producer state capture has no state seat".into());
            };
            let ty = self.ty(&capture.ty);
            self.service.require_type_facts(&ty)?;
            if self.service.checked_facts.rows()[&TypeInstanceKey(ty.clone())].clone
                == hew_types::CloneKind::None
            {
                return Err(format!(
                    "stream producer cannot snapshot state field `{}`: its type has no copy",
                    binding.name
                ));
            }
            let value =
                self.emit_typed(Provenance::Synthesized, &ty, SemOpKind::LoadCopy { place })?;
            self.bind_source_value(binding, value)?;
        }
        Ok(())
    }

    fn lower_source_body(&mut self, source: BodySource) -> Result<Option<Operand>, String> {
        match source {
            BodySource::Actor { state_bindings, .. } if self.stream_sink.is_some() => {
                // HIR shapes a stream producer as a generator block; the body
                // runs as this actor turn and yields straight into the sink.
                let body = self.function.body.clone();
                let Some(HirExpr {
                    kind:
                        HirExprKind::GenBlock {
                            body: producer,
                            captures,
                            ..
                        },
                    ..
                }) = body.tail.as_deref()
                else {
                    return Err("stream producer body is not a generator block".into());
                };
                if !body.statements.is_empty() {
                    return Err("stream producer body carries statements outside its block".into());
                }
                self.snapshot_stream_captures(captures, &state_bindings)?;
                self.lower_block(producer, OwnedBindingUse::Return)
            }
            BodySource::Function | BodySource::Actor { .. } => {
                let body = self.function.body.clone();
                self.lower_block(&body, OwnedBindingUse::Return)
            }
            BodySource::EntryAdapter(adapter) => {
                self.lower_entry_adapter(&adapter)?;
                Ok(None)
            }
            BodySource::Closure(expression) => {
                let HirExprKind::Closure { body, ret_ty, .. } = &expression.kind else {
                    unreachable!()
                };
                if self.ty(ret_ty) != self.callable.signature.return_ty {
                    return Err("closure body return differs from its exact signature".to_string());
                }
                if matches!(self.ty(&body.ty), ResolvedTy::Unit | ResolvedTy::Never) {
                    self.lower_discarded_expr(body)?;
                    Ok(None)
                } else {
                    Ok(Some(Operand {
                        value: lower_initial_value_transfer(
                            self,
                            body,
                            "closure body result",
                            OwnedBindingUse::Return,
                        )?,
                    }))
                }
            }
        }
    }

    /// Lower one HIR expression in a semantic operand position.
    ///
    /// The initial scalar SIR domain admits only read uses, but it still
    /// translates every HIR intent before rejecting a non-read mode. This
    /// prevents a source move/borrow/discharge from being silently weakened
    /// into a reusable SIR value during the migration.
    fn lower_read_operand(&mut self, expr: &HirExpr, context: &str) -> Result<Operand, String> {
        require_initial_scalar_read(expr.intent)
            .map_err(|reason| format!("{context}: {reason}"))?;
        Ok(Operand {
            value: self.lower_expr(expr)?,
        })
    }

    /// A binding whose value has no copy operation. `Some` is the loan the
    /// binding names, read through that loan rather than transferred out of
    /// it; `None` means the binding owns its value and transfers normally.
    fn read_bound_loan(
        &mut self,
        binding: BindingId,
        source: ValueId,
        binding_use: OwnedBindingUse,
    ) -> Result<Option<ValueId>, String> {
        self.require_selected_binding(binding, source)?;
        let name = self.source_bindings[self.binding_declarations[&binding]]
            .name
            .clone();
        if binding_use == OwnedBindingUse::Probe
            && self.value_own_kind(source) == Some(OwnKind::Guaranteed)
        {
            if self.ended_loans.contains(&source) {
                return Err(format!(
                    "E_OWN_CONSUME_BORROWED: `{name}` borrows a collection that was since mutated or drained; the loan ended there and cannot be read again"
                ));
            }
            return Ok(Some(source));
        }
        if self.value_own_kind(source) != Some(OwnKind::Owned) {
            return Err(format!(
                "E_OWN_CONSUME_BORROWED: `{name}` is borrowed here; a value with no copy operation transfers only from an owning binding"
            ));
        }
        Ok(None)
    }

    fn lower_owned_transfer(
        &mut self,
        expr: &HirExpr,
        binding_use: OwnedBindingUse,
    ) -> Result<ValueId, String> {
        let mut expr = expr;
        while let HirExprKind::SubsumedValue { source } = &expr.kind {
            if self.ty(&source.ty) != self.ty(&expr.ty) {
                return Err("transparent value transfer must preserve its exact type".into());
            }
            expr = source;
        }
        let ty = self.ty(&expr.ty);
        let own = OwnKind::of_ty(&ty, self.service.checked_facts.rows())?;
        let movable_owner = self
            .service
            .checked_facts
            .rows()
            .get(&TypeInstanceKey(ty.clone()))
            .is_some_and(|row| row.clone == hew_types::CloneKind::None);
        if own == OwnKind::Owned {
            let movable = self
                .service
                .checked_facts
                .rows()
                .get(&TypeInstanceKey(ty.clone()))
                .is_some_and(|row| row.clone == hew_types::CloneKind::None);
            if movable {
                if let Some(value) = self.lower_consuming_projection(expr)? {
                    return Ok(value);
                }
            }
            if let HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } = &expr.kind
            {
                match self.binding_target(*binding)? {
                    BindingTarget::Place(place) => {
                        let mut take = movable
                            || binding_use == OwnedBindingUse::Move
                            || (binding_use == OwnedBindingUse::Return && self.defers.is_empty());
                        if take {
                            if let Some((_, field)) = self.capture_field(*binding) {
                                if field.consumption
                                    != hew_types::ClosureCaptureConsumption::Consumed
                                {
                                    if movable {
                                        return Err("E_OWN_CONSUME_BORROWED: capture transfer requires consuming access".into());
                                    }
                                    take = false;
                                }
                            }
                        }
                        if take && self.state_field_leaves_as_copy(place, expr)? {
                            take = false;
                        }
                        return self.emit(
                            expr,
                            if take {
                                SemOpKind::LoadTake { place }
                            } else {
                                SemOpKind::LoadCopy { place }
                            },
                        );
                    }
                    BindingTarget::Value(source) => {
                        if movable {
                            if let Some(loan) =
                                self.read_bound_loan(*binding, source, binding_use)?
                            {
                                return Ok(loan);
                            }
                        }
                        return self.emit(
                            expr,
                            SemOpKind::CopyValue {
                                source: Operand { value: source },
                            },
                        );
                    }
                }
            }
        }
        let source = self.lower_expr_with_binding_use(expr, binding_use)?;
        // A loan of a clone-free value has no owned copy to make: the binding
        // holds the loan and the wall against consuming it is the loan itself.
        if own == OwnKind::Owned
            && !movable_owner
            && self.value_own_kind(source) == Some(OwnKind::Guaranteed)
        {
            self.emit(
                expr,
                SemOpKind::CopyValue {
                    source: Operand { value: source },
                },
            )
        } else {
            Ok(source)
        }
    }

    fn capture_field(
        &self,
        binding: BindingId,
    ) -> Option<(crate::PlaceId, crate::SemCaptureField)> {
        let place = *self.capture_places.get(&binding)?;
        let PlaceOrigin::Capture { field, .. } = self.places.get(place.0 as usize)?.origin else {
            return None;
        };
        let CallableInstance::Closure(id) = self.callable.instance else {
            return None;
        };
        self.service
            .closures
            .get(id.0 as usize)?
            .fields
            .get(field as usize)
            .cloned()
            .map(|field| (place, field))
    }

    fn load_capture(
        &mut self,
        binding: BindingId,
        provenance: Provenance,
        take: bool,
    ) -> Result<ValueId, String> {
        let (place, field) = self
            .capture_field(binding)
            .ok_or_else(|| "capture binding has no exact environment field".to_string())?;
        if take && field.consumption != hew_types::ClosureCaptureConsumption::Consumed {
            return Err("capture extraction lacks checker-selected consuming access".to_string());
        }
        self.emit_typed(
            provenance,
            &field.ty,
            if take {
                SemOpKind::LoadTake { place }
            } else {
                SemOpKind::LoadCopy { place }
            },
        )
    }

    fn lower_closure(&mut self, expression: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::Closure { captures, .. } = &expression.kind else {
            unreachable!()
        };
        let closure =
            self.service
                .request_closure(self.callable.id, expression, &self.substitution)?;
        let mut fields = Vec::with_capacity(captures.len());
        for capture in captures {
            let ty = self.ty(&capture.ty);
            let take = capture.acquisition == hew_types::ClosureCaptureAcquisition::Move;
            let provenance = Provenance::Site(expression.site);
            let target = self.binding_target(capture.binding)?;
            if self.target_ty(target)? != ty {
                return Err("closure acquisition changes its captured binding type".into());
            }
            let value = match target {
                BindingTarget::Place(place) => {
                    if take {
                        if let Some((_, field)) = self.capture_field(capture.binding) {
                            if field.consumption != hew_types::ClosureCaptureConsumption::Consumed {
                                return Err("capture extraction lacks consuming access".into());
                            }
                        }
                    }
                    self.emit_typed(
                        provenance,
                        &ty,
                        if take {
                            SemOpKind::LoadTake { place }
                        } else {
                            SemOpKind::LoadCopy { place }
                        },
                    )?
                }
                BindingTarget::Value(source) => {
                    if take && self.value_own_kind(source) == Some(OwnKind::Owned) {
                        self.owned_live.remove(&source);
                        source
                    } else if OwnKind::of_ty(&ty, self.service.checked_facts.rows())?
                        == OwnKind::Owned
                    {
                        self.emit_typed(
                            provenance,
                            &ty,
                            SemOpKind::CopyValue {
                                source: Operand { value: source },
                            },
                        )?
                    } else {
                        source
                    }
                }
            };
            self.owned_live.remove(&value);
            fields.push(Operand { value });
        }
        self.emit(expression, SemOpKind::ClosureMake { closure, fields })
    }

    fn coerce_value(
        &mut self,
        value: ValueId,
        target: &ResolvedTy,
        provenance: Provenance,
    ) -> Result<ValueId, String> {
        let source = self
            .value_ty(value)
            .ok_or_else(|| "coercion has no typed source value".to_string())?;
        if crate::call_boundary_types_match(&source, target) {
            return Ok(value);
        }
        self.service.require_type_facts(target)?;
        crate::verify_callable_coercion(&source, target, self.service.checked_facts.rows())
            .map_err(|reason| {
                format!("value coercion from {source:?} to {target:?} refused: {reason}")
            })?;
        self.owned_live.remove(&value);
        self.emit_typed(
            provenance,
            target,
            SemOpKind::CallableCoerce {
                source: Operand { value },
            },
        )
    }

    /// A pattern binding names its probed payload until the arm is selected;
    /// an owning payload transfers exactly once, after every guard has passed.
    fn require_selected_binding(&self, binding: BindingId, value: ValueId) -> Result<(), String> {
        let borrowed = self.value_own_kind(value) == Some(OwnKind::Guaranteed);
        if borrowed
            || self.value_own_kind(value) == Some(OwnKind::Owned)
            || self.argument_receiver_loans.contains(&value)
        {
            let name = &self.source_bindings[self.binding_declarations[&binding]].name;
            if borrowed {
                // A loaned payload never becomes an owner, in a guard or in the
                // arm body: the consume wall, not the guard rule, is what it
                // meets.
                if !self.argument_receiver_loans.contains(&value) {
                    return Ok(());
                }
                return Err(format!(
                    "E_OWN_CONSUME_BORROWED: `{name}` is borrowed here; a value with no copy operation transfers only from an owning binding"
                ));
            }
            return Err(format!(
                "E_OWN_GUARD_CONSUME: match guard consumes pattern binding `{name}`; a guard can only read its bindings"
            ));
        }
        Ok(())
    }

    fn value_own_kind(&self, value: ValueId) -> Option<OwnKind> {
        self.params
            .iter()
            .find(|param| param.value == value)
            .map(|param| param.own)
            .or_else(|| {
                self.blocks
                    .iter()
                    .flat_map(|block| block.args.iter())
                    .find(|arg| arg.value == value)
                    .map(|arg| arg.own)
            })
            .or_else(|| {
                self.blocks
                    .iter()
                    .flat_map(|block| block.ops.iter())
                    .flat_map(|op| op.results.iter())
                    .find(|result| result.id == value)
                    .map(|result| result.own)
            })
    }

    fn value_ty(&self, value: ValueId) -> Option<ResolvedTy> {
        self.params
            .iter()
            .find(|param| param.value == value)
            .map(|param| param.ty.clone())
            .or_else(|| {
                self.blocks
                    .iter()
                    .flat_map(|block| block.args.iter())
                    .find(|arg| arg.value == value)
                    .map(|arg| arg.ty.clone())
            })
            .or_else(|| {
                self.blocks
                    .iter()
                    .flat_map(|block| block.ops.iter())
                    .flat_map(|op| op.results.iter())
                    .find(|result| result.id == value)
                    .map(|result| result.ty.clone())
            })
            .or_else(|| {
                self.blocks.iter().find_map(|block| {
                    let mut found = None;
                    block.terminator.as_ref()?.visit_results(|result| {
                        if result.id == value {
                            found = Some(result.ty.clone());
                        }
                    });
                    found
                })
            })
    }

    fn record_binding_version(&mut self, binding: BindingId, value: ValueId) -> Result<(), String> {
        let declaration = *self.binding_declarations.get(&binding).ok_or_else(|| {
            format!("binding `{binding}` has no source declaration in SIR lowering")
        })?;
        let source = self.source_bindings[declaration].clone();
        self.source_bindings.push(Binding {
            id: crate::BindingId(
                u32::try_from(self.source_bindings.len())
                    .map_err(|_| "SIR source binding count exceeds u32".to_string())?,
            ),
            name: source.name,
            span: source.span,
            mutable: source.mutable,
            target: crate::BindingTarget::Value(value),
        });
        Ok(())
    }

    fn bind_source_value(&mut self, binding: &HirBinding, value: ValueId) -> Result<(), String> {
        let target = if binding.mutable {
            self.acquire_local_target(value)?
        } else {
            self.acquire_binding_target(value)?
        };
        let declaration = self.source_bindings.len();
        self.source_bindings.push(Binding {
            id: crate::BindingId(
                u32::try_from(declaration).map_err(|_| "binding count exceeds u32")?,
            ),
            name: binding.name.clone(),
            span: binding.span.clone(),
            mutable: binding.mutable,
            target,
        });
        self.binding_declarations.insert(binding.id, declaration);
        self.bindings.insert(binding.id, target);
        self.declare_in_scope(binding.id);
        Ok(())
    }

    fn mutable_bindings(&self) -> Vec<BindingId> {
        let mut bindings = self
            .binding_declarations
            .iter()
            .filter_map(|(binding, &index)| {
                (self.source_bindings[index].mutable
                    && matches!(self.bindings.get(binding), Some(BindingTarget::Value(_))))
                .then_some(*binding)
            })
            .collect::<Vec<_>>();
        bindings.sort_unstable();
        bindings
    }

    fn emit_destroy(&mut self, value: ValueId) -> Result<(), String> {
        if self
            .value_ty(value)
            .as_ref()
            .is_some_and(|ty| self.value_needs_close(ty))
        {
            self.close_value(None, Some(value))?;
        }
        let id = OpId(self.ops);
        self.current_block_mut().append_op(SemOp {
            id,
            results: Vec::new(),
            kind: SemOpKind::DestroyValue {
                value: Operand { value },
            },
            provenance: Provenance::Synthesized,
        })?;
        self.ops += 1;
        self.owned_live.remove(&value);
        if self.cleanup_may_fail && !self.cleanup_draining {
            self.dispatch_value_cleanup()?;
        }
        Ok(())
    }

    fn destroy_all_live(&mut self) -> Result<(), String> {
        // Keep the lexical stack: another generated continuation can still
        // finish the enclosing call's argument evaluation normally.
        self.end_call_loans(&self.argument_receiver_loans.clone())?;
        let values: Vec<_> = self.owned_live.keys().copied().collect();
        for value in values.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        self.end_scopes(0)
    }

    fn control_state(&self) -> ControlState {
        ControlState {
            block: self.current,
            bindings: self.bindings.clone(),
            binding_declarations: self.binding_declarations.clone(),
            owned_live: self.owned_live.clone(),
            loans: self.argument_receiver_loans.clone(),
            scopes: self.scopes.clone(),
            scope_loans: self.scope_loans.clone(),
            scope_loan_floors: self.scope_loan_floors.clone(),
            ended_loans: self.ended_loans.clone(),
            defers: self.defers.clone(),
            task_scopes: self.task_scopes.clone(),
            cleanup_may_fail: self.cleanup_may_fail,
            cleanup_draining: self.cleanup_draining,
            deferred_initialized: self.deferred_initialized.clone(),
        }
    }

    fn restore_control_state(&mut self, state: &ControlState) {
        self.current = state.block;
        self.bindings.clone_from(&state.bindings);
        self.binding_declarations
            .clone_from(&state.binding_declarations);
        self.owned_live = state.owned_live.clone();
        self.argument_receiver_loans.clone_from(&state.loans);
        self.scopes.clone_from(&state.scopes);
        self.scope_loans.clone_from(&state.scope_loans);
        self.scope_loan_floors.clone_from(&state.scope_loan_floors);
        self.ended_loans.clone_from(&state.ended_loans);
        self.defers.clone_from(&state.defers);
        self.task_scopes.clone_from(&state.task_scopes);
        self.cleanup_may_fail = state.cleanup_may_fail;
        self.cleanup_draining = state.cleanup_draining;
        self.deferred_initialized
            .clone_from(&state.deferred_initialized);
    }

    fn retain_bindings(
        bindings: &HashMap<BindingId, BindingTarget>,
        retained: &std::collections::HashSet<BindingId>,
    ) -> HashMap<BindingId, BindingTarget> {
        bindings
            .iter()
            .filter(|(binding, _)| retained.contains(binding))
            .map(|(binding, value)| (*binding, *value))
            .collect()
    }

    /// End the loans opened since `depth`, innermost first.
    fn end_loans_since(&mut self, depth: usize) -> Result<(), String> {
        let loans = self.argument_receiver_loans.split_off(depth);
        self.end_call_loans(&loans)
    }

    fn cleanup_match_candidate(
        &mut self,
        root_live: &BTreeMap<ValueId, ResolvedTy>,
        root_loans: usize,
        outer_bindings: &std::collections::HashSet<BindingId>,
    ) -> Result<(), String> {
        self.end_loans_since(root_loans)?;
        let keep = root_live
            .iter()
            .filter(|(value, _)| self.owned_live.contains_key(value))
            .map(|(value, ty)| (*value, ty.clone()))
            .collect();
        self.destroy_live_since(&keep)?;
        let leaving = self
            .scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .filter(|binding| !outer_bindings.contains(binding))
            .copied()
            .collect::<Vec<_>>();
        for binding in leaving {
            self.end_binding_scope(binding)?;
        }
        for scope in &mut self.scopes {
            scope.retain(|binding| outer_bindings.contains(binding));
        }
        self.bindings = Self::retain_bindings(&self.bindings, outer_bindings);
        self.binding_declarations
            .retain(|binding, _| outer_bindings.contains(binding));
        if self.cleanup_may_fail && !self.cleanup_draining {
            self.dispatch_value_cleanup()?;
        }
        Ok(())
    }

    fn merge_control_states(&mut self, states: Vec<ControlState>) -> Result<(), String> {
        let Some(first) = states.first() else {
            return Err("control-flow join has no live predecessor".to_string());
        };
        if states.len() == 1 {
            self.restore_control_state(first);
            return Ok(());
        }
        let edge_args = vec![Vec::new(); states.len()];
        self.join_control_states(states, Vec::new(), edge_args)
    }

    fn join_control_states(
        &mut self,
        states: Vec<ControlState>,
        mut block_args: Vec<BlockArg>,
        mut edge_args: Vec<Vec<Operand>>,
    ) -> Result<(), String> {
        let first = states
            .first()
            .ok_or_else(|| "control-flow join has no predecessor".to_string())?;
        if edge_args.len() != states.len() {
            return Err("control-flow join has inconsistent edge metadata".into());
        }
        let keys = first.bindings.keys().copied().collect::<BTreeSet<_>>();
        if states.iter().any(|state| {
            state.bindings.keys().copied().collect::<BTreeSet<_>>() != keys
                || state.binding_declarations != first.binding_declarations
                || state.scopes != first.scopes
        }) {
            return Err("control-flow predecessors expose different lexical declarations".into());
        }
        if states
            .iter()
            .any(|state| state.owned_live != first.owned_live)
        {
            return Err("control-flow predecessors leave different temporary owners live".into());
        }
        if states
            .iter()
            .any(|state| state.deferred_initialized != first.deferred_initialized)
        {
            return Err(
                "control-flow predecessors disagree on which deferred actor fields are initialized"
                    .into(),
            );
        }
        for binding in &keys {
            if states
                .iter()
                .any(|state| matches!(state.bindings[binding], BindingTarget::Place(_)))
                && states
                    .iter()
                    .any(|state| state.bindings[binding] != first.bindings[binding])
            {
                return Err("lexical place identity changed across a control-flow edge".into());
            }
        }
        let mut joined = first.clone();
        joined.cleanup_may_fail = states.iter().any(|state| state.cleanup_may_fail);
        self.binding_declarations
            .clone_from(&first.binding_declarations);
        self.bindings.clone_from(&first.bindings);
        for binding in self.mutable_bindings() {
            let values = states
                .iter()
                .map(|state| match state.bindings[&binding] {
                    BindingTarget::Value(value) => Ok(value),
                    BindingTarget::Place(_) => {
                        Err("scalar join received a place binding".to_string())
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            let ty = self
                .value_ty(values[0])
                .ok_or_else(|| "scalar join value has no type".to_string())?;
            if values.iter().any(|value| {
                self.value_ty(*value).as_ref() != Some(&ty)
                    || self.value_own_kind(*value) == Some(OwnKind::Owned)
            }) {
                return Err("scalar binding join has inconsistent type or ownership".into());
            }
            let own = self
                .value_own_kind(values[0])
                .ok_or_else(|| "scalar join has no ownership facts".to_string())?;
            let value = self.fresh_value();
            block_args.push(BlockArg { value, ty, own });
            for (args, value) in edge_args.iter_mut().zip(values) {
                args.push(Operand { value });
            }
            joined.bindings.insert(binding, BindingTarget::Value(value));
            self.record_binding_version(binding, value)?;
        }
        let join = self.new_block(block_args);
        for (state, args) in states.into_iter().zip(edge_args) {
            self.current = state.block;
            self.set_terminator(SemTerminator::Goto(Edge { target: join, args }))?;
        }
        joined.block = join;
        self.restore_control_state(&joined);
        Ok(())
    }

    /// Lower one `let` statement: evaluate its initializer, name the value and
    /// record any loan the initializer took on a collection.
    fn lower_let_statement(
        &mut self,
        binding: &hew_hir::HirBinding,
        value: Option<&HirExpr>,
    ) -> Result<(), String> {
        if let Some(expr) = value.filter(|expr| self.ty(&expr.ty) == ResolvedTy::Never) {
            self.lower_discarded_expr(expr)?;
            if self.is_open() {
                return Err(
                    "Never-typed binding initializer did not terminate its SIR block".to_string(),
                );
            }
            return Ok(());
        }
        let loan_floor = self.scope_loans.len();
        let value = value
            .map(|expr| {
                lower_initial_value_transfer(
                    self,
                    expr,
                    "binding initializer",
                    if binding.is_consume {
                        OwnedBindingUse::Move
                    } else {
                        OwnedBindingUse::Copy
                    },
                )
            })
            .transpose()?
            .ok_or_else(|| {
                "uninitialised bindings are not in the initial SIR subset".to_string()
            })?;
        // §1.6: the value a binding names carries the binding's name, span and
        // mutability, so a rule 2, 3, 4 or 6 violation rooted in it renders its
        // `E_OWN_*` code rather than `E_SIR_ICE`, and rule 6a has a mutability
        // bit to read. A `let` aliases the SSA value its initializer produced
        // rather than defining one of its own, so the provenance lands on that
        // definition — and only when it has none, because `let y = x` must not
        // rename the parameter `x` already named.
        let value = self.coerce_value(value, &self.ty(&binding.ty), Provenance::Synthesized)?;
        // The binding names the initializer's loans: they end at its last use
        // rather than at this scope's exit. Their root is the collection the
        // read borrowed.
        if self.scope_loans.len() > loan_floor {
            let loans = self.scope_loans[loan_floor..].to_vec();
            let root = self.value_borrow_root(loans[0])?;
            self.binding_loans.push(BindingLoans {
                root,
                loans,
                loop_depth: self.loops.len(),
                branch_depth: self.branch_depth,
            });
        }
        self.bind_source_value(binding, value)
    }

    fn lower_block(
        &mut self,
        block: &HirBlock,
        tail_binding_use: OwnedBindingUse,
    ) -> Result<Option<Operand>, String> {
        for statement in &block.statements {
            if !self.is_open() {
                break;
            }
            match &statement.kind {
                HirStmtKind::Let(binding, value) => {
                    self.lower_let_statement(binding, value.as_ref())?;
                }
                HirStmtKind::Expr(expr) => {
                    let loan_floor = self.scope_loans.len();
                    self.lower_discarded_expr(expr)?;
                    // A discarded expression cannot retain a borrowed result.
                    // End its interior element loans before the next statement,
                    // so an indexed field read does not freeze its collection.
                    if self.is_open() && self.scope_loans.len() > loan_floor {
                        let loans = self.scope_loans.split_off(loan_floor);
                        self.end_call_loans(&loans)?;
                    }
                }
                HirStmtKind::Return(value) => {
                    self.lower_function_return(value.as_ref())?;
                }
                HirStmtKind::Assign {
                    target,
                    value,
                    first_store,
                } => {
                    self.lower_assignment(target, value, *first_store)?;
                }
                HirStmtKind::Destructure { value, fields } => {
                    self.lower_destructure(value, fields)?;
                }
                HirStmtKind::Defer { body, scope_id } => {
                    self.register_defer(body, scope_id.0)?;
                }
            }
        }
        if self.is_open() {
            match block.tail.as_deref() {
                Some(expr) if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) => {
                    let divergent = self.ty(&expr.ty) == ResolvedTy::Never;
                    self.lower_discarded_expr(expr)?;
                    if divergent && self.is_open() {
                        return Err(
                            "Never-typed block tail did not terminate its SIR block".to_string()
                        );
                    }
                    Ok(None)
                }
                Some(expr) => Ok(Some(Operand {
                    value: lower_initial_value_transfer(
                        self,
                        expr,
                        "block tail value",
                        tail_binding_use,
                    )?,
                })),
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn lower_scoped_block(
        &mut self,
        block: &HirBlock,
        tail_binding_use: OwnedBindingUse,
    ) -> Result<Option<Operand>, String> {
        let floor = self.scopes.len();
        let live_before = self.owned_live.clone();
        self.open_scope();
        let result = self.lower_block(block, tail_binding_use)?;
        if self.is_open() {
            self.end_scopes(floor)?;
            // Temporaries created inside the block die with it, exactly as its
            // bindings do. Only the block's own result leaves; without this a
            // conditional block hands its leftover owners to the join, which
            // then sees predecessors with different live temporaries.
            let mut keep = live_before;
            if let Some(result) = &result {
                if let Some(ty) = self.owned_live.get(&result.value) {
                    keep.insert(result.value, ty.clone());
                }
            }
            self.destroy_live_since(&keep)?;
        }
        self.leave_scope();
        Ok(result)
    }

    fn lower_assignment(
        &mut self,
        target: &HirExpr,
        value: &HirExpr,
        first_store: bool,
    ) -> Result<(), String> {
        if let HirExprKind::Index { container, index } = &target.kind {
            let family = match self.ty(&container.ty) {
                ResolvedTy::Array(_, _) => Some(hew_types::RuntimeCallFamily::Array(
                    hew_types::runtime_call::ArrayValueOp::Set,
                )),
                ResolvedTy::Bytes => Some(hew_types::RuntimeCallFamily::BytesSet),
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::Vec),
                    ..
                } => Some(hew_types::RuntimeCallFamily::Vector(
                    hew_types::runtime_call::VecValueOp::Set,
                )),
                _ => None,
            };
            if let Some(family) = family {
                let mut operation = target.clone();
                operation.ty = ResolvedTy::Unit;
                let mut replacement = value.clone();
                replacement.intent = IntentKind::Read;
                self.lower_runtime_operation(
                    &operation,
                    family,
                    &[container.as_ref(), index.as_ref(), &replacement],
                    false,
                )?;
                return Ok(());
            }
        }
        if matches!(
            target.kind,
            HirExprKind::FieldAccess { .. } | HirExprKind::TupleIndex { .. }
        ) {
            if self.lower_element_field_assignment(target, value)? {
                return Ok(());
            }
            return self.lower_field_assignment(target, value);
        }
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &target.kind
        else {
            return Err("assignment requires a resolved local binding target".into());
        };
        let declaration = *self
            .binding_declarations
            .get(binding)
            .ok_or_else(|| "assignment target has no declaration".to_string())?;
        if !self.source_bindings[declaration].mutable {
            return Err("assignment target is not mutable".into());
        }
        let target = self.binding_target(*binding)?;
        let ty = self.target_ty(target)?;
        let new =
            lower_initial_value_transfer(self, value, "assignment value", OwnedBindingUse::Copy)?;
        let new = self.coerce_value(new, &ty, Provenance::Site(value.site))?;
        match target {
            BindingTarget::Place(place) if first_store => {
                // A deferred actor field's first store (D447): the seat holds
                // nothing to release, and from here the fault path owns it.
                if !matches!(
                    self.places[place.0 as usize].origin,
                    PlaceOrigin::ActorState {
                        initialized: false,
                        ..
                    }
                ) {
                    return Err("a first store requires an uninitialized actor state seat".into());
                }
                self.emit_place_operation(
                    SemOpKind::StoreInit {
                        place,
                        value: Operand { value: new },
                    },
                    Provenance::Site(value.site),
                )?;
                self.owned_live.remove(&new);
                self.deferred_initialized.insert(place);
                Ok(())
            }
            BindingTarget::Place(place) => {
                self.store_projected(place, new, Provenance::Site(value.site))
            }
            BindingTarget::Value(_) => {
                if self.value_own_kind(new) == Some(OwnKind::Owned) {
                    let BindingTarget::Place(place) = self.source_bindings[declaration].target
                    else {
                        return Err("owned assignment has no lexical storage declaration".into());
                    };
                    self.store_projected(place, new, Provenance::Site(value.site))?;
                    self.bindings.insert(*binding, BindingTarget::Place(place));
                    return Ok(());
                }
                self.bindings.insert(*binding, BindingTarget::Value(new));
                self.record_binding_version(*binding, new)
            }
        }
    }

    /// `rows[i].field = v`: a projection chain rooted at a vector element.
    ///
    /// The element is not a place - only the vector's slot is - so the write
    /// goes through the same element set entry `rows[i] = v` uses (D460): read
    /// the element, replace the selected field in that owner, and move the
    /// updated element back into its slot. The set entry releases the element
    /// it replaces, so the replaced field value is released exactly once, and
    /// the index keeps its bounds trap.
    ///
    /// Returns `false` for every other projection root, which the ordinary
    /// binding-rooted path handles.
    fn lower_element_field_assignment(
        &mut self,
        target: &HirExpr,
        value: &HirExpr,
    ) -> Result<bool, String> {
        let (root, projections) = self.projection_chain(target)?;
        if !projections.is_empty() {
            if let HirExprKind::BorrowedIndex { .. } = &root.kind {
                // The checker read this element as a loan because it has no
                // semantic copy, and the element set entry needs an owner to
                // move in. Name the construct rather than the place model.
                return Err(
                    "assignment through a field of a collection element requires an element \
                     with a semantic copy; move it out with `remove`, update it and put it back"
                        .into(),
                );
            }
        }
        let HirExprKind::Index { container, index } = &root.kind else {
            return Ok(false);
        };
        if projections.is_empty() {
            return Ok(false);
        }
        let container_ty = self.ty(&container.ty);
        if !matches!(
            collection_type_arguments(&container_ty),
            Some((hew_types::BuiltinType::Vec, _))
        ) {
            return Ok(false);
        }
        let element_ty = self.ty(&root.ty);
        let leaf_ty = self.ty(&target.ty);
        let provenance = Provenance::Site(target.site);

        // Evaluate the replacement before the element it lands in, exactly as
        // the binding-rooted field assignment does.
        let replacement = lower_initial_value_transfer(
            self,
            value,
            "element field assignment",
            OwnedBindingUse::Copy,
        )?;
        let replacement = self.coerce_value(replacement, &leaf_ty, Provenance::Site(value.site))?;

        // One evaluation of the index expression feeds both the read and the
        // write; lowering it twice would run its effects twice.
        let position = self.lower_expr(index)?;
        let element = self
            .lower_runtime_operation_with(
                root,
                hew_types::RuntimeCallFamily::Vector(hew_types::runtime_call::VecValueOp::Index),
                &[container.as_ref(), index.as_ref()],
                true,
                &[(1, position)],
            )?
            .ok_or_else(|| "element read must produce a semantic copy".to_string())?;
        if self.value_own_kind(element) != Some(OwnKind::Owned) {
            return Err(
                "assignment through a collection element requires an owning element read".into(),
            );
        }
        self.assign_through_owned_value(
            element,
            &element_ty,
            &projections,
            replacement,
            provenance,
        )?;

        let mut operation = target.clone();
        operation.ty = ResolvedTy::Unit;
        self.lower_runtime_operation_with(
            &operation,
            hew_types::RuntimeCallFamily::Vector(hew_types::runtime_call::VecValueOp::Set),
            &[container.as_ref(), index.as_ref(), root],
            false,
            &[(1, position), (2, element)],
        )?;
        Ok(true)
    }

    /// Assignment and runtime receiver mutation resolve and rebuild the same
    /// mutable place. Evaluate the RHS before taking its current root apart.
    fn lower_field_assignment(&mut self, target: &HirExpr, value: &HirExpr) -> Result<(), String> {
        let place = self.resolve_mutable_place(target)?;
        let replacement = lower_initial_value_transfer(
            self,
            value,
            "record field assignment",
            OwnedBindingUse::Copy,
        )?;
        let replacement =
            self.coerce_value(replacement, &place.leaf_ty, Provenance::Site(value.site))?;
        let provenance = Provenance::Site(target.site);
        if let Some(projected) = self.owned_projection(&place)? {
            return self.store_projected(projected, replacement, provenance);
        }
        if let BindingTarget::Place(root) = self.binding_target(place.binding)? {
            if !place.projections.is_empty() {
                return self.assign_through_owned_place(root, &place, replacement, provenance);
            }
        }
        let (_, parents) = self.take_scalar_place(&place, &provenance)?;
        self.replace_scalar_aggregate_leaf(place.binding, replacement, parents, &provenance)
    }

    fn resolve_mutable_place(&mut self, target: &HirExpr) -> Result<BindingPlace, String> {
        let place = self
            .resolve_binding_place(target)?
            .ok_or_else(|| "mutable place requires a local binding root".to_string())?;
        let declaration = *self
            .binding_declarations
            .get(&place.binding)
            .ok_or_else(|| {
                format!(
                    "mutable place root `{}` has no source declaration",
                    place.binding
                )
            })?;
        if !self.source_bindings[declaration].mutable {
            return Err(format!(
                "mutable place root `{}` is not mutable",
                place.binding
            ));
        }
        Ok(place)
    }

    /// The aggregate selections between `target` and the expression they are
    /// rooted at, outermost first. The root is whatever the chain reaches: a
    /// binding, a collection element, or any other expression.
    fn projection_chain<'expr>(
        &mut self,
        target: &'expr HirExpr,
    ) -> Result<(&'expr HirExpr, Vec<AggregateSelection>), String> {
        let mut root = target;
        let mut projections = Vec::new();
        loop {
            let (object, shape, index) = match &root.kind {
                HirExprKind::FieldAccess { object, field } => {
                    let (shape, index) = self.aggregate_projection_shape(root, object, field)?;
                    (object.as_ref(), shape, index)
                }
                HirExprKind::TupleIndex { tuple, index } => {
                    let index = self.tuple_projection_index(root, tuple, *index)?;
                    let shape = self.service.require_aggregate_shape(&self.ty(&tuple.ty))?;
                    (tuple.as_ref(), shape, index)
                }
                HirExprKind::SubsumedValue { source } => {
                    root = source;
                    continue;
                }
                _ => break,
            };
            projections.push((
                self.ty(&object.ty),
                shape,
                usize::try_from(index).map_err(|_| "mutable place field exceeds usize")?,
            ));
            root = object;
        }
        projections.reverse();
        Ok((root, projections))
    }

    fn resolve_binding_place(&mut self, target: &HirExpr) -> Result<Option<BindingPlace>, String> {
        let (root, projections) = self.projection_chain(target)?;
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = root.kind
        else {
            return Ok(None);
        };
        Ok(Some(BindingPlace {
            binding,
            root_ty: self.ty(&root.ty),
            leaf_ty: self.ty(&target.ty),
            projections,
        }))
    }

    /// Extract a scalar leaf and retain its non-owning sibling fields.
    fn take_scalar_place(
        &mut self,
        place: &BindingPlace,
        provenance: &Provenance,
    ) -> Result<(ValueId, Vec<ScalarAggregateParent>), String> {
        let mut current = self.scalar_binding(place.binding)?;
        if self.value_own_kind(current) != Some(OwnKind::None)
            || self.value_ty(current).as_ref() != Some(&place.root_ty)
        {
            return Err("scalar aggregate update requires a non-owning root".into());
        }
        let mut parents = Vec::new();
        for (ty, shape, index) in &place.projections {
            let fields = self.emit_destructure_value(current, ty, *shape, provenance.clone())?;
            if fields.iter().any(|field| field.own != OwnKind::None) {
                return Err("scalar aggregate update cannot acquire ownership".into());
            }
            current = fields[*index].id;
            parents.push(ScalarAggregateParent {
                ty: ty.clone(),
                shape: *shape,
                index: *index,
                fields,
            });
        }
        Ok((current, parents))
    }

    /// Rebuild a non-owning aggregate after a scalar field assignment.
    fn replace_scalar_aggregate_leaf(
        &mut self,
        binding: BindingId,
        replacement: ValueId,
        parents: Vec<ScalarAggregateParent>,
        provenance: &Provenance,
    ) -> Result<(), String> {
        if self.value_own_kind(replacement) != Some(OwnKind::None) {
            return Err("scalar aggregate replacement cannot carry ownership".into());
        }
        let mut updated = replacement;
        for ScalarAggregateParent {
            ty,
            shape,
            index,
            fields,
        } in parents.into_iter().rev()
        {
            let fields = fields
                .into_iter()
                .enumerate()
                .map(|(position, field)| Operand {
                    value: if position == index { updated } else { field.id },
                })
                .collect();
            updated = self.emit_typed(
                provenance.clone(),
                &ty,
                SemOpKind::AggregateMake { shape, fields },
            )?;
        }
        self.bindings.insert(binding, BindingTarget::Value(updated));
        self.record_binding_version(binding, updated)
    }

    /// Lower an expression whose value is intentionally discarded.
    ///
    /// Scalar expressions keep their ordinary one-result SSA operation even
    /// when the result is unused.  A unit direct call is different: there is
    /// no semantic value to define, but the call itself must remain in SIR so
    /// later lowering can realize its call/continuation CFG edge.
    #[expect(
        clippy::too_many_lines,
        reason = "effect-position dispatch keeps control flow and cleanup together"
    )]
    #[allow(
        deprecated,
        reason = "a trait method reached through a where-clause bound is not builtin-generic dispatch, so `ResolvedImplCall` does not carry it; these arms read the node, they do not construct one"
    )]
    fn lower_discarded_expr(&mut self, expr: &HirExpr) -> Result<(), String> {
        match &expr.kind {
            HirExprKind::ActorDelivery {
                operation: hew_types::actor_delivery::ActorDeliveryCall::AwaitClosed,
                ..
            } => {
                self.lower_actor_boundary(expr)?;
                return Ok(());
            }
            HirExprKind::ActorDelivery { .. } => {
                let value = self.lower_actor_delivery(expr)?;
                if self.owned_live.contains_key(&value) {
                    self.emit_destroy(value)?;
                }
                return Ok(());
            }
            HirExprKind::Yield { value, yield_ty } => {
                return self.lower_generator_yield(expr, value.as_deref(), yield_ty)
            }
            HirExprKind::ScopeRecovery {
                scope,
                error,
                handler,
            } if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) => {
                self.lower_scope_recovery(expr, scope, error, handler)?;
                return Ok(());
            }

            HirExprKind::Select(select)
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_task_select(expr, select)?;
                return Ok(());
            }
            HirExprKind::AwaitTask { operand, .. }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_task_await(expr, operand)?;
                return Ok(());
            }
            HirExprKind::Race { body }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_race(body)?;
                return Ok(());
            }
            HirExprKind::Scope { body }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_task_scope(body)?;
                return Ok(());
            }
            HirExprKind::ScopeDeadline { duration, body }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_task_scope_with_deadline(body, Some(duration))?;
                return Ok(());
            }
            HirExprKind::SubsumedValue { source } => {
                if self.ty(&source.ty) != self.ty(&expr.ty) {
                    return Err(
                        "transparent discarded expression must preserve its exact type".into(),
                    );
                }
                return self.lower_discarded_expr(source);
            }

            HirExprKind::Return { value } => return self.lower_function_return(value.as_deref()),
            HirExprKind::Call {
                target: CallTarget::Builtin { endpoint },
                args,
                ..
            } if endpoint == "panic" => return self.lower_panic(expr, args),
            _ => {}
        }
        if expr.intent != IntentKind::Consume {
            require_initial_scalar_read(expr.intent)
                .map_err(|reason| format!("discarded expression: {reason}"))?;
        }
        let live_before_expression: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        if expr.intent == IntentKind::Consume
            && !matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never)
        {
            // `let _ = value` takes the value and releases it here.
            let value =
                lower_initial_value_transfer(self, expr, "discarded value", OwnedBindingUse::Move)?;
            if self.owned_live.contains_key(&value) && !live_before_expression.contains(&value) {
                self.emit_destroy(value)?;
            }
            return Ok(());
        }
        match &expr.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(_),
                ..
            } => {
                // Reading an existing binding in effect position needs no
                // owned copy. Keep the borrow so availability is still checked.
                let mut loans = Vec::new();
                self.lower_borrowed_read(expr, &mut loans)?;
                return self.end_call_loans(&loans);
            }
            HirExprKind::Block(block) => {
                if let Some(value) = self.lower_scoped_block(block, OwnedBindingUse::Copy)? {
                    if self.owned_live.contains_key(&value.value)
                        && !live_before_expression.contains(&value.value)
                    {
                        self.emit_destroy(value.value)?;
                    }
                }
                return Ok(());
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) => {
                return self.lower_unit_if(condition, then_expr, else_expr.as_deref());
            }
            HirExprKind::Match { scrutinee, arms }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_match_control(expr, scrutinee, arms)?;
                return Ok(());
            }
            HirExprKind::Break { label, value } => {
                return self.lower_loop_exit(false, label.as_deref(), value.as_deref());
            }
            HirExprKind::Continue { label } => {
                return self.lower_loop_exit(true, label.as_deref(), None);
            }
            HirExprKind::While {
                label,
                condition,
                body,
            } => {
                return self.lower_while(label.as_deref(), Some(condition), body);
            }
            HirExprKind::Loop { label, body } => {
                return self.lower_while(label.as_deref(), None, body);
            }
            HirExprKind::ForRange {
                label,
                binding,
                start,
                end,
                inclusive,
                step,
                descending,
                body,
            } => {
                return self.lower_for_range(
                    label.as_deref(),
                    binding,
                    start,
                    end,
                    step,
                    *inclusive,
                    *descending,
                    body,
                );
            }
            _ => {}
        }
        if matches!(
            expr.kind,
            HirExprKind::Call { .. } | HirExprKind::CallTraitMethodStatic { .. }
        ) {
            if let Some(value) = self.lower_call(expr, false)? {
                if self.owned_live.contains_key(&value) && !live_before_expression.contains(&value)
                {
                    self.emit_destroy(value)?;
                }
            }
            return Ok(());
        }
        let value = self.lower_expr(expr)?;
        if self.owned_live.contains_key(&value) && !live_before_expression.contains(&value) {
            self.emit_destroy(value)?;
        }
        Ok(())
    }

    /// Preserve the panic message before releasing its owner and propagating
    /// the active fault through the ordinary function cleanup boundary.
    fn lower_panic(&mut self, expr: &HirExpr, args: &[HirExpr]) -> Result<(), String> {
        let [message] = args else {
            return Err("panic requires exactly one string message".into());
        };
        if self.ty(&message.ty) != ResolvedTy::String || self.ty(&expr.ty) != ResolvedTy::Never {
            return Err("panic requires a string message and a Never result".into());
        }
        let mut loans = Vec::new();
        let operand = self.lower_call_read(message, &mut loans, true, true)?;
        self.finish_panic(operand, &loans)
    }

    fn lower_assert(&mut self, expr: &HirExpr, args: &[HirExpr]) -> Result<(), String> {
        let [condition] = args else {
            return Err("assert requires exactly one boolean condition".into());
        };
        if self.ty(&condition.ty) != ResolvedTy::Bool || self.ty(&expr.ty) != ResolvedTy::Unit {
            return Err("assert requires a boolean condition and a unit result".into());
        }
        let condition = self.lower_read_operand(condition, "assert condition")?;
        let success = self.new_block(Vec::new());
        let failure = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: success,
                args: Vec::new(),
            },
            else_target: Edge {
                target: failure,
                args: Vec::new(),
            },
        })?;
        let before = self.control_state();
        self.current = failure;
        let literal = self.service.intern_string("assertion failed");
        let message = self.emit_typed(
            Provenance::Site(expr.site),
            &ResolvedTy::String,
            SemOpKind::ConstStr(literal),
        )?;
        self.finish_panic(Operand { value: message }, &[])?;
        self.restore_control_state(&before);
        self.current = success;
        Ok(())
    }

    fn finish_panic(&mut self, operand: Operand, loans: &[ValueId]) -> Result<(), String> {
        let cleanup = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Panic {
            message: crate::BoundaryOperand {
                operand,
                decision: crate::BoundaryDecision::Borrow,
            },
            cleanup: Edge {
                target: cleanup,
                args: Vec::new(),
            },
        })?;
        self.current = cleanup;
        self.end_call_loans(loans)?;
        self.finish_fault_exit()
    }

    /// Seal the current block with the one function-return cleanup contract.
    /// Both statement returns and Never-typed HIR return expressions use this
    /// path, so a divergent expression cannot manufacture a placeholder SSA
    /// value or continue evaluating sibling operands.
    fn lower_function_return(&mut self, value: Option<&HirExpr>) -> Result<(), String> {
        if self.in_deferred_body() {
            return Err("return and error propagation cannot escape a deferred body".into());
        }
        let mut value = match value {
            Some(expr) if self.ty(&expr.ty) == ResolvedTy::Unit => {
                lower_initial_unit_return(self, expr)?;
                None
            }
            Some(expr) => Some(crate::BoundaryOperand {
                operand: Operand {
                    value: lower_initial_value_transfer(
                        self,
                        expr,
                        "return value",
                        OwnedBindingUse::Return,
                    )?,
                },
                decision: crate::BoundaryDecision::Move,
            }),
            None => None,
        };
        if let Some(value) = &mut value {
            value.operand.value = self.coerce_value(
                value.operand.value,
                &self.callable.signature.return_ty.clone(),
                Provenance::Synthesized,
            )?;
        }
        self.finish_return_value(value)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the closed initial HIR-to-SIR expression mapping remains intentionally local"
    )]
    fn lower_expr(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        self.lower_expr_with_binding_use(expr, OwnedBindingUse::Copy)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the closed initial HIR-to-SIR expression mapping remains intentionally local"
    )]
    #[allow(
        deprecated,
        reason = "a trait method reached through a where-clause bound is not builtin-generic dispatch, so `ResolvedImplCall` does not carry it; these arms read the node, they do not construct one"
    )]
    fn lower_expr_with_binding_use(
        &mut self,
        expr: &HirExpr,
        binding_use: OwnedBindingUse,
    ) -> Result<ValueId, String> {
        // A supervisor pool accessor is decided by the checker, not by the
        // expression shape: `sup.pool[i]` and `sup.pool.get(i)` are an ordinary
        // index and call until this site table says otherwise.
        if let Some(kind) = self
            .service
            .module
            .pool_accessor_sites
            .get(&expr.site)
            .map(|accessor| accessor.kind)
        {
            return self.lower_pool_accessor(expr, kind, false);
        }
        match &expr.kind {
            HirExprKind::Literal(literal) => self.lower_literal(expr, literal),
            HirExprKind::RcIntrinsic {
                op,
                receiver,
                value,
                ..
            } => {
                let mut operands: Vec<&HirExpr> = Vec::with_capacity(2);
                operands.extend(receiver.as_deref());
                operands.extend(value.as_deref());
                let family = shared_handle_family(*op);
                if self.ty(&expr.ty) == ResolvedTy::Unit {
                    self.lower_runtime_operation(expr, family, &operands, false)?;
                    return self.emit(expr, SemOpKind::ConstUnit);
                }
                self.lower_runtime_operation(expr, family, &operands, true)?
                    .ok_or_else(|| format!("`{op:?}` must produce a shared-handle value"))
            }
            HirExprKind::Select(select) => match self.lower_task_select(expr, select)? {
                Some(value) => Ok(value),
                None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                None => Err("divergent select cannot produce a SIR value".into()),
            },
            HirExprKind::GenBlock { .. } => self.lower_generator(expr),
            HirExprKind::GeneratorNext { receiver, .. } => {
                self.lower_generator_next(expr, receiver)
            }
            HirExprKind::Yield { value, yield_ty } => {
                self.lower_generator_yield(expr, value.as_deref(), yield_ty)?;
                self.emit(expr, SemOpKind::ConstUnit)
            }
            HirExprKind::Closure { .. } => self.lower_closure(expr),
            HirExprKind::ForkBlock { body, captures, .. } => {
                self.lower_fork_block(expr, body, captures)
            }
            HirExprKind::AwaitTask { operand, .. } => match self.lower_task_await(expr, operand)? {
                Some(value) => Ok(value),
                None => self.emit(expr, SemOpKind::ConstUnit),
            },
            HirExprKind::Race { body } => match self.lower_race(body)? {
                Some(value) => Ok(value),
                None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                None => Err("divergent race cannot produce a SIR value".into()),
            },
            HirExprKind::Scope { body } => match self.lower_task_scope(body)? {
                Some(value) => Ok(value),
                None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                None => Err("divergent scope cannot produce a SIR value".into()),
            },
            HirExprKind::ScopeDeadline { duration, body } => {
                match self.lower_task_scope_with_deadline(body, Some(duration))? {
                    Some(value) => Ok(value),
                    None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                    None => Err("divergent scope cannot produce a SIR value".into()),
                }
            }
            HirExprKind::ScopeRecovery {
                scope,
                error,
                handler,
            } => match self.lower_scope_recovery(expr, scope, error, handler)? {
                Some(value) => Ok(value),
                None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                None => Err("divergent recovery cannot produce a SIR value".into()),
            },
            HirExprKind::WireCodec {
                direction,
                operand,
                value_ty,
            } => self.lower_wire_codec(expr, *direction, operand, value_ty),
            HirExprKind::RecordCloneCall { src, .. } => {
                let mut loans = Vec::new();
                let source = self.lower_borrowed_read(src, &mut loans)?;
                // A record whose fields all copy by value carries no ownership,
                // so the borrowed read is already the independent copy. Only an
                // owned record needs the ownership operation.
                let ty = self.ty(&expr.ty);
                let result =
                    if OwnKind::of_ty(&ty, self.service.checked_facts.rows())? == OwnKind::Owned {
                        self.emit(expr, SemOpKind::CopyValue { source })?
                    } else {
                        source.value
                    };
                self.end_call_loans(&loans)?;
                Ok(result)
            }
            HirExprKind::Spawn { .. } => self
                .lower_actor_boundary(expr)?
                .ok_or_else(|| "actor spawn lacks its handle result".into()),
            HirExprKind::ActorSelf => self
                .lower_actor_boundary(expr)?
                .ok_or_else(|| "`self` lacks its actor handle result".into()),
            HirExprKind::ActorMessage { .. } => self.lower_actor_message(expr),
            HirExprKind::ActorDelivery { .. } => self.lower_actor_delivery(expr),
            HirExprKind::ActorAsk { .. } => self.lower_actor_ask(expr),
            HirExprKind::ActorGenStream { .. } => self.lower_actor_stream(expr),
            HirExprKind::CoerceToDynTrait {
                value,
                concrete_type,
                vtable_entries,
                ..
            } => self.lower_dyn_make(expr, value, concrete_type, vtable_entries),
            HirExprKind::CallDynMethod {
                receiver,
                slot,
                args,
                signature,
                ..
            } => self
                .lower_dyn_call(expr, receiver, *slot, args, signature, true)?
                .ok_or_else(|| "dynamic dispatch produced no SIR value".to_string()),
            HirExprKind::ArrayLiteral { elements } => self.lower_array_make(expr, elements),
            HirExprKind::ArrayRepeat { value } => self.lower_array_repeat(expr, value),
            HirExprKind::TupleLiteral { elements } => self.lower_tuple_make(expr, elements),
            HirExprKind::TupleIndex { tuple, index } => self.lower_tuple_get(expr, tuple, *index),
            HirExprKind::StructInit { fields, base, .. } => {
                self.lower_aggregate_make(expr, fields, base.as_deref())
            }
            HirExprKind::MachineVariantCtor {
                state_idx, payload, ..
            } => self.lower_variant_make(expr, *state_idx, payload.as_deref()),
            HirExprKind::AwaitRestart { child } => self.lower_supervisor_await_restart(expr, child),
            HirExprKind::FieldAccess { object, field } => {
                if let Some(slot) = self.service.module.supervisor_child_slots.get(&expr.site) {
                    return self.lower_supervisor_child(expr, object, slot);
                }
                self.lower_aggregate_project(expr, object, field)
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } => {
                if let Some((_, field)) = self.capture_field(*binding) {
                    let take = binding_use == OwnedBindingUse::Move
                        && field.consumption == hew_types::ClosureCaptureConsumption::Consumed;
                    return self.load_capture(*binding, Provenance::Site(expr.site), take);
                }
                match self.binding_target(*binding)? {
                    BindingTarget::Value(value) => {
                        if !matches!(binding_use, OwnedBindingUse::Copy | OwnedBindingUse::Probe) {
                            self.require_selected_binding(*binding, value)?;
                        }
                        Ok(value)
                    }
                    BindingTarget::Place(place) => self.emit(expr, SemOpKind::LoadCopy { place }),
                }
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Const(item),
                ..
            } => {
                let constant = self
                    .service
                    .module
                    .items
                    .iter()
                    .find_map(|candidate| match candidate {
                        HirItem::Const(constant) if constant.id == *item => Some(constant),
                        _ => None,
                    })
                    .ok_or("const reference has no HIR declaration")?;
                let literal = match &constant.value {
                    hew_hir::HirConstValue::Integer(value) => HirLiteral::Integer(*value),
                    hew_hir::HirConstValue::String(value) => HirLiteral::String(value.clone()),
                    hew_hir::HirConstValue::Float(value) => HirLiteral::Float(*value),
                };
                self.lower_literal(expr, &literal)
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Item(item),
                ..
            } => {
                let declaration = self
                    .service
                    .table
                    .functions_by_item
                    .get(item)
                    .ok_or_else(|| "function value has no checked HIR declaration".to_string())?
                    .declaration
                    .clone();
                let target = self.service.resolve_direct_call(
                    &declaration,
                    expr.site,
                    &self.substitution,
                )?;
                let ty = ResolvedTy::Function {
                    params: target
                        .signature
                        .params
                        .iter()
                        .map(|param| param.ty.clone())
                        .collect(),
                    ret: Box::new(target.signature.return_ty.clone()),
                    capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                };
                let value = self.emit_typed(
                    Provenance::Site(expr.site),
                    &ty,
                    SemOpKind::FunctionMake {
                        callable: target.id,
                    },
                )?;
                self.coerce_value(value, &self.ty(&expr.ty), Provenance::Site(expr.site))
            }
            HirExprKind::Unary { op, operand, .. } => {
                let value = self.lower_read_operand(operand, "unary operand")?;
                if *op == hew_parser::ast::UnaryOp::Negate && self.ty(&expr.ty).is_signed_integer()
                {
                    let zero = self.emit(expr, SemOpKind::ConstInteger(0))?;
                    self.lower_checked_binary(
                        expr,
                        hew_parser::ast::BinaryOp::Subtract,
                        Operand { value: zero },
                        value,
                    )
                } else {
                    self.emit(expr, SemOpKind::Unary { op: *op, value })
                }
            }
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::And,
                left,
                right,
            } => self.lower_logical_and(expr, left, right),
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::Or,
                left,
                right,
            } => self.lower_logical_or(expr, left, right),
            HirExprKind::Binary { op, left, right } => {
                if matches!(
                    op,
                    hew_parser::ast::BinaryOp::Equal | hew_parser::ast::BinaryOp::NotEqual
                ) && matches!(
                    self.ty(&left.ty),
                    ResolvedTy::Bytes
                        | ResolvedTy::Tuple(_)
                        | ResolvedTy::Named { .. }
                        | ResolvedTy::Unit
                ) {
                    let equals = self.lower_value_equality(expr, [left, right])?;
                    return if *op == hew_parser::ast::BinaryOp::NotEqual {
                        self.emit(
                            expr,
                            SemOpKind::Unary {
                                op: hew_parser::ast::UnaryOp::Not,
                                value: Operand { value: equals },
                            },
                        )
                    } else {
                        Ok(equals)
                    };
                }
                if self.ty(&left.ty) == ResolvedTy::String {
                    return match op {
                        hew_parser::ast::BinaryOp::Add => self
                            .lower_runtime_operation(
                                expr,
                                hew_types::RuntimeCallFamily::StringConcat,
                                &[left.as_ref(), right.as_ref()],
                                true,
                            )?
                            .ok_or_else(|| "string concatenation must produce a value".to_string()),
                        hew_parser::ast::BinaryOp::Equal | hew_parser::ast::BinaryOp::NotEqual => {
                            let equals = self
                                .lower_runtime_operation(
                                    expr,
                                    hew_types::RuntimeCallFamily::StringEquals,
                                    &[left.as_ref(), right.as_ref()],
                                    true,
                                )?
                                .ok_or_else(|| {
                                    "string comparison must produce a value".to_string()
                                })?;
                            if *op == hew_parser::ast::BinaryOp::NotEqual {
                                self.emit(
                                    expr,
                                    SemOpKind::Unary {
                                        op: hew_parser::ast::UnaryOp::Not,
                                        value: Operand { value: equals },
                                    },
                                )
                            } else {
                                Ok(equals)
                            }
                        }
                        hew_parser::ast::BinaryOp::Less
                        | hew_parser::ast::BinaryOp::LessEqual
                        | hew_parser::ast::BinaryOp::Greater
                        | hew_parser::ast::BinaryOp::GreaterEqual => {
                            let mut comparison = expr.clone();
                            comparison.ty = ResolvedTy::I32;
                            let ordering = self
                                .lower_runtime_operation(
                                    &comparison,
                                    hew_types::RuntimeCallFamily::StringCompare,
                                    &[left.as_ref(), right.as_ref()],
                                    true,
                                )?
                                .ok_or("string ordering must produce a value")?;
                            let zero = self.emit_typed(
                                Provenance::Site(expr.site),
                                &ResolvedTy::I32,
                                SemOpKind::ConstInteger(0),
                            )?;
                            self.emit(
                                expr,
                                SemOpKind::Binary {
                                    op: *op,
                                    lhs: Operand { value: ordering },
                                    rhs: Operand { value: zero },
                                },
                            )
                        }
                        _ => Err(format!(
                            "string binary `{op}` has no ownership-SIR runtime operation"
                        )),
                    };
                }
                let lhs = self.lower_read_operand(left, "binary left operand")?;
                let rhs = self.lower_read_operand(right, "binary right operand")?;
                if crate::checked_binary_failure_kinds(*op, &self.ty(&expr.ty)).is_some() {
                    self.lower_checked_binary(expr, *op, lhs, rhs)
                } else {
                    self.emit(expr, SemOpKind::Binary { op: *op, lhs, rhs })
                }
            }
            HirExprKind::NumericCast { value, to_ty, .. } => {
                let value = self.lower_read_operand(value, "cast operand")?;
                self.emit(
                    expr,
                    SemOpKind::Cast {
                        value,
                        to: self.ty(to_ty),
                    },
                )
            }
            HirExprKind::VarSelfMethodCall { .. } => self.lower_var_self_call(expr),
            HirExprKind::Call { .. } | HirExprKind::CallTraitMethodStatic { .. }
                if self.ty(&expr.ty) == ResolvedTy::Unit =>
            {
                self.lower_call(expr, false)?;
                self.emit(expr, SemOpKind::ConstUnit)
            }
            HirExprKind::Call { .. } | HirExprKind::CallTraitMethodStatic { .. } => self
                .lower_call(expr, true)?
                .ok_or_else(|| "value-producing checked call has no result".to_string()),
            HirExprKind::SubsumedValue { source, .. } => {
                self.lower_expr_with_binding_use(source, binding_use)
            }
            HirExprKind::Index { container, index }
                if matches!(self.ty(&container.ty), ResolvedTy::Array(_, _)) =>
            {
                self.lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::Array(
                        hew_types::runtime_call::ArrayValueOp::Index,
                    ),
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "array index must produce a value".to_string())
            }
            HirExprKind::BorrowedIndex { container, index }
                if matches!(self.ty(&container.ty), ResolvedTy::Array(_, _)) =>
            {
                self.lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::Array(
                        hew_types::runtime_call::ArrayValueOp::IndexBorrow,
                    ),
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "array index must produce an element loan".to_string())
            }
            HirExprKind::Index { container, index }
                if matches!(
                    collection_type_arguments(&self.ty(&container.ty)),
                    Some((
                        hew_types::BuiltinType::Vec | hew_types::BuiltinType::HashMap,
                        _
                    ))
                ) =>
            {
                let family = match collection_type_arguments(&self.ty(&container.ty)) {
                    Some((hew_types::BuiltinType::Vec, _)) => {
                        hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Index)
                    }
                    Some((hew_types::BuiltinType::HashMap, _)) => {
                        hew_types::RuntimeCallFamily::Map(
                            hew_types::runtime_call::MapValueOp::Index,
                        )
                    }
                    _ => unreachable!("matched a canonical indexed collection"),
                };
                self.lower_runtime_operation(
                    expr,
                    family,
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "collection index must produce a semantic copy".to_string())
            }
            HirExprKind::ResolvedImplCall {
                target:
                    CallTarget::RuntimeCollection(
                        hew_types::MethodTargetFamily::HashMap(hew_types::HashMapMethod::Clone)
                        | hew_types::MethodTargetFamily::HashSet(hew_types::HashSetMethod::Clone)
                        | hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Clone),
                    ),
                receiver,
                args,
                ..
            } if args.is_empty() => {
                let mut loans = Vec::new();
                let source = self.lower_borrowed_read(receiver, &mut loans)?;
                let copy = self.emit(expr, SemOpKind::CopyValue { source })?;
                self.end_call_loans(&loans)?;
                Ok(copy)
            }
            // D432: the checker decided this read is a loan of the element the
            // vector still owns.
            HirExprKind::BorrowedIndex { container, index } => self
                .lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::IndexBorrow),
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "borrowed element read must produce a SIR value".to_string()),
            HirExprKind::Index { container, index }
                if self.ty(&container.ty) == ResolvedTy::Bytes =>
            {
                self.lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::BytesIndex,
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "bytes index must produce a SIR value".to_string())
            }
            HirExprKind::Index { container, index }
                if self.ty(&container.ty) == ResolvedTy::String =>
            {
                self.lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::StringIndex,
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "string index must produce a SIR value".to_string())
            }
            // `x[a..b]` / `x[a..]` / `x[..b]` / `x[..]` over `string`, `bytes`
            // and `Vec<T>`. An absent start is the literal zero; an absent end
            // routes to the receiver's own open-ended family so the container
            // expression is evaluated exactly once. HIR has already rewritten
            // `x[a..=b]` to the exclusive `x[a..b + 1]`.
            HirExprKind::Slice {
                container,
                start,
                end,
            } => {
                let container_ty = self.ty(&container.ty);
                let (ranged, open) = if container_ty == ResolvedTy::String {
                    (
                        hew_types::RuntimeCallFamily::StringSliceCodepoints,
                        hew_types::RuntimeCallFamily::StringSliceCodepointsFrom,
                    )
                } else if container_ty == ResolvedTy::Bytes {
                    (
                        hew_types::RuntimeCallFamily::BytesSlice,
                        hew_types::RuntimeCallFamily::BytesSliceFrom,
                    )
                } else if matches!(
                    collection_type_arguments(&container_ty),
                    Some((hew_types::BuiltinType::Vec, _))
                ) {
                    (
                        hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Slice),
                        hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::SliceFrom),
                    )
                } else {
                    return Err(format!(
                        "`{}` has no ownership-SIR range-slice operation",
                        container_ty.user_facing()
                    ));
                };
                let mut zero = (**container).clone();
                zero.ty = ResolvedTy::I64;
                zero.kind = HirExprKind::Literal(HirLiteral::Integer(0));
                let start: &HirExpr = start.as_deref().unwrap_or(&zero);
                match end {
                    Some(end) => self.lower_runtime_operation(
                        expr,
                        ranged,
                        &[container.as_ref(), start, end.as_ref()],
                        true,
                    )?,
                    None => self.lower_runtime_operation(
                        expr,
                        open,
                        &[container.as_ref(), start],
                        true,
                    )?,
                }
                .ok_or_else(|| "range slice must produce a SIR value".to_string())
            }
            // `re"..."` in value position. The module compiles every literal
            // once at process entry; this materializes an owned pattern from
            // the slot the match arms already read.
            HirExprKind::RegexLiteralRef { literal_id, .. } => {
                let pattern_ty = self.ty(&expr.ty);
                let shape = self.service.require_aggregate_shape(&pattern_ty)?;
                let AggregateShapeRef::Record(id) = shape else {
                    return Err("a regex literal must produce a named pattern record".to_string());
                };
                let [field] = self.service.aggregate_shapes[id.0 as usize]
                    .fields
                    .as_slice()
                else {
                    return Err("a regex pattern record holds exactly its handle".to_string());
                };
                let handle_ty = field.ty.clone();
                let mut slot = expr.clone();
                slot.ty = ResolvedTy::I64;
                slot.kind = HirExprKind::Literal(HirLiteral::Integer(i128::from(*literal_id)));
                let mut handle = expr.clone();
                handle.ty = handle_ty;
                let handle = self
                    .lower_runtime_operation(
                        &handle,
                        hew_types::RuntimeCallFamily::RegexHandle,
                        &[&slot],
                        true,
                    )?
                    .ok_or_else(|| "a regex literal must produce a SIR value".to_string())?;
                let pattern = self.emit(
                    expr,
                    SemOpKind::AggregateMake {
                        shape,
                        fields: vec![Operand { value: handle }],
                    },
                )?;
                self.owned_live.remove(&handle);
                Ok(pattern)
            }
            HirExprKind::Block(block) => self
                .lower_scoped_block(block, binding_use)?
                .map(|value| value.value)
                .ok_or_else(|| "a divergent block cannot produce a SIR value".to_string()),
            HirExprKind::If {
                condition,
                then_expr,
                else_expr: Some(else_expr),
            } => self.lower_if(expr, condition, then_expr, else_expr),
            HirExprKind::Match { scrutinee, arms } => self.lower_match(expr, scrutinee, arms),
            HirExprKind::If {
                else_expr: None, ..
            } => Err(
                "one-armed if expressions are deferred until unit values are modeled".to_string(),
            ),
            _ => Err(format!(
                "unsupported HIR expression kind `{}` in the initial SIR subset",
                hir_expr_kind_name(&expr.kind)
            )),
        }
    }

    fn lower_literal(&mut self, expr: &HirExpr, literal: &HirLiteral) -> Result<ValueId, String> {
        match literal {
            HirLiteral::Unit if self.ty(&expr.ty) == ResolvedTy::Unit => {
                self.emit(expr, SemOpKind::ConstUnit)
            }
            HirLiteral::Integer(value) => {
                if !self.ty(&expr.ty).is_integer() {
                    return Err(format!(
                        "integer literal resolved as `{}` needs a dedicated SIR literal representation",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstInteger(*value))
            }
            HirLiteral::Bool(value) => {
                if self.ty(&expr.ty) != ResolvedTy::Bool {
                    return Err(format!(
                        "boolean literal resolved as `{}` violates the SIR bool literal invariant",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstBool(*value))
            }
            HirLiteral::Float(value) => {
                if !self.ty(&expr.ty).is_float() {
                    return Err(format!(
                        "floating literal resolved as `{}` needs a dedicated SIR literal representation",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstFloat(*value))
            }
            HirLiteral::Char(value) => {
                if self.ty(&expr.ty) != ResolvedTy::Char {
                    return Err(format!(
                        "character literal resolved as `{}` violates the SIR char literal invariant",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstChar(*value))
            }
            HirLiteral::String(value) => {
                let literal = self.service.intern_string(value);
                self.emit(expr, SemOpKind::ConstStr(literal))
            }
            HirLiteral::Bytes(value) => {
                let literal = self.service.intern_bytes(value);
                self.emit(expr, SemOpKind::ConstBytes(literal))
            }
            HirLiteral::Duration(value) if self.ty(&expr.ty) == ResolvedTy::Duration => {
                self.emit(expr, SemOpKind::ConstDuration(*value))
            }
            _ => Err("unsupported HIR literal kind in the initial SIR subset".to_string()),
        }
    }

    fn lower_variant_make(
        &mut self,
        expr: &HirExpr,
        variant: usize,
        payload: Option<&[(String, HirExpr)]>,
    ) -> Result<ValueId, String> {
        let enum_ty = self.ty(&expr.ty);
        let shape = self.service.require_variant_shape(&enum_ty)?;
        let descriptor = self
            .service
            .variant_shapes
            .get(usize::try_from(shape.0).map_err(|_| "variant shape id exceeds usize")?)
            .filter(|descriptor| descriptor.id == shape)
            .ok_or_else(|| format!("variant shape {} disappeared during lowering", shape.0))?;
        let declared = descriptor.variants.get(variant).cloned().ok_or_else(|| {
            format!(
                "variant constructor tag {variant} is absent from exact shape `{}`",
                enum_ty.user_facing()
            )
        })?;
        let supplied = payload.unwrap_or_default();
        if supplied.len() != declared.fields.len() {
            return Err(format!(
                "variant constructor {} for `{}` has {} field(s), expected {}",
                variant,
                enum_ty.user_facing(),
                supplied.len(),
                declared.fields.len()
            ));
        }
        let mut ordered = vec![None; declared.fields.len()];
        for (name, field) in supplied {
            let index = declared
                .fields
                .iter()
                .position(|candidate| candidate.name == *name)
                .ok_or_else(|| {
                    format!(
                        "variant constructor field `{name}` is absent from exact shape `{}` tag {variant}",
                        enum_ty.user_facing()
                    )
                })?;
            if ordered[index].is_some() {
                return Err(format!(
                    "variant constructor repeats field `{name}` for `{}` tag {variant}",
                    enum_ty.user_facing()
                ));
            }
            let value = lower_initial_value_transfer(
                self,
                field,
                &format!("variant field `{name}`"),
                OwnedBindingUse::Copy,
            )?;
            ordered[index] = Some(Operand {
                value: self.coerce_value(
                    value,
                    &declared.fields[index].ty,
                    Provenance::Site(field.site),
                )?,
            });
        }
        let fields = ordered
            .into_iter()
            .zip(&declared.fields)
            .map(|(operand, field)| {
                operand.ok_or_else(|| {
                    format!(
                        "variant constructor omits field `{}` from exact shape `{}` tag {variant}",
                        field.name,
                        enum_ty.user_facing()
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let consumed = fields.iter().map(|field| field.value).collect::<Vec<_>>();
        let value = self.emit(
            expr,
            SemOpKind::VariantMake {
                shape,
                variant: u32::try_from(variant)
                    .map_err(|_| "variant constructor tag exceeds u32".to_string())?,
                fields,
            },
        )?;
        for field in consumed {
            self.owned_live.remove(&field);
        }
        Ok(value)
    }

    fn bind_selected_value(
        &mut self,
        binding: BindingId,
        name: &str,
        value: ValueId,
        span: Range<usize>,
    ) -> Result<(), String> {
        // Candidate names refer to the projected owner until every predicate
        // and guard has passed. A failed candidate must leave it available.
        let target = BindingTarget::Value(value);
        let declaration = self.source_bindings.len();
        self.source_bindings.push(Binding {
            id: crate::BindingId(
                u32::try_from(declaration).map_err(|_| "binding count exceeds u32")?,
            ),
            name: name.to_string(),
            span,
            mutable: false,
            target,
        });
        self.binding_declarations.insert(binding, declaration);
        self.bindings.insert(binding, target);
        self.declare_in_scope(binding);
        Ok(())
    }

    /// Rebuild one variant value from the payloads its switch handed out.
    fn emit_variant_make(
        &mut self,
        shape: VariantShapeId,
        variant: u32,
        ty: &ResolvedTy,
        fields: &[BlockArg],
    ) -> Result<ValueId, String> {
        let operands = fields
            .iter()
            .map(|field| Operand { value: field.value })
            .collect::<Vec<_>>();
        let value = self.emit_typed(
            Provenance::Synthesized,
            ty,
            SemOpKind::VariantMake {
                shape,
                variant,
                fields: operands,
            },
        )?;
        for field in fields {
            self.owned_live.remove(&field.value);
        }
        Ok(value)
    }

    /// Take a rebuilt variant value apart again, returning its fresh payloads.
    fn emit_variant_destructure(
        &mut self,
        shape: VariantShapeId,
        variant: u32,
        descriptor: &SemVariantShape,
        source: ValueId,
    ) -> Result<Vec<BlockArg>, String> {
        let declared = &descriptor
            .variants
            .get(usize::try_from(variant).map_err(|_| "variant tag exceeds usize".to_string())?)
            .ok_or_else(|| format!("variant tag {variant} is absent from its shape"))?
            .fields;
        let mut results = Vec::with_capacity(declared.len());
        for field in declared {
            results.push(ValueDef {
                id: self.fresh_value(),
                ty: field.ty.clone(),
                own: OwnKind::of_ty(&field.ty, self.service.checked_facts.rows())?,
            });
        }
        let id = OpId(self.ops);
        self.current_block_mut().append_op(SemOp {
            id,
            results: results.clone(),
            kind: SemOpKind::VariantDestructure {
                shape,
                variant,
                source: Operand { value: source },
            },
            provenance: Provenance::Synthesized,
        })?;
        self.ops += 1;
        self.owned_live.remove(&source);
        Ok(results
            .into_iter()
            .map(|field| {
                if field.own == OwnKind::Owned {
                    self.owned_live.insert(field.id, field.ty.clone());
                }
                BlockArg {
                    value: field.id,
                    ty: field.ty,
                    own: field.own,
                }
            })
            .collect())
    }

    fn destroy_live_since(
        &mut self,
        baseline: &BTreeMap<ValueId, ResolvedTy>,
    ) -> Result<(), String> {
        let values = self
            .owned_live
            .keys()
            .filter(|value| !baseline.contains_key(value))
            .copied()
            .collect::<Vec<_>>();
        for value in values.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        Ok(())
    }

    fn emit_variant_switch(
        &mut self,
        shape: VariantShapeId,
        descriptor: &SemVariantShape,
        scrutinee: ValueId,
    ) -> Result<Vec<VariantBranch>, String> {
        let mut semantic_arms = Vec::with_capacity(descriptor.variants.len());
        let mut branches = Vec::with_capacity(descriptor.variants.len());
        let mut inherited_live = self.owned_live.clone();
        inherited_live.remove(&scrutinee);
        // A loaned scrutinee is not destructured: its payloads are loans of the
        // same region, so they carry no release obligation and the switch
        // transfers nothing.
        let borrowed = self.value_own_kind(scrutinee) == Some(OwnKind::Guaranteed);
        for (variant_index, variant) in descriptor.variants.iter().enumerate() {
            let mut fields = Vec::with_capacity(variant.fields.len());
            let mut block_args = Vec::with_capacity(variant.fields.len());
            let mut edge_args = Vec::with_capacity(variant.fields.len());
            let mut branch_live = inherited_live.clone();
            for field in &variant.fields {
                self.service.require_type_facts(&field.ty)?;
                let mut own = OwnKind::of_ty(&field.ty, self.service.checked_facts.rows())?;
                if borrowed {
                    own = OwnKind::Guaranteed;
                }
                let field_value = self.fresh_value();
                fields.push(ValueDef {
                    id: field_value,
                    ty: field.ty.clone(),
                    own,
                });
                edge_args.push(Operand { value: field_value });
                let arg = BlockArg {
                    value: self.fresh_value(),
                    ty: field.ty.clone(),
                    own,
                };
                if own == OwnKind::Owned {
                    branch_live.insert(arg.value, arg.ty.clone());
                }
                block_args.push(arg);
            }
            let block = self.new_block(block_args.clone());
            let variant = u32::try_from(variant_index)
                .map_err(|_| "variant arm index exceeds u32".to_string())?;
            semantic_arms.push(SemVariantArm {
                variant,
                fields,
                target: Edge {
                    target: block,
                    args: edge_args,
                },
            });
            branches.push(VariantBranch {
                variant,
                block,
                fields: block_args,
                owned_live: branch_live,
            });
        }
        let id = OpId(self.ops);
        self.ops += 1;
        self.owned_live.remove(&scrutinee);
        self.set_terminator(SemTerminator::SwitchVariant {
            id,
            shape,
            scrutinee: Operand { value: scrutinee },
            arms: semantic_arms,
        })?;
        Ok(branches)
    }

    fn lower_string_equals_values(
        &mut self,
        lhs: ValueId,
        rhs: ValueId,
    ) -> Result<ValueId, String> {
        let raw = self.fresh_value();
        let continuation = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value: continuation,
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }]);
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::RtCall {
            id,
            family: hew_types::RuntimeCallFamily::StringEquals,
            args: vec![
                crate::BoundaryOperand {
                    operand: Operand { value: lhs },
                    decision: crate::BoundaryDecision::Borrow,
                },
                crate::BoundaryOperand {
                    operand: Operand { value: rhs },
                    decision: crate::BoundaryDecision::Borrow,
                },
            ],
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
            }),
            normal: Edge {
                target: normal,
                args: vec![Operand { value: raw }],
            },
            unwind: CallUnwind::NotApplicable,
        })?;
        self.current = normal;
        Ok(continuation)
    }

    fn branch_candidate_test(&mut self, condition: ValueId) -> Result<ControlState, String> {
        let pass = self.new_block(Vec::new());
        let fail = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition: Operand { value: condition },
            then_target: Edge {
                target: pass,
                args: Vec::new(),
            },
            else_target: Edge {
                target: fail,
                args: Vec::new(),
            },
        })?;
        let mut failure = self.control_state();
        failure.block = fail;
        self.current = pass;
        Ok(failure)
    }

    /// Give a selected binding a new declaration for its transferred owner.
    /// Failed-candidate control states still reference the original
    /// declaration, so it is never changed under those saved states.
    fn redeclare_binding(
        &mut self,
        binding: BindingId,
        target: BindingTarget,
    ) -> Result<(), String> {
        let mut selected = self.source_bindings[self.binding_declarations[&binding]].clone();
        let declaration = self.source_bindings.len();
        selected.id =
            crate::BindingId(u32::try_from(declaration).map_err(|_| "binding count exceeds u32")?);
        selected.target = target;
        self.source_bindings.push(selected);
        self.binding_declarations.insert(binding, declaration);
        self.bindings.insert(binding, target);
        Ok(())
    }

    fn lower_selected_body(
        &mut self,
        body: &HirExpr,
        result_ty: &ResolvedTy,
    ) -> Result<Option<Operand>, String> {
        if *result_ty == ResolvedTy::Unit {
            self.lower_discarded_expr(body)?;
            return Ok(None);
        }
        if matches!(self.ty(&body.ty), ResolvedTy::Unit | ResolvedTy::Never) {
            self.lower_discarded_expr(body)?;
            if self.is_open() {
                return Err(
                    "non-divergent variant arm does not produce the match result".to_string(),
                );
            }
            return Ok(None);
        }
        let value = if let HirExprKind::Block(block) = &body.kind {
            // A checked value tail may be unreachable after a return or fault.
            // Preserve the block's terminated control flow without inventing
            // an operand for a branch that never reaches the join.
            let result = self.lower_scoped_block(block, OwnedBindingUse::Copy)?;
            if !self.is_open() {
                return Ok(None);
            }
            result
                .ok_or("non-divergent selected block does not produce its result")?
                .value
        } else {
            lower_initial_value_transfer(self, body, "selected arm result", OwnedBindingUse::Copy)?
        };
        let value = self.coerce_value(value, result_ty, Provenance::Site(body.site))?;
        Ok(Some(Operand { value }))
    }

    fn merge_match_exits(
        &mut self,
        exits: Vec<MatchExit>,
        result_ty: &ResolvedTy,
    ) -> Result<Option<ValueId>, String> {
        if exits.is_empty() {
            return Ok(None);
        }
        let mut result_arg = None;
        let mut block_args = Vec::new();
        let mut edge_prefixes = vec![Vec::new(); exits.len()];
        if *result_ty != ResolvedTy::Unit {
            self.service.require_type_facts(result_ty)?;
            let own = OwnKind::of_ty(result_ty, self.service.checked_facts.rows())?;
            let joined = self.fresh_value();
            block_args.push(BlockArg {
                value: joined,
                ty: result_ty.clone(),
                own,
            });
            for (index, exit) in exits.iter().enumerate() {
                let result = exit
                    .result
                    .as_ref()
                    .ok_or_else(|| "non-divergent match arm has no result operand".to_string())?;
                edge_prefixes[index].push(result.clone());
            }
            result_arg = Some((joined, own));
        } else if exits.iter().any(|exit| exit.result.is_some()) {
            return Err("unit match arm unexpectedly carries a result".to_string());
        }

        let states = exits.into_iter().map(|exit| exit.state).collect::<Vec<_>>();
        self.join_control_states(states, block_args, edge_prefixes)?;
        if let Some((result, OwnKind::Owned)) = result_arg {
            self.owned_live.insert(result, result_ty.clone());
        }
        Ok(result_arg.map(|(result, _)| result))
    }

    fn lower_array_make(
        &mut self,
        expr: &HirExpr,
        elements: &[HirExpr],
    ) -> Result<ValueId, String> {
        let ResolvedTy::Array(element_ty, length) = self.ty(&expr.ty) else {
            return Err("fixed array literal has no exact array type".into());
        };
        if usize::try_from(length).ok() != Some(elements.len()) {
            return Err("fixed array literal length differs from its checked type".into());
        }
        let mut fields = Vec::with_capacity(elements.len());
        for element in elements {
            if self.ty(&element.ty) != *element_ty {
                return Err("fixed array literal element differs from its checked type".into());
            }
            self.service.require_type_facts(&element_ty)?;
            let binding_use =
                if self.service.checked_facts.rows()[&TypeInstanceKey((*element_ty).clone())].clone
                    == hew_types::CloneKind::None
                {
                    OwnedBindingUse::Move
                } else {
                    OwnedBindingUse::Copy
                };
            let value =
                lower_initial_value_transfer(self, element, "fixed array element", binding_use)?;
            fields.push(Operand { value });
        }
        let consumed = fields.iter().map(|field| field.value).collect::<Vec<_>>();
        let result = self.emit(expr, SemOpKind::ArrayMake { fields })?;
        for value in consumed {
            self.owned_live.remove(&value);
        }
        Ok(result)
    }

    fn lower_array_repeat(&mut self, expr: &HirExpr, seed: &HirExpr) -> Result<ValueId, String> {
        let ResolvedTy::Array(element_ty, length) = self.ty(&expr.ty) else {
            return Err("fixed array repeat has no exact array type".into());
        };
        if self.ty(&seed.ty) != *element_ty {
            return Err("fixed array repeat seed differs from its checked element type".into());
        }
        self.service.require_type_facts(&element_ty)?;
        let copy = self.service.checked_facts.rows()[&TypeInstanceKey((*element_ty).clone())].clone;
        if length > 1 && copy == hew_types::CloneKind::None {
            return Err("fixed array repeat requires Clone when its length exceeds one".into());
        }
        let value = lower_initial_value_transfer(
            self,
            seed,
            "fixed array repeat seed",
            if copy == hew_types::CloneKind::None {
                OwnedBindingUse::Move
            } else {
                OwnedBindingUse::Copy
            },
        )?;
        if length == 0 {
            if self.owned_live.contains_key(&value) {
                self.emit_destroy(value)?;
            }
            return self.emit(expr, SemOpKind::ArrayMake { fields: Vec::new() });
        }
        let result = self.emit(
            expr,
            SemOpKind::ArrayRepeat {
                value: Operand { value },
            },
        )?;
        self.owned_live.remove(&value);
        Ok(result)
    }

    /// Construct a tuple from exact semantic fields, including receiver transfers.
    fn lower_tuple_make(
        &mut self,
        expr: &HirExpr,
        elements: &[HirExpr],
    ) -> Result<ValueId, String> {
        let tuple_ty = self.ty(&expr.ty);
        if tuple_ty == ResolvedTy::Unit && elements.is_empty() {
            return self.emit(expr, SemOpKind::ConstUnit);
        }
        let ResolvedTy::Tuple(expected_elements) = &tuple_ty else {
            return Err(format!(
                "tuple literal has non-tuple resolved type `{}` in SIR lowering",
                tuple_ty.user_facing()
            ));
        };
        if expected_elements.len() != elements.len() {
            return Err(format!(
                "tuple literal has {} element(s), but its resolved type `{}` has {} element type(s)",
                elements.len(),
                tuple_ty.user_facing(),
                expected_elements.len()
            ));
        }
        // The marked receiver occupies the writeback field of the established
        // dual return. The method result may itself mention Self; it keeps the
        // ordinary source copy policy and must not take the writeback receiver.
        let receiver_return = tuple_ty == self.ty(&self.function.return_ty)
            && self.function.var_self_receiver.is_some_and(|binding| {
                elements.get(1).is_some_and(|element| {
                    element.intent == IntentKind::Consume
                        && matches!(element.kind, HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. }
                            if id == binding)
                })
            });
        let mut lowered_elements = Vec::with_capacity(elements.len());
        for (index, (element, expected_ty)) in elements.iter().zip(expected_elements).enumerate() {
            let actual_ty = self.ty(&element.ty);
            if &actual_ty != expected_ty {
                return Err(format!(
                    "tuple literal element {index} has resolved type `{}`, expected `{}`",
                    actual_ty.user_facing(),
                    expected_ty.user_facing()
                ));
            }
            let value = if is_initial_value_type(&tuple_ty) && !receiver_return {
                self.lower_read_operand(element, &format!("tuple literal element {index}"))?
            } else {
                Operand {
                    value: lower_initial_value_transfer(
                        self,
                        element,
                        &format!("owned tuple field {index}"),
                        if receiver_return && index == 1 {
                            OwnedBindingUse::Move
                        } else {
                            OwnedBindingUse::Copy
                        },
                    )?,
                }
            };
            lowered_elements.push(value);
        }
        if is_initial_value_type(&tuple_ty) {
            self.emit(
                expr,
                SemOpKind::TupleMake {
                    elements: lowered_elements,
                },
            )
        } else {
            let shape = self.service.require_aggregate_shape(&tuple_ty)?;
            let consumed = lowered_elements
                .iter()
                .map(|field| field.value)
                .collect::<Vec<_>>();
            let aggregate = self.emit(
                expr,
                SemOpKind::AggregateMake {
                    shape,
                    fields: lowered_elements,
                },
            )?;
            for field in consumed {
                self.owned_live.remove(&field);
            }
            Ok(aggregate)
        }
    }

    fn tuple_projection_index(
        &mut self,
        expr: &HirExpr,
        tuple_expr: &HirExpr,
        index: usize,
    ) -> Result<u32, String> {
        let tuple_ty = self.ty(&tuple_expr.ty);
        let ResolvedTy::Tuple(elements) = &tuple_ty else {
            return Err(format!(
                "tuple projection has non-tuple operand type `{}` in SIR lowering",
                tuple_ty.user_facing()
            ));
        };
        let expected_ty = elements.get(index).ok_or_else(|| {
            format!(
                "tuple projection index {index} is out of bounds for `{}` with {} element(s)",
                tuple_ty.user_facing(),
                elements.len()
            )
        })?;
        let result_ty = self.ty(&expr.ty);
        if &result_ty != expected_ty {
            return Err(format!(
                "tuple projection index {index} from `{}` has result type `{}`, expected `{}`",
                tuple_ty.user_facing(),
                result_ty.user_facing(),
                expected_ty.user_facing()
            ));
        }
        let index = u32::try_from(index).map_err(|_| {
            "tuple projection index exceeds SIR's target-independent u32 field range".to_string()
        })?;
        Ok(index)
    }

    /// Lower a semantic tuple projection without exposing aggregate layout.
    fn lower_tuple_get(
        &mut self,
        expr: &HirExpr,
        tuple_expr: &HirExpr,
        index: usize,
    ) -> Result<ValueId, String> {
        if let Some(place) = self.expression_projection(expr)? {
            return self.emit(expr, SemOpKind::LoadCopy { place });
        }
        let index = self.tuple_projection_index(expr, tuple_expr, index)?;
        let tuple_ty = self.ty(&tuple_expr.ty);
        let tuple = self.lower_read_operand(tuple_expr, "tuple projection operand")?;
        if is_initial_value_type(&tuple_ty) {
            self.emit(expr, SemOpKind::TupleGet { tuple, index })
        } else {
            let shape = self.service.require_aggregate_shape(&tuple_ty)?;
            self.emit(
                expr,
                SemOpKind::AggregateProjectCopy {
                    shape,
                    aggregate: tuple,
                    field: index,
                },
            )
        }
    }

    fn lower_initial_tuple_destructure(
        &mut self,
        value: &HirExpr,
        fields: &[HirDestructureField],
    ) -> Result<(), String> {
        let aggregate_ty = self.ty(&value.ty);
        let ResolvedTy::Tuple(field_tys) = &aggregate_ty else {
            return Err(format!(
                "irrefutable destructure has non-aggregate type `{}`",
                aggregate_ty.user_facing()
            ));
        };
        if fields.len() != field_tys.len() {
            return Err(format!(
                "tuple destructure for `{}` binds {} field(s), expected {}",
                aggregate_ty.user_facing(),
                fields.len(),
                field_tys.len()
            ));
        }
        require_initial_value_transfer(value.intent, &aggregate_ty, "tuple destructure source")?;
        let tuple = self.lower_expr(value)?;
        for (index, (field, expected_ty)) in fields.iter().zip(field_tys).enumerate() {
            let index = u32::try_from(index)
                .map_err(|_| "tuple destructure index exceeds u32".to_string())?;
            let expected_selector = HirDestructureSelector::Tuple(index);
            let binding_ty = self.ty(&field.binding.ty);
            if field.selector != expected_selector || binding_ty != *expected_ty {
                return Err(format!(
                    "tuple destructure field {index} has selector {:?} and type `{}`, expected {:?} and `{}`",
                    field.selector,
                    binding_ty.user_facing(),
                    expected_selector,
                    expected_ty.user_facing()
                ));
            }
            let result = self.emit_typed(
                Provenance::Site(value.site),
                expected_ty,
                SemOpKind::TupleGet {
                    tuple: Operand { value: tuple },
                    index,
                },
            )?;
            self.bind_source_value(&field.binding, result)?;
        }
        Ok(())
    }

    /// Lower one checker-normalized irrefutable aggregate pattern.
    ///
    /// Owned source bindings are copied as whole values before the consuming
    /// operation. The destructure itself then transfers every ordered field
    /// into a distinct SSA result, including compiler-created wildcard
    /// bindings, so cleanup remains explicit on every path.
    fn lower_destructure(
        &mut self,
        value: &HirExpr,
        fields: &[HirDestructureField],
    ) -> Result<(), String> {
        let aggregate_ty = self.ty(&value.ty);
        if is_initial_value_type(&aggregate_ty) {
            return self.lower_initial_tuple_destructure(value, fields);
        }

        let shape = self.service.require_aggregate_shape(&aggregate_ty)?;
        let recipes = crate::aggregate_field_recipes(
            shape,
            &aggregate_ty,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        let expected_selectors = match shape {
            AggregateShapeRef::Tuple => (0..recipes.len())
                .map(|index| {
                    u32::try_from(index)
                        .map(HirDestructureSelector::Tuple)
                        .map_err(|_| "tuple destructure index exceeds u32".to_string())
                })
                .collect::<Result<Vec<_>, _>>()?,
            AggregateShapeRef::Record(id) => self
                .service
                .aggregate_shapes
                .get(usize::try_from(id.0).map_err(|_| "aggregate shape id exceeds usize")?)
                .ok_or_else(|| format!("aggregate shape {} disappeared during lowering", id.0))?
                .fields
                .iter()
                .map(|field| HirDestructureSelector::Record(field.name.clone()))
                .collect(),
        };
        if fields.len() != recipes.len() {
            return Err(format!(
                "aggregate destructure for `{}` binds {} field(s), expected {}",
                aggregate_ty.user_facing(),
                fields.len(),
                recipes.len()
            ));
        }
        for (index, ((field, recipe), expected_selector)) in fields
            .iter()
            .zip(&recipes)
            .zip(&expected_selectors)
            .enumerate()
        {
            let binding_ty = self.ty(&field.binding.ty);
            if &field.selector != expected_selector || binding_ty != recipe.ty {
                return Err(format!(
                    "aggregate destructure field {index} has selector {:?} and type `{}`, expected {:?} and `{}`",
                    field.selector,
                    binding_ty.user_facing(),
                    expected_selector,
                    recipe.ty.user_facing()
                ));
            }
        }

        let aggregate = lower_initial_value_transfer(
            self,
            value,
            "aggregate destructure source",
            OwnedBindingUse::Copy,
        )?;
        if self.value_own_kind(aggregate).is_none() {
            return Err(format!(
                "aggregate destructure source `{}` has no exact ownership facts",
                aggregate_ty.user_facing()
            ));
        }
        let results = self.emit_destructure_value(
            aggregate,
            &aggregate_ty,
            shape,
            Provenance::Site(value.site),
        )?;
        for (field, result) in fields.iter().zip(results) {
            self.bind_source_value(&field.binding, result.id)?;
        }
        Ok(())
    }

    fn emit_destructure_value(
        &mut self,
        aggregate: ValueId,
        ty: &ResolvedTy,
        shape: AggregateShapeRef,
        provenance: Provenance,
    ) -> Result<Vec<ValueDef>, String> {
        let recipes = crate::aggregate_field_recipes(
            shape,
            ty,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        let mut results = Vec::with_capacity(recipes.len());
        for recipe in &recipes {
            self.service.require_type_facts(&recipe.ty)?;
            results.push(ValueDef {
                id: self.fresh_value(),
                ty: recipe.ty.clone(),
                own: recipe.own,
            });
        }
        let operation = SemOp {
            id: OpId(self.ops),
            results: results.clone(),
            kind: SemOpKind::Destructure {
                shape,
                aggregate: Operand { value: aggregate },
            },
            provenance,
        };
        self.current_block_mut().append_op(operation)?;
        self.ops += 1;
        self.owned_live.remove(&aggregate);
        for result in &results {
            if result.own == OwnKind::Owned {
                self.owned_live.insert(result.id, result.ty.clone());
            }
        }
        Ok(results)
    }

    /// Lower one named aggregate construction in source evaluation order,
    /// then present its operands in the declaration's exact field order.
    ///
    /// A functional update `R { x: v, ..base }` evaluates its named fields
    /// first and fills the rest from the base afterwards. A base that is
    /// consumed - a temporary, or a value with a non-copyable carried field -
    /// is destructured so the carried fields transfer and the overridden ones
    /// are destroyed here. Any other base is only read: every carried field
    /// is an independent copy and the base stays live for its owner.
    fn lower_aggregate_make(
        &mut self,
        expr: &HirExpr,
        fields: &[(String, HirExpr)],
        base: Option<&HirExpr>,
    ) -> Result<ValueId, String> {
        let aggregate_ty = self.ty(&expr.ty);
        let shape = self.service.require_aggregate_shape(&aggregate_ty)?;
        let AggregateShapeRef::Record(id) = shape else {
            return Err("struct initializer resolved to a non-record aggregate shape".to_string());
        };
        let declared_fields = self
            .service
            .aggregate_shapes
            .get(usize::try_from(id.0).map_err(|_| "aggregate shape id exceeds usize")?)
            .ok_or_else(|| format!("aggregate shape {} disappeared during lowering", id.0))?
            .fields
            .clone();
        let mut ordered = vec![None; declared_fields.len()];
        for (name, field) in fields {
            let index = declared_fields
                .iter()
                .position(|declared| declared.name == *name)
                .ok_or_else(|| {
                    format!(
                        "record initializer field `{name}` is absent from exact shape `{}`",
                        aggregate_ty.user_facing()
                    )
                })?;
            if ordered[index].is_some() {
                return Err(format!("record initializer repeats field `{name}`"));
            }
            let value = lower_initial_value_transfer(
                self,
                field,
                &format!("owned record field `{name}`"),
                OwnedBindingUse::Copy,
            )?;
            ordered[index] = Some(Operand {
                value: self.coerce_value(
                    value,
                    &declared_fields[index].ty,
                    Provenance::Site(field.site),
                )?,
            });
        }
        if let Some(base) = base {
            self.lower_aggregate_update_base(base, &aggregate_ty, shape, &mut ordered)?;
        }
        let fields = ordered
            .into_iter()
            .zip(&declared_fields)
            .map(|(operand, declared)| {
                operand.ok_or_else(|| {
                    format!(
                        "record initializer omits field `{}` from exact shape `{}`",
                        declared.name,
                        aggregate_ty.user_facing()
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let consumed = fields.iter().map(|field| field.value).collect::<Vec<_>>();
        let aggregate = self.emit(expr, SemOpKind::AggregateMake { shape, fields })?;
        for field in consumed {
            self.owned_live.remove(&field);
        }
        Ok(aggregate)
    }

    /// Fill every field the update leaves unnamed from its base.
    fn lower_aggregate_update_base(
        &mut self,
        base: &HirExpr,
        aggregate_ty: &ResolvedTy,
        shape: AggregateShapeRef,
        ordered: &mut [Option<Operand>],
    ) -> Result<(), String> {
        if self.ty(&base.ty) != *aggregate_ty {
            return Err(format!(
                "functional update base `{}` differs from the constructed `{}`",
                self.ty(&base.ty).user_facing(),
                aggregate_ty.user_facing()
            ));
        }
        let recipes = crate::aggregate_field_recipes(
            shape,
            aggregate_ty,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        let carried = ordered.iter().map(Option::is_none).collect::<Vec<_>>();
        let mut transfer_only = false;
        for (index, recipe) in recipes.iter().enumerate() {
            if !carried[index] {
                continue;
            }
            self.service.require_type_facts(&recipe.ty)?;
            if recipe.own == OwnKind::Owned
                && self.service.checked_facts.rows()[&TypeInstanceKey(recipe.ty.clone())].clone
                    == hew_types::CloneKind::None
            {
                transfer_only = true;
            }
        }
        let provenance = Provenance::Site(base.site);
        let live_before: std::collections::HashSet<_> = self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let source = if transfer_only {
            let value = lower_initial_value_transfer(
                self,
                base,
                "functional update base",
                OwnedBindingUse::Move,
            )?;
            if self.value_own_kind(value) != Some(OwnKind::Owned) {
                return Err(
                    "E_OWN_CONSUME_BORROWED: functional update of a borrowed base cannot transfer its non-copyable fields"
                        .into(),
                );
            }
            value
        } else {
            self.lower_borrowed_read(base, &mut loans)?.value
        };
        let consumed = self.owned_live.contains_key(&source) && !live_before.contains(&source);
        if consumed {
            let results = self.emit_destructure_value(source, aggregate_ty, shape, provenance)?;
            for (index, result) in results.into_iter().enumerate() {
                if carried[index] {
                    ordered[index] = Some(Operand { value: result.id });
                } else if result.own == OwnKind::Owned {
                    self.emit_destroy(result.id)?;
                }
            }
        } else {
            for (index, recipe) in recipes.iter().enumerate() {
                if !carried[index] {
                    continue;
                }
                let field = u32::try_from(index).map_err(|_| "aggregate field exceeds u32")?;
                let value = self.emit_typed(
                    provenance.clone(),
                    &recipe.ty,
                    SemOpKind::AggregateProjectCopy {
                        shape,
                        aggregate: Operand { value: source },
                        field,
                    },
                )?;
                ordered[index] = Some(Operand { value });
            }
        }
        self.end_call_loans(&loans)
    }

    /// Resolve a named projection once for both owned and borrowed reads.
    fn aggregate_projection_shape(
        &mut self,
        expr: &HirExpr,
        object: &HirExpr,
        field: &str,
    ) -> Result<(AggregateShapeRef, u32), String> {
        let aggregate_ty = self.ty(&object.ty);
        let shape = self.service.require_aggregate_shape(&aggregate_ty)?;
        let AggregateShapeRef::Record(id) = shape else {
            return Err("named field access resolved to a non-record aggregate shape".to_string());
        };
        let descriptor = self
            .service
            .aggregate_shapes
            .get(usize::try_from(id.0).map_err(|_| "aggregate shape id exceeds usize")?)
            .ok_or_else(|| format!("aggregate shape {} disappeared during lowering", id.0))?;
        let index = descriptor
            .fields
            .iter()
            .position(|candidate| candidate.name == field)
            .ok_or_else(|| {
                format!(
                    "field `{field}` is absent from exact aggregate shape `{}`",
                    aggregate_ty.user_facing()
                )
            })?;
        let expected_ty = descriptor.fields[index].ty.clone();
        let result_ty = self.ty(&expr.ty);
        if !crate::call_boundary_types_match(&result_ty, &expected_ty) {
            return Err(format!(
                "field `{field}` from `{}` has `{}`, expected `{}`",
                aggregate_ty.user_facing(),
                result_ty.user_facing(),
                expected_ty.user_facing()
            ));
        }
        Ok((
            shape,
            u32::try_from(index).map_err(|_| "aggregate field index exceeds u32")?,
        ))
    }

    /// Lower an ordinary named-field read as an explicit independent copy.
    fn lower_aggregate_project(
        &mut self,
        expr: &HirExpr,
        object: &HirExpr,
        field: &str,
    ) -> Result<ValueId, String> {
        if let Some(place) = self.expression_projection(expr)? {
            return self.emit(expr, SemOpKind::LoadCopy { place });
        }
        let (shape, field) = self.aggregate_projection_shape(expr, object, field)?;
        let mut loans = Vec::new();
        let aggregate = self.lower_borrowed_read(object, &mut loans)?;
        let value = self.emit(
            expr,
            SemOpKind::AggregateProjectCopy {
                shape,
                aggregate,
                field,
            },
        )?;
        self.end_call_loans(&loans)?;
        Ok(value)
    }

    /// These expressions cannot consume a prior argument's owner or branch to
    /// cleanup while a call-local loan is open. Other evaluation requires a
    /// snapshot of copyable arguments or a protected loan of affine arguments.
    fn stable_argument_read(expr: &HirExpr) -> bool {
        match &expr.kind {
            HirExprKind::Literal(_) | HirExprKind::BindingRef { .. } => true,
            HirExprKind::FieldAccess { object, .. } => Self::stable_argument_read(object),
            HirExprKind::TupleIndex { tuple, .. } => Self::stable_argument_read(tuple),
            HirExprKind::SubsumedValue { source, .. } => Self::stable_argument_read(source),
            _ => false,
        }
    }

    /// A call-local projection keeps its immediate parent live. Recursing over
    /// a field chain therefore protects its root without copying intermediate
    /// owning records. Whole-value operands already have the call's borrow
    /// boundary and do not need an additional projection loan.
    #[expect(
        clippy::too_many_lines,
        reason = "one borrow boundary covers bindings, projections and computed supervisor roles"
    )]
    fn lower_borrowed_read(
        &mut self,
        expr: &HirExpr,
        loans: &mut Vec<ValueId>,
    ) -> Result<Operand, String> {
        // Declared child access computes a stable role; it does not borrow a
        // field from the supervisor handle's physical representation.
        if self
            .service
            .module
            .supervisor_child_slots
            .contains_key(&expr.site)
        {
            return self.lower_expr(expr).map(|value| Operand { value });
        }
        if let Some(place) = self.expression_projection(expr)? {
            let owning = OwnKind::of_ty(&self.ty(&expr.ty), self.service.checked_facts.rows())?
                == OwnKind::Owned;
            let kind = if owning {
                SemOpKind::LoadBorrow { place }
            } else {
                SemOpKind::LoadCopy { place }
            };
            let value = self.emit(expr, kind)?;
            if owning {
                loans.push(value);
            }
            return Ok(Operand { value });
        }
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &expr.kind
        {
            if let Some((place, field)) = self.capture_field(*binding) {
                if OwnKind::of_ty(&field.ty, self.service.checked_facts.rows())? == OwnKind::Owned {
                    let value = self.emit_typed(
                        Provenance::Site(expr.site),
                        &field.ty,
                        SemOpKind::LoadBorrow { place },
                    )?;
                    loans.push(value);
                    return Ok(Operand { value });
                }
                return Ok(Operand {
                    value: self.load_capture(*binding, Provenance::Site(expr.site), false)?,
                });
            }
            if let BindingTarget::Value(value) = self.binding_target(*binding)? {
                if self.ended_loans.contains(&value) {
                    return Err("E_OWN_CONSUME_BORROWED: call argument names an ended loan".into());
                }
                return Ok(Operand { value });
            }
        }
        let (object, shape, field) = match &expr.kind {
            HirExprKind::FieldAccess { object, field } => {
                require_initial_scalar_read(expr.intent)?;
                let (shape, field) = self.aggregate_projection_shape(expr, object, field)?;
                (object.as_ref(), shape, field)
            }
            HirExprKind::TupleIndex { tuple, index } => {
                require_initial_scalar_read(expr.intent)?;
                let field = self.tuple_projection_index(expr, tuple, *index)?;
                let ty = self.ty(&tuple.ty);
                if is_initial_value_type(&ty) {
                    return self.lower_read_operand(expr, "borrowed call argument");
                }
                (
                    tuple.as_ref(),
                    self.service.require_aggregate_shape(&ty)?,
                    field,
                )
            }
            HirExprKind::SubsumedValue { source, .. } => {
                if self.ty(&source.ty) != self.ty(&expr.ty) {
                    return Err("transparent borrowed value must preserve its exact type".into());
                }
                return self.lower_borrowed_read(source, loans);
            }
            _ if expr.intent == IntentKind::Read => {
                return self.lower_read_operand(expr, "borrowed call argument")
            }
            _ => {
                return Ok(Operand {
                    value: lower_initial_value_transfer(
                        self,
                        expr,
                        "borrowed call argument",
                        OwnedBindingUse::Copy,
                    )?,
                })
            }
        };
        let aggregate = self.lower_borrowed_read(object, loans)?;
        let owning = OwnKind::of_ty(&self.ty(&expr.ty), self.service.checked_facts.rows())?
            == OwnKind::Owned;
        let kind = if owning {
            SemOpKind::AggregateProjectBorrow {
                shape,
                aggregate,
                field,
            }
        } else {
            SemOpKind::AggregateProjectCopy {
                shape,
                aggregate,
                field,
            }
        };
        let value = self.emit(expr, kind)?;
        if owning {
            loans.push(value);
        }
        Ok(Operand { value })
    }

    /// End the loans a `let` binding holds on `root` before the collection
    /// they borrow is taken. The take is past the binding's last use, so the
    /// loan ends here; a read of the binding afterwards is refused by name.
    fn end_binding_loans_on(&mut self, root: crate::OwnerRoot) -> Result<(), String> {
        let mut ending = Vec::new();
        for group in &self.binding_loans {
            if group.root != root {
                continue;
            }
            // A loan its scope already ended is not this take's concern.
            let live: Vec<ValueId> = group
                .loans
                .iter()
                .copied()
                .filter(|loan| !self.ended_loans.contains(loan) && self.scope_loans.contains(loan))
                .collect();
            if live.is_empty() {
                continue;
            }
            // Ending it inside a branch or a loop would leave it live on the
            // sibling path, so a take there meets the loan instead.
            if group.loop_depth != self.loops.len() || group.branch_depth != self.branch_depth {
                return Err(
                    "E_OWN_CONSUME_BORROWED: this collection is borrowed by a live element \
                     loan; the loop or read holding it must end before the collection is \
                     mutated or drained"
                        .to_string(),
                );
            }
            ending.extend(live);
        }
        if ending.is_empty() {
            return Ok(());
        }
        self.end_call_loans(&ending)?;
        self.ended_loans.extend(ending);
        Ok(())
    }

    fn end_call_loans(&mut self, loans: &[ValueId]) -> Result<(), String> {
        for &value in loans.iter().rev() {
            // A binding's loan may have ended at its last use already.
            if self.ended_loans.contains(&value) {
                continue;
            }
            let op = SemOp {
                id: OpId(self.ops),
                results: Vec::new(),
                kind: SemOpKind::EndBorrow {
                    borrow: Operand { value },
                },
                provenance: Provenance::Synthesized,
            };
            self.current_block_mut().append_op(op)?;
            self.ops += 1;
        }
        Ok(())
    }

    /// Capture an earlier value before a later argument can replace its owner.
    /// An affine argument cannot be snapshotted: its loan remains live during
    /// later evaluation, which must not consume or replace the borrowed owner.
    fn lower_call_read(
        &mut self,
        expr: &HirExpr,
        loans: &mut Vec<ValueId>,
        later_arguments_are_stable: bool,
        can_borrow_projection: bool,
    ) -> Result<Operand, String> {
        let scope_loan_floor = self.scope_loans.len();
        let call_loan_floor = loans.len();
        let ty = self.ty(&expr.ty);
        self.service.require_type_facts(&ty)?;
        let affine = self.service.checked_facts.rows()[&TypeInstanceKey(ty)].clone
            == hew_types::CloneKind::None;
        let operand = if !later_arguments_are_stable && !affine {
            Ok(Operand {
                value: lower_initial_value_transfer(
                    self,
                    expr,
                    "call argument snapshot",
                    OwnedBindingUse::Copy,
                )?,
            })
        } else if can_borrow_projection || affine {
            let mut operand = self.lower_borrowed_read(expr, loans)?;
            if affine && self.value_own_kind(operand.value) == Some(OwnKind::Owned) {
                operand.value = self.emit(
                    expr,
                    SemOpKind::BeginBorrow {
                        owner: operand.clone(),
                    },
                )?;
                loans.push(operand.value);
            }
            Ok(operand)
        } else {
            self.lower_read_operand(expr, "call argument")
        }?;
        // Interior loans created while evaluating an argument belong to this
        // call. An independent return cannot keep them alive in the caller's
        // lexical scope; a borrowed runtime result explicitly promotes the
        // call's loans when it needs them.
        if self.scope_loans.len() > scope_loan_floor {
            let interior = self.scope_loans.split_off(scope_loan_floor);
            // New projected field loans depend on these interior parents.
            loans.splice(call_loan_floor..call_loan_floor, interior);
        }
        Ok(operand)
    }

    fn lower_value_equality(
        &mut self,
        expr: &HirExpr,
        args: [&HirExpr; 2],
    ) -> Result<ValueId, String> {
        let ty = self.ty(&args[0].ty);
        if self.ty(&args[1].ty) != ty || self.ty(&expr.ty) != ResolvedTy::Bool {
            return Err("selected equality requires matching operands and a bool result".into());
        }
        self.service
            .require_value_capability(&ty, hew_types::ValueCapability::Eq)?;
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let mut lowered_args = Vec::new();
        let argument_loan_depth = self.argument_receiver_loans.len();
        for (index, arg) in args.iter().enumerate() {
            let loan_floor = loans.len();
            let stable_tail = args[index + 1..]
                .iter()
                .all(|arg| Self::stable_argument_read(arg));
            let operand = self.lower_call_read(arg, &mut loans, stable_tail, true)?;
            lowered_args.push(crate::BoundaryOperand {
                operand,
                decision: crate::BoundaryDecision::Borrow,
            });
            self.argument_receiver_loans
                .extend_from_slice(&loans[loan_floor..]);
        }
        self.argument_receiver_loans.truncate(argument_loan_depth);
        let live_at_call = self.owned_live.clone();
        let argument_temporaries: Vec<_> = live_at_call
            .keys()
            .filter(|value| !live_before_arguments.contains(value))
            .copied()
            .collect();
        let raw = self.fresh_value();
        let continuation = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value: continuation,
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }]);
        let unwind = self.new_block(Vec::new());
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::ValueCall {
            id,
            ty,
            capability: hew_types::ValueCapability::Eq,
            args: lowered_args,
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
            }),
            normal: Edge {
                target: normal,
                args: vec![Operand { value: raw }],
            },
            unwind: CallUnwind::Cleanup(Edge {
                target: unwind,
                args: Vec::new(),
            }),
        })?;
        self.current = unwind;
        self.owned_live = live_at_call.clone();
        self.end_call_loans(&loans)?;
        self.finish_fault_exit()?;
        self.current = normal;
        self.owned_live = live_at_call;
        self.end_call_loans(&loans)?;
        for value in argument_temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        Ok(continuation)
    }

    /// A projected consume needs a verified transfer of the selected field.
    /// Check before evaluating the receiver: ordinary projection lowering is a
    /// copy and must never manufacture an owner for a consuming field call.
    fn reject_projected_callable_consume(&self, callee: &HirExpr) -> Result<(), String> {
        let mut root = callee;
        let mut projected = false;
        loop {
            root = match &root.kind {
                HirExprKind::SubsumedValue { source } => source,
                HirExprKind::FieldAccess { object, .. } => {
                    projected = true;
                    object
                }
                HirExprKind::TupleIndex { tuple, .. } => {
                    projected = true;
                    tuple
                }
                _ => break,
            };
        }
        if !projected {
            return Ok(());
        }
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &root.kind
        {
            if self
                .bindings
                .get(binding)
                .is_some_and(|target| matches!(target, BindingTarget::Value(value) if self.value_own_kind(*value) == Some(OwnKind::Guaranteed)))
            {
                return Err("E_OWN_CONSUME_BORROWED: a borrowed aggregate field cannot be consumed; acquire an owned aggregate and destructure it first".into());
            }
        }
        Err("E_OWN_PARTIAL_CONSUME: a live aggregate field cannot be consumed; destructure the aggregate into owning bindings before calling the once field".into())
    }

    /// The state seat keeps every field for the actor's lifetime, so a field
    /// consumed by value leaves as a copy. A field without a copy cannot leave.
    fn state_field_leaves_as_copy(
        &mut self,
        place: PlaceId,
        expression: &HirExpr,
    ) -> Result<bool, String> {
        if !matches!(
            self.places[place.0 as usize].origin,
            crate::PlaceOrigin::ActorState { .. }
        ) {
            return Ok(false);
        }
        let ty = self.ty(&expression.ty);
        self.service.require_type_facts(&ty)?;
        if self.service.checked_facts.rows()[&TypeInstanceKey(ty)].clone
            == hew_types::CloneKind::None
        {
            return Err("an actor state field without a copy cannot leave the state seat".into());
        }
        Ok(true)
    }

    /// Taking a field from an owned temporary transfers its siblings into the
    /// existing cleanup relation. No temporary container remains to own them.
    fn lower_consuming_projection(
        &mut self,
        expression: &HirExpr,
    ) -> Result<Option<ValueId>, String> {
        if let Some(place) = self.expression_projection(expression)? {
            let kind = if self.state_field_leaves_as_copy(place, expression)? {
                SemOpKind::LoadCopy { place }
            } else {
                SemOpKind::LoadTake { place }
            };
            return self.emit(expression, kind).map(Some);
        }
        let mut root = expression;
        let mut projections = Vec::new();
        loop {
            let (object, shape, field) = match &root.kind {
                HirExprKind::SubsumedValue { source } => {
                    root = source;
                    continue;
                }
                HirExprKind::FieldAccess { object, field } => {
                    let (shape, field) = self.aggregate_projection_shape(root, object, field)?;
                    (object.as_ref(), shape, field)
                }
                HirExprKind::TupleIndex { tuple, index } => {
                    let field = self.tuple_projection_index(root, tuple, *index)?;
                    let shape = self.service.require_aggregate_shape(&self.ty(&tuple.ty))?;
                    (tuple.as_ref(), shape, field)
                }
                _ => break,
            };
            projections.push((self.ty(&object.ty), shape, field));
            root = object;
        }
        if projections.is_empty() {
            return Ok(None);
        }
        if matches!(root.kind, HirExprKind::BindingRef { .. }) {
            // Local roots need persistent projected-place availability.
            self.reject_projected_callable_consume(expression)?;
        }
        let mut value = self.lower_expr_with_binding_use(root, OwnedBindingUse::Move)?;
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Err(
                "E_OWN_CONSUME_BORROWED: projected consumption requires an owned aggregate".into(),
            );
        }
        for (ty, shape, field) in projections.into_iter().rev() {
            let fields =
                self.emit_destructure_value(value, &ty, shape, Provenance::Site(expression.site))?;
            value = fields[usize::try_from(field).map_err(|_| "aggregate field exceeds usize")?].id;
        }
        Ok(Some(value))
    }

    /// Transfer a receiver or argument before evaluating later arguments. Its
    /// new owner remains live for argument-failure cleanup until the call starts.
    fn lower_consuming_value(&mut self, argument: &HirExpr) -> Result<ValueId, String> {
        self.require_consuming_capture(argument)?;
        let mut source = argument;
        while let HirExprKind::SubsumedValue { source: inner } = &source.kind {
            source = inner;
        }
        let value = match self.lower_consuming_projection(source)? {
            Some(value) => value,
            None => self.lower_expr_with_binding_use(source, OwnedBindingUse::Move)?,
        };
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Err("E_OWN_CONSUME_BORROWED: a consuming argument requires an owned value; declare the forwarding parameter consume".into());
        }
        self.owned_live.remove(&value);
        self.emit(
            argument,
            SemOpKind::Move {
                source: Operand { value },
            },
        )
    }

    /// Lower one adopted runtime operand that still has a copy recipe into the
    /// independent owner the operation takes. The binding is read as a copy, so
    /// the caller keeps its own value; that copy transfers rather than being
    /// cloned again inside the operation and destroyed on the normal edge.
    fn lower_adopted_copy(&mut self, argument: &HirExpr) -> Result<ValueId, String> {
        let value = lower_initial_value_transfer(
            self,
            argument,
            "runtime operand adoption",
            OwnedBindingUse::Copy,
        )?;
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Err("an adopted runtime operand requires an owned value".into());
        }
        self.owned_live.remove(&value);
        self.emit(
            argument,
            SemOpKind::Move {
                source: Operand { value },
            },
        )
    }

    fn require_consuming_capture(&self, expression: &HirExpr) -> Result<(), String> {
        let mut source = expression;
        while let HirExprKind::SubsumedValue { source: inner } = &source.kind {
            source = inner;
        }
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &source.kind
        {
            if let Some((_, field)) = self.capture_field(*binding) {
                if field.consumption != hew_types::ClosureCaptureConsumption::Consumed {
                    return Err("E_OWN_CONSUME_BORROWED: consuming a captured argument requires an owning capture transfer".into());
                }
            }
        }
        Ok(())
    }

    /// Erase one owned concrete value into a trait object.
    fn lower_dyn_make(
        &mut self,
        expr: &HirExpr,
        value: &HirExpr,
        concrete_type: &ResolvedTy,
        entries: &[hew_types::DynVtableEntry],
    ) -> Result<ValueId, String> {
        let dyn_ty = self.ty(&expr.ty);
        let concrete_ty = self.ty(concrete_type);
        let vtable = self
            .service
            .request_vtable(&dyn_ty, &concrete_ty, entries)?;
        if self.ty(&value.ty) != concrete_ty {
            return Err(format!(
                "erasure input `{}` differs from the checker's concrete type `{}`",
                self.ty(&value.ty).user_facing(),
                concrete_ty.user_facing()
            ));
        }
        // A bit-copy concrete value carries no obligation to transfer; the
        // box holds its bits and the table's drop slot has nothing to run.
        let source =
            if OwnKind::of_ty(&concrete_ty, self.service.checked_facts.rows())? == OwnKind::Owned {
                let source = self.lower_consuming_value(value)?;
                self.owned_live.remove(&source);
                source
            } else {
                self.lower_expr(value)?
            };
        self.emit(
            expr,
            SemOpKind::DynMake {
                vtable,
                value: Operand { value: source },
            },
        )
    }

    /// The erased dispatch boundary for one trait method.
    ///
    /// Parameter transfer follows the same rule the implementations were
    /// admitted under, so the verifier can compare this boundary against every
    /// table that erases into the receiver's trait object.
    fn dyn_dispatch_signature(
        &mut self,
        args: &[HirExpr],
        return_ty: &ResolvedTy,
    ) -> Result<SemSignature, String> {
        let mut params = Vec::with_capacity(args.len());
        for arg in args {
            let ty = self.ty(&arg.ty);
            self.service.require_type_facts(&ty)?;
            let own = OwnKind::of_ty(&ty, self.service.checked_facts.rows())?;
            params.push(SemAbiParam {
                passing: dyn_boundary_passing(own),
                ty,
                caller_visible_projection: false,
            });
        }
        Ok(SemSignature {
            params,
            return_ty: return_ty.clone(),
        })
    }

    /// Dispatch one trait method through the receiver's vtable slot.
    fn lower_dyn_call(
        &mut self,
        expr: &HirExpr,
        receiver: &HirExpr,
        slot: u32,
        args: &[HirExpr],
        signature: &hew_types::FnSig,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let return_ty = self.ty(&expr.ty);
        let dispatch = self.dyn_dispatch_signature(args, &return_ty)?;
        let decision = match dyn_receiver_passing(signature) {
            SemParamPassing::Consume => crate::BoundaryDecision::Move,
            SemParamPassing::BorrowMut => crate::BoundaryDecision::BorrowMut,
            SemParamPassing::Borrow | SemParamPassing::ReadOnly => crate::BoundaryDecision::Borrow,
        };
        let value = if decision == crate::BoundaryDecision::Move {
            self.lower_consuming_value(receiver)?
        } else {
            self.lower_borrowed_read(receiver, &mut loans)?.value
        };
        let lowered_args = self.lower_user_arguments(args, &dispatch.params, &mut loans)?;
        self.finish_user_call(
            PreparedCallee::Dyn {
                receiver: crate::BoundaryOperand {
                    operand: Operand { value },
                    decision,
                },
                slot,
            },
            dispatch,
            lowered_args,
            &loans,
            &live_before_arguments,
            value_required,
        )
    }

    /// Direct and indirect user calls share argument capture and both cleanup paths.
    #[allow(
        clippy::too_many_lines,
        reason = "one user-call boundary owns evaluation order, receiver transfer and both continuations"
    )]
    fn lower_direct_call(
        &mut self,
        expr: &HirExpr,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let HirExprKind::Call {
            target,
            callee,
            args,
        } = &expr.kind
        else {
            return Err("user-call lowering received a non-call".to_string());
        };
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let (callee, signature, actor) = match target {
            CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => {
                let local_actor_method =
                    if let SemCallableKind::HewActor(actor) = self.callable.kind {
                        self.service.actors[actor.0 as usize]
                            .methods
                            .iter()
                            .find_map(|id| {
                                self.service
                                    .callable(*id)
                                    .filter(|method| &method.declaration == declaration)
                                    .cloned()
                            })
                    } else {
                        None
                    };
                let target = match local_actor_method {
                    Some(method) => method,
                    None => self.service.resolve_direct_call(
                        declaration,
                        expr.site,
                        &self.substitution,
                    )?,
                };
                let actor = match target.kind {
                    SemCallableKind::HewActor(actor) => Some(actor),
                    SemCallableKind::HewDirect | SemCallableKind::HewClosure => None,
                };
                (PreparedCallee::Direct(target.id), target.signature, actor)
            }
            CallTarget::IndirectFunctionValue => {
                let ty = self.ty(&callee.ty);
                self.service.require_type_facts(&ty)?;
                let signature =
                    crate::callable_value_signature(&ty, self.service.checked_facts.rows())?;
                let (_, _, capabilities) = crate::callable_parts(&ty)?;
                let value = if capabilities.call == hew_types::CallableCallMode::Once {
                    self.lower_consuming_value(callee)?
                } else {
                    self.lower_borrowed_read(callee, &mut loans)?.value
                };
                let decision = match capabilities.call {
                    hew_types::CallableCallMode::Read => crate::BoundaryDecision::Borrow,
                    hew_types::CallableCallMode::Var => crate::BoundaryDecision::BorrowMut,
                    hew_types::CallableCallMode::Once => crate::BoundaryDecision::Move,
                };
                (
                    PreparedCallee::Indirect(crate::BoundaryOperand {
                        operand: Operand { value },
                        decision,
                    }),
                    signature,
                    None,
                )
            }
            _ => {
                return Err(
                    "user-call lowering requires a resolved direct or indirect target".to_string(),
                )
            }
        };
        let result_ty = self.ty(&expr.ty);
        // An actor method's first parameter is the caller's own state seat.
        let seats = usize::from(actor.is_some());
        if args.len() + seats != signature.params.len()
            || !crate::call_boundary_types_match(&result_ty, &signature.return_ty)
        {
            let name = match target {
                CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => {
                    declaration.full_path()
                }
                _ => "<function value>",
            };
            return Err(format!(
                "user call to `{name}` differs from its semantic signature: {} arguments, expected {}; result {result_ty:?}, expected {:?}",
                args.len(), signature.params.len(), signature.return_ty
            ));
        }
        // The two spellings of a channel half denote one value; the call site's
        // is the one that carries the message type, so the produced value
        // takes it and every downstream shape agrees with the scrutinee.
        let mut signature = signature;
        if signature.return_ty != result_ty {
            signature.return_ty = result_ty.clone();
        }
        let mut lowered_args =
            self.lower_user_arguments(args, &signature.params[seats..], &mut loans)?;
        if let Some(actor) = actor {
            if self.callable.kind != SemCallableKind::HewActor(actor) {
                return Err("actor method is entered only from its own actor's bodies".into());
            }
            let state = ValueId(0);
            // The callee mutates state exclusively; arguments read from it
            // travel as independent copies.
            self.snapshot_arguments_rooted_at(
                crate::OwnerRoot::Value(state),
                &mut lowered_args,
                &mut loans,
                &Provenance::Site(expr.site),
            )?;
            lowered_args.insert(
                0,
                crate::BoundaryOperand {
                    operand: Operand { value: state },
                    decision: crate::BoundaryDecision::BorrowMut,
                },
            );
        }
        self.finish_user_call(
            callee,
            signature,
            lowered_args,
            &loans,
            &live_before_arguments,
            value_required,
        )
    }

    /// Lower a trait-method call reached through a where-clause bound.
    ///
    /// Once the implementation is selected the receiver is simply its first
    /// parameter, so the call enters the same argument transfer and call
    /// boundary as any other direct call.
    #[allow(
        deprecated,
        reason = "a trait method reached through a where-clause bound is not builtin-generic dispatch, so `ResolvedImplCall` does not carry it; this arm reads the node, it does not construct one"
    )]
    fn lower_static_trait_call(
        &mut self,
        expr: &HirExpr,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let HirExprKind::CallTraitMethodStatic {
            receiver,
            target,
            args,
            ..
        } = &expr.kind
        else {
            return Err("static trait call lowering received a non-call".to_string());
        };
        let CallTarget::StaticTraitMethod {
            declaring_trait,
            method,
        } = target
        else {
            return Err("static trait call requires a checker-selected trait method".to_string());
        };
        let receiver_ty = self.ty(&receiver.ty);
        let callee = self.service.resolve_static_trait_call(
            declaring_trait,
            method,
            &receiver_ty,
            expr.site,
            &self.substitution,
        )?;
        let signature = callee.signature.clone();
        let result_ty = self.ty(&expr.ty);
        let arguments: Vec<HirExpr> = std::iter::once((**receiver).clone())
            .chain(args.iter().cloned())
            .collect();
        if arguments.len() != signature.params.len() || result_ty != signature.return_ty {
            return Err(format!(
                "static trait call to `{}` differs from its semantic signature: {} arguments, expected {}; result {result_ty:?}, expected {:?}",
                callee.declaration.full_path(),
                arguments.len(),
                signature.params.len(),
                signature.return_ty
            ));
        }
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let lowered_args = self.lower_user_arguments(&arguments, &signature.params, &mut loans)?;
        self.finish_user_call(
            PreparedCallee::Direct(callee.id),
            signature,
            lowered_args,
            &loans,
            &live_before_arguments,
            value_required,
        )
    }

    /// Capture arguments while keeping earlier consumed values live until the call.
    fn lower_user_arguments(
        &mut self,
        args: &[HirExpr],
        params: &[SemAbiParam],
        loans: &mut Vec<ValueId>,
    ) -> Result<Vec<crate::BoundaryOperand>, String> {
        let receiver_loan_depth = self.argument_receiver_loans.len();
        self.argument_receiver_loans.extend(loans.iter().copied());
        let mut lowered_args = Vec::with_capacity(args.len());
        for (index, (arg, expected)) in args.iter().zip(params).enumerate() {
            let loan_floor = loans.len();
            let stable_tail = args[index + 1..].iter().all(Self::stable_argument_read);
            let operand = if expected.passing == SemParamPassing::Consume {
                let value = self.lower_consuming_value(arg)?;
                Operand {
                    value: self.coerce_value(value, &expected.ty, Provenance::Site(arg.site))?,
                }
            } else if expected.passing == SemParamPassing::ReadOnly
                && arg.intent == IntentKind::Consume
            {
                Operand {
                    value: lower_initial_value_transfer(
                        self,
                        arg,
                        "trivial consuming argument",
                        OwnedBindingUse::Move,
                    )?,
                }
            } else if self.ty(&arg.ty) == expected.ty {
                self.lower_call_read(arg, loans, stable_tail, true)?
            } else {
                let value = lower_initial_value_transfer(
                    self,
                    arg,
                    "call argument coercion",
                    OwnedBindingUse::Copy,
                )?;
                Operand {
                    value: self.coerce_value(value, &expected.ty, Provenance::Site(arg.site))?,
                }
            };
            lowered_args.push(crate::BoundaryOperand {
                operand,
                decision: match expected.passing {
                    SemParamPassing::ReadOnly => crate::BoundaryDecision::Copy,
                    SemParamPassing::Borrow => crate::BoundaryDecision::Borrow,
                    SemParamPassing::Consume => crate::BoundaryDecision::Move,
                    SemParamPassing::BorrowMut => {
                        return Err(
                            "direct parameter transfer requires its source contract".to_string()
                        )
                    }
                },
            });
            self.argument_receiver_loans
                .extend_from_slice(&loans[loan_floor..]);
        }
        self.argument_receiver_loans.truncate(receiver_loan_depth);
        Ok(lowered_args)
    }

    /// One user-call boundary owns argument temporaries and both continuations.
    #[allow(
        clippy::too_many_lines,
        reason = "normal and fault continuations share one ownership boundary"
    )]
    fn finish_user_call(
        &mut self,
        callee: PreparedCallee,
        signature: SemSignature,
        lowered_args: Vec<crate::BoundaryOperand>,
        loans: &[ValueId],
        live_before_arguments: &std::collections::HashSet<ValueId>,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        for argument in &lowered_args {
            if argument.decision == crate::BoundaryDecision::Move {
                self.owned_live.remove(&argument.operand.value);
            }
        }
        if let PreparedCallee::Indirect(receiver) | PreparedCallee::Dyn { receiver, .. } = &callee {
            if receiver.decision == crate::BoundaryDecision::Move {
                self.owned_live.remove(&receiver.operand.value);
            }
        }
        let live_at_call = self.owned_live.clone();
        let temporaries: Vec<_> = live_at_call
            .keys()
            .filter(|value| !live_before_arguments.contains(value))
            .copied()
            .collect();
        let return_ty = signature.return_ty.clone();
        let (result, normal, continuation) = if return_ty == ResolvedTy::Never {
            (CallResult::Never, None, None)
        } else if return_ty == ResolvedTy::Unit {
            if value_required {
                return Err("unit-valued call cannot produce an SSA value".to_string());
            }
            (
                CallResult::Unit,
                Some(Edge {
                    target: self.new_block(Vec::new()),
                    args: vec![],
                }),
                None,
            )
        } else {
            self.service.require_type_facts(&return_ty)?;
            let own = OwnKind::of_ty(&return_ty, self.service.checked_facts.rows())?;
            let raw = self.fresh_value();
            let continuation = self.fresh_value();
            let normal = self.new_block(vec![BlockArg {
                value: continuation,
                ty: return_ty.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef {
                    id: raw,
                    ty: return_ty.clone(),
                    own,
                }),
                Some(Edge {
                    target: normal,
                    args: vec![Operand { value: raw }],
                }),
                Some((continuation, own)),
            )
        };
        let normal_block = normal.as_ref().map(|edge| edge.target);
        let unwind = self.new_block(Vec::new());
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(callee.invoke(
            id,
            signature,
            lowered_args,
            result,
            normal,
            CallUnwind::Cleanup(Edge {
                target: unwind,
                args: vec![],
            }),
        ))?;
        self.current = unwind;
        self.owned_live = live_at_call.clone();
        self.end_call_loans(loans)?;
        self.finish_fault_exit()?;
        let Some(normal_block) = normal_block else {
            self.current = self.new_block(Vec::new());
            self.set_terminator(SemTerminator::Unreachable)?;
            return Ok(None);
        };
        self.current = normal_block;
        self.owned_live = live_at_call;
        // A successful result already owns its value before argument cleanup.
        // Cooperative cleanup may fail, so its unwind edge must release that
        // result along with the caller's other live values.
        if let Some((value, OwnKind::Owned)) = continuation {
            self.owned_live.insert(value, return_ty);
        }
        self.end_call_loans(loans)?;
        for value in temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        Ok(continuation.map(|(value, _)| value))
    }

    /// Snapshot borrowed arguments which would otherwise overlap an exclusive
    /// receiver rooted at `root`.
    pub(super) fn snapshot_arguments_rooted_at(
        &mut self,
        root: crate::OwnerRoot,
        lowered_args: &mut [crate::BoundaryOperand],
        loans: &mut Vec<ValueId>,
        provenance: &Provenance,
    ) -> Result<(), String> {
        for argument in lowered_args {
            let value = argument.operand.value;
            if self.value_own_kind(value) == Some(OwnKind::Guaranteed)
                && self.value_borrow_root(value)? == root
            {
                let ty = self
                    .value_ty(value)
                    .ok_or_else(|| "borrowed argument has no type".to_string())?;
                argument.operand.value = self.emit_typed(
                    provenance.clone(),
                    &ty,
                    SemOpKind::CopyValue {
                        source: Operand { value },
                    },
                )?;
            }
        }
        let related = loans
            .iter()
            .copied()
            .filter_map(|loan| match self.value_borrow_root(loan) {
                Ok(owner) if owner == root => Some(Ok(loan)),
                Ok(_) => None,
                Err(error) => Some(Err(error)),
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.end_call_loans(&related)?;
        loans.retain(|loan| !related.contains(loan));
        Ok(())
    }

    #[allow(
        deprecated,
        reason = "a trait method reached through a where-clause bound is not builtin-generic dispatch, so `ResolvedImplCall` does not carry it; these arms read the node, they do not construct one"
    )]
    fn lower_call(
        &mut self,
        expr: &HirExpr,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        if matches!(expr.kind, HirExprKind::CallTraitMethodStatic { .. }) {
            return self.lower_static_trait_call(expr, value_required);
        }
        let HirExprKind::Call { target, args, .. } = &expr.kind else {
            return Err(
                "internal SIR lowering error: call lowering received a non-call".to_string(),
            );
        };
        match target {
            CallTarget::Builtin { endpoint } if endpoint == "assert" => {
                self.lower_assert(expr, args)?;
                Ok(None)
            }
            CallTarget::Builtin { endpoint } if endpoint == "sleep" => {
                self.lower_sleep(expr, args)?;
                Ok(None)
            }
            CallTarget::Builtin { endpoint } if endpoint == "sleep_until" => {
                self.lower_sleep_until(expr, args)?;
                Ok(None)
            }
            CallTarget::Extern {
                declaration,
                endpoint,
                ..
            } => self.lower_extern_call(expr, declaration, endpoint, args, value_required),
            CallTarget::Runtime(hew_types::RuntimeCallFamily::SupervisorStop) => {
                let [handle] = args.as_slice() else {
                    return Err("supervisor stop takes exactly one handle".into());
                };
                self.lower_supervisor_stop(handle)
            }
            CallTarget::Runtime(family) => self.lower_runtime_operation(
                expr,
                *family,
                &args.iter().collect::<Vec<_>>(),
                value_required,
            ),
            CallTarget::User(_) | CallTarget::ImplMethod(_) | CallTarget::IndirectFunctionValue => {
                self.lower_direct_call(expr, value_required)
            }
            CallTarget::Builtin { endpoint } => {
                let family = hew_types::RuntimeCallFamily::from_catalog_endpoint(endpoint)
                    .ok_or_else(|| {
                        format!(
                            "call target {target:?} has no verified ownership-SIR operation contract"
                        )
                    })?;
                self.lower_runtime_operation(
                    expr,
                    family,
                    &args.iter().collect::<Vec<_>>(),
                    value_required,
                )
            }
            _ => Err(format!(
                "call target {target:?} has no verified ownership-SIR operation contract"
            )),
        }
    }

    fn lower_runtime_operation(
        &mut self,
        expr: &HirExpr,
        family: hew_types::RuntimeCallFamily,
        args: &[&HirExpr],
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        self.lower_runtime_operation_with(expr, family, args, value_required, &[])
    }

    /// As [`Self::lower_runtime_operation`], with `prelowered` naming argument
    /// positions whose owning value this body already produced. The named
    /// argument is transferred as it stands instead of being lowered from its
    /// expression, so an element rebuilt in place reaches the set entry rather
    /// than a second read of the slot it is replacing.
    #[allow(
        clippy::too_many_lines,
        reason = "runtime contract admission and its explicit success/failure CFG form one semantic boundary"
    )]
    fn lower_runtime_operation_with(
        &mut self,
        expr: &HirExpr,
        family: hew_types::RuntimeCallFamily,
        args: &[&HirExpr],
        value_required: bool,
        prelowered: &[(usize, ValueId)],
    ) -> Result<Option<ValueId>, String> {
        use hew_types::{RuntimeArgumentEffect, RuntimeResultEffect};

        let observation = match family {
            hew_types::RuntimeCallFamily::ActorLink => Some(crate::LocalObservationKind::Link),
            hew_types::RuntimeCallFamily::ActorMonitor => {
                Some(crate::LocalObservationKind::Monitor)
            }
            hew_types::RuntimeCallFamily::ActorUnlink => Some(crate::LocalObservationKind::Unlink),
            hew_types::RuntimeCallFamily::ActorDemonitor => {
                Some(crate::LocalObservationKind::Demonitor)
            }
            _ => None,
        };
        if let Some(kind) = observation {
            let [target] = args else {
                return Err("local observation takes one target".into());
            };
            let operation = crate::ActorOperation::LocalObservation {
                kind,
                target: self.ty(&target.ty),
                result: self.ty(&expr.ty),
            };
            let signature = self.actor_signature(&operation)?;
            self.service.require_type_facts(&signature.return_ty)?;
            let value = self.lower_expr(target)?;
            return self.emit_actor_call(operation, signature, vec![value]);
        }

        if let hew_types::RuntimeCallFamily::AsyncIo(operation) = family {
            return self.lower_native_io(expr, operation, args);
        }
        if matches!(
            family,
            hew_types::RuntimeCallFamily::SinkWrite(_)
                | hew_types::RuntimeCallFamily::StreamSendLayout
        ) {
            let [sink, value] = args else {
                return Err("stream write takes one sink and one element".into());
            };
            self.lower_sink_write(sink, value)?;
            return Ok(None);
        }
        if family == hew_types::RuntimeCallFamily::StreamNextLayout {
            let [stream] = args else {
                return Err("stream receive takes exactly one stream".into());
            };
            return self.lower_stream_next(expr, stream).map(Some);
        }
        if matches!(
            family,
            hew_types::RuntimeCallFamily::ChannelRecvLayout
                | hew_types::RuntimeCallFamily::ChannelTryRecvLayout
        ) {
            let [channel] = args else {
                return Err("channel receive takes exactly one receiver".into());
            };
            let park = family == hew_types::RuntimeCallFamily::ChannelRecvLayout;
            return self.lower_channel_recv(expr, channel, park).map(Some);
        }
        if family == hew_types::RuntimeCallFamily::ChannelSendLayout {
            let [channel, value] = args else {
                return Err("channel send takes exactly one sender and one element".into());
            };
            self.lower_channel_send(channel, value)?;
            return Ok(None);
        }

        let contract = family.semantic_contract().ok_or_else(|| {
            format!("runtime family `{family:?}` has no ownership-SIR semantic contract")
        })?;
        let source_types = args.iter().map(|arg| self.ty(&arg.ty)).collect::<Vec<_>>();
        let instantiated = contract
            .resolve_types(&source_types, &self.ty(&expr.ty))
            .map_err(|error| format!("runtime operation {family:?}: {error}"))?;
        let parameter_types = &instantiated.arguments;
        for (index, (source, target)) in source_types.iter().zip(parameter_types).enumerate() {
            self.service.require_type_facts(source)?;
            self.service.require_type_facts(target)?;
            if source != target {
                if contract.arguments[index].effect != RuntimeArgumentEffect::Value {
                    return Err(format!(
                        "runtime argument {index} cannot coerce a non-value operand"
                    ));
                }
                crate::verify_callable_coercion(source, target, self.service.checked_facts.rows())?;
            }
        }
        if matches!(
            family,
            hew_types::RuntimeCallFamily::Map(hew_types::runtime_call::MapValueOp::New)
                | hew_types::RuntimeCallFamily::Set(hew_types::runtime_call::SetValueOp::New)
        ) {
            let result_ty = self.ty(&expr.ty);
            let collection_ty = parameter_types.first().unwrap_or(&result_ty);
            let (_, arguments) = collection_type_arguments(collection_ty)
                .ok_or_else(|| "collection operation has no canonical receiver type".to_string())?;
            self.service.require_key_capabilities(&arguments[0])?;
        }
        if family == hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Contains) {
            self.service
                .require_value_capability(&parameter_types[1], hew_types::ValueCapability::Eq)?;
        }
        if matches!(contract.result, RuntimeResultEffect::IndependentValue(_)) {
            self.service.require_type_facts(&instantiated.result_ty)?;
            if self.service.checked_facts.rows()[&TypeInstanceKey(instantiated.result_ty.clone())]
                .clone
                == hew_types::CloneKind::None
            {
                return Err(
                    "runtime read cannot copy an affine element; use an owning removal".into(),
                );
            }
        }
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut transformed_place = None;
        let mut lowered_args = Vec::with_capacity(args.len());
        let mut loans = Vec::new();
        let effects = contract
            .arguments
            .iter()
            .zip(parameter_types)
            .map(|(argument, ty)| {
                argument.effect.resolve_operand(
                    self.service.checked_facts.rows()[&TypeInstanceKey(ty.clone())].class,
                )
            })
            .collect::<Vec<_>>();
        // An adopted operand that still has a copy recipe keeps the caller's
        // own value: lowering reads the binding as an independent owner and
        // transfers that owner, rather than consuming what the caller named.
        let copied_ingress = contract
            .arguments
            .iter()
            .zip(parameter_types)
            .map(|(argument, ty)| {
                argument.effect == RuntimeArgumentEffect::Value
                    && self.service.checked_facts.rows()[&TypeInstanceKey(ty.clone())].clone
                        != hew_types::CloneKind::None
            })
            .collect::<Vec<bool>>();
        let read_only = effects
            .iter()
            .all(|effect| *effect != RuntimeArgumentEffect::Move);
        let argument_loan_depth = self.argument_receiver_loans.len();
        for (index, (&arg, effect)) in args.iter().zip(effects).enumerate() {
            if let Some(&(_, value)) = prelowered.iter().find(|(at, _)| *at == index) {
                let decision = match effect {
                    RuntimeArgumentEffect::Move => crate::BoundaryDecision::Move,
                    RuntimeArgumentEffect::Borrow => crate::BoundaryDecision::Borrow,
                    RuntimeArgumentEffect::Copy | RuntimeArgumentEffect::Value => {
                        crate::BoundaryDecision::Copy
                    }
                };
                lowered_args.push(crate::BoundaryOperand {
                    operand: Operand { value },
                    decision,
                });
                continue;
            }
            let loan_floor = loans.len();
            let (value, decision) = if source_types[index] == parameter_types[index] {
                match effect {
                    RuntimeArgumentEffect::Value => unreachable!("value ingress was resolved"),
                    RuntimeArgumentEffect::Borrow => {
                        let stable_tail = args[index + 1..]
                            .iter()
                            .all(|arg| Self::stable_argument_read(arg));
                        let operand =
                            self.lower_call_read(arg, &mut loans, stable_tail, read_only)?;
                        (operand.value, crate::BoundaryDecision::Borrow)
                    }
                    RuntimeArgumentEffect::Copy => {
                        let no_owner = OwnKind::of_ty(
                            &parameter_types[index],
                            self.service.checked_facts.rows(),
                        )? == OwnKind::None;
                        let stable_tail = args[index + 1..]
                            .iter()
                            .all(|arg| Self::stable_argument_read(arg));
                        let operand = self.lower_call_read(
                            arg,
                            &mut loans,
                            stable_tail,
                            read_only && no_owner,
                        )?;
                        (operand.value, crate::BoundaryDecision::Copy)
                    }
                    RuntimeArgumentEffect::Move
                        if index == 0
                            && matches!(
                                contract.result,
                                RuntimeResultEffect::UpdatedReceiver(_)
                                    | RuntimeResultEffect::UpdatedReceiverAndValue(_)
                            ) =>
                    {
                        let place = self.resolve_mutable_place(arg)?;
                        if OwnKind::of_ty(&place.leaf_ty, self.service.checked_facts.rows())?
                            != OwnKind::Owned
                        {
                            return Err("runtime transform receiver must be an owned value".into());
                        }
                        transformed_place = Some(place);
                        // The receiver is retaken after later arguments finish;
                        // no operand or snapshot is emitted for it here.
                        continue;
                    }
                    RuntimeArgumentEffect::Move if copied_ingress[index] => {
                        (self.lower_adopted_copy(arg)?, crate::BoundaryDecision::Move)
                    }
                    RuntimeArgumentEffect::Move => (
                        self.lower_consuming_value(arg)?,
                        crate::BoundaryDecision::Move,
                    ),
                }
            } else {
                // Weakening a callable's capabilities must not consume a
                // copyable source binding. Convert an independent owner first.
                let source_clone = self.service.checked_facts.rows()
                    [&TypeInstanceKey(source_types[index].clone())]
                    .clone;
                let value = if source_clone == hew_types::CloneKind::None {
                    self.lower_consuming_value(arg)?
                } else {
                    lower_initial_value_transfer(
                        self,
                        arg,
                        "runtime value coercion",
                        OwnedBindingUse::Copy,
                    )?
                };
                let value =
                    self.coerce_value(value, &parameter_types[index], Provenance::Site(arg.site))?;
                let decision = match effect {
                    RuntimeArgumentEffect::Copy => crate::BoundaryDecision::Copy,
                    RuntimeArgumentEffect::Move => crate::BoundaryDecision::Move,
                    _ => unreachable!("value ingress resolves to copy or move"),
                };
                (value, decision)
            };
            lowered_args.push(crate::BoundaryOperand {
                operand: Operand { value },
                decision,
            });
            self.argument_receiver_loans
                .extend_from_slice(&loans[loan_floor..]);
        }
        self.argument_receiver_loans.truncate(argument_loan_depth);

        // Preserve arguments borrowing the receiver's owner before its take.
        // Alias identity comes from the same declared place paths as loans.
        if let Some(place) = &transformed_place {
            let selected = self
                .owned_projection(place)?
                .ok_or_else(|| "runtime receiver has no owning place".to_string())?;
            let root = self.place_borrow_root(selected)?;
            self.snapshot_arguments_rooted_at(
                root,
                &mut lowered_args,
                &mut loans,
                &Provenance::Site(expr.site),
            )?;
        }

        // Argument temporaries precede the receiver's actual transfer.
        let argument_temporaries: Vec<_> = self
            .owned_live
            .keys()
            .filter(|value| {
                !live_before_arguments.contains(value)
                    && !lowered_args.iter().any(|arg| {
                        arg.decision == crate::BoundaryDecision::Move
                            && arg.operand.value == **value
                    })
            })
            .copied()
            .collect();
        let mut transformed_projection = None;
        if let Some(place) = &transformed_place {
            let provenance = Provenance::Site(expr.site);
            let projected = self
                .owned_projection(place)?
                .ok_or_else(|| "runtime receiver has no owning place".to_string())?;
            // A transform takes its receiver, which a live element loan of the
            // same owner forbids. Refusing here names the source construct
            // instead of leaving it to the ownership verifier.
            let root = self.place_borrow_root(projected)?;
            self.end_binding_loans_on(root)?;
            for loan in self.scope_loans.clone() {
                if self.ended_loans.contains(&loan) {
                    continue;
                }
                if self.value_borrow_root(loan)? == root {
                    return Err(
                        "E_OWN_CONSUME_BORROWED: this collection is borrowed by a live element \
                         loan; the loop or read holding it must end before the collection is \
                         mutated or drained"
                            .to_string(),
                    );
                }
            }
            transformed_projection = Some(projected);
            // Actor state remains initialized while a runtime operation runs.
            // Stage an independent value and publish it with StoreAssign only
            // after success, preserving the original field on a fault edge.
            let receiver_kind = if self.state_field_leaves_as_copy(projected, args[0])? {
                SemOpKind::LoadCopy { place: projected }
            } else {
                SemOpKind::LoadTake { place: projected }
            };
            let source = self.emit_typed(provenance.clone(), &place.leaf_ty, receiver_kind)?;
            if matches!(
                family,
                hew_types::RuntimeCallFamily::Array(hew_types::runtime_call::ArrayValueOp::Set)
                    | hew_types::RuntimeCallFamily::Vector(
                        hew_types::runtime_call::VecValueOp::Clear
                            | hew_types::runtime_call::VecValueOp::Set
                    )
            ) && self.value_needs_close(&place.leaf_ty)
            {
                let index = matches!(
                    family,
                    hew_types::RuntimeCallFamily::Vector(hew_types::runtime_call::VecValueOp::Set)
                        | hew_types::RuntimeCallFamily::Array(
                            hew_types::runtime_call::ArrayValueOp::Set
                        )
                )
                .then(|| lowered_args[0].operand.value);
                let loan_depth = self.argument_receiver_loans.len();
                self.argument_receiver_loans.extend(loans.iter().copied());
                self.close_selected_value(None, Some(source), index)?;
                self.dispatch_value_cleanup()?;
                self.argument_receiver_loans.truncate(loan_depth);
            }
            self.owned_live.remove(&source);
            let moved = self.emit_typed(
                provenance,
                &place.leaf_ty,
                SemOpKind::Move {
                    source: Operand { value: source },
                },
            )?;
            self.owned_live.remove(&moved);
            lowered_args.insert(
                0,
                crate::BoundaryOperand {
                    operand: Operand { value: moved },
                    decision: crate::BoundaryDecision::Move,
                },
            );
        }
        // Keep transferred values available to cleanup until pre-mutation close succeeds.
        for argument in &lowered_args {
            if argument.decision == crate::BoundaryDecision::Move {
                self.owned_live.remove(&argument.operand.value);
            }
        }
        let live_at_call = self.owned_live.clone();
        if let RuntimeResultEffect::FreshOwnedVariant(kind) = contract.result {
            self.service
                .require_runtime_variant_result_shapes(kind, &instantiated.result_ty)?;
        }
        let mut live_on_failure = live_at_call.clone();
        if contract.preserves_inputs_on_failure() {
            for argument in &lowered_args {
                if argument.decision == crate::BoundaryDecision::Move {
                    let value = argument.operand.value;
                    live_on_failure.insert(
                        value,
                        self.value_ty(value).ok_or("runtime input has no type")?,
                    );
                }
            }
        }
        let semantic_result_ty =
            (instantiated.result_ty != ResolvedTy::Unit).then_some(instantiated.result_ty);
        match (contract.result, &semantic_result_ty) {
            (RuntimeResultEffect::Unit, None) if self.ty(&expr.ty) == ResolvedTy::Unit => {}
            (RuntimeResultEffect::UpdatedReceiver(_), Some(_))
                if self.ty(&expr.ty) == ResolvedTy::Unit && !value_required => {}
            (RuntimeResultEffect::UpdatedReceiverAndValue(_), Some(ResolvedTy::Tuple(fields)))
                if fields.len() == 2 && self.ty(&expr.ty) == fields[1] => {}
            (_, Some(result_ty)) if self.ty(&expr.ty) == *result_ty => {}
            _ => {
                return Err(format!(
                    "runtime family `{family:?}` result contract disagrees with expression type `{}`",
                    self.ty(&expr.ty).user_facing()
                ));
            }
        }

        if semantic_result_ty == Some(ResolvedTy::Never) {
            if !contract.failures.is_empty() {
                return Err(format!(
                    "runtime family `{family:?}` never returns but declares failure edges"
                ));
            }
            // The process ends here: no continuation, no scope cleanup. The
            // mandatory normal edge is a structural unreachable block.
            let unreachable = self.new_block(Vec::new());
            let id = OpId(self.ops);
            self.ops += 1;
            self.set_terminator(SemTerminator::RtCall {
                id,
                family,
                args: lowered_args,
                result: CallResult::Never,
                normal: Edge {
                    target: unreachable,
                    args: Vec::new(),
                },
                unwind: CallUnwind::NotApplicable,
            })?;
            self.current = unreachable;
            self.set_terminator(SemTerminator::Unreachable)?;
            return Ok(None);
        }
        let borrowed_result = matches!(contract.result, RuntimeResultEffect::Borrowed(_));
        let (result, normal, continuation) = if let Some(result_ty) = semantic_result_ty {
            self.service.require_type_facts(&result_ty)?;
            let own = if borrowed_result {
                OwnKind::Guaranteed
            } else {
                OwnKind::of_ty(&result_ty, self.service.checked_facts.rows())?
            };
            if matches!(contract.result, RuntimeResultEffect::FreshOwnedVariant(_))
                && own != OwnKind::Owned
            {
                return Err(format!(
                    "runtime family `{family:?}` variant result `{}` is not owned",
                    result_ty.user_facing()
                ));
            }
            let raw = self.fresh_value();
            let continuation = self.fresh_value();
            let normal = self.new_block(vec![BlockArg {
                value: continuation,
                ty: result_ty.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef {
                    id: raw,
                    ty: result_ty,
                    own,
                }),
                Edge {
                    target: normal,
                    args: vec![Operand { value: raw }],
                },
                Some(continuation),
            )
        } else {
            (
                CallResult::Unit,
                Edge {
                    target: self.new_block(Vec::new()),
                    args: Vec::new(),
                },
                None,
            )
        };

        let failure = contract.failures.first().copied();
        if contract.failures.len() > 1 && !contract.propagates_fault() {
            return Err(format!(
                "runtime family `{family:?}` has more failure edges than RtCall currently represents"
            ));
        }
        let failure_block = failure.map(|_| self.new_block(Vec::new()));
        let unwind = failure_block.map_or(CallUnwind::NotApplicable, |target| {
            CallUnwind::Cleanup(Edge {
                target,
                args: Vec::new(),
            })
        });
        let id = OpId(self.ops);
        self.ops += 1;
        let normal_target = normal.target;
        self.set_terminator(SemTerminator::RtCall {
            id,
            family,
            args: lowered_args,
            result,
            normal,
            unwind,
        })?;

        if let (Some(failure), Some(block)) = (failure, failure_block) {
            self.current = block;
            self.owned_live = live_on_failure;
            self.end_call_loans(&loans)?;
            if contract.propagates_fault() {
                self.finish_fault_exit()?;
            } else {
                self.finish_checked_fault(
                    crate::runtime_failure_trap_kind(failure)
                        .ok_or_else(|| "static runtime failure has no trap kind".to_string())?,
                )?;
            }
        }
        self.current = normal_target;
        self.owned_live = live_at_call;
        if borrowed_result {
            // The result is a loan of argument zero: its owner must stay
            // borrowed for as long as the result is readable, and the loan the
            // result itself names ends with the scope that reads it. The
            // enclosing scope ends both on every exit, including a loop
            // back-edge, innermost first.
            self.scope_loans.extend(loans.iter().copied());
            if let Some(continuation) = continuation {
                self.scope_loans.push(continuation);
            }
        } else {
            self.end_call_loans(&loans)?;
        }
        for value in argument_temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        if let Some(continuation) = continuation {
            let result_ty = self
                .value_ty(continuation)
                .expect("runtime continuation block argument was just created");
            if self.value_own_kind(continuation) == Some(OwnKind::Owned) {
                self.owned_live.insert(continuation, result_ty);
            }
            if matches!(
                contract.result,
                RuntimeResultEffect::UpdatedReceiverAndValue(_)
            ) {
                let ty = self
                    .value_ty(continuation)
                    .ok_or_else(|| "runtime transform result disappeared".to_string())?;
                let shape = self.service.require_aggregate_shape(&ty)?;
                let results = self.emit_destructure_value(
                    continuation,
                    &ty,
                    shape,
                    Provenance::Site(expr.site),
                )?;
                self.store_projected(
                    transformed_projection
                        .ok_or_else(|| "runtime transform has no source place".to_string())?,
                    results[0].id,
                    Provenance::Site(expr.site),
                )?;
                return Ok(Some(results[1].id));
            }
            if matches!(contract.result, RuntimeResultEffect::UpdatedReceiver(_)) {
                self.store_projected(
                    transformed_projection
                        .ok_or_else(|| "runtime transform has no source place".to_string())?,
                    continuation,
                    Provenance::Site(expr.site),
                )?;
                return Ok(None);
            }
        }
        if value_required && continuation.is_none() {
            return Err(format!(
                "unit-valued runtime family `{family:?}` cannot produce an SSA value"
            ));
        }
        Ok(continuation)
    }

    /// The exact `extern` declaration behind one call target.
    fn extern_signature(
        &self,
        declaration: &hew_types::DefId,
        endpoint: &str,
    ) -> Result<crate::ExternSignature, String> {
        self.service
            .module
            .items
            .iter()
            .find_map(|item| {
                let hew_hir::HirItem::ExternFn(function) = item else {
                    return None;
                };
                (&function.declaration == declaration && function.name == endpoint).then(|| {
                    crate::ExternSignature {
                        declaration: function.declaration.clone(),
                        symbol: function.name.clone(),
                        params: function.param_tys.clone(),
                        consumes: function.param_consume.clone(),
                        result: function.return_ty.clone(),
                    }
                })
            })
            .ok_or_else(|| {
                format!(
                    "extern `{endpoint}` is named by an `#[extern_symbol]` method rather than an \
                     `extern` block, and an open-set extern method has no declared C signature to \
                     call through"
                )
            })
    }

    /// Refuse a declaration that disagrees with the generated ownership row.
    ///
    /// The row is the audited truth for a classified runtime symbol; an
    /// unclassified symbol has no row and the declaration stands alone. Only
    /// parameters that carry a Hew obligation are compared: a `#[opaque]`
    /// pointer-width handle is a bit-copied id whose lifecycle belongs to its
    /// `#[resource]` owner, so a row that frees the underlying C allocation
    /// says nothing about the handle's Hew boundary.
    fn verify_extern_declaration_ownership(
        signature: &crate::ExternSignature,
        obligations: &[bool],
        result_owned: bool,
    ) -> Result<(), String> {
        use hew_types::ffi_contracts::{
            extern_ownership_contract, ExternParamOwnership, ExternResultOwnership,
        };
        let Some(contract) = extern_ownership_contract(&signature.symbol).contract() else {
            return Ok(());
        };
        if contract.params.len() == signature.consumes.len() {
            for (index, (declared, audited)) in
                signature.consumes.iter().zip(contract.params).enumerate()
            {
                if obligations[index] && *declared != (*audited == ExternParamOwnership::Consume) {
                    return Err(format!(
                        "extern `{}` parameter {index} declares {}, its audited ownership row says {audited:?}",
                        signature.symbol,
                        if *declared { "`consume`" } else { "a borrow" }
                    ));
                }
            }
        }
        match contract.result {
            ExternResultOwnership::Borrowed => Err(format!(
                "extern `{}` returns a borrow of a foreign allocation, which has no owner to borrow from",
                signature.symbol
            )),
            ExternResultOwnership::None if result_owned => Err(format!(
                "extern `{}` declares an owned result, its audited ownership row transfers nothing",
                signature.symbol
            )),
            _ => Ok(()),
        }
    }

    /// Lower a call to a declared C-ABI symbol.
    ///
    /// The `extern` declaration is the ownership authority: `consume` pins a
    /// transfer, its absence a borrow, and the declared return type decides
    /// whether the caller receives an owner. A C call cannot raise a Hew
    /// fault, so the call has no unwind edge.
    #[expect(
        clippy::too_many_lines,
        reason = "one extern boundary: declaration admission, operand transfer and result"
    )]
    fn lower_extern_call(
        &mut self,
        expr: &HirExpr,
        declaration: &hew_types::DefId,
        endpoint: &str,
        args: &[HirExpr],
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let signature = self.extern_signature(declaration, endpoint)?;
        if signature.params.len() != args.len() || signature.consumes.len() != args.len() {
            return Err(format!(
                "extern `{endpoint}` declares {} parameters, called with {}",
                signature.params.len(),
                args.len()
            ));
        }
        if signature.result == ResolvedTy::Never {
            return Err(format!(
                "extern `{endpoint}` cannot be declared to never return"
            ));
        }
        if self.ty(&expr.ty) != signature.result {
            return Err(format!(
                "extern `{endpoint}` returns `{}`, used as `{}`",
                signature.result.user_facing(),
                self.ty(&expr.ty).user_facing()
            ));
        }
        for ty in signature
            .params
            .iter()
            .chain(std::iter::once(&signature.result))
        {
            if *ty != ResolvedTy::Unit {
                self.service.require_type_facts(ty)?;
            }
        }
        let mut decisions = Vec::with_capacity(args.len());
        for (index, ty) in signature.params.iter().enumerate() {
            decisions.push(
                if OwnKind::of_ty(ty, self.service.checked_facts.rows())? == OwnKind::None {
                    crate::BoundaryDecision::Copy
                } else if signature.consumes[index] {
                    crate::BoundaryDecision::Move
                } else {
                    crate::BoundaryDecision::Borrow
                },
            );
        }
        let obligations = decisions
            .iter()
            .map(|decision| *decision != crate::BoundaryDecision::Copy)
            .collect::<Vec<_>>();
        let result_owned = signature.result != ResolvedTy::Unit
            && OwnKind::of_ty(&signature.result, self.service.checked_facts.rows())?
                == OwnKind::Owned;
        Self::verify_extern_declaration_ownership(&signature, &obligations, result_owned)?;
        let read_only = decisions
            .iter()
            .all(|decision| *decision != crate::BoundaryDecision::Move);
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut lowered_args = Vec::with_capacity(args.len());
        let mut loans = Vec::new();
        let argument_loan_depth = self.argument_receiver_loans.len();
        for (index, (arg, decision)) in args.iter().zip(&decisions).enumerate() {
            let loan_floor = loans.len();
            let value = if *decision == crate::BoundaryDecision::Move {
                self.lower_consuming_value(arg)?
            } else {
                let stable_tail = args[index + 1..].iter().all(Self::stable_argument_read);
                self.lower_call_read(arg, &mut loans, stable_tail, read_only)?
                    .value
            };
            lowered_args.push(crate::BoundaryOperand {
                operand: Operand { value },
                decision: *decision,
            });
            self.argument_receiver_loans
                .extend_from_slice(&loans[loan_floor..]);
        }
        self.argument_receiver_loans.truncate(argument_loan_depth);
        let argument_temporaries: Vec<_> = self
            .owned_live
            .keys()
            .filter(|value| {
                !live_before_arguments.contains(value)
                    && !lowered_args.iter().any(|arg| {
                        arg.decision == crate::BoundaryDecision::Move
                            && arg.operand.value == **value
                    })
            })
            .copied()
            .collect();
        for argument in &lowered_args {
            if argument.decision == crate::BoundaryDecision::Move {
                self.owned_live.remove(&argument.operand.value);
            }
        }
        let live_at_call = self.owned_live.clone();
        let (result, normal, continuation) = if signature.result == ResolvedTy::Unit {
            (
                CallResult::Unit,
                Edge {
                    target: self.new_block(Vec::new()),
                    args: Vec::new(),
                },
                None,
            )
        } else {
            let own = OwnKind::of_ty(&signature.result, self.service.checked_facts.rows())?;
            let raw = self.fresh_value();
            let continuation = self.fresh_value();
            let normal = self.new_block(vec![BlockArg {
                value: continuation,
                ty: signature.result.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef {
                    id: raw,
                    ty: signature.result.clone(),
                    own,
                }),
                Edge {
                    target: normal,
                    args: vec![Operand { value: raw }],
                },
                Some(continuation),
            )
        };
        let id = OpId(self.ops);
        self.ops += 1;
        let normal_target = normal.target;
        self.set_terminator(SemTerminator::ExternCall {
            id,
            signature: Box::new(signature),
            args: lowered_args,
            result,
            normal,
            unwind: CallUnwind::NotApplicable,
        })?;
        self.current = normal_target;
        self.owned_live = live_at_call;
        self.end_call_loans(&loans)?;
        for value in argument_temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        if let Some(continuation) = continuation {
            let ty = self
                .value_ty(continuation)
                .ok_or_else(|| "extern continuation lost its type".to_string())?;
            if self.value_own_kind(continuation) == Some(OwnKind::Owned) {
                self.owned_live.insert(continuation, ty);
            }
        } else if value_required {
            return Err(format!(
                "unit-valued extern `{endpoint}` cannot produce an SSA value"
            ));
        }
        Ok(continuation)
    }

    fn lower_unit_if(
        &mut self,
        condition: &HirExpr,
        then_expr: &HirExpr,
        else_expr: Option<&HirExpr>,
    ) -> Result<(), String> {
        let condition = self.lower_read_operand(condition, "if condition")?;
        let then_block = self.new_block(Vec::new());
        let else_block = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: then_block,
                args: Vec::new(),
            },
            else_target: Edge {
                target: else_block,
                args: Vec::new(),
            },
        })?;

        let before = self.control_state();
        // Both bodies are conditional paths: a loan may not end inside one.
        self.branch_depth += 1;
        self.current = then_block;
        self.lower_discarded_expr(then_expr)?;
        let then_state = self.is_open().then(|| self.control_state());

        self.restore_control_state(&before);
        self.current = else_block;
        if let Some(else_expr) = else_expr {
            self.lower_discarded_expr(else_expr)?;
        }
        let else_state = self.is_open().then(|| self.control_state());
        self.branch_depth -= 1;

        match (then_state, else_state) {
            (Some(then_state), Some(else_state)) => {
                self.merge_control_states(vec![then_state, else_state])
            }
            (Some(state), None) | (None, Some(state)) => {
                self.restore_control_state(&state);
                Ok(())
            }
            (None, None) => {
                self.current = then_block;
                Ok(())
            }
        }
    }

    fn lower_if(
        &mut self,
        whole: &HirExpr,
        condition: &HirExpr,
        then_expr: &HirExpr,
        else_expr: &HirExpr,
    ) -> Result<ValueId, String> {
        let condition = self.lower_read_operand(condition, "if condition")?;
        let then_block = self.new_block(Vec::new());
        let else_block = self.new_block(Vec::new());
        let join_ty = self.ty(&whole.ty);
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: then_block,
                args: Vec::new(),
            },
            else_target: Edge {
                target: else_block,
                args: Vec::new(),
            },
        })?;
        let before = self.control_state();
        let mut exits = Vec::new();
        self.branch_depth += 1;
        for (block, expression) in [(then_block, then_expr), (else_block, else_expr)] {
            self.restore_control_state(&before);
            self.current = block;
            let result = self.lower_selected_body(expression, &join_ty)?;
            if !self.is_open() {
                continue;
            }
            let value = result
                .ok_or("non-divergent if branch does not produce its result")?
                .value;
            self.owned_live.remove(&value);
            exits.push(MatchExit {
                state: self.control_state(),
                result: Some(Operand { value }),
            });
        }
        self.branch_depth -= 1;
        self.merge_match_exits(exits, &join_ty)?
            .ok_or_else(|| "divergent if expression cannot produce an SSA value".to_string())
    }

    /// Lower short-circuit `&&` as CFG rather than an eager binary operation.
    ///
    /// The false edge materialises the result while the true edge alone
    /// evaluates the right-hand side. This keeps effectful future SIR
    /// operations on the RHS structurally guarded from the outset.
    fn lower_logical_and(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
    ) -> Result<ValueId, String> {
        self.lower_short_circuit(whole, left, right, false)
    }

    /// Lower short-circuit `||` as CFG rather than an eager binary operation.
    fn lower_logical_or(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
    ) -> Result<ValueId, String> {
        self.lower_short_circuit(whole, left, right, true)
    }

    fn lower_short_circuit(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
        short_circuit_value: bool,
    ) -> Result<ValueId, String> {
        let result_ty = self.ty(&whole.ty);
        if result_ty != ResolvedTy::Bool {
            return Err("short-circuit logical expressions must have bool type in SIR".to_string());
        }
        let condition = self.lower_read_operand(left, "logical condition")?;
        let evaluate_right = self.new_block(Vec::new());
        let short_circuit = self.new_block(Vec::new());
        let (then_target, else_target) = if short_circuit_value {
            (short_circuit, evaluate_right)
        } else {
            (evaluate_right, short_circuit)
        };
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: then_target,
                args: Vec::new(),
            },
            else_target: Edge {
                target: else_target,
                args: Vec::new(),
            },
        })?;

        let before = self.control_state();
        let mut exits = Vec::new();
        // Both operands are conditional paths: a loan may not end inside one.
        self.branch_depth += 1;

        self.current = evaluate_right;
        let loan_floor = self.scope_loans.len();
        let right_value = self.lower_read_operand(right, "logical right value")?;
        if self.is_open() {
            // The right operand runs on one edge only. Its temporaries and
            // interior loans end on that edge, never after the join, where the
            // short-circuit edge never created them.
            if self.scope_loans.len() > loan_floor {
                let loans = self.scope_loans.split_off(loan_floor);
                self.end_call_loans(&loans)?;
            }
            self.destroy_live_since(&before.owned_live)?;
            exits.push(MatchExit {
                state: self.control_state(),
                result: Some(right_value),
            });
        }

        self.restore_control_state(&before);
        self.current = short_circuit;
        let constant = self.emit(whole, SemOpKind::ConstBool(short_circuit_value))?;
        exits.push(MatchExit {
            state: self.control_state(),
            result: Some(Operand { value: constant }),
        });

        self.branch_depth -= 1;
        self.merge_match_exits(exits, &result_ty)?
            .ok_or_else(|| "short-circuit logical expression produced no SSA value".to_string())
    }

    fn emit(&mut self, expr: &HirExpr, kind: SemOpKind) -> Result<ValueId, String> {
        self.emit_typed(Provenance::Site(expr.site), &self.ty(&expr.ty), kind)
    }

    pub(super) fn emit_typed(
        &mut self,
        provenance: Provenance,
        result_ty: &ResolvedTy,
        kind: SemOpKind,
    ) -> Result<ValueId, String> {
        if let SemOpKind::LoadTake { place } = &kind {
            // Only a local or projected owner can carry a binding's loan; a
            // capture or state place has no path here and none to end.
            if let Ok((root, _)) = crate::projection::place_path(&self.places, *place) {
                self.end_binding_loans_on(root)?;
            }
        }
        let value = self.fresh_value();
        self.service.require_type_facts(result_ty)?;
        let own = OwnKind::of_ty(result_ty, self.service.checked_facts.rows())?;
        let own = if let Some(parent) = kind.borrow_parent() {
            self.borrow_parents.insert(value, parent);
            OwnKind::Guaranteed
        } else {
            own
        };
        let op = SemOp {
            id: OpId(self.ops),
            results: vec![ValueDef {
                id: value,
                own,
                ty: result_ty.clone(),
            }],
            kind,
            provenance,
        };
        if op.results[0].own == OwnKind::Owned {
            self.owned_live.insert(value, result_ty.clone());
        }
        self.current_block_mut().append_op(op)?;
        self.ops += 1;
        Ok(value)
    }

    fn lower_checked_binary(
        &mut self,
        expr: &HirExpr,
        op: hew_parser::ast::BinaryOp,
        lhs: Operand,
        rhs: Operand,
    ) -> Result<ValueId, String> {
        let result_ty = self.ty(&expr.ty);
        let required = crate::checked_binary_failure_kinds(op, &result_ty).ok_or_else(|| {
            format!(
                "`{op}` over `{}` is not a checked integer operation",
                result_ty.user_facing()
            )
        })?;
        self.service.require_type_facts(&result_ty)?;
        let own = OwnKind::of_ty(&result_ty, self.service.checked_facts.rows())?;
        let raw_result = self.fresh_value();
        let continuation = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value: continuation,
            ty: result_ty.clone(),
            own,
        }]);
        let failure_blocks: Vec<_> = required
            .iter()
            .map(|kind| (*kind, self.new_block(Vec::new())))
            .collect();
        let failures = failure_blocks
            .iter()
            .map(|(kind, block)| CheckedFailure {
                kind: *kind,
                edge: Edge {
                    target: *block,
                    args: Vec::new(),
                },
            })
            .collect();
        let live_at_operation = self.owned_live.clone();
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::CheckedBinary {
            id,
            op,
            lhs,
            rhs,
            result: ValueDef {
                id: raw_result,
                ty: result_ty,
                own,
            },
            normal: Edge {
                target: normal,
                args: vec![Operand { value: raw_result }],
            },
            failures,
        })?;
        for (kind, block) in failure_blocks {
            self.current = block;
            self.owned_live = live_at_operation.clone();
            self.finish_checked_fault(kind)?;
        }
        self.current = normal;
        self.owned_live = live_at_operation;
        if own == OwnKind::Owned {
            self.owned_live.insert(continuation, self.ty(&expr.ty));
        }
        Ok(continuation)
    }

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.values);
        self.values += 1;
        value
    }

    fn ty(&self, ty: &ResolvedTy) -> ResolvedTy {
        self.substitution.apply(ty)
    }

    fn new_block(&mut self, args: Vec<BlockArg>) -> BlockId {
        let id = BlockId(u32::try_from(self.blocks.len()).expect("SIR block count exceeds u32"));
        self.blocks.push(PendingBlock::new(id, args));
        id
    }
    fn current_block(&self) -> &PendingBlock {
        &self.blocks[self.current.0 as usize]
    }
    fn current_block_mut(&mut self) -> &mut PendingBlock {
        &mut self.blocks[self.current.0 as usize]
    }
    fn is_open(&self) -> bool {
        self.current_block().is_open()
    }
    fn set_terminator(&mut self, term: SemTerminator) -> Result<(), String> {
        let block = self.current_block_mut();
        if block.terminator.is_some() {
            return Err(format!(
                "SIR builder attempted to overwrite completed block bb{}",
                block.id.0
            ));
        }
        block.terminator = Some(term);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{
        is_initial_value_type, require_initial_scalar_read, require_initial_value_transfer,
        PendingBlock,
    };
    use crate::ownership::{OwnKind, TypeFactTable};
    use crate::{BlockId, OpId, Provenance, SemOp, SemOpKind, SemParamPassing, SemTerminator};
    use hew_hir::IntentKind;
    use hew_types::{ResolvedTy, TypeFactContext, TypeFactService};

    #[test]
    fn only_a_read_intent_reaches_an_initial_scalar_operand() {
        assert_eq!(Ok(()), require_initial_scalar_read(IntentKind::Read));

        for intent in [
            IntentKind::Modify,
            IntentKind::Consume,
            IntentKind::Discharge,
            IntentKind::Capture,
            IntentKind::Yield,
            IntentKind::Unknown,
        ] {
            let reason = require_initial_scalar_read(intent)
                .expect_err("a non-read HIR intent must not become a scalar SIR operand");
            assert!(
                reason.contains("ownership operation")
                    || reason.contains("requires")
                    || reason.contains("not a legal"),
                "the failure must explain why {intent:?} is outside the current SIR ownership domain: {reason}"
            );
        }
    }

    #[test]
    fn scalar_and_tuple_binding_transfers_admit_only_bitcopy_values() {
        assert!(
            require_initial_value_transfer(IntentKind::Consume, &ResolvedTy::I64, "test").is_ok()
        );
        assert!(
            require_initial_value_transfer(IntentKind::Read, &ResolvedTy::Bool, "test").is_ok()
        );
        let tuple = ResolvedTy::Tuple(vec![
            ResolvedTy::I64,
            ResolvedTy::Tuple(vec![ResolvedTy::Bool]),
        ]);
        assert!(is_initial_value_type(&tuple));
        assert!(require_initial_value_transfer(IntentKind::Consume, &tuple, "test").is_ok());
        for intent in [IntentKind::Read, IntentKind::Consume] {
            let error = require_initial_value_transfer(intent, &ResolvedTy::String, "test")
                .expect_err(
                    "an ownership-bearing transfer must stay outside the SIR value-only subset",
                );
            assert!(
                error.contains("ownership-bearing")
                    && error.contains("only aliases BitCopy scalar/tuple"),
                "the transfer diagnostic must explain that this would erase ownership: {error}",
            );
        }
    }

    /// The ownership kind of every value this lowering mints comes from a
    /// published row. A missing row is never reclassified locally.
    #[test]
    fn missing_rows_are_never_reclassified_by_lowering() {
        let none = TypeFactTable::new();
        for ty in [
            ResolvedTy::I64,
            ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]),
            ResolvedTy::String,
            conn_ty(),
        ] {
            let refused =
                OwnKind::of_ty(&ty, &none).expect_err("every missing concrete row must be refused");
            assert!(
                refused.contains("concrete type facts are missing"),
                "{refused}"
            );
        }

        let mut service = TypeFactService::new(TypeFactContext::default(), none);
        service.require(&ResolvedTy::I64).unwrap();
        service.require(&ResolvedTy::Bool).unwrap();
        let tuple = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]);
        service.require(&tuple).unwrap();
        service.require(&ResolvedTy::String).unwrap();
        assert_eq!(Ok(OwnKind::None), OwnKind::of_ty(&tuple, service.rows()));
        assert_eq!(
            Ok(OwnKind::Owned),
            OwnKind::of_ty(&ResolvedTy::String, service.rows())
        );
    }

    /// The checker's row is the authority the lowering reads: a user
    /// declaration the class rule cannot reach on its own is decided by its
    /// published row, so the same type is refused without one and owning with
    /// it.
    #[test]
    fn a_published_row_decides_a_kind_the_empty_context_refuses() {
        let mut facts = TypeFactTable::new();
        facts.insert(
            hew_types::TypeInstanceKey(conn_ty()),
            hew_types::TypeFacts {
                class: hew_types::ValueClass::AffineResource,
                clone: hew_types::CloneKind::None,
                send: hew_types::SendFact::Known(true),
                hash: false,
                eq: false,
            },
        );
        assert!(OwnKind::of_ty(&conn_ty(), &TypeFactTable::new()).is_err());
        assert_eq!(Ok(OwnKind::Owned), OwnKind::of_ty(&conn_ty(), &facts));
    }

    /// §1.2 rule 3: a parameter whose header slot is `Borrow` is `Guaranteed`
    /// for the whole body whatever its type's class says, and the same type in
    /// a `ReadOnly` slot keeps the class table's kind. Without the slot read,
    /// a borrowed parameter presents as an `Owned` value the callee owes a
    /// consuming use it must never make.
    #[test]
    fn a_borrow_slot_parameter_is_guaranteed_whatever_its_class_says() {
        let none = TypeFactTable::new();
        assert_eq!(
            Ok(OwnKind::Guaranteed),
            OwnKind::of_param(&ResolvedTy::String, SemParamPassing::Borrow, &none)
        );
        assert!(OwnKind::of_param(&ResolvedTy::String, SemParamPassing::ReadOnly, &none).is_err());
        let mut service = TypeFactService::new(TypeFactContext::default(), none.clone());
        service.require(&ResolvedTy::String).unwrap();
        assert_eq!(
            Ok(OwnKind::Owned),
            OwnKind::of_param(
                &ResolvedTy::String,
                SemParamPassing::ReadOnly,
                service.rows()
            )
        );
        // The slot decides before the class rule is consulted, so a type the
        // rule cannot decide is still `Guaranteed` in a borrow slot.
        assert_eq!(
            Ok(OwnKind::Guaranteed),
            OwnKind::of_param(&conn_ty(), SemParamPassing::Borrow, &none)
        );
        assert!(OwnKind::of_param(&conn_ty(), SemParamPassing::ReadOnly, &none).is_err());
    }

    #[test]
    fn consuming_parameters_require_concrete_owners() {
        let mut service = TypeFactService::new(TypeFactContext::default(), TypeFactTable::new());
        service.require(&ResolvedTy::String).unwrap();
        service.require(&ResolvedTy::I64).unwrap();
        assert_eq!(
            OwnKind::of_param(
                &ResolvedTy::String,
                SemParamPassing::Consume,
                service.rows()
            ),
            Ok(OwnKind::Owned)
        );
        assert!(
            OwnKind::of_param(&ResolvedTy::I64, SemParamPassing::Consume, service.rows()).is_err()
        );
        assert!(OwnKind::of_param(&conn_ty(), SemParamPassing::Consume, service.rows()).is_err());
        assert_eq!(
            OwnKind::of_param(
                &ResolvedTy::String,
                SemParamPassing::BorrowMut,
                service.rows()
            ),
            Ok(OwnKind::Guaranteed)
        );
    }

    fn conn_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Conn".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }
    }

    #[test]
    fn pending_blocks_do_not_conflate_open_with_semantic_unreachable() {
        let open = PendingBlock::new(BlockId(0), Vec::new());
        assert!(open.is_open());
        assert!(open
            .into_sem_block()
            .expect_err("an unfilled builder block must fail finalization")
            .contains("without a semantic terminator"));

        let mut completed = PendingBlock::new(BlockId(1), Vec::new());
        completed.terminator = Some(SemTerminator::Unreachable);
        assert!(!completed.is_open());
        let error = completed
            .append_op(SemOp {
                id: OpId(0),
                results: Vec::new(),
                kind: SemOpKind::ConstInteger(0),
                provenance: Provenance::Synthesized,
            })
            .expect_err("semantic unreachable must close the builder block");
        assert!(error.contains("after completed block bb1"));
        assert!(matches!(
            completed
                .into_sem_block()
                .expect("semantic unreachable is a completed block")
                .terminator,
            SemTerminator::Unreachable
        ));
    }

    #[test]
    fn pending_blocks_reject_operations_after_a_semantic_terminator() {
        let mut completed = PendingBlock::new(BlockId(0), Vec::new());
        completed.terminator = Some(SemTerminator::Return { value: None });
        let error = completed
            .append_op(SemOp {
                id: OpId(0),
                results: Vec::new(),
                kind: SemOpKind::ConstInteger(0),
                provenance: Provenance::Synthesized,
            })
            .expect_err("completed blocks must reject late operations");
        assert!(error.contains("after completed block bb0"));
        assert!(completed.ops.is_empty());
    }
}
