use std::collections::{BTreeMap, HashMap, VecDeque};

use hew_hir::{
    BindingId, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmtKind,
    IntentKind, ResolvedRef,
};
use hew_types::{CallTarget, DefId, ResolvedTy, TypeCheckOutput, TypeFactService, TypeInstanceKey};

use crate::ownership::{Binding, BytesLiteralId, OwnKind, StringLiteralId, TypeFactTable};
use crate::{
    BlockArg, BlockId, CallResult, CallUnwind, CallableId, CallableInstance, CheckedFailure, Edge,
    FunctionSourceOrigin, GenericTemplateId, OpId, Operand, Provenance, SemAbiParam, SemBlock,
    SemCallConv, SemCallable, SemCallableKind, SemFunction, SemGenericTemplate, SemModule, SemOp,
    SemOpKind, SemParamPassing, SemSignature, SemTerminator, SirInstanceKey, ValueDef, ValueId,
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
    /// reported [`SirLoweringStatus::NotReached`], and so is a declaration
    /// whose header was refused, because no call can name it.
    ///
    /// WHY this shortcut exists: the strict route currently proves only the
    /// entry-reachable slice of the module, matching `hew_mir`'s closed
    /// scalar component.
    /// WHEN obsolete: the final-ladder plan's whole-module admission lands
    /// (plan §6 — "every function body lowers and verifies whether or not it
    /// is reachable from an entry; reachability is an optimization input,
    /// never an admission rule").
    /// WHAT the real fix looks like: `--sir-lower` demands
    /// [`Self::EveryCallable`] instead of `Self::Entry`, and this variant is
    /// deleted once nothing asks for entry-only demand any more.
    Entry,
    /// Demand every admitted callable header, entry or not: the coverage
    /// inventory. A refused header is reported
    /// [`SirLoweringStatus::Unsupported`] with the refusal reason, because the
    /// question asked is "would SIR take this body", not "does this program
    /// need it".
    EveryCallable,
}

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

/// Lower the SIR bodies the program actually needs.
///
/// The callable *table* is still built eagerly over every admitted
/// declaration — it is the resolved direct-call authority, and a header must
/// exist before a call can name it. Body lowering, by contrast, is
/// demand-driven: it starts at the module's entry callable and follows
/// resolved call edges, exactly like the strict component closure in
/// `hew_mir::lower_closed_scalar_component`. A declaration the entry cannot
/// reach is reported [`SirLoweringStatus::NotReached`] and costs nothing, so
/// an unsupported body in an unrelated corner of the module can neither
/// consume lowering effort nor be mistaken for a fact about this program.
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
    let mut service = InstanceService::new(module, facts, demand);
    match demand {
        SirLoweringDemand::Entry => service.request_entry(),
        SirLoweringDemand::EveryCallable => service.request_every_callable(),
    }
    service.lower_pending();

    let statuses = module
        .items
        .iter()
        .filter_map(|item| match item {
            HirItem::Function(function) => Some(SirSourceStatus {
                declaration: function.declaration.clone(),
                name: function.name.clone(),
                status: service.source_status(function),
            }),
            _ => None,
        })
        .collect();
    let callable_statuses = service.callable_statuses();
    LoweredModule {
        module: service.into_module(),
        statuses,
        callable_statuses,
    }
}

/// Deterministic SIR view of the HIR direct-call projection.
///
/// The HIR dispatcher is still the owner of exact emitted symbols.  SIR only
/// projects those checked facts into its semantic callable table; it never
/// reconstructs a symbol from a declaration's presentation spelling.
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
    templates: HashMap<DefId, GenericTemplate<'a>>,
    functions_by_item: HashMap<hew_hir::ItemId, &'a HirFn>,
    /// Why a declaration was refused a SIR callable header, keyed by the
    /// declaration a call would name.
    ///
    /// A refused declaration has no header, so no resolved call can reach it
    /// and no body is ever demanded of it. The reason is therefore reported at
    /// the call site that needed it — where it is actionable — rather than as
    /// a standing complaint about every unused declaration in the module.
    ineligible: HashMap<DefId, String>,
}

impl<'a> CallableTable<'a> {
    #[allow(
        clippy::too_many_lines,
        reason = "one deterministic HIR collection pass keeps monomorphic and generic callable admission auditable together"
    )]
    fn from_hir(module: &'a HirModule, facts: &mut TypeFactService) -> Self {
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
            let signature = match scalar_callable_signature(function, facts) {
                Ok(signature) => signature,
                Err(reason) => {
                    ineligible.insert(function.declaration.clone(), reason);
                    continue;
                }
            };
            pending.push((
                function,
                function_source_origin(module, function),
                symbol.clone(),
                signature,
            ));
        }
        pending.sort_unstable_by(|(left, _, left_symbol, _), (right, _, right_symbol, _)| {
            left.declaration
                .cmp(&right.declaration)
                .then_with(|| left_symbol.cmp(right_symbol))
                .then_with(|| left.id.cmp(&right.id))
        });

        let mut callables = Vec::with_capacity(pending.len());
        let mut root_unit_callables = Vec::new();
        let mut entry_callable = None;
        let mut monomorphic_by_declaration = HashMap::with_capacity(pending.len());
        for (index, (function, source_origin, symbol, signature)) in pending.into_iter().enumerate()
        {
            let id = CallableId(
                u32::try_from(index).expect("SIR callable count exceeds the module-local ID range"),
            );
            if source_origin == FunctionSourceOrigin::RootUnit {
                root_unit_callables.push(id);
            }
            // Entry selection joins on HIR's resolved entry declaration. SIR
            // never re-applies the language's entry rule, so it never compares
            // a declaration path or an emitted symbol against "main". A fact
            // that names a non-root declaration is admitted here and rejected
            // by the verifier's entry rule rather than silently dropped.
            if module.entry_exit_plan.as_ref().map(|plan| &plan.entry)
                == Some(&function.declaration)
            {
                entry_callable = Some(id);
            }
            monomorphic_by_declaration.insert(function.declaration.clone(), id);
            callables.push(SemCallable {
                id,
                function: function.id,
                declaration: function.declaration.clone(),
                instance: CallableInstance::Monomorphic,
                symbol,
                source_origin,
                signature,
                call_conv: SemCallConv::Default,
                kind: SemCallableKind::HewDirect,
            });
        }
        generic_templates.sort_by(|left, right| left.id.cmp(&right.id));
        Self {
            callables,
            generic_templates,
            root_unit_callables,
            entry_callable,
            entry_exit_plan: module.entry_exit_plan.clone(),
            monomorphic_by_declaration,
            templates,
            functions_by_item,
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
    demand: SirLoweringDemand,
    table: CallableTable<'a>,
    states: Vec<CallableState>,
    statuses: Vec<Option<SirLoweringStatus>>,
    by_instance: HashMap<SirInstanceKey, CallableId>,
    /// Only template headers that back a requested concrete SIR instance are
    /// emitted into the SIR module. HIR remains the authority for unselected
    /// generic definitions, so SIR does not accumulate an unrelated second
    /// template inventory.
    used_templates: std::collections::HashSet<GenericTemplateId>,
    pending: VecDeque<CallableId>,
    functions: Vec<SemFunction>,
    string_literals: BTreeMap<StringLiteralId, String>,
    bytes_literals: BTreeMap<BytesLiteralId, Vec<u8>>,
}

impl<'a> InstanceService<'a> {
    fn new(module: &'a HirModule, facts: &TypeCheckOutput, demand: SirLoweringDemand) -> Self {
        let mut checked_facts =
            TypeFactService::new(facts.type_fact_context.clone(), facts.type_facts.clone());
        let table = CallableTable::from_hir(module, &mut checked_facts);
        let count = table.callables.len();
        Self {
            module,
            checked_facts,
            demand,
            table,
            states: vec![CallableState::Unreached; count],
            statuses: vec![None; count],
            by_instance: HashMap::new(),
            used_templates: std::collections::HashSet::new(),
            pending: VecDeque::new(),
            functions: Vec::new(),
            string_literals: BTreeMap::new(),
            bytes_literals: BTreeMap::new(),
        }
    }

    fn callable(&self, id: CallableId) -> Option<&SemCallable> {
        self.table.callable(id)
    }

    fn require_type_facts(&mut self, ty: &ResolvedTy) -> Result<(), String> {
        self.checked_facts
            .require(ty)
            .map(|_| ())
            .map_err(|error| format!("type facts refused `{}`: {error}", ty.user_facing()))
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
        if let Some(entry) = self.table.entry_callable {
            self.request_body(entry);
        }
    }

    /// Seed the worklist with every admitted header, in `CallableId` order.
    ///
    /// Generic templates have no header of their own; their instances are
    /// still minted only by resolved call edges, so an uncalled template stays
    /// unproven and its status says so.
    fn request_every_callable(&mut self) {
        let ids: Vec<CallableId> = self
            .table
            .callables
            .iter()
            .map(|callable| callable.id)
            .collect();
        for id in ids {
            self.request_body(id);
        }
    }

    /// Record demand for one callable's body, once.
    fn request_body(&mut self, callable: CallableId) {
        if self.state(callable) != Some(CallableState::Unreached) {
            return;
        }
        self.set_state(callable, CallableState::Queued);
        self.pending.push_back(callable);
    }

    fn lower_pending(&mut self) {
        while let Some(callable) = self.pending.pop_front() {
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
        Builder::new(input.function, input.callable, input.substitution, self)?.lower()
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

    fn input_for_callable(&self, callable: CallableId) -> Result<LoweringInput<'a>, String> {
        let callable_meta = self.callable(callable).cloned().ok_or_else(|| {
            format!(
                "SIR callable {} is absent from its deterministic table",
                callable.0
            )
        })?;
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
        let substitution = match &callable_meta.instance {
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
            function,
            callable: callable_meta,
            substitution,
        })
    }

    fn resolve_direct_call(
        &mut self,
        declaration: &DefId,
        call_target: &CallTarget,
        site: hew_hir::SiteId,
        substitution: &TypeSubstitution,
    ) -> Result<SemCallable, String> {
        if self.table.templates.contains_key(declaration) {
            if !matches!(call_target, CallTarget::User(_)) {
                return Err(format!(
                    "generic direct callee `{}` is not an ordinary user-function call",
                    declaration.full_path()
                ));
            }
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
        let id = self
            .table
            .monomorphic_by_declaration
            .get(declaration)
            .copied()
            .ok_or_else(|| match self.table.ineligible.get(declaration) {
                Some(reason) => format!(
                    "direct callee `{}` has no scalar default-call SIR callable: {reason}",
                    declaration.full_path()
                ),
                None => format!(
                    "direct callee `{}` has no scalar default-call SIR callable",
                    declaration.full_path()
                ),
            })?;
        // Resolving a call edge is what makes the callee reachable, so this is
        // where its body becomes demanded. Generic callees go through
        // `request_instance`, which queues the instance it mints.
        self.request_body(id);
        self.callable(id)
            .cloned()
            .ok_or_else(|| format!("SIR callable {id:?} is absent from its deterministic table"))
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
            if !is_initial_scalar(argument) {
                return Err(format!(
                    "generic direct callee `{}` type argument {index} is `{}`; initial SIR generic instances require closed scalar i*/u*/bool types",
                    declaration.full_path(),
                    argument.user_facing()
                ));
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
        let signature = scalar_callable_signature_with_substitution(
            template.function,
            &substitution,
            &mut self.checked_facts,
        )?;
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
        // No admitted header: no resolved call can name this declaration, so
        // the entry closure never demanded a body from it. Under entry demand
        // the refusal belongs to the call site that wanted it; under
        // every-callable demand the refusal is the answer being asked for.
        match self.demand {
            SirLoweringDemand::Entry => SirLoweringStatus::NotReached,
            SirLoweringDemand::EveryCallable => {
                self.table.ineligible.get(&function.declaration).map_or(
                    SirLoweringStatus::NotReached,
                    |reason| SirLoweringStatus::Unsupported {
                        reason: reason.clone(),
                    },
                )
            }
        }
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
            table,
            checked_facts,
            used_templates,
            mut functions,
            string_literals,
            bytes_literals,
            ..
        } = self;
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
        );
        SemModule {
            callables: table.callables,
            generic_templates,
            root_unit_callables: table.root_unit_callables,
            entry_exit_plan: table.entry_exit_plan,
            entry_callable: table.entry_callable,
            functions,
            type_facts,
            string_literals,
            bytes_literals,
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
fn project_type_facts(
    checked: &TypeFactTable,
    callables: &[SemCallable],
    templates: &[SemGenericTemplate],
    functions: &[SemFunction],
) -> TypeFactTable {
    let mut mentioned: Vec<ResolvedTy> = Vec::new();
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
    function: &'a HirFn,
    callable: SemCallable,
    substitution: TypeSubstitution,
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
    if function.is_generator || function.intrinsic_id.is_some() {
        return Err(
            "generators and floor intrinsics remain outside SIR's ordinary direct-call domain"
                .to_string(),
        );
    }
    for (index, parameter) in function.params.iter().enumerate() {
        if parameter.is_consume {
            return Err(format!(
                "parameter {index} is consume-owned; SIR direct calls initially require Read operands"
            ));
        }
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
                passing: SemParamPassing::ReadOnly,
                caller_visible_projection: false,
            })
            .collect(),
        return_ty: function.return_ty.clone(),
    })
}

fn scalar_callable_signature(
    function: &HirFn,
    facts: &mut TypeFactService,
) -> Result<SemSignature, String> {
    if !function.type_params.is_empty() {
        return Err(
            "generic origin functions are instantiated by the SIR instance service, not admitted as abstract callable bodies"
                .to_string(),
        );
    }
    scalar_callable_signature_with_substitution(function, &TypeSubstitution::empty(), facts)
}

fn scalar_callable_signature_with_substitution(
    function: &HirFn,
    substitution: &TypeSubstitution,
    facts: &mut TypeFactService,
) -> Result<SemSignature, String> {
    generic_template_admission(function)?;
    let mut params = Vec::with_capacity(function.params.len());
    for (index, parameter) in function.params.iter().enumerate() {
        let ty = substitution.apply(&parameter.ty);
        if !is_initial_call_value(&ty) {
            return Err(format!(
                "parameter {index} has non-scalar type `{}` after semantic substitution; aggregate/reference ABI lowering is deferred",
                ty.user_facing()
            ));
        }
        let row = facts
            .require(&ty)
            .map_err(|error| format!("type facts refused `{}`: {error}", ty.user_facing()))?;
        params.push(SemAbiParam {
            ty,
            passing: if OwnKind::of_class(row.class) == OwnKind::Owned {
                SemParamPassing::Borrow
            } else {
                SemParamPassing::ReadOnly
            },
            caller_visible_projection: false,
        });
    }
    let return_ty = substitution.apply(&function.return_ty);
    if !is_initial_call_return(&return_ty) {
        return Err(format!(
            "return type `{}` is outside SIR's initial scalar call-result domain after semantic substitution",
            return_ty.user_facing()
        ));
    }
    Ok(SemSignature { params, return_ty })
}

fn is_initial_scalar(ty: &ResolvedTy) -> bool {
    ty.is_integer() || matches!(ty, hew_types::ResolvedTy::Bool)
}

fn is_initial_call_value(ty: &ResolvedTy) -> bool {
    is_initial_scalar(ty) || matches!(ty, ResolvedTy::String | ResolvedTy::Bytes)
}

fn is_initial_call_return(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Unit) || is_initial_call_value(ty)
}

/// The first aggregate value family admitted into SIR.
///
/// These values remain purely semantic until Raw MIR decides whether a
/// representation boundary requires storage. Restricting tuple leaves to the
/// existing `BitCopy` scalar domain keeps this first slice free of drops,
/// borrowing, reference counts, and layout-dependent behavior.
fn is_initial_value_type(ty: &ResolvedTy) -> bool {
    is_initial_scalar(ty)
        || matches!(ty, ResolvedTy::Tuple(elements)
            if !elements.is_empty() && elements.iter().all(is_initial_value_type))
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
/// to a new binding or the caller. For `i*`/`u*`/`bool`, that semantic transfer
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
    if is_initial_value_type(&ty) {
        require_initial_value_transfer(expr.intent, &ty, context)?;
        return builder.lower_expr(expr);
    }
    if !matches!(ty, ResolvedTy::String | ResolvedTy::Bytes)
        || !matches!(expr.intent, IntentKind::Read | IntentKind::Consume)
    {
        return Err(format!(
            "{context}: HIR {:?} intent cannot transfer `{}` in the owned SIR slice",
            expr.intent,
            ty.user_facing()
        ));
    }
    builder.lower_owned_transfer(expr, binding_use)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OwnedBindingUse {
    Copy,
    Move,
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
    if !matches!(expr.kind, HirExprKind::Call { .. }) {
        return Err(
            "unit return values are initially supported only for a resolved direct call"
                .to_string(),
        );
    }
    builder.lower_direct_call(expr, false).map(|_| ())
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

struct Builder<'hir, 'service> {
    function: &'hir HirFn,
    service: &'service mut InstanceService<'hir>,
    callable: SemCallable,
    substitution: TypeSubstitution,
    blocks: Vec<PendingBlock>,
    current: BlockId,
    values: u32,
    ops: u32,
    bindings: HashMap<BindingId, ValueId>,
    binding_declarations: HashMap<BindingId, usize>,
    owned_live: BTreeMap<ValueId, ResolvedTy>,
    /// Every source binding this body declares, parameters first and then
    /// statement bindings in source order (§1.6).
    source_bindings: Vec<Binding>,
    params: Vec<BlockArg>,
}

impl<'hir, 'service> Builder<'hir, 'service> {
    fn new(
        function: &'hir HirFn,
        callable: SemCallable,
        substitution: TypeSubstitution,
        service: &'service mut InstanceService<'hir>,
    ) -> Result<Self, String> {
        if function.params.len() != callable.signature.params.len() {
            return Err(format!(
                "SIR callable `{}` has {} parameter ABI facts, but its HIR template has {} parameter(s)",
                callable.symbol,
                callable.signature.params.len(),
                function.params.len()
            ));
        }
        let entry = BlockId(0);
        let mut values = 0;
        let mut bindings = HashMap::new();
        let params = function
            .params
            .iter()
            .zip(&callable.signature.params)
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
                bindings.insert(param.id, value);
                // §1.2 rule 3: the header slot decides before the type's
                // class does. A `Borrow` slot makes the parameter
                // `Guaranteed` for the whole body, so a consuming use of it is
                // rule 3's `E_OWN_CONSUME_BORROWED` wall and not rule 1's
                // leak. No lowering emits that slot yet, so every parameter
                // here takes the class table's answer.
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
        let (params, source_bindings): (Vec<BlockArg>, Vec<Binding>) = params.into_iter().unzip();
        let binding_declarations = function
            .params
            .iter()
            .enumerate()
            .map(|(index, param)| (param.id, index))
            .collect();
        Ok(Self {
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
            owned_live: BTreeMap::new(),
            source_bindings,
            params,
        })
    }

    fn lower(mut self) -> Result<SemFunction, String> {
        if self.callable.function != self.function.id
            || self.callable.declaration != self.function.declaration
        {
            return Err(
                "SIR callable provenance does not match the HIR function's checked identity"
                    .to_string(),
            );
        }
        if self.function.is_generator || self.function.intrinsic_id.is_some() {
            return Err(
                "generators and floor intrinsics remain on the established MIR path".to_string(),
            );
        }
        match (
            &self.callable.instance,
            self.function.type_params.is_empty(),
        ) {
            (CallableInstance::Monomorphic, true) => {}
            (CallableInstance::Generic(key), false)
                if key.template.declaration == self.function.declaration
                    && key.type_args == self.substitution.args => {}
            _ => return Err(
                "SIR callable instance does not match its HIR template and semantic substitution"
                    .to_string(),
            ),
        }
        if self.callable.signature.return_ty != self.ty(&self.function.return_ty) {
            return Err(format!(
                "SIR callable `{}` return type `{}` differs from substituted HIR template return `{}`",
                self.callable.symbol,
                self.callable.signature.return_ty.user_facing(),
                self.ty(&self.function.return_ty).user_facing()
            ));
        }
        let result = self.lower_block(&self.function.body, OwnedBindingUse::Move)?;
        if self.is_open() {
            if let Some(result) = &result {
                self.owned_live.remove(&result.value);
            }
            self.destroy_all_live()?;
            self.set_terminator(SemTerminator::Return {
                value: result.map(|operand| crate::BoundaryOperand {
                    operand,
                    decision: crate::BoundaryDecision::Move,
                }),
            })?;
        }
        let blocks = std::mem::take(&mut self.blocks)
            .into_iter()
            .map(PendingBlock::into_sem_block)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(SemFunction {
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
            // No P1 program produces a function-owned place: HIR-to-SIR
            // construction does mem2reg and the only escape hatch, an extern
            // `&`/`&mut` on a local, has no producer on this route yet.
            places: Vec::new(),
            bindings: self.source_bindings,
        })
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

    fn lower_owned_transfer(
        &mut self,
        expr: &HirExpr,
        binding_use: OwnedBindingUse,
    ) -> Result<ValueId, String> {
        let source = self.lower_expr_with_binding_use(expr, binding_use)?;
        let ty = self.ty(&expr.ty);
        let own = OwnKind::of_ty(&ty, self.service.checked_facts.rows())?;
        if own != OwnKind::Owned {
            return Ok(source);
        }
        let source_kind = self.value_own_kind(source);
        match source_kind {
            Some(OwnKind::Guaranteed) => self.emit(
                expr,
                SemOpKind::CopyValue {
                    source: Operand { value: source },
                },
            ),
            Some(OwnKind::Owned) if matches!(expr.kind, HirExprKind::BindingRef { .. }) => {
                match binding_use {
                    OwnedBindingUse::Copy => self.emit(
                        expr,
                        SemOpKind::CopyValue {
                            source: Operand { value: source },
                        },
                    ),
                    OwnedBindingUse::Move => {
                        self.owned_live.remove(&source);
                        self.emit(
                            expr,
                            SemOpKind::Move {
                                source: Operand { value: source },
                            },
                        )
                    }
                }
            }
            Some(OwnKind::Owned) => Ok(source),
            _ => Err(format!(
                "owned transfer of `{}` has no owning or guaranteed source",
                ty.user_facing()
            )),
        }
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

    fn mutable_bindings(&self) -> Vec<BindingId> {
        let mut bindings: Vec<_> = self
            .binding_declarations
            .iter()
            .filter_map(|(binding, &index)| self.source_bindings[index].mutable.then_some(*binding))
            .collect();
        bindings.sort_unstable();
        bindings
    }

    fn emit_destroy(&mut self, value: ValueId) -> Result<(), String> {
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
        Ok(())
    }

    fn destroy_all_live(&mut self) -> Result<(), String> {
        let values: Vec<_> = self.owned_live.keys().copied().collect();
        for value in values.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        Ok(())
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
                    let value = value
                        .as_ref()
                        .map(|expr| {
                            lower_initial_value_transfer(
                                self,
                                expr,
                                "binding initializer",
                                OwnedBindingUse::Copy,
                            )
                        })
                        .transpose()?
                        .ok_or_else(|| {
                            "uninitialised bindings are not in the initial SIR subset".to_string()
                        })?;
                    // §1.6: the value a binding names carries the binding's
                    // name, span and mutability, so a rule 2, 3, 4 or 6
                    // violation rooted in it renders its `E_OWN_*` code rather
                    // than `E_SIR_ICE`, and rule 6a has a mutability bit to
                    // read. A `let` aliases the SSA value its initializer
                    // produced rather than defining one of its own, so the
                    // provenance lands on that definition — and only when it
                    // has none, because `let y = x` must not rename the
                    // parameter `x` already named.
                    let declaration = self.source_bindings.len();
                    self.source_bindings.push(Binding {
                        id: crate::BindingId(
                            u32::try_from(self.source_bindings.len())
                                .map_err(|_| "SIR source binding count exceeds u32".to_string())?,
                        ),
                        name: binding.name.clone(),
                        span: binding.span.clone(),
                        mutable: binding.mutable,
                        target: crate::BindingTarget::Value(value),
                    });
                    self.binding_declarations.insert(binding.id, declaration);
                    self.bindings.insert(binding.id, value);
                }
                HirStmtKind::Expr(expr) => {
                    self.lower_discarded_expr(expr)?;
                }
                HirStmtKind::Return(value) => {
                    let value = match value {
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
                                    OwnedBindingUse::Move,
                                )?,
                            },
                            decision: crate::BoundaryDecision::Move,
                        }),
                        None => None,
                    };
                    if let Some(value) = &value {
                        self.owned_live.remove(&value.operand.value);
                    }
                    self.destroy_all_live()?;
                    self.set_terminator(SemTerminator::Return { value })?;
                }
                HirStmtKind::Assign { target, value } => {
                    self.lower_assignment(target, value)?;
                }
                HirStmtKind::LetElse { .. } | HirStmtKind::Defer { .. } => {
                    return Err(
                        "control-flow ownership forms are deferred to a later SIR slice"
                            .to_string(),
                    )
                }
            }
        }
        if self.is_open() {
            match block.tail.as_deref() {
                Some(expr) if self.ty(&expr.ty) == ResolvedTy::Unit => {
                    self.lower_discarded_expr(expr)?;
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
        let outer: std::collections::HashSet<_> = self.bindings.keys().copied().collect();
        let result = self.lower_block(block, tail_binding_use)?;
        if self.is_open() {
            let locals: Vec<_> = self
                .bindings
                .keys()
                .filter(|binding| !outer.contains(binding))
                .copied()
                .collect();
            for binding in locals.into_iter().rev() {
                if let Some(value) = self.bindings.remove(&binding) {
                    if self.owned_live.contains_key(&value) {
                        self.emit_destroy(value)?;
                    }
                }
                self.binding_declarations.remove(&binding);
            }
        }
        Ok(result)
    }

    fn lower_assignment(&mut self, target: &HirExpr, value: &HirExpr) -> Result<(), String> {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &target.kind
        else {
            return Err(
                "owned SIR assignment currently requires a resolved local binding target"
                    .to_string(),
            );
        };
        let binding = *binding;
        let declaration = *self
            .binding_declarations
            .get(&binding)
            .ok_or_else(|| format!("assignment target `{binding}` has no source declaration"))?;
        if !self.source_bindings[declaration].mutable {
            return Err(format!("assignment target `{binding}` is not mutable"));
        }
        let old = *self.bindings.get(&binding).ok_or_else(|| {
            format!("assignment target `{binding}` is not available in the SIR environment")
        })?;
        let old_ty = self
            .value_ty(old)
            .ok_or_else(|| format!("assignment target `{binding}` has no concrete SIR type"))?;
        let new_ty = self.ty(&value.ty);
        if old_ty != new_ty {
            return Err(format!(
                "assignment target `{binding}` has `{}`, but its value has `{}`",
                old_ty.user_facing(),
                new_ty.user_facing()
            ));
        }
        let new =
            lower_initial_value_transfer(self, value, "assignment value", OwnedBindingUse::Copy)?;
        if self.owned_live.contains_key(&old) {
            self.emit_destroy(old)?;
        }
        self.bindings.insert(binding, new);
        self.record_binding_version(binding, new)
    }

    /// Lower an expression whose value is intentionally discarded.
    ///
    /// Scalar expressions keep their ordinary one-result SSA operation even
    /// when the result is unused.  A unit direct call is different: there is
    /// no semantic value to define, but the call itself must remain in SIR so
    /// later lowering can realize its call/continuation CFG edge.
    fn lower_discarded_expr(&mut self, expr: &HirExpr) -> Result<(), String> {
        require_initial_scalar_read(expr.intent)
            .map_err(|reason| format!("discarded expression: {reason}"))?;
        let live_before_expression: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        match &expr.kind {
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
            } if self.ty(&expr.ty) == ResolvedTy::Unit => {
                return self.lower_unit_if(condition, then_expr, else_expr.as_deref());
            }
            HirExprKind::While {
                label,
                condition,
                body,
            } => {
                if label.is_some() {
                    return Err(
                        "labelled while loops are not yet in the owned SIR slice".to_string()
                    );
                }
                return self.lower_while(condition, body);
            }
            _ => {}
        }
        if matches!(expr.kind, HirExprKind::Call { .. }) {
            if let Some(value) = self.lower_direct_call(expr, false)? {
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

    #[allow(
        clippy::too_many_lines,
        reason = "the closed initial HIR-to-SIR expression mapping remains intentionally local"
    )]
    fn lower_expr(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        self.lower_expr_with_binding_use(expr, OwnedBindingUse::Copy)
    }

    fn lower_expr_with_binding_use(
        &mut self,
        expr: &HirExpr,
        binding_use: OwnedBindingUse,
    ) -> Result<ValueId, String> {
        match &expr.kind {
            HirExprKind::Literal(literal) => self.lower_literal(expr, literal),
            HirExprKind::TupleLiteral { elements } => self.lower_tuple_make(expr, elements),
            HirExprKind::TupleIndex { tuple, index } => self.lower_tuple_get(expr, tuple, *index),
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } => {
                let value = self.bindings.get(binding).copied().ok_or_else(|| {
                    format!("binding `{binding}` is not available in the SIR environment")
                })?;
                if self.value_own_kind(value) == Some(OwnKind::Owned)
                    && !self.owned_live.contains_key(&value)
                {
                    return Err(format!(
                        "binding `{binding}` names a consumed owned SIR value"
                    ));
                }
                Ok(value)
            }
            HirExprKind::Unary { op, operand, .. } => {
                let value = self.lower_read_operand(operand, "unary operand")?;
                if *op == hew_parser::ast::UnaryOp::Negate && self.ty(&expr.ty).is_signed_integer()
                {
                    let zero = self.emit(expr, SemOpKind::ConstI64(0))?;
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
            HirExprKind::Call { .. } => self.lower_direct_call(expr, true)?.ok_or_else(|| {
                "unit-valued direct calls are valid only in a discarded or unit-return context"
                    .to_string()
            }),
            HirExprKind::Block(block) => self
                .lower_scoped_block(block, binding_use)?
                .map(|value| value.value)
                .ok_or_else(|| "a divergent block cannot produce a SIR value".to_string()),
            HirExprKind::If {
                condition,
                then_expr,
                else_expr: Some(else_expr),
            } => self.lower_if(expr, condition, then_expr, else_expr),
            HirExprKind::If {
                else_expr: None, ..
            } => Err(
                "one-armed if expressions are deferred until unit values are modeled".to_string(),
            ),
            _ => Err("unsupported HIR expression kind in the initial SIR subset".to_string()),
        }
    }

    fn lower_literal(&mut self, expr: &HirExpr, literal: &HirLiteral) -> Result<ValueId, String> {
        match literal {
            HirLiteral::Integer(value) => {
                if !self.ty(&expr.ty).is_integer() {
                    return Err(format!(
                        "integer literal resolved as `{}` needs a dedicated SIR literal representation",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstI64(*value))
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
            HirLiteral::String(value) => {
                let literal = self.service.intern_string(value);
                self.emit(expr, SemOpKind::ConstStr(literal))
            }
            HirLiteral::Bytes(value) => {
                let literal = self.service.intern_bytes(value);
                self.emit(expr, SemOpKind::ConstBytes(literal))
            }
            _ => Err("unsupported HIR literal kind in the initial SIR subset".to_string()),
        }
    }

    /// Lower an immutable, no-drop tuple as one semantic aggregate value.
    ///
    /// The tuple's exact `ResolvedTy` is retained; neither its field layout nor
    /// an addressable temporary is introduced at the HIR → SIR boundary.
    fn lower_tuple_make(
        &mut self,
        expr: &HirExpr,
        elements: &[HirExpr],
    ) -> Result<ValueId, String> {
        let tuple_ty = self.ty(&expr.ty);
        let ResolvedTy::Tuple(expected_elements) = &tuple_ty else {
            return Err(format!(
                "tuple literal has non-tuple resolved type `{}` in SIR lowering",
                tuple_ty.user_facing()
            ));
        };
        if !is_initial_value_type(&tuple_ty) {
            return Err(format!(
                "tuple literal type `{}` is outside SIR's initial no-drop scalar/tuple value domain",
                tuple_ty.user_facing()
            ));
        }
        if expected_elements.len() != elements.len() {
            return Err(format!(
                "tuple literal has {} element(s), but its resolved type `{}` has {} element type(s)",
                elements.len(),
                tuple_ty.user_facing(),
                expected_elements.len()
            ));
        }
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
            lowered_elements
                .push(self.lower_read_operand(element, &format!("tuple literal element {index}"))?);
        }
        self.emit(
            expr,
            SemOpKind::TupleMake {
                elements: lowered_elements,
            },
        )
    }

    /// Lower a semantic tuple projection without exposing aggregate layout.
    fn lower_tuple_get(
        &mut self,
        expr: &HirExpr,
        tuple_expr: &HirExpr,
        index: usize,
    ) -> Result<ValueId, String> {
        let tuple_ty = self.ty(&tuple_expr.ty);
        let ResolvedTy::Tuple(elements) = &tuple_ty else {
            return Err(format!(
                "tuple projection has non-tuple operand type `{}` in SIR lowering",
                tuple_ty.user_facing()
            ));
        };
        if !is_initial_value_type(&tuple_ty) {
            return Err(format!(
                "tuple projection operand type `{}` is outside SIR's initial no-drop scalar/tuple value domain",
                tuple_ty.user_facing()
            ));
        }
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
        let tuple = self.lower_read_operand(tuple_expr, "tuple projection operand")?;
        self.emit(expr, SemOpKind::TupleGet { tuple, index })
    }

    /// Lower an HIR direct call through the resolved SIR callable table.
    ///
    /// `value_required` distinguishes a value context from a discarded/unit
    /// context.  Non-unit calls always retain their single SSA result; unit
    /// calls are admitted only in the latter and become zero-result `Call`
    /// operations.
    #[allow(
        clippy::too_many_lines,
        reason = "the initial direct-call ABI admission is deliberately kept as one auditable HIR-to-SIR boundary"
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
            return Err(
                "internal SIR lowering error: direct-call lowering received a non-call".to_string(),
            );
        };
        let declaration =
            match target {
                CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => declaration,
                CallTarget::IndirectFunctionValue => {
                    return Err(
                        "indirect calls are deferred until SIR models the callee value explicitly"
                            .to_string(),
                    )
                }
                _ => return Err(
                    "only ordinary user/impl direct calls are in SIR's initial scalar call domain"
                        .to_string(),
                ),
            };
        if !matches!(callee.kind, HirExprKind::BindingRef { .. }) {
            return Err(
                "calls with an evaluated callee are deferred until SIR models callee values"
                    .to_string(),
            );
        }
        let callee =
            self.service
                .resolve_direct_call(declaration, target, expr.site, &self.substitution)?;
        let callee_id = callee.id;
        let callee_declaration = callee.declaration.clone();
        let params = callee.signature.params.clone();
        let return_ty = callee.signature.return_ty.clone();
        if args.len() != params.len() {
            return Err(format!(
                "direct callee `{}` expects {} argument(s), HIR carries {}",
                callee_declaration.full_path(),
                params.len(),
                args.len()
            ));
        }
        let expression_ty = self.ty(&expr.ty);
        if expression_ty != return_ty {
            return Err(format!(
                "direct callee `{}` returns `{}`, but call expression has `{}`",
                callee_declaration.full_path(),
                return_ty.user_facing(),
                expression_ty.user_facing()
            ));
        }
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut lowered_args = Vec::with_capacity(args.len());
        for (index, (arg, expected)) in args.iter().zip(&params).enumerate() {
            let argument_ty = self.ty(&arg.ty);
            if argument_ty != expected.ty {
                return Err(format!(
                    "direct call argument {index} to `{}` has `{}`, expected `{}`",
                    callee_declaration.full_path(),
                    argument_ty.user_facing(),
                    expected.ty.user_facing()
                ));
            }
            lowered_args.push(crate::BoundaryOperand {
                operand: self.lower_read_operand(
                    arg,
                    &format!(
                        "direct call argument {index} to `{}`",
                        callee_declaration.full_path()
                    ),
                )?,
                decision: match expected.passing {
                    SemParamPassing::ReadOnly => crate::BoundaryDecision::Copy,
                    SemParamPassing::Borrow => crate::BoundaryDecision::Borrow,
                },
            });
        }
        let live_at_call = self.owned_live.clone();
        let argument_temporaries: Vec<_> = live_at_call
            .keys()
            .filter(|value| !live_before_arguments.contains(value))
            .copied()
            .collect();
        if return_ty == ResolvedTy::Unit {
            if value_required {
                return Err(format!(
                    "unit-valued direct call `{}` cannot produce an SSA value",
                    callee_declaration.full_path()
                ));
            }
            let normal = self.new_block(Vec::new());
            let unwind = self.new_block(Vec::new());
            let id = OpId(self.ops);
            self.ops += 1;
            self.set_terminator(SemTerminator::Call {
                id,
                callee: callee_id,
                args: lowered_args,
                result: CallResult::Unit,
                normal: Edge {
                    target: normal,
                    args: Vec::new(),
                },
                unwind: CallUnwind::Cleanup(Edge {
                    target: unwind,
                    args: Vec::new(),
                }),
            })?;
            self.current = unwind;
            self.owned_live = live_at_call.clone();
            self.destroy_all_live()?;
            self.set_terminator(SemTerminator::ResumeUnwind)?;
            self.current = normal;
            self.owned_live = live_at_call;
            for value in argument_temporaries.into_iter().rev() {
                self.emit_destroy(value)?;
            }
            Ok(None)
        } else {
            let result = self.fresh_value();
            let continuation = self.fresh_value();
            self.service.require_type_facts(&return_ty)?;
            let own = OwnKind::of_ty(&return_ty, self.service.checked_facts.rows())?;
            let normal = self.new_block(vec![BlockArg {
                value: continuation,
                own,
                ty: return_ty.clone(),
            }]);
            let unwind = self.new_block(Vec::new());
            let id = OpId(self.ops);
            self.ops += 1;
            self.set_terminator(SemTerminator::Call {
                id,
                callee: callee_id,
                args: lowered_args,
                result: CallResult::Value(ValueDef {
                    id: result,
                    own,
                    ty: return_ty,
                }),
                normal: Edge {
                    target: normal,
                    args: vec![Operand { value: result }],
                },
                unwind: CallUnwind::Cleanup(Edge {
                    target: unwind,
                    args: Vec::new(),
                }),
            })?;
            self.current = unwind;
            self.owned_live = live_at_call.clone();
            self.destroy_all_live()?;
            self.set_terminator(SemTerminator::ResumeUnwind)?;
            self.current = normal;
            self.owned_live = live_at_call;
            for value in argument_temporaries.into_iter().rev() {
                self.emit_destroy(value)?;
            }
            if own == OwnKind::Owned {
                self.owned_live.insert(continuation, self.ty(&expr.ty));
            }
            Ok(Some(continuation))
        }
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

        let before_bindings = self.bindings.clone();
        let before_live = self.owned_live.clone();
        self.current = then_block;
        self.bindings = before_bindings.clone();
        self.owned_live = before_live.clone();
        self.lower_discarded_expr(then_expr)?;
        let then_state = self
            .is_open()
            .then(|| (self.current, self.bindings.clone(), self.owned_live.clone()));

        self.current = else_block;
        self.bindings = before_bindings;
        self.owned_live = before_live;
        if let Some(else_expr) = else_expr {
            self.lower_discarded_expr(else_expr)?;
        }
        let else_state = self
            .is_open()
            .then(|| (self.current, self.bindings.clone(), self.owned_live.clone()));

        match (then_state, else_state) {
            (Some(then_state), Some(else_state)) => {
                self.merge_unit_branches(then_state, else_state)
            }
            (Some((block, bindings, live)), None) | (None, Some((block, bindings, live))) => {
                self.current = block;
                self.bindings = bindings;
                self.owned_live = live;
                Ok(())
            }
            (None, None) => {
                self.current = then_block;
                Ok(())
            }
        }
    }

    fn merge_unit_branches(
        &mut self,
        then_state: (
            BlockId,
            HashMap<BindingId, ValueId>,
            BTreeMap<ValueId, ResolvedTy>,
        ),
        else_state: (
            BlockId,
            HashMap<BindingId, ValueId>,
            BTreeMap<ValueId, ResolvedTy>,
        ),
    ) -> Result<(), String> {
        let (then_block, then_bindings, mut then_live) = then_state;
        let (else_block, else_bindings, mut else_live) = else_state;
        let then_keys: std::collections::HashSet<_> = then_bindings.keys().copied().collect();
        let else_keys: std::collections::HashSet<_> = else_bindings.keys().copied().collect();
        if then_keys != else_keys {
            return Err("if branches expose different source bindings at their join".to_string());
        }
        let mut args = Vec::new();
        let mut then_args = Vec::new();
        let mut else_args = Vec::new();
        let mut joined_bindings = then_bindings.clone();
        let mut joined_owners = Vec::new();

        for binding in self.mutable_bindings() {
            let Some(&then_value) = then_bindings.get(&binding) else {
                continue;
            };
            let else_value = *else_bindings.get(&binding).ok_or_else(|| {
                format!("mutable binding `{binding}` is missing from one if branch")
            })?;
            let ty = self.value_ty(then_value).ok_or_else(|| {
                format!("mutable binding `{binding}` has no concrete type at its if join")
            })?;
            if self.value_ty(else_value).as_ref() != Some(&ty) {
                return Err(format!(
                    "mutable binding `{binding}` has mismatched types at its if join"
                ));
            }
            self.service.require_type_facts(&ty)?;
            let own = OwnKind::of_ty(&ty, self.service.checked_facts.rows())?;
            let joined = self.fresh_value();
            args.push(BlockArg {
                value: joined,
                own,
                ty: ty.clone(),
            });
            then_args.push(Operand { value: then_value });
            else_args.push(Operand { value: else_value });
            joined_bindings.insert(binding, joined);
            then_live.remove(&then_value);
            else_live.remove(&else_value);
            if own == OwnKind::Owned {
                joined_owners.push((joined, ty));
            }
            self.record_binding_version(binding, joined)?;
        }
        if then_live != else_live {
            return Err(
                "if branches leave different non-binding owned values live at their join"
                    .to_string(),
            );
        }
        let join = self.new_block(args);
        self.current = then_block;
        self.set_terminator(SemTerminator::Goto(Edge {
            target: join,
            args: then_args,
        }))?;
        self.current = else_block;
        self.set_terminator(SemTerminator::Goto(Edge {
            target: join,
            args: else_args,
        }))?;
        self.current = join;
        self.bindings = joined_bindings;
        self.owned_live = then_live;
        self.owned_live.extend(joined_owners);
        Ok(())
    }

    fn lower_while(&mut self, condition: &HirExpr, body: &HirBlock) -> Result<(), String> {
        let preheader = self.current;
        let before_bindings = self.bindings.clone();
        let mut header_bindings = before_bindings.clone();
        let mut header_live = self.owned_live.clone();
        let mut header_args = Vec::new();
        let mut entry_args = Vec::new();
        let mut carried = Vec::new();
        for binding in self.mutable_bindings() {
            let Some(&source) = before_bindings.get(&binding) else {
                continue;
            };
            let ty = self.value_ty(source).ok_or_else(|| {
                format!("mutable binding `{binding}` has no concrete type at its loop header")
            })?;
            self.service.require_type_facts(&ty)?;
            let own = OwnKind::of_ty(&ty, self.service.checked_facts.rows())?;
            if own == OwnKind::Guaranteed {
                return Err(format!(
                    "borrowed parameter `{binding}` cannot be a loop-carried mutable owner"
                ));
            }
            let header_value = self.fresh_value();
            header_args.push(BlockArg {
                value: header_value,
                own,
                ty: ty.clone(),
            });
            entry_args.push(Operand { value: source });
            header_bindings.insert(binding, header_value);
            header_live.remove(&source);
            if own == OwnKind::Owned {
                header_live.insert(header_value, ty);
            }
            carried.push(binding);
            self.record_binding_version(binding, header_value)?;
        }

        let header = self.new_block(header_args);
        self.current = preheader;
        self.set_terminator(SemTerminator::Goto(Edge {
            target: header,
            args: entry_args,
        }))?;
        self.current = header;
        self.bindings = header_bindings.clone();
        self.owned_live = header_live.clone();
        let condition = self.lower_read_operand(condition, "while condition")?;
        let body_block = self.new_block(Vec::new());
        let exit = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: body_block,
                args: Vec::new(),
            },
            else_target: Edge {
                target: exit,
                args: Vec::new(),
            },
        })?;

        self.current = body_block;
        self.bindings = header_bindings.clone();
        self.owned_live = header_live.clone();
        let tail = self.lower_scoped_block(body, OwnedBindingUse::Copy)?;
        if let Some(tail) = tail {
            if self.owned_live.contains_key(&tail.value) {
                self.emit_destroy(tail.value)?;
            }
        }
        if self.is_open() {
            let back_args = carried
                .iter()
                .map(|binding| {
                    self.bindings
                        .get(binding)
                        .copied()
                        .map(|value| Operand { value })
                        .ok_or_else(|| {
                            format!("loop-carried binding `{binding}` is missing on its back edge")
                        })
                })
                .collect::<Result<Vec<_>, _>>()?;
            self.set_terminator(SemTerminator::Goto(Edge {
                target: header,
                args: back_args,
            }))?;
        }

        self.current = exit;
        self.bindings = header_bindings;
        self.owned_live = header_live;
        Ok(())
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
        let join_value = self.fresh_value();
        let join_ty = self.ty(&whole.ty);
        if !is_initial_value_type(&join_ty) {
            return Err(format!(
                "value-producing if over `{}` is outside the current SIR value domain",
                join_ty.user_facing()
            ));
        }
        self.service.require_type_facts(&join_ty)?;
        let join_own = OwnKind::of_ty(&join_ty, self.service.checked_facts.rows())?;
        let join_block = self.new_block(vec![BlockArg {
            value: join_value,
            own: join_own,
            ty: join_ty,
        }]);
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
        let before = self.bindings.clone();
        self.current = then_block;
        self.bindings = before.clone();
        let then_value = self.lower_read_operand(then_expr, "if then value")?;
        if self.is_open() {
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join_block,
                args: vec![then_value],
            }))?;
        }
        self.current = else_block;
        self.bindings = before;
        let else_value = self.lower_read_operand(else_expr, "if else value")?;
        if self.is_open() {
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join_block,
                args: vec![else_value],
            }))?;
        }
        self.current = join_block;
        Ok(join_value)
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
        if self.ty(&whole.ty) != ResolvedTy::Bool {
            return Err("short-circuit logical expressions must have bool type in SIR".to_string());
        }
        let condition = self.lower_read_operand(left, "logical condition")?;
        let evaluate_right = self.new_block(Vec::new());
        let short_circuit = self.new_block(Vec::new());
        let result = self.fresh_value();
        let join_ty = self.ty(&whole.ty);
        self.service.require_type_facts(&join_ty)?;
        let join_own = OwnKind::of_ty(&join_ty, self.service.checked_facts.rows())?;
        let join = self.new_block(vec![BlockArg {
            value: result,
            own: join_own,
            ty: join_ty,
        }]);
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

        let before = self.bindings.clone();
        self.current = evaluate_right;
        self.bindings = before.clone();
        let right_value = self.lower_read_operand(right, "logical right value")?;
        if self.is_open() {
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join,
                args: vec![right_value],
            }))?;
        }

        self.current = short_circuit;
        self.bindings = before;
        let constant = self.emit(whole, SemOpKind::ConstBool(short_circuit_value))?;
        self.set_terminator(SemTerminator::Goto(Edge {
            target: join,
            args: vec![Operand { value: constant }],
        }))?;

        self.current = join;
        Ok(result)
    }

    fn emit(&mut self, expr: &HirExpr, kind: SemOpKind) -> Result<ValueId, String> {
        let value = self.fresh_value();
        let result_ty = self.ty(&expr.ty);
        self.service.require_type_facts(&result_ty)?;
        let op = SemOp {
            id: OpId(self.ops),
            results: vec![ValueDef {
                id: value,
                own: OwnKind::of_ty(&result_ty, self.service.checked_facts.rows())?,
                ty: result_ty.clone(),
            }],
            kind,
            provenance: Provenance::Site(expr.site),
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
            self.destroy_all_live()?;
            self.set_terminator(SemTerminator::Trap { kind })?;
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
                kind: SemOpKind::ConstI64(0),
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
                kind: SemOpKind::ConstI64(0),
                provenance: Provenance::Synthesized,
            })
            .expect_err("completed blocks must reject late operations");
        assert!(error.contains("after completed block bb0"));
        assert!(completed.ops.is_empty());
    }
}
