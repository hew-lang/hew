use std::collections::{HashMap, VecDeque};

use hew_hir::{
    BindingId, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmtKind,
    IntentKind, ResolvedRef,
};
use hew_types::{CallTarget, DefId, ResolvedTy};

use crate::{
    BlockArg, BlockId, CallableId, CallableInstance, Edge, EffectSummary, FunctionSourceOrigin,
    GenericTemplateId, OpId, Operand, Provenance, SemAbiParam, SemBlock, SemCallConv, SemCallable,
    SemCallableKind, SemFunction, SemGenericTemplate, SemModule, SemOp, SemOpKind, SemParamPassing,
    SemSignature, SemTerminator, SirInstanceKey, UseMode, ValueDef, ValueId,
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
pub fn lower_module(module: &HirModule) -> LoweredModule {
    lower_module_with_demand(module, SirLoweringDemand::Entry)
}

/// Lower SIR bodies under an explicit demand policy.
///
/// [`SirLoweringDemand::Entry`] is [`lower_module`].
/// [`SirLoweringDemand::EveryCallable`] asks for every admitted header's body
/// so a coverage inventory can say, per declaration, whether SIR takes it.
/// The strict compile route never asks for that demand, so nothing about it
/// changes here.
#[must_use]
pub fn lower_module_with_demand(module: &HirModule, demand: SirLoweringDemand) -> LoweredModule {
    // The HIR monomorphisation registry remains deliberately unused here.
    // SIR discovers concrete direct-user instances from each resolved call's
    // `SiteId -> call_site_type_args` fact, applies the enclosing semantic
    // substitution, and creates its own closed instance worklist.
    let mut service = InstanceService::new(module, demand);
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
            let signature = match scalar_callable_signature(function) {
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
            if module.entry_declaration.as_ref() == Some(&function.declaration) {
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
                effect_summary: EffectSummary::Unknown,
            });
        }
        generic_templates.sort_by(|left, right| left.id.cmp(&right.id));
        Self {
            callables,
            generic_templates,
            root_unit_callables,
            entry_callable,
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
}

impl<'a> InstanceService<'a> {
    fn new(module: &'a HirModule, demand: SirLoweringDemand) -> Self {
        let table = CallableTable::from_hir(module);
        let count = table.callables.len();
        Self {
            module,
            demand,
            table,
            states: vec![CallableState::Unreached; count],
            statuses: vec![None; count],
            by_instance: HashMap::new(),
            used_templates: std::collections::HashSet::new(),
            pending: VecDeque::new(),
            functions: Vec::new(),
        }
    }

    fn callable(&self, id: CallableId) -> Option<&SemCallable> {
        self.table.callable(id)
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
        let signature =
            scalar_callable_signature_with_substitution(template.function, &substitution)?;
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
            effect_summary: EffectSummary::Unknown,
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
            used_templates,
            mut functions,
            ..
        } = self;
        let generic_templates = table
            .generic_templates
            .into_iter()
            .filter(|template| used_templates.contains(&template.id))
            .collect();
        // Bodies are produced in demand order, which depends on the entry's
        // call graph. Publishing them in callable order instead keeps the
        // module — and every dump taken from it — a function of the program,
        // not of the traversal that discovered it.
        functions.sort_unstable_by_key(|function| function.callable);
        SemModule {
            callables: table.callables,
            generic_templates,
            root_unit_callables: table.root_unit_callables,
            entry_callable: table.entry_callable,
            functions,
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

fn scalar_callable_signature(function: &HirFn) -> Result<SemSignature, String> {
    if !function.type_params.is_empty() {
        return Err(
            "generic origin functions are instantiated by the SIR instance service, not admitted as abstract callable bodies"
                .to_string(),
        );
    }
    scalar_callable_signature_with_substitution(function, &TypeSubstitution::empty())
}

fn scalar_callable_signature_with_substitution(
    function: &HirFn,
    substitution: &TypeSubstitution,
) -> Result<SemSignature, String> {
    generic_template_admission(function)?;
    let mut params = Vec::with_capacity(function.params.len());
    for (index, parameter) in function.params.iter().enumerate() {
        let ty = substitution.apply(&parameter.ty);
        if !is_initial_scalar(&ty) {
            return Err(format!(
                "parameter {index} has non-scalar type `{}` after semantic substitution; aggregate/reference ABI lowering is deferred",
                ty.user_facing()
            ));
        }
        params.push(SemAbiParam {
            ty,
            passing: SemParamPassing::ReadOnly,
            caller_visible_projection: false,
        });
    }
    let return_ty = substitution.apply(&function.return_ty);
    if !is_initial_scalar_return(&return_ty) {
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

fn is_initial_scalar_return(ty: &ResolvedTy) -> bool {
    matches!(ty, hew_types::ResolvedTy::Unit) || is_initial_scalar(ty)
}

/// Translate HIR's authoritative source-semantic intent into the SIR operand
/// vocabulary exactly once.
///
/// This deliberately does *not* normalize an ownership intent to `Read` just
/// because the first scalar SIR slice cannot realize it yet. Callers use the
/// returned mode to reject unsupported ownership semantics at the HIR → SIR
/// boundary, preserving a precise implementation gap for later domains.
fn use_mode_from_hir_intent(intent: IntentKind) -> Result<UseMode, String> {
    match intent {
        IntentKind::Read => Ok(UseMode::Read),
        IntentKind::Modify => Ok(UseMode::BorrowMut),
        // HIR's `Consume` is an ownership transfer to a receiving semantic
        // operation/callee, whereas `Discharge` releases an obligation with
        // no receiver. SIR preserves that distinction as Move vs Consume.
        IntentKind::Consume => Ok(UseMode::Move),
        IntentKind::Discharge => Ok(UseMode::Consume),
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

fn initial_scalar_use_mode(intent: IntentKind) -> Result<UseMode, String> {
    let mode = use_mode_from_hir_intent(intent)?;
    if mode != UseMode::Read {
        return Err(format!(
            "HIR {intent:?} intent maps to SIR {mode:?}; initial scalar SIR admits only Read operands"
        ));
    }
    Ok(mode)
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
fn initial_value_transfer_mode(
    intent: IntentKind,
    ty: &hew_types::ResolvedTy,
    context: &str,
) -> Result<(), String> {
    let mode = use_mode_from_hir_intent(intent).map_err(|reason| format!("{context}: {reason}"))?;
    match mode {
        UseMode::Read | UseMode::Move if is_initial_value_type(ty) => Ok(()),
        UseMode::Read | UseMode::Move => Err(format!(
            "{context}: HIR intent maps to SIR {mode:?} for ownership-bearing `{}`; initial SIR only aliases BitCopy scalar/tuple binding/return flow",
            ty.user_facing()
        )),
        other => Err(format!(
            "{context}: HIR intent maps to SIR {other:?}; initial scalar/tuple binding/return flow admits only Read or BitCopy Move"
        )),
    }
}

fn lower_initial_value_transfer(
    builder: &mut Builder<'_, '_>,
    expr: &HirExpr,
    context: &str,
) -> Result<ValueId, String> {
    initial_value_transfer_mode(expr.intent, &builder.ty(&expr.ty), context)?;
    builder.lower_expr(expr)
}

/// Lower a unit expression transferred by an explicit `return`.
///
/// This is intentionally narrower than [`Builder::lower_discarded_expr`]. A
/// standalone discarded expression is an ordinary effect position and stays
/// read-only in the initial slice. A unit expression in `return` instead
/// transfers control to the caller; HIR marks that transfer `Consume`, which
/// is harmless for `Unit` but must not be rechecked as an ordinary operand use.
fn lower_initial_unit_return(builder: &mut Builder<'_, '_>, expr: &HirExpr) -> Result<(), String> {
    let mode = use_mode_from_hir_intent(expr.intent)
        .map_err(|reason| format!("unit return value: {reason}"))?;
    let ty = builder.ty(&expr.ty);
    if !matches!(mode, UseMode::Read | UseMode::Move) || ty != ResolvedTy::Unit {
        return Err(format!(
            "unit return value: HIR intent maps to SIR {mode:?} for `{}`; initial SIR admits only Read or Unit Move return transfer",
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
                Ok(BlockArg {
                    value,
                    ty,
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
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
        let result = self.lower_block(&self.function.body)?;
        if self.is_open() {
            self.set_terminator(SemTerminator::Return { value: result })?;
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
        })
    }

    /// Lower one HIR expression in a semantic operand position.
    ///
    /// The initial scalar SIR domain admits only read uses, but it still
    /// translates every HIR intent before rejecting a non-read mode. This
    /// prevents a source move/borrow/discharge from being silently weakened
    /// into a reusable SIR value during the migration.
    fn lower_read_operand(&mut self, expr: &HirExpr, context: &str) -> Result<Operand, String> {
        let mode = initial_scalar_use_mode(expr.intent)
            .map_err(|reason| format!("{context}: {reason}"))?;
        Ok(Operand {
            value: self.lower_expr(expr)?,
            mode,
        })
    }

    fn lower_block(&mut self, block: &HirBlock) -> Result<Option<Operand>, String> {
        for statement in &block.statements {
            if !self.is_open() {
                break;
            }
            match &statement.kind {
                HirStmtKind::Let(binding, value) => {
                    if binding.mutable {
                        return Err("mutable bindings are deferred until a dedicated SIR feature requires them".to_string());
                    }
                    let value = value
                        .as_ref()
                        .map(|expr| lower_initial_value_transfer(self, expr, "binding initializer"))
                        .transpose()?
                        .ok_or_else(|| {
                            "uninitialised bindings are not in the initial SIR subset".to_string()
                        })?;
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
                        Some(expr) => Some(Operand {
                            value: lower_initial_value_transfer(self, expr, "return value")?,
                            mode: UseMode::Read,
                        }),
                        None => None,
                    };
                    self.set_terminator(SemTerminator::Return { value })?;
                }
                HirStmtKind::Assign { .. } => {
                    return Err(
                        "assignment is deferred until SIR has an explicit mutable-location design"
                            .to_string(),
                    )
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
                Some(expr) => Ok(Some(self.lower_read_operand(expr, "block tail value")?)),
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    /// Lower an expression whose value is intentionally discarded.
    ///
    /// Scalar expressions keep their ordinary one-result SSA operation even
    /// when the result is unused.  A unit direct call is different: there is
    /// no semantic value to define, but the call itself must remain in SIR so
    /// later lowering can realize its call/continuation CFG edge.
    fn lower_discarded_expr(&mut self, expr: &HirExpr) -> Result<(), String> {
        initial_scalar_use_mode(expr.intent)
            .map_err(|reason| format!("discarded expression: {reason}"))?;
        if matches!(expr.kind, HirExprKind::Call { .. }) {
            self.lower_direct_call(expr, false)?;
            return Ok(());
        }
        self.lower_expr(expr).map(|_| ())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the closed initial HIR-to-SIR expression mapping remains intentionally local"
    )]
    fn lower_expr(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        match &expr.kind {
            HirExprKind::Literal(HirLiteral::Integer(value)) => {
                if !self.ty(&expr.ty).is_integer() {
                    return Err(format!(
                        "integer literal resolved as `{}` needs a dedicated SIR literal representation",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstI64(*value))
            }
            HirExprKind::Literal(HirLiteral::Bool(value)) => {
                if self.ty(&expr.ty) != ResolvedTy::Bool {
                    return Err(format!(
                        "boolean literal resolved as `{}` violates the SIR bool literal invariant",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstBool(*value))
            }
            HirExprKind::TupleLiteral { elements } => self.lower_tuple_make(expr, elements),
            HirExprKind::TupleIndex { tuple, index } => self.lower_tuple_get(expr, tuple, *index),
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } => self.bindings.get(binding).copied().ok_or_else(|| {
                format!("binding `{binding}` is not available in the SIR environment")
            }),
            HirExprKind::Unary { op, operand, .. } => {
                let value = self.lower_read_operand(operand, "unary operand")?;
                self.emit(expr, SemOpKind::Unary { op: *op, value })
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
                self.emit(expr, SemOpKind::Binary { op: *op, lhs, rhs })
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
                .lower_block(block)?
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
        let mut lowered_args = Vec::with_capacity(args.len());
        for (index, (arg, expected)) in args.iter().zip(&params).enumerate() {
            if expected.passing != SemParamPassing::ReadOnly {
                return Err(format!(
                    "direct callee `{}` has a non-ReadOnly SIR ABI parameter {index}",
                    callee_declaration.full_path()
                ));
            }
            let argument_ty = self.ty(&arg.ty);
            if argument_ty != expected.ty {
                return Err(format!(
                    "direct call argument {index} to `{}` has `{}`, expected `{}`",
                    callee_declaration.full_path(),
                    argument_ty.user_facing(),
                    expected.ty.user_facing()
                ));
            }
            lowered_args.push(self.lower_read_operand(
                arg,
                &format!(
                    "direct call argument {index} to `{}`",
                    callee_declaration.full_path()
                ),
            )?);
        }
        let kind = SemOpKind::Call {
            callee: callee_id,
            args: lowered_args,
        };
        if return_ty == ResolvedTy::Unit {
            if value_required {
                return Err(format!(
                    "unit-valued direct call `{}` cannot produce an SSA value",
                    callee_declaration.full_path()
                ));
            }
            self.emit_without_result(expr, kind)?;
            Ok(None)
        } else {
            Ok(Some(self.emit(expr, kind)?))
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
        let join_value = self.fresh_value();
        let join_block = self.new_block(vec![BlockArg {
            value: join_value,
            ty: self.ty(&whole.ty),
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
        let join = self.new_block(vec![BlockArg {
            value: result,
            ty: self.ty(&whole.ty),
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
            args: vec![Operand {
                value: constant,
                mode: UseMode::Read,
            }],
        }))?;

        self.current = join;
        Ok(result)
    }

    fn emit(&mut self, expr: &HirExpr, kind: SemOpKind) -> Result<ValueId, String> {
        let value = self.fresh_value();
        let op = SemOp {
            id: OpId(self.ops),
            results: vec![ValueDef {
                id: value,
                ty: self.ty(&expr.ty),
            }],
            kind,
            provenance: Provenance::Site(expr.site),
        };
        self.current_block_mut().append_op(op)?;
        self.ops += 1;
        Ok(value)
    }

    fn emit_without_result(&mut self, expr: &HirExpr, kind: SemOpKind) -> Result<(), String> {
        let op = SemOp {
            id: OpId(self.ops),
            results: Vec::new(),
            kind,
            provenance: Provenance::Site(expr.site),
        };
        self.current_block_mut().append_op(op)?;
        self.ops += 1;
        Ok(())
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
        initial_scalar_use_mode, initial_value_transfer_mode, is_initial_value_type,
        use_mode_from_hir_intent, PendingBlock,
    };
    use crate::{BlockId, OpId, Provenance, SemOp, SemOpKind, SemTerminator, UseMode};
    use hew_hir::IntentKind;
    use hew_types::ResolvedTy;

    #[test]
    fn hir_intents_map_once_and_initial_scalar_sir_fails_closed() {
        assert_eq!(
            use_mode_from_hir_intent(IntentKind::Read),
            Ok(UseMode::Read)
        );
        assert_eq!(
            use_mode_from_hir_intent(IntentKind::Modify),
            Ok(UseMode::BorrowMut)
        );
        assert_eq!(
            use_mode_from_hir_intent(IntentKind::Consume),
            Ok(UseMode::Move)
        );
        assert_eq!(
            use_mode_from_hir_intent(IntentKind::Discharge),
            Ok(UseMode::Consume)
        );

        for intent in [
            IntentKind::Modify,
            IntentKind::Consume,
            IntentKind::Discharge,
            IntentKind::Capture,
            IntentKind::Yield,
            IntentKind::Unknown,
        ] {
            let reason = initial_scalar_use_mode(intent)
                .expect_err("non-Read or unresolved HIR intent must not become a scalar SIR Read");
            assert!(
                reason.contains("initial scalar SIR")
                    || reason.contains("requires")
                    || reason.contains("not a legal"),
                "the failure must explain why {intent:?} is outside the current SIR ownership domain: {reason}"
            );
        }
    }

    #[test]
    fn scalar_and_tuple_binding_transfers_admit_only_bitcopy_values() {
        assert!(initial_value_transfer_mode(IntentKind::Consume, &ResolvedTy::I64, "test").is_ok());
        assert!(initial_value_transfer_mode(IntentKind::Read, &ResolvedTy::Bool, "test").is_ok());
        let tuple = ResolvedTy::Tuple(vec![
            ResolvedTy::I64,
            ResolvedTy::Tuple(vec![ResolvedTy::Bool]),
        ]);
        assert!(is_initial_value_type(&tuple));
        assert!(initial_value_transfer_mode(IntentKind::Consume, &tuple, "test").is_ok());
        for intent in [IntentKind::Read, IntentKind::Consume] {
            let error = initial_value_transfer_mode(intent, &ResolvedTy::String, "test")
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
