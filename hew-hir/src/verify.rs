// The verify pass walks `HirExprKind` exhaustively, which includes the
// `#[deprecated]` `CallTraitMethodStatic` variant. The deprecation
// enforcement is structural (allowlist test on construction sites),
// not lint-driven.
#![allow(
    deprecated,
    reason = "legacy CallTraitMethodStatic variant is allowlist-gated; \
              see hew-hir/tests/call_trait_method_static_creation_allowlist.rs"
)]

use std::collections::{HashMap, HashSet};
use std::ops::Range;

use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::{BindingId, HirNodeId, ResolvedRef, SiteId};
use crate::node::{
    HirBlock, HirExpr, HirExprKind, HirGenCaptureSource, HirItem, HirLiteral, HirMatchArmPredicate,
    HirModule, HirProducedValueRelation, HirProducedValueSourceAnchor, HirStmtKind,
};
use hew_types::{
    BuiltinType, DefId, HashMapMethod, HashSetMethod, MethodTargetFamily, ProducedValueAcquisition,
    ProducedValueOwnership, RcIntrinsicOp, ResolvedTy, RuntimeCallFamily, VecMethod,
};

#[must_use]
pub fn verify_hir(module: &HirModule) -> Vec<HirDiagnostic> {
    let mut verifier = Verifier::default();
    verifier.module(module);
    verifier.diagnostics
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirSiteSource {
    pub span: Range<usize>,
    pub source_module: Option<String>,
}

#[must_use]
pub fn collect_site_spans(module: &HirModule) -> HashMap<SiteId, HirSiteSource> {
    let mut verifier = Verifier::default();
    verifier.module(module);
    verifier.site_spans
}

/// Parent relation for every HIR expression occurrence.  Checker facts name
/// logical AST spans; HIR may clone those spans during desugaring, so consumers
/// pair a dependency only with the candidate inside the current occurrence's
/// structural subtree.
#[must_use]
pub fn collect_site_parents(module: &HirModule) -> HashMap<SiteId, Option<SiteId>> {
    let mut verifier = Verifier::default();
    verifier.module(module);
    verifier.site_parents
}

/// Complete the module carrier after all HIR transforms have run.
///
/// Source expressions retain their checker-authored row.  Expressions created
/// by HIR-only desugarings have no source checker span, so they receive an
/// explicit `Unknown` result fact instead of silently escaping the ownership
/// authority.  This is intentionally fail-closed: a later ownership-demanding
/// sink must reject the synthetic result until its producer gains a typed
/// contract.
#[must_use]
pub fn complete_produced_value_facts(
    module: &HirModule,
) -> HashMap<SiteId, crate::node::HirProducedValueFact> {
    let mut verifier = Verifier::default();
    verifier.module(module);
    let mut facts = module.produced_value_facts.clone();
    for (&site, &producer) in &verifier.observed_producers {
        facts
            .entry(site)
            .or_insert(crate::node::HirProducedValueFact {
                producer,
                ownership: ProducedValueOwnership::Unknown,
                relation: crate::node::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            });
    }
    finalize_resolved_produced_value_facts(&verifier, &mut facts);
    facts
}

/// Close the checker-authored graph after HIR has attached exact declaration
/// and collection-family identities to every executable call. Generic method
/// bodies can remain abstract while checking, and a direct user call cannot
/// know its callee's result disposition until every body is available. This
/// fixed point fills only those `Unknown` leaves; it never revises a concrete
/// checker verdict or reconstructs authority from a symbol spelling.
fn resolved_producer_ownership(
    producer: crate::node::HirProducedValueProducer,
) -> ProducedValueOwnership {
    match producer {
        crate::node::HirProducedValueProducer::GeneratorNext
        | crate::node::HirProducedValueProducer::ChannelRecvAwait
        | crate::node::HirProducedValueProducer::StreamRecvAwait => {
            ProducedValueOwnership::owned(ProducedValueAcquisition::Delivery)
        }
        _ => ProducedValueOwnership::Unknown,
    }
}

fn resolved_runtime_call_ownership(family: RuntimeCallFamily) -> ProducedValueOwnership {
    match family {
        RuntimeCallFamily::ChannelRecvLayout
        | RuntimeCallFamily::ChannelTryRecvLayout
        | RuntimeCallFamily::StreamNextLayout
        | RuntimeCallFamily::StreamTryNextLayout
        | RuntimeCallFamily::DuplexRecv
        | RuntimeCallFamily::DuplexRecvHalf
        | RuntimeCallFamily::DuplexTryRecv => {
            ProducedValueOwnership::owned(ProducedValueAcquisition::Delivery)
        }
        _ => ProducedValueOwnership::Unknown,
    }
}

fn call_argument_is_proven_owned(
    site: SiteId,
    verifier: &Verifier,
    facts: &HashMap<SiteId, crate::node::HirProducedValueFact>,
    visiting: &mut HashSet<SiteId>,
) -> bool {
    use ProducedValueOwnership as Ownership;

    if !visiting.insert(site) {
        return false;
    }
    let result = facts.get(&site).is_some_and(|fact| match fact.ownership {
        Ownership::Owned { .. } | Ownership::NoOwner => true,
        Ownership::Borrowed if fact.producer == crate::node::HirProducedValueProducer::Literal => {
            true
        }
        Ownership::Borrowed | Ownership::ReceiverIdentity
            if fact.producer == crate::node::HirProducedValueProducer::BindingRef =>
        {
            verifier
                .binding_reference_targets
                .get(&site)
                .is_some_and(|binding| {
                    verifier
                        .binding_reference_sites
                        .get(binding)
                        .is_some_and(|references| references.len() == 1)
                        && verifier
                            .binding_definitions
                            .get(binding)
                            .is_some_and(|definitions| {
                                definitions.len() == 1
                                    && call_argument_is_proven_owned(
                                        definitions[0],
                                        verifier,
                                        facts,
                                        visiting,
                                    )
                            })
                })
        }
        Ownership::Borrowed | Ownership::ReceiverIdentity | Ownership::Unknown => false,
    });
    visiting.remove(&site);
    result
}

fn finalize_resolved_produced_value_facts(
    verifier: &Verifier,
    facts: &mut HashMap<SiteId, crate::node::HirProducedValueFact>,
) {
    seed_resolved_produced_value_facts(verifier, facts);
    let limit = verifier
        .function_return_sites
        .len()
        .saturating_add(verifier.user_call_targets.len())
        .saturating_add(verifier.machine_variant_payloads.len())
        .saturating_add(1);
    for _ in 0..limit {
        propagate_produced_value_relations(facts);
        let variants_changed = resolve_machine_variant_facts(verifier, facts);
        let calls_changed = resolve_user_call_facts(verifier, facts);
        if !variants_changed && !calls_changed {
            break;
        }
    }
    propagate_produced_value_relations(facts);
}

fn seed_resolved_produced_value_facts(
    verifier: &Verifier,
    facts: &mut HashMap<SiteId, crate::node::HirProducedValueFact>,
) {
    use ProducedValueOwnership as Ownership;
    for fact in facts.values_mut() {
        if matches!(fact.ownership, Ownership::Unknown) {
            fact.ownership = resolved_producer_ownership(fact.producer);
        }
    }

    for (site, family) in &verifier.resolved_collection_calls {
        let Some(fact) = facts.get_mut(site) else {
            continue;
        };
        if !matches!(fact.ownership, Ownership::Unknown) {
            continue;
        }
        fact.ownership = match family {
            MethodTargetFamily::HashMap(HashMapMethod::Remove)
            | MethodTargetFamily::Vec(VecMethod::Pop | VecMethod::Remove) => {
                Ownership::owned(ProducedValueAcquisition::MoveOut)
            }
            MethodTargetFamily::HashMap(
                HashMapMethod::Clone
                | HashMapMethod::Get
                | HashMapMethod::Keys
                | HashMapMethod::Values,
            )
            | MethodTargetFamily::HashSet(HashSetMethod::Clone | HashSetMethod::ToVec)
            | MethodTargetFamily::Vec(VecMethod::Clone | VecMethod::Get) => {
                Ownership::owned(ProducedValueAcquisition::Clone)
            }
            MethodTargetFamily::HashMap(_)
            | MethodTargetFamily::HashSet(_)
            | MethodTargetFamily::Vec(_) => Ownership::Unknown,
        };
    }
    for (site, family) in &verifier.runtime_call_targets {
        let Some(fact) = facts.get_mut(site) else {
            continue;
        };
        if !matches!(fact.ownership, Ownership::Unknown) {
            continue;
        }
        fact.ownership = resolved_runtime_call_ownership(*family);
    }
}

fn resolve_machine_variant_facts(
    verifier: &Verifier,
    facts: &mut HashMap<SiteId, crate::node::HirProducedValueFact>,
) -> bool {
    use ProducedValueOwnership as Ownership;

    let mut changed = false;
    for (site, payloads) in &verifier.machine_variant_payloads {
        let Some(fact) = facts.get(site) else {
            continue;
        };
        if !matches!(fact.ownership, Ownership::Unknown) {
            continue;
        }
        let mut owns_payload = false;
        let mut borrows_payload = false;
        let mut resolved = true;
        for payload in payloads {
            match facts.get(payload).map(|payload| payload.ownership) {
                Some(Ownership::Owned { .. }) => owns_payload = true,
                Some(Ownership::NoOwner) => {}
                Some(Ownership::Borrowed)
                    if verifier.observed_producers.get(payload)
                        == Some(&crate::node::HirProducedValueProducer::Literal) =>
                {
                    owns_payload = true;
                }
                Some(Ownership::Borrowed | Ownership::ReceiverIdentity) => {
                    borrows_payload = true;
                }
                Some(Ownership::Unknown) | None => resolved = false,
            }
        }
        if !resolved {
            continue;
        }
        let ownership = if borrows_payload {
            Ownership::Borrowed
        } else if owns_payload || !payloads.is_empty() {
            Ownership::owned(ProducedValueAcquisition::Fresh)
        } else {
            Ownership::NoOwner
        };
        if let Some(fact) = facts.get_mut(site) {
            fact.ownership = ownership;
            changed = true;
        }
    }
    changed
}

fn function_return_ownership_summaries(
    verifier: &Verifier,
    facts: &HashMap<SiteId, crate::node::HirProducedValueFact>,
) -> HashMap<DefId, ProducedValueOwnership> {
    verifier
        .function_return_sites
        .iter()
        .map(|(declaration, sites)| {
            let mut ownership = sites
                .iter()
                .filter_map(|site| facts.get(site))
                .map(|fact| fact.ownership);
            let summary = ownership
                .next()
                .map_or(ProducedValueOwnership::Unknown, |first| {
                    if ownership.all(|next| next == first) {
                        first
                    } else {
                        ProducedValueOwnership::Unknown
                    }
                });
            (declaration.clone(), summary)
        })
        .collect()
}

fn resolve_user_call_facts(
    verifier: &Verifier,
    facts: &mut HashMap<SiteId, crate::node::HirProducedValueFact>,
) -> bool {
    use ProducedValueOwnership as Ownership;

    let summaries = function_return_ownership_summaries(verifier, facts);
    let mut changed = false;
    for (site, target) in &verifier.user_call_targets {
        let Some(summary) = summaries.get(target).copied() else {
            continue;
        };
        let summary = if matches!(summary, Ownership::Borrowed) {
            let arguments = verifier.user_call_arguments.get(site);
            if arguments.is_some_and(|arguments| {
                !arguments.is_empty()
                    && arguments.iter().all(|argument| {
                        call_argument_is_proven_owned(
                            *argument,
                            verifier,
                            facts,
                            &mut HashSet::new(),
                        )
                    })
            }) {
                Ownership::owned(ProducedValueAcquisition::Fresh)
            } else {
                summary
            }
        } else {
            summary
        };
        let Some(fact) = facts.get_mut(site) else {
            continue;
        };
        if matches!(fact.ownership, Ownership::Unknown) && !matches!(summary, Ownership::Unknown) {
            fact.ownership = summary;
            changed = true;
        }
    }
    changed
}

fn propagate_produced_value_relations(
    facts: &mut HashMap<SiteId, crate::node::HirProducedValueFact>,
) {
    use ProducedValueOwnership as Ownership;

    let snapshot: HashMap<SiteId, Ownership> = facts
        .iter()
        .map(|(site, fact)| (*site, fact.ownership))
        .collect();
    for fact in facts.values_mut() {
        if !matches!(fact.ownership, Ownership::Unknown) {
            continue;
        }
        fact.ownership = match &fact.relation {
            HirProducedValueRelation::Identity(source)
            | HirProducedValueRelation::Subsumes(source) => {
                snapshot.get(source).copied().unwrap_or(Ownership::Unknown)
            }
            HirProducedValueRelation::MoveOut(source)
            | HirProducedValueRelation::Projection(source) => match snapshot.get(source) {
                Some(Ownership::Owned { .. }) => {
                    Ownership::owned(ProducedValueAcquisition::MoveOut)
                }
                Some(Ownership::Borrowed | Ownership::ReceiverIdentity) => Ownership::Borrowed,
                Some(Ownership::NoOwner | Ownership::Unknown) | None => Ownership::Unknown,
            },
            HirProducedValueRelation::Join(sources) => {
                let mut ownership = sources
                    .iter()
                    .map(|site| snapshot.get(site).copied().unwrap_or(Ownership::Unknown));
                ownership.next().map_or(Ownership::Unknown, |first| {
                    if ownership.all(|next| next == first) {
                        first
                    } else {
                        Ownership::Unknown
                    }
                })
            }
            HirProducedValueRelation::Leaf => Ownership::Unknown,
        };
    }
}

#[derive(Debug, Default)]
struct Verifier {
    bindings: HashSet<BindingId>,
    sites: HashSet<SiteId>,
    nodes: HashSet<HirNodeId>,
    diagnostics: Vec<HirDiagnostic>,
    current_source_module: Option<String>,
    site_spans: HashMap<SiteId, HirSiteSource>,
    produced_value_facts: HashMap<SiteId, crate::node::HirProducedValueFact>,
    observed_producers: HashMap<SiteId, crate::node::HirProducedValueProducer>,
    site_types: HashMap<SiteId, ResolvedTy>,
    current_expr_parent: Option<SiteId>,
    site_parents: HashMap<SiteId, Option<SiteId>>,
    current_function: Option<DefId>,
    function_return_sites: HashMap<DefId, Vec<SiteId>>,
    user_call_targets: HashMap<SiteId, DefId>,
    user_call_arguments: HashMap<SiteId, Vec<SiteId>>,
    resolved_collection_calls: HashMap<SiteId, MethodTargetFamily>,
    runtime_call_targets: HashMap<SiteId, RuntimeCallFamily>,
    machine_variant_payloads: HashMap<SiteId, Vec<SiteId>>,
    binding_definitions: HashMap<BindingId, Vec<SiteId>>,
    binding_reference_sites: HashMap<BindingId, Vec<SiteId>>,
    binding_reference_targets: HashMap<SiteId, BindingId>,
    nested_callable_depth: usize,
}

impl Verifier {
    #[expect(
        clippy::too_many_lines,
        reason = "module verification exhaustively dispatches every HIR item family"
    )]
    fn module(&mut self, module: &HirModule) {
        self.produced_value_facts
            .clone_from(&module.produced_value_facts);
        for item in &module.items {
            self.current_source_module = Self::item_source_module(module, item);
            match item {
                HirItem::Function(func) => {
                    let prior_function = self.current_function.replace(func.declaration.clone());
                    self.node(func.node, func.span.clone());
                    for param in &func.params {
                        self.binding(param.id, param.span.clone());
                    }
                    self.block(&func.body);
                    if let Some(tail) = &func.body.tail {
                        self.function_return_sites
                            .entry(func.declaration.clone())
                            .or_default()
                            .push(tail.site);
                    }
                    self.current_function = prior_function;
                }
                HirItem::TypeDecl(decl) => {
                    // Type declarations contribute only their HirNodeId
                    // uniqueness to the verifier — they carry no bindings,
                    // sites, or expressions to validate. The marker /
                    // consuming-method validations fire upstream in
                    // `lower_type_decl`.
                    self.node(decl.node, decl.span.clone());
                }
                HirItem::Machine(machine) => {
                    self.node(machine.node, machine.span.clone());
                    for state in &machine.states {
                        for field in &state.fields {
                            if let Some(default) = &field.default {
                                self.expr(default);
                            }
                        }
                        if let Some(entry) = &state.entry {
                            self.block(entry);
                        }
                        if let Some(exit) = &state.exit {
                            self.block(exit);
                        }
                    }
                    for event in &machine.events {
                        for field in &event.fields {
                            if let Some(default) = &field.default {
                                self.expr(default);
                            }
                        }
                    }
                    for transition in &machine.transitions {
                        if let Some(guard) = &transition.guard {
                            self.expr(guard);
                        }
                        self.expr(&transition.body);
                    }
                }
                HirItem::Record(record) => {
                    // Record declarations contribute only their HirNodeId
                    // uniqueness to the verifier — they carry no bindings,
                    // sites, or expressions to validate. The @linear-field
                    // guard fires upstream in `lower_record_decl`.
                    self.node(record.node, record.span.clone());
                }
                HirItem::Actor(actor) => {
                    self.node(actor.node, actor.span.clone());
                    for field in &actor.state_fields {
                        if let Some(default) = &field.default {
                            self.expr(default);
                        }
                    }
                    if let Some(init) = &actor.init {
                        for param in &init.params {
                            self.binding(param.id, param.span.clone());
                        }
                        self.block(&init.body);
                    }
                    for receive in &actor.receive_handlers {
                        for param in &receive.params {
                            self.binding(param.id, param.span.clone());
                        }
                        self.block(&receive.body);
                    }
                    for method in &actor.methods {
                        for param in &method.params {
                            self.binding(param.id, param.span.clone());
                        }
                        self.block(&method.body);
                    }
                    for hook in &actor.lifecycle_hooks {
                        for param in &hook.params {
                            self.binding(param.id, param.span.clone());
                        }
                        self.block(&hook.body);
                    }
                }
                HirItem::Supervisor(sup) => {
                    // Supervisor declarations contribute only their HirNodeId
                    // uniqueness in S-A; children-list resolution and
                    // wired_to validation are S-B's job.
                    self.node(sup.node, sup.span.clone());
                    // Register each child's declaration site and every
                    // init-arg/pool-count expression's real site. Without
                    // this, MIR diagnostics that carry a supervisor child's
                    // site (a missing required field, an unknown field name)
                    // key into `collect_site_spans` with an ID the table
                    // never claims — silently rendering with no location, or
                    // worse, numerically colliding with an unrelated site
                    // that IS registered and rendering a wrong caret.
                    for child in &sup.children {
                        self.site(child.site, child.span.clone());
                        for (_, arg) in &child.init_args {
                            self.expr(arg);
                        }
                        if let Some(pool_count) = &child.pool_count {
                            self.expr(pool_count);
                        }
                    }
                }
                HirItem::Impl(block) => {
                    // V0b: impl-block metadata only contributes its own
                    // HirNodeId. The per-method bodies are emitted as
                    // sibling `HirItem::Function` entries and are walked
                    // through the `Function` arm above, so no recursion
                    // into `block.method_symbols` is needed here.
                    self.node(block.node, block.span.clone());
                }
                HirItem::ExternFn(ef) => {
                    // Extern fns have no body, no parameter bindings — only
                    // their own HirNodeId contributes to uniqueness. The
                    // signature is verified by the checker before lowering.
                    self.node(ef.node, ef.span.clone());
                }
                HirItem::Const(c) => {
                    // Const declarations contribute only their HirNodeId
                    // uniqueness — the initializer was constant-folded into a
                    // value at lowering time, so there are no bindings, sites,
                    // or expressions to verify.
                    self.node(c.node, c.span.clone());
                }
            }
            self.current_source_module = None;
        }
        self.verify_produced_value_facts();
    }

    /// Validate the HIR result-fact carrier independently of lowering.  This
    /// catches stale carrier classifications, facts attached to a disappeared
    /// expression, and receiver-identity rows that lost their receiver site.
    #[expect(
        clippy::too_many_lines,
        clippy::items_after_statements,
        reason = "one fail-closed pass validates carrier structure, types, and cycles"
    )]
    fn verify_produced_value_facts(&mut self) {
        let is_in_subtree = |candidate: SiteId, root: SiteId| {
            let mut cursor = Some(candidate);
            while let Some(site) = cursor {
                if site == root {
                    return true;
                }
                cursor = self.site_parents.get(&site).copied().flatten();
            }
            false
        };
        let mut relation_edges: HashMap<SiteId, Vec<SiteId>> = HashMap::new();
        for (site, fact) in &self.produced_value_facts {
            if !self.sites.contains(site) {
                self.diagnostics.push(self.diagnostic(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "produced value fact".to_string(),
                        reason: format!("fact refers to unknown HIR site {site}"),
                    },
                    0..0,
                    format!(
                        "produced-value fact at {site} must be attached to a live HIR expression"
                    ),
                ));
            }
            if matches!(fact.ownership, ProducedValueOwnership::ReceiverIdentity)
                != fact.receiver.is_some()
            {
                self.diagnostics.push(
                    self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "produced value receiver identity".to_string(),
                            reason: "receiver site presence disagrees with ownership disposition"
                                .to_string(),
                        },
                        0..0,
                        "receiver-identity ownership must carry exactly one receiver site",
                    ),
                );
            }
            if let Some(receiver) = fact.receiver {
                if !self.sites.contains(&receiver) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "produced value receiver identity".to_string(),
                            reason: format!("receiver refers to unknown HIR site {receiver}"),
                        },
                        0..0,
                        "receiver-identity ownership must reference a live HIR expression",
                    ));
                } else if !is_in_subtree(receiver, *site) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "produced value receiver identity".to_string(),
                            reason: format!(
                                "receiver {receiver} is outside result {site}'s structural subtree"
                            ),
                        },
                        0..0,
                        "receiver ownership transfer must remain inside the result occurrence subtree",
                    ));
                } else if let (Some(result_ty), Some(receiver_ty)) =
                    (self.site_types.get(site), self.site_types.get(&receiver))
                {
                    if result_ty != receiver_ty {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "produced value receiver identity".to_string(),
                                reason: format!(
                                    "receiver {receiver} has type {receiver_ty:?}, but result {site} has type {result_ty:?}"
                                ),
                            },
                            0..0,
                            "receiver ownership transfer requires type-congruent storage",
                        ));
                    }
                }
            }
            let sources = match &fact.relation {
                HirProducedValueRelation::Leaf => Vec::new(),
                HirProducedValueRelation::Identity(source)
                | HirProducedValueRelation::Subsumes(source)
                | HirProducedValueRelation::MoveOut(source)
                | HirProducedValueRelation::Projection(source) => vec![*source],
                HirProducedValueRelation::Join(sources) => {
                    if sources.is_empty() {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "produced value dependency".to_string(),
                                reason: "join relation has no source sites".to_string(),
                            },
                            0..0,
                            "produced-value join must name at least one live source",
                        ));
                    }
                    sources.clone()
                }
            };
            for source in &sources {
                if !self.sites.contains(source) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "produced value dependency".to_string(),
                            reason: format!("relation refers to unknown HIR site {source}"),
                        },
                        0..0,
                        "produced-value relation must reference a live HIR expression",
                    ));
                } else if !self.produced_value_facts.contains_key(source) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "produced value dependency".to_string(),
                            reason: format!("relation source {source} has no produced-value fact"),
                        },
                        0..0,
                        "produced-value relation source must retain its authority row",
                    ));
                } else if !is_in_subtree(*source, *site) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "produced value dependency".to_string(),
                            reason: format!(
                                "relation source {source} is outside result {site}'s structural subtree"
                            ),
                        },
                        0..0,
                        "produced-value relation must remain inside its result occurrence subtree",
                    ));
                }
            }
            if let HirProducedValueRelation::Identity(source) = &fact.relation {
                if let (Some(result_ty), Some(source_ty)) =
                    (self.site_types.get(site), self.site_types.get(source))
                {
                    if result_ty != source_ty
                        && !source_ty.can_implicitly_numeric_normalize_to(result_ty)
                    {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "produced value identity".to_string(),
                                reason: format!(
                                    "identity source {source} has type {source_ty:?}, but result {site} has type {result_ty:?}"
                                ),
                            },
                            self.site_spans
                                .get(site)
                                .map_or(0..0, |source| source.span.clone()),
                            "identity ownership transfer requires type-congruent storage or a checker-admitted numeric normalization",
                        ));
                    }
                }
            }
            if let HirProducedValueRelation::Join(sources) = &fact.relation {
                if let Some(result_ty) = self.site_types.get(site) {
                    for source in sources {
                        if let Some(source_ty) = self.site_types.get(source) {
                            if source_ty != result_ty
                                && !source_ty.can_implicitly_numeric_normalize_to(result_ty)
                            {
                                self.diagnostics.push(self.diagnostic(
                                    HirDiagnosticKind::CheckerBoundaryViolation {
                                        name: "produced value join".to_string(),
                                        reason: format!(
                                            "join source {source} has type {source_ty:?}, but result {site} has type {result_ty:?}"
                                        ),
                                    },
                                    self.site_spans
                                        .get(site)
                                        .map_or(0..0, |source| source.span.clone()),
                                    "join ownership convergence requires type-congruent storage or a checker-admitted numeric normalization",
                                ));
                            }
                        }
                    }
                }
            }
            if let HirProducedValueRelation::Subsumes(source) = &fact.relation {
                if self.site_parents.get(source).copied().flatten() != Some(*site) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "produced value subsumption".to_string(),
                            reason: format!(
                                "subsumed source {source} is not a direct structural child of {site}"
                            ),
                        },
                        self.site_spans
                            .get(site)
                            .map_or(0..0, |source| source.span.clone()),
                        "specialised ownership subsumption must preserve its ordered nested source spine",
                    ));
                }
            }
            relation_edges.insert(*site, sources);
        }
        for (site, producer) in &self.observed_producers {
            if !self.produced_value_facts.contains_key(site) {
                self.diagnostics.push(self.diagnostic(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "produced value fact".to_string(),
                        reason: format!(
                            "live HIR site {site} ({producer:?}) has no produced-value authority row"
                        ),
                    },
                    self.site_spans
                        .get(site)
                        .map_or(0..0, |source| source.span.clone()),
                    "every observed HIR result producer must retain an explicit fact",
                ));
            }
        }

        // Relation edges form a checker-authored DAG.  Never let a cyclic
        // carrier degrade into a traversal-order ownership inference.
        fn visit(
            site: SiteId,
            edges: &HashMap<SiteId, Vec<SiteId>>,
            states: &mut HashMap<SiteId, u8>,
            stack: &mut Vec<SiteId>,
            cyclic: &mut HashSet<SiteId>,
        ) {
            states.insert(site, 1);
            stack.push(site);
            if let Some(sources) = edges.get(&site) {
                for source in sources {
                    if !edges.contains_key(source) {
                        continue;
                    }
                    match states.get(source).copied().unwrap_or(0) {
                        0 => visit(*source, edges, states, stack, cyclic),
                        1 => {
                            if let Some(start) = stack.iter().position(|entry| entry == source) {
                                cyclic.extend(stack[start..].iter().copied());
                            }
                        }
                        _ => {}
                    }
                }
            }
            stack.pop();
            states.insert(site, 2);
        }
        let mut states = HashMap::new();
        let mut stack = Vec::new();
        let mut cyclic = HashSet::new();
        for site in relation_edges.keys().copied().collect::<Vec<_>>() {
            if states.get(&site).copied().unwrap_or(0) == 0 {
                visit(site, &relation_edges, &mut states, &mut stack, &mut cyclic);
            }
        }
        for site in cyclic {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "produced value dependency".to_string(),
                    reason: format!("relation graph contains a cycle through HIR site {site}"),
                },
                0..0,
                "produced-value relation graph must be acyclic",
            ));
        }
    }

    fn diagnostic(
        &self,
        kind: HirDiagnosticKind,
        span: std::ops::Range<usize>,
        note: impl Into<String>,
    ) -> HirDiagnostic {
        HirDiagnostic::new(kind, span, note).with_source_module(self.current_source_module.clone())
    }

    /// The checker may preserve an unsupported target for diagnostics while
    /// recovering its surrounding expression.  That sentinel must never cross
    /// into an executable HIR call carrier: later phases intentionally consume
    /// the structured target and have no name-based recovery path.
    fn executable_call_target(&mut self, target: &hew_types::CallTarget, expr: &HirExpr) {
        if let hew_types::CallTarget::Unsupported { reason } = target {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "call target".to_string(),
                    reason: reason.clone(),
                },
                expr.span.clone(),
                "unsupported checker call target must not reach executable HIR",
            ));
        }
    }

    fn block(&mut self, block: &HirBlock) {
        self.node(block.node, 0..0);
        for stmt in &block.statements {
            self.node(stmt.node, stmt.span.clone());
            match &stmt.kind {
                HirStmtKind::Let(binding, value) => {
                    self.binding(binding.id, binding.span.clone());
                    if let Some(value) = value {
                        if self.nested_callable_depth == 0 {
                            self.binding_definitions
                                .entry(binding.id)
                                .or_default()
                                .push(value.site);
                        }
                        self.expr(value);
                    }
                }
                HirStmtKind::Assign { target, value } => {
                    if self.nested_callable_depth == 0 {
                        if let HirExprKind::BindingRef {
                            resolved: ResolvedRef::Binding(binding),
                            ..
                        } = &target.kind
                        {
                            self.binding_definitions
                                .entry(*binding)
                                .or_default()
                                .push(value.site);
                        }
                    }
                    self.expr(target);
                    self.expr(value);
                }
                HirStmtKind::Expr(expr) => self.expr(expr),
                HirStmtKind::Return(Some(expr)) => {
                    if self.nested_callable_depth == 0 {
                        if let Some(function) = self.current_function.clone() {
                            self.function_return_sites
                                .entry(function)
                                .or_default()
                                .push(expr.site);
                        }
                    }
                    self.expr(expr);
                }
                HirStmtKind::Return(None) => {}
                HirStmtKind::Defer { body, .. } => self.expr(body),
                HirStmtKind::LetElse {
                    scrutinee,
                    bindings,
                    success_prelude,
                    else_body,
                    ..
                } => {
                    self.expr(scrutinee);
                    // The Ok-path bindings escape into the enclosing scope —
                    // register them here so later references resolve.
                    for binding in bindings {
                        self.binding(binding.binding, scrutinee.span.clone());
                    }
                    // Aggregate payload destructure (e.g. `Ok((n, s))`): the
                    // prelude's `Let` statements introduce the leaf binders
                    // (`n`, `s`) that also escape into the enclosing scope.
                    // Register them and verify their projection values so a
                    // later reference resolves and is not flagged unresolved.
                    for prelude_stmt in success_prelude {
                        if let HirStmtKind::Let(binding, value) = &prelude_stmt.kind {
                            self.binding(binding.id, binding.span.clone());
                            if let Some(value) = value {
                                self.expr(value);
                            }
                        }
                    }
                    self.block(else_body);
                }
            }
        }
        if let Some(tail) = &block.tail {
            self.expr(tail);
        }
    }

    fn expr(&mut self, expr: &HirExpr) {
        let parent = self.current_expr_parent.replace(expr.site);
        self.site_parents.insert(expr.site, parent);
        self.expr_inner(expr);
        self.current_expr_parent = parent;
    }

    fn produced_value_source_anchor(
        &mut self,
        anchor: &HirProducedValueSourceAnchor,
        parent: SiteId,
    ) {
        self.site_parents.insert(anchor.site, Some(parent));
        self.site_types.insert(anchor.site, anchor.ty.clone());
        self.node(anchor.node, anchor.span.clone());
        self.site(anchor.site, anchor.span.clone());
        self.observed_producers.insert(anchor.site, anchor.producer);
        if let Some(fact) = self.produced_value_facts.get(&anchor.site) {
            if fact.producer != anchor.producer {
                self.diagnostics.push(self.diagnostic(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "produced value source anchor".to_string(),
                        reason: format!(
                            "carrier says {:?}, but source anchor is {:?}",
                            fact.producer, anchor.producer
                        ),
                    },
                    anchor.span.clone(),
                    "produced-value source anchor must retain the consumed node's producer class",
                ));
            }
        }
        if let Some(source) = &anchor.source {
            self.produced_value_source_anchor(source, anchor.site);
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "exhaustive match remains in one structural HIR walker"
    )]
    fn expr_inner(&mut self, expr: &HirExpr) {
        self.node(expr.node, expr.span.clone());
        self.site(expr.site, expr.span.clone());
        self.site_types.insert(expr.site, expr.ty.clone());
        let actual_producer = crate::node::HirProducedValueProducer::classify(&expr.kind);
        self.observed_producers.insert(expr.site, actual_producer);
        if let Some(fact) = self.produced_value_facts.get(&expr.site) {
            if fact.producer != actual_producer {
                self.diagnostics.push(self.diagnostic(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "produced value fact".to_string(),
                        reason: format!(
                            "carrier says {:?}, but HIR node is {:?}",
                            fact.producer, actual_producer
                        ),
                    },
                    expr.span.clone(),
                    "produced-value fact must use the node's structural producer class",
                ));
            }
        }
        match &expr.kind {
            HirExprKind::RcIntrinsic {
                op,
                receiver,
                value,
                result_ty,
                ..
            } => {
                let operands_valid = match op {
                    RcIntrinsicOp::New => receiver.is_none() && value.is_some(),
                    RcIntrinsicOp::Set => receiver.is_some() && value.is_some(),
                    RcIntrinsicOp::Clone
                    | RcIntrinsicOp::GetCopy
                    | RcIntrinsicOp::Downgrade
                    | RcIntrinsicOp::StrongCount
                    | RcIntrinsicOp::WeakCount
                    | RcIntrinsicOp::IsUnique
                    | RcIntrinsicOp::WeakClone
                    | RcIntrinsicOp::WeakUpgrade => receiver.is_some() && value.is_none(),
                };
                if !operands_valid || &expr.ty != result_ty {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: format!("Rc/Weak intrinsic {op:?}"),
                            reason: "operand shape or result type is inconsistent".to_string(),
                        },
                        expr.span.clone(),
                        "typed Rc/Weak intrinsic failed HIR validation",
                    ));
                }
                for operand in receiver.iter().chain(value.iter()) {
                    self.expr(operand);
                }
            }
            HirExprKind::BindingRef { resolved, name } => {
                if let ResolvedRef::Binding(binding) = resolved {
                    self.binding_reference_sites
                        .entry(*binding)
                        .or_default()
                        .push(expr.site);
                    self.binding_reference_targets.insert(expr.site, *binding);
                }
                if *resolved == ResolvedRef::Unresolved {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::UnresolvedSymbol { name: name.clone() },
                        expr.span.clone(),
                        "resolved HIR contains an unresolved binding reference",
                    ));
                }
            }
            HirExprKind::Binary { left, right, .. }
            | HirExprKind::IdentityCompare { left, right } => {
                self.expr(left);
                self.expr(right);
            }
            HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
                self.expr(operand);
            }
            HirExprKind::ConnAwaitRead {
                conn,
                source_anchor,
                ..
            } => {
                self.expr(conn);
                self.produced_value_source_anchor(source_anchor, expr.site);
            }
            HirExprKind::AwaitRestart { child } => self.expr(child),
            HirExprKind::ListenerAwaitAccept {
                listener,
                source_anchor,
                ..
            } => {
                self.expr(listener);
                self.produced_value_source_anchor(source_anchor, expr.site);
            }
            HirExprKind::ChannelRecvAwait {
                receiver,
                source_anchor,
                ..
            } => {
                self.expr(receiver);
                self.produced_value_source_anchor(source_anchor, expr.site);
            }
            HirExprKind::StreamRecvAwait {
                stream,
                source_anchor,
                ..
            } => {
                self.expr(stream);
                self.produced_value_source_anchor(source_anchor, expr.site);
            }
            HirExprKind::NumericCast {
                value,
                from_ty,
                to_ty,
            }
            | HirExprKind::SaturatingWidthCast {
                value,
                from_ty,
                to_ty,
            }
            | HirExprKind::TryWidthCast {
                value,
                from_ty,
                to_ty,
                ..
            } => {
                let node_name = match &expr.kind {
                    HirExprKind::SaturatingWidthCast { .. } => "saturating width cast",
                    HirExprKind::TryWidthCast { .. } => "try width cast",
                    _ => "numeric cast",
                };
                self.expr(value);
                if value.ty != *from_ty {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: node_name.to_string(),
                            reason: format!(
                                "cast source metadata {} disagrees with value type {}",
                                from_ty.user_facing(),
                                value.ty.user_facing()
                            ),
                        },
                        expr.span.clone(),
                        "cast source type metadata must match the lowered operand",
                    ));
                }
                let expected_expr_ty = if matches!(expr.kind, HirExprKind::TryWidthCast { .. }) {
                    ResolvedTy::Named {
                        name: "Option".to_string(),
                        args: vec![to_ty.clone()],
                        builtin: Some(BuiltinType::Option),
                        is_opaque: false,
                    }
                } else {
                    to_ty.clone()
                };
                if expr.ty != expected_expr_ty {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: node_name.to_string(),
                            reason: format!(
                                "cast result metadata {} disagrees with expression type {}",
                                expected_expr_ty.user_facing(),
                                expr.ty.user_facing()
                            ),
                        },
                        expr.span.clone(),
                        "cast result type metadata must match the expression type",
                    ));
                }
                if !from_ty.can_explicitly_numeric_cast_to(to_ty) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: node_name.to_string(),
                            reason: format!(
                                "cast from {} to {} is outside the checker-admitted numeric matrix",
                                from_ty.user_facing(),
                                to_ty.user_facing()
                            ),
                        },
                        expr.span.clone(),
                        "cast HIR node carries a non-numeric cast",
                    ));
                }
            }
            HirExprKind::TupleLiteral { elements } => {
                // Arity check: expr.ty must be ResolvedTy::Tuple with width
                // matching elements.len(). Checker-authoritative invariant: the
                // lowering pass reads tuple types from TypeCheckOutput.expr_types,
                // never re-derives them. A mismatch here surfaces a checker/HIR
                // boundary violation (poison in the side-table), not a user error.
                match &expr.ty {
                    ResolvedTy::Tuple(fields) => {
                        if fields.len() != elements.len() {
                            self.diagnostics.push(self.diagnostic(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: "tuple literal".to_string(),
                                    reason: format!(
                                        "tuple type has arity {} but literal has {} elements",
                                        fields.len(),
                                        elements.len()
                                    ),
                                },
                                expr.span.clone(),
                                "tuple literal element count does not match declared type width",
                            ));
                        }
                    }
                    // A zero-element tuple literal `()` whose resolved type is
                    // `ResolvedTy::Unit` is structurally valid: the checker
                    // accepts `Ok(())` and `Result<(),E>` returns (fixture
                    // `result_constructors_accept_unit_payloads` passes), but
                    // `()` resolves to `Unit` rather than `Tuple([])`, so it
                    // must be admitted here rather than rejected as a
                    // CheckerBoundaryViolation. Non-empty literals with a
                    // non-tuple type still fail closed below.
                    ResolvedTy::Unit if elements.is_empty() => {}
                    other => {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "tuple literal".to_string(),
                                reason: format!("expected tuple type, got {}", other.user_facing()),
                            },
                            expr.span.clone(),
                            "tuple literal must have tuple type",
                        ));
                    }
                }
                for elem in elements {
                    self.expr(elem);
                }
            }
            HirExprKind::Call {
                target,
                callee,
                args,
            } => {
                self.executable_call_target(target, expr);
                if let hew_types::CallTarget::User(declaration) = target {
                    self.user_call_targets
                        .insert(expr.site, declaration.clone());
                    self.user_call_arguments
                        .insert(expr.site, args.iter().map(|arg| arg.site).collect());
                }
                if let hew_types::CallTarget::Runtime(family) = target {
                    self.runtime_call_targets.insert(expr.site, *family);
                }
                self.expr(callee);
                for arg in args {
                    self.expr(arg);
                }
            }
            HirExprKind::SpawnedCall {
                callee,
                args,
                source_anchor,
                ..
            } => {
                self.expr(callee);
                for arg in args {
                    self.expr(arg);
                }
                self.produced_value_source_anchor(source_anchor, expr.site);
            }
            HirExprKind::Spawn { args, .. } => {
                for (_, arg) in args {
                    self.expr(arg);
                }
            }
            HirExprKind::ActorAsk {
                receiver,
                args,
                source_anchor,
                ..
            } => {
                self.expr(receiver);
                for arg in args {
                    self.expr(arg);
                }
                if let Some(anchor) = source_anchor {
                    self.produced_value_source_anchor(anchor, expr.site);
                }
            }
            HirExprKind::CallDynMethod {
                target,
                receiver,
                args,
                ..
            }
            | HirExprKind::ResolvedImplCall {
                target,
                receiver,
                args,
                ..
            }
            | HirExprKind::CallTraitMethodStatic {
                target,
                receiver,
                args,
                ..
            } => {
                self.executable_call_target(target, expr);
                if let HirExprKind::ResolvedImplCall { target_family, .. } = &expr.kind {
                    self.resolved_collection_calls
                        .insert(expr.site, *target_family);
                }
                self.expr(receiver);
                for arg in args {
                    self.expr(arg);
                }
            }
            HirExprKind::VarSelfMethodCall {
                call_target,
                receiver,
                args,
                ..
            } => {
                self.executable_call_target(call_target, expr);
                self.expr(receiver);
                for arg in args {
                    self.expr(arg);
                }
            }
            HirExprKind::ActorSend { receiver, args, .. }
            | HirExprKind::ActorGenStream { receiver, args, .. } => {
                self.expr(receiver);
                for arg in args {
                    self.expr(arg);
                }
            }
            HirExprKind::RemoteActorAsk {
                receiver,
                msg,
                timeout_ms,
                ..
            } => {
                self.expr(receiver);
                self.expr(msg);
                self.expr(timeout_ms);
            }
            HirExprKind::NumericMethod { receiver, arg, .. } => {
                self.expr(receiver);
                self.expr(arg);
            }
            HirExprKind::Block(block) => self.block(block),
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => {
                self.expr(condition);
                self.expr(then_expr);
                if let Some(else_expr) = else_expr {
                    self.expr(else_expr);
                }
            }
            HirExprKind::StructInit { fields, base, .. } => {
                for (_, field) in fields {
                    self.expr(field);
                }
                if let Some(base) = base {
                    self.expr(base);
                }
            }
            HirExprKind::FieldAccess { object, .. } => {
                self.expr(object);
            }
            HirExprKind::MachineFieldAccess { source_anchor, .. }
            | HirExprKind::MachineEventFieldAccess { source_anchor, .. } => {
                if let Some(anchor) = source_anchor {
                    self.produced_value_source_anchor(anchor, expr.site);
                }
            }
            HirExprKind::ContextReader { .. }
            | HirExprKind::Literal(_)
            | HirExprKind::RegexLiteralRef { .. }
            | HirExprKind::Continue { .. }
            | HirExprKind::ActorSelf => {}
            HirExprKind::Scope { body }
            | HirExprKind::ForkBlock { body, .. }
            | HirExprKind::Loop { body, .. } => self.block(body),
            HirExprKind::ScopeDeadline { duration, body } => {
                self.expr(duration);
                self.block(body);
            }
            HirExprKind::AwaitTask {
                binding_id,
                source_anchor,
                ..
            } => {
                self.produced_value_source_anchor(source_anchor, expr.site);
                // Verify the binding-id referenced by the await is known to the verifier.
                // If it's not in `self.bindings`, that indicates a dangling reference.
                if !self.bindings.contains(binding_id) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::DanglingRef {
                            resolved: ResolvedRef::Binding(*binding_id),
                        },
                        expr.span.clone(),
                        "await-task references a binding that was not declared in resolved HIR",
                    ));
                }
            }
            HirExprKind::Select(select) => {
                for arm in &select.arms {
                    match &arm.kind {
                        crate::node::HirSelectArmKind::StreamNext { stream } => {
                            self.expr(stream);
                        }
                        crate::node::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                            self.expr(actor);
                            for arg in args {
                                self.expr(arg);
                            }
                        }
                        crate::node::HirSelectArmKind::TaskAwait { task } => {
                            self.expr(task);
                        }
                        crate::node::HirSelectArmKind::ChannelRecv { receiver, .. } => {
                            self.expr(receiver);
                        }
                        crate::node::HirSelectArmKind::AfterTimer { duration } => {
                            self.expr(duration);
                        }
                    }
                    self.expr(&arm.body);
                }
            }
            HirExprKind::Join(join) => {
                for branch in &join.branches {
                    self.expr(&branch.actor);
                    for arg in &branch.args {
                        self.expr(arg);
                    }
                }
            }
            HirExprKind::SpawnLambdaActor { body, captures, .. } => {
                // The lambda body is a child expression — recurse for
                // node/site/diagnostic coverage. The capture set is
                // metadata produced by the resolver; verify each
                // captured binding id was declared somewhere in the
                // HIR (catches a resolver bug that records a freed
                // binding id).
                self.nested_callable_depth += 1;
                self.expr(body);
                self.nested_callable_depth -= 1;
                for capture in captures {
                    if !self.bindings.contains(&capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DanglingRef {
                                resolved: ResolvedRef::Binding(capture.binding),
                            },
                            expr.span.clone(),
                            "lambda-actor capture references a binding not declared in resolved HIR",
                        ));
                    }
                }
            }
            HirExprKind::Closure {
                params,
                body,
                captures,
                ..
            } => {
                // Register the closure's own parameters BEFORE walking the
                // body: a nested closure in the body may capture one of these
                // params (`|x| { |y| x + y }`), and the capture-declared check
                // below resolves `capture.binding` against `self.bindings`.
                // Without this the inner capture of an outer-closure param is
                // a spurious DanglingRef — exactly the path actor methods and
                // named-fn bodies already register (see the actor-method and
                // Function arms). Mirrors that registration here.
                for param in params {
                    self.binding(param.id, param.span.clone());
                }
                self.nested_callable_depth += 1;
                self.expr(body);
                self.nested_callable_depth -= 1;
                let mut seen = std::collections::HashSet::new();
                for capture in captures {
                    if !self.bindings.contains(&capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DanglingRef {
                                resolved: ResolvedRef::Binding(capture.binding),
                            },
                            expr.span.clone(),
                            "closure capture references a binding not declared in resolved HIR",
                        ));
                    }
                    if !seen.insert(capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DuplicateBindingId {
                                id: capture.binding,
                            },
                            expr.span.clone(),
                            "closure capture list contains the same binding more than once",
                        ));
                    }
                }
            }
            HirExprKind::GenBlock {
                body,
                yield_ty,
                return_ty,
                captures,
            } => {
                // Each gen capture must reference a binding that exists in the
                // resolved HIR, and no binding may appear twice (the env field
                // layout is keyed by binding — duplicates would collide). Mirrors
                // the closure/lambda capture verification above.
                //
                // The declared-binding requirement applies only to `Local`
                // captures (a `gen fn`'s params, a `gen {}` block's outer
                // locals, or a `receive gen fn` handler param) — those resolve
                // to a real `HirBinding` declaration and a dangling one is a
                // resolver bug. An `ActorStateField` capture is intentionally
                // synthetic: `lower_actor_generator_body` mints its binding id
                // while binding the actor's state fields into scope, and no
                // `HirBinding` declaration node carries that id, so it will
                // never be in `self.bindings`. Trust the HIR-authority source
                // tag (`type-info-survival` — do not re-derive) and exempt it
                // from the DanglingRef gate. The duplicate-id guard still
                // covers ALL captures (the env layout keys on binding id
                // regardless of source).
                let mut seen_captures = std::collections::HashSet::new();
                for capture in captures {
                    let checked_local = matches!(capture.source, HirGenCaptureSource::Local);
                    if checked_local && !self.bindings.contains(&capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DanglingRef {
                                resolved: ResolvedRef::Binding(capture.binding),
                            },
                            expr.span.clone(),
                            "generator capture references a binding not declared in resolved HIR",
                        ));
                    }
                    if !seen_captures.insert(capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DuplicateBindingId {
                                id: capture.binding,
                            },
                            expr.span.clone(),
                            "generator capture list contains the same binding more than once",
                        ));
                    }
                }
                match &expr.ty {
                    ResolvedTy::Named { name, args, .. }
                        if name == "Generator" && args.len() == 2 =>
                    {
                        if args[0] != *yield_ty || args[1] != *return_ty {
                            self.diagnostics.push(self.diagnostic(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: "gen block".to_string(),
                                    reason: format!(
                                        "GenBlock carries Yield={}, Return={} but expr type is {}",
                                        yield_ty.user_facing(),
                                        return_ty.user_facing(),
                                        expr.ty.user_facing()
                                    ),
                                },
                                expr.span.clone(),
                                "gen block HIR metadata disagrees with its expression type",
                            ));
                        }
                    }
                    other => {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "gen block".to_string(),
                                reason: format!(
                                    "expected Generator<Yield, Return>, got {}",
                                    other.user_facing()
                                ),
                            },
                            expr.span.clone(),
                            "gen block HIR expression does not have Generator type",
                        ));
                    }
                }
                self.nested_callable_depth += 1;
                self.block(body);
                self.nested_callable_depth -= 1;
            }
            HirExprKind::Yield { value, yield_ty } => {
                if expr.ty != ResolvedTy::Unit {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "yield".to_string(),
                            reason: format!(
                                "yield expression has non-unit result type {}",
                                expr.ty.user_facing()
                            ),
                        },
                        expr.span.clone(),
                        "yield HIR expression result type must be unit",
                    ));
                }
                if let Some(value) = value {
                    self.expr(value);
                    if value.ty != *yield_ty {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "yield".to_string(),
                                reason: format!(
                                    "yield value type {} disagrees with enclosing Yield {}",
                                    value.ty.user_facing(),
                                    yield_ty.user_facing()
                                ),
                            },
                            value.span.clone(),
                            "yield value type does not match enclosing generator Yield type",
                        ));
                    }
                }
            }
            HirExprKind::TupleIndex { tuple, .. } => {
                self.expr(tuple);
            }
            HirExprKind::Index { container, index } => {
                self.expr(container);
                self.expr(index);
            }
            HirExprKind::Slice {
                container,
                start,
                end,
                inclusive: _,
            } => {
                self.expr(container);
                if let Some(s) = start {
                    self.expr(s);
                }
                if let Some(e) = end {
                    self.expr(e);
                }
            }
            HirExprKind::CoerceToDynTrait { value, .. } => {
                self.expr(value);
            }
            HirExprKind::MachineEmit { fields, .. } => {
                for (_, field_val) in fields {
                    self.expr(field_val);
                }
            }
            HirExprKind::MachineStep {
                receiver, event, ..
            }
            | HirExprKind::MachineTakeEmits {
                receiver, event, ..
            } => {
                self.expr(receiver);
                self.expr(event);
            }
            HirExprKind::CancellationTokenIsCancelled { receiver }
            | HirExprKind::GeneratorNext { receiver, .. }
            | HirExprKind::MachineStateName { receiver, .. }
            | HirExprKind::RecordCloneCall { src: receiver, .. } => {
                self.expr(receiver);
            }
            HirExprKind::SubsumedValue { source, .. } => self.expr(source),
            HirExprKind::MachineVariantCtor { payload, .. } => {
                self.machine_variant_payloads.insert(
                    expr.site,
                    payload
                        .iter()
                        .flatten()
                        .map(|(_, value)| value.site)
                        .collect(),
                );
                if let Some(fields) = payload {
                    for (_, val) in fields {
                        self.expr(val);
                    }
                }
            }
            HirExprKind::While {
                condition, body, ..
            } => {
                self.expr(condition);
                self.block(body);
            }
            HirExprKind::Match { scrutinee, arms } => {
                self.expr(scrutinee);
                for arm in arms {
                    match &arm.predicate {
                        HirMatchArmPredicate::Literal { lit, ty } => {
                            self.match_literal_predicate(lit, ty, arm.span.clone());
                        }
                        HirMatchArmPredicate::RecordProject { ty } => {
                            self.match_record_project_predicate(
                                ty,
                                &scrutinee.ty,
                                arm.span.clone(),
                            );
                        }
                        HirMatchArmPredicate::TupleProject { arity } => {
                            self.match_tuple_project_predicate(
                                *arity,
                                &scrutinee.ty,
                                &arm.bindings,
                                arm.span.clone(),
                            );
                        }
                        HirMatchArmPredicate::Wildcard
                        | HirMatchArmPredicate::Binding { .. }
                        | HirMatchArmPredicate::EnumVariant { .. }
                        | HirMatchArmPredicate::Regex { .. } => {}
                    }
                    for predicate in &arm.payload_predicates {
                        self.match_literal_predicate(
                            &predicate.literal,
                            &predicate.ty,
                            arm.span.clone(),
                        );
                    }
                    for binding in &arm.bindings {
                        self.binding(binding.binding, arm.span.clone());
                    }
                    if let Some(guard) = &arm.guard {
                        self.expr(guard);
                    }
                    self.expr(&arm.body);
                }
            }
            HirExprKind::ForRange {
                binding,
                start,
                end,
                step,
                body,
                ..
            } => {
                self.binding(binding.id, binding.span.clone());
                self.expr(start);
                self.expr(end);
                self.expr(step);
                self.block(body);
            }
            HirExprKind::WhileLet {
                scrutinee,
                bindings,
                body,
                ..
            } => {
                self.expr(scrutinee);
                for binding in bindings {
                    // While-let payload bindings are scoped to the body
                    // (one fresh BindingId allocated by HIR lowering);
                    // register them here so the duplicate-binding check
                    // covers the new shape, mirroring `Match` arm bindings.
                    self.binding(binding.binding, expr.span.clone());
                }
                self.block(body);
            }
            HirExprKind::IfLet {
                scrutinee,
                bindings,
                body,
                else_body,
                ..
            } => {
                self.expr(scrutinee);
                for binding in bindings {
                    // If-let payload bindings are scoped to the then-body;
                    // register them here mirroring `WhileLet` and `Match`.
                    self.binding(binding.binding, expr.span.clone());
                }
                self.block(body);
                if let Some(eb) = else_body {
                    self.block(eb);
                }
            }
            HirExprKind::Break { value, .. } | HirExprKind::Return { value } => {
                if let Some(value) = value {
                    self.expr(value);
                }
            }
            HirExprKind::Unsupported(reason) => {
                if !self.diagnostics.iter().any(|diag| {
                    diag.span == expr.span
                        && matches!(diag.kind, HirDiagnosticKind::NotYetImplemented { .. })
                }) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::NotYetImplemented {
                            construct: reason.clone(),
                            owning_pass: "hir-lowering".to_string(),
                        },
                        expr.span.clone(),
                        "verifier: Unsupported HIR node reached verification without a prior diagnostic",
                    ));
                }
            }
        }
    }

    fn binding(&mut self, id: BindingId, span: std::ops::Range<usize>) {
        if !self.bindings.insert(id) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::DuplicateBindingId { id },
                span,
                "binding id reused inside resolved HIR",
            ));
        }
    }

    fn match_literal_predicate(&mut self, lit: &HirLiteral, ty: &ResolvedTy, span: Range<usize>) {
        let valid = match (lit, ty) {
            (HirLiteral::Integer(_), ty) => ty.is_integer_literal_match_scrutinee(),
            (HirLiteral::Float(_), ResolvedTy::F32 | ResolvedTy::F64)
            | (HirLiteral::Bool(_), ResolvedTy::Bool)
            | (HirLiteral::Char(_), ResolvedTy::Char)
            | (HirLiteral::String(_), ResolvedTy::String) => true,
            _ => false,
        };
        if !valid {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "unsupported top-level literal match predicate {lit:?}: {ty:?}"
                    ),
                    owning_pass: "match-literal".to_string(),
                },
                span,
                "literal match predicates are currently limited to integers, floats, bool, char, and string",
            ));
        }
    }

    fn match_record_project_predicate(
        &mut self,
        predicate_ty: &ResolvedTy,
        scrutinee_ty: &ResolvedTy,
        span: Range<usize>,
    ) {
        if predicate_ty != scrutinee_ty || !matches!(predicate_ty, ResolvedTy::Named { .. }) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "record match project".to_string(),
                    reason: format!(
                        "predicate type {} disagrees with scrutinee type {}",
                        predicate_ty.user_facing(),
                        scrutinee_ty.user_facing()
                    ),
                },
                span,
                "record project predicates must carry the resolved record scrutinee type",
            ));
        }
    }

    fn match_tuple_project_predicate(
        &mut self,
        arity: u32,
        scrutinee_ty: &ResolvedTy,
        bindings: &[crate::node::HirMatchArmBinding],
        span: Range<usize>,
    ) {
        let ResolvedTy::Tuple(items) = scrutinee_ty else {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "tuple match project".to_string(),
                    reason: format!(
                        "tuple project predicate on non-tuple scrutinee {}",
                        scrutinee_ty.user_facing()
                    ),
                },
                span,
                "tuple project predicates require a tuple-typed scrutinee",
            ));
            return;
        };
        if usize::try_from(arity).ok() != Some(items.len()) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "tuple match project".to_string(),
                    reason: format!(
                        "predicate arity {arity} disagrees with scrutinee arity {}",
                        items.len()
                    ),
                },
                span.clone(),
                "tuple project arity must match the scrutinee tuple width",
            ));
        }
        if let Some(binding) = bindings.iter().find(|binding| {
            usize::try_from(binding.field_idx).map_or(true, |idx| idx >= items.len())
        }) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "tuple match project".to_string(),
                    reason: format!(
                        "binding `{}` projects field {} from arity {} tuple",
                        binding.name,
                        binding.field_idx,
                        items.len()
                    ),
                },
                span,
                "tuple project binding indices must be within tuple arity",
            ));
        }
    }

    fn site(&mut self, id: SiteId, span: Range<usize>) {
        self.site_spans.entry(id).or_insert_with(|| HirSiteSource {
            span: span.clone(),
            source_module: self.current_source_module.clone(),
        });
        if !self.sites.insert(id) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::DuplicateSiteId { id },
                span,
                "site id reused inside resolved HIR",
            ));
        }
    }

    fn node(&mut self, id: HirNodeId, span: std::ops::Range<usize>) {
        if !self.nodes.insert(id) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::DuplicateNodeId { id },
                span,
                "HIR node id reused inside resolved HIR",
            ));
        }
    }

    fn item_source_module(module: &HirModule, item: &HirItem) -> Option<String> {
        let id = match item {
            HirItem::Function(item) => item.id,
            HirItem::TypeDecl(item) => item.id,
            HirItem::Machine(item) => item.id,
            HirItem::Record(item) => item.id,
            HirItem::Actor(item) => item.id,
            HirItem::Supervisor(item) => item.id,
            HirItem::Impl(item) => item.id,
            HirItem::ExternFn(item) => item.id,
            HirItem::Const(item) => item.id,
        };
        module.diagnostic_source_modules.get(&id).cloned()
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        sync::Arc,
    };

    use super::verify_hir;
    use crate::ids::IdGen;
    use crate::node::{
        HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule,
        HirVarSelfMethodTarget,
    };
    use crate::{IntentKind, TypeClassTable, ValueClass};
    use hew_types::{CallTarget, ImplId, MethodTargetFamily, ResolvedTy, VecMethod};

    fn unit_expr(ids: &mut IdGen) -> HirExpr {
        HirExpr {
            node: ids.node(),
            site: ids.site(),
            ty: ResolvedTy::Unit,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::Unit),
            span: 0..0,
        }
    }

    fn executable_expr(ids: &mut IdGen, kind: HirExprKind) -> HirExpr {
        HirExpr {
            node: ids.node(),
            site: ids.site(),
            ty: ResolvedTy::Unit,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind,
            span: 0..0,
        }
    }

    fn function_with_tail(ids: &mut IdGen, name: &str, tail: HirExpr) -> HirItem {
        HirItem::Function(HirFn {
            id: ids.item(),
            node: ids.node(),
            declaration: hew_types::DefId::new(name),
            name: name.to_string(),
            type_params: Vec::new(),
            params: Vec::new(),
            return_ty: ResolvedTy::Unit,
            body: HirBlock {
                node: ids.node(),
                scope: ids.scope(),
                statements: Vec::new(),
                tail: Some(Box::new(tail)),
                ty: ResolvedTy::Unit,
                span: 0..0,
            },
            span: 0..0,
            is_generator: false,
            intrinsic_id: None,
        })
    }

    fn module(items: Vec<HirItem>) -> HirModule {
        HirModule {
            items,
            produced_value_facts: HashMap::new(),
            diagnostic_source_modules: HashMap::new(),
            root_item_ids: HashSet::default(),
            caller_visible_param_projections: HashSet::default(),
            wire_layouts: Arc::new(HashMap::new()),
            type_classes: TypeClassTable::default(),
            monomorphisations: Vec::new(),
            call_site_type_args: HashMap::new(),
            vec_generic_element_abi: HashMap::new(),
            record_layouts: Vec::new(),
            enum_layouts: Vec::new(),
            machine_instantiations: Vec::new(),
            supervisor_child_slots: HashMap::new(),
            pool_accessor_sites: HashMap::new(),
            regex_literals: Vec::new(),
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "the verifier regression exercises every executable call carrier in one shared fixture"
    )]
    fn unsupported_targets_are_rejected_for_every_executable_call_carrier() {
        let mut ids = IdGen::default();
        let unsupported = |carrier: &str| CallTarget::Unsupported {
            reason: format!("unsupported {carrier}"),
        };

        let ordinary_callee = unit_expr(&mut ids);
        let ordinary = executable_expr(
            &mut ids,
            HirExprKind::Call {
                target: unsupported("ordinary call"),
                callee: Box::new(ordinary_callee),
                args: Vec::new(),
            },
        );
        let indirect_callee = unit_expr(&mut ids);
        let indirect = executable_expr(
            &mut ids,
            HirExprKind::Call {
                target: CallTarget::IndirectFunctionValue,
                callee: Box::new(indirect_callee),
                args: Vec::new(),
            },
        );
        let dynamic_receiver = unit_expr(&mut ids);
        let dynamic = executable_expr(
            &mut ids,
            HirExprKind::CallDynMethod {
                receiver: Box::new(dynamic_receiver),
                target: unsupported("dynamic method call"),
                trait_name: "T".to_string(),
                method_name: "m".to_string(),
                slot: 0,
                args: Vec::new(),
                ret_ty: ResolvedTy::Unit,
                signature: Box::default(),
            },
        );
        let resolved_impl_receiver = unit_expr(&mut ids);
        let resolved_impl = executable_expr(
            &mut ids,
            HirExprKind::ResolvedImplCall {
                receiver: Box::new(resolved_impl_receiver),
                target: unsupported("resolved impl call"),
                impl_id: ImplId(0),
                method_name: "len".to_string(),
                target_symbol: "hew_vec_len_i64".to_string(),
                target_family: MethodTargetFamily::Vec(VecMethod::Len),
                type_args: Vec::new(),
                args: Vec::new(),
                ret_ty: ResolvedTy::Unit,
            },
        );
        let static_trait_receiver = unit_expr(&mut ids);
        #[allow(
            deprecated,
            reason = "the verifier must continue rejecting the deprecated static carrier"
        )]
        let static_trait = executable_expr(
            &mut ids,
            HirExprKind::CallTraitMethodStatic {
                receiver: Box::new(static_trait_receiver),
                target: unsupported("static trait call"),
                receiver_type_param: "T".to_string(),
                bound_trait: "T".to_string(),
                declaring_trait: "T".to_string(),
                method_name: "m".to_string(),
                args: Vec::new(),
                ret_ty: ResolvedTy::Unit,
            },
        );
        let var_self_receiver = unit_expr(&mut ids);
        let var_self = executable_expr(
            &mut ids,
            HirExprKind::VarSelfMethodCall {
                receiver: Box::new(var_self_receiver),
                call_target: unsupported("var-self method call"),
                target: HirVarSelfMethodTarget::Direct,
                args: Vec::new(),
                ret_ty: ResolvedTy::Unit,
                receiver_ty: ResolvedTy::Unit,
            },
        );

        let module = module(vec![
            function_with_tail(&mut ids, "ordinary", ordinary),
            function_with_tail(&mut ids, "indirect", indirect),
            function_with_tail(&mut ids, "dynamic", dynamic),
            function_with_tail(&mut ids, "resolved_impl", resolved_impl),
            function_with_tail(&mut ids, "static_trait", static_trait),
            function_with_tail(&mut ids, "var_self", var_self),
        ]);
        let diagnostics = verify_hir(&module);
        let mut reasons: Vec<_> = diagnostics
            .iter()
            .filter_map(|diagnostic| match &diagnostic.kind {
                crate::HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                    if name == "call target" =>
                {
                    Some(reason.as_str())
                }
                _ => None,
            })
            .collect();
        reasons.sort_unstable();

        assert_eq!(
            reasons,
            vec![
                "unsupported dynamic method call",
                "unsupported ordinary call",
                "unsupported resolved impl call",
                "unsupported static trait call",
                "unsupported var-self method call",
            ],
            "every executable HIR call carrier must reject an Unsupported checker target, while a valid indirect function-value call remains executable"
        );
    }
}
