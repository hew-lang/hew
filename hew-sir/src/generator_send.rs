//! Conditional generator Send from checked captures and retained SIR ownership.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use hew_types::{ResolvedTy, SendFact, TypeInstanceKey};

use crate::{
    BlockId, CallResult, CallableId, CheckedFunction, ClosureId, Operand, OwnerRoot, PlaceBase,
    PlaceOrigin, SemFunction, SemModule, SemOp, SemOpKind, SemTerminator, SirDiagnostic,
    SirDiagnosticKind, ValueId,
};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Capability {
    safe: bool,
    generators: BTreeSet<ClosureId>,
    members: Vec<Self>,
}

impl Capability {
    fn known() -> Self {
        Self {
            safe: true,
            ..Self::default()
        }
    }

    fn aggregate(members: Vec<Self>) -> Self {
        Self {
            safe: members.iter().all(|member| member.safe),
            generators: members
                .iter()
                .flat_map(|member| member.generators.iter().copied())
                .collect(),
            members,
        }
    }

    fn join(&mut self, other: &Self) {
        self.safe &= other.safe;
        self.generators.extend(&other.generators);
        if self.members.len() == other.members.len() {
            for (left, right) in self.members.iter_mut().zip(&other.members) {
                left.join(right);
            }
        } else {
            self.members.clear();
        }
    }

    fn field(&self, index: u32) -> Self {
        self.members
            .get(index as usize)
            .cloned()
            .unwrap_or_else(|| self.clone())
    }
}

type State = BTreeMap<PlaceBase, Capability>;

fn static_capability(module: &SemModule, ty: &ResolvedTy) -> Capability {
    if module
        .type_facts
        .get(&TypeInstanceKey(ty.clone()))
        .is_some_and(|facts| facts.send == SendFact::Known(true))
    {
        Capability::known()
    } else {
        Capability::default()
    }
}

fn refine(module: &SemModule, ty: &ResolvedTy, value: Capability) -> Capability {
    let fact = module
        .type_facts
        .get(&TypeInstanceKey(ty.clone()))
        .map(|facts| facts.send);
    match fact {
        Some(SendFact::Known(true)) => Capability::known(),
        Some(SendFact::DeferredToClosureFacts | SendFact::DeferredToGeneratorFacts) => value,
        _ if matches!(ty.callable_body(), ResolvedTy::Function { .. }) => value,
        _ => Capability {
            members: value.members,
            ..Capability::default()
        },
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "verify concrete producer contracts and report missing evidence at the Send boundary"
)]
pub(crate) fn verify(
    module: &SemModule,
    checked: &BTreeMap<CallableId, CheckedFunction>,
) -> Vec<SirDiagnostic> {
    let forwarding: BTreeMap<_, _> = module
        .callables
        .iter()
        .filter(|callable| checked.contains_key(&callable.id))
        .filter_map(|callable| {
            forwarded_inputs(module, callable).map(|inputs| (callable.id, inputs))
        })
        .collect();
    let mut pending = VecDeque::new();
    for closure in &module.closures {
        if let Some(site) = closure.generator_send_sites.first() {
            pending.push_back((closure.id, site.clone()));
        }
    }
    let mut verified = BTreeSet::new();
    let mut diagnostics = Vec::new();
    while let Some((id, site)) = pending.pop_front() {
        if !verified.insert(id) {
            continue;
        }
        let refuse = |function: Option<&SemFunction>, message: String| SirDiagnostic {
            function: function.map_or_else(String::new, |function| function.name.clone()),
            callable: function.map(|function| function.callable),
            kind: SirDiagnosticKind::InvalidGeneratorSend {
                message,
                site: site.clone(),
            },
        };
        let Some(producer) = module.closures.iter().find(|closure| closure.id == id) else {
            diagnostics.push(refuse(
                None,
                "generator Send is missing its concrete producer".into(),
            ));
            continue;
        };
        if producer.entries.is_empty() {
            diagnostics.push(refuse(
                None,
                "generator Send is missing its checked body".into(),
            ));
            continue;
        }
        let Some((yielded, returned)) = producer
            .generator_type
            .as_ref()
            .and_then(ResolvedTy::generator_parts)
        else {
            diagnostics.push(refuse(
                None,
                "generator Send is missing its producer contract".into(),
            ));
            continue;
        };
        if let Some(output) = [yielded, returned]
            .into_iter()
            .find(|output| !static_capability(module, output).safe)
        {
            diagnostics.push(refuse(
                None,
                format!(
                    "generator cannot be sent: its output `{}` is not proven Send",
                    output.user_facing()
                ),
            ));
            continue;
        }
        for entry in &producer.entries {
            let Some(function) = module
                .functions
                .iter()
                .find(|function| function.callable == entry.body)
            else {
                diagnostics.push(refuse(
                    None,
                    "generator Send is missing its checked body".into(),
                ));
                continue;
            };
            let Some(analysis) = checked.get(&entry.body) else {
                diagnostics.push(refuse(
                    Some(function),
                    "generator Send is missing checked lifetime evidence".into(),
                ));
                continue;
            };
            let states = capability_states(module, function, analysis, &forwarding);
            for (block, retained) in analysis.place_lifetimes().generator_residency() {
                let Some(state) = states.get(block) else {
                    diagnostics.push(refuse(
                        Some(function),
                        "generator Send is missing retained value evidence".into(),
                    ));
                    continue;
                };
                for retained in retained {
                    let value = read(function, state, *retained);
                    if !value.safe {
                        let name = match retained {
                            PlaceBase::Place(place) => function
                                .places
                                .iter()
                                .find(|item| item.id == *place)
                                .map(|item| item.ty.user_facing().to_string()),
                            PlaceBase::Value(value) => value_types(function)
                                .get(value)
                                .map(|ty| ty.user_facing().to_string()),
                        }
                        .unwrap_or_else(|| "value".into());
                        diagnostics.push(refuse(
                            Some(function),
                            format!(
                                "generator cannot be sent: retained `{name}` is not proven Send"
                            ),
                        ));
                        break;
                    }
                    pending.extend(value.generators.iter().map(|id| (*id, site.clone())));
                }
            }
        }
    }
    diagnostics
}

/// A checked single-binder `T -> T` contract cannot manufacture another T.
/// Every T-valued input remains a possible result. Nested occurrences (such
/// as `fn() -> T`) can introduce new values and deliberately get no summary.
fn forwarded_inputs(module: &SemModule, callable: &crate::SemCallable) -> Option<Vec<usize>> {
    let crate::CallableInstance::Generic(instance) = &callable.instance else {
        return None;
    };
    if callable.kind != crate::SemCallableKind::HewDirect
        || callable.signature.return_ty.generator_parts().is_none()
    {
        return None;
    }
    let template = module.generic_template(&instance.template)?;
    let ResolvedTy::TypeParam { name: parameter } = &template.signature.return_ty else {
        return None;
    };
    if !template.unconstrained_parameters || template.type_params.as_slice() != [*parameter] {
        return None;
    }
    let mut inputs = Vec::new();
    for (index, input) in template.signature.params.iter().enumerate() {
        if input.ty == template.signature.return_ty {
            inputs.push(index);
        } else if input.ty.to_ty().mentions_type_param(*parameter) {
            return None;
        }
    }
    (!inputs.is_empty()).then_some(inputs)
}

fn value_types(function: &SemFunction) -> BTreeMap<ValueId, ResolvedTy> {
    let mut types = BTreeMap::new();
    for param in &function.params {
        types.insert(param.value, param.ty.clone());
    }
    for block in &function.blocks {
        for arg in &block.args {
            types.insert(arg.value, arg.ty.clone());
        }
        for operation in &block.ops {
            for result in &operation.results {
                types.insert(result.id, result.ty.clone());
            }
        }
        block.terminator.visit_results(|result| {
            types.insert(result.id, result.ty.clone());
        });
    }
    types
}

fn read(function: &SemFunction, state: &State, base: PlaceBase) -> Capability {
    if let Some(value) = state.get(&base) {
        return value.clone();
    }
    let PlaceBase::Place(id) = base else {
        return Capability::default();
    };
    let Some(place) = function.places.iter().find(|place| place.id == id) else {
        return Capability::default();
    };
    match place.origin {
        PlaceOrigin::Capture { .. } => Capability::known(),
        PlaceOrigin::Aggregate { base, field, .. } => read(function, state, base).field(field),
        _ => Capability::default(),
    }
}

fn write(
    function: &SemFunction,
    analysis: &CheckedFunction,
    state: &mut State,
    base: PlaceBase,
    value: &Capability,
) {
    state.insert(base, value.clone());
    let root = match base {
        PlaceBase::Value(id) => OwnerRoot::Value(id),
        PlaceBase::Place(id) => OwnerRoot::Local(id),
    };
    if let Some(leaves) = analysis.place_plan().leaves(root) {
        for leaf in leaves {
            let Some(projection) = analysis.place_plan().projection(*leaf) else {
                continue;
            };
            let mut member = value.clone();
            for step in &projection.path {
                member = member.field(step.field);
            }
            state.insert(PlaceBase::Place(*leaf), member);
        }
    } else if let PlaceBase::Place(id) = base {
        // Assignment to a subtree updates its declared leaf partition.
        if let Some(projection) = analysis.place_plan().projection(id) {
            for leaf in &projection.leaves {
                let Some(child) = analysis.place_plan().projection(*leaf) else {
                    continue;
                };
                let mut member = value.clone();
                for step in child.path.iter().skip(projection.path.len()) {
                    member = member.field(step.field);
                }
                state.insert(PlaceBase::Place(*leaf), member);
            }
        }
    }
    // Actor state never acquires transferable ownership through a store.
    for place in &function.places {
        if matches!(place.origin, PlaceOrigin::ActorState { .. }) {
            state.insert(PlaceBase::Place(place.id), Capability::default());
        }
    }
}

fn capability_states(
    module: &SemModule,
    function: &SemFunction,
    analysis: &CheckedFunction,
    forwarding: &BTreeMap<CallableId, Vec<usize>>,
) -> BTreeMap<BlockId, State> {
    let mut initial = State::new();
    // Constructor captures have already been proved at each source Send
    // admission. This reusable body proof is conditional on those values.
    for param in &function.params {
        write(
            function,
            analysis,
            &mut initial,
            PlaceBase::Value(param.value),
            &Capability::known(),
        );
    }
    let blocks: BTreeMap<_, _> = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect();
    let mut incoming = BTreeMap::from([(function.entry, initial)]);
    let mut outgoing = BTreeMap::new();
    let mut pending = BTreeSet::from([function.entry]);
    while let Some(id) = pending.pop_first() {
        let block = blocks[&id];
        let mut state = incoming[&id].clone();
        for operation in &block.ops {
            apply_operation(module, function, analysis, operation, &mut state);
        }
        outgoing.insert(id, state.clone());
        block.terminator.visit_results(|result| {
            write(
                function,
                analysis,
                &mut state,
                PlaceBase::Value(result.id),
                &static_capability(module, &result.ty),
            );
        });
        if let Some((result, capability)) =
            call_result_capability(module, function, &state, forwarding, &block.terminator)
        {
            write(
                function,
                analysis,
                &mut state,
                PlaceBase::Value(result),
                &capability,
            );
        }
        block.terminator.visit_successors(|edge| {
            if !analysis.place_lifetimes().is_reachable(edge.target) {
                return;
            }
            let mut next = state.clone();
            for (source, target) in edge.args.iter().zip(&blocks[&edge.target].args) {
                let value = read(function, &state, PlaceBase::Value(source.value));
                write(
                    function,
                    analysis,
                    &mut next,
                    PlaceBase::Value(target.value),
                    &value,
                );
                if let Ok(leaves) = analysis.place_plan().transfer(source.value, target.value) {
                    for (source, target) in leaves {
                        next.insert(
                            PlaceBase::Place(target),
                            read(function, &state, PlaceBase::Place(source)),
                        );
                    }
                }
            }
            let changed = match incoming.get_mut(&edge.target) {
                None => {
                    incoming.insert(edge.target, next);
                    true
                }
                Some(previous) => {
                    let before = previous.clone();
                    for (base, value) in next {
                        previous
                            .entry(base)
                            .and_modify(|previous| previous.join(&value))
                            .or_insert(value);
                    }
                    *previous != before
                }
            };
            if changed {
                pending.insert(edge.target);
            }
        });
    }
    outgoing
}

fn call_result_capability(
    module: &SemModule,
    function: &SemFunction,
    state: &State,
    forwarding: &BTreeMap<CallableId, Vec<usize>>,
    terminator: &SemTerminator,
) -> Option<(ValueId, Capability)> {
    let SemTerminator::Call {
        callee,
        args,
        result: CallResult::Value(result),
        ..
    } = terminator
    else {
        return None;
    };
    if let Some(inputs) = forwarding.get(callee) {
        let mut value = Capability::aggregate(
            inputs
                .iter()
                .map(|index| {
                    read(
                        function,
                        state,
                        PlaceBase::Value(args[*index].operand.value),
                    )
                })
                .collect(),
        );
        value.members.clear();
        return Some((result.id, value));
    }
    let target = module
        .callables
        .iter()
        .find(|target| target.id == *callee)?;
    let ResolvedTy::Generator { producers, .. } = &result.ty else {
        return None;
    };
    if producers.is_empty()
        || producers.iter().any(|producer| {
            producer.body != hew_types::ty::EffectBody::Generator(target.declaration)
        })
    {
        return None;
    }
    let mut value = Capability::aggregate(
        args.iter()
            .map(|argument| read(function, state, PlaceBase::Value(argument.operand.value)))
            .collect(),
    );
    value.members.clear();
    for closure in &module.closures {
        if matches!(&closure.generator_type, Some(ResolvedTy::Generator { producers: known, .. }) if known.iter().any(|producer| producers.contains(producer)))
        {
            value.generators.insert(closure.id);
        }
    }
    value.safe &= !value.generators.is_empty();
    Some((result.id, value))
}

fn apply_operation(
    module: &SemModule,
    function: &SemFunction,
    analysis: &CheckedFunction,
    operation: &SemOp,
    state: &mut State,
) {
    let operand = |source: &Operand| read(function, state, PlaceBase::Value(source.value));
    let derived = match &operation.kind {
        SemOpKind::FunctionMake { .. } => Capability::known(),
        SemOpKind::Move { source }
        | SemOpKind::CopyValue { source }
        | SemOpKind::Fork { source }
        | SemOpKind::CallableCoerce { source, .. }
        | SemOpKind::GeneratorCoerce { source }
        | SemOpKind::DynMake { value: source, .. }
        | SemOpKind::BeginBorrow { owner: source } => operand(source),
        SemOpKind::ClosureMake { fields, .. }
        | SemOpKind::AggregateMake { fields, .. }
        | SemOpKind::ArrayMake { fields }
        | SemOpKind::VariantMake { fields, .. }
        | SemOpKind::TupleMake { elements: fields } => {
            Capability::aggregate(fields.iter().map(operand).collect())
        }
        SemOpKind::GeneratorMake { callable, closure } => {
            let mut value = operand(callable);
            value.generators.insert(*closure);
            value.members.clear();
            value
        }
        SemOpKind::TupleGet { tuple, index } => operand(tuple).field(*index),
        SemOpKind::AggregateProjectCopy {
            aggregate, field, ..
        }
        | SemOpKind::AggregateProjectBorrow {
            aggregate, field, ..
        } => operand(aggregate).field(*field),
        SemOpKind::VariantProjectCopy { source, field, .. }
        | SemOpKind::VariantProjectBorrow { source, field, .. } => operand(source).field(*field),
        SemOpKind::Destructure { aggregate, .. }
        | SemOpKind::VariantDestructure {
            source: aggregate, ..
        } => operand(aggregate),
        SemOpKind::LoadBorrow { place }
        | SemOpKind::LoadCopy { place }
        | SemOpKind::LoadTake { place } => read(function, state, PlaceBase::Place(*place)),
        SemOpKind::StoreInit { place, value } | SemOpKind::StoreAssign { place, value } => {
            let value = operand(value);
            let ty = &function
                .places
                .iter()
                .find(|item| item.id == *place)
                .expect("checked place")
                .ty;
            write(
                function,
                analysis,
                state,
                PlaceBase::Place(*place),
                &refine(module, ty, value),
            );
            Capability::default()
        }
        SemOpKind::EndLifetime { place } | SemOpKind::AllocPlace { place } => {
            state.remove(&PlaceBase::Place(*place));
            Capability::default()
        }
        _ => Capability::default(),
    };
    for (index, result) in operation.results.iter().enumerate() {
        let value = if matches!(
            operation.kind,
            SemOpKind::Destructure { .. } | SemOpKind::VariantDestructure { .. }
        ) {
            derived.field(u32::try_from(index).expect("checked aggregate result index exceeds u32"))
        } else {
            derived.clone()
        };
        write(
            function,
            analysis,
            state,
            PlaceBase::Value(result.id),
            &refine(module, &result.ty, value),
        );
    }
}
