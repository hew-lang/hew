//! Generator capture conditions and their exact value provenance.

use super::{
    effects, CallableArgumentFlow, Checker, ClosureCaptureFact, HashMap, HashSet,
    IndirectCallCandidates, MarkerTrait, SpanKey, Ty, TypeError, TypeErrorKind,
};

#[derive(Debug, Clone)]
pub(super) struct ConditionalSendBound {
    pub site: SpanKey,
    pub parameter: crate::ParamHead,
    pub argument: Ty,
    pub inputs: Vec<Ty>,
    pub source: Option<String>,
}

pub(super) struct GeneratorCaptureScope {
    depth: Option<usize>,
    captures: Vec<Ty>,
    facts: Vec<ClosureCaptureFact>,
    sources: HashMap<crate::env::TypeBindingId, IndirectCallCandidates>,
}

impl Checker {
    pub(super) fn finish_conditional_send_bounds(
        &mut self,
        obligations: &[ConditionalSendBound],
        demands: &HashMap<SpanKey, Vec<Ty>>,
        output: &mut effects::SuspensionEffects,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
    ) {
        let mut proven_demands = HashSet::new();
        for obligation in obligations {
            let argument = self.normalize_for_use(&obligation.argument);
            if !argument.contains_generator() {
                continue;
            }
            let mut inputs = Vec::new();
            let complete =
                self.pending_callable_arguments
                    .get(&obligation.site)
                    .is_some_and(|actuals| {
                        obligation.inputs.iter().zip(&actuals.arguments).all(
                            |(schema, candidates)| {
                                self.project_generator_bound_input(
                                    schema,
                                    obligation.parameter,
                                    candidates,
                                    &mut inputs,
                                    &mut HashSet::new(),
                                )
                            },
                        )
                    });
            // Signature checking also visits already-instantiated signatures;
            // their erased binder is no longer an input schema. The original
            // declaration supplies the per-value proof for this same demand.
            if inputs.is_empty() {
                continue;
            }
            let mut requirements = Vec::new();
            let proven = complete
                && inputs.iter().all(|candidates| {
                    let (proven, nested) = self.value_candidates_have_marker(
                        &argument,
                        candidates,
                        MarkerTrait::Send,
                        flows,
                    );
                    requirements.extend(nested);
                    proven
                });
            if proven {
                proven_demands.insert((obligation.site.clone(), argument));
                output
                    .send_obligations
                    .entry(obligation.site.clone())
                    .or_default()
                    .extend(requirements);
            }
        }
        for (site, types) in demands {
            for argument in types
                .iter()
                .map(|ty| self.normalize_for_use(ty))
                .collect::<Vec<_>>()
            {
                if argument.contains_generator()
                    && !proven_demands.contains(&(site.clone(), argument.clone()))
                {
                    let mut error = TypeError::new(TypeErrorKind::InvalidSend, site.start..site.end,
                        format!("cannot prove `{}` is Send: its generator captures need checked transferable values", argument.user_facing()));
                    error.source_module = obligations
                        .iter()
                        .find(|obligation| obligation.site == *site)
                        .and_then(|obligation| obligation.source.clone());
                    self.errors.push(error);
                }
            }
        }
    }

    fn project_generator_bound_input(
        &self,
        schema: &Ty,
        parameter: crate::ParamHead,
        candidates: &IndirectCallCandidates,
        output: &mut Vec<IndirectCallCandidates>,
        seen: &mut HashSet<Ty>,
    ) -> bool {
        if *schema == Ty::param(parameter) {
            output.push(candidates.clone());
            return true;
        }
        if !schema.mentions_type_param(parameter) {
            return true;
        }
        if !seen.insert(schema.clone()) {
            return false;
        }
        let proven = match schema {
            Ty::Tuple(fields) => fields.iter().enumerate().all(|(index, field)| {
                self.project_generator_bound_input(
                    field,
                    parameter,
                    &candidates.project_member(None, index),
                    output,
                    seen,
                )
            }),
            Ty::Named { head, args } => {
                head.nominal()
                    .zip(self.head_type_def(*head))
                    .is_some_and(|(owner, definition)| {
                        definition
                            .field_order
                            .iter()
                            .enumerate()
                            .all(|(index, name)| {
                                definition.fields.get(name).is_some_and(|field| {
                                    let field = Self::instantiate_type_def_member(
                                        field,
                                        &definition.type_params,
                                        args,
                                    );
                                    self.project_generator_bound_input(
                                        &field,
                                        parameter,
                                        &candidates.project_member(Some(owner), index),
                                        output,
                                        seen,
                                    )
                                })
                            })
                    })
            }
            _ => false,
        };
        seen.remove(schema);
        proven
    }

    pub(super) fn begin_generator_captures(&mut self) -> GeneratorCaptureScope {
        let sources = self
            .env
            .visible_bindings()
            .map(|binding| {
                (
                    binding.id,
                    self.env
                        .value_candidates(binding.id)
                        .cloned()
                        .unwrap_or_else(IndirectCallCandidates::unknown),
                )
            })
            .collect();
        GeneratorCaptureScope {
            depth: self.lambda_capture_depth.replace(self.env.depth()),
            captures: std::mem::take(&mut self.lambda_captures),
            facts: std::mem::take(&mut self.lambda_capture_facts),
            sources,
        }
    }

    pub(super) fn finish_generator_captures(
        &mut self,
        body: effects::EffectBody,
        previous: GeneratorCaptureScope,
    ) {
        let raw = std::mem::take(&mut self.lambda_capture_facts);
        self.lambda_captures = previous.captures;
        self.lambda_capture_facts = previous.facts;
        self.lambda_capture_depth = previous.depth;
        let mut seen = HashSet::new();
        let mut captures = Vec::new();
        for mut capture in raw {
            if !seen.insert(capture.binding_id) {
                continue;
            }
            capture.ty = self
                .subst
                .resolve(&capture.ty)
                .materialize_literal_defaults();
            capture.value_candidates = previous
                .sources
                .get(&capture.binding_id)
                .cloned()
                .unwrap_or_else(IndirectCallCandidates::unknown);
            if previous.depth.is_some_and(|boundary| {
                self.env
                    .lookup_with_depth(&capture.name)
                    .is_some_and(|(depth, binding)| {
                        depth < boundary && binding.id == capture.binding_id
                    })
            }) {
                self.lambda_captures.push(capture.ty.clone());
                self.lambda_capture_facts.push(capture.clone());
            }
            captures.push(capture);
        }
        self.generator_capture_facts.insert(body, captures);
    }
}
