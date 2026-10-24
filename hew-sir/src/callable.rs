//! Concrete callable environments, independent of storage layout and ABI.

use hew_hir::{BindingId, HirNodeId};
use hew_types::{
    CallableCallMode, CallableCapabilities, ClosureCaptureAccess, ClosureCaptureConsumption,
    ResolvedTy, TypeInstanceKey, ValueClass,
};

use crate::ownership::TypeFactTable;
use crate::{
    AggregateFieldRecipe, AggregateShapeRef, CallableId, CallableInstance, OwnKind, SemAbiParam,
    SemCallableKind, SemModule, SemParamPassing, SemSignature,
};

/// Canonical position of a concrete closure environment in its module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClosureId(pub u32);

/// A literal belongs to an exact enclosing instance, including specialization.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClosureInstanceKey {
    pub enclosing: CallableId,
    pub literal: HirNodeId,
}

/// Checked callable origins projected into concrete semantic instances.
/// Consuming invocation also releases the descriptor-owned environment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallableTargets {
    pub known: Vec<CallableId>,
    pub may_be_unknown: bool,
}

impl Default for CallableTargets {
    fn default() -> Self {
        Self {
            known: Vec::new(),
            may_be_unknown: true,
        }
    }
}

/// One checker-selected environment field in declaration order.
/// Acquisition is expressed by the construction operand's copy or move.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemCaptureField {
    pub binding: BindingId,
    pub ty: ResolvedTy,
    pub access: ClosureCaptureAccess,
    pub consumption: ClosureCaptureConsumption,
}

/// One concrete application shared by callable environment descriptors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallableSpecializationId(pub u32);

/// A concrete body borrowing or consuming its family's single environment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemClosureEntry {
    pub body: CallableId,
    pub specialization: Option<CallableSpecializationId>,
}

/// The semantic identity and contents of one owned closure environment.
/// Its body signature starts with this exact closure type as receiver.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemClosure {
    /// Exact source producer refinement for generator construction.
    pub generator_type: Option<ResolvedTy>,
    /// Source Send demands whose retained-frame proof this instance owes.
    pub generator_send_sites: Vec<GeneratorSendSite>,
    pub id: ClosureId,
    pub instance: ClosureInstanceKey,
    pub entries: Vec<SemClosureEntry>,
    pub ty: ResolvedTy,
    pub fields: Vec<SemCaptureField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratorSendSite {
    pub span: std::ops::Range<usize>,
    pub source: crate::FunctionSourceOrigin,
}

/// Read the checker-owned signature and capabilities of a callable value.
///
/// # Errors
/// Refuses non-callable types instead of interpreting their storage as a callable.
pub fn callable_parts(
    ty: &ResolvedTy,
) -> Result<
    (
        &[hew_types::CallableParameter<ResolvedTy>],
        &ResolvedTy,
        CallableCapabilities,
    ),
    String,
> {
    match ty.callable_body() {
        ResolvedTy::Function {
            params,
            ret,
            capabilities,
        }
        | ResolvedTy::Closure {
            params,
            ret,
            capabilities,
            ..
        } => Ok((params, ret, *capabilities)),
        _ => Err(format!(
            "`{}` has no callable value contract",
            ty.user_facing()
        )),
    }
}

/// Derive a value invocation signature from exact semantic type facts.
/// The environment receiver is separate from the source argument list.
///
/// # Errors
/// Refuses a non-callable type or an argument without concrete ownership facts.
pub fn callable_value_signature(
    ty: &ResolvedTy,
    facts: &TypeFactTable,
) -> Result<SemSignature, String> {
    let (params, ret, _) = callable_parts(ty)?;
    Ok(SemSignature {
        failure_domain: hew_types::CallableFailureDomain::HewFault,
        params: params
            .iter()
            .map(|parameter| {
                let ty = &parameter.ty;
                let passing = match OwnKind::of_ty(ty, facts)? {
                    OwnKind::Owned
                        if parameter.ownership == hew_types::ParameterOwnership::Consume =>
                    {
                        SemParamPassing::Consume
                    }
                    OwnKind::Owned => SemParamPassing::Borrow,
                    OwnKind::None => SemParamPassing::ReadOnly,
                    OwnKind::Guaranteed => {
                        return Err(
                            "a concrete argument type cannot own a borrow-only kind".to_string()
                        )
                    }
                };
                Ok(SemAbiParam {
                    ty: ty.clone(),
                    passing,
                    caller_visible_projection: false,
                })
            })
            .collect::<Result<_, String>>()?,
        return_ty: ret.clone(),
    })
}

/// Verify capability weakening without changing callable arguments or results.
///
/// # Errors
/// Refuses invented capabilities, changed capture shapes and erased linear duties.
pub fn verify_callable_coercion(
    source: &ResolvedTy,
    target: &ResolvedTy,
    facts: &TypeFactTable,
) -> Result<(), String> {
    let alpha_source = match (source, target) {
        (ResolvedTy::GenericCallable { .. }, ResolvedTy::GenericCallable { parameters, .. }) => {
            let arguments: Vec<_> = parameters
                .iter()
                .map(|parameter| ResolvedTy::TypeParam {
                    name: parameter.parameter,
                })
                .collect();
            Some(
                source
                    .instantiate_callable(&arguments)
                    .ok_or_else(|| "callable coercion changes quantifier arity".to_string())?,
            )
        }
        (ResolvedTy::GenericCallable { .. }, _) | (_, ResolvedTy::GenericCallable { .. }) => {
            return Err(
                "quantifier elimination requires an explicit callable specialization".to_string(),
            );
        }
        _ => None,
    };
    let (source_params, source_ret, source_caps) =
        callable_parts(alpha_source.as_ref().unwrap_or(source))?;
    let (target_params, target_ret, target_caps) = callable_parts(target)?;
    if source_params != target_params || source_ret != target_ret {
        return Err("callable coercion changes an invariant argument or result type".to_string());
    }
    if source_caps.call > target_caps.call || (!source_caps.clone && target_caps.clone) {
        return Err("callable coercion invents an invocation or Clone capability".to_string());
    }
    if let ResolvedTy::Closure {
        captures: target_captures,
        ..
    } = target.callable_body()
    {
        if !matches!(source.callable_body(), ResolvedTy::Closure { captures, .. } if captures == target_captures)
        {
            return Err(
                "callable coercion invents or changes a concrete capture shape".to_string(),
            );
        }
    }
    let row = |ty: &ResolvedTy| {
        facts.get(&TypeInstanceKey(ty.clone())).ok_or_else(|| {
            format!(
                "callable coercion has no concrete facts for `{}`",
                ty.user_facing()
            )
        })
    };
    if row(source)?.class == ValueClass::Linear && row(target)?.class != ValueClass::Linear {
        return Err("callable coercion erases a linear ownership obligation".to_string());
    }
    Ok(())
}

/// Verify a generator view that retains its producer and exact output owners.
///
/// # Errors
/// Refuses changed output types except for proved callable capability weakening.
pub fn verify_generator_coercion(
    source: &ResolvedTy,
    target: &ResolvedTy,
    facts: &TypeFactTable,
) -> Result<(), String> {
    if let ResolvedTy::Generator {
        producers: required,
        ..
    } = target
    {
        let ResolvedTy::Generator {
            producers: supplied,
            ..
        } = source
        else {
            return Err("generator coercion cannot invent producer evidence".into());
        };
        if supplied.is_empty() || supplied.iter().any(|producer| !required.contains(producer)) {
            return Err("generator coercion changes its possible producers".into());
        }
    }
    let (source_yield, source_return) =
        generator_parts(source).ok_or("generator coercion requires a source generator")?;
    let (target_yield, target_return) =
        generator_parts(target).ok_or("generator coercion requires a target generator")?;
    for (source, target) in [(source_yield, target_yield), (source_return, target_return)] {
        if source != target {
            verify_callable_coercion(source, target, facts)?;
        }
    }
    Ok(())
}

impl SemClosure {
    /// Checked yield type derived from the producer's generator contract.
    #[must_use]
    pub fn generator_yield(&self) -> Option<&ResolvedTy> {
        self.generator_type
            .as_ref()?
            .generator_parts()
            .map(|(yielded, _)| yielded)
    }

    /// Derive capture ownership with the same field recipes used by aggregates.
    ///
    /// # Errors
    /// Refuses any field lacking a checker-published concrete type-fact row.
    pub fn field_recipes(
        &self,
        facts: &TypeFactTable,
    ) -> Result<Vec<AggregateFieldRecipe>, String> {
        if self.fields.is_empty() {
            return Ok(Vec::new());
        }
        let fields = ResolvedTy::Tuple(self.fields.iter().map(|field| field.ty.clone()).collect());
        crate::aggregate_field_recipes(AggregateShapeRef::Tuple, &fields, &[], facts)
    }

    /// Check the exact literal, body, capture and receiver contract.
    ///
    /// # Errors
    /// Refuses inconsistent semantic identities, field types or capabilities.
    #[allow(
        clippy::too_many_lines,
        reason = "checks one closure environment and all of its invocation entries"
    )]
    pub fn validate(&self, module: &SemModule) -> Result<(), String> {
        if module.closure(self.id) != Some(self) {
            return Err("closure identity differs from its canonical table position".to_string());
        }
        let parent = module
            .callable(self.instance.enclosing)
            .ok_or_else(|| "closure has no exact enclosing callable instance".to_string())?;
        let ResolvedTy::Closure {
            captures,
            capabilities,
            ..
        } = self.ty.callable_body()
        else {
            return Err(
                "closure descriptor requires its concrete capture-bearing type".to_string(),
            );
        };
        if captures
            != &self
                .fields
                .iter()
                .map(|field| field.ty.clone())
                .collect::<Vec<_>>()
        {
            return Err(
                "closure descriptor fields differ from its exact capture types".to_string(),
            );
        }
        let mut bindings = std::collections::BTreeSet::new();
        for field in &self.fields {
            if !bindings.insert(field.binding) {
                return Err("closure descriptor repeats a captured binding identity".to_string());
            }
            if (field.access == ClosureCaptureAccess::Var
                && capabilities.call == CallableCallMode::Read)
                || (field.consumption == ClosureCaptureConsumption::Consumed
                    && capabilities.call != CallableCallMode::Once)
            {
                return Err(
                    "closure invocation capability does not permit its capture effects".to_string(),
                );
            }
        }
        let recipes = self.field_recipes(&module.type_facts)?;
        if capabilities.clone
            && recipes
                .iter()
                .any(|field| field.clone == hew_types::CloneKind::None)
        {
            return Err("closure promises Clone for a non-cloneable captured owner".to_string());
        }
        if self.generator_type.is_some() && self.generator_yield().is_none() {
            return Err("generator producer contract has no generator type".into());
        }
        if let Some(yielded) = self.generator_yield() {
            let (params, _, capabilities) = callable_parts(&self.ty)?;
            if !params.is_empty()
                || capabilities.call != CallableCallMode::Once
                || capabilities.clone
            {
                return Err(
                    "generator producer must be a nullary non-cloneable once callable".into(),
                );
            }
            OwnKind::of_ty(yielded, &module.type_facts)?;
        }
        if !matches!(self.ty, ResolvedTy::GenericCallable { .. }) && self.entries.len() != 1 {
            return Err("monomorphic closure requires exactly one invocation entry".to_string());
        }
        let mut specializations = std::collections::BTreeSet::new();
        for entry in &self.entries {
            if !specializations.insert(entry.specialization) {
                return Err("closure repeats an invocation specialization".to_string());
            }
            let body = module
                .callable(entry.body)
                .ok_or_else(|| "closure has no exact body callable".to_string())?;
            if self.instance.enclosing.0 >= entry.body.0
                || body.instance != CallableInstance::Closure(self.id)
                || body.kind != SemCallableKind::HewClosure
                || body.function != parent.function
                || body.declaration != parent.declaration
                || body.source_origin != parent.source_origin
            {
                return Err(
                    "closure body identity or provenance differs from its enclosing instance"
                        .to_string(),
                );
            }
            let invocation_ty = match entry.specialization {
                Some(id) => self
                    .ty
                    .instantiate_callable(
                        module
                            .callable_specializations
                            .get(id.0 as usize)
                            .ok_or_else(|| {
                                "closure specialization has no type arguments".to_string()
                            })?,
                    )
                    .ok_or_else(|| {
                        "closure entry arguments disagree with its scheme".to_string()
                    })?,
                None if !matches!(self.ty, ResolvedTy::GenericCallable { .. }) => self.ty.clone(),
                None => return Err("generic closure has an unspecialized invocation".to_string()),
            };
            let mut signature = callable_value_signature(&invocation_ty, &module.type_facts)?;
            signature.params.insert(
                0,
                SemAbiParam {
                    ty: self.ty.clone(),
                    passing: match capabilities.call {
                        CallableCallMode::Read => SemParamPassing::Borrow,
                        CallableCallMode::Var => SemParamPassing::BorrowMut,
                        CallableCallMode::Once => SemParamPassing::Consume,
                    },
                    caller_visible_projection: capabilities.call == CallableCallMode::Var,
                },
            );
            if body.signature != signature {
                return Err(
                    "closure body signature differs from its receiver and source arguments"
                        .to_string(),
                );
            }
        }
        Ok(())
    }
}

impl SemModule {
    /// Resolve a closure only when its identity agrees with its table position.
    #[must_use]
    pub fn closure(&self, id: ClosureId) -> Option<&SemClosure> {
        self.closures
            .get(usize::try_from(id.0).ok()?)
            .filter(|closure| closure.id == id)
    }
}

/// Read the exact yield and return types of a checked generator value.
#[must_use]
pub fn generator_parts(ty: &ResolvedTy) -> Option<(&ResolvedTy, &ResolvedTy)> {
    ty.generator_parts()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FunctionSourceOrigin, SemCallConv, SemCallable};
    use hew_types::{DefId, TypeFactContext, TypeFactService};

    fn private_counter() -> SemModule {
        let ty = ResolvedTy::Closure {
            params: vec![],
            ret: Box::new(ResolvedTy::I64),
            captures: vec![ResolvedTy::I64],
            capabilities: CallableCapabilities {
                suspends: false,
                call: CallableCallMode::Var,
                clone: true,
            },
        };
        let parent = SemCallable {
            id: CallableId(0),
            function: hew_hir::ItemId(0),
            declaration: DefId::for_test("counter"),
            instance: CallableInstance::Monomorphic,
            symbol: "counter".to_string(),
            source_origin: FunctionSourceOrigin::Unknown,
            signature: SemSignature {
                failure_domain: hew_types::CallableFailureDomain::HewFault,
                params: vec![],
                return_ty: ResolvedTy::Unit,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        };
        let body = SemCallable {
            id: CallableId(1),
            instance: CallableInstance::Closure(ClosureId(0)),
            symbol: "counter_closure".to_string(),
            kind: SemCallableKind::HewClosure,
            signature: SemSignature {
                failure_domain: hew_types::CallableFailureDomain::HewFault,
                params: vec![SemAbiParam {
                    ty: ty.clone(),
                    passing: SemParamPassing::BorrowMut,
                    caller_visible_projection: true,
                }],
                return_ty: ResolvedTy::I64,
            },
            ..parent.clone()
        };
        let mut service = TypeFactService::new(TypeFactContext::default(), TypeFactTable::new());
        for required in [&ResolvedTy::I64, &ResolvedTy::Unit, &ty] {
            service.require(required).unwrap();
        }
        SemModule {
            debug: crate::SemDebugFacts::default(),
            regex_patterns: Vec::new(),
            actors: Vec::new(),
            supervisors: Vec::new(),
            callables: vec![parent, body],
            closures: vec![SemClosure {
                generator_type: None,
                generator_send_sites: Vec::new(),
                id: ClosureId(0),
                instance: ClosureInstanceKey {
                    enclosing: CallableId(0),
                    literal: HirNodeId(7),
                },
                entries: vec![SemClosureEntry {
                    body: CallableId(1),
                    specialization: None,
                }],
                ty,
                fields: vec![SemCaptureField {
                    binding: BindingId(4),
                    ty: ResolvedTy::I64,
                    access: ClosureCaptureAccess::Var,
                    consumption: ClosureCaptureConsumption::Retained,
                }],
            }],
            type_facts: service.rows().clone(),
            ..SemModule::default()
        }
    }

    #[test]
    fn environment_contract_rejects_wrong_receiver_capture_and_identity() {
        let module = private_counter();
        assert!(
            crate::verify_module(&module).is_empty(),
            "{:?}",
            crate::verify_module(&module)
        );
        let mut wrong_generator = module.clone();
        wrong_generator.closures[0].generator_type = Some(ResolvedTy::I64);
        assert!(wrong_generator.closures[0]
            .validate(&wrong_generator)
            .is_err_and(|message| message == "generator producer contract has no generator type"));
        let mut wrong_receiver = module.clone();
        wrong_receiver.callables[1].signature.params[0].passing = SemParamPassing::Borrow;
        assert!(!crate::verify_module(&wrong_receiver).is_empty());
        let mut wrong_field = module.clone();
        wrong_field.closures[0].fields[0].ty = ResolvedTy::String;
        assert!(!crate::verify_module(&wrong_field).is_empty());
        let mut wrong_parent = module;
        wrong_parent.closures[0].instance.enclosing = CallableId(1);
        assert!(!crate::verify_module(&wrong_parent).is_empty());
    }

    #[test]
    fn environment_cannot_hide_consuming_access_behind_a_repeated_receiver() {
        let mut module = private_counter();
        module.closures[0].fields[0].consumption = ClosureCaptureConsumption::Consumed;
        assert!(module.closures[0]
            .validate(&module)
            .unwrap_err()
            .contains("capture effects"));
    }

    #[test]
    fn callable_coercions_forget_but_never_invent_capabilities() {
        let mut service = TypeFactService::new(TypeFactContext::default(), TypeFactTable::new());
        let source = ResolvedTy::Function {
            params: vec![hew_types::CallableParameter::borrowed(ResolvedTy::I64)],
            ret: Box::new(ResolvedTy::I64),
            capabilities: CallableCapabilities::FUNCTION_ITEM,
        };
        let target = ResolvedTy::Function {
            params: vec![hew_types::CallableParameter::borrowed(ResolvedTy::I64)],
            ret: Box::new(ResolvedTy::I64),
            capabilities: CallableCapabilities {
                suspends: false,
                call: CallableCallMode::Once,
                clone: false,
            },
        };
        service.require(&source).unwrap();
        service.require(&target).unwrap();
        assert!(verify_callable_coercion(&source, &target, service.rows()).is_ok());
        assert!(verify_callable_coercion(&target, &source, service.rows()).is_err());
        let mut wrong_result = target;
        if let ResolvedTy::Function { ret, .. } = &mut wrong_result {
            **ret = ResolvedTy::String;
        }
        assert!(verify_callable_coercion(&source, &wrong_result, service.rows()).is_err());
    }
    fn counter_body(module: &SemModule) -> crate::SemFunction {
        use crate::{
            BlockArg, BlockId, BoundaryDecision, BoundaryOperand, Operand, PlaceDecl, PlaceId,
            PlaceOrigin, SemBlock, SemOp, SemOpKind, SemTerminator, ValueDef, ValueId,
        };
        let callable = &module.callables[1];
        let operation = |id, kind, results| SemOp {
            id: crate::OpId(id),
            kind,
            results,
            provenance: crate::Provenance::Synthesized,
        };
        crate::SemFunction {
            id: callable.function,
            callable: callable.id,
            declaration: callable.declaration,
            name: callable.symbol.clone(),
            source_origin: callable.source_origin.clone(),
            terminal_receiver: None,
            span: 0..0,
            params: vec![BlockArg {
                value: ValueId(0),
                ty: module.closures[0].ty.clone(),
                own: OwnKind::Guaranteed,
            }],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            bindings: vec![],
            places: vec![PlaceDecl {
                id: PlaceId(0),
                ty: ResolvedTy::I64,
                origin: PlaceOrigin::Capture {
                    environment: ValueId(0),
                    field: 0,
                },
            }],
            blocks: vec![SemBlock {
                terminator_provenance: crate::Provenance::Synthesized,
                id: BlockId(0),
                args: vec![],
                ops: vec![
                    operation(
                        0,
                        SemOpKind::LoadCopy { place: PlaceId(0) },
                        vec![ValueDef {
                            id: ValueId(1),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                    ),
                    operation(
                        1,
                        SemOpKind::StoreAssign {
                            place: PlaceId(0),
                            value: Operand { value: ValueId(1) },
                        },
                        vec![],
                    ),
                ],
                terminator: SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value: ValueId(1) },
                        decision: BoundaryDecision::Copy,
                    }),
                },
            }],
        }
    }

    #[test]
    fn private_capture_body_verifies_and_refuses_forged_access() {
        let mut module = private_counter();
        module.functions.push(counter_body(&module));
        assert!(
            crate::verify_module(&module).is_empty(),
            "{:?}",
            crate::verify_module(&module)
        );
        let mut wrong_owner = module.clone();
        wrong_owner.functions[0].places[0].origin = crate::PlaceOrigin::Capture {
            environment: crate::ValueId(1),
            field: 0,
        };
        assert!(!crate::verify_module(&wrong_owner).is_empty());
        let mut immutable = module.clone();
        immutable.closures[0].fields[0].access = ClosureCaptureAccess::Read;
        assert!(crate::verify_module(&immutable)
            .iter()
            .any(|diagnostic| format!("{diagnostic:?}").contains("private mutable access")));
        // A mutation may take a private mutable capture only to
        // re-initialize it before the body exits.
        let mut restored = module.clone();
        restored.functions[0].blocks[0].ops[0].kind = crate::SemOpKind::LoadTake {
            place: crate::PlaceId(0),
        };
        restored.functions[0].blocks[0].ops[1].kind = crate::SemOpKind::StoreInit {
            place: crate::PlaceId(0),
            value: crate::Operand {
                value: crate::ValueId(1),
            },
        };
        assert!(
            crate::verify_module(&restored).is_empty(),
            "{:?}",
            crate::verify_module(&restored)
        );
        let mut abandoned = restored.clone();
        abandoned.functions[0].blocks[0].ops.truncate(1);
        assert!(crate::verify_module(&abandoned).iter().any(|diagnostic| {
            format!("{diagnostic:?}").contains("capture field is not re-initialized at this exit")
        }));
        let mut snapshot = restored;
        snapshot.closures[0].fields[0].access = ClosureCaptureAccess::Read;
        assert!(crate::verify_module(&snapshot).iter().any(|diagnostic| {
            format!("{diagnostic:?}").contains("call-once body or private mutable access")
        }));
    }

    fn drop_callable(id: u32, value: u32) -> crate::SemOp {
        crate::SemOp {
            id: crate::OpId(id),
            kind: crate::SemOpKind::DestroyValue {
                value: crate::Operand {
                    value: crate::ValueId(value),
                },
            },
            results: vec![],
            provenance: crate::Provenance::Synthesized,
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one hand-built module: the indirect call, its cleanup dispatch and both exits"
    )]
    fn indirect_counter_module() -> SemModule {
        use crate::{
            BlockArg, BlockId, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind, Edge,
            Operand, SemBlock, SemFunction, SemTerminator, ValueDef, ValueId,
        };
        let mut module = private_counter();
        let callee_ty = module.closures[0].ty.clone();
        let signature = callable_value_signature(&callee_ty, &module.type_facts).unwrap();
        module.callables[0].signature.return_ty = ResolvedTy::I64;
        module.functions.push(SemFunction {
            id: module.callables[0].function,
            callable: CallableId(0),
            declaration: module.callables[0].declaration,
            name: "counter".into(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            terminal_receiver: None,
            params: vec![],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            places: vec![],
            bindings: vec![],
            blocks: vec![
                SemBlock {
                    terminator_provenance: crate::Provenance::Synthesized,
                    id: BlockId(0),
                    args: vec![],
                    ops: vec![
                        crate::SemOp {
                            id: crate::OpId(0),
                            kind: crate::SemOpKind::ConstInteger(10),
                            results: vec![ValueDef {
                                id: ValueId(0),
                                ty: ResolvedTy::I64,
                                own: OwnKind::None,
                            }],
                            provenance: crate::Provenance::Synthesized,
                        },
                        crate::SemOp {
                            id: crate::OpId(1),
                            kind: crate::SemOpKind::ClosureMake {
                                closure: ClosureId(0),
                                fields: vec![Operand { value: ValueId(0) }],
                            },
                            results: vec![ValueDef {
                                id: ValueId(1),
                                ty: callee_ty,
                                own: OwnKind::Owned,
                            }],
                            provenance: crate::Provenance::Synthesized,
                        },
                    ],
                    terminator: SemTerminator::IndirectCall {
                        specialization: None,
                        id: crate::OpId(2),
                        targets: crate::CallableTargets::default(),
                        callee: BoundaryOperand {
                            operand: Operand { value: ValueId(1) },
                            decision: BoundaryDecision::BorrowMut,
                        },
                        signature,
                        args: vec![],
                        result: CallResult::Value(ValueDef {
                            id: ValueId(2),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }),
                        normal: Some(Edge {
                            target: BlockId(1),
                            args: vec![Operand { value: ValueId(2) }],
                        }),
                        unwind: CallUnwind::Cleanup(Edge {
                            target: BlockId(2),
                            args: vec![],
                        }),
                    },
                },
                SemBlock {
                    terminator_provenance: crate::Provenance::Synthesized,
                    id: BlockId(1),
                    args: vec![BlockArg {
                        value: ValueId(3),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    ops: vec![drop_callable(3, 1)],
                    // A callable release can run a capture's `close`, so the
                    // normal exit dispatches on the release's outcome.
                    terminator: SemTerminator::CleanupDispatch {
                        normal: Edge {
                            target: BlockId(3),
                            args: vec![Operand { value: ValueId(3) }],
                        },
                        fault: Edge {
                            target: BlockId(4),
                            args: vec![],
                        },
                    },
                },
                SemBlock {
                    terminator_provenance: crate::Provenance::Synthesized,
                    id: BlockId(2),
                    args: vec![],
                    ops: vec![drop_callable(4, 1)],
                    terminator: SemTerminator::ResumeUnwind { handback: None },
                },
                SemBlock {
                    terminator_provenance: crate::Provenance::Synthesized,
                    id: BlockId(3),
                    args: vec![BlockArg {
                        value: ValueId(4),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    ops: vec![],
                    terminator: SemTerminator::Return {
                        value: Some(BoundaryOperand {
                            operand: Operand { value: ValueId(4) },
                            decision: BoundaryDecision::Copy,
                        }),
                    },
                },
                SemBlock {
                    terminator_provenance: crate::Provenance::Synthesized,
                    id: BlockId(4),
                    args: vec![],
                    ops: vec![],
                    terminator: SemTerminator::ResumeUnwind { handback: None },
                },
            ],
        });
        module
    }

    #[test]
    fn indirect_call_checks_signature_and_receiver_permission() {
        use crate::{BoundaryDecision, CallUnwind, SemTerminator};
        let mut module = indirect_counter_module();
        assert!(
            crate::verify_module(&module).is_empty(),
            "{:?}",
            crate::verify_module(&module)
        );
        let mut missing_fault = module.clone();
        if let SemTerminator::IndirectCall { unwind, .. } =
            &mut missing_fault.functions[0].blocks[0].terminator
        {
            *unwind = CallUnwind::NotApplicable;
        }
        assert!(crate::verify_module(&missing_fault)
            .iter()
            .any(|diagnostic| format!("{diagnostic:?}").contains("propagates the original fault")));
        let mut wrong_generator = module.clone();
        wrong_generator.closures[0].generator_type = Some(ResolvedTy::I64);
        assert!(wrong_generator.closures[0]
            .validate(&wrong_generator)
            .is_err_and(|message| message == "generator producer contract has no generator type"));
        let mut wrong_receiver = module.clone();
        if let SemTerminator::IndirectCall { callee, .. } =
            &mut wrong_receiver.functions[0].blocks[0].terminator
        {
            callee.decision = BoundaryDecision::Borrow;
        }
        assert!(crate::verify_module(&wrong_receiver)
            .iter()
            .any(|diagnostic| format!("{diagnostic:?}").contains("invocation capability")));
        if let SemTerminator::IndirectCall { signature, .. } =
            &mut module.functions[0].blocks[0].terminator
        {
            signature.return_ty = ResolvedTy::Bool;
        }
        assert!(crate::verify_module(&module)
            .iter()
            .any(|diagnostic| format!("{diagnostic:?}")
                .contains("exact callable type and signature")));
    }
}
