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

/// One checker-selected environment field in declaration order.
/// Acquisition is expressed by the construction operand's copy or move.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemCaptureField {
    pub binding: BindingId,
    pub ty: ResolvedTy,
    pub access: ClosureCaptureAccess,
    pub consumption: ClosureCaptureConsumption,
}

/// The semantic identity and contents of one owned closure environment.
/// Its body signature starts with this exact closure type as receiver.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemClosure {
    /// Checked yield type for a generator body using this ordinary environment.
    pub generator_yield: Option<ResolvedTy>,
    pub id: ClosureId,
    pub instance: ClosureInstanceKey,
    pub body: CallableId,
    pub ty: ResolvedTy,
    pub fields: Vec<SemCaptureField>,
}

/// Read the checker-owned signature and capabilities of a callable value.
///
/// # Errors
/// Refuses non-callable types instead of interpreting their storage as a callable.
pub fn callable_parts(
    ty: &ResolvedTy,
) -> Result<(&[ResolvedTy], &ResolvedTy, CallableCapabilities), String> {
    match ty {
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
        params: params
            .iter()
            .map(|ty| {
                let passing = match OwnKind::of_ty(ty, facts)? {
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
    let (source_params, source_ret, source_caps) = callable_parts(source)?;
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
    } = target
    {
        if !matches!(source, ResolvedTy::Closure { captures, .. } if captures == target_captures) {
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

impl SemClosure {
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
    pub fn validate(&self, module: &SemModule) -> Result<(), String> {
        if module.closure(self.id) != Some(self) {
            return Err("closure identity differs from its canonical table position".to_string());
        }
        let parent = module
            .callable(self.instance.enclosing)
            .ok_or_else(|| "closure has no exact enclosing callable instance".to_string())?;
        let body = module
            .callable(self.body)
            .ok_or_else(|| "closure has no exact body callable".to_string())?;
        if self.instance.enclosing.0 >= self.body.0
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
        let ResolvedTy::Closure {
            captures,
            capabilities,
            ..
        } = &self.ty
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
        if let Some(yielded) = &self.generator_yield {
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
        let mut signature = callable_value_signature(&self.ty, &module.type_facts)?;
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
                "closure body signature differs from its receiver and source arguments".to_string(),
            );
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
    let ResolvedTy::Named {
        builtin: Some(hew_types::BuiltinType::Generator),
        args,
        ..
    } = ty
    else {
        return None;
    };
    let [yielded, returned] = args.as_slice() else {
        return None;
    };
    Some((yielded, returned))
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
            actors: Vec::new(),
            callables: vec![parent, body],
            closures: vec![SemClosure {
                generator_yield: None,
                id: ClosureId(0),
                instance: ClosureInstanceKey {
                    enclosing: CallableId(0),
                    literal: HirNodeId(7),
                },
                body: CallableId(1),
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
            params: vec![ResolvedTy::I64],
            ret: Box::new(ResolvedTy::I64),
            capabilities: CallableCapabilities::FUNCTION_ITEM,
        };
        let target = ResolvedTy::Function {
            params: vec![ResolvedTy::I64],
            ret: Box::new(ResolvedTy::I64),
            capabilities: CallableCapabilities {
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
            declaration: callable.declaration.clone(),
            name: callable.symbol.clone(),
            source_origin: callable.source_origin.clone(),
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
        let mut taken = module;
        taken.functions[0].blocks[0].ops[0].kind = crate::SemOpKind::LoadTake {
            place: crate::PlaceId(0),
        };
        assert!(crate::verify_module(&taken)
            .iter()
            .any(|diagnostic| format!("{diagnostic:?}").contains("call-once body")));
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
            declaration: module.callables[0].declaration.clone(),
            name: "counter".into(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            places: vec![],
            bindings: vec![],
            blocks: vec![
                SemBlock {
                    id: BlockId(0),
                    args: vec![],
                    ops: vec![
                        crate::SemOp {
                            id: crate::OpId(0),
                            kind: crate::SemOpKind::ConstI64(10),
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
                        id: crate::OpId(2),
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
                    id: BlockId(1),
                    args: vec![BlockArg {
                        value: ValueId(3),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    ops: vec![drop_callable(3, 1)],
                    terminator: SemTerminator::Return {
                        value: Some(BoundaryOperand {
                            operand: Operand { value: ValueId(3) },
                            decision: BoundaryDecision::Copy,
                        }),
                    },
                },
                SemBlock {
                    id: BlockId(2),
                    args: vec![],
                    ops: vec![drop_callable(4, 1)],
                    terminator: SemTerminator::ResumeUnwind,
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
