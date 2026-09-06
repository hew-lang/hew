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
            callables: vec![parent, body],
            closures: vec![SemClosure {
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
}
