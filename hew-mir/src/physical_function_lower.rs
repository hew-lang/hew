//! Per-function lowering: the `FunctionLowerer` walk from SIR blocks into physical operations.

use super::{
    clone_action_for_type, destroy_action_for_type, integer_bit_range, integer_width_bits,
    mask_to_width, physical_value_recipe, required_layout, ArgumentTransfer, ArrayValueOp,
    BTreeSet, BlockId, BoundaryDecision, BuiltinType, CallResult, CallUnwind, CloneAction,
    DestroyAction, Edge, FunctionLowerer, MapValueOp, OwnKind, PhysicalAggregateId,
    PhysicalCheckedFailure, PhysicalConst, PhysicalEdge, PhysicalError, PhysicalMapOp, PhysicalOp,
    PhysicalRuntimeAction, PhysicalRuntimeCarrier, PhysicalSelectSource, PhysicalSetOp,
    PhysicalStorage, PhysicalTerminator, PhysicalVariantArm, PhysicalVariantId, PhysicalVectorOp,
    PhysicalVtableId, PhysicalWireTextResult, ResolvedTy, ReturnTransfer, RuntimeCallFamily,
    RuntimePhysicalForm, SemOp, SemOpKind, SemTerminator, SetValueOp, SnapshotDecision, StorageId,
    StorageOrigin, TypeInstanceKey, UnaryOp, ValueId, VecValueOp,
};

impl FunctionLowerer<'_> {
    pub(crate) fn next_storage_id(&self) -> Result<StorageId, PhysicalError> {
        Ok(StorageId(u32::try_from(self.storage.len()).map_err(
            |_| PhysicalError::new("physical function has more than u32::MAX storage slots"),
        )?))
    }

    pub(crate) fn insert_value(
        &mut self,
        value: ValueId,
        ty: &ResolvedTy,
        own: OwnKind,
        origin: StorageOrigin,
    ) -> Result<StorageId, PhysicalError> {
        let id = self.next_storage_id()?;
        if self.values.insert(value, id).is_some() {
            return Err(PhysicalError::new(format!(
                "SIR value {} is defined more than once while assigning physical storage",
                value.0
            )));
        }
        self.storage.push(PhysicalStorage {
            id,
            ty: ty.clone(),
            layout: required_layout(self.target, ty)?.clone(),
            own,
            origin,
            borrow_parent: None,
        });
        Ok(id)
    }

    pub(crate) fn value(&self, value: ValueId) -> Result<StorageId, PhysicalError> {
        self.values.get(&value).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "physical lowering cannot find SIR value {}",
                value.0
            ))
        })
    }

    pub(crate) fn place(&self, place: hew_sir::PlaceId) -> Result<StorageId, PhysicalError> {
        self.places.get(&place).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "physical lowering cannot find SIR place {}",
                place.0
            ))
        })
    }

    /// Derive the destination-width two's-complement bit pattern for an exact
    /// integer constant.
    ///
    /// The width and signedness come from the destination storage the same
    /// pass just allocated -- physical MIR owns representation, so the target
    /// fact is read here and never reconstructed in the backend. The checker
    /// and the SIR verifier already admitted the value against its type, so a
    /// value outside the destination range is malformed IR and fails closed.
    pub(crate) fn integer_const_bits(
        &self,
        dest: StorageId,
        value: i128,
    ) -> Result<u64, PhysicalError> {
        let storage = self
            .storage
            .get(dest.0 as usize)
            .ok_or_else(|| PhysicalError::new("integer constant has no destination storage"))?;
        let width = integer_width_bits(&storage.layout).ok_or_else(|| {
            PhysicalError::new("integer constant requires an integer destination layout")
        })?;
        let (low, high) = integer_bit_range(width, storage.ty.is_signed_integer());
        if value < low || value > high {
            return Err(PhysicalError::new(format!(
                "integer constant {value} does not fit its {width}-bit destination `{}`",
                storage.ty.user_facing()
            )));
        }
        // Deliberate two's-complement truncation: the value is already proven
        // in range, and the destination-width bit pattern is exactly the low
        // bits of its two's-complement encoding.
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            reason = "the destination-width bit pattern is the low bits of the two's-complement encoding"
        )]
        Ok(mask_to_width(value as u64, width))
    }

    pub(crate) fn one_result(&self, operation: &SemOp) -> Result<StorageId, PhysicalError> {
        let [result] = operation.results.as_slice() else {
            return Err(PhysicalError::new(format!(
                "SIR op {} requires exactly one physical result, got {}",
                operation.id.0,
                operation.results.len()
            )));
        };
        self.value(result.id)
    }

    pub(crate) fn no_results(operation: &SemOp) -> Result<(), PhysicalError> {
        if operation.results.is_empty() {
            Ok(())
        } else {
            Err(PhysicalError::new(format!(
                "SIR op {} must not produce physical results",
                operation.id.0
            )))
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the exhaustive SIR operation match is the auditable ownership-to-physical boundary"
    )]
    pub(crate) fn lower_op(
        &self,
        operation: &SemOp,
        site: (BlockId, usize),
    ) -> Result<Vec<PhysicalOp>, PhysicalError> {
        if matches!(
            operation.kind,
            SemOpKind::Unary {
                op: UnaryOp::RawDeref,
                ..
            }
        ) {
            return Err(PhysicalError::new(format!(
                "SIR op {} can fail without an explicit cleanup CFG edge",
                operation.id.0
            )));
        }
        let one = |op| Ok(vec![op]);
        match &operation.kind {
            SemOpKind::RegisterDefer {
                defer,
                scope,
                dependencies,
            } => {
                Self::no_results(operation)?;
                one(PhysicalOp::RegisterDefer {
                    defer: *defer,
                    scope: *scope,
                    dependencies: dependencies
                        .iter()
                        .map(|p| self.place(*p))
                        .collect::<Result<_, _>>()?,
                })
            }
            SemOpKind::ConstInteger(value) => {
                let dest = self.one_result(operation)?;
                let bits = self.integer_const_bits(dest, *value)?;
                one(PhysicalOp::Const {
                    dest,
                    value: PhysicalConst::IntegerBits(bits),
                })
            }
            SemOpKind::ActorIngressAdapter(adapter) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::ActorIngressAdapter(*adapter),
            }),
            SemOpKind::ConstBool(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Bool(*value),
            }),
            SemOpKind::ConstFloat(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Float(*value),
            }),
            SemOpKind::ConstChar(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Char(*value),
            }),
            SemOpKind::FinishLinearReceiver => Ok(Vec::new()),
            SemOpKind::ConstUnit => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Unit,
            }),
            SemOpKind::ConstDuration(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Duration(*value),
            }),
            SemOpKind::ConstStr(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::String(*value),
            }),
            SemOpKind::ConstBytes(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Bytes(*value),
            }),
            SemOpKind::Unary { op, value } => one(PhysicalOp::Unary {
                dest: self.one_result(operation)?,
                op: *op,
                source: self.value(value.value)?,
            }),
            SemOpKind::Binary { op, lhs, rhs } => one(PhysicalOp::Binary {
                dest: self.one_result(operation)?,
                op: *op,
                lhs: self.value(lhs.value)?,
                rhs: self.value(rhs.value)?,
            }),
            SemOpKind::Cast { value, to } => one(PhysicalOp::Cast {
                dest: self.one_result(operation)?,
                source: self.value(value.value)?,
                to: to.clone(),
            }),
            SemOpKind::TupleMake { elements } => one(PhysicalOp::TupleMake {
                dest: self.one_result(operation)?,
                elements: elements
                    .iter()
                    .map(|element| self.value(element.value))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            SemOpKind::TupleGet { tuple, index } => one(PhysicalOp::TupleGet {
                dest: self.one_result(operation)?,
                tuple: self.value(tuple.value)?,
                index: *index,
            }),
            SemOpKind::ArrayMake { fields } => {
                let dest = self.one_result(operation)?;
                let glue = self
                    .glue_ids
                    .vectors
                    .get(&self.storage[dest.0 as usize].ty)
                    .copied()
                    .ok_or_else(|| PhysicalError::new("fixed array has no element recipe"))?;
                one(PhysicalOp::ArrayMake {
                    dest,
                    glue,
                    fields: fields
                        .iter()
                        .map(|field| self.value(field.value))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            SemOpKind::ArrayRepeat { value } => {
                let dest = self.one_result(operation)?;
                let glue = self
                    .glue_ids
                    .vectors
                    .get(&self.storage[dest.0 as usize].ty)
                    .copied()
                    .ok_or_else(|| PhysicalError::new("fixed array has no element recipe"))?;
                one(PhysicalOp::ArrayRepeat {
                    dest,
                    glue,
                    seed: self.value(value.value)?,
                })
            }
            SemOpKind::AggregateMake { fields, .. } => {
                let dest = self.one_result(operation)?;
                one(PhysicalOp::AggregateMake {
                    dest,
                    fields: fields
                        .iter()
                        .map(|field| self.value(field.value))
                        .collect::<Result<Vec<_>, _>>()?,
                    glue: self.aggregate_id(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::AggregateProjectCopy {
                aggregate, field, ..
            } => {
                let aggregate = self.value(aggregate.value)?;
                let dest = self.one_result(operation)?;
                one(PhysicalOp::AggregateProjectCopy {
                    dest,
                    aggregate,
                    field: *field,
                    glue: self.aggregate_id(&self.storage[aggregate.0 as usize].ty)?,
                    action: self.clone_action(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::AggregateProjectBorrow {
                aggregate, field, ..
            } => {
                let aggregate = self.value(aggregate.value)?;
                one(PhysicalOp::AggregateProjectBorrow {
                    dest: self.one_result(operation)?,
                    aggregate,
                    field: *field,
                    glue: self.aggregate_id(&self.storage[aggregate.0 as usize].ty)?,
                })
            }
            SemOpKind::CopyValue { source } => {
                let dest = self.one_result(operation)?;
                let ty = &operation.results[0].ty;
                one(PhysicalOp::Clone {
                    dest,
                    source: self.value(source.value)?,
                    action: self.clone_action(ty)?,
                })
            }
            SemOpKind::DestroyValue { value } => {
                Self::no_results(operation)?;
                let source = self.value(value.value)?;
                one(PhysicalOp::Destroy {
                    source,
                    action: self.destroy_action(&self.storage[source.0 as usize].ty)?,
                    cleanup: self.cleanup_recipe(operation.id, site, source)?,
                })
            }
            SemOpKind::Move { source } | SemOpKind::Fork { source } => one(PhysicalOp::Transfer {
                dest: self.one_result(operation)?,
                source: self.value(source.value)?,
            }),
            SemOpKind::BeginBorrow { owner } => one(PhysicalOp::Borrow {
                dest: self.one_result(operation)?,
                source: self.value(owner.value)?,
            }),
            SemOpKind::EndBorrow { borrow } => {
                Self::no_results(operation)?;
                one(PhysicalOp::EndBorrow {
                    source: self.value(borrow.value)?,
                })
            }
            SemOpKind::FunctionMake { callable } => one(PhysicalOp::FunctionMake {
                dest: self.one_result(operation)?,
                callee: *callable,
            }),
            SemOpKind::TaskScopeEnter {
                scope,
                parent,
                duration,
            } => one(PhysicalOp::TaskScopeEnter {
                scope: *scope,
                parent: *parent,
                duration: duration
                    .as_ref()
                    .map(|duration| self.value(duration.value))
                    .transpose()?,
            }),
            SemOpKind::TaskScopeClose { scope } => {
                one(PhysicalOp::TaskScopeClose { scope: *scope })
            }
            SemOpKind::GeneratorMake { closure, callable } => {
                one(self.lower_generator_make(operation, *closure, callable)?)
            }
            SemOpKind::StreamPipe { capacity } => {
                let [stream, sink] = operation.results.as_slice() else {
                    return Err(PhysicalError::new("stream pipe lacks its two halves"));
                };
                let element = hew_sir::pipe_parts(&stream.ty, &sink.ty)
                    .ok_or_else(|| PhysicalError::new("stream pipe halves disagree on element"))?;
                one(PhysicalOp::StreamPipe {
                    capacity: *capacity,
                    stream: self.value(stream.id)?,
                    sink: self.value(sink.id)?,
                    element: physical_value_recipe(self.module, self.glue_ids, element)?,
                })
            }
            SemOpKind::TaskSpawn { scope, callable } => {
                let dest = self.one_result(operation)?;
                let ResolvedTy::Task(output) = &self.storage[dest.0 as usize].ty else {
                    return Err(PhysicalError::new("task spawn has no exact output type"));
                };
                one(PhysicalOp::TaskSpawn {
                    scope: *scope,
                    callable: self.value(callable.value)?,
                    dest,
                    output: (output.as_ref() != &ResolvedTy::Never)
                        .then(|| physical_value_recipe(self.module, self.glue_ids, output))
                        .transpose()?,
                })
            }
            SemOpKind::ClosureMake { closure, fields } => one(PhysicalOp::ClosureMake {
                dest: self.one_result(operation)?,
                closure: *closure,
                fields: fields
                    .iter()
                    .map(|field| self.value(field.value))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            SemOpKind::DynMake { vtable, value } => one(PhysicalOp::DynMake {
                dest: self.one_result(operation)?,
                vtable: PhysicalVtableId(vtable.0),
                source: self.value(value.value)?,
            }),
            SemOpKind::GeneratorCoerce { source } => one(PhysicalOp::GeneratorCoerce {
                dest: self.one_result(operation)?,
                source: self.value(source.value)?,
            }),
            SemOpKind::CallableCoerce { source } => one(PhysicalOp::CallableCoerce {
                dest: self.one_result(operation)?,
                source: self.value(source.value)?,
            }),
            SemOpKind::LoadBorrow { place, .. } => one(PhysicalOp::Borrow {
                dest: self.one_result(operation)?,
                source: self.place(*place)?,
            }),
            SemOpKind::AllocPlace { place } => {
                Self::no_results(operation)?;
                one(PhysicalOp::StorageLive {
                    storage: self.place(*place)?,
                })
            }
            SemOpKind::LoadCopy { place } => {
                let source = self.place(*place)?;
                let ty = &self.storage[source.0 as usize].ty;
                one(PhysicalOp::Clone {
                    dest: self.one_result(operation)?,
                    source,
                    action: self.clone_action(ty)?,
                })
            }
            SemOpKind::LoadTake { place } => one(PhysicalOp::Transfer {
                dest: self.one_result(operation)?,
                source: self.place(*place)?,
            }),
            SemOpKind::StoreInit { place, value } => {
                Self::no_results(operation)?;
                one(PhysicalOp::Transfer {
                    dest: self.place(*place)?,
                    source: self.value(value.value)?,
                })
            }
            SemOpKind::StoreAssign { place, value } => {
                Self::no_results(operation)?;
                let dest = self.place(*place)?;
                let source = self.value(value.value)?;
                one(PhysicalOp::Assign {
                    dest,
                    source,
                    destroy_old: self.optional_destroy(dest)?,
                    cleanup: self.cleanup_recipe(operation.id, site, dest)?,
                })
            }
            SemOpKind::EndLifetime { place } => {
                Self::no_results(operation)?;
                let storage = self.place(*place)?;
                one(PhysicalOp::StorageDead {
                    storage,
                    destroy: self.optional_destroy(storage)?,
                    cleanup: self.cleanup_recipe(operation.id, site, storage)?,
                })
            }
            SemOpKind::Destructure { aggregate, .. } => {
                let aggregate = self.value(aggregate.value)?;
                one(PhysicalOp::AggregateDestructure {
                    aggregate,
                    fields: operation
                        .results
                        .iter()
                        .map(|result| self.value(result.id))
                        .collect::<Result<Vec<_>, _>>()?,
                    glue: self.aggregate_id(&self.storage[aggregate.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantMake {
                variant, fields, ..
            } => {
                let dest = self.one_result(operation)?;
                one(PhysicalOp::VariantMake {
                    dest,
                    variant: *variant,
                    fields: fields
                        .iter()
                        .map(|field| self.value(field.value))
                        .collect::<Result<Vec<_>, _>>()?,
                    glue: self.variant_id(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantIs {
                variant, source, ..
            } => {
                let source = self.value(source.value)?;
                one(PhysicalOp::VariantIs {
                    dest: self.one_result(operation)?,
                    source,
                    variant: *variant,
                    glue: self.variant_id(&self.storage[source.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantProjectCopy {
                variant,
                source,
                field,
                ..
            } => {
                let source = self.value(source.value)?;
                let dest = self.one_result(operation)?;
                one(PhysicalOp::VariantProjectCopy {
                    dest,
                    source,
                    variant: *variant,
                    field: *field,
                    glue: self.variant_id(&self.storage[source.0 as usize].ty)?,
                    action: self.clone_action(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantProjectBorrow {
                variant,
                source,
                field,
                ..
            } => {
                let source = self.value(source.value)?;
                one(PhysicalOp::VariantProjectBorrow {
                    dest: self.one_result(operation)?,
                    source,
                    variant: *variant,
                    field: *field,
                    glue: self.variant_id(&self.storage[source.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantDestructure {
                variant, source, ..
            } => {
                let source = self.value(source.value)?;
                one(PhysicalOp::VariantDestructure {
                    source,
                    variant: *variant,
                    fields: operation
                        .results
                        .iter()
                        .map(|result| self.value(result.id))
                        .collect::<Result<Vec<_>, _>>()?,
                    glue: self.variant_id(&self.storage[source.0 as usize].ty)?,
                })
            }
            SemOpKind::StrEq { .. } | SemOpKind::BytesEq { .. } => {
                Err(PhysicalError::new(format!(
                    "SIR op {} is not yet admitted by physical MIR",
                    operation.id.0
                )))
            }
        }
    }

    pub(crate) fn lower_edge(&self, edge: &Edge) -> Result<PhysicalEdge, PhysicalError> {
        let target = self
            .function
            .blocks
            .iter()
            .find(|block| block.id == edge.target)
            .ok_or_else(|| PhysicalError::new(format!("unknown SIR block {}", edge.target.0)))?;
        if edge.args.len() != target.args.len() {
            return Err(PhysicalError::new(format!(
                "edge to block {} has {} arguments for {} block parameters",
                edge.target.0,
                edge.args.len(),
                target.args.len()
            )));
        }
        let transfers = edge
            .args
            .iter()
            .zip(&target.args)
            .map(|(source, dest)| Ok((self.value(source.value)?, self.value(dest.value)?)))
            .collect::<Result<Vec<_>, PhysicalError>>()?;
        let leaf_transfers = edge
            .args
            .iter()
            .zip(&target.args)
            .map(|(source, dest)| {
                self.projections
                    .transfer(source.value, dest.value)
                    .map_err(PhysicalError::new)?
                    .into_iter()
                    .map(|(source, dest)| Ok((self.place(source)?, self.place(dest)?)))
                    .collect::<Result<Vec<_>, PhysicalError>>()
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect();
        Ok(PhysicalEdge {
            target: edge.target,
            transfers,
            leaf_transfers,
        })
    }

    #[allow(
        clippy::too_many_lines,
        reason = "keep the exhaustive semantic-to-physical terminator mapping together"
    )]
    pub(crate) fn lower_terminator(
        &self,
        terminator: &SemTerminator,
    ) -> Result<PhysicalTerminator, PhysicalError> {
        match terminator {
            SemTerminator::EnterDefer { defer, park, body } => Ok(PhysicalTerminator::EnterDefer {
                defer: *defer,
                park: *park,
                body: self.lower_edge(body)?,
            }),
            SemTerminator::FinishDefer { defer, park, next } => {
                Ok(PhysicalTerminator::FinishDefer {
                    defer: *defer,
                    park: *park,
                    next: self.lower_edge(next)?,
                })
            }
            SemTerminator::CleanupDispatch { normal, fault } => {
                Ok(PhysicalTerminator::CleanupDispatch {
                    normal: self.lower_edge(normal)?,
                    fault: self.lower_edge(fault)?,
                })
            }
            SemTerminator::RecoverFault {
                result,
                deadline_variant,
                fault_variant,
                normal,
                unwind,
            } => Ok(PhysicalTerminator::RecoverFault {
                result: self.value(result.id)?,
                glue: self.variant_id(&result.ty)?,
                deadline_variant: *deadline_variant,
                fault_variant: *fault_variant,
                normal: self.lower_edge(normal)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::CheckedRaiseFault { kind, cleanup } => {
                Ok(PhysicalTerminator::CheckedRaiseFault {
                    kind: *kind,
                    cleanup: self.lower_edge(cleanup)?,
                })
            }
            SemTerminator::Return { value } => Ok(PhysicalTerminator::Return {
                value: value
                    .as_ref()
                    .map(|value| self.return_transfer(value.operand.value, value.decision))
                    .transpose()?,
            }),
            SemTerminator::Goto(edge) => Ok(PhysicalTerminator::Goto(self.lower_edge(edge)?)),
            SemTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => Ok(PhysicalTerminator::Branch {
                condition: self.value(condition.value)?,
                then_target: self.lower_edge(then_target)?,
                else_target: self.lower_edge(else_target)?,
            }),
            SemTerminator::SwitchVariant {
                shape,
                scrutinee,
                arms,
                ..
            } => self.lower_variant_switch(*shape, scrutinee, arms),
            checked @ SemTerminator::CheckedBinary { .. } => self.lower_checked_binary(checked),
            SemTerminator::Call {
                callee,
                args,
                result,
                normal,
                unwind,
                handback,
                ..
            } => Ok(PhysicalTerminator::Call {
                handback: handback
                    .as_ref()
                    .map(|handback| self.value(handback.id))
                    .transpose()?,
                callee: *callee,
                args: self.argument_transfers(args)?,
                result: match result {
                    CallResult::Unit | CallResult::Never => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: normal
                    .as_ref()
                    .map(|edge| self.lower_edge(edge))
                    .transpose()?,
                unwind: match unwind {
                    CallUnwind::NotApplicable => None,
                    CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
                },
            }),
            SemTerminator::ActorCall {
                operation,
                args,
                result,
                normal,
                unwind,
                ..
            } => Ok(PhysicalTerminator::ActorCall {
                operation: operation.clone(),
                args: self.argument_transfers(args)?,
                result: match result {
                    CallResult::Unit | CallResult::Never => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: self.lower_edge(normal)?,
                unwind: match unwind {
                    CallUnwind::NotApplicable => None,
                    CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
                },
            }),
            SemTerminator::ExternCall {
                signature,
                args,
                result,
                normal,
                ..
            } => Ok(PhysicalTerminator::ExternCall {
                symbol: signature.symbol.clone(),
                runtime_capability: signature.runtime_capability,
                args: self.argument_transfers(args)?,
                result_abi: self.target.extern_result_abi(&signature.result)?,
                result: match result {
                    CallResult::Unit | CallResult::Never => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: self.lower_edge(normal)?,
            }),
            SemTerminator::WireCodec {
                direction,
                plan,
                text_result,
                args,
                result,
                normal,
                unwind,
                ..
            } => {
                let (CallResult::Value(result), CallUnwind::Cleanup(unwind)) = (result, unwind)
                else {
                    return Err(PhysicalError::new(
                        "wire codec requires result and fault cleanup",
                    ));
                };
                let transfers = self.argument_transfers(args)?;
                let [input] = transfers.as_slice() else {
                    return Err(PhysicalError::new("wire codec requires one borrowed input"));
                };
                let mut types = BTreeSet::new();
                plan.visit_types(&mut |ty| {
                    types.insert(ty.clone());
                });
                let recipes = types
                    .into_iter()
                    .map(|ty| {
                        physical_value_recipe(self.module, self.glue_ids, &ty)
                            .map(|recipe| (ty, recipe))
                    })
                    .collect::<Result<_, _>>()?;
                let text_result = text_result
                    .map(|cases| {
                        self.variant_id(&result.ty)
                            .map(|glue| PhysicalWireTextResult {
                                glue,
                                ok: cases.ok,
                                error: cases.error,
                            })
                    })
                    .transpose()?;
                Ok(PhysicalTerminator::WireCodec {
                    text_result,
                    direction: *direction,
                    plan: std::sync::Arc::clone(plan),
                    recipes,
                    input: *input,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(normal)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::RtCall {
                family,
                args,
                result,
                normal,
                unwind,
                ..
            } => Ok(PhysicalTerminator::RuntimeCall {
                action: self.runtime_action(*family, args, result)?,
                args: self.argument_transfers(args)?,
                result: match result {
                    CallResult::Unit | CallResult::Never => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: self.lower_edge(normal)?,
                failure: match unwind {
                    CallUnwind::NotApplicable => None,
                    CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
                },
            }),
            call @ SemTerminator::IndirectCall { .. } => self.lower_indirect_call(call),
            call @ SemTerminator::DynCall { .. } => self.lower_dyn_call(call),
            SemTerminator::ValueCall {
                ty,
                capability,
                args,
                result,
                normal,
                unwind,
                ..
            } => {
                let (CallResult::Value(result), CallUnwind::Cleanup(unwind)) = (result, unwind)
                else {
                    return Err(PhysicalError::new(
                        "selected value call requires a scalar result and fault cleanup",
                    ));
                };
                Ok(PhysicalTerminator::ValueCall {
                    ty: ty.clone(),
                    capability: *capability,
                    args: self.argument_transfers(args)?,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(normal)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Panic { message, cleanup } => Ok(PhysicalTerminator::Panic {
                message: self.argument_transfers(std::slice::from_ref(message))?[0],
                cleanup: self.lower_edge(cleanup)?,
            }),
            SemTerminator::Trap { kind } => Ok(PhysicalTerminator::Trap(*kind)),
            SemTerminator::ResumeUnwind { handback } => Ok(PhysicalTerminator::PropagateFault {
                handback: handback
                    .as_ref()
                    .map(|handback| self.value(handback.operand.value))
                    .transpose()?,
            }),
            SemTerminator::Unreachable => Ok(PhysicalTerminator::Unreachable),
            term @ SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Yield | hew_sir::SuspendKind::GeneratorNext,
                ..
            } => self.lower_generator_suspend(term),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Select { has_timeout, order },
                inputs,
                result,
                resumes,
                cancel,
                unwind,
            } => {
                let (tasks, timeout) = if *has_timeout {
                    let (duration, tasks) = inputs.split_last().ok_or_else(|| {
                        PhysicalError::new("timed selection lacks its duration input")
                    })?;
                    (tasks, Some(self.value(duration.operand.value)?))
                } else {
                    (inputs.as_slice(), None)
                };
                let CallResult::Value(result) = result else {
                    return Err(PhysicalError::new(
                        "selection lacks its source index result",
                    ));
                };
                let sources = self
                    .argument_transfers(tasks)?
                    .into_iter()
                    .zip(tasks)
                    .map(|(transfer, input)| {
                        let ty = &self.storage[self.value(input.operand.value)?.0 as usize].ty;
                        // The operand's own type says which substrate this arm
                        // observes; the selection has no other authority.
                        if ty.is_builtin(hew_types::BuiltinType::Stream) {
                            Ok(PhysicalSelectSource::StreamNext(transfer))
                        } else if ty.is_builtin(hew_types::BuiltinType::ActorCall) {
                            Ok(PhysicalSelectSource::ActorCall(transfer))
                        } else {
                            Ok(PhysicalSelectSource::Task(transfer))
                        }
                    })
                    .collect::<Result<Vec<_>, PhysicalError>>()?;
                Ok(PhysicalTerminator::TaskSelect {
                    order: *order,
                    sources,
                    timeout,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(&resumes[0])?,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::NativeIo { operation },
                inputs,
                result: CallResult::Value(result),
                resumes,
                cancel,
                unwind,
            } => Ok(PhysicalTerminator::NativeIo {
                operation: *operation,
                args: self.argument_transfers(inputs)?,
                result: self.value(result.id)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Sleep,
                inputs,
                resumes,
                cancel,
                unwind,
                ..
            } => Ok(PhysicalTerminator::Sleep {
                duration: self.value(inputs[0].operand.value)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::SleepUntil,
                inputs,
                resumes,
                cancel,
                unwind,
                ..
            } => Ok(PhysicalTerminator::SleepUntil {
                deadline: self.value(inputs[0].operand.value)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Await,
                inputs,
                result,
                resumes,
                cancel,
                unwind,
            } => Ok(PhysicalTerminator::TaskAwait {
                task: self.argument_transfers(inputs)?[0],
                result: match result {
                    CallResult::Value(value) => Some(self.value(value.id)?),
                    CallResult::Unit | CallResult::Never => None,
                },
                normal: resumes
                    .first()
                    .map(|edge| self.lower_edge(edge))
                    .transpose()?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Join { scope, mode },
                resumes,
                unwind,
                ..
            } => Ok(PhysicalTerminator::TaskScopeJoin {
                scope: *scope,
                mode: *mode,
                normal: self.lower_edge(&resumes[0])?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind:
                    hew_sir::SuspendKind::Ask {
                        actor,
                        message,
                        policy,
                        deadline_ns,
                        sealed,
                    },
                inputs,
                result: CallResult::Value(result),
                resumes,
                cancel,
                unwind,
            } => Ok(PhysicalTerminator::ActorAsk {
                actor: *actor,
                message: *message,
                policy: *policy,
                deadline_ns: *deadline_ns,
                sealed: *sealed,
                args: self.argument_transfers(inputs)?,
                result: self.value(result.id)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::RemoteAsk { actor, message },
                inputs,
                result: CallResult::Value(result),
                resumes,
                cancel,
                unwind,
            } => Ok(PhysicalTerminator::RemoteAsk {
                actor: *actor,
                message: *message,
                target: self.value(inputs[0].operand.value)?,
                payload: self.value(inputs[1].operand.value)?,
                timeout: self.value(inputs[2].operand.value)?,
                result: self.value(result.id)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::StreamNext { park },
                inputs,
                result: CallResult::Value(result),
                resumes,
                cancel,
                unwind,
            } => {
                let shape = self
                    .module
                    .variant_shape_for_type(&result.ty)
                    .ok_or_else(|| {
                        PhysicalError::new("stream receive lacks its Option descriptor")
                    })?;
                let element = shape
                    .variants
                    .first()
                    .and_then(|variant| variant.fields.first())
                    .map(|field| field.ty.clone())
                    .ok_or_else(|| PhysicalError::new("stream receive lacks its element type"))?;
                Ok(PhysicalTerminator::StreamNext {
                    park: *park,
                    stream: self.argument_transfers(inputs)?[0],
                    element: physical_value_recipe(self.module, self.glue_ids, &element)?,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(&resumes[0])?,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::StreamSend { park },
                inputs,
                resumes,
                cancel,
                unwind,
                ..
            } => {
                let transfers = self.argument_transfers(inputs)?;
                let (normal, closed, full) = match resumes.as_slice() {
                    [normal, closed] if *park => (normal, closed, None),
                    [normal, closed, full] if !*park => (normal, closed, Some(full)),
                    _ => {
                        return Err(PhysicalError::new(
                            "stream send lacks its accepted, closed and full resumes",
                        ))
                    }
                };
                let element = &self.storage[self.value(inputs[1].operand.value)?.0 as usize].ty;
                Ok(PhysicalTerminator::StreamSend {
                    park: *park,
                    sink: transfers[0],
                    value: transfers[1],
                    element: physical_value_recipe(self.module, self.glue_ids, element)?,
                    normal: self.lower_edge(normal)?,
                    closed: self.lower_edge(closed)?,
                    full: full.map(|full| self.lower_edge(full)).transpose()?,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Suspend { .. } => Err(PhysicalError::new(
                "suspension lacks a physical operation contract",
            )),
        }
    }

    pub(crate) fn lower_variant_switch(
        &self,
        shape: hew_sir::VariantShapeId,
        scrutinee: &hew_sir::Operand,
        arms: &[hew_sir::SemVariantArm],
    ) -> Result<PhysicalTerminator, PhysicalError> {
        let scrutinee = self.value(scrutinee.value)?;
        let scrutinee_ty = &self.storage[scrutinee.0 as usize].ty;
        let glue = self.variant_id(scrutinee_ty)?;
        let expected_shape = self
            .module
            .variant_shape_for_type(scrutinee_ty)
            .map(|descriptor| descriptor.id)
            .ok_or_else(|| PhysicalError::new("variant switch has no exact descriptor"))?;
        if shape != expected_shape {
            return Err(PhysicalError::new(
                "variant switch shape disagrees with its scrutinee type",
            ));
        }
        Ok(PhysicalTerminator::SwitchVariant {
            scrutinee,
            glue,
            arms: arms
                .iter()
                .map(|arm| {
                    Ok(PhysicalVariantArm {
                        variant: arm.variant,
                        fields: arm
                            .fields
                            .iter()
                            .map(|field| self.value(field.id))
                            .collect::<Result<Vec<_>, _>>()?,
                        target: self.lower_edge(&arm.target)?,
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?,
        })
    }

    pub(crate) fn lower_checked_binary(
        &self,
        terminator: &SemTerminator,
    ) -> Result<PhysicalTerminator, PhysicalError> {
        let SemTerminator::CheckedBinary {
            op,
            lhs,
            rhs,
            result,
            normal,
            failures,
            ..
        } = terminator
        else {
            return Err(PhysicalError::new(
                "physical checked-binary lowering received another terminator",
            ));
        };
        Ok(PhysicalTerminator::CheckedBinary {
            op: *op,
            lhs: self.value(lhs.value)?,
            rhs: self.value(rhs.value)?,
            result: self.value(result.id)?,
            normal: self.lower_edge(normal)?,
            failures: failures
                .iter()
                .map(|failure| {
                    Ok(PhysicalCheckedFailure {
                        kind: failure.kind,
                        edge: self.lower_edge(&failure.edge)?,
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?,
        })
    }

    pub(crate) fn argument_transfers(
        &self,
        args: &[hew_sir::BoundaryOperand],
    ) -> Result<Vec<ArgumentTransfer>, PhysicalError> {
        args.iter()
            .map(|argument| self.argument_transfer(argument.operand.value, argument.decision))
            .collect()
    }

    pub(crate) fn argument_transfer(
        &self,
        value: ValueId,
        decision: BoundaryDecision,
    ) -> Result<ArgumentTransfer, PhysicalError> {
        let source = self.value(value)?;
        Ok(match decision {
            BoundaryDecision::Borrow => ArgumentTransfer::Borrow(source),
            BoundaryDecision::BorrowMut => ArgumentTransfer::BorrowMut(source),
            BoundaryDecision::Move => ArgumentTransfer::Move(source),
            BoundaryDecision::Copy => ArgumentTransfer::Clone {
                source,
                action: self.clone_action(&self.storage[source.0 as usize].ty)?,
            },
            BoundaryDecision::Snapshot(
                SnapshotDecision::Share | SnapshotDecision::DeepCopy | SnapshotDecision::Transfer,
            ) => {
                return Err(PhysicalError::new(
                    "snapshot boundaries are not yet admitted by physical MIR",
                ));
            }
        })
    }

    pub(crate) fn return_transfer(
        &self,
        value: ValueId,
        decision: BoundaryDecision,
    ) -> Result<ReturnTransfer, PhysicalError> {
        let source = self.value(value)?;
        Ok(match decision {
            BoundaryDecision::Borrow => ReturnTransfer::Borrow(source),
            BoundaryDecision::BorrowMut => {
                return Err(PhysicalError::new(
                    "exclusive callable loans cannot escape through a return",
                ));
            }
            BoundaryDecision::Move => ReturnTransfer::Move(source),
            BoundaryDecision::Copy => ReturnTransfer::Clone {
                source,
                action: self.clone_action(&self.storage[source.0 as usize].ty)?,
            },
            BoundaryDecision::Snapshot(_) => {
                return Err(PhysicalError::new(
                    "snapshot returns are not yet admitted by physical MIR",
                ));
            }
        })
    }

    pub(crate) fn clone_action(&self, ty: &ResolvedTy) -> Result<CloneAction, PhysicalError> {
        let facts = self
            .module
            .type_facts
            .get(&TypeInstanceKey(ty.clone()))
            .ok_or_else(|| {
                PhysicalError::new(format!(
                    "physical copy of `{}` has no checker-owned type facts",
                    ty.user_facing()
                ))
            })?;
        clone_action_for_type(ty, facts.clone, self.glue_ids)?.ok_or_else(|| {
            PhysicalError::new(format!(
                "physical copy of `{}` has no admitted clone action",
                ty.user_facing()
            ))
        })
    }

    /// The release this body *is*, when the body being lowered is the exact
    /// `close` that releases `ty`. Selecting that release again would re-enter
    /// `close`.
    pub(super) fn own_close_body_of(&self, ty: &ResolvedTy) -> Option<&hew_sir::ResourceRelease> {
        let release = self.module.resources.get(ty)?;
        let close = match release {
            hew_sir::ResourceRelease::RecordClose { close, .. }
            | hew_sir::ResourceRelease::OpaqueClose { close, .. } => *close,
            _ => return None,
        };
        (close == self.function.callable).then_some(release)
    }

    pub(crate) fn destroy_action(&self, ty: &ResolvedTy) -> Result<DestroyAction, PhysicalError> {
        // A record resource's members drop directly inside its own close. An
        // authored opaque handle has no members and no release but the close
        // already running; `cleanup_recipe` proves no cleanup site in that
        // body still holds one, so the ordinary recipe below never runs there.
        if matches!(
            self.own_close_body_of(ty),
            Some(hew_sir::ResourceRelease::RecordClose { .. })
        ) {
            return self
                .glue_ids
                .aggregates
                .get(ty)
                .copied()
                .map(DestroyAction::Aggregate)
                .ok_or_else(|| {
                    PhysicalError::new(format!(
                        "record resource `{}` has no member glue for its own close body",
                        ty.user_facing()
                    ))
                });
        }
        destroy_action_for_type(ty, self.glue_ids).ok_or_else(|| {
            PhysicalError::new(format!(
                "physical destroy action for `{}` is not implemented",
                ty.user_facing()
            ))
        })
    }

    pub(crate) fn collection_receiver_type<'a>(
        &'a self,
        args: &[hew_sir::BoundaryOperand],
        result: &'a hew_sir::ValueDef,
        constructor: bool,
    ) -> Result<&'a ResolvedTy, PhysicalError> {
        if constructor {
            return Ok(&result.ty);
        }
        let receiver = args
            .first()
            .ok_or_else(|| PhysicalError::new("collection operation lacks its receiver"))?;
        Ok(&self.storage[self.value(receiver.operand.value)?.0 as usize].ty)
    }

    pub(crate) fn map_carrier(
        &self,
        family: RuntimeCallFamily,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeCarrier, PhysicalError> {
        let RuntimeCallFamily::Map(op) = family else {
            return Err(PhysicalError::new("map carrier requires a map operation"));
        };
        let CallResult::Value(value) = result else {
            return Err(PhysicalError::new("map operation has no result value"));
        };
        let receiver = self.collection_receiver_type(args, value, op == MapValueOp::New)?;
        let glue = self
            .glue_ids
            .maps
            .get(receiver)
            .copied()
            .ok_or_else(|| PhysicalError::new("map receiver has no physical glue identity"))?;
        let operation = match op {
            MapValueOp::New => PhysicalMapOp::New,
            MapValueOp::Len => PhysicalMapOp::Len,
            MapValueOp::Index => PhysicalMapOp::Index,
            MapValueOp::Get => PhysicalMapOp::Get {
                result: self.variant_id(&value.ty)?,
            },
            MapValueOp::GetBorrow => PhysicalMapOp::GetBorrow {
                result: self.variant_id(&value.ty)?,
            },
            MapValueOp::ContainsKey => PhysicalMapOp::ContainsKey,
            MapValueOp::Insert => PhysicalMapOp::Insert,
            MapValueOp::Remove => {
                let ResolvedTy::Tuple(fields) = &value.ty else {
                    return Err(PhysicalError::new(
                        "map removal lacks its receiver/value pair",
                    ));
                };
                let optional = fields
                    .get(1)
                    .ok_or_else(|| PhysicalError::new("map removal lacks its optional value"))?;
                PhysicalMapOp::Remove {
                    result: self.aggregate_id(&value.ty)?,
                    value: self.variant_id(optional)?,
                }
            }
            MapValueOp::Clear => PhysicalMapOp::Clear,
            MapValueOp::Keys => PhysicalMapOp::Keys,
            MapValueOp::Values => PhysicalMapOp::Values,
            MapValueOp::Entries => PhysicalMapOp::Entries {
                result: self
                    .glue_ids
                    .vectors
                    .get(&value.ty)
                    .copied()
                    .ok_or_else(|| PhysicalError::new("map entries lack their vector recipe"))?,
            },
        };
        Ok(PhysicalRuntimeCarrier::Map { operation, glue })
    }

    pub(crate) fn set_carrier(
        &self,
        family: RuntimeCallFamily,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeCarrier, PhysicalError> {
        let RuntimeCallFamily::Set(op) = family else {
            return Err(PhysicalError::new("set carrier requires a set operation"));
        };
        let CallResult::Value(value) = result else {
            return Err(PhysicalError::new("set operation has no result value"));
        };
        let receiver = self.collection_receiver_type(args, value, op == SetValueOp::New)?;
        let glue = self
            .glue_ids
            .sets
            .get(receiver)
            .copied()
            .ok_or_else(|| PhysicalError::new("set receiver has no physical glue identity"))?;
        let operation = match op {
            SetValueOp::New => PhysicalSetOp::New,
            SetValueOp::Len => PhysicalSetOp::Len,
            SetValueOp::Contains => PhysicalSetOp::Contains,
            SetValueOp::Insert => PhysicalSetOp::Insert {
                result: self.aggregate_id(&value.ty)?,
            },
            SetValueOp::Remove => PhysicalSetOp::Remove {
                result: self.aggregate_id(&value.ty)?,
            },
            SetValueOp::Clear => PhysicalSetOp::Clear,
            SetValueOp::Elements => PhysicalSetOp::Elements,
        };
        Ok(PhysicalRuntimeCarrier::Set { operation, glue })
    }

    /// Resolve one verified runtime operation into its physical action.
    ///
    /// The operation's row says which physical form it takes, so the shapes
    /// below are selected by that fact rather than recognised again here.
    pub(crate) fn runtime_action(
        &self,
        family: RuntimeCallFamily,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeAction, PhysicalError> {
        let carrier = match family.row().physical {
            RuntimePhysicalForm::NotAnAction => {
                return Err(PhysicalError::new(format!(
                    "runtime operation `{family:?}` has no physical no-unwind ABI action"
                )))
            }
            RuntimePhysicalForm::Direct => PhysicalRuntimeCarrier::None,
            RuntimePhysicalForm::Map => self.map_carrier(family, args, result)?,
            RuntimePhysicalForm::Set => self.set_carrier(family, args, result)?,
            RuntimePhysicalForm::Vector => self.vector_carrier(family, args, result)?,
            RuntimePhysicalForm::SharedHandle => self.shared_carrier(family, args, result)?,
            RuntimePhysicalForm::StructuralFormat => {
                let operand = args
                    .first()
                    .ok_or_else(|| PhysicalError::new("structural rendering lacks its operand"))?;
                let ty = self.storage[self.value(operand.operand.value)?.0 as usize]
                    .ty
                    .clone();
                PhysicalRuntimeCarrier::StructuralFormat(
                    self.structural.borrow_mut().intern(self.module, &ty)?,
                )
            }
            RuntimePhysicalForm::NodeResult => self.node_result_carrier(family, args, result)?,
            RuntimePhysicalForm::VariantResult => {
                let CallResult::Value(value) = result else {
                    return Err(PhysicalError::new(
                        "optional runtime result has no result value",
                    ));
                };
                PhysicalRuntimeCarrier::Variant(self.variant_id(&value.ty)?)
            }
            RuntimePhysicalForm::PairWithOption => {
                let CallResult::Value(value) = result else {
                    return Err(PhysicalError::new("bytes pop has no transformed result"));
                };
                let ResolvedTy::Tuple(fields) = &value.ty else {
                    return Err(PhysicalError::new("bytes pop result is not a pair"));
                };
                let [_, option] = fields.as_slice() else {
                    return Err(PhysicalError::new("bytes pop result is not a pair"));
                };
                PhysicalRuntimeCarrier::PairWithOption {
                    pair: self.aggregate_id(&value.ty)?,
                    option: self.variant_id(option)?,
                }
            }
            RuntimePhysicalForm::Utf8Decode => {
                let CallResult::Value(value) = result else {
                    return Err(PhysicalError::new("UTF-8 decode has no result value"));
                };
                let refs = hew_sir::runtime_variant_shape_refs(
                    &self.module.defs,
                    hew_types::RuntimeVariantResultKind::Utf8Decode,
                    &value.ty,
                    &self.module.aggregate_shapes,
                    &self.module.variant_shapes,
                )
                .map_err(PhysicalError::new)?;
                PhysicalRuntimeCarrier::Utf8Decode {
                    result: self.variant_id(&value.ty)?,
                    error: self.aggregate_id(
                        &self.module.aggregate_shapes[refs.error.0 as usize].aggregate_ty,
                    )?,
                    error_len: self.variant_id(
                        &self.module.variant_shapes[refs.error_len.0 as usize].enum_ty,
                    )?,
                }
            }
        };
        Ok(PhysicalRuntimeAction { family, carrier })
    }

    /// Fixed arrays share the vector glue and the vector operation vocabulary.
    /// The shared allocation one `Rc` operation reaches. `Rc.new` names it
    /// through its result; every other form names it through its receiver.
    pub(crate) fn shared_carrier(
        &self,
        family: RuntimeCallFamily,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeCarrier, PhysicalError> {
        let handle = if family == RuntimeCallFamily::RcNew {
            let CallResult::Value(value) = result else {
                return Err(PhysicalError::new("`Rc.new` has no result value"));
            };
            value.ty.clone()
        } else {
            let receiver = args
                .first()
                .ok_or_else(|| PhysicalError::new("shared handle operation has no receiver"))?;
            self.storage[self.value(receiver.operand.value)?.0 as usize]
                .ty
                .clone()
        };
        let glue = self.glue_ids.shared.get(&handle).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "shared handle `{}` has no physical glue identity",
                handle.user_facing()
            ))
        })?;
        Ok(PhysicalRuntimeCarrier::SharedHandle(glue))
    }

    pub(crate) fn vector_carrier(
        &self,
        family: RuntimeCallFamily,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeCarrier, PhysicalError> {
        if let RuntimeCallFamily::Array(op) = family {
            let receiver = args
                .first()
                .ok_or_else(|| PhysicalError::new("array operation has no receiver"))?;
            let ty = &self.storage[self.value(receiver.operand.value)?.0 as usize].ty;
            let glue = self
                .glue_ids
                .vectors
                .get(ty)
                .copied()
                .ok_or_else(|| PhysicalError::new("array receiver has no element recipe"))?;
            let operation = match op {
                ArrayValueOp::Len => PhysicalVectorOp::Len,
                ArrayValueOp::Index => PhysicalVectorOp::Index,
                ArrayValueOp::IndexBorrow => PhysicalVectorOp::IndexBorrow,
                ArrayValueOp::Set => PhysicalVectorOp::Set,
            };
            return Ok(PhysicalRuntimeCarrier::Vector { operation, glue });
        }
        let RuntimeCallFamily::Vector(op) = family else {
            return Err(PhysicalError::new(
                "vector carrier requires a vector operation",
            ));
        };
        let CallResult::Value(value) = result else {
            return Err(PhysicalError::new("vector operation has no result value"));
        };
        let vector = if op == VecValueOp::New {
            &value.ty
        } else {
            let receiver = args
                .first()
                .ok_or_else(|| PhysicalError::new("vector operation has no receiver"))?;
            &self.storage[self.value(receiver.operand.value)?.0 as usize].ty
        };
        let glue = self.glue_ids.vectors.get(vector).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "vector `{}` has no physical glue identity",
                vector.user_facing()
            ))
        })?;
        let operation = match op {
            VecValueOp::New => PhysicalVectorOp::New,
            VecValueOp::Len => PhysicalVectorOp::Len,
            VecValueOp::Contains => PhysicalVectorOp::Contains,
            VecValueOp::Index => PhysicalVectorOp::Index,
            VecValueOp::Get => PhysicalVectorOp::Get {
                result: self.variant_id(&value.ty)?,
            },
            VecValueOp::Push => PhysicalVectorOp::Push,
            VecValueOp::Set => PhysicalVectorOp::Set,
            VecValueOp::Pop => PhysicalVectorOp::Pop {
                result: self.aggregate_id(&value.ty)?,
            },
            VecValueOp::Remove => PhysicalVectorOp::Remove {
                result: self.aggregate_id(&value.ty)?,
            },
            VecValueOp::Clear => PhysicalVectorOp::Clear,
            VecValueOp::IndexBorrow => PhysicalVectorOp::IndexBorrow,
            VecValueOp::GetBorrow => PhysicalVectorOp::GetBorrow {
                result: self.variant_id(&value.ty)?,
            },
            VecValueOp::TakeFirst => PhysicalVectorOp::TakeFirst {
                result: self.aggregate_id(&value.ty)?,
            },
            VecValueOp::TakeAll => PhysicalVectorOp::TakeAll {
                result: self.aggregate_id(&value.ty)?,
            },
            VecValueOp::Slice => PhysicalVectorOp::Slice,
            VecValueOp::SliceFrom => PhysicalVectorOp::SliceFrom,
            VecValueOp::Append => PhysicalVectorOp::Append,
            VecValueOp::Join => PhysicalVectorOp::Join,
        };
        Ok(PhysicalRuntimeCarrier::Vector { operation, glue })
    }

    /// `Node::start`, `Node::connect` and `Node::lookup` all return a `Result`
    /// whose success and error cases are separate variant glue.
    pub(crate) fn node_result_carrier(
        &self,
        family: RuntimeCallFamily,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeCarrier, PhysicalError> {
        if family == RuntimeCallFamily::NodeStart {
            let config = args
                .first()
                .ok_or_else(|| PhysicalError::new("Node::start has no NodeConfig argument"))?;
            let config_ty = &self.storage[self.value(config.operand.value)?.0 as usize].ty;
            let expected_fields = vec![
                ResolvedTy::String,
                ResolvedTy::String,
                ResolvedTy::String,
                ResolvedTy::String,
                ResolvedTy::named_builtin(BuiltinType::Vec, vec![ResolvedTy::String]),
                ResolvedTy::named_builtin(BuiltinType::Vec, vec![ResolvedTy::String]),
            ];
            let shape = self
                .module
                .aggregate_shapes
                .iter()
                .find(|shape| shape.aggregate_ty == *config_ty);
            if !shape.is_some_and(|shape| {
                shape
                    .fields
                    .iter()
                    .map(|field| field.ty.clone())
                    .collect::<Vec<_>>()
                    == expected_fields
            }) {
                return Err(PhysicalError::new(
                    "Node::start requires NodeConfig ABI fields bind, transport, key, trust, peers, seeds in source order",
                ));
            }
        }
        let CallResult::Value(value) = result else {
            return Err(PhysicalError::new(
                "node lifecycle operation has no Result value",
            ));
        };
        let error_ty = if family == RuntimeCallFamily::NodeLookup {
            match &value.ty {
                ResolvedTy::Named {
                    head: hew_types::TypeHead::Builtin(BuiltinType::Result),
                    args,
                    ..
                } if args.len() == 2
                    && args[0].is_builtin(BuiltinType::RemotePid)
                    && args[1].is_builtin(BuiltinType::LookupError) =>
                {
                    &args[1]
                }
                _ => {
                    return Err(PhysicalError::new(
                        "Node::lookup result is not Result<RemotePid<T>, LookupError>",
                    ))
                }
            }
        } else {
            match &value.ty {
                ResolvedTy::Named { args, .. } if args.len() == 2 => &args[1],
                _ => {
                    return Err(PhysicalError::new(
                        "node lifecycle result is not Result<(), NodeError>",
                    ))
                }
            }
        };
        Ok(PhysicalRuntimeCarrier::NodeResult {
            result: self.variant_id(&value.ty)?,
            error: self.variant_id(error_ty)?,
        })
    }

    pub(crate) fn aggregate_id(
        &self,
        ty: &ResolvedTy,
    ) -> Result<PhysicalAggregateId, PhysicalError> {
        self.glue_ids.aggregates.get(ty).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "aggregate `{}` has no physical glue identity",
                ty.user_facing()
            ))
        })
    }

    pub(crate) fn variant_id(&self, ty: &ResolvedTy) -> Result<PhysicalVariantId, PhysicalError> {
        self.glue_ids.variants.get(ty).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "variant `{}` has no physical glue identity",
                ty.user_facing()
            ))
        })
    }
}
