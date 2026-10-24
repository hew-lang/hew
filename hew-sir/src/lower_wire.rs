//! One checked semantic plan supplies every wire codec direction.

use std::collections::BTreeMap;
use std::sync::Arc;

use hew_types::{Codec, ResolvedTy};

use crate::SirLoweringError;

use super::{Builder, InstanceService};
use crate::{
    AggregateShapeRef, BlockArg, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind, Edge,
    OpId, Operand, OwnKind, SemTerminator, SemWireKind, SemWireMember, SemWirePayload, SemWirePlan,
    SemWirePlans, SemWireTable, ValueDef, ValueId,
};

impl InstanceService<'_> {
    /// The plans of every type a codec of `root` walks. The checker admitted
    /// the type (`TypeFactService::data_error`); a shape this cannot plan is a
    /// broken stage contract.
    pub(super) fn wire_plans(
        &mut self,
        root: &ResolvedTy,
    ) -> Result<Arc<SemWirePlans>, SirLoweringError> {
        if let Some(plans) = self.wire_plans.get(root) {
            return Ok(Arc::clone(plans));
        }
        let plans = Arc::new(self.data_storage_plans(root, false)?);
        self.wire_plans.insert(root.clone(), Arc::clone(&plans));
        Ok(plans)
    }

    fn data_storage_plans(
        &mut self,
        root: &ResolvedTy,
        property: bool,
    ) -> Result<SemWirePlans, SirLoweringError> {
        let data = self.checked_facts.data_plans(root, property)?;
        let mut plans = BTreeMap::new();
        for (ty, plan) in data.plans {
            self.require_type_facts(&ty)?;
            let kind = self.bind_data_storage(&ty, plan.kind)?;
            plans.insert(ty.clone(), SemWirePlan { ty, kind });
        }
        Ok(SemWirePlans {
            property_input: false,
            root: root.clone(),
            plans,
        })
    }

    /// Generation uses the same layouts, retaining fields and every Option state.
    pub(super) fn property_plans(
        &mut self,
        root: &ResolvedTy,
    ) -> Result<Arc<SemWirePlans>, SirLoweringError> {
        let mut plans = self.data_storage_plans(root, true)?;
        plans.property_input = true;
        let preserve_fields = |table: &mut SemWireTable| {
            for member in &mut table.members {
                member.flags &= !(hew_codec::Member::SKIP
                    | hew_codec::Member::ACCEPT_ABSENT
                    | hew_codec::Member::OMIT_NULL);
            }
        };
        for plan in plans.plans.values_mut() {
            match &mut plan.kind {
                SemWireKind::Option {
                    shape,
                    none,
                    some,
                    value,
                } => {
                    let mut members = vec![
                        SemWireMember {
                            key: String::new(),
                            tag: 0,
                            flags: 0
                        };
                        2
                    ];
                    members[*none as usize].key = "None".into();
                    members[*some as usize] = SemWireMember {
                        key: "Some".into(),
                        tag: 0,
                        flags: hew_codec::Member::PAYLOAD,
                    };
                    let mut variants = vec![SemWirePayload::Unit; 2];
                    variants[*some as usize] = SemWirePayload::Single(value.clone());
                    plan.kind = SemWireKind::Enum {
                        shape: *shape,
                        table: SemWireTable {
                            tagged: false,
                            members,
                        },
                        variants,
                    };
                }
                SemWireKind::Record {
                    table: Some(table), ..
                } => preserve_fields(table),
                SemWireKind::Enum {
                    table, variants, ..
                } => {
                    preserve_fields(table);
                    for variant in variants {
                        if let SemWirePayload::Record { table, .. } = variant {
                            preserve_fields(table);
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(Arc::new(plans))
    }

    fn bind_data_storage(
        &mut self,
        ty: &ResolvedTy,
        kind: hew_types::data_plan::DataKind<(), ()>,
    ) -> Result<SemWireKind, SirLoweringError> {
        use hew_types::data_plan::DataKind;
        Ok(match kind {
            DataKind::Scalar => SemWireKind::Scalar,
            DataKind::Unit => SemWireKind::Unit,
            DataKind::Tuple(fields) => {
                self.require_aggregate_shape(ty)?;
                SemWireKind::Tuple(fields)
            }
            DataKind::Array { element, len } => SemWireKind::Array { element, len },
            DataKind::Vector(element) => SemWireKind::Vector(element),
            DataKind::Set(element) => {
                self.require_key_capabilities(&element)?;
                SemWireKind::Set(element)
            }
            DataKind::Map { key, value } => {
                self.require_key_capabilities(&key)?;
                SemWireKind::Map { key, value }
            }
            DataKind::Option {
                none, some, value, ..
            } => SemWireKind::Option {
                shape: self.require_variant_shape(ty)?,
                none,
                some,
                value,
            },
            DataKind::Timestamp {
                fields,
                nanoseconds_max,
                ..
            } => {
                let AggregateShapeRef::Record(shape) = self.require_aggregate_shape(ty)? else {
                    return Err("timestamp data has no aggregate storage".into());
                };
                SemWireKind::Timestamp {
                    shape,
                    fields,
                    nanoseconds_max,
                }
            }
            DataKind::Record { table, fields, .. } => {
                let AggregateShapeRef::Record(shape) = self.require_aggregate_shape(ty)? else {
                    return Err("checked data record has no record storage".into());
                };
                SemWireKind::Record {
                    shape,
                    table,
                    fields,
                }
            }
            DataKind::Enum {
                table, variants, ..
            } => SemWireKind::Enum {
                shape: self.require_variant_shape(ty)?,
                table,
                variants,
            },
        })
    }
}

impl Builder<'_, '_> {
    #[expect(
        clippy::too_many_lines,
        reason = "one codec invocation keeps its result and failure continuations together"
    )]
    pub(super) fn lower_wire_codec(
        &mut self,
        expr: &hew_hir::HirExpr,
        codec: Codec,
        input: &hew_hir::HirExpr,
        value_ty: &ResolvedTy,
    ) -> Result<ValueId, SirLoweringError> {
        let value_ty = self.ty(value_ty);
        let mut plan = self.service.wire_plans(&value_ty)?;
        let result_ty = self.ty(&expr.ty);
        self.service.require_type_facts(&result_ty)?;
        let decode_result = if codec.is_serialize() {
            None
        } else {
            // A failure decodes the reader's error through the plan of the
            // checked `Err` type, `wire.DecodeError`.
            let ResolvedTy::Named { args, .. } = &result_ty else {
                return Err("codec decode result is not a Result".to_string().into());
            };
            let error_ty = args.get(1).cloned().ok_or_else(|| {
                SirLoweringError::from("codec decode result lacks its error type".to_string())
            })?;
            let error_plans = self.service.wire_plans(&error_ty)?;
            let mut merged = (*plan).clone();
            merged.plans.extend(
                error_plans
                    .plans
                    .iter()
                    .map(|(ty, p)| (ty.clone(), p.clone())),
            );
            plan = Arc::new(merged);
            let shape = self.service.require_variant_shape(&result_ty)?;
            let tags = &self.service.variant_shapes[shape.0 as usize].runtime_tags;
            let case = |role: crate::RuntimeVariantRole| -> Result<u32, SirLoweringError> {
                tags.iter()
                    .find(|(found, _)| *found == role)
                    .map(|(_, tag)| *tag)
                    .ok_or_else(|| {
                        SirLoweringError::from(
                            "codec text result is not an exact Result".to_string(),
                        )
                    })
            };
            Some(crate::SemWireDecodeResult {
                shape,
                ok: case(crate::RuntimeVariantRole::ResultOk)?,
                error: case(crate::RuntimeVariantRole::ResultErr)?,
                error_ty,
            })
        };
        let before = self
            .owned_live
            .keys()
            .copied()
            .collect::<std::collections::HashSet<_>>();
        let mut loans = Vec::new();
        let operand = self.lower_call_read(input, &mut loans, true, true)?;
        let live = self.owned_live.clone();
        let own = OwnKind::of_ty(&result_ty, self.service.checked_facts.rows())?;
        let raw = self.fresh_value();
        let value = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value,
            ty: result_ty.clone(),
            own,
        }]);
        let unwind = self.new_block(Vec::new());
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::WireCodec {
            id,
            codec,
            plan,
            decode_result,
            args: vec![BoundaryOperand {
                operand,
                decision: BoundaryDecision::Borrow,
            }],
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: result_ty.clone(),
                own,
            }),
            normal: Edge {
                target: normal,
                args: vec![Operand { value: raw }],
            },
            unwind: CallUnwind::Cleanup(Edge {
                target: unwind,
                args: vec![],
            }),
        })?;
        self.current = unwind;
        self.owned_live = live.clone();
        self.end_call_loans(&loans)?;
        self.finish_fault_exit()?;
        self.current = normal;
        self.owned_live = live;
        self.end_call_loans(&loans)?;
        let temporaries = self
            .owned_live
            .keys()
            .filter(|value| !before.contains(value))
            .copied()
            .collect::<Vec<_>>();
        if own == OwnKind::Owned {
            self.owned_live.insert(value, result_ty);
        }
        for temporary in temporaries.into_iter().rev() {
            self.emit_destroy(temporary)?;
        }
        Ok(value)
    }
}
