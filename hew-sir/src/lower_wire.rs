//! One checked semantic plan supplies every wire codec direction.

use std::collections::BTreeMap;
use std::sync::Arc;

use hew_types::{BuiltinType, ResolvedTy, WireCodecDirection};

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
    pub(super) fn wire_plans(&mut self, root: &ResolvedTy) -> Result<Arc<SemWirePlans>, String> {
        if let Some(plans) = self.wire_plans.get(root) {
            return Ok(Arc::clone(plans));
        }
        let mut plans = BTreeMap::new();
        let mut work = vec![root.clone()];
        while let Some(ty) = work.pop() {
            if plans.contains_key(&ty) {
                continue;
            }
            let plan = self.wire_plan(&ty)?;
            work.extend(plan.children().into_iter().cloned());
            plans.insert(ty, plan);
        }
        let plans = Arc::new(SemWirePlans {
            root: root.clone(),
            plans,
        });
        self.wire_plans.insert(root.clone(), Arc::clone(&plans));
        Ok(plans)
    }

    fn serial_table(
        &self,
        ty: &ResolvedTy,
    ) -> Result<&hew_types::data_shape::SerialLayout, String> {
        ty.nominal_instance(&self.module.defs)
            .and_then(|instance| self.checked_facts.context().serial_layout(instance.nominal))
            .ok_or_else(|| format!("`{}` has no checked serial layout", ty.user_facing()))
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive match keeps every planned shape together"
    )]
    fn wire_plan(&mut self, ty: &ResolvedTy) -> Result<SemWirePlan, String> {
        self.require_type_facts(ty)?;
        let kind = match ty {
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration
            | ResolvedTy::String
            | ResolvedTy::Bytes => SemWireKind::Scalar,
            ResolvedTy::Unit => SemWireKind::Unit,
            ResolvedTy::Tuple(elements) => {
                self.require_aggregate_shape(ty)?;
                SemWireKind::Tuple(elements.clone())
            }
            ResolvedTy::Array(element, len) => SemWireKind::Array {
                element: (**element).clone(),
                len: *len,
            },
            ResolvedTy::Named {
                head: hew_types::TypeHead::Builtin(builtin),
                args,
                ..
            } => match (builtin, args.as_slice()) {
                (BuiltinType::Vec, [element]) => SemWireKind::Vector(element.clone()),
                (BuiltinType::HashSet, [element]) => {
                    self.require_key_capabilities(element)?;
                    SemWireKind::Set(element.clone())
                }
                (BuiltinType::HashMap, [key, value]) => {
                    self.require_key_capabilities(key)?;
                    SemWireKind::Map {
                        key: key.clone(),
                        value: value.clone(),
                    }
                }
                (BuiltinType::Option, [value]) => {
                    let shape = self.require_variant_shape(ty)?;
                    let variants = &self.variant_shapes[shape.0 as usize].variants;
                    let none = variants
                        .iter()
                        .position(|v| v.fields.is_empty())
                        .ok_or("codec Option has no empty case")?;
                    let some = variants
                        .iter()
                        .position(|v| v.fields.len() == 1 && v.fields[0].ty == *value)
                        .ok_or("codec Option has no exact payload case")?;
                    SemWireKind::Option {
                        shape,
                        none: u32::try_from(none).map_err(|_| "Option index exceeds u32")?,
                        some: u32::try_from(some).map_err(|_| "Option index exceeds u32")?,
                        value: value.clone(),
                    }
                }
                (BuiltinType::Result, [_, _]) => {
                    let shape = self.require_variant_shape(ty)?;
                    let variants = self.variant_shapes[shape.0 as usize].variants.clone();
                    let table = SemWireTable {
                        tagged: false,
                        members: variants
                            .iter()
                            .map(|variant| SemWireMember {
                                key: variant.name.clone(),
                                tag: 0,
                                flags: hew_codec::Member::PAYLOAD,
                            })
                            .collect(),
                    };
                    SemWireKind::Enum {
                        shape,
                        table,
                        variants: variants
                            .iter()
                            .map(|variant| payload(variant, None))
                            .collect(),
                    }
                }
                _ => {
                    return Err(format!(
                        "`{}` is outside the checked data shape",
                        ty.user_facing()
                    ))
                }
            },
            ResolvedTy::Named {
                head:
                    hew_types::TypeHead::Nominal(_)
                    | hew_types::TypeHead::Param(_)
                    | hew_types::TypeHead::Unresolved(_),
                is_opaque: false,
                ..
            } => {
                let layout = self.serial_table(ty)?.clone();
                if let Ok(AggregateShapeRef::Record(shape)) = self.require_aggregate_shape(ty) {
                    let fields = self.aggregate_shapes[shape.0 as usize]
                        .fields
                        .iter()
                        .map(|field| field.ty.clone())
                        .collect::<Vec<_>>();
                    let table = (!layout.positional).then(|| table(layout.tagged, &layout.members));
                    if table
                        .as_ref()
                        .is_some_and(|table| table.members.len() != fields.len())
                    {
                        return Err("record fields and serial layout disagree".into());
                    }
                    SemWireKind::Record {
                        shape,
                        table,
                        fields,
                    }
                } else {
                    let shape = self.require_variant_shape(ty)?;
                    let variants = self.variant_shapes[shape.0 as usize].variants.clone();
                    if variants.len() != layout.members.len() {
                        return Err("enum variants and serial layout disagree".into());
                    }
                    SemWireKind::Enum {
                        shape,
                        table: table(layout.tagged, &layout.members),
                        variants: variants
                            .iter()
                            .zip(&layout.members)
                            .map(|(variant, member)| payload(variant, Some(member)))
                            .collect(),
                    }
                }
            }
            _ => {
                return Err(format!(
                    "`{}` is outside the checked data shape",
                    ty.user_facing()
                ))
            }
        };
        Ok(SemWirePlan {
            ty: ty.clone(),
            kind,
        })
    }
}

fn table(tagged: bool, members: &[hew_types::data_shape::SerialMember]) -> SemWireTable {
    SemWireTable {
        tagged,
        members: members
            .iter()
            .map(|member| SemWireMember {
                key: member.key.clone(),
                tag: member.tag,
                flags: member.flags,
            })
            .collect(),
    }
}

/// A variant's payload: one value directly, several as a sequence, named
/// fields as a record keyed by their text keys.
fn payload(
    variant: &crate::SemVariant,
    member: Option<&hew_types::data_shape::SerialMember>,
) -> SemWirePayload {
    let types = variant
        .fields
        .iter()
        .map(|field| field.ty.clone())
        .collect::<Vec<_>>();
    match (variant.kind, types.as_slice()) {
        (_, []) => SemWirePayload::Unit,
        (crate::SemVariantKind::Struct, _) => SemWirePayload::Record {
            table: member.map_or_else(
                || SemWireTable {
                    tagged: false,
                    members: Vec::new(),
                },
                |member| table(false, &member.fields),
            ),
            fields: types,
        },
        (_, [single]) => SemWirePayload::Single(single.clone()),
        _ => SemWirePayload::Tuple(types),
    }
}

impl Builder<'_, '_> {
    pub(super) fn lower_wire_codec(
        &mut self,
        expr: &hew_hir::HirExpr,
        direction: WireCodecDirection,
        input: &hew_hir::HirExpr,
        value_ty: &ResolvedTy,
    ) -> Result<ValueId, String> {
        let value_ty = self.ty(value_ty);
        let plan = self.service.wire_plans(&value_ty)?;
        let result_ty = self.ty(&expr.ty);
        self.service.require_type_facts(&result_ty)?;
        let text_result = if matches!(
            direction,
            WireCodecDirection::FromJson | WireCodecDirection::FromYaml
        ) {
            let shape = self.service.require_variant_shape(&result_ty)?;
            let tags = &self.service.variant_shapes[shape.0 as usize].runtime_tags;
            let case = |role: crate::RuntimeVariantRole| -> Result<u32, String> {
                tags.iter()
                    .find(|(found, _)| *found == role)
                    .map(|(_, tag)| *tag)
                    .ok_or_else(|| "codec text result is not an exact Result".to_string())
            };
            Some(crate::SemWireTextResult {
                shape,
                ok: case(crate::RuntimeVariantRole::ResultOk)?,
                error: case(crate::RuntimeVariantRole::ResultErr)?,
            })
        } else {
            None
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
            direction,
            plan,
            text_result,
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
