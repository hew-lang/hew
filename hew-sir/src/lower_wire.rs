//! One checked semantic plan supplies every wire codec direction.

use std::sync::Arc;

use heck::{ToKebabCase, ToLowerCamelCase, ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase};
use hew_parser::ast::NamingCase;
use hew_types::{BuiltinType, ResolvedTy, WireCodecDirection};

use super::{Builder, InstanceService};
use crate::{
    AggregateShapeRef, BlockArg, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind, Edge,
    OpId, Operand, OwnKind, SemTerminator, SemWireField, SemWireKind, SemWirePlan, SemWireVariant,
    ValueDef, ValueId,
};

fn wire_name(name: &str, case: Option<NamingCase>) -> String {
    match case {
        None => name.to_owned(),
        Some(NamingCase::CamelCase) => name.to_lower_camel_case(),
        Some(NamingCase::PascalCase) => name.to_upper_camel_case(),
        Some(NamingCase::SnakeCase) => name.to_snake_case(),
        Some(NamingCase::ScreamingSnake) => name.to_shouty_snake_case(),
        Some(NamingCase::KebabCase) => name.to_kebab_case(),
    }
}

impl InstanceService<'_> {
    #[expect(
        clippy::too_many_lines,
        reason = "the exhaustive schema walk keeps every admitted value shape together"
    )]
    fn wire_plan(
        &mut self,
        ty: &ResolvedTy,
        path: &mut Vec<ResolvedTy>,
    ) -> Result<Arc<SemWirePlan>, String> {
        if let Some(plan) = self.wire_plans.get(ty) {
            return Ok(Arc::clone(plan));
        }
        if path.contains(ty) || path.len() >= 128 {
            return Err(format!(
                "wire schema for `{}` exceeds the finite codec depth",
                ty.user_facing()
            ));
        }
        self.require_type_facts(ty)?;
        path.push(ty.clone());
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
            ResolvedTy::Named {
                builtin: Some(builtin),
                args,
                ..
            } => match (builtin, args.as_slice()) {
                (BuiltinType::Vec, [element]) => {
                    SemWireKind::Vector(self.wire_plan(element, path)?)
                }
                (BuiltinType::HashSet, [element]) => {
                    self.require_key_capabilities(element)?;
                    SemWireKind::Set(self.wire_plan(element, path)?)
                }
                (BuiltinType::HashMap, [key, value]) => {
                    self.require_key_capabilities(key)?;
                    SemWireKind::Map {
                        key: self.wire_plan(key, path)?,
                        value: self.wire_plan(value, path)?,
                    }
                }
                (BuiltinType::Option, [value]) => {
                    if matches!(
                        value,
                        ResolvedTy::Named {
                            builtin: Some(BuiltinType::Option),
                            ..
                        }
                    ) {
                        return Err("Option<Option<_>> is ambiguous under the null encoding".into());
                    }
                    let shape = self.require_variant_shape(ty)?;
                    let variants = &self.variant_shapes[shape.0 as usize].variants;
                    let none = variants
                        .iter()
                        .position(|v| v.fields.is_empty())
                        .ok_or("wire Option has no empty case")?;
                    let some = variants
                        .iter()
                        .position(|v| v.fields.len() == 1 && v.fields[0].ty == *value)
                        .ok_or("wire Option has no exact payload case")?;
                    SemWireKind::Option {
                        shape,
                        none: u32::try_from(none).map_err(|_| "Option index exceeds u32")?,
                        some: u32::try_from(some).map_err(|_| "Option index exceeds u32")?,
                        value: self.wire_plan(value, path)?,
                    }
                }
                _ => {
                    return Err(format!(
                        "`{}` is outside the checked wire value surface",
                        ty.user_facing()
                    ))
                }
            },
            ResolvedTy::Named {
                name,
                builtin: None,
                is_opaque: false,
                ..
            } => {
                let layout = self
                    .module
                    .wire_layouts
                    .get(name)
                    .cloned()
                    .ok_or_else(|| format!("`{name}` has no checked wire layout"))?;
                if layout.is_struct {
                    let AggregateShapeRef::Record(shape) = self.require_aggregate_shape(ty)? else {
                        return Err("wire record has no exact aggregate identity".into());
                    };
                    let fields = self.aggregate_shapes[shape.0 as usize].fields.clone();
                    if fields.len() != layout.fields.len() {
                        return Err("wire and record field inventories disagree".into());
                    }
                    let mut plans = Vec::with_capacity(fields.len());
                    for (index, field) in fields.iter().enumerate() {
                        let wire = layout
                            .fields
                            .iter()
                            .find(|wire| wire.name == field.name)
                            .ok_or("wire record field has no checked tag")?;
                        plans.push(SemWireField {
                            index: u32::try_from(index)
                                .map_err(|_| "wire field index exceeds u32")?,
                            tag: wire.tag,
                            json_name: wire
                                .json_name
                                .clone()
                                .unwrap_or_else(|| wire_name(&field.name, layout.json_case)),
                            yaml_name: wire
                                .yaml_name
                                .clone()
                                .unwrap_or_else(|| wire_name(&field.name, layout.yaml_case)),
                            presence: wire.presence,
                            value: self.wire_plan(&field.ty, path)?,
                        });
                    }
                    plans.sort_by_key(|field| field.tag);
                    SemWireKind::Record {
                        shape,
                        fields: plans,
                    }
                } else {
                    let shape = self.require_variant_shape(ty)?;
                    let variants = self.variant_shapes[shape.0 as usize].variants.clone();
                    let mut plans = Vec::with_capacity(variants.len());
                    for (index, variant) in variants.iter().enumerate() {
                        let (_, tag) = layout
                            .variants
                            .iter()
                            .find(|(name, _)| *name == variant.name)
                            .ok_or("wire enum case has no checked tag")?;
                        plans.push(SemWireVariant {
                            index: u32::try_from(index)
                                .map_err(|_| "wire variant index exceeds u32")?,
                            tag: *tag,
                            json_name: wire_name(&variant.name, layout.json_case),
                            yaml_name: wire_name(&variant.name, layout.yaml_case),
                            fields: variant
                                .fields
                                .iter()
                                .map(|field| self.wire_plan(&field.ty, path))
                                .collect::<Result<_, _>>()?,
                        });
                    }
                    SemWireKind::Enum {
                        shape,
                        variants: plans,
                    }
                }
            }
            _ => {
                return Err(format!(
                    "`{}` is outside the checked wire value surface",
                    ty.user_facing()
                ))
            }
        };
        path.pop();
        let plan = Arc::new(SemWirePlan {
            ty: ty.clone(),
            kind,
        });
        self.wire_plans.insert(ty.clone(), Arc::clone(&plan));
        Ok(plan)
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
        let plan = self.service.wire_plan(&value_ty, &mut Vec::new())?;
        if let Some(format) = direction.text_format() {
            plan.verify_text_names(format)?;
        }
        let result_ty = self.ty(&expr.ty);
        self.service.require_type_facts(&result_ty)?;
        let text_result = if matches!(
            direction,
            WireCodecDirection::FromJson | WireCodecDirection::FromYaml
        ) {
            let shape = self.service.require_variant_shape(&result_ty)?;
            let variants = &self.service.variant_shapes[shape.0 as usize].variants;
            let case = |name: &str, ty: &ResolvedTy| -> Result<u32, String> {
                let index = variants
                    .iter()
                    .position(|variant| {
                        variant.name == name
                            && variant.fields.len() == 1
                            && variant.fields[0].ty == *ty
                    })
                    .ok_or_else(|| format!("wire text result lacks its exact `{name}` case"))?;
                u32::try_from(index).map_err(|_| "wire Result index exceeds u32".into())
            };
            Some(crate::SemWireTextResult {
                shape,
                ok: case("Ok", &value_ty)?,
                error: case("Err", &ResolvedTy::String)?,
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
