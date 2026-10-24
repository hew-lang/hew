//! Integer ranges are ordinary bit-copy aggregates; all loops consume this
//! same normalized finite-sequence representation.

use hew_hir::HirRangeAdapter;
use hew_parser::ast::BinaryOp;
use hew_types::range_value::RangeField;

use super::{
    BlockArg, Builder, Edge, HirExpr, Operand, OwnKind, Provenance, ResolvedTy, SemOpKind,
    SemTerminator, SirLoweringError, ValueId,
};

impl Builder<'_, '_> {
    pub(super) fn range_fields(
        &mut self,
        expr: &HirExpr,
        aggregate: &Operand,
    ) -> Result<[Operand; 7], SirLoweringError> {
        let ty = self.ty(&expr.ty);
        let fields = hew_types::range_value::fields(&ty)
            .ok_or_else(|| SirLoweringError::from("range value lacks its checked element type"))?;
        let shape = self.service.require_aggregate_shape(&ty)?;
        let mut values = Vec::with_capacity(fields.len());
        for (index, (_, field_ty)) in fields.iter().enumerate() {
            values.push(Operand {
                value: self.emit_typed(
                    Provenance::Site(expr.site),
                    field_ty,
                    SemOpKind::AggregateProjectCopy {
                        shape,
                        aggregate: aggregate.clone(),
                        field: u32::try_from(index).unwrap(),
                    },
                )?,
            });
        }
        values
            .try_into()
            .map_err(|_| SirLoweringError::from("range storage contract has incorrect arity"))
    }

    fn make_range_fields(
        &mut self,
        expr: &HirExpr,
        fields: [Operand; 7],
    ) -> Result<ValueId, SirLoweringError> {
        let shape = self.service.require_aggregate_shape(&self.ty(&expr.ty))?;
        self.emit(
            expr,
            SemOpKind::AggregateMake {
                shape,
                fields: fields.into(),
            },
        )
    }

    pub(super) fn lower_range_value(
        &mut self,
        expr: &HirExpr,
        start: &HirExpr,
        end: &HirExpr,
        inclusive: bool,
    ) -> Result<ValueId, SirLoweringError> {
        let ty = self.ty(&expr.ty);
        let ResolvedTy::Named { args, .. } = ty else {
            return Err("range has no element type".into());
        };
        let [element] = args.as_slice() else {
            return Err("range has incorrect arity".into());
        };
        if !element.is_integer() {
            return Err("range requires an integer element".into());
        }
        let start = self.lower_range_operand(start, element)?;
        let end = self.lower_range_operand(end, element)?;
        let nonempty = self.range_compare(
            if inclusive {
                BinaryOp::LessEqual
            } else {
                BinaryOp::Less
            },
            start.clone(),
            end.clone(),
        )?;
        let last = if inclusive {
            end.clone()
        } else {
            let one = Operand {
                value: self.emit_typed(
                    Provenance::Synthesized,
                    element,
                    SemOpKind::ConstInteger(1),
                )?,
            };
            // Empty MIN..MIN may wrap this unused endpoint, but never faults or
            // enters a loop. Nonempty ranges have a representable end - 1.
            Operand {
                value: self.emit_typed(
                    Provenance::Synthesized,
                    element,
                    SemOpKind::Binary {
                        op: BinaryOp::WrappingSub,
                        lhs: end.clone(),
                        rhs: one,
                    },
                )?,
            }
        };
        let stride = Operand {
            value: self.emit_typed(
                Provenance::Synthesized,
                &ResolvedTy::U64,
                SemOpKind::ConstInteger(1),
            )?,
        };
        let descending = Operand {
            value: self.emit_typed(
                Provenance::Synthesized,
                &ResolvedTy::Bool,
                SemOpKind::ConstBool(false),
            )?,
        };
        self.make_range_fields(
            expr,
            [
                start.clone(),
                end,
                start,
                last,
                stride,
                descending,
                nonempty,
            ],
        )
    }

    pub(super) fn lower_range_adapter(
        &mut self,
        expr: &HirExpr,
        range: &HirExpr,
        adapter: &HirRangeAdapter,
    ) -> Result<ValueId, SirLoweringError> {
        let aggregate = self.lower_read_operand(range, "range adapter receiver")?;
        let mut fields = self.range_fields(range, &aggregate)?;
        match adapter {
            HirRangeAdapter::Reverse => {
                fields.swap(RangeField::First as usize, RangeField::Last as usize);
                fields[RangeField::Descending as usize] = Operand {
                    value: self.emit_typed(
                        Provenance::Site(expr.site),
                        &ResolvedTy::Bool,
                        SemOpKind::Unary {
                            op: hew_parser::ast::UnaryOp::Not,
                            value: fields[RangeField::Descending as usize].clone(),
                        },
                    )?,
                };
            }
            HirRangeAdapter::Step(step) => {
                let ty = self.ty(&step.ty);
                let step_value = self.lower_range_operand(step, &ty)?;
                self.require_positive_range_step(step, &ty, step_value.clone())?;
                let step_value = self.range_cast(step_value, &ResolvedTy::U64)?;
                let first =
                    self.range_cast(fields[RangeField::First as usize].clone(), &ResolvedTy::U64)?;
                let last =
                    self.range_cast(fields[RangeField::Last as usize].clone(), &ResolvedTy::U64)?;
                let descending = fields[RangeField::Descending as usize].clone();
                let up_distance = self.range_integer_op(
                    expr,
                    BinaryOp::WrappingSub,
                    last.clone(),
                    first.clone(),
                )?;
                let down_distance =
                    self.range_integer_op(expr, BinaryOp::WrappingSub, first.clone(), last)?;
                let distance = self.range_select(
                    descending.clone(),
                    &ResolvedTy::U64,
                    down_distance,
                    up_distance,
                )?;
                let stride = fields[RangeField::Stride as usize].clone();
                let gaps =
                    self.range_integer_op(expr, BinaryOp::Divide, distance, stride.clone())?;
                let gaps =
                    self.range_integer_op(expr, BinaryOp::Divide, gaps, step_value.clone())?;
                let product =
                    self.range_integer_op(expr, BinaryOp::WrappingMul, stride, step_value)?;
                let offset =
                    self.range_integer_op(expr, BinaryOp::WrappingMul, gaps, product.clone())?;
                let up_endpoint = self.range_integer_op(
                    expr,
                    BinaryOp::WrappingAdd,
                    first.clone(),
                    offset.clone(),
                )?;
                let down_endpoint =
                    self.range_integer_op(expr, BinaryOp::WrappingSub, first, offset)?;
                let endpoint =
                    self.range_select(descending, &ResolvedTy::U64, down_endpoint, up_endpoint)?;
                let range_ty = self.ty(&range.ty);
                let ResolvedTy::Named { args, .. } = range_ty else {
                    return Err("range adapter lacks its element type".into());
                };
                fields[RangeField::Last as usize] = self.range_cast(endpoint, &args[0])?;
                // A wrapped stride product can only select a singleton. Keep
                // subsequent adapters' divisor nonzero for that singleton.
                fields[RangeField::Stride as usize] = self.nonzero_range_stride(product)?;
            }
        }
        self.make_range_fields(expr, fields)
    }

    pub(super) fn range_select(
        &mut self,
        condition: Operand,
        ty: &ResolvedTy,
        yes: Operand,
        no: Operand,
    ) -> Result<Operand, SirLoweringError> {
        let value = self.fresh_value();
        let join = self.new_block(vec![BlockArg {
            value,
            ty: ty.clone(),
            own: OwnKind::None,
        }]);
        let otherwise = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: join,
                args: vec![yes],
            },
            else_target: Edge {
                target: otherwise,
                args: Vec::new(),
            },
        })?;
        self.current = otherwise;
        self.set_terminator(SemTerminator::Goto(Edge {
            target: join,
            args: vec![no],
        }))?;
        self.current = join;
        Ok(Operand { value })
    }
}
