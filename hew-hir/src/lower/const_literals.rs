use super::*;
use hew_types::compile_time::{ConstantValue, TypedConstant};

impl LowerCtx {
    fn fold_typed_const(&mut self, expr: &Spanned<Expr>, ty: &ResolvedTy) -> TypedConstant {
        TypedConstant {
            ty: ty.clone(),
            value: self.fold_const_expr(&expr.0, ty, expr.1.clone()),
        }
    }

    pub(super) fn fold_aggregate_const_expr(
        &mut self,
        expr: &Expr,
        ty: &ResolvedTy,
        span: &Span,
    ) -> Option<ConstantValue> {
        if let Some(value) = self.fold_variant_const(expr, ty, span) {
            return Some(value);
        }
        match expr {
            Expr::Tuple(elements) => {
                let ResolvedTy::Tuple(types) = ty else {
                    return (elements.is_empty() && *ty == ResolvedTy::Unit)
                        .then_some(ConstantValue::Unit);
                };
                Some(ConstantValue::Tuple(
                    elements
                        .iter()
                        .zip(types)
                        .map(|(value, ty)| self.fold_typed_const(value, ty))
                        .collect(),
                ))
            }
            Expr::Array(elements) => self.fold_const_array(elements, ty),
            Expr::ArrayRepeat { value, count } => {
                let ResolvedTy::Array(element, length) = ty else {
                    return None;
                };
                let count_ty = self.checker_expr_ty(&count.1, "constant repeat count")?;
                let ConstantValue::Integer(count) = self.fold_typed_const(count, &count_ty).value
                else {
                    return None;
                };
                if u64::try_from(count).ok() != Some(*length) {
                    return None;
                }
                let value = self.fold_typed_const(value, element);
                Some(ConstantValue::Array(vec![
                    value;
                    usize::try_from(*length).ok()?
                ]))
            }
            Expr::MapLiteral { entries } => {
                let (BuiltinType::HashMap, args) =
                    hew_types::runtime_call::collection_type_arguments(ty)?
                else {
                    return None;
                };
                let (key_ty, value_ty) = (args[0].clone(), args[1].clone());
                Some(ConstantValue::Map(
                    entries
                        .iter()
                        .map(|(key, value)| {
                            (
                                self.fold_typed_const(key, &key_ty),
                                self.fold_typed_const(value, &value_ty),
                            )
                        })
                        .collect(),
                ))
            }
            Expr::StructInit { fields, base, .. } => {
                let (_, declared) = self.display_facts.record_fields(ty).ok()?;
                self.fold_named_const_fields(&declared, fields, base.as_deref(), ty)
                    .map(ConstantValue::Record)
            }
            Expr::Block(block) if block.stmts.is_empty() => {
                if let Some(value) = &block.trailing_expr {
                    Some(self.fold_typed_const(value, ty).value)
                } else if *ty == ResolvedTy::Unit {
                    Some(ConstantValue::Unit)
                } else if matches!(
                    hew_types::runtime_call::collection_type_arguments(ty),
                    Some((BuiltinType::HashMap, _))
                ) {
                    Some(ConstantValue::Map(Vec::new()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn fold_const_array(
        &mut self,
        elements: &[ArrayElement],
        ty: &ResolvedTy,
    ) -> Option<ConstantValue> {
        let (element_ty, fixed) = match ty {
            ResolvedTy::Array(element, _) => ((**element).clone(), true),
            _ => {
                let (BuiltinType::Vec, args) =
                    hew_types::runtime_call::collection_type_arguments(ty)?
                else {
                    return None;
                };
                (args[0].clone(), false)
            }
        };
        let mut values = Vec::new();
        for element in elements {
            match element {
                ArrayElement::Value(value) => {
                    values.push(self.fold_typed_const(value, &element_ty))
                }
                ArrayElement::Spread(value) => {
                    let spread_ty = self.checker_expr_ty(&value.1, "constant spread")?;
                    match self.fold_typed_const(value, &spread_ty).value {
                        ConstantValue::Array(elements) | ConstantValue::Vector(elements) => {
                            values.extend(elements)
                        }
                        _ => return None,
                    }
                }
            }
        }
        Some(if fixed {
            ConstantValue::Array(values)
        } else {
            ConstantValue::Vector(values)
        })
    }

    fn fold_named_const_fields(
        &mut self,
        declared: &[(String, ResolvedTy)],
        fields: &[(hew_parser::ast::Ident, Spanned<Expr>)],
        base: Option<&Spanned<Expr>>,
        ty: &ResolvedTy,
    ) -> Option<Vec<(String, TypedConstant)>> {
        let inherited = match base {
            Some(base) => match self.fold_typed_const(base, ty).value {
                ConstantValue::Record(fields) | ConstantValue::Variant { fields, .. } => fields,
                _ => return None,
            },
            None => Vec::new(),
        };
        declared
            .iter()
            .map(|(name, field_ty)| {
                let value = if let Some((_, value)) =
                    fields.iter().find(|(field, _)| field.name.as_str() == name)
                {
                    self.fold_typed_const(value, field_ty)
                } else {
                    inherited.iter().find(|(field, _)| field == name)?.1.clone()
                };
                Some((name.clone(), value))
            })
            .collect()
    }

    fn fold_variant_const(
        &mut self,
        expr: &Expr,
        ty: &ResolvedTy,
        span: &Span,
    ) -> Option<ConstantValue> {
        let (target, target_span) = match expr {
            Expr::Call { function, .. } => (&function.0, &function.1),
            _ => (expr, span),
        };
        let variants = self.display_facts.data_variants(ty).ok()?;
        let ResolvedTy::Named { head, .. } = ty else {
            return None;
        };
        let resolution_span = match target {
            Expr::StructInit { path, .. } => &path.segments.last()?.1,
            Expr::FieldAccess { field, .. } => &field.1,
            Expr::MethodCall { method, .. } => &method.1,
            Expr::GenericApplySuffix { target, .. } => &target.1,
            _ => target_span,
        };
        let index = match self
            .resolutions
            .get(&self.mk_key(resolution_span))
            .or_else(|| self.resolutions.get(&self.mk_key(span)))
        {
            Some(Resolution::Variant(owner, index)) if owner == head => {
                usize::try_from(*index).ok()?
            }
            _ => return None,
        };
        let variant = variants.get(index)?;
        let fields = match expr {
            Expr::Call { args, .. } | Expr::MethodCall { args, .. } => {
                if args.len() != variant.fields.len() {
                    return None;
                }
                variant
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(index, (name, ty))| {
                        let arg = args
                            .iter()
                            .find(|arg| arg.name().is_some_and(|label| label.name.as_str() == name))
                            .or_else(|| {
                                args.get(index)
                                    .filter(|arg| matches!(arg, CallArg::Positional(_)))
                            })?;
                        Some((name.clone(), self.fold_typed_const(arg.expr(), ty)))
                    })
                    .collect::<Option<Vec<_>>>()?
            }
            Expr::StructInit { fields, base, .. } => {
                self.fold_named_const_fields(&variant.fields, fields, base.as_deref(), ty)?
            }
            Expr::ContextVariant(context) => match &context.record {
                Some(record) => self.fold_named_const_fields(
                    &variant.fields,
                    &record.fields,
                    record.base.as_deref(),
                    ty,
                )?,
                None if variant.fields.is_empty() => Vec::new(),
                None => return None,
            },
            _ if variant.fields.is_empty() => Vec::new(),
            _ => return None,
        };
        Some(ConstantValue::Variant { index, fields })
    }
}
