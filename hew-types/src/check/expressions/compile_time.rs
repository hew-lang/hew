use super::super::{Checker, Expr, Literal, ResolvedTy, Span, SpanKey, Spanned, Ty, TypeErrorKind};
use crate::compile_time::{ConstantValue, InputFile, InputProvider, TypedConstant};
use std::sync::Arc;

impl Checker {
    pub fn set_input_provider(&mut self, provider: Arc<dyn InputProvider>) {
        self.module_registry
            .set_input_provider(Arc::clone(&provider));
        self.input_provider = Some(provider);
    }

    pub(in crate::check) fn check_macro_invocation(
        &mut self,
        name: &str,
        args: &[Spanned<Expr>],
        span: &Span,
    ) -> Ty {
        let result = (|| {
            if name == "location" {
                if !args.is_empty() {
                    return Err(("E_MACRO_ARGUMENTS", "location! expects no arguments".into()));
                }
                return self.source_location_constant(span);
            }
            if name == "stringify" {
                let [arg] = args else {
                    return Err((
                        "E_MACRO_ARGUMENTS",
                        "stringify! expects one expression".into(),
                    ));
                };
                return Ok((
                    ConstantValue::String(hew_parser::fmt::format_expression(arg)),
                    Ty::String,
                ));
            }
            if name == "embed" {
                return Err((
                    "E_EMBED_NEEDS_TYPE",
                    "embed! needs a concrete expected type; annotate the binding or constant"
                        .into(),
                ));
            }
            if !matches!(name, "include_str" | "include_bytes") {
                return Err(("E_UNKNOWN_MACRO", format!("unknown macro `{name}!`")));
            }
            let file = self.compile_time_input(name, args)?;
            if name == "include_bytes" {
                return Ok((ConstantValue::Bytes(file.bytes), Ty::Bytes));
            }
            let text = String::from_utf8(file.bytes).map_err(|error| {
                (
                    "E_INCLUDE_NOT_UTF8",
                    format!(
                        "{} is not UTF-8 at byte {}",
                        file.display_path,
                        error.utf8_error().valid_up_to()
                    ),
                )
            })?;
            Ok((ConstantValue::String(text), Ty::String))
        })();
        match result {
            Ok((value, ty)) => {
                self.compile_time_values
                    .insert(SpanKey::in_module(span, self.current_module_idx), value);
                ty
            }
            Err((code, message)) => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!("{code}: {message}"),
                );
                Ty::Error
            }
        }
    }

    fn compile_time_input(
        &mut self,
        name: &str,
        args: &[Spanned<Expr>],
    ) -> Result<InputFile, (&'static str, String)> {
        let [(Expr::Literal(Literal::String(path)), _)] = args else {
            return Err((
                "E_MACRO_ARGUMENTS",
                format!("{name}! expects one string literal path"),
            ));
        };
        let provider = self.input_provider.as_ref().ok_or_else(|| {
            (
                "E_INCLUDE_NOT_FOUND",
                "compilation inputs are unavailable".into(),
            )
        })?;
        let source = self
            .current_item_source
            .as_deref()
            .unwrap_or_else(|| provider.root_source());
        let file = provider
            .include(source, path)
            .map_err(|error| (error.code, error.message))?;
        self.input_references.insert(
            SpanKey::in_module(&args[0].1, self.current_module_idx),
            crate::compile_time::InputReference {
                path: file.path.clone(),
                display_path: file.display_path.clone(),
                size: file.bytes.len(),
            },
        );
        Ok(file)
    }

    pub(super) fn check_embed(&mut self, args: &[Spanned<Expr>], span: &Span, expected: &Ty) -> Ty {
        let result = (|| {
            let ty = ResolvedTy::from_ty(expected).map_err(|_| {
                (
                    "E_EMBED_NEEDS_TYPE",
                    "embed! needs a concrete expected type".into(),
                )
            })?;
            let mut facts = crate::TypeFactService::new(
                self.type_fact_context(),
                std::collections::BTreeMap::new(),
            );
            let plans = facts
                .data_plans(&ty, false)
                .map_err(|reason| ("E_EMBED_TYPE", reason))?;
            check_embed_keys(&mut facts, &plans).map_err(|reason| ("E_EMBED_KEY", reason))?;
            let shape = crate::data_plan::property_shape(
                &ty,
                &plans.plans,
                self.pointer_width(),
                |()| None,
                |()| None,
            )
            .map_err(|reason| ("E_EMBED_TYPE", reason))?;
            let file = self.compile_time_input("embed", args)?;
            let format = match file
                .path
                .extension()
                .and_then(|extension| extension.to_str())
            {
                Some("json") => hew_codec::Format::Json,
                Some("yaml" | "yml") => hew_codec::Format::Yaml,
                Some("toml") => hew_codec::Format::Toml,
                _ => {
                    return Err((
                        "E_EMBED_FORMAT",
                        "embed! expects a .json, .yaml, .yml or .toml file".into(),
                    ))
                }
            };
            let value = hew_codec::typed::decode(&shape, format, &file.bytes).and_then(|decoded| {
                let constant = embed_constant(&facts, &plans, &ty, &decoded.value);
                match (constant, decoded.error) {
                    (Err(error @ hew_codec::DecodeError::Duplicate { .. }), _)
                    | (_, Some(error)) => Err(error),
                    (constant, None) => constant.map(|constant| constant.value),
                }
            });
            match value {
                Ok(value) => {
                    self.compile_time_values
                        .insert(SpanKey::in_module(span, self.current_module_idx), value);
                    Ok(())
                }
                Err(error) => {
                    let offset = match &error {
                        hew_codec::DecodeError::Syntax { offset, .. } => *offset,
                        _ => 0,
                    };
                    self.errors.push(
                        crate::error::TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            offset..offset,
                            format!("E_EMBED_DECODE: cannot embed {}: {error}", ty.user_facing()),
                        )
                        .with_source_input(&file.path),
                    );
                    Ok(())
                }
            }
        })();
        if let Err((code, message)) = result {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("{code}: {message}"),
            );
            return Ty::Error;
        }
        expected.clone()
    }

    fn source_location_constant(
        &self,
        span: &Span,
    ) -> Result<(ConstantValue, Ty), (&'static str, String)> {
        let provider = self.input_provider.as_ref().ok_or_else(|| {
            (
                "E_INCLUDE_NOT_FOUND",
                "compilation inputs are unavailable".into(),
            )
        })?;
        let source = self
            .current_item_source
            .as_deref()
            .unwrap_or_else(|| provider.root_source());
        let filename = source
            .file_name()
            .and_then(|name| name.to_str())
            .ok_or_else(|| {
                (
                    "E_SOURCE_LOCATION",
                    "source file has no usable filename".into(),
                )
            })?;
        let file = provider
            .include(source, filename)
            .map_err(|error| (error.code, error.message))?;
        let text = std::str::from_utf8(&file.bytes)
            .map_err(|_| ("E_SOURCE_LOCATION", "source file is not UTF-8".into()))?;
        let prefix = text.get(..span.start).ok_or_else(|| {
            (
                "E_SOURCE_LOCATION",
                "invocation position is outside the source file".into(),
            )
        })?;
        let line = prefix.bytes().filter(|byte| *byte == b'\n').count() + 1;
        let column = prefix
            .rsplit('\n')
            .next()
            .unwrap_or_default()
            .chars()
            .count()
            + 1;
        let fields = vec![
            (
                "file".into(),
                TypedConstant {
                    ty: ResolvedTy::String,
                    value: ConstantValue::String(file.display_path),
                },
            ),
            (
                "line".into(),
                TypedConstant {
                    ty: ResolvedTy::I64,
                    value: ConstantValue::Integer(line as i128),
                },
            ),
            (
                "column".into(),
                TypedConstant {
                    ty: ResolvedTy::I64,
                    value: ConstantValue::Integer(column as i128),
                },
            ),
        ];
        Ok((
            ConstantValue::Record(fields),
            Ty::Named {
                head: crate::KnownDecl::SourceLocation.head(),
                args: Vec::new(),
            },
        ))
    }
}

fn check_embed_keys(
    facts: &mut crate::TypeFactService,
    plans: &crate::data_plan::DataPlans,
) -> Result<(), String> {
    use crate::data_plan::DataKind;
    let mut work = plans
        .plans
        .values()
        .filter_map(|plan| match &plan.kind {
            DataKind::Map { key, .. } => Some(key.clone()),
            DataKind::Set(element) => Some(element.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut seen = std::collections::BTreeSet::new();
    while let Some(ty) = work.pop() {
        if !seen.insert(ty.clone()) {
            continue;
        }
        let selection = facts
            .capability_plan(&ty, crate::ValueCapability::Eq)
            .map_err(|error| format!("cannot select equality for {ty}: {error:?}"))?;
        if matches!(
            selection.as_ref().map(crate::ValueMethodSelection::plan),
            Some(crate::ValueMethodPlan::User { .. })
        ) {
            return Err(format!("embed! cannot execute user-defined equality for key `{ty}`; decode this collection at runtime"));
        }
        let plan = plans
            .plans
            .get(&ty)
            .ok_or_else(|| format!("missing checked key plan for `{ty}`"))?;
        work.extend(plan.children().into_iter().cloned());
    }
    Ok(())
}

fn embed_invalid(reason: impl Into<String>) -> hew_codec::DecodeError {
    hew_codec::DecodeError::Invalid {
        path: String::new(),
        reason: reason.into(),
    }
}

fn embed_sequence(
    value: &serde_json::Value,
) -> Result<&[serde_json::Value], hew_codec::DecodeError> {
    value
        .as_array()
        .map(Vec::as_slice)
        .ok_or_else(|| embed_invalid("decoded bridge is not a sequence"))
}

fn embed_scalar(
    ty: &ResolvedTy,
    value: &serde_json::Value,
) -> Result<ConstantValue, hew_codec::DecodeError> {
    let text = || {
        value
            .as_str()
            .ok_or_else(|| embed_invalid("decoded bridge is not text"))
    };
    Ok(match ty {
        ResolvedTy::Bool => ConstantValue::Bool(
            value
                .as_bool()
                .ok_or_else(|| embed_invalid("decoded bridge is not a boolean"))?,
        ),
        ResolvedTy::Char => ConstantValue::Char(
            text()?
                .chars()
                .next()
                .ok_or_else(|| embed_invalid("decoded bridge has an empty character"))?,
        ),
        ResolvedTy::String => ConstantValue::String(text()?.to_owned()),
        ResolvedTy::Bytes => ConstantValue::Bytes(
            embed_sequence(value)?
                .iter()
                .map(|byte| {
                    byte.as_u64()
                        .and_then(|byte| u8::try_from(byte).ok())
                        .ok_or_else(|| embed_invalid("decoded bridge has an invalid byte"))
                })
                .collect::<Result<_, _>>()?,
        ),
        ResolvedTy::F32 | ResolvedTy::F64 => ConstantValue::Float(
            text()?
                .parse()
                .map_err(|_| embed_invalid("decoded bridge has an invalid float"))?,
        ),
        ResolvedTy::Duration => ConstantValue::Duration(
            text()?
                .parse()
                .map_err(|_| embed_invalid("decoded bridge has an invalid duration"))?,
        ),
        _ => ConstantValue::Integer(
            text()?
                .parse()
                .map_err(|_| embed_invalid("decoded bridge has an invalid integer"))?,
        ),
    })
}

#[expect(
    clippy::too_many_lines,
    reason = "the bridge projection mirrors the exhaustive checked data plan"
)]
fn embed_constant(
    facts: &crate::TypeFactService,
    plans: &crate::data_plan::DataPlans,
    ty: &ResolvedTy,
    bridge: &serde_json::Value,
) -> Result<TypedConstant, hew_codec::DecodeError> {
    use crate::data_plan::{DataKind, DataPayload};
    let plan = plans
        .plans
        .get(ty)
        .ok_or_else(|| embed_invalid(format!("missing checked data plan for `{ty}`")))?;
    let child =
        |ty: &ResolvedTy, bridge: &serde_json::Value| embed_constant(facts, plans, ty, bridge);
    let children = |types: &[ResolvedTy], bridges: &[serde_json::Value]| {
        let values = types
            .iter()
            .zip(bridges)
            .map(|(ty, bridge)| child(ty, bridge))
            .collect::<Result<Vec<_>, _>>()?;
        if types.len() != bridges.len() {
            return Err(embed_invalid(
                "decoded bridge field count disagrees with its plan",
            ));
        }
        Ok(values)
    };
    let value = match &plan.kind {
        DataKind::Scalar => embed_scalar(ty, bridge)?,
        DataKind::Unit => ConstantValue::Unit,
        DataKind::Tuple(fields) => ConstantValue::Tuple(children(fields, embed_sequence(bridge)?)?),
        DataKind::Array { element, .. } => ConstantValue::Array(
            embed_sequence(bridge)?
                .iter()
                .map(|bridge| child(element, bridge))
                .collect::<Result<_, _>>()?,
        ),
        DataKind::Vector(element) => ConstantValue::Vector(
            embed_sequence(bridge)?
                .iter()
                .map(|bridge| child(element, bridge))
                .collect::<Result<_, _>>()?,
        ),
        DataKind::Set(element) => {
            let mut elements = Vec::new();
            for bridge in embed_sequence(bridge)? {
                let [value, duplicate] = embed_sequence(bridge)? else {
                    return Err(embed_invalid(
                        "decoded set element lacks duplicate metadata",
                    ));
                };
                let value = child(element, value)?;
                if elements.contains(&value) {
                    return Err(embed_duplicate(duplicate)?);
                }
                elements.push(value);
            }
            ConstantValue::Set(elements)
        }
        DataKind::Map { key, value } => {
            let mut entries: Vec<(TypedConstant, TypedConstant)> = Vec::new();
            for bridge in embed_sequence(bridge)? {
                let [key_bridge, value_bridge, duplicate] = embed_sequence(bridge)? else {
                    return Err(embed_invalid("decoded map entry lacks duplicate metadata"));
                };
                let key = child(key, key_bridge)?;
                let value = child(value, value_bridge)?;
                if entries.iter().any(|(previous, _)| previous == &key) {
                    return Err(embed_duplicate(duplicate)?);
                }
                entries.push((key, value));
            }
            ConstantValue::Map(entries)
        }
        DataKind::Record { fields, .. } => {
            let (_, named) = facts.record_fields(ty).map_err(embed_invalid)?;
            let values = children(fields, embed_sequence(bridge)?)?;
            ConstantValue::Record(
                named
                    .into_iter()
                    .map(|(name, _)| name)
                    .zip(values)
                    .collect(),
            )
        }
        DataKind::Option {
            none, some, value, ..
        } => {
            if bridge.is_null() {
                ConstantValue::Variant {
                    index: *none as usize,
                    fields: Vec::new(),
                }
            } else {
                let [bridge] = embed_sequence(bridge)? else {
                    return Err(embed_invalid("decoded option has incorrect payload arity"));
                };
                ConstantValue::Variant {
                    index: *some as usize,
                    fields: vec![("0".into(), child(value, bridge)?)],
                }
            }
        }
        DataKind::Enum { variants, .. } => {
            let [index, payload] = embed_sequence(bridge)? else {
                return Err(embed_invalid("decoded enum lacks its tag and payload"));
            };
            let index = index
                .as_u64()
                .and_then(|index| usize::try_from(index).ok())
                .ok_or_else(|| embed_invalid("decoded enum tag is invalid"))?;
            let variant = variants
                .get(index)
                .ok_or_else(|| embed_invalid("decoded enum tag is outside its plan"))?;
            let values = match variant {
                DataPayload::Unit => Vec::new(),
                DataPayload::Single(ty) => vec![child(ty, payload)?],
                DataPayload::Tuple(types) | DataPayload::Record { fields: types, .. } => {
                    children(types, embed_sequence(payload)?)?
                }
            };
            let declarations = facts.data_variants(ty).map_err(embed_invalid)?;
            let declaration = declarations
                .get(index)
                .ok_or_else(|| embed_invalid("decoded enum tag has no declaration"))?;
            ConstantValue::Variant {
                index,
                fields: declaration
                    .fields
                    .iter()
                    .map(|(name, _)| name.clone())
                    .zip(values)
                    .collect(),
            }
        }
    };
    Ok(TypedConstant {
        ty: ty.clone(),
        value,
    })
}

fn embed_duplicate(
    bridge: &serde_json::Value,
) -> Result<hew_codec::DecodeError, hew_codec::DecodeError> {
    let [path, key] = embed_sequence(bridge)? else {
        return Err(embed_invalid("decoded duplicate metadata is incomplete"));
    };
    Ok(hew_codec::DecodeError::Duplicate {
        path: path
            .as_str()
            .ok_or_else(|| embed_invalid("decoded duplicate path is invalid"))?
            .into(),
        key: key
            .as_str()
            .ok_or_else(|| embed_invalid("decoded duplicate key is invalid"))?
            .into(),
    })
}
