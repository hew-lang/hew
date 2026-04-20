//! Hover analysis: produce rich hover information for identifiers and expressions.

use std::collections::HashMap;

use hew_parser::ast::{
    Block, FnDecl, Item, Param, Pattern, Span, Stmt, TraitBound, TraitItem, TypeBodyItem, TypeExpr,
};
use hew_parser::ParseResult;
use hew_types::builtin_names::{builtin_named_type, BuiltinNamedType};
use hew_types::check::{FnSig, SpanKey, TypeDef, TypeDefKind};
use hew_types::method_resolution;
use hew_types::{ResolvedTy, Ty, TypeCheckOutput, VariantDef};

use crate::db::SourceDatabase;
use crate::{HoverResult, OffsetSpan};

/// Compute hover information by reading inputs through a [`SourceDatabase`].
///
/// This is the routing path feature handlers should use: it is the single
/// place that joins source text, parse result, and type-check output from
/// the database before delegating to [`hover`] for the actual computation.
///
/// Returns `None` if the URI is unknown or produces no hover at `offset`.
#[must_use]
pub fn hover_via_db(db: &dyn SourceDatabase, uri: &str, offset: usize) -> Option<HoverResult> {
    let source = db.source(uri)?;
    let parse_result = db.parse(uri)?;
    let type_check = db.type_check(uri);
    hover(&source, &parse_result, type_check.as_deref(), offset)
}

/// Compute hover information at the given byte offset.
///
/// Returns a `HoverResult` with markdown contents and an optional span, or
/// `None` if there is nothing meaningful to display at this position.
#[must_use]
pub fn hover(
    source: &str,
    parse_result: &hew_parser::ParseResult,
    type_output: Option<&TypeCheckOutput>,
    offset: usize,
) -> Option<HoverResult> {
    // Find the word under the cursor for function/type lookup.
    let word = crate::util::word_at_offset(source, offset);
    let simple_word = crate::util::simple_word_at_offset(source, offset);

    let type_output = type_output?;

    if let Some((word, word_span)) = &simple_word {
        if let Some(result) =
            hover_param_at_offset(parse_result, &type_output.fn_sigs, word, *word_span, offset)
        {
            return Some(result);
        }

        if let Some(result) =
            hover_binding_at_offset(parse_result, type_output, word, *word_span, offset)
        {
            return Some(result);
        }
    }

    if let Some(result) = hover_field_at_offset(source, parse_result, type_output, offset) {
        return Some(result);
    }

    // Fall back to narrowest expression type that covers this offset.
    let mut best: Option<(&SpanKey, &Ty)> = None;
    for (span_key, ty) in &type_output.expr_types {
        if span_key.start <= offset && offset <= span_key.end {
            match best {
                Some((prev, _)) if (span_key.end - span_key.start) < (prev.end - prev.start) => {
                    best = Some((span_key, ty));
                }
                None => {
                    best = Some((span_key, ty));
                }
                _ => {}
            }
        }
    }

    best.map(|(span_key, ty)| {
        let snippet = &source[span_key.start..span_key.end];
        let value = render_expr_hover(snippet, ty);
        HoverResult {
            contents: value,
            span: Some(OffsetSpan {
                start: span_key.start,
                end: span_key.end,
            }),
        }
    })
    .or_else(|| {
        word.as_ref().and_then(|word| {
            if let Some(sig) = type_output.fn_sigs.get(word.as_str()) {
                let hover_text = format_fn_signature(word, sig);
                return Some(HoverResult {
                    contents: hover_text,
                    span: None,
                });
            }

            method_resolution::lookup_type_def(&type_output.type_defs, word.as_str()).map(
                |type_def| HoverResult {
                    contents: format_type_def_hover(&type_def),
                    span: None,
                },
            )
        })
    })
}

/// Convert a checker-boundary [`Ty`] into the rendered hover form via
/// [`ResolvedTy`], defaulting any numeric-literal kinds first.
///
/// Returns `None` if the type carries checker-internal state
/// (`Ty::Var`, `Ty::Error`) that the boundary converter rejects. Callers
/// decide what to render in that case; today they fall back to the
/// best-effort `Ty::user_facing` rendering to preserve historical hover
/// output for error-recovery paths.
fn resolved_hover_display(ty: &Ty) -> Option<String> {
    ResolvedTy::from_ty(&ty.materialize_literal_defaults())
        .ok()
        .map(|resolved| resolved.user_facing().to_string())
}

fn render_expr_hover(snippet: &str, ty: &Ty) -> String {
    if let Ty::Function { params, ret } = ty {
        let param_list: Vec<String> = params.iter().map(fn_component_display).collect();
        let ret_text = fn_component_display(ret);
        return format!(
            "```hew\nfn {snippet}({}) -> {ret_text}\n```",
            param_list.join(", ")
        );
    }
    let body = resolved_hover_display(ty).unwrap_or_else(|| ty.user_facing().to_string());
    format!("```hew\n{snippet}: {body}\n```")
}

fn fn_component_display(ty: &Ty) -> String {
    resolved_hover_display(ty).unwrap_or_else(|| ty.user_facing().to_string())
}

fn hover_param_at_offset(
    parse_result: &ParseResult,
    fn_sigs: &HashMap<String, FnSig>,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    for (item, _) in &parse_result.program.items {
        if let Some(result) = hover_param_in_item(item, fn_sigs, word, word_span, offset) {
            return Some(result);
        }
    }
    None
}

fn hover_binding_at_offset(
    parse_result: &ParseResult,
    type_output: &TypeCheckOutput,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    for (item, _) in &parse_result.program.items {
        if let Some(result) = hover_binding_in_item(item, type_output, word, word_span, offset) {
            return Some(result);
        }
    }
    None
}

fn hover_field_at_offset(
    source: &str,
    parse_result: &ParseResult,
    type_output: &TypeCheckOutput,
    offset: usize,
) -> Option<HoverResult> {
    hover_field_declaration_at_offset(source, parse_result, type_output, offset)
        .or_else(|| hover_field_access_at_offset(source, type_output, offset))
}

fn hover_field_declaration_at_offset(
    source: &str,
    parse_result: &ParseResult,
    type_output: &TypeCheckOutput,
    offset: usize,
) -> Option<HoverResult> {
    for (item, item_span) in &parse_result.program.items {
        let Item::TypeDecl(type_decl) = item else {
            continue;
        };
        let mut search_from = item_span.start;
        for body_item in &type_decl.body {
            match body_item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let span = crate::util::find_name_span(source, search_from, name);
                    if span.start <= offset && offset < span.end {
                        let ty_text = method_resolution::lookup_type_def(
                            &type_output.type_defs,
                            &type_decl.name,
                        )
                        .and_then(|type_def| {
                            type_def
                                .fields
                                .get(name)
                                .map(|ty| ty.user_facing().to_string())
                        })
                        .unwrap_or_else(|| format_type_expr_hover(&ty.0));
                        return Some(field_hover_result(name, &ty_text, span));
                    }
                    search_from = ty.1.end.max(span.end);
                }
                TypeBodyItem::Variant(variant) => {
                    search_from =
                        crate::util::find_name_span(source, search_from, &variant.name).end;
                }
                TypeBodyItem::Method(method) => {
                    search_from = search_from.max(method.decl_span.end);
                }
            }
        }
    }
    None
}

fn hover_field_access_at_offset(
    source: &str,
    type_output: &TypeCheckOutput,
    offset: usize,
) -> Option<HoverResult> {
    let (field_name, field_span) = crate::util::simple_word_at_offset(source, offset)?;
    let receiver_end = crate::definition::find_field_receiver_end(source, field_span.start)?;
    let receiver_ty = crate::method_lookup::find_receiver_type(type_output, receiver_end)?;
    let receiver_type_name = receiver_ty.type_name()?;
    let type_def = type_output.type_defs.iter().find_map(|(name, type_def)| {
        Ty::names_match_qualified(name, receiver_type_name).then_some(type_def)
    })?;
    let field_ty = type_def.fields.get(&field_name)?;
    Some(field_hover_result(
        &field_name,
        &field_ty.user_facing().to_string(),
        field_span,
    ))
}

fn field_hover_result(name: &str, ty_text: &str, span: OffsetSpan) -> HoverResult {
    HoverResult {
        contents: format!("```hew\n(field) {name}: {ty_text}\n```"),
        span: Some(span),
    }
}

fn hover_binding_in_item(
    item: &Item,
    type_output: &TypeCheckOutput,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    match item {
        Item::Function(function) => {
            hover_binding_in_block(&function.body, type_output, word, word_span, offset)
        }
        Item::Actor(actor) => {
            if let Some(init) = &actor.init {
                if let Some(result) =
                    hover_binding_in_block(&init.body, type_output, word, word_span, offset)
                {
                    return Some(result);
                }
            }
            if let Some(term) = &actor.terminate {
                if let Some(result) =
                    hover_binding_in_block(&term.body, type_output, word, word_span, offset)
                {
                    return Some(result);
                }
            }
            for recv in &actor.receive_fns {
                if let Some(result) =
                    hover_binding_in_block(&recv.body, type_output, word, word_span, offset)
                {
                    return Some(result);
                }
            }
            for method in &actor.methods {
                if let Some(result) =
                    hover_binding_in_block(&method.body, type_output, word, word_span, offset)
                {
                    return Some(result);
                }
            }
            None
        }
        Item::TypeDecl(type_decl) => {
            for body_item in &type_decl.body {
                if let TypeBodyItem::Method(method) = body_item {
                    if let Some(result) =
                        hover_binding_in_block(&method.body, type_output, word, word_span, offset)
                    {
                        return Some(result);
                    }
                }
            }
            None
        }
        Item::Impl(impl_decl) => {
            for method in &impl_decl.methods {
                if let Some(result) =
                    hover_binding_in_block(&method.body, type_output, word, word_span, offset)
                {
                    return Some(result);
                }
            }
            None
        }
        Item::Trait(trait_decl) => {
            for trait_item in &trait_decl.items {
                if let TraitItem::Method(method) = trait_item {
                    if let Some(body) = &method.body {
                        if let Some(result) =
                            hover_binding_in_block(body, type_output, word, word_span, offset)
                        {
                            return Some(result);
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn hover_binding_in_block(
    block: &Block,
    type_output: &TypeCheckOutput,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    for (stmt, stmt_span) in &block.stmts {
        if let Some(result) =
            hover_binding_in_stmt(stmt, stmt_span, type_output, word, word_span, offset)
        {
            return Some(result);
        }
    }
    None
}

#[allow(
    clippy::too_many_lines,
    reason = "explicit statement traversal keeps hover binding scope checks readable"
)]
fn hover_binding_in_stmt(
    stmt: &Stmt,
    stmt_span: &Span,
    type_output: &TypeCheckOutput,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    match stmt {
        Stmt::Let { pattern, ty, value } => {
            find_binding_name(pattern, word, offset)?;
            let ty_text = if let Some((type_expr, _)) = ty {
                format_type_expr_hover(type_expr)
            } else {
                let (_, value_span) = value.as_ref()?;
                let span_key = SpanKey {
                    start: value_span.start,
                    end: value_span.end,
                };
                type_output
                    .expr_types
                    .get(&span_key)?
                    .user_facing()
                    .to_string()
            };
            Some(HoverResult {
                contents: format!("```hew\n{word}: {ty_text}\n```"),
                span: Some(word_span),
            })
        }
        Stmt::Var { name, ty, value } => {
            if !is_var_name_span(
                stmt_span,
                word,
                word_span,
                offset,
                name,
                ty.as_ref(),
                value.as_ref(),
            ) {
                return None;
            }
            let ty_text = if let Some((type_expr, _)) = ty {
                format_type_expr_hover(type_expr)
            } else {
                let (_, value_span) = value.as_ref()?;
                let span_key = SpanKey {
                    start: value_span.start,
                    end: value_span.end,
                };
                type_output
                    .expr_types
                    .get(&span_key)?
                    .user_facing()
                    .to_string()
            };
            Some(HoverResult {
                contents: format!("```hew\n{word}: {ty_text}\n```"),
                span: Some(word_span),
            })
        }
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            if let Some(result) =
                hover_binding_in_block(then_block, type_output, word, word_span, offset)
            {
                return Some(result);
            }
            let Some(else_block) = else_block else {
                return None;
            };
            if let Some(if_stmt) = &else_block.if_stmt {
                if let Some(result) = hover_binding_in_stmt(
                    &if_stmt.0,
                    &if_stmt.1,
                    type_output,
                    word,
                    word_span,
                    offset,
                ) {
                    return Some(result);
                }
            }
            else_block.block.as_ref().and_then(|block| {
                hover_binding_in_block(block, type_output, word, word_span, offset)
            })
        }
        Stmt::IfLet {
            pattern,
            expr,
            body,
            else_body,
            ..
        } => {
            if let Some(source_ty) = type_output.expr_types.get(&SpanKey::from(&expr.1)) {
                if let Some(result) = hover_pattern_binding(
                    pattern,
                    source_ty,
                    &type_output.type_defs,
                    word,
                    word_span,
                    offset,
                ) {
                    return Some(result);
                }
            }
            if let Some(result) = hover_binding_in_block(body, type_output, word, word_span, offset)
            {
                return Some(result);
            }
            else_body.as_ref().and_then(|block| {
                hover_binding_in_block(block, type_output, word, word_span, offset)
            })
        }
        Stmt::For {
            pattern,
            iterable,
            body,
            ..
        } => {
            if let Some(result) = type_output
                .expr_types
                .get(&SpanKey::from(&iterable.1))
                .and_then(iterable_element_type)
                .and_then(|elem_ty| {
                    hover_pattern_binding(
                        pattern,
                        &elem_ty,
                        &type_output.type_defs,
                        word,
                        word_span,
                        offset,
                    )
                })
            {
                return Some(result);
            }
            hover_binding_in_block(body, type_output, word, word_span, offset)
        }
        Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
            hover_binding_in_block(body, type_output, word, word_span, offset)
        }
        Stmt::WhileLet {
            pattern,
            expr,
            body,
            ..
        } => {
            if let Some(source_ty) = type_output.expr_types.get(&SpanKey::from(&expr.1)) {
                if let Some(result) = hover_pattern_binding(
                    pattern,
                    source_ty,
                    &type_output.type_defs,
                    word,
                    word_span,
                    offset,
                ) {
                    return Some(result);
                }
            }
            hover_binding_in_block(body, type_output, word, word_span, offset)
        }
        Stmt::Match { scrutinee, arms } => type_output
            .expr_types
            .get(&SpanKey::from(&scrutinee.1))
            .and_then(|scrutinee_ty| {
                arms.iter().find_map(|arm| {
                    hover_pattern_binding(
                        &arm.pattern,
                        scrutinee_ty,
                        &type_output.type_defs,
                        word,
                        word_span,
                        offset,
                    )
                })
            }),
        _ => None,
    }
}

fn hover_pattern_binding(
    pattern: &(Pattern, Span),
    source_ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    let binding_ty = find_pattern_binding_type(pattern, source_ty, type_defs, word, offset)?;
    Some(HoverResult {
        contents: format!("```hew\n{word}: {}\n```", binding_ty.user_facing()),
        span: Some(word_span),
    })
}

fn find_pattern_binding_type(
    pattern: &(Pattern, Span),
    source_ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
    word: &str,
    offset: usize,
) -> Option<Ty> {
    if !span_contains_offset(&pattern.1, offset) {
        return None;
    }
    match &pattern.0 {
        Pattern::Identifier(name) => (name == word).then(|| source_ty.clone()),
        Pattern::Constructor { name, patterns } => {
            constructor_payload_tys(source_ty, name, type_defs).and_then(|payload_tys| {
                patterns
                    .iter()
                    .zip(payload_tys.iter())
                    .find_map(|(pattern, payload_ty)| {
                        find_pattern_binding_type(pattern, payload_ty, type_defs, word, offset)
                    })
            })
        }
        Pattern::Struct { name, fields } => fields.iter().find_map(|field| {
            let field_ty = struct_pattern_field_ty(source_ty, name, &field.name, type_defs)?;
            field.pattern.as_ref().and_then(|pattern| {
                find_pattern_binding_type(pattern, &field_ty, type_defs, word, offset)
            })
        }),
        Pattern::Tuple(patterns) => {
            let Ty::Tuple(elem_tys) = source_ty else {
                return None;
            };
            patterns
                .iter()
                .zip(elem_tys.iter())
                .find_map(|(pattern, elem_ty)| {
                    find_pattern_binding_type(pattern, elem_ty, type_defs, word, offset)
                })
        }
        Pattern::Or(left, right) => {
            find_pattern_binding_type(left, source_ty, type_defs, word, offset)
                .or_else(|| find_pattern_binding_type(right, source_ty, type_defs, word, offset))
        }
        Pattern::Wildcard | Pattern::Literal(_) => None,
    }
}

fn find_binding_name(pattern: &(Pattern, Span), word: &str, offset: usize) -> Option<()> {
    if !span_contains_offset(&pattern.1, offset) {
        return None;
    }
    match &pattern.0 {
        Pattern::Identifier(name) => (name == word).then_some(()),
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => patterns
            .iter()
            .find_map(|pattern| find_binding_name(pattern, word, offset)),
        Pattern::Struct { fields, .. } => fields.iter().find_map(|field| {
            field
                .pattern
                .as_ref()
                .and_then(|pattern| find_binding_name(pattern, word, offset))
        }),
        Pattern::Or(left, right) => {
            find_binding_name(left, word, offset).or_else(|| find_binding_name(right, word, offset))
        }
        Pattern::Wildcard | Pattern::Literal(_) => None,
    }
}

fn constructor_payload_tys(
    source_ty: &Ty,
    pattern_name: &str,
    type_defs: &HashMap<String, TypeDef>,
) -> Option<Vec<Ty>> {
    let Ty::Named { name, args } = source_ty else {
        return None;
    };
    let type_def = method_resolution::lookup_type_def(type_defs, name)?;
    let short_name = pattern_name.rsplit("::").next().unwrap_or(pattern_name);
    let VariantDef::Tuple(payload_tys) = type_def.variants.get(short_name)? else {
        return None;
    };
    Some(apply_type_args(payload_tys, &type_def.type_params, args))
}

fn struct_pattern_field_ty(
    source_ty: &Ty,
    pattern_name: &str,
    field_name: &str,
    type_defs: &HashMap<String, TypeDef>,
) -> Option<Ty> {
    let Ty::Named { name, args } = source_ty else {
        return None;
    };
    let type_def = method_resolution::lookup_type_def(type_defs, name)?;
    let short_name = pattern_name.rsplit("::").next().unwrap_or(pattern_name);
    if let Some(VariantDef::Struct(fields)) = type_def.variants.get(short_name) {
        let field_ty = fields
            .iter()
            .find_map(|(name, ty)| (name == field_name).then(|| ty.clone()))?;
        return Some(apply_type_args_to_ty(
            &field_ty,
            &type_def.type_params,
            args,
        ));
    }
    let field_ty = type_def.fields.get(field_name)?;
    Some(apply_type_args_to_ty(field_ty, &type_def.type_params, args))
}

fn apply_type_args(payload_tys: &[Ty], type_params: &[String], type_args: &[Ty]) -> Vec<Ty> {
    payload_tys
        .iter()
        .map(|ty| apply_type_args_to_ty(ty, type_params, type_args))
        .collect()
}

fn apply_type_args_to_ty(ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
    type_params
        .iter()
        .zip(type_args.iter())
        .fold(ty.clone(), |acc, (param, arg)| {
            acc.substitute_named_param(param, arg)
        })
}

fn iterable_element_type(iterable_ty: &Ty) -> Option<Ty> {
    match iterable_ty {
        Ty::Array(inner, _) | Ty::Slice(inner) => Some((**inner).clone()),
        Ty::Named { name, args } if name == "Range" && args.len() == 1 => args.first().cloned(),
        Ty::Named { name, args }
            if builtin_named_type(name) == Some(BuiltinNamedType::Stream)
                || builtin_named_type(name) == Some(BuiltinNamedType::Receiver)
                || (name == "Generator" && !args.is_empty())
                || (name == "AsyncGenerator" && args.len() == 1)
                || name == "Vec" =>
        {
            args.first().cloned()
        }
        Ty::Named { name, args } if name == "HashMap" && args.len() >= 2 => {
            Some(Ty::Tuple(vec![args[0].clone(), args[1].clone()]))
        }
        _ => None,
    }
}

fn is_var_name_span(
    stmt_span: &Span,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
    name: &str,
    ty: Option<&(TypeExpr, Span)>,
    value: Option<&(hew_parser::ast::Expr, Span)>,
) -> bool {
    if name != word || !span_contains_offset(stmt_span, offset) {
        return false;
    }
    let name_boundary = ty
        .as_ref()
        .map(|(_, span)| span.start)
        .or_else(|| value.as_ref().map(|(_, span)| span.start))
        .unwrap_or(stmt_span.end);
    word_span.start >= stmt_span.start && word_span.end <= name_boundary
}

fn format_type_expr_hover(type_expr: &TypeExpr) -> String {
    match type_expr {
        TypeExpr::Named { name, type_args } => {
            let base =
                Ty::from_name(name).map_or_else(|| name.clone(), |ty| ty.user_facing().to_string());
            if let Some(type_args) = type_args {
                let args = type_args
                    .iter()
                    .map(|(arg, _)| format_type_expr_hover(arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{base}<{args}>")
            } else {
                base
            }
        }
        TypeExpr::Result { ok, err } => format!(
            "Result<{}, {}>",
            format_type_expr_hover(&ok.0),
            format_type_expr_hover(&err.0)
        ),
        TypeExpr::Option(inner) => format!("Option<{}>", format_type_expr_hover(&inner.0)),
        TypeExpr::Tuple(elements) => format!(
            "({})",
            elements
                .iter()
                .map(|(element, _)| format_type_expr_hover(element))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        TypeExpr::Array { element, size } => {
            format!("[{}; {size}]", format_type_expr_hover(&element.0))
        }
        TypeExpr::Slice(element) => format!("[{}]", format_type_expr_hover(&element.0)),
        TypeExpr::Function {
            params,
            return_type,
        } => format!(
            "fn({}) -> {}",
            params
                .iter()
                .map(|(param, _)| format_type_expr_hover(param))
                .collect::<Vec<_>>()
                .join(", "),
            format_type_expr_hover(&return_type.0)
        ),
        TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => {
            let mutability = if *is_mutable { "mut " } else { "" };
            format!("*{mutability}{}", format_type_expr_hover(&pointee.0))
        }
        TypeExpr::TraitObject(bounds) => bounds
            .iter()
            .map(format_trait_bound_hover)
            .collect::<Vec<_>>()
            .join(" + "),
        TypeExpr::Infer => "_".to_string(),
    }
}

fn format_trait_bound_hover(bound: &TraitBound) -> String {
    if let Some(type_args) = &bound.type_args {
        format!(
            "{}<{}>",
            bound.name,
            type_args
                .iter()
                .map(|(arg, _)| format_type_expr_hover(arg))
                .collect::<Vec<_>>()
                .join(", ")
        )
    } else {
        bound.name.clone()
    }
}

fn hover_param_in_item(
    item: &Item,
    fn_sigs: &HashMap<String, FnSig>,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    match item {
        Item::Function(function) => hover_param_in_decl(
            &function.fn_span,
            &function.params,
            fn_sigs.get(function.name.as_str()),
            word,
            word_span,
            offset,
        ),
        Item::Actor(actor) => {
            for recv in &actor.receive_fns {
                let key = format!("{}::{}", actor.name, recv.name);
                if let Some(result) = hover_param_in_decl(
                    &recv.span,
                    &recv.params,
                    fn_sigs.get(key.as_str()),
                    word,
                    word_span,
                    offset,
                ) {
                    return Some(result);
                }
            }
            for method in &actor.methods {
                let key = format!("{}::{}", actor.name, method.name);
                if let Some(result) = hover_param_in_method(
                    method,
                    fn_sigs.get(key.as_str()),
                    word,
                    word_span,
                    offset,
                ) {
                    return Some(result);
                }
            }
            None
        }
        Item::TypeDecl(type_decl) => {
            for body_item in &type_decl.body {
                if let TypeBodyItem::Method(method) = body_item {
                    let key = format!("{}::{}", type_decl.name, method.name);
                    if let Some(result) = hover_param_in_method(
                        method,
                        fn_sigs.get(key.as_str()),
                        word,
                        word_span,
                        offset,
                    ) {
                        return Some(result);
                    }
                }
            }
            None
        }
        Item::Impl(impl_decl) => {
            let TypeExpr::Named { name, .. } = &impl_decl.target_type.0 else {
                return None;
            };
            for method in &impl_decl.methods {
                let key = format!("{name}::{}", method.name);
                if let Some(result) = hover_param_in_method(
                    method,
                    fn_sigs.get(key.as_str()),
                    word,
                    word_span,
                    offset,
                ) {
                    return Some(result);
                }
            }
            None
        }
        Item::Trait(trait_decl) => {
            for trait_item in &trait_decl.items {
                if let TraitItem::Method(method) = trait_item {
                    let key = format!("{}::{}", trait_decl.name, method.name);
                    if let Some(result) = hover_param_in_decl(
                        &method.span,
                        &method.params,
                        fn_sigs.get(key.as_str()),
                        word,
                        word_span,
                        offset,
                    ) {
                        return Some(result);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn hover_param_in_method(
    method: &FnDecl,
    sig: Option<&FnSig>,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    hover_param_in_decl(
        &method.fn_span,
        &method.params,
        sig,
        word,
        word_span,
        offset,
    )
}

fn hover_param_in_decl(
    decl_span: &Span,
    params: &[Param],
    sig: Option<&FnSig>,
    word: &str,
    word_span: OffsetSpan,
    offset: usize,
) -> Option<HoverResult> {
    if !span_contains_offset(decl_span, offset) {
        return None;
    }

    let sig = sig?;
    let param = params
        .iter()
        .find(|param| is_param_name_span(param, word, word_span))?;
    let ty = sig
        .param_names
        .iter()
        .zip(&sig.params)
        .find_map(|(param_name, ty)| (param_name == &param.name).then_some(ty))?;

    Some(HoverResult {
        contents: format!("```hew\n{word}: {}\n```", ty.user_facing()),
        span: Some(word_span),
    })
}

fn is_param_name_span(param: &Param, word: &str, word_span: OffsetSpan) -> bool {
    if param.name != word || word_span.end > param.ty.1.start {
        return false;
    }

    param.ty.1.start > word_span.end && param.ty.1.start.saturating_sub(word_span.end) <= 4
}

fn span_contains_offset(span: &Span, offset: usize) -> bool {
    span.is_empty() || (span.start <= offset && offset <= span.end)
}

/// Format a bare function signature line: `[pure] [async] fn name(params)[-> ret]`.
#[must_use]
pub fn format_fn_sig_line(name: &str, params: &[String], sig: &FnSig) -> String {
    let pure_prefix = if sig.is_pure { "pure " } else { "" };
    let async_prefix = if sig.is_async { "async " } else { "" };
    let ret = if sig.return_type == Ty::Unit {
        String::new()
    } else {
        format!(" -> {}", sig.return_type.user_facing())
    };
    format!(
        "{pure_prefix}{async_prefix}fn {name}({}){ret}",
        params.join(", ")
    )
}

/// Format a function signature in a markdown code block for hover display.
#[must_use]
pub fn format_fn_signature(name: &str, sig: &FnSig) -> String {
    let params = format_fn_hover_params(sig);
    let code = format!("```hew\n{}\n```", format_fn_sig_line(name, &params, sig));
    if let Some(doc) = &sig.doc_comment {
        format!("{doc}\n\n---\n\n{code}")
    } else {
        code
    }
}

/// Format a function signature as a single inline line (for embedding in type hover).
#[must_use]
pub fn format_fn_signature_inline(name: &str, sig: &FnSig) -> String {
    let params = format_fn_hover_params(sig);
    format_fn_sig_line(name, &params, sig)
}

fn format_fn_hover_params(sig: &FnSig) -> Vec<String> {
    sig.params
        .iter()
        .enumerate()
        .map(|(index, ty)| match sig.param_names.get(index) {
            Some(name) if !name.is_empty() => format!("{name}: {}", ty.user_facing()),
            _ => ty.user_facing().to_string(),
        })
        .collect()
}

/// Format a type definition for hover display.
#[must_use]
pub fn format_type_def_hover(type_def: &TypeDef) -> String {
    use std::fmt::Write;
    let kind_str = match type_def.kind {
        TypeDefKind::Struct => "type",
        TypeDefKind::Enum => "enum",
        TypeDefKind::Actor => "actor",
        TypeDefKind::Machine => "machine",
    };
    let type_params = if type_def.type_params.is_empty() {
        String::new()
    } else {
        format!("<{}>", type_def.type_params.join(", "))
    };
    let mut parts = format!("```hew\n{kind_str} {}{type_params}", type_def.name);
    let has_body = !type_def.fields.is_empty()
        || !type_def.variants.is_empty()
        || !type_def.methods.is_empty();
    if has_body {
        parts.push_str(" {\n");
        for (field_name, field_ty) in &type_def.fields {
            let _ = writeln!(parts, "    {field_name}: {},", field_ty.user_facing());
        }
        for (variant_name, payload) in &type_def.variants {
            match payload {
                VariantDef::Unit => {
                    let _ = writeln!(parts, "    {variant_name},");
                }
                VariantDef::Tuple(types) => {
                    let types: Vec<String> = types
                        .iter()
                        .map(|ty| ty.user_facing().to_string())
                        .collect();
                    let _ = writeln!(parts, "    {variant_name}({}),", types.join(", "));
                }
                VariantDef::Struct(fields) => {
                    let fields: Vec<String> = fields
                        .iter()
                        .map(|(n, t)| format!("{n}: {}", t.user_facing()))
                        .collect();
                    let _ = writeln!(parts, "    {variant_name} {{ {} }},", fields.join(", "));
                }
            }
        }
        for (method_name, sig) in &type_def.methods {
            let _ = writeln!(
                parts,
                "    {}",
                format_fn_signature_inline(method_name, sig)
            );
        }
        parts.push('}');
    }
    parts.push_str("\n```");
    if let Some(doc) = &type_def.doc_comment {
        format!("{doc}\n\n---\n\n{parts}")
    } else {
        parts
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_types::module_registry::ModuleRegistry;
    use std::collections::HashSet;

    fn make_fn_sig(param_names: Vec<&str>, params: Vec<Ty>, ret: Ty) -> FnSig {
        FnSig {
            param_names: param_names.into_iter().map(String::from).collect(),
            params,
            return_type: ret,
            ..FnSig::default()
        }
    }

    fn make_tc_with_fn(name: &str, sig: FnSig) -> TypeCheckOutput {
        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(name.to_string(), sig);
        TypeCheckOutput {
            expr_types: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs,
            handle_bearing_structs: std::collections::HashSet::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        }
    }

    fn type_check(pr: &hew_parser::ParseResult) -> TypeCheckOutput {
        let registry = ModuleRegistry::new(vec![]);
        let mut checker = hew_types::Checker::new(registry);
        checker.check_program(&pr.program)
    }

    #[test]
    fn format_sig_line_basic() {
        let sig = make_fn_sig(vec!["x", "y"], vec![Ty::I32, Ty::Bool], Ty::String);
        let line = format_fn_sig_line("greet", &["x: i32".into(), "y: bool".into()], &sig);
        assert!(line.starts_with("fn greet("));
        assert!(line.contains("-> String"));
    }

    #[test]
    fn format_sig_line_unit_return_omitted() {
        let sig = make_fn_sig(vec![], vec![], Ty::Unit);
        let line = format_fn_sig_line("run", &[], &sig);
        assert_eq!(line, "fn run()");
        assert!(!line.contains("->"));
    }

    #[test]
    fn format_sig_line_async() {
        let mut sig = make_fn_sig(vec![], vec![], Ty::Unit);
        sig.is_async = true;
        let line = format_fn_sig_line("fetch", &[], &sig);
        assert!(line.starts_with("async fn fetch"));
    }

    #[test]
    fn format_sig_line_pure() {
        let mut sig = make_fn_sig(vec![], vec![], Ty::I32);
        sig.is_pure = true;
        let line = format_fn_sig_line("compute", &[], &sig);
        assert!(line.starts_with("pure fn compute"));
    }

    #[test]
    fn format_fn_signature_wraps_in_code_block() {
        let sig = make_fn_sig(vec!["n"], vec![Ty::I32], Ty::I32);
        let text = format_fn_signature("factorial", &sig);
        assert!(text.contains("```hew"));
        assert!(text.contains("fn factorial(n: i32)"));
    }

    #[test]
    fn format_fn_signature_includes_doc_comment() {
        let mut sig = make_fn_sig(vec![], vec![], Ty::Unit);
        sig.doc_comment = Some("Runs the main loop.".to_string());
        let text = format_fn_signature("run", &sig);
        assert!(text.contains("Runs the main loop."));
        assert!(text.contains("```hew"));
    }

    #[test]
    fn format_type_def_struct() {
        let mut fields = HashMap::new();
        fields.insert("x".to_string(), Ty::F64);
        fields.insert("y".to_string(), Ty::F64);
        let td = TypeDef {
            kind: TypeDefKind::Struct,
            name: "Point".to_string(),
            type_params: vec![],
            fields,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        let text = format_type_def_hover(&td);
        assert!(text.contains("type Point"));
        assert!(text.contains("float"), "should contain field types");
    }

    #[test]
    fn format_type_def_enum_with_variants() {
        let mut variants = HashMap::new();
        variants.insert("Red".to_string(), VariantDef::Unit);
        variants.insert(
            "Rgb".to_string(),
            VariantDef::Tuple(vec![Ty::U8, Ty::U8, Ty::U8]),
        );
        let td = TypeDef {
            kind: TypeDefKind::Enum,
            name: "Colour".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        let text = format_type_def_hover(&td);
        assert!(text.contains("enum Colour"));
        assert!(text.contains("Red"));
    }

    #[test]
    fn format_type_def_with_type_params() {
        let td = TypeDef {
            kind: TypeDefKind::Struct,
            name: "Pair".to_string(),
            type_params: vec!["A".to_string(), "B".to_string()],
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        let text = format_type_def_hover(&td);
        assert!(text.contains("Pair<A, B>"));
    }

    #[test]
    fn hover_returns_none_without_type_output() {
        let source = "fn main() {}";
        let pr = hew_parser::parse(source);
        let result = hover(source, &pr, None, 3);
        assert!(result.is_none(), "hover without type output returns None");
    }

    #[test]
    fn hover_finds_function_signature() {
        let source = "fn add(x: i32, y: i32) -> i32 {\n    x + y\n}";
        let pr = hew_parser::parse(source);
        let sig = make_fn_sig(vec!["x", "y"], vec![Ty::I32, Ty::I32], Ty::I32);
        let tc = make_tc_with_fn("add", sig);
        let offset = source.find("add").unwrap();
        let result = hover(source, &pr, Some(&tc), offset);
        assert!(result.is_some(), "should find hover for add");
        let hr = result.unwrap();
        assert!(
            hr.contents.contains("fn add(x: i32, y: i32)"),
            "should show function signature"
        );
    }

    #[test]
    fn format_fn_signature_inline_includes_param_names() {
        let sig = make_fn_sig(vec!["value"], vec![Ty::String], Ty::Bool);
        let text = format_fn_signature_inline("validate", &sig);
        assert_eq!(text, "fn validate(value: String) -> bool");
    }

    #[test]
    fn hover_shows_top_level_fn_param_type() {
        let source = "fn add(x: i32, y: i32) -> i32 {\n    x + y\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("x: i32").unwrap();
        let result = hover(source, &pr, Some(&tc), offset).unwrap();

        assert_eq!(result.contents, "```hew\nx: i32\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + 1
            })
        );
    }

    #[test]
    fn hover_shows_actor_receive_param_type() {
        let source = "actor Worker {\n    receive fn handle(msg: string) {\n        msg\n    }\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("msg: string").unwrap();
        let result = hover(source, &pr, Some(&tc), offset).unwrap();

        assert_eq!(result.contents, "```hew\nmsg: String\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + 3
            })
        );
    }

    #[test]
    fn hover_shows_method_param_type() {
        let source =
            "type Counter {\n    fn bump(counter: Counter, n: i32) -> i32 {\n        n\n    }\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("n: i32").unwrap();
        let result = hover(source, &pr, Some(&tc), offset).unwrap();

        assert_eq!(result.contents, "```hew\nn: i32\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + 1
            })
        );
    }

    #[test]
    fn hover_param_does_not_shadow_fn_sig_lookup() {
        let source = "fn add(x: i32, y: i32) -> i32 {\n    x + y\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("add").unwrap();
        let result = hover(source, &pr, Some(&tc), offset).unwrap();

        assert!(result.contents.contains("fn add("));
        assert!(result.span.is_none());
    }

    #[test]
    fn hover_finds_type_def() {
        let source = "type Point {\n    x: f64;\n    y: f64;\n}";
        let pr = hew_parser::parse(source);
        let mut type_defs = HashMap::new();
        type_defs.insert(
            "Point".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Point".to_string(),
                type_params: vec![],
                fields: {
                    let mut f = HashMap::new();
                    f.insert("x".to_string(), Ty::F64);
                    f.insert("y".to_string(), Ty::F64);
                    f
                },
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            },
        );
        let tc = TypeCheckOutput {
            expr_types: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs,
            fn_sigs: HashMap::new(),
            handle_bearing_structs: std::collections::HashSet::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        };
        let offset = source.find("Point").unwrap();
        let result = hover(source, &pr, Some(&tc), offset);
        assert!(result.is_some(), "should find hover for Point");
        let hr = result.unwrap();
        assert!(hr.contents.contains("type Point"));
    }

    #[test]
    fn hover_shows_struct_field_declaration_type() {
        let source =
            "type Point {\n    x: i32;\n    y: i32;\n}\nfn main() { let p = Point { x: 1, y: 2 }; p.x }";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("x: i32").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\n(field) x: i32\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + 1
            })
        );
    }

    #[test]
    fn hover_shows_struct_field_access_type() {
        let source =
            "type Point {\n    x: i32;\n    y: i32;\n}\nfn main() { let p = Point { x: 1, y: 2 }; p.x }";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.rfind("p.x").unwrap() + 2;

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\n(field) x: i32\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + 1
            })
        );
    }

    #[test]
    fn hover_falls_back_to_expr_type() {
        let source = "fn main() {\n    let x = 42;\n}";
        let pr = hew_parser::parse(source);
        // Manually populate expr_types for the identifier "x"
        let x_offset = source.find("let x").unwrap() + 4;
        let mut expr_types = HashMap::new();
        expr_types.insert(
            SpanKey {
                start: x_offset,
                end: x_offset + 1,
            },
            Ty::I32,
        );
        let tc = TypeCheckOutput {
            expr_types,
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            handle_bearing_structs: std::collections::HashSet::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        };
        let result = hover(source, &pr, Some(&tc), x_offset);
        assert!(result.is_some(), "should find hover via expr_types");
        let hr = result.unwrap();
        assert!(hr.contents.contains("i32"), "should show expression type");
    }

    #[test]
    fn hover_uses_int_alias_for_user_facing_types() {
        let source = "fn main() {\n    let count = 42;\n}";
        let pr = hew_parser::parse(source);
        let count_offset = source.find("count").unwrap();
        let mut expr_types = HashMap::new();
        expr_types.insert(
            SpanKey {
                start: count_offset,
                end: count_offset + "count".len(),
            },
            Ty::I64,
        );
        let tc = TypeCheckOutput {
            expr_types,
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            handle_bearing_structs: std::collections::HashSet::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        };

        let result = hover(source, &pr, Some(&tc), count_offset).unwrap();
        assert!(result.contents.contains("count: int"));
        assert!(!result.contents.contains("i64"));
    }

    #[test]
    fn hover_shows_unannotated_let_binding_type() {
        let source = "fn compute() -> int { 42 }\nfn main() {\n    let count = compute();\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("count").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\ncount: int\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + "count".len()
            })
        );
    }

    #[test]
    fn hover_shows_annotated_var_binding_type() {
        let source = "fn main() {\n    var total: int = 0;\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("total").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\ntotal: int\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + "total".len()
            })
        );
    }

    #[test]
    fn hover_shows_nested_let_binding_inside_if_body() {
        let source = "fn yes() -> bool { true }\nfn main() {\n    if true {\n        let flag = yes();\n    }\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("flag").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\nflag: bool\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + "flag".len()
            })
        );
    }

    #[test]
    fn hover_binding_does_not_fire_on_binding_use_in_expr() {
        let source = "fn main() {\n    let count = 42;\n    count + 1\n}";
        let pr = hew_parser::parse(source);
        let let_offset = source.find("count").unwrap();
        let use_offset = source.rfind("count").unwrap();
        let mut expr_types = HashMap::new();
        expr_types.insert(
            SpanKey {
                start: use_offset,
                end: use_offset + "count".len(),
            },
            Ty::I32,
        );
        let tc = TypeCheckOutput {
            expr_types,
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            handle_bearing_structs: std::collections::HashSet::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        };

        let result = hover(source, &pr, Some(&tc), use_offset).unwrap();
        assert_eq!(result.contents, "```hew\ncount: i32\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: use_offset,
                end: use_offset + "count".len()
            })
        );
        assert_ne!(result.span.unwrap().start, let_offset);
    }

    #[test]
    fn hover_shows_for_pattern_binding_type() {
        let source = "fn main() {\n    for item in [1, 2] {\n        item\n    }\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("item in").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\nitem: int\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + "item".len()
            })
        );
    }

    #[test]
    fn hover_shows_while_let_pattern_binding_type() {
        let source =
            "fn pair() -> (bool, int) { (true, 1) }\nfn main() {\n    while let (flag, _) = pair() {\n        flag\n    }\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("flag, _").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\nflag: bool\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + "flag".len()
            })
        );
    }

    #[test]
    fn hover_shows_if_let_pattern_binding_type() {
        let source =
            "fn pair() -> (bool, int) { (true, 1) }\nfn main() {\n    if let (flag, _) = pair() {\n        flag\n    }\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("flag, _").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\nflag: bool\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + "flag".len()
            })
        );
    }

    #[test]
    fn hover_prefers_local_over_global_name() {
        let source = "fn value() -> int { 1 }\nfn main() {\n    let value = 2;\n    value\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.rfind("value").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert!(
            result.contents.contains(": int"),
            "local hover should render the local binding type: {result:?}"
        );
        assert_eq!(result.span.map(|span| span.start), Some(offset));
    }

    #[test]
    fn hover_shows_match_arm_pattern_binding_type() {
        let source =
            "fn pair() -> (bool, int) { (true, 1) }\nfn main() {\n    match pair() {\n        (flag, _) => flag,\n    }\n}";
        let pr = hew_parser::parse(source);
        let tc = type_check(&pr);
        let offset = source.find("flag, _").unwrap();

        let result = hover(source, &pr, Some(&tc), offset).unwrap();
        assert_eq!(result.contents, "```hew\nflag: bool\n```");
        assert_eq!(
            result.span,
            Some(OffsetSpan {
                start: offset,
                end: offset + "flag".len()
            })
        );
    }

    /// Regression: the expr-types fallback hover path crosses the
    /// checker boundary through `ResolvedTy::from_ty`. Int-literal kinds
    /// must be defaulted on the way through — rendering `<int literal>`
    /// (the internal-form spelling) would prove the boundary was
    /// bypassed.
    #[test]
    fn hover_expr_fallback_renders_through_resolved_ty_boundary() {
        use hew_types::resolved_ty::ResolvedTy;

        let source = "fn main() {\n    let x = 42;\n}";
        let pr = hew_parser::parse(source);
        let x_offset = source.find("let x").unwrap() + 4;
        let mut expr_types = HashMap::new();
        expr_types.insert(
            SpanKey {
                start: x_offset,
                end: x_offset + 1,
            },
            Ty::IntLiteral,
        );
        let tc = TypeCheckOutput {
            expr_types,
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            handle_bearing_structs: std::collections::HashSet::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        };
        let result = hover(source, &pr, Some(&tc), x_offset).unwrap();
        // Goes through ResolvedTy::from_ty(materialize_literal_defaults)
        // which produces ResolvedTy::I64, whose user-facing form is
        // "int" — the same as a concrete i64 at the boundary. If the
        // boundary were bypassed we would render "<int literal>".
        assert!(
            result.contents.contains("int"),
            "expected boundary-defaulted rendering, got {}",
            result.contents
        );
        assert!(!result.contents.contains("literal"));

        // And byte-equal to ResolvedTy::user_facing directly, which is
        // the authoritative rendering at the boundary.
        let expected_body = ResolvedTy::I64.user_facing().to_string();
        assert!(
            result.contents.contains(&expected_body),
            "hover body should equal ResolvedTy rendering {expected_body}, got {}",
            result.contents
        );
    }

    #[test]
    fn hover_via_db_matches_direct_hover() {
        use crate::db::{InMemorySourceDatabase, SourceDatabase};

        // A source with a function definition so hover has something to
        // look up through fn_sigs.
        let source = "fn greet(name: string) -> string { name }\n";
        let uri = "file:///greet.hew";
        let db = InMemorySourceDatabase::new();
        db.set_source(uri.to_string(), source.to_string(), 1);

        // Find the offset of `greet` at the declaration site.
        let greet_offset = source.find("greet").unwrap();

        let via_db = super::hover_via_db(&db, uri, greet_offset)
            .expect("hover_via_db must produce a result for a known function name");

        // The direct path must produce the same rendering when given the
        // same inputs the database produced — proves the DB-backed route is
        // semantically identical to the legacy path.
        let parse_result = db.parse(uri).unwrap();
        let type_check = db.type_check(uri);
        let direct = super::hover(source, &parse_result, type_check.as_deref(), greet_offset)
            .expect("direct hover must agree with hover_via_db for this offset");

        assert_eq!(via_db.contents, direct.contents);
        assert_eq!(via_db.span, direct.span);
        // The function signature for `greet` must appear in the rendered
        // hover, so the test is not trivially satisfied by two empty
        // strings.
        assert!(
            via_db.contents.contains("greet"),
            "expected function signature to mention the function name; got {}",
            via_db.contents
        );
    }
}
