//! Hover analysis: produce rich hover information for identifiers and expressions.

use hew_types::check::{FnSig, SpanKey, TypeDef, TypeDefKind};
use hew_types::{Ty, TypeCheckOutput, VariantDef};

use crate::{HoverResult, OffsetSpan};

/// Compute hover information at the given byte offset.
///
/// Returns a `HoverResult` with markdown contents and an optional span, or
/// `None` if there is nothing meaningful to display at this position.
#[must_use]
pub fn hover(
    source: &str,
    _parse_result: &hew_parser::ParseResult,
    type_output: Option<&TypeCheckOutput>,
    offset: usize,
) -> Option<HoverResult> {
    // Find the word under the cursor for function/type lookup.
    let word = crate::util::word_at_offset(source, offset);

    let type_output = type_output?;

    // Check if the word is a known function — show its full signature.
    if let Some(word) = &word {
        if let Some(sig) = type_output.fn_sigs.get(word.as_str()) {
            let hover_text = format_fn_signature(word, sig);
            return Some(HoverResult {
                contents: hover_text,
                span: None,
            });
        }

        // Check if the word is a known type definition.
        if let Some(type_def) = type_output.type_defs.get(word.as_str()) {
            let hover_text = format_type_def_hover(type_def);
            return Some(HoverResult {
                contents: hover_text,
                span: None,
            });
        }
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
        let value = if let Ty::Function { params, ret } = ty {
            let param_list: Vec<String> = params.iter().map(ToString::to_string).collect();
            format!(
                "```hew\nfn {snippet}({}) -> {ret}\n```",
                param_list.join(", ")
            )
        } else {
            format!("```hew\n{snippet}: {ty}\n```")
        };
        HoverResult {
            contents: value,
            span: Some(OffsetSpan {
                start: span_key.start,
                end: span_key.end,
            }),
        }
    })
}

/// Format a bare function signature line: `[pure] [async] fn name(params)[-> ret]`.
#[must_use]
pub fn format_fn_sig_line(name: &str, params: &[String], sig: &FnSig) -> String {
    let pure_prefix = if sig.is_pure { "pure " } else { "" };
    let async_prefix = if sig.is_async { "async " } else { "" };
    let ret = if sig.return_type == Ty::Unit {
        String::new()
    } else {
        format!(" -> {}", sig.return_type)
    };
    format!(
        "{pure_prefix}{async_prefix}fn {name}({}){ret}",
        params.join(", ")
    )
}

/// Format a function signature in a markdown code block for hover display.
pub fn format_fn_signature(name: &str, sig: &FnSig) -> String {
    let params: Vec<String> = sig.params.iter().map(ToString::to_string).collect();
    let code = format!("```hew\n{}\n```", format_fn_sig_line(name, &params, sig));
    if let Some(doc) = &sig.doc_comment {
        format!("{doc}\n\n---\n\n{code}")
    } else {
        code
    }
}

/// Format a function signature as a single inline line (for embedding in type hover).
pub fn format_fn_signature_inline(name: &str, sig: &FnSig) -> String {
    let params: Vec<String> = sig.params.iter().map(ToString::to_string).collect();
    format_fn_sig_line(name, &params, sig)
}

/// Format a type definition for hover display.
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
            let _ = writeln!(parts, "    {field_name}: {field_ty},");
        }
        for (variant_name, payload) in &type_def.variants {
            match payload {
                VariantDef::Unit => {
                    let _ = writeln!(parts, "    {variant_name},");
                }
                VariantDef::Tuple(types) => {
                    let types: Vec<String> = types.iter().map(ToString::to_string).collect();
                    let _ = writeln!(parts, "    {variant_name}({}),", types.join(", "));
                }
                VariantDef::Struct(fields) => {
                    let fields: Vec<String> =
                        fields.iter().map(|(n, t)| format!("{n}: {t}")).collect();
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
