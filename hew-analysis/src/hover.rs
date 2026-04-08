//! Hover analysis: produce rich hover information for identifiers and expressions.

use hew_types::check::{FnSig, SpanKey, TypeDef, TypeDefKind};
use hew_types::method_resolution;
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
        if let Some(type_def) =
            method_resolution::lookup_type_def(&type_output.type_defs, word.as_str())
        {
            let hover_text = format_type_def_hover(&type_def);
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
            let param_list: Vec<String> = params
                .iter()
                .map(|ty| ty.user_facing().to_string())
                .collect();
            format!(
                "```hew\nfn {snippet}({}) -> {}\n```",
                param_list.join(", "),
                ret.user_facing()
            )
        } else {
            format!("```hew\n{snippet}: {}\n```", ty.user_facing())
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
    let params: Vec<String> = sig
        .params
        .iter()
        .map(|ty| ty.user_facing().to_string())
        .collect();
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
    let params: Vec<String> = sig
        .params
        .iter()
        .map(|ty| ty.user_facing().to_string())
        .collect();
    format_fn_sig_line(name, &params, sig)
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
    use std::collections::{HashMap, HashSet};

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
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs,
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
        }
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
        assert!(text.contains("fn factorial("));
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
            hr.contents.contains("fn add("),
            "should show function signature"
        );
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
            errors: vec![],
            warnings: vec![],
            type_defs,
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
        };
        let offset = source.find("Point").unwrap();
        let result = hover(source, &pr, Some(&tc), offset);
        assert!(result.is_some(), "should find hover for Point");
        let hr = result.unwrap();
        assert!(hr.contents.contains("type Point"));
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
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
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
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
        };

        let result = hover(source, &pr, Some(&tc), count_offset).unwrap();
        assert!(result.contents.contains("count: int"));
        assert!(!result.contents.contains("i64"));
    }
}
