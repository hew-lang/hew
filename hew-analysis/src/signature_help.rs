//! Signature help analysis: determine the active function call and parameter at a cursor position.

use hew_types::check::FnSig;
use hew_types::TypeCheckOutput;

use crate::method_lookup::{find_receiver_type, lookup_method_sig as lookup_receiver_method_sig};
use crate::{ParameterInfo, SignatureHelpResult, SignatureInfo};

struct CallContext {
    callee: String,
    receiver_end: Option<usize>,
    active_param: usize,
}

/// Build signature help at the given byte offset within `source`.
///
/// Returns a `SignatureHelpResult` with parameter label offsets, or `None` if the
/// cursor is not inside a recognised function call.
#[expect(
    clippy::cast_possible_truncation,
    reason = "active parameter index and label offsets fit in u32"
)]
#[must_use]
pub fn build_signature_help(
    source: &str,
    tc: &TypeCheckOutput,
    offset: usize,
) -> Option<SignatureHelpResult> {
    let context = find_call_context(source, offset)?;
    let sig = find_call_sig(&context, tc)?;
    let label = format_sig_label(&context.callee, &sig);

    // Build parameter infos with byte-offset ranges within the label string.
    let mut params = Vec::new();
    for (name, ty) in sig.param_names.iter().zip(&sig.params) {
        let param_text = format!("{name}: {}", ty.user_facing());
        if let Some(start) = label.find(&param_text) {
            params.push(ParameterInfo {
                label_start: start as u32,
                label_end: (start + param_text.len()) as u32,
            });
        }
    }

    Some(SignatureHelpResult {
        signatures: vec![SignatureInfo {
            label,
            parameters: params,
        }],
        active_signature: Some(0),
        active_parameter: Some(context.active_param as u32),
    })
}

fn find_call_sig(context: &CallContext, tc: &TypeCheckOutput) -> Option<FnSig> {
    if let Some(sig) = find_exact_fn_sig(&context.callee, tc) {
        return Some(sig);
    }

    if let Some(sig) = find_receiver_method_sig(context, tc) {
        return Some(sig);
    }

    if context.receiver_end.is_none() {
        return find_fallback_fn_sig(&context.callee, tc);
    }

    None
}

/// Find the function name and active parameter index at the cursor offset.
fn find_call_context(source: &str, offset: usize) -> Option<CallContext> {
    let bytes = &source.as_bytes()[..offset];
    let mut depth: i32 = 0;
    let mut comma_count: usize = 0;
    let mut i = bytes.len();

    while i > 0 {
        i -= 1;
        match bytes[i] {
            b')' | b']' | b'}' => depth += 1,
            b'(' => {
                if depth == 0 {
                    let (callee, receiver_end) = extract_fn_name_before(source, i)?;
                    return Some(CallContext {
                        callee,
                        receiver_end,
                        active_param: comma_count,
                    });
                }
                depth -= 1;
            }
            b'[' | b'{' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            b',' if depth == 0 => comma_count += 1,
            _ => {}
        }
    }
    None
}

/// Extract the function/method name immediately before the `(` at `paren_pos`.
fn extract_fn_name_before(source: &str, paren_pos: usize) -> Option<(String, Option<usize>)> {
    let before = source[..paren_pos].trim_end();
    if before.is_empty() {
        return None;
    }
    let bytes = before.as_bytes();
    let end = bytes.len();
    let mut start = end;

    while start > 0 {
        let ch = bytes[start - 1];
        if ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'.' || ch == b':' {
            start -= 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    let callee = before[start..end].to_string();
    let receiver_end = callee.rfind('.').map(|dot_pos| start + dot_pos);

    Some((callee, receiver_end))
}

fn find_receiver_method_sig(context: &CallContext, tc: &TypeCheckOutput) -> Option<FnSig> {
    let receiver_end = context.receiver_end?;
    let method = context.callee.rsplit('.').next()?;
    let receiver_ty = find_receiver_type(tc, receiver_end)?;
    lookup_receiver_method_sig(tc, receiver_ty, method)
}

/// Find a function signature by name via qualified-name fallbacks.
fn find_fallback_fn_sig(name: &str, tc: &TypeCheckOutput) -> Option<FnSig> {
    // Try just the last component as a plain function name.
    let last = name.rsplit(['.', ':']).find(|s| !s.is_empty())?;
    if last != name {
        if let Some(sig) = tc.fn_sigs.get(last) {
            return Some(sig.clone());
        }
        for (sig_name, sig) in &tc.fn_sigs {
            if sig_name.ends_with(&format!("::{last}")) {
                return Some(sig.clone());
            }
        }
    }

    None
}

fn find_exact_fn_sig(name: &str, tc: &TypeCheckOutput) -> Option<FnSig> {
    tc.fn_sigs.get(name).cloned()
}

/// Format signature label like `fn name(param1: Type, param2: Type) -> RetType`.
fn format_sig_label(name: &str, sig: &FnSig) -> String {
    let params: Vec<String> = sig
        .param_names
        .iter()
        .zip(&sig.params)
        .map(|(n, t)| format!("{n}: {}", t.user_facing()))
        .collect();
    let display_name = name
        .rsplit(['.', ':'])
        .find(|s| !s.is_empty())
        .unwrap_or(name);
    crate::hover::format_fn_sig_line(display_name, &params, sig)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_types::check::{SpanKey, TypeDef, TypeDefKind};
    use hew_types::Ty;
    use std::collections::{HashMap, HashSet};

    fn make_tc_with_fn(
        name: &str,
        param_names: Vec<&str>,
        params: Vec<Ty>,
        ret: Ty,
    ) -> TypeCheckOutput {
        let sig = FnSig {
            param_names: param_names.into_iter().map(String::from).collect(),
            params,
            return_type: ret,
            ..FnSig::default()
        };
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
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        }
    }

    #[test]
    fn sig_help_first_param() {
        // Cursor right after the opening paren — first parameter active
        let source = "greet(";
        let tc = make_tc_with_fn("greet", vec!["name"], vec![Ty::String], Ty::Unit);
        let result = build_signature_help(source, &tc, source.len());
        assert!(result.is_some(), "should provide signature help");
        let sh = result.unwrap();
        assert_eq!(sh.active_parameter, Some(0));
        assert_eq!(sh.signatures.len(), 1);
        assert!(sh.signatures[0].label.contains("fn greet("));
    }

    #[test]
    fn sig_help_second_param() {
        // After the first comma — second parameter active
        let source = "add(1, ";
        let tc = make_tc_with_fn("add", vec!["x", "y"], vec![Ty::I32, Ty::I32], Ty::I32);
        let result = build_signature_help(source, &tc, source.len());
        assert!(result.is_some());
        let sh = result.unwrap();
        assert_eq!(sh.active_parameter, Some(1));
    }

    #[test]
    fn no_sig_help_outside_call() {
        let source = "let x = 42";
        let tc = make_tc_with_fn("greet", vec!["name"], vec![Ty::String], Ty::Unit);
        let result = build_signature_help(source, &tc, source.len());
        assert!(result.is_none(), "no signature help outside function call");
    }

    #[test]
    fn sig_help_with_nested_parens() {
        // Inner call is closed — cursor is in the outer call's second parameter
        let source = "add(inner(1), ";
        let tc = make_tc_with_fn("add", vec!["x", "y"], vec![Ty::I32, Ty::I32], Ty::I32);
        let result = build_signature_help(source, &tc, source.len());
        assert!(result.is_some());
        let sh = result.unwrap();
        assert_eq!(sh.active_parameter, Some(1));
    }

    #[test]
    fn sig_help_parameter_labels_have_offsets() {
        let source = "greet(";
        let tc = make_tc_with_fn(
            "greet",
            vec!["name", "age"],
            vec![Ty::String, Ty::I32],
            Ty::Unit,
        );
        let result = build_signature_help(source, &tc, source.len());
        assert!(result.is_some());
        let sh = result.unwrap();
        let sig = &sh.signatures[0];
        assert!(
            !sig.parameters.is_empty(),
            "should have parameter info entries"
        );
        for param in &sig.parameters {
            assert!(
                param.label_end > param.label_start,
                "parameter label should have non-zero length"
            );
        }
    }

    #[test]
    fn sig_help_uses_int_alias() {
        let source = "sum(";
        let tc = make_tc_with_fn("sum", vec!["value"], vec![Ty::I64], Ty::I64);
        let result = build_signature_help(source, &tc, source.len()).unwrap();
        assert_eq!(result.signatures[0].label, "fn sum(value: int) -> int");
    }

    #[test]
    fn module_qualified_function_sig_help_prefers_exact_dotted_name_over_receiver_method_fallback()
    {
        let source = "channel.new(";
        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(
            "channel.new".to_string(),
            FnSig {
                param_names: vec!["capacity".to_string()],
                params: vec![Ty::I64],
                return_type: Ty::Unit,
                ..FnSig::default()
            },
        );

        let mut type_defs = HashMap::new();
        type_defs.insert(
            "ChannelModule".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "ChannelModule".to_string(),
                type_params: vec![],
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::from([(
                    "new".to_string(),
                    FnSig {
                        param_names: vec!["count".to_string()],
                        params: vec![Ty::I32],
                        return_type: Ty::Unit,
                        ..FnSig::default()
                    },
                )]),
                doc_comment: None,
                is_indirect: false,
            },
        );

        let mut expr_types = HashMap::new();
        expr_types.insert(
            SpanKey { start: 0, end: 7 },
            Ty::Named {
                name: "ChannelModule".to_string(),
                args: vec![],
            },
        );

        let tc = TypeCheckOutput {
            expr_types,
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs,
            fn_sigs,
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        };

        let result = build_signature_help(source, &tc, source.len());
        assert!(
            result.is_some(),
            "module-qualified function call should still provide signature help"
        );
        let sh = result.unwrap();
        assert_eq!(sh.active_parameter, Some(0));
        assert_eq!(sh.signatures[0].label, "fn new(capacity: int)");
    }

    #[test]
    fn method_call_sig_help_does_not_fall_back_to_unrelated_top_level_function() {
        let source = "value.foo(";
        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(
            "foo".to_string(),
            FnSig {
                param_names: vec!["count".to_string()],
                params: vec![Ty::I32],
                return_type: Ty::Unit,
                ..FnSig::default()
            },
        );

        let mut type_defs = HashMap::new();
        type_defs.insert(
            "Widget".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Widget".to_string(),
                type_params: vec![],
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            },
        );

        let mut expr_types = HashMap::new();
        expr_types.insert(
            SpanKey { start: 0, end: 5 },
            Ty::Named {
                name: "Widget".to_string(),
                args: vec![],
            },
        );

        let tc = TypeCheckOutput {
            expr_types,
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs,
            fn_sigs,
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        };

        let result = build_signature_help(source, &tc, source.len());
        assert!(
            result.is_none(),
            "method-call syntax must not fall back to unrelated top-level `foo`"
        );
    }

    #[test]
    fn method_call_sig_help_without_receiver_type_does_not_fall_back_to_unrelated_top_level_function(
    ) {
        let source = "value.foo(";
        let tc = make_tc_with_fn("foo", vec!["count"], vec![Ty::I32], Ty::Unit);

        let result = build_signature_help(source, &tc, source.len());
        assert!(
            result.is_none(),
            "method-call syntax without receiver typing must not fall back to unrelated top-level `foo`"
        );
    }
}
