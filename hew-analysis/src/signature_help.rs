//! Signature help analysis: determine the active function call and parameter at a cursor position.

use hew_types::check::FnSig;
use hew_types::TypeCheckOutput;

use crate::{ParameterInfo, SignatureHelpResult, SignatureInfo};

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
    let (fn_name, active_param) = find_call_context(source, offset)?;
    let sig = find_fn_sig(&fn_name, tc)?;
    let label = format_sig_label(&fn_name, sig);

    // Build parameter infos with byte-offset ranges within the label string.
    let mut params = Vec::new();
    for (name, ty) in sig.param_names.iter().zip(&sig.params) {
        let param_text = format!("{name}: {ty}");
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
        active_parameter: Some(active_param as u32),
    })
}

/// Find the function name and active parameter index at the cursor offset.
fn find_call_context(source: &str, offset: usize) -> Option<(String, usize)> {
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
                    let fn_name = extract_fn_name_before(source, i)?;
                    return Some((fn_name, comma_count));
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
fn extract_fn_name_before(source: &str, paren_pos: usize) -> Option<String> {
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

    Some(before[start..end].to_string())
}

/// Find a function signature by name, checking `fn_sigs`, `type_defs` methods, and qualified names.
fn find_fn_sig<'a>(name: &str, tc: &'a TypeCheckOutput) -> Option<&'a FnSig> {
    if let Some(sig) = tc.fn_sigs.get(name) {
        return Some(sig);
    }

    // For method calls like `receiver.method`, try the method part.
    if let Some(method) = name.rsplit('.').next() {
        if method != name {
            for type_def in tc.type_defs.values() {
                if let Some(sig) = type_def.methods.get(method) {
                    return Some(sig);
                }
            }
            for (sig_name, sig) in &tc.fn_sigs {
                if sig_name.ends_with(&format!("::{method}")) {
                    return Some(sig);
                }
            }
        }
    }

    // Try just the last component as a plain function name.
    let last = name.rsplit(['.', ':']).find(|s| !s.is_empty())?;
    if last != name {
        if let Some(sig) = tc.fn_sigs.get(last) {
            return Some(sig);
        }
    }

    None
}

/// Format signature label like `fn name(param1: Type, param2: Type) -> RetType`.
fn format_sig_label(name: &str, sig: &FnSig) -> String {
    let params: Vec<String> = sig
        .param_names
        .iter()
        .zip(&sig.params)
        .map(|(n, t)| format!("{n}: {t}"))
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
    use hew_types::Ty;
    use std::collections::{HashMap, HashSet};

    fn make_tc_with_fn(name: &str, param_names: Vec<&str>, params: Vec<Ty>, ret: Ty) -> TypeCheckOutput {
        let sig = FnSig {
            type_params: vec![],
            type_param_bounds: HashMap::new(),
            param_names: param_names.into_iter().map(String::from).collect(),
            params,
            return_type: ret,
            is_async: false,
            is_pure: false,
            accepts_kwargs: false,
            doc_comment: None,
        };
        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(name.to_string(), sig);
        TypeCheckOutput {
            expr_types: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs,
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
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
}
