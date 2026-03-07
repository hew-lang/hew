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
