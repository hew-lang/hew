//! Signature help analysis: determine the active function call and parameter at a cursor position.

use hew_types::check::FnSig;
use hew_types::TypeCheckOutput;

use crate::method_lookup::{find_receiver_type, lookup_method_sig as lookup_receiver_method_sig};
use crate::{ParameterInfo, SignatureHelpResult, SignatureInfo};

struct CallContext {
    callee: String,
    receiver_end: Option<usize>,
    /// Source text of each argument up to the cursor; the last is the one
    /// the cursor is in.
    args: Vec<String>,
}

impl CallContext {
    /// The parameter the cursor's argument fills: the one it names, or its
    /// position when it names none.
    fn active_param(&self, sig: &FnSig) -> usize {
        let current = self.args.last().map_or("", String::as_str);
        argument_label(current)
            .and_then(|name| sig.param_names.iter().position(|param| param == name))
            .unwrap_or(self.args.len().saturating_sub(1))
    }
}

/// The parameter name an argument's text starts with, as in `timeout: 5s`.
fn argument_label(text: &str) -> Option<&str> {
    let text = text.trim_start();
    let end = text
        .find(|c: char| !(c.is_alphanumeric() || c == '_'))
        .unwrap_or(text.len());
    let rest = text[end..].trim_start();
    (end > 0 && rest.starts_with(':') && !rest.starts_with("::")).then(|| &text[..end])
}

/// `name: ` completions for the parameters the call under the cursor has not
/// yet supplied, offered where an argument starts.
#[must_use]
pub fn named_argument_completions(
    source: &str,
    tc: &TypeCheckOutput,
    offset: usize,
) -> Vec<crate::CompletionItem> {
    let Some(context) = find_call_context(source, offset) else {
        return Vec::new();
    };
    let current = context.args.last().map_or("", String::as_str).trim_start();
    if !current.chars().all(|c| c.is_alphanumeric() || c == '_') {
        return Vec::new();
    }
    let Some(sig) = find_call_sig(&context, tc) else {
        return Vec::new();
    };
    let earlier = &context.args[..context.args.len() - 1];
    let positional = earlier
        .iter()
        .take_while(|arg| argument_label(arg).is_none())
        .count();
    let named: Vec<&str> = earlier
        .iter()
        .filter_map(|arg| argument_label(arg))
        .collect();
    sig.param_names
        .iter()
        .skip(positional)
        .filter(|name| !name.starts_with('_') && !named.contains(&name.as_str()))
        .map(|name| crate::CompletionItem {
            label: format!("{name}:"),
            kind: crate::CompletionKind::Variable,
            detail: Some("named argument".to_string()),
            documentation: None,
            insert_text: Some(format!("{name}: ")),
            insert_text_is_snippet: false,
            sort_text: Some(format!("0_{name}")),
        })
        .collect()
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
    let active_param = context.active_param(&sig);
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
            documentation: sig.doc_comment.clone(),
            parameters: params,
        }],
        active_signature: Some(0),
        active_parameter: Some(active_param as u32),
    })
}

fn find_call_sig(context: &CallContext, tc: &TypeCheckOutput) -> Option<FnSig> {
    if let Some(sig) = find_exact_fn_sig(&context.callee, tc) {
        return Some(sig);
    }

    if let Some(sig) = find_receiver_method_sig(context, tc) {
        return Some(sig);
    }

    // A module-qualified free call (`unicode.is_upper(cp)`) reaches here: it is
    // not the exact key, and its prefix is an import binding rather than a
    // value, so it has no receiver type. Resolve the binding to the module it
    // names and ask for the canonical identity. Ordered AFTER the receiver path
    // so a local value shadowing an import binding still resolves as the method
    // call it is.
    if let Some(sig) = find_module_qualified_fn_sig(&context.callee, tc) {
        return Some(sig);
    }

    if context.receiver_end.is_none() {
        return find_fallback_fn_sig(&context.callee, tc);
    }

    None
}

/// Resolve `<binding>.<fn>` through the importer's own binding table.
///
/// `fn_sigs` is keyed by the declaring module's canonical path
/// (`std.text.unicode.is_upper`), while the source spells the lexical import
/// binding (`unicode.is_upper`). `module_import_bindings` is the checker's
/// record of exactly which module each binding names, so the surface spelling
/// resolves to one identity without the LSP inventing an owner or scanning
/// `fn_sigs` for a matching leaf.
///
/// Keyed on the root importer (`None`): the analyzed document is the
/// compilation root. A miss returns `None`; no owner is guessed.
fn find_module_qualified_fn_sig(callee: &str, tc: &TypeCheckOutput) -> Option<FnSig> {
    let (binding, leaf) = callee.rsplit_once('.')?;
    if binding.is_empty() || leaf.is_empty() {
        return None;
    }
    let owner = tc
        .module_import_bindings
        .get(&(None, 0, binding.to_string()))?;
    tc.sigs().get(&format!("{owner}.{leaf}")).cloned()
}

/// Find the call the cursor is inside and its arguments up to the cursor.
fn find_call_context(source: &str, offset: usize) -> Option<CallContext> {
    let bytes = &source.as_bytes()[..offset];
    let mut depth: i32 = 0;
    let mut commas = Vec::new();
    let mut i = bytes.len();

    while i > 0 {
        i -= 1;
        match bytes[i] {
            b')' | b']' | b'}' => depth += 1,
            b'(' => {
                if depth == 0 {
                    let (callee, receiver_end) = extract_fn_name_before(source, i)?;
                    let bounds: Vec<usize> = std::iter::once(i)
                        .chain(commas.into_iter().rev())
                        .chain(std::iter::once(offset))
                        .collect();
                    let args = bounds
                        .windows(2)
                        .map(|pair| source[pair[0] + 1..pair[1]].to_string())
                        .collect();
                    return Some(CallContext {
                        callee,
                        receiver_end,
                        args,
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
            b',' if depth == 0 => commas.push(i),
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
    let method = hew_types::short_name(&context.callee);
    let receiver_ty = find_receiver_type(tc, receiver_end)?;
    lookup_receiver_method_sig(tc, receiver_ty, method)
}

/// Find a function signature by name via qualified-name fallbacks.
fn find_fallback_fn_sig(name: &str, tc: &TypeCheckOutput) -> Option<FnSig> {
    // Try just the last component as a plain function name.
    let last = name.rsplit(['.', ':']).find(|s| !s.is_empty())?;
    if last != name {
        // Root free functions publish under their canonical `{root}.{name}`
        // key; the leaf resolves through the checker's declaration table
        // before the bare rung, which remains the builtin/extern floor.
        if let Some(sig) = find_root_fn_sig(last, tc) {
            return Some(sig);
        }
        if let Some(sig) = tc.sigs().get(last) {
            return Some(sig.clone());
        }
        for (sig_name, sig) in tc.sigs().entries() {
            if sig_name.ends_with(&format!("::{last}")) {
                return Some(sig.clone());
            }
        }
    }

    None
}

fn find_exact_fn_sig(name: &str, tc: &TypeCheckOutput) -> Option<FnSig> {
    if let Some(sig) = find_root_fn_sig(name, tc) {
        return Some(sig);
    }
    tc.sigs().get(name).cloned()
}

/// Signature of a bare free-function spelling declared by the ROOT unit.
///
/// The checker publishes root free functions under `{root}.{name}` and
/// records that declaration in its identity table. Resolve the spelling
/// through that table rather than reconstructing the key: a bare spelling
/// that names no root declaration yields `None` and the caller falls
/// through to the builtin/extern rung.
fn find_root_fn_sig(name: &str, tc: &TypeCheckOutput) -> Option<FnSig> {
    if name.contains('.') || name.contains("::") {
        return None;
    }
    let root = tc.defs.root_module_path()?;
    let declaration = tc.defs.lookup_path(&format!("{root}.{name}"))?;
    tc.sigs().get(tc.defs.path(declaration)).cloned()
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

    fn make_tc_with_fn_sigs(fn_sigs: HashMap<String, FnSig>) -> TypeCheckOutput {
        let fn_sig_parts = hew_types::check::FnSigFixture::new(fn_sigs).into_parts();
        TypeCheckOutput {
            expr_types: HashMap::new(),
            resolved_expr_types: HashMap::new(),
            is_type_patterns: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            internal_builtin_enum_names: std::collections::HashSet::new(),
            fn_sigs: fn_sig_parts.0,
            fn_sig_keys: fn_sig_parts.1,
            builtin_fn_sigs: fn_sig_parts.2,
            root_value_bindings: HashSet::new(),
            handle_bearing_structs: std::collections::HashSet::new(),
            method_call_consumes_receiver: HashSet::new(),
            method_call_preserves_receiver_identity: HashSet::new(),
            opaque_resource_candidates: hew_types::check::OpaqueResourceCandidateGraph::default(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            record_init_type_args: HashMap::new(),
            intrinsic_declarations: HashMap::new(),
            stack_hints: Vec::new(),
            actor_handler_state_guards: HashMap::new(),
            actor_max_heap: HashMap::new(),
            supervisor_child_slots: HashMap::new(),
            pool_accessor_sites: HashMap::new(),
            dyn_trait_coercions: HashMap::new(),
            dyn_trait_method_calls: HashMap::new(),
            closure_capture_facts: std::collections::HashMap::new(),
            closure_escape_facts: std::collections::HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            lowering_facts: HashMap::new(),
            method_call_rewrites: HashMap::new(),
            wire_layouts: HashMap::new(),
            width_cast_lowerings: HashMap::new(),
            try_width_cast_lowerings: HashMap::new(),
            actor_method_dispatch: HashMap::new(),
            actor_protocol_descriptors: HashMap::new(),
            machine_method_dispatch: HashMap::new(),
            tail_ok_coercions: std::collections::HashSet::new(),
            pattern_resolutions: HashMap::new(),
            pattern_plans: HashMap::new(),
            lang_items: hew_types::LangItemRegistry::new(),
            resolved_calls: HashMap::new(),
            vec_generic_element_abi: HashMap::new(),
            user_clone_record_seeds: vec![],
            import_type_name_aliases: HashMap::new(),
            ..TypeCheckOutput::default()
        }
    }

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
        make_tc_with_fn_sigs(fn_sigs)
    }

    /// Build a `TypeCheckOutput` with specific `fn_sigs`, `type_defs`, and
    /// `expr_types`; all other fields are defaulted via `make_tc_with_fn_sigs`.
    fn make_tc_with_fields(
        fn_sigs: HashMap<String, FnSig>,
        type_defs: HashMap<hew_types::NominalId, hew_types::check::TypeDef>,
        expr_types: HashMap<hew_types::check::SpanKey, Ty>,
    ) -> TypeCheckOutput {
        let mut tc = make_tc_with_fn_sigs(fn_sigs);
        tc.type_defs = type_defs;
        tc.expr_types = expr_types;
        tc
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
    fn sig_help_follows_a_named_argument() {
        let source = "span(end: 9, start: ";
        let tc = make_tc_with_fn(
            "span",
            vec!["start", "end"],
            vec![Ty::I64, Ty::I64],
            Ty::String,
        );
        let sh = build_signature_help(source, &tc, source.len()).expect("signature help");
        assert_eq!(
            sh.active_parameter,
            Some(0),
            "`start:` names the first parameter"
        );
        let sh = build_signature_help(source, &tc, "span(end: ".len()).expect("signature help");
        assert_eq!(
            sh.active_parameter,
            Some(1),
            "`end:` names the second parameter"
        );
    }

    #[test]
    fn named_argument_completions_offer_unsupplied_parameters() {
        let tc = make_tc_with_fn(
            "connect",
            vec!["host", "port", "timeout", "_trace"],
            vec![Ty::String, Ty::I64, Ty::I64, Ty::Bool],
            Ty::Unit,
        );
        let source = "connect(\"db\", timeout: 5, ";
        let labels: Vec<String> = named_argument_completions(source, &tc, source.len())
            .into_iter()
            .map(|item| item.label)
            .collect();
        assert_eq!(
            labels,
            vec!["port:"],
            "host is positional, timeout named, _trace hidden"
        );
        let inside_value = "connect(\"db\", timeout: 5";
        assert!(
            named_argument_completions(inside_value, &tc, inside_value.len()).is_empty(),
            "no labels are offered inside an argument's value"
        );
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
        assert_eq!(result.signatures[0].label, "fn sum(value: i64) -> i64");
    }

    /// Signature help must label an `impl`-block method on a named type with
    /// its full parameter list and return type. This is the analysis-layer
    /// coverage the LSP relies on for imported stdlib method surfaces once the
    /// module is inlined (e.g. regex `captures(input: string) -> CaptureMatches`).
    #[test]
    fn sig_help_labels_impl_block_method_with_return_type() {
        let source = "\
type Caps { count: i64, }
type Matcher { id: i64, }
trait MatcherMethods {
    fn captures(self, input: string) -> Caps;
}
impl MatcherMethods for Matcher {
    fn captures(m: Matcher, input: string) -> Caps { Caps { count: 0 } }
}
fn probe(mat: Matcher, s: string) {
    let c = mat.captures(s);
}
";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut checker = hew_types::Checker::new(registry);
        let tc = checker.check_program(&parse_result.program);
        assert!(
            !tc.errors
                .iter()
                .any(|e| e.severity == hew_types::error::Severity::Error),
            "fixture must type-check cleanly: {:?}",
            tc.errors
        );
        let call = source.find("mat.captures").expect("call present");
        let paren = call + source[call..].find('(').unwrap() + 1;
        let sh = build_signature_help(source, &tc, paren)
            .expect("signature help should resolve the impl-block method");
        assert_eq!(
            sh.signatures[0].label, "fn captures(input: string) -> Caps",
            "signature label should include the param list and return type",
        );
    }

    #[test]
    fn module_qualified_function_sig_help_prefers_exact_dotted_name_over_receiver_method_fallback()
    {
        let source = "stream.pipe(";

        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(
            "stream.pipe".to_string(),
            FnSig {
                param_names: vec!["capacity".to_string()],
                params: vec![Ty::I64],
                return_type: Ty::Unit,
                ..FnSig::default()
            },
        );

        let mut type_defs = HashMap::new();
        type_defs.insert(
            hew_types::NominalId::for_test("StreamModule"),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "StreamModule".to_string(),
                type_params: vec![],
                bounds: HashMap::new(),
                fields: HashMap::new(),
                field_order: vec![],
                variants: HashMap::new(),
                methods: HashMap::from([(
                    "pipe".to_string(),
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
            SpanKey {
                start: 0,
                end: 6,
                module_idx: 0,
            },
            Ty::named_for_test("StreamModule", vec![]),
        );

        let tc = make_tc_with_fields(fn_sigs, type_defs, expr_types);

        let result = build_signature_help(source, &tc, source.len());
        assert!(
            result.is_some(),
            "module-qualified function call should still provide signature help"
        );
        let sh = result.unwrap();
        assert_eq!(sh.active_parameter, Some(0));
        assert_eq!(sh.signatures[0].label, "fn pipe(capacity: i64)");
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
            hew_types::NominalId::for_test("Widget"),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Widget".to_string(),
                type_params: vec![],
                bounds: HashMap::new(),
                fields: HashMap::new(),
                field_order: vec![],
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            },
        );

        let mut expr_types = HashMap::new();
        expr_types.insert(
            SpanKey {
                start: 0,
                end: 5,
                module_idx: 0,
            },
            Ty::named_for_test("Widget", vec![]),
        );

        let tc = make_tc_with_fields(fn_sigs, type_defs, expr_types);

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

    #[test]
    fn sig_help_carries_doc_comment() {
        // A function with a doc comment must surface that comment on the
        // signature's `documentation` field.
        let sig = FnSig {
            param_names: vec!["x".to_string()],
            params: vec![Ty::I32],
            return_type: Ty::I32,
            doc_comment: Some("Returns the square of x.".to_string()),
            ..FnSig::default()
        };
        let mut fn_sigs = HashMap::new();
        fn_sigs.insert("square".to_string(), sig);
        let tc = make_tc_with_fn_sigs(fn_sigs);
        let source = "square(";
        let result = build_signature_help(source, &tc, source.len());
        assert!(result.is_some(), "expected signature help");
        let sh = result.unwrap();
        assert_eq!(
            sh.signatures[0].documentation.as_deref(),
            Some("Returns the square of x."),
            "signature info should carry the doc comment; got: {:?}",
            sh.signatures[0].documentation
        );
    }

    #[test]
    fn sig_help_without_doc_comment_has_none_documentation() {
        // A function without a doc comment must yield `None` — not an empty string.
        let source = "add(";
        let tc = make_tc_with_fn("add", vec!["a", "b"], vec![Ty::I32, Ty::I32], Ty::I32);
        let result = build_signature_help(source, &tc, source.len());
        assert!(result.is_some());
        let sh = result.unwrap();
        assert_eq!(
            sh.signatures[0].documentation, None,
            "function without doc comment must have None documentation in sig help"
        );
    }
}
