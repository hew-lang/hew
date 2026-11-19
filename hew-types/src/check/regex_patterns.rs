//! Named regex groups introduce lexical bindings in their match arm.

use super::{
    ArmResolution, Checker, PatternKind, PayloadBinding, Span, SpanKey, Ty, TypeErrorKind,
};

impl Checker {
    pub(super) fn bind_regex_pattern(
        &mut self,
        pattern: &str,
        groups: &[hew_parser::ast::RegexCapture],
        ty: &Ty,
        span: &Span,
    ) {
        if !matches!(ty, Ty::String | Ty::Var(_) | Ty::Error) {
            self.report_error(
                TypeErrorKind::RegexPatternNotString {
                    actual_ty: ty.user_facing().to_string(),
                },
                span,
                format!(
                    "regex pattern requires a string, got `{}`",
                    ty.user_facing()
                ),
            );
        }
        let mut payload_bindings = Vec::new();
        match regex::Regex::new(pattern) {
            Ok(_) => {
                for group in groups {
                    let name = group.name.name.as_str();
                    let tokens = hew_lexer::lex(name);
                    if name == "_"
                        || !matches!(&tokens[..], [(token, token_span)] if token_span.start == 0 && token_span.end == name.len() && hew_parser::parser::Parser::is_ident_token(token))
                    {
                        let error =
                            format!("regex capture `{name}` needs a valid Hew binding name");
                        self.report_error(
                            TypeErrorKind::InvalidRegexLiteral {
                                pattern: pattern.to_owned(),
                                error: error.clone(),
                            },
                            &group.span,
                            error,
                        );
                        continue;
                    }
                    if self.env.lookup_ref(group.name).is_some() {
                        self.report_error(
                            TypeErrorKind::Shadowing,
                            &group.span,
                            format!("regex capture `{name}` conflicts with an existing binding"),
                        );
                        continue;
                    }
                    let capture_ty = if group.required {
                        Ty::String
                    } else {
                        Ty::option(Ty::String)
                    };
                    self.env.define_with_span(
                        group.name,
                        capture_ty.clone(),
                        false,
                        group.span.clone(),
                    );
                    self.record_local_resolution(group.name, &group.span);
                    payload_bindings.push(PayloadBinding {
                        field_idx: group.index as usize,
                        binding_name: name.to_owned(),
                        def_span: Some(group.span.clone()),
                        ty: capture_ty,
                    });
                }
            }
            Err(error) => self.report_error(
                TypeErrorKind::InvalidRegexLiteral {
                    pattern: pattern.to_owned(),
                    error: error.to_string(),
                },
                span,
                format!("invalid regex pattern: {error}"),
            ),
        }
        self.pending_pattern_resolutions.insert(
            SpanKey::in_module(span, self.current_module_idx),
            ArmResolution {
                pattern_kind: PatternKind::Regex,
                variant_match: None,
                payload_bindings,
                payload_variant_patterns: Vec::new(),
            },
        );
    }
}
