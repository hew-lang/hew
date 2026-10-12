use super::super::{Checker, Expr, Literal, Span, SpanKey, Spanned, Ty, TypeErrorKind};
use crate::compile_time::{ConstantValue, InputProvider};
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
            if !matches!(name, "include_str" | "include_bytes") {
                return Err(("E_UNKNOWN_MACRO", format!("unknown macro `{name}!`")));
            }
            let [(Expr::Literal(Literal::String(path)), _)] = args else {
                return Err((
                    "E_MACRO_ARGUMENTS",
                    format!("{name}! expects one string literal path"),
                ));
            };
            let Some(provider) = self.input_provider.as_ref() else {
                return Err((
                    "E_INCLUDE_NOT_FOUND",
                    "compilation inputs are unavailable".into(),
                ));
            };
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
}
