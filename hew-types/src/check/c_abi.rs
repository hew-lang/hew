//! Source C exports use ordinary C values and require explicit record layouts.
use super::{Checker, FnDecl, Span, Ty, TypeErrorKind};
use crate::{DefId, NominalId, TypeHead};
use hew_parser::ast::TypeDecl;

impl Checker {
    fn c_value_type(&self, ty: &Ty) -> bool {
        match ty {
            Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::Isize
            | Ty::Usize
            | Ty::F32
            | Ty::F64
            | Ty::Bool => true,
            Ty::Pointer { pointee, .. } => **pointee == Ty::Unit || self.c_value_type(pointee),
            Ty::Named {
                head: TypeHead::Nominal(head),
                args,
            } => args.is_empty() && self.c_abi.records.contains(&head.id),
            _ => false,
        }
    }

    pub(super) fn check_c_record(&mut self, record: &TypeDecl, span: &Span) {
        let Some(id) = self
            .type_declaration_identity(record)
            .map(NominalId::of_declaration)
        else {
            return;
        };
        let Some(definition) = self.type_defs.get(&id) else {
            return;
        };
        let invalid: Vec<_> = definition
            .field_order
            .iter()
            .filter_map(|name| {
                let ty = &definition.fields[name];
                (!self.c_value_type(ty)).then(|| {
                    format!(
                        "field `{name}: {}` has no explicit C representation",
                        ty.user_facing()
                    )
                })
            })
            .collect();
        for message in invalid {
            self.report_error(TypeErrorKind::InvalidOperation, span, message);
        }
    }

    pub(super) fn check_c_export(&mut self, function: &FnDecl, declaration: DefId) {
        let mut attrs = function
            .attributes
            .iter()
            .filter(|attr| attr.name == "export");
        let Some(attr) = attrs.next() else { return };
        let symbol = attr
            .args
            .first()
            .map_or(function.name.name.as_str(), |arg| arg.as_str());
        let failure = if attrs.next().is_some() {
            Some("use one #[export] attribute per function".into())
        } else if !crate::c_abi::is_c_export_identifier(symbol) {
            Some(format!(
                "C export `{symbol}` requires a non-reserved ASCII C identifier"
            ))
        } else if function.type_params.is_some() || function.is_generator {
            Some("C exports require a monomorphic ordinary function".into())
        } else if self
            .c_abi
            .exports
            .iter()
            .any(|(other, name)| *other != declaration && name == symbol)
            || self
                .source_extern_declarations
                .iter()
                .any(|row| row.symbol == symbol)
        {
            Some(format!("C export symbol `{symbol}` is already declared"))
        } else {
            let Some(sig) = self.fn_sigs.get(&declaration) else {
                return;
            };
            sig.params.iter().map(|p| &p.ty).chain(
                (sig.return_type != Ty::Unit).then_some(&sig.return_type)
            ).find(|ty| !self.c_value_type(ty)).map(|ty| format!(
                "C export `{symbol}` cannot represent `{}`; use a C scalar, raw pointer or #[repr(C)] record", ty.user_facing()
            ))
        };
        if let Some(message) = failure {
            self.report_error(TypeErrorKind::InvalidOperation, &attr.span, message);
        } else {
            self.c_abi.exports.insert(declaration, symbol.to_owned());
        }
    }
}
