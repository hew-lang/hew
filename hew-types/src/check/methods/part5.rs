//! Split from `methods.rs`: checker methods, part 5 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::check::calls::SignatureArgApplication;
use crate::check::dispatch::resolve_method_call;
use crate::check::types::GenericCallee;
use crate::check::types::{BareActorResolution, DeferredBuiltinCloneAdmission, DeferredWireCodec};
use crate::method_resolution::{
    collect_method_sigs_for_receiver, instantiate_stdlib_method_sig, lookup_builtin_method_sig,
    lookup_named_method_sig as shared_lookup_named_method_sig,
};
use crate::runtime_call::{FloatMethodOp, IntArithKind, IntBitOp, IntMethodWidth};
use crate::stdlib::{STD_NET_CONNECTION, STD_NET_LISTENER};
use crate::BuiltinType;

impl Checker {
    pub(super) fn report_missing_method_with_shadow_note(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        span: &Span,
        message: String,
    ) {
        let mut error = TypeError::new(TypeErrorKind::UndefinedMethod, span.clone(), message);
        if let Expr::Identifier(binding) = &receiver.0 {
            if self.env.lookup_ref(binding).is_some()
                && self.module_import_bindings.contains_key(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    binding.clone(),
                ))
            {
                error = error.with_note(
                    receiver.1.clone(),
                    format!(
                        "lexical binding `{binding}` shadows the imported module; `{binding}.{method}` was resolved as a value method lookup"
                    ),
                );
            }
        }
        self.errors.push(error);
    }
}
