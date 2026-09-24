//! Checker methods grouped by responsibility: runtime symbol dispatch.
//! Split from `methods.rs`: checker methods, part 1 of 5.
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
    /// Retain scalar collection call metadata. Key admission is decided by the
    /// semantic capability service, independently of target representation.
    pub(in crate::check) fn finalize_lowering_facts(&mut self) -> HashMap<SpanKey, LoweringFact> {
        let pending = std::mem::take(&mut self.pending_lowering_facts);
        let mut result = HashMap::new();
        let mut reported_vars = HashSet::new();
        for (site, fact) in pending {
            let ty = self
                .subst
                .resolve(&fact.hashset_element_ty)
                .materialize_literal_defaults();
            if ty.contains_error() {
                continue;
            }
            let before = self.errors.len();
            if let Ty::Var(var) = &ty {
                if reported_vars.insert(*var) {
                    self.report_error(
                        TypeErrorKind::InferenceFailed,
                        &(site.start..site.end),
                        "cannot infer HashSet element type; add an explicit type annotation".into(),
                    );
                }
            } else {
                self.validate_collection_key_capabilities(&ty, "Set", &(site.start..site.end));
                if let Ok(fact) = LoweringFact::from_hashset_element_type(&ty) {
                    result.insert(site, fact);
                }
            }
            for error in &mut self.errors[before..] {
                error.source_module.clone_from(&fact.source_module);
            }
        }
        result
    }

    pub(in crate::check) fn record_method_call_receiver_kind(
        &mut self,
        span: &Span,
        kind: MethodCallReceiverKind,
    ) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        if matches!(
            self.method_call_receiver_kinds.get(&key),
            Some(MethodCallReceiverKind::LexicalBinding { .. })
        ) {
            return;
        }
        self.method_call_receiver_kinds.insert(key, kind);
    }

    /// Returns true if any trait impl on `type_name` registered a method
    /// named `method` that is in the recognised consume-receiver set.
    ///
    /// Trait methods flatten into the inherent-method table on `T`, so the
    /// dispatch at the named-type
    /// site doesn't carry the originating trait. To honour
    /// `consumes_receiver` declared on the trait, we walk the
    /// `trait_impls_set` for matching `(type, trait)` pairs and check the
    /// qualified `Trait::method` form against the consume set.
    pub(super) fn named_type_method_consumes_receiver(
        &self,
        type_name: &str,
        method: &str,
    ) -> bool {
        if self.consume_receiver_methods.is_empty() {
            return false;
        }
        self.trait_impls_set
            .iter()
            .filter(|(ty, _)| ty == type_name)
            .any(|(_, trait_name)| {
                self.is_consume_receiver_method(&format!("{trait_name}::{method}"))
            })
    }

    /// Returns true when the dispatched call is a `#[resource]` type's inherent
    /// terminal `close(self)` — the implicit-drop dispatch target (W3.030) that
    /// also moves its receiver when called explicitly (#1295).
    ///
    /// The match is precise: the receiver type must carry the `#[resource]`
    /// marker, the method must be the discipline-mandated unit-returning
    /// `close`, and the receiver must be by-value `self` (a `var self` /
    /// mutable-receiver method takes the in-place-mutation path and is NOT an
    /// ownership-transfer move — R4). A `#[resource]` type's `close` is required
    /// to be `fn close(consume self)` by `check_resource_close_discipline`; this guard
    /// keeps the consume marking aligned with that contract.
    pub(super) fn named_type_inherent_close_consumes_receiver(
        &self,
        type_name: &str,
        builtin: Option<BuiltinType>,
        method: &str,
        sig: &FnSig,
    ) -> bool {
        if method != "close" || sig.requires_mutable_receiver {
            return false;
        }
        // Compiler carriers such as `MonitorRef` already carry their exact,
        // shadow-proof identity on the resolved `Ty`.  Use that discriminator
        // instead of asking the name-indexed source registry to rediscover a
        // prelude spelling: imported source declarations are registry-owned,
        // while compiler carriers are catalog-owned.
        if builtin.is_some_and(|kind| kind.close_method() == Some(method)) {
            return true;
        }
        // The trait registry is the single authority for source-declared
        // `#[resource]` facts, keyed by exact declaration identity.
        self.registry.is_resource(type_name)
    }

    pub(in crate::check) fn record_method_call_rewrite(
        &mut self,
        span: &Span,
        rewrite: MethodCallRewrite,
    ) {
        self.method_call_rewrites
            .insert(SpanKey::in_module(span, self.current_module_idx), rewrite);
    }

    pub(in crate::check) fn canonical_handle_receiver_type_name(
        &self,
        receiver_ty: &Ty,
    ) -> Option<String> {
        let Ty::Named { name, .. } = receiver_ty else {
            return None;
        };
        self.module_registry.canonical_handle_type_identity(name)
    }

    pub(super) fn check_primitive_receiver_method_fallback(
        &mut self,
        receiver_ty: &Ty,
        receiver_label: &str,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        if let Some(ret_ty) =
            self.try_dispatch_primitive_trait_method(receiver_ty, method, args, span)
        {
            return ret_ty;
        }
        for arg in args {
            let (expr, sp) = arg.expr();
            self.synthesize(expr, sp);
        }
        self.report_error(
            TypeErrorKind::UndefinedMethod,
            span,
            format!("no method `{method}` on {receiver_label}"),
        );
        Ty::Error
    }

    pub(super) fn missing_builtin_contract_error(
        &mut self,
        span: &Span,
        builtin: &str,
        method: &str,
        item: &str,
    ) {
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "internal compiler error: builtin {builtin}.{method} is missing {item} metadata"
            ),
        );
    }

    pub(super) fn require_builtin_runtime_symbol(
        &mut self,
        span: &Span,
        builtin: &str,
        method: &str,
        symbol: Option<&'static str>,
    ) -> Option<&'static str> {
        symbol.or_else(|| {
            self.missing_builtin_contract_error(span, builtin, method, "runtime rewrite");
            None
        })
    }

    pub(super) fn require_builtin_method_sig(
        &mut self,
        span: &Span,
        receiver_ty: &Ty,
        builtin: &str,
        method: &str,
    ) -> Option<FnSig> {
        lookup_builtin_method_sig(receiver_ty, method).or_else(|| {
            self.missing_builtin_contract_error(span, builtin, method, "type signature");
            None
        })
    }
}
