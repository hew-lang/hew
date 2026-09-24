//! Checker methods grouped by responsibility: primitive trait dispatch.
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
    /// Resolve a method on a builtin `Result`/`Option` receiver against the
    /// canonical stdlib method surface ONLY.
    ///
    /// Dispatch on a builtin `Result<T, E>` / `Option<T>` receiver (e.g. the
    /// `Result<T, AskError>` wrapper an actor ask produces) must never consult
    /// the user `type_defs`/`fn_sigs`: a user package may declare its own
    /// `type Result`/`type Option` whose methods land under the same bare
    /// `Result::<method>` keys and shadow the stdlib entries by registration
    /// order. Resolving here against the origin-based
    /// [`Checker::builtin_result_option_method_sigs`] snapshot guarantees the
    /// builtin surface (and its `extern_symbol` rewrite) is selected for every
    /// method, not just a fixed allowlist of names. A method absent from the
    /// snapshot returns `None`, so the caller falls through to the
    /// `no method on Result<...>`/`Option<...>` diagnostic.
    pub(in crate::check) fn lookup_builtin_result_option_method_sig(
        &self,
        builtin: BuiltinType,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        let (impl_params, sig) = self
            .builtin_result_option_method_sigs
            .get(&(builtin, method.to_string()))?;
        Some(instantiate_stdlib_method_sig(sig, impl_params, type_args))
    }

    /// The inherent `impl T { fn method(…) }` declaration identity, which is
    /// what a direct `T.method(…)` call targets.  A trait slot filled by
    /// structural satisfaction names this same declaration.
    pub(in crate::check) fn inherent_impl_method_declaration(
        &self,
        receiver_ty: &Ty,
        method: &str,
    ) -> Option<crate::DefId> {
        let key = self.named_source_method_dispatch_key(receiver_ty, method)?;
        self.impl_method_declaration_ids.get(&key).cloned()
    }

    /// Stage A2: dispatch a method call on a primitive or compiler-builtin
    /// generic receiver to a user `impl Trait for <kind>` body via the
    /// `primitive_trait_impls` side table populated in Stage A1.
    ///
    /// Receiver-keyed (NOT trait-name-keyed): the lookup goes through
    /// `lookup_primitive_trait_method`, which keys on the canonical receiver
    /// kind first, so the surviving five magic `dyn Display` callers
    /// (`assert_eq` / `assert_ne` / `to_string` / `len` / `stop`) cannot be
    /// hijacked by trait-name string matching.
    ///
    /// Returns `Some(return_ty)` after applying argument checks and recording
    /// the dispatch metadata for codegen, or `None` when no impl matches and
    /// the caller should emit its own "no method on X" diagnostic so existing
    /// "no method on Vec" / "no method on string" wording survives.
    ///
    /// Limitation: if two distinct traits each define a method of the same
    /// name on the same receiver kind, this returns the first match the
    /// table iteration encounters.  Acceptable today (only `Display` is in
    /// scope per Phase 1 of #1565), but Phase 2 (`Debug`) must add a
    /// disambiguation rule before introducing same-name conflicts.
    pub(super) fn try_dispatch_primitive_trait_method(
        &mut self,
        resolved_receiver: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        // Default `IntLiteral` / `FloatLiteral` receivers to their canonical
        // numeric kind before the side-table lookup.  Without this,
        // `(42).fmt()` (literal-form receiver, never bound to a typed `let`)
        // would short-circuit on `canonical_primitive_or_builtin_key` returning
        // `None` for the still-polymorphic literal shape and the caller would
        // emit `no method `fmt` on int`, even though `(42_i64).fmt()` and
        // `let x: i64 = 42; x.fmt()` both succeed.  Mirrors the existing
        // defaulting sites at methods.rs:49 / 125 / 129 / 157 / 161 / 202 /
        // 254 / 317 — collapse the literal exactly at the boundary that would
        // otherwise diagnose, never eagerly upstream.
        let defaulted_receiver = resolved_receiver.materialize_literal_defaults();
        let canonical = Checker::canonical_primitive_or_builtin_key(&defaulted_receiver)?;
        let (trait_name, sig) = self.lookup_primitive_trait_method(&defaulted_receiver, method)?;
        // Bind the impl's type parameters from the concrete receiver's type
        // arguments BEFORE applying the signature, and PROVE the impl's `Self`
        // structurally matches the receiver. Without the binding a generic
        // builtin impl (`impl<E> Index for Vec<E>`) dispatched on `Vec<i64>`
        // leaves `E` (hence an `Option<E>` / `Self::Output` return) as an
        // unresolved inference var that escapes past the checker output
        // boundary — the builtin-vs-user asymmetry this fix closes. Without the
        // shape proof a constrained/concrete impl (`impl Acc for Vec<i64>`)
        // would be over-applied to a non-matching receiver (`Vec<string>`) and
        // project an authoritative-but-wrong return type (a fail-open). On a
        // non-match `instantiate_*` returns `None` and we fall through to the
        // "no method" path below, failing closed. User `Ty::Named` receivers
        // already do both via `lookup_named_method_sig`.
        let sig = self.instantiate_primitive_trait_method_sig(
            sig,
            &canonical,
            &trait_name,
            &defaulted_receiver,
        )?;
        let applied_sig = self.apply_instantiated_call_signature(
            &sig,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method `{method}`"),
            },
            true,
            Some(GenericCallee::Method {
                type_name: &canonical,
                method,
                owner_type_args: &[],
            }),
        );
        let method_key = format!("{canonical}::{method}");
        // Concrete-specialised primitive impl (#2270): when the builtin receiver
        // has non-empty type args (e.g. `Vec<i64>`) and the impl is a concrete
        // specialisation (`impl Summable for Vec<i64>` — no generic impl type
        // params), the HIR fn_registry key is the mangled form
        // (`"Vec$$i64::total"`), not the bare `"Vec::total"`. The HIR first pass
        // mangled the key to prevent `impl Trait for Vec<i64>` and
        // `impl Trait for Vec<string>` from registering the same LLVM symbol.
        // Use the mangled c_symbol here so HIR can resolve it to a
        // `ResolvedRef::Item`; fall back to the bare key for generic impls and
        // for any type arg that cannot be mangled.
        let c_symbol = if let Ty::Named {
            args: receiver_type_args,
            ..
        } = &defaulted_receiver
        {
            if receiver_type_args.is_empty() {
                method_key.clone()
            } else {
                let resolved_args: Option<Vec<ResolvedTy>> = receiver_type_args
                    .iter()
                    .map(|ty| ResolvedTy::from_ty(ty).ok())
                    .collect();
                resolved_args
                    .as_ref()
                    .and_then(|args| crate::resolved_ty::mangle_impl_self_name(&canonical, args))
                    .filter(|m| self.fn_sigs.contains_key(&format!("{m}::{method}")))
                    .map_or_else(|| method_key.clone(), |m| format!("{m}::{method}"))
            }
        } else {
            method_key.clone()
        };
        let target = self
            .impl_method_declaration_ids
            .get(&c_symbol)
            .or_else(|| self.impl_method_declaration_ids.get(&method_key))
            .cloned()
            .map_or_else(
                || CallTarget::Unsupported {
                    reason: format!(
                        "primitive impl method `{c_symbol}` has no registered declaration identity"
                    ),
                },
                CallTarget::impl_method,
            );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name,
                canonical_receiver: canonical,
            },
        );
        if self.fn_sigs.contains_key(&method_key) || self.fn_sigs.contains_key(&c_symbol) {
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RewriteToFunction {
                    target,
                    c_symbol,
                    // User-fn dispatch into a primitive trait impl
                    // (`i64::fmt` etc.) is open-set; the typed runtime-call
                    // catalog does not enumerate user-defined method keys.
                    descriptor: None,
                    extern_identity: None,
                    // Primitive trait-impl dispatch is a user-fn call; it never
                    // consumes the receiver as a handle release.
                    consumes_receiver: sig.consumes_receiver,
                    requires_mutable_receiver: sig.requires_mutable_receiver,
                    receiver_update: sig.receiver_update,
                    returns_receiver_identity: sig.returns_receiver_identity,
                },
            );
        }
        // Project any `Self::Output`-style associated-type carrier in the
        // return now that the impl's type params are bound to the concrete
        // receiver — `project_assoc_types` keys the concrete builtin base
        // (`<Vec<i64> as Index>::Output`) through `impl_assoc_type_bindings`.
        Some(self.project_assoc_types(&applied_sig.return_type))
    }

    /// Instantiate a primitive/builtin-generic trait-impl method signature
    /// against a concrete receiver — **fallibly**.
    ///
    /// Dispatch reaches here keyed only on the canonical builtin kind
    /// (`Vec`/`HashMap`/…), so the registered impl's `Self` type must still be
    /// *proven* to structurally match the concrete receiver before its return
    /// type is treated as authoritative. Returns `None` when the impl does not
    /// apply (constrained/concrete/nested `Self` that the receiver does not
    /// satisfy), so the caller falls through to the normal "no matching
    /// method/impl" path and fails **closed** — never projecting an authoritative
    /// return type for a receiver that does not implement the impl.
    ///
    /// On a match, binds the impl's type parameters by structurally unifying the
    /// impl's recorded `Self` type arguments
    /// ([`Checker::primitive_trait_impl_self_args`]) against the receiver's
    /// concrete type arguments, then substitutes both `Self` (→ the receiver)
    /// and the bound parameters through the signature's params and return type.
    /// Substituted parameters are dropped from `type_params` so
    /// `apply_instantiated_call_signature` does not re-instantiate them as fresh
    /// inference vars. Mirrors `instantiate_named_method_sig` (the user
    /// `Ty::Named` path) for builtin receivers, which have no `type_defs` entry.
    ///
    /// Hew rejects overlapping impls (the associated-type binding table is keyed
    /// by type *name*, so a second impl for the same `(kind, trait)` collides and
    /// is diagnosed at registration for builtins and user records alike). There
    /// is therefore at most one usable impl per `(canonical, trait)`, and the
    /// stored `Self` args identify exactly that impl — no per-impl table needed.
    ///
    /// Non-generic primitive impls (`impl Display for i64`) have empty stored
    /// `Self` args and an argument-less receiver, so the arity check passes
    /// trivially, no parameters bind, and only the `Self` → receiver substitution
    /// applies — leaving existing dispatch behaviour unchanged.
    pub(super) fn instantiate_primitive_trait_method_sig(
        &mut self,
        mut sig: FnSig,
        canonical: &str,
        trait_name: &str,
        receiver_ty: &Ty,
    ) -> Option<FnSig> {
        let mut subst: HashMap<String, Ty> = HashMap::new();
        // Clone out of the side table so the structural match can take `&mut
        // self` (it unifies unbound receiver vars against concrete `Self` args).
        if let Some(self_args) = self
            .primitive_trait_impl_self_args
            .get(&(canonical.to_string(), trait_name.to_string()))
            .cloned()
        {
            let receiver_args: Vec<Ty> = match receiver_ty {
                Ty::Named { args, .. } => args.clone(),
                _ => Vec::new(),
            };
            // Arity is part of the shape: an impl whose `Self` constructor takes
            // a different number of arguments than the receiver cannot apply.
            if self_args.len() != receiver_args.len() {
                return None;
            }
            let impl_params: HashSet<String> = sig.type_params.iter().cloned().collect();
            // Snapshot `self.subst` around the applicability probe: a multi-arg
            // `Self` (`HashMap<K, V>`) unifies each concrete arg into `self.subst`
            // as it matches, but a LATER arg may reject the impl. Without the
            // rollback an early concrete-arg unification would persist into the
            // checker's substitution past a `None` reject — binding an inference
            // var from an impl that does not actually apply. Restore on every
            // non-match so the probe is side-effect-free (hardening per the
            // A-general gate's non-blocking note).
            let subst_snapshot = self.subst.snapshot();
            for (self_arg, receiver_arg) in self_args.iter().zip(receiver_args.iter()) {
                if !self.match_self_arg_param(self_arg, receiver_arg, &impl_params, &mut subst) {
                    // `Self` does not structurally match the receiver — this impl
                    // does not apply. Roll back any partial unification, then fail
                    // closed.
                    self.subst.restore(subst_snapshot);
                    return None;
                }
            }
        }
        for param_ty in &mut sig.params {
            *param_ty = param_ty
                .substitute_named_param("Self", receiver_ty)
                .substitute_named_params_parallel(&subst);
        }
        sig.return_type = sig
            .return_type
            .substitute_named_param("Self", receiver_ty)
            .substitute_named_params_parallel(&subst);
        sig.type_params.retain(|tp| !subst.contains_key(tp));
        sig.type_param_bounds
            .retain(|tp, _| !subst.contains_key(tp));
        Some(sig)
    }

    /// Fallibly match a single impl `Self`-position type argument against the
    /// receiver's corresponding concrete argument, recording impl
    /// type-parameter bindings into `subst`. Returns `false` on any structural
    /// mismatch so the impl is rejected (fail closed) rather than over-applied.
    ///
    /// - A bare `Ty::Named { name, args: [] }` whose `name` is an impl type
    ///   parameter (`impl<E> … for Vec<E>` → `E`) binds to the resolved receiver
    ///   argument; a parameter appearing more than once (`HashMap<K, K>`) must
    ///   bind consistently.
    /// - A constructed nominal `Self` type (`Vec<T>` in `impl<T> Acc for
    ///   Vec<Vec<T>>`, or a concrete `Vec<i64>` / user `Point`) requires the
    ///   receiver to be the **same constructor** — identical `name`, `builtin`,
    ///   and arity — before recursing element-wise. This rejects
    ///   `Vec<Vec<T>>` ⊄ `Vec<Option<i64>>` (Vec ≠ Option) and `Vec<i64>` ⊄
    ///   `Vec<string>`.
    /// - A concrete non-`Named` leaf (`i64`, `string`, …) requires equality with
    ///   the resolved receiver argument.
    /// - In either concrete case, an unbound receiver `Ty::Var` is unified with
    ///   the fully-concrete `Self` argument, so inference can resolve the element
    ///   type when exactly one impl could apply.
    pub(super) fn match_self_arg_param(
        &mut self,
        self_arg: &Ty,
        receiver_arg: &Ty,
        impl_params: &HashSet<String>,
        subst: &mut HashMap<String, Ty>,
    ) -> bool {
        let receiver_resolved = self.subst.resolve(receiver_arg);
        match self_arg {
            // Bare impl type parameter: bind to the receiver arg (consistently).
            Ty::Named { name, args, .. } if args.is_empty() && impl_params.contains(name) => {
                if let Some(existing) = subst.get(name) {
                    return *existing == receiver_resolved;
                }
                subst.insert(name.clone(), receiver_resolved);
                true
            }
            // Constructed / concrete-nominal `Self` type: same constructor, then
            // recurse. An unbound receiver var unifies with a fully-concrete one.
            Ty::Named {
                name: s_name,
                builtin: s_builtin,
                args: s_args,
            } => {
                if matches!(receiver_resolved, Ty::Var(_)) {
                    return !Self::ty_contains_impl_param(self_arg, impl_params)
                        && self.try_unify_with_owner_identity(&receiver_resolved, self_arg);
                }
                let Ty::Named {
                    name: r_name,
                    builtin: r_builtin,
                    args: r_args,
                } = &receiver_resolved
                else {
                    return false;
                };
                if s_name != r_name || s_builtin != r_builtin || s_args.len() != r_args.len() {
                    return false;
                }
                s_args
                    .iter()
                    .zip(r_args.iter())
                    .all(|(s, r)| self.match_self_arg_param(s, r, impl_params, subst))
            }
            // Concrete non-`Named` leaf (`i64`, `string`, `bool`, …): require
            // equality, or unify an unbound receiver var with the concrete leaf.
            concrete => {
                if matches!(receiver_resolved, Ty::Var(_)) {
                    return self.try_unify_with_owner_identity(&receiver_resolved, concrete);
                }
                receiver_resolved == *concrete
            }
        }
    }

    /// Whether `ty` mentions any of the impl's type parameters anywhere in its
    /// structure. Used to gate unifying an unbound receiver var against a `Self`
    /// argument: only fully-concrete `Self` args (no impl params) may drive
    /// inference of the receiver's element type.
    pub(super) fn ty_contains_impl_param(ty: &Ty, impl_params: &HashSet<String>) -> bool {
        match ty {
            Ty::Named { name, args, .. } => {
                impl_params.contains(name)
                    || args
                        .iter()
                        .any(|a| Self::ty_contains_impl_param(a, impl_params))
            }
            _ => false,
        }
    }

    /// Stage A3: UFCS form of [`Self::try_dispatch_primitive_trait_method`].
    ///
    /// `Display::fmt(x)` registers `Display::fmt` in `fn_sigs` with the
    /// receiver param stripped (the `Trait::method` key triggers the
    /// is-method branch in `register_fn_sig_with_name`), so the call site
    /// would mis-arity (sig.params=[] vs args=[x]).  This helper detects
    /// the trait-qualified form, synthesizes the first arg as the
    /// receiver, and consults the same side table that powers
    /// receiver-form dispatch.  The first arg is type-checked against the
    /// canonical receiver kind itself; remaining args are type-checked
    /// against the registered sig's params (already receiver-stripped at
    /// registration time).
    ///
    /// Returns `None` when the receiver is not a primitive or builtin
    /// generic, or when no impl exists for the (kind, trait, method)
    /// triple.  The caller falls through to the existing trait-qualified
    /// dispatch in `calls.rs`, which keeps emitting today's diagnostics.
    pub(in crate::check) fn try_dispatch_ufcs_primitive_trait_method(
        &mut self,
        trait_name: &str,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let first_arg = args.first()?;
        let trait_key = self.trait_ref_lookup_key(trait_name);
        // Short-circuit before synthesising first_arg: if trait_name has no
        // primitive impls registered at all, return immediately.  This
        // prevents the fallback trait-qualified path from synthesising
        // first_arg a second time when the helper was never going to handle
        // the dispatch.  Synthesis of first_arg is deferred to after this
        // guard so it only happens when there is a real chance we will own
        // the call.
        let has_primitive_impl = self
            .primitive_trait_impls
            .keys()
            .any(|(_, tn)| tn == &trait_key);
        if !has_primitive_impl {
            return None;
        }
        let (first_expr, first_sp) = first_arg.expr();
        // Synthesize the receiver arg's type so we can route to the
        // canonical primitive/builtin-generic key.
        let receiver_ty = self.synthesize(first_expr, first_sp);
        // Default `IntLiteral` / `FloatLiteral` receivers in UFCS form
        // (e.g. `Display::fmt(42)`) for the same reason as the method-form
        // path at try_dispatch_primitive_trait_method: without defaulting,
        // the synthesized receiver_ty is still in literal shape and
        // `canonical_primitive_or_builtin_key` returns `None`, causing the
        // caller to fall through to the trait-qualified path which then
        // mis-arities (the receiver-stripped sig has 0 params vs the 1 arg
        // we just synthesized).
        let resolved_receiver = self
            .subst
            .resolve(&receiver_ty)
            .materialize_literal_defaults();
        let canonical = Checker::canonical_primitive_or_builtin_key(&resolved_receiver)?;
        // Lookup keyed on the canonical receiver kind + trait name +
        // method.  We call into the table directly (not the
        // walk-every-trait helper) because the trait name is known at
        // this call site and there is no ambiguity to resolve.
        let sig = self
            .primitive_trait_impls
            .get(&(canonical.clone(), trait_key.clone()))
            .and_then(|methods| methods.get(method_name))
            .cloned()?;
        // Bind the impl's type params from the concrete receiver before
        // applying the signature, and prove `Self` matches (mirroring the
        // method-form path) so a UFCS call (`Index::get(v, i)`) on a generic
        // builtin receiver projects its `Output`/return instead of leaking an
        // unresolved inference var — and a constrained/concrete impl is not
        // over-applied. On a non-match, fall through to the trait-qualified
        // path, which fails closed.
        let sig = self.instantiate_primitive_trait_method_sig(
            sig,
            &canonical,
            &trait_key,
            &resolved_receiver,
        )?;
        // Do not add an outer check_arity here.  apply_instantiated_call_signature
        // already calls check_arity on trailing_args via PositionalOnly, matching
        // the receiver-form path at try_dispatch_primitive_trait_method.  An
        // outer check on all args (receiver + trailing) would fire a second
        // arity diagnostic for the same call — e.g. Display::fmt(x, extra)
        // would emit both "expected 1 arg" and "expected 0 trailing args".
        // Type-check remaining args against the (receiver-stripped)
        // params using the same machinery as method-form dispatch.
        let trailing_args = &args[1.min(args.len())..];
        let applied = self.apply_instantiated_call_signature(
            &sig,
            None,
            trailing_args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method `{trait_name}.{method_name}`"),
            },
            true,
            Some(GenericCallee::Method {
                type_name: &canonical,
                method: method_name,
                owner_type_args: &[],
            }),
        );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name: trait_name.to_string(),
                canonical_receiver: canonical,
            },
        );
        Some(self.project_assoc_types(&applied.return_type))
    }
}
