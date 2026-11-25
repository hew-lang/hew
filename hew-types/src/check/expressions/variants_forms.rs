//! Checker methods grouped by responsibility: variants forms.
//! Split from `expressions.rs`: checker methods, part 1 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::branch_join::BranchArmExit;
use super::super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    GenericRequirement, PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

impl Checker {
    pub(in crate::check) fn display_impl_type(&mut self, ty: &Ty) -> Option<Ty> {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if matches!(resolved, Ty::String) {
            return Some(resolved);
        }
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return None;
        }
        // `instant` is a monotonic timestamp that canonicalises to a bare i64 at
        // the MIR boundary; HIR's Display dispatch routes it through the i64
        // catalog arm (raw-nanos rendering), so it is Display-able without a
        // dedicated `impl Display for instant` body. A monotonic timestamp has
        // no wall-clock meaning, so raw nanos is the honest rendering.
        if resolved.is_instant() {
            return Some(resolved);
        }
        // These compiler carriers have a closed Display ABI selected by their
        // builtin discriminator in HIR/codegen (`hew_*_display`), with the
        // shipped `std.builtins` impl supplying the source-level contract.
        // The carrier representation deliberately stays canonical rather than
        // inheriting a `std.builtins.*` nominal name, so a generic nominal-impl
        // lookup alone cannot prove the implementation.  This is the same
        // typed identity boundary used by f-string lowering, not a leaf-name
        // exception; a user `NodeId` remains `builtin: None` and reaches the
        // ordinary trait lookup below.
        if matches!(
            resolved,
            Ty::Named {
                head: crate::TypeHead::Builtin(
                    crate::BuiltinType::NodeId | crate::BuiltinType::Location
                ),
                ..
            }
        ) {
            return Some(resolved);
        }
        // Display is the prelude's lang-item trait. A program with no
        // `#[lang_item("display")]` declaration has no Display at all, and
        // f-string interpolation accepts only the cases handled above.
        let display = self.lang_trait(crate::LangItem::Display)?;
        if self.type_implements_trait(&resolved, display) {
            return Some(resolved);
        }
        // R6: a trait object is Display when Display is in its bounds'
        // supertrait closure; HIR renders it through that layout slot.
        if let Ty::TraitObject { traits } = &resolved {
            let renders = traits.iter().any(|bound| {
                bound
                    .trait_id
                    .is_some_and(|id| id == display || self.trait_extends(id, display))
            });
            return renders.then_some(resolved);
        }
        // A bare type parameter (e.g. `T` in `fn f<T: Display>(x: T)`)
        // carries no registered impl of its own, but the enclosing item's
        // where-clause may declare a `Display` bound that satisfies the
        // obligation abstractly. The concrete `Display` impl is selected per
        // monomorphisation by HIR's static trait-dispatch lowering.
        if let Some(param) = self.bound_parameter(&resolved) {
            if self.param_carries_trait(param.id, display) {
                return Some(resolved);
            }
        }
        None
    }

    /// Whether `ty` has a structural rendering: a value `f"{v:?}"` can spell
    /// from its own parts.
    ///
    /// A pending inference variable defers - the surrounding inference
    /// reports its own error, and a resolved type reaches physical MIR, which
    /// verifies the recipe it builds. A user declaration renders through its
    /// declared fields; a compiler carrier renders only when its builtin
    /// identity says it has structure.
    pub(super) fn renders_structurally(&mut self, ty: &Ty) -> bool {
        self.subst
            .resolve(ty)
            .materialize_literal_defaults()
            .renders_structurally()
    }

    /// Verify that `ty` renders under `:?`.
    ///
    /// `f"{v:?}"` reaches here only when `v` has no `Display` impl to defer
    /// to, so this is the structural half of the same admission.
    pub(in crate::check) fn require_structural_render(&mut self, ty: &Ty, span: &Span) {
        if self.renders_structurally(ty) {
            return;
        }
        let rendered = self
            .subst
            .resolve(ty)
            .materialize_literal_defaults()
            .user_facing()
            .to_string();
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "type `{rendered}` has no structural rendering (`:?` renders \
scalars, strings, tuples, records, enums, `Vec` and `HashMap`; anything \
else needs `impl Display for {rendered}`)"
            ),
        );
    }

    /// Verify that `ty` has a `Display` impl reachable by f-string
    /// interpolation lowering.
    pub(in crate::check) fn require_display_impl(&mut self, ty: &Ty, span: &Span) {
        if matches!(self.subst.resolve(ty), Ty::Var(_) | Ty::Error) {
            return;
        }
        if self.display_impl_type(ty).is_some() {
            return;
        }
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        let display_trait = self.lang_items.get(crate::LANG_ITEM_DISPLAY).map_or_else(
            || "Display".to_string(),
            |binding| binding.trait_name.clone(),
        );
        let ty_str = format!("{}", resolved.user_facing());
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "type `{ty_str}` does not implement `{display_trait}` \
                 (f-string interpolation requires `impl {display_trait} for {ty_str}`)"
            ),
        );
    }

    pub(super) fn expect_concrete_integer_operands(
        &mut self,
        common_ty: &Ty,
        left: BinaryLeft<'_>,
        left_ty: &Ty,
        right: &Spanned<Expr>,
        right_ty: &Ty,
    ) {
        if integer_type_info(common_ty, self.pointer_width()).is_some() {
            // Preserve the source types and publish explicit widening targets
            // for HIR. Literal and inference operands use contextual unification.
            self.record_concrete_integer_left(common_ty, left, left_ty);
            self.record_concrete_integer_operand(common_ty, right, right_ty);
        }
    }

    pub(super) fn record_concrete_integer_operand(
        &mut self,
        common_ty: &Ty,
        operand: &Spanned<Expr>,
        operand_ty: &Ty,
    ) {
        let resolved = self.subst.resolve(operand_ty);
        if resolved.is_integer() && !resolved.is_integer_literal() && resolved != *common_ty {
            self.numeric_operand_coercions.insert(
                SpanKey::in_module(&operand.1, self.current_module_idx),
                common_ty.clone(),
            );
        }
        if resolved.is_integer_literal() || matches!(resolved, Ty::Var(_)) {
            if self.is_coercible_numeric(&operand.0)
                || (resolved.is_integer_literal()
                    && Self::is_literal_integer_arithmetic(&operand.0))
            {
                self.check_against(&operand.0, &operand.1, common_ty);
            } else {
                // The operand has already been checked. Rechecking an await
                // or call would repeat its ownership effects.
                self.promote_literal_binding(operand_ty, common_ty);
                self.expect_type(common_ty, operand_ty, &operand.1);
                self.record_type(&operand.1, common_ty);
            }
        }
    }

    /// Give a binding whose type is still a defaulting integer literal the
    /// concrete width its arithmetic requires.
    ///
    /// `expect_type` cannot do this: `IntLiteral` already unifies with every
    /// integer type, so the variable keeps the literal kind, the operand site
    /// alone records the narrower width, and the declaration exports the
    /// `i64` default. HIR then reads an `i64` binding under an `i32`
    /// expression, which no later stage can reconcile. Promoting the variable
    /// keeps the declaration and every reference on one type.
    pub(super) fn promote_literal_binding(&mut self, operand_ty: &Ty, common_ty: &Ty) {
        let Ty::Var(var) = operand_ty else {
            return;
        };
        if !common_ty.is_integer() || common_ty.is_integer_literal() {
            return;
        }
        if !self.subst.resolve(&Ty::Var(*var)).is_integer_literal() {
            return;
        }
        self.subst.insert(*var, common_ty).expect(
            "promoting a literal-defaulting binding to a concrete integer width stays acyclic",
        );
    }

    /// Integer arithmetic whose own checked type is still a literal type, so
    /// every leaf under it is a literal or an untyped const.
    ///
    /// Recording only the top node's contextual width would leave those leaves
    /// to default independently (`a == 0 - 1` with `a: i32` recorded the
    /// subtraction as `i32` while both literals defaulted to `i64`, which SIR
    /// then rejected as a mismatched checked-arithmetic terminator). Rechecking
    /// such a subtree against the contextual width repeats no ownership effect
    /// because it contains no call, await or resource use.
    pub(super) fn is_literal_integer_arithmetic(expr: &Expr) -> bool {
        match expr {
            Expr::Binary { .. } => expr
                .binary_last_op()
                .is_some_and(binary_chain::integer_arithmetic_op),
            Expr::Unary {
                op: UnaryOp::BitNot | UnaryOp::Negate,
                ..
            } => true,
            _ => false,
        }
    }

    pub(super) fn concrete_integer_float_mismatch(left: &Ty, right: &Ty, ptr_width: u8) -> bool {
        (integer_type_info(left, ptr_width).is_some()
            && right.is_float()
            && !right.is_float_literal())
            || (integer_type_info(right, ptr_width).is_some()
                && left.is_float()
                && !left.is_float_literal())
    }

    pub(super) fn expect_inferable_literal_binding(
        &mut self,
        name: Ident,
        expected: &Ty,
        span: &Span,
    ) {
        let Some(binding) = self.env.lookup_ref(name) else {
            return;
        };
        if binding.def_span.is_none() {
            return;
        }
        let actual = binding.ty.clone();
        // Keep the binding variable intact for unification. `expect_type`
        // normalizes first, which resolves this `Var` to `IntLiteral` and
        // loses the root that `unify` must promote to the concrete contextual
        // width. This is the use-site inference path for `let n = 7; f(n)`.
        if self.subst.resolve(&actual).is_numeric_literal()
            && expected.is_numeric()
            && self.try_unify_inference_with_owner_identity(expected, &actual)
        {
            return;
        }
        self.expect_type(expected, &actual, span);
    }

    /// Reject a bare constant binding that is ill-formed for this file: one a
    /// file import published into ANOTHER file's scope, or one published here
    /// by more than one owner. The value environment retains a single flat
    /// slot, so selecting it before this check would admit a name the file
    /// never imported, or silently choose the last registration.
    pub(super) fn report_bare_const_scope_error(&mut self, name: &str, span: &Span) -> bool {
        if name.contains('.') || name.contains("::") {
            return false;
        }
        // A local/parameter in an inner body scope shadows imports normally.
        // Only an outer import-scope binding can be ill-formed here.
        if matches!(self.env.lookup_ref_with_depth(Ident::new(name)), Some((depth, _)) if depth > 0)
        {
            return false;
        }
        if self.current_module.is_none() && self.root_value_bindings.contains(name) {
            return false;
        }
        let published = self.published_bare_const_owners.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        ));
        if published.is_none() {
            // A file import publishes its constants into the importing file
            // only. Reaching the flat env slot from a file that did not write
            // that import is out of scope, exactly as it is for its types.
            if let Some(owners) = self.file_import_const_exports.get(name) {
                let candidates: Vec<String> = owners.iter().cloned().collect();
                // The declaring file's own body is a same-owner self-reference,
                // not a cross-file one: the gate exists for a file that did not
                // write the import, and `helper.hew` never imports itself.
                if candidates.iter().any(|identity| {
                    identity
                        .rsplit_once('.')
                        .is_some_and(|(owner, _)| Some(owner) == self.current_module.as_deref())
                }) {
                    return false;
                }
                let detail = candidates
                    .iter()
                    .filter_map(|identity| identity.rsplit_once('.'))
                    .map(|(owner, _)| format!("`{owner}`"))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedVariable,
                    span,
                    format!(
                        "constant `{name}` is not in scope; it is declared by file {detail}, \
                         which this file does not import"
                    ),
                    candidates
                        .iter()
                        .map(|candidate| format!("qualify the reference, e.g. `{candidate}`"))
                        .collect(),
                );
                return true;
            }
            return false;
        }
        let Some(owners) = published else {
            return false;
        };
        if owners.len() < 2 {
            return false;
        }
        let candidates: Vec<String> = owners.iter().cloned().collect();
        self.report_error_with_suggestions(
            TypeErrorKind::AmbiguousType,
            span,
            format!(
                "ambiguous constant `{name}`: published by {} imported modules",
                candidates.len()
            ),
            candidates
                .iter()
                .map(|candidate| format!("qualify the reference, e.g. `{candidate}`"))
                .collect(),
        );
        true
    }

    /// Construct an enum nominal from the declaration authority already chosen
    /// by the checker. An exact generated-enum owner retains the catalog's
    /// builtin discriminator; every other source declaration wins over builtin
    /// normalization even when its leaf spelling is `Option` or `Result`.
    pub(in crate::check) fn variant_nominal_ty(
        &self,
        owner: crate::NominalId,
        type_args: Vec<Ty>,
    ) -> Ty {
        let path = self.defs.path(owner.declaration());
        if let Some(builtin) = self.source_authorized_generated_enum_builtin(path) {
            return Ty::named_head(crate::TypeHead::Builtin(builtin), type_args);
        }
        Ty::named_head(self.head_of_declaration(owner), type_args)
    }

    /// A bare variant is an error-recovery candidate only when its enum is
    /// bound in this file. Generated companion types are nested members.
    pub(in crate::check) fn bare_variant_owner(
        &self,
        name: &str,
        accepts: impl Fn(&TypeDef, &VariantDef) -> bool,
    ) -> Option<(crate::NominalId, &TypeDef, Symbol)> {
        let file = self.current_declaration_module()?;
        self.type_defs
            .iter()
            .filter_map(|(id, td)| {
                let variant = td.variants.get(name)?;
                if !accepts(td, variant) {
                    return None;
                }
                let spelling =
                    self.scopes
                        .visible_type_name(file, self.defs.name(id.declaration()), *id)?;
                Some((*id, td, spelling))
            })
            .min_by_key(|(id, _, _)| {
                let module = self
                    .defs
                    .module(id.declaration())
                    .map(|module| self.scopes.namespace_of(module));
                (module != Some(self.scopes.namespace_of(file)), *id)
            })
    }

    /// Look up an identifier as a unit enum variant or qualified variant name.
    #[allow(
        clippy::too_many_lines,
        reason = "multi-branch variant resolution: unqualified, qualified-in-type_defs, and qualified-in-fn_sigs each need distinct handling"
    )]
    pub(in crate::check) fn resolve_identifier_variant(&mut self, name: &str, span: &Span) -> Ty {
        // `Machine::State` / `Enum::Variant` is a value expression, so it
        // bypasses the ordinary TypeExpr resolver. Apply the same published
        // bare-type ambiguity gate before a last-writer `type_defs` entry can
        // select one machine/enum owner.
        if let Some((type_prefix, _)) = name.rsplit_once("::") {
            if !type_prefix.contains('.') && self.report_bare_type_scope_error(type_prefix, span) {
                return Ty::Error;
            }
        }
        if let Some((id, td, spelling)) = self.bare_variant_owner(name, |td, variant| {
            matches!(variant, VariantDef::Tuple(_)) || Self::variant_is_source_unit(td, name)
        }) {
            let recovery = if matches!(td.variants.get(name), Some(VariantDef::Tuple(_))) {
                Ty::Error
            } else {
                self.instantiated_unit_variant_ty(id, td)
            };
            self.report_bare_variant_expr(name, &format!("{spelling}.{name}"), span);
            return recovery;
        }
        let mut found = None;
        // Handle qualified variant names (e.g., Light::Off, LightEvent::Toggle)
        if found.is_none() {
            if let Some(pos) = name.rfind("::") {
                let type_prefix = &name[..pos];
                let variant_name = &name[pos + 2..];
                // A bare prelude/import binding may name a source-owned enum
                // whose declaration identity is qualified.  Preserve the
                // exact published owner before constructing the variant result
                // so `LookupError::NotFound` agrees with a `LookupError`
                // annotation that already resolved to `std.lookup_error`.
                let owner = self
                    .nominal_in_scope(Ident::new(type_prefix))
                    .or_else(|| self.nominal_at_path(type_prefix))
                    .or_else(|| {
                        let canonical = self.published_bare_type_qualified(type_prefix)?;
                        self.nominal_at_path(&canonical)
                    });
                if let Some((owner, td)) = owner.and_then(|id| Some((id, self.type_defs.get(&id)?)))
                {
                    if let Some(_variant) = td.variants.get(variant_name) {
                        if Self::variant_is_source_unit(
                            td,
                            name.rsplit("::").next().unwrap_or(name),
                        ) {
                            // Instantiate type params with fresh inference variables
                            // so that `Option::None` in `let x: Option<i64> = Option::None`
                            // unifies correctly with the annotation.  Bare `Named { "Option",
                            // [] }` fails arity unification against `Named { "Option", [I64] }`.
                            //
                            // Guard: only use fn_sig when its return type names the same enum as
                            // type_prefix.  Two enums sharing a bare variant name (e.g. both
                            // declaring `None`) would collide in fn_sigs because the key is the
                            // bare variant name; without the guard, `A::None` could return
                            // `Named { B, [?] }`.
                            let ty = self.instantiated_unit_variant_ty(owner, td);
                            found = Some(ty);
                        }
                    }
                }
                // Also check fn_sigs for qualified constructors
                if found.is_none() {
                    if let Some(sig) = self.fn_sig(variant_name) {
                        if sig.params.is_empty() {
                            let ret = &sig.return_type;
                            let matches_type =
                                ret.type_name().is_some_and(|name| name == type_prefix);
                            if matches_type {
                                found = Some(sig.return_type.clone());
                            }
                        }
                    }
                }
                // Import-alias fallback: `Geo::Unit` where "Geo" is an alias for
                // "shapes.Shape".  Resolve through `import_type_name_aliases` and
                // retry the unit-variant lookup under the canonical qualified name.
                if found.is_none() {
                    if let Some(canonical) = self
                        .import_type_name_aliases
                        .get(&(
                            self.current_module.clone(),
                            self.current_module_idx,
                            type_prefix.to_string(),
                        ))
                        .cloned()
                    {
                        if let Some((owner, td)) = self
                            .nominal_at_path(&canonical)
                            .and_then(|id| Some((id, self.type_defs.get(&id)?)))
                        {
                            if let Some(_variant) = td.variants.get(variant_name) {
                                if Self::variant_is_source_unit(
                                    td,
                                    name.rsplit("::").next().unwrap_or(name),
                                ) {
                                    let ty = self.instantiated_unit_variant_ty(owner, td);
                                    found = Some(ty);
                                }
                            }
                        }
                    }
                }
            }
        }
        if let Some(ty) = found {
            self.record_variant_constructor_plan(
                name.rsplit("::").next().unwrap_or(name),
                &ty,
                &[],
                span,
            );
            ty
        } else {
            // Detect recursive closure self-reference: if we are inside a lambda
            // body (capture depth is set) and the name matches the let-binding
            // being defined, emit ClosureRecursive rather than UndefinedVariable.
            // By-value capture cannot capture a value before construction, so
            // recursive closures are forbidden in v0.5.
            if self.lambda_capture_depth.is_some()
                && self
                    .pending_let_closure_name
                    .as_deref()
                    .is_some_and(|pending| pending == name)
            {
                self.report_error(
                    TypeErrorKind::ClosureRecursive {
                        name: name.to_string(),
                    },
                    span,
                    format!(
                        "closure cannot refer to its own binding \
                         `{name}` — recursive closures require a fixed-point surface that \
                         is not available in this version; use a named function instead"
                    ),
                );
                return Ty::Error;
            }
            if name == "self" {
                // Inside an actor, bare `self` is the actor's own handle:
                // `Self` is the actor type. Actor state is still reached
                // through a field, as `self.count` or bare `count`.
                if let Some(actor_ty) = &self.current_actor_type {
                    return Ty::actor_handle_of(actor_ty);
                }
                self.report_error(
                    TypeErrorKind::UndefinedVariable,
                    span,
                    "`self` is the actor's own handle and exists only inside an actor \
                     body or a method that declares it; add a `self` receiver: \
                     `fn method(self)`"
                        .to_string(),
                );
            } else {
                let local_names: Vec<&str> = self.env.all_names().map(Symbol::as_str).collect();
                let similar = crate::error::find_similar(
                    name,
                    local_names
                        .iter()
                        .copied()
                        .chain(self.sigs().entries().map(|(key, _)| key)),
                );
                let message = if self.field_shorthand_values.contains(span) {
                    format!("cannot find `{name}` in scope (field shorthand for `{name}`)")
                } else {
                    format!("undefined variable `{name}`")
                };
                self.report_error_with_suggestions_at(
                    TypeErrorKind::UndefinedVariable,
                    span,
                    Some(span),
                    message,
                    similar,
                );
            }
            Ty::Error
        }
    }

    /// Materialize a unit enum or machine-state constructor from the owning
    /// declaration, not the globally shared variant-name signature.  A
    /// generic `Lifecycle::Created` has no payload from which to infer `T`, so
    /// it must introduce fresh variables that its expected type can unify;
    /// consulting a bare `Created` signature lets an unrelated owner erase
    /// that generic identity.
    fn instantiated_unit_variant_ty(&self, owner: crate::NominalId, td: &TypeDef) -> Ty {
        let args = td
            .type_params
            .iter()
            .map(|_| Ty::Var(TypeVar::fresh()))
            .collect();
        self.variant_nominal_ty(owner, args)
    }

    pub(super) fn record_dyn_index_method_call(
        &mut self,
        traits: &[crate::ty::TraitObjectBound],
        span: &Span,
    ) -> Option<FnSig> {
        let trait_id = self.lang_trait(crate::LangItem::Index)?;
        let trait_name = self.defs.path(trait_id).to_string();
        let layout_slot = self.dyn_layout_slot_of(traits, trait_id, "at", span)?;
        let bound = &traits[layout_slot.bound];
        let slot = layout_slot.slot;
        let slot_facts = self
            .trait_object_layouts
            .get(traits)?
            .slots
            .get(slot as usize)?;
        let sig = slot_facts.signature.clone();
        let effect = slot_facts.effect;
        let target = crate::check::dispatch::CallTarget::DynamicVtable {
            declaring_trait: layout_slot.declaring_trait,
            method: layout_slot.method,
            slot,
        };
        self.dyn_trait_method_calls.insert(
            SpanKey::in_module(span, self.current_module_idx),
            crate::check::types::DynMethodCall {
                target,
                trait_name: bound.trait_name.clone(),
                method_name: "at".to_string(),
                slot,
                signature: sig.clone(),
                effect,
            },
        );
        self.record_method_call_receiver_kind(
            span,
            crate::check::types::MethodCallReceiverKind::TraitObject { trait_name },
        );
        Some(sig)
    }

    pub(in crate::check) fn context_variant_expected_owner(
        &mut self,
        expected: &Ty,
        span: &Span,
    ) -> Option<String> {
        let resolved = self.subst.resolve(expected);
        // An already-broken expected type has a diagnostic of its own. Naming
        // it again as "found `<error>`" is a cascade, and since v0.6.0 the
        // dotted spelling is the only one users write, so every scrutinee or
        // argument that fails to resolve would carry this second error.
        if matches!(resolved, Ty::Error) {
            return None;
        }
        let Ty::Named { head, .. } = &resolved else {
            self.report_error_with_suggestions(
                TypeErrorKind::ContextVariantNoType,
                span,
                format!(
                    "E_CONTEXT_VARIANT_NO_TYPE: contextual variant requires one expected enum or machine type, found `{}`",
                    resolved.user_facing()
                ),
                self.context_variant_edge_hint(),
            );
            return None;
        };
        let name = head.registry_key();
        let builtin = head.builtin();

        if matches!(builtin, Some(BuiltinType::Option | BuiltinType::Result)) {
            return Some(name.to_string());
        }
        let declaration = head.declaration(&self.defs);
        let Some(definition) = declaration.and_then(|id| self.type_defs.get(&id)) else {
            self.report_error(
                TypeErrorKind::ContextVariantNoType,
                span,
                format!(
                    "E_CONTEXT_VARIANT_NO_TYPE: expected type `{name}` is not an enum or machine"
                ),
            );
            return None;
        };
        if !matches!(definition.kind, TypeDefKind::Enum | TypeDefKind::Machine) {
            self.report_error(
                TypeErrorKind::ContextVariantNoType,
                span,
                format!(
                    "E_CONTEXT_VARIANT_NO_TYPE: expected type `{name}` is not an enum or machine"
                ),
            );
            return None;
        }
        Some(
            self.defs
                .path(declaration.expect("definition has an owner").declaration())
                .to_string(),
        )
    }

    pub(in crate::check) fn context_variant_definition(
        &self,
        expected: &Ty,
        variant: &str,
    ) -> Option<VariantDef> {
        self.ty_type_def(&self.subst.resolve(expected))
            .and_then(|definition| definition.variants.get(variant))
            .cloned()
    }

    pub(in crate::check) fn record_context_variant_resolution(
        &mut self,
        expected: &Ty,
        name: hew_parser::ast::Symbol,
        span: &Span,
    ) {
        let Ty::Named { head, .. } = self.subst.resolve(expected) else {
            return;
        };
        let Some(owner) = head.declaration(&self.defs) else {
            return;
        };
        let declaration = self
            .defs
            .member_of_kind(owner.declaration(), name, crate::DeclarationKind::Variant)
            .or_else(|| {
                self.defs.member_of_kind(
                    owner.declaration(),
                    name,
                    crate::DeclarationKind::MachineState,
                )
            });
        if let (Some(declaration), Some(site)) = (declaration, self.scope_site()) {
            self.scopes.record_resolution(
                site,
                span,
                super::super::scope::Resolution::of_member(&self.defs, declaration),
            );
        }
    }

    pub(in crate::check) fn require_unsafe(
        &mut self,
        declaration: Option<crate::DefId>,
        name: &str,
        span: &Span,
    ) {
        if !self.in_unsafe
            && declaration.is_some_and(|declaration| self.extern_table.requires_unsafe(declaration))
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("calling extern function `{name}` requires `unsafe {{ ... }}`"),
            );
        }
    }

    /// Resolve a module-qualified value-constructor reference of the form
    /// `module.Type::Variant` to its result type.  Emits a fail-closed
    /// diagnostic for each of the four error shapes (unknown module alias,
    /// no exported type, no such variant, struct-variant without braces)
    /// — never falls through to the leaky "undefined variable" /
    /// "undefined type" surface.  Called only from the
    /// `check_field_access` pre-dispatch arm.
    #[expect(
        clippy::too_many_lines,
        reason = "module-qualified variant references retain import provenance, diagnostics and constructor plans together"
    )]
    pub(in crate::check) fn check_module_qualified_variant_ref(
        &mut self,
        module_short: &str,
        type_name: &str,
        variant_name: &str,
        span: &Span,
    ) -> Ty {
        let lifecycle_surface = format!("{module_short}.{type_name}::{variant_name}");
        let Ok(_) = self.canonicalize_source_lifecycle_value_path(&lifecycle_surface, span) else {
            return Ty::Error;
        };
        if !self.module_binding_in_current_file(Ident::new(module_short)) {
            let similar =
                crate::error::find_similar(module_short, self.modules.iter().map(String::as_str));
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedVariable,
                span,
                format!("unknown module alias `{module_short}`"),
                similar,
            );
            return Ty::Error;
        }
        self.resolve_import_use(Ident::new(module_short));
        let Some((owner, td)) = self
            .resolve_module_type_declaration(module_short, type_name)
            .and_then(|id| Some((id, self.type_defs.get(&id)?.clone())))
        else {
            let similar = self
                .module_type_exports_for_binding(module_short)
                .map(|set| crate::error::find_similar(type_name, set.iter().map(String::as_str)))
                .unwrap_or_default();
            self.report_error_with_suggestions(
                TypeErrorKind::PathMemberNotFound,
                span,
                format!("module `{module_short}` has no exported type `{type_name}`"),
                similar,
            );
            return Ty::Error;
        };
        let Some(variant) = td.variants.get(variant_name).cloned() else {
            let similar =
                crate::error::find_similar(variant_name, td.variants.keys().map(String::as_str));
            self.report_error_with_suggestions(
                TypeErrorKind::PathMemberNotFound,
                span,
                format!("type `{module_short}.{type_name}` has no variant `{variant_name}`"),
                similar,
            );
            return Ty::Error;
        };
        if Self::variant_is_source_unit(&td, variant_name) {
            let args = td
                .type_params
                .iter()
                .map(|_| Ty::Var(TypeVar::fresh()))
                .collect();
            let result = self.variant_nominal_ty(owner, args);
            self.record_variant_constructor_plan(variant_name, &result, &[], span);
            return result;
        }
        match variant {
            VariantDef::Unit => {
                // Instantiate type params with fresh inference vars so generic
                // enums (e.g. `Option<T>::None`) unify against later annotations.
                // Mirrors the unit-variant path in `resolve_identifier_variant`.
                let args: Vec<Ty> = td
                    .type_params
                    .iter()
                    .map(|_| Ty::Var(TypeVar::fresh()))
                    .collect();
                self.variant_nominal_ty(owner, args)
            }
            VariantDef::Tuple(params) => {
                // Tuple-variant naked reference (no call): treat as a function
                // value, matching the bare-identifier function-value path at
                // expressions.rs (resolve_identifier).  The call form
                // `m.Type::V(args)` is handled by `check_method_call` via
                // `lookup_variant_constructor`.
                let args: Vec<Ty> = td
                    .type_params
                    .iter()
                    .map(|_| Ty::Var(TypeVar::fresh()))
                    .collect();
                let ctor_subst_map: HashMap<crate::ParamHead, Ty> = td
                    .type_params
                    .iter()
                    .zip(args.iter())
                    .map(|(p, a)| (*p, a.clone()))
                    .collect();
                let subst_params: Vec<Ty> = params
                    .iter()
                    .map(|p| p.substitute_type_params_parallel(&ctor_subst_map))
                    .collect();
                let ret = self.variant_nominal_ty(owner, args);
                Ty::Function {
                    capabilities: crate::CallableCapabilities::FUNCTION_ITEM,
                    params: subst_params
                        .into_iter()
                        .map(crate::CallableParameter::borrowed)
                        .collect(),
                    ret: Box::new(ret),
                }
            }
            VariantDef::Struct(_) => {
                // Struct variants require the braced initialiser; parse never
                // reaches this arm in the StructInit shape (that goes through
                // `check_struct_init`).  A naked `m.E::V` for a struct variant
                // is a user error — emit a hint rather than silently typing it.
                self.report_error(
                    TypeErrorKind::UndefinedField,
                    span,
                    format!(
                        "variant `{module_short}.{type_name}.{variant_name}` is a struct \
                         variant; use `{module_short}.{type_name}.{variant_name} {{ ... }}` \
                         to construct it"
                    ),
                );
                Ty::Error
            }
        }
    }

    #[expect(
        clippy::type_complexity,
        reason = "exact variant-owner lookup carries the owner, fields, and type parameters together"
    )]
    pub(super) fn lookup_struct_variant_init(
        &self,
        surface_name: &str,
    ) -> Option<(crate::NominalId, Vec<(String, Ty)>, Vec<crate::ParamHead>)> {
        let variant_name = surface_name.rsplit("::").next().unwrap_or(surface_name);
        let candidates: Vec<_> = self
            .type_defs
            .iter()
            .filter_map(|(&owner, td)| {
                let expected = self.variant_nominal_ty(owner, Vec::new());
                if !self.variant_surface_owner_matches(surface_name, &expected) {
                    return None;
                }
                match td
                    .variants
                    .get(variant_name)
                    .or_else(|| td.variants.get(surface_name))
                {
                    Some(VariantDef::Struct(fields)) => {
                        Some((owner, fields.clone(), td.type_params.clone()))
                    }
                    _ => None,
                }
            })
            .collect();

        if !surface_name.contains("::") {
            let mut local = candidates
                .iter()
                .filter(|(owner, _, _)| {
                    let type_name = self.defs.path(owner.declaration());
                    self.local_type_defs.contains(type_name)
                        || self.source_type_defs.contains(type_name)
                })
                .cloned();
            let first = local.next();
            if first.is_some() && local.next().is_none() {
                return first;
            }
        }

        match candidates.as_slice() {
            [only] => Some(only.clone()),
            _ => None,
        }
    }

    /// The `is` value-form allowance decision over two fully resolved
    /// operands, as the diagnostics it produces (empty when the comparison is
    /// admitted).
    ///
    /// Shared by [`Self::synthesize_is`] and the deferred re-check in
    /// [`Self::report_unresolved_inference_holes`] so an `is` whose operand
    /// types only settle at a call site reaches the identical answer. The two
    /// callers differ only in how a diagnostic is routed to its source module,
    /// which is why this returns them instead of reporting.
    pub(in crate::check) fn is_value_form_diagnostics(
        &self,
        lhs_span: &Span,
        lhs_resolved: &Ty,
        rhs_span: &Span,
        rhs_resolved: &Ty,
        span: &Span,
    ) -> Vec<(TypeErrorKind, Span, String)> {
        let lhs_ok = self.is_identity_capable(lhs_resolved);
        let rhs_ok = self.is_identity_capable(rhs_resolved);
        let mut diagnostics = Vec::new();

        // One rejection per `is` expression when both operands resolve to the
        // same value type: two carets carrying a byte-identical message about
        // one type reads as two separate bugs. Operands of *different* value
        // types still get one diagnostic each, since each names its own type.
        let same_value_type = !lhs_ok && !rhs_ok && lhs_resolved == rhs_resolved;
        if !lhs_ok {
            diagnostics.push(is_value_type_diagnostic(lhs_span, lhs_resolved));
        }
        if !rhs_ok && !same_value_type {
            diagnostics.push(is_value_type_diagnostic(rhs_span, rhs_resolved));
        }

        // Cross-class / cross-instantiation mismatch (e.g. `Vec<int> is Vec<String>`
        // or `<actor handle> is Vec<int>`) — only reported when both sides are
        // independently identity-capable; otherwise the value-type rejection
        // above carries the diagnostic.
        if lhs_ok && rhs_ok && lhs_resolved != rhs_resolved {
            diagnostics.push((
                TypeErrorKind::Mismatch {
                    expected: lhs_resolved.user_facing().to_string(),
                    actual: rhs_resolved.user_facing().to_string(),
                },
                span.clone(),
                format!(
                    "`is` operands must have the same type; found `{}` and `{}`",
                    lhs_resolved.user_facing(),
                    rhs_resolved.user_facing()
                ),
            ));
        }

        diagnostics
    }

    pub(super) fn resolve_is_type_pattern(&self, rhs: &Expr) -> Option<Ty> {
        let Expr::Ident(name) = rhs else {
            return None;
        };
        Ty::from_name(name.name.as_str()).or_else(|| {
            self.nominal_in_scope(*name)
                .map(|id| Ty::named_head(self.head_of_declaration(id), Vec::new()))
        })
    }

    pub(super) fn synthesize_is_type_pattern(
        &mut self,
        lhs: &Spanned<Expr>,
        lhs_ty: &Ty,
        rhs: &Spanned<Expr>,
        rhs_ty: &Ty,
        span: &Span,
    ) -> Ty {
        let lhs_resolved = self.subst.resolve(lhs_ty);
        let rhs_resolved = self.subst.resolve(rhs_ty);

        if matches!(lhs_resolved, Ty::Error | Ty::Var(_))
            || matches!(rhs_resolved, Ty::Error | Ty::Var(_))
        {
            return Ty::Bool;
        }

        let lhs_ok = self.is_identity_capable(&lhs_resolved);
        let rhs_ok = self.is_identity_capable(&rhs_resolved);

        // Same de-duplication as the value form: `a is i64` where `a: i64`
        // names one type, so it gets one diagnostic.
        let same_value_type = !lhs_ok && !rhs_ok && lhs_resolved == rhs_resolved;
        if !lhs_ok {
            self.report_is_value_type(&lhs.1, &lhs_resolved);
        }
        if !rhs_ok && !same_value_type {
            self.report_is_value_type(&rhs.1, &rhs_resolved);
        }

        if lhs_ok && rhs_ok && lhs_resolved != rhs_resolved {
            self.report_error(
                TypeErrorKind::Mismatch {
                    expected: lhs_resolved.user_facing().to_string(),
                    actual: rhs_resolved.user_facing().to_string(),
                },
                span,
                format!(
                    "`is` type pattern must match the operand type; found `{}` and `{}`",
                    lhs_resolved.user_facing(),
                    rhs_resolved.user_facing()
                ),
            );
        } else if lhs_ok && rhs_ok {
            if !matches!(lhs.0, Expr::Ident(_)) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    &lhs.1,
                    "`is` type patterns currently require an identifier operand".to_string(),
                );
                return Ty::Bool;
            }
            let rhs_key = SpanKey::in_module(&rhs.1, self.current_module_idx);
            self.is_type_patterns.insert(rhs_key, rhs_resolved.clone());
            self.record_type(&rhs.1, &rhs_resolved);
            // Static-tautology warning: the LHS type already equals the RHS
            // type pattern, so the comparison lowers to `Bool(true)` (see
            // `hew-hir/src/lower.rs` Expr::Is branch) and any `else` branch
            // gated on the negation is silently dead. Surface this as a
            // `RedundantIs` warning so the user is told before they wonder
            // why their else-branch never runs.
            self.warnings.push(crate::error::TypeError {
                fixes: Vec::new(),
                severity: crate::error::Severity::Warning,
                kind: TypeErrorKind::RedundantIs,
                span: span.clone(),
                message: format!(
                    "`is {0}` is always true here: the operand already has type `{0}`",
                    rhs_resolved.user_facing()
                ),
                notes: vec![],
                suggestions: vec![
                    "remove the `is` check, or compare against a different type".to_string()
                ],
                source_module: None,
            });
        }

        Ty::Bool
    }

    /// Report `E_IS_VALUE_TYPE` for a value-type operand of `is`.
    pub(super) fn report_is_value_type(&mut self, span: &Span, ty: &Ty) {
        let (kind, span, message) = is_value_type_diagnostic(span, ty);
        self.report_error(kind, &span, message);
    }

    /// Classify a resolved type as identity-bearing per plan §D-D2 (D340: the
    /// `is` admission set is handle identity only, HEW-SPEC-2026 §3.4.3's pid
    /// handle category).
    ///
    /// Returns `true` when `is` is valid on values of this type:
    ///
    /// * Actors and actor handles: `TypeDefKind::Actor` named types and
    ///   their own actor-handle types.
    ///
    /// Returns `false` for value types: scalars, `String`, `bytes`,
    /// `type Foo { ... }` record declarations (`TypeDefKind::Struct`),
    /// `record` types, `enum` declarations (`TypeDefKind::Enum`), machines
    /// (`TypeDefKind::Machine`), heap-backed collections (`Vec<T>`,
    /// `HashMap<K,V>`, `HashSet<T>`), tuples, arrays, slices, ranges,
    /// durations, functions, closures, and trait objects. Caller is
    /// responsible for handling `Ty::Var` / `Ty::Error` before invoking this
    /// predicate.
    ///
    /// This is the single authority for the `is` allowance set: HIR lowering,
    /// MIR, and the codegen front all read the answer from here and never
    /// re-derive it (LESSONS `checker-authority`). The set is exactly the set
    /// of shapes the codegen front can identity-compare, so the
    /// `Instr::IdentityCompare` legality check stays an unreachable backstop
    /// rather than a user-visible diagnostic.
    pub(super) fn is_identity_capable(&self, ty: &Ty) -> bool {
        match ty {
            // Named types: actor handles and any user `TypeDef` whose kind
            // carries heap/reference identity.
            Ty::Named { head, .. } => {
                // Actor handles.
                if ty.as_local_actor_ref().is_some() {
                    return true;
                }
                // Actor declarations are the only identity-bearing user
                // `TypeDef`. Everything else a `TypeDef` can name is a value:
                //
                // * `type Foo { ... }` records (`TypeDefKind::Struct`) are
                //   copy-on-write values with structural `==` and no pointer
                //   identity (`docs/language/ownership.hew`), settled by #3108.
                // * `enum` declarations are tagged values. An `indirect` enum
                //   does carry a heap box, but `indirect` is a layout
                //   annotation (HEW-SPEC-2026 §3.7.4) — admitting it to `is`
                //   would promote it to a semantic one and make identity
                //   depend on how a variant happens to be laid out, so every
                //   enum is rejected uniformly (#3134).
                // * Machines are tagged state values with payload fields, the
                //   same value class as an enum.
                //
                // None of the three has an `IdentityCompare` representation in
                // the codegen front, which is the other half of the answer:
                // the set here is the set codegen can lower, so its legality
                // check stays an unreachable backstop (#3108, #3134).
                if let Some(td) = self.head_type_def(*head) {
                    return matches!(td.kind, TypeDefKind::Actor);
                }
                false
            }

            // Everything else is a value type for `is` purposes: scalars,
            // `String`, `bytes`, `Vec`/`HashMap`/`HashSet` (copy-on-write
            // values with structural `==`, HEW-SPEC-2026 §3.4.3's value
            // category, D340), tuples, arrays, slices, function/closure
            // types, pointers, trait objects, durations, unit, never, tasks,
            // type vars (handled by caller), and the error sentinel.
            _ => false,
        }
    }

    /// Return `true` if `ty` is a v0.5 substrate handle type (affine — consumed
    /// by exactly one method call). These are `Duplex<S,R>`, `Sink<T>`,
    /// `Stream<T>`, `SendHalf<S>`, and `RecvHalf<R>`.
    ///
    /// Used by [`synthesize_identifier`](Self::synthesize_identifier) to add a
    /// targeted suggestion when a `UseAfterMove` fires on a substrate binding.
    pub(super) fn ty_is_substrate_handle(ty: &Ty) -> bool {
        matches!(ty, Ty::Named { head: crate::TypeHead::Builtin(builtin), .. } if builtin.is_substrate_handle())
    }
}
