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
use super::super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

impl Checker {
    pub(in crate::check) fn reject_if_wasm_incompatible_expr(&mut self, expr: &Expr, span: &Span) {
        if !self.wasm_target {
            return;
        }
        match expr {
            Expr::Scope { .. } | Expr::ScopeDeadline { .. } | Expr::Race(_) => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::StructuredConcurrency);
            }
            Expr::ForkChild { .. } | Expr::ForkBlock { .. } => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Tasks);
            }
            _ => {}
        }
    }

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
                    crate::BuiltinType::NodeId
                        | crate::BuiltinType::Location
                        | crate::BuiltinType::RemotePid
                ),
                ..
            }
        ) {
            return Some(resolved);
        }
        // Resolve the Display trait name through the lang-item registry.
        // No `#[lang_item("display")]` in scope means the program defines no
        // Display trait at all — in which case f-string interpolation can
        // only accept the trivially-string / inference-pending cases handled
        // above. Falling back to the literal name `"Display"` keeps
        // pre-lang-item check-time tests (no stdlib loaded) working with the
        // implicit naming convention.
        let (_display_trait, display_trait_key) =
            self.lang_items.get(crate::LANG_ITEM_DISPLAY).map_or_else(
                || ("Display".to_string(), "Display".to_string()),
                |binding| {
                    (
                        binding.trait_name.clone(),
                        self.defs.path(binding.trait_id).to_string(),
                    )
                },
            );
        if let Some(canonical) = resolved.canonical_lowering_name() {
            if self
                .primitive_trait_impls
                .contains_key(&(canonical.to_string(), display_trait_key.clone()))
            {
                return Some(resolved);
            }
        }
        if let Ty::Named { head, args } = &resolved {
            let name = head.registry_key();
            if self.type_implements_trait_for_ty(&resolved, &display_trait_key) {
                return Some(resolved);
            }
            // A bare type parameter (e.g. `T` in `fn f<T: Display>(x: T)`)
            // carries no registered impl of its own, but the enclosing
            // item's where-clause may declare a `Display` bound that
            // satisfies the obligation abstractly. The concrete `Display`
            // impl is selected per monomorphisation by HIR's static
            // trait-dispatch lowering. Mirrors `type_satisfies_trait_bound`.
            if args.is_empty() && self.type_param_carries_bound(name, &display_trait_key) {
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
        match self.subst.resolve(ty).materialize_literal_defaults() {
            Ty::Var(_)
            | Ty::Error
            | Ty::I8
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
            | Ty::IntLiteral
            | Ty::FloatLiteral
            | Ty::Bool
            | Ty::Char
            | Ty::String
            | Ty::Unit => true,
            Ty::Tuple(members) => members
                .iter()
                .all(|member| self.renders_structurally(member)),
            Ty::Named { head, args } => {
                head.builtin().is_none_or(BuiltinType::renders_structurally)
                    && args.iter().all(|arg| self.renders_structurally(arg))
            }
            _ => false,
        }
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
        left: &Spanned<Expr>,
        left_ty: &Ty,
        right: &Spanned<Expr>,
        right_ty: &Ty,
    ) {
        if integer_type_info(common_ty, self.pointer_width()).is_some() {
            // Preserve the source types and publish explicit widening targets
            // for HIR. Literal and inference operands use contextual unification.
            self.record_concrete_integer_operand(common_ty, left, left_ty);
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
            Expr::Binary { op, .. } => matches!(
                op,
                BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Modulo
                    | BinaryOp::WrappingAdd
                    | BinaryOp::WrappingSub
                    | BinaryOp::WrappingMul
                    | BinaryOp::BitAnd
                    | BinaryOp::BitOr
                    | BinaryOp::BitXor
                    | BinaryOp::Shl
                    | BinaryOp::Shr
            ),
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
        name: &str,
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
        if !matches!(self.env.lookup_ref_with_depth(name), Some((0, _))) {
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
        self.mark_ambiguous_import_owners_used(&candidates);
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

    /// Look up whether any user-declared enum type (in `local_type_defs` or
    /// `source_type_defs`) has a variant named `variant_name`.  Returns the
    /// resolved `Ty` for that variant if found (unit variant → the enum type;
    /// tuple variant → a `Ty::Function` constructing that enum), or `None` if
    /// no user type shadows the given bare name.
    ///
    /// Used by `check_identifier` to implement the local-shadows-global rule:
    /// when `fn_sigs[variant_name].is_builtin_variant` is `true`, the builtin
    /// won the bare-name slot but a user-declared variant with the same name
    /// should take priority within this compilation unit.
    /// Construct an enum nominal from the declaration authority already chosen
    /// by the checker. An exact generated-enum owner retains the catalog's
    /// builtin discriminator; every other source declaration wins over builtin
    /// normalization even when its leaf spelling is `Option` or `Result`.
    /// Non-source entries retain normal builtin canonicalization (including
    /// imported aliases).
    pub(in crate::check) fn variant_nominal_ty(&self, type_name: &str, type_args: Vec<Ty>) -> Ty {
        if let Some(builtin) = self.source_authorized_generated_enum_builtin(type_name) {
            return Ty::named_head(crate::TypeHead::Builtin(builtin), type_args);
        }
        self.named_ty_for_key(type_name, type_args)
    }

    pub(super) fn find_user_variant_shadow_ty(&self, variant_name: &str) -> Option<Ty> {
        // Iterate over local types (user-declared in root or imported source modules).
        // Two-set iteration: local first, then source.  In practice most programs
        // won't have shadowing at all, so this is a short-circuit path.
        for type_name in self
            .local_type_defs
            .iter()
            .chain(self.source_type_defs.iter())
        {
            let Some(td) = self.type_def_at(type_name.as_str()) else {
                continue;
            };
            if td.kind != TypeDefKind::Enum {
                continue;
            }
            let Some(variant) = td.variants.get(variant_name) else {
                continue;
            };
            // Build fresh inference variables for any type parameters on the
            // user enum (e.g. `enum Outcome<T> { Found(T); NotFound; }`).
            let type_args: Vec<Ty> = td
                .type_params
                .iter()
                .map(|_| Ty::Var(TypeVar::fresh()))
                .collect();
            let return_type = self.variant_nominal_ty(type_name, type_args.clone());
            return Some(match variant {
                VariantDef::Tuple(payload_tys) => {
                    // Substitute generic type params with their corresponding
                    // fresh inference variables in each payload type.
                    let params: Vec<Ty> = payload_tys
                        .iter()
                        .map(|ty| {
                            td.type_params.iter().zip(&type_args).fold(
                                ty.clone(),
                                |acc, (tp_name, fresh_var)| {
                                    acc.substitute_named_param(tp_name, fresh_var)
                                },
                            )
                        })
                        .collect();
                    Ty::Function {
                        capabilities: crate::CallableCapabilities::FUNCTION_ITEM,
                        params,
                        ret: Box::new(return_type),
                    }
                }
                // Unit variants are values (no call needed); struct variants
                // are constructed via `Expr::StructInit`, not
                // `Expr::Ident` — this path is not reached for them.
                VariantDef::Unit | VariantDef::Struct(_) => return_type,
            });
        }
        None
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
        // Two-pass scan: user-declared (local/source) types win over builtin/
        // imported types when both declare a unit variant with the same bare name
        // (local-shadows-global rule).  Pass 1 considers only types recorded in
        // `local_type_defs` or `source_type_defs`; pass 2 considers the rest.
        let mut found = None;
        // Pass 1: user-declared types.
        for (id, td) in &self.type_defs {
            let type_name = self.defs.path(id.declaration());
            if !self.local_type_defs.contains(td.name.as_str())
                && !self.source_type_defs.contains(td.name.as_str())
                && !self.is_current_module_type_def(type_name)
            {
                continue;
            }
            if let Some(variant) = td.variants.get(name) {
                if matches!(variant, VariantDef::Unit) {
                    let ty = self.instantiated_unit_variant_ty(type_name, td);
                    found = Some(ty);
                    break;
                }
            }
        }
        // Pass 2: builtin/imported types (only when no user type matched).
        if found.is_none() {
            for (id, td) in &self.type_defs {
                let type_name = self.defs.path(id.declaration());
                if self.local_type_defs.contains(td.name.as_str())
                    || self.source_type_defs.contains(td.name.as_str())
                    || self.is_current_module_type_def(type_name)
                {
                    continue; // already scanned in pass 1
                }
                if let Some(variant) = td.variants.get(name) {
                    if matches!(variant, VariantDef::Unit) {
                        let ty = self.instantiated_unit_variant_ty(type_name, td);
                        found = Some(ty);
                        break;
                    }
                }
            }
        }
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
                let canonical_type_prefix = if type_prefix.contains('.') {
                    type_prefix.to_string()
                } else {
                    self.current_module_identity()
                        .map(|owner| format!("{owner}.{type_prefix}"))
                        .filter(|candidate| self.type_def_at(candidate).is_some())
                        .or_else(|| {
                            (!self.local_type_defs.contains(type_prefix)
                                && !self.source_type_defs.contains(type_prefix))
                            .then(|| self.published_bare_type_qualified(type_prefix))
                            .flatten()
                        })
                        .unwrap_or_else(|| type_prefix.to_string())
                };
                if let Some(td) = self.type_def_at(&canonical_type_prefix) {
                    if let Some(variant) = td.variants.get(variant_name) {
                        if matches!(variant, VariantDef::Unit) {
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
                            let ty = self.instantiated_unit_variant_ty(&canonical_type_prefix, td);
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
                        if let Some(td) = self.type_def_at(canonical.as_str()) {
                            if let Some(variant) = td.variants.get(variant_name) {
                                if matches!(variant, VariantDef::Unit) {
                                    let ty = self.instantiated_unit_variant_ty(&canonical, td);
                                    found = Some(ty);
                                }
                            }
                        }
                    }
                }
            }
        }
        if let Some(ty) = found {
            if !name.contains("::") {
                let replacement = ty.type_name().map_or_else(
                    || format!(".{name}"),
                    |owner| format!("{}.{name}", super::calls::variant_owner_spelling(owner)),
                );
                self.report_bare_variant_expr(name, &replacement, span);
            }
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
                     body; elsewhere use a named receiver parameter: \
                     `fn method(val: Self)` in traits or `fn method(p: Point)` in impls"
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
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedVariable,
                    span,
                    format!("undefined variable `{name}`"),
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
    pub(super) fn instantiated_unit_variant_ty(&self, type_name: &str, td: &TypeDef) -> Ty {
        let args = td
            .type_params
            .iter()
            .map(|_| Ty::Var(TypeVar::fresh()))
            .collect();
        self.variant_nominal_ty(type_name, args)
    }

    pub(super) fn record_dyn_index_method_call(
        &mut self,
        traits: &[crate::ty::TraitObjectBound],
        bound: &crate::ty::TraitObjectBound,
        span: &Span,
    ) {
        let trait_name = bound.trait_name.as_str();
        let index_key = self.trait_ref_lookup_key(trait_name);
        let Some(layout_slot) = self.dyn_layout_slot_of(traits, &index_key, "at", span) else {
            return;
        };
        let slot = layout_slot.slot;
        // Compute the substituted `at` signature for the originating
        // bound (the bound's assoc bindings carry e.g. `Output = T`).
        // W3.031 Stage 1.6: the typed `FnSig` is self-contained on
        // the call-site side table; no codegen-time re-derivation.
        let Some(mut sig) = self.lookup_trait_method(&layout_slot.trait_key, "at") else {
            return;
        };
        self.apply_trait_object_bound_substitutions(&mut sig, bound);
        let target = crate::check::dispatch::CallTarget::DynamicVtable {
            declaring_trait: layout_slot.declaring_trait,
            method: layout_slot.method,
            slot,
        };
        self.dyn_trait_method_calls.insert(
            SpanKey::in_module(span, self.current_module_idx),
            crate::check::types::DynMethodCall {
                target,
                trait_name: trait_name.to_string(),
                method_name: "at".to_string(),
                slot,
                signature: sig,
            },
        );
        self.record_method_call_receiver_kind(
            span,
            crate::check::types::MethodCallReceiverKind::TraitObject {
                trait_name: trait_name.to_string(),
            },
        );
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
            self.report_error(
                TypeErrorKind::ContextVariantNoType,
                span,
                format!(
                    "E_CONTEXT_VARIANT_NO_TYPE: contextual variant requires one expected enum or machine type, found `{}`",
                    resolved.user_facing()
                ),
            );
            return None;
        };
        let name = head.registry_key();
        let builtin = head.builtin();

        if !name.contains('.') {
            if let Some(owners) = self.published_bare_type_owners.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                name.to_string(),
            )) {
                if owners.len() > 1 {
                    let candidates = owners.iter().cloned().collect::<Vec<_>>();
                    self.report_error_with_suggestions(
                        TypeErrorKind::ContextVariantAmbiguous,
                        span,
                        format!(
                            "E_CONTEXT_VARIANT_AMBIGUOUS: expected type `{name}` has {} imported owners",
                            candidates.len()
                        ),
                        candidates
                            .iter()
                            .map(|candidate| format!("use an owner-qualified type such as `{candidate}`"))
                            .collect(),
                    );
                    return None;
                }
            }
        }

        if matches!(builtin, Some(BuiltinType::Option | BuiltinType::Result)) {
            return Some(name.to_string());
        }
        let Some(definition) = self.type_def_at(name) else {
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
        Some(name.to_string())
    }

    pub(in crate::check) fn context_variant_definition(
        &self,
        owner: &str,
        variant: &str,
    ) -> Option<VariantDef> {
        self.type_def_at(owner)
            .and_then(|definition| definition.variants.get(variant))
            .cloned()
    }

    pub(in crate::check) fn require_unsafe(&mut self, name: &str, span: &Span) {
        // rc1-F1 stage B: `unsafe` gating is derived from the extern table's
        // declaration index — a call requires `unsafe` exactly when its
        // resolved declaration key names a registered extern declaration.
        // The canonical-owner probe covers root extern declarations, which
        // key `{root_module}.{name}` inside the checker while root call
        // sites spell the bare leaf.
        // TRANSITION(P2): the path lookups are deleted by A1 commit 3, when
        // the call carries its resolved declaration.
        let requires_unsafe = |path: &str| {
            self.defs
                .lookup_path(path)
                .is_some_and(|declaration| self.extern_table.requires_unsafe(declaration))
        };
        let scoped_unsafe = scoped_module_item_name(self.canonical_fn_owner(), name)
            .is_some_and(|qualified| requires_unsafe(&qualified));
        if !self.in_unsafe && (scoped_unsafe || requires_unsafe(name)) {
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
        reason = "qualified variant resolution handles each failure shape together"
    )]
    pub(in crate::check) fn check_module_qualified_variant_ref(
        &mut self,
        module_short: &str,
        type_name: &str,
        variant_name: &str,
        span: &Span,
    ) -> Ty {
        let lifecycle_surface = format!("{module_short}.{type_name}::{variant_name}");
        let Ok(canonical_lifecycle) =
            self.canonicalize_source_lifecycle_value_path(&lifecycle_surface, span)
        else {
            return Ty::Error;
        };
        if !self.module_binding_in_current_file(module_short) {
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
        self.used_modules.borrow_mut().insert(ImportKey::in_file(
            self.current_module.clone(),
            self.current_module_idx,
            module_short.to_string(),
        ));
        let Some(td) = self.resolve_module_type(module_short, type_name) else {
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
        let Some((_td_again, variant)) =
            self.resolve_module_variant(module_short, type_name, variant_name)
        else {
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
        let qualified_type = canonical_lifecycle
            .as_deref()
            .and_then(|path| path.split_once("::").map(|(ty, _)| ty.to_string()))
            .unwrap_or_else(|| {
                format!(
                    "{}.{type_name}",
                    self.canonical_module_import_owner(module_short)
                )
            });
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
                self.named_ty_for_key(&qualified_type, args)
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
                let ctor_subst_map: HashMap<String, Ty> = td
                    .type_params
                    .iter()
                    .zip(args.iter())
                    .map(|(p, a)| (p.clone(), a.clone()))
                    .collect();
                let subst_params: Vec<Ty> = params
                    .iter()
                    .map(|p| p.substitute_named_params_parallel(&ctor_subst_map))
                    .collect();
                let ret = self.named_ty_for_key(&qualified_type, args);
                Ty::Function {
                    capabilities: crate::CallableCapabilities::FUNCTION_ITEM,
                    params: subst_params,
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
    ) -> Option<(String, Vec<(String, Ty)>, Vec<String>)> {
        let variant_name = surface_name.rsplit("::").next().unwrap_or(surface_name);
        let mut candidates: Vec<(String, Vec<(String, Ty)>, Vec<String>)> = self
            .type_defs
            .iter()
            .filter_map(|(type_name, td)| {
                let type_name = self.defs.path(type_name.declaration());
                let canonical_type_name = self
                    .canonical_nominal_name(type_name)
                    .unwrap_or_else(|| type_name.to_string());
                let expected = self.named_ty_for_key(&canonical_type_name, vec![]);
                if !self.variant_surface_owner_matches(surface_name, &expected) {
                    return None;
                }
                match td
                    .variants
                    .get(variant_name)
                    .or_else(|| td.variants.get(surface_name))
                {
                    Some(VariantDef::Struct(fields)) => {
                        Some((canonical_type_name, fields.clone(), td.type_params.clone()))
                    }
                    _ => None,
                }
            })
            .collect();
        candidates.sort_by(|a, b| a.0.cmp(&b.0));
        candidates.dedup_by(|a, b| a.0 == b.0);

        if !surface_name.contains("::") {
            let mut local = candidates
                .iter()
                .filter(|(type_name, _, _)| {
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
            self.lookup_type_def(name.name.as_str())
                .map(|type_def| self.named_ty_for_key(&type_def.name, vec![]))
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
                let name = head.registry_key();
                // Actor handles.
                if ty.as_local_actor_ref().is_some() {
                    return true;
                }
                // Actor declarations are the only identity-bearing user
                // `TypeDef`. Everything else a `TypeDef` can name is a value:
                //
                // * `type Foo { ... }` records (`TypeDefKind::Struct`) are
                //   copy-on-write values with structural `==` and no pointer
                //   identity (`docs/v05/ownership.md`), settled by #3108.
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
                if let Some(td) = self.type_def_at(name) {
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
