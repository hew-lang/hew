//! Split from `expressions.rs`: checker methods, part 2 of 5.
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
    #[allow(
        clippy::too_many_lines,
        reason = "single dispatch over all identifier forms (context readers, module-qualified variants, bindings, fn sigs, constructors, type aliases); splitting would fragment shared error-reporting state"
    )]
    pub(super) fn synthesize_identifier_with_type_args(
        &mut self,
        name: &str,
        type_args: Option<&[Spanned<TypeExpr>]>,
        span: &Span,
    ) -> Ty {
        if type_args.is_some() && self.env.lookup_ref(name).is_some() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "explicit type arguments require a function declaration, not a value binding"
                    .to_string(),
            );
            return Ty::Error;
        }
        if let Some(reader) = ExecutionContextReader::from_surface_name(name) {
            if self.in_actor_handler_context {
                return reader.ty();
            }
            self.report_error(
                TypeErrorKind::ContextReaderOutsideHandler,
                span,
                format!(
                    "context reader `{}` is only available directly inside an actor handler body; \
                     nested lambdas and ordinary functions have no in-scope execution context",
                    reader.surface_name()
                ),
            );
            return Ty::Error;
        }
        if name.starts_with('@') {
            self.report_error(
                TypeErrorKind::UndefinedVariable,
                span,
                format!(
                    "unknown context reader `{name}`; valid readers are @actor_id, \
                     @supervisor, and @trace_span"
                ),
            );
            return Ty::Error;
        }
        let Ok(canonical_lifecycle_name) =
            self.canonicalize_source_lifecycle_value_path(name, span)
        else {
            return Ty::Error;
        };
        // The lifecycle authority MINTS the canonical identity here; the
        // lexical spelling stays available for the surfaces that must split a
        // `module.Type::Variant` path into its parts. Splitting the minted
        // identity instead would read `std` as a module binding — a rendered
        // identity is never parsed back into one (rc1-F1 stage D).
        let surface_name = name;
        let name = canonical_lifecycle_name.as_deref().unwrap_or(name);
        if self.report_bare_const_scope_error(name, span) {
            return Ty::Error;
        }
        // Module-qualified value constructor reference encoded as a flat
        // `Identifier("module.Type::Variant")` by `parse_dot_postfix` when no
        // call-args or brace-body follow.  Dispatch to the fail-closed
        // module-aware checker before falling through to the generic
        // "undefined variable" path, which would produce a misleading error.
        //
        // Guard: only intercept when:
        //  - the module part isn't a known binding or local type (mirrors
        //    the check_field_access guard at line 3631)
        //  - the combined "module.Type" key is NOT already in type_defs
        //    (registered module-qualified types like "lifecycle.Lifecycle"
        //    are correctly resolved by resolve_identifier_variant via the
        //    type_defs flat-key path — don't short-circuit that path)
        if let Some(dot_pos) = surface_name.find('.') {
            let candidate_module = &surface_name[..dot_pos];
            let rest = &surface_name[dot_pos + 1..];
            if let Some(colon_pos) = rest.find("::") {
                let type_name = &rest[..colon_pos];
                let variant_name = &rest[colon_pos + 2..];
                let is_binding = self.env.lookup_ref(candidate_module).is_some();
                let is_known_type = self.type_defs.contains_key(candidate_module);
                let qualified_key = format!("{candidate_module}.{type_name}");
                let qualified_in_type_defs = self.type_defs.contains_key(&qualified_key);
                if !is_binding && !is_known_type && !qualified_in_type_defs {
                    return self.check_module_qualified_variant_ref(
                        candidate_module,
                        type_name,
                        variant_name,
                        span,
                    );
                }
            }
        }
        if let Some((depth, binding)) = self.env.lookup_with_depth(name) {
            let binding_id = binding.id;
            let is_moved = binding.is_moved;
            let deferred_init = binding.deferred_init();
            let moved_at = binding.moved_at.clone();
            let ty = binding.ty.clone();
            let def_span = binding
                .def_span
                .clone()
                .or_else(|| binding.shadow_span.clone());
            // The outermost place of an assignment target is written, not read:
            // `sock = Socket { .. }` after `sock.detach()` is the re-initialisation
            // that plugs the hole, not a use of the value that left.
            let is_write_target = self.place_write_depth > 0 && self.place_base_depth == 0;
            if !is_write_target {
                self.reject_crash_hook_consumed_state_read(binding_id, span);
            }
            if is_moved && deferred_init && !is_write_target {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "E_ACTOR_FIELD_UNINITIALIZED: state field `{name}` is read before \
                         `init` initializes it; assign it first"
                    ),
                );
            } else if is_moved && !is_write_target {
                let is_linear = matches!(
                    &ty,
                    Ty::Named { name, .. } if self.registry.is_linear(name)
                );
                let mut err = TypeError::new(
                    if is_linear {
                        TypeErrorKind::UseAfterConsume
                    } else {
                        TypeErrorKind::UseAfterMove
                    },
                    span.clone(),
                    if is_linear {
                        format!("UseAfterConsume: use of consumed linear value `{name}`")
                    } else {
                        format!("use of moved value `{name}`")
                    },
                );
                if let Some(ref source_module) = self.current_module {
                    err = err.with_source_module(source_module.clone());
                }
                if let Some(moved_span) = moved_at {
                    err = err.with_note(moved_span, "value was consumed here");
                }
                // Substrate handles (Duplex, Sink, Stream, SendHalf, RecvHalf) are
                // affine: each consuming method (`.close()`, `.send_half()`,
                // `.recv_half()`, etc.) moves the handle exactly once. Subsequent
                // uses are rejected here. Name the type so the user knows why.
                if Self::ty_is_substrate_handle(&ty) {
                    err = err.with_suggestion(format!(
                        "`{}` is a substrate handle — consuming methods like `.close()`, \
                         `.send_half()`, and `.recv_half()` move the handle; \
                         use a single consuming call per binding",
                        ty.user_facing()
                    ));
                } else if is_linear {
                    err = err.with_suggestion(
                        "a `#[linear]` binding has exactly one ownership path; invoke its \
                         consuming method only once"
                            .to_string(),
                    );
                } else if self.registry.implements_marker(&ty, MarkerTrait::Clone) {
                    // The value's type has a clone path, so the canonical fix is
                    // to duplicate it before the consuming use and pass the copy.
                    err = err.with_suggestion(format!(
                        "duplicate `{name}` with `clone {name}` before the consuming use \
                         to keep the original usable"
                    ));
                }
                self.errors.push(err);
            }
            // A whole-value use of a partially-moved aggregate would hand a
            // second owner the storage that already moved out. Projection bases
            // are exempt (handled inside the reporter, which is the one
            // authority on that rule) and so are assignment targets, which
            // write rather than read.
            if !is_moved && !is_write_target {
                self.report_place_use_after_move(name, &[], span);
            }
            // A read inside a generator body captures into the generator frame.
            // `in_generator` covers `gen fn`, `receive gen fn` and `gen { }`,
            // and is cleared inside a nested lambda body, whose own capture
            // rule (`finish_closure_captures`) owns that boundary instead.
            if self.in_generator && !is_write_target {
                self.reject_borrowed_generator_capture(name, span);
            }
            // Track captures: variable from scope below the lambda boundary
            if let Some(capture_depth) = self.lambda_capture_depth {
                if depth < capture_depth {
                    self.lambda_captures.push(ty.clone());
                    self.lambda_capture_facts.push(ClosureCaptureFact {
                        binding_id,
                        name: name.to_string(),
                        ty: ty.clone(),
                        acquisition: crate::ClosureCaptureAcquisition::Snapshot,
                        access: crate::ClosureCaptureAccess::Read,
                        consumption: crate::ClosureCaptureConsumption::Retained,
                        is_send: false,
                        is_sync: false,
                        use_span: span.clone(),
                        def_span,
                    });
                }
            }
            ty
        } else if let Some(fn_sig_key) = self.visible_fn_signature_key(name) {
            // Function name used as a value (e.g., variant constructor)
            if let Some(source_identity) = self
                .import_fn_name_aliases
                .get(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    name.to_string(),
                ))
                .cloned()
            {
                self.reject_wasm_native_only_function_identity(&source_identity, span);
                if let Some((source_owner, _)) = source_identity.rsplit_once('.') {
                    self.mark_module_owner_bindings_used(source_owner);
                }
            }
            self.record_call_edge(&fn_sig_key);
            let sig = self.fn_sigs[&fn_sig_key].clone();
            // A bare enum variant used as a value (`let c = Red;`,
            // `xs.map(Wrap)`) is refused like its call form; nothing here
            // selects the enum, so the fix-it qualifies it.
            // A machine's states are written bare only inside that machine
            // (§3.11.3); elsewhere they follow the same rule (D550).
            if !surface_name.contains("::") {
                if let Some((owner, _, _)) =
                    self.lookup_variant_constructor(name)
                        .filter(|(owner, _, _)| {
                            self.type_defs
                                .get(owner)
                                .is_some_and(|td| td.kind == TypeDefKind::Enum)
                                && !self.machine_state_is_bare_here(owner)
                        })
                {
                    let replacement =
                        format!("{}.{name}", super::calls::variant_owner_spelling(&owner));
                    self.report_bare_variant_expr(name, &replacement, span);
                }
            }
            // local-shadows-global: when the fn_sig slot was won by a builtin enum
            // variant, prefer any user-declared enum that has a variant with the
            // same name (e.g. user `enum AppError { NotFound(string); }` shadows
            // the builtin `LookupError::NotFound` unit variant).
            if sig.is_builtin_variant {
                if let Some(user_ty) = self.find_user_variant_shadow_ty(name) {
                    return user_ty;
                }
            }
            if sig.params.is_empty() && self.let_identifier_is_unit_variant(name) {
                sig.return_type
            } else {
                self.instantiate_function_value(&fn_sig_key, type_args, span)
            }
        } else if self.module_binding_in_current_file(surface_name) {
            self.report_error(
                TypeErrorKind::ModuleUsedAsValue,
                span,
                format!("module `{surface_name}` cannot be used as a value"),
            );
            Ty::Error
        } else if self.type_defs.contains_key(surface_name)
            || self.known_types.contains(surface_name)
            || self.type_aliases.contains_key(surface_name)
            || crate::lookup_builtin_type(surface_name).is_some()
            || crate::ty::is_reserved_type_name(surface_name)
        {
            self.report_error(
                TypeErrorKind::TypeUsedAsValue,
                span,
                format!("type `{surface_name}` cannot be used as a value"),
            );
            Ty::Error
        } else {
            self.resolve_identifier_variant(name, span)
        }
    }

    pub(super) fn synthesize_qualified_assoc(
        &mut self,
        path: &hew_parser::ast::QualifiedAssocExpr,
        span: &Span,
    ) -> Ty {
        self.resolve_type_expr(&path.base);
        let Some(member) = path.members.first() else {
            self.report_error(
                TypeErrorKind::PathMemberNotFound,
                span,
                "qualified associated path requires an item name".to_string(),
            );
            return Ty::Error;
        };
        if path.members.len() != 1 {
            self.report_error(
                TypeErrorKind::PathKindMismatch,
                span,
                "qualified associated values cannot continue through another path segment"
                    .to_string(),
            );
            return Ty::Error;
        }

        let trait_name = path.trait_path.source_spelling();
        let mut candidates = Vec::new();
        if self.trait_defs.contains_key(&trait_name) {
            candidates.push(trait_name.clone());
        } else if !trait_name.contains('.') && !trait_name.contains("::") {
            if let Some(owners) = self.published_bare_trait_owners.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                trait_name.clone(),
            )) {
                candidates.extend(
                    owners
                        .iter()
                        .filter(|owner| self.trait_defs.contains_key(*owner))
                        .cloned(),
                );
            }
        }
        candidates.sort_unstable();
        candidates.dedup();

        if candidates.len() > 1 {
            self.report_error_with_suggestions(
                TypeErrorKind::AssocItemAmbiguous,
                span,
                format!(
                    "associated item `{member}` is ambiguous because trait `{trait_name}` has multiple imported owners"
                ),
                candidates
                    .iter()
                    .map(|candidate| format!("qualify the trait as `{candidate}`"))
                    .collect(),
            );
            return Ty::Error;
        }
        let Some(trait_key) = candidates.first() else {
            self.report_error(
                TypeErrorKind::PathMemberNotFound,
                span,
                format!("cannot resolve trait `{trait_name}` for associated item `{member}`"),
            );
            return Ty::Error;
        };
        let info = &self.trait_defs[trait_key];
        if info
            .associated_types
            .iter()
            .any(|associated| associated.name == *member)
        {
            self.report_error(
                TypeErrorKind::PathKindMismatch,
                span,
                format!(
                    "associated item `{trait_key}.{member}` is a type and cannot be used as a value"
                ),
            );
            return Ty::Error;
        }
        if info.methods.iter().any(|method| method.name == *member) {
            self.report_error(
                TypeErrorKind::PathKindMismatch,
                span,
                format!(
                    "associated method `{trait_key}.{member}` requires method-call syntax on a value"
                ),
            );
            return Ty::Error;
        }
        self.report_error(
            TypeErrorKind::PathMemberNotFound,
            span,
            format!("trait `{trait_key}` has no associated item `{member}`"),
        );
        Ty::Error
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
    pub(in crate::check) fn variant_nominal_ty(&self, type_name: String, type_args: Vec<Ty>) -> Ty {
        if self
            .source_authorized_generated_enum_builtin(&type_name)
            .is_some()
        {
            return Ty::normalize_named(type_name, type_args);
        }
        if self.local_type_defs.contains(type_name.as_str())
            || self.source_type_defs.contains(type_name.as_str())
            || self.is_current_module_type_def(&type_name)
        {
            Ty::named(type_name, type_args)
        } else {
            Ty::normalize_named(type_name, type_args)
        }
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
            let Some(td) = self.type_defs.get(type_name.as_str()) else {
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
            let return_type = self.variant_nominal_ty(type_name.clone(), type_args.clone());
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
                // `Expr::Identifier` — this path is not reached for them.
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
        for (type_name, td) in &self.type_defs {
            if !self.local_type_defs.contains(type_name.as_str())
                && !self.source_type_defs.contains(type_name.as_str())
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
            for (type_name, td) in &self.type_defs {
                if self.local_type_defs.contains(type_name.as_str())
                    || self.source_type_defs.contains(type_name.as_str())
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
                        .filter(|candidate| self.type_defs.contains_key(candidate))
                        .or_else(|| {
                            (!self.local_type_defs.contains(type_prefix)
                                && !self.source_type_defs.contains(type_prefix))
                            .then(|| self.published_bare_type_qualified(type_prefix))
                            .flatten()
                        })
                        .unwrap_or_else(|| type_prefix.to_string())
                };
                if let Some(td) = self.type_defs.get(&canonical_type_prefix) {
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
                    if let Some(sig) = self.fn_sigs.get(variant_name) {
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
                        if let Some(td) = self.type_defs.get(canonical.as_str()) {
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
                let similar = crate::error::find_similar(
                    name,
                    self.env
                        .all_names()
                        .chain(self.fn_sigs.keys().map(String::as_str)),
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
        self.variant_nominal_ty(type_name.to_string(), args)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "index checking covers range slices, Vec runtime indexing, user Index impls, and dyn Index dispatch"
    )]
    pub(in crate::check) fn synthesize_index(
        &mut self,
        object: &Spanned<Expr>,
        index: &Spanned<Expr>,
        span: &Span,
        ctx: IndexContext,
    ) -> Ty {
        let obj_ty = self.synthesize(&object.0, &object.1);

        // C-3 range-slice (`xs[a..b]`, `xs[a..=b]`, `xs[..b]`, `xs[a..]`,
        // `xs[..]`): when the index is a range expression, the result type
        // is `Vec<T>` (a freshly-allocated copy) for `Vec<T>` receivers.
        // Each present endpoint must check against `i64`. Open endpoints
        // contribute no constraint; MIR fills them at lowering.
        // Other receivers (`Array<T, N>`, `Slice<T>`) are not supported by
        // this slice — the checker rejects with a typed-receiver diagnostic
        // that names Vec as the only supported receiver, mirroring C-2's
        // narrow surface.
        if let Expr::Range {
            start,
            end,
            inclusive: _,
        } = &index.0
        {
            if let Some(s) = start.as_deref() {
                self.check_against(&s.0, &s.1, &Ty::I64);
            }
            if let Some(e) = end.as_deref() {
                self.check_against(&e.0, &e.1, &Ty::I64);
            }
            return match &obj_ty {
                Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    args,
                    ..
                } if !args.is_empty() => {
                    let element = args[0].clone();
                    if self.validate_vec_slice_element_clone_type(&element, span) {
                        obj_ty.clone()
                    } else {
                        Ty::Error
                    }
                }
                // W3 collections-sugar S2: `s[a..b]` over `string` returns a
                // fresh owned `string`. Codepoint-bounds slice, O(n), panic on
                // invalid bounds. Endpoints are i64 (validated above).
                Ty::String => Ty::String,
                // W3 collections-sugar S2: `b[a..b]` over `bytes` returns a
                // refcounted `bytes` slice. Byte-bounds, O(1), panic on
                // invalid bounds. Endpoints are i64 (validated above).
                Ty::Bytes => Ty::Bytes,
                _ => {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "cannot range-slice `{}`; range-slice syntax `xs[a..b]` is \
                             supported only for `Vec<T>`, `string`, and `bytes` receivers",
                            obj_ty.user_facing()
                        ),
                    );
                    Ty::Error
                }
            };
        }

        let resolved_obj = self.subst.resolve(&obj_ty);
        if let Some((_, child_ty)) = resolved_obj.as_supervisor_pool() {
            let idx_actual = self.synthesize(&index.0, &index.1);
            let idx_resolved = self.subst.resolve(&idx_actual);
            if Self::is_narrower_signed_int(&idx_resolved) {
                self.numeric_operand_coercions.insert(
                    SpanKey::in_module(&index.1, self.current_module_idx),
                    Ty::I64,
                );
            } else {
                self.check_against(&index.0, &index.1, &Ty::I64);
            }
            if ctx == IndexContext::AssignTarget {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "supervisor pool members cannot be assigned through indexed access".to_string(),
                );
                return Ty::Error;
            }
            self.pool_accessor_sites.insert(
                SpanKey::in_module(span, self.current_module_idx),
                crate::check::types::PoolAccessor {
                    kind: crate::check::types::PoolAccessorKind::Index,
                },
            );
            return Ty::child_ref(child_ty.clone());
        }
        if let Ty::TraitObject { traits } = &resolved_obj {
            for bound in traits {
                if bound.trait_name != "Index" {
                    continue;
                }
                self.check_against(&index.0, &index.1, &Ty::I32);
                if let Some((_, output_ty)) = bound
                    .assoc_bindings
                    .iter()
                    .find(|(name, _)| name == "Output")
                {
                    self.record_dyn_index_method_call(traits, bound, span);
                    return output_ty.clone();
                }
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "`[]` over `dyn Index` requires an `Output` associated-type binding"
                        .to_string(),
                );
                return Ty::Error;
            }
        }

        match &resolved_obj {
            // Vec keeps the existing runtime-backed indexing ABI. The std
            // `Index` impl exposes the trait surface, but MIR still owns the
            // bounds-check + hew_vec_get_T lowering and that ABI takes i64.
            //
            // Implicit index-site widening: accept a signed integer narrower
            // than i64 (i8/i16/i32) as a Vec index.  The operand widens to i64
            // at the call site; the element result type is NOT changed (LESSONS
            // `widen-operands-not-result-when-tightening-int-coercion`).
            // Publish the operand widening so HIR inserts an explicit cast
            // before the runtime bounds check.
            Ty::Named {
                builtin: Some(BuiltinType::Vec),
                args,
                ..
            } if !args.is_empty() => {
                let idx_actual = self.synthesize(&index.0, &index.1);
                let idx_resolved = self.subst.resolve(&idx_actual);
                if Self::is_narrower_signed_int(&idx_resolved) {
                    self.numeric_operand_coercions.insert(
                        SpanKey::in_module(&index.1, self.current_module_idx),
                        Ty::I64,
                    );
                } else {
                    self.check_against(&index.0, &index.1, &Ty::I64);
                }
                if matches!(ctx, IndexContext::Read) {
                    if !self.validate_vec_index_borrow_surface(&args[0], span) {
                        return Ty::Error;
                    }
                    // D432: an element with no clone is read as a loan of the
                    // slot the vector still owns, never copied out.
                    match self.vec_iteration_element_mode(&args[0], span) {
                        Some(super::types::VecIterationMode::Borrow) => {
                            self.borrowed_element_index_reads
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                        }
                        Some(super::types::VecIterationMode::Clone) => {}
                        None => return Ty::Error,
                    }
                }
                self.indexed_place_operations.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    (
                        crate::RuntimeCallFamily::Vector(crate::VecValueOp::Index),
                        crate::RuntimeCallFamily::Vector(crate::VecValueOp::Set),
                    ),
                );
                if matches!(ctx, IndexContext::AssignTarget) {
                    self.record_resolved_vec_call("set", &args[0], span);
                }
                args[0].clone()
            }
            // `m[k]` over `HashMap<K, V>` is the trait-routed `Index<K>`
            // accessor (`<HashMap<K, V> as Index>::Output = V`), mirroring
            // `v[i]` over `Vec<T>`.
            //
            // Read context (`let x = m[k]`): the TRAPPING accessor
            // (`Index::at`) — result type is the BARE value `V`. A missing key
            // aborts with `IndexOutOfBounds` (the map analogue of a `v[i]`
            // out-of-bounds trap), so there is no `Option` round-trip. No
            // resolved `.get` call is recorded here: the MIR `Index` node lowers
            // directly to the `hew_hashmap_get_clone_layout` trap choke
            // (`lower_hashmap_index_trap`). Callers who want the non-aborting
            // outcome use `m.get(k) -> Option<V>` instead.
            //
            // Write context (`m[k] = v`): the assignment-target type is the
            // bare value `V` (so the RHS checks against `V`), and the checker
            // records a `ResolvedCall` to `hew_hashmap_insert_layout` at this
            // span. The key bound is the existing `K: Hash + Eq` admission
            // contract — the same one every HashMap method call enforces.
            Ty::Named {
                builtin: Some(BuiltinType::HashMap),
                args,
                ..
            } if args.len() == 2 => {
                let key_ty = args[0].clone();
                let val_ty = args[1].clone();
                self.check_against(&index.0, &index.1, &key_ty);
                // Enforce `K: Hash + Eq` and reject unsafe key/value element
                // types, exactly as the method-call path does — for both the
                // read (trap) and the write (insert) surfaces.
                if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                // The trapping read clones the value out of its slot; the
                // write only moves one in.
                if ctx == IndexContext::Read
                    && !self.validate_collection_value_clone_type(
                        &val_ty,
                        BuiltinType::HashMap,
                        "m[k]",
                        span,
                    )
                {
                    return Ty::Error;
                }
                self.indexed_place_operations.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    (
                        crate::RuntimeCallFamily::Map(crate::runtime_call::MapValueOp::Index),
                        crate::RuntimeCallFamily::Map(crate::runtime_call::MapValueOp::Insert),
                    ),
                );
                match ctx {
                    // Trapping bare-`V` read: no `.get` resolved call; MIR's
                    // `Index` node owns the `hew_hashmap_get_clone_layout` trap
                    // lowering.
                    IndexContext::Read => val_ty,
                    // Write target: record the `hew_hashmap_insert_layout` call
                    // at the index span (the same one `m.insert(k, v)` emits).
                    IndexContext::AssignTarget => {
                        self.record_resolved_hashmap_call("insert", &key_ty, &val_ty, span);
                        val_ty
                    }
                }
            }
            // W3 collections-sugar S2: `s[i]` over `string` returns a `char`
            // at codepoint offset, O(n), panic on OOB. Index is i64. The
            // checker is authoritative; MIR will route to `hew_string_index`.
            Ty::String => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                Ty::Char
            }
            // W3 collections-sugar S2: `b[i]` over `bytes` returns a `u8`
            // at byte offset, O(1), panic on OOB. Index is i64. MIR will
            // route to `hew_bytes_index`.
            Ty::Bytes => {
                self.indexed_place_operations.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    (
                        crate::RuntimeCallFamily::BytesIndex,
                        crate::RuntimeCallFamily::BytesSet,
                    ),
                );
                self.check_against(&index.0, &index.1, &Ty::I64);
                Ty::U8
            }
            Ty::Named { name, args, .. } => {
                if self.type_satisfies_trait_bound(&resolved_obj, "Index") {
                    let expected_key = self
                        .lookup_named_method_sig(name, args, "at")
                        .and_then(|sig| sig.params.first().cloned())
                        .unwrap_or(Ty::I32);
                    self.check_against(&index.0, &index.1, &expected_key);
                    let output = self.project_assoc_types(&Ty::AssocType {
                        base: Box::new(resolved_obj.clone()),
                        trait_name: "Index".into(),
                        assoc_name: "Output".into(),
                    });
                    if matches!(output, Ty::AssocType { .. }) {
                        self.report_error(
                            TypeErrorKind::AssocTypeProjectionFailed {
                                type_name: resolved_obj.user_facing().to_string(),
                                trait_name: "Index".to_string(),
                                assoc_name: "Output".to_string(),
                            },
                            span,
                            format!(
                                "could not project associated type `<{} as Index>.Output` \
                                 while checking `[]`; ensure the impl defines \
                                 `type Output = ...`",
                                resolved_obj.user_facing()
                            ),
                        );
                        return Ty::Error;
                    }
                    return output;
                }

                self.check_against(&index.0, &index.1, &Ty::I64);
                // Bracket indexing via a named type's `.get()` method is no longer
                // supported. Use the explicit method call instead.
                if self.lookup_named_method_sig(name, args, "get").is_some() {
                    self.report_error_with_suggestions(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "cannot index into `{}` with `[]`; use `.get(k)` instead",
                            resolved_obj.user_facing()
                        ),
                        vec![format!("use `.get(k)` on `{}`", resolved_obj.user_facing())],
                    );
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("cannot index into `{}`", resolved_obj.user_facing()),
                    );
                }
                Ty::Error
            }
            Ty::Array(elem, _) => {
                self.indexed_place_operations.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    (
                        crate::RuntimeCallFamily::Array(crate::runtime_call::ArrayValueOp::Index),
                        crate::RuntimeCallFamily::Array(crate::runtime_call::ArrayValueOp::Set),
                    ),
                );
                self.check_against(&index.0, &index.1, &Ty::I64);
                if matches!(ctx, IndexContext::Read) {
                    match self.vec_iteration_element_mode(elem, span) {
                        Some(super::types::VecIterationMode::Borrow) => {
                            self.borrowed_element_index_reads
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                        }
                        Some(super::types::VecIterationMode::Clone) => {}
                        None => return Ty::Error,
                    }
                }
                (**elem).clone()
            }
            Ty::Slice(elem) => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                (**elem).clone()
            }
            other => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                if *other != Ty::Error {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("cannot index into `{}`", other.user_facing()),
                    );
                }
                Ty::Error
            }
        }
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

    /// `await` joins a task and nothing else. A plain call suspends the caller
    /// on its own, so `await` adds nothing there; every other operand is not a
    /// task and is refused with the move that replaces it.
    pub(super) fn check_await_operand(&mut self, expr: &Expr, span: &Span, ty: &Ty) {
        if matches!(ty, Ty::Error) {
            return;
        }
        // Every call waits on its own, an actor call included (U383): `await`
        // adds nothing there. `fork` is how a call runs concurrently, and
        // `await` then joins that task.
        if matches!(expr, Expr::Call { .. } | Expr::MethodCall { .. }) {
            self.errors.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::InvalidOperation,
                span: span.clone(),
                message: "`await` on a plain call adds nothing: the call suspends on its own"
                    .to_string(),
                notes: vec![],
                suggestions: vec![
                    "remove `await`, or fork the call to run it concurrently".to_string()
                ],
                source_module: self.current_module.clone(),
            });
        } else {
            let mut suggestions = vec!["remove `await`, or fork a call to get a task".to_string()];
            if matches!(
                ty,
                Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    ..
                }
            ) {
                suggestions.push(
                    "`await` over a vector joins a vector of task handles, so fill it with \
                     forked calls"
                        .to_string(),
                );
            }
            if ty.as_local_actor_ref().is_some() {
                suggestions
                    .push("`closed(actor)` waits for an actor to finish terminating".to_string());
            }
            self.report_error_with_suggestions(
                TypeErrorKind::InvalidOperation,
                span,
                format!("`await` joins a task; `{}` is not one", ty.user_facing()),
                suggestions,
            );
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "concurrency variants (scope/select/join/spawn/unsafe/timeout)"
    )]
    pub(in crate::check) fn synthesize_concurrency(&mut self, expr: &Expr, span: &Span) -> Ty {
        match expr {
            Expr::ForkChild { expr: child } => {
                let children: Vec<&Spanned<Expr>> = match &child.0 {
                    Expr::Array(elements) => elements.iter().map(ArrayElement::expr).collect(),
                    Expr::Tuple(children) => children.iter().collect(),
                    _ => vec![child.as_ref()],
                };
                for branch in &children {
                    if !matches!(branch.0, Expr::Call { .. } | Expr::MethodCall { .. }) {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &branch.1,
                            "fork expects a call or a batch of calls; use fork { ... } for a body"
                                .to_string(),
                        );
                    }
                    self.suspension_operands
                        .insert(SpanKey::in_module(&branch.1, self.current_module_idx));
                }
                let ret_ty = self.synthesize(&child.0, &child.1);
                for branch in &children {
                    self.record_fork_call_inputs(branch);
                }
                Ty::Task(Box::new(ret_ty))
            }
            Expr::ForkBlock { body } => {
                // Share capture identity and child return inference with closures.
                let synthetic_body = (Expr::Block(body.clone()), span.clone());
                let lambda_ty = self.check_lambda(
                    true,
                    &[],
                    None,
                    &[],
                    None,
                    &synthetic_body,
                    None,
                    span,
                    false,
                    true,
                );

                self.check_fork_transfer(expr, span, &lambda_ty);

                // Ordinary parameters are borrowed at Hew call boundaries.
                // Value snapshots acquire an independent child owner; an affine
                // borrowed parameter or explicit view cannot escape that way.
                let capture_key = SpanKey::in_module(span, self.current_module_idx);
                if let Some(captures) = self.closure_capture_facts.get(&capture_key).cloned() {
                    for capture in captures {
                        let capture_is_copy = self.ty_is_non_owning(&capture.ty);
                        let borrowed_parameter = !capture_is_copy
                            && capture.acquisition == crate::ClosureCaptureAcquisition::Move
                            && self.env.lookup_ref(&capture.name).is_some_and(|binding| {
                                binding.id == capture.binding_id && binding.is_param()
                            });
                        let borrowed_view = matches!(capture.ty, Ty::Borrow { .. });
                        if borrowed_parameter || borrowed_view {
                            self.errors.push(TypeError::new(
                                TypeErrorKind::ForkBorrowCapture {
                                    binding: capture.name.clone(),
                                },
                                capture.use_span,
                                format!(
                                    "fork body cannot borrow parent binding `{}` across the child boundary",
                                    capture.name
                                ),
                            ));
                        }
                    }
                }
                match lambda_ty {
                    Ty::Function { ret, .. } | Ty::Closure { ret, .. } => Ty::Task(ret),
                    _ => Ty::Error,
                }
            }
            Expr::SpawnLambdaActor {
                is_move,
                params,
                return_type,
                body,
            } => {
                // A lambda actor is an actor declaration without a source
                // name. Mint its identity here, keyed by the exact span of
                // the `actor` expression, so HIR can synthesize the actor
                // declaration and its single receive handler against a
                // resolver-owned `DefId` like every named actor.
                self.declare_lambda_actor(span);
                // Synthesise the body without propagating the return-type annotation as
                // a contextual hint.  This lets us extract the actual body return type
                // and emit targeted diagnostics rather than generic Mismatch errors:
                //   - E_LAMBDA_RETURN_TYPE_MISMATCH: body return type ≠ declared reply type.
                //   - E_LAMBDA_SELF_ESCAPE: body returns an actor handle (leaks the actor).
                // Bidirectional hint for the body is intentionally omitted here (slight
                // inference degradation for actor bodies) to keep diagnostics clean.
                // WHEN-OBSOLETE: if a richer bidirectional inference mode is added that
                // can propagate a "return type hint" without actually checking the body
                // against it, restore the hint while keeping targeted diagnostics.
                //
                // Pass is_actor_body=true so check_call inside the body can permit
                // recursive self-sends (a Duplex capture called from within its own
                // actor body). Nested fn-closures inside the body pass is_actor_body=false,
                // so they correctly see in_lambda_actor_body=false.
                let lambda_ty = self.check_lambda(
                    *is_move,
                    &[],
                    None,
                    params,
                    None,
                    body,
                    None,
                    span,
                    true,
                    false,
                );
                // Check captures for Send (E_DUPLEX_NON_SEND).
                let body_ret = match &lambda_ty {
                    Ty::Function { ret, .. } | Ty::Closure { ret, .. } => {
                        let mut non_send_captures = vec![];
                        if let Ty::Closure { captures, .. } = &lambda_ty {
                            let mut seen = HashSet::new();
                            for capture in captures {
                                if !self.registry.implements_marker(capture, MarkerTrait::Send)
                                    && seen.insert(capture.clone())
                                {
                                    non_send_captures.push(capture.clone());
                                }
                            }
                        }
                        for capture in &non_send_captures {
                            self.report_error(
                                TypeErrorKind::InvalidSend,
                                span,
                                format!(
                                    "cannot capture `{}` in spawned actor: type is not Send (E_DUPLEX_NON_SEND)",
                                    capture.user_facing()
                                ),
                            );
                        }
                        (**ret).clone()
                    }
                    _ => Ty::Unit,
                };
                // E_LAMBDA_SELF_ESCAPE: the lambda body returns an actor handle.
                // A lambda body that produces an `actor(...) -> ...` handle (lambda-actor
                // handle) or a raw `Duplex<...>` channel is leaking a move-only handle outside
                // the actor boundary — the handle's lifetime is bound to the let-binding
                // site, not to values the body produces.
                //
                // CONSERVATIVE APPROXIMATION (slice 2): any handle-typed body is rejected,
                // including the "factory" pattern (actor body returns a *different* actor's
                // handle). Slice 3 can narrow this to only reject handle values that alias
                // a capture from the enclosing let-binding, using MIR-level alias analysis.
                // Until then, returning any actor handle from an actor body is forbidden.
                //
                // WHEN-OBSOLETE: slice 3 adds MIR-level self-ref weak capture that covers
                // the runtime dimension of self-escape; this is the static type-level gate.
                if body_ret.as_actor_fn().is_some() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "actor lambda body returns an actor handle — actor handles cannot \
                         escape the actor boundary via a return value (E_LAMBDA_SELF_ESCAPE); \
                         use an actor with no return type instead"
                            .to_string(),
                    );
                }
                // Build the message type from the parameter list.
                // Single param → that param's type; multiple params → Tuple.
                // No params → Unit (actor takes no argument).
                let msg_ty = {
                    let param_types: Vec<Ty> = params
                        .iter()
                        .map(|p| {
                            p.ty.as_ref().map_or(Ty::Var(TypeVar::fresh()), |ann| {
                                self.resolve_type_expr(ann)
                            })
                        })
                        .collect();
                    match param_types.len() {
                        0 => Ty::Unit,
                        1 => param_types.into_iter().next().unwrap(),
                        _ => Ty::Tuple(param_types),
                    }
                };
                // The reply type determines send vs ask:
                //   send-shaped (`actor |p| { ... }` — no explicit return type, or `-> ()`)
                //     → `actor(Msg) -> ()` — call-site returns `Result<(), SendError>`
                //   ask-shaped (`actor |p| -> Reply { ... }`)
                //     → `actor(Msg) -> Reply` — call-site returns `Result<Reply, AskError>`
                let reply_ty = if let Some(ret_ann) = return_type.as_ref() {
                    let resolved = self.resolve_type_expr(ret_ann);
                    if matches!(resolved, Ty::Unit) {
                        Ty::Unit
                    } else {
                        // E_LAMBDA_RETURN_TYPE_MISMATCH: body return type ≠ declared return type
                        // for ask-shaped actors. The generic Mismatch that check_lambda would
                        // normally emit is suppressed because we passed `None` as the return
                        // annotation hint; we emit the targeted diagnostic here instead.
                        let resolved_body = self.subst.resolve(&body_ret);
                        if !matches!(resolved_body, Ty::Error | Ty::Var(_)) {
                            let snapshot = self.subst.snapshot();
                            let mismatch =
                                !self.try_unify_with_owner_identity(&resolved_body, &resolved);
                            self.subst.restore(snapshot);
                            if mismatch {
                                self.report_error(
                                    TypeErrorKind::ReturnTypeMismatch,
                                    span,
                                    format!(
                                        "ask-shaped actor body returns `{}` but the declared reply \
                                         type is `{}` (E_LAMBDA_RETURN_TYPE_MISMATCH)",
                                        resolved_body.user_facing(),
                                        resolved.user_facing()
                                    ),
                                );
                            }
                        }
                        // Validate: ask-shaped reply must be Send (crosses actor boundary).
                        if !self
                            .registry
                            .implements_marker(&resolved, MarkerTrait::Send)
                        {
                            self.report_error(
                                TypeErrorKind::InvalidSend,
                                span,
                                format!(
                                    "ask-shaped actor reply type `{}` is not Send (E_DUPLEX_NON_SEND)",
                                    resolved.user_facing()
                                ),
                            );
                        }
                        resolved
                    }
                } else {
                    Ty::Unit
                };
                // Msg type must also be Send (it crosses the actor boundary on call).
                if !matches!(msg_ty, Ty::Unit | Ty::Var(_))
                    && !self.registry.implements_marker(&msg_ty, MarkerTrait::Send)
                {
                    self.report_error(
                        TypeErrorKind::InvalidSend,
                        span,
                        format!(
                            "lambda actor message type `{}` is not Send (E_DUPLEX_NON_SEND)",
                            msg_ty.user_facing()
                        ),
                    );
                }
                Ty::actor_fn(msg_ty, reply_ty)
            }
            Expr::Scope { body: block } => {
                self.task_scope_depth += 1;
                let ty = self.check_block(block, None);
                self.task_scope_depth -= 1;
                ty
            }
            Expr::ScopeDeadline { duration, body } => {
                self.check_against(&duration.0, &duration.1, &Ty::Duration);
                self.task_scope_depth += 1;
                let ty = self.check_block(body, None);
                self.task_scope_depth -= 1;
                ty
            }
            Expr::UnsafeBlock(block) => {
                let prev = self.in_unsafe;
                self.in_unsafe = true;
                let ty = self.check_block(block, None);
                self.in_unsafe = prev;
                ty
            }
            Expr::Select { arms, timeout } => {
                // WASM-TODO(suspending-select): compile the readiness waitset for wasm32.
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Select);
                if arms.is_empty() && timeout.is_none() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "a `select` needs at least one arm: a source arm \
                         (`name from source => body`), or an `after` timer arm"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                let mut result_ty: Option<Ty> = None;
                let prepared_depth = self.prepared_select_tasks.len();
                // Only the BODIES of a select are alternatives. Every arm's
                // source is prepared before dispatch chooses a winner — all the
                // asks are issued, all the receivers polled — so the sources run
                // on one execution, in order, and handing the same affine value
                // to two of them is a real double transfer. They thread
                // sequentially; the same goes for the timeout duration, which
                // arms the deadline before any arm fires.
                let mut source_tys = Vec::with_capacity(arms.len());
                let mut sources = Vec::with_capacity(arms.len());
                for arm in arms {
                    self.env.push_scope();
                    let (ty, source) = self.synthesize_select_source(&arm.source.0, &arm.source.1);
                    if matches!(source, Some(super::CheckedSelectSource::TaskAwait { .. })) {
                        if let Some((root, path)) = self.expr_place(&arm.source.0) {
                            if let Some(binding) = self.env.lookup_ref(&root) {
                                self.prepared_select_tasks
                                    .push(super::types::PreparedSelectTask {
                                        binding: binding.id,
                                        path,
                                        span: arm.source.1.clone(),
                                    });
                            }
                        }
                    }
                    source_tys.push(ty);
                    sources.push(source);
                    self.env.pop_scope();
                }
                if let Some(checked) = sources.iter().cloned().collect::<Option<Vec<_>>>() {
                    self.select_sources
                        .insert(SpanKey::in_module(span, self.current_module_idx), checked);
                }
                if let Some(tc) = timeout {
                    self.check_against(&tc.duration.0, &tc.duration.1, &Ty::Duration);
                }
                self.prepared_select_tasks.truncate(prepared_depth);

                // Dispatch happens here: from this state exactly one body runs.
                let entry = self.env.ownership_snapshot();
                let mut arm_exits = Vec::with_capacity(arms.len() + 1);
                for ((arm, source_ty), source) in arms.iter().zip(&source_tys).zip(&sources) {
                    self.env.push_scope();
                    self.env.restore_ownership(&entry);
                    if matches!(source, Some(super::CheckedSelectSource::TaskAwait { .. }))
                        && !self.reject_borrowed_consumption(&arm.source.0, &arm.source.1)
                    {
                        self.mark_expr_moved(&arm.source.0, &arm.source.1);
                    }
                    self.bind_pattern(&arm.binding.0, source_ty, false, &arm.binding.1);
                    let body_ty = if let Some(expected) = &result_ty {
                        self.check_against(&arm.body.0, &arm.body.1, expected)
                    } else {
                        self.synthesize(&arm.body.0, &arm.body.1)
                    };
                    arm_exits.push(BranchArmExit {
                        ownership: self.env.ownership_snapshot(),
                        diverges: Self::arm_skips_join(&body_ty),
                    });
                    if result_ty.is_none() {
                        result_ty = Some(body_ty);
                    }
                    self.env.pop_scope();
                }
                if let Some(tc) = timeout {
                    self.env.restore_ownership(&entry);
                    let timeout_ty = self.synthesize(&tc.body.0, &tc.body.1);
                    arm_exits.push(BranchArmExit {
                        ownership: self.env.ownership_snapshot(),
                        diverges: Self::arm_skips_join(&timeout_ty),
                    });
                    if let Some(expected) = &result_ty {
                        self.expect_type(expected, &timeout_ty, &tc.body.1);
                    } else {
                        result_ty = Some(timeout_ty);
                    }
                }
                self.join_branch_ownership(&entry, &arm_exits);
                result_ty.unwrap_or(Ty::Unit)
            }
            Expr::Race(branches) => self.synthesize_race(branches, span),
            Expr::GenBlock { body } => {
                // A98 / Q98: generator blocks inside actor receive handlers are
                // permanently forbidden.  The scheduler holds the actor-state lock
                // for the entire handler invocation; there is no safe point to
                // yield mid-handler.  This is a typed compile error, not a runtime
                // trap.
                if self.in_actor_handler_context {
                    self.report_error(
                        TypeErrorKind::GenBlockInActorReceive,
                        span,
                        "`gen { }` blocks are forbidden inside \
                         actor receive handlers — the scheduler holds the actor-state lock for \
                         the entire handler invocation; use a named generator function outside \
                         the handler instead"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                // Typed gen{} checking.
                //
                // Two fresh type-variables seed independent inference:
                //   yield_var — unified by each `yield <expr>` site in the body.
                //   return_var — unified with the body's tail expression type
                //                (and by explicit `return <expr>` statements when
                //                 Stmt::Return extracts the Return component from
                //                 the enclosing Generator type).
                //
                // After the body, EmptyGenerator fires only when the body is
                // genuinely empty of generator-relevant content: yield_var is
                // still unbound AND the Return component is Unit or Never (i.e.
                // no tail expression or explicit `return <value>` provided a
                // useful return type).  `gen { return 1; }` and `gen { 1 }` are
                // both valid generators with inferred Return=i64.
                //
                let yield_var = TypeVar::fresh();
                let return_var = TypeVar::fresh();
                let gen_ty = Ty::generator(Ty::Var(yield_var), Ty::Var(return_var));

                let prev_in_generator = self.in_generator;
                let prev_return_type = self.current_return_type.take();
                let previous_defer = self.deferred_body.take();
                let prev_fails = std::mem::replace(&mut self.current_fails, false);
                self.in_generator = true;
                self.current_return_type = Some(gen_ty.clone());

                let effect_body = super::effects::EffectBody::GeneratorBlock(SpanKey::in_module(
                    span,
                    self.current_module_idx,
                ));
                self.effect_graph
                    .bodies
                    .entry(effect_body.clone())
                    .or_default();
                let previous_effect_body = self.effect_graph.current_body.replace(effect_body);
                let body_ty = self.check_block(body, None);
                self.effect_graph.current_body = previous_effect_body;

                self.in_generator = prev_in_generator;
                self.current_return_type = prev_return_type;
                self.deferred_body = previous_defer;
                self.current_fails = prev_fails;

                // Unify the tail-expression type with the Return type-variable.
                // Never / Error propagate vacuously (unify is a no-op for Error).
                self.expect_type(&Ty::Var(return_var), &body_ty, span);

                let resolved_yield = self.subst.resolve(&Ty::Var(yield_var));
                let resolved_return = self.subst.resolve(&Ty::Var(return_var));

                // EmptyGenerator: no yield AND no useful return path.
                // A resolved return_var (from a tail expr or `return <expr>`)
                // means the body is doing real work even without a yield site.
                let yield_unresolved = matches!(resolved_yield, Ty::Var(_));
                let return_trivial = matches!(resolved_return, Ty::Var(_) | Ty::Unit | Ty::Never);

                if yield_unresolved && return_trivial {
                    self.report_error(
                        TypeErrorKind::EmptyGenerator,
                        span,
                        "`gen { }` body contains no `yield` expression \
                         and no value-producing tail expression or `return`; \
                         the yield type cannot be inferred — add at least one \
                         `yield <value>` statement"
                            .to_string(),
                    );
                    Ty::Error
                } else {
                    // If yield_var is still unresolved (body has a return but no
                    // yield), the generator never yields — represent that as Never.
                    let final_yield = if yield_unresolved {
                        Ty::Never
                    } else {
                        resolved_yield
                    };
                    Ty::generator(final_yield, resolved_return)
                }
            }
            _ => Ty::Unit,
        }
    }

    pub(in crate::check) fn check_expr_with_expected(
        &mut self,
        expr: &Expr,
        span: &Span,
        expected: &Ty,
    ) -> Ty {
        match expr {
            Expr::Block(block) => self.check_block_expr_with_expected(expr, block, span, expected),
            // An `unsafe` block is a block: its tail flows to the surrounding
            // expectation, so `.Ok(x)` resolves inside one.
            Expr::UnsafeBlock(block) => {
                let prev = self.in_unsafe;
                self.in_unsafe = true;
                let ty = self.check_block_expr_with_expected(expr, block, span, expected);
                self.in_unsafe = prev;
                ty
            }
            _ => self.check_against(expr, span, expected),
        }
    }

    pub(super) fn check_block_expr_with_expected(
        &mut self,
        expr: &Expr,
        block: &Block,
        span: &Span,
        expected: &Ty,
    ) -> Ty {
        let actual = self.check_block(block, Some(expected));
        let result = if matches!(actual, Ty::Never | Ty::Error) {
            actual.clone()
        } else {
            let n = self.errors.len();
            self.expect_type(expected, &actual, span);
            if self.errors.len() > n {
                Ty::Error
            } else {
                actual.clone()
            }
        };
        // A block's value IS its trailing expression's value. When that
        // tail fails to meet the expectation, `check_against` reports
        // the mismatch on the tail's own span, PUBLISHES the tail's
        // recovered type, and returns the error placeholder to poison
        // the caller. Publish the same recovered type for the block so
        // the two agree: the produced-value graph treats the tail as
        // the block's identity dependency and rejects a disagreement
        // ("identity dependency changes type from T to Error"), and
        // consumers that read published types -- hover -- surface the
        // placeholder as an unknown type. The placeholder is still what
        // this call returns, so callers keep their poisoned result.
        let published = if matches!(result, Ty::Error) {
            block
                .trailing_expr
                .as_ref()
                .and_then(|tail| {
                    self.expr_types
                        .get(&SpanKey::in_module(&tail.1, self.current_module_idx))
                        .cloned()
                })
                .unwrap_or_else(|| result.clone())
        } else {
            result.clone()
        };
        self.publish_checked_expression(expr, span, published);
        result
    }

    /// Check: verify expression against expected type (top-down).
    pub(in crate::check) fn check_against(
        &mut self,
        expr: &Expr,
        span: &Span,
        expected: &Ty,
    ) -> Ty {
        let result = self.check_against_inner(expr, span, expected);
        self.publish_checked_expression(expr, span, result)
    }
}
