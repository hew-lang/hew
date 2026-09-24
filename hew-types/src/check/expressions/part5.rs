//! Split from `expressions.rs`: checker methods, part 5 of 5.
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
    #[expect(
        clippy::too_many_lines,
        reason = "struct and enum-variant initialization share one exact-owner diagnostic path"
    )]
    pub(in crate::check) fn check_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Spanned<Expr>)],
        type_args: Option<&[Spanned<TypeExpr>]>,
        base: Option<&Spanned<Expr>>,
        span: &Span,
    ) -> Ty {
        // Every field is initialized exactly once: a base supplies the fields
        // the literal does not name, and naming one twice leaves no reading
        // that says which value wins.
        let mut named: HashSet<&str> = HashSet::new();
        for (field_name, (_, field_span)) in fields {
            if !named.insert(field_name.as_str()) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    field_span,
                    format!("record literal names field `{field_name}` more than once"),
                );
            }
        }

        // Expression-position struct variants use the final dotted surface
        // (`Type.Variant { ... }`). Normalize that spelling only after the
        // owner has been selected by checker authority: a lexical nominal
        // binding for `Type.Variant`, or the exact export table for
        // `module.Type.Variant`. The resulting registry key retains the full
        // declaration owner and never scans by final segment.
        let dotted_struct_variant = if name.contains("::") {
            None
        } else {
            let segments = name.split('.').collect::<Vec<_>>();
            match segments.as_slice() {
                [surface_type, variant] if self.env.lookup_ref(surface_type).is_none() => self
                    .source_nominal_declaration(surface_type)
                    .and_then(|canonical_type| {
                        self.lookup_type_def(&canonical_type)
                            .filter(|type_def| {
                                matches!(
                                    type_def.variants.get(*variant),
                                    Some(VariantDef::Struct(_))
                                )
                            })
                            .map(|_| format!("{canonical_type}::{variant}"))
                    }),
                [module_short, surface_type, variant]
                    if self.env.lookup_ref(module_short).is_none() =>
                {
                    self.resolve_module_variant(module_short, surface_type, variant)
                        .filter(|(_, variant_def)| matches!(variant_def, VariantDef::Struct(_)))
                        .map(|_| {
                            self.used_modules.borrow_mut().insert(ImportKey::in_file(
                                self.current_module.clone(),
                                self.current_module_idx,
                                (*module_short).to_string(),
                            ));
                            format!(
                                "{}.{surface_type}::{variant}",
                                self.canonical_module_import_owner(module_short)
                            )
                        })
                }
                _ => None,
            }
        };
        // A plain record constructor uses the two-segment `module.Type`
        // surface, which overlaps syntactically with a local
        // `Type.StructVariant`. Give the proven local variant above first
        // refusal, then resolve a lexical module binding through the same
        // export table as annotation-position qualified types. The lexical
        // alias is never a nominal identity: carry the declaration's full
        // source owner into the shared record-initialiser path.
        let module_record_name = if dotted_struct_variant.is_none() && !name.contains("::") {
            let segments = name.split('.').collect::<Vec<_>>();
            match segments.as_slice() {
                [module_short, type_name]
                    if self.env.lookup_ref(module_short).is_none()
                        && self.module_binding_in_current_file(module_short) =>
                {
                    self.used_modules.borrow_mut().insert(ImportKey::in_file(
                        self.current_module.clone(),
                        self.current_module_idx,
                        (*module_short).to_string(),
                    ));
                    let Some(_) = self.resolve_module_type(module_short, type_name) else {
                        let similar = self
                            .module_type_exports_for_binding(module_short)
                            .map(|set| {
                                crate::error::find_similar(
                                    type_name,
                                    set.iter().map(String::as_str),
                                )
                            })
                            .unwrap_or_default();
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!("module `{module_short}` has no exported type `{type_name}`"),
                            similar,
                        );
                        return Ty::Error;
                    };
                    Some(format!(
                        "{}.{type_name}",
                        self.canonical_module_import_owner(module_short)
                    ))
                }
                _ => None,
            }
        } else {
            None
        };
        let name = dotted_struct_variant
            .as_deref()
            .or(module_record_name.as_deref())
            .unwrap_or(name);
        let Ok(canonical_lifecycle_name) =
            self.canonicalize_source_lifecycle_value_path(name, span)
        else {
            return Ty::Error;
        };
        let name = canonical_lifecycle_name.as_deref().unwrap_or(name);

        // Module-qualified diagnostic pre-pass: when `name` has the shape
        // `module.Type::Variant` and `module` is a known module alias, route
        // the failure modes (no exported type / no such variant) through the
        // same fail-closed diagnostics used by `check_field_access`'s
        // module-qualified pre-dispatch.  Without this pre-pass the
        // enum-variant fallback in the main body falls through to
        // "undefined type `module.Type::Variant`" which leaks the
        // qualified-name layout into the diagnostic and gives the user no
        // actionable signal.  Success cases (both type and variant exist) fall
        // through to the existing struct/enum-variant init logic.
        let mut resolved_module_variant_name = None;
        if let Some(dot) = name.find('.') {
            let module_short = &name[..dot];
            if self.module_binding_in_current_file(module_short) {
                let after_dot = &name[dot + 1..];
                if let Some(colon) = after_dot.find("::") {
                    let type_name = &after_dot[..colon];
                    let variant_name = &after_dot[colon + 2..];
                    self.used_modules.borrow_mut().insert(ImportKey::in_file(
                        self.current_module.clone(),
                        self.current_module_idx,
                        module_short.to_string(),
                    ));
                    let Some(td) = self.resolve_module_type(module_short, type_name) else {
                        let similar = self
                            .module_type_exports_for_binding(module_short)
                            .map(|set| {
                                crate::error::find_similar(
                                    type_name,
                                    set.iter().map(String::as_str),
                                )
                            })
                            .unwrap_or_default();
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!("module `{module_short}` has no exported type `{type_name}`"),
                            similar,
                        );
                        return Ty::Error;
                    };
                    if !td.variants.contains_key(variant_name) {
                        let similar = crate::error::find_similar(
                            variant_name,
                            td.variants.keys().map(String::as_str),
                        );
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!(
                                "type `{module_short}.{type_name}` has no variant `{variant_name}`"
                            ),
                            similar,
                        );
                        return Ty::Error;
                    }
                    // Both type and variant exist. Carry the exact declaration
                    // owner into the shared struct-variant path; retaining the
                    // lexical module alias here would leave both its surface
                    // alias and the full declaration as candidates.
                    resolved_module_variant_name = Some(format!(
                        "{}.{type_name}::{variant_name}",
                        self.canonical_module_import_owner(module_short)
                    ));
                }
            }
        }
        let name = resolved_module_variant_name.as_deref().unwrap_or(name);
        // Fail closed under qualified-by-default before binding a bare record
        // constructor: a bare name published by more than one module is
        // ambiguous, and one exported but published by none is not in scope.
        // Without this gate the construction falls through to `lookup_type_def`
        // and silently binds a last-write-wins bare def, then trips a confusing
        // downstream MIR field-order failure. The `::` enum-variant and
        // explicitly module-qualified spellings already routed above are left
        // untouched (they carry a `.` or `::` and never match a bare name).
        let is_bare_constructor = !name.contains('.') && !name.contains("::");
        if is_bare_constructor && self.report_bare_type_scope_error(name, span) {
            return Ty::Error;
        }
        // A bare construction (`Gadget { … }`) of a type published by exactly
        // one imported module binds to that owner's QUALIFIED identity, so the
        // constructed value carries `owner.Gadget` rather than the bare
        // last-write-wins key. This keeps two modules' same-bare-name records
        // from colliding in the downstream MIR record-layout / field-order
        // registry (the same identity discipline `samename_type_layout` proves
        // for explicitly qualified constructions).
        let qualified_owned = self
            .published_bare_type_qualified(name)
            .or_else(|| self.flat_file_import_type_owner(name));
        let delivery_owner = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| qualified_owned.clone().unwrap_or_else(|| name.to_string()));
        if self.reject_sealed_delivery_access(
            &crate::actor_delivery::nominal(&delivery_owner, Vec::new()),
            span,
        ) {
            return Ty::Error;
        }

        if let Some(qualified) = qualified_owned.as_deref() {
            // `qualified` is the full owner-qualified source identity
            // (`owner.TypeName`), and `owner` itself may be a dotted module
            // path (`src.plain`). Splitting on the FIRST dot mistook the
            // owner's leading path segment for the lexical import binding —
            // `import_spans` keys a selective import by the MODULE's short
            // name (`plain`), not its first path segment (`src`), so that
            // mis-derived key never matched and `Plain { … }` warned
            // "unused import" even though it constructed the imported type.
            // `mark_module_owner_bindings_used` resolves the owner back to
            // the correct lexical binding via `module_import_bindings`,
            // mirroring the working annotation-position credit above.
            if let Some((owner, _)) = qualified.rsplit_once('.') {
                self.mark_module_owner_bindings_used(owner);
            }
        }
        let name = qualified_owned.as_deref().unwrap_or(name);
        if self
            .lookup_type_def(name)
            .is_some_and(|definition| definition.kind == TypeDefKind::Enum)
        {
            self.report_error(
                TypeErrorKind::TypeUsedAsValue,
                span,
                format!("enum `{name}` requires a declared variant; it cannot be constructed as a record"),
            );
            return Ty::Error;
        }
        // Fail closed on opaque handle direct construction — but ONLY for
        // cross-module constructions. The module that DECLARES an `#[opaque]`
        // type is the producer: its impl blocks contain the legitimate FFI
        // constructors (`extern "C"` stubs returning the handle) and must be
        // allowed to write `Handle { }` as the return value stub. Only OTHER
        // modules (importers / users) see it as opaque and must use the
        // declared constructor functions.
        //
        // `local_type_defs` is seeded (in `mod.rs`) with every type NAME
        // declared in the current module before body-checking begins. A bare
        // name that is present there means "this module declared it", so the
        // construction is in-module / producer-side and is ALLOWED.
        //
        // `name` at this point may be qualified (`module.Handle`) after
        // `published_bare_type_qualified` resolves a bare import reference.
        // `user_opaque_type_names` stores exact declaration identities. A
        // same-leaf type from another module must not acquire opacity.
        let unqualified = name.split_once('.').map_or(name, |(_, unqual)| unqual);
        let canonical_owner_is_current_source = name
            .rsplit_once('.')
            .is_some_and(|(owner, _)| self.checking_canonical_stdlib_source(owner));
        let is_declaring_module = self.local_type_defs.contains(unqualified)
            // A bundled stdlib package can contain several source files that
            // are registered into one source-owner frame.  Let that proven
            // source owner build its opaque wrapper stubs, but never extend
            // the exemption to an importer or to a user module with a
            // std-looking spelling.
            || canonical_owner_is_current_source;
        let is_opaque_handle = !is_declaring_module
            && (self.user_opaque_type_names.contains(name)
                || self.canonical_owned_handle_type_name(name).is_some());
        if is_opaque_handle {
            self.report_error(
                TypeErrorKind::OpaqueDirectConstruct {
                    type_name: name.to_string(),
                },
                span,
                format!(
                    "cannot construct opaque type `{name}` directly; \
                     opaque handles are produced by their stdlib constructors \
                     [E_OPAQUE_CONSTRUCT]"
                ),
            );
            return Ty::Error;
        }
        let module_local_name = if is_bare_constructor {
            self.current_module_identity().and_then(|owner| {
                let qualified = format!("{owner}.{unqualified}");
                self.type_defs.contains_key(&qualified).then_some(qualified)
            })
        } else {
            None
        };
        let td = module_local_name
            .as_deref()
            .and_then(|qualified| self.lookup_type_def(qualified))
            .or_else(|| self.lookup_type_def(name));
        if let Some(td) = td {
            // Track inferred type arguments for generic structs.
            // If the caller supplied explicit type args (e.g. `Wrapper<String> { ... }`),
            // pre-seed the map from them so field checking constrains against the
            // declared types immediately rather than synthesizing unconstrained.
            let mut type_arg_map: HashMap<String, Ty> = HashMap::new();
            if let Some(explicit_args) = type_args {
                if explicit_args.len() == td.type_params.len() {
                    for (tp, te) in td.type_params.iter().zip(explicit_args.iter()) {
                        let resolved = self.resolve_type_expr(te);
                        type_arg_map.insert(tp.clone(), resolved);
                    }
                } else {
                    // Covers both `Foo<>` (zero explicit args) and wrong-count args.
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "{} `{name}` has {} type parameter(s) but {} type argument(s) were supplied",
                            value_type_kind_label(td.kind),
                            td.type_params.len(),
                            explicit_args.len()
                        ),
                    );
                }
            }

            // Pre-seed every still-unbound type parameter with a fresh inference
            // var so a field whose declared type CONTAINS a parameter (e.g.
            // `items: Vec<T>`) constrains that parameter from its initializer,
            // exactly as a field whose type IS the bare parameter (`val: T`)
            // already does. Without this the nested parameter stays the raw
            // type-def symbol `T` in the initializer's own type and never
            // monomorphises (`E_MIR: unknown type T` at the MIR boundary). The
            // vars unify during field checking below and are resolved back to
            // concrete types before the result type is built.
            for tp in &td.type_params {
                type_arg_map
                    .entry(tp.clone())
                    .or_insert_with(|| Ty::Var(TypeVar::fresh()));
            }

            for (field_name, (expr, es)) in fields {
                if let Some(declared_ty) = td.fields.get(field_name) {
                    // Substitute already-inferred type params into the expected type.
                    // Use parallel substitution so a swap map {"A": B, "B": A} does not
                    // alias both params: each Named leaf is replaced in one structural pass.
                    let expected = declared_ty.substitute_named_params_parallel(&type_arg_map);

                    // If the expected type is still an unbound type parameter,
                    // synthesize so the field value determines the type (rather
                    // than failing with "expected T, found i64").
                    let is_unbound_param = td.type_params.iter().any(|tp| {
                        !type_arg_map.contains_key(tp)
                            && expected
                                == (Ty::Named {
                                    builtin: None,
                                    name: tp.clone(),
                                    args: vec![],
                                })
                    });
                    let actual = if is_unbound_param {
                        self.synthesize(expr, es)
                    } else {
                        self.check_against(expr, es, &expected)
                    };
                    self.record_value_transfer(expr, es);

                    // Infer type params: if field type is a bare type param, bind it
                    for tp in &td.type_params {
                        if !type_arg_map.contains_key(tp)
                            && *declared_ty
                                == (Ty::Named {
                                    builtin: None,
                                    name: tp.clone(),
                                    args: vec![],
                                })
                        {
                            type_arg_map.insert(tp.clone(), actual.clone());
                        }
                    }
                } else {
                    let similar = crate::error::find_similar(
                        field_name,
                        td.fields.keys().map(String::as_str),
                    );
                    self.report_error_with_suggestions(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!(
                            "no field `{field_name}` on {} `{name}`",
                            value_type_kind_label(td.kind)
                        ),
                        similar,
                    );
                }
            }
            // Functional-update base: `R { x: 5, ..base }`.
            // The base must evaluate to the same named record/struct type.
            // When base is present, fields not listed explicitly are filled from base,
            // so the missing-field check is skipped.
            if let Some((base_expr, base_span)) = base {
                let base_ty = self.synthesize(base_expr, base_span);
                match &base_ty {
                    Ty::Named {
                        name: base_name, ..
                    } if base_name == name => {}
                    _ => {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!(
                                "functional-update base must be of type `{name}`, found `{base_ty}`"
                            ),
                        );
                    }
                }
            } else {
                // No base: all fields must be explicitly provided.
                let provided: HashSet<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
                for declared in td.fields.keys() {
                    if !provided.contains(declared.as_str()) {
                        self.report_error(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("missing field `{declared}` in initializer of `{name}`"),
                        );
                    }
                }
            }
            // Build type args from inferred bindings
            let type_args: Vec<Ty> = td
                .type_params
                .iter()
                .map(|tp| {
                    type_arg_map.get(tp).map_or_else(
                        || Ty::Var(TypeVar::fresh()),
                        |bound| self.subst.resolve(bound),
                    )
                })
                .collect();
            // Record the resolved type arguments for downstream monomorphisation
            // (HIR registry, MIR per-instantiation RecordLayout).
            //
            // Emit unconditionally: a record-init's type args may only become
            // fully concrete *after* `check_struct_init` returns (e.g. via an
            // outer annotation `let b: Box<int> = Box { value: 1 }`), so
            // eagerly rejecting at emission time would drop entries that the
            // post-inference boundary resolve in `check_program` would have made
            // concrete.  The fail-closed contract (no `Ty::Var` crosses into HIR)
            // is enforced at the output boundary by
            // `validate_record_init_type_args_output_contract` in `admissibility.rs`.
            self.record_concrete_record_init_type_args(span, &type_args);
            // Declaration-bound enforcement on the plain struct-init path.
            // The helper short-circuits cleanly for bound-free names and
            // enforces the TypeDef-owned bound map for every generic nominal
            // whose arguments were inferred from the fields above.
            let result_name = module_local_name.as_deref().unwrap_or(name);
            self.enforce_type_def_instantiation_bounds(result_name, &type_args, span);
            Ty::Named {
                builtin: None,
                name: result_name.to_string(),
                args: type_args,
            }
        } else if let Some((enum_name, variant_fields, enum_type_params)) =
            self.lookup_struct_variant_init(name)
        {
            // Infer generic type args from field values, mirroring the plain-struct path.
            let mut type_arg_map: HashMap<String, Ty> = HashMap::new();
            // If the caller supplied explicit type args (e.g. `Keeper::Holding<int> { … }`),
            // pre-seed the map so field checking constrains against the declared types
            // rather than synthesizing unconstrained.
            if let Some(explicit_args) = type_args {
                if explicit_args.len() == enum_type_params.len() {
                    for (tp, te) in enum_type_params.iter().zip(explicit_args.iter()) {
                        let resolved = self.resolve_type_expr(te);
                        type_arg_map.insert(tp.clone(), resolved);
                    }
                } else {
                    // Covers both `Variant<>` (zero explicit args) and wrong-count args.
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "enum variant `{name}` has {} type parameter(s) but {} type argument(s) were supplied",
                            enum_type_params.len(),
                            explicit_args.len()
                        ),
                    );
                }
            }

            for (field_name, (expr, es)) in fields {
                if let Some((_, declared_ty)) = variant_fields.iter().find(|(n, _)| n == field_name)
                {
                    // Substitute already-inferred type params into the expected type
                    let expected = declared_ty.substitute_named_params_parallel(&type_arg_map);

                    // If the expected type is still an unbound type parameter, synthesize
                    // so the field value determines the concrete type.
                    let is_unbound_param = enum_type_params.iter().any(|tp| {
                        !type_arg_map.contains_key(tp)
                            && expected
                                == (Ty::Named {
                                    builtin: None,
                                    name: tp.clone(),
                                    args: vec![],
                                })
                    });
                    let actual = if is_unbound_param {
                        self.synthesize(expr, es)
                    } else {
                        self.check_against(expr, es, &expected)
                    };
                    self.record_value_transfer(expr, es);

                    // Bind bare type params from this field's declared type
                    for tp in &enum_type_params {
                        if !type_arg_map.contains_key(tp)
                            && *declared_ty
                                == (Ty::Named {
                                    builtin: None,
                                    name: tp.clone(),
                                    args: vec![],
                                })
                        {
                            type_arg_map.insert(tp.clone(), actual.clone());
                        }
                    }
                } else {
                    let similar = crate::error::find_similar(
                        field_name,
                        variant_fields.iter().map(|(n, _)| n.as_str()),
                    );
                    self.report_error_with_suggestions(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("no field `{field_name}` on variant `{name}`"),
                        similar,
                    );
                }
            }
            let provided: HashSet<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
            for (declared, _) in &variant_fields {
                if !provided.contains(declared.as_str()) {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("missing field `{declared}` in initializer of `{name}`"),
                    );
                }
            }
            // Build concrete type args from inferred bindings
            let type_args: Vec<Ty> = enum_type_params
                .iter()
                .map(|tp| {
                    type_arg_map
                        .get(tp)
                        .cloned()
                        .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
                })
                .collect();
            // Emit unconditionally; see the struct-init branch above for the
            // boundary-prune rationale and validator location.
            self.record_concrete_record_init_type_args(span, &type_args);
            // Enforce trait bounds declared on the enum's generic type
            // parameters via the canonical nominal helper. This keeps
            // struct-variant brace init on the same TypeDef-bound authority as
            // annotations, tuple variants, and plain struct/record init.
            self.enforce_type_def_instantiation_bounds(&enum_name, &type_args, span);
            Ty::Named {
                builtin: None,
                name: enum_name,
                args: type_args,
            }
        } else {
            let similar = crate::error::find_similar(
                name,
                self.type_defs
                    .keys()
                    .map(String::as_str)
                    .chain(self.type_aliases.keys().map(String::as_str))
                    .chain(self.known_types.iter().map(String::as_str)),
            );
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedType,
                span,
                format!("undefined type `{name}`"),
                similar,
            );
            Ty::Error
        }
    }

    /// Check each constructor argument of a `spawn` expression, pushing the
    /// actor field's declared type down as the expected type so that generic
    /// constructors like `HashMap::new()` and `Vec::new()` can resolve their
    /// type parameters.
    ///
    /// Without this, `spawn Cache(store: HashMap::new())` synthesises the arg
    /// with unbound type variables (`HashMap<?T, ?U>`).  The Send check then
    /// fires on those unbound vars with the misleading message:
    ///   "cannot send `HashMap<?T22, ?T23>` to actor: type is not Send"
    ///
    /// Mirrors `check_struct_init`'s field push-down.  Two exceptions fall back
    /// to `synthesize`:
    ///
    /// 1. Unknown field name — an error will be reported separately.
    /// 2. Bare-actor-name field (e.g. `let target: Printer`): the spawn arg
    ///    carries `Printer`'s own actor-handle type, not the bare `Printer`
    ///    used for construction, so checking against the bare name produces
    ///    a spurious type mismatch.
    pub(super) fn check_spawn_constructor_args(
        &mut self,
        actor_name: &str,
        args: &[(String, Spanned<Expr>)],
        type_subst: Option<&HashMap<String, Ty>>,
    ) {
        let actor_fields: Option<HashMap<String, Ty>> =
            self.lookup_type_def(actor_name).map(|td| td.fields);
        // An actor with an explicit `init(...)` names its spawn args after
        // the INIT PARAMETERS, not the state fields they assign into (the
        // two names may differ, and even when they match, the init
        // parameter's declared width is the checker-authoritative one — the
        // init body may narrow/widen before storing into the field). Look up
        // `actor_init_params` first so a param like `init(start: i32)` gets
        // `check_against(..., i32)` here. The field-type lookup is the
        // fallback for two shapes: an actor with no explicit `init` (whose
        // spawn args map directly onto bare field names), and an init-bearing
        // actor whose spawn arg name does not match any init parameter and so
        // routes straight into a same-named state field. Without this, an
        // unmatched `field_name` silently synthesizes the arg (defaulting an
        // untyped int literal to `i64`), which then mismatches the init
        // thunk's declared i32 parameter and trips the LLVM verifier at the
        // spawn call site (#2402).
        let init_params = self.actor_init_params.get(actor_name).cloned();
        for (field_name, (arg, as_)) in args {
            let declared_init_param = init_params
                .as_ref()
                .and_then(|params| params.iter().find(|p| &p.name == field_name))
                .map(|p| p.ty.clone());
            // A field init initializes has no spawn value (D447): one init
            // body cannot be a first store at one spawn site and a
            // replacement at another.
            if declared_init_param.is_none()
                && self
                    .actor_deferred_fields
                    .get(actor_name)
                    .is_some_and(|deferred| deferred.contains(field_name))
            {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    as_,
                    format!(
                        "E_ACTOR_FIELD_DEFERRED: state field `{field_name}` of actor \
                         `{actor_name}` is initialized by `init`; remove it from the spawn \
                         arguments"
                    ),
                );
                let ty_raw = self.synthesize(arg, as_);
                self.enforce_actor_boundary_send(arg, as_, as_, &ty_raw);
                continue;
            }
            // A spawn arg name that matches BOTH an init parameter and a state
            // field with a DIFFERENT declared type is unsatisfiable: the one
            // provided value cannot simultaneously be the init parameter's type
            // (which the init thunk expects) and the field's type (which the
            // constructor stores it into). Checking the arg against the init
            // parameter alone lets it pass here, but codegen then stores the
            // value into the mismatched field slot and fails closed with a raw
            // RecordInit verifier dump (#2448). Name the collision at the
            // checker level -- the parameter, the field, and the two disagreeing
            // types -- and skip the per-arg check so no confusing secondary
            // diagnostic piles on.
            let field_ty = actor_fields.as_ref().and_then(|f| f.get(field_name));
            if let (Some(param_ty), Some(field_ty)) = (declared_init_param.as_ref(), field_ty) {
                if param_ty != field_ty {
                    let param_display = param_ty.user_facing().to_string();
                    let field_display = field_ty.user_facing().to_string();
                    self.report_error(
                        TypeErrorKind::Mismatch {
                            expected: field_display.clone(),
                            actual: param_display.clone(),
                        },
                        as_,
                        format!(
                            "spawn argument `{field_name}` matches both the `init` \
                             parameter `{field_name}: {param_display}` and the state \
                             field `{field_name}: {field_display}` of actor \
                             `{actor_name}`, whose types disagree; one value cannot \
                             fill both. Rename the `init` parameter or the field so the \
                             spawn argument targets exactly one of them."
                        ),
                    );
                    // Still synthesize the arg so downstream expression typing
                    // sees a type for this span, but do not run `check_against`
                    // (it would emit a second, less-informative mismatch).
                    let ty_raw = self.synthesize(arg, as_);
                    self.enforce_actor_boundary_send(arg, as_, as_, &ty_raw);
                    continue;
                }
            }
            let declared = declared_init_param
                .as_ref()
                .or_else(|| actor_fields.as_ref().and_then(|f| f.get(field_name)));
            // Substitute the spawn site's type arguments into the declared
            // type before checking, so a generic field/init param (`value: T`)
            // is compared against the instantiated type (`i64`) rather than
            // the unbound generic `T` (#2447). Non-generic actors and
            // arity-mismatched spawns pass `None` and check against the raw
            // declared type unchanged.
            let declared_owned: Option<Ty> = match (declared, type_subst) {
                (Some(ty), Some(subst)) => Some(ty.substitute_named_params_parallel(subst)),
                _ => None,
            };
            let declared = declared_owned.as_ref().or(declared);
            let ty_raw = match declared {
                Some(declared_ty) => {
                    let is_bare_actor = if let Ty::Named {
                        name: field_type_name,
                        ..
                    } = declared_ty
                    {
                        self.type_defs
                            .get(field_type_name)
                            .is_some_and(|td| td.kind == TypeDefKind::Actor)
                    } else {
                        false
                    };
                    if is_bare_actor {
                        self.synthesize(arg, as_)
                    } else {
                        self.check_against(arg, as_, declared_ty)
                    }
                }
                None => self.synthesize(arg, as_),
            };
            self.enforce_actor_boundary_send(arg, as_, as_, &ty_raw);
        }
    }

    /// Resolve a `spawn` target expression to the registered actor identity.
    ///
    /// `Ok(Some(identity))` carries the identity key (bare for root/flat
    /// actors, dotted `{module}.{name}` for module actors); `Ok(None)` is an
    /// unsupported target shape; `Err(())` means a diagnostic was already
    /// emitted and the spawn must type to bare `Ty::Error`.
    pub(super) fn resolve_spawn_target(
        &mut self,
        target: &Spanned<Expr>,
        span: &Span,
    ) -> Result<Option<String>, ()> {
        Ok(match &target.0 {
            // Bare spawn target: resolve local-first to the registered
            // actor identity (the current module's own actor, then a
            // root/flat actor, then a named-import binding, then a unique
            // module export). A bare name exported by 2+ modules with no
            // local actor is a typed error naming the candidates — never
            // silent first-wins.
            Expr::Identifier(name) => match self.resolve_bare_spawn_target_identity(name) {
                super::types::BareActorResolution::Resolved(identity) => Some(identity),
                super::types::BareActorResolution::Ambiguous(candidate_modules) => {
                    self.report_ambiguous_actor_reference(name, &candidate_modules, span);
                    return Err(());
                }
                // Unknown actor: keep the bare name so the pre-existing
                // unknown-actor diagnostics downstream fire unchanged.
                super::types::BareActorResolution::Unknown => Some(name.clone()),
            },
            // Handle module-qualified actor: spawn module.ActorName(args)
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(module) = &object.0 {
                    if self.module_binding_in_current_file(module) {
                        // Verify the qualifier resolves to something spawnable
                        // that is a public export of `module` before stripping
                        // it to the bare name. `module_type_exports` membership
                        // alone is insufficient: that set also holds public
                        // non-spawnable types, so it is true even when
                        // `secret.Account` is a `pub type`/struct/enum (and a
                        // private actor is absent from it entirely). Resolve the
                        // qualified definition and require an actor or a
                        // supervisor, which spawn the same way; otherwise
                        // `spawn secret.Account()` would lower to bare `Account`
                        // and silently route to a same-named root/pub actor -- a
                        // capability-boundary hole. `resolve_module_type` already
                        // gates on `pub` export + the module-qualified `type_defs`
                        // entry (which is copied from the module's own decl, so it
                        // is not clobbered by a same-named root/other-module type).
                        // Fail closed before HIR/MIR rather than misroute.
                        let actor_identity = self
                            .resolve_module_type(module, field)
                            .filter(|td| {
                                matches!(td.kind, TypeDefKind::Actor | TypeDefKind::Supervisor)
                            })
                            .map(|td| td.name);
                        let Some(actor_identity) = actor_identity else {
                            let similar = self
                                .module_type_exports_for_binding(module)
                                .map(|set| {
                                    crate::error::find_similar(
                                        field,
                                        set.iter().map(String::as_str),
                                    )
                                })
                                .unwrap_or_default();
                            self.report_error_with_suggestions(
                                TypeErrorKind::UndefinedType,
                                span,
                                format!(
                                    "module `{module}` has no exported actor or supervisor \
                                     `{field}`"
                                ),
                                similar,
                            );
                            // The caller types the spawn as bare `Ty::Error`
                            // (not `Error`'s own actor-handle type) so a subsequent
                            // `await handle.method()` is suppressed (method
                            // calls on a `Ty::Error` receiver short-circuit),
                            // keeping a single clear diagnostic.
                            return Err(());
                        };
                        self.used_modules.borrow_mut().insert(ImportKey::in_file(
                            self.current_module.clone(),
                            self.current_module_idx,
                            module.clone(),
                        ));
                        // Keep the exact source identity recovered through the
                        // lexical module binding. The surface spelling may be
                        // an alias or share its leaf with another module.
                        Some(actor_identity)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        })
    }

    pub(in crate::check) fn check_spawn(
        &mut self,
        target: &Spanned<Expr>,
        type_args: &[Spanned<TypeExpr>],
        args: &[(String, Spanned<Expr>)],
        span: &Span,
    ) -> Ty {
        let Ok(actor_name) = self.resolve_spawn_target(target, span) else {
            return Ty::Error;
        };

        if let Some(name) = actor_name {
            let owner_kind = if self.supervisor_children.contains_key(&name) {
                "supervisor"
            } else {
                "actor"
            };
            let type_params = self
                .type_defs
                .get(&name)
                .map_or_else(Vec::new, |definition| definition.type_params.clone());
            let declared_arity = type_params.len();
            let mut resolved_type_args: Vec<Ty> = if type_args.is_empty() {
                type_params
                    .iter()
                    .map(|_| Ty::Var(TypeVar::fresh()))
                    .collect()
            } else {
                type_args
                    .iter()
                    .map(|argument| self.resolve_type_expr(argument))
                    .collect()
            };
            if resolved_type_args.len() != declared_arity {
                self.report_error(
                    TypeErrorKind::ActorTypeArgArityMismatch {
                        actor_name: name.clone(), expected: declared_arity, got: resolved_type_args.len(),
                    }, span,
                    format!("{owner_kind} `{name}` has {declared_arity} type parameter(s) but {} type argument(s) were supplied", resolved_type_args.len()),
                );
                return Ty::Error;
            }
            let type_subst: HashMap<_, _> = type_params
                .iter()
                .cloned()
                .zip(resolved_type_args.iter().cloned())
                .collect();
            self.check_spawn_constructor_args(&name, args, Some(&type_subst));
            if let Some(expected_args) = self.actor_spawn_args.get(&name).cloned() {
                for (argument, required) in expected_args {
                    if required && !args.iter().any(|(provided, _)| provided == &argument) {
                        self.report_error(
                            TypeErrorKind::MissingActorSpawnArgument,
                            span,
                            format!(
                                "actor `{name}` requires an initialized spawn value for `{argument}`"
                            ),
                        );
                    }
                }
            }
            resolved_type_args = resolved_type_args
                .iter()
                .map(|argument| self.subst.resolve(argument))
                .collect();
            if resolved_type_args.iter().any(Ty::has_inference_var) {
                self.report_error(
                    TypeErrorKind::MissingActorTypeArgs { actor_name: name.clone(), expected_arity: declared_arity },
                    span,
                    format!("cannot infer all type arguments of {owner_kind} `{name}` from its spawn arguments; supply explicit type arguments"),
                );
                return Ty::Error;
            }
            self.enforce_type_def_instantiation_bounds(&name, &resolved_type_args, span);

            Ty::actor_handle(name, resolved_type_args)
        } else {
            Ty::Error
        }
    }

    /// Report the typed ambiguity error for a bare actor reference that is
    /// exported by two or more modules with no local actor to win the
    /// local-first resolution. Names every candidate and suggests the
    /// qualified spawn spelling — never silent first-wins.
    pub(super) fn report_ambiguous_actor_reference(
        &mut self,
        name: &str,
        candidate_modules: &[String],
        span: &Span,
    ) {
        let candidate_identities: Vec<String> = candidate_modules
            .iter()
            .map(|module| format!("{module}.{name}"))
            .collect();
        self.mark_ambiguous_import_owners_used(&candidate_identities);
        let candidates_list = candidate_modules
            .iter()
            .map(|m| format!("`{m}.{name}`"))
            .collect::<Vec<_>>()
            .join(", ");
        let qualified_examples = candidate_modules
            .iter()
            .map(|m| format!("`spawn {m}.{name}(...)`"))
            .collect::<Vec<_>>()
            .join(" or ");
        self.report_error_with_suggestions(
            TypeErrorKind::AmbiguousActorReference {
                actor_name: name.to_string(),
                candidate_modules: candidate_modules.to_vec(),
            },
            span,
            format!(
                "actor `{name}` is ambiguous: it is exported by multiple \
                 modules ({candidates_list}) and no local actor `{name}` \
                 exists to take precedence"
            ),
            vec![format!(
                "qualify the spawn target with its module: {qualified_examples}"
            )],
        );
    }

    /// Publish a checked expression type without overwriting a more precise
    /// source type recorded during contextual checking.
    pub(super) fn publish_checked_expression(
        &mut self,
        expr: &Expr,
        span: &Span,
        result: Ty,
    ) -> Ty {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.expr_type_source_modules
            .entry(key.clone())
            .or_insert_with(|| self.current_module.clone());
        self.expr_types.entry(key).or_insert_with(|| result.clone());
        self.record_expression_effect(expr, span);
        self.check_receiver_whole_at_expr(expr, span, &result);
        result
    }

    /// Check if an expression is typically used for side effects (not for its return value).
    pub(in crate::check) fn record_type(&mut self, span: &Span, ty: &Ty) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.expr_type_source_modules
            .insert(key.clone(), self.current_module.clone());
        self.expr_types.insert(key, ty.clone());
    }

    pub(in crate::check) fn record_integer_literal_type(
        &mut self,
        expr: &Expr,
        span: &Span,
        ty: &Ty,
    ) {
        self.record_type(span, ty);
        if let Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } = expr
        {
            self.record_type(&operand.1, ty);
        }
    }

    // ── Diagnostic-only stack-allocation hints (HEW-PERF-001) ─────────────
    //
    // Phase A.0 scaffold: walk every `let` / `var` binding in a function body,
    // classify the right-hand side's allocation class, and append a `StackHint`
    // for every non-`Stack` (non-`Indeterminate`) classification. This pass is
    // intentionally noisy — it emits a hint on every observed heap allocation
    // without escape filtering. False-positive suppression lands in subsequent
    // slices (A.1: return-path; A.2: capture/container; A.3: field/send).
    //
    // Conservative bias: when the RHS form is not recognised, classify as
    // `Indeterminate` (no hint emitted). False negatives are safe; false
    // positives are user-trust defects. This rule already applies in A.0
    // because some well-formed RHS expressions lack populated `expr_types`
    // (e.g. inside generic lambda bodies still being inferred).

    /// Walk a function body and emit `StackHint` entries for every binding
    /// whose RHS resolves to a heap allocation class. Called from
    /// `check_function_as` after `warn_affine_param_escape`.
    pub(in crate::check) fn classify_stack_hints(&mut self, fd: &FnDecl) {
        // Ignore the function's parameter types and return type for hint
        // emission — the walker is binding-scoped, not signature-scoped.
        // Sub-body discipline (`sub-body-scoped-traversal` LESSONS row): a
        // nested function literal is reached via `Stmt::Let` of an
        // `Expr::Lambda`, which is classified as `ClosureEnv` here. The body
        // of that lambda is not re-walked — nested function decls run their
        // own `classify_stack_hints` pass via `check_function_as`.
        self.scan_block_for_stack_hints(&fd.body);
    }

    /// Recursive descent over a block, classifying every binding statement.
    pub(super) fn scan_block_for_stack_hints(&mut self, block: &Block) {
        for (stmt, span) in &block.stmts {
            self.scan_stmt_for_stack_hints(stmt, span);
        }
        if let Some(trailing) = &block.trailing_expr {
            self.scan_expr_for_stack_hints(&trailing.0);
        }
    }

    pub(super) fn scan_stmt_for_stack_hints(&mut self, stmt: &Stmt, stmt_span: &Span) {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                if let Some((expr, expr_span)) = value {
                    let class = self.classify_alloc(expr, expr_span);
                    let name = match &pattern.0 {
                        Pattern::Identifier(n) => n.clone(),
                        _ => String::new(),
                    };
                    self.maybe_record_stack_hint(stmt_span, &name, class);
                    // Descend into the RHS to classify nested bindings inside
                    // block expressions (`let x = { let y = ...; y }`).
                    self.scan_expr_for_stack_hints(expr);
                }
            }
            Stmt::Var { name, value, .. } => {
                if let Some((expr, expr_span)) = value {
                    let class = self.classify_alloc(expr, expr_span);
                    self.maybe_record_stack_hint(stmt_span, name, class);
                    self.scan_expr_for_stack_hints(expr);
                }
            }
            Stmt::Assign { value, .. } => {
                self.scan_expr_for_stack_hints(&value.0);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_block_for_stack_hints(then_block);
                if let Some(eb) = else_block {
                    self.scan_else_block_for_stack_hints(eb);
                }
            }
            Stmt::IfLet {
                conditions,
                body,
                else_body,
            } => {
                for expr in condition_exprs(conditions) {
                    self.scan_expr_for_stack_hints(&expr.0);
                }
                self.scan_block_for_stack_hints(body);
                if let Some(else_expr) = else_body {
                    self.scan_expr_for_stack_hints(&else_expr.0);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.scan_expr_for_stack_hints(&scrutinee.0);
                for arm in arms {
                    self.scan_match_arm_body_for_stack_hints(arm);
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_block_for_stack_hints(body);
            }
            Stmt::WhileLet {
                conditions, body, ..
            } => {
                for expr in condition_exprs(conditions) {
                    self.scan_expr_for_stack_hints(&expr.0);
                }
                self.scan_block_for_stack_hints(body);
            }
            Stmt::For { iterable, body, .. } => {
                self.scan_expr_for_stack_hints(&iterable.0);
                self.scan_block_for_stack_hints(body);
            }
            Stmt::Loop { body, .. } => {
                self.scan_block_for_stack_hints(body);
            }
            Stmt::Expression(expr) => {
                self.scan_expr_for_stack_hints(&expr.0);
            }
            Stmt::Return(opt) => {
                if let Some((e, _)) = opt {
                    self.scan_expr_for_stack_hints(e);
                }
            }
            Stmt::Break { value, .. } => {
                if let Some((e, _)) = value {
                    self.scan_expr_for_stack_hints(e);
                }
            }
            Stmt::Defer(expr) => {
                self.scan_expr_for_stack_hints(&expr.0);
            }
            // Statement forms that cannot host a binding RHS: nothing to do.
            // Listed explicitly so a future Stmt variant addition forces a
            // compile error here (`exhaustive-traversal-and-lowering` LESSONS
            // row — no silent `_ => {}` in semantic positions).
            Stmt::Continue { .. } => {}
        }
    }

    pub(super) fn scan_else_block_for_stack_hints(&mut self, eb: &hew_parser::ast::ElseBlock) {
        if let Some(b) = &eb.block {
            self.scan_block_for_stack_hints(b);
        }
        if let Some(if_stmt) = &eb.if_stmt {
            let (stmt, span) = if_stmt.as_ref();
            self.scan_stmt_for_stack_hints(stmt, span);
        }
    }

    pub(super) fn scan_match_arm_body_for_stack_hints(&mut self, arm: &MatchArm) {
        if let Some((g, _)) = &arm.guard {
            self.scan_expr_for_stack_hints(g);
        }
        self.scan_expr_for_stack_hints(&arm.body.0);
    }

    /// Descend into nested expressions to find `let`-bearing block expressions
    /// and inner lambda bodies. Phase A.0 does not classify expression-position
    /// allocations on their own (e.g. `vec.push(Vec::new())` does not emit a
    /// hint for the inner `Vec::new()` because it is unbound). Only `let` /
    /// `var` bindings produce hints in this slice.
    pub(super) fn scan_expr_for_stack_hints(&mut self, expr: &Expr) {
        match expr {
            Expr::Coalesce { left, right }
            | Expr::Handle {
                operand: left,
                body: right,
                ..
            } => {
                self.scan_expr_for_stack_hints(&left.0);
                self.scan_expr_for_stack_hints(&right.0);
            }
            Expr::Block(block) => self.scan_block_for_stack_hints(block),
            Expr::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_expr_for_stack_hints(&then_block.0);
                if let Some(eb) = else_block {
                    self.scan_expr_for_stack_hints(&eb.0);
                }
            }
            Expr::IfLet {
                conditions,
                body,
                else_body,
            } => {
                // Mirrors the `Stmt::IfLet` arm in `scan_stmt_for_stack_hints`.
                // `body` and `else_body` are bare `Block` values (not `Spanned<Expr>`),
                // so we call `scan_block_for_stack_hints` directly.
                for expr in condition_exprs(conditions) {
                    self.scan_expr_for_stack_hints(&expr.0);
                }
                self.scan_block_for_stack_hints(body);
                if let Some(else_expr) = else_body {
                    self.scan_expr_for_stack_hints(&else_expr.0);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.scan_expr_for_stack_hints(&scrutinee.0);
                for arm in arms {
                    self.scan_match_arm_body_for_stack_hints(arm);
                }
            }
            // All remaining expression forms — including `Expr::Lambda`
            // (whose body is *not* re-walked here: nested fn / lambda decls
            // run their own walker pass, and the lambda value itself when
            // assigned is classified at the binding site as `ClosureEnv`,
            // so walking the lambda body would double-emit hints) — cannot
            // host a `let` statement directly. Anything reachable through
            // call args, indices, struct fields, or tuple elements is
            // wrapped in an `Expr::Block` when it contains statements,
            // covered by the `Expr::Block` arm above.
            _ => {}
        }
    }

    /// Classify a binding's RHS expression by looking up its synthesised type
    /// in `expr_types`. Phase A.0 recognises the named heap types
    /// (`Vec`, `String`, `HashMap`, `HashSet`, `Rc`) plus closure literals
    /// (`Expr::Lambda`). Everything else maps to `Stack` (already
    /// stack-shaped) or `Indeterminate` (unknown form, no hint).
    pub(super) fn classify_alloc(&self, expr: &Expr, span: &Span) -> AllocationClass {
        // Closure literals are env-heap regardless of resolved type.
        if matches!(expr, Expr::Lambda { .. }) {
            return AllocationClass::ClosureEnv;
        }
        let key = SpanKey::in_module(span, self.current_module_idx);
        match self.expr_types.get(&key) {
            Some(ty) => Self::classify_ty(&self.subst.resolve(ty)),
            // Type not recorded — happens for some inferred or rewritten
            // expressions. Conservative silence per the bias policy.
            None => AllocationClass::Indeterminate,
        }
    }

    pub(super) fn classify_ty(ty: &Ty) -> AllocationClass {
        match ty {
            Ty::String => AllocationClass::String,
            Ty::Named { name, .. } => match name.as_str() {
                "Vec" => AllocationClass::Vec,
                "HashMap" => AllocationClass::HashMap,
                "HashSet" => AllocationClass::HashSet,
                "Rc" => AllocationClass::Rc,
                _ => AllocationClass::Stack,
            },
            // Type variables, primitives, tuples, arrays, function types:
            // either already stack-shaped or not yet resolved. A.0 is
            // conservative.
            _ => AllocationClass::Stack,
        }
    }

    pub(super) fn maybe_record_stack_hint(
        &mut self,
        stmt_span: &Span,
        binding_name: &str,
        class: AllocationClass,
    ) {
        // No hint for stack-shaped or unclassifiable RHSs.
        if matches!(
            class,
            AllocationClass::Stack | AllocationClass::Indeterminate
        ) {
            return;
        }
        self.stack_hints.push(StackHint {
            span_key: SpanKey::in_module(stmt_span, self.current_module_idx),
            binding_name: binding_name.to_string(),
            alloc_class: class,
        });
    }

    /// Type-check `lhs is rhs` (identity comparison, slice D-2).
    ///
    /// See the doc comment on the `Expr::Is` arm in [`Self::synthesize_inner`]
    /// for the allowance set, rejection rules, and cross-class behaviour.
    ///
    /// Always returns `Ty::Bool` (even after reporting errors); the operator
    /// is total at the type level so downstream uses (`if (a is b) { ... }`)
    /// don't double-poison.
    pub(super) fn synthesize_is(
        &mut self,
        lhs: &Spanned<Expr>,
        rhs: &Spanned<Expr>,
        span: &Span,
    ) -> Ty {
        let lhs_ty = self.synthesize(&lhs.0, &lhs.1);
        if let Some(rhs_ty) = self.resolve_is_type_pattern(&rhs.0) {
            return self.synthesize_is_type_pattern(lhs, &lhs_ty, rhs, &rhs_ty, span);
        }
        let rhs_ty = self.synthesize(&rhs.0, &rhs.1);
        let lhs_resolved = self.subst.resolve(&lhs_ty);
        let rhs_resolved = self.subst.resolve(&rhs_ty);

        // Don't double-report when either side is already poisoned by an
        // upstream diagnostic (`Ty::Error`). The operator still produces
        // `bool` so enclosing expressions see a stable type.
        if matches!(lhs_resolved, Ty::Error) || matches!(rhs_resolved, Ty::Error) {
            return Ty::Bool;
        }

        // An operand still under inference cannot be decided here, and it must
        // not be abandoned either: a closure's parameter types are fresh
        // variables while its body is checked and only settle when a call site
        // unifies them, so `let same = |a, b| a is b;` used to escape
        // `is_identity_capable` entirely and die in the codegen front on the
        // span-less `IdentityCompare lhs must be a pointer or integer value`.
        // Record the obligation and re-run the same decision once inference
        // has settled (`report_unresolved_inference_holes`) — #3134.
        if matches!(lhs_resolved, Ty::Var(_)) || matches!(rhs_resolved, Ty::Var(_)) {
            let key = SpanKey::in_module(span, self.current_module_idx);
            let check = DeferredIsCheck {
                span: span.clone(),
                lhs_span: lhs.1.clone(),
                lhs_ty,
                rhs_span: rhs.1.clone(),
                rhs_ty,
                source_module: self.current_diagnostic_source_module(),
            };
            self.deferred_is_checks.insert(key, check);
            return Ty::Bool;
        }

        for (kind, span, message) in
            self.is_value_form_diagnostics(&lhs.1, &lhs_resolved, &rhs.1, &rhs_resolved, span)
        {
            self.report_error(kind, &span, message);
        }

        Ty::Bool
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
        let Expr::Identifier(name) = rhs else {
            return None;
        };
        Ty::from_name(name).or_else(|| {
            self.lookup_type_def(name)
                .map(|type_def| Ty::normalize_named(type_def.name, vec![]))
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
            if !matches!(lhs.0, Expr::Identifier(_)) {
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
            Ty::Named { name, .. } => {
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
                if let Some(td) = self.type_defs.get(name) {
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
        matches!(ty, Ty::Named { builtin: Some(builtin), .. } if builtin.is_substrate_handle())
    }
}
