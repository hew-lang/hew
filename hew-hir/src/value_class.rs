use std::collections::HashMap;

use hew_parser::ast::ResourceMarker as AstResourceMarker;
use hew_types::{BuiltinType, ResolvedTy};

/// HIR-owned type classification marker.
///
/// Parser-level markers only represent user-written ownership attributes
/// (`#[resource]` / `#[linear]`). HIR also needs substrate registrations for
/// compiler-known value types that are not user-authored attributes, such as
/// `BitCopy` crash-hook payload records.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ResourceMarker {
    #[default]
    None,
    BitCopy,
    Resource,
    Linear,
}

impl From<hew_parser::ast::ResourceMarker> for ResourceMarker {
    fn from(marker: hew_parser::ast::ResourceMarker) -> Self {
        match marker {
            AstResourceMarker::None => Self::None,
            AstResourceMarker::Resource => Self::Resource,
            AstResourceMarker::Linear => Self::Linear,
        }
    }
}

/// Per-named-type classification table consumed by `ValueClass::of_ty`.
///
/// Construction-site authority: the table is populated by HIR lowering
/// from every `Item::TypeDecl`'s `#[resource]` / `#[linear]` marker and
/// compiler-known substrate registrations. Parser-level storage is retained
/// for compatibility with existing HIR/MIR construction sites; callers must use
/// `lookup_type_marker` so `BitCopy` registrations that have no parser spelling
/// are still observed. LESSONS: `type-info-survival`.
pub type TypeClassTable = HashMap<String, (ResourceMarker, Option<String>)>;

#[must_use]
pub fn lookup_type_marker(name: &str, type_classes: &TypeClassTable) -> Option<ResourceMarker> {
    crate::builtin_type_classes::builtin_type_registration(name)
        .map(|registration| registration.marker)
        .or_else(|| {
            let exact = type_classes.get(name).map(|(marker, _)| *marker);
            let short = hew_types::short_name(name);
            match (exact, short != name) {
                (Some(ResourceMarker::None), true) => {
                    type_classes.get(short).map(|(marker, _)| *marker).or(exact)
                }
                (Some(marker), _) => Some(marker),
                (None, true) => type_classes.get(short).map(|(marker, _)| *marker),
                (None, false) => None,
            }
        })
}

#[must_use]
pub fn lookup_type_marker_for_ty(
    ty: &ResolvedTy,
    type_classes: &TypeClassTable,
) -> Option<ResourceMarker> {
    let ResolvedTy::Named {
        name,
        args,
        builtin,
        ..
    } = ty
    else {
        return None;
    };

    if builtin.is_some() {
        return lookup_type_marker(name, type_classes);
    }

    if !args.is_empty() {
        // `type_classes` is keyed on the per-instantiation `mangled_name`, which
        // the registry side (`RecordLayoutRegistry::insert`, `layout_mono`,
        // `finalize_user_record_value_classes`) builds from the BARE-normalised
        // type-arg spine. So the probe must shorten its args too: a generic with
        // a module-qualified payload (`Holder<lmonobox.Box>`) is registered as
        // `Holder$$Box`, and a raw `mangle(name, args)` keying `Holder$$lmonobox.Box`
        // would miss and silently fall through to the coarse outer-name lookup
        // below, losing the per-instantiation marker. Nested qualified payloads
        // are shortened at depth by `shorten_named_arg_qualifiers`.
        let short_args: Vec<ResolvedTy> = args
            .iter()
            .cloned()
            .map(crate::monomorph::shorten_named_arg_qualifiers)
            .collect();
        let concrete_key = crate::monomorph::mangle(name, &short_args);
        if let Some((marker, _)) = type_classes.get(&concrete_key) {
            return Some(*marker);
        }
        // A generic value record defined in an IMPORTED module carries a
        // module-qualified USE-site name (`k.Key<string>`), but its layout is
        // registered under the bare-origin mangling the DECLARATION produced
        // (`Key$$string`) — `RecordLayoutRegistry::insert` shortens the type
        // args but keeps the origin name verbatim, and the declaration's own
        // name is unqualified. So the qualified `concrete_key`
        // (`k.Key$$string`) misses while the bare per-instantiation marker is
        // present. Probe the short-origin mangling too so an imported-origin
        // generic instance resolves to the same per-instantiation value class
        // as its single-file / non-generic siblings (#2744). Without this the
        // probe falls through to the coarse outer-name lookup below, which
        // sees the generic origin's `ResourceMarker::None` and misclassifies
        // the mono instance as `Unknown` → the MIR value-class gate rejects a
        // valid BitCopy record.
        let short_origin = hew_types::short_name(name);
        if short_origin != name.as_str() {
            let short_key = crate::monomorph::mangle(short_origin, &short_args);
            if let Some((marker, _)) = type_classes.get(&short_key) {
                return Some(*marker);
            }
        }
    }

    let exact = type_classes.get(name).map(|(marker, _)| *marker);
    let short = hew_types::short_name(name);
    match (exact, short != name.as_str()) {
        (Some(ResourceMarker::None), true) => {
            type_classes.get(short).map(|(marker, _)| *marker).or(exact)
        }
        (Some(marker), _) => Some(marker),
        (None, true) => type_classes.get(short).map(|(marker, _)| *marker),
        (None, false) => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueClass {
    BitCopy,
    CowValue,
    PersistentShare,
    /// `@resource` types — external-resource values with an implicit drop side
    /// effect (`close(consuming self)`). Drop elaboration emits an explicit
    /// `ElabMir::Drop { drop_fn: Some(close) }` on every reachable exit.
    AffineResource,
    /// `@linear` types — single-owner values with **no implicit drop**.
    /// The move-checker rejects any function where a `Linear` binding is
    /// live at an exit without being consumed via a declared consuming
    /// method (`MirCheck::MustConsume`).
    Linear,
    View,
    Unknown,
}

impl ValueClass {
    /// Resolve a type's value-class.
    ///
    /// For `ResolvedTy::Named { name, .. }`, looks up the marker in the
    /// supplied `TypeClassTable`:
    /// - `Some((BitCopy, _))` → `Self::BitCopy`
    /// - `Some((Resource, _))` → `Self::AffineResource`
    /// - `Some((Linear, _))` → `Self::Linear`
    /// - `Some((None, _))` or absent → `Self::Unknown` (preserved fallback;
    ///   the unmarked Named-type behaviour the slice still routes through
    ///   `Strategy::UnknownBlocked` at MIR boundary).
    ///
    /// Builtin types are independent of the table.
    #[must_use]
    pub fn of_ty(ty: &ResolvedTy, type_classes: &TypeClassTable) -> Self {
        match ty {
            ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Duration
            // `instant` is a monotonic i64-nanos timestamp. The field-type
            // producer (`lower_type`) emits it as `Named { builtin: Instant }`
            // (only `duration` short-circuits to `ResolvedTy::Duration`), so it
            // reaches this classifier as a Named type and would otherwise fall
            // to `Unknown` (record-field reject, W3.029). It is a plain 8-byte
            // copyable scalar — classify it BitCopy like `duration` / `i64`.
            | ResolvedTy::Named {
                builtin: Some(BuiltinType::Instant | BuiltinType::SupervisorPool),
                ..
            }
            | ResolvedTy::Unit
            | ResolvedTy::Never => Self::BitCopy,
            ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::Array(_, _)
            | ResolvedTy::Tuple(_) => Self::CowValue,
            // A `Generator<Y, R>` / `AsyncGenerator<Y>` value is an owned, affine
            // runtime handle (`*mut HewGenCtx`), same as CancellationToken: it
            // has exactly one owner, must be released exactly once on scope exit
            // (via `hew_gen_free`), and is never bit-copied. Classifying it as
            // `AffineResource` makes the construction binding enter `owned_locals`
            // and get a scope-exit drop.
            ResolvedTy::CancellationToken
            | ResolvedTy::Named {
                builtin: Some(
                    BuiltinType::Generator
                        | BuiltinType::AsyncGenerator
                        | BuiltinType::Rc
                        | BuiltinType::Weak,
                ),
                ..
            } => Self::AffineResource,
            // An extern-returned `&T` is a non-owning foreign boundary view:
            // reuse `View` so it shares the no-retain/no-drop elaboration arm.
            ResolvedTy::Slice(_) | ResolvedTy::Pointer { .. } | ResolvedTy::Borrow { .. } => {
                Self::View
            }
            ResolvedTy::Function { .. }
            | ResolvedTy::Closure { .. }
            | ResolvedTy::TraitObject { .. } => Self::PersistentShare,
            ResolvedTy::Named { builtin, .. } => {
                match lookup_type_marker_for_ty(ty, type_classes) {
                    Some(ResourceMarker::BitCopy) => Self::BitCopy,
                    Some(ResourceMarker::Resource) => Self::AffineResource,
                    Some(ResourceMarker::Linear) => Self::Linear,
                    Some(ResourceMarker::None) | None => {
                        if matches!(
                            builtin,
                            Some(BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet)
                        ) {
                            Self::CowValue
                        } else {
                            Self::Unknown
                        }
                    }
                }
            }
            // Task handles are consume-once: MirCheck::MustConsume fires if a
            // ForkTaskHandle binding is live at an exit without being consumed
            // via AwaitTask or the implicit block-end join. Linear is the
            // correct class — it threads through C2's existing UseAfterConsume /
            // MustConsume machinery without new checks. The inner type T's own
            // class is checked independently when the task is awaited and T is
            // produced.
            ResolvedTy::Task(_) => Self::Linear,
            // An abstract parameter's value-class depends on the type that
            // monomorphisation substitutes in. Until then it is genuinely
            // unknown, so it routes through the conservative `Unknown` arm
            // (the same fail-closed boundary as an unmarked Named). This only
            // arises in gated polymorphic bodies, which never reach codegen.
            ResolvedTy::TypeParam { .. } => Self::Unknown,
        }
    }
}

#[must_use]
pub fn contains_named_type(ty: &ResolvedTy) -> bool {
    !named_type_names(ty).is_empty()
}

#[must_use]
pub fn named_type_names(ty: &ResolvedTy) -> Vec<String> {
    named_type_components(ty)
        .into_iter()
        .map(|component| component.name)
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedTypeComponent {
    pub name: String,
    pub builtin: Option<BuiltinType>,
    pub has_args: bool,
}

#[must_use]
pub fn named_type_components(ty: &ResolvedTy) -> Vec<NamedTypeComponent> {
    let mut components = Vec::new();
    collect_named_type_components(ty, &mut components);
    components
}

fn collect_named_type_components(ty: &ResolvedTy, components: &mut Vec<NamedTypeComponent>) {
    match ty {
        ResolvedTy::Tuple(elems) => {
            for elem in elems {
                collect_named_type_components(elem, components);
            }
        }
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            collect_named_type_components(elem, components);
        }
        ResolvedTy::Named {
            name,
            args,
            builtin,
            ..
        } => {
            components.push(NamedTypeComponent {
                name: name.clone(),
                builtin: *builtin,
                has_args: !args.is_empty(),
            });
            for arg in args {
                collect_named_type_components(arg, components);
            }
        }
        ResolvedTy::Function { params, ret } => {
            for param in params {
                collect_named_type_components(param, components);
            }
            collect_named_type_components(ret, components);
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            for param in params {
                collect_named_type_components(param, components);
            }
            collect_named_type_components(ret, components);
            for capture in captures {
                collect_named_type_components(capture, components);
            }
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            collect_named_type_components(pointee, components);
        }
        ResolvedTy::TraitObject { traits } => {
            for bound in traits {
                for arg in &bound.args {
                    collect_named_type_components(arg, components);
                }
                for (_, ty) in &bound.assoc_bindings {
                    collect_named_type_components(ty, components);
                }
            }
        }
        // Task<T> is compiler-internal; recurse into T so that a
        // `Task<SomeResource>` binding is still diagnosed correctly if T
        // is a named type with a resource/linear marker.
        ResolvedTy::Task(inner) => collect_named_type_components(inner, components),
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::F32
        | ResolvedTy::F64
        | ResolvedTy::Bool
        | ResolvedTy::Char
        | ResolvedTy::String
        | ResolvedTy::Bytes
        | ResolvedTy::CancellationToken
        | ResolvedTy::Duration
        | ResolvedTy::Unit
        | ResolvedTy::Never
        // A structural type parameter is abstract, not a named user type, so
        // it contributes no named-type component.
        | ResolvedTy::TypeParam { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::{named_type_components, named_type_names};
    use hew_types::BuiltinType;
    use hew_types::{ResolvedTraitBound, ResolvedTy};

    #[test]
    fn trait_object_names_are_not_reported_as_unknown_named_types() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Display".to_string(),
                args: Vec::new(),
                assoc_bindings: Vec::new(),
            }],
        };

        assert!(named_type_names(&ty).is_empty());
    }

    #[test]
    fn trait_object_type_arguments_still_report_nested_named_types() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Iterator".to_string(),
                args: vec![ResolvedTy::named_user("Foo", Vec::new())],
                assoc_bindings: Vec::new(),
            }],
        };

        assert_eq!(named_type_names(&ty), vec!["Foo".to_string()]);
    }

    #[test]
    fn trait_object_nested_arguments_recurse_without_reporting_trait_names() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "OuterTrait".to_string(),
                args: vec![ResolvedTy::Tuple(vec![
                    ResolvedTy::named_user("Foo", Vec::new()),
                    ResolvedTy::TraitObject {
                        traits: vec![ResolvedTraitBound {
                            trait_name: "InnerTrait".to_string(),
                            args: vec![ResolvedTy::named_user("Bar", Vec::new())],
                            assoc_bindings: Vec::new(),
                        }],
                    },
                ])],
                assoc_bindings: Vec::new(),
            }],
        };

        assert_eq!(
            named_type_names(&ty),
            vec!["Foo".to_string(), "Bar".to_string()]
        );
    }

    #[test]
    fn named_type_components_preserve_builtin_discriminator_and_arg_shape() {
        let ty = ResolvedTy::named_builtin(
            "Vec",
            BuiltinType::Vec,
            vec![ResolvedTy::named_user("Foo", Vec::new())],
        );

        let components = named_type_components(&ty);
        assert_eq!(components.len(), 2);
        assert_eq!(components[0].name, "Vec");
        assert_eq!(components[0].builtin, Some(BuiltinType::Vec));
        assert!(components[0].has_args);
        assert_eq!(components[1].name, "Foo");
        assert_eq!(components[1].builtin, None);
        assert!(!components[1].has_args);
    }

    // ── C1 qualified-payload concrete-key symmetry ──────────────────────────
    //
    // `lookup_type_marker_for_ty` probes the per-instantiation marker via a
    // mangled concrete key. `type_classes` is registered under the BARE-spine
    // mangled name (`RecordLayoutRegistry::insert` / `layout_mono` /
    // `finalize_user_record_value_classes` all shorten the spine). So a generic
    // with a module-qualified payload (`Holder<lmonobox.Box>`) must shorten its
    // probe args or the concrete key (`Holder$$lmonobox.Box`) diverges from the
    // registered `Holder$$Box`, silently missing the per-instantiation marker.

    #[test]
    fn qualified_payload_resolves_per_instantiation_marker() {
        use super::{lookup_type_marker_for_ty, ResourceMarker, TypeClassTable};

        let mut table: TypeClassTable = std::collections::HashMap::default();
        // Registered under the BARE spine, exactly as the registry keys it.
        let bare_key = crate::monomorph::mangle("Holder", &[ResolvedTy::named_user("Box", vec![])]);
        table.insert(bare_key, (ResourceMarker::BitCopy, None));

        // Probe with the QUALIFIED payload (`lmonobox.Box`), the import-use form.
        // A raw `mangle("Holder", [lmonobox.Box])` keys `Holder$$lmonobox.Box`
        // and would miss the registered `Holder$$Box`.
        let qualified = ResolvedTy::named_user(
            "Holder",
            vec![ResolvedTy::named_user("lmonobox.Box", vec![])],
        );
        assert_eq!(
            lookup_type_marker_for_ty(&qualified, &table),
            Some(ResourceMarker::BitCopy),
            "the qualified-payload probe must shorten its spine to hit the \
             bare-key per-instantiation marker"
        );
    }

    // ── #2744 qualified-ORIGIN concrete-key symmetry ────────────────────────
    //
    // A generic value record defined in an IMPORTED module is used through its
    // module-qualified OUTER name (`keyed.Key<string>`), but its layout — and
    // therefore its per-instantiation value-class marker — is registered under
    // the bare-origin mangling the declaration produced (`Key$$string`). The
    // probe must shorten the qualified ORIGIN (not just the payload spine) or
    // the concrete key (`keyed.Key$$string`) diverges from the registered
    // `Key$$string`, the outer-name fallback sees the generic origin's
    // `ResourceMarker::None`, and the mono instance is misclassified Unknown.

    #[test]
    fn qualified_origin_resolves_per_instantiation_marker() {
        use super::{lookup_type_marker_for_ty, ResourceMarker, TypeClassTable};

        let mut table: TypeClassTable = std::collections::HashMap::default();
        // The mono instance is registered under the BARE origin, exactly as the
        // declaration-side registry / finalize keys it.
        let bare_key = crate::monomorph::mangle("Key", &[ResolvedTy::String]);
        table.insert(bare_key, (ResourceMarker::BitCopy, None));
        // The generic origin itself carries `ResourceMarker::None` under both
        // its qualified and bare spellings (finalize seeds every registry key),
        // so the coarse outer-name fallback must NOT be what answers the probe.
        table.insert("keyed.Key".to_string(), (ResourceMarker::None, None));
        table.insert("Key".to_string(), (ResourceMarker::None, None));

        // Probe with the QUALIFIED origin (`keyed.Key<string>`), the import-use
        // form. A raw `mangle("keyed.Key", [string])` keys `keyed.Key$$string`
        // and misses the registered bare `Key$$string`.
        let qualified = ResolvedTy::named_user("keyed.Key", vec![ResolvedTy::String]);
        assert_eq!(
            lookup_type_marker_for_ty(&qualified, &table),
            Some(ResourceMarker::BitCopy),
            "the qualified-origin probe must shorten the outer name to hit the \
             bare-key per-instantiation marker, not fall through to the \
             generic origin's None marker"
        );
    }

    #[test]
    fn distinct_payload_does_not_resolve_marker_via_concrete_key() {
        use super::{lookup_type_marker_for_ty, ResourceMarker, TypeClassTable};

        let mut table: TypeClassTable = std::collections::HashMap::default();
        // Only `Holder$$i64` is registered with a BitCopy marker.
        let bare_key = crate::monomorph::mangle("Holder", &[ResolvedTy::I64]);
        table.insert(bare_key, (ResourceMarker::BitCopy, None));

        // Probe `Holder<lmonobox.Box>` (→ concrete key `Holder$$Box`): the
        // concrete-key path must NOT match the `Holder$$i64` entry. With no
        // outer-name `Holder` entry either, the lookup yields None. Pins that
        // shortening collapses qualifiers, not distinct payloads.
        let qualified = ResolvedTy::named_user(
            "Holder",
            vec![ResolvedTy::named_user("lmonobox.Box", vec![])],
        );
        assert_eq!(lookup_type_marker_for_ty(&qualified, &table), None);
        // Sanity: the unused variant keeps the linter honest.
        let _ = ResourceMarker::Resource;
    }
}
