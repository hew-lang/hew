use std::collections::{BTreeMap, HashMap};
use std::ops::{Deref, DerefMut};

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
/// Checker-admitted lifecycle for one exact qualified opaque resource.
///
/// This is deliberately distinct from the user-facing type-class entry.  The
/// class says that values are affine; this fact says *why* that class was
/// admitted and pins its sole automatic close to the exact producer/release
/// contract the checker validated.  Downstream stages consume this fact and
/// never reconstruct it from a short type name or a method spelling.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OpaqueResourceLifecycle {
    pub resource_declaration: hew_types::DefId,
    pub close_declaration: hew_types::DefId,
    pub release_declaration: hew_types::DefId,
    /// Emitted linkage names. These are never semantic lookup keys.
    pub close_symbol: String,
    pub release_symbol: String,
    pub discharge_depth: hew_types::ffi_contracts::ReleaseDischargeDepth,
    pub producer_declarations: std::collections::BTreeSet<hew_types::DefId>,
    pub producer_symbols: std::collections::BTreeSet<String>,
    pub producer_modules: std::collections::BTreeSet<String>,
}

/// MIR-admitted lifecycle for one exact field-bearing resource record.
///
/// Record resources share the same declaration-identity authority as opaque
/// resources.  The emitted close symbol is linkage metadata only; semantic
/// lookup is always keyed by the exact qualified resource declaration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResourceRecordLifecycle {
    pub resource_declaration: hew_types::DefId,
    pub close_declaration: hew_types::DefId,
    pub close_symbol: String,
}

/// Canonical declaration-identity registry for resource lifecycles.
///
/// This is the only carrier permitted beyond HIR lowering.  It intentionally
/// exposes exact [`hew_types::DefId`] lookup only: callers may not retry a
/// qualified miss with a short, suffix, or leaf name.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LifecycleRegistry {
    opaque_resources: BTreeMap<hew_types::DefId, OpaqueResourceLifecycle>,
    resource_records: BTreeMap<hew_types::DefId, ResourceRecordLifecycle>,
}

impl LifecycleRegistry {
    /// Admit one exact opaque-resource lifecycle into a standalone registry.
    ///
    /// # Errors
    /// Returns the rejected lifecycle on duplicate declaration identity.
    pub fn admit_opaque_resource(
        &mut self,
        lifecycle: OpaqueResourceLifecycle,
    ) -> Result<(), Box<OpaqueResourceLifecycle>> {
        use std::collections::btree_map::Entry;
        match self
            .opaque_resources
            .entry(lifecycle.resource_declaration.clone())
        {
            Entry::Vacant(entry) => {
                entry.insert(lifecycle);
                Ok(())
            }
            Entry::Occupied(_) => Err(Box::new(lifecycle)),
        }
    }
    #[must_use]
    pub fn opaque_resource(
        &self,
        resource_declaration: &hew_types::DefId,
    ) -> Option<&OpaqueResourceLifecycle> {
        self.opaque_resources.get(resource_declaration)
    }

    #[must_use]
    pub fn opaque_resource_for_ty(&self, ty: &ResolvedTy) -> Option<&OpaqueResourceLifecycle> {
        let ResolvedTy::Named {
            name,
            builtin: None,
            ..
        } = ty
        else {
            return None;
        };
        self.opaque_resource(&hew_types::DefId::new(name))
    }

    #[must_use]
    pub fn opaque_resources(&self) -> impl ExactSizeIterator<Item = &OpaqueResourceLifecycle> {
        self.opaque_resources.values()
    }

    /// Admit one exact field-bearing resource-record lifecycle.
    ///
    /// # Errors
    /// Returns the rejected lifecycle on duplicate declaration identity.
    fn admit_resource_record(
        &mut self,
        lifecycle: ResourceRecordLifecycle,
    ) -> Result<(), Box<ResourceRecordLifecycle>> {
        use std::collections::btree_map::Entry;
        match self
            .resource_records
            .entry(lifecycle.resource_declaration.clone())
        {
            Entry::Vacant(entry) => {
                entry.insert(lifecycle);
                Ok(())
            }
            Entry::Occupied(_) => Err(Box::new(lifecycle)),
        }
    }

    #[must_use]
    pub fn resource_record(
        &self,
        resource_declaration: &hew_types::DefId,
    ) -> Option<&ResourceRecordLifecycle> {
        self.resource_records.get(resource_declaration)
    }

    #[must_use]
    pub fn resource_records(&self) -> impl ExactSizeIterator<Item = &ResourceRecordLifecycle> {
        self.resource_records.values()
    }
}

/// Per-named-type classification plus exact closeable-opaque lifecycles.
///
/// `Deref` preserves the long-established map API for ordinary class reads;
/// lifecycle consumers must use [`Self::opaque_resource_lifecycle`] so an
/// exact qualified key is mandatory and no short-name retry is available.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeClassTable {
    classes: HashMap<String, (ResourceMarker, Option<String>)>,
    lifecycle_registry: LifecycleRegistry,
}

impl Deref for TypeClassTable {
    type Target = HashMap<String, (ResourceMarker, Option<String>)>;

    fn deref(&self) -> &Self::Target {
        &self.classes
    }
}

impl DerefMut for TypeClassTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.classes
    }
}

impl TypeClassTable {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Admit one lifecycle. Duplicate exact identities are refused even when
    /// equal: HIR admission is a one-shot boundary, not a merge operation.
    ///
    /// # Errors
    ///
    /// Returns the rejected lifecycle when its exact qualified identity was
    /// already admitted.
    pub fn admit_opaque_resource_lifecycle(
        &mut self,
        lifecycle: OpaqueResourceLifecycle,
    ) -> Result<(), Box<OpaqueResourceLifecycle>> {
        self.lifecycle_registry.admit_opaque_resource(lifecycle)
    }

    /// Admit one exact field-bearing resource-record lifecycle at the HIR
    /// boundary. Downstream phases receive only an immutable registry view.
    ///
    /// # Errors
    /// Returns the rejected lifecycle when the resource declaration was
    /// already admitted.
    pub fn admit_resource_record_lifecycle(
        &mut self,
        lifecycle: ResourceRecordLifecycle,
    ) -> Result<(), Box<ResourceRecordLifecycle>> {
        self.lifecycle_registry.admit_resource_record(lifecycle)
    }

    #[must_use]
    pub fn opaque_resource_lifecycle(
        &self,
        resource_declaration: &hew_types::DefId,
    ) -> Option<&OpaqueResourceLifecycle> {
        self.lifecycle_registry
            .opaque_resource(resource_declaration)
    }

    #[must_use]
    pub fn opaque_resource_lifecycle_for_type_name(
        &self,
        canonical_type_name: &str,
    ) -> Option<&OpaqueResourceLifecycle> {
        self.opaque_resource_lifecycle(&hew_types::DefId::new(canonical_type_name))
    }

    #[must_use]
    pub fn opaque_resource_lifecycles(
        &self,
    ) -> impl ExactSizeIterator<Item = &OpaqueResourceLifecycle> {
        self.lifecycle_registry.opaque_resources()
    }

    /// Return the structured lifecycle authority carried into MIR/codegen.
    #[must_use]
    pub const fn lifecycle_registry(&self) -> &LifecycleRegistry {
        &self.lifecycle_registry
    }
}

#[must_use]
pub fn lookup_type_marker(name: &str, type_classes: &TypeClassTable) -> Option<ResourceMarker> {
    crate::builtin_type_classes::builtin_type_registration(name)
        .map(|registration| registration.marker)
        .or_else(|| type_classes.get(name).map(|(marker, _)| *marker))
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

    if let Some(builtin) = builtin {
        // Builtin ownership is an identity fact, not a spelling convention.
        // In particular, a user record named `Sender` or `Receiver` must not
        // inherit the channel endpoint's resource/drop class merely because
        // the compiler also has a builtin with that short name.  Use the
        // carried discriminator and its canonical registration so qualified
        // spellings such as `channel.Sender<T>` retain the endpoint class.
        if let Some(registration) =
            crate::builtin_type_classes::builtin_type_registration(builtin.canonical_name())
        {
            return Some(registration.marker);
        }
        // Source-layout-backed lifecycle values retain an exact builtin
        // discriminator even though their record/enum shape comes from the
        // imported `.hew` declaration rather than `BUILTIN_TYPE_REGISTRATIONS`.
        // A non-`None` marker on that discriminator is already the checker-
        // admitted value-semantic authority: consume it directly instead of
        // trying to rediscover the class through a leaf-name table row. This
        // keeps `DownNotification` and its nested `DownTarget`/`DownReason`
        // values total at every HIR/MIR decision site while a user declaration
        // with the same spelling (which carries `builtin: None`) remains
        // completely distinct. Builtins whose marker is `None` still fall
        // through to the source-derived table so aggregate payload ownership
        // such as `CrashInfo { message: string }` is classified structurally.
        match builtin.marker() {
            hew_types::builtin_type::BuiltinTypeMarker::BitCopy => {
                return Some(ResourceMarker::BitCopy);
            }
            hew_types::builtin_type::BuiltinTypeMarker::Resource => {
                return Some(ResourceMarker::Resource);
            }
            hew_types::builtin_type::BuiltinTypeMarker::Linear => {
                return Some(ResourceMarker::Linear);
            }
            hew_types::builtin_type::BuiltinTypeMarker::None => {}
        }
    }

    if !args.is_empty() {
        // Generic keys retain their canonical nominal paths throughout the
        // spine; no leaf-name normalisation may select a value class.
        let canonical_args: Vec<ResolvedTy> = args
            .iter()
            .cloned()
            .map(crate::monomorph::shorten_named_arg_qualifiers)
            .collect();
        let concrete_key =
            crate::monomorph::mangle(&crate::mangle_dotted_name(name), &canonical_args);
        if let Some((marker, _)) = type_classes.get(&concrete_key) {
            return Some(*marker);
        }
    }

    type_classes.get(name).map(|(marker, _)| *marker)
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

    #[test]
    fn source_layout_lifecycle_discriminators_are_total_without_leaf_rows() {
        use super::{lookup_type_marker_for_ty, ResourceMarker, TypeClassTable, ValueClass};

        let table = TypeClassTable::default();
        for builtin in [
            BuiltinType::MonitorId,
            BuiltinType::DownTarget,
            BuiltinType::DownReason,
            BuiltinType::DownNotification,
        ] {
            let ty = ResolvedTy::named_builtin(
                format!("std.link_monitor.{}", builtin.canonical_name()),
                builtin,
                Vec::new(),
            );
            assert_eq!(
                lookup_type_marker_for_ty(&ty, &table),
                Some(ResourceMarker::BitCopy),
                "the exact {builtin:?} discriminator must carry its compiler-admitted marker"
            );
            assert_eq!(
                ValueClass::of_ty(&ty, &table),
                ValueClass::BitCopy,
                "the exact {builtin:?} discriminator must produce a concrete decision"
            );
        }
    }

    #[test]
    fn source_layout_lifecycle_marker_does_not_cross_user_nominal_identity() {
        use super::{lookup_type_marker_for_ty, TypeClassTable, ValueClass};

        let table = TypeClassTable::default();
        for name in ["DownNotification", "user.DownNotification"] {
            let ty = ResolvedTy::named_user(name, Vec::new());
            assert_eq!(lookup_type_marker_for_ty(&ty, &table), None);
            assert_eq!(ValueClass::of_ty(&ty, &table), ValueClass::Unknown);
        }
    }

    // ── Canonical qualified-payload concrete-key identity ───────────────────

    #[test]
    fn qualified_payload_does_not_select_bare_instantiation_marker() {
        use super::{lookup_type_marker_for_ty, ResourceMarker, TypeClassTable};

        let mut table = TypeClassTable::default();
        // Registered under a distinct bare declaration identity.
        let bare_key = crate::monomorph::mangle("Holder", &[ResolvedTy::named_user("Box", vec![])]);
        table.insert(bare_key, (ResourceMarker::BitCopy, None));

        // Probe with a QUALIFIED payload. It must not select the bare key.
        let qualified = ResolvedTy::named_user(
            "Holder",
            vec![ResolvedTy::named_user("lmonobox.Box", vec![])],
        );
        assert_eq!(
            lookup_type_marker_for_ty(&qualified, &table),
            None,
            "the qualified-payload probe must not collapse onto a bare declaration"
        );
    }

    // ── Canonical qualified-origin concrete-key identity ────────────────────

    #[test]
    fn qualified_origin_does_not_select_bare_instantiation_marker() {
        use super::{lookup_type_marker_for_ty, ResourceMarker, TypeClassTable};

        let mut table = TypeClassTable::default();
        // The mono instance is registered under a distinct bare origin.
        let bare_key = crate::monomorph::mangle("Key", &[ResolvedTy::String]);
        table.insert(bare_key, (ResourceMarker::BitCopy, None));
        // The generic origin itself carries `ResourceMarker::None` under both
        // spellings, allowing the assertion to prove the concrete bare marker
        // was not selected.
        table.insert("keyed.Key".to_string(), (ResourceMarker::None, None));
        table.insert("Key".to_string(), (ResourceMarker::None, None));

        // A QUALIFIED origin must remain distinct from bare `Key<string>`.
        let qualified = ResolvedTy::named_user("keyed.Key", vec![ResolvedTy::String]);
        assert_eq!(
            lookup_type_marker_for_ty(&qualified, &table),
            Some(ResourceMarker::None),
            "the qualified-origin probe must not collapse onto the bare instance"
        );
    }

    #[test]
    fn distinct_payload_does_not_resolve_marker_via_concrete_key() {
        use super::{lookup_type_marker_for_ty, ResourceMarker, TypeClassTable};

        let mut table = TypeClassTable::default();
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

    #[test]
    fn resource_record_registry_is_exact_and_refuses_duplicate_identity() {
        use super::{ResourceRecordLifecycle, TypeClassTable};
        use hew_types::DefId;

        let lifecycle = |owner: &str, close: &str, symbol: &str| ResourceRecordLifecycle {
            resource_declaration: DefId::new(owner),
            close_declaration: DefId::new(close),
            close_symbol: symbol.to_string(),
        };
        let mut table = TypeClassTable::default();
        table
            .admit_resource_record_lifecycle(lifecycle(
                "left.Connection",
                "left.Connection::close",
                "left.Connection::close",
            ))
            .unwrap();
        table
            .admit_resource_record_lifecycle(lifecycle(
                "right.Connection",
                "right.Connection::close",
                "right.Connection::close",
            ))
            .unwrap();

        let registry = table.lifecycle_registry();
        assert_eq!(registry.resource_records().len(), 2);
        assert!(registry
            .resource_record(&DefId::new("Connection"))
            .is_none());
        assert_eq!(
            registry
                .resource_record(&DefId::new("right.Connection"))
                .unwrap()
                .close_declaration,
            DefId::new("right.Connection::close")
        );

        assert!(table
            .admit_resource_record_lifecycle(lifecycle(
                "left.Connection",
                "other.close",
                "other_close",
            ))
            .is_err());
        assert_eq!(
            table
                .lifecycle_registry()
                .resource_record(&DefId::new("left.Connection"))
                .unwrap()
                .close_declaration,
            DefId::new("left.Connection::close"),
            "duplicate admission must not overwrite the sole close authority"
        );
    }
}
