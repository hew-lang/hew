//! Hew type checker with bidirectional inference.
//!
//! Implements constraint-based type inference with unification,
//! automatic marker trait derivation (Send, Frozen, Copy),
//! and exhaustive pattern match checking.

pub mod actor_protocol;
pub mod builtin_enums;
pub mod builtin_names;
pub mod builtin_type;
pub mod check;
pub mod cycle;
pub mod env;
pub(crate) mod eq_eligibility;
pub mod error;
pub mod extern_symbol;
pub mod extern_table;
pub mod ffi_contracts;
pub(crate) mod hash_eligibility;
pub mod identity;
pub mod jit_symbols;
pub mod lang_items;
pub mod lowering_facts;
pub mod method_resolution;
pub mod module_registry;
pub mod resolved_ty;
pub mod runtime_call;
pub mod runtime_calling_convention;
pub mod stdlib;
pub mod stdlib_authority;
pub mod stdlib_catalog_identity;
pub mod stdlib_loader;
pub mod traits;
pub mod ty;
pub mod type_descriptor;
pub mod unify;
pub mod vec_authority;
mod wasm_capabilities_generated;

pub use actor_protocol::{
    compute_default_msg_id, qualified_handler_name, ActorHandlerDescriptor, ActorHandlerSpec,
    ActorProtocolCollision, ActorProtocolDescriptor,
};
pub use builtin_type::{
    builtin_types, has_builtin_associated_item_identity, lookup_builtin_type,
    lookup_source_owned_lifecycle_type, source_owned_lifecycle_owner, BuiltinType, BuiltinTypeInfo,
    SourceOwnedLifecycleOwner, SOURCE_OWNED_LIFECYCLE_OWNERS,
};
pub use check::{
    builtin_function_names, directive_suppresses, ActorMethodKind, ActorStateGuard, ArmResolution,
    AssignTargetKind, AssignTargetShape, Bound, CallAbiHint, CallTarget, CaptureModeOrigin,
    Checker, ChildKind, ChildSlot, ClosureCaptureFact, ClosureCaptureMode, ClosureEscapeFact,
    ClosureEscapeKind, ClosureEscapeRule, DynAssocBinding, DynCoercion, DynMethodCall,
    DynVtableEntry, DynVtableKey, ExecutionContextReader, FnSig, HashMapMethod, HashSetMethod,
    ImplDef, ImplId, ImplRegistry, LintId, LintLevel, LintLevels, LintSources, LookupError,
    MachineMethodKind, MathGenericOp, MethodCallReceiverKind, MethodCallRewrite, MethodTarget,
    MethodTargetFamily, NumericMethodFamily, NumericMethodLowering, NumericMethodOp,
    NumericSignedness, NumericWidth, OpaqueResourceCandidateGraph,
    OpaqueResourceLifecycleCandidate, OpaqueResourceLifecycleConflict,
    OpaqueResourceLifecycleConflictKind, OptionResultMethod, PatternKind, PatternPlan,
    PayloadBinding, PayloadVariantPattern, PlanField, PlanSub, PoolAccessor, PoolAccessorKind,
    ProducedValueDependency, ProducedValueFact, RcIntrinsicOp, ResolvedCall, RuntimeAbi, SpanKey,
    TryConversionKind, TryWidthCastLowering, TyPattern, TypeCheckOutput, UserComparisonDispatch,
    VariantDef, VariantMatch, VecHigherOrderOp, VecMethod, WidthCastKind, WidthCastLowering,
    WireCodecDirection, WireFieldLayout, WireFieldPresence, WireLayoutEntry, WireLayoutTable,
    WireTextFormat,
};
pub use error::TypeError;
pub use extern_symbol::{
    ExternSymbolSpec, ExternSymbolTemplate, PlaceholderName, TemplateError, TemplateExpansionError,
    TemplateSegment,
};
pub use identity::{
    DeclarationIdentityError, DeclarationKind, DeclarationOccurrence, IdentityView, ModuleId,
};
pub use lang_items::{
    LangItem, LangItemBinding, LangItemRegistry, LANG_ITEM_DISPLAY, LANG_ITEM_DISPLAY_FMT,
};
pub use lowering_facts::{
    assert_lowering_facts_consistent, hashmap_layout_key_fact,
    hashmap_layout_key_layout_value_fact, hashset_layout_element_admissible, hashset_layout_fact,
    CollectionMethodDispatch, DropKind, HashMapAbi, HashMapKeyType, HashMapLoweringFact,
    HashMapLoweringFactError, HashMapLoweringFactState, HashMapValueType, HashSetAbi,
    HashSetElementType, HashSetLoweringFact, HashSetLoweringFactError, LoweringFact,
    LoweringFactConsistencyError, LoweringFactError, LoweringKind,
};
pub use resolved_ty::{
    default_impl_method_declaration, BoundaryError, NominalInstance, ResolvedTraitBound, ResolvedTy,
};
pub use runtime_call::{
    AsyncSuspendKind, DescriptorError, MathIntrinsic, ProducedArgumentBoundary,
    ProducedValueAcquisition, ProducedValueOwnership, RuntimeCallDescriptor, RuntimeCallFamily,
    RuntimeDropDescriptor, StreamElementKind, VecGetElem, VecSliceElem,
};
pub use runtime_calling_convention::RuntimeCallingConvention;
pub use stdlib_authority::{
    authority as stdlib_authority, AuthorityBinding, AuthorityDeclarationKind, AuthorityError,
    AuthorityErrorKind, AuthoritySource, DiagnosticItem, EnumVariantOrder, ExternAbiEntry,
    ExternAbiFact, ExternRuntimeCapability, ExternRuntimeCapabilityEntry, Intrinsic, OverloadGroup,
    PreludeExport, PreludeExportKind, StdlibAuthority, StdlibRoot, STDLIB_AUTHORITY,
    SUBSTRATE_SOURCES,
};
pub use ty::{TraitObjectBound, Ty};
pub use type_descriptor::TypeDescriptor;
pub use vec_authority::VecElementToken;
pub use wasm_capabilities_generated::{
    wasm_capability_ids, WasmCapabilityId, WasmFeatureDisposition, WasmFunctionRejection,
    WasmModuleRejection, WasmUnsupportedFeature, NATIVE_ONLY_WASM_FUNCTION_REJECTIONS,
    NATIVE_ONLY_WASM_MODULES, NATIVE_ONLY_WASM_MODULE_REJECTIONS,
};

/// Canonical identity of one declared definition.
///
/// A `DefId` deliberately stores the complete declaration path rather than a
/// leaf spelling.  It is suitable for semantic maps and dispatch tables; use
/// [`DefId::display_name`] only when rendering a diagnostic.
///
/// # Compile-time boundary
///
/// A downstream layer cannot mint a definition identity from a leaf spelling:
///
/// ```compile_fail
/// use hew_types::DefId;
///
/// let leaf = "Widget";
/// let _identity = DefId::new(leaf);
/// ```
///
/// Fixture construction is test-only as well:
///
/// ```compile_fail
/// use hew_types::DefId;
///
/// let leaf = "Widget";
/// let _identity = DefId::for_test(leaf);
/// ```
///
/// The resolver/checker owns declaration minting; downstream phases receive a
/// `DefId` and carry it unchanged.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(transparent)]
pub struct DefId {
    full_path: String,
}

impl std::borrow::Borrow<str> for DefId {
    fn borrow(&self) -> &str {
        &self.full_path
    }
}

impl DefId {
    /// Construct an identity from a path minted by the checker/resolver.
    ///
    /// This is intentionally crate-private: declaration identity is minted
    /// once while resolving declarations, then carried through later compiler
    /// phases.
    ///
    /// # Panics
    ///
    /// Panics when `full_path` is empty, because an empty declaration path has
    /// no canonical identity.
    #[must_use]
    pub(crate) fn from_minted_path(
        full_path: impl Into<String>,
        _authority: crate::identity::MintingAuthority,
    ) -> Self {
        let full_path = full_path.into();
        assert!(
            !full_path.is_empty(),
            "DefId requires a non-empty canonical declaration path"
        );
        Self { full_path }
    }

    /// Temporarily reconstruct an identity in a downstream compiler phase.
    ///
    /// This is the sole migration escape hatch while declaration identities are
    /// threaded through HIR, MIR, and code generation. Remove each use by
    /// carrying the resolver-minted [`DefId`] instead.
    #[deprecated(
        note = "carry the resolver-minted DefId; this temporary migration escape hatch must not mint new identity"
    )]
    #[must_use]
    pub fn legacy_reconstruct_from_full_path(full_path: impl Into<String>) -> Self {
        crate::identity::legacy_reconstruct_def_id(full_path)
    }

    /// Create a fixture identity without granting production code a minting API.
    ///
    /// This seam exists only in test builds or with the explicit
    /// `test` feature for direct test dependencies.
    #[cfg(any(test, feature = "test"))]
    #[doc(hidden)]
    #[must_use]
    pub fn for_test(full_path: impl Into<String>) -> Self {
        crate::identity::test_def_id(full_path)
    }

    /// The canonical full declaration path used for identity and linker
    /// derivation.
    #[must_use]
    pub fn full_path(&self) -> &str {
        &self.full_path
    }

    /// The non-authoritative display leaf for diagnostics.
    #[must_use]
    pub fn display_name(&self) -> &str {
        short_name(&self.full_path)
    }
}

/// Canonical identity of a declared nominal type.
///
/// A nominal is backed by the declaration identity, so two `Box` declarations
/// from different modules can never compare equal merely because they share a
/// leaf spelling.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct NominalId {
    declaration: DefId,
}

impl NominalId {
    #[must_use]
    pub(crate) fn from_minted_declaration(declaration: DefId) -> Self {
        Self { declaration }
    }

    /// Temporarily reconstruct a nominal identity in a downstream compiler
    /// phase. Remove each use by carrying the resolver-minted `NominalId`.
    #[deprecated(
        note = "carry the resolver-minted NominalId; this temporary migration escape hatch must not mint new identity"
    )]
    #[must_use]
    pub fn legacy_reconstruct_from_full_path(full_path: impl Into<String>) -> Self {
        crate::identity::legacy_reconstruct_nominal_id(full_path)
    }

    /// Create a fixture nominal identity without granting production code a
    /// minting API.
    #[cfg(any(test, feature = "test"))]
    #[doc(hidden)]
    #[must_use]
    pub fn for_test(full_path: impl Into<String>) -> Self {
        crate::identity::test_nominal_id(full_path)
    }

    #[must_use]
    pub fn declaration(&self) -> &DefId {
        &self.declaration
    }

    #[must_use]
    pub fn full_path(&self) -> &str {
        self.declaration.full_path()
    }

    #[must_use]
    pub fn display_name(&self) -> &str {
        self.declaration.display_name()
    }
}

/// Return the final segment of a dot-qualified name.
#[must_use]
pub fn short_name(name: &str) -> &str {
    name.rsplit_once('.').map_or(name, |(_, short)| short)
}

/// Return the full current-module owner that a self-qualified type spelling
/// denotes, without guessing whether that owner actually declares the type.
///
/// For example, in `hew.alpha.render`, `render.Box` has the candidate owner
/// `hew.alpha.render.Box`. Callers must still prove that exact declaration
/// exists in their authority table. An explicit import binding is the only
/// authority that may take precedence over this lexical self spelling.
#[must_use]
pub fn current_module_qualified_type_candidate(
    current_module: Option<&str>,
    type_spelling: &str,
) -> Option<String> {
    let (binding, tail) = type_spelling.split_once('.')?;
    let owner = current_module?;
    (binding == short_name(owner)).then(|| format!("{owner}.{tail}"))
}
#[cfg(test)]
mod tests {
    use super::{current_module_qualified_type_candidate, short_name, DefId, NominalId};

    #[test]
    fn short_name_uses_the_final_qualified_segment() {
        assert_eq!(short_name("a.b.c"), "c");
        assert_eq!(short_name("Name"), "Name");
    }

    #[test]
    fn current_module_candidate_preserves_the_full_nested_owner() {
        assert_eq!(
            current_module_qualified_type_candidate(Some("hew.alpha.render"), "render.Box"),
            Some("hew.alpha.render.Box".to_string())
        );
        assert_eq!(
            current_module_qualified_type_candidate(Some("hew.alpha.render"), "other.render.Box"),
            None
        );
    }

    #[test]
    fn canonical_ids_keep_same_leaf_declarations_distinct() {
        let left = NominalId::for_test("left.Box");
        let right = NominalId::for_test("right.Box");
        assert_ne!(left, right);
        assert_eq!(left.display_name(), "Box");
        assert_eq!(left.full_path(), "left.Box");
        assert_ne!(DefId::for_test("left.Box"), DefId::for_test("right.Box"));
    }
}
