//! Hew type checker with bidirectional inference.
//!
//! Implements constraint-based type inference with unification,
//! automatic marker trait derivation (Send, Frozen, Copy),
//! and exhaustive pattern match checking.

pub mod actor_delivery;
pub mod actor_protocol;
pub mod builtin_enums;
pub mod builtin_names;
pub mod builtin_type;
pub mod check;
pub mod cycle;
pub mod def_table;
pub mod dump;
pub mod env;
pub(crate) mod eq_eligibility;
pub mod error;
pub mod extern_symbol;
pub mod extern_table;
pub mod ffi_contracts;
pub(crate) mod hash_eligibility;
pub mod jit_symbols;
pub mod lang_items;
pub mod lowering_facts;
pub mod mangle;
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
pub mod type_facts;
pub mod unify;
pub mod value_class;
pub mod vec_authority;
mod wasm_capabilities_generated;

mod callable;
pub use callable::{ClosureCaptureAccess, ClosureCaptureAcquisition, ClosureCaptureConsumption};

pub use hew_parser::ast::{CallableCallMode, CallableCapabilities};

pub use actor_protocol::{
    compute_default_msg_id, qualified_handler_name, ActorHandlerDescriptor, ActorHandlerSpec,
    ActorProtocolCollision, ActorProtocolDescriptor, ReceiveFailureDisplay,
};
pub use builtin_type::{
    builtin_types, canonical_source_owned_lifecycle_name, has_builtin_associated_item_identity,
    lookup_builtin_type, lookup_source_owned_lifecycle_type, source_owned_lifecycle_owner,
    BuiltinType, BuiltinTypeInfo, SourceOwnedLifecycleOwner, SOURCE_OWNED_LIFECYCLE_OWNERS,
};
pub use check::{
    builtin_function_names, directive_suppresses, ActorMethodKind, ActorStateGuard, ArmResolution,
    AssignTargetKind, AssignTargetShape, Bound, CallAbiHint, CallTarget, Checker, ChildKind,
    ChildSlot, ClosureCaptureFact, ClosureEscapeFact, ClosureEscapeKind, ClosureEscapeRule,
    DynAssocBinding, DynCoercion, DynMethodCall, DynVtableEntry, DynVtableKey,
    EntryCallableInstance, EntryDisplayTarget, EntryExitAction, EntryExitPlan, EntryIntegerType,
    ExecutionContextReader, ExternMethodSignature, FnSig, HashMapMethod, HashSetMethod, ImplDef,
    ImplId, ImplRegistry, LintId, LintLevel, LintLevels, LintSources, LookupError,
    MachineMethodKind, MathGenericOp, MethodCallReceiverKind, MethodCallRewrite, MethodTarget,
    MethodTargetFamily, OpaqueResourceCandidateGraph, OpaqueResourceLifecycleCandidate,
    OpaqueResourceLifecycleConflict, OpaqueResourceLifecycleConflictKind, PatternKind, PatternPlan,
    PayloadBinding, PayloadLiteralPattern, PayloadVariantPattern, PlanField, PlanSub, PoolAccessor,
    PoolAccessorKind, RcIntrinsicOp, ReceiverUpdate, ResolvedCall, ResolvedTraitDefault,
    ResultReturnKind, RuntimeAbi, SpanKey, TryConversionKind, TryWidthCastLowering, TyPattern,
    TypeAliasDef, TypeCheckOutput, UserComparisonDispatch, VariantDef, VariantMatch,
    VecHigherOrderOp, VecMethod, WidthCastKind, WidthCastLowering, WireCodecDirection,
    WireFieldLayout, WireFieldPresence, WireLayoutEntry, WireLayoutTable, WireTextFormat,
};
pub use def_table::{
    DeclarationIdentityError, DeclarationKind, DeclarationOccurrence, DefId, DefTable, ModuleId,
    NominalId, Predicate, TypeParamId,
};
pub use error::TypeError;
pub use extern_symbol::{
    ExternSymbolSpec, ExternSymbolTemplate, PlaceholderName, TemplateError, TemplateExpansionError,
    TemplateSegment,
};
pub use hew_parser::ast::Symbol;
pub use lang_items::{
    LangItem, LangItemBinding, LangItemRegistry, LANG_ITEM_DISPLAY, LANG_ITEM_DISPLAY_FMT,
};
pub use lowering_facts::{
    DropKind, HashSetAbi, HashSetElementType, LoweringFact, LoweringFactError, LoweringKind,
};
pub use mangle::mangle_resolved_ty;
pub use resolved_ty::{BoundaryError, NominalInstance, ResolvedTraitBound, ResolvedTy};
pub use runtime_call::{
    vector_element_type, AsyncSuspendKind, DescriptorError, EncodingFormat, EncodingOp,
    MathIntrinsic, RuntimeArgumentContract, RuntimeArgumentEffect, RuntimeCReturn,
    RuntimeCallDescriptor, RuntimeCallFamily, RuntimeDropDescriptor, RuntimeInstantiatedContract,
    RuntimeLogicalFailure, RuntimeOpRow, RuntimePhysicalForm, RuntimeResultEffect,
    RuntimeSemanticContract, RuntimeStaging, RuntimeValueKind, RuntimeVariantResultKind,
    VecGetElem, VecSliceElem, VecValueOp,
};
pub use runtime_calling_convention::RuntimeCallingConvention;
pub use stdlib_authority::{
    authority as stdlib_authority, AuthorityBinding, AuthorityDeclarationKind, AuthorityError,
    AuthorityErrorKind, AuthoritySource, DiagnosticItem, EnumVariantOrder, ExternAbiEntry,
    ExternAbiFact, ExternRuntimeCapability, ExternRuntimeCapabilityEntry, Intrinsic, OverloadGroup,
    PreludeExport, StdlibAuthority, StdlibRoot, STDLIB_AUTHORITY, SUBSTRATE_SOURCES,
};
pub use ty::{NominalHead, ParamHead, TraitObjectBound, TraitRef, Ty, TypeHead};
pub use type_descriptor::TypeDescriptor;
pub use type_facts::push_type_components;
pub use type_facts::{
    CloneKind, SendFact, TypeFactContext, TypeFactService, TypeFacts, TypeInstanceKey,
    ValueCapability, ValueMethodPlan, ValueMethodSelection,
};
pub use value_class::{ClassContext, ClassError, DeclarationMarker, DeclaredType, ValueClass};
pub use vec_authority::VecElementToken;
pub use wasm_capabilities_generated::{
    wasm_capability_ids, WasmCapabilityId, WasmFeatureDisposition, WasmFunctionRejection,
    WasmModuleRejection, WasmUnsupportedFeature, NATIVE_ONLY_WASM_FUNCTION_REJECTIONS,
    NATIVE_ONLY_WASM_MODULES, NATIVE_ONLY_WASM_MODULE_REJECTIONS,
};

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
    use super::{current_module_qualified_type_candidate, short_name, DefTable, Symbol};

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
        let mut defs = DefTable::new();
        let left = defs.mint_nominal_for_test("left.Box");
        let right = defs.mint_nominal_for_test("right.Box");
        assert_ne!(left, right);
        assert_eq!(defs.name(left.declaration()), Symbol::intern("left.Box"));
        assert_eq!(defs.display(left.declaration()), "Box");
        assert_eq!(defs.path(left.declaration()), "left.Box");
    }

    #[test]
    fn def_id_is_copy() {
        fn assert_copy<T: Copy>() {}
        assert_copy::<super::DefId>();
        assert_copy::<super::NominalId>();
    }
}
