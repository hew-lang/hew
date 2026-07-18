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
pub(crate) mod eligibility_walker;
pub mod env;
pub(crate) mod eq_eligibility;
pub mod error;
pub mod extern_symbol;
pub(crate) mod hash_eligibility;
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
pub mod stdlib_loader;
pub mod traits;
pub mod ty;
pub mod type_descriptor;
pub mod unify;
pub mod vec_authority;

pub use actor_protocol::{
    compute_default_msg_id, qualified_handler_name, ActorHandlerDescriptor, ActorHandlerSpec,
    ActorProtocolCollision, ActorProtocolDescriptor,
};
pub use builtin_type::{builtin_types, lookup_builtin_type, BuiltinType, BuiltinTypeInfo};
pub use check::{
    builtin_function_names, directive_suppresses, ActorMethodKind, ActorSendAliasing,
    ActorSendCopyReason, ActorStateGuard, ArmResolution, AssignTargetKind, AssignTargetShape,
    Bound, CallAbiHint, CaptureModeOrigin, Checker, ChildKind, ChildSlot, ClosureCaptureFact,
    ClosureCaptureMode, ClosureEscapeFact, ClosureEscapeKind, ClosureEscapeRule, DynAssocBinding,
    DynCoercion, DynMethodCall, DynVtableEntry, DynVtableKey, ExecutionContextReader, FnSig,
    HashMapMethod, HashSetMethod, ImplDef, ImplId, ImplRegistry, LintId, LintLevel, LintLevels,
    LintSources, LookupError, MachineMethodKind, MathGenericOp, MethodCallReceiverKind,
    MethodCallRewrite, MethodTarget, MethodTargetFamily, NumericMethodFamily,
    NumericMethodLowering, NumericMethodOp, NumericSignedness, NumericWidth, OptionResultMethod,
    PatternKind, PatternPlan, PayloadBinding, PayloadVariantPattern, PlanField, PlanSub,
    PoolAccessor, PoolAccessorKind, ResolvedCall, RuntimeAbi, SpanKey, TryConversionKind,
    TryWidthCastLowering, TyPattern, TypeCheckOutput, VariantDef, VariantMatch, VecHigherOrderOp,
    VecMethod, WidthCastKind, WidthCastLowering, WireCodecDirection, WireFieldLayout,
    WireLayoutEntry, WireLayoutTable, WireTextFormat,
};
pub use error::TypeError;
pub use extern_symbol::{
    ExternSymbolSpec, ExternSymbolTemplate, PlaceholderName, TemplateError, TemplateExpansionError,
    TemplateSegment,
};
pub use lang_items::{
    LangItem, LangItemBinding, LangItemRegistry, LANG_ITEM_DISPLAY, LANG_ITEM_DISPLAY_FMT,
};
pub use lowering_facts::{
    assert_lowering_facts_consistent, hashmap_layout_key_fact,
    hashmap_layout_key_layout_value_fact, hashset_layout_element_admissible, hashset_layout_fact,
    DropKind, HashMapAbi, HashMapKeyType, HashMapLoweringFact, HashMapLoweringFactError,
    HashMapLoweringFactState, HashMapValueType, HashSetAbi, HashSetElementType,
    HashSetLoweringFact, HashSetLoweringFactError, LoweringFact, LoweringFactConsistencyError,
    LoweringFactError, LoweringKind,
};
pub use resolved_ty::{BoundaryError, ResolvedTraitBound, ResolvedTy};
pub use runtime_call::{
    AsyncSuspendKind, DescriptorError, MathIntrinsic, RuntimeCallDescriptor, RuntimeCallFamily,
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

/// Return the final segment of a dot-qualified name.
#[must_use]
pub fn short_name(name: &str) -> &str {
    name.rsplit_once('.').map_or(name, |(_, short)| short)
}

/// Native-only stdlib module short-names that are rejected on the wasm32 target
/// and in the browser sandbox.
///
/// This is the single authoritative list shared between the type checker's
/// call-form and value-position wasm32 guards (`check_method_call` in
/// `methods.rs`, `check_field_access` in `expressions.rs`) and the browser
/// sandbox profile gate (`hew-sandbox-wasm/src/profile.rs`).  Every guard that
/// rejects a value-position or call-position reference to these modules on
/// wasm32 must reference this const rather than maintaining its own copy.
///
/// Note: `crypto` is intentionally absent — only the specific symbol
/// `crypto.random_bytes` is rejected, not the entire `crypto` module.
pub const NATIVE_ONLY_WASM_MODULES: &[&str] = &[
    "stream",
    "http",
    "net",
    "process",
    "tls",
    "quic",
    "dns",
    "os",
    "encrypt",
    "sign",
    "http_client",
    "smtp",
];

#[cfg(test)]
mod tests {
    use super::short_name;

    #[test]
    fn short_name_uses_the_final_qualified_segment() {
        assert_eq!(short_name("a.b.c"), "c");
        assert_eq!(short_name("Name"), "Name");
    }
}
