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
pub mod declarative_vec_ffi;
pub(crate) mod eligibility_walker;
pub mod env;
pub(crate) mod eq_eligibility;
pub mod error;
pub mod extern_symbol;
pub(crate) mod hash_eligibility;
pub mod lang_items;
pub mod lowering_facts;
pub mod method_resolution;
pub mod module_registry;
pub mod resolved_ty;
pub mod runtime_call;
pub mod runtime_calling_convention;
pub mod stdlib;
pub mod stdlib_loader;
pub mod traits;
pub mod ty;
pub mod type_descriptor;
pub mod unify;

pub use actor_protocol::{
    compute_default_msg_id, qualified_handler_name, ActorHandlerDescriptor, ActorHandlerSpec,
    ActorProtocolCollision, ActorProtocolDescriptor,
};
pub use builtin_type::{builtin_types, lookup_builtin_type, BuiltinType, BuiltinTypeInfo};
pub use check::{
    builtin_function_names, ActorMethodKind, ActorSendAliasing, ActorSendCopyReason,
    ActorStateGuard, ArmResolution, AssignTargetKind, AssignTargetShape, Bound, CallAbiHint,
    CaptureModeOrigin, Checker, ChildKind, ChildSlot, ClosureCaptureFact, ClosureCaptureMode,
    ClosureEscapeFact, ClosureEscapeKind, ClosureEscapeRule, DynAssocBinding, DynCoercion,
    DynMethodCall, DynVtableEntry, DynVtableKey, ExecutionContextReader, FnSig, HashMapMethod,
    HashSetMethod, ImplDef, ImplId, ImplRegistry, LookupError, MachineMethodKind, MathGenericOp,
    MethodCallReceiverKind, MethodCallRewrite, MethodTarget, MethodTargetFamily,
    NumericMethodFamily, NumericMethodLowering, NumericMethodOp, NumericSignedness, NumericWidth,
    OptionResultMethod, PatternKind, PayloadBinding, PayloadVariantPattern, ResolvedCall,
    RuntimeAbi, SpanKey, TyPattern, TypeCheckOutput, VariantDef, VariantMatch, VecHigherOrderOp,
    VecMethod, WidthCastKind, WidthCastLowering, WireCodecDirection, WireFieldLayout,
    WireLayoutEntry, WireLayoutTable,
};
pub use error::TypeError;
pub use extern_symbol::{
    ExternSymbolSpec, ExternSymbolTemplate, PlaceholderName, TemplateError, TemplateExpansionError,
    TemplateSegment,
};
pub use lang_items::{LangItemBinding, LangItemRegistry, LANG_ITEM_DISPLAY, LANG_ITEM_DISPLAY_FMT};
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
pub use ty::{TraitObjectBound, Ty};
pub use type_descriptor::TypeDescriptor;
