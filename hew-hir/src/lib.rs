//! HIR is the first IR layer after parsing. It gives every binding and
//! value-bearing expression a stable identity before later THIR/MIR ownership
//! analysis attaches value-model decisions.

pub mod builtin_type_classes;
pub mod diagnostic;
pub mod dispatch;
pub mod dump;
pub mod ids;
pub mod intent;
pub mod layout_mono;
pub mod lower;
pub mod machine_mono;
pub mod mono;
pub mod monomorph;
pub mod node;
pub mod stdlib_catalog;
pub mod value_class;
pub mod verify;

pub use diagnostic::{HirDiagnostic, HirDiagnosticKind, ImportedItemKind};
pub use dump::dump_hir;
pub use ids::{BindingId, HirNodeId, ItemId, ResolvedRef, ScopeId, SiteId};
pub use intent::IntentKind;
pub use layout_mono::run_layout_mono_pass;
#[cfg(any(test, feature = "internal-test-hooks"))]
pub use lower::run_call_shape_gates_for_test;
pub use lower::{
    lower_program, lower_program_host_target, lower_program_with_mono_cap, LowerOutput,
    ResolutionCtx, TargetArch,
};
pub use machine_mono::run_machine_mono_pass;
pub use mono::{
    mangle_instantiation, sanitize_for_symbol, ActorMonoKey, ConstValue as MonoConstValue,
    FunctionMonoKey, MachineMonoEntry, MachineMonoKey, MonoKind, SymbolClass,
};
pub use monomorph::{
    mangle, mangle_resolved_ty, substitute_type_params, EnumLayout, EnumMonoKey, EnumVariantLayout,
    MonoKey, MonomorphizedFn, RecordLayout, RecordMonoKey, MONOMORPHISATION_REGISTRY_CAP,
};
pub use node::{
    HirActorDecl, HirActorInit, HirActorMethod, HirActorReceiveFn, HirActorStateGuard, HirBinding,
    HirBlock, HirCaptureKind, HirClosureCapture, HirConst, HirConstValue, HirExpr, HirExprKind,
    HirField, HirFn, HirItem, HirJoin, HirJoinBranch, HirLambdaCapture, HirLifecycleHook,
    HirLifecycleHookKind, HirLiteral, HirMachineBound, HirMachineDecl, HirMachineEvent,
    HirMachineState, HirMachineTransition, HirMatchArm, HirMatchArmBinding, HirMatchArmPredicate,
    HirModule, HirPayloadPredicate, HirPayloadVariantPredicate, HirRecordDecl, HirRegexLiteral,
    HirRestartPolicy, HirSelect, HirSelectArm, HirSelectArmKind, HirShutdownDirective, HirStmt,
    HirStmtKind, HirSupervisorChild, HirSupervisorDecl, HirSupervisorStrategy, HirTypeDecl,
    HirVarSelfMethodTarget, HirVariant, HirVariantKind, WhereOrigin,
};
pub use value_class::{
    contains_named_type, lookup_type_marker, lookup_type_marker_for_ty, named_type_components,
    named_type_names, NamedTypeComponent, ResourceMarker, TypeClassTable, ValueClass,
};
pub use verify::{collect_site_spans, verify_hir, HirSiteSource};

/// Convert a dotted module-qualified name to a native-object-safe symbol.
///
/// Dots in LLVM IR identifiers are legal (emitted as quoted globals,
/// e.g. `@"greeting.hello"`), but dots in native object-file symbol
/// names are problematic on macOS and Linux linkers.  This helper
/// replaces every `.` separator with `$` so the symbol is safe on all
/// supported targets.
///
/// Used symmetrically in HIR fn-registry keying, HIR fn-body emission,
/// MIR `module_fn_names` collection, and codegen `declare_function`.
/// All three call sites must use this function — drift between sites
/// produces wrong-code: the call target and the definition diverge.
///
/// Examples
/// --------
/// ```
/// use hew_hir::mangle_dotted_name;
/// assert_eq!(mangle_dotted_name("greeting.hello"), "greeting$hello");
/// assert_eq!(mangle_dotted_name("hello"),          "hello");
/// ```
#[must_use]
pub fn mangle_dotted_name(name: &str) -> String {
    name.replace('.', "$")
}
