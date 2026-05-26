//! HIR is the first IR layer after parsing. It gives every binding and
//! value-bearing expression a stable identity before later THIR/MIR ownership
//! analysis attaches value-model decisions.

pub mod builtin_type_classes;
pub mod diagnostic;
pub mod dispatch;
pub mod dump;
pub mod ids;
pub mod intent;
pub mod lower;
pub mod monomorph;
pub mod node;
pub mod stdlib_catalog;
pub mod value_class;
pub mod verify;

pub use diagnostic::{HirDiagnostic, HirDiagnosticKind, ImportedItemKind};
pub use dump::dump_hir;
pub use ids::{BindingId, HirNodeId, ItemId, ResolvedRef, ScopeId, SiteId};
pub use intent::IntentKind;
#[cfg(any(test, feature = "internal-test-hooks"))]
pub use lower::run_call_shape_gates_for_test;
pub use lower::{
    lower_program, lower_program_host_target, lower_program_with_mono_cap, LowerOutput,
    ResolutionCtx, TargetArch,
};
pub use monomorph::{
    mangle, substitute_type_params, EnumLayout, EnumMonoKey, EnumVariantLayout, MonoKey,
    MonomorphizedFn, RecordLayout, RecordMonoKey, MONOMORPHISATION_REGISTRY_CAP,
};
pub use node::{
    HirActorDecl, HirActorInit, HirActorMethod, HirActorReceiveFn, HirActorStateGuard, HirBinding,
    HirBlock, HirCaptureKind, HirClosureCapture, HirExpr, HirExprKind, HirField, HirFn, HirItem,
    HirLambdaCapture, HirLifecycleHook, HirLifecycleHookKind, HirLiteral, HirMachineDecl,
    HirMachineEvent, HirMachineState, HirMachineTransition, HirMatchArm, HirMatchArmBinding,
    HirMatchArmPredicate, HirModule, HirRegexLiteral, HirRestartPolicy, HirSelect, HirSelectArm,
    HirSelectArmKind, HirStmt, HirStmtKind, HirSupervisorChild, HirSupervisorDecl,
    HirSupervisorStrategy, HirTypeDecl, HirVariant, HirVariantKind,
};
pub use value_class::{
    contains_named_type, lookup_type_marker, named_type_components, named_type_names,
    NamedTypeComponent, ResourceMarker, TypeClassTable, ValueClass,
};
pub use verify::verify_hir;

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
