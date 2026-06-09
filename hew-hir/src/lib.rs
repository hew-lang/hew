//! Resolved HIR for the Hew v0.5 compiler pipeline.
//!
//! HIR is the first IR layer after parsing. It gives every binding and
//! value-bearing expression a stable identity before later THIR/MIR ownership
//! analysis attaches value-model decisions.

pub mod builtin_type_classes;
pub mod diagnostic;
pub mod dump;
pub mod ids;
pub mod intent;
pub mod lower;
pub mod monomorph;
pub mod node;
pub mod value_class;
pub mod verify;

pub use diagnostic::{HirDiagnostic, HirDiagnosticKind};
pub use dump::dump_hir;
pub use ids::{BindingId, HirNodeId, ItemId, ResolvedRef, ScopeId, SiteId};
pub use intent::IntentKind;
pub use lower::{lower_program, lower_program_with_mono_cap, LowerOutput, ResolutionCtx};
pub use monomorph::{
    mangle, substitute_type_params, MonoKey, MonomorphizedFn, RecordLayout, RecordMonoKey,
    MONOMORPHISATION_REGISTRY_CAP,
};
pub use node::{
    HirActorDecl, HirActorInit, HirActorMethod, HirActorParam, HirActorReceiveFn, HirBinding,
    HirBlock, HirCaptureKind, HirExpr, HirExprKind, HirField, HirFn, HirItem, HirLambdaCapture,
    HirLifecycleHook, HirLifecycleHookKind, HirLiteral, HirMachineDecl, HirMachineEvent,
    HirMachineState, HirMachineTransition, HirModule, HirRestartPolicy, HirSelect, HirSelectArm,
    HirSelectArmKind, HirStmt, HirStmtKind, HirSupervisorChild, HirSupervisorDecl,
    HirSupervisorStrategy, HirTypeDecl,
};
pub use value_class::{contains_named_type, named_type_names, TypeClassTable, ValueClass};
pub use verify::verify_hir;
