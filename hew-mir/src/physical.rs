//! Target-realized MIR consumed by native code generation.
//!
//! This is the only IR below ownership SIR. It records concrete storage,
//! layouts, private call ABI transfers, and CFG edges. Ownership has already
//! been decided by SIR: this lowering resolves each explicit copy or destroy
//! exactly once to a physical action and never infers another lifetime.

pub use hew_sir::{
    ActorId, ActorIngressAdapter, ActorOperation, LocalObservationKind, RemoteObservationKind,
    SemActor, SemActorCoalesce, SemActorField, SemActorHandler, SemActorOverflow,
    SemCoalesceFallback, SemCoalesceKey, SemCoalesceKeyKind, SemFailureDisplay, SemRestartPolicy,
    SemRestartStrategy, SemSupervisedRole, SemSupervisor, SemVariantKind, SupervisorId,
    TaskScopeJoinMode, TaskSelectionOrder,
};
use hew_types::runtime_call::{sequence_element_type, ArrayValueOp};

pub use hew_sir::{LeafContents, SemWireKind, SemWirePlan};
use std::collections::{BTreeMap, BTreeSet};

#[path = "physical_wire.rs"]
mod wire;

#[path = "physical_capability.rs"]
mod capability;
pub use capability::{PhysicalValueCapability, PhysicalValueMethod};

#[path = "physical_structural.rs"]
mod structural;
pub use structural::{
    PhysicalStructuralCase, PhysicalStructuralField, PhysicalStructuralGlue, PhysicalStructuralId,
    PhysicalStructuralShape,
};

use hew_parser::ast::{BinaryOp, UnaryOp};
#[path = "physical_callable.rs"]
mod callable;
#[path = "physical_defer.rs"]
mod defer;
#[path = "physical_extern.rs"]
mod extern_abi;
pub use extern_abi::PhysicalExternResultAbi;
#[cfg(test)]
#[path = "physical_defer_tests.rs"]
mod defer_tests;
#[path = "physical_partial.rs"]
mod partial;
#[path = "physical_release.rs"]
mod release;
pub use release::ReleaseEffects;
#[cfg(test)]
#[path = "physical_select_tests.rs"]
mod select_tests;

#[path = "physical_generators.rs"]
mod generators;

#[path = "physical_suspend.rs"]
mod suspend;
pub use partial::{
    PhysicalAggregateStep, PhysicalCleanup, PhysicalPlaceLeaf, PhysicalPlaceStorage,
};
#[cfg(test)]
#[path = "physical_encoding_fixture.rs"]
mod encoding_fixture;
#[cfg(test)]
#[path = "physical_encoding_tests.rs"]
mod encoding_tests;
#[cfg(test)]
#[path = "physical_local_fixture.rs"]
mod local_fixture;
#[cfg(test)]
#[path = "physical_local_tests.rs"]
mod local_tests;
#[cfg(test)]
#[path = "physical_partial_fixture.rs"]
mod partial_fixture;
#[cfg(test)]
#[path = "physical_partial_tests.rs"]
mod partial_tests;

#[cfg(test)]
#[path = "physical_resource_tests.rs"]
mod resource_tests;

#[cfg(test)]
#[path = "physical_panic_tests.rs"]
mod panic_tests;

use hew_sir::{
    AggregateShapeRef, BoundaryDecision, CallResult, CallUnwind, Edge, SemFunction, SemModule,
    SemOp, SemOpKind, SemTerminator, SnapshotDecision, ValueId,
};
pub use hew_sir::{
    BlockId, CallableId, ClosureId, DeferId, DeferScopeId, FaultParkId, OwnKind, ResourceCarrier,
    ResourceRelease, SemParamPassing, TaskScopeId, TrapKind,
};
use hew_types::runtime_call::{
    collection_type_arguments, shared_handle_payload, MapValueOp, SetValueOp,
};
pub use hew_types::runtime_call::{EncodingFormat, EncodingOp};
use hew_types::{
    BuiltinType, CloneKind, EntryExitPlan, ResolvedTy, RuntimeArgumentEffect, RuntimeCallFamily,
    RuntimePhysicalForm, RuntimeResultEffect, TypeInstanceKey, ValueCapability, VecValueOp,
};

#[path = "physical_types.rs"]
mod types;
pub use types::*;

#[path = "physical_module_lower.rs"]
mod module_lower;
pub use module_lower::*;

#[path = "physical_function_lower.rs"]
mod function_lower;

#[path = "physical_verify_structural.rs"]
mod verify_structural;
pub use verify_structural::*;

#[path = "physical_verify_ops.rs"]
mod verify_ops;
#[cfg(test)]
use verify_ops::require_no_live_borrows;
use verify_ops::{
    apply_edge, call_successors, call_successors_with_handback, consume_if_owned, define,
    initialized, initialized_slot, integer_bit_range, integer_width_bits, mask_to_width,
    runtime_receiver_release, verify_initialization, verify_operation_storage, BorrowDependents,
    FaultState, FlowState, InitState,
};

#[path = "physical_verify_terminator.rs"]
mod verify_terminator;
use verify_terminator::{merge_flow, terminator_successors, verify_terminator};

#[path = "physical_verify_calls.rs"]
mod verify_calls;
pub use verify_calls::*;

#[cfg(test)]
#[path = "physical_tests.rs"]
mod tests;
