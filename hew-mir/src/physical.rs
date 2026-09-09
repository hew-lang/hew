//! Target-realized MIR consumed by native code generation.
//!
//! This is the only IR below ownership SIR. It records concrete storage,
//! layouts, private call ABI transfers, and CFG edges. Ownership has already
//! been decided by SIR: this lowering resolves each explicit copy or destroy
//! exactly once to a physical action and never infers another lifetime.

pub use hew_sir::{
    ActorId, ActorOperation, LocalObservationKind, SemActor, SemActorField, SemActorHandler,
    SemActorOverflow, SemFailureDisplay, SemRestartPolicy, SemRestartStrategy, SemSupervisedRole,
    SemSupervisor, SupervisorId, TaskScopeJoinMode, TaskSelectionOrder,
};
use hew_types::runtime_call::{sequence_element_type, ArrayValueOp, MathIntrinsic};

pub use hew_sir::{SemWireKind, SemWirePlan};
use std::collections::{BTreeMap, BTreeSet};

#[path = "physical_wire.rs"]
mod wire;

#[path = "physical_capability.rs"]
mod capability;
pub use capability::{PhysicalValueCapability, PhysicalValueMethod};

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
use hew_types::runtime_call::{collection_type_arguments, MapValueOp, SetValueOp};
pub use hew_types::runtime_call::{EncodingFormat, EncodingOp};
use hew_types::{
    BuiltinType, CloneKind, EntryExitPlan, ResolvedTy, RuntimeArgumentEffect, RuntimeCallFamily,
    RuntimeResultEffect, TypeInstanceKey, ValueCapability, VecValueOp,
};

/// Function-local identity of one concrete storage allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StorageId(pub u32);

/// Module-local identity of one verified aggregate layout and glue recipe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalAggregateId(pub u32);

/// Module-local identity of one verified tagged-variant glue recipe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalVariantId(pub u32);

/// Module-local identity of one exact vector element copy/drop recipe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalVectorId(pub u32);

/// Module-local identity of a map key/value copy/drop recipe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalMapId(pub u32);

/// Module-local identity of a set element copy/drop recipe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalSetId(pub u32);

/// Module-local identity of an exact resource release contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalResourceId(pub u32);

/// Checked semantic authority for a pointer-carried affine resource.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalResourceDescriptor {
    pub ty: ResolvedTy,
    pub release: hew_sir::ResourceRelease,
}

/// A canonical map and its exact key and value types, before target layout.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalMapDescriptor {
    pub ty: ResolvedTy,
    pub key: ResolvedTy,
    pub value: ResolvedTy,
}

/// A canonical set and its exact element type, before target layout.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalSetDescriptor {
    pub ty: ResolvedTy,
    pub element: ResolvedTy,
}

/// A vector value and its exact semantic element, before target layout.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVectorDescriptor {
    pub ty: ResolvedTy,
    pub element: ResolvedTy,
}

/// One exact demanded aggregate descriptor in the physical type inventory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalAggregateDescriptor {
    pub ty: ResolvedTy,
    pub fields: Vec<ResolvedTy>,
}

/// One exact demanded tagged-variant descriptor in the physical inventory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVariantDescriptor {
    pub ty: ResolvedTy,
    pub is_indirect: bool,
    pub variants: Vec<Vec<ResolvedTy>>,
}

/// Concrete types and aggregate shapes demanded by verified SIR bodies.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PhysicalTypeInventory {
    types: BTreeSet<ResolvedTy>,
    resources: BTreeMap<ResolvedTy, PhysicalResourceDescriptor>,
    aggregates: BTreeMap<ResolvedTy, PhysicalAggregateDescriptor>,
    variants: BTreeMap<ResolvedTy, PhysicalVariantDescriptor>,
    vectors: BTreeMap<ResolvedTy, PhysicalVectorDescriptor>,
    maps: BTreeMap<ResolvedTy, PhysicalMapDescriptor>,
    sets: BTreeMap<ResolvedTy, PhysicalSetDescriptor>,
}

impl PhysicalTypeInventory {
    #[must_use]
    pub fn contains(&self, ty: &ResolvedTy) -> bool {
        self.types.contains(ty)
    }

    pub fn types(&self) -> impl Iterator<Item = &ResolvedTy> {
        self.types.iter()
    }

    pub fn resources(&self) -> impl Iterator<Item = &PhysicalResourceDescriptor> {
        self.resources.values()
    }

    pub fn aggregates(&self) -> impl Iterator<Item = &PhysicalAggregateDescriptor> {
        self.aggregates.values()
    }

    pub fn variants(&self) -> impl Iterator<Item = &PhysicalVariantDescriptor> {
        self.variants.values()
    }

    pub fn maps(&self) -> impl Iterator<Item = &PhysicalMapDescriptor> {
        self.maps.values()
    }

    pub fn sets(&self) -> impl Iterator<Item = &PhysicalSetDescriptor> {
        self.sets.values()
    }

    pub fn vectors(&self) -> impl Iterator<Item = &PhysicalVectorDescriptor> {
        self.vectors.values()
    }
}

/// LLVM-independent carrier chosen by the target layout resolver.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PhysicalRepr {
    Unit,
    Integer {
        bits: u16,
    },
    Float {
        bits: u16,
    },
    Pointer,
    Array {
        element: Box<PhysicalLayout>,
        len: u32,
    },
    Struct(Vec<PhysicalLayout>),
}

/// Concrete target layout for one closed semantic type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalLayout {
    pub size: u64,
    pub align: u32,
    pub repr: PhysicalRepr,
}

/// Target-realized storage for one exact tagged-variant type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVariantLayout {
    pub ty: ResolvedTy,
    pub is_indirect: bool,
    pub object: PhysicalLayout,
    pub variants: Vec<PhysicalLayout>,
}

/// Complete target authority used to create one physical module.
///
/// The codegen crate constructs this from the active LLVM target machine and
/// `TargetData`. MIR merely consumes the resulting concrete rows; it does not
/// carry a second target-layout calculator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalTarget {
    pub triple: String,
    pub data_layout: String,
    layouts: BTreeMap<TypeInstanceKey, PhysicalLayout>,
    variant_layouts: BTreeMap<TypeInstanceKey, PhysicalVariantLayout>,
    environment_layouts: BTreeMap<TypeInstanceKey, PhysicalLayout>,
}

impl PhysicalTarget {
    #[must_use]
    pub fn new(triple: impl Into<String>, data_layout: impl Into<String>) -> Self {
        Self {
            triple: triple.into(),
            data_layout: data_layout.into(),
            layouts: BTreeMap::new(),
            variant_layouts: BTreeMap::new(),
            environment_layouts: BTreeMap::new(),
        }
    }

    /// Register the LLVM-derived layout for one concrete type.
    pub fn insert_layout(&mut self, ty: ResolvedTy, layout: PhysicalLayout) {
        self.layouts.insert(TypeInstanceKey(ty), layout);
    }

    #[must_use]
    pub fn layout(&self, ty: &ResolvedTy) -> Option<&PhysicalLayout> {
        self.layouts.get(&TypeInstanceKey(ty.clone()))
    }

    /// Register the target-measured initialization mask and capture storage.
    pub fn insert_environment_layout(&mut self, ty: ResolvedTy, layout: PhysicalLayout) {
        self.environment_layouts.insert(TypeInstanceKey(ty), layout);
    }

    #[must_use]
    pub fn environment_layout(&self, ty: &ResolvedTy) -> Option<&PhysicalLayout> {
        self.environment_layouts.get(&TypeInstanceKey(ty.clone()))
    }

    pub fn insert_variant_layout(&mut self, layout: PhysicalVariantLayout) {
        self.variant_layouts
            .insert(TypeInstanceKey(layout.ty.clone()), layout);
    }

    #[must_use]
    pub fn variant_layout(&self, ty: &ResolvedTy) -> Option<&PhysicalVariantLayout> {
        self.variant_layouts.get(&TypeInstanceKey(ty.clone()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StorageOrigin {
    /// A field of the actor state receiver. `initialized` is false only in
    /// init for a field init owns (D447): the slot starts uninitialized,
    /// must be initialized at every return and uninitialized at every fault
    /// propagation.
    ActorState {
        state: StorageId,
        field: u32,
        initialized: bool,
    },
    Capture {
        environment: StorageId,
        field: u32,
    },
    /// Aliases the root and path in `PhysicalFunction::place_storage`.
    Aggregate(hew_sir::PlaceId),
    Parameter(ValueId),
    BlockArgument(ValueId),
    Value(ValueId),
    Local(hew_sir::PlaceId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalStorage {
    pub id: StorageId,
    pub ty: ResolvedTy,
    pub layout: PhysicalLayout,
    pub own: OwnKind,
    pub origin: StorageOrigin,
    /// Immediate SIR loan dependency, mapped to concrete storage. This is
    /// retained provenance for validation, never a physical cleanup decision.
    pub borrow_parent: Option<StorageId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalParam {
    pub ty: ResolvedTy,
    pub layout: PhysicalLayout,
    pub passing: hew_sir::SemParamPassing,
    pub carrier: ParamCarrier,
}

/// Concrete private-ABI carrier selected before LLVM emission.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamCarrier {
    Direct,
    Indirect,
}

/// The one native private ABI: `i32(args..., result_out?, fault_out)`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalCallable {
    pub id: CallableId,
    pub declaration: hew_types::DefId,
    pub instance: hew_sir::CallableInstance,
    pub symbol: String,
    pub params: Vec<PhysicalParam>,
    pub return_ty: ResolvedTy,
    pub return_layout: Option<PhysicalLayout>,
    /// The private body uses the resumable status/result/fault convention.
    /// Derived from verified suspension and call edges, then checked again
    /// against the physical CFG before emission.
    pub is_resumable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PhysicalConst {
    /// Exact destination-width two's-complement bit pattern for an integer
    /// constant, derived once here from the SIR value and the destination's
    /// realized layout (D421). Codegen emits these bits unsigned; it decides
    /// nothing about signedness or width.
    IntegerBits(u64),
    Bool(bool),
    Float(f64),
    Char(char),
    Unit,
    Duration(i64),
    String(hew_sir::StringLiteralId),
    Bytes(hew_sir::BytesLiteralId),
}

/// A clone selected once from an explicit SIR copy plus concrete type facts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CloneAction {
    Encoding(EncodingFormat),
    Callable,
    Bitwise,
    StringRetain,
    BytesRetain,
    Aggregate(PhysicalAggregateId),
    Variant(PhysicalVariantId),
    Vector(PhysicalVectorId),
    /// Fixed-array element storage with reverse-order cleanup.
    Array(PhysicalVectorId),
    Map(PhysicalMapId),
    Set(PhysicalSetId),
}

/// A release selected once from an explicit SIR destroy plus concrete type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DestroyAction {
    Resource(PhysicalResourceId),
    /// Release an erased value through its own vtable: the drop slot runs the
    /// concrete destructor, then the heap box is freed with the size and
    /// alignment the same table carries.
    TraitObject,
    Encoding(EncodingFormat),
    Callable,
    StringRelease,
    BytesRelease,
    Aggregate(PhysicalAggregateId),
    Variant(PhysicalVariantId),
    Vector(PhysicalVectorId),
    /// Fixed-array element storage with reverse-order cleanup.
    Array(PhysicalVectorId),
    Map(PhysicalMapId),
    Set(PhysicalSetId),
}

/// Shared physical copy/drop recipe for one concrete value type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalValueRecipe {
    pub ty: ResolvedTy,
    pub own: OwnKind,
    pub clone: Option<CloneAction>,
    pub destroy: Option<DestroyAction>,
}

/// One exact closure body and its already selected concrete environment type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalClosure {
    pub generator_yield: Option<ResolvedTy>,
    pub id: ClosureId,
    pub body: CallableId,
    pub ty: ResolvedTy,
}

/// The user argument/result ABI of an indirect invocation, excluding its receiver.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalCallSignature {
    pub params: Vec<PhysicalParam>,
    pub return_ty: ResolvedTy,
    pub return_layout: Option<PhysicalLayout>,
}

/// Module-local identity of one realized trait-object dispatch table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalVtableId(pub u32);

/// One dispatchable slot of a realized trait-object table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVtableSlot {
    pub slot: u32,
    pub callee: CallableId,
    /// The erased boundary: the receiver rides a pointer, the rest of the ABI
    /// is this signature.
    pub signature: PhysicalCallSignature,
}

/// Realized dispatch table for one erasure of a concrete type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVtable {
    pub id: PhysicalVtableId,
    pub dyn_ty: ResolvedTy,
    pub concrete_ty: ResolvedTy,
    pub concrete_layout: PhysicalLayout,
    /// Copy/drop recipe of the boxed concrete value, used to emit the table's
    /// drop slot.
    pub concrete: PhysicalValueRecipe,
    pub slots: Vec<PhysicalVtableSlot>,
}

/// Capture recipes shared by concrete environments of an exact closure type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalEnvironmentGlue {
    pub ty: ResolvedTy,
    pub fields: Vec<PhysicalValueRecipe>,
    pub cloneable: bool,
}

/// Shared physical glue for one exact concrete aggregate type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalAggregateGlue {
    pub id: PhysicalAggregateId,
    pub ty: ResolvedTy,
    pub own: OwnKind,
    pub fields: Vec<PhysicalValueRecipe>,
}

/// One declaration-order case in a tagged-variant copy/drop recipe.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVariantCase {
    pub fields: Vec<PhysicalValueRecipe>,
}

/// Shared physical glue for one exact concrete tagged-variant type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVariantGlue {
    pub id: PhysicalVariantId,
    pub ty: ResolvedTy,
    pub own: OwnKind,
    pub is_indirect: bool,
    pub variants: Vec<PhysicalVariantCase>,
}

/// One `select` source in arm order. The physical index a selection reports is
/// this position, so the arms and the registrations share one order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PhysicalSelectSource {
    /// A borrowed checked task handle; the winning arm awaits it.
    Task(ArgumentTransfer),
    /// A borrowed channel receiver; the winning arm performs the receive.
    ChannelRecv(ArgumentTransfer),
    /// A borrowed ephemeral completion; the winning arm consumes its result.
    ActorCall(ArgumentTransfer),
}

impl PhysicalSelectSource {
    /// The borrowed handle this source observes.
    #[must_use]
    pub const fn transfer(self) -> ArgumentTransfer {
        match self {
            Self::Task(transfer) | Self::ChannelRecv(transfer) | Self::ActorCall(transfer) => {
                transfer
            }
        }
    }
}

/// Element recipe shared by vector operations and ordinary value copy/drop.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVectorGlue {
    pub id: PhysicalVectorId,
    pub ty: ResolvedTy,
    pub element: PhysicalValueRecipe,
}

/// Key and value recipes shared by map operations and ordinary copy/drop.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalMapGlue {
    pub id: PhysicalMapId,
    pub ty: ResolvedTy,
    pub key: PhysicalValueRecipe,
    pub value: PhysicalValueRecipe,
}

/// Element recipe shared by set operations and ordinary copy/drop.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalSetGlue {
    pub id: PhysicalSetId,
    pub ty: ResolvedTy,
    pub element: PhysicalValueRecipe,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PhysicalVectorOp {
    New,
    Len,
    Contains,
    Index,
    Get {
        result: PhysicalVariantId,
    },
    Push,
    Set,
    Pop {
        result: PhysicalAggregateId,
    },
    Remove {
        result: PhysicalAggregateId,
    },
    Clear,
    /// A loan of the element the vector still owns: the slot bytes are read
    /// without a clone and the result carries no release obligation.
    IndexBorrow,
    /// `Some` carries a loan of the element the vector still owns; the result
    /// carries no release obligation.
    GetBorrow {
        result: PhysicalVariantId,
    },
    /// The consuming iterator's step: the first element moves out to the caller
    /// and the vector shrinks by one.
    TakeFirst {
        result: PhysicalAggregateId,
    },
    Slice,
    SliceFrom,
}

impl PhysicalVectorOp {
    const fn semantic_op(self) -> VecValueOp {
        match self {
            Self::New => VecValueOp::New,
            Self::Len => VecValueOp::Len,
            Self::Contains => VecValueOp::Contains,
            Self::Index => VecValueOp::Index,
            Self::Get { .. } => VecValueOp::Get,
            Self::Push => VecValueOp::Push,
            Self::Set => VecValueOp::Set,
            Self::Pop { .. } => VecValueOp::Pop,
            Self::Remove { .. } => VecValueOp::Remove,
            Self::Clear => VecValueOp::Clear,
            Self::IndexBorrow => VecValueOp::IndexBorrow,
            Self::GetBorrow { .. } => VecValueOp::GetBorrow,
            Self::TakeFirst { .. } => VecValueOp::TakeFirst,
            Self::Slice => VecValueOp::Slice,
            Self::SliceFrom => VecValueOp::SliceFrom,
        }
    }
}

/// Map operations retain exact result-shape identities selected by physical lowering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PhysicalMapOp {
    New,
    Len,
    Index,
    Get {
        result: PhysicalVariantId,
    },
    /// `Some` carries a loan of the value the map still owns; the result
    /// carries no release obligation.
    GetBorrow {
        result: PhysicalVariantId,
    },
    ContainsKey,
    Insert,
    Remove {
        result: PhysicalAggregateId,
        value: PhysicalVariantId,
    },
    Clear,
    Keys,
    Values,
    Entries {
        result: PhysicalVectorId,
    },
}

impl PhysicalMapOp {
    const fn semantic_op(self) -> MapValueOp {
        match self {
            Self::New => MapValueOp::New,
            Self::Len => MapValueOp::Len,
            Self::Index => MapValueOp::Index,
            Self::Get { .. } => MapValueOp::Get,
            Self::GetBorrow { .. } => MapValueOp::GetBorrow,
            Self::ContainsKey => MapValueOp::ContainsKey,
            Self::Insert => MapValueOp::Insert,
            Self::Remove { .. } => MapValueOp::Remove,
            Self::Clear => MapValueOp::Clear,
            Self::Keys => MapValueOp::Keys,
            Self::Values => MapValueOp::Values,
            Self::Entries { .. } => MapValueOp::Entries,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PhysicalSetOp {
    New,
    Len,
    Contains,
    Insert { result: PhysicalAggregateId },
    Remove { result: PhysicalAggregateId },
    Clear,
    Elements,
}

impl PhysicalSetOp {
    const fn semantic_op(self) -> SetValueOp {
        match self {
            Self::New => SetValueOp::New,
            Self::Len => SetValueOp::Len,
            Self::Contains => SetValueOp::Contains,
            Self::Insert { .. } => SetValueOp::Insert,
            Self::Remove { .. } => SetValueOp::Remove,
            Self::Clear => SetValueOp::Clear,
            Self::Elements => SetValueOp::Elements,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PhysicalOp {
    /// One bounded element pipe. Unconsumed elements are released through
    /// the element recipe by whichever half closes last.
    StreamPipe {
        capacity: u32,
        stream: StorageId,
        sink: StorageId,
        element: PhysicalValueRecipe,
    },
    GeneratorMake {
        closure: ClosureId,
        callable: StorageId,
        dest: StorageId,
        yielded: PhysicalValueRecipe,
        returned: PhysicalValueRecipe,
    },
    TaskScopeEnter {
        scope: hew_sir::TaskScopeId,
        parent: Option<hew_sir::TaskScopeId>,
        duration: Option<StorageId>,
    },
    TaskScopeClose {
        scope: hew_sir::TaskScopeId,
    },
    TaskSpawn {
        scope: hew_sir::TaskScopeId,
        callable: StorageId,
        dest: StorageId,
        /// Absent only for the uninhabited result of Task<!>.
        output: Option<PhysicalValueRecipe>,
    },
    /// Static scheduling marker; dependencies alias existing storage.
    RegisterDefer {
        defer: DeferId,
        scope: DeferScopeId,
        dependencies: Vec<StorageId>,
    },
    FunctionMake {
        dest: StorageId,
        callee: CallableId,
    },
    ClosureMake {
        dest: StorageId,
        closure: ClosureId,
        fields: Vec<StorageId>,
    },
    CallableCoerce {
        dest: StorageId,
        source: StorageId,
    },
    /// Box the concrete value and pair it with its table's constant.
    DynMake {
        dest: StorageId,
        vtable: PhysicalVtableId,
        source: StorageId,
    },
    Const {
        dest: StorageId,
        value: PhysicalConst,
    },
    Unary {
        dest: StorageId,
        op: UnaryOp,
        source: StorageId,
    },
    Binary {
        dest: StorageId,
        op: BinaryOp,
        lhs: StorageId,
        rhs: StorageId,
    },
    Cast {
        dest: StorageId,
        source: StorageId,
        to: ResolvedTy,
    },
    TupleMake {
        dest: StorageId,
        elements: Vec<StorageId>,
    },
    TupleGet {
        dest: StorageId,
        tuple: StorageId,
        index: u32,
    },
    AggregateMake {
        dest: StorageId,
        fields: Vec<StorageId>,
        glue: PhysicalAggregateId,
    },
    /// Heap-backed fixed array construction with one shared element recipe.
    ArrayMake {
        dest: StorageId,
        fields: Vec<StorageId>,
        glue: PhysicalVectorId,
    },
    ArrayRepeat {
        dest: StorageId,
        seed: StorageId,
        glue: PhysicalVectorId,
    },
    AggregateProjectCopy {
        dest: StorageId,
        aggregate: StorageId,
        field: u32,
        glue: PhysicalAggregateId,
        action: CloneAction,
    },
    AggregateProjectBorrow {
        dest: StorageId,
        aggregate: StorageId,
        field: u32,
        glue: PhysicalAggregateId,
    },
    AggregateDestructure {
        aggregate: StorageId,
        fields: Vec<StorageId>,
        glue: PhysicalAggregateId,
    },
    VariantMake {
        dest: StorageId,
        variant: u32,
        fields: Vec<StorageId>,
        glue: PhysicalVariantId,
    },
    VariantIs {
        dest: StorageId,
        source: StorageId,
        variant: u32,
        glue: PhysicalVariantId,
    },
    VariantProjectCopy {
        dest: StorageId,
        source: StorageId,
        variant: u32,
        field: u32,
        glue: PhysicalVariantId,
        action: CloneAction,
    },
    VariantProjectBorrow {
        dest: StorageId,
        source: StorageId,
        variant: u32,
        field: u32,
        glue: PhysicalVariantId,
    },
    VariantDestructure {
        source: StorageId,
        variant: u32,
        fields: Vec<StorageId>,
        glue: PhysicalVariantId,
    },
    Transfer {
        dest: StorageId,
        source: StorageId,
    },
    Clone {
        dest: StorageId,
        source: StorageId,
        action: CloneAction,
    },
    Destroy {
        source: StorageId,
        action: DestroyAction,
        cleanup: PhysicalCleanup,
    },
    Borrow {
        dest: StorageId,
        source: StorageId,
    },
    EndBorrow {
        source: StorageId,
    },
    StorageLive {
        storage: StorageId,
    },
    Assign {
        dest: StorageId,
        source: StorageId,
        destroy_old: Option<DestroyAction>,
    },
    StorageDead {
        storage: StorageId,
        destroy: Option<DestroyAction>,
        cleanup: PhysicalCleanup,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalEdge {
    pub target: BlockId,
    /// Parallel moves from predecessor storage into target block-argument
    /// storage. No clone is implicit in an edge transfer.
    pub transfers: Vec<(StorageId, StorageId)>,
    /// Initialization identities transferred in parallel with the payloads.
    pub leaf_transfers: Vec<(StorageId, StorageId)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgumentTransfer {
    Borrow(StorageId),
    BorrowMut(StorageId),
    Move(StorageId),
    Clone {
        source: StorageId,
        action: CloneAction,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnTransfer {
    Borrow(StorageId),
    Move(StorageId),
    Clone {
        source: StorageId,
        action: CloneAction,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalCheckedFailure {
    pub kind: TrapKind,
    pub edge: PhysicalEdge,
}

/// One exact successor of a consuming tagged-variant switch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalVariantArm {
    pub variant: u32,
    pub fields: Vec<StorageId>,
    pub target: PhysicalEdge,
}

/// Exact no-unwind runtime ABI operation selected from a verified SIR runtime
/// family. The emitter executes this closed physical action; it never selects
/// ownership or failure behaviour from a linker symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PhysicalRuntimeAction {
    FileRead(hew_types::runtime_call::FileReadOp),
    Tcp(hew_types::runtime_call::TcpOp),
    StreamClose,
    SinkClose,
    /// The channel substrate's non-suspending entries: allocating and
    /// splitting a pair, and closing either half.
    ChannelSenderClone,
    ChannelSenderClose,
    ChannelReceiverClose,
    ChannelPairNew,
    ChannelPairFree,
    ActorRequestRelease,
    ActorCallFree,
    ActorRequestTake,
    ChannelPairIsValid,
    ChannelPairSender,
    ChannelPairReceiver,
    Encoding {
        format: EncodingFormat,
        op: EncodingOp,
    },
    StringConcat,
    StringEquals,
    StringCompare,
    StringStartsWith,
    StringEndsWith,
    StringContains,
    StringFind {
        result: PhysicalVariantId,
    },
    BytesGet {
        result: PhysicalVariantId,
    },
    /// `bytes.pop()` - shrinks the receiver and yields `(bytes, Option<u8>)`.
    /// `result` names the pair, `option` its optional byte half.
    BytesPop {
        result: PhysicalAggregateId,
        option: PhysicalVariantId,
    },
    StringCharAt {
        result: PhysicalVariantId,
    },
    StringIsEmpty,
    StringIsDigit,
    StringIsAlpha,
    StringIsAlphanumeric,
    StringToBytesOwned,
    StringToUppercase,
    StringToLowercase,
    StringSlice,
    StringRepeat,
    MathIntrinsic(MathIntrinsic),
    StringReplace,
    StringClone,
    StringSplit,
    StringLines,
    StringChars,
    StringIndex,
    StringSliceCodepoints,
    StringSliceCodepointsFrom,
    StringTrim,
    StringLen,
    StringByteLen,
    /// One `duration`/`instant` scalar accessor. Both receivers are i64-backed,
    /// so every one of these is the same physical shape - i64 arguments in, one
    /// scalar out, no ownership - and the family is the only thing that varies.
    /// `physical_runtime_action` is the sole constructor and admits only the
    /// eleven time families.
    TimeScalar(RuntimeCallFamily),
    BytesDecodeUtf8 {
        result: PhysicalVariantId,
        error: PhysicalAggregateId,
        error_len: PhysicalVariantId,
    },
    BytesDecodeUtf8Lossy,
    U8ToString,
    I32ToString,
    I64ToString,
    U32ToString,
    U64ToString,
    F64ToString,
    CharToString,
    BoolToString,
    Print {
        kind: hew_types::runtime_call::PrintKind,
        newline: bool,
    },
    ProcessExit,
    StderrWrite,
    BytesNew,
    BytesLen,
    BytesIsEmpty,
    BytesClear,
    BytesContains,
    BytesSet,
    BytesIndex,
    BytesSlice,
    BytesSliceFrom,
    BytesPushOwned,
    BytesAppendOwned,
    Array {
        operation: ArrayValueOp,
        glue: PhysicalVectorId,
    },
    Vector {
        operation: PhysicalVectorOp,
        glue: PhysicalVectorId,
    },
    Map {
        operation: PhysicalMapOp,
        glue: PhysicalMapId,
    },
    Set {
        operation: PhysicalSetOp,
        glue: PhysicalSetId,
    },
    /// Source-visible node lifecycle operation returning Result<(), `NodeError`>.
    NodeShutdown,
    /// Register a local actor handle under a source-visible name.
    NodeRegister,
    /// Resolve a registered actor name into its full carried remote location.
    NodeLookup {
        result: PhysicalVariantId,
        error: PhysicalVariantId,
    },
    NodeLifecycle {
        family: RuntimeCallFamily,
        result: PhysicalVariantId,
        error: PhysicalVariantId,
    },
}

impl PhysicalRuntimeAction {
    const fn semantic_family(self) -> RuntimeCallFamily {
        match self {
            Self::FileRead(op) => RuntimeCallFamily::FileRead(op),
            Self::Tcp(op) => RuntimeCallFamily::Tcp(op),
            Self::StreamClose => RuntimeCallFamily::StreamClose,
            Self::SinkClose => RuntimeCallFamily::SinkClose,
            Self::ChannelSenderClone => RuntimeCallFamily::ChannelSenderClone,
            Self::ChannelSenderClose => RuntimeCallFamily::ChannelSenderClose,
            Self::ChannelReceiverClose => RuntimeCallFamily::ChannelReceiverClose,
            Self::ChannelPairNew => RuntimeCallFamily::ChannelPairNew,
            Self::ChannelPairFree => RuntimeCallFamily::ChannelPairFree,
            Self::ActorRequestRelease => RuntimeCallFamily::ActorRequestRelease,
            Self::ActorCallFree => RuntimeCallFamily::ActorCallFree,
            Self::ActorRequestTake => RuntimeCallFamily::ActorRequestTake,
            Self::ChannelPairIsValid => RuntimeCallFamily::ChannelPairIsValid,
            Self::ChannelPairSender => RuntimeCallFamily::ChannelPairSender,
            Self::ChannelPairReceiver => RuntimeCallFamily::ChannelPairReceiver,
            Self::Encoding { format, op } => RuntimeCallFamily::Encoding { format, op },
            Self::StringConcat => RuntimeCallFamily::StringConcat,
            Self::StringEquals => RuntimeCallFamily::StringEquals,
            Self::StringCompare => RuntimeCallFamily::StringCompare,
            Self::StringStartsWith => RuntimeCallFamily::StringStartsWith,
            Self::StringEndsWith => RuntimeCallFamily::StringEndsWith,
            Self::StringContains => RuntimeCallFamily::StringContains,
            Self::StringFind { .. } => RuntimeCallFamily::StringFind,
            Self::BytesGet { .. } => RuntimeCallFamily::BytesGet,
            Self::StringCharAt { .. } => RuntimeCallFamily::StringCharAt,
            Self::StringIsEmpty => RuntimeCallFamily::StringIsEmpty,
            Self::StringIsDigit => RuntimeCallFamily::StringIsDigit,
            Self::StringIsAlpha => RuntimeCallFamily::StringIsAlpha,
            Self::StringIsAlphanumeric => RuntimeCallFamily::StringIsAlphanumeric,
            Self::StringToBytesOwned => RuntimeCallFamily::StringToBytes,
            Self::StringToUppercase => RuntimeCallFamily::StringToUppercase,
            Self::StringToLowercase => RuntimeCallFamily::StringToLowercase,
            Self::StringSlice => RuntimeCallFamily::StringSlice,
            Self::StringRepeat => RuntimeCallFamily::StringRepeat,
            Self::MathIntrinsic(kind) => RuntimeCallFamily::MathIntrinsic(kind),
            Self::StringReplace => RuntimeCallFamily::StringReplace,
            Self::StringClone => RuntimeCallFamily::StringClone,
            Self::StringSplit => RuntimeCallFamily::StringSplit,
            Self::StringLines => RuntimeCallFamily::StringLines,
            Self::StringChars => RuntimeCallFamily::StringChars,
            Self::StringIndex => RuntimeCallFamily::StringIndex,
            Self::StringSliceCodepoints => RuntimeCallFamily::StringSliceCodepoints,
            Self::StringSliceCodepointsFrom => RuntimeCallFamily::StringSliceCodepointsFrom,
            Self::StringTrim => RuntimeCallFamily::StringTrim,
            Self::StringLen => RuntimeCallFamily::StringLen,
            Self::StringByteLen => RuntimeCallFamily::StringByteLen,
            Self::TimeScalar(family) | Self::NodeLifecycle { family, .. } => family,
            Self::NodeShutdown => RuntimeCallFamily::NodeShutdown,
            Self::NodeRegister => RuntimeCallFamily::NodeRegister,
            Self::NodeLookup { .. } => RuntimeCallFamily::NodeLookup,
            Self::BytesDecodeUtf8 { .. } => RuntimeCallFamily::BytesDecodeUtf8,
            Self::BytesDecodeUtf8Lossy => RuntimeCallFamily::BytesDecodeUtf8Lossy,
            Self::U8ToString => RuntimeCallFamily::U8ToString,
            Self::I32ToString => RuntimeCallFamily::I32ToString,
            Self::I64ToString => RuntimeCallFamily::I64ToString,
            Self::U32ToString => RuntimeCallFamily::U32ToString,
            Self::U64ToString => RuntimeCallFamily::U64ToString,
            Self::F64ToString => RuntimeCallFamily::F64ToString,
            Self::CharToString => RuntimeCallFamily::CharToString,
            Self::BoolToString => RuntimeCallFamily::BoolToString,
            Self::Print { kind, newline } => RuntimeCallFamily::Print { kind, newline },
            Self::ProcessExit => RuntimeCallFamily::ProcessExit,
            Self::StderrWrite => RuntimeCallFamily::StderrWrite,
            Self::BytesNew => RuntimeCallFamily::BytesNew,
            Self::BytesLen => RuntimeCallFamily::BytesLen,
            Self::BytesIsEmpty => RuntimeCallFamily::BytesIsEmpty,
            Self::BytesClear => RuntimeCallFamily::BytesClear,
            Self::BytesPop { .. } => RuntimeCallFamily::BytesPop,
            Self::BytesContains => RuntimeCallFamily::BytesContains,
            Self::BytesSet => RuntimeCallFamily::BytesSet,
            Self::BytesIndex => RuntimeCallFamily::BytesIndex,
            Self::BytesSlice => RuntimeCallFamily::BytesSlice,
            Self::BytesSliceFrom => RuntimeCallFamily::BytesSliceFrom,
            Self::BytesPushOwned => RuntimeCallFamily::BytesPush,
            Self::BytesAppendOwned => RuntimeCallFamily::BytesAppend,
            Self::Array { operation, .. } => RuntimeCallFamily::Array(operation),
            Self::Vector { operation, .. } => RuntimeCallFamily::Vector(operation.semantic_op()),
            Self::Map { operation, .. } => RuntimeCallFamily::Map(operation.semantic_op()),
            Self::Set { operation, .. } => RuntimeCallFamily::Set(operation.semantic_op()),
        }
    }
}

/// Physical Result storage with its SIR-selected success and error cases.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhysicalWireTextResult {
    pub glue: PhysicalVariantId,
    pub ok: u32,
    pub error: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PhysicalTerminator {
    /// Park until the exclusively borrowed stream yields an element or ends.
    StreamNext {
        stream: ArgumentTransfer,
        element: PhysicalValueRecipe,
        result: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    /// Take the next element from the exclusively borrowed channel. `park`
    /// waits for an element or for every sender to close; without it an empty
    /// channel produces `None` at once. The element carries its typed
    /// envelope recipe.
    ChannelRecv {
        park: bool,
        channel: ArgumentTransfer,
        element: PhysicalValueRecipe,
        result: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    /// Deep-copy one element into the borrowed channel, parking on capacity.
    /// The producer keeps its value on every exit.
    ChannelSend {
        channel: ArgumentTransfer,
        value: ArgumentTransfer,
        element: PhysicalValueRecipe,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    /// Transfer one element into the borrowed sink, parking on capacity. The
    /// element is consumed on every exit; `closed` resumes after the consumer
    /// closed its half.
    StreamSend {
        sink: ArgumentTransfer,
        value: ArgumentTransfer,
        element: PhysicalValueRecipe,
        normal: PhysicalEdge,
        closed: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    ActorAsk {
        actor: ActorId,
        message: u32,
        /// Admission behaviour when the destination mailbox is full: `Wait`
        /// parks the caller, `Reject` refuses the call.
        policy: hew_types::actor_delivery::SendPolicy,
        deadline_ns: Option<i64>,
        sealed: bool,
        args: Vec<ArgumentTransfer>,
        result: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    TaskSelect {
        order: hew_sir::TaskSelectionOrder,
        sources: Vec<PhysicalSelectSource>,
        timeout: Option<StorageId>,
        result: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    GeneratorYield {
        value: ArgumentTransfer,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    GeneratorNext {
        generator: ArgumentTransfer,
        result: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    ValueClose {
        index: Option<StorageId>,
        destroy: Option<DestroyAction>,
        generator: StorageId,
        conditional: bool,
        next: PhysicalEdge,
    },
    TaskAwait {
        task: ArgumentTransfer,
        result: Option<StorageId>,
        normal: Option<PhysicalEdge>,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    TaskScopeJoin {
        scope: hew_sir::TaskScopeId,
        mode: hew_sir::TaskScopeJoinMode,
        normal: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    /// Submit once, suspend without blocking, and drain resource loans before exit.
    NativeIo {
        operation: hew_types::runtime_call::AsyncIoOp,
        args: Vec<ArgumentTransfer>,
        result: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    Sleep {
        duration: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    /// Suspend until a monotonic deadline. The operand is an `instant`, which
    /// is `i64` at this boundary; the runtime measures the remaining wait when
    /// it arms the timer.
    SleepUntil {
        deadline: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    ActorCall {
        operation: hew_sir::ActorOperation,
        args: Vec<ArgumentTransfer>,
        result: Option<StorageId>,
        normal: PhysicalEdge,
        unwind: Option<PhysicalEdge>,
    },
    EnterDefer {
        defer: DeferId,
        park: FaultParkId,
        body: PhysicalEdge,
    },
    FinishDefer {
        defer: DeferId,
        park: FaultParkId,
        next: PhysicalEdge,
    },
    CleanupDispatch {
        normal: PhysicalEdge,
        fault: PhysicalEdge,
    },
    RecoverFault {
        result: StorageId,
        glue: PhysicalVariantId,
        deadline_variant: u32,
        fault_variant: u32,
        normal: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    CheckedRaiseFault {
        kind: TrapKind,
        cleanup: PhysicalEdge,
    },
    IndirectCall {
        callee: ArgumentTransfer,
        signature: PhysicalCallSignature,
        args: Vec<ArgumentTransfer>,
        result: Option<StorageId>,
        normal: Option<PhysicalEdge>,
        unwind: Option<PhysicalEdge>,
    },
    /// Load one slot from the receiver's vtable and call through it.
    DynCall {
        receiver: ArgumentTransfer,
        slot: u32,
        signature: PhysicalCallSignature,
        args: Vec<ArgumentTransfer>,
        result: Option<StorageId>,
        normal: Option<PhysicalEdge>,
        unwind: Option<PhysicalEdge>,
    },
    Return {
        value: Option<ReturnTransfer>,
    },
    Goto(PhysicalEdge),
    Branch {
        condition: StorageId,
        then_target: PhysicalEdge,
        else_target: PhysicalEdge,
    },
    SwitchVariant {
        scrutinee: StorageId,
        glue: PhysicalVariantId,
        arms: Vec<PhysicalVariantArm>,
    },
    CheckedBinary {
        op: BinaryOp,
        lhs: StorageId,
        rhs: StorageId,
        result: StorageId,
        normal: PhysicalEdge,
        failures: Vec<PhysicalCheckedFailure>,
    },
    /// Calls use the module's fixed status/result/fault ABI. A non-zero status
    /// writes a non-null owned fault and enters `unwind`; a zero status enters
    /// `normal`, where `result` (when present) is initialized. Never-returning
    /// calls have neither result storage nor a normal edge; success is invalid.
    Call {
        callee: CallableId,
        args: Vec<ArgumentTransfer>,
        result: Option<StorageId>,
        normal: Option<PhysicalEdge>,
        unwind: Option<PhysicalEdge>,
    },
    /// Execute the exact selected value callback with borrowed slots. Success
    /// initializes the scalar result; failure owns a fault on the cleanup edge.
    WireCodec {
        direction: hew_types::WireCodecDirection,
        plan: std::sync::Arc<hew_sir::SemWirePlan>,
        recipes: BTreeMap<ResolvedTy, PhysicalValueRecipe>,
        text_result: Option<PhysicalWireTextResult>,
        input: ArgumentTransfer,
        result: StorageId,
        normal: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    ValueCall {
        ty: ResolvedTy,
        capability: ValueCapability,
        args: Vec<ArgumentTransfer>,
        result: StorageId,
        normal: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    /// A direct call to a declared C-ABI symbol through its declared
    /// signature. Argument transfers carry the ownership the `extern`
    /// declaration pinned; a C call has no fault ABI and no unwind edge.
    ExternCall {
        symbol: String,
        args: Vec<ArgumentTransfer>,
        result: Option<StorageId>,
        result_abi: PhysicalExternResultAbi,
        normal: PhysicalEdge,
    },
    /// A closed no-unwind runtime operation. Logical failures are explicit
    /// SIR-authored CFG edges and never become C unwinds.
    RuntimeCall {
        action: PhysicalRuntimeAction,
        args: Vec<ArgumentTransfer>,
        result: Option<StorageId>,
        normal: PhysicalEdge,
        failure: Option<PhysicalEdge>,
    },
    /// Copy the borrowed message into the active fault, then enter cleanup.
    Panic {
        message: ArgumentTransfer,
        cleanup: PhysicalEdge,
    },
    Trap(TrapKind),
    /// Propagate the currently owned fault and non-zero status through this
    /// function's private `fault_out` and status result.
    PropagateFault,
    Unreachable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PhysicalBlock {
    pub id: BlockId,
    pub arguments: Vec<StorageId>,
    pub ops: Vec<PhysicalOp>,
    pub terminator: PhysicalTerminator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PhysicalFunction {
    pub callable: CallableId,
    pub entry: BlockId,
    pub parameters: Vec<StorageId>,
    pub storage: Vec<PhysicalStorage>,
    /// Verified root partitions and their target-realized projection paths.
    pub place_storage: BTreeMap<StorageId, PhysicalPlaceStorage>,
    pub blocks: Vec<PhysicalBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PhysicalModule {
    pub actors: Vec<hew_sir::SemActor>,
    pub supervisors: Vec<hew_sir::SemSupervisor>,
    pub actor_recipes: BTreeMap<ResolvedTy, PhysicalValueRecipe>,
    pub resources: Vec<PhysicalResourceDescriptor>,
    pub value_capabilities:
        BTreeMap<(ResolvedTy, hew_types::ValueCapability), PhysicalValueCapability>,
    pub target: PhysicalTarget,
    pub closures: Vec<PhysicalClosure>,
    pub vtables: Vec<PhysicalVtable>,
    pub environment_glue: Vec<PhysicalEnvironmentGlue>,
    pub aggregate_glue: Vec<PhysicalAggregateGlue>,
    pub variant_glue: Vec<PhysicalVariantGlue>,
    pub vector_glue: Vec<PhysicalVectorGlue>,
    pub map_glue: Vec<PhysicalMapGlue>,
    pub set_glue: Vec<PhysicalSetGlue>,
    /// Retained semantic authority for verification, never a physical classifier.
    type_facts: BTreeMap<TypeInstanceKey, hew_types::TypeFacts>,
    pub callables: Vec<PhysicalCallable>,
    pub functions: Vec<PhysicalFunction>,
    pub entry_callable: Option<CallableId>,
    pub entry_exit_plan: Option<EntryExitPlan>,
    pub string_literals: BTreeMap<hew_sir::StringLiteralId, String>,
    pub bytes_literals: BTreeMap<hew_sir::BytesLiteralId, Vec<u8>>,
}

/// Immutable evidence that physical MIR passed its structural verifier.
#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedPhysicalModule(PhysicalModule);

impl VerifiedPhysicalModule {
    #[must_use]
    pub const fn module(&self) -> &PhysicalModule {
        &self.0
    }

    /// Explicitly discard verification before applying a transform.
    #[must_use]
    pub fn into_unverified(self) -> PhysicalModule {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalError {
    pub message: String,
}

impl PhysicalError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for PhysicalError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for PhysicalError {}

/// Lower verified ownership SIR into the sole target-realized MIR.
///
/// This boundary re-runs SIR verification, resolves every type layout and
/// explicit ownership action, then verifies the resulting storage/CFG model.
/// Code generation accepts only the returned immutable wrapper.
///
/// # Errors
///
/// Returns [`PhysicalError`] when SIR verification fails, a concrete target
/// layout is absent, an ownership action has no admitted physical realization,
/// or the resulting storage/CFG model violates the physical verifier.
#[expect(
    clippy::too_many_lines,
    reason = "materializes the complete checked module and its callable ABI"
)]
pub fn lower_physical_module(
    module: &SemModule,
    target: PhysicalTarget,
) -> Result<VerifiedPhysicalModule, PhysicalError> {
    let checked = hew_sir::check_module(module).map_err(|diagnostics| {
        PhysicalError::new(format!(
            "SIR verification failed before physical lowering: {:?}",
            diagnostics[0].kind
        ))
    })?;

    let PhysicalGlue {
        resources,
        environment_glue,
        aggregate_glue,
        variant_glue,
        vector_glue,
        map_glue,
        set_glue,
        ids,
    } = build_glue(module)?;

    let resumable = suspend::semantic_callables(&checked);
    let callables = module
        .callables
        .iter()
        .map(|callable| {
            let params = callable
                .signature
                .params
                .iter()
                .map(|param| {
                    Ok(PhysicalParam {
                        ty: param.ty.clone(),
                        layout: required_layout(&target, &param.ty)?.clone(),
                        passing: param.passing,
                        carrier: param_carrier(param.passing, required_layout(&target, &param.ty)?),
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            let return_layout = if callable.signature.return_ty == ResolvedTy::Never
                || (callable.signature.return_ty == ResolvedTy::Unit
                    && !module.closures.iter().any(|closure| {
                        closure.body == callable.id && closure.generator_yield.is_some()
                    })) {
                None
            } else {
                Some(required_layout(&target, &callable.signature.return_ty)?.clone())
            };
            Ok(PhysicalCallable {
                id: callable.id,
                declaration: callable.declaration.clone(),
                instance: callable.instance.clone(),
                symbol: callable.symbol.clone(),
                params,
                return_ty: callable.signature.return_ty.clone(),
                return_layout,
                is_resumable: resumable.contains(&callable.id),
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;

    let functions = module
        .functions
        .iter()
        .map(|function| {
            let certificate = checked
                .function(function.callable)
                .ok_or_else(|| PhysicalError::new("physical function lacks its SIR certificate"))?;
            lower_function(module, &target, function, &ids, certificate)
        })
        .collect::<Result<Vec<_>, _>>()?;

    let vtables = module
        .vtables
        .iter()
        .map(|vtable| {
            let slots = vtable
                .slots
                .iter()
                .map(|slot| {
                    let params = slot
                        .signature
                        .params
                        .iter()
                        .map(|param| {
                            let layout = required_layout(&target, &param.ty)?.clone();
                            Ok(PhysicalParam {
                                ty: param.ty.clone(),
                                carrier: param_carrier(param.passing, &layout),
                                passing: param.passing,
                                layout,
                            })
                        })
                        .collect::<Result<Vec<_>, PhysicalError>>()?;
                    let return_layout = if slot.signature.return_ty == ResolvedTy::Never
                        || slot.signature.return_ty == ResolvedTy::Unit
                    {
                        None
                    } else {
                        Some(required_layout(&target, &slot.signature.return_ty)?.clone())
                    };
                    Ok(PhysicalVtableSlot {
                        slot: slot.slot,
                        callee: slot.callee,
                        signature: PhysicalCallSignature {
                            params,
                            return_ty: slot.signature.return_ty.clone(),
                            return_layout,
                        },
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            Ok(PhysicalVtable {
                id: PhysicalVtableId(vtable.id.0),
                dyn_ty: vtable.dyn_ty.clone(),
                concrete_ty: vtable.concrete_ty.clone(),
                concrete_layout: required_layout(&target, &vtable.concrete_ty)?.clone(),
                concrete: physical_value_recipe(module, &ids, &vtable.concrete_ty)?,
                slots,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;

    let physical = PhysicalModule {
        actor_recipes: actor_value_recipes(module, &ids)?,
        actors: module.actors.clone(),
        supervisors: module.supervisors.clone(),
        resources,
        value_capabilities: capability::build(module, &ids)?,
        closures: module
            .closures
            .iter()
            .map(|closure| PhysicalClosure {
                generator_yield: closure.generator_yield.clone(),
                id: closure.id,
                body: closure.body,
                ty: closure.ty.clone(),
            })
            .collect(),
        vtables,
        environment_glue,
        target,
        aggregate_glue,
        variant_glue,
        vector_glue,
        map_glue,
        set_glue,
        type_facts: module.type_facts.clone(),
        callables,
        functions,
        entry_callable: module.entry_callable,
        entry_exit_plan: module.entry_exit_plan.clone(),
        string_literals: module.string_literals.clone(),
        bytes_literals: module.bytes_literals.clone(),
    };
    verify_physical_module(&physical)?;
    Ok(VerifiedPhysicalModule(physical))
}

/// One index for resolving type-directed actions to their concrete recipes.
struct PhysicalGlueIds {
    resources: BTreeMap<ResolvedTy, PhysicalResourceId>,
    aggregates: BTreeMap<ResolvedTy, PhysicalAggregateId>,
    variants: BTreeMap<ResolvedTy, PhysicalVariantId>,
    vectors: BTreeMap<ResolvedTy, PhysicalVectorId>,
    maps: BTreeMap<ResolvedTy, PhysicalMapId>,
    sets: BTreeMap<ResolvedTy, PhysicalSetId>,
}

struct PhysicalGlue {
    resources: Vec<PhysicalResourceDescriptor>,
    environment_glue: Vec<PhysicalEnvironmentGlue>,
    aggregate_glue: Vec<PhysicalAggregateGlue>,
    variant_glue: Vec<PhysicalVariantGlue>,
    vector_glue: Vec<PhysicalVectorGlue>,
    map_glue: Vec<PhysicalMapGlue>,
    set_glue: Vec<PhysicalSetGlue>,
    ids: PhysicalGlueIds,
}

#[allow(
    clippy::too_many_lines,
    reason = "value recipes are resolved together so recursive actions share one identity index"
)]
fn build_glue(module: &SemModule) -> Result<PhysicalGlue, PhysicalError> {
    let inventory = physical_type_inventory(module);
    let aggregates = inventory
        .aggregates()
        .map(|aggregate| {
            OwnKind::of_ty(&aggregate.ty, &module.type_facts)
                .map(|own| (aggregate, own))
                .map_err(PhysicalError::new)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let variants = inventory
        .variants()
        .map(|variant| {
            OwnKind::of_ty(&variant.ty, &module.type_facts)
                .map(|own| (variant, own))
                .map_err(PhysicalError::new)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let aggregate_ids = aggregates
        .iter()
        .enumerate()
        .map(|(index, (aggregate, _))| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical aggregate count exceeds u32"))?;
            Ok((aggregate.ty.clone(), PhysicalAggregateId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let variant_ids = variants
        .iter()
        .enumerate()
        .map(|(index, (variant, _))| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical variant count exceeds u32"))?;
            Ok((variant.ty.clone(), PhysicalVariantId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let vector_ids = inventory
        .vectors()
        .enumerate()
        .map(|(index, vector)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical vector count exceeds u32"))?;
            Ok((vector.ty.clone(), PhysicalVectorId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let map_ids = inventory
        .maps()
        .enumerate()
        .map(|(index, map)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical map count exceeds u32"))?;
            Ok((map.ty.clone(), PhysicalMapId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let set_ids = inventory
        .sets()
        .enumerate()
        .map(|(index, set)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical set count exceeds u32"))?;
            Ok((set.ty.clone(), PhysicalSetId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let resources = inventory
        .resources()
        .enumerate()
        .map(|(index, resource)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical resource count exceeds u32"))?;
            Ok((resource.ty.clone(), PhysicalResourceId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let ids = PhysicalGlueIds {
        resources,
        aggregates: aggregate_ids,
        variants: variant_ids,
        vectors: vector_ids,
        maps: map_ids,
        sets: set_ids,
    };
    let value_recipe = |ty: &ResolvedTy| physical_value_recipe(module, &ids, ty);
    let aggregate_glue = aggregates
        .into_iter()
        .map(|(aggregate, own)| {
            let id = ids.aggregates[&aggregate.ty];
            let shape = aggregate_shape_ref(module, &aggregate.ty)?;
            let recipes = hew_sir::aggregate_field_recipes(
                shape,
                &aggregate.ty,
                &module.aggregate_shapes,
                &module.type_facts,
            )
            .map_err(PhysicalError::new)?;
            let fields = recipes
                .iter()
                .map(|recipe| value_recipe(&recipe.ty))
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            Ok(PhysicalAggregateGlue {
                id,
                ty: aggregate.ty.clone(),
                own,
                fields,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let variant_glue = variants
        .into_iter()
        .map(|(descriptor, own)| {
            let shape = module
                .variant_shape_for_type(&descriptor.ty)
                .ok_or_else(|| {
                    PhysicalError::new(format!(
                        "variant `{}` has no exact SIR descriptor",
                        descriptor.ty.user_facing()
                    ))
                })?;
            let variants = shape
                .variants
                .iter()
                .enumerate()
                .map(|(index, _)| {
                    let index = u32::try_from(index)
                        .map_err(|_| PhysicalError::new("variant index exceeds u32"))?;
                    let recipes = hew_sir::variant_field_recipes(
                        shape.id,
                        index,
                        &descriptor.ty,
                        &module.variant_shapes,
                        &module.type_facts,
                    )
                    .map_err(PhysicalError::new)?;
                    let fields = recipes
                        .iter()
                        .map(|recipe| value_recipe(&recipe.ty))
                        .collect::<Result<Vec<_>, PhysicalError>>()?;
                    Ok(PhysicalVariantCase { fields })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            Ok(PhysicalVariantGlue {
                id: ids.variants[&descriptor.ty],
                ty: descriptor.ty.clone(),
                own,
                is_indirect: descriptor.is_indirect,
                variants,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let vector_glue = inventory
        .vectors()
        .map(|descriptor| {
            let element = value_recipe(&descriptor.element)?;
            if element.own == OwnKind::Owned && element.destroy.is_none() {
                return Err(PhysicalError::new(format!(
                    "vector element `{}` lacks a complete value recipe",
                    descriptor.element.user_facing()
                )));
            }
            Ok(PhysicalVectorGlue {
                id: ids.vectors[&descriptor.ty],
                ty: descriptor.ty.clone(),
                element,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let map_glue = inventory
        .maps()
        .map(|descriptor| {
            Ok(PhysicalMapGlue {
                id: ids.maps[&descriptor.ty],
                ty: descriptor.ty.clone(),
                key: value_recipe(&descriptor.key)?,
                value: value_recipe(&descriptor.value)?,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let set_glue = inventory
        .sets()
        .map(|descriptor| {
            Ok(PhysicalSetGlue {
                id: ids.sets[&descriptor.ty],
                ty: descriptor.ty.clone(),
                element: value_recipe(&descriptor.element)?,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let environment_glue = inventory
        .types()
        .filter_map(|ty| {
            let captures = match ty {
                ResolvedTy::Closure { captures, .. } => captures.as_slice(),
                ResolvedTy::Function { .. } => &[],
                _ => return None,
            };
            Some((|| {
                let facts = module
                    .type_facts
                    .get(&TypeInstanceKey(ty.clone()))
                    .ok_or_else(|| {
                        PhysicalError::new("callable environment lacks concrete type facts")
                    })?;
                if !matches!(
                    facts.clone,
                    CloneKind::None | CloneKind::DeepCopy | CloneKind::FieldWise
                ) {
                    return Err(PhysicalError::new(
                        "callable environment requires independent-copy type facts",
                    ));
                }
                Ok(PhysicalEnvironmentGlue {
                    ty: ty.clone(),
                    fields: captures
                        .iter()
                        .map(value_recipe)
                        .collect::<Result<Vec<_>, PhysicalError>>()?,
                    cloneable: facts.clone != CloneKind::None,
                })
            })())
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    Ok(PhysicalGlue {
        resources: inventory.resources().cloned().collect(),
        environment_glue,
        aggregate_glue,
        variant_glue,
        vector_glue,
        map_glue,
        set_glue,
        ids,
    })
}

fn aggregate_shape_ref(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<AggregateShapeRef, PhysicalError> {
    match ty {
        ResolvedTy::Tuple(_) => Ok(AggregateShapeRef::Tuple),
        _ => module
            .aggregate_shape_for_type(ty)
            .map(|shape| AggregateShapeRef::Record(shape.id))
            .ok_or_else(|| {
                PhysicalError::new(format!(
                    "aggregate `{}` has no exact SIR shape descriptor",
                    ty.user_facing()
                ))
            }),
    }
}

fn clone_action_for_type(
    ty: &ResolvedTy,
    clone: CloneKind,
    ids: &PhysicalGlueIds,
) -> Result<Option<CloneAction>, PhysicalError> {
    let action = match clone {
        CloneKind::None => return Ok(None),
        CloneKind::Bits => CloneAction::Bitwise,
        CloneKind::Retain if ty == &ResolvedTy::String => CloneAction::StringRetain,
        CloneKind::Retain if ty == &ResolvedTy::Bytes => CloneAction::BytesRetain,
        CloneKind::DeepCopy if encoding_format(ty).is_some() => {
            CloneAction::Encoding(encoding_format(ty).expect("checked encoding receiver"))
        }
        CloneKind::DeepCopy | CloneKind::FieldWise
            if matches!(ty, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }) =>
        {
            CloneAction::Callable
        }
        CloneKind::DeepCopy | CloneKind::FieldWise if ids.vectors.contains_key(ty) => {
            if matches!(ty, ResolvedTy::Array(_, _)) {
                CloneAction::Array(ids.vectors[ty])
            } else {
                CloneAction::Vector(ids.vectors[ty])
            }
        }
        CloneKind::DeepCopy | CloneKind::FieldWise if ids.maps.contains_key(ty) => {
            CloneAction::Map(ids.maps[ty])
        }
        CloneKind::DeepCopy | CloneKind::FieldWise if ids.sets.contains_key(ty) => {
            CloneAction::Set(ids.sets[ty])
        }
        CloneKind::FieldWise if ids.aggregates.contains_key(ty) => {
            CloneAction::Aggregate(ids.aggregates[ty])
        }
        CloneKind::FieldWise if ids.variants.contains_key(ty) => {
            CloneAction::Variant(ids.variants[ty])
        }
        CloneKind::FieldWise => {
            return Err(PhysicalError::new(format!(
                "field-wise value `{}` has no demanded physical descriptor",
                ty.user_facing()
            )));
        }
        unsupported => {
            return Err(PhysicalError::new(format!(
                "physical clone action for `{}` and {unsupported:?} is not implemented",
                ty.user_facing()
            )));
        }
    };
    Ok(Some(action))
}

fn destroy_action_for_type(ty: &ResolvedTy, ids: &PhysicalGlueIds) -> Option<DestroyAction> {
    match ty {
        _ if ids.resources.contains_key(ty) => Some(DestroyAction::Resource(ids.resources[ty])),
        _ if encoding_format(ty).is_some() => encoding_format(ty).map(DestroyAction::Encoding),
        ResolvedTy::Function { .. } | ResolvedTy::Closure { .. } => Some(DestroyAction::Callable),
        ResolvedTy::TraitObject { .. } => Some(DestroyAction::TraitObject),
        ResolvedTy::String => Some(DestroyAction::StringRelease),
        ResolvedTy::Bytes => Some(DestroyAction::BytesRelease),
        ResolvedTy::Array(_, _) if ids.vectors.contains_key(ty) => {
            Some(DestroyAction::Array(ids.vectors[ty]))
        }
        _ if ids.vectors.contains_key(ty) => Some(DestroyAction::Vector(ids.vectors[ty])),
        _ if ids.maps.contains_key(ty) => Some(DestroyAction::Map(ids.maps[ty])),
        _ if ids.sets.contains_key(ty) => Some(DestroyAction::Set(ids.sets[ty])),
        _ if ids.aggregates.contains_key(ty) => Some(DestroyAction::Aggregate(ids.aggregates[ty])),
        _ => ids.variants.get(ty).copied().map(DestroyAction::Variant),
    }
}

/// Resolve an encoding carrier through the shared canonical receiver contract.
#[must_use]
pub fn encoding_format(ty: &ResolvedTy) -> Option<EncodingFormat> {
    [EncodingFormat::Json, EncodingFormat::Yaml]
        .into_iter()
        .find(|format| {
            hew_types::RuntimeValueKind::Receiver(format.builtin())
                .resolve(Some(ty))
                .is_some()
        })
}

/// Collect the concrete semantic types that the physical module must realize.
///
/// Generic templates and unrelated checker fact rows are deliberately absent:
/// the inventory follows only callable headers and storage-producing types in
/// the verified concrete SIR module.
#[must_use]
pub fn physical_type_inventory(module: &SemModule) -> PhysicalTypeInventory {
    let mut types = BTreeSet::new();
    for callable in &module.callables {
        types.extend(
            callable
                .signature
                .params
                .iter()
                .map(|parameter| parameter.ty.clone()),
        );
        types.insert(callable.signature.return_ty.clone());
    }
    for function in &module.functions {
        types.insert(function.return_ty.clone());
        types.extend(function.params.iter().map(|parameter| parameter.ty.clone()));
        types.extend(function.places.iter().map(|place| place.ty.clone()));
        for block in &function.blocks {
            types.extend(block.args.iter().map(|argument| argument.ty.clone()));
            for operation in &block.ops {
                types.extend(operation.results.iter().map(|result| result.ty.clone()));
            }
            if let Some(result) = terminator_result(&block.terminator) {
                types.insert(result.ty.clone());
            }
            if let SemTerminator::WireCodec { plan, .. } = &block.terminator {
                plan.visit_types(&mut |ty| {
                    types.insert(ty.clone());
                });
            }
        }
    }
    for vtable in &module.vtables {
        types.insert(vtable.dyn_ty.clone());
        types.insert(vtable.concrete_ty.clone());
    }
    let mut inventory = PhysicalTypeInventory {
        types,
        resources: BTreeMap::new(),
        aggregates: BTreeMap::new(),
        variants: BTreeMap::new(),
        vectors: BTreeMap::new(),
        maps: BTreeMap::new(),
        sets: BTreeMap::new(),
    };
    let demanded = inventory.types.iter().cloned().collect::<Vec<_>>();
    for ty in demanded {
        collect_inventory_type(module, &mut inventory, &ty);
    }
    inventory
}

fn collect_resource_type(
    module: &SemModule,
    inventory: &mut PhysicalTypeInventory,
    ty: &ResolvedTy,
) -> bool {
    let Some(release) = module.resources.get(ty) else {
        return false;
    };
    let Some(facts) = module.type_facts.get(&TypeInstanceKey(ty.clone())) else {
        return false;
    };
    if hew_sir::verify_resource_release(ty, release, facts).is_err() {
        return false;
    }
    inventory.resources.insert(
        ty.clone(),
        PhysicalResourceDescriptor {
            ty: ty.clone(),
            release: release.clone(),
        },
    );
    true
}

#[allow(
    clippy::too_many_lines,
    reason = "one recursive inventory walk keeps concrete type descriptors at the same boundary"
)]
fn collect_inventory_type(
    module: &SemModule,
    inventory: &mut PhysicalTypeInventory,
    ty: &ResolvedTy,
) {
    if inventory.resources.contains_key(ty)
        || inventory.aggregates.contains_key(ty)
        || inventory.variants.contains_key(ty)
        || inventory.vectors.contains_key(ty)
        || inventory.maps.contains_key(ty)
        || inventory.sets.contains_key(ty)
    {
        return;
    }
    inventory.types.insert(ty.clone());
    if collect_resource_type(module, inventory, ty) {
        if let Some((yielded, returned)) = hew_sir::generator_parts(ty) {
            collect_inventory_type(module, inventory, yielded);
            collect_inventory_type(module, inventory, returned);
        }
        // A record resource is released by its own `close`, and inside that
        // body its members drop directly, so it needs the aggregate glue as
        // well as the release. Every other resource is a bare handle.
        if !matches!(
            module.resources.get(ty),
            Some(hew_sir::ResourceRelease::RecordClose { .. })
        ) {
            return;
        }
    }
    if let ResolvedTy::Closure { captures, .. } = ty {
        for capture in captures {
            collect_inventory_type(module, inventory, capture);
        }
        return;
    }
    if let Some(element) = sequence_element_type(ty) {
        inventory.vectors.insert(
            ty.clone(),
            PhysicalVectorDescriptor {
                ty: ty.clone(),
                element: element.clone(),
            },
        );
        collect_inventory_type(module, inventory, element);
        return;
    }
    match collection_type_arguments(ty) {
        Some((BuiltinType::HashMap, args)) => {
            inventory.maps.insert(
                ty.clone(),
                PhysicalMapDescriptor {
                    ty: ty.clone(),
                    key: args[0].clone(),
                    value: args[1].clone(),
                },
            );
            for argument in args {
                collect_inventory_type(module, inventory, argument);
            }
            return;
        }
        Some((BuiltinType::HashSet, args)) => {
            inventory.sets.insert(
                ty.clone(),
                PhysicalSetDescriptor {
                    ty: ty.clone(),
                    element: args[0].clone(),
                },
            );
            collect_inventory_type(module, inventory, &args[0]);
            return;
        }
        _ => {}
    }
    if let Some(shape) = module.variant_shape_for_type(ty) {
        let variants = shape
            .variants
            .iter()
            .map(|variant| {
                variant
                    .fields
                    .iter()
                    .map(|field| field.ty.clone())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        inventory.variants.insert(
            ty.clone(),
            PhysicalVariantDescriptor {
                ty: ty.clone(),
                is_indirect: shape.is_indirect,
                variants: variants.clone(),
            },
        );
        for fields in variants {
            for field in fields {
                collect_inventory_type(module, inventory, &field);
            }
        }
        return;
    }
    let fields = match ty {
        ResolvedTy::Tuple(fields) => Some(fields.clone()),
        _ => module
            .aggregate_shape_for_type(ty)
            .map(|shape| shape.fields.iter().map(|field| field.ty.clone()).collect()),
    };
    let Some(fields) = fields else {
        return;
    };
    inventory.aggregates.insert(
        ty.clone(),
        PhysicalAggregateDescriptor {
            ty: ty.clone(),
            fields: fields.clone(),
        },
    );
    for field in fields {
        collect_inventory_type(module, inventory, &field);
    }
}

/// The one rule that decides whether a parameter rides its own value or a
/// pointer to it. Direct calls, indirect invocations and vtable slots all
/// derive their ABI from this, so a dispatch and its implementation cannot
/// disagree about a carrier.
pub(crate) fn param_carrier(
    passing: hew_sir::SemParamPassing,
    layout: &PhysicalLayout,
) -> ParamCarrier {
    if passing == hew_sir::SemParamPassing::BorrowMut
        || matches!(layout.repr, PhysicalRepr::Struct(_))
    {
        ParamCarrier::Indirect
    } else {
        ParamCarrier::Direct
    }
}

fn required_layout<'a>(
    target: &'a PhysicalTarget,
    ty: &ResolvedTy,
) -> Result<&'a PhysicalLayout, PhysicalError> {
    target.layout(ty).ok_or_else(|| {
        PhysicalError::new(format!(
            "target `{}` has no concrete layout for `{}`",
            target.triple,
            ty.user_facing()
        ))
    })
}

struct FunctionLowerer<'a> {
    module: &'a SemModule,
    target: &'a PhysicalTarget,
    function: &'a SemFunction,
    glue_ids: &'a PhysicalGlueIds,
    values: BTreeMap<ValueId, StorageId>,
    places: BTreeMap<hew_sir::PlaceId, StorageId>,
    storage: Vec<PhysicalStorage>,
    projections: &'a hew_sir::PlacePlan,
    lifetimes: &'a hew_sir::PlaceLifetimes,
}

#[allow(
    clippy::too_many_lines,
    reason = "one lowering pass allocates every verified SIR value, place, and variant-arm payload before lowering CFG bodies"
)]
fn lower_function(
    module: &SemModule,
    target: &PhysicalTarget,
    function: &SemFunction,
    glue_ids: &PhysicalGlueIds,
    certificate: &hew_sir::CheckedFunction,
) -> Result<PhysicalFunction, PhysicalError> {
    let mut lowerer = FunctionLowerer {
        module,
        target,
        function,
        glue_ids,
        values: BTreeMap::new(),
        places: BTreeMap::new(),
        storage: Vec::new(),
        projections: certificate.place_plan(),
        lifetimes: certificate.place_lifetimes(),
    };
    let mut parameters = Vec::with_capacity(function.params.len());
    for parameter in &function.params {
        parameters.push(lowerer.insert_value(
            parameter.value,
            &parameter.ty,
            parameter.own,
            StorageOrigin::Parameter(parameter.value),
        )?);
    }
    for block in &function.blocks {
        for argument in &block.args {
            lowerer.insert_value(
                argument.value,
                &argument.ty,
                argument.own,
                StorageOrigin::BlockArgument(argument.value),
            )?;
        }
        for operation in &block.ops {
            for result in &operation.results {
                lowerer.insert_value(
                    result.id,
                    &result.ty,
                    result.own,
                    StorageOrigin::Value(result.id),
                )?;
            }
        }
        if let Some(result) = terminator_result(&block.terminator) {
            lowerer.insert_value(
                result.id,
                &result.ty,
                result.own,
                StorageOrigin::Value(result.id),
            )?;
        }
        if let SemTerminator::SwitchVariant { arms, .. } = &block.terminator {
            for arm in arms {
                for field in &arm.fields {
                    lowerer.insert_value(
                        field.id,
                        &field.ty,
                        field.own,
                        StorageOrigin::Value(field.id),
                    )?;
                }
            }
        }
    }
    for place in &function.places {
        let id = lowerer.next_storage_id()?;
        let previous = lowerer.places.insert(place.id, id);
        if previous.is_some() {
            return Err(PhysicalError::new(format!(
                "function `{}` declares physical place {} more than once",
                function.name, place.id.0
            )));
        }
        lowerer.storage.push(PhysicalStorage {
            id,
            ty: place.ty.clone(),
            layout: required_layout(target, &place.ty)?.clone(),
            own: if let Some(projection) = lowerer.projections.projection(place.id) {
                projection.recipe.own
            } else {
                OwnKind::of_ty(&place.ty, &module.type_facts).map_err(PhysicalError::new)?
            },
            origin: match place.origin {
                hew_sir::PlaceOrigin::ActorState {
                    state,
                    field,
                    initialized,
                    ..
                } => StorageOrigin::ActorState {
                    state: lowerer.value(state)?,
                    field,
                    initialized,
                },
                hew_sir::PlaceOrigin::Capture { environment, field } => StorageOrigin::Capture {
                    environment: lowerer.value(environment)?,
                    field,
                },
                hew_sir::PlaceOrigin::Aggregate { .. } => StorageOrigin::Aggregate(place.id),
                hew_sir::PlaceOrigin::Local => StorageOrigin::Local(place.id),
                hew_sir::PlaceOrigin::Runtime => {
                    return Err(PhysicalError::new(
                        "runtime place lacks a physical storage contract",
                    ));
                }
            },
            borrow_parent: None,
        });
    }

    for operation in function.blocks.iter().flat_map(|block| &block.ops) {
        if let Some(parent) = operation.kind.borrow_parent() {
            let dest = lowerer.one_result(operation)?;
            let source = match parent {
                hew_sir::PlaceBase::Place(place) => lowerer.place(place)?,
                hew_sir::PlaceBase::Value(value) => lowerer.value(value)?,
            };
            lowerer.storage[dest.0 as usize].borrow_parent = Some(source);
        }
    }

    // A runtime family whose contract result is a loan names argument zero as
    // the owner the result depends on. The dependency travels to the normal
    // successor's parameter, which is the loan the body reads.
    for block in &function.blocks {
        let hew_sir::SemTerminator::RtCall {
            family,
            args,
            result: hew_sir::CallResult::Value(value),
            normal,
            ..
        } = &block.terminator
        else {
            continue;
        };
        if !matches!(
            family.semantic_contract().map(|contract| contract.result),
            Some(hew_types::RuntimeResultEffect::Borrowed(_))
        ) {
            continue;
        }
        let owner = args
            .first()
            .ok_or_else(|| PhysicalError::new("borrowed runtime read has no receiver"))?;
        let source = lowerer.value(owner.operand.value)?;
        let dest = lowerer.value(value.id)?;
        lowerer.storage[dest.0 as usize].borrow_parent = Some(source);
        for parameter in blocks_by_id(function, normal.target)
            .map(|target| target.args.iter().map(|arg| arg.value).collect::<Vec<_>>())
            .unwrap_or_default()
        {
            let parameter = lowerer.value(parameter)?;
            lowerer.storage[parameter.0 as usize].borrow_parent = Some(source);
        }
    }

    // A switch over a loaned scrutinee names it as the owner its payloads
    // depend on: the arm's parameters are loans of the same region.
    for block in &function.blocks {
        let hew_sir::SemTerminator::SwitchVariant {
            scrutinee, arms, ..
        } = &block.terminator
        else {
            continue;
        };
        let source = lowerer.value(scrutinee.value)?;
        if lowerer.storage[source.0 as usize].own != OwnKind::Guaranteed {
            continue;
        }
        for arm in arms {
            for field in &arm.fields {
                let dest = lowerer.value(field.id)?;
                if lowerer.storage[dest.0 as usize].own == OwnKind::Guaranteed {
                    lowerer.storage[dest.0 as usize].borrow_parent = Some(source);
                }
            }
            for parameter in blocks_by_id(function, arm.target.target)
                .map(|target| target.args.iter().map(|arg| arg.value).collect::<Vec<_>>())
                .unwrap_or_default()
            {
                let parameter = lowerer.value(parameter)?;
                if lowerer.storage[parameter.0 as usize].own == OwnKind::Guaranteed {
                    lowerer.storage[parameter.0 as usize].borrow_parent = Some(source);
                }
            }
        }
    }

    let cfg = hew_sir::build_cfg_index(function);
    let blocks = function
        .blocks
        .iter()
        .filter(|block| cfg.reachable().contains(&block.id))
        .map(|block| {
            if !lowerer.lifetimes.is_reachable(block.id) {
                // Fault dispatch can prove one structurally present edge
                // impossible. Preserve its target identity without inventing
                // ownership certificates for code that cannot execute.
                return Ok(PhysicalBlock {
                    id: block.id,
                    arguments: block
                        .args
                        .iter()
                        .map(|arg| lowerer.value(arg.value))
                        .collect::<Result<_, _>>()?,
                    ops: vec![],
                    terminator: PhysicalTerminator::Unreachable,
                });
            }
            let arguments = block
                .args
                .iter()
                .map(|argument| lowerer.value(argument.value))
                .collect::<Result<Vec<_>, _>>()?;
            let ops = block
                .ops
                .iter()
                .try_fold(Vec::new(), |mut ops, operation| {
                    ops.extend(lowerer.lower_op(operation, (block.id, ops.len()))?);
                    Ok::<_, PhysicalError>(ops)
                })?;
            let terminator = lowerer.lower_terminator(&block.terminator)?;
            Ok(PhysicalBlock {
                id: block.id,
                arguments,
                ops,
                terminator,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;

    Ok(PhysicalFunction {
        callable: function.callable,
        entry: function.entry,
        parameters,
        place_storage: lowerer.lower_place_storage()?,
        storage: lowerer.storage,
        blocks,
    })
}

fn terminator_result(terminator: &SemTerminator) -> Option<&hew_sir::ValueDef> {
    match terminator {
        SemTerminator::Call {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::WireCodec {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::RtCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::ExternCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::ActorCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::IndirectCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::DynCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::ValueCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::Suspend {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::CheckedBinary { result, .. }
        | SemTerminator::RecoverFault { result, .. } => Some(result),
        _ => None,
    }
}

fn blocks_by_id(
    function: &hew_sir::SemFunction,
    id: hew_sir::BlockId,
) -> Option<&hew_sir::SemBlock> {
    function.blocks.iter().find(|block| block.id == id)
}

fn physical_runtime_action(
    family: RuntimeCallFamily,
) -> Result<PhysicalRuntimeAction, PhysicalError> {
    Ok(match family {
        RuntimeCallFamily::FileRead(op) => PhysicalRuntimeAction::FileRead(op),
        RuntimeCallFamily::Tcp(op) => PhysicalRuntimeAction::Tcp(op),
        RuntimeCallFamily::StreamClose => PhysicalRuntimeAction::StreamClose,
        RuntimeCallFamily::SinkClose => PhysicalRuntimeAction::SinkClose,
        RuntimeCallFamily::ChannelSenderClone => PhysicalRuntimeAction::ChannelSenderClone,
        RuntimeCallFamily::ChannelSenderClose => PhysicalRuntimeAction::ChannelSenderClose,
        RuntimeCallFamily::ChannelReceiverClose => PhysicalRuntimeAction::ChannelReceiverClose,
        RuntimeCallFamily::ChannelPairNew => PhysicalRuntimeAction::ChannelPairNew,
        RuntimeCallFamily::ChannelPairFree => PhysicalRuntimeAction::ChannelPairFree,
        RuntimeCallFamily::ActorRequestRelease => PhysicalRuntimeAction::ActorRequestRelease,
        RuntimeCallFamily::ActorCallFree => PhysicalRuntimeAction::ActorCallFree,
        RuntimeCallFamily::ActorRequestTake => PhysicalRuntimeAction::ActorRequestTake,
        RuntimeCallFamily::ChannelPairIsValid => PhysicalRuntimeAction::ChannelPairIsValid,
        RuntimeCallFamily::ChannelPairSender => PhysicalRuntimeAction::ChannelPairSender,
        RuntimeCallFamily::ChannelPairReceiver => PhysicalRuntimeAction::ChannelPairReceiver,
        RuntimeCallFamily::Encoding { format, op } => {
            PhysicalRuntimeAction::Encoding { format, op }
        }
        RuntimeCallFamily::StringConcat => PhysicalRuntimeAction::StringConcat,
        RuntimeCallFamily::StringEquals => PhysicalRuntimeAction::StringEquals,
        RuntimeCallFamily::StringCompare => PhysicalRuntimeAction::StringCompare,
        RuntimeCallFamily::StringStartsWith => PhysicalRuntimeAction::StringStartsWith,
        RuntimeCallFamily::StringEndsWith => PhysicalRuntimeAction::StringEndsWith,
        RuntimeCallFamily::StringContains => PhysicalRuntimeAction::StringContains,
        RuntimeCallFamily::StringIsEmpty => PhysicalRuntimeAction::StringIsEmpty,
        RuntimeCallFamily::StringIsDigit => PhysicalRuntimeAction::StringIsDigit,
        RuntimeCallFamily::StringIsAlpha => PhysicalRuntimeAction::StringIsAlpha,
        RuntimeCallFamily::StringIsAlphanumeric => PhysicalRuntimeAction::StringIsAlphanumeric,
        RuntimeCallFamily::StringToBytes => PhysicalRuntimeAction::StringToBytesOwned,
        RuntimeCallFamily::StringToUppercase => PhysicalRuntimeAction::StringToUppercase,
        RuntimeCallFamily::StringToLowercase => PhysicalRuntimeAction::StringToLowercase,
        RuntimeCallFamily::StringSlice => PhysicalRuntimeAction::StringSlice,
        RuntimeCallFamily::StringRepeat => PhysicalRuntimeAction::StringRepeat,
        RuntimeCallFamily::MathIntrinsic(kind) => PhysicalRuntimeAction::MathIntrinsic(kind),
        RuntimeCallFamily::StringReplace => PhysicalRuntimeAction::StringReplace,
        RuntimeCallFamily::StringClone => PhysicalRuntimeAction::StringClone,
        RuntimeCallFamily::StringSplit => PhysicalRuntimeAction::StringSplit,
        RuntimeCallFamily::StringLines => PhysicalRuntimeAction::StringLines,
        RuntimeCallFamily::StringChars => PhysicalRuntimeAction::StringChars,
        RuntimeCallFamily::StringIndex => PhysicalRuntimeAction::StringIndex,
        RuntimeCallFamily::StringSliceCodepoints => PhysicalRuntimeAction::StringSliceCodepoints,
        RuntimeCallFamily::StringSliceCodepointsFrom => {
            PhysicalRuntimeAction::StringSliceCodepointsFrom
        }
        RuntimeCallFamily::StringTrim => PhysicalRuntimeAction::StringTrim,
        RuntimeCallFamily::StringLen => PhysicalRuntimeAction::StringLen,
        RuntimeCallFamily::StringByteLen => PhysicalRuntimeAction::StringByteLen,
        RuntimeCallFamily::BytesDecodeUtf8Lossy => PhysicalRuntimeAction::BytesDecodeUtf8Lossy,
        RuntimeCallFamily::U8ToString => PhysicalRuntimeAction::U8ToString,
        RuntimeCallFamily::I32ToString => PhysicalRuntimeAction::I32ToString,
        RuntimeCallFamily::I64ToString => PhysicalRuntimeAction::I64ToString,
        RuntimeCallFamily::U32ToString => PhysicalRuntimeAction::U32ToString,
        RuntimeCallFamily::U64ToString => PhysicalRuntimeAction::U64ToString,
        RuntimeCallFamily::F64ToString => PhysicalRuntimeAction::F64ToString,
        RuntimeCallFamily::CharToString => PhysicalRuntimeAction::CharToString,
        RuntimeCallFamily::BoolToString => PhysicalRuntimeAction::BoolToString,
        RuntimeCallFamily::Print { kind, newline } => {
            PhysicalRuntimeAction::Print { kind, newline }
        }
        RuntimeCallFamily::ProcessExit => PhysicalRuntimeAction::ProcessExit,
        RuntimeCallFamily::StderrWrite => PhysicalRuntimeAction::StderrWrite,
        RuntimeCallFamily::BytesNew => PhysicalRuntimeAction::BytesNew,
        RuntimeCallFamily::BytesLen => PhysicalRuntimeAction::BytesLen,
        RuntimeCallFamily::BytesIsEmpty => PhysicalRuntimeAction::BytesIsEmpty,
        RuntimeCallFamily::BytesClear => PhysicalRuntimeAction::BytesClear,
        RuntimeCallFamily::BytesContains => PhysicalRuntimeAction::BytesContains,
        RuntimeCallFamily::BytesSet => PhysicalRuntimeAction::BytesSet,
        RuntimeCallFamily::BytesIndex => PhysicalRuntimeAction::BytesIndex,
        RuntimeCallFamily::BytesSlice => PhysicalRuntimeAction::BytesSlice,
        RuntimeCallFamily::BytesSliceFrom => PhysicalRuntimeAction::BytesSliceFrom,
        family @ (RuntimeCallFamily::InstantNow
        | RuntimeCallFamily::InstantElapsed
        | RuntimeCallFamily::InstantDurationSince
        | RuntimeCallFamily::DurationNanos
        | RuntimeCallFamily::DurationMicros
        | RuntimeCallFamily::DurationMillis
        | RuntimeCallFamily::DurationSecs
        | RuntimeCallFamily::DurationMins
        | RuntimeCallFamily::DurationHours
        | RuntimeCallFamily::DurationAbs
        | RuntimeCallFamily::DurationIsZero) => PhysicalRuntimeAction::TimeScalar(family),
        RuntimeCallFamily::BytesPush => PhysicalRuntimeAction::BytesPushOwned,
        RuntimeCallFamily::BytesAppend => PhysicalRuntimeAction::BytesAppendOwned,
        RuntimeCallFamily::NodeShutdown => PhysicalRuntimeAction::NodeShutdown,
        RuntimeCallFamily::NodeRegister => PhysicalRuntimeAction::NodeRegister,
        _ => {
            return Err(PhysicalError::new(format!(
                "runtime family `{family:?}` has no physical no-unwind ABI action"
            )));
        }
    })
}

impl FunctionLowerer<'_> {
    fn next_storage_id(&self) -> Result<StorageId, PhysicalError> {
        Ok(StorageId(u32::try_from(self.storage.len()).map_err(
            |_| PhysicalError::new("physical function has more than u32::MAX storage slots"),
        )?))
    }

    fn insert_value(
        &mut self,
        value: ValueId,
        ty: &ResolvedTy,
        own: OwnKind,
        origin: StorageOrigin,
    ) -> Result<StorageId, PhysicalError> {
        let id = self.next_storage_id()?;
        if self.values.insert(value, id).is_some() {
            return Err(PhysicalError::new(format!(
                "SIR value {} is defined more than once while assigning physical storage",
                value.0
            )));
        }
        self.storage.push(PhysicalStorage {
            id,
            ty: ty.clone(),
            layout: required_layout(self.target, ty)?.clone(),
            own,
            origin,
            borrow_parent: None,
        });
        Ok(id)
    }

    fn value(&self, value: ValueId) -> Result<StorageId, PhysicalError> {
        self.values.get(&value).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "physical lowering cannot find SIR value {}",
                value.0
            ))
        })
    }

    fn place(&self, place: hew_sir::PlaceId) -> Result<StorageId, PhysicalError> {
        self.places.get(&place).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "physical lowering cannot find SIR place {}",
                place.0
            ))
        })
    }

    /// Derive the destination-width two's-complement bit pattern for an exact
    /// integer constant.
    ///
    /// The width and signedness come from the destination storage the same
    /// pass just allocated -- physical MIR owns representation, so the target
    /// fact is read here and never reconstructed in the backend. The checker
    /// and the SIR verifier already admitted the value against its type, so a
    /// value outside the destination range is malformed IR and fails closed.
    fn integer_const_bits(&self, dest: StorageId, value: i128) -> Result<u64, PhysicalError> {
        let storage = self
            .storage
            .get(dest.0 as usize)
            .ok_or_else(|| PhysicalError::new("integer constant has no destination storage"))?;
        let width = integer_width_bits(&storage.layout).ok_or_else(|| {
            PhysicalError::new("integer constant requires an integer destination layout")
        })?;
        let (low, high) = integer_bit_range(width, storage.ty.is_signed_integer());
        if value < low || value > high {
            return Err(PhysicalError::new(format!(
                "integer constant {value} does not fit its {width}-bit destination `{}`",
                storage.ty.user_facing()
            )));
        }
        // Deliberate two's-complement truncation: the value is already proven
        // in range, and the destination-width bit pattern is exactly the low
        // bits of its two's-complement encoding.
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            reason = "the destination-width bit pattern is the low bits of the two's-complement encoding"
        )]
        Ok(mask_to_width(value as u64, width))
    }

    fn one_result(&self, operation: &SemOp) -> Result<StorageId, PhysicalError> {
        let [result] = operation.results.as_slice() else {
            return Err(PhysicalError::new(format!(
                "SIR op {} requires exactly one physical result, got {}",
                operation.id.0,
                operation.results.len()
            )));
        };
        self.value(result.id)
    }

    fn no_results(operation: &SemOp) -> Result<(), PhysicalError> {
        if operation.results.is_empty() {
            Ok(())
        } else {
            Err(PhysicalError::new(format!(
                "SIR op {} must not produce physical results",
                operation.id.0
            )))
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the exhaustive SIR operation match is the auditable ownership-to-physical boundary"
    )]
    fn lower_op(
        &self,
        operation: &SemOp,
        site: (BlockId, usize),
    ) -> Result<Vec<PhysicalOp>, PhysicalError> {
        if matches!(
            operation.kind,
            SemOpKind::Unary {
                op: UnaryOp::RawDeref,
                ..
            }
        ) {
            return Err(PhysicalError::new(format!(
                "SIR op {} can fail without an explicit cleanup CFG edge",
                operation.id.0
            )));
        }
        let one = |op| Ok(vec![op]);
        match &operation.kind {
            SemOpKind::RegisterDefer {
                defer,
                scope,
                dependencies,
            } => {
                Self::no_results(operation)?;
                one(PhysicalOp::RegisterDefer {
                    defer: *defer,
                    scope: *scope,
                    dependencies: dependencies
                        .iter()
                        .map(|p| self.place(*p))
                        .collect::<Result<_, _>>()?,
                })
            }
            SemOpKind::ConstInteger(value) => {
                let dest = self.one_result(operation)?;
                let bits = self.integer_const_bits(dest, *value)?;
                one(PhysicalOp::Const {
                    dest,
                    value: PhysicalConst::IntegerBits(bits),
                })
            }
            SemOpKind::ConstBool(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Bool(*value),
            }),
            SemOpKind::ConstFloat(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Float(*value),
            }),
            SemOpKind::ConstChar(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Char(*value),
            }),
            SemOpKind::ConstUnit => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Unit,
            }),
            SemOpKind::ConstDuration(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Duration(*value),
            }),
            SemOpKind::ConstStr(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::String(*value),
            }),
            SemOpKind::ConstBytes(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Bytes(*value),
            }),
            SemOpKind::Unary { op, value } => one(PhysicalOp::Unary {
                dest: self.one_result(operation)?,
                op: *op,
                source: self.value(value.value)?,
            }),
            SemOpKind::Binary { op, lhs, rhs } => one(PhysicalOp::Binary {
                dest: self.one_result(operation)?,
                op: *op,
                lhs: self.value(lhs.value)?,
                rhs: self.value(rhs.value)?,
            }),
            SemOpKind::Cast { value, to } => one(PhysicalOp::Cast {
                dest: self.one_result(operation)?,
                source: self.value(value.value)?,
                to: to.clone(),
            }),
            SemOpKind::TupleMake { elements } => one(PhysicalOp::TupleMake {
                dest: self.one_result(operation)?,
                elements: elements
                    .iter()
                    .map(|element| self.value(element.value))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            SemOpKind::TupleGet { tuple, index } => one(PhysicalOp::TupleGet {
                dest: self.one_result(operation)?,
                tuple: self.value(tuple.value)?,
                index: *index,
            }),
            SemOpKind::ArrayMake { fields } => {
                let dest = self.one_result(operation)?;
                let glue = self
                    .glue_ids
                    .vectors
                    .get(&self.storage[dest.0 as usize].ty)
                    .copied()
                    .ok_or_else(|| PhysicalError::new("fixed array has no element recipe"))?;
                one(PhysicalOp::ArrayMake {
                    dest,
                    glue,
                    fields: fields
                        .iter()
                        .map(|field| self.value(field.value))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            SemOpKind::ArrayRepeat { value } => {
                let dest = self.one_result(operation)?;
                let glue = self
                    .glue_ids
                    .vectors
                    .get(&self.storage[dest.0 as usize].ty)
                    .copied()
                    .ok_or_else(|| PhysicalError::new("fixed array has no element recipe"))?;
                one(PhysicalOp::ArrayRepeat {
                    dest,
                    glue,
                    seed: self.value(value.value)?,
                })
            }
            SemOpKind::AggregateMake { fields, .. } => {
                let dest = self.one_result(operation)?;
                one(PhysicalOp::AggregateMake {
                    dest,
                    fields: fields
                        .iter()
                        .map(|field| self.value(field.value))
                        .collect::<Result<Vec<_>, _>>()?,
                    glue: self.aggregate_id(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::AggregateProjectCopy {
                aggregate, field, ..
            } => {
                let aggregate = self.value(aggregate.value)?;
                let dest = self.one_result(operation)?;
                one(PhysicalOp::AggregateProjectCopy {
                    dest,
                    aggregate,
                    field: *field,
                    glue: self.aggregate_id(&self.storage[aggregate.0 as usize].ty)?,
                    action: self.clone_action(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::AggregateProjectBorrow {
                aggregate, field, ..
            } => {
                let aggregate = self.value(aggregate.value)?;
                one(PhysicalOp::AggregateProjectBorrow {
                    dest: self.one_result(operation)?,
                    aggregate,
                    field: *field,
                    glue: self.aggregate_id(&self.storage[aggregate.0 as usize].ty)?,
                })
            }
            SemOpKind::CopyValue { source } => {
                let dest = self.one_result(operation)?;
                let ty = &operation.results[0].ty;
                one(PhysicalOp::Clone {
                    dest,
                    source: self.value(source.value)?,
                    action: self.clone_action(ty)?,
                })
            }
            SemOpKind::DestroyValue { value } => {
                Self::no_results(operation)?;
                let source = self.value(value.value)?;
                one(PhysicalOp::Destroy {
                    source,
                    action: self.destroy_action(&self.storage[source.0 as usize].ty)?,
                    cleanup: self.cleanup_recipe(operation.id, site, source)?,
                })
            }
            SemOpKind::Move { source } | SemOpKind::Fork { source } => one(PhysicalOp::Transfer {
                dest: self.one_result(operation)?,
                source: self.value(source.value)?,
            }),
            SemOpKind::BeginBorrow { owner } => one(PhysicalOp::Borrow {
                dest: self.one_result(operation)?,
                source: self.value(owner.value)?,
            }),
            SemOpKind::EndBorrow { borrow } => {
                Self::no_results(operation)?;
                one(PhysicalOp::EndBorrow {
                    source: self.value(borrow.value)?,
                })
            }
            SemOpKind::FunctionMake { callable } => one(PhysicalOp::FunctionMake {
                dest: self.one_result(operation)?,
                callee: *callable,
            }),
            SemOpKind::TaskScopeEnter {
                scope,
                parent,
                duration,
            } => one(PhysicalOp::TaskScopeEnter {
                scope: *scope,
                parent: *parent,
                duration: duration
                    .as_ref()
                    .map(|duration| self.value(duration.value))
                    .transpose()?,
            }),
            SemOpKind::TaskScopeClose { scope } => {
                one(PhysicalOp::TaskScopeClose { scope: *scope })
            }
            SemOpKind::GeneratorMake { closure, callable } => {
                one(self.lower_generator_make(operation, *closure, callable)?)
            }
            SemOpKind::StreamPipe { capacity } => {
                let [stream, sink] = operation.results.as_slice() else {
                    return Err(PhysicalError::new("stream pipe lacks its two halves"));
                };
                let element = hew_sir::pipe_parts(&stream.ty, &sink.ty)
                    .ok_or_else(|| PhysicalError::new("stream pipe halves disagree on element"))?;
                one(PhysicalOp::StreamPipe {
                    capacity: *capacity,
                    stream: self.value(stream.id)?,
                    sink: self.value(sink.id)?,
                    element: physical_value_recipe(self.module, self.glue_ids, element)?,
                })
            }
            SemOpKind::TaskSpawn { scope, callable } => {
                let dest = self.one_result(operation)?;
                let ResolvedTy::Task(output) = &self.storage[dest.0 as usize].ty else {
                    return Err(PhysicalError::new("task spawn has no exact output type"));
                };
                one(PhysicalOp::TaskSpawn {
                    scope: *scope,
                    callable: self.value(callable.value)?,
                    dest,
                    output: (output.as_ref() != &ResolvedTy::Never)
                        .then(|| physical_value_recipe(self.module, self.glue_ids, output))
                        .transpose()?,
                })
            }
            SemOpKind::ClosureMake { closure, fields } => one(PhysicalOp::ClosureMake {
                dest: self.one_result(operation)?,
                closure: *closure,
                fields: fields
                    .iter()
                    .map(|field| self.value(field.value))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            SemOpKind::DynMake { vtable, value } => one(PhysicalOp::DynMake {
                dest: self.one_result(operation)?,
                vtable: PhysicalVtableId(vtable.0),
                source: self.value(value.value)?,
            }),
            SemOpKind::CallableCoerce { source } => one(PhysicalOp::CallableCoerce {
                dest: self.one_result(operation)?,
                source: self.value(source.value)?,
            }),
            SemOpKind::LoadBorrow { place, .. } => one(PhysicalOp::Borrow {
                dest: self.one_result(operation)?,
                source: self.place(*place)?,
            }),
            SemOpKind::AllocPlace { place } => {
                Self::no_results(operation)?;
                one(PhysicalOp::StorageLive {
                    storage: self.place(*place)?,
                })
            }
            SemOpKind::LoadCopy { place } => {
                let source = self.place(*place)?;
                let ty = &self.storage[source.0 as usize].ty;
                one(PhysicalOp::Clone {
                    dest: self.one_result(operation)?,
                    source,
                    action: self.clone_action(ty)?,
                })
            }
            SemOpKind::LoadTake { place } => one(PhysicalOp::Transfer {
                dest: self.one_result(operation)?,
                source: self.place(*place)?,
            }),
            SemOpKind::StoreInit { place, value } => {
                Self::no_results(operation)?;
                one(PhysicalOp::Transfer {
                    dest: self.place(*place)?,
                    source: self.value(value.value)?,
                })
            }
            SemOpKind::StoreAssign { place, value } => {
                Self::no_results(operation)?;
                let dest = self.place(*place)?;
                let source = self.value(value.value)?;
                one(PhysicalOp::Assign {
                    dest,
                    source,
                    destroy_old: self.optional_destroy(dest)?,
                })
            }
            SemOpKind::EndLifetime { place } => {
                Self::no_results(operation)?;
                let storage = self.place(*place)?;
                one(PhysicalOp::StorageDead {
                    storage,
                    destroy: self.optional_destroy(storage)?,
                    cleanup: self.cleanup_recipe(operation.id, site, storage)?,
                })
            }
            SemOpKind::Destructure { aggregate, .. } => {
                let aggregate = self.value(aggregate.value)?;
                one(PhysicalOp::AggregateDestructure {
                    aggregate,
                    fields: operation
                        .results
                        .iter()
                        .map(|result| self.value(result.id))
                        .collect::<Result<Vec<_>, _>>()?,
                    glue: self.aggregate_id(&self.storage[aggregate.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantMake {
                variant, fields, ..
            } => {
                let dest = self.one_result(operation)?;
                one(PhysicalOp::VariantMake {
                    dest,
                    variant: *variant,
                    fields: fields
                        .iter()
                        .map(|field| self.value(field.value))
                        .collect::<Result<Vec<_>, _>>()?,
                    glue: self.variant_id(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantIs {
                variant, source, ..
            } => {
                let source = self.value(source.value)?;
                one(PhysicalOp::VariantIs {
                    dest: self.one_result(operation)?,
                    source,
                    variant: *variant,
                    glue: self.variant_id(&self.storage[source.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantProjectCopy {
                variant,
                source,
                field,
                ..
            } => {
                let source = self.value(source.value)?;
                let dest = self.one_result(operation)?;
                one(PhysicalOp::VariantProjectCopy {
                    dest,
                    source,
                    variant: *variant,
                    field: *field,
                    glue: self.variant_id(&self.storage[source.0 as usize].ty)?,
                    action: self.clone_action(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantProjectBorrow {
                variant,
                source,
                field,
                ..
            } => {
                let source = self.value(source.value)?;
                one(PhysicalOp::VariantProjectBorrow {
                    dest: self.one_result(operation)?,
                    source,
                    variant: *variant,
                    field: *field,
                    glue: self.variant_id(&self.storage[source.0 as usize].ty)?,
                })
            }
            SemOpKind::VariantDestructure {
                variant, source, ..
            } => {
                let source = self.value(source.value)?;
                one(PhysicalOp::VariantDestructure {
                    source,
                    variant: *variant,
                    fields: operation
                        .results
                        .iter()
                        .map(|result| self.value(result.id))
                        .collect::<Result<Vec<_>, _>>()?,
                    glue: self.variant_id(&self.storage[source.0 as usize].ty)?,
                })
            }
            SemOpKind::StrEq { .. } | SemOpKind::BytesEq { .. } => {
                Err(PhysicalError::new(format!(
                    "SIR op {} is not yet admitted by physical MIR",
                    operation.id.0
                )))
            }
        }
    }

    fn lower_edge(&self, edge: &Edge) -> Result<PhysicalEdge, PhysicalError> {
        let target = self
            .function
            .blocks
            .iter()
            .find(|block| block.id == edge.target)
            .ok_or_else(|| PhysicalError::new(format!("unknown SIR block {}", edge.target.0)))?;
        if edge.args.len() != target.args.len() {
            return Err(PhysicalError::new(format!(
                "edge to block {} has {} arguments for {} block parameters",
                edge.target.0,
                edge.args.len(),
                target.args.len()
            )));
        }
        let transfers = edge
            .args
            .iter()
            .zip(&target.args)
            .map(|(source, dest)| Ok((self.value(source.value)?, self.value(dest.value)?)))
            .collect::<Result<Vec<_>, PhysicalError>>()?;
        let leaf_transfers = edge
            .args
            .iter()
            .zip(&target.args)
            .map(|(source, dest)| {
                self.projections
                    .transfer(source.value, dest.value)
                    .map_err(PhysicalError::new)?
                    .into_iter()
                    .map(|(source, dest)| Ok((self.place(source)?, self.place(dest)?)))
                    .collect::<Result<Vec<_>, PhysicalError>>()
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect();
        Ok(PhysicalEdge {
            target: edge.target,
            transfers,
            leaf_transfers,
        })
    }

    #[allow(
        clippy::too_many_lines,
        reason = "keep the exhaustive semantic-to-physical terminator mapping together"
    )]
    fn lower_terminator(
        &self,
        terminator: &SemTerminator,
    ) -> Result<PhysicalTerminator, PhysicalError> {
        match terminator {
            SemTerminator::EnterDefer { defer, park, body } => Ok(PhysicalTerminator::EnterDefer {
                defer: *defer,
                park: *park,
                body: self.lower_edge(body)?,
            }),
            SemTerminator::FinishDefer { defer, park, next } => {
                Ok(PhysicalTerminator::FinishDefer {
                    defer: *defer,
                    park: *park,
                    next: self.lower_edge(next)?,
                })
            }
            SemTerminator::CleanupDispatch { normal, fault } => {
                Ok(PhysicalTerminator::CleanupDispatch {
                    normal: self.lower_edge(normal)?,
                    fault: self.lower_edge(fault)?,
                })
            }
            SemTerminator::RecoverFault {
                result,
                deadline_variant,
                fault_variant,
                normal,
                unwind,
            } => Ok(PhysicalTerminator::RecoverFault {
                result: self.value(result.id)?,
                glue: self.variant_id(&result.ty)?,
                deadline_variant: *deadline_variant,
                fault_variant: *fault_variant,
                normal: self.lower_edge(normal)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::CheckedRaiseFault { kind, cleanup } => {
                Ok(PhysicalTerminator::CheckedRaiseFault {
                    kind: *kind,
                    cleanup: self.lower_edge(cleanup)?,
                })
            }
            SemTerminator::Return { value } => Ok(PhysicalTerminator::Return {
                value: value
                    .as_ref()
                    .map(|value| self.return_transfer(value.operand.value, value.decision))
                    .transpose()?,
            }),
            SemTerminator::Goto(edge) => Ok(PhysicalTerminator::Goto(self.lower_edge(edge)?)),
            SemTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => Ok(PhysicalTerminator::Branch {
                condition: self.value(condition.value)?,
                then_target: self.lower_edge(then_target)?,
                else_target: self.lower_edge(else_target)?,
            }),
            SemTerminator::SwitchVariant {
                shape,
                scrutinee,
                arms,
                ..
            } => self.lower_variant_switch(*shape, scrutinee, arms),
            checked @ SemTerminator::CheckedBinary { .. } => self.lower_checked_binary(checked),
            SemTerminator::Call {
                callee,
                args,
                result,
                normal,
                unwind,
                ..
            } => Ok(PhysicalTerminator::Call {
                callee: *callee,
                args: self.argument_transfers(args)?,
                result: match result {
                    CallResult::Unit | CallResult::Never => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: normal
                    .as_ref()
                    .map(|edge| self.lower_edge(edge))
                    .transpose()?,
                unwind: match unwind {
                    CallUnwind::NotApplicable => None,
                    CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
                },
            }),
            SemTerminator::ActorCall {
                operation,
                args,
                result,
                normal,
                unwind,
                ..
            } => Ok(PhysicalTerminator::ActorCall {
                operation: operation.clone(),
                args: self.argument_transfers(args)?,
                result: match result {
                    CallResult::Unit | CallResult::Never => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: self.lower_edge(normal)?,
                unwind: match unwind {
                    CallUnwind::NotApplicable => None,
                    CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
                },
            }),
            SemTerminator::ExternCall {
                signature,
                args,
                result,
                normal,
                ..
            } => Ok(PhysicalTerminator::ExternCall {
                symbol: signature.symbol.clone(),
                args: self.argument_transfers(args)?,
                result_abi: self.target.extern_result_abi(&signature.result)?,
                result: match result {
                    CallResult::Unit | CallResult::Never => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: self.lower_edge(normal)?,
            }),
            SemTerminator::WireCodec {
                direction,
                plan,
                text_result,
                args,
                result,
                normal,
                unwind,
                ..
            } => {
                let (CallResult::Value(result), CallUnwind::Cleanup(unwind)) = (result, unwind)
                else {
                    return Err(PhysicalError::new(
                        "wire codec requires result and fault cleanup",
                    ));
                };
                let transfers = self.argument_transfers(args)?;
                let [input] = transfers.as_slice() else {
                    return Err(PhysicalError::new("wire codec requires one borrowed input"));
                };
                let mut types = BTreeSet::new();
                plan.visit_types(&mut |ty| {
                    types.insert(ty.clone());
                });
                let recipes = types
                    .into_iter()
                    .map(|ty| {
                        physical_value_recipe(self.module, self.glue_ids, &ty)
                            .map(|recipe| (ty, recipe))
                    })
                    .collect::<Result<_, _>>()?;
                let text_result = text_result
                    .map(|cases| {
                        self.variant_id(&result.ty)
                            .map(|glue| PhysicalWireTextResult {
                                glue,
                                ok: cases.ok,
                                error: cases.error,
                            })
                    })
                    .transpose()?;
                Ok(PhysicalTerminator::WireCodec {
                    text_result,
                    direction: *direction,
                    plan: std::sync::Arc::clone(plan),
                    recipes,
                    input: *input,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(normal)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::RtCall {
                family,
                args,
                result,
                normal,
                unwind,
                ..
            } => Ok(PhysicalTerminator::RuntimeCall {
                action: self.runtime_action(*family, args, result)?,
                args: self.argument_transfers(args)?,
                result: match result {
                    CallResult::Unit | CallResult::Never => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: self.lower_edge(normal)?,
                failure: match unwind {
                    CallUnwind::NotApplicable => None,
                    CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
                },
            }),
            call @ SemTerminator::IndirectCall { .. } => self.lower_indirect_call(call),
            call @ SemTerminator::DynCall { .. } => self.lower_dyn_call(call),
            SemTerminator::ValueCall {
                ty,
                capability,
                args,
                result,
                normal,
                unwind,
                ..
            } => {
                let (CallResult::Value(result), CallUnwind::Cleanup(unwind)) = (result, unwind)
                else {
                    return Err(PhysicalError::new(
                        "selected value call requires a scalar result and fault cleanup",
                    ));
                };
                Ok(PhysicalTerminator::ValueCall {
                    ty: ty.clone(),
                    capability: *capability,
                    args: self.argument_transfers(args)?,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(normal)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Panic { message, cleanup } => Ok(PhysicalTerminator::Panic {
                message: self.argument_transfers(std::slice::from_ref(message))?[0],
                cleanup: self.lower_edge(cleanup)?,
            }),
            SemTerminator::Trap { kind } => Ok(PhysicalTerminator::Trap(*kind)),
            SemTerminator::ResumeUnwind => Ok(PhysicalTerminator::PropagateFault),
            SemTerminator::Unreachable => Ok(PhysicalTerminator::Unreachable),
            term @ SemTerminator::Suspend {
                kind:
                    hew_sir::SuspendKind::Yield
                    | hew_sir::SuspendKind::GeneratorNext
                    | hew_sir::SuspendKind::ValueClose { .. },
                ..
            } => self.lower_generator_suspend(term),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Select { has_timeout, order },
                inputs,
                result,
                resumes,
                cancel,
                unwind,
            } => {
                let (tasks, timeout) = if *has_timeout {
                    let (duration, tasks) = inputs.split_last().ok_or_else(|| {
                        PhysicalError::new("timed selection lacks its duration input")
                    })?;
                    (tasks, Some(self.value(duration.operand.value)?))
                } else {
                    (inputs.as_slice(), None)
                };
                let CallResult::Value(result) = result else {
                    return Err(PhysicalError::new(
                        "selection lacks its source index result",
                    ));
                };
                let sources = self
                    .argument_transfers(tasks)?
                    .into_iter()
                    .zip(tasks)
                    .map(|(transfer, input)| {
                        let ty = &self.storage[self.value(input.operand.value)?.0 as usize].ty;
                        // The operand's own type says which substrate this arm
                        // observes; the selection has no other authority.
                        if ty.is_builtin(hew_types::BuiltinType::Receiver) {
                            Ok(PhysicalSelectSource::ChannelRecv(transfer))
                        } else if ty.is_builtin(hew_types::BuiltinType::ActorCall) {
                            Ok(PhysicalSelectSource::ActorCall(transfer))
                        } else {
                            Ok(PhysicalSelectSource::Task(transfer))
                        }
                    })
                    .collect::<Result<Vec<_>, PhysicalError>>()?;
                Ok(PhysicalTerminator::TaskSelect {
                    order: *order,
                    sources,
                    timeout,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(&resumes[0])?,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::NativeIo { operation },
                inputs,
                result: CallResult::Value(result),
                resumes,
                cancel,
                unwind,
            } => Ok(PhysicalTerminator::NativeIo {
                operation: *operation,
                args: self.argument_transfers(inputs)?,
                result: self.value(result.id)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Sleep,
                inputs,
                resumes,
                cancel,
                unwind,
                ..
            } => Ok(PhysicalTerminator::Sleep {
                duration: self.value(inputs[0].operand.value)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::SleepUntil,
                inputs,
                resumes,
                cancel,
                unwind,
                ..
            } => Ok(PhysicalTerminator::SleepUntil {
                deadline: self.value(inputs[0].operand.value)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Await,
                inputs,
                result,
                resumes,
                cancel,
                unwind,
            } => Ok(PhysicalTerminator::TaskAwait {
                task: self.argument_transfers(inputs)?[0],
                result: match result {
                    CallResult::Value(value) => Some(self.value(value.id)?),
                    CallResult::Unit | CallResult::Never => None,
                },
                normal: resumes
                    .first()
                    .map(|edge| self.lower_edge(edge))
                    .transpose()?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::Join { scope, mode },
                resumes,
                unwind,
                ..
            } => Ok(PhysicalTerminator::TaskScopeJoin {
                scope: *scope,
                mode: *mode,
                normal: self.lower_edge(&resumes[0])?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind:
                    hew_sir::SuspendKind::Ask {
                        actor,
                        message,
                        policy,
                        deadline_ns,
                        sealed,
                    },
                inputs,
                result: CallResult::Value(result),
                resumes,
                cancel,
                unwind,
            } => Ok(PhysicalTerminator::ActorAsk {
                actor: *actor,
                message: *message,
                policy: *policy,
                deadline_ns: *deadline_ns,
                sealed: *sealed,
                args: self.argument_transfers(inputs)?,
                result: self.value(result.id)?,
                normal: self.lower_edge(&resumes[0])?,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            }),
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::StreamNext,
                inputs,
                result: CallResult::Value(result),
                resumes,
                cancel,
                unwind,
            } => {
                let shape = self
                    .module
                    .variant_shape_for_type(&result.ty)
                    .ok_or_else(|| {
                        PhysicalError::new("stream receive lacks its Option descriptor")
                    })?;
                let element = shape
                    .variants
                    .first()
                    .and_then(|variant| variant.fields.first())
                    .map(|field| field.ty.clone())
                    .ok_or_else(|| PhysicalError::new("stream receive lacks its element type"))?;
                Ok(PhysicalTerminator::StreamNext {
                    stream: self.argument_transfers(inputs)?[0],
                    element: physical_value_recipe(self.module, self.glue_ids, &element)?,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(&resumes[0])?,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::ChannelRecv { park },
                inputs,
                result: CallResult::Value(result),
                resumes,
                cancel,
                unwind,
            } => {
                let shape = self
                    .module
                    .variant_shape_for_type(&result.ty)
                    .ok_or_else(|| {
                        PhysicalError::new("channel receive lacks its Option descriptor")
                    })?;
                let element = shape
                    .variants
                    .first()
                    .and_then(|variant| variant.fields.first())
                    .map(|field| field.ty.clone())
                    .ok_or_else(|| PhysicalError::new("channel receive lacks its element type"))?;
                Ok(PhysicalTerminator::ChannelRecv {
                    park: *park,
                    channel: self.argument_transfers(inputs)?[0],
                    element: physical_value_recipe(self.module, self.glue_ids, &element)?,
                    result: self.value(result.id)?,
                    normal: self.lower_edge(&resumes[0])?,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::ChannelSend,
                inputs,
                resumes,
                cancel,
                unwind,
                ..
            } => {
                let transfers = self.argument_transfers(inputs)?;
                let element = self.storage[self.value(inputs[1].operand.value)?.0 as usize]
                    .ty
                    .clone();
                Ok(PhysicalTerminator::ChannelSend {
                    channel: transfers[0],
                    value: transfers[1],
                    element: physical_value_recipe(self.module, self.glue_ids, &element)?,
                    normal: self.lower_edge(&resumes[0])?,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::StreamSend,
                inputs,
                resumes,
                cancel,
                unwind,
                ..
            } => {
                let transfers = self.argument_transfers(inputs)?;
                let [normal, closed] = resumes.as_slice() else {
                    return Err(PhysicalError::new(
                        "stream send lacks its accepted and closed resumes",
                    ));
                };
                let element = &self.storage[self.value(inputs[1].operand.value)?.0 as usize].ty;
                Ok(PhysicalTerminator::StreamSend {
                    sink: transfers[0],
                    value: transfers[1],
                    element: physical_value_recipe(self.module, self.glue_ids, element)?,
                    normal: self.lower_edge(normal)?,
                    closed: self.lower_edge(closed)?,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                })
            }
            SemTerminator::Suspend { .. } => Err(PhysicalError::new(
                "suspension lacks a physical operation contract",
            )),
        }
    }

    fn lower_variant_switch(
        &self,
        shape: hew_sir::VariantShapeId,
        scrutinee: &hew_sir::Operand,
        arms: &[hew_sir::SemVariantArm],
    ) -> Result<PhysicalTerminator, PhysicalError> {
        let scrutinee = self.value(scrutinee.value)?;
        let scrutinee_ty = &self.storage[scrutinee.0 as usize].ty;
        let glue = self.variant_id(scrutinee_ty)?;
        let expected_shape = self
            .module
            .variant_shape_for_type(scrutinee_ty)
            .map(|descriptor| descriptor.id)
            .ok_or_else(|| PhysicalError::new("variant switch has no exact descriptor"))?;
        if shape != expected_shape {
            return Err(PhysicalError::new(
                "variant switch shape disagrees with its scrutinee type",
            ));
        }
        Ok(PhysicalTerminator::SwitchVariant {
            scrutinee,
            glue,
            arms: arms
                .iter()
                .map(|arm| {
                    Ok(PhysicalVariantArm {
                        variant: arm.variant,
                        fields: arm
                            .fields
                            .iter()
                            .map(|field| self.value(field.id))
                            .collect::<Result<Vec<_>, _>>()?,
                        target: self.lower_edge(&arm.target)?,
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?,
        })
    }

    fn lower_checked_binary(
        &self,
        terminator: &SemTerminator,
    ) -> Result<PhysicalTerminator, PhysicalError> {
        let SemTerminator::CheckedBinary {
            op,
            lhs,
            rhs,
            result,
            normal,
            failures,
            ..
        } = terminator
        else {
            return Err(PhysicalError::new(
                "physical checked-binary lowering received another terminator",
            ));
        };
        Ok(PhysicalTerminator::CheckedBinary {
            op: *op,
            lhs: self.value(lhs.value)?,
            rhs: self.value(rhs.value)?,
            result: self.value(result.id)?,
            normal: self.lower_edge(normal)?,
            failures: failures
                .iter()
                .map(|failure| {
                    Ok(PhysicalCheckedFailure {
                        kind: failure.kind,
                        edge: self.lower_edge(&failure.edge)?,
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?,
        })
    }

    fn argument_transfers(
        &self,
        args: &[hew_sir::BoundaryOperand],
    ) -> Result<Vec<ArgumentTransfer>, PhysicalError> {
        args.iter()
            .map(|argument| self.argument_transfer(argument.operand.value, argument.decision))
            .collect()
    }

    fn argument_transfer(
        &self,
        value: ValueId,
        decision: BoundaryDecision,
    ) -> Result<ArgumentTransfer, PhysicalError> {
        let source = self.value(value)?;
        Ok(match decision {
            BoundaryDecision::Borrow => ArgumentTransfer::Borrow(source),
            BoundaryDecision::BorrowMut => ArgumentTransfer::BorrowMut(source),
            BoundaryDecision::Move => ArgumentTransfer::Move(source),
            BoundaryDecision::Copy => ArgumentTransfer::Clone {
                source,
                action: self.clone_action(&self.storage[source.0 as usize].ty)?,
            },
            BoundaryDecision::Snapshot(
                SnapshotDecision::Share | SnapshotDecision::DeepCopy | SnapshotDecision::Transfer,
            ) => {
                return Err(PhysicalError::new(
                    "snapshot boundaries are not yet admitted by physical MIR",
                ));
            }
        })
    }

    fn return_transfer(
        &self,
        value: ValueId,
        decision: BoundaryDecision,
    ) -> Result<ReturnTransfer, PhysicalError> {
        let source = self.value(value)?;
        Ok(match decision {
            BoundaryDecision::Borrow => ReturnTransfer::Borrow(source),
            BoundaryDecision::BorrowMut => {
                return Err(PhysicalError::new(
                    "exclusive callable loans cannot escape through a return",
                ));
            }
            BoundaryDecision::Move => ReturnTransfer::Move(source),
            BoundaryDecision::Copy => ReturnTransfer::Clone {
                source,
                action: self.clone_action(&self.storage[source.0 as usize].ty)?,
            },
            BoundaryDecision::Snapshot(_) => {
                return Err(PhysicalError::new(
                    "snapshot returns are not yet admitted by physical MIR",
                ));
            }
        })
    }

    fn clone_action(&self, ty: &ResolvedTy) -> Result<CloneAction, PhysicalError> {
        let facts = self
            .module
            .type_facts
            .get(&TypeInstanceKey(ty.clone()))
            .ok_or_else(|| {
                PhysicalError::new(format!(
                    "physical copy of `{}` has no checker-owned type facts",
                    ty.user_facing()
                ))
            })?;
        clone_action_for_type(ty, facts.clone, self.glue_ids)?.ok_or_else(|| {
            PhysicalError::new(format!(
                "physical copy of `{}` has no admitted clone action",
                ty.user_facing()
            ))
        })
    }

    /// Inside a record resource's own `close`, the receiver is already being
    /// released: its members drop directly. Selecting the release again would
    /// re-enter `close`.
    fn releases_members_of(&self, ty: &ResolvedTy) -> bool {
        matches!(
            self.module.resources.get(ty),
            Some(hew_sir::ResourceRelease::RecordClose { close, .. })
                if *close == self.function.callable
        )
    }

    fn destroy_action(&self, ty: &ResolvedTy) -> Result<DestroyAction, PhysicalError> {
        if self.releases_members_of(ty) {
            return self
                .glue_ids
                .aggregates
                .get(ty)
                .copied()
                .map(DestroyAction::Aggregate)
                .ok_or_else(|| {
                    PhysicalError::new(format!(
                        "record resource `{}` has no member glue for its own close body",
                        ty.user_facing()
                    ))
                });
        }
        destroy_action_for_type(ty, self.glue_ids).ok_or_else(|| {
            PhysicalError::new(format!(
                "physical destroy action for `{}` is not implemented",
                ty.user_facing()
            ))
        })
    }

    fn collection_receiver_type<'a>(
        &'a self,
        args: &[hew_sir::BoundaryOperand],
        result: &'a hew_sir::ValueDef,
        constructor: bool,
    ) -> Result<&'a ResolvedTy, PhysicalError> {
        if constructor {
            return Ok(&result.ty);
        }
        let receiver = args
            .first()
            .ok_or_else(|| PhysicalError::new("collection operation lacks its receiver"))?;
        Ok(&self.storage[self.value(receiver.operand.value)?.0 as usize].ty)
    }

    fn map_action(
        &self,
        op: MapValueOp,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeAction, PhysicalError> {
        let CallResult::Value(value) = result else {
            return Err(PhysicalError::new("map operation has no result value"));
        };
        let receiver = self.collection_receiver_type(args, value, op == MapValueOp::New)?;
        let glue = self
            .glue_ids
            .maps
            .get(receiver)
            .copied()
            .ok_or_else(|| PhysicalError::new("map receiver has no physical glue identity"))?;
        let operation = match op {
            MapValueOp::New => PhysicalMapOp::New,
            MapValueOp::Len => PhysicalMapOp::Len,
            MapValueOp::Index => PhysicalMapOp::Index,
            MapValueOp::Get => PhysicalMapOp::Get {
                result: self.variant_id(&value.ty)?,
            },
            MapValueOp::GetBorrow => PhysicalMapOp::GetBorrow {
                result: self.variant_id(&value.ty)?,
            },
            MapValueOp::ContainsKey => PhysicalMapOp::ContainsKey,
            MapValueOp::Insert => PhysicalMapOp::Insert,
            MapValueOp::Remove => {
                let ResolvedTy::Tuple(fields) = &value.ty else {
                    return Err(PhysicalError::new(
                        "map removal lacks its receiver/value pair",
                    ));
                };
                let optional = fields
                    .get(1)
                    .ok_or_else(|| PhysicalError::new("map removal lacks its optional value"))?;
                PhysicalMapOp::Remove {
                    result: self.aggregate_id(&value.ty)?,
                    value: self.variant_id(optional)?,
                }
            }
            MapValueOp::Clear => PhysicalMapOp::Clear,
            MapValueOp::Keys => PhysicalMapOp::Keys,
            MapValueOp::Values => PhysicalMapOp::Values,
            MapValueOp::Entries => PhysicalMapOp::Entries {
                result: self
                    .glue_ids
                    .vectors
                    .get(&value.ty)
                    .copied()
                    .ok_or_else(|| PhysicalError::new("map entries lack their vector recipe"))?,
            },
        };
        Ok(PhysicalRuntimeAction::Map { operation, glue })
    }

    fn set_action(
        &self,
        op: SetValueOp,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeAction, PhysicalError> {
        let CallResult::Value(value) = result else {
            return Err(PhysicalError::new("set operation has no result value"));
        };
        let receiver = self.collection_receiver_type(args, value, op == SetValueOp::New)?;
        let glue = self
            .glue_ids
            .sets
            .get(receiver)
            .copied()
            .ok_or_else(|| PhysicalError::new("set receiver has no physical glue identity"))?;
        let operation = match op {
            SetValueOp::New => PhysicalSetOp::New,
            SetValueOp::Len => PhysicalSetOp::Len,
            SetValueOp::Contains => PhysicalSetOp::Contains,
            SetValueOp::Insert => PhysicalSetOp::Insert {
                result: self.aggregate_id(&value.ty)?,
            },
            SetValueOp::Remove => PhysicalSetOp::Remove {
                result: self.aggregate_id(&value.ty)?,
            },
            SetValueOp::Clear => PhysicalSetOp::Clear,
            SetValueOp::Elements => PhysicalSetOp::Elements,
        };
        Ok(PhysicalRuntimeAction::Set { operation, glue })
    }

    #[expect(
        clippy::too_many_lines,
        reason = "runtime families select target-specific glue in one closed authority"
    )]
    fn runtime_action(
        &self,
        family: RuntimeCallFamily,
        args: &[hew_sir::BoundaryOperand],
        result: &CallResult,
    ) -> Result<PhysicalRuntimeAction, PhysicalError> {
        match family {
            RuntimeCallFamily::Map(op) => return self.map_action(op, args, result),
            RuntimeCallFamily::Set(op) => return self.set_action(op, args, result),
            _ => {}
        }
        if let RuntimeCallFamily::Array(operation) = family {
            let receiver = args
                .first()
                .ok_or_else(|| PhysicalError::new("array operation has no receiver"))?;
            let ty = &self.storage[self.value(receiver.operand.value)?.0 as usize].ty;
            let glue = self
                .glue_ids
                .vectors
                .get(ty)
                .copied()
                .ok_or_else(|| PhysicalError::new("array receiver has no element recipe"))?;
            return Ok(PhysicalRuntimeAction::Array { operation, glue });
        }
        if let RuntimeCallFamily::Vector(op) = family {
            let CallResult::Value(value) = result else {
                return Err(PhysicalError::new("vector operation has no result value"));
            };
            let vector = if op == VecValueOp::New {
                &value.ty
            } else {
                let receiver = args
                    .first()
                    .ok_or_else(|| PhysicalError::new("vector operation has no receiver"))?;
                &self.storage[self.value(receiver.operand.value)?.0 as usize].ty
            };
            let glue = self.glue_ids.vectors.get(vector).copied().ok_or_else(|| {
                PhysicalError::new(format!(
                    "vector `{}` has no physical glue identity",
                    vector.user_facing()
                ))
            })?;
            let operation = match op {
                VecValueOp::New => PhysicalVectorOp::New,
                VecValueOp::Len => PhysicalVectorOp::Len,
                VecValueOp::Contains => PhysicalVectorOp::Contains,
                VecValueOp::Index => PhysicalVectorOp::Index,
                VecValueOp::Get => PhysicalVectorOp::Get {
                    result: self.variant_id(&value.ty)?,
                },
                VecValueOp::Push => PhysicalVectorOp::Push,
                VecValueOp::Set => PhysicalVectorOp::Set,
                VecValueOp::Pop => PhysicalVectorOp::Pop {
                    result: self.aggregate_id(&value.ty)?,
                },
                VecValueOp::Remove => PhysicalVectorOp::Remove {
                    result: self.aggregate_id(&value.ty)?,
                },
                VecValueOp::Clear => PhysicalVectorOp::Clear,
                VecValueOp::IndexBorrow => PhysicalVectorOp::IndexBorrow,
                VecValueOp::GetBorrow => PhysicalVectorOp::GetBorrow {
                    result: self.variant_id(&value.ty)?,
                },
                VecValueOp::TakeFirst => PhysicalVectorOp::TakeFirst {
                    result: self.aggregate_id(&value.ty)?,
                },
                VecValueOp::Slice => PhysicalVectorOp::Slice,
                VecValueOp::SliceFrom => PhysicalVectorOp::SliceFrom,
            };
            return Ok(PhysicalRuntimeAction::Vector { operation, glue });
        }
        if matches!(
            family,
            RuntimeCallFamily::NodeStart | RuntimeCallFamily::NodeConnect
        ) {
            if family == RuntimeCallFamily::NodeStart {
                let config = args
                    .first()
                    .ok_or_else(|| PhysicalError::new("Node::start has no NodeConfig argument"))?;
                let config_ty = &self.storage[self.value(config.operand.value)?.0 as usize].ty;
                let expected_fields = vec![
                    ResolvedTy::String,
                    ResolvedTy::String,
                    ResolvedTy::String,
                    ResolvedTy::String,
                    ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String]),
                    ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String]),
                ];
                let shape = self
                    .module
                    .aggregate_shapes
                    .iter()
                    .find(|shape| shape.aggregate_ty == *config_ty);
                if !shape.is_some_and(|shape| {
                    shape
                        .fields
                        .iter()
                        .map(|field| field.ty.clone())
                        .collect::<Vec<_>>()
                        == expected_fields
                }) {
                    return Err(PhysicalError::new(
                        "Node::start requires NodeConfig ABI fields bind, transport, key, trust, peers, seeds in source order",
                    ));
                }
            }
            let CallResult::Value(value) = result else {
                return Err(PhysicalError::new(
                    "node lifecycle operation has no Result value",
                ));
            };
            let result = self.variant_id(&value.ty)?;
            let error_ty = match &value.ty {
                ResolvedTy::Named { args, .. } if args.len() == 2 => &args[1],
                _ => {
                    return Err(PhysicalError::new(
                        "node lifecycle result is not Result<(), NodeError>",
                    ))
                }
            };
            return Ok(PhysicalRuntimeAction::NodeLifecycle {
                family,
                result,
                error: self.variant_id(error_ty)?,
            });
        }
        if family == RuntimeCallFamily::NodeLookup {
            let CallResult::Value(value) = result else {
                return Err(PhysicalError::new("Node::lookup has no Result value"));
            };
            let (_remote_ty, error_ty) = match &value.ty {
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::Result),
                    args,
                    ..
                } if args.len() == 2
                    && args[0].is_builtin(BuiltinType::RemotePid)
                    && args[1].is_builtin(BuiltinType::LookupError) =>
                {
                    (&args[0], &args[1])
                }
                _ => {
                    return Err(PhysicalError::new(
                        "Node::lookup result is not Result<RemotePid<T>, LookupError>",
                    ))
                }
            };
            let result = self.variant_id(&value.ty)?;
            return Ok(PhysicalRuntimeAction::NodeLookup {
                result,
                error: self.variant_id(error_ty)?,
            });
        }
        if matches!(
            family,
            RuntimeCallFamily::StringFind
                | RuntimeCallFamily::StringCharAt
                | RuntimeCallFamily::BytesGet
        ) {
            let CallResult::Value(value) = result else {
                return Err(PhysicalError::new("string find has no optional result"));
            };
            let result = self.variant_id(&value.ty)?;
            return Ok(match family {
                RuntimeCallFamily::StringFind => PhysicalRuntimeAction::StringFind { result },
                RuntimeCallFamily::StringCharAt => PhysicalRuntimeAction::StringCharAt { result },
                RuntimeCallFamily::BytesGet => PhysicalRuntimeAction::BytesGet { result },
                _ => unreachable!("matched optional runtime family"),
            });
        }
        if family == RuntimeCallFamily::BytesPop {
            let CallResult::Value(value) = result else {
                return Err(PhysicalError::new("bytes pop has no transformed result"));
            };
            let ResolvedTy::Tuple(fields) = &value.ty else {
                return Err(PhysicalError::new("bytes pop result is not a pair"));
            };
            let [_, option] = fields.as_slice() else {
                return Err(PhysicalError::new("bytes pop result is not a pair"));
            };
            return Ok(PhysicalRuntimeAction::BytesPop {
                result: self.aggregate_id(&value.ty)?,
                option: self.variant_id(option)?,
            });
        }
        if family != RuntimeCallFamily::BytesDecodeUtf8 {
            return physical_runtime_action(family);
        }
        let CallResult::Value(value) = result else {
            return Err(PhysicalError::new("UTF-8 decode has no result value"));
        };
        let refs = hew_sir::runtime_variant_shape_refs(
            hew_types::RuntimeVariantResultKind::Utf8Decode,
            &value.ty,
            &self.module.aggregate_shapes,
            &self.module.variant_shapes,
        )
        .map_err(PhysicalError::new)?;
        Ok(PhysicalRuntimeAction::BytesDecodeUtf8 {
            result: self.variant_id(&value.ty)?,
            error: self
                .aggregate_id(&self.module.aggregate_shapes[refs.error.0 as usize].aggregate_ty)?,
            error_len: self
                .variant_id(&self.module.variant_shapes[refs.error_len.0 as usize].enum_ty)?,
        })
    }

    fn aggregate_id(&self, ty: &ResolvedTy) -> Result<PhysicalAggregateId, PhysicalError> {
        self.glue_ids.aggregates.get(ty).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "aggregate `{}` has no physical glue identity",
                ty.user_facing()
            ))
        })
    }

    fn variant_id(&self, ty: &ResolvedTy) -> Result<PhysicalVariantId, PhysicalError> {
        self.glue_ids.variants.get(ty).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "variant `{}` has no physical glue identity",
                ty.user_facing()
            ))
        })
    }
}

fn verify_resources(module: &PhysicalModule) -> Result<(), PhysicalError> {
    for recipe in module.actor_recipes.values() {
        verify_value_recipe(module, recipe)?;
    }
    let mut resource_types = BTreeSet::new();
    for resource in &module.resources {
        if !resource_types.insert(&resource.ty) {
            return Err(PhysicalError::new("duplicate physical resource authority"));
        }
        hew_sir::verify_resource_release(
            &resource.ty,
            &resource.release,
            semantic_type_facts(module, &resource.ty)?,
        )
        .map_err(PhysicalError::new)?;
        if let hew_sir::ResourceRelease::RecordClose { lifecycle, close } = &resource.release {
            let callable = module
                .callables
                .get(close.0 as usize)
                .ok_or_else(|| PhysicalError::new("record release names no admitted callable"))?;
            if callable.declaration != lifecycle.close_declaration
                || callable.params.len() != 1
                || callable.params[0].ty != resource.ty
                || callable.return_ty != ResolvedTy::Unit
            {
                return Err(PhysicalError::new(
                    "record release callable does not consume one exact owner and return unit",
                ));
            }
            if !matches!(
                required_layout(&module.target, &resource.ty)?.repr,
                PhysicalRepr::Struct(_)
            ) {
                return Err(PhysicalError::new(
                    "record release requires its exact field-bearing layout",
                ));
            }
            continue;
        }
        let expected = match resource.release.carrier().map_err(PhysicalError::new)? {
            hew_sir::ResourceCarrier::Pointer => PhysicalRepr::Pointer,
            hew_sir::ResourceCarrier::I32 => PhysicalRepr::Integer { bits: 32 },
            hew_sir::ResourceCarrier::Record => {
                unreachable!("record releases are verified above")
            }
        };
        if required_layout(&module.target, &resource.ty)?.repr != expected {
            return Err(PhysicalError::new(
                "resource release requires its exact checked carrier",
            ));
        }
    }
    Ok(())
}

fn verify_physical_module(module: &PhysicalModule) -> Result<(), PhysicalError> {
    suspend::verify_callables(module)?;
    capability::verify(module)?;
    verify_resources(module)?;
    if module.target.triple.is_empty() || module.target.data_layout.is_empty() {
        return Err(PhysicalError::new(
            "physical module requires a target triple and data layout",
        ));
    }
    for (index, glue) in module.aggregate_glue.iter().enumerate() {
        if usize::try_from(glue.id.0).ok() != Some(index) {
            return Err(PhysicalError::new(format!(
                "physical aggregate glue {} is not at its canonical table index {index}",
                glue.id.0
            )));
        }
        if glue.own == OwnKind::Guaranteed {
            return Err(PhysicalError::new(format!(
                "physical aggregate glue {} carries a borrow-only ownership class",
                glue.id.0
            )));
        }
        let layout = required_layout(&module.target, &glue.ty)?;
        let PhysicalRepr::Struct(layout_fields) = &layout.repr else {
            return Err(PhysicalError::new(format!(
                "physical aggregate glue {} has a non-aggregate layout",
                glue.id.0
            )));
        };
        if layout_fields.len() != glue.fields.len() {
            return Err(PhysicalError::new(format!(
                "physical aggregate glue {} has {} recipes for {} layout fields",
                glue.id.0,
                glue.fields.len(),
                layout_fields.len()
            )));
        }
        for (field_index, (field, layout_field)) in
            glue.fields.iter().zip(layout_fields).enumerate()
        {
            if field.own == OwnKind::Guaranteed {
                return Err(PhysicalError::new(format!(
                    "physical aggregate glue {} field {field_index} carries a borrow-only obligation",
                    glue.id.0
                )));
            }
            if module.target.layout(&field.ty) != Some(layout_field) {
                return Err(PhysicalError::new(format!(
                    "physical aggregate glue {} field {field_index} layout disagrees with target authority",
                    glue.id.0
                )));
            }
            verify_value_recipe(module, field)?;
        }
    }
    for (index, glue) in module.variant_glue.iter().enumerate() {
        verify_variant_glue(module, index, glue)?;
    }
    verify_collection_glue_tables(module)?;
    for (index, callable) in module.callables.iter().enumerate() {
        if usize::try_from(callable.id.0).ok() != Some(index) {
            return Err(PhysicalError::new(format!(
                "physical callable {} is not at its canonical table index {index}",
                callable.id.0
            )));
        }
    }
    if let Some(entry) = module.entry_callable {
        if module
            .callables
            .get(entry.0 as usize)
            .is_none_or(|callable| callable.id != entry)
        {
            return Err(PhysicalError::new(format!(
                "physical entry callable {} is absent from the callable table",
                entry.0
            )));
        }
    }
    let mut function_ids = BTreeSet::new();
    for function in &module.functions {
        if !function_ids.insert(function.callable) {
            return Err(PhysicalError::new(format!(
                "physical callable {} has more than one body",
                function.callable.0
            )));
        }
        verify_physical_function(module, function)?;
    }
    Ok(())
}

fn verify_environment_glue(module: &PhysicalModule) -> Result<(), PhysicalError> {
    callable::verify_closures(module)?;
    let mut environment_types = BTreeSet::new();
    for glue in &module.environment_glue {
        if !environment_types.insert(&glue.ty) {
            return Err(PhysicalError::new("duplicate physical environment type"));
        }
        let captures = match &glue.ty {
            ResolvedTy::Closure { captures, .. } => captures.as_slice(),
            ResolvedTy::Function { .. } => &[],
            _ => {
                return Err(PhysicalError::new(
                    "environment recipe has no callable type",
                ))
            }
        };
        let facts = semantic_type_facts(module, &glue.ty)?;
        if !matches!(
            facts.clone,
            CloneKind::None | CloneKind::DeepCopy | CloneKind::FieldWise
        ) || glue.cloneable != (facts.clone != CloneKind::None)
            || captures.len() != glue.fields.len()
        {
            return Err(PhysicalError::new(
                "environment recipe differs from concrete callable facts",
            ));
        }
        let layout = module
            .target
            .environment_layout(&glue.ty)
            .ok_or_else(|| PhysicalError::new("environment recipe lacks a target layout"))?;
        if captures.is_empty() {
            if layout.size != 0 || layout.align != 1 {
                return Err(PhysicalError::new("empty callable environment has storage"));
            }
        } else {
            let PhysicalRepr::Struct(fields) = &layout.repr else {
                return Err(PhysicalError::new(
                    "captured environment lacks its mask and field struct",
                ));
            };
            if fields.len() != captures.len() + 1 {
                return Err(PhysicalError::new("environment field layout count differs"));
            }
            let PhysicalRepr::Array { element, len } = &fields[0].repr else {
                return Err(PhysicalError::new("environment mask is not a byte array"));
            };
            if element.repr != (PhysicalRepr::Integer { bits: 8 })
                || usize::try_from(*len).ok() != Some(captures.len().div_ceil(8))
                || fields[0].size != u64::from(*len)
            {
                return Err(PhysicalError::new(
                    "environment mask differs from logical capture count",
                ));
            }
            for (ty, layout) in captures.iter().zip(&fields[1..]) {
                if Some(layout) != module.target.layout(ty) {
                    return Err(PhysicalError::new(
                        "environment capture layout differs from target type",
                    ));
                }
            }
        }
        for (capture, recipe) in captures.iter().zip(&glue.fields) {
            if capture != &recipe.ty || (glue.cloneable && recipe.clone.is_none()) {
                return Err(PhysicalError::new(
                    "environment field lacks its exact copy contract",
                ));
            }
            verify_value_recipe(module, recipe)?;
        }
    }
    Ok(())
}

fn verify_collection_glue_tables(module: &PhysicalModule) -> Result<(), PhysicalError> {
    verify_environment_glue(module)?;
    let mut vector_types = BTreeSet::new();
    for (index, glue) in module.vector_glue.iter().enumerate() {
        if !vector_types.insert(&glue.ty) {
            return Err(PhysicalError::new(
                "physical vector type has more than one glue identity",
            ));
        }
        verify_vector_glue(module, index, glue)?;
    }
    let mut map_types = BTreeSet::new();
    for (index, glue) in module.map_glue.iter().enumerate() {
        if usize::try_from(glue.id.0).ok() != Some(index) || !map_types.insert(&glue.ty) {
            return Err(PhysicalError::new(
                "physical map glue has a noncanonical identity",
            ));
        }
        verify_collection_value_glue(
            module,
            &glue.ty,
            BuiltinType::HashMap,
            &[&glue.key, &glue.value],
        )?;
    }
    let mut set_types = BTreeSet::new();
    for (index, glue) in module.set_glue.iter().enumerate() {
        if usize::try_from(glue.id.0).ok() != Some(index) || !set_types.insert(&glue.ty) {
            return Err(PhysicalError::new(
                "physical set glue has a noncanonical identity",
            ));
        }
        verify_collection_value_glue(module, &glue.ty, BuiltinType::HashSet, &[&glue.element])?;
    }
    Ok(())
}

fn semantic_type_facts<'a>(
    module: &'a PhysicalModule,
    ty: &ResolvedTy,
) -> Result<&'a hew_types::TypeFacts, PhysicalError> {
    module
        .type_facts
        .get(&TypeInstanceKey(ty.clone()))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical value `{}` has no retained semantic type facts",
                ty.user_facing()
            ))
        })
}

fn verify_value_recipe(
    module: &PhysicalModule,
    field: &PhysicalValueRecipe,
) -> Result<(), PhysicalError> {
    let facts = semantic_type_facts(module, &field.ty)?;
    if field.own != OwnKind::of_class(facts.class)
        || field.clone.is_some() != (facts.clone != CloneKind::None)
        || field.destroy.is_some() != (field.own == OwnKind::Owned)
    {
        return Err(PhysicalError::new(format!(
            "physical field recipe for `{}` disagrees with semantic ownership or cloneability",
            field.ty.user_facing()
        )));
    }
    if let Some(action) = field.clone {
        verify_clone_action(module, &field.ty, field.own, action)?;
    }
    if let Some(action) = field.destroy {
        verify_destroy_action(module, &field.ty, field.own, action)?;
    }
    Ok(())
}

/// Check fixed-array allocation geometry against the target pointer width and
/// the runtime's signed length ABI. This is shared by target realization and
/// physical verification; source size is never inferred from a stack budget.
///
/// # Errors
/// Refuses an invalid element layout, overflowing allocation or unrepresentable length.
pub fn validate_array_allocation(
    length: u64,
    element: &PhysicalLayout,
    pointer_bytes: u64,
) -> Result<(), PhysicalError> {
    if !(1..=8).contains(&pointer_bytes)
        || element.align == 0
        || !element.align.is_power_of_two()
        || element.size > i64::MAX as u64
        || !element.size.is_multiple_of(u64::from(element.align))
    {
        return Err(PhysicalError::new(
            "fixed array has an invalid target element layout",
        ));
    }
    let bits = pointer_bytes * 8;
    let length_limit = if bits == 64 {
        i64::MAX as u64
    } else {
        (1u64 << bits) - 1
    };
    let allocation_limit = (1u64 << (bits - 1)) - 1;
    let padded_limit = allocation_limit.checked_sub(u64::from(element.align - 1));
    let bytes = length.checked_mul(element.size).filter(|bytes| {
        let allocation_bytes = if length == 0 { 0 } else { (*bytes).max(1) };
        padded_limit.is_some_and(|limit| allocation_bytes <= limit)
    });
    if length > length_limit || bytes.is_none() {
        return Err(PhysicalError::new(
            "fixed array exceeds the target allocation or runtime length range",
        ));
    }
    Ok(())
}

fn verify_vector_glue(
    module: &PhysicalModule,
    index: usize,
    glue: &PhysicalVectorGlue,
) -> Result<(), PhysicalError> {
    if usize::try_from(glue.id.0).ok() != Some(index) {
        return Err(PhysicalError::new(format!(
            "physical vector glue {} is not at its canonical table index {index}",
            glue.id.0
        )));
    }
    if sequence_element_type(&glue.ty) != Some(&glue.element.ty) {
        return Err(PhysicalError::new(
            "physical sequence descriptor disagrees with its exact element identity",
        ));
    }
    if let ResolvedTy::Array(_, length) = &glue.ty {
        let element_layout = required_layout(&module.target, &glue.element.ty)?;
        let carrier = required_layout(&module.target, &glue.ty)?;
        validate_array_allocation(*length, element_layout, carrier.size)?;
    }
    let vector_facts = semantic_type_facts(module, &glue.ty)?;
    if OwnKind::of_class(vector_facts.class) != OwnKind::Owned {
        return Err(PhysicalError::new(
            "physical vector has no semantic owning contract",
        ));
    }
    if required_layout(&module.target, &glue.ty)?.repr != PhysicalRepr::Pointer {
        return Err(PhysicalError::new(
            "physical vector descriptor requires its target pointer carrier",
        ));
    }
    let element_layout = required_layout(&module.target, &glue.element.ty)?;
    if !element_layout.align.is_power_of_two() {
        return Err(PhysicalError::new(
            "physical vector element has an invalid target alignment",
        ));
    }
    // Zero-sized elements retain their exact target size. The runtime owns
    // allocation bookkeeping; no payload byte is invented here.
    verify_value_recipe(module, &glue.element)?;
    if vector_facts.clone != CloneKind::None && glue.element.clone.is_none() {
        return Err(PhysicalError::new(
            "physical vector element has no clone action",
        ));
    }
    Ok(())
}

fn verify_collection_value_glue(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    kind: BuiltinType,
    recipes: &[&PhysicalValueRecipe],
) -> Result<(), PhysicalError> {
    let Some((actual, arguments)) = collection_type_arguments(ty) else {
        return Err(PhysicalError::new(
            "physical collection lacks a canonical type identity",
        ));
    };
    if actual != kind
        || arguments.len() != recipes.len()
        || !arguments
            .iter()
            .zip(recipes)
            .all(|(argument, recipe)| argument == &recipe.ty)
    {
        return Err(PhysicalError::new(
            "physical collection recipes disagree with type arguments",
        ));
    }
    let facts = semantic_type_facts(module, ty)?;
    // A map whose value has no clone has none itself; its own clone action is
    // required only where the semantics keep one.
    let clonable = matches!(facts.clone, CloneKind::DeepCopy | CloneKind::FieldWise);
    if OwnKind::of_class(facts.class) != OwnKind::Owned
        || (!clonable && facts.clone != CloneKind::None)
        || required_layout(&module.target, ty)?.repr != PhysicalRepr::Pointer
    {
        return Err(PhysicalError::new(
            "physical collection lacks an owning pointer copy contract",
        ));
    }
    for recipe in recipes {
        let layout = required_layout(&module.target, &recipe.ty)?;
        if !layout.align.is_power_of_two() || layout.size % u64::from(layout.align) != 0 {
            return Err(PhysicalError::new(
                "physical collection element has an invalid target layout",
            ));
        }
        verify_value_recipe(module, recipe)?;
        if clonable && recipe.clone.is_none() {
            return Err(PhysicalError::new(
                "physical collection element has no clone action",
            ));
        }
    }
    Ok(())
}

fn verify_variant_glue(
    module: &PhysicalModule,
    index: usize,
    glue: &PhysicalVariantGlue,
) -> Result<(), PhysicalError> {
    if usize::try_from(glue.id.0).ok() != Some(index) {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} is not at its canonical table index {index}",
            glue.id.0
        )));
    }
    if glue.own == OwnKind::Guaranteed {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} carries a borrow-only ownership class",
            glue.id.0
        )));
    }
    let layout = module.target.variant_layout(&glue.ty).ok_or_else(|| {
        PhysicalError::new(format!(
            "physical variant glue {} has no target variant layout",
            glue.id.0
        ))
    })?;
    let PhysicalRepr::Struct(object_fields) = &layout.object.repr else {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} has a non-struct object layout",
            glue.id.0
        )));
    };
    let expected_tag_bits = match glue.variants.len() {
        0..=256 => 8,
        257..=65_536 => 16,
        count => {
            return Err(PhysicalError::new(format!(
                "physical variant glue {} has unsupported case count {count}",
                glue.id.0
            )));
        }
    };
    if object_fields.len() != 2
        || object_fields[0].repr
            != (PhysicalRepr::Integer {
                bits: expected_tag_bits,
            })
        || !matches!(object_fields[1].repr, PhysicalRepr::Array { .. })
    {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} object layout lacks its exact tag and payload carriers",
            glue.id.0
        )));
    }
    // An indirect enum value is one pointer to its heap node; the node keeps
    // the tag-and-payload object layout. A direct enum value is that object.
    let value_matches = match module.target.layout(&glue.ty) {
        Some(value) if glue.is_indirect => value.repr == PhysicalRepr::Pointer,
        Some(value) => value == &layout.object,
        None => false,
    };
    if !value_matches
        || layout.is_indirect != glue.is_indirect
        || layout.variants.len() != glue.variants.len()
    {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} disagrees with target variant shape",
            glue.id.0
        )));
    }
    let payload_carrier = &object_fields[1];
    for (variant_index, (variant, variant_layout)) in
        glue.variants.iter().zip(&layout.variants).enumerate()
    {
        if payload_carrier.size < variant_layout.size
            || payload_carrier.align < variant_layout.align
        {
            return Err(PhysicalError::new(format!(
                "physical variant glue {} payload carrier cannot hold case {variant_index}",
                glue.id.0
            )));
        }
        let PhysicalRepr::Struct(layout_fields) = &variant_layout.repr else {
            return Err(PhysicalError::new(format!(
                "physical variant glue {} case {variant_index} has a non-struct payload layout",
                glue.id.0
            )));
        };
        if layout_fields.len() != variant.fields.len() {
            return Err(PhysicalError::new(format!(
                    "physical variant glue {} case {variant_index} recipe count disagrees with its payload layout",
                    glue.id.0
                )));
        }
        for (field_index, (field, layout_field)) in
            variant.fields.iter().zip(layout_fields).enumerate()
        {
            if field.own == OwnKind::Guaranteed
                || module.target.layout(&field.ty) != Some(layout_field)
            {
                return Err(PhysicalError::new(format!(
                        "physical variant glue {} case {variant_index} field {field_index} disagrees with target authority",
                        glue.id.0
                    )));
            }
            verify_value_recipe(module, field)?;
        }
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "keep per-function ABI, storage and cleanup contracts together"
)]
fn verify_physical_function(
    module: &PhysicalModule,
    function: &PhysicalFunction,
) -> Result<(), PhysicalError> {
    suspend::verify_task_scopes(function)?;
    let callable = module
        .callables
        .get(function.callable.0 as usize)
        .filter(|candidate| candidate.id == function.callable)
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical function has unknown callable {}",
                function.callable.0
            ))
        })?;
    if function.parameters.len() != callable.params.len() {
        return Err(PhysicalError::new(format!(
            "physical callable {} has {} parameter storage slots for {} ABI parameters",
            function.callable.0,
            function.parameters.len(),
            callable.params.len()
        )));
    }
    callable::verify_capture_slots(module, function)?;
    partial::verify_storage(module, function)?;
    for (index, storage) in function.storage.iter().enumerate() {
        if usize::try_from(storage.id.0).ok() != Some(index) {
            return Err(PhysicalError::new(format!(
                "physical storage {} is not at canonical index {index}",
                storage.id.0
            )));
        }
        if module.target.layout(&storage.ty) != Some(&storage.layout) {
            return Err(PhysicalError::new(format!(
                "physical storage {} layout disagrees with target authority",
                storage.id.0
            )));
        }
        if let Some(parent) = storage.borrow_parent {
            if storage.own != OwnKind::Guaranteed
                || parent == storage.id
                || function.storage.get(parent.0 as usize).is_none()
            {
                return Err(PhysicalError::new(
                    "physical loan storage has an invalid SIR parent dependency",
                ));
            }
        } else if storage.own == OwnKind::Guaranteed
            && !matches!(storage.origin, StorageOrigin::Parameter(_))
        {
            return Err(PhysicalError::new(
                "physical local loan storage has no SIR parent dependency",
            ));
        }
    }
    for (index, (parameter, abi)) in function.parameters.iter().zip(&callable.params).enumerate() {
        let slot = storage(function, *parameter)?;
        let expected_own = match abi.passing {
            hew_sir::SemParamPassing::ReadOnly => OwnKind::None,
            hew_sir::SemParamPassing::Borrow | hew_sir::SemParamPassing::BorrowMut => {
                OwnKind::Guaranteed
            }
            hew_sir::SemParamPassing::Consume => {
                OwnKind::of_param(&abi.ty, abi.passing, &module.type_facts)
                    .map_err(PhysicalError::new)?
            }
        };
        if abi.passing == hew_sir::SemParamPassing::BorrowMut
            && abi.carrier != ParamCarrier::Indirect
        {
            return Err(PhysicalError::new(
                "physical exclusive parameter requires caller storage by address",
            ));
        }
        if slot.ty != abi.ty || slot.own != expected_own {
            return Err(PhysicalError::new(format!(
                "physical callable {} parameter {index} disagrees with its ABI type or ownership",
                function.callable.0
            )));
        }
    }
    let block_ids = function
        .blocks
        .iter()
        .map(|block| block.id)
        .collect::<BTreeSet<_>>();
    if !block_ids.contains(&function.entry) {
        return Err(PhysicalError::new(format!(
            "physical function {} has no entry block {}",
            function.callable.0, function.entry.0
        )));
    }
    for block in &function.blocks {
        for operation in &block.ops {
            verify_operation_storage(module, function, operation)?;
        }
        verify_terminator(module, function, &block_ids, &block.terminator)?;
    }
    // Compute the suffix facts once, retaining any error. Initialization keeps
    // its existing diagnostic priority over stale physical cleanup sites.
    let needs_fault = partial::verify_trap_cleanup_refinement(function);
    verify_initialization(module, function, needs_fault.as_ref().ok())?;
    for block in &function.blocks {
        for (index, operation) in block.ops.iter().enumerate() {
            partial::verify_cleanup_site(function, operation, (block.id, index))?;
        }
    }
    needs_fault.map(|_| ())
}

fn storage(function: &PhysicalFunction, id: StorageId) -> Result<&PhysicalStorage, PhysicalError> {
    function
        .storage
        .get(id.0 as usize)
        .filter(|candidate| candidate.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))
}

fn require_same_storage_type(
    function: &PhysicalFunction,
    left: StorageId,
    right: StorageId,
    context: &str,
) -> Result<(), PhysicalError> {
    if storage(function, left)?.ty == storage(function, right)?.ty {
        Ok(())
    } else {
        Err(PhysicalError::new(format!(
            "{context} uses incompatible physical storage types"
        )))
    }
}

fn aggregate_glue(
    module: &PhysicalModule,
    id: PhysicalAggregateId,
) -> Result<&PhysicalAggregateGlue, PhysicalError> {
    module
        .aggregate_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical aggregate glue {}", id.0)))
}

fn variant_glue(
    module: &PhysicalModule,
    id: PhysicalVariantId,
) -> Result<&PhysicalVariantGlue, PhysicalError> {
    module
        .variant_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical variant glue {}", id.0)))
}

fn vector_glue(
    module: &PhysicalModule,
    id: PhysicalVectorId,
) -> Result<&PhysicalVectorGlue, PhysicalError> {
    module
        .vector_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical vector glue {}", id.0)))
}

fn map_glue(module: &PhysicalModule, id: PhysicalMapId) -> Result<&PhysicalMapGlue, PhysicalError> {
    module
        .map_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical map glue {}", id.0)))
}

fn set_glue(module: &PhysicalModule, id: PhysicalSetId) -> Result<&PhysicalSetGlue, PhysicalError> {
    module
        .set_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical set glue {}", id.0)))
}

fn verify_clone_action(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    own: OwnKind,
    action: CloneAction,
) -> Result<(), PhysicalError> {
    let facts = semantic_type_facts(module, ty)?;
    let clone_kind_matches = matches!(
        (facts.clone, action),
        (CloneKind::Bits, CloneAction::Bitwise)
            | (CloneKind::DeepCopy, CloneAction::Encoding(_))
            | (
                CloneKind::Retain,
                CloneAction::StringRetain | CloneAction::BytesRetain
            )
            | (
                CloneKind::FieldWise,
                CloneAction::Aggregate(_) | CloneAction::Variant(_)
            )
            | (
                CloneKind::DeepCopy | CloneKind::FieldWise,
                CloneAction::Callable
                    | CloneAction::Vector(_)
                    | CloneAction::Array(_)
                    | CloneAction::Map(_)
                    | CloneAction::Set(_)
            )
    );
    let valid = clone_kind_matches
        && own == OwnKind::of_class(facts.class)
        && match action {
            CloneAction::Encoding(format) => {
                own == OwnKind::Owned && encoding_format(ty) == Some(format)
            }
            CloneAction::Callable => {
                own == OwnKind::Owned
                    && matches!(ty,
                    ResolvedTy::Function { capabilities, .. } | ResolvedTy::Closure { capabilities, .. }
                    if capabilities.clone)
            }
            CloneAction::Bitwise => own == OwnKind::None,
            CloneAction::StringRetain => ty == &ResolvedTy::String && own == OwnKind::Owned,
            CloneAction::BytesRetain => ty == &ResolvedTy::Bytes && own == OwnKind::Owned,
            CloneAction::Aggregate(id) => {
                let glue = aggregate_glue(module, id)?;
                glue.ty == *ty
                    && glue.own == OwnKind::Owned
                    && own == OwnKind::Owned
                    && glue.fields.iter().all(|field| field.clone.is_some())
            }
            CloneAction::Vector(id) | CloneAction::Array(id) => {
                let glue = vector_glue(module, id)?;
                let fixed = matches!(ty, ResolvedTy::Array(_, _));
                fixed == matches!(action, CloneAction::Array(_))
                    && glue.ty == *ty
                    && own == OwnKind::Owned
                    && glue.element.clone.is_some()
            }
            CloneAction::Map(id) => {
                let glue = map_glue(module, id)?;
                glue.ty == *ty
                    && own == OwnKind::Owned
                    && glue.key.clone.is_some()
                    && glue.value.clone.is_some()
            }
            CloneAction::Set(id) => {
                let glue = set_glue(module, id)?;
                glue.ty == *ty && own == OwnKind::Owned && glue.element.clone.is_some()
            }
            CloneAction::Variant(id) => {
                let glue = variant_glue(module, id)?;
                glue.ty == *ty
                    && glue.own == OwnKind::Owned
                    && own == OwnKind::Owned
                    && glue
                        .variants
                        .iter()
                        .flat_map(|variant| &variant.fields)
                        .all(|field| field.clone.is_some())
            }
        };
    if valid {
        Ok(())
    } else {
        Err(PhysicalError::new(format!(
            "physical clone action {action:?} disagrees with `{}` storage",
            ty.user_facing()
        )))
    }
}

fn verify_destroy_action(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    own: OwnKind,
    action: DestroyAction,
) -> Result<(), PhysicalError> {
    let own_from_facts = OwnKind::of_class(semantic_type_facts(module, ty)?.class);
    let valid = own == own_from_facts
        && match action {
            DestroyAction::Resource(id) => {
                own == OwnKind::Owned
                    && module
                        .resources
                        .get(id.0 as usize)
                        .is_some_and(|resource| resource.ty == *ty)
            }
            DestroyAction::Encoding(format) => {
                own == OwnKind::Owned && encoding_format(ty) == Some(format)
            }
            DestroyAction::Callable => {
                own == OwnKind::Owned
                    && matches!(ty, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
            }
            DestroyAction::TraitObject => {
                own == OwnKind::Owned && matches!(ty, ResolvedTy::TraitObject { .. })
            }
            DestroyAction::StringRelease => ty == &ResolvedTy::String && own == OwnKind::Owned,
            DestroyAction::BytesRelease => ty == &ResolvedTy::Bytes && own == OwnKind::Owned,
            DestroyAction::Aggregate(id) => {
                let glue = aggregate_glue(module, id)?;
                glue.ty == *ty
                    && glue.own == OwnKind::Owned
                    && own == OwnKind::Owned
                    && glue
                        .fields
                        .iter()
                        .all(|field| field.own != OwnKind::Owned || field.destroy.is_some())
            }
            DestroyAction::Vector(id) | DestroyAction::Array(id) => {
                let glue = vector_glue(module, id)?;
                matches!(ty, ResolvedTy::Array(_, _)) == matches!(action, DestroyAction::Array(_))
                    && glue.ty == *ty
                    && own == OwnKind::Owned
                    && (glue.element.own != OwnKind::Owned || glue.element.destroy.is_some())
            }
            DestroyAction::Map(id) => {
                let glue = map_glue(module, id)?;
                glue.ty == *ty
                    && own == OwnKind::Owned
                    && [&glue.key, &glue.value]
                        .iter()
                        .all(|recipe| recipe.own != OwnKind::Owned || recipe.destroy.is_some())
            }
            DestroyAction::Set(id) => {
                let glue = set_glue(module, id)?;
                glue.ty == *ty
                    && own == OwnKind::Owned
                    && (glue.element.own != OwnKind::Owned || glue.element.destroy.is_some())
            }
            DestroyAction::Variant(id) => {
                let glue = variant_glue(module, id)?;
                glue.ty == *ty
                    && glue.own == OwnKind::Owned
                    && own == OwnKind::Owned
                    && glue
                        .variants
                        .iter()
                        .flat_map(|variant| &variant.fields)
                        .all(|field| field.own != OwnKind::Owned || field.destroy.is_some())
            }
        };
    if valid {
        Ok(())
    } else {
        Err(PhysicalError::new(format!(
            "physical destroy action {action:?} disagrees with `{}` storage",
            ty.user_facing()
        )))
    }
}

fn verify_aggregate_make(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    fields: &[StorageId],
    glue: PhysicalAggregateId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let recipe = aggregate_glue(module, glue)?;
    if destination.ty != recipe.ty || destination.own != recipe.own {
        return Err(PhysicalError::new(
            "physical aggregate construction destination disagrees with its glue recipe",
        ));
    }
    if fields.len() != recipe.fields.len() {
        return Err(PhysicalError::new(format!(
            "physical aggregate construction has {} fields for {} recipes",
            fields.len(),
            recipe.fields.len()
        )));
    }
    if fields.contains(&dest) {
        return Err(PhysicalError::new(
            "physical aggregate construction aliases its destination storage",
        ));
    }
    let mut consumed = BTreeSet::new();
    for (index, (field, expected)) in fields.iter().zip(&recipe.fields).enumerate() {
        let field = storage(function, *field)?;
        if field.ty != expected.ty || field.own != expected.own {
            return Err(PhysicalError::new(format!(
                "physical aggregate construction field {index} disagrees with its glue recipe"
            )));
        }
        if expected.own == OwnKind::Owned && !consumed.insert(field.id) {
            return Err(PhysicalError::new(format!(
                "physical aggregate construction consumes owned field {index} more than once"
            )));
        }
    }
    Ok(())
}

fn aggregate_projection_field<'a>(
    module: &'a PhysicalModule,
    function: &PhysicalFunction,
    aggregate: StorageId,
    field: u32,
    glue: PhysicalAggregateId,
) -> Result<&'a PhysicalValueRecipe, PhysicalError> {
    let aggregate = storage(function, aggregate)?;
    let recipe = aggregate_glue(module, glue)?;
    let source_own_matches = aggregate.own == recipe.own
        || (recipe.own == OwnKind::Owned && aggregate.own == OwnKind::Guaranteed);
    if aggregate.ty != recipe.ty || !source_own_matches {
        return Err(PhysicalError::new(
            "physical aggregate projection source disagrees with its glue recipe",
        ));
    }
    usize::try_from(field)
        .ok()
        .and_then(|index| recipe.fields.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical aggregate projection index {field} is out of bounds"
            ))
        })
}

fn verify_aggregate_project_copy(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    aggregate: StorageId,
    field: u32,
    glue: PhysicalAggregateId,
    action: CloneAction,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let expected = aggregate_projection_field(module, function, aggregate, field, glue)?;
    if destination.ty != expected.ty
        || destination.own != expected.own
        || expected.clone != Some(action)
    {
        return Err(PhysicalError::new(
            "physical aggregate projection disagrees with its field copy recipe",
        ));
    }
    verify_clone_action(module, &destination.ty, destination.own, action)
}

fn verify_borrow_dependency(
    function: &PhysicalFunction,
    dest: StorageId,
    source: StorageId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    if destination.own != OwnKind::Guaranteed
        || destination.borrow_parent != Some(source)
        || !matches!(
            storage(function, source)?.own,
            OwnKind::Owned | OwnKind::Guaranteed
        )
    {
        return Err(PhysicalError::new(
            "physical borrow disagrees with its SIR loan dependency",
        ));
    }
    Ok(())
}

fn verify_aggregate_project_borrow(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    aggregate: StorageId,
    field: u32,
    glue: PhysicalAggregateId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let expected = aggregate_projection_field(module, function, aggregate, field, glue)?;
    if destination.ty != expected.ty || expected.own != OwnKind::Owned {
        return Err(PhysicalError::new(
            "physical borrowed projection disagrees with its owning field recipe",
        ));
    }
    verify_borrow_dependency(function, dest, aggregate)
}

fn verify_aggregate_destructure(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    aggregate: StorageId,
    fields: &[StorageId],
    glue: PhysicalAggregateId,
) -> Result<(), PhysicalError> {
    let aggregate = storage(function, aggregate)?;
    let recipe = aggregate_glue(module, glue)?;
    if aggregate.ty != recipe.ty || aggregate.own != recipe.own {
        return Err(PhysicalError::new(
            "physical aggregate destructure source disagrees with its glue recipe",
        ));
    }
    if fields.len() != recipe.fields.len() {
        return Err(PhysicalError::new(format!(
            "physical aggregate destructure has {} results for {} fields",
            fields.len(),
            recipe.fields.len()
        )));
    }
    for (index, (field, expected)) in fields.iter().zip(&recipe.fields).enumerate() {
        let field = storage(function, *field)?;
        if field.ty != expected.ty || field.own != expected.own {
            return Err(PhysicalError::new(format!(
                "physical aggregate destructure field {index} disagrees with its glue recipe"
            )));
        }
    }
    Ok(())
}

/// The exact case recipe of one enum storage a probe reads or a take consumes.
/// A borrowed source may only carry an owning recipe.
fn variant_case<'a>(
    module: &'a PhysicalModule,
    function: &PhysicalFunction,
    source: StorageId,
    variant: u32,
    glue: PhysicalVariantId,
) -> Result<&'a PhysicalVariantCase, PhysicalError> {
    let source = storage(function, source)?;
    let recipe = variant_glue(module, glue)?;
    let source_own_matches = source.own == recipe.own
        || (recipe.own == OwnKind::Owned && source.own == OwnKind::Guaranteed);
    if source.ty != recipe.ty || !source_own_matches {
        return Err(PhysicalError::new(
            "physical variant source disagrees with its glue recipe",
        ));
    }
    usize::try_from(variant)
        .ok()
        .and_then(|index| recipe.variants.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!("physical variant tag {variant} is out of bounds"))
        })
}

fn variant_projection_field<'a>(
    module: &'a PhysicalModule,
    function: &PhysicalFunction,
    source: StorageId,
    variant: u32,
    field: u32,
    glue: PhysicalVariantId,
) -> Result<&'a PhysicalValueRecipe, PhysicalError> {
    let case = variant_case(module, function, source, variant, glue)?;
    usize::try_from(field)
        .ok()
        .and_then(|index| case.fields.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical variant projection index {field} is out of bounds"
            ))
        })
}

fn verify_variant_make(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    variant: u32,
    fields: &[StorageId],
    glue: PhysicalVariantId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let recipe = variant_glue(module, glue)?;
    if destination.ty != recipe.ty || destination.own != recipe.own {
        return Err(PhysicalError::new(
            "physical variant construction destination disagrees with its glue recipe",
        ));
    }
    let case = usize::try_from(variant)
        .ok()
        .and_then(|variant| recipe.variants.get(variant))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical variant construction tag {variant} is out of bounds"
            ))
        })?;
    if fields.len() != case.fields.len() {
        return Err(PhysicalError::new(format!(
            "physical variant construction has {} fields for {} recipes",
            fields.len(),
            case.fields.len()
        )));
    }
    if fields.contains(&dest) {
        return Err(PhysicalError::new(
            "physical variant construction aliases its destination storage",
        ));
    }
    let mut consumed = BTreeSet::new();
    for (index, (field, expected)) in fields.iter().zip(&case.fields).enumerate() {
        let field = storage(function, *field)?;
        if field.ty != expected.ty || field.own != expected.own {
            return Err(PhysicalError::new(format!(
                "physical variant construction field {index} disagrees with its glue recipe"
            )));
        }
        if expected.own == OwnKind::Owned && !consumed.insert(field.id) {
            return Err(PhysicalError::new(format!(
                "physical variant construction consumes owned field {index} more than once"
            )));
        }
    }
    Ok(())
}

fn verify_tuple_make(
    function: &PhysicalFunction,
    dest: StorageId,
    elements: &[StorageId],
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let ResolvedTy::Tuple(field_types) = &destination.ty else {
        return Err(PhysicalError::new(
            "physical tuple construction has a non-tuple destination",
        ));
    };
    if destination.own != OwnKind::None {
        return Err(PhysicalError::new(
            "physical tuple construction is limited to no-drop values",
        ));
    }
    if field_types.len() != elements.len() {
        return Err(PhysicalError::new(format!(
            "physical tuple construction has {} elements for {} fields",
            elements.len(),
            field_types.len()
        )));
    }
    for (index, (element, expected)) in elements.iter().zip(field_types).enumerate() {
        let element = storage(function, *element)?;
        if element.own != OwnKind::None || &element.ty != expected {
            return Err(PhysicalError::new(format!(
                "physical tuple element {index} disagrees with its no-drop field type"
            )));
        }
    }
    Ok(())
}

fn verify_tuple_get(
    function: &PhysicalFunction,
    dest: StorageId,
    tuple: StorageId,
    index: u32,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let tuple = storage(function, tuple)?;
    let ResolvedTy::Tuple(field_types) = &tuple.ty else {
        return Err(PhysicalError::new(
            "physical tuple projection reads a non-tuple value",
        ));
    };
    let field = usize::try_from(index)
        .ok()
        .and_then(|index| field_types.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical tuple projection index {index} is out of bounds"
            ))
        })?;
    if tuple.own != OwnKind::None || destination.own != OwnKind::None || &destination.ty != field {
        return Err(PhysicalError::new(
            "physical tuple projection disagrees with its no-drop field type",
        ));
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "keep the exhaustive physical operation contract together"
)]
fn verify_operation_storage(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    operation: &PhysicalOp,
) -> Result<(), PhysicalError> {
    match operation {
        PhysicalOp::GeneratorMake { .. } => generators::verify_make(module, function, operation)?,
        PhysicalOp::StreamPipe {
            capacity,
            stream,
            sink,
            element,
        } => {
            let stream = storage(function, *stream)?;
            let sink = storage(function, *sink)?;
            if *capacity == 0
                || stream.own != OwnKind::Owned
                || sink.own != OwnKind::Owned
                || hew_sir::pipe_parts(&stream.ty, &sink.ty) != Some(&element.ty)
            {
                return Err(PhysicalError::new(
                    "stream pipe halves disagree with their owned element contract",
                ));
            }
            verify_value_recipe(module, element)?;
        }
        PhysicalOp::TaskScopeEnter { duration, .. } => {
            if let Some(duration) = duration {
                if storage(function, *duration)?.ty != ResolvedTy::Duration {
                    return Err(PhysicalError::new(
                        "scope deadline requires Duration storage",
                    ));
                }
            }
        }
        PhysicalOp::TaskScopeClose { .. } => {}
        PhysicalOp::TaskSpawn {
            callable,
            dest,
            output,
            ..
        } => {
            let input = storage(function, *callable)?;
            let result = storage(function, *dest)?;
            let (params, ret, capabilities) =
                hew_sir::callable_parts(&input.ty).map_err(PhysicalError::new)?;
            if !params.is_empty()
                || capabilities.call != hew_types::CallableCallMode::Once
                || result.ty != ResolvedTy::Task(Box::new(ret.clone()))
            {
                return Err(PhysicalError::new(
                    "task spawn disagrees with its callable/result contract",
                ));
            }
            match output {
                Some(output) if &output.ty == ret && *ret != ResolvedTy::Never => {
                    verify_value_recipe(module, output)?;
                }
                None if *ret == ResolvedTy::Never => {}
                _ => {
                    return Err(PhysicalError::new(
                        "task result recipe differs from its callable result",
                    ))
                }
            }
        }
        PhysicalOp::RegisterDefer { dependencies, .. } => {
            for dependency in dependencies {
                storage(function, *dependency)?;
            }
        }
        operation @ (PhysicalOp::FunctionMake { .. }
        | PhysicalOp::ClosureMake { .. }
        | PhysicalOp::DynMake { .. }
        | PhysicalOp::CallableCoerce { .. }) => {
            callable::verify_operation(module, function, operation)?;
        }
        PhysicalOp::Const { dest, value } => {
            verify_constant(module, function, *dest, value)?;
        }
        PhysicalOp::StorageLive { storage: dest } => {
            partial::require_local(function, *dest)?;
        }
        PhysicalOp::Unary { dest, source, .. } => {
            require_same_storage_type(function, *dest, *source, "physical operation")?;
        }
        PhysicalOp::Transfer { dest, source } => {
            verify_transfer(function, *dest, *source)?;
        }
        PhysicalOp::Borrow { dest, source } => {
            verify_whole_value_borrow(module, function, *dest, *source)?;
        }
        PhysicalOp::Cast { dest, source, .. } => {
            storage(function, *dest)?;
            storage(function, *source)?;
        }
        PhysicalOp::TupleMake { dest, elements } => verify_tuple_make(function, *dest, elements)?,
        PhysicalOp::TupleGet { dest, tuple, index } => {
            verify_tuple_get(function, *dest, *tuple, *index)?;
        }
        PhysicalOp::ArrayMake { dest, fields, glue } => {
            let descriptor = vector_glue(module, *glue)?;
            let ResolvedTy::Array(element, length) = &descriptor.ty else {
                return Err(PhysicalError::new("array.make has a non-array descriptor"));
            };
            if storage(function, *dest)?.ty != descriptor.ty
                || storage(function, *dest)?.own != OwnKind::Owned
                || fields.contains(dest)
                || usize::try_from(*length).ok() != Some(fields.len())
            {
                return Err(PhysicalError::new(
                    "array.make length or result differs from its descriptor",
                ));
            }
            let mut consumed = BTreeSet::new();
            for field in fields {
                let field = storage(function, *field)?;
                if field.ty != **element
                    || field.own != descriptor.element.own
                    || (field.own == OwnKind::Owned && !consumed.insert(field.id))
                {
                    return Err(PhysicalError::new(
                        "array.make element differs from its descriptor",
                    ));
                }
            }
        }
        PhysicalOp::ArrayRepeat { dest, seed, glue } => {
            let descriptor = vector_glue(module, *glue)?;
            let ResolvedTy::Array(element, length) = &descriptor.ty else {
                return Err(PhysicalError::new(
                    "array.repeat has a non-array descriptor",
                ));
            };
            if storage(function, *dest)?.ty != descriptor.ty
                || storage(function, *dest)?.own != OwnKind::Owned
                || dest == seed
                || storage(function, *seed)?.ty != **element
                || storage(function, *seed)?.own != descriptor.element.own
                || *length == 0
                || (*length > 1 && descriptor.element.clone.is_none())
            {
                return Err(PhysicalError::new(
                    "array.repeat seed, length or copy recipe differs from its descriptor",
                ));
            }
        }
        PhysicalOp::AggregateMake { dest, fields, glue } => {
            verify_aggregate_make(module, function, *dest, fields, *glue)?;
        }
        PhysicalOp::AggregateProjectCopy {
            dest,
            aggregate,
            field,
            glue,
            action,
        } => verify_aggregate_project_copy(
            module, function, *dest, *aggregate, *field, *glue, *action,
        )?,
        PhysicalOp::AggregateProjectBorrow {
            dest,
            aggregate,
            field,
            glue,
        } => verify_aggregate_project_borrow(module, function, *dest, *aggregate, *field, *glue)?,
        PhysicalOp::AggregateDestructure {
            aggregate,
            fields,
            glue,
        } => verify_aggregate_destructure(module, function, *aggregate, fields, *glue)?,
        PhysicalOp::VariantMake {
            dest,
            variant,
            fields,
            glue,
        } => verify_variant_make(module, function, *dest, *variant, fields, *glue)?,
        PhysicalOp::VariantIs {
            dest,
            source,
            variant,
            glue,
        } => {
            variant_case(module, function, *source, *variant, *glue)?;
            if storage(function, *dest)?.ty != ResolvedTy::Bool {
                return Err(PhysicalError::new(
                    "physical variant test must produce a boolean",
                ));
            }
        }
        PhysicalOp::VariantProjectCopy {
            dest,
            source,
            variant,
            field,
            glue,
            action,
        } => {
            let destination = storage(function, *dest)?;
            let expected =
                variant_projection_field(module, function, *source, *variant, *field, *glue)?;
            if destination.ty != expected.ty
                || destination.own != expected.own
                || expected.clone != Some(*action)
            {
                return Err(PhysicalError::new(
                    "physical variant projection disagrees with its field copy recipe",
                ));
            }
            verify_clone_action(module, &destination.ty, destination.own, *action)?;
        }
        PhysicalOp::VariantProjectBorrow {
            dest,
            source,
            variant,
            field,
            glue,
        } => {
            let destination = storage(function, *dest)?;
            let expected =
                variant_projection_field(module, function, *source, *variant, *field, *glue)?;
            if destination.ty != expected.ty || expected.own != OwnKind::Owned {
                return Err(PhysicalError::new(
                    "physical borrowed variant projection disagrees with its owning field recipe",
                ));
            }
            verify_borrow_dependency(function, *dest, *source)?;
        }
        PhysicalOp::VariantDestructure {
            source,
            variant,
            fields,
            glue,
        } => {
            let case = variant_case(module, function, *source, *variant, *glue)?;
            if fields.len() != case.fields.len() {
                return Err(PhysicalError::new(format!(
                    "physical variant destructure has {} results for {} fields",
                    fields.len(),
                    case.fields.len()
                )));
            }
            for (index, (field, expected)) in fields.iter().zip(&case.fields).enumerate() {
                let field = storage(function, *field)?;
                if field.ty != expected.ty || field.own != expected.own {
                    return Err(PhysicalError::new(format!(
                        "physical variant destructure field {index} disagrees with its glue recipe"
                    )));
                }
            }
        }
        PhysicalOp::Binary { dest, op, lhs, rhs } => {
            verify_binary(function, *dest, *op, *lhs, *rhs)?;
        }
        PhysicalOp::Destroy { source, action, .. } => {
            let source = storage(function, *source)?;
            verify_destroy_action(module, &source.ty, source.own, *action)?;
        }
        PhysicalOp::EndBorrow { source } => {
            let source = storage(function, *source)?;
            if source.own != OwnKind::Guaranteed || source.borrow_parent.is_none() {
                return Err(PhysicalError::new(
                    "physical end-borrow requires a local SIR loan",
                ));
            }
        }
        PhysicalOp::Clone {
            dest,
            source,
            action,
        } => {
            require_same_storage_type(function, *dest, *source, "physical clone")?;
            let destination = storage(function, *dest)?;
            verify_clone_action(module, &destination.ty, destination.own, *action)?;
        }
        PhysicalOp::Assign {
            dest,
            source,
            destroy_old,
        } => {
            require_same_storage_type(function, *dest, *source, "physical assignment")?;
            let destination = storage(function, *dest)?;
            partial::verify_optional_destroy(module, destination, *destroy_old)?;
        }
        PhysicalOp::StorageDead {
            storage: id,
            destroy,
            ..
        } => {
            partial::require_local(function, *id)?;
            partial::verify_optional_destroy(module, storage(function, *id)?, *destroy)?;
        }
    }
    Ok(())
}

fn verify_transfer(
    function: &PhysicalFunction,
    dest: StorageId,
    source: StorageId,
) -> Result<(), PhysicalError> {
    require_same_storage_type(function, dest, source, "physical transfer")?;
    let source = storage(function, source)?;
    let dest = storage(function, dest)?;
    if source.own != dest.own || source.own == OwnKind::Guaranteed {
        return Err(PhysicalError::new(
            "physical transfer cannot change ownership or move a loan",
        ));
    }
    Ok(())
}

fn verify_whole_value_borrow(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    source: StorageId,
) -> Result<(), PhysicalError> {
    require_same_storage_type(function, dest, source, "physical borrow")?;
    let ty = &storage(function, source)?.ty;
    if OwnKind::of_class(semantic_type_facts(module, ty)?.class) != OwnKind::Owned {
        return Err(PhysicalError::new(
            "physical borrow requires an owning value",
        ));
    }
    verify_borrow_dependency(function, dest, source)
}

/// Width in bits of an integer destination layout, or `None` when the layout
/// is not an integer.
fn integer_width_bits(layout: &PhysicalLayout) -> Option<u16> {
    match layout.repr {
        PhysicalRepr::Integer { bits } => Some(bits),
        _ => None,
    }
}

/// Inclusive mathematical range a `width`-bit integer destination admits.
fn integer_bit_range(width: u16, signed: bool) -> (i128, i128) {
    if signed {
        let magnitude = 1i128 << (width - 1);
        (-magnitude, magnitude - 1)
    } else {
        (0, (1i128 << width) - 1)
    }
}

/// Keep only the low `width` bits of a bit pattern.
fn mask_to_width(bits: u64, width: u16) -> u64 {
    if width >= 64 {
        bits
    } else {
        bits & ((1u64 << width) - 1)
    }
}

/// True when no bit above the destination width is set. A constant that
/// carries stray high bits is malformed IR: the backend emits the pattern as
/// written and would silently produce a different value.
fn canonical_integer_bits(bits: u64, width: u16) -> bool {
    mask_to_width(bits, width) == bits
}

fn verify_constant(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    value: &PhysicalConst,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let matches_destination = match value {
        PhysicalConst::IntegerBits(bits) => {
            destination.ty.is_integer()
                && destination.own == OwnKind::None
                && integer_width_bits(&destination.layout)
                    .is_some_and(|width| canonical_integer_bits(*bits, width))
        }
        PhysicalConst::Bool(_) => {
            destination.ty == ResolvedTy::Bool && destination.own == OwnKind::None
        }
        PhysicalConst::Float(_) => destination.ty.is_float() && destination.own == OwnKind::None,
        PhysicalConst::Char(_) => {
            destination.ty == ResolvedTy::Char && destination.own == OwnKind::None
        }
        PhysicalConst::Unit => {
            destination.ty == ResolvedTy::Unit && destination.own == OwnKind::None
        }
        PhysicalConst::Duration(_) => {
            destination.ty == ResolvedTy::Duration && destination.own == OwnKind::None
        }
        PhysicalConst::String(id) => {
            if !module.string_literals.contains_key(id) {
                return Err(PhysicalError::new(format!(
                    "physical string constant references unknown literal {}",
                    id.0
                )));
            }
            destination.ty == ResolvedTy::String && destination.own == OwnKind::Owned
        }
        PhysicalConst::Bytes(id) => {
            if !module.bytes_literals.contains_key(id) {
                return Err(PhysicalError::new(format!(
                    "physical bytes constant references unknown literal {}",
                    id.0
                )));
            }
            destination.ty == ResolvedTy::Bytes && destination.own == OwnKind::Owned
        }
    };
    if !matches_destination {
        return Err(PhysicalError::new(
            "physical constant payload disagrees with destination type or ownership",
        ));
    }
    Ok(())
}

fn verify_binary(
    function: &PhysicalFunction,
    dest: StorageId,
    op: BinaryOp,
    lhs: StorageId,
    rhs: StorageId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let left = storage(function, lhs)?;
    let right = storage(function, rhs)?;
    if left.ty != right.ty
        || left.own != OwnKind::None
        || right.own != OwnKind::None
        || destination.own != OwnKind::None
    {
        return Err(PhysicalError::new(
            "physical binary operation uses incompatible storage types or ownership",
        ));
    }
    let valid_result = match op {
        BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::Less
        | BinaryOp::LessEqual
        | BinaryOp::Greater
        | BinaryOp::GreaterEqual => destination.ty == ResolvedTy::Bool,
        BinaryOp::And | BinaryOp::Or | BinaryOp::Range | BinaryOp::RangeInclusive => false,
        BinaryOp::Add
        | BinaryOp::Subtract
        | BinaryOp::Multiply
        | BinaryOp::Divide
        | BinaryOp::Modulo
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::WrappingAdd
        | BinaryOp::WrappingSub
        | BinaryOp::WrappingMul => destination.ty == left.ty,
    };
    if !valid_result {
        return Err(PhysicalError::new(
            "physical binary result type disagrees with its operation",
        ));
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InitState {
    Uninitialized,
    Initialized,
    MaybeInitialized,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FlowState {
    slots: Vec<InitState>,
    active: Vec<InitState>,
    fault: FaultState,
    exit: u8,
    defers: defer::State,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FaultState {
    None,
    Active,
    MaybeActive,
}

fn verify_initialization(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    cleanup_needs_fault: Option<&BTreeSet<BlockId>>,
) -> Result<(), PhysicalError> {
    let defer_plan = defer::verify_regions(function)?;
    defer::verify_calls(module, function, &defer_plan)?;
    let blocks = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<BTreeMap<_, _>>();
    let mut entry = FlowState {
        slots: vec![InitState::Uninitialized; function.storage.len()],
        active: vec![InitState::Uninitialized; function.storage.len()],
        fault: FaultState::None,
        exit: defer::ORDINARY,
        defers: defer::State::default(),
    };
    for parameter in &function.parameters {
        *entry.slots.get_mut(parameter.0 as usize).ok_or_else(|| {
            PhysicalError::new(format!("unknown physical storage {}", parameter.0))
        })? = InitState::Initialized;
        partial::set_leaves(function, &mut entry, *parameter, InitState::Initialized);
    }

    for slot in &function.storage {
        if matches!(
            slot.origin,
            StorageOrigin::Capture { .. }
                | StorageOrigin::ActorState {
                    initialized: true,
                    ..
                }
        ) {
            entry.slots[slot.id.0 as usize] = InitState::Initialized;
        }
    }
    let mut incoming = BTreeMap::from([(function.entry, vec![entry])]);
    let mut pending = vec![function.entry];
    while let Some(block_id) = pending.pop() {
        let block = blocks.get(&block_id).ok_or_else(|| {
            PhysicalError::new(format!("physical CFG has no block {}", block_id.0))
        })?;
        for mut state in incoming[&block_id].clone() {
            defer::verify_entry_phase(&defer_plan, block_id, &state)?;
            for operation in &block.ops {
                if cleanup_needs_fault.is_some_and(|blocks| blocks.contains(&block_id))
                    && (state.exit == 0 || state.exit & defer::ORDINARY != 0)
                    && matches!(operation, PhysicalOp::Destroy { cleanup, .. } | PhysicalOp::StorageDead { cleanup, .. }
                        if cleanup.mode() == hew_sir::CleanupMode::Trap)
                {
                    return Err(PhysicalError::new(
                        "physical trap-only cleanup lost its fault exit cause",
                    ));
                }
                apply_operation(module, function, operation, &mut state, block_id)?;
            }
            for (target, successor) in
                terminator_successors(function, &block.terminator, state, block_id, &defer_plan)?
            {
                let alternatives = incoming.entry(target).or_default();
                if alternatives
                    .iter()
                    .any(|old| !old.defers.same_phase(&successor.defers))
                {
                    return Err(PhysicalError::new(
                        "physical CFG joins incompatible defer phases",
                    ));
                }
                let changed = if let Some(existing) = alternatives.iter_mut().find(|old| {
                    old.fault == successor.fault
                        && old.exit == successor.exit
                        && old.defers.same_faults(&successor.defers)
                }) {
                    merge_flow(existing, &successor)
                } else {
                    alternatives.push(successor);
                    true
                };
                if changed {
                    pending.push(target);
                }
            }
        }
    }
    Ok(())
}

fn initialized(
    function: &PhysicalFunction,
    state: &FlowState,
    id: StorageId,
    block: BlockId,
    context: &str,
) -> Result<(), PhysicalError> {
    if let Some(entry) = function.place_storage.get(&id) {
        partial::require_root(function, state, id, block, context)?;
        for leaf in &entry.leaves {
            initialized_slot(state, leaf.storage, block, context)?;
        }
        Ok(())
    } else {
        initialized_slot(state, id, block, context)
    }
}

fn initialized_slot(
    state: &FlowState,
    id: StorageId,
    block: BlockId,
    context: &str,
) -> Result<(), PhysicalError> {
    let slot = state
        .slots
        .get(id.0 as usize)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))?;
    match slot {
        InitState::Initialized => Ok(()),
        InitState::Uninitialized => Err(PhysicalError::new(format!(
            "physical bb{} {context} reads uninitialized storage {}",
            block.0, id.0
        ))),
        InitState::MaybeInitialized => Err(PhysicalError::new(format!(
            "physical bb{} {context} reads storage {} that is not initialized on every path",
            block.0, id.0
        ))),
    }
}

fn define(
    function: &PhysicalFunction,
    state: &mut FlowState,
    id: StorageId,
    block: BlockId,
    context: &str,
) -> Result<(), PhysicalError> {
    require_no_live_borrows(function, state, id)?;
    let own = storage(function, id)?.own;
    if let Some(entry) = function.place_storage.get(&id).filter(|entry| {
        entry.root != id
            || matches!(
                function.storage[id.0 as usize].origin,
                StorageOrigin::Local(_)
            )
    }) {
        partial::require_root(function, state, id, block, context)?;
        if entry
            .leaves
            .iter()
            .any(|leaf| state.slots[leaf.storage.0 as usize] != InitState::Uninitialized)
        {
            return Err(PhysicalError::new(format!(
                "physical bb{} {context} overwrites initialized aggregate contents {}",
                block.0, id.0
            )));
        }
        partial::set_leaves(function, state, id, InitState::Initialized);
        return Ok(());
    }
    let slot = state
        .slots
        .get_mut(id.0 as usize)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))?;
    match (*slot, own) {
        (InitState::Uninitialized, _) | (_, OwnKind::None) => {}
        (InitState::Initialized, OwnKind::Owned | OwnKind::Guaranteed) => {
            return Err(PhysicalError::new(format!(
                "physical bb{} {context} overwrites initialized storage {}",
                block.0, id.0
            )));
        }
        (InitState::MaybeInitialized, OwnKind::Owned | OwnKind::Guaranteed) => {
            return Err(PhysicalError::new(format!(
                "physical bb{} {context} may overwrite a live obligation in storage {}",
                block.0, id.0
            )));
        }
    }
    *slot = InitState::Initialized;
    partial::set_leaves(function, state, id, InitState::Initialized);
    Ok(())
}

fn require_no_live_borrows(
    function: &PhysicalFunction,
    state: &FlowState,
    source: StorageId,
) -> Result<(), PhysicalError> {
    let source = function
        .place_storage
        .get(&source)
        .map_or(source, |entry| entry.root);
    if function.storage.iter().any(|slot| {
        slot.borrow_parent
            .is_some_and(|parent| callable::depends_on(function, parent, source))
            && state.slots[slot.id.0 as usize] != InitState::Uninitialized
    }) {
        return Err(PhysicalError::new(format!(
            "physical storage {} cannot end or change while a dependent loan is live",
            source.0
        )));
    }
    Ok(())
}

fn consume_if_owned(
    function: &PhysicalFunction,
    state: &mut FlowState,
    id: StorageId,
) -> Result<(), PhysicalError> {
    defer::require_unreserved(function, state, id)?;
    if let Some(entry) = function.place_storage.get(&id) {
        require_no_live_borrows(function, state, id)?;
        if entry.root == id {
            state.slots[id.0 as usize] = InitState::Uninitialized;
        }
        partial::set_leaves(function, state, id, InitState::Uninitialized);
        return Ok(());
    }
    if storage(function, id)?.own == OwnKind::Owned
        || matches!(
            storage(function, id)?.origin,
            StorageOrigin::Capture { .. } | StorageOrigin::ActorState { .. }
        )
    {
        require_no_live_borrows(function, state, id)?;
        *state
            .slots
            .get_mut(id.0 as usize)
            .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))? =
            InitState::Uninitialized;
        callable::invalidate_captures(function, state, id);
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "keep physical content and storage transitions in one exhaustive match"
)]
fn apply_operation(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    operation: &PhysicalOp,
    state: &mut FlowState,
    block: BlockId,
) -> Result<(), PhysicalError> {
    match operation {
        PhysicalOp::RegisterDefer {
            defer,
            scope,
            dependencies,
        } => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "physical registration would abandon an active fault",
                ));
            }
            for dependency in dependencies {
                initialized(function, state, *dependency, block, "defer dependency")?;
            }
            state.defers.register(*defer, *scope, dependencies)?;
        }
        PhysicalOp::TaskScopeEnter { duration, .. } => {
            if let Some(duration) = duration {
                initialized(function, state, *duration, block, "scope deadline")?;
            }
        }
        PhysicalOp::TaskScopeClose { .. } => {}
        PhysicalOp::GeneratorMake { callable, dest, .. }
        | PhysicalOp::TaskSpawn { callable, dest, .. } => {
            initialized(function, state, *callable, block, "task callable")?;
            consume_if_owned(function, state, *callable)?;
            define(function, state, *dest, block, "task handle")?;
        }
        PhysicalOp::StreamPipe { stream, sink, .. } => {
            define(function, state, *stream, block, "stream half")?;
            define(function, state, *sink, block, "sink half")?;
        }
        PhysicalOp::FunctionMake { dest, .. } | PhysicalOp::Const { dest, .. } => {
            define(function, state, *dest, block, "constant")?;
        }
        PhysicalOp::StorageLive { storage: dest } => {
            partial::activate(function, state, *dest, block)?;
        }
        PhysicalOp::Unary { dest, source, .. } | PhysicalOp::Cast { dest, source, .. } => {
            initialized(function, state, *source, block, "operation")?;
            define(function, state, *dest, block, "operation")?;
        }
        PhysicalOp::TupleMake { dest, elements } => {
            for element in elements {
                initialized(function, state, *element, block, "tuple construction")?;
            }
            define(function, state, *dest, block, "tuple construction")?;
        }
        PhysicalOp::TupleGet { dest, tuple, .. } => {
            initialized(function, state, *tuple, block, "tuple projection")?;
            define(function, state, *dest, block, "tuple projection")?;
        }
        PhysicalOp::AggregateMake { dest, fields, .. }
        | PhysicalOp::ArrayMake { dest, fields, .. }
        | PhysicalOp::ClosureMake { dest, fields, .. } => {
            for field in fields {
                initialized(function, state, *field, block, "aggregate construction")?;
            }
            define(function, state, *dest, block, "aggregate construction")?;
            for field in fields {
                consume_if_owned(function, state, *field)?;
            }
        }
        PhysicalOp::ArrayRepeat { dest, seed, .. } => {
            initialized(function, state, *seed, block, "array repeat seed")?;
            define(function, state, *dest, block, "array repeat")?;
            consume_if_owned(function, state, *seed)?;
        }
        PhysicalOp::VariantMake { dest, fields, .. } => {
            for field in fields {
                initialized(function, state, *field, block, "variant construction")?;
            }
            define(function, state, *dest, block, "variant construction")?;
            for field in fields {
                consume_if_owned(function, state, *field)?;
            }
        }
        PhysicalOp::AggregateProjectCopy {
            dest, aggregate, ..
        }
        | PhysicalOp::AggregateProjectBorrow {
            dest, aggregate, ..
        }
        | PhysicalOp::VariantIs {
            dest,
            source: aggregate,
            ..
        }
        | PhysicalOp::VariantProjectCopy {
            dest,
            source: aggregate,
            ..
        }
        | PhysicalOp::VariantProjectBorrow {
            dest,
            source: aggregate,
            ..
        } => {
            initialized(function, state, *aggregate, block, "aggregate projection")?;
            define(function, state, *dest, block, "aggregate projection")?;
        }
        PhysicalOp::VariantDestructure { source, fields, .. } => {
            initialized(function, state, *source, block, "variant destructure")?;
            for field in fields {
                define(function, state, *field, block, "variant destructure")?;
            }
            consume_if_owned(function, state, *source)?;
        }
        PhysicalOp::AggregateDestructure {
            aggregate, fields, ..
        } => {
            initialized(function, state, *aggregate, block, "aggregate destructure")?;
            for field in fields {
                define(function, state, *field, block, "aggregate destructure")?;
            }
            consume_if_owned(function, state, *aggregate)?;
        }
        PhysicalOp::Binary { dest, lhs, rhs, .. } => {
            initialized(function, state, *lhs, block, "binary operation")?;
            initialized(function, state, *rhs, block, "binary operation")?;
            define(function, state, *dest, block, "binary operation")?;
        }
        PhysicalOp::Transfer { dest, source }
        | PhysicalOp::CallableCoerce { dest, source }
        | PhysicalOp::DynMake { dest, source, .. } => {
            initialized(function, state, *source, block, "transfer")?;
            if dest != source {
                define(function, state, *dest, block, "transfer")?;
                consume_if_owned(function, state, *source)?;
            }
        }
        PhysicalOp::Clone { dest, source, .. } | PhysicalOp::Borrow { dest, source } => {
            initialized(function, state, *source, block, "copy or borrow")?;
            define(function, state, *dest, block, "copy or borrow")?;
        }
        PhysicalOp::Destroy {
            source, cleanup, ..
        } => {
            defer::require_unreserved(function, state, *source)?;
            partial::require_root(function, state, *source, block, "destroy")?;
            partial::require_droppable(module, function, state, *source, cleanup.mode())?;
            require_no_live_borrows(function, state, *source)?;
            invalidate_storage(function, state, *source);
        }
        PhysicalOp::EndBorrow { source } => {
            initialized(function, state, *source, block, "end-borrow")?;
            require_no_live_borrows(function, state, *source)?;
            invalidate_storage(function, state, *source);
        }
        PhysicalOp::Assign { dest, source, .. } => {
            partial::require_root(function, state, *dest, block, "assignment destination")?;
            initialized(function, state, *source, block, "assignment source")?;
            partial::require_droppable(
                module,
                function,
                state,
                *dest,
                hew_sir::CleanupMode::Ordinary,
            )?;
            require_no_live_borrows(function, state, *dest)?;
            consume_if_owned(function, state, *source)?;
            partial::set_leaves(function, state, *dest, InitState::Initialized);
        }
        PhysicalOp::StorageDead {
            storage: id,
            cleanup,
            ..
        } => {
            defer::require_unreserved(function, state, *id)?;
            partial::require_root(function, state, *id, block, "end-lifetime")?;
            partial::require_droppable(module, function, state, *id, cleanup.mode())?;
            require_no_live_borrows(function, state, *id)?;
            partial::set_leaves(function, state, *id, InitState::Uninitialized);
            if matches!(
                storage(function, *id)?.origin,
                StorageOrigin::ActorState {
                    initialized: false,
                    ..
                }
            ) {
                // A deferred actor seat (D447) has no partition of its own:
                // ending its lifetime releases the seat itself.
                state.slots[id.0 as usize] = InitState::Uninitialized;
            }
            state.active[id.0 as usize] = InitState::Uninitialized;
        }
    }
    Ok(())
}

fn invalidate_storage(function: &PhysicalFunction, state: &mut FlowState, id: StorageId) {
    state.slots[id.0 as usize] = InitState::Uninitialized;
    partial::set_leaves(function, state, id, InitState::Uninitialized);
    callable::invalidate_captures(function, state, id);
}

fn apply_edge(
    function: &PhysicalFunction,
    edge: &PhysicalEdge,
    mut state: FlowState,
    block: BlockId,
) -> Result<(BlockId, FlowState), PhysicalError> {
    let before = state.slots.clone();
    for (source, _) in &edge.transfers {
        partial::require_root(function, &state, *source, block, "edge transfer")?;
        require_no_live_borrows(function, &state, *source)?;
    }
    // The predecessor is one simultaneous move: a destination may itself
    // supply another destination in a loop permutation.
    for (source, _) in &edge.transfers {
        consume_if_owned(function, &mut state, *source)?;
        // A loan rename moves the alias: the source name is gone in the
        // successor, so it must not stay live to the function exit.
        if storage(function, *source)?.own == OwnKind::Guaranteed
            && storage(function, *source)?.borrow_parent.is_some()
        {
            invalidate_storage(function, &mut state, *source);
        }
    }
    for (source, destination) in &edge.transfers {
        if source == destination && storage(function, *source)?.own == OwnKind::None {
            continue;
        }
        define(function, &mut state, *destination, block, "edge transfer")?;
    }
    for (source, destination) in &edge.leaf_transfers {
        state.slots[destination.0 as usize] = before[source.0 as usize];
    }
    Ok((edge.target, state))
}

fn call_successors(
    function: &PhysicalFunction,
    args: &[ArgumentTransfer],
    result: Option<StorageId>,
    normal: Option<&PhysicalEdge>,
    unwind: Option<&PhysicalEdge>,
    mut state: FlowState,
    block: BlockId,
) -> Result<Vec<(BlockId, FlowState)>, PhysicalError> {
    if state.fault != FaultState::None {
        return Err(PhysicalError::new(format!(
            "physical bb{} issues a call while an earlier fault is active",
            block.0
        )));
    }
    for argument in args {
        let (source, moves) = match argument {
            ArgumentTransfer::Borrow(source)
            | ArgumentTransfer::BorrowMut(source)
            | ArgumentTransfer::Clone { source, .. } => (*source, false),
            ArgumentTransfer::Move(source) => (*source, true),
        };
        if matches!(argument, ArgumentTransfer::BorrowMut(_)) {
            require_no_live_borrows(function, &state, source)?;
        }
        initialized(function, &state, source, block, "call argument")?;
        if storage(function, source)?.own == OwnKind::Guaranteed
            && !matches!(
                argument,
                ArgumentTransfer::Borrow(_) | ArgumentTransfer::BorrowMut(_)
            )
        {
            return Err(PhysicalError::new(
                "physical guaranteed call argument must use its borrow contract",
            ));
        }
        if moves {
            consume_if_owned(function, &mut state, source)?;
        }
    }
    let mut normal_state = state.clone();
    if let Some(result) = result {
        define(function, &mut normal_state, result, block, "call result")?;
    }
    let mut successors = Vec::new();
    if let Some(normal) = normal {
        successors.push(apply_edge(function, normal, normal_state, block)?);
    }
    if let Some(unwind) = unwind {
        let mut failure_state = state;
        if let Some(result) = result {
            failure_state.slots[result.0 as usize] = InitState::Uninitialized;
        }
        failure_state.fault = FaultState::Active;
        failure_state.exit = defer::TRAP;
        successors.push(apply_edge(function, unwind, failure_state, block)?);
    }
    Ok(successors)
}

#[allow(
    clippy::too_many_lines,
    reason = "the terminator transfer is the complete status/result/fault initialization contract"
)]
fn terminator_successors(
    function: &PhysicalFunction,
    terminator: &PhysicalTerminator,
    mut state: FlowState,
    block: BlockId,
    defer_plan: &defer::Plan,
) -> Result<Vec<(BlockId, FlowState)>, PhysicalError> {
    if matches!(
        terminator,
        PhysicalTerminator::EnterDefer { .. }
            | PhysicalTerminator::FinishDefer { .. }
            | PhysicalTerminator::CleanupDispatch { .. }
            | PhysicalTerminator::CheckedRaiseFault { .. }
    ) {
        return defer::successors(function, defer_plan, terminator, state, block);
    }
    if matches!(
        terminator,
        PhysicalTerminator::Return { .. }
            | PhysicalTerminator::PropagateFault
            | PhysicalTerminator::Trap(_)
            | PhysicalTerminator::Unreachable
    ) && (!state.defers.pending.is_empty() || !state.defers.active.is_empty())
    {
        return Err(PhysicalError::new(
            "physical exit leaves pending actions or live fault parks",
        ));
    }
    if matches!(
        terminator,
        PhysicalTerminator::Return { .. }
            | PhysicalTerminator::Trap(_)
            | PhysicalTerminator::Unreachable
            | PhysicalTerminator::PropagateFault
    ) && function.storage.iter().any(|slot| {
        slot.borrow_parent.is_some() && state.slots[slot.id.0 as usize] != InitState::Uninitialized
    }) {
        return Err(PhysicalError::new(
            "physical function exit leaves a local loan live",
        ));
    }
    if matches!(
        terminator,
        PhysicalTerminator::Return { .. }
            | PhysicalTerminator::Trap(_)
            | PhysicalTerminator::Unreachable
            | PhysicalTerminator::PropagateFault
    ) && state
        .active
        .iter()
        .any(|active| *active != InitState::Uninitialized)
    {
        return Err(PhysicalError::new(
            "physical function exit leaves local storage active",
        ));
    }
    match terminator {
        PhysicalTerminator::ActorAsk {
            args,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            // The target is borrowed to address the actor; the request
            // arguments are consumed into the message wrapper.
            for argument in args {
                let (ArgumentTransfer::Borrow(source) | ArgumentTransfer::Move(source)) = argument
                else {
                    return Err(PhysicalError::new("ask must consume its complete request"));
                };
                initialized(function, &state, *source, block, "ask request")?;
                if matches!(argument, ArgumentTransfer::Move(_)) {
                    consume_if_owned(function, &mut state, *source)?;
                }
            }
            if state.fault != FaultState::None {
                return Err(PhysicalError::new("ask cannot replace an active fault"));
            }
            let mut completed = state.clone();
            define(function, &mut completed, *result, block, "ask result")?;
            let mut successors = vec![apply_edge(function, normal, completed, block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::TaskSelect {
            sources,
            timeout,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            for source in sources {
                let ArgumentTransfer::Borrow(handle) = source.transfer() else {
                    return Err(PhysicalError::new(
                        "selection must borrow its source handles",
                    ));
                };
                initialized(function, &state, handle, block, "selected source")?;
            }
            if let Some(timeout) = timeout {
                initialized(function, &state, *timeout, block, "selection timeout")?;
            }
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "selection cannot replace an active fault",
                ));
            }
            let mut completed = state.clone();
            define(
                function,
                &mut completed,
                *result,
                block,
                "selected source index",
            )?;
            let mut successors = vec![apply_edge(function, normal, completed, block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::GeneratorYield { .. }
        | PhysicalTerminator::GeneratorNext { .. }
        | PhysicalTerminator::ValueClose { .. } => {
            generators::successors(function, terminator, state, block)
        }
        PhysicalTerminator::StreamNext {
            stream,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            let ArgumentTransfer::BorrowMut(stream) = stream else {
                return Err(PhysicalError::new(
                    "stream receive requires an exclusive stream",
                ));
            };
            initialized(function, &state, *stream, block, "received stream")?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "stream receive cannot replace an active fault",
                ));
            }
            let mut completed = state.clone();
            define(function, &mut completed, *result, block, "received element")?;
            let mut successors = vec![apply_edge(function, normal, completed, block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::ChannelRecv {
            channel,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            let ArgumentTransfer::BorrowMut(channel) = channel else {
                return Err(PhysicalError::new(
                    "channel receive requires an exclusive receiver",
                ));
            };
            initialized(function, &state, *channel, block, "received channel")?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "channel receive cannot replace an active fault",
                ));
            }
            let mut completed = state.clone();
            define(function, &mut completed, *result, block, "received element")?;
            let mut successors = vec![apply_edge(function, normal, completed, block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::ChannelSend {
            channel,
            value,
            normal,
            cancel,
            unwind,
            ..
        } => {
            let (ArgumentTransfer::BorrowMut(channel), ArgumentTransfer::Borrow(value)) =
                (channel, value)
            else {
                return Err(PhysicalError::new(
                    "channel send borrows its sender exclusively and reads its element",
                ));
            };
            initialized(function, &state, *channel, block, "sending channel")?;
            initialized(function, &state, *value, block, "sent element")?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "channel send cannot replace an active fault",
                ));
            }
            let mut successors = vec![apply_edge(function, normal, state.clone(), block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::StreamSend {
            sink,
            value,
            normal,
            closed,
            cancel,
            unwind,
            ..
        } => {
            let (ArgumentTransfer::Borrow(sink), ArgumentTransfer::Move(value)) = (sink, value)
            else {
                return Err(PhysicalError::new(
                    "stream send borrows its sink and consumes its element",
                ));
            };
            initialized(function, &state, *sink, block, "sending sink")?;
            initialized(function, &state, *value, block, "sent element")?;
            consume_if_owned(function, &mut state, *value)?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "stream send cannot replace an active fault",
                ));
            }
            let mut successors = vec![
                apply_edge(function, normal, state.clone(), block)?,
                apply_edge(function, closed, state.clone(), block)?,
            ];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::TaskAwait {
            task,
            result,
            normal,
            cancel,
            unwind,
        } => {
            let ArgumentTransfer::Move(task) = task else {
                return Err(PhysicalError::new("task await must consume its handle"));
            };
            initialized(function, &state, *task, block, "await task")?;
            consume_if_owned(function, &mut state, *task)?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new("await cannot replace an active fault"));
            }
            let mut completed = state.clone();
            if let Some(result) = result {
                define(function, &mut completed, *result, block, "await result")?;
            }
            let mut successors = normal
                .as_ref()
                .map(|normal| apply_edge(function, normal, completed, block))
                .transpose()?
                .into_iter()
                .collect::<Vec<_>>();
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::TaskScopeJoin {
            mode,
            normal,
            unwind,
            ..
        } => {
            if mode.preserves_fault() {
                if state.fault != FaultState::Active {
                    return Err(PhysicalError::new(
                        "fault drain requires an active primary fault",
                    ));
                }
                Ok(vec![
                    apply_edge(function, normal, state.clone(), block)?,
                    apply_edge(function, unwind, state, block)?,
                ])
            } else {
                if state.fault != FaultState::None {
                    return Err(PhysicalError::new(
                        "normal drain cannot replace an active fault",
                    ));
                }
                let completed = apply_edge(function, normal, state.clone(), block)?;
                state.fault = FaultState::Active;
                state.exit = defer::TRAP | defer::CANCEL;
                Ok(vec![completed, apply_edge(function, unwind, state, block)?])
            }
        }
        PhysicalTerminator::NativeIo {
            args,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            let mut successors = call_successors(
                function,
                args,
                Some(*result),
                Some(normal),
                Some(unwind),
                state.clone(),
                block,
            )?;
            let (_, mut cancelled) = call_successors(
                function,
                args,
                Some(*result),
                Some(normal),
                Some(cancel),
                state,
                block,
            )?
            .pop()
            .expect("native I/O cancel successor");
            cancelled.exit = defer::CANCEL;
            successors.push((cancel.target, cancelled));
            Ok(successors)
        }
        PhysicalTerminator::Sleep {
            duration: operand,
            normal,
            cancel,
            unwind,
        }
        | PhysicalTerminator::SleepUntil {
            deadline: operand,
            normal,
            cancel,
            unwind,
        } => {
            initialized(function, &state, *operand, block, "sleep input")?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new("sleep cannot overwrite an active fault"));
            }
            let mut successors = vec![apply_edge(function, normal, state.clone(), block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::EnterDefer { .. }
        | PhysicalTerminator::FinishDefer { .. }
        | PhysicalTerminator::CleanupDispatch { .. }
        | PhysicalTerminator::CheckedRaiseFault { .. } => {
            unreachable!("defer boundary handled above")
        }
        PhysicalTerminator::RecoverFault {
            result,
            normal,
            unwind,
            ..
        } => {
            if state.fault != FaultState::Active {
                return Err(PhysicalError::new(
                    "scope recovery requires an active fault",
                ));
            }
            let mut recovered = state.clone();
            recovered.fault = FaultState::None;
            recovered.exit = defer::ORDINARY;
            define(function, &mut recovered, *result, block, "scope failure")?;
            Ok(vec![
                apply_edge(function, normal, recovered, block)?,
                apply_edge(function, unwind, state, block)?,
            ])
        }
        PhysicalTerminator::IndirectCall {
            callee: receiver,
            args,
            result,
            normal,
            unwind,
            ..
        }
        | PhysicalTerminator::DynCall {
            receiver,
            args,
            result,
            normal,
            unwind,
            ..
        } => {
            let transfers = std::iter::once(*receiver)
                .chain(args.iter().copied())
                .collect::<Vec<_>>();
            call_successors(
                function,
                &transfers,
                *result,
                normal.as_ref(),
                unwind.as_ref(),
                state,
                block,
            )
        }

        PhysicalTerminator::Return { value } => {
            callable::verify_capture_return(function, &state)?;
            if state.exit != defer::ORDINARY {
                return Err(PhysicalError::new(
                    "physical trap cleanup cannot return normally",
                ));
            }
            if function.storage.iter().any(|slot| {
                matches!(
                    slot.origin,
                    StorageOrigin::ActorState {
                        initialized: false,
                        ..
                    }
                ) && state.slots[slot.id.0 as usize] != InitState::Initialized
            }) {
                return Err(PhysicalError::new(format!(
                    "physical bb{} returns from init with a deferred actor field uninitialized",
                    block.0
                )));
            }
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} returns normally while owning an active fault",
                    block.0
                )));
            }
            if let Some(value) = value {
                let id = match value {
                    ReturnTransfer::Borrow(id)
                    | ReturnTransfer::Move(id)
                    | ReturnTransfer::Clone { source: id, .. } => *id,
                };
                initialized(function, &state, id, block, "return")?;
            }
            Ok(vec![])
        }
        PhysicalTerminator::Goto(edge) => Ok(vec![apply_edge(function, edge, state, block)?]),
        PhysicalTerminator::Branch {
            condition,
            then_target,
            else_target,
        } => {
            initialized(function, &state, *condition, block, "branch")?;
            Ok(vec![
                apply_edge(function, then_target, state.clone(), block)?,
                apply_edge(function, else_target, state, block)?,
            ])
        }
        PhysicalTerminator::SwitchVariant {
            scrutinee, arms, ..
        } => {
            initialized(function, &state, *scrutinee, block, "variant switch")?;
            let mut successors = Vec::with_capacity(arms.len());
            for arm in arms {
                let mut arm_state = state.clone();
                consume_if_owned(function, &mut arm_state, *scrutinee)?;
                for field in &arm.fields {
                    define(function, &mut arm_state, *field, block, "variant payload")?;
                }
                successors.push(apply_edge(function, &arm.target, arm_state, block)?);
            }
            Ok(successors)
        }
        PhysicalTerminator::CheckedBinary {
            lhs,
            rhs,
            result,
            normal,
            failures,
            ..
        } => {
            initialized(function, &state, *lhs, block, "checked binary operation")?;
            initialized(function, &state, *rhs, block, "checked binary operation")?;

            let mut normal_state = state.clone();
            define(
                function,
                &mut normal_state,
                *result,
                block,
                "checked binary result",
            )?;
            let mut successors = vec![apply_edge(function, normal, normal_state, block)?];
            for failure in failures {
                let mut failed = state.clone();
                failed.exit = defer::TRAP;
                successors.push(apply_edge(function, &failure.edge, failed, block)?);
            }
            Ok(successors)
        }
        PhysicalTerminator::Call {
            args,
            result,
            normal,
            unwind,
            ..
        } => call_successors(
            function,
            args,
            *result,
            normal.as_ref(),
            unwind.as_ref(),
            state,
            block,
        ),
        PhysicalTerminator::ActorCall {
            args,
            result,
            normal,
            unwind,
            ..
        } => call_successors(
            function,
            args,
            *result,
            Some(normal),
            unwind.as_ref(),
            state,
            block,
        ),
        PhysicalTerminator::WireCodec {
            input,
            result,
            normal,
            unwind,
            ..
        } => call_successors(
            function,
            std::slice::from_ref(input),
            Some(*result),
            Some(normal),
            Some(unwind),
            state,
            block,
        ),
        PhysicalTerminator::ValueCall {
            args,
            result,
            normal,
            unwind,
            ..
        } => call_successors(
            function,
            args,
            Some(*result),
            Some(normal),
            Some(unwind),
            state,
            block,
        ),
        PhysicalTerminator::ExternCall {
            args,
            result,
            normal,
            ..
        } => call_successors(function, args, *result, Some(normal), None, state, block),
        PhysicalTerminator::RuntimeCall {
            action,
            args,
            result,
            normal,
            failure,
            ..
        } => {
            let failure_inputs = action
                .semantic_family()
                .semantic_contract()
                .is_some_and(hew_types::RuntimeSemanticContract::preserves_inputs_on_failure)
                .then(|| state.clone());
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} issues a runtime call while an earlier fault is active",
                    block.0
                )));
            }
            for argument in args {
                let (source, moves) = match argument {
                    ArgumentTransfer::Borrow(source)
                    | ArgumentTransfer::BorrowMut(source)
                    | ArgumentTransfer::Clone { source, .. } => (*source, false),
                    ArgumentTransfer::Move(source) => (*source, true),
                };
                initialized(function, &state, source, block, "runtime call argument")?;
                if storage(function, source)?.own == OwnKind::Guaranteed
                    && !matches!(
                        argument,
                        ArgumentTransfer::Borrow(_) | ArgumentTransfer::BorrowMut(_)
                    )
                {
                    return Err(PhysicalError::new(
                        "physical guaranteed runtime argument must use its borrow contract",
                    ));
                }
                if moves {
                    consume_if_owned(function, &mut state, source)?;
                }
            }
            let mut normal_state = state.clone();
            if let Some(result) = result {
                define(
                    function,
                    &mut normal_state,
                    *result,
                    block,
                    "runtime call result",
                )?;
            }
            // A never-returning action ends the path; its normal edge is only
            // the structural unreachable continuation.
            let returns = !action
                .semantic_family()
                .semantic_contract()
                .is_some_and(|contract| {
                    matches!(contract.result, hew_types::RuntimeResultEffect::Never)
                });
            let mut successors = Vec::new();
            if returns {
                successors.push(apply_edge(function, normal, normal_state, block)?);
            }
            if let Some(failure) = failure {
                if let Some(preserved) = failure_inputs {
                    state = preserved;
                }
                state.exit = defer::TRAP;
                if action
                    .semantic_family()
                    .semantic_contract()
                    .is_some_and(hew_types::RuntimeSemanticContract::propagates_fault)
                {
                    state.fault = FaultState::Active;
                }
                if let Some(result) = result {
                    state.slots[result.0 as usize] = InitState::Uninitialized;
                }
                successors.push(apply_edge(function, failure, state, block)?);
            }
            Ok(successors)
        }
        PhysicalTerminator::Panic { message, cleanup } => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "physical panic cannot overwrite an active fault",
                ));
            }
            let ArgumentTransfer::Borrow(source) = message else {
                return Err(PhysicalError::new("physical panic must borrow its message"));
            };
            initialized(function, &state, *source, block, "panic message")?;
            state.fault = FaultState::Active;
            state.exit = defer::TRAP;
            Ok(vec![apply_edge(function, cleanup, state, block)?])
        }
        PhysicalTerminator::Trap(_) => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} creates a trap while an earlier fault is active",
                    block.0
                )));
            }
            Ok(vec![])
        }
        PhysicalTerminator::Unreachable => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} abandons an active fault at unreachable",
                    block.0
                )));
            }
            Ok(vec![])
        }
        PhysicalTerminator::PropagateFault => {
            if state.fault != FaultState::Active {
                return Err(PhysicalError::new(format!(
                    "physical bb{} propagates a fault that is not initialized",
                    block.0
                )));
            }
            if function.storage.iter().any(|slot| {
                slot.own == OwnKind::Owned
                    && !matches!(
                        slot.origin,
                        StorageOrigin::Capture { .. }
                            | StorageOrigin::ActorState {
                                initialized: true,
                                ..
                            }
                    )
                    && function
                        .place_storage
                        .get(&slot.id)
                        .is_none_or(|place| place.root == slot.id)
                    && state.slots[slot.id.0 as usize] != InitState::Uninitialized
            }) {
                return Err(PhysicalError::new(
                    "physical fault propagation leaves owned storage initialized",
                ));
            }
            Ok(vec![])
        }
    }
}

fn merge_flow(existing: &mut FlowState, incoming: &FlowState) -> bool {
    let mut changed = existing.defers.join(&incoming.defers);
    let exit = existing.exit | incoming.exit;
    changed |= exit != existing.exit;
    existing.exit = exit;
    for (left, right) in existing
        .slots
        .iter_mut()
        .zip(&incoming.slots)
        .chain(existing.active.iter_mut().zip(&incoming.active))
    {
        let merged = if *left == *right {
            *left
        } else {
            InitState::MaybeInitialized
        };
        if *left != merged {
            *left = merged;
            changed = true;
        }
    }
    let merged_fault = if existing.fault == incoming.fault {
        existing.fault
    } else {
        FaultState::MaybeActive
    };
    if existing.fault != merged_fault {
        existing.fault = merged_fault;
        changed = true;
    }
    changed
}

#[allow(
    clippy::too_many_lines,
    reason = "one exhaustive verifier match keeps every physical terminator contract visible together"
)]
fn verify_terminator(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    blocks: &BTreeSet<BlockId>,
    terminator: &PhysicalTerminator,
) -> Result<(), PhysicalError> {
    let slot = |id: StorageId| {
        function
            .storage
            .get(id.0 as usize)
            .filter(|storage| storage.id == id)
            .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))
    };
    let edge = |edge: &PhysicalEdge| {
        partial::verify_edge(function, edge)?;
        if blocks.contains(&edge.target) {
            for (source, destination) in &edge.transfers {
                // A loan may be renamed across an edge, but only onto a
                // parameter that names the same owner: the transfer copies the
                // alias, never an obligation.
                let loan_rename = slot(*source)?.own == OwnKind::Guaranteed
                    && slot(*source)?.borrow_parent.is_some()
                    && slot(*source)?.borrow_parent == slot(*destination)?.borrow_parent;
                if slot(*source)?.ty != slot(*destination)?.ty
                    || slot(*source)?.own != slot(*destination)?.own
                    || (slot(*source)?.own == OwnKind::Guaranteed && !loan_rename)
                {
                    return Err(PhysicalError::new(format!(
                        "physical edge to block {} transfers incompatible storage",
                        edge.target.0
                    )));
                }
            }
            Ok(())
        } else {
            Err(PhysicalError::new(format!(
                "physical edge targets unknown block {}",
                edge.target.0
            )))
        }
    };
    match terminator {
        PhysicalTerminator::ActorAsk {
            actor,
            message,
            sealed,
            args,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            let target = match args.first() {
                Some(ArgumentTransfer::Borrow(source)) => slot(*source)?.ty.clone(),
                _ => return Err(PhysicalError::new("ask must borrow its target first")),
            };
            let signature = module
                .actors
                .get(actor.0 as usize)
                .filter(|descriptor| descriptor.id == *actor)
                .ok_or_else(|| PhysicalError::new("ask requires its exact actor descriptor"))?
                .ask_signature(*message, &target, slot(*result)?.ty.clone(), *sealed)
                .map_err(PhysicalError::new)?;
            if args.len() != signature.params.len() || slot(*result)?.ty != signature.return_ty {
                return Err(PhysicalError::new(
                    "ask differs from its full protocol signature",
                ));
            }
            for (index, (argument, parameter)) in args.iter().zip(&signature.params).enumerate() {
                let (ArgumentTransfer::Borrow(source) | ArgumentTransfer::Move(source)) = argument
                else {
                    return Err(PhysicalError::new("ask must transfer its complete request"));
                };
                let borrowed = matches!(argument, ArgumentTransfer::Borrow(_));
                if borrowed != (index == 0) {
                    return Err(PhysicalError::new(
                        "ask borrows its target and transfers its complete request",
                    ));
                }
                if slot(*source)?.ty != parameter.ty {
                    return Err(PhysicalError::new(
                        "ask request field changes its protocol type",
                    ));
                }
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::TaskSelect {
            sources,
            timeout,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            if sources.is_empty() && timeout.is_none() {
                return Err(PhysicalError::new("selection requires a source or timeout"));
            }
            for source in sources {
                let ArgumentTransfer::Borrow(handle) = source.transfer() else {
                    return Err(PhysicalError::new(
                        "selection requires borrowed source handles",
                    ));
                };
                let ty = &slot(handle)?.ty;
                let agrees = match source {
                    PhysicalSelectSource::Task(_) => matches!(ty, ResolvedTy::Task(_)),
                    PhysicalSelectSource::ActorCall(_) => {
                        ty.is_builtin(hew_types::BuiltinType::ActorCall)
                    }
                    PhysicalSelectSource::ChannelRecv(_) => {
                        ty.is_builtin(hew_types::BuiltinType::Receiver)
                    }
                };
                if !agrees {
                    return Err(PhysicalError::new(
                        "selection source type disagrees with its registration",
                    ));
                }
            }
            if let Some(timeout) = timeout {
                if slot(*timeout)?.ty != ResolvedTy::Duration
                    || slot(*timeout)?.own != OwnKind::None
                {
                    return Err(PhysicalError::new(
                        "selection timeout must be a trivial duration",
                    ));
                }
            }
            if slot(*result)?.ty != ResolvedTy::I64 || slot(*result)?.own != OwnKind::None {
                return Err(PhysicalError::new(
                    "selection result must be a trivial source index",
                ));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::GeneratorYield { .. }
        | PhysicalTerminator::GeneratorNext { .. }
        | PhysicalTerminator::ValueClose { .. } => {
            generators::verify_suspend(module, function, terminator)?;
            for successor in defer::edges(terminator) {
                edge(successor)?;
            }
            Ok(())
        }
        PhysicalTerminator::StreamNext {
            stream,
            element: recipe,
            result,
            ..
        } => {
            let ArgumentTransfer::BorrowMut(stream) = stream else {
                return Err(PhysicalError::new(
                    "stream receive requires an exclusive stream",
                ));
            };
            let element = hew_sir::stream_element(&slot(*stream)?.ty)
                .ok_or_else(|| PhysicalError::new("stream receive has no stream input"))?;
            if recipe.ty != *element
                || slot(*result)?.ty
                    != ResolvedTy::named_builtin(
                        "Option",
                        BuiltinType::Option,
                        vec![element.clone()],
                    )
            {
                return Err(PhysicalError::new(
                    "stream receive changes its element type",
                ));
            }
            verify_value_recipe(module, recipe)?;
            for successor in defer::edges(terminator) {
                edge(successor)?;
            }
            Ok(())
        }
        PhysicalTerminator::ChannelRecv {
            channel,
            element,
            result,
            ..
        } => {
            let ArgumentTransfer::BorrowMut(channel) = channel else {
                return Err(PhysicalError::new(
                    "channel receive requires an exclusive receiver",
                ));
            };
            if hew_sir::receiver_element(&slot(*channel)?.ty) != Some(&element.ty)
                || slot(*result)?.ty
                    != ResolvedTy::named_builtin(
                        "Option",
                        BuiltinType::Option,
                        vec![element.ty.clone()],
                    )
            {
                return Err(PhysicalError::new(
                    "channel receive changes its element type",
                ));
            }
            verify_value_recipe(module, element)?;
            for successor in defer::edges(terminator) {
                edge(successor)?;
            }
            Ok(())
        }
        PhysicalTerminator::ChannelSend {
            channel,
            value,
            element,
            ..
        } => {
            let (ArgumentTransfer::BorrowMut(channel), ArgumentTransfer::Borrow(value)) =
                (channel, value)
            else {
                return Err(PhysicalError::new(
                    "channel send borrows its sender exclusively and reads its element",
                ));
            };
            if hew_sir::sender_element(&slot(*channel)?.ty) != Some(&element.ty)
                || slot(*value)?.ty != element.ty
            {
                return Err(PhysicalError::new(
                    "channel send element differs from its sender",
                ));
            }
            verify_value_recipe(module, element)?;
            for successor in defer::edges(terminator) {
                edge(successor)?;
            }
            Ok(())
        }
        PhysicalTerminator::StreamSend {
            sink,
            value,
            element,
            ..
        } => {
            let (ArgumentTransfer::Borrow(sink), ArgumentTransfer::Move(value)) = (sink, value)
            else {
                return Err(PhysicalError::new(
                    "stream send borrows its sink and consumes its element",
                ));
            };
            if hew_sir::sink_element(&slot(*sink)?.ty) != Some(&element.ty)
                || slot(*value)?.ty != element.ty
            {
                return Err(PhysicalError::new(
                    "stream send element differs from its sink",
                ));
            }
            verify_value_recipe(module, element)?;
            for successor in defer::edges(terminator) {
                edge(successor)?;
            }
            Ok(())
        }
        PhysicalTerminator::TaskAwait {
            task,
            result,
            normal,
            cancel,
            unwind,
        } => {
            let ArgumentTransfer::Move(task) = task else {
                return Err(PhysicalError::new("await requires a moved task handle"));
            };
            let ResolvedTy::Task(output) = &slot(*task)?.ty else {
                return Err(PhysicalError::new("await requires an exact Task type"));
            };
            match result {
                Some(result)
                    if slot(*result)?.ty == **output
                        && normal.is_some()
                        && **output != ResolvedTy::Never => {}
                None if **output == ResolvedTy::Unit && normal.is_some() => {}
                None if **output == ResolvedTy::Never && normal.is_none() => {}
                _ => return Err(PhysicalError::new("await output differs from task result")),
            }
            if let Some(normal) = normal {
                edge(normal)?;
            }
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::TaskScopeJoin { normal, unwind, .. } => {
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::NativeIo {
            operation,
            args,
            result,
            normal,
            cancel,
            unwind,
        } => {
            let arguments = args
                .iter()
                .map(|argument| match argument {
                    ArgumentTransfer::Borrow(id) => Ok(slot(*id)?.ty.clone()),
                    _ => Err(PhysicalError::new("native I/O requires borrowed inputs")),
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            if !operation
                .contract()
                .matches_signature(&arguments, &slot(*result)?.ty)
            {
                return Err(PhysicalError::new(
                    "native I/O inputs or result differ from the operation contract",
                ));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::Sleep {
            duration,
            normal,
            cancel,
            unwind,
        } => {
            if slot(*duration)?.ty != ResolvedTy::Duration || slot(*duration)?.own != OwnKind::None
            {
                return Err(PhysicalError::new("sleep input must be a trivial duration"));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::SleepUntil {
            deadline,
            normal,
            cancel,
            unwind,
        } => {
            if slot(*deadline)?.ty != ResolvedTy::I64 || slot(*deadline)?.own != OwnKind::None {
                return Err(PhysicalError::new(
                    "sleep-until input must be a trivial instant",
                ));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::EnterDefer { body, .. }
        | PhysicalTerminator::FinishDefer { next: body, .. }
        | PhysicalTerminator::CheckedRaiseFault { cleanup: body, .. } => edge(body),
        PhysicalTerminator::RecoverFault {
            result,
            glue,
            deadline_variant,
            fault_variant,
            normal,
            unwind,
        } => {
            let glue = variant_glue(module, *glue)?;
            if slot(*result)?.ty != glue.ty
                || slot(*result)?.own != OwnKind::Owned
                || glue.is_indirect
                || deadline_variant == fault_variant
                || glue.variants.len() != 2
                || [*deadline_variant, *fault_variant].iter().any(|tag| {
                    glue.variants.get(*tag as usize).is_none_or(|variant| {
                        variant.fields.len() != 1 || variant.fields[0].ty != ResolvedTy::String
                    })
                })
            {
                return Err(PhysicalError::new(
                    "scope recovery requires owned string failure variants",
                ));
            }
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::CleanupDispatch { normal, fault } => {
            edge(normal)?;
            edge(fault)
        }
        PhysicalTerminator::DynCall {
            receiver,
            slot,
            signature,
            args,
            result,
            normal,
            unwind,
        } => {
            callable::verify_dyn_call(
                module, function, *receiver, *slot, signature, args, *result,
            )?;
            if normal.is_none() != (signature.return_ty == ResolvedTy::Never) {
                return Err(PhysicalError::new(
                    "call normal edge differs from its return type",
                ));
            }
            if let Some(normal) = normal {
                edge(normal)?;
            }
            edge(unwind.as_ref().ok_or_else(|| {
                PhysicalError::new("dynamic dispatch requires a fault cleanup edge")
            })?)?;
            Ok(())
        }
        PhysicalTerminator::IndirectCall {
            callee,
            signature,
            args,
            result,
            normal,
            unwind,
        } => {
            callable::verify_indirect_call(module, function, *callee, signature, args, *result)?;
            if normal.is_none() != (signature.return_ty == ResolvedTy::Never) {
                return Err(PhysicalError::new(
                    "call normal edge differs from its return type",
                ));
            }
            if let Some(normal) = normal {
                edge(normal)?;
            }
            edge(unwind.as_ref().ok_or_else(|| {
                PhysicalError::new("indirect invocation requires a fault cleanup edge")
            })?)?;
            Ok(())
        }

        PhysicalTerminator::Return { value } => {
            let returned = value.map(|value| match value {
                ReturnTransfer::Borrow(id)
                | ReturnTransfer::Move(id)
                | ReturnTransfer::Clone { source: id, .. } => id,
            });
            if let Some(id) = returned {
                if slot(id)?.own == OwnKind::Guaranteed {
                    return Err(PhysicalError::new(
                        "physical function return cannot escape guaranteed storage",
                    ));
                }
            }
            match (
                &callable_for(module, function.callable)?.return_ty,
                returned,
            ) {
                (ResolvedTy::Unit, None) => Ok(()),
                (ResolvedTy::Unit, Some(_)) | (_, None) => Err(PhysicalError::new(
                    "physical return/result-out presence disagrees with callable ABI",
                )),
                (expected, Some(id)) if &slot(id)?.ty == expected => Ok(()),
                _ => Err(PhysicalError::new(
                    "physical return storage type disagrees with callable ABI",
                )),
            }
        }
        PhysicalTerminator::Goto(target) => edge(target),
        PhysicalTerminator::Branch {
            condition,
            then_target,
            else_target,
        } => {
            if slot(*condition)?.ty != ResolvedTy::Bool {
                return Err(PhysicalError::new(
                    "physical branch condition is not bool storage",
                ));
            }
            edge(then_target)?;
            edge(else_target)
        }
        PhysicalTerminator::SwitchVariant {
            scrutinee,
            glue,
            arms,
        } => {
            let scrutinee = slot(*scrutinee)?;
            let recipe = variant_glue(module, *glue)?;
            // A loaned scrutinee reads the same descriptor without taking the
            // recipe's release obligation.
            let own_agrees = scrutinee.own == recipe.own || scrutinee.own == OwnKind::Guaranteed;
            if scrutinee.ty != recipe.ty || !own_agrees {
                return Err(PhysicalError::new(
                    "physical variant switch source disagrees with its glue recipe",
                ));
            }
            if arms.len() != recipe.variants.len() {
                return Err(PhysicalError::new(
                    "physical variant switch is not exhaustive",
                ));
            }
            let mut seen = BTreeSet::new();
            for arm in arms {
                if !seen.insert(arm.variant) {
                    return Err(PhysicalError::new(
                        "physical variant switch repeats a declaration-order tag",
                    ));
                }
                let case = usize::try_from(arm.variant)
                    .ok()
                    .and_then(|variant| recipe.variants.get(variant))
                    .ok_or_else(|| {
                        PhysicalError::new(format!(
                            "physical variant switch tag {} is out of bounds",
                            arm.variant
                        ))
                    })?;
                if arm.fields.len() != case.fields.len() {
                    return Err(PhysicalError::new(format!(
                        "physical variant switch tag {} has {} fields for {} recipes",
                        arm.variant,
                        arm.fields.len(),
                        case.fields.len()
                    )));
                }
                for (field, expected) in arm.fields.iter().zip(&case.fields) {
                    let field = slot(*field)?;
                    // A loaned scrutinee hands its payloads out as loans of the
                    // same region rather than as the recipe's owners.
                    let own_agrees = field.own == expected.own
                        || (scrutinee.own == OwnKind::Guaranteed
                            && field.own == OwnKind::Guaranteed);
                    if field.ty != expected.ty || !own_agrees {
                        return Err(PhysicalError::new(format!(
                            "physical variant switch tag {} payload disagrees with its recipe",
                            arm.variant
                        )));
                    }
                }
                edge(&arm.target)?;
            }
            Ok(())
        }
        PhysicalTerminator::CheckedBinary {
            op,
            lhs,
            rhs,
            result,
            normal,
            failures,
        } => {
            if !hew_sir::checked_binary_types_match(
                *op,
                &slot(*lhs)?.ty,
                &slot(*rhs)?.ty,
                &slot(*result)?.ty,
            ) {
                return Err(PhysicalError::new(
                    "physical checked binary type relation is invalid",
                ));
            }
            let ty = &slot(*result)?.ty;
            let required = hew_sir::checked_binary_failure_kinds(*op, ty).ok_or_else(|| {
                PhysicalError::new(
                    "physical checked binary uses an operator or type without checked failures",
                )
            })?;
            if failures
                .iter()
                .map(|failure| failure.kind)
                .ne(required.iter().copied())
            {
                return Err(PhysicalError::new(
                    "physical checked binary failure set disagrees with SIR semantics",
                ));
            }
            edge(normal)?;
            for failure in failures {
                edge(&failure.edge)?;
            }
            Ok(())
        }
        PhysicalTerminator::Call {
            callee,
            args,
            result,
            normal,
            unwind,
        } => {
            let callee = callable_for(module, *callee)?;
            if args.len() != callee.params.len() {
                return Err(PhysicalError::new(format!(
                    "physical call to {} has {} arguments for {} parameters",
                    callee.id.0,
                    args.len(),
                    callee.params.len()
                )));
            }
            for (argument, parameter) in args.iter().zip(&callee.params) {
                let id = match argument {
                    ArgumentTransfer::Borrow(id)
                    | ArgumentTransfer::BorrowMut(id)
                    | ArgumentTransfer::Move(id)
                    | ArgumentTransfer::Clone { source: id, .. } => *id,
                };
                if slot(id)?.ty != parameter.ty {
                    return Err(PhysicalError::new(
                        "physical call argument type disagrees with callee ABI",
                    ));
                }
                let valid_transfer = match parameter.passing {
                    SemParamPassing::ReadOnly => {
                        !matches!(argument, ArgumentTransfer::BorrowMut(_))
                    }
                    SemParamPassing::Borrow => matches!(argument, ArgumentTransfer::Borrow(_)),
                    SemParamPassing::BorrowMut => {
                        matches!(argument, ArgumentTransfer::BorrowMut(_))
                    }
                    SemParamPassing::Consume => {
                        matches!(argument, ArgumentTransfer::Move(_))
                            && slot(id)?.own == OwnKind::Owned
                    }
                };
                if !valid_transfer {
                    return Err(PhysicalError::new(
                        "physical call argument transfer disagrees with parameter passing",
                    ));
                }
            }
            match (&callee.return_ty, result) {
                (ResolvedTy::Unit | ResolvedTy::Never, None) => {}
                (ResolvedTy::Unit | ResolvedTy::Never, Some(_)) | (_, None) => {
                    return Err(PhysicalError::new(
                        "physical call result-out presence disagrees with callee ABI",
                    ));
                }
                (expected, Some(id))
                    if hew_sir::call_boundary_types_match(&slot(*id)?.ty, expected) => {}
                _ => {
                    return Err(PhysicalError::new(
                        "physical call result storage type disagrees with callee ABI",
                    ));
                }
            }
            if normal.is_none() != (callee.return_ty == ResolvedTy::Never) {
                return Err(PhysicalError::new(
                    "call normal edge differs from its return type",
                ));
            }
            if let Some(normal) = normal {
                edge(normal)?;
            }
            if let Some(unwind) = unwind {
                edge(unwind)?;
            }
            Ok(())
        }
        PhysicalTerminator::WireCodec {
            direction,
            plan,
            recipes,
            text_result,
            input,
            result,
            normal,
            unwind,
        } => {
            wire::verify_wire_plan(module, plan)?;
            let ArgumentTransfer::Borrow(source) = input else {
                return Err(PhysicalError::new("wire codec input must borrow its slot"));
            };
            let input_ty = if direction.is_serialize() {
                plan.ty.clone()
            } else if direction.is_text() {
                ResolvedTy::String
            } else {
                ResolvedTy::Bytes
            };
            if slot(*source)?.ty != input_ty {
                return Err(PhysicalError::new(
                    "wire codec input type differs from its schema",
                ));
            }
            let output = &slot(*result)?.ty;
            let valid_result = match direction {
                hew_types::WireCodecDirection::Encode => *output == ResolvedTy::Bytes,
                hew_types::WireCodecDirection::Decode => *output == plan.ty,
                hew_types::WireCodecDirection::ToJson | hew_types::WireCodecDirection::ToYaml => {
                    *output == ResolvedTy::String
                }
                hew_types::WireCodecDirection::FromJson
                | hew_types::WireCodecDirection::FromYaml => {
                    matches!(output, ResolvedTy::Named { builtin: Some(hew_types::BuiltinType::Result), args, .. } if args == &[plan.ty.clone(), ResolvedTy::String])
                }
            };
            if !valid_result {
                return Err(PhysicalError::new(
                    "wire codec result type differs from its direction",
                ));
            }
            if let Some(cases) = text_result {
                let glue = module
                    .variant_glue
                    .get(cases.glue.0 as usize)
                    .filter(|glue| glue.ty == *output)
                    .ok_or_else(|| {
                        PhysicalError::new("wire text Result has no exact physical glue")
                    })?;
                if cases.ok == cases.error
                    || glue.variants.len() != 2
                    || !glue
                        .variants
                        .get(cases.ok as usize)
                        .is_some_and(|case| case.fields.len() == 1 && case.fields[0].ty == plan.ty)
                    || !glue.variants.get(cases.error as usize).is_some_and(|case| {
                        case.fields.len() == 1 && case.fields[0].ty == ResolvedTy::String
                    })
                {
                    return Err(PhysicalError::new(
                        "wire text Result cases disagree with payloads",
                    ));
                }
            }
            if text_result.is_some()
                != matches!(
                    direction,
                    hew_types::WireCodecDirection::FromJson
                        | hew_types::WireCodecDirection::FromYaml
                )
            {
                return Err(PhysicalError::new(
                    "wire Result cases differ from codec direction",
                ));
            }
            let mut expected = BTreeSet::new();
            plan.visit_types(&mut |ty| {
                expected.insert(ty.clone());
            });
            if recipes.keys().cloned().collect::<BTreeSet<_>>() != expected {
                return Err(PhysicalError::new(
                    "wire codec value recipes do not cover its exact schema",
                ));
            }
            for (ty, recipe) in recipes {
                if *ty != recipe.ty {
                    return Err(PhysicalError::new(
                        "wire recipe key differs from its value type",
                    ));
                }
                verify_value_recipe(module, recipe)?;
            }
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::ValueCall {
            ty,
            capability,
            args,
            result,
            normal,
            unwind,
        } => {
            if !module
                .value_capabilities
                .contains_key(&(ty.clone(), *capability))
            {
                return Err(PhysicalError::new(
                    "physical value call lacks its exact selected capability",
                ));
            }
            let (arity, result_ty) = match capability {
                ValueCapability::Hash => (1, ResolvedTy::I64),
                ValueCapability::Eq => (2, ResolvedTy::Bool),
            };
            if args.len() != arity {
                return Err(PhysicalError::new(
                    "physical value call has the wrong callback arity",
                ));
            }
            for argument in args {
                let ArgumentTransfer::Borrow(source) = argument else {
                    return Err(PhysicalError::new(
                        "physical value call arguments must borrow their slots",
                    ));
                };
                if &slot(*source)?.ty != ty {
                    return Err(PhysicalError::new(
                        "physical value call argument has another type",
                    ));
                }
            }
            let result = slot(*result)?;
            if result.ty != result_ty || result.own != OwnKind::None {
                return Err(PhysicalError::new(
                    "physical value call requires its scalar callback result",
                ));
            }
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::ActorCall {
            operation,
            args,
            result,
            normal,
            unwind,
        } => {
            let signature = actor_signature(module, operation)?;
            if args.len() != signature.params.len() {
                return Err(PhysicalError::new(
                    "actor boundary argument count differs from protocol",
                ));
            }
            for (argument, parameter) in args.iter().zip(&signature.params) {
                let ((SemParamPassing::Borrow, ArgumentTransfer::Borrow(id))
                | (SemParamPassing::Consume, ArgumentTransfer::Move(id))) =
                    (parameter.passing, argument)
                else {
                    return Err(PhysicalError::new(
                        "actor boundary changes its argument ownership",
                    ));
                };
                if slot(*id)?.ty != parameter.ty {
                    return Err(PhysicalError::new(
                        "actor payload differs from its protocol type",
                    ));
                }
            }
            match result {
                None if signature.return_ty == ResolvedTy::Unit => {}
                Some(id) if slot(*id)?.ty == signature.return_ty => {}
                _ => {
                    return Err(PhysicalError::new(
                        "actor boundary result differs from its protocol",
                    ))
                }
            }
            let unwind = unwind
                .as_ref()
                .ok_or_else(|| PhysicalError::new("actor boundary lacks fault cleanup"))?;
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::ExternCall {
            symbol,
            args,
            result,
            result_abi,
            normal,
        } => {
            // SIR proved types and ownership against the declaration. Verify
            // the transfer/edge structure and the target's C result carrier.
            for argument in args {
                let id = match argument {
                    ArgumentTransfer::Borrow(id)
                    | ArgumentTransfer::BorrowMut(id)
                    | ArgumentTransfer::Move(id)
                    | ArgumentTransfer::Clone { source: id, .. } => *id,
                };
                slot(id)?;
            }
            if symbol.is_empty() {
                return Err(PhysicalError::new("extern call has no linker symbol"));
            }
            let result_ty = match result {
                Some(result) => &slot(*result)?.ty,
                None => &ResolvedTy::Unit,
            };
            if *result_abi != module.target.extern_result_abi(result_ty)? {
                return Err(PhysicalError::new(
                    "extern result ABI differs from its target classification",
                ));
            }
            edge(normal)
        }
        PhysicalTerminator::RuntimeCall {
            action,
            args,
            result,
            normal,
            failure,
        } => {
            let contract = action
                .semantic_family()
                .semantic_contract()
                .ok_or_else(|| PhysicalError::new("physical runtime action lost its contract"))?;
            if args.len() != contract.arguments.len() {
                return Err(PhysicalError::new(format!(
                    "physical runtime action {action:?} has {} arguments for {} parameters",
                    args.len(),
                    contract.arguments.len()
                )));
            }
            let parameter_types = args
                .iter()
                .map(|argument| {
                    let id = match argument {
                        ArgumentTransfer::Borrow(id)
                        | ArgumentTransfer::BorrowMut(id)
                        | ArgumentTransfer::Move(id)
                        | ArgumentTransfer::Clone { source: id, .. } => *id,
                    };
                    Ok(slot(id)?.ty.clone())
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            let result_type = result
                .map(|id| slot(id).map(|slot| &slot.ty))
                .transpose()?
                .unwrap_or(if matches!(contract.result, RuntimeResultEffect::Never) {
                    &ResolvedTy::Never
                } else {
                    &ResolvedTy::Unit
                });
            let signature = contract.instantiate(&parameter_types, result_type)
                .map_err(|reason| PhysicalError::new(format!("physical runtime action {action:?} signature disagrees with its semantic contract: {reason}")))?;
            if &signature.result_ty != result_type {
                return Err(PhysicalError::new(format!("physical runtime action {action:?} result type disagrees with its semantic contract")));
            }
            for (argument, expected) in args.iter().zip(contract.arguments) {
                let (id, actual_effect) = match argument {
                    ArgumentTransfer::Borrow(id) => (*id, RuntimeArgumentEffect::Borrow),
                    ArgumentTransfer::BorrowMut(_) => {
                        return Err(PhysicalError::new(
                            "physical runtime operation has no exclusive argument contract",
                        ));
                    }
                    ArgumentTransfer::Move(id) => {
                        if slot(*id)?.own != OwnKind::Owned {
                            return Err(PhysicalError::new(format!(
                                "physical runtime action {action:?} moves a non-owned argument"
                            )));
                        }
                        (*id, RuntimeArgumentEffect::Move)
                    }
                    ArgumentTransfer::Clone {
                        source,
                        action: clone_action,
                    } => {
                        if *clone_action != CloneAction::Bitwise
                            || slot(*source)?.own != OwnKind::None
                        {
                            return Err(PhysicalError::new(format!(
                                "physical runtime action {action:?} copies through a non-bitwise action"
                            )));
                        }
                        (*source, RuntimeArgumentEffect::Copy)
                    }
                };
                let _ = slot(id)?;
                if actual_effect
                    != expected
                        .effect
                        .resolve(semantic_type_facts(module, &slot(id)?.ty)?.clone)
                {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} argument disagrees with its semantic contract"
                    )));
                }
            }
            if let PhysicalRuntimeAction::BytesPop {
                result: pair,
                option: descriptor,
            } = action
            {
                let Some(output) = result else {
                    return Err(PhysicalError::new("bytes pop has no result storage"));
                };
                let pair = aggregate_glue(module, *pair)?;
                let descriptor = variant_glue(module, *descriptor)?;
                let [receiver, optional] = pair.fields.as_slice() else {
                    return Err(PhysicalError::new("bytes pop pair is not two fields"));
                };
                if pair.ty != slot(*output)?.ty
                    || receiver.ty != ResolvedTy::Bytes
                    || descriptor.ty != optional.ty
                    || descriptor.is_indirect
                    || descriptor.variants.len() != 2
                    || descriptor.variants[0].fields.len() != 1
                    || descriptor.variants[0].fields[0].ty != ResolvedTy::U8
                    || !descriptor.variants[1].fields.is_empty()
                {
                    return Err(PhysicalError::new(
                        "bytes pop descriptor disagrees with result",
                    ));
                }
            }
            if let PhysicalRuntimeAction::StringFind { result: descriptor }
            | PhysicalRuntimeAction::StringCharAt { result: descriptor }
            | PhysicalRuntimeAction::BytesGet { result: descriptor } = action
            {
                let payload = match action {
                    PhysicalRuntimeAction::StringCharAt { .. } => ResolvedTy::Char,
                    PhysicalRuntimeAction::BytesGet { .. } => ResolvedTy::U8,
                    _ => ResolvedTy::I64,
                };
                let Some(output) = result else {
                    return Err(PhysicalError::new("string find has no result storage"));
                };
                let descriptor = variant_glue(module, *descriptor)?;
                if descriptor.ty != slot(*output)?.ty
                    || descriptor.is_indirect
                    || descriptor.variants.len() != 2
                    || descriptor.variants[0].fields.len() != 1
                    || descriptor.variants[0].fields[0].ty != payload
                    || !descriptor.variants[1].fields.is_empty()
                {
                    return Err(PhysicalError::new(
                        "optional runtime descriptor disagrees with result",
                    ));
                }
            }
            match (contract.result, result) {
                (RuntimeResultEffect::FreshOwnedVariant(kind), Some(id)) => {
                    let result_slot = slot(*id)?;
                    let PhysicalRuntimeAction::BytesDecodeUtf8 {
                        result: result_glue,
                        error,
                        error_len,
                    } = *action
                    else {
                        return Err(PhysicalError::new(
                            "variant runtime result has no physical contract",
                        ));
                    };
                    let Some((ok_ty, error_ty)) = kind.payload_types(&result_slot.ty) else {
                        return Err(PhysicalError::new(
                            "UTF-8 decode result has the wrong nominal type",
                        ));
                    };
                    let result_glue = variant_glue(module, result_glue)?;
                    let error_glue = aggregate_glue(module, error)?;
                    let option_glue = variant_glue(module, error_len)?;
                    let option_is_i64 = matches!(
                        &option_glue.ty,
                        ResolvedTy::Named { builtin: Some(hew_types::BuiltinType::Option), args, .. }
                            if args.as_slice() == [ResolvedTy::I64]
                    );
                    let field_is = |fields: &[PhysicalValueRecipe], ty: &ResolvedTy| {
                        fields.len() == 1 && &fields[0].ty == ty
                    };
                    if result_slot.own != OwnKind::Owned
                        || result_glue.ty != result_slot.ty
                        || result_glue.is_indirect
                        || result_glue.variants.len() != 2
                        || !field_is(&result_glue.variants[0].fields, ok_ty)
                        || !field_is(&result_glue.variants[1].fields, error_ty)
                        || &error_glue.ty != error_ty
                        || error_glue.fields.len() != 2
                        || error_glue.fields[0].ty != ResolvedTy::I64
                        || error_glue.fields[1].ty != option_glue.ty
                        || !option_is_i64
                        || option_glue.is_indirect
                        || option_glue.variants.len() != 2
                        || !field_is(&option_glue.variants[0].fields, &ResolvedTy::I64)
                        || !option_glue.variants[1].fields.is_empty()
                    {
                        return Err(PhysicalError::new(
                            "UTF-8 decode physical descriptors disagree with its result contract",
                        ));
                    }
                }
                (RuntimeResultEffect::Unit | RuntimeResultEffect::Never, None) => {}
                (RuntimeResultEffect::Unit | RuntimeResultEffect::Never, Some(_)) | (_, None) => {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} result presence disagrees with its semantic contract"
                    )));
                }
                (
                    RuntimeResultEffect::BitCopy(_)
                    | RuntimeResultEffect::Borrowed(_)
                    | RuntimeResultEffect::FreshOwned(_)
                    | RuntimeResultEffect::UpdatedReceiver(_)
                    | RuntimeResultEffect::IndependentValue(_)
                    | RuntimeResultEffect::UpdatedReceiverAndValue(_),
                    Some(id),
                ) => {
                    let expected_own = match contract.result {
                        RuntimeResultEffect::BitCopy(_) => OwnKind::None,
                        RuntimeResultEffect::Borrowed(_) => OwnKind::Guaranteed,
                        RuntimeResultEffect::FreshOwned(_)
                        | RuntimeResultEffect::UpdatedReceiver(_)
                        | RuntimeResultEffect::UpdatedReceiverAndValue(_) => OwnKind::Owned,
                        RuntimeResultEffect::IndependentValue(_) => {
                            OwnKind::of_class(semantic_type_facts(module, result_type)?.class)
                        }
                        RuntimeResultEffect::Unit
                        | RuntimeResultEffect::Never
                        | RuntimeResultEffect::FreshOwnedVariant(_) => {
                            unreachable!()
                        }
                    };
                    if slot(*id)?.own != expected_own {
                        return Err(PhysicalError::new(format!(
                            "physical runtime action {action:?} result ownership disagrees with its semantic contract"
                        )));
                    }
                }
            }
            match *action {
                PhysicalRuntimeAction::Array { operation, glue } => {
                    let descriptor = vector_glue(module, glue)?;
                    if !matches!(descriptor.ty, ResolvedTy::Array(_, _)) {
                        return Err(PhysicalError::new(
                            "array operation has a non-array descriptor",
                        ));
                    }
                    verify_vector_call(
                        module,
                        match operation {
                            ArrayValueOp::Len => PhysicalVectorOp::Len,
                            ArrayValueOp::Index => PhysicalVectorOp::Index,
                            ArrayValueOp::IndexBorrow => PhysicalVectorOp::IndexBorrow,
                            ArrayValueOp::Set => PhysicalVectorOp::Set,
                        },
                        glue,
                        &parameter_types,
                        result_type,
                    )?;
                }
                PhysicalRuntimeAction::Vector { operation, glue } => {
                    verify_vector_call(module, operation, glue, &parameter_types, result_type)?;
                }
                PhysicalRuntimeAction::Map { operation, glue } => {
                    verify_map_call(module, operation, glue, &parameter_types, result_type)?;
                }
                PhysicalRuntimeAction::Set { operation, glue } => {
                    verify_set_call(module, operation, glue, &parameter_types, result_type)?;
                }
                _ => {}
            }
            match (contract.failures.is_empty(), failure) {
                (true, None) | (false, Some(_)) => {}
                _ => {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} failure edge disagrees with its semantic contract"
                    )));
                }
            }
            edge(normal)?;
            if let Some(failure) = failure {
                edge(failure)?;
            }
            Ok(())
        }
        PhysicalTerminator::Panic { message, cleanup } => {
            let ArgumentTransfer::Borrow(source) = message else {
                return Err(PhysicalError::new("physical panic must borrow its message"));
            };
            if slot(*source)?.ty != ResolvedTy::String {
                return Err(PhysicalError::new("physical panic message must be String"));
            }
            edge(cleanup)
        }
        PhysicalTerminator::Trap(_)
        | PhysicalTerminator::PropagateFault
        | PhysicalTerminator::Unreachable => Ok(()),
    }
}

fn verify_vector_call(
    module: &PhysicalModule,
    operation: PhysicalVectorOp,
    id: PhysicalVectorId,
    arguments: &[ResolvedTy],
    result: &ResolvedTy,
) -> Result<(), PhysicalError> {
    let glue = vector_glue(module, id)?;
    if matches!(
        operation,
        PhysicalVectorOp::Index
            | PhysicalVectorOp::Get { .. }
            | PhysicalVectorOp::Slice
            | PhysicalVectorOp::SliceFrom
    ) && glue.element.clone.is_none()
    {
        return Err(PhysicalError::new(
            "vector read requires an element copy recipe",
        ));
    }
    let receiver = if operation == PhysicalVectorOp::New {
        result
    } else {
        arguments
            .first()
            .ok_or_else(|| PhysicalError::new("physical vector operation lacks its receiver"))?
    };
    if receiver != &glue.ty {
        return Err(PhysicalError::new(
            "physical vector operation uses a foreign vector descriptor",
        ));
    }
    match operation {
        PhysicalVectorOp::Contains => {
            if !module
                .value_capabilities
                .contains_key(&(glue.element.ty.clone(), ValueCapability::Eq))
            {
                return Err(PhysicalError::new(
                    "vector membership lacks its selected element equality",
                ));
            }
        }
        PhysicalVectorOp::Get { result: option }
        | PhysicalVectorOp::GetBorrow { result: option } => {
            let option = variant_glue(module, option)?;
            if &option.ty != result
                || option.is_indirect
                || option.variants.len() != 2
                || option.variants[0].fields.as_slice() != [glue.element.clone()]
                || !option.variants[1].fields.is_empty()
            {
                return Err(PhysicalError::new(
                    "physical vector get result descriptor is not its exact Some(T)/None value",
                ));
            }
        }
        PhysicalVectorOp::Pop { result: tuple }
        | PhysicalVectorOp::Remove { result: tuple }
        | PhysicalVectorOp::TakeFirst { result: tuple } => {
            let tuple = aggregate_glue(module, tuple)?;
            if &tuple.ty != result
                || tuple.own != OwnKind::Owned
                || tuple.fields.len() != 2
                || tuple.fields[0].ty != glue.ty
                || tuple.fields[1] != glue.element
            {
                return Err(PhysicalError::new(
                    "physical vector removal result descriptor is not its exact (Vec<T>, T) value",
                ));
            }
        }
        PhysicalVectorOp::New
        | PhysicalVectorOp::Len
        | PhysicalVectorOp::Index
        | PhysicalVectorOp::Push
        | PhysicalVectorOp::Set
        | PhysicalVectorOp::Clear
        | PhysicalVectorOp::IndexBorrow => {}
        PhysicalVectorOp::Slice | PhysicalVectorOp::SliceFrom => {
            if result != &glue.ty {
                return Err(PhysicalError::new(
                    "physical vector slice result is not its own vector type",
                ));
            }
        }
    }
    Ok(())
}

fn verify_optional_value(
    module: &PhysicalModule,
    id: PhysicalVariantId,
    result: &ResolvedTy,
    value: &PhysicalValueRecipe,
) -> Result<(), PhysicalError> {
    let option = variant_glue(module, id)?;
    if &option.ty != result
        || option.is_indirect
        || option.variants.len() != 2
        || option.variants[0].fields.as_slice() != [value.clone()]
        || !option.variants[1].fields.is_empty()
    {
        return Err(PhysicalError::new(
            "physical optional descriptor is not its exact Some(T)/None value",
        ));
    }
    Ok(())
}

fn verify_map_call(
    module: &PhysicalModule,
    operation: PhysicalMapOp,
    id: PhysicalMapId,
    arguments: &[ResolvedTy],
    result: &ResolvedTy,
) -> Result<(), PhysicalError> {
    let glue = map_glue(module, id)?;
    let receiver = if operation == PhysicalMapOp::New {
        capability::require_key(module, &glue.key.ty)?;
        result
    } else {
        arguments
            .first()
            .ok_or_else(|| PhysicalError::new("map operation lacks its receiver"))?
    };
    if receiver != &glue.ty {
        return Err(PhysicalError::new(
            "physical map operation uses a foreign map descriptor",
        ));
    }
    match operation {
        PhysicalMapOp::Get { result: option } | PhysicalMapOp::GetBorrow { result: option } => {
            verify_optional_value(module, option, result, &glue.value)?;
        }
        PhysicalMapOp::Remove {
            result: pair,
            value: option,
        } => {
            let pair = aggregate_glue(module, pair)?;
            if &pair.ty != result || pair.fields.len() != 2 || pair.fields[0].ty != glue.ty {
                return Err(PhysicalError::new(
                    "map removal result is not its receiver/value pair",
                ));
            }
            verify_optional_value(module, option, &pair.fields[1].ty, &glue.value)?;
        }
        PhysicalMapOp::Entries { result: vector } => {
            let vector = vector_glue(module, vector)?;
            if &vector.ty != result
                || vector.element.ty
                    != ResolvedTy::Tuple(vec![glue.key.ty.clone(), glue.value.ty.clone()])
            {
                return Err(PhysicalError::new(
                    "map entries descriptor has the wrong key/value pair",
                ));
            }
        }
        PhysicalMapOp::New
        | PhysicalMapOp::Len
        | PhysicalMapOp::Index
        | PhysicalMapOp::ContainsKey
        | PhysicalMapOp::Insert
        | PhysicalMapOp::Clear
        | PhysicalMapOp::Keys
        | PhysicalMapOp::Values => {}
    }
    Ok(())
}

fn verify_set_call(
    module: &PhysicalModule,
    operation: PhysicalSetOp,
    id: PhysicalSetId,
    arguments: &[ResolvedTy],
    result: &ResolvedTy,
) -> Result<(), PhysicalError> {
    let glue = set_glue(module, id)?;
    let receiver = if operation == PhysicalSetOp::New {
        capability::require_key(module, &glue.element.ty)?;
        result
    } else {
        arguments
            .first()
            .ok_or_else(|| PhysicalError::new("set operation lacks its receiver"))?
    };
    if receiver != &glue.ty {
        return Err(PhysicalError::new(
            "physical set operation uses a foreign set descriptor",
        ));
    }
    if let PhysicalSetOp::Insert { result: pair } | PhysicalSetOp::Remove { result: pair } =
        operation
    {
        let pair = aggregate_glue(module, pair)?;
        if &pair.ty != result
            || pair.fields.len() != 2
            || pair.fields[0].ty != glue.ty
            || pair.fields[1].ty != ResolvedTy::Bool
        {
            return Err(PhysicalError::new(
                "set update result is not its receiver/presence pair",
            ));
        }
    }
    Ok(())
}

fn callable_for(
    module: &PhysicalModule,
    id: CallableId,
) -> Result<&PhysicalCallable, PhysicalError> {
    module
        .callables
        .get(id.0 as usize)
        .filter(|callable| callable.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical callable {}", id.0)))
}

/// Project the checked actor protocol through this module's physical callables.
///
/// # Errors
/// Refuses missing actor declarations, handlers or initializer callables.
pub fn actor_signature(
    module: &PhysicalModule,
    operation: &hew_sir::ActorOperation,
) -> Result<hew_sir::SemSignature, PhysicalError> {
    operation
        .signature(&module.actors, &module.supervisors, |id| {
            module
                .callables
                .iter()
                .find(|callable| callable.id == id)
                .map(|callable| hew_sir::SemSignature {
                    params: callable
                        .params
                        .iter()
                        .map(|param| hew_sir::SemAbiParam {
                            ty: param.ty.clone(),
                            passing: param.passing,
                            caller_visible_projection: param.passing == SemParamPassing::BorrowMut,
                        })
                        .collect(),
                    return_ty: callable.return_ty.clone(),
                })
        })
        .map_err(PhysicalError::new)
}

fn actor_value_recipes(
    module: &SemModule,
    ids: &PhysicalGlueIds,
) -> Result<BTreeMap<ResolvedTy, PhysicalValueRecipe>, PhysicalError> {
    let mut types = Vec::new();
    for supervisor in &module.supervisors {
        types.extend(supervisor.config.iter().cloned());
    }
    for actor in &module.actors {
        types.push(actor.state_ty.clone());
        // A rejected completion returns its addressed target with the request.
        // Include the exact handle/role instances already admitted by SIR.
        types.extend(
            module
                .type_facts
                .keys()
                .filter(|key| actor.admits_target(&key.0))
                .map(|key| key.0.clone()),
        );
        types.extend(actor.fields.iter().map(|field| field.ty.clone()));
        for handler in &actor.handlers {
            types.extend(handler.params.iter().cloned());
            types.push(handler.return_ty.clone());
        }
    }
    // Waiting requests retain their typed payload until admission.
    types.extend(module.functions.iter().flat_map(|function| {
        function
            .blocks
            .iter()
            .filter_map(|block| match &block.terminator {
                SemTerminator::ActorCall {
                    operation:
                        ActorOperation::Submit {
                            policy, message_ty, ..
                        },
                    ..
                } if policy.may_suspend() => Some(message_ty.clone()),
                SemTerminator::ActorCall {
                    operation: ActorOperation::StreamStart { actor, message, .. },
                    ..
                } => module
                    .actor(*actor)
                    .and_then(|actor| {
                        actor
                            .handlers
                            .iter()
                            .find(|handler| handler.message_id == *message)
                    })
                    .map(|handler| ResolvedTy::Tuple(handler.params.clone())),
                _ => None,
            })
    }));
    types
        .iter()
        .filter(|ty| **ty != ResolvedTy::Unit)
        .map(|ty| physical_value_recipe(module, ids, ty).map(|recipe| (ty.clone(), recipe)))
        .collect::<Result<_, _>>()
}

fn physical_value_recipe(
    module: &SemModule,
    ids: &PhysicalGlueIds,
    ty: &ResolvedTy,
) -> Result<PhysicalValueRecipe, PhysicalError> {
    let facts = module
        .type_facts
        .get(&TypeInstanceKey(ty.clone()))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical field `{}` has no semantic type facts",
                ty.user_facing()
            ))
        })?;
    let own = OwnKind::of_class(facts.class);
    Ok(PhysicalValueRecipe {
        ty: ty.clone(),
        own,
        clone: clone_action_for_type(ty, facts.clone, ids)?,
        destroy: if own == OwnKind::Owned {
            destroy_action_for_type(ty, ids)
        } else {
            None
        },
    })
}

#[cfg(test)]
mod tests {
    mod borrow_fixture {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../hew-sir/tests/support/borrowed_aggregate.rs"
        ));
    }

    mod utf8_fixture {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../hew-sir/tests/support/runtime_utf8.rs"
        ));
    }

    use hew_hir::{lower_program_host_target, ItemId, ResolutionCtx};
    use hew_sir::{
        BoundaryOperand, CallableInstance, CheckedFailure, FunctionSourceOrigin, Operand,
        Provenance, SemBlock, SemCallConv, SemCallable, SemCallableKind, SemSignature, ValueDef,
    };
    use hew_types::{
        module_registry::ModuleRegistry, Checker, DefId, SendFact, TypeFacts, ValueClass,
    };

    use super::*;

    fn exclusive_receiver() -> (PhysicalModule, CallableId) {
        let semantic = lower_source(
            r"
            fn inspect(values: Vec<i64>) -> i64 { values.len() }
            fn main() -> i64 {
                var values: Vec<i64> = Vec.new();
                values.push(3);
                inspect(values)
            }
            ",
        );
        let mut physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
            .unwrap()
            .into_unverified();
        let callee = physical
            .callables
            .iter_mut()
            .find(|callee| callee.declaration.full_path() == "inspect")
            .unwrap();
        callee.params[0].passing = SemParamPassing::BorrowMut;
        callee.params[0].carrier = ParamCarrier::Indirect;
        let id = callee.id;
        for function in &mut physical.functions {
            for block in &mut function.blocks {
                if let PhysicalTerminator::Call { callee, args, .. } = &mut block.terminator {
                    if *callee == id {
                        let ArgumentTransfer::Borrow(source) = args[0] else {
                            panic!("source fixture must borrow its vector");
                        };
                        args[0] = ArgumentTransfer::BorrowMut(source);
                    }
                }
            }
        }
        verify_physical_module(&physical).unwrap();
        (physical, id)
    }

    #[test]
    fn exclusive_receiver_rejects_value_carriers_and_shared_call_arguments() {
        let (physical, id) = exclusive_receiver();
        let mut invalid = physical.clone();
        invalid.callables[id.0 as usize].params[0].carrier = ParamCarrier::Direct;
        assert!(verify_physical_module(&invalid)
            .unwrap_err()
            .message
            .contains("caller storage by address"));

        let mut invalid = physical;
        for function in &mut invalid.functions {
            for block in &mut function.blocks {
                if let PhysicalTerminator::Call { callee, args, .. } = &mut block.terminator {
                    if *callee == id {
                        let ArgumentTransfer::BorrowMut(source) = args[0] else {
                            unreachable!()
                        };
                        args[0] = ArgumentTransfer::Borrow(source);
                    }
                }
            }
        }
        assert!(verify_physical_module(&invalid)
            .unwrap_err()
            .message
            .contains("transfer disagrees with parameter passing"));
    }

    fn i64_layout() -> PhysicalLayout {
        PhysicalLayout {
            size: 8,
            align: 8,
            repr: PhysicalRepr::Integer { bits: 64 },
        }
    }

    fn target() -> PhysicalTarget {
        let mut target = PhysicalTarget::new("x86_64-unknown-linux-gnu", "e-p:64:64-i64:64");
        target.insert_layout(ResolvedTy::I64, i64_layout());
        target.insert_layout(ResolvedTy::Duration, i64_layout());
        target.insert_layout(
            ResolvedTy::Bool,
            PhysicalLayout {
                size: 1,
                align: 1,
                repr: PhysicalRepr::Integer { bits: 8 },
            },
        );
        target.insert_layout(
            ResolvedTy::U8,
            PhysicalLayout {
                size: 1,
                align: 1,
                repr: PhysicalRepr::Integer { bits: 8 },
            },
        );
        target.insert_layout(
            ResolvedTy::I8,
            PhysicalLayout {
                size: 1,
                align: 1,
                repr: PhysicalRepr::Integer { bits: 8 },
            },
        );
        target.insert_layout(
            ResolvedTy::U64,
            PhysicalLayout {
                size: 8,
                align: 8,
                repr: PhysicalRepr::Integer { bits: 64 },
            },
        );
        target.insert_layout(
            ResolvedTy::String,
            PhysicalLayout {
                size: 8,
                align: 8,
                repr: PhysicalRepr::Pointer,
            },
        );
        target.insert_layout(
            ResolvedTy::Bytes,
            PhysicalLayout {
                size: 16,
                align: 8,
                repr: PhysicalRepr::Struct(vec![
                    PhysicalLayout {
                        size: 8,
                        align: 8,
                        repr: PhysicalRepr::Pointer,
                    },
                    PhysicalLayout {
                        size: 4,
                        align: 4,
                        repr: PhysicalRepr::Integer { bits: 32 },
                    },
                    PhysicalLayout {
                        size: 4,
                        align: 4,
                        repr: PhysicalRepr::Integer { bits: 32 },
                    },
                ]),
            },
        );
        target.insert_layout(
            ResolvedTy::Unit,
            PhysicalLayout {
                size: 0,
                align: 1,
                repr: PhysicalRepr::Unit,
            },
        );
        target
    }

    fn utf8_target(module: &SemModule) -> PhysicalTarget {
        // This fixture uses the existing fixed 64-bit test target. Its two
        // variants have an eight-byte-aligned payload following a one-byte tag.
        fn variant(target: &mut PhysicalTarget, ty: ResolvedTy, cases: Vec<PhysicalLayout>) {
            let payload_size = cases.iter().map(|case| case.size).max().unwrap();
            let payload = PhysicalLayout {
                size: payload_size,
                align: 8,
                repr: PhysicalRepr::Array {
                    element: Box::new(i64_layout()),
                    len: u32::try_from(payload_size / 8).unwrap(),
                },
            };
            let object = PhysicalLayout {
                size: 8 + payload.size,
                align: 8,
                repr: PhysicalRepr::Struct(vec![
                    PhysicalLayout {
                        size: 1,
                        align: 1,
                        repr: PhysicalRepr::Integer { bits: 8 },
                    },
                    payload,
                ]),
            };
            target.insert_layout(ty.clone(), object.clone());
            target.insert_variant_layout(PhysicalVariantLayout {
                ty,
                is_indirect: false,
                object,
                variants: cases,
            });
        }
        let mut target = target();
        let option_ty = module.variant_shapes[1].enum_ty.clone();
        variant(
            &mut target,
            option_ty.clone(),
            vec![
                PhysicalLayout {
                    size: 8,
                    align: 8,
                    repr: PhysicalRepr::Struct(vec![i64_layout()]),
                },
                PhysicalLayout {
                    size: 0,
                    align: 1,
                    repr: PhysicalRepr::Struct(vec![]),
                },
            ],
        );
        let error_ty = module.aggregate_shapes[0].aggregate_ty.clone();
        let error_layout = PhysicalLayout {
            size: 24,
            align: 8,
            repr: PhysicalRepr::Struct(vec![
                i64_layout(),
                target.layout(&option_ty).unwrap().clone(),
            ]),
        };
        target.insert_layout(error_ty, error_layout.clone());
        let string = target.layout(&ResolvedTy::String).unwrap().clone();
        variant(
            &mut target,
            module.variant_shapes[0].enum_ty.clone(),
            vec![
                PhysicalLayout {
                    size: 8,
                    align: 8,
                    repr: PhysicalRepr::Struct(vec![string]),
                },
                PhysicalLayout {
                    size: 24,
                    align: 8,
                    repr: PhysicalRepr::Struct(vec![error_layout]),
                },
            ],
        );
        target
    }

    #[test]
    fn utf8_decode_rejects_a_foreign_optional_payload_descriptor() {
        let semantic = utf8_fixture::decode_module();
        let mut physical = lower_physical_module(&semantic, utf8_target(&semantic))
            .expect("valid UTF-8 runtime result must lower")
            .into_unverified();
        let PhysicalTerminator::RuntimeCall {
            action:
                PhysicalRuntimeAction::BytesDecodeUtf8 {
                    result, error_len, ..
                },
            failure,
            ..
        } = &mut physical.functions[0].blocks[0].terminator
        else {
            panic!("expected the typed decoder runtime action");
        };
        assert!(
            failure.is_none(),
            "invalid UTF-8 is not a native fault edge"
        );
        *error_len = *result;
        let error = verify_physical_module(&physical)
            .expect_err("Result storage cannot stand in for Option<i64>");
        assert!(
            error.message.contains("UTF-8 decode physical descriptors"),
            "{error:?}"
        );
    }

    fn test_struct_layout(fields: Vec<PhysicalLayout>) -> PhysicalLayout {
        let align = fields.iter().map(|field| field.align).max().unwrap_or(1);
        let mut size = 0_u64;
        for field in &fields {
            size = size.next_multiple_of(u64::from(field.align)) + field.size;
        }
        PhysicalLayout {
            size: size.next_multiple_of(u64::from(align)),
            align,
            repr: PhysicalRepr::Struct(fields),
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "the fixed test target realizes resource and composite layouts from one inventory"
    )]
    pub(super) fn target_for_inventory(module: &SemModule) -> PhysicalTarget {
        let inventory = physical_type_inventory(module);
        let mut target = target();
        // Source fixtures retain checked callable headers as well as bodies.
        // Realize their resource carriers from the same inventory as codegen.
        for resource in inventory.resources() {
            let layout = match resource.release.carrier().unwrap() {
                ResourceCarrier::Pointer => PhysicalLayout {
                    size: 8,
                    align: 8,
                    repr: PhysicalRepr::Pointer,
                },
                ResourceCarrier::I32 => PhysicalLayout {
                    size: 4,
                    align: 4,
                    repr: PhysicalRepr::Integer { bits: 32 },
                },
                ResourceCarrier::Record => continue,
            };
            target.insert_layout(resource.ty.clone(), layout);
        }
        for ty in inventory
            .types()
            .filter(|ty| collection_type_arguments(ty).is_some())
        {
            target.insert_layout(
                ty.clone(),
                PhysicalLayout {
                    size: 8,
                    align: 8,
                    repr: PhysicalRepr::Pointer,
                },
            );
        }
        for variant in inventory.variants().filter(|variant| variant.is_indirect) {
            target.insert_layout(
                variant.ty.clone(),
                PhysicalLayout {
                    size: 8,
                    align: 8,
                    repr: PhysicalRepr::Pointer,
                },
            );
        }
        let mut aggregates = inventory.aggregates().collect::<Vec<_>>();
        let mut variants = inventory.variants().collect::<Vec<_>>();
        while !aggregates.is_empty() || !variants.is_empty() {
            let previous = aggregates.len() + variants.len();
            aggregates.retain(|aggregate| {
                let Some(fields) = aggregate
                    .fields
                    .iter()
                    .map(|field| target.layout(field).cloned())
                    .collect::<Option<Vec<_>>>()
                else {
                    return true;
                };
                target.insert_layout(aggregate.ty.clone(), test_struct_layout(fields));
                false
            });
            variants.retain(|variant| {
                let Some(cases) = variant
                    .variants
                    .iter()
                    .map(|fields| {
                        fields
                            .iter()
                            .map(|field| target.layout(field).cloned())
                            .collect::<Option<Vec<_>>>()
                            .map(test_struct_layout)
                    })
                    .collect::<Option<Vec<_>>>()
                else {
                    return true;
                };
                let align = cases.iter().map(|case| case.align).max().unwrap();
                let size = cases.iter().map(|case| case.size).max().unwrap();
                let count = size.div_ceil(u64::from(align));
                let payload = PhysicalLayout {
                    size: count * u64::from(align),
                    align,
                    repr: PhysicalRepr::Array {
                        element: Box::new(PhysicalLayout {
                            size: u64::from(align),
                            align,
                            repr: PhysicalRepr::Integer {
                                bits: u16::try_from(align * 8).unwrap(),
                            },
                        }),
                        len: u32::try_from(count).unwrap(),
                    },
                };
                let object = test_struct_layout(vec![
                    PhysicalLayout {
                        size: 1,
                        align: 1,
                        repr: PhysicalRepr::Integer { bits: 8 },
                    },
                    payload,
                ]);
                if !variant.is_indirect {
                    target.insert_layout(variant.ty.clone(), object.clone());
                }
                target.insert_variant_layout(PhysicalVariantLayout {
                    ty: variant.ty.clone(),
                    is_indirect: variant.is_indirect,
                    object,
                    variants: cases,
                });
                false
            });
            assert!(
                aggregates.len() + variants.len() < previous,
                "test layouts must terminate through concrete fields or vector pointers"
            );
        }
        target
    }

    fn lower_source(source: &str) -> SemModule {
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:#?}",
            parsed.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
        let facts = checker.check_program(&parsed.program);
        assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
        let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
        assert!(
            hir.diagnostics.is_empty(),
            "HIR errors: {:#?}",
            hir.diagnostics
        );
        let lowered = hew_sir::lower_module(&hir.module, &facts);
        assert!(
            lowered.statuses.iter().any(|status| {
                status.name == "main"
                    && matches!(status.status, hew_sir::SirLoweringStatus::Lowered)
            }),
            "source main did not lower: {:#?}",
            lowered.statuses
        );
        assert!(
            !lowered.module.functions.is_empty(),
            "source fixture must exercise a lowered function"
        );
        lowered.module
    }

    #[test]
    fn extern_byte_result_rejects_storage_aggregate_return() {
        let semantic = borrow_fixture::lower_source(
            r#"
            extern "C" { fn make_bytes() -> bytes; }
            fn main() { println(unsafe { make_bytes() }.len()); }
            "#,
        );
        let mut physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
            .expect("declared byte result must have a C return ABI")
            .into_unverified();
        let call = physical
            .functions
            .iter_mut()
            .flat_map(|function| &mut function.blocks)
            .find_map(|block| match &mut block.terminator {
                PhysicalTerminator::ExternCall { result_abi, .. } => Some(result_abi),
                _ => None,
            })
            .expect("source must reach its declared byte producer");
        *call = PhysicalExternResultAbi::Direct;
        let error = verify_physical_module(&physical)
            .expect_err("a byte storage aggregate is not the C return ABI");
        assert!(error.message.contains("extern result ABI"), "{error:?}");
    }

    fn module_with_return() -> SemModule {
        let declaration = DefId::for_test("main");
        let callable = SemCallable {
            id: CallableId(0),
            function: ItemId(0),
            declaration: declaration.clone(),
            instance: CallableInstance::Monomorphic,
            symbol: "main".to_string(),
            source_origin: FunctionSourceOrigin::RootUnit,
            signature: SemSignature {
                params: vec![],
                return_ty: ResolvedTy::I64,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        };
        let value = ValueDef {
            id: ValueId(0),
            ty: ResolvedTy::I64,
            own: OwnKind::None,
        };
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration,
            name: "main".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: vec![],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: vec![SemOp {
                    id: hew_sir::OpId(0),
                    results: vec![value],
                    kind: SemOpKind::ConstInteger(7),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value: ValueId(0) },
                        decision: BoundaryDecision::Move,
                    }),
                },
            }],
            places: vec![],
            bindings: vec![],
        };
        let mut type_facts = BTreeMap::new();
        type_facts.insert(
            TypeInstanceKey(ResolvedTy::I64),
            TypeFacts {
                class: ValueClass::BitCopy,
                clone: CloneKind::Bits,
                send: SendFact::Known(true),
                hash: true,
                eq: true,
            },
        );
        SemModule {
            actors: Vec::new(),
            supervisors: Vec::new(),
            resources: BTreeMap::new(),
            closures: Vec::new(),
            vtables: Vec::new(),
            value_capabilities: BTreeMap::new(),
            callables: vec![callable],
            generic_templates: vec![],
            root_unit_callables: vec![CallableId(0)],
            entry_exit_plan: None,
            entry_callable: Some(CallableId(0)),
            functions: vec![function],
            aggregate_shapes: vec![],
            variant_shapes: vec![],
            type_facts,
            string_literals: BTreeMap::new(),
            bytes_literals: BTreeMap::new(),
        }
    }

    fn module_with_call_and_unwind() -> SemModule {
        let mut module = module_with_return();
        let helper_declaration = DefId::for_test("helper");
        let mut helper = module.functions[0].clone();
        helper.id = ItemId(1);
        helper.callable = CallableId(1);
        helper.declaration.clone_from(&helper_declaration);
        helper.name = "helper".to_string();
        helper.source_origin = FunctionSourceOrigin::Unknown;
        module.callables.push(SemCallable {
            id: CallableId(1),
            function: ItemId(1),
            declaration: helper_declaration,
            instance: CallableInstance::Monomorphic,
            symbol: "helper".to_string(),
            source_origin: FunctionSourceOrigin::Unknown,
            signature: SemSignature {
                params: vec![],
                return_ty: ResolvedTy::I64,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        let main = &mut module.functions[0];
        main.blocks = vec![
            SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: vec![],
                terminator: SemTerminator::Call {
                    id: hew_sir::OpId(0),
                    callee: CallableId(1),
                    args: vec![],
                    result: CallResult::Value(ValueDef {
                        id: ValueId(0),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }),
                    normal: Some(Edge {
                        target: BlockId(1),
                        args: vec![Operand { value: ValueId(0) }],
                    }),
                    unwind: CallUnwind::Cleanup(Edge {
                        target: BlockId(2),
                        args: vec![],
                    }),
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![hew_sir::BlockArg {
                    value: ValueId(1),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }],
                ops: vec![],
                terminator: SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value: ValueId(1) },
                        decision: BoundaryDecision::Move,
                    }),
                },
            },
            SemBlock {
                id: BlockId(2),
                args: vec![],
                ops: vec![],
                terminator: SemTerminator::ResumeUnwind,
            },
        ];
        module.functions.push(helper);
        module
    }

    fn module_with_checked_add() -> SemModule {
        let mut module = module_with_return();
        module.functions[0].blocks = vec![
            SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: vec![
                    SemOp {
                        id: hew_sir::OpId(0),
                        results: vec![ValueDef {
                            id: ValueId(0),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                        kind: SemOpKind::ConstInteger(40),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: hew_sir::OpId(1),
                        results: vec![ValueDef {
                            id: ValueId(1),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                        kind: SemOpKind::ConstInteger(2),
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::CheckedBinary {
                    id: hew_sir::OpId(2),
                    op: BinaryOp::Add,
                    lhs: Operand { value: ValueId(0) },
                    rhs: Operand { value: ValueId(1) },
                    result: ValueDef {
                        id: ValueId(2),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    },
                    normal: Edge {
                        target: BlockId(1),
                        args: vec![Operand { value: ValueId(2) }],
                    },
                    failures: vec![CheckedFailure {
                        kind: TrapKind::IntegerOverflow,
                        edge: Edge {
                            target: BlockId(2),
                            args: vec![],
                        },
                    }],
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![hew_sir::BlockArg {
                    value: ValueId(3),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }],
                ops: vec![],
                terminator: SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value: ValueId(3) },
                        decision: BoundaryDecision::Move,
                    }),
                },
            },
            SemBlock {
                id: BlockId(2),
                args: vec![],
                ops: vec![],
                terminator: SemTerminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                },
            },
        ];
        module
    }

    #[test]
    fn wire_schema_cannot_read_another_physical_field() {
        let semantic = lower_source(
            r#"
            #[wire]
            type WireRecordProbe { label: string @7, code: u8 @2 }
            fn main() {
                let message = WireRecordProbe { label: "owned", code: 7 };
                let encoded = message.encode();
                let decoded = WireRecordProbe.decode(encoded);
                println(decoded.label);
            }
        "#,
        );
        let mut physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
            .unwrap()
            .into_unverified();
        let mut changed = false;
        for block in physical
            .functions
            .iter_mut()
            .flat_map(|function| &mut function.blocks)
        {
            if let PhysicalTerminator::WireCodec { plan, .. } = &mut block.terminator {
                let plan = std::sync::Arc::make_mut(plan);
                let SemWireKind::Record { fields, .. } = &mut plan.kind else {
                    panic!("record codec");
                };
                fields[0].index = 0;
                changed = true;
                break;
            }
        }
        assert!(changed);
        assert!(verify_physical_module(&physical)
            .unwrap_err()
            .message
            .contains("wire schema selects a different physical value shape"));
    }

    #[test]
    fn lowers_scalar_return_to_private_result_out_contract() {
        let physical = lower_physical_module(&module_with_return(), target()).expect("lower");
        let module = physical.module();
        assert_eq!(module.entry_callable, Some(CallableId(0)));
        assert_eq!(module.callables[0].return_layout, Some(i64_layout()));
        assert!(matches!(
            module.functions[0].blocks[0].terminator,
            PhysicalTerminator::Return {
                value: Some(ReturnTransfer::Move(StorageId(0)))
            }
        ));
    }

    #[test]
    fn inventories_only_types_used_by_concrete_sir_bodies() {
        let module = lower_source(
            r"
            fn pair_second(x: i64, y: i64) -> i64 {
                let pair = (x, y);
                pair.1
            }

            fn main() -> i64 { pair_second(0, 42) }
            ",
        );
        let inventory = physical_type_inventory(&module);
        assert!(inventory.contains(&ResolvedTy::I64));
        assert!(inventory.contains(&ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64,])));
        assert!(!inventory.contains(&ResolvedTy::String));
    }

    #[test]
    fn lowers_scalar_tuple_construction_and_projection_to_explicit_ops() {
        let module = lower_source(
            r"
            fn main() -> i64 {
                let pair = (0, 42);
                pair.1
            }
            ",
        );
        let verified =
            lower_physical_module(&module, target_for_inventory(&module)).expect("physical tuple");
        let operations = verified
            .module()
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| &block.ops)
            .collect::<Vec<_>>();
        assert!(operations.iter().any(|operation| matches!(
            operation,
            PhysicalOp::TupleMake { elements, .. } if elements.len() == 2
        )));
        assert!(operations
            .iter()
            .any(|operation| matches!(operation, PhysicalOp::TupleGet { index: 1, .. })));
    }

    #[test]
    fn lowers_owned_aggregate_operations_with_exact_recursive_glue() {
        let module = lower_source(
            r#"
            type Packet { label: string, payload: bytes }

            fn main() {
                let pair = ("tuple", b"T");
                let pair_copy = pair;
                let tuple_label = pair_copy.0;
                let packet = Packet { payload: b"P", label: "record" };
                let packet_copy = packet;
                let record_label = packet_copy.label;
            }
            "#,
        );
        let verified = lower_physical_module(&module, target_for_inventory(&module))
            .expect("owned aggregate physical lowering");
        let physical = verified.module();
        assert_eq!(physical.aggregate_glue.len(), 2);
        assert!(physical.aggregate_glue.iter().all(|glue| {
            matches!(glue.fields[0].clone, Some(CloneAction::StringRetain))
                && matches!(glue.fields[1].clone, Some(CloneAction::BytesRetain))
                && matches!(glue.fields[0].destroy, Some(DestroyAction::StringRelease))
                && matches!(glue.fields[1].destroy, Some(DestroyAction::BytesRelease))
        }));
        let operations = physical
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| &block.ops)
            .collect::<Vec<_>>();
        assert!(operations
            .iter()
            .any(|operation| matches!(operation, PhysicalOp::AggregateMake { .. })));
        assert!(operations.iter().any(|operation| matches!(
            operation,
            PhysicalOp::Clone {
                action: CloneAction::Aggregate(_),
                ..
            }
        )));
        let projected_copies = physical
            .functions
            .iter()
            .flat_map(|function| {
                function
                    .blocks
                    .iter()
                    .flat_map(|block| &block.ops)
                    .filter_map(|operation| {
                        let PhysicalOp::Clone {
                            source,
                            action: CloneAction::StringRetain,
                            ..
                        } = operation
                        else {
                            return None;
                        };
                        function.place_storage.get(source)
                    })
            })
            .collect::<Vec<_>>();
        assert_eq!(projected_copies.len(), 2);
        assert!(projected_copies
            .iter()
            .all(|projection| { projection.path.len() == 1 && projection.path[0].field == 0 }));
        assert!(operations.iter().any(|operation| matches!(
            operation,
            PhysicalOp::StorageDead {
                destroy: Some(DestroyAction::Aggregate(_)),
                ..
            }
        )));
    }

    fn borrowed_aggregate_fixture() -> PhysicalModule {
        let semantic = borrow_fixture::nested_borrow_module();
        lower_physical_module(&semantic, target_for_inventory(&semantic))
            .expect("nested field loans lower through physical storage")
            .into_unverified()
    }

    #[test]
    fn borrowed_aggregate_fields_retain_exact_sir_parent_dependencies() {
        let physical = borrowed_aggregate_fixture();
        let function = &physical.functions[0];
        let mut loans = 0;
        for operation in function.blocks.iter().flat_map(|block| &block.ops) {
            if let PhysicalOp::AggregateProjectBorrow {
                dest, aggregate, ..
            } = operation
            {
                loans += 1;
                let slot = &function.storage[dest.0 as usize];
                assert_eq!(slot.own, OwnKind::Guaranteed);
                assert_eq!(slot.borrow_parent, Some(*aggregate));
            }
        }
        assert_eq!(loans, 2);
    }

    #[test]
    fn physical_field_loans_refuse_wrong_fields_and_forged_dependencies() {
        for wrong_field in [false, true] {
            let mut physical = borrowed_aggregate_fixture();
            let function = &mut physical.functions[0];
            let operation = function
                .blocks
                .iter_mut()
                .flat_map(|block| &mut block.ops)
                .find(|op| matches!(op, PhysicalOp::AggregateProjectBorrow { .. }))
                .unwrap();
            let PhysicalOp::AggregateProjectBorrow { dest, field, .. } = operation else {
                unreachable!()
            };
            if wrong_field {
                *field = u32::MAX;
            } else {
                function.storage[dest.0 as usize].borrow_parent = None;
            }
            let error = verify_physical_module(&physical).expect_err("malformed field loan");
            assert!(
                error.message.contains("out of bounds")
                    || error.message.contains("no SIR parent dependency"),
                "{error}"
            );
        }
    }

    #[test]
    fn physical_borrow_roots_cannot_end_before_their_dependent_reads() {
        for end_owner in [false, true] {
            let mut physical = borrowed_aggregate_fixture();
            let function = &mut physical.functions[0];
            let loans = function
                .storage
                .iter()
                .filter(|slot| slot.borrow_parent.is_some())
                .map(|slot| (slot.id, slot.borrow_parent.unwrap()))
                .collect::<Vec<_>>();
            let (parent, owner) = loans[0];
            let value = if end_owner { owner } else { parent };
            let mut cleanup = None;
            for block in &mut function.blocks {
                if let Some(index) = block.ops.iter().position(|op| {
                    matches!(op,
                    PhysicalOp::Destroy { source, .. } | PhysicalOp::EndBorrow { source }
                        if *source == value)
                }) {
                    cleanup = Some(block.ops.remove(index));
                    break;
                }
            }
            let read = vector_block(function, VecValueOp::Index);
            read.ops.push(cleanup.unwrap());
            let error = verify_physical_module(&physical).expect_err("live dependent loan");
            assert!(error.message.contains("dependent loan is live"), "{error}");
        }
    }

    #[test]
    fn physical_fault_cleanup_must_end_its_field_loans() {
        let mut physical = borrowed_aggregate_fixture();
        let fault = physical.functions[0]
            .blocks
            .iter_mut()
            .find(|block| {
                matches!(
                    block.terminator,
                    PhysicalTerminator::CheckedRaiseFault { .. }
                )
            })
            .unwrap();
        let before = fault.ops.len();
        fault
            .ops
            .retain(|op| !matches!(op, PhysicalOp::EndBorrow { .. }));
        assert!(fault.ops.len() < before);
        let error = verify_physical_module(&physical).expect_err("loan cleanup on fault edge");
        assert!(error.message.contains("dependent loan is live"), "{error}");
    }

    fn borrowed_variant_fixture() -> PhysicalModule {
        let semantic = lower_source(
            r#"
            enum Choice { Values(Vec<string>, i64), Empty }

            fn drive(consume choice: Option<Choice>) -> string {
                match choice {
                    .Some(.Values(_, 0)) => "zero",
                    .Some(.Values(values, weight)) => { let _kept = values; "kept" }
                    .Some(.Empty) => "empty",
                    .None => "none",
                }
            }

            fn keep_text(value: string) {}
            fn main() {
                keep_text(drive(Some(Choice.Values(["word"], 7))));
            }
            "#,
        );
        lower_physical_module(&semantic, target_for_inventory(&semantic))
            .expect("probed variant payloads lower through physical storage")
            .into_unverified()
    }

    #[test]
    fn borrowed_variant_payloads_retain_their_enum_dependency() {
        let physical = borrowed_variant_fixture();
        let mut loans = 0;
        for function in &physical.functions {
            for operation in function.blocks.iter().flat_map(|block| &block.ops) {
                if let PhysicalOp::VariantProjectBorrow { dest, source, .. } = operation {
                    loans += 1;
                    let slot = &function.storage[dest.0 as usize];
                    assert_eq!(slot.own, OwnKind::Guaranteed);
                    assert_eq!(slot.borrow_parent, Some(*source));
                }
            }
        }
        assert_eq!(
            loans, 2,
            "each candidate borrows the generator payload once"
        );
        verify_physical_module(&physical).expect("probed payload loans verify");
    }

    #[test]
    fn physical_variant_loans_refuse_forged_dependencies_and_fields() {
        for wrong_field in [false, true] {
            let mut physical = borrowed_variant_fixture();
            let function = physical
                .functions
                .iter_mut()
                .find(|function| {
                    function
                        .blocks
                        .iter()
                        .flat_map(|block| &block.ops)
                        .any(|op| matches!(op, PhysicalOp::VariantProjectBorrow { .. }))
                })
                .unwrap();
            let operation = function
                .blocks
                .iter_mut()
                .flat_map(|block| &mut block.ops)
                .find(|op| matches!(op, PhysicalOp::VariantProjectBorrow { .. }))
                .unwrap();
            let PhysicalOp::VariantProjectBorrow { dest, field, .. } = operation else {
                unreachable!()
            };
            if wrong_field {
                *field = u32::MAX;
            } else {
                function.storage[dest.0 as usize].borrow_parent = None;
            }
            let error = verify_physical_module(&physical).expect_err("malformed payload loan");
            assert!(
                error.message.contains("out of bounds")
                    || error.message.contains("no SIR parent dependency"),
                "{error}"
            );
        }
    }

    #[test]
    fn verifier_rejects_variant_payload_carriers_that_cannot_hold_every_case() {
        let module = lower_source(
            r#"
            enum Payload { Wide(i64, string), Empty }

            fn main() -> i64 {
                let payload = Payload.Wide(7, "wide");
                match payload {
                    .Wide(number, text) => { let copy = text; number },
                    .Empty => 0,
                }
            }
            "#,
        );
        let enum_ty = module.variant_shapes[0].enum_ty.clone();
        let wide = PhysicalLayout {
            size: 16,
            align: 8,
            repr: PhysicalRepr::Struct(vec![
                i64_layout(),
                target().layout(&ResolvedTy::String).unwrap().clone(),
            ]),
        };
        let empty = PhysicalLayout {
            size: 0,
            align: 1,
            repr: PhysicalRepr::Struct(vec![]),
        };
        let target_with_carrier = |carrier: PhysicalLayout| {
            let mut target = target_for_inventory(&module);
            let object = PhysicalLayout {
                size: 24,
                align: 8,
                repr: PhysicalRepr::Struct(vec![
                    PhysicalLayout {
                        size: 1,
                        align: 1,
                        repr: PhysicalRepr::Integer { bits: 8 },
                    },
                    carrier,
                ]),
            };
            target.insert_layout(enum_ty.clone(), object.clone());
            target.insert_variant_layout(PhysicalVariantLayout {
                ty: enum_ty.clone(),
                is_indirect: false,
                object,
                variants: vec![wide.clone(), empty.clone()],
            });
            target
        };

        let short = target_with_carrier(PhysicalLayout {
            size: 8,
            align: 8,
            repr: PhysicalRepr::Array {
                element: Box::new(i64_layout()),
                len: 1,
            },
        });
        let short_error = lower_physical_module(&module, short)
            .expect_err("variant payload carrier must fit its widest case");
        assert!(short_error.message.contains("payload carrier"));

        let under_aligned = target_with_carrier(PhysicalLayout {
            size: 16,
            align: 1,
            repr: PhysicalRepr::Array {
                element: Box::new(PhysicalLayout {
                    size: 1,
                    align: 1,
                    repr: PhysicalRepr::Integer { bits: 8 },
                }),
                len: 16,
            },
        });
        let alignment_error = lower_physical_module(&module, under_aligned)
            .expect_err("variant payload carrier must meet every case alignment");
        assert!(alignment_error.message.contains("payload carrier"));
    }

    #[test]
    fn verifier_refuses_malformed_aggregate_copy_and_consumption() {
        let module = lower_source(
            r#"
            type Packet { first: string, second: string }
            fn main() {
                let packet = Packet { first: "one", second: "two" };
                let label = packet.first;
            }
            "#,
        );
        let mut physical = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid aggregate physical lowering")
            .into_unverified();
        let mut duplicate_consume = physical.clone();
        let fields = duplicate_consume
            .functions
            .iter_mut()
            .flat_map(|function| &mut function.blocks)
            .flat_map(|block| &mut block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::AggregateMake { fields, .. } => Some(fields),
                _ => None,
            })
            .expect("aggregate construction");
        fields[1] = fields[0];
        let error = verify_physical_module(&duplicate_consume)
            .expect_err("aggregate construction must not consume one owner twice");
        assert!(error.message.contains("more than once"));

        let mut bad_path = physical.clone();
        let function = &mut physical.functions[0];
        let projected_sources = function
            .place_storage
            .keys()
            .copied()
            .collect::<BTreeSet<_>>();
        let (source, action) = function
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::Clone { source, action, .. } if projected_sources.contains(source) => {
                    Some((*source, action))
                }
                _ => None,
            })
            .expect("clone from an aggregate field alias");
        assert_eq!(*action, CloneAction::StringRetain);
        assert_eq!(function.place_storage[&source].path[0].field, 0);
        *action = CloneAction::BytesRetain;
        let error = verify_physical_module(&physical)
            .expect_err("projected clone must use its exact field recipe");
        assert!(error.message.contains("physical clone action"), "{error:?}");

        bad_path.functions[0]
            .place_storage
            .get_mut(&source)
            .unwrap()
            .path[0]
            .field = u32::MAX;
        let error = verify_physical_module(&bad_path)
            .expect_err("projected clone must address a declared aggregate field");
        assert!(error.message.contains("aggregate"), "{error:?}");
    }

    #[test]
    fn verifier_rejects_owned_or_out_of_bounds_tuple_operations() {
        let module = lower_source(
            r"
            fn main() -> i64 {
                let pair = (0, 42);
                pair.1
            }
            ",
        );

        let mut owned_tuple = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid physical tuple")
            .into_unverified();
        let tuple_dest = owned_tuple.functions[0]
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::TupleMake { dest, .. } => Some(*dest),
                _ => None,
            })
            .expect("tuple construction");
        owned_tuple.functions[0].storage[tuple_dest.0 as usize].own = OwnKind::Owned;
        let error = verify_physical_module(&owned_tuple)
            .expect_err("physical tuple must not infer aggregate ownership");
        assert!(error.message.contains("limited to no-drop values"));

        let mut bad_index = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid physical tuple")
            .into_unverified();
        let index = bad_index.functions[0]
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::TupleGet { index, .. } => Some(index),
                _ => None,
            })
            .expect("tuple projection");
        *index = 2;
        let error = verify_physical_module(&bad_index)
            .expect_err("physical tuple projection must stay in bounds");
        assert!(error.message.contains("index 2 is out of bounds"));
    }

    #[test]
    fn refuses_a_missing_target_layout_before_codegen() {
        let error = lower_physical_module(
            &module_with_return(),
            PhysicalTarget::new("x86_64-unknown-linux-gnu", "e-p:64:64"),
        )
        .expect_err("layout must be required");
        assert!(error.message.contains("no concrete layout for `i64`"));
    }

    #[test]
    fn runtime_families_lower_to_closed_physical_actions() {
        let module = lower_source(
            r#"
            fn main() -> i64 {
                let upper = "core".to_upper();
                if upper != "CORE" { return 1; }
                if !upper.starts_with("CO") { return 2; }
                if upper.is_empty() { return 3; }
                println(upper);
                0
            }
            "#,
        );
        let verified = lower_physical_module(&module, target_for_inventory(&module))
            .expect("physical runtime calls");
        let actions = verified
            .module()
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .filter_map(|block| match block.terminator {
                PhysicalTerminator::RuntimeCall { action, .. } => Some(action),
                _ => None,
            })
            .collect::<std::collections::HashSet<_>>();
        assert_eq!(
            actions,
            std::collections::HashSet::from([
                PhysicalRuntimeAction::StringEquals,
                PhysicalRuntimeAction::StringStartsWith,
                PhysicalRuntimeAction::StringIsEmpty,
                PhysicalRuntimeAction::StringToUppercase,
                PhysicalRuntimeAction::Print {
                    kind: hew_types::runtime_call::PrintKind::Str,
                    newline: true
                },
            ])
        );
    }

    #[test]
    fn scalar_print_lowers_to_the_exact_physical_runtime_action() {
        let module = lower_source("fn main() { println(1 + 2); }");
        let verified = lower_physical_module(&module, target_for_inventory(&module))
            .expect("physical scalar print");
        assert!(verified
            .module()
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .any(|block| matches!(
                block.terminator,
                PhysicalTerminator::RuntimeCall {
                    action: PhysicalRuntimeAction::Print {
                        kind: hew_types::runtime_call::PrintKind::I64,
                        newline: true
                    },
                    ..
                }
            )));
    }

    #[test]
    fn bytes_transform_and_bounds_failure_are_physical_contracts() {
        let module = lower_source(include_str!(
            "../../tests/core-acceptance/cases/bytes-copy-mutate.hew"
        ));
        let verified = lower_physical_module(&module, target_for_inventory(&module))
            .expect("physical bytes calls");
        let actions = verified
            .module()
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .filter_map(|block| match block.terminator {
                PhysicalTerminator::RuntimeCall { action, .. } => Some(action),
                _ => None,
            })
            .collect::<std::collections::HashSet<_>>();
        assert!(actions.contains(&PhysicalRuntimeAction::StringToBytesOwned));
        assert!(actions.contains(&PhysicalRuntimeAction::BytesPushOwned));
        assert!(actions.contains(&PhysicalRuntimeAction::BytesLen));
        assert!(actions.contains(&PhysicalRuntimeAction::BytesIndex));
    }

    #[test]
    fn verifier_rejects_changed_runtime_transfer_and_failure_contracts() {
        let module = lower_source(include_str!(
            "../../tests/core-acceptance/cases/bytes-copy-mutate.hew"
        ));
        let mut wrong_transfer = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid physical bytes module")
            .into_unverified();
        let push_args = wrong_transfer
            .functions
            .iter_mut()
            .flat_map(|function| &mut function.blocks)
            .find_map(|block| match &mut block.terminator {
                PhysicalTerminator::RuntimeCall {
                    action: PhysicalRuntimeAction::BytesPushOwned,
                    args,
                    ..
                } => Some(args),
                _ => None,
            })
            .expect("bytes push physical action");
        let moved = match push_args[0] {
            ArgumentTransfer::Move(source) => source,
            other => panic!("expected moved bytes receiver, got {other:?}"),
        };
        push_args[0] = ArgumentTransfer::Borrow(moved);
        let error = verify_physical_module(&wrong_transfer)
            .expect_err("borrow must not replace the bytes owner move");
        assert!(error.message.contains("argument disagrees"));

        let mut missing_failure = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid physical bytes module")
            .into_unverified();
        let failure = missing_failure
            .functions
            .iter_mut()
            .flat_map(|function| &mut function.blocks)
            .find_map(|block| match &mut block.terminator {
                PhysicalTerminator::RuntimeCall {
                    action: PhysicalRuntimeAction::BytesIndex,
                    failure,
                    ..
                } => Some(failure),
                _ => None,
            })
            .expect("bytes index physical action");
        *failure = None;
        let error = verify_physical_module(&missing_failure)
            .expect_err("bytes index must retain its SIR-authored failure edge");
        assert!(error.message.contains("failure edge disagrees"));
    }

    #[test]
    fn copy_boundary_is_resolved_to_a_concrete_clone_action() {
        let mut module = module_with_return();
        let function = &mut module.functions[0];
        let SemTerminator::Return { value: Some(value) } = &mut function.blocks[0].terminator
        else {
            panic!("return fixture");
        };
        value.decision = BoundaryDecision::Copy;
        let physical =
            lower_physical_module(&module, target_for_inventory(&module)).expect("lower");
        assert!(matches!(
            physical.module().functions[0].blocks[0].terminator,
            PhysicalTerminator::Return {
                value: Some(ReturnTransfer::Clone {
                    action: CloneAction::Bitwise,
                    ..
                })
            }
        ));
    }

    #[test]
    fn verifier_rejects_noncanonical_callable_identity() {
        let verified = lower_physical_module(&module_with_return(), target()).expect("lower");
        let mut physical = verified.into_unverified();
        physical.callables[0].id = CallableId(4);
        let error = verify_physical_module(&physical).expect_err("identity must be checked");
        assert!(error.message.contains("canonical table index"));
    }

    #[test]
    fn call_result_is_initialized_only_on_the_normal_edge() {
        let verified =
            lower_physical_module(&module_with_call_and_unwind(), target()).expect("lower call");
        let main = &verified.module().functions[0];
        assert!(matches!(
            main.blocks[0].terminator,
            PhysicalTerminator::Call {
                result: Some(StorageId(0)),
                unwind: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn verifier_rejects_call_result_missing_from_normal_contract() {
        let verified =
            lower_physical_module(&module_with_call_and_unwind(), target()).expect("lower call");
        let mut physical = verified.into_unverified();
        let PhysicalTerminator::Call { result, .. } =
            &mut physical.functions[0].blocks[0].terminator
        else {
            panic!("call fixture");
        };
        *result = None;
        let error = verify_physical_module(&physical).expect_err("result-out must be checked");
        assert!(error.message.contains("result-out presence"));
    }

    #[test]
    fn verifier_rejects_call_result_read_on_the_unwind_edge() {
        let verified =
            lower_physical_module(&module_with_call_and_unwind(), target()).expect("lower call");
        let mut physical = verified.into_unverified();
        let PhysicalTerminator::Call { unwind, .. } =
            &mut physical.functions[0].blocks[0].terminator
        else {
            panic!("call fixture");
        };
        *unwind = Some(PhysicalEdge {
            target: BlockId(1),
            transfers: vec![(StorageId(0), StorageId(1))],
            leaf_transfers: vec![],
        });
        let error = verify_physical_module(&physical).expect_err("fault cannot expose result");
        assert!(error.message.contains("reads uninitialized storage 0"));
    }

    #[test]
    fn verifier_rejects_propagating_an_uninitialized_fault() {
        let verified = lower_physical_module(&module_with_return(), target()).expect("lower");
        let mut physical = verified.into_unverified();
        physical.functions[0].blocks[0].terminator = PhysicalTerminator::PropagateFault;
        let error = verify_physical_module(&physical).expect_err("fault must be initialized");
        assert!(error.message.contains("fault that is not initialized"));
    }

    #[test]
    fn verifier_rejects_terminals_that_abandon_an_active_fault() {
        let verified =
            lower_physical_module(&module_with_call_and_unwind(), target()).expect("lower call");
        for (terminal, expected) in [
            (
                PhysicalTerminator::Trap(TrapKind::IntegerOverflow),
                "creates a trap while an earlier fault is active",
            ),
            (
                PhysicalTerminator::Unreachable,
                "abandons an active fault at unreachable",
            ),
        ] {
            let mut physical = verified.clone().into_unverified();
            physical.functions[0]
                .blocks
                .iter_mut()
                .find(|block| block.id == BlockId(2))
                .expect("unwind cleanup block")
                .terminator = terminal;
            let error = verify_physical_module(&physical)
                .expect_err("an active fault owner must be propagated exactly once");
            assert!(error.message.contains(expected), "{}", error.message);
        }
    }

    #[test]
    fn verifier_checks_assign_source_before_initialization_analysis() {
        let module = lower_source(
            r#"
            fn main() {
                let value = "first";
            }
            "#,
        );
        let mut physical = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid assignment")
            .into_unverified();
        let operation = physical.functions[0]
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find(|operation| {
                matches!(
                    operation,
                    PhysicalOp::StorageDead {
                        destroy: Some(DestroyAction::StringRelease),
                        ..
                    }
                )
            })
            .expect("physical string Local cleanup");
        let PhysicalOp::StorageDead { storage: dest, .. } = *operation else {
            unreachable!("matched storage lifetime end")
        };
        *operation = PhysicalOp::Assign {
            dest,
            source: StorageId(u32::MAX),
            destroy_old: Some(DestroyAction::StringRelease),
        };
        let error = verify_physical_module(&physical)
            .expect_err("invalid assignment storage must fail without indexing it");
        assert!(error.message.contains("unknown physical storage"));
    }

    #[test]
    fn verifier_checks_binary_result_and_constant_payload_types() {
        let module = lower_source(
            r"
            fn main() -> i64 {
                let value = 1 &+ 2;
                if value == 3 { 0 } else { 1 }
            }
            ",
        );
        let verified = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid scalar operations");

        let mut wrong_binary = verified.clone().into_unverified();
        let bool_dest = wrong_binary.functions[0]
            .storage
            .iter()
            .find(|slot| slot.ty == ResolvedTy::Bool)
            .expect("boolean result")
            .id;
        let destination = wrong_binary.functions[0]
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::Binary {
                    op: BinaryOp::WrappingAdd,
                    dest,
                    ..
                } => Some(dest),
                _ => None,
            })
            .expect("wrapping binary operation");
        *destination = bool_dest;
        let error = verify_physical_module(&wrong_binary)
            .expect_err("binary result must retain the operand type");
        assert!(error.message.contains("binary result type"));

        let mut wrong_constant = verified.into_unverified();
        let constant = wrong_constant.functions[0]
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::Const {
                    value: value @ PhysicalConst::IntegerBits(_),
                    ..
                } => Some(value),
                _ => None,
            })
            .expect("integer constant");
        *constant = PhysicalConst::Bool(true);
        let error = verify_physical_module(&wrong_constant)
            .expect_err("constant payload must agree with its destination");
        assert!(error.message.contains("constant payload"));
    }

    #[test]
    fn full_range_u64_literal_lowers_to_its_exact_bit_pattern() {
        let module = lower_source("fn main() { let value: u64 = 18446744073709551615; }");
        let physical = lower_physical_module(&module, target_for_inventory(&module))
            .expect("full-range u64 literal")
            .into_unverified();
        let bits = physical.functions[0]
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::Const {
                    value: PhysicalConst::IntegerBits(bits),
                    ..
                } => Some(*bits),
                _ => None,
            })
            .expect("integer constant");
        assert_eq!(bits, u64::MAX);
    }

    #[test]
    fn negative_signed_literal_lowers_to_its_destination_width_twos_complement() {
        let module = lower_source("fn main() { let value: i8 = -128; }");
        let physical = lower_physical_module(&module, target_for_inventory(&module))
            .expect("i8 minimum literal")
            .into_unverified();
        let bits = physical.functions[0]
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::Const {
                    value: PhysicalConst::IntegerBits(bits),
                    ..
                } => Some(*bits),
                _ => None,
            })
            .expect("integer constant");
        // The eight-bit two's-complement encoding of -128, with no stray high
        // bits from the wider carrier.
        assert_eq!(bits, 0x80);
    }

    #[test]
    fn verifier_rejects_a_constant_with_bits_above_its_destination_width() {
        let module = lower_source("fn main() { let value: u8 = 7; }");
        let verified =
            lower_physical_module(&module, target_for_inventory(&module)).expect("u8 literal");
        let mut malformed = verified.into_unverified();
        let constant = malformed.functions[0]
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::Const {
                    value: value @ PhysicalConst::IntegerBits(_),
                    ..
                } => Some(value),
                _ => None,
            })
            .expect("integer constant");
        // Bit 8 cannot belong to an eight-bit destination: the backend would
        // emit the pattern as written and produce a different value.
        *constant = PhysicalConst::IntegerBits(0x107);
        let error = verify_physical_module(&malformed)
            .expect_err("a constant must be canonical for its destination width");
        assert!(error.message.contains("constant payload"));
    }

    #[test]
    fn verifier_rejects_missing_physical_literal_pool_entries() {
        let module = lower_source(r#"fn main() { let value = "literal"; }"#);
        let mut physical = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid string literal")
            .into_unverified();
        let literal = physical.functions[0]
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::Const {
                    value: PhysicalConst::String(id),
                    ..
                } => Some(id),
                _ => None,
            })
            .expect("string constant");
        *literal = hew_sir::StringLiteralId(u32::MAX);
        let error = verify_physical_module(&physical)
            .expect_err("physical string constant must name a module literal");
        assert!(error.message.contains("unknown literal"));
    }

    #[test]
    fn checked_binary_defines_its_result_only_on_the_normal_edge() {
        let verified = lower_physical_module(&module_with_checked_add(), target())
            .expect("checked add should lower");
        let PhysicalTerminator::CheckedBinary {
            result,
            normal,
            failures,
            ..
        } = &verified.module().functions[0].blocks[0].terminator
        else {
            panic!("checked add should remain an explicit physical terminator");
        };
        assert_eq!(*result, StorageId(2));
        assert_eq!(normal.target, BlockId(1));
        assert_eq!(failures.len(), 1);
        assert_eq!(failures[0].kind, TrapKind::IntegerOverflow);
        assert_eq!(failures[0].edge.target, BlockId(2));
    }

    #[test]
    fn verifier_rejects_checked_result_on_a_failure_edge() {
        let verified =
            lower_physical_module(&module_with_checked_add(), target()).expect("checked add");
        let mut physical = verified.into_unverified();
        let PhysicalTerminator::CheckedBinary { failures, .. } =
            &mut physical.functions[0].blocks[0].terminator
        else {
            panic!("checked add fixture");
        };
        failures[0].edge = PhysicalEdge {
            target: BlockId(1),
            transfers: vec![(StorageId(2), StorageId(3))],
            leaf_transfers: vec![],
        };
        let error = verify_physical_module(&physical)
            .expect_err("a failure edge cannot observe the normal-only result");
        assert!(error.message.contains("reads uninitialized storage 2"));
    }

    #[test]
    fn verifier_rejects_a_changed_checked_failure_kind() {
        let verified =
            lower_physical_module(&module_with_checked_add(), target()).expect("checked add");
        let mut physical = verified.into_unverified();
        let PhysicalTerminator::CheckedBinary { failures, .. } =
            &mut physical.functions[0].blocks[0].terminator
        else {
            panic!("checked add fixture");
        };
        failures[0].kind = TrapKind::DivideByZero;
        let error = verify_physical_module(&physical)
            .expect_err("physical failure kinds must preserve SIR semantics");
        assert!(error.message.contains("failure set disagrees"));
    }

    #[test]
    fn scalar_loop_reinitializes_dynamic_ssa_storage() {
        let module = lower_source(
            r"
            fn main() -> i64 {
                var value = 0;
                while value < 3 {
                    value = value &+ 1;
                }
                value
            }
            ",
        );
        lower_physical_module(&module, target_for_inventory(&module))
            .expect("scalar loop should verify physically");
    }

    #[test]
    fn owned_loop_discharges_each_dynamic_owner_before_reinitialization() {
        let module = lower_source(
            r#"
            fn main() {
                var selected = "start";
                var keep = true;
                while keep {
                    selected = "loop";
                    keep = false;
                }
            }
            "#,
        );
        lower_physical_module(&module, target_for_inventory(&module))
            .expect("owned loop should verify physically");
    }

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "the malformed branch fixture must show both owner states and their merge"
    )]
    fn verifier_rejects_overwriting_a_maybe_live_owner() {
        let mut physical_target = target();
        physical_target.insert_layout(
            ResolvedTy::Unit,
            PhysicalLayout {
                size: 0,
                align: 1,
                repr: PhysicalRepr::Unit,
            },
        );
        let callable = PhysicalCallable {
            id: CallableId(0),
            declaration: hew_types::DefId::for_test("malformed_owner_merge"),
            instance: CallableInstance::Monomorphic,
            symbol: "malformed_owner_merge".to_string(),
            is_resumable: false,
            params: vec![],
            return_ty: ResolvedTy::Unit,
            return_layout: None,
        };
        let function = PhysicalFunction {
            callable: CallableId(0),
            entry: BlockId(0),
            parameters: vec![],
            place_storage: BTreeMap::new(),
            storage: vec![
                PhysicalStorage {
                    id: StorageId(0),
                    ty: ResolvedTy::String,
                    layout: physical_target
                        .layout(&ResolvedTy::String)
                        .expect("string layout")
                        .clone(),
                    own: OwnKind::Owned,
                    origin: StorageOrigin::Value(ValueId(0)),
                    borrow_parent: None,
                },
                PhysicalStorage {
                    id: StorageId(1),
                    ty: ResolvedTy::Bool,
                    layout: physical_target
                        .layout(&ResolvedTy::Bool)
                        .expect("bool layout")
                        .clone(),
                    own: OwnKind::None,
                    origin: StorageOrigin::Value(ValueId(1)),
                    borrow_parent: None,
                },
            ],
            blocks: vec![
                PhysicalBlock {
                    id: BlockId(0),
                    arguments: vec![],
                    ops: vec![PhysicalOp::Const {
                        dest: StorageId(1),
                        value: PhysicalConst::Bool(true),
                    }],
                    terminator: PhysicalTerminator::Branch {
                        condition: StorageId(1),
                        then_target: PhysicalEdge {
                            target: BlockId(1),
                            transfers: vec![],
                            leaf_transfers: vec![],
                        },
                        else_target: PhysicalEdge {
                            target: BlockId(2),
                            transfers: vec![],
                            leaf_transfers: vec![],
                        },
                    },
                },
                PhysicalBlock {
                    id: BlockId(1),
                    arguments: vec![],
                    ops: vec![PhysicalOp::Const {
                        dest: StorageId(0),
                        value: PhysicalConst::String(hew_sir::StringLiteralId(0)),
                    }],
                    terminator: PhysicalTerminator::Goto(PhysicalEdge {
                        target: BlockId(3),
                        transfers: vec![],
                        leaf_transfers: vec![],
                    }),
                },
                PhysicalBlock {
                    id: BlockId(2),
                    arguments: vec![],
                    ops: vec![],
                    terminator: PhysicalTerminator::Goto(PhysicalEdge {
                        target: BlockId(3),
                        transfers: vec![],
                        leaf_transfers: vec![],
                    }),
                },
                PhysicalBlock {
                    id: BlockId(3),
                    arguments: vec![],
                    ops: vec![PhysicalOp::Const {
                        dest: StorageId(0),
                        value: PhysicalConst::String(hew_sir::StringLiteralId(1)),
                    }],
                    terminator: PhysicalTerminator::Return { value: None },
                },
            ],
        };
        let physical = PhysicalModule {
            actors: Vec::new(),
            supervisors: Vec::new(),
            actor_recipes: BTreeMap::new(),
            resources: vec![],
            closures: vec![],
            vtables: vec![],
            environment_glue: vec![],
            value_capabilities: BTreeMap::new(),
            target: physical_target,
            aggregate_glue: vec![],
            variant_glue: vec![],
            vector_glue: vec![],
            map_glue: vec![],
            set_glue: vec![],
            type_facts: BTreeMap::new(),
            callables: vec![callable],
            functions: vec![function],
            entry_callable: None,
            entry_exit_plan: None,
            string_literals: BTreeMap::from([
                (hew_sir::StringLiteralId(0), "left".to_string()),
                (hew_sir::StringLiteralId(1), "right".to_string()),
            ]),
            bytes_literals: BTreeMap::new(),
        };
        let error = verify_physical_module(&physical)
            .expect_err("a path-dependent live owner cannot be overwritten");
        assert!(error.message.contains("may overwrite a live obligation"));
    }

    fn collection_parameter_fixture() -> PhysicalModule {
        let collection = |kind: BuiltinType, args| ResolvedTy::Named {
            name: kind.canonical_name().to_string(),
            builtin: Some(kind),
            args,
            is_opaque: false,
        };
        let vector = collection(BuiltinType::Vec, vec![ResolvedTy::String]);
        let set = collection(BuiltinType::HashSet, vec![ResolvedTy::String]);
        let map = collection(BuiltinType::HashMap, vec![ResolvedTy::String, vector]);
        let nested = collection(BuiltinType::HashMap, vec![ResolvedTy::I64, set]);
        let mut module = module_with_return();
        module.functions.clear();
        module.entry_callable = None;
        let mut facts = hew_types::TypeFactService::new(
            hew_types::TypeFactContext::default(),
            module.type_facts,
        );
        module.callables[0].signature.params = [map, nested]
            .into_iter()
            .map(|ty| {
                facts
                    .require(&ty)
                    .expect("canonical collection value facts");
                hew_sir::SemAbiParam {
                    ty,
                    passing: hew_sir::SemParamPassing::Borrow,
                    caller_visible_projection: false,
                }
            })
            .collect();
        module.type_facts = facts.into_rows();
        lower_physical_module(&module, target_for_inventory(&module))
            .expect("collection parameters have complete physical value recipes")
            .into_unverified()
    }

    #[test]
    fn collection_callback_cleanup_requires_its_existing_fault_owner() {
        let semantic = lower_source(
            r#"fn main() -> i64 {
                var values: HashMap<i64, string> = HashMap.new();
                values.insert(1, "one");
                if values.contains_key(1) { 0 } else { 1 }
            }"#,
        );
        let physical = lower_physical_module(&semantic, target_for_inventory(&semantic))
            .unwrap()
            .into_unverified();
        let function = physical
            .functions
            .iter()
            .position(|function| {
                function.blocks.iter().any(|block| {
                    matches!(
                        block.terminator,
                        PhysicalTerminator::RuntimeCall {
                            action: PhysicalRuntimeAction::Map {
                                operation: PhysicalMapOp::ContainsKey,
                                ..
                            },
                            ..
                        }
                    )
                })
            })
            .unwrap();
        let cleanup = physical.functions[function]
            .blocks
            .iter()
            .find_map(|block| match &block.terminator {
                PhysicalTerminator::RuntimeCall {
                    action:
                        PhysicalRuntimeAction::Map {
                            operation: PhysicalMapOp::ContainsKey,
                            ..
                        },
                    failure: Some(edge),
                    ..
                } => Some(edge.target),
                _ => None,
            })
            .unwrap();
        let mut invalid = physical.clone();
        invalid.functions[function].blocks[cleanup.0 as usize].terminator =
            PhysicalTerminator::Trap(TrapKind::IndexOutOfBounds);
        let error = verify_physical_module(&invalid).unwrap_err();
        assert!(
            error
                .message
                .contains("creates a trap while an earlier fault is active"),
            "{error}"
        );

        let mut invalid = physical;
        let success = invalid.functions[function]
            .blocks
            .iter_mut()
            .find(|block| matches!(block.terminator, PhysicalTerminator::Return { .. }))
            .unwrap();
        success.terminator = PhysicalTerminator::PropagateFault;
        let error = verify_physical_module(&invalid).unwrap_err();
        assert!(
            error
                .message
                .contains("propagates a fault that is not initialized"),
            "{error}"
        );
    }

    #[test]
    fn selected_key_methods_keep_checked_physical_recipes_and_callable_bodies() {
        let semantic = lower_source(
            r#"
            type Key { id: i64 }
            impl Hash for Key { fn hash(self) -> i64 { self.id % 10 } }
            type Outer { key: Key }
            fn main() -> i64 {
                var values: HashMap<Outer, string> = HashMap.new();
                values.insert(Outer { key: Key { id: 7 } }, "kept");
                values.len()
            }
        "#,
        );
        let target = target_for_inventory(&semantic);
        let physical = lower_physical_module(&semantic, target)
            .unwrap()
            .into_unverified();
        let (user_key, user) = physical
            .value_capabilities
            .iter()
            .find(|(_, plan)| matches!(plan.method, PhysicalValueMethod::User(_)))
            .expect("selected user hash");
        let PhysicalValueMethod::User(callable) = user.method else {
            unreachable!()
        };
        let mut missing_body = physical.clone();
        missing_body
            .functions
            .retain(|function| function.callable != callable);
        assert!(verify_physical_module(&missing_body)
            .unwrap_err()
            .message
            .contains("signature or body"));

        let mut substituted = physical.clone();
        substituted
            .value_capabilities
            .get_mut(user_key)
            .unwrap()
            .method = PhysicalValueMethod::Scalar;
        assert!(verify_physical_module(&substituted)
            .unwrap_err()
            .message
            .contains("changed its selected callable"));

        let mut forged_identity = physical.clone();
        forged_identity.callables[callable.0 as usize].declaration =
            hew_types::DefId::for_test("unselected_compatible_hash");
        assert!(verify_physical_module(&forged_identity)
            .unwrap_err()
            .message
            .contains("checker selection"));

        let mut transplanted = physical.clone();
        let derived = physical
            .value_capabilities
            .get(&(user_key.0.clone(), hew_types::ValueCapability::Eq))
            .unwrap();
        transplanted
            .value_capabilities
            .get_mut(user_key)
            .unwrap()
            .selection = derived.selection.clone();
        assert!(verify_physical_module(&transplanted)
            .unwrap_err()
            .message
            .contains("another type or capability"));

        let mut missing_component = physical.clone();
        missing_component.value_capabilities.remove(user_key);
        assert!(verify_physical_module(&missing_component)
            .unwrap_err()
            .message
            .contains("selected component"));

        let mut absent_keys = physical;
        absent_keys.value_capabilities.clear();
        assert!(verify_physical_module(&absent_keys)
            .unwrap_err()
            .message
            .contains("selected key capability"));
    }

    #[test]
    fn map_and_set_owners_compose_the_shared_value_recipes() {
        let module = collection_parameter_fixture();
        for map in &module.map_glue {
            verify_clone_action(&module, &map.ty, OwnKind::Owned, CloneAction::Map(map.id))
                .unwrap();
            verify_destroy_action(&module, &map.ty, OwnKind::Owned, DestroyAction::Map(map.id))
                .unwrap();
            match collection_type_arguments(&map.value.ty).unwrap().0 {
                BuiltinType::Vec => {
                    assert!(matches!(map.value.clone, Some(CloneAction::Vector(_))));
                }
                BuiltinType::HashSet => {
                    assert!(matches!(map.value.clone, Some(CloneAction::Set(_))));
                }
                other => panic!("unexpected nested collection {other:?}"),
            }
        }
        let set = &module.set_glue[0];
        assert_eq!(set.element.clone, Some(CloneAction::StringRetain));
        assert_eq!(set.element.destroy, Some(DestroyAction::StringRelease));
    }

    #[test]
    fn collection_glue_rejects_wrong_identity_layout_and_lifetime_recipes() {
        let original = collection_parameter_fixture();
        for mutation in 0..5 {
            let mut module = original.clone();
            match mutation {
                0 => module.map_glue[0].key.ty = ResolvedTy::Bool,
                1 => module.set_glue[0].element.destroy = None,
                2 => module.map_glue[0].id = PhysicalMapId(99),
                3 => module.map_glue.push(module.map_glue[0].clone()),
                4 => {
                    let ty = module.map_glue[0].ty.clone();
                    module.target.insert_layout(ty, i64_layout());
                }
                _ => unreachable!(),
            }
            verify_physical_module(&module).expect_err("forged collection glue must be refused");
        }
        let map = &original.map_glue[0];
        verify_clone_action(
            &original,
            &map.ty,
            OwnKind::Owned,
            CloneAction::Map(original.map_glue[1].id),
        )
        .expect_err("foreign map key/value recipe");
        verify_destroy_action(
            &original,
            &map.ty,
            OwnKind::Owned,
            DestroyAction::Set(original.set_glue[0].id),
        )
        .expect_err("set drop cannot consume a map");
    }

    #[test]
    fn verifier_rejects_selected_close_with_a_non_integer_index() {
        let mut module = vector_fixture();
        module.callables[0].is_resumable = true;
        let descriptor = module.vector_glue[0].id;
        let block = vector_block(&mut module.functions[0], VecValueOp::Set);
        let PhysicalTerminator::RuntimeCall { args, normal, .. } = &block.terminator else {
            unreachable!()
        };
        let ArgumentTransfer::Move(owner) = args[0] else {
            panic!("set must own its receiver");
        };
        block.terminator = PhysicalTerminator::ValueClose {
            index: Some(owner),
            generator: owner,
            destroy: Some(DestroyAction::Vector(descriptor)),
            conditional: false,
            next: normal.clone(),
        };
        let error = verify_physical_module(&module).expect_err("vector pointer cannot be an index");
        assert!(error.message.contains("copied i64 index"), "{error}");
    }

    fn vector_fixture() -> PhysicalModule {
        let module = lower_source(
            r#"
            fn main() -> i64 {
                var values = ["one", "two"];
                var numbers = [7, 8];
                let snapshot = values;
                let independent = values[0];
                let optional = values.get(99);
                let numeric_option = numbers.get(0);
                values.set(0, "replacement");
                let removed = values.pop();
                let number = numbers.pop();
                values.clear();
                return snapshot.len();
            }
            "#,
        );
        lower_physical_module(&module, target_for_inventory(&module))
            .expect("vector source must have a complete physical realization")
            .into_unverified()
    }

    fn vector_block(function: &mut PhysicalFunction, op: VecValueOp) -> &mut PhysicalBlock {
        function.blocks.iter_mut().find(|block| matches!(block.terminator,
            PhysicalTerminator::RuntimeCall { action: PhysicalRuntimeAction::Vector { operation, .. }, .. }
                if operation.semantic_op() == op)).expect("fixture vector operation")
    }

    #[test]
    fn vector_values_use_shared_copy_drop_recipes_for_every_element_shape() {
        for (declarations, element, value) in [
            ("", "i64", "7"),
            ("", "string", "\"payload\""),
            (
                "type Leaf { label: string, }",
                "Leaf",
                "Leaf { label: \"payload\" }",
            ),
            (
                "enum Choice { Text(string), Empty, }",
                "Choice",
                "Choice.Text(\"payload\")",
            ),
            ("", "Vec<string>", "[\"payload\"]"),
            ("", "(Vec<string>, i64)", "([\"payload\"], 1)"),
        ] {
            let source = format!(
                r"
                {declarations}
                fn duplicate<T>(values: Vec<T>) -> Vec<T> {{ values }}
                fn main() -> i64 {{
                    var values: Vec<{element}> = Vec.new();
                    values.push({value});
                    let snapshot = duplicate(values);
                    let independent = values[0];
                    let optional = values.get(9);
                    values.set(0, {value});
                    let removed = values.pop();
                    values.clear();
                    return snapshot.len();
                }}"
            );
            let module = lower_source(&source);
            let physical = lower_physical_module(&module, target_for_inventory(&module))
                .unwrap_or_else(|error| panic!("{element}: {error}"));
            let physical = physical.module();
            let operations = physical
                .functions
                .iter()
                .flat_map(|function| &function.blocks)
                .filter_map(|block| match block.terminator {
                    PhysicalTerminator::RuntimeCall {
                        action: PhysicalRuntimeAction::Vector { operation, .. },
                        ..
                    } => Some(operation.semantic_op()),
                    _ => None,
                })
                .collect::<Vec<_>>();
            for operation in [
                VecValueOp::New,
                VecValueOp::Len,
                VecValueOp::Index,
                VecValueOp::Get,
                VecValueOp::Push,
                VecValueOp::Set,
                VecValueOp::Pop,
                VecValueOp::Clear,
            ] {
                assert!(
                    operations.contains(&operation),
                    "{element}: missing {operation:?}"
                );
            }
            assert!(
                physical
                    .functions
                    .iter()
                    .flat_map(|function| &function.blocks)
                    .flat_map(|block| &block.ops)
                    .any(|op| matches!(
                        op,
                        PhysicalOp::Clone {
                            action: CloneAction::Vector(_),
                            ..
                        }
                    )),
                "{element}: ordinary vector value must use the shared vector clone"
            );
            for glue in &physical.vector_glue {
                assert_eq!(sequence_element_type(&glue.ty), Some(&glue.element.ty));
                assert_eq!(
                    glue.element.own,
                    OwnKind::of_ty(&glue.element.ty, &module.type_facts).unwrap()
                );
                assert!(glue.element.clone.is_some());
                assert_eq!(
                    glue.element.destroy.is_some(),
                    glue.element.own == OwnKind::Owned
                );
            }
        }
    }

    #[test]
    fn vector_inventory_demands_recursive_element_shapes_without_payload_construction() {
        let module = lower_source(
            r"
            type Leaf { text: string, }
            enum Tree { Value(Leaf), Children(Vec<Tree>), }
            fn main() -> i64 {
                let trees: Vec<Tree> = Vec.new();
                let snapshot = trees;
                return snapshot.len();
            }
        ",
        );
        let inventory = physical_type_inventory(&module);
        assert!(inventory
            .aggregates()
            .any(|shape| shape.fields == [ResolvedTy::String]));
        assert_eq!(inventory.vectors().count(), 1);
        let physical = lower_physical_module(&module, target_for_inventory(&module)).unwrap();
        let physical = physical.module();
        let vector = &physical.vector_glue[0];
        let Some(CloneAction::Variant(tree)) = vector.element.clone else {
            panic!("tree copy recipe");
        };
        let tree = variant_glue(physical, tree).unwrap();
        assert_eq!(
            tree.variants[1].fields[0].clone,
            Some(CloneAction::Vector(vector.id))
        );
        assert_eq!(
            tree.variants[1].fields[0].destroy,
            Some(DestroyAction::Vector(vector.id))
        );
    }

    #[test]
    fn vector_zero_sized_elements_preserve_the_exact_target_layout() {
        let module = lower_source(
            r"
            type Empty {}
            fn main() -> i64 {
                var values: Vec<Empty> = Vec.new();
                values.push(Empty {});
                let snapshot = values;
                let extracted = values[0];
                let optional = values.get(0);
                let removed = values.pop();
                values.clear();
                return snapshot.len();
            }
        ",
        );
        let physical = lower_physical_module(&module, target_for_inventory(&module)).unwrap();
        let physical = physical.module();
        let element = &physical.vector_glue[0].element;
        assert_eq!(physical.target.layout(&element.ty).unwrap().size, 0);
        assert_eq!(element.clone, Some(CloneAction::Bitwise));
        assert_eq!(element.destroy, None);
        assert_eq!(element.own, OwnKind::None);
    }

    #[test]
    fn verifier_rejects_vector_descriptor_identity_and_recipe_drift() {
        let original = vector_fixture();
        let string_id = original
            .vector_glue
            .iter()
            .position(|glue| glue.element.ty == ResolvedTy::String)
            .unwrap();
        for mutation in 0..4 {
            let mut physical = original.clone();
            let glue = &mut physical.vector_glue[string_id];
            match mutation {
                0 => glue.element.ty = ResolvedTy::I64,
                1 => glue.element.clone = None,
                2 => glue.element.destroy = None,
                3 => {
                    glue.element.own = OwnKind::None;
                    glue.element.clone = Some(CloneAction::Bitwise);
                    glue.element.destroy = None;
                }
                _ => unreachable!(),
            }
            let error = verify_physical_module(&physical).expect_err("forged vector recipe");
            let expected = match mutation {
                0 => "exact element identity",
                1 => "physical clone action Vector",
                2 => "physical destroy action Vector",
                3 => "semantic ownership or cloneability",
                _ => unreachable!(),
            };
            assert!(error.message.contains(expected), "{mutation}: {error}");
        }
        let mut physical = original;
        physical.vector_glue[string_id].id = PhysicalVectorId(999);
        let error = verify_physical_module(&physical).unwrap_err();
        assert!(
            error.message.contains("unknown physical vector glue"),
            "{error}"
        );
    }

    #[test]
    fn verifier_rejects_foreign_vector_and_extraction_descriptors() {
        let original = vector_fixture();
        for operation in [VecValueOp::Len, VecValueOp::Get, VecValueOp::Pop] {
            let mut physical = original.clone();
            let foreign_vector = physical
                .vector_glue
                .iter()
                .find(|glue| glue.element.ty == ResolvedTy::I64)
                .unwrap()
                .id;
            let foreign_variant = physical
                .variant_glue
                .iter()
                .find(|glue| {
                    matches!(&glue.ty,
                ResolvedTy::Named { builtin: Some(hew_types::BuiltinType::Option), args, .. }
                    if args == &[ResolvedTy::I64])
                })
                .unwrap()
                .id;
            let foreign_tuple = physical
                .aggregate_glue
                .iter()
                .find(|glue| {
                    matches!(&glue.ty,
                ResolvedTy::Tuple(fields) if fields.last() == Some(&ResolvedTy::I64))
                })
                .unwrap()
                .id;
            let block = vector_block(&mut physical.functions[0], operation);
            let PhysicalTerminator::RuntimeCall {
                action:
                    PhysicalRuntimeAction::Vector {
                        operation: action,
                        glue,
                    },
                ..
            } = &mut block.terminator
            else {
                unreachable!()
            };
            match operation {
                VecValueOp::Len => *glue = foreign_vector,
                VecValueOp::Get => {
                    *action = PhysicalVectorOp::Get {
                        result: foreign_variant,
                    }
                }
                VecValueOp::Pop => {
                    *action = PhysicalVectorOp::Pop {
                        result: foreign_tuple,
                    }
                }
                _ => unreachable!(),
            }
            let error = verify_physical_module(&physical).expect_err("foreign physical descriptor");
            assert!(
                error.message.contains("foreign vector descriptor")
                    || error.message.contains("result descriptor"),
                "{operation:?}: {error}"
            );
        }
    }

    #[test]
    fn verifier_checks_vector_element_types_and_result_ownership() {
        let original = vector_fixture();
        let mut physical = original.clone();
        let integer = physical.functions[0]
            .storage
            .iter()
            .find(|slot| slot.ty == ResolvedTy::I64)
            .unwrap()
            .id;
        let block = vector_block(&mut physical.functions[0], VecValueOp::Set);
        let PhysicalTerminator::RuntimeCall { args, .. } = &mut block.terminator else {
            unreachable!()
        };
        args[2] = ArgumentTransfer::Borrow(integer);
        let error = verify_physical_module(&physical).expect_err("right arity, wrong element type");
        assert!(error.message.contains("runtime argument 2"), "{error}");

        for operation in [VecValueOp::Index, VecValueOp::Get, VecValueOp::Pop] {
            let mut physical = original.clone();
            let block = vector_block(&mut physical.functions[0], operation);
            let PhysicalTerminator::RuntimeCall {
                result: Some(result),
                ref args,
                ..
            } = block.terminator
            else {
                unreachable!()
            };
            let parent = match args[0] {
                ArgumentTransfer::Borrow(source)
                | ArgumentTransfer::BorrowMut(source)
                | ArgumentTransfer::Move(source)
                | ArgumentTransfer::Clone { source, .. } => source,
            };
            let slot = &mut physical.functions[0].storage[result.0 as usize];
            slot.own = OwnKind::Guaranteed;
            // Even a forged loan dependency cannot change an owning extraction
            // into a borrowed result of this runtime operation.
            slot.borrow_parent = Some(parent);
            let error = verify_physical_module(&physical)
                .expect_err("extraction cannot yield an interior borrow");
            assert!(
                error.message.contains("result ownership"),
                "{operation:?}: {error}"
            );
        }
    }

    #[test]
    fn verifier_checks_vector_failure_presence_and_consuming_receiver_contracts() {
        let original = vector_fixture();
        for operation in [VecValueOp::Index, VecValueOp::Set, VecValueOp::Pop] {
            let mut physical = original.clone();
            let block = vector_block(&mut physical.functions[0], operation);
            let PhysicalTerminator::RuntimeCall { failure, .. } = &mut block.terminator else {
                unreachable!()
            };
            *failure = None;
            let error = verify_physical_module(&physical).expect_err("missing vector failure edge");
            assert!(
                error.message.contains("failure edge"),
                "{operation:?}: {error}"
            );
        }
        for operation in [
            VecValueOp::Push,
            VecValueOp::Set,
            VecValueOp::Pop,
            VecValueOp::Clear,
        ] {
            let mut physical = original.clone();
            let block = vector_block(&mut physical.functions[0], operation);
            let PhysicalTerminator::RuntimeCall { args, .. } = &mut block.terminator else {
                unreachable!()
            };
            let ArgumentTransfer::Move(receiver) = args[0] else {
                panic!("receiver must transfer");
            };
            args[0] = ArgumentTransfer::Borrow(receiver);
            let error =
                verify_physical_module(&physical).expect_err("mutation must transfer its receiver");
            assert!(
                error.message.contains("argument disagrees"),
                "{operation:?}: {error}"
            );
        }
    }

    #[test]
    fn vector_transfers_on_success_and_retains_inputs_on_failure() {
        let original = vector_fixture();
        for operation in [VecValueOp::Set, VecValueOp::Pop] {
            for failed in [false, true] {
                let mut physical = original.clone();
                let block = vector_block(&mut physical.functions[0], operation);
                let PhysicalTerminator::RuntimeCall {
                    action: PhysicalRuntimeAction::Vector { glue, .. },
                    args,
                    normal,
                    failure,
                    ..
                } = &block.terminator
                else {
                    unreachable!()
                };
                let ArgumentTransfer::Move(receiver) = args[0] else {
                    unreachable!()
                };
                let action = CloneAction::Vector(*glue);
                let target = if failed {
                    failure.as_ref().unwrap().target
                } else {
                    normal.target
                };
                physical.functions[0]
                    .blocks
                    .iter_mut()
                    .find(|block| block.id == target)
                    .unwrap()
                    .ops
                    .insert(
                        0,
                        PhysicalOp::Clone {
                            dest: receiver,
                            source: receiver,
                            action,
                        },
                    );
                let error =
                    verify_physical_module(&physical).expect_err("transferred receiver was reused");
                assert!(
                    error.message.contains(if failed {
                        "overwrites initialized"
                    } else {
                        "uninitialized"
                    }),
                    "{operation:?} failed={failed}: {error}"
                );
            }
        }
    }

    #[test]
    fn vector_index_failure_never_initializes_its_result_storage() {
        let mut physical = vector_fixture();
        let block = vector_block(&mut physical.functions[0], VecValueOp::Index);
        let PhysicalTerminator::RuntimeCall {
            result: Some(result),
            failure: Some(failure),
            ..
        } = &block.terminator
        else {
            unreachable!()
        };
        let result = *result;
        let target = failure.target;
        physical.functions[0]
            .blocks
            .iter_mut()
            .find(|block| block.id == target)
            .unwrap()
            .ops
            .insert(
                0,
                PhysicalOp::Clone {
                    dest: result,
                    source: result,
                    action: CloneAction::StringRetain,
                },
            );
        let error =
            verify_physical_module(&physical).expect_err("failed read did not produce an element");
        assert!(error.message.contains("uninitialized"), "{error}");
    }

    #[test]
    fn vector_copies_and_mutations_preserve_loop_and_early_return_storage() {
        let module = lower_source(
            r#"
            fn grow(values: Vec<string>, stop: bool) -> i64 {
                var current = values;
                for i in 0..3 {
                    let snapshot = current;
                    current.push("loop");
                    if stop && i == 1 { return snapshot.len(); }
                }
                return current.len();
            }
            fn main() -> i64 {
                let input = ["seed"];
                grow(input, true) + grow(input, false)
            }
        "#,
        );
        let physical = lower_physical_module(&module, target_for_inventory(&module))
            .expect("each dynamic vector owner must discharge before its storage is reused");
        assert_eq!(physical.module().functions.len(), 2);
    }

    #[test]
    fn failed_vector_index_clears_a_previous_iteration_scalar_result() {
        let module = lower_source("fn main() -> i64 { let values = [1]; values[0] }");
        let mut physical = lower_physical_module(&module, target_for_inventory(&module))
            .unwrap()
            .into_unverified();
        let function = &mut physical.functions[0];
        let block = vector_block(function, VecValueOp::Index).clone();
        let PhysicalTerminator::RuntimeCall {
            result: Some(result),
            failure: Some(failure),
            normal,
            ..
        } = &block.terminator
        else {
            unreachable!()
        };
        assert_eq!(function.storage[result.0 as usize].own, OwnKind::None);
        // Scalar storage may still hold the previous dynamic iteration's bits.
        // A failed read initializes no result of the current invocation.
        let state = FlowState {
            slots: vec![InitState::Initialized; function.storage.len()],
            active: vec![InitState::Uninitialized; function.storage.len()],
            fault: FaultState::None,
            exit: defer::ORDINARY,
            defers: defer::State::default(),
        };
        let successors = terminator_successors(
            function,
            &block.terminator,
            state,
            block.id,
            &defer::verify_regions(function).unwrap(),
        )
        .unwrap();
        let failed = &successors
            .iter()
            .find(|(id, _)| *id == failure.target)
            .unwrap()
            .1;
        assert_eq!(failed.slots[result.0 as usize], InitState::Uninitialized);
        let succeeded = &successors
            .iter()
            .find(|(id, _)| *id == normal.target)
            .unwrap()
            .1;
        assert_eq!(succeeded.slots[result.0 as usize], InitState::Initialized);
    }

    fn selected_value_call_module(capability: ValueCapability) -> PhysicalModule {
        let (method, invocation) = match capability {
            ValueCapability::Hash => ("fn selected(a: i64) -> i64 { a }", "selected(7)"),
            ValueCapability::Eq => (
                "fn selected(a: i64, b: i64) -> bool { a == b }",
                "if selected(1, 2) { 1 } else { 0 }",
            ),
        };
        let mut module = lower_source(&format!("{method} fn main() -> i64 {{ let map: HashMap<i64, string> = HashMap.new(); {invocation} }}"));
        let selected = module
            .functions
            .iter()
            .find(|function| function.declaration.full_path() == "selected")
            .unwrap()
            .callable;
        let mut converted = 0;
        for function in &mut module.functions {
            for block in &mut function.blocks {
                if let SemTerminator::Call {
                    id,
                    callee,
                    args,
                    result,
                    normal,
                    unwind,
                } = &block.terminator
                {
                    if *callee == selected {
                        let mut args = args.clone();
                        for argument in &mut args {
                            argument.decision = BoundaryDecision::Borrow;
                        }
                        block.terminator = SemTerminator::ValueCall {
                            id: *id,
                            ty: ResolvedTy::I64,
                            capability,
                            args,
                            result: result.clone(),
                            normal: normal.clone().expect("returning call"),
                            unwind: unwind.clone(),
                        };
                        converted += 1;
                    }
                }
            }
        }
        assert_eq!(converted, 1);
        assert!(
            hew_sir::verify_module(&module).is_empty(),
            "{:?}",
            hew_sir::verify_module(&module)
        );
        lower_physical_module(&module, target_for_inventory(&module))
            .unwrap()
            .module()
            .clone()
    }

    #[test]
    fn selected_value_calls_keep_borrowed_operands_and_success_only_results() {
        for capability in [ValueCapability::Hash, ValueCapability::Eq] {
            let module = selected_value_call_module(capability);
            let (function, block) = module
                .functions
                .iter()
                .find_map(|function| {
                    function
                        .blocks
                        .iter()
                        .find(|block| {
                            matches!(block.terminator, PhysicalTerminator::ValueCall { .. })
                        })
                        .map(|block| (function, block))
                })
                .unwrap();
            let PhysicalTerminator::ValueCall {
                args,
                result,
                normal,
                unwind,
                ..
            } = &block.terminator
            else {
                unreachable!()
            };
            let mut state = FlowState {
                slots: vec![InitState::Initialized; function.storage.len()],
                active: vec![InitState::Uninitialized; function.storage.len()],
                fault: FaultState::None,
                exit: defer::ORDINARY,
                defers: defer::State::default(),
            };
            state.slots[result.0 as usize] = InitState::Uninitialized;
            let successors = terminator_successors(
                function,
                &block.terminator,
                state,
                block.id,
                &defer::verify_regions(function).unwrap(),
            )
            .unwrap();
            for (edge, outcome) in successors {
                assert_eq!(
                    outcome.slots[result.0 as usize],
                    if edge == normal.target {
                        InitState::Initialized
                    } else {
                        InitState::Uninitialized
                    }
                );
                assert_eq!(
                    outcome.fault,
                    if edge == unwind.target {
                        FaultState::Active
                    } else {
                        FaultState::None
                    }
                );
                for arg in args {
                    let ArgumentTransfer::Borrow(source) = arg else {
                        panic!("selected calls must borrow")
                    };
                    assert_eq!(outcome.slots[source.0 as usize], InitState::Initialized);
                }
            }
        }
    }

    #[test]
    fn selected_value_call_verifier_rejects_signature_and_selection_drift() {
        for mutation in 0..5 {
            let mut module = selected_value_call_module(ValueCapability::Eq);
            let function_index = module
                .functions
                .iter()
                .position(|function| {
                    function.blocks.iter().any(|block| {
                        matches!(block.terminator, PhysicalTerminator::ValueCall { .. })
                    })
                })
                .unwrap();
            let function = &mut module.functions[function_index];
            let block = function
                .blocks
                .iter_mut()
                .find(|block| matches!(block.terminator, PhysicalTerminator::ValueCall { .. }))
                .unwrap();
            let PhysicalTerminator::ValueCall {
                ty, args, result, ..
            } = &mut block.terminator
            else {
                unreachable!()
            };
            match mutation {
                0 => {
                    module
                        .value_capabilities
                        .remove(&(ResolvedTy::I64, ValueCapability::Eq));
                }
                1 => {
                    args.pop();
                }
                2 => {
                    let ArgumentTransfer::Borrow(source) = args[0] else {
                        unreachable!()
                    };
                    args[0] = ArgumentTransfer::Move(source);
                }
                3 => *ty = ResolvedTy::Bool,
                4 => {
                    let ArgumentTransfer::Borrow(source) = args[0] else {
                        unreachable!()
                    };
                    *result = source;
                }
                _ => unreachable!(),
            }
            let function = &module.functions[function_index];
            let block = function
                .blocks
                .iter()
                .find(|block| matches!(block.terminator, PhysicalTerminator::ValueCall { .. }))
                .unwrap();
            let blocks = function.blocks.iter().map(|block| block.id).collect();
            assert!(
                verify_terminator(&module, function, &blocks, &block.terminator).is_err(),
                "mutation {mutation}"
            );
            assert!(
                verify_physical_module(&module).is_err(),
                "mutation {mutation}"
            );
        }
    }

    #[test]
    fn selected_value_call_cleanup_cannot_discard_fault_or_read_failed_result() {
        for discard_fault in [false, true] {
            let mut module = selected_value_call_module(ValueCapability::Eq);
            let function = module
                .functions
                .iter_mut()
                .find(|function| {
                    function.blocks.iter().any(|block| {
                        matches!(block.terminator, PhysicalTerminator::ValueCall { .. })
                    })
                })
                .unwrap();
            let (result, cleanup) = function
                .blocks
                .iter()
                .find_map(|block| match &block.terminator {
                    PhysicalTerminator::ValueCall { result, unwind, .. } => {
                        Some((*result, unwind.target))
                    }
                    _ => None,
                })
                .unwrap();
            let cleanup = function
                .blocks
                .iter_mut()
                .find(|block| block.id == cleanup)
                .unwrap();
            if discard_fault {
                cleanup.terminator = PhysicalTerminator::Unreachable;
            } else {
                cleanup.terminator = PhysicalTerminator::Branch {
                    condition: result,
                    then_target: PhysicalEdge {
                        target: cleanup.id,
                        transfers: vec![],
                        leaf_transfers: vec![],
                    },
                    else_target: PhysicalEdge {
                        target: cleanup.id,
                        transfers: vec![],
                        leaf_transfers: vec![],
                    },
                };
            }
            assert!(verify_physical_module(&module).is_err());
        }
    }
}
