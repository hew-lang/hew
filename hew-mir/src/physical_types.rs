//! Physical MIR type definitions: storage, layouts, glue tables and the module shape.

use super::{
    ActorId, BTreeMap, BTreeSet, BinaryOp, BlockId, CallableId, ClosureId, DeferId, DeferScopeId,
    EncodingFormat, EntryExitPlan, FaultParkId, OwnKind, PhysicalCleanup, PhysicalExternResultAbi,
    PhysicalPlaceStorage, PhysicalStructuralGlue, PhysicalStructuralId, PhysicalValueCapability,
    ReleaseEffects, ResolvedTy, RuntimeCallFamily, TrapKind, TypeInstanceKey, UnaryOp,
    ValueCapability, ValueId,
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

/// Module-local identity of a shared handle's payload copy/drop recipe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalSharedId(pub u32);

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

/// An `Rc<T>` or `Weak<T>` handle and the exact payload the allocation holds.
/// Both spellings of one payload share the allocation, so both carry the same
/// payload recipe: `Rc.new` installs it as the allocation's destructor and
/// `Rc.get`/`Rc.set` read and replace the value it describes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalSharedDescriptor {
    pub ty: ResolvedTy,
    pub payload: ResolvedTy,
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
    pub(crate) types: BTreeSet<ResolvedTy>,
    pub(crate) resources: BTreeMap<ResolvedTy, PhysicalResourceDescriptor>,
    pub(crate) aggregates: BTreeMap<ResolvedTy, PhysicalAggregateDescriptor>,
    pub(crate) variants: BTreeMap<ResolvedTy, PhysicalVariantDescriptor>,
    pub(crate) vectors: BTreeMap<ResolvedTy, PhysicalVectorDescriptor>,
    pub(crate) maps: BTreeMap<ResolvedTy, PhysicalMapDescriptor>,
    pub(crate) sets: BTreeMap<ResolvedTy, PhysicalSetDescriptor>,
    pub(crate) shared: BTreeMap<ResolvedTy, PhysicalSharedDescriptor>,
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

    pub fn shared(&self) -> impl Iterator<Item = &PhysicalSharedDescriptor> {
        self.shared.values()
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
    Vector {
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
    /// A `var self` method: its fault exits write the receiver into the
    /// dual result's receiver field instead of releasing it.
    pub receiver_handback: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PhysicalConst {
    ActorIngressAdapter(hew_sir::ActorIngressAdapter),
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
    /// Retain one more strong reference to a shared allocation.
    RcRetain,
    /// Retain one more weak reference to a shared allocation.
    WeakRetain,
    Aggregate(PhysicalAggregateId),
    Variant(PhysicalVariantId),
    Vector(PhysicalVectorId),
    /// Fixed-array element storage with index-order cleanup.
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
    /// Release one strong reference. When it was the last, the runtime runs
    /// the payload's release recipe - which the glue identity names - before
    /// the allocation goes.
    RcRelease(PhysicalSharedId),
    /// Release one weak reference. A weak handle never owns the payload, so
    /// this releases nothing a program can observe.
    WeakRelease,
    Aggregate(PhysicalAggregateId),
    Variant(PhysicalVariantId),
    Vector(PhysicalVectorId),
    /// Fixed-array element storage with index-order cleanup.
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
    /// A borrowed pipe stream; the winning arm performs the receive.
    StreamNext(ArgumentTransfer),
    /// A borrowed ephemeral completion; the winning arm consumes its result.
    ActorCall(ArgumentTransfer),
}

impl PhysicalSelectSource {
    /// The borrowed handle this source observes.
    #[must_use]
    pub const fn transfer(self) -> ArgumentTransfer {
        match self {
            Self::Task(transfer) | Self::StreamNext(transfer) | Self::ActorCall(transfer) => {
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

/// Payload recipe shared by every operation over one shared allocation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalSharedGlue {
    pub id: PhysicalSharedId,
    pub ty: ResolvedTy,
    pub payload: PhysicalValueRecipe,
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
    /// The whole buffer moves out to the caller and the receiver is left a
    /// valid empty vector with its element representation intact.
    TakeAll {
        result: PhysicalAggregateId,
    },
    Slice,
    SliceFrom,
    /// Bulk add: every element of argument one joins the receiver.
    Append,
    /// Concatenate a vector of strings with a separator into a fresh string.
    Join,
}

impl PhysicalVectorOp {}

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

impl PhysicalMapOp {}

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

impl PhysicalSetOp {}

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
    GeneratorCoerce {
        dest: StorageId,
        source: StorageId,
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
        cleanup: PhysicalCleanup,
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

/// The physical carriers one runtime action's result needs, resolved against
/// this module's glue tables. Which shape an operation takes is its row's
/// `physical` form, so lowering never has to recognise the operation twice.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PhysicalRuntimeCarrier {
    /// Nothing beyond the call's own storage.
    None,
    /// One tagged-variant result built in place.
    Variant(PhysicalVariantId),
    /// A `(receiver, Option<element>)` pair: the pair and its optional half.
    PairWithOption {
        pair: PhysicalAggregateId,
        option: PhysicalVariantId,
    },
    /// `Result<string, Utf8Error>`, with the error aggregate and the variant
    /// carrying its length.
    Utf8Decode {
        result: PhysicalVariantId,
        error: PhysicalAggregateId,
        error_len: PhysicalVariantId,
    },
    /// A `Result` over two variant glues.
    NodeResult {
        result: PhysicalVariantId,
        error: PhysicalVariantId,
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
    /// The shared allocation's payload recipe and layout.
    SharedHandle(PhysicalSharedId),
    /// The borrowed operand's rendering recipe.
    StructuralFormat(PhysicalStructuralId),
}

/// One no-unwind runtime ABI operation: the verified SIR operation itself plus
/// the physical carriers its result needs. The emitter executes this; it never
/// selects ownership or failure behaviour from a linker symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhysicalRuntimeAction {
    pub family: RuntimeCallFamily,
    pub carrier: PhysicalRuntimeCarrier,
}

impl PhysicalRuntimeAction {
    /// A runtime action needing no physical carrier.
    #[must_use]
    pub const fn direct(family: RuntimeCallFamily) -> Self {
        Self {
            family,
            carrier: PhysicalRuntimeCarrier::None,
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
    /// Take the next element from the exclusively borrowed stream. `park`
    /// waits for an element or for the producer to finish; without it an empty
    /// stream produces `None` at once.
    StreamNext {
        park: bool,
        stream: ArgumentTransfer,
        element: PhysicalValueRecipe,
        result: StorageId,
        normal: PhysicalEdge,
        cancel: PhysicalEdge,
        unwind: PhysicalEdge,
    },
    /// Transfer one element into the borrowed sink. `park` waits for
    /// capacity; without it a full pipe resumes at once on `full`. The
    /// element is consumed on every exit; `closed` resumes after the consumer
    /// closed its half.
    StreamSend {
        park: bool,
        sink: ArgumentTransfer,
        value: ArgumentTransfer,
        element: PhysicalValueRecipe,
        normal: PhysicalEdge,
        closed: PhysicalEdge,
        full: Option<PhysicalEdge>,
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
    /// A `RemotePid` ask: borrowed pid, moved message and copied millisecond
    /// timeout. The message is encoded and released before the caller parks.
    RemoteAsk {
        actor: ActorId,
        message: u32,
        target: StorageId,
        payload: StorageId,
        timeout: StorageId,
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
        /// A failing `var self` callee writes its receiver into the dual
        /// result's receiver field; the unwind edge finds it here.
        handback: Option<StorageId>,
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
        /// Preserved declaration authority used by process entry setup.
        runtime_capability: Option<hew_types::ExternRuntimeCapability>,
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
    /// function's private `fault_out` and status result. A `var self` method
    /// first hands its receiver back through the dual result's receiver field.
    PropagateFault {
        handback: Option<StorageId>,
    },
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
    pub shared_glue: Vec<PhysicalSharedGlue>,
    /// Rendering recipes for every type reached by `f"{v:?}"`, in identity
    /// order. Empty when the module renders nothing structurally.
    pub structural_glue: Vec<PhysicalStructuralGlue>,
    /// What releasing each glue identity does: whether it runs user-visible
    /// action, so the runtime may walk it iteratively instead of nesting a
    /// native frame per level, and whether it can raise a fault the enclosing
    /// frame must own.
    pub releases: ReleaseEffects,
    /// Retained semantic authority for verification, never a physical classifier.
    pub(crate) type_facts: BTreeMap<TypeInstanceKey, hew_types::TypeFacts>,
    pub callables: Vec<PhysicalCallable>,
    pub functions: Vec<PhysicalFunction>,
    pub entry_callable: Option<CallableId>,
    pub entry_exit_plan: Option<EntryExitPlan>,
    pub string_literals: BTreeMap<hew_sir::StringLiteralId, String>,
    pub bytes_literals: BTreeMap<hew_sir::BytesLiteralId, Vec<u8>>,
    /// Regex-literal patterns in slot order; each is compiled once into the
    /// module's handle array and selected by index at a match arm.
    pub regex_patterns: Vec<String>,
    /// Source attribution for `hew build -g`.
    pub debug: PhysicalDebug,
    /// The compilation's declaration table: every `DefId` in the module
    /// indexes it.
    pub defs: std::sync::Arc<hew_types::DefTable>,
}

pub use hew_sir::SemDebugScope;

/// Source attribution carried for native debug metadata.
///
/// Every fact here is projected from SIR during lowering: codegen reads it and
/// decides nothing about naming or attribution. Only root-unit bodies appear —
/// a foreign module's spans belong to another file's coordinate space.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PhysicalDebug {
    /// Lexical blocks of the root compilation unit, in HIR scope order.
    pub scopes: Vec<hew_sir::SemDebugScope>,
    pub functions: BTreeMap<CallableId, PhysicalDebugFunction>,
    /// Source field names of each concrete record, in declaration order.
    pub records: BTreeMap<ResolvedTy, Vec<PhysicalDebugField>>,
    /// Source variant names of each concrete enum, in declaration (tag) order.
    pub enums: BTreeMap<ResolvedTy, Vec<PhysicalDebugVariant>>,
}

/// One aggregate or variant-payload field as the source spells it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalDebugField {
    pub name: String,
    pub ty: ResolvedTy,
}

/// One enum variant as the source spells it. Its position in the enum's row is
/// its tag value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalDebugVariant {
    pub name: String,
    pub fields: Vec<PhysicalDebugField>,
}

pub(crate) fn debug_field(field: &hew_sir::SemAggregateField) -> PhysicalDebugField {
    PhysicalDebugField {
        name: field.name.clone(),
        ty: field.ty.clone(),
    }
}

pub(crate) fn debug_variant_field(field: &hew_sir::SemVariantField) -> PhysicalDebugField {
    PhysicalDebugField {
        name: field.name.clone(),
        ty: field.ty.clone(),
    }
}

/// One body's source name, declaration point, named locals and op attribution.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PhysicalDebugFunction {
    pub name: String,
    /// Byte offset where the declaration begins.
    pub decl: u32,
    /// Byte offset just past the declaration, bounding the body's scopes.
    pub end: u32,
    /// Named source locals by the storage that realizes them.
    pub locals: BTreeMap<StorageId, PhysicalDebugLocal>,
    /// Source byte of each physical op, by block and index within the block.
    /// Sparse: an op lowered from a synthesized operation has no source point.
    pub sites: BTreeMap<(BlockId, u32), u32>,
}

/// One source binding and the storage that realizes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalDebugLocal {
    pub name: String,
    /// Byte offset of the binding that names this storage.
    pub decl: u32,
    /// One-based position when this storage realizes a parameter.
    pub parameter: Option<u32>,
}

/// Immutable evidence that physical MIR passed its structural verifier.
#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedPhysicalModule(pub(crate) PhysicalModule);

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
    pub(crate) fn new(message: impl Into<String>) -> Self {
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
