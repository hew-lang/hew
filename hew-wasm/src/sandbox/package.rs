//! The complete public v1 wire package. Instruction payloads live in bytecode.
//! Generated bindings and validation derive from these concrete carriers.
use super::bytecode as wire;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use ts_rs::TS;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "PackageV1")]
pub struct Package {
    /// Compiler-authored language accounting layouts; widths are never host sizes.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[ts(optional, as = "Option<_>")]
    pub managed_types: Vec<ManagedType>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub codecs: Vec<String>,
    #[serde(default)]
    #[ts(optional, as = "Option<_>")]
    pub structural_render: Vec<StructuralRecipe>,
    #[serde(default)]
    #[ts(optional, as = "Option<_>")]
    pub resources: Vec<Resource>,
    pub schema_version: SchemaVersion,
    pub hew_version: String,
    pub compiler_version: String,
    pub profile: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub entry: Option<Entry>,
    pub strings: Vec<String>,
    pub bytes: Vec<Vec<u8>>,
    pub regex_patterns: Vec<String>,
    pub aggregates: Vec<AggregateShape>,
    pub variants: Vec<VariantShape>,
    pub closures: Vec<Closure>,
    #[serde(default)]
    #[ts(optional, as = "Option<_>")]
    pub actors: Vec<Actor>,
    #[serde(default)]
    #[ts(optional, as = "Option<_>")]
    pub supervisors: Vec<Supervisor>,
    pub vtables: Vec<Vtable>,
    pub value_capabilities: Vec<ValueCapabilityPlan>,
    #[ts(type = "Array<RuntimeFamilyEntry>")]
    #[schemars(with = "Vec<RuntimeFamilySchema>")]
    pub runtime_families: Vec<RuntimeFamily>,
    pub externs: Vec<Extern>,
    /// Every suspension kind the instruction stream reaches, so load-time
    /// admission stays a walk of the package's own manifest.
    #[ts(type = "Array<Extract<TermV1, { op: \"suspend\" }>[\"kind\"]>")]
    pub suspend_kinds: Vec<String>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ManagedType {
    pub id: u32,
    /// Canonical width of a slot holding this value, encoded without JS rounding.
    pub slot_bytes: String,
    #[serde(flatten)]
    pub layout: ManagedLayout,
}

/// Exact semantic children supplied by SIR's physical type inventory.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ManagedLayout {
    Scalar,
    Handle {
        category: ManagedHandle,
    },
    String,
    Bytes,
    Vector {
        element: u32,
        share_on_copy: bool,
    },
    Array {
        element: u32,
        length: String,
        share_on_copy: bool,
    },
    Slice {
        element: u32,
        share_on_copy: bool,
    },
    Map {
        key: u32,
        value: u32,
    },
    Set {
        element: u32,
    },
    Aggregate {
        fields: Vec<u32>,
    },
    Variant {
        indirect: bool,
        payload_bytes: String,
        cases: Vec<Vec<u32>>,
    },
    Callable {
        captures: Option<Vec<u32>>,
    },
    Generator {
        yielded: u32,
        returned: u32,
        payload_bytes: String,
    },
    Shared {
        payload: u32,
        weak: bool,
    },
    Dynamic,
    Provider {
        provider: ManagedProvider,
        children: Vec<u32>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum ManagedHandle {
    Actor,
    Pointer,
    Borrow,
    Cancellation,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum ManagedProvider {
    Task,
    ActorCall,
    Stream,
    Sink,
    Json,
    Yaml,
    Toml,
    Opaque,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "EntryPlan")]
pub struct Entry {
    #[serde(skip)]
    pub properties: Vec<hew_codec::property::Shape>,
    pub function: u32,
    pub exit: EntryExit,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct AggregateShape {
    pub id: u32,
    pub name: String,
    pub fields: Vec<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub builtin: Option<AggregateBuiltin>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema, TS)]
pub enum AggregateBuiltin {
    Timestamp,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct VariantShape {
    pub id: u32,
    pub name: String,
    pub cases: Vec<VariantCase>,
    #[serde(default)]
    #[ts(optional, as = "Option<_>")]
    pub runtime_tags: BTreeMap<RuntimeVariantRole, u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct VariantCase {
    pub name: String,
    pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "ClosureShape")]
pub struct Closure {
    pub id: u32,
    pub entries: Vec<ClosureEntry>,
    pub fields: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ClosureEntry {
    pub body: u32,
    pub specialization: Option<u32>,
}

/// One demanded actor: its state seats, its lifecycle callables and the
/// handlers its mailbox dispatches, all as function indexes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "ActorShape")]
pub struct Actor {
    pub id: u32,
    pub name: String,
    pub state_fields: Vec<ActorField>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub state_type: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub init: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub start: Option<u32>,
    pub stop: Vec<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub crash: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub crash_info: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub crash_kind: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub crash_action: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exit: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub down: Option<u32>,
    pub handlers: Vec<ActorHandler>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mailbox_capacity: Option<u32>,
    pub overflow: ActorOverflow,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub coalesce: Option<Coalesce>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_heap_bytes: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum ActorOverflow {
    Block,
    DropNew,
    DropOld,
    Fail,
    Coalesce,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum CoalesceFallback {
    DropNew,
    DropOld,
    Fail,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum RestartStrategy {
    OneForOne,
    OneForAll,
    RestForOne,
    SimpleOneForOne,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum RestartPolicy {
    Permanent,
    Transient,
    Temporary,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ActorField {
    pub mutable: bool,
    pub deferred: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ActorHandler {
    pub name: String,
    pub message_id: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(type = "number | null")]
    pub every_ns: Option<i64>,
    pub callable: u32,
    pub params: u32,
    pub streams: bool,
    pub fallible: bool,
    pub result_shape: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct Coalesce {
    pub fallback: CoalesceFallback,
    pub keys: Vec<CoalesceKey>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct CoalesceKey {
    pub message: u32,
    pub param: u32,
}

/// One demanded supervisor: its restart policy and the children it starts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "SupervisorShape")]
pub struct Supervisor {
    pub id: u32,
    pub strategy: RestartStrategy,
    pub max_restarts: u32,
    pub window_secs: u32,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub backoff: bool,
    pub children: Vec<SupervisorChild>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub startup_order: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct SupervisorChild {
    pub name: String,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[ts(optional, as = "Option<_>")]
    pub dependencies: Vec<usize>,
    /// The role this child plays, as `{"actor": id}` or `{"supervisor": id}`.
    pub role: SupervisedRole,
    pub restart: RestartPolicy,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(type = "number | null")]
    #[schemars(range(max = i64::MAX))]
    pub stop_ns: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub stop_config: Option<usize>,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub is_pool: bool,
    pub spawn: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "VtableShape")]
pub struct Vtable {
    pub id: u32,
    pub slots: Vec<VtableSlot>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct VtableSlot {
    pub slot: u32,
    pub method: String,
    pub callee: u32,
    /// How the erased receiver crosses the dispatch boundary. A `dyn.call`
    /// passes the wrapped value to the callee's `self` parameter under this
    /// decision, which the verifier proves matches the boundary the concrete
    /// type was erased under.
    pub receiver: wire::BoundaryDecision,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub receiver_handback: bool,
}

/// The checker's selection for one `(type, capability)` pair.
///
/// `callable` names a user implementation; its absence is the derived
/// structural operation, which the VM performs over its own value
/// representation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "ValueCapability")]
pub struct ValueCapabilityPlan {
    pub id: u32,
    pub capability: Capability,
    pub ty: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub callable: Option<u32>,
    #[serde(default)]
    #[ts(optional, as = "Option<_>")]
    pub components: Vec<ValueComponentCapabilities>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub variants: Option<Vec<Vec<ValueComponentCapabilities>>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ValueComponentCapabilities {
    pub selected: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ordering: Option<ValueOrderingCapabilities>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ValueOrderingCapabilities {
    pub less: u32,
    pub less_equal: u32,
    pub greater: u32,
    pub greater_equal: u32,
}

pub use super::bytecode::RuntimeFamilyEntry as RuntimeFamily;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "ExternEntry")]
pub struct Extern {
    pub id: u32,
    pub symbol: String,
    /// The capability manifest's label for a native-only runtime endpoint,
    /// so a refusal can name what needs native execution. Admission still
    /// keys on `symbol` alone.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub native_capability: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "FunctionV1")]
pub struct Function {
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub actor_construction: bool,
    pub id: u32,
    pub name: String,
    pub params: Vec<Value>,
    pub entry: u32,
    pub places: Vec<Place>,
    pub blocks: Vec<Block>,
}

/// A semantic place carries exactly the fields needed by its storage origin.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(tag = "origin", rename_all = "snake_case")]
#[ts(rename = "PlaceDecl")]
pub enum Place {
    Local {
        id: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        managed_type: Option<u32>,
    },
    Runtime {
        id: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        managed_type: Option<u32>,
    },
    Aggregate {
        id: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        managed_type: Option<u32>,
        base: PlaceBase,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        shape: Option<u32>,
        field: u32,
    },
    Capture {
        id: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        managed_type: Option<u32>,
        environment: u32,
        field: u32,
    },
    ActorState {
        id: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        managed_type: Option<u32>,
        environment: u32,
        field: u32,
        initialized: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum PlaceBase {
    Place(u32),
    Value(u32),
}

pub type Value = wire::ValueDef;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[ts(rename = "BlockV1")]
pub struct Block {
    pub id: u32,
    pub params: Vec<Value>,
    pub ops: Vec<wire::OpV1>,
    pub term: wire::TermV1,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum SchemaVersion {
    #[serde(rename = "hew.sandbox.bytecode.v1")]
    V1,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum EntryExit {
    Unit,
    Status,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema, TS,
)]
pub enum RuntimeVariantRole {
    CrashKindFault,
    CrashKindHeapExceeded,
    CrashKindPartitionDetected,
    CrashKindHandlerFailed,
    CrashKindTimerOverdue,
    OptionSome,
    OptionNone,
    ResultOk,
    ResultErr,
    ActorErrorRejected,
    ActorErrorFailed,
    ActorErrorTrapped,
    ActorErrorRefused,
    CallerActor,
    CallerTask,
    CallerTimer,
    CallerNode,
    ActorErrorDead,
    ActorErrorTimedOut,
    ActorErrorNodeNotRunning,
    ActorErrorRoutingFailed,
    ActorErrorEncodeFailed,
    ActorErrorMessageTooLarge,
    ActorErrorConnectionDropped,
    ActorErrorPartition,
    SendErrorFull,
    SendErrorClosed,
    SendErrorPartition,
    SendErrorStaleRef,
    SendErrorLocalShutdown,
    SendErrorCancelled,
    SendErrorVersionMismatch,
    SendErrorBackpressure,
    SendErrorDead,
    SendErrorUnauthorized,
    SendErrorInvalidMessage,
    SendErrorMessageTooLarge,
    SendErrorTimedOut,
    SendErrorWriteTimedOut,
    DeliveryAccepted,
    DeliveryDiscarded,
    LinkErrorNoContext,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum Capability {
    Eq,
    Hash,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum SupervisedRole {
    Actor(u32),
    Supervisor(u32),
}

/// Resource destruction follows the compiler-authored close contract.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Resource {
    Record {
        shape: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        ty: Option<String>,
        close: u32,
    },
    Opaque {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        ty: Option<String>,
        close: u32,
    },
    Nominal {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        ty: Option<String>,
        release: String,
    },
    Task,
    ActorCall,
    ActorRequest,
    Generator,
    Stream,
    Sink,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum StructuralRecipe {
    Scalar,
    Unit,
    Display {
        callee: u32,
    },
    Identity {
        name: String,
    },
    Tuple {
        members: Vec<u32>,
    },
    Vector {
        members: Vec<u32>,
    },
    Map {
        members: Vec<u32>,
    },
    Record {
        name: String,
        fields: Vec<StructuralField>,
    },
    Enum {
        cases: Vec<StructuralCase>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct StructuralField {
    pub name: String,
    pub recipe: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct StructuralCase {
    pub name: String,
    pub kind: StructuralCaseKind,
    pub fields: Vec<StructuralField>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum StructuralCaseKind {
    Unit,
    Tuple,
    Struct,
}

// RuntimeFamilyEntry has a custom serializer preserving v1 family/detail.
// Its schema is filled from RuntimeCallFamily by the binding generator.
#[derive(JsonSchema)]
#[schemars(rename = "RuntimeFamilyEntry")]
#[allow(
    dead_code,
    reason = "schema placeholder replaced by the authoritative runtime enum"
)]
struct RuntimeFamilySchema {
    id: u32,
}
