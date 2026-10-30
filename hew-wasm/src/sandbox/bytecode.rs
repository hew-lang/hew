//! Concrete v1 instruction wire types, shared by SIR projection and generated VM bindings.
//!
//! Runtime identities and suspension payloads retain their v1 JSON representation.
//! Host admission and execution policy consume these typed contracts.
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use ts_rs::TS;

/// Runtime identity at the sandbox boundary. The compiler enum owns its vocabulary.
#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeFamilyEntry {
    pub id: u32,
    pub identity: hew_types::RuntimeCallFamily,
    /// Unit identities accept both omitted and explicit-null v1 detail fields.
    pub null_detail: bool,
}

impl Serialize for RuntimeFamilyEntry {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap as _;
        let value = serde_json::to_value(self.identity).map_err(serde::ser::Error::custom)?;
        let mut map = serializer.serialize_map(None)?;
        map.serialize_entry("id", &self.id)?;
        match value {
            serde_json::Value::String(name) => {
                map.serialize_entry("family", &name)?;
                if self.null_detail {
                    map.serialize_entry("detail", &())?;
                }
            }
            serde_json::Value::Object(fields) if fields.len() == 1 => {
                let (name, detail) = fields
                    .into_iter()
                    .next()
                    .ok_or_else(|| serde::ser::Error::custom("runtime identity has no variant"))?;
                map.serialize_entry("family", &name)?;
                map.serialize_entry("detail", &detail)?;
            }
            _ => return Err(serde::ser::Error::custom("runtime identity is not an enum")),
        }
        map.end()
    }
}

impl<'de> Deserialize<'de> for RuntimeFamilyEntry {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct Entry {
            id: u32,
            family: String,
            #[serde(default, deserialize_with = "present_nullable")]
            #[expect(
                clippy::option_option,
                reason = "v1 preserves omitted, present-null and present-value detail fields"
            )]
            detail: Option<Option<serde_json::Value>>,
        }
        let entry = Entry::deserialize(deserializer)?;
        let null_detail = entry.detail == Some(None);
        let identity = match entry.detail.flatten() {
            Some(detail) => serde_json::json!({entry.family: detail}),
            None => entry.family.into(),
        };
        Ok(Self {
            id: entry.id,
            identity: serde_json::from_value(identity).map_err(serde::de::Error::custom)?,
            null_detail,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum JoinMode {
    Wait,
    CancelLosers,
    PropagateFault,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(rename_all = "snake_case")]
pub enum SelectionOrder {
    Source,
    Completion,
}

/// The kind and payload travel together so consumers cannot mix identities.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(tag = "kind")]
pub enum Suspension {
    Sleep {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    SleepUntil {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    Await {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    ActorSend {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    RestartWait {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    Read {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    Accept {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    CallClosure {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    Timeout {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    ScopeDeadline {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    Yield {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    GeneratorNext {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        detail: Option<Option<()>>,
    },
    Safepoint {
        detail: SafepointDetail,
    },
    ServiceAcquire {
        detail: ServiceAcquireDetail,
    },
    SemaphoreAcquire {
        detail: SemaphoreAcquireDetail,
    },
    NativeIo {
        detail: NativeIoDetail,
    },
    Offload {
        detail: OffloadDetail,
    },
    Ask {
        detail: AskDetail,
    },
    Join {
        detail: JoinDetail,
    },
    StreamSend {
        detail: StreamSendDetail,
    },
    StreamNext {
        detail: StreamNextDetail,
    },
    Select {
        detail: SelectDetail,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct SafepointDetail {
    pub budget: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ServiceAcquireDetail {
    pub actor: u32,
    pub export: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct SemaphoreAcquireDetail {
    pub operation: hew_types::runtime_call::SemaphoreOp,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct NativeIoDetail {
    pub operation: hew_types::runtime_call::AsyncIoOp,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct OffloadDetail {
    pub function: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct AskDetail {
    pub actor: u32,
    pub message: u32,
    pub policy: SendPolicy,
    pub sealed: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct JoinDetail {
    pub scope: u32,
    pub mode: JoinMode,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct StreamSendDetail {
    pub park: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct StreamNextDetail {
    pub park: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct SelectDetail {
    pub has_timeout: bool,
    pub order: SelectionOrder,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum Own {
    #[serde(rename = "none")]
    None,
    #[serde(rename = "owned")]
    Owned,
    #[serde(rename = "guaranteed")]
    Guaranteed,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum BoundaryDecision {
    #[serde(rename = "borrow")]
    Borrow,
    #[serde(rename = "borrow_mut")]
    BorrowMut,
    #[serde(rename = "copy")]
    Copy,
    #[serde(rename = "move")]
    Move,
    #[serde(rename = "snapshot")]
    Snapshot,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum TrapName {
    #[serde(rename = "heap_exceeded")]
    HeapExceeded,
    #[serde(rename = "integer_overflow")]
    IntegerOverflow,
    #[serde(rename = "divide_by_zero")]
    DivideByZero,
    #[serde(rename = "signed_min_div_neg_one")]
    SignedMinDivNegOne,
    #[serde(rename = "shift_out_of_range")]
    ShiftOutOfRange,
    #[serde(rename = "vector_bounds")]
    VectorBounds,
    #[serde(rename = "actor_refused")]
    ActorRefused,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct SpanV1 {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct BoundaryOperand {
    pub value: u32,
    pub decision: BoundaryDecision,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct Edge {
    pub to: u32,
    pub args: Vec<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ValueDef {
    pub value: u32,
    pub own: Own,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub managed_type: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum CallResultNever {
    #[serde(rename = "never")]
    Never,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(untagged)]
pub enum CallResult {
    Unit(()),
    Never(CallResultNever),
    ValueDef(ValueDef),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct SwitchArm {
    pub variant: u32,
    pub fields: Vec<ValueDef>,
    pub edge: Edge,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct CheckedFailure {
    pub trap: TrapName,
    pub edge: Edge,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct RequestShapes {
    pub failure: Option<u32>,
    pub reason: Option<u32>,
    pub message: Option<u32>,
    pub request: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum SendPolicy {
    #[serde(rename = "reject")]
    Reject,
    #[serde(rename = "wait")]
    Wait,
    #[serde(rename = "drop_newest")]
    DropNewest,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct ActorProtocol {
    pub actor: u32,
    pub message: u32,
    pub policy: SendPolicy,
    pub sealed: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum Nonfinite {
    #[serde(rename = "nan")]
    Nan,
    #[serde(rename = "inf")]
    Inf,
    #[serde(rename = "-inf")]
    InfNegative,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct DirectInvocation {
    pub direct: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct MutableInvocation {
    pub mutable: u32,
    pub receiver: BoundaryOperand,
    pub staged: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct DynInvocation {
    pub r#dyn: u32,
    pub method: String,
    pub receiver: BoundaryOperand,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct IndirectInvocation {
    pub receiver: BoundaryOperand,
    pub specialization: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(untagged)]
pub enum InvocationCallee {
    Variant0(DirectInvocation),
    Variant1(MutableInvocation),
    Variant2(DynInvocation),
    Variant3(IndirectInvocation),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(tag = "op")]
pub enum OpKind {
    #[serde(rename = "coverage.hit")]
    CoverageHit { counter: u32 },
    #[serde(rename = "const.int")]
    ConstInt { dst: u32, value: String, ty: String },
    #[serde(rename = "const.bool")]
    ConstBool { dst: u32, value: bool },
    #[serde(rename = "const.float")]
    ConstFloat {
        dst: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        value: Option<f64>,
        ty: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        nonfinite: Option<Nonfinite>,
    },
    #[serde(rename = "const.char")]
    ConstChar { dst: u32, value: String },
    #[serde(rename = "const.unit")]
    ConstUnit { dst: u32 },
    #[serde(rename = "const.duration")]
    ConstDuration { dst: u32, nanos: String },
    #[serde(rename = "const.str")]
    ConstStr { dst: u32, r#str: u32 },
    #[serde(rename = "const.bytes")]
    ConstBytes { dst: u32, bytes: u32 },
    #[serde(rename = "copy_value")]
    CopyValue { dst: u32, source: u32 },
    #[serde(rename = "move")]
    Move { dst: u32, source: u32 },
    #[serde(rename = "fork")]
    Fork { dst: u32, source: u32 },
    #[serde(rename = "destroy_value")]
    DestroyValue {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        value: u32,
    },
    #[serde(rename = "begin_borrow")]
    BeginBorrow { dst: u32, owner: u32 },
    #[serde(rename = "end_borrow")]
    EndBorrow {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        borrow: u32,
    },
    #[serde(rename = "finish_linear_receiver")]
    FinishLinearReceiver {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
    },
    #[serde(rename = "alloc_place")]
    AllocPlace {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        place: u32,
    },
    #[serde(rename = "store.init")]
    StoreInit {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        place: u32,
        value: u32,
    },
    #[serde(rename = "store.assign")]
    StoreAssign {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        place: u32,
        value: u32,
    },
    #[serde(rename = "load.copy")]
    LoadCopy { dst: u32, place: u32 },
    #[serde(rename = "load.take")]
    LoadTake { dst: u32, place: u32 },
    #[serde(rename = "load.borrow")]
    LoadBorrow { dst: u32, place: u32 },
    #[serde(rename = "end_lifetime")]
    EndLifetime {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        place: u32,
    },
    #[serde(rename = "tuple.make")]
    TupleMake { dst: u32, elements: Vec<u32> },
    #[serde(rename = "tuple.get")]
    TupleGet { dst: u32, tuple: u32, index: u32 },
    #[serde(rename = "aggregate.make")]
    AggregateMake {
        dst: u32,
        shape: Option<u32>,
        fields: Vec<u32>,
    },
    #[serde(rename = "aggregate.project_copy")]
    AggregateProjectCopy {
        dst: u32,
        shape: Option<u32>,
        aggregate: u32,
        field: u32,
    },
    #[serde(rename = "aggregate.project_borrow")]
    AggregateProjectBorrow {
        dst: u32,
        shape: Option<u32>,
        aggregate: u32,
        field: u32,
    },
    #[serde(rename = "destructure")]
    Destructure {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        shape: Option<u32>,
        aggregate: u32,
        results: Vec<u32>,
    },
    #[serde(rename = "array.make")]
    ArrayMake { dst: u32, elements: Vec<u32> },
    #[serde(rename = "array.repeat")]
    ArrayRepeat { dst: u32, value: u32 },
    #[serde(rename = "variant.make")]
    VariantMake {
        dst: u32,
        shape: u32,
        variant: u32,
        fields: Vec<u32>,
    },
    #[serde(rename = "variant.is")]
    VariantIs {
        dst: u32,
        shape: u32,
        variant: u32,
        source: u32,
    },
    #[serde(rename = "variant.project_copy")]
    VariantProjectCopy {
        dst: u32,
        shape: u32,
        variant: u32,
        source: u32,
        field: u32,
    },
    #[serde(rename = "variant.project_borrow")]
    VariantProjectBorrow {
        dst: u32,
        shape: u32,
        variant: u32,
        source: u32,
        field: u32,
    },
    #[serde(rename = "variant.destructure")]
    VariantDestructure {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        shape: u32,
        variant: u32,
        source: u32,
        results: Vec<u32>,
    },
    #[serde(rename = "unary")]
    Unary {
        dst: u32,
        unary_op: String,
        value: u32,
        ty: String,
    },
    #[serde(rename = "binary")]
    Binary {
        dst: u32,
        binary_op: String,
        lhs: u32,
        rhs: u32,
        ty: String,
    },
    #[serde(rename = "cast")]
    Cast {
        dst: u32,
        value: u32,
        from: String,
        to: String,
    },
    #[serde(rename = "str.eq")]
    StrEq { dst: u32, lhs: u32, rhs: u32 },
    #[serde(rename = "bytes.eq")]
    BytesEq { dst: u32, lhs: u32, rhs: u32 },
    #[serde(rename = "function.make")]
    FunctionMake { dst: u32, callable: u32 },
    #[serde(rename = "closure.make")]
    ClosureMake {
        dst: u32,
        closure: u32,
        fields: Vec<u32>,
    },
    #[serde(rename = "callable.coerce")]
    CallableCoerce {
        dst: u32,
        source: u32,
        specialization: Option<u32>,
    },
    #[serde(rename = "dyn.make")]
    DynMake { dst: u32, vtable: u32, value: u32 },
    #[serde(rename = "register_defer")]
    RegisterDefer {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        defer: u32,
        scope: u32,
        dependencies: Vec<u32>,
    },
    #[serde(rename = "generator.make")]
    GeneratorMake {
        dst: u32,
        closure: u32,
        callable: u32,
    },
    #[serde(rename = "actor.ingress_adapter")]
    ActorIngressAdapter {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        actor: u32,
        message: u32,
    },
    #[serde(rename = "stream.pipe")]
    StreamPipe {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        capacity: u32,
        results: Vec<ValueDef>,
    },
    #[serde(rename = "task_scope.enter")]
    TaskScopeEnter {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        scope: u32,
        parent: Option<u32>,
        duration: Option<u32>,
    },
    #[serde(rename = "task_scope.close")]
    TaskScopeClose {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        dst: Option<Option<u32>>,
        scope: u32,
    },
    #[serde(rename = "task.race")]
    TaskRace {
        dst: u32,
        scope: u32,
        members: Vec<u32>,
        member_slot_bytes: String,
    },
    #[serde(rename = "task.spawn")]
    TaskSpawn { dst: u32, scope: u32, callable: u32 },
    #[serde(rename = "task.invoke")]
    TaskInvoke {
        dst: u32,
        scope: u32,
        callee: InvocationCallee,
        args: Vec<BoundaryOperand>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum CodecDirection {
    #[serde(rename = "encode")]
    Encode,
    #[serde(rename = "decode")]
    Decode,
    #[serde(rename = "test_input")]
    TestInput,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct DecodedCodec {
    pub shape: u32,
    pub ok: u32,
    pub error: u32,
    pub error_root: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct VariantResultShapes {
    pub error: u32,
    pub error_len: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct SubmissionShapes {
    pub success: Option<u32>,
    pub failure: Option<u32>,
    pub reason: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum TimestampBinaryOp {
    Add,
    Subtract,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(tag = "op")]
pub enum TermKind {
    #[serde(rename = "materialize")]
    Materialize {
        operation: Box<OpV1>,
        normal: Edge,
        failure: Edge,
    },
    #[serde(rename = "return")]
    Return { value: Option<BoundaryOperand> },
    #[serde(rename = "goto")]
    Goto { edge: Edge },
    #[serde(rename = "branch")]
    Branch {
        condition: u32,
        then: Edge,
        r#else: Edge,
    },
    #[serde(rename = "switch.variant")]
    SwitchVariant {
        shape: u32,
        scrutinee: u32,
        arms: Vec<SwitchArm>,
    },
    #[serde(rename = "checked.binary")]
    CheckedBinary {
        binary_op: String,
        lhs: u32,
        rhs: u32,
        result: ValueDef,
        normal: Edge,
        failures: Vec<CheckedFailure>,
        ty: String,
    },
    #[serde(rename = "timestamp.binary")]
    TimestampBinary {
        binary_op: TimestampBinaryOp,
        lhs: u32,
        rhs: u32,
        lhs_timestamp: bool,
        rhs_timestamp: bool,
        shape: Option<u32>,
        result: ValueDef,
        normal: Edge,
        failures: Vec<CheckedFailure>,
    },
    #[serde(rename = "unreachable")]
    Unreachable {},
    #[serde(rename = "call")]
    Call {
        args: Vec<BoundaryOperand>,
        result: CallResult,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        normal: Option<Option<Edge>>,
        unwind: Option<Edge>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        callee: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        handback: Option<Option<ValueDef>>,
    },
    #[serde(rename = "indirect.call")]
    IndirectCall {
        args: Vec<BoundaryOperand>,
        result: CallResult,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        normal: Option<Option<Edge>>,
        unwind: Option<Edge>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        callee: BoundaryOperand,
        specialization: Option<u32>,
    },
    #[serde(rename = "dyn.call")]
    DynCall {
        args: Vec<BoundaryOperand>,
        result: CallResult,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        normal: Option<Option<Edge>>,
        unwind: Option<Edge>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        receiver: BoundaryOperand,
        slot: u32,
    },
    #[serde(rename = "value.call")]
    ValueCall {
        args: Vec<BoundaryOperand>,
        result: CallResult,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        normal: Option<Option<Edge>>,
        unwind: Option<Edge>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        plan: u32,
    },
    #[serde(rename = "wire.codec")]
    WireCodec {
        args: Vec<BoundaryOperand>,
        result: CallResult,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        normal: Option<Option<Edge>>,
        unwind: Option<Edge>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        codec: u32,
        format: u32,
        direction: CodecDirection,
        decoded: Option<DecodedCodec>,
        callbacks: BTreeMap<String, Vec<u32>>,
    },
    #[serde(rename = "runtime.call")]
    RuntimeCall {
        args: Vec<BoundaryOperand>,
        result: CallResult,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        normal: Option<Option<Edge>>,
        unwind: Option<Edge>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        shared_payload_type: Option<String>,
        family: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        structural: Option<Option<u32>>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        callbacks: Option<Vec<u32>>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        releases_contents: Option<bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        keeps_inputs_on_failure: Option<bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        result_member_shapes: Option<Vec<Option<u32>>>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        variant_result_shapes: Option<VariantResultShapes>,
        result_shape: Option<u32>,
        /// Checked aggregate descriptor supplied by the SIR result type.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        aggregate_result_shape: Option<Option<u32>>,
    },
    #[serde(rename = "extern.call")]
    ExternCall {
        /// Exact resource release slot selected from the checked result type.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        result_resource: Option<u32>,
        args: Vec<BoundaryOperand>,
        result: CallResult,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        normal: Option<Option<Edge>>,
        unwind: Option<Edge>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        r#extern: u32,
        result_shape: Option<u32>,
        /// Checked aggregate descriptor supplied by the SIR result type.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        aggregate_result_shape: Option<Option<u32>>,
    },
    #[serde(rename = "actor.call")]
    ActorCall {
        args: Vec<BoundaryOperand>,
        result: CallResult,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        normal: Option<Option<Edge>>,
        unwind: Option<Edge>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        operation: ActorOperation,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        submission_shapes: Option<Option<SubmissionShapes>>,
        result_shape: Option<u32>,
        error_shape: Option<u32>,
    },
    #[serde(rename = "panic")]
    Panic {
        message: BoundaryOperand,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        assertion: Option<Option<[BoundaryOperand; 3]>>,
        cleanup: Edge,
    },
    #[serde(rename = "trap")]
    Trap { trap: TrapName },
    #[serde(rename = "checked_raise")]
    CheckedRaise { trap: TrapName, cleanup: Edge },
    #[serde(rename = "cleanup.dispatch")]
    CleanupDispatch { normal: Edge, fault: Edge },
    #[serde(rename = "resume_unwind")]
    ResumeUnwind {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        handback: Option<Option<BoundaryOperand>>,
    },
    #[serde(rename = "enter_defer")]
    EnterDefer { defer: u32, park: u32, body: Edge },
    #[serde(rename = "finish_defer")]
    FinishDefer { defer: u32, park: u32, next: Edge },
    #[serde(rename = "recover_fault")]
    RecoverFault {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        result_shape: Option<Option<u32>>,
        result: ValueDef,
        deadline_variant: u32,
        fault_variant: u32,
        normal: Edge,
        unwind: Option<Edge>,
    },
    #[serde(rename = "suspend")]
    Suspend {
        #[serde(flatten)]
        suspension: Suspension,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        request_shapes: Option<Option<RequestShapes>>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        result_shape: Option<Option<u32>>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        #[serde(deserialize_with = "present_nullable")]
        error_shape: Option<Option<u32>>,
        inputs: Vec<BoundaryOperand>,
        result: CallResult,
        resumes: Vec<Edge>,
        cancel: Option<Edge>,
        unwind: Option<Edge>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(untagged)]
pub enum LifecycleTarget {
    Actor { actor: u32 },
    Supervisor { supervisor: u32 },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub enum TimerSendPolicy {
    #[serde(rename = "reject")]
    Reject,
    #[serde(rename = "drop_newest")]
    DropNewest,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
#[serde(tag = "op")]
pub enum ActorOperation {
    #[serde(rename = "identity")]
    Identity {},
    #[serde(rename = "spawn")]
    Spawn { actor: u32 },
    #[serde(rename = "self_handle")]
    SelfHandle { actor: u32 },
    #[serde(rename = "stop")]
    Stop {
        #[serde(flatten)]
        target: LifecycleTarget,
    },
    #[serde(rename = "terminate")]
    Terminate {
        #[serde(flatten)]
        target: LifecycleTarget,
    },
    #[serde(rename = "await_stopped")]
    AwaitStopped {
        #[serde(flatten)]
        target: LifecycleTarget,
    },
    #[serde(rename = "await_restarted")]
    AwaitRestarted {
        #[serde(flatten)]
        target: LifecycleTarget,
    },
    #[serde(rename = "call_start")]
    CallStart { protocol: ActorProtocol },
    #[serde(rename = "call_take")]
    CallTake { protocol: ActorProtocol },
    #[serde(rename = "submit")]
    Submit { actor: u32, policy: SendPolicy },
    #[serde(rename = "stream_start")]
    StreamStart { actor: u32, message: u32 },
    #[serde(rename = "timer_schedule")]
    TimerSchedule {
        actor: u32,
        message: u32,
        resource: u32,
        policy: TimerSendPolicy,
    },
    #[serde(rename = "local_observation")]
    LocalObservation { kind: String },
    #[serde(rename = "supervisor_spawn")]
    SupervisorSpawn { supervisor: u32 },
    #[serde(rename = "supervisor_child")]
    SupervisorChild { supervisor: u32, child: u32 },
    #[serde(rename = "supervisor_pool_view")]
    SupervisorPoolView { supervisor: u32, child: u32 },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct OpV1 {
    #[serde(flatten)]
    pub kind: OpKind,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[ts(optional, as = "Option<_>")]
    pub definitions: Vec<ValueDef>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub own: Option<Own>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "present_nullable")]
    pub span: Option<Option<SpanV1>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema, TS)]
pub struct TermV1 {
    #[serde(flatten)]
    pub kind: TermKind,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "present_nullable")]
    pub span: Option<Option<SpanV1>>,
}

/// Generate the operation/terminator declarations from their Rust wire types.
///
/// # Errors
/// Returns an error if a derive no longer describes tagged instruction variants.
pub fn typescript() -> Result<String, String> {
    use std::any::TypeId;
    use std::collections::BTreeSet;
    use std::fmt::Write as _;
    struct Declarations {
        seen: BTreeSet<TypeId>,
        entries: BTreeMap<String, String>,
        config: ts_rs::Config,
    }
    impl ts_rs::TypeVisitor for Declarations {
        fn visit<T: TS + 'static + ?Sized>(&mut self) {
            if !self.seen.insert(TypeId::of::<T>()) {
                return;
            }
            if T::output_path().is_some() {
                self.entries.insert(
                    T::ident(&self.config),
                    format!("export {}\n", T::decl(&self.config)),
                );
            }
            T::visit_dependencies(self);
        }
    }
    let mut declarations = Declarations {
        seen: BTreeSet::new(),
        entries: BTreeMap::new(),
        config: ts_rs::Config::default(),
    };
    ts_rs::TypeVisitor::visit::<super::package::Package>(&mut declarations);
    ts_rs::TypeVisitor::visit::<hew_codec::property::Shape>(&mut declarations);
    ts_rs::TypeVisitor::visit::<OpV1>(&mut declarations);
    ts_rs::TypeVisitor::visit::<TermV1>(&mut declarations);
    ts_rs::TypeVisitor::visit::<hew_types::RuntimeCallFamily>(&mut declarations);
    let mut output = String::from(
        "// Generated from the Rust sandbox package and instruction types. Run make sandbox-bytecode.\n",
    );
    for declaration in declarations.entries.into_values() {
        output.push_str(&declaration);
    }
    output.push_str("type RuntimeIdentity<T> = T extends string ? { family: T; detail?: null } : { [K in keyof T]: { family: K; detail: T[K] } }[keyof T];\nexport type RuntimeFamilyEntry = { id: number } & RuntimeIdentity<RuntimeCallFamily>;\n");
    for (name, discriminator, schema) in [
        ("OP_NAMES", "op", schemars::schema_for!(OpKind).to_value()),
        (
            "TERM_NAMES",
            "op",
            schemars::schema_for!(TermKind).to_value(),
        ),
        (
            "SUSPENSION_NAMES",
            "kind",
            schemars::schema_for!(Suspension).to_value(),
        ),
    ] {
        let names = discriminator_names(&schema, discriminator)?;
        writeln!(
            &mut output,
            "export const {name} = {} as const;",
            serde_json::to_string(&names).map_err(|error| error.to_string())?
        )
        .map_err(|error| error.to_string())?;
    }
    output.push_str("export type OpName = OpV1[\"op\"];\nexport type TermName = TermV1[\"op\"];\n");
    Ok(output)
}

fn discriminator_names(
    schema: &serde_json::Value,
    discriminator: &str,
) -> Result<Vec<String>, String> {
    schema["oneOf"]
        .as_array()
        .ok_or_else(|| "wire enum is no longer tagged".to_string())?
        .iter()
        .map(|variant| {
            variant["properties"][discriminator]["const"]
                .as_str()
                .map(str::to_string)
                .ok_or_else(|| "wire variant has no discriminator".to_string())
        })
        .collect()
}

/// Derive the complete package schema, including the custom runtime identity wire shape.
///
/// # Errors
/// Returns an error if the authoritative runtime enum cannot be projected.
pub fn package_schema() -> Result<serde_json::Value, String> {
    fn integer_ranges(value: &mut serde_json::Value) {
        match value {
            serde_json::Value::Object(object) => {
                if object.get("format").and_then(serde_json::Value::as_str) == Some("uint32") {
                    object.insert("maximum".to_string(), u32::MAX.into());
                }
                for child in object.values_mut() {
                    integer_ranges(child);
                }
            }
            serde_json::Value::Array(array) => {
                for child in array {
                    integer_ranges(child);
                }
            }
            _ => {}
        }
    }
    let mut envelope = schemars::schema_for!(super::package::Package).to_value();
    envelope["$id"] = "https://hew.sh/schemas/sandbox/bytecode-v1.schema.json".into();
    integer_ranges(&mut envelope);
    let definitions = envelope
        .get_mut("$defs")
        .and_then(serde_json::Value::as_object_mut)
        .ok_or_else(|| "bytecode envelope has no definitions".to_string())?;
    let runtime = schemars::schema_for!(hew_types::RuntimeCallFamily).to_value();
    if let Some(nested) = runtime["$defs"].as_object() {
        definitions.extend(nested.clone());
    }
    let mut variants = Vec::new();
    for variant in runtime["oneOf"]
        .as_array()
        .ok_or("runtime identity is not an enum")?
    {
        let unit_names = variant["enum"].as_array().cloned().or_else(|| {
            variant
                .get("const")
                .filter(|name| name.is_string())
                .map(|name| vec![name.clone()])
        });
        if let Some(names) = unit_names {
            for name in names {
                variants.push(serde_json::json!({
                    "properties": {"family": {"const": name}, "detail": {"type": "null"}}
                }));
            }
        } else {
            let fields = variant["properties"]
                .as_object()
                .ok_or_else(|| format!("runtime payload has no properties: {variant}"))?;
            if fields.len() != 1 {
                return Err("runtime payload must have exactly one variant".to_string());
            }
            let (name, detail) = fields
                .iter()
                .next()
                .ok_or("runtime payload has no variant")?;
            variants.push(serde_json::json!({
                "required": ["detail"],
                "properties": {"family": {"const": name}, "detail": detail}
            }));
        }
    }
    definitions.insert(
        "RuntimeFamilyEntry".to_string(),
        serde_json::json!({
            "type": "object", "additionalProperties": false,
            "required": ["id", "family"],
            "properties": {"id": {"type": "integer", "minimum": 0, "maximum": u32::MAX}, "family": {"type": "string"}, "detail": {}},
            "oneOf": variants,
        }),
    );
    let mut codec = schemars::schema_for!(hew_codec::property::Shape).to_value();
    integer_ranges(&mut codec);
    if let Some(nested) = codec
        .as_object_mut()
        .and_then(|object| object.remove("$defs"))
    {
        definitions.extend(
            nested
                .as_object()
                .ok_or("codec definitions are not an object")?
                .clone(),
        );
    }
    definitions.insert("CodecShape".to_string(), codec);
    definitions.insert(
        "SuspensionKind".to_string(),
        serde_json::json!({
            "type": "string",
            "enum": discriminator_names(&schemars::schema_for!(Suspension).to_value(), "kind")?,
        }),
    );
    envelope["properties"]["suspend_kinds"]["items"] =
        serde_json::json!({"$ref": "#/$defs/SuspensionKind"});
    envelope["$comment"] =
        "Generated from the Rust package and instruction DTOs; run make sandbox-bytecode".into();
    Ok(envelope)
}

/// Preserve the difference between an omitted wire field and a present null.
#[expect(
    clippy::option_option,
    reason = "v1 distinguishes omitted, present-null and present-value fields"
)]
fn present_nullable<'de, D, T>(deserializer: D) -> Result<Option<Option<T>>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer).map(Some)
}
