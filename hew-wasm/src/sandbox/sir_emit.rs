//! `hew.sandbox.bytecode.v1`: a projection of verified ownership SIR.
//!
//! The contract is `hew-sandbox-vm/bytecode/package-v1.md` and its opcode
//! registry. This walker decides nothing: it names the facts `hew_sir` already
//! proved so the VM can execute them. Ownership, cleanup edges, checked
//! failures and suspension points are copied across, never re-derived from a
//! type, a name or a symbol spelling.
//!
//! The match on [`SemOpKind`] and [`SemTerminator`] is exhaustive with no
//! wildcard arm, so a new SIR variant fails to compile here rather than
//! silently disappearing from the package.

use std::collections::BTreeMap;

use super::bytecode as wire;
use hew_sir::{
    AggregateShapeRef, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind, CallableId, Edge,
    Operand, OwnKind, Provenance, RuntimeVariantRole, SemBlock, SemFunction, SemModule, SemOp,
    SemOpKind, SemTerminator, SuspendKind, TrapKind, ValueDef,
};
use serde::{Deserialize, Serialize};

pub const SCHEMA_VERSION: &str = "hew.sandbox.bytecode.v1";

/// A construct the walker cannot name in the package.
///
/// Every reachable construct has an opcode; this reports the ones whose
/// supporting tables the sequential package does not carry, so a caller that
/// routed a module here in error fails closed instead of emitting a package
/// the VM would silently mis-execute.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmitError {
    pub message: String,
}

impl EmitError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for EmitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for EmitError {}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Package {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub codecs: Vec<String>,
    pub structural_render: Vec<serde_json::Value>,
    pub resources: Vec<serde_json::Value>,
    pub schema_version: String,
    pub hew_version: String,
    pub compiler_version: String,
    pub profile: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub entry: Option<Entry>,
    pub strings: Vec<String>,
    pub bytes: Vec<Vec<u8>>,
    pub regex_patterns: Vec<String>,
    pub aggregates: Vec<AggregateShape>,
    pub variants: Vec<VariantShape>,
    pub closures: Vec<Closure>,
    pub actors: Vec<Actor>,
    pub supervisors: Vec<Supervisor>,
    pub vtables: Vec<Vtable>,
    pub value_capabilities: Vec<ValueCapabilityPlan>,
    pub runtime_families: Vec<RuntimeFamily>,
    pub externs: Vec<Extern>,
    /// Every suspension kind the instruction stream reaches, so load-time
    /// admission stays a walk of the package's own manifest.
    pub suspend_kinds: Vec<String>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Entry {
    #[serde(skip)]
    pub properties: Vec<hew_codec::property::Shape>,
    pub function: u32,
    pub exit: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AggregateShape {
    pub id: u32,
    pub name: String,
    pub fields: Vec<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub builtin: Option<AggregateBuiltin>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AggregateBuiltin {
    Timestamp,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariantShape {
    pub id: u32,
    pub name: String,
    pub cases: Vec<VariantCase>,
    pub runtime_tags: BTreeMap<String, u32>,
}

fn runtime_role_name(role: RuntimeVariantRole) -> &'static str {
    use RuntimeVariantRole as Role;
    match role {
        Role::CrashKindFault => "CrashKindFault",
        Role::CrashKindHeapExceeded => "CrashKindHeapExceeded",
        Role::CrashKindPartitionDetected => "CrashKindPartitionDetected",
        Role::CrashKindHandlerFailed => "CrashKindHandlerFailed",
        Role::CrashKindTimerOverdue => "CrashKindTimerOverdue",
        Role::OptionSome => "OptionSome",
        Role::OptionNone => "OptionNone",
        Role::ResultOk => "ResultOk",
        Role::ResultErr => "ResultErr",
        Role::ActorErrorRejected => "ActorErrorRejected",
        Role::ActorErrorFailed => "ActorErrorFailed",
        Role::ActorErrorTrapped => "ActorErrorTrapped",
        Role::ActorErrorRefused => "ActorErrorRefused",
        Role::CallerActor => "CallerActor",
        Role::CallerTask => "CallerTask",
        Role::CallerTimer => "CallerTimer",
        Role::CallerNode => "CallerNode",
        Role::ActorErrorDead => "ActorErrorDead",
        Role::ActorErrorTimedOut => "ActorErrorTimedOut",
        Role::ActorErrorNodeNotRunning => "ActorErrorNodeNotRunning",
        Role::ActorErrorRoutingFailed => "ActorErrorRoutingFailed",
        Role::ActorErrorEncodeFailed => "ActorErrorEncodeFailed",
        Role::ActorErrorConnectionDropped => "ActorErrorConnectionDropped",
        Role::ActorErrorPartition => "ActorErrorPartition",
        Role::SendErrorFull => "SendErrorFull",
        Role::SendErrorClosed => "SendErrorClosed",
        Role::SendErrorPartition => "SendErrorPartition",
        Role::SendErrorStaleRef => "SendErrorStaleRef",
        Role::SendErrorLocalShutdown => "SendErrorLocalShutdown",
        Role::SendErrorCancelled => "SendErrorCancelled",
        Role::SendErrorVersionMismatch => "SendErrorVersionMismatch",
        Role::SendErrorBackpressure => "SendErrorBackpressure",
        Role::SendErrorDead => "SendErrorDead",
        Role::SendErrorUnauthorized => "SendErrorUnauthorized",
        Role::SendErrorInvalidMessage => "SendErrorInvalidMessage",
        Role::SendErrorTimedOut => "SendErrorTimedOut",
        Role::SendErrorWriteTimedOut => "SendErrorWriteTimedOut",
        Role::DeliveryAccepted => "DeliveryAccepted",
        Role::DeliveryDiscarded => "DeliveryDiscarded",
        Role::LinkErrorNoContext => "LinkErrorNoContext",
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariantCase {
    pub name: String,
    pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Closure {
    pub id: u32,
    pub entries: Vec<ClosureEntry>,
    pub fields: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClosureEntry {
    pub body: u32,
    pub specialization: Option<u32>,
}

/// One demanded actor: its state seats, its lifecycle callables and the
/// handlers its mailbox dispatches, all as function indexes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Actor {
    pub id: u32,
    pub name: String,
    pub state_fields: Vec<ActorField>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub init: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start: Option<u32>,
    pub stop: Vec<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub crash: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub crash_info: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub crash_kind: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub crash_action: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exit: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub down: Option<u32>,
    pub handlers: Vec<ActorHandler>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mailbox_capacity: Option<u32>,
    pub overflow: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub coalesce: Option<Coalesce>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_heap_bytes: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ActorField {
    pub mutable: bool,
    pub deferred: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ActorHandler {
    pub name: String,
    pub message_id: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub every_ns: Option<i64>,
    pub callable: u32,
    pub params: u32,
    pub streams: bool,
    pub fallible: bool,
    pub result_shape: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Coalesce {
    pub fallback: String,
    pub keys: Vec<CoalesceKey>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CoalesceKey {
    pub message: u32,
    pub param: u32,
}

/// One demanded supervisor: its restart policy and the children it starts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Supervisor {
    pub id: u32,
    pub strategy: String,
    pub max_restarts: u32,
    pub window_secs: u32,
    pub children: Vec<SupervisorChild>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub startup_order: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SupervisorChild {
    pub name: String,
    #[serde(default)]
    pub dependencies: Vec<usize>,
    /// The role this child plays, as `{"actor": id}` or `{"supervisor": id}`.
    pub role: serde_json::Value,
    pub restart: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stop_ns: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stop_config: Option<usize>,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub is_pool: bool,
    pub spawn: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Vtable {
    pub id: u32,
    pub slots: Vec<VtableSlot>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VtableSlot {
    pub slot: u32,
    pub method: String,
    pub callee: u32,
    /// How the erased receiver crosses the dispatch boundary. A `dyn.call`
    /// passes the wrapped value to the callee's `self` parameter under this
    /// decision, which the verifier proves matches the boundary the concrete
    /// type was erased under.
    pub receiver: String,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub receiver_handback: bool,
}

/// The checker's selection for one `(type, capability)` pair.
///
/// `callable` names a user implementation; its absence is the derived
/// structural operation, which the VM performs over its own value
/// representation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueCapabilityPlan {
    pub id: u32,
    pub capability: String,
    pub ty: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub callable: Option<u32>,
    pub components: Vec<ValueComponentCapabilities>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub variants: Option<Vec<Vec<ValueComponentCapabilities>>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueComponentCapabilities {
    pub selected: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ordering: Option<ValueOrderingCapabilities>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueOrderingCapabilities {
    pub less: u32,
    pub less_equal: u32,
    pub greater: u32,
    pub greater_equal: u32,
}

pub use super::bytecode::RuntimeFamilyEntry as RuntimeFamily;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Extern {
    pub id: u32,
    pub symbol: String,
    /// The capability manifest's label for a native-only runtime endpoint,
    /// so a refusal can name what needs native execution. Admission still
    /// keys on `symbol` alone.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub native_capability: Option<String>,
}

/// The manifest label of the native-only capability `symbol` belongs to.
fn native_capability(symbol: &str) -> Option<String> {
    let endpoint = hew_types::DETERMINISTIC_ENDPOINT_REJECTIONS
        .iter()
        .find(|endpoint| endpoint.identity == symbol)?;
    hew_types::WasmUnsupportedFeature::ALL
        .iter()
        .find(|feature| feature.capability_id() == endpoint.capability)
        .map(|feature| feature.label().to_string())
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

/// One semantic storage location and where its storage comes from.
///
/// A `local` place is a cell of its own, created by `alloc_place`. Every other
/// origin resolves through something the body already holds: an `aggregate`
/// place is one field of its base, so it is never allocated and a load of it
/// reads that field in place. Dropping the origin would leave a load with no
/// cell to read.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Place {
    pub id: u32,
    pub origin: String,
    /// `aggregate`: the place or value this field belongs to.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub base: Option<serde_json::Value>,
    /// `aggregate`: the record shape, or absent for a tuple.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub shape: Option<u32>,
    /// `aggregate`, `capture` and `actor_state`: the field index.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub field: Option<u32>,
    /// `capture` and `actor_state`: the value whose field this place is - the
    /// closure environment, or the state the actor's turn holds.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub environment: Option<u32>,
    /// `actor_state`: false only for a seat the actor's `init` body owns,
    /// which holds no value until init's first store.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub initialized: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Value {
    pub value: u32,
    pub own: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub id: u32,
    pub params: Vec<Value>,
    pub ops: Vec<wire::OpV1>,
    pub term: wire::TermV1,
}

/// Project a verified semantic module into a sandbox bytecode package.
///
/// # Errors
///
/// Returns the first construct whose supporting table the sequential package
/// does not carry.
pub fn emit_package(
    module: &SemModule,
    profile: &str,
    hew_version: &str,
    compiler_version: &str,
) -> Result<Package, EmitError> {
    Walker::new(module).emit(profile, hew_version, compiler_version)
}

struct Walker<'m> {
    module: &'m SemModule,
    codecs: Vec<String>,
    strings: BTreeMap<hew_sir::StringLiteralId, u32>,
    bytes: BTreeMap<hew_sir::BytesLiteralId, u32>,
    functions: BTreeMap<CallableId, u32>,
    capabilities: BTreeMap<(hew_types::ResolvedTy, hew_types::ValueCapability), u32>,
    families: Vec<RuntimeFamily>,
    family_index: std::collections::HashMap<hew_types::RuntimeCallFamily, u32>,
    externs: Vec<Extern>,
    extern_index: BTreeMap<String, u32>,
    /// Every SSA value's semantic type in the function being walked.
    ///
    /// The VM's value representation carries no integer width, so an
    /// arithmetic op names the operand type SIR already proved rather than
    /// letting the VM assume one.
    value_types: BTreeMap<hew_sir::ValueId, hew_types::ResolvedTy>,
    suspend_kinds: Vec<String>,
}

impl<'m> Walker<'m> {
    fn new(module: &'m SemModule) -> Self {
        let strings = module
            .string_literals
            .keys()
            .enumerate()
            .map(|(index, id)| (*id, table_index(index)))
            .collect();
        let bytes = module
            .bytes_literals
            .keys()
            .enumerate()
            .map(|(index, id)| (*id, table_index(index)))
            .collect();
        let functions = module
            .functions
            .iter()
            .enumerate()
            .map(|(index, function)| (function.callable, table_index(index)))
            .collect();
        let capabilities = module
            .value_capabilities
            .keys()
            .enumerate()
            .map(|(index, (ty, capability))| ((ty.clone(), *capability), table_index(index)))
            .collect();
        Self {
            module,
            codecs: Vec::new(),
            strings,
            bytes,
            functions,
            capabilities,
            families: Vec::new(),
            family_index: std::collections::HashMap::new(),
            externs: Vec::new(),
            extern_index: BTreeMap::new(),
            value_types: BTreeMap::new(),
            suspend_kinds: Vec::new(),
        }
    }

    /// The semantic type of a value, as the `cast` opcode spells types.
    fn value_ty(&self, operand: &Operand) -> Result<String, EmitError> {
        self.value_types
            .get(&operand.value)
            .map(|ty| ty.user_facing().to_string())
            .ok_or_else(|| {
                EmitError::new(format!("value {} has no semantic type", operand.value.0))
            })
    }

    /// Collect every SSA definition's type before walking a body, so an
    /// operand's type is available wherever it is used.
    fn index_value_types(&mut self, function: &SemFunction) {
        self.value_types.clear();
        for param in &function.params {
            self.value_types.insert(param.value, param.ty.clone());
        }
        for block in &function.blocks {
            for arg in &block.args {
                self.value_types.insert(arg.value, arg.ty.clone());
            }
            for op in &block.ops {
                for result in &op.results {
                    self.value_types.insert(result.id, result.ty.clone());
                }
            }
            match &block.terminator {
                SemTerminator::Call { result, .. }
                | SemTerminator::IndirectCall { result, .. }
                | SemTerminator::DynCall { result, .. }
                | SemTerminator::ValueCall { result, .. }
                | SemTerminator::ActorCall { result, .. }
                | SemTerminator::WireCodec { result, .. }
                | SemTerminator::RtCall { result, .. }
                | SemTerminator::ExternCall { result, .. }
                | SemTerminator::Suspend { result, .. } => {
                    if let CallResult::Value(def) = result {
                        self.value_types.insert(def.id, def.ty.clone());
                    }
                }
                SemTerminator::CheckedBinary { result, .. }
                | SemTerminator::RecoverFault { result, .. } => {
                    self.value_types.insert(result.id, result.ty.clone());
                }
                SemTerminator::SwitchVariant { arms, .. } => {
                    for arm in arms {
                        for field in &arm.fields {
                            self.value_types.insert(field.id, field.ty.clone());
                        }
                    }
                }
                SemTerminator::Safepoint { .. }
                | SemTerminator::EnterDefer { .. }
                | SemTerminator::FinishDefer { .. }
                | SemTerminator::CleanupDispatch { .. }
                | SemTerminator::CheckedRaiseFault { .. }
                | SemTerminator::Return { .. }
                | SemTerminator::Goto(_)
                | SemTerminator::Branch { .. }
                | SemTerminator::Panic { .. }
                | SemTerminator::Trap { .. }
                | SemTerminator::ResumeUnwind { .. }
                | SemTerminator::Unreachable => {}
            }
            if let SemTerminator::Call {
                handback: Some(handback),
                ..
            } = &block.terminator
            {
                self.value_types.insert(handback.id, handback.ty.clone());
            }
        }
    }

    /// The demanded enum descriptor a runtime-produced value is built against.
    ///
    /// The descriptor table owns declaration-order tags, so a shim constructs
    /// the enum the compiler demanded instead of assuming a variant order.
    fn result_shape(&self, result: &CallResult) -> Option<u32> {
        match result {
            CallResult::Value(def) => self
                .module
                .variant_shape_for_type(&def.ty)
                .map(|shape| shape.id.0),
            CallResult::Unit | CallResult::Never => None,
        }
    }

    fn aggregate_result_shape(&self, result: &CallResult) -> Option<u32> {
        match result {
            CallResult::Value(def) => self
                .module
                .aggregate_shape_for_type(&def.ty)
                .map(|shape| shape.id.0),
            CallResult::Unit | CallResult::Never => None,
        }
    }

    /// The checked error envelope of a runtime-produced Result.
    fn error_shape(&self, result: &CallResult) -> Option<u32> {
        let CallResult::Value(def) = result else {
            return None;
        };
        let hew_types::ResolvedTy::Named { args, .. } = &def.ty else {
            return None;
        };
        args.get(1)
            .and_then(|ty| self.module.variant_shape_for_type(ty))
            .map(|shape| shape.id.0)
    }

    fn request_shapes(&self, result: &CallResult) -> Option<wire::RequestShapes> {
        let CallResult::Value(def) = result else {
            return None;
        };
        let hew_types::ResolvedTy::Named { args, .. } = &def.ty else {
            return None;
        };
        let error = args
            .get(1)
            .and_then(|ty| self.module.variant_shape_for_type(ty));
        let failure_ty = error
            .and_then(|shape| {
                shape
                    .runtime_tag(RuntimeVariantRole::ActorErrorRejected)
                    .and_then(|tag| shape.variants.get(tag as usize))
            })
            .and_then(|variant| variant.fields.first())
            .map(|field| &field.ty);
        let failure = failure_ty.and_then(|ty| self.module.aggregate_shape_for_type(ty));
        let message = failure
            .and_then(|shape| shape.fields.get(1))
            .and_then(|field| self.module.aggregate_shape_for_type(&field.ty));
        let request = message
            .and_then(|shape| shape.fields.get(2))
            .and_then(|field| self.module.aggregate_shape_for_type(&field.ty));
        Some(wire::RequestShapes {
            failure: failure.map(|shape| shape.id.0),
            reason: failure
                .and_then(|shape| shape.fields.first())
                .and_then(|field| self.module.variant_shape_for_type(&field.ty))
                .map(|shape| shape.id.0),
            message: message.map(|shape| shape.id.0),
            request: request.map(|shape| shape.id.0),
        })
    }

    fn submission_shapes(&self, result: &CallResult) -> Option<wire::SubmissionShapes> {
        let CallResult::Value(def) = result else {
            return None;
        };
        let hew_types::ResolvedTy::Named { args, .. } = &def.ty else {
            return None;
        };
        let success = args
            .first()
            .and_then(|ty| self.module.variant_shape_for_type(ty));
        let error = args
            .get(1)
            .and_then(|ty| self.module.variant_shape_for_type(ty));
        let failure = error
            .and_then(|shape| shape.variants.first())
            .and_then(|variant| variant.fields.first())
            .and_then(|field| self.module.aggregate_shape_for_type(&field.ty));
        Some(wire::SubmissionShapes {
            success: success.map(|shape| shape.id.0),
            failure: failure.map(|shape| shape.id.0),
            reason: failure
                .and_then(|shape| shape.fields.first())
                .and_then(|field| self.module.variant_shape_for_type(&field.ty))
                .map(|shape| shape.id.0),
        })
    }

    fn suspend_kind_id(&mut self, name: &str) {
        if !self.suspend_kinds.iter().any(|kind| kind == name) {
            self.suspend_kinds.push(name.to_string());
        }
    }

    fn value_capabilities(&self) -> Result<Vec<ValueCapabilityPlan>, EmitError> {
        self.module
            .value_capabilities
            .iter()
            .enumerate()
            .map(|(index, ((ty, capability), plan))| {
                Ok(ValueCapabilityPlan {
                    id: table_index(index),
                    capability: capability_name(*capability).to_string(),
                    ty: ty.user_facing().to_string(),
                    components: if plan.callable.is_none() {
                        hew_sir::derived_capability_components(
                            ty,
                            &self.module.aggregate_shapes,
                            &self.module.variant_shapes,
                        )
                        .map_err(EmitError::new)?
                        .iter()
                        .map(|component| self.component_capabilities(component, *capability))
                        .collect::<Result<Vec<_>, _>>()?
                    } else {
                        Vec::new()
                    },
                    variants: if plan.callable.is_none() {
                        self.module
                            .variant_shape_for_type(ty)
                            .map(|shape| {
                                shape
                                    .variants
                                    .iter()
                                    .map(|variant| {
                                        variant
                                            .fields
                                            .iter()
                                            .map(|field| {
                                                self.component_capabilities(&field.ty, *capability)
                                            })
                                            .collect::<Result<Vec<_>, _>>()
                                    })
                                    .collect::<Result<Vec<_>, _>>()
                            })
                            .transpose()?
                    } else {
                        None
                    },
                    callable: plan
                        .callable
                        .map(|callable| self.function_id(callable))
                        .transpose()?,
                })
            })
            .collect::<Result<Vec<_>, EmitError>>()
    }

    fn resources(&self) -> Result<Vec<serde_json::Value>, EmitError> {
        self.module.resources.iter().map(|(ty, release)| {
                use hew_sir::ResourceRelease;
                Ok(match release {
                    ResourceRelease::RecordClose { close, .. } => {
                        let shape = self.module.aggregate_shape_for_type(ty).ok_or_else(|| {
                            EmitError::new(format!(
                                "record resource `{}` has no exact aggregate shape",
                                ty.user_facing()
                            ))
                        })?;
                        serde_json::json!({"kind": "record", "shape": shape.id.0, "ty": ty.user_facing().to_string(), "close": self.function_id(*close)?})
                    },
                    ResourceRelease::OpaqueClose { close, .. } => serde_json::json!({"kind": "opaque", "ty": ty.user_facing().to_string(), "close": self.function_id(*close)?}),
                    ResourceRelease::Nominal { release, .. } => serde_json::json!({"kind": "nominal", "ty": ty.user_facing().to_string(), "release": release.symbol}),
                    ResourceRelease::Task => serde_json::json!({"kind": "task"}),
                    ResourceRelease::ActorCall => serde_json::json!({"kind": "actor_call"}),
                    ResourceRelease::ActorRequest => serde_json::json!({"kind": "actor_request"}),
                    ResourceRelease::Generator => serde_json::json!({"kind": "generator"}),
                    ResourceRelease::Stream => serde_json::json!({"kind": "stream"}),
                    ResourceRelease::Sink => serde_json::json!({"kind": "sink"}),
                })
            }).collect::<Result<Vec<_>, EmitError>>()
    }

    fn emit(
        mut self,
        profile: &str,
        hew_version: &str,
        compiler_version: &str,
    ) -> Result<Package, EmitError> {
        let functions = self
            .module
            .functions
            .iter()
            .enumerate()
            .map(|(index, function)| self.function(table_index(index), function))
            .collect::<Result<Vec<_>, _>>()?;

        let entry = match (self.module.entry_callable, &self.module.entry_exit_plan) {
            (Some(callable), Some(plan)) => Some(Entry {
                properties: Vec::new(),
                function: self.function_id(callable)?,
                // SIR consumes a `Result` action in the entry adapter and
                // publishes the integer status that body returns, so it reaches
                // the package as an ordinary integer exit.
                exit: match plan.action {
                    hew_types::EntryExitAction::Unit => "unit",
                    hew_types::EntryExitAction::Integer(_)
                    | hew_types::EntryExitAction::Result { .. } => "status",
                }
                .to_string(),
            }),
            _ => None,
        };

        Ok(Package {
            codecs: self.codecs.clone(),
            structural_render: self.structural_render()?,
            resources: self.resources()?,
            schema_version: SCHEMA_VERSION.to_string(),
            hew_version: hew_version.to_string(),
            compiler_version: compiler_version.to_string(),
            profile: profile.to_string(),
            entry,
            strings: self.module.string_literals.values().cloned().collect(),
            bytes: self.module.bytes_literals.values().cloned().collect(),
            regex_patterns: self.module.regex_patterns.clone(),
            aggregates: self.aggregate_shapes(),
            variants: self.variant_shapes(),
            actors: self.actors()?,
            supervisors: self.supervisors()?,
            closures: self
                .module
                .closures
                .iter()
                .map(|closure| {
                    Ok(Closure {
                        id: closure.id.0,
                        entries: closure
                            .entries
                            .iter()
                            .map(|entry| {
                                Ok(ClosureEntry {
                                    body: self.function_id(entry.body)?,
                                    specialization: entry.specialization.map(|id| id.0),
                                })
                            })
                            .collect::<Result<Vec<_>, EmitError>>()?,
                        fields: table_index(closure.fields.len()),
                    })
                })
                .collect::<Result<Vec<_>, EmitError>>()?,
            vtables: self
                .module
                .vtables
                .iter()
                .map(|vtable| {
                    Ok(Vtable {
                        id: vtable.id.0,
                        slots: vtable
                            .slots
                            .iter()
                            .map(|slot| {
                                Ok(VtableSlot {
                                    slot: slot.slot,
                                    method: self.module.defs.name(slot.method).to_string(),
                                    callee: self.function_id(slot.callee)?,
                                    receiver: passing_name(slot.receiver).to_string(),
                                    receiver_handback: self
                                        .module
                                        .callable(slot.callee)
                                        .is_some_and(|callee| {
                                            callee.signature.hands_back_receiver()
                                        }),
                                })
                            })
                            .collect::<Result<Vec<_>, EmitError>>()?,
                    })
                })
                .collect::<Result<Vec<_>, EmitError>>()?,
            value_capabilities: self.value_capabilities()?,
            runtime_families: self.families,
            externs: self.externs,
            suspend_kinds: self.suspend_kinds,
            functions,
        })
    }

    /// Project the checked structural selections, retaining alias identities
    /// and variant forms rather than inferring them from runtime values.
    fn structural_id(&self, ty: &hew_sir::StructuralType) -> Result<u32, EmitError> {
        self.module
            .structural_display
            .keys()
            .position(|key| key == ty)
            .map(table_index)
            .ok_or_else(|| EmitError::new("structural type lacks its checked selection"))
    }

    fn structural_render(&self) -> Result<Vec<serde_json::Value>, EmitError> {
        use hew_types::{BuiltinType, ResolvedTy};
        self.module.structural_display.iter().map(|(key, selected)| {
            if let Some(callable) = selected.display {
                return Ok(serde_json::json!({ "kind": "display", "callee": self.function_id(callable)? }));
            }
            let members = selected.members.iter().map(|member| self.structural_id(member)).collect::<Result<Vec<_>, _>>()?;
            let scalar = |kind| Ok(serde_json::json!({"kind": kind}));
            match &key.value {
                ResolvedTy::Unit => scalar("unit"),
                ResolvedTy::I8 | ResolvedTy::I16 | ResolvedTy::I32 | ResolvedTy::I64 | ResolvedTy::Isize |
                ResolvedTy::U8 | ResolvedTy::U16 | ResolvedTy::U32 | ResolvedTy::U64 | ResolvedTy::Usize |
                ResolvedTy::F32 | ResolvedTy::F64 | ResolvedTy::Bool | ResolvedTy::Char | ResolvedTy::String => scalar("scalar"),
                ResolvedTy::Tuple(_) => Ok(serde_json::json!({"kind": "tuple", "members": members})),
                ResolvedTy::Named { head, is_opaque, .. } => { let name = head.registry_key(); let builtin = head.builtin();
                    if builtin == Some(BuiltinType::Vec) { return Ok(serde_json::json!({"kind": "vector", "members": members})); }
                    if builtin == Some(BuiltinType::HashMap) { return Ok(serde_json::json!({"kind": "map", "members": members})); }
                    if *is_opaque { return Ok(serde_json::json!({"kind": "identity", "name": name})); }
                    if let Some(shape) = self.module.variant_shape_for_type(&key.value) {
                        let mut offset = 0;
                        let cases: Vec<_> = shape.variants.iter().map(|variant| {
                            let kind = match variant.kind { hew_sir::SemVariantKind::Unit => "unit", hew_sir::SemVariantKind::Tuple => "tuple", hew_sir::SemVariantKind::Struct => "struct" };
                            let fields: Vec<_> = variant.fields.iter().map(|field| {
                                let recipe = members[offset]; offset += 1;
                                serde_json::json!({"name": field.name, "recipe": recipe})
                            }).collect();
                            serde_json::json!({"name": variant.name, "kind": kind, "fields": fields})
                        }).collect();
                        return Ok(serde_json::json!({"kind": "enum", "cases": cases}));
                    }
                    if let Some(shape) = self.module.aggregate_shape_for_type(&key.value) {
                        let fields: Vec<_> = shape.fields.iter().zip(members).map(|(field, recipe)| serde_json::json!({"name": field.name, "recipe": recipe})).collect();
                        return Ok(serde_json::json!({"kind": "record", "name": name, "fields": fields}));
                    }
                    Err(EmitError::new("structural named type has no shape"))
                }
                _ => Err(EmitError::new("structural type has no sandbox representation")),
            }
        }).collect()
    }

    /// Project each demanded actor. Every callable becomes a function index,
    /// so the VM reaches a handler the way it reaches any other body.
    fn actors(&self) -> Result<Vec<Actor>, EmitError> {
        self.module
            .actors
            .iter()
            .map(|actor| {
                let hew_types::ResolvedTy::Named { head, .. } = &actor.handle_ty else {
                    return Err(EmitError::new("actor diagnostic lacks its declared type"));
                };
                Ok(Actor {
                    id: actor.id.0,
                    name: head.registry_key().to_owned(),
                    state_fields: actor
                        .fields
                        .iter()
                        .map(|field| ActorField {
                            mutable: field.mutable,
                            deferred: field.deferred,
                        })
                        .collect(),
                    init: actor.init.map(|id| self.function_id(id)).transpose()?,
                    start: actor.start.map(|id| self.function_id(id)).transpose()?,
                    stop: actor
                        .stop
                        .iter()
                        .map(|id| self.function_id(*id))
                        .collect::<Result<Vec<_>, _>>()?,
                    crash: actor.crash.map(|id| self.function_id(id)).transpose()?,
                    crash_info: actor
                        .crash
                        .and_then(|id| self.module.callable(id))
                        .and_then(|callable| callable.signature.params.get(1))
                        .and_then(|parameter| self.module.aggregate_shape_for_type(&parameter.ty))
                        .map(|shape| shape.id.0),
                    crash_kind: actor
                        .crash
                        .and_then(|id| self.module.callable(id))
                        .and_then(|callable| callable.signature.params.get(1))
                        .and_then(|parameter| self.module.aggregate_shape_for_type(&parameter.ty))
                        .and_then(|shape| shape.fields.first())
                        .and_then(|field| self.module.variant_shape_for_type(&field.ty))
                        .map(|shape| shape.id.0),
                    crash_action: actor
                        .crash
                        .and_then(|id| self.module.callable(id))
                        .and_then(|callable| {
                            self.module
                                .variant_shape_for_type(&callable.signature.return_ty)
                        })
                        .map(|shape| shape.id.0),
                    exit: actor.exit.map(|id| self.function_id(id)).transpose()?,
                    down: actor.down.map(|id| self.function_id(id)).transpose()?,
                    handlers: actor
                        .handlers
                        .iter()
                        .map(|handler| {
                            Ok(ActorHandler {
                                name: handler.name.clone(),
                                message_id: handler.message_id,
                                every_ns: handler.every_ns,
                                callable: self.function_id(handler.callable)?,
                                params: table_index(handler.params.len()),
                                streams: handler.stream.is_some(),
                                fallible: handler.declared_failure,
                                result_shape: self
                                    .module
                                    .variant_shape_for_type(&handler.return_ty)
                                    .map(|shape| shape.id.0),
                            })
                        })
                        .collect::<Result<Vec<_>, EmitError>>()?,
                    mailbox_capacity: actor.mailbox_capacity,
                    overflow: overflow_name(actor.overflow).to_string(),
                    coalesce: actor.coalesce.as_ref().map(|coalesce| Coalesce {
                        fallback: coalesce_fallback_name(coalesce.fallback).to_string(),
                        keys: coalesce
                            .keys
                            .iter()
                            .map(|key| CoalesceKey {
                                message: key.message,
                                param: key.param,
                            })
                            .collect(),
                    }),
                    max_heap_bytes: actor.max_heap_bytes,
                })
            })
            .collect()
    }

    fn supervisors(&self) -> Result<Vec<Supervisor>, EmitError> {
        self.module
            .supervisors
            .iter()
            .map(|supervisor| {
                let hew_sir::SemSupervisor {
                    id,
                    declaration: _,
                    handle_ty: _,
                    interceptor_chain: _,
                    config: _,
                    strategy,
                    max_restarts,
                    window_secs,
                    children,
                    startup_order,
                } = supervisor;
                Ok(Supervisor {
                    id: id.0,
                    strategy: strategy_name(*strategy).to_string(),
                    max_restarts: *max_restarts,
                    window_secs: *window_secs,
                    startup_order: startup_order.clone(),
                    children: children
                        .iter()
                        .map(|child| {
                            let hew_sir::SemSupervisorChild {
                                name,
                                dependencies,
                                role,
                                restart,
                                stop_deadline,
                                pool_count,
                                spawn,
                            } = child;
                            Ok(SupervisorChild {
                                name: name.clone(),
                                dependencies: dependencies.clone(),
                                role: match role {
                                    hew_sir::SemSupervisedRole::Actor(actor) => {
                                        serde_json::json!({ "actor": actor.0 })
                                    }
                                    hew_sir::SemSupervisedRole::Supervisor(supervisor) => {
                                        serde_json::json!({ "supervisor": supervisor.0 })
                                    }
                                },
                                restart: restart_name(*restart).to_string(),
                                stop_ns: match *stop_deadline {
                                    hew_sir::SemStopDeadline::Literal(ns) if ns >= 0 => Some(ns),
                                    _ => None,
                                },
                                stop_config: match *stop_deadline {
                                    hew_sir::SemStopDeadline::Config(index) => Some(index),
                                    _ => None,
                                },
                                is_pool: pool_count.is_some(),
                                spawn: self.function_id(*spawn)?,
                            })
                        })
                        .collect::<Result<Vec<_>, EmitError>>()?,
                })
            })
            .collect()
    }

    fn aggregate_shapes(&self) -> Vec<AggregateShape> {
        self.module
            .aggregate_shapes
            .iter()
            .map(|shape| AggregateShape {
                id: shape.id.0,
                name: shape.aggregate_ty.user_facing().to_string(),
                builtin: shape
                    .aggregate_ty
                    .is_builtin(hew_types::BuiltinType::Timestamp)
                    .then_some(AggregateBuiltin::Timestamp),
                fields: shape
                    .fields
                    .iter()
                    .map(|field| field.name.clone())
                    .collect(),
            })
            .collect()
    }

    fn variant_shapes(&self) -> Vec<VariantShape> {
        self.module
            .variant_shapes
            .iter()
            .map(|shape| VariantShape {
                id: shape.id.0,
                name: shape.enum_ty.user_facing().to_string(),
                runtime_tags: shape
                    .runtime_tags
                    .iter()
                    .map(|(role, tag)| (runtime_role_name(*role).to_string(), *tag))
                    .collect(),
                cases: shape
                    .variants
                    .iter()
                    .map(|variant| VariantCase {
                        name: variant.name.clone(),
                        fields: variant
                            .fields
                            .iter()
                            .map(|field| field.name.clone())
                            .collect(),
                    })
                    .collect(),
            })
            .collect()
    }

    fn function_id(&self, callable: CallableId) -> Result<u32, EmitError> {
        self.functions.get(&callable).copied().ok_or_else(|| {
            EmitError::new(format!(
                "callable {} has no semantic body in this module",
                callable.0
            ))
        })
    }

    fn string_id(&self, id: hew_sir::StringLiteralId) -> Result<u32, EmitError> {
        self.strings
            .get(&id)
            .copied()
            .ok_or_else(|| EmitError::new("string literal is absent from the module pool"))
    }

    fn bytes_id(&self, id: hew_sir::BytesLiteralId) -> Result<u32, EmitError> {
        self.bytes
            .get(&id)
            .copied()
            .ok_or_else(|| EmitError::new("bytes literal is absent from the module pool"))
    }

    fn capability_id(
        &self,
        ty: &hew_types::ResolvedTy,
        capability: hew_types::ValueCapability,
    ) -> Result<u32, EmitError> {
        self.capabilities
            .get(&(ty.clone(), capability))
            .copied()
            .ok_or_else(|| {
                EmitError::new(format!(
                    "`{}` has no selected {capability:?} operation",
                    ty.user_facing()
                ))
            })
    }

    fn component_capabilities(
        &self,
        ty: &hew_types::ResolvedTy,
        capability: hew_types::ValueCapability,
    ) -> Result<ValueComponentCapabilities, EmitError> {
        use hew_types::ValueCapability as C;
        Ok(ValueComponentCapabilities {
            selected: self.capability_id(ty, capability)?,
            ordering: capability
                .is_ordering()
                .then(|| {
                    Ok(ValueOrderingCapabilities {
                        less: self.capability_id(ty, C::Less)?,
                        less_equal: self.capability_id(ty, C::LessEqual)?,
                        greater: self.capability_id(ty, C::Greater)?,
                        greater_equal: self.capability_id(ty, C::GreaterEqual)?,
                    })
                })
                .transpose()?,
        })
    }

    fn collection_callbacks(
        &self,
        family: hew_types::RuntimeCallFamily,
        args: &[BoundaryOperand],
    ) -> Result<Vec<u32>, EmitError> {
        let demands = family.value_callback_capabilities();
        if demands.is_empty() {
            return Ok(Vec::new());
        }
        let receiver = args
            .first()
            .and_then(|arg| self.value_types.get(&arg.operand.value))
            .ok_or_else(|| EmitError::new("collection receiver has no checked type"))?;
        let (_, arguments) = hew_types::runtime_call::collection_type_arguments(receiver)
            .ok_or_else(|| EmitError::new("collection operation has no type arguments"))?;
        demands
            .iter()
            .map(|capability| self.capability_id(&arguments[0], *capability))
            .collect()
    }

    fn result_member_shapes(&self, result: &CallResult) -> Vec<Option<u32>> {
        let CallResult::Value(def) = result else {
            return Vec::new();
        };
        let hew_types::ResolvedTy::Tuple(fields) = &def.ty else {
            return Vec::new();
        };
        fields
            .iter()
            .map(|ty| {
                self.module
                    .variant_shape_for_type(ty)
                    .map(|shape| shape.id.0)
            })
            .collect()
    }

    /// Intern the compiler-owned runtime identity without reconstructing it from JSON.
    fn family_id(&mut self, family: hew_types::RuntimeCallFamily) -> u32 {
        if let Some(id) = self.family_index.get(&family) {
            return *id;
        }
        let id = table_index(self.families.len());
        self.families.push(RuntimeFamily {
            id,
            identity: family,
            null_detail: false,
        });
        self.family_index.insert(family, id);
        id
    }

    fn extern_id(&mut self, symbol: &str) -> u32 {
        if let Some(id) = self.extern_index.get(symbol) {
            return *id;
        }
        let id = table_index(self.externs.len());
        self.externs.push(Extern {
            id,
            symbol: symbol.to_string(),
            native_capability: native_capability(symbol),
        });
        self.extern_index.insert(symbol.to_string(), id);
        id
    }

    fn function(&mut self, id: u32, function: &SemFunction) -> Result<Function, EmitError> {
        self.index_value_types(function);
        Ok(Function {
            actor_construction: matches!(
                self.module.callables[function.callable.0 as usize].instance,
                hew_sir::CallableInstance::ActorConstruction { .. }
            ),
            id,
            name: function.name.clone(),
            params: function.params.iter().map(block_arg).collect(),
            entry: function.entry.0,
            places: function.places.iter().map(place_decl).collect(),
            blocks: function
                .blocks
                .iter()
                .map(|block| self.block(function.callable, block))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn block(
        &mut self,
        callable: hew_sir::CallableId,
        block: &SemBlock,
    ) -> Result<Block, EmitError> {
        let mut ops = block
            .ops
            .iter()
            .map(|op| self.op(op))
            .collect::<Result<Vec<_>, _>>()?;
        if let Some(counter) = self.module.coverage.get(&(callable, block.id)) {
            ops.insert(
                0,
                wire::OpV1 {
                    kind: wire::OpKind::CoverageHit {
                        counter: counter.id,
                    },
                    own: None,
                    span: None,
                },
            );
        }
        Ok(Block {
            id: block.id.0,
            params: block.args.iter().map(block_arg).collect(),
            ops,
            term: self.terminator(&block.terminator, &block.terminator_provenance)?,
        })
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one arm per SemOpKind variant with no wildcard is the point: a new SIR operation must fail to compile here"
    )]
    fn op(&mut self, op: &SemOp) -> Result<wire::OpV1, EmitError> {
        // An operation that defines several values carries them all in
        // `results`; every other operation has the one `dst`.
        let dst = match op.kind {
            SemOpKind::Destructure { .. }
            | SemOpKind::VariantDestructure { .. }
            | SemOpKind::StreamPipe { .. } => None,
            _ => op.results.first().map(|result| result.id.0),
        };
        let span = self.span(&op.provenance);
        let output = || dst.ok_or_else(|| EmitError::new("verified operation has no destination"));
        let result_type = || {
            op.results
                .first()
                .map(|result| result.ty.user_facing().to_string())
                .ok_or_else(|| EmitError::new("verified constant has no type"))
        };
        let kind = match &op.kind {
            SemOpKind::ConstInteger(value) => wire::OpKind::ConstInt {
                dst: output()?,
                value: value.to_string(),
                ty: result_type()?,
            },
            SemOpKind::ConstBool(value) => wire::OpKind::ConstBool {
                dst: output()?,
                value: *value,
            },
            SemOpKind::ConstFloat(value) => wire::OpKind::ConstFloat {
                dst: output()?,
                ty: result_type()?,
                value: Some(if value.is_finite() { *value } else { 0.0 }),
                nonfinite: if value.is_nan() {
                    Some(wire::Nonfinite::Nan)
                } else if value.is_infinite() {
                    Some(if value.is_sign_negative() {
                        wire::Nonfinite::InfNegative
                    } else {
                        wire::Nonfinite::Inf
                    })
                } else {
                    None
                },
            },
            SemOpKind::ConstChar(value) => wire::OpKind::ConstChar {
                dst: output()?,
                value: value.to_string(),
            },
            SemOpKind::ConstUnit => wire::OpKind::ConstUnit { dst: output()? },
            SemOpKind::ConstDuration(nanos) => wire::OpKind::ConstDuration {
                dst: output()?,
                nanos: nanos.to_string(),
            },
            SemOpKind::ConstStr(id) => wire::OpKind::ConstStr {
                dst: output()?,
                r#str: self.string_id(*id)?,
            },
            SemOpKind::ConstBytes(id) => wire::OpKind::ConstBytes {
                dst: output()?,
                bytes: self.bytes_id(*id)?,
            },

            SemOpKind::CopyValue { source } => wire::OpKind::CopyValue {
                dst: output()?,
                source: operand(source),
            },
            SemOpKind::Move { source } | SemOpKind::GeneratorCoerce { source } => {
                wire::OpKind::Move {
                    dst: output()?,
                    source: operand(source),
                }
            }
            SemOpKind::Fork { source } => wire::OpKind::Fork {
                dst: output()?,
                source: operand(source),
            },
            SemOpKind::DestroyValue { value } => wire::OpKind::DestroyValue {
                dst: Some(dst),
                value: operand(value),
            },
            SemOpKind::BeginBorrow { owner } => wire::OpKind::BeginBorrow {
                dst: output()?,
                owner: operand(owner),
            },
            SemOpKind::EndBorrow { borrow } => wire::OpKind::EndBorrow {
                dst: Some(dst),
                borrow: operand(borrow),
            },
            SemOpKind::FinishLinearReceiver => {
                wire::OpKind::FinishLinearReceiver { dst: Some(dst) }
            }

            SemOpKind::AllocPlace { place } => wire::OpKind::AllocPlace {
                dst: Some(dst),
                place: place.0,
            },
            SemOpKind::StoreInit { place, value } => wire::OpKind::StoreInit {
                dst: Some(dst),
                place: place.0,
                value: operand(value),
            },
            SemOpKind::StoreAssign { place, value } => wire::OpKind::StoreAssign {
                dst: Some(dst),
                place: place.0,
                value: operand(value),
            },
            SemOpKind::LoadCopy { place } => wire::OpKind::LoadCopy {
                dst: output()?,
                place: place.0,
            },
            SemOpKind::LoadTake { place } => wire::OpKind::LoadTake {
                dst: output()?,
                place: place.0,
            },
            SemOpKind::LoadBorrow { place } => wire::OpKind::LoadBorrow {
                dst: output()?,
                place: place.0,
            },
            SemOpKind::EndLifetime { place } => wire::OpKind::EndLifetime {
                dst: Some(dst),
                place: place.0,
            },

            SemOpKind::TupleMake { elements } => wire::OpKind::TupleMake {
                dst: output()?,
                elements: operands(elements),
            },
            SemOpKind::TupleGet { tuple, index } => wire::OpKind::TupleGet {
                dst: output()?,
                tuple: operand(tuple),
                index: *index,
            },
            SemOpKind::AggregateMake { shape, fields } => wire::OpKind::AggregateMake {
                dst: output()?,
                shape: shape_ref(*shape),
                fields: operands(fields),
            },
            SemOpKind::AggregateProjectCopy {
                shape,
                aggregate,
                field,
            } => wire::OpKind::AggregateProjectCopy {
                dst: output()?,
                shape: shape_ref(*shape),
                aggregate: operand(aggregate),
                field: *field,
            },
            SemOpKind::AggregateProjectBorrow {
                shape,
                aggregate,
                field,
            } => wire::OpKind::AggregateProjectBorrow {
                dst: output()?,
                shape: shape_ref(*shape),
                aggregate: operand(aggregate),
                field: *field,
            },
            SemOpKind::Destructure { shape, aggregate } => wire::OpKind::Destructure {
                dst: Some(dst),
                shape: shape_ref(*shape),
                aggregate: operand(aggregate),
                results: op
                    .results
                    .iter()
                    .map(|result| result.id.0)
                    .collect::<Vec<_>>(),
            },
            SemOpKind::ArrayMake { fields } => wire::OpKind::ArrayMake {
                dst: output()?,
                elements: operands(fields),
            },
            SemOpKind::ArrayRepeat { value } => wire::OpKind::ArrayRepeat {
                dst: output()?,
                value: operand(value),
                count: None,
            },

            SemOpKind::VariantMake {
                shape,
                variant,
                fields,
            } => wire::OpKind::VariantMake {
                dst: output()?,
                shape: shape.0,
                variant: *variant,
                fields: operands(fields),
            },
            SemOpKind::VariantIs {
                shape,
                variant,
                source,
            } => wire::OpKind::VariantIs {
                dst: output()?,
                shape: shape.0,
                variant: *variant,
                source: operand(source),
            },
            SemOpKind::VariantProjectCopy {
                shape,
                variant,
                source,
                field,
            } => wire::OpKind::VariantProjectCopy {
                dst: output()?,
                shape: shape.0,
                variant: *variant,
                source: operand(source),
                field: *field,
            },
            SemOpKind::VariantProjectBorrow {
                shape,
                variant,
                source,
                field,
            } => wire::OpKind::VariantProjectBorrow {
                dst: output()?,
                shape: shape.0,
                variant: *variant,
                source: operand(source),
                field: *field,
            },
            SemOpKind::VariantDestructure {
                shape,
                variant,
                source,
            } => wire::OpKind::VariantDestructure {
                dst: Some(dst),
                shape: shape.0,
                variant: *variant,
                source: operand(source),
                results: op
                    .results
                    .iter()
                    .map(|result| result.id.0)
                    .collect::<Vec<_>>(),
            },

            SemOpKind::Unary { op: unary, value } => wire::OpKind::Unary {
                dst: output()?,
                unary_op: format!("{unary:?}"),
                value: operand(value),
                ty: self.value_ty(value)?,
            },
            SemOpKind::Binary {
                op: binary,
                lhs,
                rhs,
            } => wire::OpKind::Binary {
                dst: output()?,
                binary_op: format!("{binary:?}"),
                lhs: operand(lhs),
                rhs: operand(rhs),
                ty: self.value_ty(lhs)?,
            },
            SemOpKind::Cast { value, to } => wire::OpKind::Cast {
                dst: output()?,
                value: operand(value),
                from: self.value_ty(value)?,
                to: to.user_facing().to_string(),
            },
            SemOpKind::FunctionMake { callable } => wire::OpKind::FunctionMake {
                dst: output()?,
                callable: self.function_id(*callable)?,
            },
            SemOpKind::ClosureMake { closure, fields } => wire::OpKind::ClosureMake {
                dst: output()?,
                closure: closure.0,
                fields: operands(fields),
            },
            SemOpKind::CallableCoerce {
                source,
                specialization,
            } => wire::OpKind::CallableCoerce {
                dst: output()?,
                source: operand(source),
                specialization: specialization.map(|id| id.0),
            },
            SemOpKind::DynMake { vtable, value } => wire::OpKind::DynMake {
                dst: output()?,
                vtable: vtable.0,
                value: operand(value),
            },

            SemOpKind::RegisterDefer {
                defer,
                scope,
                dependencies,
            } => wire::OpKind::RegisterDefer {
                dst: Some(dst),
                defer: defer.0,
                scope: scope.0,
                dependencies: dependencies.iter().map(|place| place.0).collect::<Vec<_>>(),
            },

            SemOpKind::GeneratorMake { closure, callable } => wire::OpKind::GeneratorMake {
                dst: output()?,
                closure: closure.0,
                callable: operand(callable),
            },
            SemOpKind::StreamPipe { capacity } => wire::OpKind::StreamPipe {
                dst: Some(dst),
                capacity: *capacity,
                results: op.results.iter().map(value_def).collect::<Vec<_>>(),
            },
            SemOpKind::TaskScopeEnter {
                scope,
                parent,
                duration,
            } => wire::OpKind::TaskScopeEnter {
                dst: Some(dst),
                scope: scope.0,
                parent: parent.map(|parent| parent.0),
                duration: duration.as_ref().map(operand),
            },
            SemOpKind::TaskScopeClose { scope } => wire::OpKind::TaskScopeClose {
                dst: Some(dst),
                scope: scope.0,
            },
            SemOpKind::TaskRace { scope, members } => wire::OpKind::TaskRace {
                dst: output()?,
                scope: scope.0,
                members: members.iter().map(operand).collect::<Vec<_>>(),
            },
            SemOpKind::TaskSpawn { scope, callable } => wire::OpKind::TaskSpawn {
                dst: output()?,
                scope: scope.0,
                callable: operand(callable),
            },
            SemOpKind::TaskInvoke {
                scope,
                callee,
                args,
                ..
            } => wire::OpKind::TaskInvoke {
                dst: output()?,
                scope: scope.0,
                callee: match callee {
                    hew_sir::InvocationCallee::Direct(target) => {
                        wire::InvocationCallee::Variant0(wire::DirectInvocation {
                            direct: target.0,
                        })
                    }
                    hew_sir::InvocationCallee::Mutable {
                        target,
                        receiver,
                        update,
                    } => wire::InvocationCallee::Variant1(wire::MutableInvocation {
                        mutable: target.0,
                        receiver: boundary(receiver),
                        staged: *update == hew_types::ReceiverUpdate::Staged,
                    }),
                    hew_sir::InvocationCallee::Dyn {
                        receiver,
                        slot,
                        method,
                    } => wire::InvocationCallee::Variant2(wire::DynInvocation {
                        r#dyn: *slot,
                        method: self.module.defs.name(*method).to_string(),
                        receiver: boundary(receiver),
                    }),
                    hew_sir::InvocationCallee::Indirect {
                        receiver,
                        specialization,
                    } => wire::InvocationCallee::Variant3(wire::IndirectInvocation {
                        receiver: boundary(receiver),
                        specialization: specialization.map(|id| id.0),
                    }),
                },
                args: args.iter().map(boundary).collect::<Vec<_>>(),
            },
            SemOpKind::ActorIngressAdapter(adapter) => wire::OpKind::ActorIngressAdapter {
                dst: Some(dst),
                actor: adapter.actor.0,
                message: adapter.message,
            },
        };
        Ok(wire::OpV1 {
            kind,
            own: op
                .results
                .first()
                .filter(|_| dst.is_some())
                .map(|result| own_kind(result.own)),
            span: Some(span),
        })
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one arm per SemTerminator variant with no wildcard is the point: a new SIR terminator must fail to compile here"
    )]
    fn terminator(
        &mut self,
        terminator: &SemTerminator,
        provenance: &Provenance,
    ) -> Result<wire::TermV1, EmitError> {
        let span = self.span(provenance);
        let kind = match terminator {
            SemTerminator::Return { value } => wire::TermKind::Return {
                value: value.as_ref().map(boundary),
            },
            SemTerminator::Goto(edge) => wire::TermKind::Goto {
                edge: encode_edge(edge),
            },
            SemTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => wire::TermKind::Branch {
                condition: operand(condition),
                then: encode_edge(then_target),
                r#else: encode_edge(else_target),
            },
            SemTerminator::SwitchVariant {
                shape,
                scrutinee,
                arms,
                ..
            } => wire::TermKind::SwitchVariant {
                shape: shape.0,
                scrutinee: operand(scrutinee),
                arms: arms
                    .iter()
                    .map(|arm| wire::SwitchArm {
                        variant: arm.variant,
                        fields: arm.fields.iter().map(value_def).collect(),
                        edge: encode_edge(&arm.target),
                    })
                    .collect::<Vec<_>>(),
            },
            SemTerminator::CheckedBinary {
                op,
                lhs,
                rhs,
                result,
                normal,
                failures,
                ..
            } if self
                .value_types
                .get(&lhs.value)
                .is_some_and(|ty| ty.is_builtin(hew_types::BuiltinType::Timestamp))
                || self
                    .value_types
                    .get(&rhs.value)
                    .is_some_and(|ty| ty.is_builtin(hew_types::BuiltinType::Timestamp)) =>
            {
                let shape = if result.ty.is_builtin(hew_types::BuiltinType::Timestamp) {
                    Some(
                        self.module
                            .aggregate_shape_for_type(&result.ty)
                            .ok_or_else(|| {
                                EmitError::new("timestamp result lacks its aggregate shape")
                            })?
                            .id
                            .0,
                    )
                } else {
                    None
                };
                wire::TermKind::TimestampBinary {
                    binary_op: match op {
                        hew_parser::ast::BinaryOp::Add => wire::TimestampBinaryOp::Add,
                        hew_parser::ast::BinaryOp::Subtract => wire::TimestampBinaryOp::Subtract,
                        _ => return Err(EmitError::new("invalid checked timestamp operation")),
                    },
                    lhs: operand(lhs),
                    rhs: operand(rhs),
                    lhs_timestamp: self.value_types[&lhs.value]
                        .is_builtin(hew_types::BuiltinType::Timestamp),
                    rhs_timestamp: self.value_types[&rhs.value]
                        .is_builtin(hew_types::BuiltinType::Timestamp),
                    shape,
                    result: value_def(result),
                    normal: encode_edge(normal),
                    failures: failures
                        .iter()
                        .map(|failure| wire::CheckedFailure {
                            trap: trap_kind(failure.kind),
                            edge: encode_edge(&failure.edge),
                        })
                        .collect(),
                }
            }
            SemTerminator::CheckedBinary {
                op,
                lhs,
                rhs,
                result,
                normal,
                failures,
                ..
            } => wire::TermKind::CheckedBinary {
                binary_op: format!("{op:?}"),
                lhs: operand(lhs),
                rhs: operand(rhs),
                result: value_def(result),
                normal: encode_edge(normal),
                failures: failures
                    .iter()
                    .map(|failure| wire::CheckedFailure {
                        trap: trap_kind(failure.kind),
                        edge: encode_edge(&failure.edge),
                    })
                    .collect::<Vec<_>>(),
                ty: self.value_ty(lhs)?,
            },
            SemTerminator::Unreachable => wire::TermKind::Unreachable {},

            SemTerminator::Call {
                callee,
                args,
                result,
                normal,
                unwind,
                handback,
                ..
            } => wire::TermKind::Call {
                args: boundaries(args),
                result: call_result(result),
                normal: Some(normal.as_ref().map(encode_edge)),
                unwind: encode_unwind(unwind),
                request_shapes: None,
                callee: self.function_id(*callee)?,
                handback: handback.as_ref().map(|value| Some(value_def(value))),
            },
            SemTerminator::IndirectCall {
                callee,
                specialization,
                args,
                result,
                normal,
                unwind,
                ..
            } => wire::TermKind::IndirectCall {
                args: boundaries(args),
                result: call_result(result),
                normal: Some(normal.as_ref().map(encode_edge)),
                unwind: encode_unwind(unwind),
                request_shapes: None,
                callee: boundary(callee),
                specialization: specialization.map(|id| id.0),
            },
            SemTerminator::DynCall {
                receiver,
                slot,
                args,
                result,
                normal,
                unwind,
                ..
            } => wire::TermKind::DynCall {
                args: boundaries(args),
                result: call_result(result),
                normal: Some(normal.as_ref().map(encode_edge)),
                unwind: encode_unwind(unwind),
                request_shapes: None,
                receiver: boundary(receiver),
                slot: *slot,
            },
            SemTerminator::ValueCall {
                ty,
                capability,
                args,
                result,
                normal,
                unwind,
                ..
            } => wire::TermKind::ValueCall {
                args: boundaries(args),
                result: call_result(result),
                normal: Some(Some(encode_edge(normal))),
                unwind: encode_unwind(unwind),
                request_shapes: None,
                plan: self.capability_id(ty, *capability)?,
            },
            SemTerminator::RtCall {
                family,
                args,
                result,
                normal,
                unwind,
                ..
            } => {
                let shared_payload_type = if *family == hew_types::RuntimeCallFamily::RcNew {
                    Some(
                        self.value_types
                            .get(&args[0].operand.value)
                            .ok_or_else(|| EmitError::new("Rc payload lacks its checked type"))?
                            .user_facing()
                            .to_string(),
                    )
                } else {
                    None
                };
                let variant_result_shapes =
                    if let Some(hew_types::runtime_call::RuntimeSemanticContract {
                        result:
                            hew_types::runtime_call::RuntimeResultEffect::FreshOwnedVariant(kind),
                        ..
                    }) = family.semantic_contract()
                    {
                        let CallResult::Value(value) = result else {
                            return Err(EmitError::new("runtime variant result has no value"));
                        };
                        let shapes = hew_sir::runtime_variant_shape_refs(
                            &self.module.defs,
                            kind,
                            &value.ty,
                            &self.module.aggregate_shapes,
                            &self.module.variant_shapes,
                        )
                        .map_err(EmitError::new)?;
                        Some(wire::VariantResultShapes {
                            error: shapes.error.0,
                            error_len: shapes.error_len.0,
                        })
                    } else {
                        None
                    };
                // A failing operation can return its still-owned inputs on the unwind edge.
                let keeps_inputs_on_failure = family
                    .semantic_contract()
                    .is_some_and(hew_types::RuntimeSemanticContract::preserves_inputs_on_failure)
                    .then_some(true);
                wire::TermKind::RuntimeCall {
                    aggregate_result_shape: self.aggregate_result_shape(result).map(Some),
                    args: boundaries(args),
                    result: call_result(result),
                    normal: Some(Some(encode_edge(normal))),
                    unwind: encode_unwind(unwind),
                    request_shapes: None,
                    shared_payload_type,
                    family: self.family_id(*family),
                    structural: Some(
                        if *family == hew_types::RuntimeCallFamily::StructuralFormat {
                            Some(self.structural_id(
                                &hew_sir::StructuralType::canonical(
                                    self.value_types.get(&args[0].operand.value).ok_or_else(
                                        || EmitError::new("format operand has no type"),
                                    )?,
                                ),
                            )?)
                        } else {
                            None
                        },
                    ),
                    callbacks: Some(self.collection_callbacks(*family, args)?),
                    releases_contents: Some(family.releases_receiver_contents()),
                    keeps_inputs_on_failure,
                    result_member_shapes: Some(self.result_member_shapes(result)),
                    variant_result_shapes,
                    result_shape: self.result_shape(result),
                }
            }
            SemTerminator::ExternCall {
                signature,
                args,
                result,
                normal,
                unwind,
                ..
            } => wire::TermKind::ExternCall {
                aggregate_result_shape: self.aggregate_result_shape(result).map(Some),
                args: boundaries(args),
                result: call_result(result),
                normal: Some(Some(encode_edge(normal))),
                unwind: encode_unwind(unwind),
                request_shapes: None,
                r#extern: self.extern_id(&signature.symbol),
                result_shape: self.result_shape(result),
            },

            SemTerminator::Panic {
                message,
                assertion,
                cleanup,
            } => wire::TermKind::Panic {
                message: boundary(message),
                assertion: Some(
                    assertion
                        .as_ref()
                        .map(|values| values.each_ref().map(boundary)),
                ),
                cleanup: encode_edge(cleanup),
            },
            SemTerminator::Trap { kind } => wire::TermKind::Trap {
                trap: trap_kind(*kind),
            },
            SemTerminator::CheckedRaiseFault { kind, cleanup } => wire::TermKind::CheckedRaise {
                trap: trap_kind(*kind),
                cleanup: encode_edge(cleanup),
            },
            SemTerminator::CleanupDispatch { normal, fault } => wire::TermKind::CleanupDispatch {
                normal: encode_edge(normal),
                fault: encode_edge(fault),
            },
            SemTerminator::ResumeUnwind { handback } => wire::TermKind::ResumeUnwind {
                handback: handback.as_ref().map(|value| Some(boundary(value))),
            },
            SemTerminator::EnterDefer { defer, park, body } => wire::TermKind::EnterDefer {
                defer: defer.0,
                park: park.0,
                body: encode_edge(body),
            },
            SemTerminator::FinishDefer { defer, park, next } => wire::TermKind::FinishDefer {
                defer: defer.0,
                park: park.0,
                next: encode_edge(next),
            },
            SemTerminator::RecoverFault {
                result,
                deadline_variant,
                fault_variant,
                normal,
                unwind,
            } => wire::TermKind::RecoverFault {
                result_shape: Some(
                    self.module
                        .variant_shape_for_type(&result.ty)
                        .map(|shape| shape.id.0),
                ),
                result: value_def(result),
                deadline_variant: *deadline_variant,
                fault_variant: *fault_variant,
                normal: encode_edge(normal),
                unwind: Some(encode_edge(unwind)),
            },
            SemTerminator::Safepoint { resume, cancel } => {
                self.suspend_kind_id("Safepoint");
                wire::TermKind::Suspend {
                    suspension: wire::Suspension::Safepoint {
                        detail: wire::SafepointDetail {
                            budget: hew_sir::SAFEPOINT_BUDGET,
                        },
                    },
                    request_shapes: Some(None),
                    result_shape: Some(None),
                    error_shape: Some(None),
                    inputs: vec![],
                    result: wire::CallResult::Unit(()),
                    resumes: vec![encode_edge(resume)],
                    cancel: cancel.as_ref().map(encode_edge),
                    unwind: None,
                }
            }
            // The VM has no pool: an offloaded extern runs as the call it wraps.
            SemTerminator::Suspend {
                kind: SuspendKind::Offload { function },
                inputs,
                result,
                resumes,
                ..
            } => {
                let signature = usize::try_from(function.0)
                    .ok()
                    .and_then(|index| self.module.offloads.get(index))
                    .ok_or_else(|| EmitError::new("offload names no published extern"))?;
                let normal = resumes
                    .first()
                    .ok_or_else(|| EmitError::new("offload has no resume edge"))?;
                wire::TermKind::ExternCall {
                    aggregate_result_shape: None,
                    args: boundaries(inputs),
                    result: call_result(result),
                    normal: Some(Some(encode_edge(normal))),
                    unwind: None,
                    request_shapes: None,
                    r#extern: self.extern_id(&signature.symbol),
                    result_shape: self.result_shape(result),
                }
            }
            SemTerminator::Suspend {
                kind,
                inputs,
                result,
                resumes,
                cancel,
                unwind,
            } => {
                let (name, detail) = suspend_shape(kind);
                self.suspend_kind_id(name);
                wire::TermKind::Suspend {
                    suspension: detail,
                    request_shapes: Some(self.request_shapes(result)),
                    result_shape: Some(self.result_shape(result)),
                    error_shape: Some(self.error_shape(result)),
                    inputs: boundaries(inputs),
                    result: call_result(result),
                    resumes: resumes.iter().map(encode_edge).collect::<Vec<_>>(),
                    cancel: Some(encode_edge(cancel)),
                    unwind: Some(encode_edge(unwind)),
                }
            }

            SemTerminator::ActorCall {
                operation,
                args,
                result,
                normal,
                unwind,
                ..
            } => {
                let mut resource = None;
                if matches!(operation, hew_sir::ActorOperation::TimerSchedule { .. }) {
                    let CallResult::Value(result) = result else {
                        return Err(EmitError::new("timer has no resource result"));
                    };
                    let resource_index = self
                        .module
                        .resources
                        .keys()
                        .position(|ty| *ty == result.ty)
                        .ok_or_else(|| EmitError::new("timer has no checked release recipe"))?;
                    resource = Some(table_index(resource_index));
                }
                wire::TermKind::ActorCall {
                    args: boundaries(args),
                    result: call_result(result),
                    normal: Some(Some(encode_edge(normal))),
                    unwind: encode_unwind(unwind),
                    request_shapes: Some(self.request_shapes(result)),
                    operation: actor_operation(operation, resource)?,
                    submission_shapes: Some(self.submission_shapes(result)),
                    result_shape: self.result_shape(result),
                    error_shape: self.error_shape(result),
                }
            }
            SemTerminator::WireCodec {
                codec,
                plan,
                decode_result,
                args,
                result,
                normal,
                unwind,
                ..
            } => {
                let shape = plan.property_shape().map_err(EmitError::new)?;
                let encoded = serde_json::to_string(&shape)
                    .map_err(|error| EmitError::new(error.to_string()))?;
                let index = if let Some(index) =
                    self.codecs.iter().position(|existing| *existing == encoded)
                {
                    index
                } else {
                    self.codecs.push(encoded);
                    self.codecs.len() - 1
                };
                let mut callbacks = BTreeMap::new();
                if !codec.is_serialize() {
                    for (index, (_, member)) in plan.plans.iter().enumerate() {
                        let key = match &member.kind {
                            hew_sir::SemWireKind::Map { key, .. }
                            | hew_sir::SemWireKind::Set(key) => Some(key),
                            _ => None,
                        };
                        if let Some(key) = key {
                            callbacks.insert(
                                index.to_string(),
                                vec![
                                    self.capability_id(key, hew_types::ValueCapability::Hash)?,
                                    self.capability_id(key, hew_types::ValueCapability::Eq)?,
                                ],
                            );
                        }
                    }
                }
                let decoded = decode_result.as_ref().map(|decode| {
                    let error_root = plan
                        .plans
                        .keys()
                        .position(|ty| *ty == decode.error_ty)
                        .expect("verified codec error plan");
                    wire::DecodedCodec {
                        shape: decode.shape.0,
                        ok: decode.ok,
                        error: decode.error,
                        error_root: table_index(error_root),
                    }
                });
                wire::TermKind::WireCodec {
                    args: boundaries(args),
                    result: call_result(result),
                    normal: Some(Some(encode_edge(normal))),
                    unwind: encode_unwind(unwind),
                    request_shapes: None,
                    codec: table_index(index),
                    format: u32::try_from(codec.format.code()).expect("codec format code fits u32"),
                    direction: if codec.is_serialize() {
                        wire::CodecDirection::Encode
                    } else if codec.direction == hew_types::CodecDirection::TestInput {
                        wire::CodecDirection::TestInput
                    } else {
                        wire::CodecDirection::Decode
                    },
                    decoded,
                    callbacks,
                }
            }
        };
        Ok(wire::TermV1 {
            kind,
            span: Some(span),
        })
    }

    /// The source byte a site names, as a caret.
    ///
    /// SIR's debug facts carry an offset per operation site, not an extent, so
    /// the package reports the point rather than inventing a width.
    fn span(&self, provenance: &Provenance) -> Option<wire::SpanV1> {
        self.module
            .debug
            .site_offset(provenance)
            .map(|offset| wire::SpanV1 {
                start: offset,
                end: offset,
            })
    }
}

/// Carry a place's semantic origin into the package.
///
/// `Runtime` storage the runtime owns and `ActorState` seats belong to the
/// concurrency path, which never reaches this walker; they are named here so
/// the projection is closed against `PlaceOrigin`.
fn place_decl(place: &hew_sir::PlaceDecl) -> Place {
    let mut encoded = Place {
        id: place.id.0,
        origin: String::new(),
        base: None,
        shape: None,
        field: None,
        environment: None,
        initialized: None,
    };
    match &place.origin {
        hew_sir::PlaceOrigin::Local => encoded.origin = "local".to_string(),
        hew_sir::PlaceOrigin::Runtime => encoded.origin = "runtime".to_string(),
        hew_sir::PlaceOrigin::Aggregate { base, shape, field } => {
            encoded.origin = "aggregate".to_string();
            encoded.base = Some(match base {
                hew_sir::PlaceBase::Value(value) => serde_json::json!({ "value": value.0 }),
                hew_sir::PlaceBase::Place(place) => serde_json::json!({ "place": place.0 }),
            });
            encoded.shape = match shape {
                AggregateShapeRef::Tuple => None,
                AggregateShapeRef::Record(id) => Some(id.0),
            };
            encoded.field = Some(*field);
        }
        hew_sir::PlaceOrigin::Capture { environment, field } => {
            encoded.origin = "capture".to_string();
            encoded.environment = Some(environment.0);
            encoded.field = Some(*field);
        }
        hew_sir::PlaceOrigin::ActorState {
            state,
            field,
            initialized,
            ..
        } => {
            // The seat is a field of the state value the turn holds, so the
            // state's own identity is what makes the place resolvable - the
            // same fact `capture` carries as `environment`. `initialized` is
            // false only in the actor's `init` body for a seat init owns: it
            // holds no value until init's first store.
            encoded.origin = "actor_state".to_string();
            encoded.environment = Some(state.0);
            encoded.field = Some(*field);
            encoded.initialized = Some(*initialized);
        }
    }
    encoded
}

/// A table position as the package's id type.
///
/// SIR indexes its own tables with `u32`, so a module that overflows this has
/// already overflowed the IR it came from.
///
/// # Panics
///
/// Panics only when a package table exceeds the `u32` range.
fn table_index(index: usize) -> u32 {
    u32::try_from(index).expect("sandbox package table exceeds u32")
}

fn call_result(result: &CallResult) -> wire::CallResult {
    match result {
        CallResult::Unit => wire::CallResult::Unit(()),
        CallResult::Never => wire::CallResult::Never(wire::CallResultNever::Never),
        CallResult::Value(def) => wire::CallResult::ValueDef(value_def(def)),
    }
}
fn encode_edge(edge: &Edge) -> wire::Edge {
    wire::Edge {
        to: edge.target.0,
        args: operands(&edge.args),
    }
}
fn encode_unwind(unwind: &CallUnwind) -> Option<wire::Edge> {
    match unwind {
        CallUnwind::NotApplicable => None,
        CallUnwind::Cleanup(target) => Some(encode_edge(target)),
    }
}
fn block_arg(arg: &hew_sir::BlockArg) -> Value {
    Value {
        value: arg.value.0,
        own: own_name(arg.own).to_string(),
    }
}

fn value_def(def: &ValueDef) -> wire::ValueDef {
    wire::ValueDef {
        value: def.id.0,
        own: own_kind(def.own),
    }
}
fn operand(operand: &Operand) -> u32 {
    operand.value.0
}

fn operands(operands: &[Operand]) -> Vec<u32> {
    operands.iter().map(|value| value.value.0).collect()
}

fn boundary(operand: &BoundaryOperand) -> wire::BoundaryOperand {
    wire::BoundaryOperand {
        value: operand.operand.value.0,
        decision: match operand.decision {
            BoundaryDecision::Borrow => wire::BoundaryDecision::Borrow,
            BoundaryDecision::BorrowMut => wire::BoundaryDecision::BorrowMut,
            BoundaryDecision::Copy => wire::BoundaryDecision::Copy,
            BoundaryDecision::Move => wire::BoundaryDecision::Move,
            BoundaryDecision::Snapshot(_) => wire::BoundaryDecision::Snapshot,
        },
    }
}
fn boundaries(operands: &[BoundaryOperand]) -> Vec<wire::BoundaryOperand> {
    operands.iter().map(boundary).collect()
}
fn shape_ref(shape: AggregateShapeRef) -> Option<u32> {
    match shape {
        AggregateShapeRef::Tuple => None,
        AggregateShapeRef::Record(id) => Some(id.0),
    }
}
const fn own_kind(own: OwnKind) -> wire::Own {
    match own {
        OwnKind::None => wire::Own::None,
        OwnKind::Owned => wire::Own::Owned,
        OwnKind::Guaranteed => wire::Own::Guaranteed,
    }
}
const fn own_name(own: OwnKind) -> &'static str {
    match own {
        OwnKind::None => "none",
        OwnKind::Owned => "owned",
        OwnKind::Guaranteed => "guaranteed",
    }
}

const fn passing_name(passing: hew_sir::SemParamPassing) -> &'static str {
    match passing {
        hew_sir::SemParamPassing::ReadOnly => "copy",
        hew_sir::SemParamPassing::Borrow => "borrow",
        hew_sir::SemParamPassing::BorrowMut => "borrow_mut",
        hew_sir::SemParamPassing::Consume => "move",
    }
}

/// One actor or supervisor operation, named by its own variant.
///
/// The VM dispatches on this name and its payload; it never infers an
/// operation from a symbol or from the shape of the arguments.
fn actor_operation(
    operation: &hew_sir::ActorOperation,
    resource: Option<u32>,
) -> Result<wire::ActorOperation, EmitError> {
    use hew_sir::ActorOperation as Op;
    use wire::ActorOperation as Wire;
    fn protocol(p: &hew_sir::ActorCallProtocol) -> wire::ActorProtocol {
        wire::ActorProtocol {
            actor: p.actor.0,
            message: p.message,
            policy: send_policy(p.policy),
            sealed: p.sealed,
        }
    }
    fn target(t: hew_sir::LifecycleTarget) -> wire::LifecycleTarget {
        match t {
            hew_sir::LifecycleTarget::Actor(id) => wire::LifecycleTarget::Actor { actor: id.0 },
            hew_sir::LifecycleTarget::Supervisor(id) => {
                wire::LifecycleTarget::Supervisor { supervisor: id.0 }
            }
        }
    }
    Ok(match operation {
        Op::Stop(t) => Wire::Stop { target: target(*t) },
        Op::Terminate(t) => Wire::Terminate { target: target(*t) },
        Op::AwaitStopped(t) => Wire::AwaitStopped { target: target(*t) },
        Op::AwaitRestarted(t) => Wire::AwaitRestarted { target: target(*t) },
        Op::Spawn(actor) => Wire::Spawn { actor: actor.0 },
        Op::SelfHandle(actor) => Wire::SelfHandle { actor: actor.0 },
        Op::CallStart(p) => Wire::CallStart {
            protocol: protocol(p),
        },
        Op::CallTake(p) => Wire::CallTake {
            protocol: protocol(p),
        },
        Op::Submit { actor, policy, .. } => Wire::Submit {
            actor: actor.0,
            policy: send_policy(*policy),
        },
        Op::TimerSchedule {
            actor,
            message,
            policy,
        } => Wire::TimerSchedule {
            actor: actor.0,
            message: *message,
            resource: resource.ok_or_else(|| EmitError::new("timer has no checked resource"))?,
            policy: match policy {
                hew_types::actor_delivery::SendPolicy::Reject => wire::TimerSendPolicy::Reject,
                hew_types::actor_delivery::SendPolicy::DropNewest => {
                    wire::TimerSendPolicy::DropNewest
                }
                hew_types::actor_delivery::SendPolicy::Wait => {
                    return Err(EmitError::new("timer cannot wait for delivery"))
                }
            },
        },
        Op::StreamStart { actor, message, .. } => Wire::StreamStart {
            actor: actor.0,
            message: *message,
        },
        Op::Identity { .. } => Wire::Identity {},
        Op::LocalObservation { kind, .. } => Wire::LocalObservation {
            kind: observation_name(*kind).to_string(),
        },
        Op::SupervisorSpawn(supervisor) => Wire::SupervisorSpawn {
            supervisor: supervisor.0,
        },
        Op::SupervisorChild { supervisor, child } => Wire::SupervisorChild {
            supervisor: supervisor.0,
            child: *child,
        },
        Op::SupervisorPoolView { supervisor, child } => Wire::SupervisorPoolView {
            supervisor: supervisor.0,
            child: *child,
        },
    })
}
fn send_policy(policy: hew_types::actor_delivery::SendPolicy) -> wire::SendPolicy {
    match policy {
        hew_types::actor_delivery::SendPolicy::Reject => wire::SendPolicy::Reject,
        hew_types::actor_delivery::SendPolicy::Wait => wire::SendPolicy::Wait,
        hew_types::actor_delivery::SendPolicy::DropNewest => wire::SendPolicy::DropNewest,
    }
}

const fn observation_name(kind: hew_sir::LocalObservationKind) -> &'static str {
    match kind {
        hew_sir::LocalObservationKind::Link => "link",
        hew_sir::LocalObservationKind::Monitor => "monitor",
        hew_sir::LocalObservationKind::Unlink => "unlink",
        hew_sir::LocalObservationKind::Demonitor => "demonitor",
    }
}

const fn overflow_name(overflow: hew_sir::SemActorOverflow) -> &'static str {
    match overflow {
        hew_sir::SemActorOverflow::Block => "block",
        hew_sir::SemActorOverflow::DropNew => "drop_new",
        hew_sir::SemActorOverflow::DropOld => "drop_old",
        hew_sir::SemActorOverflow::Fail => "fail",
        hew_sir::SemActorOverflow::Coalesce => "coalesce",
    }
}

const fn coalesce_fallback_name(fallback: hew_sir::SemCoalesceFallback) -> &'static str {
    match fallback {
        hew_sir::SemCoalesceFallback::DropNew => "drop_new",
        hew_sir::SemCoalesceFallback::DropOld => "drop_old",
        hew_sir::SemCoalesceFallback::Fail => "fail",
    }
}

const fn strategy_name(strategy: hew_sir::SemRestartStrategy) -> &'static str {
    match strategy {
        hew_sir::SemRestartStrategy::OneForOne => "one_for_one",
        hew_sir::SemRestartStrategy::OneForAll => "one_for_all",
        hew_sir::SemRestartStrategy::RestForOne => "rest_for_one",
        hew_sir::SemRestartStrategy::SimpleOneForOne => "simple_one_for_one",
    }
}

const fn restart_name(policy: hew_sir::SemRestartPolicy) -> &'static str {
    match policy {
        hew_sir::SemRestartPolicy::Permanent => "permanent",
        hew_sir::SemRestartPolicy::Transient => "transient",
        hew_sir::SemRestartPolicy::Temporary => "temporary",
    }
}

const fn capability_name(capability: hew_types::ValueCapability) -> &'static str {
    match capability {
        hew_types::ValueCapability::Hash => "Hash",
        hew_types::ValueCapability::Eq => "Eq",
        hew_types::ValueCapability::Less => "Less",
        hew_types::ValueCapability::LessEqual => "LessEqual",
        hew_types::ValueCapability::Greater => "Greater",
        hew_types::ValueCapability::GreaterEqual => "GreaterEqual",
    }
}

/// SIR's trap kinds carried into the trace schema's vocabulary.
///
/// `SignedMinDivNegOne` is an overflow: dividing the signed minimum by -1 has
/// no representable result, and native execution exits with the overflow
/// status.
const fn trap_kind(kind: TrapKind) -> wire::TrapName {
    match kind {
        TrapKind::IntegerOverflow | TrapKind::SignedMinDivNegOne => wire::TrapName::IntegerOverflow,
        TrapKind::DivideByZero => wire::TrapName::DivideByZero,
        TrapKind::ShiftOutOfRange => wire::TrapName::ShiftOutOfRange,
        TrapKind::IndexOutOfBounds => wire::TrapName::VectorBounds,
        TrapKind::ActorRefused => wire::TrapName::ActorRefused,
    }
}

/// Every suspension kind the compiler can produce, named with its payload.
#[expect(
    clippy::too_many_lines,
    reason = "exhaustive suspension-to-wire mapping has one authoritative match"
)]
fn suspend_shape(kind: &SuspendKind) -> (&'static str, wire::Suspension) {
    use wire::Suspension as S;
    match kind {
        SuspendKind::Sleep => ("Sleep", S::Sleep { detail: Some(None) }),
        SuspendKind::SleepUntil => ("SleepUntil", S::SleepUntil { detail: Some(None) }),
        SuspendKind::Await => ("Await", S::Await { detail: Some(None) }),
        SuspendKind::ActorSend => ("ActorSend", S::ActorSend { detail: Some(None) }),
        SuspendKind::RestartWait => ("RestartWait", S::RestartWait { detail: Some(None) }),
        SuspendKind::Read => ("Read", S::Read { detail: Some(None) }),
        SuspendKind::Accept => ("Accept", S::Accept { detail: Some(None) }),
        SuspendKind::CallClosure => ("CallClosure", S::CallClosure { detail: Some(None) }),
        SuspendKind::Timeout => ("Timeout", S::Timeout { detail: Some(None) }),
        SuspendKind::ScopeDeadline => ("ScopeDeadline", S::ScopeDeadline { detail: Some(None) }),
        SuspendKind::Yield => ("Yield", S::Yield { detail: Some(None) }),
        SuspendKind::GeneratorNext => ("GeneratorNext", S::GeneratorNext { detail: Some(None) }),
        SuspendKind::ServiceAcquire { actor, export } => (
            "ServiceAcquire",
            S::ServiceAcquire {
                detail: wire::ServiceAcquireDetail {
                    actor: actor.0,
                    export: *export,
                },
            },
        ),
        SuspendKind::SemaphoreAcquire { operation } => (
            "SemaphoreAcquire",
            S::SemaphoreAcquire {
                detail: wire::SemaphoreAcquireDetail {
                    operation: *operation,
                },
            },
        ),
        SuspendKind::NativeIo { operation } => (
            "NativeIo",
            S::NativeIo {
                detail: wire::NativeIoDetail {
                    operation: *operation,
                },
            },
        ),
        SuspendKind::Offload { function } => (
            "Offload",
            S::Offload {
                detail: wire::OffloadDetail {
                    function: function.0,
                },
            },
        ),
        SuspendKind::Ask {
            actor,
            message,
            policy,
            sealed,
        } => (
            "Ask",
            S::Ask {
                detail: wire::AskDetail {
                    actor: actor.0,
                    message: *message,
                    policy: send_policy(*policy),
                    sealed: *sealed,
                },
            },
        ),
        SuspendKind::Join { scope, mode } => (
            "Join",
            S::Join {
                detail: wire::JoinDetail {
                    scope: scope.0,
                    mode: match mode {
                        hew_sir::TaskScopeJoinMode::Wait => wire::JoinMode::Wait,
                        hew_sir::TaskScopeJoinMode::CancelLosers => wire::JoinMode::CancelLosers,
                        hew_sir::TaskScopeJoinMode::PropagateFault => {
                            wire::JoinMode::PropagateFault
                        }
                    },
                },
            },
        ),
        SuspendKind::StreamSend { park } => (
            "StreamSend",
            S::StreamSend {
                detail: wire::StreamSendDetail { park: *park },
            },
        ),
        SuspendKind::StreamNext { park } => (
            "StreamNext",
            S::StreamNext {
                detail: wire::StreamNextDetail { park: *park },
            },
        ),
        SuspendKind::Select { has_timeout, order } => (
            "Select",
            S::Select {
                detail: wire::SelectDetail {
                    has_timeout: *has_timeout,
                    order: match order {
                        hew_sir::TaskSelectionOrder::Source => wire::SelectionOrder::Source,
                        hew_sir::TaskSelectionOrder::Completion => wire::SelectionOrder::Completion,
                    },
                },
            },
        ),
    }
}
