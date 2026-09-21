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

use hew_sir::{
    AggregateShapeRef, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind, CallableId, Edge,
    Operand, OwnKind, Provenance, SemBlock, SemFunction, SemModule, SemOp, SemOpKind,
    SemTerminator, SuspendKind, TrapKind, ValueDef,
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
    pub function: u32,
    pub exit: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AggregateShape {
    pub id: u32,
    pub name: String,
    pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariantShape {
    pub id: u32,
    pub name: String,
    pub cases: Vec<VariantCase>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariantCase {
    pub name: String,
    pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Closure {
    pub id: u32,
    pub body: u32,
    pub fields: u32,
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
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RuntimeFamily {
    pub id: u32,
    pub family: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detail: Option<serde_json::Value>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Extern {
    pub id: u32,
    pub symbol: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
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
    /// `capture`: the closure environment receiver.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub environment: Option<u32>,
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
    pub ops: Vec<serde_json::Value>,
    pub term: serde_json::Value,
}

/// Does this module need the concurrency path?
///
/// WHY: the AST emitter still owns actors, supervisors, tasks, select and
/// pipes for one more change, so a module that reaches them is routed there by
/// this structural SIR fact rather than by trying the walker and catching an
/// error. WHEN OBSOLETE: the actor change lands these constructs in the
/// walker. REAL FIX: that change deletes this predicate and its caller's
/// branch, and the following change deletes the AST emitter.
#[must_use]
pub fn uses_concurrency(module: &SemModule) -> bool {
    if !module.actors.is_empty() || !module.supervisors.is_empty() {
        return true;
    }
    module.functions.iter().any(|function| {
        function.blocks.iter().any(|block| {
            block.ops.iter().any(|op| {
                matches!(
                    op.kind,
                    SemOpKind::GeneratorMake { .. }
                        | SemOpKind::StreamPipe { .. }
                        | SemOpKind::TaskScopeEnter { .. }
                        | SemOpKind::TaskScopeClose { .. }
                        | SemOpKind::TaskSpawn { .. }
                        | SemOpKind::ActorIngressAdapter(_)
                )
            }) || matches!(block.terminator, SemTerminator::ActorCall { .. })
                || matches!(
                    &block.terminator,
                    SemTerminator::Suspend { kind, .. } if suspend_needs_scheduler(kind)
                )
        })
    })
}

/// `Sleep` and `NativeIo` resume on the running activation:
/// a virtual-clock advance and a rejected capability. Every other
/// kind parks a frame the scheduler must wake.
fn suspend_needs_scheduler(kind: &SuspendKind) -> bool {
    !matches!(kind, SuspendKind::Sleep | SuspendKind::NativeIo { .. })
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
    strings: BTreeMap<hew_sir::StringLiteralId, u32>,
    bytes: BTreeMap<hew_sir::BytesLiteralId, u32>,
    functions: BTreeMap<CallableId, u32>,
    capabilities: BTreeMap<(String, &'static str), u32>,
    families: Vec<RuntimeFamily>,
    family_index: BTreeMap<String, u32>,
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
            .map(|(index, (ty, capability))| {
                (
                    (ty.user_facing().to_string(), capability_name(*capability)),
                    table_index(index),
                )
            })
            .collect();
        Self {
            module,
            strings,
            bytes,
            functions,
            capabilities,
            families: Vec::new(),
            family_index: BTreeMap::new(),
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
                SemTerminator::EnterDefer { .. }
                | SemTerminator::FinishDefer { .. }
                | SemTerminator::CleanupDispatch { .. }
                | SemTerminator::CheckedRaiseFault { .. }
                | SemTerminator::Return { .. }
                | SemTerminator::Goto(_)
                | SemTerminator::Branch { .. }
                | SemTerminator::Panic { .. }
                | SemTerminator::Trap { .. }
                | SemTerminator::ResumeUnwind
                | SemTerminator::Unreachable => {}
            }
        }
    }

    /// The demanded enum descriptor a runtime-produced value is built against.
    ///
    /// The descriptor table owns declaration-order tags, so a shim constructs
    /// the enum the compiler demanded instead of assuming a variant order.
    fn result_shape(&self, result: &CallResult) -> serde_json::Value {
        match result {
            CallResult::Value(def) => self
                .module
                .variant_shape_for_type(&def.ty)
                .map_or(serde_json::Value::Null, |shape| shape.id.0.into()),
            CallResult::Unit | CallResult::Never => serde_json::Value::Null,
        }
    }

    fn suspend_kind_id(&mut self, name: &str) {
        if !self.suspend_kinds.iter().any(|kind| kind == name) {
            self.suspend_kinds.push(name.to_string());
        }
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
            closures: self
                .module
                .closures
                .iter()
                .map(|closure| {
                    Ok(Closure {
                        id: closure.id.0,
                        body: self.function_id(closure.body)?,
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
                                    method: slot.method_name.clone(),
                                    callee: self.function_id(slot.callee)?,
                                    receiver: passing_name(slot.receiver).to_string(),
                                })
                            })
                            .collect::<Result<Vec<_>, EmitError>>()?,
                    })
                })
                .collect::<Result<Vec<_>, EmitError>>()?,
            value_capabilities: self
                .module
                .value_capabilities
                .iter()
                .enumerate()
                .map(|(index, ((ty, capability), plan))| {
                    Ok(ValueCapabilityPlan {
                        id: table_index(index),
                        capability: capability_name(*capability).to_string(),
                        ty: ty.user_facing().to_string(),
                        callable: plan
                            .callable
                            .map(|callable| self.function_id(callable))
                            .transpose()?,
                    })
                })
                .collect::<Result<Vec<_>, EmitError>>()?,
            runtime_families: self.families,
            externs: self.externs,
            suspend_kinds: self.suspend_kinds,
            functions,
        })
    }

    fn aggregate_shapes(&self) -> Vec<AggregateShape> {
        self.module
            .aggregate_shapes
            .iter()
            .map(|shape| AggregateShape {
                id: shape.id.0,
                name: shape.aggregate_ty.user_facing().to_string(),
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
            .get(&(ty.user_facing().to_string(), capability_name(capability)))
            .copied()
            .ok_or_else(|| {
                EmitError::new(format!(
                    "`{}` has no selected {capability:?} operation",
                    ty.user_facing()
                ))
            })
    }

    /// Intern a runtime-call family by its own identity.
    ///
    /// `RuntimeCallFamily` serializes as a bare name for a unit variant and as
    /// a one-key object for a payload variant, so the family name and its
    /// detail come straight from the type rather than from a spelling this
    /// walker invents.
    fn family_id(&mut self, family: hew_types::RuntimeCallFamily) -> Result<u32, EmitError> {
        let value = serde_json::to_value(family)
            .map_err(|error| EmitError::new(format!("runtime family is not encodable: {error}")))?;
        let (name, detail) = match value {
            serde_json::Value::String(name) => (name, None),
            serde_json::Value::Object(map) if map.len() == 1 => {
                let (name, detail) = map.into_iter().next().expect("one key");
                (name, Some(detail))
            }
            other => {
                return Err(EmitError::new(format!(
                    "runtime family has an unexpected encoding: {other}"
                )))
            }
        };
        let key = match &detail {
            Some(detail) => format!("{name}:{detail}"),
            None => name.clone(),
        };
        if let Some(id) = self.family_index.get(&key) {
            return Ok(*id);
        }
        let id = table_index(self.families.len());
        self.families.push(RuntimeFamily {
            id,
            family: name,
            detail,
        });
        self.family_index.insert(key, id);
        Ok(id)
    }

    fn extern_id(&mut self, symbol: &str) -> u32 {
        if let Some(id) = self.extern_index.get(symbol) {
            return *id;
        }
        let id = table_index(self.externs.len());
        self.externs.push(Extern {
            id,
            symbol: symbol.to_string(),
        });
        self.extern_index.insert(symbol.to_string(), id);
        id
    }

    fn function(&mut self, id: u32, function: &SemFunction) -> Result<Function, EmitError> {
        self.index_value_types(function);
        Ok(Function {
            id,
            name: function.name.clone(),
            params: function.params.iter().map(block_arg).collect(),
            entry: function.entry.0,
            places: function.places.iter().map(place_decl).collect(),
            blocks: function
                .blocks
                .iter()
                .map(|block| self.block(block))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn block(&mut self, block: &SemBlock) -> Result<Block, EmitError> {
        Ok(Block {
            id: block.id.0,
            params: block.args.iter().map(block_arg).collect(),
            ops: block
                .ops
                .iter()
                .map(|op| self.op(op))
                .collect::<Result<Vec<_>, _>>()?,
            term: self.terminator(&block.terminator, &block.terminator_provenance)?,
        })
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one arm per SemOpKind variant with no wildcard is the point: a new SIR operation must fail to compile here"
    )]
    fn op(&mut self, op: &SemOp) -> Result<serde_json::Value, EmitError> {
        // An operation that defines several values carries them all in
        // `results`; every other operation has the one `dst`.
        let dst = match op.kind {
            SemOpKind::Destructure { .. } | SemOpKind::VariantDestructure { .. } => None,
            _ => op.results.first().map(|result| result.id.0),
        };
        let span = self.span(&op.provenance);
        let mut encoded = match &op.kind {
            SemOpKind::ConstInteger(value) => serde_json::json!({
                "op": "const.int",
                "value": value.to_string(),
                "ty": op
                    .results
                    .first()
                    .map(|result| result.ty.user_facing().to_string()),
            }),
            SemOpKind::ConstBool(value) => {
                serde_json::json!({ "op": "const.bool", "value": value })
            }
            SemOpKind::ConstFloat(value) => {
                let mut encoded = serde_json::json!({
                    "op": "const.float",
                    "ty": op
                        .results
                        .first()
                        .map(|result| result.ty.user_facing().to_string()),
                });
                if value.is_nan() {
                    encoded["nonfinite"] = "nan".into();
                    encoded["value"] = 0.into();
                } else if value.is_infinite() {
                    encoded["nonfinite"] = if value.is_sign_negative() {
                        "-inf".into()
                    } else {
                        "inf".into()
                    };
                    encoded["value"] = 0.into();
                } else {
                    encoded["value"] = serde_json::Number::from_f64(*value)
                        .map(serde_json::Value::Number)
                        .ok_or_else(|| EmitError::new("float literal is not encodable"))?;
                }
                encoded
            }
            SemOpKind::ConstChar(value) => {
                serde_json::json!({ "op": "const.char", "value": value.to_string() })
            }
            SemOpKind::ConstUnit => serde_json::json!({ "op": "const.unit" }),
            SemOpKind::ConstDuration(nanos) => {
                serde_json::json!({ "op": "const.duration", "nanos": nanos.to_string() })
            }
            SemOpKind::ConstStr(id) => {
                serde_json::json!({ "op": "const.str", "str": self.string_id(*id)? })
            }
            SemOpKind::ConstBytes(id) => {
                serde_json::json!({ "op": "const.bytes", "bytes": self.bytes_id(*id)? })
            }

            SemOpKind::CopyValue { source } => {
                serde_json::json!({ "op": "copy_value", "source": operand(source) })
            }
            SemOpKind::Move { source } => {
                serde_json::json!({ "op": "move", "source": operand(source) })
            }
            SemOpKind::Fork { source } => {
                serde_json::json!({ "op": "fork", "source": operand(source) })
            }
            SemOpKind::DestroyValue { value } => {
                serde_json::json!({ "op": "destroy_value", "value": operand(value) })
            }
            SemOpKind::BeginBorrow { owner } => {
                serde_json::json!({ "op": "begin_borrow", "owner": operand(owner) })
            }
            SemOpKind::EndBorrow { borrow } => {
                serde_json::json!({ "op": "end_borrow", "borrow": operand(borrow) })
            }
            SemOpKind::FinishLinearReceiver => {
                serde_json::json!({ "op": "finish_linear_receiver" })
            }

            SemOpKind::AllocPlace { place } => {
                serde_json::json!({ "op": "alloc_place", "place": place.0 })
            }
            SemOpKind::StoreInit { place, value } => {
                serde_json::json!({ "op": "store.init", "place": place.0, "value": operand(value) })
            }
            SemOpKind::StoreAssign { place, value } => {
                serde_json::json!({ "op": "store.assign", "place": place.0, "value": operand(value) })
            }
            SemOpKind::LoadCopy { place } => {
                serde_json::json!({ "op": "load.copy", "place": place.0 })
            }
            SemOpKind::LoadTake { place } => {
                serde_json::json!({ "op": "load.take", "place": place.0 })
            }
            SemOpKind::LoadBorrow { place } => {
                serde_json::json!({ "op": "load.borrow", "place": place.0 })
            }
            SemOpKind::EndLifetime { place } => {
                serde_json::json!({ "op": "end_lifetime", "place": place.0 })
            }

            SemOpKind::TupleMake { elements } => {
                serde_json::json!({ "op": "tuple.make", "elements": operands(elements) })
            }
            SemOpKind::TupleGet { tuple, index } => {
                serde_json::json!({ "op": "tuple.get", "tuple": operand(tuple), "index": index })
            }
            SemOpKind::AggregateMake { shape, fields } => serde_json::json!({
                "op": "aggregate.make",
                "shape": shape_ref(*shape),
                "fields": operands(fields),
            }),
            SemOpKind::AggregateProjectCopy {
                shape,
                aggregate,
                field,
            } => serde_json::json!({
                "op": "aggregate.project_copy",
                "shape": shape_ref(*shape),
                "aggregate": operand(aggregate),
                "field": field,
            }),
            SemOpKind::AggregateProjectBorrow {
                shape,
                aggregate,
                field,
            } => serde_json::json!({
                "op": "aggregate.project_borrow",
                "shape": shape_ref(*shape),
                "aggregate": operand(aggregate),
                "field": field,
            }),
            SemOpKind::Destructure { shape, aggregate } => serde_json::json!({
                "op": "destructure",
                "shape": shape_ref(*shape),
                "aggregate": operand(aggregate),
                "results": op.results.iter().map(|result| result.id.0).collect::<Vec<_>>(),
            }),
            SemOpKind::ArrayMake { fields } => {
                serde_json::json!({ "op": "array.make", "elements": operands(fields) })
            }
            SemOpKind::ArrayRepeat { value } => {
                serde_json::json!({ "op": "array.repeat", "value": operand(value) })
            }

            SemOpKind::VariantMake {
                shape,
                variant,
                fields,
            } => serde_json::json!({
                "op": "variant.make",
                "shape": shape.0,
                "variant": variant,
                "fields": operands(fields),
            }),
            SemOpKind::VariantIs {
                shape,
                variant,
                source,
            } => serde_json::json!({
                "op": "variant.is",
                "shape": shape.0,
                "variant": variant,
                "source": operand(source),
            }),
            SemOpKind::VariantProjectCopy {
                shape,
                variant,
                source,
                field,
            } => serde_json::json!({
                "op": "variant.project_copy",
                "shape": shape.0,
                "variant": variant,
                "source": operand(source),
                "field": field,
            }),
            SemOpKind::VariantProjectBorrow {
                shape,
                variant,
                source,
                field,
            } => serde_json::json!({
                "op": "variant.project_borrow",
                "shape": shape.0,
                "variant": variant,
                "source": operand(source),
                "field": field,
            }),
            SemOpKind::VariantDestructure {
                shape,
                variant,
                source,
            } => serde_json::json!({
                "op": "variant.destructure",
                "shape": shape.0,
                "variant": variant,
                "source": operand(source),
                "results": op.results.iter().map(|result| result.id.0).collect::<Vec<_>>(),
            }),

            SemOpKind::Unary { op: unary, value } => serde_json::json!({
                "op": "unary",
                "unary_op": format!("{unary:?}"),
                "value": operand(value),
                "ty": self.value_ty(value)?,
            }),
            SemOpKind::Binary {
                op: binary,
                lhs,
                rhs,
            } => serde_json::json!({
                "op": "binary",
                "binary_op": format!("{binary:?}"),
                "lhs": operand(lhs),
                "rhs": operand(rhs),
                "ty": self.value_ty(lhs)?,
            }),
            SemOpKind::Cast { value, to } => serde_json::json!({
                "op": "cast",
                "value": operand(value),
                "from": self.value_ty(value)?,
                "to": to.user_facing().to_string(),
            }),
            SemOpKind::StrEq { lhs, rhs } => serde_json::json!({
                "op": "str.eq", "lhs": operand(lhs), "rhs": operand(rhs),
            }),
            SemOpKind::BytesEq { lhs, rhs } => serde_json::json!({
                "op": "bytes.eq", "lhs": operand(lhs), "rhs": operand(rhs),
            }),

            SemOpKind::FunctionMake { callable } => serde_json::json!({
                "op": "function.make",
                "callable": self.function_id(*callable)?,
            }),
            SemOpKind::ClosureMake { closure, fields } => serde_json::json!({
                "op": "closure.make",
                "closure": closure.0,
                "fields": operands(fields),
            }),
            SemOpKind::GeneratorCoerce { source } => serde_json::json!({
                "op": "move",
                "source": operand(source),
            }),
            SemOpKind::CallableCoerce { source } => serde_json::json!({
                "op": "callable.coerce",
                "source": operand(source),
            }),
            SemOpKind::DynMake { vtable, value } => serde_json::json!({
                "op": "dyn.make",
                "vtable": vtable.0,
                "value": operand(value),
            }),

            SemOpKind::RegisterDefer {
                defer,
                scope,
                dependencies,
            } => serde_json::json!({
                "op": "register_defer",
                "defer": defer.0,
                "scope": scope.0,
                "dependencies": dependencies.iter().map(|place| place.0).collect::<Vec<_>>(),
            }),

            // The concurrency constructs. `uses_concurrency` routes a module
            // that reaches one of these away from this walker, so these arms
            // keep the match closed against `SemOpKind` without claiming
            // support the package's tables do not carry.
            SemOpKind::GeneratorMake { .. }
            | SemOpKind::StreamPipe { .. }
            | SemOpKind::TaskScopeEnter { .. }
            | SemOpKind::TaskScopeClose { .. }
            | SemOpKind::TaskSpawn { .. }
            | SemOpKind::ActorIngressAdapter(_) => {
                return Err(EmitError::new(
                    "a concurrency operation reached the sequential package walker",
                ))
            }
        };
        encoded["dst"] = match dst {
            Some(dst) => dst.into(),
            None => serde_json::Value::Null,
        };
        encoded["span"] = span;
        Ok(encoded)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one arm per SemTerminator variant with no wildcard is the point: a new SIR terminator must fail to compile here"
    )]
    fn terminator(
        &mut self,
        terminator: &SemTerminator,
        provenance: &Provenance,
    ) -> Result<serde_json::Value, EmitError> {
        let span = self.span(provenance);
        let mut encoded = match terminator {
            SemTerminator::Return { value } => serde_json::json!({
                "op": "return",
                "value": value.as_ref().map(boundary),
            }),
            SemTerminator::Goto(edge) => {
                serde_json::json!({ "op": "goto", "edge": encode_edge(edge) })
            }
            SemTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => serde_json::json!({
                "op": "branch",
                "condition": operand(condition),
                "then": encode_edge(then_target),
                "else": encode_edge(else_target),
            }),
            SemTerminator::SwitchVariant {
                shape,
                scrutinee,
                arms,
                ..
            } => serde_json::json!({
                "op": "switch.variant",
                "shape": shape.0,
                "scrutinee": operand(scrutinee),
                "arms": arms.iter().map(|arm| serde_json::json!({
                    "variant": arm.variant,
                    "fields": arm.fields.iter().map(value_def).collect::<Vec<_>>(),
                    "edge": encode_edge(&arm.target),
                })).collect::<Vec<_>>(),
            }),
            SemTerminator::CheckedBinary {
                op,
                lhs,
                rhs,
                result,
                normal,
                failures,
                ..
            } => serde_json::json!({
                "op": "checked.binary",
                "binary_op": format!("{op:?}"),
                "lhs": operand(lhs),
                "rhs": operand(rhs),
                "ty": self.value_ty(lhs)?,
                "result": value_def(result),
                "normal": encode_edge(normal),
                "failures": failures.iter().map(|failure| serde_json::json!({
                    "trap": trap_name(failure.kind),
                    "edge": encode_edge(&failure.edge),
                })).collect::<Vec<_>>(),
            }),
            SemTerminator::Unreachable => serde_json::json!({ "op": "unreachable" }),

            SemTerminator::Call {
                callee,
                args,
                result,
                normal,
                unwind,
                ..
            } => serde_json::json!({
                "op": "call",
                "callee": self.function_id(*callee)?,
                "args": boundaries(args),
                "result": call_result(result),
                "normal": normal.as_ref().map(encode_edge),
                "unwind": encode_unwind(unwind),
            }),
            SemTerminator::IndirectCall {
                callee,
                args,
                result,
                normal,
                unwind,
                ..
            } => serde_json::json!({
                "op": "indirect.call",
                "callee": boundary(callee),
                "args": boundaries(args),
                "result": call_result(result),
                "normal": normal.as_ref().map(encode_edge),
                "unwind": encode_unwind(unwind),
            }),
            SemTerminator::DynCall {
                receiver,
                slot,
                args,
                result,
                normal,
                unwind,
                ..
            } => serde_json::json!({
                "op": "dyn.call",
                "receiver": boundary(receiver),
                "slot": slot,
                "args": boundaries(args),
                "result": call_result(result),
                "normal": normal.as_ref().map(encode_edge),
                "unwind": encode_unwind(unwind),
            }),
            SemTerminator::ValueCall {
                ty,
                capability,
                args,
                result,
                normal,
                unwind,
                ..
            } => serde_json::json!({
                "op": "value.call",
                "plan": self.capability_id(ty, *capability)?,
                "args": boundaries(args),
                "result": call_result(result),
                "normal": encode_edge(normal),
                "unwind": encode_unwind(unwind),
            }),
            SemTerminator::RtCall {
                family,
                args,
                result,
                normal,
                unwind,
                ..
            } => serde_json::json!({
                "op": "runtime.call",
                "family": self.family_id(*family)?,
                "args": boundaries(args),
                "result": call_result(result),
                "result_shape": self.result_shape(result),
                "normal": encode_edge(normal),
                "unwind": encode_unwind(unwind),
            }),
            SemTerminator::ExternCall {
                signature,
                args,
                result,
                normal,
                unwind,
                ..
            } => serde_json::json!({
                "op": "extern.call",
                "extern": self.extern_id(&signature.symbol),
                "args": boundaries(args),
                "result": call_result(result),
                "result_shape": self.result_shape(result),
                "normal": encode_edge(normal),
                "unwind": encode_unwind(unwind),
            }),

            SemTerminator::Panic { message, cleanup } => serde_json::json!({
                "op": "panic",
                "message": boundary(message),
                "cleanup": encode_edge(cleanup),
            }),
            SemTerminator::Trap { kind } => {
                serde_json::json!({ "op": "trap", "trap": trap_name(*kind) })
            }
            SemTerminator::CheckedRaiseFault { kind, cleanup } => serde_json::json!({
                "op": "checked_raise",
                "trap": trap_name(*kind),
                "cleanup": encode_edge(cleanup),
            }),
            SemTerminator::CleanupDispatch { normal, fault } => serde_json::json!({
                "op": "cleanup.dispatch",
                "normal": encode_edge(normal),
                "fault": encode_edge(fault),
            }),
            SemTerminator::ResumeUnwind => serde_json::json!({ "op": "resume_unwind" }),
            SemTerminator::EnterDefer { defer, park, body } => serde_json::json!({
                "op": "enter_defer",
                "defer": defer.0,
                "park": park.0,
                "body": encode_edge(body),
            }),
            SemTerminator::FinishDefer { defer, park, next } => serde_json::json!({
                "op": "finish_defer",
                "defer": defer.0,
                "park": park.0,
                "next": encode_edge(next),
            }),
            SemTerminator::RecoverFault {
                result,
                deadline_variant,
                fault_variant,
                normal,
                unwind,
            } => serde_json::json!({
                "op": "recover_fault",
                "result": value_def(result),
                "deadline_variant": deadline_variant,
                "fault_variant": fault_variant,
                "normal": encode_edge(normal),
                "unwind": encode_edge(unwind),
            }),
            SemTerminator::Suspend {
                kind,
                inputs,
                result,
                resumes,
                cancel,
                unwind,
            } => {
                let (name, detail) = suspend_shape(kind)?;
                self.suspend_kind_id(name);
                serde_json::json!({
                    "op": "suspend",
                    "kind": name,
                    "detail": detail,
                    "inputs": boundaries(inputs),
                    "result": call_result(result),
                    "resumes": resumes.iter().map(encode_edge).collect::<Vec<_>>(),
                    "cancel": encode_edge(cancel),
                    "unwind": encode_edge(unwind),
                })
            }

            SemTerminator::ActorCall { .. } => {
                return Err(EmitError::new(
                    "an actor operation reached the sequential package walker",
                ))
            }
            SemTerminator::WireCodec { .. } => {
                return Err(EmitError::new(
                    "wire codec calls have no sandbox package table",
                ))
            }
        };
        encoded["span"] = span;
        Ok(encoded)
    }

    /// The source byte a site names, as a caret.
    ///
    /// SIR's debug facts carry an offset per operation site, not an extent, so
    /// the package reports the point rather than inventing a width.
    fn span(&self, provenance: &Provenance) -> serde_json::Value {
        match self.module.debug.site_offset(provenance) {
            Some(offset) => serde_json::json!({ "start": offset, "end": offset }),
            None => serde_json::Value::Null,
        }
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
        hew_sir::PlaceOrigin::ActorState { field, .. } => {
            encoded.origin = "actor_state".to_string();
            encoded.field = Some(*field);
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

fn call_result(result: &CallResult) -> serde_json::Value {
    match result {
        CallResult::Unit => serde_json::Value::Null,
        CallResult::Never => "never".into(),
        CallResult::Value(def) => value_def(def),
    }
}

fn encode_edge(edge: &Edge) -> serde_json::Value {
    serde_json::json!({ "to": edge.target.0, "args": operands(&edge.args) })
}

fn encode_unwind(unwind: &CallUnwind) -> serde_json::Value {
    match unwind {
        CallUnwind::NotApplicable => serde_json::Value::Null,
        CallUnwind::Cleanup(target) => encode_edge(target),
    }
}

fn block_arg(arg: &hew_sir::BlockArg) -> Value {
    Value {
        value: arg.value.0,
        own: own_name(arg.own).to_string(),
    }
}

fn value_def(def: &ValueDef) -> serde_json::Value {
    serde_json::json!({ "value": def.id.0, "own": own_name(def.own) })
}

fn operand(operand: &Operand) -> u32 {
    operand.value.0
}

fn operands(operands: &[Operand]) -> Vec<u32> {
    operands.iter().map(|value| value.value.0).collect()
}

fn boundary(operand: &BoundaryOperand) -> serde_json::Value {
    serde_json::json!({
        "value": operand.operand.value.0,
        "decision": decision_name(operand.decision),
    })
}

fn boundaries(operands: &[BoundaryOperand]) -> Vec<serde_json::Value> {
    operands.iter().map(boundary).collect()
}

fn shape_ref(shape: AggregateShapeRef) -> serde_json::Value {
    match shape {
        AggregateShapeRef::Tuple => serde_json::Value::Null,
        AggregateShapeRef::Record(id) => id.0.into(),
    }
}

const fn own_name(own: OwnKind) -> &'static str {
    match own {
        OwnKind::None => "none",
        OwnKind::Owned => "owned",
        OwnKind::Guaranteed => "guaranteed",
    }
}

const fn decision_name(decision: BoundaryDecision) -> &'static str {
    match decision {
        BoundaryDecision::Borrow => "borrow",
        BoundaryDecision::BorrowMut => "borrow_mut",
        BoundaryDecision::Copy => "copy",
        BoundaryDecision::Move => "move",
        BoundaryDecision::Snapshot(_) => "snapshot",
    }
}

/// How a parameter crosses a call boundary, as the package spells a boundary
/// operand's decision.
const fn passing_name(passing: hew_sir::SemParamPassing) -> &'static str {
    match passing {
        hew_sir::SemParamPassing::ReadOnly => "copy",
        hew_sir::SemParamPassing::Borrow => "borrow",
        hew_sir::SemParamPassing::BorrowMut => "borrow_mut",
        hew_sir::SemParamPassing::Consume => "move",
    }
}

const fn capability_name(capability: hew_types::ValueCapability) -> &'static str {
    match capability {
        hew_types::ValueCapability::Hash => "Hash",
        hew_types::ValueCapability::Eq => "Eq",
    }
}

/// SIR's trap kinds carried into the trace schema's vocabulary.
///
/// `SignedMinDivNegOne` is an overflow: dividing the signed minimum by -1 has
/// no representable result, and native execution exits with the overflow
/// status.
const fn trap_name(kind: TrapKind) -> &'static str {
    match kind {
        TrapKind::IntegerOverflow | TrapKind::SignedMinDivNegOne => "integer_overflow",
        TrapKind::DivideByZero => "divide_by_zero",
        TrapKind::ShiftOutOfRange => "shift_out_of_range",
        TrapKind::IndexOutOfBounds => "vector_bounds",
    }
}

fn suspend_shape(kind: &SuspendKind) -> Result<(&'static str, serde_json::Value), EmitError> {
    match kind {
        SuspendKind::Sleep => Ok(("Sleep", serde_json::Value::Null)),
        SuspendKind::NativeIo { operation } => Ok((
            "NativeIo",
            serde_json::json!({ "operation": format!("{operation:?}") }),
        )),
        _ => Err(EmitError::new(
            "a scheduler suspension reached the sequential package walker",
        )),
    }
}
