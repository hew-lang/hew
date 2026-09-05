//! Target-realized MIR consumed by native code generation.
//!
//! This is the only IR below ownership SIR. It records concrete storage,
//! layouts, private call ABI transfers, and CFG edges. Ownership has already
//! been decided by SIR: this lowering resolves each explicit copy or destroy
//! exactly once to a physical action and never infers another lifetime.

use std::collections::{BTreeMap, BTreeSet};

use hew_parser::ast::{BinaryOp, UnaryOp};
use hew_sir::{
    AggregateShapeRef, BoundaryDecision, CallResult, CallUnwind, Edge, SemFunction, SemModule,
    SemOp, SemOpKind, SemTerminator, SnapshotDecision, ValueId,
};
pub use hew_sir::{BlockId, CallableId, OwnKind, TrapKind};
use hew_types::{
    CloneKind, EntryExitPlan, ResolvedTy, RuntimeArgumentEffect, RuntimeCallFamily,
    RuntimeResultEffect, TypeInstanceKey,
};

/// Function-local identity of one concrete storage allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StorageId(pub u32);

/// Module-local identity of one verified aggregate layout and glue recipe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalAggregateId(pub u32);

/// One exact demanded aggregate descriptor in the physical type inventory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalAggregateDescriptor {
    pub ty: ResolvedTy,
    pub fields: Vec<ResolvedTy>,
}

/// Concrete types and aggregate shapes demanded by verified SIR bodies.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PhysicalTypeInventory {
    types: BTreeSet<ResolvedTy>,
    aggregates: BTreeMap<ResolvedTy, PhysicalAggregateDescriptor>,
}

impl PhysicalTypeInventory {
    #[must_use]
    pub fn contains(&self, ty: &ResolvedTy) -> bool {
        self.types.contains(ty)
    }

    pub fn types(&self) -> impl Iterator<Item = &ResolvedTy> {
        self.types.iter()
    }

    pub fn aggregates(&self) -> impl Iterator<Item = &PhysicalAggregateDescriptor> {
        self.aggregates.values()
    }

    #[must_use]
    pub fn aggregate(&self, ty: &ResolvedTy) -> Option<&PhysicalAggregateDescriptor> {
        self.aggregates.get(ty)
    }
}

/// LLVM-independent carrier chosen by the target layout resolver.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PhysicalRepr {
    Unit,
    Integer { bits: u16 },
    Float { bits: u16 },
    Pointer,
    Struct(Vec<PhysicalLayout>),
}

/// Concrete target layout for one closed semantic type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalLayout {
    pub size: u64,
    pub align: u32,
    pub repr: PhysicalRepr,
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
}

impl PhysicalTarget {
    #[must_use]
    pub fn new(triple: impl Into<String>, data_layout: impl Into<String>) -> Self {
        Self {
            triple: triple.into(),
            data_layout: data_layout.into(),
            layouts: BTreeMap::new(),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StorageOrigin {
    Parameter(ValueId),
    BlockArgument(ValueId),
    Value(ValueId),
    Place(hew_sir::PlaceId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalStorage {
    pub id: StorageId,
    pub ty: ResolvedTy,
    pub layout: PhysicalLayout,
    pub own: OwnKind,
    pub origin: StorageOrigin,
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
    pub symbol: String,
    pub params: Vec<PhysicalParam>,
    pub return_ty: ResolvedTy,
    pub return_layout: Option<PhysicalLayout>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PhysicalConst {
    I64(i64),
    Bool(bool),
    F64(f64),
    Char(char),
    Unit,
    Duration(i64),
    String(hew_sir::StringLiteralId),
    Bytes(hew_sir::BytesLiteralId),
}

/// A clone selected once from an explicit SIR copy plus concrete type facts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CloneAction {
    Bitwise,
    StringRetain,
    BytesRetain,
    Aggregate(PhysicalAggregateId),
}

/// A release selected once from an explicit SIR destroy plus concrete type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DestroyAction {
    StringRelease,
    BytesRelease,
    Aggregate(PhysicalAggregateId),
}

/// One field in an aggregate's physical copy/drop recipe.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalAggregateField {
    pub ty: ResolvedTy,
    pub own: OwnKind,
    pub clone: Option<CloneAction>,
    pub destroy: Option<DestroyAction>,
}

/// Shared physical glue for one exact concrete aggregate type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalAggregateGlue {
    pub id: PhysicalAggregateId,
    pub ty: ResolvedTy,
    pub fields: Vec<PhysicalAggregateField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PhysicalOp {
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
    AggregateProjectCopy {
        dest: StorageId,
        aggregate: StorageId,
        field: u32,
        glue: PhysicalAggregateId,
        action: CloneAction,
    },
    AggregateDestructure {
        aggregate: StorageId,
        fields: Vec<StorageId>,
        glue: PhysicalAggregateId,
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
        destroy_old: DestroyAction,
    },
    StorageDead {
        storage: StorageId,
        destroy: DestroyAction,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalEdge {
    pub target: BlockId,
    /// Parallel moves from predecessor storage into target block-argument
    /// storage. No clone is implicit in an edge transfer.
    pub transfers: Vec<(StorageId, StorageId)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgumentTransfer {
    Borrow(StorageId),
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

/// Exact no-unwind runtime ABI operation selected from a verified SIR runtime
/// family. The emitter executes this closed physical action; it never selects
/// ownership or failure behaviour from a linker symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PhysicalRuntimeAction {
    StringConcat,
    StringEquals,
    StringToBytesOwned,
    StringToUppercase,
    U8ToString,
    PrintlnI64,
    PrintlnString,
    BytesLen,
    BytesIndex,
    BytesPushOwned,
}

impl PhysicalRuntimeAction {
    const fn semantic_family(self) -> RuntimeCallFamily {
        match self {
            Self::StringConcat => RuntimeCallFamily::StringConcat,
            Self::StringEquals => RuntimeCallFamily::StringEquals,
            Self::StringToBytesOwned => RuntimeCallFamily::StringToBytes,
            Self::StringToUppercase => RuntimeCallFamily::StringToUppercase,
            Self::U8ToString => RuntimeCallFamily::U8ToString,
            Self::PrintlnI64 => RuntimeCallFamily::PrintlnI64,
            Self::PrintlnString => RuntimeCallFamily::PrintlnString,
            Self::BytesLen => RuntimeCallFamily::BytesLen,
            Self::BytesIndex => RuntimeCallFamily::BytesIndex,
            Self::BytesPushOwned => RuntimeCallFamily::BytesPush,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PhysicalTerminator {
    Return {
        value: Option<ReturnTransfer>,
    },
    Goto(PhysicalEdge),
    Branch {
        condition: StorageId,
        then_target: PhysicalEdge,
        else_target: PhysicalEdge,
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
    /// `normal`, where `result` (when present) is initialized.
    Call {
        callee: CallableId,
        args: Vec<ArgumentTransfer>,
        result: Option<StorageId>,
        normal: PhysicalEdge,
        unwind: Option<PhysicalEdge>,
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
    pub blocks: Vec<PhysicalBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PhysicalModule {
    pub target: PhysicalTarget,
    pub aggregate_glue: Vec<PhysicalAggregateGlue>,
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
pub fn lower_physical_module(
    module: &SemModule,
    target: PhysicalTarget,
) -> Result<VerifiedPhysicalModule, PhysicalError> {
    if let Some(diagnostic) = hew_sir::verify_module(module).into_iter().next() {
        return Err(PhysicalError::new(format!(
            "SIR verification failed before physical lowering: {:?}",
            diagnostic.kind
        )));
    }

    let (aggregate_glue, aggregate_ids) = build_aggregate_glue(module)?;

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
                        carrier: if matches!(
                            required_layout(&target, &param.ty)?.repr,
                            PhysicalRepr::Struct(_)
                        ) {
                            ParamCarrier::Indirect
                        } else {
                            ParamCarrier::Direct
                        },
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            let return_layout = if callable.signature.return_ty == ResolvedTy::Unit {
                None
            } else {
                Some(required_layout(&target, &callable.signature.return_ty)?.clone())
            };
            Ok(PhysicalCallable {
                id: callable.id,
                symbol: callable.symbol.clone(),
                params,
                return_ty: callable.signature.return_ty.clone(),
                return_layout,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;

    let functions = module
        .functions
        .iter()
        .map(|function| lower_function(module, &target, function, &aggregate_ids))
        .collect::<Result<Vec<_>, _>>()?;

    let physical = PhysicalModule {
        target,
        aggregate_glue,
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

fn build_aggregate_glue(
    module: &SemModule,
) -> Result<
    (
        Vec<PhysicalAggregateGlue>,
        BTreeMap<ResolvedTy, PhysicalAggregateId>,
    ),
    PhysicalError,
> {
    let inventory = physical_type_inventory(module);
    let owned_aggregates = inventory
        .aggregates()
        .map(|aggregate| {
            OwnKind::of_ty(&aggregate.ty, &module.type_facts)
                .map(|own| (aggregate, own))
                .map_err(PhysicalError::new)
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .filter_map(|(aggregate, own)| (own == OwnKind::Owned).then_some(aggregate))
        .collect::<Vec<_>>();
    let aggregate_ids = owned_aggregates
        .iter()
        .enumerate()
        .map(|(index, aggregate)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical aggregate count exceeds u32"))?;
            Ok((aggregate.ty.clone(), PhysicalAggregateId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let glue = owned_aggregates
        .into_iter()
        .map(|aggregate| {
            let id = aggregate_ids[&aggregate.ty];
            let shape = aggregate_shape_ref(module, &aggregate.ty)?;
            let recipes = hew_sir::aggregate_field_recipes(
                shape,
                &aggregate.ty,
                &module.aggregate_shapes,
                &module.type_facts,
            )
            .map_err(PhysicalError::new)?;
            let fields = recipes
                .into_iter()
                .map(|recipe| {
                    Ok(PhysicalAggregateField {
                        clone: clone_action_for_type(&recipe.ty, recipe.clone, &aggregate_ids)?,
                        destroy: destroy_action_for_type(&recipe.ty, &aggregate_ids),
                        ty: recipe.ty,
                        own: recipe.own,
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            Ok(PhysicalAggregateGlue {
                id,
                ty: aggregate.ty.clone(),
                fields,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    Ok((glue, aggregate_ids))
}

fn aggregate_shape_ref(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<AggregateShapeRef, PhysicalError> {
    match ty {
        ResolvedTy::Tuple(fields) if !fields.is_empty() => Ok(AggregateShapeRef::Tuple),
        _ => module
            .aggregate_shape_for_type(ty)
            .map(|shape| AggregateShapeRef::Record(shape.id))
            .ok_or_else(|| {
                PhysicalError::new(format!(
                    "owned aggregate `{}` has no exact SIR shape descriptor",
                    ty.user_facing()
                ))
            }),
    }
}

fn clone_action_for_type(
    ty: &ResolvedTy,
    clone: CloneKind,
    aggregate_ids: &BTreeMap<ResolvedTy, PhysicalAggregateId>,
) -> Result<Option<CloneAction>, PhysicalError> {
    let action = match clone {
        CloneKind::None => return Ok(None),
        CloneKind::Bits => CloneAction::Bitwise,
        CloneKind::Retain if ty == &ResolvedTy::String => CloneAction::StringRetain,
        CloneKind::Retain if ty == &ResolvedTy::Bytes => CloneAction::BytesRetain,
        CloneKind::FieldWise => {
            CloneAction::Aggregate(*aggregate_ids.get(ty).ok_or_else(|| {
                PhysicalError::new(format!(
                    "field-wise aggregate `{}` has no demanded physical descriptor",
                    ty.user_facing()
                ))
            })?)
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

fn destroy_action_for_type(
    ty: &ResolvedTy,
    aggregate_ids: &BTreeMap<ResolvedTy, PhysicalAggregateId>,
) -> Option<DestroyAction> {
    match ty {
        ResolvedTy::String => Some(DestroyAction::StringRelease),
        ResolvedTy::Bytes => Some(DestroyAction::BytesRelease),
        _ => aggregate_ids.get(ty).copied().map(DestroyAction::Aggregate),
    }
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
        }
    }
    let mut inventory = PhysicalTypeInventory {
        types,
        aggregates: BTreeMap::new(),
    };
    let demanded = inventory.types.iter().cloned().collect::<Vec<_>>();
    for ty in demanded {
        collect_inventory_aggregate(module, &mut inventory, &ty);
    }
    inventory
}

fn collect_inventory_aggregate(
    module: &SemModule,
    inventory: &mut PhysicalTypeInventory,
    ty: &ResolvedTy,
) {
    if !inventory.types.insert(ty.clone()) && inventory.aggregates.contains_key(ty) {
        return;
    }
    let fields = match ty {
        ResolvedTy::Tuple(fields) if !fields.is_empty() => Some(fields.clone()),
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
        collect_inventory_aggregate(module, inventory, &field);
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
    aggregate_ids: &'a BTreeMap<ResolvedTy, PhysicalAggregateId>,
    values: BTreeMap<ValueId, StorageId>,
    places: BTreeMap<hew_sir::PlaceId, StorageId>,
    storage: Vec<PhysicalStorage>,
}

fn lower_function(
    module: &SemModule,
    target: &PhysicalTarget,
    function: &SemFunction,
    aggregate_ids: &BTreeMap<ResolvedTy, PhysicalAggregateId>,
) -> Result<PhysicalFunction, PhysicalError> {
    let mut lowerer = FunctionLowerer {
        module,
        target,
        function,
        aggregate_ids,
        values: BTreeMap::new(),
        places: BTreeMap::new(),
        storage: Vec::new(),
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
            own: OwnKind::of_ty(&place.ty, &module.type_facts).map_err(PhysicalError::new)?,
            origin: StorageOrigin::Place(place.id),
        });
    }

    let blocks = function
        .blocks
        .iter()
        .map(|block| {
            let arguments = block
                .args
                .iter()
                .map(|argument| lowerer.value(argument.value))
                .collect::<Result<Vec<_>, _>>()?;
            let ops = block
                .ops
                .iter()
                .map(|operation| lowerer.lower_op(operation))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect();
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
        | SemTerminator::RtCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::CheckedBinary { result, .. } => Some(result),
        _ => None,
    }
}

fn physical_runtime_action(
    family: RuntimeCallFamily,
) -> Result<PhysicalRuntimeAction, PhysicalError> {
    Ok(match family {
        RuntimeCallFamily::StringConcat => PhysicalRuntimeAction::StringConcat,
        RuntimeCallFamily::StringEquals => PhysicalRuntimeAction::StringEquals,
        RuntimeCallFamily::StringToBytes => PhysicalRuntimeAction::StringToBytesOwned,
        RuntimeCallFamily::StringToUppercase => PhysicalRuntimeAction::StringToUppercase,
        RuntimeCallFamily::U8ToString => PhysicalRuntimeAction::U8ToString,
        RuntimeCallFamily::PrintlnI64 => PhysicalRuntimeAction::PrintlnI64,
        RuntimeCallFamily::PrintlnString => PhysicalRuntimeAction::PrintlnString,
        RuntimeCallFamily::BytesLen => PhysicalRuntimeAction::BytesLen,
        RuntimeCallFamily::BytesIndex => PhysicalRuntimeAction::BytesIndex,
        RuntimeCallFamily::BytesPush => PhysicalRuntimeAction::BytesPushOwned,
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
    fn lower_op(&self, operation: &SemOp) -> Result<Vec<PhysicalOp>, PhysicalError> {
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
            SemOpKind::ConstI64(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::I64(*value),
            }),
            SemOpKind::ConstBool(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::Bool(*value),
            }),
            SemOpKind::ConstF64(value) => one(PhysicalOp::Const {
                dest: self.one_result(operation)?,
                value: PhysicalConst::F64(*value),
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
                one(PhysicalOp::Assign {
                    dest,
                    source: self.value(value.value)?,
                    destroy_old: self.destroy_action(&self.storage[dest.0 as usize].ty)?,
                })
            }
            SemOpKind::EndLifetime { place } => {
                Self::no_results(operation)?;
                let storage = self.place(*place)?;
                one(PhysicalOp::StorageDead {
                    storage,
                    destroy: self.destroy_action(&self.storage[storage.0 as usize].ty)?,
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
        Ok(PhysicalEdge {
            target: edge.target,
            transfers,
        })
    }

    fn lower_terminator(
        &self,
        terminator: &SemTerminator,
    ) -> Result<PhysicalTerminator, PhysicalError> {
        match terminator {
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
            SemTerminator::CheckedBinary {
                op,
                lhs,
                rhs,
                result,
                normal,
                failures,
                ..
            } => Ok(PhysicalTerminator::CheckedBinary {
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
            }),
            SemTerminator::Call {
                callee,
                args,
                result,
                normal,
                unwind,
                ..
            } => Ok(PhysicalTerminator::Call {
                callee: *callee,
                args: args
                    .iter()
                    .map(|argument| {
                        self.argument_transfer(argument.operand.value, argument.decision)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                result: match result {
                    CallResult::Unit => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: self.lower_edge(normal)?,
                unwind: match unwind {
                    CallUnwind::NotApplicable => None,
                    CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
                },
            }),
            SemTerminator::RtCall {
                family,
                args,
                result,
                normal,
                unwind,
                ..
            } => Ok(PhysicalTerminator::RuntimeCall {
                action: physical_runtime_action(*family)?,
                args: args
                    .iter()
                    .map(|argument| {
                        self.argument_transfer(argument.operand.value, argument.decision)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                result: match result {
                    CallResult::Unit => None,
                    CallResult::Value(value) => Some(self.value(value.id)?),
                },
                normal: self.lower_edge(normal)?,
                failure: match unwind {
                    CallUnwind::NotApplicable => None,
                    CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
                },
            }),
            SemTerminator::Trap { kind } => Ok(PhysicalTerminator::Trap(*kind)),
            SemTerminator::ResumeUnwind => Ok(PhysicalTerminator::PropagateFault),
            SemTerminator::Unreachable => Ok(PhysicalTerminator::Unreachable),
            SemTerminator::Suspend { .. } => Err(PhysicalError::new(
                "runtime and suspending calls need explicit status ABI wrappers",
            )),
        }
    }

    fn argument_transfer(
        &self,
        value: ValueId,
        decision: BoundaryDecision,
    ) -> Result<ArgumentTransfer, PhysicalError> {
        let source = self.value(value)?;
        Ok(match decision {
            BoundaryDecision::Borrow => ArgumentTransfer::Borrow(source),
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
        clone_action_for_type(ty, facts.clone, self.aggregate_ids)?.ok_or_else(|| {
            PhysicalError::new(format!(
                "physical copy of `{}` has no admitted clone action",
                ty.user_facing()
            ))
        })
    }

    fn destroy_action(&self, ty: &ResolvedTy) -> Result<DestroyAction, PhysicalError> {
        destroy_action_for_type(ty, self.aggregate_ids).ok_or_else(|| {
            PhysicalError::new(format!(
                "physical destroy action for `{}` is not implemented",
                ty.user_facing()
            ))
        })
    }

    fn aggregate_id(&self, ty: &ResolvedTy) -> Result<PhysicalAggregateId, PhysicalError> {
        self.aggregate_ids.get(ty).copied().ok_or_else(|| {
            PhysicalError::new(format!(
                "owned aggregate `{}` has no physical glue identity",
                ty.user_facing()
            ))
        })
    }
}

fn verify_physical_module(module: &PhysicalModule) -> Result<(), PhysicalError> {
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
            if let Some(action) = field.clone {
                verify_clone_action(module, &field.ty, field.own, action)?;
            }
            if let Some(action) = field.destroy {
                verify_destroy_action(module, &field.ty, field.own, action)?;
            }
        }
    }
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

fn verify_physical_function(
    module: &PhysicalModule,
    function: &PhysicalFunction,
) -> Result<(), PhysicalError> {
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
    verify_initialization(function)?;
    Ok(())
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

fn verify_clone_action(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    own: OwnKind,
    action: CloneAction,
) -> Result<(), PhysicalError> {
    let valid = match action {
        CloneAction::Bitwise => own == OwnKind::None,
        CloneAction::StringRetain => ty == &ResolvedTy::String && own == OwnKind::Owned,
        CloneAction::BytesRetain => ty == &ResolvedTy::Bytes && own == OwnKind::Owned,
        CloneAction::Aggregate(id) => {
            let glue = aggregate_glue(module, id)?;
            glue.ty == *ty
                && own == OwnKind::Owned
                && glue.fields.iter().all(|field| field.clone.is_some())
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
    let valid = match action {
        DestroyAction::StringRelease => ty == &ResolvedTy::String && own == OwnKind::Owned,
        DestroyAction::BytesRelease => ty == &ResolvedTy::Bytes && own == OwnKind::Owned,
        DestroyAction::Aggregate(id) => {
            let glue = aggregate_glue(module, id)?;
            glue.ty == *ty
                && own == OwnKind::Owned
                && glue
                    .fields
                    .iter()
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
    if destination.ty != recipe.ty || destination.own != OwnKind::Owned {
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
    for (index, (field, expected)) in fields.iter().zip(&recipe.fields).enumerate() {
        let field = storage(function, *field)?;
        if field.ty != expected.ty || field.own != expected.own {
            return Err(PhysicalError::new(format!(
                "physical aggregate construction field {index} disagrees with its glue recipe"
            )));
        }
    }
    Ok(())
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
    let aggregate = storage(function, aggregate)?;
    let destination = storage(function, dest)?;
    let recipe = aggregate_glue(module, glue)?;
    if aggregate.ty != recipe.ty || !matches!(aggregate.own, OwnKind::Owned | OwnKind::Guaranteed) {
        return Err(PhysicalError::new(
            "physical aggregate projection source disagrees with its glue recipe",
        ));
    }
    let expected = usize::try_from(field)
        .ok()
        .and_then(|index| recipe.fields.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical aggregate projection index {field} is out of bounds"
            ))
        })?;
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

fn verify_aggregate_destructure(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    aggregate: StorageId,
    fields: &[StorageId],
    glue: PhysicalAggregateId,
) -> Result<(), PhysicalError> {
    let aggregate = storage(function, aggregate)?;
    let recipe = aggregate_glue(module, glue)?;
    if aggregate.ty != recipe.ty || aggregate.own != OwnKind::Owned {
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

fn verify_operation_storage(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    operation: &PhysicalOp,
) -> Result<(), PhysicalError> {
    match operation {
        PhysicalOp::Const { dest, .. } | PhysicalOp::StorageLive { storage: dest } => {
            storage(function, *dest)?;
        }
        PhysicalOp::Unary { dest, source, .. }
        | PhysicalOp::Transfer { dest, source }
        | PhysicalOp::Borrow { dest, source } => {
            require_same_storage_type(function, *dest, *source, "physical operation")?;
        }
        PhysicalOp::Cast { dest, source, .. } => {
            storage(function, *dest)?;
            storage(function, *source)?;
        }
        PhysicalOp::TupleMake { dest, elements } => verify_tuple_make(function, *dest, elements)?,
        PhysicalOp::TupleGet { dest, tuple, index } => {
            verify_tuple_get(function, *dest, *tuple, *index)?;
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
        PhysicalOp::AggregateDestructure {
            aggregate,
            fields,
            glue,
        } => verify_aggregate_destructure(module, function, *aggregate, fields, *glue)?,
        PhysicalOp::Binary { dest, lhs, rhs, .. } => {
            storage(function, *dest)?;
            require_same_storage_type(function, *lhs, *rhs, "physical binary operation")?;
        }
        PhysicalOp::Destroy { source, action } => {
            let source = storage(function, *source)?;
            verify_destroy_action(module, &source.ty, source.own, *action)?;
        }
        PhysicalOp::EndBorrow { source } => {
            storage(function, *source)?;
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
            dest, destroy_old, ..
        } => {
            let destination = storage(function, *dest)?;
            verify_destroy_action(module, &destination.ty, destination.own, *destroy_old)?;
        }
        PhysicalOp::StorageDead {
            storage: id,
            destroy,
        } => {
            let source = storage(function, *id)?;
            verify_destroy_action(module, &source.ty, source.own, *destroy)?;
        }
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
    fault: FaultState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FaultState {
    None,
    Active,
    MaybeActive,
}

fn verify_initialization(function: &PhysicalFunction) -> Result<(), PhysicalError> {
    let blocks = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<BTreeMap<_, _>>();
    let mut entry = FlowState {
        slots: vec![InitState::Uninitialized; function.storage.len()],
        fault: FaultState::None,
    };
    for parameter in &function.parameters {
        entry.slots[parameter.0 as usize] = InitState::Initialized;
    }

    let mut incoming = BTreeMap::from([(function.entry, entry)]);
    let mut pending = vec![function.entry];
    while let Some(block_id) = pending.pop() {
        let block = blocks.get(&block_id).ok_or_else(|| {
            PhysicalError::new(format!("physical CFG has no block {}", block_id.0))
        })?;
        let mut state = incoming
            .get(&block_id)
            .cloned()
            .expect("pending physical block always has an incoming state");
        for operation in &block.ops {
            apply_operation(function, operation, &mut state, block_id)?;
        }
        for (target, successor) in
            terminator_successors(function, &block.terminator, state, block_id)?
        {
            let changed = if let Some(existing) = incoming.get_mut(&target) {
                merge_flow(existing, &successor)
            } else {
                incoming.insert(target, successor);
                true
            };
            if changed {
                pending.push(target);
            }
        }
    }
    Ok(())
}

fn initialized(
    state: &FlowState,
    id: StorageId,
    block: BlockId,
    context: &str,
) -> Result<(), PhysicalError> {
    match state.slots[id.0 as usize] {
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
    match (state.slots[id.0 as usize], storage(function, id)?.own) {
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
    state.slots[id.0 as usize] = InitState::Initialized;
    Ok(())
}

fn consume_if_owned(
    function: &PhysicalFunction,
    state: &mut FlowState,
    id: StorageId,
) -> Result<(), PhysicalError> {
    if storage(function, id)?.own == OwnKind::Owned {
        state.slots[id.0 as usize] = InitState::Uninitialized;
    }
    Ok(())
}

fn apply_operation(
    function: &PhysicalFunction,
    operation: &PhysicalOp,
    state: &mut FlowState,
    block: BlockId,
) -> Result<(), PhysicalError> {
    match operation {
        PhysicalOp::Const { dest, .. } | PhysicalOp::StorageLive { storage: dest } => {
            if matches!(operation, PhysicalOp::Const { .. }) {
                define(function, state, *dest, block, "constant")?;
            } else if state.slots[dest.0 as usize] != InitState::Uninitialized {
                return Err(PhysicalError::new(format!(
                    "physical bb{} starts the lifetime of initialized storage {}",
                    block.0, dest.0
                )));
            }
        }
        PhysicalOp::Unary { dest, source, .. } | PhysicalOp::Cast { dest, source, .. } => {
            initialized(state, *source, block, "operation")?;
            define(function, state, *dest, block, "operation")?;
        }
        PhysicalOp::TupleMake { dest, elements } => {
            for element in elements {
                initialized(state, *element, block, "tuple construction")?;
            }
            define(function, state, *dest, block, "tuple construction")?;
        }
        PhysicalOp::TupleGet { dest, tuple, .. } => {
            initialized(state, *tuple, block, "tuple projection")?;
            define(function, state, *dest, block, "tuple projection")?;
        }
        PhysicalOp::AggregateMake { dest, fields, .. } => {
            for field in fields {
                initialized(state, *field, block, "aggregate construction")?;
            }
            define(function, state, *dest, block, "aggregate construction")?;
            for field in fields {
                consume_if_owned(function, state, *field)?;
            }
        }
        PhysicalOp::AggregateProjectCopy {
            dest, aggregate, ..
        } => {
            initialized(state, *aggregate, block, "aggregate projection")?;
            define(function, state, *dest, block, "aggregate projection")?;
        }
        PhysicalOp::AggregateDestructure {
            aggregate, fields, ..
        } => {
            initialized(state, *aggregate, block, "aggregate destructure")?;
            for field in fields {
                define(function, state, *field, block, "aggregate destructure")?;
            }
            consume_if_owned(function, state, *aggregate)?;
        }
        PhysicalOp::Binary { dest, lhs, rhs, .. } => {
            initialized(state, *lhs, block, "binary operation")?;
            initialized(state, *rhs, block, "binary operation")?;
            define(function, state, *dest, block, "binary operation")?;
        }
        PhysicalOp::Transfer { dest, source } => {
            initialized(state, *source, block, "transfer")?;
            if dest != source {
                define(function, state, *dest, block, "transfer")?;
                consume_if_owned(function, state, *source)?;
            }
        }
        PhysicalOp::Clone { dest, source, .. } | PhysicalOp::Borrow { dest, source } => {
            initialized(state, *source, block, "copy or borrow")?;
            define(function, state, *dest, block, "copy or borrow")?;
        }
        PhysicalOp::Destroy { source, .. } | PhysicalOp::EndBorrow { source } => {
            initialized(state, *source, block, "destroy or end-borrow")?;
            state.slots[source.0 as usize] = InitState::Uninitialized;
        }
        PhysicalOp::Assign { dest, source, .. } => {
            initialized(state, *dest, block, "assignment destination")?;
            initialized(state, *source, block, "assignment source")?;
            consume_if_owned(function, state, *source)?;
        }
        PhysicalOp::StorageDead { storage: id, .. } => {
            initialized(state, *id, block, "end-lifetime")?;
            state.slots[id.0 as usize] = InitState::Uninitialized;
        }
    }
    Ok(())
}

fn apply_edge(
    function: &PhysicalFunction,
    edge: &PhysicalEdge,
    mut state: FlowState,
    block: BlockId,
) -> Result<(BlockId, FlowState), PhysicalError> {
    let before = state.slots.clone();
    for (source, _) in &edge.transfers {
        match before[source.0 as usize] {
            InitState::Initialized => {}
            InitState::Uninitialized | InitState::MaybeInitialized => {
                initialized(&state, *source, block, "edge transfer")?;
            }
        }
    }
    for (source, destination) in &edge.transfers {
        if source == destination {
            continue;
        }
        define(function, &mut state, *destination, block, "edge transfer")?;
    }
    for (source, destination) in &edge.transfers {
        if source != destination && storage(function, *source)?.own == OwnKind::Owned {
            state.slots[source.0 as usize] = InitState::Uninitialized;
        }
    }
    Ok((edge.target, state))
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
) -> Result<Vec<(BlockId, FlowState)>, PhysicalError> {
    match terminator {
        PhysicalTerminator::Return { value } => {
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
                initialized(&state, id, block, "return")?;
            }
            Ok(vec![])
        }
        PhysicalTerminator::Goto(edge) => Ok(vec![apply_edge(function, edge, state, block)?]),
        PhysicalTerminator::Branch {
            condition,
            then_target,
            else_target,
        } => {
            initialized(&state, *condition, block, "branch")?;
            Ok(vec![
                apply_edge(function, then_target, state.clone(), block)?,
                apply_edge(function, else_target, state, block)?,
            ])
        }
        PhysicalTerminator::CheckedBinary {
            lhs,
            rhs,
            result,
            normal,
            failures,
            ..
        } => {
            initialized(&state, *lhs, block, "checked binary operation")?;
            initialized(&state, *rhs, block, "checked binary operation")?;

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
                successors.push(apply_edge(function, &failure.edge, state.clone(), block)?);
            }
            Ok(successors)
        }
        PhysicalTerminator::Call {
            args,
            result,
            normal,
            unwind,
            ..
        } => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} issues a call while an earlier fault is active",
                    block.0
                )));
            }
            for argument in args {
                let (source, moves) = match argument {
                    ArgumentTransfer::Borrow(source) | ArgumentTransfer::Clone { source, .. } => {
                        (*source, false)
                    }
                    ArgumentTransfer::Move(source) => (*source, true),
                };
                initialized(&state, source, block, "call argument")?;
                if moves {
                    consume_if_owned(function, &mut state, source)?;
                }
            }
            let mut normal_state = state.clone();
            if let Some(result) = result {
                define(function, &mut normal_state, *result, block, "call result")?;
            }
            let normal_state = apply_edge(function, normal, normal_state, block)?;
            let mut successors = vec![normal_state];
            if let Some(unwind) = unwind {
                let mut failure_state = state;
                if let Some(result) = result {
                    failure_state.slots[result.0 as usize] = InitState::Uninitialized;
                }
                failure_state.fault = FaultState::Active;
                successors.push(apply_edge(function, unwind, failure_state, block)?);
            }
            Ok(successors)
        }
        PhysicalTerminator::RuntimeCall {
            args,
            result,
            normal,
            failure,
            ..
        } => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} issues a runtime call while an earlier fault is active",
                    block.0
                )));
            }
            for argument in args {
                let (source, moves) = match argument {
                    ArgumentTransfer::Borrow(source) | ArgumentTransfer::Clone { source, .. } => {
                        (*source, false)
                    }
                    ArgumentTransfer::Move(source) => (*source, true),
                };
                initialized(&state, source, block, "runtime call argument")?;
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
            let mut successors = vec![apply_edge(function, normal, normal_state, block)?];
            if let Some(failure) = failure {
                successors.push(apply_edge(function, failure, state, block)?);
            }
            Ok(successors)
        }
        PhysicalTerminator::Trap(_) | PhysicalTerminator::Unreachable => Ok(vec![]),
        PhysicalTerminator::PropagateFault => {
            if state.fault != FaultState::Active {
                return Err(PhysicalError::new(format!(
                    "physical bb{} propagates a fault that is not initialized",
                    block.0
                )));
            }
            Ok(vec![])
        }
    }
}

fn merge_flow(existing: &mut FlowState, incoming: &FlowState) -> bool {
    let mut changed = false;
    for (left, right) in existing.slots.iter_mut().zip(&incoming.slots) {
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
        if blocks.contains(&edge.target) {
            for (source, destination) in &edge.transfers {
                if slot(*source)?.ty != slot(*destination)?.ty {
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
        PhysicalTerminator::Return { value } => {
            let returned = value.map(|value| match value {
                ReturnTransfer::Borrow(id)
                | ReturnTransfer::Move(id)
                | ReturnTransfer::Clone { source: id, .. } => id,
            });
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
        PhysicalTerminator::CheckedBinary {
            op,
            lhs,
            rhs,
            result,
            normal,
            failures,
        } => {
            require_same_storage_type(function, *lhs, *rhs, "physical checked binary")?;
            require_same_storage_type(function, *lhs, *result, "physical checked binary result")?;
            let ty = &slot(*lhs)?.ty;
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
                    | ArgumentTransfer::Move(id)
                    | ArgumentTransfer::Clone { source: id, .. } => *id,
                };
                if slot(id)?.ty != parameter.ty {
                    return Err(PhysicalError::new(
                        "physical call argument type disagrees with callee ABI",
                    ));
                }
            }
            match (&callee.return_ty, result) {
                (ResolvedTy::Unit, None) => {}
                (ResolvedTy::Unit, Some(_)) | (_, None) => {
                    return Err(PhysicalError::new(
                        "physical call result-out presence disagrees with callee ABI",
                    ));
                }
                (expected, Some(id)) if &slot(*id)?.ty == expected => {}
                _ => {
                    return Err(PhysicalError::new(
                        "physical call result storage type disagrees with callee ABI",
                    ));
                }
            }
            edge(normal)?;
            if let Some(unwind) = unwind {
                edge(unwind)?;
            }
            Ok(())
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
            for (argument, expected) in args.iter().zip(contract.arguments) {
                let (id, actual_effect) = match argument {
                    ArgumentTransfer::Borrow(id) => (*id, RuntimeArgumentEffect::Borrow),
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
                if actual_effect != expected.effect || !expected.ty.matches(&slot(id)?.ty) {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} argument disagrees with its semantic contract"
                    )));
                }
            }
            match (contract.result, result) {
                (RuntimeResultEffect::Unit, None) => {}
                (RuntimeResultEffect::Unit, Some(_)) | (_, None) => {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} result presence disagrees with its semantic contract"
                    )));
                }
                (
                    RuntimeResultEffect::BitCopy(kind)
                    | RuntimeResultEffect::FreshOwned(kind)
                    | RuntimeResultEffect::UpdatedReceiver(kind),
                    Some(id),
                ) if kind.matches(&slot(*id)?.ty) => {
                    let expected_own = match contract.result {
                        RuntimeResultEffect::BitCopy(_) => OwnKind::None,
                        RuntimeResultEffect::FreshOwned(_)
                        | RuntimeResultEffect::UpdatedReceiver(_) => OwnKind::Owned,
                        RuntimeResultEffect::Unit => unreachable!(),
                    };
                    if slot(*id)?.own != expected_own {
                        return Err(PhysicalError::new(format!(
                            "physical runtime action {action:?} result ownership disagrees with its semantic contract"
                        )));
                    }
                }
                _ => {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} result type disagrees with its semantic contract"
                    )));
                }
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
        PhysicalTerminator::Trap(_)
        | PhysicalTerminator::PropagateFault
        | PhysicalTerminator::Unreachable => Ok(()),
    }
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

#[cfg(test)]
mod tests {
    use hew_hir::{lower_program_host_target, ItemId, ResolutionCtx};
    use hew_sir::{
        BoundaryOperand, CallableInstance, CheckedFailure, FunctionSourceOrigin, Operand,
        Provenance, SemBlock, SemCallConv, SemCallable, SemCallableKind, SemSignature, ValueDef,
    };
    use hew_types::{
        module_registry::ModuleRegistry, Checker, DefId, SendFact, TypeFacts, ValueClass,
    };

    use super::*;

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

    fn target_with_i64_pair() -> PhysicalTarget {
        let mut target = target();
        target.insert_layout(
            ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
            PhysicalLayout {
                size: 16,
                align: 8,
                repr: PhysicalRepr::Struct(vec![i64_layout(), i64_layout()]),
            },
        );
        target
    }

    fn target_for_inventory(module: &SemModule) -> PhysicalTarget {
        let inventory = physical_type_inventory(module);
        let mut target = target();
        let mut pending = inventory.aggregates().collect::<Vec<_>>();
        while !pending.is_empty() {
            let previous = pending.len();
            pending.retain(|aggregate| {
                let Some(fields) = aggregate
                    .fields
                    .iter()
                    .map(|field| target.layout(field).cloned())
                    .collect::<Option<Vec<_>>>()
                else {
                    return true;
                };
                let align = fields.iter().map(|field| field.align).max().unwrap_or(1);
                let size = fields.iter().map(|field| field.size).sum();
                target.insert_layout(
                    aggregate.ty.clone(),
                    PhysicalLayout {
                        size,
                        align,
                        repr: PhysicalRepr::Struct(fields),
                    },
                );
                false
            });
            assert!(
                pending.len() < previous,
                "test aggregate layouts must be acyclic"
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
                    kind: SemOpKind::ConstI64(7),
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
            callables: vec![callable],
            generic_templates: vec![],
            root_unit_callables: vec![CallableId(0)],
            entry_exit_plan: None,
            entry_callable: Some(CallableId(0)),
            functions: vec![function],
            aggregate_shapes: vec![],
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
                    normal: Edge {
                        target: BlockId(1),
                        args: vec![Operand { value: ValueId(0) }],
                    },
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
                        kind: SemOpKind::ConstI64(40),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: hew_sir::OpId(1),
                        results: vec![ValueDef {
                            id: ValueId(1),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                        kind: SemOpKind::ConstI64(2),
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
            lower_physical_module(&module, target_with_i64_pair()).expect("physical tuple");
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
        assert!(operations.iter().any(|operation| matches!(
            operation,
            PhysicalOp::AggregateProjectCopy {
                action: CloneAction::StringRetain,
                ..
            }
        )));
        assert!(operations.iter().any(|operation| matches!(
            operation,
            PhysicalOp::Destroy {
                action: DestroyAction::Aggregate(_),
                ..
            }
        )));
    }

    #[test]
    fn verifier_refuses_aggregate_projection_with_wrong_copy_action() {
        let module = lower_source(
            r#"
            type Packet { label: string, payload: bytes }
            fn main() {
                let packet = Packet { label: "record", payload: b"P" };
                let label = packet.label;
            }
            "#,
        );
        let mut physical = lower_physical_module(&module, target_for_inventory(&module))
            .expect("valid aggregate physical lowering")
            .into_unverified();
        let action = physical
            .functions
            .iter_mut()
            .flat_map(|function| &mut function.blocks)
            .flat_map(|block| &mut block.ops)
            .find_map(|operation| match operation {
                PhysicalOp::AggregateProjectCopy { action, .. } => Some(action),
                _ => None,
            })
            .expect("aggregate field projection");
        *action = CloneAction::BytesRetain;
        let error = verify_physical_module(&physical)
            .expect_err("aggregate projection must use its exact field recipe");
        assert!(error.message.contains("field copy recipe"));
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

        let mut owned_tuple = lower_physical_module(&module, target_with_i64_pair())
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

        let mut bad_index = lower_physical_module(&module, target_with_i64_pair())
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
                println(upper);
                0
            }
            "#,
        );
        let verified = lower_physical_module(&module, target()).expect("physical runtime calls");
        let actions = verified
            .module()
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .filter_map(|block| match block.terminator {
                PhysicalTerminator::RuntimeCall { action, .. } => Some(action),
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        assert_eq!(
            actions,
            BTreeSet::from([
                PhysicalRuntimeAction::StringEquals,
                PhysicalRuntimeAction::StringToUppercase,
                PhysicalRuntimeAction::PrintlnString,
            ])
        );
    }

    #[test]
    fn scalar_print_lowers_to_the_exact_physical_runtime_action() {
        let module = lower_source("fn main() { println(1 + 2); }");
        let verified = lower_physical_module(&module, target()).expect("physical scalar print");
        assert!(verified
            .module()
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .any(|block| matches!(
                block.terminator,
                PhysicalTerminator::RuntimeCall {
                    action: PhysicalRuntimeAction::PrintlnI64,
                    ..
                }
            )));
    }

    #[test]
    fn bytes_transform_and_bounds_failure_are_physical_contracts() {
        let module = lower_source(include_str!(
            "../../tests/core-acceptance/cases/bytes-copy-mutate.hew"
        ));
        let verified = lower_physical_module(&module, target()).expect("physical bytes calls");
        let actions = verified
            .module()
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .filter_map(|block| match block.terminator {
                PhysicalTerminator::RuntimeCall { action, .. } => Some(action),
                _ => None,
            })
            .collect::<BTreeSet<_>>();
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
        let mut wrong_transfer = lower_physical_module(&module, target())
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

        let mut missing_failure = lower_physical_module(&module, target())
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
        let physical = lower_physical_module(&module, target()).expect("lower");
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
        lower_physical_module(&module, target()).expect("scalar loop should verify physically");
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
        lower_physical_module(&module, target()).expect("owned loop should verify physically");
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
            symbol: "malformed_owner_merge".to_string(),
            params: vec![],
            return_ty: ResolvedTy::Unit,
            return_layout: None,
        };
        let function = PhysicalFunction {
            callable: CallableId(0),
            entry: BlockId(0),
            parameters: vec![],
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
                        },
                        else_target: PhysicalEdge {
                            target: BlockId(2),
                            transfers: vec![],
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
                    }),
                },
                PhysicalBlock {
                    id: BlockId(2),
                    arguments: vec![],
                    ops: vec![],
                    terminator: PhysicalTerminator::Goto(PhysicalEdge {
                        target: BlockId(3),
                        transfers: vec![],
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
            target: physical_target,
            aggregate_glue: vec![],
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
}
