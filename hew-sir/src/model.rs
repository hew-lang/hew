use std::collections::BTreeMap;

use hew_hir::{ItemId, SiteId};
use hew_parser::ast::Span;
use hew_types::{DefId, NominalInstance, ResolvedTy, TypeFacts, TypeInstanceKey};

use crate::ownership::{
    Binding, BindingTarget, BoundaryDecision, BytesLiteralId, OwnKind, PlaceDecl, PlaceId,
    StringLiteralId, SuspendKind, TrapKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpId(pub u32);
/// Stable, module-local identity for a SIR direct-call target.
///
/// IDs are assigned from the deterministic [`SemModule::callables`] order;
/// unlike an emitted symbol, they are not reconstructed from spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallableId(pub u32);

/// Stable semantic identity for one generic HIR template.
///
/// This deliberately contains the resolver-minted declaration identity only.
/// A linker symbol is an emitted-name projection, not part of semantic
/// instance identity: two concrete SIR bodies are deduplicated by this
/// template plus their closed semantic substitutions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericTemplateId {
    pub declaration: DefId,
}

/// Closed semantic specialization of a generic HIR template.
///
/// SIR creates these at the normalized-HIR boundary.  `type_args` are
/// `ResolvedTy` facts only; no layout, ABI, storage, or target information is
/// permitted to enter this key.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SirInstanceKey {
    pub template: GenericTemplateId,
    pub type_args: Vec<ResolvedTy>,
}

/// Semantic instance kind of a resolved SIR callable.
///
/// `CallableId` is the identity of an emitted SIR body.  The source `ItemId`
/// and `DefId` retained by [`SemCallable`] are provenance for that body; they
/// are not sufficient to identify a generic instance on their own.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallableInstance {
    Monomorphic,
    Generic(SirInstanceKey),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Provenance {
    Site(SiteId),
    Synthesized,
    Derived(Vec<SiteId>),
}

/// Proven source attribution for a semantic function body.
///
/// SIR carries this independently of raw MIR so the eventual direct SIR → MIR
/// path owns diagnostic attribution instead of borrowing it from a legacy
/// lowered function template.  The variants deliberately match the proven
/// attribution model in HIR: absence is `Unknown`, never an inferred root.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum FunctionSourceOrigin {
    RootUnit,
    Foreign(String),
    #[default]
    Unknown,
}

/// One semantic use of an SSA value.
///
/// An operand carries no mode: what a use does to its value **is the op it
/// feeds** (§1.3). `begin_borrow`, `copy_value`, `move`, `fork`, `load.*` and
/// `store.*` are operations, not annotations on a read.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operand {
    pub value: ValueId,
}

/// One value crossing a semantic call or suspension boundary.
///
/// The boundary owns its physical ownership decision. Keeping this wrapper
/// distinct from [`Operand`] prevents ordinary reads from acquiring a second
/// ownership authority.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundaryOperand {
    pub operand: Operand,
    pub decision: BoundaryDecision,
}

/// Stable position of an operand within one operation or terminator.
///
/// Operation slots follow the source order of their operands. Terminator
/// slots are deliberately flattened in a deterministic order: return value;
/// goto edge arguments; or branch condition, then-edge arguments, and
/// else-edge arguments. Rewrites use this together with [`UseSite`] rather
/// than relying on an incidental pointer into a mutable IR vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OperandSlot(pub u32);

/// Stable position of an outgoing semantic CFG edge within one terminator.
///
/// A successor slot names the edge's *role*, rather than its target. This is
/// important because a branch may legitimately carry two distinct edges to
/// the same target block. The initial terminator vocabulary assigns slot `0`
/// to a `goto` edge and slots `0` and `1` to the then and else edges of a
/// branch, respectively. Future terminators with several resume edges must
/// extend this deterministic ordinal convention instead of identifying an
/// edge by its target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SuccessorSlot(pub u32);

/// Concrete, deterministic identity of one semantic SSA use.
///
/// A value's use site is either an operation operand, identified by its stable
/// [`OpId`], or a terminator operand, identified by its containing [`BlockId`].
/// The expected value closes the stale-index hole: a rewrite must not replace a
/// different value that later occupies the same slot. What the use *does* is
/// the operation's own kind, so no mode is part of this identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UseSite {
    Operation {
        op: OpId,
        operand: OperandSlot,
        value: ValueId,
    },
    Terminator {
        block: BlockId,
        operand: OperandSlot,
        value: ValueId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDef {
    pub id: ValueId,
    pub ty: ResolvedTy,
    /// The §1.2 ownership obligation this value carries.
    pub own: OwnKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockArg {
    pub value: ValueId,
    pub ty: ResolvedTy,
    /// The §1.2 ownership obligation this argument carries. §1.4 requires edge
    /// argument kinds to match block argument kinds exactly.
    pub own: OwnKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Edge {
    pub target: BlockId,
    /// Semantic uses forwarded to the target block arguments. §1.4 requires
    /// their ownership kinds to match the target's exactly.
    pub args: Vec<Operand>,
}

impl Edge {
    /// Visit edge arguments in their deterministic target-argument order.
    ///
    /// # Panics
    ///
    /// Panics only when an edge carries more operands than the module-local
    /// `u32` operand-slot range can represent.
    pub fn visit_operands(&self, mut visit: impl FnMut(OperandSlot, &Operand)) {
        for (index, operand) in self.args.iter().enumerate() {
            visit(
                OperandSlot(u32::try_from(index).expect("SIR edge operand count exceeds u32")),
                operand,
            );
        }
    }

    /// Mutable counterpart to [`Self::visit_operands`].
    ///
    /// # Panics
    ///
    /// Panics only when an edge carries more operands than the module-local
    /// `u32` operand-slot range can represent.
    pub fn visit_operands_mut(&mut self, mut visit: impl FnMut(OperandSlot, &mut Operand)) {
        for (index, operand) in self.args.iter_mut().enumerate() {
            visit(
                OperandSlot(u32::try_from(index).expect("SIR edge operand count exceeds u32")),
                operand,
            );
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemBlock {
    pub id: BlockId,
    pub args: Vec<BlockArg>,
    pub ops: Vec<SemOp>,
    pub terminator: SemTerminator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemFunction {
    /// Source HIR item provenance for this body.
    ///
    /// A generic template can produce several SIR functions with this same
    /// value.  Consumers must use [`Self::callable`] as the body identity.
    pub id: ItemId,
    /// Module-local ABI-neutral identity of this body's resolved callable.
    /// The verifier proves this agrees with the callable table's checker
    /// declaration and exact emitted symbol; consumers must use it rather
    /// than rejoining functions by a symbol spelling.
    pub callable: CallableId,
    /// Resolver-minted source declaration identity.  `name` is an emitted
    /// symbol spelling, whereas calls and future monomorphization carry this
    /// stable semantic identity.
    pub declaration: DefId,
    pub name: String,
    /// Function declaration/body extent in the attribution named by
    /// [`Self::source_origin`].
    pub span: Span,
    pub source_origin: FunctionSourceOrigin,
    pub params: Vec<BlockArg>,
    pub return_ty: ResolvedTy,
    pub entry: BlockId,
    pub blocks: Vec<SemBlock>,
    /// Memory places this body addresses (§1.3 `alloc_place`). Non-escaping
    /// `var`s never get one: HIR-to-SIR construction does mem2reg.
    pub places: Vec<PlaceDecl>,
    /// Every source binding in this body, parameters first and then statement
    /// bindings in source order (§1.6).
    ///
    /// Several bindings may name one value; a value no binding names is a
    /// lowering temp.
    pub bindings: Vec<Binding>,
}

impl SemFunction {
    /// The binding a §1.6 diagnostic should name for `value`: the most recent
    /// one, because a later `let` shadows an earlier name for the same value.
    #[must_use]
    pub fn binding_naming(&self, value: ValueId) -> Option<&Binding> {
        self.bindings
            .iter()
            .rev()
            .find(|binding| binding.target == BindingTarget::Value(value))
    }

    /// The source binding whose storage root is `place`.
    ///
    /// Rule 6a reads the binding's name, span, and mutability through this
    /// relation. The provenance remains in the ordered binding table; neither
    /// [`PlaceDecl`] nor `store.assign` duplicates it.
    #[must_use]
    pub fn binding_rooting(&self, place: PlaceId) -> Option<&Binding> {
        self.bindings
            .iter()
            .find(|binding| binding.target == BindingTarget::Place(place))
    }
}

/// The call convention SIR is permitted to model before ownership/layout MIR
/// decides concrete ABI carriers.
///
/// Runtime, C-ABI, coroutine, actor, and other specialised conventions stay
/// outside this initial domain.  Keeping this enum explicit prevents a
/// resolved SIR call from silently acquiring a target-specific ABI policy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemCallConv {
    Default,
}

/// Semantic class of a callable in the initial SIR direct-call domain.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemCallableKind {
    /// An ordinary Hew user function or flattened impl-method body.
    HewDirect,
}

/// ABI disposition for one semantic callable parameter.
///
/// This is intentionally separate from an operand: a call operand expresses the
/// semantic use at one call site, while this records the callee-side ABI
/// obligation that ownership/layout MIR must eventually realize.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemParamPassing {
    /// The initial scalar direct-call domain accepts only non-owning reads.
    ReadOnly,
    /// The callee borrows the caller's value for the whole body: §1.2 rule 3
    /// makes the parameter a `Guaranteed` value, so a consuming use of it is
    /// rule 3's `E_OWN_CONSUME_BORROWED` wall rather than a leak.
    ///
    /// No lowering emits this slot yet — the ownership-bearing parameter types
    /// that need it arrive with the callable header's borrow disposition (L3) —
    /// and every structural check in `verify.rs` refuses a non-`ReadOnly` slot
    /// until then. The kind derivation reads the slot rather than the type
    /// alone, so the slot is the only thing that has to change.
    Borrow,
}

/// ABI-neutral parameter facts owned by a resolved SIR callable.
#[derive(Debug, Clone, PartialEq)]
pub struct SemAbiParam {
    pub ty: ResolvedTy,
    pub passing: SemParamPassing,
    /// Positive authority for a parameter whose storage projection is visible
    /// to its caller. Scalar v1 direct calls always set this to `false`; later
    /// ownership/layout slices must make any `true` fact explicit here rather
    /// than borrowing a legacy raw-MIR decision.
    pub caller_visible_projection: bool,
}

/// ABI-neutral signature of a resolved SIR callable.
#[derive(Debug, Clone, PartialEq)]
pub struct SemSignature {
    pub params: Vec<SemAbiParam>,
    pub return_ty: ResolvedTy,
}

/// Body-free semantic header for one generic HIR definition.
///
/// SIR never retains an abstract generic function body: every
/// [`CallableInstance::Generic`] is a concrete, closed semantic instance.
/// The header preserves just enough pre-substitution semantic information for
/// the verifier to prove that a concrete callable's provenance, signature,
/// and derived emitted symbol actually match its [`SirInstanceKey`].  It is
/// deliberately free of layout, ABI carrier, storage, and target facts.
#[derive(Debug, Clone, PartialEq)]
pub struct SemGenericTemplate {
    /// Semantic template identity used by every concrete instance key.
    pub id: GenericTemplateId,
    /// Source HIR provenance only; this is not the identity of a concrete SIR
    /// callable body.
    pub function: ItemId,
    /// Base emitted symbol. Concrete symbols are derived from this *after*
    /// semantic-key selection and are never part of the key itself.
    pub symbol: String,
    pub source_origin: FunctionSourceOrigin,
    /// Canonical source-semantic parameters in substitution order.
    pub type_params: Vec<String>,
    /// Pre-substitution semantic callable signature.
    pub signature: SemSignature,
}

/// One checker-resolved, ABI-neutral SIR callable.
///
/// This table is the sole SIR authority for a direct call's stable semantic
/// declaration, exact emitted symbol, source provenance, and signature.  A
/// callable may deliberately have no [`SemFunction`] body: HIR-to-SIR records
/// every eligible scalar declaration before body lowering so a strict driver
/// can diagnose a *reachable* missing body without treating unrelated
/// unsupported functions as a module-wide failure.
#[derive(Debug, Clone, PartialEq)]
pub struct SemCallable {
    pub id: CallableId,
    /// Source HIR item provenance for the body.  This is not an SIR-body
    /// identity: one generic HIR item may have multiple concrete callables.
    pub function: ItemId,
    /// Resolver-minted semantic declaration identity.
    pub declaration: DefId,
    /// Whether this callable is a monomorphic source body or one concrete
    /// generic specialization.  This is the authoritative semantic instance
    /// identity; `symbol` is only its derived emitted-name projection.
    pub instance: CallableInstance,
    /// Exact emitted body symbol. Monomorphic callables retain the resolver's
    /// direct-call symbol; a generic callable derives this only after its
    /// canonical semantic instance has been selected.
    pub symbol: String,
    pub source_origin: FunctionSourceOrigin,
    pub signature: SemSignature,
    pub call_conv: SemCallConv,
    pub kind: SemCallableKind,
}

/// Module-local identity of one demanded concrete record shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AggregateShapeId(pub u32);

/// Semantic aggregate shape selected by an operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AggregateShapeRef {
    /// Tuple field order and types come from the operation's exact tuple type.
    Tuple,
    /// Named record shape carried in [`SemModule::aggregate_shapes`].
    Record(AggregateShapeId),
}

/// One ordered, fully substituted record field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemAggregateField {
    pub name: String,
    pub ty: ResolvedTy,
}

/// Exact semantic shape of one concrete named record used by demanded bodies.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemAggregateShape {
    pub id: AggregateShapeId,
    pub aggregate_ty: ResolvedTy,
    pub instance: NominalInstance,
    pub fields: Vec<SemAggregateField>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SemModule {
    /// Deterministic resolved direct-call authority.  IDs must equal their
    /// indexes in this vector; [`crate::verify_module`] checks that invariant.
    pub callables: Vec<SemCallable>,
    /// Body-free generic semantic headers. These are the substitution
    /// authority for concrete generic callables and must not be confused with
    /// abstract SIR function bodies.
    pub generic_templates: Vec<SemGenericTemplate>,
    /// Every root-unit callable in `callables` order. This supports source
    /// provenance and diagnostics; executable strict reachability starts only
    /// from [`Self::entry_callable`], so unrelated root bodies do not block a
    /// selected program.
    pub root_unit_callables: Vec<CallableId>,
    /// Checker-selected process entry and its complete typed exit contract.
    pub entry_exit_plan: Option<hew_types::EntryExitPlan>,
    /// Resolved entry callable, projected by joining the entry plan's `DefId`.
    /// Neither lowering nor the verifier rediscovers an entry from a
    /// declaration path or emitted symbol.
    pub entry_callable: Option<CallableId>,
    pub functions: Vec<SemFunction>,
    /// Concrete named aggregate shapes mentioned by demanded SIR bodies, in
    /// module-local ID order. Tuple shapes remain structural in `ResolvedTy`.
    pub aggregate_shapes: Vec<SemAggregateShape>,
    /// The §6.3 fact table for every type this module's bodies mention,
    /// projected from `TypeCheckOutput::type_facts`.
    ///
    /// Carried on the module rather than passed to `verify_module` so a
    /// hand-written SIR fixture can be verified at all, and so every existing
    /// caller keeps its signature.
    pub type_facts: BTreeMap<TypeInstanceKey, TypeFacts>,
    /// Interned `string` literal pool. `BTreeMap` per §6.1's determinism rule.
    pub string_literals: BTreeMap<StringLiteralId, String>,
    /// Interned `bytes` literal pool. `BTreeMap` per §6.1's determinism rule.
    pub bytes_literals: BTreeMap<BytesLiteralId, Vec<u8>>,
}

impl SemModule {
    /// Look up a callable only when its identity agrees with the canonical
    /// table index.  This intentionally fails closed for malformed tables.
    #[must_use]
    pub fn callable(&self, id: CallableId) -> Option<&SemCallable> {
        self.callables
            .get(usize::try_from(id.0).ok()?)
            .filter(|callable| callable.id == id)
    }

    /// Resolve a record shape only when its ID agrees with the canonical table
    /// position.
    #[must_use]
    pub fn aggregate_shape(&self, id: AggregateShapeId) -> Option<&SemAggregateShape> {
        self.aggregate_shapes
            .get(usize::try_from(id.0).ok()?)
            .filter(|shape| shape.id == id)
    }

    /// Resolve the one exact concrete record descriptor for a semantic type.
    #[must_use]
    pub fn aggregate_shape_for_type(&self, ty: &ResolvedTy) -> Option<&SemAggregateShape> {
        self.aggregate_shapes
            .iter()
            .find(|shape| &shape.aggregate_ty == ty)
    }

    /// Find a monomorphic resolved callable from checker-owned declaration
    /// identity.
    ///
    /// A generic declaration can own many concrete SIR bodies, so callers
    /// must use [`Self::callable_for_instance`] for that case rather than
    /// accidentally selecting an arbitrary specialization.
    #[must_use]
    pub fn callable_for_declaration(&self, declaration: &DefId) -> Option<&SemCallable> {
        self.callables.iter().find(|callable| {
            &callable.declaration == declaration
                && matches!(callable.instance, CallableInstance::Monomorphic)
        })
    }

    /// Find one concrete generic callable from its closed semantic key.
    #[must_use]
    pub fn callable_for_instance(&self, key: &SirInstanceKey) -> Option<&SemCallable> {
        self.callables.iter().find(|callable| {
            matches!(&callable.instance, CallableInstance::Generic(candidate) if candidate == key)
        })
    }

    /// Look up the body-free semantic template header for an instance key.
    #[must_use]
    pub fn generic_template(&self, id: &GenericTemplateId) -> Option<&SemGenericTemplate> {
        self.generic_templates
            .iter()
            .find(|template| &template.id == id)
    }

    /// Build the callable-to-body association once.
    ///
    /// Every consumer that needs a body for a resolved callable goes through
    /// this. Walking `functions` per lookup made whole-module work quadratic
    /// and invited each caller to reinvent the "exactly one body" rule.
    #[must_use]
    pub fn function_index(&self) -> SemFunctionIndex<'_> {
        let mut by_callable = vec![BodySlot::Absent; self.callables.len()];
        for (position, function) in self.functions.iter().enumerate() {
            let Ok(slot) = usize::try_from(function.callable.0) else {
                continue;
            };
            let Some(entry) = by_callable.get_mut(slot) else {
                continue;
            };
            *entry = match entry {
                BodySlot::Absent => BodySlot::One(position),
                BodySlot::One(_) | BodySlot::Ambiguous => BodySlot::Ambiguous,
            };
        }
        SemFunctionIndex {
            module: self,
            by_callable,
        }
    }
}

/// Which body in `SemModule::functions` realizes a callable.
#[derive(Debug, Clone, Copy)]
enum BodySlot {
    Absent,
    One(usize),
    /// More than one body claims the callable. The module is malformed
    /// (`crate::verify_module` reports it); the index refuses the id rather
    /// than picking a winner by position.
    Ambiguous,
}

/// A module's callable-to-body association, built once by
/// [`SemModule::function_index`].
#[derive(Debug)]
pub struct SemFunctionIndex<'m> {
    module: &'m SemModule,
    by_callable: Vec<BodySlot>,
}

impl<'m> SemFunctionIndex<'m> {
    /// Return the lowered SIR body for `callable`.
    ///
    /// Absence is meaningful to strict call-graph selection — a callable may
    /// legitimately have no body — and is not by itself a malformed table.
    #[must_use]
    pub fn function(&self, callable: CallableId) -> Option<&'m SemFunction> {
        self.module.callable(callable)?;
        let slot = self
            .by_callable
            .get(usize::try_from(callable.0).ok()?)
            .copied()?;
        let BodySlot::One(position) = slot else {
            return None;
        };
        self.module.functions.get(position)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemOp {
    pub id: OpId,
    /// SSA values defined by this operation, in operation-defined order.
    pub results: Vec<ValueDef>,
    pub kind: SemOpKind,
    pub provenance: Provenance,
}

impl SemOp {
    /// Visit this operation's semantic operands in deterministic operand-slot
    /// order. Analyses and rewrites must use this rather than matching every
    /// [`SemOpKind`] variant themselves.
    pub fn visit_operands(&self, visit: impl FnMut(OperandSlot, &Operand)) {
        self.kind.visit_operands(visit);
    }

    /// Mutable counterpart to [`Self::visit_operands`].
    pub fn visit_operands_mut(&mut self, visit: impl FnMut(OperandSlot, &mut Operand)) {
        self.kind.visit_operands_mut(visit);
    }

    /// Replace the value at one concrete operand slot when the slot still
    /// holds the value an analysis saw. Returning `false` makes stale rewrite
    /// sites explicit instead of mutating an unrelated operand after an IR
    /// change.
    #[must_use]
    pub fn replace_operand_at(
        &mut self,
        slot: OperandSlot,
        expected: ValueId,
        replacement: ValueId,
    ) -> bool {
        let mut replaced = false;
        self.visit_operands_mut(|candidate, operand| {
            if candidate == slot && operand.value == expected {
                operand.value = replacement;
                replaced = true;
            }
        });
        replaced
    }
}

/// Derived semantic effects for a value-producing SIR operation.
///
/// Effects deliberately live on operation *kinds*, not on [`SemOp`]: rewrites
/// can clone or synthesize operations without maintaining a second source of
/// truth.  The initial bit set is intentionally small; it gives early
/// canonicalization passes a sound motion/CSE barrier without committing SIR
/// to memory SSA or effect tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct EffectSet(u8);

impl EffectSet {
    /// No observable effect or trap edge.
    pub const PURE: Self = Self(0);
    /// The operation can transfer control to a language-visible trap.
    pub const MAY_TRAP: Self = Self(1 << 0);
    /// An observable operation that is also a full optimization barrier.
    pub const IMPURE: Self = Self(Self::MAY_TRAP.0 | (1 << 1));

    #[must_use]
    pub const fn contains(self, effect: Self) -> bool {
        self.0 & effect.0 == effect.0
    }

    #[must_use]
    pub const fn is_pure(self) -> bool {
        self.0 == Self::PURE.0
    }

    #[must_use]
    pub const fn may_trap(self) -> bool {
        self.contains(Self::MAY_TRAP)
    }
}

/// The value, if any, produced by an invoke-style call terminator.
///
/// A value result is defined at the call and must be forwarded on its normal
/// edge to a continuation block argument. It is never available for another
/// operation in the terminated block.
#[derive(Debug, Clone, PartialEq)]
pub enum CallResult {
    Unit,
    Value(ValueDef),
}

/// Whether an invoke-style call has an unwind CFG successor.
///
/// Applicability is decided by the producer. Later passes inspect this closed
/// value instead of deriving unwind behaviour from a callee or symbol.
#[derive(Debug, Clone, PartialEq)]
pub enum CallUnwind {
    NotApplicable,
    Cleanup(Edge),
}

/// One exact checked-arithmetic failure successor.
#[derive(Debug, Clone, PartialEq)]
pub struct CheckedFailure {
    pub kind: TrapKind,
    pub edge: Edge,
}

/// Value-producing, non-suspending operations in the first SIR slice.
///
/// Effects are derived by [`Self::effects`] rather than stored redundantly on
/// each operation. Suspension and control flow belong in terminators, not
/// ordinary SSA operations.
#[derive(Debug, Clone, PartialEq)]
pub enum SemOpKind {
    ConstI64(i64),
    ConstBool(bool),
    /// Construct a semantic tuple value from its ordered elements.
    ///
    /// This is deliberately an aggregate-value operation: it says nothing
    /// about field offsets, padding, allocation, or storage. Raw MIR decides
    /// whether the resulting value can remain virtual or must be materialized.
    TupleMake {
        elements: Vec<Operand>,
    },
    /// Observe one semantic element of a tuple value.
    ///
    /// `index` is a target-independent semantic position, not a byte offset
    /// or representation tag. The verifier proves that it is in bounds for
    /// the tuple type and that the result has the selected element type.
    TupleGet {
        tuple: Operand,
        index: u32,
    },
    /// Construct one owned semantic aggregate by consuming every ordered
    /// field operand. The shape carries no layout or storage decision.
    AggregateMake {
        shape: AggregateShapeRef,
        fields: Vec<Operand>,
    },
    /// Read one aggregate field and produce an independent logical copy.
    AggregateProjectCopy {
        shape: AggregateShapeRef,
        aggregate: Operand,
        field: u32,
    },
    Unary {
        op: hew_parser::ast::UnaryOp,
        value: Operand,
    },
    Binary {
        op: hew_parser::ast::BinaryOp,
        lhs: Operand,
        rhs: Operand,
    },
    Cast {
        value: Operand,
        to: ResolvedTy,
    },
    // --- P1 literal producers (matrix Legend `const.{f,char,unit,duration,str,bytes}`)
    ConstF64(f64),
    ConstChar(char),
    ConstUnit,
    /// Nanoseconds, the representation `duration` already carries.
    ConstDuration(i64),
    /// A `string` literal, interned in [`SemModule::string_literals`]. The
    /// result is `Owned`: the constant pool hands out a retained value.
    ConstStr(StringLiteralId),
    /// A `bytes` literal, interned in [`SemModule::bytes_literals`].
    ConstBytes(BytesLiteralId),

    // --- P1 structural equality (matrix Legend `str.eq`, `bytes.eq`)
    /// Structural equality over two borrowed `string` operands.
    StrEq {
        lhs: Operand,
        rhs: Operand,
    },
    /// Structural equality over two borrowed `bytes` operands.
    BytesEq {
        lhs: Operand,
        rhs: Operand,
    },

    // --- §1.3 ownership operations
    /// `copy_value %v` - a new obligation over the same value. Legal only when
    /// the type's clone kind is not `None` (rule 6b).
    CopyValue {
        source: Operand,
    },
    /// `destroy_value %v` - consumes the obligation. Illegal on a `Linear`
    /// value except on an unwind edge (rule 6d).
    DestroyValue {
        value: Operand,
    },
    /// `begin_borrow %v` - produces a `Guaranteed` value; the owner is not
    /// consumed inside the region.
    BeginBorrow {
        owner: Operand,
    },
    /// `end_borrow %b` - ends the region on this path.
    EndBorrow {
        borrow: Operand,
    },
    /// `move %v` - one obligation in, one out.
    Move {
        source: Operand,
    },
    /// `fork %v` - one obligation in, one unique obligation out. A fork of a
    /// `BitCopy`, `View` or `PersistentShare` value is `E_SIR_ICE`.
    Fork {
        source: Operand,
    },
    /// `destructure %agg` - consumes the aggregate and produces one result per
    /// field, each of which must be consumed on every path.
    Destructure {
        shape: AggregateShapeRef,
        aggregate: Operand,
    },

    // --- §1.3 place operations
    /// `alloc_place T` - definite initialization is tracked from here (rule 4).
    AllocPlace {
        place: PlaceId,
    },
    /// `load.copy %p` - retain out; the place stays initialized.
    LoadCopy {
        place: PlaceId,
    },
    /// `load.take %p` - the place becomes uninitialized.
    LoadTake {
        place: PlaceId,
    },
    /// `store.init %p, %v` - the place becomes initialized.
    StoreInit {
        place: PlaceId,
        value: Operand,
    },
    /// `store.assign %p, %v` - the old value is destroyed, then stored.
    StoreAssign {
        place: PlaceId,
        value: Operand,
    },
    /// `end_lifetime %p` - destroys the contents; the place is uninitialized.
    EndLifetime {
        place: PlaceId,
    },
}

impl SemOpKind {
    /// Visit operands in their deterministic source order.
    ///
    /// This is intentionally the single structural operand traversal for the
    /// initial operation vocabulary. Extending `SemOpKind` therefore requires
    /// choosing its operand order once, instead of teaching every analysis and
    /// rewrite the new shape independently.
    ///
    /// # Panics
    ///
    /// Panics only when an operation carries more operands than the
    /// module-local `u32` operand-slot range can represent.
    pub fn visit_operands(&self, mut visit: impl FnMut(OperandSlot, &Operand)) {
        match self {
            Self::ConstI64(_)
            | Self::ConstBool(_)
            | Self::ConstF64(_)
            | Self::ConstChar(_)
            | Self::ConstUnit
            | Self::ConstDuration(_)
            | Self::ConstStr(_)
            | Self::ConstBytes(_)
            | Self::AllocPlace { .. }
            | Self::LoadCopy { .. }
            | Self::LoadTake { .. }
            | Self::EndLifetime { .. } => {}
            Self::TupleMake { elements } => {
                for (index, element) in elements.iter().enumerate() {
                    visit(
                        OperandSlot(
                            u32::try_from(index).expect("SIR operation operand count exceeds u32"),
                        ),
                        element,
                    );
                }
            }
            Self::TupleGet { tuple, .. } => visit(OperandSlot(0), tuple),
            Self::AggregateMake { fields, .. } => {
                for (index, field) in fields.iter().enumerate() {
                    visit(
                        OperandSlot(
                            u32::try_from(index).expect("SIR operation operand count exceeds u32"),
                        ),
                        field,
                    );
                }
            }
            Self::AggregateProjectCopy { aggregate, .. } => visit(OperandSlot(0), aggregate),
            Self::Unary { value, .. } | Self::Cast { value, .. } => {
                visit(OperandSlot(0), value);
            }
            Self::Binary { lhs, rhs, .. }
            | Self::StrEq { lhs, rhs }
            | Self::BytesEq { lhs, rhs } => {
                visit(OperandSlot(0), lhs);
                visit(OperandSlot(1), rhs);
            }
            Self::CopyValue { source: value }
            | Self::Move { source: value }
            | Self::Fork { source: value }
            | Self::DestroyValue { value }
            | Self::BeginBorrow { owner: value }
            | Self::EndBorrow { borrow: value }
            | Self::Destructure {
                aggregate: value, ..
            }
            | Self::StoreInit { value, .. }
            | Self::StoreAssign { value, .. } => visit(OperandSlot(0), value),
        }
    }

    /// Mutable counterpart to [`Self::visit_operands`].
    ///
    /// # Panics
    ///
    /// Panics only when an operation carries more operands than the
    /// module-local `u32` operand-slot range can represent.
    pub fn visit_operands_mut(&mut self, mut visit: impl FnMut(OperandSlot, &mut Operand)) {
        match self {
            Self::ConstI64(_)
            | Self::ConstBool(_)
            | Self::ConstF64(_)
            | Self::ConstChar(_)
            | Self::ConstUnit
            | Self::ConstDuration(_)
            | Self::ConstStr(_)
            | Self::ConstBytes(_)
            | Self::AllocPlace { .. }
            | Self::LoadCopy { .. }
            | Self::LoadTake { .. }
            | Self::EndLifetime { .. } => {}
            Self::TupleMake { elements } => {
                for (index, element) in elements.iter_mut().enumerate() {
                    visit(
                        OperandSlot(
                            u32::try_from(index).expect("SIR operation operand count exceeds u32"),
                        ),
                        element,
                    );
                }
            }
            Self::TupleGet { tuple, .. } => visit(OperandSlot(0), tuple),
            Self::AggregateMake { fields, .. } => {
                for (index, field) in fields.iter_mut().enumerate() {
                    visit(
                        OperandSlot(
                            u32::try_from(index).expect("SIR operation operand count exceeds u32"),
                        ),
                        field,
                    );
                }
            }
            Self::AggregateProjectCopy { aggregate, .. } => visit(OperandSlot(0), aggregate),
            Self::Unary { value, .. } | Self::Cast { value, .. } => {
                visit(OperandSlot(0), value);
            }
            Self::Binary { lhs, rhs, .. }
            | Self::StrEq { lhs, rhs }
            | Self::BytesEq { lhs, rhs } => {
                visit(OperandSlot(0), lhs);
                visit(OperandSlot(1), rhs);
            }
            Self::CopyValue { source: value }
            | Self::Move { source: value }
            | Self::Fork { source: value }
            | Self::DestroyValue { value }
            | Self::BeginBorrow { owner: value }
            | Self::EndBorrow { borrow: value }
            | Self::Destructure {
                aggregate: value, ..
            }
            | Self::StoreInit { value, .. }
            | Self::StoreAssign { value, .. } => visit(OperandSlot(0), value),
        }
    }

    /// Return the operation's conservative semantic effect classification.
    #[must_use]
    pub const fn effects(&self) -> EffectSet {
        match self {
            Self::Unary {
                op: hew_parser::ast::UnaryOp::RawDeref,
                ..
            } => EffectSet::MAY_TRAP,
            // Ownership operations are optimization barriers, not pure
            // values: two `copy_value`s of one value are two retains and must
            // never be common-subexpression-eliminated into one, and a
            // `destroy_value` or a place write is observable.
            Self::CopyValue { .. }
            | Self::DestroyValue { .. }
            | Self::BeginBorrow { .. }
            | Self::EndBorrow { .. }
            | Self::Move { .. }
            | Self::Fork { .. }
            | Self::AggregateMake { .. }
            | Self::AggregateProjectCopy { .. }
            | Self::Destructure { .. }
            | Self::AllocPlace { .. }
            | Self::LoadCopy { .. }
            | Self::LoadTake { .. }
            | Self::StoreInit { .. }
            | Self::StoreAssign { .. }
            | Self::EndLifetime { .. } => EffectSet::IMPURE,
            Self::ConstI64(_)
            | Self::ConstBool(_)
            | Self::ConstF64(_)
            | Self::ConstChar(_)
            | Self::ConstUnit
            | Self::ConstDuration(_)
            | Self::ConstStr(_)
            | Self::ConstBytes(_)
            | Self::StrEq { .. }
            | Self::BytesEq { .. }
            | Self::TupleMake { .. }
            | Self::TupleGet { .. }
            | Self::Unary { .. }
            | Self::Binary { .. }
            | Self::Cast { .. } => EffectSet::PURE,
        }
    }

    /// Whether this operation moves or discharges an ownership obligation.
    ///
    /// A CFG rewrite that discards a block containing one would drop or
    /// duplicate an obligation, which is what the discard-safety check refuses.
    #[must_use]
    pub const fn transfers_obligation(&self) -> bool {
        matches!(
            self,
            Self::DestroyValue { .. }
                | Self::Move { .. }
                | Self::Fork { .. }
                | Self::AggregateMake { .. }
                | Self::Destructure { .. }
                | Self::LoadTake { .. }
                | Self::StoreInit { .. }
                | Self::StoreAssign { .. }
                | Self::EndLifetime { .. }
        )
    }
}

/// Semantic control-flow terminator.
///
/// Value-producing operations retain their own [`Provenance`] today. A
/// terminator can be caused by several source sites (for example, a synthesized
/// short-circuit edge plus a branch expression), so this first rewrite
/// foundation deliberately does not invent one misleading `SiteId` field for
/// it. A future control-provenance design must use the existing multi-origin
/// [`Provenance`] model rather than collapsing that attribution during a pass.
#[derive(Debug, Clone, PartialEq)]
pub enum SemTerminator {
    Return {
        value: Option<BoundaryOperand>,
    },
    Goto(Edge),
    Branch {
        condition: Operand,
        then_target: Edge,
        else_target: Edge,
    },
    /// Checked integer arithmetic with explicit normal and failure control.
    ///
    /// `result` exists only on the normal edge. Each failure edge must clean
    /// up live owners before reaching a matching [`SemTerminator::Trap`].
    CheckedBinary {
        id: OpId,
        op: hew_parser::ast::BinaryOp,
        lhs: Operand,
        rhs: Operand,
        result: ValueDef,
        normal: Edge,
        failures: Vec<CheckedFailure>,
    },
    /// A resolved ordinary Hew direct call with explicit normal and unwind
    /// control flow.
    Call {
        id: OpId,
        callee: CallableId,
        args: Vec<BoundaryOperand>,
        result: CallResult,
        normal: Edge,
        unwind: CallUnwind,
    },
    /// A call to a runtime symbol family. Per-operand ownership comes from the
    /// family's FFI ownership row, never from the symbol spelling.
    RtCall {
        id: OpId,
        family: hew_types::RuntimeCallFamily,
        args: Vec<BoundaryOperand>,
        result: CallResult,
        normal: Edge,
        unwind: CallUnwind,
    },
    /// A language-visible trap (§1.6). Unlike [`Self::Unreachable`] this is a
    /// reachable endpoint the program can take.
    Trap {
        kind: TrapKind,
    },
    /// A suspension (§1.5).
    ///
    /// Operand slots are flattened deterministically: the inputs in order, then
    /// each resume edge's arguments in resume order, then the cancel edge's
    /// arguments. Successor slots are the resume edges at `0..resumes.len()`
    /// followed by the cancel edge.
    Suspend {
        kind: SuspendKind,
        inputs: Vec<BoundaryOperand>,
        /// One edge per outcome: `await` has one, `select` has one per arm,
        /// a deadline form has two, `join` has one.
        resumes: Vec<Edge>,
        /// Always present; its first op is the kind's abandon op.
        cancel: Edge,
    },
    /// Continue unwinding after an invoke cleanup block has discharged its
    /// obligations.
    ResumeUnwind,
    /// A semantically unreachable CFG endpoint.
    ///
    /// Raw MIR preserves this as its own semantic endpoint, and LLVM lowers it
    /// directly to `unreachable`. It is not a language-visible trap and owns no
    /// cleanup path.
    Unreachable,
}

impl SemTerminator {
    /// Visit call, return and suspension values together with their total
    /// boundary decisions.
    ///
    /// # Panics
    ///
    /// Panics only when a suspension carries more inputs than the module-local
    /// `u32` operand-slot range can represent.
    pub fn visit_boundary_operands(&self, mut visit: impl FnMut(OperandSlot, &BoundaryOperand)) {
        match self {
            Self::Return { value: Some(value) } => visit(OperandSlot(0), value),
            Self::Call { args, .. } | Self::RtCall { args, .. } => {
                for (index, argument) in args.iter().enumerate() {
                    visit(
                        OperandSlot(
                            u32::try_from(index).expect("SIR boundary operand count exceeds u32"),
                        ),
                        argument,
                    );
                }
            }
            Self::Suspend { inputs, .. } => {
                for (index, input) in inputs.iter().enumerate() {
                    visit(
                        OperandSlot(
                            u32::try_from(index).expect("SIR boundary operand count exceeds u32"),
                        ),
                        input,
                    );
                }
            }
            Self::Return { value: None }
            | Self::Goto(_)
            | Self::Branch { .. }
            | Self::CheckedBinary { .. }
            | Self::Trap { .. }
            | Self::ResumeUnwind
            | Self::Unreachable => {}
        }
    }

    /// Visit SSA values defined by this terminator.
    pub fn visit_results(&self, mut visit: impl FnMut(&ValueDef)) {
        match self {
            Self::Call {
                result: CallResult::Value(result),
                ..
            }
            | Self::RtCall {
                result: CallResult::Value(result),
                ..
            }
            | Self::CheckedBinary { result, .. } => visit(result),
            Self::Return { .. }
            | Self::Goto(_)
            | Self::Branch { .. }
            | Self::Call {
                result: CallResult::Unit,
                ..
            }
            | Self::RtCall {
                result: CallResult::Unit,
                ..
            }
            | Self::Trap { .. }
            | Self::Suspend { .. }
            | Self::ResumeUnwind
            | Self::Unreachable => {}
        }
    }

    /// Visit every semantic use carried by this terminator.
    ///
    /// Slots are stable within the terminator shape: a return has slot `0`; a
    /// goto uses its edge-argument indexes; and a branch uses condition `0`,
    /// then-edge arguments beginning at `1`, followed by else-edge arguments.
    ///
    /// # Panics
    ///
    /// Panics only when a branch carries more operands than the module-local
    /// `u32` operand-slot range can represent.
    pub fn visit_operands(&self, mut visit: impl FnMut(OperandSlot, &Operand)) {
        match self {
            Self::Return { value: Some(value) } => visit(OperandSlot(0), &value.operand),
            Self::Goto(edge) => edge.visit_operands(visit),
            Self::Branch {
                condition,
                then_target,
                else_target,
            } => {
                visit(OperandSlot(0), condition);
                let mut next = 1_u32;
                for operand in &then_target.args {
                    visit(OperandSlot(next), operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR branch operand count exceeds u32");
                }
                for operand in &else_target.args {
                    visit(OperandSlot(next), operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR branch operand count exceeds u32");
                }
            }
            Self::CheckedBinary {
                lhs,
                rhs,
                normal,
                failures,
                ..
            } => visit_checked_binary_operands(lhs, rhs, normal, failures, visit),
            Self::Call {
                args,
                normal,
                unwind,
                ..
            }
            | Self::RtCall {
                args,
                normal,
                unwind,
                ..
            } => {
                let mut next = 0_u32;
                for argument in args {
                    visit(OperandSlot(next), &argument.operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR call operand count exceeds u32");
                }
                for operand in &normal.args {
                    visit(OperandSlot(next), operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR call operand count exceeds u32");
                }
                if let CallUnwind::Cleanup(edge) = unwind {
                    for operand in &edge.args {
                        visit(OperandSlot(next), operand);
                        next = next
                            .checked_add(1)
                            .expect("SIR call operand count exceeds u32");
                    }
                }
            }
            Self::Suspend {
                inputs,
                resumes,
                cancel,
                ..
            } => {
                let mut next = 0_u32;
                for input in inputs {
                    visit(OperandSlot(next), &input.operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR suspend operand count exceeds u32");
                }
                for edge in resumes.iter().chain(std::iter::once(cancel)) {
                    for operand in &edge.args {
                        visit(OperandSlot(next), operand);
                        next = next
                            .checked_add(1)
                            .expect("SIR suspend operand count exceeds u32");
                    }
                }
            }
            Self::Return { value: None }
            | Self::Trap { .. }
            | Self::ResumeUnwind
            | Self::Unreachable => {}
        }
    }

    /// Mutable counterpart to [`Self::visit_operands`].
    ///
    /// # Panics
    ///
    /// Panics only when a branch carries more operands than the module-local
    /// `u32` operand-slot range can represent.
    pub fn visit_operands_mut(&mut self, mut visit: impl FnMut(OperandSlot, &mut Operand)) {
        match self {
            Self::Return { value: Some(value) } => visit(OperandSlot(0), &mut value.operand),
            Self::Goto(edge) => edge.visit_operands_mut(visit),
            Self::Branch {
                condition,
                then_target,
                else_target,
            } => {
                visit(OperandSlot(0), condition);
                let mut next = 1_u32;
                for operand in &mut then_target.args {
                    visit(OperandSlot(next), operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR branch operand count exceeds u32");
                }
                for operand in &mut else_target.args {
                    visit(OperandSlot(next), operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR branch operand count exceeds u32");
                }
            }
            Self::CheckedBinary {
                lhs,
                rhs,
                normal,
                failures,
                ..
            } => visit_checked_binary_operands_mut(lhs, rhs, normal, failures, visit),
            Self::Call {
                args,
                normal,
                unwind,
                ..
            }
            | Self::RtCall {
                args,
                normal,
                unwind,
                ..
            } => {
                let mut next = 0_u32;
                for argument in args {
                    visit(OperandSlot(next), &mut argument.operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR call operand count exceeds u32");
                }
                for operand in &mut normal.args {
                    visit(OperandSlot(next), operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR call operand count exceeds u32");
                }
                if let CallUnwind::Cleanup(edge) = unwind {
                    for operand in &mut edge.args {
                        visit(OperandSlot(next), operand);
                        next = next
                            .checked_add(1)
                            .expect("SIR call operand count exceeds u32");
                    }
                }
            }
            Self::Suspend {
                inputs,
                resumes,
                cancel,
                ..
            } => {
                let mut next = 0_u32;
                for input in inputs.iter_mut() {
                    visit(OperandSlot(next), &mut input.operand);
                    next = next
                        .checked_add(1)
                        .expect("SIR suspend operand count exceeds u32");
                }
                for edge in resumes.iter_mut().chain(std::iter::once(cancel)) {
                    for operand in &mut edge.args {
                        visit(OperandSlot(next), operand);
                        next = next
                            .checked_add(1)
                            .expect("SIR suspend operand count exceeds u32");
                    }
                }
            }
            Self::Return { value: None }
            | Self::Trap { .. }
            | Self::ResumeUnwind
            | Self::Unreachable => {}
        }
    }

    /// Visit CFG successors together with their stable structural slots.
    ///
    /// A slot identifies one edge within this terminator, not the target
    /// block. This preserves the distinction between duplicate edges such as
    /// `branch %condition, bb1, bb1`, which later CFG rewrites must be able
    /// to redirect independently.
    ///
    /// # Panics
    ///
    /// Panics only when a suspension carries more edges than the module-local
    /// `u32` successor-slot range can represent.
    pub fn visit_successors_with_slots(&self, mut visit: impl FnMut(SuccessorSlot, &Edge)) {
        match self {
            Self::Return { .. } | Self::Trap { .. } | Self::ResumeUnwind | Self::Unreachable => {}
            Self::Goto(edge) => visit(SuccessorSlot(0), edge),
            Self::Branch {
                then_target,
                else_target,
                ..
            } => {
                visit(SuccessorSlot(0), then_target);
                visit(SuccessorSlot(1), else_target);
            }
            Self::CheckedBinary {
                normal, failures, ..
            } => {
                visit(SuccessorSlot(0), normal);
                for (index, failure) in failures.iter().enumerate() {
                    visit(
                        SuccessorSlot(
                            u32::try_from(index + 1)
                                .expect("SIR checked-binary edge count exceeds u32"),
                        ),
                        &failure.edge,
                    );
                }
            }
            Self::Call { normal, unwind, .. } | Self::RtCall { normal, unwind, .. } => {
                visit(SuccessorSlot(0), normal);
                if let CallUnwind::Cleanup(edge) = unwind {
                    visit(SuccessorSlot(1), edge);
                }
            }
            Self::Suspend {
                resumes, cancel, ..
            } => {
                for (index, edge) in resumes.iter().chain(std::iter::once(cancel)).enumerate() {
                    visit(
                        SuccessorSlot(
                            u32::try_from(index).expect("SIR suspend edge count exceeds u32"),
                        ),
                        edge,
                    );
                }
            }
        }
    }

    /// Mutable counterpart to [`Self::visit_successors_with_slots`].
    ///
    /// # Panics
    ///
    /// Panics only when a suspension carries more edges than the module-local
    /// `u32` successor-slot range can represent.
    pub fn visit_successors_with_slots_mut(
        &mut self,
        mut visit: impl FnMut(SuccessorSlot, &mut Edge),
    ) {
        match self {
            Self::Return { .. } | Self::Trap { .. } | Self::ResumeUnwind | Self::Unreachable => {}
            Self::Goto(edge) => visit(SuccessorSlot(0), edge),
            Self::Branch {
                then_target,
                else_target,
                ..
            } => {
                visit(SuccessorSlot(0), then_target);
                visit(SuccessorSlot(1), else_target);
            }
            Self::CheckedBinary {
                normal, failures, ..
            } => {
                visit(SuccessorSlot(0), normal);
                for (index, failure) in failures.iter_mut().enumerate() {
                    visit(
                        SuccessorSlot(
                            u32::try_from(index + 1)
                                .expect("SIR checked-binary edge count exceeds u32"),
                        ),
                        &mut failure.edge,
                    );
                }
            }
            Self::Call { normal, unwind, .. } | Self::RtCall { normal, unwind, .. } => {
                visit(SuccessorSlot(0), normal);
                if let CallUnwind::Cleanup(edge) = unwind {
                    visit(SuccessorSlot(1), edge);
                }
            }
            Self::Suspend {
                resumes, cancel, ..
            } => {
                for (index, edge) in resumes
                    .iter_mut()
                    .chain(std::iter::once(cancel))
                    .enumerate()
                {
                    visit(
                        SuccessorSlot(
                            u32::try_from(index).expect("SIR suspend edge count exceeds u32"),
                        ),
                        edge,
                    );
                }
            }
        }
    }

    /// Return the edge at one stable successor slot, if this terminator owns
    /// that slot.
    ///
    /// The slot is intentionally structural: callers can distinguish and
    /// inspect duplicate edges without comparing targets.
    #[must_use]
    pub fn successor(&self, slot: SuccessorSlot) -> Option<&Edge> {
        match self {
            Self::Goto(edge) if slot == SuccessorSlot(0) => Some(edge),
            Self::Branch {
                then_target,
                else_target,
                ..
            } => match slot.0 {
                0 => Some(then_target),
                1 => Some(else_target),
                _ => None,
            },
            Self::Call { normal, unwind, .. } | Self::RtCall { normal, unwind, .. } => {
                match slot.0 {
                    0 => Some(normal),
                    1 => match unwind {
                        CallUnwind::NotApplicable => None,
                        CallUnwind::Cleanup(edge) => Some(edge),
                    },
                    _ => None,
                }
            }
            Self::CheckedBinary {
                normal, failures, ..
            } => match slot.0 {
                0 => Some(normal),
                value => failures
                    .get(usize::try_from(value - 1).ok()?)
                    .map(|failure| &failure.edge),
            },
            Self::Suspend {
                resumes, cancel, ..
            } => resumes
                .iter()
                .chain(std::iter::once(cancel))
                .nth(usize::try_from(slot.0).ok()?),
            Self::Return { .. }
            | Self::Goto(_)
            | Self::Trap { .. }
            | Self::ResumeUnwind
            | Self::Unreachable => None,
        }
    }

    /// Mutable counterpart to [`Self::successor`].
    #[must_use]
    pub fn successor_mut(&mut self, slot: SuccessorSlot) -> Option<&mut Edge> {
        match self {
            Self::Goto(edge) if slot == SuccessorSlot(0) => Some(edge),
            Self::Branch {
                then_target,
                else_target,
                ..
            } => match slot.0 {
                0 => Some(then_target),
                1 => Some(else_target),
                _ => None,
            },
            Self::Call { normal, unwind, .. } | Self::RtCall { normal, unwind, .. } => {
                match slot.0 {
                    0 => Some(normal),
                    1 => match unwind {
                        CallUnwind::NotApplicable => None,
                        CallUnwind::Cleanup(edge) => Some(edge),
                    },
                    _ => None,
                }
            }
            Self::CheckedBinary {
                normal, failures, ..
            } => match slot.0 {
                0 => Some(normal),
                value => failures
                    .get_mut(usize::try_from(value - 1).ok()?)
                    .map(|failure| &mut failure.edge),
            },
            Self::Suspend {
                resumes, cancel, ..
            } => resumes
                .iter_mut()
                .chain(std::iter::once(cancel))
                .nth(usize::try_from(slot.0).ok()?),
            Self::Return { .. }
            | Self::Goto(_)
            | Self::Trap { .. }
            | Self::ResumeUnwind
            | Self::Unreachable => None,
        }
    }

    /// Visit CFG successors without exposing terminator shape to each caller.
    ///
    /// This compatibility visitor deliberately delegates to
    /// [`Self::visit_successors_with_slots`]. New CFG analyses should retain
    /// the slot so they can distinguish duplicate edges.
    pub fn visit_successors(&self, mut visit: impl FnMut(&Edge)) {
        self.visit_successors_with_slots(|_, edge| visit(edge));
    }

    /// Mutable counterpart to [`Self::visit_successors`].
    ///
    /// This compatibility visitor deliberately delegates to
    /// [`Self::visit_successors_with_slots_mut`].
    pub fn visit_successors_mut(&mut self, mut visit: impl FnMut(&mut Edge)) {
        self.visit_successors_with_slots_mut(|_, edge| visit(edge));
    }

    /// Human-readable role of one operand slot, for exact verifier
    /// diagnostics. Callers only pass slots previously produced by
    /// [`Self::visit_operands`].
    #[must_use]
    pub fn operand_context(&self, slot: OperandSlot) -> &'static str {
        match self {
            Self::Return { .. } => "return value",
            Self::Goto(_) => "goto edge argument",
            Self::Branch { then_target, .. } if slot.0 == 0 => "branch condition",
            Self::Branch { then_target, .. }
                if usize::try_from(slot.0).is_ok_and(|slot| slot <= then_target.args.len()) =>
            {
                "branch then-edge argument"
            }
            Self::Branch { .. } => "branch else-edge argument",
            Self::CheckedBinary { normal, .. }
                if usize::try_from(slot.0).is_ok_and(|slot| slot < 2 + normal.args.len()) =>
            {
                if slot.0 == 0 {
                    "checked-binary left operand"
                } else if slot.0 == 1 {
                    "checked-binary right operand"
                } else {
                    "checked-binary normal-edge argument"
                }
            }
            Self::CheckedBinary { .. } => "checked-binary failure-edge argument",
            Self::Call { args, normal, .. } | Self::RtCall { args, normal, .. }
                if usize::try_from(slot.0).is_ok_and(|slot| slot < args.len()) =>
            {
                "call argument"
            }
            Self::Call { args, normal, .. } | Self::RtCall { args, normal, .. }
                if usize::try_from(slot.0)
                    .is_ok_and(|slot| slot < args.len() + normal.args.len()) =>
            {
                "call normal-edge argument"
            }
            Self::Call { .. } | Self::RtCall { .. } => "call unwind-edge argument",
            Self::Suspend { inputs, .. }
                if usize::try_from(slot.0).is_ok_and(|slot| slot < inputs.len()) =>
            {
                "suspend input"
            }
            Self::Suspend { .. } => "suspend edge argument",
            Self::Trap { .. } => "trap terminator operand",
            Self::ResumeUnwind => "resume-unwind terminator operand",
            Self::Unreachable => "unreachable terminator operand",
        }
    }

    /// Replace the value at one concrete terminator operand slot when the slot
    /// still holds the value the indexed use site named.
    #[must_use]
    pub fn replace_operand_at(
        &mut self,
        slot: OperandSlot,
        expected: ValueId,
        replacement: ValueId,
    ) -> bool {
        let mut replaced = false;
        self.visit_operands_mut(|candidate, operand| {
            if candidate == slot && operand.value == expected {
                operand.value = replacement;
                replaced = true;
            }
        });
        replaced
    }
}

fn visit_checked_binary_operands(
    lhs: &Operand,
    rhs: &Operand,
    normal: &Edge,
    failures: &[CheckedFailure],
    mut visit: impl FnMut(OperandSlot, &Operand),
) {
    visit(OperandSlot(0), lhs);
    visit(OperandSlot(1), rhs);
    let mut next = 2_u32;
    for edge in std::iter::once(normal).chain(failures.iter().map(|failure| &failure.edge)) {
        for operand in &edge.args {
            visit(OperandSlot(next), operand);
            next = next
                .checked_add(1)
                .expect("SIR checked-binary operand count exceeds u32");
        }
    }
}

fn visit_checked_binary_operands_mut(
    lhs: &mut Operand,
    rhs: &mut Operand,
    normal: &mut Edge,
    failures: &mut [CheckedFailure],
    mut visit: impl FnMut(OperandSlot, &mut Operand),
) {
    visit(OperandSlot(0), lhs);
    visit(OperandSlot(1), rhs);
    let mut next = 2_u32;
    for edge in std::iter::once(normal).chain(failures.iter_mut().map(|failure| &mut failure.edge))
    {
        for operand in &mut edge.args {
            visit(OperandSlot(next), operand);
            next = next
                .checked_add(1)
                .expect("SIR checked-binary operand count exceeds u32");
        }
    }
}
