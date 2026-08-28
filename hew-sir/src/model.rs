use hew_hir::{ItemId, SiteId};
use hew_parser::ast::Span;
use hew_types::{DefId, ResolvedTy};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UseMode {
    Read,
    BorrowShared,
    BorrowMut,
    /// Transfer this use's ownership into another semantic value or operation.
    ///
    /// This is deliberately distinct from [`Self::Consume`]: a move has a
    /// receiving owner, while consumption discharges the source value without
    /// creating a new semantic owner.
    Move,
    /// Discharge a value's source-semantic ownership obligation.
    ///
    /// Ownership/layout MIR will later decide whether that becomes a drop,
    /// release, move-out, or another concrete lifetime action.
    Consume,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operand {
    pub value: ValueId,
    pub mode: UseMode,
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

/// Concrete, deterministic identity of one semantic SSA use.
///
/// A value's use site is either an operation operand, identified by its stable
/// [`OpId`], or a terminator operand, identified by its containing [`BlockId`].
/// The use mode is part of the identity so a rewrite cannot silently apply an
/// analysis result after the operand's semantic ownership use changed. The
/// expected value closes the final stale-index hole: a rewrite must not replace
/// a different value that later occupies the same slot with the same mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UseSite {
    Operation {
        op: OpId,
        operand: OperandSlot,
        value: ValueId,
        mode: UseMode,
    },
    Terminator {
        block: BlockId,
        operand: OperandSlot,
        value: ValueId,
        mode: UseMode,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDef {
    pub id: ValueId,
    pub ty: ResolvedTy,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockArg {
    pub value: ValueId,
    pub ty: ResolvedTy,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Edge {
    pub target: BlockId,
    /// Semantic uses forwarded to the target block arguments. These retain
    /// their source ownership mode until ownership/layout MIR realizes it.
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
/// This is intentionally separate from [`UseMode`]: call operands express the
/// semantic use at one call site, while this records the callee-side ABI
/// obligation that ownership/layout MIR must eventually realize.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemParamPassing {
    /// The initial scalar direct-call domain accepts only non-owning reads.
    ReadOnly,
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

/// Conservative interprocedural effect summary carried by a resolved SIR
/// callable.
///
/// SIR does not yet compute a function-summary fixed point, so every
/// producer currently writes [`Self::Unknown`].  The explicit carrier means
/// call operations can derive their effects from resolved callee metadata when
/// that analysis arrives, without turning `SemOp` into a second source of
/// truth.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum EffectSummary {
    #[default]
    Unknown,
    /// The callable has no other known observable effect, but can transfer
    /// control to a language-visible trap. Initial scalar bodies commonly
    /// fall in this class because checked integer operations can overflow.
    MayTrap,
    Pure,
}

impl EffectSummary {
    #[must_use]
    pub const fn effects(self) -> EffectSet {
        match self {
            Self::Unknown => EffectSet::UNKNOWN_CALL,
            Self::MayTrap => EffectSet::MAY_TRAP,
            Self::Pure => EffectSet::PURE,
        }
    }
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
    pub effect_summary: EffectSummary,
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
    /// Checked root-unit entry callable, if its signature belongs to the
    /// current SIR surface. HIR establishes this from the root source `main`
    /// declaration once; strict selection never rediscovers it by symbol.
    pub entry_callable: Option<CallableId>,
    pub functions: Vec<SemFunction>,
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

    /// Return the lowered SIR body for `id`, if the body was in the current
    /// supported surface slice.  Absence is meaningful to strict call-graph
    /// selection and is not a malformed callable-table condition by itself.
    #[must_use]
    pub fn function_for_callable(&self, id: CallableId) -> Option<&SemFunction> {
        self.callable(id)?;
        self.functions
            .iter()
            .find(|function| function.callable == id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemOp {
    pub id: OpId,
    /// SSA values defined by this operation.  Ordinary value operations and
    /// non-unit calls define exactly one value in the initial SIR slice.
    /// A direct call whose resolved callable returns `Unit` defines no value:
    /// its control/effect still remains explicit as a zero-result `Call`.
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

    /// Replace the value at one concrete operand slot when its use mode still
    /// agrees with an analysis result. Returning `false` makes stale rewrite
    /// sites explicit instead of mutating an unrelated operand after an IR
    /// change.
    #[must_use]
    pub fn replace_operand_at(
        &mut self,
        slot: OperandSlot,
        expected: ValueId,
        mode: UseMode,
        replacement: ValueId,
    ) -> bool {
        let mut replaced = false;
        self.visit_operands_mut(|candidate, operand| {
            if candidate == slot && operand.value == expected && operand.mode == mode {
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
/// to memory SSA or effect tokens.  Isolated operations conservatively report
/// [`Self::UNKNOWN_CALL`] for calls; module-aware consumers use the resolved
/// callable's effect summary through [`SemOpKind::effects_in`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct EffectSet(u8);

impl EffectSet {
    /// No observable effect or trap edge.
    pub const PURE: Self = Self(0);
    /// The operation can transfer control to a language-visible trap.
    pub const MAY_TRAP: Self = Self(1 << 0);
    /// The callee has no resolved SIR effect summary yet; treat it as a full
    /// optimization barrier. It includes [`Self::MAY_TRAP`] so both the
    /// semantic predicate and ordinary bit-set containment remain sound for
    /// early optimizer clients.
    pub const UNKNOWN_CALL: Self = Self(Self::MAY_TRAP.0 | (1 << 1));

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
    /// A resolved ordinary Hew direct call.
    ///
    /// The callable table determines result arity: a non-unit callee produces
    /// one SSA value, while a unit callee is an explicit zero-result operation.
    /// This avoids inventing a unit SSA carrier solely to model a call with
    /// observable control/effect behavior.
    Call {
        callee: CallableId,
        args: Vec<Operand>,
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
            Self::ConstI64(_) | Self::ConstBool(_) => {}
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
            Self::Unary { value, .. } | Self::Cast { value, .. } => {
                visit(OperandSlot(0), value);
            }
            Self::Binary { lhs, rhs, .. } => {
                visit(OperandSlot(0), lhs);
                visit(OperandSlot(1), rhs);
            }
            Self::Call { args, .. } => {
                for (index, argument) in args.iter().enumerate() {
                    visit(
                        OperandSlot(
                            u32::try_from(index).expect("SIR operation operand count exceeds u32"),
                        ),
                        argument,
                    );
                }
            }
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
            Self::ConstI64(_) | Self::ConstBool(_) => {}
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
            Self::Unary { value, .. } | Self::Cast { value, .. } => {
                visit(OperandSlot(0), value);
            }
            Self::Binary { lhs, rhs, .. } => {
                visit(OperandSlot(0), lhs);
                visit(OperandSlot(1), rhs);
            }
            Self::Call { args, .. } => {
                for (index, argument) in args.iter_mut().enumerate() {
                    visit(
                        OperandSlot(
                            u32::try_from(index).expect("SIR operation operand count exceeds u32"),
                        ),
                        argument,
                    );
                }
            }
        }
    }

    /// Return the operation's conservative semantic effect classification.
    #[must_use]
    pub const fn effects(&self) -> EffectSet {
        match self {
            Self::Call { .. } => EffectSet::UNKNOWN_CALL,
            Self::Unary {
                op: hew_parser::ast::UnaryOp::Negate | hew_parser::ast::UnaryOp::RawDeref,
                ..
            }
            | Self::Binary {
                op:
                    hew_parser::ast::BinaryOp::Add
                    | hew_parser::ast::BinaryOp::Subtract
                    | hew_parser::ast::BinaryOp::Multiply
                    | hew_parser::ast::BinaryOp::Divide
                    | hew_parser::ast::BinaryOp::Modulo
                    | hew_parser::ast::BinaryOp::Shl
                    | hew_parser::ast::BinaryOp::Shr,
                ..
            } => EffectSet::MAY_TRAP,
            Self::ConstI64(_)
            | Self::ConstBool(_)
            | Self::TupleMake { .. }
            | Self::TupleGet { .. }
            | Self::Unary { .. }
            | Self::Binary { .. }
            | Self::Cast { .. } => EffectSet::PURE,
        }
    }

    /// Return the operation's effect set using resolved module call metadata
    /// when it is available.  The context-free [`Self::effects`] remains
    /// conservative for tools that inspect isolated operations.
    #[must_use]
    pub fn effects_in(&self, module: &SemModule) -> EffectSet {
        match self {
            Self::Call { callee, .. } => module
                .callable(*callee)
                .map_or(EffectSet::UNKNOWN_CALL, |callable| {
                    callable.effect_summary.effects()
                }),
            _ => self.effects(),
        }
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
        value: Option<Operand>,
    },
    Goto(Edge),
    Branch {
        condition: Operand,
        then_target: Edge,
        else_target: Edge,
    },
    /// A semantically unreachable CFG endpoint.
    ///
    /// Raw MIR preserves this as its own semantic endpoint, and LLVM lowers it
    /// directly to `unreachable`. It is not a language-visible trap and owns no
    /// cleanup path.
    Unreachable,
}

impl SemTerminator {
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
            Self::Return { value } => {
                if let Some(value) = value {
                    visit(OperandSlot(0), value);
                }
            }
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
            Self::Unreachable => {}
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
            Self::Return { value } => {
                if let Some(value) = value {
                    visit(OperandSlot(0), value);
                }
            }
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
            Self::Unreachable => {}
        }
    }

    /// Visit CFG successors without exposing terminator shape to each caller.
    pub fn visit_successors(&self, mut visit: impl FnMut(&Edge)) {
        match self {
            Self::Return { .. } | Self::Unreachable => {}
            Self::Goto(edge) => visit(edge),
            Self::Branch {
                then_target,
                else_target,
                ..
            } => {
                visit(then_target);
                visit(else_target);
            }
        }
    }

    /// Mutable counterpart to [`Self::visit_successors`].
    pub fn visit_successors_mut(&mut self, mut visit: impl FnMut(&mut Edge)) {
        match self {
            Self::Return { .. } | Self::Unreachable => {}
            Self::Goto(edge) => visit(edge),
            Self::Branch {
                then_target,
                else_target,
                ..
            } => {
                visit(then_target);
                visit(else_target);
            }
        }
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
            Self::Unreachable => "unreachable terminator operand",
        }
    }

    /// Replace the value at one concrete terminator operand slot when its use
    /// mode still agrees with the indexed use site.
    #[must_use]
    pub fn replace_operand_at(
        &mut self,
        slot: OperandSlot,
        expected: ValueId,
        mode: UseMode,
        replacement: ValueId,
    ) -> bool {
        let mut replaced = false;
        self.visit_operands_mut(|candidate, operand| {
            if candidate == slot && operand.value == expected && operand.mode == mode {
                operand.value = replacement;
                replaced = true;
            }
        });
        replaced
    }
}
