use hew_hir::{ItemId, SiteId};
use hew_parser::ast::Span;
use hew_types::{CallTarget, DefId, ResolvedTy};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpId(pub u32);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UseMode {
    Read,
    BorrowShared,
    BorrowMut,
    Move,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operand {
    pub value: ValueId,
    pub mode: UseMode,
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
    pub args: Vec<ValueId>,
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
    pub id: ItemId,
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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SemModule {
    pub functions: Vec<SemFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemOp {
    pub id: OpId,
    pub results: Vec<ValueDef>,
    pub kind: SemOpKind,
    pub provenance: Provenance,
}

/// Derived semantic effects for a value-producing SIR operation.
///
/// Effects deliberately live on operation *kinds*, not on [`SemOp`]: rewrites
/// can clone or synthesize operations without maintaining a second source of
/// truth.  The initial bit set is intentionally small; it gives early
/// canonicalization passes a sound motion/CSE barrier without committing SIR
/// to memory SSA or effect tokens.  Resolved calls currently lack an effect
/// summary, so they are conservatively [`Self::UNKNOWN_CALL`] until the call
/// ABI slice introduces one.
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
        self.contains(Self::MAY_TRAP) || self.contains(Self::UNKNOWN_CALL)
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
    Call {
        target: CallTarget,
        args: Vec<Operand>,
    },
}

impl SemOpKind {
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
            | Self::Unary { .. }
            | Self::Binary { .. }
            | Self::Cast { .. } => EffectSet::PURE,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SemTerminator {
    Return {
        value: Option<ValueId>,
    },
    Goto(Edge),
    Branch {
        condition: ValueId,
        then_target: Edge,
        else_target: Edge,
    },
    Unreachable,
}

impl SemTerminator {
    #[must_use]
    pub fn successors(&self) -> Vec<&Edge> {
        match self {
            Self::Return { .. } | Self::Unreachable => Vec::new(),
            Self::Goto(edge) => vec![edge],
            Self::Branch {
                then_target,
                else_target,
                ..
            } => vec![then_target, else_target],
        }
    }
}
