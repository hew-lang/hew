use hew_hir::{ItemId, SiteId};
use hew_types::{CallTarget, ResolvedTy};

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
    pub name: String,
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

/// Pure, non-suspending operations in the first SIR slice.  Effects are
/// derived from this closed operation set and resolved call metadata; they are
/// intentionally not stored redundantly on each operation.
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
