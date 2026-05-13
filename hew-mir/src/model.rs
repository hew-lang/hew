use hew_hir::{BindingId, IntentKind, SiteId, ValueClass};
use hew_types::ResolvedTy;

#[derive(Debug, Clone, PartialEq)]
pub struct IrPipeline {
    pub thir: Vec<ThirFunction>,
    pub raw_mir: Vec<RawMirFunction>,
    pub checked_mir: Vec<CheckedMirFunction>,
    pub elaborated_mir: Vec<ElaboratedMirFunction>,
    pub hew_mlir: HewMlirModule,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub statements: Vec<MirStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RawMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub blocks: Vec<BasicBlock>,
    pub decisions: Vec<DecisionFact>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub id: u32,
    pub statements: Vec<MirStatement>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CheckedMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub block: BasicBlock,
    pub decisions: Vec<DecisionFact>,
    pub checks: Vec<MirCheck>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirCheck {
    InitialisedBeforeUse,
    DecisionMapTotal,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElaboratedMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub statements: Vec<MirStatement>,
    pub decisions: Vec<DecisionFact>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirStatement {
    Bind {
        binding: BindingId,
        name: String,
        site: SiteId,
        ty: ResolvedTy,
    },
    Evaluate {
        site: SiteId,
        ty: ResolvedTy,
    },
    Return {
        site: Option<SiteId>,
        ty: ResolvedTy,
    },
    Drop {
        binding: BindingId,
        name: String,
        ty: ResolvedTy,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecisionFact {
    pub site: SiteId,
    pub value_class: ValueClass,
    pub intent: IntentKind,
    pub strategy: Strategy,
    pub why: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Strategy {
    BorrowRead,
    Move,
    CowShare,
    EnsureUnique,
    Materialize,
    ConsumeCall,
    Freeze,
    UnknownBlocked,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HewMlirModule {
    pub functions: Vec<HewMlirFunction>,
}

impl HewMlirModule {
    #[must_use]
    pub fn dump(&self) -> String {
        let mut out = String::new();
        for func in &self.functions {
            out.push_str(&func.dump());
        }
        out
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HewMlirFunction {
    pub name: String,
    pub ops: Vec<HewMlirOp>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HewMlirOp {
    Bind {
        name: String,
        ty: ResolvedTy,
        site: SiteId,
        decision: Strategy,
    },
    Read {
        ty: ResolvedTy,
        site: SiteId,
        decision: Strategy,
    },
    Return {
        ty: ResolvedTy,
        site: Option<SiteId>,
        decision: Option<Strategy>,
    },
    Drop {
        name: String,
        ty: ResolvedTy,
    },
}

impl HewMlirFunction {
    #[must_use]
    pub fn dump(&self) -> String {
        let mut out = format!("hew.func @{} {{\n", self.name);
        for op in &self.ops {
            out.push_str("  ");
            out.push_str(&op.dump());
            out.push('\n');
        }
        out.push_str("}\n");
        out
    }
}

impl HewMlirOp {
    #[must_use]
    pub fn dump(&self) -> String {
        match self {
            Self::Bind {
                name,
                ty,
                site,
                decision,
            } => format!(
                "hew.bind @{name} : {} {{hew.site_id = \"{site}\", hew.value_decision = \"{decision:?}\"}}",
                ty.user_facing()
            ),
            Self::Read { ty, site, decision } => format!(
                "hew.read : {} {{hew.site_id = \"{site}\", hew.value_decision = \"{decision:?}\"}}",
                ty.user_facing()
            ),
            Self::Return {
                ty,
                site: Some(site),
                decision: Some(decision),
            } => format!(
                "hew.return : {} {{hew.site_id = \"{site}\", hew.value_decision = \"{decision:?}\"}}",
                ty.user_facing()
            ),
            Self::Return {
                ty,
                site: None,
                decision: None,
            } => format!("hew.return : {}", ty.user_facing()),
            Self::Return { .. } => unreachable!("return MLIR op has mismatched site/decision"),
            Self::Drop { name, ty } => format!("hew.drop @{name} : {}", ty.user_facing()),
        }
    }
}
