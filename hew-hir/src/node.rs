use hew_parser::ast::{BinaryOp, Span};
use hew_types::ResolvedTy;

use crate::ids::{BindingId, HirNodeId, ItemId, ResolvedRef, ScopeId, SiteId};
use crate::{IntentKind, ValueClass};

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    pub items: Vec<HirItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirItem {
    Function(HirFn),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFn {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<HirBinding>,
    pub return_ty: ResolvedTy,
    pub body: HirBlock,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirBinding {
    pub id: BindingId,
    pub name: String,
    pub ty: ResolvedTy,
    pub mutable: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirBlock {
    pub node: HirNodeId,
    pub scope: ScopeId,
    pub statements: Vec<HirStmt>,
    pub tail: Option<Box<HirExpr>>,
    pub ty: ResolvedTy,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirStmt {
    pub node: HirNodeId,
    pub kind: HirStmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStmtKind {
    Let(HirBinding, Option<HirExpr>),
    Expr(HirExpr),
    Return(Option<HirExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirExpr {
    pub node: HirNodeId,
    pub site: SiteId,
    pub ty: ResolvedTy,
    pub value_class: ValueClass,
    pub intent: IntentKind,
    pub kind: HirExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExprKind {
    Literal(HirLiteral),
    BindingRef {
        name: String,
        resolved: ResolvedRef,
    },
    Binary {
        op: BinaryOp,
        left: Box<HirExpr>,
        right: Box<HirExpr>,
    },
    Call {
        callee: Box<HirExpr>,
        args: Vec<HirExpr>,
    },
    Block(HirBlock),
    If {
        condition: Box<HirExpr>,
        then_expr: Box<HirExpr>,
        else_expr: Option<Box<HirExpr>>,
    },
    StructInit {
        name: String,
        type_args: Vec<ResolvedTy>,
        fields: Vec<(String, HirExpr)>,
    },
    Unsupported(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirLiteral {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    Duration(i64),
    Unit,
}
