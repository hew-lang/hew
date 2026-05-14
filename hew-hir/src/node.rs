use hew_parser::ast::{BinaryOp, ResourceMarker, Span};
use hew_types::ResolvedTy;

use crate::ids::{BindingId, HirNodeId, ItemId, ResolvedRef, ScopeId, SiteId};
use crate::value_class::TypeClassTable;
use crate::{IntentKind, ValueClass};

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    pub items: Vec<HirItem>,
    /// Per-named-type classification table populated during HIR lowering from
    /// each `Item::TypeDecl` carrying a `#[resource]` or `#[linear]` marker.
    /// Keyed by type name; value is `(marker, close_method_name)` where
    /// `close_method_name` is `Some(name)` for `@resource` types (the
    /// consuming method named `close`) and `None` for `@linear` and `None`-
    /// marked types.
    ///
    /// This is the single authority for downstream phases asking "is this
    /// Named type a resource/linear?" — `ValueClass::of_ty(ty, &type_classes)`
    /// reads from here. No phase re-derives the answer by walking the parser
    /// AST. LESSONS: `type-info-survival`.
    pub type_classes: TypeClassTable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirItem {
    Function(HirFn),
    TypeDecl(HirTypeDecl),
}

/// Lowered top-level type declaration.
///
/// Carries the `#[resource]` / `#[linear]` marker (if any), the list of
/// consuming-method names lifted from the parser body, and the field set.
/// The field set is not consumed by the v0.5 vertical-slice MIR but is
/// recorded for snapshot stability and future analysis passes.
#[derive(Debug, Clone, PartialEq)]
pub struct HirTypeDecl {
    pub id: ItemId,
    pub node: HirNodeId,
    pub name: String,
    pub marker: ResourceMarker,
    /// Names of methods declared with a `consuming self` receiver in the
    /// type body. Lifted verbatim from `TypeDecl.consuming_methods`.
    pub consuming_methods: Vec<String>,
    pub fields: Vec<HirField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirField {
    pub name: String,
    pub ty: ResolvedTy,
    pub span: Span,
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
