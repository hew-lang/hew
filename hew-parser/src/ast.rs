//! Abstract syntax tree types for the Hew language.

use serde::{Deserialize, Serialize};

/// Source span with byte offsets.
pub type Span = std::ops::Range<usize>;

/// A value with an associated source span.
pub type Spanned<T> = (T, Span);

// ── Program ──────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub items: Vec<Spanned<Item>>,
    pub module_doc: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub module_graph: Option<crate::module::ModuleGraph>,
}

// ── Attributes ───────────────────────────────────────────────────────

/// An attribute annotation like `#[test]` or `#[should_panic]`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Attribute {
    /// Attribute name, e.g. `"test"`, `"ignore"`, `"should_panic"`.
    pub name: String,
    /// Optional parenthesised arguments, e.g. `#[cfg(feature = "x")]`.
    pub args: Vec<String>,
    /// Source span of the attribute (from `#` through `]`).
    pub span: Span,
}

// ── Items (top-level declarations) ───────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Item {
    Import(ImportDecl),
    Const(ConstDecl),
    TypeDecl(TypeDecl),
    TypeAlias(TypeAliasDecl),
    Trait(TraitDecl),
    Impl(ImplDecl),
    Wire(WireDecl),
    Function(FnDecl),
    ExternBlock(ExternBlock),
    Actor(ActorDecl),
    Supervisor(SupervisorDecl),
}

// ── Expressions ──────────────────────────────────────────────────────

/// A part of an interpolated (f-string / template literal) string.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StringPart {
    /// Literal text segment.
    Literal(String),
    /// Expression to be evaluated and converted to string.
    Expr(Spanned<Expr>),
}

/// A function call argument — either positional or named (`name: expr`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CallArg {
    /// A positional argument: just an expression.
    Positional(Spanned<Expr>),
    /// A named argument: `name: expr`.
    Named { name: String, value: Spanned<Expr> },
}

impl CallArg {
    /// Extract the expression and span, ignoring whether it's named or positional.
    #[must_use]
    pub fn expr(&self) -> &Spanned<Expr> {
        match self {
            CallArg::Positional(e) => e,
            CallArg::Named { value, .. } => value,
        }
    }

    /// Extract the expression and span mutably, ignoring whether it's named or positional.
    pub fn expr_mut(&mut self) -> &mut Spanned<Expr> {
        match self {
            CallArg::Positional(e) => e,
            CallArg::Named { value, .. } => value,
        }
    }

    /// Get the name if this is a named argument.
    #[must_use]
    pub fn name(&self) -> Option<&str> {
        match self {
            CallArg::Positional(_) => None,
            CallArg::Named { name, .. } => Some(name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Binary {
        left: Box<Spanned<Expr>>,
        op: BinaryOp,
        right: Box<Spanned<Expr>>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Spanned<Expr>>,
    },
    Literal(Literal),
    Identifier(String),
    Tuple(Vec<Spanned<Expr>>),
    Array(Vec<Spanned<Expr>>),
    ArrayRepeat {
        value: Box<Spanned<Expr>>,
        count: Box<Spanned<Expr>>,
    },
    Block(Block),
    If {
        condition: Box<Spanned<Expr>>,
        then_block: Box<Spanned<Expr>>,
        else_block: Option<Box<Spanned<Expr>>>,
    },
    IfLet {
        pattern: Box<Spanned<Pattern>>,
        expr: Box<Spanned<Expr>>,
        body: Block,
        else_body: Option<Block>,
    },
    Match {
        scrutinee: Box<Spanned<Expr>>,
        arms: Vec<MatchArm>,
    },
    Lambda {
        is_move: bool,
        type_params: Option<Vec<TypeParam>>,
        params: Vec<LambdaParam>,
        return_type: Option<Spanned<TypeExpr>>,
        body: Box<Spanned<Expr>>,
    },
    Spawn {
        target: Box<Spanned<Expr>>,
        args: Vec<(String, Spanned<Expr>)>,
    },
    SpawnLambdaActor {
        is_move: bool,
        params: Vec<LambdaParam>,
        return_type: Option<Spanned<TypeExpr>>,
        body: Box<Spanned<Expr>>,
    },
    Scope {
        binding: Option<String>,
        body: Block,
    },
    InterpolatedString(Vec<StringPart>),
    Call {
        function: Box<Spanned<Expr>>,
        type_args: Option<Vec<Spanned<TypeExpr>>>,
        args: Vec<CallArg>,
        is_tail_call: bool,
    },
    MethodCall {
        receiver: Box<Spanned<Expr>>,
        method: String,
        args: Vec<CallArg>,
    },
    StructInit {
        name: String,
        fields: Vec<(String, Spanned<Expr>)>,
    },
    Send {
        target: Box<Spanned<Expr>>,
        message: Box<Spanned<Expr>>,
    },
    Select {
        arms: Vec<SelectArm>,
        timeout: Option<Box<TimeoutClause>>,
    },
    Join(Vec<Spanned<Expr>>),
    Timeout {
        expr: Box<Spanned<Expr>>,
        duration: Box<Spanned<Expr>>,
    },
    Unsafe(Block),
    Yield(Option<Box<Spanned<Expr>>>),
    Cooperate,
    FieldAccess {
        object: Box<Spanned<Expr>>,
        field: String,
    },
    Index {
        object: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },
    Cast {
        expr: Box<Spanned<Expr>>,
        ty: Spanned<TypeExpr>,
    },
    PostfixTry(Box<Spanned<Expr>>),
    Range {
        start: Option<Box<Spanned<Expr>>>,
        end: Option<Box<Spanned<Expr>>>,
        inclusive: bool,
    },
    Await(Box<Spanned<Expr>>),
    ScopeLaunch(Block),
    ScopeSpawn(Block),
    ScopeCancel,

    /// Regex literal, e.g. `re"pattern"`.
    RegexLiteral(String),
}

// ── Statements ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    Let {
        pattern: Spanned<Pattern>,
        ty: Option<Spanned<TypeExpr>>,
        value: Option<Spanned<Expr>>,
    },
    Var {
        name: String,
        ty: Option<Spanned<TypeExpr>>,
        value: Option<Spanned<Expr>>,
    },
    Assign {
        target: Spanned<Expr>,
        op: Option<CompoundAssignOp>,
        value: Spanned<Expr>,
    },
    If {
        condition: Spanned<Expr>,
        then_block: Block,
        else_block: Option<ElseBlock>,
    },
    IfLet {
        pattern: Box<Spanned<Pattern>>,
        expr: Box<Spanned<Expr>>,
        body: Block,
        else_body: Option<Block>,
    },
    Match {
        scrutinee: Spanned<Expr>,
        arms: Vec<MatchArm>,
    },
    Loop {
        label: Option<String>,
        body: Block,
    },
    For {
        label: Option<String>,
        is_await: bool,
        pattern: Spanned<Pattern>,
        iterable: Spanned<Expr>,
        body: Block,
    },
    While {
        label: Option<String>,
        condition: Spanned<Expr>,
        body: Block,
    },
    Break {
        label: Option<String>,
        value: Option<Spanned<Expr>>,
    },
    Continue {
        label: Option<String>,
    },
    Return(Option<Spanned<Expr>>),
    Defer(Box<Spanned<Expr>>),
    Expression(Spanned<Expr>),
}

// ── Type expressions ─────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeExpr {
    Named {
        name: String,
        type_args: Option<Vec<Spanned<TypeExpr>>>,
    },
    Result {
        ok: Box<Spanned<TypeExpr>>,
        err: Box<Spanned<TypeExpr>>,
    },
    Option(Box<Spanned<TypeExpr>>),
    Tuple(Vec<Spanned<TypeExpr>>),
    Array {
        element: Box<Spanned<TypeExpr>>,
        size: u64,
    },
    Slice(Box<Spanned<TypeExpr>>),
    Function {
        params: Vec<Spanned<TypeExpr>>,
        return_type: Box<Spanned<TypeExpr>>,
    },
    Pointer {
        is_mutable: bool,
        pointee: Box<Spanned<TypeExpr>>,
    },
    TraitObject(Vec<TraitBound>),
    Infer,
}

// ── Patterns ─────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard,
    Literal(Literal),
    Identifier(String),
    Constructor {
        name: String,
        patterns: Vec<Spanned<Pattern>>,
    },
    Struct {
        name: String,
        fields: Vec<PatternField>,
    },
    Tuple(Vec<Spanned<Pattern>>),
    Or(Box<Spanned<Pattern>>, Box<Spanned<Pattern>>),
}

// ── Supporting types ─────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
    pub trailing_expr: Option<Box<Spanned<Expr>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Range,
    RangeInclusive,
    Send,
    /// Regex match: `string =~ regex`.
    RegexMatch,
    /// Regex non-match: `string !~ regex`.
    RegexNotMatch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Negate,
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompoundAssignOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

/// Radix of an integer literal for faithful round-trip formatting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IntRadix {
    Decimal,
    Hex,
    Octal,
    Binary,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer {
        value: i64,
        radix: IntRadix,
    },
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    /// Duration literal in nanoseconds (e.g. `100ns` → 100, `5s` → `5_000_000_000`).
    Duration(i64),
}

// Custom Serialize/Deserialize for Literal so that Integer { value, radix }
// serializes as just the plain i64 on the wire (backward-compatible with C++ codegen).
// The radix field is only used by the Rust-side formatter and is not sent over MessagePack.
impl serde::Serialize for Literal {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        #[derive(serde::Serialize)]
        enum LiteralWire<'a> {
            Integer(i64),
            Float(f64),
            String(&'a str),
            Bool(bool),
            Char(char),
            Duration(i64),
        }
        match self {
            Literal::Integer { value, .. } => LiteralWire::Integer(*value),
            Literal::Float(v) => LiteralWire::Float(*v),
            Literal::String(s) => LiteralWire::String(s),
            Literal::Bool(b) => LiteralWire::Bool(*b),
            Literal::Char(c) => LiteralWire::Char(*c),
            Literal::Duration(d) => LiteralWire::Duration(*d),
        }
        .serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Literal {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        enum LiteralWire {
            Integer(i64),
            Float(f64),
            String(String),
            Bool(bool),
            Char(char),
            Duration(i64),
        }
        let wire = LiteralWire::deserialize(deserializer)?;
        Ok(match wire {
            LiteralWire::Integer(v) => Literal::Integer {
                value: v,
                radix: IntRadix::Decimal,
            },
            LiteralWire::Float(v) => Literal::Float(v),
            LiteralWire::String(s) => Literal::String(s),
            LiteralWire::Bool(b) => Literal::Bool(b),
            LiteralWire::Char(c) => Literal::Char(c),
            LiteralWire::Duration(d) => Literal::Duration(d),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub guard: Option<Spanned<Expr>>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SelectArm {
    pub binding: Spanned<Pattern>,
    pub source: Spanned<Expr>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TimeoutClause {
    pub duration: Box<Spanned<Expr>>,
    pub body: Box<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LambdaParam {
    pub name: String,
    pub ty: Option<Spanned<TypeExpr>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PatternField {
    pub name: String,
    pub pattern: Option<Spanned<Pattern>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ElseBlock {
    pub is_if: bool,
    pub if_stmt: Option<Box<Spanned<Stmt>>>,
    pub block: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Param {
    pub name: String,
    pub ty: Spanned<TypeExpr>,
    pub is_mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeParam {
    pub name: String,
    pub bounds: Vec<TraitBound>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitBound {
    pub name: String,
    pub type_args: Option<Vec<Spanned<TypeExpr>>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WherePredicate {
    pub ty: Spanned<TypeExpr>,
    pub bounds: Vec<TraitBound>,
}

// ── Visibility ───────────────────────────────────────────────────────

/// Item visibility level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    /// Not visible outside the defining module.
    Private,
    /// Fully public.
    Pub,
    /// Visible within the same package.
    PubPackage,
    /// Visible to the parent module.
    PubSuper,
}

impl Default for Visibility {
    fn default() -> Self {
        Self::Private
    }
}

impl Visibility {
    /// Returns `true` when the item has any public visibility (`pub`, `pub(package)`, or `pub(super)`).
    #[must_use]
    pub fn is_pub(self) -> bool {
        self != Self::Private
    }
}

// ── Item-level types ─────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FnDecl {
    pub attributes: Vec<Attribute>,
    pub is_async: bool,
    pub is_generator: bool,
    #[serde(default)]
    pub visibility: Visibility,
    pub is_pure: bool,
    pub name: String,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub where_clause: Option<WhereClause>,
    pub body: Block,
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImportDecl {
    pub path: Vec<String>,
    pub spec: Option<ImportSpec>,
    pub file_path: Option<String>,
    /// Resolved items from the imported file (populated by `resolve_file_imports`).
    /// Used by the type checker to register user module items under the module namespace.
    #[serde(skip)]
    pub resolved_items: Option<Vec<Spanned<Item>>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImportName {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ImportSpec {
    Glob,
    Names(Vec<ImportName>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    pub ty: Spanned<TypeExpr>,
    pub value: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub kind: TypeDeclKind,
    pub name: String,
    pub type_params: Option<Vec<TypeParam>>,
    pub where_clause: Option<WhereClause>,
    pub body: Vec<TypeBodyItem>,
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeAliasDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    pub ty: Spanned<TypeExpr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeDeclKind {
    Struct,
    Enum,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeBodyItem {
    Field { name: String, ty: Spanned<TypeExpr> },
    Variant(VariantDecl),
    Method(FnDecl),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariantDecl {
    pub name: String,
    pub kind: VariantKind,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VariantKind {
    Unit,
    Tuple(Vec<Spanned<TypeExpr>>),
    Struct(Vec<(String, Spanned<TypeExpr>)>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    pub type_params: Option<Vec<TypeParam>>,
    pub super_traits: Option<Vec<TraitBound>>,
    pub items: Vec<TraitItem>,
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TraitItem {
    Method(TraitMethod),
    AssociatedType {
        name: String,
        bounds: Vec<TraitBound>,
        default: Option<Spanned<TypeExpr>>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitMethod {
    pub name: String,
    pub is_pure: bool,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub where_clause: Option<WhereClause>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImplDecl {
    pub type_params: Option<Vec<TypeParam>>,
    pub trait_bound: Option<TraitBound>,
    pub target_type: Spanned<TypeExpr>,
    pub where_clause: Option<WhereClause>,
    #[serde(default)]
    pub type_aliases: Vec<ImplTypeAlias>,
    pub methods: Vec<FnDecl>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImplTypeAlias {
    pub name: String,
    pub ty: Spanned<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WireDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub kind: WireDeclKind,
    pub name: String,
    pub fields: Vec<WireFieldDecl>,
    pub variants: Vec<VariantDecl>,
    /// Struct-level JSON key naming convention (`#[json(camelCase)]` etc.).
    pub json_case: Option<NamingCase>,
    /// Struct-level YAML key naming convention (`#[yaml(snake_case)]` etc.).
    pub yaml_case: Option<NamingCase>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WireDeclKind {
    Struct,
    Enum,
}

/// Naming case convention for JSON/YAML struct-level key transformation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NamingCase {
    CamelCase,
    PascalCase,
    SnakeCase,
    ScreamingSnake,
    KebabCase,
}

impl NamingCase {
    /// Parse a naming case from an attribute argument string (e.g. `"camelCase"`).
    #[must_use]
    pub fn from_attr(s: &str) -> Option<Self> {
        match s {
            "camelCase" | "camel" => Some(Self::CamelCase),
            "PascalCase" | "pascal" => Some(Self::PascalCase),
            "snake_case" | "snake" => Some(Self::SnakeCase),
            "SCREAMING_SNAKE" | "screaming_snake" => Some(Self::ScreamingSnake),
            "kebab-case" | "kebab" => Some(Self::KebabCase),
            _ => None,
        }
    }

    /// Canonical attribute string for this naming case.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::CamelCase => "camelCase",
            Self::PascalCase => "PascalCase",
            Self::SnakeCase => "snake_case",
            Self::ScreamingSnake => "SCREAMING_SNAKE",
            Self::KebabCase => "kebab-case",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "mirrors wire format field attributes"
)]
pub struct WireFieldDecl {
    pub name: String,
    pub ty: String,
    pub field_number: u32,
    pub is_optional: bool,
    pub is_repeated: bool,
    pub is_reserved: bool,
    pub is_deprecated: bool,
    /// Per-field JSON key override (`json("name")`).
    pub json_name: Option<String>,
    /// Per-field YAML key override (`yaml("name")`).
    pub yaml_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExternBlock {
    pub abi: String,
    pub functions: Vec<ExternFnDecl>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExternFnDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub is_variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ActorDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    pub super_traits: Option<Vec<TraitBound>>,
    pub init: Option<ActorInit>,
    pub fields: Vec<FieldDecl>,
    pub receive_fns: Vec<ReceiveFnDecl>,
    pub methods: Vec<FnDecl>,
    pub mailbox_capacity: Option<u32>,
    pub overflow_policy: Option<OverflowPolicy>,
    pub is_isolated: bool,
    pub doc_comment: Option<String>,
}

/// Mailbox overflow policy.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum OverflowPolicy {
    DropNew,
    DropOld,
    Block,
    Fail,
    Coalesce {
        key_field: String,
        fallback: Option<OverflowFallback>,
    },
}

/// Fallback policy for coalesce when no matching key is found.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum OverflowFallback {
    DropNew,
    DropOld,
    Block,
    Fail,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ActorInit {
    pub params: Vec<Param>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldDecl {
    pub name: String,
    pub ty: Spanned<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReceiveFnDecl {
    pub is_generator: bool,
    pub is_pure: bool,
    pub name: String,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub where_clause: Option<WhereClause>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SupervisorDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    pub strategy: Option<SupervisorStrategy>,
    pub max_restarts: Option<i64>,
    pub window: Option<String>,
    pub children: Vec<ChildSpec>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SupervisorStrategy {
    OneForOne,
    OneForAll,
    RestForOne,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ChildSpec {
    pub name: String,
    pub actor_type: String,
    pub args: Vec<Spanned<Expr>>,
    pub restart: Option<RestartPolicy>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RestartPolicy {
    Permanent,
    Transient,
    Temporary,
}
