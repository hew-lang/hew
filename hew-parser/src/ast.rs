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

/// A single attribute argument — either positional or key-value.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AttributeArg {
    /// Positional argument, e.g. `camelCase` in `#[json(camelCase)]`.
    Positional(String),
    /// Key-value argument, e.g. `since = 1` in `#[wire(since = 1)]`.
    KeyValue { key: String, value: String },
    /// Duration argument in nanoseconds, e.g. `5s` in `#[every(5s)]`.
    Duration(i64),
}

impl AttributeArg {
    /// Get the value as a string regardless of whether it's positional or key-value.
    /// Returns `""` for duration arguments (use [`as_duration_ns`] instead).
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            AttributeArg::Positional(s) => s,
            AttributeArg::KeyValue { value, .. } => value,
            AttributeArg::Duration(_) => "",
        }
    }

    /// If this argument is a duration literal, return the value in nanoseconds.
    #[must_use]
    pub fn as_duration_ns(&self) -> Option<i64> {
        match self {
            AttributeArg::Duration(ns) => Some(*ns),
            _ => None,
        }
    }
}

/// An attribute annotation like `#[test]` or `#[should_panic]`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Attribute {
    /// Attribute name, e.g. `"test"`, `"ignore"`, `"should_panic"`.
    pub name: String,
    /// Optional parenthesised arguments, e.g. `#[json(camelCase)]` or `#[wire(since = 1)]`.
    pub args: Vec<AttributeArg>,
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
    Function(FnDecl),
    ExternBlock(ExternBlock),
    Actor(ActorDecl),
    Supervisor(SupervisorDecl),
    Machine(MachineDecl),
    Record(RecordDecl),
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
    /// Explicit duplication prefix: `clone <operand>`.
    ///
    /// The canonical surface for producing an independent owned copy of a
    /// value. `clone` is a contextual prefix, not a reserved word — it is
    /// still usable as a method name (`x.clone()`) and a free/impl function
    /// name (`fn clone(s: string) -> string`). The parser only reads it as
    /// this prefix when it sits in operator position immediately followed by
    /// an operand token (see `token_begins_clone_operand`).
    ///
    /// The operand is read non-consumingly; the result is an owned value that
    /// drops normally. The checker resolves cloneability via the same method
    /// resolution that backs `<operand>.clone()`, and HIR lowering reuses the
    /// `.clone()` lowering path, so `clone x` and `x.clone()` share one
    /// substrate. Types without a clone path fail closed with the existing
    /// clone diagnostic rather than silently aliasing.
    Clone(Box<Spanned<Expr>>),
    Literal(Literal),
    Identifier(String),
    Tuple(Vec<Spanned<Expr>>),
    Array(Vec<Spanned<Expr>>),
    ArrayRepeat {
        value: Box<Spanned<Expr>>,
        count: Box<Spanned<Expr>>,
    },
    MapLiteral {
        entries: Vec<(Spanned<Expr>, Spanned<Expr>)>,
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
        /// Explicit turbofish type arguments: `spawn Foo<T>(...)`.
        ///
        /// Empty when the user writes `spawn Foo(...)` without a type-argument
        /// list. Generic actors require a non-empty list; the checker emits
        /// `MissingActorTypeArgs` when a generic actor is spawned without them.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        type_args: Vec<Spanned<TypeExpr>>,
        args: Vec<(String, Spanned<Expr>)>,
    },
    SpawnLambdaActor {
        is_move: bool,
        params: Vec<LambdaParam>,
        return_type: Option<Spanned<TypeExpr>>,
        body: Box<Spanned<Expr>>,
    },
    /// Structured-concurrency block: `scope { ... }`.
    ///
    /// Establishes a lexical-lifetime boundary for any tasks spawned inside.
    /// Statement-position call expressions become spawned tasks (TI-1);
    /// `fork name = call(...)` statements introduce `Task<T>` bindings (TI-2).
    /// All tasks are awaited at the closing brace.
    Scope {
        body: Block,
    },
    /// Child-task binding inside a `scope { ... }` block: `fork name = call(...)`
    /// or bare `fork call(...)`.
    ///
    /// Outside a scope this is malformed and rejected during HIR lowering.
    ForkChild {
        binding: Option<String>,
        expr: Box<Spanned<Expr>>,
    },
    /// Anonymous child-task block inside a `scope { ... }` block: `fork { ... }`.
    ForkBlock {
        body: Block,
    },
    /// Scope deadline clause inside a `scope { ... }` block: `after(duration) { ... }`.
    ScopeDeadline {
        duration: Box<Spanned<Expr>>,
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
        /// Explicit type arguments supplied at the struct literal site,
        /// e.g. `Wrapper<String> { value: "hello" }`.
        /// Absent when the user omits them and inference fills the gap.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        type_args: Option<Vec<Spanned<TypeExpr>>>,
        /// Functional-update base expression: `R { x: 5, ..base }`.
        /// When `Some`, the base is required to have the same record type as
        /// the literal; unspecified fields are filled from the base value.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        base: Option<Box<Spanned<Expr>>>,
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
    UnsafeBlock(Box<Block>),
    Yield(Option<Box<Spanned<Expr>>>),
    /// `return [expr]` in expression position — a divergent (`!`-typed) early
    /// exit usable anywhere an expression is expected (match arms, `let`
    /// initialisers, `&&`/`||` operands, `if`-as-expression branches), not just
    /// at statement position. The operand is the value returned from the
    /// enclosing function; `None` means `return` with no value (unit return).
    ///
    /// Mirrors `Stmt::Return`'s value-or-`None` shape but with no trailing `;`:
    /// in expression position the operand ends where the surrounding expression
    /// ends (before a `,`/`}`/`)`/`;` or any token that cannot begin an
    /// expression). The checker synthesizes `Ty::Never`; HIR lowers it to
    /// `HirExprKind::Return` (sibling of `HirExprKind::Break`).
    Return(Option<Box<Spanned<Expr>>>),
    This,
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

    /// `await_restart <supervised-child>` — suspend the current actor until the
    /// named supervised child's slot is Live again (it restarted), then resume
    /// with the re-fetched live `LocalPid<ChildType>`. A dedicated prefix
    /// keyword (not a method, not an extension of `await`): the supervisor
    /// analogue of `await task`, lowered through `SuspendKind::RestartWait`. The
    /// operand must be a static supervised-child accessor; a permanently-Dead
    /// child fails closed (resumes immediately) rather than hanging forever.
    AwaitRestart(Box<Spanned<Expr>>),

    /// Regex literal, e.g. `re"pattern"`.
    RegexLiteral(String),

    /// Byte string literal, e.g. `b"hello"`.
    ByteStringLiteral(Vec<u8>),
    /// Byte array literal, e.g. `bytes [0x48, 0x65]`.
    ByteArrayLiteral(Vec<u8>),

    /// Identity comparison: `lhs is rhs`.
    ///
    /// Parsed at equality precedence (same as `==`/`!=`). The checker (slice D-2)
    /// enforces that both operands carry identity (machines, actors, heap-backed
    /// collections, user named types) and rejects scalars, `String`, and records.
    /// Parser admits any expression on either side.
    Is {
        lhs: Box<Spanned<Expr>>,
        rhs: Box<Spanned<Expr>>,
    },

    /// `emit EventName { field: expr, ... }` — fire a machine event from a
    /// transition body, `entry`, or `exit` block. Legality (must appear inside
    /// a machine context) is checked at HIR lowering, not parsing.
    MachineEmit {
        event_name: String,
        fields: Vec<(String, Spanned<Expr>)>,
    },

    /// Generator block expression: `gen { yield ...; }`.
    ///
    /// The block body is lazy — it does not run until `Iterator::next` is
    /// called on the produced generator value.  Each `yield expr;` statement
    /// emits one `Some(expr)` from `next`.  When the body falls off the end
    /// the generator transitions to `Ended`; a subsequent `next` call traps.
    ///
    /// HIR/MIR/codegen lowering is not yet implemented.  The type checker
    /// infers `Generator<Yield, Return>` from yield-expression sites and the
    /// body's tail expression; an actor-receive boundary emits
    /// `E_GENBLOCK_IN_ACTOR_RECEIVE` early.  HIR lowering remains fail-closed
    /// on `GenBlock` (no coroutine state-machine variant yet), surfacing a real
    /// diagnostic instead of silently fabricating a value.
    GenBlock {
        body: Block,
    },
}

// ── Statements ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    Let {
        pattern: Spanned<Pattern>,
        ty: Option<Spanned<TypeExpr>>,
        value: Option<Spanned<Expr>>,
        /// `let Pat = expr else { <diverging block> };` — the let-else
        /// fallback. `None` for an ordinary `let`. When `Some`, a refutable
        /// pattern is admitted: the Ok-path binders escape into the enclosing
        /// scope and the else block runs (and must diverge) on a failed match.
        /// The block is carried structurally through the checker because the
        /// divergence obligation is enforced in the type checker — it cannot be
        /// desugared away in the parser.
        else_block: Option<Block>,
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
    WhileLet {
        label: Option<String>,
        pattern: Box<Spanned<Pattern>>,
        expr: Box<Spanned<Expr>>,
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
    /// Immutable non-owning borrow marker: `&T`.
    ///
    /// Lowers to `ResolvedTy::Pointer { is_mutable: false }` today
    /// (→ `ValueClass::View`).  No retain is emitted on borrow; borrow
    /// cannot outlive its owner (enforcement added in P4).
    Borrow(Box<Spanned<TypeExpr>>),
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
        rest: Option<Span>,
    },
    /// Shorthand record destructure: `{ a, b }` with no type name.
    ///
    /// The checker infers the record type from the expected/scrutinee type and
    /// delegates to the same field-binding path as `Pattern::Struct`.  Only
    /// valid in `let` positions against an irrefutable (product) type.
    RecordShorthand {
        fields: Vec<PatternField>,
        rest: Option<Span>,
    },
    Tuple(Vec<Spanned<Pattern>>),
    Or(Box<Spanned<Pattern>>, Box<Spanned<Pattern>>),
    /// A regex literal pattern in a match arm: `re"^Bearer\s+(.+)$"`.
    ///
    /// `pattern` is the normalised regex string (delimiter escapes decoded,
    /// regex backslashes preserved verbatim — see `normalize_regex_literal`).
    /// `captures` is populated by the checker from `regex::Regex::capture_names()`
    /// and is empty when produced by the parser.
    Regex {
        pattern: String,
        captures: Vec<String>,
    },
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
    /// Two's-complement wrapping add. Parser sugar for `&+`. Lowers to
    /// `Instr::IntAdd` (plain add, no overflow trap). Integer operands only;
    /// string concat and duration arithmetic are not allowed.
    WrappingAdd,
    /// Two's-complement wrapping subtract. Parser sugar for `&-`. Lowers to
    /// `Instr::IntSub` (plain sub, no overflow trap).
    WrappingSub,
    /// Two's-complement wrapping multiply. Parser sugar for `&*`. Lowers to
    /// `Instr::IntMul` (plain mul, no overflow trap).
    WrappingMul,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::BitAnd => write!(f, "&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
            Self::Range => write!(f, ".."),
            Self::RangeInclusive => write!(f, "..="),
            Self::WrappingAdd => write!(f, "&+"),
            Self::WrappingSub => write!(f, "&-"),
            Self::WrappingMul => write!(f, "&*"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Negate,
    BitNot,
    /// Raw pointer dereference (`*expr`).
    ///
    /// v0.5 endpoint: the parser recognizes this only far enough to
    /// reject it deterministically in the type checker.  Outside
    /// `unsafe { ... }` the diagnostic is `UnsafeOperationRequiresBlock`;
    /// inside `unsafe { ... }` the operation is rejected as "not lowered
    /// in v0.5" before HIR.  No HIR/MIR/codegen path is reached.
    RawDeref,
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
// serializes as just the plain i64 on the wire (backward-compatible with the codegen wire contract).
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
    /// Span of the parameter name.
    pub name_span: Span,
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
    /// `true` when this value parameter was declared with the `consume`
    /// modifier (`fn sink(consume c: Conn)`), pinning by-move ownership: the
    /// caller's binding is consumed at the call site and the callee owns the
    /// value (auto-dropped at callee scope-exit unless moved out). Only
    /// meaningful for affine `#[resource]`/`#[linear]`-typed params; `CoW` value
    /// types are unaffected. The modifier is the explicit override of the
    /// inferred borrow/consume disposition — MANDATORY where the body is not
    /// visible to inference (extern fns, trait method signatures,
    /// cross-module `pub` fns), optional elsewhere.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub is_consume: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeParam {
    pub name: String,
    pub bounds: Vec<TraitBound>,
}

/// Const-parameter element type. Phase 0 (R269=A) admits `usize` only;
/// other widths are deferred to a follow-on widening change.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConstParamTy {
    /// `const N: usize` — the only width supported in Phase 0.
    Usize,
}

/// Const-generic parameter on a machine declaration:
/// `machine M<const N: usize>` or `machine M<const N: usize = 16>`.
///
/// Default values are evaluated by the constexpr sub-engine
/// (`hew-types/src/check/const_eval.rs`) at parse-time and stored
/// here as a resolved `u64`. R270=A permits defaults.
///
/// Const parameters live alongside `TypeParam`s on `MachineDecl`; this
/// is purely additive — existing `type_params` consumers see no change.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstParam {
    pub name: String,
    pub ty: ConstParamTy,
    /// Optional `= 16` default. `None` when the parameter has no default.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AssocTypeBinding {
    pub name: String,
    pub ty: Spanned<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitBound {
    pub name: String,
    pub type_args: Option<Vec<Spanned<TypeExpr>>>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub assoc_type_bindings: Vec<AssocTypeBinding>,
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
///
/// Three tiers (D18 — standalone keyword design):
/// - `fn foo()`          → `Private`  (no keyword; module-private)
/// - `package fn foo()`  → `Package`  (visible within the package)
/// - `pub fn foo()`      → `Pub`      (globally public)
///
/// Enforcement of `Package` vs `Pub` is deferred to a future edition; both
/// currently resolve identically at all call sites that inspect `is_pub()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum Visibility {
    /// Not visible outside the defining module.
    #[default]
    Private,
    /// Fully public.
    Pub,
    /// Visible within the same package.
    Package,
}

impl Visibility {
    /// Returns `true` when the item has any public visibility (`pub` or `package`).
    ///
    /// `Package` and `Pub` are treated identically here — enforcement of the
    /// package boundary is deferred to a future edition.  Code written with
    /// `package` visibility today will keep compiling after the restriction is
    /// applied, because callers within the package are unaffected.
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
    pub name: String,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub where_clause: Option<WhereClause>,
    pub body: Block,
    pub doc_comment: Option<String>,
    /// Byte span covering the function-name token, populated by the parser.
    /// Used by the debug-info pipeline to emit the correct `DW_AT_decl_line`
    /// for impl methods instead of inheriting the enclosing impl-block span.
    #[serde(default)]
    pub decl_span: Span,
    /// Byte span from the `fn` keyword through the byte after the closing `}`.
    #[serde(default)]
    pub fn_span: Span,
    /// Intrinsic key from `#[intrinsic("name")]`, if present.
    ///
    /// When `Some`, this function is a compiler-intrinsic declaration: the body
    /// is a stub (empty block or semicolon) and the named intrinsic key is the
    /// dispatch authority used by HIR/MIR/codegen. The checker validates the key
    /// against the known intrinsic catalog at registration time.
    ///
    /// WHY: the catalog previously conjured `CompilerIntrinsic` entries with no
    /// Hew-side declaration. This field enables typed declarations that the checker
    /// can validate, making the intrinsic surface opt-in and fail-closed.
    /// WHEN-OBSOLETE: when all catalog `CompilerIntrinsic` rows have been migrated
    /// to `#[intrinsic]` declarations (slices 4–7).
    /// WHAT: add `#[intrinsic("key")]` declarations to stdlib `.hew` files and
    /// remove the corresponding catalog rows in the migration slices.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub intrinsic: Option<String>,
    /// `true` when this method declares a `consuming self` receiver — the
    /// terminal single-consume surface (`fn build(consuming self) -> T`, a
    /// `#[linear]` type's consuming method). The receiver is moved into the call
    /// and a later use of the binding surfaces `UseAfterMove`; the checker
    /// registers the method into its consume-receiver set so the move-checker
    /// marks the receiver consumed. Only meaningful on inherent-impl methods —
    /// type-body `consuming self` methods carry the same fact through
    /// `TypeDecl.consuming_methods` instead.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub consumes_self: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImportDecl {
    pub path: Vec<String>,
    pub spec: Option<ImportSpec>,
    /// Whole-module alias from `import path::to::mod as alias;`. Legal only for
    /// the whole-module form (`spec` is `None`); aliasing a brace/glob spec
    /// (`import m::{ A } as f`, `import m::* as f`) is a parse error. When
    /// present, `alias` is the qualifier a bare reference reaches the module's
    /// names through (`alias.Thing`), replacing the last path segment.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub module_alias: Option<String>,
    pub file_path: Option<String>,
    /// Resolved items from the imported file (populated by `resolve_file_imports`).
    /// Used by the type checker to register user module items under the module namespace.
    #[serde(skip)]
    pub resolved_items: Option<Vec<Spanned<Item>>>,
    /// Per-item source path for `resolved_items` (same length/order when present).
    #[serde(skip)]
    pub resolved_item_source_paths: Vec<std::path::PathBuf>,
    /// Source file paths for the resolved module (populated by `resolve_file_imports`).
    /// Multiple paths indicate a directory module with peer files.
    #[serde(skip)]
    pub resolved_source_paths: Vec<std::path::PathBuf>,
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
    pub doc_comment: Option<String>,
}

/// Ownership discipline marker placed on a type via `#[resource]` or `#[linear]`.
///
/// - `Resource`: type holds an external resource; the runtime implicitly calls
///   `fn close(consuming self) -> Result<(), E>` on scope exit.
/// - `Linear`: single-owner type with no implicit drop; every consuming method
///   declared on the type exhausts it; all live bindings must be consumed on
///   every exit path.
/// - `None`: ordinary type — no ownership discipline enforced.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum ResourceMarker {
    #[default]
    None,
    Resource,
    Linear,
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
    /// Wire protocol metadata — present when the type has `#[wire]` attribute.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub wire: Option<WireMetadata>,
    /// When true, this enum is heap-allocated behind a pointer, enabling recursive types.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub is_indirect: bool,
    /// Ownership discipline marker from `#[resource]` or `#[linear]`.
    #[serde(default, skip_serializing_if = "ResourceMarker::is_none")]
    pub resource_marker: ResourceMarker,
    /// When true, this type is an opaque pointer-width runtime handle
    /// (`#[opaque]`): empty body, surface-inaccessible (no field access, no
    /// direct construction), lowers to LLVM `ptr`, ABI-round-trips runtime
    /// `*mut T`. Orthogonal to `resource_marker` (representation vs ownership).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub is_opaque: bool,
    /// Names of methods declared with a `consuming self` receiver in this type body.
    /// Populated by the parser; used by the type checker to validate ownership rules.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub consuming_methods: Vec<String>,
}

impl ResourceMarker {
    /// Returns `true` when the marker is `None` (used for serde skip).
    #[must_use]
    pub fn is_none(&self) -> bool {
        *self == Self::None
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeAliasDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    pub ty: Spanned<TypeExpr>,
    pub doc_comment: Option<String>,
}

/// A `record` named-field declaration.
///
/// Records are pure data carriers: they hold named, typed fields and support
/// generic type parameters and where-clauses.  Methods, variants, and tuple
/// forms are not permitted (those belong to `TypeDecl`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    pub type_params: Option<Vec<TypeParam>>,
    pub where_clause: Option<WhereClause>,
    pub kind: RecordKind,
    pub doc_comment: Option<String>,
    pub span: Span,
}

/// The body of a `record` declaration.
///
/// - `Named` form: `record Point { x: int, y: int }` — at least one field required.
/// - `Tuple` form: `record Pair(int, int);` — at least one positional field required;
///   terminates with `;` like a type alias.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RecordKind {
    Named(Vec<RecordField>),
    Tuple(Vec<Spanned<TypeExpr>>),
}

/// A single field in a named-form `record` body.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordField {
    pub name: String,
    pub ty: Spanned<TypeExpr>,
    pub doc_comment: Option<String>,
    #[serde(skip)]
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeDeclKind {
    Struct,
    Enum,
}

/// Wire protocol metadata attached to a `TypeDecl` via `#[wire]`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WireMetadata {
    pub field_meta: Vec<WireFieldMeta>,
    pub reserved_numbers: Vec<u32>,
    pub json_case: Option<NamingCase>,
    pub yaml_case: Option<NamingCase>,
    /// Schema version from `#[wire(version = N)]`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<u32>,
    /// Minimum version that can decode this schema, from `#[wire(min_version = N)]`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub min_version: Option<u32>,
}

/// Per-field wire protocol metadata (auto-assigned or explicit field numbers, modifiers).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WireFieldMeta {
    pub field_name: String,
    pub field_number: u32,
    pub is_optional: bool,
    pub is_deprecated: bool,
    pub is_repeated: bool,
    pub json_name: Option<String>,
    pub yaml_name: Option<String>,
    /// Schema version that introduced this field, from `since N` modifier.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub since: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeBodyItem {
    Field {
        name: String,
        ty: Spanned<TypeExpr>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        attributes: Vec<Attribute>,
        #[serde(default)]
        doc_comment: Option<String>,
        /// Source byte range of this field (after any doc comments), used by
        /// the formatter to flush inline comments before each field.
        /// Set to `0..0` for synthetically constructed fields (e.g., wire
        /// decls) that have no source position.
        #[serde(skip)]
        span: Span,
    },
    Variant(VariantDecl),
    Method(FnDecl),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariantDecl {
    pub name: String,
    pub kind: VariantKind,
    #[serde(default)]
    pub doc_comment: Option<String>,
    /// Source byte range of this variant (after any doc comments), used by
    /// the formatter to flush inline comments before each variant.
    /// Set to `0..0` for synthetically constructed variants.
    #[serde(skip)]
    pub span: Span,
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
    /// Lang-item key from `#[lang_item("key")]`, if present.
    ///
    /// Tags this trait as a compiler-recognised "lang item" — e.g.
    /// `#[lang_item("display")]` marks the trait the f-string lowering
    /// pass dispatches through. The checker populates a side registry
    /// (`(key → trait name)`) from these tags; HIR lowering looks up
    /// the trait/method names through the registry instead of hardcoding
    /// the literal `"Display"` / `"fmt"` symbols.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lang_item: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TraitItem {
    Method(TraitMethod),
    AssociatedType {
        name: String,
        bounds: Vec<TraitBound>,
        default: Option<Spanned<TypeExpr>>,
        #[serde(default)]
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitMethod {
    pub name: String,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub where_clause: Option<WhereClause>,
    pub body: Option<Block>,
    /// Byte span from the `fn` keyword through the byte after the closing `}` or `;`.
    #[serde(default)]
    pub span: Span,
    #[serde(default)]
    pub doc_comment: Option<String>,
    /// Lang-item key from `#[lang_item("key")]`, if present.
    ///
    /// Tags this method as a compiler-recognised "lang item" — e.g.
    /// `#[lang_item("display_fmt")]` marks the method the f-string
    /// lowering pass dispatches through.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lang_item: Option<String>,
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
pub struct ExternBlock {
    pub abi: String,
    pub functions: Vec<ExternFnDecl>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExternFnDecl {
    /// Outer attributes attached to this extern fn declaration, e.g.
    /// `#[extern_symbol("hew_vec_push_{T}")]`. The parser captures the raw
    /// attribute payload; semantic validation of the template grammar and
    /// downstream `FnSig` / MIR ingest live in later compiler stages.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub is_variadic: bool,
    /// Source byte range covering this declaration. Spans from the `fn`
    /// keyword (after any doc-comment trivia) through the position
    /// immediately after the trailing `;`. The formatter uses this to
    /// flush inline comments around each extern fn. Defaults to `0..0`
    /// for synthetic decls (e.g. wire enrichment, test fixtures).
    #[serde(skip)]
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ActorDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    /// Optional generic type parameters declared as `actor Name<T, U> { ... }`
    /// or `actor Name<T: Trait> { ... }`.
    ///
    /// Matches the `MachineDecl` shape; bound enforcement is handled by the
    /// type-checker via `enforce_actor_instantiation_bounds`. Where-clause
    /// syntax on actors is deferred (actors have no matching `where_clause`
    /// field today); inline bounds suffice for v0.5.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub type_params: Vec<TypeParam>,
    pub super_traits: Option<Vec<TraitBound>>,
    pub init: Option<ActorInit>,
    pub fields: Vec<FieldDecl>,
    pub receive_fns: Vec<ReceiveFnDecl>,
    pub methods: Vec<FnDecl>,
    pub mailbox_capacity: Option<u32>,
    pub overflow_policy: Option<OverflowPolicy>,
    pub is_isolated: bool,
    pub doc_comment: Option<String>,
    /// Maximum heap bytes this actor may allocate from its arena.
    ///
    /// `None` = no `#[max_heap]` annotation (unbounded, legacy default).
    /// `Some(0)` = explicit zero, treated as unbounded by the runtime (same as
    /// arena cap=0 in `hew_arena_new_with_cap`). `Some(N)` for N > 0 = hard cap.
    ///
    /// Parse-time byte conversion: bare integer = bytes; `N kb` = N × 1024;
    /// `N mb` = N × 1024²; `N b` = N. `gb` and larger are rejected in v0.5.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_heap_bytes: Option<u64>,
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
    /// `true` when declared with `var` (mutable actor field); `false` for `let`.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub is_mutable: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default: Option<Spanned<Expr>>,
    #[serde(default)]
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReceiveFnDecl {
    pub is_generator: bool,
    pub name: String,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub where_clause: Option<WhereClause>,
    pub body: Block,
    pub span: Span,
    /// Attributes such as `#[every(5s)]` for periodic scheduling.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub attributes: Vec<Attribute>,
    #[serde(default)]
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SupervisorDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    /// Construction-time config parameters, written `supervisor App(config: T)`.
    /// In scope throughout the body (the child init-arg exprs reference them, so
    /// a child's init value can derive from runtime config). Empty when the
    /// declaration omits the `(...)` clause. Mirrors the actor `init(params)` /
    /// `spawn Actor(args)` shape — the dynamic-data source for the v0.6
    /// init-closure restart model.
    #[serde(default)]
    pub params: Vec<Param>,
    pub strategy: Option<SupervisorStrategy>,
    /// Restart-budget contract, written `intensity: N within <duration>`.
    /// Fuses the legacy `max_restarts:` + `window:` fields into one typed
    /// field so a budget can never be half-specified. `None` when the
    /// declaration omits the clause — runtime defaults apply at codegen.
    pub intensity: Option<Intensity>,
    pub children: Vec<ChildSpec>,
}

/// A supervisor restart-budget contract: at most `restarts` restarts are
/// permitted inside the rolling `window`.  Written `intensity: N within <D>`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Intensity {
    /// Maximum number of restarts permitted inside the window.
    pub restarts: i64,
    /// Window duration, retained as the raw `Token::Duration` source string
    /// (e.g. `"60s"`, `"5m"`).  Duration unit interpretation stays in codegen
    /// so the lexer remains the single source of truth for duration units.
    pub window: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SupervisorStrategy {
    OneForOne,
    OneForAll,
    RestForOne,
    /// Dynamic pool strategy: children spawned and terminated at runtime.
    /// Use with `pool name: Type` declarations.
    SimpleOneForOne,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ChildSpec {
    pub name: String,
    pub actor_type: String,
    /// Named init args for this child's actor, e.g. `child w: Worker(id: 7)`.
    /// Mirrors `Spawn.args` at the AST level: each entry is `(field_name, expr)`.
    /// Positional args (no `name:` prefix) are rejected by the parser with a
    /// migration diagnostic.
    pub args: Vec<(String, Spanned<Expr>)>,
    #[serde(default)]
    pub restart: Option<RestartPolicy>,
    /// Declarative sibling wiring: maps init-param name → sibling child name.
    /// Populated by `wired_to: { key: sibling, ... }` clauses. S-B validates
    /// that each key matches a sibling name and the ref type is compatible.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub wired_to: Option<std::collections::HashMap<String, String>>,
    /// True when declared with `pool name: Type` instead of `child name: Type`.
    /// Indicates a dynamic pool (`simple_one_for_one` strategy child).
    #[serde(default)]
    pub is_pool: bool,
    /// Per-child graceful-stop deadline, written `shutdown: <duration> |
    /// brutal_kill | infinity`.  `None` means the supervisor default applies.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub shutdown: Option<ShutdownDirective>,
    /// Byte span of the whole child declaration, from the `child`/`pool`
    /// keyword through the clause's terminating `;`/`,`. HIR lowering mints a
    /// real `SiteId` registered against this span so MIR diagnostics that have
    /// no specific argument expression to point at (a missing required field)
    /// still carry a caret at the child declaration rather than a sentinel.
    #[serde(default)]
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RestartPolicy {
    Permanent,
    Transient,
    Temporary,
}

/// Per-child shutdown directive: how long the supervisor waits for a child to
/// stop gracefully before killing it.  Written `shutdown: <duration> |
/// brutal_kill | infinity`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ShutdownDirective {
    /// Graceful-stop deadline as a raw `Token::Duration` source string
    /// (e.g. `"30s"`).  Unit interpretation stays in codegen.
    Timeout(String),
    /// Skip the graceful-stop deadline; kill the child immediately.
    BrutalKill,
    /// Wait indefinitely.  ACCEPTED-ONLY in v0.5: there is no per-child
    /// deadline wheel in the runtime yet, so this parses but is not enforced.
    /// See `format_child_spec` and the codegen note for the accepted-only seam.
    Infinity,
}

// ── Machine declarations ─────────────────────────────────────────────

/// serde `skip_serializing_if` helper: true when the count is zero, keeping
/// the additive `composite_prelude_len` field out of the wire form for the
/// common (flat / non-boundary) case.
#[expect(
    clippy::trivially_copy_pass_by_ref,
    reason = "serde `skip_serializing_if` requires a `fn(&T) -> bool` signature"
)]
fn is_zero_usize(n: &usize) -> bool {
    *n == 0
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MachineDecl {
    #[serde(default)]
    pub visibility: Visibility,
    pub name: String,
    /// Optional generic type parameters declared as `machine Name<T, U> { ... }`
    /// or `machine Name<T: Trait> { ... }`.
    ///
    /// Trait bounds are parsed and stored verbatim. Bound enforcement is
    /// handled by downstream passes (type-checker / HIR); the parser only
    /// accepts the syntax. Variance markers, defaults, and
    /// machine-over-machine generics are not yet supported (see
    /// `docs/specs/HEW-SPEC-2026.md` §3.11.8).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub type_params: Vec<TypeParam>,
    /// Const-generic parameters declared on the machine, e.g.
    /// `machine M<const N: usize>` or `machine M<T, const N: usize = 16>`.
    ///
    /// Stored in a separate vector from `type_params` (purely additive —
    /// no existing `type_params` consumer needs to change) and admits
    /// `usize` only in Phase 0 (R269=A). Per the source-position
    /// convention enforced by `parse_machine_generic_params`, all type
    /// parameters appear before any const parameters in `<...>`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub const_params: Vec<ConstParam>,
    /// Optional `where T: Trait, U: Trait + Trait, …` clause appearing
    /// between the `<…>` type-parameter list and the opening `{` of the
    /// machine body. Inline bounds on `type_params` and where-clause
    /// predicates are stored separately so downstream lowering can
    /// preserve per-entry provenance (inline vs where-clause source
    /// span); the type checker folds both forms into a single
    /// per-machine bound table consulted at instantiation sites.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub where_clause: Option<WhereClause>,
    pub states: Vec<MachineState>,
    pub events: Vec<MachineEvent>,
    /// Optional `emits { … }` Mealy-output manifest. Each entry is the name of
    /// an event the machine may produce via `emit Name { … }` in a transition
    /// body. Names reference declared `events`; the manifest is an auditable
    /// allowlist, not a second declaration site. When non-empty, the HIR
    /// cross-checks that every `emit` in a body names an event in this list.
    /// Empty when the machine declares no `emits {}` header (no cross-check).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub emits: Vec<String>,
    pub transitions: Vec<MachineTransition>,
    #[serde(default)]
    pub has_default: bool, // `default { self }` — unhandled events stay in current state
    /// Composite-state grouping side-table populated by the parser when a
    /// `state Name { … }` body contains substate declarations. Consumed only
    /// by the formatter (to re-emit composite blocks) and the diagram renderer
    /// (to draw nested boxes). Hierarchy desugars to the flat `transitions` /
    /// `states` lists at parse time — HIR/MIR/codegen never see a composite.
    /// Empty for flat machines, keeping the JSON/msgpack schema additive.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub composite_groups: Vec<CompositeGroup>,
}

/// Parser-level grouping record for a depth-1 composite state. Carries the
/// information the flat `MachineDecl` lists drop during desugaring so the
/// formatter and diagram renderer can reconstruct the nested view. Not seen by
/// HIR/MIR/codegen.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CompositeGroup {
    /// Composite (grouping) name. Not a live state — only its members are.
    pub name: String,
    /// Leaf substate names, in declaration order.
    pub members: Vec<String>,
    /// The substate marked `initial` — the state entered when the composite is
    /// targeted by name.
    pub initial: String,
    /// Composite-level `entry` block, spliced parent-then-child on entry.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub entry: Option<Block>,
    /// Composite-level `exit` block, spliced child-then-parent on exit.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exit: Option<Block>,
    /// Composite-owned shared fields (sugar: stamped onto every member). Kept
    /// here so the formatter re-emits them on the composite, not each member.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub fields: Vec<(String, Spanned<TypeExpr>)>,
    /// Parent-level transitions authored inside the composite block, retained
    /// verbatim (source `_` = any member) so the formatter re-emits them on the
    /// composite rather than as the N expanded flat copies.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub parent_transitions: Vec<MachineTransition>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MachineState {
    pub name: String,
    pub fields: Vec<(String, Spanned<TypeExpr>)>,
    /// Optional `entry { ... }` lifecycle block executed when entering this state.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub entry: Option<Block>,
    /// Optional `exit { ... }` lifecycle block executed when leaving this state.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exit: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MachineEvent {
    pub name: String,
    pub fields: Vec<(String, Spanned<TypeExpr>)>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MachineTransition {
    pub event_name: String,
    pub source_state: String,
    pub target_state: String,
    /// Event-payload field names bound in the transition head
    /// (`on E(a, b): …`). These alias the event's fields so the body can use
    /// the bare names instead of `event.field`. The parser splices a
    /// `let a = event.a;` prelude into `body` for lowering (no new HIR kind);
    /// this list is retained purely so the formatter can re-emit the head form
    /// and strip that prelude. Empty when the rule used no head binding.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub event_bindings: Vec<String>,
    /// Number of leading `body` statements that are composite entry/exit hook
    /// splices (D2/D3), prepended by the parser's composite post-pass. The
    /// formatter strips exactly this many leading statements so the authored
    /// transition (without the spliced hooks) is re-emitted; HIR/MIR see the
    /// spliced body unchanged. Zero for flat machines and non-boundary
    /// transitions, so the schema stays additive.
    #[serde(default, skip_serializing_if = "is_zero_usize")]
    pub composite_prelude_len: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub guard: Option<Spanned<Expr>>,
    pub body: Spanned<Expr>,
    /// When true, a self-transition (`source == target`) explicitly opts in to
    /// Mealy re-entry semantics: the source `exit` block and target `entry` block
    /// both run even though the state does not change.  Written as the `reenter`
    /// contextual keyword after the target state name (`=> Tgt reenter`). Without
    /// it, a non-empty self-transition body is a compile error (HIR enforces the
    /// Moore-style rule that self-loops must be annotated or empty).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub reenter: bool,
}
