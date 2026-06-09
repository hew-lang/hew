//===- ast_types.h - C++ AST types for Hew (msgpack deserialization) ------===//
//
// Mirror of the Rust AST types from hew-parser/src/ast.rs.
// These types are deserialized from msgpack (via rmp_serde on the Rust side).
//
// Serde serialization format (rmp_serde::to_vec_named):
//   - Structs → msgpack map with string keys
//   - Enums (externally tagged) → {"VariantName": payload}
//     - Struct variants → {"Variant": {"field1": ..., "field2": ...}}
//     - Newtype variants → {"Variant": value}
//     - Unit variants → "Variant"
//   - Tuples → msgpack array
//   - Spanned<T> = (T, Range<usize>) → [T, {"start": N, "end": N}]
//   - Option<T> → null or T
//   - Vec<T> → msgpack array
//
//===----------------------------------------------------------------------===//

#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace hew {
namespace ast {

// ── Visibility ────────────────────────────────────────────────────────────

/// Item visibility level (mirrors Rust Visibility enum).
enum class Visibility : int {
  Private = 0,
  Pub = 1,
  PubPackage = 2,
  PubSuper = 3,
};

/// Returns true if the visibility is any form of public.
inline bool is_pub(Visibility v) {
  return v != Visibility::Private;
}

// ── Span ──────────────────────────────────────────────────────────────────

/// Source span with byte offsets (mirrors Rust's Range<usize>).
struct Span {
  uint64_t start = 0;
  uint64_t end = 0;
};

/// A value with an associated source span (mirrors Rust's Spanned<T> = (T, Span)).
template <typename T> struct Spanned {
  T value;
  Span span;
};

// Forward declarations
struct Expr;
struct Stmt;
struct Block;
struct TypeExpr;
struct Pattern;
struct TypeParam;

// ── Attributes ────────────────────────────────────────────────────────────

struct AttributeArgPositional {
  std::string value;
};

struct AttributeArgKeyValue {
  std::string key;
  std::string value;
};

struct AttributeArg {
  std::variant<AttributeArgPositional, AttributeArgKeyValue> kind;

  /// Get the string value regardless of positional/key-value.
  const std::string &as_str() const {
    return std::visit([](const auto &v) -> const std::string & { return v.value; }, kind);
  }
};

struct Attribute {
  std::string name;
  std::vector<AttributeArg> args;
  Span span;
};

// ── Literals ──────────────────────────────────────────────────────────────

struct LitInteger {
  int64_t value;
};
struct LitFloat {
  double value;
};
struct LitString {
  std::string value;
};
struct LitBool {
  bool value;
};
struct LitChar {
  char32_t value;
}; // serde serializes char as string
struct LitDuration {
  int64_t value; // nanoseconds
};

using Literal = std::variant<LitInteger, LitFloat, LitString, LitBool, LitChar, LitDuration>;

// ── Binary / Unary / CompoundAssign operators ─────────────────────────────

enum class BinaryOp {
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
  WrappingAdd,
  WrappingSub,
  WrappingMul,
};

enum class UnaryOp {
  Not,
  Negate,
  BitNot,
};

enum class CompoundAssignOp {
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
};

// ── Type expressions ──────────────────────────────────────────────────────

struct TypeNamed {
  std::string name;
  std::optional<std::vector<Spanned<TypeExpr>>> type_args;
};
struct TypeResult {
  std::unique_ptr<Spanned<TypeExpr>> ok;
  std::unique_ptr<Spanned<TypeExpr>> err;
};
struct TypeOption {
  std::unique_ptr<Spanned<TypeExpr>> inner;
};
struct TypeTuple {
  std::vector<Spanned<TypeExpr>> elements;
};
struct TypeArray {
  std::unique_ptr<Spanned<TypeExpr>> element;
  uint64_t size;
};
struct TypeSlice {
  std::unique_ptr<Spanned<TypeExpr>> inner;
};
struct TypeFunction {
  std::vector<Spanned<TypeExpr>> params;
  std::unique_ptr<Spanned<TypeExpr>> return_type;
};
struct TypePointer {
  bool is_mutable;
  std::unique_ptr<Spanned<TypeExpr>> pointee;
};

// Forward declare TraitBound
struct TraitBound;

struct TypeTraitObject {
  std::vector<TraitBound> bounds;
};

struct TypeInfer {};

struct TypeExpr {
  std::variant<TypeNamed, TypeResult, TypeOption, TypeTuple, TypeArray, TypeSlice, TypeFunction,
               TypePointer, TypeTraitObject, TypeInfer>
      kind;
};

// ── Trait bound ───────────────────────────────────────────────────────────

struct TraitBound {
  std::string name;
  std::optional<std::vector<Spanned<TypeExpr>>> type_args;
};

// ── Patterns ──────────────────────────────────────────────────────────────
// Pattern and its sub-types are mutually recursive, so we use unique_ptr
// to break the cycle (unique_ptr works with incomplete types).

struct PatWildcard {};
struct PatLiteral {
  Literal lit;
};
struct PatIdentifier {
  std::string name;
};
struct PatConstructor {
  std::string name;
  std::vector<std::unique_ptr<Spanned<Pattern>>> patterns;
};

struct PatternField {
  std::string name;
  std::unique_ptr<Spanned<Pattern>> pattern; // optional via nullptr
};

struct PatStruct {
  std::string name;
  std::vector<PatternField> fields;
};

struct PatTuple {
  std::vector<std::unique_ptr<Spanned<Pattern>>> elements;
};
struct PatOr {
  std::unique_ptr<Spanned<Pattern>> left;
  std::unique_ptr<Spanned<Pattern>> right;
};

struct Pattern {
  std::variant<PatWildcard, PatLiteral, PatIdentifier, PatConstructor, PatStruct, PatTuple, PatOr>
      kind;
};

// ── String interpolation parts ────────────────────────────────────────────

struct StringPartLiteral {
  std::string text;
};
struct StringPartExpr {
  std::unique_ptr<Spanned<Expr>> expr;
};

using StringPart = std::variant<StringPartLiteral, StringPartExpr>;

// ── Match / Select / Timeout ──────────────────────────────────────────────
// Pattern is complete here so Spanned<Pattern> can be by-value.
// Expr is incomplete at this point, so Spanned<Expr> uses unique_ptr.

struct MatchArm {
  Spanned<Pattern> pattern;
  std::unique_ptr<Spanned<Expr>> guard; // nullptr if no guard
  std::unique_ptr<Spanned<Expr>> body;
};

struct SelectArm {
  Spanned<Pattern> binding;
  std::unique_ptr<Spanned<Expr>> source;
  std::unique_ptr<Spanned<Expr>> body;
};

struct TimeoutClause {
  std::unique_ptr<Spanned<Expr>> duration;
  std::unique_ptr<Spanned<Expr>> body;
};

// ── Lambda param ──────────────────────────────────────────────────────────

struct LambdaParam {
  std::string name;
  std::optional<Spanned<TypeExpr>> ty;
};

// ── Block ─────────────────────────────────────────────────────────────────
// Uses unique_ptr to Spanned because Stmt/Expr are incomplete here.

struct Block {
  std::vector<std::unique_ptr<Spanned<Stmt>>> stmts;
  std::unique_ptr<Spanned<Expr>> trailing_expr; // nullptr if none
};

// ── Else block ────────────────────────────────────────────────────────────

struct ElseBlock {
  bool is_if;
  std::unique_ptr<Spanned<Stmt>> if_stmt; // nullptr if none
  std::optional<Block> block;
};

// ── Expressions ───────────────────────────────────────────────────────────

struct ExprBinary {
  std::unique_ptr<Spanned<Expr>> left;
  BinaryOp op;
  std::unique_ptr<Spanned<Expr>> right;
};
struct ExprUnary {
  UnaryOp op;
  std::unique_ptr<Spanned<Expr>> operand;
};
struct ExprLiteral {
  Literal lit;
};
struct ExprIdentifier {
  std::string name;
};
struct ExprTuple {
  std::vector<std::unique_ptr<Spanned<Expr>>> elements;
};
struct ExprArray {
  std::vector<std::unique_ptr<Spanned<Expr>>> elements;
};
struct ExprBlock {
  Block block;
};
struct ExprIf {
  std::unique_ptr<Spanned<Expr>> condition;
  std::unique_ptr<Spanned<Expr>> then_block;
  std::optional<std::unique_ptr<Spanned<Expr>>> else_block;
};
struct ExprIfLet {
  Spanned<Pattern> pattern;
  std::unique_ptr<Spanned<Expr>> expr;
  Block body;
  std::optional<Block> else_body;
};
struct ExprMatch {
  std::unique_ptr<Spanned<Expr>> scrutinee;
  std::vector<MatchArm> arms;
};
struct ExprLambda {
  bool is_move;
  std::optional<std::vector<TypeParam>> type_params;
  std::vector<LambdaParam> params;
  std::optional<Spanned<TypeExpr>> return_type;
  std::unique_ptr<Spanned<Expr>> body;
};
struct ExprSpawn {
  std::unique_ptr<Spanned<Expr>> target;
  std::vector<std::pair<std::string, std::unique_ptr<Spanned<Expr>>>> args;
};
struct ExprSpawnLambdaActor {
  bool is_move;
  std::vector<LambdaParam> params;
  std::optional<Spanned<TypeExpr>> return_type;
  std::unique_ptr<Spanned<Expr>> body;
};
struct ExprScope {
  Block block;
};
struct ExprInterpolatedString {
  std::vector<StringPart> parts;
};
/// A positional function call argument.
struct CallArgPositional {
  std::unique_ptr<Spanned<Expr>> expr;
};

/// A named function call argument (name: expr).
struct CallArgNamed {
  std::string name;
  std::unique_ptr<Spanned<Expr>> value;
};

/// A function call argument — positional or named.
using CallArg = std::variant<CallArgPositional, CallArgNamed>;

struct ExprCall {
  std::unique_ptr<Spanned<Expr>> function;
  std::optional<std::vector<Spanned<TypeExpr>>> type_args;
  std::vector<CallArg> args;
  bool is_tail_call;
};
struct ExprMethodCall {
  std::unique_ptr<Spanned<Expr>> receiver;
  std::string method;
  std::vector<CallArg> args;
};
struct ExprStructInit {
  std::string name;
  std::vector<std::pair<std::string, std::unique_ptr<Spanned<Expr>>>> fields;
  /// Explicit type arguments from the struct literal, e.g. `Wrapper<String> { ... }`.
  /// Absent when the user omits them (backward-compatible: old wire payloads have no
  /// `type_args` key and deserialize to `std::nullopt`).
  std::optional<std::vector<Spanned<TypeExpr>>> type_args;
};
struct ExprSelect {
  std::vector<SelectArm> arms;
  std::optional<std::unique_ptr<TimeoutClause>> timeout;
};
struct ExprJoin {
  std::vector<std::unique_ptr<Spanned<Expr>>> exprs;
};
struct ExprTimeout {
  std::unique_ptr<Spanned<Expr>> expr;
  std::unique_ptr<Spanned<Expr>> duration;
};
struct ExprUnsafe {
  Block block;
};
struct ExprYield {
  std::optional<std::unique_ptr<Spanned<Expr>>> value;
};
struct ExprCooperate {};
struct ExprThis {};
struct ExprFieldAccess {
  std::unique_ptr<Spanned<Expr>> object;
  std::string field;
};
struct ExprIndex {
  std::unique_ptr<Spanned<Expr>> object;
  std::unique_ptr<Spanned<Expr>> index;
};
struct ExprCast {
  std::unique_ptr<Spanned<Expr>> expr;
  Spanned<TypeExpr> ty;
};
struct ExprPostfixTry {
  std::unique_ptr<Spanned<Expr>> inner;
};
struct ExprRange {
  std::optional<std::unique_ptr<Spanned<Expr>>> start;
  std::optional<std::unique_ptr<Spanned<Expr>>> end;
  bool inclusive;
};
struct ExprAwait {
  std::unique_ptr<Spanned<Expr>> inner;
};
struct ExprRegexLiteral {
  std::string pattern;
};
struct ExprArrayRepeat {
  std::unique_ptr<Spanned<Expr>> value;
  std::unique_ptr<Spanned<Expr>> count;
};
struct ExprByteStringLiteral {
  std::vector<uint8_t> data;
};
struct ExprByteArrayLiteral {
  std::vector<uint8_t> data;
};
struct ExprMapEntry {
  std::unique_ptr<Spanned<Expr>> key;
  std::unique_ptr<Spanned<Expr>> value;
};
struct ExprMapLiteral {
  std::vector<ExprMapEntry> entries;
};
struct ExprIs {
  std::unique_ptr<Spanned<Expr>> lhs;
  std::unique_ptr<Spanned<Expr>> rhs;
};

struct Expr {
  std::variant<ExprBinary, ExprUnary, ExprLiteral, ExprIdentifier, ExprTuple, ExprArray, ExprBlock,
               ExprIf, ExprIfLet, ExprMatch, ExprLambda, ExprSpawn, ExprSpawnLambdaActor, ExprScope,
               ExprInterpolatedString, ExprCall, ExprMethodCall, ExprStructInit, ExprSelect,
               ExprJoin, ExprTimeout, ExprUnsafe, ExprYield, ExprCooperate, ExprThis,
               ExprFieldAccess, ExprIndex, ExprCast, ExprPostfixTry, ExprRange, ExprAwait,
               ExprRegexLiteral, ExprArrayRepeat, ExprByteStringLiteral, ExprByteArrayLiteral,
               ExprMapLiteral, ExprIs>
      kind;
  Span span; // Copied from Spanned<Expr> wrapper for codegen convenience
};

// ── Statements ────────────────────────────────────────────────────────────

struct StmtLet {
  Spanned<Pattern> pattern;
  std::optional<Spanned<TypeExpr>> ty;
  std::optional<Spanned<Expr>> value;
};
struct StmtVar {
  std::string name;
  std::optional<Spanned<TypeExpr>> ty;
  std::optional<Spanned<Expr>> value;
};
struct StmtAssign {
  Spanned<Expr> target;
  std::optional<CompoundAssignOp> op;
  Spanned<Expr> value;
};
struct StmtIf {
  Spanned<Expr> condition;
  Block then_block;
  std::optional<ElseBlock> else_block;
};
struct StmtIfLet {
  Spanned<Pattern> pattern;
  std::unique_ptr<Spanned<Expr>> expr;
  Block body;
  std::optional<Block> else_body;
};
struct StmtMatch {
  Spanned<Expr> scrutinee;
  std::vector<MatchArm> arms;
};
struct StmtLoop {
  std::optional<std::string> label;
  Block body;
};
struct StmtFor {
  std::optional<std::string> label;
  bool is_await;
  Spanned<Pattern> pattern;
  Spanned<Expr> iterable;
  Block body;
};
struct StmtWhile {
  std::optional<std::string> label;
  Spanned<Expr> condition;
  Block body;
};
struct StmtWhileLet {
  std::optional<std::string> label;
  Spanned<Pattern> pattern;
  std::unique_ptr<Spanned<Expr>> expr;
  Block body;
};
struct StmtBreak {
  std::optional<std::string> label;
  std::optional<Spanned<Expr>> value;
};
struct StmtContinue {
  std::optional<std::string> label;
};
struct StmtReturn {
  std::optional<Spanned<Expr>> value;
};
struct StmtDefer {
  std::unique_ptr<Spanned<Expr>> expr;
};
struct StmtExpression {
  Spanned<Expr> expr;
};

struct Stmt {
  std::variant<StmtLet, StmtVar, StmtAssign, StmtIf, StmtIfLet, StmtMatch, StmtLoop, StmtFor,
               StmtWhile, StmtWhileLet, StmtBreak, StmtContinue, StmtReturn, StmtDefer,
               StmtExpression>
      kind;
  Span span; // Copied from Spanned<Stmt> wrapper for codegen convenience
};

// ── Parameters ────────────────────────────────────────────────────────────

struct Param {
  std::string name;
  Spanned<TypeExpr> ty;
  bool is_mutable;
};

struct TypeParam {
  std::string name;
  std::vector<TraitBound> bounds;
};

struct WherePredicate {
  Spanned<TypeExpr> ty;
  std::vector<TraitBound> bounds;
};

struct WhereClause {
  std::vector<WherePredicate> predicates;
};

// ── Import ────────────────────────────────────────────────────────────────

struct ImportSpecGlob {};
struct ImportName {
  std::string name;
  std::optional<std::string> alias;
};
struct ImportSpecNames {
  std::vector<ImportName> names;
};
using ImportSpec = std::variant<ImportSpecGlob, ImportSpecNames>;

struct ImportDecl {
  std::vector<std::string> path;
  std::optional<ImportSpec> spec;
  std::optional<std::string> file_path;
};

// ── Const ─────────────────────────────────────────────────────────────────

struct ConstDecl {
  std::string name;
  Spanned<TypeExpr> ty;
  Spanned<Expr> value;
  Visibility visibility = Visibility::Private;
  std::optional<std::string> doc_comment;
};

// ── Type declarations ─────────────────────────────────────────────────────

enum class TypeDeclKind { Struct, Enum };

// NOTE: ResourceMarker and the fields below are present because hew-astgen
// generates a parseResourceMarker helper from the Rust AST enum of the same
// name, and the generated msgpack_reader.cpp references ast::ResourceMarker.
// The C++ reader does NOT populate resource_marker or consuming_methods on
// TypeDecl — those fields are consumed only on the Rust side.  The C++ codegen
// subtree is scheduled for deletion; do not add logic here to consume them.
// (Global CLAUDE.md "Shims, Stubs, and Seams" rule.)
enum class ResourceMarker { None, Resource, Linear };

struct VariantDecl {
  std::string name;
  struct VariantUnit {};
  struct VariantTuple {
    std::vector<Spanned<TypeExpr>> fields;
  };
  struct VariantStructField {
    std::string name;
    Spanned<TypeExpr> ty;
  };
  struct VariantStruct {
    std::vector<VariantStructField> fields;
  };
  std::variant<VariantUnit, VariantTuple, VariantStruct> kind;
};

struct TypeBodyField {
  std::string name;
  Spanned<TypeExpr> ty;
};

// Forward declaration of FnDecl
struct FnDecl;

struct TypeBodyVariant {
  VariantDecl variant;
};
struct TypeBodyMethod {
  FnDecl *fn;
}; // owned via unique_ptr in actual storage

struct TypeBodyItemField {
  std::string name;
  Spanned<TypeExpr> ty;
};

struct TypeBodyItem {
  std::variant<TypeBodyItemField, TypeBodyVariant, TypeBodyMethod> kind;
};

// ── Naming Case ──────────────────────────────────────────────────────────

enum class NamingCase {
  CamelCase,
  PascalCase,
  SnakeCase,
  ScreamingSnake,
  KebabCase,
};

// ── Wire Metadata (on TypeDecl for #[wire] structs) ──────────────────────

struct WireFieldMeta {
  std::string field_name;
  uint32_t field_number = 0;
  bool is_optional = false;
  bool is_deprecated = false;
  bool is_repeated = false;
  std::optional<std::string> json_name;
  std::optional<std::string> yaml_name;
  std::optional<uint32_t> since; // schema version that introduced this field
};

struct WireMetadata {
  std::vector<WireFieldMeta> field_meta;
  std::vector<uint32_t> reserved_numbers;
  std::optional<NamingCase> json_case;
  std::optional<NamingCase> yaml_case;
  std::optional<uint32_t> version;     // from #[wire(version = N)]
  std::optional<uint32_t> min_version; // from #[wire(min_version = N)]
};

struct TypeDecl {
  Visibility visibility = Visibility::Private;
  TypeDeclKind kind;
  std::string name;
  std::optional<std::vector<TypeParam>> type_params;
  std::optional<WhereClause> where_clause;
  std::vector<TypeBodyItem> body;
  std::optional<std::string> doc_comment;
  // Storage for TypeBodyMethod FnDecl pointers
  std::vector<std::unique_ptr<FnDecl>> method_storage;
  std::optional<WireMetadata> wire;
  bool is_indirect = false;
  // NOTE: these fields are intentionally not consumed by the C++ reader.
  // The C++ codegen subtree is scheduled for deletion; resource_marker and
  // consuming_methods are consumed only on the Rust side.  This is a
  // deliberate seam per the global CLAUDE.md "Shims, Stubs, and Seams" rule.
  ResourceMarker resource_marker = ResourceMarker::None;
  std::vector<std::string> consuming_methods;
};

struct TypeAliasDecl {
  Visibility visibility = Visibility::Private;
  std::string name;
  Spanned<TypeExpr> ty;
  std::optional<std::string> doc_comment;
};

// ── Record declarations ───────────────────────────────────────────────────

struct RecordField {
  std::string name;
  Spanned<TypeExpr> ty;
  std::optional<std::string> doc_comment;
};

// Named variant payload for RecordKind.
struct Named {
  std::vector<RecordField> fields;
};

// Tuple variant payload for RecordKind (positional fields).
struct Tuple {
  std::vector<Spanned<TypeExpr>> elements;
};

// RecordKind — Named + Tuple. C++ subtree slated for M6 deletion; this
// is the minimum change to keep the msgpack round-trip green until M6.
struct RecordKind {
  std::variant<Named, Tuple> kind;
};

struct RecordDecl {
  Visibility visibility = Visibility::Private;
  std::string name;
  std::optional<std::vector<TypeParam>> type_params;
  std::optional<WhereClause> where_clause;
  RecordKind kind;
  std::optional<std::string> doc_comment;
  Span span;
};

// ── Traits ────────────────────────────────────────────────────────────────

struct TraitMethod {
  std::string name;
  bool is_pure = false;
  std::optional<std::vector<TypeParam>> type_params;
  std::vector<Param> params;
  std::optional<Spanned<TypeExpr>> return_type;
  std::optional<WhereClause> where_clause;
  std::optional<Block> body;
  std::optional<Span> span;
  std::optional<std::string> doc_comment;
};

struct TraitItemMethod {
  TraitMethod method;
};
struct TraitItemAssociatedType {
  std::string name;
  std::vector<TraitBound> bounds;
  std::optional<Spanned<TypeExpr>> default_value;
};

using TraitItem = std::variant<TraitItemMethod, TraitItemAssociatedType>;

struct TraitDecl {
  Visibility visibility = Visibility::Private;
  std::string name;
  std::optional<std::vector<TypeParam>> type_params;
  std::optional<std::vector<TraitBound>> super_traits;
  std::vector<TraitItem> items;
  std::optional<std::string> doc_comment;
};

// ── Impl ──────────────────────────────────────────────────────────────────

struct ImplTypeAlias {
  std::string name;
  Spanned<TypeExpr> ty;
};

struct ImplDecl {
  std::optional<std::vector<TypeParam>> type_params;
  std::optional<TraitBound> trait_bound;
  Spanned<TypeExpr> target_type;
  std::optional<WhereClause> where_clause;
  std::vector<ImplTypeAlias> type_aliases;
  std::vector<FnDecl> methods;
};

// ── Wire ──────────────────────────────────────────────────────────────────

enum class WireDeclKind { Struct, Enum };

struct WireFieldDecl {
  std::string name;
  std::string ty;
  uint32_t field_number;
  bool is_optional;
  bool is_repeated;
  bool is_reserved;
  bool is_deprecated;
  std::optional<std::string> json_name;
  std::optional<std::string> yaml_name;
  std::optional<uint32_t> since;
};

struct WireDecl {
  Visibility visibility = Visibility::Private;
  WireDeclKind kind;
  std::string name;
  std::vector<WireFieldDecl> fields;
  std::vector<VariantDecl> variants;
  std::optional<NamingCase> json_case;
  std::optional<NamingCase> yaml_case;
  std::optional<uint32_t> version;     // from #[wire(version = N)]
  std::optional<uint32_t> min_version; // from #[wire(min_version = N)]
};

// ── Extern ────────────────────────────────────────────────────────────────

struct ExternFnDecl {
  std::string name;
  std::vector<Param> params;
  std::optional<Spanned<TypeExpr>> return_type;
  bool is_variadic;
};

struct ExternBlock {
  std::string abi;
  std::vector<ExternFnDecl> functions;
};

// ── Actor ─────────────────────────────────────────────────────────────────

struct FieldDecl {
  std::string name;
  Spanned<TypeExpr> ty;
  std::optional<std::string> doc_comment;
};

struct ReceiveFnDecl {
  bool is_generator = false;
  bool is_pure = false;
  std::string name;
  std::optional<std::vector<TypeParam>> type_params;
  std::vector<Param> params;
  std::optional<Spanned<TypeExpr>> return_type;
  std::optional<WhereClause> where_clause;
  Block body;
  Span span;
  /// Periodic interval in nanoseconds from `#[every(duration)]`, or empty.
  std::optional<int64_t> periodic_interval_ns;
};

struct ActorInit {
  std::vector<Param> params;
  Block body;
};

enum class OverflowFallback { DropNew, DropOld, Block, Fail };

struct OverflowCoalesce {
  std::string key_field;
  std::optional<OverflowFallback> fallback;
};

// OverflowPolicy: DropNew | DropOld | Block | Fail | Coalesce{...}
struct OverflowDropNew {};
struct OverflowDropOld {};
struct OverflowBlock {};
struct OverflowFail {};

using OverflowPolicy =
    std::variant<OverflowDropNew, OverflowDropOld, OverflowBlock, OverflowFail, OverflowCoalesce>;

struct ActorDecl {
  Visibility visibility = Visibility::Private;
  std::string name;
  std::optional<std::vector<TraitBound>> super_traits;
  std::optional<ActorInit> init;
  std::vector<FieldDecl> fields;
  std::vector<ReceiveFnDecl> receive_fns;
  std::vector<FnDecl> methods;
  std::optional<uint32_t> mailbox_capacity;
  std::optional<OverflowPolicy> overflow_policy;
  bool is_isolated = false;
  std::optional<std::string> doc_comment;
  // `#[max_heap(N)]` annotation: bytes cap for the actor's arena.
  // None = no annotation (unbounded). Populated by A2 codegen lowering.
  std::optional<uint64_t> max_heap_bytes;
};

// ── Supervisor ────────────────────────────────────────────────────────────

enum class SupervisorStrategy { OneForOne, OneForAll, RestForOne };
enum class RestartPolicy { Permanent, Transient, Temporary };

struct ChildSpec {
  std::string name;
  std::string actor_type;
  std::vector<Spanned<Expr>> args;
  std::optional<RestartPolicy> restart;
};

struct SupervisorDecl {
  Visibility visibility = Visibility::Private;
  std::string name;
  std::optional<SupervisorStrategy> strategy;
  std::optional<int64_t> max_restarts;
  std::optional<std::string> window;
  std::vector<ChildSpec> children;
};

// ── Machine ──────────────────────────────────────────────────────────

struct MachineState {
  std::string name;
  std::vector<std::pair<std::string, Spanned<TypeExpr>>> fields;
  std::optional<Block> entry; // entry hook — present when the state has an `entry { }` block
  std::optional<Block> exit;  // exit hook  — present when the state has an `exit  { }` block
};

struct MachineEvent {
  std::string name;
  std::vector<std::pair<std::string, Spanned<TypeExpr>>> fields;
};

struct MachineTransition {
  std::string event_name;
  std::string source_state;
  std::string target_state;
  std::unique_ptr<Spanned<Expr>> guard; // nullptr if no guard
  Spanned<Expr> body;
  bool reenter = false; // @reenter annotation: Mealy re-entry for self-transitions
};

struct MachineDecl {
  Visibility visibility = Visibility::Private;
  std::string name;
  std::vector<MachineState> states;
  std::vector<MachineEvent> events;
  std::vector<MachineTransition> transitions;
  bool has_default = false;
};

// ── Function declaration ──────────────────────────────────────────────────

struct FnDecl {
  std::vector<Attribute> attributes;
  bool is_async = false;
  bool is_generator = false;
  Visibility visibility = Visibility::Private;
  bool is_pure = false;
  std::string name;
  std::optional<std::vector<TypeParam>> type_params;
  std::vector<Param> params;
  std::optional<Spanned<TypeExpr>> return_type;
  std::optional<WhereClause> where_clause;
  Block body;
  std::optional<std::string> doc_comment;
  /// Byte span of the function-name token from the Rust parser.
  /// Present whenever the AST was produced by a new-enough parser build.
  /// Used by MLIRGen to emit the correct DW_AT_decl_line for impl methods.
  std::optional<Span> decl_span;
  std::optional<Span> fn_span;
};

// ── Items ─────────────────────────────────────────────────────────────────

struct Item {
  std::variant<ImportDecl, ConstDecl, TypeDecl, TypeAliasDecl, TraitDecl, ImplDecl, WireDecl,
               FnDecl, ExternBlock, ActorDecl, SupervisorDecl, MachineDecl, RecordDecl>
      kind;
};

// ── Program ───────────────────────────────────────────────────────────────

/// Unique module identifier based on path segments
struct ModuleId {
  std::vector<std::string> path;

  bool operator==(const ModuleId &other) const { return path == other.path; }
};

/// Hash for ModuleId to use in unordered_map
struct ModuleIdHash {
  std::size_t operator()(const ModuleId &id) const {
    std::size_t h = 0;
    for (const auto &s : id.path) {
      h ^= std::hash<std::string>{}(s) + 0x9e3779b9 + (h << 6) + (h >> 2);
    }
    return h;
  }
};

/// A resolved import within a module
struct ModuleImport {
  ModuleId target;
  std::optional<ImportSpec> spec;
  Span span;
};

/// A single module in the module graph
struct Module {
  ModuleId id;
  std::vector<Spanned<Item>> items;
  std::vector<ModuleImport> imports;
  std::vector<std::string> source_paths;
  std::optional<std::string> doc;
};

/// Complete module graph for a compilation
struct ModuleGraph {
  std::unordered_map<ModuleId, Module, ModuleIdHash> modules;
  ModuleId root;
  std::vector<ModuleId> topo_order;
};

// ── Expression Type Map ───────────────────────────────────────────────────

/// A resolved type for an expression, identified by its source span.
///
/// Populated by the Rust type checker and serialized alongside the AST.
struct ExprTypeEntry {
  uint64_t start = 0;
  uint64_t end = 0;
  Spanned<TypeExpr> ty;
};

struct MethodCallReceiverKindNamedTypeInstance {
  std::string type_name;
};

struct MethodCallReceiverKindHandleInstance {
  std::string type_name;
};

struct MethodCallReceiverKindPrimitiveTraitImpl {
  std::string trait_name;
  std::string canonical_receiver;
};

struct MethodCallReceiverKindTraitObject {
  std::string trait_name;
};

struct MethodCallReceiverKindStreamInstance {
  std::string element_kind;
};

using MethodCallReceiverKindData =
    std::variant<MethodCallReceiverKindNamedTypeInstance, MethodCallReceiverKindHandleInstance,
                 MethodCallReceiverKindTraitObject, MethodCallReceiverKindStreamInstance,
                 MethodCallReceiverKindPrimitiveTraitImpl>;

struct MethodCallReceiverKindEntry {
  uint64_t start = 0;
  uint64_t end = 0;
  MethodCallReceiverKindData kind;
  /// Whether the resolved method declares `consumes_receiver`. When true,
  /// codegen nulls the receiver's drop slot after the call so the
  /// scope-exit drop becomes a null-guarded no-op. Plumbed by issue #1295.
  /// Defaults to false (and remains false for every Hew program until PR 2
  /// introduces `Closable::close`).
  bool consumes_receiver = false;
};

// ── Actor-send aliasing side table ────────────────────────────────────────

/// Codegen choice between aliasing the sender's payload buffer (refcount
/// bump on a `HewMsgEnvelope`) and the legacy deep-copy mailbox path. The
/// type checker records one of these for every accepted actor send; codegen
/// reads the side table fail-closed at every actor-send lowering site.
enum class ActorSendAliasingKind {
  /// Sender retains the payload independently; runtime deep-copies into
  /// the mailbox. The legacy mailbox path; safe everywhere.
  Copy,
  /// Sender and receiver share a refcounted `HewMsgEnvelope` payload.
  /// Requires the move-checker to have invalidated the sender's binding.
  Alias,
};

/// Why the type-checker classified an actor-send arg as `Copy` instead
/// of `Alias`.  Mirrors `hew_types::check::ActorSendCopyReason`.  Only
/// meaningful when `kind == Copy`.
enum class ActorSendCopyReason {
  /// Arg expression was not a bare identifier — move-checker would not
  /// invalidate the parent binding.
  NotIdentifier,
  /// Arg's resolved type implements the `Copy` marker.
  CopyType,
  /// Arg's resolved type implements the stdlib-registered `Drop`
  /// marker.
  StdlibDrop,
  /// Arg's resolved type carries a user `impl Drop for T` (recorded
  /// in `trait_impls_set` rather than the marker).
  UserDrop,
};

struct ActorSendAliasingEntry {
  uint64_t start = 0;
  uint64_t end = 0;
  ActorSendAliasingKind kind = ActorSendAliasingKind::Copy;
  /// Populated when `kind == Copy`; meaningless otherwise.  Defaults to
  /// `CopyType` so a stale build that didn't read the wire field still
  /// produces a sensible fallback for diagnostic rendering.
  ActorSendCopyReason copy_reason = ActorSendCopyReason::CopyType;
};

// ── Assign-target authority side table ────────────────────────────────────

struct AssignTargetKindLocalVar {};
struct AssignTargetKindActorField {};
struct AssignTargetKindFieldAccess {};
struct AssignTargetKindIndex {};

using AssignTargetKindData = std::variant<AssignTargetKindLocalVar, AssignTargetKindActorField,
                                          AssignTargetKindFieldAccess, AssignTargetKindIndex>;

struct AssignTargetKindEntry {
  uint64_t start = 0;
  uint64_t end = 0;
  AssignTargetKindData kind;
};

/// Checker-owned type-shape annotation for an assignment target.
///
/// Carries the signedness flag MLIR lowering needs for compound-assignment
/// arithmetic, eliminating the `resolvedTypeOf` re-derivation path and its
/// silent `isUnsigned = false` fallback when the resolved type is absent.
struct AssignTargetShapeEntry {
  uint64_t start = 0;
  uint64_t end = 0;
  /// `true` when the assignment target has an unsigned integer type
  /// (u8/u16/u32/u64).  `false` for signed integers, floats, and all other
  /// non-numeric types.
  bool is_unsigned = false;
};

enum class LoweringKind {
  HashSet,
};

enum class HashSetElementType {
  I64,
  U64,
  Str,
};

enum class HashSetAbi {
  Int64,
  String,
};

enum class DropKind {
  HashSetFree,
};

struct LoweringFactEntry {
  uint64_t start = 0;
  uint64_t end = 0;
  LoweringKind kind = LoweringKind::HashSet;
  HashSetElementType element_type = HashSetElementType::I64;
  HashSetAbi abi_variant = HashSetAbi::Int64;
  DropKind drop_kind = DropKind::HashSetFree;
};

/// Inferred type arguments for a generic free-function call site, keyed by
/// the call expression's source span.
///
/// Populated from `tco.call_type_args` in the Rust type checker and serialized
/// alongside the AST. C++ codegen uses this side table to look up the resolved
/// type arguments for generic free-function calls without re-running inference.
///
/// The program-level `call_type_args` key is read with `mapReq` (required,
/// fail-closed on missing key). Payload compatibility is enforced by the
/// schema version exact-match check that runs before any field is read, so
/// mismatched payloads are rejected before reaching this reader.
/// Within each entry, the `type_args` array is read with `mapGet` (optional,
/// defaults to empty); the Rust side mirrors this with `#[serde(default)]`
/// on the `type_args` field.
struct CallTypeArgsEntry {
  uint64_t start = 0;
  uint64_t end = 0;
  /// Resolved type arguments in parameter order. Each element is a
  /// `Spanned<TypeExpr>` carrying the concrete type and a synthetic zero span.
  std::vector<Spanned<TypeExpr>> type_args;
};

struct Program {
  /// Schema version of the msgpack AST payload.
  /// The reader requires an explicit exact match and rejects missing or
  /// mismatched versions.
  uint32_t schema_version = 0;
  std::vector<Spanned<Item>> items;
  std::optional<std::string> module_doc;
  /// Resolved expression types from the type checker (indexed by span).
  std::vector<ExprTypeEntry> expr_types;
  /// Checker-resolved receiver classification for surviving method calls.
  std::vector<MethodCallReceiverKindEntry> method_call_receiver_kinds;
  /// Inferred type arguments for generic free-function call sites (keyed by call span).
  /// Populated from schema version 9+; fail-closed for mismatched schema versions.
  ///
  /// WASM-TODO(#1451): call_type_args is carried through the msgpack wire format and deserialized
  /// on all targets. The WASM codegen path does not yet exercise generic free-function lowering,
  /// so no WASM-specific fixture exists for this field; covered by the existing native roundtrip
  /// test in test_msgpack_reader.cpp. Add a WASM fixture when WASM lowering of generic calls
  /// is implemented.
  std::vector<CallTypeArgsEntry> call_type_args;
  /// Checker-resolved alias-vs-copy decision for every accepted actor send.
  /// Keyed by the message expression's span. Codegen reads this fail-closed
  /// at every actor-send lowering site (see `actorSendAliasingOf` in
  /// `MLIRGen.h`); a missing entry for a known send is a hard error.
  std::vector<ActorSendAliasingEntry> actor_send_aliasing;
  /// Checker-resolved assignment target classification (keyed by target span).
  /// Missing entry means checker rejected the target; MLIR lowering must fail closed.
  std::vector<AssignTargetKindEntry> assign_target_kinds;
  /// Checker-resolved assignment target type-shape metadata (keyed by target span).
  /// Consumed fail-closed by MLIR lowering to determine compound-assignment signedness.
  std::vector<AssignTargetShapeEntry> assign_target_shapes;
  /// Checker-owned lowering metadata for erased runtime types.
  std::vector<LoweringFactEntry> lowering_facts;
  /// Known handle type names (e.g., "http.Server", "json.Value").
  /// Populated from the Rust type checker's handle type registry.
  std::vector<std::string> handle_types;
  /// Struct type names whose fields directly or transitively contain owned handles.
  std::vector<std::string> handle_bearing_structs;
  /// Handle type name → MLIR representation ("handle" or "i32").
  /// Types not in this map default to opaque pointer (HandleType).
  std::unordered_map<std::string, std::string> handle_type_repr;
  /// Stdlib handle drop functions from `impl Drop` metadata.
  ///
  /// Maps qualified type name (e.g. `"http.Request"`) to the C function that
  /// frees it (e.g. `"hew_http_request_free"`). Consumed by MLIRGen to drive
  /// handle-drop codegen without hardcoded type→function tables.
  std::unordered_map<std::string, std::string> drop_funcs;
  std::optional<ModuleGraph> module_graph;

  /// Source file path for DWARF debug info (empty if not provided).
  std::string source_path;
  /// Line map: byte offset of the start of each line. Empty if not provided.
  std::vector<size_t> line_map;
};

} // namespace ast
} // namespace hew
