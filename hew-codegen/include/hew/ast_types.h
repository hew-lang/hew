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
    return std::visit(
        [](const auto &v) -> const std::string & { return v.value; }, kind);
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
  Send,
  RegexMatch,
  RegexNotMatch,
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
  std::optional<std::string> binding;
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
};
struct ExprSend {
  std::unique_ptr<Spanned<Expr>> target;
  std::unique_ptr<Spanned<Expr>> message;
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
struct ExprScopeLaunch {
  Block block;
};
struct ExprScopeSpawn {
  Block block;
};
struct ExprScopeCancel {};

struct ExprRegexLiteral {
  std::string pattern;
};
struct ExprArrayRepeat {
  std::unique_ptr<Spanned<Expr>> value;
  std::unique_ptr<Spanned<Expr>> count;
};

struct Expr {
  std::variant<ExprBinary, ExprUnary, ExprLiteral, ExprIdentifier, ExprTuple, ExprArray, ExprBlock,
               ExprIf, ExprIfLet, ExprMatch, ExprLambda, ExprSpawn, ExprSpawnLambdaActor, ExprScope,
               ExprInterpolatedString, ExprCall, ExprMethodCall, ExprStructInit, ExprSend,
               ExprSelect, ExprJoin, ExprTimeout, ExprUnsafe, ExprYield, ExprCooperate,
               ExprFieldAccess, ExprIndex, ExprCast, ExprPostfixTry, ExprRange, ExprAwait,
               ExprScopeLaunch, ExprScopeSpawn, ExprScopeCancel, ExprRegexLiteral, ExprArrayRepeat>
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
               StmtWhile, StmtBreak, StmtContinue, StmtReturn, StmtDefer, StmtExpression>
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
};

// ── Type declarations ─────────────────────────────────────────────────────

enum class TypeDeclKind { Struct, Enum };

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
};

struct WireMetadata {
  std::vector<WireFieldMeta> field_meta;
  std::vector<uint32_t> reserved_numbers;
  std::optional<NamingCase> json_case;
  std::optional<NamingCase> yaml_case;
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
};

struct TypeAliasDecl {
  Visibility visibility = Visibility::Private;
  std::string name;
  Spanned<TypeExpr> ty;
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
};

struct WireDecl {
  Visibility visibility = Visibility::Private;
  WireDeclKind kind;
  std::string name;
  std::vector<WireFieldDecl> fields;
  std::vector<VariantDecl> variants;
  std::optional<NamingCase> json_case;
  std::optional<NamingCase> yaml_case;
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
};

struct ReceiveFnDecl {
  bool is_generator;
  bool is_pure;
  std::string name;
  std::optional<std::vector<TypeParam>> type_params;
  std::vector<Param> params;
  std::optional<Spanned<TypeExpr>> return_type;
  std::optional<WhereClause> where_clause;
  Block body;
  Span span;
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

// ── Function declaration ──────────────────────────────────────────────────

struct FnDecl {
  std::vector<Attribute> attributes;
  bool is_async;
  bool is_generator;
  Visibility visibility = Visibility::Private;
  bool is_pure;
  std::string name;
  std::optional<std::vector<TypeParam>> type_params;
  std::vector<Param> params;
  std::optional<Spanned<TypeExpr>> return_type;
  std::optional<WhereClause> where_clause;
  Block body;
  std::optional<std::string> doc_comment;
};

// ── Items ─────────────────────────────────────────────────────────────────

struct Item {
  std::variant<ImportDecl, ConstDecl, TypeDecl, TypeAliasDecl, TraitDecl, ImplDecl, WireDecl,
               FnDecl, ExternBlock, ActorDecl, SupervisorDecl>
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
  std::optional<std::string> source_path;
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

struct Program {
  std::vector<Spanned<Item>> items;
  std::optional<std::string> module_doc;
  /// Resolved expression types from the type checker (indexed by span).
  std::vector<ExprTypeEntry> expr_types;
  /// Known handle type names (e.g., "http.Server", "json.Value").
  /// Populated from the Rust type checker's handle type registry.
  std::vector<std::string> handle_types;
  /// Handle type name → MLIR representation ("handle" or "i32").
  /// Types not in this map default to opaque pointer (HandleType).
  std::unordered_map<std::string, std::string> handle_type_repr;
  std::optional<ModuleGraph> module_graph;
};

} // namespace ast
} // namespace hew
