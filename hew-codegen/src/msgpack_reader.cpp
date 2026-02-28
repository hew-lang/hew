//===- msgpack_reader.cpp - Deserialize msgpack AST to C++ types ----------===//
//
// Deserializes a Hew AST from msgpack bytes produced by Rust's
// rmp_serde::to_vec_named. The format uses maps with string keys for structs
// and externally-tagged representation for enums.
//
//===----------------------------------------------------------------------===//

#include "hew/msgpack_reader.h"

#include <msgpack.hpp>
#include <nlohmann/json.hpp>

#include <cassert>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

namespace hew {

// ── Error helper ────────────────────────────────────────────────────────────

[[noreturn]] static void fail(const std::string &msg) {
  throw std::runtime_error("msgpack AST parse error: " + msg);
}

// ── msgpack object helpers ──────────────────────────────────────────────────

/// Get a string from a msgpack object.
static std::string getString(const msgpack::object &obj) {
  if (obj.type != msgpack::type::STR)
    fail("expected string, got type " + std::to_string(obj.type));
  return std::string(obj.via.str.ptr, obj.via.str.size);
}

/// Get integer from msgpack object.
static int64_t getInt(const msgpack::object &obj) {
  if (obj.type == msgpack::type::POSITIVE_INTEGER) {
    if (obj.via.u64 > static_cast<uint64_t>(INT64_MAX))
      fail("unsigned value " + std::to_string(obj.via.u64) + " overflows int64_t");
    return static_cast<int64_t>(obj.via.u64);
  }
  if (obj.type == msgpack::type::NEGATIVE_INTEGER)
    return obj.via.i64;
  fail("expected integer, got type " + std::to_string(obj.type));
}

/// Get unsigned integer from msgpack object.
static uint64_t getUint(const msgpack::object &obj) {
  if (obj.type == msgpack::type::POSITIVE_INTEGER)
    return obj.via.u64;
  if (obj.type == msgpack::type::NEGATIVE_INTEGER)
    fail("negative value " + std::to_string(obj.via.i64) + " cannot be converted to uint64_t");
  fail("expected unsigned integer, got type " + std::to_string(obj.type));
}

/// Get float from msgpack object.
static double getFloat(const msgpack::object &obj) {
  if (obj.type == msgpack::type::FLOAT32 || obj.type == msgpack::type::FLOAT64)
    return obj.via.f64;
  if (obj.type == msgpack::type::POSITIVE_INTEGER)
    return static_cast<double>(obj.via.u64);
  if (obj.type == msgpack::type::NEGATIVE_INTEGER)
    return static_cast<double>(obj.via.i64);
  fail("expected float, got type " + std::to_string(obj.type));
}

/// Get bool from msgpack object.
static bool getBool(const msgpack::object &obj) {
  if (obj.type == msgpack::type::BOOLEAN)
    return obj.via.boolean;
  fail("expected bool, got type " + std::to_string(obj.type));
}

/// Check if msgpack object is nil.
static bool isNil(const msgpack::object &obj) {
  return obj.type == msgpack::type::NIL;
}

/// Interpret a msgpack object as a map and find a key.
/// Returns nullptr if not found.
static const msgpack::object *mapGet(const msgpack::object &obj, std::string_view key) {
  if (obj.type != msgpack::type::MAP)
    fail("expected map, got type " + std::to_string(obj.type));
  for (uint32_t i = 0; i < obj.via.map.size; ++i) {
    const auto &kv = obj.via.map.ptr[i];
    if (kv.key.type == msgpack::type::STR &&
        std::string_view(kv.key.via.str.ptr, kv.key.via.str.size) == key)
      return &kv.val;
  }
  return nullptr;
}

/// Interpret a msgpack object as a map and get a required key.
static const msgpack::object &mapReq(const msgpack::object &obj, std::string_view key) {
  const auto *v = mapGet(obj, key);
  if (!v)
    fail("missing required key: " + std::string(key));
  return *v;
}

/// Get an array from a msgpack object.
static const msgpack::object *arrayData(const msgpack::object &obj, uint32_t &size) {
  if (obj.type != msgpack::type::ARRAY)
    fail("expected array, got type " + std::to_string(obj.type));
  size = obj.via.array.size;
  return obj.via.array.ptr;
}

/// Get the variant name from an externally-tagged enum.
/// Returns the variant name and a pointer to the payload.
/// For unit variants (encoded as bare string), payload is nullptr.
static std::pair<std::string, const msgpack::object *> getEnumVariant(const msgpack::object &obj) {
  // Unit variant: encoded as a bare string
  if (obj.type == msgpack::type::STR)
    return {getString(obj), nullptr};
  // Map with single entry: {"VariantName": payload}
  if (obj.type == msgpack::type::MAP && obj.via.map.size == 1) {
    const auto &kv = obj.via.map.ptr[0];
    return {getString(kv.key), &kv.val};
  }
  fail("expected enum variant (string or single-entry map), got type " + std::to_string(obj.type));
}

// ── Forward declarations ────────────────────────────────────────────────────

static ast::Expr parseExpr(const msgpack::object &obj);
static ast::Stmt parseStmt(const msgpack::object &obj);
static ast::Block parseBlock(const msgpack::object &obj);
static ast::TypeExpr parseTypeExpr(const msgpack::object &obj);
static ast::Pattern parsePattern(const msgpack::object &obj);
static ast::FnDecl parseFnDecl(const msgpack::object &obj);

// ── Span / Spanned ──────────────────────────────────────────────────────────

static ast::Span parseSpan(const msgpack::object &obj) {
  // Rust Range<usize> serializes as {"start": N, "end": N}
  return {getUint(mapReq(obj, "start")), getUint(mapReq(obj, "end"))};
}

template <typename T, typename ParseFn>
static ast::Spanned<T> parseSpanned(const msgpack::object &obj, ParseFn parseFn) {
  // Spanned<T> = (T, Span) → [T_value, span_map]
  uint32_t size;
  const auto *arr = arrayData(obj, size);
  if (size != 2)
    fail("Spanned tuple should have 2 elements");
  ast::Spanned<T> result{parseFn(arr[0]), parseSpan(arr[1])};
  // Copy span into inner type for codegen convenience (Expr/Stmt have span field)
  if constexpr (std::is_same_v<T, ast::Expr> || std::is_same_v<T, ast::Stmt>) {
    result.value.span = result.span;
  }
  return result;
}

/// Parse a Spanned<T> and wrap in unique_ptr (for forward-declared T).
template <typename T, typename ParseFn>
static std::unique_ptr<ast::Spanned<T>> parseSpannedPtr(const msgpack::object &obj,
                                                        ParseFn parseFn) {
  return std::make_unique<ast::Spanned<T>>(parseSpanned<T>(obj, parseFn));
}

// ── Optional helpers ────────────────────────────────────────────────────────

template <typename T, typename ParseFn>
static std::optional<T> parseOptional(const msgpack::object &obj, ParseFn parseFn) {
  if (isNil(obj))
    return std::nullopt;
  return parseFn(obj);
}

template <typename T, typename ParseFn>
static std::vector<T> parseVec(const msgpack::object &obj, ParseFn parseFn) {
  uint32_t size;
  const auto *arr = arrayData(obj, size);
  std::vector<T> result;
  result.reserve(size);
  for (uint32_t i = 0; i < size; ++i)
    result.push_back(parseFn(arr[i]));
  return result;
}

template <typename T, typename ParseFn>
static std::vector<T> parseOptVec(const msgpack::object &obj, ParseFn parseFn) {
  if (isNil(obj))
    return {};
  return parseVec<T>(obj, parseFn);
}

/// Parse a vector of unique_ptr<T> from a msgpack array.
template <typename T, typename ParseFn>
static std::vector<std::unique_ptr<T>> parseVecPtr(const msgpack::object &obj, ParseFn parseFn) {
  uint32_t size;
  const auto *arr = arrayData(obj, size);
  std::vector<std::unique_ptr<T>> result;
  result.reserve(size);
  for (uint32_t i = 0; i < size; ++i)
    result.push_back(std::make_unique<T>(parseFn(arr[i])));
  return result;
}

// ── Literals ────────────────────────────────────────────────────────────────

static ast::Literal parseLiteral(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);
  if (name == "Integer")
    return ast::LitInteger{getInt(*payload)};
  if (name == "Float")
    return ast::LitFloat{getFloat(*payload)};
  if (name == "String")
    return ast::LitString{getString(*payload)};
  if (name == "Bool")
    return ast::LitBool{getBool(*payload)};
  if (name == "Char") {
    // Rust char serializes as a string
    auto s = getString(*payload);
    return ast::LitChar{s.empty() ? '\0' : s[0]};
  }
  if (name == "Duration")
    return ast::LitDuration{getInt(*payload)};
  fail("unknown Literal variant: " + name);
}

// ── Operators ───────────────────────────────────────────────────────────────

static ast::BinaryOp parseBinaryOp(const msgpack::object &obj) {
  auto s = getString(obj);
  if (s == "Add")
    return ast::BinaryOp::Add;
  if (s == "Subtract")
    return ast::BinaryOp::Subtract;
  if (s == "Multiply")
    return ast::BinaryOp::Multiply;
  if (s == "Divide")
    return ast::BinaryOp::Divide;
  if (s == "Modulo")
    return ast::BinaryOp::Modulo;
  if (s == "Equal")
    return ast::BinaryOp::Equal;
  if (s == "NotEqual")
    return ast::BinaryOp::NotEqual;
  if (s == "Less")
    return ast::BinaryOp::Less;
  if (s == "LessEqual")
    return ast::BinaryOp::LessEqual;
  if (s == "Greater")
    return ast::BinaryOp::Greater;
  if (s == "GreaterEqual")
    return ast::BinaryOp::GreaterEqual;
  if (s == "And")
    return ast::BinaryOp::And;
  if (s == "Or")
    return ast::BinaryOp::Or;
  if (s == "BitAnd")
    return ast::BinaryOp::BitAnd;
  if (s == "BitOr")
    return ast::BinaryOp::BitOr;
  if (s == "BitXor")
    return ast::BinaryOp::BitXor;
  if (s == "Shl")
    return ast::BinaryOp::Shl;
  if (s == "Shr")
    return ast::BinaryOp::Shr;
  if (s == "Range")
    return ast::BinaryOp::Range;
  if (s == "RangeInclusive")
    return ast::BinaryOp::RangeInclusive;
  if (s == "Send")
    return ast::BinaryOp::Send;
  if (s == "RegexMatch")
    return ast::BinaryOp::RegexMatch;
  if (s == "RegexNotMatch")
    return ast::BinaryOp::RegexNotMatch;
  fail("unknown BinaryOp: " + s);
}

static ast::UnaryOp parseUnaryOp(const msgpack::object &obj) {
  auto s = getString(obj);
  if (s == "Not")
    return ast::UnaryOp::Not;
  if (s == "Negate")
    return ast::UnaryOp::Negate;
  if (s == "BitNot")
    return ast::UnaryOp::BitNot;
  fail("unknown UnaryOp: " + s);
}

static ast::CompoundAssignOp parseCompoundAssignOp(const msgpack::object &obj) {
  auto s = getString(obj);
  if (s == "Add")
    return ast::CompoundAssignOp::Add;
  if (s == "Subtract")
    return ast::CompoundAssignOp::Subtract;
  if (s == "Multiply")
    return ast::CompoundAssignOp::Multiply;
  if (s == "Divide")
    return ast::CompoundAssignOp::Divide;
  if (s == "Modulo")
    return ast::CompoundAssignOp::Modulo;
  if (s == "BitAnd")
    return ast::CompoundAssignOp::BitAnd;
  if (s == "BitOr")
    return ast::CompoundAssignOp::BitOr;
  if (s == "BitXor")
    return ast::CompoundAssignOp::BitXor;
  if (s == "Shl")
    return ast::CompoundAssignOp::Shl;
  if (s == "Shr")
    return ast::CompoundAssignOp::Shr;
  fail("unknown CompoundAssignOp: " + s);
}

// ── TraitBound ──────────────────────────────────────────────────────────────

static ast::TraitBound parseTraitBound(const msgpack::object &obj) {
  ast::TraitBound tb;
  tb.name = getString(mapReq(obj, "name"));
  const auto *ta = mapGet(obj, "type_args");
  if (ta && !isNil(*ta)) {
    tb.type_args = parseVec<ast::Spanned<ast::TypeExpr>>(*ta, [](const msgpack::object &o) {
      return parseSpanned<ast::TypeExpr>(o, parseTypeExpr);
    });
  }
  return tb;
}

// ── TypeExpr ────────────────────────────────────────────────────────────────

static ast::TypeExpr parseTypeExpr(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);

  if (name == "Named") {
    ast::TypeNamed tn;
    tn.name = getString(mapReq(*payload, "name"));
    const auto *ta = mapGet(*payload, "type_args");
    if (ta && !isNil(*ta)) {
      tn.type_args = parseVec<ast::Spanned<ast::TypeExpr>>(*ta, [](const msgpack::object &o) {
        return parseSpanned<ast::TypeExpr>(o, parseTypeExpr);
      });
    }
    return ast::TypeExpr{std::move(tn)};
  }
  if (name == "Result") {
    ast::TypeResult tr;
    tr.ok = std::make_unique<ast::Spanned<ast::TypeExpr>>(
        parseSpanned<ast::TypeExpr>(mapReq(*payload, "ok"), parseTypeExpr));
    tr.err = std::make_unique<ast::Spanned<ast::TypeExpr>>(
        parseSpanned<ast::TypeExpr>(mapReq(*payload, "err"), parseTypeExpr));
    return ast::TypeExpr{std::move(tr)};
  }
  if (name == "Option") {
    // Serde: {"Option": inner_spanned_type}  (newtype variant)
    ast::TypeOption to;
    to.inner = std::make_unique<ast::Spanned<ast::TypeExpr>>(
        parseSpanned<ast::TypeExpr>(*payload, parseTypeExpr));
    return ast::TypeExpr{std::move(to)};
  }
  if (name == "Tuple") {
    // Serde: {"Tuple": [spanned_types...]}  (newtype variant wrapping Vec)
    ast::TypeTuple tt;
    tt.elements = parseVec<ast::Spanned<ast::TypeExpr>>(*payload, [](const msgpack::object &o) {
      return parseSpanned<ast::TypeExpr>(o, parseTypeExpr);
    });
    return ast::TypeExpr{std::move(tt)};
  }
  if (name == "Array") {
    ast::TypeArray ta;
    ta.element = std::make_unique<ast::Spanned<ast::TypeExpr>>(
        parseSpanned<ast::TypeExpr>(mapReq(*payload, "element"), parseTypeExpr));
    ta.size = getUint(mapReq(*payload, "size"));
    return ast::TypeExpr{std::move(ta)};
  }
  if (name == "Slice") {
    ast::TypeSlice ts;
    ts.inner = std::make_unique<ast::Spanned<ast::TypeExpr>>(
        parseSpanned<ast::TypeExpr>(*payload, parseTypeExpr));
    return ast::TypeExpr{std::move(ts)};
  }
  if (name == "Function") {
    ast::TypeFunction tf;
    tf.params = parseVec<ast::Spanned<ast::TypeExpr>>(
        mapReq(*payload, "params"),
        [](const msgpack::object &o) { return parseSpanned<ast::TypeExpr>(o, parseTypeExpr); });
    tf.return_type = std::make_unique<ast::Spanned<ast::TypeExpr>>(
        parseSpanned<ast::TypeExpr>(mapReq(*payload, "return_type"), parseTypeExpr));
    return ast::TypeExpr{std::move(tf)};
  }
  if (name == "Pointer") {
    ast::TypePointer tp;
    tp.is_mutable = getBool(mapReq(*payload, "is_mutable"));
    tp.pointee = std::make_unique<ast::Spanned<ast::TypeExpr>>(
        parseSpanned<ast::TypeExpr>(mapReq(*payload, "pointee"), parseTypeExpr));
    return ast::TypeExpr{std::move(tp)};
  }
  if (name == "TraitObject") {
    // Serde: {"TraitObject": trait_bound_map}  (newtype wrapping TraitBound)
    ast::TypeTraitObject tto;
    tto.bound = std::make_unique<ast::TraitBound>(parseTraitBound(*payload));
    return ast::TypeExpr{std::move(tto)};
  }
  fail("unknown TypeExpr variant: " + name);
}

// ── Pattern ─────────────────────────────────────────────────────────────────

static ast::PatternField parsePatternField(const msgpack::object &obj) {
  ast::PatternField pf;
  pf.name = getString(mapReq(obj, "name"));
  const auto *pat = mapGet(obj, "pattern");
  if (pat && !isNil(*pat)) {
    pf.pattern = parseSpannedPtr<ast::Pattern>(*pat, parsePattern);
  }
  return pf;
}

static ast::Pattern parsePattern(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);

  if (name == "Wildcard")
    return ast::Pattern{ast::PatWildcard{}};
  if (name == "Literal")
    return ast::Pattern{ast::PatLiteral{parseLiteral(*payload)}};
  if (name == "Identifier")
    return ast::Pattern{ast::PatIdentifier{getString(*payload)}};
  if (name == "Constructor") {
    ast::PatConstructor pc;
    pc.name = getString(mapReq(*payload, "name"));
    pc.patterns = parseVecPtr<ast::Spanned<ast::Pattern>>(
        mapReq(*payload, "patterns"),
        [](const msgpack::object &o) { return parseSpanned<ast::Pattern>(o, parsePattern); });
    return ast::Pattern{std::move(pc)};
  }
  if (name == "Struct") {
    ast::PatStruct ps;
    ps.name = getString(mapReq(*payload, "name"));
    ps.fields = parseVec<ast::PatternField>(mapReq(*payload, "fields"), parsePatternField);
    return ast::Pattern{std::move(ps)};
  }
  if (name == "Tuple") {
    ast::PatTuple pt;
    pt.elements = parseVecPtr<ast::Spanned<ast::Pattern>>(*payload, [](const msgpack::object &o) {
      return parseSpanned<ast::Pattern>(o, parsePattern);
    });
    return ast::Pattern{std::move(pt)};
  }
  if (name == "Or") {
    // Serde: {"Or": [left, right]} — tuple variant with 2 elements
    uint32_t sz;
    const auto *arr = arrayData(*payload, sz);
    if (sz != 2)
      fail("Or pattern expects 2 elements");
    ast::PatOr po;
    po.left = parseSpannedPtr<ast::Pattern>(arr[0], parsePattern);
    po.right = parseSpannedPtr<ast::Pattern>(arr[1], parsePattern);
    return ast::Pattern{std::move(po)};
  }
  fail("unknown Pattern variant: " + name);
}

// ── StringPart ──────────────────────────────────────────────────────────────

static ast::StringPart parseStringPart(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);
  if (name == "Literal")
    return ast::StringPartLiteral{getString(*payload)};
  if (name == "Expr")
    return ast::StringPartExpr{parseSpannedPtr<ast::Expr>(*payload, parseExpr)};
  fail("unknown StringPart variant: " + name);
}

// ── MatchArm / SelectArm / TimeoutClause ────────────────────────────────────

static ast::MatchArm parseMatchArm(const msgpack::object &obj) {
  ast::MatchArm arm;
  arm.pattern = parseSpanned<ast::Pattern>(mapReq(obj, "pattern"), parsePattern);
  const auto *g = mapGet(obj, "guard");
  if (g && !isNil(*g))
    arm.guard = parseSpannedPtr<ast::Expr>(*g, parseExpr);
  arm.body = parseSpannedPtr<ast::Expr>(mapReq(obj, "body"), parseExpr);
  return arm;
}

static ast::SelectArm parseSelectArm(const msgpack::object &obj) {
  ast::SelectArm arm;
  arm.binding = parseSpanned<ast::Pattern>(mapReq(obj, "binding"), parsePattern);
  arm.source = parseSpannedPtr<ast::Expr>(mapReq(obj, "source"), parseExpr);
  arm.body = parseSpannedPtr<ast::Expr>(mapReq(obj, "body"), parseExpr);
  return arm;
}

static ast::TimeoutClause parseTimeoutClause(const msgpack::object &obj) {
  ast::TimeoutClause tc;
  tc.duration = std::make_unique<ast::Spanned<ast::Expr>>(
      parseSpanned<ast::Expr>(mapReq(obj, "duration"), parseExpr));
  tc.body = std::make_unique<ast::Spanned<ast::Expr>>(
      parseSpanned<ast::Expr>(mapReq(obj, "body"), parseExpr));
  return tc;
}

// ── LambdaParam ─────────────────────────────────────────────────────────────

static ast::LambdaParam parseLambdaParam(const msgpack::object &obj) {
  ast::LambdaParam lp;
  lp.name = getString(mapReq(obj, "name"));
  const auto *ty = mapGet(obj, "ty");
  if (ty && !isNil(*ty))
    lp.ty = parseSpanned<ast::TypeExpr>(*ty, parseTypeExpr);
  return lp;
}

// ── Block ───────────────────────────────────────────────────────────────────

static ast::Block parseBlock(const msgpack::object &obj) {
  ast::Block block;
  block.stmts =
      parseVecPtr<ast::Spanned<ast::Stmt>>(mapReq(obj, "stmts"), [](const msgpack::object &o) {
        return parseSpanned<ast::Stmt>(o, parseStmt);
      });
  const auto *te = mapGet(obj, "trailing_expr");
  if (te && !isNil(*te)) {
    block.trailing_expr = parseSpannedPtr<ast::Expr>(*te, parseExpr);
  }
  return block;
}

// ── ElseBlock ───────────────────────────────────────────────────────────────

static ast::ElseBlock parseElseBlock(const msgpack::object &obj) {
  ast::ElseBlock eb;
  eb.is_if = getBool(mapReq(obj, "is_if"));
  const auto *ifs = mapGet(obj, "if_stmt");
  if (ifs && !isNil(*ifs)) {
    eb.if_stmt = parseSpannedPtr<ast::Stmt>(*ifs, parseStmt);
  }
  const auto *blk = mapGet(obj, "block");
  if (blk && !isNil(*blk))
    eb.block = parseBlock(*blk);
  return eb;
}

// ── CallArg ─────────────────────────────────────────────────────────────────

static ast::CallArg parseCallArg(const msgpack::object &obj) {
  // Serde serializes Rust enums as {"VariantName": payload}
  if (obj.type != msgpack::type::MAP)
    fail("CallArg: expected map, got type " + std::to_string(obj.type));

  auto &map = obj.via.map;
  for (uint32_t i = 0; i < map.size; i++) {
    auto key = getString(map.ptr[i].key);
    if (key == "Positional") {
      ast::CallArgPositional arg;
      arg.expr = std::make_unique<ast::Spanned<ast::Expr>>(
          parseSpanned<ast::Expr>(map.ptr[i].val, parseExpr));
      return arg;
    }
    if (key == "Named") {
      auto &payload = map.ptr[i].val;
      ast::CallArgNamed arg;
      arg.name = getString(mapReq(payload, "name"));
      arg.value = std::make_unique<ast::Spanned<ast::Expr>>(
          parseSpanned<ast::Expr>(mapReq(payload, "value"), parseExpr));
      return arg;
    }
  }
  fail("CallArg: unknown variant");
}

// ── Expressions ─────────────────────────────────────────────────────────────

static ast::Expr parseExpr(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);

  if (name == "Binary") {
    ast::ExprBinary e;
    e.left = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "left"), parseExpr));
    e.op = parseBinaryOp(mapReq(*payload, "op"));
    e.right = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "right"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Unary") {
    ast::ExprUnary e;
    e.op = parseUnaryOp(mapReq(*payload, "op"));
    e.operand = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "operand"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Literal")
    return ast::Expr{ast::ExprLiteral{parseLiteral(*payload)}, {}};
  if (name == "Identifier")
    return ast::Expr{ast::ExprIdentifier{getString(*payload)}, {}};
  if (name == "Tuple") {
    ast::ExprTuple e;
    e.elements = parseVecPtr<ast::Spanned<ast::Expr>>(
        *payload, [](const msgpack::object &o) { return parseSpanned<ast::Expr>(o, parseExpr); });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Array") {
    ast::ExprArray e;
    e.elements = parseVecPtr<ast::Spanned<ast::Expr>>(
        *payload, [](const msgpack::object &o) { return parseSpanned<ast::Expr>(o, parseExpr); });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Block")
    return ast::Expr{ast::ExprBlock{parseBlock(*payload)}, {}};
  if (name == "If") {
    ast::ExprIf e;
    e.condition = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "condition"), parseExpr));
    e.then_block = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "then_block"), parseExpr));
    const auto *eb = mapGet(*payload, "else_block");
    if (eb && !isNil(*eb)) {
      e.else_block =
          std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*eb, parseExpr));
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Match") {
    ast::ExprMatch e;
    e.scrutinee = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "scrutinee"), parseExpr));
    e.arms = parseVec<ast::MatchArm>(mapReq(*payload, "arms"), parseMatchArm);
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Lambda") {
    ast::ExprLambda e;
    e.is_move = getBool(mapReq(*payload, "is_move"));
    e.params = parseVec<ast::LambdaParam>(mapReq(*payload, "params"), parseLambdaParam);
    const auto *rt = mapGet(*payload, "return_type");
    if (rt && !isNil(*rt))
      e.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
    e.body = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "body"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Spawn") {
    ast::ExprSpawn e;
    e.target = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "target"), parseExpr));
    e.args = parseVec<std::pair<std::string, std::unique_ptr<ast::Spanned<ast::Expr>>>>(
        mapReq(*payload, "args"), [](const msgpack::object &o) {
          // (String, Spanned<Expr>) = [name_str, spanned_expr]
          uint32_t sz;
          const auto *arr = arrayData(o, sz);
          if (sz != 2)
            fail("Spawn arg tuple should have 2 elements");
          return std::make_pair(getString(arr[0]), parseSpannedPtr<ast::Expr>(arr[1], parseExpr));
        });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "SpawnLambdaActor") {
    ast::ExprSpawnLambdaActor e;
    e.is_move = getBool(mapReq(*payload, "is_move"));
    e.params = parseVec<ast::LambdaParam>(mapReq(*payload, "params"), parseLambdaParam);
    const auto *rt = mapGet(*payload, "return_type");
    if (rt && !isNil(*rt))
      e.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
    e.body = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "body"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Scope") {
    ast::ExprScope e;
    const auto *bind = mapGet(*payload, "binding");
    if (bind && !isNil(*bind))
      e.binding = getString(*bind);
    e.block = parseBlock(mapReq(*payload, "body"));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "InterpolatedString") {
    ast::ExprInterpolatedString e;
    e.parts = parseVec<ast::StringPart>(*payload, parseStringPart);
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Call") {
    ast::ExprCall e;
    e.function = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "function"), parseExpr));
    const auto *ta = mapGet(*payload, "type_args");
    if (ta && !isNil(*ta)) {
      e.type_args = parseVec<ast::Spanned<ast::TypeExpr>>(*ta, [](const msgpack::object &o) {
        return parseSpanned<ast::TypeExpr>(o, parseTypeExpr);
      });
    }
    {
      auto &argsArr = mapReq(*payload, "args");
      if (argsArr.type == msgpack::type::ARRAY) {
        auto &arrData = argsArr.via.array;
        for (uint32_t j = 0; j < arrData.size; j++) {
          e.args.push_back(parseCallArg(arrData.ptr[j]));
        }
      }
    }
    e.is_tail_call = getBool(mapReq(*payload, "is_tail_call"));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "MethodCall") {
    ast::ExprMethodCall e;
    e.receiver = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "receiver"), parseExpr));
    e.method = getString(mapReq(*payload, "method"));
    {
      auto &argsArr = mapReq(*payload, "args");
      if (argsArr.type == msgpack::type::ARRAY) {
        auto &arrData = argsArr.via.array;
        for (uint32_t j = 0; j < arrData.size; j++) {
          e.args.push_back(parseCallArg(arrData.ptr[j]));
        }
      }
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "StructInit") {
    ast::ExprStructInit e;
    e.name = getString(mapReq(*payload, "name"));
    e.fields = parseVec<std::pair<std::string, std::unique_ptr<ast::Spanned<ast::Expr>>>>(
        mapReq(*payload, "fields"), [](const msgpack::object &o) {
          // (String, Spanned<Expr>) = [name_str, spanned_expr]
          uint32_t sz;
          const auto *arr = arrayData(o, sz);
          if (sz != 2)
            fail("StructInit field tuple should have 2 elements");
          return std::make_pair(getString(arr[0]), parseSpannedPtr<ast::Expr>(arr[1], parseExpr));
        });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Send") {
    ast::ExprSend e;
    e.target = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "target"), parseExpr));
    e.message = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "message"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Select") {
    ast::ExprSelect e;
    e.arms = parseVec<ast::SelectArm>(mapReq(*payload, "arms"), parseSelectArm);
    const auto *to = mapGet(*payload, "timeout");
    if (to && !isNil(*to)) {
      e.timeout = std::make_unique<ast::TimeoutClause>(parseTimeoutClause(*to));
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Join") {
    ast::ExprJoin e;
    e.exprs = parseVecPtr<ast::Spanned<ast::Expr>>(
        *payload, [](const msgpack::object &o) { return parseSpanned<ast::Expr>(o, parseExpr); });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Timeout") {
    ast::ExprTimeout e;
    e.expr = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "expr"), parseExpr));
    e.duration = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "duration"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Unsafe")
    return ast::Expr{ast::ExprUnsafe{parseBlock(*payload)}, {}};
  if (name == "Yield") {
    ast::ExprYield e;
    if (payload && !isNil(*payload)) {
      e.value =
          std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*payload, parseExpr));
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Cooperate")
    return ast::Expr{ast::ExprCooperate{}, {}};
  if (name == "FieldAccess") {
    ast::ExprFieldAccess e;
    e.object = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "object"), parseExpr));
    e.field = getString(mapReq(*payload, "field"));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Index") {
    ast::ExprIndex e;
    e.object = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "object"), parseExpr));
    e.index = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "index"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "PostfixTry") {
    ast::ExprPostfixTry e;
    e.inner =
        std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*payload, parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Range") {
    ast::ExprRange e;
    const auto *s = mapGet(*payload, "start");
    if (s && !isNil(*s)) {
      e.start = std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*s, parseExpr));
    }
    const auto *en = mapGet(*payload, "end");
    if (en && !isNil(*en)) {
      e.end = std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*en, parseExpr));
    }
    e.inclusive = getBool(mapReq(*payload, "inclusive"));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Await") {
    ast::ExprAwait e;
    e.inner =
        std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*payload, parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "ScopeLaunch")
    return ast::Expr{ast::ExprScopeLaunch{parseBlock(*payload)}, {}};
  if (name == "ScopeCancel")
    return ast::Expr{ast::ExprScopeCancel{}, {}};

  if (name == "RegexLiteral")
    return ast::Expr{ast::ExprRegexLiteral{getString(*payload)}, {}};
  fail("unknown Expr variant: " + name);
}

// ── Statements ──────────────────────────────────────────────────────────────

static ast::Stmt parseStmt(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);

  if (name == "Let") {
    ast::StmtLet s;
    s.pattern = parseSpanned<ast::Pattern>(mapReq(*payload, "pattern"), parsePattern);
    const auto *ty = mapGet(*payload, "ty");
    if (ty && !isNil(*ty))
      s.ty = parseSpanned<ast::TypeExpr>(*ty, parseTypeExpr);
    const auto *val = mapGet(*payload, "value");
    if (val && !isNil(*val))
      s.value = parseSpanned<ast::Expr>(*val, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Var") {
    ast::StmtVar s;
    s.name = getString(mapReq(*payload, "name"));
    const auto *ty = mapGet(*payload, "ty");
    if (ty && !isNil(*ty))
      s.ty = parseSpanned<ast::TypeExpr>(*ty, parseTypeExpr);
    const auto *val = mapGet(*payload, "value");
    if (val && !isNil(*val))
      s.value = parseSpanned<ast::Expr>(*val, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Assign") {
    ast::StmtAssign s;
    s.target = parseSpanned<ast::Expr>(mapReq(*payload, "target"), parseExpr);
    const auto *op = mapGet(*payload, "op");
    if (op && !isNil(*op))
      s.op = parseCompoundAssignOp(*op);
    s.value = parseSpanned<ast::Expr>(mapReq(*payload, "value"), parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "If") {
    ast::StmtIf s;
    s.condition = parseSpanned<ast::Expr>(mapReq(*payload, "condition"), parseExpr);
    s.then_block = parseBlock(mapReq(*payload, "then_block"));
    const auto *eb = mapGet(*payload, "else_block");
    if (eb && !isNil(*eb))
      s.else_block = parseElseBlock(*eb);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Match") {
    ast::StmtMatch s;
    s.scrutinee = parseSpanned<ast::Expr>(mapReq(*payload, "scrutinee"), parseExpr);
    s.arms = parseVec<ast::MatchArm>(mapReq(*payload, "arms"), parseMatchArm);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Loop") {
    ast::StmtLoop s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    s.body = parseBlock(mapReq(*payload, "body"));
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "For") {
    ast::StmtFor s;
    s.is_await = getBool(mapReq(*payload, "is_await"));
    s.pattern = parseSpanned<ast::Pattern>(mapReq(*payload, "pattern"), parsePattern);
    s.iterable = parseSpanned<ast::Expr>(mapReq(*payload, "iterable"), parseExpr);
    s.body = parseBlock(mapReq(*payload, "body"));
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "While") {
    ast::StmtWhile s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    s.condition = parseSpanned<ast::Expr>(mapReq(*payload, "condition"), parseExpr);
    s.body = parseBlock(mapReq(*payload, "body"));
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Break") {
    ast::StmtBreak s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    const auto *val = mapGet(*payload, "value");
    if (val && !isNil(*val))
      s.value = parseSpanned<ast::Expr>(*val, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Continue") {
    ast::StmtContinue s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Return") {
    ast::StmtReturn s;
    if (payload && !isNil(*payload))
      s.value = parseSpanned<ast::Expr>(*payload, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Defer") {
    ast::StmtDefer s;
    s.expr =
        std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*payload, parseExpr));
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Expression") {
    ast::StmtExpression s;
    s.expr = parseSpanned<ast::Expr>(*payload, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  fail("unknown Stmt variant: " + name);
}

// ── Param / TypeParam / WhereClause ─────────────────────────────────────────

static ast::Param parseParam(const msgpack::object &obj) {
  ast::Param p;
  p.name = getString(mapReq(obj, "name"));
  p.ty = parseSpanned<ast::TypeExpr>(mapReq(obj, "ty"), parseTypeExpr);
  p.is_mutable = getBool(mapReq(obj, "is_mutable"));
  return p;
}

static ast::TypeParam parseTypeParam(const msgpack::object &obj) {
  ast::TypeParam tp;
  tp.name = getString(mapReq(obj, "name"));
  tp.bounds = parseVec<ast::TraitBound>(mapReq(obj, "bounds"), parseTraitBound);
  return tp;
}

static ast::WherePredicate parseWherePredicate(const msgpack::object &obj) {
  ast::WherePredicate wp;
  wp.ty = parseSpanned<ast::TypeExpr>(mapReq(obj, "ty"), parseTypeExpr);
  wp.bounds = parseVec<ast::TraitBound>(mapReq(obj, "bounds"), parseTraitBound);
  return wp;
}

static ast::WhereClause parseWhereClause(const msgpack::object &obj) {
  ast::WhereClause wc;
  wc.predicates = parseVec<ast::WherePredicate>(mapReq(obj, "predicates"), parseWherePredicate);
  return wc;
}

// ── Attribute ───────────────────────────────────────────────────────────────

static ast::Attribute parseAttribute(const msgpack::object &obj) {
  ast::Attribute attr;
  attr.name = getString(mapReq(obj, "name"));
  attr.args = parseVec<std::string>(mapReq(obj, "args"),
                                    [](const msgpack::object &o) { return getString(o); });
  attr.span = parseSpan(mapReq(obj, "span"));
  return attr;
}

// ── FnDecl ──────────────────────────────────────────────────────────────────

static ast::FnDecl parseFnDecl(const msgpack::object &obj) {
  ast::FnDecl fn;
  fn.attributes = parseVec<ast::Attribute>(mapReq(obj, "attributes"), parseAttribute);
  fn.is_async = getBool(mapReq(obj, "is_async"));
  fn.is_generator = getBool(mapReq(obj, "is_generator"));
  fn.is_pub = getBool(mapReq(obj, "is_pub"));
  fn.is_pure = getBool(mapReq(obj, "is_pure"));
  fn.name = getString(mapReq(obj, "name"));
  const auto *tp = mapGet(obj, "type_params");
  if (tp && !isNil(*tp))
    fn.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
  fn.params = parseVec<ast::Param>(mapReq(obj, "params"), parseParam);
  const auto *rt = mapGet(obj, "return_type");
  if (rt && !isNil(*rt))
    fn.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
  const auto *wc = mapGet(obj, "where_clause");
  if (wc && !isNil(*wc))
    fn.where_clause = parseWhereClause(*wc);
  fn.body = parseBlock(mapReq(obj, "body"));
  const auto *dc = mapGet(obj, "doc_comment");
  if (dc && !isNil(*dc))
    fn.doc_comment = getString(*dc);
  return fn;
}

// ── ImportDecl ──────────────────────────────────────────────────────────────

static ast::ImportDecl parseImportDecl(const msgpack::object &obj) {
  ast::ImportDecl id;
  id.path = parseVec<std::string>(mapReq(obj, "path"),
                                  [](const msgpack::object &o) { return getString(o); });
  const auto *spec = mapGet(obj, "spec");
  if (spec && !isNil(*spec)) {
    auto [name, payload] = getEnumVariant(*spec);
    if (name == "Glob") {
      id.spec = ast::ImportSpecGlob{};
    } else if (name == "Names") {
      id.spec =
          ast::ImportSpecNames{parseVec<ast::ImportName>(*payload, [](const msgpack::object &o) {
            ast::ImportName n;
            n.name = getString(mapReq(o, "name"));
            const auto *alias = mapGet(o, "alias");
            if (alias && !isNil(*alias))
              n.alias = getString(*alias);
            return n;
          })};
    }
  }
  const auto *fp = mapGet(obj, "file_path");
  if (fp && !isNil(*fp))
    id.file_path = getString(*fp);
  return id;
}

// ── ConstDecl ───────────────────────────────────────────────────────────────

static ast::ConstDecl parseConstDecl(const msgpack::object &obj) {
  ast::ConstDecl cd;
  cd.name = getString(mapReq(obj, "name"));
  cd.ty = parseSpanned<ast::TypeExpr>(mapReq(obj, "ty"), parseTypeExpr);
  cd.value = parseSpanned<ast::Expr>(mapReq(obj, "value"), parseExpr);
  const auto *isPub = mapGet(obj, "is_pub");
  if (isPub && !isNil(*isPub))
    cd.is_pub = getBool(*isPub);
  return cd;
}

// ── VariantDecl ─────────────────────────────────────────────────────────────

static ast::VariantDecl parseVariantDecl(const msgpack::object &obj) {
  ast::VariantDecl vd;
  vd.name = getString(mapReq(obj, "name"));
  vd.fields =
      parseVec<ast::Spanned<ast::TypeExpr>>(mapReq(obj, "fields"), [](const msgpack::object &o) {
        return parseSpanned<ast::TypeExpr>(o, parseTypeExpr);
      });
  return vd;
}

// ── TypeDecl ────────────────────────────────────────────────────────────────

static ast::TypeDecl parseTypeDecl(const msgpack::object &obj) {
  ast::TypeDecl td;
  const auto *isPub = mapGet(obj, "is_pub");
  if (isPub && !isNil(*isPub))
    td.is_pub = getBool(*isPub);
  auto kindStr = getString(mapReq(obj, "kind"));
  td.kind = (kindStr == "Enum") ? ast::TypeDeclKind::Enum : ast::TypeDeclKind::Struct;
  td.name = getString(mapReq(obj, "name"));
  const auto *tp = mapGet(obj, "type_params");
  if (tp && !isNil(*tp))
    td.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
  const auto *wc = mapGet(obj, "where_clause");
  if (wc && !isNil(*wc))
    td.where_clause = parseWhereClause(*wc);
  td.body = parseVec<ast::TypeBodyItem>(mapReq(obj, "body"), [&td](const msgpack::object &o) {
    auto [name, payload] = getEnumVariant(o);
    if (name == "Field") {
      ast::TypeBodyItemField f;
      f.name = getString(mapReq(*payload, "name"));
      f.ty = parseSpanned<ast::TypeExpr>(mapReq(*payload, "ty"), parseTypeExpr);
      return ast::TypeBodyItem{std::move(f)};
    }
    if (name == "Variant") {
      return ast::TypeBodyItem{ast::TypeBodyVariant{parseVariantDecl(*payload)}};
    }
    if (name == "Method") {
      auto fn = std::make_unique<ast::FnDecl>(parseFnDecl(*payload));
      ast::TypeBodyMethod m;
      m.fn = fn.get();
      td.method_storage.push_back(std::move(fn));
      return ast::TypeBodyItem{std::move(m)};
    }
    fail("unknown TypeBodyItem variant: " + name);
  });
  const auto *dc = mapGet(obj, "doc_comment");
  if (dc && !isNil(*dc))
    td.doc_comment = getString(*dc);
  return td;
}

// ── TypeAliasDecl ───────────────────────────────────────────────────────────

static ast::TypeAliasDecl parseTypeAliasDecl(const msgpack::object &obj) {
  ast::TypeAliasDecl ta;
  const auto *isPub = mapGet(obj, "is_pub");
  if (isPub && !isNil(*isPub))
    ta.is_pub = getBool(*isPub);
  ta.name = getString(mapReq(obj, "name"));
  ta.ty = parseSpanned<ast::TypeExpr>(mapReq(obj, "ty"), parseTypeExpr);
  return ta;
}

// ── TraitDecl ───────────────────────────────────────────────────────────────

static ast::TraitMethod parseTraitMethod(const msgpack::object &obj) {
  ast::TraitMethod tm;
  tm.name = getString(mapReq(obj, "name"));
  const auto *isPure = mapGet(obj, "is_pure");
  if (isPure && !isNil(*isPure))
    tm.is_pure = getBool(*isPure);
  const auto *tp = mapGet(obj, "type_params");
  if (tp && !isNil(*tp))
    tm.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
  tm.params = parseVec<ast::Param>(mapReq(obj, "params"), parseParam);
  const auto *rt = mapGet(obj, "return_type");
  if (rt && !isNil(*rt))
    tm.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
  const auto *wc = mapGet(obj, "where_clause");
  if (wc && !isNil(*wc))
    tm.where_clause = parseWhereClause(*wc);
  const auto *body = mapGet(obj, "body");
  if (body && !isNil(*body))
    tm.body = parseBlock(*body);
  return tm;
}

static ast::TraitDecl parseTraitDecl(const msgpack::object &obj) {
  ast::TraitDecl td;
  const auto *isPub = mapGet(obj, "is_pub");
  if (isPub && !isNil(*isPub))
    td.is_pub = getBool(*isPub);
  td.name = getString(mapReq(obj, "name"));
  const auto *tp = mapGet(obj, "type_params");
  if (tp && !isNil(*tp))
    td.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
  const auto *st = mapGet(obj, "super_traits");
  if (st && !isNil(*st))
    td.super_traits = parseVec<ast::TraitBound>(*st, parseTraitBound);
  td.items = parseVec<ast::TraitItem>(mapReq(obj, "items"), [](const msgpack::object &o) {
    auto [name, payload] = getEnumVariant(o);
    if (name == "Method") {
      return ast::TraitItem{ast::TraitItemMethod{parseTraitMethod(*payload)}};
    }
    if (name == "AssociatedType") {
      ast::TraitItemAssociatedType at;
      at.name = getString(mapReq(*payload, "name"));
      at.bounds = parseVec<ast::TraitBound>(mapReq(*payload, "bounds"), parseTraitBound);
      return ast::TraitItem{std::move(at)};
    }
    fail("unknown TraitItem variant: " + name);
  });
  const auto *dc = mapGet(obj, "doc_comment");
  if (dc && !isNil(*dc))
    td.doc_comment = getString(*dc);
  return td;
}

// ── ImplDecl ────────────────────────────────────────────────────────────────

static ast::ImplDecl parseImplDecl(const msgpack::object &obj) {
  ast::ImplDecl id;
  const auto *tp = mapGet(obj, "type_params");
  if (tp && !isNil(*tp))
    id.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
  const auto *tb = mapGet(obj, "trait_bound");
  if (tb && !isNil(*tb))
    id.trait_bound = parseTraitBound(*tb);
  id.target_type = parseSpanned<ast::TypeExpr>(mapReq(obj, "target_type"), parseTypeExpr);
  const auto *wc = mapGet(obj, "where_clause");
  if (wc && !isNil(*wc))
    id.where_clause = parseWhereClause(*wc);
  id.methods = parseVec<ast::FnDecl>(mapReq(obj, "methods"), parseFnDecl);
  return id;
}

// ── WireDecl ────────────────────────────────────────────────────────────────

static ast::NamingCase parseNamingCase(const msgpack::object &obj) {
  auto s = getString(obj);
  if (s == "CamelCase")
    return ast::NamingCase::CamelCase;
  if (s == "PascalCase")
    return ast::NamingCase::PascalCase;
  if (s == "SnakeCase")
    return ast::NamingCase::SnakeCase;
  if (s == "ScreamingSnake")
    return ast::NamingCase::ScreamingSnake;
  if (s == "KebabCase")
    return ast::NamingCase::KebabCase;
  fail("unknown NamingCase: " + s);
}

static ast::WireFieldDecl parseWireFieldDecl(const msgpack::object &obj) {
  ast::WireFieldDecl wf;
  wf.name = getString(mapReq(obj, "name"));
  wf.ty = getString(mapReq(obj, "ty"));
  wf.field_number = static_cast<uint32_t>(getUint(mapReq(obj, "field_number")));
  wf.is_optional = getBool(mapReq(obj, "is_optional"));
  wf.is_repeated = getBool(mapReq(obj, "is_repeated"));
  wf.is_reserved = getBool(mapReq(obj, "is_reserved"));
  wf.is_deprecated = getBool(mapReq(obj, "is_deprecated"));
  const auto *jn = mapGet(obj, "json_name");
  if (jn && !isNil(*jn))
    wf.json_name = getString(*jn);
  const auto *yn = mapGet(obj, "yaml_name");
  if (yn && !isNil(*yn))
    wf.yaml_name = getString(*yn);
  return wf;
}

static ast::WireDecl parseWireDecl(const msgpack::object &obj) {
  ast::WireDecl wd;
  const auto *isPub = mapGet(obj, "is_pub");
  if (isPub && !isNil(*isPub))
    wd.is_pub = getBool(*isPub);
  auto kindStr = getString(mapReq(obj, "kind"));
  wd.kind = (kindStr == "Enum") ? ast::WireDeclKind::Enum : ast::WireDeclKind::Struct;
  wd.name = getString(mapReq(obj, "name"));
  wd.fields = parseVec<ast::WireFieldDecl>(mapReq(obj, "fields"), parseWireFieldDecl);
  wd.variants = parseVec<ast::VariantDecl>(mapReq(obj, "variants"), parseVariantDecl);
  const auto *jc = mapGet(obj, "json_case");
  if (jc && !isNil(*jc))
    wd.json_case = parseNamingCase(*jc);
  const auto *yc = mapGet(obj, "yaml_case");
  if (yc && !isNil(*yc))
    wd.yaml_case = parseNamingCase(*yc);
  return wd;
}

// ── ExternBlock ─────────────────────────────────────────────────────────────

static ast::ExternFnDecl parseExternFnDecl(const msgpack::object &obj) {
  ast::ExternFnDecl ef;
  ef.name = getString(mapReq(obj, "name"));
  ef.params = parseVec<ast::Param>(mapReq(obj, "params"), parseParam);
  const auto *rt = mapGet(obj, "return_type");
  if (rt && !isNil(*rt))
    ef.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
  ef.is_variadic = getBool(mapReq(obj, "is_variadic"));
  return ef;
}

static ast::ExternBlock parseExternBlock(const msgpack::object &obj) {
  ast::ExternBlock eb;
  eb.abi = getString(mapReq(obj, "abi"));
  eb.functions = parseVec<ast::ExternFnDecl>(mapReq(obj, "functions"), parseExternFnDecl);
  return eb;
}

// ── ActorDecl ───────────────────────────────────────────────────────────────

static ast::FieldDecl parseFieldDecl(const msgpack::object &obj) {
  ast::FieldDecl fd;
  fd.name = getString(mapReq(obj, "name"));
  fd.ty = parseSpanned<ast::TypeExpr>(mapReq(obj, "ty"), parseTypeExpr);
  return fd;
}

static ast::ReceiveFnDecl parseReceiveFnDecl(const msgpack::object &obj) {
  ast::ReceiveFnDecl rf;
  rf.is_generator = getBool(mapReq(obj, "is_generator"));
  rf.is_pure = getBool(mapReq(obj, "is_pure"));
  rf.name = getString(mapReq(obj, "name"));
  const auto *tp = mapGet(obj, "type_params");
  if (tp && !isNil(*tp))
    rf.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
  rf.params = parseVec<ast::Param>(mapReq(obj, "params"), parseParam);
  const auto *rt = mapGet(obj, "return_type");
  if (rt && !isNil(*rt))
    rf.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
  const auto *wc = mapGet(obj, "where_clause");
  if (wc && !isNil(*wc))
    rf.where_clause = parseWhereClause(*wc);
  rf.body = parseBlock(mapReq(obj, "body"));
  rf.span = parseSpan(mapReq(obj, "span"));
  return rf;
}

static ast::ActorInit parseActorInit(const msgpack::object &obj) {
  ast::ActorInit ai;
  ai.params = parseVec<ast::Param>(mapReq(obj, "params"), parseParam);
  ai.body = parseBlock(mapReq(obj, "body"));
  return ai;
}

static ast::OverflowPolicy parseOverflowPolicy(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);
  if (name == "DropNew")
    return ast::OverflowDropNew{};
  if (name == "DropOld")
    return ast::OverflowDropOld{};
  if (name == "Block")
    return ast::OverflowBlock{};
  if (name == "Fail")
    return ast::OverflowFail{};
  if (name == "Coalesce") {
    ast::OverflowCoalesce c;
    c.key_field = getString(mapReq(*payload, "key_field"));
    const auto *fb = mapGet(*payload, "fallback");
    if (fb && !isNil(*fb)) {
      auto s = getString(*fb);
      if (s == "DropNew")
        c.fallback = ast::OverflowFallback::DropNew;
      else if (s == "DropOld")
        c.fallback = ast::OverflowFallback::DropOld;
      else if (s == "Block")
        c.fallback = ast::OverflowFallback::Block;
      else if (s == "Fail")
        c.fallback = ast::OverflowFallback::Fail;
    }
    return c;
  }
  fail("unknown OverflowPolicy variant: " + name);
}

static ast::ActorDecl parseActorDecl(const msgpack::object &obj) {
  ast::ActorDecl ad;
  const auto *isPub = mapGet(obj, "is_pub");
  if (isPub && !isNil(*isPub))
    ad.is_pub = getBool(*isPub);
  ad.name = getString(mapReq(obj, "name"));
  const auto *st = mapGet(obj, "super_traits");
  if (st && !isNil(*st))
    ad.super_traits = parseVec<ast::TraitBound>(*st, parseTraitBound);
  const auto *init = mapGet(obj, "init");
  if (init && !isNil(*init))
    ad.init = parseActorInit(*init);
  ad.fields = parseVec<ast::FieldDecl>(mapReq(obj, "fields"), parseFieldDecl);
  ad.receive_fns = parseVec<ast::ReceiveFnDecl>(mapReq(obj, "receive_fns"), parseReceiveFnDecl);
  ad.methods = parseVec<ast::FnDecl>(mapReq(obj, "methods"), parseFnDecl);
  const auto *mc = mapGet(obj, "mailbox_capacity");
  if (mc && !isNil(*mc))
    ad.mailbox_capacity = static_cast<uint32_t>(getUint(*mc));
  const auto *op = mapGet(obj, "overflow_policy");
  if (op && !isNil(*op))
    ad.overflow_policy = parseOverflowPolicy(*op);
  const auto *iso = mapGet(obj, "is_isolated");
  if (iso && !isNil(*iso))
    ad.is_isolated = getBool(*iso);
  const auto *dc = mapGet(obj, "doc_comment");
  if (dc && !isNil(*dc))
    ad.doc_comment = getString(*dc);
  return ad;
}

// ── SupervisorDecl ──────────────────────────────────────────────────────────

static ast::ChildSpec parseChildSpec(const msgpack::object &obj) {
  ast::ChildSpec cs;
  cs.name = getString(mapReq(obj, "name"));
  cs.actor_type = getString(mapReq(obj, "actor_type"));
  cs.args = parseVec<ast::Spanned<ast::Expr>>(mapReq(obj, "args"), [](const msgpack::object &o) {
    return parseSpanned<ast::Expr>(o, parseExpr);
  });
  const auto *r = mapGet(obj, "restart");
  if (r && !isNil(*r)) {
    auto s = getString(*r);
    if (s == "Permanent")
      cs.restart = ast::RestartPolicy::Permanent;
    else if (s == "Transient")
      cs.restart = ast::RestartPolicy::Transient;
    else if (s == "Temporary")
      cs.restart = ast::RestartPolicy::Temporary;
  }
  return cs;
}

static ast::SupervisorDecl parseSupervisorDecl(const msgpack::object &obj) {
  ast::SupervisorDecl sd;
  const auto *isPub = mapGet(obj, "is_pub");
  if (isPub && !isNil(*isPub))
    sd.is_pub = getBool(*isPub);
  sd.name = getString(mapReq(obj, "name"));
  const auto *strat = mapGet(obj, "strategy");
  if (strat && !isNil(*strat)) {
    auto s = getString(*strat);
    if (s == "OneForOne")
      sd.strategy = ast::SupervisorStrategy::OneForOne;
    else if (s == "OneForAll")
      sd.strategy = ast::SupervisorStrategy::OneForAll;
    else if (s == "RestForOne")
      sd.strategy = ast::SupervisorStrategy::RestForOne;
  }
  const auto *mr = mapGet(obj, "max_restarts");
  if (mr && !isNil(*mr))
    sd.max_restarts = getInt(*mr);
  const auto *w = mapGet(obj, "window");
  if (w && !isNil(*w))
    sd.window = getString(*w);
  sd.children = parseVec<ast::ChildSpec>(mapReq(obj, "children"), parseChildSpec);
  return sd;
}

// ── Item ────────────────────────────────────────────────────────────────────

static ast::Item parseItem(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);

  if (name == "Import")
    return ast::Item{parseImportDecl(*payload)};
  if (name == "Const")
    return ast::Item{parseConstDecl(*payload)};
  if (name == "TypeDecl")
    return ast::Item{parseTypeDecl(*payload)};
  if (name == "TypeAlias")
    return ast::Item{parseTypeAliasDecl(*payload)};
  if (name == "Trait")
    return ast::Item{parseTraitDecl(*payload)};
  if (name == "Impl")
    return ast::Item{parseImplDecl(*payload)};
  if (name == "Wire")
    return ast::Item{parseWireDecl(*payload)};
  if (name == "Function")
    return ast::Item{parseFnDecl(*payload)};
  if (name == "ExternBlock")
    return ast::Item{parseExternBlock(*payload)};
  if (name == "Actor")
    return ast::Item{parseActorDecl(*payload)};
  if (name == "Supervisor")
    return ast::Item{parseSupervisorDecl(*payload)};
  fail("unknown Item variant: " + name);
}

static ast::ModuleId parseModuleId(const msgpack::object &obj) {
  ast::ModuleId id;
  // JSON input: module IDs are serialized as flat strings (e.g., "std::misc::log")
  // because JSON objects require string keys. Split on "::" to recover path segments.
  if (obj.type == msgpack::type::STR) {
    auto s = getString(obj);
    if (s == "(root)") {
      // Root module has an empty path.
      return id;
    }
    // Split on "::"
    size_t pos = 0;
    while (pos < s.size()) {
      auto next = s.find("::", pos);
      if (next == std::string::npos) {
        id.path.push_back(s.substr(pos));
        break;
      }
      id.path.push_back(s.substr(pos, next - pos));
      pos = next + 2;
    }
    return id;
  }
  // Msgpack input: module IDs are serialized as {"path": ["seg1", "seg2", ...]}.
  id.path = parseVec<std::string>(mapReq(obj, "path"),
                                  [](const msgpack::object &o) { return getString(o); });
  return id;
}

static ast::ModuleImport parseModuleImport(const msgpack::object &obj) {
  ast::ModuleImport mi;
  mi.target = parseModuleId(mapReq(obj, "target"));
  const auto *spec = mapGet(obj, "spec");
  if (spec && !isNil(*spec)) {
    auto [name, payload] = getEnumVariant(*spec);
    if (name == "Glob") {
      mi.spec = ast::ImportSpecGlob{};
    } else if (name == "Names") {
      mi.spec =
          ast::ImportSpecNames{parseVec<ast::ImportName>(*payload, [](const msgpack::object &o) {
            ast::ImportName n;
            n.name = getString(mapReq(o, "name"));
            const auto *alias = mapGet(o, "alias");
            if (alias && !isNil(*alias))
              n.alias = getString(*alias);
            return n;
          })};
    }
  }
  mi.span = parseSpan(mapReq(obj, "span"));
  return mi;
}

static ast::Module parseModule(const msgpack::object &obj) {
  ast::Module m;
  m.id = parseModuleId(mapReq(obj, "id"));
  m.items = parseVec<ast::Spanned<ast::Item>>(mapReq(obj, "items"), [](const msgpack::object &o) {
    return parseSpanned<ast::Item>(o, parseItem);
  });
  m.imports = parseVec<ast::ModuleImport>(mapReq(obj, "imports"), parseModuleImport);
  const auto *sp = mapGet(obj, "source_path");
  if (sp && !isNil(*sp))
    m.source_path = getString(*sp);
  const auto *doc = mapGet(obj, "doc");
  if (doc && !isNil(*doc))
    m.doc = getString(*doc);
  return m;
}

static ast::ModuleGraph parseModuleGraph(const msgpack::object &obj) {
  ast::ModuleGraph mg;
  mg.root = parseModuleId(mapReq(obj, "root"));
  mg.topo_order = parseVec<ast::ModuleId>(mapReq(obj, "topo_order"), parseModuleId);

  const auto &modulesObj = mapReq(obj, "modules");
  if (modulesObj.type == msgpack::type::ARRAY) {
    for (uint32_t i = 0; i < modulesObj.via.array.size; ++i) {
      const auto &pair = modulesObj.via.array.ptr[i];
      if (pair.type == msgpack::type::ARRAY && pair.via.array.size == 2) {
        auto id = parseModuleId(pair.via.array.ptr[0]);
        auto mod = parseModule(pair.via.array.ptr[1]);
        mg.modules.emplace(std::move(id), std::move(mod));
      }
    }
  } else if (modulesObj.type == msgpack::type::MAP) {
    for (uint32_t i = 0; i < modulesObj.via.map.size; ++i) {
      auto id = parseModuleId(modulesObj.via.map.ptr[i].key);
      auto mod = parseModule(modulesObj.via.map.ptr[i].val);
      mg.modules.emplace(std::move(id), std::move(mod));
    }
  }
  return mg;
}

// ── Program (top-level) ─────────────────────────────────────────────────────

static ast::ExprTypeEntry parseExprTypeEntry(const msgpack::object &obj) {
  ast::ExprTypeEntry entry;
  entry.start = getUint(mapReq(obj, "start"));
  entry.end = getUint(mapReq(obj, "end"));
  entry.ty = parseSpanned<ast::TypeExpr>(mapReq(obj, "ty"), parseTypeExpr);
  return entry;
}

static ast::Program parseProgram(const msgpack::object &obj) {
  ast::Program prog;
  prog.items =
      parseVec<ast::Spanned<ast::Item>>(mapReq(obj, "items"), [](const msgpack::object &o) {
        return parseSpanned<ast::Item>(o, parseItem);
      });
  const auto *md = mapGet(obj, "module_doc");
  if (md && !isNil(*md))
    prog.module_doc = getString(*md);
  const auto *et = mapGet(obj, "expr_types");
  if (et && !isNil(*et))
    prog.expr_types = parseVec<ast::ExprTypeEntry>(*et, parseExprTypeEntry);

  // Handle type metadata: list of known handle type names
  const auto *ht = mapGet(obj, "handle_types");
  if (ht && !isNil(*ht))
    prog.handle_types =
        parseVec<std::string>(*ht, [](const msgpack::object &o) { return getString(o); });

  // Handle type representations: map of type name → repr string ("i32", etc.)
  const auto *hr = mapGet(obj, "handle_type_repr");
  if (hr && !isNil(*hr) && hr->type == msgpack::type::MAP) {
    for (uint32_t i = 0; i < hr->via.map.size; ++i) {
      auto &kv = hr->via.map.ptr[i];
      std::string key = getString(kv.key);
      std::string val = getString(kv.val);
      prog.handle_type_repr[key] = val;
    }
  }
  const auto *mg = mapGet(obj, "module_graph");
  if (mg && !isNil(*mg))
    prog.module_graph = parseModuleGraph(*mg);

  return prog;
}

// ── Public API ──────────────────────────────────────────────────────────────

ast::Program parseMsgpackAST(const uint8_t *data, size_t size) {
  msgpack::object_handle oh = msgpack::unpack(reinterpret_cast<const char *>(data), size);
  return parseProgram(oh.get());
}

ast::Program parseJsonAST(const uint8_t *data, size_t size) {
  // Parse JSON, convert to msgpack bytes, then reuse the existing parser.
  auto j = nlohmann::json::parse(data, data + size);
  auto msgpackBytes = nlohmann::json::to_msgpack(j);
  return parseMsgpackAST(msgpackBytes.data(), msgpackBytes.size());
}

} // namespace hew
