/// Hard-coded C++ parser for Literal (custom serde, not derive(Serialize)).
pub fn literal_parser() -> &'static str {
    r#"static ast::Literal parseLiteral(const msgpack::object &obj) {
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
}"#
}

/// Hard-coded C++ parser for `ExprTypeEntry` (C++-only type from serialization layer).
pub fn expr_type_entry_parser() -> &'static str {
    r#"static ast::ExprTypeEntry parseExprTypeEntry(const msgpack::object &obj) {
  ast::ExprTypeEntry entry;
  entry.start = getUint(mapReq(obj, "start"));
  entry.end = getUint(mapReq(obj, "end"));
  entry.ty = parseSpanned<ast::TypeExpr>(mapReq(obj, "ty"), parseTypeExpr);
  return entry;
}"#
}

/// Hard-coded C++ parser for `ModuleGraph` (`HashMap`<`ModuleId`, Module> with custom hasher).
pub fn module_graph_parser() -> &'static str {
    r#"static ast::ModuleGraph parseModuleGraph(const msgpack::object &obj) {
  ast::ModuleGraph mg;
  mg.root = parseModuleId(mapReq(obj, "root"));
  mg.topo_order = parseVec<ast::ModuleId>(mapReq(obj, "topo_order"), parseModuleId);

  const auto &modulesObj = mapReq(obj, "modules");
  if (modulesObj.type != msgpack::type::MAP)
    fail("expected map for ModuleGraph.modules");
  for (uint32_t i = 0; i < modulesObj.via.map.size; ++i) {
    auto id = parseModuleId(modulesObj.via.map.ptr[i].key);
    auto mod = parseModule(modulesObj.via.map.ptr[i].val);
    mg.modules.emplace(std::move(id), std::move(mod));
  }
  return mg;
}"#
}

/// Hard-coded C++ parser for Program (wraps `TypedProgram` with extra fields).
pub fn program_parser() -> &'static str {
    r#"static ast::Program parseProgram(const msgpack::object &obj) {
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
}"#
}

/// Hard-coded parser for `TypeDecl` (has `method_storage` ownership pattern).
pub fn type_decl_parser() -> &'static str {
    r#"static ast::TypeDecl parseTypeDecl(const msgpack::object &obj) {
  ast::TypeDecl td;
  const auto *vis = mapGet(obj, "visibility");
  if (vis && !isNil(*vis))
    td.visibility = parseVisibility(*vis);
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
}"#
}

/// Public API functions at the end of the file.
pub fn public_api() -> &'static str {
    r"ast::Program parseMsgpackAST(const uint8_t *data, size_t size) {
  msgpack::object_handle oh = msgpack::unpack(reinterpret_cast<const char *>(data), size);
  return parseProgram(oh.get());
}

ast::Program parseJsonAST(const uint8_t *data, size_t size) {
  // Parse JSON, convert to msgpack bytes, then reuse the existing parser.
  auto j = nlohmann::json::parse(data, data + size);
  auto msgpackBytes = nlohmann::json::to_msgpack(j);
  return parseMsgpackAST(msgpackBytes.data(), msgpackBytes.size());
}"
}

/// File header (includes and namespace).
pub fn file_header() -> &'static str {
    r#"//===- msgpack_reader_gen.cpp - GENERATED - Deserialize msgpack AST --------===//
//
// AUTO-GENERATED by hew-astgen from hew-parser/src/ast.rs
// DO NOT EDIT - changes will be overwritten.
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

namespace hew {"#
}

/// Helper functions preamble (shared utilities used by all parsers).
pub fn helpers_preamble() -> &'static str {
    r#"// ── Error helper ────────────────────────────────────────────────────────────

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
  if (obj.type == msgpack::type::POSITIVE_INTEGER)
    return static_cast<int64_t>(obj.via.u64);
  if (obj.type == msgpack::type::NEGATIVE_INTEGER)
    return obj.via.i64;
  fail("expected integer, got type " + std::to_string(obj.type));
}

/// Get unsigned integer from msgpack object.
static uint64_t getUint(const msgpack::object &obj) {
  if (obj.type == msgpack::type::POSITIVE_INTEGER)
    return obj.via.u64;
  if (obj.type == msgpack::type::NEGATIVE_INTEGER)
    return static_cast<uint64_t>(obj.via.i64);
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

/// Get a map as an array of key-value pairs.
static const msgpack::object_kv *mapEntries(const msgpack::object &obj, uint32_t &size) {
  if (obj.type != msgpack::type::MAP)
    fail("expected map, got type " + std::to_string(obj.type));
  size = obj.via.map.size;
  return obj.via.map.ptr;
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
}"#
}

/// Span/Spanned/Optional/Vec template helpers.
pub fn template_helpers() -> &'static str {
    r#"// ── Span / Spanned ──────────────────────────────────────────────────────────

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
}"#
}
