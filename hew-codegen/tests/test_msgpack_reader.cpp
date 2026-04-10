//===- test_msgpack_reader.cpp - Tests for msgpack AST deserialization -----===//
//
// Verifies that parseMsgpackAST handles malformed, truncated, and corrupt
// input correctly. Happy-path deserialization is exercised exhaustively by
// test_mlirgen (which feeds real Hew programs through the same parser).
// These tests target error paths and edge cases.
//
//===----------------------------------------------------------------------===//

#include "hew/msgpack_reader.h"

#include <msgpack.hpp>

#include <cassert>
#include <cstdio>
#include <cstring>
#include <functional>
#include <stdexcept>
#include <string>
#include <vector>

#ifndef _WIN32
#include <sys/wait.h>
#include <unistd.h>
#endif

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name)                                                                                 \
  do {                                                                                             \
    tests_run++;                                                                                   \
    printf("  test %s ... ", #name);                                                               \
  } while (0)

#define PASS()                                                                                     \
  do {                                                                                             \
    tests_passed++;                                                                                \
    printf("ok\n");                                                                                \
  } while (0)

#define FAIL(msg)                                                                                  \
  do {                                                                                             \
    printf("FAILED: %s\n", msg);                                                                   \
  } while (0)

// ---------------------------------------------------------------------------
// Helper: run a function in a child process to test that it rejects input.
// On macOS, C++ exception handling across static library boundaries is
// unreliable (Homebrew LLVM builds with -fno-exceptions propagated via
// HandleLLVMOptions). Using fork() isolates the test: if the child exits
// non-zero or is killed by a signal, the input was rejected (good).
// On Windows, exceptions work normally so we use try/catch directly.
// ---------------------------------------------------------------------------
#ifndef _WIN32
static bool rejects_in_child(std::function<void()> fn) {
  fflush(stdout);
  pid_t pid = fork();
  if (pid == 0) {
    fn();
    _exit(0); // reached only if fn didn't throw/abort
  }
  int status = 0;
  waitpid(pid, &status, 0);
  // Rejected = child did NOT exit 0 (threw, aborted, or signalled)
  return !(WIFEXITED(status) && WEXITSTATUS(status) == 0);
}
#endif

// ═══════════════════════════════════════════════════════════════════════════
// Error handling: invalid input
//
// These tests verify parseMsgpackAST rejects bad input. On macOS,
// C++ exception handling is unreliable across static library boundaries
// (Homebrew LLVM), so we use fork() to isolate the throwing code.
// ═══════════════════════════════════════════════════════════════════════════

// Macro: verify that an expression does NOT succeed (throws or aborts).
// On Unix, uses fork() to isolate. On Windows, uses try/catch.
#ifdef _WIN32
#define EXPECT_REJECTS(expr)                                                                       \
  do {                                                                                             \
    try {                                                                                          \
      expr;                                                                                        \
      FAIL("expected rejection but call succeeded");                                               \
      return;                                                                                      \
    } catch (...) {                                                                                \
    }                                                                                              \
  } while (0)
#else
#define EXPECT_REJECTS(expr)                                                                       \
  do {                                                                                             \
    if (!rejects_in_child([&] { expr; })) {                                                        \
      FAIL("expected rejection but call succeeded");                                               \
      return;                                                                                      \
    }                                                                                              \
  } while (0)
#endif

static void test_empty_input_rejects() {
  TEST(empty_input_rejects);
  uint8_t empty = 0;
  EXPECT_REJECTS(hew::parseMsgpackAST(&empty, 0));
  PASS();
}

static void test_truncated_msgpack_rejects() {
  TEST(truncated_msgpack_rejects);
  uint8_t truncated[] = {0x8A, 0xA1}; // fixmap(10), fixstr(1) — then EOF
  EXPECT_REJECTS(hew::parseMsgpackAST(truncated, sizeof(truncated)));
  PASS();
}

static void test_wrong_top_level_type_rejects() {
  TEST(wrong_top_level_type_rejects);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_array(3);
  pk.pack(1);
  pk.pack(2);
  pk.pack(3);
  EXPECT_REJECTS(hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_single_byte_garbage_rejects() {
  TEST(single_byte_garbage_rejects);
  uint8_t garbage[] = {0xC1}; // 0xC1 is "never used" in msgpack spec
  EXPECT_REJECTS(hew::parseMsgpackAST(garbage, sizeof(garbage)));
  PASS();
}

// ═══════════════════════════════════════════════════════════════════════════
// Error handling: structurally valid msgpack, semantically wrong AST
// ═══════════════════════════════════════════════════════════════════════════

// Helper: pack a map with schema_version = 6 and the given extra fields
static std::vector<uint8_t>
packWithSchema(std::function<void(msgpack::packer<msgpack::sbuffer> &)> extraFields,
               int extraCount) {
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  // Required top-level fields: schema_version, items, expr_types,
  // method_call_receiver_kinds, assign_target_kinds, assign_target_shapes,
  // lowering_facts, handle_types, handle_type_repr
  pk.pack_map(9 + extraCount);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack_array(0); // empty array
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);
  extraFields(pk);
  return {reinterpret_cast<const uint8_t *>(buf.data()),
          reinterpret_cast<const uint8_t *>(buf.data()) + buf.size()};
}

static void test_missing_schema_version_rejects() {
  TEST(missing_schema_version_rejects);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(1);
  pk.pack(std::string("items"));
  pk.pack_array(0);
  EXPECT_REJECTS(hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_wrong_schema_version_rejects() {
  TEST(wrong_schema_version_rejects);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(1);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(999));
  EXPECT_REJECTS(hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_items_wrong_type_rejects() {
  TEST(items_wrong_type_rejects);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(2);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack(42); // should be array
  EXPECT_REJECTS(hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_minimal_valid_program_parses() {
  TEST(minimal_valid_program_parses);
  auto data = packWithSchema([](msgpack::packer<msgpack::sbuffer> &) {}, 0);
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.schema_version != 6) {
      FAIL("schema_version should be 6");
      return;
    }
    if (!prog.items.empty()) {
      FAIL("items should be empty");
      return;
    }
  } catch (...) {
    std::string detail = "unexpected exception";
    FAIL(detail.c_str());
    return;
  }
  PASS();
}

// ─── drop_funcs parsing ──────────────────────────────────────────────────────

static void test_drop_funcs_roundtrip() {
  TEST(drop_funcs_roundtrip);
  // Build a minimal program payload that includes a drop_funcs array.
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(10); // 9 required + drop_funcs
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);
  pk.pack(std::string("drop_funcs"));
  pk.pack_array(2);
  pk.pack_array(2);
  pk.pack(std::string("http.Request"));
  pk.pack(std::string("hew_http_request_free"));
  pk.pack_array(2);
  pk.pack(std::string("http.Server"));
  pk.pack(std::string("hew_http_server_close"));

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    auto it = prog.drop_funcs.find("http.Request");
    if (it == prog.drop_funcs.end() || it->second != "hew_http_request_free") {
      FAIL("http.Request drop func not parsed correctly");
      return;
    }
    auto it2 = prog.drop_funcs.find("http.Server");
    if (it2 == prog.drop_funcs.end() || it2->second != "hew_http_server_close") {
      FAIL("http.Server drop func not parsed correctly");
      return;
    }
    if (prog.drop_funcs.size() != 2) {
      FAIL("expected exactly 2 drop_funcs entries");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
  PASS();
}

static void test_drop_funcs_absent_gives_empty_map() {
  TEST(drop_funcs_absent_gives_empty_map);
  // A payload without drop_funcs should produce an empty map (field is optional).
  auto data = packWithSchema([](msgpack::packer<msgpack::sbuffer> &) {}, 0);
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (!prog.drop_funcs.empty()) {
      FAIL("drop_funcs should be empty when absent from payload");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
  PASS();
}

static void test_drop_funcs_wrong_type_rejects() {
  TEST(drop_funcs_wrong_type_rejects);
  // drop_funcs must be an array, not a map.
  auto data = packWithSchema(
      [](msgpack::packer<msgpack::sbuffer> &pk) {
        pk.pack(std::string("drop_funcs"));
        pk.pack_map(1); // wrong: should be array
        pk.pack(std::string("http.Request"));
        pk.pack(std::string("hew_http_request_free"));
      },
      1);
  EXPECT_REJECTS(hew::parseMsgpackAST(data.data(), data.size()));
  PASS();
}

static void test_method_call_receiver_kinds_roundtrip() {
  TEST(method_call_receiver_kinds_roundtrip);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(9);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(2);

  pk.pack_map(4);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(20));
  pk.pack(std::string("kind"));
  pk.pack(std::string("named_type_instance"));
  pk.pack(std::string("type_name"));
  pk.pack(std::string("Widget"));

  pk.pack_map(4);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(30));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(40));
  pk.pack(std::string("kind"));
  pk.pack(std::string("trait_object"));
  pk.pack(std::string("trait_name"));
  pk.pack(std::string("Greeter"));

  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);
  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());

  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.method_call_receiver_kinds.size() != 2) {
      FAIL("expected two method_call_receiver_kinds entries");
      return;
    }
    auto *named = std::get_if<hew::ast::MethodCallReceiverKindNamedTypeInstance>(
        &prog.method_call_receiver_kinds[0].kind);
    if (!named || named->type_name != "Widget") {
      FAIL("named type receiver kind not parsed correctly");
      return;
    }
    auto *trait = std::get_if<hew::ast::MethodCallReceiverKindTraitObject>(
        &prog.method_call_receiver_kinds[1].kind);
    if (!trait || trait->trait_name != "Greeter") {
      FAIL("trait-object receiver kind not parsed correctly");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
  PASS();
}

static void test_lowering_facts_roundtrip() {
  TEST(lowering_facts_roundtrip);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(9);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(1);
  pk.pack_map(6);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(20));
  pk.pack(std::string("kind"));
  pk.pack(std::string("hash_set"));
  pk.pack(std::string("element_type"));
  pk.pack(std::string("str"));
  pk.pack(std::string("abi_variant"));
  pk.pack(std::string("string"));
  pk.pack(std::string("drop_kind"));
  pk.pack(std::string("hash_set_free"));
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());

  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.lowering_facts.size() != 1) {
      FAIL("expected one lowering_facts entry");
      return;
    }
    const auto &fact = prog.lowering_facts[0];
    if (fact.start != 10 || fact.end != 20) {
      FAIL("lowering fact span wrong");
      return;
    }
    if (fact.kind != hew::ast::LoweringKind::HashSet) {
      FAIL("lowering fact kind wrong");
      return;
    }
    if (fact.element_type != hew::ast::HashSetElementType::Str) {
      FAIL("lowering fact element type wrong");
      return;
    }
    if (fact.abi_variant != hew::ast::HashSetAbi::String) {
      FAIL("lowering fact ABI wrong");
      return;
    }
    if (fact.drop_kind != hew::ast::DropKind::HashSetFree) {
      FAIL("lowering fact drop kind wrong");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
  PASS();
}

// ─── assign_target_shapes parsing ───────────────────────────────────────────

static void test_assign_target_shapes_roundtrip() {
  TEST(assign_target_shapes_roundtrip);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  // 9 required fields; assign_target_shapes carries the populated entries.
  pk.pack_map(9);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  // Two assign_target_shapes entries: one unsigned, one signed
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(2);
  // Entry 0: unsigned (u8 target)
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(5));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("is_unsigned"));
  pk.pack(true);
  // Entry 1: signed (i32 target)
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(20));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(30));
  pk.pack(std::string("is_unsigned"));
  pk.pack(false);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.assign_target_shapes.size() != 2) {
      FAIL("expected 2 assign_target_shapes entries");
      return;
    }
    if (prog.assign_target_shapes[0].start != 5 || prog.assign_target_shapes[0].end != 10) {
      FAIL("first shape entry span wrong");
      return;
    }
    if (!prog.assign_target_shapes[0].is_unsigned) {
      FAIL("first shape entry should be unsigned");
      return;
    }
    if (prog.assign_target_shapes[1].start != 20 || prog.assign_target_shapes[1].end != 30) {
      FAIL("second shape entry span wrong");
      return;
    }
    if (prog.assign_target_shapes[1].is_unsigned) {
      FAIL("second shape entry should be signed (is_unsigned=false)");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
  PASS();
}

// ─── assign_target_kinds parsing ────────────────────────────────────────────

static void test_assign_target_kinds_roundtrip() {
  TEST(assign_target_kinds_roundtrip);
  // Certify that all four AssignTargetKindData variants are parsed correctly
  // by the C++ reader: local_var, actor_field, field_access, index.
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(9);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  // Four assign_target_kinds entries — one per variant.
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(4);

  // Entry 0: local_var [10..16]
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(16));
  pk.pack(std::string("kind"));
  pk.pack(std::string("local_var"));

  // Entry 1: actor_field [20..28]
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(20));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(28));
  pk.pack(std::string("kind"));
  pk.pack(std::string("actor_field"));

  // Entry 2: field_access [30..42]
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(30));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(42));
  pk.pack(std::string("kind"));
  pk.pack(std::string("field_access"));

  // Entry 3: index [50..58]
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(50));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(58));
  pk.pack(std::string("kind"));
  pk.pack(std::string("index"));

  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.assign_target_kinds.size() != 4) {
      FAIL("expected 4 assign_target_kinds entries");
      return;
    }
    if (!std::get_if<hew::ast::AssignTargetKindLocalVar>(&prog.assign_target_kinds[0].kind)) {
      FAIL("entry 0 should be LocalVar");
      return;
    }
    if (prog.assign_target_kinds[0].start != 10 || prog.assign_target_kinds[0].end != 16) {
      FAIL("entry 0 span wrong");
      return;
    }
    if (!std::get_if<hew::ast::AssignTargetKindActorField>(&prog.assign_target_kinds[1].kind)) {
      FAIL("entry 1 should be ActorField");
      return;
    }
    if (prog.assign_target_kinds[1].start != 20 || prog.assign_target_kinds[1].end != 28) {
      FAIL("entry 1 span wrong");
      return;
    }
    if (!std::get_if<hew::ast::AssignTargetKindFieldAccess>(&prog.assign_target_kinds[2].kind)) {
      FAIL("entry 2 should be FieldAccess");
      return;
    }
    if (prog.assign_target_kinds[2].start != 30 || prog.assign_target_kinds[2].end != 42) {
      FAIL("entry 2 span wrong");
      return;
    }
    if (!std::get_if<hew::ast::AssignTargetKindIndex>(&prog.assign_target_kinds[3].kind)) {
      FAIL("entry 3 should be Index");
      return;
    }
    if (prog.assign_target_kinds[3].start != 50 || prog.assign_target_kinds[3].end != 58) {
      FAIL("entry 3 span wrong");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
  PASS();
}

// ─── expr_types positive parsing ────────────────────────────────────────────

static void test_expr_types_named_roundtrip() {
  TEST(expr_types_named_roundtrip);
  // Certify that a populated expr_types entry carrying a concrete TypeNamed
  // (non-Infer) TypeExpr is parsed correctly by the C++ reader.
  //
  // Wire shape for Spanned<TypeExpr>:
  //   [TypeExpr_enum_value, {"start": N, "end": N}]
  // Wire shape for TypeExpr::Named (externally tagged by serde):
  //   {"Named": {"name": "Int", "type_args": nil}}
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(9);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack_array(0);

  // expr_types: one entry with a TypeNamed("Int") type
  pk.pack(std::string("expr_types"));
  pk.pack_array(1);
  // ExprTypeEntry map: {start, end, ty}
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(3));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(12));
  pk.pack(std::string("ty"));
  // Spanned<TypeExpr> = [TypeExpr_value, span_map]
  pk.pack_array(2);
  // TypeExpr::Named externally tagged: {"Named": {"name": "Int", "type_args": nil}}
  pk.pack_map(1);
  pk.pack(std::string("Named"));
  pk.pack_map(2);
  pk.pack(std::string("name"));
  pk.pack(std::string("Int"));
  pk.pack(std::string("type_args"));
  pk.pack_nil();
  // span for the ty field
  pk.pack_map(2);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(3));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(12));

  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.expr_types.size() != 1) {
      FAIL("expected 1 expr_types entry");
      return;
    }
    const auto &entry = prog.expr_types[0];
    if (entry.start != 3 || entry.end != 12) {
      FAIL("expr_types entry span wrong");
      return;
    }
    auto *named = std::get_if<hew::ast::TypeNamed>(&entry.ty.value.kind);
    if (!named) {
      FAIL("expr_types TypeExpr should be TypeNamed");
      return;
    }
    if (named->name != "Int") {
      FAIL("TypeNamed.name should be 'Int'");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
  PASS();
}

// ═══════════════════════════════════════════════════════════════════════════
// TypeInfer in wire: must be rejected
// ═══════════════════════════════════════════════════════════════════════════

static void test_type_infer_in_wire_rejects() {
  TEST(type_infer_in_wire_rejects);

  // Build a minimal valid program with an expr_types entry whose TypeExpr
  // is "Infer".  This represents an inference hole that was not resolved
  // before serialization — the fail-closed invariant requires rejection.
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);

  pk.pack_map(9);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));
  pk.pack(std::string("items"));
  pk.pack_array(0);

  // expr_types: one entry with type "Infer"
  pk.pack(std::string("expr_types"));
  pk.pack_array(1);
  // ExprTypeEntry: {start, end, ty}
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(0));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(1));
  pk.pack(std::string("ty"));
  // Spanned<TypeExpr>: [TypeExpr, Span]
  pk.pack_array(2);
  // TypeExpr "Infer" variant (no payload → plain string)
  pk.pack(std::string("Infer"));
  // Span: {start, end}
  pk.pack_map(2);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(0));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(1));

  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());

  EXPECT_REJECTS(hew::parseMsgpackAST(data.data(), data.size()));
  PASS();
}

// ═══════════════════════════════════════════════════════════════════════════
// Wire declaration kind rejection
//
// Verifies that a WireDecl with an unrecognised "kind" field is rejected by
// the reader.  parseWireDeclKind calls fail() for any string other than
// "Struct" or "Enum", so a future third variant that is not yet handled in
// the C++ codegen cannot silently pass as one of the known kinds.
// ═══════════════════════════════════════════════════════════════════════════

static void test_wire_unknown_decl_kind_rejects() {
  TEST(wire_unknown_decl_kind_rejects);

  // Build a minimal valid program that contains one Wire item whose "kind"
  // field is an unrecognised string.  The WireDecl is the minimal shape the
  // reader expects: {kind, name, fields, variants}.
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);

  pk.pack_map(9);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(6));

  // items: one Wire item with an unknown kind.
  // Items are Spanned<Item> = [item_value, span], where item_value is
  // a single-key enum-variant map like {"Wire": <WireDecl>}.
  pk.pack(std::string("items"));
  pk.pack_array(1);
  // Spanned<Item> = [item, span]
  pk.pack_array(2);
  // item = {"Wire": <WireDecl>}
  pk.pack_map(1);
  pk.pack(std::string("Wire"));
  // WireDecl map: kind, name, fields, variants (required fields)
  pk.pack_map(4);
  pk.pack(std::string("kind"));
  pk.pack(std::string("UnknownWireDeclKind")); // not "Struct" or "Enum"
  pk.pack(std::string("name"));
  pk.pack(std::string("BadDecl"));
  pk.pack(std::string("fields"));
  pk.pack_array(0);
  pk.pack(std::string("variants"));
  pk.pack_array(0);
  // span = {start, end}
  pk.pack_map(2);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(0));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(1));

  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());

  EXPECT_REJECTS(hew::parseMsgpackAST(data.data(), data.size()));
  PASS();
}

int main() {
  printf("Running msgpack reader tests...\n");

  // Invalid input
  test_empty_input_rejects();
  test_truncated_msgpack_rejects();
  test_wrong_top_level_type_rejects();
  test_single_byte_garbage_rejects();

  // Semantically wrong AST
  test_missing_schema_version_rejects();
  test_wrong_schema_version_rejects();
  test_items_wrong_type_rejects();
  test_minimal_valid_program_parses();

  // drop_funcs field
  test_method_call_receiver_kinds_roundtrip();
  test_lowering_facts_roundtrip();
  test_drop_funcs_roundtrip();
  test_drop_funcs_absent_gives_empty_map();
  test_drop_funcs_wrong_type_rejects();

  // assign_target_shapes field
  test_assign_target_shapes_roundtrip();

  // assign_target_kinds: all four variants
  test_assign_target_kinds_roundtrip();

  // expr_types: populated Named entry round-trip
  test_expr_types_named_roundtrip();

  // TypeInfer must not survive in the wire
  test_type_infer_in_wire_rejects();

  // Wire declaration with unknown kind must be rejected
  test_wire_unknown_decl_kind_rejects();

  printf("\n%d/%d tests passed\n", tests_passed, tests_run);
  return tests_passed == tests_run ? 0 : 1;
}
