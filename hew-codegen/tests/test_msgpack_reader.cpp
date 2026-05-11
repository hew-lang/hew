//===- test_msgpack_reader.cpp - Tests for msgpack AST deserialization -----===//
//
// Verifies that parseMsgpackAST handles malformed, truncated, and corrupt
// input correctly. Happy-path deserialization is exercised exhaustively by
// test_mlirgen (which feeds real Hew programs through the same parser).
// These tests target error paths and edge cases.
//
//===----------------------------------------------------------------------===//

#include "hew/msgpack_reader.h"
#include "test_utils.h"

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

// Helper: pack a map with schema_version = 10 and the given extra fields
static std::vector<uint8_t>
packWithSchema(std::function<void(msgpack::packer<msgpack::sbuffer> &)> extraFields,
               int extraCount) {
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  // Required top-level fields: schema_version, items, expr_types,
  // method_call_receiver_kinds, call_type_args, actor_send_aliasing,
  // assign_target_kinds, assign_target_shapes, lowering_facts,
  // handle_types, handle_bearing_structs, handle_type_repr (12 fields as of schema version 10)
  pk.pack_map(12 + extraCount);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0); // empty array
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
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
  pk.pack_map(2);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(999));
  EXPECT_REJECTS(hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_schema_version_u32_overflow_rejects() {
  TEST(schema_version_u32_overflow_rejects);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(UINT64_C(4294967296) + 9));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);
  EXPECT_REJECTS(hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_items_wrong_type_rejects() {
  TEST(items_wrong_type_rejects);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(3);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
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
    if (prog.schema_version != 10) {
      FAIL("schema_version should be 10");
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
  pk.pack_map(13); // 12 required + drop_funcs
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
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

static void test_handle_bearing_structs_roundtrip() {
  TEST(handle_bearing_structs_roundtrip);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
  pk.pack_array(2);
  pk.pack(std::string("PatternWrapper"));
  pk.pack(std::string("regexwrap.Outer"));
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);
  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.handle_bearing_structs.size() != 2 ||
        prog.handle_bearing_structs[0] != "PatternWrapper" ||
        prog.handle_bearing_structs[1] != "regexwrap.Outer") {
      FAIL("handle_bearing_structs not parsed correctly");
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

static void test_program_metadata_roundtrip() {
  TEST(program_metadata_roundtrip);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(14); // 12 required + source_path + line_map
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(2);
  pk.pack(std::string("fd.Socket"));
  pk.pack(std::string("http.Server"));
  pk.pack(std::string("handle_bearing_structs"));
  pk.pack_array(2);
  pk.pack(std::string("PatternWrapper"));
  pk.pack(std::string("Outer"));
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(2);
  pk.pack(std::string("fd.Socket"));
  pk.pack(std::string("i32"));
  pk.pack(std::string("http.Server"));
  pk.pack(std::string("handle"));
  pk.pack(std::string("source_path"));
  pk.pack(std::string("/workspace/examples/service.hew"));
  pk.pack(std::string("line_map"));
  pk.pack_array(3);
  pk.pack(static_cast<uint64_t>(0));
  pk.pack(static_cast<uint64_t>(17));
  pk.pack(static_cast<uint64_t>(42));

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.handle_types.size() != 2 || prog.handle_types[0] != "fd.Socket" ||
        prog.handle_types[1] != "http.Server") {
      FAIL("handle_types not parsed correctly");
      return;
    }
    if (prog.handle_bearing_structs.size() != 2 ||
        prog.handle_bearing_structs[0] != "PatternWrapper" ||
        prog.handle_bearing_structs[1] != "Outer") {
      FAIL("handle_bearing_structs not parsed correctly");
      return;
    }
    auto fd = prog.handle_type_repr.find("fd.Socket");
    if (fd == prog.handle_type_repr.end() || fd->second != "i32") {
      FAIL("fd.Socket handle_type_repr not parsed correctly");
      return;
    }
    auto http = prog.handle_type_repr.find("http.Server");
    if (http == prog.handle_type_repr.end() || http->second != "handle") {
      FAIL("http.Server handle_type_repr not parsed correctly");
      return;
    }
    if (prog.source_path != "/workspace/examples/service.hew") {
      FAIL("source_path not parsed correctly");
      return;
    }
    if (prog.line_map.size() != 3 || prog.line_map[0] != 0 || prog.line_map[1] != 17 ||
        prog.line_map[2] != 42) {
      FAIL("line_map not parsed correctly");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
  PASS();
}

static void test_method_call_receiver_kinds_roundtrip() {
  TEST(method_call_receiver_kinds_roundtrip);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(5);

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

  pk.pack_map(4);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(50));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(60));
  pk.pack(std::string("kind"));
  pk.pack(std::string("handle_instance"));
  pk.pack(std::string("type_name"));
  pk.pack(std::string("net.Connection"));

  pk.pack_map(5);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(65));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(69));
  pk.pack(std::string("kind"));
  pk.pack(std::string("primitive_trait_impl"));
  pk.pack(std::string("trait_name"));
  pk.pack(std::string("Display"));
  pk.pack(std::string("canonical_receiver"));
  pk.pack(std::string("i64"));

  pk.pack_map(4);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(70));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(80));
  pk.pack(std::string("kind"));
  pk.pack(std::string("stream_instance"));
  pk.pack(std::string("element_kind"));
  pk.pack(std::string("bytes"));

  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);
  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());

  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.method_call_receiver_kinds.size() != 5) {
      FAIL("expected five method_call_receiver_kinds entries");
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
    auto *handle = std::get_if<hew::ast::MethodCallReceiverKindHandleInstance>(
        &prog.method_call_receiver_kinds[2].kind);
    if (!handle || handle->type_name != "net.Connection") {
      FAIL("handle receiver kind not parsed correctly");
      return;
    }
    auto *primitive = std::get_if<hew::ast::MethodCallReceiverKindPrimitiveTraitImpl>(
        &prog.method_call_receiver_kinds[3].kind);
    if (!primitive || primitive->trait_name != "Display" ||
        primitive->canonical_receiver != "i64") {
      FAIL("primitive-trait receiver kind not parsed correctly");
      return;
    }
    auto *stream = std::get_if<hew::ast::MethodCallReceiverKindStreamInstance>(
        &prog.method_call_receiver_kinds[4].kind);
    if (!stream || stream->element_kind != "bytes") {
      FAIL("stream receiver kind not parsed correctly");
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
  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
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
  pk.pack(std::string("handle_bearing_structs"));
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

// ─── actor_send_aliasing parsing ────────────────────────────────────────────

/// Pack a non-uniform `actor_send_aliasing` array containing both `Copy`
/// (with each of the four `reason` values) and `Alias` entries, then
/// parse it through `parseMsgpackAST` and assert the variant + reason on
/// every entry round-trips.
///
/// Mirrors the `method_call_receiver_kinds` / `lowering_facts` /
/// `assign_target_kinds` non-empty roundtrip tests; closes the gap
/// where the actor-send aliasing wire-format had only an empty-array
/// fixture.
static void test_actor_send_aliasing_roundtrip() {
  TEST(actor_send_aliasing_roundtrip);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  // 12 top-level fields as of schema version 10 — must include
  // `call_type_args`, which the schema requires and the reader rejects
  // when absent (see packWithSchema above).
  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);

  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(5);

  // Entry 0: Copy / not_identifier.
  pk.pack_map(4);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(11));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(15));
  pk.pack(std::string("kind"));
  pk.pack(std::string("copy"));
  pk.pack(std::string("reason"));
  pk.pack(std::string("not_identifier"));

  // Entry 1: Copy / copy_type.
  pk.pack_map(4);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(20));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(24));
  pk.pack(std::string("kind"));
  pk.pack(std::string("copy"));
  pk.pack(std::string("reason"));
  pk.pack(std::string("copy_type"));

  // Entry 2: Copy / stdlib_drop.
  pk.pack_map(4);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(30));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(34));
  pk.pack(std::string("kind"));
  pk.pack(std::string("copy"));
  pk.pack(std::string("reason"));
  pk.pack(std::string("stdlib_drop"));

  // Entry 3: Copy / user_drop.
  pk.pack_map(4);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(40));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(44));
  pk.pack(std::string("kind"));
  pk.pack(std::string("copy"));
  pk.pack(std::string("reason"));
  pk.pack(std::string("user_drop"));

  // Entry 4: Alias.
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(50));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(54));
  pk.pack(std::string("kind"));
  pk.pack(std::string("alias"));

  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.actor_send_aliasing.size() != 5) {
      FAIL("expected five actor_send_aliasing entries");
      return;
    }
    const auto &e0 = prog.actor_send_aliasing[0];
    if (e0.kind != hew::ast::ActorSendAliasingKind::Copy ||
        e0.copy_reason != hew::ast::ActorSendCopyReason::NotIdentifier || e0.start != 11 ||
        e0.end != 15) {
      FAIL("entry 0: Copy/NotIdentifier did not round-trip");
      return;
    }
    const auto &e1 = prog.actor_send_aliasing[1];
    if (e1.kind != hew::ast::ActorSendAliasingKind::Copy ||
        e1.copy_reason != hew::ast::ActorSendCopyReason::CopyType || e1.start != 20 ||
        e1.end != 24) {
      FAIL("entry 1: Copy/CopyType did not round-trip");
      return;
    }
    const auto &e2 = prog.actor_send_aliasing[2];
    if (e2.kind != hew::ast::ActorSendAliasingKind::Copy ||
        e2.copy_reason != hew::ast::ActorSendCopyReason::StdlibDrop || e2.start != 30 ||
        e2.end != 34) {
      FAIL("entry 2: Copy/StdlibDrop did not round-trip");
      return;
    }
    const auto &e3 = prog.actor_send_aliasing[3];
    if (e3.kind != hew::ast::ActorSendAliasingKind::Copy ||
        e3.copy_reason != hew::ast::ActorSendCopyReason::UserDrop || e3.start != 40 ||
        e3.end != 44) {
      FAIL("entry 3: Copy/UserDrop did not round-trip");
      return;
    }
    const auto &e4 = prog.actor_send_aliasing[4];
    if (e4.kind != hew::ast::ActorSendAliasingKind::Alias || e4.start != 50 || e4.end != 54) {
      FAIL("entry 4: Alias did not round-trip");
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
  // 12 required fields (schema v10); assign_target_shapes carries the populated entries.
  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
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
  pk.pack(std::string("handle_bearing_structs"));
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
  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
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
  pk.pack(std::string("handle_bearing_structs"));
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
  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
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
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
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

  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
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
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
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

  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));

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
  pk.pack(std::string("call_type_args"));
  pk.pack_array(0);
  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());

  EXPECT_REJECTS(hew::parseMsgpackAST(data.data(), data.size()));
  PASS();
}

// ─── call_type_args parsing ──────────────────────────────────────────────────

static void test_call_type_args_roundtrip() {
  TEST(call_type_args_roundtrip);

  // Pack a v9 payload with two call_type_args entries:
  //   entry 0: call site [10..25], one type arg "Bool"
  //   entry 1: call site [40..60], two type args "Int" and "String"
  // Verifies that:
  //   - prog.call_type_args is populated correctly
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);

  pk.pack_map(12);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
  pk.pack_array(0);
  pk.pack(std::string("method_call_receiver_kinds"));
  pk.pack_array(0);

  // call_type_args: two entries
  pk.pack(std::string("call_type_args"));
  pk.pack_array(2);

  // Entry 0: [10..25], one type arg "Bool"
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(25));
  pk.pack(std::string("type_args"));
  pk.pack_array(1);
  // Spanned<TypeExpr> = [TypeExpr_value, span_map]
  pk.pack_array(2);
  // TypeExpr::Named externally tagged: {"Named": {"name": "Bool", "type_args": nil}}
  pk.pack_map(1);
  pk.pack(std::string("Named"));
  pk.pack_map(2);
  pk.pack(std::string("name"));
  pk.pack(std::string("Bool"));
  pk.pack(std::string("type_args"));
  pk.pack_nil();
  // span for type arg
  pk.pack_map(2);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(10));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(14));

  // Entry 1: [40..60], two type args "Int" and "String"
  pk.pack_map(3);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(40));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(60));
  pk.pack(std::string("type_args"));
  pk.pack_array(2);
  // type arg 0: "Int"
  pk.pack_array(2);
  pk.pack_map(1);
  pk.pack(std::string("Named"));
  pk.pack_map(2);
  pk.pack(std::string("name"));
  pk.pack(std::string("Int"));
  pk.pack(std::string("type_args"));
  pk.pack_nil();
  pk.pack_map(2);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(40));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(43));
  // type arg 1: "String"
  pk.pack_array(2);
  pk.pack_map(1);
  pk.pack(std::string("Named"));
  pk.pack_map(2);
  pk.pack(std::string("name"));
  pk.pack(std::string("String"));
  pk.pack(std::string("type_args"));
  pk.pack_nil();
  pk.pack_map(2);
  pk.pack(std::string("start"));
  pk.pack(static_cast<uint64_t>(45));
  pk.pack(std::string("end"));
  pk.pack(static_cast<uint64_t>(51));

  pk.pack(std::string("actor_send_aliasing"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_kinds"));
  pk.pack_array(0);
  pk.pack(std::string("assign_target_shapes"));
  pk.pack_array(0);
  pk.pack(std::string("lowering_facts"));
  pk.pack_array(0);
  pk.pack(std::string("handle_types"));
  pk.pack_array(0);
  pk.pack(std::string("handle_bearing_structs"));
  pk.pack_array(0);
  pk.pack(std::string("handle_type_repr"));
  pk.pack_map(0);

  auto data = std::vector<uint8_t>(reinterpret_cast<const uint8_t *>(buf.data()),
                                   reinterpret_cast<const uint8_t *>(buf.data()) + buf.size());
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.call_type_args.size() != 2) {
      FAIL("expected 2 call_type_args entries");
      return;
    }
    // Entry 0: span [10..25], one type arg "Bool"
    const auto &e0 = prog.call_type_args[0];
    if (e0.start != 10 || e0.end != 25) {
      FAIL("entry 0 span wrong");
      return;
    }
    if (e0.type_args.size() != 1) {
      FAIL("entry 0 should have 1 type arg");
      return;
    }
    auto *n0 = std::get_if<hew::ast::TypeNamed>(&e0.type_args[0].value.kind);
    if (!n0 || n0->name != "Bool") {
      FAIL("entry 0 type arg should be Named('Bool')");
      return;
    }
    // Entry 1: span [40..60], two type args "Int" and "String"
    const auto &e1 = prog.call_type_args[1];
    if (e1.start != 40 || e1.end != 60) {
      FAIL("entry 1 span wrong");
      return;
    }
    if (e1.type_args.size() != 2) {
      FAIL("entry 1 should have 2 type args");
      return;
    }
    auto *n1a = std::get_if<hew::ast::TypeNamed>(&e1.type_args[0].value.kind);
    if (!n1a || n1a->name != "Int") {
      FAIL("entry 1 type arg 0 should be Named('Int')");
      return;
    }
    auto *n1b = std::get_if<hew::ast::TypeNamed>(&e1.type_args[1].value.kind);
    if (!n1b || n1b->name != "String") {
      FAIL("entry 1 type arg 1 should be Named('String')");
      return;
    }
  } catch (const std::exception &e) {
    printf("FAILED: exception: %s\n", e.what());
    ++tests_run;
    return;
  }
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
  test_schema_version_u32_overflow_rejects();
  test_items_wrong_type_rejects();
  test_minimal_valid_program_parses();

  // drop_funcs field
  test_program_metadata_roundtrip();
  test_method_call_receiver_kinds_roundtrip();
  test_lowering_facts_roundtrip();
  test_actor_send_aliasing_roundtrip();
  test_drop_funcs_roundtrip();
  test_handle_bearing_structs_roundtrip();
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

  // call_type_args: non-empty roundtrip with typed type-arg entries
  test_call_type_args_roundtrip();

  printf("\n%d/%d tests passed\n", tests_passed, tests_run);
  return tests_passed == tests_run ? 0 : 1;
}
