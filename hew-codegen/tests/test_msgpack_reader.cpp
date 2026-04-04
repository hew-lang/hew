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
  EXPECT_REJECTS(
      hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
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

// Helper: pack a map with schema_version = 3 and the given extra fields
static std::vector<uint8_t> packWithSchema(
    std::function<void(msgpack::packer<msgpack::sbuffer> &)> extraFields, int extraCount) {
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  // Required top-level fields: schema_version, items, expr_types,
  // handle_types, handle_type_repr
  pk.pack_map(5 + extraCount);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(3));
  pk.pack(std::string("items"));
  pk.pack_array(0); // empty array
  pk.pack(std::string("expr_types"));
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
  EXPECT_REJECTS(
      hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_wrong_schema_version_rejects() {
  TEST(wrong_schema_version_rejects);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(1);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(999));
  EXPECT_REJECTS(
      hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_items_wrong_type_rejects() {
  TEST(items_wrong_type_rejects);
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(2);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(3));
  pk.pack(std::string("items"));
  pk.pack(42); // should be array
  EXPECT_REJECTS(
      hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size()));
  PASS();
}

static void test_minimal_valid_program_parses() {
  TEST(minimal_valid_program_parses);
  auto data = packWithSchema([](msgpack::packer<msgpack::sbuffer> &) {}, 0);
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.schema_version != 3) {
      FAIL("schema_version should be 3");
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
  pk.pack_map(6); // 5 required + drop_funcs
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(3));
  pk.pack(std::string("items"));
  pk.pack_array(0);
  pk.pack(std::string("expr_types"));
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

  auto data = std::vector<uint8_t>(
      reinterpret_cast<const uint8_t *>(buf.data()),
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

// ═══════════════════════════════════════════════════════════════════════════
// Entry point
// ═══════════════════════════════════════════════════════════════════════════

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
  test_drop_funcs_roundtrip();
  test_drop_funcs_absent_gives_empty_map();
  test_drop_funcs_wrong_type_rejects();

  printf("\n%d/%d tests passed\n", tests_passed, tests_run);
  return tests_passed == tests_run ? 0 : 1;
}
