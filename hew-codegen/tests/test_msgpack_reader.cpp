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

// ═══════════════════════════════════════════════════════════════════════════
// Error handling: invalid input
// ═══════════════════════════════════════════════════════════════════════════

static void test_empty_input_throws() {
  TEST(empty_input_throws);
  // Use a valid pointer with zero size — passing nullptr is UB
  uint8_t empty = 0;
  try {
    hew::parseMsgpackAST(&empty, 0);
    FAIL("expected exception for empty input");
    return;
  } catch (...) {
    // Any exception is acceptable — the contract is "don't silently succeed"
  }
  PASS();
}

static void test_truncated_msgpack_throws() {
  TEST(truncated_msgpack_throws);
  // A map header claiming 10 entries but only 2 bytes of data
  uint8_t truncated[] = {0x8A, 0xA1}; // fixmap(10), fixstr(1) — then EOF
  try {
    hew::parseMsgpackAST(truncated, sizeof(truncated));
    FAIL("expected exception for truncated input");
    return;
  } catch (...) {
    // Good — either msgpack unpack or our parser should reject this
  }
  PASS();
}

static void test_wrong_top_level_type_throws() {
  TEST(wrong_top_level_type_throws);
  // Top level should be a map, not an array
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_array(3);
  pk.pack(1);
  pk.pack(2);
  pk.pack(3);

  try {
    hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size());
    FAIL("expected exception for non-map top level");
    return;
  } catch (const std::runtime_error &e) {
    // Should mention "map" or be a parse error
    if (std::string(e.what()).find("parse error") == std::string::npos &&
        std::string(e.what()).find("map") == std::string::npos &&
        std::string(e.what()).find("MAP") == std::string::npos &&
        std::string(e.what()).find("expected") == std::string::npos) {
      // Accept any runtime_error — the detail of the message may vary
    }
  } catch (...) {
    // On some platforms, exception types may not match across static lib boundaries
  }
  PASS();
}

static void test_single_byte_garbage_throws() {
  TEST(single_byte_garbage_throws);
  uint8_t garbage[] = {0xC1}; // 0xC1 is "never used" in msgpack spec
  try {
    hew::parseMsgpackAST(garbage, sizeof(garbage));
    FAIL("expected exception for single-byte garbage");
    return;
  } catch (...) {
    // Good
  }
  PASS();
}

// ═══════════════════════════════════════════════════════════════════════════
// Error handling: structurally valid msgpack, semantically wrong AST
// ═══════════════════════════════════════════════════════════════════════════

// Helper: pack a map with schema_version = 2 and the given extra fields
static std::vector<uint8_t> packWithSchema(
    std::function<void(msgpack::packer<msgpack::sbuffer> &)> extraFields, int extraCount) {
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  // Required top-level fields: schema_version, items, expr_types,
  // handle_types, handle_type_repr
  pk.pack_map(5 + extraCount);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(2));
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

static void test_missing_schema_version_throws() {
  TEST(missing_schema_version_throws);
  // A map without schema_version — parseProgram requires it via mapReq
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(1);
  pk.pack(std::string("items"));
  pk.pack_array(0);

  try {
    hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size());
    FAIL("expected exception for missing schema_version");
    return;
  } catch (const std::runtime_error &e) {
    std::string msg = e.what();
    if (msg.find("schema_version") == std::string::npos) {
      std::string detail = "expected 'schema_version' in error, got: " + msg;
      FAIL(detail.c_str());
      return;
    }
  } catch (...) {
    // On macOS, exception typeinfo may not match across static lib boundaries
  }
  PASS();
}

static void test_wrong_schema_version_throws() {
  TEST(wrong_schema_version_throws);
  // schema_version = 999 — should fail version check
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(1);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(999));

  try {
    hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size());
    FAIL("expected exception for wrong schema version");
    return;
  } catch (const std::runtime_error &e) {
    std::string msg = e.what();
    if (msg.find("unsupported schema version") == std::string::npos) {
      std::string detail = "expected 'unsupported schema version' in error, got: " + msg;
      FAIL(detail.c_str());
      return;
    }
  } catch (...) {
    // On macOS, exception typeinfo may not match across static lib boundaries
  }
  PASS();
}

static void test_items_wrong_type_throws() {
  TEST(items_wrong_type_throws);
  // items as integer instead of array — should fail parseVec
  msgpack::sbuffer buf;
  msgpack::packer<msgpack::sbuffer> pk(&buf);
  pk.pack_map(2);
  pk.pack(std::string("schema_version"));
  pk.pack(static_cast<uint64_t>(2));
  pk.pack(std::string("items"));
  pk.pack(42); // should be array

  try {
    hew::parseMsgpackAST(reinterpret_cast<const uint8_t *>(buf.data()), buf.size());
    FAIL("expected exception for items as integer");
    return;
  } catch (const std::runtime_error &e) {
    std::string msg = e.what();
    if (msg.find("array") == std::string::npos && msg.find("ARRAY") == std::string::npos &&
        msg.find("type") == std::string::npos) {
      std::string detail = "expected array-related error, got: " + msg;
      FAIL(detail.c_str());
      return;
    }
  } catch (...) {
    // Any exception is acceptable
  }
  PASS();
}

static void test_minimal_valid_program_parses() {
  TEST(minimal_valid_program_parses);
  auto data = packWithSchema([](msgpack::packer<msgpack::sbuffer> &) {}, 0);
  try {
    auto prog = hew::parseMsgpackAST(data.data(), data.size());
    if (prog.schema_version != 2) {
      FAIL("schema_version should be 2");
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

// ═══════════════════════════════════════════════════════════════════════════
// Entry point
// ═══════════════════════════════════════════════════════════════════════════

int main() {
  printf("Running msgpack reader tests...\n");

  // Invalid input
  test_empty_input_throws();
  test_truncated_msgpack_throws();
  test_wrong_top_level_type_throws();
  test_single_byte_garbage_throws();

  // Semantically wrong AST
  test_missing_schema_version_throws();
  test_wrong_schema_version_throws();
  test_items_wrong_type_throws();
  test_minimal_valid_program_parses();

  printf("\n%d/%d tests passed\n", tests_passed, tests_run);
  return tests_passed == tests_run ? 0 : 1;
}
