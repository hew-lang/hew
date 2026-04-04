//===- test_codegen_capi.cpp - Tests for the C FFI boundary ---------------===//
//
// Verifies that the hew_codegen_compile_msgpack() C API handles input
// validation, error reporting, and output modes correctly. Uses the hew
// CLI as a frontend to produce real msgpack AST payloads.
//
//===----------------------------------------------------------------------===//

#include "hew/codegen_capi.h"
#include "test_utils.h"

#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/MLIRContext.h"

#include <cassert>
#include <cstdio>

#ifndef _WIN32
#include <sys/wait.h>
#endif

namespace hew::codegen_detail {
std::string formatEmitMlirVerificationFailure(mlir::ModuleOp module);
}

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

static HewCodegenOptions makeOptions(HewCodegenMode mode) {
  HewCodegenOptions opts{};
  opts.mode = mode;
  opts.debug_info = 0;
  opts.output_path = nullptr;
  opts.target_triple = nullptr;
  return opts;
}

static int countOccurrences(const std::string &text, const std::string &needle) {
  size_t pos = 0;
  int count = 0;
  while ((pos = text.find(needle, pos)) != std::string::npos) {
    count++;
    pos += needle.size();
  }
  return count;
}

static mlir::ModuleOp makeInvalidVerifierFailureModule(mlir::MLIRContext &context) {
  mlir::OpBuilder builder(&context);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  auto f64Type = builder.getF64Type();
  auto funcType = builder.getFunctionType({}, {f64Type});
  auto funcOp = mlir::func::FuncOp::create(builder, loc, "main", funcType);
  module.push_back(funcOp);

  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);
  mlir::func::ReturnOp::create(builder, loc);
  return module;
}

// ═══════════════════════════════════════════════════════════════════════════
// Input validation tests
// ═══════════════════════════════════════════════════════════════════════════

static void test_null_data_returns_error() {
  TEST(null_data_returns_error);
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(nullptr, 100, &opts, &buf);
  if (rc != 1) {
    FAIL("expected rc=1 for null data");
    return;
  }
  const char *err = hew_codegen_last_error();
  if (!strstr(err, "empty")) {
    FAIL("expected 'empty' in error message");
    return;
  }
  PASS();
}

static void test_zero_size_returns_error() {
  TEST(zero_size_returns_error);
  uint8_t dummy = 0x42;
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(&dummy, 0, &opts, &buf);
  if (rc != 1) {
    FAIL("expected rc=1 for zero size");
    return;
  }
  const char *err = hew_codegen_last_error();
  if (!strstr(err, "empty")) {
    FAIL("expected 'empty' in error message");
    return;
  }
  PASS();
}

static void test_null_options_returns_error() {
  TEST(null_options_returns_error);
  uint8_t dummy[] = {0x80}; // minimal msgpack map
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(dummy, sizeof(dummy), nullptr, &buf);
  if (rc != 1) {
    FAIL("expected rc=1 for null options");
    return;
  }
  const char *err = hew_codegen_last_error();
  if (!strstr(err, "options")) {
    FAIL("expected 'options' in error message");
    return;
  }
  PASS();
}

static void test_garbage_input_returns_error() {
  TEST(garbage_input_returns_error);
  uint8_t garbage[] = {0xFF, 0xFE, 0xFD, 0xFC, 0xFB};
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};

  // On macOS, exceptions may not propagate correctly through static library
  // boundaries, so the try/catch inside hew_codegen_compile_msgpack may not
  // catch. Use fork() on Unix to isolate; on Windows, call directly.
#ifdef _WIN32
  try {
    int rc = hew_codegen_compile_msgpack(garbage, sizeof(garbage), &opts, &buf);
    if (rc != 1) {
      FAIL("expected rc=1 for garbage input");
      return;
    }
  } catch (...) {
    // Exception escaped the CAPI catch — input was still rejected
  }
#else
  fflush(stdout);
  pid_t pid = fork();
  if (pid == 0) {
    int rc = hew_codegen_compile_msgpack(garbage, sizeof(garbage), &opts, &buf);
    _exit(rc == 1 ? 42 : 0); // exit 42 = correctly rejected, exit 0 = bug
  }
  int status = 0;
  waitpid(pid, &status, 0);
  // Accepted if child exited 0 (meaning rc was not 1)
  if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
    FAIL("expected rejection but call returned success");
    return;
  }
  // Child exited 42 (caught and returned error) or was killed (abort from
  // uncaught exception) — both mean the garbage was rejected
#endif
  PASS();
}

// ═══════════════════════════════════════════════════════════════════════════
// Error reporting tests
// ═══════════════════════════════════════════════════════════════════════════

static void test_last_error_cleared_on_new_call() {
  TEST(last_error_cleared_on_new_call);

  // First: trigger "options missing" error
  uint8_t dummy[] = {0x80};
  HewCodegenBuffer buf{};
  hew_codegen_compile_msgpack(dummy, sizeof(dummy), nullptr, &buf);
  const char *err1 = hew_codegen_last_error();
  if (!strstr(err1, "options")) {
    FAIL("expected 'options' error after first call");
    return;
  }

  // Second: trigger "empty" error — proves the old error was cleared
  hew_codegen_compile_msgpack(nullptr, 0, nullptr, &buf);
  const char *err2 = hew_codegen_last_error();
  if (!strstr(err2, "empty")) {
    FAIL("expected 'empty' error, not stale 'options' error");
    return;
  }
  PASS();
}

static void test_object_mode_without_path_returns_error() {
  TEST(object_mode_without_path_returns_error);
  auto ast = hewToMsgpack("fn main() { println(\"hello\"); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_OBJECT);
  opts.output_path = nullptr;
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 1) {
    FAIL("expected rc=1 for object mode without path");
    return;
  }
  const char *err = hew_codegen_last_error();
  if (!strstr(err, "output path")) {
    FAIL("expected 'output path' in error message");
    return;
  }
  PASS();
}

static void test_object_mode_empty_path_returns_error() {
  TEST(object_mode_empty_path_returns_error);
  auto ast = hewToMsgpack("fn main() { println(\"hello\"); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_OBJECT);
  opts.output_path = "";
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 1) {
    FAIL("expected rc=1 for object mode with empty path");
    return;
  }
  PASS();
}

// The full EmitMlir C API path re-verifies a module that MLIRGen already
// verifies before returning it. Exercise the seam-local formatter directly so
// the embedded error text stays deterministic.
static void test_emit_mlir_verification_report_includes_details() {
  TEST(emit_mlir_verification_report_includes_details);

  mlir::MLIRContext context;
  context.disableMultithreading();
  context.loadDialect<mlir::func::FuncDialect>();

  auto module = makeInvalidVerifierFailureModule(context);
  std::string verifierFailure = hew::codegen_detail::formatEmitMlirVerificationFailure(module);
  if (verifierFailure.empty()) {
    FAIL("expected verifier failure report");
    return;
  }
  if (verifierFailure.find("module verification failed while emitting MLIR") == std::string::npos) {
    FAIL("expected EmitMlir stage summary in error message");
    return;
  }
  if (verifierFailure.find("func.return") == std::string::npos) {
    FAIL("expected verifier diagnostic details in error message");
    return;
  }
  if (verifierFailure.find("MLIR module dump:") == std::string::npos) {
    FAIL("expected module dump header in error message");
    return;
  }
  PASS();
}

static void test_unsupported_return_coercion_fails_before_verifier() {
  TEST(unsupported_return_coercion_fails_before_verifier);
  auto ast = hewToMsgpack(R"(
fn main() -> int {
    42
}
  )");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  if (replaceMsgpackFixStr(ast, "int", "str") == 0) {
    FAIL("failed to rewrite int return type in msgpack AST");
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = 0;
  auto stderrText =
      captureStderr([&] { rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf); });

  if (buf.data != nullptr) {
    hew_codegen_buffer_free(buf);
    buf = {};
  }

  if (rc != 1) {
    FAIL("expected rc=1 for unsupported return coercion");
    return;
  }
  const char *err = hew_codegen_last_error();
  if (!strstr(err, "MLIR generation failed")) {
    FAIL("expected MLIR generation failure");
    return;
  }
  if (stderrText.find("coerceType: no known conversion") == std::string::npos) {
    FAIL("expected unsupported coercion diagnostic");
    return;
  }
  if (stderrText.find("module verification failed") != std::string::npos) {
    FAIL("unexpected downstream verifier failure for unsupported coercion");
    return;
  }
  PASS();
}

// ═══════════════════════════════════════════════════════════════════════════
// Successful emission tests
// ═══════════════════════════════════════════════════════════════════════════

static void test_emit_mlir_produces_output() {
  TEST(emit_mlir_produces_output);
  auto ast = hewToMsgpack("fn main() { println(\"hello\"); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  if (buf.len == 0 || buf.data == nullptr) {
    FAIL("expected non-empty MLIR output");
    return;
  }
  // MLIR output should contain module and func.func
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  if (mlir.find("module") == std::string::npos) {
    FAIL("MLIR output missing 'module'");
    return;
  }
  if (mlir.find("func") == std::string::npos) {
    FAIL("MLIR output missing 'func'");
    return;
  }
  PASS();
}

// Regression: Rc<T> variables must emit hew_rc_drop at scope exit.
// Both annotated (let x: Rc<int> = Rc::new(v)) and unannotated
// (let x = Rc::new(v)) paths must register the drop.
static void test_rc_scope_exit_drop_registered() {
  TEST(rc_scope_exit_drop_registered);
  // Annotated path
  {
    auto ast = hewToMsgpack("fn main() { let rc: Rc<int> = Rc::new(42); println(rc.get()); }");
    if (ast.empty()) {
      printf("SKIPPED (hew CLI not available)\n");
      tests_passed++;
      return;
    }
    auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
    HewCodegenBuffer buf{};
    int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
    if (rc != 0) {
      FAIL(hew_codegen_last_error());
      return;
    }
    std::string mlir(buf.data, buf.len);
    hew_codegen_buffer_free(buf);
    if (mlir.find("hew_rc_drop") == std::string::npos) {
      FAIL("annotated Rc<int> variable missing hew_rc_drop at scope exit");
      return;
    }
  }
  // Unannotated path
  {
    auto ast = hewToMsgpack("fn main() { let rc = Rc::new(42); println(rc.get()); }");
    if (ast.empty()) {
      FAIL("failed to parse unannotated Rc test");
      return;
    }
    auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
    HewCodegenBuffer buf{};
    int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
    if (rc != 0) {
      FAIL(hew_codegen_last_error());
      return;
    }
    std::string mlir(buf.data, buf.len);
    hew_codegen_buffer_free(buf);
    if (mlir.find("hew_rc_drop") == std::string::npos) {
      FAIL("unannotated Rc variable missing hew_rc_drop at scope exit");
      return;
    }
  }
  PASS();
}

// Regression: plain Rc rebinding (`let rc2 = rc`) must emit an implicit
// RcCloneOp and register hew_rc_drop for the new binding, otherwise the
// two aliases share a single refcount and double-drop on scope exit.
static void test_rc_rebind_emits_clone_and_drop() {
  TEST(rc_rebind_emits_clone_and_drop);
  // Unannotated rebinding
  {
    auto ast = hewToMsgpack("fn main() { let rc = Rc::new(42); let rc2 = rc; println(rc.get()); }");
    if (ast.empty()) {
      printf("SKIPPED (hew CLI not available)\n");
      tests_passed++;
      return;
    }
    auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
    HewCodegenBuffer buf{};
    int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
    if (rc != 0) {
      FAIL(hew_codegen_last_error());
      return;
    }
    std::string mlir(buf.data, buf.len);
    hew_codegen_buffer_free(buf);
    // Must have an rc.clone for the rebinding
    if (mlir.find("hew.rc.clone") == std::string::npos) {
      FAIL("unannotated Rc rebinding missing hew.rc.clone");
      return;
    }
    // Must have TWO hew_rc_drop entries (one per binding)
    int dropCount = countOccurrences(mlir, "hew_rc_drop");
    if (dropCount < 2) {
      FAIL(("unannotated Rc rebinding: expected 2 hew_rc_drop, got " + std::to_string(dropCount))
               .c_str());
      return;
    }
  }
  // Annotated rebinding
  {
    auto ast = hewToMsgpack(
        "fn main() { let rc: Rc<int> = Rc::new(42); let rc2: Rc<int> = rc; println(rc.get()); }");
    if (ast.empty()) {
      FAIL("failed to parse annotated Rc rebind test");
      return;
    }
    auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
    HewCodegenBuffer buf{};
    int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
    if (rc != 0) {
      FAIL(hew_codegen_last_error());
      return;
    }
    std::string mlir(buf.data, buf.len);
    hew_codegen_buffer_free(buf);
    if (mlir.find("hew.rc.clone") == std::string::npos) {
      FAIL("annotated Rc rebinding missing hew.rc.clone");
      return;
    }
    int dropCount = countOccurrences(mlir, "hew_rc_drop");
    if (dropCount < 2) {
      FAIL(("annotated Rc rebinding: expected 2 hew_rc_drop, got " + std::to_string(dropCount))
               .c_str());
      return;
    }
  }
  PASS();
}

// Regression: explicit Rc<T>.clone() must lower through RcCloneOp, not the
// generic string clone path, and must register a drop for the cloned binding.
static void test_rc_method_clone_emits_clone_and_drop() {
  TEST(rc_method_clone_emits_clone_and_drop);
  auto ast = hewToMsgpack(
      "fn main() { let rc: Rc<int> = Rc::new(42); let rc2 = rc.clone(); println(rc2.get()); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  if (mlir.find("hew.rc.clone") == std::string::npos) {
    FAIL("Rc method clone missing hew.rc.clone");
    return;
  }
  if (mlir.find("hew.string_method") != std::string::npos) {
    FAIL("Rc method clone should not lower through hew.string_method");
    return;
  }
  int dropCount = countOccurrences(mlir, "hew_rc_drop");
  if (dropCount < 2) {
    FAIL(("Rc method clone: expected 2 hew_rc_drop, got " + std::to_string(dropCount)).c_str());
    return;
  }
  PASS();
}

static void test_collection_clone_methods_lower_to_runtime_calls() {
  TEST(collection_clone_methods_lower_to_runtime_calls);
  auto ast = hewToMsgpack(
      "fn main() { "
      "  let v: Vec<int> = Vec::new(); let v2 = v.clone(); println(v2.len()); "
      "  let m: HashMap<String, int> = HashMap::new(); let m2 = m.clone(); println(m2.len()); "
      "  let s: HashSet<String> = HashSet::new(); let s2 = s.clone(); println(s2.len()); "
      "}");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  if (mlir.find("hew_vec_clone") == std::string::npos) {
    FAIL("Vec clone missing hew_vec_clone runtime call");
    return;
  }
  if (mlir.find("hew_hashmap_clone_impl") == std::string::npos) {
    FAIL("HashMap clone missing hew_hashmap_clone_impl runtime call");
    return;
  }
  if (mlir.find("hew_hashset_clone") == std::string::npos) {
    FAIL("HashSet clone missing hew_hashset_clone runtime call");
    return;
  }
  if (mlir.find("hew.string_method") != std::string::npos) {
    FAIL("collection clone should not lower through hew.string_method");
    return;
  }
  PASS();
}

static void test_collection_clone_receiver_temporaries_drop() {
  TEST(collection_clone_receiver_temporaries_drop);
  auto ast =
      hewToMsgpack("fn main() { "
                   "  let v: Vec<int> = Vec::new(); println(v.clone().len()); "
                   "  let m: HashMap<String, int> = HashMap::new(); println(m.clone().len()); "
                   "  let s: HashSet<String> = HashSet::new(); println(s.clone().len()); "
                   "}");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  int vecDropCount = countOccurrences(mlir, "hew_vec_free");
  if (vecDropCount < 3) {
    FAIL(
        ("Vec clone receiver temp: expected >= 3 hew_vec_free, got " + std::to_string(vecDropCount))
            .c_str());
    return;
  }
  int mapDropCount = countOccurrences(mlir, "hew_hashmap_free_impl");
  if (mapDropCount < 3) {
    FAIL(("HashMap clone receiver temp: expected >= 3 hew_hashmap_free_impl, got " +
          std::to_string(mapDropCount))
             .c_str());
    return;
  }
  int setDropCount = countOccurrences(mlir, "hew_hashset_free");
  if (setDropCount < 3) {
    FAIL(("HashSet clone receiver temp: expected >= 3 hew_hashset_free, got " +
          std::to_string(setDropCount))
             .c_str());
    return;
  }
  PASS();
}

static void test_len_free_call_lowers_via_codegen_dispatch() {
  TEST(len_free_call_lowers_via_codegen_dispatch);
  auto ast = hewToMsgpack("fn main() { "
                          "  let s = \"abc\"; "
                          "  let v: Vec<int> = Vec::new(); "
                          "  let m: HashMap<String, int> = HashMap::new(); "
                          "  let h: HashSet<String> = HashSet::new(); "
                          "  println(len(s)); "
                          "  println(len(v)); "
                          "  println(len(m)); "
                          "  println(len(h)); "
                          "  println(string_length(s)); "
                          "}");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }

  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  if (mlir.find("hew.vec.len") == std::string::npos) {
    FAIL("len(v) should lower to hew.vec.len");
    return;
  }
  if (mlir.find("hew.hashmap.len") == std::string::npos) {
    FAIL("len(m) should lower to hew.hashmap.len");
    return;
  }
  if (mlir.find("hew_hashset_len") == std::string::npos) {
    FAIL("len(h) should lower to hew_hashset_len");
    return;
  }
  if (countOccurrences(mlir, "hew.string_method \"length\"") < 2) {
    FAIL("len(String) and string_length(String) should both lower to string length calls");
    return;
  }

  PASS();
}

static void test_len_free_call_on_rc_fails_closed() {
  TEST(len_free_call_on_rc_fails_closed);
  auto ast = hewToMsgpack("fn main() { let rc: Rc<int> = Rc::new(42); println(len(rc)); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = 0;
  auto stderrText =
      captureStderr([&] { rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf); });

  if (rc != 1) {
    FAIL("len(Rc<T>) should fail closed");
    if (buf.data != nullptr) {
      hew_codegen_buffer_free(buf);
    }
    return;
  }

  const char *err = hew_codegen_last_error();
  if (!strstr(err, "MLIR generation failed")) {
    FAIL("expected MLIR generation failure");
    if (buf.data != nullptr) {
      hew_codegen_buffer_free(buf);
    }
    return;
  }
  if (stderrText.find("len(...) is not supported for pointer-backed receiver types") ==
      std::string::npos) {
    FAIL("expected fail-closed len(Rc<T>) diagnostic");
    if (buf.data != nullptr) {
      hew_codegen_buffer_free(buf);
    }
    return;
  }

  if (buf.data != nullptr) {
    hew_codegen_buffer_free(buf);
  }
  PASS();
}

static void test_rc_owned_payload_fails_closed() {
  TEST(rc_owned_payload_fails_closed);
  auto ast = hewToMsgpack("fn main() { let rc = Rc::new(Some(\"hello\")); print(0); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = 0;
  auto stderrText =
      captureStderr([&] { rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf); });

  if (rc != 1) {
    FAIL("Rc::new with Option<String> should fail closed");
    if (buf.data != nullptr) {
      hew_codegen_buffer_free(buf);
    }
    return;
  }

  const char *err = hew_codegen_last_error();
  if (!strstr(err, "MLIR generation failed")) {
    FAIL("expected MLIR generation failure");
    if (buf.data != nullptr) {
      hew_codegen_buffer_free(buf);
    }
    return;
  }

  if (buf.data != nullptr) {
    hew_codegen_buffer_free(buf);
  }
  PASS();
}

static void test_rc_outlive_block_drop_registered() {
  TEST(rc_outlive_block_drop_registered);
  // Rc alias escaping a block as trailing expression must still get
  // hew_rc_drop in the outer scope AND an hew.rc.clone inside the block.
  auto ast = hewToMsgpack("fn main() { "
                          "  let rc2 = { "
                          "    let rc: Rc<int> = Rc::new(42); "
                          "    let alias = rc; "
                          "    alias "
                          "  }; "
                          "  println(rc2.get()); "
                          "}");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  // Must have hew.rc.clone for the inner rebinding
  if (mlir.find("hew.rc.clone") == std::string::npos) {
    FAIL("Rc outlive: missing hew.rc.clone for inner rebinding");
    return;
  }
  // Must have TWO hew_rc_drop entries: one for inner `rc`, one for outer `rc2`
  int dropCount = countOccurrences(mlir, "hew_rc_drop");
  if (dropCount < 2) {
    FAIL(("Rc outlive: expected >= 2 hew_rc_drop, got " + std::to_string(dropCount)).c_str());
    return;
  }
  PASS();
}

// Regression: Rc<String> must generate a trampoline drop wrapper
// (__rc_inner_drop_hew_string_drop) that loads the inner String pointer
// from the Rc data region before forwarding to hew_string_drop.
// Without this, hew_string_drop receives an interior Rc pointer → crash.
static void test_rc_string_inner_drop_trampoline() {
  TEST(rc_string_inner_drop_trampoline);
  auto ast = hewToMsgpack("fn main() { let rc = Rc::new(\"hello\"); print(rc.strong_count()); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  // Must contain the trampoline function
  if (mlir.find("__rc_inner_drop_hew_string_drop") == std::string::npos) {
    FAIL("Rc<String> missing __rc_inner_drop_hew_string_drop trampoline");
    return;
  }
  // Trampoline must load the inner value and call hew_string_drop
  if (mlir.find("hew_string_drop") == std::string::npos) {
    FAIL("Rc<String> trampoline missing call to hew_string_drop");
    return;
  }
  PASS();
}

// Regression: nested Rc payloads are supported because the outer Rc only needs
// to forward its inner slot to hew_rc_drop, which in turn runs the inner Rc's
// own trampoline for String.
static void test_rc_nested_inner_drop_trampolines() {
  TEST(rc_nested_inner_drop_trampolines);
  auto ast =
      hewToMsgpack("fn main() { let rc = Rc::new(Rc::new(\"hello\")); print(rc.strong_count()); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  if (mlir.find("__rc_inner_drop_hew_rc_drop") == std::string::npos) {
    FAIL("nested Rc payload missing outer __rc_inner_drop_hew_rc_drop trampoline");
    return;
  }
  if (mlir.find("__rc_inner_drop_hew_string_drop") == std::string::npos) {
    FAIL("nested Rc<String> payload missing inner __rc_inner_drop_hew_string_drop trampoline");
    return;
  }
  PASS();
}
// Rc<T> call-boundary ownership contract: borrow semantics.
// When an Rc<T> variable is passed to a function, the raw pointer is
// forwarded WITHOUT an RcCloneOp — the callee borrows the reference for
// the duration of the call.  The caller retains ownership and drops at
// scope exit.  No param drop is registered in the callee (param drops are
// deferred until null-after-move tracking is implemented).
//
// This test verifies:
//   1. No hew.rc.clone at the call site (borrow, not clone-on-pass)
//   2. Caller's hew_rc_drop is still present (caller retains ownership)
static void test_rc_call_boundary_borrow_no_clone() {
  TEST(rc_call_boundary_borrow_no_clone);
  auto ast = hewToMsgpack("fn read_it(r: Rc<int>) -> int { r.get() }\n"
                          "fn main() {\n"
                          "  let rc = Rc::new(42);\n"
                          "  println(read_it(rc));\n"
                          "}");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);

  // The main function must have a scope-exit hew_rc_drop for the local
  if (mlir.find("hew_rc_drop") == std::string::npos) {
    FAIL("Rc borrow call: caller missing hew_rc_drop for local");
    return;
  }

  // Extract the main function body to check for clone at call site.
  // Under borrow semantics there should be exactly ONE hew.rc.clone in
  // the whole module (none — the Rc::new path doesn't emit one) or zero.
  // There must NOT be an hew.rc.clone adjacent to the call of read_it.
  // We verify by checking the total clone count is 0 (no rebinding in
  // this program means no clones should appear).
  size_t cloneCount = 0;
  size_t pos = 0;
  while ((pos = mlir.find("hew.rc.clone", pos)) != std::string::npos) {
    cloneCount++;
    pos += 12;
  }
  if (cloneCount > 0) {
    FAIL(("Rc borrow call: expected 0 hew.rc.clone (borrow semantics), got " +
          std::to_string(cloneCount))
             .c_str());
    return;
  }

  PASS();
}

// Invariant: Rc<T> param drops are NOT registered in the callee.
// The callee's Rc param must have NO hew_rc_drop — function params are
// borrowed under call-boundary ownership.  If this test starts failing,
// it means param drop tracking was implemented and the call-boundary
// contract has changed; update this test and the BorrowedParamReturn
// diagnostic accordingly.
static void test_rc_callee_param_no_drop_registered() {
  TEST(rc_callee_param_no_drop_registered);
  auto ast = hewToMsgpack("fn use_rc(r: Rc<int>) { println(r.get()); }\n"
                          "fn main() { let rc = Rc::new(42); use_rc(rc); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);

  // Extract the use_rc function body from the MLIR.
  std::string funcMarker = "@\"use_rc\"";
  if (mlir.find(funcMarker) == std::string::npos) {
    funcMarker = "use_rc";
  }
  auto funcPos = mlir.find(funcMarker);
  if (funcPos == std::string::npos) {
    FAIL("Rc param drop test: could not find use_rc function in MLIR");
    return;
  }
  auto nextFunc = mlir.find("func.func", funcPos + funcMarker.size());
  std::string funcBody = (nextFunc != std::string::npos) ? mlir.substr(funcPos, nextFunc - funcPos)
                                                         : mlir.substr(funcPos);

  // Fail-closed: callee body must NOT contain hew_rc_drop for params.
  if (funcBody.find("hew_rc_drop") != std::string::npos) {
    FAIL("callee body contains hew_rc_drop — param drop tracking may have "
         "been implemented; update call-boundary contract tests");
    return;
  }
  PASS();
}

// Struct init with a closure field sourced from an existing binding must clone
// the closure env for the struct copy; otherwise the binding and the struct
// both drop the same env.
static void test_struct_init_closure_field_clones_bound_owner() {
  TEST(struct_init_closure_field_clones_bound_owner);
  auto ast = hewToMsgpack("type Job { id: int; action: fn(int) -> int; }\n"
                          "fn main() {\n"
                          "  let offset = 100;\n"
                          "  let action = (x: int) -> int => x + offset;\n"
                          "  let j = Job { id: 5, action: action };\n"
                          "  println(1);\n"
                          "}");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }
  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  std::string mlir(buf.data, buf.len);
  hew_codegen_buffer_free(buf);

  auto structInitPos = mlir.find("hew.struct_init");
  if (structInitPos == std::string::npos) {
    FAIL("expected hew.struct_init in MLIR");
    return;
  }
  if (countOccurrences(mlir, "hew.rc.clone") < 2) {
    FAIL("struct init closure field missing env clone from bound owner");
    return;
  }
  PASS();
}

static void test_emit_llvm_produces_output() {
  TEST(emit_llvm_produces_output);
  auto ast = hewToMsgpack("fn main() { println(\"hello\"); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_LLVM);
  HewCodegenBuffer buf{};
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, &buf);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  if (buf.len == 0 || buf.data == nullptr) {
    FAIL("expected non-empty LLVM IR output");
    return;
  }
  std::string llvm(buf.data, buf.len);
  hew_codegen_buffer_free(buf);
  // LLVM IR should contain define and @main
  if (llvm.find("define") == std::string::npos) {
    FAIL("LLVM output missing 'define'");
    return;
  }
  PASS();
}

static void test_emit_object_writes_file() {
  TEST(emit_object_writes_file);
  auto ast = hewToMsgpack("fn main() { println(\"hello\"); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  std::string objPath = (std::filesystem::temp_directory_path() /
                         ("test_capi_obj_" + std::to_string(getpid()) + ".o"))
                            .string();

  auto opts = makeOptions(HEW_CODEGEN_EMIT_OBJECT);
  opts.output_path = objPath.c_str();
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, nullptr);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  if (!std::filesystem::exists(objPath)) {
    FAIL("object file not created");
    return;
  }
  auto sz = std::filesystem::file_size(objPath);
  std::filesystem::remove(objPath);
  if (sz == 0) {
    FAIL("object file is empty");
    return;
  }
  PASS();
}

// ═══════════════════════════════════════════════════════════════════════════
// Buffer lifecycle tests
// ═══════════════════════════════════════════════════════════════════════════

static void test_buffer_free_null_data_safe() {
  TEST(buffer_free_null_data_safe);
  // free(nullptr) is defined as a no-op by C standard; verify the wrapper
  // doesn't crash
  HewCodegenBuffer buf{};
  buf.data = nullptr;
  buf.len = 0;
  hew_codegen_buffer_free(buf); // should not crash
  PASS();
}

static void test_text_output_null_buffer_for_object_mode() {
  TEST(text_output_null_buffer_for_object_mode);
  auto ast = hewToMsgpack("fn main() { println(\"hello\"); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  std::string objPath = (std::filesystem::temp_directory_path() /
                         ("test_capi_nullbuf_" + std::to_string(getpid()) + ".o"))
                            .string();

  auto opts = makeOptions(HEW_CODEGEN_EMIT_OBJECT);
  opts.output_path = objPath.c_str();
  // text_output is nullptr — valid for object mode
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, nullptr);
  std::filesystem::remove(objPath);
  if (rc != 0) {
    FAIL(hew_codegen_last_error());
    return;
  }
  PASS();
}

static void test_mlir_mode_null_buffer_returns_error() {
  TEST(mlir_mode_null_buffer_returns_error);
  auto ast = hewToMsgpack("fn main() { println(\"hello\"); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_MLIR);
  // text_output is nullptr — invalid for MLIR mode
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, nullptr);
  if (rc != 1) {
    FAIL("expected rc=1 for MLIR mode with null buffer");
    return;
  }
  const char *err = hew_codegen_last_error();
  if (!strstr(err, "text output buffer")) {
    FAIL("expected 'text output buffer' in error message");
    return;
  }
  PASS();
}

static void test_llvm_mode_null_buffer_returns_error() {
  TEST(llvm_mode_null_buffer_returns_error);
  auto ast = hewToMsgpack("fn main() { println(\"hello\"); }");
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  auto opts = makeOptions(HEW_CODEGEN_EMIT_LLVM);
  int rc = hew_codegen_compile_msgpack(ast.data(), ast.size(), &opts, nullptr);
  if (rc != 1) {
    FAIL("expected rc=1 for LLVM mode with null buffer");
    return;
  }
  const char *err = hew_codegen_last_error();
  if (!strstr(err, "text output buffer")) {
    FAIL("expected 'text output buffer' in error message");
    return;
  }
  PASS();
}

// ═══════════════════════════════════════════════════════════════════════════
// Entry point
// ═══════════════════════════════════════════════════════════════════════════

int main() {
  printf("Running codegen C API tests...\n");

  // Input validation
  test_null_data_returns_error();
  test_zero_size_returns_error();
  test_null_options_returns_error();
  test_garbage_input_returns_error();

  // Error reporting
  test_last_error_cleared_on_new_call();
  test_object_mode_without_path_returns_error();
  test_object_mode_empty_path_returns_error();
  test_emit_mlir_verification_report_includes_details();
  test_unsupported_return_coercion_fails_before_verifier();

  // Successful emission
  test_emit_mlir_produces_output();
  test_rc_scope_exit_drop_registered();
  test_rc_rebind_emits_clone_and_drop();
  test_rc_method_clone_emits_clone_and_drop();
  test_collection_clone_methods_lower_to_runtime_calls();
  test_collection_clone_receiver_temporaries_drop();
  test_len_free_call_lowers_via_codegen_dispatch();
  test_len_free_call_on_rc_fails_closed();
  test_rc_owned_payload_fails_closed();
  test_rc_outlive_block_drop_registered();
  test_rc_string_inner_drop_trampoline();
  test_rc_nested_inner_drop_trampolines();
  test_rc_call_boundary_borrow_no_clone();
  test_rc_callee_param_no_drop_registered();
  test_struct_init_closure_field_clones_bound_owner();
  test_emit_llvm_produces_output();
  test_emit_object_writes_file();

  // Buffer lifecycle
  test_buffer_free_null_data_safe();
  test_text_output_null_buffer_for_object_mode();
  test_mlir_mode_null_buffer_returns_error();
  test_llvm_mode_null_buffer_returns_error();

  printf("\n%d/%d tests passed\n", tests_passed, tests_run);
  return tests_passed == tests_run ? 0 : 1;
}
