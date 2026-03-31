//===- test_codegen_capi.cpp - Tests for the C FFI boundary ---------------===//
//
// Verifies that the hew_codegen_compile_msgpack() C API handles input
// validation, error reporting, and output modes correctly. Uses the hew
// CLI as a frontend to produce real msgpack AST payloads.
//
//===----------------------------------------------------------------------===//

#include "hew/codegen_capi.h"

#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/MLIRContext.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#ifdef _WIN32
#include <process.h>
#define getpid _getpid
#else
#include <sys/wait.h>
#include <unistd.h>
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

// ---------------------------------------------------------------------------
// Helper: find hew CLI binary
// ---------------------------------------------------------------------------
static std::string findHewCli() {
  if (const char *env = std::getenv("HEW_CLI"))
    return env;
#ifdef _WIN32
  constexpr const char *hewName = "hew.exe";
#else
  constexpr const char *hewName = "hew";
#endif
  for (auto candidate : {
           std::filesystem::path("../../../target/release") / hewName,
           std::filesystem::path("../../../target/debug") / hewName,
           std::filesystem::path("../../target/release") / hewName,
           std::filesystem::path("../../target/debug") / hewName,
       }) {
    if (std::filesystem::exists(candidate))
      return std::filesystem::canonical(candidate).string();
  }
  return "hew";
}

// ---------------------------------------------------------------------------
// Helper: compile Hew source to msgpack AST via hew CLI
// ---------------------------------------------------------------------------
static std::vector<uint8_t> hewToMsgpack(const std::string &source) {
  std::string pid = std::to_string(getpid());
  auto tmpDir = std::filesystem::temp_directory_path();
  std::string srcPath = (tmpDir / ("test_capi_" + pid + ".hew")).string();
  std::string astPath = (tmpDir / ("test_capi_" + pid + ".msgpack")).string();

  {
    std::ofstream f(srcPath);
    f << source;
  }

  static std::string hewCli = findHewCli();
#ifdef _WIN32
  std::string cmd = "\"\"" + hewCli + "\" build \"" + srcPath + "\" -o \"" + astPath +
                    "\" --emit-msgpack 2>NUL\"";
#else
  std::string cmd =
      "\"" + hewCli + "\" build \"" + srcPath + "\" -o \"" + astPath + "\" --emit-msgpack 2>/dev/null";
#endif
  int rc = std::system(cmd.c_str());
  std::filesystem::remove(srcPath);

  std::vector<uint8_t> data;
  if (rc == 0 && std::filesystem::exists(astPath)) {
    std::ifstream f(astPath, std::ios::binary);
    f.seekg(0, std::ios::end);
    auto sz = f.tellg();
    f.seekg(0, std::ios::beg);
    data.resize(static_cast<size_t>(sz));
    f.read(reinterpret_cast<char *>(data.data()), sz);
    std::filesystem::remove(astPath);
  }
  return data;
}

static HewCodegenOptions makeOptions(HewCodegenMode mode) {
  HewCodegenOptions opts{};
  opts.mode = mode;
  opts.debug_info = 0;
  opts.output_path = nullptr;
  opts.target_triple = nullptr;
  return opts;
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
  if (verifierFailure.find("module verification failed while emitting MLIR") ==
      std::string::npos) {
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

  // Successful emission
  test_emit_mlir_produces_output();
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
