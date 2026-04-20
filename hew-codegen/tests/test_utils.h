//===- test_utils.h - Shared test helpers for hew-codegen tests -----------===//
//
// Shared utility functions used across multiple codegen test binaries.
// All helpers are inline to avoid ODR violations when included in
// multiple translation units.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "hew/mlir/HewDialect.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/MLIRContext.h"

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <functional>
#include <string>
#include <vector>

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#include <process.h>
#define getpid _getpid
#else
#include <unistd.h>
#endif

#ifndef TEST
#define TEST(name)                                                                                 \
  do {                                                                                             \
    tests_run++;                                                                                   \
    printf("  test %s ... ", #name);                                                               \
  } while (0)
#endif

#ifndef PASS
#define PASS()                                                                                     \
  do {                                                                                             \
    tests_passed++;                                                                                \
    printf("ok\n");                                                                                \
  } while (0)
#endif

#ifndef FAIL
#define FAIL(msg)                                                                                  \
  do {                                                                                             \
    printf("FAILED: %s\n", msg);                                                                   \
  } while (0)
#endif

inline void initContext(mlir::MLIRContext &ctx) {
  ctx.disableMultithreading();
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::arith::ArithDialect>();
  ctx.loadDialect<mlir::scf::SCFDialect>();
  ctx.loadDialect<mlir::memref::MemRefDialect>();
  ctx.loadDialect<mlir::cf::ControlFlowDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();
}

// ---------------------------------------------------------------------------
// findHewCli: locate the hew CLI binary for use as a test frontend.
// Prefers HEW_CLI env var (set by CMake), then searches common build dirs,
// then falls back to PATH.
// ---------------------------------------------------------------------------
inline std::string findHewCli() {
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
// hewToMsgpack: compile Hew source text to a msgpack AST byte vector via
// the hew CLI. Returns an empty vector on failure.
// ---------------------------------------------------------------------------
inline std::vector<uint8_t> hewToMsgpack(const std::string &source) {
  std::string pid = std::to_string(getpid());
  auto tmpDir = std::filesystem::temp_directory_path();
  std::string srcPath = (tmpDir / ("hew_test_" + pid + ".hew")).string();
  std::string astPath = (tmpDir / ("hew_test_" + pid + ".msgpack")).string();

  {
    std::ofstream f(srcPath);
    if (!f)
      return {};
    f << source;
  }

  static std::string hewCli = findHewCli();
#ifdef _WIN32
  std::string cmd = "\"\"" + hewCli + "\" build \"" + srcPath + "\" -o \"" + astPath +
                    "\" --emit-msgpack 2>NUL\"";
#else
  std::string cmd = "\"" + hewCli + "\" build \"" + srcPath + "\" -o \"" + astPath +
                    "\" --emit-msgpack 2>/dev/null";
#endif
  int rc = std::system(cmd.c_str());
  std::filesystem::remove(srcPath);

  std::vector<uint8_t> data;
  if (rc == 0 && std::filesystem::exists(astPath)) {
    std::ifstream f(astPath, std::ios::binary);
    data.assign(std::istreambuf_iterator<char>(f), {});
    std::filesystem::remove(astPath);
  }
  return data;
}

// ---------------------------------------------------------------------------
// replaceMsgpackFixStr: in-place replace all fixstr occurrences of `from`
// with `to` in a msgpack byte buffer. Both strings must have the same
// non-zero length (≤ 31). Returns the number of replacements made.
// ---------------------------------------------------------------------------
inline int replaceMsgpackFixStr(std::vector<uint8_t> &data, const char *from, const char *to) {
  const auto fromLen = std::strlen(from);
  if (fromLen != std::strlen(to) || fromLen == 0 || fromLen > 31)
    return 0;

  const uint8_t tag = static_cast<uint8_t>(0xa0 | fromLen);
  int replacements = 0;
  for (size_t i = 0; i + 1 + fromLen <= data.size(); ++i) {
    if (data[i] != tag)
      continue;
    if (std::memcmp(data.data() + i + 1, from, fromLen) != 0)
      continue;
    std::memcpy(data.data() + i + 1, to, fromLen);
    ++replacements;
    i += fromLen;
  }
  return replacements;
}

// ---------------------------------------------------------------------------
// captureStderr (file-based): redirect stderr to a temp file, execute fn,
// restore stderr, and return the captured output as a string.
//
// NOTE: test_translate.cpp uses a pipe-based captureStderr with different
// semantics and is intentionally left as a local static in that file.
// ---------------------------------------------------------------------------
inline std::string captureStderr(const std::function<void()> &fn) {
  fflush(stderr);
  auto capturePath =
      std::filesystem::current_path() / ("hew_test_stderr_" + std::to_string(getpid()) + ".log");
  FILE *capture = std::fopen(capturePath.string().c_str(), "wb");
  if (!capture)
    return {};

#ifdef _WIN32
  const int stderrFd = _fileno(stderr);
  const int savedStderr = _dup(stderrFd);
  if (savedStderr < 0) {
    std::fclose(capture);
    std::filesystem::remove(capturePath);
    return {};
  }
  if (_dup2(_fileno(capture), stderrFd) < 0) {
    _close(savedStderr);
    std::fclose(capture);
    std::filesystem::remove(capturePath);
    return {};
  }
#else
  const int stderrFd = fileno(stderr);
  const int savedStderr = dup(stderrFd);
  if (savedStderr < 0) {
    std::fclose(capture);
    std::filesystem::remove(capturePath);
    return {};
  }
  if (dup2(fileno(capture), stderrFd) < 0) {
    close(savedStderr);
    std::fclose(capture);
    std::filesystem::remove(capturePath);
    return {};
  }
#endif

  fn();
  fflush(stderr);

#ifdef _WIN32
  _dup2(savedStderr, stderrFd);
  _close(savedStderr);
#else
  dup2(savedStderr, stderrFd);
  close(savedStderr);
#endif
  std::fclose(capture);

  std::ifstream captured(capturePath, std::ios::binary);
  std::string output((std::istreambuf_iterator<char>(captured)), {});
  std::filesystem::remove(capturePath);
  return output;
}
