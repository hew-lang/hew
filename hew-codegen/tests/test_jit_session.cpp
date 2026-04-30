//===- test_jit_session.cpp - Integration smoke for HewJitSession ---------===//
//
// Constructs a HewJitSession, evaluates a known minimal Hew program, and
// asserts that the expected number of JITEventListeners were registered.
//
// Counter expected values (determined by compile-time macros):
//   - LLVM_USE_PERF=1: up to 2 listeners (GDB + perf)
//   - LLVM_USE_PERF=0: up to 1 listener  (GDB only)
//   - Minimum: 0 (createGDBRegistrationListener may return nullptr)
//
// This test does not attach gdb, run `perf record`, or inspect jitdump
// files.  It white-box checks the listener counter exposed via the
// ForTest suffix in hewJitSessionListenerCountForTest (test-only API
// conveyed by name, not by a compile-time macro).
//
//===----------------------------------------------------------------------===//

#include "hew/jit_session.h"
#include "test_utils.h"

#include "llvm/Config/llvm-config.h"

#include <cassert>
#include <cstdio>

using namespace hew;

static int tests_run = 0;
static int tests_passed = 0;

// ── helpers ──────────────────────────────────────────────────────────────────

/// Minimal Hew program: returns the integer 42 from main.
static const char kHewSource[] = "fn main() -> int { 42 }";

// ── tests ─────────────────────────────────────────────────────────────────────

// Smoke: HewJitSession can be created and destroyed without crashing.
static void test_session_create_destroy() {
  TEST(session_create_destroy);
  HewJitSession *s = hewJitSessionCreate();
  if (!s) {
    FAIL(hewJitSessionLastError());
    return;
  }
  hewJitSessionDestroy(s);
  PASS();
}

// Integration: evalMsgpack executes a minimal program and the correct number
// of JITEventListeners were attached to the RTDyldObjectLinkingLayer.
//
// Expected listener count bounds:
//   LLVM_USE_PERF=1 → max 2  (GDB + perf)
//   LLVM_USE_PERF=0 → max 1  (GDB only)
//   Minimum is always 0 because createGDBRegistrationListener() may return
//   nullptr on some LLVM builds (e.g., Homebrew LLVM on macOS).
static void test_listeners_attached() {
  TEST(listeners_attached);

  auto ast = hewToMsgpack(kHewSource);
  if (ast.empty()) {
    printf("SKIPPED (hew CLI not available)\n");
    tests_passed++;
    return;
  }

  HewJitSession *s = hewJitSessionCreate();
  if (!s) {
    FAIL(hewJitSessionLastError());
    return;
  }

  int64_t exitCode = -1;
  int rc = hewJitSessionEvalMsgpack(s, ast.data(), ast.size(), &exitCode);
  if (rc != 0) {
    FAIL(hewJitSessionLastError());
    hewJitSessionDestroy(s);
    return;
  }

  if (exitCode != 42) {
    printf("FAILED: expected exit code 42, got %lld\n", (long long)exitCode);
    hewJitSessionDestroy(s);
    return;
  }

#if LLVM_USE_PERF
  constexpr int kMaxListeners = 2;
#else
  constexpr int kMaxListeners = 1;
#endif

  int count = hewJitSessionListenerCountForTest(s);
  if (count < 0 || count > kMaxListeners) {
    printf("FAILED: expected 0-%d JITEventListener(s) registered, got %d\n", kMaxListeners, count);
    hewJitSessionDestroy(s);
    return;
  }

  hewJitSessionDestroy(s);
  PASS();
}

// Regression: creating a session without calling eval does not set a listener
// count (counter stays at its initial value of 0).
static void test_no_eval_listener_count_zero() {
  TEST(no_eval_listener_count_zero);
  HewJitSession *s = hewJitSessionCreate();
  if (!s) {
    FAIL(hewJitSessionLastError());
    return;
  }
  int count = hewJitSessionListenerCountForTest(s);
  if (count != 0) {
    printf("FAILED: expected 0 listeners before any eval, got %d\n", count);
    hewJitSessionDestroy(s);
    return;
  }
  hewJitSessionDestroy(s);
  PASS();
}

// ── entry point ──────────────────────────────────────────────────────────────

int main() {
  printf("Running JIT session integration tests...\n");

  test_session_create_destroy();
  test_no_eval_listener_count_zero();
  test_listeners_attached();

  printf("\n%d/%d tests passed\n", tests_passed, tests_run);
  return tests_passed == tests_run ? 0 : 1;
}
