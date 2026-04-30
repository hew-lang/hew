//===- jit_session.h - Synchronous LLJIT-based JIT session ------*- C++ -*-===//
//
// Wraps a single-use `llvm::orc::LLJIT` instance for one REPL cell.
//
// Design: synchronous, whole-module, one LLJIT per cell — see issue #1235.
// Each `HewJitSession` is created, used once (eval_msgpack), then destroyed.
// No cross-cell symbol sharing; no `LLLazyJIT`; no tiered optimisation.
//
// WHY this shape: the M1 keystone needs to be the smallest surface that
// proves out the JIT warm-path.  Persistent sessions and incremental
// compilation are out of scope (issues #1227, #1232, #1228).
// WHEN obsolete: when #1228 introduces a Runtime handle API that can carry a
// persistent LLJIT session across cells.
// WHAT the real solution looks like: a long-lived `HewJitSession` that
// accumulates symbol tables across cells, potentially sharing a single
// `ThreadSafeContext`.
//
//===----------------------------------------------------------------------===//

#pragma once

#include <cstddef>
#include <cstdint>

namespace hew {

/// Opaque handle for a single-cell JIT session.
/// Created via `HewJitSession::create`, used once, then deleted.
class HewJitSession;

/// Factory method: create a JIT session for one REPL cell.
/// Returns null on failure; call `HewJitSession::lastError()` for details.
HewJitSession *hewJitSessionCreate();

/// Evaluate a single Hew program encoded as MessagePack.
///
/// @param session  Opaque session handle (must be non-null).
/// @param data     MessagePack-encoded AST bytes.
/// @param size     Byte count.
/// @param out_exit_code  Receives the i64 return value of `main` (or 0 on
///                       success when main is void), or the JIT error code.
/// @return 0 on success, non-zero on failure.
int hewJitSessionEvalMsgpack(HewJitSession *session, const uint8_t *data, size_t size,
                             int64_t *out_exit_code);

/// Destroy the session and free all associated resources.
void hewJitSessionDestroy(HewJitSession *session);

/// Thread-local error message from the most recent failed operation.
const char *hewJitSessionLastError();

/// Return the number of JITEventListeners registered during the most recent
/// evalMsgpack call.  Used by integration tests to assert that GDB and perf
/// listeners were (or were not) attached.
///
/// WHY exists: gives tests a deterministic white-box invariant without
/// forking a gdb or perf process.  Not meaningful outside an eval context.
int hewJitSessionListenerCountForTest(const HewJitSession *session);

} // namespace hew
