#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

enum HewCodegenMode {
  HEW_CODEGEN_EMIT_MLIR = 0,
  HEW_CODEGEN_EMIT_LLVM = 1,
  HEW_CODEGEN_EMIT_OBJECT = 2,
};

struct HewCodegenOptions {
  uint32_t mode;
  uint8_t debug_info;
  const char *output_path;
  const char *target_triple;
};

struct HewCodegenBuffer {
  char *data;
  size_t len;
};

int hew_codegen_compile_msgpack(const uint8_t *data, size_t size,
                                const struct HewCodegenOptions *options,
                                struct HewCodegenBuffer *text_output);

void hew_codegen_buffer_free(struct HewCodegenBuffer buffer);

const char *hew_codegen_last_error(void);

// ── JIT session API ───────────────────────────────────────────────────────────
//
// Synchronous, single-module, per-cell JIT execution via LLJIT.
// Each session is created, used once, then destroyed.  See issue #1235.
//
// WHY this shape: the M1 keystone prescribes one LLJIT per REPL cell.
// WHEN obsolete: when #1228 introduces a Runtime handle with a long-lived JIT.
// WHAT the real solution looks like: a persistent session shared across cells.

/// Opaque handle for a single-cell JIT session.
typedef struct HewJitSession HewJitSession;

/// Create a JIT session.  Returns NULL on failure; call
/// hew_jit_session_last_error() to retrieve the message.
HewJitSession *hew_jit_session_create(void);

/// Compile and execute a MessagePack-encoded Hew program in-process.
///
/// @param session       Non-null session handle.
/// @param data          MessagePack-encoded AST bytes.
/// @param size          Byte count.
/// @param out_exit_code Receives the i64 return value of `main` (0 if void).
/// @return 0 on success, non-zero on failure.
int hew_jit_session_eval_msgpack(HewJitSession *session, const uint8_t *data, size_t size,
                                 int64_t *out_exit_code);

/// Destroy the session and free all associated resources.
void hew_jit_session_destroy(HewJitSession *session);

/// Thread-local error message from the most recent failed JIT operation.
const char *hew_jit_session_last_error(void);

#ifdef __cplusplus
}
#endif
