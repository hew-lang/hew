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

#ifdef __cplusplus
}
#endif
