/* Real compiled Hew standard-library parsers through the checked host ABI.
 *
 * The continuous builder compiles stdlib_text.hew with the matching compiler
 * and ASan instrumentation, then links this harness and libhew.a exactly once.
 */
#include "hew.h"
#include "fuzz_stdlib_text.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  if (size == 0)
    return 0;
  if (size - 1 > UINT32_MAX)
    return 0;

  HewError *error = NULL;
  int64_t observed = 0;
  int32_t status = hxfuzz_input((uint64_t)(uintptr_t)(data + 1), size - 1,
                                 data[0], &observed, &error);
  if (error != NULL)
    hew_host_error_release(error);
  if (status != HEW_OK)
    abort(); /* Logical faults are findings, like sanitizer reports. */

  (void)observed;
  return 0;
}

HewBytes hew_fuzz_bytes(uint64_t address, uint64_t length) {
  if (length > UINT32_MAX)
    abort();
  return hew_bytes_copy((const uint8_t *)(uintptr_t)address, (uint32_t)length);
}
