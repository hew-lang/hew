/* Hew values at an `extern "C"` boundary.
 *
 * A C function declared in a Hew `extern "C" { .. }` block sees Hew values
 * through the types below. The language specification, section 3.9.3, is the
 * authority for the mapping; this header spells it for C11 and C++17.
 */
#ifndef HEW_H
#define HEW_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* A Hew `bytes` value: a view of `len` bytes starting `offset` bytes into a
 * reference-counted buffer. Several values may view one buffer. The empty
 * value is {NULL, 0, 0}; any other value has a non-null `ptr` returned by
 * `hew_bytes_new`.
 *
 * - A `bytes` parameter arrives as `const HewBytes *`. It is borrowed for the
 *   call: read through it, never write, retain or release it. A parameter
 *   declared `consume` arrives the same way and transfers its one reference to
 *   the callee, which must release it or return it.
 * - A `bytes` result is returned by value and transfers one reference to Hew.
 *   Build it with `hew_bytes_new`, which returns a buffer whose reference
 *   count is one.
 */
typedef struct HewBytes {
  uint8_t *ptr;
  uint32_t offset;
  uint32_t len;
} HewBytes;

/* A Hew `string` is an opaque handle to immutable UTF-8 whose layout is
 * private; NULL is the empty string. A `string` parameter arrives as
 * `const HewString *` and is borrowed. This header offers no way to read or
 * build one, so text that C code reads or produces crosses as `bytes`:
 * `s.to_bytes()` on the way in, `std.encoding.utf8.decode` on the way out. */
typedef struct HewString HewString;

/* Allocate a buffer of `capacity` writable bytes with a reference count of
 * one. Allocation failure aborts the process. */
uint8_t *hew_bytes_new(uint32_t capacity);

/* Add one reference to the buffer `data` (a `ptr` from a HewBytes). Null is
 * ignored. */
void hew_bytes_clone_ref(uint8_t *data);

/* Release one reference to the buffer `data`, freeing it with the last one.
 * Null is ignored. */
void hew_bytes_drop(uint8_t *data);

/* The first active byte of `value`, or NULL for the empty value. */
static inline const uint8_t *hew_bytes_data(const HewBytes *value) {
  return value->len == 0 ? NULL : value->ptr + value->offset;
}

/* A fresh `bytes` result holding a copy of `len` bytes from `data`. */
static inline HewBytes hew_bytes_copy(const uint8_t *data, uint32_t len) {
  HewBytes value = {NULL, 0, 0};
  if (len != 0) {
    value.ptr = hew_bytes_new(len);
    value.len = len;
    memcpy(value.ptr, data, len);
  }
  return value;
}

#ifdef __cplusplus
}
#endif

#endif /* HEW_H */
