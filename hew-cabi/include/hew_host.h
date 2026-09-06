/* Experimental synchronous Hew host API: same-build static linkage only.
 * This deliberate public surface is separate from the runtime export census.
 */
#ifndef HEW_HOST_H
#define HEW_HOST_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct HewText HewText;
typedef struct HewJson HewJson;
typedef struct HewError HewError;

/* Function results use int32_t, not the implementation-defined C enum ABI. */
#define HEW_OK INT32_C(0)
#define HEW_INVALID_UTF8 INT32_C(1)
#define HEW_INVALID_JSON INT32_C(2)
#define HEW_MISSING_FIELD INT32_C(3)
#define HEW_WRONG_KIND INT32_C(4)
#define HEW_ENCODE_ERROR INT32_C(5)
#define HEW_LOGICAL_FAULT INT32_C(6)

/* Ownership and validity:
 * - One host thread, synchronous calls; no callbacks, actors or scheduler
 * setup. Release all owners before unloading the library. No cross-version ABI
 * promise.
 * - All handles are opaque. Non-null arguments must be correctly typed, live
 *   owners from this build; never inspect their layout or use free/delete on
 * them.
 * - Inputs are borrowed for the call. Mutation exclusively borrows its JSON
 * owner.
 * - Fallible-call result/error pointers are required, aligned, writable,
 *   initially null pointer slots.
 *   Slots must be distinct and must not overlap any input or handle allocation.
 * - Success sets one non-null output owner (except set_text) and a null error.
 *   Failure leaves output null and supplies one non-null error whose code
 * equals the return status. Failed mutation leaves the JSON value unchanged.
 * - Output owners are independent of inputs. Copy is a logical value copy;
 *   set_text copies its borrowed text. No call consumes input owners except
 * release.
 * - Release accepts null; otherwise discharge each returned owner exactly once,
 *   after all borrowed views end. Invalid handles/aliasing violate
 * preconditions.
 * - Pointer/length inputs permit null only when length is zero; otherwise they
 *   name at least length readable bytes, with length <= PTRDIFF_MAX.
 * - Text is validated UTF-8, including NUL. Empty text has a non-null owner.
 *   Views include all bytes, with no NUL-termination promise, and remain valid
 *   until that owner is released. Empty views point to a readable zero
 * sentinel. Inspector length pointers must be writable and disjoint from the
 * owner.
 * - Error messages are owned snapshots unaffected by later calls. Their
 * complete UTF-8 bytes may contain NUL/newlines; wording is not a
 * machine-readable protocol.
 * - Allocation failure is process-fatal. Hardware faults and invalid pointers
 *   are not recoverable statuses. No Rust unwind may cross this C boundary.
 */
int32_t hew_host_text_from_utf8(const unsigned char *data, size_t len,
                                HewText **out, HewError **error);
const unsigned char *hew_host_text_data(const HewText *text, size_t *len);
void hew_host_text_release(HewText *text);

int32_t hew_host_json_parse(const unsigned char *data, size_t len,
                            HewJson **out, HewError **error);
int32_t hew_host_json_copy(const HewJson *value, HewJson **out,
                           HewError **error);
/* Missing field is distinct from wrong kind, including an explicit JSON null.
 */
int32_t hew_host_json_get_text(const HewJson *value, const unsigned char *key,
                               size_t key_len, HewText **out, HewError **error);
int32_t hew_host_json_set_text(HewJson *value, const unsigned char *key,
                               size_t key_len, const HewText *text,
                               HewError **error);
int32_t hew_host_json_encode(const HewJson *value, HewText **out,
                             HewError **error);
void hew_host_json_release(HewJson *value);

int32_t hew_host_error_code(const HewError *error);
const unsigned char *hew_host_error_message(const HewError *error, size_t *len);
void hew_host_error_release(HewError *error);

#ifdef __cplusplus
}
#endif
#endif /* HEW_HOST_H */
