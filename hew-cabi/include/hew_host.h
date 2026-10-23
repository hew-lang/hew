/* Experimental Hew host API: same-build linkage only.
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
typedef struct HewRuntime HewRuntime;

/* Retained readiness only: all callbacks are thread-safe and never unwind.
 * wake schedules a future host-thread poll; it must not resume/release inline.
 * retain/release own one context reference, including late wake races. */
typedef struct HewWaker {
  void *context;
  void (*wake)(void *);
  void (*retain)(void *);
  void (*release)(void *);
} HewWaker;

#define HEW_CALL_PENDING INT32_C(0)
#define HEW_CALL_READY INT32_C(1)

/* One process-default runtime, shared by all handles from new/retain.
 * Hosted work and opaque values must finish before explicit shutdown or the
 * final release. No independent-runtime isolation is promised. */
HewRuntime *hew_runtime_new(void);
void hew_runtime_retain(HewRuntime *runtime);
void hew_runtime_shutdown(HewRuntime *runtime);
void hew_runtime_release(HewRuntime *runtime);

/* Function results use int32_t, not the implementation-defined C enum ABI. */
#define HEW_OK INT32_C(0)
#define HEW_INVALID_UTF8 INT32_C(1)
#define HEW_INVALID_JSON INT32_C(2)
#define HEW_MISSING_FIELD INT32_C(3)
#define HEW_WRONG_KIND INT32_C(4)
#define HEW_ENCODE_ERROR INT32_C(5)
#define HEW_LOGICAL_FAULT INT32_C(6)
#define HEW_INVALID_STATE INT32_C(7)

/* Ownership and validity:
 * - One host thread; synchronous exports complete before returning, including
 *   cooperative suspension. Actor/I/O components require an owned runtime.
 *   Release all owners before shutdown or unloading descriptor code. No
 *   cross-version ABI promise.
 * - All handles are opaque. Non-null arguments must be correctly typed, live
 *   owners from this build; never inspect their layout or use free/delete on
 * them.
 * - SDK text/JSON inputs are borrowed for the call. Mutation exclusively
 *   borrows its JSON owner. Generated exports describe opaque consuming inputs
 *   as TYPE **owner: handoff clears that slot, including on failure. Opaque
 *   TYPE * inputs borrow exclusively; const TYPE * inputs borrow shared.
 * - Fallible-call result/error storage is required, aligned and writable.
 *   Pointer-valued result and error slots must initially be null. Scalar result
 *   storage may hold any prior value.
 *   Slots must be distinct and must not overlap any input or handle allocation.
 * - Success sets one non-null output owner (except set_text) and a null error.
 *   Failure leaves output null and supplies one non-null error whose code
 * equals the return status. Failed mutation leaves the JSON value unchanged.
 * - Output owners are independent of inputs. Copy is a logical value copy;
 *   set_text copies its borrowed text. SDK calls consume input owners only on
 *   release; generated consuming arguments and typed release functions also
 *   transfer ownership. Generated scalar outputs are written only on success.
 * - Asynchronous exports retain their process runtime and wake target. Keep
 *   borrowed owners and mutable input slots live and exclusively accessible as
 *   declared until poll reports HEW_CALL_READY. Only wake callbacks may run on
 *   other threads; start/poll/cancel/result/release stay on the owning host
 *   thread and must not overlap or re-enter each other.
 * - poll is nonblocking and resumes work only after readiness. cancel requests
 *   cancellation; continue polling until mandatory cleanup completes. A
 * terminal result wins a late cancel. result transfers the terminal value/error
 * once.
 * - Early or repeated result and early release return HEW_INVALID_STATE with
 *   an owned error. Refusal preserves the call owner and result storage. Take
 *   the terminal result before release, including after cancellation.
 * Successful release clears the call slot; releasing a null call slot succeeds.
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
