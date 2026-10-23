#include "async_component.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#ifdef __linux__
#include <pthread.h>
#endif
#ifdef __cplusplus
#include <atomic>
typedef std::atomic<unsigned> Counter;
#define increment(value) ((value)->fetch_add(1))
#define decrement(value) ((value)->fetch_sub(1))
#define load(value) ((value)->load())
#else
#include <stdatomic.h>
typedef atomic_uint Counter;
#define increment(value) atomic_fetch_add(value, 1)
#define decrement(value) atomic_fetch_sub(value, 1)
#define load(value) atomic_load(value)
#endif

typedef struct Wake {
  Counter refs;
  Counter notifications;
#ifdef __linux__
  pthread_mutex_t mutex;
  pthread_cond_t changed;
  int pending;
#endif
} Wake;

static void wake(void *context) {
  Wake *target = (Wake *)context;
#ifdef __linux__
  assert(pthread_mutex_lock(&target->mutex) == 0);
#endif
  increment(&target->notifications);
#ifdef __linux__
  target->pending = 1;
  assert(pthread_cond_signal(&target->changed) == 0);
  assert(pthread_mutex_unlock(&target->mutex) == 0);
#endif
}
static void retain(void *context) { increment(&((Wake *)context)->refs); }
static void release(void *context) { decrement(&((Wake *)context)->refs); }

static void refused(HewError **error, int32_t status) {
  assert(status == HEW_INVALID_STATE && *error);
  assert(hew_host_error_code(*error) == status);
  hew_host_error_release(*error);
  *error = NULL;
}

#ifdef __linux__
/* The existing Linux safety target exercises actual pending operations. A
 * notification is the only reason to poll again; the runner owns the watchdog.
 */
static void wait_ready(Wake *target) {
  assert(pthread_mutex_lock(&target->mutex) == 0);
  while (!target->pending)
    assert(pthread_cond_wait(&target->changed, &target->mutex) == 0);
  target->pending = 0;
  assert(pthread_mutex_unlock(&target->mutex) == 0);
}

#define drain(prefix, call, target)                                            \
  do {                                                                         \
    while (prefix##_poll(call) == HEW_CALL_PENDING)                            \
      wait_ready(target);                                                      \
  } while (0)

static void error_contains(const HewError *error, const char *needle) {
  size_t len = 0;
  const unsigned char *message = hew_host_error_message(error, &len);
  size_t size = strlen(needle), index = 0;
  while (index + size <= len && memcmp(message + index, needle, size))
    ++index;
  assert(index + size <= len);
}

static void closed_once(const char *path) {
  FILE *file = fopen(path, "rb");
  assert(file);
  unsigned char bytes[32];
  size_t len = fread(bytes, 1, sizeof(bytes), file);
  assert(len == 11 && !memcmp(bytes, "closed once", len));
  assert(fclose(file) == 0);
}

static HewOwned_Marker *marker(HewRuntime *runtime, Wake *readiness,
                               HewWaker *notify, const char *path,
                               uint8_t fail_close) {
  HewText *text = NULL;
  HewError *error = NULL;
  assert(hew_host_text_from_utf8((const unsigned char *)path, strlen(path),
                                 &text, &error) == HEW_OK);
  HewCall_async_marker_new *call = NULL;
  assert(async_marker_new_start(runtime, text, fail_close, notify, &call,
                                &error) == HEW_OK);
  drain(async_marker_new, call, readiness);
  HewOwned_Marker *owner = NULL;
  assert(async_marker_new_result(call, &owner, &error) == HEW_OK && owner &&
         !error);
  assert(async_marker_new_release(&call, &error) == HEW_OK && !call && !error);
  hew_host_text_release(text);
  return owner;
}

static void pending_calls(HewRuntime *runtime, Wake *readiness,
                          HewWaker *notify, const char *path) {
  for (uint8_t fail_close = 0; fail_close != 2; ++fail_close) {
    HewOwned_Marker *owner =
        marker(runtime, readiness, notify, path, fail_close);
    HewCall_async_wait *call = NULL;
    HewError *error = NULL;
    assert(async_wait_start(runtime, &owner, notify, &call, &error) == HEW_OK &&
           !owner && call && !error);
    assert(async_wait_poll(call) == HEW_CALL_PENDING);
    HewCall_async_wait *retained = call;
    refused(&error, async_wait_release(&call, &error));
    assert(call == retained);
    async_wait_cancel(call);
    async_wait_cancel(call);
    drain(async_wait, call, readiness);
    HewText *result = NULL;
    assert(async_wait_result(call, &result, &error) == HEW_LOGICAL_FAULT &&
           !result && error);
    if (fail_close)
      error_contains(error, "host cleanup failed");
    hew_host_error_release(error);
    error = NULL;
    assert(async_wait_release(&call, &error) == HEW_OK && !call && !error);
    closed_once(path);
  }
  HewOwned_Marker *owner = marker(runtime, readiness, notify, path, 1);
  HewCall_async_fail *failed = NULL;
  HewError *error = NULL;
  assert(async_fail_start(runtime, &owner, notify, &failed, &error) == HEW_OK &&
         !owner && failed && !error);
  drain(async_fail, failed, readiness);
  HewText *result = NULL;
  assert(async_fail_result(failed, &result, &error) == HEW_LOGICAL_FAULT &&
         !result && error);
  error_contains(error, "host body failed");
  error_contains(error, "host cleanup failed");
  hew_host_error_release(error);
  error = NULL;
  assert(async_fail_release(&failed, &error) == HEW_OK && !failed && !error);
  closed_once(path);

  const unsigned char expected[] = {'h', 0, 0xc3, 0xa9};
  HewText *input = NULL;
  assert(hew_host_text_from_utf8(expected, sizeof(expected), &input, &error) ==
             HEW_OK &&
         input && !error);
  HewCall_async_echo *echo = NULL;
  assert(async_echo_start(runtime, input, notify, &echo, &error) == HEW_OK &&
         echo && !error);
  drain(async_echo, echo, readiness);
  hew_host_text_release(input);
  async_echo_cancel(echo);
  assert(async_echo_result(echo, &result, &error) == HEW_OK && result &&
         !error);
  assert(async_echo_release(&echo, &error) == HEW_OK && !echo && !error);
  size_t len = 0;
  const unsigned char *bytes = hew_host_text_data(result, &len);
  assert(len == sizeof(expected) && !memcmp(bytes, expected, len));
  hew_host_text_release(result);
}
#endif

int main(int argc, char **argv) {
  HewRuntime *runtime = hew_runtime_new();
#ifdef __linux__
  assert(argc == 2);
  Wake readiness = {1, 0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER,
                    0};
#else
  (void)argc;
  (void)argv;
  Wake readiness = {1, 0};
#endif
  HewWaker notify = {&readiness, wake, retain, release};
  HewCall_async_add *call = NULL;
  HewError *error = NULL;
  int64_t result = -99;
  assert(async_add_start(runtime, 20, 22, &notify, &call, &error) == HEW_OK);
  assert(call && !error && load(&readiness.refs) == 2);
  HewCall_async_add *retained = call;
  refused(&error, async_add_release(&call, &error));
  assert(call == retained);
  refused(&error, async_add_result(call, &result, &error));
  assert(result == -99);
  assert(async_add_poll(call) == HEW_CALL_READY);
  async_add_cancel(call);
  refused(&error, async_add_release(&call, &error));
  assert(call == retained);
  assert(async_add_result(call, &result, &error) == HEW_OK && result == 42 &&
         !error);
  refused(&error, async_add_result(call, &result, &error));
  assert(result == 42);
  assert(async_add_release(&call, &error) == HEW_OK && !call && !error);
  assert(async_add_release(&call, &error) == HEW_OK && !error);
  assert(load(&readiness.refs) == 1 && load(&readiness.notifications) == 1);
#ifdef __linux__
  pending_calls(runtime, &readiness, &notify, argv[1]);
#endif
  hew_runtime_shutdown(runtime);
  hew_runtime_release(runtime);
  assert(load(&readiness.refs) == 1);
#ifdef __linux__
  assert(pthread_cond_destroy(&readiness.changed) == 0);
  assert(pthread_mutex_destroy(&readiness.mutex) == 0);
#endif
  return 0;
}
