#include "async_component.h"
#include <assert.h>
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
} Wake;

static void wake(void *context) {
  increment(&((Wake *)context)->notifications);
}
static void retain(void *context) { increment(&((Wake *)context)->refs); }
static void release(void *context) { decrement(&((Wake *)context)->refs); }

static void refused(HewError **error, int32_t status) {
  assert(status == HEW_INVALID_STATE && *error);
  assert(hew_host_error_code(*error) == status);
  hew_host_error_release(*error);
  *error = NULL;
}

int main(void) {
  HewRuntime *runtime = hew_runtime_new();
  Wake readiness = {1, 0};
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
  hew_runtime_shutdown(runtime);
  hew_runtime_release(runtime);
  return 0;
}
