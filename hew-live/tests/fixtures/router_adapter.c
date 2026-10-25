// Reference adapter: WIT service subtasks complete ordinary typed Hew pipes.
#include "router.h"
#include "guest.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

extern int32_t hew_sched_init(void);
extern bool hew_guest_step(void);
extern void hew_guest_begin_drain(void);
extern bool hew_guest_is_drained(void);
extern void hew_guest_resume(void);
extern void hew_sink_write(void *, const void *, size_t);
extern void hew_sink_write_string(void *, const HewText *);
extern void hew_sink_close(void *);

typedef struct Pending {
    bool emit;
    bool has_handle;
    uint32_t handle;
    void *status;
    void *value;
    hew_live_router_events_delivery_t input;
    union {
        hew_live_router_services_result_option_string_string_t destination;
        hew_live_router_services_result_void_string_t emit;
    } result;
} Pending;

typedef enum { ROUTE, SNAPSHOT, DISPOSE } TaskKind;

typedef struct Task {
    size_t refs;
    TaskKind kind;
    bool cancelled;
    bool completed;
    bool accepted;
    uint64_t snapshot;
    router_string_t failure;
    uint32_t waiters;
    Pending *pending;
    HewText *source;
    HewText *body;
    union {
        HewCall_guest_route *route;
        HewCall_guest_snapshot *snapshot;
        HewCall_guest_dispose *dispose;
    } call;
} Task;

static HewOwned_Router *state;
static size_t active;
typedef enum { NEW, RUNNING, QUIESCING, PAUSED, DISPOSING, CLOSED } Phase;
static Phase phase;

static void retain(void *raw) { ((Task *)raw)->refs++; }
static void release(void *raw) {
    Task *task = raw;
    if (--task->refs == 0) free(task);
}
// Polling occurs only after returning to the component callback.
static void wake(void *raw) { (void)raw; }

static router_string_t copy_text(const HewText *text) {
    size_t length;
    const unsigned char *bytes = hew_host_text_data(text, &length);
    router_string_t result;
    router_string_dup_n(&result, (const char *)bytes, length);
    return result;
}

static HewText *hew_text(router_string_t text) {
    HewText *result = NULL;
    HewError *error = NULL;
    assert(hew_host_text_from_utf8(text.ptr, text.len, &result, &error) == 0);
    return result;
}

static void failure(Task *task, HewError *error) {
    size_t length;
    const unsigned char *bytes = hew_host_error_message(error, &length);
    router_string_dup_n(&task->failure, (const char *)bytes, length);
    hew_host_error_release(error);
}

static void complete_pending(Task *task, bool cancelled) {
    Pending *pending = task->pending;
    if (!cancelled) {
        int64_t status;
        router_string_t *value = NULL;
        if (pending->emit) {
            status = pending->result.emit.is_err ? 1 : 0;
            if (status != 0) value = &pending->result.emit.val.err;
        } else if (pending->result.destination.is_err) {
            status = 2;
            value = &pending->result.destination.val.err;
        } else if (pending->result.destination.val.ok.is_some) {
            status = 1;
            value = &pending->result.destination.val.ok.val;
        } else {
            status = 0;
        }
        // Each fresh capacity-one pipe receives at most one item. The host
        // callback is outside Hew execution, and the receiving provider owns
        // admission and its exact element descriptor.
        if (value != NULL) {
            HewText *text = hew_text(*value);
            hew_sink_write_string(pending->value, text);
            hew_host_text_release(text);
        }
        hew_sink_write(pending->status, &status, sizeof(status));
        if (pending->emit) {
            hew_live_router_services_result_void_string_free(&pending->result.emit);
        } else {
            hew_live_router_services_result_option_string_string_free(&pending->result.destination);
        }
    }
    hew_sink_close(pending->status);
    hew_sink_close(pending->value);
    hew_live_router_events_delivery_free(&pending->input);
    if (pending->has_handle) router_subtask_drop(pending->handle);
    free(pending);
    task->pending = NULL;
}

static void await_pending(Task *task, router_subtask_status_t status) {
    task->pending->handle = ROUTER_SUBTASK_HANDLE(status);
    task->pending->has_handle = ROUTER_SUBTASK_STATE(status) != ROUTER_SUBTASK_RETURNED;
    if (ROUTER_SUBTASK_STATE(status) == ROUTER_SUBTASK_RETURNED) {
        complete_pending(task, false);
    } else {
        router_waitable_join(task->pending->handle, task->waiters);
    }
}

void router_destination(const HewText *source, void *status, void *value) {
    Task *task = router_context_get_0();
    assert(task != NULL && task->pending == NULL);
    Pending *pending = calloc(1, sizeof(*pending));
    assert(pending != NULL);
    pending->status = status;
    pending->value = value;
    pending->input.destination = copy_text(source);
    task->pending = pending;
    await_pending(task, hew_live_router_services_destination(
        pending->input.destination, &pending->result.destination));
}

void router_emit(const HewText *destination, const HewText *body,
                 void *status, void *value) {
    Task *task = router_context_get_0();
    assert(task != NULL && task->pending == NULL);
    Pending *pending = calloc(1, sizeof(*pending));
    assert(pending != NULL);
    pending->emit = true;
    pending->status = status;
    pending->value = value;
    pending->input.destination = copy_text(destination);
    pending->input.body = copy_text(body);
    task->pending = pending;
    await_pending(task, hew_live_router_services_emit(pending->input, &pending->result.emit));
}

static router_callback_code_t finish(Task *task) {
    assert(task->pending == NULL);
    if (task->cancelled) {
        if (task->kind == ROUTE) active--;
        else if (task->kind == SNAPSHOT && phase == QUIESCING) {
            hew_guest_resume();
            phase = RUNNING;
        }
        else if (task->kind == DISPOSE && phase == DISPOSING) phase = CLOSED;
        router_task_cancel();
    } else if (task->kind == SNAPSHOT) {
        exports_hew_live_router_handler_result_snapshot_string_t result = {0};
        result.is_err = task->failure.ptr != NULL;
        if (result.is_err) result.val.err = task->failure;
        else result.val.ok.accepted = task->snapshot;
        if (phase == QUIESCING) {
            if (result.is_err) hew_guest_resume();
            phase = result.is_err ? RUNNING : PAUSED;
        }
        exports_hew_live_router_handler_quiesce_return(result);
    } else if (task->kind == DISPOSE) {
        exports_hew_live_router_handler_result_void_string_t result = {0};
        result.is_err = task->failure.ptr != NULL;
        if (result.is_err) result.val.err = task->failure;
        if (phase == DISPOSING) phase = CLOSED;
        exports_hew_live_router_handler_dispose_return(result);
    } else {
        exports_hew_live_router_handler_result_bool_string_t result = {0};
        result.is_err = task->failure.ptr != NULL;
        if (result.is_err) result.val.err = task->failure;
        else result.val.ok = task->accepted;
        exports_hew_live_router_handler_route_return(result);
        active--;
    }
    router_string_free(&task->failure);
    hew_host_text_release(task->source);
    hew_host_text_release(task->body);
    router_waitable_set_drop(task->waiters);
    router_context_set_0(NULL);
    release(task);
    return ROUTER_CALLBACK_CODE_EXIT;
}

static router_callback_code_t pump(Task *task) {
    for (unsigned i = 0; i < 64; i++) {
        if (!task->completed && task->kind == SNAPSHOT && task->call.snapshot == NULL) {
            if (!hew_guest_is_drained()) {
                hew_guest_step();
                continue;
            }
            HewWaker waker = {task, wake, retain, release};
            HewError *error = NULL;
            if (guest_snapshot_start(state, &waker, &task->call.snapshot, &error) != 0) {
                failure(task, error);
                task->completed = true;
            }
        }
        if (!task->completed) {
            bool ready = task->kind == SNAPSHOT ? guest_snapshot_poll(task->call.snapshot)
                       : task->kind == DISPOSE ? guest_dispose_poll(task->call.dispose)
                       : guest_route_poll(task->call.route);
            if (ready) {
                HewError *error = NULL;
                int32_t status;
                if (task->kind == SNAPSHOT) {
                    status = guest_snapshot_result(task->call.snapshot, &task->snapshot, &error);
                } else if (task->kind == DISPOSE) {
                    status = guest_dispose_result(task->call.dispose, &error);
                } else {
                    uint8_t accepted = 0;
                    status = guest_route_result(task->call.route, &accepted, &error);
                    task->accepted = accepted != 0;
                }
                if (status != 0) failure(task, error);
                error = NULL;
                status = task->kind == SNAPSHOT ? guest_snapshot_release(&task->call.snapshot, &error)
                       : task->kind == DISPOSE ? guest_dispose_release(&task->call.dispose, &error)
                       : guest_route_release(&task->call.route, &error);
                assert(status == 0 && error == NULL);
                task->completed = true;
            }
        }
        if (task->completed && task->pending == NULL) return finish(task);
        if (!hew_guest_step()) {
            if (task->pending != NULL) return ROUTER_CALLBACK_CODE_WAIT(task->waiters);
            return ROUTER_CALLBACK_CODE_YIELD;
        }
    }
    return ROUTER_CALLBACK_CODE_YIELD;
}

static router_callback_code_t callback(router_event_t *event) {
    Task *task = router_context_get_0();
    assert(task != NULL);
    if (event->event == ROUTER_EVENT_SUBTASK) {
        assert(task->pending != NULL && task->pending->handle == event->waitable);
        if (event->code == ROUTER_SUBTASK_RETURNED) complete_pending(task, false);
        else if (event->code == ROUTER_SUBTASK_RETURNED_CANCELLED ||
                 event->code == ROUTER_SUBTASK_STARTED_CANCELLED) complete_pending(task, true);
    } else if (event->event == ROUTER_EVENT_CANCEL) {
        task->cancelled = true;
        if (task->kind == SNAPSHOT) {
            if (task->call.snapshot != NULL) guest_snapshot_cancel(task->call.snapshot);
            else task->completed = true;
        }
        else if (task->kind == DISPOSE) guest_dispose_cancel(task->call.dispose);
        else guest_route_cancel(task->call.route);
        if (task->pending != NULL) {
            router_subtask_status_t status = router_subtask_cancel(task->pending->handle);
            if (ROUTER_SUBTASK_STATE(status) == ROUTER_SUBTASK_RETURNED) complete_pending(task, false);
            else if (ROUTER_SUBTASK_STATE(status) == ROUTER_SUBTASK_RETURNED_CANCELLED ||
                     ROUTER_SUBTASK_STATE(status) == ROUTER_SUBTASK_STARTED_CANCELLED) complete_pending(task, true);
        }
    }
    return pump(task);
}

static Task *new_task(TaskKind kind) {
    Task *task = calloc(1, sizeof(*task));
    assert(task != NULL);
    task->refs = 1;
    task->kind = kind;
    task->waiters = router_waitable_set_new();
    router_context_set_0(task);
    return task;
}

bool exports_hew_live_router_handler_initialize(
    exports_hew_live_router_handler_snapshot_t *snapshot, router_string_t *error) {
    if (phase != NEW) {
        router_string_dup(error, "router is already initialized");
        return false;
    }
    assert(hew_sched_init() == 0);
    HewError *fault = NULL;
    if (guest_create(snapshot->accepted, &state, &fault) != 0) {
        size_t length;
        const unsigned char *bytes = hew_host_error_message(fault, &length);
        router_string_dup_n(error, (const char *)bytes, length);
        hew_host_error_release(fault);
        return false;
    }
    phase = RUNNING;
    return true;
}

router_callback_code_t exports_hew_live_router_handler_route(
    exports_hew_live_router_handler_event_t *message) {
    Task *task = new_task(ROUTE);
    active++;
    if (phase != RUNNING) {
        router_string_dup(&task->failure, "router is not accepting events");
        task->completed = true;
    } else {
        task->source = hew_text(message->source);
        task->body = hew_text(message->body);
        HewWaker waker = {task, wake, retain, release};
        HewError *error = NULL;
        if (guest_route_start(state, task->source, task->body, &waker, &task->call.route, &error) != 0) {
            failure(task, error);
            task->completed = true;
        }
    }
    exports_hew_live_router_handler_event_free(message);
    return pump(task);
}

router_callback_code_t exports_hew_live_router_handler_route_callback(router_event_t *event) {
    return callback(event);
}

router_callback_code_t exports_hew_live_router_handler_quiesce(void) {
    Task *task = new_task(SNAPSHOT);
    if (phase != RUNNING || active != 0) {
        router_string_dup(&task->failure, "router must drain admitted calls before quiescence");
        task->completed = true;
    } else {
        phase = QUIESCING;
        hew_guest_begin_drain();
    }
    return pump(task);
}

router_callback_code_t exports_hew_live_router_handler_quiesce_callback(router_event_t *event) {
    return callback(event);
}

bool exports_hew_live_router_handler_resume(router_string_t *error) {
    if (phase != PAUSED) {
        router_string_dup(error, "router has no suspended instance to resume");
        return false;
    }
    phase = RUNNING;
    hew_guest_resume();
    return true;
}

router_callback_code_t exports_hew_live_router_handler_dispose(void) {
    Task *task = new_task(DISPOSE);
    if (phase != PAUSED || active != 0) {
        router_string_dup(&task->failure, "router must quiesce before disposal");
        task->completed = true;
    } else {
        phase = DISPOSING;
        HewWaker waker = {task, wake, retain, release};
        HewError *error = NULL;
        if (guest_dispose_start(&state, &waker, &task->call.dispose, &error) != 0) {
            phase = PAUSED;
            failure(task, error);
            task->completed = true;
        }
    }
    return pump(task);
}

router_callback_code_t exports_hew_live_router_handler_dispose_callback(router_event_t *event) {
    return callback(event);
}
