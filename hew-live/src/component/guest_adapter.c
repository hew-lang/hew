// SDK lifecycle adapter. Service carriers are generated from the WIT contract.
#include <assert.h>
#include <stdlib.h>
#include "guest.h"

#define JOIN_LITERAL(prefix, suffix) prefix##suffix
#define JOIN(prefix, suffix) JOIN_LITERAL(prefix, suffix)
#define WIT(suffix) JOIN(WIT_WORLD, suffix)
#define WIT_CONSTANT(suffix) JOIN(WIT_WORLD_UPPER, suffix)
#define HANDLER(suffix) JOIN(WIT_HANDLER, suffix)

#include "guest_contract.h"

extern int32_t hew_sched_init(void);
extern bool hew_guest_step(void);
extern int64_t hew_guest_next_timer_ms(void);
extern void hew_guest_begin_drain(void);
extern bool hew_guest_is_drained(void);
extern void hew_guest_resume(void);
extern void hew_sink_write(void *, const void *, size_t);
extern void hew_sink_write_string(void *, const HewText *);
extern void hew_sink_close(void *);

typedef struct Pending {
    struct Pending *next;
    bool has_handle;
    uint32_t handle;
    void *status;
    void *ok;
    void *error;
    void (*settle)(struct Pending *, bool);
} Pending;

typedef enum { INVOKE, SNAPSHOT, DISPOSE, DRIVE } TaskKind;

typedef struct Task {
    size_t refs;
    TaskKind kind;
    bool cancelled;
    bool abort_requested;
    bool completed;
    GuestReply reply;
    GuestSnapshot snapshot;
    WIT(_string_t) failure;
    uint32_t waiters;
    Pending *pending;
    GuestInput input;
    union {
        HewCall_guest_route *invoke;
        HewCall_guest_snapshot *snapshot;
        HewCall_guest_dispose *dispose;
    } call;
} Task;

static GuestState *state;
static size_t active;
typedef enum { NEW, RUNNING, QUIESCING, PAUSED, DISPOSING, CLOSED } Phase;
static Phase phase;
static Task *quiescing;

static void retain(void *raw) { ((Task *)raw)->refs++; }
static void release(void *raw) {
    Task *task = raw;
    if (--task->refs == 0) free(task);
}
// Polling occurs only after returning to the component callback.
static void wake(void *raw) { (void)raw; }

static HewText *hew_text(WIT(_string_t) text) {
    HewText *result = NULL;
    HewError *error = NULL;
    assert(hew_host_text_from_utf8(text.ptr, text.len, &result, &error) == 0);
    return result;
}

static void failure(Task *task, HewError *error) {
    size_t length;
    const unsigned char *bytes = hew_host_error_message(error, &length);
    WIT(_string_dup_n)(&task->failure, (const char *)bytes, length);
    hew_host_error_release(error);
}

static void complete_pending(Task *task, Pending *pending, bool cancelled) {
    Pending **link = &task->pending;
    while (*link != pending) link = &(*link)->next;
    *link = pending->next;
    pending->settle(pending, cancelled);
    hew_sink_close(pending->status);
    if (pending->ok != NULL) hew_sink_close(pending->ok);
    hew_sink_close(pending->error);
    if (pending->has_handle) WIT(_subtask_drop)(pending->handle);
    free(pending);
}

static void await_pending(Task *task, Pending *pending, WIT(_subtask_status_t) status) {
    pending->handle = WIT_CONSTANT(_SUBTASK_HANDLE)(status);
    pending->has_handle = WIT_CONSTANT(_SUBTASK_STATE)(status) != WIT_CONSTANT(_SUBTASK_RETURNED);
    if (WIT_CONSTANT(_SUBTASK_STATE)(status) == WIT_CONSTANT(_SUBTASK_RETURNED)) {
        complete_pending(task, pending, false);
    } else {
        WIT(_waitable_join)(pending->handle, task->waiters);
    }
}

/* HEW_WIT_SERVICE_BINDINGS */

static WIT(_callback_code_t) finish(Task *task) {
    assert(task->pending == NULL);
    if (task->kind != DRIVE) active--;
    if (task->cancelled) {
        if (task->kind == SNAPSHOT && quiescing == task) {
            quiescing = NULL;
            hew_guest_resume();
            phase = RUNNING;
        }
        else if (task->kind == DISPOSE && phase == DISPOSING) phase = CLOSED;
        WIT(_task_cancel)();
    } else if (task->kind == DRIVE) {
        int64_t delay = phase == RUNNING || phase == QUIESCING ? hew_guest_next_timer_ms() : -1;
        WIT(_option_u64_t) result = { .is_some = delay >= 0, .val = delay < 0 ? 0 : (uint64_t)delay };
        HANDLER(_drive_return)(result);
    } else if (task->kind == SNAPSHOT) {
        if (task->abort_requested && task->failure.ptr == NULL) {
            WIT(_string_dup)(&task->failure, "quiescence attempt abandoned");
        }
        HANDLER(_result_snapshot_string_t) result = {0};
        result.is_err = task->failure.ptr != NULL;
        if (result.is_err) result.val.err = task->failure;
        else result.val.ok = task->snapshot;
        if (quiescing == task) {
            quiescing = NULL;
            if (result.is_err) hew_guest_resume();
            phase = result.is_err ? RUNNING : PAUSED;
        }
        HANDLER(_quiesce_return)(result);
    } else if (task->kind == DISPOSE) {
        HANDLER(_result_void_string_t) result = {0};
        result.is_err = task->failure.ptr != NULL;
        if (result.is_err) result.val.err = task->failure;
        if (phase == DISPOSING) phase = CLOSED;
        HANDLER(_dispose_return)(result);
    } else {
        WireReply result = {0};
        result.is_err = task->failure.ptr != NULL;
        if (result.is_err) result.val.err = task->failure;
        else result.val.ok = task->reply;
        HANDLER(WIT_INVOKE_RETURN)(result);
    }
    WIT(_string_free)(&task->failure);
    release_input(&task->input);
    WIT(_waitable_set_drop)(task->waiters);
    WIT(_context_set_0)(NULL);
    release(task);
    return WIT_CONSTANT(_CALLBACK_CODE_EXIT);
}

static WIT(_callback_code_t) pump(Task *task) {
    for (unsigned i = 0; i < 64; i++) {
        if (!task->completed && task->kind == SNAPSHOT && quiescing != task &&
            !task->abort_requested) {
            task->abort_requested = true;
            if (task->call.snapshot == NULL) {
                task->completed = true;
                WIT(_string_dup)(&task->failure, "quiescence attempt abandoned");
            } else {
                guest_snapshot_cancel(task->call.snapshot);
            }
        }
        if (!task->completed && task->kind == SNAPSHOT && task->call.snapshot == NULL) {
            if (!hew_guest_is_drained()) {
                if (!hew_guest_step() && task->pending != NULL) return WIT_CONSTANT(_CALLBACK_CODE_WAIT)(task->waiters);
                continue;
            }
            HewWaker waker = {task, wake, retain, release};
            HewError *error = NULL;
            if (guest_snapshot_start(state, &waker, &task->call.snapshot, &error) != 0) {
                failure(task, error);
                task->completed = true;
            }
        }
        if (!task->completed && task->kind != DRIVE) {
            bool ready = task->kind == SNAPSHOT ? guest_snapshot_poll(task->call.snapshot)
                       : task->kind == DISPOSE ? guest_dispose_poll(task->call.dispose)
                       : guest_route_poll(task->call.invoke);
            if (ready) {
                HewError *error = NULL;
                int32_t status;
                if (task->kind == SNAPSHOT) {
                    status = snapshot_result(task->call.snapshot, &task->snapshot, &error);
                } else if (task->kind == DISPOSE) {
                    status = guest_dispose_result(task->call.dispose, &error);
                } else {
                    status = invoke_result(task->call.invoke, &task->reply, &error);
                }
                if (status != 0) failure(task, error);
                error = NULL;
                status = task->kind == SNAPSHOT ? guest_snapshot_release(&task->call.snapshot, &error)
                       : task->kind == DISPOSE ? guest_dispose_release(&task->call.dispose, &error)
                       : guest_route_release(&task->call.invoke, &error);
                assert(status == 0 && error == NULL);
                task->completed = true;
            }
        }
        if (task->completed) {
            if (task->pending == NULL) return finish(task);
            return WIT_CONSTANT(_CALLBACK_CODE_WAIT)(task->waiters);
        }
        if (!hew_guest_step()) {
            if (task->kind == DRIVE) {
                task->completed = true;
                if (task->pending == NULL) return finish(task);
            }
            if (task->pending != NULL) return WIT_CONSTANT(_CALLBACK_CODE_WAIT)(task->waiters);
            return WIT_CONSTANT(_CALLBACK_CODE_YIELD);
        }
    }
    return WIT_CONSTANT(_CALLBACK_CODE_YIELD);
}

static WIT(_callback_code_t) callback(WIT(_event_t) *event) {
    Task *task = WIT(_context_get_0)();
    assert(task != NULL);
    if (event->event == WIT_CONSTANT(_EVENT_SUBTASK)) {
        Pending *pending = task->pending;
        while (pending != NULL && pending->handle != event->waitable) pending = pending->next;
        assert(pending != NULL);
        if (event->code == WIT_CONSTANT(_SUBTASK_RETURNED)) complete_pending(task, pending, false);
        else if (event->code == WIT_CONSTANT(_SUBTASK_RETURNED_CANCELLED) ||
                 event->code == WIT_CONSTANT(_SUBTASK_STARTED_CANCELLED)) complete_pending(task, pending, true);
    } else if (event->event == WIT_CONSTANT(_EVENT_CANCEL)) {
        task->cancelled = true;
        if (task->kind == SNAPSHOT) {
            if (task->call.snapshot != NULL) guest_snapshot_cancel(task->call.snapshot);
            else task->completed = true;
        }
        else if (task->kind == DISPOSE) guest_dispose_cancel(task->call.dispose);
        else if (task->kind == INVOKE) guest_route_cancel(task->call.invoke);
        else task->completed = true;
        for (Pending *pending = task->pending, *next; pending != NULL; pending = next) {
            next = pending->next;
            WIT(_subtask_status_t) status = WIT(_subtask_cancel)(pending->handle);
            if (WIT_CONSTANT(_SUBTASK_STATE)(status) == WIT_CONSTANT(_SUBTASK_RETURNED)) complete_pending(task, pending, false);
            else if (WIT_CONSTANT(_SUBTASK_STATE)(status) == WIT_CONSTANT(_SUBTASK_RETURNED_CANCELLED) ||
                     WIT_CONSTANT(_SUBTASK_STATE)(status) == WIT_CONSTANT(_SUBTASK_STARTED_CANCELLED)) complete_pending(task, pending, true);
        }
    }
    return pump(task);
}

static Task *new_task(TaskKind kind) {
    Task *task = calloc(1, sizeof(*task));
    assert(task != NULL);
    task->refs = 1;
    if (kind != DRIVE) active++;
    task->kind = kind;
    task->waiters = WIT(_waitable_set_new)();
    WIT(_context_set_0)(task);
    return task;
}

bool HANDLER(_initialize)(
    GuestSnapshot *snapshot, WIT(_string_t) *error) {
    if (phase != NEW) {
        WIT(_string_dup)(error, GUEST_NAME " is already initialized");
        return false;
    }
    assert(hew_sched_init() == 0);
    HewError *fault = NULL;
    if (initialize(snapshot, &state, &fault) != 0) {
        size_t length;
        const unsigned char *bytes = hew_host_error_message(fault, &length);
        WIT(_string_dup_n)(error, (const char *)bytes, length);
        hew_host_error_release(fault);
        return false;
    }
    phase = RUNNING;
    return true;
}

WIT(_callback_code_t) HANDLER(WIT_INVOKE)(WireInput *message) {
    Task *task = new_task(INVOKE);
    if (phase != RUNNING) {
        WIT(_string_dup)(&task->failure, GUEST_NAME " is not accepting events");
        task->completed = true;
    } else {
        task->input = acquire_input(message);
        HewWaker waker = {task, wake, retain, release};
        HewError *error = NULL;
        if (invoke_start(state, &task->input, &waker, &task->call.invoke, &error) != 0) {
            failure(task, error);
            task->completed = true;
        }
    }
    release_message(message);
    return pump(task);
}

WIT(_callback_code_t) HANDLER(WIT_INVOKE_CALLBACK)(WIT(_event_t) *event) {
    return callback(event);
}

WIT(_callback_code_t) HANDLER(_quiesce)(void) {
    Task *task = new_task(SNAPSHOT);
    if (phase != RUNNING || active != 1) {
        WIT(_string_dup)(&task->failure, GUEST_NAME " must drain admitted calls before quiescence");
        task->completed = true;
    } else {
        phase = QUIESCING;
        quiescing = task;
        hew_guest_begin_drain();
    }
    return pump(task);
}

WIT(_callback_code_t) HANDLER(_quiesce_callback)(WIT(_event_t) *event) {
    return callback(event);
}

bool HANDLER(_resume)(WIT(_string_t) *error) {
    if (phase != PAUSED && phase != QUIESCING) {
        WIT(_string_dup)(error, GUEST_NAME " has no suspended instance to resume");
        return false;
    }
    phase = RUNNING;
    quiescing = NULL;
    hew_guest_resume();
    return true;
}

WIT(_callback_code_t) HANDLER(_dispose)(void) {
    Task *task = new_task(DISPOSE);
    if (phase != PAUSED || active != 1) {
        WIT(_string_dup)(&task->failure, GUEST_NAME " must quiesce before disposal");
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

WIT(_callback_code_t) HANDLER(_dispose_callback)(WIT(_event_t) *event) {
    return callback(event);
}

WIT(_callback_code_t) HANDLER(_drive)(void) {
    Task *task = new_task(DRIVE);
    if (phase == NEW || phase == PAUSED || phase == CLOSED) return finish(task);
    return pump(task);
}

WIT(_callback_code_t) HANDLER(_drive_callback)(WIT(_event_t) *event) {
    return callback(event);
}
