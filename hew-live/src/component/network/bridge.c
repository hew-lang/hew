// SDK callbacks connect the ordinary typed node service to its component host.
#include "hew.h"
#include "hew_host.h"
#include <assert.h>

typedef struct LiveControlCallbacks {
    void *context;
    void (*prepare)(void *, const HewBytes *, void *, void *, void *);
    void (*replace)(void *, const HewText *, uint64_t, uint64_t, void *, void *, void *, void *, void *);
    void (*status)(void *, void *, void *, void *, void *);
    void (*route)(void *, const HewText *, const HewText *, void *, void *, void *);
} LiveControlCallbacks;

static LiveControlCallbacks callbacks;

void live_control_install(const LiveControlCallbacks *value) {
    assert(value != NULL && callbacks.context == NULL);
    callbacks = *value;
}

void live_control_uninstall(void) {
    callbacks = (LiveControlCallbacks){0};
}

void live_control_prepare(const HewBytes *artifact, void *status, void *digest, void *error) {
    callbacks.prepare(callbacks.context, artifact, status, digest, error);
}

void live_control_replace(const HewText *digest, uint64_t predecessor, uint64_t within_ms,
                          void *status, void *epoch, void *identity, void *accepted, void *error) {
    callbacks.replace(callbacks.context, digest, predecessor, within_ms, status, epoch, identity, accepted, error);
}

void live_control_status(void *status, void *epoch, void *digest, void *error) {
    callbacks.status(callbacks.context, status, epoch, digest, error);
}

void live_control_route(const HewText *source, const HewText *body, void *status, void *accepted, void *error) {
    callbacks.route(callbacks.context, source, body, status, accepted, error);
}
