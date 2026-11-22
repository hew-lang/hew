#include "router.h"

#define WIT_WORLD router
#define WIT_WORLD_UPPER ROUTER
#define WIT_HANDLER exports_hew_live_router_handler
#define WIT_INVOKE _route
#define WIT_INVOKE_CALLBACK _route_callback
#define WIT_INVOKE_RETURN _route_return
#define GUEST_NAME "router"

typedef HewOwned_Router GuestState;
typedef HANDLER(_snapshot_t) GuestSnapshot;
typedef HANDLER(_event_t) WireInput;
typedef HANDLER(_result_bool_string_t) WireReply;
typedef bool GuestReply;

typedef struct {
    HewText *source;
    HewText *body;
} GuestInput;

static HewText *hew_text(WIT(_string_t) text);

static WIT(_string_t) copy_text(const HewText *text) {
    size_t length;
    const unsigned char *bytes = hew_host_text_data(text, &length);
    WIT(_string_t) result;
    WIT(_string_dup_n)(&result, (const char *)bytes, length);
    return result;
}

static GuestInput acquire_input(const WireInput *input) {
    return (GuestInput) {hew_text(input->source), hew_text(input->body)};
}

static void release_message(WireInput *input) { HANDLER(_event_free)(input); }

static void release_input(GuestInput *input) {
    hew_host_text_release(input->source);
    hew_host_text_release(input->body);
}

static int32_t initialize(const GuestSnapshot *snapshot, GuestState **state, HewError **error) {
    return guest_create(snapshot->accepted, state, error);
}

static int32_t invoke_start(const GuestState *state, const GuestInput *input,
                            const HewWaker *waker, HewCall_guest_route **call, HewError **error) {
    return guest_route_start(state, input->source, input->body, waker, call, error);
}

static int32_t invoke_result(HewCall_guest_route *call, GuestReply *reply, HewError **error) {
    uint8_t accepted = 0;
    int32_t status = guest_route_result(call, &accepted, error);
    *reply = accepted != 0;
    return status;
}

static int32_t snapshot_result(HewCall_guest_snapshot *call, GuestSnapshot *snapshot, HewError **error) {
    return guest_snapshot_result(call, &snapshot->accepted, error);
}
