#include "telemetry.h"

#define WIT_WORLD telemetry
#define WIT_WORLD_UPPER TELEMETRY
#define WIT_HANDLER exports_hew_live_telemetry_handler
#define WIT_INVOKE _observe
#define WIT_INVOKE_CALLBACK _observe_callback
#define WIT_INVOKE_RETURN _observe_return
#define GUEST_NAME "telemetry monitor"

typedef HewOwned_Monitor GuestState;
typedef HANDLER(_snapshot_t) GuestSnapshot;
typedef HANDLER(_sample_t) WireInput;
typedef HANDLER(_result_observation_string_t) WireReply;
typedef HANDLER(_observation_t) GuestReply;
typedef WireInput GuestInput;

static GuestInput acquire_input(const WireInput *input) { return *input; }
static void release_message(WireInput *input) { (void)input; }
static void release_input(GuestInput *input) { (void)input; }

static int32_t initialize(const GuestSnapshot *snapshot, GuestState **state, HewError **error) {
    return guest_create(snapshot->accepted, snapshot->previous, snapshot->last, state, error);
}

static int32_t invoke_start(const GuestState *state, const GuestInput *input,
                            const HewWaker *waker, HewCall_guest_route **call, HewError **error) {
    return guest_route_start(state, input->sequence, input->vibration, waker, call, error);
}

// Checked exports keep records opaque. Read their scalar fields before releasing
// the owner, and preserve any primary failure through release.
static int32_t settle_owner(int32_t status, HewError **error,
                            int32_t cleanup_status, HewError *cleanup_error) {
    if (status == 0 && cleanup_status != 0) {
        *error = cleanup_error;
        return cleanup_status;
    }
    hew_host_error_release(cleanup_error);
    return status;
}

static int32_t invoke_result(HewCall_guest_route *call, GuestReply *reply, HewError **error) {
    HewOwned_Observation *owner = NULL;
    int32_t status = guest_route_result(call, &owner, error);
    if (status != 0) return status;
    uint8_t ready = 0, anomaly = 0;
    if (status == 0) status = guest_baseline(owner, &reply->baseline, error);
    if (status == 0) status = guest_ready(owner, &ready, error);
    if (status == 0) status = guest_anomaly(owner, &anomaly, error);
    reply->ready = ready != 0;
    reply->anomaly = anomaly != 0;
    HewError *cleanup = NULL;
    int32_t released = HewOwned_Observation_release(&owner, &cleanup);
    return settle_owner(status, error, released, cleanup);
}

static int32_t snapshot_result(HewCall_guest_snapshot *call, GuestSnapshot *snapshot, HewError **error) {
    HewOwned_Snapshot *owner = NULL;
    int32_t status = guest_snapshot_result(call, &owner, error);
    if (status != 0) return status;
    if (status == 0) status = guest_accepted(owner, &snapshot->accepted, error);
    if (status == 0) status = guest_previous(owner, &snapshot->previous, error);
    if (status == 0) status = guest_last(owner, &snapshot->last, error);
    HewError *cleanup = NULL;
    int32_t released = HewOwned_Snapshot_release(&owner, &cleanup);
    return settle_owner(status, error, released, cleanup);
}
