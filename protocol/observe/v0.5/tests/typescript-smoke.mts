import assert from "node:assert/strict";
import {
  decodeMetricsEnvelope,
  decodeCrashesEnvelope,
  decodeTracesEnvelope,
  OBSERVE_SCHEMA_VERSION,
  parseLosslessJson,
} from "../generated/observe-protocol-v0.5.ts";

const max = "18446744073709551615";
const metrics = decodeMetricsEnvelope(`{
  "schema_version":"${OBSERVE_SCHEMA_VERSION}",
  "data":{
    "timestamp_secs":${max},"tasks_spawned":${max},"tasks_completed":${max},
    "steals":${max},"messages_sent":${max},"messages_received":${max},
    "active_workers":${max},"alloc_count":${max},"dealloc_count":${max},
    "bytes_allocated":${max},"bytes_freed":${max},"bytes_live":${max},
    "peak_bytes_live":${max},"tcp_bytes_read":${max},"tcp_bytes_written":${max},
    "tcp_accept_count":${max},"tcp_connect_count":${max},"tcp_error_count":${max},
    "future_property":true
  },
  "future_envelope_property":true
}`);
assert.equal(metrics.data.timestamp_secs, 18446744073709551615n);
assert.equal(metrics.data.bytes_live, 18446744073709551615n);
assert.equal(metrics.data.future_property, true);

const traces = decodeTracesEnvelope(`{
  "schema_version":"v0.5",
  "data":[{
    "trace_id":"0123456789abcdef0123456789abcdef",
    "span_id":${max},"parent_span_id":0,"actor_id":${max},"actor_type_id":0,
    "actor_type":null,"event_type":"future_event","msg_type":-2147483648,
    "timestamp_ns":${max},"handler_name":null,"future_property":"accepted"
  }]
}`);
assert.equal(traces.data[0].event_type, "future_event");
assert.equal(traces.data[0].actor_id, 18446744073709551615n);
assert.equal(traces.data[0].future_property, "accepted");

const crashes = decodeCrashesEnvelope('{"schema_version":"v0.5","data":[{"time_s":1.5,"actor_id":18446744073709551615,"signal":202,"trap_kind":"FutureTrap","msg_type":-1,"fault_addr":0}]}');
assert.equal(crashes.data[0].time_s, 1.5);
assert.equal(crashes.data[0].trap_kind, "FutureTrap");

const integerCrashTime = decodeCrashesEnvelope('{"schema_version":"v0.5","data":[{"time_s":0,"actor_id":0,"signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":0}]}');
assert.equal(integerCrashTime.data[0].time_s, 0);
const exponentCrashTime = decodeCrashesEnvelope('{"schema_version":"v0.5","data":[{"time_s":-1.25e2,"actor_id":0,"signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":0}]}');
assert.equal(exponentCrashTime.data[0].time_s, -125);

const ipcMetrics = decodeMetricsEnvelope(`{"schema_version":"v0.5","data":{"timestamp_secs":"${max}","tasks_spawned":"0","tasks_completed":"0","steals":"0","messages_sent":"0","messages_received":"0","active_workers":"0","alloc_count":"0","dealloc_count":"0","bytes_allocated":"0","bytes_freed":"0","bytes_live":"${max}","peak_bytes_live":"${max}","tcp_bytes_read":"0","tcp_bytes_written":"0","tcp_accept_count":"0","tcp_connect_count":"0","tcp_error_count":"0"}}`);
assert.equal(ipcMetrics.data.bytes_live, 18446744073709551615n);

assert.throws(() => decodeMetricsEnvelope('{"schema_version":"v9","data":{}}'));
assert.throws(() => decodeTracesEnvelope('{"schema_version":"v0.5","data":[{}]}'));
assert.throws(() => decodeMetricsEnvelope(`{"schema_version":"v0.5","data":{"timestamp_secs":"18446744073709551616","tasks_spawned":"0","tasks_completed":"0","steals":"0","messages_sent":"0","messages_received":"0","active_workers":"0","alloc_count":"0","dealloc_count":"0","bytes_allocated":"0","bytes_freed":"0","bytes_live":"0","peak_bytes_live":"0","tcp_bytes_read":"0","tcp_bytes_written":"0","tcp_accept_count":"0","tcp_connect_count":"0","tcp_error_count":"0"}}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v0.5","data":[{"trace_id":"0123456789abcdef0123456789abcdef","span_id":0,"parent_span_id":0,"actor_id":0,"actor_type_id":0,"actor_type":null,"event_type":"send","msg_type":2147483648,"timestamp_ns":0,"handler_name":null}]}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v0.5","data":[{"trace_id":"0123456789abcdef0123456789abcdef","span_id":0,"parent_span_id":0,"actor_id":0,"actor_type_id":0,"event_type":"send","msg_type":0,"timestamp_ns":0,"handler_name":null}]}`));
assert.throws(() => decodeCrashesEnvelope('{"schema_version":"v0.5","data":[{"time_s":1e309,"actor_id":0,"signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":0}]}'));
assert.throws(() => parseLosslessJson('{"invalid":01}'));
assert.throws(() => decodeMetricsEnvelope('{"schema_version":"v0.5","data":{"timestamp_secs":"01","tasks_spawned":"0","tasks_completed":"0","steals":"0","messages_sent":"0","messages_received":"0","active_workers":"0","alloc_count":"0","dealloc_count":"0","bytes_allocated":"0","bytes_freed":"0","bytes_live":"0","peak_bytes_live":"0","tcp_bytes_read":"0","tcp_bytes_written":"0","tcp_accept_count":"0","tcp_connect_count":"0","tcp_error_count":"0"}}'));

const escaped = decodeTracesEnvelope(`{"schema_version":"v0.5","data":[{"trace_id":"0123456789abcdef0123456789abcdef","span_id":0,"parent_span_id":0,"actor_id":0,"actor_type_id":0,"actor_type":"digits 18446744073709551616 and \\\"quote\\\"","event_type":"send","msg_type":-0,"timestamp_ns":0,"handler_name":null}]}`);
assert.equal(escaped.data[0].actor_type, 'digits 18446744073709551616 and "quote"');
assert.equal(escaped.data[0].msg_type, 0);
