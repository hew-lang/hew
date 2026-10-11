import assert from "node:assert/strict";
import {
  decodeMetricsEnvelope,
  decodeCrashesEnvelope,
  decodeTracesEnvelope,
  OBSERVE_SCHEMA_VERSION,
} from "../generated/observe-protocol-v1.ts";

const max = "18446744073709551615";
const metricsData = (timestamp = max) => `{
  "timestamp_secs":"${timestamp}","tasks_spawned":"${max}","tasks_completed":"${max}",
  "steals":"${max}","messages_sent":"${max}","messages_received":"${max}",
  "active_workers":"${max}","alloc_count":"${max}","dealloc_count":"${max}",
  "bytes_allocated":"${max}","bytes_freed":"${max}","bytes_live":"${max}",
  "peak_bytes_live":"${max}","tcp_bytes_read":"${max}","tcp_bytes_written":"${max}",
  "tcp_accept_count":"${max}","tcp_connect_count":"${max}","tcp_error_count":"${max}"
}`;

const metrics = decodeMetricsEnvelope(`{
  "schema_version":"${OBSERVE_SCHEMA_VERSION}",
  "data":${metricsData()}
}`);
assert.equal(metrics.data.timestamp_secs, 18446744073709551615n);
assert.equal(metrics.data.bytes_live, 18446744073709551615n);

const additive = decodeMetricsEnvelope(`{"schema_version":"v1","data":${metricsData().replace(/}$/, ',"future_property":true}')},"future_envelope_property":true}`);
assert.equal(additive.data.future_property, true);
assert.equal(additive.future_envelope_property, true);

const traces = decodeTracesEnvelope(`{
  "schema_version":"v1",
  "data":[{
    "trace_id":"0123456789abcdef0123456789abcdef",
    "span_id":"${max}","parent_span_id":"0","actor_id":"${max}","actor_type_id":"0",
    "actor_type":null,"event_type":"send","msg_type":-2147483648,
    "timestamp_ns":"${max}","handler_name":null
  }]
}`);
assert.equal(traces.data[0].actor_id, 18446744073709551615n);

const crashes = decodeCrashesEnvelope('{"schema_version":"v1","data":[{"time_s":1.5,"actor_id":"18446744073709551615","signal":202,"trap_kind":"Unknown","msg_type":-1,"fault_addr":"0"}]}');
assert.equal(crashes.data[0].time_s, 1.5);

const integerCrashTime = decodeCrashesEnvelope('{"schema_version":"v1","data":[{"time_s":0,"actor_id":"0","signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":"0"}]}');
assert.equal(integerCrashTime.data[0].time_s, 0);
const exponentCrashTime = decodeCrashesEnvelope('{"schema_version":"v1","data":[{"time_s":-1.25e2,"actor_id":"0","signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":"0"}]}');
assert.equal(exponentCrashTime.data[0].time_s, -125);

assert.throws(() => decodeMetricsEnvelope('{"schema_version":"v9","data":{}}'));
assert.throws(() => decodeTracesEnvelope('{"schema_version":"v1","data":[{}]}'));
assert.throws(() => decodeMetricsEnvelope(`{"schema_version":"v1","data":${metricsData("18446744073709551616")}}`));
assert.throws(() => decodeMetricsEnvelope(`{"schema_version":"v1","data":${metricsData("01")}}`));
assert.throws(() => decodeMetricsEnvelope(`{"schema_version":"v1","data":${metricsData("1").replace('"timestamp_secs":"1"', '"timestamp_secs":1')}}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v1","data":[{"trace_id":"0123456789abcdef0123456789abcdef","span_id":"0","parent_span_id":"0","actor_id":"0","actor_type_id":"0","actor_type":null,"event_type":"send","msg_type":2147483648,"timestamp_ns":"0","handler_name":null}]}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v1","data":[{"trace_id":"0123456789abcdef0123456789abcdef","span_id":"0","parent_span_id":"0","actor_id":"0","actor_type_id":"0","event_type":"send","msg_type":0,"timestamp_ns":"0","handler_name":null}]}`));
assert.throws(() => decodeCrashesEnvelope('{"schema_version":"v1","data":[{"time_s":1e309,"actor_id":"0","signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":"0"}]}'));

const escaped = decodeTracesEnvelope(`{"schema_version":"v1","data":[{"trace_id":"0123456789abcdef0123456789abcdef","span_id":"0","parent_span_id":"0","actor_id":"0","actor_type_id":"0","actor_type":"digits 18446744073709551616 and \\"quote\\"","event_type":"send","msg_type":-0,"timestamp_ns":"0","handler_name":null}]}`);
assert.equal(escaped.data[0].actor_type, 'digits 18446744073709551616 and "quote"');
assert.equal(escaped.data[0].msg_type, 0);
