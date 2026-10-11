import assert from "node:assert/strict";
import {
  decodeMetricsEnvelope,
  decodeCrashesEnvelope,
  decodeTraceStreamMismatchEnvelope,
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

const traceEvent = `{
  "sequence":"${max}","trace_id":"0123456789abcdef0123456789abcdef",
  "span_id":"${max}","parent_span_id":"0","subject":null,
  "source_attribution":"local_actor",
  "source_actor":{"actor_id":"${max}","spawn_serial":"${max}","actor_type":null},
  "destination_attribution":"remote_unknown","destination_actor":null,
  "event_type":"send","msg_type":-2147483648,
  "timestamp_ns":"${max}","handler_name":null
}`;
const traceBatch = (event = traceEvent) => `{
  "producer_instance_id":"00112233445566778899aabbccddeeff",
  "stream_id":"ffeeddccbbaa99887766554433221100",
  "oldest_sequence":"${max}","next_sequence":"${max}","next_cursor":"${max}",
  "has_more":false,"overrun":null,"tracing_enabled":true,"sampling_mode":"all",
  "events_recorded_total":"${max}","events_overwritten_total":"0","events_sampled_out_total":"0",
  "batch_source_monotonic_ns":"${max}",
  "clock_anchor":{"monotonic_ns":"${max}","unix_epoch_ns":"${max}","uncertainty_ns":"0"},
  "events":[${event}]
}`;
const traces = decodeTracesEnvelope(`{"schema_version":"v1","data":${traceBatch()}}`);
assert.equal(traces.data.next_cursor, 18446744073709551615n);
assert.equal(traces.data.events[0].source_actor?.actor_id, 18446744073709551615n);

const mismatch = decodeTraceStreamMismatchEnvelope(`{
  "schema_version":"v1",
  "data":{
    "error":"trace_stream_mismatch",
    "requested_stream_id":"00112233445566778899aabbccddeeff",
    "current_stream_id":"ffeeddccbbaa99887766554433221100",
    "current_producer_instance_id":"0123456789abcdef0123456789abcdef"
  }
}`);
assert.equal(mismatch.data.current_producer_instance_id, "0123456789abcdef0123456789abcdef");
assert.throws(() => decodeTraceStreamMismatchEnvelope(`{
  "schema_version":"v1",
  "data":{
    "error":"future_value",
    "requested_stream_id":"00112233445566778899aabbccddeeff",
    "current_stream_id":"ffeeddccbbaa99887766554433221100",
    "current_producer_instance_id":"0123456789abcdef0123456789abcdef"
  }
}`));

const crashes = decodeCrashesEnvelope('{"schema_version":"v1","data":[{"time_s":1.5,"actor_id":"18446744073709551615","signal":202,"trap_kind":"Unknown","msg_type":-1,"fault_addr":"0"}]}');
assert.equal(crashes.data[0].time_s, 1.5);

const integerCrashTime = decodeCrashesEnvelope('{"schema_version":"v1","data":[{"time_s":0,"actor_id":"0","signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":"0"}]}');
assert.equal(integerCrashTime.data[0].time_s, 0);
const exponentCrashTime = decodeCrashesEnvelope('{"schema_version":"v1","data":[{"time_s":-1.25e2,"actor_id":"0","signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":"0"}]}');
assert.equal(exponentCrashTime.data[0].time_s, -125);

assert.throws(() => decodeMetricsEnvelope('{"schema_version":"v9","data":{}}'));
assert.throws(() => decodeTracesEnvelope('{"schema_version":"v1","data":{}}'));
assert.throws(() => decodeMetricsEnvelope(`{"schema_version":"v1","data":${metricsData("18446744073709551616")}}`));
assert.throws(() => decodeMetricsEnvelope(`{"schema_version":"v1","data":${metricsData("01")}}`));
assert.throws(() => decodeMetricsEnvelope(`{"schema_version":"v1","data":${metricsData("1").replace('"timestamp_secs":"1"', '"timestamp_secs":1')}}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v1","data":${traceBatch(traceEvent.replace('"msg_type":-2147483648', '"msg_type":2147483648'))}}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v1","data":${traceBatch(traceEvent.replace('"spawn_serial":"18446744073709551615",', ''))}}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v1","data":${traceBatch(traceEvent.replace('"source_attribution":"local_actor"', '"source_attribution":"remote_unknown"'))}}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v1","data":${traceBatch(traceEvent.replace('"subject":null', '"subject":{"kind":"actor","id":"1","actor":null}'))}}`));
assert.throws(() => decodeTracesEnvelope(`{"schema_version":"v1","data":${traceBatch().replace('ffeeddccbbaa99887766554433221100', 'FFEEDDCCBBAA99887766554433221100')}}`));
assert.throws(() => decodeCrashesEnvelope('{"schema_version":"v1","data":[{"time_s":1e309,"actor_id":"0","signal":0,"trap_kind":"Normal","msg_type":0,"fault_addr":"0"}]}'));

const escapedEvent = traceEvent
  .replace('"actor_type":null', '"actor_type":"digits 18446744073709551616 and \\"quote\\""')
  .replace('"msg_type":-2147483648', '"msg_type":-0');
const escaped = decodeTracesEnvelope(`{"schema_version":"v1","data":${traceBatch(escapedEvent)}}`);
assert.equal(escaped.data.events[0].source_actor?.actor_type, 'digits 18446744073709551616 and "quote"');
assert.equal(escaped.data.events[0].msg_type, 0);
