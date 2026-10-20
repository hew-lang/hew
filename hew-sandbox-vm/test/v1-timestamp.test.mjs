import assert from "node:assert/strict";
import test from "node:test";
import { timestampBinary } from "../dist/interpreter/v1/timestamp.js";

const term = {
  op: "timestamp.binary",
  binary_op: "Add",
  lhs_timestamp: true,
  rhs_timestamp: false,
  shape: 0,
};
const duration = (value) => ({ kind: "i64", value });
const value = (seconds, nanos, shape = 0) => ({
  kind: "record", typeId: "timestamp", shape,
  fields: [duration(seconds), duration(nanos)],
});
const pkg = { aggregates: [
  { id: 0, name: "timestamp", fields: ["seconds", "nanoseconds"], builtin: "Timestamp" },
  { id: 1, name: "timestamp", fields: ["seconds", "nanoseconds"] },
] };

test("timestamp operands require canonical shape identity and normalized fields", () => {
  for (const forged of [
    value(0n, 0n, 1), value(0n, 0n, 99), value(0n, -1n),
    value(0n, 1_000_000_000n), value(1n << 63n, 0n),
  ]) {
    assert.throws(() => timestampBinary(pkg, term, forged, duration(1n)), TypeError);
  }
});

test("timestamp overflow is a checked result at both seconds boundaries", () => {
  const minimum = -(1n << 63n);
  const maximum = (1n << 63n) - 1n;
  assert.equal(timestampBinary(pkg, term, value(minimum, 0n), duration(-1n)), null);
  assert.equal(timestampBinary(pkg, term, value(maximum, 999_999_999n), duration(1n)), null);
  assert.equal(timestampBinary(pkg, {
    ...term, binary_op: "Subtract", rhs_timestamp: true, shape: null,
  }, value(maximum, 0n), value(minimum, 0n)), null);
});
