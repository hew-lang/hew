import assert from "node:assert/strict";
import test from "node:test";
import {
  encodeFixedWidthHex,
  executeConstantTimeEq,
} from "../dist/interpreter/v1/crypto.js";
import { byteVectorValue } from "../dist/interpreter/values.js";

function bytesVal(nums) {
  return byteVectorValue(nums.map((n) => ({ kind: "i64", value: BigInt(n) })));
}

test("encodeFixedWidthHex emits two lowercase hex chars for every byte value", () => {
  assert.equal(encodeFixedWidthHex([]), "");
  const allBytes = Array.from({ length: 256 }, (_, i) => i);
  const expected = allBytes
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
  assert.equal(encodeFixedWidthHex(allBytes), expected);
  assert.equal(encodeFixedWidthHex(allBytes).length, 512);
});

test("executeConstantTimeEq uses fixed-width hex transport independent of byte values", () => {
  const requests = [];
  const bridge = (request) => {
    requests.push(request);
    const parsed = JSON.parse(request);
    assert.equal(parsed.operation, "constant_time_eq");
    assert.equal(
      parsed.args,
      undefined,
      "must not serialize decimal args array",
    );
    assert.ok(Array.isArray(parsed.args_hex) && parsed.args_hex.length === 2);
    return JSON.stringify({
      ok: true,
      equal: parsed.args_hex[0] === parsed.args_hex[1],
    });
  };

  // Equal-length inputs spanning 1-digit, 2-digit, and 3-digit decimal values
  // must produce identical serialized request lengths.
  const low = bytesVal([0, 1, 9]);
  const mid = bytesVal([10, 50, 99]);
  const high = bytesVal([100, 200, 255]);

  assert.deepEqual(executeConstantTimeEq(bridge, [low, low]), {
    kind: "i64",
    value: 1n,
  });
  assert.deepEqual(executeConstantTimeEq(bridge, [low, high]), {
    kind: "i64",
    value: 0n,
  });
  assert.deepEqual(executeConstantTimeEq(bridge, [mid, high]), {
    kind: "i64",
    value: 0n,
  });
  assert.deepEqual(executeConstantTimeEq(bridge, [high, high]), {
    kind: "i64",
    value: 1n,
  });

  const lengths = new Set(requests.map((req) => req.length));
  assert.equal(
    lengths.size,
    1,
    `expected identical payload lengths across equal-length inputs, got ${JSON.stringify(requests)}`,
  );
  assert.equal(
    requests[1],
    '{"operation":"constant_time_eq","args_hex":["000109","64c8ff"]}',
  );

  // Empty byte vectors remain valid and fixed-width.
  requests.length = 0;
  assert.deepEqual(
    executeConstantTimeEq(bridge, [bytesVal([]), bytesVal([])]),
    {
      kind: "i64",
      value: 1n,
    },
  );
  assert.equal(
    requests[0],
    '{"operation":"constant_time_eq","args_hex":["",""]}',
  );
});
