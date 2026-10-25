import assert from "node:assert/strict";
import test from "node:test";
import {
  ManagedBudget,
  ManagedLayouts,
  HeapExceeded,
} from "../dist/interpreter/v1/managed-budget.js";
import {
  byteVectorValue,
  cloneValue,
  stringValue,
  vectorStorage,
} from "../dist/interpreter/values.js";

const rows = [
  { id: 0, slot_bytes: "8", kind: "scalar" },
  { id: 1, slot_bytes: "8", kind: "string" },
  { id: 2, slot_bytes: "8", kind: "bytes" },
  { id: 3, slot_bytes: "8", kind: "vector", element: 0, share_on_copy: true },
];
const integer = (value) => ({ kind: "i64", value: BigInt(value) });

test("managed aliases charge backing once per retaining incarnation", () => {
  const layouts = new ManagedLayouts(rows);
  const source = new ManagedBudget(layouts, 3n);
  const recipient = new ManagedBudget(layouts, 3n);
  const value = stringValue("abc");
  const first = source.retain({ value, type: 1 });
  const alias = source.retain({ value: cloneValue(value), type: 1 });
  const destination = recipient.retain({ value: cloneValue(value), type: 1 });
  assert.equal(source.liveBytes, 3n);
  assert.equal(recipient.liveBytes, 3n);
  first.release();
  assert.equal(source.liveBytes, 3n);
  alias.release();
  assert.equal(source.liveBytes, 0n);
  assert.equal(recipient.liveBytes, 3n);
  destination.release();
  assert.equal(recipient.liveBytes, 0n);
});

test("managed recipient acquisition rolls back the complete typed graph", () => {
  const layouts = new ManagedLayouts(rows);
  const recipient = new ManagedBudget(layouts, 18n);
  const live = recipient.retain({ value: stringValue("ok"), type: 1 });
  assert.throws(
    () =>
      recipient.retainMany([
        { value: stringValue("a"), type: 1 },
        { value: byteVectorValue([integer(1)]), type: 2 },
      ]),
    HeapExceeded,
  );
  assert.equal(recipient.liveBytes, 2n);
  live.release();
  assert.equal(recipient.liveBytes, 0n);
});

test("zero permits primitive SSA values but refuses a stored scalar slot", () => {
  const budget = new ManagedBudget(new ManagedLayouts(rows), 0n);
  const scalar = budget.retain({ value: integer(7), type: 0 });
  assert.equal(budget.liveBytes, 0n);
  assert.throws(
    () => budget.retain({ value: integer(7), type: 0, slots: true }),
    HeapExceeded,
  );
  scalar.release();
  assert.equal(budget.liveBytes, 0n);
});

test("managed capacity multiplication refuses u64 overflow without acquisition", () => {
  const layouts = new ManagedLayouts([
    { id: 0, slot_bytes: "18446744073709551615", kind: "scalar" },
    { id: 1, slot_bytes: "8", kind: "vector", element: 0, share_on_copy: true },
  ]);
  const budget = new ManagedBudget(layouts, 0xffffffffffffffffn);
  const value = {
    kind: "vector",
    elementType: "",
    items: [integer(1), integer(2)],
  };
  vectorStorage(value);
  assert.throws(() => budget.retain({ value, type: 1 }), HeapExceeded);
  assert.equal(budget.liveBytes, 0n);
});
