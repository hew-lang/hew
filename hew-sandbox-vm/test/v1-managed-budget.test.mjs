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
import { Pipes } from "../dist/interpreter/v1/pipes.js";

const rows = [
  { id: 0, slot_bytes: "8", kind: "scalar" },
  { id: 1, slot_bytes: "8", kind: "string" },
  { id: 2, slot_bytes: "8", kind: "bytes" },
  { id: 3, slot_bytes: "8", kind: "vector", element: 0, share_on_copy: true },
];
const integer = (value) => ({ kind: "i64", value: BigInt(value) });

test("unbounded descriptor propagation does not acquire opaque payloads", () => {
  const layouts = new ManagedLayouts(
    [
      ...rows,
      {
        id: 4,
        slot_bytes: "8",
        kind: "provider",
        provider: "opaque",
        children: [],
      },
      { id: 5, slot_bytes: "16", kind: "aggregate", fields: [3, 4] },
    ],
    () => {
      throw new HeapExceeded();
    },
  );
  const values = { kind: "vector", elementType: "", items: [integer(7)] };
  const foreign = { kind: "record", typeId: "foreign", fields: [] };
  const value = {
    kind: "record",
    typeId: "wrapper",
    fields: [values, foreign],
  };
  layouts.stampGraph({ value, type: 5 });
  assert.equal(layouts.type(values), 3);
  assert.equal(layouts.type(values.items[0]), 0);
  assert.equal(vectorStorage(values).shareOnCopy, true);
  const bounded = new ManagedBudget(layouts, 64n);
  assert.throws(() => bounded.retain({ value, type: 5 }), HeapExceeded);
  assert.equal(bounded.liveBytes, 0n);
});

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

test("each retain of one ownership root requires its matching release", () => {
  const root = { value: stringValue("abc"), type: 1, slots: true };
  const budget = new ManagedBudget(new ManagedLayouts(rows), 11n);
  const first = budget.retain(root);
  const second = budget.retain(root);
  first.release();
  first.release();
  assert.equal(budget.liveBytes, 11n);
  second.release();
  assert.equal(budget.liveBytes, 0n);
});

test("retaining and releasing a live backing alias never revisits its elements", () => {
  const value = { kind: "vector", elementType: "", items: [integer(1)] };
  const backing = vectorStorage(value);
  const budget = new ManagedBudget(new ManagedLayouts(rows), 8n);
  const first = budget.retain({ value, type: 3 });
  Object.defineProperty(backing, "items", {
    get() {
      assert.fail("an existing backing owns its already-acquired children");
    },
  });
  const second = budget.retain({ value: { ...value }, type: 3 });
  first.release();
  assert.equal(budget.liveBytes, 8n);
  second.release();
  assert.equal(budget.liveBytes, 0n);
});

test("final creator membership release retires a cyclic typed backing graph", () => {
  const layouts = new ManagedLayouts([
    ...rows,
    { id: 4, slot_bytes: "16", kind: "aggregate", fields: [1, 5] },
    { id: 5, slot_bytes: "8", kind: "shared", payload: 4, weak: false },
  ]);
  const cell = {
    value: null,
    refs: 2,
    weak: 0,
    dying: false,
    owner: null,
    typeName: "",
  };
  const value = { kind: "rc", cell, closed: false };
  cell.value = {
    kind: "record",
    typeId: "",
    fields: [stringValue("abc"), value],
  };
  const budget = new ManagedBudget(layouts, 19n);
  const creator = budget.retain({ value, type: 5 });
  const alias = budget.retain({ value: { ...value }, type: 5 });
  alias.release();
  assert.equal(budget.liveBytes, 19n);
  creator.release();
  assert.equal(budget.liveBytes, 0n);
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

test("managed stream admission faults the recipient without consuming refused input", () => {
  const pipes = new Pipes();
  const pair = pipes.create("quota", 3);
  const sink = pipes.extract(pair, "sink");
  const stream = pipes.extract(pair, "stream");
  const budget = new ManagedBudget(new ManagedLayouts(rows), 18n);
  let faults = 0;
  pipes.bind(stream, {
    owner: budget,
    acquire: (item) => {
      const lease = budget.retain({
        value: item.value,
        type: 1,
        slots: true,
        identity: item,
      });
      return () => lease.release();
    },
    refused: () => {
      faults++;
    },
  });
  const statuses = [];
  const accepted = stringValue("a");
  const second = stringValue("b");
  const refused = stringValue("c");
  pipes.send(sink, accepted, false, (status) => statuses.push(status));
  pipes.send(sink, second, false, (status) => statuses.push(status));
  pipes.send(sink, refused, false, (status) => statuses.push(status));
  assert.deepEqual(statuses, [0, 0, 1]);
  assert.equal(faults, 1);
  assert.equal(refused.value, "c");
  assert.equal(budget.liveBytes, 18n);
  assert.deepEqual(pipes.close(stream), [accepted, second]);
  // Payloads stay owned until their actual asynchronous drain completes.
  assert.equal(budget.liveBytes, 18n);
  pipes.finishClose(stream);
  assert.equal(budget.liveBytes, 0n);
});

test("managed stream waiting admission keeps producer ownership until a slot opens", () => {
  const pipes = new Pipes();
  const pair = pipes.create("waiting", 1);
  const sink = pipes.extract(pair, "sink");
  const stream = pipes.extract(pair, "stream");
  const budget = new ManagedBudget(new ManagedLayouts(rows), 18n);
  let acquisitions = 0;
  pipes.bind(stream, {
    owner: budget,
    acquire: (item) => {
      acquisitions++;
      const lease = budget.retain({
        value: item.value,
        type: 1,
        slots: true,
        identity: item,
      });
      return () => lease.release();
    },
    refused: () => assert.fail("admitted values fit the receiver"),
  });
  const statuses = [];
  pipes.send(sink, stringValue("a"), true, (status) => statuses.push(status));
  pipes.send(sink, stringValue("b"), true, (status) => statuses.push(status));
  assert.deepEqual(statuses, [0]);
  assert.equal(acquisitions, 1);
  assert.equal(budget.liveBytes, 9n);
  let release;
  pipes.receive(stream, false, (value, fault, released) => {
    assert.equal(value.value, "a");
    assert.equal(fault, null);
    release = released;
  });
  assert.deepEqual(statuses, [0, 0]);
  assert.equal(acquisitions, 2);
  assert.equal(budget.liveBytes, 18n);
  release();
  assert.equal(budget.liveBytes, 9n);
  pipes.close(stream);
  pipes.finishClose(stream);
  assert.equal(budget.liveBytes, 0n);
});
