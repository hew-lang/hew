import assert from "node:assert/strict";
import test from "node:test";
import {
  byteVectorValue,
  clearByteVector,
  cloneValue,
  mutableVector,
  releaseVector,
  sliceByteVector,
  stringValue,
  vectorStorage,
} from "../dist/interpreter/values.js";

const int = (value) => ({ kind: "i64", value: BigInt(value) });
const vector = (items) => ({ kind: "vector", elementType: "", items });

test("plain vectors retain capacity until a copy detaches for mutation", () => {
  const first = vector([int(1), int(2), int(3)]);
  const backing = vectorStorage(first);
  backing.shareOnCopy = true;
  mutableVector(first, 4).items.push(int(4));
  assert.equal(backing.capacity, 6);
  const copy = cloneValue(first);
  assert.equal(vectorStorage(copy), backing);
  mutableVector(copy).items[0] = int(9);
  assert.equal(first.items[0].value, 1n);
  assert.equal(copy.items[0].value, 9n);
  assert.equal(vectorStorage(copy).capacity, 6);
  assert.deepEqual(
    releaseVector(first).map((item) => item.value),
    [1n, 2n, 3n, 4n],
  );
  assert.equal(copy.items[0].value, 9n);
});

test("managed vectors copy occupied elements into exact-length storage", () => {
  const first = vector([stringValue("before")]);
  mutableVector(first, 4);
  const copy = cloneValue(first);
  assert.notEqual(vectorStorage(copy), vectorStorage(first));
  assert.equal(vectorStorage(copy).capacity, 1);
  copy.items[0].append(stringValue(" after"));
  assert.equal(first.items[0].value, "before");
  assert.equal(copy.items[0].value, "before after");
  releaseVector(first).forEach((value) => value.release());
  assert.equal(copy.items[0].value, "before after");
});

test("a bytes slice retains full storage and mutation copies only its view", () => {
  const first = byteVectorValue(
    Array.from({ length: 40 }, (_, index) => int(index)),
  );
  const slice = sliceByteVector(first, 10, 12);
  assert.equal(vectorStorage(slice), vectorStorage(first));
  assert.equal(vectorStorage(slice).capacity, 40);
  mutableVector(slice, 3).items.push(int(99));
  assert.deepEqual(
    slice.items.map((value) => value.value),
    [10n, 11n, 99n],
  );
  assert.equal(vectorStorage(slice).capacity, 16);
  assert.equal(first.items[12].value, 12n);
  clearByteVector(first);
  assert.equal(vectorStorage(first).capacity, 0);
  assert.deepEqual(
    slice.items.map((value) => value.value),
    [10n, 11n, 99n],
  );
});

test("empty bytes slices release spare capacity while empty clones retain it", () => {
  const first = byteVectorValue([int(1)]);
  first.items.pop();
  const copy = cloneValue(first);
  assert.equal(vectorStorage(copy), vectorStorage(first));
  const empty = sliceByteVector(first, 0, 0);
  assert.equal(vectorStorage(empty).capacity, 0);
  clearByteVector(first);
  mutableVector(copy, 1).items.push(int(2));
  assert.equal(vectorStorage(copy).capacity, 16);
  assert.equal(copy.items[0].value, 2n);
});
