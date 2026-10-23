import assert from "node:assert/strict";
import test from "node:test";
import { Semaphores } from "../dist/interpreter/v1/semaphores.js";

test("a reserved permit cannot be stolen and cancellation hands it onwards", () => {
  const semaphores = new Semaphores(() => {
    throw new Error("untimed acquisition armed a timer");
  });
  const sem = semaphores.create(0n);
  const events = [];
  const first = semaphores.acquire(sem, null, (acquired) =>
    events.push([1, acquired]),
  );
  const second = semaphores.acquire(sem, null, (acquired) =>
    events.push([2, acquired]),
  );
  semaphores.release(sem);
  assert.deepEqual(events, [[1, true]]);
  assert.equal(semaphores.tryAcquire(sem), false);
  first.cancel();
  assert.deepEqual(events, [
    [1, true],
    [2, true],
  ]);
  second.take();
  second.cancel();
  assert.equal(semaphores.count(sem), 0n);
  semaphores.release(sem);
  assert.equal(semaphores.count(sem), 1n);
});

test("timeout and release choose one outcome and detach their timer", () => {
  let expire;
  let detached = 0;
  const semaphores = new Semaphores((nanos, ready) => {
    assert.equal(nanos, 5_000_000n);
    expire = ready;
    return () => detached++;
  });
  const sem = semaphores.create(0n);
  const events = [];
  semaphores.acquire(sem, 5n, (acquired) => events.push(acquired));
  semaphores.release(sem);
  expire();
  assert.deepEqual(events, [true]);
  assert.equal(detached, 1);
  assert.equal(semaphores.count(sem), 0n);
  semaphores.acquire(sem, 5n, (acquired) => events.push(acquired));
  expire();
  semaphores.release(sem);
  assert.deepEqual(events, [true, false]);
  assert.equal(detached, 2);
  assert.equal(semaphores.count(sem), 1n);
});

test("pending acquisitions retain backing after their handle closes", () => {
  const semaphores = new Semaphores(() => () => {});
  const original = semaphores.create(0n);
  const clone = semaphores.clone(original);
  let acquired = false;
  const operation = semaphores.acquire(
    original,
    null,
    (value) => (acquired = value),
  );
  semaphores.close(original);
  semaphores.release(clone);
  assert.equal(acquired, true);
  operation.cancel();
  assert.equal(semaphores.count(clone), 1n);
  assert.throws(() => semaphores.count(original), /live semaphore/);
});
