import assert from "node:assert/strict";
import test from "node:test";
import { ActorWaits } from "../dist/interpreter/v1/actor-waits.js";

test("strict waits detect rings only when every select alternative is blocked", () => {
  const waits = new ActorWaits();
  assert.equal(waits.pending("a", ["b", "outside"]), null);
  assert.equal(waits.pending("b", ["a"]), null);
  assert.deepEqual(waits.pending("a", ["b"]), ["a", "b", "a"]);
});

test("readiness removes dependencies before another actor can park", () => {
  const waits = new ActorWaits();
  assert.equal(waits.pending("a", ["b"]), null);
  waits.ready("a");
  assert.equal(waits.pending("b", ["a"]), null);
});

test("replacement incarnations cannot close an earlier incarnation's ring", () => {
  const waits = new ActorWaits();
  const first = {}, second = {}, replacement = {};
  assert.equal(waits.pending(first, [second]), null);
  assert.equal(waits.pending(second, [replacement]), null);
});
