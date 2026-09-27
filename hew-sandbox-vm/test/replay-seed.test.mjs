import assert from "node:assert/strict";
import test from "node:test";
import { SeededPrng } from "../dist/scheduler/prng.js";

test("adjacent full-width seeds use distinct scheduler streams", () => {
  const lower = new SeededPrng("9007199254740992");
  const higher = new SeededPrng("9007199254740993");
  const lowerPicks = Array.from({ length: 8 }, () => lower.nextUint32());
  const higherPicks = Array.from({ length: 8 }, () => higher.nextUint32());
  assert.notDeepEqual(lowerPicks, higherPicks);
});
