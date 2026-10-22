import assert from "node:assert/strict";
import test from "node:test";
import { cloneValue, stringValue } from "../dist/interpreter/values.js";
import { executeJson } from "../dist/interpreter/v1/json-values.js";
import {
  resolveRuntime,
  registrationRefusal,
} from "../dist/interpreter/v1/shims.js";

const int = (value) => ({ kind: "i64", value: BigInt(value) });
const call = (op, ...args) => executeJson({}, op, args);

test("JSON owners copy independently and insertion transfers only the child owner", () => {
  const original = call("ObjectNew");
  const child = call("FromU64", int((1n << 64n) - 1n));
  const retained = cloneValue(child);
  assert.equal(call("ObjectSet", original, stringValue("n"), child), original);
  assert.equal(child.value, null);
  const copy = cloneValue(original);
  call("ObjectSet", copy, stringValue("n"), call("FromInt", int(2)));
  const extracted = call("GetField", original, stringValue("n"));
  call("Free", original);
  assert.equal(call("GetU64", extracted).value, (1n << 64n) - 1n);
  assert.equal(call("GetU64", retained).value, (1n << 64n) - 1n);
  assert.equal(call("IntStatus", extracted).value, 0n);
  assert.equal(
    call("GetInt", call("GetField", copy, stringValue("n"))).value,
    2n,
  );
});

test("JSON invalid handles differ from live null and rejected insertion consumes its child", () => {
  const liveNull = call("FromNull");
  const missing = call("GetField", call("ObjectNew"), stringValue("absent"));
  assert.equal(call("Type", liveNull).value, 0n);
  assert.equal(call("Type", missing).value, -1n);
  assert.equal(call("Eq", missing, missing).value, 0n);
  const child = call("ArrayNew");
  assert.equal(call("ArrayPush", liveNull, child), liveNull);
  assert.equal(child.value, null);
  assert.equal(call("Type", liveNull).value, 0n);
  const nan = call("FromFloat", { kind: "f64", value: NaN });
  assert.equal(call("Type", nan).value, -1n);
});

test("JSON text requires the codec bridge while other formats remain refused", () => {
  const registration = (format, op) =>
    resolveRuntime({ family: "Encoding", detail: { format, op } });
  assert.equal(
    registrationRefusal(registration("Json", "Parse"), {}).capability,
    "codecBridge",
  );
  assert.equal(
    registrationRefusal(registration("Json", "ObjectNew"), {}),
    null,
  );
  for (const format of ["Yaml", "Toml"])
    assert.equal(registration(format, "Parse").kind, "refused");
});
