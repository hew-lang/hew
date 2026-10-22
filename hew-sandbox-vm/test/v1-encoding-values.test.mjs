import assert from "node:assert/strict";
import test from "node:test";
import { cloneValue, stringValue } from "../dist/interpreter/values.js";
import { executeEncoding } from "../dist/interpreter/v1/encoding-values.js";
import {
  resolveRuntime,
  registrationRefusal,
} from "../dist/interpreter/v1/shims.js";

const int = (value) => ({ kind: "i64", value: BigInt(value) });
const call = (op, ...args) => executeEncoding({}, "Json", op, args);

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

test("each Encoding format requires the text bridge and refuses only its absent operations", () => {
  const registration = (format, op) =>
    resolveRuntime({ family: "Encoding", detail: { format, op } });
  for (const format of ["Json", "Yaml", "Toml"]) {
    assert.equal(
      registrationRefusal(registration(format, "Parse"), {}).capability,
      "codecBridge",
    );
    assert.equal(
      registrationRefusal(registration(format, "ObjectNew"), {}),
      null,
    );
  }
  for (const format of ["Json", "Yaml"])
    assert.equal(registration(format, "GetDatetime").kind, "refused");
  for (const op of ["GetU64", "IntStatus", "FromU64", "FromNull"])
    assert.equal(registration("Toml", op).kind, "refused");
});

for (const format of ["Json", "Yaml", "Toml"]) {
  test(`${format} copies, extraction and rejected insertion preserve independent owners`, () => {
    const run = (op, ...args) => executeEncoding({}, format, op, args);
    const parent = run("ObjectNew");
    const child = run("FromInt", int(1));
    const retained = cloneValue(child);
    run("ObjectSet", parent, stringValue("n"), child);
    assert.equal(child.value, null);
    const copy = cloneValue(parent);
    run("ObjectSet", copy, stringValue("n"), run("FromInt", int(2)));
    const extracted = run("GetField", parent, stringValue("n"));
    run("Free", parent);
    assert.equal(run("GetInt", extracted).value, 1n);
    assert.equal(run("GetInt", retained).value, 1n);
    assert.equal(
      run("GetInt", run("GetField", copy, stringValue("n"))).value,
      2n,
    );
    run("ArrayPush", retained, extracted);
    assert.equal(extracted.value, null);
    assert.equal(run("GetInt", retained).value, 1n);
    assert.throws(
      () =>
        executeEncoding({}, format === "Yaml" ? "Json" : "Yaml", "Type", [
          retained,
        ]),
      TypeError,
    );
  });
}

test("format-specific nonfinite equality preserves YAML and TOML semantics", () => {
  for (const format of ["Yaml", "Toml"]) {
    const nan = executeEncoding({}, format, "FromFloat", [
      { kind: "f64", value: NaN },
    ]);
    assert.equal(executeEncoding({}, format, "Type", [nan]).value, 3n);
    assert.equal(
      executeEncoding({}, format, "Eq", [nan, cloneValue(nan)]).value,
      format === "Yaml" ? 1n : 0n,
    );
  }
});
