import assert from "node:assert/strict";
import fs from "node:fs";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

// Independently stored compiler output exercises the full loader, not a
// package assembled to match the validator's implementation.
const original = JSON.parse(
  fs.readFileSync(
    new URL("../fixtures/01-hello-world/bytecode.json", import.meta.url),
    "utf8",
  ),
);

function rejected(pkg) {
  const trace = runBytecode(pkg);
  assert.equal(trace.result, "sandbox_rejected");
  assert.equal(
    trace.final_state.sandbox_rejections[0].category,
    "invalid_package",
  );
  assert.equal(trace.final_state.step_count, 0);
  assert.deepEqual(trace.final_state.stdout, []);
  return trace;
}

test("complete package validation rejects malformed tables before execution", () => {
  const baseline = runBytecode(original);
  assert.equal(baseline.result, "ok");
  assert.ok(baseline.final_state.stdout.join("").length > 0);
  for (const mutate of [
    (pkg) => {
      pkg.actors = [null];
    },
    (pkg) => {
      pkg.functions[0].params = [null];
    },
    (pkg) => {
      pkg.resources = [{ kind: "record", shape: 0 }];
    },
    (pkg) => {
      pkg.structural_render = [{ kind: "display" }];
    },
  ]) {
    const pkg = structuredClone(original);
    mutate(pkg);
    const trace = rejected(pkg);
    if (pkg.actors?.[0] === null)
      assert.match(
        trace.final_state.sandbox_rejections[0].message,
        /\/actors\/0.*object/,
      );
  }
});

test("invalid references, edge arguments and manifests fail before the first step", () => {
  for (const mutate of [
    (pkg) => {
      pkg.entry.function = pkg.functions.length;
    },
    (pkg) => {
      pkg.functions.push(structuredClone(pkg.functions[0]));
    },
    (pkg) => {
      pkg.functions[0].blocks[0].term.resumes[0].to = 999;
    },
    (pkg) => {
      pkg.functions[0].blocks[0].term.resumes[0].args = [0];
    },
    (pkg) => {
      const op = pkg.functions[0].blocks
        .flatMap((block) => block.ops)
        .find((op) => op.op === "const.str");
      op.str = pkg.strings.length;
    },
    (pkg) => {
      pkg.suspend_kinds = [];
    },
    (pkg) => {
      pkg.functions[0].places = [
        { id: 0, origin: "aggregate", base: { place: 0 }, field: 0 },
      ];
    },
  ]) {
    const pkg = structuredClone(original);
    mutate(pkg);
    rejected(pkg);
  }
});

test("codec string payloads and their references are checked at load", () => {
  for (const shape of [
    "not JSON",
    JSON.stringify({
      root: 0,
      nodes: [null],
      names: [""],
      aggregates: [null],
      variants: [null],
      grouped_payloads: [false],
    }),
    JSON.stringify({
      root: 0,
      nodes: [{ Sequence: 1 }],
      names: [""],
      aggregates: [null],
      variants: [null],
      grouped_payloads: [false],
    }),
  ]) {
    const pkg = structuredClone(original);
    pkg.codecs = [shape];
    rejected(pkg);
  }
});
