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

async function rejected(pkg) {
  const trace = await runBytecode(pkg);
  assert.equal(trace.result, "sandbox_rejected");
  assert.equal(
    trace.final_state.sandbox_rejections[0].category,
    "invalid_package",
  );
  assert.equal(trace.final_state.step_count, 0);
  assert.deepEqual(trace.final_state.stdout, []);
  return trace;
}

test("complete package validation rejects malformed tables before execution", async () => {
  const baseline = await runBytecode(original);
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
    const trace = await rejected(pkg);
    if (pkg.actors?.[0] === null)
      assert.match(
        trace.final_state.sandbox_rejections[0].message,
        /\/actors\/0.*object/,
      );
  }
});

test("invalid references, edge arguments and manifests fail before the first step", async () => {
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
    await rejected(pkg);
  }
});

test("codec string payloads and their references are checked at load", async () => {
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
    await rejected(pkg);
  }
});

test("negative supervisor stop deadlines reject before construction or host effects", async () => {
  const pkg = JSON.parse(
    fs.readFileSync(
      new URL(
        "../fixtures/19-supervisor-restart/bytecode.json",
        import.meta.url,
      ),
      "utf8",
    ),
  );
  assert.equal(
    (await runBytecode(pkg)).result,
    "ok",
    "independent supervisor package executes",
  );
  pkg.supervisors[0].children[0].stop_ns = -1;
  let calls = 0;
  const bridge = () => {
    calls++;
    throw new Error("malformed package reached a bridge");
  };
  const trace = await runBytecode(pkg, {
    codecBridge: bridge,
    calendarBridge: bridge,
  });
  assert.equal(trace.result, "sandbox_rejected");
  assert.equal(
    trace.final_state.sandbox_rejections[0].category,
    "invalid_package",
  );
  assert.match(
    trace.final_state.sandbox_rejections[0].message,
    /\/supervisors\/0\/children\/0\/stop_ns/,
  );
  assert.equal(trace.final_state.step_count, 0);
  assert.deepEqual(trace.final_state.stdout, []);
  assert.equal(calls, 0);
});

test("unknown actor and supervisor policies reject before execution", async () => {
  const supervisor = JSON.parse(
    fs.readFileSync(
      new URL(
        "../fixtures/19-supervisor-restart/bytecode.json",
        import.meta.url,
      ),
      "utf8",
    ),
  );
  assert.equal((await runBytecode(supervisor)).result, "ok");
  for (const mutate of [
    (pkg) => {
      pkg.actors[0].overflow = "invent_overflow";
    },
    (pkg) => {
      pkg.supervisors[0].strategy = "invent_strategy";
    },
    (pkg) => {
      pkg.supervisors[0].children[0].restart = "invent_restart";
    },
  ]) {
    const pkg = structuredClone(supervisor);
    mutate(pkg);
    await rejected(pkg);
  }
  const counter = JSON.parse(
    fs.readFileSync(
      new URL("../fixtures/15-actor-counter/bytecode.json", import.meta.url),
      "utf8",
    ),
  );
  counter.actors[0].overflow = "coalesce";
  counter.actors[0].coalesce = {
    fallback: "drop_new",
    keys: [{ message: 0, param: 0 }],
  };
  assert.equal(
    (await runBytecode(counter)).result,
    "ok",
    "the recorded actor executes with a valid coalescing key",
  );
  counter.actors[0].coalesce.fallback = "invent_fallback";
  await rejected(counter);
});
