import assert from "node:assert/strict";
import test from "node:test";
import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";
import { admitPackage } from "../dist/interpreter/v1/validate.js";

const ret = { op: "return", value: null, span: null };
const edge = (to) => ({ to, args: [] });
const block = (id, ops, term = ret) => ({ id, params: [], ops, term });
const op = (op, fields) => ({ op, ...fields, span: null });
const make = (dst, str) => [
  op("const.str", { dst: dst + 1, str }),
  op("aggregate.make", { dst, shape: 0, fields: [dst + 1] }),
];
const store = (value) => op("store.assign", { place: 0, value });
const end = op("end_lifetime", { place: 0 });
const take = (dst) => op("load.take", { dst, place: 0 });
const destroy = (value) => op("destroy_value", { value });

function pkg(ops, { fault = false, projected = false } = {}) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.4",
    compiler_version: "resource-assignment-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: ["closed 1", "closed 2", "closed 3", "close failed"],
    bytes: [],
    regex_patterns: [],
    aggregates: [
      { id: 0, name: "Ticket", fields: ["label"] },
      { id: 1, name: "Holder", fields: ["ticket"] },
    ],
    variants: [],
    resources: [{ kind: "record", ty: "Ticket", shape: 0, close: 1 }],
    runtime_families: [
      { id: 0, family: "Print", detail: { kind: "Str", newline: true } },
    ],
    externs: [],
    suspend_kinds: [],
    value_capabilities: [],
    closures: [],
    vtables: [],
    functions: [
      {
        id: 0,
        name: "main",
        params: [],
        entry: 0,
        places: [
          projected
            ? {
                id: 0,
                origin: "aggregate",
                base: { value: 90 },
                shape: 1,
                field: 0,
              }
            : { id: 0, origin: "local" },
        ],
        blocks: [
          block(
            0,
            [
              ...make(0, 0),
              ...(projected
                ? [op("aggregate.make", { dst: 90, shape: 1, fields: [0] })]
                : [
                    op("alloc_place", { place: 0 }),
                    op("store.init", { place: 0, value: 0 }),
                  ]),
              ...ops,
            ],
            fault
              ? {
                  op: "cleanup.dispatch",
                  normal: edge(1),
                  fault: edge(2),
                  span: null,
                }
              : ret,
          ),
          block(1, []),
          block(2, [end], { op: "resume_unwind", span: null }),
        ],
      },
      {
        id: 1,
        name: "Ticket.close",
        params: [{ value: 0, own: "owned" }],
        entry: 0,
        places: [],
        blocks: [
          block(
            0,
            [
              op("aggregate.project_borrow", {
                dst: 1,
                aggregate: 0,
                field: 0,
              }),
            ],
            {
              op: "runtime.call",
              family: 0,
              args: [{ value: 1, decision: "borrow" }],
              result: null,
              result_shape: null,
              normal: edge(1),
              unwind: null,
              span: null,
            },
          ),
          block(
            1,
            fault ? [op("const.str", { dst: 2, str: 3 })] : [destroy(0)],
            fault
              ? {
                  op: "panic",
                  message: { value: 2, decision: "borrow" },
                  cleanup: edge(2),
                  span: null,
                }
              : ret,
          ),
          block(2, [destroy(0)], { op: "resume_unwind", span: null }),
        ],
      },
    ],
  };
}
async function run(ops, options) {
  const bytecode = pkg(ops, options);
  assert.equal(admitPackage(bytecode), null);
  return await runBytecode(bytecode, {
    replay: {
      seed: 0,
      step_budget: 1000,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: [],
    },
  });
}
for (const projected of [false, true]) {
  test(`repeated assignment closes each ${projected ? "projected" : "local"} owner once`, async () => {
    const trace = await run([...make(2, 1), store(2), ...make(4, 2), store(4), end], {
      projected,
    });
    assert.equal(trace.result, "ok");
    assert.equal(
      trace.final_state.stdout.join(""),
      "closed 1\nclosed 2\nclosed 3\n",
    );
  });
}
test("explicit consumption after reassignment leaves scope cleanup empty", async () => {
  const trace = await run([...make(2, 1), store(2), take(4), destroy(4), end]);
  assert.equal(trace.result, "ok");
  assert.equal(trace.final_state.stdout.join(""), "closed 1\nclosed 2\n");
});
test("assignment reinitializes a consumed place without closing it again", async () => {
  const trace = await run([take(4), destroy(4), ...make(2, 1), store(2), end]);
  assert.equal(trace.result, "ok");
  assert.equal(trace.final_state.stdout.join(""), "closed 1\nclosed 2\n");
});
test("faulting close publishes the replacement for unwind cleanup exactly once", async () => {
  const trace = await run([...make(2, 1), store(2)], { fault: true });
  assert.equal(trace.result, "panic");
  assert.equal(trace.final_state.runtime_failures[0].message, "close failed");
  assert.equal(trace.final_state.stdout.join(""), "closed 1\nclosed 2\n");
});

// Keep native parity executable with either the in-tree compiler or an explicit
// compiler path. Bytecode-level regressions above run without a native build.
const native =
  process.env.HEW_NATIVE_BIN ??
  fileURLToPath(new URL("../../target/debug/hew", import.meta.url));
const cases = [
  {
    name: "repeated assignment and scope exit",
    body: "fresh = Ticket { id: 2 }; fresh = Ticket { id: 3 };",
    ops: [...make(2, 1), store(2), ...make(4, 2), store(4), end],
  },
  {
    name: "explicit consumption",
    body: "fresh = Ticket { id: 2 }; fresh.close();",
    ops: [...make(2, 1), store(2), take(4), destroy(4), end],
  },
  {
    name: "consumption followed by reinitialization",
    body: "fresh.close(); fresh = Ticket { id: 2 };",
    ops: [take(4), destroy(4), ...make(2, 1), store(2), end],
  },
];
for (const entry of cases) {
  test(
    `native/sandbox resource parity: ${entry.name}`,
    {
      skip:
        !fs.existsSync(native) &&
        "native compiler unavailable (set HEW_NATIVE_BIN)",
    },
    async () => {
      const directory = fs.mkdtempSync(
        path.join(os.tmpdir(), "hew-resource-assignment-"),
      );
      try {
        const source = path.join(directory, "main.hew");
        fs.writeFileSync(
          source,
          `
#[resource]
type Ticket { id: i64; }
impl Ticket { fn close(consume self) { println(f"closed {self.id}"); } }
fn main() { var fresh = Ticket { id: 1 }; ${entry.body} }
`,
        );
        const result = spawnSync(native, ["run", source], {
          encoding: "utf8",
          timeout: 30000,
        });
        assert.ifError(result.error);
        assert.equal(result.status, 0, result.stderr);
        const trace = await run(entry.ops);
        assert.equal(trace.result, "ok");
        assert.equal(trace.final_state.stdout.join(""), result.stdout);
      } finally {
        fs.rmSync(directory, { recursive: true, force: true });
      }
    },
  );
}

test("an explicit close call consumes the replacement before scope exit", async () => {
  const bytecode = pkg([...make(2, 1), store(2), take(4)]);
  bytecode.functions[0].blocks[0].term = {
    op: "call",
    callee: 1,
    args: [{ value: 4, decision: "move" }],
    result: null,
    normal: edge(1),
    unwind: null,
    span: null,
  };
  bytecode.functions[0].blocks[1].ops = [end];
  const trace = await runBytecode(bytecode);
  assert.equal(trace.result, "ok");
  assert.equal(trace.final_state.stdout.join(""), "closed 1\nclosed 2\n");
});

test("a fault after assignment drains only the current owner", async () => {
  const bytecode = pkg([...make(2, 1), store(2)]);
  bytecode.functions[0].blocks[0].term = {
    op: "checked_raise",
    trap: "vector_bounds",
    cleanup: edge(2),
    span: null,
  };
  const trace = await runBytecode(bytecode);
  assert.equal(trace.result, "trap");
  assert.equal(
    trace.final_state.runtime_failures[0].trap_kind,
    "vector_bounds",
  );
  assert.equal(trace.final_state.stdout.join(""), "closed 1\nclosed 2\n");
});
