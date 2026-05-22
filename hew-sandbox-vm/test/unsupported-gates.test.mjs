import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

test("M6 machine operations remain fail closed", () => {
  for (const op of ["machine.new"]) {
    const trace = runBytecode(packageWithInstruction(op, []), {
      fixtureId: `unsupported-${op.replaceAll(".", "-")}`,
      traceId: `trace:unsupported-${op.replaceAll(".", "-")}`,
      replay: replay()
    });

    assert.equal(trace.result, "runtime_failure");
    assert.equal(trace.final_state.runtime_failures[0].kind, "unsupported");
    assert.equal(trace.final_state.runtime_failures[0].message, `Unsupported::M6_DEFERRED: ${op}`);
  }
});

test("M7 file and network stream operations fail closed", () => {
  for (const op of ["stream.file.open", "stream.net.connect"]) {
    const trace = runBytecode(packageWithInstruction(op, []), {
      fixtureId: `unsupported-${op.replaceAll(".", "-")}`,
      traceId: `trace:unsupported-${op.replaceAll(".", "-")}`,
      replay: replay()
    });

    assert.equal(trace.result, "runtime_failure");
    assert.equal(trace.final_state.runtime_failures[0].kind, "unsupported");
    assert.equal(trace.final_state.runtime_failures[0].message, `Unsupported::NATIVE_ONLY: ${op}`);
    assert.equal(trace.final_state.runtime_failures[0].unsupported.kind, "Unsupported::NATIVE_ONLY");
  }
});

test("M7 host I/O stdlib symbols fail closed", () => {
  const bytecode = packageWithInstruction("call.stdlib", [{ kind: "symbol", value: "sym:std.fs.read" }]);
  bytecode.stdlib_symbols = [
    {
      id: "sym:std.fs.read",
      module: "std.fs",
      name: "read",
      params: [],
      result: "type:string",
      capability: "std.fs.read",
      admission: "rejected"
    }
  ];
  bytecode.capabilities = [
    { id: "std.fs.read", disposition: "rejected", reason: "host filesystem unavailable", required_by: ["sym:std.fs.read"] }
  ];

  const trace = runBytecode(bytecode, {
    fixtureId: "unsupported-host-io",
    traceId: "trace:unsupported-host-io",
    replay: replay()
  });

  assert.equal(trace.result, "runtime_failure");
  assert.equal(trace.events[1].type, "runtime.failure");
  assert.equal(trace.final_state.runtime_failures[0].kind, "unsupported");
  assert.match(trace.final_state.runtime_failures[0].message, /Unsupported::NATIVE_ONLY: std\.fs\.read/);
  assert.equal(trace.final_state.runtime_failures[0].unsupported.status, "unsupported_native_only");
});

function replay() {
  return { seed: 999, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] };
}

function packageWithInstruction(op, args) {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: `pkg:${op.replaceAll(".", "-")}`,
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{ id: "mod:main", path: "main.hew", source_id: "src:main", imports: [], functions: ["fn:main"] }]
    },
    layouts: {
      types: [
        { id: "type:unit", kind: "unit", name: "()" },
        { id: "type:i64", kind: "integer", name: "i64" },
        { id: "type:string", kind: "string", name: "String" },
        { id: "type:actor", kind: "actor", name: "Actor" }
      ],
      records: [],
      enums: [],
      actors: [],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [],
    capabilities: [],
    functions: [
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [{ id: "local:main.0", name: null, type: "type:unit", mutable: false, span: null }],
        blocks: [
          {
            id: "block:main.entry",
            params: [],
            instructions: [{ op, dst: "local:main.0", args, span: null }],
            terminator: { op: "return", args: [], span: null },
            span: null
          }
        ],
        span: null
      }
    ]
  };
}
