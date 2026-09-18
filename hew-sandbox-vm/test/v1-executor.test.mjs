// Behavioural tests for the `hew.sandbox.bytecode.v1` executor. Each package is
// hand-written in the shape the emitter produces, so a test states what a Hew
// programmer observes: what the program printed, what it exited with, and which
// edge the VM took.

import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

const PRINT_LN = { id: 0, family: "Print", detail: { kind: "Str", newline: true } };

function pkg(overrides) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "v1-executor-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: [],
    bytes: [],
    regex_patterns: [],
    aggregates: [],
    variants: [],
    runtime_families: [],
    externs: [],
    suspend_kinds: [],
    value_capabilities: [],
    closures: [],
    vtables: [],
    functions: [],
    ...overrides
  };
}

function fn(id, name, blocks, extra = {}) {
  return { id, name, params: [], entry: 0, places: [], blocks, ...extra };
}

function block(id, ops, term, params = []) {
  return { id, params, ops, term };
}

const RETURN_UNIT = { op: "return", value: null, span: null };

/// A `Print` call that writes `value` and continues at `to`.
function printTerm(value, to) {
  return {
    op: "runtime.call",
    family: 0,
    args: [{ value, decision: "borrow" }],
    result: null,
    result_shape: null,
    normal: { to, args: [] },
    unwind: null,
    span: null
  };
}

function run(bytecode, options = {}) {
  return runBytecode(bytecode, {
    fixtureId: "v1-test",
    traceId: "trace:v1-test",
    replay: {
      seed: options.seed ?? 3,
      step_budget: options.stepBudget ?? 10_000,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: options.inputs ?? []
    }
  });
}

function stdout(trace) {
  return trace.final_state.stdout.join("");
}

test("v1: a package prints its string literal and exits 0", () => {
  const trace = run(
    pkg({
      strings: ["hello, sandbox"],
      runtime_families: [PRINT_LN],
      functions: [
        fn(0, "main", [
          block(0, [{ op: "const.str", dst: 0, str: 0, span: null }], printTerm(0, 1)),
          block(1, [{ op: "destroy_value", value: 0, span: null }], RETURN_UNIT)
        ])
      ]
    })
  );

  assert.equal(trace.result, "ok");
  assert.equal(stdout(trace), "hello, sandbox\n");
  assert.equal(trace.final_state.exit_code, 0);
});

test("v1: an entry that returns a status publishes it as the exit code", () => {
  const trace = run(
    pkg({
      entry: { function: 0, exit: "status" },
      functions: [
        fn(0, "main", [
          block(0, [{ op: "const.int", dst: 0, value: "7", ty: "i64", span: null }], {
            op: "return",
            value: { value: 0, decision: "move" },
            span: null
          })
        ])
      ]
    })
  );

  assert.equal(trace.result, "ok");
  assert.equal(trace.final_state.exit_code, 7);
});

test("v1: copy_value is a deep clone, so mutating the original leaves the copy alone", () => {
  // let v = Vec::new(); let c = v.clone(); v.push(1); print(c.len()); print(v.len())
  const trace = run(
    pkg({
      runtime_families: [
        PRINT_LN,
        { id: 1, family: "Vector", detail: "New" },
        { id: 2, family: "Vector", detail: "Push" },
        { id: 3, family: "Vector", detail: "Len" },
        { id: 4, family: "I64ToString" }
      ],
      functions: [
        fn(0, "main", [
          block(0, [], {
            op: "runtime.call",
            family: 1,
            args: [],
            result: { value: 0, own: "owned" },
            result_shape: null,
            normal: { to: 1, args: [0] },
            unwind: null,
            span: null
          }),
          block(
            1,
            [
              { op: "copy_value", dst: 2, source: 1, span: null },
              { op: "const.int", dst: 3, value: "1", ty: "i64", span: null }
            ],
            {
              op: "runtime.call",
              family: 2,
              args: [
                { value: 1, decision: "move" },
                { value: 3, decision: "move" }
              ],
              result: { value: 4, own: "owned" },
              result_shape: null,
              normal: { to: 2, args: [] },
              unwind: null,
              span: null
            },
            [{ value: 1, own: "owned" }]
          ),
          // The copy's length, then the mutated original's length.
          block(2, [], lenCall(2, 5, 3)),
          block(3, [], toStringCall(5, 6, 4)),
          block(4, [], printTerm(6, 5)),
          block(5, [], lenCall(4, 7, 6)),
          block(6, [], toStringCall(7, 8, 7)),
          block(7, [], printTerm(8, 8)),
          block(8, [], RETURN_UNIT)
        ])
      ]
    })
  );

  assert.equal(trace.result, "ok");
  assert.equal(stdout(trace), "0\n1\n");
});

function lenCall(vector, dst, to) {
  return {
    op: "runtime.call",
    family: 3,
    args: [{ value: vector, decision: "borrow" }],
    result: { value: dst, own: "owned" },
    result_shape: null,
    normal: { to, args: [] },
    unwind: null,
    span: null
  };
}

function toStringCall(value, dst, to) {
  return {
    op: "runtime.call",
    family: 4,
    args: [{ value, decision: "copy" }],
    result: { value: dst, own: "owned" },
    result_shape: null,
    normal: { to, args: [] },
    unwind: null,
    span: null
  };
}

test("v1: a store through a place is visible through a live loan of it", () => {
  const trace = run(
    pkg({
      strings: ["before", "after"],
      runtime_families: [PRINT_LN],
      functions: [
        fn(
          0,
          "main",
          [
            block(
              0,
              [
                { op: "const.str", dst: 0, str: 0, span: null },
                { op: "alloc_place", place: 0, span: null },
                { op: "store.init", place: 0, value: 0, span: null },
                // The loan is taken before the assignment, and read after it.
                { op: "load.borrow", dst: 1, place: 0, span: null },
                { op: "const.str", dst: 2, str: 1, span: null },
                { op: "store.assign", place: 0, value: 2, span: null }
              ],
              printTerm(1, 1)
            ),
            block(1, [{ op: "end_borrow", borrow: 1, span: null }, { op: "end_lifetime", place: 0, span: null }], RETURN_UNIT)
          ],
          { places: [{ id: 0, origin: "local" }] }
        )
      ]
    })
  );

  assert.equal(trace.result, "ok");
  assert.equal(stdout(trace), "after\n");
});

test("v1: an out-of-bounds index takes the unwind edge before the program ends", () => {
  const indexPackage = (at) =>
    pkg({
      strings: ["cleanup ran"],
      runtime_families: [
        PRINT_LN,
        { id: 1, family: "Vector", detail: "New" },
        { id: 2, family: "Vector", detail: "Index" },
        { id: 3, family: "Vector", detail: "Push" },
        { id: 4, family: "I64ToString" }
      ],
      functions: [
        fn(0, "main", [
          block(0, [], {
            op: "runtime.call",
            family: 1,
            args: [],
            result: { value: 0, own: "owned" },
            result_shape: null,
            normal: { to: 1, args: [0] },
            unwind: null,
            span: null
          }),
          block(
            1,
            [{ op: "const.int", dst: 2, value: "42", ty: "i64", span: null }],
            {
              op: "runtime.call",
              family: 3,
              args: [
                { value: 1, decision: "move" },
                { value: 2, decision: "move" }
              ],
              result: { value: 3, own: "owned" },
              result_shape: null,
              normal: { to: 2, args: [3] },
              unwind: null,
              span: null
            },
            [{ value: 1, own: "owned" }]
          ),
          block(
            2,
            [{ op: "const.int", dst: 5, value: String(at), ty: "i64", span: null }],
            {
              op: "runtime.call",
              family: 2,
              args: [
                { value: 4, decision: "borrow" },
                { value: 5, decision: "borrow" }
              ],
              result: { value: 6, own: "owned" },
              result_shape: null,
              normal: { to: 3, args: [] },
              unwind: { to: 5, args: [] },
              span: null
            },
            [{ value: 4, own: "owned" }]
          ),
          block(3, [], toStringCall(6, 7, 4)),
          block(4, [], printTerm(7, 6)),
          // The unwind edge: run cleanup, then keep unwinding.
          block(5, [{ op: "const.str", dst: 8, str: 0, span: null }], printTerm(8, 7)),
          block(6, [], RETURN_UNIT),
          block(7, [], { op: "resume_unwind", span: null })
        ])
      ]
    });

  const trapped = run(indexPackage(9));
  assert.equal(trapped.result, "trap");
  assert.equal(stdout(trapped), "cleanup ran\n", "the cleanup the package names runs before the program ends");
  assert.equal(trapped.final_state.exit_code, 205);
  assert.equal(trapped.final_state.runtime_failures[0].trap_kind, "vector_bounds");

  // Negative control: the same package with an index in range never reaches
  // the unwind edge.
  const fine = run(indexPackage(0));
  assert.equal(fine.result, "ok");
  assert.equal(stdout(fine), "42\n");
  assert.equal(fine.final_state.exit_code, 0);
});

test("v1: checked.binary takes the failure edge the package names instead of trapping", () => {
  const overflowing = (lhs) =>
    pkg({
      strings: ["overflowed"],
      runtime_families: [PRINT_LN, { id: 1, family: "I64ToString" }],
      functions: [
        fn(0, "main", [
          block(
            0,
            [
              { op: "const.int", dst: 0, value: String(lhs), ty: "i32", span: null },
              { op: "const.int", dst: 1, value: "1", ty: "i32", span: null }
            ],
            {
              op: "checked.binary",
              binary_op: "Add",
              lhs: 0,
              rhs: 1,
              ty: "i32",
              result: { value: 2, own: "owned" },
              normal: { to: 1, args: [] },
              failures: [{ trap: "integer_overflow", edge: { to: 3, args: [] } }],
              span: null
            }
          ),
          block(1, [], toStringCall2(2, 3, 2)),
          block(2, [], printTerm(3, 4)),
          block(3, [{ op: "const.str", dst: 4, str: 0, span: null }], printTerm(4, 4)),
          block(4, [], RETURN_UNIT)
        ])
      ]
    });

  // i32's largest value plus one leaves on the named overflow edge, and the
  // program keeps running: the VM never trapped on its own.
  const overflowed = run(overflowing(2147483647));
  assert.equal(overflowed.result, "ok");
  assert.equal(stdout(overflowed), "overflowed\n");
  assert.equal(overflowed.final_state.exit_code, 0);

  // Negative control: one below the bound stays on the normal edge.
  const fine = run(overflowing(2147483646));
  assert.equal(fine.result, "ok");
  assert.equal(stdout(fine), "2147483647\n");
});

function toStringCall2(value, dst, to) {
  return {
    op: "runtime.call",
    family: 1,
    args: [{ value, decision: "copy" }],
    result: { value: dst, own: "owned" },
    result_shape: null,
    normal: { to, args: [] },
    unwind: null,
    span: null
  };
}

test("v1: switch.variant dispatches on the recorded tag", () => {
  const forTag = (tag) =>
    pkg({
      strings: ["none", "some"],
      variants: [{ id: 0, name: "Option", cases: [{ name: "Some", fields: ["0"] }, { name: "None", fields: [] }] }],
      runtime_families: [PRINT_LN],
      functions: [
        fn(0, "main", [
          block(
            0,
            [
              { op: "const.int", dst: 0, value: "5", ty: "i64", span: null },
              { op: "variant.make", dst: 1, shape: 0, variant: tag, fields: tag === 0 ? [0] : [], span: null }
            ],
            {
              op: "switch.variant",
              shape: 0,
              scrutinee: 1,
              arms: [
                { variant: 0, fields: [{ value: 2, own: "none" }], edge: { to: 1, args: [] } },
                { variant: 1, fields: [], edge: { to: 3, args: [] } }
              ],
              span: null
            }
          ),
          block(1, [{ op: "const.str", dst: 3, str: 1, span: null }], printTerm(3, 2)),
          block(2, [], RETURN_UNIT),
          block(3, [{ op: "const.str", dst: 4, str: 0, span: null }], printTerm(4, 2))
        ])
      ]
    });

  assert.equal(stdout(run(forTag(0))), "some\n");
  assert.equal(stdout(run(forTag(1))), "none\n");
});

test("v1: read_line hands out successive lines of the replay stdin", () => {
  const trace = run(
    pkg({
      runtime_families: [PRINT_LN],
      externs: [{ id: 0, symbol: "hew_io_read_line" }],
      functions: [
        fn(0, "main", [
          block(0, [], readLineTerm(0, 1)),
          block(1, [], printTerm(0, 2), [{ value: 0, own: "owned" }]),
          block(2, [], readLineTerm(1, 3)),
          block(3, [], printTerm(1, 4), [{ value: 1, own: "owned" }]),
          block(4, [], RETURN_UNIT)
        ])
      ]
    }),
    { inputs: [{ kind: "stdin", data: "first\nsecond\n" }] }
  );

  assert.equal(trace.result, "ok");
  assert.equal(stdout(trace), "first\nsecond\n");
});

function readLineTerm(dst, to) {
  return {
    op: "extern.call",
    extern: 0,
    args: [],
    result: { value: dst, own: "owned" },
    result_shape: null,
    normal: { to, args: [dst] },
    unwind: null,
    span: null
  };
}

test("v1: a seeded program prints the same numbers whatever the chaos seed is", () => {
  const randomPackage = pkg({
    runtime_families: [PRINT_LN, { id: 1, family: "I64ToString" }],
    externs: [
      { id: 0, symbol: "hew_random_seed" },
      { id: 1, symbol: "hew_random_randint" }
    ],
    functions: [
      fn(0, "main", [
        block(0, [{ op: "const.int", dst: 0, value: "1", ty: "i64", span: null }], {
          op: "extern.call",
          extern: 0,
          args: [{ value: 0, decision: "copy" }],
          result: null,
          result_shape: null,
          normal: { to: 1, args: [] },
          unwind: null,
          span: null
        }),
        block(
          1,
          [
            { op: "const.int", dst: 1, value: "0", ty: "i64", span: null },
            { op: "const.int", dst: 2, value: "99", ty: "i64", span: null }
          ],
          {
            op: "extern.call",
            extern: 1,
            args: [
              { value: 1, decision: "copy" },
              { value: 2, decision: "copy" }
            ],
            result: { value: 3, own: "owned" },
            result_shape: null,
            normal: { to: 2, args: [] },
            unwind: null,
            span: null
          }
        ),
        block(2, [], toStringCall2(3, 4, 3)),
        block(3, [], printTerm(4, 4)),
        block(4, [], RETURN_UNIT)
      ])
    ]
  });

  // `random.seed(1); random.randint(0, 99)` is 17 on native Hew, and the VM
  // runs the same MT19937, so the draw does not depend on the VM's own seed.
  assert.equal(stdout(run(randomPackage, { seed: 1 })), "17\n");
  assert.equal(stdout(run(randomPackage, { seed: 9999 })), "17\n");
});

// ── admission ───────────────────────────────────────────────────────────────

/// A package that would print if it ever started, so a rejection proves no
/// instruction ran rather than merely that nothing was printed by accident.
function wouldPrint(extra) {
  return pkg({
    strings: ["this must never be printed"],
    runtime_families: [PRINT_LN, ...(extra.runtime_families ?? [])],
    suspend_kinds: extra.suspend_kinds ?? [],
    externs: extra.externs ?? [],
    functions: [
      fn(0, "main", [
        block(0, [{ op: "const.str", dst: 0, str: 0, span: null }], printTerm(0, 1)),
        block(1, [], RETURN_UNIT)
      ])
    ]
  });
}

function assertRejected(trace, named) {
  assert.equal(trace.result, "sandbox_rejected");
  assert.equal(trace.final_state.status, "sandbox_rejected");
  assert.deepEqual(trace.final_state.stdout, [], "no instruction runs before admission");
  assert.equal(trace.final_state.step_count, 0);
  assert.equal(trace.final_state.sandbox_rejections.length, 1);
  assert.equal(trace.final_state.sandbox_rejections[0].capability, named);
  assert.ok(
    trace.events.some((event) => event.type === "sandbox.rejected" && event.rejection.capability === named),
    "the trace carries a sandbox.rejected event naming the capability"
  );
}

test("v1: a native-only runtime family is refused at load", () => {
  assertRejected(run(wouldPrint({ runtime_families: [{ id: 1, family: "FileRead", detail: "Open" }] })), "FileRead::Open");
});

test("v1: a runtime family the shim table does not know is refused at load", () => {
  assertRejected(run(wouldPrint({ runtime_families: [{ id: 1, family: "NoSuchFamily" }] })), "NoSuchFamily");
});

test("v1: an extern symbol with no shim is refused at load", () => {
  assertRejected(run(wouldPrint({ externs: [{ id: 0, symbol: "hew_open_socket" }] })), "hew_open_socket");
});

test("v1: native I/O suspension is refused at load", () => {
  assertRejected(run(wouldPrint({ suspend_kinds: ["NativeIo"] })), "NativeIo");
});

test("v1: a known family with an operation the VM has no shim for is refused", () => {
  // `Vector` is admitted, but only for the operations the table implements.
  assertRejected(run(wouldPrint({ runtime_families: [{ id: 1, family: "Vector", detail: "Remove" }] })), "Vector::Remove");
});

test("v1: a package with no entry is refused rather than picking a function to run", () => {
  const headless = wouldPrint({});
  delete headless.entry;
  const trace = run(headless);
  assert.equal(trace.result, "sandbox_rejected");
  assert.deepEqual(trace.final_state.stdout, []);
});
