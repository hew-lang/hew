import { validateBytecodePackage } from "./schema-validator.js";
import { runPackageV1 } from "./v1/exec.js";
import { TraceBuilder } from "./trace.js";
import type { RunOptions, SandboxTrace } from "./types.js";

export function runBytecode(
  input: unknown,
  options: RunOptions = {},
): SandboxTrace {
  let bytecode;
  try {
    bytecode = validateBytecodePackage(input);
  } catch (error) {
    const trace = new TraceBuilder(
      { profile: "sandbox-vm-export", hew_version: "unknown" },
      options.fixtureId ?? "package",
      options.traceId ?? "trace:package",
      {
        seed: options.replay?.seed ?? 0,
        step_budget:
          options.stepBudget ?? options.replay?.step_budget ?? 1000000,
        virtual_clock: options.replay?.virtual_clock ?? {
          epoch_ms: 0,
          tick_ms: 1,
          current_ms: 0,
        },
        inputs: options.replay?.inputs ?? [],
      },
      options.sandboxVersion ?? "0.0.0-spec",
    );
    trace.reject({
      category: "invalid_package",
      code: "sandbox.package.invalid",
      capability: null,
      message: String(error),
      span: null,
    });
    return trace.finish();
  }
  return runPackageV1(bytecode, options);
}
