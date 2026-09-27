import type { SandboxTrace } from "./types.js";

/** A Hew fault from a selected test, excluding host failures and VM limits. */
export function classifyHewFault(
  trace: SandboxTrace,
): { code: number; kind: string; text: string } | null {
  if (trace.final_state.sandbox_rejections.length !== 0) return null;
  const failure = trace.final_state.runtime_failures[0];
  if (!failure?.hew_fault || failure.hew_fault.code <= 0) return null;
  if (
    !(
      (trace.result === "panic" && failure.kind === "panic") ||
      (trace.result === "trap" && failure.kind === "trap")
    )
  ) {
    return null;
  }
  const { code, kind } = failure.hew_fault;
  return {
    code,
    kind,
    text: failure.message ? `${kind}: ${failure.message}` : kind,
  };
}

/** Apply the `#[should_panic("fragment")]` contract to a VM trace. */
export function matchesExpectedFault(
  trace: SandboxTrace,
  fragment: string | null = null,
): boolean {
  const fault = classifyHewFault(trace);
  return fault !== null && (fragment === null || fault.text.includes(fragment));
}
