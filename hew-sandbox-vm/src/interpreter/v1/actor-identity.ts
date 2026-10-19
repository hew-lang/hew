import type { VmValue } from "../values.js";

/** Role handles retain their own identity independently of their occupant. */
export function actorIdentical(
  left: VmValue | undefined,
  right: VmValue | undefined,
): boolean {
  if (
    (left?.kind !== "actor" && left?.kind !== "supervisor") ||
    (right?.kind !== "actor" && right?.kind !== "supervisor")
  )
    throw new Error("actor identity requires actor or supervisor handles");
  return left.kind === right.kind && left.id === right.id;
}
