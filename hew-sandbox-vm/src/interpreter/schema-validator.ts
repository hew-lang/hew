import { validPackage } from "./v1/package-validator.generated.js";
import type { PackageV1 } from "./v1/package.js";
import { validatePackageGraph } from "./v1/package-graph.js";

declare const checkedPackage: unique symbol;
/// Shape and reference validity only. Imported bytecode has no SIR ownership proof.
export type CheckedPackage = PackageV1 & { readonly [checkedPackage]: true };

export class BytecodeValidationError extends Error {
  constructor(readonly errors: string[]) {
    super(errors.join("; "));
    this.name = "BytecodeValidationError";
  }
}

/** Decode an isolated package before capability admission or any host effect.
 * Cloning prevents a caller or host callback from changing the checked input. */
export function validateBytecodePackage(input: unknown): CheckedPackage {
  try {
    const decoded: unknown = structuredClone(input);
    if (!validPackage(decoded)) {
      const reasons = (validPackage.errors ?? []).map(
        (issue: { instancePath: string; message?: string }) =>
          `${issue.instancePath || "/"} ${issue.message ?? "invalid value"}`,
      );
      throw new Error(
        reasons.join("; ") || "malformed sandbox bytecode package",
      );
    }
    const pkg = decoded as PackageV1;
    validatePackageGraph(pkg);
    return pkg as CheckedPackage;
  } catch (error) {
    throw new BytecodeValidationError([String(error)]);
  }
}
