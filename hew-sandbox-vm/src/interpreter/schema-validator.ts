import { isPackageV1, type PackageV1 } from "./v1/package.js";

export class BytecodeValidationError extends Error {
  constructor(readonly errors: string[]) {
    super(errors.join("; "));
    this.name = "BytecodeValidationError";
  }
}

/** Check the package envelope before capability admission traverses its tables.
 * Operation support belongs to v1/validate, with one shared executor registry. */
export function validateBytecodePackage(input: unknown): PackageV1 {
  if (!isPackageV1(input))
    throw new BytecodeValidationError(["expected hew.sandbox.bytecode.v1"]);
  const errors: string[] = [];
  for (const key of ["profile", "hew_version", "compiler_version"] as const) {
    if (typeof input[key] !== "string") errors.push(`${key} must be a string`);
  }
  for (const key of [
    "functions",
    "strings",
    "bytes",
    "regex_patterns",
    "aggregates",
    "variants",
    "runtime_families",
    "externs",
    "suspend_kinds",
    "value_capabilities",
    "closures",
    "vtables",
  ] as const) {
    if (!Array.isArray(input[key])) errors.push(`${key} must be an array`);
  }
  if (errors.length) throw new BytecodeValidationError(errors);
  for (const fn of input.functions) {
    if (
      !fn ||
      !Number.isInteger(fn.id) ||
      !Array.isArray(fn.params) ||
      !Array.isArray(fn.places) ||
      !Array.isArray(fn.blocks)
    ) {
      errors.push("malformed function");
      continue;
    }
    for (const block of fn.blocks) {
      if (
        !block ||
        !Number.isInteger(block.id) ||
        !Array.isArray(block.params) ||
        !Array.isArray(block.ops) ||
        !block.term ||
        typeof block.term.op !== "string"
      )
        errors.push("malformed block");
    }
  }
  if (errors.length) throw new BytecodeValidationError(errors);
  return input;
}
