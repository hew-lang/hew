import type { SandboxBytecodePackage } from "./types.js";

const INSTRUCTION_OPS = new Set([
  "const.unit",
  "const.bool",
  "const.i64",
  "const.u64",
  "const.f64",
  "const.string",
  "const.regex",
  "local.get",
  "local.set",
  "local.move",
  "local.borrow",
  "i64.add",
  "i64.sub",
  "i64.mul",
  "i64.div",
  "i64.rem",
  "i64.neg",
  "i64.checked_add",
  "i64.checked_sub",
  "i64.checked_mul",
  "i64.checked_div",
  "i64.checked_rem",
  "f64.add",
  "f64.sub",
  "f64.mul",
  "f64.div",
  "f64.rem",
  "f64.neg",
  "cmp.eq",
  "cmp.ne",
  "cmp.lt",
  "cmp.le",
  "cmp.gt",
  "cmp.ge",
  "call.direct",
  "call.indirect",
  "call.stdlib",
  "record.new",
  "record.get",
  "record.set",
  "enum.new",
  "enum.tag",
  "enum.payload",
  "vector.new",
  "vector.push",
  "vector.get",
  "vector.set",
  "vector.len",
  "vector.contains",
  "vector.range_slice",
  "string.concat",
  "string.len",
  "string.slice",
  "regex.compile",
  "regex.is_match",
  "regex.find",
  "regex.replace",
  "regex.free",
  "panic",
  "trap",
  "actor.spawn",
  "actor.send",
  "actor.ask",
  "actor.reply",
  "mailbox.dequeue",
  "actor.link",
  "actor.unlink",
  "actor.monitor",
  "actor.demonitor",
  "channel.new",
  "channel.send",
  "channel.recv",
  "channel.close",
  "stream.memory",
  "stream.write",
  "stream.read",
  "stream.close",
  "stream.file.open",
  "stream.net.connect",
  "task.spawn",
  "task.await",
  "task.cancel",
  "scope.enter",
  "scope.launch",
  "scope.await",
  "scope.cancel",
  "scope.exit",
  "select.poll",
  "clock.sleep",
  "supervisor.spawn",
  "supervisor.child",
  "supervisor.restart",
  "supervisor.stop",
  "machine.new",
  "machine.step",
  "machine.state"
]);

const TERMINATOR_OPS = new Set(["br", "br_if", "return", "tail_call", "trap", "unreachable"]);

export class BytecodeValidationError extends Error {
  constructor(readonly errors: string[]) {
    super(`Invalid sandbox bytecode package: ${errors.join("; ")}`);
    this.name = "BytecodeValidationError";
  }
}

export function validateBytecodePackage(input: unknown): SandboxBytecodePackage {
  const errors: string[] = [];
  const pkg = input as Partial<SandboxBytecodePackage>;

  if (!isObject(pkg)) {
    throw new BytecodeValidationError(["package must be an object"]);
  }
  if (pkg.schema_version !== "hew.sandbox.bytecode.v0") {
    errors.push("schema_version must be hew.sandbox.bytecode.v0");
  }
  for (const field of [
    "package_id",
    "hew_version",
    "compiler_version",
    "profile",
    "source_map",
    "module_graph",
    "layouts",
    "stdlib_symbols",
    "capabilities",
    "functions"
  ] as const) {
    if (!(field in pkg)) {
      errors.push(`missing ${field}`);
    }
  }
  if (!Array.isArray(pkg.functions) || pkg.functions.length === 0) {
    errors.push("functions must be a non-empty array");
  } else {
    for (const fn of pkg.functions) {
      if (!isObject(fn) || typeof fn.id !== "string") {
        errors.push("function id must be a string");
        continue;
      }
      if (!Array.isArray(fn.locals)) {
        errors.push(`${fn.id}: locals must be an array`);
      }
      if (!Array.isArray(fn.blocks) || fn.blocks.length === 0) {
        errors.push(`${fn.id}: blocks must be a non-empty array`);
        continue;
      }
      const blockIds = new Set<string>();
      for (const block of fn.blocks) {
        if (!isObject(block) || typeof block.id !== "string") {
          errors.push(`${fn.id}: block id must be a string`);
          continue;
        }
        blockIds.add(block.id);
        if (!Array.isArray(block.instructions)) {
          errors.push(`${fn.id}/${block.id}: instructions must be an array`);
        } else {
          for (const instruction of block.instructions) {
            if (!isObject(instruction) || typeof instruction.op !== "string") {
              errors.push(`${fn.id}/${block.id}: instruction op must be a string`);
            } else if (!INSTRUCTION_OPS.has(instruction.op)) {
              errors.push(`${fn.id}/${block.id}: unknown instruction ${instruction.op}`);
            }
            if (!Array.isArray((instruction as { args?: unknown }).args)) {
              errors.push(`${fn.id}/${block.id}/${String((instruction as { op?: unknown }).op)}: args must be an array`);
            }
          }
        }
        if (!isObject(block.terminator) || !TERMINATOR_OPS.has(String(block.terminator.op))) {
          errors.push(`${fn.id}/${block.id}: invalid terminator`);
        }
      }
      for (const block of fn.blocks) {
        if (!isObject(block) || !isObject(block.terminator)) {
          continue;
        }
        for (const target of [block.terminator.target, block.terminator.else_target]) {
          if (typeof target === "string" && !blockIds.has(target)) {
            errors.push(`${fn.id}/${block.id}: unknown branch target ${target}`);
          }
        }
      }
    }
  }

  if (errors.length > 0) {
    throw new BytecodeValidationError(errors);
  }
  return pkg as SandboxBytecodePackage;
}

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}
