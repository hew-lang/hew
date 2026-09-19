/// Load-time admission for a v1 package (D517 Q415).
///
/// The package declares what it needs: `runtime_families` names every runtime
/// call family the instruction stream reaches, and `externs` names every
/// declared C-ABI symbol. This validator matches each against the shim table
/// the VM implements. A family or symbol with no shim is refused before a
/// single instruction runs. Unknown is rejected, never allowed.
///
/// The check keys on the declared identity: it never matches a symbol prefix
/// and never reads an instruction stream to guess a capability.

import type { SandboxRejection } from "../types.js";
import type { PackageV1 } from "./package.js";
import {
  SUPPORTED_SUSPEND_KINDS,
  resolveExternShim,
  resolveRuntimeShim,
} from "./shims.js";

const UNSUPPORTED = "sandbox.capability.unsupported";
const NATIVE_FAMILIES = new Set([
  "AsyncIo",
  "FileRead",
  "Tcp",
  "TcpAttachLocal",
]);
const NATIVE_SUSPENSIONS = new Set(["NativeIo", "RemoteAsk", "Read", "Accept"]);

const OPS = new Set([
  "const.int",
  "const.bool",
  "const.float",
  "const.char",
  "const.unit",
  "const.duration",
  "const.str",
  "const.bytes",
  "copy_value",
  "fork",
  "move",
  "destroy_value",
  "begin_borrow",
  "end_borrow",
  "finish_linear_receiver",
  "alloc_place",
  "store.init",
  "store.assign",
  "load.copy",
  "load.take",
  "load.borrow",
  "end_lifetime",
  "tuple.make",
  "tuple.get",
  "aggregate.make",
  "aggregate.project_copy",
  "aggregate.project_borrow",
  "destructure",
  "array.make",
  "array.repeat",
  "variant.make",
  "variant.is",
  "variant.project_copy",
  "variant.project_borrow",
  "variant.destructure",
  "unary",
  "binary",
  "cast",
  "str.eq",
  "bytes.eq",
  "function.make",
  "closure.make",
  "generator.make",
  "callable.coerce",
  "dyn.make",
  "register_defer",
  "stream.pipe",
  "task_scope.enter",
  "task_scope.close",
  "task.spawn",
]);
const TERMS = new Set([
  "goto",
  "branch",
  "switch.variant",
  "return",
  "checked.binary",
  "dyn.call",
  "call",
  "indirect.call",
  "actor.call",
  "runtime.call",
  "extern.call",
  "value.call",
  "panic",
  "checked_raise",
  "trap",
  "cleanup.dispatch",
  "enter_defer",
  "finish_defer",
  "recover_fault",
  "resume_unwind",
  "suspend",
  "unreachable",
]);

/// The first refusal, or `null` when every declared capability has a shim.
export function admitPackage(pkg: PackageV1): SandboxRejection | null {
  if (!pkg.entry) {
    return {
      category: "invalid_package",
      code: "sandbox.entry.missing",
      capability: null,
      message: "package declares no entry function",
      span: null,
    };
  }

  for (const function_ of pkg.functions) {
    for (const block of function_.blocks) {
      const unknown =
        block.ops.find((op) => !OPS.has(op.op))?.op ??
        (!TERMS.has(block.term.op) ? block.term.op : null);
      if (unknown)
        return {
          category: "invalid_package",
          code: "sandbox.package.unknown_opcode",
          capability: unknown,
          message: `unknown bytecode opcode ${unknown}`,
          span: null,
        };
    }
  }

  for (const entry of pkg.runtime_families) {
    if (!resolveRuntimeShim(entry)) {
      return {
        category: NATIVE_FAMILIES.has(entry.family)
          ? "native_only"
          : "not_implemented",
        code: UNSUPPORTED,
        capability: familyIdentity(entry.family, entry.detail),
        message: `runtime family ${familyIdentity(entry.family, entry.detail)} has no sandbox shim`,
        span: null,
      };
    }
  }

  for (const entry of pkg.externs) {
    if (!resolveExternShim(entry.symbol)) {
      return {
        category: "not_implemented",
        code: UNSUPPORTED,
        capability: entry.symbol,
        message: `extern symbol ${entry.symbol} has no sandbox shim`,
        span: null,
      };
    }
  }

  for (const kind of pkg.suspend_kinds) {
    if (!SUPPORTED_SUSPEND_KINDS.has(kind)) {
      return {
        category: NATIVE_SUSPENSIONS.has(kind)
          ? "native_only"
          : "not_implemented",
        code: UNSUPPORTED,
        capability: kind,
        message: `suspension kind ${kind} is not a sandbox capability`,
        span: null,
      };
    }
  }

  return null;
}

function familyIdentity(family: string, detail: unknown): string {
  return typeof detail === "string" && detail.length > 0
    ? `${family}::${detail}`
    : family;
}
