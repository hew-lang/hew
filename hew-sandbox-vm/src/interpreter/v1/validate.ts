/// Load-time admission for a verified SIR package.
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

const ACTOR_OPS = new Set([
  "spawn",
  "self_handle",
  "close",
  "await_closed",
  "call_start",
  "call_take",
  "submit",
  "stream_start",
  "supervisor_spawn",
  "supervisor_child",
  "supervisor_await_restart",
  "supervisor_pool_view",
  "supervisor_stop",
  "supervisor_await_closed",
  "supervisor_role_await_closed",
]);

function capabilityMessage(capability: string, native: boolean): string {
  const nativeFeatures: Record<string, string> = {
    FileRead: "Filesystem access",
    Tcp: "Network sockets",
    TcpAttachLocal: "Network streams",
    AsyncIo: "Host input and output",
    NativeIo: "Host input and output",
    Read: "Host input and output",
    Accept: "Accepting network connections",
    RemoteAsk: "Remote actor calls",
  };
  const features: Record<string, string> = {
    Vector: "This vector operation",
    Map: "This map operation",
    Set: "This set operation",
    "actor.periodic": "Periodic actor handlers",
    "actor.local_observation": "Actor links and monitors",
    "actor.mailbox.coalesce": "Mailbox coalescing",
    "actor.heap_limit": "Per-actor heap limits",
    "actor.ingress_adapter": "Actor ingress adapters",
    "wire.codec": "Wire encoding",
  };
  return native
    ? `${nativeFeatures[capability] ?? "This host capability"} requires native execution.`
    : `${features[capability] ?? "This language or standard-library operation"} is not available in the browser runtime yet.`;
}

function unavailable(capability: string): SandboxRejection {
  return {
    category: "not_implemented",
    code: UNSUPPORTED,
    capability,
    message: capabilityMessage(capability, false),
    span: null,
  };
}

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
      if (block.ops.some((op) => op.op === "actor.ingress_adapter"))
        return unavailable("actor.ingress_adapter");
      if (String(block.term.op) === "wire.codec")
        return unavailable("wire.codec");
      if (
        block.term.op === "actor.call" &&
        !ACTOR_OPS.has(block.term.operation.op)
      )
        return unavailable(`actor.${block.term.operation.op}`);
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

  for (const resource of pkg.resources ?? []) {
    if (resource.kind === "opaque") return unavailable("resource.opaque_close");
    if (
      resource.kind === "nominal" &&
      !resolveExternShim(resource.release ?? "")
    )
      return unavailable("resource.external_close");
  }

  for (const actor of pkg.actors ?? []) {
    if (actor.handlers.some((handler) => handler.every_ns != null))
      return unavailable("actor.periodic");
    if (actor.exit != null || actor.down != null)
      return unavailable("actor.local_observation");
    if (actor.coalesce != null) return unavailable("actor.mailbox.coalesce");
    if (actor.max_heap_bytes != null) return unavailable("actor.heap_limit");
  }

  for (const entry of pkg.runtime_families) {
    if (!resolveRuntimeShim(entry)) {
      return {
        category: NATIVE_FAMILIES.has(entry.family)
          ? "native_only"
          : "not_implemented",
        code: UNSUPPORTED,
        capability: familyIdentity(entry.family, entry.detail),
        message: capabilityMessage(
          entry.family,
          NATIVE_FAMILIES.has(entry.family),
        ),
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
        message:
          "This external-library operation is not available in the browser runtime yet.",
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
        message: capabilityMessage(kind, NATIVE_SUSPENSIONS.has(kind)),
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
