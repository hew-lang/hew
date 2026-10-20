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
import { OP_NAMES, TERM_NAMES } from "./instructions.generated.js";
import {
  SUPPORTED_NATIVE_IO,
  SUPPORTED_SUSPEND_KINDS,
  resolveExternShim,
  resolveRuntimeShim,
} from "./shims.js";

const UNSUPPORTED = "sandbox.capability.unsupported";
const NATIVE_FAMILIES = new Set(["AsyncIo", "FileRead", "Tcp"]);
const NATIVE_SUSPENSIONS = new Set(["NativeIo", "Read", "Accept"]);

const OPS: ReadonlySet<string> = new Set(OP_NAMES);
const TERMS: ReadonlySet<string> = new Set(TERM_NAMES);

function capabilityMessage(capability: string, native: boolean): string {
  const nativeFeatures: Record<string, string> = {
    FileRead: "Filesystem access",
    Tcp: "Network sockets",
    AsyncIo: "Host input and output",
    NativeIo: "Host input and output",
    Read: "Host input and output",
    Accept: "Accepting network connections",
  };
  const features: Record<string, string> = {
    Vector: "This vector operation",
    Map: "This map operation",
    Set: "This set operation",
    "actor.local_observation": "Actor links and monitors",
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

  const timerResources = new Set<number>();
  for (const function_ of pkg.functions) {
    for (const block of function_.blocks) {
      if (block.ops.some((op) => op.op === "actor.ingress_adapter"))
        return unavailable("actor.ingress_adapter");
      if (
        block.term.op === "wire.codec" &&
        (!Number.isSafeInteger(block.term.codec) ||
          block.term.codec < 0 ||
          typeof pkg.codecs?.[block.term.codec] !== "string" ||
          ![0, 1, 2, 3, 4].includes(block.term.format) ||
          !["encode", "decode", "test_input"].includes(block.term.direction))
      )
        return {
          category: "invalid_package",
          code: "sandbox.package.invalid_codec",
          capability: "wire.codec",
          message: "codec call lacks its checked shape or format",
          span: null,
        };
      if (
        block.term.op === "actor.call" &&
        block.term.operation.op === "local_observation"
      )
        return unavailable(`actor.${block.term.operation.op}`);
      if (
        block.ops.some(
          (op) =>
            op.op === "coverage.hit" &&
            (!Number.isSafeInteger(op.counter) ||
              op.counter < 0 ||
              op.counter > 0xffff_ffff),
        )
      ) {
        return {
          category: "invalid_package",
          code: "sandbox.package.invalid_counter",
          capability: "coverage.hit",
          message: "coverage counter must be a u32",
          span: null,
        };
      }
      const term = block.term;
      if (
        term.op === "runtime.call" &&
        pkg.runtime_families[term.family]?.family === "BytesDecodeUtf8"
      ) {
        const shapes = term.variant_result_shapes;
        const error = shapes && pkg.aggregates[shapes.error];
        const length = shapes && pkg.variants[shapes.error_len];
        const result =
          term.result_shape === null
            ? undefined
            : pkg.variants[term.result_shape];
        if (
          !error ||
          error.fields.length !== 2 ||
          !length ||
          !result ||
          length.runtime_tags?.OptionSome === undefined ||
          length.runtime_tags?.OptionNone === undefined ||
          result.runtime_tags?.ResultOk === undefined ||
          result.runtime_tags?.ResultErr === undefined
        )
          return {
            category: "invalid_package",
            code: "sandbox.package.variant_result_shapes",
            capability: null,
            message: "UTF-8 decode lacks its checked result carrier",
            span: null,
          };
      }
      if (term.op === "actor.call" && term.operation.op === "timer_schedule") {
        const operation = term.operation;
        const handler = pkg.actors
          ?.find((actor) => actor.id === operation.actor)
          ?.handlers.find(
            (handler) => handler.message_id === operation.message,
          );
        const resource = pkg.resources?.[operation.resource];
        if (
          !handler ||
          handler.streams ||
          term.args.length !== 5 ||
          !["reject", "drop_newest"].includes(operation.policy) ||
          resource?.kind !== "opaque" ||
          resource.close === undefined ||
          !pkg.functions.some((fn) => fn.id === resource.close)
        )
          return {
            category: "invalid_package",
            code: "sandbox.package.invalid_timer",
            capability: "actor.timer_schedule",
            message:
              "timer schedule lacks its checked handler, timing or non-waiting admission",
            span: null,
          };
        timerResources.add(operation.resource);
      }
      if (
        term.op === "actor.call" &&
        term.operation.op === "supervisor_spawn"
      ) {
        const id = term.operation.supervisor;
        const supervisor = pkg.supervisors?.find((layout) => layout.id === id);
        const config =
          term.args.length -
          (supervisor?.children.some((child) => child.is_pool) ? 1 : 0);
        if (
          !supervisor ||
          config < 0 ||
          supervisor.children.some((child) => {
            const params = pkg.functions.find(
              (fn) => fn.id === child.spawn,
            )?.params;
            return !params || ![config, config + 1].includes(params.length);
          })
        )
          return {
            category: "invalid_package",
            code: "sandbox.package.supervisor_constructor",
            capability: null,
            message:
              "supervisor constructor signature does not match its config",
            span: null,
          };
      }
      if (term.op === "suspend") {
        const mandatoryClose = (pkg.resources ?? []).some(
          (resource) => resource.close === function_.id,
        );
        if (
          term.kind === "Safepoint"
            ? (term.cancel == null) !== mandatoryClose
            : term.cancel == null
        )
          return {
            category: "invalid_package",
            code: "sandbox.package.cancellation_ancestry",
            capability: null,
            message:
              "safepoint cancellation ancestry must match its authored close lifecycle",
            span: null,
          };
      }
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

  for (const [index, resource] of (pkg.resources ?? []).entries()) {
    if (
      resource.kind === "record" &&
      (resource.shape === undefined ||
        !pkg.aggregates.some((shape) => shape.id === resource.shape))
    )
      return {
        category: "invalid_package",
        code: "sandbox.package.resource_shape_missing",
        capability: null,
        message: "record resource has no exact aggregate shape",
        span: null,
      };
    if (resource.kind === "opaque" && !timerResources.has(index))
      return unavailable("resource.opaque_close");
    if (
      resource.kind === "nominal" &&
      !resolveExternShim(resource.release ?? "")
    )
      return unavailable("resource.external_close");
  }

  for (const actor of pkg.actors ?? []) {
    if (typeof actor.name !== "string" || actor.name.length === 0)
      return {
        category: "invalid_package",
        code: "sandbox.package.actor_name",
        capability: null,
        message: "actor lacks its declared diagnostic name",
        span: null,
      };
    if (
      actor.crash != null &&
      (actor.crash_info == null ||
        !pkg.aggregates[actor.crash_info] ||
        actor.crash_kind == null ||
        !pkg.variants[actor.crash_kind] ||
        actor.crash_action == null ||
        !pkg.variants[actor.crash_action])
    )
      return {
        category: "invalid_package",
        code: "sandbox.package.crash_hook_shapes",
        capability: null,
        message: "crash hook lacks its exact info, kind or action shape",
        span: null,
      };
    if (
      actor.handlers.some(
        (handler) =>
          handler.every_ns != null &&
          (!Number.isInteger(handler.every_ns) ||
            handler.every_ns < 1_000_000 ||
            handler.params !== 0 ||
            handler.streams ||
            handler.fallible ||
            !pkg.functions.some(
              (fn) => fn.id === handler.callable && fn.params.length === 1,
            )),
      )
    )
      return {
        category: "invalid_package",
        code: "sandbox.package.invalid_periodic",
        capability: "actor.periodic",
        message:
          "periodic handler lacks its checked interval or zero-argument body",
        span: null,
      };
    if (actor.exit != null || actor.down != null)
      return unavailable("actor.local_observation");
    if (actor.max_heap_bytes != null) return unavailable("actor.heap_limit");
  }

  for (const supervisor of pkg.supervisors ?? []) {
    const order =
      supervisor.startup_order === undefined
        ? supervisor.children.map((_child, index) => index)
        : supervisor.startup_order;
    if (
      !Array.isArray(order) ||
      supervisor.children.some(
        (child) =>
          child.dependencies !== undefined &&
          (!Array.isArray(child.dependencies) ||
            new Set(child.dependencies).size !== child.dependencies.length),
      )
    )
      return {
        category: "invalid_package",
        code: "sandbox.package.supervisor_startup_order",
        capability: null,
        message:
          "supervisor startup order and dependencies must be arrays of unique children",
        span: null,
      };
    const ranks = new Map(order.map((child, rank) => [child, rank]));
    if (
      order.length !== supervisor.children.length ||
      ranks.size !== order.length ||
      order.some(
        (child) =>
          !Number.isInteger(child) ||
          child < 0 ||
          child >= supervisor.children.length,
      ) ||
      supervisor.children.some((child, index) =>
        (child.dependencies ?? []).some(
          (dependency) =>
            !Number.isInteger(dependency) ||
            dependency < 0 ||
            !ranks.has(dependency) ||
            ranks.get(dependency)! >= ranks.get(index)!,
        ),
      )
    )
      return {
        category: "invalid_package",
        code: "sandbox.package.supervisor_startup_order",
        capability: null,
        message:
          "supervisor startup order must precede each declared dependency",
        span: null,
      };
    if (
      supervisor.children.some(
        (child) => !pkg.functions.some((fn) => fn.id === child.spawn),
      )
    )
      return {
        category: "invalid_package",
        code: "sandbox.package.supervisor_constructor",
        capability: null,
        message: "supervisor child has no constructor callable",
        span: null,
      };
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
      const native = entry.native_capability;
      return {
        category: native === undefined ? "not_implemented" : "native_only",
        code: UNSUPPORTED,
        capability: entry.symbol,
        message:
          native === undefined
            ? "This external-library operation is not available in the browser runtime yet."
            : `${native} require native execution.`,
        span: null,
      };
    }
  }

  for (const kind of pkg.suspend_kinds) {
    const refused =
      kind === "NativeIo"
        ? refusedNativeIo(pkg)
        : SUPPORTED_SUSPEND_KINDS.has(kind)
          ? null
          : kind;
    if (refused !== null) {
      return {
        category: NATIVE_SUSPENSIONS.has(kind)
          ? "native_only"
          : "not_implemented",
        code: UNSUPPORTED,
        capability: refused,
        message: capabilityMessage(kind, NATIVE_SUSPENSIONS.has(kind)),
        span: null,
      };
    }
  }

  return null;
}

/// The first native I/O suspension whose operation the VM does not serve.
function refusedNativeIo(pkg: PackageV1): string | null {
  for (const function_ of pkg.functions) {
    for (const block of function_.blocks) {
      const term = block.term;
      if (term.op !== "suspend" || term.kind !== "NativeIo") continue;
      const operation = String(term.detail?.operation);
      if (!SUPPORTED_NATIVE_IO.has(operation)) return `NativeIo::${operation}`;
    }
  }
  return null;
}

function familyIdentity(family: string, detail: unknown): string {
  return typeof detail === "string" && detail.length > 0
    ? `${family}::${detail}`
    : family;
}
