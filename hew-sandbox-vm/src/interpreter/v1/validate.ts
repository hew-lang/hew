import { calendarResultFields } from "./calendar.js";
/// Load-time admission for a verified SIR package.
///
/// The package declares what it needs: `runtime_families` names every runtime
/// call family the instruction stream reaches, and `externs` names every
/// declared C-ABI symbol. Admission consumes the same typed host registrations
/// as execution, including resumable operations and required Wasm bridges.
/// An unavailable operation is refused before a single instruction runs.
///
/// The check keys on the declared identity: it never matches a symbol prefix
/// and never reads an instruction stream to guess a capability.

import type { SandboxRejection } from "../types.js";
import type { CheckedPackage } from "../schema-validator.js";
import {
  CODEC_REGISTRATION,
  registrationRefusal,
  resolveExtern,
  resolveRuntime,
  resolveSuspension,
  suspensionRefusal,
  type HostBridges,
  type Refused,
} from "./shims.js";

const UNSUPPORTED = "sandbox.capability.unsupported";

/// The executor can only be constructed after structural/reference checks and
/// the canonical host registration have both accepted the same package.
export class AdmittedPackage {
  private constructor(readonly bytecode: CheckedPackage) {}

  static load(
    pkg: CheckedPackage,
    bridges: HostBridges,
  ): AdmittedPackage | SandboxRejection {
    return admitPackage(pkg, bridges) ?? new AdmittedPackage(pkg);
  }
}

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

/// The first refusal, or `null` when all operations and bridges are available.
export function admitPackage(
  pkg: CheckedPackage,
  bridges: HostBridges = {},
): SandboxRejection | null {
  if (!pkg.entry) {
    return {
      category: "invalid_package",
      code: "sandbox.entry.missing",
      capability: null,
      message: "package declares no entry function",
      span: null,
    };
  }

  const opaqueResources = new Set<number>();
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
      const term = block.term;
      if (
        term.op === "runtime.call" &&
        pkg.runtime_families[term.family]?.family === "TimestampNow"
      ) {
        const shape =
          term.aggregate_result_shape == null
            ? undefined
            : pkg.aggregates[term.aggregate_result_shape];
        if (
          !shape ||
          shape.builtin !== "Timestamp" ||
          shape.fields.length !== 2
        ) {
          return {
            category: "invalid_package",
            code: "sandbox.package.timestamp_shape",
            capability: "TimestampNow",
            message: "timestamp clock lacks its checked result shape",
            span: null,
          };
        }
      }
      if (term.op === "extern.call") {
        const symbol = pkg.externs[term.extern]?.symbol ?? "";
        const fields = calendarResultFields(symbol);
        if (fields) {
          const shape =
            term.aggregate_result_shape == null
              ? undefined
              : pkg.aggregates[term.aggregate_result_shape];
          if (
            !shape ||
            shape.fields.length !== fields.length ||
            shape.fields.some((field, index) => field !== fields[index])
          ) {
            return {
              category: "invalid_package",
              code: "sandbox.package.calendar_shape",
              capability: symbol,
              message: "calendar operation lacks its checked result shape",
              span: null,
            };
          }
        }
      }
      if (term.op === "timestamp.binary") {
        const both = term.lhs_timestamp && term.rhs_timestamp;
        const validOperation =
          (term.binary_op === "Add" &&
            term.lhs_timestamp !== term.rhs_timestamp) ||
          (term.binary_op === "Subtract" && term.lhs_timestamp);
        const shape =
          term.shape === null ? undefined : pkg.aggregates[term.shape];
        if (
          typeof term.lhs_timestamp !== "boolean" ||
          typeof term.rhs_timestamp !== "boolean" ||
          !validOperation ||
          (both
            ? term.shape !== null
            : !shape ||
              shape.fields.length !== 2 ||
              shape.builtin !== "Timestamp") ||
          term.failures.length !== 1 ||
          term.failures[0]?.trap !== "integer_overflow"
        ) {
          return {
            category: "invalid_package",
            code: "sandbox.package.timestamp_shape",
            capability: "timestamp.binary",
            message:
              "timestamp arithmetic lacks its checked operands, result shape or overflow edge",
            span: null,
          };
        }
      }
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
      if (term.op === "extern.call") {
        const registration = resolveExtern(pkg.externs[term.extern]!);
        if (
          registration.kind === "directory" &&
          "release" in registration.operation
        ) {
          const resource =
            term.result_resource == null
              ? undefined
              : pkg.resources?.[term.result_resource];
          if (
            resource?.kind !== "nominal" ||
            resource.release !== registration.operation.release ||
            term.result == null ||
            term.result === "never"
          )
            return {
              category: "invalid_package",
              code: "sandbox.package.owner_result",
              capability: null,
              message:
                "filesystem acquisition lacks its compiler-selected owner release slot",
              span: null,
            };
        }
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
        opaqueResources.add(operation.resource);
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
      if (term.op === "wire.codec") {
        const refusal = registrationRefusal(CODEC_REGISTRATION, bridges);
        if (refusal) return rejectRegistration(refusal);
      }
      if (term.op === "suspend") {
        const registration = resolveSuspension(term);
        if (registration.kind === "refused")
          return rejectRegistration(registration);
        const mandatoryClose = (pkg.resources ?? []).some(
          (resource) => "close" in resource && resource.close === function_.id,
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
    }
  }

  for (const [index, resource] of (pkg.resources ?? []).entries()) {
    if (resource.kind === "opaque" && !opaqueResources.has(index))
      return unavailable("resource.opaque_close");
    if (
      resource.kind === "nominal" &&
      registrationRefusal(
        resolveExtern({ symbol: resource.release ?? "" }),
        bridges,
      )
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
    const refusal = registrationRefusal(resolveRuntime(entry), bridges);
    if (refusal) return rejectRegistration(refusal);
  }

  for (const entry of pkg.externs) {
    const refusal = registrationRefusal(resolveExtern(entry), bridges);
    if (refusal) return rejectRegistration(refusal);
  }

  for (const kind of pkg.suspend_kinds) {
    const refusal = suspensionRefusal(kind);
    if (refusal) return rejectRegistration(refusal);
  }

  return null;
}

function rejectRegistration(refusal: Refused): SandboxRejection {
  return {
    category: refusal.category,
    code: UNSUPPORTED,
    capability: refusal.capability,
    message:
      refusal.message ??
      (refusal.capability === "codecBridge" ||
      refusal.capability === "calendarBridge"
        ? `This operation requires the ${refusal.capability === "codecBridge" ? "codec" : "calendar"} Wasm bridge.`
        : refusal.description !== undefined
          ? `${refusal.description} require native execution.`
          : capabilityMessage(
              refusal.capability.split("::")[0]!,
              refusal.category === "native_only",
            )),
    span: null,
  };
}
