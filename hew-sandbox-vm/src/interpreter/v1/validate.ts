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

/// The first refusal, or `null` when every declared capability has a shim.
export function admitPackage(pkg: PackageV1): SandboxRejection | null {
  if (!pkg.entry) {
    return {
      code: "sandbox.entry.missing",
      capability: null,
      message: "package declares no entry function",
      span: null,
    };
  }

  for (const entry of pkg.runtime_families) {
    if (!resolveRuntimeShim(entry)) {
      return {
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
