import type { EncodingFormat, EncodingNode } from "./encoding-values.js";
import { HeapExceeded } from "./managed-budget.js";

export interface EncodingStorage {
  owners: number;
  bytes?: bigint;
  copyBytes?: bigint;
  capacities?: unknown;
  sharedCopy?: boolean;
}

export interface EncodingOwner {
  kind: "encoding";
  format: EncodingFormat;
  value: EncodingNode | null;
  storage?: EncodingStorage;
  bridge?: (request: string) => string;
  prepareMutation?: (replacement: EncodingOwner) => void;
}

interface ManagedTree {
  bytes: string;
  copy_bytes: string;
  shared_copy: boolean;
  capacities: unknown;
}

export function extractEncoding(
  value: EncodingOwner,
  child: EncodingNode | null,
  path: number[],
): EncodingOwner {
  if (!value.bridge || child === null)
    return encodingOwner(value.format, structuredClone(child), value.bridge);
  let capacities = encodingStorage(value).capacities;
  for (const index of path)
    capacities = Array.isArray(capacities) ? capacities[index] : undefined;
  const result = encodingOwner(value.format, child, value.bridge);
  storage(result).capacities = capacities;
  return native(result, "clone");
}

export function encodingOwner(
  format: EncodingFormat,
  value: EncodingNode | null,
  bridge?: EncodingOwner["bridge"],
  managed?: ManagedTree,
): EncodingOwner {
  return {
    kind: "encoding",
    format,
    value,
    bridge,
    storage: {
      owners: 1,
      ...(managed
        ? {
            bytes: BigInt(managed.bytes),
            copyBytes: BigInt(managed.copy_bytes),
            capacities: managed.capacities,
            sharedCopy: managed.shared_copy,
          }
        : {}),
    },
  };
}

function storage(value: EncodingOwner): EncodingStorage {
  return (value.storage ??= { owners: 1 });
}

function native(
  value: EncodingOwner,
  operation: string,
  fields: Record<string, unknown> = {},
): EncodingOwner {
  if (!value.bridge)
    throw new Error(
      "managed encoding storage requires the native codec bridge",
    );
  const response = JSON.parse(
    value.bridge(
      JSON.stringify({
        encoding_value: {
          format: value.format,
          operation,
          value: value.value,
          capacities: storage(value).capacities,
          ...fields,
        },
      }),
    ),
  ) as { value?: EncodingNode | null; managed?: ManagedTree; failure?: string };
  if (response.failure) {
    if (response.failure.includes("capacity overflow"))
      throw new HeapExceeded();
    throw new Error(response.failure);
  }
  return encodingOwner(
    value.format,
    response.value ?? null,
    value.bridge,
    response.managed,
  );
}

/** The native provider measures actual typed capacities. The VM retains this
 * whole-tree backing; no JavaScript allocation estimate enters its charge. */
export function encodingStorage(value: EncodingOwner): EncodingStorage {
  const retained = storage(value);
  if (value.value !== null && retained.bytes == null) {
    const measured = native(value, "retain");
    Object.assign(retained, measured.storage, { owners: retained.owners });
  }
  return retained;
}

export function cloneEncoding(
  value: EncodingOwner,
  preview = false,
): EncodingOwner {
  const retained =
    value.bridge && value.value !== null
      ? encodingStorage(value)
      : storage(value);
  if (retained.sharedCopy ?? value.format === "Yaml") {
    if (preview) return value;
    retained.owners++;
    return { ...value, storage: retained, prepareMutation: undefined };
  }
  if (value.bridge && value.value !== null) return native(value, "clone");
  return encodingOwner(
    value.format,
    structuredClone(value.value),
    value.bridge,
  );
}

export function releaseEncoding(value: EncodingOwner): void {
  if (value.value !== null) storage(value).owners--;
  value.value = null;
}

/** Stage the real provider operation before committing either consumed input.
 * A refused copy-on-write detach leaves both trees and aliases unchanged. */
export function mutateEncoding(
  value: EncodingOwner,
  operation: "array_push" | "object_set",
  child: EncodingOwner,
  key?: string,
): boolean {
  if (!value.bridge) {
    if (value.format === "Yaml" && storage(value).owners > 1) {
      storage(value).owners--;
      value.value = structuredClone(value.value);
      value.storage = { owners: 1 };
    }
    if (child.format === "Yaml" && storage(child).owners > 1) {
      storage(child).owners--;
      child.value = structuredClone(child.value);
      child.storage = { owners: 1 };
    }
    return false;
  }
  const retained = encodingStorage(value);
  const childStorage = encodingStorage(child);
  const replacement = native(value, operation, {
    detach: retained.sharedCopy && retained.owners > 1,
    child: child.value,
    child_capacities: childStorage.capacities,
    key,
    child_detach: childStorage.sharedCopy && childStorage.owners > 1,
  });
  value.prepareMutation?.(replacement);
  retained.owners--;
  value.value = replacement.value;
  value.storage = replacement.storage;
  releaseEncoding(child);
  return true;
}
