import { UNIT, type VmValue } from "../values.js";

/** Authority comes only from the embedding host, never from a path or cwd. */
export interface DirectoryRoots {
  read?: FileSystemDirectoryHandle;
  write?: FileSystemDirectoryHandle;
}

export type BrowserFileOwner = {
  access: "read" | "write";
  handle: FileSystemFileHandle | null;
  cursor: number;
  closed: boolean;
  tail: Promise<void>;
};
export type BrowserDirectoryOwner = {
  access: "read" | "write";
  handle: FileSystemDirectoryHandle | null;
  closed: boolean;
};
export type BrowserFsOwner = BrowserFileOwner | BrowserDirectoryOwner;
export interface BrowserIoError {
  kind: number;
  message: string;
}
export interface BrowserIoResult {
  value: VmValue;
  error: BrowserIoError | null;
}

export const DIRECTORY_OPERATIONS = {
  hew_dir_open_read: {
    release: "hew_dir_read_close",
    action: "root",
    access: "read",
  },
  hew_dir_open_write: {
    release: "hew_dir_write_close",
    action: "root",
    access: "write",
  },
  hew_dir_read_subdir: {
    release: "hew_dir_read_close",
    action: "subdir",
    access: "read",
  },
  hew_dir_write_subdir: {
    release: "hew_dir_write_close",
    action: "subdir",
    access: "write",
  },
  hew_dir_read_open_file: {
    release: "hew_file_read_close",
    action: "file",
    access: "read",
  },
  hew_dir_write_open_file: {
    release: "hew_file_write_close",
    action: "file",
    access: "write",
  },
  hew_dir_read_is_valid: { action: "valid", access: "read" },
  hew_dir_write_is_valid: { action: "valid", access: "write" },
  hew_file_read_is_valid: { action: "valid", access: "read" },
  hew_file_write_is_valid: { action: "valid", access: "write" },
  hew_dir_read_close: { action: "close", access: "read" },
  hew_dir_write_close: { action: "close", access: "write" },
  hew_file_read_close: { action: "close", access: "read" },
  hew_file_write_close: { action: "close", access: "write" },
} as const;
export type DirectoryOperation =
  (typeof DIRECTORY_OPERATIONS)[keyof typeof DIRECTORY_OPERATIONS];

function errorOf(error: unknown): BrowserIoError {
  const name = error instanceof DOMException ? error.name : "";
  return {
    kind:
      name === "NotFoundError"
        ? 1
        : name === "NotAllowedError" || name === "SecurityError"
          ? 2
          : 0,
    message: error instanceof Error ? error.message : String(error),
  };
}
function pathOf(value: VmValue | undefined): string {
  if (value?.kind !== "string")
    throw new Error("directory operation requires a string path");
  const path = value.value;
  if (
    !path ||
    path.startsWith("/") ||
    path.includes("\\") ||
    path.includes("\0") ||
    /^[A-Za-z]:/.test(path)
  )
    throw new DOMException(
      "path must stay beneath the supplied directory",
      "NotAllowedError",
    );
  return path;
}
function ownerOf(
  value: VmValue | undefined,
  access: "read" | "write",
): BrowserFsOwner {
  if (
    value?.kind !== "host_resource" ||
    value.owner.closed ||
    !value.owner.handle ||
    value.owner.access !== access
  )
    throw new Error("closed or incompatible filesystem owner");
  return value.owner;
}

/** Resolve actual entries in order. A child starts a new confinement boundary;
 * parent components never recover the parent's authority. FSA exposes no symlink
 * traversal primitive: each lookup stays beneath its concrete directory handle. */
async function directory(
  root: FileSystemDirectoryHandle,
  parts: string[],
): Promise<FileSystemDirectoryHandle> {
  const stack = [root];
  for (const part of parts) {
    if (!part || part === ".") continue;
    if (part === "..") {
      if (stack.length === 1)
        throw new DOMException(
          "path escapes its directory owner",
          "NotAllowedError",
        );
      stack.pop();
    } else stack.push(await stack[stack.length - 1]!.getDirectoryHandle(part));
  }
  return stack[stack.length - 1]!;
}

export class BrowserDirectories {
  // The adapter retains its physical owner slots until selected release. This
  // also covers acquisition completed after a terminal VM failure, when its
  // source continuation must never run.
  private readonly owners = new Set<
    Extract<VmValue, { kind: "host_resource" }>
  >();

  retire(
    release: (value: Extract<VmValue, { kind: "host_resource" }>) => void,
  ): void {
    for (const value of this.owners) release(value);
  }

  /** OPFS has no symlink entries. Picker-backed filesystem link confinement is
   * not standardized, so this adapter refuses those handles before execution. */
  static async admission(
    roots: DirectoryRoots | undefined,
  ): Promise<string | null> {
    const supplied = [roots?.read, roots?.write].filter(
      (root): root is FileSystemDirectoryHandle => root !== undefined,
    );
    if (supplied.length === 0) return null;
    if (typeof navigator === "undefined" || !navigator.storage?.getDirectory)
      return "This host does not provide the browser origin-private filesystem.";
    try {
      const canonical = await navigator.storage.getDirectory();
      for (const root of supplied)
        if (
          root.kind !== "directory" ||
          (await canonical.resolve(root)) === null
        )
          return "Only supplied origin-private directory handles are supported; external picker directory confinement is unavailable.";
      return null;
    } catch (error) {
      return `Cannot validate supplied directory authority: ${errorOf(error).message}`;
    }
  }

  constructor(private readonly roots: DirectoryRoots = {}) {}

  call(
    operation: DirectoryOperation,
    args: VmValue[],
    resource?: number | null,
  ): VmValue | Promise<BrowserIoResult> {
    if (operation.action === "valid") {
      const value = args[0];
      return {
        kind: "bool",
        value:
          value?.kind === "host_resource" &&
          !value.owner.closed &&
          value.owner.handle !== null &&
          value.owner.access === operation.access,
      };
    }
    if (operation.action === "close") {
      const value = args[0];
      if (value?.kind !== "host_resource")
        throw new Error("close requires a filesystem owner");
      value.owner.closed = true;
      value.owner.handle = null;
      this.owners.delete(value);
      return UNIT;
    }
    if (resource == null)
      throw new Error(
        "filesystem acquisition has no compiler-selected owner slot",
      );
    return this.acquire(operation, args, resource);
  }

  private async acquire(
    operation: DirectoryOperation,
    args: VmValue[],
    resource: number,
  ): Promise<BrowserIoResult> {
    let owner: BrowserFsOwner =
      operation.action === "file"
        ? {
            access: operation.access,
            handle: null,
            cursor: 0,
            closed: false,
            tail: Promise.resolve(),
          }
        : { access: operation.access, handle: null, closed: false };
    try {
      const path = pathOf(args[operation.action === "root" ? 0 : 1]);
      const parent =
        operation.action === "root"
          ? this.roots[operation.access]
          : ownerOf(args[0], operation.access).handle;
      if (!parent || parent.kind !== "directory")
        throw new DOMException(
          "no supplied directory authority",
          "NotAllowedError",
        );
      const parts = path.split("/");
      if (operation.action === "file") {
        const name = parts.pop()!;
        if (!name || name === "." || name === "..")
          throw new Error("file path must name a regular file");
        const target = await directory(parent, parts);
        const file = await target.getFileHandle(name, {
          create: operation.access === "write",
        });
        if (operation.access === "write") {
          // Native open_write truncates before publishing the owner. FSA only
          // publishes writes on close, so close this truncation before resuming.
          const writer = await file.createWritable();
          try {
            await writer.close();
          } catch (error) {
            await writer.abort().catch(() => {});
            throw error;
          }
        }
        owner = {
          access: operation.access,
          handle: file,
          cursor: 0,
          closed: false,
          tail: Promise.resolve(),
        };
      } else owner.handle = await directory(parent, parts);
      return this.owned(resource, owner, null);
    } catch (error) {
      return this.owned(resource, owner, errorOf(error));
    }
  }

  private owned(
    resource: number,
    owner: BrowserFsOwner,
    error: BrowserIoError | null,
  ): BrowserIoResult {
    const value: Extract<VmValue, { kind: "host_resource" }> = {
      kind: "host_resource",
      resource,
      owner,
    };
    this.owners.add(value);
    return { value, error };
  }

  io(
    operation: "FileRead" | "FileWrite",
    args: VmValue[],
    cancelled: () => boolean,
  ): Promise<BrowserIoResult> {
    const fallback: VmValue =
      operation === "FileRead"
        ? { kind: "vector", elementType: "u8", items: [] }
        : { kind: "i64", value: -1n };
    let owner: BrowserFileOwner;
    try {
      const selected = ownerOf(
        args[0],
        operation === "FileRead" ? "read" : "write",
      );
      if (!("cursor" in selected))
        throw new Error("file operation requires a file owner");
      owner = selected;
    } catch (error) {
      return Promise.resolve({ value: fallback, error: errorOf(error) });
    }
    const result = owner.tail.then(async (): Promise<BrowserIoResult> => {
      try {
        if (cancelled()) return { value: fallback, error: null };
        const handle = owner.handle;
        if (!handle || owner.closed) throw new Error("closed file owner");
        if (operation === "FileRead") {
          const file = await handle.getFile();
          if (cancelled()) return { value: fallback, error: null };
          const bytes = new Uint8Array(
            await file.slice(owner.cursor).arrayBuffer(),
          );
          owner.cursor += bytes.length;
          return {
            value: {
              kind: "vector",
              elementType: "u8",
              items: Array.from(bytes, (value) => ({
                kind: "i64",
                value: BigInt(value),
              })),
            },
            error: null,
          };
        }
        const data = args[1];
        if (data?.kind !== "vector")
          throw new Error("file write requires bytes");
        const bytes = Uint8Array.from(data.items, (value) => {
          if (value.kind !== "i64")
            throw new Error("file write requires byte elements");
          return Number(value.value);
        });
        const writer = await handle.createWritable({ keepExistingData: true });
        try {
          if (cancelled()) {
            await writer.abort();
            return { value: fallback, error: null };
          }
          await writer.seek(owner.cursor);
          await writer.write(bytes);
          if (cancelled()) {
            await writer.abort();
            return { value: fallback, error: null };
          }
          await writer.close();
        } catch (error) {
          await writer.abort().catch(() => {});
          throw error;
        }
        owner.cursor += bytes.length;
        return { value: { kind: "i64", value: 0n }, error: null };
      } catch (error) {
        return { value: fallback, error: errorOf(error) };
      }
    });
    // Independent files progress independently; each cursor serializes its own
    // borrowed operations as the native file owner's lock does.
    owner.tail = result.then(() => {});
    return result;
  }
}
