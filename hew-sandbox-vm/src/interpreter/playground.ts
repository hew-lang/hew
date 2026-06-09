import type { JsonValue, SandboxTrace, TraceEvent } from "./types.js";
import { canonicalJson } from "./values.js";

const SHARE_SCHEMA_VERSION = "hew.sandbox.share.v0";
const PLAYGROUND_SCHEMA_VERSION = "hew.sandbox.playground.v0";
const MAX_SHARE_PAYLOAD_BYTES = 256 * 1024;

export interface PlaygroundSharePayload {
  schema_version: typeof SHARE_SCHEMA_VERSION;
  source: string;
  seed: number;
  fixture_id?: string;
  profile?: string;
}

export interface PlaygroundState {
  schema_version: typeof PLAYGROUND_SCHEMA_VERSION;
  trace_id: string;
  fixture_id: string;
  status: SandboxTrace["result"];
  source_diagnostics: JsonValue[];
  output: {
    stdout: string[];
    stderr: string[];
    page_log: string[];
  };
  runtime_trace: TraceEvent[];
  views: {
    actor_mailboxes: JsonValue[];
    supervisor_tree: JsonValue[];
    channel_queues: JsonValue[];
    machine_transitions: JsonValue[];
  };
  controls: {
    virtual_clock: SandboxTrace["final_state"]["virtual_clock"];
    seed: number;
    replay: SandboxTrace["replay"];
    stop_reset: { can_stop: boolean; reset_replay: SandboxTrace["replay"] };
  };
  share: {
    seed: number;
    profile: string;
  };
  trace_download: {
    filename: string;
    json: string;
  };
}

export interface LessonVirtualFile {
  id: string;
  name: string;
  path: string;
  source: string;
  seed: number;
}

export function buildPlaygroundState(trace: SandboxTrace): PlaygroundState {
  const snapshots = trace.events
    .filter((event) => event.type === "state.snapshot" && typeof event.text === "string")
    .map((event) => ({ event, payload: JSON.parse(event.text!) as JsonValue }));
  return {
    schema_version: PLAYGROUND_SCHEMA_VERSION,
    trace_id: trace.trace_id,
    fixture_id: trace.fixture_id,
    status: trace.result,
    source_diagnostics: diagnosticsForTrace(trace),
    output: {
      stdout: [...trace.final_state.stdout],
      stderr: [...trace.final_state.stderr],
      page_log: snapshots
        .filter(({ event }) => event.message === "page.log")
        .map(({ payload }) => (isRecord(payload) && typeof payload.text === "string" ? payload.text : canonicalJson(payload)))
    },
    runtime_trace: trace.events.map((event) => ({ ...event })),
    views: {
      actor_mailboxes: snapshots
        .filter(({ event }) => event.message?.startsWith("actor.") === true)
        .map(({ event, payload }) => ({ family: event.message ?? "state.snapshot", ...recordOrPayload(payload) })),
      supervisor_tree: snapshots
        .filter(({ event }) => event.message?.startsWith("supervisor") === true || event.message?.startsWith("child-") === true || event.message === "restart-decision")
        .map(({ event, payload }) => ({ family: event.message ?? "state.snapshot", ...recordOrPayload(payload) })),
      channel_queues: snapshots
        .filter(({ event }) => event.message?.startsWith("channel-") === true)
        .map(({ event, payload }) => ({ family: event.message ?? "state.snapshot", ...recordOrPayload(payload) })),
      machine_transitions: snapshots
        .filter(({ event }) => event.message?.startsWith("machine.") === true)
        .map(({ event, payload }) => ({ family: event.message ?? "state.snapshot", ...recordOrPayload(payload) }))
    },
    controls: {
      virtual_clock: { ...trace.final_state.virtual_clock },
      seed: trace.replay.seed,
      replay: cloneReplay(trace.replay),
      stop_reset: {
        can_stop: trace.result === "ok",
        reset_replay: {
          seed: trace.replay.seed,
          step_budget: trace.replay.step_budget,
          virtual_clock: { epoch_ms: trace.replay.virtual_clock.epoch_ms, tick_ms: trace.replay.virtual_clock.tick_ms, current_ms: trace.replay.virtual_clock.epoch_ms },
          inputs: []
        }
      }
    },
    share: {
      seed: trace.replay.seed,
      profile: trace.profile
    },
    trace_download: {
      filename: `${trace.fixture_id}.trace.json`,
      json: traceToDownloadJson(trace)
    }
  };
}

export function encodePlaygroundShare(payload: Omit<PlaygroundSharePayload, "schema_version">): string {
  const normalized: PlaygroundSharePayload = {
    schema_version: SHARE_SCHEMA_VERSION,
    source: payload.source,
    seed: payload.seed,
    ...(payload.fixture_id ? { fixture_id: payload.fixture_id } : {}),
    ...(payload.profile ? { profile: payload.profile } : {})
  };
  validateSharePayload(normalized);
  const json = canonicalJson(normalized as unknown as JsonValue);
  const encoded = base64UrlEncode(json);
  if (encoded.length > MAX_SHARE_PAYLOAD_BYTES) {
    throw new PlaygroundIntegrationError("share_payload_too_large", "share payload exceeds sandbox playground limit");
  }
  const params = new URLSearchParams();
  params.set("hew", encoded);
  return `?${params.toString()}`;
}

export function decodePlaygroundShare(input: string): PlaygroundSharePayload {
  const params = input.includes("?")
    ? new URL(input, "https://sandbox.invalid/").searchParams
    : new URLSearchParams(input.startsWith("hew=") ? input : `hew=${input}`);
  const encoded = params.get("hew");
  if (!encoded) {
    throw new PlaygroundIntegrationError("share_payload_missing", "share link is missing the hew payload");
  }
  if (encoded.length > MAX_SHARE_PAYLOAD_BYTES) {
    throw new PlaygroundIntegrationError("share_payload_too_large", "share payload exceeds sandbox playground limit");
  }
  let payload: unknown;
  try {
    payload = JSON.parse(base64UrlDecode(encoded)) as unknown;
  } catch (error) {
    throw new PlaygroundIntegrationError("share_payload_invalid", `share payload is not valid JSON: ${error instanceof Error ? error.message : String(error)}`);
  }
  validateSharePayload(payload);
  return payload;
}

export function traceToDownloadJson(trace: SandboxTrace): string {
  return `${canonicalJson(trace as unknown as JsonValue)}\n`;
}

export function validateLessonManifest(manifest: unknown, sources: Readonly<Record<string, string>> = {}): LessonVirtualFile[] {
  if (!Array.isArray(manifest)) {
    throw new PlaygroundIntegrationError("lesson_manifest_invalid", "lesson manifest must be an array");
  }
  const seen = new Set<string>();
  return manifest.map((entry, index) => {
    if (!isRecord(entry)) {
      throw new PlaygroundIntegrationError("lesson_manifest_invalid", `lesson entry ${index} must be an object`);
    }
    const id = stringField(entry, "id", index);
    const name = stringField(entry, "name", index);
    const path = stringField(entry, "source_path", index);
    if (seen.has(id)) {
      throw new PlaygroundIntegrationError("lesson_manifest_invalid", `duplicate lesson id ${id}`);
    }
    seen.add(id);
    if (path.startsWith("/") || path.includes("..") || path.includes("\\")) {
      throw new PlaygroundIntegrationError("lesson_manifest_invalid", `lesson ${id} has an unsafe source path`);
    }
    const source = typeof entry.source === "string" ? entry.source : sources[path];
    if (typeof source !== "string" || source.length === 0) {
      throw new PlaygroundIntegrationError("lesson_manifest_invalid", `lesson ${id} is missing source content`);
    }
    const seed = typeof entry.seed === "number" && Number.isInteger(entry.seed) && entry.seed >= 0 ? entry.seed : 0;
    return { id, name, path, source, seed };
  });
}

export class PlaygroundIntegrationError extends Error {
  constructor(readonly code: string, message: string) {
    super(message);
    this.name = "PlaygroundIntegrationError";
  }
}

function diagnosticsForTrace(trace: SandboxTrace): JsonValue[] {
  const diagnostics = trace.events
    .filter((event) => event.type === "compile.diagnostic")
    .map((event) => (event as TraceEvent & { diagnostic?: JsonValue }).diagnostic ?? null)
    .filter((diagnostic): diagnostic is JsonValue => diagnostic !== null);
  return [...diagnostics, ...(trace.final_state.diagnostics as JsonValue[])];
}

function cloneReplay(replay: SandboxTrace["replay"]): SandboxTrace["replay"] {
  return {
    seed: replay.seed,
    step_budget: replay.step_budget,
    virtual_clock: { ...replay.virtual_clock },
    inputs: replay.inputs.map((input) => ({ kind: input.kind, data: input.data }))
  };
}

function validateSharePayload(payload: unknown): asserts payload is PlaygroundSharePayload {
  if (!isRecord(payload) || payload.schema_version !== SHARE_SCHEMA_VERSION) {
    throw new PlaygroundIntegrationError("share_payload_invalid", "share payload has an invalid schema version");
  }
  if (typeof payload.source !== "string") {
    throw new PlaygroundIntegrationError("share_payload_invalid", "share payload source must be a string");
  }
  if (typeof payload.seed !== "number" || !Number.isInteger(payload.seed) || payload.seed < 0) {
    throw new PlaygroundIntegrationError("share_payload_invalid", "share payload seed must be a non-negative integer");
  }
  if ("fixture_id" in payload && typeof payload.fixture_id !== "string") {
    throw new PlaygroundIntegrationError("share_payload_invalid", "share payload fixture id must be a string");
  }
  if ("profile" in payload && typeof payload.profile !== "string") {
    throw new PlaygroundIntegrationError("share_payload_invalid", "share payload profile must be a string");
  }
}

function base64UrlEncode(value: string): string {
  const bytes = new TextEncoder().encode(value);
  let binary = "";
  for (let index = 0; index < bytes.length; index += 8192) {
    binary += String.fromCharCode(...bytes.slice(index, index + 8192));
  }
  return btoa(binary).replaceAll("+", "-").replaceAll("/", "_").replace(/=+$/u, "");
}

function base64UrlDecode(value: string): string {
  const normalized = value.replaceAll("-", "+").replaceAll("_", "/");
  const padded = normalized.padEnd(normalized.length + ((4 - (normalized.length % 4)) % 4), "=");
  const binary = atob(padded);
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) {
    bytes[index] = binary.charCodeAt(index);
  }
  return new TextDecoder().decode(bytes);
}

function recordOrPayload(value: JsonValue): Record<string, JsonValue> {
  return isRecord(value) ? value : { value };
}

function stringField(record: Record<string, unknown>, field: string, index: number): string {
  const value = record[field];
  if (typeof value !== "string" || value.length === 0) {
    throw new PlaygroundIntegrationError("lesson_manifest_invalid", `lesson entry ${index} missing ${field}`);
  }
  return value;
}

function isRecord(value: unknown): value is Record<string, JsonValue> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}
