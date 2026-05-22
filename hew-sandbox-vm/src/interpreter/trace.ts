import type {
  JsonValue,
  ReplayConfig,
  RuntimeFailure,
  RuntimeStatus,
  SandboxBytecodePackage,
  SandboxTrace,
  TraceEvent,
  TraceSpan,
  UnsupportedDiagnostic
} from "./types.js";
import { canonicalJson } from "./values.js";

export class TraceBuilder {
  readonly stdout: string[] = [];
  readonly stderr: string[] = [];
  readonly runtimeFailures: RuntimeFailure[] = [];
  status: RuntimeStatus = "ok";
  stepCount = 0;
  exitCode = 0;

  private readonly events: TraceEvent[] = [];
  private readonly spanById = new Map<string, TraceSpan>();
  private readonly ids = {
    actors: ["actor:root"],
    channels: [] as string[],
    tasks: [] as string[],
    supervisors: [] as string[],
    machines: [] as string[]
  };
  private readonly virtualClock: ReplayConfig["virtual_clock"];

  constructor(
    private readonly bytecode: SandboxBytecodePackage,
    private readonly fixtureId: string,
    private readonly traceId: string,
    readonly replay: ReplayConfig,
    private readonly sandboxVersion: string
  ) {
    this.virtualClock = { ...replay.virtual_clock };
    const sourceById = new Map(bytecode.source_map.sources.map((source) => [source.id, source]));
    for (const span of bytecode.source_map.spans) {
      const source = sourceById.get(span.source_id);
      if (source) {
        this.spanById.set(span.id, {
          source_id: span.source_id,
          path: source.path,
          start: span.start,
          end: span.end
        });
      }
    }
    this.push({ type: "trace.started", phase: "run", span: null, message: "fixture start" });
  }

  get budgetRemaining(): number {
    return Math.max(0, this.replay.step_budget - this.stepCount);
  }

  get virtualTimeMs(): number {
    return this.virtualClock.current_ms;
  }

  clockSnapshot(): ReplayConfig["virtual_clock"] {
    return { ...this.virtualClock };
  }

  span(spanRef: string | null | undefined): TraceSpan | null {
    return spanRef ? this.spanById.get(spanRef) ?? null : null;
  }

  push(event: Omit<TraceEvent, "seq">): void {
    this.events.push({ seq: this.events.length, ...event });
  }

  writeStdout(text: string, span: TraceSpan | null): void {
    this.stdout.push(text);
    this.push({ type: "io.stdout", phase: "run", span, text });
  }

  writeStderr(text: string, span: TraceSpan | null): void {
    this.stderr.push(text);
    this.push({ type: "io.stderr", phase: "run", span, text });
  }

  allocateId(kind: "actor" | "channel" | "task" | "supervisor" | "machine", id: string, parentId: string | null): void {
    const bucket = this.ids[`${kind}s` as keyof typeof this.ids];
    if (!bucket.includes(id)) {
      bucket.push(id);
    }
    this.push({
      type: "runtime.id_allocated",
      phase: "run",
      span: null,
      id_kind: kind,
      id,
      parent_id: parentId
    });
  }

  snapshot(message: string, payload: JsonValue, span: TraceSpan | null = null): void {
    this.push({
      type: "state.snapshot",
      phase: "run",
      span,
      message,
      text: canonicalJson(payload)
    });
  }

  recordReplayInput(input: { kind: string; data: JsonValue }, persist = true): void {
    if (persist) {
      this.replay.inputs.push(input);
    }
    this.push({
      type: "replay.input",
      phase: "replay",
      span: null,
      replay_input: input
    });
  }

  advanceVirtualClock(amountMs: number, span: TraceSpan | null): void {
    this.virtualClock.current_ms += amountMs;
    this.push({
      type: "clock.virtual_advance",
      phase: "run",
      span,
      amount_ms: amountMs,
      clock: { ...this.virtualClock }
    });
  }

  exit(code: number, span: TraceSpan | null): void {
    this.exitCode = code;
    this.snapshot("sandbox-exit", { exit_code: code }, span);
  }

  fail(status: RuntimeStatus, eventType: "runtime.failure" | "budget.exhausted", failure: RuntimeFailure): void {
    this.status = status;
    this.runtimeFailures.push(failure);
    this.push({
      type: eventType,
      phase: "run",
      span: failure.span,
      ...(eventType === "budget.exhausted"
        ? { step_count: this.stepCount, budget_remaining: this.budgetRemaining }
        : {}),
      failure
    });
  }

  stepCommitted(message?: string, id?: string): void {
    this.push({
      type: "step.committed",
      phase: "run",
      span: null,
      ...(message ? { message } : {}),
      ...(id ? { id_kind: "actor" as const, id } : {}),
      step_count: this.stepCount,
      budget_remaining: this.budgetRemaining
    });
  }

  finish(): SandboxTrace {
    this.push({ type: "trace.ended", phase: "run", span: null, message: this.status });
    return {
      schema_version: "hew.sandbox.trace.v0",
      trace_id: this.traceId,
      fixture_id: this.fixtureId,
      profile: this.bytecode.profile,
      hew_version: this.bytecode.hew_version,
      sandbox_version: this.sandboxVersion,
      result: this.status,
      replay: this.replay,
      events: this.events,
      final_state: {
        status: this.status,
        exit_code: this.status === "ok" ? this.exitCode : null,
        step_count: this.stepCount,
        budget_remaining: this.budgetRemaining,
        virtual_clock: this.virtualClock,
        stdout: this.stdout,
        stderr: this.stderr,
        ids: {
          actors: this.ids.actors,
          channels: this.ids.channels,
          tasks: this.ids.tasks,
          supervisors: this.ids.supervisors,
          machines: this.ids.machines
        },
        diagnostics: [],
        sandbox_rejections: [],
        runtime_failures: this.runtimeFailures,
        globals: []
      }
    };
  }
}

export function runtimeFailure(
  kind: RuntimeFailure["kind"],
  message: string,
  trapKind: RuntimeFailure["trap_kind"],
  span: TraceSpan | null,
  unsupported?: UnsupportedDiagnostic
): RuntimeFailure {
  return unsupported ? { kind, message, span, trap_kind: trapKind, unsupported } : { kind, message, span, trap_kind: trapKind };
}
