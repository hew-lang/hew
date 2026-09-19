import type { TraceBuilder } from "../trace.js";
import { SeededPrng } from "../../scheduler/prng.js";

interface ReadyFrame {
  id: string;
  ticket: number;
  resume(): void;
}

/// The VM runs a frame until it returns or parks on a SIR suspension. A wake
/// queues that same frame; no actor turn recursively runs another actor.
export class FrameScheduler {
  private readonly ready: ReadyFrame[] = [];
  private readonly timers: Array<{ at: number; wake(): void }> = [];
  private readonly random: SeededPrng;
  private readonly replaySteps: Array<{ id: string; ticket: number }>;
  private nextTicket = 0;
  private replayIndex = 0;

  constructor(
    private readonly trace: TraceBuilder,
    private readonly policy: "round_robin" | "chaos",
  ) {
    this.random = new SeededPrng(trace.replay.seed);
    this.replaySteps = trace.replay.inputs.flatMap((input) => {
      const data = input.data;
      return input.kind === "user_event" &&
        data !== null &&
        typeof data === "object" &&
        !Array.isArray(data) &&
        data.family === "actor.scheduler-step" &&
        typeof data.actor_id === "string" &&
        typeof data.ready_id === "number"
        ? [{ id: data.actor_id, ticket: data.ready_id }]
        : [];
    });
  }

  enqueue(id: string, resume: () => void): void {
    this.ready.push({ id, ticket: ++this.nextTicket, resume });
  }

  after(nanos: bigint, wake: () => void): () => void {
    const timer = {
      at: this.trace.virtualTimeMs + Math.max(0, Number(nanos) / 1_000_000),
      wake,
    };
    this.timers.push(timer);
    return () => {
      const index = this.timers.indexOf(timer);
      if (index >= 0) this.timers.splice(index, 1);
    };
  }

  run(): void {
    while (this.ready.length > 0 || this.timers.length > 0) {
      if (this.ready.length === 0) {
        const next = Math.min(...this.timers.map((timer) => timer.at));
        this.trace.advanceVirtualClock(next - this.trace.virtualTimeMs, null);
        for (let index = 0; index < this.timers.length; ) {
          if (this.timers[index]!.at <= next) {
            this.timers.splice(index, 1)[0]!.wake();
          } else {
            index += 1;
          }
        }
      }
      if (this.ready.length === 0) continue;
      const replaying = this.replaySteps.length > 0;
      let index =
        this.policy === "chaos" ? this.random.nextIndex(this.ready.length) : 0;
      if (replaying) {
        const step = this.replaySteps[this.replayIndex++];
        index = this.ready.findIndex(
          (frame) => frame.id === step?.id && frame.ticket === step?.ticket,
        );
        if (index < 0)
          throw new Error(
            `replay scheduler frame ${step?.id}/${step?.ticket} is not ready`,
          );
      }
      const frame = this.ready.splice(index, 1)[0]!;
      this.trace.stepCommitted("actor.scheduler-step", frame.id);
      this.trace.recordReplayInput(
        {
          kind: "user_event",
          data: {
            family: "actor.scheduler-step",
            actor_id: frame.id,
            ready_id: frame.ticket,
          },
        },
        !replaying,
      );
      frame.resume();
    }
  }
}
