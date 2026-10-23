import type { VmValue } from "../values.js";

interface State {
  count: bigint;
  waiting: Set<Acquisition>;
}

export interface SemaphoreHandle {
  kind: "semaphore";
  state: State;
  closed: boolean;
}

type After = (nanos: bigint, ready: () => void) => () => void;

function handle(value: VmValue): SemaphoreHandle {
  if (value.kind !== "semaphore" || value.closed)
    throw new Error("expected a live semaphore handle");
  return value;
}

function release(state: State): void {
  const waiter = state.waiting.values().next().value;
  if (waiter) waiter.reserve();
  else if (state.count < 2_147_483_647n) state.count += 1n;
}

/** A released permit belongs to its selected waiter until the resume edge
 *  takes it. Cancellation before that edge hands the permit onwards. */
export class Acquisition {
  private status: "waiting" | "reserved" | "finished" = "waiting";
  private cancelTimer = () => {};

  constructor(
    private readonly state: State,
    private readonly ready: (acquired: boolean) => void,
  ) {}

  start(timeoutMs: bigint | null, after: After): void {
    if (this.state.count > 0n) {
      this.state.count -= 1n;
      this.reserve();
    } else if (timeoutMs !== null && timeoutMs <= 0n) {
      this.status = "finished";
      this.ready(false);
    } else {
      this.state.waiting.add(this);
      if (timeoutMs !== null)
        this.cancelTimer = after(timeoutMs * 1_000_000n, () => {
          if (this.status !== "waiting") return;
          this.detach();
          this.status = "finished";
          this.ready(false);
        });
    }
  }

  reserve(): void {
    this.detach();
    this.status = "reserved";
    this.ready(true);
  }

  take(): void {
    if (this.status === "reserved") this.status = "finished";
  }

  cancel(): void {
    this.detach();
    const reserved = this.status === "reserved";
    this.status = "finished";
    if (reserved) release(this.state);
  }

  private detach(): void {
    this.state.waiting.delete(this);
    this.cancelTimer();
    this.cancelTimer = () => {};
  }
}

/** The VM executes the compiler's semaphore operations against one shared
 *  permit count. Explicit clones own independent handles, not independent counts. */
export class Semaphores {
  constructor(private readonly after: After) {}

  create(count: bigint): VmValue {
    if (count < 0n || count > 2_147_483_647n)
      throw new Error("semaphore capacity is outside the supported range");
    return {
      kind: "semaphore",
      state: { count, waiting: new Set() },
      closed: false,
    };
  }

  clone(value: VmValue): VmValue {
    return { ...handle(value), closed: false };
  }

  close(value: VmValue): void {
    if (value.kind !== "semaphore")
      throw new Error("expected a semaphore handle");
    value.closed = true;
  }

  count(value: VmValue): bigint {
    return handle(value).state.count;
  }

  tryAcquire(value: VmValue): boolean {
    const state = handle(value).state;
    if (state.count === 0n) return false;
    state.count -= 1n;
    return true;
  }

  release(value: VmValue): void {
    release(handle(value).state);
  }

  acquire(
    value: VmValue,
    timeoutMs: bigint | null,
    ready: (acquired: boolean) => void,
  ): Acquisition {
    const operation = new Acquisition(handle(value).state, ready);
    operation.start(timeoutMs, this.after);
    return operation;
  }
}
