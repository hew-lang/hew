import { type VmValue } from "../values.js";
import { HeapExceeded } from "./managed-budget.js";

interface PendingSend {
  value: VmValue;
  complete(status: number): void;
}

interface PendingRead {
  complete(
    value: VmValue | null,
    fault: string | null,
    released?: () => void,
  ): void;
}

interface Envelope {
  value: VmValue;
  release?: () => void;
}

interface Receiver {
  owner: object;
  acquire(envelope: Envelope): () => void;
  refused(): void;
}

interface Pipe {
  capacity: number;
  queue: Envelope[];
  receiver?: Receiver;
  sinks: number;
  readerOpen: boolean;
  fault: string | null;
  sending: PendingSend[];
  reading: PendingRead[];
  observers: Set<() => void>;
}

interface Half {
  pipe: Pipe;
  kind: "sink" | "stream";
  open: boolean;
  retiring?: Array<() => void>;
}

/// The VM's one bounded pipe substrate. Selection observes readiness; only
/// the winning receive removes an item. Values transfer without a snapshot.
export class Pipes {
  private readonly pairs = new Map<string, Pipe>();
  private readonly halves = new Map<string, Half>();
  private next = 0;
  private managedReceivers = 0;

  get hasManagedReceivers(): boolean {
    return this.managedReceivers !== 0;
  }

  create(id: string, capacity: number): VmValue {
    this.pairs.set(id, {
      capacity,
      queue: [],
      sinks: 0,
      readerOpen: true,
      fault: null,
      sending: [],
      reading: [],
      observers: new Set(),
    });
    return { kind: "channel", id };
  }

  freePair(pair: VmValue): void {
    if (pair.kind === "channel") this.pairs.delete(pair.id);
  }

  extract(pair: VmValue, kind: "sink" | "stream"): VmValue {
    if (pair.kind !== "channel") throw new Error("pipe pair is not a channel");
    const pipe = this.pairs.get(pair.id);
    if (!pipe) throw new Error("pipe pair does not exist");
    return this.half(pipe, kind);
  }

  private half(pipe: Pipe, kind: "sink" | "stream", open = true): VmValue {
    const id = `pipe-half:${++this.next}`;
    this.halves.set(id, { pipe, kind, open });
    if (kind === "sink" && open) pipe.sinks += 1;
    return { kind, channelId: id };
  }

  private get(value: VmValue): Half {
    if (value.kind !== "sink" && value.kind !== "stream")
      throw new Error("expected a pipe half");
    const half = this.halves.get(value.channelId);
    if (!half) throw new Error("pipe half does not exist");
    return half;
  }

  cloneSink(value: VmValue): VmValue {
    const half = this.get(value);
    if (half.kind !== "sink")
      throw new Error("cannot clone a stream as a sink");
    return this.half(half.pipe, "sink", half.open);
  }

  isClosed(value: VmValue): boolean {
    return !this.get(value).open;
  }

  close(value: VmValue, fault: string | null = null): VmValue[] {
    const half = this.get(value);
    if (!half.open) return [];
    const discarded: VmValue[] = [];
    half.open = false;
    if (half.kind === "sink") {
      half.pipe.sinks -= 1;
      half.pipe.fault ??= fault;
    } else {
      half.pipe.readerOpen = false;
      if (half.pipe.receiver) this.managedReceivers--;
      half.pipe.receiver = undefined;
      for (const item of half.pipe.queue.splice(0)) {
        discarded.push(item.value);
        if (item.release) (half.retiring ??= []).push(item.release);
      }
    }
    this.flush(half.pipe);
    return discarded;
  }

  finishClose(value: VmValue): void {
    if (value.kind !== "stream" && value.kind !== "sink") return;
    for (const release of this.halves
      .get(value.channelId)
      ?.retiring?.splice(0) ?? [])
      release();
  }

  /** Called only after ownership acquisition succeeds. The receiving half
   * carries queued envelopes; producer aliases never acquire them. */
  bind(value: VmValue, receiver?: Receiver): void {
    const half = this.get(value);
    if (half.kind !== "stream" || half.pipe.receiver?.owner === receiver?.owner)
      return;
    this.managedReceivers += Number(!!receiver) - Number(!!half.pipe.receiver);
    half.pipe.receiver = receiver;
    for (const envelope of half.pipe.queue) {
      const release = receiver?.acquire(envelope);
      envelope.release?.();
      envelope.release = release;
    }
  }

  private admit(pipe: Pipe, value: VmValue): Envelope | null {
    const envelope = { value } as Envelope;
    try {
      envelope.release = pipe.receiver?.acquire(envelope);
      return envelope;
    } catch (error) {
      if (!(error instanceof HeapExceeded)) throw error;
      // Acquisition is atomic. A refused recipient follows its existing
      // checked failure path while the producer still owns the item.
      pipe.readerOpen = false;
      pipe.fault = "actor heap limit exceeded";
      pipe.receiver?.refused();
      this.flush(pipe);
      return null;
    }
  }

  peerClosed(value: VmValue): boolean {
    return !this.get(value).pipe.readerOpen;
  }

  readable(value: VmValue): boolean {
    const pipe = this.get(value).pipe;
    return (
      pipe.queue.length > 0 ||
      pipe.sending.length > 0 ||
      pipe.sinks === 0 ||
      pipe.fault !== null ||
      !pipe.readerOpen
    );
  }

  observe(value: VmValue, changed: () => void): () => void {
    const observers = this.get(value).pipe.observers;
    observers.add(changed);
    return () => observers.delete(changed);
  }

  send(
    value: VmValue,
    item: VmValue,
    park: boolean,
    complete: (status: number) => void,
  ): () => void {
    const half = this.get(value);
    const pipe = half.pipe;
    if (!half.open || !pipe.readerOpen) {
      complete(1);
      return () => {};
    }
    if (pipe.reading.length > 0) {
      const envelope = this.admit(pipe, item);
      if (!envelope) {
        complete(1);
        return () => {};
      }
      pipe.reading.shift()!.complete(item, null, envelope.release);
      complete(0);
    } else if (pipe.queue.length < pipe.capacity) {
      const envelope = this.admit(pipe, item);
      if (!envelope) {
        complete(1);
        return () => {};
      }
      pipe.queue.push(envelope);
      complete(0);
    } else if (!park) {
      complete(2);
    } else {
      const pending = { value: item, complete };
      pipe.sending.push(pending);
      this.notify(pipe);
      return () => {
        const index = pipe.sending.indexOf(pending);
        if (index >= 0) pipe.sending.splice(index, 1);
      };
    }
    this.notify(pipe);
    return () => {};
  }

  receive(
    value: VmValue,
    park: boolean,
    complete: PendingRead["complete"],
  ): () => void {
    const pipe = this.get(value).pipe;
    if (pipe.queue.length > 0) {
      const envelope = pipe.queue.shift()!;
      complete(envelope.value, null, envelope.release);
      this.flush(pipe);
    } else if (pipe.sending.length > 0) {
      const sender = pipe.sending.shift()!;
      const envelope = this.admit(pipe, sender.value);
      if (envelope) {
        complete(sender.value, null, envelope.release);
        sender.complete(0);
      } else {
        complete(null, pipe.fault);
        sender.complete(1);
      }
    } else if (
      pipe.fault !== null ||
      pipe.sinks === 0 ||
      !pipe.readerOpen ||
      !park
    ) {
      complete(null, pipe.fault);
    } else {
      const pending = { complete };
      pipe.reading.push(pending);
      return () => {
        const index = pipe.reading.indexOf(pending);
        if (index >= 0) pipe.reading.splice(index, 1);
      };
    }
    this.notify(pipe);
    return () => {};
  }

  private flush(pipe: Pipe): void {
    if (!pipe.readerOpen) {
      for (const sender of pipe.sending.splice(0)) sender.complete(1);
    } else {
      while (
        pipe.sending.length > 0 &&
        (pipe.reading.length > 0 || pipe.queue.length < pipe.capacity)
      ) {
        const sender = pipe.sending.shift()!;
        const envelope = this.admit(pipe, sender.value);
        if (!envelope) {
          sender.complete(1);
          break;
        }
        const reader = pipe.reading.shift();
        if (reader) reader.complete(sender.value, null, envelope.release);
        else pipe.queue.push(envelope);
        sender.complete(0);
      }
    }
    // Native delivers accepted queued items before exposing a producer fault.
    if (
      pipe.queue.length === 0 &&
      (pipe.sinks === 0 || pipe.fault !== null || !pipe.readerOpen)
    ) {
      for (const reader of pipe.reading.splice(0))
        reader.complete(null, pipe.fault);
    }
    this.notify(pipe);
  }

  private notify(pipe: Pipe): void {
    for (const observer of [...pipe.observers]) observer();
  }

  /** Only the unique receiving half owns admitted typed envelopes. */
  managedRetained(value: VmValue): {
    identity: object;
    queued: readonly { value: VmValue }[];
  } {
    const half = this.get(value);
    return {
      identity: half.kind === "stream" ? half.pipe : half,
      queued: half.open && half.kind === "stream" ? half.pipe.queue : [],
    };
  }
}
