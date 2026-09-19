import { type VmValue } from "../values.js";

interface PendingSend {
  value: VmValue;
  complete(status: number): void;
}

interface PendingRead {
  complete(value: VmValue | null, fault: string | null): void;
}

interface Pipe {
  capacity: number;
  queue: VmValue[];
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
}

/// The VM's one bounded pipe substrate. Selection observes readiness; only
/// the winning receive removes an item. Values transfer without a snapshot.
export class Pipes {
  private readonly pairs = new Map<string, Pipe>();
  private readonly halves = new Map<string, Half>();
  private next = 0;

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

  private half(pipe: Pipe, kind: "sink" | "stream"): VmValue {
    const id = `pipe-half:${++this.next}`;
    this.halves.set(id, { pipe, kind, open: true });
    if (kind === "sink") pipe.sinks += 1;
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
    if (!half.open || half.kind !== "sink")
      throw new Error("cannot clone a closed sink");
    return this.half(half.pipe, "sink");
  }

  close(value: VmValue, fault: string | null = null): void {
    const half = this.get(value);
    if (!half.open) return;
    half.open = false;
    if (half.kind === "sink") {
      half.pipe.sinks -= 1;
      half.pipe.fault ??= fault;
    } else {
      half.pipe.readerOpen = false;
      half.pipe.queue.length = 0;
    }
    this.flush(half.pipe);
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
      pipe.reading.shift()!.complete(item, null);
      complete(0);
    } else if (pipe.queue.length < pipe.capacity) {
      pipe.queue.push(item);
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
      complete(pipe.queue.shift()!, null);
      this.flush(pipe);
    } else if (pipe.sending.length > 0) {
      const sender = pipe.sending.shift()!;
      complete(sender.value, null);
      sender.complete(0);
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
        const reader = pipe.reading.shift();
        if (reader) reader.complete(sender.value, null);
        else pipe.queue.push(sender.value);
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
}
