/** The VM's owned UTF-8 string storage. Scalar positions use the same
 * bounded-stride index as native strings; no prefix is rescanned on access. */
const INDEX_STRIDE = 32;
const ENCODER = new TextEncoder();
const DECODER = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

interface StringStorage {
  bytes: Uint8Array;
  byteLength: number;
  scalarLength: number;
  offsets: number[] | null;
  owners: number;
}

function scalarWidth(first: number): number {
  return first < 0x80 ? 1 : first < 0xe0 ? 2 : first < 0xf0 ? 3 : 4;
}

function indexStorage(
  storage: StringStorage,
  byte: number,
  scalar: number,
): void {
  const offsets = storage.offsets!;
  while (byte < storage.byteLength) {
    if (scalar % INDEX_STRIDE === 0) offsets[scalar / INDEX_STRIDE] = byte;
    byte += scalarWidth(storage.bytes[byte]!);
    scalar++;
  }
}

function newStorage(bytes: Uint8Array, capacity = bytes.length): StringStorage {
  const buffer = new Uint8Array(capacity);
  buffer.set(bytes);
  let scalars = 0;
  for (let at = 0; at < bytes.length; at += scalarWidth(bytes[at]!)) scalars++;
  const storage: StringStorage = {
    bytes: buffer,
    byteLength: bytes.length,
    scalarLength: scalars,
    offsets: scalars === bytes.length ? null : [],
    owners: 1,
  };
  if (storage.offsets) indexStorage(storage, 0, 0);
  return storage;
}

export class IndexedString {
  readonly kind = "string";
  prepareGrowth?: (capacity: number) => void;
  private storage: StringStorage | null;

  private constructor(storage: StringStorage) {
    this.storage = storage;
  }

  static fromText(text: string): IndexedString {
    return new IndexedString(newStorage(ENCODER.encode(text)));
  }

  private live(): StringStorage {
    if (!this.storage) throw new Error("cannot read a released string");
    return this.storage;
  }

  get value(): string {
    return DECODER.decode(this.encoded());
  }

  get scalarLength(): number {
    return this.live().scalarLength;
  }

  get byteLength(): number {
    return this.live().byteLength;
  }

  /** The same UTF-8 backing retained by clones; indexing metadata is excluded. */
  managedStorage(): { identity: object; capacity: number } {
    const storage = this.live();
    return { identity: storage, capacity: storage.bytes.length };
  }

  encoded(): Uint8Array {
    const storage = this.live();
    return storage.bytes.subarray(0, storage.byteLength);
  }

  clone(): IndexedString {
    const storage = this.live();
    storage.owners++;
    return new IndexedString(storage);
  }

  release(): void {
    if (this.storage) {
      this.storage.owners--;
      this.storage = null;
    }
  }

  scalarOffset(index: number): number | undefined {
    const storage = this.live();
    if (
      !Number.isSafeInteger(index) ||
      index < 0 ||
      index > storage.scalarLength
    )
      return undefined;
    if (index === storage.scalarLength) return storage.byteLength;
    if (!storage.offsets) return index;
    let byte = storage.offsets[Math.floor(index / INDEX_STRIDE)]!;
    for (let tail = index % INDEX_STRIDE; tail > 0; tail--)
      byte += scalarWidth(storage.bytes[byte]!);
    return byte;
  }

  at(index: number): string | undefined {
    if (index === this.scalarLength) return undefined;
    const byte = this.scalarOffset(index);
    if (byte === undefined) return undefined;
    const storage = this.live();
    return DECODER.decode(
      storage.bytes.subarray(byte, byte + scalarWidth(storage.bytes[byte]!)),
    );
  }

  slice(start: number, end: number): IndexedString | undefined {
    const from = this.scalarOffset(start);
    const to = this.scalarOffset(end);
    if (from === undefined || to === undefined || from > to) return undefined;
    return new IndexedString(newStorage(this.live().bytes.subarray(from, to)));
  }

  private scalarIndex(byte: number): number {
    const storage = this.live();
    if (!storage.offsets) return byte;
    if (byte === storage.byteLength) return storage.scalarLength;
    let low = 0;
    let high = storage.offsets.length;
    while (low + 1 < high) {
      const mid = Math.floor((low + high) / 2);
      if (storage.offsets[mid]! <= byte) low = mid;
      else high = mid;
    }
    let scalar = low * INDEX_STRIDE;
    for (
      let at = storage.offsets[low]!;
      at < byte;
      at += scalarWidth(storage.bytes[at]!)
    )
      scalar++;
    return scalar;
  }

  find(needle: IndexedString, start = 0): number | undefined {
    const from = this.scalarOffset(start);
    if (from === undefined) return undefined;
    const bytes = this.encoded();
    const pattern = needle.encoded();
    if (pattern.length === 0) return start;
    // KMP searches only the suffix requested by the caller. Decoding the
    // complete value for String.indexOf would rescan its prefix on every call.
    const prefixes = new Array<number>(pattern.length).fill(0);
    for (let i = 1, matched = 0; i < pattern.length; i++) {
      while (matched && pattern[i] !== pattern[matched])
        matched = prefixes[matched - 1]!;
      if (pattern[i] === pattern[matched]) matched++;
      prefixes[i] = matched;
    }
    for (let i = from, matched = 0; i < bytes.length; i++) {
      while (matched && bytes[i] !== pattern[matched])
        matched = prefixes[matched - 1]!;
      if (bytes[i] === pattern[matched]) matched++;
      if (matched === pattern.length) return this.scalarIndex(i + 1 - matched);
    }
    return undefined;
  }

  /** Consume this receiver, retaining the suffix's borrowed snapshot. */
  append(suffix: IndexedString): IndexedString {
    const bytes = suffix.encoded();
    const added = suffix.scalarLength;
    if (!bytes.length) return this;
    let storage = this.live();
    const oldLength = storage.byteLength;
    const oldScalars = storage.scalarLength;
    const newLength = oldLength + bytes.length;
    const capacity =
      newLength > storage.bytes.length
        ? Math.max(newLength, storage.bytes.length * 2, 16)
        : storage.bytes.length;
    if (storage.owners !== 1 || capacity !== storage.bytes.length)
      this.prepareGrowth?.(capacity);
    if (storage.owners !== 1) {
      storage.owners--;
      storage = newStorage(this.encoded(), capacity);
      this.storage = storage;
    } else if (capacity !== storage.bytes.length) {
      const buffer = new Uint8Array(capacity);
      buffer.set(storage.bytes.subarray(0, oldLength));
      storage.bytes = buffer;
    }
    storage.bytes.set(bytes, oldLength);
    storage.byteLength = newLength;
    storage.scalarLength += added;
    if (storage.scalarLength !== newLength) {
      if (storage.offsets) indexStorage(storage, oldLength, oldScalars);
      else {
        storage.offsets = [];
        indexStorage(storage, 0, 0);
      }
    }
    return this;
  }
}

export function stringValue(value: string): IndexedString {
  return IndexedString.fromText(value);
}
