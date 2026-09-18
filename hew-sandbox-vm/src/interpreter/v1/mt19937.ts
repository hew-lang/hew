/// CPython-compatible MT19937, ported from `hew-runtime/src/random.rs`.
///
/// `std.random` is not a generic seeded generator: a seeded Hew program prints
/// a specific sequence natively, and reproducible randomness that is not
/// reproducible across engines would not be reproducible at all. This is the
/// same algorithm and the same seeding, so `random.seed(1)` followed by
/// `random.randint(0, 99)` reads 17 in the VM as it does natively.
///
/// The scheduler's own generator stays separate: a program's `seed` call must
/// not disturb scheduling chaos.

const N = 624;
const M = 397;
const MATRIX_A = 0x9908b0df;
const UPPER_MASK = 0x80000000;
const LOWER_MASK = 0x7fffffff;
const MASK32 = 0xffffffff;

function mul32(left: number, right: number): number {
  return Math.imul(left, right) >>> 0;
}

export class Mt19937 {
  private readonly mt = new Uint32Array(N);
  private mti = N + 1;

  /// CPython `init_genrand`.
  private initGenrand(seed: number): void {
    this.mt[0] = seed >>> 0;
    for (let i = 1; i < N; i += 1) {
      const previous = this.mt[i - 1]!;
      this.mt[i] = (mul32(1812433253, previous ^ (previous >>> 30)) + i) >>> 0;
    }
    this.mti = N;
  }

  /// CPython `init_by_array`.
  initByArray(key: readonly number[]): void {
    this.initGenrand(19650218);
    let i = 1;
    let j = 0;
    for (let count = Math.max(N, key.length); count > 0; count -= 1) {
      const previous = this.mt[i - 1]!;
      this.mt[i] =
        (((this.mt[i]! ^ mul32(previous ^ (previous >>> 30), 1664525)) >>> 0) +
          key[j]! +
          j) >>>
        0;
      i += 1;
      j += 1;
      if (i >= N) {
        this.mt[0] = this.mt[N - 1]!;
        i = 1;
      }
      if (j >= key.length) {
        j = 0;
      }
    }
    for (let count = N - 1; count > 0; count -= 1) {
      const previous = this.mt[i - 1]!;
      this.mt[i] =
        (((this.mt[i]! ^ mul32(previous ^ (previous >>> 30), 1566083941)) >>>
          0) -
          i) >>>
        0;
      i += 1;
      if (i >= N) {
        this.mt[0] = this.mt[N - 1]!;
        i = 1;
      }
    }
    this.mt[0] = UPPER_MASK;
  }

  genrandUint32(): number {
    if (this.mti >= N) {
      for (let kk = 0; kk < N - M; kk += 1) {
        const y = (this.mt[kk]! & UPPER_MASK) | (this.mt[kk + 1]! & LOWER_MASK);
        this.mt[kk] =
          (this.mt[kk + M]! ^ (y >>> 1) ^ (y & 1 ? MATRIX_A : 0)) >>> 0;
      }
      for (let kk = N - M; kk < N - 1; kk += 1) {
        const y = (this.mt[kk]! & UPPER_MASK) | (this.mt[kk + 1]! & LOWER_MASK);
        this.mt[kk] =
          (this.mt[kk + M - N]! ^ (y >>> 1) ^ (y & 1 ? MATRIX_A : 0)) >>> 0;
      }
      const y = (this.mt[N - 1]! & UPPER_MASK) | (this.mt[0]! & LOWER_MASK);
      this.mt[N - 1] =
        (this.mt[M - 1]! ^ (y >>> 1) ^ (y & 1 ? MATRIX_A : 0)) >>> 0;
      this.mti = 0;
    }

    let y = this.mt[this.mti]!;
    this.mti += 1;
    y ^= y >>> 11;
    y = (y ^ ((y << 7) & 0x9d2c5680)) >>> 0;
    y = (y ^ ((y << 15) & 0xefc60000)) >>> 0;
    return (y ^ (y >>> 18)) >>> 0;
  }

  /// CPython `random()` — a 53-bit float in [0, 1).
  random(): number {
    const high = this.genrandUint32() >>> 5;
    const low = this.genrandUint32() >>> 6;
    return (high * 67108864 + low) / 9007199254740992;
  }

  /// CPython `getrandbits(k)`.
  private getrandbits(bits: number): bigint {
    if (bits === 0) {
      return 0n;
    }
    const fullWords = Math.floor(bits / 32);
    const extraBits = bits % 32;
    let result = 0n;
    for (let word = 0; word < fullWords; word += 1) {
      result |= BigInt(this.genrandUint32()) << BigInt(word * 32);
    }
    if (extraBits > 0) {
      result |=
        BigInt(this.genrandUint32() >>> (32 - extraBits)) <<
        BigInt(fullWords * 32);
    }
    return result;
  }

  /// CPython `_randbelow(n)` — rejection sampling, so the draw is uniform and
  /// consumes the same words CPython does.
  randbelow(bound: bigint): bigint {
    if (bound <= 1n) {
      return 0n;
    }
    const bits = BigInt.asUintN(64, bound).toString(2).length;
    while (true) {
      const draw = this.getrandbits(bits);
      if (draw < bound) {
        return draw;
      }
    }
  }
}

/// `hew_random_seed`: the seed is reduced to its low 32 bits and passed to
/// `init_by_array` as a one-word key.
export function seededMt(seed: bigint): Mt19937 {
  const generator = new Mt19937();
  generator.initByArray([Number(BigInt.asUintN(64, seed) & BigInt(MASK32))]);
  return generator;
}
