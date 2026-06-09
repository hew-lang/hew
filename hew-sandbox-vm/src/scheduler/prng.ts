export class SeededPrng {
  private state: bigint;

  constructor(seed: number) {
    this.state = BigInt.asUintN(64, BigInt(seed) + 0x9e3779b97f4a7c15n);
  }

  nextUint32(): number {
    this.state = BigInt.asUintN(64, this.state + 0x9e3779b97f4a7c15n);
    let z = this.state;
    z = BigInt.asUintN(64, (z ^ (z >> 30n)) * 0xbf58476d1ce4e5b9n);
    z = BigInt.asUintN(64, (z ^ (z >> 27n)) * 0x94d049bb133111ebn);
    z = z ^ (z >> 31n);
    return Number((z >> 32n) & 0xffffffffn);
  }

  nextIndex(length: number): number {
    if (length <= 0) {
      throw new RangeError("length must be positive");
    }
    return this.nextUint32() % length;
  }
}
