/** Pending strict actor turns. Keys identify incarnations, never role handles. */
export class ActorWaits<T> {
  private readonly edges = new Map<T, readonly T[]>();

  ready(owner: T): void {
    this.edges.delete(owner);
  }

  pending(owner: T, targets: readonly T[]): T[] | null {
    this.edges.set(owner, targets);
    const blocked = new Set(
      [...this.edges]
        .filter(([, targets]) => targets.length > 0)
        .map(([owner]) => owner),
    );
    for (;;) {
      const escaped = [...blocked].filter((actor) =>
        this.edges.get(actor)!.some((target) => !blocked.has(target)),
      );
      if (escaped.length === 0) break;
      for (const actor of escaped) blocked.delete(actor);
    }
    if (!blocked.has(owner)) return null;
    const path: T[] = [];
    const seen = new Set<T>();
    let next = owner;
    while (!seen.has(next)) {
      seen.add(next);
      path.push(next);
      next = this.edges.get(next)![0]!;
    }
    path.push(next);
    this.ready(owner);
    return path;
  }
}
