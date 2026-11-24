import type { UnicodeBridge } from "./types.js";

/** Native Unicode scalar operations routed through the hew-wasm bridge. */
export function charProperty(
  bridge: UnicodeBridge | undefined,
  value: string,
  bit: number,
): boolean {
  if (!bridge) {
    throw new Error("Unicode operations require the hew-wasm unicode bridge");
  }
  const cp = value.codePointAt(0);
  if (cp === undefined) {
    return false;
  }
  return (bridge.charProperties(cp) & (1 << bit)) !== 0;
}

export function charCase(
  bridge: UnicodeBridge | undefined,
  value: string,
  kind: "upper" | "lower" | "title",
): string {
  if (!bridge) {
    throw new Error("Unicode operations require the hew-wasm unicode bridge");
  }
  const cp = value.codePointAt(0);
  if (cp === undefined) {
    return value;
  }
  const mapped =
    kind === "upper"
      ? bridge.charToUpper(cp)
      : kind === "lower"
        ? bridge.charToLower(cp)
        : bridge.charToTitle(cp);
  return mapped === cp ? value : String.fromCodePoint(mapped);
}
