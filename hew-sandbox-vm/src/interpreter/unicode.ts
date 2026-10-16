import {
  CATEGORY_RANGES,
  UPPER_MAPPING,
  LOWER_MAPPING,
  TITLE_MAPPING,
} from "./unicode-data.js";

/** The native Unicode operations, independent of the JavaScript host's version. */
export function charProperty(value: string, bit: number): boolean {
  const scalar = value.codePointAt(0)!;
  let low = 0,
    high = CATEGORY_RANGES.length / 3;
  while (low < high) {
    const middle = Math.floor((low + high) / 2);
    if (CATEGORY_RANGES[middle * 3 + 1]! < scalar) low = middle + 1;
    else high = middle;
  }
  return (
    low * 3 < CATEGORY_RANGES.length &&
    CATEGORY_RANGES[low * 3]! <= scalar &&
    (CATEGORY_RANGES[low * 3 + 2]! & (1 << bit)) !== 0
  );
}

export function charCase(
  value: string,
  kind: "upper" | "lower" | "title",
): string {
  const table =
    kind === "upper"
      ? UPPER_MAPPING
      : kind === "lower"
        ? LOWER_MAPPING
        : TITLE_MAPPING;
  const scalar = value.codePointAt(0)!;
  let low = 0,
    high = table.length / 2;
  while (low < high) {
    const middle = Math.floor((low + high) / 2);
    if (table[middle * 2]! < scalar) low = middle + 1;
    else high = middle;
  }
  return table[low * 2] === scalar
    ? String.fromCodePoint(table[low * 2 + 1]!)
    : value;
}
