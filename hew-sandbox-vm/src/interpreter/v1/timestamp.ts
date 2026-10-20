/// Exact realization of SIR's checked wall-clock operations.
/// Fields are normalized at creation and decode; arithmetic checks only the
/// mathematical result's range, then follows the package's own failure edge.
import type { VmValue } from "../values.js";
import type { PackageV1, TermV1 } from "./package.js";

type TimestampBinary = Extract<TermV1, { op: "timestamp.binary" }>;
const BILLION = 1_000_000_000n;
const MIN_I64 = -(1n << 63n);
const MAX_I64 = (1n << 63n) - 1n;

function exactNanos(value: VmValue, timestamp: boolean): bigint {
  if (!timestamp) {
    if (value.kind !== "i64")
      throw new TypeError("timestamp offset is not a duration");
    return value.value;
  }
  if (value.kind !== "record" || value.fields.length !== 2)
    throw new TypeError("timestamp lacks its normalized pair");
  const [seconds, nanos] = value.fields;
  if (
    seconds?.kind !== "i64" ||
    nanos?.kind !== "i64" ||
    seconds.value < MIN_I64 ||
    seconds.value > MAX_I64 ||
    nanos.value < 0n ||
    nanos.value >= BILLION
  )
    throw new TypeError("invalid normalized timestamp");
  return seconds.value * BILLION + nanos.value;
}

export function timestampBinary(
  pkg: PackageV1,
  term: TimestampBinary,
  lhs: VmValue,
  rhs: VmValue,
): VmValue | null {
  const left = exactNanos(lhs, term.lhs_timestamp);
  const right = exactNanos(rhs, term.rhs_timestamp);
  const exact = term.binary_op === "Add" ? left + right : left - right;
  if (term.shape === null) {
    return exact < MIN_I64 || exact > MAX_I64
      ? null
      : { kind: "i64", value: exact };
  }
  if (exact < MIN_I64 * BILLION || exact > MAX_I64 * BILLION + BILLION - 1n)
    return null;
  let seconds = exact / BILLION;
  let nanos = exact % BILLION;
  if (nanos < 0n) {
    seconds -= 1n;
    nanos += BILLION;
  }
  const shape = pkg.aggregates[term.shape];
  if (!shape) throw new TypeError("timestamp result shape is absent");
  return {
    kind: "record",
    typeId: shape.name,
    shape: term.shape,
    fields: [
      { kind: "i64", value: seconds },
      { kind: "i64", value: nanos },
    ],
  };
}
