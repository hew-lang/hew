import { stringValue, type VmValue } from "../values.js";
import type { AggregateShape } from "./package.js";

const TIME_FIELDS = ["status", "seconds", "nanoseconds", "message"];
const CALENDAR_FIELDS: Record<string, readonly string[] | undefined> = {
  hew_calendar_format: ["status", "text", "message"],
  hew_calendar_parse: TIME_FIELDS,
  hew_calendar_from_parts: TIME_FIELDS,
  hew_calendar_components: [
    "status",
    "year",
    "month",
    "day",
    "hour",
    "minute",
    "second",
    "nanosecond",
    "weekday",
    "days_in_month",
    "message",
  ],
};

export function calendarResultFields(
  symbol: string,
): readonly string[] | undefined {
  return CALENDAR_FIELDS[symbol];
}

export function executeCalendar(
  bridge: ((request: string) => string) | undefined,
  symbol: string,
  args: VmValue[],
  shape: AggregateShape | null,
): VmValue {
  if (!bridge)
    throw new Error(
      "calendar operations require the hew-wasm executeCalendar bridge",
    );
  const fields = calendarResultFields(symbol);
  if (
    !shape ||
    !fields ||
    shape.fields.length !== fields.length ||
    shape.fields.some((field, index) => field !== fields[index])
  )
    throw new Error("calendar operation lacks its checked result shape");
  const request = {
    operation: symbol.slice("hew_calendar_".length),
    args: args.map((value) => {
      if (value.kind === "i64") return value.value.toString();
      if (value.kind === "string") return value.value;
      throw new Error("invalid calendar argument");
    }),
  };
  const result = JSON.parse(bridge(JSON.stringify(request))) as Record<
    string,
    unknown
  >;
  if (
    typeof result.status !== "number" ||
    ![0, 1, 2, 3].includes(result.status) ||
    typeof result.message !== "string"
  )
    throw new Error("calendar bridge returned an invalid result");
  return {
    kind: "record",
    typeId: shape.name,
    shape: shape.id,
    fields: shape.fields.map((field) => {
      const value = result[field];
      if (result.status === 0 && value === undefined)
        throw new Error("calendar bridge omitted a successful result field");
      if (field === "message" || field === "text")
        return stringValue(value === undefined ? "" : String(value));
      return {
        kind: "i64",
        value: BigInt(value === undefined ? 0 : String(value)),
      };
    }),
  };
}
