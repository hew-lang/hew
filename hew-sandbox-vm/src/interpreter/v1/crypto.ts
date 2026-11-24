import { byteVectorValue, UNIT, vectorItems, type VmValue } from "../values.js";

export type CryptoBridge = (request: string) => string;

type CryptoResult =
  | { ok: true; bytes?: unknown; equal?: unknown }
  | { ok: false; error?: unknown };

const UTF8_DECODER = new TextDecoder("utf-8");

function byteArrayArg(args: VmValue[], index: number): number[] {
  const value = args[index];
  if (!value || value.kind !== "vector")
    throw new TypeError("crypto operation requires a bytes argument");
  return vectorItems(value).map((item) => {
    if (item.kind !== "i64")
      throw new TypeError("crypto byte element must be an integer");
    const n = Number(item.value);
    if (!Number.isInteger(n) || n < 0 || n > 255)
      throw new RangeError(`crypto byte ${item.value} is outside u8`);
    return n;
  });
}

/**
 * Encode bytes as fixed-width two-character lowercase hex without
 * value-dependent output length, branches, or lookup tables.
 */
export function encodeFixedWidthHex(bytes: readonly number[]): string {
  const ascii = new Uint8Array(bytes.length * 2);
  for (let i = 0; i < bytes.length; i += 1) {
    const byte = bytes[i];
    const hi = (byte >>> 4) & 0x0f;
    const lo = byte & 0x0f;
    ascii[i * 2] = hi + 48 + (((9 - hi) >> 31) & 39);
    ascii[i * 2 + 1] = lo + 48 + (((9 - lo) >> 31) & 39);
  }
  return UTF8_DECODER.decode(ascii);
}

function invokeCryptoRaw(
  bridge: CryptoBridge | undefined,
  request: string,
): Extract<CryptoResult, { ok: true }> {
  if (!bridge)
    throw new Error(
      "crypto operations require the hew-wasm executeCrypto bridge",
    );
  const parsed = JSON.parse(bridge(request)) as CryptoResult;
  if (!parsed || typeof parsed !== "object" || parsed.ok !== true) {
    const message =
      parsed && typeof parsed === "object" && typeof parsed.error === "string"
        ? parsed.error
        : "crypto bridge returned an error";
    throw new Error(message);
  }
  return parsed;
}

function invokeCrypto(
  bridge: CryptoBridge | undefined,
  operation: string,
  encodedArgs: number[][],
): Extract<CryptoResult, { ok: true }> {
  return invokeCryptoRaw(
    bridge,
    JSON.stringify({ operation, args: encodedArgs }),
  );
}

function digestResult(
  bridge: CryptoBridge | undefined,
  operation: "sha256" | "sha384" | "sha512" | "hmac_sha256",
  encodedArgs: number[][],
): VmValue {
  const result = invokeCrypto(bridge, operation, encodedArgs);
  if (!Array.isArray(result.bytes))
    throw new Error("crypto bridge omitted digest bytes");
  return byteVectorValue(
    result.bytes.map((byte) => {
      if (
        typeof byte !== "number" ||
        !Number.isInteger(byte) ||
        byte < 0 ||
        byte > 255
      )
        throw new Error("crypto bridge returned an invalid byte value");
      return { kind: "i64" as const, value: BigInt(byte) };
    }),
  );
}

export function executeShaDigest(
  bridge: CryptoBridge | undefined,
  operation: "sha256" | "sha384" | "sha512",
  args: VmValue[],
): VmValue {
  return digestResult(bridge, operation, [byteArrayArg(args, 0)]);
}

export function executeHmacSha256(
  bridge: CryptoBridge | undefined,
  args: VmValue[],
): VmValue {
  return digestResult(bridge, "hmac_sha256", [
    byteArrayArg(args, 0),
    byteArrayArg(args, 1),
  ]);
}

export function executeConstantTimeEq(
  bridge: CryptoBridge | undefined,
  args: VmValue[],
): VmValue {
  const hexA = encodeFixedWidthHex(byteArrayArg(args, 0));
  const hexB = encodeFixedWidthHex(byteArrayArg(args, 1));
  const result = invokeCryptoRaw(
    bridge,
    `{"operation":"constant_time_eq","args_hex":["${hexA}","${hexB}"]}`,
  );
  if (typeof result.equal !== "boolean")
    throw new Error("crypto bridge returned a non-boolean comparison result");
  return { kind: "i64", value: result.equal ? 1n : 0n };
}

export { UNIT };
