import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { WASIShim } from "@bytecodealliance/preview2-shim/instantiation";
import { instantiate, type ImportObject } from "./component/router.js";
import type * as Handler from "./wit/interfaces/hew-live-router-handler.js";
import type * as Services from "./wit/interfaces/hew-live-router-services.js";
import type {
  Delivery,
  Snapshot,
} from "./wit/interfaces/hew-live-router-events.js";

let complete: ((value: string) => void) | undefined;
const delivered: Delivery[] = [];
const services: typeof Services = {
  async destination(source) {
    if (source === "missing") return undefined;
    if (source === "denied") throw "destination denied";
    return new Promise<string>((resolve) => {
      complete = resolve;
    });
  },
  async emit(message) {
    delivered.push(message);
  },
};
async function activate(state: Snapshot) {
  // Jco's component-specific WASI resource classes omit unused methods from
  // the shim's full resource declarations. Keep that assertion at this boundary;
  // the Hew services and handler remain checked against generated declarations.
  type WasiImports = Pick<
    ImportObject,
    Extract<keyof ImportObject, `wasi:${string}`>
  >;
  const shim = new WASIShim({
    sandbox: { preopens: {}, env: {}, args: [], enableNetwork: false },
  });
  const wasi = shim.getImportObject({
    asVersion: "0.2.12",
  }) as unknown as WasiImports;
  // Jco's instantiation runtime uses unversioned keys, while its declarations
  // retain WIT versions. Supply both spellings to the same service objects.
  const imports = {
    ...wasi,
    ...shim.getImportObject(),
    "hew:live-router/events@0.1.0": {},
    "hew:live-router/services@0.1.0": services,
    "hew:live-router/services": services,
  };
  const instance = await instantiate(
    async (path) =>
      WebAssembly.compile(
        await readFile(new URL(`./component/${path}`, import.meta.url)),
      ),
    imports,
  );
  // Check the executable component against declarations generated from the WIT.
  const handler: typeof Handler = instance.handler;
  handler.initialize(state);
  return handler;
}
const initial = 9_007_199_254_740_993n;
const handler = await activate({ accepted: initial });
assert.equal(await handler.drive(), undefined);
assert.equal(await handler.route({ source: "missing", body: "" }), false);
// The service error is a rejected WIT result, not a Wasm trap.
await assert.rejects(
  handler.route({ source: "denied", body: "" }),
  (error) => typeof error === "string" && error.includes("destination denied"),
);
let finished = false;
const pending = handler
  .route({ source: "sensor", body: "hé\0雪" })
  .then((value) => {
    finished = true;
    return value;
  });
while (!complete) await new Promise((resolve) => setImmediate(resolve));
assert.equal(finished, false);
assert.deepEqual(delivered, []);
complete("destination");
assert.equal(await pending, true);
assert.deepEqual(delivered, [{ destination: "destination", body: "hé\0雪" }]);
const snapshot = await handler.quiesce();
assert.equal(snapshot.accepted, initial + 1n);
await assert.rejects(
  handler.route({ source: "sensor", body: "" }),
  (error) => typeof error === "string" && error.includes("not accepting"),
);
handler.resume();
assert.equal(await handler.route({ source: "missing", body: "" }), false);
assert.deepEqual(await handler.quiesce(), snapshot);
await handler.dispose();
const replacement = await activate(snapshot);
assert.deepEqual(await replacement.quiesce(), snapshot);
await replacement.dispose();
console.log(
  "TypeScript component consumer: suspended service, errors, bigint snapshot, resume and replacement passed",
);
