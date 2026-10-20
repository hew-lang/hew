import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import Ajv2020 from "ajv/dist/2020.js";
import standaloneCode from "ajv/dist/standalone/index.js";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../..");
const schema = JSON.parse(fs.readFileSync(path.join(root, "bytecode/sandbox-bytecode-v1.schema.json"), "utf8"));
const ajv = new Ajv2020({
  strict: false,
  validateFormats: false,
  discriminator: true,
  code: { source: true, esm: true },
});
// Discriminator routing is a code-generation optimization; the public schema
// remains standard JSON Schema and independent validators need no extension.
for (const name of ["OpV1", "TermV1"])
  schema.$defs[name].discriminator = { propertyName: "op" };
ajv.addSchema(schema);
const generated = "// @ts-nocheck\n// Generated from the Rust instruction schema. Run make sandbox-bytecode.\n" +
  standaloneCode(ajv, {
    validOperation: schema.$id + "#/$defs/OpV1",
    validTerminator: schema.$id + "#/$defs/TermV1",
  }) + "\n";
const destination = path.join(root, "src/interpreter/v1/instruction-validator.generated.ts");
if (process.argv[2] === "--write") fs.writeFileSync(destination, generated);
else if (process.argv[2] === "--check") {
  if (fs.readFileSync(destination, "utf8") !== generated) {
    throw new Error("instruction validator is stale; run make sandbox-bytecode");
  }
} else throw new Error("expected --write or --check");
