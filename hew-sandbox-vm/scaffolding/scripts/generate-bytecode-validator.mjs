import { spawnSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import Ajv2020 from "ajv/dist/2020.js";
import standaloneCode from "ajv/dist/standalone/index.js";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const schema = JSON.parse(
  fs.readFileSync(
    path.join(root, "bytecode/sandbox-bytecode-v1.schema.json"),
    "utf8",
  ),
);
const ajv = new Ajv2020({
  strict: false,
  strictNumbers: true,
  validateFormats: false,
  discriminator: true,
  inlineRefs: false,
  code: { source: true, esm: true },
});
// Discriminator routing is a code-generation optimization; the public schema
// remains standard JSON Schema and independent validators need no extension.
for (const name of ["OpV1", "TermV1"])
  schema.$defs[name].discriminator = { propertyName: "op" };
schema.$defs.RuntimeFamilyEntry.discriminator = { propertyName: "family" };
const suspension = schema.$defs.TermV1.oneOf.find(
  (variant) => variant.properties.op.const === "suspend",
);
suspension.discriminator = { propertyName: "kind" };
ajv.addSchema(schema);
const unformatted =
  "// @ts-nocheck\n// Generated from the complete Rust package schema. Run make sandbox-bytecode.\n" +
  'import type { ValidateFunction } from "ajv";\n' +
  standaloneCode(ajv, {
    validPackage: schema.$id,
    validCodecShape: schema.$id + "#/$defs/CodecShape",
  }).replace(
    /export const (validPackage|validCodecShape)\s*=/g,
    "export const $1: ValidateFunction =",
  ) +
  "\n";
const destination = path.join(
  root,
  "src/interpreter/v1/package-validator.generated.ts",
);
const formatted = spawnSync(
  "bash",
  ["-c", 'exec prettier --stdin-filepath "$1"', "prettier", destination],
  {
    input: unformatted,
    encoding: "utf8",
    maxBuffer: 16 * 1024 * 1024,
  },
);
if (formatted.error) throw formatted.error;
if (formatted.status !== 0)
  throw new Error("prettier failed for package validator");
const generated = formatted.stdout;
if (process.argv[2] === "--write") fs.writeFileSync(destination, generated);
else if (process.argv[2] === "--check") {
  if (fs.readFileSync(destination, "utf8") !== generated) {
    throw new Error("package validator is stale; run make sandbox-bytecode");
  }
} else throw new Error("expected --write or --check");
