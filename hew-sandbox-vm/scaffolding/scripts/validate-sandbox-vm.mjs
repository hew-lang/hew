import Ajv2020 from "ajv/dist/2020.js";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(scriptDir, "../..");

const failures = [];

function fail(message) {
  failures.push(message);
}

function readJson(relativePath) {
  const absolutePath = path.join(root, relativePath);
  try {
    return JSON.parse(fs.readFileSync(absolutePath, "utf8"));
  } catch (error) {
    fail(`${relativePath}: ${error.message}`);
    return undefined;
  }
}

function assertExists(relativePath) {
  const absolutePath = path.join(root, relativePath);
  if (!fs.existsSync(absolutePath)) {
    fail(`${relativePath}: missing file`);
    return false;
  }
  return true;
}

function listFiles(dir, predicate, results = []) {
  if (!fs.existsSync(dir)) {
    return results;
  }
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    if (entry.name === "node_modules") {
      continue;
    }
    const absolutePath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      listFiles(absolutePath, predicate, results);
    } else if (predicate(absolutePath)) {
      results.push(absolutePath);
    }
  }
  return results;
}

function validateMarkdownLinks() {
  const markdownFiles = listFiles(root, (file) => file.endsWith(".md"));
  const linkPattern = /\[[^\]]+\]\(([^)]+)\)/g;

  for (const file of markdownFiles) {
    const text = fs.readFileSync(file, "utf8");
    const relativeFile = path.relative(root, file);
    for (const match of text.matchAll(linkPattern)) {
      const rawTarget = match[1].trim();
      if (
        rawTarget.startsWith("#") ||
        rawTarget.startsWith("http://") ||
        rawTarget.startsWith("https://") ||
        rawTarget.startsWith("mailto:")
      ) {
        continue;
      }

      const targetPath = rawTarget.split("#", 1)[0];
      const absoluteTarget = path.resolve(path.dirname(file), targetPath);
      if (!absoluteTarget.startsWith(root) || !fs.existsSync(absoluteTarget)) {
        fail(`${relativeFile}: broken Markdown link to ${rawTarget}`);
      }
    }
  }
}

function validateSchemas() {
  const traceSchema = readJson("specs/trace-schema-v0.schema.json");
  const bytecodeSchema = readJson("bytecode/sandbox-bytecode-v0.schema.json");
  if (!traceSchema || !bytecodeSchema) {
    return undefined;
  }

  const ajv = new Ajv2020({
    allErrors: true,
    strict: true,
    validateFormats: false
  });

  for (const [name, schema] of [
    ["trace schema", traceSchema],
    ["bytecode schema", bytecodeSchema]
  ]) {
    if (!ajv.validateSchema(schema)) {
      fail(`${name}: ${ajv.errorsText(ajv.errors, { separator: "\n" })}`);
    }
  }

  let validateTrace;
  try {
    validateTrace = ajv.compile(traceSchema);
    ajv.compile(bytecodeSchema);
  } catch (error) {
    fail(`schema compilation: ${error.message}`);
    return undefined;
  }

  return validateTrace;
}

function validateFixtureCatalog(validateTrace) {
  const manifest = readJson("fixtures/manifest.json");
  if (!Array.isArray(manifest)) {
    fail("fixtures/manifest.json: expected an array");
    return;
  }

  if (manifest.length < 20) {
    fail(`fixtures/manifest.json: expected at least 20 fixtures, found ${manifest.length}`);
  }

  const seenIds = new Set();
  for (const [index, fixture] of manifest.entries()) {
    const label = fixture && fixture.id ? fixture.id : `entry ${index}`;
    for (const field of [
      "id",
      "category",
      "name",
      "description",
      "source_path",
      "expected_trace_path",
      "status",
      "features"
    ]) {
      if (!(field in fixture)) {
        fail(`fixtures/manifest.json ${label}: missing ${field}`);
      }
    }

    if (seenIds.has(fixture.id)) {
      fail(`fixtures/manifest.json ${label}: duplicate id`);
    }
    seenIds.add(fixture.id);

    if (!Array.isArray(fixture.features) || fixture.features.length === 0) {
      fail(`fixtures/manifest.json ${label}: features must be a non-empty array`);
    }

    if (fixture.source_path && assertExists(path.join("fixtures", fixture.source_path))) {
      const source = fs.readFileSync(path.join(root, "fixtures", fixture.source_path), "utf8");
      if (!source.trim()) {
        fail(`fixtures/${fixture.source_path}: source must not be empty`);
      }
    }

    if (fixture.expected_trace_path && assertExists(path.join("fixtures", fixture.expected_trace_path))) {
      const trace = readJson(path.join("fixtures", fixture.expected_trace_path));
      if (validateTrace && trace && !validateTrace(trace)) {
        fail(
          `fixtures/${fixture.expected_trace_path}: ${validateTrace.errors
            .map((error) => `${error.instancePath || "/"} ${error.message}`)
            .join("; ")}`
        );
      }
      if (trace && trace.fixture_id !== fixture.id) {
        fail(`fixtures/${fixture.expected_trace_path}: fixture_id does not match manifest id`);
      }
    }
  }
}

const validateTrace = validateSchemas();
validateFixtureCatalog(validateTrace);
validateMarkdownLinks();

if (failures.length > 0) {
  console.error("Sandbox VM validation failed:");
  for (const failure of failures) {
    console.error(`- ${failure}`);
  }
  process.exit(1);
}

console.log("Sandbox VM specs, fixtures, and links validated.");
