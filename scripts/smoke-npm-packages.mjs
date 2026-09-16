#!/usr/bin/env node
/** Execute the staged web packages together before npm publication. */

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const sourceRoot = resolve(
  process.env.HEW_SOURCE_ROOT ?? join(dirname(fileURLToPath(import.meta.url)), ".."),
);
const stagingRoot = resolve(process.argv[2] ?? join(sourceRoot, "target/npm/@hew-lang"));

async function loadPackage(name, wasm = false) {
  const root = join(stagingRoot, name);
  const metadata = JSON.parse(await readFile(join(root, "package.json"), "utf8"));
  const entry = wasm ? (metadata.module ?? metadata.main) : metadata.exports["."].import;
  const module = await import(pathToFileURL(join(root, entry)).href);
  if (wasm) {
    const bytes = await readFile(join(root, entry.replace(/\.js$/, "_bg.wasm")));
    await module.default({ module_or_path: bytes });
  }
  return { module, version: metadata.version };
}

const hello = 'fn main() { println("Hello, npm!"); }';
const counter = `
actor Counter {
    var count: i64,

    receive fn increment(n: i64) -> i64 {
        count = count + n;
        count
    }
}

fn main() {
    let counter = spawn Counter(count: 0);
    println(match counter.increment(5) { .Ok(value) => value, .Err(_) => 0 - 1 });
    println(match counter.increment(3) { .Ok(value) => value, .Err(_) => 0 - 1 });
}
`;

const analysis = await loadPackage("wasm", true);
const compiler = await loadPackage("sandbox-wasm", true);
const vm = await loadPackage("sandbox-vm");
assert.equal(analysis.version, compiler.version, "analysis and compiler package versions differ");
assert.equal(compiler.version, vm.version, "compiler and VM package versions differ");

const analyzed = JSON.parse(analysis.module.analyze(hello));
assert.deepEqual(analyzed.diagnostics, [], "staged browser analysis rejected hello");
const invalid = JSON.parse(analysis.module.analyze("fn main( {"));
assert.ok(invalid.diagnostics.length > 0, "staged browser analysis accepted invalid syntax");

for (const [name, source, stdout] of [
  ["hello", hello, "Hello, npm!\n"],
  ["stateful actor", counter, "5\n8\n"],
]) {
  const compiled = JSON.parse(
    compiler.module.compileToSandboxBytecode(source, "sandbox-vm-export"),
  );
  assert.deepEqual(compiled.diagnostics, [], `${name}: compiler diagnostics`);
  assert.ok(compiled.bytecode, `${name}: compiler produced no bytecode`);
  const trace = vm.module.runBytecode(compiled.bytecode, {
    replay: { step_budget: 1000 },
  });
  assert.equal(trace.result, "ok", `${name}: ${JSON.stringify(trace.final_state)}`);
  assert.equal(trace.final_state.exit_code, 0, `${name}: unsuccessful exit`);
  assert.equal(trace.final_state.stdout.join(""), stdout, `${name}: stdout differs`);
  assert.deepEqual(trace.final_state.stderr, [], `${name}: unexpected stderr`);
  console.log(`PASS staged npm ${name}: expected stdout and exit 0`);
}

console.log(`PASS @hew-lang packages ${vm.version}: browser analysis and sandbox execution`);
