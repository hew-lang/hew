// Exercise the actual browser export with the matching VM after `make wasm`.
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {dirname, join, resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import test from 'node:test';

const compilerPath = resolve(process.env.HEW_PROJECT_COMPILER || 'hew-wasm/pkg/hew_wasm.js');
const vmPath = resolve(process.env.HEW_PROJECT_VM || 'hew-sandbox-vm/dist/interpreter/index.js');
const compiler = await import(pathToFileURL(compilerPath));
compiler.initSync({module: readFileSync(join(dirname(compilerPath), 'hew_wasm_bg.wasm'))});
const {runBytecode} = await import(pathToFileURL(vmPath));
const compile = (entry, files) => JSON.parse(compiler.compileProjectToSandboxBytecode(JSON.stringify({entry, files}), 'sandbox-vm-export'));
const source = (path, source) => ({path, source});
const manifest = source('user/hew.toml', '[package]\nname = "user"\n');

async function execute(output, stdin = '') {
  assert.ok(output.bytecode, JSON.stringify(output.diagnostics));
  const trace = await runBytecode(output.bytecode, {replay: {inputs: [{kind: 'stdin', data: stdin}]}});
  return {trace, stdout: trace.final_state.stdout.join('')};
}

test('language cohort executes through the browser compiler and VM', async t => {
  const cases = [
    ['tests/core-acceptance/cases/generic-closure-composition.hew', 'generic closure composition\n'],
    ['tests/core-acceptance/cases/default-argument-values.hew', '1\n2\n90\n721\n80\n1206\n90\n7\n90\n7\n90\n7\n90\n7\n90\n7\n0\n'],
    ['tests/core-acceptance/cases/duration-decimals.hew', 'exact duration decimals\n'],
    ['tests/vertical-slice/accept/semaphore_leases.hew', 'semaphore leases ok\n'],
    ['tests/core-acceptance/cases/machine-generic-step-limits.hew', 'generic machine rollback passed\n'],
  ];
  for (const [path, expected] of cases) {
    await t.test(path, async () => {
      const output = compile('main.hew', [source('main.hew', readFileSync(path, 'utf8'))]);
      const {trace, stdout} = await execute(output);
      assert.equal(stdout, expected);
      assert.equal(trace.final_state.exit_code, 0);
    });
  }
});

test('named imports execute through the browser and retain the exact transitive closure', async () => {
  for (const [declaration, alias] of [['import user.nav;', 'nav'], ['import user . nav;', 'nav'], ['import user.nav as route;', 'route']]) {
    const output = compile('main.hew', [
      source('main.hew', `${declaration} fn main() { println(${alias}.go()); }`), manifest,
      source('user/nav.hew', 'import user.steps; pub fn go() -> string { steps.go() }'),
      source('user/steps.hew', 'pub fn go() -> string { "MOVE" }'),
      source('user/unused.hew', 'invalid unused source'),
    ]);
    const result = await execute(output);
    assert.equal(result.stdout, 'MOVE\n');
    assert.equal(result.trace.final_state.exit_code, 0);
    assert.deepEqual(output.resolved_files, ['main.hew', 'user/nav.hew', 'user/steps.hew']);
  }
});

test('directory peers and ordinary manifests share native module semantics', async () => {
  const output = compile('main.hew', [
    source('main.hew', 'import user.nav; fn main() { println(nav.go()); }'), manifest,
    source('user/nav/nav.hew', 'pub fn go() -> string { command() }'),
    source('user/nav/steps.hew', 'fn command() -> string { "RIGHT" }'),
  ]);
  assert.equal((await execute(output)).stdout, 'RIGHT\n');
  assert.deepEqual(output.resolved_files, ['main.hew', 'user/nav/nav.hew', 'user/nav/steps.hew']);
});

test('project stdin advances and executions do not retain prior input', async () => {
  const output = compile('main.hew', [source('main.hew', 'import std.io; fn main() { for i in 0..2 { println(io.read_line() ?? "<none>"); } }')]);
  assert.equal((await execute(output, 'first\nsecond\n')).stdout, 'first\nsecond\n');
  assert.equal((await execute(output, 'third\nfourth\n')).stdout, 'third\nfourth\n');
});

test('imported errors retain source identity and subsequent projects cannot reuse missing files', () => {
  const main = source('mission.hew', 'import user.nav; fn main() { println(nav.go()); }');
  const library = source('user/nav.hew', '// 🐦\npub fn go() -> string { 42 }');
  const output = compile('mission.hew', [main, library]);
  assert.equal(output.bytecode, null);
  const diagnostic = output.diagnostics.find(d => d.severity === 'error');
  assert.equal(diagnostic.source_module, library.path);
  assert.equal(new TextDecoder().decode(new TextEncoder().encode(library.source).slice(diagnostic.start_offset, diagnostic.end_offset)).trim(), '42');
  const missing = compile('mission.hew', [main]);
  assert.equal(missing.bytecode, null);
  assert.ok(missing.diagnostics.some(d => d.kind === 'E_MODULE_NOT_FOUND'));
  assert.deepEqual(missing.resolved_files, ['mission.hew']);
});

test('a real imported trap is terminal and keeps its original execution', async () => {
  const output = compile('main.hew', [
    source('main.hew', 'import user.nav; fn main() { println(nav.divide(0)); }'),
    source('user/nav.hew', 'pub fn divide(n: i64) -> i64 { 10 / n }'),
  ]);
  const {trace, stdout} = await execute(output);
  assert.notEqual(trace.result, 'ok');
  assert.equal(stdout, '');
  assert.equal(trace.final_state.runtime_failures[0].trap_kind, 'divide_by_zero');
});

test('loose editor buffers retain relative imports rather than acquiring project package roots', async () => {
  const files = new Map([
    ['user/hew.toml', '[package]\nname = "user"\n'],
    ['user/nav.hew', 'import steps; pub fn go() -> string { steps.go() }'],
    ['user/steps.hew', 'pub fn go() -> string { "LEFT" }'],
  ]);
  const main = 'import user.nav; fn main() { println(nav.go()); }';
  const output = JSON.parse(compiler.compileToSandboxBytecode(main, 'sandbox-vm-export', files));
  assert.equal((await execute(output)).stdout, 'LEFT\n');
  files.set('user/nav.hew', 'import user.steps; pub fn go() -> string { steps.go() }');
  const rootedImport = JSON.parse(compiler.compileToSandboxBytecode(main, 'sandbox-vm-export', files));
  assert.equal(rootedImport.bytecode, null);
  assert.ok(rootedImport.diagnostics.some(d => d.kind === 'E_MODULE_NOT_FOUND'));
});

test('browser Test compiles embedded stdlib and executes each selected test', async () => {
  const output = JSON.parse(compiler.compileTestsToSandboxBytecode(`
import std.math;
#[test] fn passes() { assert(math.abs(-4) == 4); println("browser test passed"); }
#[test] fn fails() { assert(1 == 2); }
#[test] #[real_time] fn host_only() {}
`, 'playground.hew'));
  assert.deepEqual(output.diagnostics, []);
  assert.equal(output.tests.length, 3);
  const [passes, fails, hostOnly] = output.tests;
  assert.equal(passes.identity, 'playground.hew::passes');
  const options = {codecBridge: compiler.executeCodec};
  const passed = await runBytecode(passes.bytecode, options);
  assert.equal(passed.result, 'ok');
  assert.equal(passed.final_state.stdout.join(''), 'browser test passed\n');
  const failed = await runBytecode(fails.bytecode, options);
  assert.notEqual(failed.result, 'ok');
  assert.equal(failed.final_state.runtime_failures[0].assertion.left, '1');
  assert.equal(hostOnly.real_time, true);
  assert.equal(hostOnly.bytecode, null);
});

for (const format of ['json', 'yaml', 'toml']) {
  test(`${format} Value stdlib cases execute with the native format engine`, async () => {
    const source = readFileSync(new URL(`../../tests/hew/${format}_test.hew`, import.meta.url), 'utf8');
    const output = JSON.parse(compiler.compileTestsToSandboxBytecode(source, `${format}_test.hew`));
    assert.ok(!output.diagnostics.some(d => d.severity === 'error'), JSON.stringify(output.diagnostics));
    assert.ok(output.tests.length > 0);
    for (const selected of output.tests) {
      assert.ok(selected.bytecode, `${selected.identity}: ${JSON.stringify(selected.diagnostics)}`);
      const trace = await runBytecode(selected.bytecode, {codecBridge: compiler.executeCodec});
      assert.equal(trace.result, 'ok', `${selected.identity}: ${JSON.stringify(trace.final_state)}`);
    }
  });
}

test('YAML Value Wasm parsing preserves native input guards', () => {
  const parse = text => JSON.parse(compiler.executeCodec(JSON.stringify({encoding_value: {format: 'Yaml', operation: 'parse', text}})));
  for (const [text, reason] of [
    ['a'.repeat(2 * 1024 * 1024), 'size limit'],
    [Array.from({length: 201}, (_, i) => `k${i}: &a${i} value`).join('\n'), 'anchor limit'],
    ['a: &a 1\nb: [' + Array(1025).fill('*a').join(',') + ']\n', 'alias limit'],
  ]) assert.match(parse(text).error, new RegExp(reason));
  assert.ok(parse('title: "AT&T"\nurl: "https://example.com?a=1&b=2"').value);
});

test('JSON parsing errors and recovery execute in separate actors', async () => {
  const source = `
import std.encoding.json;
actor Parser {
    receive fn parse(text: string) -> string {
        match json.parse(text) {
            .Ok(_) => "",
            .Err(json.ParseError.Invalid(message)) => message,
        }
    }
}
fn main() {
    let first = spawn Parser;
    let second = spawn Parser;
    assert(first.parse("[").expect("first parse") == "EOF while parsing a list at line 1 column 1");
    assert(second.parse("null").expect("second parse") == "");
    assert(first.parse("null").expect("clear parse") == "");
}`;
  const output = JSON.parse(compiler.compileToSandboxBytecode(source, 'sandbox-vm-export'));
  assert.ok(output.bytecode, JSON.stringify(output.diagnostics));
  const trace = await runBytecode(output.bytecode, {codecBridge: compiler.executeCodec});
  assert.equal(trace.result, 'ok', JSON.stringify(trace.final_state));
});
