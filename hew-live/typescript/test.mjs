import { generate } from './generate.mjs';
import { copyFile, mkdtemp, rm, symlink } from 'node:fs/promises';
import { spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const directory = await mkdtemp(join(tmpdir(), 'hew-typescript-'));
function run(args) {
  const result = spawnSync(process.execPath, args, { cwd: directory, stdio: 'inherit', timeout: 30_000 });
  if (result.error) throw result.error;
  if (result.status !== 0) throw new Error(`binding consumer exited with ${result.status}`);
}
try {
  let component = process.argv[2];
  if (!component) {
    if (!process.env.HEW_LIVE_NODE) throw new Error('set HEW_LIVE_NODE to an installed SDK companion or run make live-node');
    component = join(directory, 'preview-router.wasm');
    const build = spawnSync(process.env.HEW_LIVE_NODE, ['build', '--source',
      fileURLToPath(new URL('../tests/fixtures/preview_router.hew', import.meta.url)), '--output', component],
      { stdio: 'inherit', timeout: 120_000 });
    if (build.error) throw build.error;
    if (build.status !== 0) throw new Error(`fixture build exited with ${build.status}`);
  }
  await generate(directory, component);
  await copyFile(new URL('consumer.ts', import.meta.url), join(directory, 'consumer.ts'));
  await symlink(fileURLToPath(new URL('node_modules', import.meta.url)), join(directory, 'node_modules'), 'junction');
  run([fileURLToPath(new URL('node_modules/typescript/bin/tsc', import.meta.url)),
    '--strict', '--target', 'es2022', '--module', 'nodenext', '--skipLibCheck', 'consumer.ts']);
  run(['--experimental-wasm-jspi', 'consumer.js']);
} finally {
  await rm(directory, { recursive: true, force: true });
}
