#!/usr/bin/env node
// Builds the publishable npm packages for Hew's browser-facing artifacts into
// `target/npm/<pkg>`, each a ready-to-`npm publish` directory:
//
//   @hew-lang/wasm          <- hew-wasm        (analysis-only diagnostics / editor + LSP tooling)
//   @hew-lang/sandbox-wasm  <- hew-sandbox-wasm (sandbox bytecode export)
//   @hew-lang/sandbox-vm    <- hew-sandbox-vm   (deterministic TypeScript interpreter)
//
// All three are versioned in lockstep with the Cargo workspace version
// ([workspace.package].version in Cargo.toml). The two wasm packages are built
// with wasm-pack; the interpreter is built with its own `tsc`. Set
// NPM_WASM_PROFILE=dev for a faster (unoptimized) local validation build.

import { execFileSync } from 'node:child_process';
import {
  cpSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const outRoot = join(repoRoot, 'target', 'npm');
const profileFlag = process.env.NPM_WASM_PROFILE === 'dev' ? '--dev' : '--release';

function run(command, args, cwd = repoRoot) {
  execFileSync(command, args, { cwd, stdio: 'inherit' });
}

function workspaceVersion() {
  const cargo = readFileSync(join(repoRoot, 'Cargo.toml'), 'utf8');
  const match = cargo.match(/\[workspace\.package\][\s\S]*?\nversion\s*=\s*"([^"]+)"/);
  if (!match) {
    throw new Error('could not read [workspace.package].version from Cargo.toml');
  }
  return match[1];
}

const COMMON = {
  author: 'Stephen Olesen',
  license: 'MIT OR Apache-2.0',
  repository: { type: 'git', url: 'git+https://github.com/hew-lang/hew.git' },
  homepage: 'https://github.com/hew-lang/hew#readme',
  bugs: { url: 'https://github.com/hew-lang/hew/issues' },
  publishConfig: { access: 'public' },
};

function buildWasmPackage({ crate, name, dir }, version) {
  const out = join(outRoot, dir);
  run('wasm-pack', [
    'build',
    crate,
    '--target',
    'web',
    '--scope',
    'hew-lang',
    '--out-dir',
    out,
    profileFlag,
  ]);
  const pkgPath = join(out, 'package.json');
  const pkg = JSON.parse(readFileSync(pkgPath, 'utf8'));
  // wasm-pack scopes the crate name (@hew-lang/hew-sandbox-wasm); rename to the
  // published name and stamp the lockstep version + publish metadata.
  pkg.name = name;
  pkg.version = version;
  Object.assign(pkg, COMMON);
  writeFileSync(pkgPath, `${JSON.stringify(pkg, null, 2)}\n`);
  console.log(`built ${name}@${version} -> ${out}`);
}

function buildInterpreterPackage(version) {
  const src = join(repoRoot, 'hew-sandbox-vm');
  run('npm', ['ci'], src);
  run('npm', ['run', 'build'], src);

  const out = join(outRoot, 'sandbox-vm');
  mkdirSync(out, { recursive: true });
  cpSync(join(src, 'dist'), join(out, 'dist'), { recursive: true });
  cpSync(join(src, 'README.md'), join(out, 'README.md'));

  // Derive a clean, script-free publish manifest from the in-repo package.json
  // so `npm publish` never runs dev/prepack scripts against the staged copy.
  const source = JSON.parse(readFileSync(join(src, 'package.json'), 'utf8'));
  const manifest = {
    name: '@hew-lang/sandbox-vm',
    version,
    description: source.description,
    ...COMMON,
    type: 'module',
    sideEffects: false,
    main: './dist/interpreter/index.js',
    types: './dist/interpreter/index.d.ts',
    exports: {
      '.': {
        types: './dist/interpreter/index.d.ts',
        import: './dist/interpreter/index.js',
      },
    },
    files: ['dist', 'README.md'],
    keywords: ['hew', 'sandbox', 'interpreter', 'vm', 'typescript'],
  };
  writeFileSync(join(out, 'package.json'), `${JSON.stringify(manifest, null, 2)}\n`);
  console.log(`built @hew-lang/sandbox-vm@${version} -> ${out}`);
}

function main() {
  const version = workspaceVersion();
  rmSync(outRoot, { recursive: true, force: true });
  mkdirSync(outRoot, { recursive: true });

  buildWasmPackage({ crate: 'hew-wasm', name: '@hew-lang/wasm', dir: 'wasm' }, version);
  buildWasmPackage(
    { crate: 'hew-sandbox-wasm', name: '@hew-lang/sandbox-wasm', dir: 'sandbox-wasm' },
    version,
  );
  buildInterpreterPackage(version);

  console.log(`\nAll npm packages staged under ${outRoot} at version ${version}.`);
}

main();
