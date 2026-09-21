#!/usr/bin/env node
/** Resolve release consumers using the registry's authenticated package data. */
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { copyFileSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join, resolve } from 'node:path';

const registry = 'https://npm.pkg.github.com';
const names = ['@hew-lang/wasm', '@hew-lang/sandbox-vm', '@hew-lang/playground-sandbox'];
const output = resolve(process.env.HEW_DEPENDENCY_ARTIFACT_DIR ?? 'browser-packages');
mkdirSync(output, { recursive: true });
const json = (path) => JSON.parse(readFileSync(path, 'utf8'));
const npm = (args) => execFileSync('npm', args, { encoding: 'utf8' }).trim();

if (process.argv[2] === 'inspect') {
  const versions = Object.fromEntries(names.map((name) => [
    name, JSON.parse(npm(['view', name, 'versions', '--json', '--registry', registry])),
  ]));
  writeFileSync(join(output, 'registry-versions.json'), JSON.stringify(versions, null, 2) + '\n');
  console.log(JSON.stringify(versions, null, 2));
} else if (process.argv[2] === 'resolve') {
  const commit = execFileSync('git', ['rev-parse', 'HEAD'], { encoding: 'utf8' }).trim();
  if (commit !== process.env.HEW_CONSUMER_COMMIT) throw new Error('Consumer checkout does not match its requested commit.');
  const manifest = json('package.json');
  for (const section of ['dependencies', 'devDependencies', 'optionalDependencies', 'peerDependencies']) {
    for (const [name, version] of Object.entries(manifest[section] ?? {})) {
      if (/^(file:|link:|workspace:)/.test(version)) throw new Error(`${name} still uses a local dependency.`);
    }
  }
  execFileSync('npm', ['install', '--package-lock-only', '--ignore-scripts', '--no-audit', '--no-fund'], { stdio: 'inherit' });
  execFileSync('npm', ['ci', '--no-audit', '--no-fund'], { stdio: 'inherit' });
  const lockBytes = readFileSync('package-lock.json');
  const lock = JSON.parse(lockBytes);
  const packages = [];
  for (const name of names) {
    const entry = lock.packages[`node_modules/${name}`];
    if (!entry) continue;
    if (entry.link || !entry.resolved?.startsWith(`${registry}/`) || !entry.integrity) {
      throw new Error(`${name} did not resolve to an authenticated registry archive.`);
    }
    const installed = json(join('node_modules', name, 'package.json'));
    if (name !== '@hew-lang/playground-sandbox' &&
        (!/^[a-f0-9]{40}$/.test(installed.hewSource?.commit ?? '') || installed.hewSource.dirty !== false)) {
      throw new Error(`${name} has no clean source revision in its published metadata.`);
    }
    const archives = JSON.parse(npm(['pack', `${name}@${entry.version}`, '--ignore-scripts', '--json', '--pack-destination', output, '--registry', registry]));
    if (archives.length !== 1 || archives[0].integrity !== entry.integrity) {
      throw new Error(`${name} archive does not match the generated lockfile.`);
    }
    packages.push({ name, version: entry.version, resolved: entry.resolved, integrity: entry.integrity,
      archive: archives[0].filename, ...(installed.hewSource ? { source: installed.hewSource } : {}) });
  }
  const engine = packages.filter((pkg) => pkg.name !== '@hew-lang/playground-sandbox');
  if (engine.length !== 2 || engine[0].version !== engine[1].version || engine[0].source.commit !== engine[1].source.commit) {
    throw new Error('Consumer must resolve the compiler and VM from one release revision.');
  }
  copyFileSync('package-lock.json', join(output, 'package-lock.json'));
  writeFileSync(join(output, 'source-manifest.json'), JSON.stringify({
    repository: process.env.HEW_CONSUMER_REPOSITORY,
    commit,
    package: { name: manifest.name, version: manifest.version },
    lockfileSha256: createHash('sha256').update(lockBytes).digest('hex'),
    packages,
  }, null, 2) + '\n');
} else {
  throw new Error('Expected inspect or resolve.');
}
