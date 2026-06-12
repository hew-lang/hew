#!/usr/bin/env node
/**
 * Build all three Hew npm packages and stage them under target/npm/<pkg>.
 *
 * Packages produced:
 *   target/npm/@hew-lang/wasm          ← hew-wasm crate (wasm-pack --target web)
 *   target/npm/@hew-lang/sandbox-wasm  ← hew-sandbox-wasm crate (wasm-pack --target web)
 *   target/npm/@hew-lang/sandbox-vm    ← hew-sandbox-vm (tsc)
 *
 * Environment:
 *   NPM_WASM_PROFILE=dev   — use debug wasm-pack profile (fast local builds)
 *
 * Usage:
 *   node scripts/build-npm-packages.mjs
 */

import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync, cpSync, rmSync } from "node:fs";
import { resolve, join } from "node:path";
import { fileURLToPath } from "node:url";

const REPO_ROOT = resolve(fileURLToPath(import.meta.url), "../..");
const GITHUB_PACKAGES_REGISTRY = "https://npm.pkg.github.com";

// Honour NPM_WASM_PROFILE=dev for fast local builds.
// WHY: wasm-pack release builds run wasm-opt which takes minutes; dev skips it.
// WHEN obsolete: never — this is the intentional local-vs-CI distinction.
// REAL solution: this IS the real solution.
const wasmProfile = process.env.NPM_WASM_PROFILE === "dev" ? "dev" : "release";

/** Read the workspace Cargo.toml and extract the package version. */
function workspaceVersion() {
  const cargoToml = readFileSync(join(REPO_ROOT, "Cargo.toml"), "utf8");
  const m = cargoToml.match(/\[workspace\.package\][\s\S]*?\nversion\s*=\s*"([^"]+)"/);
  if (!m) throw new Error("Could not find [workspace.package].version in workspace Cargo.toml");
  return m[1];
}

function run(cmd, args, opts = {}) {
  console.log(`  $ ${cmd} ${args.join(" ")}`);
  execFileSync(cmd, args, { stdio: "inherit", cwd: REPO_ROOT, ...opts });
}

/**
 * Build a wasm-pack crate and stage the output as a properly-named package.
 *
 * wasm-pack sets the package name to `@<scope>/<crate-name>` (e.g.
 * `@hew-lang/hew-wasm`). We rename it to the canonical name and update
 * `publishConfig` for GitHub Packages.
 */
function buildWasmCrate({ crate, outName, version }) {
  const stagingDir = join(REPO_ROOT, "target", "npm", "@hew-lang", outName);

  console.log(`\n==> Building ${crate} (profile: ${wasmProfile}) → @hew-lang/${outName}`);

  // wasm-pack outputs to <crate>/pkg by default; use --out-dir to target our stage location.
  rmSync(stagingDir, { recursive: true, force: true });
  mkdirSync(stagingDir, { recursive: true });

  run("wasm-pack", [
    "build",
    crate,
    "--target", "web",
    "--scope", "hew-lang",
    `--${wasmProfile}`,
    "--out-dir", stagingDir,
    "--out-name", outName.replace(/-/g, "_"),
  ]);

  // Patch package.json: rename, stamp version, set publishConfig.
  const pkgPath = join(stagingDir, "package.json");
  const pkg = JSON.parse(readFileSync(pkgPath, "utf8"));
  pkg.name = `@hew-lang/${outName}`;
  pkg.version = version;
  pkg.publishConfig = {
    registry: GITHUB_PACKAGES_REGISTRY,
    access: "public",
  };
  // Remove the .gitignore wasm-pack emits so `npm pack` captures all files.
  delete pkg[".gitignore"];
  writeFileSync(pkgPath, JSON.stringify(pkg, null, 2) + "\n");

  // Remove the wasm-pack .gitignore (it excludes .wasm from pack).
  const gitignorePath = join(stagingDir, ".gitignore");
  rmSync(gitignorePath, { force: true });

  console.log(`  ✓ staged @hew-lang/${outName}@${version}`);
  return stagingDir;
}

/**
 * Build hew-sandbox-vm via tsc and stage the dist output.
 */
function buildSandboxVm({ version }) {
  const vmDir = join(REPO_ROOT, "hew-sandbox-vm");
  const distDir = join(vmDir, "dist");
  const stagingDir = join(REPO_ROOT, "target", "npm", "@hew-lang", "sandbox-vm");

  console.log(`\n==> Building hew-sandbox-vm (tsc) → @hew-lang/sandbox-vm`);

  // Install deps if not present (idempotent).
  run("npm", ["ci", "--prefix", vmDir]);

  // Build TypeScript and run post-build validation via the package's own build
  // script (tsc + validate-sandbox-vm.mjs). Using `npm run build` ensures the
  // full build pipeline runs, including any future pre/post hooks.
  run("npm", ["run", "build", "--prefix", vmDir]);

  // Stage: copy dist output + a fresh package.json.
  rmSync(stagingDir, { recursive: true, force: true });
  mkdirSync(stagingDir, { recursive: true });

  cpSync(distDir, join(stagingDir, "dist"), { recursive: true });

  // Copy README if present.
  const readmeSrc = join(vmDir, "README.md");
  try {
    cpSync(readmeSrc, join(stagingDir, "README.md"));
  } catch {
    // No README — that is fine.
  }

  // Build a clean package.json for the published package.
  const srcPkg = JSON.parse(readFileSync(join(vmDir, "package.json"), "utf8"));
  const publishPkg = {
    name: "@hew-lang/sandbox-vm",
    version,
    description: srcPkg.description ?? "Deterministic TypeScript interpreter for the Hew educational sandbox",
    type: "module",
    main: "./dist/interpreter/index.js",
    types: "./dist/interpreter/index.d.ts",
    exports: {
      ".": {
        import: "./dist/interpreter/index.js",
        types: "./dist/interpreter/index.d.ts",
      },
      "./scheduler": {
        import: "./dist/scheduler/scheduler.js",
        types: "./dist/scheduler/scheduler.d.ts",
      },
    },
    files: ["dist/"],
    keywords: srcPkg.keywords ?? ["hew", "sandbox", "interpreter", "wasm"],
    license: "MIT OR Apache-2.0",
    repository: {
      type: "git",
      url: "git+https://github.com/hew-lang/hew.git",
      directory: "hew-sandbox-vm",
    },
    publishConfig: {
      registry: GITHUB_PACKAGES_REGISTRY,
      access: "public",
    },
  };
  writeFileSync(
    join(stagingDir, "package.json"),
    JSON.stringify(publishPkg, null, 2) + "\n"
  );

  console.log(`  ✓ staged @hew-lang/sandbox-vm@${version}`);
  return stagingDir;
}

async function main() {
  const version = workspaceVersion();
  console.log(`Building Hew npm packages at version ${version} (wasm profile: ${wasmProfile})`);

  mkdirSync(join(REPO_ROOT, "target", "npm"), { recursive: true });

  buildWasmCrate({ crate: "hew-wasm", outName: "wasm", version });
  buildWasmCrate({ crate: "hew-sandbox-wasm", outName: "sandbox-wasm", version });
  buildSandboxVm({ version });

  console.log(`\nAll packages staged under target/npm/@hew-lang/`);
  console.log(`  @hew-lang/wasm@${version}`);
  console.log(`  @hew-lang/sandbox-wasm@${version}`);
  console.log(`  @hew-lang/sandbox-vm@${version}`);
  console.log(`\nTo verify: npm pack --dry-run  in each staged directory.`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
