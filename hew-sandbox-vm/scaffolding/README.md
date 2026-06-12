# Sandbox VM scaffolding

This directory contains tooling for the specification-only sandbox VM layer.
It is deliberately small and browser-runtime-neutral:

- `../package.json` defines the private validation package.
- `scripts/validate-sandbox-vm.mjs` validates schema syntax, fixture traces, the
  minimum fixture count, and Markdown links.

The scaffold must stay free of interpreter logic. Runtime behavior belongs to
later milestones after the canonical frontend reuse and sandbox stdlib surfaces
are available.
