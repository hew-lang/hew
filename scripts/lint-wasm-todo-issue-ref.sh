#!/usr/bin/env bash
# Compatibility entry point for the offline WASM backlog-authority lint.
set -Eeuo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
exec python3 "${REPO_ROOT}/scripts/lint-wasm-todo.py" "$@"
