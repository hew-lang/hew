#!/usr/bin/env bash
#
# smoke-bootstrap.sh — self-provision the libFuzzer/cargo-fuzz toolchain the
# nightly fuzz-smoke gate needs (`make fuzz-smoke`).
#
# Mirrors the structural-lint bootstrap pattern (scripts/ast-grep-lint.sh
# --bootstrap / build-ast-grep-lang.sh): the install is a prerequisite of the
# gate target, not a separate manual step, and it is idempotent — a warm
# runner (nightly toolchain + cargo-fuzz already present) makes this a fast
# no-op instead of a network round-trip on every invocation.
set -euo pipefail

if ! rustup toolchain list 2>/dev/null | grep -q '^nightly'; then
  echo "fuzz-smoke-bootstrap: installing nightly toolchain"
  rustup toolchain install nightly --profile minimal
else
  echo "fuzz-smoke-bootstrap: nightly toolchain already installed"
fi

if ! command -v cargo-fuzz >/dev/null 2>&1; then
  echo "fuzz-smoke-bootstrap: installing cargo-fuzz"
  cargo install cargo-fuzz --locked
else
  echo "fuzz-smoke-bootstrap: cargo-fuzz already installed ($(cargo fuzz --version 2>&1 | head -1))"
fi
