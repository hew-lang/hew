#!/usr/bin/env bash
#
# ast-grep-lint.sh — run the Hew ast-grep rule set over the repository.
# Rules live under rules/ (wired via sgconfig.yml ruleDirs); .hew rules need
# the compiled grammar (scripts/build-ast-grep-lang.sh). Exits non-zero if any
# error-severity rule matches, so it can gate CI.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."
exec ast-grep scan "$@"
