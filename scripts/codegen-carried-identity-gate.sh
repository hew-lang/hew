#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

pattern='contains\("__recv__"\)|split_once\("__recv__"\)|strip_suffix\("__step"\)|starts_with\("hew_metric_"\)|actor_name_from_handler_symbol|actor_layout_key_from_handler_symbol|is_machine_step_symbol'

# Deliberately not gated here: the TCP/DNS blocking-offload capability still
# uses module_uses_blocking_offload's hew_tcp_connect/hew_dns_ scan. A follow-up
# change will carry that identity from stdlib externs via a capability attribute.
if rg -n "$pattern" hew-codegen-rs/src; then
    echo "error: codegen reintroduced a string consumer for MIR-carried identity" >&2
    exit 1
fi

echo "codegen carried-identity gate: OK"
