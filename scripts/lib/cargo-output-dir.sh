#!/usr/bin/env bash
# Shared Cargo artifact-directory resolver for standalone gate runners.
#
# Make already derives DEBUG_DIR from scripts/cargo-output-dir.py. Scripts
# remain directly runnable, so their fallback must use that same resolver
# instead of assuming <repo>/target/debug.

cargo_profile_dir() {
    local repo_root="$1"
    local profile="$2"
    local resolved
    resolved="$("${repo_root}/scripts/cargo-output-dir.py" --profile "${profile}")"
    case "${resolved}" in
        /*) printf '%s\n' "${resolved}" ;;
        *)  printf '%s\n' "${repo_root}/${resolved}" ;;
    esac
}

cargo_debug_dir() {
    cargo_profile_dir "$1" debug
}
