#!/usr/bin/env bash

# Invoke one runtime-coverage program with its deterministic product inputs.
#
# Keep the zero-argument path explicit: macOS ships Bash 3.2, where expanding
# an empty array under `set -u` aborts with "unbound variable".  Passing the
# command-style example's arguments here also keeps the invocation policy in
# one reusable, directly testable seam.
coverage_runtime_run_program() {
    local stem="$1"
    local profile_file="$2"
    local timeout_bin="$3"
    local timeout_seconds="$4"
    local program_bin="$5"
    local grep_input="$6"

    case "${stem}" in
        hew_grep)
            LLVM_PROFILE_FILE="${profile_file}" \
                "${timeout_bin}" "${timeout_seconds}" "${program_bin}" \
                "needle" "${grep_input}"
            ;;
        *)
            LLVM_PROFILE_FILE="${profile_file}" \
                "${timeout_bin}" "${timeout_seconds}" "${program_bin}"
            ;;
    esac
}
