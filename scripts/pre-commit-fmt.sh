#!/usr/bin/env bash
# Pre-commit: format staged files + run clippy on Rust changes.
# Called by git-multi-hook via .git/hooks/pre-commit.d/format symlink.

staged_into() {
    # shellcheck disable=SC2178 # nameref, not a regular variable
    local -n arr=$1
    shift
    arr=()
    while IFS= read -r f; do
        [ -n "$f" ] && arr+=("$f")
    done < <(git diff --cached --name-only --diff-filter=ACM -- "$@")
}

fmt_and_restage() {
    # shellcheck disable=SC2178 # nameref, not a regular variable
    local -n files=$1
    shift
    if ((${#files[@]} > 0)); then
        "$@" "${files[@]}" 2>/dev/null
        git add "${files[@]}"
    fi
}

# Rust — cargo fmt formats the whole project; just restage what was staged.
# Then run clippy to catch warnings before they fail CI.
staged_into rs_files '*.rs'
# shellcheck disable=SC2154 # rs_files set via nameref in staged_into
if ((${#rs_files[@]} > 0)); then
    cargo fmt --all --quiet
    git add "${rs_files[@]}"

    # Block commit if clippy finds warnings — matches CI exactly.
    # --message-format=json forces fresh analysis (no cached human-readable
    # output) and mirrors the CI invocation. -D warnings treats all warnings
    # as errors, same as CI.
    # Stdout (JSON diagnostics) goes to a tmpfile; stderr (progress) passes
    # through so the developer sees compilation output.
    clippy_json=$(mktemp)
    if ! cargo clippy --workspace --tests --message-format=json -- -D warnings \
        >"$clippy_json"; then
        echo ""
        echo "clippy found issues — fix before committing."
        echo ""
        if command -v jq &>/dev/null; then
            jq -r '
                select(.message.level == "error" or .message.level == "warning")
                | .message.rendered // empty
            ' <"$clippy_json" | head -80
        else
            python3 -c '
import json, sys
for line in open(sys.argv[1]):
    try:
        msg = json.loads(line).get("message", {})
        if isinstance(msg, dict) and msg.get("level") in ("error", "warning"):
            r = msg.get("rendered", "")
            if r: sys.stdout.write(r)
    except Exception:
        pass
' "$clippy_json" | head -80
        fi
        rm -f "$clippy_json"
        exit 1
    fi
    rm -f "$clippy_json"
fi

# C++ / Headers
staged_into cpp_files '*.cpp' '*.h' '*.hpp'
fmt_and_restage cpp_files clang-format -i

# TOML
staged_into toml_files '*.toml'
fmt_and_restage toml_files taplo fmt

# Shell
staged_into sh_files '*.sh'
fmt_and_restage sh_files shfmt -i 4 -w

# Markdown, YAML, JSON, JavaScript
staged_into pretty_files '*.yml' '*.yaml' '*.json' '*.md' '*.js' '*.jsx' '*.ts' '*.tsx'
fmt_and_restage pretty_files prettier --write --log-level silent

# Python
staged_into py_files '*.py'
fmt_and_restage py_files ruff format
