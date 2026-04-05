#!/usr/bin/env bash
# Pre-commit: format staged files + run clippy on Rust changes.
# Called by git-multi-hook via .git/hooks/pre-commit.d/format symlink.
#
# Compatible with Bash 3.2 (macOS default) and Bash 5+ (Linux).

# Collect staged files matching the given glob patterns into a global
# array variable. Usage: staged_files VARNAME '*.rs' '*.toml'
# Uses eval to work around Bash 3.2 lacking `local -n` (nameref).
staged_files() {
    local varname=$1
    shift
    local _staged=()
    while IFS= read -r f; do
        [ -n "$f" ] && _staged+=("$f")
    done < <(git diff --cached --name-only --diff-filter=ACM -- "$@")
    # shellcheck disable=SC2294 # eval is intentional — Bash 3.2 compat
    eval "$varname=(\"\${_staged[@]}\")"
}

# Format files and re-stage them. Usage: fmt_and_restage VARNAME cmd args...
fmt_and_restage() {
    local varname=$1
    shift
    # shellcheck disable=SC2294,SC2154 # eval + indirect variable — Bash 3.2 compat
    eval "local _count=\${#${varname}[@]}"
    # shellcheck disable=SC2154 # _count set via eval above
    if (( _count > 0 )); then
        # shellcheck disable=SC2294 # eval is intentional — Bash 3.2 compat
        eval '"$@" "\${'"$varname"'[@]}"' 2>/dev/null
        # shellcheck disable=SC2294 # eval is intentional — Bash 3.2 compat
        eval 'git add "\${'"$varname"'[@]}"'
    fi
}

# Rust — cargo fmt formats the whole project; just restage what was staged.
# Then run clippy to catch warnings before they fail CI.
staged_files rs_files '*.rs'
# shellcheck disable=SC2154 # rs_files set by staged_files via eval
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
    if ! CARGO_INCREMENTAL=0 cargo clippy --workspace --tests --message-format=json -- -D warnings \
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
staged_files cpp_files '*.cpp' '*.h' '*.hpp'
fmt_and_restage cpp_files clang-format -i

# TOML
staged_files toml_files '*.toml'
fmt_and_restage toml_files taplo fmt

# Shell
staged_files sh_files '*.sh'
fmt_and_restage sh_files shfmt -i 4 -w

# Markdown, YAML, JSON, JavaScript
staged_files pretty_files '*.yml' '*.yaml' '*.json' '*.md' '*.js' '*.jsx' '*.ts' '*.tsx'
fmt_and_restage pretty_files prettier --write --log-level silent

# Python
staged_files py_files '*.py'
fmt_and_restage py_files ruff format
