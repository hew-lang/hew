#!/usr/bin/env bash
# Pre-commit: format staged files.
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
    if ((_count > 0)); then
        # shellcheck disable=SC2294 # eval is intentional — Bash 3.2 compat
        eval '"$@" "${'"$varname"'[@]}"' 2>/dev/null
        # shellcheck disable=SC2294 # eval is intentional — Bash 3.2 compat
        eval 'git add "${'"$varname"'[@]}"'
    fi
}

# Rust — cargo fmt formats the whole project; just restage what was staged.
# `make lint-rust` and CI run the authoritative JSON Clippy check.
staged_files rs_files '*.rs'
# shellcheck disable=SC2154 # rs_files set by staged_files via eval
if ((${#rs_files[@]} > 0)); then
    cargo fmt --all --quiet
    git add "${rs_files[@]}"
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
