#!/usr/bin/env bash
# Format staged files before commit.
# Called by git-multi-hook via .git/hooks/pre-commit.d/format symlink.
# Each formatter runs only when staged files match its glob.
# Formatted files are automatically re-staged.

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

# Rust — cargo fmt formats the whole project; just restage what was staged
staged_into rs_files '*.rs'
# shellcheck disable=SC2154 # rs_files set via nameref in staged_into
if ((${#rs_files[@]} > 0)); then
    cargo fmt --quiet 2>/dev/null
    git add "${rs_files[@]}"
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
