#!/usr/bin/env bash
# sync-downstream.sh — Comprehensive downstream sync for the Hew ecosystem.
#
# Validates syntax-data.json against the lexer, generates artefacts into dist/,
# then distributes them to all downstream repos.
#
# Usage:
#   ./scripts/sync-downstream.sh           # generate + distribute
#   ./scripts/sync-downstream.sh --check   # dry-run: report drift only
#   ./scripts/sync-downstream.sh --commit  # also git commit each repo
#   ./scripts/sync-downstream.sh --push    # commit + push each repo
#
# Environment variables (override sibling repo locations):
#   HEW_SYNC_PARENT       Parent dir of sibling repos (default: parent of REPO_ROOT)
#   HEW_SYNC_VSCODE_HEW   Path to vscode-hew repo
#   HEW_SYNC_HEW_SH       Path to hew.sh repo
#   HEW_SYNC_HEW_RUN      Path to hew.run repo
#   HEW_SYNC_TREE_SITTER   Path to tree-sitter-hew repo
#   HEW_SYNC_VIM_HEW      Path to vim-hew repo
#   HEW_SYNC_HEW_STUDIO   Path to hew-studio repo

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SYNTAX_DATA="$REPO_ROOT/docs/syntax-data.json"
TOOLS_DIR="$REPO_ROOT/tools/downstream"
DIST_DIR="$REPO_ROOT/dist"

# Downstream repos (sibling directories by convention, overridable)
DOWNSTREAM_PARENT="${HEW_SYNC_PARENT:-$(dirname "$REPO_ROOT")}"
VSCODE_HEW="${HEW_SYNC_VSCODE_HEW:-$DOWNSTREAM_PARENT/vscode-hew}"
HEW_SH="${HEW_SYNC_HEW_SH:-$DOWNSTREAM_PARENT/hew.sh}"
HEW_RUN="${HEW_SYNC_HEW_RUN:-$DOWNSTREAM_PARENT/hew.run}"
TREE_SITTER="${HEW_SYNC_TREE_SITTER:-$DOWNSTREAM_PARENT/tree-sitter-hew}"
VIM_HEW="${HEW_SYNC_VIM_HEW:-$DOWNSTREAM_PARENT/vim-hew}"
HEW_STUDIO="${HEW_SYNC_HEW_STUDIO:-$DOWNSTREAM_PARENT/hew-studio}"

# Parse flags
COMMIT=false
PUSH=false
CHECK_ONLY=false
for arg in "$@"; do
    case "$arg" in
    --commit) COMMIT=true ;;
    --push)
        COMMIT=true
        PUSH=true
        ;;
    --check) CHECK_ONLY=true ;;
    --help | -h)
        echo "Usage: $0 [--check] [--commit] [--push]"
        echo ""
        echo "  --check   Dry-run: validate syntax-data.json and report drift only"
        echo "  --commit  Commit changes in each downstream repo after distribution"
        echo "  --push    Commit and push changes in each downstream repo"
        echo ""
        echo "Environment variables:"
        echo "  HEW_SYNC_PARENT       Parent dir of sibling repos"
        echo "  HEW_SYNC_VSCODE_HEW   Path to vscode-hew repo"
        echo "  HEW_SYNC_HEW_SH       Path to hew.sh repo"
        echo "  HEW_SYNC_HEW_RUN      Path to hew.run repo"
        echo "  HEW_SYNC_TREE_SITTER   Path to tree-sitter-hew repo"
        echo "  HEW_SYNC_VIM_HEW      Path to vim-hew repo"
        echo "  HEW_SYNC_HEW_STUDIO   Path to hew-studio repo"
        exit 0
        ;;
    esac
done

# Colours
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

ok() { echo -e "  ${GREEN}✓${NC} $1"; }
warn() { echo -e "  ${YELLOW}⚠${NC} $1"; }
fail() { echo -e "  ${RED}✗${NC} $1"; }
info() { echo -e "  ${CYAN}→${NC} $1"; }

ERRORS=0
UPDATED=0
DRIFTS=0
SKIPPED=0
STEP=0

next_step() {
    STEP=$((STEP + 1))
    echo ""
    echo -e "${BOLD}${STEP}. $1${NC}"
}

# Check-mode temporary directory
CHECK_ROOT=""
cleanup() {
    if [ -n "$CHECK_ROOT" ] && [ -d "$CHECK_ROOT" ]; then
        rm -rf "$CHECK_ROOT"
    fi
}

copy_repo_snapshot() {
    local src=$1 dest=$2
    rm -rf "$dest"
    mkdir -p "$dest"
    if command -v rsync >/dev/null 2>&1; then
        rsync -a --exclude .git "$src"/ "$dest"/
    else
        tar -C "$src" --exclude=.git -cf - . | tar -C "$dest" -xf -
    fi
}

# Commit helper — commits staged files in the given repo directory
try_commit() {
    local repo_dir=$1
    local message=$2
    shift 2
    local files=("$@")

    cd "$repo_dir"
    git add "${files[@]}" 2>/dev/null || true
    if ! git diff --cached --quiet 2>/dev/null; then
        git commit -m "$message" --quiet
        ok "Committed"
        if $PUSH; then
            if git push --quiet 2>/dev/null; then
                ok "Pushed"
            else
                warn "Push failed (manual push needed)"
            fi
        fi
    fi
}

# Compare files between original and regenerated repos (for --check mode)
compare_files() {
    local original=$1 regenerated=$2 label=$3
    shift 3
    local drifted=0

    for file in "$@"; do
        local orig_file="$original/$file"
        local regen_file="$regenerated/$file"

        if [ -e "$orig_file" ] && [ -e "$regen_file" ]; then
            if ! cmp -s "$orig_file" "$regen_file"; then
                warn "$label drift: $file"
                drifted=1
            fi
        elif [ -e "$orig_file" ] || [ -e "$regen_file" ]; then
            warn "$label drift: $file (file presence differs)"
            drifted=1
        fi
    done

    return $drifted
}

if $CHECK_ONLY; then
    CHECK_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-downstream-sync.XXXXXX")"
    trap cleanup EXIT
fi

echo ""
echo "╔══════════════════════════════════════════════════════╗"
echo "║  Hew Downstream Sync                                ║"
echo "╚══════════════════════════════════════════════════════╝"
echo ""

if $CHECK_ONLY; then
    info "Mode: check (dry-run, reporting drift only)"
elif $PUSH; then
    info "Mode: commit + push"
elif $COMMIT; then
    info "Mode: commit"
else
    info "Mode: generate + distribute"
fi

# ── Step 1: Validate syntax-data.json against the lexer ──────────────
next_step "Validating syntax-data.json against lexer..."

if [ ! -f "$SYNTAX_DATA" ]; then
    fail "docs/syntax-data.json not found!"
    exit 1
fi

cd "$REPO_ROOT"
if cargo test -p hew-lexer -- syntax_data --quiet 2>/dev/null; then
    ok "syntax-data.json matches ALL_KEYWORDS"
else
    fail "syntax-data.json is OUT OF SYNC with hew-lexer!"
    echo ""
    echo "    Run: cargo test -p hew-lexer -- syntax_data"
    echo "    Then update docs/syntax-data.json to match ALL_KEYWORDS."
    exit 1
fi

# ── Step 2: Generate artefacts into dist/ ─────────────────────────────
next_step "Generating artefacts into dist/..."

mkdir -p "$DIST_DIR"

# 2a. TextMate grammar
if [ -d "$VSCODE_HEW" ] && [ -f "$VSCODE_HEW/syntaxes/hew.tmLanguage.json" ]; then
    info "Generating TextMate grammar..."
    if HEW_VSCODE_HEW="$VSCODE_HEW" node "$TOOLS_DIR/generate-tmgrammar.mjs" 2>&1 | tail -3; then
        ok "dist/hew.tmLanguage.json generated"
    else
        fail "TextMate grammar generation failed!"
        ERRORS=$((ERRORS + 1))
    fi
else
    warn "vscode-hew not found at $VSCODE_HEW (skipping tmLanguage generation)"
    SKIPPED=$((SKIPPED + 1))
fi

# 2b. nano syntax
info "Generating nano syntax..."
if node "$TOOLS_DIR/generate-nano.mjs" 2>&1 | tail -2; then
    ok "dist/hew.nanorc generated"
else
    fail "nano syntax generation failed!"
    ERRORS=$((ERRORS + 1))
fi

# ── Step 3: Distribute to downstream repos ────────────────────────────

# 3a. vscode-hew — copy dist/hew.tmLanguage.json → syntaxes/
next_step "vscode-hew (TextMate grammar)..."
if [ -d "$VSCODE_HEW" ]; then
    if [ -f "$DIST_DIR/hew.tmLanguage.json" ]; then
        if $CHECK_ONLY; then
            info "Comparing dist/hew.tmLanguage.json with vscode-hew/syntaxes/..."
            if cmp -s "$DIST_DIR/hew.tmLanguage.json" "$VSCODE_HEW/syntaxes/hew.tmLanguage.json" 2>/dev/null; then
                ok "Already in sync"
            else
                fail "Drift detected in syntaxes/hew.tmLanguage.json"
                DRIFTS=$((DRIFTS + 1))
            fi
        else
            cp "$DIST_DIR/hew.tmLanguage.json" "$VSCODE_HEW/syntaxes/hew.tmLanguage.json"
            cd "$VSCODE_HEW"
            if git diff --quiet syntaxes/hew.tmLanguage.json 2>/dev/null; then
                ok "Already up to date"
            else
                ok "Updated syntaxes/hew.tmLanguage.json"
                UPDATED=$((UPDATED + 1))
                if $COMMIT; then
                    try_commit "$VSCODE_HEW" "Sync TextMate grammar with compiler syntax data" \
                        syntaxes/hew.tmLanguage.json
                fi
            fi
        fi
    else
        warn "dist/hew.tmLanguage.json not available (skipped)"
        SKIPPED=$((SKIPPED + 1))
    fi
else
    warn "Not found at $VSCODE_HEW (skipped)"
    SKIPPED=$((SKIPPED + 1))
fi

# 3b. hew.sh — copy dist/hew.tmLanguage.json → public/syntax/
next_step "hew.sh (TextMate grammar)..."
if [ -d "$HEW_SH" ]; then
    if [ -f "$DIST_DIR/hew.tmLanguage.json" ]; then
        # Determine the correct destination path
        local_tm_dest=""
        for candidate in "public/syntax" "src/lib/syntax"; do
            if [ -d "$HEW_SH/$candidate" ]; then
                local_tm_dest="$candidate/hew.tmLanguage.json"
                break
            fi
        done
        if [ -z "$local_tm_dest" ]; then
            # Default to src/lib/syntax
            local_tm_dest="src/lib/syntax/hew.tmLanguage.json"
            mkdir -p "$HEW_SH/src/lib/syntax"
        fi

        if $CHECK_ONLY; then
            info "Comparing dist/hew.tmLanguage.json with hew.sh/$local_tm_dest..."
            if cmp -s "$DIST_DIR/hew.tmLanguage.json" "$HEW_SH/$local_tm_dest" 2>/dev/null; then
                ok "Already in sync"
            else
                fail "Drift detected in $local_tm_dest"
                DRIFTS=$((DRIFTS + 1))
            fi
        else
            cp "$DIST_DIR/hew.tmLanguage.json" "$HEW_SH/$local_tm_dest"
            cd "$HEW_SH"
            if git diff --quiet "$local_tm_dest" 2>/dev/null; then
                ok "Already up to date"
            else
                ok "Updated $local_tm_dest"
                UPDATED=$((UPDATED + 1))
                if $COMMIT; then
                    try_commit "$HEW_SH" "Sync TextMate grammar with compiler syntax data" \
                        "$local_tm_dest"
                fi
            fi
        fi
    else
        warn "dist/hew.tmLanguage.json not available (skipped)"
        SKIPPED=$((SKIPPED + 1))
    fi
else
    warn "Not found at $HEW_SH (skipped)"
    SKIPPED=$((SKIPPED + 1))
fi

# 3c. hew.run — copy dist/hew.tmLanguage.json → static/syntax/
next_step "hew.run (TextMate grammar)..."
if [ -d "$HEW_RUN" ]; then
    if [ -f "$DIST_DIR/hew.tmLanguage.json" ]; then
        # Determine the correct destination path
        local_tm_dest=""
        for candidate in "static/syntax" "src/lib/syntax"; do
            if [ -d "$HEW_RUN/$candidate" ]; then
                local_tm_dest="$candidate/hew.tmLanguage.json"
                break
            fi
        done
        if [ -z "$local_tm_dest" ]; then
            local_tm_dest="static/syntax/hew.tmLanguage.json"
            mkdir -p "$HEW_RUN/static/syntax"
        fi

        if $CHECK_ONLY; then
            info "Comparing dist/hew.tmLanguage.json with hew.run/$local_tm_dest..."
            if cmp -s "$DIST_DIR/hew.tmLanguage.json" "$HEW_RUN/$local_tm_dest" 2>/dev/null; then
                ok "Already in sync"
            else
                fail "Drift detected in $local_tm_dest"
                DRIFTS=$((DRIFTS + 1))
            fi
        else
            cp "$DIST_DIR/hew.tmLanguage.json" "$HEW_RUN/$local_tm_dest"
            cd "$HEW_RUN"
            if git diff --quiet "$local_tm_dest" 2>/dev/null; then
                ok "Already up to date"
            else
                ok "Updated $local_tm_dest"
                UPDATED=$((UPDATED + 1))
                if $COMMIT; then
                    try_commit "$HEW_RUN" "Sync TextMate grammar with compiler syntax data" \
                        "$local_tm_dest"
                fi
            fi
        fi
    else
        warn "dist/hew.tmLanguage.json not available (skipped)"
        SKIPPED=$((SKIPPED + 1))
    fi
else
    warn "Not found at $HEW_RUN (skipped)"
    SKIPPED=$((SKIPPED + 1))
fi

# 3d. hew-studio — copy dist/hew.tmLanguage.json → src/lib/syntax/
next_step "hew-studio (TextMate grammar)..."
if [ -d "$HEW_STUDIO" ]; then
    if [ -f "$DIST_DIR/hew.tmLanguage.json" ]; then
        local_tm_dest="src/lib/syntax/hew.tmLanguage.json"
        mkdir -p "$HEW_STUDIO/src/lib/syntax"

        if $CHECK_ONLY; then
            info "Comparing dist/hew.tmLanguage.json with hew-studio/$local_tm_dest..."
            if cmp -s "$DIST_DIR/hew.tmLanguage.json" "$HEW_STUDIO/$local_tm_dest" 2>/dev/null; then
                ok "Already in sync"
            else
                fail "Drift detected in $local_tm_dest"
                DRIFTS=$((DRIFTS + 1))
            fi
        else
            cp "$DIST_DIR/hew.tmLanguage.json" "$HEW_STUDIO/$local_tm_dest"
            cd "$HEW_STUDIO"
            if git diff --quiet "$local_tm_dest" 2>/dev/null; then
                ok "Already up to date"
            else
                ok "Updated $local_tm_dest"
                UPDATED=$((UPDATED + 1))
                if $COMMIT; then
                    try_commit "$HEW_STUDIO" "Sync TextMate grammar with compiler syntax data" \
                        "$local_tm_dest"
                fi
            fi
        fi
    else
        warn "dist/hew.tmLanguage.json not available (skipped)"
        SKIPPED=$((SKIPPED + 1))
    fi
else
    warn "Not found at $HEW_STUDIO (skipped)"
    SKIPPED=$((SKIPPED + 1))
fi

# 3e. tree-sitter-hew — patch grammar.js keyword blocks
next_step "tree-sitter-hew (grammar.js keyword sync)..."
if [ -d "$TREE_SITTER" ] && [ -f "$TREE_SITTER/grammar.js" ]; then
    if $CHECK_ONLY; then
        # Run the patcher against a temp copy
        info "Checking grammar.js for drift..."
        tmp_grammar="$(mktemp "${TMPDIR:-/tmp}/grammar-XXXXXX.js")"
        cp "$TREE_SITTER/grammar.js" "$tmp_grammar"
        if HEW_TREE_SITTER="$TREE_SITTER" node "$TOOLS_DIR/patch-tree-sitter-keywords.mjs" "$tmp_grammar" 2>&1 | tail -3; then
            if cmp -s "$tmp_grammar" "$TREE_SITTER/grammar.js"; then
                ok "Already in sync"
            else
                fail "Drift detected in grammar.js"
                DRIFTS=$((DRIFTS + 1))
            fi
        else
            fail "Patch script failed!"
            ERRORS=$((ERRORS + 1))
        fi
        rm -f "$tmp_grammar"
    else
        info "Patching grammar.js..."
        if HEW_TREE_SITTER="$TREE_SITTER" node "$TOOLS_DIR/patch-tree-sitter-keywords.mjs" 2>&1 | tail -3; then
            cd "$TREE_SITTER"
            if git diff --quiet grammar.js 2>/dev/null; then
                ok "Already up to date"
            else
                ok "Patched grammar.js"
                UPDATED=$((UPDATED + 1))

                # Regenerate the parser
                info "Running tree-sitter generate..."
                if (cd "$TREE_SITTER" && npx tree-sitter generate 2>&1 | tail -3); then
                    ok "Parser regenerated"
                else
                    warn "tree-sitter generate failed (manual regeneration needed)"
                fi

                if $COMMIT; then
                    try_commit "$TREE_SITTER" "Sync grammar keywords with compiler syntax data" \
                        grammar.js src/
                fi
            fi
        else
            fail "Patch script failed!"
            ERRORS=$((ERRORS + 1))
        fi
    fi
else
    warn "Not found at $TREE_SITTER (skipped)"
    SKIPPED=$((SKIPPED + 1))
fi

# 3f. vim-hew — patch syntax/hew.vim keyword blocks
next_step "vim-hew (hew.vim keyword sync)..."
if [ -d "$VIM_HEW" ] && [ -f "$VIM_HEW/syntax/hew.vim" ]; then
    if $CHECK_ONLY; then
        info "Checking hew.vim for drift..."
        tmp_vim="$(mktemp "${TMPDIR:-/tmp}/hew-XXXXXX.vim")"
        cp "$VIM_HEW/syntax/hew.vim" "$tmp_vim"
        if HEW_VIM_HEW="$VIM_HEW" node "$TOOLS_DIR/patch-vim-syntax.mjs" "$tmp_vim" 2>&1 | tail -3; then
            if cmp -s "$tmp_vim" "$VIM_HEW/syntax/hew.vim"; then
                ok "Already in sync"
            else
                fail "Drift detected in syntax/hew.vim"
                DRIFTS=$((DRIFTS + 1))
            fi
        else
            fail "Patch script failed!"
            ERRORS=$((ERRORS + 1))
        fi
        rm -f "$tmp_vim"
    else
        info "Patching syntax/hew.vim..."
        if HEW_VIM_HEW="$VIM_HEW" node "$TOOLS_DIR/patch-vim-syntax.mjs" 2>&1 | tail -3; then
            cd "$VIM_HEW"
            if git diff --quiet syntax/hew.vim 2>/dev/null; then
                ok "Already up to date"
            else
                ok "Patched syntax/hew.vim"
                UPDATED=$((UPDATED + 1))
                if $COMMIT; then
                    try_commit "$VIM_HEW" "Sync Vim syntax with compiler syntax data" \
                        syntax/hew.vim
                fi
            fi
        else
            fail "Patch script failed!"
            ERRORS=$((ERRORS + 1))
        fi
    fi
else
    warn "Not found at $VIM_HEW (skipped)"
    SKIPPED=$((SKIPPED + 1))
fi

# 3g. editors/ — copy dist/hew.nanorc → editors/nano/hew.nanorc
next_step "editors/nano (nanorc)..."
if [ -f "$DIST_DIR/hew.nanorc" ]; then
    if $CHECK_ONLY; then
        info "Comparing dist/hew.nanorc with editors/nano/hew.nanorc..."
        if cmp -s "$DIST_DIR/hew.nanorc" "$REPO_ROOT/editors/nano/hew.nanorc" 2>/dev/null; then
            ok "Already in sync"
        else
            fail "Drift detected in editors/nano/hew.nanorc"
            DRIFTS=$((DRIFTS + 1))
        fi
    else
        cp "$DIST_DIR/hew.nanorc" "$REPO_ROOT/editors/nano/hew.nanorc"
        cd "$REPO_ROOT"
        if git diff --quiet editors/nano/hew.nanorc 2>/dev/null; then
            ok "Already up to date"
        else
            ok "Updated editors/nano/hew.nanorc"
            UPDATED=$((UPDATED + 1))
        fi
    fi
else
    warn "dist/hew.nanorc not available (skipped)"
    SKIPPED=$((SKIPPED + 1))
fi

# ── Summary ───────────────────────────────────────────────────────────
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ $ERRORS -gt 0 ]; then
    fail "$ERRORS error(s) occurred"
fi

if [ $SKIPPED -gt 0 ]; then
    warn "$SKIPPED target(s) skipped (repo not found or artefact missing)"
fi

if $CHECK_ONLY; then
    if [ $DRIFTS -gt 0 ]; then
        fail "$DRIFTS downstream target(s) drifted from regenerated output"
    else
        ok "All checked downstream targets match regenerated output"
    fi
elif [ $UPDATED -gt 0 ]; then
    ok "$UPDATED target(s) updated"
    if ! $COMMIT; then
        info "Run with --commit to auto-commit changes"
    fi
else
    ok "All downstream targets already in sync"
fi

echo ""

# Exit with error if anything failed
if [ $ERRORS -gt 0 ]; then
    exit 1
elif $CHECK_ONLY && [ $DRIFTS -gt 0 ]; then
    exit 1
fi
