#!/usr/bin/env bash
# sync-syntax-downstream.sh — Propagate syntax changes to downstream repos.
#
# Validates docs/syntax-data.json against the lexer, then runs the
# appropriate generator in each downstream repo that has a local clone.
#
# Usage:
#   ./scripts/sync-syntax-downstream.sh           # validate + generate
#   ./scripts/sync-syntax-downstream.sh --commit  # also git commit each repo
#   ./scripts/sync-syntax-downstream.sh --check   # dry-run: report drift only

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SYNTAX_DATA="$REPO_ROOT/docs/syntax-data.json"

# Downstream repos (sibling directories by convention)
PARENT="$(dirname "$REPO_ROOT")"
VSCODE_HEW="$PARENT/vscode-hew"
HEW_SH="$PARENT/hew.sh"
HEW_RUN="$PARENT/hew.run"
TREE_SITTER="$PARENT/tree-sitter-hew"

# Parse flags
COMMIT=false
CHECK_ONLY=false
for arg in "$@"; do
    case "$arg" in
    --commit) COMMIT=true ;;
    --check) CHECK_ONLY=true ;;
    --help | -h)
        echo "Usage: $0 [--check] [--commit]"
        echo "  --check   Dry-run: validate syntax-data.json and report drift only"
        echo "  --commit  Commit changes in each downstream repo after generation"
        exit 0
        ;;
    esac
done

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

ok() { echo -e "  ${GREEN}✓${NC} $1"; }
warn() { echo -e "  ${YELLOW}⚠${NC} $1"; }
fail() { echo -e "  ${RED}✗${NC} $1"; }
info() { echo -e "  ${CYAN}→${NC} $1"; }

ERRORS=0
UPDATED=0

echo ""
echo "╔══════════════════════════════════════════════╗"
echo "║  Hew Syntax Sync                             ║"
echo "╚══════════════════════════════════════════════╝"
echo ""

# ── Step 1: Validate syntax-data.json against the lexer ─────────────
echo "1. Validating syntax-data.json against lexer..."

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

if $CHECK_ONLY; then
    echo ""
    echo "2. Checking downstream drift..."
fi

# ── Step 2: Update vscode-hew (TextMate grammar) ────────────────────
echo ""
echo "2. vscode-hew (TextMate grammar)..."
if [ -d "$VSCODE_HEW" ] && [ -f "$VSCODE_HEW/tools/generate-tmgrammar.mjs" ]; then
    if $CHECK_ONLY; then
        # Compare current grammar keywords against what generator would produce
        info "Clone found at $VSCODE_HEW"
        cd "$VSCODE_HEW"
        if git diff --quiet syntaxes/hew.tmLanguage.json 2>/dev/null; then
            ok "No local changes (may still be outdated vs syntax-data)"
        else
            warn "Has uncommitted changes"
        fi
    else
        cd "$VSCODE_HEW"
        info "Running generate-tmgrammar.mjs..."
        if node tools/generate-tmgrammar.mjs 2>&1; then
            if git diff --quiet syntaxes/hew.tmLanguage.json 2>/dev/null; then
                ok "Already up to date"
            else
                ok "Updated syntaxes/hew.tmLanguage.json"
                UPDATED=$((UPDATED + 1))
                if $COMMIT; then
                    git add syntaxes/hew.tmLanguage.json
                    git commit -m "Sync TextMate grammar with compiler syntax data" --quiet
                    ok "Committed"
                fi
            fi
        else
            fail "Generator failed!"
            ERRORS=$((ERRORS + 1))
        fi
    fi
else
    warn "Not found at $VSCODE_HEW (skipped)"
fi

# ── Step 3: Update hew.sh (Monarch tokens + TextMate copy) ──────────
echo ""
echo "3. hew.sh (Monarch tokens + TextMate grammar)..."
if [ -d "$HEW_SH" ] && [ -f "$HEW_SH/tools/generate-monarch.mjs" ]; then
    if $CHECK_ONLY; then
        info "Clone found at $HEW_SH"
        cd "$HEW_SH"
        if git diff --quiet src/lib/monaco/hew-language.ts src/lib/syntax/hew.tmLanguage.json 2>/dev/null; then
            ok "No local changes"
        else
            warn "Has uncommitted changes"
        fi
    else
        cd "$HEW_SH"
        info "Running generate-monarch.mjs..."
        if node tools/generate-monarch.mjs 2>&1; then
            ok "Monarch tokens updated"
        else
            fail "Monarch generator failed!"
            ERRORS=$((ERRORS + 1))
        fi

        # Copy TextMate grammar from vscode-hew if available
        if [ -f "$VSCODE_HEW/syntaxes/hew.tmLanguage.json" ]; then
            cp "$VSCODE_HEW/syntaxes/hew.tmLanguage.json" src/lib/syntax/hew.tmLanguage.json
            ok "TextMate grammar copied from vscode-hew"
        fi

        if git diff --quiet src/lib/monaco/hew-language.ts src/lib/syntax/hew.tmLanguage.json 2>/dev/null; then
            ok "Already up to date"
        else
            UPDATED=$((UPDATED + 1))
            if $COMMIT; then
                git add src/lib/monaco/hew-language.ts src/lib/syntax/hew.tmLanguage.json
                git commit -m "Sync syntax highlighting with compiler syntax data" --quiet
                ok "Committed"
            fi
        fi
    fi
else
    warn "Not found at $HEW_SH (skipped)"
fi

# ── Step 4: Update hew.run (Monarch tokens copy) ────────────────────
echo ""
echo "4. hew.run (Monarch tokens)..."
if [ -d "$HEW_RUN" ]; then
    if [ -f "$HEW_SH/src/lib/monaco/hew-language.ts" ]; then
        if $CHECK_ONLY; then
            info "Clone found at $HEW_RUN"
            cd "$HEW_RUN"
            if diff -q "$HEW_SH/src/lib/monaco/hew-language.ts" src/lib/monaco/hew-language.ts >/dev/null 2>&1; then
                ok "In sync with hew.sh"
            else
                warn "Differs from hew.sh"
            fi
        else
            cp "$HEW_SH/src/lib/monaco/hew-language.ts" "$HEW_RUN/src/lib/monaco/hew-language.ts"
            cd "$HEW_RUN"
            if git diff --quiet src/lib/monaco/hew-language.ts 2>/dev/null; then
                ok "Already up to date"
            else
                ok "Updated from hew.sh"
                UPDATED=$((UPDATED + 1))
                if $COMMIT; then
                    git add src/lib/monaco/hew-language.ts
                    git commit -m "Sync Monarch tokens with compiler syntax data" --quiet
                    ok "Committed"
                fi
            fi
        fi
    else
        warn "hew.sh Monarch source not found (skipped)"
    fi
else
    warn "Not found at $HEW_RUN (skipped)"
fi

# ── Step 5: Check tree-sitter drift ──────────────────────────────────
echo ""
echo "5. tree-sitter-hew (drift check)..."
if [ -d "$TREE_SITTER" ] && [ -f "$TREE_SITTER/tools/check-drift.sh" ]; then
    info "Running check-drift.sh..."
    cd "$TREE_SITTER"
    if bash tools/check-drift.sh 2>&1 | tail -5; then
        ok "Drift check complete (review output above)"
    fi
else
    warn "Not found at $TREE_SITTER (skipped)"
fi

# ── Summary ──────────────────────────────────────────────────────────
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ $ERRORS -gt 0 ]; then
    fail "$ERRORS error(s) occurred"
elif $CHECK_ONLY; then
    ok "Drift check complete"
elif [ $UPDATED -gt 0 ]; then
    ok "$UPDATED repo(s) updated"
    if ! $COMMIT; then
        info "Run with --commit to auto-commit changes"
    fi
else
    ok "All downstream repos already in sync"
fi
echo ""
