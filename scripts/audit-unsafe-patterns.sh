#!/usr/bin/env bash
# Reject new high-risk runtime unsafe operations unless their nearby SAFETY
# rationale explicitly covers provenance, bounds, and the type tag.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

if [[ -n "${UNSAFE_AUDIT_BASE:-}" ]]; then
  base="$UNSAFE_AUDIT_BASE"
elif [[ "${GITHUB_EVENT_NAME:-}" == "pull_request" && -n "${GITHUB_BASE_REF:-}" ]]; then
  base=$(git merge-base "origin/$GITHUB_BASE_REF" HEAD)
else
  base=$(git rev-parse HEAD^ 2>/dev/null || true)
fi

# A root commit has no newly introduced code to compare. CI always has a base.
[[ -n "$base" ]] || exit 0

fail=0
while IFS=$'\t' read -r file line source; do
  [[ -n "$file" ]] || continue
  if [[ ! "$source" =~ transmute|from_raw_parts|as[[:space:]]+\*mut[[:space:]]+c_void ]]; then
    continue
  fi
  first=$((line > 5 ? line - 5 : 1))
  last=$((line + 5))
  rationale=$(sed -n "${first},${last}p" "$file")
  if ! grep -qi 'SAFETY:' <<<"$rationale" \
    || ! grep -qi 'provenance' <<<"$rationale" \
    || ! grep -qi 'bounds' <<<"$rationale" \
    || ! grep -Eqi 'type[[:space:]-]*tag' <<<"$rationale"; then
    echo "$file:$line: new high-risk unsafe operation needs a nearby SAFETY: comment naming Provenance, Bounds, and Type-tag" >&2
    fail=1
  fi
done < <(
  git diff --no-ext-diff --unified=0 "$base"...HEAD -- hew-runtime/src \
    | awk '
      /^\+\+\+ b\// { file=substr($0, 7); next }
      /^@@ / {
        added=$0
        sub(/^.*\+/, "", added)
        sub(/ .*/, "", added)
        split(added, range, ",")
        line=range[1]
        next
      }
      /^\+/ && !/^\+\+\+/ { print file "\t" line "\t" substr($0, 2); line++ }
    '
)

[[ "$fail" -eq 0 ]] || exit 1
echo "audit-unsafe-patterns: clean"
