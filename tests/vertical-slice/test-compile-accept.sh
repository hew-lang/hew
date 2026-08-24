#!/usr/bin/env bash
# Counterfactuals for compile-accept.sh's Bash-3.2-safe argument selection and
# fail-closed compiler status propagation.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HELPER="${ROOT}/tests/vertical-slice/compile-accept.sh"
RUNNER="${ROOT}/tests/vertical-slice/run.sh"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/hew-vertical-compile-selftest.XXXXXX")"
trap 'rm -rf "${tmpdir}"' EXIT

fake_root="${tmpdir}/repo"
fixture_dir="${fake_root}/tests/vertical-slice/accept"
mkdir -p "${fixture_dir}"
touch \
  "${fixture_dir}/ordinary.hew" \
  "${fixture_dir}/link_monitor_value_monitor_in_actor.hew"

fake_hew="${tmpdir}/fake-hew.sh"
cat >"${fake_hew}" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" >"${FAKE_HEW_ARGS}"
echo "fake compiler diagnostic"
exit "${FAKE_HEW_STATUS:-0}"
EOF
chmod +x "${fake_hew}"

args="${tmpdir}/args.txt"
output="${tmpdir}/output.txt"

# An ordinary fixture must execute without an empty-array nounset abort and
# must not request a textual LLVM sidecar.
FAKE_HEW_ARGS="${args}" FAKE_HEW_STATUS=0 \
  "${HELPER}" "${fake_hew}" "${fake_root}" ordinary "${output}"
printf 'compile\n%s\n' "${fixture_dir}/ordinary.hew" >"${tmpdir}/expected.txt"
diff -u "${tmpdir}/expected.txt" "${args}"

# Only the two real sidecar consumers request --emit-llvm.
FAKE_HEW_ARGS="${args}" FAKE_HEW_STATUS=0 \
  "${HELPER}" "${fake_hew}" "${fake_root}" \
  link_monitor_value_monitor_in_actor "${output}"
printf 'compile\n--emit-llvm\n%s\n' \
  "${fixture_dir}/link_monitor_value_monitor_in_actor.hew" \
  >"${tmpdir}/expected.txt"
diff -u "${tmpdir}/expected.txt" "${args}"

# A compiler/fixture failure must make the helper nonzero and retain both the
# original status and diagnostic in the runner-facing error.
if FAKE_HEW_ARGS="${args}" FAKE_HEW_STATUS=42 \
  "${HELPER}" "${fake_hew}" "${fake_root}" ordinary "${output}" \
  >"${tmpdir}/failure.stdout" 2>"${tmpdir}/failure.stderr"; then
  echo "forced compiler failure unexpectedly passed" >&2
  exit 1
fi
grep -qF 'got exit 42' "${tmpdir}/failure.stderr"
grep -qF 'fake compiler diagnostic' "${tmpdir}/failure.stderr"

# The full runner turns any helper failure into an unconditional shell exit;
# it cannot be swallowed by an outer `if` disabling `errexit` in the function.
# shellcheck disable=SC2016  # Match the literal expansion syntax in run.sh.
grep -qF '"${HEW}" "${ROOT}" "${fixture}" "${accept_output}" || exit 1' \
  "${RUNNER}"

echo "vertical-slice compile runner counterfactuals: PASS"
