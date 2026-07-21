#!/usr/bin/env bash
# Verify that every public stdlib index row has an executable API proof.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INDEX="${ROOT}/std/README.md"
MANIFEST="${ROOT}/scripts/stdlib-execution-proofs.tsv"
RUNNER="${ROOT}/tests/vertical-slice/run.sh"

if [[ "${1:-}" != "--check" || $# -ne 1 ]]; then
    echo "usage: scripts/stdlib-execution-proof.sh --check" >&2
    exit 2
fi

[[ -f "${INDEX}" ]] || { echo "missing stdlib index: ${INDEX}" >&2; exit 1; }
[[ -f "${MANIFEST}" ]] || { echo "missing proof manifest: ${MANIFEST}" >&2; exit 1; }

tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/stdlib-execution-proof.XXXXXX")"
trap 'rm -rf "${tmpdir}"' EXIT
indexed="${tmpdir}/indexed"
mapped="${tmpdir}/mapped"
touch "${mapped}"
awk -F'|' '
    /^\| \[`/ {
        module = $3
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", module)
        gsub(/`/, "", module)
        if (module == "_(auto-imported)_") {
            print "std::builtins"
        } else {
            print module
        }
    }
' "${INDEX}" | sort -u > "${indexed}"

failures=0
while IFS=$'\t' read -r module fixture command exercise; do
    [[ -z "${module}" || "${module}" == \#* ]] && continue
    if [[ -z "${fixture}" || -z "${command}" || -z "${exercise}" ]]; then
        echo "invalid proof entry for ${module}" >&2
        failures=1
        continue
    fi
    if grep -qxF "${module}" "${mapped}"; then
        echo "duplicate proof entry for ${module}" >&2
        failures=1
    fi
    printf '%s\n' "${module}" >> "${mapped}"
    if ! grep -qxF "${module}" "${indexed}"; then
        echo "proof entry is not a public index module: ${module}" >&2
        failures=1
    fi
    if [[ ! -f "${ROOT}/${fixture}" ]]; then
        echo "missing proof fixture for ${module}: ${fixture}" >&2
        failures=1
        continue
    fi
    if [[ "${module}" != "std::builtins" ]] \
        && ! grep -qF "import ${module}" "${ROOT}/${fixture}"; then
        echo "proof fixture does not import ${module}: ${fixture}" >&2
        failures=1
    fi
    case "${command}" in
        "make test-hew-ratchet")
            [[ "${fixture}" == tests/hew/* ]] || {
                echo "${module} must use a Hew test fixture" >&2
                failures=1
            }
            ;;
        "make test-vertical-slice")
            [[ "${fixture}" == tests/vertical-slice/accept/* ]] || {
                echo "${module} must use a vertical-slice fixture" >&2
                failures=1
            }
            name="$(basename "${fixture}" .hew)"
            grep -qF "${name}" "${RUNNER}" || {
                echo "vertical-slice runner does not execute ${fixture}" >&2
                failures=1
            }
            ;;
        *)
            echo "non-executing proof command for ${module}: ${command}" >&2
            failures=1
            ;;
    esac
    body="$(sed -E '/^[[:space:]]*(\/\/|\/\*|\*|import )/d' "${ROOT}/${fixture}")"
    if ! grep -qF "${exercise}" <<<"${body}"; then
        echo "proof fixture only imports or does not exercise ${module}: ${fixture}" >&2
        failures=1
    fi
done < "${MANIFEST}"

while IFS= read -r module; do
    if ! grep -qxF "${module}" "${mapped}"; then
        echo "missing executable proof for public module: ${module}" >&2
        failures=1
    fi
done < "${indexed}"

if [[ "${failures}" -ne 0 ]]; then
    exit 1
fi

count="$(wc -l < "${mapped}" | tr -d ' ')"
echo "stdlib execution proofs: ${count} public modules mapped to executable fixtures"
