#!/usr/bin/env bash
# Verify that every public stdlib index row has an executable API proof.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INDEX="${ROOT}/std/README.md"
MANIFEST="${ROOT}/scripts/stdlib-execution-proofs.tsv"
RUNNER="${ROOT}/tests/vertical-slice/run.sh"

runner_fixture_declarations() {
    awk '
        function emit_first_argument(line, value) {
            sub(/^[[:space:]]*/, "", line)
            if (match(line, /^"[^"]+"/)) {
                value = substr(line, RSTART + 1, RLENGTH - 2)
                print value
                return 1
            }
            if (match(line, /^[[:alnum:]_-]+/)) {
                print substr(line, RSTART, RLENGTH)
                return 1
            }
            return 0
        }

        awaiting_fixture {
            if (/^[[:space:]]*$/ || /^[[:space:]]*#/) {
                exit 2
            }
            if (!emit_first_argument($0)) {
                exit 2
            }
            awaiting_fixture = 0
            next
        }

        /^(run_accept_expect_(status|stdout|stdout_contains|trap|status_and_stdout|panic)|run_actor_bounds_trap_fixture)([[:space:]]|$)/ {
            line = $0
            sub(/^[[:space:]]*[^[:space:]]+[[:space:]]*/, "", line)
            if (!emit_first_argument(line)) {
                if (line != "\\") {
                    exit 2
                }
                awaiting_fixture = 1
            }
        }

        /^[[:space:]]*#/ { next }

        END {
            if (awaiting_fixture) {
                exit 2
            }
        }
    ' "$1"
}

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
runner_fixtures="${tmpdir}/runner-fixtures"
touch "${mapped}"

# Mutation teeth for the runner parser. Generated fixtures run under Bash as
# well as through the parser so declaration evidence cannot diverge from what
# the product runner actually executes.
parser_fixture="${tmpdir}/runner-parser-fixture.sh"
parser_expected="${tmpdir}/runner-parser-expected"
parser_actual="${tmpdir}/runner-parser-actual"
parser_executed="${tmpdir}/runner-parser-executed"

parser_fixture_prelude() {
    printf '%s\n' \
        '#!/usr/bin/env bash' \
        'set -euo pipefail' \
        'record_fixture() {' \
        '    if [[ "$#" -lt 1 ]]; then' \
        '        return 97' \
        '    fi' \
        "    printf '%s\\n' \"\$1\" >> \"\${PARSER_EXECUTED:?}\"" \
        '}' \
        'run_accept_expect_stdout() { record_fixture "$@"; }' \
        'run_accept_expect_stdout_contains() { record_fixture "$@"; }'
}

assert_parser_fixture_rejected() {
    local fixture="$1"
    local label="$2"
    if runner_fixture_declarations "${fixture}" > "${tmpdir}/${label}-parsed"; then
        echo "internal error: declaration parser accepted ${label}" >&2
        exit 1
    fi
    if PARSER_EXECUTED="${tmpdir}/${label}-executed" bash "${fixture}" >/dev/null 2>&1; then
        echo "internal error: Bash executed invalid ${label}" >&2
        exit 1
    fi
}

parser_fixture_prelude > "${parser_fixture}"
printf '%s\n' \
    '# run_accept_expect_stdout "comment_only"' \
    'helper_body_decoy() {' \
    '    run_accept_expect_stdout "helper_body_literal"' \
    '}' \
    'run_accept_expect_stdout "inline_fixture"' \
    "run_accept_expect_stdout_contains \\" \
    "    \"continued_fixture\" \\" \
    '    "stable output"' \
    '    # run_accept_expect_status "indented_comment_only" 0' \
    >> "${parser_fixture}"
printf '%s\n' "inline_fixture" "continued_fixture" > "${parser_expected}"
runner_fixture_declarations "${parser_fixture}" > "${parser_actual}"
if ! diff -u "${parser_expected}" "${parser_actual}"; then
    echo "internal error: vertical-slice declaration parser accepted a comment or lost a declaration" >&2
    exit 1
fi
PARSER_EXECUTED="${parser_executed}" bash "${parser_fixture}"
if ! diff -u "${parser_expected}" "${parser_executed}"; then
    echo "internal error: generated parser fixture did not execute as declared" >&2
    exit 1
fi

parser_comment_interruption="${tmpdir}/runner-parser-comment-interruption.sh"
parser_fixture_prelude > "${parser_comment_interruption}"
printf '%s\n' \
    "run_accept_expect_stdout_contains \\" \
    '    # a comment terminates the continued helper command' \
    "    \"detached_fixture\" \\" \
    '    "stable output"' \
    >> "${parser_comment_interruption}"
assert_parser_fixture_rejected "${parser_comment_interruption}" "comment interruption"

parser_blank_interruption="${tmpdir}/runner-parser-blank-interruption.sh"
parser_fixture_prelude > "${parser_blank_interruption}"
printf '%s\n' \
    "run_accept_expect_stdout_contains \\" \
    '' \
    "    \"detached_fixture\" \\" \
    '    "stable output"' \
    >> "${parser_blank_interruption}"
assert_parser_fixture_rejected "${parser_blank_interruption}" "blank interruption"

parser_eof_interruption="${tmpdir}/runner-parser-eof-interruption.sh"
parser_fixture_prelude > "${parser_eof_interruption}"
printf '%s\n' "run_accept_expect_stdout_contains \\" >> "${parser_eof_interruption}"
assert_parser_fixture_rejected "${parser_eof_interruption}" "EOF interruption"

parser_missing_continuation="${tmpdir}/runner-parser-missing-continuation.sh"
parser_fixture_prelude > "${parser_missing_continuation}"
printf '%s\n' \
    'run_accept_expect_stdout_contains' \
    "    \"detached_fixture\" \\" \
    '    "stable output"' \
    >> "${parser_missing_continuation}"
assert_parser_fixture_rejected "${parser_missing_continuation}" "missing continuation"

runner_fixture_declarations "${RUNNER}" | sort -u > "${runner_fixtures}"
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
            grep -qxF "${name}" "${runner_fixtures}" || {
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
