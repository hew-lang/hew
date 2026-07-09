#!/usr/bin/env bash
# End-to-end package-manager consumer proof: local publish setup -> user-facing
# `hew package install` -> lock/materialization -> compiler check/run.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW="${ROOT}/target/debug/hew"
ADZE="${ROOT}/target/debug/adze"

fail() {
  echo "FAIL package-install: $*" >&2
  exit 1
}

require_binary() {
  local bin="$1"
  if [[ ! -x "${bin}" ]]; then
    fail "missing executable ${bin}; run the Makefile target so build prerequisites are present"
  fi
}

run_in() {
  local dir="$1"
  local name="$2"
  shift 2
  local log="${TMP}/logs/${name}.log"
  if ! (cd "${dir}" && "$@") >"${log}" 2>&1; then
    cat "${log}" >&2
    fail "${name} exited non-zero"
  fi
}

assert_contains() {
  local path="$1"
  local needle="$2"
  if ! grep -Fq "${needle}" "${path}"; then
    echo "Expected ${path} to contain: ${needle}" >&2
    echo "--- ${path} ---" >&2
    cat "${path}" >&2
    fail "missing expected file content"
  fi
}

assert_output() {
  local consumer="$1"
  local expected="$2"
  local name="$3"
  local actual="${TMP}/logs/${name}.out"
  local stderr="${TMP}/logs/${name}.err"
  if ! (cd "${consumer}" && "${HEW}" run main.hew) >"${actual}" 2>"${stderr}"; then
    cat "${stderr}" >&2
    fail "${name}: hew run exited non-zero"
  fi
  printf '%s\n' "${expected}" >"${TMP}/logs/${name}.expected"
  if ! diff -u "${TMP}/logs/${name}.expected" "${actual}"; then
    fail "${name}: stdout mismatch"
  fi
}

last_segment() {
  local package="$1"
  printf '%s\n' "${package##*::}"
}

write_package() {
  local dir="$1"
  local package="$2"
  local version="$3"
  local value="$4"
  local entry
  entry="$(last_segment "${package}")"
  mkdir -p "${dir}"
  cat >"${dir}/hew.toml" <<EOF_MANIFEST
[package]
name = "${package}"
version = "${version}"
edition = "2026"
description = "package-install e2e fixture"
authors = ["Hew Contributors"]
license = "MIT OR Apache-2.0"
EOF_MANIFEST
  cat >"${dir}/${entry}.hew" <<EOF_SOURCE
pub fn answer() -> i64 {
    ${value}
}
EOF_SOURCE
}

write_consumer() {
  local dir="$1"
  local package="$2"
  local version="$3"
  local module
  module="$(last_segment "${package}")"
  mkdir -p "${dir}"
  cat >"${dir}/hew.toml" <<EOF_MANIFEST
[package]
name = "consumer"
version = "0.1.0"
edition = "2026"

[dependencies]
"${package}" = "${version}"
EOF_MANIFEST
  cat >"${dir}/main.hew" <<EOF_SOURCE
import ${package};

fn main() {
    println(f"{${module}.answer()}");
}
EOF_SOURCE
}

expect_check_failure() {
  local dir="$1"
  local name="$2"
  local needle="$3"
  local output
  if output="$(cd "${dir}" && "${HEW}" check main.hew 2>&1)"; then
    echo "${output}" >&2
    fail "${name}: hew check unexpectedly succeeded"
  fi
  if ! grep -Fq "${needle}" <<<"${output}"; then
    echo "${output}" >&2
    fail "${name}: missing expected diagnostic: ${needle}"
  fi
}

expect_install_failure() {
  local dir="$1"
  local name="$2"
  local needle="$3"
  local output
  if output="$(cd "${dir}" && "${HEW}" package install 2>&1)"; then
    echo "${output}" >&2
    fail "${name}: hew package install unexpectedly succeeded"
  fi
  if ! grep -Fq "${needle}" <<<"${output}"; then
    echo "${output}" >&2
    fail "${name}: missing expected install diagnostic: ${needle}"
  fi
}

require_binary "${HEW}"
require_binary "${ADZE}"

TMP="$(mktemp -d "${TMPDIR:-/tmp}/hew-package-install.XXXXXX")"
trap 'rm -rf "${TMP}"' EXIT
mkdir -p "${TMP}/logs"

export HOME="${TMP}/home"
mkdir -p "${HOME}"

run_in "${TMP}" keygen "${ADZE}" key generate

adder_pkg="${TMP}/pkgs/adder-0.1.0"
write_package "${adder_pkg}" "acme::adder" "0.1.0" "42"
run_in "${adder_pkg}" publish-adder "${ADZE}" publish

consumer="${TMP}/consumer"
write_consumer "${consumer}" "acme::adder" "0.1.0"
run_in "${consumer}" install-adder "${HEW}" package install
test -f "${consumer}/adze.lock" || fail "adze.lock was not written"
assert_contains "${consumer}/adze.lock" 'name = "acme::adder"'
assert_contains "${consumer}/adze.lock" 'version = "0.1.0"'
test -f "${consumer}/.adze/packages/acme/adder/hew.toml" \
  || fail "project-local package manifest was not materialized"
test -f "${consumer}/.adze/packages/acme/adder/adder.hew" \
  || fail "project-local package entry source was not materialized"
run_in "${consumer}" check-adder "${HEW}" check main.hew
assert_output "${consumer}" "42" "run-adder"
echo "PASS package-install minimal consumer"

missing_consumer="${TMP}/missing-consumer"
write_consumer "${missing_consumer}" "acme::missing" "0.1.0"
expect_check_failure "${missing_consumer}" "missing package" "run \`adze install\`"
echo "PASS package-install missing rejection"

traversal_consumer="${TMP}/traversal-consumer"
mkdir -p "${traversal_consumer}"
cat >"${traversal_consumer}/hew.toml" <<EOF_MANIFEST
[package]
name = "traversal_consumer"
version = "0.1.0"
edition = "2026"

[dependencies]
"evil::../../../tmp/pwned" = "1.0.0"
EOF_MANIFEST
expect_install_failure \
  "${traversal_consumer}" \
  "traversal package name" \
  "invalid package name \`evil::../../../tmp/pwned\`"
echo "PASS package-install traversal dependency rejection"

broken_registry="${HOME}/.adze/packages/acme/broken/0.1.0"
mkdir -p "${broken_registry}"
printf 'not valid {{{\n' >"${broken_registry}/hew.toml"
broken_consumer="${TMP}/broken-consumer"
write_consumer "${broken_consumer}" "acme::broken" "0.1.0"
expect_install_failure \
  "${broken_consumer}" \
  "malformed package" \
  "cannot read installed manifest for \`acme::broken@0.1.0\`"
echo "PASS package-install malformed rejection"

versioned_v1="${TMP}/pkgs/versioned-0.1.0"
versioned_v2="${TMP}/pkgs/versioned-0.2.0"
write_package "${versioned_v1}" "acme::versioned" "0.1.0" "101"
write_package "${versioned_v2}" "acme::versioned" "0.2.0" "202"
run_in "${versioned_v1}" publish-versioned-v1 "${ADZE}" publish
run_in "${versioned_v2}" publish-versioned-v2 "${ADZE}" publish

versioned_consumer="${TMP}/versioned-consumer"
write_consumer "${versioned_consumer}" "acme::versioned" "0.1.0"
run_in "${versioned_consumer}" install-versioned-v1 "${HEW}" package install
assert_contains "${versioned_consumer}/adze.lock" 'name = "acme::versioned"'
assert_contains "${versioned_consumer}/adze.lock" 'version = "0.1.0"'
run_in "${versioned_consumer}" check-versioned-v1 "${HEW}" check main.hew
assert_output "${versioned_consumer}" "101" "run-versioned-v1"

write_consumer "${versioned_consumer}" "acme::versioned" "0.2.0"
run_in "${versioned_consumer}" install-versioned-v2 "${HEW}" package install
assert_contains "${versioned_consumer}/adze.lock" 'name = "acme::versioned"'
assert_contains "${versioned_consumer}/adze.lock" 'version = "0.2.0"'
run_in "${versioned_consumer}" check-versioned-v2 "${HEW}" check main.hew
assert_output "${versioned_consumer}" "202" "run-versioned-v2"

stale_target="${HOME}/.adze/packages/acme/versioned/0.1.0"
stale_link="${versioned_consumer}/.adze/packages/acme/versioned"
rm -rf "${stale_link}"
mkdir -p "$(dirname "${stale_link}")"
if ! ln -s "${stale_target}" "${stale_link}" 2>/dev/null; then
  cp -R "${stale_target}" "${stale_link}"
fi
expect_check_failure \
  "${versioned_consumer}" \
  "stale package materialization" \
  'does not match adze.lock (expected acme::versioned@0.2.0'
echo "PASS package-install version matrix"
