#!/usr/bin/env bash
# Download, checksum-verify, and extract a pinned LLVM release tarball.
#
# The asset is an immutable, version-pinned release artifact, so a fresh
# download after a network failure returns the same bytes as any prior
# attempt. Callers wrap the whole unit (download + verify + extract) in
# retry-download.sh: a transfer that dies mid-flight or an extraction that
# aborts partway both leave the tarball/install_root in a state a second
# attempt cleanly overwrites, so retrying the unit rather than only the
# curl call is correct and keeps checksum verification mandatory on every
# attempt, not just the first.
#
# Usage: download-verify-llvm.sh <url> <expected-sha256-or-companion> <tarball> <install_root>
set -euo pipefail

url="$1"
expected="$2"
tarball="$3"
install_root="$4"

echo "Downloading ${url}"
curl -fL --retry 3 --retry-delay 5 --retry-all-errors -o "${tarball}" "${url}"

if [ "${expected}" = "companion" ]; then
  curl -fL --retry 3 --retry-all-errors -o "${tarball}.companion" "${url}.sha256"
  expected="$(awk '{print $1}' "${tarball}.companion")"
fi

echo "${expected}  ${tarball}" >"${tarball}.sha256"
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum -c "${tarball}.sha256"
else
  # macOS shasum fallback
  shasum -a 256 -c "${tarball}.sha256"
fi

echo "Extracting to ${install_root}"
case "${tarball}" in
*.tar.gz) tar -xzf "${tarball}" -C "${install_root}" ;;
*) tar -xJf "${tarball}" -C "${install_root}" ;;
esac

rm -f "${tarball}" "${tarball}.sha256" "${tarball}.companion"
