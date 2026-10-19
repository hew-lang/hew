#!/usr/bin/env bash
# Download, checksum-verify, and extract an LLVM release tarball.
#
# The caller supplies the expected sha256, so a fresh download after a
# network failure either returns the same bytes as any prior attempt or fails
# verification. Callers wrap the whole unit (download + verify + extract) in
# retry-download.sh: a transfer that dies mid-flight or an extraction that
# aborts partway both leave the tarball/install_root in a state a second
# attempt cleanly overwrites, so retrying the unit rather than only the
# curl call is correct and keeps checksum verification mandatory on every
# attempt, not just the first.
#
# Usage: download-verify-llvm.sh <url> <expected-sha256> <tarball> <install_root>
set -euo pipefail

url="$1"
expected="$2"
tarball="$3"
install_root="$4"

echo "Downloading ${url}"
curl -fL --retry 3 --retry-delay 5 --retry-all-errors -o "${tarball}" "${url}"

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

rm -f "${tarball}" "${tarball}.sha256"
