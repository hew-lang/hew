#!/usr/bin/env bash
# Download, checksum-verify, and extract the Binaryen release used by
# wasm-pack v0.13.1 on Linux x86_64.
#
# The caller wraps this complete immutable-download unit in retry-download.sh,
# so every attempt fetches fresh bytes and verifies them before extraction.
#
# Usage: download-verify-binaryen.sh <version> <expected_sha256> <install_root>
set -euo pipefail

version="$1"
expected="$2"
install_root="$3"
asset="binaryen-${version}-x86_64-linux.tar.gz"
tarball="${install_root}/${asset}"
url="https://github.com/WebAssembly/binaryen/releases/download/${version}/${asset}"

mkdir -p "${install_root}"
echo "Downloading ${url}"
curl -fL --retry 3 --retry-delay 5 --retry-all-errors -o "${tarball}" "${url}"

echo "${expected}  ${tarball}" > "${tarball}.sha256"
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum -c "${tarball}.sha256"
else
  shasum -a 256 -c "${tarball}.sha256"
fi

tar -xzf "${tarball}" -C "${install_root}"
rm -f "${tarball}" "${tarball}.sha256"
