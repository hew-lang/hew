#!/usr/bin/env bash
# Use the hosted image's package lists first. Refresh once when a package is
# missing from the lists or its recorded download is no longer available.
set -euo pipefail

apt=(sudo apt-get -o Acquire::Retries=1 -o DPkg::Lock::Timeout=30
    -o Acquire::http::Timeout=15 -o Acquire::https::Timeout=15)
install=("${apt[@]}" install -y --no-upgrade --no-install-recommends "$@")
if ! "${install[@]}"; then
    "${apt[@]}" update
    "${install[@]}"
fi
