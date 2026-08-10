#!/usr/bin/env bash
# Shared FreeBSD package and Rust toolchain setup for every VM-backed job.

set -euo pipefail

readonly HEW_FREEBSD_RUST_VERSION=1.96.0
readonly HEW_FREEBSD_NEXTEST_VERSION=0.9.99
readonly HEW_FREEBSD_CARGO_ABOUT_VERSION=0.9.1

hew_freebsd_prepare() {
    local build_user=${1:-}
    mkdir -p /usr/local/etc/pkg/repos
    # pkg expands ABI when it reads this repository configuration.
    # shellcheck disable=SC2016
    printf 'FreeBSD: { url: "pkg+https://pkg.FreeBSD.org/${ABI}/latest", mirror_type: "srv", enabled: yes }\n' \
        > /usr/local/etc/pkg/repos/FreeBSD.conf
    /usr/sbin/pkg bootstrap -fy -r FreeBSD
    pkg update -f -r FreeBSD

    local rust_package=rust
    if [[ $(uname -m) == amd64 ]]; then
        rust_package=rustup-init
    fi
    pkg install -y -U -r FreeBSD \
        llvm22 gdb "${rust_package}" python3 cmake ninja git gmake bash \
        pkgconf libffi libxml2 wasmtime

    ln -sf /usr/local/llvm22/bin/wasm-ld /usr/local/bin/wasm-ld
    if [[ -n ${build_user} ]] && ! pw usershow "${build_user}" >/dev/null 2>&1; then
        pw useradd -n "${build_user}" -m -s /usr/local/bin/bash
    fi
}

hew_freebsd_activate() {
    local gates=${1:-build}
    export PATH="${HOME}/.cargo/bin:/usr/local/llvm22/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"
    export LLVM_SYS_221_PREFIX=/usr/local/llvm22
    export PYTHON=/usr/local/bin/python3

    if command -v rustup-init >/dev/null 2>&1; then
        if [[ ! -x ${HOME}/.cargo/bin/rustup ]]; then
            rustup-init -y --no-modify-path --profile minimal \
                --default-toolchain "${HEW_FREEBSD_RUST_VERSION}"
        fi
        rustup toolchain install "${HEW_FREEBSD_RUST_VERSION}" --profile minimal
        rustup default "${HEW_FREEBSD_RUST_VERSION}"
        rustup target add wasm32-wasip1 --toolchain "${HEW_FREEBSD_RUST_VERSION}"
    fi

    CARGO="$(command -v cargo)"
    export CARGO
    command -v rustc
    command -v "${PYTHON}"
    command -v gmake
    command -v bash
    command -v wasm-ld
    command -v wasmtime
    /usr/local/llvm22/bin/llvm-config --version
    rustc --version
    wasm-ld --version
    wasmtime --version

    case ",${gates}," in
        *,workspace,*|*,platform-smoke,*|*,platform-full,*|*,smoke,*|*,cabi,*)
            if ! cargo nextest --version 2>/dev/null | grep -q "${HEW_FREEBSD_NEXTEST_VERSION}"; then
                cargo install cargo-nextest --locked \
                    --version "${HEW_FREEBSD_NEXTEST_VERSION}"
            fi
            cargo nextest --version | grep -q "${HEW_FREEBSD_NEXTEST_VERSION}"
            ;;
    esac
    case ",${gates}," in
        *,licenses,*)
            if ! cargo about --version 2>/dev/null | grep -q "${HEW_FREEBSD_CARGO_ABOUT_VERSION}"; then
                cargo install cargo-about --locked --features cli \
                    --version "${HEW_FREEBSD_CARGO_ABOUT_VERSION}"
            fi
            cargo about --version | grep -q "${HEW_FREEBSD_CARGO_ABOUT_VERSION}"
            ;;
    esac
}

if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
    case ${1:-} in
        prepare)
            shift
            hew_freebsd_prepare "$@"
            ;;
        *)
            echo "usage: $0 prepare [build-user]" >&2
            exit 2
            ;;
    esac
fi
