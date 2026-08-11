# Compatibility facade. The build and gate graph lives in xtask so the same
# prerequisites run on Unix, Windows, and FreeBSD.

.DEFAULT_GOAL := all
PREFIX ?= /usr/local/hew
DESTDIR ?=

.PHONY: all build hew hew-native hew-lsp runtime stdlib wasm-runtime release
all build:
	cargo xtask build all

hew hew-native hew-lsp runtime stdlib:
	cargo xtask build native

wasm-runtime:
	cargo xtask build wasm

release:
	cargo xtask build release

.PHONY: test ci-preflight-smoke
test:
	cargo xtask gate workspace

ci-preflight-smoke:
	cargo xtask gate smoke

.PHONY: test-cabi
test-cabi:
	cargo xtask gate cabi

.PHONY: test-vertical-slice
test-vertical-slice:
	cargo xtask gate vertical-slice

.PHONY: test-hew-ratchet
test-hew-ratchet:
	cargo xtask gate hew-ratchet

.PHONY: test-stdlib-ratchet
test-stdlib-ratchet:
	cargo xtask gate stdlib-ratchet

.PHONY: playground-check playground-manifest-check
playground-check playground-manifest-check:
	cargo xtask gate playground

.PHONY: test-release-binary
test-release-binary:
	cargo xtask gate release-smoke

.PHONY: test-release-lib-link
test-release-lib-link:
	cargo xtask gate release-link

.PHONY: lint
lint:
	cargo xtask gate lint

.PHONY: workflow-lint workflow-local
workflow-lint:
	cargo xtask gate workflow-lint

workflow-local:
	cargo xtask gate workflow-local

.PHONY: structural-lint structural-lint-bootstrap
structural-lint:
	cargo xtask gate structural-lint

structural-lint-bootstrap:
	cargo xtask gate structural-bootstrap-contract

.PHONY: structural-lint-bootstrap-install
structural-lint-bootstrap-install:
	cargo xtask gate structural-bootstrap

.PHONY: test-ast-grep-contract test-structural-lint-bootstrap
test-ast-grep-contract test-structural-lint-bootstrap:
	cargo xtask gate structural-bootstrap-contract

.PHONY: freebsd-workflow-contract-check
freebsd-workflow-contract-check:
	cargo xtask gate freebsd-contract

.PHONY: test-release-workflow-contract
test-release-workflow-contract:
	cargo xtask gate release-contract

.PHONY: check-gate-reachability test-check-gate-reachability
check-gate-reachability test-check-gate-reachability:
	cargo xtask gate reachability

.PHONY: check-sanitizer-gate
check-sanitizer-gate:
	cargo xtask gate sanitizer-contract

.PHONY: hew-fmt-check
hew-fmt-check:
	cargo xtask gate hew-fmt

.PHONY: verify-ffi test-verify-ffi test-python310-toml-compat
verify-ffi test-verify-ffi test-python310-toml-compat:
	cargo xtask gate ffi

.PHONY: runtime-poison-safe-lint runtime-poison-safe-lint-self-test
runtime-poison-safe-lint runtime-poison-safe-lint-self-test:
	cargo xtask gate runtime-poison-lint

.PHONY: lint-wasm-todo lint-wasm-todo-self-test wasm-capability-check
lint-wasm-todo lint-wasm-todo-self-test wasm-capability-check:
	cargo xtask gate wasm-todo-lint

.PHONY: codegen-carried-identity-gate codegen-trap-inventory-check
codegen-carried-identity-gate:
	cargo xtask gate codegen-identity

codegen-trap-inventory-check:
	cargo xtask gate codegen-traps

.PHONY: verify-sys-lane-closure test-sys-lane-closure
verify-sys-lane-closure test-sys-lane-closure:
	cargo xtask gate sys-closure

.PHONY: leak-scan
leak-scan:
	cargo xtask gate leak-scan

.PHONY: ll-identity-selftest
ll-identity-selftest:
	cargo xtask gate ll-identity

.PHONY: sandbox-parity-coverage-check test-sandbox-parity-coverage-check
sandbox-parity-coverage-check test-sandbox-parity-coverage-check:
	cargo xtask gate sandbox-coverage

.PHONY: sandbox-parity observe-functional-test libhew-link-race-test
sandbox-parity:
	cargo xtask gate sandbox-parity

observe-functional-test:
	cargo xtask gate observe-functional

libhew-link-race-test:
	cargo xtask gate libhew-link-race

.PHONY: licenses-check licenses
licenses-check:
	cargo xtask gate licenses

licenses:
	cargo xtask gate licenses-generate

.PHONY: sandbox-fixtures-check
sandbox-fixtures-check:
	cargo xtask gate sandbox-fixtures-check

.PHONY: test-compiler-lifecycle test-opaque-resource-lifecycle-matrix
test-compiler-lifecycle test-opaque-resource-lifecycle-matrix:
	cargo xtask gate compiler-lifecycle

.PHONY: test-opaque-resource-lifecycle-matrix-external
test-opaque-resource-lifecycle-matrix-external:
	cargo xtask gate compiler-lifecycle-external

.PHONY: test-stdlib-execution-proofs
test-stdlib-execution-proofs:
	cargo xtask gate stdlib-execution

.PHONY: mqtt-broker-e2e
mqtt-broker-e2e:
	cargo xtask gate mqtt

.PHONY: test-o2-differential
test-o2-differential:
	cargo xtask gate o2-differential

.PHONY: coverage coverage-summary coverage-lcov coverage-runtime coverage-combined coverage-branch
coverage coverage-lcov coverage-combined coverage-branch:
	cargo xtask gate coverage

coverage-summary:
	cargo xtask gate coverage-summary

coverage-runtime:
	cargo xtask gate coverage-runtime

.PHONY: asan asan-fixtures tsan miri fuzz-smoke
asan:
	cargo xtask gate asan

asan-fixtures:
	cargo xtask gate asan-fixtures

tsan:
	cargo xtask gate tsan

miri:
	cargo xtask gate miri

fuzz-smoke:
	cargo xtask gate fuzz-smoke

.PHONY: stdlib-lint stdlib-errno-gate
stdlib-lint stdlib-errno-gate:
	cargo xtask gate stdlib-lint

.PHONY: publish-docs
publish-docs:
	cargo xtask gate docs

.PHONY: ci-local-linux
ci-local-linux: workflow-local

.PHONY: bootstrap install-hooks clean
bootstrap install-hooks:
	cargo xtask install-hooks

clean:
	cargo xtask clean

.PHONY: assemble assemble-release
assemble:
	cargo xtask assemble

assemble-release:
	cargo xtask assemble --release

.PHONY: install uninstall
install:
	PREFIX="$(PREFIX)" DESTDIR="$(DESTDIR)" cargo xtask install

uninstall:
	PREFIX="$(PREFIX)" DESTDIR="$(DESTDIR)" cargo xtask uninstall

.PHONY: check-libhew-fresh observe
check-libhew-fresh observe:
	cargo xtask build native

.PHONY: ci-preflight ci-preflight-strict pre-release
ci-preflight:
	cargo xtask preflight $(ARGS)

ci-preflight-strict:
	cargo xtask gate ci

pre-release:
	cargo xtask build release
	cargo xtask pre-release $(PLATFORMS)

.PHONY: sandbox-fixtures sandbox-vm-deps playground-manifest playground-wasi-check
sandbox-vm-deps:
	cargo xtask gate sandbox-vm-deps

sandbox-fixtures:
	cargo xtask gate sandbox-fixtures

playground-manifest:
	cargo xtask gate playground-manifest

playground-wasi-check:
	cargo xtask gate playground-wasi

.PHONY: wasm wasm-capability wasm-dist
wasm wasm-dist:
	cargo xtask gate wasm-package

wasm-capability:
	cargo xtask gate wasm-capability

.PHONY: fuzz-corpus fuzz-oracle fuzz-oracle-selftest fuzz-smoke-bootstrap-install
fuzz-corpus:
	cargo xtask gate fuzz-corpus

fuzz-oracle:
	cargo xtask gate fuzz-oracle

fuzz-oracle-selftest:
	cargo xtask gate fuzz-oracle-selftest

fuzz-smoke-bootstrap-install:
	cargo xtask gate fuzz-smoke-bootstrap

.PHONY: checked-mir-expect checked-mir-golden checked-mir-run checked-mir-verify
checked-mir-expect:
	cargo xtask gate checked-mir-expect

checked-mir-golden:
	cargo xtask gate checked-mir-golden

checked-mir-run:
	cargo xtask gate checked-mir-run

checked-mir-verify:
	cargo xtask gate checked-mir-verify

.PHONY: ll-diff ll-golden
ll-diff:
	cargo xtask gate ll-diff

ll-golden:
	cargo xtask gate ll-golden

.PHONY: macos-leak-oracle test-leak-oracle-selftest
macos-leak-oracle:
	cargo xtask gate macos-leak-oracle

test-leak-oracle-selftest:
	cargo xtask gate leak-oracle-selftest

.PHONY: test-compiler-pipeline test-runtime-unit
test-compiler-pipeline:
	cargo xtask gate compiler-pipeline

test-runtime-unit:
	cargo xtask gate runtime-unit

.PHONY: test-core-matrix hew-check-all hew-fmt-property
test-core-matrix:
	cargo xtask gate core-matrix

hew-check-all:
	cargo xtask gate hew-check

hew-fmt-property:
	cargo xtask gate hew-fmt-property

.PHONY: test-package-install test-pkg-import
test-package-install:
	cargo xtask gate package-install

test-pkg-import:
	cargo xtask gate pkg-import

.PHONY: test-example-expectations-selftest test-surface-examples test-ux-examples
test-example-expectations-selftest:
	cargo xtask gate example-contract

test-surface-examples:
	cargo xtask gate surface-examples

test-ux-examples:
	cargo xtask gate ux-examples

.PHONY: test-doc-examples
test-doc-examples:
	cargo xtask gate docs-examples

.PHONY: doc-ratchet-selftest test-asan-fixture-selftest test-structural-authority-audit o2-differential-selftest
doc-ratchet-selftest:
	cargo xtask gate doc-contract

test-asan-fixture-selftest:
	cargo xtask gate asan-fixture-selftest

test-structural-authority-audit:
	cargo xtask gate structural-bootstrap-contract

o2-differential-selftest:
	cargo xtask gate o2-contract
