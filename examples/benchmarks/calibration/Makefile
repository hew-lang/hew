ROOT := ../../..
HEW ?= $(ROOT)/build/bin/hew
OUT ?= $(CURDIR)/out
RUSTC ?= rustc
CC := clang
GO ?= go
PYTHON ?= python3
CARGO_BUILD_JOBS ?= 1
PRIMITIVE_SOURCE ?= primitives

.PHONY: build primitives concurrency run smoke compile-scaling check primitives-hew-o0 primitives-hew primitives-rust primitives-rust-checked primitives-go primitives-c concurrency-hew-o0 concurrency-hew concurrency-go concurrency-tokio concurrency-beam

build: primitives concurrency

primitives: primitives-hew-o0 primitives-hew primitives-rust primitives-rust-checked primitives-go primitives-c

primitives-hew-o0:
	mkdir -p "$(OUT)"
	"$(HEW)" build "$(PRIMITIVE_SOURCE).hew" --opt-level 0 -o "$(OUT)/primitives-hew-o0"

primitives-hew:
	mkdir -p "$(OUT)"
	"$(HEW)" build "$(PRIMITIVE_SOURCE).hew" --release -o "$(OUT)/primitives-hew"

primitives-rust:
	mkdir -p "$(OUT)"
	$(RUSTC) --edition 2021 -C opt-level=2 "$(PRIMITIVE_SOURCE).rs" -o "$(OUT)/primitives-rust"

primitives-rust-checked:
	mkdir -p "$(OUT)"
	$(RUSTC) --edition 2021 -C opt-level=2 -C overflow-checks=yes "$(PRIMITIVE_SOURCE).rs" -o "$(OUT)/primitives-rust-checked"

primitives-go:
	mkdir -p "$(OUT)"
	$(GO) build -o "$(OUT)/primitives-go" "$(PRIMITIVE_SOURCE).go"

primitives-c:
	mkdir -p "$(OUT)"
	$(CC) -O2 -std=c11 "$(PRIMITIVE_SOURCE).c" -o "$(OUT)/primitives-c"

concurrency: concurrency-hew-o0 concurrency-hew concurrency-go concurrency-tokio concurrency-beam

concurrency-hew-o0:
	mkdir -p "$(OUT)"
	"$(HEW)" build concurrency.hew --opt-level 0 -o "$(OUT)/concurrency-hew-o0"

concurrency-hew:
	mkdir -p "$(OUT)"
	"$(HEW)" build concurrency.hew --release -o "$(OUT)/concurrency-hew"

concurrency-go:
	mkdir -p "$(OUT)"
	$(GO) build -o "$(OUT)/concurrency-go" concurrency.go

concurrency-tokio:
	mkdir -p "$(OUT)"
	CARGO_BUILD_JOBS=$(CARGO_BUILD_JOBS) cargo build --manifest-path tokio/Cargo.toml --release --locked --target-dir "$(OUT)/tokio-target"

concurrency-beam:
	mkdir -p "$(OUT)"
	erlc -o "$(OUT)" concurrency.erl

run:
	$(PYTHON) run.py --hew "$(HEW)" --output "$(OUT)" $(ARGS)

smoke:
	$(PYTHON) run.py --hew "$(HEW)" --output "$(OUT)" --smoke $(ARGS)

compile-scaling:
	$(PYTHON) run.py --hew "$(HEW)" --output "$(OUT)" --compile-only $(ARGS)

check:
	$(PYTHON) -m unittest discover -s . -p 'test_*.py'
