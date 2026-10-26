OUT ?= ../out

.PHONY: build run

build:
	mkdir -p "$(OUT)"
	RUSTC_WRAPPER= CARGO_TARGET_DIR="$(OUT)/rust-target" cargo build --locked --release
	cp "$(OUT)/rust-target/release/calibration-service-rust" "$(OUT)/service-rust"

run: build
	"$(OUT)/service-rust"
