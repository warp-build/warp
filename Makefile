WARP_EXE=warp

all: setup.local build test

.PHONY: clean
clean:
	cargo clean

.PHONY: build
build:
	cargo build

.PHONY: release
release:
	cargo build --release
	tar czf release.tar.gz -C ./target/release/ $(WARP_EXE)

.PHONY: install
install:
	cargo install --debug --path cli

.PHONY: fmt
fmt:
	cargo fmt
	deno fmt

.PHONY: setup
setup:
	rustup default stable

.PHONY: setup.local
setup.local: setup
	rustup target add x86_64-apple-darwin
	rustup target add x86_64-unknown-linux-gnu
	rustup target add aarch64-unknown-linux-gnu
	cargo install hyperfine cargo-strip cargo-insta mdbook flamegraph miri cargo-asm

.PHONY: test
test: test.tricorder test.unit test.conc

.PHONY: test.tricorder
test.tricorder:
	cd core/tests/test_tricorder/ \
		&& cargo build \
		&& tar czf package.tar.gz target/debug/tricorder Manifest.json

.PHONY: test.unit
test.unit:
	cargo test

.PHONY: test.conc
test.conc:
	RUSTFLAGS="--cfg shuttle" cargo test conc_ --release

.PHONY: cov
cov:
	cargo llvm-cov --open

.PHONY: doc
doc:
	cargo doc --document-private-items --open
