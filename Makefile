export
WARP_EXE=warp

all: setup.local build test

.PHONY: clean
clean:
	cargo clean

.PHONY: build
build:
	cargo build

.PHONY: build
bench:
	cargo criterion


.PHONY: release
release: release.mac.intel release.mac.m1 release.linux.intel release.linux.arm

release.mac.m1:
	cargo build --release --target aarch64-apple-darwin
	tar czf dist/warp-aarch64-apple-darwin.tar.gz -C ./target/aarch64-apple-darwin/release $(WARP_EXE)

release.mac.intel:
	cargo build --release --target x86_64-apple-darwin
	tar czf dist/warp-x86_64-apple-darwin.tar.gz -C ./target/x86_64-apple-darwin/release $(WARP_EXE)

release.linux.intel:
	CC=x86_64-unknown-linux-gnu-gcc cargo build --release --target x86_64-unknown-linux-gnu
	tar czf dist/warp-x86_64-unknown-linux-gnu.tar.gz -C ./target/x86_64-unknown-linux-gnu/release $(WARP_EXE)

release.linux.arm:
	CC=aarch64-unknown-linux-gnu-gcc cargo build --release --target aarch64-unknown-linux-gnu
	tar czf dist/warp-aarch64-unknown-linux-gnu.tar.gz -C ./target/aarch64-unknown-linux-gnu/release $(WARP_EXE)


.PHONY: install
install:
	cargo install --debug --path cli

.PHONY: install.release
install.release:
	cargo install --path cli

.PHONY: fmt
fmt:
	cargo fmt
	deno fmt
	mix format

.PHONY: setup
setup:
	rustup default stable

.PHONY: setup.local
setup.local: setup
	rustup target add aarch64-apple-darwin
	rustup target add x86_64-apple-darwin
	rustup target add x86_64-unknown-linux-gnu
	rustup target add aarch64-unknown-linux-gnu
	cargo install hyperfine cargo-strip cargo-insta mdbook flamegraph miri cargo-asm cargo-criterion

.PHONY: test
test: test.tricorder test.unit test.conc test.beam

.PHONY: test.beam
test.beam:
	cd tricorders/beam \
		&& mix deps.get \
		&& mix test

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
