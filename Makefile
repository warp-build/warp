export
WARP_EXE=warp

all: setup.local build test

.PHONY: clean
clean:
	cargo clean

.PHONY: build
build:
	cargo build

build.mac.m1:
	cargo build --target aarch64-apple-darwin

build.mac.intel:
	cargo build --target x86_64-apple-darwin

build.linux.arm:
	CC=aarch64-unknown-linux-gnu-gcc cargo build --target aarch64-unknown-linux-gnu

build.linux.intel:
	CC=x86_64-unknown-linux-gnu-gcc cargo build --target x86_64-unknown-linux-gnu

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

.PHONY: check
check:
	cargo fmt --check -- \
		--config unstable_features=true \
		--config imports_granularity="Module" \
		--config normalize_doc_attributes=true  \
		--config space_after_colon=true 
	deno fmt --check
	mix format --check-formatted

.PHONY: fmt
fmt:
	cargo clippy --fix --allow-dirty --allow-staged
	cargo fix --allow-dirty --allow-staged
	cargo fmt -- \
		--config unstable_features=true \
		--config imports_granularity="Module" \
		--config normalize_doc_attributes=true  \
		--config space_after_colon=true 
	deno fmt
	mix format

.PHONY: setup
setup:
	rustup default stable

.PHONY: setup.local
setup.local: setup
	cargo install hyperfine cargo-strip cargo-insta mdbook flamegraph miri cargo-asm cargo-criterion

.PHONY: test
test: test.tricorder test.unit test.conc test.beam bootstrap

.PHONY: test.beam
test.beam:
	cd tricorders/beam \
		&& ./bootstrap.sh \
		&& mix deps.get \
		&& mix test

.PHONY: test.tricorder
test.tricorder:
	cargo build --package test-tricorder \
		&& cp target/debug/test-tricorder ./core/tests/test_tricorder \
		&& cd ./core/tests/test_tricorder \
		&& tar czf package.tar.gz test-tricorder Manifest.json

.PHONY: test.unit
test.unit:
	cargo test

.PHONY: test.conc
test.conc:
	RUSTFLAGS="--cfg shuttle" cargo test conc_ --release

bootstrap: install
	warp bootstrap --print-hashes
	warp build ./tricorders/beam/mix.exs
	# warp build ./tricorders/rust/Cargo.toml

.PHONY: cov
cov:
	cargo llvm-cov --open

.PHONY: doc
doc:
	cargo doc --document-private-items --open
