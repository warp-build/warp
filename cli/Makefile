WARP_EXE=warp

.PHONY: build
build:
	cargo build

.PHONY: manual
manual:
	mdbook build --dest-dir ../docs ./manual

.PHONY: test
test:
	cargo test \
		--no-fail-fast \
		--tests

.PHONY: integration_test
test.integration:
	cargo test \
		--no-fail-fast -- \
		--ignored

.PHONY: release.win
release.win: WARP_EXE=warp.exe
release.win: release

.PHONY: release
release:
	cargo build --release --target aarch64-apple-darwin
	cargo strip --target aarch64-apple-darwin
	tar czf warp-aarch64-apple-darwin.tar.gz -C ./target/aarch64-apple-darwin/release/ $(WARP_EXE)
	cargo build --release --target x86_64-apple-darwin
	cargo strip --target x86_64-apple-darwin
	tar czf warp-x86_64-apple-darwin.tar.gz -C ./target/x86_64-apple-darwin/release/ $(WARP_EXE)

.PHONY: install
install:
	cargo install --locked --debug --path ./bin

.PHONY: setup
setup:
	rustup default stable
	rustup target add x86_64-apple-darwin
	cargo install hyperfine cargo-strip cargo-insta mdbook flamegraph miri

.PHONY: clean
clean:
	cargo clean

.PHONY: fmt
fmt:
	cargo fmt
