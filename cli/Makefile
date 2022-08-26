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
	cargo build --release
	cargo strip
	tar czf release.tar.gz -C ./target/release/ $(WARP_EXE)

.PHONY: install
install:
	cargo install --debug --path ./bin

.PHONY: setup
setup:
	cargo install cargo-strip cargo-insta mdbook flamegraph miri

.PHONY: clean
clean:
	cargo clean

.PHONY: fmt
fmt:
	cargo fmt
