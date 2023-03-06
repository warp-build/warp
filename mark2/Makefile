setup:
	rustup default stable
	rustup target add x86_64-apple-darwin
	cargo install hyperfine cargo-strip cargo-insta mdbook flamegraph miri cargo-asm

test: test.tricorder test.unit test.conc

test.tricorder:
	cd tests/test_tricorder/ \
		&& cargo build \
		&& tar czf package.tar.gz target/debug/tricorder Manifest.json

test.unit:
	cargo test

test.conc:
	RUSTFLAGS="--cfg shuttle" cargo test conc_ --release

cov:
	cargo llvm-cov --open

doc:
	cargo doc --document-private-items --open
