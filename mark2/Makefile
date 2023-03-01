setup:
	rustup default stable
	rustup target add x86_64-apple-darwin
	cargo install hyperfine cargo-strip cargo-insta mdbook flamegraph miri cargo-asm

test:
	cargo test
	RUSTFLAGS="--cfg shuttle" cargo test conc_ --release

cov:
	cargo llvm-cov --open

doc:
	cargo doc --document-private-items --open
