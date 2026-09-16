# Warp

Warp is being rebuilt around a protocol-first execution model.

## Current direction

```text
TaskRequest
  -> ScannedBlueprint
  -> Wormhole execution
  -> VerifiedBlueprint
  -> content-addressed result
```

The initial protocol crate lives at [`crates/warp-protocol`](crates/warp-protocol).
The previous Warp implementation is preserved locally under [`_legacy`](_legacy)
while the new protocol and execution model are developed.

## Development

```bash
cargo test --workspace
```

The rewrite targets a stable Rust toolchain and will eventually provide local,
GitLab, and Cloudflare-backed execution with CAS/R2 storage.
