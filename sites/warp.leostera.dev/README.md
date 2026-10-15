# warp.leostera.dev

The Warp project site is a React + Vite single-page app deployed to Cloudflare
Workers with the Cloudflare Vite plugin.

```bash
bun install --frozen-lockfile
bun run dev
bun run build
bun run deploy
```

To build through Warp instead of invoking Vite directly, run from the repository
root:

```bash
./target/debug/warp build ./sites/warp.leostera.dev
```
