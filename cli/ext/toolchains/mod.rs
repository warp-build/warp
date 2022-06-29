pub const TOOLCHAINS: [(&str, &str); 4] = [
    (
        "https://warp.build/toolchains/archive.js",
        include_str!("archive.js"),
    ),
    (
        "https://warp.build/toolchains/deno.js",
        include_str!("deno.js"),
    ),
    (
        "https://warp.build/toolchains/elixir.js",
        include_str!("elixir.js"),
    ),
    (
        "https://warp.build/toolchains/erlang.js",
        include_str!("erlang.js"),
    ),
];
