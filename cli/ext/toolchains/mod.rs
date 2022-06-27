pub const TOOLCHAINS: [(&str, &str); 4] = [
    (
        "https://zap.build/toolchains/archive.js",
        include_str!("archive.js"),
    ),
    ("https://zap.build/toolchains/deno.js", include_str!("deno.js")),
    (
        "https://zap.build/toolchains/elixir.js",
        include_str!("elixir.js"),
    ),
    (
        "https://zap.build/toolchains/erlang.js",
        include_str!("erlang.js"),
    ),
];
