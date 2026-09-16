import Config

config :tricorder,
  start_server: false,
  grpc_port: System.get_env("TRICORDER_GRPC_PORT", "21000"),
  workspace_root: "."

config :grpc, start_server: true
