import Config

config :tricorder,
  start_server: false,
  grpc_port: System.get_env("TRICORDER_GRPC_PORT", "21000")

config :grpc, start_server: true
