import Config

config :tricorder,
  grpc_port: System.get_env("TRICORDER_GRPC_PORT", "21000")

config :grpc, start_server: true

import_config "#{config_env()}.exs"
