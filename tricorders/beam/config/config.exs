import Config

config :grpc, start_server: true

import_config "#{config_env()}.exs"
