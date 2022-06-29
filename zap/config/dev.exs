import Config

# Configure your database
config :zap, Zap.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "zap_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application. For example, we use it
# with esbuild to bundle .js and .css sources.
config :zap, ZapWeb.Endpoint,
  # Binding to loopback ipv4 address prevents access from other machines.
  # Change to `ip: {0, 0, 0, 0}` to allow access from other machines.
  http: [ip: {0, 0, 0, 0}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "/FBNVVmAHValD1I6+9DZS6iqBZEpt4OqpMI8ozbjf+uT4lZJ0P3HFmxznnBopsVu",
  watchers: []

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime

config :zap, SharedCache.Artifactory,
  store: Zap.SharedCache.Artifactory.S3

config :zap, SharedCache.Artifactory.S3,
  bucket_name: "local.abstractmachines.zap.artifacts"
  # bucket_name: "local.warp.artifacts"

config :ex_aws,
  debug_request: true,
  region: "",
  access_key_id: "JPT3JEIGTMRDY1HAHYB2",
  secret_access_key: "ffLx8m8m+NX9DGAvzb3LY2X4DwZ2DPgF0DlglgAD"

config :ex_aws, :s3,
  debug_request: true,
  local: true,
  region: "local",
  host: "192.168.1.252",
  scheme: "http://",
  port: 9000

