import Config

config :game_backend, GameBackend.Repo,
  username: "postgres",
  password: "postgres",
  database: "game_backend_dev",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

config :game_backend, GameBackendWeb.Endpoint,
  http: [port: 4000],
  code_reloader: true,
  check_origin: false,
  debug_errors: true,
  secret_key_base: "dev_secret_key_base",
  watchers: []

config :logger, level: :debug

config :phoenix, :stacktrace_depth, 20
