import Config

database_url = System.get_env("DATABASE_URL") || raise "DATABASE_URL not set"

config :game_backend, GameBackend.Repo,
  url: database_url,
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
  ssl: true

secret_key_base = System.get_env("SECRET_KEY_BASE") || raise "SECRET_KEY_BASE not set"

config :game_backend, GameBackendWeb.Endpoint,
  http: [port: String.to_integer(System.get_env("PORT") || "4000")],
  secret_key_base: secret_key_base,
  server: true

config :logger, level: :info
