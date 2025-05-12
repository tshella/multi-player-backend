import Config

config :game_backend, GameBackendWeb.Endpoint,
  server: true,
  http: [port: String.to_integer(System.get_env("PORT") || "4000")],
  secret_key_base: System.fetch_env!("SECRET_KEY_BASE")

config :game_backend, GameBackend.Repo,
  url: System.fetch_env!("DATABASE_URL"),
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

config :game_backend, GameBackend.Guardian,
  issuer: "game_backend",
  secret_key: System.fetch_env!("GUARDIAN_SECRET")
