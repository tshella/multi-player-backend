import Config

config :game_backend, GameBackend.Repo,
  username: "postgres",
  password: "postgres",
  database: "game_backend_test#{System.get_env("MIX_TEST_PARTITION")}",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

config :game_backend, GameBackendWeb.Endpoint,
  http: [port: 4002],
  server: false

config :logger, level: :warn
