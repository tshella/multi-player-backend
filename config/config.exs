import Config

config :game_backend,
  ecto_repos: [GameBackend.Repo]

config :phoenix, :json_library, Jason

config :game_backend, Oban,
  repo: GameBackend.Repo,
  plugins: [Oban.Plugins.Pruner],
  queues: [default: 10, ranking: 5]

config :game_backend, GameBackend.Cache.GameCache,
  adapter: NebulexRedisAdapter,
  primary: [
    backend: NebulexRedisAdapter,
    host: "localhost",
    port: 6379
  ]

config :game_backend, GameBackend.Guardian,
  issuer: "game_backend",
  secret_key: "your_super_secret_key"

config :logger, :console,
  format: "[$level] $message\n",
  metadata: [:request_id]

import_config "#{config_env()}.exs"
