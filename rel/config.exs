import Config

# Start Phoenix Endpoint in release
config :game_backend, GameBackendWeb.Endpoint,
  server: true,
  http: [
    port: String.to_integer(System.get_env("PORT") || "4000")
  ],
  secret_key_base: System.fetch_env!("SECRET_KEY_BASE")

# Configure database
config :game_backend, GameBackend.Repo,
  url: System.fetch_env!("DATABASE_URL"),
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
  ssl: true

# Guardian JWT
config :game_backend, GameBackend.Guardian,
  issuer: "game_backend",
  secret_key: System.fetch_env!("GUARDIAN_SECRET")

# Redis (Nebulex)
config :game_backend, GameBackend.Cache.GameCache,
  adapter: NebulexRedisAdapter,
  primary: [
    backend: NebulexRedisAdapter,
    host: System.get_env("REDIS_HOST") || "localhost",
    port: String.to_integer(System.get_env("REDIS_PORT") || "6379")
  ]

# Oban (Background jobs)
config :game_backend, Oban,
  repo: GameBackend.Repo,
  queues: [
    default: 10,
    ranking: 5,
    events: 5
  ],
  plugins: [Oban.Plugins.Pruner]

# Prometheus metrics
config :game_backend, :telemetry_metrics,
  prometheus_port: String.to_integer(System.get_env("PROMETHEUS_PORT") || "9090")
