defmodule GameBackend.MixProject do
  use Mix.Project

  def project do
    [
      app: :game_backend,
      version: "0.1.0",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {GameBackend.Application, []},
      extra_applications: [:logger, :runtime_tools, :gettext]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      # Phoenix + Web
      {:phoenix, "~> 1.7"},
      {:phoenix_pubsub, "~> 2.1"},
      {:phoenix_ecto, "~> 4.4"},
      {:phoenix_live_dashboard, "~> 0.7"},
      {:phoenix_swagger, "~> 0.8"},
      {:cors_plug, "~> 3.0"},

      # Ecto + DB
      {:ecto_sql, "~> 3.7"},
      {:postgrex, ">= 0.0.0"},

      # Real-time + Messaging
      {:broadway, "~> 1.0"},
      {:broadway_rabbitmq, "~> 0.7"},
      {:libcluster, "~> 3.3"},
      {:horde, "~> 0.8.7"},

      # Background jobs
      {:oban, "~> 2.15"},

      # Auth
      {:guardian, "~> 2.0"},

      # API + GraphQL
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},

      # Caching
      {:nebulex, "~> 2.4"},
      {:nebulex_redis_adapter, "~> 2.4"},

      # Telemetry + Metrics
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},

      # Utilities
      {:jason, "~> 1.2"},
      {:plug_cowboy, "~> 2.5"},
      {:gettext, "~> 0.20"},
      {:elixir_uuid, "~> 1.2"},
      {:bcrypt_elixir, "~> 3.0"},
      {:ex_json_schema, "~> 0.9"},

      # gRPC
      {:grpc, github: "elixir-grpc/grpc", override: true}
    ]
  end
end
