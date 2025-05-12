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
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]


  defp deps do
    [
      {:phoenix, "~> 1.7"},
      {:phoenix_pubsub, "~> 2.1"},
      {:phoenix_ecto, "~> 4.4"},
      {:ecto_sql, "~> 3.7"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_live_dashboard, "~> 0.7"},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:oban, "~> 2.15"},
      {:guardian, "~> 2.0"},
      {:broadway, "~> 1.0"},
      {:broadway_rabbitmq, "~> 0.7"},
      {:jason, "~> 1.2"},
      {:plug_cowboy, "~> 2.5"},
      {:phoenix_swagger, "~> 0.8"},
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},
      {:libcluster, "~> 3.3"},
      {:horde, "~> 0.8.7"},
      {:nebulex, "~> 2.4"},
      {:nebulex_redis_adapter, "~> 2.4"},
      {:grpc, github: "elixir-grpc/grpc", override: true}
    ]
  end
end
