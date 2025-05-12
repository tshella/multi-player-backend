defmodule GameBackend.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Database
      GameBackend.Repo,

      # PubSub for real-time
      {Phoenix.PubSub, name: GameBackend.PubSub},

      # Registry for named GameServer processes
      {Horde.Registry, [name: GameBackend.Registry, keys: :unique, members: :auto]},

      # Horde-based distributed Dynamic Supervisor for games
      {GameBackend.GameEngine.GameSupervisor, []},

      # Oban background job processor
      {Oban, Application.fetch_env!(:game_backend, Oban)},

      # Telemetry (Prometheus + metrics)
      GameBackend.Telemetry
    ]

    opts = [strategy: :one_for_one, name: GameBackend.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
