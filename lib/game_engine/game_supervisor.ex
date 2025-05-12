defmodule GameBackend.GameEngine.GameSupervisor do
  use Horde.DynamicSupervisor

  alias GameBackend.GameEngine.GameServer

  def start_link(_args) do
    Horde.DynamicSupervisor.start_link(
      __MODULE__,
      [],
      name: __MODULE__
    )
  end

  def init(_) do
    Horde.DynamicSupervisor.init(strategy: :one_for_one, members: :auto)
  end

  def start_game(match_id, opts \\ %{}) do
    child_spec = %{
      id: match_id,
      start: {GameServer, :start_link, [%{match_id: match_id} |> Map.merge(opts)]},
      restart: :transient,
      shutdown: 5000,
      type: :worker
    }

    Horde.DynamicSupervisor.start_child(__MODULE__, child_spec)
  end
end
