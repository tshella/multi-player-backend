defmodule GameBackend.Matchmaking.MatchSupervisor do
  @moduledoc """
  Launches a new match using the GameSupervisor.
  """

  alias GameBackend.GameEngine.GameSupervisor

  def start_match(match_id, players) do
    opts = %{
      match_id: match_id,
      players: players,
      map: "default_arena"
    }

    GameSupervisor.start_game(match_id, opts)
  end
end
