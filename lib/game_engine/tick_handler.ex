defmodule GameBackend.GameEngine.TickHandler do
  require Logger

  @doc """
  Handles game logic on every tick. Returns updated game state.
  """
  def handle_tick(state) do
    tick = Map.get(state, :tick, 0) + 1

    Logger.debug("Match #{state.match_id} tick #{tick}")

    # Update positions, check collisions, update bots, etc.
    updated_state = state
    |> Map.put(:tick, tick)
    |> maybe_broadcast_frame()

    updated_state
  end

  defp maybe_broadcast_frame(state) do
    # Push new game frame to clients
    # (Use Phoenix.PubSub or Phoenix.Channel broadcast if available)
    state
  end
end
