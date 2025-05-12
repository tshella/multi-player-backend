defmodule GameBackend.GameEngine.GameServer do
  use GenServer
  require Logger

  @tick_rate 33  # approx 30 Hz (33ms interval)

  def start_link(%{match_id: match_id} = args) do
    GenServer.start_link(__MODULE__, args, name: via_tuple(match_id))
  end

  def init(state) do
    Logger.info("GameServer started for match #{state.match_id}")
    schedule_tick()
    {:ok, Map.merge(state, %{tick: 0})}
  end

  def handle_info(:tick, state) do
    new_state = GameBackend.GameEngine.TickHandler.handle_tick(state)
    schedule_tick()
    {:noreply, new_state}
  end

  def handle_cast({:player_action, player_id, action}, state) do
    # Update game state based on action
    Logger.debug("Player #{player_id} performed action #{inspect(action)}")
    {:noreply, state}
  end

  defp schedule_tick do
    Process.send_after(self(), :tick, @tick_rate)
  end

  defp via_tuple(match_id) do
    {:via, Horde.Registry, {GameBackend.Registry, match_id}}
  end
end
