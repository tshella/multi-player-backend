defmodule GameBackend.Bots.BasicBot do
  use GenServer
  require Logger

  def start_link(match_id) do
    GenServer.start_link(__MODULE__, %{match_id: match_id}, name: via_tuple(match_id))
  end

  def init(state) do
    schedule_decision()
    {:ok, state}
  end

  def handle_info(:make_move, state) do
    action = Enum.random(["move_up", "move_down", "attack", "defend"])
    Logger.debug("Bot sending action: #{action}")

    GenServer.cast({:via, Horde.Registry, {GameBackend.Registry, state.match_id}},
      {:player_action, "bot_#{:rand.uniform(1000)}", action})

    schedule_decision()
    {:noreply, state}
  end

  defp schedule_decision do
    Process.send_after(self(), :make_move, 1_000)
  end

  defp via_tuple(match_id), do: {:via, Registry, {GameBackend.Registry, "bot_#{match_id}"}}
end
