defmodule GameBackendWeb.GameChannel do
  use Phoenix.Channel
  alias GameBackendWeb.Presence

  def join("game:" <> match_id, _params, socket) do
    send(self(), :after_join)
    {:ok, assign(socket, :match_id, match_id)}
  end

  def handle_info(:after_join, socket) do
    Presence.track(socket, socket.id, %{online_at: inspect(System.system_time(:second))})
    push(socket, "presence_state", Presence.list(socket))
    {:noreply, socket}
  end

  def handle_in("player_action", %{"action" => action}, socket) do
    match_id = socket.assigns.match_id
    player_id = socket.id

    GenServer.cast({:via, Horde.Registry, {GameBackend.Registry, match_id}}, {:player_action, player_id, action})
    {:noreply, socket}
  end
end
