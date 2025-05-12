defmodule GameBackend.Bots.BotSupervisor do
  use DynamicSupervisor
  alias GameBackend.Bots.BasicBot

  def start_link(_args) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_bot(match_id) do
    spec = {BasicBot, match_id}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end
