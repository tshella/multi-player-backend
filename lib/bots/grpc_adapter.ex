defmodule GameBackend.Bots.GRPCAdapter do
  @moduledoc """
  Placeholder for future gRPC communication with external AI service.
  """

  def get_action(game_state, bot_id) do
    # Simulate external call — replace with actual gRPC logic later
    IO.inspect({:calling_grpc_for, bot_id, game_state})
    {:ok, Enum.random(["move_left", "move_right", "shoot", "hide"])}
  end
end
