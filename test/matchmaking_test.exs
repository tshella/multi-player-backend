defmodule GameBackend.MatchmakingTest do
  use ExUnit.Case, async: true

  alias GameBackend.Matchmaking

  test "queues players and forms a match" do
    Enum.each(1..4, fn i ->
      Matchmaking.join_queue("player_#{i}", "eu", 1300 + i)
    end)

    # Allow match cycle to run
    :timer.sleep(2500)

    match_pid = Horde.Registry.lookup(GameBackend.Registry, "match-")
    assert length(match_pid) > 0
  end
end
