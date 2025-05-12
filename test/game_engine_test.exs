defmodule GameBackend.GameEngineTest do
  use ExUnit.Case, async: true

  alias GameBackend.GameEngine.{GameServer, GameSupervisor}

  @match_id "test-match-123"

  setup do
    {:ok, _pid} = GameSupervisor.start_game(@match_id, %{players: [], map: "test"})
    :ok
  end

  test "game server starts and ticks" do
    pid = Process.whereis({:via, Horde.Registry, {GameBackend.Registry, @match_id}})
    assert is_pid(pid)
    assert Process.alive?(pid)

    :timer.sleep(50)
    state = :sys.get_state(pid)
    assert Map.has_key?(state, :tick)
  end
end
