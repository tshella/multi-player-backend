defmodule GameBackend.Matchmaking do
  @moduledoc """
  Handles matchmaking with region and ELO-based grouping.
  """

  use GenServer
  require Logger

  alias GameBackend.Matchmaking.MatchSupervisor

  @match_size 4
  @elo_tolerance 200

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{queue: []}, name: __MODULE__)
  end

  def join_queue(player_id, region \\ "global", elo \\ 1200) do
    GenServer.cast(__MODULE__, {:join, %{id: player_id, region: region, elo: elo}})
  end

  def init(state) do
    schedule_match_check()
    {:ok, state}
  end

  def handle_cast({:join, player_info}, %{queue: queue} = state) do
    Logger.info("Player #{player_info.id} queued (#{player_info.region}, ELO: #{player_info.elo})")
    {:noreply, %{state | queue: queue ++ [player_info]}}
  end

  def handle_info(:check_match, state) do
    {matches, remaining_queue} = group_and_match(state.queue)
    Enum.each(matches, fn players ->
      match_id = "match-" <> UUID.uuid4()
      Logger.info("Starting match #{match_id} with players #{inspect(Enum.map(players, & &1.id))}")

      MatchSupervisor.start_match(match_id, players)

      Phoenix.PubSub.broadcast(
        GameBackend.PubSub,
        "matchmaking:lobby",
        {:match_found, %{match_id: match_id, players: Enum.map(players, & &1.id)}}
      )
    end)

    schedule_match_check()
    {:noreply, %{state | queue: remaining_queue}}
  end

  defp group_and_match(queue) do
    queue
    |> Enum.group_by(& &1.region)
    |> Enum.flat_map(fn {_region, players} -> match_by_elo(players) end)
    |> Enum.split_with(fn
      {:match, _players} -> true
      _ -> false
    end)
    |> then(fn {matches, leftovers} ->
      {
        Enum.map(matches, fn {:match, group} -> group end),
        leftovers |> Enum.map(fn {:left, player} -> player end)
      }
    end)
  end

  defp match_by_elo(players) do
    players
    |> Enum.sort_by(& &1.elo)
    |> Enum.chunk_every(@match_size, 1, :discard)
    |> Enum.map(fn chunk ->
      max_elo = Enum.max_by(chunk, & &1.elo).elo
      min_elo = Enum.min_by(chunk, & &1.elo).elo
      if length(chunk) == @match_size and max_elo - min_elo <= @elo_tolerance do
        {:match, chunk}
      else
        Enum.map(chunk, &{:left, &1})
      end
    end)
    |> List.flatten()
  end

  defp schedule_match_check do
    Process.send_after(self(), :check_match, 2_000)
  end
end
