defmodule GameBackend.Ranking do
  @moduledoc """
  Contains logic for calculating and updating player ELO rankings.
  """

  alias GameBackend.Repo
  alias GameBackend.Accounts.User

  @default_elo 1200
  @k_factor 32

  @doc """
  Processes results from a completed match and updates each player's ELO.
  `results` is a list of maps: [%{id: "p1", score: 20}, %{id: "p2", score: 10}]
  """
  def process_results(_match_id, results) when length(results) > 1 do
    Enum.each(pairwise(results), fn [{p1}, {p2}] ->
      update_elo(p1, p2)
    end)
  end

  defp update_elo(%{id: id1, score: s1}, %{id: id2, score: s2}) do
    with {:ok, user1} <- get_user(id1),
         {:ok, user2} <- get_user(id2) do
      {r1, r2} = {user1.elo || @default_elo, user2.elo || @default_elo}

      e1 = expected_score(r1, r2)
      e2 = expected_score(r2, r1)

      {s1n, s2n} = actual_score(s1, s2)

      new_r1 = r1 + @k_factor * (s1n - e1)
      new_r2 = r2 + @k_factor * (s2n - e2)

      user1 |> Ecto.Changeset.change(%{elo: round(new_r1)}) |> Repo.update()
      user2 |> Ecto.Changeset.change(%{elo: round(new_r2)}) |> Repo.update()
    end
  end

  defp expected_score(ra, rb), do: 1 / (1 + :math.pow(10, (rb - ra) / 400))

  defp actual_score(s1, s2) do
    cond do
      s1 > s2 -> {1, 0}
      s1 < s2 -> {0, 1}
      true -> {0.5, 0.5}
    end
  end

  defp get_user(id) do
    case Repo.get(User, id) do
      nil -> {:error, :not_found}
      user -> {:ok, user}
    end
  end

  defp pairwise(players) do
    for i <- 0..(length(players) - 2) do
      [Enum.at(players, i), Enum.at(players, i + 1)]
    end
  end
end
