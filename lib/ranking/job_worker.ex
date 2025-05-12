defmodule GameBackend.Ranking.JobWorker do
  @moduledoc """
  Oban worker to process match results and update player rankings.
  """

  use Oban.Worker,
    queue: :ranking,
    max_attempts: 5

  alias GameBackend.Ranking

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"match_id" => match_id, "results" => results}}) do
    Ranking.process_results(match_id, results)
    :ok
  end
end
