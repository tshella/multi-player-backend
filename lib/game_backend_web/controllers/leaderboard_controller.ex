defmodule GameBackendWeb.LeaderboardController do
  use GameBackendWeb, :controller
  alias GameBackend.Ranking

  def index(conn, _params) do
    leaderboard = Ranking.get_leaderboard()
    json(conn, leaderboard)
  end
end
