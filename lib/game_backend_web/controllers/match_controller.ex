defmodule GameBackendWeb.MatchController do
  use GameBackendWeb, :controller
  alias GameBackend.GameEngine.GameSupervisor

  def create(conn, %{"match_id" => match_id}) do
    case GameSupervisor.start_game(match_id) do
      {:ok, _pid} -> json(conn, %{status: "started", match_id: match_id})
      {:error, reason} -> conn |> put_status(:bad_request) |> json(%{error: reason})
    end
  end
end
