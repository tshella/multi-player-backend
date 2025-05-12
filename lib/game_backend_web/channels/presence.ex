defmodule GameBackendWeb.Presence do
  use Phoenix.Presence,
    otp_app: :game_backend,
    pubsub_server: GameBackend.PubSub
end

# lib/game_backend_web/controllers/auth_controller.ex
defmodule GameBackendWeb.AuthController do
  use GameBackendWeb, :controller
  alias GameBackend.Accounts

  def login(conn, %{"email" => email, "password" => password}) do
    with {:ok, user, token} <- Accounts.authenticate_user(email, password) do
      json(conn, %{user: user, token: token})
    else
      _ -> conn |> put_status(:unauthorized) |> json(%{error: "Invalid credentials"})
    end
  end
end
