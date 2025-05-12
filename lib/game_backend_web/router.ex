defmodule GameBackendWeb.Router do
  use GameBackendWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", GameBackendWeb do
    pipe_through :api

    post "/login", AuthController, :login
    get "/leaderboard", LeaderboardController, :index
    post "/match", MatchController, :create
  end

  forward "/api-docs", PhoenixSwagger.Plug.SwaggerUI,
    otp_app: :game_backend,
    swagger_file: "swagger.json"

  forward "/graphql", Absinthe.Plug,
    schema: GameBackendWeb.Schema
end
