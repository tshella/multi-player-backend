defmodule GameBackendWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :game_backend

  socket "/socket", GameBackendWeb.UserSocket,
    websocket: true,
    longpoll: false

  plug Plug.Static,
    at: "/",
    from: :game_backend,
    gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt)

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, store: :cookie, key: "_game_backend_key", signing_salt: "change_me"

  plug CORSPlug
  plug GameBackendWeb.Router
end
