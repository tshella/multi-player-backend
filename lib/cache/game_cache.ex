defmodule GameBackend.Cache.GameCache do
  use Nebulex.Cache,
    otp_app: :game_backend,
    adapter: NebulexRedisAdapter
end
