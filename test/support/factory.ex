defmodule GameBackend.Factory do
  alias GameBackend.Accounts
  alias GameBackend.Accounts.User
  alias GameBackend.Repo

  def user_fixture(attrs \\ %{}) do
    defaults = %{
      email: "user#{System.unique_integer()}@example.com",
      username: "user_#{:rand.uniform(1000)}",
      password: "password123"
    }

    merged = Map.merge(defaults, attrs)
    {:ok, user} = Accounts.register_user(merged)
    user
  end

  def match_fixture(players \\ []) do
    match_id = "match_#{System.unique_integer()}"
    %{id: match_id, players: players, map: "arena_1"}
  end
end
