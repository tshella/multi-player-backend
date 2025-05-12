defmodule GameBackend.DataCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      alias GameBackend.Repo

      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      import GameBackend.DataCase
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(GameBackend.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(GameBackend.Repo, {:shared, self()})
    end

    :ok
  end

  @doc """
  A helper to convert changeset errors into a map of messages.
  Example: %{field: ["can't be blank"]}
  """
  def errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end
