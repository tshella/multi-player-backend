ExUnit.start()
Ecto.Adapters.SQL.Sandbox.mode(GameBackend.Repo, :manual)

# Load test support files
Code.require_file("support/data_case.ex", __DIR__)
Code.require_file("support/factory.ex", __DIR__)
