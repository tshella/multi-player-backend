defmodule GameBackend.AccountsTest do
  use GameBackend.DataCase, async: true

  alias GameBackend.Accounts

  describe "create_user/1" do
    test "creates a user with valid data" do
      attrs = %{email: "test@example.com", username: "testuser", password: "secure123"}

      assert {:ok, user} = Accounts.register_user(attrs)
      assert user.email == "test@example.com"
    end

    test "fails with missing fields" do
      assert {:error, changeset} = Accounts.register_user(%{})
      assert %{email: ["can't be blank"]} = errors_on(changeset)
    end
  end
end
