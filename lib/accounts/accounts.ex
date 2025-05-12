defmodule GameBackend.Accounts do
  import Ecto.Query
  alias GameBackend.Repo
  alias GameBackend.Accounts.User
  alias GameBackend.Accounts.Guardian

  def get_user!(id), do: Repo.get!(User, id)

  def get_user_by_email(email) do
    Repo.get_by(User, email: email)
  end

  def authenticate_user(email, password) do
    case get_user_by_email(email) do
      nil -> {:error, :invalid_credentials}
      user ->
        if Bcrypt.verify_pass(password, user.password_hash) do
          {:ok, user, Guardian.encode_and_sign(user)}
        else
          {:error, :unauthorized}
        end
    end
  end

  def create_user(attrs) do
    %User{}
    |> User.registration_changeset(attrs)
    |> Repo.insert()
  end
end
