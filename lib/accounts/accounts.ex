defmodule GameBackend.Accounts do
  alias GameBackend.Repo
  alias GameBackend.Accounts.User
  alias GameBackend.Accounts.Guardian

  @doc """
  Gets a user by ID. Raises if not found.
  """
  def get_user!(id), do: Repo.get!(User, id)

  @doc """
  Fetches a user by email, or returns nil.
  """
  def get_user_by_email(email) do
    Repo.get_by(User, email: email)
  end

  @doc """
  Authenticates a user by email and password.
  Returns {:ok, user, token} or {:error, reason}.
  """
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

  @doc """
  Creates a new user using the registration changeset.
  """
  def create_user(attrs) do
    %User{}
    |> User.registration_changeset(attrs)
    |> Repo.insert()
  end
end
