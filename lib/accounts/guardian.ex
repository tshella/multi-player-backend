defmodule GameBackend.Accounts.Guardian do
  use Guardian, otp_app: :game_backend

  alias GameBackend.Accounts

  def subject_for_token(user, _claims) do
    {:ok, to_string(user.id)}
  end

  def resource_from_claims(%{"sub" => id}) do
    case Accounts.get_user!(id) do
      user -> {:ok, user}
      _ -> {:error, :not_found}
    end
  end
end
