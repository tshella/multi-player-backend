defmodule GameBackendWeb.SwaggerController do
  use GameBackendWeb, :controller
  use PhoenixSwagger

  @doc """
  Swagger schema definitions for API types.
  """
  def swagger_definitions do
    %{
      User: swagger_schema do
        title "User"
        description "User account"
        properties do
          email :string, "Email address", required: true
          username :string, "Username"
        end
        example(%{email: "test@example.com", username: "gamer123"})
      end
    }
  end
end
