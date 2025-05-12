defmodule GameBackendWeb.Schema do
  use Absinthe.Schema

  query do
    field :hello, :string do
      resolve fn _, _ -> {:ok, "Hello GraphQL!"} end
    end
  end
end
