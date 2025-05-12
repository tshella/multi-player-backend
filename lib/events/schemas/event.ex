defmodule GameBackend.Events.Schemas.Event do
  @moduledoc """
  Schema for validating and documenting game event structures.
  """
  defstruct [:type, :data]

  @type t :: %__MODULE__{
          type: String.t(),
          data: map()
        }
end
