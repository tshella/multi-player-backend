defmodule GameBackend.Events.EventPublisher do
  @moduledoc "Publishes game events to RabbitMQ"

  alias AMQP.{Connection, Channel, Basic}

  @exchange "game_events"

  @doc """
  Publishes a JSON-encoded event to the fanout exchange.
  Example payload: %{type: "match_ended", data: %{match_id: "abc123"}}
  """
  def publish(event_type, payload) do
    {:ok, conn} = Connection.open()
    {:ok, chan} = Channel.open(conn)
    :ok = AMQP.Exchange.declare(chan, @exchange, :fanout, durable: true)

    event = %{type: event_type, data: payload} |> Jason.encode!()
    Basic.publish(chan, @exchange, "", event)

    Channel.close(chan)
    Connection.close(conn)
  end
end
