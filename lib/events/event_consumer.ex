defmodule GameBackend.Events.EventConsumer do
  use Broadway

  alias Broadway.Message

  def start_link(_opts) do
    Broadway.start_link(__MODULE__,
      name: __MODULE__,
      producer: [
        module: {
          BroadwayRabbitMQ.Producer,
          queue: "game_event_queue",
          connection: [host: "localhost"]
        },
        concurrency: 1
      ],
      processors: [default: [concurrency: 5]]
    )
  end

  def handle_message(_processor, %Message{data: data} = msg, _context) do
    event = Jason.decode!(data)
    IO.inspect(event, label: "[Game Event Received]")
    Message.ack_immediately(msg)
  end
end
