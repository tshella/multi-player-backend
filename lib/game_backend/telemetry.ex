defmodule GameBackend.Telemetry do
  use Supervisor
  import Telemetry.Metrics

  def start_link(_arg) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    children = [
      {TelemetryMetricsPrometheus, metrics: metrics(), name: GameBackend.Prometheus}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def metrics do
    [
      # Oban metrics
      summary("oban.job.duration", unit: {:native, :millisecond}),
      counter("oban.job.success"),
      counter("oban.job.failure"),

      # Phoenix Endpoint
      summary("phoenix.endpoint.stop.duration", unit: {:native, :millisecond}),
      summary("phoenix.router_dispatch.stop.duration", tags: [:route], unit: {:native, :millisecond}),

      # Custom
      counter("game_backend.match_started.count"),
      summary("game_backend.game_tick.duration", unit: {:native, :millisecond})
    ]
  end
end
