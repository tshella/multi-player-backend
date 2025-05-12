defmodule GameBackend.Metrics.PrometheusMetrics do
  @moduledoc """
  Registers Telemetry metrics and exposes them to Prometheus.
  """

  use Supervisor
  import Telemetry.Metrics

  def start_link(_arg) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    children = [
      {
        TelemetryMetricsPrometheus,
        metrics: metrics(), name: :prometheus_metrics
      }
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp metrics do
    [
      # HTTP and Phoenix Channels
      summary("phoenix.endpoint.stop.duration", unit: {:native, :millisecond}),
      summary("phoenix.router_dispatch.stop.duration", tags: [:route], unit: {:native, :millisecond}),
      counter("phoenix.channel.join.count", tags: [:channel]),
      counter("phoenix.channel.message.count", tags: [:event]),

      # Game ticks and matches
      counter("game.match_started.count"),
      summary("game.tick.duration", unit: {:native, :millisecond}),
      summary("game.player_action.duration", unit: {:native, :millisecond}),

      # Matchmaking
      counter("matchmaking.join_queue.count"),
      summary("matchmaking.matchmaking.duration", unit: {:native, :millisecond}),
      counter("matchmaking.match_found.count"),

      # Oban jobs
      summary("oban.job.duration", unit: {:native, :millisecond}),
      counter("oban.job.success"),
      counter("oban.job.failure")
    ]
  end
end
