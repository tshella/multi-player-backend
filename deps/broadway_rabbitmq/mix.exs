defmodule BroadwayRabbitMQ.MixProject do
  use Mix.Project

  @version "0.8.2"
  @description "A RabbitMQ connector for Broadway"
  @source_url "https://github.com/dashbitco/broadway_rabbitmq"

  def project do
    [
      app: :broadway_rabbitmq,
      version: @version,
      elixir: "~> 1.8",
      name: "BroadwayRabbitMQ",
      description: @description,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      package: package(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        docs: :docs,
        "coveralls.html": :test
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:broadway, "~> 1.0"},
      {:amqp, "~> 1.3 or ~> 2.0 or ~> 3.0 or ~> 4.0"},
      {:nimble_options, "~> 0.3.5 or ~> 0.4.0 or ~> 1.0"},
      {:telemetry, "~> 0.4.3 or ~> 1.0"},

      # Dev and test dependencies
      {:ex_doc, ">= 0.25.0", only: :docs},
      {:excoveralls, "~> 0.18.0", only: :test}
    ]
  end

  defp docs do
    [
      main: "BroadwayRabbitMQ.Producer",
      source_ref: "v#{@version}",
      source_url: @source_url,
      extras: ["CHANGELOG.md"]
    ]
  end

  defp package do
    %{
      licenses: ["Apache-2.0"],
      links: %{
        "Changelog" => @source_url <> "/blob/master/CHANGELOG.md",
        "GitHub" => @source_url
      }
    }
  end
end
