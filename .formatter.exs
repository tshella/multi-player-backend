# .formatter.exs
[
  inputs: [
    "{mix,.formatter}.exs",
    "config/**/*.exs",
    "lib/**/*.{ex,exs}",
    "test/**/*.{ex,exs}"
  ],
  line_length: 100,
  import_deps: [:ecto, :phoenix, :plug, :oban],
  locals_without_parens: [
    field: 2,
    belongs_to: 2,
    has_many: 2,
    has_one: 2,
    timestamps: 0,
    pipe_through: 1,
    plug: 1,
    forward: 2,
    resources: 2
  ]
]
