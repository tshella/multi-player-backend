# ────────────────
# Stage 1: Build
# ────────────────
FROM hexpm/elixir:1.16.3-erlang-26.2.2-alpine-3.19.1 AS builder

ENV MIX_ENV=prod

# Install system dependencies
RUN apk add --no-cache \
    build-base \
    git \
    npm \
    python3 \
    postgresql-dev \
    curl \
    openssl \
    inotify-tools

# Create and set working directory
WORKDIR /app

# Copy mix files
COPY mix.exs mix.lock ./

# Copy config and install deps
COPY config ./config
RUN mix do local.hex --force, local.rebar --force, deps.get --only=prod

# Copy the rest of the application
COPY . .

# Compile the project and build release
RUN mix deps.compile
RUN mix compile
RUN mix assets.deploy || true  # skip if no assets
RUN mix release

# ─────────────────────────────
# Stage 2: Final Image (runtime)
# ─────────────────────────────
FROM alpine:3.19.1 AS app

RUN apk add --no-cache openssl ncurses-libs libstdc++ bash

ENV MIX_ENV=prod \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    REPLACE_OS_VARS=true \
    HOME=/app

WORKDIR /app

# Copy release from build stage
COPY --from=builder /app/_build/prod/rel/game_backend ./

# Expose Phoenix port
EXPOSE 4000

# Command to run the release
CMD ["bin/game_backend", "start"]
