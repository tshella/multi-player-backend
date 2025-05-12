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

WORKDIR /app

# Copy mix.exs and optionally mix.lock if it exists
COPY mix.exs ./

# Attempt to copy mix.lock via shell logic to avoid breaking the build
# This will only succeed if mix.lock is present in the build context
RUN test -f mix.lock && cp mix.lock . || echo "⚠️  mix.lock not found, skipping."

# Copy config and install dependencies
COPY config ./config
RUN mix local.hex --force && mix local.rebar --force

# If mix.lock was not copied, generate it
RUN test -f mix.lock || mix deps.get
RUN mix deps.get --only=prod

# Copy all application code
COPY . .

# Compile and release
RUN mix deps.compile
RUN mix compile
RUN mix assets.deploy || true
RUN mix release

# ─────────────────────────────
# Stage 2: Runtime
# ─────────────────────────────
FROM alpine:3.19.1 AS app

RUN apk add --no-cache openssl ncurses-libs libstdc++ bash

ENV MIX_ENV=prod \
  LANG=en_US.UTF-8 \
  LC_ALL=en_US.UTF-8 \
  REPLACE_OS_VARS=true \
  HOME=/app

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/game_backend ./

EXPOSE 4000

CMD ["bin/game_backend", "start"]
