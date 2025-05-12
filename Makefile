# Makefile

APP_NAME=game_backend
SCRIPT_DIR=scripts
DOCKER_SAFE_SCRIPT=$(SCRIPT_DIR)/docker-safe-build.sh

# ─────────────────────────────
# 📦 Docker Commands
# ─────────────────────────────
up:
	docker compose up --build

down:
	docker compose down

logs:
	docker compose logs -f

restart:
	docker compose down && docker compose up --build

shell:
	docker compose exec $(APP_NAME) sh

clean:
	rm -rf _build deps

# ─────────────────────────────
# 🚀 Gamer Banner Build
# ─────────────────────────────
gamer-up:
	@chmod +x $(DOCKER_SAFE_SCRIPT)
	@$(DOCKER_SAFE_SCRIPT)

# ─────────────────────────────
# 🔧 Mix/Elixir
# ─────────────────────────────
release:
	MIX_ENV=prod mix deps.get --only prod && \
	MIX_ENV=prod mix compile && \
	MIX_ENV=prod mix release

migrate:
	mix ecto.migrate

seed:
	mix run priv/repo/seeds.exs

test:
	mix test --trace

format:
	mix format

lint:
	mix credo --strict
