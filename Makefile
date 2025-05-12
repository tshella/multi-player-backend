# Makefile

APP_NAME=game_backend

# Docker commands
up:
	docker compose up --build

down:
	docker compose down

logs:
	docker compose logs -f

restart:
	docker compose down && docker compose up --build

release:
	MIX_ENV=prod mix deps.get --only prod && \
	MIX_ENV=prod mix compile && \
	MIX_ENV=prod mix release

shell:
	docker compose exec $(APP_NAME) sh

test:
	mix test --trace

format:
	mix format

lint:
	mix credo --strict

migrate:
	mix ecto.migrate

seed:
	mix run priv/repo/seeds.exs

clean:
	rm -rf _build deps
