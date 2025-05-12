# 🎮 Elixir Multiplayer Game Backend

A scalable, fault-tolerant, real-time multiplayer game backend built using **Elixir**, **Phoenix**, and **OTP**, supporting live gameplay, AI bots, matchmaking, real-time communication, background jobs, analytics, and observability.

**Architect:** Manaka Anthony Raphasha

---

## 📚 Table of Contents

1. [Features](#features)
2. [Tech Stack](#tech-stack)
3. [Architecture Overview](#architecture-overview)
4. [Getting Started](#getting-started)
5. [Folder Structure](#folder-structure)
6. [Key Modules](#key-modules)
7. [Testing](#testing)
8. [API Documentation](#api-documentation)
9. [Prometheus Metrics](#prometheus-metrics)
10. [Deployment](#deployment)
11. [Makefile & Scripts](#makefile--scripts)
12. [Helm / Kubernetes](#helm--kubernetes)

---

## ✅ Features

- 🧠 Region + ELO-based **Matchmaking**
- 🎮 Real-time **Game Loops** (tick-based GenServers)
- 🤖 **AI Bots** (GenServer + gRPC adapter for ML)
- 🧾 **JWT Auth** (Guardian-based secure login)
- 🔁 **Phoenix Channels** for multiplayer actions
- 📨 **RabbitMQ + Broadway** for event streaming
- ⚙️ **Oban background jobs** for ranking, replays, etc.
- 📈 **Telemetry + Prometheus** metrics
- 🔐 **Redis caching** via Nebulex
- 📘 Dual interface: **REST + GraphQL + Swagger**

---

## 🔧 Tech Stack

| Layer              | Technology                  |
|--------------------|-----------------------------|
| Web Framework      | Phoenix                     |
| Game Engine        | Elixir GenServers + OTP     |
| Real-time          | Phoenix Channels (WebSocket)|
| Queue / Messaging  | RabbitMQ + Broadway         |
| Background Jobs    | Oban                         |
| Auth               | Guardian (JWT)              |
| Metrics            | Telemetry + Prometheus      |
| DB ORM             | Ecto                        |
| Database           | PostgreSQL                  |
| Caching            | Nebulex + Redis             |
| AI Integration     | gRPC adapter for ML models  |
| API Docs           | PhoenixSwagger/OpenApiSpex  |

---

## 🧱 Architecture Overview

- One `GameServer` per match
- Bots run as supervised actors (can integrate ML)
- `Matchmaking` pairs players by region + ELO
- Events published to RabbitMQ; consumed via Broadway
- Background workers use Oban
- Prometheus scrapes app on `/metrics`

---

## 🚀 Getting Started

### 1. Clone and install dependencies

```bash
git clone https://github.com/your-org/game_backend.git
cd game_backend
mix deps.get

2. Setup your environment

Create a .env file:

DATABASE_URL=ecto://postgres:postgres@db/game_backend_dev
SECRET_KEY_BASE=dev_secret
GUARDIAN_SECRET=jwt_dev_secret
REDIS_HOST=redis
REDIS_PORT=6379
PROMETHEUS_PORT=9090
PORT=4000

3. Run with Docker Compose

make gamer-up

Or directly:

docker compose up --build

To view your backend:

    App: http://localhost:4000

    RabbitMQ UI: http://localhost:15672 (guest/guest)

    Grafana: http://localhost:3000 (admin/admin)

    Prometheus: http://localhost:9090

    Swagger UI: http://localhost:4000/api-docs

📁 Folder Structure

lib/
├── accounts/        # Guardian-based JWT auth
├── bots/            # AI bots + gRPC adapter
├── cache/           # Redis cache layer
├── events/          # RabbitMQ publisher/consumer
├── game_engine/     # Core tick-based game logic
├── matchmaking/     # ELO + region matchmaker
├── metrics/         # Telemetry / Prometheus
├── ranking/         # ELO updater and Oban jobs
├── web/             # Channels, controllers, GraphQL

🔩 Key Modules

    GameServer / GameSupervisor: Match logic

    Matchmaking: Queue + ELO pairing

    BasicBot, GrpcAdapter: AI integrations

    EventPublisher + BroadwayConsumer: RabbitMQ stream

    Ranking: Oban-based post-match updates

    GameChannel, MatchmakingChannel: Real-time WebSocket

    PrometheusMetrics: /metrics endpoint setup

🧪 Testing

mix test

Includes:

    accounts_test.exs

    game_engine_test.exs

    matchmaking_test.exs

    Factory: test/support/factory.ex

📘 API Documentation

    REST: /api/...

    Swagger: /api-docs

    GraphQL: /graphql

📈 Prometheus Metrics

Accessible via:

GET /metrics

Includes:

    game.tick.duration

    matchmaking.join_queue.count

    oban.job.success/failure

    Phoenix channel join/message counts

⚙️ Makefile & Scripts

Use make to simplify tasks:

make gamer-up     # Show GAMER banner + safe docker build
make up           # Docker Compose up
make restart      # Rebuild and restart
make test         # Run all tests
make format       # Format code
make migrate      # Run DB migrations
make seed         # Load seed data
make shell        # Open app container shell

Includes banner in:

scripts/docker-safe-build.sh

☸️ Helm / Kubernetes

A Helm chart is included in helm/game-backend.

helm install game-backend ./helm/game-backend

Includes:

    deployment.yaml

    service.yaml

    secret.yaml

    values.yaml

    NOTES.txt

📝 License

MIT © 2025 Manaka Anthony Raphasha
Built with passion, concurrency, and BEAM 🔥