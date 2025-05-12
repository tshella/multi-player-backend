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
13. [License](#license)

---

## ✅ Features

- 🧠 Region + ELO-based **Matchmaking**
- 🎮 Tick-driven **Game Servers** per match
- 🤖 **AI Bots** (GenServer + gRPC adapter for ML)
- 🔐 **JWT Auth** (Guardian-based secure login)
- 🔁 Real-time sync via **Phoenix Channels**
- 📨 **RabbitMQ + Broadway** event ingestion
- ⚙️ **Oban background jobs** for async workflows
- 📈 **Prometheus + Telemetry** instrumentation
- 🧠 **Redis caching** via Nebulex
- 🧾 **REST + GraphQL + Swagger** interfaces

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

- `GameServer` is a GenServer per active match
- AI bots are supervised processes using rule-based or gRPC strategies
- `Matchmaking` queues players by ELO, region, party size
- Game events stream via RabbitMQ to Broadway consumers
- Ranking updates run as Oban jobs
- Metrics and logs are collected for Prometheus and Grafana

---

## 🚀 Getting Started

### 1. Clone and install dependencies

```bash
git clone https://github.com/your-org/game_backend.git
cd game_backend
mix deps.get

2. Set up your .env

Create .env in the root:

DATABASE_URL=ecto://postgres:postgres@db/game_backend_dev
SECRET_KEY_BASE=dev_secret
GUARDIAN_SECRET=jwt_dev_secret
REDIS_HOST=redis
REDIS_PORT=6379
PROMETHEUS_PORT=9090
PORT=4000

3. Run with Docker Compose

make gamer-up

or manually:

docker compose up --build

4. Access the interfaces
Service	URL
App	http://localhost:4000
Swagger Docs	http://localhost:4000/api-docs
RabbitMQ	http://localhost:15672
Grafana	http://localhost:3000
Prometheus	http://localhost:9090
📁 Folder Structure

lib/
├── accounts/        # JWT auth & user handling
├── bots/            # BasicBot + gRPC adapter
├── cache/           # Redis-based session/cache
├── events/          # RabbitMQ publisher + Broadway
├── game_engine/     # Core gameplay loop (GenServers)
├── matchmaking/     # Match queues, ELO pairing
├── metrics/         # Prometheus metrics/telemetry
├── ranking/         # Ranking logic, Oban workers
├── web/             # Channels, controllers, GraphQL

🔩 Key Modules

    GameServer & GameSupervisor: core match lifecycle

    Matchmaking: queueing, party, region-based matching

    BasicBot, GrpcAdapter: AI-controlled units

    EventPublisher, EventConsumer: RabbitMQ interactions

    Ranking: ELO-based updates post-match

    GameChannel: WebSocket stream per player/match

    PrometheusMetrics: custom telemetry setup

🧪 Testing

Run:

mix test

Includes:

    accounts_test.exs

    game_engine_test.exs

    matchmaking_test.exs

    test/support/factory.ex for user/bot stubs

📘 API Documentation

    REST: /api/...

    Swagger: /api-docs

    GraphQL: /graphql (via Absinthe)

📈 Prometheus Metrics

Available at:

GET /metrics

Tracks:

    game.tick.duration

    matchmaking.join_queue.count

    oban.job.success, oban.job.failure

    phoenix.channel.join/message

⚙️ Makefile & Scripts

Make your life easier:

make gamer-up     # Safe docker build with GAMER banner
make up           # Start Docker stack
make restart      # Rebuild + start
make migrate      # DB migrations
make seed         # Seed data
make test         # Run tests
make lint         # Credo lint
make format       # Format code
make bootstrap    # Install Terraform + init infra/

🧪 Included Scripts:

    scripts/docker-safe-build.sh

    scripts/bootstrap.sh (Terraform installer + runner)

☸️ Helm / Kubernetes

Includes Helm chart at helm/game-backend.

Install manually:

helm install game-backend ./helm/game-backend

Files:

    values.yaml

    deployment.yaml

    service.yaml

    secret.yaml

    NOTES.txt

Supports Terraform-based deployment via:

cd infra/
terraform init
terraform apply

📝 License

MIT © 2025 Manaka Anthony Raphasha
Built with passion, concurrency, and BEAM 🔥