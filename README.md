# 🎮 Elixir Multiplayer Game Backend

A scalable, fault-tolerant, real-time multiplayer game backend built using **Elixir**, **Phoenix**, and **OTP**, supporting live gameplay, AI bots, matchmaking, real-time communication, background jobs, analytics, and observability.

Architect: Manaka Anthony Raphasha
---

## 📚 Table of Contents

1. [Features](#features)
2. [Tech Stack](#tech-stack)
3. [Architecture Overview](#architecture-overview)
4. [Getting Started](#getting-started)
5. [Folder Structure](#folder-structure)
6. [Key Modules](#key-modules)
   - [Game Engine](#game-engine)
   - [Matchmaking](#matchmaking)
   - [Bots](#bots)
   - [Ranking](#ranking)
   - [Real-Time Channels](#real-time-channels)
   - [Event Streaming](#event-streaming)
   - [Caching](#caching)
   - [Authentication](#authentication)
   - [Analytics](#analytics)
7. [Testing](#testing)
8. [API Documentation](#api-documentation)
9. [Prometheus Metrics](#prometheus-metrics)
10. [Deployment](#deployment)

---

## ✅ Features

- 🧠 **Matchmaking**: Region + ELO-based queue matching
- 🎮 **Game loop**: Ticking GenServers per match (30–60Hz)
- 🤖 **AI Bots**: GenServer + gRPC for rule-based or ML-based bots
- 🧾 **JWT Auth**: Via Guardian for secure access
- 🔁 **Phoenix Channels**: Real-time gameplay and matchmaking events
- 📨 **RabbitMQ + Broadway**: Event publishing and consumption
- ⚙️ **Background Jobs**: Oban jobs for ranking and async processing
- 📈 **Telemetry + Prometheus**: Metrics and observability
- 🔐 **Redis Caching**: Nebulex with Redis for match/session storage
- 📘 **GraphQL + REST + Swagger**: Developer-friendly API interface

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

- Supervised GenServers run each live game session
- Bots are supervised actors using rule logic or gRPC
- Phoenix Channels power WebSocket communication
- Events published to RabbitMQ are consumed via Broadway for analytics/ranking
- Oban jobs process match results in background
- Redis is used for session/match caching
- Metrics exposed via Prometheus `/metrics` endpoint

---

## 🚀 Getting Started

### 1. Clone and Install

```bash
git clone https://github.com/your-org/game_backend.git
cd game_backend
mix deps.get
