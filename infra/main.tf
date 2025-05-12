terraform {
  required_providers {
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.22"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.12"
    }
  }

  required_version = ">= 1.3"
}

provider "kubernetes" {
  config_path = "~/.kube/config"
}

provider "helm" {
  kubernetes {
    config_path = "~/.kube/config"
  }
}

resource "helm_release" "game_backend" {
  name       = "game-backend"
  chart      = "${path.module}/../helm/game-backend"
  namespace  = "default"
  cleanup_on_fail = true

  set {
    name  = "image.repository"
    value = "your-dockerhub-username/game_backend"
  }

  set {
    name  = "image.tag"
    value = "latest"
  }

  set {
    name  = "env.DATABASE_URL"
    value = "ecto://postgres:postgres@postgres:5432/game_backend_prod"
  }

  set {
    name  = "env.REDIS_HOST"
    value = "redis"
  }

  set {
    name  = "env.REDIS_PORT"
    value = "6379"
  }

  set {
    name  = "secretEnv.SECRET_KEY_BASE"
    value = "yhu7nlqvznqak1s8mtefpfj03f"
  }

  set {
    name  = "secretEnv.GUARDIAN_SECRET"
    value = "83bd0dba-aabd-43fa-b501-39183d5f4860"
  }
}
