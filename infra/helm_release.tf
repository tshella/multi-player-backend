resource "helm_release" "game_backend" {
  name       = "game-backend"
  chart      = "../helm/game-backend"
  namespace  = "default"

  set {
    name  = "image.repository"
    value = "your-dockerhub-user/game_backend"
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
    name  = "env.SECRET_KEY_BASE"
    value = "some-super-secret"
  }

  set {
    name  = "env.GUARDIAN_SECRET"
    value = "jwt-super-secret"
  }
}
