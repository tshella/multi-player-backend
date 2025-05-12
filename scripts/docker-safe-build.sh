#!/bin/sh

# ──────────────────────────────
# 🎮 GAMER Backend Banner
# ──────────────────────────────
echo "────────────────────────────────────────────"
echo "   ██████╗  █████╗ ███╗   ███╗███████╗██████╗ "
echo "  ██╔════╝ ██╔══██╗████╗ ████║██╔════╝██╔══██╗"
echo "  ██║  ███╗███████║██╔████╔██║█████╗  ██████╔╝"
echo "  ██║   ██║██╔══██║██║╚██╔╝██║██╔══╝  ██╔═══╝ "
echo "  ╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗██║     "
echo "   ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚═╝     "
echo "────────────────────────────────────────────"
echo ""

# ──────────────────────────────
# Lock file check
# ──────────────────────────────
if [ ! -f mix.lock ]; then
  echo "⚠️  mix.lock not found. Running mix deps.get to create it..."
  mix deps.get
fi

echo "🚀 Launching Docker build and starting containers..."
docker compose up --build
